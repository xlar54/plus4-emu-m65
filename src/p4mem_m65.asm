; ============================================================
; p4mem_m65.asm - Memory system for Plus/4 emulator (FAST VERSION)
; Host: MEGA65
; Assembler: 64tass (with 45GS02 support)
; ============================================================
;
; Memory Map:
;   Bank 4 ($40000-$4FFFF): Plus/4 ROMs
;     $48000-$4BFFF: BASIC ROM (mapped to Plus/4 $8000-$BFFF)
;     $4C000-$4FFFF: KERNAL ROM (mapped to Plus/4 $C000-$FFFF)
;   Bank 5 ($50000-$5FFFF): Plus/4 RAM (64KB)
;
; FAST Optimization Strategy:
;   - LOW_RAM_BUFFER for $0000-$0FFF (zero page, stack, color, screen)
;     This enables fast host mirroring for text mode display
;   - Direct 32-bit flat addressing for ALL other RAM ($1000-$FFFF)
;   - Direct 32-bit flat addressing for ROM reads (no more buffering!)
;   - No DMA copying, no ROM buffers - everything is direct access
;
; ============================================================

        .cpu "45gs02"

; Bank numbers
BANK_ROM    = $04                       ; Plus/4 ROMs in bank 4
BANK_RAM    = $05                       ; Plus/4 RAM in bank 5

; Memory buffer in bank 0 (host RAM) - only for low RAM now
LOW_RAM_BUFFER  = $5000                 ; 4KB - Plus/4 low RAM $0000-$0FFF (always resident)

; Common low-RAM offsets (computed from LOW_RAM_BUFFER)
P4_SCREEN_BASE  = LOW_RAM_BUFFER + $0C00
P4_TCOLOR       = LOW_RAM_BUFFER + $07ED

; 32-bit pointer for direct memory access
; Located in zero page for use with 32-bit indirect addressing
; Format: [lo, hi, bank, megabyte]
P4_MEM_PTR      = $F0                   ; 4 bytes at $F0-$F3

; State variables
p4_rom_visible:      .byte 1    ; 1 = ROM visible, 0 = RAM only
p4_rom_bank:         .byte 0    ; Which function ROM bank (0-3)

; TED registers are defined in plus4_cpu_m65.asm

; --- Cursor overlay state (host-side rendering aid) ---
p4_cur_prev_lo:   .byte $FF   ; $FF = none
p4_cur_prev_hi:   .byte $FF
p4_cur_phase:     .byte 0     ; 0/1 toggle for blink
p4_cur_div:       .byte 0     ; frame divider

; --- Video present state (host-side) ---
; 0=text mirror active, 1=bitmap present active
p4_video_mode:    .byte 0
p4_gfx_dirty:     .byte 0
p4_bmp_hi:        .byte $20   ; default $2000

; Host VIC state save/restore (when flipping to bitmap)
p4_host_bmp_on:   .byte 0
p4_save_dd00:     .byte 0
p4_save_d011:     .byte 0
p4_save_d016:     .byte 0
p4_save_d018:     .byte 0
p4_save_d020:     .byte 0
p4_save_d021:     .byte 0

p4_multicolor:    .byte 0     ; 0=hires, 1=multicolor

; ============================================================
; P4MEM_Init - Initialize memory system
; ============================================================
P4MEM_Init:
        ; Initialize state
        lda #1
        sta p4_rom_visible
        lda #0
        sta p4_rom_bank
        
        ; Clear TED registers
        ldx #31
        lda #0
_clear_ted:
        sta ted_regs,x
        dex
        bpl _clear_ted
        
        ; Initialize TED color registers to Plus/4 defaults
        lda #$71                ; Default background (white, lum 7, color 1)
        sta ted_regs+$15
        lda #$6E                ; Default border (light blue, lum 6, color 14)
        sta ted_regs+$19
        
        ; Initialize timer to $FFFF
        lda #$FF
        sta ted_timer1_lo
        sta ted_timer1_hi
        lda #0
        sta ted_raster_lo
        sta ted_raster_hi
        
        ; Initialize SID for sound emulation
        jsr P4SND_Init
        
        ; Clear Plus/4 RAM (Bank 5) 
        jsr P4MEM_ClearRAM
        
        ; Clear local low RAM buffer
        jsr clear_low_ram_buffer

        rts

; ============================================================
; P4MEM_ClearRAM - Initialize Plus/4 RAM (Bank 5) with pattern
; ============================================================
P4MEM_ClearRAM:
        lda #$00
        sta $D707
        .byte $80, $00, $81, $00, $00   ; enhanced DMA options
        .byte $03                       ; fill
        .word $0000                     ; count (0 = 64K)
        .word $AA55                     ; fill with $AA,$55 pattern
        .byte $00                       ; unused
        .word $0000                     ; dest start
        .byte BANK_RAM                  ; dest bank 5
        .byte $00
        .word $0000
        rts

; ============================================================
; clear_low_ram_buffer - Clear the 4KB low RAM buffer to zero
; ============================================================
clear_low_ram_buffer:
        lda #$00
        sta $D707
        .byte $80, $00, $81, $00, $00
        .byte $03                       ; fill
        .word $1000                     ; 4KB
        .word $0000                     ; fill with $00
        .byte $00                       ; unused
        .byte <LOW_RAM_BUFFER
        .byte >LOW_RAM_BUFFER
        .byte $00                       ; dest bank 0
        .byte $00
        .word $0000
        rts

; ============================================================
; P4MEM_Read - FAST VERSION
; Input: p4_addr_hi:p4_addr_lo = Plus/4 address to read
; Output: A = byte read
; ============================================================
P4MEM_Read:
        lda p4_addr_hi

        ; Check for low RAM $0000-$0FFF (always in local buffer)
        cmp #$10
        bcs _read_not_low_ram
        
        ; Read from LOW_RAM_BUFFER + (page * 256) + offset
        clc
        adc #>LOW_RAM_BUFFER
        sta _read_low+2
        ldx p4_addr_lo
_read_low:
        lda LOW_RAM_BUFFER,x            ; High byte modified
        rts
        
_read_not_low_ram:
        ; Check for I/O area ($FF00-$FF3F) - TED registers
        cmp #$FF
        bne _read_not_ff
        lda p4_addr_lo
        cmp #$40
        bcs _read_ff40_plus             ; $FF40+ -> KERNAL ROM
        
        ; $FF00-$FF3F: ALL are TED registers (with $FF20-$FF3F mirroring $FF00-$FF1F)
        jmp read_ted_register

_read_ff40_plus:
        ; $FF40-$FFFF: KERNAL ROM
        jmp read_from_kernal

_read_not_ff:
        ; Check for $FD page (ACIA, keyboard)
        cmp #$FD
        bne _read_check_rom
        jmp read_fd_page

_read_check_rom:
        ; Check ROM visibility for $8000+
        cmp #$80
        bcc read_ram_direct             ; $1000-$7FFF -> always RAM
        
        ; $8000-$FCFF: Check ROM visibility
        lda p4_rom_visible
        beq read_ram_direct             ; ROM not visible, read RAM
        
        ; ROM is visible - determine which ROM
        lda p4_addr_hi
        cmp #$C0
        bcs read_from_kernal            ; $C000-$FEFF -> KERNAL
        jmp read_from_basic             ; $8000-$BFFF -> BASIC

        ; Fall through to read_ram_direct

; ============================================================
; read_ram_direct - Direct 32-bit read from RAM (bank 5)
; ============================================================
read_ram_direct:
        lda p4_addr_lo
        sta P4_MEM_PTR
        lda p4_addr_hi
        sta P4_MEM_PTR+1
        lda #BANK_RAM
        sta P4_MEM_PTR+2
        lda #$00
        sta P4_MEM_PTR+3
        
        ldz #$00
        lda [P4_MEM_PTR],z
        rts

; ============================================================
; read_ted_register - Handle TED register reads $FF00-$FF3F
; $FF20-$FF3F mirrors $FF00-$FF1F
; ============================================================
read_ted_register:
        lda p4_addr_lo
        and #$1F                        ; Mask to $00-$1F (handle mirroring)
        
        cmp #$08
        beq _read_keyboard
        cmp #$15
        beq _read_ff15
        cmp #$16
        beq _read_ff16              ; RAM size / memory config register
        cmp #$19
        beq _read_ff19
        cmp #$1C
        beq _read_raster_hi     ; $FF1C = high bit (bit 8 of raster)
        cmp #$1D
        beq _read_raster_lo     ; $FF1D = low byte of raster
        
        tax
        lda ted_regs,x
        rts

_read_ff15:
        lda ted_regs+$15
        ora #$80
        rts

_read_ff16:
        ; $FF16 is the RAM size / memory configuration register
        ; On Plus/4 (64KB): reading returns $C0 (bits 7,6 set) regardless of what was written
        ; On C16 (16KB): reading returns $00 or $40
        ; We emulate a Plus/4 with 64KB, so always return with bits 7,6 set
        lda ted_regs+$16
        ora #$C0                        ; Set bits 7 and 6 for 64KB Plus/4
        rts

_read_ff19:
        lda ted_regs+$19
        ora #$80
        rts

_read_keyboard:
        lda $DC01
        rts

_read_raster_lo:
        lda ted_raster_lo
        rts

_read_raster_hi:
        lda ted_regs+$1D
        and #$FE
        ora ted_raster_hi
        rts

; ============================================================
; read_fd_page - Handle $FD00-$FDFF reads (ACIA, keyboard)
; ============================================================
read_fd_page:
        lda p4_addr_lo
        cmp #$30
        beq _read_fd30
        cmp #$10
        bcc _read_acia
        
        ; $FD10-$FDFF (except $FD30): Read from RAM
        jmp read_ram_direct

_read_fd30:
        lda $DC00
        rts

_read_acia:
        cmp #$01
        beq _read_acia_status
        lda #$00
        rts
_read_acia_status:
        lda #$10
        rts

; ============================================================
; read_from_kernal - Direct 32-bit read from KERNAL ROM (bank 4)
; Plus/4 $C000-$FFFF -> Bank 4 $C000-$FFFF ($4C000-$4FFFF)
; ============================================================
read_from_kernal:
        lda p4_addr_lo
        sta P4_MEM_PTR
        lda p4_addr_hi
        sta P4_MEM_PTR+1
        lda #BANK_ROM                   ; Bank 4
        sta P4_MEM_PTR+2
        lda #$00
        sta P4_MEM_PTR+3
        
        ldz #$00
        lda [P4_MEM_PTR],z
        rts

; ============================================================
; read_from_basic - Direct 32-bit read from BASIC ROM (bank 4)
; Plus/4 $8000-$BFFF -> Bank 4 $8000-$BFFF ($48000-$4BFFF)
; ============================================================
read_from_basic:
        lda p4_addr_lo
        sta P4_MEM_PTR
        lda p4_addr_hi
        sta P4_MEM_PTR+1
        lda #BANK_ROM                   ; Bank 4
        sta P4_MEM_PTR+2
        lda #$00
        sta P4_MEM_PTR+3
        
        ldz #$00
        lda [P4_MEM_PTR],z
        rts

; ============================================================
; P4MEM_Write - FAST VERSION
; Input: p4_addr_hi:p4_addr_lo = Plus/4 address
;        p4_data = byte to write
; ============================================================
P4MEM_Write:
        lda p4_addr_hi
        
        ; Check for low RAM $0000-$0FFF (in local buffer for fast mirroring)
        cmp #$10
        bcs _write_not_low_ram
        
        ; Write to LOW_RAM_BUFFER (used for screen/color mirroring in text mode)
        pha                             ; Save page for mirror checks
        clc
        adc #>LOW_RAM_BUFFER
        sta _write_low+2
        ldx p4_addr_lo
        lda p4_data
_write_low:
        sta LOW_RAM_BUFFER,x            ; High byte modified

        ; If we're in bitmap mode, don't mirror text/color to host
        lda p4_video_mode
        beq _mirror_check               ; Text mode - always mirror
        pla                             ; Bitmap mode - skip mirror
        rts
_mirror_check:
        
        ; Check for mirroring
        pla                             ; Get page back
        cmp #$0C
        bcs _do_screen_mirror           ; $0C-$0F: screen mirror
        cmp #$08
        bcs _do_color_mirror            ; $08-$0B: color mirror
        rts                             ; $00-$07: no mirror needed

_do_screen_mirror:
        ; Screen mirror: Plus/4 $0C00-$0FFF -> MEGA65 $0800-$0BFF
        sec
        sbc #$04                        ; $0C->$08, $0D->$09, etc.
        sta _scr_mir+2
        ldx p4_addr_lo
        lda p4_data
_scr_mir:
        sta $0800,x
        rts

_do_color_mirror:
        ; Color RAM mirror: Plus/4 $0800-$0BFF -> MEGA65 $D800-$DBFF
        clc
        adc #$D0                        ; $08->$D8, $09->$D9, etc.
        sta _col_mir+2
        ldx p4_addr_lo
        lda p4_data
        and #$7F
        tay
        lda ted_to_c64_color,y
_col_mir:
        sta $D800,x
        rts
        
_write_not_low_ram:
        cmp #$FF
        beq _write_ff_page
        jmp p4mem_write_check_fd

_write_ff_page:
        lda p4_addr_lo
        cmp #$3F
        bne _write_not_ff3f
        lda #0
        sta p4_rom_visible
        rts

_write_not_ff3f:
        cmp #$3E
        bne _write_not_ff3e
        lda #1
        sta p4_rom_visible
        rts

_write_not_ff3e:
        cmp #$40
        bcs p4mem_write_to_ram          ; $FF40+ -> RAM under ROM
        
        ; $FF00-$FF3F: ALL are TED registers (with $FF20-$FF3F mirroring $FF00-$FF1F)
        and #$1F                        ; Mask to $00-$1F (handle mirroring)
        tax
        lda p4_data
        sta ted_regs,x

        ; Check for sound register writes
        ; Note: BASIC writes frequency LOW byte last, so we only trigger
        ; voice updates on the LOW byte write. The HIGH byte writes just
        ; update ted_regs but don't trigger sound changes.
        cpx #$0E
        beq _ted_sound_v1_changed       ; Voice 1 freq low - triggers update
        cpx #$0F
        beq _ted_write_done             ; Voice 1 freq high - just store, no update
        cpx #$10
        beq _ted_sound_v2_changed       ; Voice 2 freq low - triggers update
        cpx #$11
        beq _ted_sound_vol_changed      ; Volume + Voice 2 freq high + control

        ; Cursor position changes
        cpx #$0C
        beq _ted_cursor_changed
        cpx #$0D
        bne _ted_write_done
_ted_cursor_changed:
        phx
        jsr P4VID_UpdateCursor
        plx
_ted_write_done:

        ; Video mode / bitmap base changes
         cpx #$06
         beq _ted_vidchg
         cpx #$07               ; <-- ADD THIS: MCM bit is in $FF07
         beq _ted_vidchg        ; <-- ADD THIS
         cpx #$12
         beq _ted_vidchg
         cpx #$14
         bne _ted_viddone
 _ted_vidchg:
        jsr P4VID_GfxConfigChanged
 _ted_viddone:
        
        cpx #$01
        beq _write_ff01
        cpx #$09
        beq _write_ff09
        cpx #$15
        beq _write_ff15
        cpx #$19
        beq _write_ff19
        rts

_write_ff01:
        lda ted_regs+$00
        sta ted_timer1_lo
        lda ted_regs+$01
        sta ted_timer1_hi
        rts

_write_ff09:
        lda p4_data
        eor #$FF
        and ted_regs+$09
        sta ted_regs+$09
        rts

_write_ff15:
        lda p4_data
        and #$7F
        tax
        lda ted_to_c64_color,x
        sta $D021
        rts

_write_ff19:
        lda p4_data
        and #$7F
        tax
        lda ted_to_c64_color,x
        sta $D020
        rts

; Jump targets for sound register changes
_ted_sound_v1_changed:
        jsr P4SND_UpdateVoice1
        rts

_ted_sound_v2_changed:
        jsr P4SND_UpdateVoice2
        rts

_ted_sound_vol_changed:
        jsr P4SND_UpdateVolume
        ; Don't update voice 2 here - it will be updated when $FF10 is written
        ; This prevents accidentally enabling voice 2 when only changing volume
        rts

; ============================================================
; P4SND - TED to SID Sound Conversion
; 
; TED Sound Registers:
;   $FF0E - Voice 1 frequency low (8 bits)
;   $FF0F - Voice 1 frequency high (bits 0-1)
;   $FF10 - Voice 2 frequency low (8 bits)
;   $FF11 - Control register:
;           Bits 0-3: Volume (0-8, where 8=max)
;           Bit 4: Voice 2 frequency bit 8
;           Bit 5: Voice 2 frequency bit 9
;           Bit 6: Voice 2 sound type (0=tone, 1=noise)
;           Bit 7: Voice 1 enable? (DA bit)
;
; TED frequency formula: freq = 111860.8 / (1024 - value)
;   Value 0 = ~109 Hz, Value 1023 = highest
;
; SID frequency formula: freq = (Fn * Fclk) / 16777216
;   For 1MHz clock: freq = Fn * 0.0596
;   Fn = freq / 0.0596 = freq * 16.78
;
; Conversion: SID_freq = TED_freq * 16.78 / 0.0596
;           = 111860.8 / (1024 - TED_value) * 16.78
; ============================================================

; SID base addresses on MEGA65
SID_BASE = $D400
SID2_BASE = $D420               ; Second SID on MEGA65

; SID register offsets
SID_V1_FREQ_LO  = $00
SID_V1_FREQ_HI  = $01
SID_V1_PWLO     = $02
SID_V1_PWHI     = $03
SID_V1_CTRL     = $04
SID_V1_AD       = $05
SID_V1_SR       = $06

SID_V2_FREQ_LO  = $07
SID_V2_FREQ_HI  = $08
SID_V2_PWLO     = $09
SID_V2_PWHI     = $0A
SID_V2_CTRL     = $0B
SID_V2_AD       = $0C
SID_V2_SR       = $0D

SID_V3_FREQ_LO  = $0E
SID_V3_FREQ_HI  = $0F
SID_V3_PWLO     = $10
SID_V3_PWHI     = $11
SID_V3_CTRL     = $12
SID_V3_AD       = $13
SID_V3_SR       = $14

SID_FILT_LO     = $15
SID_FILT_HI     = $16
SID_RES_FILT    = $17
SID_MODE_VOL    = $18

; Sound state
p4snd_voice1_on: .byte 0
p4snd_voice2_on: .byte 0
p4snd_noise_mode: .byte 0

; Plus/4 stores sound duration counters at:
; $04FC/$04FD = Voice 1 duration (16-bit, decremented by IRQ)
; $04FE/$04FF = Voice 2 duration (16-bit, decremented by IRQ)
; We monitor these and turn off SID when they hit 0

; ============================================================
; P4SND_FrameTick - Called once per frame to check sound duration
; This ensures sounds stop even if BASIC IRQ handling isn't perfect
; ============================================================
P4SND_FrameTick:
        ; Check voice 1 duration counter at $04FC/$04FD
        lda p4snd_voice1_on
        beq _snd_tick_v2          ; Voice 1 not on, skip
        
        ; Read duration from Plus/4 RAM mirror (LOW_RAM_BUFFER + $4FC)
        lda LOW_RAM_BUFFER + $4FC
        ora LOW_RAM_BUFFER + $4FD
        bne _snd_tick_v2          ; Duration > 0, still playing
        
        ; Duration is 0, turn off voice 1
        lda #$00                  ; Gate off, no waveform
        sta SID_BASE + SID_V1_CTRL
        lda #0
        sta p4snd_voice1_on
        
_snd_tick_v2:
        ; Check voice 2 duration counter at $04FE/$04FF
        lda p4snd_voice2_on
        beq _snd_tick_done        ; Voice 2 not on, skip
        
        ; Read duration from Plus/4 RAM mirror
        lda LOW_RAM_BUFFER + $4FE
        ora LOW_RAM_BUFFER + $4FF
        bne _snd_tick_done        ; Duration > 0, still playing
        
        ; Duration is 0, turn off voice 2 (and voice 3 for noise)
        lda #$00                  ; Gate off, no waveform
        sta SID_BASE + SID_V2_CTRL
        sta SID_BASE + SID_V3_CTRL
        lda #0
        sta p4snd_voice2_on
        
_snd_tick_done:
        rts

; ============================================================
; P4SND_Init - Initialize SID for TED sound emulation
; ============================================================
P4SND_Init:
        ; First, silence everything on BOTH SIDs
        lda #$00
        sta SID_BASE + SID_MODE_VOL     ; Volume off SID 1
        sta SID2_BASE + SID_MODE_VOL    ; Volume off SID 2
        
        ; Reset all voice controls (gate off) on both SIDs
        lda #$00
        sta SID_BASE + SID_V1_CTRL
        sta SID_BASE + SID_V2_CTRL
        sta SID_BASE + SID_V3_CTRL
        sta SID2_BASE + SID_V1_CTRL
        sta SID2_BASE + SID_V2_CTRL
        sta SID2_BASE + SID_V3_CTRL
        
        ; Clear all frequencies on both SIDs
        sta SID_BASE + SID_V1_FREQ_LO
        sta SID_BASE + SID_V1_FREQ_HI
        sta SID_BASE + SID_V2_FREQ_LO
        sta SID_BASE + SID_V2_FREQ_HI
        sta SID_BASE + SID_V3_FREQ_LO
        sta SID_BASE + SID_V3_FREQ_HI
        sta SID2_BASE + SID_V1_FREQ_LO
        sta SID2_BASE + SID_V1_FREQ_HI
        sta SID2_BASE + SID_V2_FREQ_LO
        sta SID2_BASE + SID_V2_FREQ_HI
        sta SID2_BASE + SID_V3_FREQ_LO
        sta SID2_BASE + SID_V3_FREQ_HI
        
        ; Set up SID voice 1 (pulse wave)
        lda #$08                ; Pulse width 50%
        sta SID_BASE + SID_V1_PWHI
        lda #$00
        sta SID_BASE + SID_V1_PWLO
        lda #$00                ; Attack=0, Decay=0 (instant)
        sta SID_BASE + SID_V1_AD
        lda #$F0                ; Sustain=15 (max), Release=0 (instant)
        sta SID_BASE + SID_V1_SR
        
        ; Set up SID voice 2 (pulse wave)
        lda #$08
        sta SID_BASE + SID_V2_PWHI
        lda #$00
        sta SID_BASE + SID_V2_PWLO
        lda #$00                ; Attack=0, Decay=0
        sta SID_BASE + SID_V2_AD
        lda #$F0                ; Sustain=15, Release=0
        sta SID_BASE + SID_V2_SR
        
        ; Set up SID voice 3 (noise)
        lda #$00
        sta SID_BASE + SID_V3_AD
        lda #$F0
        sta SID_BASE + SID_V3_SR
        
        ; No filters
        lda #$00
        sta SID_BASE + SID_FILT_LO
        sta SID_BASE + SID_FILT_HI
        sta SID_BASE + SID_RES_FILT
        
        ; Set volume to max (will be controlled by VOL command)
        lda #$0F
        sta SID_BASE + SID_MODE_VOL
        
        ; Clear state
        lda #0
        sta p4snd_voice1_on
        sta p4snd_voice2_on
        sta p4snd_noise_mode
        
        rts

; ============================================================
; P4SND_UpdateVolume - Update SID volume from TED $FF11
; TED volume is 0-8, SID volume is 0-15
; We need to scale: SID = TED * 15 / 8 ≈ TED * 2 (close enough)
; But be careful: TED 8 * 2 = 16 which overflows!
; Better: use a lookup table or cap at 15
; ============================================================
P4SND_UpdateVolume:
        lda ted_regs+$11
        and #$0F                ; Get volume bits 0-3
        cmp #$08                ; TED volume max is 8
        bcc +
        lda #$0F                ; If >= 8, use SID max (15)
        bra _snd_vol_store
+       asl                     ; Multiply by 2 (0->0, 4->8, 7->14)
        cmp #$10
        bcc _snd_vol_store
        lda #$0F                ; Clamp to 15
_snd_vol_store:
        sta SID_BASE + SID_MODE_VOL
        rts

; ============================================================
; P4SND_UpdateVoice1 - Update SID voice 1 from TED registers
; ============================================================
P4SND_UpdateVoice1:
        ; TED frequency value: $FF0E (low) + $FF0F bits 0-1 (high)
        lda ted_regs+$0E        ; Voice 1 freq low
        sta p4snd_ted_freq_lo
        lda ted_regs+$0F        ; Voice 1 freq high (bits 0-1)
        and #$03
        sta p4snd_ted_freq_hi
        
        ; Check if voice 1 is enabled
        ; Voice 1 should only play if the LOW byte ($FF0E) is non-zero
        ; The high bits alone (from $FF0F) are not enough to trigger sound
        lda p4snd_ted_freq_lo
        beq _v1_off             ; If low byte is 0, voice is off
        
        ; Convert TED frequency to SID frequency
        jsr P4SND_ConvertFreq
        
        ; Store to SID voice 1
        lda p4snd_sid_freq_lo
        sta SID_BASE + SID_V1_FREQ_LO
        lda p4snd_sid_freq_hi
        sta SID_BASE + SID_V1_FREQ_HI
        
        ; Make sure voices 2 and 3 are OFF (gate off) if not in use
        lda p4snd_voice2_on
        bne _v1_skip_silence_v2
        lda #$00                ; No waveform, no gate
        sta SID_BASE + SID_V2_CTRL
        sta SID_BASE + SID_V3_CTRL
_v1_skip_silence_v2:
        
        ; Enable voice 1 (pulse wave, gate on)
        lda #$41                ; Pulse + gate
        sta SID_BASE + SID_V1_CTRL
        lda #1
        sta p4snd_voice1_on
        rts
        
_v1_off:
        ; Disable voice 1
        lda #$00                ; Gate off, no waveform
        sta SID_BASE + SID_V1_CTRL
        lda #0
        sta p4snd_voice1_on
        rts

; ============================================================
; P4SND_UpdateVoice2 - Update SID voice 2/3 from TED registers
; ============================================================
P4SND_UpdateVoice2:
        ; TED frequency value: $FF10 (low) + $FF11 bits 4-5 (high)
        lda ted_regs+$10        ; Voice 2 freq low
        sta p4snd_ted_freq_lo
        lda ted_regs+$11        ; Control register
        lsr                     ; Shift bits 4-5 down to 0-1
        lsr
        lsr
        lsr
        and #$03
        sta p4snd_ted_freq_hi
        
        ; Check noise mode (bit 6 of $FF11)
        lda ted_regs+$11
        and #$40
        sta p4snd_noise_mode
        
        ; Check if voice 2 is enabled
        ; Voice 2 should only play if the LOW byte ($FF10) is non-zero
        ; The high bits alone (from $FF11) are not enough to trigger sound
        ; This matches real TED behavior where BASIC sets high bits first
        ; but sound doesn't start until low byte is written
        lda p4snd_ted_freq_lo
        beq _v2_off             ; If low byte is 0, voice is off
        
        ; Convert TED frequency to SID frequency
        jsr P4SND_ConvertFreq
        
        ; Check if noise or tone mode
        lda p4snd_noise_mode
        bne _v2_noise
        
        ; Tone mode - use SID voice 2 with pulse wave
        lda p4snd_sid_freq_lo
        sta SID_BASE + SID_V2_FREQ_LO
        lda p4snd_sid_freq_hi
        sta SID_BASE + SID_V2_FREQ_HI
        lda #$41                ; Pulse + gate
        sta SID_BASE + SID_V2_CTRL
        ; Turn off voice 3 (noise) completely
        lda #$00                ; No waveform, gate off
        sta SID_BASE + SID_V3_CTRL
        lda #1
        sta p4snd_voice2_on
        rts
        
_v2_noise:
        ; Noise mode - use SID voice 3 with noise waveform
        lda p4snd_sid_freq_lo
        sta SID_BASE + SID_V3_FREQ_LO
        lda p4snd_sid_freq_hi
        sta SID_BASE + SID_V3_FREQ_HI
        lda #$81                ; Noise + gate
        sta SID_BASE + SID_V3_CTRL
        ; Turn off voice 2 (tone) completely
        lda #$00                ; No waveform, gate off
        sta SID_BASE + SID_V2_CTRL
        lda #1
        sta p4snd_voice2_on
        rts
        
_v2_off:
        ; Disable voice 2 and 3
        lda #$00                ; Gate off, no waveform
        sta SID_BASE + SID_V2_CTRL
        sta SID_BASE + SID_V3_CTRL
        lda #0
        sta p4snd_voice2_on
        rts

; Temporary storage for frequency conversion
p4snd_ted_freq_lo: .byte 0
p4snd_ted_freq_hi: .byte 0
p4snd_sid_freq_lo: .byte 0
p4snd_sid_freq_hi: .byte 0

; ============================================================
; P4SND_ConvertFreq - Convert TED frequency to SID frequency
; Input: p4snd_ted_freq_lo/hi = 10-bit TED frequency value
; Output: p4snd_sid_freq_lo/hi = 16-bit SID frequency value
;
; Formula: SID = 1904812 / (1024 - TED)
; We use K = $1D0000 = 1900544 (0.22% error, inaudible)
;
; This computes: SID = $1D0000 / (1024 - TED)
;                    = $1D0000 / divisor
; where divisor is 10-bit (1-1024)
; ============================================================
P4SND_ConvertFreq:
        ; Calculate divisor = 1024 - TED
        ; 1024 = $0400
        sec
        lda #$00
        sbc p4snd_ted_freq_lo
        sta _snd_divisor_lo
        lda #$04
        sbc p4snd_ted_freq_hi
        sta _snd_divisor_hi
        
        ; Check for divisor = 0 (TED = 1024, shouldn't happen)
        ora _snd_divisor_lo
        bne _div_ok
        ; Divisor is 0, return max frequency
        lda #$FF
        sta p4snd_sid_freq_lo
        sta p4snd_sid_freq_hi
        rts
        
_div_ok:
        ; Perform 24-bit / 16-bit division using standard shift-subtract algorithm
        ; Dividend = $1D0000
        ; Divisor = _snd_divisor (16-bit)
        ; Result goes in p4snd_sid_freq (16-bit)
        
        ; Initialize: dividend in _snd_dividend (3 bytes), remainder starts at 0
        lda #$00
        sta _snd_dividend+0     ; Low byte = $00
        sta _snd_dividend+1     ; Mid byte = $00  
        lda #$1D
        sta _snd_dividend+2     ; High byte = $1D
        
        ; Clear remainder
        lda #$00
        sta _snd_remainder
        sta _snd_remainder+1
        
        ; Clear result
        sta p4snd_sid_freq_lo
        sta p4snd_sid_freq_hi
        
        ; 24-bit division: shift dividend bits into remainder, compare, subtract
        ldx #24                 ; 24 bits to process
        
_div_loop:
        ; Shift dividend left, MSB goes into carry
        asl _snd_dividend+0
        rol _snd_dividend+1
        rol _snd_dividend+2
        ; Carry now has the bit shifted out of dividend
        
        ; Shift that bit into remainder
        rol _snd_remainder
        rol _snd_remainder+1
        
        ; Shift result left (make room for new quotient bit)
        asl p4snd_sid_freq_lo
        rol p4snd_sid_freq_hi
        
        ; Compare remainder with divisor
        lda _snd_remainder+1
        cmp _snd_divisor_hi
        bcc _div_no_sub         ; remainder < divisor, don't subtract
        bne _div_do_sub         ; remainder > divisor, do subtract
        ; High bytes equal, check low bytes
        lda _snd_remainder
        cmp _snd_divisor_lo
        bcc _div_no_sub         ; remainder < divisor
        
_div_do_sub:
        ; Subtract divisor from remainder
        sec
        lda _snd_remainder
        sbc _snd_divisor_lo
        sta _snd_remainder
        lda _snd_remainder+1
        sbc _snd_divisor_hi
        sta _snd_remainder+1
        
        ; Set bit 0 of result (quotient bit = 1)
        inc p4snd_sid_freq_lo
        ; Note: inc won't affect high byte, but that's ok since we just shifted
        
_div_no_sub:
        dex
        bne _div_loop
        
        rts

; Division working variables
_snd_divisor_lo:  .byte 0
_snd_divisor_hi:  .byte 0
_snd_dividend:    .fill 3, 0    ; 24-bit dividend
_snd_remainder:   .word 0       ; 16-bit remainder

_snd_tmp_lo: .byte 0
_snd_tmp_hi: .byte 0

p4mem_write_check_fd:
        lda p4_addr_hi
        cmp #$FD
        bne p4mem_write_to_ram
        
        lda p4_addr_lo
        cmp #$30
        bne _write_chk_fdd0
        lda p4_data
        sta $DC00
        rts

_write_chk_fdd0:
        cmp #$D0
        bcc p4mem_write_to_ram
        and #$0F
        sta p4_rom_bank
        rts

p4mem_write_to_ram:
        ; Direct 32-bit write to RAM (bank 5)
        lda p4_addr_lo
        sta P4_MEM_PTR
        lda p4_addr_hi
        sta P4_MEM_PTR+1
        lda #BANK_RAM
        sta P4_MEM_PTR+2
        lda #$00
        sta P4_MEM_PTR+3
        
        ldz #$00
        lda p4_data
        sta [P4_MEM_PTR],z

        ; If this write hits the active bitmap region, mark frame dirty
        lda p4_video_mode
        beq _wtram_done
        lda p4_addr_hi
        sec
        sbc p4_bmp_hi
        cmp #$20                        ; within 8KB window?
        bcs _wtram_done
        lda #1
        sta p4_gfx_dirty
_wtram_done:
        rts

; ============================================================
; Cursor overlay for text mode
; ============================================================
P4VID_UpdateCursor:
        ; --- restore previous cursor cell ---
        lda p4_cur_prev_hi
        cmp #$FF
        beq _no_restore

        lda p4_cur_prev_hi
        and #$03
        clc
        adc #>P4_SCREEN_BASE
        sta _scr_restore+2

        lda p4_cur_prev_hi
        and #$03
        clc
        adc #$08
        sta _host_restore+2

        ldx p4_cur_prev_lo
_scr_restore:
        lda P4_SCREEN_BASE,x
        and #$7F
_host_restore:
        sta $0800,x

_no_restore:
        ; --- load new cursor pos from TED regs ---
        lda ted_regs+$0C
        and #$03
        sta p4_cur_prev_hi
        lda ted_regs+$0D
        sta p4_cur_prev_lo

        lda p4_cur_prev_hi
        and #$03
        clc
        adc #>P4_SCREEN_BASE
        sta _scr_set+2

        lda p4_cur_prev_hi
        and #$03
        clc
        adc #$08
        sta _host_set+2

        ldx p4_cur_prev_lo
_scr_set:
        lda P4_SCREEN_BASE,x
        and #$7F
        ldy p4_cur_phase
        beq _write_cursor
        ora #$80
_write_cursor:
_host_set:
        sta $0800,x
        rts

; ============================================================
; Video mode switching
; ============================================================
P4VID_GfxConfigChanged:
        ; If GRAPHIC 2 mode (bit 6 of $0083 set), always stay in bitmap mode
        ; This prevents the Plus/4's split screen IRQ from toggling our display
        lda LOW_RAM_BUFFER + $83
        and #$40                ; Check bit 6 (split screen flag)
        beq _gfx_check_normal   ; Not GRAPHIC 2, handle normally
        
        ; GRAPHIC 2 mode - force bitmap mode, ignore TED $FF06 toggling
        lda ted_regs+$06
        and #$20
        beq _gfx_g2_done        ; If Plus/4 switched to text phase, just ignore it
        
        ; Plus/4 is in bitmap phase - update if needed
        lda p4_video_mode
        bne _gfx_g2_done        ; Already in bitmap mode
        bra _gfx_enter_bitmap   ; Enter bitmap mode
        
_gfx_g2_done:
        rts

_gfx_check_normal:
        lda ted_regs+$06
        and #$20                ; Check bitmap mode bit (bit 5)
        beq _gfx_to_text

_gfx_enter_bitmap:
        ; Enter bitmap mode - determine if multicolor
        lda #1
        sta p4_video_mode

        ; Check multicolor bit in $FF07 (bit 4)
        lda ted_regs+$07
        and #$10
        beq _gfx_hires
        lda #1
        sta p4_multicolor
        bra _gfx_setup_bmp
_gfx_hires:
        lda #0
        sta p4_multicolor

_gfx_setup_bmp:
        ; Get bitmap base address from $FF12 bits 3-5
        lda ted_regs+$12
        and #$38
        asl
        asl
        sta p4_bmp_hi

        lda #1
        sta p4_gfx_dirty
        jsr P4VID_EnableHostBitmap
        rts

_gfx_to_text:
        lda #0
        sta p4_video_mode
        sta p4_multicolor       ; Clear multicolor flag too
        lda #1
        sta p4_gfx_dirty
        jsr P4VID_DisableHostBitmap
        rts

; ============================================================
; P4VID_Frame - Called once per frame
; ============================================================
P4VID_Frame:
        ; Check sound duration and turn off expired sounds
        jsr P4SND_FrameTick
        
        lda p4_video_mode
        beq _pf_done
        lda p4_gfx_dirty
        beq _pf_done
        lda #0
        sta p4_gfx_dirty

        jsr P4VID_RenderHiresBitmap
_pf_done:
        rts

; ============================================================
; P4VID_EnableHostBitmap
; ============================================================
P4VID_EnableHostBitmap:
        ; Check if we need to do initial setup
        lda p4_host_bmp_on
        bne _update_mcm_only     ; Already in bitmap - just update MCM bit

        ; === First time bitmap enable - full setup ===
        
        ; Save host VIC/CIA2 state
        lda $DD00
        sta p4_save_dd00
        lda $D011
        sta p4_save_d011
        lda $D016
        sta p4_save_d016
        lda $D018
        sta p4_save_d018
        lda $D020
        sta p4_save_d020
        lda $D021
        sta p4_save_d021

        ; Select VIC bank 1 ($4000-$7FFF)
        lda $DD00
        and #$FC
        ora #$02
        sta $DD00

        ; Screen at $4800, Bitmap at $6000
        lda #$28
        sta $D018

        ; Enable bitmap mode in $D011
        lda $D011
        ora #$20                ; Enable bitmap mode
        sta $D011
        
        ; Mark bitmap as on
        lda #1
        sta p4_host_bmp_on

_update_mcm_only:
        ; === Update multicolor bit (called every time) ===
        ; This is the KEY fix - always update MCM when mode changes!
        
        lda $D016
        and #$EF                ; Clear MCM bit (bit 4), keep everything else
        ldx p4_multicolor
        beq _mcm_cleared
        ora #$10                ; Set MCM bit if multicolor mode
_mcm_cleared:
        sta $D016

        ; === Setup colors based on mode ===
        lda p4_multicolor
        beq _setup_hires_colors
        
        ; Multicolor: Copy screen RAM and color RAM
        jsr copy_mc_screen_ram
        jsr copy_mc_color_ram
        rts

_setup_hires_colors:
        ; Hires mode: simple fill
        lda $D021
        and #$0F
        sta _scr_fill_color
        
        lda P4_TCOLOR
        and #$7F
        tax
        lda ted_to_c64_color,x
        and #$0F
        asl
        asl
        asl
        asl
        ora _scr_fill_color
        sta _scr_fill_color
        sta _scr_fill_color+1

        ; Fill screen matrix
        lda #$00
        sta $D707
        .byte $80, $00, $81, $00, $00
        .byte $03
        .word $03E8
_scr_fill_color:
        .word $1010
        .byte $00
        .word $4800
        .byte $00
        .byte $00
        .word $0000

        rts

; ============================================================
; P4VID_DisableHostBitmap
; ============================================================
P4VID_DisableHostBitmap:
        lda p4_host_bmp_on
        beq _dis_done
        lda p4_save_dd00
        sta $DD00
        lda p4_save_d018
        sta $D018
        lda p4_save_d016
        sta $D016
        lda p4_save_d011
        sta $D011
        lda p4_save_d020
        sta $D020
        lda p4_save_d021
        sta $D021
        lda #0
        sta p4_host_bmp_on
_dis_done:
        rts

; ============================================================
; P4VID_RenderHiresBitmap - DMA copy from bank 5 to host bitmap
; ============================================================
P4VID_RenderHiresBitmap:
        lda p4_bmp_hi
        sta _bm_src+1

        lda #$00
        sta $D707
        .byte $80, $00, $81, $00, $00
        .byte $00                       ; copy
        .word $1F40                     ; 8000 bytes
_bm_src:
        .byte $00, $20, BANK_RAM        ; src lo/hi/bank (hi patched)
        .byte $00, $60, $00             ; dst lo/hi/bank 0
        .byte $00
        .word $0000
        rts

copy_mc_screen_ram:
        ; Screen RAM provides the FOREGROUND color for %01/%10 pixels
        ; Get the foreground color (black) from P4_TCOLOR
        lda P4_TCOLOR
        and #$7F
        tax
        lda ted_to_c64_color,x
        and #$0F
        sta _mc_scr_tmp
        asl
        asl
        asl
        asl
        ora _mc_scr_tmp             ; Same color in both nybbles
        sta _mc_scr_fill
        sta _mc_scr_fill+1
        
        lda #$00
        sta $D707
        .byte $80, $00, $81, $00, $00
        .byte $03
        .word $03E8
_mc_scr_fill:
        .word $0000
        .byte $00
        .word $4800
        .byte $00
        .byte $00
        .word $0000
        rts

_mc_scr_tmp: .byte 0


copy_mc_color_ram:
        ; Color RAM provides the BACKGROUND color for %11 pixels
        ; Get background from TED $FF15
        lda ted_regs+$15
        and #$7F
        tax
        lda ted_to_c64_color,x
        and #$0F
        
        ldx #0
_fill:
        sta $D800,x
        sta $D900,x
        sta $DA00,x
        inx
        bne _fill
        ldx #0
_fill2:
        sta $DB00,x
        inx
        cpx #$E8
        bne _fill2
        rts

; Split screen mode not implemented - GRAPHIC 2 acts like GRAPHIC 1

; ============================================================
; TED to C64 color mapping table (128 entries)
; ============================================================
ted_to_c64_color:
        ; Luminance 0 (darkest)
        .byte $00, $0B, $02, $03, $04, $05, $06, $07
        .byte $08, $09, $05, $0A, $0B, $0E, $06, $0D
        
        ; Luminance 1
        .byte $00, $0B, $02, $03, $04, $05, $06, $07
        .byte $08, $09, $05, $0A, $0B, $0E, $06, $0D
        
        ; Luminance 2
        .byte $00, $0C, $02, $03, $04, $05, $06, $07
        .byte $08, $09, $05, $0A, $0B, $0E, $06, $0D
        
        ; Luminance 3
        .byte $00, $0C, $02, $03, $04, $05, $06, $07
        .byte $08, $09, $0D, $0A, $03, $0E, $06, $0D
        
        ; Luminance 4
        .byte $0B, $0F, $0A, $03, $04, $0D, $0E, $07
        .byte $08, $09, $0D, $0A, $03, $0E, $0E, $0D
        
        ; Luminance 5
        .byte $0B, $0F, $0A, $03, $0A, $0D, $0E, $07
        .byte $08, $09, $0D, $07, $03, $0E, $0E, $0D
        
        ; Luminance 6
        .byte $0C, $0F, $0A, $03, $0A, $0D, $0E, $07
        .byte $07, $07, $07, $07, $03, $07, $0E, $0D
        
        ; Luminance 7 (brightest)
        .byte $01, $01, $0A, $03, $0A, $0D, $0E, $07
        .byte $07, $07, $07, $07, $03, $07, $07, $01