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
LOW_RAM_BUFFER  = $A000                 ; 4KB - Plus/4 low RAM $0000-$0FFF (always resident)

; Common low-RAM offsets (computed from LOW_RAM_BUFFER)
P4_SCREEN_BASE  = LOW_RAM_BUFFER + $0C00
P4_TCOLOR       = LOW_RAM_BUFFER + $07ED

; 32-bit pointer for direct memory access
; Located in zero page for use with 32-bit indirect addressing
; Format: [lo, hi, bank, megabyte]
P4_MEM_PTR      = $F0                   ; 4 bytes at $F0-$F3
p4_saved_data   = $F4                   ; Saved p4_data at P4MEM_Write entry (ZP for safety)

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
p4_screen_fill_pending: .byte 0  ; 1=need to fill screen RAM in frame handler

; --- Character set tracking ---
p4_charset_dirty: .byte 0     ; 1=need to sync charset to MEGA65
p4_charset_ram:   .byte 0     ; 0=using ROM charset, 1=using RAM charset
p4_charset_base:  .byte 0     ; Character set base address high byte (from $FF13)

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
        ; Initialize state variables only
        ; Video setup is done separately by P4MEM_InitVideo (after KERNAL calls)
        lda #1
        sta p4_rom_visible
        lda #0
        sta p4_rom_bank
        
        ; Initialize charset state
        lda #0
        sta p4_charset_ram
        sta p4_charset_dirty
        
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
; P4MEM_InitVideo - Set up VIC-IV for Plus/4 display
; Call this AFTER all MEGA65 KERNAL calls are done!
; ============================================================
P4MEM_InitVideo:
        ; Enable VIC-III/VIC-IV mode to access registers
        ; First unlock VIC-III: write $A5 then $96 to $D02F
        lda #$A5
        sta $D02F
        lda #$96
        sta $D02F
        ; Then unlock VIC-IV: write $47 then $53 to $D02F
        lda #$47
        sta $D02F
        lda #$53
        sta $D02F
        
        ; Initialize the TED 128-color palette for border/background
        jsr P4VID_InitPalette
        
        ; ============================================================
        ; Point VIC-IV screen at LOW_RAM_BUFFER + $0C00
        ; Plus/4 low RAM ($0000-$0FFF) is mirrored to LOW_RAM_BUFFER ($A000)
        ; So screen at Plus/4 $0C00 is at host $AC00 (bank 0)
        ; ============================================================
        lda #$00
        sta $D060               ; SCRNPTR LSB = $00
        lda #$AC
        sta $D061               ; SCRNPTR middle = $AC (for $AC00)
        lda #$00
        sta $D062               ; SCRNPTR bank = 0
        
        ; Point charset at default location initially
        ; $2D000 is the C64 charset in MEGA65
        lda #$00
        sta $D068               ; CHARPTR LSB
        lda #$D0
        sta $D069               ; CHARPTR middle  
        lda #$02
        sta $D06A               ; CHARPTR bank
        
        ; Color RAM pointer - use offset 0 (start of color RAM at $D800)
        lda #$00
        sta $D064               ; COLPTR LSB
        lda #$00  
        sta $D065               ; COLPTR MSB
        
        ; Standard 40-column text mode
        ; Virtual row width = 40 bytes (standard)
        lda #40
        sta $D058
        lda #0
        sta $D059
        
        ; Make sure CHR16 is OFF (standard 1-byte screen codes)
        lda $D054
        and #$FE                ; Clear CHR16 bit
        sta $D054
        
        ; Disable MCM for now (no multicolor text)
        lda $D016
        and #$EF                ; Clear MCM bit
        sta $D016
        
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
        ; Check and inject cursor keys on every keyboard poll
        jsr P4_CheckCursorKeys
        
        ; Read CIA column selector
        lda $DC00
        sta _kb_selector
        
        ; Read CIA result
        lda $DC01
        
        ; Mask out C64 cursor keys to prevent ghost characters
        ; C64 cursor DOWN/UP is column 0 ($FE), row 7 (bit 7)
        ; C64 cursor RIGHT/LEFT is column 0 ($FE), row 2 (bit 2)
        ldx _kb_selector
        cpx #$FE                ; Column 0?
        bne _kb_done
        ora #$84                ; Force bits 7 and 2 HIGH (mask out cursors)
_kb_done:
        rts

_kb_selector: .byte $FF

        ; Get the selector that Plus/4 wrote
        lda $DC00
        sta _kb_selector
        
        ; Get the base result from CIA
        lda $DC01
        sta _kb_result

        ; Check if Plus/4 is scanning column $DF (UP/DOWN cursor)
        lda _kb_selector
        and #$20
        bne _kb_check_bf
        
        ; Scanning $DF - inject cached cursor UP/DOWN state
        lda cursor_down_pressed
        beq _kb_check_up
        lda _kb_result
        and #$FE                ; DOWN = clear bit 0
        sta _kb_result
        
_kb_check_up:
        lda cursor_up_pressed
        beq _kb_check_bf
        lda _kb_result
        and #$F7                ; UP = clear bit 3
        sta _kb_result

_kb_check_bf:
        ; Check if Plus/4 is scanning column $BF (LEFT/RIGHT cursor)
        lda _kb_selector
        and #$40
        bne _kb_check_at
        
        ; Scanning $BF - inject cached cursor LEFT/RIGHT state
        lda cursor_right_pressed
        beq _kb_check_left
        lda _kb_result
        and #$F7                ; RIGHT = clear bit 3
        sta _kb_result

_kb_check_left:
        lda cursor_left_pressed
        beq _kb_check_at
        lda _kb_result
        and #$FE                ; LEFT = clear bit 0
        sta _kb_result

_kb_check_at:
        ; HACK: Suppress @ ghost when cursor down is pressed
        ; @ is at column $7F (bit 7 clear), row bit 0
        ; When cursor down sets row bit 0 low, it can ghost to @
        lda cursor_down_pressed
        beq _kb_done
        
        ; Check if scanning column $7F (bit 7 clear)
        lda _kb_selector
        and #$80
        bne _kb_done            ; Bit 7 set = not column $7F
        
        ; We ARE scanning column $7F and cursor down is pressed
        ; Force bit 0 HIGH to suppress @ ghost
        lda _kb_result
        ora #$01
        sta _kb_result

_kb_donew:
        lda _kb_result
        rts

_kb_result:      .byte $FF
_kb_selectorw:    .byte $FF

; Cursor key state - updated once per frame by P4_CheckCursorKeys
cursor_up_pressed:    .byte 0
cursor_down_pressed:  .byte 0
cursor_left_pressed:  .byte 0
cursor_right_pressed: .byte 0

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
        ; Save p4_data immediately since something corrupts $0B
        lda p4_data
        sta p4_saved_data
        
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
        lda p4_saved_data
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
        ; Screen mirror NO LONGER NEEDED!
        ; VIC-IV SCRNPTR points directly to Plus/4 RAM at $050C00
        ; Plus/4 writes to $0C00-$0FFF go to bank 5 which VIC-IV reads
        rts

_do_color_mirror:
        ; Color RAM mirror: Plus/4 $0800-$0BFF -> MEGA65 $D800-$DBFF
        ; Map TED 128 colors to C64 16 colors using lookup table
        clc
        adc #$D0                        ; $08->$D8, $09->$D9, etc.
        sta _col_mir+2
        ldx p4_addr_lo
        lda p4_saved_data
        and #$7F                        ; TED color is 7 bits
        tay
        lda ted_to_c64_color,y          ; Map to 16 C64 colors
_col_mir:
        sta $D800,x
        rts
        
_write_not_low_ram:
        cmp #$FD
        bne _write_not_fd
        ; $FD page - keyboard selector
        lda p4_addr_lo
        cmp #$30
        bne _write_fd_other
        ; $FD30 - write directly from p4_a (emulated accumulator)
        lda p4_a
        sta $DC00
        rts
_write_fd_other:
        ; Other $FD addresses - go to normal handler
        jmp p4mem_write_check_fd
        
_write_not_fd:
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
        lda p4_saved_data
        sta ted_regs,x

        ; Check for sound register writes
        ; Plus/4 BASIC SOUND command has unusual register mapping:
        ; SOUND 1: freq_lo -> $FF0E, freq_hi -> $FF12 (!)
        ; SOUND 2/3: freq_lo -> $FF0F, freq_hi -> $FF10, control -> $FF11
        ;
        ; For SOUND 2/3, we ONLY trigger when $FF11 is written (last in sequence)
        ; This ensures all frequency values are in place before playing
        cpx #$08
        beq _ted_kbd_latch              ; $FF08 - keyboard/joystick latch
        cpx #$0E
        beq _ted_sound_v1_changed       ; Voice 1 freq low - triggers update
        cpx #$0F
        beq _ted_check_video            ; Voice 2 freq low - just store
        cpx #$10
        beq _ted_check_video            ; Voice 2 freq high - just store
        cpx #$11
        beq _ted_sound_vol_and_noise    ; Volume + control - triggers voice 2 update
        cpx #$12
        beq _ted_reg12_write            ; $FF12 - sound AND video AND charset!
        cpx #$13
        beq _ted_reg13_write            ; $FF13 - charset base address
        ; Cursor position changes
        cpx #$0C
        beq _ted_cursor_changed
        cpx #$0D
        beq _ted_cursor_changed
        bra _ted_check_video

_ted_kbd_latch:
        ; $FF08 write - keyboard selector already captured by CPU op_8d
        ; Just return, nothing else needed here
        rts

_ted_cursor_changed:
        phx
        jsr P4VID_UpdateCursor
        plx
        bra _ted_check_video

_ted_reg12_write:
        ; $FF12 handles: sound voice 1 high bits, video mode, AND charset
        ; First do sound
        lda ted_regs+$12
        and #$03
        sta p4snd_v1_freq_hi
        jsr P4SND_UpdateVoice1_WithHigh
        ; Then check video mode
        jsr P4VID_GfxConfigChanged
        ; Then check charset
        jsr P4VID_CharsetChanged
        rts

_ted_reg13_write:
        ; $FF13 is charset base address
        jsr P4VID_CharsetChanged
        rts

_ted_check_video:
        ; Video mode / bitmap base changes
        cpx #$06
        beq _ted_vidchg
        cpx #$07
        beq _ted_vidchg
        cpx #$14
        bne _ted_check_other
_ted_vidchg:
        jsr P4VID_GfxConfigChanged

_ted_check_other:
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
        lda p4_saved_data
        eor #$FF
        and ted_regs+$09
        sta ted_regs+$09
        rts

_write_ff15:
        ; Background color - store in ted_regs AND apply to VIC-IV
        ; In text mode, use TED color directly (128-color palette)
        ; Value is also stored for bitmap mode to map later
        lda p4_saved_data
        sta ted_regs+$15        ; Store in TED register shadow
        and #$7F                ; TED color is 7 bits (0-127)
        sta $D021               ; VIC-IV uses our custom 128-color palette
        rts

_write_ff19:
        ; Border color - store in ted_regs AND apply to VIC-IV
        ; In text mode, use TED color directly (128-color palette)
        lda p4_saved_data
        sta ted_regs+$19        ; Store in TED register shadow
        and #$7F                ; TED color is 7 bits (0-127)
        sta $D020               ; VIC-IV uses our custom 128-color palette
        rts

; Jump targets for sound register changes
_ted_sound_v1_changed:
        jsr P4SND_UpdateVoice1
        rts

_ted_sound_vol_and_noise:
        ; $FF11 written - update volume
        jsr P4SND_UpdateVolume
        ; Check if this is a SOUND 2/3 command
        ; SOUND 2 sets bit 5 ($20), SOUND 3 sets bit 6 ($40)
        ; SOUND 1 only sets bit 4 ($10)
        ; So if bits 5 or 6 are set, this is SOUND 2/3
        lda ted_regs+$11
        and #$60                ; Check bits 5 and 6
        beq _vol_done           ; If neither set, this is SOUND 1, skip voice 2 update
        ; This is SOUND 2 or 3 - trigger voice 2 update
        ; Now all registers ($FF0F, $FF10, $FF11) have been written
        jsr P4SND_UpdateVoice2_FromBasic
_vol_done:
        rts

; Variable to store voice 1 freq high bits from $FF12 writes
p4snd_v1_freq_hi: .byte 0

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
; Uses ted_regs[$0E] for low byte and ted_regs[$0F] bits 0-1 for high
; ============================================================
P4SND_UpdateVoice1:
        ; TED frequency value: $FF0E (low) + $FF0F bits 0-1 (high)
        lda ted_regs+$0E        ; Voice 1 freq low
        sta p4snd_ted_freq_lo
        lda ted_regs+$0F        ; Voice 1 freq high (bits 0-1)
        and #$03
        sta p4snd_ted_freq_hi
        bra P4SND_V1_DoUpdate

; ============================================================
; P4SND_UpdateVoice1_WithHigh - Update voice 1 using stored high bits
; Uses ted_regs[$0E] for low byte and p4snd_v1_freq_hi for high bits
; This is called when BASIC writes freq high to $FF12
; ============================================================
P4SND_UpdateVoice1_WithHigh:
        lda ted_regs+$0E        ; Voice 1 freq low
        sta p4snd_ted_freq_lo
        lda p4snd_v1_freq_hi    ; Use the separately stored high bits
        sta p4snd_ted_freq_hi
        ; Fall through to DoUpdate

P4SND_V1_DoUpdate:
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

; ============================================================
; P4SND_UpdateVoice2_FromBasic - Handle BASIC's weird frequency encoding
; BASIC SOUND 2 writes: freq_lo to $FF0F, freq_hi to $FF10
; This is backwards from the normal TED register layout!
; ============================================================
P4SND_UpdateVoice2_FromBasic:
        ; BASIC encoding:
        ; ted_regs[$0F] = frequency LOW byte (8 bits)
        ; ted_regs[$10] = frequency HIGH bits (2 bits, in bits 0-1)
        
        lda ted_regs+$0F        ; Freq LOW from $FF0F
        sta p4snd_ted_freq_lo
        lda ted_regs+$10        ; Freq HIGH from $FF10 bits 0-1
        and #$03
        sta p4snd_ted_freq_hi
        
        ; Check noise mode (bit 6 of $FF11)
        lda ted_regs+$11
        and #$40
        sta p4snd_noise_mode
        
        ; Check if voice 2 is enabled (freq low must be non-zero)
        lda p4snd_ted_freq_lo
        beq _v2b_off
        
        ; Convert TED frequency to SID frequency
        jsr P4SND_ConvertFreq
        
        ; Check if noise or tone mode
        lda p4snd_noise_mode
        bne _v2b_noise
        
        ; Tone mode - use SID voice 2 with pulse wave
        lda p4snd_sid_freq_lo
        sta SID_BASE + SID_V2_FREQ_LO
        lda p4snd_sid_freq_hi
        sta SID_BASE + SID_V2_FREQ_HI
        lda #$41                ; Pulse + gate
        sta SID_BASE + SID_V2_CTRL
        ; Turn off voice 3 (noise) completely
        lda #$00
        sta SID_BASE + SID_V3_CTRL
        lda #1
        sta p4snd_voice2_on
        rts
        
_v2b_noise:
        ; Noise mode - use SID voice 3 with noise waveform
        ; TED noise has a different character than SID noise
        ; Scale frequency down by 4 to get a lower rumble that matches TED better
        lsr p4snd_sid_freq_hi
        ror p4snd_sid_freq_lo
        lsr p4snd_sid_freq_hi
        ror p4snd_sid_freq_lo
        lsr p4snd_sid_freq_hi
        ror p4snd_sid_freq_lo
        lsr p4snd_sid_freq_hi
        ror p4snd_sid_freq_lo
        
        lda p4snd_sid_freq_lo
        sta SID_BASE + SID_V3_FREQ_LO
        lda p4snd_sid_freq_hi
        sta SID_BASE + SID_V3_FREQ_HI
        lda #$81                ; Noise + gate
        sta SID_BASE + SID_V3_CTRL
        lda #$00
        sta SID_BASE + SID_V2_CTRL
        lda #1
        sta p4snd_voice2_on
        rts
        
_v2b_off:
        ; Disable voice 2 and 3
        lda #$00
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
        ; $FD30 - keyboard selector already captured by CPU op_8d
        lda p4_data
        sta $DC00               ; Also write to CIA for other keys
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
        lda p4_saved_data
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
        sta _host_restore+2     ; Same location - P4_SCREEN_BASE

        ldx p4_cur_prev_lo
_scr_restore:
        lda P4_SCREEN_BASE,x
        and #$7F
_host_restore:
        sta P4_SCREEN_BASE,x    ; Write back to screen buffer

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
        sta _host_set+2         ; Same location - P4_SCREEN_BASE

        ldx p4_cur_prev_lo
_scr_set:
        lda P4_SCREEN_BASE,x
        and #$7F
        ldy p4_cur_phase
        beq _write_cursor
        ora #$80
_write_cursor:
_host_set:
        sta P4_SCREEN_BASE,x    ; Write to screen buffer
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
; P4VID_CharsetChanged - Called when $FF12 or $FF13 is written
; Detects if Plus/4 is using ROM or RAM character set
; ============================================================
P4VID_CharsetChanged:
        ; Check $FF12 bit 2: 0=RAM, 1=ROM
        lda ted_regs+$12
        and #$04
        bne _charset_rom
        
        ; Using RAM character set
        lda #1
        sta p4_charset_ram
        
        ; Get base address from $FF13 bits 2-7
        ; Address = ($FF13 & $FC) * 256 (gives 1K aligned address)
        lda ted_regs+$13
        and #$FC
        sta p4_charset_base
        
        ; Mark charset as needing sync
        lda #1
        sta p4_charset_dirty
        rts

_charset_rom:
        ; Using ROM character set
        lda p4_charset_ram      ; Was it previously RAM?
        beq _charset_already_rom
        
        ; Switching back to ROM - need to restore MEGA65 charset pointer
        lda #0
        sta p4_charset_ram
        lda #1
        sta p4_charset_dirty
        
_charset_already_rom:
        rts

; ============================================================
; P4VID_SyncCharset - Copy Plus/4 charset from RAM to MEGA65
; Called from P4VID_Frame when p4_charset_dirty is set
; Only syncs when in text mode (not bitmap mode)
; ============================================================
P4VID_SyncCharset:
        ; Only sync charset in text mode
        lda p4_video_mode
        bne _sync_charset_done  ; In bitmap mode, skip
        
        ; Also skip if host bitmap is on (transitioning)
        lda p4_host_bmp_on
        bne _sync_charset_done
        
        lda p4_charset_ram
        bne _sync_ram_charset
        
        ; ROM charset - point VIC-IV back to default C64 charset
        ; Default C64 charset is at $2D000 in MEGA65 address space
        ; Use CHARPTR registers $D068-$D06A
        lda #$00
        sta $D068               ; CHARPTR LSB
        lda #$D0
        sta $D069               ; CHARPTR MSB  
        lda #$02
        sta $D06A               ; CHARPTR bank (bank 2 = $2xxxx)
        
        ; Re-enable hot registers
        lda #$80
        tsb $D05D               ; Set bit 7 (HOTREG)
        rts
        
_sync_ram_charset:
        ; RAM charset - point VIC-IV directly to Plus/4 RAM in bank 5
        ; Charset is at p4_charset_base * 256 in Plus/4 address space
        ; Plus/4 RAM is in bank 5, so address is $05xx00
        
        ; Disable hot registers so D018 writes don't override our CHARPTR
        lda #$80
        trb $D05D               ; Clear bit 7 (HOTREG)
        
        ; Set CHARPTR to bank 5 + charset base
        lda #$00
        sta $D068               ; CHARPTR LSB = 0
        lda p4_charset_base
        sta $D069               ; CHARPTR MSB = charset base high byte
        lda #BANK_RAM           ; Bank 5
        sta $D06A               ; CHARPTR bank
        
_sync_charset_done:
        rts

; ============================================================
; P4VID_Frame - Called once per frame
; ============================================================
P4VID_Frame:
        ; Check sound duration and turn off expired sounds
        jsr P4SND_FrameTick
        
        ; Update cursor blink (only in text mode)
        lda p4_video_mode
        bne _pf_skip_cursor
        inc p4_cur_div
        lda p4_cur_div
        and #$0F                ; Toggle every 16 frames
        bne _pf_skip_cursor
        lda p4_cur_phase
        eor #1
        sta p4_cur_phase
        jsr P4VID_UpdateCursor
_pf_skip_cursor:
        
        ; Check if charset sync is pending
        lda p4_charset_dirty
        beq _pf_no_charset
        lda #0
        sta p4_charset_dirty
        jsr P4VID_SyncCharset
_pf_no_charset:
        
        ; Check if screen fill is pending (deferred from EnableHostBitmap)
        lda p4_screen_fill_pending
        beq _pf_no_fill
        lda #0
        sta p4_screen_fill_pending
        jsr P4VID_DoScreenFill
_pf_no_fill:
        
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
        bne _update_mcm_only

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

        ; Select VIC bank 3 ($C000-$FFFF) - avoids conflict with our code
        ; DD00 bits 0-1: 00=bank 3, 01=bank 2, 10=bank 1, 11=bank 0
        lda $DD00
        and #$FC
        ora #$00                ; Bank 3
        sta $DD00

        ; Screen at $CC00, Bitmap at $E000 within bank 3
        ; D018: bits 4-7 = screen offset/64, bit 3 = bitmap offset (0=$0000, 1=$2000)
        ; Screen $CC00 in bank = $0C00 offset, /64 = $30 -> bits 4-7 = $3
        ; Bitmap $E000 in bank = $2000 offset -> bit 3 = 1
        ; D018 = $38
        lda #$38
        sta $D018

        ; Enable bitmap mode in $D011
        lda $D011
        ora #$20
        sta $D011

        ; Mark bitmap as on
        lda #1
        sta p4_host_bmp_on

        ; In VIC-II bitmap mode we only get 16 color indices.
        ; Force palette entries 0..15 to the *brightest* TED hues so
        ; bitmap colors resemble TED (brightness 7) instead of the
        ; very-dark luminance-0 entries.
        jsr P4VID_SetBrightTEDPalette16

_update_mcm_only:
        ; === Update multicolor bit (called every time) ===
        lda $D016
        and #$EF                ; Clear MCM bit (bit 4)
        ldx p4_multicolor
        beq _mcm_cleared
        ora #$10                ; Set MCM bit if multicolor mode
_mcm_cleared:
        sta $D016

        ; === Set border/background from TED colors ===
        ; In bitmap mode, VIC-II $D020/$D021 only use 4 bits (0-15)
        ; So we must map TED 128 colors to C64 16 colors
        lda ted_regs+$19        ; TED border color
        and #$7F
        tax
        lda ted_to_c64_color,x  ; Map to C64 color (0-15)
        sta $D020
        
        lda ted_regs+$15        ; TED background color
        and #$7F
        tax
        lda ted_to_c64_color,x  ; Map to C64 color (0-15)
        sta $D021

        ; === Schedule deferred screen fill ===
        ; Can't do it here (crashes), so do it in P4VID_Frame
        lda #1
        sta p4_screen_fill_pending
        rts

_setup_hires_colors:
        ; Not used - screen fill is deferred to P4VID_Frame
        rts

_hires_fill_byte: .byte 0

; ============================================================
; P4VID_DoScreenFill - Fill screen RAM (called from P4VID_Frame)
; This is deferred from EnableHostBitmap to avoid crashes
; ============================================================
P4VID_DoScreenFill:
        ; Check if multicolor or hires
        lda p4_multicolor
        bne _dsf_multicolor
        
        ; === Hires mode: fill screen RAM at $CC00 with color ===
        lda $D021
        and #$0F
        sta _dsf_fill_byte
        
        lda P4_TCOLOR
        and #$7F
        tax
        lda ted_to_c64_color,x
        and #$0F
        asl
        asl
        asl
        asl
        ora _dsf_fill_byte
        sta _dsf_fill_byte
        sta _dsf_fill_byte+1

        ; DMA fill screen RAM at $CC00 (1000 bytes)
        lda #$00
        sta $D707
        .byte $80, $00, $81, $00, $00   ; enhanced DMA options
        .byte $03                       ; fill command
        .word $03E8                     ; count (1000 bytes)
_dsf_fill_byte:
        .word $1010                     ; fill value (patched above)
        .byte $00                       ; source bank (unused for fill)
        .word $CC00                     ; dest address - screen RAM in bank 3
        .byte $00                       ; dest bank 0
        .byte $00                       ; sub-command
        .word $0000                     ; modulo
        rts

_dsf_multicolor:
        ; Multicolor mode: copy screen RAM and color RAM
        jsr copy_mc_screen_ram
        jsr copy_mc_color_ram
        rts

; ============================================================
; P4VID_DisableHostBitmap
; ============================================================
P4VID_DisableHostBitmap:
        lda p4_host_bmp_on
        beq _dis_done
        
        ; Restore DD00 (preserve IEC bits)
        lda $DD00
        and #$FC                ; Keep current IEC state (bits 2-7)
        sta _dis_temp
        lda p4_save_dd00
        and #$03                ; Get saved VIC bank bits only
        ora _dis_temp
        sta $DD00
        
        ; Restore VIC state
        lda p4_save_d018
        sta $D018
        lda p4_save_d016
        sta $D016
        lda p4_save_d011
        sta $D011
        
        ; Restore border/background from TED registers (use 128-color palette)
        ; Don't use saved values - those were C64-mapped for bitmap mode
        lda ted_regs+$19
        and #$7F
        sta $D020
        lda ted_regs+$15
        and #$7F
        sta $D021
        
        ; Restore VIC-IV SCRNPTR to text screen location
        ; Screen is at LOW_RAM_BUFFER + $0C00 = $AC00 in bank 0
        lda #$00
        sta $D060               ; SCRNPTR LSB = $00
        lda #$AC
        sta $D061               ; SCRNPTR middle = $AC
        lda #$00
        sta $D062               ; SCRNPTR bank = 0
        
        lda #0
        sta p4_host_bmp_on

        ; Restore full 128-entry TED palette for text mode
        ; (also restores entries 0..15 back to the standard TED table).
        jsr P4VID_InitPalette
_dis_done:
        rts

_dis_temp: .byte 0


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
        .byte $00, $E0, $00             ; dst lo/hi/bank 0 - bitmap at $E000
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
        
        ; Use standard DMA trigger (not inline)
        lda #$00
        sta $D702               ; DMA list in bank 0
        lda #>_mc_scr_dmalist
        sta $D701               ; DMA list MSB
        lda #<_mc_scr_dmalist
        sta $D700               ; DMA list LSB - triggers DMA
        rts

_mc_scr_dmalist:
        .byte $03                       ; fill command
        .word $03E8                     ; count (1000 bytes)
_mc_scr_fill:
        .word $0000                     ; fill value (patched above)
        .byte $00                       ; source bank (unused for fill)
        .word $CC00                     ; Screen RAM at $CC00 in bank 3
        .byte $00                       ; dest bank 0
        .byte $00                       ; sub-command (F018B)
        .word $0000                     ; modulo

_mc_scr_tmp: .byte 0


copy_mc_color_ram:
        ; Color RAM provides the BACKGROUND color for %11 pixels
        ; Get background from TED $FF15
        lda ted_regs+$15
        and #$7F
        tax
        lda ted_to_c64_color,x
        and #$0F
        sta _mc_col_fill
        sta _mc_col_fill+1
        
        ; Use standard DMA trigger (not inline)
        lda #$00
        sta $D702               ; DMA list in bank 0
        lda #>_mc_col_dmalist
        sta $D701               ; DMA list MSB
        lda #<_mc_col_dmalist
        sta $D700               ; DMA list LSB - triggers DMA
        rts

_mc_col_dmalist:
        .byte $03                       ; fill command
        .word $03E8                     ; count (1000 bytes)
_mc_col_fill:
        .word $0000                     ; fill value (patched above)
        .byte $00                       ; source bank (unused for fill)
        .word $D800                     ; dest address - color RAM
        .byte $80                       ; dest bank 0 with I/O enabled (bit 7)
        .byte $00                       ; sub-command (F018B)
        .word $0000                     ; modulo

; Split screen mode not implemented - GRAPHIC 2 acts like GRAPHIC 1

; ============================================================
; TED to C64 color mapping table (128 entries)
; ============================================================
; ============================================================
; P4VID_InitPalette - Initialize VIC-IV palette with TED colors
; TED color format: bits 6-4 = luminance (0-7), bits 3-0 = hue (0-15)
; Palette entries 0-127 are set to TED colors
; ============================================================
P4VID_InitPalette:
        ; Loop through all 128 TED colors
        ldx #0
        
_pal_loop:
        ; X = TED color value (0-127)
        ; Look up RGB values from our pre-calculated table
        lda ted_palette_r,x
        sta $D100,x             ; Red palette
        lda ted_palette_g,x
        sta $D200,x             ; Green palette
        lda ted_palette_b,x
        sta $D300,x             ; Blue palette
        
        inx
        cpx #128
        bne _pal_loop
        rts

; ============================================================
; P4VID_SetBrightTEDPalette16
; Overwrite VIC palette entries 0..15 with the *brightest* TED hues.
;
; Why this exists:
;   - In VIC-II bitmap modes, color indices are only 0..15.
;   - Our normal TED palette maps 0..127 to TED colors, so entries 0..15
;     are luminance-0 (mostly very dark).
;   - When we present TED graphics through VIC-II bitmap, we want indices
;     0..15 to correspond to TED hues at luminance 7 (indices $70..$7F).
; ============================================================
P4VID_SetBrightTEDPalette16:
        ldx #0
_btp_loop:
        lda ted_palette_r+$70,x
        sta $D100,x
        lda ted_palette_g+$70,x
        sta $D200,x
        lda ted_palette_b+$70,x
        sta $D300,x
        inx
        cpx #16
        bne _btp_loop
        rts

; ============================================================
; TED Palette RGB values (4-bit per channel, 0-15)
; Index = (luminance * 16) + hue
; Based on standard TED color calculations
; ============================================================

; Hue definitions:
;  0 = Black (no hue, just luminance)
;  1 = White (no hue, just luminance) 
;  2 = Red
;  3 = Cyan
;  4 = Purple
;  5 = Green
;  6 = Blue
;  7 = Yellow
;  8 = Orange
;  9 = Brown
; 10 = Yellow-Green
; 11 = Pink
; 12 = Blue-Green
; 13 = Light Blue
; 14 = Dark Blue (purple-ish)
; 15 = Light Green

ted_palette_r:
        ; Luminance 0 (darkest)
        .byte $0, $2, $5, $0, $5, $0, $0, $2, $5, $2, $2, $5, $0, $0, $2, $0
        ; Luminance 1
        .byte $0, $4, $7, $0, $7, $0, $0, $4, $7, $4, $4, $7, $0, $0, $4, $0
        ; Luminance 2
        .byte $0, $6, $9, $2, $9, $2, $2, $6, $9, $6, $6, $9, $2, $0, $6, $2
        ; Luminance 3
        .byte $0, $8, $B, $4, $B, $4, $4, $8, $B, $8, $6, $B, $4, $3, $8, $4
        ; Luminance 4
        .byte $0, $9, $D, $6, $D, $6, $6, $9, $D, $9, $8, $D, $6, $6, $9, $6
        ; Luminance 5
        .byte $0, $B, $F, $8, $F, $8, $8, $B, $F, $B, $9, $F, $8, $8, $B, $8
        ; Luminance 6
        .byte $0, $D, $F, $A, $F, $A, $A, $D, $F, $D, $B, $F, $A, $A, $D, $A
        ; Luminance 7 (brightest)
        .byte $0, $F, $F, $C, $F, $C, $C, $F, $F, $F, $D, $F, $C, $C, $F, $C

ted_palette_g:
        ; Luminance 0 (darkest)
        .byte $0, $2, $0, $5, $0, $5, $0, $5, $2, $0, $5, $0, $5, $2, $0, $5
        ; Luminance 1
        .byte $0, $4, $0, $7, $0, $7, $0, $7, $4, $0, $7, $0, $7, $4, $0, $7
        ; Luminance 2
        .byte $0, $6, $0, $9, $0, $9, $0, $9, $6, $2, $9, $2, $9, $6, $2, $9
        ; Luminance 3
        .byte $0, $8, $0, $B, $0, $B, $2, $B, $8, $4, $B, $4, $B, $8, $4, $B
        ; Luminance 4
        .byte $0, $9, $0, $D, $0, $D, $4, $D, $9, $6, $D, $6, $D, $9, $6, $D
        ; Luminance 5
        .byte $0, $B, $0, $F, $0, $F, $6, $F, $B, $8, $F, $8, $F, $B, $8, $F
        ; Luminance 6
        .byte $0, $D, $2, $F, $2, $F, $8, $F, $D, $A, $F, $A, $F, $D, $A, $F
        ; Luminance 7 (brightest)
        .byte $0, $F, $4, $F, $4, $F, $A, $F, $F, $C, $F, $C, $F, $F, $C, $F

ted_palette_b:
        ; Luminance 0 (darkest)
        .byte $0, $2, $0, $5, $5, $0, $5, $0, $0, $0, $0, $5, $5, $5, $5, $0
        ; Luminance 1
        .byte $0, $4, $0, $7, $7, $0, $7, $0, $0, $0, $0, $7, $7, $7, $7, $0
        ; Luminance 2
        .byte $0, $6, $0, $9, $9, $0, $9, $0, $0, $0, $0, $9, $9, $9, $9, $0
        ; Luminance 3
        .byte $0, $8, $0, $B, $B, $0, $B, $0, $0, $0, $0, $B, $B, $B, $B, $0
        ; Luminance 4
        .byte $0, $9, $0, $D, $D, $0, $D, $0, $0, $0, $0, $D, $D, $D, $D, $0
        ; Luminance 5
        .byte $0, $B, $0, $F, $F, $0, $F, $0, $0, $0, $0, $F, $F, $F, $F, $0
        ; Luminance 6
        .byte $0, $D, $2, $F, $F, $2, $F, $2, $2, $0, $2, $F, $F, $F, $F, $2
        ; Luminance 7 (brightest)
        .byte $0, $F, $4, $F, $F, $4, $F, $4, $4, $2, $4, $F, $F, $F, $F, $4

; ============================================================
; TED to C64 color mapping (for text mode color RAM)
; Used when we need to map TED colors to the 16 C64 colors
; ============================================================
ted_to_c64_color:
        ; Bitmap-mode mapping:
        ;   The VIC-II bitmap pipeline can only emit color indices 0..15.
        ;   For the Plus/4 we choose to preserve *hue* and drop luminance,
        ;   then program palette entries 0..15 to TED luminance 7.
        ;
        ; Result: TED $00..$7F -> (TED & $0F)
        ;         (i.e., 0..15 repeated for all 8 luminance levels)
        .byte $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0A,$0B,$0C,$0D,$0E,$0F
        .byte $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0A,$0B,$0C,$0D,$0E,$0F
        .byte $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0A,$0B,$0C,$0D,$0E,$0F
        .byte $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0A,$0B,$0C,$0D,$0E,$0F
        .byte $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0A,$0B,$0C,$0D,$0E,$0F
        .byte $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0A,$0B,$0C,$0D,$0E,$0F
        .byte $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0A,$0B,$0C,$0D,$0E,$0F
        .byte $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0A,$0B,$0C,$0D,$0E,$0F


; ============================================================
; Cursor Key Injection for MEGA65
; ============================================================
;
; Injects cursor key PETSCII codes directly into the Plus/4
; keyboard buffer, bypassing matrix scanning entirely.
;
; Plus/4 keyboard buffer (in LOW_RAM_BUFFER mirror):
;   Guest $0527-$0530 = KEYD -> Host $A527
;   Guest $00EF = NDX -> Host $A0EF
;
; MEGA65 $D604: Bit 0 = LEFT, Bit 1 = UP (active high)
; MEGA65 $D611: Bits 0-1 = shift keys
;
; ============================================================

; Host addresses for Plus/4 keyboard buffer
P4_NDX_HOST  = $A0EF            ; LOW_RAM_BUFFER + $EF
P4_KEYD_HOST = $A527            ; LOW_RAM_BUFFER + $527
P4_KEYD_MAX  = 10               ; Maximum buffer size

; PETSCII cursor codes
PETSCII_DOWN  = $11
PETSCII_UP    = $91
PETSCII_RIGHT = $1D
PETSCII_LEFT  = $9D

; ============================================================
; P4_CheckCursorKeys - Hybrid: $D610 for up/left, CIA for down/right
; ============================================================
P4_CheckCursorKeys:
        ; Save current CIA state
        lda $DC00
        sta _cki_saved_dc00
        
        ; Read shift column FIRST (column 6 = $BF)
        lda #$BF
        sta $DC00
        nop
        nop
        lda $DC01
        and #$10                ; Right shift is bit 4
        sta _cki_shift          ; 0 = shift pressed, $10 = not pressed
        
        ; Read cursor column (column 0 = $FE)
        lda #$FE
        sta $DC00
        nop
        nop
        lda $DC01
        sta _cki_cursor_bits
        
        ; Restore CIA
        lda _cki_saved_dc00
        sta $DC00
        
        ; Check vertical cursor (bit 7 LOW = pressed)
        lda _cki_cursor_bits
        and #$80
        bne _cki_vert_released      ; Bit 7 HIGH = not pressed
        
        ; Vertical cursor is pressed
        lda _cki_vert_held
        bne _cki_vert_repeat
        
        ; New press
        lda #$01
        sta _cki_vert_held
        lda #$00
        sta _cki_vert_cnt
        lda _cki_shift
        bne _cki_inject_down        ; $10 = no shift = DOWN
        lda #$91                    ; 0 = shift = UP
        jsr P4_InjectKey
        bra _cki_check_horiz
_cki_inject_down:
        lda #$11
        jsr P4_InjectKey
        bra _cki_check_horiz
        
_cki_vert_repeat:
        inc _cki_vert_cnt
        lda _cki_vert_cnt
        cmp #100
        bcc _cki_check_horiz
        lda #70
        sta _cki_vert_cnt
        lda _cki_shift
        bne _cki_rep_down
        lda #$91
        jsr P4_InjectKey
        bra _cki_check_horiz
_cki_rep_down:
        lda #$11
        jsr P4_InjectKey
        bra _cki_check_horiz
        
_cki_vert_released:
        lda #$00
        sta _cki_vert_held
        sta _cki_vert_cnt
        
_cki_check_horiz:
        ; Check horizontal cursor (bit 2 LOW = pressed)
        lda _cki_cursor_bits
        and #$04
        bne _cki_horiz_released     ; Bit 2 HIGH = not pressed
        
        ; Horizontal cursor is pressed
        lda _cki_horiz_held
        bne _cki_horiz_repeat
        
        ; New press
        lda #$01
        sta _cki_horiz_held
        lda #$00
        sta _cki_horiz_cnt
        lda _cki_shift
        bne _cki_inject_right       ; $10 = no shift = RIGHT
        lda #$9D                    ; 0 = shift = LEFT
        jsr P4_InjectKey
        bra _cki_done
_cki_inject_right:
        lda #$1D
        jsr P4_InjectKey
        bra _cki_done
        
_cki_horiz_repeat:
        inc _cki_horiz_cnt
        lda _cki_horiz_cnt
        cmp #100
        bcc _cki_done
        lda #70
        sta _cki_horiz_cnt
        lda _cki_shift
        bne _cki_rep_right
        lda #$9D
        jsr P4_InjectKey
        bra _cki_done
_cki_rep_right:
        lda #$1D
        jsr P4_InjectKey
        bra _cki_done
        
_cki_horiz_released:
        lda #$00
        sta _cki_horiz_held
        sta _cki_horiz_cnt
        
_cki_done:
        rts

_cki_saved_dc00:  .byte $FF
_cki_cursor_bits: .byte $FF
_cki_shift:       .byte $00
_cki_vert_held:   .byte $00
_cki_vert_cnt:    .byte $00
_cki_horiz_held:  .byte $00
_cki_horiz_cnt:   .byte $00

; ============================================================
; P4_InjectKey - Inject a PETSCII key into Plus/4 keyboard buffer
; Input: A = PETSCII code to inject
; Clobbers: A, X
; (Kept for potential future use)
; ============================================================
P4_InjectKey:
        pha                     ; Save key code

        ; Check if buffer has room
        ldx P4_NDX_HOST         ; Read current count from host mirror
        cpx #P4_KEYD_MAX
        bcs _ik_full            ; Buffer full

        ; Store key at buffer[NDX]
        pla                     ; Get key code
        sta P4_KEYD_HOST,x      ; Store in keyboard buffer

        ; Increment buffer count
        inc P4_NDX_HOST
        rts

_ik_full:
        pla                     ; Discard key code
        rts