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
HOST_BUF_BANK = $01                      ; Host-only buffers (bank 1)
HOST_BMP_SCR  = $0400                    ; 1KB screen-matrix buffer in HOST_BUF_BANK
HOST_BMP_BASE = $2000                    ; 8000-byte bitmap buffer in HOST_BUF_BANK

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
p4_bitmap_was_active: .byte 0   ; Set when bitmap mode entered, cleared after frame render
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
p4_file_op_active: .byte 0      ; 1 = file operation in progress, don't switch video modes

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

        ; Initialize $FF16 to proper Plus/4 64KB value
        lda #$DB
        sta ted_regs+$16
        
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
        
        ; CRITICAL: Disable VIC-IV hot register propagation BEFORE setting pointers
        ; Otherwise $D018 writes will override our SCRNPTR/CHARPTR settings
        lda #$80
        trb $D05D               ; Clear bit 7 (HOTREG)
        
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
        .byte HOST_BUF_BANK                       ; dest bank 0
        .byte $00
        .word $0000
        rts

; ============================================================
; P4MEM_Read - FAST VERSION
; Input: p4_addr_hi:p4_addr_lo = Plus/4 address to read
; Output: A = byte read
; ============================================================
; ============================================================
; P4MEM_ReadFast - Fast path for memory reads
; Only handles simple RAM cases ($0000-$7FFF), falls through
; to full P4MEM_Read for anything complex ($8000+)
; ============================================================
P4MEM_ReadFast:
        lda p4_addr_hi
        bmi P4MEM_Read          ; $80+ needs full handler (ROM/IO)
        
        ; $0000-$7FFF: Simple RAM access
        cmp #$10
        bcs _readfast_bank5
        
        ; $0000-$0FFF: LOW_RAM_BUFFER
        clc
        adc #>LOW_RAM_BUFFER
        sta _readfast_low+2
        ldx p4_addr_lo
_readfast_low:
        lda LOW_RAM_BUFFER,x    ; High byte modified
        rts
        
_readfast_bank5:
        ; $1000-$7FFF: Direct read from bank 5
        lda p4_addr_lo
        sta P4_MEM_PTR
        lda p4_addr_hi
        sta P4_MEM_PTR+1
        lda #BANK_RAM
        sta P4_MEM_PTR+2
        lda #$00
        sta P4_MEM_PTR+3
        ldz #0
        lda [P4_MEM_PTR],z
        rts

; ============================================================
; P4MEM_Read - Full memory read handler (all address ranges)
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
        bcs _read_ff40_plus             ; $FF40+ -> check ROM visibility
        
        ; $FF00-$FF3F: ALL are TED registers (with $FF20-$FF3F mirroring $FF00-$FF1F)
        jmp read_ted_register

_read_ff40_plus:
        ; $FFFA-$FFFF: Always KERNAL ROM (hardware vectors)
        lda p4_addr_lo
        cmp #$FA
        bcs read_from_kernal            ; $FFFA-$FFFF always ROM

        ; $FF40-$FFF9: Check ROM visibility
        lda p4_rom_visible
        beq _read_ff40_ram              ; ROM disabled, read from RAM
        jmp read_from_kernal            ; ROM enabled, read from KERNAL

_read_ff40_ram:
        ; Read from RAM at $FF40-$FFF9 (when ROM is disabled)
        jmp read_ram_direct

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
        ; Use jump table for TED register reads
        ; Much faster than CMP/BEQ ladder
        lda p4_addr_lo
        and #$1F                        ; Mask to $00-$1F (handle mirroring)
        asl                             ; × 2 for word index
        tax
        jmp (ted_read_table,x)

; Variables for TED read handlers
kb_selector: .byte $FF

; Default handler - just read from ted_regs array
ted_read_default:
        lda p4_addr_lo
        and #$1F
        tax
        lda ted_regs,x
        rts

; Special handlers for registers that need extra processing
ted_read_00:                            ; Timer 1 low (running value)
        lda ted_timer1_lo
        rts

ted_read_01:                            ; Timer 1 high (running value)
        lda ted_timer1_hi
        rts

ted_read_08:                            ; Keyboard/joystick latch
        jsr P4_CheckCursorKeys
        lda $DC00
        sta kb_selector
        lda $DC01
        ldx kb_selector
        cpx #$FE
        bne +
        ora #$84                        ; Mask out C64 cursor keys
+       rts

ted_read_12:
        lda ted_regs+$12
        ora #$C0                        ; Bits 7,6 always read as 1
        rts

ted_read_13:
        lda ted_regs+$13
        ora #$C1                        ; Bits 7,6,0 always read as 1
        rts

ted_read_14:
        lda ted_regs+$14
        ora #$07                        ; Bits 2-0 always read as 1
        rts

ted_read_15:
        lda ted_regs+$15
        ora #$80                        ; Bit 7 always reads as 1
        rts

ted_read_16:
        lda ted_regs+$16
        ora #$C0                        ; 64KB Plus/4: bits 7,6 always set
        rts

ted_read_17:
        lda ted_regs+$17
        ora #$80
        rts

ted_read_18:
        lda ted_regs+$18
        ora #$80
        rts

ted_read_19:
        lda ted_regs+$19
        ora #$80
        rts

ted_read_1c:                            ; Raster high bit
        lda ted_regs+$1D
        and #$FE
        ora ted_raster_hi
        rts

ted_read_1d:                            ; Raster low byte
        lda ted_raster_lo
        rts

; Jump table for TED register reads (32 entries)
ted_read_table:
        .word ted_read_00               ; $00 - Timer 1 low
        .word ted_read_01               ; $01 - Timer 1 high
        .word ted_read_default          ; $02
        .word ted_read_default          ; $03
        .word ted_read_default          ; $04
        .word ted_read_default          ; $05
        .word ted_read_default          ; $06
        .word ted_read_default          ; $07
        .word ted_read_08               ; $08 - Keyboard
        .word ted_read_default          ; $09
        .word ted_read_default          ; $0A
        .word ted_read_default          ; $0B
        .word ted_read_default          ; $0C
        .word ted_read_default          ; $0D
        .word ted_read_default          ; $0E
        .word ted_read_default          ; $0F
        .word ted_read_default          ; $10
        .word ted_read_default          ; $11
        .word ted_read_12               ; $12
        .word ted_read_13               ; $13
        .word ted_read_14               ; $14
        .word ted_read_15               ; $15
        .word ted_read_16               ; $16
        .word ted_read_17               ; $17
        .word ted_read_18               ; $18
        .word ted_read_19               ; $19
        .word ted_read_default          ; $1A
        .word ted_read_default          ; $1B
        .word ted_read_1c               ; $1C - Raster high
        .word ted_read_1d               ; $1D - Raster low
        .word ted_read_default          ; $1E
        .word ted_read_default          ; $1F

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
        
        ; Invalidate code cache if writing to current code page
        ; This is critical for self-modifying code (depackers run from $0100)
        cmp p4_code_page_hi
        bne _write_low_no_inv
        lda #0
        sta p4_code_valid
        lda p4_addr_hi              ; Reload A for the write below
_write_low_no_inv:
        
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
        
        ; $FF00-$FF3F: TED registers (with $FF20-$FF3F mirroring $FF00-$FF1F)
        ; First store to ted_regs, then call handler for side effects
        and #$1F                        ; Mask to $00-$1F (handle mirroring)
        tax
        lda p4_saved_data
        sta ted_regs,x
        
        ; Use jump table for post-write side effects
        txa
        asl                             ; × 2 for word index
        tax
        jmp (ted_write_table,x)

; Default handler - no side effects needed
ted_write_default:
        rts

; Special handlers for registers with side effects
ted_write_01:                           ; Timer reload (when $FF01 written)
        lda ted_regs+$00
        sta ted_timer1_lo
        lda ted_regs+$01
        sta ted_timer1_hi
        rts

ted_write_06:                           ; Video mode
ted_write_07:                           ; Multicolor / scroll
ted_write_14:                           ; Screen address
        jsr P4VID_GfxConfigChanged
        rts

ted_write_08:                           ; Keyboard latch - already handled by CPU
        rts

ted_write_09:                           ; IRQ acknowledge (write 1 to clear)
        lda p4_saved_data
        eor #$FF
        and ted_regs+$09
        sta ted_regs+$09
        rts

ted_write_0c:                           ; Cursor position high
ted_write_0d:                           ; Cursor position low
        jsr P4VID_UpdateCursor
        rts

ted_write_0e:                           ; Voice 1 freq low
        jsr P4SND_UpdateVoice1
        rts

ted_write_11:                           ; Volume + voice control
        jsr P4SND_UpdateVolume
        lda ted_regs+$11
        and #$60                        ; Check bits 5 and 6 (voice 2/3)
        beq +
        jsr P4SND_UpdateVoice2_FromBasic
+       rts

ted_write_12:                           ; Sound high bits + video mode + charset
        lda p4snd_voice1_on
        beq +
        lda ted_regs+$12
        and #$03
        sta p4snd_v1_freq_hi
        jsr P4SND_UpdateVoice1_WithHigh
+       jsr P4VID_GfxConfigChanged
        jsr P4VID_CharsetChanged
        rts

ted_write_13:                           ; Charset base address
        jsr P4VID_CharsetChanged
        rts

ted_write_15:                           ; Background color
        lda p4_saved_data
        sta ted_regs+$15
        and #$7F
        sta $D021
        rts

ted_write_19:                           ; Border color
        lda p4_saved_data
        sta ted_regs+$19
        and #$7F
        sta $D020
        rts

; Jump table for TED register writes (32 entries)
ted_write_table:
        .word ted_write_default         ; $00 - Timer low latch
        .word ted_write_01              ; $01 - Timer high + reload
        .word ted_write_default         ; $02
        .word ted_write_default         ; $03
        .word ted_write_default         ; $04
        .word ted_write_default         ; $05
        .word ted_write_06              ; $06 - Video mode
        .word ted_write_07              ; $07 - Multicolor
        .word ted_write_08              ; $08 - Keyboard latch
        .word ted_write_09              ; $09 - IRQ acknowledge
        .word ted_write_default         ; $0A
        .word ted_write_default         ; $0B
        .word ted_write_0c              ; $0C - Cursor high
        .word ted_write_0d              ; $0D - Cursor low
        .word ted_write_0e              ; $0E - Voice 1 freq low
        .word ted_write_default         ; $0F - Voice 2 freq low (just store)
        .word ted_write_default         ; $10 - Voice 2 freq high (just store)
        .word ted_write_11              ; $11 - Volume + control
        .word ted_write_12              ; $12 - Sound/video/charset
        .word ted_write_13              ; $13 - Charset base
        .word ted_write_14              ; $14 - Screen address
        .word ted_write_15              ; $15 - Background color
        .word ted_write_default         ; $16
        .word ted_write_default         ; $17
        .word ted_write_default         ; $18
        .word ted_write_19              ; $19 - Border color
        .word ted_write_default         ; $1A
        .word ted_write_default         ; $1B
        .word ted_write_default         ; $1C
        .word ted_write_default         ; $1D
        .word ted_write_default         ; $1E
        .word ted_write_default         ; $1F

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
        ; Check if writing to current code page - invalidate cache if so
        lda p4_addr_hi
        cmp p4_code_page_hi
        bne _wtram_no_inv
        lda #0
        sta p4_code_valid
_wtram_no_inv:

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

        ; Don't switch video modes during file operations
        lda p4_file_op_active
        bne _gfx_skip

        ; Check TED $FF06 bit 5 for bitmap mode
        lda ted_regs+$06
        and #$20                ; Check bitmap mode bit (bit 5)
        beq _gfx_to_text

_gfx_enter_bitmap:
        ; Enter bitmap mode - determine if multicolor
        lda #1
        sta p4_video_mode
        sta p4_bitmap_was_active        ; Track that bitmap was used this frame

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
        lda p4_video_mode
        beq _gfx_already_text   ; Already in text mode, skip
        lda #0
        sta p4_video_mode
        sta p4_multicolor
        jsr P4VID_DisableHostBitmap
_gfx_already_text:
_gfx_skip:
        rts

; ============================================================
; P4VID_ScreenAddrChanged - Called when $FF14 is written
; Updates VIC-IV SCRNPTR to match Plus/4 screen address
; ============================================================
P4VID_ScreenAddrChanged:
        ; Don't change screen pointer during file operations or bitmap mode
        lda p4_file_op_active
        bne _scr_addr_done
        lda p4_video_mode
        bne _scr_addr_done      ; In bitmap mode, screen is handled differently
        
        ; Calculate Plus/4 screen address from $FF14
        ; Screen address = (bits 3-7 of $FF14) * $400
        ; Which is: ($FF14 & $F8) << 6, giving screen address bits 8-15
        ; Or equivalently: ($FF14 >> 2) & $3C gives the high byte of screen address
        lda ted_regs+$14
        lsr                     ; Shift right by 2 to get high byte
        lsr
        and #$3C                ; Mask to valid range (bits 10-13 become bits 2-5)
        ; Wait, this is getting complicated. Let me do it differently.
        
        ; Actually: ($FF14 & $F8) directly gives screen_addr / 256 * 8
        ; So screen_hi = ($FF14 & $F8) >> 2 = ($FF14 >> 2) & $3E
        ; For $FF14 = $18: $18 >> 2 = $06, & $3E = $06. But we want $0C!
        ; Hmm, let me recalculate...
        
        ; TED $FF14: bits 3-7 are address bits A10-A14
        ; Screen address bit 10 = $FF14 bit 3
        ; Screen address bit 14 = $FF14 bit 7
        ; So screen address = ($FF14 & $F8) << 7 = ($FF14 & $F8) * 128
        ; Which gives us: ($FF14 & $F8) >> 1 as the high byte
        
        lda ted_regs+$14
        and #$F8
        lsr                     ; Divide by 2 to get high byte of screen address
        sta _p4_scr_addr_hi
        
        ; Now we need to point VIC-IV SCRNPTR at the right place
        ; If screen is in $0000-$0FFF, we use LOW_RAM_BUFFER
        ; If screen is in $1000+, we use Bank 5 directly
        
        cmp #$10                ; Is high byte >= $10 ($1000+)?
        bcs _scr_in_bank5
        
        ; Screen is in $0000-$0FFF range - use LOW_RAM_BUFFER
        ; VIC-IV address = LOW_RAM_BUFFER + screen_offset
        ; LOW_RAM_BUFFER = $A000, so:
        clc
        adc #>LOW_RAM_BUFFER    ; Add $A0 to get $Axxx
        sta $D061               ; SCRNPTR middle byte
        lda #$00
        sta $D060               ; SCRNPTR LSB = $00
        sta $D062               ; SCRNPTR bank = 0
        bra _scr_addr_done
        
_scr_in_bank5:
        ; Screen is at $1000+ - point directly to Bank 5
        ; VIC-IV address = $050000 + screen_addr
        lda #$00
        sta $D060               ; SCRNPTR LSB = $00
        lda _p4_scr_addr_hi
        sta $D061               ; SCRNPTR middle = screen high byte
        lda #$05
        sta $D062               ; SCRNPTR bank = 5

_scr_addr_done:
        rts

_p4_scr_addr_hi: .byte 0

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

        ; HOTREG stays disabled globally (we use VIC-IV pointers directly)
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
        ; Only render bitmap if we're currently in bitmap mode
        lda p4_video_mode
        beq _pf_done            ; Not in bitmap mode, skip
        
        ; We're in bitmap mode - render if dirty
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
        ; VIC-IV only: keep HOTREG disabled so VIC-II regs don't stomp pointers
        lda #$80
        trb $D05D               ; Clear bit 7 (HOTREG)

        ; If already enabled, just refresh MCM + colors
        lda p4_host_bmp_on
        bne _hb_update

        ; Mark bitmap as on
        lda #1
        sta p4_host_bmp_on

        ; Point VIC-IV screen matrix to HOST_BMP_SCR in HOST_BUF_BANK
        lda #<HOST_BMP_SCR
        sta $D060               ; SCRNPTR LSB
        lda #>HOST_BMP_SCR
        sta $D061               ; SCRNPTR MSB
        lda #HOST_BUF_BANK
        sta $D062               ; SCRNPTR bank

        ; Point VIC-IV bitmap base (CHARPTR used as bitmap base in bitmap mode)
        lda #<HOST_BMP_BASE
        sta $D068               ; CHARPTR LSB
        lda #>HOST_BMP_BASE
        sta $D069               ; CHARPTR MSB
        lda #HOST_BUF_BANK
        sta $D06A               ; CHARPTR bank

        ; Enable bitmap mode
        lda $D011
        ora #$20                ; BMM
        sta $D011

        ; Clear host bitmap buffer to 0 so we start with a "clear" bitmap
        lda #$00
        sta $D707
        .byte $80, $00, $81, $00, $00   ; enhanced DMA options
        .byte $03                       ; fill command
        .word $1F40                     ; 8000 bytes
        .word $0000                     ; fill with $00
        .byte $00                       ; source bank (unused)
        .word HOST_BMP_BASE             ; dest address
        .byte HOST_BUF_BANK             ; dest bank
        .byte $00                       ; sub-command
        .word $0000                     ; modulo

        ; Request screen-matrix fill and a bitmap render
        lda #1
        sta p4_screen_fill_pending
        sta p4_gfx_dirty

        ; Use a 16-color palette for bitmap mode
        jsr P4VID_SetBrightTEDPalette16

_hb_update:
        ; Update multicolor bit
        lda $D016
        and #$EF                ; Clear MCM
        ldx p4_multicolor
        beq _hb_mcm_done
        ora #$10                ; Set MCM if multicolor
_hb_mcm_done:
        sta $D016

        ; Border/background from TED colors (map to C64 16 colors for safety)
        lda ted_regs+$19
        and #$7F
        tax
        lda ted_to_c64_color,x
        and #$0F
        sta $D020

        lda ted_regs+$15
        and #$7F
        tax
        lda ted_to_c64_color,x
        and #$0F
        sta $D021

        rts

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
        .word HOST_BMP_SCR                     ; dest address - screen RAM in bank 3
        .byte HOST_BUF_BANK                       ; dest bank 0
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

        ; Leave HOTREG disabled (VIC-IV pointers only)
        lda #$80
        trb $D05D

        ; Disable bitmap mode (clear BMM)
        lda $D011
        and #$DF
        sta $D011

        ; Disable multicolor (clear MCM)
        lda $D016
        and #$EF
        sta $D016

        ; Restore border/background using TED palette indices (0-127)
        lda ted_regs+$19
        and #$7F
        sta $D020
        lda ted_regs+$15
        and #$7F
        sta $D021

        ; Restore VIC-IV screen pointer back to text screen in LOW_RAM_BUFFER ($AC00) bank 0
        lda #$00
        sta $D060
        lda #$AC
        sta $D061
        lda #$00
        sta $D062

        ; Restore charset pointer based on current Plus/4 charset selection
        jsr P4VID_SyncCharset

        lda #0
        sta p4_host_bmp_on

        ; Restore full TED palette for text mode
        jsr P4VID_InitPalette
_dis_done:
        rts

_dis_temp: .byte 0

; ============================================================
; P4VID_RenderHiresBitmap - Convert TED linear bitmap -> VIC interleaved bitmap
; Source: Plus/4 RAM (BANK_RAM) at (p4_bmp_hi<<8), linear 40 bytes * 200 lines
; Dest:   HOST_BMP_BASE in HOST_BUF_BANK, VIC bitmap interleaved layout
; Layout:
;   For row=0..24 (8 lines each):
;     For line=0..7:
;       dst = HOST_BMP_BASE + (row*320) + (line*40)
;       src = bmp_base     + (row*320)  + (line*40)
; ============================================================
P4VID_RenderHiresBitmap:
        ; If host bitmap not on, nothing to do
        lda p4_host_bmp_on
        beq _rb_done

        ; Base source address = (p4_bmp_hi<<8) in bank 5
        lda #$00
        sta rb_src_row_lo
        lda p4_bmp_hi
        sta rb_src_row_hi

        ; Base dest row address = HOST_BMP_BASE
        lda #<HOST_BMP_BASE
        sta rb_dst_row_lo
        lda #>HOST_BMP_BASE
        sta rb_dst_row_hi

        ldx #25                 ; 25 character rows (200 lines / 8)
_rb_row_loop:
        ; Patch DMA list source address
        lda rb_src_row_lo
        sta _rb_dma_src_lo
        lda rb_src_row_hi
        sta _rb_dma_src_hi

        ; Patch DMA list destination address
        lda rb_dst_row_lo
        sta _rb_dma_dst_lo
        lda rb_dst_row_hi
        sta _rb_dma_dst_hi

        ; Trigger DMA copy of 320 bytes (one full character row = 8 lines × 40 bytes)
        lda #$00
        sta $D702               ; DMA list bank 0
        lda #>_rb_dma_list
        sta $D701
        lda #<_rb_dma_list
        sta $D700               ; triggers DMA

        ; Advance src_row by 320 bytes ($0140)
        clc
        lda rb_src_row_lo
        adc #$40
        sta rb_src_row_lo
        lda rb_src_row_hi
        adc #$01
        sta rb_src_row_hi

        ; Advance dst_row by 320 bytes ($0140)
        clc
        lda rb_dst_row_lo
        adc #$40
        sta rb_dst_row_lo
        lda rb_dst_row_hi
        adc #$01
        sta rb_dst_row_hi

        dex
        bne _rb_row_loop

_rb_done:
        rts

; DMA list for 320-byte copy (one full character row)
_rb_dma_list:
        .byte $00               ; copy command
        .word 320               ; count (8 lines × 40 bytes = 320)
_rb_dma_src_lo:
        .byte $00
_rb_dma_src_hi:
        .byte $20
        .byte BANK_RAM          ; src bank (5)
_rb_dma_dst_lo:
        .byte $00
_rb_dma_dst_hi:
        .byte $20
        .byte HOST_BUF_BANK     ; dst bank (1)
        .byte $00               ; sub-command
        .word $0000             ; modulo

; Working vars
rb_src_row_lo:   .byte 0
rb_src_row_hi:   .byte 0
rb_dst_row_lo:   .byte 0
rb_dst_row_hi:   .byte 0

copy_mc_screen_ram:
        ; In TED multicolor bitmap mode:
        ; - Attribute RAM at $0800 contains per-cell color data
        ; - Each byte: bits 6-4 = luminance, bits 3-0 = hue (0-127 range)
        ;
        ; For VIC-IV multicolor bitmap with NCM (Nibble Color Mode):
        ; - Screen RAM holds 8-bit color indices per cell
        ; - We can use the full TED palette (0-127) directly!
        ;
        ; But standard VIC-II multicolor only uses 4-bit colors (0-15).
        ; So we must map TED colors to 16 entries.
        ;
        ; Strategy: Extract hue (lower 4 bits) for screen RAM
        ;           This maps all luminances of same hue to same color.
        
        ; Source: attribute RAM at $0800 = LOW_RAM_BUFFER + $0800 = $A800
        lda #$08
        sta _mc_src_hi
        
        ; Copy 4 pages (1024 bytes, only 1000 used)
        lda #0
        sta _mc_page_cnt
        
_mc_page_loop:
        ; Calculate source high byte
        lda _mc_page_cnt
        clc
        adc _mc_src_hi
        
        ; Check if source is in low RAM ($0000-$0FFF)
        cmp #$10
        bcs _mc_setup_bank5
        
        ; Source in LOW_RAM_BUFFER
        clc
        adc #>LOW_RAM_BUFFER
        sta P4_MEM_PTR+1
        lda #0
        sta P4_MEM_PTR
        bra _mc_setup_dest
        
_mc_setup_bank5:
        ; Source in bank 5 - for simplicity, assume it's in LOW_RAM_BUFFER range
        ; Most Plus/4 programs use default $0800 which is in LOW_RAM_BUFFER
        clc
        adc #>LOW_RAM_BUFFER
        sta P4_MEM_PTR+1
        lda #0
        sta P4_MEM_PTR
        
_mc_setup_dest:
        ; Set up destination in bank 1 (HOST_BUF_BANK)
        ; We'll use 32-bit addressing with Z=0 and increment manually
        lda _mc_page_cnt
        clc
        adc #>HOST_BMP_SCR      ; Add page offset to $04
        sta P4_MEM_PTR+3
        lda #<HOST_BMP_SCR
        sta P4_MEM_PTR+2
        
        ; Copy this page (256 bytes)
        ldy #0
_mc_byte_loop:
        ; Read byte from Plus/4 video matrix (in LOW_RAM_BUFFER, bank 0)
        lda (P4_MEM_PTR),y
        
        ; Convert TED color to C64 color
        ; TED format: bits 6-4 = luminance, bits 3-0 = hue
        and #$7F
        tax
        lda ted_to_c64_color,x
        and #$0F
        sta _mc_tmp
        
        ; Put same color in both nibbles
        asl
        asl
        asl
        asl
        ora _mc_tmp
        
        ; Write to screen RAM in bank 1
        ; Store dest address in separate ZP location for 32-bit write
        phy
        sty _mc_tmp2            ; Save Y offset
        
        ; Calculate full dest address: HOST_BMP_SCR + page*256 + Y
        clc
        lda P4_MEM_PTR+2        ; Low byte of dest
        adc _mc_tmp2            ; Add Y offset
        sta $FA
        lda P4_MEM_PTR+3        ; High byte of dest
        adc #0
        sta $FB
        lda #HOST_BUF_BANK      ; Bank 1
        sta $FC
        lda #0
        sta $FD
        
        ; Get the converted color back
        lda _mc_tmp
        asl
        asl
        asl
        asl
        ora _mc_tmp
        
        ; Write using 32-bit addressing
        ldz #0
        sta [$FA],z
        
        ply
        
        ; Next byte
        iny
        bne _mc_byte_loop
        
        ; Next page
        inc _mc_page_cnt
        lda _mc_page_cnt
        cmp #4                  ; 4 pages = 1024 bytes (1000 used)
        bcc _mc_page_loop
        
        rts

_mc_src_hi:     .byte 0
_mc_page_cnt:   .byte 0
_mc_tmp:        .byte 0
_mc_tmp2:       .byte 0


copy_mc_color_ram:
        ; Color RAM provides color for %11 pixels
        ; On TED this comes from $FF17 (color 3 / background 2)
        lda ted_regs+$17
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
; Set VIC palette entries 0..15 with brightest TED hues.
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
        lda ted_palette_r+$70,x   ; Luminance 7 (brightest)
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
        .byte $0, $f, $f, $0, $9, $0, $0, $f, $f, $4, $2, $B, $0, $0, $2, $0
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
        .byte $0, $f, $0, $f, $0, $8, $0, $f, $8, $0, $5, $4, $5, $4, $0, $f
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
        .byte $0, $f, $0, $f, $9, $0, $f, $0, $0, $0, $0, $B, $5, $8, $5, $0
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