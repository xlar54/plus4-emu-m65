; ============================================================
; p4mem_m65.asm - Memory system for Plus/4 emulator (OPTIMIZED)
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
; Optimization Strategy:
;   - Zero page ($00xx) and Stack ($01xx) are kept in local RAM
;     and synced at init/exit - these are accessed constantly
;   - 4KB page buffer for other memory accesses
;   - Separate ROM buffer (read-only, no flush needed)
;
; ============================================================

        .cpu "45gs02"

; Bank numbers
BANK_ROM    = $04                       ; Plus/4 ROMs in bank 4
BANK_RAM    = $05                       ; Plus/4 RAM in bank 5

; Memory buffers in bank 0 (host RAM)
; Using addresses above emulator code (which ends around $4000)
;
; VIC Bank 1 layout (for bitmap mode):
;   $4000-$47FF: Available
;   $4800-$4BFF: screen matrix (1KB)
;   $4C00-$5FFF: LOW_RAM_BUFFER extends into this
;   $6000-$7FFF: bitmap (8KB)
;
; OPTIMIZED STRATEGY:
;   - Plus/4 RAM in bank 5 ($50000-$5FFFF) accessed directly via 32-bit pointers
;   - No RAM buffer needed for $1000+ - direct 32-bit addressing to bank 5
;   - LOW_RAM_BUFFER only for $0000-$0FFF (zero page, stack, screen, color)
;   - KERNAL shadow for fast ROM reads
;
LOW_RAM_BUFFER  = $5000                 ; 4KB - Plus/4 low RAM $0000-$1000 (always resident)

; Common low-RAM offsets (computed from LOW_RAM_BUFFER)
P4_SCREEN_BASE  = LOW_RAM_BUFFER + $0C00
P4_TCOLOR       = LOW_RAM_BUFFER + $07ED

; KERNAL shadow: Plus/4 $E000-$FFFF (upper 8KB) copied to bank 0 for fast access
; This covers reset vectors, IRQ handlers, and most KERNAL routines
KERNAL_SHADOW   = $8000                 ; 8KB shadow of Plus/4 $E000-$FFFF
KERNAL_SHADOW_SIZE = $2000              ; 8KB

ROM_BUFFER      = $A000                 ; 8KB - ROM buffer (BASIC + lower KERNAL)
ROM_BUF_SIZE    = $2000                 ; 8KB

; 32-bit pointer for direct access to Plus/4 RAM in bank 5
; Located in zero page for use with 32-bit indirect addressing
; Format: [lo, hi, bank, 0] where bank 5 = $05
P4_RAM_PTR      = $F0                   ; 4 bytes at $F0-$F3

; --- Buffer geometry (derived) ---
ROM_BUF_PAGES   = ROM_BUF_SIZE / $0100    ; pages in ROM buffer
ROM_BUF_MASK    = $0100 - ROM_BUF_PAGES   ; align mask for base page

; State variables
p4_rom_visible:      .byte 1    ; 1 = ROM visible, 0 = RAM only
p4_rom_bank:         .byte 0    ; Which function ROM bank (0-3)

; ROM buffer state

; ROM buffer state: covers 16 consecutive pages  
rom_buf_base:        .byte $FF  ; Base page number in ROM buffer ($FF = none)
rom_buf_is_basic:    .byte 0    ; 0 = KERNAL, 1 = BASIC

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

; ============================================================
; P4MEM_Init - Initialize memory system
; ============================================================
P4MEM_Init:
        ; NOTE: Don't change C64 banking here - we still need MEGA65 KERNAL
        ; for CHROUT etc. Banking will be set up in main.asm before emulation.
        
        ; Initialize state
        lda #1
        sta p4_rom_visible
        lda #0
        sta p4_rom_bank
        
        ; No ROM buffer loaded yet
        lda #$FF
        sta rom_buf_base
        
        ; Initialize 32-bit pointer for Plus/4 RAM access (bank 5)
        ; P4_RAM_PTR will be: [lo, hi, $05, $00]
        ; We'll update lo/hi before each access
        lda #$00
        sta P4_RAM_PTR          ; lo byte (will be set per access)
        sta P4_RAM_PTR+1        ; hi byte (will be set per access)  
        lda #BANK_RAM           ; $05 for bank 5
        sta P4_RAM_PTR+2
        lda #$00
        sta P4_RAM_PTR+3        ; mega-byte (0)
        
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
        
        ; Don't set $D020/$D021 here - let startup screen use default C64 colors
        ; The Plus/4 KERNAL will set the colors when emulation starts
        
        ; Initialize timer to $FFFF (will reload from ted_regs when it underflows)
        lda #$FF
        sta ted_timer1_lo
        sta ted_timer1_hi
        lda #0
        sta ted_raster_lo
        sta ted_raster_hi
        
        ; Clear Plus/4 RAM (Bank 5) to zero
        jsr P4MEM_ClearRAM
        
        ; Clear local low RAM buffer to zero
        jsr clear_low_ram_buffer

        ; Load upper 8KB of KERNAL ($E000-$FFFF) into shadow for fast access
        jsr load_kernal_shadow

        rts

; ============================================================
; load_kernal_shadow - Copy upper 8KB KERNAL to shadow buffer
; Source: Bank 4 $E000-$FFFF (Plus/4 KERNAL upper half)
; Dest:   KERNAL_SHADOW ($8000-$9FFF in bank 0)
; ============================================================
load_kernal_shadow:
        lda #$00
        sta $D707
        .byte $80, $00, $81, $00, $00   ; enhanced DMA options
        .byte $00                       ; copy
        .word KERNAL_SHADOW_SIZE        ; 8KB
        .byte $00, $E0, BANK_ROM        ; src: $E000, bank 4
        .byte <KERNAL_SHADOW, >KERNAL_SHADOW, $00   ; dst: KERNAL_SHADOW, bank 0
        .byte $00
        .word $0000
        rts

; ============================================================
; P4MEM_ClearRAM - Initialize Plus/4 RAM (Bank 5) with pattern
; Real Plus/4 has random data at startup - we use a pattern
; ============================================================
P4MEM_ClearRAM:
        ; Fill with $AA pattern (alternating bits) to simulate uninitialized RAM
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
        ; Use DMA to clear 4KB at LOW_RAM_BUFFER
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
; P4MEM_Read
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
        adc #>LOW_RAM_BUFFER     ; Add buffer base high byte
        sta _read_low+2
        ldx p4_addr_lo
_read_low:
        lda LOW_RAM_BUFFER,x     ; High byte modified
        rts
        
_read_not_low_ram:
        cmp #$80
        bcs _read_high_memory
        
        ; Low RAM $0200-$7FFF (excluding screen)
        jmp read_from_ram

_read_high_memory:
        ; Check for I/O area ($FF00-$FF3F)
        cmp #$FF
        bne _read_not_io
        lda p4_addr_lo
        cmp #$40
        bcs _read_kernal_high
        cmp #$20
        bcs _read_not_ted_io
        
        ; TED registers $FF00-$FF1F
        cmp #$08
        beq _read_keyboard
        cmp #$15
        beq _read_ff15
        cmp #$19
        beq _read_ff19
        cmp #$1C
        beq _read_raster_lo
        cmp #$1D
        beq _read_raster_hi
        
        tax
        lda ted_regs,x
        rts

_read_ff15:
        ; Background color - TED returns bit 7 set
        lda ted_regs+$15
        ora #$80
        rts

_read_ff19:
        ; Border color - TED returns bit 7 set
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

_read_not_ted_io:
_read_kernal_high:
        ; $FF40-$FFFF or $FF20-$FF3F: KERNAL ROM
        jmp read_from_kernal

_read_not_io:
        lda p4_addr_hi
        cmp #$FD
        bne _read_not_fd
        
        lda p4_addr_lo
        cmp #$30
        beq _read_fd30
        cmp #$10
        bcc _read_acia
        jmp _read_not_fd

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

_read_not_fd:
        ; Check ROM visibility for $8000-$FCFF
        lda p4_rom_visible
        beq read_from_ram
        
        ; ROM is visible - determine which ROM
        lda p4_addr_hi
        cmp #$C0
        bcs read_from_kernal    ; $C000-$FCFF -> KERNAL
        jmp read_from_basic     ; $8000-$BFFF -> BASIC

; ============================================================
; read_from_ram - Read from Plus/4 RAM using 32-bit flat addressing
; Direct access to bank 5 without DMA buffering
; Uses LDA [$nn],Z instruction for 32-bit indirect addressing
; ============================================================
read_from_ram:
        ; Set up 32-bit pointer to Plus/4 RAM address
        ; P4_RAM_PTR = [p4_addr_lo, p4_addr_hi, $05, $00]
        lda p4_addr_lo
        sta P4_RAM_PTR
        lda p4_addr_hi
        sta P4_RAM_PTR+1
        ; P4_RAM_PTR+2 already has $05 (bank 5)
        ; P4_RAM_PTR+3 already has $00
        
        ; Use 32-bit indirect addressing: LDA [P4_RAM_PTR],Z
        ; Z register is index (use 0 for no offset)
        ldz #$00
        nop                     ; 32-bit addressing prefix (NOP = $EA)
        lda (P4_RAM_PTR),z      ; 32-bit indirect load
        rts

; ============================================================
; read_from_kernal - Read from KERNAL ROM
; Upper 8KB ($E000-$FFFF) uses fast shadow in bank 0
; Lower 8KB ($C000-$DFFF) uses ROM buffer with DMA
; ============================================================
read_from_kernal:
        ; Check if address is in shadow range ($E000-$FFFF)
        lda p4_addr_hi
        cmp #$E0
        bcs _rfk_shadow          ; >= $E0, use shadow
        
        ; Lower KERNAL ($C000-$DFFF) - use ROM buffer
        ; Check if ROM buffer is loaded with correct KERNAL pages
        lda rom_buf_base
        cmp #$FF
        beq _rfk_load            ; Buffer not loaded
        
        ; Check if it's KERNAL (not BASIC)
        lda rom_buf_is_basic
        bne _rfk_load            ; Wrong ROM type loaded
        
        ; Check if page is in buffer range
        lda p4_addr_hi
        sec
        sbc rom_buf_base
        bcc _rfk_load
        cmp #ROM_BUF_PAGES
        bcs _rfk_load
        
        ; Page is in buffer
        clc
        adc #>ROM_BUFFER
        sta _rfk_rd+2
        ldx p4_addr_lo
_rfk_rd:
        lda ROM_BUFFER,x
        rts
        
_rfk_load:
        ; Load KERNAL pages into ROM buffer
        lda p4_addr_hi
        and #ROM_BUF_MASK        ; Align to ROM buffer boundary
        sta rom_buf_base
        lda #0
        sta rom_buf_is_basic
        
        jsr load_rom_buffer
        jmp read_from_kernal

; Fast path: read from KERNAL shadow (upper 8KB: $E000-$FFFF)
; Shadow is at KERNAL_SHADOW ($8000), maps $E000->$8000, $FF->$9F
_rfk_shadow:
        ; Map $E0-$FF to $80-$9F (subtract $60)
        sec
        sbc #$60
        sta _rfk_shrd+2
        ldx p4_addr_lo
_rfk_shrd:
        lda KERNAL_SHADOW,x      ; High byte modified above
        rts

; ============================================================
; read_from_basic - Read from BASIC ROM via buffer
; ============================================================
read_from_basic:
        ; Check if ROM buffer is loaded
        lda rom_buf_base
        cmp #$FF
        beq _rfb_load            ; Buffer not loaded
        
        ; Check if it's BASIC (not KERNAL)
        lda rom_buf_is_basic
        beq _rfb_load            ; Wrong ROM type loaded
        
        ; Check if page is in buffer range
        lda p4_addr_hi
        sec
        sbc rom_buf_base
        bcc _rfb_load
        cmp #ROM_BUF_PAGES
        bcs _rfb_load
        
        ; Page is in buffer
        clc
        adc #>ROM_BUFFER
        sta _rfb_rd+2
        ldx p4_addr_lo
_rfb_rd:
        lda ROM_BUFFER,x
        rts
        
_rfb_load:
        ; Load BASIC pages into ROM buffer
        lda p4_addr_hi
        and #ROM_BUF_MASK        ; Align to ROM buffer boundary
        sta rom_buf_base
        lda #1
        sta rom_buf_is_basic
        
        jsr load_rom_buffer
        jmp read_from_basic

; ============================================================
; P4MEM_Write
; Input: p4_addr_hi:p4_addr_lo = Plus/4 address
;        p4_data = byte to write
; ============================================================
P4MEM_Write:
        lda p4_addr_hi
        
        ; Check for low RAM $0000-$0FFF (always in local buffer)
        cmp #$10
        bcs _write_not_low_ram
        
        ; Write to LOW_RAM_BUFFER + (page * 256) + offset
        pha                      ; Save page for mirror checks
        clc
        adc #>LOW_RAM_BUFFER
        sta _write_low+2
        ldx p4_addr_lo
        lda p4_data
_write_low:
        sta LOW_RAM_BUFFER,x     ; High byte modified

        ; If we're in bitmap present mode, don't mirror text/color to host.
        lda p4_video_mode
        beq _mirror_check
        pla                      ; discard saved page
        rts
_mirror_check:
        
        ; Check for mirroring
        pla                      ; Get page back
        cmp #$0C
        bcs _do_screen_mirror    ; $0C-$0F: screen mirror
        cmp #$08
        bcs _do_color_mirror     ; $08-$0B: color mirror
        rts                      ; $00-$07: no mirror needed

_do_screen_mirror:
        ; Screen mirror: Plus/4 $0C00-$0FFF -> MEGA65 $0800-$0BFF
        sec
        sbc #$04                 ; $0C->$08, $0D->$09, etc.
        sta _scr_mir+2
        ldx p4_addr_lo
        lda p4_data
_scr_mir:
        sta $0800,x
        rts

_do_color_mirror:
        ; Color RAM mirror: Plus/4 $0800-$0BFF -> MEGA65 $D800-$DBFF
        clc
        adc #$D0                 ; $08->$D8, $09->$D9, etc.
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
        jmp _write_check_fd

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
        cmp #$20
        bcs _write_not_ted
        
        tax
        lda p4_data
        sta ted_regs,x

; FOR BLINKING CURSOR OVERLAY
        cpx #$0C
        beq _ted_cursor_changed
        cpx #$0D
        bne _ted_write_done
_ted_cursor_changed:
        phx                     ; Save X before calling UpdateCursor
        jsr P4VID_UpdateCursor
        plx                     ; Restore X
_ted_write_done:
; END BLINKING CURSOR OVERLAY

        ; Video mode / bitmap base changes (BASIC GRAPHIC uses these)
        cpx #$06
        beq _ted_vidchg
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
        ; Background color - use low 4 bits for base color
        lda p4_data
        and #$7F                 ; Get full TED color (0-127)
        tax
        lda ted_to_c64_color,x   ; Map to C64 color
        sta $D021
        rts

_write_ff19:
        ; Border color - use low 4 bits for base color
        lda p4_data
        and #$7F                 ; Get full TED color (0-127)
        tax
        lda ted_to_c64_color,x   ; Map to C64 color
        sta $D020
        rts

_write_not_ted:
        jmp write_to_ram

_write_check_fd:
        lda p4_addr_hi
        cmp #$FD
        bne write_to_ram
        
        lda p4_addr_lo
        cmp #$30
        bne _write_chk_fdd0
        lda p4_data
        sta $DC00
        rts

_write_chk_fdd0:
        cmp #$D0
        bcc write_to_ram
        and #$0F
        sta p4_rom_bank
        rts

; FOR BLINKING CURSOR OVERLAY
; P4VID_UpdateCursor
; Uses:
;  - Plus/4 screen RAM is in LOW_RAM_BUFFER+$0C00
;  - Host screen is $0800-$0BFF (your mirror target)

P4VID_UpdateCursor:
    ; --- restore previous cursor cell ---
    lda p4_cur_prev_hi
    cmp #$FF
    beq _no_restore

    ; set screen source page: $8C + (prev_hi & 3)
    lda p4_cur_prev_hi
    and #$03
    clc
    adc #>P4_SCREEN_BASE
    sta _scr_restore+2

    ; set host dest page: $08 + (prev_hi & 3)
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

    ; set pages for new pos
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
    ora #$80          ; reverse-video on host to show cursor
_write_cursor:
_host_set:
    sta $0800,x
; END BLINKING CURSOR OVERLAY
    rts

; ============================================================
; write_to_ram - Write to Plus/4 RAM using 32-bit flat addressing
; Direct access to bank 5 without DMA buffering
; Uses STA [$nn],Z instruction for 32-bit indirect addressing
; ============================================================
write_to_ram:
        ; Set up 32-bit pointer to Plus/4 RAM address
        ; P4_RAM_PTR = [p4_addr_lo, p4_addr_hi, $05, $00]
        lda p4_addr_lo
        sta P4_RAM_PTR
        lda p4_addr_hi
        sta P4_RAM_PTR+1
        ; P4_RAM_PTR+2 already has $05 (bank 5)
        ; P4_RAM_PTR+3 already has $00
        
        ; Use 32-bit indirect addressing: STA [P4_RAM_PTR],Z
        ldz #$00
        lda p4_data
        nop                     ; 32-bit addressing prefix
        sta (P4_RAM_PTR),z      ; 32-bit indirect store

        ; If this write hits the active bitmap region, mark frame dirty
        lda p4_video_mode
        beq _wtram_done
        lda p4_addr_hi
        sec
        sbc p4_bmp_hi
        cmp #$20                ; within 8KB window?
        bcs _wtram_done
        lda #1
        sta p4_gfx_dirty
_wtram_done:
        rts

; ============================================================
; Color RAM mirroring - called from low RAM write for $0800-$0BFF
; ============================================================
_write_color_mirror:
        ; Plus/4 color RAM $0800-$0BFF -> MEGA65 $D800-$DBFF
        lda p4_addr_hi
        cmp #$08
        bcc _write_color_done
        cmp #$0C
        bcs _write_color_done
        
        clc
        adc #$D0
        sta _col_st+2
        ldx p4_addr_lo
        lda p4_data
        and #$7F                 ; Get full TED color
        tay
        lda ted_to_c64_color,y   ; Map to C64 color
_col_st sta $D800,x

_write_color_done:
        rts

; ============================================================
; Buffer management routines
; ============================================================

; load_rom_buffer - Load ROM pages from Plus/4 ROM into buffer
; Uses rom_buf_base and rom_buf_is_basic
load_rom_buffer:
        lda rom_buf_base
        sta _load_rom_src+1
        
        lda #$00
        sta $D707
        .byte $80, $00, $81, $00, $00
        .byte $00                       ; copy
        .word ROM_BUF_SIZE              ; 4KB
_load_rom_src:
        .byte $00, $00, BANK_ROM        ; src hi byte modified, bank 4
        .byte <ROM_BUFFER, >ROM_BUFFER, $00
        .byte $00
        .word $0000
        rts

; ============================================================
; Video: bitmap present (simple)
;
; This is intentionally minimal: when TED bitmap mode is enabled,
; we switch the host VIC into standard C64 bitmap mode and DMA-copy
; the Plus/4 bitmap into the host bitmap at $6000 (VIC bank 1).
; Colors are forced to white-on-black by filling the host screen
; matrix with $10.
; ============================================================

; ------------------------------------------------------------
; P4VID_GfxConfigChanged
; Called when TED regs that affect graphics mode/base are written.
; ------------------------------------------------------------
P4VID_GfxConfigChanged:
        ; TED $FF06 bitmap enable is bit 5 (BMM).
        lda ted_regs+$06
        and #$20
        beq _gfx_to_text

        ; Enter bitmap present
        lda #1
        sta p4_video_mode

        ; TED $FF12 bits 5..3 select 8KB bitmap page (A15..A13)
        ; hi byte = (bits 5..3) << 5  => (reg & $38) << 2
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
        lda #1
        sta p4_gfx_dirty
        jsr P4VID_DisableHostBitmap
        rts

; ------------------------------------------------------------
; P4VID_Frame
; Call once per TED frame (we hook this on raster wrap).
; ------------------------------------------------------------
P4VID_Frame:
        lda p4_video_mode
        beq _pf_done
        lda p4_gfx_dirty
        beq _pf_done
        lda #0
        sta p4_gfx_dirty

        ; With direct 32-bit RAM access, writes go straight to bank 5
        ; No buffer flush needed

_pf_copy:
        jsr P4VID_RenderHiresBitmap
_pf_done:
        rts

; ------------------------------------------------------------
; P4VID_EnableHostBitmap
; Switch host VIC into bitmap mode using VIC bank 1.
; Screen = $4800, Bitmap = $6000 (absolute addresses).
; ------------------------------------------------------------
P4VID_EnableHostBitmap:
        lda p4_host_bmp_on
        bne _en_done

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

        ; Select VIC bank 1 ($4000-$7FFF): CIA2 $DD00 bits 1..0 = %10 (inverted scheme)
        lda $DD00
        and #$FC
        ora #$02
        sta $DD00

        ; Screen at $0800 within VIC bank1 => $4800 absolute (screen index=2)
        ; Bitmap at $2000 within VIC bank1 => $6000 absolute (bitmap index=4)
        lda #$28
        sta $D018

        ; Hires bitmap: set BMM (D011 bit 5), clear MCM (D016 bit 4)
        lda $D016
        and #$EF
        sta $D016
        lda $D011
        ora #$20
        sta $D011

        ; Build screen color byte: fg in high nybble, bg in low nybble
        ; Get current background color (already converted to C64)
        lda $D021
        and #$0F
        sta _scr_fill_color
        
        ; Get foreground from Plus/4 TCOLOR ($07ED) 
        ; Read from LOW_RAM_BUFFER + $7ED = $87ED
        lda P4_TCOLOR
        and #$7F                ; Get TED color (0-127)
        tax
        lda ted_to_c64_color,x  ; Convert to C64 color
        and #$0F
        asl
        asl
        asl
        asl                     ; Shift to high nybble
        ora _scr_fill_color
        sta _scr_fill_color
        sta _scr_fill_color+1   ; DMA fill uses word

        ; Don't clear bitmap here - let RenderHiresBitmap copy actual Plus/4 data
        ; GRAPHIC 1,1 clears in Plus/4 RAM, GRAPHIC 1,0 preserves existing data

        ; Fill screen matrix ($4800..$4BE7 = 1000 bytes) with fg/bg color
        lda #$00
        sta $D707
        .byte $80, $00, $81, $00, $00
        .byte $03
        .word $03E8
_scr_fill_color:
        .word $1010             ; Will be modified above
        .byte $00
        .word $4800
        .byte $00
        .byte $00
        .word $0000

        lda #1
        sta p4_host_bmp_on
_en_done:
        rts

; ------------------------------------------------------------
; P4VID_DisableHostBitmap
; Restore saved VIC/CIA2 state.
; ------------------------------------------------------------
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

; ------------------------------------------------------------
; P4VID_RenderHiresBitmap
; DMA copy Plus/4 bitmap -> host bitmap ($6000).
; Source: bank 5 at $pp00 where pp = p4_bmp_hi (8KB aligned).
; ------------------------------------------------------------
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

; ============================================================
; TED to C64 color mapping table (128 entries)
; TED color format: 0LLL CCCC (L=luminance 0-7, C=color 0-15)
; Maps each TED color to closest C64 color (0-15)
;
; Plus/4 colors:  0=black, 1=white, 2=red, 3=cyan, 4=purple, 5=green
;                 6=blue, 7=yellow, 8=orange, 9=brown, 10=yellow-green
;                 11=pink, 12=blue-green, 13=light blue, 14=dark blue, 15=light green
; C64 colors: 0=black, 1=white, 2=red, 3=cyan, 4=purple, 5=green
;             6=blue, 7=yellow, 8=orange, 9=brown, 10=light red, 11=dark grey
;             12=grey, 13=light green, 14=light blue, 15=light grey
; ============================================================
ted_to_c64_color:
        ; Luminance 0 (darkest) - all map to black or dark colors
        .byte $00, $0B, $02, $03, $04, $05, $06, $07  ; colors 0-7
        .byte $08, $09, $05, $0A, $0B, $0E, $06, $0D  ; colors 8-15
        
        ; Luminance 1
        .byte $00, $0B, $02, $03, $04, $05, $06, $07
        .byte $08, $09, $05, $0A, $0B, $0E, $06, $0D
        
        ; Luminance 2
        .byte $00, $0C, $02, $03, $04, $05, $06, $07
        .byte $08, $09, $05, $0A, $0B, $0E, $06, $0D
        
        ; Luminance 3 - medium luminance
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
        
        ; Luminance 7 (brightest) - TED white (1) maps to C64 white ($01)
        .byte $01, $01, $0A, $03, $0A, $0D, $0E, $07
        .byte $07, $07, $07, $07, $03, $07, $07, $01