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
BANK_ROM    = $04          ; Plus/4 ROMs in bank 4
BANK_RAM    = $05          ; Plus/4 RAM in bank 5

; Memory buffers in bank 0 (host RAM)
; Using addresses above emulator code (which ends around $6000-$7000)
; $8000 was used for ROM staging but is free after init
LOW_RAM_BUFFER = $8000     ; 4KB - Plus/4 low RAM $0000-$0FFF (always resident)
RAM_BUFFER  = $9000        ; 4KB - general RAM buffer for $1000+
ROM_BUFFER  = $A000        ; 4KB - ROM buffer (BASIC or KERNAL)

RAM_BUF_SIZE = $1000       ; 4KB = 16 pages
ROM_BUF_SIZE = $1000       ; 4KB = 16 pages

; State variables
p4_rom_visible:      .byte 1    ; 1 = ROM visible, 0 = RAM only
p4_rom_bank:         .byte 0    ; Which function ROM bank (0-3)

; RAM buffer state: covers 16 consecutive pages
ram_buf_base:        .byte $FF  ; Base page number in RAM buffer ($FF = none)
ram_buf_dirty:       .byte 0    ; Buffer dirty flag

; ROM buffer state: covers 16 consecutive pages  
rom_buf_base:        .byte $FF  ; Base page number in ROM buffer ($FF = none)
rom_buf_is_basic:    .byte 0    ; 0 = KERNAL, 1 = BASIC

; TED registers are defined in plus4_cpu_m65.asm

; --- Cursor overlay state (host-side rendering aid) ---
p4_cur_prev_lo:   .byte $FF   ; $FF = none
p4_cur_prev_hi:   .byte $FF
p4_cur_phase:     .byte 0     ; 0/1 toggle for blink
p4_cur_div:       .byte 0     ; frame divider

; ============================================================
; P4MEM_Init - Initialize memory system
; ============================================================
P4MEM_Init:
        ; Initialize state
        lda #1
        sta p4_rom_visible
        lda #0
        sta p4_rom_bank
        sta ram_buf_dirty
        
        ; No buffers loaded yet
        lda #$FF
        sta ram_buf_base
        sta rom_buf_base
        
        ; Clear TED registers
        ldx #31
        lda #0
_clear_ted:
        sta ted_regs,x
        dex
        bpl _clear_ted
        
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

        rts

; ============================================================
; P4MEM_ClearRAM - Clear all Plus/4 RAM (Bank 5)
; ============================================================
P4MEM_ClearRAM:
        lda #$00
        sta $D707
        .byte $80, $00, $81, $00, $00   ; enhanced DMA options
        .byte $03                       ; fill
        .word $0000                     ; count (0 = 64K)
        .word $0000                     ; fill with $00
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
        cmp #$1C
        beq _read_raster_lo
        cmp #$1D
        beq _read_raster_hi
        
        tax
        lda ted_regs,x
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
; read_from_ram - Read from Plus/4 RAM via buffer
; ============================================================
read_from_ram:
        ; Check if RAM buffer is loaded
        lda ram_buf_base
        cmp #$FF
        beq _rfram_load          ; Buffer not loaded
        
        ; Check if page is in buffer
        lda p4_addr_hi
        sec
        sbc ram_buf_base
        bcc _rfram_load          ; Below buffer range
        cmp #$10
        bcs _rfram_load          ; Above buffer range (16 pages)
        
        ; Page is in buffer - calculate address
        ; high byte = >RAM_BUFFER + page_offset
        clc
        adc #>RAM_BUFFER         ; add to buffer base high byte
        sta _rfram_rd+2          ; self-modify
        ldx p4_addr_lo
_rfram_rd:
        lda RAM_BUFFER,x         ; high byte gets modified
        rts
        
_rfram_load:
        jsr load_ram_buffer
        jmp read_from_ram        ; retry

; ============================================================
; read_from_kernal - Read from KERNAL ROM via buffer
; ============================================================
read_from_kernal:
        ; Check if ROM buffer is loaded
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
        cmp #$10
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
        and #$F0                 ; Align to 16-page boundary
        sta rom_buf_base
        lda #0
        sta rom_buf_is_basic
        
        jsr load_rom_buffer
        jmp read_from_kernal

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
        cmp #$10
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
        and #$F0                 ; Align to 16-page boundary
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
        jsr P4VID_UpdateCursor
_ted_write_done:
; END BLINKING CURSOR OVERLAY
        
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
;  - Plus/4 screen RAM is in LOW_RAM_BUFFER+$0C00 => $8C00
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
    adc #$8C
    sta _scr_restore+2

    ; set host dest page: $08 + (prev_hi & 3)
    lda p4_cur_prev_hi
    and #$03
    clc
    adc #$08
    sta _host_restore+2

    ldx p4_cur_prev_lo
_scr_restore:
    lda $8C00,x
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
    adc #$8C
    sta _scr_set+2

    lda p4_cur_prev_hi
    and #$03
    clc
    adc #$08
    sta _host_set+2

    ldx p4_cur_prev_lo
_scr_set:
    lda $8C00,x
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
; write_to_ram - Write to Plus/4 RAM via buffer (for pages $10+)
; ============================================================
write_to_ram:
        ; Check if RAM buffer is loaded
        lda ram_buf_base
        cmp #$FF
        beq _wtram_load          ; Buffer not loaded
        
        ; Check if page is in buffer
        lda p4_addr_hi
        sec
        sbc ram_buf_base
        bcc _wtram_load
        cmp #$10
        bcs _wtram_load
        
        ; Page is in buffer - calculate address
        clc
        adc #>RAM_BUFFER
        sta _wtram_wr+2          ; self-modify
        ldx p4_addr_lo
        lda p4_data
_wtram_wr:
        sta RAM_BUFFER,x
        
        ; Mark buffer as dirty
        lda #1
        sta ram_buf_dirty
        rts
        
_wtram_load:
        jsr flush_ram_buffer
        jsr load_ram_buffer
        jmp write_to_ram

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

; flush_ram_buffer - Write dirty pages back to Plus/4 RAM
flush_ram_buffer:
        lda ram_buf_dirty
        beq _flush_done          ; Nothing dirty
        
        lda ram_buf_base
        cmp #$FF
        beq _flush_done          ; No buffer loaded
        
        ; DMA copy entire 4KB buffer back to bank 5
        lda ram_buf_base
        sta _flush_dst+1
        
        lda #$00
        sta $D707
        .byte $80, $00, $81, $00, $00
        .byte $00                       ; copy
        .word RAM_BUF_SIZE              ; 4KB
        .byte <RAM_BUFFER, >RAM_BUFFER, $00
_flush_dst:
        .byte $00, $00, BANK_RAM        ; dst hi byte modified
        .byte $00
        .word $0000
        
        lda #0
        sta ram_buf_dirty
        
_flush_done:
        rts

; load_ram_buffer - Load 4KB from Plus/4 RAM into buffer
; Aligns to 16-page boundary based on p4_addr_hi
load_ram_buffer:
        jsr flush_ram_buffer
        
        ; Align to 16-page boundary
        lda p4_addr_hi
        and #$F0
        sta ram_buf_base
        sta _load_ram_src+1
        
        lda #$00
        sta $D707
        .byte $80, $00, $81, $00, $00
        .byte $00                       ; copy
        .word RAM_BUF_SIZE              ; 4KB
_load_ram_src:
        .byte $00, $00, BANK_RAM        ; src hi byte modified
        .byte <RAM_BUFFER, >RAM_BUFFER, $00
        .byte $00
        .word $0000
        rts

; load_rom_buffer - Load 4KB from Plus/4 ROM into buffer
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
; TED to C64 color mapping table (128 entries)
; TED color format: 0LLL CCCC (L=luminance 0-7, C=color 0-15)
; Maps each TED color to closest C64 color (0-15)
;
; Plus/4 colors:  0=black, 1=white, 2=red, 3=cyan, 4=purple, 5=green
;                 6=blue, 7=yellow, 8=orange, 9=brown, 10=yellow-green
;                 11=pink, 12=blue-green, 13=light blue, 14=dark blue, 15=light green
; ============================================================
ted_to_c64_color:
        ; Luminance 0 (darkest) - all map to black or dark colors
        .byte $00, $0F, $02, $03, $04, $05, $06, $07  ; colors 0-7
        .byte $08, $09, $05, $0A, $0B, $0E, $06, $0D  ; colors 8-15
        
        ; Luminance 1
        .byte $00, $0F, $02, $03, $04, $05, $06, $07
        .byte $08, $09, $05, $0A, $0B, $0E, $06, $0D
        
        ; Luminance 2
        .byte $00, $0C, $02, $03, $04, $05, $06, $07
        .byte $08, $09, $05, $0A, $0B, $0E, $06, $0D
        
        ; Luminance 3 - medium luminance (default Plus/4 colors)
        .byte $00, $0F, $02, $03, $04, $05, $06, $07
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
        
        ; Luminance 7 (brightest) - all map to white or light colors
        .byte $0F, $0F, $0A, $03, $0A, $0D, $0E, $07
        .byte $07, $07, $07, $07, $03, $07, $07, $0F