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
        ; Check for I/O area ($FF00-$FF3F)
        cmp #$FF
        bne _read_not_ff
        lda p4_addr_lo
        cmp #$40
        bcs _read_ff40_plus             ; $FF40+ -> KERNAL ROM
        cmp #$20
        bcs _read_ff20_3f               ; $FF20-$FF3F -> not TED, check further
        
        ; TED registers $FF00-$FF1F
        jmp read_ted_register

_read_ff20_3f:
        ; $FF20-$FF3F: Read from KERNAL ROM
        jmp read_from_kernal

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
; read_ted_register - Handle TED register reads $FF00-$FF1F
; ============================================================
read_ted_register:
        lda p4_addr_lo
        cmp #$08
        beq _read_keyboard
        cmp #$15
        beq _read_ff15
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
        bcs _write_to_ram               ; $FF20+ -> RAM under ROM
        
        ; TED registers $FF00-$FF1F
        tax
        lda p4_data
        sta ted_regs,x

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

_write_check_fd:
        lda p4_addr_hi
        cmp #$FD
        bne _write_to_ram
        
        lda p4_addr_lo
        cmp #$30
        bne _write_chk_fdd0
        lda p4_data
        sta $DC00
        rts

_write_chk_fdd0:
        cmp #$D0
        bcc _write_to_ram
        and #$0F
        sta p4_rom_bank
        rts

_write_to_ram:
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