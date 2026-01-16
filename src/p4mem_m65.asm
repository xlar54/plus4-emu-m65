; ============================================================
; p4mem_m65.asm - Memory system for Plus/4 emulator
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
; ============================================================

        .cpu "45gs02"

; Bank numbers
BANK_ROM    = $04          ; Plus/4 ROMs in bank 4
BANK_RAM    = $05          ; Plus/4 RAM in bank 5

; We use a 256-byte page buffer at $4000 in bank 0 (safe RAM area)
PAGE_BUFFER = $4000

; State variables
p4_rom_visible:      .byte 1    ; 1 = ROM visible, 0 = RAM only
p4_rom_bank:         .byte 0    ; Which function ROM bank (0-3)
current_page_num:    .byte $FF  ; Which page is in buffer ($FF = none)
current_page_is_rom: .byte 0    ; 0 = RAM page, 1 = ROM page
page_dirty:          .byte 0    ; 1 = buffer modified, needs writeback

; TED registers are defined in plus4_cpu_m65.asm
; ted_regs, ted_timer1_lo/hi, ted_raster_lo/hi

; ============================================================
; P4MEM_Init
; Initialize memory system
; ============================================================
P4MEM_Init:
        ; Initialize state
        lda #1
        sta p4_rom_visible
        lda #0
        sta p4_rom_bank
        
        ; No page loaded yet
        lda #$FF
        sta current_page_num
        lda #0
        sta current_page_is_rom
        sta page_dirty
        
        ; Clear TED registers
        ldx #31
        lda #0
_clear_ted:
        sta ted_regs,x
        dex
        bpl _clear_ted
        
        sta ted_timer1_lo
        sta ted_timer1_hi
        sta ted_raster_lo
        sta ted_raster_hi
        
        ; Clear Plus/4 RAM (Bank 5) to zero
        jsr P4MEM_ClearRAM

        rts

; ============================================================
; P4MEM_ClearRAM
; Clear all of Plus/4 RAM (Bank 5) to zero
; ============================================================
P4MEM_ClearRAM:
        ; use DMA to clear bank 5 (inline, no modification needed)
        lda #$00
        sta $D707
        .byte $80                       ; enhanced dma - src bits 20-27
        .byte $00                       ; src MB
        .byte $81                       ; enhanced dma - dest bits 20-27
        .byte $00                       ; dest MB
        .byte $00                       ; end of job options
        .byte $03                       ; 3 = fill                                 
        .word $0000                     ; count (0 = 64K)
        .word $0000                     ; fill with $00
        .byte $00                       ; unused with fill
        .word $0000                     ; destination start
        .byte BANK_RAM                  ; dest bank 5
        .byte $00                       ; command high byte
        .word $0000                     ; modulo (ignored)
        
        rts

; ============================================================
; P4MEM_Read
; Input: p4_addr_hi:p4_addr_lo = Plus/4 address to read
; Output: A = byte read
; ============================================================
P4MEM_Read:
        lda p4_addr_hi
        cmp #$80
        bcs _read_high_memory
        
        ; --- Low RAM $0000-$7FFF: Always from RAM bank ---
        jmp read_from_ram

_read_high_memory:
        ; Check for I/O area first ($FF00-$FF3F)
        cmp #$FF
        bne _read_not_io
        lda p4_addr_lo
        cmp #$40
        bcs _read_kernal_high     ; $FF40-$FFFF is KERNAL ROM
        cmp #$20
        bcs _read_not_ted_io      ; $FF20-$FF3F: other I/O
        
        ; --- TED registers $FF00-$FF1F ---
        cmp #$08
        beq _read_keyboard
        cmp #$1C
        beq _read_raster_lo
        cmp #$1D
        beq _read_raster_hi
        
        ; Default: read from TED shadow
        tax
        lda ted_regs,x
        rts

_read_keyboard:
        ; Read C64 keyboard matrix
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
        ; $FF20-$FF3F: Minimal I/O handling - fall through to KERNAL
        
_read_kernal_high:
        ; $FF40-$FFFF: Always KERNAL ROM
        jmp read_from_kernal

_read_not_io:
        ; Check for $FDxx (ACIA, etc)
        lda p4_addr_hi
        cmp #$FD
        bne _read_not_fd
        
        lda p4_addr_lo
        cmp #$30
        beq _read_fd30           ; Keyboard row select
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
        lda #$10                 ; Transmit buffer empty
        rts

_read_not_fd:
        ; Check for $FC00-$FCFF (always KERNAL)
        lda p4_addr_hi
        cmp #$FC
        beq read_from_kernal
        
        ; Check ROM visibility for $8000-$FBFF
        lda p4_rom_visible
        beq read_from_ram        ; ROM not visible, read RAM
        
        ; ROM visible - determine which ROM
        lda p4_addr_hi
        cmp #$C0
        bcs read_from_kernal     ; $C000-$FBFF = KERNAL
        ; $8000-$BFFF = BASIC
        jmp read_from_basic

; ============================================================
; read_from_ram - Read byte from Plus/4 RAM (Bank 5)
; ============================================================
read_from_ram:
        ; Check if we have the right page loaded
        lda p4_addr_hi
        cmp current_page_num
        bne _rfr_load
        lda current_page_is_rom
        beq _rfr_read            ; Already have correct RAM page
        
_rfr_load:
        jsr flush_if_dirty
        jsr load_ram_page
        
_rfr_read:
        ldx p4_addr_lo
        lda PAGE_BUFFER,x
        rts

; ============================================================
; read_from_kernal - Read byte from KERNAL ROM (Bank 4, $C000+)
; ============================================================
read_from_kernal:
        ; Check if we have the right page loaded
        lda p4_addr_hi
        cmp current_page_num
        bne _rfk_load
        lda current_page_is_rom
        bne _rfk_read            ; Already have correct ROM page
        
_rfk_load:
        jsr flush_if_dirty
        jsr load_rom_page
        
_rfk_read:
        ldx p4_addr_lo
        lda PAGE_BUFFER,x
        rts

; ============================================================
; read_from_basic - Read byte from BASIC ROM (Bank 4, $8000+)
; ============================================================
read_from_basic:
        ; Check if we have the right page loaded
        lda p4_addr_hi
        cmp current_page_num
        bne _rfb_load
        lda current_page_is_rom
        bne _rfb_read            ; Already have correct ROM page
        
_rfb_load:
        jsr flush_if_dirty
        jsr load_rom_page
        
_rfb_read:
        ldx p4_addr_lo
        lda PAGE_BUFFER,x
        rts

; ============================================================
; P4MEM_Write
; Input: p4_addr_hi:p4_addr_lo = Plus/4 address
;        p4_data = byte to write
; ============================================================
P4MEM_Write:
        lda p4_addr_hi
        cmp #$FF
        beq _write_ff_page
        jmp _write_check_fd

_write_ff_page:
        ; Check for ROM bank switching
        lda p4_addr_lo
        cmp #$3F
        bne _write_not_ff3f
        ; $FF3F: Make RAM visible (ROM disabled)
        lda #0
        sta p4_rom_visible
        rts

_write_not_ff3f:
        cmp #$3E
        bne _write_not_ff3e
        ; $FF3E: Make ROM visible
        lda #1
        sta p4_rom_visible
        rts

_write_not_ff3e:
        ; Check for TED registers $FF00-$FF1F
        cmp #$20
        bcs _write_not_ted
        
        ; TED register write
        tax
        lda p4_data
        sta ted_regs,x
        
        ; Handle special registers
        cpx #$01
        beq _write_ff01          ; Timer reload
        cpx #$09
        beq _write_ff09          ; IRQ acknowledge
        cpx #$15
        beq _write_ff15          ; Background color
        cpx #$19
        beq _write_ff19          ; Border color
        rts

_write_ff01:
        ; Writing Timer 1 high byte reloads timer
        lda ted_regs+$00
        sta ted_timer1_lo
        lda ted_regs+$01
        sta ted_timer1_hi
        rts

_write_ff09:
        ; Acknowledge interrupts (write 1 to clear)
        lda p4_data
        eor #$FF
        and ted_regs+$09
        sta ted_regs+$09
        rts

_write_ff15:
        ; Background color -> C64 $D021
        lda p4_data
        and #$0F
        tax
        lda p4_to_c64_color,x
        sta $D021
        rts

_write_ff19:
        ; Border color -> C64 $D020
        lda p4_data
        and #$0F
        tax
        lda p4_to_c64_color,x
        sta $D020
        rts

_write_not_ted:
        ; Other $FFxx - write to RAM underneath
        jmp write_to_ram

_write_check_fd:
        lda p4_addr_hi
        cmp #$FD
        bne write_to_ram
        
        lda p4_addr_lo
        cmp #$30
        bne _write_chk_fdd0
        ; $FD30: Keyboard column select
        lda p4_data
        sta $DC00
        rts

_write_chk_fdd0:
        cmp #$D0
        bcc write_to_ram
        ; $FDD0-$FDDF: ROM bank select
        and #$0F
        sta p4_rom_bank
        rts

; ============================================================
; write_to_ram - Write byte to Plus/4 RAM (Bank 5)
; ============================================================
write_to_ram:
        ; Check if we have the right page loaded
        lda p4_addr_hi
        cmp current_page_num
        bne _wtr_load
        lda current_page_is_rom
        beq _wtr_write           ; Already have correct RAM page
        
_wtr_load:
        jsr flush_if_dirty
        jsr load_ram_page
        
_wtr_write:
        ; Write to page buffer
        ldx p4_addr_lo
        lda p4_data
        sta PAGE_BUFFER,x
        
        ; Mark dirty
        lda #1
        sta page_dirty
        
        ; --- Screen mirroring ---
        ; Plus/4 screen $0C00-$0FFF -> MEGA65 $0800-$0BFF
        lda p4_addr_hi
        cmp #$0C
        bcc _write_check_color
        cmp #$10
        bcs _write_done
        
        ; Map $0Cxx-$0Fxx to $08xx-$0Bxx
        lda p4_addr_hi
        sec
        sbc #$04                 ; $0C->$08, $0D->$09, etc
        sta _scr_st+2            ; Self-modify high byte
        lda p4_data
_scr_st sta $0800,x              ; X still has p4_addr_lo
        rts

_write_check_color:
        ; Plus/4 color $0800-$0BFF -> C64 color $D800-$DBFF
        cmp #$08
        bcc _write_done
        cmp #$0C
        bcs _write_done
        
        ; Map $08xx-$0Bxx to $D8xx-$DBxx
        lda p4_addr_hi
        clc
        adc #$D0                 ; $08->$D8, $09->$D9, etc
        sta _col_st+2            ; Self-modify high byte
        lda p4_data
        and #$0F
        tay
        lda p4_to_c64_color,y
_col_st sta $D800,x              ; X still has p4_addr_lo

_write_done:
        rts

; ============================================================
; Page management routines
; ============================================================

; flush_if_dirty - Write page buffer back to RAM if modified
flush_if_dirty:
        lda page_dirty
        beq fid_done
        lda current_page_is_rom
        bne fid_clear            ; Don't write back ROM pages
        
        ; Set destination high byte in the DMA data
        lda current_page_num
        sta dma_flush_data+12    ; dst hi is at offset 12
        
        ; Execute the DMA by jumping to the trigger code
        jsr do_dma_flush
        
fid_clear:
        lda #0
        sta page_dirty
        
fid_done:
        rts

; load_ram_page - Load page from Plus/4 RAM (Bank 5) into buffer
load_ram_page:
        lda p4_addr_hi
        sta current_page_num
        sta dma_ram_data+9       ; src hi is at offset 9
        lda #0
        sta current_page_is_rom
        sta page_dirty
        
        ; Execute the DMA
        jsr do_dma_ram
        rts

; load_rom_page - Load page from Plus/4 ROM (Bank 4) into buffer
load_rom_page:
        lda p4_addr_hi
        sta current_page_num
        sta dma_rom_data+9       ; src hi is at offset 9
        lda #1
        sta current_page_is_rom
        lda #0
        sta page_dirty
        
        ; Execute the DMA
        jsr do_dma_rom
        rts

; ============================================================
; DMA trigger routines - each has inline data following sta $D707
; ============================================================

do_dma_flush:
        lda #$00
        sta $D707
dma_flush_data:
        .byte $80                ; +0: enhanced dma - src bits 20-27
        .byte $00                ; +1: src MB
        .byte $81                ; +2: enhanced dma - dest bits 20-27
        .byte $00                ; +3: dest MB
        .byte $00                ; +4: end of job options
        .byte $00                ; +5: command = copy
        .word $0100              ; +6,+7: count = 256 bytes
        .byte <PAGE_BUFFER       ; +8: src lo
        .byte >PAGE_BUFFER       ; +9: src hi
        .byte $00                ; +10: src bank 0
        .byte $00                ; +11: dst lo
        .byte $00                ; +12: dst hi (MODIFIED)
        .byte BANK_RAM           ; +13: dst bank 5
        .byte $00                ; +14: command high byte
        .word $0000              ; +15,+16: modulo
        rts

do_dma_ram:
        lda #$00
        sta $D707
dma_ram_data:
        .byte $80                ; +0: enhanced dma - src bits 20-27
        .byte $00                ; +1: src MB
        .byte $81                ; +2: enhanced dma - dest bits 20-27
        .byte $00                ; +3: dest MB
        .byte $00                ; +4: end of job options
        .byte $00                ; +5: command = copy
        .word $0100              ; +6,+7: count = 256 bytes
        .byte $00                ; +8: src lo
        .byte $00                ; +9: src hi (MODIFIED)
        .byte BANK_RAM           ; +10: src bank 5
        .byte <PAGE_BUFFER       ; +11: dst lo
        .byte >PAGE_BUFFER       ; +12: dst hi
        .byte $00                ; +13: dst bank 0
        .byte $00                ; +14: command high byte
        .word $0000              ; +15,+16: modulo
        rts

do_dma_rom:
        lda #$00
        sta $D707
dma_rom_data:
        .byte $80                ; +0: enhanced dma - src bits 20-27
        .byte $00                ; +1: src MB
        .byte $81                ; +2: enhanced dma - dest bits 20-27
        .byte $00                ; +3: dest MB
        .byte $00                ; +4: end of job options
        .byte $00                ; +5: command = copy
        .word $0100              ; +6,+7: count = 256 bytes
        .byte $00                ; +8: src lo
        .byte $00                ; +9: src hi (MODIFIED)
        .byte BANK_ROM           ; +10: src bank 4
        .byte <PAGE_BUFFER       ; +11: dst lo
        .byte >PAGE_BUFFER       ; +12: dst hi
        .byte $00                ; +13: dst bank 0
        .byte $00                ; +14: command high byte
        .word $0000              ; +15,+16: modulo
        rts

; ============================================================
; Plus/4 to C64 color mapping
; ============================================================
p4_to_c64_color:
        .byte $00, $0F, $08, $07, $0A, $0C, $00, $0D
        .byte $06, $09, $02, $04, $0E, $05, $03, $01