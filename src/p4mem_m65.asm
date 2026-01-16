; ============================================================
; p4mem_m65.asm - Memory system for Plus/4 emulator
; Host: MEGA65
; Assembler: 64tass (with 45GS02 support)
; ============================================================
;
; Memory Map:
;   Bank 4 ($40000-$4FFFF): Plus/4 RAM (64KB)
;   Bank 5 ($50000-$5FFFF): Plus/4 ROMs
;     $50000-$57FFF: (unused/future expansion)
;     $58000-$5BFFF: BASIC ROM (mapped to Plus/4 $8000-$BFFF)
;     $5C000-$5FFFF: KERNAL ROM (mapped to Plus/4 $C000-$FFFF)
;
; ============================================================

        .cpu "45gs02"

; Bank numbers for 32-bit addressing
BANK_RAM    = $05          ; Plus/4 RAM in bank 5
BANK_ROM    = $04          ; Plus/4 ROMs in bank 4

; Zero page pointers for 32-bit addressing (need 4 bytes each)
; Using ZP locations that won't conflict with emulator state
mem_ptr     = $FB          ; 4-byte pointer: $FB-$FE

; State variables
p4_rom_visible: .byte 1    ; 1 = ROM visible, 0 = RAM only
p4_rom_bank:    .byte 0    ; Which function ROM bank (0-3)
p4_screen_base: .byte $0C  ; Screen memory base page

; TED registers are defined in plus4_cpu_m65.asm
; ted_regs, ted_timer1_lo/hi, ted_raster_lo/hi

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
        jmp _read_from_ram

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
        ; Read C64 keyboard matrix (directly compatible enough for basic use)
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
        ; $FF20-$FF3F: Minimal I/O handling
        lda p4_addr_lo
        ; Fall through to KERNAL read for now
        
_read_kernal_high:
        ; $FF40-$FFFF: Always KERNAL ROM
        jmp _read_from_kernal

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
        beq _read_from_kernal
        
        ; Check ROM visibility for $8000-$FBFF
        lda p4_rom_visible
        beq _read_from_ram       ; ROM not visible, read RAM
        
        ; ROM visible - determine which ROM
        lda p4_addr_hi
        cmp #$C0
        bcs _read_from_kernal    ; $C000-$FBFF = KERNAL
        ; $8000-$BFFF = BASIC
        jmp _read_from_basic

; --- Read from Plus/4 RAM (Bank 4) ---
_read_from_ram:
        ; Set up 32-bit pointer to bank 4
        lda p4_addr_lo
        sta mem_ptr
        lda p4_addr_hi
        sta mem_ptr+1
        lda #BANK_RAM
        sta mem_ptr+2
        lda #$00
        sta mem_ptr+3
        
        ; Use 32-bit indirect addressing
        ldz #$00
        nop                      ; Required before 32-bit ops on some revisions
        lda [mem_ptr],z
        rts

; --- Read from KERNAL ROM (Bank 5, offset $C000) ---
_read_from_kernal:
        ; KERNAL is at $5C000-$5FFFF (Plus/4 $C000-$FFFF -> Bank 5 + $C000)
        lda p4_addr_lo
        sta mem_ptr
        lda p4_addr_hi
        sta mem_ptr+1
        lda #BANK_ROM
        sta mem_ptr+2
        lda #$00
        sta mem_ptr+3
        
        ldz #$00
        nop
        lda [mem_ptr],z
        rts

; --- Read from BASIC ROM (Bank 5, offset $8000) ---
_read_from_basic:
        ; BASIC is at $58000-$5BFFF (Plus/4 $8000-$BFFF -> Bank 5 + $8000)
        lda p4_addr_lo
        sta mem_ptr
        lda p4_addr_hi
        sta mem_ptr+1
        lda #BANK_ROM
        sta mem_ptr+2
        lda #$00
        sta mem_ptr+3
        
        ldz #$00
        nop
        lda [mem_ptr],z
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
        ; Other $FFxx - ignore (ROM area)
        rts

_write_check_fd:
        lda p4_addr_hi
        cmp #$FD
        bne _write_to_ram
        
        lda p4_addr_lo
        cmp #$30
        bne _write_chk_fdd0
        ; $FD30: Keyboard column select
        lda p4_data
        sta $DC00
        rts

_write_chk_fdd0:
        cmp #$D0
        bcc _write_to_ram
        ; $FDD0-$FDDF: ROM bank select
        and #$0F
        sta p4_rom_bank
        rts

_write_to_ram:
        ; All RAM writes go to Bank 5
        ; Set up 32-bit pointer
        lda p4_addr_lo
        sta mem_ptr
        lda p4_addr_hi
        sta mem_ptr+1
        lda #BANK_RAM
        sta mem_ptr+2
        lda #$00
        sta mem_ptr+3
        
        ; Write using 32-bit addressing
        lda p4_data
        ldz #$00
        nop
        sta [mem_ptr],z
        
        ; --- Screen mirroring for display ---
        ; Mirror $0C00-$0FFF -> C64 $0400-$07FF (screen RAM)
        lda p4_addr_hi
        cmp #$0C
        bcc _write_check_color
        cmp #$10
        bcs _write_done
        
        ; Screen char: $0Cxx -> $04xx
        sec
        sbc #$08
        sta mem_ptr+1
        lda p4_addr_lo
        sta mem_ptr
        lda #$00
        sta mem_ptr+2
        sta mem_ptr+3
        lda p4_data
        ldz #$00
        sta [mem_ptr],z
        rts

_write_check_color:
        ; Mirror $0800-$0BFF -> C64 $D800-$DBFF (color RAM)
        cmp #$08
        bcc _write_done
        cmp #$0C
        bcs _write_done
        
        ; Color: $08xx -> $D8xx
        clc
        adc #$D0
        sta mem_ptr+1
        lda p4_addr_lo
        sta mem_ptr
        lda #$00
        sta mem_ptr+2
        sta mem_ptr+3
        ; Convert Plus/4 color to C64
        lda p4_data
        and #$0F
        tax
        lda p4_to_c64_color,x
        ldz #$00
        sta [mem_ptr],z

_write_done:
        rts

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
        
        ; Clear Plus/4 RAM (Bank 4) to zero
        jsr P4MEM_ClearRAM

        rts

; ============================================================
; P4MEM_ClearRAM
; Clear all of Plus/4 RAM (Bank 5) to zero
; ============================================================
P4MEM_ClearRAM:
        ; use DMA to clear bank 5
        lda #$00
        sta $D707
        .byte $80                       ; enhanced dma - src bits 20-27
        .byte $00   ; src hi
        .byte $81                       ; enhanced dma - dest bits 20-27
        .byte $00   ; dest hi
        .byte $00                       ; end of job options
        .byte $03                       ; 3 = fill                                 
        .word $FFFF                     ; count of bytes
        .word $0000                     ; fill with $00 (second byte)
        .byte $00                       ; unusued with fill
        .word $0000                     ; destination start
        .byte $05                       ; dest bank 5
        .byte $00                       ; command high byte
        .word $0000                     ; modulo (ignored)
        
        rts

; ============================================================
; Plus/4 to C64 color mapping
; ============================================================
p4_to_c64_color:
        .byte $00, $0F, $08, $07, $0A, $0C, $00, $0D
        .byte $06, $09, $02, $04, $0E, $05, $03, $01