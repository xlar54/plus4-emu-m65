; ============================================================
; main_m65.asm - Plus/4 Emulator for MEGA65
; Assembler: 64tass

; BANK 4 = Plus 4 ROM
; BANK 5 = Plus 4 RAM
; ============================================================

        .cpu "45gs02"
        
        * = $2001

; BASIC stub to auto-start
        .word (+), 2025
        .byte $fe, $02, $30     ; BANK 0
        .byte ':'
        .byte $9e               ; SYS
        .text "8210"            ; Start address
        .byte 0
+       .word 0

        * = $2012

; MEGA65 / C65 KERNAL calls
SETNAM  = $FFBD
SETLFS  = $FFBA
SETBNK  = $FF6B
LOAD    = $FFD5
CHROUT  = $FFD2
GETIN   = $FFE4
OPEN    = $FFC0
CLOSE   = $FFC3
CHKIN   = $FFC6
CHKOUT  = $FFC9
CHRIN   = $FFCF
CLRCHN  = $FFCC
READST  = $FFB7

; MEGA65 DMA controller
DMA_REG = $D707

; ============================================================
; Entry point
; ============================================================
start:
        ; Enable MEGA65 mode and fast CPU
        lda #$47
        sta $D02F
        lda #$53
        sta $D02F              ; Unlock MEGA65 registers


        ; Print banner
        ldx #0
_banner:
        lda banner_msg,x
        beq _banner_done
        jsr CHROUT
        inx
        bne _banner
_banner_done:

        ; use DMA to clear bank 4
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
        .byte $04                       ; dest bank 4
        .byte $00                       ; command high byte
        .word $0000                     ; modulo (ignored)

        lda #$00
        ldx #$00
        jsr SETBNK

        lda #$00
        ldx #$08
        ldy #$00
        jsr setlfs

        ;=============================================================================
        ; Load KERNAL ROM
        ;==============================================================================

        ; ----------------------------------------------------
        ; 1) Load KERNAL ROM to staging area of $8000 bank 0
        ; ----------------------------------------------------
        ldx #<kernal_name
        ldy #>kernal_name
        lda #kernal_name_end - kernal_name
        jsr SETNAM
        lda #$01
        ldx #$08
        ldy #$00
        jsr SETLFS
        
        ; Load to $8000
        lda #$40               ; Load to specified address
        ldx #<$8000
        ldy #>$8000
        jsr LOAD
        bcc _kernal_ok
        jmp load_fail
        
_kernal_ok:
        ; Print status
        ldx #0
_k_msg: lda kernal_ok_msg,x
        beq _k_done
        jsr CHROUT
        inx
        bne _k_msg
_k_done:

        ; ----------------------------------------------------
        ; DMA copy $08000 (staging) to $4C000 (+4 BASIC ROM)
        ; ----------------------------------------------------
        lda #$00
        sta $D707
        .byte $80                       ; enhanced dma - src bits 20-27
        .byte $00                       ; src MB
        .byte $81                       ; enhanced dma - dest bits 20-27
        .byte $00                       ; dest MB
        .byte $00                       ; end of job options
        .byte $00                       ; copy                                 
        .byte $00, $40                  ; length lsb, msb = $4000
        .byte $00, $80, $00             ; src lsb, msb, bank 0
        .byte $00, $C0, $04             ; dest lsb, msb, bank 4
        .byte $00                       ; command high byte
        .word $0000                     ; modulo (ignored)

        ;=============================================================================
        ; Load BASIC ROM
        ;==============================================================================

        ; ----------------------------------------------------
        ; 1) Load BASIC ROM to staging area of $8000 bank 0
        ; ----------------------------------------------------
        ldx #<basic_name
        ldy #>basic_name
        lda #basic_name_end - basic_name
        jsr SETNAM
        lda #$01
        ldx #$08
        ldy #$00
        jsr SETLFS
        
        ; Load to $8000 
        lda #$40
        ldx #<$8000
        ldy #>$8000
        jsr LOAD
        bcc _basic_ok
        jmp load_fail
        
_basic_ok:
        ; Print status
        ldx #0
_b_msg: lda basic_ok_msg,x
        beq _b_done
        jsr CHROUT
        inx
        bne _b_msg
_b_done:

        ; ----------------------------------------------------
        ; DMA copy $08000 (staging) to $48000 (+4 KERNAL ROM)
        ; ----------------------------------------------------
        lda #$00
        sta $D707
        .byte $80                       ; enhanced dma - src bits 20-27
        .byte $00                       ; src MB
        .byte $81                       ; enhanced dma - dest bits 20-27
        .byte $00                       ; dest MB
        .byte $00                       ; end of job options
        .byte $00                       ; copy                                 
        .byte $00, $40                  ; length lsb, msb = $4000
        .byte $00, $80, $00             ; src lsb, msb, bank 0
        .byte $00, $80, $04             ; dest lsb, msb, bank 4
        .byte $00                       ; command high byte
        .word $0000                     ; modulo (ignored)


        ; ----------------------------------------------------
        ; 3) Initialize memory system (clears Plus/4 RAM in bank 5)
        ; ----------------------------------------------------
        ldx #0
_init_msg:
        lda init_msg,x
        beq _init_done
        jsr CHROUT
        inx
        bne _init_msg
_init_done:

        jsr P4MEM_Init

        ldx #0
_ready_msg:
        lda ready_msg,x
        beq _ready_done
        jsr CHROUT
        inx
        bne _ready_msg
_ready_done:

        ; Wait for keypress
        ldx #0
_press: lda press_msg,x
        beq _wait
        jsr CHROUT
        inx
        bne _press
_wait:
        jsr GETIN
        beq _wait


        ; ----------------------------------------------------
        ; Reset Plus/4 CPU and start emulation
        ; ----------------------------------------------------
        ; Initialize VIC-IV video settings NOW (after all KERNAL calls)
        jsr P4MEM_InitVideo
        
        jsr P4CPU_Reset
        
main_loop:

        ; --- Check if print requested ---
        lda p4h_print_pending
        beq _no_print_start
        jsr P4Host_StartPrint

_no_print_start:
        jsr P4CPU_Step

        ; --- If printing, check if done ---
        lda p4h_print_active
        beq _no_print_check
        jsr P4Host_CheckPrintDone   ; C=1 when done

_no_print_check:

        jmp main_loop

; ============================================================
; Load failure handler
; ============================================================
load_fail:
        jsr print_hex8

        ; Reset bank
        lda #$00
        sta $D702
        
        ldx #0
_lf:    lda fail_msg,x
        beq _lf_hang
        jsr CHROUT
        inx
        bne _lf
_lf_hang:
        jmp _lf_hang

;=============================================================
; emulator debug
;=============================================================
output_emulator_steps:
    ; print: PC=HHLL OP=BB A=AA X=XX Y=YY SP=SS P=PP
    ; Now prints BEFORE execution, so use p4_pc directly
    lda #'p'
    jsr CHROUT
    lda #'c'
    jsr CHROUT
    lda #'='
    jsr CHROUT
    lda p4_pc_hi
    jsr print_hex8
    lda p4_pc_lo
    jsr print_hex8

    lda #' '
    jsr CHROUT
    lda #'o'
    jsr CHROUT
    lda #'p'
    jsr CHROUT
    lda #'='
    jsr CHROUT

    ; read opcode at guest PC (instruction start)
    lda p4_pc_lo
    sta p4_addr_lo
    lda p4_pc_hi
    sta p4_addr_hi
    jsr P4MEM_Read
    jsr print_hex8

    ; A
    lda #' '
    jsr CHROUT
    lda #'a'
    jsr CHROUT
    lda #'='
    jsr CHROUT
    lda p4_a
    jsr print_hex8

    ; X
    lda #' '
    jsr CHROUT
    lda #'x'
    jsr CHROUT
    lda #'='
    jsr CHROUT
    lda p4_x
    jsr print_hex8

    ; Y
    lda #' '
    jsr CHROUT
    lda #'y'
    jsr CHROUT
    lda #'='
    jsr CHROUT
    lda p4_y
    jsr print_hex8

    ; SP
    lda #' '
    jsr CHROUT
    lda #'s'
    jsr CHROUT
    lda #'p'
    jsr CHROUT
    lda #'='
    jsr CHROUT
    lda p4_sp
    jsr print_hex8

    ; P
    lda #' '
    jsr CHROUT
    lda #'p'
    jsr CHROUT
    lda #'='
    jsr CHROUT
    lda p4_p
    jsr print_hex8

    lda #$0d
    jsr CHROUT

    ;jsr wait_key

_done:
    rts

; ============================================================
; print_hex8 - Print A as 2 hex digits
; ============================================================
print_hex8:
        pha
        lsr
        lsr
        lsr
        lsr
        jsr _print_nybble
        pla
        and #$0F
_print_nybble:
        cmp #$0A
        bcc _digit
        adc #$06
_digit:
        adc #$30
        jmp CHROUT


; ============================================================
; Messages
; ============================================================
banner_msg:
        .byte $93, 27, 52
        .text "plus/4 emulator for mega65"
        .byte $0D, 0

kernal_name:
        .text "kernal.bin"
kernal_name_end:

basic_name:
        .text "basic.bin"
basic_name_end:

kernal_ok_msg:
        .text "kernal loaded to bank 4"
        .byte $0D, 0

basic_ok_msg:
        .text "basic loaded to bank 4"
        .byte $0D, 0

init_msg:
        .text "clearing plus/4 ram in bank 5..."
        .byte $0D, 0

ready_msg:
        .text "ready."
        .byte $0D, 0

press_msg:
        .text "press any key to start"
        .byte $0D, 0

fail_msg:
        .text "?load error"
        .byte $0D, 0

; ============================================================
; Include emulator components
; ============================================================
        .include "plus4_cpu_m65.asm"
        .include "p4hooks.asm"
        .include "p4mem_m65.asm"
        .include "plus4_sound_m65.asm"
        .include "p4host.asm"
        