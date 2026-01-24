; ============================================================
; plus4_cpu.asm - Plus/4 CPU core - ALL 151 LEGAL OPCODES
; Assembler: 64tass
; ============================================================

        .cpu "45gs02"

; Zero-page CPU state
p4_a        = $02
p4_x        = $03
p4_y        = $04
p4_sp       = $05
p4_p        = $06
p4_pc_lo    = $07
p4_pc_hi    = $08
p4_addr_lo  = $09
p4_addr_hi  = $0A
p4_data     = $0b
p4_vec_lo   = $0c
p4_vec_hi   = $0d
p4_tmp      = $0e
p4_tmp2     = $0f
p4_cycles   = $10
p4_xtra     = $11
p4_dec_a    = $12           ; Saved A for decimal mode ADC/SBC
p4_inst_pc_lo = $1A
p4_inst_pc_hi = $1B
p4_irq_pending = $19
p4_nmi_pending = $1C


; Status flags
P_C = %00000001
P_Z = %00000010
P_I = %00000100
P_D = %00001000
P_B = %00010000
P_U = %00100000
P_V = %01000000
P_N = %10000000

; TED timing constants (PAL)
TED_CYCLES_PER_LINE = 57    ; ~57 cycles per scanline
TED_LINES_PER_FRAME = 312   ; PAL has 312 lines

; --- set_zn_a: set Z and N flags from A ---
set_zn_a:
;        pha
;        lda p4_p
;        and #(~(P_Z|P_N)) & $ff
;        sta p4_p
;        pla
;        pha
;        beq _set_zn_z
;        jmp _set_zn_chk_n
;_set_zn_z:
;        lda p4_p
;        ora #P_Z
;        sta p4_p
;_set_zn_chk_n:
;        pla
;        bmi _set_zn_n
;        rts
;_set_zn_n:
;        lda p4_p
;        ora #P_N
 ;       sta p4_p
;        rts

        pha                     ; preserve A for caller
        phx                     ; preserve X for caller

        tax                     ; X = result byte (original A)
        lda p4_p
        and #(~(P_Z|P_N)) & $ff  ; clear old Z/N
        ora zn_table,x           ; OR in new Z/N
        sta p4_p

        plx
        pla
        rts

; ------------------------------------------------------------
; Z/N lookup table: entry = (val==0 ? P_Z : 0) | (val&$80 ? P_N : 0)
; ------------------------------------------------------------
zn_table:
        .byte $02,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

        .byte $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
        .byte $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
        .byte $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
        .byte $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
        .byte $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
        .byte $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
        .byte $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
        .byte $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80


; --- inc_pc ---
inc_pc:
        inw p4_pc_lo
        rts

; --- fetch8: A = mem[PC], PC++ ---
fetch8:
        lda p4_pc_lo
        sta p4_addr_lo
        lda p4_pc_hi
        sta p4_addr_hi
        jsr P4MEM_Read
        inw p4_pc_lo            ; Inlined inc_pc
        rts

; --- fetch16_to_addr ---
fetch16_to_addr:
        jsr fetch8
        sta p4_tmp
        jsr fetch8
        sta p4_addr_hi
        lda p4_tmp
        sta p4_addr_lo
        rts

; --- push_data ---
push_data:
        ; Optimized push - direct access to LOW_RAM_BUFFER stack area
        ldx p4_sp
        lda p4_data
        sta LOW_RAM_BUFFER+$0100,x             ; Stack is at LOW_RAM_BUFFER+100 - LOW_RAM_BUFFER+1FF
        dec p4_sp
        rts

; --- pull_to_a ---
pull_to_a:
        ; Optimized pull - direct access to LOW_RAM_BUFFER stack area
        inc p4_sp
        ldx p4_sp
        lda LOW_RAM_BUFFER+$0100,x             ; Stack is at LOW_RAM_BUFFER+100 - LOW_RAM_BUFFER+1FF
        rts

; --- Addressing modes ---
addr_zp:
        jsr fetch8
        sta p4_addr_lo
        lda #$00
        sta p4_addr_hi
        rts

addr_zpx:
        jsr fetch8
        clc
        adc p4_x
        sta p4_addr_lo
        lda #$00
        sta p4_addr_hi
        rts

addr_zpy:
        jsr fetch8
        clc
        adc p4_y
        sta p4_addr_lo
        lda #$00
        sta p4_addr_hi
        rts

addr_abs:
        jmp fetch16_to_addr

addr_absx:
        jsr fetch16_to_addr
        lda #$00
        sta p4_xtra
        lda p4_addr_lo
        clc
        adc p4_x
        sta p4_addr_lo
        bcc _absx_nc
        inc p4_addr_hi
        lda #$01
        sta p4_xtra
_absx_nc:
        rts

addr_absy:
        jsr fetch16_to_addr
        lda #$00
        sta p4_xtra
        lda p4_addr_lo
        clc
        adc p4_y
        sta p4_addr_lo
        bcc _absy_nc
        inc p4_addr_hi
        lda #$01
        sta p4_xtra
_absy_nc:
        rts

addr_indx:
        jsr fetch8
        clc
        adc p4_x
        tay
        sty p4_addr_lo
        lda #$00
        sta p4_addr_hi
        jsr P4MEM_Read
        sta p4_tmp
        iny
        tya
        sta p4_addr_lo
        lda #$00
        sta p4_addr_hi
        jsr P4MEM_Read
        sta p4_tmp2
        lda p4_tmp
        sta p4_addr_lo
        lda p4_tmp2
        sta p4_addr_hi
        rts

addr_indy:
        jsr fetch8
        tay
        lda #$00
        sta p4_xtra
        sty p4_addr_lo
        sta p4_addr_hi
        jsr P4MEM_Read
        sta p4_tmp
        iny
        tya
        sta p4_addr_lo
        lda #$00
        sta p4_addr_hi
        jsr P4MEM_Read
        sta p4_tmp2
        lda p4_tmp
        clc
        adc p4_y
        sta p4_addr_lo
        lda p4_tmp2
        adc #$00
        sta p4_addr_hi
        lda p4_tmp
        clc
        adc p4_y
        bcc _indy_nc
        lda #$01
        sta p4_xtra
_indy_nc:
        rts

addr_ind_jmp:
        jsr fetch16_to_addr

;debug
        ;  save the pointer address (e.g. $0318)
        lda p4_addr_lo
        sta p4_vec_lo
        lda p4_addr_hi
        sta p4_vec_hi
;

        jsr P4MEM_Read
        sta p4_tmp
        inc p4_addr_lo
        jsr P4MEM_Read
        sta p4_tmp2

        lda p4_tmp
        sta p4_addr_lo
        lda p4_tmp2
        sta p4_addr_hi
        rts

fetch_rel:
        jsr fetch8
        sta p4_tmp
        rts

; --- finish_cycles ---
finish_cycles:
        sta p4_cycles
        lda p4_xtra
        beq _finish_tick
        lda p4_cycles
        clc
        adc p4_xtra
        sta p4_cycles
_finish_tick:
        lda p4_cycles
        jsr TED_Tick
        rts

; ============================================================
; TED_Tick - Advance TED timing by A cycles
; Tracks raster line and triggers IRQ when raster matches compare
; Also handles Timer 1 countdown
; ============================================================
TED_Tick:
        sta ted_add_cycles
        
        ; === Timer 1 countdown ===
        ; Timer 1 counts down every cycle
        lda ted_timer1_lo
        sec
        sbc ted_add_cycles
        sta ted_timer1_lo
        bcs _timer1_no_borrow
        ; Borrow from high byte
        dec ted_timer1_hi
        lda ted_timer1_hi
        cmp #$FF                ; Did we underflow?
        bne _timer1_no_borrow
        
        ; Timer 1 underflowed! Reload and trigger IRQ
        lda ted_regs+$00        ; Reload low
        sta ted_timer1_lo
        lda ted_regs+$01        ; Reload high
        sta ted_timer1_hi
        
        ; Set Timer 1 IRQ flag (bit 3 in $FF09)
        lda ted_regs+$09
        ora #$08
        sta ted_regs+$09
        
        ; Check if Timer 1 IRQ is enabled (bit 3 in $FF0A)
        lda ted_regs+$0A
        and #$08
        beq _timer1_no_borrow
        
        ; Set IRQ occurred flag and signal CPU
        lda ted_regs+$09
        ora #$80
        sta ted_regs+$09
        lda #1
        sta p4_irq_pending

_timer1_no_borrow:
        
        ; Accumulate cycles for raster
        lda ted_cycle_accum
        clc
        adc ted_add_cycles
        sta ted_cycle_accum
        
_ted_line_loop:
        ; Check if we've completed a scanline
        lda ted_cycle_accum
        cmp #TED_CYCLES_PER_LINE
        bcc _ted_done
        
        ; Subtract one line's worth of cycles
        sec
        sbc #TED_CYCLES_PER_LINE
        sta ted_cycle_accum
        
        ; Advance raster line
        inc ted_raster_lo
        bne _ted_check_wrap
        inc ted_raster_hi
        
_ted_check_wrap:
        ; Wrap at 312 lines (PAL)
        lda ted_raster_hi
        cmp #$01
        bcc _ted_check_irq      ; < 256, no wrap yet
        lda ted_raster_lo
        cmp #$38                ; 312 = $138
        bcc _ted_check_irq
        ; Wrap to line 0
        lda #0
		sta ted_raster_lo
		sta ted_raster_hi

		; Present video once per frame (bitmap mode uses this)
		jsr P4VID_Frame

		; Cursor blink only makes sense in text mode
		lda p4_video_mode
		bne _no_cur_toggle

        inc p4_cur_div
        lda p4_cur_div
        cmp #8 ; cursor blink rate
        bcc _no_cur_toggle
        lda #0
        sta p4_cur_div
        lda p4_cur_phase
        eor #1
        sta p4_cur_phase
        jsr P4VID_UpdateCursor
_no_cur_toggle:
        
_ted_check_irq:
        ; Check if raster matches compare register for IRQ
        jsr TED_CheckRasterIRQ
        jmp _ted_line_loop
        
_ted_done:
        rts

; ============================================================
; TED_CheckRasterIRQ - Check if raster IRQ should fire
; Compares current raster to $FF0A(bit0)/$FF0B compare value
; Sets p4_irq_pending if match and IRQ enabled
; ============================================================
TED_CheckRasterIRQ:
        ; Build compare value: high bit from $FF0A bit 0, low byte from $FF0B
        lda ted_regs+$0B        ; Compare low byte
        cmp ted_raster_lo
        bne _raster_no_match
        lda ted_regs+$0A        ; Compare high bit
        and #$01
        cmp ted_raster_hi
        bne _raster_no_match

        ; Raster matches! Check if flag already set (already triggered this line)
        lda ted_regs+$09
        and #$02
        bne _raster_no_match    ; Already flagged, don't re-trigger
        
        ; Set raster flag (bit 1) in $FF09
        lda ted_regs+$09
        ora #$02                ; Set raster IRQ flag
        sta ted_regs+$09
        
        ; Check if raster IRQ is enabled (bit 1 in $FF0A)
        lda ted_regs+$0A
        and #$02
        beq _raster_no_match    ; IRQ not enabled
        
        ; Set IRQ occurred flag (bit 7) and signal CPU
        lda ted_regs+$09
        ora #$80
        sta ted_regs+$09
        lda #1
        sta p4_irq_pending
        
_raster_no_match:
        rts

; TED state variables
ted_regs:
        .fill 32, 0             ; $FF00-$FF1F shadow registers
ted_cycle_accum:  .byte 0
ted_timer1_lo:    .byte $FF     ; Timer 1 current value (starts at $FFFF)
ted_timer1_hi:    .byte $FF
ted_add_cycles:   .byte 0
ted_raster_lo:    .byte 0
ted_raster_hi:    .byte 0

; ============================================================
; cpu_take_irq - Execute IRQ sequence
; Push PC, push P (with B=0), set I flag, load vector from $FFFE
; ============================================================
cpu_take_irq:

        ; Push PC high
        lda p4_pc_hi
        sta p4_data
        jsr push_data
        ; Push PC low
        lda p4_pc_lo
        sta p4_data
        jsr push_data
        ; Push P with B=0, U=1
        lda p4_p
        and #(~P_B) & $ff       ; Clear B flag
        ora #P_U
        sta p4_data
        jsr push_data
        ; Set I flag
        lda p4_p
        ora #P_I
        sta p4_p
        ; Load IRQ vector from $FFFE/$FFFF
        lda #$FE
        sta p4_addr_lo
        lda #$FF
        sta p4_addr_hi
        jsr P4MEM_Read
        sta p4_pc_lo
        lda #$FF
        sta p4_addr_lo
        jsr P4MEM_Read
        sta p4_pc_hi
        rts

; ============================================================
; cpu_take_nmi - Execute NMI sequence
; Push PC, push P (with B=0), set I flag, load vector from $FFFA
; ============================================================
cpu_take_nmi:
        ; Push PC high
        lda p4_pc_hi
        sta p4_data
        jsr push_data
        ; Push PC low
        lda p4_pc_lo
        sta p4_data
        jsr push_data
        ; Push P with B=0, U=1
        lda p4_p
        and #(~P_B) & $ff
        ora #P_U
        sta p4_data
        jsr push_data
        ; Set I flag
        lda p4_p
        ora #P_I
        sta p4_p
        ; Load NMI vector from $FFFA/$FFFB
        lda #$FA
        sta p4_addr_lo
        lda #$FF
        sta p4_addr_hi
        jsr P4MEM_Read
        sta p4_pc_lo
        lda #$FB
        sta p4_addr_lo
        jsr P4MEM_Read
        sta p4_pc_hi
        rts

; --- Reset/Step ---
P4CPU_Reset:
        lda #$00
        sta p4_a
        sta p4_x
        sta p4_y
        sta p4_irq_pending
        sta p4_nmi_pending
        sta ted_cycle_accum
        sta ted_raster_lo
        sta ted_raster_hi

        lda #$f2
        sta p4_sp
        lda #(P_I|P_U)
        sta p4_p
        lda #$fc
        sta p4_addr_lo
        lda #$ff
        sta p4_addr_hi
        jsr P4MEM_Read
        sta p4_pc_lo
        lda #$fd
        sta p4_addr_lo
        jsr P4MEM_Read
        sta p4_pc_hi
        rts

P4CPU_Step:
        ; --------------------------------------------------------
        ; Check for pending interrupts (NMI first, then IRQ)
        ; --------------------------------------------------------
        lda p4_nmi_pending
        beq _step_chk_irq
        lda #0
        sta p4_nmi_pending
        jsr cpu_take_nmi
        lda #7                  ; NMI takes 7 cycles
        jmp finish_cycles

_step_chk_irq:
        lda p4_irq_pending
        beq _step_execute
        ; Check if IRQ is masked (I flag set)
        lda p4_p
        and #P_I
        bne _step_execute       ; I=1, IRQ masked, skip

; ---- DEBUG: dump vectors once, right before first IRQ taken ----
        ; Take IRQ
        lda #0
        sta p4_irq_pending
        jsr cpu_take_irq
        lda #7                  ; IRQ takes 7 cycles
        jmp finish_cycles

_step_execute:
        lda #$00
        sta p4_xtra
        lda p4_pc_lo
        sta p4_inst_pc_lo
        lda p4_pc_hi
        sta p4_inst_pc_hi

        ; --------------------------------------------------------
        ; BASIC/KERNAL hooks (host-side traps)
        ; --------------------------------------------------------
        jsr P4HOOK_CheckAndRun
        
        ; --------------------------------------------------------
        ; Fetch/decode/execute
        ; --------------------------------------------------------
        jsr fetch8
        tax
        
        ; Get handler address
        lda op_vec_lo,x
        sta p4_vec_lo
        lda op_vec_hi,x
        sta p4_vec_hi
        
        ; Execute
        jmp (p4_vec_lo)

op_illegal:
        ;lda #2
        ;jmp finish_cycles
        lda #$00
        sta $d020
        jmp op_illegal

; --- branch_do ---
branch_do:
        lda #$00
        sta p4_xtra
        lda p4_pc_lo
        clc
        adc p4_tmp
        sta p4_addr_lo
        lda p4_pc_hi
        adc #$00
        sta p4_addr_hi
        lda p4_tmp
        bpl _br_hi_ok
        dec p4_addr_hi
_br_hi_ok:
        lda p4_pc_hi
        cmp p4_addr_hi
        beq _br_same
        lda #$01
        sta p4_xtra
_br_same:
        lda p4_addr_lo
        sta p4_pc_lo
        lda p4_addr_hi
        sta p4_pc_hi
        rts

; --- do_adc ---  (supports decimal mode when P_D set)
do_adc:
        sta p4_tmp

        ; If D flag clear -> original binary path
        lda p4_p
        and #P_D
        beq _do_adc_bin

        ; ---------- decimal ADC ----------
        ; Save original A
        lda p4_a
        sta p4_dec_a

        ; carry_in -> p4_vec_lo (0/1)
        lda p4_p
        and #P_C
        beq _adc_dec_c0
        lda #1
        bne _adc_dec_cstore
_adc_dec_c0:
        lda #0
_adc_dec_cstore:
        sta p4_vec_lo

        ; Binary add first (for V computation)
        lda p4_vec_lo
        beq _adc_dec_clc
        sec
        bne _adc_dec_go
_adc_dec_clc:
        clc
_adc_dec_go:
        lda p4_dec_a
        adc p4_tmp
        sta p4_a
        php                     ; save binary flags (esp V)

        ; Low nibble adjust test: (A_lo + M_lo + carry_in) > 9 ?
        lda p4_tmp
        and #$0F
        sta p4_tmp2             ; m_lo
        lda p4_dec_a
        and #$0F
        clc
        adc p4_tmp2
        clc
        adc p4_vec_lo           ; + carry_in (0/1)
        cmp #$0A
        bcc _adc_dec_no6
        lda p4_a
        clc
        adc #$06
        sta p4_a
_adc_dec_no6:

        ; High adjust if result >= $9A (i.e., > 99 in BCD)
        lda #0
        sta p4_vec_hi           ; decimal carry out (0/1)
        lda p4_a
        cmp #$9A
        bcc _adc_dec_no60
        clc
        adc #$60
        sta p4_a
        lda #1
        sta p4_vec_hi
_adc_dec_no60:

        ; Restore binary flags for V via PLP, then rebuild p4_p C/V
        plp
        lda p4_p
        and #(~(P_C|P_V)) & $ff
        sta p4_p

        ; C from decimal carry (p4_vec_hi)
        lda p4_vec_hi
        beq _adc_dec_noc
        lda p4_p
        ora #P_C
        sta p4_p
_adc_dec_noc:

        ; V from binary add (host V flag after PLP)
        bvc _adc_dec_nov
        lda p4_p
        ora #P_V
        sta p4_p
_adc_dec_nov:
        lda p4_a
        jsr set_zn_a
        rts

_do_adc_bin:
        ; ---------- your original binary ADC ----------
        lda p4_p
        and #P_C
        beq _adc_nc
        sec
        jmp _adc_go2
_adc_nc:
        clc
_adc_go2:
        lda p4_a
        adc p4_tmp
        sta p4_a
        php
        lda p4_p
        and #(~(P_C|P_V)) & $ff
        sta p4_p
        plp
        bcc _adc_noc2
        lda p4_p
        ora #P_C
        sta p4_p
_adc_noc2:
        bvc _adc_nov2
        lda p4_p
        ora #P_V
        sta p4_p
_adc_nov2:
        lda p4_a
        jsr set_zn_a
        rts


; --- do_sbc --- (supports decimal mode when P_D set)
do_sbc:
        sta p4_tmp

        ; If D flag clear -> original binary path
        lda p4_p
        and #P_D
        bne _do_sbc_decimal      ; D set, do decimal
        jmp _do_sbc_bin          ; D clear, do binary

_do_sbc_decimal:
        ; ---------- decimal SBC ----------
        ; Save original A for nibble comparisons
        lda p4_a
        sta p4_dec_a

        ; Do binary subtraction first (for V flag)
        lda p4_p
        and #P_C
        beq _sbc_dec_clc
        sec
        bne _sbc_dec_go
_sbc_dec_clc:
        clc
_sbc_dec_go:
        lda p4_a
        sbc p4_tmp
        sta p4_a
        php                     ; save flags for V

        ; Save carry (1 = no borrow, 0 = borrow)
        lda #0
        rol                     ; A = carry (0 or 1)
        sta p4_vec_hi           ; save for later

        ; Check if low nibble needs adjustment
        ; If (A_lo & $0F) > (orig_A_lo & $0F), we had a borrow from high nibble
        lda p4_a
        and #$0F
        sta p4_tmp2             ; result low nibble
        lda p4_dec_a
        and #$0F                ; original low nibble
        cmp p4_tmp2
        bcs _sbc_no_lo_adj      ; orig >= result, no low borrow
        ; Low nibble borrowed, subtract 6
        lda p4_a
        sec
        sbc #$06
        sta p4_a
_sbc_no_lo_adj:

        ; Check if high nibble needs adjustment
        ; If we had an overall borrow (carry was 0), subtract $60
        lda p4_vec_hi
        bne _sbc_no_hi_adj      ; carry was 1, no borrow
        lda p4_a
        sec
        sbc #$60
        sta p4_a
_sbc_no_hi_adj:

        ; Restore flags and set C/V in p4_p
        plp
        lda p4_p
        and #(~(P_C|P_V)) & $ff
        sta p4_p

        ; Set C from saved carry
        lda p4_vec_hi
        beq _sbc_dec_noc
        lda p4_p
        ora #P_C
        sta p4_p
_sbc_dec_noc:

        ; Set V from binary subtract
        bvc _sbc_dec_nov
        lda p4_p
        ora #P_V
        sta p4_p
_sbc_dec_nov:
        lda p4_a
        jsr set_zn_a
        rts

_do_sbc_bin:
        ; ---------- your original binary SBC ----------
        lda p4_p
        and #P_C
        bne _sbc_c
        clc
        jmp _sbc_go2
_sbc_c:
        sec
_sbc_go2:
        lda p4_a
        sbc p4_tmp
        sta p4_a
        php
        lda p4_p
        and #(~(P_C|P_V)) & $ff
        sta p4_p
        plp
        bcc _sbc_noc2
        lda p4_p
        ora #P_C
        sta p4_p
_sbc_noc2:
        bvc _sbc_nov2
        lda p4_p
        ora #P_V
        sta p4_p
_sbc_nov2:
        lda p4_a
        jsr set_zn_a
        rts

; --- do_cmp ---
do_cmp:
        sta p4_tmp
        lda p4_p
        and #(~(P_C|P_Z|P_N)) & $ff
        sta p4_p
        lda p4_a
        cmp p4_tmp
        bcc _cmp_noc
        lda p4_p
        ora #P_C
        sta p4_p
_cmp_noc:
        lda p4_a
        sec
        sbc p4_tmp
        beq _cmp_z
        bmi _cmp_n
        rts
_cmp_z:
        lda p4_p
        ora #P_Z
        sta p4_p
        rts
_cmp_n:
        lda p4_p
        ora #P_N
        sta p4_p
        rts

; --- do_cpx ---
do_cpx:
        sta p4_tmp
        lda p4_p
        and #(~(P_C|P_Z|P_N)) & $ff
        sta p4_p
        lda p4_x
        cmp p4_tmp
        bcc _cpx_noc
        lda p4_p
        ora #P_C
        sta p4_p
_cpx_noc:
        lda p4_x
        sec
        sbc p4_tmp
        beq _cpx_z
        bmi _cpx_n
        rts
_cpx_z:
        lda p4_p
        ora #P_Z
        sta p4_p
        rts
_cpx_n:
        lda p4_p
        ora #P_N
        sta p4_p
        rts

; --- do_cpy ---
do_cpy:
        sta p4_tmp
        lda p4_p
        and #(~(P_C|P_Z|P_N)) & $ff
        sta p4_p
        lda p4_y
        cmp p4_tmp
        bcc _cpy_noc
        lda p4_p
        ora #P_C
        sta p4_p
_cpy_noc:
        lda p4_y
        sec
        sbc p4_tmp
        beq _cpy_z
        bmi _cpy_n
        rts
_cpy_z:
        lda p4_p
        ora #P_Z
        sta p4_p
        rts
_cpy_n:
        lda p4_p
        ora #P_N
        sta p4_p
        rts

; ============================================================
; OPCODE HANDLERS
; ============================================================

; $00 BRK
op_00:
;lda #$02
;sta $D020
;jmp op_00


        jsr fetch8
        lda p4_pc_hi
        sta p4_data
        jsr push_data
        lda p4_pc_lo
        sta p4_data
        jsr push_data
        lda p4_p
        ora #(P_B|P_U)
        sta p4_data
        jsr push_data
        lda p4_p
        ora #P_I
        sta p4_p
        lda #$fe
        sta p4_addr_lo
        lda #$ff
        sta p4_addr_hi
        jsr P4MEM_Read
        sta p4_pc_lo
        lda #$ff
        sta p4_addr_lo
        jsr P4MEM_Read
        sta p4_pc_hi
        lda #7
        jmp finish_cycles

; $01 ORA (zp,X)
op_01:
        jsr addr_indx
        jsr P4MEM_Read
        ora p4_a
        sta p4_a
        jsr set_zn_a
        lda #6
        jmp finish_cycles

; $05 ORA zp
op_05:
        jsr addr_zp
        jsr P4MEM_Read
        ora p4_a
        sta p4_a
        jsr set_zn_a
        lda #3
        jmp finish_cycles

; $06 ASL zp
op_06:
        jsr addr_zp
        jsr P4MEM_Read
        asl
        php
        sta p4_data
        jsr P4MEM_Write
        lda p4_p
        and #(~P_C) & $ff
        sta p4_p
        plp
        bcc _op06_nc
        lda p4_p
        ora #P_C
        sta p4_p
_op06_nc:
        lda p4_data
        jsr set_zn_a
        lda #5
        jmp finish_cycles

; $08 PHP
op_08:
        lda p4_p
        ora #(P_B|P_U)
        sta p4_data
        jsr push_data
        lda #3
        jmp finish_cycles

; $09 ORA #imm
op_09:
        jsr fetch8
        ora p4_a
        sta p4_a
        jsr set_zn_a
        lda #2
        jmp finish_cycles

; $0A ASL A
op_0a:
        lda p4_a
        asl
        sta p4_a
        php
        lda p4_p
        and #(~P_C) & $ff
        sta p4_p
        plp
        bcc _op0a_nc
        lda p4_p
        ora #P_C
        sta p4_p
_op0a_nc:
        lda p4_a
        jsr set_zn_a
        lda #2
        jmp finish_cycles

; $0D ORA abs
op_0d:
        jsr addr_abs
        jsr P4MEM_Read
        ora p4_a
        sta p4_a
        jsr set_zn_a
        lda #4
        jmp finish_cycles

; $0E ASL abs
op_0e:
        jsr addr_abs
        jsr P4MEM_Read
        asl
        php
        sta p4_data
        jsr P4MEM_Write
        lda p4_p
        and #(~P_C) & $ff
        sta p4_p
        plp
        bcc _op0e_nc
        lda p4_p
        ora #P_C
        sta p4_p
_op0e_nc:
        lda p4_data
        jsr set_zn_a
        lda #6
        jmp finish_cycles

; $10 BPL
op_10:
        jsr fetch_rel
        bbs 7, p4_p,_op10_nt
        ;lda p4_p
        ;and #P_N
        ;bne _op10_nt
        jsr branch_do
        lda p4_xtra
        clc
        adc #1
        sta p4_xtra
        lda #2
        jmp finish_cycles
_op10_nt:
        lda #0
        sta p4_xtra
        lda #2
        jmp finish_cycles

; $11 ORA (zp),Y
op_11:
        jsr addr_indy
        jsr P4MEM_Read
        ora p4_a
        sta p4_a
        jsr set_zn_a
        lda #5
        jmp finish_cycles

; $15 ORA zp,X
op_15:
        jsr addr_zpx
        jsr P4MEM_Read
        ora p4_a
        sta p4_a
        jsr set_zn_a
        lda #4
        jmp finish_cycles

; $16 ASL zp,X
op_16:
        jsr addr_zpx
        jsr P4MEM_Read
        asl
        php
        sta p4_data
        jsr P4MEM_Write
        lda p4_p
        and #(~P_C) & $ff
        sta p4_p
        plp
        bcc _op16_nc
        lda p4_p
        ora #P_C
        sta p4_p
_op16_nc:
        lda p4_data
        jsr set_zn_a
        lda #6
        jmp finish_cycles

; $18 CLC
op_18:
        lda p4_p
        and #(~P_C) & $ff
        sta p4_p
        lda #2
        jmp finish_cycles

; $19 ORA abs,Y
op_19:
        jsr addr_absy
        jsr P4MEM_Read
        ora p4_a
        sta p4_a
        jsr set_zn_a
        lda #4
        jmp finish_cycles

; $1D ORA abs,X
op_1d:
        jsr addr_absx
        jsr P4MEM_Read
        ora p4_a
        sta p4_a
        jsr set_zn_a
        lda #4
        jmp finish_cycles

; $1E ASL abs,X
op_1e:
        jsr addr_absx
        jsr P4MEM_Read
        asl
        php
        sta p4_data
        jsr P4MEM_Write
        lda p4_p
        and #(~P_C) & $ff
        sta p4_p
        plp
        bcc _op1e_nc
        lda p4_p
        ora #P_C
        sta p4_p
_op1e_nc:
        lda p4_data
        jsr set_zn_a
        lda #7
        jmp finish_cycles

; $20 JSR
op_20:
        jsr fetch16_to_addr
        lda p4_addr_lo
        sta p4_vec_lo
        lda p4_addr_hi
        sta p4_vec_hi
        lda p4_pc_lo
        sec
        sbc #1
        sta p4_tmp
        lda p4_pc_hi
        sbc #0
        sta p4_data
        jsr push_data
        lda p4_tmp
        sta p4_data
        jsr push_data
        lda p4_vec_lo
        sta p4_pc_lo
        lda p4_vec_hi
        sta p4_pc_hi
        lda #6
        jmp finish_cycles

; $21 AND (zp,X)
op_21:
        jsr addr_indx
        jsr P4MEM_Read
        and p4_a
        sta p4_a
        jsr set_zn_a
        lda #6
        jmp finish_cycles

; $24 BIT zp
op_24:
        jsr addr_zp
        jsr P4MEM_Read
        sta p4_tmp
        lda p4_p
        and #(~(P_N|P_V|P_Z)) & $ff
        sta p4_p
        lda p4_tmp
        and #P_N
        ora p4_p
        sta p4_p
        lda p4_tmp
        and #P_V
        ora p4_p
        sta p4_p
        lda p4_tmp
        and p4_a
        bne _op24_nz
        lda p4_p
        ora #P_Z
        sta p4_p
_op24_nz:
        lda #3
        jmp finish_cycles

; $25 AND zp
op_25:
        jsr addr_zp
        jsr P4MEM_Read
        and p4_a
        sta p4_a
        jsr set_zn_a
        lda #3
        jmp finish_cycles

; $26 ROL zp
op_26:
        jsr addr_zp
        jsr P4MEM_Read
        sta p4_tmp
        lda p4_p
        and #P_C
        sta p4_tmp2
        lda p4_tmp
        asl
        ora p4_tmp2
        sta p4_data
        lda p4_p
        and #(~P_C) & $ff
        sta p4_p
        lda p4_tmp
        and #$80
        beq _op26_nc
        lda p4_p
        ora #P_C
        sta p4_p
_op26_nc:
        jsr P4MEM_Write
        lda p4_data
        jsr set_zn_a
        lda #5
        jmp finish_cycles

; $28 PLP
op_28:
        jsr pull_to_a
        and #(~P_B) & $ff
        ora #P_U
        sta p4_p
        lda #4
        jmp finish_cycles

; $29 AND #imm
op_29:
        jsr fetch8
        and p4_a
        sta p4_a
        jsr set_zn_a
        lda #2
        jmp finish_cycles

; $2A ROL A
op_2a:
        lda p4_p
        and #P_C
        sta p4_tmp2
        lda p4_a
        sta p4_tmp
        asl
        ora p4_tmp2
        sta p4_a
        lda p4_p
        and #(~P_C) & $ff
        sta p4_p
        lda p4_tmp
        and #$80
        beq _op2a_nc
        lda p4_p
        ora #P_C
        sta p4_p
_op2a_nc:
        lda p4_a
        jsr set_zn_a
        lda #2
        jmp finish_cycles

; $2C BIT abs
op_2c:
        jsr addr_abs
        jsr P4MEM_Read
        sta p4_tmp
        lda p4_p
        and #(~(P_N|P_V|P_Z)) & $ff
        sta p4_p
        lda p4_tmp
        and #P_N
        ora p4_p
        sta p4_p
        lda p4_tmp
        and #P_V
        ora p4_p
        sta p4_p
        lda p4_tmp
        and p4_a
        bne _op2c_nz
        lda p4_p
        ora #P_Z
        sta p4_p
_op2c_nz:
        lda #4
        jmp finish_cycles

; $2D AND abs
op_2d:
        jsr addr_abs
        jsr P4MEM_Read
        and p4_a
        sta p4_a
        jsr set_zn_a
        lda #4
        jmp finish_cycles

; $2E ROL abs
op_2e:
        jsr addr_abs
        jsr P4MEM_Read
        sta p4_tmp
        lda p4_p
        and #P_C
        sta p4_tmp2
        lda p4_tmp
        asl
        ora p4_tmp2
        sta p4_data
        lda p4_p
        and #(~P_C) & $ff
        sta p4_p
        lda p4_tmp
        and #$80
        beq _op2e_nc
        lda p4_p
        ora #P_C
        sta p4_p
_op2e_nc:
        jsr P4MEM_Write
        lda p4_data
        jsr set_zn_a
        lda #6
        jmp finish_cycles

; $30 BMI
op_30:
        jsr fetch_rel
        bbr 7, p4_p, _op30_nt
        ;lda p4_p
        ;and #P_N
        ;beq _op30_nt
        jsr branch_do
        lda p4_xtra
        clc
        adc #1
        sta p4_xtra
        lda #2
        jmp finish_cycles
_op30_nt:
        lda #0
        sta p4_xtra
        lda #2
        jmp finish_cycles

; $31 AND (zp),Y
op_31:
        jsr addr_indy
        jsr P4MEM_Read
        and p4_a
        sta p4_a
        jsr set_zn_a
        lda #5
        jmp finish_cycles

; $35 AND zp,X
op_35:
        jsr addr_zpx
        jsr P4MEM_Read
        and p4_a
        sta p4_a
        jsr set_zn_a
        lda #4
        jmp finish_cycles

; $36 ROL zp,X
op_36:
        jsr addr_zpx
        jsr P4MEM_Read
        sta p4_tmp
        lda p4_p
        and #P_C
        sta p4_tmp2
        lda p4_tmp
        asl
        ora p4_tmp2
        sta p4_data
        lda p4_p
        and #(~P_C) & $ff
        sta p4_p
        lda p4_tmp
        and #$80
        beq _op36_nc
        lda p4_p
        ora #P_C
        sta p4_p
_op36_nc:
        jsr P4MEM_Write
        lda p4_data
        jsr set_zn_a
        lda #6
        jmp finish_cycles

; $38 SEC
op_38:
        lda p4_p
        ora #P_C
        sta p4_p
        lda #2
        jmp finish_cycles

; $39 AND abs,Y
op_39:
        jsr addr_absy
        jsr P4MEM_Read
        and p4_a
        sta p4_a
        jsr set_zn_a
        lda #4
        jmp finish_cycles

; $3D AND abs,X
op_3d:
        jsr addr_absx
        jsr P4MEM_Read
        and p4_a
        sta p4_a
        jsr set_zn_a
        lda #4
        jmp finish_cycles

; $3E ROL abs,X
op_3e:
        jsr addr_absx
        jsr P4MEM_Read
        sta p4_tmp
        lda p4_p
        and #P_C
        sta p4_tmp2
        lda p4_tmp
        asl
        ora p4_tmp2
        sta p4_data
        lda p4_p
        and #(~P_C) & $ff
        sta p4_p
        lda p4_tmp
        and #$80
        beq _op3e_nc
        lda p4_p
        ora #P_C
        sta p4_p
_op3e_nc:
        jsr P4MEM_Write
        lda p4_data
        jsr set_zn_a
        lda #7
        jmp finish_cycles

; $40 RTI
op_40:
        jsr pull_to_a
        and #(~P_B) & $ff
        ora #P_U
        sta p4_p
        jsr pull_to_a
        sta p4_pc_lo
        jsr pull_to_a
        sta p4_pc_hi
        lda #6
        jmp finish_cycles

; $41 EOR (zp,X)
op_41:
        jsr addr_indx
        jsr P4MEM_Read
        eor p4_a
        sta p4_a
        jsr set_zn_a
        lda #6
        jmp finish_cycles

; $45 EOR zp
op_45:
        jsr addr_zp
        jsr P4MEM_Read
        eor p4_a
        sta p4_a
        jsr set_zn_a
        lda #3
        jmp finish_cycles

; $46 LSR zp
op_46:
        jsr addr_zp
        jsr P4MEM_Read
        sta p4_tmp
        lsr
        sta p4_data
        lda p4_p
        and #(~P_C) & $ff
        sta p4_p
        lda p4_tmp
        and #$01
        beq _op46_nc
        lda p4_p
        ora #P_C
        sta p4_p
_op46_nc:
        jsr P4MEM_Write
        lda p4_data
        jsr set_zn_a
        lda #5
        jmp finish_cycles

; $48 PHA
op_48:
        lda p4_a
        sta p4_data
        jsr push_data
        lda #3
        jmp finish_cycles

; $49 EOR #imm
op_49:
        jsr fetch8
        eor p4_a
        sta p4_a
        jsr set_zn_a
        lda #2
        jmp finish_cycles

; $4A LSR A
op_4a:
        lda p4_a
        sta p4_tmp
        lsr
        sta p4_a
        lda p4_p
        and #(~P_C) & $ff
        sta p4_p
        lda p4_tmp
        and #$01
        beq _op4a_nc
        lda p4_p
        ora #P_C
        sta p4_p
_op4a_nc:
        lda p4_a
        jsr set_zn_a
        lda #2
        jmp finish_cycles

; $4C JMP abs
op_4c:
        jsr fetch16_to_addr
        lda p4_addr_lo
        sta p4_pc_lo
        lda p4_addr_hi
        sta p4_pc_hi
        lda #3
        jmp finish_cycles

; $4D EOR abs
op_4d:
        jsr addr_abs
        jsr P4MEM_Read
        eor p4_a
        sta p4_a
        jsr set_zn_a
        lda #4
        jmp finish_cycles

; $4E LSR abs
op_4e:
        jsr addr_abs
        jsr P4MEM_Read
        sta p4_tmp
        lsr
        sta p4_data
        lda p4_p
        and #(~P_C) & $ff
        sta p4_p
        lda p4_tmp
        and #$01
        beq _op4e_nc
        lda p4_p
        ora #P_C
        sta p4_p
_op4e_nc:
        jsr P4MEM_Write
        lda p4_data
        jsr set_zn_a
        lda #6
        jmp finish_cycles

; $50 BVC
op_50:
        jsr fetch_rel
        bbs 6, p4_p, _op50_nt
        ;lda p4_p
        ;and #P_V
        ;bne _op50_nt
        jsr branch_do
        lda p4_xtra
        clc
        adc #1
        sta p4_xtra
        lda #2
        jmp finish_cycles
_op50_nt:
        lda #0
        sta p4_xtra
        lda #2
        jmp finish_cycles

; $51 EOR (zp),Y
op_51:
        jsr addr_indy
        jsr P4MEM_Read
        eor p4_a
        sta p4_a
        jsr set_zn_a
        lda #5
        jmp finish_cycles

; $55 EOR zp,X
op_55:
        jsr addr_zpx
        jsr P4MEM_Read
        eor p4_a
        sta p4_a
        jsr set_zn_a
        lda #4
        jmp finish_cycles

; $56 LSR zp,X
op_56:
        jsr addr_zpx
        jsr P4MEM_Read
        sta p4_tmp
        lsr
        sta p4_data
        lda p4_p
        and #(~P_C) & $ff
        sta p4_p
        lda p4_tmp
        and #$01
        beq _op56_nc
        lda p4_p
        ora #P_C
        sta p4_p
_op56_nc:
        jsr P4MEM_Write
        lda p4_data
        jsr set_zn_a
        lda #6
        jmp finish_cycles

; $58 CLI
op_58:
        lda p4_p
        and #(~P_I) & $ff
        sta p4_p
        lda #2
        jmp finish_cycles

; $59 EOR abs,Y
op_59:
        jsr addr_absy
        jsr P4MEM_Read
        eor p4_a
        sta p4_a
        jsr set_zn_a
        lda #4
        jmp finish_cycles

; $5D EOR abs,X
op_5d:
        jsr addr_absx
        jsr P4MEM_Read
        eor p4_a
        sta p4_a
        jsr set_zn_a
        lda #4
        jmp finish_cycles

; $5E LSR abs,X
op_5e:
        jsr addr_absx
        jsr P4MEM_Read
        sta p4_tmp
        lsr
        sta p4_data
        lda p4_p
        and #(~P_C) & $ff
        sta p4_p
        lda p4_tmp
        and #$01
        beq _op5e_nc
        lda p4_p
        ora #P_C
        sta p4_p
_op5e_nc:
        jsr P4MEM_Write
        lda p4_data
        jsr set_zn_a
        lda #7
        jmp finish_cycles

; $60 RTS
op_60:
        jsr pull_to_a
        sta p4_pc_lo
        jsr pull_to_a
        sta p4_pc_hi
        inc p4_pc_lo
        bne _op60_done
        inc p4_pc_hi
_op60_done:
        lda #6
        jmp finish_cycles

; $61 ADC (zp,X)
op_61:
        jsr addr_indx
        jsr P4MEM_Read
        jsr do_adc
        lda #6
        jmp finish_cycles

; $65 ADC zp
op_65:
        jsr addr_zp
        jsr P4MEM_Read
        jsr do_adc
        lda #3
        jmp finish_cycles

; $66 ROR zp
op_66:
        jsr addr_zp
        jsr P4MEM_Read
        sta p4_tmp
        lda p4_p
        and #P_C
        beq _op66_nci
        lda #$80
        sta p4_tmp2
        jmp _op66_do
_op66_nci:
        lda #$00
        sta p4_tmp2
_op66_do:
        lda p4_tmp
        lsr
        ora p4_tmp2
        sta p4_data
        lda p4_p
        and #(~P_C) & $ff
        sta p4_p
        lda p4_tmp
        and #$01
        beq _op66_nc
        lda p4_p
        ora #P_C
        sta p4_p
_op66_nc:
        jsr P4MEM_Write
        lda p4_data
        jsr set_zn_a
        lda #5
        jmp finish_cycles

; $68 PLA
op_68:
        jsr pull_to_a
        sta p4_a
        jsr set_zn_a
        lda #4
        jmp finish_cycles

; $69 ADC #imm
op_69:
        jsr fetch8
        jsr do_adc
        lda #2
        jmp finish_cycles

; $6A ROR A
op_6a:
        lda p4_p
        and #P_C
        beq _op6a_nci
        lda #$80
        sta p4_tmp2
        jmp _op6a_do
_op6a_nci:
        lda #$00
        sta p4_tmp2
_op6a_do:
        lda p4_a
        sta p4_tmp
        lsr
        ora p4_tmp2
        sta p4_a
        lda p4_p
        and #(~P_C) & $ff
        sta p4_p
        lda p4_tmp
        and #$01
        beq _op6a_nc
        lda p4_p
        ora #P_C
        sta p4_p
_op6a_nc:
        lda p4_a
        jsr set_zn_a
        lda #2
        jmp finish_cycles

; $6C JMP (ind)
op_6c:
        jsr addr_ind_jmp

        lda p4_addr_lo
        sta p4_pc_lo
        lda p4_addr_hi
        sta p4_pc_hi
        lda #5
        jmp finish_cycles

; $6D ADC abs
op_6d:
        jsr addr_abs
        jsr P4MEM_Read
        jsr do_adc
        lda #4
        jmp finish_cycles

; $6E ROR abs
op_6e:
        jsr addr_abs
        jsr P4MEM_Read
        sta p4_tmp
        lda p4_p
        and #P_C
        beq _op6e_nci
        lda #$80
        sta p4_tmp2
        jmp _op6e_do
_op6e_nci:
        lda #$00
        sta p4_tmp2
_op6e_do:
        lda p4_tmp
        lsr
        ora p4_tmp2
        sta p4_data
        lda p4_p
        and #(~P_C) & $ff
        sta p4_p
        lda p4_tmp
        and #$01
        beq _op6e_nc
        lda p4_p
        ora #P_C
        sta p4_p
_op6e_nc:
        jsr P4MEM_Write
        lda p4_data
        jsr set_zn_a
        lda #6
        jmp finish_cycles

; $70 BVS
op_70:
        jsr fetch_rel
        bbr 6, p4_p, _op70_nt
        ;lda p4_p
        ;and #P_V
        ;beq _op70_nt
        jsr branch_do
        lda p4_xtra
        clc
        adc #1
        sta p4_xtra
        lda #2
        jmp finish_cycles
_op70_nt:
        lda #0
        sta p4_xtra
        lda #2
        jmp finish_cycles

; $71 ADC (zp),Y
op_71:
        jsr addr_indy
        jsr P4MEM_Read
        jsr do_adc
        lda #5
        jmp finish_cycles

; $75 ADC zp,X
op_75:
        jsr addr_zpx
        jsr P4MEM_Read
        jsr do_adc
        lda #4
        jmp finish_cycles

; $76 ROR zp,X
op_76:
        jsr addr_zpx
        jsr P4MEM_Read
        sta p4_tmp
        lda p4_p
        and #P_C
        beq _op76_nci
        lda #$80
        sta p4_tmp2
        jmp _op76_do
_op76_nci:
        lda #$00
        sta p4_tmp2
_op76_do:
        lda p4_tmp
        lsr
        ora p4_tmp2
        sta p4_data
        lda p4_p
        and #(~P_C) & $ff
        sta p4_p
        lda p4_tmp
        and #$01
        beq _op76_nc
        lda p4_p
        ora #P_C
        sta p4_p
_op76_nc:
        jsr P4MEM_Write
        lda p4_data
        jsr set_zn_a
        lda #6
        jmp finish_cycles

; $78 SEI
op_78:
        lda p4_p
        ora #P_I
        sta p4_p
        lda #2
        jmp finish_cycles

; $79 ADC abs,Y
op_79:
        jsr addr_absy
        jsr P4MEM_Read
        jsr do_adc
        lda #4
        jmp finish_cycles

; $7D ADC abs,X
op_7d:
        jsr addr_absx
        jsr P4MEM_Read
        jsr do_adc
        lda #4
        jmp finish_cycles

; $7E ROR abs,X
op_7e:
        jsr addr_absx
        jsr P4MEM_Read
        sta p4_tmp
        lda p4_p
        and #P_C
        beq _op7e_nci
        lda #$80
        sta p4_tmp2
        jmp _op7e_do
_op7e_nci:
        lda #$00
        sta p4_tmp2
_op7e_do:
        lda p4_tmp
        lsr
        ora p4_tmp2
        sta p4_data
        lda p4_p
        and #(~P_C) & $ff
        sta p4_p
        lda p4_tmp
        and #$01
        beq _op7e_nc
        lda p4_p
        ora #P_C
        sta p4_p
_op7e_nc:
        jsr P4MEM_Write
        lda p4_data
        jsr set_zn_a
        lda #7
        jmp finish_cycles

; $81 STA (zp,X)
op_81:
        jsr addr_indx
        lda p4_a
        sta p4_data
        jsr P4MEM_Write
        lda #6
        jmp finish_cycles

; $84 STY zp
op_84:
        ; Optimized STY zp
        jsr fetch8
        tax
        lda p4_y
        sta LOW_RAM_BUFFER,x
        lda #3
        jmp finish_cycles


; $85 STA zp
op_85:
        jsr fetch8
        tax
        lda p4_a
        sta LOW_RAM_BUFFER,x
        lda #3
        jmp finish_cycles

; $86 STX zp  
op_86:
        jsr fetch8
        tax
        lda p4_x
        sta LOW_RAM_BUFFER,x
        lda #3
        jmp finish_cycles

; $88 DEY
op_88:
        dec p4_y
        lda p4_y
        jsr set_zn_a
        lda #2
        jmp finish_cycles

; $8A TXA
op_8a:
        lda p4_x
        sta p4_a
        jsr set_zn_a
        lda #2
        jmp finish_cycles

; $8C STY abs
op_8c:
        jsr addr_abs
        lda p4_y
        sta p4_data
        jsr P4MEM_Write
        lda #4
        jmp finish_cycles

; $8D STA abs
op_8d:
        jsr addr_abs
        lda p4_a
        sta p4_data
        jsr P4MEM_Write
        lda #4
        jmp finish_cycles

; $8E STX abs
op_8e:
        jsr addr_abs
        lda p4_x
        sta p4_data
        jsr P4MEM_Write
        lda #4
        jmp finish_cycles

; $90 BCC
op_90:
        jsr fetch_rel

        ; P_C is Bit 0. If Bit 0 is Set, we do NOT take the branch (BCC).
        bbs 0, p4_p, _op90_nt 

        ;lda p4_p
        ;and #P_C
        ;bne _op90_nt
        jsr branch_do
        lda p4_xtra
        clc
        adc #1
        sta p4_xtra
        lda #2
        jmp finish_cycles
_op90_nt:
        lda #0
        sta p4_xtra
        lda #2
        jmp finish_cycles

; $91 STA (zp),Y
op_91:
        jsr addr_indy
        lda p4_a
        sta p4_data
        jsr P4MEM_Write
        lda #6
        jmp finish_cycles

; $94 STY zp,X
op_94:
        jsr addr_zpx
        lda p4_y
        sta p4_data
        jsr P4MEM_Write
        lda #4
        jmp finish_cycles

; $95 STA zp,X
op_95:
        jsr addr_zpx
        lda p4_a
        sta p4_data
        jsr P4MEM_Write
        lda #4
        jmp finish_cycles

; $96 STX zp,Y
op_96:
        jsr addr_zpy
        lda p4_x
        sta p4_data
        jsr P4MEM_Write
        lda #4
        jmp finish_cycles

; $98 TYA
op_98:
        lda p4_y
        sta p4_a
        jsr set_zn_a
        lda #2
        jmp finish_cycles

; $99 STA abs,Y
op_99:
        jsr addr_absy
        lda p4_a
        sta p4_data
        jsr P4MEM_Write
        lda #5
        jmp finish_cycles

; $9A TXS
op_9a:
        lda p4_x
        sta p4_sp
        lda #2
        jmp finish_cycles

; $9D STA abs,X
op_9d:
        jsr addr_absx
        lda p4_a
        sta p4_data
        jsr P4MEM_Write
        lda #5
        jmp finish_cycles

; $A0 LDY #imm
op_a0:
        jsr fetch8
        sta p4_y
        jsr set_zn_a
        lda #2
        jmp finish_cycles

; $A1 LDA (zp,X)
op_a1:
        jsr addr_indx
        jsr P4MEM_Read
        sta p4_a
        jsr set_zn_a
        lda #6
        jmp finish_cycles

; $A2 LDX #imm
op_a2:
        jsr fetch8
        sta p4_x
        jsr set_zn_a
        lda #2
        jmp finish_cycles

; $A4 LDY zp
op_a4:
        ; Optimized LDY zp
        jsr fetch8
        tax
        lda LOW_RAM_BUFFER,x
        sta p4_y
        jsr set_zn_a
        lda #3
        jmp finish_cycles

; $A5 LDA zp
op_a5:
        ; Optimized LDA zp - direct access to LOW_RAM_BUFFER
        jsr fetch8              ; Get zero page address in A
        tax
        lda LOW_RAM_BUFFER,x             ; Direct read from LOW_RAM_BUFFER
        sta p4_a
        jsr set_zn_a
        lda #3
        jmp finish_cycles

; $A6 LDX zp
op_a6:
        ; Optimized LDX zp
        jsr fetch8
        tax
        lda LOW_RAM_BUFFER,x
        sta p4_x
        jsr set_zn_a
        lda #3
        jmp finish_cycles

; $A8 TAY
op_a8:
        lda p4_a
        sta p4_y
        jsr set_zn_a
        lda #2
        jmp finish_cycles

; $A9 LDA #imm
op_a9:
        jsr fetch8
        sta p4_a
        jsr set_zn_a
        lda #2
        jmp finish_cycles

; $AA TAX
op_aa:
        lda p4_a
        sta p4_x
        jsr set_zn_a
        lda #2
        jmp finish_cycles

; $AC LDY abs
op_ac:
        jsr addr_abs
        jsr P4MEM_Read
        sta p4_y
        jsr set_zn_a
        lda #4
        jmp finish_cycles

; $AD LDA abs
op_ad:
        jsr addr_abs
        jsr P4MEM_Read
        sta p4_a
        jsr set_zn_a
        lda #4
        jmp finish_cycles

; $AE LDX abs
op_ae:
        jsr addr_abs
        jsr P4MEM_Read
        sta p4_x
        jsr set_zn_a
        lda #4
        jmp finish_cycles

; $B0 BCS
op_b0:
        jsr fetch_rel
        bbr 0, p4_p, _opb0_nt
        ;lda p4_p
        ;and #P_C
        ;beq _opb0_nt
        jsr branch_do
        lda p4_xtra
        clc
        adc #1
        sta p4_xtra
        lda #2
        jmp finish_cycles
_opb0_nt:
        lda #0
        sta p4_xtra
        lda #2
        jmp finish_cycles

; $B1 LDA (zp),Y
op_b1:
        jsr addr_indy
        jsr P4MEM_Read
        sta p4_a
        jsr set_zn_a
        lda #5
        jmp finish_cycles

; $B4 LDY zp,X
op_b4:
        jsr addr_zpx
        jsr P4MEM_Read
        sta p4_y
        jsr set_zn_a
        lda #4
        jmp finish_cycles

; $B5 LDA zp,X
op_b5:
        jsr addr_zpx
        jsr P4MEM_Read
        sta p4_a
        jsr set_zn_a
        lda #4
        jmp finish_cycles

; $B6 LDX zp,Y
op_b6:
        jsr addr_zpy
        jsr P4MEM_Read
        sta p4_x
        jsr set_zn_a
        lda #4
        jmp finish_cycles

; $B8 CLV
op_b8:
        lda p4_p
        and #(~P_V) & $ff
        sta p4_p
        lda #2
        jmp finish_cycles

; $B9 LDA abs,Y
op_b9:
        jsr addr_absy
        jsr P4MEM_Read
        sta p4_a
        jsr set_zn_a
        lda #4
        jmp finish_cycles

; $BA TSX
op_ba:
        lda p4_sp
        sta p4_x
        jsr set_zn_a
        lda #2
        jmp finish_cycles

; $BC LDY abs,X
op_bc:
        jsr addr_absx
        jsr P4MEM_Read
        sta p4_y
        jsr set_zn_a
        lda #4
        jmp finish_cycles

; $BD LDA abs,X
op_bd:
        jsr addr_absx
        jsr P4MEM_Read
        sta p4_a
        jsr set_zn_a
        lda #4
        jmp finish_cycles

; $BE LDX abs,Y
op_be:
        jsr addr_absy
        jsr P4MEM_Read
        sta p4_x
        jsr set_zn_a
        lda #4
        jmp finish_cycles

; $C0 CPY #imm
op_c0:
        jsr fetch8
        jsr do_cpy
        lda #2
        jmp finish_cycles

; $C1 CMP (zp,X)
op_c1:
        jsr addr_indx
        jsr P4MEM_Read
        jsr do_cmp
        lda #6
        jmp finish_cycles

; $C4 CPY zp
op_c4:
        jsr addr_zp
        jsr P4MEM_Read
        jsr do_cpy
        lda #3
        jmp finish_cycles

; $C5 CMP zp
op_c5:
        jsr addr_zp
        jsr P4MEM_Read
        jsr do_cmp
        lda #3
        jmp finish_cycles

; $C6 DEC zp
op_c6:
        ; Optimized DEC zp
        jsr fetch8
        tax
        dec LOW_RAM_BUFFER,x
        lda LOW_RAM_BUFFER,x
        jsr set_zn_a
        lda #5
        jmp finish_cycles

; $C8 INY
op_c8:
        inc p4_y
        lda p4_y
        jsr set_zn_a
        lda #2
        jmp finish_cycles

; $C9 CMP #imm
op_c9:
        jsr fetch8
        jsr do_cmp
        lda #2
        jmp finish_cycles

; $CA DEX
op_ca:
        dec p4_x
        lda p4_x
        jsr set_zn_a
        lda #2
        jmp finish_cycles

; $CC CPY abs
op_cc:
        jsr addr_abs
        jsr P4MEM_Read
        jsr do_cpy
        lda #4
        jmp finish_cycles

; $CD CMP abs
op_cd:
        jsr addr_abs
        jsr P4MEM_Read
        jsr do_cmp
        lda #4
        jmp finish_cycles

; $CE DEC abs
op_ce:
        jsr addr_abs
        jsr P4MEM_Read
        sec
        sbc #1
        sta p4_data
        jsr P4MEM_Write
        lda p4_data
        jsr set_zn_a
        lda #6
        jmp finish_cycles

; $D0 BNE
op_d0:
        jsr fetch_rel
        bbs 1, p4_p, _opd0_nt
        ;lda p4_p
        ;and #P_Z
        ;bne _opd0_nt
        jsr branch_do
        lda p4_xtra
        clc
        adc #1
        sta p4_xtra
        lda #2
        jmp finish_cycles
_opd0_nt:
        lda #0
        sta p4_xtra
        lda #2
        jmp finish_cycles

; $D1 CMP (zp),Y
op_d1:
        jsr addr_indy
        jsr P4MEM_Read
        jsr do_cmp
        lda #5
        jmp finish_cycles

; $D5 CMP zp,X
op_d5:
        jsr addr_zpx
        jsr P4MEM_Read
        jsr do_cmp
        lda #4
        jmp finish_cycles

; $D6 DEC zp,X
op_d6:
        jsr addr_zpx
        jsr P4MEM_Read
        sec
        sbc #1
        sta p4_data
        jsr P4MEM_Write
        lda p4_data
        jsr set_zn_a
        lda #6
        jmp finish_cycles

; $D8 CLD
op_d8:
        lda p4_p
        and #(~P_D) & $ff
        sta p4_p
        lda #2
        jmp finish_cycles

; $D9 CMP abs,Y
op_d9:
        jsr addr_absy
        jsr P4MEM_Read
        jsr do_cmp
        lda #4
        jmp finish_cycles

; $DD CMP abs,X
op_dd:
        jsr addr_absx
        jsr P4MEM_Read
        jsr do_cmp
        lda #4
        jmp finish_cycles

; $DE DEC abs,X
op_de:
        jsr addr_absx
        jsr P4MEM_Read
        sec
        sbc #1
        sta p4_data
        jsr P4MEM_Write
        lda p4_data
        jsr set_zn_a
        lda #7
        jmp finish_cycles

; $E0 CPX #imm
op_e0:
        jsr fetch8
        jsr do_cpx
        lda #2
        jmp finish_cycles

; $E1 SBC (zp,X)
op_e1:
        jsr addr_indx
        jsr P4MEM_Read
        jsr do_sbc
        lda #6
        jmp finish_cycles

; $E4 CPX zp
op_e4:
        jsr addr_zp
        jsr P4MEM_Read
        jsr do_cpx
        lda #3
        jmp finish_cycles

; $E5 SBC zp
op_e5:
        jsr addr_zp
        jsr P4MEM_Read
        jsr do_sbc
        lda #3
        jmp finish_cycles

; $E6 INC zp
op_e6:
        ; Optimized INC zp
        jsr fetch8
        tax
        inc LOW_RAM_BUFFER,x
        lda LOW_RAM_BUFFER,x
        jsr set_zn_a
        lda #5
        jmp finish_cycles

; $E8 INX
op_e8:
        inc p4_x
        lda p4_x
        jsr set_zn_a
        lda #2
        jmp finish_cycles

; $E9 SBC #imm
op_e9:
        jsr fetch8
        jsr do_sbc
        lda #2
        jmp finish_cycles

; $EA NOP
op_ea:
        lda #2
        jmp finish_cycles

; $EC CPX abs
op_ec:
        jsr addr_abs
        jsr P4MEM_Read
        jsr do_cpx
        lda #4
        jmp finish_cycles

; $ED SBC abs
op_ed:
        jsr addr_abs
        jsr P4MEM_Read
        jsr do_sbc
        lda #4
        jmp finish_cycles

; $EE INC abs
op_ee:
        jsr addr_abs
        jsr P4MEM_Read
        clc
        adc #1
        sta p4_data
        jsr P4MEM_Write
        lda p4_data
        jsr set_zn_a
        lda #6
        jmp finish_cycles

; $F0 BEQ
op_f0:
        jsr fetch_rel
        bbr 1, p4_p, _opf0_nt
        ;lda p4_p
        ;and #P_Z
        ;beq _opf0_nt
        jsr branch_do
        lda p4_xtra
        clc
        adc #1
        sta p4_xtra
        lda #2
        jmp finish_cycles
_opf0_nt:
        lda #0
        sta p4_xtra
        lda #2
        jmp finish_cycles

; $F1 SBC (zp),Y
op_f1:
        jsr addr_indy
        jsr P4MEM_Read
        jsr do_sbc
        lda #5
        jmp finish_cycles

; $F5 SBC zp,X
op_f5:
        jsr addr_zpx
        jsr P4MEM_Read
        jsr do_sbc
        lda #4
        jmp finish_cycles

; $F6 INC zp,X
op_f6:
        jsr addr_zpx
        jsr P4MEM_Read
        clc
        adc #1
        sta p4_data
        jsr P4MEM_Write
        lda p4_data
        jsr set_zn_a
        lda #6
        jmp finish_cycles

; $F8 SED
op_f8:
        lda p4_p
        ora #P_D
        sta p4_p
        lda #2
        jmp finish_cycles

; $F9 SBC abs,Y
op_f9:
        jsr addr_absy
        jsr P4MEM_Read
        jsr do_sbc
        lda #4
        jmp finish_cycles

; $FD SBC abs,X
op_fd:
        jsr addr_absx
        jsr P4MEM_Read
        jsr do_sbc
        lda #4
        jmp finish_cycles

; $FE INC abs,X
op_fe:
        jsr addr_absx
        jsr P4MEM_Read
        clc
        adc #1
        sta p4_data
        jsr P4MEM_Write
        lda p4_data
        jsr set_zn_a
        lda #7
        jmp finish_cycles

; ============================================================
; Illegal opcodes - all point to op_illegal
; ============================================================
op_02: jmp op_illegal
op_03: jmp op_illegal
op_04: jmp op_illegal
op_07: jmp op_illegal
op_0b: jmp op_illegal
op_0c: jmp op_illegal
op_0f: jmp op_illegal
op_12: jmp op_illegal
op_13: jmp op_illegal
op_14: jmp op_illegal
op_17: jmp op_illegal
op_1a: jmp op_illegal
op_1b: jmp op_illegal
op_1c: jmp op_illegal
op_1f: jmp op_illegal
op_22: jmp op_illegal
op_23: jmp op_illegal
op_27: jmp op_illegal
op_2b: jmp op_illegal
op_2f: jmp op_illegal
op_32: jmp op_illegal
op_33: jmp op_illegal
op_34: jmp op_illegal
op_37: jmp op_illegal
op_3a: jmp op_illegal
op_3b: jmp op_illegal
op_3c: jmp op_illegal
op_3f: jmp op_illegal
op_42: jmp op_illegal
op_43: jmp op_illegal
op_44: jmp op_illegal
op_47: jmp op_illegal
op_4b: jmp op_illegal
op_4f: jmp op_illegal
op_52: jmp op_illegal
op_53: jmp op_illegal
op_54: jmp op_illegal
op_57: jmp op_illegal
op_5a: jmp op_illegal
op_5b: jmp op_illegal
op_5c: jmp op_illegal
op_5f: jmp op_illegal
op_62: jmp op_illegal
op_63: jmp op_illegal
op_64: jmp op_illegal
op_67: jmp op_illegal
op_6b: jmp op_illegal
op_6f: jmp op_illegal
op_72: jmp op_illegal
op_73: jmp op_illegal
op_74: jmp op_illegal
op_77: jmp op_illegal
op_7a: jmp op_illegal
op_7b: jmp op_illegal
op_7c: jmp op_illegal
op_7f: jmp op_illegal
op_80: jmp op_illegal
op_82: jmp op_illegal
op_83: jmp op_illegal
op_87: jmp op_illegal
op_89: jmp op_illegal
op_8b: jmp op_illegal
op_8f: jmp op_illegal
op_92: jmp op_illegal
op_93: jmp op_illegal
op_97: jmp op_illegal
op_9b: jmp op_illegal
op_9c: jmp op_illegal
op_9e: jmp op_illegal
op_9f: jmp op_illegal
op_a3: jmp op_illegal
op_a7: jmp op_illegal
op_ab: jmp op_illegal
op_af: jmp op_illegal
op_b2: jmp op_illegal
op_b3: jmp op_illegal
op_b7: jmp op_illegal
op_bb: jmp op_illegal
op_bf: jmp op_illegal
op_c2: jmp op_illegal
op_c3: jmp op_illegal
op_c7: jmp op_illegal
op_cb: jmp op_illegal
op_cf: jmp op_illegal
op_d2: jmp op_illegal
op_d3: jmp op_illegal
op_d4: jmp op_illegal
op_d7: jmp op_illegal
op_da: jmp op_illegal
op_db: jmp op_illegal
op_dc: jmp op_illegal
op_df: jmp op_illegal
op_e2: jmp op_illegal
op_e3: jmp op_illegal
op_e7: jmp op_illegal
op_eb: jmp op_illegal
op_ef: jmp op_illegal
op_f2: jmp op_illegal
op_f3: jmp op_illegal
op_f4: jmp op_illegal
op_f7: jmp op_illegal
op_fa: jmp op_illegal
op_fb: jmp op_illegal
op_fc: jmp op_illegal
op_ff: jmp op_illegal


; ============================================================
; Opcode vector tables (split for fast indexing)
; ============================================================

op_vec_lo:
        .byte <op_00, <op_01, <op_02, <op_03, <op_04, <op_05, <op_06, <op_07
        .byte <op_08, <op_09, <op_0a, <op_0b, <op_0c, <op_0d, <op_0e, <op_0f
        .byte <op_10, <op_11, <op_12, <op_13, <op_14, <op_15, <op_16, <op_17
        .byte <op_18, <op_19, <op_1a, <op_1b, <op_1c, <op_1d, <op_1e, <op_1f
        .byte <op_20, <op_21, <op_22, <op_23, <op_24, <op_25, <op_26, <op_27
        .byte <op_28, <op_29, <op_2a, <op_2b, <op_2c, <op_2d, <op_2e, <op_2f
        .byte <op_30, <op_31, <op_32, <op_33, <op_34, <op_35, <op_36, <op_37
        .byte <op_38, <op_39, <op_3a, <op_3b, <op_3c, <op_3d, <op_3e, <op_3f
        .byte <op_40, <op_41, <op_42, <op_43, <op_44, <op_45, <op_46, <op_47
        .byte <op_48, <op_49, <op_4a, <op_4b, <op_4c, <op_4d, <op_4e, <op_4f
        .byte <op_50, <op_51, <op_52, <op_53, <op_54, <op_55, <op_56, <op_57
        .byte <op_58, <op_59, <op_5a, <op_5b, <op_5c, <op_5d, <op_5e, <op_5f
        .byte <op_60, <op_61, <op_62, <op_63, <op_64, <op_65, <op_66, <op_67
        .byte <op_68, <op_69, <op_6a, <op_6b, <op_6c, <op_6d, <op_6e, <op_6f
        .byte <op_70, <op_71, <op_72, <op_73, <op_74, <op_75, <op_76, <op_77
        .byte <op_78, <op_79, <op_7a, <op_7b, <op_7c, <op_7d, <op_7e, <op_7f
        .byte <op_80, <op_81, <op_82, <op_83, <op_84, <op_85, <op_86, <op_87
        .byte <op_88, <op_89, <op_8a, <op_8b, <op_8c, <op_8d, <op_8e, <op_8f
        .byte <op_90, <op_91, <op_92, <op_93, <op_94, <op_95, <op_96, <op_97
        .byte <op_98, <op_99, <op_9a, <op_9b, <op_9c, <op_9d, <op_9e, <op_9f
        .byte <op_a0, <op_a1, <op_a2, <op_a3, <op_a4, <op_a5, <op_a6, <op_a7
        .byte <op_a8, <op_a9, <op_aa, <op_ab, <op_ac, <op_ad, <op_ae, <op_af
        .byte <op_b0, <op_b1, <op_b2, <op_b3, <op_b4, <op_b5, <op_b6, <op_b7
        .byte <op_b8, <op_b9, <op_ba, <op_bb, <op_bc, <op_bd, <op_be, <op_bf
        .byte <op_c0, <op_c1, <op_c2, <op_c3, <op_c4, <op_c5, <op_c6, <op_c7
        .byte <op_c8, <op_c9, <op_ca, <op_cb, <op_cc, <op_cd, <op_ce, <op_cf
        .byte <op_d0, <op_d1, <op_d2, <op_d3, <op_d4, <op_d5, <op_d6, <op_d7
        .byte <op_d8, <op_d9, <op_da, <op_db, <op_dc, <op_dd, <op_de, <op_df
        .byte <op_e0, <op_e1, <op_e2, <op_e3, <op_e4, <op_e5, <op_e6, <op_e7
        .byte <op_e8, <op_e9, <op_ea, <op_eb, <op_ec, <op_ed, <op_ee, <op_ef
        .byte <op_f0, <op_f1, <op_f2, <op_f3, <op_f4, <op_f5, <op_f6, <op_f7
        .byte <op_f8, <op_f9, <op_fa, <op_fb, <op_fc, <op_fd, <op_fe, <op_ff

op_vec_hi:
        .byte >op_00, >op_01, >op_02, >op_03, >op_04, >op_05, >op_06, >op_07
        .byte >op_08, >op_09, >op_0a, >op_0b, >op_0c, >op_0d, >op_0e, >op_0f
        .byte >op_10, >op_11, >op_12, >op_13, >op_14, >op_15, >op_16, >op_17
        .byte >op_18, >op_19, >op_1a, >op_1b, >op_1c, >op_1d, >op_1e, >op_1f
        .byte >op_20, >op_21, >op_22, >op_23, >op_24, >op_25, >op_26, >op_27
        .byte >op_28, >op_29, >op_2a, >op_2b, >op_2c, >op_2d, >op_2e, >op_2f
        .byte >op_30, >op_31, >op_32, >op_33, >op_34, >op_35, >op_36, >op_37
        .byte >op_38, >op_39, >op_3a, >op_3b, >op_3c, >op_3d, >op_3e, >op_3f
        .byte >op_40, >op_41, >op_42, >op_43, >op_44, >op_45, >op_46, >op_47
        .byte >op_48, >op_49, >op_4a, >op_4b, >op_4c, >op_4d, >op_4e, >op_4f
        .byte >op_50, >op_51, >op_52, >op_53, >op_54, >op_55, >op_56, >op_57
        .byte >op_58, >op_59, >op_5a, >op_5b, >op_5c, >op_5d, >op_5e, >op_5f
        .byte >op_60, >op_61, >op_62, >op_63, >op_64, >op_65, >op_66, >op_67
        .byte >op_68, >op_69, >op_6a, >op_6b, >op_6c, >op_6d, >op_6e, >op_6f
        .byte >op_70, >op_71, >op_72, >op_73, >op_74, >op_75, >op_76, >op_77
        .byte >op_78, >op_79, >op_7a, >op_7b, >op_7c, >op_7d, >op_7e, >op_7f
        .byte >op_80, >op_81, >op_82, >op_83, >op_84, >op_85, >op_86, >op_87
        .byte >op_88, >op_89, >op_8a, >op_8b, >op_8c, >op_8d, >op_8e, >op_8f
        .byte >op_90, >op_91, >op_92, >op_93, >op_94, >op_95, >op_96, >op_97
        .byte >op_98, >op_99, >op_9a, >op_9b, >op_9c, >op_9d, >op_9e, >op_9f
        .byte >op_a0, >op_a1, >op_a2, >op_a3, >op_a4, >op_a5, >op_a6, >op_a7
        .byte >op_a8, >op_a9, >op_aa, >op_ab, >op_ac, >op_ad, >op_ae, >op_af
        .byte >op_b0, >op_b1, >op_b2, >op_b3, >op_b4, >op_b5, >op_b6, >op_b7
        .byte >op_b8, >op_b9, >op_ba, >op_bb, >op_bc, >op_bd, >op_be, >op_bf
        .byte >op_c0, >op_c1, >op_c2, >op_c3, >op_c4, >op_c5, >op_c6, >op_c7
        .byte >op_c8, >op_c9, >op_ca, >op_cb, >op_cc, >op_cd, >op_ce, >op_cf
        .byte >op_d0, >op_d1, >op_d2, >op_d3, >op_d4, >op_d5, >op_d6, >op_d7
        .byte >op_d8, >op_d9, >op_da, >op_db, >op_dc, >op_dd, >op_de, >op_df
        .byte >op_e0, >op_e1, >op_e2, >op_e3, >op_e4, >op_e5, >op_e6, >op_e7
        .byte >op_e8, >op_e9, >op_ea, >op_eb, >op_ec, >op_ed, >op_ee, >op_ef
        .byte >op_f0, >op_f1, >op_f2, >op_f3, >op_f4, >op_f5, >op_f6, >op_f7
        .byte >op_f8, >op_f9, >op_fa, >op_fb, >op_fc, >op_fd, >op_fe, >op_ff