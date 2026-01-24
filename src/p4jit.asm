; ============================================================
; P4 JIT CACHE - On-demand instruction decode cache
; ============================================================
;
; Caches decoded instruction info for ROM addresses ($8000-$FFFF)
; Built on-demand as instructions execute - never caches data.
;
; Cache structure: 2 bytes per ROM address
;   +0: opcode + 1 (0 = not cached, 1-256 = opcode 0-255)
;   +1: flags + length (bits 6-7 = length)
;
; Cache location: Attic RAM at $8000000 (64KB used)
; ROM range: $8000-$FFFF (32KB)
; Cache address = $8000000 + ((pc - $8000) << 1)
;
; ============================================================

        .cpu "45gs02"

; JIT_CheckCache is in plus4_cpu_m65.asm (must be at low address)

.if JIT_ENABLED

; ------------------------------------------------------------
; Constants
; ------------------------------------------------------------
; Attic RAM at $8000000 = $08000000
; For [zp],z 32-bit pointer: little-endian $00, $00, $00, $08
; For DMA $81 option: bits 20-27 of $8000000 = $80
JIT_CACHE_MB_DMA = $80          ; For DMA: bits 20-27 of address
JIT_CACHE_MB_PTR = $08          ; For [zp],z: byte 3 of 32-bit address
JIT_CACHE_BANK   = $00          ; Bank byte
JIT_ROM_START    = $8000
JIT_ROM_END      = $FFFF

; ------------------------------------------------------------
; Instruction lengths by opcode (0 = invalid)
; ------------------------------------------------------------
jit_inst_lengths:
        ; $00-$0F: BRK ORA(izx) - - - ORA(zp) ASL(zp) - PHP ORA# ASL - - ORA(abs) ASL(abs) -
        .byte 1, 2, 0, 0, 0, 2, 2, 0, 1, 2, 1, 0, 0, 3, 3, 0
        ; $10-$1F: BPL ORA(izy) - - - ORA(zpx) ASL(zpx) - CLC ORA(aby) - - - ORA(abx) ASL(abx) -
        .byte 2, 2, 0, 0, 0, 2, 2, 0, 1, 3, 0, 0, 0, 3, 3, 0
        ; $20-$2F: JSR AND(izx) - - BIT(zp) AND(zp) ROL(zp) - PLP AND# ROL BIT(abs) AND(abs) ROL(abs) -
        .byte 3, 2, 0, 0, 2, 2, 2, 0, 1, 2, 1, 0, 3, 3, 3, 0
        ; $30-$3F: BMI AND(izy) - - - AND(zpx) ROL(zpx) - SEC AND(aby) - - - AND(abx) ROL(abx) -
        .byte 2, 2, 0, 0, 0, 2, 2, 0, 1, 3, 0, 0, 0, 3, 3, 0
        ; $40-$4F: RTI EOR(izx) - - - EOR(zp) LSR(zp) - PHA EOR# LSR - JMP(abs) EOR(abs) LSR(abs) -
        .byte 1, 2, 0, 0, 0, 2, 2, 0, 1, 2, 1, 0, 3, 3, 3, 0
        ; $50-$5F: BVC EOR(izy) - - - EOR(zpx) LSR(zpx) - CLI EOR(aby) - - - EOR(abx) LSR(abx) -
        .byte 2, 2, 0, 0, 0, 2, 2, 0, 1, 3, 0, 0, 0, 3, 3, 0
        ; $60-$6F: RTS ADC(izx) - - - ADC(zp) ROR(zp) - PLA ADC# ROR - JMP(ind) ADC(abs) ROR(abs) -
        .byte 1, 2, 0, 0, 0, 2, 2, 0, 1, 2, 1, 0, 3, 3, 3, 0
        ; $70-$7F: BVS ADC(izy) - - - ADC(zpx) ROR(zpx) - SEI ADC(aby) - - - ADC(abx) ROR(abx) -
        .byte 2, 2, 0, 0, 0, 2, 2, 0, 1, 3, 0, 0, 0, 3, 3, 0
        ; $80-$8F: - STA(izx) - - STY(zp) STA(zp) STX(zp) - DEY - TXA - STY(abs) STA(abs) STX(abs) -
        .byte 0, 2, 0, 0, 2, 2, 2, 0, 1, 0, 1, 0, 3, 3, 3, 0
        ; $90-$9F: BCC STA(izy) - - STY(zpx) STA(zpx) STX(zpy) - TYA STA(aby) TXS - - STA(abx) - -
        .byte 2, 2, 0, 0, 2, 2, 2, 0, 1, 3, 1, 0, 0, 3, 0, 0
        ; $A0-$AF: LDY# LDA(izx) LDX# - LDY(zp) LDA(zp) LDX(zp) - TAY LDA# TAX - LDY(abs) LDA(abs) LDX(abs) -
        .byte 2, 2, 2, 0, 2, 2, 2, 0, 1, 2, 1, 0, 3, 3, 3, 0
        ; $B0-$BF: BCS LDA(izy) - - LDY(zpx) LDA(zpx) LDX(zpy) - CLV LDA(aby) TSX - LDY(abx) LDA(abx) LDX(aby) -
        .byte 2, 2, 0, 0, 2, 2, 2, 0, 1, 3, 1, 0, 3, 3, 3, 0
        ; $C0-$CF: CPY# CMP(izx) - - CPY(zp) CMP(zp) DEC(zp) - INY CMP# DEX - CPY(abs) CMP(abs) DEC(abs) -
        .byte 2, 2, 0, 0, 2, 2, 2, 0, 1, 2, 1, 0, 3, 3, 3, 0
        ; $D0-$DF: BNE CMP(izy) - - - CMP(zpx) DEC(zpx) - CLD CMP(aby) - - - CMP(abx) DEC(abx) -
        .byte 2, 2, 0, 0, 0, 2, 2, 0, 1, 3, 0, 0, 0, 3, 3, 0
        ; $E0-$EF: CPX# SBC(izx) - - CPX(zp) SBC(zp) INC(zp) - INX SBC# NOP - CPX(abs) SBC(abs) INC(abs) -
        .byte 2, 2, 0, 0, 2, 2, 2, 0, 1, 2, 1, 0, 3, 3, 3, 0
        ; $F0-$FF: BEQ SBC(izy) - - - SBC(zpx) INC(zpx) - SED SBC(aby) - - - SBC(abx) INC(abx) -
        .byte 2, 2, 0, 0, 0, 2, 2, 0, 1, 3, 0, 0, 0, 3, 3, 0


; ============================================================
; JIT_InitCache - DMA clear Attic RAM cache area
; Call once at emulator startup
; ============================================================
JIT_InitCache:
        ; -------------------------------------------------
        ; DMA fill 64KB at $8000000 with $00
        ; Must do in two 32KB chunks since count is 16-bit
        ; -------------------------------------------------
        
        ; First 32KB: $8000000-$8007FFF
        lda #>_jit_dma_list1
        sta $D701
        lda #<_jit_dma_list1
        sta $D705               ; Trigger DMA
        
        ; Second 32KB: $8008000-$800FFFF
        lda #>_jit_dma_list2
        sta $D701
        lda #<_jit_dma_list2
        sta $D705               ; Trigger DMA
        
        ; Mark cache as ready
        lda #$01
        sta jit_cache_ready
        rts

_JIT_InitCache_DISABLED:

; DMA list for clearing first half (32KB at $8000000)
_jit_dma_list1:
        .byte $80, $00              ; Source MB = 0
        .byte $81, JIT_CACHE_MB_DMA ; Dest MB bits 20-27 = $80
        .byte $00                   ; End of options
        .byte $03                   ; Command: fill
        .word $8000                 ; Count: 32KB
        .word $0000                 ; Source address (fill value = $00 in low byte)
        .byte $00                   ; Source bank
        .word $0000                 ; Dest address: $0000
        .byte JIT_CACHE_BANK        ; Dest bank
        .word $0000                 ; Modulo (unused)

; DMA list for clearing second half (32KB at $8008000)
_jit_dma_list2:
        .byte $80, $00              ; Source MB = 0
        .byte $81, JIT_CACHE_MB_DMA ; Dest MB bits 20-27 = $80
        .byte $00                   ; End of options
        .byte $03                   ; Command: fill
        .word $8000                 ; Count: 32KB
        .word $0000                 ; Source address (fill value = $00 in low byte)
        .byte $00                   ; Source bank
        .word $8000                 ; Dest address: $8000
        .byte JIT_CACHE_BANK        ; Dest bank
        .word $0000                 ; Modulo (unused)


; ============================================================
; JIT_StoreCache - Store decoded instruction in cache
; ============================================================
JIT_StoreCache:
        ; Only store if instruction started in ROM range
        lda p4_inst_pc_hi
        cmp #>JIT_ROM_START
        bcc _jit_store_done     ; Not ROM, don't cache
        
        ; Calculate cache address: $8000000 + ((inst_pc - $8000) << 1)
        sec
        lda p4_inst_pc_lo
        sbc #<JIT_ROM_START
        sta jit_cache_addr
        lda p4_inst_pc_hi
        sbc #>JIT_ROM_START
        sta jit_cache_addr+1
        
        asl jit_cache_addr
        rol jit_cache_addr+1
        
        ; Set bank and megabyte for Attic RAM
        lda #JIT_CACHE_BANK
        sta jit_cache_addr+2
        lda #JIT_CACHE_MB_PTR
        sta jit_cache_addr+3
        
        ; Build byte 1: length in bits 6-7
        lda jit_inst_length
        asl
        asl
        asl
        asl
        asl
        asl                     ; Length now in bits 6-7
        sta jit_cached_info
        
        ; Write cache entry
        ; Store opcode+1 (so 0 means "not cached", opcode $00 BRK = 1)
        ldz #0
        lda jit_handler_idx
        clc
        adc #1                  ; Store opcode + 1
        ; 32-bit sta [jit_cache_addr],z = $EA $92 $1D
        .byte $EA, $92, jit_cache_addr
        inz
        lda jit_cached_info
        ; 32-bit sta [jit_cache_addr],z = $EA $92 $1D
        .byte $EA, $92, jit_cache_addr

_jit_store_done:
        rts


; ------------------------------------------------------------
; Handler lookup - uses existing op_vec_lo/hi tables
; The cache stores the opcode directly, not a separate index
; ------------------------------------------------------------

; JIT_GetHandler - Get handler address from cached opcode
; Input: jit_handler_idx (which is actually the opcode)
; Output: p4_vec_lo/hi set for dispatch
JIT_GetHandler:
        ldx jit_handler_idx
        lda op_vec_lo,x
        sta p4_vec_lo
        lda op_vec_hi,x
        sta p4_vec_hi
        rts


; ============================================================
; JIT_Execute - Execute instruction using cached decode
;
; Call after JIT_CheckCache returns C=1 and JIT_GetHandler
; Note: Does NOT advance PC - handlers do that via fetch8/fetch16
; ============================================================
JIT_Execute:
        ; Jump to handler - handlers will advance PC as needed
        jmp (p4_vec_lo)


; ------------------------------------------------------------
; Variables - all in zero page to avoid any address issues
; ------------------------------------------------------------
; Zero page locations - avoid conflicts:
;   $02-$11: CPU emulation
;   $12-$18: SID emulation  
;   $19-$1C: CPU (irq/nmi/inst_pc)
;   $1D-$24: JIT cache variables
;   $2B+: BASIC pointers
jit_cache_addr  = $1D           ; 4 bytes: 32-bit cache pointer
jit_cache_ready = $21           ; 1 byte
jit_handler_idx = $22           ; 1 byte: Cached opcode value
jit_cached_info = $23           ; 1 byte: Cached flags + length
jit_inst_length = $24           ; 1 byte: Instruction length (1-3)


; ============================================================
; INTEGRATION WITH P4CPU_Step
; ============================================================
;
; In _step_execute (plus4_cpu_m65.asm), replace:
;
;   jsr fetch8
;   tax
;   lda op_vec_lo,x
;   sta p4_vec_lo
;   lda op_vec_hi,x
;   sta p4_vec_hi
;   jmp (p4_vec_lo)
;
; With:
;
;   ; Try JIT cache first (ROM addresses only)
;   jsr JIT_CheckCache
;   bcc _jit_miss
;   
;   ; Cache hit - dispatch directly
;   jsr JIT_GetHandler
;   jmp (p4_vec_lo)
;   
; _jit_miss:
;   ; Normal fetch/decode
;   jsr fetch8
;   sta jit_handler_idx      ; Save opcode for caching
;   tax
;   
;   ; Look up instruction length for cache
;   lda jit_inst_lengths,x
;   sta jit_inst_length
;   
;   lda op_vec_lo,x
;   sta p4_vec_lo
;   lda op_vec_hi,x
;   sta p4_vec_hi
;   
;   ; Store in cache for next time
;   jsr JIT_StoreCache
;   
;   jmp (p4_vec_lo)
;
; ============================================================

.endif  ; JIT_ENABLED