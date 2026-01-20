; ============================================================
; p4hooks.asm - BASIC/KERNAL hook points for Plus/4 emulator
; Host: MEGA65
; Assembler: 64tass (45GS02)
;
; Intercepts SETNAM/SETLFS to capture filename parameters
; directly from the emulated CPU registers, then uses them when
; the LOAD or SAVE KERNAL calls happen.
;
; ============================================================

        .cpu "45gs02"

; ------------------------------------------------------------
; Hooked guest PC locations
; ------------------------------------------------------------
; Plus/4 KERNAL jump table entries
P4_SETNAM               = $FFBD         ; SETNAM entry point
P4_SETLFS               = $FFBA         ; SETLFS entry point  
P4_LOAD                 = $FFD5         ; LOAD entry point
P4_SAVE                 = $FFD8         ; SAVE entry point

; Inside BASIC LOAD handler: the JSR $FFD5 instruction
P4HOOK_ROM_LOAD_KERNAL_CALL      = $A800
P4HOOK_ROM_AFTER_KERNAL_CALL     = $A803

; Inside BASIC SAVE handler: the JSR $FFD8 instruction
; We'll find this by hooking KERNAL SAVE directly
P4HOOK_ROM_SAVE_KERNAL_CALL      = $A851  ; Approximate - we hook $FFD8 instead
P4HOOK_ROM_AFTER_SAVE_CALL       = $A854  ; Return address after SAVE

; DIRECTORY keyword handler - actual entry point in BASIC ROM
P4HOOK_ROM_DIRECTORY             = $C8BC

; DIRECTORY keyword handler entry (from BASIC keyword vector table) - old
P4HOOK_ROM_PERFORM_DIRECTORY     = $ED7F

; LIST keyword handler entry (from BASIC keyword vector table)
P4HOOK_ROM_PERFORM_LIST          = $8AFE

; A "fake" return address used to regain control after LIST prints.
P4HOOK_DIR_RESTORE_TRAP          = $02F6

; ------------------------------------------------------------
; Plus/4 BASIC low-RAM pointers we need
; ------------------------------------------------------------
ZP_TXTTAB_LO = $2B
ZP_TXTTAB_HI = $2C
ZP_VARTAB_LO = $2D
ZP_VARTAB_HI = $2E
ZP_TOPMEM_LO = $37
ZP_TOPMEM_HI = $38

ZP_PTRS_BASE  = $2B
ZP_PTRS_COUNT = 10

; ------------------------------------------------------------
; Directory load configuration
; ------------------------------------------------------------
P4_BANK_RAM     = $05           ; emulated Plus/4 RAM lives in MEGA65 bank 5

; Host-side staging buffer (bank 0 address)
P4_DIR_BUF      = $6000         ; 4KB staging buffer in bank 0

; MEGA65 KERNAL SAVE (may not be in main.asm)
SAVE            = $FFD8

p4_dir_len_lo:  .byte 0
p4_dir_len_hi:  .byte 0

p4_dir_dest_lo: .byte 0
p4_dir_dest_hi: .byte 0

p4_saved_basic_ptrs:
        .fill ZP_PTRS_COUNT, 0

p4_dir_temp_lo: .byte 0
p4_dir_temp_hi: .byte 0

tmp_lo: .byte 0
tmp_hi: .byte 0

; Filename "$" for CBM directory listing
p4_dir_name:
        .byte '$'

; ------------------------------------------------------------
; SETNAM capture variables - this is the key new addition!
; ------------------------------------------------------------
p4_setnam_len:    .byte 0       ; Captured filename length
p4_setnam_ptr_lo: .byte 0       ; Captured filename pointer lo
p4_setnam_ptr_hi: .byte 0       ; Captured filename pointer hi
p4_setnam_valid:  .byte 0       ; Flag: 1 = we have valid SETNAM data

; SETLFS capture variables
p4_setlfs_dev:    .byte 8       ; Device number (default 8)
p4_setlfs_sa:     .byte 0       ; Secondary address

; File load buffer
p4_fl_buf:      .fill 17, 0     ; Filename buffer (16 chars + null)
p4_fl_len:      .byte 0
p4_fl_end_lo:   .byte 0
p4_fl_end_hi:   .byte 0

; SAVE variables
p4_save_start_lo: .byte 0       ; Start address of data to save
p4_save_start_hi: .byte 0
p4_save_end_lo:   .byte 0       ; End address + 1
p4_save_end_hi:   .byte 0

; ------------------------------------------------------------
; P4HOOK_CheckAndRun
;   Called once per emulated instruction, right before opcode fetch.
; ------------------------------------------------------------
P4HOOK_CheckAndRun:
        pha
        txa
        pha
        tya
        pha

        lda p4_pc_hi

        ; ----- Check for $FFxx addresses (SETNAM, SETLFS, SAVE) -----
        cmp #$FF
        bne _not_ff
        lda p4_pc_lo
        cmp #$BD                        ; SETNAM = $FFBD
        beq _do_setnam
        cmp #$BA                        ; SETLFS = $FFBA
        beq _do_setlfs
        cmp #$D8                        ; SAVE = $FFD8
        beq _do_save
        jmp _check_other

_do_setnam:
        jsr P4HOOK_OnSETNAM
        jmp _done

_do_setlfs:
        jsr P4HOOK_OnSETLFS
        jmp _done

_do_save:
        jsr P4HOOK_OnSAVE
        jmp _done

_not_ff:
        ; ----- Check for $C8xx (DIRECTORY at $C8BC) -----
        cmp #$C8
        bne _check_a8
        lda p4_pc_lo
        cmp #$BC                        ; DIRECTORY = $C8BC
        bne _done
        jsr P4HOOK_OnDIRECTORY_New
        jmp _done

_check_a8:
        ; ----- Check for LOAD hook at $A800 -----
        cmp #$A8
        bne _done
        lda p4_pc_lo
        cmp #$00
        bne _done

_check_other:
        lda p4_pc_hi
        cmp #$A8
        bne _done
        lda p4_pc_lo
        bne _done
        
        ; We're at $A800 - the JSR $FFD5 inside BASIC LOAD
        jsr P4HOOK_OnLOAD

_done:
        pla
        tay
        pla
        tax
        pla
        rts


; ============================================================
; Hook: SETNAM - Capture filename parameters from registers
; Called when guest PC == $FFBD
; 
; On entry to SETNAM:
;   A = filename length
;   X = filename pointer low byte
;   Y = filename pointer high byte
;
; NOTE: LOAD calls SETNAM twice:
;   1st call: length=0 to clear filename
;   2nd call: length>0 with actual filename, pointer to RAM UNDER ROM
; ============================================================
P4HOOK_OnSETNAM:
        ; Only capture if length > 0 (skip the clearing call)
        lda p4_a
        beq _setnam_skip
        
        ; Capture the parameters from emulated CPU registers
        sta p4_setnam_len
        
        lda p4_x
        sta p4_setnam_ptr_lo
        
        lda p4_y
        sta p4_setnam_ptr_hi
        
        ; Mark that we have valid SETNAM data
        lda #1
        sta p4_setnam_valid
        
_setnam_skip:
        ; Let the ROM SETNAM continue normally
        rts


; ============================================================
; Hook: SETLFS - Capture device number and secondary address
; Called when guest PC == $FFBA
;
; On entry to SETLFS:
;   A = logical file number
;   X = device number
;   Y = secondary address
; ============================================================
P4HOOK_OnSETLFS:
        ; Capture device and secondary address
        lda p4_x
        sta p4_setlfs_dev
        
        lda p4_y
        sta p4_setlfs_sa
        
        ; Let the ROM SETLFS continue normally
        rts


; ============================================================
; Hook: LOAD - Handle the actual file loading
; Called when guest PC == $A800 (JSR $FFD5 inside BASIC LOAD)
; ============================================================
P4HOOK_OnLOAD:
        ; Check if we have valid SETNAM data
        lda p4_setnam_valid
        beq _load_no_setnam
        
        ; Check filename length - if 1, might be "$" for directory
        lda p4_setnam_len
        cmp #1
        bne _load_file
        
        ; Check if it's "$" for directory
        ; Read from RAM under ROM using 32-bit addressing
        lda p4_setnam_ptr_lo
        sta P4_MEM_PTR
        lda p4_setnam_ptr_hi
        sta P4_MEM_PTR+1
        lda #BANK_RAM
        sta P4_MEM_PTR+2
        lda #$00
        sta P4_MEM_PTR+3
        
        ldz #0
        lda [P4_MEM_PTR],z
        cmp #'$'
        beq _load_directory
        
        ; Single character filename that's not "$" - load as file
        jmp _load_file

_load_no_setnam:
        ; No SETNAM data - can't do anything, let ROM handle it
        rts

_load_directory:
        ; Clear the valid flag
        lda #0
        sta p4_setnam_valid
        
        ; Print "SEARCHING FOR "
        lda #<P4Host_Msg_Searching
        ldx #>P4Host_Msg_Searching
        jsr P4Host_PrintString
        
        ; Print the filename
        lda #'$'
        jsr P4Host_PutChar
        
        lda #<P4Host_Msg_Loading
        ldx #>P4Host_Msg_Loading
        jsr P4Host_PrintString

        ; Load directory listing
        jsr P4HOOK_LoadDirectory
        rts

_load_file:
        ; Clear the valid flag
        lda #0
        sta p4_setnam_valid
        
        ; Copy filename from guest RAM to our buffer
        jsr P4HOOK_CopyFilename
        bcs _load_file_error
        
        ; Print "SEARCHING FOR "
        lda #<P4Host_Msg_Searching
        ldx #>P4Host_Msg_Searching
        jsr P4Host_PrintString
        
        ; Print the filename
        ldy #0
    _print_name:
        cpy p4_fl_len
        beq _print_done
        lda p4_fl_buf,y
        phy
        jsr P4Host_PutChar          ; (alias for P4Host_PrintCharSync)
        ply
        iny
        bne _print_name
    _print_done:
        
        ; Now load the file using host KERNAL
        jsr P4HOOK_DoHostLoad
        rts

_load_file_error:
        ; Set carry and error code, skip to after JSR
        lda p4_p
        ora #P_C
        sta p4_p
        lda #$04                        ; FILE NOT FOUND
        sta p4_a
        lda #<P4HOOK_ROM_AFTER_KERNAL_CALL
        sta p4_pc_lo
        lda #>P4HOOK_ROM_AFTER_KERNAL_CALL
        sta p4_pc_hi
        rts


; ============================================================
; P4HOOK_CopyFilename - Copy filename from guest RAM to buffer
; 
; NOTE: The filename pointer points to RAM UNDER ROM, so we must
; read directly from Bank 5 (guest RAM), NOT through P4MEM_Read
; which would see the ROM overlay.
;
; Returns: C=0 success, C=1 error
; ============================================================
P4HOOK_CopyFilename:
        lda p4_setnam_len
        beq _cf_error                   ; No filename
        cmp #17
        bcs _cf_error                   ; Too long
        sta p4_fl_len
        
        ; Set up 32-bit pointer to read from Bank 5 (guest RAM)
        ; P4_MEM_PTR is at $F0-$F3
        lda p4_setnam_ptr_lo
        sta P4_MEM_PTR
        lda p4_setnam_ptr_hi
        sta P4_MEM_PTR+1
        lda #BANK_RAM                   ; Bank 5 = guest RAM
        sta P4_MEM_PTR+2
        lda #$00
        sta P4_MEM_PTR+3
        
        ; Copy bytes using 32-bit indirect addressing
        ldy #0
_cf_loop:
        cpy p4_fl_len
        beq _cf_done
        
        ; Read from [P4_MEM_PTR],y - this reads from Bank 5 directly
        ldz #0
        tya
        taz                             ; Z = Y (offset)
        lda [P4_MEM_PTR],z
        
        ; Store in buffer
        sta p4_fl_buf,y
        
        iny
        bne _cf_loop                    ; Always branch (Y < 17)
        
_cf_done:
        ; Null terminate
        lda #0
        sta p4_fl_buf,y
        clc
        rts
        
_cf_error:
        sec
        rts


; Old print routines removed - now using P4Host_* routines from p4host.asm


; ============================================================
; P4HOOK_DoHostLoad - Load file using host MEGA65 KERNAL
; ============================================================
P4HOOK_DoHostLoad:
        ; Set up host KERNAL for load
        lda #$00
        ldx #$00
        jsr SETBNK
        
        ; Set filename
        ldx #<p4_fl_buf
        ldy #>p4_fl_buf
        lda p4_fl_len
        jsr SETNAM
        
        ; Set device (use captured device, default to 8)
        lda #$01                        ; Logical file number
        ldx p4_setlfs_dev
        bne +
        ldx #$08                        ; Default to device 8
+       ldy p4_setlfs_sa                ; Secondary address
        jsr SETLFS
        
        ; Load to staging buffer
        lda #$00                        ; 0 = LOAD (not verify)
        ldx #<P4_DIR_BUF
        ldy #>P4_DIR_BUF
        jsr LOAD
        bcc _dhl_ok
        
        ; Load failed
        lda p4_p
        ora #P_C
        sta p4_p
        lda #$04                        ; FILE NOT FOUND error
        sta p4_a
        jmp _dhl_set_pc

_dhl_ok:
        ; Save end address from LOAD (in X/Y)
        stx p4_fl_end_lo
        sty p4_fl_end_hi
        
        ; Print "LOADING" message
        lda #<P4Host_Msg_Loading
        ldx #>P4Host_Msg_Loading
        jsr P4Host_PrintString
        
        ; Restore end address
        ldx p4_fl_end_lo
        ldy p4_fl_end_hi
        
        ; Calculate loaded length
        stx p4_fl_end_lo
        sty p4_fl_end_hi
        
        lda p4_fl_end_lo
        sec
        sbc #<P4_DIR_BUF
        sta p4_dir_len_lo
        lda p4_fl_end_hi
        sbc #>P4_DIR_BUF
        sta p4_dir_len_hi
        
        ; Need at least 2 bytes (load address header)
        lda p4_dir_len_hi
        bne _dhl_has_data
        lda p4_dir_len_lo
        cmp #2
        bcc _dhl_error

_dhl_has_data:
        ; Determine destination address
        ; SA=0: use TXTTAB, SA!=0: use file header address
        lda p4_setlfs_sa
        bne _dhl_use_file_addr
        
        ; SA=0: destination = TXTTAB
        lda LOW_RAM_BUFFER + ZP_TXTTAB_LO
        sta p4_dir_dest_lo
        lda LOW_RAM_BUFFER + ZP_TXTTAB_HI
        sta p4_dir_dest_hi
        jmp _dhl_do_copy

_dhl_use_file_addr:
        ; Use address from file header (first 2 bytes)
        lda P4_DIR_BUF
        sta p4_dir_dest_lo
        lda P4_DIR_BUF+1
        sta p4_dir_dest_hi

_dhl_do_copy:
        ; Subtract 2 from length (skip header)
        lda p4_dir_len_lo
        sec
        sbc #2
        sta p4_dir_len_lo
        lda p4_dir_len_hi
        sbc #0
        sta p4_dir_len_hi
        
        ; DMA copy to guest memory (skip 2-byte header)
        jsr P4HOOK_DMACopyFileToGuest
        
        ; Clear KERNAL status
        lda #$00
        sta LOW_RAM_BUFFER + $90
        
        ; Set end address in X/Y registers
        clc
        lda p4_dir_dest_lo
        adc p4_dir_len_lo
        sta p4_x
        lda p4_dir_dest_hi
        adc p4_dir_len_hi
        sta p4_y
        
        ; Clear carry = success
        lda p4_p
        and #((~P_C) & $FF)
        sta p4_p
        jmp _dhl_set_pc

_dhl_error:
        lda p4_p
        ora #P_C
        sta p4_p
        lda #$04
        sta p4_a

_dhl_set_pc:
        ; Skip past the JSR $FFD5
        lda #<P4HOOK_ROM_AFTER_KERNAL_CALL
        sta p4_pc_lo
        lda #>P4HOOK_ROM_AFTER_KERNAL_CALL
        sta p4_pc_hi
        rts


; ============================================================
; Hook: SAVE - Handle file saving
; Called when guest PC == $FFD8 (KERNAL SAVE entry)
;
; On entry to KERNAL SAVE:
;   A = zero page address containing start address pointer
;   X = end address low byte
;   Y = end address high byte
;
; The start address is read from the ZP location pointed to by A.
; End address is the first byte NOT to save (exclusive).
; ============================================================
P4HOOK_OnSAVE:
        ; Check if we have valid SETNAM data
        lda p4_setnam_valid
        beq _save_no_setnam
        
        ; Clear the valid flag
        lda #0
        sta p4_setnam_valid
        
        ; Get end address from X/Y registers
        lda p4_x
        sta p4_save_end_lo
        lda p4_y
        sta p4_save_end_hi
        
        ; Get start address - A register contains ZP address
        ; The ZP location contains a 2-byte pointer to start of data
        ; ZP is in LOW_RAM_BUFFER, not Bank 5!
        lda p4_a                        ; ZP address (e.g., $2B for TXTTAB)
        tax
        
        ; Read start address from LOW_RAM_BUFFER (ZP is in low RAM)
        lda LOW_RAM_BUFFER,x            ; Start lo
        sta p4_save_start_lo
        inx
        lda LOW_RAM_BUFFER,x            ; Start hi
        sta p4_save_start_hi
        
        ; Copy filename from guest RAM to buffer
        jsr P4HOOK_CopyFilename
        bcs _save_error
        
        ; Perform the save
        jsr P4HOOK_DoHostSave
        rts

_save_no_setnam:
        ; No filename set - let ROM handle error
        rts

_save_error:
        ; Filename copy error
        jsr P4HOOK_SaveSetError
        rts


; ============================================================
; P4HOOK_DoHostSave - Save file using host MEGA65 KERNAL
; ============================================================
P4HOOK_DoHostSave:
        ; Calculate data length
        lda p4_save_end_lo
        sec
        sbc p4_save_start_lo
        sta p4_dir_len_lo
        lda p4_save_end_hi
        sbc p4_save_start_hi
        sta p4_dir_len_hi
        
        ; Check for zero or negative length
        lda p4_dir_len_hi
        bmi _dhs_error                  ; Negative = error
        ora p4_dir_len_lo
        beq _dhs_error                  ; Zero length = error
        
        ; DMA copy from guest RAM to staging buffer
        ; First 2 bytes = load address header
        lda p4_save_start_lo
        sta P4_DIR_BUF
        lda p4_save_start_hi
        sta P4_DIR_BUF+1
        
        ; Copy program data from guest RAM (bank 5) to staging buffer+2
        jsr P4HOOK_DMACopyGuestToHost


        ; Print "SAVING "
        lda #<P4Host_Msg_Saving
        ldx #>P4Host_Msg_Saving
        jsr P4Host_PrintString
        
        ; Print the filename
        ldy #0
    _print_name:
        cpy p4_fl_len
        beq _print_done
        lda p4_fl_buf,y
        phy
        jsr P4Host_PutChar          ; (alias for P4Host_PrintCharSync)
        ply
        iny
        bne _print_name
    _print_done:
        
        ; Set up host KERNAL for save
        lda #$00
        ldx #$00
        jsr SETBNK
        
        ; Set filename
        ldx #<p4_fl_buf
        ldy #>p4_fl_buf
        lda p4_fl_len
        jsr SETNAM
        
        ; Set device
        lda #$01                        ; Logical file number
        ldx p4_setlfs_dev
        bne +
        ldx #$08                        ; Default to device 8
+       ldy #$01                        ; SA=1 for save with relocate address
        jsr SETLFS
        
        ; Calculate total length including 2-byte header
        clc
        lda p4_dir_len_lo
        adc #2
        sta _save_total_lo
        lda p4_dir_len_hi
        adc #0
        sta _save_total_hi
        
        ; SAVE - A = ZP pointer to start, X/Y = end address
        ; Set up a ZP pointer to P4_DIR_BUF
        lda #<P4_DIR_BUF
        sta $FB
        lda #>P4_DIR_BUF
        sta $FC
        
        ; End address = P4_DIR_BUF + total_length
        clc
        lda #<P4_DIR_BUF
        adc _save_total_lo
        tax
        lda #>P4_DIR_BUF
        adc _save_total_hi
        tay
        
        ; A = ZP address of start pointer ($FB)
        lda #$FB
        jsr SAVE
        bcc _dhs_ok
        
        ; Save failed
        jsr P4HOOK_SaveSetError
        rts

_dhs_ok:
        ; Clear KERNAL status in guest
        lda #$00
        sta LOW_RAM_BUFFER + $90
        
        ; Clear carry = success in guest P register
        lda p4_p
        and #((~P_C) & $FF)
        sta p4_p
        
        ; Set A to 0 (no error)
        lda #$00
        sta p4_a
        
        ; We intercepted at $FFD8 (KERNAL SAVE entry)
        ; Pop the return address from guest stack and set PC to return
        jsr P4HOOK_RTS_Guest
        rts

_dhs_error:
        jsr P4HOOK_SaveSetError
        rts

_save_total_lo: .byte 0
_save_total_hi: .byte 0


; ============================================================
; P4HOOK_SaveSetError - Set error status for failed SAVE
; ============================================================
P4HOOK_SaveSetError:
        lda p4_p
        ora #P_C
        sta p4_p
        lda #$05                        ; DEVICE NOT PRESENT or similar
        sta p4_a
        
        ; Pop return address and return from SAVE
        jsr P4HOOK_RTS_Guest
        rts


; ============================================================
; P4HOOK_DMACopyGuestToHost - Copy from guest RAM to staging buffer
; Source: Bank 5 at p4_save_start
; Dest: P4_DIR_BUF+2 (after load address header)
; Length: p4_dir_len
; ============================================================
P4HOOK_DMACopyGuestToHost:
        lda p4_dir_len_lo
        sta _dma_g2h_len_lo
        lda p4_dir_len_hi
        sta _dma_g2h_len_hi
        lda p4_save_start_lo
        sta _dma_g2h_src_lo
        lda p4_save_start_hi
        sta _dma_g2h_src_hi

        lda #$00
        sta $D707
        .byte $80,$00,$81,$00,$00
        .byte $00                       ; copy
_dma_g2h_len_lo:
        .byte $00
_dma_g2h_len_hi:
        .byte $00
_dma_g2h_src_lo:
        .byte $00
_dma_g2h_src_hi:
        .byte $00
        .byte BANK_RAM                  ; src bank 5 (guest RAM)
        .byte <(P4_DIR_BUF+2)           ; dest lo
        .byte >(P4_DIR_BUF+2)           ; dest hi
        .byte $00                       ; dest bank 0 (host RAM)
        .byte $00
        .word $0000
        rts


; ============================================================
; P4HOOK_LoadDirectory - Load "$" directory as BASIC program
; ============================================================
P4HOOK_LoadDirectory:
        ; Set up host KERNAL
        lda #$00
        ldx #$00
        jsr SETBNK
        
        ; SETNAM("$", len=1)
        ldx #<p4_dir_name
        ldy #>p4_dir_name
        lda #$01
        jsr SETNAM
        
        ; SETLFS(lfn=1, device=8, sa=0)
        lda #$01
        ldx #$08
        ldy #$00
        jsr SETLFS
        
        ; LOAD to staging buffer
        lda #$00
        ldx #<P4_DIR_BUF
        ldy #>P4_DIR_BUF
        jsr LOAD
        bcc _ld_ok
        
        ; Load failed - let ROM handle the error
        rts

_ld_ok:
        ; Store end address
        stx _ld_end_lo
        sty _ld_end_hi
        
        ; Calculate length
        lda _ld_end_lo
        sec
        sbc #<P4_DIR_BUF
        sta p4_dir_len_lo
        lda _ld_end_hi
        sbc #>P4_DIR_BUF
        sta p4_dir_len_hi
        
        ; Check for zero length
        lda p4_dir_len_lo
        ora p4_dir_len_hi
        beq _ld_bypass
        
        ; Destination = TXTTAB
        jsr P4HOOK_SetDestFromTXTTAB
        
        ; Clear first byte
        jsr P4HOOK_ClearGuestDest1
        
        ; DMA copy
        jsr P4HOOK_DMACopyDirToGuest

_ld_bypass:
        ; Clear KERNAL status
        lda #$00
        sta p4_addr_hi
        lda #$90
        sta p4_addr_lo
        jsr P4MEM_WriteA0
        
        ; Set end address in X/Y
        clc
        lda p4_dir_dest_lo
        adc p4_dir_len_lo
        sta p4_x
        lda p4_dir_dest_hi
        adc p4_dir_len_hi
        sta p4_y
        
        ; Clear carry
        lda p4_p
        and #((~P_C) & $ff)
        sta p4_p
        
        ; Skip JSR $FFD5
        lda #<P4HOOK_ROM_AFTER_KERNAL_CALL
        sta p4_pc_lo
        lda #>P4HOOK_ROM_AFTER_KERNAL_CALL
        sta p4_pc_hi
        rts

_ld_end_lo: .byte 0
_ld_end_hi: .byte 0


; ============================================================
; DMA copy file data (skip 2-byte header) to guest RAM
; ============================================================
P4HOOK_DMACopyFileToGuest:
        lda p4_dir_len_lo
        sta _dma_fl_len_lo
        lda p4_dir_len_hi
        sta _dma_fl_len_hi
        lda p4_dir_dest_lo
        sta _dma_fl_dst_lo
        lda p4_dir_dest_hi
        sta _dma_fl_dst_hi

        lda #$00
        sta $D707
        .byte $80,$00,$81,$00,$00
        .byte $00                       ; copy
_dma_fl_len_lo:
        .byte $00
_dma_fl_len_hi:
        .byte $00
        .byte <(P4_DIR_BUF+2)           ; src lo (skip 2-byte header)
        .byte >(P4_DIR_BUF+2)           ; src hi
        .byte $00                       ; src bank 0
_dma_fl_dst_lo:
        .byte $00
_dma_fl_dst_hi:
        .byte $00
        .byte P4_BANK_RAM               ; dest bank 5
        .byte $00
        .word $0000
        rts


; ============================================================
; Helper routines (from original)
; ============================================================

P4HOOK_SetDestFromTXTTAB:
        lda #$00
        sta p4_addr_hi
        lda #ZP_TXTTAB_LO
        sta p4_addr_lo
        jsr P4MEM_Read
        sta p4_dir_dest_lo
        lda #ZP_TXTTAB_HI
        sta p4_addr_lo
        jsr P4MEM_Read
        sta p4_dir_dest_hi
        rts

P4HOOK_DMACopyDirToGuest:
        lda p4_dir_len_lo
        sta _dma_count_lo
        lda p4_dir_len_hi
        sta _dma_count_hi
        lda p4_dir_dest_lo
        sta _dma_dst_lo
        lda p4_dir_dest_hi
        sta _dma_dst_hi

        lda #$00
        sta $D707
        .byte $80,$00,$81,$00,$00
        .byte $00
_dma_count_lo:
        .byte $00
_dma_count_hi:
        .byte $00
        .byte <P4_DIR_BUF
        .byte >P4_DIR_BUF
        .byte $00
_dma_dst_lo:
        .byte $00
_dma_dst_hi:
        .byte $00
        .byte P4_BANK_RAM
        .byte $00
        .word $0000
        rts

P4HOOK_ClearGuestDest1:
        lda p4_dir_dest_lo
        sta _fill_dst_lo
        lda p4_dir_dest_hi
        sta _fill_dst_hi

        lda #$00
        sta $D707
        .byte $80,$00,$81,$00,$00
        .byte $03
        .word $0001
        .word $0000
        .byte $00
_fill_dst_lo:
        .byte $00
_fill_dst_hi:
        .byte $00
        .byte P4_BANK_RAM
        .byte $00
        .word $0000
        rts

; --- Memory write helpers ---
P4MEM_WriteA:
        sta p4_data
        jsr P4MEM_Write
        rts

P4MEM_WriteA0:
        lda #$00
        sta p4_data
        jsr P4MEM_Write
        rts


; ============================================================
; DIRECTORY support - NEW implementation
; ============================================================

; Called when PC = $C8BC (DIRECTORY keyword entry)
P4HOOK_OnDIRECTORY_New:
        ; Load directory from host SD card
        lda #$00
        ldx #$00
        jsr SETBNK

        ldx #<p4_dir_name
        ldy #>p4_dir_name
        lda #$01
        jsr SETNAM

        lda #$01
        ldx #$08
        ldy #$00
        jsr SETLFS

        ; Load directory to staging buffer
        lda #$00                        ; LOAD (not verify)
        ldx #<P4_DIR_BUF
        ldy #>P4_DIR_BUF
        jsr LOAD
        bcc _dir_new_ok
        
        ; Load failed - just return to BASIC
        jsr P4HOOK_RTS_Guest
        rts

_dir_new_ok:
        ; Save end address
        stx p4dir_end_lo
        sty p4dir_end_hi
        
        ; Calculate length
        lda p4dir_end_lo
        sec
        sbc #<P4_DIR_BUF
        sta p4dir_len_lo
        lda p4dir_end_hi
        sbc #>P4_DIR_BUF
        sta p4dir_len_hi
        
        ; Parse and print directory listing
        jsr P4HOOK_PrintDirectory
        
        ; Return to BASIC via RTS
        jsr P4HOOK_RTS_Guest
        rts

p4dir_end_lo: .byte 0
p4dir_end_hi: .byte 0
p4dir_len_lo: .byte 0
p4dir_len_hi: .byte 0


; Zero page pointer for directory parsing
DIR_PTR         = $FB           ; 2 bytes (shared with GC_PTR when not printing)

; ============================================================
; P4HOOK_PrintDirectory - Parse and print CBM directory format
; 
; CBM directory format in memory:
;   First line: load address (2 bytes), link (2 bytes), line# (2 bytes), 
;               then reversed disk name, etc.
;   Each entry: link (2 bytes), line# (2 bytes = blocks), 
;               filename in quotes, type
;   Last line: "BLOCKS FREE"
; ============================================================
P4HOOK_PrintDirectory:
        ; Set up pointer to start of directory data
        lda #<P4_DIR_BUF
        sta DIR_PTR
        lda #>P4_DIR_BUF
        sta DIR_PTR+1
        
        ; Skip load address (2 bytes)
        jsr _dir_skip2
        
_dir_next_line:
        ; Check if we've reached end of data
        lda DIR_PTR+1
        cmp p4dir_end_hi
        bcc _dir_process_line
        bne _dir_done
        lda DIR_PTR
        cmp p4dir_end_lo
        bcs _dir_done
        
_dir_process_line:
        ; Read link pointer (2 bytes) - if zero, end of listing
        jsr _dir_read_byte
        sta _dir_link_lo
        jsr _dir_read_byte
        sta _dir_link_hi
        
        ; Check for end (link = 0)
        lda _dir_link_lo
        ora _dir_link_hi
        beq _dir_done
        
        ; Read line number (block count) - 2 bytes little-endian
        jsr _dir_read_byte
        sta _dir_blocks_lo
        jsr _dir_read_byte
        sta _dir_blocks_hi
        
        ; Print block count (as decimal number)
        jsr _dir_print_blocks
        
        ; Print space
        lda #' '
        jsr CHROUT
        
        ; Print rest of line until null
_dir_print_chars:
        jsr _dir_read_byte
        beq _dir_line_done              ; Null = end of line
        jsr CHROUT
        jmp _dir_print_chars
        
_dir_line_done:
        ; Print newline
        lda #$0d
        jsr CHROUT
        
        ; Next line
        jmp _dir_next_line

_dir_done:
        rts

; Skip 2 bytes
_dir_skip2:
        jsr _dir_read_byte
        jsr _dir_read_byte
        rts

; Read byte from directory buffer and advance pointer
_dir_read_byte:
        ldy #0
        lda (DIR_PTR),y
        inc DIR_PTR
        bne +
        inc DIR_PTR+1
+       rts

; Print block count (16-bit number in _dir_blocks)
_dir_print_blocks:
        ; Convert to decimal and print
        ; Simple approach: print as 5-digit number with leading space suppression
        lda _dir_blocks_hi
        ldx _dir_blocks_lo
        
        ; Use simple decimal conversion
        ldy #0                          ; Leading zero flag
        
        ; 10000s place
        lda #0
        sta _dir_digit
_div_10000:
        lda _dir_blocks_lo
        sec
        sbc #<10000
        tax
        lda _dir_blocks_hi
        sbc #>10000
        bcc _print_10000
        sta _dir_blocks_hi
        stx _dir_blocks_lo
        inc _dir_digit
        jmp _div_10000
_print_10000:
        lda _dir_digit
        bne _print_10000_digit
        cpy #0
        beq _skip_10000
_print_10000_digit:
        clc
        adc #'0'
        jsr CHROUT
        ldy #1                          ; No longer leading
_skip_10000:

        ; 1000s place
        lda #0
        sta _dir_digit
_div_1000:
        lda _dir_blocks_lo
        sec
        sbc #<1000
        tax
        lda _dir_blocks_hi
        sbc #>1000
        bcc _print_1000
        sta _dir_blocks_hi
        stx _dir_blocks_lo
        inc _dir_digit
        jmp _div_1000
_print_1000:
        lda _dir_digit
        bne _print_1000_digit
        cpy #0
        beq _skip_1000
_print_1000_digit:
        clc
        adc #'0'
        jsr CHROUT
        ldy #1
_skip_1000:

        ; 100s place
        lda #0
        sta _dir_digit
_div_100:
        lda _dir_blocks_lo
        sec
        sbc #100
        bcc _print_100
        sta _dir_blocks_lo
        inc _dir_digit
        jmp _div_100
_print_100:
        lda _dir_digit
        bne _print_100_digit
        cpy #0
        beq _skip_100
_print_100_digit:
        clc
        adc #'0'
        jsr CHROUT
        ldy #1
_skip_100:

        ; 10s place
        lda #0
        sta _dir_digit
_div_10:
        lda _dir_blocks_lo
        sec
        sbc #10
        bcc _print_10
        sta _dir_blocks_lo
        inc _dir_digit
        jmp _div_10
_print_10:
        lda _dir_digit
        bne _print_10_digit
        cpy #0
        beq _skip_10
_print_10_digit:
        clc
        adc #'0'
        jsr CHROUT
_skip_10:

        ; 1s place - always print
        lda _dir_blocks_lo
        clc
        adc #'0'
        jsr CHROUT
        
        rts

_dir_link_lo:   .byte 0
_dir_link_hi:   .byte 0
_dir_blocks_lo: .byte 0
_dir_blocks_hi: .byte 0
_dir_digit:     .byte 0


; ============================================================
; DIRECTORY support (preserved from original - may not be needed)
; ============================================================

P4HOOK_OnDIRECTORY:
        jsr P4HOOK_SaveBasicPtrs
        jsr P4HOOK_ComputeTempDest

        lda #$00
        ldx #$00
        jsr SETBNK

        ldx #<p4_dir_name
        ldy #>p4_dir_name
        lda #$01
        jsr SETNAM

        lda #$01
        ldx #$08
        ldy #$00
        jsr SETLFS

        lda #$40
        ldx #<P4_DIR_BUF
        ldy #>P4_DIR_BUF
        jsr LOAD
        bcc _dir_ok
        jsr P4HOOK_RTS_Guest
        rts

_dir_ok:
        stx _d_end_lo
        sty _d_end_hi

        lda _d_end_lo
        sec
        sbc #<P4_DIR_BUF
        sta p4_dir_len_lo
        lda _d_end_hi
        sbc #>P4_DIR_BUF
        sta p4_dir_len_hi

        lda p4_dir_temp_lo
        sta p4_dir_dest_lo
        lda p4_dir_temp_hi
        sta p4_dir_dest_hi

        lda p4_dir_len_lo
        ora p4_dir_len_hi
        beq _dir_call_list
        jsr P4HOOK_DMACopyDirToGuest

_dir_call_list:
        jsr P4HOOK_PointBasicToTemp

        lda #<(P4HOOK_DIR_RESTORE_TRAP-1)
        ldx #>(P4HOOK_DIR_RESTORE_TRAP-1)
        jsr P4HOOK_PushJSRReturn_AX

        lda #<P4HOOK_ROM_PERFORM_LIST
        sta p4_pc_lo
        lda #>P4HOOK_ROM_PERFORM_LIST
        sta p4_pc_hi
        rts

_d_end_lo: .byte 0
_d_end_hi: .byte 0

P4HOOK_OnDIRECTORY_Restore:
        jsr P4HOOK_RestoreBasicPtrs
        jsr P4HOOK_RTS_Guest
        rts

P4HOOK_SaveBasicPtrs:
        lda #$00
        sta p4_addr_hi
        ldx #$00
_save_loop:
        txa
        clc
        adc #ZP_PTRS_BASE
        sta p4_addr_lo
        jsr P4MEM_Read
        sta p4_saved_basic_ptrs,x
        inx
        cpx #ZP_PTRS_COUNT
        bne _save_loop
        rts

P4HOOK_RestoreBasicPtrs:
        lda #$00
        sta p4_addr_hi
        ldx #$00
_restore_loop:
        txa
        clc
        adc #ZP_PTRS_BASE
        sta p4_addr_lo
        lda p4_saved_basic_ptrs,x
        jsr P4MEM_WriteA
        inx
        cpx #ZP_PTRS_COUNT
        bne _restore_loop
        rts

P4HOOK_ComputeTempDest:
        lda #$00
        sta p4_addr_hi
        lda #ZP_TOPMEM_LO
        sta p4_addr_lo
        jsr P4MEM_Read
        sta _top_lo
        lda #ZP_TOPMEM_HI
        sta p4_addr_lo
        jsr P4MEM_Read
        sta _top_hi

        lda _top_lo
        sec
        sbc #$00
        sta p4_dir_temp_lo
        lda _top_hi
        sbc #$10
        sta p4_dir_temp_hi

        lda #$00
        sta p4_dir_temp_lo
        rts

_top_lo: .byte 0
_top_hi: .byte 0

P4HOOK_PointBasicToTemp:
        lda #$00
        sta p4_addr_hi

        lda #ZP_TXTTAB_LO
        sta p4_addr_lo
        lda p4_dir_temp_lo
        jsr P4MEM_WriteA

        lda #ZP_TXTTAB_HI
        sta p4_addr_lo
        lda p4_dir_temp_hi
        jsr P4MEM_WriteA

        clc
        lda p4_dir_temp_lo
        adc p4_dir_len_lo
        sta _vt_lo
        lda p4_dir_temp_hi
        adc p4_dir_len_hi
        sta _vt_hi

        ldx #$00
_set_vt_loop:
        txa
        clc
        adc #ZP_VARTAB_LO
        sta p4_addr_lo

        txa
        and #$01
        beq _vt_write_lo
_vt_write_hi:
        lda _vt_hi
        jsr P4MEM_WriteA
        inx
        cpx #$08
        bne _set_vt_loop
        rts
_vt_write_lo:
        lda _vt_lo
        jsr P4MEM_WriteA
        inx
        cpx #$08
        bne _set_vt_loop
        rts

_vt_lo: .byte 0
_vt_hi: .byte 0

P4HOOK_PushJSRReturn_AX:
        sta tmp_lo
        stx tmp_hi

        ldy p4_sp

        lda #$01
        sta p4_addr_hi
        tya
        sta p4_addr_lo
        lda tmp_hi
        jsr P4MEM_WriteA
        dey

        lda #$01
        sta p4_addr_hi
        tya
        sta p4_addr_lo
        lda tmp_lo
        jsr P4MEM_WriteA
        dey

        sty p4_sp
        rts

P4HOOK_RTS_Guest:
        ldy p4_sp
        iny
        sty p4_sp
        lda #$01
        sta p4_addr_hi
        tya
        sta p4_addr_lo
        jsr P4MEM_Read
        sta tmp_lo

        ldy p4_sp
        iny
        sty p4_sp
        lda #$01
        sta p4_addr_hi
        tya
        sta p4_addr_lo
        jsr P4MEM_Read
        sta tmp_hi

        clc
        lda tmp_lo
        adc #$01
        sta p4_pc_lo
        lda tmp_hi
        adc #$00
        sta p4_pc_hi
        rts