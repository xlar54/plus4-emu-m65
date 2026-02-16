; ============================================================
; p4monitor.asm - Built-in monitor/debugger for Plus/4 emulator
; Host: MEGA65
; Assembler: 64tass (45GS02)
;
; Activated by pressing the TAB key
; Provides memory viewing, disassembly, and CPU state display
; ============================================================

        .cpu "45gs02"

; ------------------------------------------------------------
; External references
; ------------------------------------------------------------
        .weak
        ; From plus4_cpu_m65.asm - MUST MATCH ACTUAL ADDRESSES
        p4_a     = $02
        p4_x     = $03
        p4_y     = $04
        p4_sp    = $05
        p4_p     = $06
        p4_pc_lo = $07
        p4_pc_hi = $08
        
        ; From p4mem_m65.asm
        ; ted_regs is defined in plus4_cpu_m65.asm - don't override!
        p4_rom_visible = $0500
        LOW_RAM_BUFFER = $A000
        BANK_RAM = $05
        BANK_ROM = $04
        
        ; MEGA65 KERNAL
        CHROUT = $FFD2
        GETIN = $FFE4
        .endweak

; ------------------------------------------------------------
; Zero page variables for indirect addressing
; ------------------------------------------------------------
mon_scrptr      = $B0           ; Screen pointer (2 bytes)
mon_addr        = $B2           ; Memory address being viewed (2 bytes)
mon_ptr         = $B4           ; General pointer (4 bytes for 32-bit)

; ------------------------------------------------------------
; Monitor state variables (not in ZP)
; ------------------------------------------------------------
mon_active:     .byte 0         ; 1 = monitor is active
mon_view_lo:    .byte 0         ; Current view address low
mon_view_hi:    .byte 0         ; Current view address high
mon_cursor_x:   .byte 0         ; Cursor X position
mon_cursor_y:   .byte 0         ; Cursor Y position
mon_mode:       .byte 0         ; 0=hex dump, 1=disassembly, 2=TED regs
mon_view_rom:   .byte 1         ; 0=view RAM only, 1=view ROM (what CPU sees)
mon_input_buf:  .fill 8, 0      ; Input buffer for addresses
mon_input_len:  .byte 0         ; Input buffer length
mon_saved_border: .byte 0       ; Saved border color
mon_saved_bg:   .byte 0         ; Saved background color
mon_line_cnt:   .byte 0         ; Line counter for drawing

; Saved VIC-IV state
mon_saved_scrnptr_lo:  .byte 0
mon_saved_scrnptr_mid: .byte 0
mon_saved_scrnptr_hi:  .byte 0
mon_saved_d063:        .byte 0          ; LINESTEP
mon_saved_charptr_lo:  .byte 0
mon_saved_charptr_mid: .byte 0
mon_saved_charptr_hi:  .byte 0
mon_saved_d054:        .byte 0
mon_saved_d031:        .byte 0
mon_saved_d016:        .byte 0
mon_saved_d018:        .byte 0
mon_saved_d011:        .byte 0
mon_saved_d058:        .byte 0          ; Logical row width low
mon_saved_d059:        .byte 0          ; Logical row width high
mon_saved_d05d:        .byte 0          ; Hot register control

; Screen layout constants
MON_SCREEN      = $0400         ; Monitor screen RAM
MON_COLOR       = $D800         ; MEGA65 color RAM (directly addressable mirror)
MON_COLS        = 40
MON_ROWS        = 25

; Colors
MON_BG_COLOR    = 0             ; Black
MON_FG_COLOR    = 5             ; Green
MON_BORDER_COLOR = 0            ; Black
MON_HIGHLIGHT   = 1             ; White

; ------------------------------------------------------------
; P4MON_Check - Check if ALT key pressed, activate monitor
; Call this from main loop
; Returns: C=1 if monitor was active and handled input
; ------------------------------------------------------------
P4MON_Check:
        ; If monitor already active, handle input
        lda mon_active
        bne _mon_handle
        
        ; Check real-time modifier key state at $D611
        ; Bit 3 = ALT key (directly from keyboard matrix, not queued)
        lda $D611
        and #$08                ; Check bit 3 (ALT key)
        beq _mon_not_active     ; ALT not pressed
        
        ; ALT pressed - but wait for release to avoid repeat
        lda mon_alt_was_pressed
        bne _mon_not_active     ; Already handled this press
        
        ; First time seeing ALT pressed
        lda #1
        sta mon_alt_was_pressed
        sta mon_active
        
        jsr P4MON_Activate
        sec
        rts
        
_mon_handle:
        ; Monitor is active, handle input
        jsr P4MON_HandleInput
        
        ; Check if we should stay active
        lda mon_active
        beq _mon_just_deactivated
        sec
        rts
        
_mon_just_deactivated:
        clc
        rts
        
_mon_not_active:
        ; Check if ALT was released
        lda $D611
        and #$08
        bne +
        ; ALT released - clear flag
        lda #0
        sta mon_alt_was_pressed
+       clc
        rts

mon_alt_was_pressed: .byte 0

; ------------------------------------------------------------
; P4MON_Activate - Enter monitor mode
; ------------------------------------------------------------
P4MON_Activate:
        ; Unlock VIC-III/IV registers first
        lda #$A5
        sta $D02F
        lda #$96
        sta $D02F
        lda #$47
        sta $D02F
        lda #$53
        sta $D02F
        
        ; Disable hot registers so we can read current state
        lda #$80
        trb $D05D
        
        ; Save current colors
        lda $D020
        sta mon_saved_border
        lda $D021
        sta mon_saved_bg
        
        ; Save VIC-IV pointers and mode registers
        lda $D060
        sta mon_saved_scrnptr_lo
        lda $D061
        sta mon_saved_scrnptr_mid
        lda $D062
        sta mon_saved_scrnptr_hi
        lda $D063
        sta mon_saved_d063
        lda $D068
        sta mon_saved_charptr_lo
        lda $D069
        sta mon_saved_charptr_mid
        lda $D06A
        sta mon_saved_charptr_hi
        lda $D054
        sta mon_saved_d054
        lda $D031
        sta mon_saved_d031
        lda $D016
        sta mon_saved_d016
        lda $D018
        sta mon_saved_d018
        lda $D011
        sta mon_saved_d011
        lda $D058
        sta mon_saved_d058
        lda $D059
        sta mon_saved_d059
        lda $D05D
        sta mon_saved_d05d
        
        ; Save screen RAM from actual location to attic RAM at $08010000
        ; First, build DMA list with actual screen address
        lda mon_saved_scrnptr_lo
        sta dma_save_screen_src
        lda mon_saved_scrnptr_mid
        sta dma_save_screen_src+1
        lda mon_saved_scrnptr_hi
        sta dma_save_screen_bank
        
        ; Using DMA for screen
        lda #$00
        sta $D702               ; DMA bank for list
        sta $D704               ; DMA list bank
        lda #>dma_save_screen
        sta $D701
        lda #<dma_save_screen
        sta $D705               ; Trigger DMA
        
        ; Save color RAM using DMA from $1F800 (bank 1, addr $F800)
        lda #$00
        sta $D702
        sta $D704
        lda #>dma_save_color
        sta $D701
        lda #<dma_save_color
        sta $D705               ; Trigger DMA
        
        ; Now switch to standard text mode for monitor
        ; 
        ; Strategy: Re-enable HOTREG temporarily so that writing $D011
        ; triggers a full recalculation of ALL VIC-IV display parameters
        ; (CHRYSCL, borders, scaling, char height, etc.) for standard
        ; C64 text mode. Then disable HOTREG and set our VIC-IV pointers.
        ; This avoids having to manually set every vernier register.
        
        lda $D054
        and #%11111100          ; Clear CHR16 and FCM bits
        sta $D054
        
        ; Enable HOTREG so D011 write recalculates everything
        lda #$80
        tsb $D05D               ; Set bit 7 = enable hot registers
        
        ; Write D011 - triggers full C64-mode display recalculation
        ; This sets CHRYSCL, borders, scaling, etc. correctly
        lda #$1B                ; Screen on, 25 rows, no bitmap
        sta $D011
        
        ; Also write D018 to set correct C64 screen/charset mapping
        ; Screen at $0400, charset at ROM default
        lda #$14                ; Screen $0400, charset $1000 (C64 default)
        sta $D018
        
        ; Now disable HOTREG before we set VIC-IV pointers
        lda #$80
        trb $D05D               ; Clear HOTREG - pointers will stick
        
        ; Set monitor colors
        lda #MON_BORDER_COLOR
        sta $D020
        lda #MON_BG_COLOR
        sta $D021
        
        ; Point screen to $0400 (bank 0)
        lda #$00
        sta $D060               ; SCRNPTR low
        lda #$04
        sta $D061               ; SCRNPTR mid = $04 for $0400
        lda #$00
        sta $D062               ; SCRNPTR bank = 0
        sta $D063               ; SCRNPTR megabyte = 0 (NOT linestep!)
        
        ; Point to standard C64 charset at $2D000 (MEGA65 ROM charset)
        lda #$00
        sta $D068
        lda #$D0
        sta $D069
        lda #$02
        sta $D06A
        
        ; Make sure we're in 40-column mode
        lda #40
        sta $D058               ; LINESTEP low (bytes per row)
        lda #0
        sta $D059               ; LINESTEP high
        
        ; Initialize view address to current PC
        lda p4_pc_lo
        sta mon_view_lo
        lda p4_pc_hi
        sta mon_view_hi
        
        ; Clear input buffer
        lda #0
        sta mon_input_len
        sta mon_cursor_x
        sta mon_cursor_y
        sta mon_mode            ; Start in hex dump mode
        
        ; Clear screen and draw initial display
        jsr P4MON_ClearScreen
        jsr P4MON_DrawHeader
        jsr P4MON_DrawContent
        jsr P4MON_DrawHelp
        
        rts

; DMA list to save screen RAM to attic
dma_save_screen:
        .byte $0A               ; Request format (F018A)
        .byte $00               ; Command: copy
        .word 1000              ; Count
dma_save_screen_src:
        .word $0800             ; Source address (modified at runtime)
dma_save_screen_bank:
        .byte $00               ; Source bank (modified at runtime)
        .word $0000             ; Dest address low (attic)
        .byte $81               ; Dest bank ($08 = attic, +$80 for MB)
        .byte $00               ; Sub-command
        .word $0100             ; Modulo (not used)

; DMA list to save color RAM to attic
; Plus/4 attribute RAM is mirrored at LOW_RAM_BUFFER+$800 = $A800
dma_save_color:
        .byte $0A               ; Request format (F018A)
        .byte $00               ; Command: copy
        .word 1000              ; Count
        .word $A800             ; Source address (Plus/4 attribute RAM mirror)
        .byte $00               ; Source bank 0
        .word $0400             ; Dest address low (attic offset)
        .byte $81               ; Dest bank ($08 = attic, +$80 for MB)
        .byte $00               ; Sub-command
        .word $0100             ; Modulo (not used)

; DMA list to restore screen RAM from attic
dma_restore_screen:
        .byte $0A               ; Request format (F018A)
        .byte $00               ; Command: copy
        .word 1000              ; Count
        .word $0000             ; Source address low (attic offset)
        .byte $81               ; Source bank ($08 = attic, +$80 for MB)
dma_restore_screen_dst:
        .word $0800             ; Dest address (modified at runtime)
dma_restore_screen_bank:
        .byte $00               ; Dest bank (modified at runtime)
        .byte $00               ; Sub-command
        .word $0100             ; Modulo (not used)

; DMA list to restore color RAM from attic
; Plus/4 attribute RAM is mirrored at LOW_RAM_BUFFER+$800 = $A800
dma_restore_color:
        .byte $0A               ; Request format (F018A)
        .byte $00               ; Command: copy
        .word 1000              ; Count
        .word $0400             ; Source address low (attic offset)
        .byte $81               ; Source bank ($08 = attic, +$80 for MB)
        .word $A800             ; Dest address (Plus/4 attribute RAM mirror)
        .byte $00               ; Dest bank 0
        .byte $00               ; Sub-command
        .word $0100             ; Modulo (not used)

; DMA list to sync attribute RAM to VIC-IV color RAM
; Copy from $A800 to $1F800 so VIC-IV sees the colors
dma_sync_color:
        .byte $0A               ; Request format (F018A)
        .byte $00               ; Command: copy
        .word 1000              ; Count
        .word $A800             ; Source: Plus/4 attribute RAM
        .byte $00               ; Source bank 0
        .word $F800             ; Dest: VIC-IV color RAM at $1F800
        .byte $01               ; Dest bank 1
        .byte $00               ; Sub-command
        .word $0100             ; Modulo (not used)

; DMA list to sync to D800 (CPU-visible color RAM mirror)
dma_sync_d800:
        .byte $0A               ; Request format (F018A)
        .byte $00               ; Command: copy
        .word 1000              ; Count
        .word $A800             ; Source: Plus/4 attribute RAM
        .byte $00               ; Source bank 0
        .word $D800             ; Dest: D800
        .byte $00               ; Dest bank 0
        .byte $00               ; Sub-command
        .word $0100             ; Modulo (not used)

; ------------------------------------------------------------
; P4MON_DrawHelp - Draw help line at bottom
; ------------------------------------------------------------
P4MON_DrawHelp:
        ldx #0
dh_loop:
        lda mon_help_text,x
        beq dh_done
        sta MON_SCREEN + 24*40,x        ; Line 25 (bottom)
        lda #MON_HIGHLIGHT
        sta MON_COLOR + 24*40,x
        inx
        cpx #40
        bcc dh_loop
dh_done:
        rts

; ------------------------------------------------------------
; P4MON_Deactivate - Exit monitor mode
; ------------------------------------------------------------
P4MON_Deactivate:
        ; Unlock VIC-III/IV registers
        lda #$A5
        sta $D02F
        lda #$96
        sta $D02F
        lda #$47
        sta $D02F
        lda #$53
        sta $D02F
        
        ; Disable hot registers for restore
        lda #$80
        trb $D05D
        
        ; Set up restore DMA with actual screen destination
        lda mon_saved_scrnptr_lo
        sta dma_restore_screen_dst
        lda mon_saved_scrnptr_mid
        sta dma_restore_screen_dst+1
        lda mon_saved_scrnptr_hi
        sta dma_restore_screen_bank
        
        ; Restore screen RAM from attic
        lda #$00
        sta $D702
        sta $D704
        lda #>dma_restore_screen
        sta $D701
        lda #<dma_restore_screen
        sta $D705               ; Trigger DMA
        
        ; Restore color RAM using DMA to $A800
        lda #$00
        sta $D702
        sta $D704
        lda #>dma_restore_color
        sta $D701
        lda #<dma_restore_color
        sta $D705               ; Trigger DMA
        
        ; Also sync to VIC-IV color RAM at $1F800
        lda #$00
        sta $D702
        sta $D704
        lda #>dma_sync_color
        sta $D701
        lda #<dma_sync_color
        sta $D705               ; Trigger DMA
        
        ; CPU copy from $A800 to $D800 (DMA can't write to I/O space)
        ; Convert TED colors to C64 colors: just keep lower 4 bits
        ; (This matches what p4mem does in _do_color_mirror)
        ldy #0
sync_d800_p0:
        lda $A800,y
        and #$0F                ; TED color -> C64 color (lower 4 bits only)
        sta $D800,y
        iny
        bne sync_d800_p0
        
        ldy #0
sync_d800_p1:
        lda $A900,y
        and #$0F
        sta $D900,y
        iny
        bne sync_d800_p1
        
        ldy #0
sync_d800_p2:
        lda $AA00,y
        and #$0F
        sta $DA00,y
        iny
        bne sync_d800_p2
        
        ldy #0
sync_d800_p3:
        lda $AB00,y
        and #$0F
        sta $DB00,y
        iny
        bne sync_d800_p3
        
        ; Restore mode registers (hot registers) FIRST
        lda mon_saved_d054
        sta $D054
        lda mon_saved_d016
        sta $D016
        lda mon_saved_d018
        sta $D018
        
        ; Restore extended mode register (also hot)
        lda mon_saved_d031
        sta $D031
        
        ; Restore D011 (also hot)
        lda mon_saved_d011
        sta $D011
        
        ; CRITICAL: All hot register writes above set pending recalc flags.
        ; Clear HOTREG now to cancel pending recalculation before we set
        ; the VIC-IV pointers, otherwise they'll get stomped.
        lda #$80
        trb $D05D               ; Clear HOTREG + cancel pending recalc
        
        ; Restore screen dimensions
        lda mon_saved_d058
        sta $D058
        lda mon_saved_d059
        sta $D059
        
        ; NOW restore VIC-IV pointers (safe - no pending recalc)
        lda mon_saved_scrnptr_lo
        sta $D060
        lda mon_saved_scrnptr_mid
        sta $D061
        lda mon_saved_scrnptr_hi
        sta $D062
        lda mon_saved_d063
        sta $D063
        lda mon_saved_charptr_lo
        sta $D068
        lda mon_saved_charptr_mid
        sta $D069
        lda mon_saved_charptr_hi
        sta $D06A
        
        ; Restore hot register setting last
        ; (emulator keeps HOTREG disabled, but be safe)
        lda mon_saved_d05d
        sta $D05D
        
        ; Restore colors last
        lda mon_saved_border
        sta $D020
        lda mon_saved_bg
        sta $D021
        
        rts

; ------------------------------------------------------------
; P4MON_ClearScreen - Clear the MEGA65 screen
; ------------------------------------------------------------
P4MON_ClearScreen:
        ; Clear screen RAM at $0400 (1000 bytes = 25*40)
        lda #<MON_SCREEN
        sta mon_ptr
        lda #>MON_SCREEN
        sta mon_ptr+1
        
        ldy #0
        ldx #4                  ; 4 pages = 1024 bytes (close enough to 1000)
        lda #$20                ; Space character
cs_loop:
        sta (mon_ptr),y
        iny
        bne cs_loop
        inc mon_ptr+1
        dex
        bne cs_loop
        
        ; Set color RAM at $D800 to green
        lda #<MON_COLOR
        sta mon_ptr
        lda #>MON_COLOR
        sta mon_ptr+1
        
        ldy #0
        ldx #4
        lda #MON_FG_COLOR
cc_loop:
        sta (mon_ptr),y
        iny
        bne cc_loop
        inc mon_ptr+1
        dex
        bne cc_loop
        
        rts

; ------------------------------------------------------------
; P4MON_DrawHeader - Draw the header line
; ------------------------------------------------------------
P4MON_DrawHeader:
        ; Print line 1: "PLUS/4 MONITOR PC:xxxx A:xx X:xx Y:xx"
        ldx #0
ph_hdr_loop:
        lda mon_header_text,x
        beq ph_hdr_line2
        sta MON_SCREEN,x
        lda #MON_HIGHLIGHT
        sta MON_COLOR,x
        inx
        cpx #40
        bcc ph_hdr_loop

ph_hdr_line2:
        ; Print line 2: "SP:xx P:xx NV-BDIZC"
        ldx #0
ph_hdr2_loop:
        lda mon_line2_text,x
        beq ph_hdr_regs
        sta MON_SCREEN+40,x     ; Line 2 starts at offset 40
        lda #MON_FG_COLOR
        sta MON_COLOR+40,x
        inx
        cpx #40
        bcc ph_hdr2_loop
        
ph_hdr_regs:
        ; Draw PC value at position 18
        lda p4_pc_hi
        jsr mon_byte_to_hex
        sta MON_SCREEN+18
        stx MON_SCREEN+19
        lda p4_pc_lo
        jsr mon_byte_to_hex
        sta MON_SCREEN+20
        stx MON_SCREEN+21
        
        ; Draw A at position 25
        lda p4_a
        jsr mon_byte_to_hex
        sta MON_SCREEN+25
        stx MON_SCREEN+26
        
        ; Draw X at position 30
        lda p4_x
        jsr mon_byte_to_hex
        sta MON_SCREEN+30
        stx MON_SCREEN+31
        
        ; Draw Y at position 35
        lda p4_y
        jsr mon_byte_to_hex
        sta MON_SCREEN+35
        stx MON_SCREEN+36
        
        ; Second line: SP at position 43 (line 2 pos 3)
        lda p4_sp
        jsr mon_byte_to_hex
        sta MON_SCREEN+43
        stx MON_SCREEN+44
        
        ; P register at position 48 (line 2 pos 8)
        lda p4_p
        jsr mon_byte_to_hex
        sta MON_SCREEN+48
        stx MON_SCREEN+49
        
        ; Draw flags as letters: NV-BDIZC at position 51
        ldx #0
        lda p4_p
ph_flag_loop:
        asl                     ; Shift flag into carry
        pha
        lda mon_flag_letters,x
        bcc ph_flag_clear
        ; Flag is set - show letter
        sta MON_SCREEN+51,x
        bra ph_flag_next
ph_flag_clear:
        ; Flag is clear - show dash
        lda #$2D                ; '-' screen code
        sta MON_SCREEN+51,x
ph_flag_next:
        pla
        inx
        cpx #8
        bcc ph_flag_loop
        
        ; Show ROM/RAM indicator at end of line 2
        lda mon_view_rom
        beq ph_show_ram
        ; Show "ROM"
        lda #$12                ; 'r' screen code
        sta MON_SCREEN+60
        lda #$0F                ; 'o' screen code
        sta MON_SCREEN+61
        lda #$0D                ; 'm' screen code
        sta MON_SCREEN+62
        bra ph_hdr_done
ph_show_ram:
        ; Show "RAM"
        lda #$12                ; 'r' screen code
        sta MON_SCREEN+60
        lda #$01                ; 'a' screen code
        sta MON_SCREEN+61
        lda #$0D                ; 'm' screen code
        sta MON_SCREEN+62
ph_hdr_done:
        rts

; Use screen code encoding for text
        .enc "screen"

mon_header_text:
        .text "plus/4 monitor pc:.... a:.. x:.. y:.."
        .byte 0
        
mon_line2_text:
        .text "sp:.. p:.. ........     "
        .byte 0

mon_flag_letters:
        .text "nv-bdizc"
        
        .enc "none"

; ------------------------------------------------------------
; P4MON_DrawContent - Draw memory/disassembly content
; ------------------------------------------------------------
P4MON_DrawContent:
        lda mon_mode
        cmp #1
        beq _draw_disasm
        cmp #2
        beq _draw_ted
        
        ; Mode 0: Hex dump
        jsr P4MON_DrawHexDump
        rts
        
_draw_disasm:
        jsr P4MON_DrawDisasm
        rts
        
_draw_ted:
        jsr P4MON_DrawTED
        rts

; ------------------------------------------------------------
; P4MON_DrawHexDump - Draw hex memory dump
; ------------------------------------------------------------
P4MON_DrawHexDump:
        ; Initialize
        lda #0
        sta mon_line_cnt
        
        ; Copy view address to working pointer
        lda mon_view_lo
        sta mon_addr
        lda mon_view_hi
        sta mon_addr+1
        
        ; Start at screen line 3 = offset 120
        lda #<(MON_SCREEN + 120)
        sta hd_smc_addr+1
        lda #>(MON_SCREEN + 120)
        sta hd_smc_addr+2
        
hd_line_loop:
        ; Reset position within line
        lda #0
        sta hd_line_pos
        
        ; Draw address high byte
        lda mon_addr+1
        jsr mon_byte_to_hex     ; A=hi, X=lo
        jsr hd_write_char       ; Write A to screen
        txa
        jsr hd_write_char       ; Write X to screen
        
        ; Draw address low byte  
        lda mon_addr
        jsr mon_byte_to_hex
        jsr hd_write_char
        txa
        jsr hd_write_char
        
        ; Colon
        lda #$3A
        jsr hd_write_char
        
        ; Space
        lda #$20
        jsr hd_write_char
        
        ; Draw 8 bytes
        ldx #0
hd_byte_loop:
        stx hd_byte_cnt
        
        ; Read byte from Plus/4 memory
        jsr P4MON_ReadByte
        sta hd_temp_byte
        
        ; Convert to hex
        lda hd_temp_byte
        jsr mon_byte_to_hex     ; A=hi, X=lo
        jsr hd_write_char
        txa
        jsr hd_write_char
        
        ; Space
        lda #$20
        jsr hd_write_char
        
        ; Increment mon_addr for next byte
        inc mon_addr
        bne +
        inc mon_addr+1
+
        ; Next byte
        ldx hd_byte_cnt
        inx
        cpx #8
        bcc hd_byte_loop
        
        ; Move to next line - add 40 to screen address
        clc
        lda hd_smc_addr+1
        adc #40
        sta hd_smc_addr+1
        lda hd_smc_addr+2
        adc #0
        sta hd_smc_addr+2
        
        ; Next line
        inc mon_line_cnt
        lda mon_line_cnt
        cmp #16
        bcc hd_line_loop
        
        rts

; Write character in A to screen at current position, advance position
hd_write_char:
        stx hd_save_x           ; Save X
        ldx hd_line_pos
hd_smc_addr:
        sta $0800,x             ; Self-modified address
        inc hd_line_pos
        ldx hd_save_x           ; Restore X
        rts

hd_save_x:      .byte 0
hd_line_pos:    .byte 0
hd_byte_cnt:    .byte 0
hd_temp_byte:   .byte 0

; ------------------------------------------------------------
; P4MON_DrawDisasm - Draw disassembly view
; ------------------------------------------------------------
P4MON_DrawDisasm:
        ldx #0
dd_msg_loop:
        lda mon_disasm_msg,x
        beq dd_done
        sta MON_SCREEN+120,x
        inx
        bne dd_msg_loop
dd_done:
        rts

        .enc "screen"
mon_disasm_msg:
        .text "disassembly mode - press d for dump"
        .byte 0
        .enc "none"

; ------------------------------------------------------------
; P4MON_DrawTED - Draw TED register view
; ------------------------------------------------------------
P4MON_DrawTED:
        ; First clear the content area (lines 3-18)
        jsr dt_clear_area
        
        ; Start at screen line 3
        lda #<(MON_SCREEN + 120)
        sta dt_smc_addr+1
        lda #>(MON_SCREEN + 120)
        sta dt_smc_addr+2
        
        ldx #0                  ; Register index (0-31)
dt_loop:
        stx dt_reg_num
        
        ; Calculate column position: even regs at col 0, odd at col 20
        lda dt_reg_num
        and #$01
        beq dt_col0
        lda #20                 ; Column 20 for odd registers
        bra dt_set_pos
dt_col0:
        lda #0                  ; Column 0 for even registers
dt_set_pos:
        sta dt_line_pos
        
        ; Draw "$ff"
        lda #$24                ; '$' screen code
        jsr dt_write_char
        lda #$06                ; 'f' screen code
        jsr dt_write_char
        lda #$06                ; 'f' screen code
        jsr dt_write_char
        
        ; Draw register number (2 hex digits)
        lda dt_reg_num
        jsr mon_byte_to_hex
        jsr dt_write_char
        txa
        jsr dt_write_char
        
        ; Draw ":"
        lda #$3A
        jsr dt_write_char
        
        ; Draw register value
        ldx dt_reg_num
        lda ted_regs,x
        jsr mon_byte_to_hex
        jsr dt_write_char
        txa
        jsr dt_write_char
        
        ; Check if we just did odd register (need new line)
        lda dt_reg_num
        and #$01
        beq dt_next             ; Even - don't move to next line yet
        
        ; Move to next line after odd register
        clc
        lda dt_smc_addr+1
        adc #40
        sta dt_smc_addr+1
        lda dt_smc_addr+2
        adc #0
        sta dt_smc_addr+2
        
dt_next:
        ldx dt_reg_num
        inx
        cpx #32
        bcc dt_loop
        rts

; Clear content area (lines 3-18)
dt_clear_area:
        lda #<(MON_SCREEN + 120)
        sta dt_clr_ptr+1
        lda #>(MON_SCREEN + 120)
        sta dt_clr_ptr+2
        
        ldx #0                  ; Line counter
dt_clr_line:
        ldy #0
dt_clr_loop:
        lda #$20                ; Space
dt_clr_ptr:
        sta $0800,y
        iny
        cpy #40
        bcc dt_clr_loop
        
        ; Next line
        clc
        lda dt_clr_ptr+1
        adc #40
        sta dt_clr_ptr+1
        lda dt_clr_ptr+2
        adc #0
        sta dt_clr_ptr+2
        
        inx
        cpx #16                 ; 16 lines
        bcc dt_clr_line
        rts

; Write character in A to TED display
dt_write_char:
        stx dt_save_x
        ldx dt_line_pos
dt_smc_addr:
        sta $0800,x
        inc dt_line_pos
        ldx dt_save_x
        rts

dt_reg_num:     .byte 0
dt_line_pos:    .byte 0
dt_save_x:      .byte 0

; ------------------------------------------------------------
; P4MON_ReadByte - Read byte from Plus/4 memory at mon_addr
; ------------------------------------------------------------
P4MON_ReadByte:
        ; Check if viewing ROM and address is in ROM area ($8000+)
        lda mon_view_rom
        beq rb_read_ram         ; ROM view disabled, read RAM
        
        lda mon_addr+1
        cmp #$80                ; Is address >= $8000?
        bcc rb_read_ram         ; No, read from RAM
        
        ; Check if ROM is actually visible (p4_rom_visible)
        lda p4_rom_visible
        beq rb_read_ram         ; ROM not visible, read RAM
        
        ; Read from ROM (bank 4)
        lda mon_addr
        sta mon_ptr
        lda mon_addr+1
        sta mon_ptr+1
        lda #$04                ; Bank 4 = Plus/4 ROM
        sta mon_ptr+2
        lda #0
        sta mon_ptr+3
        ldz #0
        lda [mon_ptr],z
        rts
        
rb_read_ram:
        ; Read from RAM
        lda mon_addr+1
        cmp #$10
        bcs rb_bank5
        
        ; Read from LOW_RAM_BUFFER ($0000-$0FFF)
        clc
        lda mon_addr
        adc #<LOW_RAM_BUFFER
        sta mon_ptr
        lda mon_addr+1
        adc #>LOW_RAM_BUFFER
        sta mon_ptr+1
        ldy #0
        lda (mon_ptr),y
        rts
        
rb_bank5:
        ; Read from bank 5 ($1000+)
        lda mon_addr
        sta mon_ptr
        lda mon_addr+1
        sta mon_ptr+1
        lda #BANK_RAM
        sta mon_ptr+2
        lda #0
        sta mon_ptr+3
        ldz #0
        lda [mon_ptr],z
        rts

; ------------------------------------------------------------
; P4MON_HandleInput - Handle keyboard input in monitor
; ------------------------------------------------------------
P4MON_HandleInput:
        ; Read key from MEGA65 keyboard
        jsr GETIN
        beq hi_done             ; No key
        
        ; Check for special keys
        cmp #$91                ; Cursor up
        beq hi_up
        cmp #$11                ; Cursor down  
        beq hi_down
        cmp #$9D                ; Cursor left
        beq hi_left
        cmp #$1D                ; Cursor right
        beq hi_right
        cmp #'D'
        beq hi_dump_mode
        cmp #'d'
        beq hi_dump_mode
        cmp #'T'
        beq hi_ted_mode
        cmp #'t'
        beq hi_ted_mode
        cmp #'A'
        beq hi_disasm_mode
        cmp #'a'
        beq hi_disasm_mode
        cmp #'G'
        beq hi_goto
        cmp #'g'
        beq hi_goto
        cmp #'P'
        beq hi_goto_pc
        cmp #'p'
        beq hi_goto_pc
        cmp #'R'
        beq hi_toggle_rom
        cmp #'r'
        beq hi_toggle_rom
        cmp #'X'
        beq hi_reset
        cmp #'x'
        beq hi_reset
        cmp #$0D                ; Return - exit monitor
        beq hi_exit
        
hi_done:
        rts

hi_reset:
        ; System reset - set PC to Plus/4 reset vector and exit
        lda #$FC
        sta mon_ptr
        lda #$FF
        sta mon_ptr+1
        lda #BANK_ROM
        sta mon_ptr+2
        lda #0
        sta mon_ptr+3
        ldz #0
        lda [mon_ptr],z         ; Low byte of reset vector
        sta p4_pc_lo
        inz
        lda [mon_ptr],z         ; High byte of reset vector
        sta p4_pc_hi
        
        ; Exit monitor normally
        lda #0
        sta mon_active
        jsr P4MON_Deactivate
        rts

hi_toggle_rom:
        lda mon_view_rom
        eor #$01
        sta mon_view_rom
        jsr P4MON_DrawHeader
        jsr P4MON_DrawContent
        rts

hi_up:
        sec
        lda mon_view_lo
        sbc #$08
        sta mon_view_lo
        lda mon_view_hi
        sbc #0
        sta mon_view_hi
        jsr P4MON_DrawContent
        rts

hi_down:
        clc
        lda mon_view_lo
        adc #$08
        sta mon_view_lo
        lda mon_view_hi
        adc #0
        sta mon_view_hi
        jsr P4MON_DrawContent
        rts

hi_left:
        sec
        lda mon_view_lo
        sbc #$80
        sta mon_view_lo
        lda mon_view_hi
        sbc #0
        sta mon_view_hi
        jsr P4MON_DrawContent
        rts

hi_right:
        clc
        lda mon_view_lo
        adc #$80
        sta mon_view_lo
        lda mon_view_hi
        adc #0
        sta mon_view_hi
        jsr P4MON_DrawContent
        rts

hi_dump_mode:
        lda #0
        sta mon_mode
        jsr P4MON_DrawContent
        rts

hi_ted_mode:
        lda #2
        sta mon_mode
        jsr P4MON_DrawContent
        rts

hi_disasm_mode:
        lda #1
        sta mon_mode
        jsr P4MON_DrawContent
        rts

hi_goto:
        jsr P4MON_GetAddress
        bcs hi_goto_cancelled
        lda goto_addr_lo
        sta mon_view_lo
        lda goto_addr_hi
        sta mon_view_hi
        lda #0
        sta mon_mode
        jsr P4MON_DrawContent
hi_goto_cancelled:
        jsr P4MON_ClearGotoLine
        rts

hi_goto_pc:
        lda p4_pc_lo
        sta mon_view_lo
        lda p4_pc_hi
        sta mon_view_hi
        jsr P4MON_DrawContent
        rts

hi_exit:
        lda #0
        sta mon_active
        jsr P4MON_Deactivate
        rts

; ------------------------------------------------------------
; P4MON_GetAddress - Get 4-digit hex address from user
; ------------------------------------------------------------
GOTO_LINE = MON_SCREEN + (22 * 40)

P4MON_ClearGotoLine:
        ldx #39
        lda #$20
cgl_loop:
        sta GOTO_LINE,x
        dex
        bpl cgl_loop
        rts

P4MON_GetAddress:
        jsr P4MON_ClearGotoLine
        
        ldx #0
ga_prompt:
        lda ga_prompt_text,x
        beq ga_prompt_done
        sta GOTO_LINE,x
        inx
        cpx #6
        bcc ga_prompt
ga_prompt_done:
        
        lda #0
        sta goto_input_len
        sta goto_addr_lo
        sta goto_addr_hi
        
        lda #$A0
        sta GOTO_LINE+6
        
ga_input_loop:
        jsr GETIN
        beq ga_input_loop
        
        cmp #$0D
        beq ga_accept
        cmp #$1B
        beq ga_cancel
        cmp #$9D
        beq ga_backspace
        cmp #$14
        beq ga_backspace
        
        jsr ga_char_to_hex
        bcs ga_input_loop
        
        ldx goto_input_len
        cpx #4
        bcs ga_input_loop
        
        sta goto_nibble
        
        asl goto_addr_lo
        rol goto_addr_hi
        asl goto_addr_lo
        rol goto_addr_hi
        asl goto_addr_lo
        rol goto_addr_hi
        asl goto_addr_lo
        rol goto_addr_hi
        lda goto_nibble
        ora goto_addr_lo
        sta goto_addr_lo
        
        lda goto_input_len
        clc
        adc #6
        tax
        ldy goto_nibble
        lda mon_hex_chars,y
        sta GOTO_LINE,x
        
        inc goto_input_len
        lda goto_input_len
        clc
        adc #6
        tax
        cpx #10
        bcs ga_input_no_cursor
        lda #$A0
        sta GOTO_LINE,x
ga_input_no_cursor:
        jmp ga_input_loop
        
ga_backspace:
        lda goto_input_len
        beq ga_input_loop
        
        lda goto_input_len
        clc
        adc #6
        tax
        lda #$20
        sta GOTO_LINE,x
        
        dec goto_input_len
        
        lsr goto_addr_hi
        ror goto_addr_lo
        lsr goto_addr_hi
        ror goto_addr_lo
        lsr goto_addr_hi
        ror goto_addr_lo
        lsr goto_addr_hi
        ror goto_addr_lo
        
        lda goto_input_len
        clc
        adc #6
        tax
        lda #$A0
        sta GOTO_LINE,x
        jmp ga_input_loop
        
ga_accept:
        lda goto_input_len
        beq ga_cancel
        clc
        rts
        
ga_cancel:
        sec
        rts

ga_char_to_hex:
        ; Digits 0-9 ($30-$39) — same in all encodings
        cmp #$30
        bcc ga_not_hex
        cmp #$3A
        bcc ga_is_digit
        ; Try all known encodings for A-F:
        ; Uppercase PETSCII: $41-$46
        ; Lowercase PETSCII: $C1-$C6
        ; Screen codes lowercase: $01-$06
        ; ASCII lowercase: $61-$66
        ; Normalize: mask with $1F to get letter position (A=1..F=6)
        pha
        and #$1F
        cmp #$01
        bcc _ga_not_af
        cmp #$07
        bcs _ga_not_af
        ; It's A-F: value 1-6, convert to nibble 10-15
        clc
        adc #9                  ; 1+9=10(A), 2+9=11(B), ... 6+9=15(F)
        plx                     ; discard saved A (pull into X to balance stack)
        clc
        rts
_ga_not_af:
        pla                     ; restore original value
ga_not_hex:
        sec
        rts
ga_is_digit:
        sec
        sbc #$30
        clc
        rts

        .enc "screen"
ga_prompt_text:
        .text "goto: "
        .byte 0
        .enc "none"

goto_addr_lo:   .byte 0
goto_addr_hi:   .byte 0
goto_input_len: .byte 0
goto_nibble:    .byte 0

; ------------------------------------------------------------
; mon_byte_to_hex - Convert byte in A to two hex chars
; ------------------------------------------------------------
mon_byte_to_hex:
        pha
        and #$0F
        tax
        lda mon_hex_chars,x
        tax                     ; Low nibble in X
        pla
        lsr
        lsr
        lsr
        lsr
        tay
        lda mon_hex_chars,y     ; High nibble in A
        rts

mon_hex_chars:
        .byte $30,$31,$32,$33,$34,$35,$36,$37,$38,$39  ; 0-9
        .byte $01,$02,$03,$04,$05,$06                  ; a-f

        .enc "screen"
mon_help_text:
        .text "d=dump t=ted g=go p=pc r=rom x=rst rtn=x"
        .byte 0
        .enc "none"