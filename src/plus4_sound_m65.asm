; ============================================================
; P4SND - TED to SID Sound Conversion
; 
; TED Sound Registers:
;   $FF0E - Voice 1 frequency low (8 bits)
;   $FF0F - Voice 1 frequency high (bits 0-1)
;   $FF10 - Voice 2 frequency low (8 bits)
;   $FF11 - Control register:
;           Bits 0-3: Volume (0-8, where 8=max)
;           Bit 4: Voice 2 frequency bit 8
;           Bit 5: Voice 2 frequency bit 9
;           Bit 6: Voice 2 sound type (0=tone, 1=noise)
;           Bit 7: Voice 1 enable? (DA bit)
;
; TED frequency formula: freq = 111860.8 / (1024 - value)
;   Value 0 = ~109 Hz, Value 1023 = highest
;
; SID frequency formula: freq = (Fn * Fclk) / 16777216
;   For 1MHz clock: freq = Fn * 0.0596
;   Fn = freq / 0.0596 = freq * 16.78
;
; Conversion: SID_freq = TED_freq * 16.78 / 0.0596
;           = 111860.8 / (1024 - TED_value) * 16.78
; ============================================================

; SID base addresses on MEGA65
SID_BASE = $D400
SID2_BASE = $D420               ; Second SID on MEGA65

; SID register offsets
SID_V1_FREQ_LO  = $00
SID_V1_FREQ_HI  = $01
SID_V1_PWLO     = $02
SID_V1_PWHI     = $03
SID_V1_CTRL     = $04
SID_V1_AD       = $05
SID_V1_SR       = $06

SID_V2_FREQ_LO  = $07
SID_V2_FREQ_HI  = $08
SID_V2_PWLO     = $09
SID_V2_PWHI     = $0A
SID_V2_CTRL     = $0B
SID_V2_AD       = $0C
SID_V2_SR       = $0D

SID_V3_FREQ_LO  = $0E
SID_V3_FREQ_HI  = $0F
SID_V3_PWLO     = $10
SID_V3_PWHI     = $11
SID_V3_CTRL     = $12
SID_V3_AD       = $13
SID_V3_SR       = $14

SID_FILT_LO     = $15
SID_FILT_HI     = $16
SID_RES_FILT    = $17
SID_MODE_VOL    = $18

; Sound state
p4snd_voice1_on: .byte 0
p4snd_voice2_on: .byte 0
p4snd_noise_mode: .byte 0

; Variable to store voice 1 freq high bits from $FF12 writes
p4snd_v1_freq_hi: .byte 0

; Plus/4 stores sound duration counters at:
; $04FC/$04FD = Voice 1 duration (16-bit, decremented by IRQ)
; $04FE/$04FF = Voice 2 duration (16-bit, decremented by IRQ)
; We monitor these and turn off SID when they hit 0

; ============================================================
; P4SND_Init - Initialize SID for TED sound emulation
; ============================================================
P4SND_Init:
        ; First, silence everything on BOTH SIDs
        lda #$00
        sta SID_BASE + SID_MODE_VOL     ; Volume off SID 1
        sta SID2_BASE + SID_MODE_VOL    ; Volume off SID 2
        
        ; Reset all voice controls (gate off) on both SIDs
        lda #$00
        sta SID_BASE + SID_V1_CTRL
        sta SID_BASE + SID_V2_CTRL
        sta SID_BASE + SID_V3_CTRL
        sta SID2_BASE + SID_V1_CTRL
        sta SID2_BASE + SID_V2_CTRL
        sta SID2_BASE + SID_V3_CTRL
        
        ; Clear all frequencies on both SIDs
        sta SID_BASE + SID_V1_FREQ_LO
        sta SID_BASE + SID_V1_FREQ_HI
        sta SID_BASE + SID_V2_FREQ_LO
        sta SID_BASE + SID_V2_FREQ_HI
        sta SID_BASE + SID_V3_FREQ_LO
        sta SID_BASE + SID_V3_FREQ_HI
        sta SID2_BASE + SID_V1_FREQ_LO
        sta SID2_BASE + SID_V1_FREQ_HI
        sta SID2_BASE + SID_V2_FREQ_LO
        sta SID2_BASE + SID_V2_FREQ_HI
        sta SID2_BASE + SID_V3_FREQ_LO
        sta SID2_BASE + SID_V3_FREQ_HI
        
        ; Set up SID voice 1 (pulse wave)
        lda #$08                ; Pulse width 50%
        sta SID_BASE + SID_V1_PWHI
        lda #$00
        sta SID_BASE + SID_V1_PWLO
        lda #$00                ; Attack=0, Decay=0 (instant)
        sta SID_BASE + SID_V1_AD
        lda #$F0                ; Sustain=15 (max), Release=0 (instant)
        sta SID_BASE + SID_V1_SR
        
        ; Set up SID voice 2 (pulse wave)
        lda #$08
        sta SID_BASE + SID_V2_PWHI
        lda #$00
        sta SID_BASE + SID_V2_PWLO
        lda #$00                ; Attack=0, Decay=0
        sta SID_BASE + SID_V2_AD
        lda #$F0                ; Sustain=15, Release=0
        sta SID_BASE + SID_V2_SR
        
        ; Set up SID voice 3 (noise)
        lda #$00
        sta SID_BASE + SID_V3_AD
        lda #$F0
        sta SID_BASE + SID_V3_SR
        
        ; No filters
        lda #$00
        sta SID_BASE + SID_FILT_LO
        sta SID_BASE + SID_FILT_HI
        sta SID_BASE + SID_RES_FILT
        
        ; Set volume to max (will be controlled by VOL command)
        lda #$0F
        sta SID_BASE + SID_MODE_VOL
        
        ; Clear state
        lda #0
        sta p4snd_voice1_on
        sta p4snd_voice2_on
        sta p4snd_noise_mode
        
        rts

; ============================================================
; P4SND_UpdateVolume - Update SID volume from TED $FF11
; TED volume is 0-8, SID volume is 0-15
; We need to scale: SID = TED * 15 / 8 ≈ TED * 2 (close enough)
; But be careful: TED 8 * 2 = 16 which overflows!
; Better: use a lookup table or cap at 15
; ============================================================
P4SND_UpdateVolume:
        lda ted_regs+$11
        and #$0F                ; Get volume bits 0-3
        cmp #$08                ; TED volume max is 8
        bcc +
        lda #$0F                ; If >= 8, use SID max (15)
        bra _snd_vol_store
+       asl                     ; Multiply by 2 (0->0, 4->8, 7->14)
        cmp #$10
        bcc _snd_vol_store
        lda #$0F                ; Clamp to 15
_snd_vol_store:
        sta SID_BASE + SID_MODE_VOL
        rts

; ============================================================
; P4SND_UpdateVoice1 - Update SID voice 1 from TED registers
; Uses ted_regs[$0E] for low byte and ted_regs[$0F] bits 0-1 for high
; ============================================================
P4SND_UpdateVoice1:
        ; TED frequency value: $FF0E (low) + $FF0F bits 0-1 (high)
        lda ted_regs+$0E        ; Voice 1 freq low
        sta p4snd_ted_freq_lo
        lda ted_regs+$0F        ; Voice 1 freq high (bits 0-1)
        and #$03
        sta p4snd_ted_freq_hi
        bra P4SND_V1_DoUpdate

; ============================================================
; P4SND_UpdateVoice1_WithHigh - Update voice 1 using stored high bits
; Uses ted_regs[$0E] for low byte and p4snd_v1_freq_hi for high bits
; This is called when BASIC writes freq high to $FF12
; ============================================================
P4SND_UpdateVoice1_WithHigh:
        lda ted_regs+$0E        ; Voice 1 freq low
        sta p4snd_ted_freq_lo
        lda p4snd_v1_freq_hi    ; Use the separately stored high bits
        sta p4snd_ted_freq_hi
        ; Fall through to DoUpdate

P4SND_V1_DoUpdate:
        ; Check if voice 1 is enabled
        ; Voice 1 should only play if the LOW byte ($FF0E) is non-zero
        ; The high bits alone (from $FF0F) are not enough to trigger sound
        lda p4snd_ted_freq_lo
        beq _v1_off             ; If low byte is 0, voice is off
        
        ; Convert TED frequency to SID frequency
        jsr P4SND_ConvertFreq
        
        ; Store to SID voice 1
        lda p4snd_sid_freq_lo
        sta SID_BASE + SID_V1_FREQ_LO
        lda p4snd_sid_freq_hi
        sta SID_BASE + SID_V1_FREQ_HI
        
        ; Make sure voices 2 and 3 are OFF (gate off) if not in use
        lda p4snd_voice2_on
        bne _v1_skip_silence_v2
        lda #$00                ; No waveform, no gate
        sta SID_BASE + SID_V2_CTRL
        sta SID_BASE + SID_V3_CTRL
_v1_skip_silence_v2:
        
        ; Enable voice 1 (pulse wave, gate on)
        lda #$41                ; Pulse + gate
        sta SID_BASE + SID_V1_CTRL
        lda #1
        sta p4snd_voice1_on
        rts
        
_v1_off:
        ; Disable voice 1
        lda #$00                ; Gate off, no waveform
        sta SID_BASE + SID_V1_CTRL
        lda #0
        sta p4snd_voice1_on
        rts

; ============================================================
; P4SND_UpdateVoice2 - Update SID voice 2/3 from TED registers
; ============================================================
P4SND_UpdateVoice2:
        ; TED frequency value: $FF10 (low) + $FF11 bits 4-5 (high)
        lda ted_regs+$10        ; Voice 2 freq low
        sta p4snd_ted_freq_lo
        lda ted_regs+$11        ; Control register
        lsr                     ; Shift bits 4-5 down to 0-1
        lsr
        lsr
        lsr
        and #$03
        sta p4snd_ted_freq_hi
        
        ; Check noise mode (bit 6 of $FF11)
        lda ted_regs+$11
        and #$40
        sta p4snd_noise_mode
        
        ; Check if voice 2 is enabled
        ; Voice 2 should only play if the LOW byte ($FF10) is non-zero
        ; The high bits alone (from $FF11) are not enough to trigger sound
        ; This matches real TED behavior where BASIC sets high bits first
        ; but sound doesn't start until low byte is written
        lda p4snd_ted_freq_lo
        beq _v2_off             ; If low byte is 0, voice is off
        
        ; Convert TED frequency to SID frequency
        jsr P4SND_ConvertFreq
        
        ; Check if noise or tone mode
        lda p4snd_noise_mode
        bne _v2_noise
        
        ; Tone mode - use SID voice 2 with pulse wave
        lda p4snd_sid_freq_lo
        sta SID_BASE + SID_V2_FREQ_LO
        lda p4snd_sid_freq_hi
        sta SID_BASE + SID_V2_FREQ_HI
        lda #$41                ; Pulse + gate
        sta SID_BASE + SID_V2_CTRL
        ; Turn off voice 3 (noise) completely
        lda #$00                ; No waveform, gate off
        sta SID_BASE + SID_V3_CTRL
        lda #1
        sta p4snd_voice2_on
        rts
        
_v2_noise:
        ; Noise mode - use SID voice 3 with noise waveform
        lda p4snd_sid_freq_lo
        sta SID_BASE + SID_V3_FREQ_LO
        lda p4snd_sid_freq_hi
        sta SID_BASE + SID_V3_FREQ_HI
        lda #$81                ; Noise + gate
        sta SID_BASE + SID_V3_CTRL
        ; Turn off voice 2 (tone) completely
        lda #$00                ; No waveform, gate off
        sta SID_BASE + SID_V2_CTRL
        lda #1
        sta p4snd_voice2_on
        rts
        
_v2_off:
        ; Disable voice 2 and 3
        lda #$00                ; Gate off, no waveform
        sta SID_BASE + SID_V2_CTRL
        sta SID_BASE + SID_V3_CTRL
        lda #0
        sta p4snd_voice2_on
        rts

; ============================================================
; P4SND_UpdateVoice2_FromBasic - Handle BASIC's weird frequency encoding
; BASIC SOUND 2 writes: freq_lo to $FF0F, freq_hi to $FF10
; This is backwards from the normal TED register layout!
; ============================================================
P4SND_UpdateVoice2_FromBasic:
        ; BASIC encoding:
        ; ted_regs[$0F] = frequency LOW byte (8 bits)
        ; ted_regs[$10] = frequency HIGH bits (2 bits, in bits 0-1)
        
        lda ted_regs+$0F        ; Freq LOW from $FF0F
        sta p4snd_ted_freq_lo
        lda ted_regs+$10        ; Freq HIGH from $FF10 bits 0-1
        and #$03
        sta p4snd_ted_freq_hi
        
        ; Check noise mode (bit 6 of $FF11)
        lda ted_regs+$11
        and #$40
        sta p4snd_noise_mode
        
        ; Check if voice 2 is enabled (freq low must be non-zero)
        lda p4snd_ted_freq_lo
        beq _v2b_off
        
        ; Convert TED frequency to SID frequency
        jsr P4SND_ConvertFreq
        
        ; Check if noise or tone mode
        lda p4snd_noise_mode
        bne _v2b_noise
        
        ; Tone mode - use SID voice 2 with pulse wave
        lda p4snd_sid_freq_lo
        sta SID_BASE + SID_V2_FREQ_LO
        lda p4snd_sid_freq_hi
        sta SID_BASE + SID_V2_FREQ_HI
        lda #$41                ; Pulse + gate
        sta SID_BASE + SID_V2_CTRL
        ; Turn off voice 3 (noise) completely
        lda #$00
        sta SID_BASE + SID_V3_CTRL
        lda #1
        sta p4snd_voice2_on
        rts
        
_v2b_noise:
        ; Noise mode - use SID voice 3 with noise waveform
        ; TED noise has a different character than SID noise
        ; Scale frequency down by 4 to get a lower rumble that matches TED better
        lsr p4snd_sid_freq_hi
        ror p4snd_sid_freq_lo
        lsr p4snd_sid_freq_hi
        ror p4snd_sid_freq_lo
        lsr p4snd_sid_freq_hi
        ror p4snd_sid_freq_lo
        lsr p4snd_sid_freq_hi
        ror p4snd_sid_freq_lo
        
        lda p4snd_sid_freq_lo
        sta SID_BASE + SID_V3_FREQ_LO
        lda p4snd_sid_freq_hi
        sta SID_BASE + SID_V3_FREQ_HI
        lda #$81                ; Noise + gate
        sta SID_BASE + SID_V3_CTRL
        lda #$00
        sta SID_BASE + SID_V2_CTRL
        lda #1
        sta p4snd_voice2_on
        rts
        
_v2b_off:
        ; Disable voice 2 and 3
        lda #$00
        sta SID_BASE + SID_V2_CTRL
        sta SID_BASE + SID_V3_CTRL
        lda #0
        sta p4snd_voice2_on
        rts

; Temporary storage for frequency conversion
p4snd_ted_freq_lo: .byte 0
p4snd_ted_freq_hi: .byte 0
p4snd_sid_freq_lo: .byte 0
p4snd_sid_freq_hi: .byte 0

; ============================================================
; P4SND_FrameTick - Called once per frame to check sound duration
; This ensures sounds stop even if BASIC IRQ handling isn't perfect
; ============================================================
P4SND_FrameTick:
        ; Check voice 1 duration counter at $04FC/$04FD
        lda p4snd_voice1_on
        beq _snd_tick_v2          ; Voice 1 not on, skip
        
        ; Read duration from Plus/4 RAM mirror (LOW_RAM_BUFFER + $4FC)
        lda LOW_RAM_BUFFER + $4FC
        ora LOW_RAM_BUFFER + $4FD
        bne _snd_tick_v2          ; Duration > 0, still playing
        
        ; Duration is 0, turn off voice 1
        lda #$00                  ; Gate off, no waveform
        sta SID_BASE + SID_V1_CTRL
        lda #0
        sta p4snd_voice1_on
        
_snd_tick_v2:
        ; Check voice 2 duration counter at $04FE/$04FF
        lda p4snd_voice2_on
        beq _snd_tick_done        ; Voice 2 not on, skip
        
        ; Read duration from Plus/4 RAM mirror
        lda LOW_RAM_BUFFER + $4FE
        ora LOW_RAM_BUFFER + $4FF
        bne _snd_tick_done        ; Duration > 0, still playing
        
        ; Duration is 0, turn off voice 2 (and voice 3 for noise)
        lda #$00                  ; Gate off, no waveform
        sta SID_BASE + SID_V2_CTRL
        sta SID_BASE + SID_V3_CTRL
        lda #0
        sta p4snd_voice2_on
        
_snd_tick_done:
        rts


; ============================================================
; P4SND_ConvertFreq - Convert TED frequency to SID frequency
; Input: p4snd_ted_freq_lo/hi = 10-bit TED frequency value
; Output: p4snd_sid_freq_lo/hi = 16-bit SID frequency value
;
; Formula: SID = 1904812 / (1024 - TED)
; We use K = $1D0000 = 1900544 (0.22% error, inaudible)
;
; This computes: SID = $1D0000 / (1024 - TED)
;                    = $1D0000 / divisor
; where divisor is 10-bit (1-1024)
; ============================================================
P4SND_ConvertFreq:
        ; Calculate divisor = 1024 - TED
        ; 1024 = $0400
        sec
        lda #$00
        sbc p4snd_ted_freq_lo
        sta _snd_divisor_lo
        lda #$04
        sbc p4snd_ted_freq_hi
        sta _snd_divisor_hi
        
        ; Check for divisor = 0 (TED = 1024, shouldn't happen)
        ora _snd_divisor_lo
        bne _div_ok
        ; Divisor is 0, return max frequency
        lda #$FF
        sta p4snd_sid_freq_lo
        sta p4snd_sid_freq_hi
        rts
        
_div_ok:
        ; Perform 24-bit / 16-bit division using standard shift-subtract algorithm
        ; Dividend = $1D0000
        ; Divisor = _snd_divisor (16-bit)
        ; Result goes in p4snd_sid_freq (16-bit)
        
        ; Initialize: dividend in _snd_dividend (3 bytes), remainder starts at 0
        lda #$00
        sta _snd_dividend+0     ; Low byte = $00
        sta _snd_dividend+1     ; Mid byte = $00  
        lda #$1D
        sta _snd_dividend+2     ; High byte = $1D
        
        ; Clear remainder
        lda #$00
        sta _snd_remainder
        sta _snd_remainder+1
        
        ; Clear result
        sta p4snd_sid_freq_lo
        sta p4snd_sid_freq_hi
        
        ; 24-bit division: shift dividend bits into remainder, compare, subtract
        ldx #24                 ; 24 bits to process
        
_div_loop:
        ; Shift dividend left, MSB goes into carry
        asl _snd_dividend+0
        rol _snd_dividend+1
        rol _snd_dividend+2
        ; Carry now has the bit shifted out of dividend
        
        ; Shift that bit into remainder
        rol _snd_remainder
        rol _snd_remainder+1
        
        ; Shift result left (make room for new quotient bit)
        asl p4snd_sid_freq_lo
        rol p4snd_sid_freq_hi
        
        ; Compare remainder with divisor
        lda _snd_remainder+1
        cmp _snd_divisor_hi
        bcc _div_no_sub         ; remainder < divisor, don't subtract
        bne _div_do_sub         ; remainder > divisor, do subtract
        ; High bytes equal, check low bytes
        lda _snd_remainder
        cmp _snd_divisor_lo
        bcc _div_no_sub         ; remainder < divisor
        
_div_do_sub:
        ; Subtract divisor from remainder
        sec
        lda _snd_remainder
        sbc _snd_divisor_lo
        sta _snd_remainder
        lda _snd_remainder+1
        sbc _snd_divisor_hi
        sta _snd_remainder+1
        
        ; Set bit 0 of result (quotient bit = 1)
        inc p4snd_sid_freq_lo
        ; Note: inc won't affect high byte, but that's ok since we just shifted
        
_div_no_sub:
        dex
        bne _div_loop
        
        rts

; Division working variables
_snd_divisor_lo:  .byte 0
_snd_divisor_hi:  .byte 0
_snd_dividend:    .fill 3, 0    ; 24-bit dividend
_snd_remainder:   .word 0       ; 16-bit remainder

_snd_tmp_lo: .byte 0
_snd_tmp_hi: .byte 0