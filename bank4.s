.include "globals.inc"

/*
	MEGAMAN 1 BANK 4 ROM MAP
 */

;---------------------------------------------------------------------------

;The music player is located in bank 4.
;Bank 4 also contains the Elecman stage and game startup screen
;settings, and therefore this code begins from $9000, not $8000.
;
; Note: Mega Man 2 has almost the same code!
;

Var41 := $41   ;If this address has low bit set, the engine pauses music (read only)

Priorities                   := $E0
; $E0 = #ssssmmmm
; mmmm = Music priority (always #$F?...)
; ssss = sfx   priority

SFX_ChannelUsage             := $E1
; $E1 = #????3210
; channels used for SFX
;

SongDataPointer              := $E2 ; E2,E3

SuspendPlayback              := $E4

ChannelsRemaining            := $E5
PriorityTemp                 := $E5 ;alias
TempVibratoPointer           := $E5 ;alias; occupies E5,E6

RegisterOffsetUnknown        := $E6 ; mirrors the purpose of $EB? I don't understand...

SpeedUpFactor                := $E7 ; If 2, each note is played at half length!
FadingSpeed                  := $E8 ; Set by Command $FD
VolumeFadeStrength           := $E9 ; Controlled by FadingSpeed

SoundFrameCounter            := $EA
CurrentChannelRegisterOffset := $EB
CurChanDataPtr               := $EC ; EC,ED   it's an index to AudioStatusBuffer
CurrentChannelNumber         := $EE
UnknownEF                    := $EF

SFX_Addr                     := $F0 ; F0,F1
SFX_FrameWaitCount           := $F2 ; Number of frames before next SFX tick
SFX_LoopCounter              := $F3

; F4,F5 various temps

MUSCMD_SetSpeed               = 0
MUSCMD_SetVibratoCtrl         = 1 ;probably wrong
MUSCMD_SetDutyCycle           = 2
MUSCMD_SetVolumeAndEnv        = 3
MUSCMD_Loop                   = 4
MUSCMD_SetNoteBaseOffset      = 5
MUSCMD_PlayNoteExtraLength    = 6
MUSCMD_SetVolumeCurve         = 7
MUSCMD_SetVibratoIndex        = 8
MUSCMD_EndOfData              = 9
MUSCMD_SetNoteDelay           = $20
MUSCMD_TripletSpeed           = $30

SFXCMD_SetFrameWaitCount      = 0
SFXCMD_SetVibratoCtrl         = 1
SFXCMD_SetDutyCycle           = 2
SFXCMD_SetVolumeAndEnv        = 3
SFXCMD_Loop                   = 4
SFXCMD_SetVibratoBuffer       = 5
SFXCMD_EndOfData              = 6
SFXCMD_SetNotePeriod          = $80
SFXCMD_Exec_80                = $80

; Each channel data is 31 bytes ($1F)
CHNVAR_MusicDataAddr           = $00 ; Pointer to the current executing position of the track
CHNVAR_NoteLengthWord          = $02 ; This value is only read at 96DF
CHNVAR_TickSpeed               = $04
CHNVAR_LoopCounter             = $05
CHNVAR_VibratoDefs             = $06 ;high 3 bits: delay in pitch? low 5 bits: vibrato depth
CHNVAR_NoteBaseOffset          = $07 ;word
CHNVAR_VibratoCtrl             = $09 ;probably wrong
CHNVAR_NotePeriodWord          = $0A ;looks right
CHNVAR_DutyCycle               = $0C ;high two bits=duty, low 6=volume and env
CHNVAR_VolumeCurveLength       = $0D ;00..7F=ascending, 80..FF=descending
CHNVAR_VolumeCurvePosition     = $0E
CHNVAR_VolumeCurveStrength     = $0F ;high 4 bits used only
CHNVAR_VibratoCtrlSFX          = $10 ;probably wrong
CHNVAR_NotePeriodWordSFX       = $11 ;looks right
CHNVAR_DutyCycleSFX            = $13
CHNVAR_VibratoBuffer           = $14 ;14,15,16,17
CHNVAR_VibratoVar1             = $18
CHNVAR_VibratoVar2             = $19
CHNVAR_PeriodLo                = $1A ; Added together with NotePeriodWord.Lo when written
CHNVAR_PeriodHi                = $1B ; Added together with NotePeriodWord.Hi when written
CHNVAR_PeriodHiOld             = $1C ; Cache of PeriodHi
CHNVAR_TremoloVar1             = $1D
CHNVAR_TremoloVar2             = $1E

AudioStatusBuffer             := $0500 ;ends at $57C
AudioChannelStatusSize         = 31

VibratoBufferPointer          := $57C ;pointer to the "fifth" channel data


; Vibrato bytes;
;   First byte seems to be the vibrato wavelength (aka. speed)
;   Second byte:
;     High nibble: Vibrato depth
;     Low nibble: Vibrato wavelength (again?)
;   Third byte:
;     Tremolo depth | $80
;   Fourth byte:
;     Tremolo speed? 0=disabled

.segment "BANK4"

inc_bank_part 4, 0, $1000

SoundCodePlay: ; at $9000
  jmp UpdateSound

SoundCodeInit: ; at $9003
  cmp #$FC
  bne :+
  jmp SoundCodeCommandFC
:
  cmp #$FD
  bne :+
  jmp SoundCodeCommandFD
:
  cmp #$FE
  bne :+

SoundCodeCommandFE: ; at 9015
  lda #$01
  sta SuspendPlayback
  lda #$00
  sta CurChanDataPtr
  jmp ResetSoundSystem

:
  cmp #$ff
  bne :+

SoundCodeCommandFF: ; at 9024
  lda #$01
  sta SuspendPlayback
  lda #$00
  sta CurChanDataPtr
  jmp StopMusic

:

; address of sound => SongDataPointer
  asl
  tax
  lda MusicAndSfxTable,x
  sta SongDataPointer+0
  inx
  lda MusicAndSfxTable,x
  sta SongDataPointer+1

  ldy #$00
  lda (SongDataPointer),y
  tax
  and #$0f
  beq StartSFX

StartMusicTrack:
; If this music score has lower priority than the currently playing one,
; then just return...
  lda Priorities
  and #$0f
  sta PriorityTemp
  cpx PriorityTemp
  bcs :+
  rts
:
; Set the priority to this new music
  stx PriorityTemp
  lda Priorities
  and #$F0
  ora PriorityTemp
  sta Priorities

; Suspend playback so the sound code won't interrupt us
  lda #$01
  sta SuspendPlayback

  lda #$00
  sta CurChanDataPtr

  lda #$00
  sta SpeedUpFactor
  sta FadingSpeed

  lda #$04    ; Number of channels to loop thru
  sta ChannelsRemaining

; SongDataPointer += 1
  lda #1

; Loop thru channels
:
  clc
  adc SongDataPointer+0
  sta SongDataPointer+0
  lda #0
  adc SongDataPointer+1
  sta SongDataPointer+1

  ldx CurChanDataPtr

; Copy channel's music data address to work RAM

  ldy #CHNVAR_MusicDataAddr
:
  lda (SongDataPointer),y
  sta AudioStatusBuffer,x
  inx
  iny
  cpy #$02
  bne :-

; Clear rest of the channel's vars (02..0F). 10..1E is left intact.

  ldy #$0E
  lda #$00
:
  sta AudioStatusBuffer,x
  inx
  dey
  bne :-

  lda SFX_ChannelUsage
  lsr
  bcs :+
  jsr ClearSFXRAM
:
  jsr SFX_NextChannel
  dec ChannelsRemaining
  beq :+
  lda #$02
  jmp :----
:
  ldy #$02
  lda (SongDataPointer),y
  sta VibratoBufferPointer+0
  iny
  lda (SongDataPointer),y
  sta VibratoBufferPointer+1
  jsr SFX_FirstChannel

; Let music callback play again
  lda #$00
  sta SuspendPlayback
  rts


StartSFX:; at 90BC
 
; If SFX priority has a lower priority then the current one, then exit
  lda Priorities
  and #$f0
  sta PriorityTemp
  cpx PriorityTemp
  bcs :+
  rts
:
; Set the sfx priority to this new sfx
  stx PriorityTemp
  lda Priorities
  and #$0f
  ora PriorityTemp
  sta Priorities
; Suspend playback so the sound code won't interrupt us
  lda #$01
  sta SuspendPlayback
  lda #$00
  sta CurChanDataPtr
  ldx #$00
  lda #$02
  clc
  adc SongDataPointer+0
  sta SFX_Addr+0
  txa
  adc SongDataPointer+1
  sta SFX_Addr+1

  stx SFX_FrameWaitCount
  stx SFX_LoopCounter

; ChannelUsage
  ldy #$01
  lda (SongDataPointer),y
  and #$0f
  tax
  ora SFX_ChannelUsage
  pha
  stx SFX_ChannelUsage


; Loop thru all channels
  lda #$04
  sta ChannelsRemaining
  lda #$02
  sta RegisterOffsetUnknown
:
  pla

  lsr
  pha
  bcc :+

; this SFX uses this channel so clear SFX RAM
  jsr ClearSFXRAM

  lda SFX_ChannelUsage
  lsr
  bcs :+
  jsr UpdateChannelUsedBySFX
: 
  jsr SFX_NextChannel

  lda #$04
  clc
  adc RegisterOffsetUnknown
  sta RegisterOffsetUnknown

  dec ChannelsRemaining
  bne :--

  jsr SFX_FirstChannel

  lda SFX_ChannelUsage
  sta UnknownEF
  pla


; Let music callback play again
  lda #$00
  sta SuspendPlayback
  rts

;
; Sound effect = #$FC
;
; Speed up the music
;
SoundCodeCommandFC: ; at 912A
  iny
  sty SpeedUpFactor
  rts

;
; Sound effect = #$FD
;
; Fade in (negative number) or out (positive number)
;
SoundCodeCommandFD: ; at 912E
  lda FadingSpeed
  bne :+
  sty FadingSpeed
  lda #$01
  sta VolumeFadeStrength
  lda SoundFrameCounter                               ; $EA
  and #$01
  sta SoundFrameCounter                               ; $EA
:
  rts



ResetSoundSystem: ; at 913F
; Set SFX priority to 0
  lda Priorities
  and #$0f
  sta Priorities

  lda #$04
  sta ChannelsRemaining
  lda #$02
  sta RegisterOffsetUnknown

:
  lda SFX_ChannelUsage
  lsr
  bcc :+
  jsr ClearSFXRAM
  jsr UpdateChannelUsedBySFX
:
  jsr SFX_NextChannel
  lda #$04
  clc
  adc RegisterOffsetUnknown
  sta RegisterOffsetUnknown
  dec ChannelsRemaining
  bne :--

  lda #$00
  sta SFX_ChannelUsage
  sta UnknownEF
  lda #$00
  sta SuspendPlayback
  rts



;Loads a SFX-related pointer(?)
UpdateChannelUsedBySFX: ; at 9171
  lda CurChanDataPtr
  clc
  adc #CHNVAR_NotePeriodWord
  tax
  lda AudioStatusBuffer,x     ;If NotePeriodWord is nonzero?
  inx
  ora AudioStatusBuffer,x
  bne ReloadVibrato
  ldy ChannelsRemaining       ;NotePeriodWord was zero.
  ldx RegisterOffsetUnknown
  jsr MuteChannel
  ldx CurChanDataPtr
  lda AudioStatusBuffer+CHNVAR_MusicDataAddr,x     ;Reads MusicDataAddr
  inx
  ora AudioStatusBuffer+CHNVAR_MusicDataAddr,x
  bne ReloadVibrato
  rts


StopMusic: ; at 9193
; Set music priority to 0
  lda Priorities
  and #$F0
  sta Priorities

  lda #$00
  sta SpeedUpFactor
  sta FadingSpeed

; Loop thru all channels
  lda #$04
  sta ChannelsRemaining
:
  lda #$00
  ldx CurChanDataPtr         ;Zeroes MusicDataAddr
  sta AudioStatusBuffer,x
  inx
  sta AudioStatusBuffer,x

  jsr NextChannelPtr
  dec ChannelsRemaining
  bne :-

  lda #$00
  sta SuspendPlayback
  rts


ClearSFXRAM: ;at 91BA ;Zeroes the region $10..$1E of the current channel.
  ldy #$0F
  lda #$10
  clc
  adc CurChanDataPtr
  tax
  lda #$00
:
  sta AudioStatusBuffer,x
  inx
  dey
  bne :-
  rts


; This routine copies the data from the currently selected
; vibrato buffer of the music track into the 14..17 range
; in the channel.
;
; It is the same region as updated by SFXCMD_SetVibratoBuffer.
;
ReloadVibrato: ; at 91CC
  lda TempVibratoPointer+0 ;save this var
  pha
  lda TempVibratoPointer+1 ;(used temporarily for other purpose)
  pha

  lda VibratoBufferPointer+0
  sta TempVibratoPointer+0
  lda VibratoBufferPointer+1
  sta TempVibratoPointer+1

  lda CurChanDataPtr
  clc
  adc #CHNVAR_VibratoDefs
  tax
  lda AudioStatusBuffer,x
  and #$1F
  beq :++    ; No vibrato?

; Vibrato
  tay
  lda #$00
:
  clc
  adc #$04
  dey
  bne :-
:
  tay
  txa
  clc
  adc #(CHNVAR_VibratoBuffer - CHNVAR_VibratoDefs)
  tax
  lda #$04
:
  pha
  lda (TempVibratoPointer),y
  sta AudioStatusBuffer,x
  iny
  inx
  pla
  sec
  sbc #$01
  bne :-

  pla
  sta TempVibratoPointer+1
  pla
  sta TempVibratoPointer+0
  rts



; Rotate SFX_ChannelUsage right without involving carry
SFX_NextChannel: ; at 920F
  lsr SFX_ChannelUsage
  bcc NextChannelPtr
  lda SFX_ChannelUsage
  ora #$80
  sta SFX_ChannelUsage
NextChannelPtr: ; at 9219
   ; CurChanDataPtr += 31
  lda #AudioChannelStatusSize
  clc
  adc CurChanDataPtr
  sta CurChanDataPtr
  rts


SFX_FirstChannel: ; at 9221
  lsr SFX_ChannelUsage
  lsr SFX_ChannelUsage
  lsr SFX_ChannelUsage
  lsr SFX_ChannelUsage
  rts


;
; Mutes channel Y by setting period to #0 (or disabling it if noise)
;
; X must be set to 4 * Y + 2
;

MuteChannel: ;at 922A
  cpy #$01    ; if chn# = 1 then disable noise channel
  beq :+
  lda #$00
  sta $4000,x
  inx
  sta $4000,x
  dex
  rts
:
  lda #$07    ; Disable noise channel
  sta $4015
  rts


UpdateSound: ;at 923F
  inc SoundFrameCounter                               ; $EA
  lda SuspendPlayback
  beq :+
  rts
:
; CurChanDataPtr = #$500
  ldx #<(AudioStatusBuffer)
  ldy #>(AudioStatusBuffer)
  stx CurChanDataPtr+0
  sty CurChanDataPtr+1
; Set sounds registers offset to 0
  lda #$00
  sta CurrentChannelRegisterOffset
; Loop thru all channels
  lda #$04
  sta CurrentChannelNumber
UpdateSound_ChannelLoop:
  lda #$01
  ldy #CHNVAR_VibratoVar1
  clc
  adc (CurChanDataPtr),y  ; adds 1 to VibratoVar1
  sta (CurChanDataPtr),y
  lda #$01
  ldy #CHNVAR_TremoloVar1
  clc
  adc (CurChanDataPtr),y  ; adds 1 to TremoloVar1
  sta (CurChanDataPtr),y
; Call SFX_TickChannel if bit set in UnknownEF for this channel...
  lda UnknownEF
  lsr
  bcc :+
  jsr SFX_TickChannel
:
; Jump if bit set in Var41 for this channel...
  lda Var41
  lsr
  bcc :+
  jmp :++
:
; Skip processing if music address = 0
  ldy #CHNVAR_MusicDataAddr+0
  lda (CurChanDataPtr),y
  iny
  ora (CurChanDataPtr),y
  beq :+
  lda #$01
  ldy #CHNVAR_VolumeCurvePosition
  clc
  adc (CurChanDataPtr),y ; adds 1 to CHNVAR_VolumeCurvePosition
  sta (CurChanDataPtr),y
  jsr MUS_TickChannel
  jmp :++
; Music data for this channel is missing, so mute it
:
  lda UnknownEF
  lsr
  bcs :+
  ldx CurrentChannelRegisterOffset
  inx
  inx
  ldy CurrentChannelNumber
  jsr MuteChannel
:
; Rotate UnknownEF right without involving carry
  lsr UnknownEF
  bcc :+
  lda UnknownEF
  ora #$80
  sta UnknownEF
:
  dec CurrentChannelNumber
  beq UpdateSound_AllChannelsDone
; CurrentChannelRegisterOffset += #4, CurChanDataPtr += #AudioChannelStatusSize
  lda #4
  clc
  adc CurrentChannelRegisterOffset
  sta CurrentChannelRegisterOffset
  lda #AudioChannelStatusSize
  clc
  adc CurChanDataPtr+0
  sta CurChanDataPtr+0
  lda #0
  adc CurChanDataPtr+1
  sta CurChanDataPtr+1
  jmp UpdateSound_ChannelLoop
UpdateSound_AllChannelsDone:
; Do something that semi-resets frame counter sometimes (not very often)
  lda FadingSpeed
  and #$7f
  beq :++
  cmp SoundFrameCounter                               ; $EA
  bne :++
  lda SoundFrameCounter                               ; $EA
  and #$01
  sta SoundFrameCounter                               ; $EA
  inc VolumeFadeStrength
  lda #$10
  cmp VolumeFadeStrength
  bne :++
  lda FadingSpeed
  bmi :+
  lda #$00
  sta FadingSpeed
:
  lda #$0f
  sta VolumeFadeStrength
:
; This seems to be the length of an SFX or something of the like...
  lda SFX_FrameWaitCount
  beq :+
  dec SFX_FrameWaitCount
:
  lsr UnknownEF
  lsr UnknownEF
  lsr UnknownEF
  lsr UnknownEF
  rts


IterateNote: ;at 92F6
; This function is arrived when
; both NoteLength > 0 and NotePeriodWord > 0.
;
  ldy #CHNVAR_DutyCycle
  lda (CurChanDataPtr),y
  ldy #$02
  cpy CurrentChannelNumber
  beq :+      ;Skip 'and #$0F' for triangle channel
  and #$0f
:
  sta $F4
  lda FadingSpeed
  and #$7F
  beq :++++++
  lda VolumeFadeStrength

  ldy #$02
  cpy CurrentChannelNumber ;triangle channel can't be faded
  bne :++
  ldx #$0c
:
  clc
  adc VolumeFadeStrength
  dex
  bne :-
:
  tay
  lda FadingSpeed
  bmi :++
  ldx #$ff
:
  inx
  cpx $F4
  beq :+++
  dey
  bne :-
  stx $F4
  jmp :+++
: 
:
  dec $F4
  beq :+
  dey
  bne :-
:
  lda #$02
  cmp CurrentChannelNumber
  beq DoneVolumeCurve
  ldy #CHNVAR_VolumeCurveLength
  lda (CurChanDataPtr),y
  tax
  and #$7f
  beq DoneVolumeCurve

; This part omitted if channel is triangle or VolumeCurveLength had no low 7 bits
  iny
  cmp (CurChanDataPtr),y    ;VolumeCurvePosition
  beq :+
  iny
  lda (CurChanDataPtr),y    ;VolumeCurveStrength
  and #$0f
  jmp :+++
:
  lda #$00
  sta (CurChanDataPtr),y    ;VolumeCurvePosition
  iny
  lda (CurChanDataPtr),y    ;VolumeCurveStrength
  lsr
  lsr
  lsr
  lsr
  sta $F5                   ; $F5 <- (VolumeCurveStrength >> 4)
  txa                       ; A = VolumeCurveLength
  bpl :+
  lda #$00
  sec
  sbc $F5
  sta $F5                   ; $F5 = 0 - $F5
:
  lda (CurChanDataPtr),y    ; VolumeCurveStrength
  and #$0f                  ; low 4 bits thereof
  clc
  adc $F5
  bpl :+
  lda #$00
  jmp :++
:
  cmp $F4
  bcc :+
  lda $F4
:
  sta $F4
  lda (CurChanDataPtr),y    ; VolumeCurveStrength
  and #$F0
  ora $F4
  sta (CurChanDataPtr),y    ; VolumeCurveStrength

; Arrived here if channel was triangle or VolumeCurveLength had no low 7 bits
DoneVolumeCurve:
  lda UnknownEF
  lsr
  bcs :+
  lda #CHNVAR_DutyCycle
  sta $F5
  jmp ProcessTremolo
:
  lda #CHNVAR_VibratoCtrl
  sta $F5
  jmp ProcessVibrato

ProcessTremolo: ;volume vibrato
    ; $F5 points to DutyCycle or DutyCycleSFX|$80
   
  ldy #CHNVAR_VibratoBuffer+2
  lda (CurChanDataPtr),y
  and #$7f
  beq :++++++
  ldy #CHNVAR_TremoloVar1
  cmp (CurChanDataPtr),y
  beq :+
  jmp :+++++
:
  lda #$00
  sta (CurChanDataPtr),y
  ldy #CHNVAR_VibratoBuffer+3
  lda (CurChanDataPtr),y
  ldy #CHNVAR_TremoloVar2
  clc
  adc (CurChanDataPtr),y
  beq :+
  bpl :++
:
  lda #$01
  sta (CurChanDataPtr),y
  jmp :++
:
  sta (CurChanDataPtr),y
  cmp #$10
  bcc :++
  lda #$0f
  sta (CurChanDataPtr),y
:
  lda #$00
  ldy #CHNVAR_VibratoBuffer+3
  sec
  sbc (CurChanDataPtr),y
  sta (CurChanDataPtr),y
:
  ldy #CHNVAR_TremoloVar2
  lda (CurChanDataPtr),y
  cmp $F4
  bcs :+
  sta $F4
:
  ldy #$02
  cpy CurrentChannelNumber
  beq :+
  lda $F5
  and #$7f
  tay
  lda (CurChanDataPtr),y
  and #$f0
  ora $F4
  sta $F4
:
  ldx CurrentChannelRegisterOffset
  lda $F4
  sta $4000,x ;Write duty,decaysettings,length_disable
  lda $F5
  bpl :+
  lda #CHNVAR_VibratoCtrlSFX | $80
  sta $F5
  jmp ProcessVibrato
:
  lda #CHNVAR_VibratoCtrl
  sta $F5

ProcessVibrato:
  ; $F5 points to VibratoCtrl or VibratoCtrlSFX|$80

  lda $F5
  and #$7f
  tay
  ldx #$00
  lda (CurChanDataPtr),y
; The value read comes usually from VibratoCtrl, but may come from VibratoCtrlSFX too
  beq :++
  bpl :+
  dex
:
  iny ;Either NotePeriodWord or NotePeriodWordSFX
  clc
  adc (CurChanDataPtr),y
  sta (CurChanDataPtr),y
  txa
  iny
  adc (CurChanDataPtr),y
  sta (CurChanDataPtr),y
:
  lda $F5
  bmi :+
  lda UnknownEF
  lsr
  bcc :+
  rts
:
  ldy #CHNVAR_VibratoBuffer+0
  lda (CurChanDataPtr),y
  and #$7f
  bne :+
  jmp :++++++
:
  ldy #CHNVAR_VibratoVar1
  cmp (CurChanDataPtr),y
  beq :+
  jmp :+++++
:
  lda #$00                    ;If VibratoVar1 was 0
  sta (CurChanDataPtr),y
  tax
  ldy #CHNVAR_VibratoBuffer+1
  lda (CurChanDataPtr),y
  rol
  rol
  rol
  rol
  and #$07
  sta $F4

  ldy #CHNVAR_VibratoVar2
  lda (CurChanDataPtr),y
  asl
  bcc :+
  lda #$00                    ;If VibratoVar2 had high bit set
  sec
  sbc $F4
  sta $F4
  dex
:
  lda $F4
  clc
  ldy #CHNVAR_PeriodLo
  adc (CurChanDataPtr),y
  sta (CurChanDataPtr),y
  iny
  txa
  adc (CurChanDataPtr),y
  sta (CurChanDataPtr),y
  ldy #CHNVAR_VibratoBuffer+1
  lda (CurChanDataPtr),y
  and #$1f
  sta $F4
  ldy #CHNVAR_VibratoVar2
  lda (CurChanDataPtr),y      ;Adds 1 to VibratoVar2
  clc
  adc #$01
  sta (CurChanDataPtr),y
  and #$7f
  cmp $F4
  bne :+++
  lda (CurChanDataPtr),y
  and #$80
  sta (CurChanDataPtr),y
  ldy #CHNVAR_VibratoBuffer+0
  lda (CurChanDataPtr),y
  asl
  bcs :++
  lda (CurChanDataPtr),y
  ora #$80
  sta (CurChanDataPtr),y
  ldy #CHNVAR_VibratoVar2
  lda (CurChanDataPtr),y
  bpl :+
  and #$7f
  sta (CurChanDataPtr),y
  jmp :+++
:
  ora #$80
  sta (CurChanDataPtr),y
  jmp :++
:
  lda (CurChanDataPtr),y
  and #$7f
  sta (CurChanDataPtr),y

:

  ; $F5 points to VibratoCtrl or VibratoCtrlSFX|$80
  
  lda $F5
  and #$7f
  sta $F5
  inc $F5         ; Now points to NotePeriodWord or NotePeriodWordSFX (no high bit)

  ldy #CHNVAR_PeriodLo
  lda (CurChanDataPtr),y
  ldy $F5
  clc
  adc (CurChanDataPtr),y
  tax

  ldy #CHNVAR_PeriodHi
  lda (CurChanDataPtr),y
  inc $F5                 ; NotePeriodWord(SFX).hi
  ldy $F5
  adc (CurChanDataPtr),y
  tay

; X,y now contains (NotePeriodWord + Period)

  lda #$01
  cmp CurrentChannelNumber
  bne :+

; This part is ignored for noise channel
  lda #15 ;enable all channels
  sta $4015
  txa
  and #15
  tax
  inc $F5
  ldy $F5
  lda (CurChanDataPtr),y
  and #$80
  sta $F4
  txa
  ora $F4
  tax
  ldy #$00
:
; Period writing: X = Lo, Y = Hi (only written if changed)
  txa
  ldx CurrentChannelRegisterOffset
  inx
  inx
  sta $4000,x     ;write wavelength low byte
  tya
  ldy #CHNVAR_PeriodHiOld
  cmp (CurChanDataPtr),y
  bne :+
  rts
:
  sta (CurChanDataPtr),y
  ora #$08
  inx
  sta $4000,x     ;write wavelength high byte
  rts


DisableChannel: ;at 9508
  ldy #$01
  cpy CurrentChannelNumber
  bne :+
  lda #$07    ; Disable noise channel
  sta $4015
  rts
:
; Write $0000 as the period value of the channel
  lda #$00
  ldx CurrentChannelRegisterOffset
  inx
  inx
  sta $4000,x
  inx
  sta $4000,x
  rts


ResetPitch: ;at 9522
   ; Input: $F4 = index to DutyCycle or DutyCycleSFX
   ;
  ldy #CHNVAR_VibratoBuffer+0
  lda (CurChanDataPtr),y
  and #$7F
  sta (CurChanDataPtr),y
  ldy #CHNVAR_VibratoBuffer+2
  lda (CurChanDataPtr),y
  asl
  bcc :++
  ldy $F4
  lda (CurChanDataPtr),y

  ldx #$02
  cpx CurrentChannelNumber
  beq :+
  and #$0f ;Ignored for triangle channel
:
  ldy #CHNVAR_TremoloVar2
  sta (CurChanDataPtr),y
:
  ldx #$06
  lda #$00
  ldy #CHNVAR_VibratoVar1  ; Will zero VibratoVar1..TremoloVar1
:
  sta (CurChanDataPtr),y
  iny
  dex
  bne :-

; Set old period to dummy value to be sure it gets updated in the next frame
  lda #$ff
  ldy #CHNVAR_PeriodHiOld
  sta (CurChanDataPtr),y
  rts


; Does something for the channel without changing PeriodHiOld
ResetPitchButSavePitchHiOld: ;at 9554
  ldy #CHNVAR_PeriodHiOld
  lda (CurChanDataPtr),y
  pha
   jsr ResetPitch
  pla
  ldy #CHNVAR_PeriodHiOld
  sta (CurChanDataPtr),y
  rts


DoCalculatedGoto: ;at 9562
  txa
  asl
  tay
  iny
  pla
  sta $F4
  pla
  sta $F5
  lda ($F4),y
  tax
  iny
  lda ($F4),y
  sta $F5
  stx $F4
  jmp ($00F4)



SFX_TickChannel: ;at 9579
  lda SFX_FrameWaitCount
  bne :+
  jmp ReadDataSFX
:
  ldy #CHNVAR_NotePeriodWordSFX
  lda (CurChanDataPtr),y ;lo
  iny
  ora (CurChanDataPtr),y ;hi
  bne :+
  rts
:
  iny
  lda (CurChanDataPtr),y ;DutyCycleSFX
  ldy #$02
  cpy CurrentChannelNumber
  beq :+
  and #$0f
:
  sta $F4
  lda #CHNVAR_DutyCycleSFX | $80
  sta $F5
  jmp ProcessTremolo



ReadDataSFX: ;at 959E
  jsr ReadByteSFX
  asl
  bcs :+
  jmp CMDSFX_Select
:
  txa
  and #$0f
  cmp #$0f
  bne CMDSFX_SetNotePeriodSFX
CMDSFX_Exec_80:
  jsr ReadByteSFX
  jmp ResetPitchButSavePitchHiOld

CMDSFX_SetNotePeriodSFX: ;at 95B4; Byte was 8x
; The low 3 bits will be written to NotePeriodWordSFX+1
; And the following byte will be written to NotePeriodWordSFX+0

  and #$07
  sta $F4
  jsr ReadByteSFX

  ldy #CHNVAR_NotePeriodWordSFX
  sta (CurChanDataPtr),y
  iny
  lda $F4
  sta (CurChanDataPtr),y
  lda #CHNVAR_DutyCycleSFX
  sta $F4

  jsr ResetPitch
  jmp DisableChannel

CMDSFX_Select: ;at 95CE
  jsr DoCalculatedGoto ;
.word CMDSFX_SetFrameWaitCount
.word CMDSFX_SetVibratoCtrl
.word CMDSFX_SetDutyCycle
.word CMDSFX_SetVolumeAndEnv
.word CMDSFX_Loop
.word CMDSFX_SetVibratoBuffer
.word CMDSFX_EndOfData

CMDSFX_SetFrameWaitCount: ;at 95DF
  jsr ReadByteSFX
  sta SFX_FrameWaitCount
  jmp ReadDataSFX

CMDSFX_SetVibratoCtrl: ;at 95E7
;Stores the param into VibratoCtrlSFX
  jsr ReadByteSFX
  ldy #CHNVAR_VibratoCtrlSFX
  sta (CurChanDataPtr),y
  jmp ReadDataSFX

CMDSFX_SetDutyCycle: ;at 95F1
  jsr ReadByteSFX
  sta $F4
  ldy #CHNVAR_DutyCycleSFX
  lda (CurChanDataPtr),y
  and #$3f
  ora $F4
  jmp :+

CMDSFX_SetVolumeAndEnv: ;at 9601
  jsr ReadByteSFX
  ldy #$02
  cpy CurrentChannelNumber
  beq :+ ;omit for triangle channel
  sta $F4
  ldy #CHNVAR_DutyCycleSFX
  lda (CurChanDataPtr),y
  and #$C0
  ora $F4
:
  ldy #CHNVAR_DutyCycleSFX
  sta (CurChanDataPtr),y
  jmp ReadDataSFX

CMDSFX_Loop: ;at 961B
  jsr ReadByteSFX
  txa
  beq :+
  cpx SFX_LoopCounter
  beq :++
  inc SFX_LoopCounter
: 
  jsr ReadByteSFX
  sta $F4
  jsr ReadByteSFX
  sta SFX_Addr+1
  lda $F4
  sta SFX_Addr+0
  jmp ReadDataSFX
:
  lda #$00
  sta SFX_LoopCounter

  lda #$02
  clc
  adc SFX_Addr+0
  sta SFX_Addr+0
  lda #$00
  adc SFX_Addr+1
  sta SFX_Addr+1

  jmp ReadDataSFX

CMDSFX_SetVibratoBuffer: ;at 964C
  lda #CHNVAR_VibratoBuffer
  sta $F4
:
  jsr ReadByteSFX
  ldy $F4
  sta (CurChanDataPtr),y
  inc $F4
  ldy $F4
  cpy #$18
  bne :-

  jmp ReadDataSFX

CMDSFX_EndOfData: ;at 9662
  lda SFX_Addr+0 ;decrease SFX_Addr by 1
  sec
  sbc #$01
  sta SFX_Addr+0
  lda SFX_Addr+1
  sbc #$00
  sta SFX_Addr+1

; Set priority to zero
  lda Priorities
  and #$0f
  sta Priorities

  lda #$00
  sta SFX_ChannelUsage

  lda UnknownEF
  and #$fe
  sta UnknownEF

  ldy #CHNVAR_NotePeriodWord
  lda (CurChanDataPtr),y           ;If NotePeriodWord is nonzero?
  iny
  ora (CurChanDataPtr),y
  bne :+

  ldx CurrentChannelRegisterOffset ;NotePeriodWord was zero.
  inx
  inx
  ldy CurrentChannelNumber
  jsr MuteChannel

  ldy #CHNVAR_MusicDataAddr+0      ;Reads MusicDataAddr
  lda (CurChanDataPtr),y
  iny
  ora (CurChanDataPtr),y
  bne :+
  rts
:
  ldy #CHNVAR_VibratoDefs          ;
  lda (CurChanDataPtr),y
  and #$1f
  tax
  jsr CopySelectedVibratoData


  lda #CHNVAR_DutyCycle
  sta $F4
  jmp ResetPitch


;
; Reads one SFX byte and increments SFX addr
;

ReadByteSFX: ;at 96AC
  ldy #0
  lda (SFX_Addr),y
  tax
  lda #1
  clc
  adc SFX_Addr+0
  sta SFX_Addr+0
  lda #0
  adc SFX_Addr+1
  sta SFX_Addr+1
  txa
  rts



MUS_TickChannel: ;at 96C0
  lda SpeedUpFactor
  beq :++
; If SpeedUpFactor > 0, this function will be evaluated many times!
:
  pha
  jsr :+
  pla
  sec
  sbc #$01
  bne :-
  rts
:
  ldy #CHNVAR_LoopCounter  ; If the high bit of LoopCounter is set, the notes
  lda (CurChanDataPtr),y   ; will be played in 2/3 length. (Triplet speed)
  asl                    ; That is, delays are 2/3 of what they were.
  bcc :+
  lda SoundFrameCounter                               ; $EA
  and #$01
  beq :+
  jsr :+
:
  ldy #CHNVAR_NoteLengthWord
  lda (CurChanDataPtr),y
  iny
  ora (CurChanDataPtr),y
  beq :++                 ;If current note length is 0000?

; Wasn't 0000
  ldx #$FF
  dey
  lda (CurChanDataPtr),y    ; NoteLengthWord.lo -= 4
  sec
  sbc #$04
  sta (CurChanDataPtr),y
  txa   ;A=$FF
  iny
  adc (CurChanDataPtr),y    ; NoteLengthWord.hi += $FF+carry
  sta (CurChanDataPtr),y
  dey
  ora (CurChanDataPtr),y    ; Is the note length now 0000?
  beq :++
  ldy #CHNVAR_NotePeriodWord ; Nope, it's not 0000
  lda (CurChanDataPtr),y
  iny
  ora (CurChanDataPtr),y    ; Check if NotePeriodWord == 0000
  bne :+
  rts
:
  jmp IterateNote           ; No, NotePeriodWord is not 0000.

:
  ldy #CHNVAR_LoopCounter   ; Note length was 0.
  lda (CurChanDataPtr),y    ; Remove the high bit of LoopCounter
  and #$7F
  sta (CurChanDataPtr),y    ; And read more instructions

ReadDataMUS: ;at 9712
  jsr ReadByteMUS

  and #$f0
  bne :+
  jmp CMDMUS_Select
:
  cmp #$20
  bne :+

CMDMUS_20: ;at 9720
  txa
  and #$07    ; These go to high 3 bits of VibratoDefs
  pha
  jsr ReadDataMUS
  pla
  jmp CMDMUS_20_End
:
  cmp #$30
  bne :+
  jmp CMDMUS_TripletSpeed
:

; It's a note command(?)...
;
; X = the note. It is either in range 10..1F or 40..FF.

; Handles MUS notes
; Get the note length
  txa
  rol ;rol is like asl, but instead of 0 it feeds from Carry-flag.
  rol
  rol
  rol
  and #$07 ; .1..4567.. (huh?)
  tay
  lda NoteLengths_1,y  ; One of: 0,8,16,32,64

; NoteLength <- A * TickSpeed
  jsr SetNoteLengthWithTickSpeed

/*

If (VibratoDefs & 0xE0)
{
   VibratoDefs -= 0xE0;
   if(UnknownEF & 1) goto ResetPitchButSavePitchHiOld;
   return;
}
else
{
  Value = Parameter & 0x1F;
  if(Value == 0 or CurrentChannel == 1)
  {
    A,x = value,0
  }
  else
  {
    A,x = word ptr [NoteBaseOffset + Value*2]
  }
  NotePeriodWord = A,x
  ....
}


 */

SetupTone: ;at 9740
  ldy #CHNVAR_VibratoDefs
  lda (CurChanDataPtr),y
  and #$E0   ; Check 7 high bits of CHNVAR_VibratoDefs
  beq :++
  sec
  sbc #$20   ; Decrease the 7-bit value by one
  sta $F4
  lda (CurChanDataPtr),y
  and #$1f
  ora $F4
  sta (CurChanDataPtr),y ; Decrease the counter (CHNVAR_VibratoDefs)
  lda UnknownEF
  lsr
  bcc :+
  rts
:
  jmp ResetPitchButSavePitchHiOld
:
  txa
  and #$1f
  bne :+
  tax
  jmp :+++
:
  ldy #$01
  cpy CurrentChannelNumber
  bne :+
; We're processing channel#1 (noise?)
  ldx #$00
  jmp :++
:
  asl
  ldy #CHNVAR_NoteBaseOffset
  clc
  adc (CurChanDataPtr),y
  sta $F4
  lda #$00
  iny
  adc (CurChanDataPtr),y
  sta $F5
  ldy #$01     ;Loads the period from table
  lda ($F4),y  ;Hi
  tax
  dey
  lda ($F4),y  ;Lo
:
  ldy #CHNVAR_NotePeriodWord ;Stores into NotePeriodWord
  sta (CurChanDataPtr),y
  iny
  txa
  sta (CurChanDataPtr),y
  ldy #CHNVAR_VolumeCurveLength
  lda (CurChanDataPtr),y    ; $F4 <- VolumeCurveLength
  sta $F4
  and #$7f
  beq :+
  jsr ResetVolumeCurvePosition          ; Called if VolumeCurveLength had 7 low bits
:
  lda UnknownEF
  lsr
  bcc :+
  rts
:
  lda #$0C
  sta $F4
  jsr ResetPitch
  jmp DisableChannel

CMDMUS_20_End: ;at 97AE
  ror
  ror
  ror
  ror
  and #$E0        ; Set high 3 bits from param
  sta $F4
  ldy #CHNVAR_VibratoDefs
  lda (CurChanDataPtr),y
  and #$1f
  ora $F4
  sta (CurChanDataPtr),y
  rts


CMDMUS_TripletSpeed: ;at 97C1
  lda #$80
  ldy #CHNVAR_LoopCounter ; Set the high bit of LoopCounter
  ora (CurChanDataPtr),y  ; It will be handled at 96CF
  sta (CurChanDataPtr),y
  jmp ReadDataMUS

CMDMUS_Select: ;at 97CC
  jsr DoCalculatedGoto
.word CMDMUS_SetSpeed
.word CMDMUS_SetVibratoCtrl
.word CMDMUS_SetDutyCycle
.word CMDMUS_SetVolumeAndEnv
.word CMDMUS_Loop
.word CMDMUS_SetNoteBaseOffset
.word CMDMUS_PlayNoteExtraLength
.word CMDMUS_SetVolumeCurve
.word CMDMUS_SetVibratoIndex
.word CMDMUS_EndOfData

; Set tickspeed for this channel
CMDMUS_SetSpeed: ;at 97E3
  jsr ReadByteMUS
  ldy #CHNVAR_TickSpeed
  sta (CurChanDataPtr),y
  jmp ReadDataMUS

CMDMUS_SetVibratoCtrl: ;at 97ED
; Possibly parameters include: $00, $0F, $04, $2F
  jsr ReadByteMUS
  ldy #CHNVAR_VibratoCtrl
  sta (CurChanDataPtr),y
  jmp ReadDataMUS


; Set duty cycle for this channel
CMDMUS_SetDutyCycle: ;at 97F7
  jsr ReadByteMUS
  sta $F4
  ldy #CHNVAR_DutyCycle
  lda (CurChanDataPtr),y
  and #$3f
  ora $F4
  jmp :+
; Set volume and envelope/looping bits
CMDMUS_SetVolumeAndEnv: ;at 9807
  jsr ReadByteMUS
  ldy #$02 ;just use the value as-is for triangle
  cpy CurrentChannelNumber
  beq :+
  sta $F4
  ldy #CHNVAR_DutyCycle
  lda (CurChanDataPtr),y
  and #$C0
  ora $F4
:
  ldy #CHNVAR_DutyCycle
  sta (CurChanDataPtr),y
  jmp ReadDataMUS


CMDMUS_Loop: ;at 9821
  jsr ReadByteMUS
  txa
  beq :+
; Loop for a specified number of times...
  ldy #CHNVAR_LoopCounter
  lda (CurChanDataPtr),y
  and #$7F ; Remove the high bit, because it's used for something else
  sta $F4

  cpx $F4
  beq :++
  inc $F4
  lda (CurChanDataPtr),y
  and #$80
  ora $F4
  sta (CurChanDataPtr),y
:
; Load restart point
  jsr ReadByteMUS
  pha
  jsr ReadByteMUS
  pla
  ldy #CHNVAR_MusicDataAddr+0
  sta (CurChanDataPtr),y
  iny
  txa
  sta (CurChanDataPtr),y
  jmp ReadDataMUS

; Looped enough times(?)
:

; Skip addresses
  lda (CurChanDataPtr),y
  and #$80
  sta (CurChanDataPtr),y

  ldy #CHNVAR_MusicDataAddr+0
  lda #$02
  clc
  adc (CurChanDataPtr),y
  sta (CurChanDataPtr),y
  iny
  lda #$00
  adc (CurChanDataPtr),y
  sta (CurChanDataPtr),y
  jmp ReadDataMUS

CMDMUS_SetNoteBaseOffset: ;at 9869
/*
  NoteBaseOffset = CMDMUS_SetNoteBaseOffsettable + (parameter*2)
 */
  jsr ReadByteMUS
  ldx #<(CMDMUS_SetNoteBaseOffsettable)
  ldy #>(CMDMUS_SetNoteBaseOffsettable)
  stx $F4
  sty $F5
  asl
  ldy #CHNVAR_NoteBaseOffset
  clc
  adc $F4
  sta (CurChanDataPtr),y
  lda #$00
  adc $F5
  iny
  sta (CurChanDataPtr),y
  jmp ReadDataMUS

CMDMUS_PlayNoteExtraLength: ;at 9886
  jsr ReadByteMUS

; Y = upper 3 bits of music data byte in lower 3 bits
  rol
  rol
  rol
  rol
  and #$07
  tay

  lda NoteLengths_1half,y
  jsr SetNoteLengthWithTickSpeed
  jmp SetupTone


CMDMUS_SetVolumeCurve: ;at 9899
; First byte goes into VolumeCurveLength
; Second byte goes into VolumeCurveStrength
  jsr ReadByteMUS
  ldy #CHNVAR_VolumeCurveLength
  sta (CurChanDataPtr),y
  pha
  jsr ReadByteMUS
  ldy #CHNVAR_VolumeCurveStrength
  sta (CurChanDataPtr),y
  pla
  sta $F4

  and #$7f
  beq :+
  jsr ResetVolumeCurvePosition
:
  jmp ReadDataMUS

ResetVolumeCurvePosition: ;at 98B5
  lda #$00
  ldy #CHNVAR_VolumeCurvePosition
  sta (CurChanDataPtr),y     ; VolumeCurvePosition <- $00
  lda $F4
  bpl :+
  lda #$0f
  jmp :++
:
  lda #$00
:
  sta $F4
  ldy #CHNVAR_VolumeCurveStrength    ; VolumeCurvePosition <- (high bits) | $00  \_ either one
  lda (CurChanDataPtr),y             ; VolumeCurvePosition <- (high bits) | $0F  /  of these.
  and #$f0
  ora $F4
  sta (CurChanDataPtr),y
  rts


CMDMUS_SetVibratoIndex: ;at 98D3 Sets low 5 bits of VibratoDefs
  jsr ReadByteMUS
  sta $F4
  ldy #CHNVAR_VibratoDefs
  lda (CurChanDataPtr),y
  and #$E0
  ora $F4
  sta (CurChanDataPtr),y
  lda UnknownEF
  lsr
  bcs :+
  jsr CopySelectedVibratoData
:
  jmp ReadDataMUS

; A and X = note(?)

CopySelectedVibratoData: ;at 98ED
  txa
  beq :++
  lda #$00
:
  clc
  adc #$04
  dex
  bne :-
:
  clc
  adc VibratoBufferPointer+0
  sta $F4
  lda #$00
  adc VibratoBufferPointer+1
  sta $F5

  ldx #$00
  ldy #CHNVAR_VibratoBuffer+0
:
  lda ($F4,x)
  sta (CurChanDataPtr),y
  iny
  cpy #CHNVAR_VibratoBuffer+4
  bne :+
  rts
:
  lda #$01
  clc
  adc $F4
  sta $F4
  lda #$00
  adc $F5
  sta $F5
  jmp :--


CMDMUS_EndOfData: ;at 9923
  ldy #CHNVAR_MusicDataAddr+0    ; Set address to zero
  lda #$00
  sta (CurChanDataPtr),y
  iny
  sta (CurChanDataPtr),y

  lda Priorities
  and #$F0         ; keep sfx only
  sta Priorities
  lda UnknownEF
  lsr
  bcc :+
  rts
:
  ldx CurrentChannelRegisterOffset
  inx
  inx
  ldy CurrentChannelNumber
  jmp MuteChannel

; Read music data?...

ReadByteMUS: ;at 9941
  ldy #CHNVAR_MusicDataAddr+0
  lda (CurChanDataPtr),y
  sta $F4
  iny
  lda (CurChanDataPtr),y
  sta $F5
  dey

  lda ($F4),y
  tax
  lda #$01
  clc
  adc $F4
  sta (CurChanDataPtr),y
  lda #$00
  adc $F5
  iny
  sta (CurChanDataPtr),y
  txa
  rts


; Sets CHNVAR_NoteLengthWord as param * CHNVAR_TickSpeed
; Uses $F4..$F5 as temps

SetNoteLengthWithTickSpeed: ;at 9960
  sta $F4
  lda #$00
  sta $F5

  ldy #CHNVAR_TickSpeed
  lda (CurChanDataPtr),y
  tay

  lda #$00
:
  clc
  adc $F4
  bcc :+
  inc $F5
:
  dey
  bne :--

  ldy #CHNVAR_NoteLengthWord
  sta (CurChanDataPtr),y
  iny
  lda $F5
  sta (CurChanDataPtr),y
  rts


NoteLengths_1:
	.byt 0,0,2,4,8,16,32,64

NoteLengths_1half:
	.byt 0,0,3,6,12,24,48,96

CMDMUS_SetNoteBaseOffsettable:; at 9991
 
	; C# C  B  A#  |  F  F# G  G# | C  C# D  D#
	; A  G# G  F#  |  A  A# B  C  | E  F  F# G
	; F  E  D# D   |  C# D  D# E  | G# A  A# B
	.word $0000,$0000,$0000,$0000 ;Octave 0
	.word $0000,$0000,$0000,$0000
	.word $0000,$07F2,$07D6,$0714

	.word $06AE,$064E,$05F3,$059E ;Octave 1
	.word $054D,$0501,$04BB,$0475
	.word $0436,$03F9,$03BF,$038A

	.word $0357,$0327,$02FA,$02CF ;Octave 2
	.word $02A7,$0281,$025D,$023B
	.word $021A,$01FC,$01E0,$01C5 ;$01FC = A (21.47727e6/12/440 Hz/8)

	.word $01AB,$0193,$017D,$0167 ;Octave 3
	.word $0153,$0140,$012E,$011D ;$0153 = A (1.7897725e6/440 Hz/12)
	.word $010D,$00FE,$00F0,$00E2   ;(which one is it? I don't know!)

	.word $00D5,$00C9,$00BE,$00B3 ;Octave 4
	.word $00A9,$00A0,$0097,$008E
	.word $0086,$007F,$0078,$0071

	.word $006A,$0064,$005F,$0059 ;Octave 5
	.word $0054,$0050,$004B,$0047
	.word $0043,$003F,$003C,$0038

	.word $0035,$0032,$002F,$002C ;Octave 6
	.word $002A,$0028,$0025,$0023
	.word $0021,$001F,$001E,$001C

	.word $001A,$0019,$0017,$0016 ;Octave 7
	.word $0015,$0014,$0012,$0011
	.word $0010,$000F,$000F,$000E
	.word 65535,65535,65535,65535
	.word 65535,65535,65535
	.byt $FF

MusicAndSfxTable: ; at 9A60
 .word MUS_DrWilyDefeated             ; $9AC6 ;0 Wily defeated music
 .word MUS_StageSelect                ; $9C29 ;1 Stage select music 
 .word MUS_BossSelected               ; $9CBE ;2 Boss selected music
 .word MUS_GameOver                   ; $9D68 ;3 Game over music
 .word MUS_StageClear                 ; $9E08 ;4 Stage clear music
 .word MUS_Cutman                     ; $9F07 ;5 Cutman music
 .word MUS_Fireman                    ; $A28F ;6 Fireman music
 .word MUS_Fireman                    ; $A28F ;7 Fireman music
 .word MUS_Bombman                    ; $A4A7 ;8 Bombman music
 .word MUS_Elecman                    ; $A7F3 ;9 Elecman music
 .word MUS_Gutsman                    ; $AA75 ;A Gutsman music
 .word MUS_BossMusicWily              ; $AC69 ;B Wily stages boss music
 .word MUS_WilyStage1and2             ; $AD1F ;C Wily stage music #1   
 .word MUS_BossMusic                  ; $AF11 ;D Boss music
 .word MUS_Credits                    ; $B048 ;E Ending music
 .word MUS_Iceman                     ; $B4D3 ;F Iceman music
 .word MUS_WilyStage3and4             ; $B7AC ;10 Wily stage music #2
 .word SFX_WilySaucer                 ; $B898 ;11 Wily saucer sound  
 .word SFX_Bomb                       ; $B8BB ;12 Bomb sound
 .word SFX_Electricity                ; $B8E2 ;13 Electricity beam on Elecman's stage
 .word SFX_Megablaster                ; $B90F ;14
 .word SFX_EnemyBullet                ; $B921 ;15
 .word SFX_MegamanHit                 ; $B939 ;16
 .word SFX_ThrowBoulder               ; $B97C ;17
 .word SFX_MeterRefill                ; $B993 ;18
 .word SFX_HittingGround              ; $B9B2 ;19 Megaman landing on ground after jump
 .word SFX_BonusPearlPickup           ; $B9C8 ;1A
 .word SFX_BossHit                    ; $BA0F ;1B ;big noisy explosion
 .word SFX_StartPressedAtTitle        ; $BA34 ;1C
 .word SFX_1D                         ; $BA97 ;1D
 .word SFX_1E                         ; $BAAC ;1E
 .word SFX_MenuMove                   ; $BAD0 ;1F When stepping in weapons menu
 .word SFX_Transform                  ; $BAF9 ;20
 .word SFX_21                         ; $BB1D ;21
 .word SFX_PauseSound                 ; $BB30 ;22 Pause sound
 .word SFX_GutsmanLift                ; $BB6B ;23
 .word SFX_BossDoor                   ; $BB8A ;24
 .word SFX_Scissor                    ; $BBA9 ;25
 .word SFX_CutmanScissors             ; $BBDC ;26 ;cutman scissors
 .word SFX_27                         ; $BBFB ;27
 .word SFX_GutsmanStomp               ; $BC15 ;28 (not sure)
 .word SFX_BigEyeJump                 ; $BC47 ;29 telephone box jumps
 .word SFX_Icebeam                    ; $BC67 ;2A
 .word SFX_JumpIntoWater              ; $BC8A ;2B
 .word SFX_BlockReplicating           ; $BCAF ;2C
 .word SFX_2D                         ; $BCC5 ;2D
 .word SFX_2E                         ; $BCDA ;2E
 .word SFX_2F                         ; $BCEF ;2F
 .word SFX_Wind                       ; $BD05 ;30
 .word SFX_MegamanKilled              ; $BD4E ;31
 .word SFX_ExtraLife                  ; $BDBD ;32

MUS_DrWilyDefeated: ;at 9AC6
.scope
 .byt $0F ;priority. Lo=music priority, Hi=sfx priority
 .word MUS_DrWilyDefeated_chn0        ; $9AD1 ;
 .word MUS_DrWilyDefeated_chn1        ; $9B37 ;
 .word MUS_DrWilyDefeated_chn2        ; $9B9D ;
 .word MUS_DrWilyDefeated_chn3        ; $9BDF ;
 .word MUS_DrWilyDefeated_vibratotable ; $9C21 ;
MUS_DrWilyDefeated_chn0: ;at 9AD1
 .byt MUSCMD_SetSpeed,          $06
 .byt MUSCMD_SetDutyCycle,      $80
 .byt MUSCMD_SetVolumeAndEnv,   $3C
 .byt MUSCMD_SetVolumeCurve, $FF,$10
 .byt MUSCMD_SetNoteBaseOffset, 2*12+5
 .byt     $48 ;MUS note (C#3, length 2)
 .byt     $4C ;MUS note (F-3, length 2)
 .byt     $4F ;MUS note (G#3, length 2)
 .byt     $54 ;MUS note (C#4, length 2)
 .byt     $4C ;MUS note (F-3, length 2)
 .byt     $4F ;MUS note (G#3, length 2)
 .byt     $54 ;MUS note (C#4, length 2)
 .byt     $58 ;MUS note (F-4, length 2)
 .byt     $4F ;MUS note (G#3, length 2)
 .byt     $54 ;MUS note (C#4, length 2)
 .byt     $58 ;MUS note (F-4, length 2)
 .byt     $4B ;MUS note (E-3, length 2)
 .byt     $52 ;MUS note (B-3, length 2)
 .byt     $54 ;MUS note (C#4, length 2)
 .byt     $58 ;MUS note (F-4, length 2)
 .byt     $5B ;MUS note (G#4, length 2)
 .byt MUSCMD_SetNoteBaseOffset, 3*12+5
 .byt     $48 ;MUS note (C#4, length 2)
 .byt     $4C ;MUS note (F-4, length 2)
 .byt     $4F ;MUS note (G#4, length 2)
 .byt     $54 ;MUS note (C#5, length 2)
 .byt     $4C ;MUS note (F-4, length 2)
 .byt     $4F ;MUS note (G#4, length 2)
 .byt     $54 ;MUS note (C#5, length 2)
 .byt     $58 ;MUS note (F-5, length 2)
 .byt     $4F ;MUS note (G#4, length 2)
 .byt     $54 ;MUS note (C#5, length 2)
 .byt     $58 ;MUS note (F-5, length 2)
 .byt     $4B ;MUS note (E-4, length 2)
 .byt     $52 ;MUS note (B-4, length 2)
 .byt     $54 ;MUS note (C#5, length 2)
 .byt     $58 ;MUS note (F-5, length 2)
 .byt     $5B ;MUS note (G#5, length 2)
 .byt MUSCMD_SetSpeed,          $07
 .byt MUSCMD_SetNoteBaseOffset, 2*12+5
 .byt $06,$D4 ;MUS note (C#4, length 48)
 .byt $30,$80 ;Pause         (length 8/1.5) (triplet)
 .byt $30,$94 ;MUS note (C#4, length 8/1.5) (triplet)
 .byt $30,$94 ;MUS note (C#4, length 8/1.5) (triplet)
 .byt     $D7 ;MUS note (E-4, length 32)
 .byt     $D5 ;MUS note (D-4, length 32)
 .byt $06,$D4 ;MUS note (C#4, length 48)
 .byt $30,$80 ;Pause         (length 8/1.5) (triplet)
 .byt $30,$94 ;MUS note (C#4, length 8/1.5) (triplet)
 .byt $30,$94 ;MUS note (C#4, length 8/1.5) (triplet)
 .byt     $B7 ;MUS note (E-4, length 16)
 .byt $30,$80 ;Pause         (length 8/1.5) (triplet)
 .byt $30,$97 ;MUS note (E-4, length 8/1.5) (triplet)
 .byt $30,$97 ;MUS note (E-4, length 8/1.5) (triplet)
 .byt     $B9 ;MUS note (F#4, length 16)
 .byt $30,$80 ;Pause         (length 8/1.5) (triplet)
 .byt $30,$99 ;MUS note (F#4, length 8/1.5) (triplet)
 .byt $30,$99 ;MUS note (F#4, length 8/1.5) (triplet)
 .byt     $7B ;MUS note (G#4, length 4)
 .byt     $80 ;Pause         (length 8)
 .byt     $79 ;MUS note (F#4, length 4)
 .byt     $DB ;MUS note (G#4, length 32)
 .byt MUSCMD_SetSpeed,          $09
 .byt $30,$9B ;MUS note (G#4, length 8/1.5) (triplet)
 .byt $30,$9B ;MUS note (G#4, length 8/1.5) (triplet)
 .byt $30,$9B ;MUS note (G#4, length 8/1.5) (triplet)
 .byt MUSCMD_SetSpeed,          $0C
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $BD ;MUS note (A#4, length 16)
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt $06,$DD ;MUS note (A#4, length 48)
 .byt MUSCMD_EndOfData
MUS_DrWilyDefeated_chn1: ;at 9B37
 .byt MUSCMD_SetSpeed,          $06
 .byt MUSCMD_SetDutyCycle,      $80
 .byt MUSCMD_SetVolumeAndEnv,   $39
 .byt MUSCMD_SetVolumeCurve, $FF,$10
 .byt MUSCMD_SetNoteBaseOffset, 2*12+5
 .byt     $43 ;MUS note (G#2, length 2)
 .byt     $48 ;MUS note (C#3, length 2)
 .byt     $4C ;MUS note (F-3, length 2)
 .byt     $4F ;MUS note (G#3, length 2)
 .byt     $46 ;MUS note (B-2, length 2)
 .byt     $48 ;MUS note (C#3, length 2)
 .byt     $4C ;MUS note (F-3, length 2)
 .byt     $4F ;MUS note (G#3, length 2)
 .byt     $48 ;MUS note (C#3, length 2)
 .byt     $4C ;MUS note (F-3, length 2)
 .byt     $4F ;MUS note (G#3, length 2)
 .byt     $54 ;MUS note (C#4, length 2)
 .byt     $4C ;MUS note (F-3, length 2)
 .byt     $4F ;MUS note (G#3, length 2)
 .byt     $54 ;MUS note (C#4, length 2)
 .byt     $58 ;MUS note (F-4, length 2)
 .byt MUSCMD_SetNoteBaseOffset, 3*12+5
 .byt     $43 ;MUS note (G#3, length 2)
 .byt     $48 ;MUS note (C#4, length 2)
 .byt     $4C ;MUS note (F-4, length 2)
 .byt     $4F ;MUS note (G#4, length 2)
 .byt     $46 ;MUS note (B-3, length 2)
 .byt     $48 ;MUS note (C#4, length 2)
 .byt     $4C ;MUS note (F-4, length 2)
 .byt     $4F ;MUS note (G#4, length 2)
 .byt     $48 ;MUS note (C#4, length 2)
 .byt     $4C ;MUS note (F-4, length 2)
 .byt     $4F ;MUS note (G#4, length 2)
 .byt     $54 ;MUS note (C#5, length 2)
 .byt     $4C ;MUS note (F-4, length 2)
 .byt     $4F ;MUS note (G#4, length 2)
 .byt     $54 ;MUS note (C#5, length 2)
 .byt     $58 ;MUS note (F-5, length 2)
 .byt MUSCMD_SetSpeed,          $07
 .byt MUSCMD_SetNoteBaseOffset, 2*12+5
 .byt $06,$D1 ;MUS note (A#3, length 48)
 .byt $30,$80 ;Pause         (length 8/1.5) (triplet)
 .byt $30,$91 ;MUS note (A#3, length 8/1.5) (triplet)
 .byt $30,$91 ;MUS note (A#3, length 8/1.5) (triplet)
 .byt     $D4 ;MUS note (C#4, length 32)
 .byt     $D2 ;MUS note (B-3, length 32)
 .byt $06,$D1 ;MUS note (A#3, length 48)
 .byt $30,$80 ;Pause         (length 8/1.5) (triplet)
 .byt $30,$91 ;MUS note (A#3, length 8/1.5) (triplet)
 .byt $30,$91 ;MUS note (A#3, length 8/1.5) (triplet)
 .byt     $B4 ;MUS note (C#4, length 16)
 .byt $30,$80 ;Pause         (length 8/1.5) (triplet)
 .byt $30,$94 ;MUS note (C#4, length 8/1.5) (triplet)
 .byt $30,$94 ;MUS note (C#4, length 8/1.5) (triplet)
 .byt     $B5 ;MUS note (D-4, length 16)
 .byt $30,$80 ;Pause         (length 8/1.5) (triplet)
 .byt $30,$95 ;MUS note (D-4, length 8/1.5) (triplet)
 .byt $30,$95 ;MUS note (D-4, length 8/1.5) (triplet)
 .byt     $74 ;MUS note (C#4, length 4)
 .byt     $80 ;Pause         (length 8)
 .byt     $74 ;MUS note (C#4, length 4)
 .byt     $D4 ;MUS note (C#4, length 32)
 .byt MUSCMD_SetSpeed,          $09
 .byt $30,$94 ;MUS note (C#4, length 8/1.5) (triplet)
 .byt $30,$94 ;MUS note (C#4, length 8/1.5) (triplet)
 .byt $30,$94 ;MUS note (C#4, length 8/1.5) (triplet)
 .byt MUSCMD_SetSpeed,          $0C
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $B4 ;MUS note (C#4, length 16)
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt $06,$D4 ;MUS note (C#4, length 48)
 .byt MUSCMD_EndOfData
MUS_DrWilyDefeated_chn2: ;at 9B9D
 .byt MUSCMD_SetSpeed,          $06
 .byt MUSCMD_SetVibratoIndex,   $00
 .byt MUSCMD_SetVolumeAndEnv,   $81
 .byt MUSCMD_SetNoteBaseOffset, 2*12+5
 .byt     $E8 ;MUS note (C#3, length 64)
 .byt MUSCMD_SetSpeed,          $07
: ;at 9BA8
 .byt     $8D ;MUS note (F#3, length 8)
 .byt     $88 ;MUS note (C#3, length 8)
 .byt MUSCMD_Loop, 3 
 .word :-
: ;at 9BAE
 .byt     $8B ;MUS note (E-3, length 8)
 .byt MUSCMD_Loop, 3 
 .word :-
: ;at 9BB3
 .byt     $89 ;MUS note (D-3, length 8)
 .byt MUSCMD_Loop, 3 
 .word :-
: ;at 9BB8
 .byt     $8D ;MUS note (F#3, length 8)
 .byt     $88 ;MUS note (C#3, length 8)
 .byt MUSCMD_Loop, 3 
 .word :-
: ;at 9BBE
 .byt     $8B ;MUS note (E-3, length 8)
 .byt MUSCMD_Loop, 3 
 .word :-
: ;at 9BC3
 .byt     $89 ;MUS note (D-3, length 8)
 .byt MUSCMD_Loop, 3 
 .word :-
: ;at 9BC8
 .byt     $88 ;MUS note (C#3, length 8)
 .byt MUSCMD_Loop, 4 
 .word :-
 .byt MUSCMD_SetSpeed,          $09
 .byt     $80 ;Pause         (length 8)
 .byt $30,$88 ;MUS note (C#3, length 8/1.5) (triplet)
 .byt $30,$88 ;MUS note (C#3, length 8/1.5) (triplet)
 .byt $30,$88 ;MUS note (C#3, length 8/1.5) (triplet)
 .byt MUSCMD_SetSpeed,          $0C
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $AD ;MUS note (F#3, length 16)
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt $06,$CD ;MUS note (F#3, length 48)
 .byt MUSCMD_EndOfData
MUS_DrWilyDefeated_chn3: ;at 9BDF
 .byt MUSCMD_SetSpeed,          $06
 .byt MUSCMD_SetVolumeAndEnv,   $39
 .byt MUSCMD_SetVolumeCurve, $81,$10
 .byt     $E0 ;Pause         (length 64)
 .byt MUSCMD_SetSpeed,          $07
: ;at 9BE9
 .byt     $65 ;MUS note (A#2, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt $30,$65 ;MUS note (A#2, length 4/1.5) (triplet)
 .byt $30,$65 ;MUS note (A#2, length 4/1.5) (triplet)
 .byt $30,$65 ;MUS note (A#2, length 4/1.5) (triplet)
 .byt     $65 ;MUS note (A#2, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt $30,$65 ;MUS note (A#2, length 4/1.5) (triplet)
 .byt $30,$65 ;MUS note (A#2, length 4/1.5) (triplet)
 .byt $30,$65 ;MUS note (A#2, length 4/1.5) (triplet)
 .byt     $65 ;MUS note (A#2, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $65 ;MUS note (A#2, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $65 ;MUS note (A#2, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $65 ;MUS note (A#2, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt MUSCMD_Loop, 3 
 .word :-
: ;at 9C05
 .byt     $65 ;MUS note (A#2, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt $30,$65 ;MUS note (A#2, length 4/1.5) (triplet)
 .byt $30,$65 ;MUS note (A#2, length 4/1.5) (triplet)
 .byt $30,$65 ;MUS note (A#2, length 4/1.5) (triplet)
 .byt MUSCMD_Loop, 2 
 .word :-
 .byt MUSCMD_SetSpeed,          $09
: ;at 9C13
 .byt $30,$65 ;MUS note (A#2, length 4/1.5) (triplet)
 .byt MUSCMD_Loop, 5 
 .word :-
 .byt MUSCMD_SetSpeed,          $0C
: ;at 9C1B
 .byt     $45 ;MUS note (A#2, length 2)
 .byt MUSCMD_Loop, 24 
 .word :-
 .byt MUSCMD_EndOfData
MUS_DrWilyDefeated_vibratotable: ;at 9C21
 .byt $00, $E0, $80, $00 ;vibrato definition
 .byt $01, $62, $80, $00 ;vibrato definition
.endscope

MUS_StageSelect: ;at 9C29
.scope
 .byt $0F ;priority. Lo=music priority, Hi=sfx priority
 .word MUS_StageSelect_chn0           ; $9C34 ;
 .word MUS_StageSelect_chn1           ; $9C66 ;
 .word MUS_StageSelect_chn2           ; $9C98 ;
 .word $0000
 .word MUS_StageSelect_vibratotable   ; $9CBA ;
MUS_StageSelect_chn0: ;at 9C34
 .byt MUSCMD_SetSpeed,          $08
: ;at 9C36
 .byt MUSCMD_SetDutyCycle,      $C0
 .byt MUSCMD_SetVolumeAndEnv,   $38
 .byt MUSCMD_SetVibratoIndex,   $00
 .byt MUSCMD_SetVolumeCurve, $FF,$20
 .byt MUSCMD_SetNoteBaseOffset, 1*12+7
 .byt     $7D ;MUS note (C-4, length 4)
 .byt     $7D ;MUS note (C-4, length 4)
 .byt     $7D ;MUS note (C-4, length 4)
 .byt     $7B ;MUS note (A#3, length 4)
 .byt     $7B ;MUS note (A#3, length 4)
 .byt     $7B ;MUS note (A#3, length 4)
 .byt     $7E ;MUS note (C#4, length 4)
 .byt     $7E ;MUS note (C#4, length 4)
 .byt     $7E ;MUS note (C#4, length 4)
 .byt     $7D ;MUS note (C-4, length 4)
 .byt     $7D ;MUS note (C-4, length 4)
 .byt     $7D ;MUS note (C-4, length 4)
 .byt     $7B ;MUS note (A#3, length 4)
 .byt     $7B ;MUS note (A#3, length 4)
 .byt     $7D ;MUS note (C-4, length 4)
 .byt     $7D ;MUS note (C-4, length 4)
 .byt     $7D ;MUS note (C-4, length 4)
 .byt     $7D ;MUS note (C-4, length 4)
 .byt     $7D ;MUS note (C-4, length 4)
 .byt     $7B ;MUS note (A#3, length 4)
 .byt     $7B ;MUS note (A#3, length 4)
 .byt     $7B ;MUS note (A#3, length 4)
 .byt     $79 ;MUS note (G#3, length 4)
 .byt     $79 ;MUS note (G#3, length 4)
 .byt     $79 ;MUS note (G#3, length 4)
 .byt     $7B ;MUS note (A#3, length 4)
 .byt     $7B ;MUS note (A#3, length 4)
 .byt     $7B ;MUS note (A#3, length 4)
 .byt     $79 ;MUS note (G#3, length 4)
 .byt     $79 ;MUS note (G#3, length 4)
 .byt     $7B ;MUS note (A#3, length 4)
 .byt     $7B ;MUS note (A#3, length 4)
 .byt MUSCMD_Loop, 0 
 .word :-
 .byt MUSCMD_EndOfData
MUS_StageSelect_chn1: ;at 9C66
 .byt MUSCMD_SetSpeed,          $08
: ;at 9C68
 .byt MUSCMD_SetDutyCycle,      $C0
 .byt MUSCMD_SetVolumeAndEnv,   $38
 .byt MUSCMD_SetVolumeCurve, $FF,$20
 .byt MUSCMD_SetNoteBaseOffset, 1*12+7
 .byt MUSCMD_SetVibratoIndex,   $00
 .byt     $74 ;MUS note (D#3, length 4)
 .byt     $74 ;MUS note (D#3, length 4)
 .byt     $74 ;MUS note (D#3, length 4)
 .byt     $74 ;MUS note (D#3, length 4)
 .byt     $74 ;MUS note (D#3, length 4)
 .byt     $74 ;MUS note (D#3, length 4)
 .byt     $74 ;MUS note (D#3, length 4)
 .byt     $74 ;MUS note (D#3, length 4)
 .byt     $74 ;MUS note (D#3, length 4)
 .byt     $74 ;MUS note (D#3, length 4)
 .byt     $74 ;MUS note (D#3, length 4)
 .byt     $74 ;MUS note (D#3, length 4)
 .byt     $74 ;MUS note (D#3, length 4)
 .byt     $74 ;MUS note (D#3, length 4)
 .byt     $74 ;MUS note (D#3, length 4)
 .byt     $74 ;MUS note (D#3, length 4)
 .byt     $72 ;MUS note (C#3, length 4)
 .byt     $72 ;MUS note (C#3, length 4)
 .byt     $72 ;MUS note (C#3, length 4)
 .byt     $72 ;MUS note (C#3, length 4)
 .byt     $72 ;MUS note (C#3, length 4)
 .byt     $72 ;MUS note (C#3, length 4)
 .byt     $72 ;MUS note (C#3, length 4)
 .byt     $72 ;MUS note (C#3, length 4)
 .byt     $72 ;MUS note (C#3, length 4)
 .byt     $72 ;MUS note (C#3, length 4)
 .byt     $72 ;MUS note (C#3, length 4)
 .byt     $72 ;MUS note (C#3, length 4)
 .byt     $72 ;MUS note (C#3, length 4)
 .byt     $72 ;MUS note (C#3, length 4)
 .byt     $72 ;MUS note (C#3, length 4)
 .byt     $72 ;MUS note (C#3, length 4)
 .byt MUSCMD_Loop, 0 
 .word :-
 .byt MUSCMD_EndOfData
MUS_StageSelect_chn2: ;at 9C98
 .byt MUSCMD_SetSpeed,          $08
: ;at 9C9A
 .byt MUSCMD_SetVibratoIndex,   $00
 .byt MUSCMD_SetVolumeAndEnv,   $81
 .byt MUSCMD_SetNoteBaseOffset, 2*12+7
 .byt     $8D ;MUS note (G#3, length 8)
 .byt     $60 ;Pause         (length 4)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $6D ;MUS note (G#3, length 4)
 .byt     $CD ;MUS note (G#3, length 32)
 .byt     $80 ;Pause         (length 8)
 .byt     $6D ;MUS note (G#3, length 4)
 .byt     $6C ;MUS note (G-3, length 4)
 .byt     $8B ;MUS note (F#3, length 8)
 .byt     $60 ;Pause         (length 4)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $6B ;MUS note (F#3, length 4)
 .byt     $AB ;MUS note (F#3, length 16)
 .byt     $60 ;Pause         (length 4)
 .byt     $8B ;MUS note (F#3, length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $6B ;MUS note (F#3, length 4)
 .byt     $6B ;MUS note (F#3, length 4)
 .byt     $6A ;MUS note (F-3, length 4)
 .byt     $6B ;MUS note (F#3, length 4)
 .byt     $6C ;MUS note (G-3, length 4)
 .byt MUSCMD_Loop, 0 
 .word :-
 .byt MUSCMD_EndOfData
MUS_StageSelect_vibratotable: ;at 9CBA
 .byt $00, $E0, $80, $00 ;vibrato definition
.endscope

MUS_BossSelected: ;at 9CBE
.scope
 .byt $0F ;priority. Lo=music priority, Hi=sfx priority
 .word MUS_BossSelected_chn0          ; $9CC9 ;
 .word MUS_BossSelected_chn1          ; $9D05 ;
 .word MUS_BossSelected_chn2          ; $9D38 ;
 .word $0000
 .word MUS_BossSelected_vibratotable  ; $9D60 ;
MUS_BossSelected_chn0: ;at 9CC9
 .byt MUSCMD_SetSpeed,          $06
 .byt MUSCMD_SetDutyCycle,      $C0
 .byt MUSCMD_SetVolumeAndEnv,   $38
 .byt MUSCMD_SetVibratoIndex,   $00
 .byt MUSCMD_SetVolumeCurve, $FF,$80
 .byt MUSCMD_SetNoteBaseOffset, 2*12+6
 .byt     $80 ;Pause         (length 8)
 .byt     $6D ;MUS note (G-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $6D ;MUS note (G-3, length 4)
 .byt     $8B ;MUS note (F-3, length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $6D ;MUS note (G-3, length 4)
 .byt     $CD ;MUS note (G-3, length 32)
 .byt     $80 ;Pause         (length 8)
 .byt     $6F ;MUS note (A-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $6F ;MUS note (A-3, length 4)
 .byt     $8D ;MUS note (G-3, length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $6F ;MUS note (A-3, length 4)
 .byt     $CF ;MUS note (A-3, length 32)
 .byt     $70 ;MUS note (A#3, length 4)
 .byt     $70 ;MUS note (A#3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $70 ;MUS note (A#3, length 4)
 .byt     $A0 ;Pause         (length 16)
 .byt     $73 ;MUS note (C#4, length 4)
 .byt     $73 ;MUS note (C#4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $73 ;MUS note (C#4, length 4)
 .byt     $A0 ;Pause         (length 16)
 .byt     $60 ;Pause         (length 4)
 .byt     $74 ;MUS note (D-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $72 ;MUS note (C-4, length 4)
 .byt     $72 ;MUS note (C-4, length 4)
 .byt     $70 ;MUS note (A#3, length 4)
 .byt     $72 ;MUS note (C-4, length 4)
 .byt     $73 ;MUS note (C#4, length 4)
 .byt     $54 ;MUS note (D-4, length 2)
 .byt     $55 ;MUS note (D#4, length 2)
 .byt     $54 ;MUS note (D-4, length 2)
 .byt     $55 ;MUS note (D#4, length 2)
 .byt     $54 ;MUS note (D-4, length 2)
 .byt     $55 ;MUS note (D#4, length 2)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $54 ;MUS note (D-4, length 2)
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt     $B4 ;MUS note (D-4, length 16)
 .byt MUSCMD_EndOfData
MUS_BossSelected_chn1: ;at 9D05
 .byt MUSCMD_SetSpeed,          $06
 .byt MUSCMD_SetDutyCycle,      $C0
 .byt MUSCMD_SetVolumeAndEnv,   $38
 .byt MUSCMD_SetVolumeCurve, $FF,$10
 .byt MUSCMD_SetVibratoIndex,   $00
 .byt MUSCMD_SetNoteBaseOffset, 1*12+6
 .byt     $80 ;Pause         (length 8)
 .byt     $70 ;MUS note (A#2, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $70 ;MUS note (A#2, length 4)
 .byt     $8F ;MUS note (A-2, length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $70 ;MUS note (A#2, length 4)
 .byt     $D0 ;MUS note (A#2, length 32)
 .byt     $80 ;Pause         (length 8)
 .byt     $72 ;MUS note (C-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $72 ;MUS note (C-3, length 4)
 .byt     $90 ;MUS note (A#2, length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $72 ;MUS note (C-3, length 4)
 .byt     $D2 ;MUS note (C-3, length 32)
 .byt     $74 ;MUS note (D-3, length 4)
 .byt     $74 ;MUS note (D-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $74 ;MUS note (D-3, length 4)
 .byt     $A0 ;Pause         (length 16)
 .byt     $76 ;MUS note (E-3, length 4)
 .byt     $76 ;MUS note (E-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $76 ;MUS note (E-3, length 4)
 .byt     $A0 ;Pause         (length 16)
 .byt     $60 ;Pause         (length 4)
 .byt     $78 ;MUS note (F#3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $75 ;MUS note (D#3, length 4)
 .byt     $75 ;MUS note (D#3, length 4)
 .byt     $74 ;MUS note (D-3, length 4)
 .byt     $75 ;MUS note (D#3, length 4)
 .byt     $75 ;MUS note (D#3, length 4)
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt     $DB ;MUS note (A-3, length 32)
 .byt MUSCMD_EndOfData
MUS_BossSelected_chn2: ;at 9D38
 .byt MUSCMD_SetSpeed,          $06
 .byt MUSCMD_SetVibratoIndex,   $00
 .byt MUSCMD_SetVolumeAndEnv,   $81
 .byt MUSCMD_SetNoteBaseOffset, 2*12+6
 .byt $06,$CD ;MUS note (G-3, length 48)
 .byt     $6D ;MUS note (G-3, length 4)
 .byt $06,$8C ;MUS note (F#3, length 12)
 .byt $06,$CB ;MUS note (F-3, length 48)
 .byt     $6B ;MUS note (F-3, length 4)
 .byt $06,$8A ;MUS note (E-3, length 12)
 .byt     $69 ;MUS note (D#3, length 4)
 .byt     $69 ;MUS note (D#3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $69 ;MUS note (D#3, length 4)
 .byt     $80 ;Pause         (length 8)
 .byt     $40 ;Pause         (length 2)
 .byt     $49 ;MUS note (D#3, length 2)
 .byt     $4B ;MUS note (F-3, length 2)
 .byt     $4D ;MUS note (G-3, length 2)
 .byt     $6F ;MUS note (A-3, length 4)
 .byt     $6F ;MUS note (A-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $6F ;MUS note (A-3, length 4)
 .byt     $80 ;Pause         (length 8)
 .byt     $40 ;Pause         (length 2)
 .byt     $4F ;MUS note (A-3, length 2)
 .byt     $51 ;MUS note (B-3, length 2)
 .byt     $53 ;MUS note (C#4, length 2)
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt     $E8 ;MUS note (D-3, length 64)
 .byt MUSCMD_EndOfData
MUS_BossSelected_vibratotable: ;at 9D60
 .byt $00, $00, $80, $00 ;vibrato definition
 .byt $01, $42, $80, $00 ;vibrato definition
.endscope

MUS_GameOver: ;at 9D68
.scope
 .byt $0F ;priority. Lo=music priority, Hi=sfx priority
 .word MUS_GameOver_chn0              ; $9D73 ;
 .word MUS_GameOver_chn1              ; $9D95 ;
 .word MUS_GameOver_chn2              ; $9DB7 ;
 .word MUS_GameOver_chn3              ; $9DD8 ;
 .word MUS_GameOver_vibratotable      ; $9E00 ;
MUS_GameOver_chn0: ;at 9D73
 .byt MUSCMD_SetSpeed,          $06
 .byt MUSCMD_SetDutyCycle,      $80
 .byt MUSCMD_SetVolumeAndEnv,   $3A
 .byt MUSCMD_SetVolumeCurve, $C0,$D0
 .byt MUSCMD_SetNoteBaseOffset, 3*12+3
 .byt     $60 ;Pause         (length 4)
 .byt     $68 ;MUS note (B-3, length 4)
 .byt     $6A ;MUS note (C#4, length 4)
 .byt     $6D ;MUS note (E-4, length 4)
 .byt     $6A ;MUS note (C#4, length 4)
 .byt     $6D ;MUS note (E-4, length 4)
 .byt     $6F ;MUS note (F#4, length 4)
 .byt     $71 ;MUS note (G#4, length 4)
 .byt     $6D ;MUS note (E-4, length 4)
 .byt     $6F ;MUS note (F#4, length 4)
 .byt     $71 ;MUS note (G#4, length 4)
 .byt     $74 ;MUS note (B-4, length 4)
 .byt     $6F ;MUS note (F#4, length 4)
 .byt     $71 ;MUS note (G#4, length 4)
 .byt     $74 ;MUS note (B-4, length 4)
 .byt     $78 ;MUS note (D#5, length 4)
 .byt     $80 ;Pause         (length 8)
 .byt     $99 ;MUS note (E-5, length 8)
 .byt     $60 ;Pause         (length 4)
 .byt $06,$98 ;MUS note (D#5, length 12)
 .byt     $76 ;MUS note (C#5, length 4)
 .byt MUSCMD_EndOfData
MUS_GameOver_chn1: ;at 9D95
 .byt MUSCMD_SetSpeed,          $06
 .byt MUSCMD_SetDutyCycle,      $C0
 .byt MUSCMD_SetVolumeAndEnv,   $38
 .byt MUSCMD_SetVolumeCurve, $C0,$D0
 .byt MUSCMD_SetNoteBaseOffset, 3*12+3
 .byt     $60 ;Pause         (length 4)
 .byt     $65 ;MUS note (G#3, length 4)
 .byt     $68 ;MUS note (B-3, length 4)
 .byt     $6A ;MUS note (C#4, length 4)
 .byt     $68 ;MUS note (B-3, length 4)
 .byt     $6A ;MUS note (C#4, length 4)
 .byt     $6D ;MUS note (E-4, length 4)
 .byt     $6F ;MUS note (F#4, length 4)
 .byt     $6A ;MUS note (C#4, length 4)
 .byt     $6D ;MUS note (E-4, length 4)
 .byt     $6F ;MUS note (F#4, length 4)
 .byt     $71 ;MUS note (G#4, length 4)
 .byt     $6D ;MUS note (E-4, length 4)
 .byt     $6F ;MUS note (F#4, length 4)
 .byt     $71 ;MUS note (G#4, length 4)
 .byt     $74 ;MUS note (B-4, length 4)
 .byt     $80 ;Pause         (length 8)
 .byt     $91 ;MUS note (G#4, length 8)
 .byt     $60 ;Pause         (length 4)
 .byt $06,$95 ;MUS note (C-5, length 12)
 .byt     $6E ;MUS note (F-4, length 4)
 .byt MUSCMD_EndOfData
MUS_GameOver_chn2: ;at 9DB7
 .byt MUSCMD_SetSpeed,          $06
 .byt MUSCMD_SetVolumeAndEnv,   $81
 .byt MUSCMD_SetNoteBaseOffset, 2*12+3
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $CA ;MUS note (C#3, length 32)
 .byt     $8A ;MUS note (C#3, length 8)
 .byt     $4B ;MUS note (D-3, length 2)
 .byt     $4C ;MUS note (D#3, length 2)
 .byt     $4D ;MUS note (E-3, length 2)
 .byt     $4E ;MUS note (F-3, length 2)
 .byt     $4F ;MUS note (F#3, length 2)
 .byt     $50 ;MUS note (G-3, length 2)
 .byt     $51 ;MUS note (G#3, length 2)
 .byt     $52 ;MUS note (A-3, length 2)
 .byt     $53 ;MUS note (A#3, length 2)
 .byt     $54 ;MUS note (B-3, length 2)
 .byt     $55 ;MUS note (C-4, length 2)
 .byt     $56 ;MUS note (C#4, length 2)
 .byt     $40 ;Pause         (length 2)
 .byt     $4D ;MUS note (E-3, length 2)
 .byt     $4E ;MUS note (F-3, length 2)
 .byt     $4F ;MUS note (F#3, length 2)
 .byt     $91 ;MUS note (G#3, length 8)
 .byt     $60 ;Pause         (length 4)
 .byt $06,$91 ;MUS note (G#3, length 12)
 .byt     $6A ;MUS note (C#3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $6A ;MUS note (C#3, length 4)
 .byt MUSCMD_EndOfData
MUS_GameOver_chn3: ;at 9DD8
 .byt MUSCMD_SetSpeed,          $06
 .byt MUSCMD_SetVolumeAndEnv,   $36
 .byt MUSCMD_SetVolumeCurve, $82,$20
 .byt     $85 ;MUS note (G#2, length 8)
 .byt     $65 ;MUS note (G#2, length 4)
 .byt     $65 ;MUS note (G#2, length 4)
 .byt     $85 ;MUS note (G#2, length 8)
 .byt     $65 ;MUS note (G#2, length 4)
 .byt     $65 ;MUS note (G#2, length 4)
 .byt     $65 ;MUS note (G#2, length 4)
 .byt     $65 ;MUS note (G#2, length 4)
 .byt     $65 ;MUS note (G#2, length 4)
 .byt     $65 ;MUS note (G#2, length 4)
 .byt     $45 ;MUS note (G#2, length 2)
 .byt     $45 ;MUS note (G#2, length 2)
 .byt     $45 ;MUS note (G#2, length 2)
 .byt     $45 ;MUS note (G#2, length 2)
 .byt     $45 ;MUS note (G#2, length 2)
 .byt     $45 ;MUS note (G#2, length 2)
 .byt     $45 ;MUS note (G#2, length 2)
 .byt     $45 ;MUS note (G#2, length 2)
 .byt     $80 ;Pause         (length 8)
 .byt     $45 ;MUS note (G#2, length 2)
 .byt     $45 ;MUS note (G#2, length 2)
 .byt     $45 ;MUS note (G#2, length 2)
 .byt     $45 ;MUS note (G#2, length 2)
 .byt     $60 ;Pause         (length 4)
 .byt     $45 ;MUS note (G#2, length 2)
 .byt     $45 ;MUS note (G#2, length 2)
 .byt     $45 ;MUS note (G#2, length 2)
 .byt     $45 ;MUS note (G#2, length 2)
 .byt     $45 ;MUS note (G#2, length 2)
 .byt     $45 ;MUS note (G#2, length 2)
 .byt     $45 ;MUS note (G#2, length 2)
 .byt     $45 ;MUS note (G#2, length 2)
 .byt MUSCMD_EndOfData
MUS_GameOver_vibratotable: ;at 9E00
 .byt $00, $00, $80, $00 ;vibrato definition
 .byt $02, $42, $80, $00 ;vibrato definition
.endscope

MUS_StageClear: ;at 9E08
.scope
 .byt $0F ;priority. Lo=music priority, Hi=sfx priority
 .word MUS_StageClear_chn0            ; $9E13 ;
 .word MUS_StageClear_chn1            ; $9E52 ;
 .word MUS_StageClear_chn2            ; $9EA4 ;
 .word MUS_StageClear_chn3            ; $9EC1 ;
 .word MUS_StageClear_vibratotable    ; $9EFF ;
MUS_StageClear_chn0: ;at 9E13
 .byt MUSCMD_SetSpeed,          $04
 .byt MUSCMD_SetDutyCycle,      $C0
 .byt MUSCMD_SetVolumeAndEnv,   $3F
 .byt MUSCMD_SetVibratoIndex,   $00
 .byt MUSCMD_SetVolumeCurve, $A0,$20
 .byt MUSCMD_SetNoteBaseOffset, 2*12+1
: ;at 9E20
 .byt $30,$9B ;MUS note (E-4, length 8/1.5) (triplet)
 .byt $30,$96 ;MUS note (B-3, length 8/1.5) (triplet)
 .byt $30,$93 ;MUS note (G#3, length 8/1.5) (triplet)
 .byt $30,$8F ;MUS note (E-3, length 8/1.5) (triplet)
 .byt $30,$8A ;MUS note (B-2, length 8/1.5) (triplet)
 .byt $30,$87 ;MUS note (G#2, length 8/1.5) (triplet)
 .byt $30,$83 ;MUS note (E-2, length 8/1.5) (triplet)
 .byt $30,$87 ;MUS note (G#2, length 8/1.5) (triplet)
 .byt $30,$8A ;MUS note (B-2, length 8/1.5) (triplet)
 .byt $30,$8F ;MUS note (E-3, length 8/1.5) (triplet)
 .byt $30,$93 ;MUS note (G#3, length 8/1.5) (triplet)
 .byt $30,$96 ;MUS note (B-3, length 8/1.5) (triplet)
 .byt MUSCMD_Loop, 1 
 .word :-
 .byt MUSCMD_SetSpeed,          $06
 .byt MUSCMD_SetNoteBaseOffset, 3*12+1
 .byt     $6F ;MUS note (E-4, length 4)
 .byt     $80 ;Pause         (length 8)
 .byt     $6F ;MUS note (E-4, length 4)
 .byt MUSCMD_SetSpeed,          $07
 .byt     $AF ;MUS note (E-4, length 16)
 .byt $30,$B1 ;MUS note (F#4, length 16/1.5) (triplet)
 .byt $30,$AF ;MUS note (E-4, length 16/1.5) (triplet)
 .byt $30,$B1 ;MUS note (F#4, length 16/1.5) (triplet)
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt MUSCMD_SetSpeed,          $08
 .byt     $F3 ;MUS note (G#4, length 64)
 .byt MUSCMD_EndOfData
MUS_StageClear_chn1: ;at 9E52
 .byt MUSCMD_SetSpeed,          $04
 .byt MUSCMD_SetDutyCycle,      $C0
 .byt MUSCMD_SetVolumeAndEnv,   $3A
 .byt MUSCMD_SetVibratoIndex,   $00
 .byt MUSCMD_SetNoteBaseOffset, 3*12+1
 .byt MUSCMD_SetVolumeCurve, $A0,$20
 .byt     $60 ;Pause         (length 4)
 .byt $30,$9B ;MUS note (E-5, length 8/1.5) (triplet)
 .byt $30,$96 ;MUS note (B-4, length 8/1.5) (triplet)
 .byt $30,$93 ;MUS note (G#4, length 8/1.5) (triplet)
 .byt $30,$8F ;MUS note (E-4, length 8/1.5) (triplet)
 .byt $30,$8A ;MUS note (B-3, length 8/1.5) (triplet)
 .byt $30,$87 ;MUS note (G#3, length 8/1.5) (triplet)
 .byt $30,$83 ;MUS note (E-3, length 8/1.5) (triplet)
 .byt $30,$87 ;MUS note (G#3, length 8/1.5) (triplet)
 .byt $30,$8A ;MUS note (B-3, length 8/1.5) (triplet)
 .byt $30,$8F ;MUS note (E-4, length 8/1.5) (triplet)
 .byt $30,$93 ;MUS note (G#4, length 8/1.5) (triplet)
 .byt $30,$96 ;MUS note (B-4, length 8/1.5) (triplet)
 .byt $30,$9B ;MUS note (E-5, length 8/1.5) (triplet)
 .byt $30,$96 ;MUS note (B-4, length 8/1.5) (triplet)
 .byt $30,$93 ;MUS note (G#4, length 8/1.5) (triplet)
 .byt $30,$8F ;MUS note (E-4, length 8/1.5) (triplet)
 .byt $30,$8A ;MUS note (B-3, length 8/1.5) (triplet)
 .byt $30,$87 ;MUS note (G#3, length 8/1.5) (triplet)
 .byt $30,$83 ;MUS note (E-3, length 8/1.5) (triplet)
 .byt $30,$87 ;MUS note (G#3, length 8/1.5) (triplet)
 .byt $30,$8A ;MUS note (B-3, length 8/1.5) (triplet)
 .byt $30,$8F ;MUS note (E-4, length 8/1.5) (triplet)
 .byt $30,$93 ;MUS note (G#4, length 8/1.5) (triplet)
 .byt $30,$76 ;MUS note (B-4, length 4/1.5) (triplet)
 .byt MUSCMD_SetSpeed,          $06
 .byt     $77 ;MUS note (C-5, length 4)
 .byt     $80 ;Pause         (length 8)
 .byt     $77 ;MUS note (C-5, length 4)
 .byt MUSCMD_SetSpeed,          $07
 .byt     $B7 ;MUS note (C-5, length 16)
 .byt $30,$B9 ;MUS note (D-5, length 16/1.5) (triplet)
 .byt $30,$B7 ;MUS note (C-5, length 16/1.5) (triplet)
 .byt $30,$B9 ;MUS note (D-5, length 16/1.5) (triplet)
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt MUSCMD_SetSpeed,          $08
 .byt     $FB ;MUS note (E-5, length 64)
 .byt MUSCMD_EndOfData
MUS_StageClear_chn2: ;at 9EA4
 .byt MUSCMD_SetSpeed,          $04
 .byt MUSCMD_SetVibratoIndex,   $00
 .byt MUSCMD_SetNoteBaseOffset, 2*12+1
 .byt MUSCMD_SetVolumeAndEnv,   $81
 .byt     $EF ;MUS note (E-3, length 64)
 .byt     $ED ;MUS note (D-3, length 64)
 .byt MUSCMD_SetSpeed,          $06
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $AB ;MUS note (C-3, length 16)
 .byt MUSCMD_SetSpeed,          $07
 .byt     $AB ;MUS note (C-3, length 16)
 .byt $30,$AD ;MUS note (D-3, length 16/1.5) (triplet)
 .byt $30,$AD ;MUS note (D-3, length 16/1.5) (triplet)
 .byt $30,$AD ;MUS note (D-3, length 16/1.5) (triplet)
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt MUSCMD_SetSpeed,          $08
 .byt     $EF ;MUS note (E-3, length 64)
 .byt MUSCMD_EndOfData
MUS_StageClear_chn3: ;at 9EC1
 .byt MUSCMD_SetSpeed,          $04
 .byt MUSCMD_SetVolumeAndEnv,   $3A
 .byt MUSCMD_SetVolumeCurve, $82,$20
: ;at 9EC8
 .byt $30,$85 ;MUS note (F#2, length 8/1.5) (triplet)
 .byt $30,$A0 ;Pause         (length 16/1.5) (triplet)
 .byt $30,$85 ;MUS note (F#2, length 8/1.5) (triplet)
 .byt $30,$85 ;MUS note (F#2, length 8/1.5) (triplet)
 .byt $30,$85 ;MUS note (F#2, length 8/1.5) (triplet)
 .byt     $85 ;MUS note (F#2, length 8)
 .byt     $85 ;MUS note (F#2, length 8)
 .byt     $85 ;MUS note (F#2, length 8)
 .byt     $85 ;MUS note (F#2, length 8)
 .byt MUSCMD_Loop, 1 
 .word :-
 .byt MUSCMD_SetSpeed,          $06
 .byt $30,$85 ;MUS note (F#2, length 8/1.5) (triplet)
 .byt $30,$85 ;MUS note (F#2, length 8/1.5) (triplet)
 .byt $30,$85 ;MUS note (F#2, length 8/1.5) (triplet)
 .byt MUSCMD_SetSpeed,          $07
 .byt $30,$85 ;MUS note (F#2, length 8/1.5) (triplet)
 .byt $30,$85 ;MUS note (F#2, length 8/1.5) (triplet)
 .byt $30,$85 ;MUS note (F#2, length 8/1.5) (triplet)
 .byt $30,$A5 ;MUS note (F#2, length 16/1.5) (triplet)
 .byt $30,$A5 ;MUS note (F#2, length 16/1.5) (triplet)
 .byt $30,$A5 ;MUS note (F#2, length 16/1.5) (triplet)
: ;at 9EF0
 .byt MUSCMD_SetSpeed,          $08
 .byt     $45 ;MUS note (F#2, length 2)
 .byt     $45 ;MUS note (F#2, length 2)
 .byt     $45 ;MUS note (F#2, length 2)
 .byt     $45 ;MUS note (F#2, length 2)
 .byt     $45 ;MUS note (F#2, length 2)
 .byt     $45 ;MUS note (F#2, length 2)
 .byt     $45 ;MUS note (F#2, length 2)
 .byt     $45 ;MUS note (F#2, length 2)
 .byt MUSCMD_Loop, 1 
 .word :-
 .byt MUSCMD_EndOfData
MUS_StageClear_vibratotable: ;at 9EFF
 .byt $00, $E0, $80, $00 ;vibrato definition
 .byt $02, $22, $80, $00 ;vibrato definition
.endscope

MUS_Cutman: ;at 9F07
.scope
 .byt $0F ;priority. Lo=music priority, Hi=sfx priority
 .word MUS_Cutman_chn0                ; $9F12 ;
 .word MUS_Cutman_chn1                ; $A06A ;
 .word MUS_Cutman_chn2                ; $A184 ;
 .word MUS_Cutman_chn3                ; $A25E ;
 .word MUS_Cutman_vibratotable        ; $A283 ;
MUS_Cutman_chn0: ;at 9F12
 .byt MUSCMD_SetSpeed,          $06
 .byt MUSCMD_SetDutyCycle,      $40
 .byt MUSCMD_SetVolumeAndEnv,   $3A
 .byt MUSCMD_SetVolumeCurve, $FF,$40
 .byt MUSCMD_SetNoteBaseOffset, 2*12+4
 .byt     $98 ;MUS note (E-4, length 8)
 .byt     $98 ;MUS note (E-4, length 8)
 .byt     $98 ;MUS note (E-4, length 8)
 .byt     $78 ;MUS note (E-4, length 4)
 .byt     $78 ;MUS note (E-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $78 ;MUS note (E-4, length 4)
 .byt     $78 ;MUS note (E-4, length 4)
 .byt     $78 ;MUS note (E-4, length 4)
 .byt     $98 ;MUS note (E-4, length 8)
 .byt     $98 ;MUS note (E-4, length 8)
 .byt     $98 ;MUS note (E-4, length 8)
 .byt     $98 ;MUS note (E-4, length 8)
 .byt     $78 ;MUS note (E-4, length 4)
 .byt     $78 ;MUS note (E-4, length 4)
 .byt     $78 ;MUS note (E-4, length 4)
 .byt     $78 ;MUS note (E-4, length 4)
 .byt     $78 ;MUS note (E-4, length 4)
 .byt     $78 ;MUS note (E-4, length 4)
 .byt     $78 ;MUS note (E-4, length 4)
 .byt     $78 ;MUS note (E-4, length 4)
 .byt     $9A ;MUS note (F#4, length 8)
 .byt     $9C ;MUS note (G#4, length 8)
: ;at 9F34
 .byt MUSCMD_SetVolumeAndEnv,   $39
 .byt MUSCMD_SetDutyCycle,      $40
 .byt MUSCMD_SetVolumeCurve, $CF,$40
 .byt MUSCMD_SetNoteBaseOffset, 3*12+4
 .byt     $51 ;MUS note (A-4, length 2)
 .byt     $5D ;MUS note (A-5, length 2)
 .byt     $40 ;Pause         (length 2)
 .byt MUSCMD_SetVolumeAndEnv,   $39
 .byt     $5D ;MUS note (A-5, length 2)
 .byt MUSCMD_SetNoteBaseOffset, 2*12+4
 .byt MUSCMD_SetVolumeAndEnv,   $3A
 .byt     $A0 ;Pause         (length 16)
 .byt     $80 ;Pause         (length 8)
 .byt     $73 ;MUS note (B-3, length 4)
 .byt     $74 ;MUS note (C-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $76 ;MUS note (D-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $74 ;MUS note (C-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $73 ;MUS note (B-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $74 ;MUS note (C-4, length 4)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $8C ;MUS note (E-3, length 8)
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt $06,$CC ;MUS note (E-3, length 48)
 .byt MUSCMD_SetVibratoIndex,   $00
 .byt MUSCMD_SetNoteBaseOffset, 3*12+4
 .byt     $51 ;MUS note (A-4, length 2)
 .byt     $5D ;MUS note (A-5, length 2)
 .byt     $40 ;Pause         (length 2)
 .byt MUSCMD_SetVolumeAndEnv,   $39
 .byt     $5D ;MUS note (A-5, length 2)
 .byt MUSCMD_SetNoteBaseOffset, 2*12+4
 .byt MUSCMD_SetVolumeAndEnv,   $3A
 .byt     $A0 ;Pause         (length 16)
 .byt     $80 ;Pause         (length 8)
 .byt     $73 ;MUS note (B-3, length 4)
 .byt     $74 ;MUS note (C-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $76 ;MUS note (D-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $74 ;MUS note (C-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $73 ;MUS note (B-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $76 ;MUS note (D-4, length 4)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $98 ;MUS note (E-4, length 8)
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt $06,$D8 ;MUS note (E-4, length 48)
 .byt MUSCMD_SetVibratoIndex,   $00
 .byt     $79 ;MUS note (F-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $80 ;Pause         (length 8)
 .byt     $A0 ;Pause         (length 16)
 .byt     $79 ;MUS note (F-4, length 4)
 .byt     $7B ;MUS note (G-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $79 ;MUS note (F-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $78 ;MUS note (E-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $76 ;MUS note (D-4, length 4)
 .byt     $78 ;MUS note (E-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $80 ;Pause         (length 8)
 .byt     $A0 ;Pause         (length 16)
 .byt     $78 ;MUS note (E-4, length 4)
 .byt     $79 ;MUS note (F-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $78 ;MUS note (E-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $76 ;MUS note (D-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $74 ;MUS note (C-4, length 4)
 .byt     $97 ;MUS note (D#4, length 8)
 .byt     $A0 ;Pause         (length 16)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $93 ;MUS note (B-3, length 8)
 .byt     $93 ;MUS note (B-3, length 8)
 .byt     $93 ;MUS note (B-3, length 8)
 .byt     $95 ;MUS note (C#4, length 8)
 .byt     $97 ;MUS note (D#4, length 8)
 .byt     $F8 ;MUS note (E-4, length 64)
 .byt MUSCMD_SetNoteBaseOffset, 3*12+4
 .byt     $51 ;MUS note (A-4, length 2)
 .byt     $5D ;MUS note (A-5, length 2)
 .byt     $40 ;Pause         (length 2)
 .byt MUSCMD_SetVolumeAndEnv,   $39
 .byt     $5D ;MUS note (A-5, length 2)
 .byt MUSCMD_SetVolumeAndEnv,   $3A
 .byt MUSCMD_SetNoteBaseOffset, 2*12+4
 .byt     $A0 ;Pause         (length 16)
 .byt     $80 ;Pause         (length 8)
 .byt     $73 ;MUS note (B-3, length 4)
 .byt     $74 ;MUS note (C-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $76 ;MUS note (D-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $74 ;MUS note (C-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $73 ;MUS note (B-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $74 ;MUS note (C-4, length 4)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $8C ;MUS note (E-3, length 8)
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt $06,$CC ;MUS note (E-3, length 48)
 .byt MUSCMD_SetVibratoIndex,   $00
 .byt MUSCMD_SetNoteBaseOffset, 3*12+4
 .byt     $51 ;MUS note (A-4, length 2)
 .byt     $5D ;MUS note (A-5, length 2)
 .byt     $40 ;Pause         (length 2)
 .byt MUSCMD_SetVolumeAndEnv,   $37
 .byt     $5D ;MUS note (A-5, length 2)
 .byt MUSCMD_SetVolumeAndEnv,   $39
 .byt MUSCMD_SetNoteBaseOffset, 2*12+4
 .byt     $A0 ;Pause         (length 16)
 .byt     $80 ;Pause         (length 8)
 .byt     $73 ;MUS note (B-3, length 4)
 .byt     $74 ;MUS note (C-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $76 ;MUS note (D-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $74 ;MUS note (C-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $73 ;MUS note (B-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $76 ;MUS note (D-4, length 4)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $98 ;MUS note (E-4, length 8)
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt $06,$D8 ;MUS note (E-4, length 48)
 .byt MUSCMD_SetVibratoIndex,   $00
 .byt     $79 ;MUS note (F-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $80 ;Pause         (length 8)
 .byt     $A0 ;Pause         (length 16)
 .byt     $79 ;MUS note (F-4, length 4)
 .byt     $7B ;MUS note (G-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $79 ;MUS note (F-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $78 ;MUS note (E-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $76 ;MUS note (D-4, length 4)
 .byt     $78 ;MUS note (E-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $80 ;Pause         (length 8)
 .byt     $A0 ;Pause         (length 16)
 .byt     $78 ;MUS note (E-4, length 4)
 .byt     $79 ;MUS note (F-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $78 ;MUS note (E-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $76 ;MUS note (D-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $74 ;MUS note (C-4, length 4)
 .byt     $93 ;MUS note (B-3, length 8)
 .byt     $A0 ;Pause         (length 16)
 .byt     $B1 ;MUS note (A-3, length 16)
 .byt     $90 ;MUS note (G#3, length 8)
 .byt     $8E ;MUS note (F#3, length 8)
 .byt     $90 ;MUS note (G#3, length 8)
 .byt     $D1 ;MUS note (A-3, length 32)
 .byt MUSCMD_SetNoteBaseOffset, 2*12+4
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt $30,$45 ;MUS note (A-2, length 2/1.5) (triplet)
 .byt $30,$47 ;MUS note (B-2, length 2/1.5) (triplet)
 .byt $30,$48 ;MUS note (C-3, length 2/1.5) (triplet)
 .byt $30,$49 ;MUS note (C#3, length 2/1.5) (triplet)
 .byt $30,$4A ;MUS note (D-3, length 2/1.5) (triplet)
 .byt $30,$4B ;MUS note (D#3, length 2/1.5) (triplet)
 .byt $30,$4C ;MUS note (E-3, length 2/1.5) (triplet)
 .byt $30,$4D ;MUS note (F-3, length 2/1.5) (triplet)
 .byt $30,$4E ;MUS note (F#3, length 2/1.5) (triplet)
 .byt $30,$4F ;MUS note (G-3, length 2/1.5) (triplet)
 .byt $30,$50 ;MUS note (G#3, length 2/1.5) (triplet)
 .byt $30,$51 ;MUS note (A-3, length 2/1.5) (triplet)
 .byt $30,$52 ;MUS note (A#3, length 2/1.5) (triplet)
 .byt $30,$53 ;MUS note (B-3, length 2/1.5) (triplet)
 .byt $30,$54 ;MUS note (C-4, length 2/1.5) (triplet)
 .byt $30,$55 ;MUS note (C#4, length 2/1.5) (triplet)
 .byt $30,$56 ;MUS note (D-4, length 2/1.5) (triplet)
 .byt $30,$57 ;MUS note (D#4, length 2/1.5) (triplet)
 .byt $30,$58 ;MUS note (E-4, length 2/1.5) (triplet)
 .byt $30,$59 ;MUS note (F-4, length 2/1.5) (triplet)
 .byt $30,$5A ;MUS note (F#4, length 2/1.5) (triplet)
 .byt $30,$5B ;MUS note (G-4, length 2/1.5) (triplet)
 .byt $30,$5C ;MUS note (G#4, length 2/1.5) (triplet)
 .byt $30,$5D ;MUS note (A-4, length 2/1.5) (triplet)
 .byt MUSCMD_SetVibratoIndex,   $02
 .byt MUSCMD_SetDutyCycle,      $40
 .byt MUSCMD_SetVolumeCurve, $FF,$10
 .byt MUSCMD_SetVolumeAndEnv,   $39
 .byt MUSCMD_SetNoteBaseOffset, 3*12+4
 .byt     $D1 ;MUS note (A-4, length 32)
 .byt     $80 ;Pause         (length 8)
 .byt     $B3 ;MUS note (B-4, length 16)
 .byt     $91 ;MUS note (A-4, length 8)
 .byt MUSCMD_SetNoteBaseOffset, 2*12+4
 .byt     $FB ;MUS note (G-4, length 64)
 .byt     $D9 ;MUS note (F-4, length 32)
 .byt     $80 ;Pause         (length 8)
 .byt     $BB ;MUS note (G-4, length 16)
 .byt     $99 ;MUS note (F-4, length 8)
 .byt     $98 ;MUS note (E-4, length 8)
 .byt     $99 ;MUS note (F-4, length 8)
 .byt     $9A ;MUS note (F#4, length 8)
 .byt     $9B ;MUS note (G-4, length 8)
 .byt     $C0 ;Pause         (length 32)
 .byt     $DD ;MUS note (A-4, length 32)
 .byt     $80 ;Pause         (length 8)
 .byt MUSCMD_SetNoteBaseOffset, 3*12+4
 .byt     $B3 ;MUS note (B-4, length 16)
 .byt     $91 ;MUS note (A-4, length 8)
 .byt     $8F ;MUS note (G-4, length 8)
 .byt     $8F ;MUS note (G-4, length 8)
 .byt     $96 ;MUS note (D-5, length 8)
 .byt $06,$B4 ;MUS note (C-5, length 24)
 .byt     $A0 ;Pause         (length 16)
 .byt     $71 ;MUS note (A-4, length 4)
 .byt     $71 ;MUS note (A-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $71 ;MUS note (A-4, length 4)
 .byt     $71 ;MUS note (A-4, length 4)
 .byt     $71 ;MUS note (A-4, length 4)
 .byt     $71 ;MUS note (A-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $71 ;MUS note (A-4, length 4)
 .byt     $91 ;MUS note (A-4, length 8)
 .byt     $71 ;MUS note (A-4, length 4)
 .byt     $93 ;MUS note (B-4, length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $91 ;MUS note (A-4, length 8)
 .byt     $D1 ;MUS note (A-4, length 32)
 .byt     $D0 ;MUS note (G#4, length 32)
 .byt MUSCMD_Loop, 0 
 .word :-
MUS_Cutman_chn1: ;at A06A
 .byt MUSCMD_SetSpeed,          $06
 .byt MUSCMD_SetVolumeAndEnv,   $37
 .byt MUSCMD_SetDutyCycle,      $40
 .byt MUSCMD_SetVolumeCurve, $FF,$40
 .byt MUSCMD_SetNoteBaseOffset, 2*12+4
 .byt     $90 ;MUS note (G#3, length 8)
 .byt     $90 ;MUS note (G#3, length 8)
 .byt     $90 ;MUS note (G#3, length 8)
 .byt     $70 ;MUS note (G#3, length 4)
 .byt     $70 ;MUS note (G#3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $70 ;MUS note (G#3, length 4)
 .byt     $70 ;MUS note (G#3, length 4)
 .byt     $70 ;MUS note (G#3, length 4)
 .byt     $90 ;MUS note (G#3, length 8)
 .byt     $90 ;MUS note (G#3, length 8)
 .byt     $90 ;MUS note (G#3, length 8)
 .byt     $90 ;MUS note (G#3, length 8)
 .byt     $70 ;MUS note (G#3, length 4)
 .byt     $70 ;MUS note (G#3, length 4)
 .byt     $70 ;MUS note (G#3, length 4)
 .byt     $70 ;MUS note (G#3, length 4)
 .byt     $70 ;MUS note (G#3, length 4)
 .byt     $70 ;MUS note (G#3, length 4)
 .byt     $70 ;MUS note (G#3, length 4)
 .byt     $70 ;MUS note (G#3, length 4)
 .byt     $91 ;MUS note (A-3, length 8)
 .byt     $93 ;MUS note (B-3, length 8)
: ;at A08C
 .byt MUSCMD_SetVolumeAndEnv,   $36
 .byt MUSCMD_SetDutyCycle,      $40
 .byt MUSCMD_SetVolumeCurve, $CF,$40
 .byt MUSCMD_SetVibratoIndex,   $00
: ;at A095
 .byt MUSCMD_SetNoteBaseOffset, 2*12+4
 .byt     $71 ;MUS note (A-3, length 4)
 .byt     $71 ;MUS note (A-3, length 4)
 .byt     $6C ;MUS note (E-3, length 4)
 .byt     $6F ;MUS note (G-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $71 ;MUS note (A-3, length 4)
 .byt     $6C ;MUS note (E-3, length 4)
 .byt     $6F ;MUS note (G-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $71 ;MUS note (A-3, length 4)
 .byt     $6C ;MUS note (E-3, length 4)
 .byt     $6F ;MUS note (G-3, length 4)
 .byt     $71 ;MUS note (A-3, length 4)
 .byt     $71 ;MUS note (A-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $71 ;MUS note (A-3, length 4)
 .byt MUSCMD_Loop, 3 
 .word :-
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $65 ;MUS note (A-2, length 4)
 .byt     $68 ;MUS note (C-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $65 ;MUS note (A-2, length 4)
 .byt     $68 ;MUS note (C-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $65 ;MUS note (A-2, length 4)
 .byt     $68 ;MUS note (C-3, length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $71 ;MUS note (A-3, length 4)
 .byt     $71 ;MUS note (A-3, length 4)
 .byt     $6C ;MUS note (E-3, length 4)
 .byt     $6F ;MUS note (G-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $71 ;MUS note (A-3, length 4)
 .byt     $6C ;MUS note (E-3, length 4)
 .byt     $6F ;MUS note (G-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $71 ;MUS note (A-3, length 4)
 .byt     $6C ;MUS note (E-3, length 4)
 .byt     $6F ;MUS note (G-3, length 4)
 .byt     $71 ;MUS note (A-3, length 4)
 .byt     $71 ;MUS note (A-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $71 ;MUS note (A-3, length 4)
 .byt     $67 ;MUS note (B-2, length 4)
 .byt     $67 ;MUS note (B-2, length 4)
 .byt     $62 ;MUS note (F#2, length 4)
 .byt     $65 ;MUS note (A-2, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $67 ;MUS note (B-2, length 4)
 .byt     $62 ;MUS note (F#2, length 4)
 .byt     $65 ;MUS note (A-2, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $67 ;MUS note (B-2, length 4)
 .byt     $62 ;MUS note (F#2, length 4)
 .byt     $65 ;MUS note (A-2, length 4)
 .byt     $67 ;MUS note (B-2, length 4)
 .byt     $67 ;MUS note (B-2, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $67 ;MUS note (B-2, length 4)
 .byt     $6C ;MUS note (E-3, length 4)
 .byt     $6C ;MUS note (E-3, length 4)
 .byt     $67 ;MUS note (B-2, length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $6C ;MUS note (E-3, length 4)
 .byt     $67 ;MUS note (B-2, length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $6C ;MUS note (E-3, length 4)
 .byt     $67 ;MUS note (B-2, length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $6C ;MUS note (E-3, length 4)
 .byt     $6C ;MUS note (E-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt MUSCMD_SetNoteBaseOffset, 2*12+4
 .byt     $6C ;MUS note (E-3, length 4)
: ;at A0ED
 .byt MUSCMD_SetNoteBaseOffset, 2*12+4
 .byt     $71 ;MUS note (A-3, length 4)
 .byt     $71 ;MUS note (A-3, length 4)
 .byt     $6C ;MUS note (E-3, length 4)
 .byt     $6F ;MUS note (G-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $71 ;MUS note (A-3, length 4)
 .byt     $6C ;MUS note (E-3, length 4)
 .byt     $6F ;MUS note (G-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $71 ;MUS note (A-3, length 4)
 .byt     $6C ;MUS note (E-3, length 4)
 .byt     $6F ;MUS note (G-3, length 4)
 .byt     $71 ;MUS note (A-3, length 4)
 .byt     $71 ;MUS note (A-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $71 ;MUS note (A-3, length 4)
 .byt MUSCMD_Loop, 3 
 .word :-
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $65 ;MUS note (A-2, length 4)
 .byt     $68 ;MUS note (C-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $65 ;MUS note (A-2, length 4)
 .byt     $68 ;MUS note (C-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $65 ;MUS note (A-2, length 4)
 .byt     $68 ;MUS note (C-3, length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $71 ;MUS note (A-3, length 4)
 .byt     $71 ;MUS note (A-3, length 4)
 .byt     $6C ;MUS note (E-3, length 4)
 .byt     $6F ;MUS note (G-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $71 ;MUS note (A-3, length 4)
 .byt     $6C ;MUS note (E-3, length 4)
 .byt     $6F ;MUS note (G-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $71 ;MUS note (A-3, length 4)
 .byt     $6C ;MUS note (E-3, length 4)
 .byt     $6F ;MUS note (G-3, length 4)
 .byt     $71 ;MUS note (A-3, length 4)
 .byt     $71 ;MUS note (A-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $71 ;MUS note (A-3, length 4)
 .byt     $67 ;MUS note (B-2, length 4)
 .byt     $67 ;MUS note (B-2, length 4)
 .byt     $62 ;MUS note (F#2, length 4)
 .byt     $65 ;MUS note (A-2, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $67 ;MUS note (B-2, length 4)
 .byt     $62 ;MUS note (F#2, length 4)
 .byt     $65 ;MUS note (A-2, length 4)
 .byt     $6C ;MUS note (E-3, length 4)
 .byt     $6C ;MUS note (E-3, length 4)
 .byt     $67 ;MUS note (B-2, length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $6C ;MUS note (E-3, length 4)
 .byt     $6C ;MUS note (E-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $6C ;MUS note (E-3, length 4)
 .byt     $71 ;MUS note (A-3, length 4)
 .byt     $71 ;MUS note (A-3, length 4)
 .byt     $6C ;MUS note (E-3, length 4)
 .byt     $6F ;MUS note (G-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $71 ;MUS note (A-3, length 4)
 .byt     $6C ;MUS note (E-3, length 4)
 .byt     $6F ;MUS note (G-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $71 ;MUS note (A-3, length 4)
 .byt     $6C ;MUS note (E-3, length 4)
 .byt     $6F ;MUS note (G-3, length 4)
 .byt     $71 ;MUS note (A-3, length 4)
 .byt     $71 ;MUS note (A-3, length 4)
 .byt     $80 ;Pause         (length 8)
 .byt MUSCMD_SetVibratoIndex,   $02
 .byt MUSCMD_SetDutyCycle,      $40
 .byt MUSCMD_SetVolumeAndEnv,   $36
 .byt MUSCMD_SetVolumeCurve, $FF,$10
 .byt MUSCMD_SetNoteBaseOffset, 3*12+4
 .byt     $60 ;Pause         (length 4)
 .byt     $40 ;Pause         (length 2)
 .byt     $D1 ;MUS note (A-4, length 32)
 .byt     $80 ;Pause         (length 8)
 .byt     $B3 ;MUS note (B-4, length 16)
 .byt     $91 ;MUS note (A-4, length 8)
 .byt MUSCMD_SetNoteBaseOffset, 2*12+4
 .byt     $FB ;MUS note (G-4, length 64)
 .byt     $D9 ;MUS note (F-4, length 32)
 .byt     $80 ;Pause         (length 8)
 .byt     $BB ;MUS note (G-4, length 16)
 .byt     $99 ;MUS note (F-4, length 8)
 .byt     $98 ;MUS note (E-4, length 8)
 .byt     $99 ;MUS note (F-4, length 8)
 .byt     $9A ;MUS note (F#4, length 8)
 .byt     $9B ;MUS note (G-4, length 8)
 .byt     $C0 ;Pause         (length 32)
 .byt     $DD ;MUS note (A-4, length 32)
 .byt     $80 ;Pause         (length 8)
 .byt MUSCMD_SetNoteBaseOffset, 3*12+4
 .byt     $B3 ;MUS note (B-4, length 16)
 .byt     $91 ;MUS note (A-4, length 8)
 .byt     $8F ;MUS note (G-4, length 8)
 .byt     $8F ;MUS note (G-4, length 8)
 .byt     $96 ;MUS note (D-5, length 8)
 .byt $06,$B4 ;MUS note (C-5, length 24)
 .byt     $A0 ;Pause         (length 16)
 .byt     $71 ;MUS note (A-4, length 4)
 .byt     $71 ;MUS note (A-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $71 ;MUS note (A-4, length 4)
 .byt     $71 ;MUS note (A-4, length 4)
 .byt     $71 ;MUS note (A-4, length 4)
 .byt     $71 ;MUS note (A-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $71 ;MUS note (A-4, length 4)
 .byt     $91 ;MUS note (A-4, length 8)
 .byt     $71 ;MUS note (A-4, length 4)
 .byt     $93 ;MUS note (B-4, length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $91 ;MUS note (A-4, length 8)
 .byt     $D1 ;MUS note (A-4, length 32)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $B0 ;MUS note (G#4, length 16)
 .byt     $80 ;Pause         (length 8)
 .byt     $50 ;MUS note (G#4, length 2)
 .byt MUSCMD_SetNoteBaseOffset, 2*12+4
 .byt MUSCMD_Loop, 0 
 .word :---
MUS_Cutman_chn2: ;at A184
 .byt MUSCMD_SetSpeed,          $06
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt MUSCMD_SetVolumeAndEnv,   $30
 .byt MUSCMD_SetNoteBaseOffset, 1*12+4
: ;at A18C
 .byt     $8C ;MUS note (E-2, length 8)
 .byt     $8C ;MUS note (E-2, length 8)
 .byt     $73 ;MUS note (B-2, length 4)
 .byt     $93 ;MUS note (B-2, length 8)
 .byt     $73 ;MUS note (B-2, length 4)
 .byt     $6C ;MUS note (E-2, length 4)
 .byt     $8C ;MUS note (E-2, length 8)
 .byt     $6C ;MUS note (E-2, length 4)
 .byt     $98 ;MUS note (E-3, length 8)
 .byt     $8C ;MUS note (E-2, length 8)
 .byt MUSCMD_Loop, 1 
 .word :-
: ;at A19A
 .byt MUSCMD_SetVolumeAndEnv,   $50
 .byt MUSCMD_SetNoteBaseOffset, 1*12+4
: ;at A19E
 .byt     $91 ;MUS note (A-2, length 8)
 .byt     $91 ;MUS note (A-2, length 8)
 .byt     $78 ;MUS note (E-3, length 4)
 .byt     $98 ;MUS note (E-3, length 8)
 .byt     $78 ;MUS note (E-3, length 4)
 .byt     $71 ;MUS note (A-2, length 4)
 .byt     $91 ;MUS note (A-2, length 8)
 .byt     $71 ;MUS note (A-2, length 4)
 .byt     $98 ;MUS note (E-3, length 8)
 .byt     $8C ;MUS note (E-2, length 8)
 .byt MUSCMD_Loop, 3 
 .word :-
 .byt     $8A ;MUS note (D-2, length 8)
 .byt     $8A ;MUS note (D-2, length 8)
 .byt     $71 ;MUS note (A-2, length 4)
 .byt     $91 ;MUS note (A-2, length 8)
 .byt     $71 ;MUS note (A-2, length 4)
 .byt     $6A ;MUS note (D-2, length 4)
 .byt     $8A ;MUS note (D-2, length 8)
 .byt     $6A ;MUS note (D-2, length 4)
 .byt     $91 ;MUS note (A-2, length 8)
 .byt     $8A ;MUS note (D-2, length 8)
 .byt     $91 ;MUS note (A-2, length 8)
 .byt     $91 ;MUS note (A-2, length 8)
 .byt     $78 ;MUS note (E-3, length 4)
 .byt     $98 ;MUS note (E-3, length 8)
 .byt     $78 ;MUS note (E-3, length 4)
 .byt     $71 ;MUS note (A-2, length 4)
 .byt     $91 ;MUS note (A-2, length 8)
 .byt     $71 ;MUS note (A-2, length 4)
 .byt     $98 ;MUS note (E-3, length 8)
 .byt     $8C ;MUS note (E-2, length 8)
 .byt     $8E ;MUS note (F#2, length 8)
 .byt     $8E ;MUS note (F#2, length 8)
 .byt     $77 ;MUS note (D#3, length 4)
 .byt     $97 ;MUS note (D#3, length 8)
 .byt     $77 ;MUS note (D#3, length 4)
 .byt     $6E ;MUS note (F#2, length 4)
 .byt     $8E ;MUS note (F#2, length 8)
 .byt     $6E ;MUS note (F#2, length 4)
 .byt     $97 ;MUS note (D#3, length 8)
 .byt     $8B ;MUS note (D#2, length 8)
 .byt     $8C ;MUS note (E-2, length 8)
 .byt     $8C ;MUS note (E-2, length 8)
 .byt     $73 ;MUS note (B-2, length 4)
 .byt     $93 ;MUS note (B-2, length 8)
 .byt     $73 ;MUS note (B-2, length 4)
 .byt     $6C ;MUS note (E-2, length 4)
 .byt     $8C ;MUS note (E-2, length 8)
 .byt     $6C ;MUS note (E-2, length 4)
 .byt     $98 ;MUS note (E-3, length 8)
 .byt     $8C ;MUS note (E-2, length 8)
: ;at A1D4
 .byt     $91 ;MUS note (A-2, length 8)
 .byt     $91 ;MUS note (A-2, length 8)
 .byt     $78 ;MUS note (E-3, length 4)
 .byt     $98 ;MUS note (E-3, length 8)
 .byt     $78 ;MUS note (E-3, length 4)
 .byt     $71 ;MUS note (A-2, length 4)
 .byt     $91 ;MUS note (A-2, length 8)
 .byt     $71 ;MUS note (A-2, length 4)
 .byt     $98 ;MUS note (E-3, length 8)
 .byt     $8C ;MUS note (E-2, length 8)
 .byt MUSCMD_Loop, 3 
 .word :-
 .byt     $8A ;MUS note (D-2, length 8)
 .byt     $8A ;MUS note (D-2, length 8)
 .byt     $71 ;MUS note (A-2, length 4)
 .byt     $91 ;MUS note (A-2, length 8)
 .byt     $71 ;MUS note (A-2, length 4)
 .byt     $6A ;MUS note (D-2, length 4)
 .byt     $8A ;MUS note (D-2, length 8)
 .byt     $6A ;MUS note (D-2, length 4)
 .byt     $91 ;MUS note (A-2, length 8)
 .byt     $8A ;MUS note (D-2, length 8)
 .byt     $91 ;MUS note (A-2, length 8)
 .byt     $91 ;MUS note (A-2, length 8)
 .byt     $78 ;MUS note (E-3, length 4)
 .byt     $98 ;MUS note (E-3, length 8)
 .byt     $78 ;MUS note (E-3, length 4)
 .byt     $71 ;MUS note (A-2, length 4)
 .byt     $91 ;MUS note (A-2, length 8)
 .byt     $71 ;MUS note (A-2, length 4)
 .byt     $98 ;MUS note (E-3, length 8)
 .byt     $8C ;MUS note (E-2, length 8)
 .byt     $8E ;MUS note (F#2, length 8)
 .byt     $8E ;MUS note (F#2, length 8)
 .byt     $77 ;MUS note (D#3, length 4)
 .byt     $97 ;MUS note (D#3, length 8)
 .byt     $77 ;MUS note (D#3, length 4)
 .byt     $6C ;MUS note (E-2, length 4)
 .byt     $8C ;MUS note (E-2, length 8)
 .byt     $6C ;MUS note (E-2, length 4)
 .byt     $98 ;MUS note (E-3, length 8)
 .byt     $8C ;MUS note (E-2, length 8)
 .byt     $91 ;MUS note (A-2, length 8)
 .byt     $91 ;MUS note (A-2, length 8)
 .byt     $78 ;MUS note (E-3, length 4)
 .byt     $98 ;MUS note (E-3, length 8)
 .byt     $78 ;MUS note (E-3, length 4)
 .byt     $71 ;MUS note (A-2, length 4)
 .byt     $91 ;MUS note (A-2, length 8)
 .byt     $71 ;MUS note (A-2, length 4)
 .byt     $98 ;MUS note (E-3, length 8)
 .byt     $8C ;MUS note (E-2, length 8)
 .byt     $8D ;MUS note (F-2, length 8)
 .byt     $8D ;MUS note (F-2, length 8)
 .byt     $79 ;MUS note (F-3, length 4)
 .byt     $99 ;MUS note (F-3, length 8)
 .byt     $79 ;MUS note (F-3, length 4)
 .byt     $6F ;MUS note (G-2, length 4)
 .byt     $8F ;MUS note (G-2, length 8)
 .byt     $6F ;MUS note (G-2, length 4)
 .byt     $9B ;MUS note (G-3, length 8)
 .byt     $9B ;MUS note (G-3, length 8)
 .byt     $88 ;MUS note (C-2, length 8)
 .byt     $88 ;MUS note (C-2, length 8)
 .byt     $74 ;MUS note (C-3, length 4)
 .byt     $94 ;MUS note (C-3, length 8)
 .byt     $74 ;MUS note (C-3, length 4)
 .byt     $68 ;MUS note (C-2, length 4)
 .byt     $88 ;MUS note (C-2, length 8)
 .byt     $68 ;MUS note (C-2, length 4)
 .byt     $94 ;MUS note (C-3, length 8)
 .byt     $94 ;MUS note (C-3, length 8)
 .byt     $8A ;MUS note (D-2, length 8)
 .byt     $8A ;MUS note (D-2, length 8)
 .byt     $76 ;MUS note (D-3, length 4)
 .byt     $96 ;MUS note (D-3, length 8)
 .byt     $74 ;MUS note (C-3, length 4)
 .byt     $73 ;MUS note (B-2, length 4)
 .byt     $93 ;MUS note (B-2, length 8)
 .byt     $73 ;MUS note (B-2, length 4)
 .byt     $93 ;MUS note (B-2, length 8)
 .byt     $93 ;MUS note (B-2, length 8)
 .byt     $88 ;MUS note (C-2, length 8)
 .byt     $88 ;MUS note (C-2, length 8)
 .byt     $74 ;MUS note (C-3, length 4)
 .byt     $94 ;MUS note (C-3, length 8)
 .byt     $74 ;MUS note (C-3, length 4)
 .byt     $68 ;MUS note (C-2, length 4)
 .byt     $88 ;MUS note (C-2, length 8)
 .byt     $68 ;MUS note (C-2, length 4)
 .byt     $94 ;MUS note (C-3, length 8)
 .byt     $94 ;MUS note (C-3, length 8)
 .byt     $8D ;MUS note (F-2, length 8)
 .byt     $8D ;MUS note (F-2, length 8)
 .byt     $79 ;MUS note (F-3, length 4)
 .byt     $99 ;MUS note (F-3, length 8)
 .byt     $79 ;MUS note (F-3, length 4)
 .byt     $6F ;MUS note (G-2, length 4)
 .byt     $8F ;MUS note (G-2, length 8)
 .byt     $6F ;MUS note (G-2, length 4)
 .byt     $9B ;MUS note (G-3, length 8)
 .byt     $9B ;MUS note (G-3, length 8)
 .byt     $8C ;MUS note (E-2, length 8)
 .byt     $8C ;MUS note (E-2, length 8)
 .byt     $78 ;MUS note (E-3, length 4)
 .byt     $98 ;MUS note (E-3, length 8)
 .byt     $78 ;MUS note (E-3, length 4)
 .byt     $71 ;MUS note (A-2, length 4)
 .byt     $91 ;MUS note (A-2, length 8)
 .byt     $71 ;MUS note (A-2, length 4)
 .byt     $9D ;MUS note (A-3, length 8)
 .byt     $9D ;MUS note (A-3, length 8)
 .byt     $8A ;MUS note (D-2, length 8)
 .byt     $8A ;MUS note (D-2, length 8)
 .byt     $76 ;MUS note (D-3, length 4)
 .byt     $96 ;MUS note (D-3, length 8)
 .byt     $76 ;MUS note (D-3, length 4)
 .byt     $6B ;MUS note (D#2, length 4)
 .byt     $8B ;MUS note (D#2, length 8)
 .byt     $6B ;MUS note (D#2, length 4)
 .byt     $97 ;MUS note (D#3, length 8)
 .byt     $97 ;MUS note (D#3, length 8)
 .byt     $8C ;MUS note (E-2, length 8)
 .byt     $8C ;MUS note (E-2, length 8)
 .byt     $78 ;MUS note (E-3, length 4)
 .byt     $98 ;MUS note (E-3, length 8)
 .byt     $78 ;MUS note (E-3, length 4)
 .byt     $6C ;MUS note (E-2, length 4)
 .byt     $8C ;MUS note (E-2, length 8)
 .byt     $6C ;MUS note (E-2, length 4)
 .byt     $98 ;MUS note (E-3, length 8)
 .byt     $98 ;MUS note (E-3, length 8)
 .byt MUSCMD_Loop, 0 
 .word :---
MUS_Cutman_chn3: ;at A25E
 .byt MUSCMD_SetSpeed,          $06
: ;at A260
 .byt MUSCMD_SetVolumeAndEnv,   $37
 .byt MUSCMD_SetVolumeCurve, $83,$30
 .byt     $85 ;MUS note (A-1, length 8)
 .byt     $65 ;MUS note (A-1, length 4)
 .byt     $65 ;MUS note (A-1, length 4)
 .byt     $85 ;MUS note (A-1, length 8)
 .byt     $65 ;MUS note (A-1, length 4)
 .byt     $65 ;MUS note (A-1, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $65 ;MUS note (A-1, length 4)
 .byt     $65 ;MUS note (A-1, length 4)
 .byt     $65 ;MUS note (A-1, length 4)
 .byt     $85 ;MUS note (A-1, length 8)
 .byt     $85 ;MUS note (A-1, length 8)
 .byt     $85 ;MUS note (A-1, length 8)
 .byt     $65 ;MUS note (A-1, length 4)
 .byt     $65 ;MUS note (A-1, length 4)
 .byt     $85 ;MUS note (A-1, length 8)
 .byt     $65 ;MUS note (A-1, length 4)
 .byt     $65 ;MUS note (A-1, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $65 ;MUS note (A-1, length 4)
 .byt     $65 ;MUS note (A-1, length 4)
 .byt     $65 ;MUS note (A-1, length 4)
 .byt     $65 ;MUS note (A-1, length 4)
 .byt     $65 ;MUS note (A-1, length 4)
 .byt     $65 ;MUS note (A-1, length 4)
 .byt     $65 ;MUS note (A-1, length 4)
 .byt MUSCMD_Loop, 0 
 .word :-
MUS_Cutman_vibratotable: ;at A283
 .byt $00, $E0, $80, $00 ;vibrato definition
 .byt $01, $62, $80, $00 ;vibrato definition
 .byt $02, $22, $80, $00 ;vibrato definition
.endscope

MUS_Fireman: ;at A28F
.scope
 .byt $0F ;priority. Lo=music priority, Hi=sfx priority
 .word MUS_Fireman_chn0               ; $A29A ;
 .word MUS_Fireman_chn1               ; $A33A ;
 .word MUS_Fireman_chn2               ; $A3CE ;
 .word MUS_Fireman_chn3               ; $A453 ;
 .word MUS_Fireman_vibratotable       ; $A4A3 ;
MUS_Fireman_chn0: ;at A29A
 .byt MUSCMD_SetSpeed,          $06
 .byt MUSCMD_SetVolumeAndEnv,   $38
 .byt MUSCMD_SetDutyCycle,      $C0
 .byt MUSCMD_SetNoteBaseOffset, 2*12+0
 .byt MUSCMD_SetVolumeCurve, $BF,$10
 .byt     $E0 ;Pause         (length 64)
 .byt     $E0 ;Pause         (length 64)
 .byt     $C0 ;Pause         (length 32)
 .byt     $A0 ;Pause         (length 16)
 .byt     $AD ;MUS note (C#3, length 16)
: ;at A2AA
 .byt MUSCMD_SetVolumeAndEnv,   $38
 .byt MUSCMD_SetDutyCycle,      $C0
 .byt MUSCMD_SetNoteBaseOffset, 2*12+0
 .byt MUSCMD_SetVolumeCurve, $BF,$10
 .byt     $D2 ;MUS note (F#3, length 32)
 .byt     $70 ;MUS note (E-3, length 4)
 .byt     $92 ;MUS note (F#3, length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $70 ;MUS note (E-3, length 4)
 .byt     $70 ;MUS note (E-3, length 4)
 .byt     $8D ;MUS note (C#3, length 8)
 .byt     $6B ;MUS note (B-2, length 4)
 .byt $06,$CD ;MUS note (C#3, length 48)
 .byt     $AD ;MUS note (C#3, length 16)
 .byt     $D2 ;MUS note (F#3, length 32)
 .byt     $70 ;MUS note (E-3, length 4)
 .byt     $92 ;MUS note (F#3, length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $74 ;MUS note (G#3, length 4)
 .byt     $74 ;MUS note (G#3, length 4)
 .byt     $92 ;MUS note (F#3, length 8)
 .byt     $70 ;MUS note (E-3, length 4)
 .byt     $D2 ;MUS note (F#3, length 32)
 .byt     $51 ;MUS note (F-3, length 2)
 .byt     $52 ;MUS note (F#3, length 2)
 .byt     $53 ;MUS note (G-3, length 2)
 .byt     $54 ;MUS note (G#3, length 2)
 .byt     $55 ;MUS note (A-3, length 2)
 .byt     $56 ;MUS note (A#3, length 2)
 .byt     $57 ;MUS note (B-3, length 2)
 .byt     $58 ;MUS note (C-4, length 2)
 .byt     $B9 ;MUS note (C#4, length 16)
 .byt MUSCMD_SetNoteBaseOffset, 3*12+0
 .byt     $D2 ;MUS note (F#4, length 32)
 .byt     $70 ;MUS note (E-4, length 4)
 .byt     $92 ;MUS note (F#4, length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $70 ;MUS note (E-4, length 4)
 .byt     $70 ;MUS note (E-4, length 4)
 .byt     $8D ;MUS note (C#4, length 8)
 .byt     $6B ;MUS note (B-3, length 4)
 .byt $06,$CD ;MUS note (C#4, length 48)
 .byt     $AD ;MUS note (C#4, length 16)
 .byt     $D2 ;MUS note (F#4, length 32)
 .byt     $70 ;MUS note (E-4, length 4)
 .byt     $92 ;MUS note (F#4, length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $74 ;MUS note (G#4, length 4)
 .byt     $74 ;MUS note (G#4, length 4)
 .byt     $92 ;MUS note (F#4, length 8)
 .byt     $70 ;MUS note (E-4, length 4)
 .byt     $F2 ;MUS note (F#4, length 64)
 .byt MUSCMD_SetNoteBaseOffset, 2*12+0
 .byt MUSCMD_SetVolumeAndEnv,   $3A
 .byt MUSCMD_SetDutyCycle,      $40
 .byt MUSCMD_SetVolumeCurve, $86,$20
 .byt $06,$BA ;MUS note (D-4, length 24)
 .byt     $99 ;MUS note (C#4, length 8)
 .byt     $97 ;MUS note (B-3, length 8)
 .byt     $94 ;MUS note (G#3, length 8)
 .byt     $95 ;MUS note (A-3, length 8)
 .byt     $97 ;MUS note (B-3, length 8)
 .byt $06,$B9 ;MUS note (C#4, length 24)
 .byt     $97 ;MUS note (B-3, length 8)
 .byt     $95 ;MUS note (A-3, length 8)
 .byt     $92 ;MUS note (F#3, length 8)
 .byt     $94 ;MUS note (G#3, length 8)
 .byt     $95 ;MUS note (A-3, length 8)
 .byt $06,$B7 ;MUS note (B-3, length 24)
 .byt     $95 ;MUS note (A-3, length 8)
 .byt     $94 ;MUS note (G#3, length 8)
 .byt     $91 ;MUS note (F-3, length 8)
 .byt     $92 ;MUS note (F#3, length 8)
 .byt     $94 ;MUS note (G#3, length 8)
 .byt     $60 ;Pause         (length 4)
 .byt     $75 ;MUS note (A-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $77 ;MUS note (B-3, length 4)
 .byt     $B7 ;MUS note (B-3, length 16)
 .byt     $60 ;Pause         (length 4)
 .byt     $78 ;MUS note (C-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $79 ;MUS note (C#4, length 4)
 .byt     $B9 ;MUS note (C#4, length 16)
 .byt $06,$BA ;MUS note (D-4, length 24)
 .byt     $99 ;MUS note (C#4, length 8)
 .byt     $97 ;MUS note (B-3, length 8)
 .byt     $94 ;MUS note (G#3, length 8)
 .byt     $7C ;MUS note (E-4, length 4)
 .byt $06,$9A ;MUS note (D-4, length 12)
 .byt $06,$B9 ;MUS note (C#4, length 24)
 .byt     $97 ;MUS note (B-3, length 8)
 .byt     $95 ;MUS note (A-3, length 8)
 .byt     $92 ;MUS note (F#3, length 8)
 .byt     $7A ;MUS note (D-4, length 4)
 .byt $06,$99 ;MUS note (C#4, length 12)
 .byt $06,$B7 ;MUS note (B-3, length 24)
 .byt     $75 ;MUS note (A-3, length 4)
 .byt     $77 ;MUS note (B-3, length 4)
 .byt $06,$B8 ;MUS note (C-4, length 24)
 .byt     $76 ;MUS note (A#3, length 4)
 .byt     $78 ;MUS note (C-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $79 ;MUS note (C#4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $7A ;MUS note (D-4, length 4)
 .byt     $BA ;MUS note (D-4, length 16)
 .byt     $7A ;MUS note (D-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $7B ;MUS note (D#4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $7C ;MUS note (E-4, length 4)
 .byt $06,$9D ;MUS note (F-4, length 12)
 .byt MUSCMD_Loop, 0 
 .word :-
 .byt MUSCMD_EndOfData
MUS_Fireman_chn1: ;at A33A
 .byt MUSCMD_SetSpeed,          $06
 .byt MUSCMD_SetDutyCycle,      $C0
 .byt MUSCMD_SetVolumeAndEnv,   $35
 .byt MUSCMD_SetVolumeCurve, $86,$30
 .byt     $E0 ;Pause         (length 64)
 .byt MUSCMD_SetNoteBaseOffset, 2*12+0
: ;at A346
 .byt     $80 ;Pause         (length 8)
 .byt     $75 ;MUS note (A-3, length 4)
 .byt     $80 ;Pause         (length 8)
 .byt     $60 ;Pause         (length 4)
 .byt     $75 ;MUS note (A-3, length 4)
 .byt     $80 ;Pause         (length 8)
 .byt     $60 ;Pause         (length 4)
 .byt     $75 ;MUS note (A-3, length 4)
 .byt     $80 ;Pause         (length 8)
 .byt     $60 ;Pause         (length 4)
 .byt     $75 ;MUS note (A-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt MUSCMD_Loop, 1 
 .word :-
: ;at A356
 .byt MUSCMD_SetNoteBaseOffset, 2*12+0
 .byt MUSCMD_SetVolumeAndEnv,   $35
 .byt MUSCMD_SetVolumeCurve, $86,$30
 .byt     $80 ;Pause         (length 8)
 .byt     $75 ;MUS note (A-3, length 4)
 .byt     $80 ;Pause         (length 8)
 .byt     $60 ;Pause         (length 4)
 .byt     $75 ;MUS note (A-3, length 4)
 .byt     $80 ;Pause         (length 8)
 .byt     $60 ;Pause         (length 4)
 .byt     $75 ;MUS note (A-3, length 4)
 .byt     $80 ;Pause         (length 8)
 .byt     $60 ;Pause         (length 4)
 .byt     $75 ;MUS note (A-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt MUSCMD_Loop, 7 
 .word :-
 .byt MUSCMD_SetVolumeAndEnv,   $35
 .byt MUSCMD_SetVolumeCurve, $83,$10
 .byt     $B7 ;MUS note (B-3, length 16)
 .byt     $77 ;MUS note (B-3, length 4)
 .byt $06,$95 ;MUS note (A-3, length 12)
 .byt     $B4 ;MUS note (G#3, length 16)
 .byt     $B0 ;MUS note (E-3, length 16)
 .byt     $B5 ;MUS note (A-3, length 16)
 .byt     $75 ;MUS note (A-3, length 4)
 .byt $06,$94 ;MUS note (G#3, length 12)
 .byt     $B2 ;MUS note (F#3, length 16)
 .byt     $AE ;MUS note (D-3, length 16)
 .byt     $B4 ;MUS note (G#3, length 16)
 .byt     $74 ;MUS note (G#3, length 4)
 .byt $06,$92 ;MUS note (F#3, length 12)
 .byt     $B1 ;MUS note (F-3, length 16)
 .byt     $AD ;MUS note (C#3, length 16)
 .byt     $60 ;Pause         (length 4)
 .byt     $6D ;MUS note (C#3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $6E ;MUS note (D-3, length 4)
 .byt     $AE ;MUS note (D-3, length 16)
 .byt     $60 ;Pause         (length 4)
 .byt     $6F ;MUS note (D#3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $70 ;MUS note (E-3, length 4)
 .byt     $B0 ;MUS note (E-3, length 16)
 .byt     $77 ;MUS note (B-3, length 4)
 .byt     $79 ;MUS note (C#4, length 4)
 .byt     $77 ;MUS note (B-3, length 4)
 .byt     $79 ;MUS note (C#4, length 4)
 .byt     $77 ;MUS note (B-3, length 4)
 .byt     $79 ;MUS note (C#4, length 4)
 .byt     $77 ;MUS note (B-3, length 4)
 .byt     $79 ;MUS note (C#4, length 4)
 .byt $30,$BC ;MUS note (E-4, length 16/1.5) (triplet)
 .byt $30,$B4 ;MUS note (G#3, length 16/1.5) (triplet)
 .byt $30,$B0 ;MUS note (E-3, length 16/1.5) (triplet)
 .byt     $75 ;MUS note (A-3, length 4)
 .byt     $77 ;MUS note (B-3, length 4)
 .byt     $75 ;MUS note (A-3, length 4)
 .byt     $77 ;MUS note (B-3, length 4)
 .byt     $75 ;MUS note (A-3, length 4)
 .byt     $77 ;MUS note (B-3, length 4)
 .byt     $75 ;MUS note (A-3, length 4)
 .byt     $77 ;MUS note (B-3, length 4)
 .byt $30,$BA ;MUS note (D-4, length 16/1.5) (triplet)
 .byt $30,$B2 ;MUS note (F#3, length 16/1.5) (triplet)
 .byt $30,$AE ;MUS note (D-3, length 16/1.5) (triplet)
 .byt     $74 ;MUS note (G#3, length 4)
 .byt     $75 ;MUS note (A-3, length 4)
 .byt     $74 ;MUS note (G#3, length 4)
 .byt     $75 ;MUS note (A-3, length 4)
 .byt     $74 ;MUS note (G#3, length 4)
 .byt     $75 ;MUS note (A-3, length 4)
 .byt     $74 ;MUS note (G#3, length 4)
 .byt     $75 ;MUS note (A-3, length 4)
 .byt     $76 ;MUS note (A#3, length 4)
 .byt     $77 ;MUS note (B-3, length 4)
 .byt     $76 ;MUS note (A#3, length 4)
 .byt     $77 ;MUS note (B-3, length 4)
 .byt     $76 ;MUS note (A#3, length 4)
 .byt     $77 ;MUS note (B-3, length 4)
 .byt     $76 ;MUS note (A#3, length 4)
 .byt     $77 ;MUS note (B-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $71 ;MUS note (F-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $72 ;MUS note (F#3, length 4)
 .byt     $B2 ;MUS note (F#3, length 16)
 .byt     $72 ;MUS note (F#3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $73 ;MUS note (G-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $74 ;MUS note (G#3, length 4)
 .byt $06,$97 ;MUS note (B-3, length 12)
 .byt MUSCMD_Loop, 0 
 .word :-
 .byt MUSCMD_EndOfData
MUS_Fireman_chn2: ;at A3CE
 .byt MUSCMD_SetSpeed,          $06
 .byt MUSCMD_SetVibratoCtrl,    $0F
 .byt MUSCMD_SetNoteBaseOffset, 3*12+1
 .byt MUSCMD_SetVolumeAndEnv,   $09
 .byt     $79 ;MUS note (D-5, length 4)
 .byt     $79 ;MUS note (D-5, length 4)
 .byt     $79 ;MUS note (D-5, length 4)
 .byt     $79 ;MUS note (D-5, length 4)
 .byt     $72 ;MUS note (G-4, length 4)
 .byt     $72 ;MUS note (G-4, length 4)
 .byt     $72 ;MUS note (G-4, length 4)
 .byt     $72 ;MUS note (G-4, length 4)
 .byt     $6D ;MUS note (D-4, length 4)
 .byt     $6D ;MUS note (D-4, length 4)
 .byt     $6D ;MUS note (D-4, length 4)
 .byt     $6D ;MUS note (D-4, length 4)
 .byt     $68 ;MUS note (A-3, length 4)
 .byt     $68 ;MUS note (A-3, length 4)
 .byt     $68 ;MUS note (A-3, length 4)
 .byt     $68 ;MUS note (A-3, length 4)
: ;at A3E6
 .byt MUSCMD_SetVibratoCtrl,    $00
 .byt MUSCMD_SetVolumeAndEnv,   $60
 .byt MUSCMD_SetNoteBaseOffset, 2*12+0
 .byt     $86 ;MUS note (F#2, length 8)
 .byt     $72 ;MUS note (F#3, length 4)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $65 ;MUS note (F-2, length 4)
 .byt     $65 ;MUS note (F-2, length 4)
 .byt     $65 ;MUS note (F-2, length 4)
 .byt     $72 ;MUS note (F#3, length 4)
 .byt     $65 ;MUS note (F-2, length 4)
 .byt     $84 ;MUS note (E-2, length 8)
 .byt     $72 ;MUS note (F#3, length 4)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $65 ;MUS note (F-2, length 4)
 .byt     $65 ;MUS note (F-2, length 4)
 .byt     $65 ;MUS note (F-2, length 4)
 .byt     $72 ;MUS note (F#3, length 4)
 .byt     $65 ;MUS note (F-2, length 4)
 .byt MUSCMD_Loop, 1 
 .word :-
: ;at A400
 .byt MUSCMD_SetVolumeAndEnv,   $60
 .byt MUSCMD_SetNoteBaseOffset, 2*12+0
 .byt     $86 ;MUS note (F#2, length 8)
 .byt     $72 ;MUS note (F#3, length 4)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $65 ;MUS note (F-2, length 4)
 .byt     $65 ;MUS note (F-2, length 4)
 .byt     $65 ;MUS note (F-2, length 4)
 .byt     $72 ;MUS note (F#3, length 4)
 .byt     $65 ;MUS note (F-2, length 4)
 .byt     $84 ;MUS note (E-2, length 8)
 .byt     $72 ;MUS note (F#3, length 4)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $65 ;MUS note (F-2, length 4)
 .byt     $65 ;MUS note (F-2, length 4)
 .byt     $65 ;MUS note (F-2, length 4)
 .byt     $72 ;MUS note (F#3, length 4)
 .byt     $65 ;MUS note (F-2, length 4)
 .byt MUSCMD_Loop, 7 
 .word :-
 .byt MUSCMD_SetVolumeAndEnv,   $81
 .byt     $8B ;MUS note (B-2, length 8)
 .byt     $8B ;MUS note (B-2, length 8)
 .byt     $60 ;Pause         (length 4)
 .byt     $8B ;MUS note (B-2, length 8)
 .byt     $6B ;MUS note (B-2, length 4)
 .byt     $B0 ;MUS note (E-3, length 16)
 .byt     $A4 ;MUS note (E-2, length 16)
 .byt     $89 ;MUS note (A-2, length 8)
 .byt     $89 ;MUS note (A-2, length 8)
 .byt     $60 ;Pause         (length 4)
 .byt     $89 ;MUS note (A-2, length 8)
 .byt     $69 ;MUS note (A-2, length 4)
 .byt     $AE ;MUS note (D-3, length 16)
 .byt     $A2 ;MUS note (D-2, length 16)
 .byt     $88 ;MUS note (G#2, length 8)
 .byt     $88 ;MUS note (G#2, length 8)
 .byt     $60 ;Pause         (length 4)
 .byt     $88 ;MUS note (G#2, length 8)
 .byt     $68 ;MUS note (G#2, length 4)
 .byt     $AD ;MUS note (C#3, length 16)
 .byt     $A1 ;MUS note (C#2, length 16)
 .byt     $C6 ;MUS note (F#2, length 32)
 .byt     $C6 ;MUS note (F#2, length 32)
 .byt     $8B ;MUS note (B-2, length 8)
 .byt     $8B ;MUS note (B-2, length 8)
 .byt     $60 ;Pause         (length 4)
 .byt     $8B ;MUS note (B-2, length 8)
 .byt     $6B ;MUS note (B-2, length 4)
 .byt     $B0 ;MUS note (E-3, length 16)
 .byt     $92 ;MUS note (F#3, length 8)
 .byt     $94 ;MUS note (G#3, length 8)
 .byt     $89 ;MUS note (A-2, length 8)
 .byt     $89 ;MUS note (A-2, length 8)
 .byt     $60 ;Pause         (length 4)
 .byt     $89 ;MUS note (A-2, length 8)
 .byt     $69 ;MUS note (A-2, length 4)
 .byt     $AE ;MUS note (D-3, length 16)
 .byt     $90 ;MUS note (E-3, length 8)
 .byt     $92 ;MUS note (F#3, length 8)
 .byt     $88 ;MUS note (G#2, length 8)
 .byt     $88 ;MUS note (G#2, length 8)
 .byt     $60 ;Pause         (length 4)
 .byt     $88 ;MUS note (G#2, length 8)
 .byt     $68 ;MUS note (G#2, length 4)
 .byt     $88 ;MUS note (G#2, length 8)
 .byt     $88 ;MUS note (G#2, length 8)
 .byt     $60 ;Pause         (length 4)
 .byt     $88 ;MUS note (G#2, length 8)
 .byt     $68 ;MUS note (G#2, length 4)
 .byt     $E1 ;MUS note (C#2, length 64)
 .byt MUSCMD_SetVolumeAndEnv,   $60
 .byt MUSCMD_Loop, 0 
 .word :-
 .byt MUSCMD_EndOfData
MUS_Fireman_chn3: ;at A453
 .byt MUSCMD_SetSpeed,          $06
 .byt MUSCMD_SetVolumeCurve, $81,$10
 .byt     $E0 ;Pause         (length 64)
: ;at A459
 .byt MUSCMD_SetDutyCycle,      $80
 .byt MUSCMD_SetVolumeCurve, $83,$30
 .byt MUSCMD_SetVolumeAndEnv,   $37
 .byt     $66 ;MUS note (F#2, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $66 ;MUS note (F#2, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $66 ;MUS note (F#2, length 4)
 .byt MUSCMD_SetDutyCycle,      $00
 .byt MUSCMD_SetVolumeCurve, $81,$10
 .byt     $85 ;MUS note (F-2, length 8)
 .byt MUSCMD_SetVolumeCurve, $83,$30
 .byt MUSCMD_SetDutyCycle,      $80
 .byt     $66 ;MUS note (F#2, length 4)
 .byt     $66 ;MUS note (F#2, length 4)
 .byt     $66 ;MUS note (F#2, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $66 ;MUS note (F#2, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $60 ;Pause         (length 4)
 .byt MUSCMD_SetVolumeCurve, $81,$10
 .byt MUSCMD_SetDutyCycle,      $00
 .byt     $85 ;MUS note (F-2, length 8)
 .byt MUSCMD_SetDutyCycle,      $80
 .byt MUSCMD_SetVolumeCurve, $83,$30
 .byt     $66 ;MUS note (F#2, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $66 ;MUS note (F#2, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $66 ;MUS note (F#2, length 4)
 .byt MUSCMD_SetDutyCycle,      $00
 .byt MUSCMD_SetVolumeCurve, $81,$10
 .byt     $85 ;MUS note (F-2, length 8)
 .byt MUSCMD_SetVolumeCurve, $83,$30
 .byt MUSCMD_SetDutyCycle,      $80
 .byt     $66 ;MUS note (F#2, length 4)
 .byt     $66 ;MUS note (F#2, length 4)
 .byt     $66 ;MUS note (F#2, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $66 ;MUS note (F#2, length 4)
 .byt MUSCMD_SetDutyCycle,      $00
 .byt MUSCMD_SetVolumeCurve, $81,$10
 .byt     $85 ;MUS note (F-2, length 8)
 .byt     $65 ;MUS note (F-2, length 4)
 .byt     $65 ;MUS note (F-2, length 4)
 .byt MUSCMD_Loop, 0 
 .word :-
MUS_Fireman_vibratotable: ;at A4A3
 .byt $00, $E0, $80, $00 ;vibrato definition
.endscope

MUS_Bombman: ;at A4A7
.scope
 .byt $0F ;priority. Lo=music priority, Hi=sfx priority
 .word MUS_Bombman_chn0               ; $A4B2 ;
 .word MUS_Bombman_chn1               ; $A5BA ;
 .word MUS_Bombman_chn2               ; $A6B3 ;
 .word MUS_Bombman_chn3               ; $A787 ;
 .word MUS_Bombman_vibratotable       ; $A7EB ;
MUS_Bombman_chn0: ;at A4B2
 .byt MUSCMD_SetSpeed,          $07
 .byt MUSCMD_SetDutyCycle,      $80
 .byt MUSCMD_SetVolumeAndEnv,   $38
 .byt MUSCMD_SetVibratoIndex,   $00
 .byt MUSCMD_SetVolumeCurve, $A0,$20
 .byt MUSCMD_SetNoteBaseOffset, 3*12+0
 .byt     $A0 ;Pause         (length 16)
 .byt     $91 ;MUS note (F-4, length 8)
 .byt     $8F ;MUS note (D#4, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $94 ;MUS note (G#4, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $92 ;MUS note (F#4, length 8)
 .byt     $F2 ;MUS note (F#4, length 64)
 .byt     $A0 ;Pause         (length 16)
 .byt     $91 ;MUS note (F-4, length 8)
 .byt     $8F ;MUS note (D#4, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $94 ;MUS note (G#4, length 8)
 .byt     $A0 ;Pause         (length 16)
 .byt     $74 ;MUS note (G#4, length 4)
 .byt     $74 ;MUS note (G#4, length 4)
 .byt     $74 ;MUS note (G#4, length 4)
 .byt     $74 ;MUS note (G#4, length 4)
 .byt     $80 ;Pause         (length 8)
 .byt     $94 ;MUS note (G#4, length 8)
 .byt     $C0 ;Pause         (length 32)
: ;at A4D5
 .byt MUSCMD_SetDutyCycle,      $C0
 .byt MUSCMD_SetVolumeCurve, $FF,$30
 .byt MUSCMD_SetNoteBaseOffset, 2*12+0
 .byt     $D4 ;MUS note (G#3, length 32)
 .byt     $71 ;MUS note (F-3, length 4)
 .byt     $72 ;MUS note (F#3, length 4)
 .byt     $74 ;MUS note (G#3, length 4)
 .byt     $99 ;MUS note (C#4, length 8)
 .byt     $94 ;MUS note (G#3, length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $77 ;MUS note (B-3, length 4)
 .byt     $D7 ;MUS note (B-3, length 32)
 .byt     $B4 ;MUS note (G#3, length 16)
 .byt     $B2 ;MUS note (F#3, length 16)
 .byt     $D4 ;MUS note (G#3, length 32)
 .byt     $71 ;MUS note (F-3, length 4)
 .byt     $72 ;MUS note (F#3, length 4)
 .byt     $74 ;MUS note (G#3, length 4)
 .byt     $9D ;MUS note (F-4, length 8)
 .byt     $99 ;MUS note (C#4, length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $7E ;MUS note (F#4, length 4)
 .byt     $DE ;MUS note (F#4, length 32)
 .byt     $BD ;MUS note (F-4, length 16)
 .byt     $BB ;MUS note (D#4, length 16)
 .byt     $D4 ;MUS note (G#3, length 32)
 .byt     $71 ;MUS note (F-3, length 4)
 .byt     $72 ;MUS note (F#3, length 4)
 .byt     $74 ;MUS note (G#3, length 4)
 .byt     $99 ;MUS note (C#4, length 8)
 .byt $06,$94 ;MUS note (G#3, length 12)
 .byt     $D7 ;MUS note (B-3, length 32)
 .byt     $74 ;MUS note (G#3, length 4)
 .byt     $75 ;MUS note (A-3, length 4)
 .byt     $77 ;MUS note (B-3, length 4)
 .byt     $9C ;MUS note (E-4, length 8)
 .byt $06,$97 ;MUS note (B-3, length 12)
 .byt     $9A ;MUS note (D-4, length 8)
 .byt     $97 ;MUS note (B-3, length 8)
 .byt     $7A ;MUS note (D-4, length 4)
 .byt $06,$9F ;MUS note (G-4, length 12)
 .byt MUSCMD_SetNoteBaseOffset, 3*12+0
 .byt     $90 ;MUS note (E-4, length 8)
 .byt     $8D ;MUS note (C#4, length 8)
 .byt     $70 ;MUS note (E-4, length 4)
 .byt $06,$95 ;MUS note (A-4, length 12)
 .byt     $B7 ;MUS note (B-4, length 16)
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt MUSCMD_SetVibratoCtrl,    $04
 .byt     $B7 ;MUS note (B-4, length 16)
 .byt MUSCMD_SetVibratoIndex,   $00
 .byt MUSCMD_SetVibratoCtrl,    $00
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $AB ;MUS note (B-3, length 16)
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt     $AB ;MUS note (B-3, length 16)
 .byt MUSCMD_SetVibratoIndex,   $00
 .byt MUSCMD_SetNoteBaseOffset, 2*12+0
 .byt MUSCMD_SetDutyCycle,      $80
 .byt MUSCMD_SetVolumeCurve, $DF,$70
 .byt     $72 ;MUS note (F#3, length 4)
 .byt     $74 ;MUS note (G#3, length 4)
 .byt     $75 ;MUS note (A-3, length 4)
 .byt     $77 ;MUS note (B-3, length 4)
 .byt     $79 ;MUS note (C#4, length 4)
 .byt     $7B ;MUS note (D#4, length 4)
 .byt     $7C ;MUS note (E-4, length 4)
 .byt     $7B ;MUS note (D#4, length 4)
 .byt     $79 ;MUS note (C#4, length 4)
 .byt     $7B ;MUS note (D#4, length 4)
 .byt     $79 ;MUS note (C#4, length 4)
 .byt     $77 ;MUS note (B-3, length 4)
 .byt     $75 ;MUS note (A-3, length 4)
 .byt     $77 ;MUS note (B-3, length 4)
 .byt     $75 ;MUS note (A-3, length 4)
 .byt     $74 ;MUS note (G#3, length 4)
 .byt     $70 ;MUS note (E-3, length 4)
 .byt     $72 ;MUS note (F#3, length 4)
 .byt     $74 ;MUS note (G#3, length 4)
 .byt     $75 ;MUS note (A-3, length 4)
 .byt     $77 ;MUS note (B-3, length 4)
 .byt     $79 ;MUS note (C#4, length 4)
 .byt     $7B ;MUS note (D#4, length 4)
 .byt     $79 ;MUS note (C#4, length 4)
 .byt     $77 ;MUS note (B-3, length 4)
 .byt     $79 ;MUS note (C#4, length 4)
 .byt     $77 ;MUS note (B-3, length 4)
 .byt     $75 ;MUS note (A-3, length 4)
 .byt     $74 ;MUS note (G#3, length 4)
 .byt     $73 ;MUS note (G-3, length 4)
 .byt     $72 ;MUS note (F#3, length 4)
 .byt     $71 ;MUS note (F-3, length 4)
 .byt     $72 ;MUS note (F#3, length 4)
 .byt     $74 ;MUS note (G#3, length 4)
 .byt     $75 ;MUS note (A-3, length 4)
 .byt     $77 ;MUS note (B-3, length 4)
 .byt     $79 ;MUS note (C#4, length 4)
 .byt     $7B ;MUS note (D#4, length 4)
 .byt     $7C ;MUS note (E-4, length 4)
 .byt     $7B ;MUS note (D#4, length 4)
 .byt     $73 ;MUS note (G-3, length 4)
 .byt     $75 ;MUS note (A-3, length 4)
 .byt     $76 ;MUS note (A#3, length 4)
 .byt     $78 ;MUS note (C-4, length 4)
 .byt     $79 ;MUS note (C#4, length 4)
 .byt     $7B ;MUS note (D#4, length 4)
 .byt     $7C ;MUS note (E-4, length 4)
 .byt     $7B ;MUS note (D#4, length 4)
 .byt $06,$88 ;MUS note (G#2, length 12)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $6D ;MUS note (C#3, length 4)
 .byt     $8D ;MUS note (C#3, length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt $06,$92 ;MUS note (F#3, length 12)
 .byt     $72 ;MUS note (F#3, length 4)
 .byt     $97 ;MUS note (B-3, length 8)
 .byt     $9C ;MUS note (E-4, length 8)
 .byt MUSCMD_SetNoteBaseOffset, 3*12+0
 .byt     $95 ;MUS note (A-4, length 8)
 .byt     $94 ;MUS note (G#4, length 8)
 .byt     $72 ;MUS note (F#4, length 4)
 .byt     $74 ;MUS note (G#4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $72 ;MUS note (F#4, length 4)
 .byt     $74 ;MUS note (G#4, length 4)
 .byt     $72 ;MUS note (F#4, length 4)
 .byt     $91 ;MUS note (F-4, length 8)
 .byt     $8D ;MUS note (C#4, length 8)
 .byt     $60 ;Pause         (length 4)
 .byt     $6D ;MUS note (C#4, length 4)
 .byt     $6F ;MUS note (D#4, length 4)
 .byt     $71 ;MUS note (F-4, length 4)
 .byt     $92 ;MUS note (F#4, length 8)
 .byt     $70 ;MUS note (E-4, length 4)
 .byt     $72 ;MUS note (F#4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $70 ;MUS note (E-4, length 4)
 .byt     $72 ;MUS note (F#4, length 4)
 .byt     $70 ;MUS note (E-4, length 4)
 .byt     $8E ;MUS note (D-4, length 8)
 .byt     $8B ;MUS note (B-3, length 8)
 .byt     $60 ;Pause         (length 4)
 .byt     $6B ;MUS note (B-3, length 4)
 .byt     $6C ;MUS note (C-4, length 4)
 .byt     $6E ;MUS note (D-4, length 4)
 .byt MUSCMD_SetNoteBaseOffset, 2*12+0
 .byt $30,$9C ;MUS note (E-4, length 8/1.5) (triplet)
 .byt $30,$98 ;MUS note (C-4, length 8/1.5) (triplet)
 .byt $30,$93 ;MUS note (G-3, length 8/1.5) (triplet)
 .byt $30,$90 ;MUS note (E-3, length 8/1.5) (triplet)
 .byt $30,$8C ;MUS note (C-3, length 8/1.5) (triplet)
 .byt $30,$87 ;MUS note (G-2, length 8/1.5) (triplet)
 .byt $30,$9E ;MUS note (F#4, length 8/1.5) (triplet)
 .byt $30,$9A ;MUS note (D-4, length 8/1.5) (triplet)
 .byt $30,$95 ;MUS note (A-3, length 8/1.5) (triplet)
 .byt $30,$92 ;MUS note (F#3, length 8/1.5) (triplet)
 .byt $30,$8E ;MUS note (D-3, length 8/1.5) (triplet)
 .byt $30,$89 ;MUS note (A-2, length 8/1.5) (triplet)
 .byt MUSCMD_SetNoteBaseOffset, 3*12+0
 .byt $30,$93 ;MUS note (G-4, length 8/1.5) (triplet)
 .byt $30,$90 ;MUS note (E-4, length 8/1.5) (triplet)
 .byt $30,$8C ;MUS note (C-4, length 8/1.5) (triplet)
 .byt MUSCMD_SetNoteBaseOffset, 2*12+0
 .byt $30,$93 ;MUS note (G-3, length 8/1.5) (triplet)
 .byt $30,$90 ;MUS note (E-3, length 8/1.5) (triplet)
 .byt $30,$8C ;MUS note (C-3, length 8/1.5) (triplet)
 .byt MUSCMD_SetNoteBaseOffset, 3*12+0
 .byt $30,$94 ;MUS note (G#4, length 8/1.5) (triplet)
 .byt $30,$8F ;MUS note (D#4, length 8/1.5) (triplet)
 .byt $30,$8C ;MUS note (C-4, length 8/1.5) (triplet)
 .byt MUSCMD_SetNoteBaseOffset, 2*12+0
 .byt $30,$94 ;MUS note (G#3, length 8/1.5) (triplet)
 .byt $30,$8F ;MUS note (D#3, length 8/1.5) (triplet)
 .byt $30,$8C ;MUS note (C-3, length 8/1.5) (triplet)
 .byt MUSCMD_Loop, 0 
 .word :-
MUS_Bombman_chn1: ;at A5BA
 .byt MUSCMD_SetSpeed,          $07
 .byt MUSCMD_SetDutyCycle,      $80
 .byt MUSCMD_SetVolumeAndEnv,   $37
 .byt MUSCMD_SetVibratoIndex,   $00
 .byt MUSCMD_SetVolumeCurve, $A0,$20
 .byt MUSCMD_SetNoteBaseOffset, 3*12+0
 .byt     $A0 ;Pause         (length 16)
 .byt     $8D ;MUS note (C#4, length 8)
 .byt     $8D ;MUS note (C#4, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $8D ;MUS note (C#4, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $8B ;MUS note (B-3, length 8)
 .byt     $EB ;MUS note (B-3, length 64)
 .byt     $A0 ;Pause         (length 16)
 .byt     $8D ;MUS note (C#4, length 8)
 .byt     $8D ;MUS note (C#4, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $8D ;MUS note (C#4, length 8)
 .byt     $A0 ;Pause         (length 16)
 .byt     $6C ;MUS note (C-4, length 4)
 .byt     $6C ;MUS note (C-4, length 4)
 .byt     $6C ;MUS note (C-4, length 4)
 .byt     $6C ;MUS note (C-4, length 4)
 .byt     $80 ;Pause         (length 8)
 .byt     $8C ;MUS note (C-4, length 8)
 .byt     $C0 ;Pause         (length 32)
: ;at A5DD
 .byt MUSCMD_SetDutyCycle,      $C0
 .byt MUSCMD_SetVolumeCurve, $FF,$30
 .byt MUSCMD_SetNoteBaseOffset, 2*12+0
 .byt     $D1 ;MUS note (F-3, length 32)
 .byt     $6D ;MUS note (C#3, length 4)
 .byt     $6F ;MUS note (D#3, length 4)
 .byt     $71 ;MUS note (F-3, length 4)
 .byt     $94 ;MUS note (G#3, length 8)
 .byt     $91 ;MUS note (F-3, length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $72 ;MUS note (F#3, length 4)
 .byt     $D2 ;MUS note (F#3, length 32)
 .byt     $B2 ;MUS note (F#3, length 16)
 .byt     $B2 ;MUS note (F#3, length 16)
 .byt     $D1 ;MUS note (F-3, length 32)
 .byt     $6D ;MUS note (C#3, length 4)
 .byt     $6F ;MUS note (D#3, length 4)
 .byt     $71 ;MUS note (F-3, length 4)
 .byt     $94 ;MUS note (G#3, length 8)
 .byt     $91 ;MUS note (F-3, length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $77 ;MUS note (B-3, length 4)
 .byt     $D7 ;MUS note (B-3, length 32)
 .byt     $B4 ;MUS note (G#3, length 16)
 .byt     $B2 ;MUS note (F#3, length 16)
 .byt     $D1 ;MUS note (F-3, length 32)
 .byt     $6D ;MUS note (C#3, length 4)
 .byt     $6F ;MUS note (D#3, length 4)
 .byt     $71 ;MUS note (F-3, length 4)
 .byt     $94 ;MUS note (G#3, length 8)
 .byt $06,$91 ;MUS note (F-3, length 12)
 .byt     $D4 ;MUS note (G#3, length 32)
 .byt     $70 ;MUS note (E-3, length 4)
 .byt     $72 ;MUS note (F#3, length 4)
 .byt     $74 ;MUS note (G#3, length 4)
 .byt     $97 ;MUS note (B-3, length 8)
 .byt $06,$94 ;MUS note (G#3, length 12)
 .byt     $97 ;MUS note (B-3, length 8)
 .byt     $93 ;MUS note (G-3, length 8)
 .byt     $77 ;MUS note (B-3, length 4)
 .byt $06,$9A ;MUS note (D-4, length 12)
 .byt MUSCMD_SetNoteBaseOffset, 3*12+0
 .byt     $8D ;MUS note (C#4, length 8)
 .byt     $89 ;MUS note (A-3, length 8)
 .byt     $6D ;MUS note (C#4, length 4)
 .byt $06,$90 ;MUS note (E-4, length 12)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $AF ;MUS note (D#4, length 16)
 .byt MUSCMD_SetVibratoCtrl,    $04
 .byt     $AF ;MUS note (D#4, length 16)
 .byt MUSCMD_SetVibratoCtrl,    $00
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $A3 ;MUS note (D#3, length 16)
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt     $A3 ;MUS note (D#3, length 16)
 .byt MUSCMD_SetNoteBaseOffset, 2*12+0
 .byt MUSCMD_SetVibratoIndex,   $00
 .byt MUSCMD_SetDutyCycle,      $40
 .byt MUSCMD_SetVolumeCurve, $8F,$70
 .byt $06,$92 ;MUS note (F#3, length 12)
 .byt $06,$8D ;MUS note (C#3, length 12)
 .byt     $90 ;MUS note (E-3, length 8)
 .byt $06,$92 ;MUS note (F#3, length 12)
 .byt $06,$8D ;MUS note (C#3, length 12)
 .byt     $92 ;MUS note (F#3, length 8)
 .byt $06,$90 ;MUS note (E-3, length 12)
 .byt $06,$8B ;MUS note (B-2, length 12)
 .byt     $8F ;MUS note (D#3, length 8)
 .byt $06,$90 ;MUS note (E-3, length 12)
 .byt $06,$8B ;MUS note (B-2, length 12)
 .byt     $91 ;MUS note (F-3, length 8)
 .byt $06,$92 ;MUS note (F#3, length 12)
 .byt $06,$8D ;MUS note (C#3, length 12)
 .byt     $90 ;MUS note (E-3, length 8)
 .byt $06,$92 ;MUS note (F#3, length 12)
 .byt $06,$8D ;MUS note (C#3, length 12)
 .byt     $93 ;MUS note (G-3, length 8)
 .byt MUSCMD_SetDutyCycle,      $80
 .byt MUSCMD_SetVolumeCurve, $80,$00
 .byt $06,$88 ;MUS note (G#2, length 12)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $68 ;MUS note (G#2, length 4)
 .byt     $88 ;MUS note (G#2, length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt $06,$8D ;MUS note (C#3, length 12)
 .byt     $6D ;MUS note (C#3, length 4)
 .byt     $92 ;MUS note (F#3, length 8)
 .byt     $97 ;MUS note (B-3, length 8)
 .byt     $9C ;MUS note (E-4, length 8)
 .byt MUSCMD_SetNoteBaseOffset, 3*12+0
 .byt     $91 ;MUS note (F-4, length 8)
 .byt     $6F ;MUS note (D#4, length 4)
 .byt     $71 ;MUS note (F-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $6F ;MUS note (D#4, length 4)
 .byt     $71 ;MUS note (F-4, length 4)
 .byt     $6F ;MUS note (D#4, length 4)
 .byt     $8D ;MUS note (C#4, length 8)
 .byt     $88 ;MUS note (G#3, length 8)
 .byt     $60 ;Pause         (length 4)
 .byt     $68 ;MUS note (G#3, length 4)
 .byt     $68 ;MUS note (G#3, length 4)
 .byt     $6D ;MUS note (C#4, length 4)
 .byt     $8E ;MUS note (D-4, length 8)
 .byt     $6D ;MUS note (C#4, length 4)
 .byt     $6E ;MUS note (D-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $6D ;MUS note (C#4, length 4)
 .byt     $6E ;MUS note (D-4, length 4)
 .byt     $6D ;MUS note (C#4, length 4)
 .byt     $8B ;MUS note (B-3, length 8)
 .byt     $86 ;MUS note (F#3, length 8)
 .byt     $60 ;Pause         (length 4)
 .byt     $66 ;MUS note (F#3, length 4)
 .byt     $66 ;MUS note (F#3, length 4)
 .byt     $6B ;MUS note (B-3, length 4)
 .byt MUSCMD_SetNoteBaseOffset, 2*12+0
 .byt $30,$98 ;MUS note (C-4, length 8/1.5) (triplet)
 .byt $30,$93 ;MUS note (G-3, length 8/1.5) (triplet)
 .byt $30,$90 ;MUS note (E-3, length 8/1.5) (triplet)
 .byt $30,$8C ;MUS note (C-3, length 8/1.5) (triplet)
 .byt $30,$87 ;MUS note (G-2, length 8/1.5) (triplet)
 .byt $30,$84 ;MUS note (E-2, length 8/1.5) (triplet)
 .byt $30,$9A ;MUS note (D-4, length 8/1.5) (triplet)
 .byt $30,$95 ;MUS note (A-3, length 8/1.5) (triplet)
 .byt $30,$92 ;MUS note (F#3, length 8/1.5) (triplet)
 .byt $30,$8E ;MUS note (D-3, length 8/1.5) (triplet)
 .byt $30,$89 ;MUS note (A-2, length 8/1.5) (triplet)
 .byt $30,$86 ;MUS note (F#2, length 8/1.5) (triplet)
 .byt MUSCMD_SetNoteBaseOffset, 3*12+0
 .byt $30,$90 ;MUS note (E-4, length 8/1.5) (triplet)
 .byt $30,$8C ;MUS note (C-4, length 8/1.5) (triplet)
 .byt $30,$86 ;MUS note (F#3, length 8/1.5) (triplet)
 .byt MUSCMD_SetNoteBaseOffset, 2*12+0
 .byt $30,$90 ;MUS note (E-3, length 8/1.5) (triplet)
 .byt $30,$8C ;MUS note (C-3, length 8/1.5) (triplet)
 .byt $30,$86 ;MUS note (F#2, length 8/1.5) (triplet)
 .byt MUSCMD_SetNoteBaseOffset, 3*12+0
 .byt $30,$92 ;MUS note (F#4, length 8/1.5) (triplet)
 .byt $30,$8F ;MUS note (D#4, length 8/1.5) (triplet)
 .byt $30,$88 ;MUS note (G#3, length 8/1.5) (triplet)
 .byt MUSCMD_SetNoteBaseOffset, 2*12+0
 .byt $30,$92 ;MUS note (F#3, length 8/1.5) (triplet)
 .byt $30,$8F ;MUS note (D#3, length 8/1.5) (triplet)
 .byt $30,$88 ;MUS note (G#2, length 8/1.5) (triplet)
 .byt MUSCMD_Loop, 0 
 .word :-
 .byt MUSCMD_EndOfData
MUS_Bombman_chn2: ;at A6B3
 .byt MUSCMD_SetSpeed,          $07
 .byt MUSCMD_SetNoteBaseOffset, 2*12+0
 .byt MUSCMD_SetVolumeAndEnv,   $81
 .byt     $8D ;MUS note (C#3, length 8)
 .byt     $6D ;MUS note (C#3, length 4)
 .byt     $6D ;MUS note (C#3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $6D ;MUS note (C#3, length 4)
 .byt     $6D ;MUS note (C#3, length 4)
 .byt     $6D ;MUS note (C#3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $6D ;MUS note (C#3, length 4)
 .byt     $6D ;MUS note (C#3, length 4)
 .byt     $6D ;MUS note (C#3, length 4)
 .byt     $80 ;Pause         (length 8)
 .byt     $6D ;MUS note (C#3, length 4)
 .byt     $6D ;MUS note (C#3, length 4)
 .byt     $8D ;MUS note (C#3, length 8)
 .byt     $6D ;MUS note (C#3, length 4)
 .byt     $6D ;MUS note (C#3, length 4)
 .byt     $80 ;Pause         (length 8)
 .byt     $88 ;MUS note (G#2, length 8)
 .byt     $8B ;MUS note (B-2, length 8)
 .byt     $88 ;MUS note (G#2, length 8)
 .byt     $8B ;MUS note (B-2, length 8)
 .byt     $8C ;MUS note (C-3, length 8)
 .byt     $8D ;MUS note (C#3, length 8)
 .byt     $6D ;MUS note (C#3, length 4)
 .byt     $6D ;MUS note (C#3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $6D ;MUS note (C#3, length 4)
 .byt     $6D ;MUS note (C#3, length 4)
 .byt     $6D ;MUS note (C#3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $6D ;MUS note (C#3, length 4)
 .byt     $6D ;MUS note (C#3, length 4)
 .byt     $6D ;MUS note (C#3, length 4)
 .byt     $80 ;Pause         (length 8)
 .byt     $6D ;MUS note (C#3, length 4)
 .byt     $6D ;MUS note (C#3, length 4)
 .byt     $68 ;MUS note (G#2, length 4)
 .byt     $68 ;MUS note (G#2, length 4)
 .byt     $68 ;MUS note (G#2, length 4)
 .byt     $68 ;MUS note (G#2, length 4)
 .byt     $80 ;Pause         (length 8)
 .byt     $88 ;MUS note (G#2, length 8)
 .byt MUSCMD_SetNoteBaseOffset, 2*12+7
 .byt MUSCMD_SetVibratoCtrl,    $0F
 .byt MUSCMD_SetVolumeAndEnv,   $09
 .byt     $60 ;Pause         (length 4)
 .byt     $74 ;MUS note (D#4, length 4)
 .byt     $74 ;MUS note (D#4, length 4)
 .byt     $6D ;MUS note (G#3, length 4)
 .byt     $6D ;MUS note (G#3, length 4)
 .byt     $68 ;MUS note (D#3, length 4)
 .byt     $68 ;MUS note (D#3, length 4)
 .byt     $61 ;MUS note (G#2, length 4)
: ;at A6F2
 .byt MUSCMD_SetVibratoCtrl,    $00
 .byt MUSCMD_SetNoteBaseOffset, 2*12+0
 .byt MUSCMD_SetVolumeAndEnv,   $81
: ;at A6F8
 .byt     $8D ;MUS note (C#3, length 8)
 .byt     $6D ;MUS note (C#3, length 4)
 .byt     $6D ;MUS note (C#3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $6D ;MUS note (C#3, length 4)
 .byt     $6D ;MUS note (C#3, length 4)
 .byt     $6D ;MUS note (C#3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $6D ;MUS note (C#3, length 4)
 .byt     $6D ;MUS note (C#3, length 4)
 .byt     $6D ;MUS note (C#3, length 4)
 .byt     $80 ;Pause         (length 8)
 .byt     $6B ;MUS note (B-2, length 4)
 .byt     $6D ;MUS note (C#3, length 4)
 .byt     $8B ;MUS note (B-2, length 8)
 .byt     $6B ;MUS note (B-2, length 4)
 .byt     $6B ;MUS note (B-2, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $6B ;MUS note (B-2, length 4)
 .byt     $6B ;MUS note (B-2, length 4)
 .byt     $6B ;MUS note (B-2, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $6B ;MUS note (B-2, length 4)
 .byt     $6B ;MUS note (B-2, length 4)
 .byt     $6B ;MUS note (B-2, length 4)
 .byt     $80 ;Pause         (length 8)
 .byt     $69 ;MUS note (A-2, length 4)
 .byt     $6B ;MUS note (B-2, length 4)
 .byt MUSCMD_Loop, 1 
 .word :-
 .byt     $8D ;MUS note (C#3, length 8)
 .byt     $6D ;MUS note (C#3, length 4)
 .byt     $6D ;MUS note (C#3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $6D ;MUS note (C#3, length 4)
 .byt     $6D ;MUS note (C#3, length 4)
 .byt     $6D ;MUS note (C#3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $6D ;MUS note (C#3, length 4)
 .byt     $6D ;MUS note (C#3, length 4)
 .byt     $6D ;MUS note (C#3, length 4)
 .byt     $80 ;Pause         (length 8)
 .byt     $6B ;MUS note (B-2, length 4)
 .byt     $6D ;MUS note (C#3, length 4)
 .byt     $90 ;MUS note (E-3, length 8)
 .byt     $70 ;MUS note (E-3, length 4)
 .byt     $70 ;MUS note (E-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $70 ;MUS note (E-3, length 4)
 .byt     $70 ;MUS note (E-3, length 4)
 .byt     $70 ;MUS note (E-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $70 ;MUS note (E-3, length 4)
 .byt     $70 ;MUS note (E-3, length 4)
 .byt     $70 ;MUS note (E-3, length 4)
 .byt     $80 ;Pause         (length 8)
 .byt     $6E ;MUS note (D-3, length 4)
 .byt     $70 ;MUS note (E-3, length 4)
 .byt $30,$B3 ;MUS note (G-3, length 16/1.5) (triplet)
 .byt $30,$B3 ;MUS note (G-3, length 16/1.5) (triplet)
 .byt $30,$B3 ;MUS note (G-3, length 16/1.5) (triplet)
 .byt $30,$B5 ;MUS note (A-3, length 16/1.5) (triplet)
 .byt $30,$B5 ;MUS note (A-3, length 16/1.5) (triplet)
 .byt $30,$B5 ;MUS note (A-3, length 16/1.5) (triplet)
 .byt     $F7 ;MUS note (B-3, length 64)
: ;at A741
 .byt     $92 ;MUS note (F#3, length 8)
 .byt MUSCMD_Loop, 7 
 .word :-
: ;at A746
 .byt     $90 ;MUS note (E-3, length 8)
 .byt MUSCMD_Loop, 7 
 .word :-
: ;at A74B
 .byt     $92 ;MUS note (F#3, length 8)
 .byt MUSCMD_Loop, 3 
 .word :-
: ;at A750
 .byt     $93 ;MUS note (G-3, length 8)
 .byt MUSCMD_Loop, 3 
 .word :-
: ;at A755
 .byt     $94 ;MUS note (G#3, length 8)
 .byt MUSCMD_Loop, 7 
 .word :-
: ;at A75A
 .byt     $8D ;MUS note (C#3, length 8)
 .byt MUSCMD_Loop, 7 
 .word :-
: ;at A75F
 .byt     $8B ;MUS note (B-2, length 8)
 .byt MUSCMD_Loop, 7 
 .word :-
: ;at A764
 .byt     $8C ;MUS note (C-3, length 8)
 .byt MUSCMD_Loop, 3 
 .word :-
: ;at A769
 .byt     $8E ;MUS note (D-3, length 8)
 .byt MUSCMD_Loop, 3 
 .word :-
 .byt MUSCMD_SetNoteBaseOffset, 2*12+7
 .byt MUSCMD_SetVibratoCtrl,    $0F
 .byt MUSCMD_SetVolumeAndEnv,   $09
 .byt     $74 ;MUS note (D#4, length 4)
 .byt     $74 ;MUS note (D#4, length 4)
 .byt     $74 ;MUS note (D#4, length 4)
 .byt     $74 ;MUS note (D#4, length 4)
 .byt     $8D ;MUS note (G#3, length 8)
 .byt     $74 ;MUS note (D#4, length 4)
 .byt     $68 ;MUS note (D#3, length 4)
 .byt     $6D ;MUS note (G#3, length 4)
 .byt     $74 ;MUS note (D#4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $6D ;MUS note (G#3, length 4)
 .byt     $74 ;MUS note (D#4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $68 ;MUS note (D#3, length 4)
 .byt     $68 ;MUS note (D#3, length 4)
 .byt MUSCMD_Loop, 0 
 .word :-----------
MUS_Bombman_chn3: ;at A787
 .byt MUSCMD_SetSpeed,          $07
 .byt MUSCMD_SetVolumeCurve, $81,$10
 .byt MUSCMD_SetVolumeAndEnv,   $39
: ;at A78E
 .byt     $80 ;Pause         (length 8)
 .byt     $83 ;MUS note (A#2, length 8)
 .byt     $89 ;MUS note (E-3, length 8)
 .byt     $63 ;MUS note (A#2, length 4)
 .byt     $63 ;MUS note (A#2, length 4)
 .byt     $83 ;MUS note (A#2, length 8)
 .byt     $63 ;MUS note (A#2, length 4)
 .byt     $63 ;MUS note (A#2, length 4)
 .byt     $89 ;MUS note (E-3, length 8)
 .byt     $63 ;MUS note (A#2, length 4)
 .byt     $63 ;MUS note (A#2, length 4)
 .byt     $83 ;MUS note (A#2, length 8)
 .byt     $69 ;MUS note (E-3, length 4)
 .byt     $63 ;MUS note (A#2, length 4)
 .byt     $89 ;MUS note (E-3, length 8)
 .byt     $63 ;MUS note (A#2, length 4)
 .byt     $63 ;MUS note (A#2, length 4)
 .byt     $63 ;MUS note (A#2, length 4)
 .byt     $63 ;MUS note (A#2, length 4)
 .byt     $63 ;MUS note (A#2, length 4)
 .byt     $63 ;MUS note (A#2, length 4)
 .byt     $89 ;MUS note (E-3, length 8)
 .byt     $69 ;MUS note (E-3, length 4)
 .byt     $69 ;MUS note (E-3, length 4)
 .byt MUSCMD_Loop, 1 
 .word :-
: ;at A7AA
 .byt MUSCMD_SetVolumeCurve, $81,$10
 .byt MUSCMD_SetVolumeAndEnv,   $39
 .byt     $80 ;Pause         (length 8)
 .byt     $83 ;MUS note (A#2, length 8)
 .byt     $89 ;MUS note (E-3, length 8)
 .byt     $63 ;MUS note (A#2, length 4)
 .byt     $63 ;MUS note (A#2, length 4)
 .byt     $83 ;MUS note (A#2, length 8)
 .byt     $63 ;MUS note (A#2, length 4)
 .byt     $63 ;MUS note (A#2, length 4)
 .byt     $89 ;MUS note (E-3, length 8)
 .byt     $63 ;MUS note (A#2, length 4)
 .byt     $63 ;MUS note (A#2, length 4)
 .byt     $83 ;MUS note (A#2, length 8)
 .byt     $69 ;MUS note (E-3, length 4)
 .byt     $63 ;MUS note (A#2, length 4)
 .byt     $89 ;MUS note (E-3, length 8)
 .byt     $63 ;MUS note (A#2, length 4)
 .byt     $63 ;MUS note (A#2, length 4)
 .byt     $63 ;MUS note (A#2, length 4)
 .byt     $63 ;MUS note (A#2, length 4)
 .byt     $63 ;MUS note (A#2, length 4)
 .byt     $63 ;MUS note (A#2, length 4)
 .byt     $89 ;MUS note (E-3, length 8)
 .byt     $69 ;MUS note (E-3, length 4)
 .byt     $69 ;MUS note (E-3, length 4)
 .byt MUSCMD_Loop, 3 
 .word :-
: ;at A7CB
 .byt     $80 ;Pause         (length 8)
 .byt     $83 ;MUS note (A#2, length 8)
 .byt     $89 ;MUS note (E-3, length 8)
 .byt     $63 ;MUS note (A#2, length 4)
 .byt     $63 ;MUS note (A#2, length 4)
 .byt     $83 ;MUS note (A#2, length 8)
 .byt     $63 ;MUS note (A#2, length 4)
 .byt     $63 ;MUS note (A#2, length 4)
 .byt     $89 ;MUS note (E-3, length 8)
 .byt     $63 ;MUS note (A#2, length 4)
 .byt     $63 ;MUS note (A#2, length 4)
 .byt     $83 ;MUS note (A#2, length 8)
 .byt     $69 ;MUS note (E-3, length 4)
 .byt     $63 ;MUS note (A#2, length 4)
 .byt     $89 ;MUS note (E-3, length 8)
 .byt     $63 ;MUS note (A#2, length 4)
 .byt     $63 ;MUS note (A#2, length 4)
 .byt     $63 ;MUS note (A#2, length 4)
 .byt     $63 ;MUS note (A#2, length 4)
 .byt     $63 ;MUS note (A#2, length 4)
 .byt     $63 ;MUS note (A#2, length 4)
 .byt     $89 ;MUS note (E-3, length 8)
 .byt     $69 ;MUS note (E-3, length 4)
 .byt     $69 ;MUS note (E-3, length 4)
 .byt MUSCMD_Loop, 3 
 .word :-
 .byt MUSCMD_Loop, 0 
 .word :--
MUS_Bombman_vibratotable: ;at A7EB
 .byt $00, $E0, $80, $00 ;vibrato definition
 .byt $01, $62, $80, $00 ;vibrato definition
.endscope

MUS_Elecman: ;at A7F3
.scope
 .byt $0F ;priority. Lo=music priority, Hi=sfx priority
 .word MUS_Elecman_chn0               ; $A7FE ;
 .word MUS_Elecman_chn1               ; $A8C4 ;
 .word MUS_Elecman_chn2               ; $A99E ;
 .word MUS_Elecman_chn3               ; $AA51 ;
 .word MUS_Elecman_vibratotable       ; $AA69 ;
MUS_Elecman_chn0: ;at A7FE
 .byt MUSCMD_SetSpeed,          $06
 .byt MUSCMD_SetNoteBaseOffset, 1*12+9
 .byt MUSCMD_SetDutyCycle,      $80
 .byt MUSCMD_SetVolumeAndEnv,   $36
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt MUSCMD_SetVolumeCurve, $8A,$40
 .byt     $E0 ;Pause         (length 64)
 .byt     $C0 ;Pause         (length 32)
 .byt     $A0 ;Pause         (length 16)
 .byt $30,$89 ;MUS note (F#2, length 8/1.5) (triplet)
 .byt $30,$8B ;MUS note (G#2, length 8/1.5) (triplet)
 .byt $30,$8D ;MUS note (A#2, length 8/1.5) (triplet)
: ;at A814
 .byt MUSCMD_SetVolumeAndEnv,   $38
 .byt MUSCMD_SetDutyCycle,      $40
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt MUSCMD_SetVolumeCurve, $CA,$F0
 .byt MUSCMD_SetNoteBaseOffset, 2*12+9
 .byt     $82 ;MUS note (B-2, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt $06,$B2 ;MUS note (D#4, length 24)
 .byt $06,$B0 ;MUS note (C#4, length 24)
 .byt $06,$B3 ;MUS note (E-4, length 24)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt $06,$B2 ;MUS note (D#4, length 24)
 .byt     $B2 ;MUS note (D#4, length 16)
 .byt     $A0 ;Pause         (length 16)
 .byt $06,$B2 ;MUS note (D#4, length 24)
 .byt $06,$B0 ;MUS note (C#4, length 24)
 .byt $06,$B3 ;MUS note (E-4, length 24)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt $06,$B2 ;MUS note (D#4, length 24)
 .byt     $B2 ;MUS note (D#4, length 16)
 .byt     $A0 ;Pause         (length 16)
 .byt $06,$B2 ;MUS note (D#4, length 24)
 .byt $06,$B0 ;MUS note (C#4, length 24)
 .byt $06,$B3 ;MUS note (E-4, length 24)
 .byt $06,$B2 ;MUS note (D#4, length 24)
 .byt     $B0 ;MUS note (C#4, length 16)
 .byt     $EE ;MUS note (B-3, length 64)
 .byt $06,$AE ;MUS note (B-3, length 24)
 .byt     $8D ;MUS note (A#3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $8B ;MUS note (G#3, length 8)
 .byt     $89 ;MUS note (F#3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $A0 ;Pause         (length 16)
 .byt $06,$B2 ;MUS note (D#4, length 24)
 .byt $06,$B0 ;MUS note (C#4, length 24)
 .byt $06,$B3 ;MUS note (E-4, length 24)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt $06,$B2 ;MUS note (D#4, length 24)
 .byt     $B2 ;MUS note (D#4, length 16)
 .byt     $A0 ;Pause         (length 16)
 .byt $06,$B2 ;MUS note (D#4, length 24)
 .byt $06,$B0 ;MUS note (C#4, length 24)
 .byt $06,$B3 ;MUS note (E-4, length 24)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt $06,$B2 ;MUS note (D#4, length 24)
 .byt     $B2 ;MUS note (D#4, length 16)
 .byt     $A0 ;Pause         (length 16)
 .byt $06,$B2 ;MUS note (D#4, length 24)
 .byt $06,$B0 ;MUS note (C#4, length 24)
 .byt $06,$B3 ;MUS note (E-4, length 24)
 .byt $06,$B2 ;MUS note (D#4, length 24)
 .byt     $B0 ;MUS note (C#4, length 16)
 .byt     $EE ;MUS note (B-3, length 64)
 .byt MUSCMD_SetVibratoIndex,   $00
 .byt     $80 ;Pause         (length 8)
 .byt     $8D ;MUS note (A#3, length 8)
 .byt     $95 ;MUS note (F#4, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $C0 ;Pause         (length 32)
 .byt MUSCMD_SetDutyCycle,      $40
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt MUSCMD_SetVolumeCurve, $86,$20
 .byt MUSCMD_SetVolumeAndEnv,   $36
 .byt     $9A ;MUS note (B-4, length 8)
 .byt     $9A ;MUS note (B-4, length 8)
 .byt     $9A ;MUS note (B-4, length 8)
 .byt     $9A ;MUS note (B-4, length 8)
 .byt     $9A ;MUS note (B-4, length 8)
 .byt     $9A ;MUS note (B-4, length 8)
 .byt     $9A ;MUS note (B-4, length 8)
 .byt     $9A ;MUS note (B-4, length 8)
 .byt     $98 ;MUS note (A-4, length 8)
 .byt     $98 ;MUS note (A-4, length 8)
 .byt     $98 ;MUS note (A-4, length 8)
 .byt     $98 ;MUS note (A-4, length 8)
 .byt     $98 ;MUS note (A-4, length 8)
 .byt     $98 ;MUS note (A-4, length 8)
 .byt     $98 ;MUS note (A-4, length 8)
 .byt     $98 ;MUS note (A-4, length 8)
 .byt     $9A ;MUS note (B-4, length 8)
 .byt     $9A ;MUS note (B-4, length 8)
 .byt     $9A ;MUS note (B-4, length 8)
 .byt     $9A ;MUS note (B-4, length 8)
 .byt     $9A ;MUS note (B-4, length 8)
 .byt     $9A ;MUS note (B-4, length 8)
 .byt     $9A ;MUS note (B-4, length 8)
 .byt     $9A ;MUS note (B-4, length 8)
 .byt     $9C ;MUS note (C#5, length 8)
 .byt     $9C ;MUS note (C#5, length 8)
 .byt     $9C ;MUS note (C#5, length 8)
 .byt     $9C ;MUS note (C#5, length 8)
 .byt     $9C ;MUS note (C#5, length 8)
 .byt     $9C ;MUS note (C#5, length 8)
 .byt     $9C ;MUS note (C#5, length 8)
 .byt     $9C ;MUS note (C#5, length 8)
 .byt     $9D ;MUS note (D-5, length 8)
 .byt     $9D ;MUS note (D-5, length 8)
 .byt     $9D ;MUS note (D-5, length 8)
 .byt     $9D ;MUS note (D-5, length 8)
 .byt     $9D ;MUS note (D-5, length 8)
 .byt     $9D ;MUS note (D-5, length 8)
 .byt     $9D ;MUS note (D-5, length 8)
 .byt     $9D ;MUS note (D-5, length 8)
 .byt     $9B ;MUS note (C-5, length 8)
 .byt     $9B ;MUS note (C-5, length 8)
 .byt     $9B ;MUS note (C-5, length 8)
 .byt     $9B ;MUS note (C-5, length 8)
 .byt     $9B ;MUS note (C-5, length 8)
 .byt     $9B ;MUS note (C-5, length 8)
 .byt     $9B ;MUS note (C-5, length 8)
 .byt     $9B ;MUS note (C-5, length 8)
 .byt     $99 ;MUS note (A#4, length 8)
 .byt     $99 ;MUS note (A#4, length 8)
 .byt     $99 ;MUS note (A#4, length 8)
 .byt     $99 ;MUS note (A#4, length 8)
 .byt     $99 ;MUS note (A#4, length 8)
 .byt     $99 ;MUS note (A#4, length 8)
 .byt     $99 ;MUS note (A#4, length 8)
 .byt     $99 ;MUS note (A#4, length 8)
 .byt MUSCMD_SetNoteBaseOffset, 3*12+9
 .byt     $75 ;MUS note (F#5, length 4)
 .byt     $75 ;MUS note (F#5, length 4)
 .byt     $75 ;MUS note (F#5, length 4)
 .byt     $75 ;MUS note (F#5, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $75 ;MUS note (F#5, length 4)
 .byt     $75 ;MUS note (F#5, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $75 ;MUS note (F#5, length 4)
 .byt     $75 ;MUS note (F#5, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $A0 ;Pause         (length 16)
 .byt MUSCMD_Loop, 0 
 .word :-
MUS_Elecman_chn1: ;at A8C4
 .byt MUSCMD_SetSpeed,          $06
 .byt MUSCMD_SetDutyCycle,      $40
 .byt MUSCMD_SetVolumeAndEnv,   $37
 .byt MUSCMD_SetVolumeCurve, $CA,$F0
 .byt MUSCMD_SetNoteBaseOffset, 1*12+9
 .byt     $E0 ;Pause         (length 64)
 .byt     $C0 ;Pause         (length 32)
 .byt     $A0 ;Pause         (length 16)
 .byt $30,$89 ;MUS note (F#2, length 8/1.5) (triplet)
 .byt $30,$88 ;MUS note (F-2, length 8/1.5) (triplet)
 .byt $30,$87 ;MUS note (E-2, length 8/1.5) (triplet)
: ;at A8D8
 .byt MUSCMD_SetVolumeAndEnv,   $38
 .byt MUSCMD_SetDutyCycle,      $40
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt MUSCMD_SetVolumeCurve, $CA,$F0
 .byt MUSCMD_SetNoteBaseOffset, 2*12+9
 .byt     $86 ;MUS note (D#3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt $06,$AE ;MUS note (B-3, length 24)
 .byt $06,$AE ;MUS note (B-3, length 24)
 .byt $06,$AE ;MUS note (B-3, length 24)
 .byt $06,$AE ;MUS note (B-3, length 24)
 .byt     $7E ;MUS note (D#5, length 4)
 .byt     $7A ;MUS note (B-4, length 4)
 .byt     $75 ;MUS note (F#4, length 4)
 .byt     $73 ;MUS note (E-4, length 4)
 .byt     $98 ;MUS note (A-4, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt $06,$AE ;MUS note (B-3, length 24)
 .byt $06,$AE ;MUS note (B-3, length 24)
 .byt $06,$AE ;MUS note (B-3, length 24)
 .byt $06,$AE ;MUS note (B-3, length 24)
 .byt     $7E ;MUS note (D#5, length 4)
 .byt     $7A ;MUS note (B-4, length 4)
 .byt     $75 ;MUS note (F#4, length 4)
 .byt     $73 ;MUS note (E-4, length 4)
 .byt     $97 ;MUS note (G#4, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt $06,$AB ;MUS note (G#3, length 24)
 .byt $06,$AB ;MUS note (G#3, length 24)
 .byt $06,$AB ;MUS note (G#3, length 24)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt $06,$AB ;MUS note (G#3, length 24)
 .byt     $AB ;MUS note (G#3, length 16)
 .byt     $EA ;MUS note (G-3, length 64)
 .byt $06,$A9 ;MUS note (F#3, length 24)
 .byt     $89 ;MUS note (F#3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $8B ;MUS note (G#3, length 8)
 .byt     $89 ;MUS note (F#3, length 8)
 .byt MUSCMD_SetVibratoIndex,   $00
 .byt MUSCMD_SetNoteBaseOffset, 3*12+9
 .byt     $95 ;MUS note (F#5, length 8)
 .byt MUSCMD_SetNoteBaseOffset, 2*12+9
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt     $A0 ;Pause         (length 16)
 .byt $06,$AE ;MUS note (B-3, length 24)
 .byt $06,$AE ;MUS note (B-3, length 24)
 .byt $06,$AE ;MUS note (B-3, length 24)
 .byt $06,$AE ;MUS note (B-3, length 24)
 .byt     $7E ;MUS note (D#5, length 4)
 .byt     $7A ;MUS note (B-4, length 4)
 .byt     $75 ;MUS note (F#4, length 4)
 .byt     $73 ;MUS note (E-4, length 4)
 .byt     $98 ;MUS note (A-4, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt $06,$AE ;MUS note (B-3, length 24)
 .byt $06,$AE ;MUS note (B-3, length 24)
 .byt $06,$AE ;MUS note (B-3, length 24)
 .byt $06,$AE ;MUS note (B-3, length 24)
 .byt     $7E ;MUS note (D#5, length 4)
 .byt     $7A ;MUS note (B-4, length 4)
 .byt     $75 ;MUS note (F#4, length 4)
 .byt     $73 ;MUS note (E-4, length 4)
 .byt     $97 ;MUS note (G#4, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt $06,$AB ;MUS note (G#3, length 24)
 .byt $06,$AB ;MUS note (G#3, length 24)
 .byt $06,$AB ;MUS note (G#3, length 24)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt $06,$AB ;MUS note (G#3, length 24)
 .byt     $AB ;MUS note (G#3, length 16)
 .byt     $EA ;MUS note (G-3, length 64)
 .byt MUSCMD_SetVibratoIndex,   $00
 .byt     $80 ;Pause         (length 8)
 .byt     $89 ;MUS note (F#3, length 8)
 .byt     $95 ;MUS note (F#4, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $C0 ;Pause         (length 32)
 .byt MUSCMD_SetDutyCycle,      $40
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt MUSCMD_SetVolumeCurve, $86,$20
 .byt MUSCMD_SetVolumeAndEnv,   $36
 .byt     $95 ;MUS note (F#4, length 8)
 .byt     $95 ;MUS note (F#4, length 8)
 .byt     $95 ;MUS note (F#4, length 8)
 .byt     $95 ;MUS note (F#4, length 8)
 .byt     $95 ;MUS note (F#4, length 8)
 .byt     $95 ;MUS note (F#4, length 8)
 .byt     $95 ;MUS note (F#4, length 8)
 .byt     $95 ;MUS note (F#4, length 8)
 .byt     $93 ;MUS note (E-4, length 8)
 .byt     $93 ;MUS note (E-4, length 8)
 .byt     $93 ;MUS note (E-4, length 8)
 .byt     $93 ;MUS note (E-4, length 8)
 .byt     $93 ;MUS note (E-4, length 8)
 .byt     $93 ;MUS note (E-4, length 8)
 .byt     $93 ;MUS note (E-4, length 8)
 .byt     $93 ;MUS note (E-4, length 8)
 .byt     $95 ;MUS note (F#4, length 8)
 .byt     $95 ;MUS note (F#4, length 8)
 .byt     $95 ;MUS note (F#4, length 8)
 .byt     $95 ;MUS note (F#4, length 8)
 .byt     $95 ;MUS note (F#4, length 8)
 .byt     $95 ;MUS note (F#4, length 8)
 .byt     $95 ;MUS note (F#4, length 8)
 .byt     $95 ;MUS note (F#4, length 8)
 .byt     $98 ;MUS note (A-4, length 8)
 .byt     $98 ;MUS note (A-4, length 8)
 .byt     $98 ;MUS note (A-4, length 8)
 .byt     $98 ;MUS note (A-4, length 8)
 .byt     $98 ;MUS note (A-4, length 8)
 .byt     $98 ;MUS note (A-4, length 8)
 .byt     $98 ;MUS note (A-4, length 8)
 .byt     $98 ;MUS note (A-4, length 8)
 .byt     $98 ;MUS note (A-4, length 8)
 .byt     $98 ;MUS note (A-4, length 8)
 .byt     $98 ;MUS note (A-4, length 8)
 .byt     $98 ;MUS note (A-4, length 8)
 .byt     $98 ;MUS note (A-4, length 8)
 .byt     $98 ;MUS note (A-4, length 8)
 .byt     $98 ;MUS note (A-4, length 8)
 .byt     $98 ;MUS note (A-4, length 8)
 .byt     $96 ;MUS note (G-4, length 8)
 .byt     $96 ;MUS note (G-4, length 8)
 .byt     $96 ;MUS note (G-4, length 8)
 .byt     $96 ;MUS note (G-4, length 8)
 .byt     $96 ;MUS note (G-4, length 8)
 .byt     $96 ;MUS note (G-4, length 8)
 .byt     $96 ;MUS note (G-4, length 8)
 .byt     $96 ;MUS note (G-4, length 8)
 .byt     $94 ;MUS note (F-4, length 8)
 .byt     $94 ;MUS note (F-4, length 8)
 .byt     $94 ;MUS note (F-4, length 8)
 .byt     $94 ;MUS note (F-4, length 8)
 .byt     $94 ;MUS note (F-4, length 8)
 .byt     $94 ;MUS note (F-4, length 8)
 .byt     $94 ;MUS note (F-4, length 8)
 .byt     $94 ;MUS note (F-4, length 8)
 .byt MUSCMD_SetNoteBaseOffset, 2*12+9
 .byt     $7A ;MUS note (B-4, length 4)
 .byt     $7A ;MUS note (B-4, length 4)
 .byt     $7A ;MUS note (B-4, length 4)
 .byt     $7A ;MUS note (B-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $7A ;MUS note (B-4, length 4)
 .byt     $7A ;MUS note (B-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $7A ;MUS note (B-4, length 4)
 .byt     $7A ;MUS note (B-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $A0 ;Pause         (length 16)
 .byt MUSCMD_Loop, 0 
 .word :-
MUS_Elecman_chn2: ;at A99E
 .byt MUSCMD_SetSpeed,          $06
: ;at A9A0
 .byt MUSCMD_SetVolumeAndEnv,   $24
 .byt MUSCMD_SetNoteBaseOffset, 4*12+7
 .byt MUSCMD_SetVibratoCtrl,    $2F
 .byt     $82 ;MUS note (A-4, length 8)
 .byt     $82 ;MUS note (A-4, length 8)
 .byt     $60 ;Pause         (length 4)
 .byt $06,$82 ;MUS note (A-4, length 12)
 .byt     $A2 ;MUS note (A-4, length 16)
 .byt     $A0 ;Pause         (length 16)
 .byt MUSCMD_Loop, 1 
 .word :-
: ;at A9B1
 .byt MUSCMD_SetVolumeAndEnv,   $7F
 .byt MUSCMD_SetVibratoCtrl,    $00
 .byt MUSCMD_SetVibratoIndex,   $00
 .byt MUSCMD_SetNoteBaseOffset, 1*12+9
 .byt     $8E ;MUS note (B-2, length 8)
 .byt     $95 ;MUS note (F#3, length 8)
 .byt     $8E ;MUS note (B-2, length 8)
 .byt     $95 ;MUS note (F#3, length 8)
 .byt     $8E ;MUS note (B-2, length 8)
 .byt     $95 ;MUS note (F#3, length 8)
 .byt     $8E ;MUS note (B-2, length 8)
 .byt     $95 ;MUS note (F#3, length 8)
 .byt     $8E ;MUS note (B-2, length 8)
 .byt     $95 ;MUS note (F#3, length 8)
 .byt     $8E ;MUS note (B-2, length 8)
 .byt     $95 ;MUS note (F#3, length 8)
 .byt     $8E ;MUS note (B-2, length 8)
 .byt     $95 ;MUS note (F#3, length 8)
 .byt     $8E ;MUS note (B-2, length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $8C ;MUS note (A-2, length 8)
 .byt     $8C ;MUS note (A-2, length 8)
 .byt     $95 ;MUS note (F#3, length 8)
 .byt     $8C ;MUS note (A-2, length 8)
 .byt     $95 ;MUS note (F#3, length 8)
 .byt     $8C ;MUS note (A-2, length 8)
 .byt     $95 ;MUS note (F#3, length 8)
 .byt     $8C ;MUS note (A-2, length 8)
 .byt     $95 ;MUS note (F#3, length 8)
 .byt     $8C ;MUS note (A-2, length 8)
 .byt     $95 ;MUS note (F#3, length 8)
 .byt     $8C ;MUS note (A-2, length 8)
 .byt     $95 ;MUS note (F#3, length 8)
 .byt     $8C ;MUS note (A-2, length 8)
 .byt     $95 ;MUS note (F#3, length 8)
 .byt     $8C ;MUS note (A-2, length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $8B ;MUS note (G#2, length 8)
 .byt     $8B ;MUS note (G#2, length 8)
 .byt     $93 ;MUS note (E-3, length 8)
 .byt     $8B ;MUS note (G#2, length 8)
 .byt     $93 ;MUS note (E-3, length 8)
 .byt     $8B ;MUS note (G#2, length 8)
 .byt     $93 ;MUS note (E-3, length 8)
 .byt     $8B ;MUS note (G#2, length 8)
 .byt     $93 ;MUS note (E-3, length 8)
 .byt     $8B ;MUS note (G#2, length 8)
 .byt     $93 ;MUS note (E-3, length 8)
 .byt     $8B ;MUS note (G#2, length 8)
 .byt     $93 ;MUS note (E-3, length 8)
 .byt     $8B ;MUS note (G#2, length 8)
 .byt     $93 ;MUS note (E-3, length 8)
 .byt     $8B ;MUS note (G#2, length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $8A ;MUS note (G-2, length 8)
 .byt     $8A ;MUS note (G-2, length 8)
 .byt     $93 ;MUS note (E-3, length 8)
 .byt     $8A ;MUS note (G-2, length 8)
 .byt     $93 ;MUS note (E-3, length 8)
 .byt     $8A ;MUS note (G-2, length 8)
 .byt     $93 ;MUS note (E-3, length 8)
 .byt     $8A ;MUS note (G-2, length 8)
 .byt     $93 ;MUS note (E-3, length 8)
 .byt     $89 ;MUS note (F#2, length 8)
 .byt     $93 ;MUS note (E-3, length 8)
 .byt     $89 ;MUS note (F#2, length 8)
 .byt     $93 ;MUS note (E-3, length 8)
 .byt     $89 ;MUS note (F#2, length 8)
 .byt     $93 ;MUS note (E-3, length 8)
 .byt     $89 ;MUS note (F#2, length 8)
 .byt     $93 ;MUS note (E-3, length 8)
 .byt MUSCMD_Loop, 1 
 .word :-
 .byt MUSCMD_SetVolumeAndEnv,   $81
 .byt MUSCMD_SetNoteBaseOffset, 2*12+9
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $82 ;MUS note (B-2, length 8)
 .byt     $42 ;MUS note (B-2, length 2)
 .byt     $44 ;MUS note (C#3, length 2)
 .byt     $46 ;MUS note (D#3, length 2)
 .byt     $47 ;MUS note (E-3, length 2)
 .byt     $49 ;MUS note (F#3, length 2)
 .byt     $4B ;MUS note (G#3, length 2)
 .byt     $4D ;MUS note (A#3, length 2)
 .byt     $4E ;MUS note (B-3, length 2)
 .byt     $50 ;MUS note (C#4, length 2)
 .byt     $52 ;MUS note (D#4, length 2)
 .byt     $53 ;MUS note (E-4, length 2)
 .byt     $54 ;MUS note (F-4, length 2)
 .byt     $95 ;MUS note (F#4, length 8)
 .byt     $BA ;MUS note (B-4, length 16)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $98 ;MUS note (A-4, length 8)
 .byt     $B8 ;MUS note (A-4, length 16)
 .byt     $80 ;Pause         (length 8)
 .byt     $97 ;MUS note (G#4, length 8)
 .byt     $98 ;MUS note (A-4, length 8)
 .byt     $97 ;MUS note (G#4, length 8)
 .byt     $95 ;MUS note (F#4, length 8)
 .byt     $93 ;MUS note (E-4, length 8)
 .byt     $B5 ;MUS note (F#4, length 16)
 .byt     $8E ;MUS note (B-3, length 8)
 .byt     $92 ;MUS note (D#4, length 8)
 .byt     $95 ;MUS note (F#4, length 8)
 .byt     $BA ;MUS note (B-4, length 16)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $98 ;MUS note (A-4, length 8)
 .byt     $F8 ;MUS note (A-4, length 64)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $85 ;MUS note (D-3, length 8)
 .byt     $45 ;MUS note (D-3, length 2)
 .byt     $47 ;MUS note (E-3, length 2)
 .byt     $49 ;MUS note (F#3, length 2)
 .byt     $4A ;MUS note (G-3, length 2)
 .byt     $4C ;MUS note (A-3, length 2)
 .byt     $4E ;MUS note (B-3, length 2)
 .byt     $50 ;MUS note (C#4, length 2)
 .byt     $51 ;MUS note (D-4, length 2)
 .byt MUSCMD_SetNoteBaseOffset, 3*12+9
 .byt     $8C ;MUS note (A-4, length 8)
 .byt     $91 ;MUS note (D-5, length 8)
 .byt     $B5 ;MUS note (F#5, length 16)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $93 ;MUS note (E-5, length 8)
 .byt $06,$B3 ;MUS note (E-5, length 24)
 .byt $06,$AF ;MUS note (C-5, length 24)
 .byt     $AC ;MUS note (A-4, length 16)
 .byt $06,$AD ;MUS note (A#4, length 24)
 .byt $06,$AF ;MUS note (C-5, length 24)
 .byt     $B1 ;MUS note (D-5, length 16)
 .byt     $75 ;MUS note (F#5, length 4)
 .byt     $75 ;MUS note (F#5, length 4)
 .byt     $75 ;MUS note (F#5, length 4)
 .byt     $75 ;MUS note (F#5, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $75 ;MUS note (F#5, length 4)
 .byt     $75 ;MUS note (F#5, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $75 ;MUS note (F#5, length 4)
 .byt     $75 ;MUS note (F#5, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $A0 ;Pause         (length 16)
 .byt MUSCMD_Loop, 0 
 .word :-
: ;at AA51
MUS_Elecman_chn3: ;at AA51
 .byt MUSCMD_SetSpeed,          $06
 .byt MUSCMD_SetVolumeAndEnv,   $3A
 .byt MUSCMD_SetVolumeCurve, $81,$10
 .byt     $A0 ;Pause         (length 16)
 .byt     $85 ;MUS note (D-4, length 8)
 .byt     $A0 ;Pause         (length 16)
 .byt     $80 ;Pause         (length 8)
 .byt     $85 ;MUS note (D-4, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $A0 ;Pause         (length 16)
 .byt     $85 ;MUS note (D-4, length 8)
 .byt     $A0 ;Pause         (length 16)
 .byt     $80 ;Pause         (length 8)
 .byt     $85 ;MUS note (D-4, length 8)
 .byt     $65 ;MUS note (D-4, length 4)
 .byt     $65 ;MUS note (D-4, length 4)
 .byt MUSCMD_Loop, 0 
 .word :-
MUS_Elecman_vibratotable: ;at AA69
 .byt $00, $E0, $80, $00 ;vibrato definition
 .byt $02, $22, $80, $00 ;vibrato definition
 .byt $00, $00, $82, $02 ;vibrato definition
.endscope

MUS_Gutsman: ;at AA75
.scope
 .byt $0F ;priority. Lo=music priority, Hi=sfx priority
 .word MUS_Gutsman_chn0               ; $AA80 ;
 .word MUS_Gutsman_chn1               ; $AB14 ;
 .word MUS_Gutsman_chn2               ; $ABAC ;
 .word MUS_Gutsman_chn3               ; $AC06 ;
 .word MUS_Gutsman_vibratotable       ; $AC61 ;
MUS_Gutsman_chn0: ;at AA80
 .byt MUSCMD_SetSpeed,          $06
 .byt MUSCMD_SetNoteBaseOffset, 2*12+4
 .byt MUSCMD_SetDutyCycle,      $80
 .byt MUSCMD_SetVolumeAndEnv,   $39
 .byt MUSCMD_SetVolumeCurve, $DF,$10
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $6E ;MUS note (F#3, length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $6E ;MUS note (F#3, length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $6E ;MUS note (F#3, length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $4A ;MUS note (D-3, length 2)
 .byt     $40 ;Pause         (length 2)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $6E ;MUS note (F#3, length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $6E ;MUS note (F#3, length 4)
 .byt     $71 ;MUS note (A-3, length 4)
 .byt     $74 ;MUS note (C-4, length 4)
 .byt     $76 ;MUS note (D-4, length 4)
 .byt     $74 ;MUS note (C-4, length 4)
 .byt     $71 ;MUS note (A-3, length 4)
 .byt     $6E ;MUS note (F#3, length 4)
 .byt     $8A ;MUS note (D-3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $60 ;Pause         (length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $6D ;MUS note (F-3, length 4)
 .byt     $6F ;MUS note (G-3, length 4)
: ;at AAAA
 .byt MUSCMD_SetDutyCycle,      $80
 .byt MUSCMD_SetVolumeCurve, $DF,$10
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt MUSCMD_SetVolumeAndEnv,   $39
 .byt MUSCMD_SetNoteBaseOffset, 2*12+4
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $D2 ;MUS note (A#3, length 32)
 .byt     $92 ;MUS note (A#3, length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $91 ;MUS note (A-3, length 8)
 .byt     $B1 ;MUS note (A-3, length 16)
 .byt     $8F ;MUS note (G-3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $C0 ;Pause         (length 32)
 .byt     $60 ;Pause         (length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $6D ;MUS note (F-3, length 4)
 .byt     $6F ;MUS note (G-3, length 4)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $D2 ;MUS note (A#3, length 32)
 .byt     $92 ;MUS note (A#3, length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $94 ;MUS note (C-4, length 8)
 .byt     $B4 ;MUS note (C-4, length 16)
 .byt     $72 ;MUS note (A#3, length 4)
 .byt     $6F ;MUS note (G-3, length 4)
 .byt     $80 ;Pause         (length 8)
 .byt     $C0 ;Pause         (length 32)
 .byt     $60 ;Pause         (length 4)
 .byt     $6F ;MUS note (G-3, length 4)
 .byt     $72 ;MUS note (A#3, length 4)
 .byt     $76 ;MUS note (D-4, length 4)
 .byt     $D4 ;MUS note (C-4, length 32)
 .byt     $D1 ;MUS note (A-3, length 32)
 .byt $06,$D6 ;MUS note (D-4, length 48)
 .byt     $60 ;Pause         (length 4)
 .byt     $6F ;MUS note (G-3, length 4)
 .byt     $72 ;MUS note (A#3, length 4)
 .byt     $76 ;MUS note (D-4, length 4)
 .byt     $D4 ;MUS note (C-4, length 32)
 .byt     $D7 ;MUS note (D#4, length 32)
 .byt     $D6 ;MUS note (D-4, length 32)
 .byt     $A0 ;Pause         (length 16)
 .byt     $79 ;MUS note (F-4, length 4)
 .byt     $7B ;MUS note (G-4, length 4)
 .byt     $80 ;Pause         (length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $D2 ;MUS note (A#3, length 32)
 .byt     $92 ;MUS note (A#3, length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $91 ;MUS note (A-3, length 8)
 .byt     $B1 ;MUS note (A-3, length 16)
 .byt     $8F ;MUS note (G-3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $C0 ;Pause         (length 32)
 .byt     $60 ;Pause         (length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $6D ;MUS note (F-3, length 4)
 .byt     $6F ;MUS note (G-3, length 4)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $D2 ;MUS note (A#3, length 32)
 .byt     $92 ;MUS note (A#3, length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $94 ;MUS note (C-4, length 8)
 .byt     $B4 ;MUS note (C-4, length 16)
 .byt     $72 ;MUS note (A#3, length 4)
 .byt     $6F ;MUS note (G-3, length 4)
 .byt     $80 ;Pause         (length 8)
 .byt     $C0 ;Pause         (length 32)
 .byt     $60 ;Pause         (length 4)
 .byt     $6F ;MUS note (G-3, length 4)
 .byt     $72 ;MUS note (A#3, length 4)
 .byt     $76 ;MUS note (D-4, length 4)
 .byt     $D6 ;MUS note (D-4, length 32)
 .byt     $B5 ;MUS note (C#4, length 16)
 .byt     $B4 ;MUS note (C-4, length 16)
 .byt     $72 ;MUS note (A#3, length 4)
 .byt     $6F ;MUS note (G-3, length 4)
 .byt     $80 ;Pause         (length 8)
 .byt     $C0 ;Pause         (length 32)
 .byt     $60 ;Pause         (length 4)
 .byt     $6F ;MUS note (G-3, length 4)
 .byt     $72 ;MUS note (A#3, length 4)
 .byt     $76 ;MUS note (D-4, length 4)
 .byt     $D6 ;MUS note (D-4, length 32)
 .byt     $B5 ;MUS note (C#4, length 16)
 .byt     $B4 ;MUS note (C-4, length 16)
 .byt     $72 ;MUS note (A#3, length 4)
 .byt     $7B ;MUS note (G-4, length 4)
 .byt     $80 ;Pause         (length 8)
 .byt     $C0 ;Pause         (length 32)
 .byt     $60 ;Pause         (length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $6D ;MUS note (F-3, length 4)
 .byt     $6F ;MUS note (G-3, length 4)
 .byt MUSCMD_Loop, 0 
 .word :-
MUS_Gutsman_chn1: ;at AB14
 .byt MUSCMD_SetSpeed,          $06
 .byt MUSCMD_SetVolumeAndEnv,   $38
 .byt MUSCMD_SetNoteBaseOffset, 2*12+4
 .byt MUSCMD_SetDutyCycle,      $80
 .byt MUSCMD_SetVolumeCurve, $DF,$10
 .byt     $68 ;MUS note (C-3, length 4)
 .byt     $68 ;MUS note (C-3, length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $68 ;MUS note (C-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $68 ;MUS note (C-3, length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $68 ;MUS note (C-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $68 ;MUS note (C-3, length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $68 ;MUS note (C-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $48 ;MUS note (C-3, length 2)
 .byt     $40 ;Pause         (length 2)
 .byt     $6E ;MUS note (F#3, length 4)
 .byt     $71 ;MUS note (A-3, length 4)
 .byt     $68 ;MUS note (C-3, length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $6D ;MUS note (F-3, length 4)
 .byt     $71 ;MUS note (A-3, length 4)
 .byt     $74 ;MUS note (C-4, length 4)
 .byt     $71 ;MUS note (A-3, length 4)
 .byt     $6E ;MUS note (F#3, length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $88 ;MUS note (C-3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt MUSCMD_SetVolumeAndEnv,   $35
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt     $60 ;Pause         (length 4)
 .byt     $68 ;MUS note (C-3, length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $6A ;MUS note (D-3, length 4)
: ;at AB42
 .byt MUSCMD_SetVolumeAndEnv,   $38
 .byt MUSCMD_SetDutyCycle,      $80
 .byt MUSCMD_SetVolumeCurve, $DF,$10
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt MUSCMD_SetNoteBaseOffset, 2*12+4
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $CF ;MUS note (G-3, length 32)
 .byt     $8F ;MUS note (G-3, length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $8D ;MUS note (F-3, length 8)
 .byt     $AD ;MUS note (F-3, length 16)
 .byt     $8A ;MUS note (D-3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $C0 ;Pause         (length 32)
 .byt     $60 ;Pause         (length 4)
 .byt     $68 ;MUS note (C-3, length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $CF ;MUS note (G-3, length 32)
 .byt     $8F ;MUS note (G-3, length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $91 ;MUS note (A-3, length 8)
 .byt     $B1 ;MUS note (A-3, length 16)
 .byt     $6F ;MUS note (G-3, length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $80 ;Pause         (length 8)
 .byt     $C0 ;Pause         (length 32)
 .byt     $60 ;Pause         (length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $6F ;MUS note (G-3, length 4)
 .byt     $72 ;MUS note (A#3, length 4)
 .byt     $D1 ;MUS note (A-3, length 32)
 .byt     $CD ;MUS note (F-3, length 32)
 .byt $06,$D2 ;MUS note (A#3, length 48)
 .byt     $60 ;Pause         (length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $6F ;MUS note (G-3, length 4)
 .byt     $72 ;MUS note (A#3, length 4)
 .byt     $D1 ;MUS note (A-3, length 32)
 .byt     $CF ;MUS note (G-3, length 32)
 .byt     $CE ;MUS note (F#3, length 32)
 .byt     $A0 ;Pause         (length 16)
 .byt     $74 ;MUS note (C-4, length 4)
 .byt     $76 ;MUS note (D-4, length 4)
 .byt     $80 ;Pause         (length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $CF ;MUS note (G-3, length 32)
 .byt     $8F ;MUS note (G-3, length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $8D ;MUS note (F-3, length 8)
 .byt     $AD ;MUS note (F-3, length 16)
 .byt     $8A ;MUS note (D-3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $C0 ;Pause         (length 32)
 .byt     $60 ;Pause         (length 4)
 .byt     $68 ;MUS note (C-3, length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $CF ;MUS note (G-3, length 32)
 .byt     $8F ;MUS note (G-3, length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $91 ;MUS note (A-3, length 8)
 .byt     $B1 ;MUS note (A-3, length 16)
 .byt     $6F ;MUS note (G-3, length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $80 ;Pause         (length 8)
 .byt     $C0 ;Pause         (length 32)
 .byt     $60 ;Pause         (length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $6F ;MUS note (G-3, length 4)
 .byt     $72 ;MUS note (A#3, length 4)
 .byt     $CD ;MUS note (F-3, length 32)
 .byt     $AC ;MUS note (E-3, length 16)
 .byt     $AB ;MUS note (D#3, length 16)
 .byt     $69 ;MUS note (C#3, length 4)
 .byt     $66 ;MUS note (A#2, length 4)
 .byt     $80 ;Pause         (length 8)
 .byt     $C0 ;Pause         (length 32)
 .byt     $60 ;Pause         (length 4)
 .byt     $68 ;MUS note (C-3, length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $CD ;MUS note (F-3, length 32)
 .byt     $AC ;MUS note (E-3, length 16)
 .byt     $AB ;MUS note (D#3, length 16)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $76 ;MUS note (D-4, length 4)
 .byt     $80 ;Pause         (length 8)
 .byt     $C0 ;Pause         (length 32)
 .byt     $60 ;Pause         (length 4)
 .byt     $68 ;MUS note (C-3, length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt MUSCMD_Loop, 0 
 .word :-
MUS_Gutsman_chn2: ;at ABAC
 .byt MUSCMD_SetSpeed,          $06
 .byt MUSCMD_SetNoteBaseOffset, 2*12+4
 .byt MUSCMD_SetVolumeAndEnv,   $7F
 .byt     $8A ;MUS note (D-3, length 8)
 .byt     $8A ;MUS note (D-3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $8A ;MUS note (D-3, length 8)
 .byt     $8A ;MUS note (D-3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $8A ;MUS note (D-3, length 8)
 .byt     $8A ;MUS note (D-3, length 8)
 .byt     $8A ;MUS note (D-3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $6A ;MUS note (D-3, length 4)
 .byt     $8A ;MUS note (D-3, length 8)
 .byt     $8A ;MUS note (D-3, length 8)
 .byt     $A0 ;Pause         (length 16)
: ;at ABC3
 .byt MUSCMD_SetVolumeAndEnv,   $7F
 .byt MUSCMD_SetNoteBaseOffset, 2*12+4
 .byt     $63 ;MUS note (G-2, length 4)
 .byt     $63 ;MUS note (G-2, length 4)
 .byt     $6F ;MUS note (G-3, length 4)
 .byt     $63 ;MUS note (G-2, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $63 ;MUS note (G-2, length 4)
 .byt     $6F ;MUS note (G-3, length 4)
 .byt     $63 ;MUS note (G-2, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $63 ;MUS note (G-2, length 4)
 .byt     $6F ;MUS note (G-3, length 4)
 .byt     $63 ;MUS note (G-2, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $43 ;MUS note (G-2, length 2)
 .byt     $40 ;Pause         (length 2)
 .byt     $6D ;MUS note (F-3, length 4)
 .byt     $6F ;MUS note (G-3, length 4)
 .byt MUSCMD_Loop, 11 
 .word :-
: ;at ABDC
 .byt     $66 ;MUS note (A#2, length 4)
 .byt     $66 ;MUS note (A#2, length 4)
 .byt     $72 ;MUS note (A#3, length 4)
 .byt     $66 ;MUS note (A#2, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $66 ;MUS note (A#2, length 4)
 .byt     $72 ;MUS note (A#3, length 4)
 .byt     $66 ;MUS note (A#2, length 4)
 .byt     $65 ;MUS note (A-2, length 4)
 .byt     $65 ;MUS note (A-2, length 4)
 .byt     $71 ;MUS note (A-3, length 4)
 .byt     $65 ;MUS note (A-2, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $44 ;MUS note (G#2, length 2)
 .byt     $40 ;Pause         (length 2)
 .byt     $70 ;MUS note (G#3, length 4)
 .byt     $64 ;MUS note (G#2, length 4)
 .byt     $63 ;MUS note (G-2, length 4)
 .byt     $63 ;MUS note (G-2, length 4)
 .byt     $6F ;MUS note (G-3, length 4)
 .byt     $63 ;MUS note (G-2, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $63 ;MUS note (G-2, length 4)
 .byt     $6F ;MUS note (G-3, length 4)
 .byt     $63 ;MUS note (G-2, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $63 ;MUS note (G-2, length 4)
 .byt     $6F ;MUS note (G-3, length 4)
 .byt     $63 ;MUS note (G-2, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $43 ;MUS note (G-2, length 2)
 .byt     $40 ;Pause         (length 2)
 .byt     $6D ;MUS note (F-3, length 4)
 .byt     $6F ;MUS note (G-3, length 4)
 .byt MUSCMD_Loop, 1 
 .word :-
 .byt MUSCMD_Loop, 0 
 .word :--
MUS_Gutsman_chn3: ;at AC06
 .byt MUSCMD_SetSpeed,          $06
: ;at AC08
 .byt MUSCMD_SetVolumeCurve, $83,$20
 .byt MUSCMD_SetVolumeAndEnv,   $33
 .byt MUSCMD_SetDutyCycle,      $80
 .byt     $66 ;MUS note (A#2, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $66 ;MUS note (A#2, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt MUSCMD_SetVolumeAndEnv,   $35
 .byt MUSCMD_SetVolumeCurve, $81,$10
 .byt MUSCMD_SetDutyCycle,      $00
 .byt     $85 ;MUS note (A-2, length 8)
 .byt MUSCMD_SetVolumeCurve, $83,$20
 .byt MUSCMD_SetVolumeAndEnv,   $33
 .byt MUSCMD_SetDutyCycle,      $80
 .byt     $66 ;MUS note (A#2, length 4)
 .byt     $66 ;MUS note (A#2, length 4)
 .byt     $66 ;MUS note (A#2, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $66 ;MUS note (A#2, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt MUSCMD_SetVolumeAndEnv,   $35
 .byt MUSCMD_SetDutyCycle,      $00
 .byt MUSCMD_SetVolumeCurve, $81,$10
 .byt     $85 ;MUS note (A-2, length 8)
 .byt MUSCMD_SetVolumeAndEnv,   $33
 .byt MUSCMD_SetVolumeCurve, $83,$20
 .byt MUSCMD_SetDutyCycle,      $80
 .byt     $66 ;MUS note (A#2, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $66 ;MUS note (A#2, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $66 ;MUS note (A#2, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt MUSCMD_SetDutyCycle,      $00
 .byt MUSCMD_SetVolumeAndEnv,   $35
 .byt MUSCMD_SetVolumeCurve, $81,$10
 .byt     $85 ;MUS note (A-2, length 8)
 .byt MUSCMD_SetVolumeCurve, $83,$20
 .byt MUSCMD_SetVolumeAndEnv,   $33
 .byt MUSCMD_SetDutyCycle,      $80
 .byt     $66 ;MUS note (A#2, length 4)
 .byt     $66 ;MUS note (A#2, length 4)
 .byt     $66 ;MUS note (A#2, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $66 ;MUS note (A#2, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt MUSCMD_SetDutyCycle,      $00
 .byt MUSCMD_SetVolumeCurve, $81,$10
 .byt MUSCMD_SetVolumeAndEnv,   $35
 .byt     $65 ;MUS note (A-2, length 4)
 .byt     $65 ;MUS note (A-2, length 4)
 .byt     $65 ;MUS note (A-2, length 4)
 .byt     $65 ;MUS note (A-2, length 4)
 .byt MUSCMD_Loop, 0 
 .word :-
MUS_Gutsman_vibratotable: ;at AC61
 .byt $00, $E0, $80, $00 ;vibrato definition
 .byt $01, $41, $80, $00 ;vibrato definition
.endscope

MUS_BossMusicWily: ;at AC69
.scope
 .byt $0F ;priority. Lo=music priority, Hi=sfx priority
 .word MUS_BossMusicWily_chn0         ; $AC74 ;
 .word MUS_BossMusicWily_chn1         ; $AC9E ;
 .word MUS_BossMusicWily_chn2         ; $ACCA ;
 .word MUS_BossMusicWily_chn3         ; $ACFF ;
 .word MUS_BossMusicWily_vibratotable ; $AD17 ;
MUS_BossMusicWily_chn0: ;at AC74
 .byt MUSCMD_SetSpeed,          $06
: ;at AC76
 .byt MUSCMD_SetDutyCycle,      $40
 .byt MUSCMD_SetVolumeAndEnv,   $38
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt MUSCMD_SetVolumeCurve, $86,$20
 .byt MUSCMD_SetNoteBaseOffset, 3*12+1
: ;at AC81
 .byt     $6F ;MUS note (E-4, length 4)
 .byt     $6D ;MUS note (D-4, length 4)
 .byt     $6F ;MUS note (E-4, length 4)
 .byt     $70 ;MUS note (F-4, length 4)
 .byt     $68 ;MUS note (A-3, length 4)
 .byt     $6D ;MUS note (D-4, length 4)
 .byt     $6F ;MUS note (E-4, length 4)
 .byt     $70 ;MUS note (F-4, length 4)
 .byt MUSCMD_Loop, 7 
 .word :-
: ;at AC8D
 .byt     $74 ;MUS note (A-4, length 4)
 .byt     $72 ;MUS note (G-4, length 4)
 .byt     $74 ;MUS note (A-4, length 4)
 .byt     $75 ;MUS note (A#4, length 4)
 .byt     $6D ;MUS note (D-4, length 4)
 .byt     $72 ;MUS note (G-4, length 4)
 .byt     $74 ;MUS note (A-4, length 4)
 .byt     $75 ;MUS note (A#4, length 4)
 .byt MUSCMD_Loop, 7 
 .word :-
 .byt MUSCMD_Loop, 0 
 .word :---
 .byt MUSCMD_EndOfData
MUS_BossMusicWily_chn1: ;at AC9E
 .byt MUSCMD_SetSpeed,          $06
: ;at ACA0
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt MUSCMD_SetDutyCycle,      $40
 .byt MUSCMD_SetVolumeAndEnv,   $36
 .byt MUSCMD_SetVolumeCurve, $86,$20
 .byt MUSCMD_SetDutyCycle,      $80
 .byt MUSCMD_SetNoteBaseOffset, 3*12+1
: ;at ACAD
 .byt     $68 ;MUS note (A-3, length 4)
 .byt     $68 ;MUS note (A-3, length 4)
 .byt     $68 ;MUS note (A-3, length 4)
 .byt     $6D ;MUS note (D-4, length 4)
 .byt     $64 ;MUS note (F-3, length 4)
 .byt     $64 ;MUS note (F-3, length 4)
 .byt     $64 ;MUS note (F-3, length 4)
 .byt     $68 ;MUS note (A-3, length 4)
 .byt MUSCMD_Loop, 7 
 .word :-
: ;at ACB9
 .byt     $6D ;MUS note (D-4, length 4)
 .byt     $6D ;MUS note (D-4, length 4)
 .byt     $6D ;MUS note (D-4, length 4)
 .byt     $72 ;MUS note (G-4, length 4)
 .byt     $69 ;MUS note (A#3, length 4)
 .byt     $69 ;MUS note (A#3, length 4)
 .byt     $69 ;MUS note (A#3, length 4)
 .byt     $6D ;MUS note (D-4, length 4)
 .byt MUSCMD_Loop, 7 
 .word :-
 .byt MUSCMD_Loop, 0 
 .word :---
 .byt MUSCMD_EndOfData
MUS_BossMusicWily_chn2: ;at ACCA
 .byt MUSCMD_SetSpeed,          $06
: ;at ACCC
 .byt MUSCMD_SetVolumeAndEnv,   $81
 .byt MUSCMD_SetNoteBaseOffset, 2*12+1
: ;at ACD0
 .byt     $CD ;MUS note (D-3, length 32)
 .byt     $CC ;MUS note (C#3, length 32)
 .byt     $CB ;MUS note (C-3, length 32)
 .byt     $CA ;MUS note (B-2, length 32)
 .byt MUSCMD_Loop, 1 
 .word :-
 .byt     $D2 ;MUS note (G-3, length 32)
 .byt     $D1 ;MUS note (F#3, length 32)
 .byt     $D0 ;MUS note (F-3, length 32)
 .byt     $CF ;MUS note (E-3, length 32)
 .byt     $D2 ;MUS note (G-3, length 32)
 .byt     $D1 ;MUS note (F#3, length 32)
 .byt MUSCMD_SetVibratoCtrl,    $0F
 .byt MUSCMD_SetVolumeAndEnv,   $09
 .byt MUSCMD_SetNoteBaseOffset, 2*12+7
 .byt     $6D ;MUS note (G#3, length 4)
 .byt     $72 ;MUS note (C#4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $6D ;MUS note (G#3, length 4)
 .byt     $72 ;MUS note (C#4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $6D ;MUS note (G#3, length 4)
 .byt     $72 ;MUS note (C#4, length 4)
 .byt     $6D ;MUS note (G#3, length 4)
 .byt     $6D ;MUS note (G#3, length 4)
 .byt     $6D ;MUS note (G#3, length 4)
 .byt     $6D ;MUS note (G#3, length 4)
 .byt     $68 ;MUS note (D#3, length 4)
 .byt     $68 ;MUS note (D#3, length 4)
 .byt     $68 ;MUS note (D#3, length 4)
 .byt     $68 ;MUS note (D#3, length 4)
 .byt MUSCMD_SetVibratoCtrl,    $00
 .byt MUSCMD_SetVolumeAndEnv,   $81
 .byt MUSCMD_SetNoteBaseOffset, 2*12+1
 .byt MUSCMD_Loop, 0 
 .word :--
 .byt MUSCMD_EndOfData
MUS_BossMusicWily_chn3: ;at ACFF
 .byt MUSCMD_SetSpeed,          $06
 .byt MUSCMD_SetVolumeAndEnv,   $38
 .byt MUSCMD_SetVolumeCurve, $82,$20
: ;at AD06
 .byt     $A0 ;Pause         (length 16)
 .byt     $85 ;MUS note (F#2, length 8)
 .byt     $A0 ;Pause         (length 16)
 .byt     $A5 ;MUS note (F#2, length 16)
 .byt     $85 ;MUS note (F#2, length 8)
 .byt     $A0 ;Pause         (length 16)
 .byt     $85 ;MUS note (F#2, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $A0 ;Pause         (length 16)
 .byt     $85 ;MUS note (F#2, length 8)
 .byt     $65 ;MUS note (F#2, length 4)
 .byt     $65 ;MUS note (F#2, length 4)
 .byt MUSCMD_Loop, 0 
 .word :-
 .byt MUSCMD_EndOfData
MUS_BossMusicWily_vibratotable: ;at AD17
 .byt $00, $22, $80, $00 ;vibrato definition
 .byt $01, $22, $80, $00 ;vibrato definition
.endscope

MUS_WilyStage1and2: ;at AD1F
.scope
 .byt $0F ;priority. Lo=music priority, Hi=sfx priority
 .word MUS_WilyStage1and2_chn0        ; $AD2A ;
 .word MUS_WilyStage1and2_chn1        ; $ADD1 ;
 .word MUS_WilyStage1and2_chn2        ; $AE74 ;
 .word MUS_WilyStage1and2_chn3        ; $AEEC ;
 .word MUS_WilyStage1and2_vibratotable ; $AF05 ;
MUS_WilyStage1and2_chn0: ;at AD2A
 .byt MUSCMD_SetSpeed,          $06
: ;at AD2C
 .byt MUSCMD_SetVibratoIndex,   $00
 .byt MUSCMD_SetVolumeAndEnv,   $38
 .byt MUSCMD_SetDutyCycle,      $00
 .byt MUSCMD_SetVolumeCurve, $8F,$E0
 .byt MUSCMD_SetNoteBaseOffset, 2*12+5
: ;at AD37
 .byt     $80 ;Pause         (length 8)
 .byt     $90 ;MUS note (A-3, length 8)
 .byt     $70 ;MUS note (A-3, length 4)
 .byt $06,$92 ;MUS note (B-3, length 12)
 .byt $30,$B4 ;MUS note (C#4, length 16/1.5) (triplet)
 .byt $30,$B2 ;MUS note (B-3, length 16/1.5) (triplet)
 .byt $30,$B0 ;MUS note (A-3, length 16/1.5) (triplet)
 .byt MUSCMD_Loop, 1 
 .word :-
: ;at AD46
 .byt     $80 ;Pause         (length 8)
 .byt     $8F ;MUS note (G#3, length 8)
 .byt     $6F ;MUS note (G#3, length 4)
 .byt $06,$90 ;MUS note (A-3, length 12)
 .byt $30,$B2 ;MUS note (B-3, length 16/1.5) (triplet)
 .byt $30,$B0 ;MUS note (A-3, length 16/1.5) (triplet)
 .byt $30,$AF ;MUS note (G#3, length 16/1.5) (triplet)
 .byt MUSCMD_Loop, 1 
 .word :-
: ;at AD55
 .byt     $80 ;Pause         (length 8)
 .byt     $8D ;MUS note (F#3, length 8)
 .byt     $6D ;MUS note (F#3, length 4)
 .byt $06,$8F ;MUS note (G#3, length 12)
 .byt $30,$B0 ;MUS note (A-3, length 16/1.5) (triplet)
 .byt $30,$AF ;MUS note (G#3, length 16/1.5) (triplet)
 .byt $30,$AD ;MUS note (F#3, length 16/1.5) (triplet)
 .byt MUSCMD_Loop, 1 
 .word :-
: ;at AD64
 .byt     $80 ;Pause         (length 8)
 .byt     $8C ;MUS note (F-3, length 8)
 .byt     $6C ;MUS note (F-3, length 4)
 .byt $06,$8D ;MUS note (F#3, length 12)
 .byt $30,$AF ;MUS note (G#3, length 16/1.5) (triplet)
 .byt $30,$AD ;MUS note (F#3, length 16/1.5) (triplet)
 .byt $30,$AC ;MUS note (F-3, length 16/1.5) (triplet)
 .byt MUSCMD_Loop, 1 
 .word :-
 .byt MUSCMD_SetVolumeAndEnv,   $38
 .byt MUSCMD_SetVolumeCurve, $D0,$10
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $B0 ;MUS note (A-3, length 16)
 .byt MUSCMD_SetVibratoIndex,   $02
 .byt     $F0 ;MUS note (A-3, length 64)
 .byt MUSCMD_SetVibratoIndex,   $00
 .byt     $A0 ;Pause         (length 16)
 .byt     $80 ;Pause         (length 8)
 .byt     $90 ;MUS note (A-3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $8F ;MUS note (G#3, length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $AF ;MUS note (G#3, length 16)
 .byt MUSCMD_SetVibratoIndex,   $02
 .byt     $EF ;MUS note (G#3, length 64)
 .byt MUSCMD_SetVibratoIndex,   $00
 .byt     $A0 ;Pause         (length 16)
 .byt     $80 ;Pause         (length 8)
 .byt     $8F ;MUS note (G#3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $8D ;MUS note (F#3, length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $AD ;MUS note (F#3, length 16)
 .byt MUSCMD_SetVibratoIndex,   $02
 .byt     $ED ;MUS note (F#3, length 64)
 .byt MUSCMD_SetVibratoIndex,   $00
 .byt     $A0 ;Pause         (length 16)
 .byt     $80 ;Pause         (length 8)
 .byt     $8D ;MUS note (F#3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $8C ;MUS note (F-3, length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $AC ;MUS note (F-3, length 16)
 .byt MUSCMD_SetVibratoIndex,   $02
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $AC ;MUS note (F-3, length 16)
 .byt     $EC ;MUS note (F-3, length 64)
 .byt     $C0 ;Pause         (length 32)
 .byt MUSCMD_SetVibratoIndex,   $00
: ;at ADA9
 .byt     $67 ;MUS note (C-3, length 4)
 .byt     $70 ;MUS note (A-3, length 4)
 .byt MUSCMD_Loop, 7 
 .word :-
 .byt     $68 ;MUS note (C#3, length 4)
 .byt     $69 ;MUS note (D-3, length 4)
 .byt     $6A ;MUS note (D#3, length 4)
 .byt     $6B ;MUS note (E-3, length 4)
 .byt     $6C ;MUS note (F-3, length 4)
 .byt     $6D ;MUS note (F#3, length 4)
 .byt     $6E ;MUS note (G-3, length 4)
 .byt     $6F ;MUS note (G#3, length 4)
 .byt     $70 ;MUS note (A-3, length 4)
 .byt     $6F ;MUS note (G#3, length 4)
 .byt     $6E ;MUS note (G-3, length 4)
 .byt     $6D ;MUS note (F#3, length 4)
 .byt     $6C ;MUS note (F-3, length 4)
 .byt     $6B ;MUS note (E-3, length 4)
 .byt     $6A ;MUS note (D#3, length 4)
 .byt     $68 ;MUS note (C#3, length 4)
: ;at ADBF
 .byt     $69 ;MUS note (D-3, length 4)
 .byt     $72 ;MUS note (B-3, length 4)
 .byt MUSCMD_Loop, 3 
 .word :-
: ;at ADC5
 .byt     $69 ;MUS note (D-3, length 4)
 .byt     $73 ;MUS note (C-4, length 4)
 .byt MUSCMD_Loop, 3 
 .word :-
 .byt     $F4 ;MUS note (C#4, length 64)
 .byt MUSCMD_Loop, 0 
 .word :--------
 .byt MUSCMD_EndOfData
MUS_WilyStage1and2_chn1: ;at ADD1
 .byt MUSCMD_SetSpeed,          $06
: ;at ADD3
 .byt MUSCMD_SetNoteBaseOffset, 2*12+5
 .byt MUSCMD_SetDutyCycle,      $00
 .byt MUSCMD_SetVolumeAndEnv,   $36
 .byt MUSCMD_SetVolumeCurve, $8F,$E0
: ;at ADDC
 .byt     $80 ;Pause         (length 8)
 .byt     $8D ;MUS note (F#3, length 8)
 .byt     $6D ;MUS note (F#3, length 4)
 .byt $06,$8F ;MUS note (G#3, length 12)
 .byt $30,$B0 ;MUS note (A-3, length 16/1.5) (triplet)
 .byt $30,$AF ;MUS note (G#3, length 16/1.5) (triplet)
 .byt $30,$AD ;MUS note (F#3, length 16/1.5) (triplet)
 .byt MUSCMD_Loop, 1 
 .word :-
: ;at ADEB
 .byt     $80 ;Pause         (length 8)
 .byt     $8B ;MUS note (E-3, length 8)
 .byt     $6B ;MUS note (E-3, length 4)
 .byt $06,$8D ;MUS note (F#3, length 12)
 .byt $30,$AF ;MUS note (G#3, length 16/1.5) (triplet)
 .byt $30,$AD ;MUS note (F#3, length 16/1.5) (triplet)
 .byt $30,$AB ;MUS note (E-3, length 16/1.5) (triplet)
 .byt MUSCMD_Loop, 1 
 .word :-
: ;at ADFA
 .byt     $80 ;Pause         (length 8)
 .byt     $89 ;MUS note (D-3, length 8)
 .byt     $69 ;MUS note (D-3, length 4)
 .byt $06,$8B ;MUS note (E-3, length 12)
 .byt $30,$AD ;MUS note (F#3, length 16/1.5) (triplet)
 .byt $30,$AB ;MUS note (E-3, length 16/1.5) (triplet)
 .byt $30,$A9 ;MUS note (D-3, length 16/1.5) (triplet)
 .byt MUSCMD_Loop, 1 
 .word :-
: ;at AE09
 .byt     $80 ;Pause         (length 8)
 .byt     $88 ;MUS note (C#3, length 8)
 .byt     $68 ;MUS note (C#3, length 4)
 .byt $06,$8A ;MUS note (D#3, length 12)
 .byt $30,$AC ;MUS note (F-3, length 16/1.5) (triplet)
 .byt $30,$A9 ;MUS note (D-3, length 16/1.5) (triplet)
 .byt $30,$A8 ;MUS note (C#3, length 16/1.5) (triplet)
 .byt MUSCMD_Loop, 1 
 .word :-
: ;at AE18
 .byt     $80 ;Pause         (length 8)
 .byt     $90 ;MUS note (A-3, length 8)
 .byt     $70 ;MUS note (A-3, length 4)
 .byt $06,$92 ;MUS note (B-3, length 12)
 .byt $30,$B4 ;MUS note (C#4, length 16/1.5) (triplet)
 .byt $30,$B2 ;MUS note (B-3, length 16/1.5) (triplet)
 .byt $30,$B0 ;MUS note (A-3, length 16/1.5) (triplet)
 .byt MUSCMD_Loop, 1 
 .word :-
: ;at AE27
 .byt     $80 ;Pause         (length 8)
 .byt     $8F ;MUS note (G#3, length 8)
 .byt     $6F ;MUS note (G#3, length 4)
 .byt $06,$90 ;MUS note (A-3, length 12)
 .byt $30,$B2 ;MUS note (B-3, length 16/1.5) (triplet)
 .byt $30,$B0 ;MUS note (A-3, length 16/1.5) (triplet)
 .byt $30,$AF ;MUS note (G#3, length 16/1.5) (triplet)
 .byt MUSCMD_Loop, 1 
 .word :-
: ;at AE36
 .byt     $80 ;Pause         (length 8)
 .byt     $8D ;MUS note (F#3, length 8)
 .byt     $6D ;MUS note (F#3, length 4)
 .byt $06,$8F ;MUS note (G#3, length 12)
 .byt $30,$B0 ;MUS note (A-3, length 16/1.5) (triplet)
 .byt $30,$AF ;MUS note (G#3, length 16/1.5) (triplet)
 .byt $30,$AD ;MUS note (F#3, length 16/1.5) (triplet)
 .byt MUSCMD_Loop, 1 
 .word :-
: ;at AE45
 .byt     $80 ;Pause         (length 8)
 .byt     $8C ;MUS note (F-3, length 8)
 .byt     $6C ;MUS note (F-3, length 4)
 .byt $06,$8D ;MUS note (F#3, length 12)
 .byt $30,$AF ;MUS note (G#3, length 16/1.5) (triplet)
 .byt $30,$AD ;MUS note (F#3, length 16/1.5) (triplet)
 .byt $30,$AC ;MUS note (F-3, length 16/1.5) (triplet)
 .byt MUSCMD_Loop, 1 
 .word :-
 .byt MUSCMD_SetVolumeAndEnv,   $37
: ;at AE56
 .byt     $8D ;MUS note (F#3, length 8)
 .byt MUSCMD_Loop, 7 
 .word :-
 .byt     $EF ;MUS note (G#3, length 64)
: ;at AE5C
 .byt     $92 ;MUS note (B-3, length 8)
 .byt MUSCMD_Loop, 3 
 .word :-
: ;at AE61
 .byt     $93 ;MUS note (C-4, length 8)
 .byt MUSCMD_Loop, 3 
 .word :-
 .byt     $D4 ;MUS note (C#4, length 32)
 .byt     $63 ;MUS note (G#2, length 4)
 .byt     $69 ;MUS note (D-3, length 4)
 .byt     $6D ;MUS note (F#3, length 4)
 .byt     $72 ;MUS note (B-3, length 4)
 .byt     $68 ;MUS note (C#3, length 4)
 .byt     $6F ;MUS note (G#3, length 4)
 .byt     $75 ;MUS note (D-4, length 4)
 .byt     $74 ;MUS note (C#4, length 4)
 .byt MUSCMD_Loop, 0 
 .word :------------
 .byt MUSCMD_EndOfData
MUS_WilyStage1and2_chn2: ;at AE74
 .byt MUSCMD_SetSpeed,          $06
: ;at AE76
 .byt MUSCMD_SetNoteBaseOffset, 2*12+5
 .byt MUSCMD_SetVolumeAndEnv,   $71
: ;at AE7A
 .byt     $8D ;MUS note (F#3, length 8)
 .byt     $8D ;MUS note (F#3, length 8)
 .byt     $8D ;MUS note (F#3, length 8)
 .byt     $8D ;MUS note (F#3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $8D ;MUS note (F#3, length 8)
 .byt     $8D ;MUS note (F#3, length 8)
 .byt     $8D ;MUS note (F#3, length 8)
 .byt MUSCMD_Loop, 1 
 .word :-
: ;at AE86
 .byt     $8B ;MUS note (E-3, length 8)
 .byt     $8B ;MUS note (E-3, length 8)
 .byt     $8B ;MUS note (E-3, length 8)
 .byt     $8B ;MUS note (E-3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $8B ;MUS note (E-3, length 8)
 .byt     $8B ;MUS note (E-3, length 8)
 .byt     $8B ;MUS note (E-3, length 8)
 .byt MUSCMD_Loop, 1 
 .word :-
: ;at AE92
 .byt     $89 ;MUS note (D-3, length 8)
 .byt     $89 ;MUS note (D-3, length 8)
 .byt     $89 ;MUS note (D-3, length 8)
 .byt     $89 ;MUS note (D-3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $89 ;MUS note (D-3, length 8)
 .byt     $89 ;MUS note (D-3, length 8)
 .byt     $89 ;MUS note (D-3, length 8)
 .byt MUSCMD_Loop, 1 
 .word :-
: ;at AE9E
 .byt     $88 ;MUS note (C#3, length 8)
 .byt     $88 ;MUS note (C#3, length 8)
 .byt     $88 ;MUS note (C#3, length 8)
 .byt     $88 ;MUS note (C#3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $88 ;MUS note (C#3, length 8)
 .byt     $88 ;MUS note (C#3, length 8)
 .byt     $88 ;MUS note (C#3, length 8)
 .byt MUSCMD_Loop, 1 
 .word :-
: ;at AEAA
 .byt     $8D ;MUS note (F#3, length 8)
 .byt MUSCMD_Loop, 15 
 .word :-
: ;at AEAF
 .byt     $8B ;MUS note (E-3, length 8)
 .byt MUSCMD_Loop, 15 
 .word :-
: ;at AEB4
 .byt     $89 ;MUS note (D-3, length 8)
 .byt MUSCMD_Loop, 15 
 .word :-
: ;at AEB9
 .byt     $88 ;MUS note (C#3, length 8)
 .byt MUSCMD_Loop, 15 
 .word :-
: ;at AEBE
 .byt     $87 ;MUS note (C-3, length 8)
 .byt MUSCMD_Loop, 7 
 .word :-
: ;at AEC3
 .byt     $88 ;MUS note (C#3, length 8)
 .byt MUSCMD_Loop, 7 
 .word :-
: ;at AEC8
 .byt     $83 ;MUS note (G#2, length 8)
 .byt MUSCMD_Loop, 7 
 .word :-
 .byt MUSCMD_SetNoteBaseOffset, 2*12+4
 .byt MUSCMD_SetVibratoCtrl,    $0E
 .byt MUSCMD_SetVolumeAndEnv,   $09
 .byt     $74 ;MUS note (C-4, length 4)
 .byt     $80 ;Pause         (length 8)
 .byt     $74 ;MUS note (C-4, length 4)
 .byt     $94 ;MUS note (C-4, length 8)
 .byt     $74 ;MUS note (C-4, length 4)
 .byt     $74 ;MUS note (C-4, length 4)
 .byt     $72 ;MUS note (A#3, length 4)
 .byt     $72 ;MUS note (A#3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $6D ;MUS note (F-3, length 4)
 .byt     $6D ;MUS note (F-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $68 ;MUS note (C-3, length 4)
 .byt     $68 ;MUS note (C-3, length 4)
 .byt MUSCMD_SetNoteBaseOffset, 2*12+5
 .byt MUSCMD_SetVibratoCtrl,    $00
 .byt MUSCMD_SetVolumeAndEnv,   $71
 .byt MUSCMD_Loop, 0 
 .word :------------
 .byt MUSCMD_EndOfData
MUS_WilyStage1and2_chn3: ;at AEEC
 .byt MUSCMD_SetSpeed,          $06
: ;at AEEE
 .byt MUSCMD_SetVolumeCurve, $82,$20
 .byt MUSCMD_SetVolumeAndEnv,   $39
 .byt     $A0 ;Pause         (length 16)
 .byt     $85 ;MUS note (A#2, length 8)
 .byt $06,$A0 ;Pause         (length 24)
 .byt     $85 ;MUS note (A#2, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $A0 ;Pause         (length 16)
 .byt     $85 ;MUS note (A#2, length 8)
 .byt $06,$A0 ;Pause         (length 24)
 .byt     $60 ;Pause         (length 4)
 .byt     $85 ;MUS note (A#2, length 8)
 .byt     $65 ;MUS note (A#2, length 4)
 .byt MUSCMD_Loop, 0 
 .word :-
 .byt MUSCMD_EndOfData
MUS_WilyStage1and2_vibratotable: ;at AF05
 .byt $00, $00, $80, $00 ;vibrato definition
 .byt $01, $61, $80, $00 ;vibrato definition
 .byt $01, $62, $80, $00 ;vibrato definition
.endscope

MUS_BossMusic: ;at AF11
.scope
 .byt $0F ;priority. Lo=music priority, Hi=sfx priority
 .word MUS_BossMusic_chn0             ; $AF1C ;
 .word MUS_BossMusic_chn1             ; $AF96 ;
 .word MUS_BossMusic_chn2             ; $AFCB ;
 .word MUS_BossMusic_chn3             ; $B01D ;
 .word MUS_BossMusic_vibratotable     ; $B040 ;
MUS_BossMusic_chn0: ;at AF1C
 .byt MUSCMD_SetSpeed,          $06
 .byt MUSCMD_SetDutyCycle,      $00
 .byt MUSCMD_SetVolumeAndEnv,   $39
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt MUSCMD_SetNoteBaseOffset, 1*12+7
 .byt     $60 ;Pause         (length 4)
 .byt     $68 ;MUS note (D#2, length 4)
 .byt     $69 ;MUS note (E-2, length 4)
 .byt     $68 ;MUS note (D#2, length 4)
 .byt     $6B ;MUS note (F#2, length 4)
 .byt     $68 ;MUS note (D#2, length 4)
 .byt     $6C ;MUS note (G-2, length 4)
 .byt     $68 ;MUS note (D#2, length 4)
 .byt     $6D ;MUS note (G#2, length 4)
 .byt     $68 ;MUS note (D#2, length 4)
 .byt     $6E ;MUS note (A-2, length 4)
 .byt     $68 ;MUS note (D#2, length 4)
 .byt     $6F ;MUS note (A#2, length 4)
 .byt     $68 ;MUS note (D#2, length 4)
 .byt     $74 ;MUS note (D#3, length 4)
 .byt     $68 ;MUS note (D#2, length 4)
 .byt MUSCMD_SetNoteBaseOffset, 2*12+7
 .byt     $60 ;Pause         (length 4)
 .byt     $68 ;MUS note (D#3, length 4)
 .byt     $69 ;MUS note (E-3, length 4)
 .byt     $68 ;MUS note (D#3, length 4)
 .byt     $6B ;MUS note (F#3, length 4)
 .byt     $68 ;MUS note (D#3, length 4)
 .byt     $6C ;MUS note (G-3, length 4)
 .byt     $68 ;MUS note (D#3, length 4)
 .byt     $6D ;MUS note (G#3, length 4)
 .byt     $68 ;MUS note (D#3, length 4)
 .byt     $6E ;MUS note (A-3, length 4)
 .byt     $68 ;MUS note (D#3, length 4)
 .byt     $6F ;MUS note (A#3, length 4)
 .byt     $94 ;MUS note (D#4, length 8)
 .byt     $74 ;MUS note (D#4, length 4)
: ;at AF47
 .byt MUSCMD_SetDutyCycle,      $00
 .byt MUSCMD_SetVolumeAndEnv,   $39
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt MUSCMD_SetNoteBaseOffset, 2*12+7
 .byt MUSCMD_SetVolumeCurve, $A0,$20
 .byt     $68 ;MUS note (D#3, length 4)
 .byt     $6D ;MUS note (G#3, length 4)
 .byt     $6F ;MUS note (A#3, length 4)
 .byt     $70 ;MUS note (B-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $6F ;MUS note (A#3, length 4)
 .byt     $6D ;MUS note (G#3, length 4)
 .byt     $70 ;MUS note (B-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $6F ;MUS note (A#3, length 4)
 .byt     $6D ;MUS note (G#3, length 4)
 .byt     $70 ;MUS note (B-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $6F ;MUS note (A#3, length 4)
 .byt     $6D ;MUS note (G#3, length 4)
 .byt     $68 ;MUS note (D#3, length 4)
 .byt     $66 ;MUS note (C#3, length 4)
 .byt     $6B ;MUS note (F#3, length 4)
 .byt     $6D ;MUS note (G#3, length 4)
 .byt     $6F ;MUS note (A#3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $6D ;MUS note (G#3, length 4)
 .byt     $6B ;MUS note (F#3, length 4)
 .byt     $6F ;MUS note (A#3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $6D ;MUS note (G#3, length 4)
 .byt     $6B ;MUS note (F#3, length 4)
 .byt     $6F ;MUS note (A#3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $6D ;MUS note (G#3, length 4)
 .byt     $6B ;MUS note (F#3, length 4)
 .byt     $66 ;MUS note (C#3, length 4)
 .byt     $64 ;MUS note (B-2, length 4)
 .byt     $69 ;MUS note (E-3, length 4)
 .byt     $6B ;MUS note (F#3, length 4)
 .byt     $6D ;MUS note (G#3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $6B ;MUS note (F#3, length 4)
 .byt     $69 ;MUS note (E-3, length 4)
 .byt     $6D ;MUS note (G#3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $6B ;MUS note (F#3, length 4)
 .byt     $69 ;MUS note (E-3, length 4)
 .byt     $6D ;MUS note (G#3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $6B ;MUS note (F#3, length 4)
 .byt     $69 ;MUS note (E-3, length 4)
 .byt     $67 ;MUS note (D-3, length 4)
 .byt     $68 ;MUS note (D#3, length 4)
 .byt     $69 ;MUS note (E-3, length 4)
 .byt     $68 ;MUS note (D#3, length 4)
 .byt     $6A ;MUS note (F-3, length 4)
 .byt     $68 ;MUS note (D#3, length 4)
 .byt     $6B ;MUS note (F#3, length 4)
 .byt     $68 ;MUS note (D#3, length 4)
 .byt     $6C ;MUS note (G-3, length 4)
 .byt     $68 ;MUS note (D#3, length 4)
 .byt     $6D ;MUS note (G#3, length 4)
 .byt     $68 ;MUS note (D#3, length 4)
 .byt     $6E ;MUS note (A-3, length 4)
 .byt     $68 ;MUS note (D#3, length 4)
 .byt     $6F ;MUS note (A#3, length 4)
 .byt     $68 ;MUS note (D#3, length 4)
 .byt     $74 ;MUS note (D#4, length 4)
 .byt MUSCMD_Loop, 0 
 .word :-
MUS_BossMusic_chn1: ;at AF96
 .byt MUSCMD_SetSpeed,          $06
 .byt MUSCMD_SetDutyCycle,      $00
 .byt MUSCMD_SetVolumeAndEnv,   $35
 .byt MUSCMD_SetVolumeCurve, $A0,$20
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt MUSCMD_SetNoteBaseOffset, 1*12+7
 .byt     $40 ;Pause         (length 2)
 .byt     $60 ;Pause         (length 4)
 .byt     $68 ;MUS note (D#2, length 4)
 .byt     $69 ;MUS note (E-2, length 4)
 .byt     $68 ;MUS note (D#2, length 4)
 .byt     $6B ;MUS note (F#2, length 4)
 .byt     $68 ;MUS note (D#2, length 4)
 .byt     $6C ;MUS note (G-2, length 4)
 .byt     $68 ;MUS note (D#2, length 4)
 .byt     $6D ;MUS note (G#2, length 4)
 .byt     $68 ;MUS note (D#2, length 4)
 .byt     $6E ;MUS note (A-2, length 4)
 .byt     $68 ;MUS note (D#2, length 4)
 .byt     $6F ;MUS note (A#2, length 4)
 .byt     $68 ;MUS note (D#2, length 4)
 .byt     $74 ;MUS note (D#3, length 4)
 .byt     $68 ;MUS note (D#2, length 4)
 .byt MUSCMD_SetNoteBaseOffset, 2*12+7
 .byt     $60 ;Pause         (length 4)
 .byt     $68 ;MUS note (D#3, length 4)
 .byt     $69 ;MUS note (E-3, length 4)
 .byt     $68 ;MUS note (D#3, length 4)
 .byt     $6B ;MUS note (F#3, length 4)
 .byt     $68 ;MUS note (D#3, length 4)
 .byt     $6C ;MUS note (G-3, length 4)
 .byt     $68 ;MUS note (D#3, length 4)
 .byt     $6D ;MUS note (G#3, length 4)
 .byt     $68 ;MUS note (D#3, length 4)
 .byt     $6E ;MUS note (A-3, length 4)
 .byt     $48 ;MUS note (D#3, length 2)
 .byt     $6C ;MUS note (G-3, length 4)
 .byt     $8C ;MUS note (G-3, length 8)
 .byt     $6C ;MUS note (G-3, length 4)
 .byt     $40 ;Pause         (length 2)
 .byt     $60 ;Pause         (length 4)
 .byt MUSCMD_Loop, 0 
 .word :-
MUS_BossMusic_chn2: ;at AFCB
 .byt MUSCMD_SetSpeed,          $06
 .byt MUSCMD_SetVibratoCtrl,    $0F
 .byt MUSCMD_SetVolumeAndEnv,   $08
 .byt MUSCMD_SetNoteBaseOffset, 2*12+4
 .byt     $E0 ;Pause         (length 64)
 .byt     $C0 ;Pause         (length 32)
 .byt     $6D ;MUS note (F-3, length 4)
 .byt     $6D ;MUS note (F-3, length 4)
 .byt     $6D ;MUS note (F-3, length 4)
 .byt     $6D ;MUS note (F-3, length 4)
 .byt     $68 ;MUS note (C-3, length 4)
 .byt     $68 ;MUS note (C-3, length 4)
 .byt     $68 ;MUS note (C-3, length 4)
 .byt     $68 ;MUS note (C-3, length 4)
: ;at AFDD
 .byt MUSCMD_SetVibratoCtrl,    $00
 .byt MUSCMD_SetVibratoIndex,   $00
 .byt MUSCMD_SetVolumeAndEnv,   $30
 .byt MUSCMD_SetNoteBaseOffset, 2*12+7
 .byt $06,$8D ;MUS note (G#3, length 12)
 .byt $06,$88 ;MUS note (D#3, length 12)
 .byt     $8D ;MUS note (G#3, length 8)
 .byt $06,$8D ;MUS note (G#3, length 12)
 .byt $06,$88 ;MUS note (D#3, length 12)
 .byt     $8D ;MUS note (G#3, length 8)
 .byt $06,$8B ;MUS note (F#3, length 12)
 .byt $06,$86 ;MUS note (C#3, length 12)
 .byt     $8B ;MUS note (F#3, length 8)
 .byt $06,$8B ;MUS note (F#3, length 12)
 .byt $06,$86 ;MUS note (C#3, length 12)
 .byt     $8B ;MUS note (F#3, length 8)
 .byt $06,$89 ;MUS note (E-3, length 12)
 .byt $06,$84 ;MUS note (B-2, length 12)
 .byt     $89 ;MUS note (E-3, length 8)
 .byt $06,$89 ;MUS note (E-3, length 12)
 .byt $06,$84 ;MUS note (B-2, length 12)
 .byt     $89 ;MUS note (E-3, length 8)
 .byt $06,$88 ;MUS note (D#3, length 12)
 .byt $06,$89 ;MUS note (E-3, length 12)
 .byt     $8A ;MUS note (F-3, length 8)
 .byt MUSCMD_SetNoteBaseOffset, 2*12+9
 .byt MUSCMD_SetVibratoCtrl,    $0F
 .byt MUSCMD_SetVolumeAndEnv,   $09
 .byt     $6D ;MUS note (A#3, length 4)
 .byt     $6D ;MUS note (A#3, length 4)
 .byt     $6D ;MUS note (A#3, length 4)
 .byt     $6D ;MUS note (A#3, length 4)
 .byt     $68 ;MUS note (F-3, length 4)
 .byt     $68 ;MUS note (F-3, length 4)
 .byt     $68 ;MUS note (F-3, length 4)
 .byt     $68 ;MUS note (F-3, length 4)
 .byt MUSCMD_SetVibratoCtrl,    $00
 .byt MUSCMD_Loop, 0 
 .word :-
 .byt MUSCMD_EndOfData
MUS_BossMusic_chn3: ;at B01D
 .byt MUSCMD_SetSpeed,          $06
 .byt MUSCMD_SetVolumeAndEnv,   $37
 .byt     $E0 ;Pause         (length 64)
 .byt     $E0 ;Pause         (length 64)
 .byt MUSCMD_SetVolumeCurve, $81,$10
: ;at B026
 .byt MUSCMD_SetVolumeCurve, $81,$10
 .byt     $80 ;Pause         (length 8)
 .byt     $85 ;MUS note (D-3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $85 ;MUS note (D-3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $85 ;MUS note (D-3, length 8)
 .byt     $60 ;Pause         (length 4)
 .byt     $85 ;MUS note (D-3, length 8)
 .byt     $65 ;MUS note (D-3, length 4)
 .byt     $80 ;Pause         (length 8)
 .byt     $85 ;MUS note (D-3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $85 ;MUS note (D-3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $85 ;MUS note (D-3, length 8)
 .byt     $65 ;MUS note (D-3, length 4)
 .byt     $65 ;MUS note (D-3, length 4)
 .byt     $65 ;MUS note (D-3, length 4)
 .byt     $65 ;MUS note (D-3, length 4)
 .byt MUSCMD_Loop, 0 
 .word :-
MUS_BossMusic_vibratotable: ;at B040
 .byt $00, $E0, $80, $00 ;vibrato definition
 .byt $02, $22, $80, $00 ;vibrato definition
.endscope

MUS_Credits: ;at B048
.scope
 .byt $0F ;priority. Lo=music priority, Hi=sfx priority
 .word MUS_Credits_chn0               ; $B053 ;
 .word MUS_Credits_chn1               ; $B1B5 ;
 .word MUS_Credits_chn2               ; $B355 ;
 .word MUS_Credits_chn3               ; $B472 ;
 .word MUS_Credits_vibratotable       ; $B4C3 ;
MUS_Credits_chn0: ;at B053
 .byt MUSCMD_SetSpeed,          $08
: ;at B055
 .byt MUSCMD_SetNoteBaseOffset, 3*12+8
 .byt MUSCMD_SetDutyCycle,      $C0
 .byt MUSCMD_SetVolumeAndEnv,   $3E
 .byt MUSCMD_SetVolumeCurve, $DF,$40
 .byt MUSCMD_SetVibratoIndex,   $00
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $AE ;MUS note (A#4, length 16)
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt $06,$CE ;MUS note (A#4, length 48)
 .byt MUSCMD_SetVibratoIndex,   $00
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $CF ;MUS note (B-4, length 32)
 .byt     $8F ;MUS note (B-4, length 8)
 .byt $06,$B1 ;MUS note (C#5, length 24)
 .byt MUSCMD_Loop, 1 
 .word :-
 .byt     $AE ;MUS note (A#4, length 16)
 .byt     $B1 ;MUS note (C#5, length 16)
 .byt     $B8 ;MUS note (G#5, length 16)
 .byt     $95 ;MUS note (F-5, length 8)
 .byt     $96 ;MUS note (F#5, length 8)
 .byt     $A0 ;Pause         (length 16)
 .byt     $80 ;Pause         (length 8)
 .byt     $96 ;MUS note (F#5, length 8)
 .byt     $95 ;MUS note (F-5, length 8)
 .byt     $93 ;MUS note (D#5, length 8)
 .byt     $91 ;MUS note (C#5, length 8)
 .byt     $93 ;MUS note (D#5, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $8E ;MUS note (A#4, length 8)
 .byt     $8C ;MUS note (G#4, length 8)
 .byt     $8A ;MUS note (F#4, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $8A ;MUS note (F#4, length 8)
 .byt     $89 ;MUS note (F-4, length 8)
 .byt     $8A ;MUS note (F#4, length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $B1 ;MUS note (C#5, length 16)
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt $06,$D1 ;MUS note (C#5, length 48)
 .byt MUSCMD_SetVibratoIndex,   $00
 .byt $06,$B3 ;MUS note (D#5, length 24)
 .byt $06,$B1 ;MUS note (C#5, length 24)
 .byt     $AF ;MUS note (B-4, length 16)
 .byt $06,$B5 ;MUS note (F-5, length 24)
 .byt $06,$B3 ;MUS note (D#5, length 24)
 .byt     $B1 ;MUS note (C#5, length 16)
 .byt $06,$B5 ;MUS note (F-5, length 24)
 .byt $06,$B3 ;MUS note (D#5, length 24)
 .byt     $B2 ;MUS note (D-5, length 16)
 .byt $06,$B6 ;MUS note (F#5, length 24)
 .byt $06,$B5 ;MUS note (F-5, length 24)
 .byt     $B3 ;MUS note (D#5, length 16)
 .byt MUSCMD_SetSpeed,          $06
 .byt     $4C ;MUS note (G#4, length 2)
 .byt     $4F ;MUS note (B-4, length 2)
 .byt     $51 ;MUS note (C#5, length 2)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $56 ;MUS note (F#5, length 2)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $96 ;MUS note (F#5, length 8)
 .byt $06,$D6 ;MUS note (F#5, length 48)
 .byt MUSCMD_SetNoteBaseOffset, 2*12+8
 .byt MUSCMD_SetDutyCycle,      $80
 .byt MUSCMD_SetVolumeAndEnv,   $3E
 .byt MUSCMD_SetVolumeCurve, $86,$20
 .byt MUSCMD_SetVibratoIndex,   $01
: ;at B0B7
 .byt     $8E ;MUS note (A#3, length 8)
 .byt     $A0 ;Pause         (length 16)
 .byt     $85 ;MUS note (C#3, length 8)
 .byt $06,$80 ;Pause         (length 12)
 .byt     $65 ;MUS note (C#3, length 4)
 .byt     $6A ;MUS note (F#3, length 4)
 .byt $06,$8E ;MUS note (A#3, length 12)
 .byt     $8F ;MUS note (B-3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $A0 ;Pause         (length 16)
 .byt $06,$AF ;MUS note (B-3, length 24)
 .byt     $70 ;MUS note (C-4, length 4)
 .byt     $71 ;MUS note (C#4, length 4)
 .byt MUSCMD_Loop, 1 
 .word :-
 .byt     $AE ;MUS note (A#3, length 16)
 .byt     $B1 ;MUS note (C#4, length 16)
 .byt     $B5 ;MUS note (F-4, length 16)
 .byt     $96 ;MUS note (F#4, length 8)
 .byt     $93 ;MUS note (D#4, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $8E ;MUS note (A#3, length 8)
 .byt     $8C ;MUS note (G#3, length 8)
 .byt     $8A ;MUS note (F#3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $8A ;MUS note (F#3, length 8)
 .byt     $89 ;MUS note (F-3, length 8)
 .byt     $8A ;MUS note (F#3, length 8)
 .byt     $4F ;MUS note (B-3, length 2)
 .byt     $50 ;MUS note (C-4, length 2)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $71 ;MUS note (C#4, length 4)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $91 ;MUS note (C#4, length 8)
 .byt MUSCMD_SetVibratoIndex,   $02
 .byt $06,$D1 ;MUS note (C#4, length 48)
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt MUSCMD_SetDutyCycle,      $00
 .byt MUSCMD_SetVolumeCurve, $FF,$10
 .byt     $80 ;Pause         (length 8)
 .byt     $93 ;MUS note (D#4, length 8)
 .byt     $93 ;MUS note (D#4, length 8)
 .byt     $91 ;MUS note (C#4, length 8)
 .byt     $C0 ;Pause         (length 32)
 .byt MUSCMD_SetDutyCycle,      $80
 .byt MUSCMD_SetVolumeCurve, $86,$20
: ;at B0F3
 .byt     $8E ;MUS note (A#3, length 8)
 .byt     $A0 ;Pause         (length 16)
 .byt     $85 ;MUS note (C#3, length 8)
 .byt $06,$80 ;Pause         (length 12)
 .byt     $65 ;MUS note (C#3, length 4)
 .byt     $6A ;MUS note (F#3, length 4)
 .byt $06,$8E ;MUS note (A#3, length 12)
 .byt     $8F ;MUS note (B-3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $A0 ;Pause         (length 16)
 .byt $06,$AF ;MUS note (B-3, length 24)
 .byt     $70 ;MUS note (C-4, length 4)
 .byt     $71 ;MUS note (C#4, length 4)
 .byt MUSCMD_Loop, 1 
 .word :-
 .byt     $AE ;MUS note (A#3, length 16)
 .byt     $B1 ;MUS note (C#4, length 16)
 .byt     $B5 ;MUS note (F-4, length 16)
 .byt     $96 ;MUS note (F#4, length 8)
 .byt     $93 ;MUS note (D#4, length 8)
 .byt     $C0 ;Pause         (length 32)
 .byt     $80 ;Pause         (length 8)
 .byt     $95 ;MUS note (F-4, length 8)
 .byt     $93 ;MUS note (D#4, length 8)
 .byt     $95 ;MUS note (F-4, length 8)
 .byt     $53 ;MUS note (D#4, length 2)
 .byt     $55 ;MUS note (F-4, length 2)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $76 ;MUS note (F#4, length 4)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $96 ;MUS note (F#4, length 8)
 .byt MUSCMD_SetVibratoIndex,   $02
 .byt $06,$D6 ;MUS note (F#4, length 48)
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt MUSCMD_SetDutyCycle,      $00
 .byt MUSCMD_SetVolumeCurve, $FF,$10
 .byt     $80 ;Pause         (length 8)
 .byt     $8F ;MUS note (B-3, length 8)
 .byt     $8F ;MUS note (B-3, length 8)
 .byt     $8E ;MUS note (A#3, length 8)
 .byt     $C0 ;Pause         (length 32)
 .byt MUSCMD_SetDutyCycle,      $80
 .byt MUSCMD_SetVolumeCurve, $FF,$20
 .byt $06,$CA ;MUS note (F#3, length 48)
 .byt     $AD ;MUS note (A-3, length 16)
 .byt $06,$CC ;MUS note (G#3, length 48)
 .byt     $AA ;MUS note (F#3, length 16)
 .byt     $E8 ;MUS note (E-3, length 64)
 .byt     $E0 ;Pause         (length 64)
 .byt $06,$CA ;MUS note (F#3, length 48)
 .byt     $AD ;MUS note (A-3, length 16)
 .byt     $CC ;MUS note (G#3, length 32)
 .byt $30,$A0 ;Pause         (length 16/1.5) (triplet)
 .byt $30,$AD ;MUS note (A-3, length 16/1.5) (triplet)
 .byt $30,$AF ;MUS note (B-3, length 16/1.5) (triplet)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $F1 ;MUS note (C#4, length 64)
 .byt MUSCMD_SetVibratoIndex,   $02
 .byt     $F1 ;MUS note (C#4, length 64)
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt     $D2 ;MUS note (D-4, length 32)
 .byt $30,$A0 ;Pause         (length 16/1.5) (triplet)
 .byt $30,$B1 ;MUS note (C#4, length 16/1.5) (triplet)
 .byt $30,$B2 ;MUS note (D-4, length 16/1.5) (triplet)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $AF ;MUS note (B-3, length 16)
 .byt MUSCMD_SetVibratoIndex,   $02
 .byt     $AF ;MUS note (B-3, length 16)
 .byt     $C0 ;Pause         (length 32)
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt     $D4 ;MUS note (E-4, length 32)
 .byt $30,$A0 ;Pause         (length 16/1.5) (triplet)
 .byt $30,$B2 ;MUS note (D-4, length 16/1.5) (triplet)
 .byt $30,$B4 ;MUS note (E-4, length 16/1.5) (triplet)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $B1 ;MUS note (C#4, length 16)
 .byt MUSCMD_SetVibratoIndex,   $02
 .byt $06,$D1 ;MUS note (C#4, length 48)
 .byt MUSCMD_SetVibratoIndex,   $00
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $B6 ;MUS note (F#4, length 16)
 .byt MUSCMD_SetVibratoIndex,   $03
 .byt $06,$D6 ;MUS note (F#4, length 48)
 .byt MUSCMD_SetVibratoIndex,   $00
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $B5 ;MUS note (F-4, length 16)
 .byt MUSCMD_SetVibratoIndex,   $03
 .byt $06,$D5 ;MUS note (F-4, length 48)
 .byt MUSCMD_SetVibratoIndex,   $00
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $B4 ;MUS note (E-4, length 16)
 .byt MUSCMD_SetVibratoIndex,   $03
 .byt $06,$D4 ;MUS note (E-4, length 48)
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt     $C0 ;Pause         (length 32)
 .byt     $A0 ;Pause         (length 16)
 .byt $30,$94 ;MUS note (E-4, length 8/1.5) (triplet)
 .byt $30,$96 ;MUS note (F#4, length 8/1.5) (triplet)
 .byt $30,$98 ;MUS note (G#4, length 8/1.5) (triplet)
 .byt MUSCMD_SetDutyCycle,      $40
 .byt MUSCMD_SetVolumeCurve, $EF,$F0
 .byt MUSCMD_SetNoteBaseOffset, 1*12+8
 .byt     $99 ;MUS note (A-3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $91 ;MUS note (C#3, length 8)
 .byt MUSCMD_SetNoteDelay     + $2
 .byt     $8F ;MUS note (B-2, length 8)
 .byt     $CF ;MUS note (B-2, length 32)
 .byt MUSCMD_SetVibratoIndex,   $03
 .byt     $EF ;MUS note (B-2, length 64)
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt     $A0 ;Pause         (length 16)
 .byt     $91 ;MUS note (C#3, length 8)
 .byt     $AF ;MUS note (B-2, length 16)
 .byt     $B4 ;MUS note (E-3, length 16)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $92 ;MUS note (D-3, length 8)
 .byt     $F2 ;MUS note (D-3, length 64)
 .byt     $A0 ;Pause         (length 16)
 .byt     $91 ;MUS note (C#3, length 8)
 .byt MUSCMD_SetNoteDelay     + $2
 .byt     $8F ;MUS note (B-2, length 8)
 .byt     $CF ;MUS note (B-2, length 32)
 .byt MUSCMD_SetVibratoIndex,   $03
 .byt     $EF ;MUS note (B-2, length 64)
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt     $A0 ;Pause         (length 16)
 .byt     $91 ;MUS note (C#3, length 8)
 .byt     $AF ;MUS note (B-2, length 16)
 .byt $06,$B4 ;MUS note (E-3, length 24)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $F8 ;MUS note (G#3, length 64)
 .byt MUSCMD_SetVolumeAndEnv,   $3A
 .byt MUSCMD_SetVibratoIndex,   $02
 .byt MUSCMD_SetSpeed,          $08
 .byt     $F8 ;MUS note (G#3, length 64)
 .byt MUSCMD_EndOfData
MUS_Credits_chn1: ;at B1B5
 .byt MUSCMD_SetSpeed,          $08
 .byt MUSCMD_SetDutyCycle,      $C0
 .byt MUSCMD_SetVolumeAndEnv,   $3A
 .byt MUSCMD_SetVolumeCurve, $8A,$30
 .byt MUSCMD_SetVibratoIndex,   $02
 .byt MUSCMD_SetNoteBaseOffset, 2*12+8
: ;at B1C2
 .byt     $80 ;Pause         (length 8)
 .byt     $91 ;MUS note (C#4, length 8)
 .byt     $96 ;MUS note (F#4, length 8)
 .byt     $98 ;MUS note (G#4, length 8)
 .byt     $DA ;MUS note (A#4, length 32)
 .byt     $80 ;Pause         (length 8)
 .byt     $92 ;MUS note (D-4, length 8)
 .byt     $96 ;MUS note (F#4, length 8)
 .byt     $98 ;MUS note (G#4, length 8)
 .byt     $DB ;MUS note (B-4, length 32)
 .byt MUSCMD_Loop, 1 
 .word :-
 .byt     $96 ;MUS note (F#4, length 8)
 .byt     $91 ;MUS note (C#4, length 8)
 .byt     $96 ;MUS note (F#4, length 8)
 .byt     $9A ;MUS note (A#4, length 8)
 .byt     $95 ;MUS note (F-4, length 8)
 .byt     $91 ;MUS note (C#4, length 8)
 .byt     $95 ;MUS note (F-4, length 8)
 .byt     $98 ;MUS note (G#4, length 8)
 .byt     $93 ;MUS note (D#4, length 8)
 .byt     $8E ;MUS note (A#3, length 8)
 .byt     $93 ;MUS note (D#4, length 8)
 .byt     $96 ;MUS note (F#4, length 8)
 .byt     $91 ;MUS note (C#4, length 8)
 .byt     $8C ;MUS note (G#3, length 8)
 .byt     $91 ;MUS note (C#4, length 8)
 .byt     $96 ;MUS note (F#4, length 8)
: ;at B1E0
 .byt     $90 ;MUS note (C-4, length 8)
 .byt     $8C ;MUS note (G#3, length 8)
 .byt     $90 ;MUS note (C-4, length 8)
 .byt     $93 ;MUS note (D#4, length 8)
 .byt MUSCMD_Loop, 1 
 .word :-
 .byt     $D1 ;MUS note (C#4, length 32)
 .byt     $91 ;MUS note (C#4, length 8)
 .byt $06,$B0 ;MUS note (C-4, length 24)
 .byt     $8F ;MUS note (B-3, length 8)
 .byt     $8A ;MUS note (F#3, length 8)
 .byt     $8F ;MUS note (B-3, length 8)
 .byt     $91 ;MUS note (C#4, length 8)
 .byt     $D3 ;MUS note (D#4, length 32)
 .byt     $91 ;MUS note (C#4, length 8)
 .byt     $8C ;MUS note (G#3, length 8)
 .byt     $91 ;MUS note (C#4, length 8)
 .byt     $93 ;MUS note (D#4, length 8)
 .byt     $D5 ;MUS note (F-4, length 32)
 .byt     $92 ;MUS note (D-4, length 8)
 .byt     $8E ;MUS note (A#3, length 8)
 .byt     $92 ;MUS note (D-4, length 8)
 .byt     $93 ;MUS note (D#4, length 8)
 .byt     $D5 ;MUS note (F-4, length 32)
 .byt     $93 ;MUS note (D#4, length 8)
 .byt     $8E ;MUS note (A#3, length 8)
 .byt     $93 ;MUS note (D#4, length 8)
 .byt     $95 ;MUS note (F-4, length 8)
 .byt     $D6 ;MUS note (F#4, length 32)
 .byt MUSCMD_SetSpeed,          $06
 .byt     $51 ;MUS note (C#4, length 2)
 .byt     $56 ;MUS note (F#4, length 2)
 .byt     $58 ;MUS note (G#4, length 2)
 .byt MUSCMD_SetNoteDelay     + $2
 .byt     $5B ;MUS note (B-4, length 2)
 .byt     $9B ;MUS note (B-4, length 8)
 .byt $06,$DB ;MUS note (B-4, length 48)
 .byt MUSCMD_SetDutyCycle,      $80
 .byt MUSCMD_SetVolumeCurve, $FF,$20
: ;at B20F
 .byt     $8A ;MUS note (F#3, length 8)
 .byt     $A0 ;Pause         (length 16)
 .byt     $82 ;MUS note (A#2, length 8)
 .byt $06,$80 ;Pause         (length 12)
 .byt     $62 ;MUS note (A#2, length 4)
 .byt     $65 ;MUS note (C#3, length 4)
 .byt $06,$8A ;MUS note (F#3, length 12)
 .byt     $86 ;MUS note (D-3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $A0 ;Pause         (length 16)
 .byt $06,$A6 ;MUS note (D-3, length 24)
 .byt     $66 ;MUS note (D-3, length 4)
 .byt     $6C ;MUS note (G#3, length 4)
 .byt MUSCMD_Loop, 1 
 .word :-
 .byt     $AA ;MUS note (F#3, length 16)
 .byt     $AE ;MUS note (A#3, length 16)
 .byt     $8E ;MUS note (A#3, length 8)
 .byt     $AE ;MUS note (A#3, length 16)
 .byt     $8E ;MUS note (A#3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $85 ;MUS note (C#3, length 8)
 .byt     $85 ;MUS note (C#3, length 8)
 .byt     $85 ;MUS note (C#3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $84 ;MUS note (C-3, length 8)
 .byt     $84 ;MUS note (C-3, length 8)
 .byt     $84 ;MUS note (C-3, length 8)
 .byt     $45 ;MUS note (C#3, length 2)
 .byt     $46 ;MUS note (D-3, length 2)
 .byt     $47 ;MUS note (D#3, length 2)
 .byt MUSCMD_SetNoteDelay     + $2
 .byt     $49 ;MUS note (F-3, length 2)
 .byt     $89 ;MUS note (F-3, length 8)
 .byt $06,$C9 ;MUS note (F-3, length 48)
 .byt MUSCMD_SetDutyCycle,      $00
 .byt MUSCMD_SetVolumeCurve, $FF,$10
 .byt     $80 ;Pause         (length 8)
 .byt     $8A ;MUS note (F#3, length 8)
 .byt     $8A ;MUS note (F#3, length 8)
 .byt     $89 ;MUS note (F-3, length 8)
 .byt     $C0 ;Pause         (length 32)
 .byt MUSCMD_SetDutyCycle,      $80
 .byt MUSCMD_SetVolumeCurve, $FF,$20
: ;at B247
 .byt     $8A ;MUS note (F#3, length 8)
 .byt     $A0 ;Pause         (length 16)
 .byt     $82 ;MUS note (A#2, length 8)
 .byt $06,$80 ;Pause         (length 12)
 .byt     $62 ;MUS note (A#2, length 4)
 .byt     $65 ;MUS note (C#3, length 4)
 .byt $06,$8A ;MUS note (F#3, length 12)
 .byt     $86 ;MUS note (D-3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $A0 ;Pause         (length 16)
 .byt $06,$A6 ;MUS note (D-3, length 24)
 .byt     $66 ;MUS note (D-3, length 4)
 .byt     $6C ;MUS note (G#3, length 4)
 .byt MUSCMD_Loop, 1 
 .word :-
 .byt     $AA ;MUS note (F#3, length 16)
 .byt     $AA ;MUS note (F#3, length 16)
 .byt     $91 ;MUS note (C#4, length 8)
 .byt     $B3 ;MUS note (D#4, length 16)
 .byt     $8F ;MUS note (B-3, length 8)
 .byt     $C0 ;Pause         (length 32)
 .byt     $80 ;Pause         (length 8)
 .byt     $91 ;MUS note (C#4, length 8)
 .byt     $91 ;MUS note (C#4, length 8)
 .byt     $8F ;MUS note (B-3, length 8)
 .byt     $4B ;MUS note (G-3, length 2)
 .byt     $4C ;MUS note (G#3, length 2)
 .byt     $4D ;MUS note (A-3, length 2)
 .byt MUSCMD_SetNoteDelay     + $2
 .byt     $4E ;MUS note (A#3, length 2)
 .byt     $8E ;MUS note (A#3, length 8)
 .byt $06,$CE ;MUS note (A#3, length 48)
 .byt MUSCMD_SetDutyCycle,      $00
 .byt MUSCMD_SetVolumeCurve, $FF,$10
 .byt     $80 ;Pause         (length 8)
 .byt     $86 ;MUS note (D-3, length 8)
 .byt     $86 ;MUS note (D-3, length 8)
 .byt     $85 ;MUS note (C#3, length 8)
 .byt     $C0 ;Pause         (length 32)
 .byt MUSCMD_SetDutyCycle,      $C0
 .byt MUSCMD_SetVolumeCurve, $CA,$40
 .byt MUSCMD_SetNoteBaseOffset, 1*12+8
 .byt     $6D ;MUS note (A-2, length 4)
 .byt     $6F ;MUS note (B-2, length 4)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $92 ;MUS note (D-3, length 8)
 .byt $06,$D2 ;MUS note (D-3, length 48)
 .byt     $6F ;MUS note (B-2, length 4)
 .byt     $72 ;MUS note (D-3, length 4)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $94 ;MUS note (E-3, length 8)
 .byt $06,$D4 ;MUS note (E-3, length 48)
 .byt     $6C ;MUS note (G#2, length 4)
 .byt     $6F ;MUS note (B-2, length 4)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $91 ;MUS note (C#3, length 8)
 .byt $06,$D1 ;MUS note (C#3, length 48)
 .byt MUSCMD_SetVolumeAndEnv,   $3E
 .byt MUSCMD_SetNoteBaseOffset, 3*12+8
 .byt     $96 ;MUS note (F#5, length 8)
 .byt     $94 ;MUS note (E-5, length 8)
 .byt     $91 ;MUS note (C#5, length 8)
 .byt     $8F ;MUS note (B-4, length 8)
 .byt MUSCMD_SetNoteBaseOffset, 2*12+8
 .byt     $96 ;MUS note (F#4, length 8)
 .byt     $94 ;MUS note (E-4, length 8)
 .byt     $91 ;MUS note (C#4, length 8)
 .byt     $8F ;MUS note (B-3, length 8)
 .byt MUSCMD_SetVolumeAndEnv,   $3A
 .byt MUSCMD_SetNoteBaseOffset, 1*12+8
 .byt     $6D ;MUS note (A-2, length 4)
 .byt     $6F ;MUS note (B-2, length 4)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $92 ;MUS note (D-3, length 8)
 .byt $06,$D2 ;MUS note (D-3, length 48)
 .byt     $6F ;MUS note (B-2, length 4)
 .byt     $72 ;MUS note (D-3, length 4)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $94 ;MUS note (E-3, length 8)
 .byt $06,$D4 ;MUS note (E-3, length 48)
 .byt MUSCMD_SetNoteBaseOffset, 2*12+8
 .byt     $EF ;MUS note (B-3, length 64)
 .byt     $EE ;MUS note (A#3, length 64)
 .byt     $6A ;MUS note (F#3, length 4)
 .byt     $6D ;MUS note (A-3, length 4)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $8F ;MUS note (B-3, length 8)
 .byt     $AF ;MUS note (B-3, length 16)
 .byt $30,$A0 ;Pause         (length 16/1.5) (triplet)
 .byt $30,$AD ;MUS note (A-3, length 16/1.5) (triplet)
 .byt $30,$AF ;MUS note (B-3, length 16/1.5) (triplet)
 .byt     $EC ;MUS note (G#3, length 64)
 .byt     $6B ;MUS note (G-3, length 4)
 .byt     $6F ;MUS note (B-3, length 4)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $91 ;MUS note (C#4, length 8)
 .byt     $B1 ;MUS note (C#4, length 16)
 .byt $30,$A0 ;Pause         (length 16/1.5) (triplet)
 .byt $30,$AF ;MUS note (B-3, length 16/1.5) (triplet)
 .byt $30,$B1 ;MUS note (C#4, length 16/1.5) (triplet)
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $AE ;MUS note (A#3, length 16)
 .byt MUSCMD_SetVibratoIndex,   $02
 .byt $06,$CE ;MUS note (A#3, length 48)
: ;at B2D1
 .byt MUSCMD_SetVibratoIndex,   $00
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $B2 ;MUS note (D-4, length 16)
 .byt MUSCMD_SetVibratoIndex,   $03
 .byt $06,$D2 ;MUS note (D-4, length 48)
 .byt MUSCMD_Loop, 1 
 .word :-
 .byt MUSCMD_SetVibratoIndex,   $00
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $AC ;MUS note (G#3, length 16)
 .byt MUSCMD_SetVibratoIndex,   $03
 .byt $06,$CC ;MUS note (G#3, length 48)
 .byt MUSCMD_SetVibratoIndex,   $00
 .byt     $C0 ;Pause         (length 32)
 .byt     $A0 ;Pause         (length 16)
 .byt $30,$88 ;MUS note (E-3, length 8/1.5) (triplet)
 .byt $30,$87 ;MUS note (D#3, length 8/1.5) (triplet)
 .byt $30,$83 ;MUS note (B-2, length 8/1.5) (triplet)
 .byt     $81 ;MUS note (A-2, length 8)
 .byt MUSCMD_SetDutyCycle,      $40
 .byt MUSCMD_SetVolumeCurve, $EF,$F0
 .byt MUSCMD_SetNoteBaseOffset, 1*12+8
 .byt MUSCMD_SetVolumeAndEnv,   $39
 .byt     $80 ;Pause         (length 8)
 .byt     $8D ;MUS note (A-2, length 8)
 .byt MUSCMD_SetNoteDelay     + $2
 .byt     $8B ;MUS note (G-2, length 8)
 .byt     $CB ;MUS note (G-2, length 32)
 .byt MUSCMD_SetVibratoIndex,   $03
 .byt     $EB ;MUS note (G-2, length 64)
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt     $A0 ;Pause         (length 16)
 .byt     $8D ;MUS note (A-2, length 8)
 .byt     $AB ;MUS note (G-2, length 16)
 .byt     $AF ;MUS note (B-2, length 16)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $8D ;MUS note (A-2, length 8)
 .byt     $ED ;MUS note (A-2, length 64)
 .byt     $A0 ;Pause         (length 16)
 .byt     $8D ;MUS note (A-2, length 8)
 .byt MUSCMD_SetNoteDelay     + $2
 .byt     $8B ;MUS note (G-2, length 8)
 .byt     $CB ;MUS note (G-2, length 32)
 .byt MUSCMD_SetVibratoIndex,   $03
 .byt     $EB ;MUS note (G-2, length 64)
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt     $A0 ;Pause         (length 16)
 .byt     $8D ;MUS note (A-2, length 8)
 .byt     $AB ;MUS note (G-2, length 16)
 .byt $06,$AF ;MUS note (B-2, length 24)
 .byt     $6D ;MUS note (A-2, length 4)
 .byt     $71 ;MUS note (C#3, length 4)
 .byt     $74 ;MUS note (E-3, length 4)
 .byt     $78 ;MUS note (G#3, length 4)
 .byt     $DB ;MUS note (B-3, length 32)
 .byt MUSCMD_SetSpeed,          $08
 .byt MUSCMD_SetDutyCycle,      $00
 .byt MUSCMD_SetVolumeAndEnv,   $3F
 .byt MUSCMD_SetNoteBaseOffset, 4*12+8
 .byt     $4D ;MUS note (A-5, length 2)
 .byt     $51 ;MUS note (C#6, length 2)
 .byt     $54 ;MUS note (E-6, length 2)
 .byt     $58 ;MUS note (G#6, length 2)
 .byt MUSCMD_SetVolumeAndEnv,   $3E
 .byt     $4D ;MUS note (A-5, length 2)
 .byt     $51 ;MUS note (C#6, length 2)
 .byt     $54 ;MUS note (E-6, length 2)
 .byt     $58 ;MUS note (G#6, length 2)
 .byt MUSCMD_SetVolumeAndEnv,   $3C
 .byt     $4D ;MUS note (A-5, length 2)
 .byt     $51 ;MUS note (C#6, length 2)
 .byt     $54 ;MUS note (E-6, length 2)
 .byt     $58 ;MUS note (G#6, length 2)
 .byt MUSCMD_SetVolumeAndEnv,   $3A
 .byt     $4D ;MUS note (A-5, length 2)
 .byt     $51 ;MUS note (C#6, length 2)
 .byt     $54 ;MUS note (E-6, length 2)
 .byt     $58 ;MUS note (G#6, length 2)
 .byt MUSCMD_SetVolumeAndEnv,   $39
 .byt     $4D ;MUS note (A-5, length 2)
 .byt     $51 ;MUS note (C#6, length 2)
 .byt     $54 ;MUS note (E-6, length 2)
 .byt     $58 ;MUS note (G#6, length 2)
 .byt MUSCMD_SetVolumeAndEnv,   $37
 .byt     $4D ;MUS note (A-5, length 2)
 .byt     $51 ;MUS note (C#6, length 2)
 .byt     $54 ;MUS note (E-6, length 2)
 .byt     $58 ;MUS note (G#6, length 2)
 .byt MUSCMD_SetVolumeAndEnv,   $35
 .byt     $4D ;MUS note (A-5, length 2)
 .byt     $51 ;MUS note (C#6, length 2)
 .byt     $54 ;MUS note (E-6, length 2)
 .byt     $58 ;MUS note (G#6, length 2)
 .byt MUSCMD_SetVolumeAndEnv,   $32
 .byt     $4D ;MUS note (A-5, length 2)
 .byt     $51 ;MUS note (C#6, length 2)
 .byt     $54 ;MUS note (E-6, length 2)
 .byt     $58 ;MUS note (G#6, length 2)
 .byt MUSCMD_EndOfData
MUS_Credits_chn2: ;at B355
 .byt MUSCMD_SetSpeed,          $08
: ;at B357
 .byt MUSCMD_SetVolumeAndEnv,   $81
 .byt MUSCMD_SetNoteBaseOffset, 2*12+8
 .byt $06,$AA ;MUS note (F#3, length 24)
 .byt     $8A ;MUS note (F#3, length 8)
 .byt     $AA ;MUS note (F#3, length 16)
 .byt     $A0 ;Pause         (length 16)
 .byt MUSCMD_Loop, 3 
 .word :-
 .byt     $CA ;MUS note (F#3, length 32)
 .byt     $C9 ;MUS note (F-3, length 32)
 .byt     $C7 ;MUS note (D#3, length 32)
 .byt     $C5 ;MUS note (C#3, length 32)
 .byt     $C4 ;MUS note (C-3, length 32)
 .byt     $C7 ;MUS note (D#3, length 32)
 .byt $06,$A5 ;MUS note (C#3, length 24)
 .byt     $85 ;MUS note (C#3, length 8)
 .byt     $A5 ;MUS note (C#3, length 16)
 .byt     $A0 ;Pause         (length 16)
 .byt $06,$C3 ;MUS note (B-2, length 48)
 .byt     $A4 ;MUS note (C-3, length 16)
 .byt     $E5 ;MUS note (C#3, length 64)
 .byt $06,$C2 ;MUS note (A#2, length 48)
 .byt     $A6 ;MUS note (D-3, length 16)
 .byt     $E7 ;MUS note (D#3, length 64)
 .byt MUSCMD_SetSpeed,          $06
 .byt MUSCMD_SetVibratoCtrl,    $0F
 .byt MUSCMD_SetVolumeAndEnv,   $15
 .byt MUSCMD_SetNoteBaseOffset, 2*12+11
 .byt     $A0 ;Pause         (length 16)
 .byt     $72 ;MUS note (F-4, length 4)
 .byt     $72 ;MUS note (F-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $72 ;MUS note (F-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $6D ;MUS note (C-4, length 4)
 .byt     $6D ;MUS note (C-4, length 4)
 .byt     $6D ;MUS note (C-4, length 4)
 .byt     $68 ;MUS note (G-3, length 4)
 .byt     $68 ;MUS note (G-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $68 ;MUS note (G-3, length 4)
 .byt MUSCMD_SetVibratoCtrl,    $00
 .byt MUSCMD_SetVolumeAndEnv,   $4F
 .byt MUSCMD_SetNoteBaseOffset, 2*12+8
: ;at B392
 .byt     $8A ;MUS note (F#3, length 8)
 .byt     $8A ;MUS note (F#3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $8A ;MUS note (F#3, length 8)
 .byt     $AA ;MUS note (F#3, length 16)
 .byt     $A0 ;Pause         (length 16)
 .byt MUSCMD_Loop, 3 
 .word :-
 .byt     $8A ;MUS note (F#3, length 8)
 .byt     $8A ;MUS note (F#3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $8A ;MUS note (F#3, length 8)
 .byt     $A9 ;MUS note (F-3, length 16)
 .byt     $A0 ;Pause         (length 16)
 .byt     $87 ;MUS note (D#3, length 8)
 .byt     $87 ;MUS note (D#3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $87 ;MUS note (D#3, length 8)
 .byt     $A5 ;MUS note (C#3, length 16)
 .byt     $A0 ;Pause         (length 16)
 .byt     $84 ;MUS note (C-3, length 8)
 .byt     $84 ;MUS note (C-3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $84 ;MUS note (C-3, length 8)
 .byt     $A4 ;MUS note (C-3, length 16)
 .byt     $A0 ;Pause         (length 16)
 .byt     $85 ;MUS note (C#3, length 8)
 .byt     $85 ;MUS note (C#3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $85 ;MUS note (C#3, length 8)
 .byt     $A5 ;MUS note (C#3, length 16)
 .byt     $A0 ;Pause         (length 16)
: ;at B3B4
 .byt     $8A ;MUS note (F#3, length 8)
 .byt     $8A ;MUS note (F#3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $8A ;MUS note (F#3, length 8)
 .byt     $AA ;MUS note (F#3, length 16)
 .byt     $A0 ;Pause         (length 16)
 .byt MUSCMD_Loop, 3 
 .word :-
 .byt     $8A ;MUS note (F#3, length 8)
 .byt     $8A ;MUS note (F#3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $8C ;MUS note (G#3, length 8)
 .byt     $AE ;MUS note (A#3, length 16)
 .byt     $A0 ;Pause         (length 16)
 .byt     $8F ;MUS note (B-3, length 8)
 .byt     $8F ;MUS note (B-3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $90 ;MUS note (C-4, length 8)
 .byt     $B1 ;MUS note (C#4, length 16)
 .byt     $A0 ;Pause         (length 16)
 .byt     $8A ;MUS note (F#3, length 8)
 .byt     $8A ;MUS note (F#3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $8A ;MUS note (F#3, length 8)
 .byt     $AA ;MUS note (F#3, length 16)
 .byt     $A0 ;Pause         (length 16)
 .byt MUSCMD_SetNoteBaseOffset, 2*12+11
 .byt MUSCMD_SetVibratoCtrl,    $0F
 .byt MUSCMD_SetVolumeAndEnv,   $15
 .byt     $72 ;MUS note (F-4, length 4)
 .byt     $72 ;MUS note (F-4, length 4)
 .byt     $72 ;MUS note (F-4, length 4)
 .byt     $72 ;MUS note (F-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $6D ;MUS note (C-4, length 4)
 .byt     $6D ;MUS note (C-4, length 4)
 .byt     $6D ;MUS note (C-4, length 4)
 .byt     $68 ;MUS note (G-3, length 4)
 .byt     $68 ;MUS note (G-3, length 4)
 .byt     $68 ;MUS note (G-3, length 4)
 .byt     $68 ;MUS note (G-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $66 ;MUS note (F-3, length 4)
 .byt     $66 ;MUS note (F-3, length 4)
 .byt     $66 ;MUS note (F-3, length 4)
 .byt MUSCMD_SetNoteBaseOffset, 2*12+8
 .byt MUSCMD_SetVibratoCtrl,    $00
 .byt MUSCMD_SetVolumeAndEnv,   $7F
 .byt     $86 ;MUS note (D-3, length 8)
 .byt     $86 ;MUS note (D-3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $86 ;MUS note (D-3, length 8)
 .byt     $A6 ;MUS note (D-3, length 16)
 .byt     $A0 ;Pause         (length 16)
 .byt     $88 ;MUS note (E-3, length 8)
 .byt     $88 ;MUS note (E-3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $88 ;MUS note (E-3, length 8)
 .byt     $A8 ;MUS note (E-3, length 16)
 .byt     $A0 ;Pause         (length 16)
: ;at B3F8
 .byt     $81 ;MUS note (A-2, length 8)
 .byt     $81 ;MUS note (A-2, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $81 ;MUS note (A-2, length 8)
 .byt     $A1 ;MUS note (A-2, length 16)
 .byt     $A0 ;Pause         (length 16)
 .byt MUSCMD_Loop, 1 
 .word :-
 .byt     $86 ;MUS note (D-3, length 8)
 .byt     $86 ;MUS note (D-3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $86 ;MUS note (D-3, length 8)
 .byt     $A6 ;MUS note (D-3, length 16)
 .byt     $A0 ;Pause         (length 16)
 .byt     $88 ;MUS note (E-3, length 8)
 .byt     $88 ;MUS note (E-3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $88 ;MUS note (E-3, length 8)
 .byt     $A8 ;MUS note (E-3, length 16)
 .byt     $A0 ;Pause         (length 16)
 .byt     $8A ;MUS note (F#3, length 8)
 .byt     $8A ;MUS note (F#3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $8A ;MUS note (F#3, length 8)
 .byt     $AA ;MUS note (F#3, length 16)
 .byt     $A0 ;Pause         (length 16)
 .byt     $88 ;MUS note (E-3, length 8)
 .byt     $88 ;MUS note (E-3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $88 ;MUS note (E-3, length 8)
 .byt     $A8 ;MUS note (E-3, length 16)
 .byt     $A0 ;Pause         (length 16)
 .byt     $83 ;MUS note (B-2, length 8)
 .byt     $83 ;MUS note (B-2, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $83 ;MUS note (B-2, length 8)
 .byt     $A3 ;MUS note (B-2, length 16)
 .byt     $A0 ;Pause         (length 16)
 .byt     $88 ;MUS note (E-3, length 8)
 .byt     $88 ;MUS note (E-3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $88 ;MUS note (E-3, length 8)
 .byt     $86 ;MUS note (D-3, length 8)
 .byt     $86 ;MUS note (D-3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $86 ;MUS note (D-3, length 8)
 .byt     $85 ;MUS note (C#3, length 8)
 .byt     $85 ;MUS note (C#3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $85 ;MUS note (C#3, length 8)
 .byt     $A5 ;MUS note (C#3, length 16)
 .byt     $A0 ;Pause         (length 16)
 .byt     $8A ;MUS note (F#3, length 8)
 .byt     $8A ;MUS note (F#3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $8A ;MUS note (F#3, length 8)
 .byt     $AA ;MUS note (F#3, length 16)
 .byt     $A0 ;Pause         (length 16)
: ;at B434
 .byt     $83 ;MUS note (B-2, length 8)
 .byt     $83 ;MUS note (B-2, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $83 ;MUS note (B-2, length 8)
 .byt     $A3 ;MUS note (B-2, length 16)
 .byt     $A0 ;Pause         (length 16)
 .byt MUSCMD_Loop, 1 
 .word :-
 .byt     $88 ;MUS note (E-3, length 8)
 .byt     $88 ;MUS note (E-3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $88 ;MUS note (E-3, length 8)
 .byt     $A8 ;MUS note (E-3, length 16)
 .byt     $A0 ;Pause         (length 16)
 .byt MUSCMD_SetNoteBaseOffset, 2*12+11
 .byt MUSCMD_SetVibratoCtrl,    $0F
 .byt MUSCMD_SetVolumeAndEnv,   $15
 .byt     $72 ;MUS note (F-4, length 4)
 .byt     $72 ;MUS note (F-4, length 4)
 .byt     $72 ;MUS note (F-4, length 4)
 .byt     $72 ;MUS note (F-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $6D ;MUS note (C-4, length 4)
 .byt     $6D ;MUS note (C-4, length 4)
 .byt     $6D ;MUS note (C-4, length 4)
 .byt     $68 ;MUS note (G-3, length 4)
 .byt     $68 ;MUS note (G-3, length 4)
 .byt     $68 ;MUS note (G-3, length 4)
 .byt     $68 ;MUS note (G-3, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $66 ;MUS note (F-3, length 4)
 .byt     $66 ;MUS note (F-3, length 4)
 .byt     $66 ;MUS note (F-3, length 4)
 .byt MUSCMD_SetVibratoCtrl,    $00
 .byt MUSCMD_SetVolumeAndEnv,   $81
 .byt MUSCMD_SetNoteBaseOffset, 2*12+8
: ;at B460
 .byt     $ED ;MUS note (A-3, length 64)
 .byt     $8D ;MUS note (A-3, length 8)
 .byt     $88 ;MUS note (E-3, length 8)
 .byt     $8D ;MUS note (A-3, length 8)
 .byt     $88 ;MUS note (E-3, length 8)
 .byt     $8D ;MUS note (A-3, length 8)
 .byt     $88 ;MUS note (E-3, length 8)
 .byt     $6D ;MUS note (A-3, length 4)
 .byt     $68 ;MUS note (E-3, length 4)
 .byt     $6A ;MUS note (F#3, length 4)
 .byt     $6C ;MUS note (G#3, length 4)
 .byt     $ED ;MUS note (A-3, length 64)
 .byt     $ED ;MUS note (A-3, length 64)
 .byt MUSCMD_Loop, 1 
 .word :-
 .byt MUSCMD_EndOfData
MUS_Credits_chn3: ;at B472
 .byt MUSCMD_SetSpeed,          $08
 .byt MUSCMD_SetVolumeAndEnv,   $3C
 .byt MUSCMD_SetVolumeCurve, $81,$10
: ;at B479
 .byt $06,$C0 ;Pause         (length 48)
 .byt     $85 ;MUS note (C#3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt MUSCMD_Loop, 11 
 .word :-
 .byt MUSCMD_SetSpeed,          $06
 .byt     $E0 ;Pause         (length 64)
: ;at B484
 .byt     $A0 ;Pause         (length 16)
 .byt     $85 ;MUS note (C#3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $A0 ;Pause         (length 16)
 .byt     $85 ;MUS note (C#3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $A0 ;Pause         (length 16)
 .byt     $85 ;MUS note (C#3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $A0 ;Pause         (length 16)
 .byt     $85 ;MUS note (C#3, length 8)
 .byt     $65 ;MUS note (C#3, length 4)
 .byt     $65 ;MUS note (C#3, length 4)
 .byt MUSCMD_Loop, 6 
 .word :-
 .byt     $A0 ;Pause         (length 16)
 .byt     $85 ;MUS note (C#3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $A0 ;Pause         (length 16)
 .byt     $85 ;MUS note (C#3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $E0 ;Pause         (length 64)
: ;at B49C
 .byt     $A0 ;Pause         (length 16)
 .byt     $85 ;MUS note (C#3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $A0 ;Pause         (length 16)
 .byt     $85 ;MUS note (C#3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $A0 ;Pause         (length 16)
 .byt     $85 ;MUS note (C#3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $A0 ;Pause         (length 16)
 .byt     $85 ;MUS note (C#3, length 8)
 .byt     $65 ;MUS note (C#3, length 4)
 .byt     $65 ;MUS note (C#3, length 4)
 .byt MUSCMD_Loop, 6 
 .word :-
 .byt     $A0 ;Pause         (length 16)
 .byt     $85 ;MUS note (C#3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $A0 ;Pause         (length 16)
 .byt     $85 ;MUS note (C#3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $E0 ;Pause         (length 64)
: ;at B4B4
 .byt $06,$C0 ;Pause         (length 48)
 .byt     $85 ;MUS note (C#3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt MUSCMD_Loop, 5 
 .word :-
 .byt MUSCMD_SetSpeed,          $08
 .byt $06,$C0 ;Pause         (length 48)
 .byt     $85 ;MUS note (C#3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt MUSCMD_EndOfData
MUS_Credits_vibratotable: ;at B4C3
 .byt $00, $E0, $80, $00 ;vibrato definition
 .byt $02, $22, $80, $00 ;vibrato definition
 .byt $01, $21, $80, $00 ;vibrato definition
 .byt $01, $62, $80, $00 ;vibrato definition
.endscope

MUS_Iceman: ;at B4D3
.scope
 .byt $0F ;priority. Lo=music priority, Hi=sfx priority
 .word MUS_Iceman_chn0                ; $B4DE ;
 .word MUS_Iceman_chn1                ; $B5C6 ;
 .word MUS_Iceman_chn2                ; $B6AD ;
 .word MUS_Iceman_chn3                ; $B763 ;
 .word MUS_Iceman_vibratotable        ; $B7A0 ;
MUS_Iceman_chn0: ;at B4DE
 .byt MUSCMD_SetSpeed,          $06
 .byt MUSCMD_SetDutyCycle,      $00
 .byt MUSCMD_SetVolumeAndEnv,   $3A
 .byt     $E0 ;Pause         (length 64)
: ;at B4E5
 .byt MUSCMD_SetNoteBaseOffset, 1*12+10
 .byt MUSCMD_SetVolumeCurve, $86,$20
 .byt     $7A ;MUS note (C-4, length 4)
 .byt     $7A ;MUS note (C-4, length 4)
 .byt     $80 ;Pause         (length 8)
 .byt     $A0 ;Pause         (length 16)
 .byt     $7A ;MUS note (C-4, length 4)
 .byt     $9A ;MUS note (C-4, length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $78 ;MUS note (A#3, length 4)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $B8 ;MUS note (A#3, length 16)
 .byt MUSCMD_SetVolumeCurve, $92,$20
 .byt MUSCMD_SetVibratoIndex,   $02
 .byt     $F8 ;MUS note (A#3, length 64)
 .byt MUSCMD_SetVolumeCurve, $86,$20
 .byt MUSCMD_SetVibratoIndex,   $00
 .byt     $7A ;MUS note (C-4, length 4)
 .byt     $7A ;MUS note (C-4, length 4)
 .byt     $80 ;Pause         (length 8)
 .byt     $A0 ;Pause         (length 16)
 .byt     $7A ;MUS note (C-4, length 4)
 .byt     $9A ;MUS note (C-4, length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $7D ;MUS note (D#4, length 4)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $BD ;MUS note (D#4, length 16)
 .byt MUSCMD_SetVolumeCurve, $92,$20
 .byt MUSCMD_SetVibratoIndex,   $02
 .byt     $FD ;MUS note (D#4, length 64)
 .byt MUSCMD_Loop, 1 
 .word :-
 .byt MUSCMD_SetDutyCycle,      $80
 .byt MUSCMD_SetVolumeAndEnv,   $38
 .byt MUSCMD_SetVolumeCurve, $FF,$40
 .byt     $80 ;Pause         (length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $89 ;MUS note (G-2, length 8)
 .byt     $69 ;MUS note (G-2, length 4)
 .byt $06,$8A ;MUS note (G#2, length 12)
 .byt     $80 ;Pause         (length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $8B ;MUS note (A-2, length 8)
 .byt     $6B ;MUS note (A-2, length 4)
 .byt $06,$8C ;MUS note (A#2, length 12)
 .byt     $60 ;Pause         (length 4)
 .byt $06,$8D ;MUS note (B-2, length 12)
 .byt     $8E ;MUS note (C-3, length 8)
 .byt     $8F ;MUS note (C#3, length 8)
 .byt     $90 ;MUS note (D-3, length 8)
 .byt     $91 ;MUS note (D#3, length 8)
 .byt     $72 ;MUS note (E-3, length 4)
 .byt $06,$93 ;MUS note (F-3, length 12)
: ;at B530
 .byt MUSCMD_SetDutyCycle,      $00
 .byt MUSCMD_SetNoteBaseOffset, 2*12+10
 .byt MUSCMD_SetVolumeAndEnv,   $3A
 .byt MUSCMD_SetVolumeCurve, $86,$20
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt     $95 ;MUS note (G-4, length 8)
 .byt     $60 ;Pause         (length 4)
 .byt     $75 ;MUS note (G-4, length 4)
 .byt     $75 ;MUS note (G-4, length 4)
 .byt $06,$93 ;MUS note (F-4, length 12)
 .byt     $95 ;MUS note (G-4, length 8)
 .byt     $92 ;MUS note (E-4, length 8)
 .byt     $95 ;MUS note (G-4, length 8)
 .byt     $9A ;MUS note (C-5, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $98 ;MUS note (A#4, length 8)
 .byt     $78 ;MUS note (A#4, length 4)
 .byt $06,$9A ;MUS note (C-5, length 12)
 .byt     $58 ;MUS note (A#4, length 2)
 .byt     $5A ;MUS note (C-5, length 2)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $5C ;MUS note (D-5, length 2)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $5C ;MUS note (D-5, length 2)
 .byt $30,$7C ;MUS note (D-5, length 4/1.5) (triplet)
 .byt $30,$B8 ;MUS note (A#4, length 16/1.5) (triplet)
 .byt $30,$B3 ;MUS note (F-4, length 16/1.5) (triplet)
 .byt     $80 ;Pause         (length 8)
 .byt     $95 ;MUS note (G-4, length 8)
 .byt     $75 ;MUS note (G-4, length 4)
 .byt $06,$93 ;MUS note (F-4, length 12)
 .byt     $95 ;MUS note (G-4, length 8)
 .byt     $92 ;MUS note (E-4, length 8)
 .byt     $95 ;MUS note (G-4, length 8)
 .byt     $9A ;MUS note (C-5, length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $B8 ;MUS note (A#4, length 16)
 .byt MUSCMD_SetVibratoIndex,   $02
 .byt $06,$D8 ;MUS note (A#4, length 48)
 .byt MUSCMD_SetVolumeCurve, $D0,$20
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt     $80 ;Pause         (length 8)
 .byt     $98 ;MUS note (A#4, length 8)
 .byt     $78 ;MUS note (A#4, length 4)
 .byt $06,$96 ;MUS note (G#4, length 12)
 .byt     $98 ;MUS note (A#4, length 8)
 .byt     $95 ;MUS note (G-4, length 8)
 .byt     $98 ;MUS note (A#4, length 8)
 .byt     $9D ;MUS note (D#5, length 8)
 .byt MUSCMD_SetNoteBaseOffset, 3*12+10
 .byt     $80 ;Pause         (length 8)
 .byt     $8F ;MUS note (C#5, length 8)
 .byt     $6F ;MUS note (C#5, length 4)
 .byt $06,$91 ;MUS note (D#5, length 12)
 .byt     $4F ;MUS note (C#5, length 2)
 .byt     $50 ;MUS note (D-5, length 2)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $53 ;MUS note (F-5, length 2)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $53 ;MUS note (F-5, length 2)
 .byt $30,$73 ;MUS note (F-5, length 4/1.5) (triplet)
 .byt $30,$AF ;MUS note (C#5, length 16/1.5) (triplet)
 .byt $30,$AA ;MUS note (G#4, length 16/1.5) (triplet)
 .byt MUSCMD_SetNoteBaseOffset, 2*12+10
 .byt     $80 ;Pause         (length 8)
 .byt     $98 ;MUS note (A#4, length 8)
 .byt     $78 ;MUS note (A#4, length 4)
 .byt $06,$96 ;MUS note (G#4, length 12)
 .byt     $98 ;MUS note (A#4, length 8)
 .byt     $95 ;MUS note (G-4, length 8)
 .byt     $98 ;MUS note (A#4, length 8)
 .byt     $9D ;MUS note (D#5, length 8)
 .byt MUSCMD_SetNoteBaseOffset, 3*12+10
 .byt     $D3 ;MUS note (F-5, length 32)
 .byt     $D5 ;MUS note (G-5, length 32)
 .byt MUSCMD_SetNoteBaseOffset, 1*12+10
: ;at B597
 .byt MUSCMD_SetVibratoIndex,   $00
 .byt MUSCMD_SetDutyCycle,      $00
 .byt MUSCMD_SetVolumeAndEnv,   $38
 .byt MUSCMD_SetVolumeCurve, $86,$20
 .byt     $7A ;MUS note (C-4, length 4)
 .byt     $7A ;MUS note (C-4, length 4)
 .byt     $80 ;Pause         (length 8)
 .byt     $A0 ;Pause         (length 16)
 .byt     $7A ;MUS note (C-4, length 4)
 .byt     $9A ;MUS note (C-4, length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $78 ;MUS note (A#3, length 4)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $B8 ;MUS note (A#3, length 16)
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt     $F8 ;MUS note (A#3, length 64)
 .byt MUSCMD_SetVibratoIndex,   $00
 .byt     $7A ;MUS note (C-4, length 4)
 .byt     $7A ;MUS note (C-4, length 4)
 .byt     $80 ;Pause         (length 8)
 .byt     $A0 ;Pause         (length 16)
 .byt     $7A ;MUS note (C-4, length 4)
 .byt     $9A ;MUS note (C-4, length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $7D ;MUS note (D#4, length 4)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $BD ;MUS note (D#4, length 16)
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt     $FD ;MUS note (D#4, length 64)
 .byt MUSCMD_SetVibratoIndex,   $00
 .byt MUSCMD_Loop, 1 
 .word :-
 .byt MUSCMD_Loop, 0 
 .word :--
MUS_Iceman_chn1: ;at B5C6
 .byt MUSCMD_SetSpeed,          $06
 .byt MUSCMD_SetNoteBaseOffset, 1*12+10
 .byt MUSCMD_SetVolumeAndEnv,   $38
 .byt MUSCMD_SetDutyCycle,      $40
 .byt MUSCMD_SetVolumeCurve, $8F,$10
 .byt     $E0 ;Pause         (length 64)
: ;at B5D2
 .byt     $6E ;MUS note (C-3, length 4)
 .byt     $72 ;MUS note (E-3, length 4)
 .byt     $73 ;MUS note (F-3, length 4)
 .byt     $74 ;MUS note (F#3, length 4)
 .byt     $75 ;MUS note (G-3, length 4)
 .byt     $74 ;MUS note (F#3, length 4)
 .byt     $73 ;MUS note (F-3, length 4)
 .byt     $72 ;MUS note (E-3, length 4)
 .byt     $6E ;MUS note (C-3, length 4)
 .byt     $72 ;MUS note (E-3, length 4)
 .byt     $73 ;MUS note (F-3, length 4)
 .byt     $74 ;MUS note (F#3, length 4)
 .byt     $75 ;MUS note (G-3, length 4)
 .byt     $74 ;MUS note (F#3, length 4)
 .byt     $73 ;MUS note (F-3, length 4)
 .byt     $72 ;MUS note (E-3, length 4)
 .byt     $6C ;MUS note (A#2, length 4)
 .byt     $70 ;MUS note (D-3, length 4)
 .byt     $71 ;MUS note (D#3, length 4)
 .byt     $72 ;MUS note (E-3, length 4)
 .byt     $73 ;MUS note (F-3, length 4)
 .byt     $72 ;MUS note (E-3, length 4)
 .byt     $71 ;MUS note (D#3, length 4)
 .byt     $70 ;MUS note (D-3, length 4)
 .byt     $6C ;MUS note (A#2, length 4)
 .byt     $70 ;MUS note (D-3, length 4)
 .byt     $71 ;MUS note (D#3, length 4)
 .byt     $72 ;MUS note (E-3, length 4)
 .byt     $73 ;MUS note (F-3, length 4)
 .byt     $72 ;MUS note (E-3, length 4)
 .byt     $71 ;MUS note (D#3, length 4)
 .byt     $70 ;MUS note (D-3, length 4)
 .byt MUSCMD_Loop, 3 
 .word :-
 .byt MUSCMD_SetDutyCycle,      $C0
 .byt MUSCMD_SetVolumeAndEnv,   $38
 .byt MUSCMD_SetVolumeCurve, $FF,$40
 .byt     $80 ;Pause         (length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $92 ;MUS note (E-3, length 8)
 .byt     $72 ;MUS note (E-3, length 4)
 .byt $06,$93 ;MUS note (F-3, length 12)
 .byt     $80 ;Pause         (length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $94 ;MUS note (F#3, length 8)
 .byt     $74 ;MUS note (F#3, length 4)
 .byt $06,$95 ;MUS note (G-3, length 12)
 .byt     $60 ;Pause         (length 4)
 .byt $06,$96 ;MUS note (G#3, length 12)
 .byt     $97 ;MUS note (A-3, length 8)
 .byt     $98 ;MUS note (A#3, length 8)
 .byt     $99 ;MUS note (B-3, length 8)
 .byt     $9A ;MUS note (C-4, length 8)
 .byt     $7B ;MUS note (C#4, length 4)
 .byt $06,$9C ;MUS note (D-4, length 12)
: ;at B613
 .byt MUSCMD_SetDutyCycle,      $00
 .byt MUSCMD_SetNoteBaseOffset, 2*12+10
 .byt MUSCMD_SetVolumeAndEnv,   $35
 .byt MUSCMD_SetVolumeCurve, $86,$20
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt     $80 ;Pause         (length 8)
 .byt     $95 ;MUS note (G-4, length 8)
 .byt     $60 ;Pause         (length 4)
 .byt     $75 ;MUS note (G-4, length 4)
 .byt     $75 ;MUS note (G-4, length 4)
 .byt $06,$93 ;MUS note (F-4, length 12)
 .byt     $95 ;MUS note (G-4, length 8)
 .byt     $92 ;MUS note (E-4, length 8)
 .byt     $95 ;MUS note (G-4, length 8)
 .byt     $9A ;MUS note (C-5, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $98 ;MUS note (A#4, length 8)
 .byt     $78 ;MUS note (A#4, length 4)
 .byt $06,$9A ;MUS note (C-5, length 12)
 .byt     $58 ;MUS note (A#4, length 2)
 .byt     $5A ;MUS note (C-5, length 2)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $5C ;MUS note (D-5, length 2)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $5C ;MUS note (D-5, length 2)
 .byt $30,$7C ;MUS note (D-5, length 4/1.5) (triplet)
 .byt $30,$B8 ;MUS note (A#4, length 16/1.5) (triplet)
 .byt $30,$B3 ;MUS note (F-4, length 16/1.5) (triplet)
 .byt     $80 ;Pause         (length 8)
 .byt     $95 ;MUS note (G-4, length 8)
 .byt     $75 ;MUS note (G-4, length 4)
 .byt $06,$93 ;MUS note (F-4, length 12)
 .byt     $95 ;MUS note (G-4, length 8)
 .byt     $92 ;MUS note (E-4, length 8)
 .byt     $95 ;MUS note (G-4, length 8)
 .byt     $9A ;MUS note (C-5, length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $B8 ;MUS note (A#4, length 16)
 .byt MUSCMD_SetVibratoIndex,   $02
 .byt $06,$D8 ;MUS note (A#4, length 48)
 .byt MUSCMD_SetVolumeCurve, $D0,$20
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt     $80 ;Pause         (length 8)
 .byt     $98 ;MUS note (A#4, length 8)
 .byt     $78 ;MUS note (A#4, length 4)
 .byt $06,$96 ;MUS note (G#4, length 12)
 .byt     $98 ;MUS note (A#4, length 8)
 .byt     $95 ;MUS note (G-4, length 8)
 .byt     $98 ;MUS note (A#4, length 8)
 .byt     $9D ;MUS note (D#5, length 8)
 .byt MUSCMD_SetNoteBaseOffset, 3*12+10
 .byt     $80 ;Pause         (length 8)
 .byt     $8F ;MUS note (C#5, length 8)
 .byt     $6F ;MUS note (C#5, length 4)
 .byt $06,$91 ;MUS note (D#5, length 12)
 .byt     $4F ;MUS note (C#5, length 2)
 .byt     $50 ;MUS note (D-5, length 2)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $53 ;MUS note (F-5, length 2)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $53 ;MUS note (F-5, length 2)
 .byt $30,$73 ;MUS note (F-5, length 4/1.5) (triplet)
 .byt $30,$AF ;MUS note (C#5, length 16/1.5) (triplet)
 .byt $30,$AA ;MUS note (G#4, length 16/1.5) (triplet)
 .byt MUSCMD_SetNoteBaseOffset, 2*12+10
 .byt     $80 ;Pause         (length 8)
 .byt     $98 ;MUS note (A#4, length 8)
 .byt     $78 ;MUS note (A#4, length 4)
 .byt $06,$96 ;MUS note (G#4, length 12)
 .byt     $98 ;MUS note (A#4, length 8)
 .byt     $95 ;MUS note (G-4, length 8)
 .byt     $98 ;MUS note (A#4, length 8)
 .byt     $9D ;MUS note (D#5, length 8)
 .byt MUSCMD_SetNoteBaseOffset, 3*12+10
 .byt $06,$B3 ;MUS note (F-5, length 24)
 .byt MUSCMD_SetNoteBaseOffset, 2*12+10
 .byt     $69 ;MUS note (G-3, length 4)
 .byt     $6B ;MUS note (A-3, length 4)
 .byt     $6D ;MUS note (B-3, length 4)
 .byt     $70 ;MUS note (D-4, length 4)
 .byt     $75 ;MUS note (G-4, length 4)
 .byt     $77 ;MUS note (A-4, length 4)
 .byt     $79 ;MUS note (B-4, length 4)
 .byt     $7C ;MUS note (D-5, length 4)
: ;at B683
 .byt MUSCMD_SetNoteBaseOffset, 1*12+10
 .byt MUSCMD_SetVibratoIndex,   $00
 .byt MUSCMD_SetDutyCycle,      $00
 .byt MUSCMD_SetVolumeAndEnv,   $35
 .byt MUSCMD_SetVolumeCurve, $86,$20
 .byt     $75 ;MUS note (G-3, length 4)
 .byt     $75 ;MUS note (G-3, length 4)
 .byt     $80 ;Pause         (length 8)
 .byt     $A0 ;Pause         (length 16)
 .byt     $75 ;MUS note (G-3, length 4)
 .byt     $95 ;MUS note (G-3, length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $73 ;MUS note (F-3, length 4)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $B3 ;MUS note (F-3, length 16)
 .byt     $F3 ;MUS note (F-3, length 64)
 .byt     $75 ;MUS note (G-3, length 4)
 .byt     $75 ;MUS note (G-3, length 4)
 .byt     $80 ;Pause         (length 8)
 .byt     $A0 ;Pause         (length 16)
 .byt     $75 ;MUS note (G-3, length 4)
 .byt     $95 ;MUS note (G-3, length 8)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $78 ;MUS note (A#3, length 4)
 .byt MUSCMD_SetNoteDelay     + $1
 .byt     $B8 ;MUS note (A#3, length 16)
 .byt     $F8 ;MUS note (A#3, length 64)
 .byt MUSCMD_Loop, 1 
 .word :-
 .byt MUSCMD_Loop, 0 
 .word :--
 .byt MUSCMD_EndOfData
MUS_Iceman_chn2: ;at B6AD
 .byt MUSCMD_SetSpeed,          $06
 .byt MUSCMD_SetVolumeAndEnv,   $09
 .byt MUSCMD_SetVibratoCtrl,    $0F
 .byt MUSCMD_SetNoteBaseOffset, 2*12+11
 .byt     $74 ;MUS note (G-4, length 4)
 .byt     $74 ;MUS note (G-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $6D ;MUS note (C-4, length 4)
 .byt     $6D ;MUS note (C-4, length 4)
 .byt     $60 ;Pause         (length 4)
 .byt     $68 ;MUS note (G-3, length 4)
 .byt     $68 ;MUS note (G-3, length 4)
 .byt     $66 ;MUS note (F-3, length 4)
 .byt     $66 ;MUS note (F-3, length 4)
 .byt     $66 ;MUS note (F-3, length 4)
 .byt     $66 ;MUS note (F-3, length 4)
 .byt     $61 ;MUS note (C-3, length 4)
 .byt     $61 ;MUS note (C-3, length 4)
 .byt     $61 ;MUS note (C-3, length 4)
 .byt     $61 ;MUS note (C-3, length 4)
: ;at B6C5
 .byt MUSCMD_SetNoteBaseOffset, 1*12+10
 .byt MUSCMD_SetVibratoCtrl,    $00
 .byt MUSCMD_SetVolumeAndEnv,   $30
 .byt     $8E ;MUS note (C-3, length 8)
 .byt     $8E ;MUS note (C-3, length 8)
 .byt     $60 ;Pause         (length 4)
 .byt     $8E ;MUS note (C-3, length 8)
 .byt     $6E ;MUS note (C-3, length 4)
 .byt     $AE ;MUS note (C-3, length 16)
 .byt     $A0 ;Pause         (length 16)
 .byt     $8C ;MUS note (A#2, length 8)
 .byt     $8C ;MUS note (A#2, length 8)
 .byt     $60 ;Pause         (length 4)
 .byt     $8C ;MUS note (A#2, length 8)
 .byt     $6C ;MUS note (A#2, length 4)
 .byt     $AC ;MUS note (A#2, length 16)
 .byt     $A0 ;Pause         (length 16)
 .byt MUSCMD_Loop, 3 
 .word :-
 .byt MUSCMD_SetVolumeAndEnv,   $81
 .byt $06,$AE ;MUS note (C-3, length 24)
 .byt     $89 ;MUS note (G-2, length 8)
 .byt $06,$AE ;MUS note (C-3, length 24)
 .byt     $89 ;MUS note (G-2, length 8)
 .byt     $AE ;MUS note (C-3, length 16)
 .byt MUSCMD_SetVolumeAndEnv,   $09
 .byt MUSCMD_SetNoteBaseOffset, 2*12+11
 .byt MUSCMD_SetVibratoCtrl,    $0F
 .byt     $72 ;MUS note (F-4, length 4)
 .byt     $72 ;MUS note (F-4, length 4)
 .byt     $72 ;MUS note (F-4, length 4)
 .byt     $72 ;MUS note (F-4, length 4)
 .byt     $6D ;MUS note (C-4, length 4)
 .byt     $6D ;MUS note (C-4, length 4)
 .byt     $6D ;MUS note (C-4, length 4)
 .byt     $6D ;MUS note (C-4, length 4)
 .byt     $68 ;MUS note (G-3, length 4)
 .byt     $68 ;MUS note (G-3, length 4)
 .byt     $68 ;MUS note (G-3, length 4)
 .byt     $68 ;MUS note (G-3, length 4)
: ;at B6F8
 .byt MUSCMD_SetVibratoCtrl,    $00
 .byt MUSCMD_SetVolumeAndEnv,   $30
 .byt MUSCMD_SetNoteBaseOffset, 1*12+10
 .byt     $8E ;MUS note (C-3, length 8)
 .byt     $8E ;MUS note (C-3, length 8)
 .byt     $60 ;Pause         (length 4)
 .byt     $8E ;MUS note (C-3, length 8)
 .byt     $6E ;MUS note (C-3, length 4)
 .byt     $AE ;MUS note (C-3, length 16)
 .byt     $A0 ;Pause         (length 16)
 .byt     $8C ;MUS note (A#2, length 8)
 .byt     $8C ;MUS note (A#2, length 8)
 .byt     $60 ;Pause         (length 4)
 .byt     $8C ;MUS note (A#2, length 8)
 .byt     $6C ;MUS note (A#2, length 4)
 .byt     $AC ;MUS note (A#2, length 16)
 .byt     $A0 ;Pause         (length 16)
 .byt MUSCMD_Loop, 1 
 .word :-
 .byt MUSCMD_SetNoteBaseOffset, 1*12+10
 .byt     $91 ;MUS note (D#3, length 8)
 .byt     $91 ;MUS note (D#3, length 8)
 .byt     $60 ;Pause         (length 4)
 .byt     $91 ;MUS note (D#3, length 8)
 .byt     $71 ;MUS note (D#3, length 4)
 .byt     $B1 ;MUS note (D#3, length 16)
 .byt     $A0 ;Pause         (length 16)
 .byt     $8F ;MUS note (C#3, length 8)
 .byt     $8F ;MUS note (C#3, length 8)
 .byt     $60 ;Pause         (length 4)
 .byt     $8F ;MUS note (C#3, length 8)
 .byt     $6F ;MUS note (C#3, length 4)
 .byt     $AF ;MUS note (C#3, length 16)
 .byt     $A0 ;Pause         (length 16)
 .byt     $91 ;MUS note (D#3, length 8)
 .byt     $91 ;MUS note (D#3, length 8)
 .byt     $60 ;Pause         (length 4)
 .byt     $91 ;MUS note (D#3, length 8)
 .byt     $71 ;MUS note (D#3, length 4)
 .byt     $B1 ;MUS note (D#3, length 16)
 .byt     $A0 ;Pause         (length 16)
 .byt     $8F ;MUS note (C#3, length 8)
 .byt     $8F ;MUS note (C#3, length 8)
 .byt     $60 ;Pause         (length 4)
 .byt     $8F ;MUS note (C#3, length 8)
 .byt     $6F ;MUS note (C#3, length 4)
 .byt     $A8 ;MUS note (F#2, length 16)
 .byt     $A0 ;Pause         (length 16)
: ;at B72E
 .byt     $8E ;MUS note (C-3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $8E ;MUS note (C-3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $8E ;MUS note (C-3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $8E ;MUS note (C-3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $8C ;MUS note (A#2, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $8C ;MUS note (A#2, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $8C ;MUS note (A#2, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $8C ;MUS note (A#2, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt MUSCMD_Loop, 2 
 .word :-
 .byt     $8E ;MUS note (C-3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $8E ;MUS note (C-3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $8E ;MUS note (C-3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $8E ;MUS note (C-3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $8C ;MUS note (A#2, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt MUSCMD_SetVolumeAndEnv,   $09
 .byt MUSCMD_SetNoteBaseOffset, 2*12+11
 .byt MUSCMD_SetVibratoCtrl,    $0F
 .byt     $72 ;MUS note (F-4, length 4)
 .byt     $72 ;MUS note (F-4, length 4)
 .byt     $72 ;MUS note (F-4, length 4)
 .byt     $72 ;MUS note (F-4, length 4)
 .byt     $6D ;MUS note (C-4, length 4)
 .byt     $6D ;MUS note (C-4, length 4)
 .byt     $6D ;MUS note (C-4, length 4)
 .byt     $6D ;MUS note (C-4, length 4)
 .byt     $68 ;MUS note (G-3, length 4)
 .byt     $68 ;MUS note (G-3, length 4)
 .byt     $68 ;MUS note (G-3, length 4)
 .byt     $68 ;MUS note (G-3, length 4)
 .byt MUSCMD_Loop, 0 
 .word :--
 .byt MUSCMD_EndOfData
MUS_Iceman_chn3: ;at B763
 .byt MUSCMD_SetSpeed,          $06
 .byt MUSCMD_SetVolumeAndEnv,   $39
 .byt MUSCMD_SetVolumeCurve, $81,$10
 .byt     $E0 ;Pause         (length 64)
: ;at B76B
 .byt $06,$C0 ;Pause         (length 48)
 .byt     $85 ;MUS note (E-3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt $06,$C0 ;Pause         (length 48)
 .byt     $65 ;MUS note (E-3, length 4)
 .byt     $65 ;MUS note (E-3, length 4)
 .byt     $65 ;MUS note (E-3, length 4)
 .byt     $65 ;MUS note (E-3, length 4)
 .byt MUSCMD_Loop, 3 
 .word :-
 .byt     $A0 ;Pause         (length 16)
 .byt     $85 ;MUS note (E-3, length 8)
 .byt     $A0 ;Pause         (length 16)
 .byt     $85 ;MUS note (E-3, length 8)
 .byt     $60 ;Pause         (length 4)
 .byt     $85 ;MUS note (E-3, length 8)
 .byt     $65 ;MUS note (E-3, length 4)
 .byt     $A0 ;Pause         (length 16)
 .byt     $65 ;MUS note (E-3, length 4)
 .byt     $65 ;MUS note (E-3, length 4)
 .byt     $65 ;MUS note (E-3, length 4)
 .byt     $65 ;MUS note (E-3, length 4)
 .byt     $65 ;MUS note (E-3, length 4)
 .byt     $65 ;MUS note (E-3, length 4)
 .byt     $65 ;MUS note (E-3, length 4)
 .byt     $65 ;MUS note (E-3, length 4)
 .byt     $65 ;MUS note (E-3, length 4)
 .byt     $65 ;MUS note (E-3, length 4)
 .byt     $65 ;MUS note (E-3, length 4)
 .byt     $65 ;MUS note (E-3, length 4)
: ;at B78D
 .byt MUSCMD_SetVolumeAndEnv,   $37
 .byt MUSCMD_SetVolumeCurve, $82,$20
 .byt $06,$C0 ;Pause         (length 48)
 .byt     $85 ;MUS note (E-3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt $06,$C0 ;Pause         (length 48)
 .byt     $65 ;MUS note (E-3, length 4)
 .byt     $65 ;MUS note (E-3, length 4)
 .byt     $65 ;MUS note (E-3, length 4)
 .byt     $65 ;MUS note (E-3, length 4)
 .byt MUSCMD_Loop, 0 
 .word :-
MUS_Iceman_vibratotable: ;at B7A0
 .byt $00, $E0, $80, $00 ;vibrato definition
 .byt $02, $22, $80, $00 ;vibrato definition
 .byt $01, $41, $80, $00 ;vibrato definition
.endscope

MUS_WilyStage3and4: ;at B7AC
.scope
 .byt $0F ;priority. Lo=music priority, Hi=sfx priority
 .word MUS_WilyStage3and4_chn0        ; $B7B7 ;
 .word MUS_WilyStage3and4_chn1        ; $B7E4 ;
 .word MUS_WilyStage3and4_chn2        ; $B811 ;
 .word MUS_WilyStage3and4_chn3        ; $B873 ;
 .word MUS_WilyStage3and4_vibratotable ; $B890 ;
MUS_WilyStage3and4_chn0: ;at B7B7
 .byt MUSCMD_SetSpeed,          $07
: ;at B7B9
 .byt MUSCMD_SetVolumeAndEnv,   $37
 .byt MUSCMD_SetDutyCycle,      $40
 .byt MUSCMD_SetVolumeCurve, $FF,$A0
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt MUSCMD_SetNoteBaseOffset, 2*12+2
 .byt     $E5 ;MUS note (G-2, length 64)
 .byt     $E6 ;MUS note (G#2, length 64)
 .byt     $E7 ;MUS note (A-2, length 64)
 .byt     $C8 ;MUS note (A#2, length 32)
 .byt $30,$8C ;MUS note (D-3, length 8/1.5) (triplet)
 .byt $30,$91 ;MUS note (G-3, length 8/1.5) (triplet)
 .byt $30,$92 ;MUS note (G#3, length 8/1.5) (triplet)
 .byt $30,$94 ;MUS note (A#3, length 8/1.5) (triplet)
 .byt $30,$98 ;MUS note (D-4, length 8/1.5) (triplet)
 .byt $30,$9E ;MUS note (G#4, length 8/1.5) (triplet)
 .byt     $F1 ;MUS note (G-3, length 64)
 .byt     $F2 ;MUS note (G#3, length 64)
 .byt     $F3 ;MUS note (A-3, length 64)
 .byt     $F4 ;MUS note (A#3, length 64)
 .byt     $D1 ;MUS note (G-3, length 32)
 .byt     $CF ;MUS note (F-3, length 32)
 .byt     $D4 ;MUS note (A#3, length 32)
 .byt     $D2 ;MUS note (G#3, length 32)
 .byt     $C5 ;MUS note (G-2, length 32)
 .byt     $C3 ;MUS note (F-2, length 32)
 .byt     $C8 ;MUS note (A#2, length 32)
 .byt     $C6 ;MUS note (G#2, length 32)
 .byt MUSCMD_Loop, 0 
 .word :-
MUS_WilyStage3and4_chn1: ;at B7E4
 .byt MUSCMD_SetSpeed,          $07
: ;at B7E6
 .byt MUSCMD_SetVolumeAndEnv,   $36
 .byt MUSCMD_SetDutyCycle,      $40
 .byt MUSCMD_SetVolumeCurve, $FF,$A0
 .byt MUSCMD_SetVibratoIndex,   $01
 .byt MUSCMD_SetNoteBaseOffset, 2*12+2
 .byt     $EC ;MUS note (D-3, length 64)
 .byt     $ED ;MUS note (D#3, length 64)
 .byt     $EE ;MUS note (E-3, length 64)
 .byt     $CF ;MUS note (F-3, length 32)
 .byt $30,$88 ;MUS note (A#2, length 8/1.5) (triplet)
 .byt $30,$8C ;MUS note (D-3, length 8/1.5) (triplet)
 .byt $30,$8F ;MUS note (F-3, length 8/1.5) (triplet)
 .byt $30,$91 ;MUS note (G-3, length 8/1.5) (triplet)
 .byt $30,$94 ;MUS note (A#3, length 8/1.5) (triplet)
 .byt $30,$99 ;MUS note (D#4, length 8/1.5) (triplet)
 .byt     $F8 ;MUS note (D-4, length 64)
 .byt     $F9 ;MUS note (D#4, length 64)
 .byt     $FA ;MUS note (E-4, length 64)
 .byt     $FB ;MUS note (F-4, length 64)
 .byt     $D8 ;MUS note (D-4, length 32)
 .byt     $D6 ;MUS note (C-4, length 32)
 .byt     $DB ;MUS note (F-4, length 32)
 .byt     $D9 ;MUS note (D#4, length 32)
 .byt     $CC ;MUS note (D-3, length 32)
 .byt     $CA ;MUS note (C-3, length 32)
 .byt     $CF ;MUS note (F-3, length 32)
 .byt     $CD ;MUS note (D#3, length 32)
 .byt MUSCMD_Loop, 0 
 .word :-
MUS_WilyStage3and4_chn2: ;at B811
 .byt MUSCMD_SetSpeed,          $07
: ;at B813
 .byt MUSCMD_SetVolumeAndEnv,   $71
 .byt MUSCMD_SetNoteBaseOffset, 2*12+2
 .byt     $65 ;MUS note (G-2, length 4)
 .byt     $71 ;MUS note (G-3, length 4)
 .byt     $6C ;MUS note (D-3, length 4)
 .byt     $6F ;MUS note (F-3, length 4)
 .byt MUSCMD_Loop, 31 
 .word :-
: ;at B81F
 .byt     $65 ;MUS note (G-2, length 4)
 .byt     $71 ;MUS note (G-3, length 4)
 .byt     $65 ;MUS note (G-2, length 4)
 .byt     $71 ;MUS note (G-3, length 4)
 .byt MUSCMD_Loop, 1 
 .word :-
: ;at B827
 .byt     $63 ;MUS note (F-2, length 4)
 .byt     $6F ;MUS note (F-3, length 4)
 .byt     $63 ;MUS note (F-2, length 4)
 .byt     $6F ;MUS note (F-3, length 4)
 .byt MUSCMD_Loop, 1 
 .word :-
: ;at B82F
 .byt     $68 ;MUS note (A#2, length 4)
 .byt     $6D ;MUS note (D#3, length 4)
 .byt     $68 ;MUS note (A#2, length 4)
 .byt     $6D ;MUS note (D#3, length 4)
 .byt MUSCMD_Loop, 1 
 .word :-
: ;at B837
 .byt     $66 ;MUS note (G#2, length 4)
 .byt     $72 ;MUS note (G#3, length 4)
 .byt     $66 ;MUS note (G#2, length 4)
 .byt     $72 ;MUS note (G#3, length 4)
 .byt MUSCMD_Loop, 1 
 .word :-
: ;at B83F
 .byt     $65 ;MUS note (G-2, length 4)
 .byt     $71 ;MUS note (G-3, length 4)
 .byt     $65 ;MUS note (G-2, length 4)
 .byt     $71 ;MUS note (G-3, length 4)
 .byt MUSCMD_Loop, 1 
 .word :-
: ;at B847
 .byt     $63 ;MUS note (F-2, length 4)
 .byt     $6F ;MUS note (F-3, length 4)
 .byt     $63 ;MUS note (F-2, length 4)
 .byt     $6F ;MUS note (F-3, length 4)
 .byt MUSCMD_Loop, 1 
 .word :-
: ;at B84F
 .byt     $68 ;MUS note (A#2, length 4)
 .byt     $6D ;MUS note (D#3, length 4)
 .byt     $68 ;MUS note (A#2, length 4)
 .byt     $6D ;MUS note (D#3, length 4)
 .byt MUSCMD_Loop, 1 
 .word :-
 .byt MUSCMD_SetVibratoCtrl,    $0F
 .byt MUSCMD_SetVolumeAndEnv,   $09
 .byt MUSCMD_SetNoteBaseOffset, 2*12+9
 .byt $30,$92 ;MUS note (D#4, length 8/1.5) (triplet)
 .byt $30,$92 ;MUS note (D#4, length 8/1.5) (triplet)
 .byt $30,$92 ;MUS note (D#4, length 8/1.5) (triplet)
 .byt $30,$8D ;MUS note (A#3, length 8/1.5) (triplet)
 .byt $30,$8D ;MUS note (A#3, length 8/1.5) (triplet)
 .byt $30,$8D ;MUS note (A#3, length 8/1.5) (triplet)
 .byt MUSCMD_SetVibratoCtrl,    $00
 .byt MUSCMD_SetVolumeAndEnv,   $7F
 .byt MUSCMD_SetNoteBaseOffset, 2*12+8
 .byt MUSCMD_Loop, 0 
 .word :--------
: ;at B873
MUS_WilyStage3and4_chn3: ;at B873
 .byt MUSCMD_SetSpeed,          $07
 .byt MUSCMD_SetVolumeAndEnv,   $36
 .byt MUSCMD_SetVolumeCurve, $82,$20
 .byt     $80 ;Pause         (length 8)
 .byt     $85 ;MUS note (C#3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $85 ;MUS note (C#3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $85 ;MUS note (C#3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $65 ;MUS note (C#3, length 4)
 .byt     $65 ;MUS note (C#3, length 4)
 .byt     $80 ;Pause         (length 8)
 .byt     $85 ;MUS note (C#3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $85 ;MUS note (C#3, length 8)
 .byt     $80 ;Pause         (length 8)
 .byt     $85 ;MUS note (C#3, length 8)
 .byt     $60 ;Pause         (length 4)
 .byt     $85 ;MUS note (C#3, length 8)
 .byt     $65 ;MUS note (C#3, length 4)
 .byt MUSCMD_Loop, 0 
 .word :-
MUS_WilyStage3and4_vibratotable: ;at B890
 .byt $00, $E0, $80, $00 ;vibrato definition
 .byt $02, $22, $80, $00 ;vibrato definition
.endscope

SFX_WilySaucer: ;at B898
.scope
 .byt $D0 ;priority. Lo=music priority, Hi=sfx priority
 .byt $03 ;channel usage
: ;at B89A
 .byt SFXCMD_SetDutyCycle,      $01*64
 .byt SFXCMD_SetVibratoCtrl,    $FF
 .byt SFXCMD_SetVibratoBuffer,  $00, $22, $80, $00
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_Exec_80,            $43
 .byt SFXCMD_SetFrameWaitCount, $10
 .byt SFXCMD_SetDutyCycle,      $01*64
 .byt SFXCMD_SetVibratoCtrl,    $FF
 .byt SFXCMD_SetVibratoBuffer,  $00, $22, $80, $00
 .byt SFXCMD_SetVolumeAndEnv,   $3A
 .byt SFXCMD_Exec_80,            $32
 .byt SFXCMD_Loop, 35 
 .word :-
 .byt SFXCMD_EndOfData
.endscope

SFX_Bomb: ;at B8BB
.scope
 .byt $D0 ;priority. Lo=music priority, Hi=sfx priority
 .byt $03 ;channel usage
 .byt SFXCMD_SetDutyCycle,      $02*64
 .byt SFXCMD_SetVibratoCtrl,    $32
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_Exec_80,            $43
 .byt SFXCMD_SetFrameWaitCount, $15
 .byt SFXCMD_SetDutyCycle,      $02*64
 .byt SFXCMD_SetVibratoCtrl,    $32
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_Exec_80,            $C9
 .byt SFXCMD_SetDutyCycle,      $03*64
 .byt SFXCMD_SetVibratoCtrl,    $2F
 .byt SFXCMD_SetVolumeAndEnv,   $39
 .byt SFXCMD_Exec_80,            $86
 .byt SFXCMD_SetFrameWaitCount, $07
 .byt SFXCMD_SetDutyCycle,      $03*64
 .byt SFXCMD_SetVibratoCtrl,    $2F
 .byt SFXCMD_SetVolumeAndEnv,   $39
 .byt SFXCMD_Exec_80,            $64
 .byt SFXCMD_EndOfData
.endscope

SFX_Electricity: ;at B8E2
.scope
 .byt $A0 ;priority. Lo=music priority, Hi=sfx priority
 .byt $0A ;channel usage
 .byt SFXCMD_SetDutyCycle,      $01*64
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_SetVibratoBuffer,  $01, $E1, $87, $01
 .byt SFXCMD_SetNotePeriod    + $1,$2E ;hi,lo (F#3)
 .byt SFXCMD_SetDutyCycle,      $02*64
 .byt SFXCMD_SetVibratoBuffer,  $01, $E1, $87, $01
 .byt SFXCMD_Exec_80,            $05
 .byt SFXCMD_SetFrameWaitCount, $18
 .byt SFXCMD_SetDutyCycle,      $01*64
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_SetVibratoBuffer,  $01, $E1, $87, $01
 .byt SFXCMD_Exec_80,            $FE
 .byt SFXCMD_SetDutyCycle,      $02*64
 .byt SFXCMD_SetVibratoBuffer,  $01, $E1, $87, $01
 .byt SFXCMD_Exec_80,            $04
 .byt SFXCMD_EndOfData
.endscope

SFX_Megablaster: ;at B90F
.scope
 .byt $B0 ;priority. Lo=music priority, Hi=sfx priority
 .byt $02 ;channel usage
 .byt SFXCMD_SetFrameWaitCount, $10
 .byt SFXCMD_SetDutyCycle,      $01*64
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_SetVibratoBuffer,  $02, $44, $80, $00
 .byt SFXCMD_SetVibratoCtrl,    $E0
 .byt SFXCMD_SetNotePeriod    + $1,$0D ;hi,lo (G#3)
 .byt SFXCMD_EndOfData
.endscope

SFX_EnemyBullet: ;at B921
.scope
 .byt $A0 ;priority. Lo=music priority, Hi=sfx priority
 .byt $06 ;channel usage
 .byt SFXCMD_SetDutyCycle,      $01*64
 .byt SFXCMD_SetVolumeAndEnv,   $3E
 .byt SFXCMD_SetVibratoCtrl,    $0F
 .byt SFXCMD_Exec_80,            $64
 .byt SFXCMD_SetFrameWaitCount, $08
 .byt SFXCMD_SetVolumeAndEnv,   $08
 .byt SFXCMD_SetVibratoBuffer,  $01, $C4, $82, $07
 .byt SFXCMD_SetVibratoCtrl,    $0F
 .byt SFXCMD_Exec_80,            $64
 .byt SFXCMD_EndOfData
.endscope

SFX_MegamanHit: ;at B939
.scope
 .byt $F0 ;priority. Lo=music priority, Hi=sfx priority
 .byt $0E ;channel usage
 .byt SFXCMD_SetDutyCycle,      $03*64
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_SetVibratoBuffer,  $02, $A3, $80, $07
 .byt SFXCMD_SetVibratoCtrl,    $5F
 .byt SFXCMD_Exec_80,            $71
 .byt SFXCMD_SetVolumeAndEnv,   $7F
 .byt SFXCMD_SetVibratoCtrl,    $5F
 .byt SFXCMD_SetVibratoBuffer,  $02, $A3, $80, $07
 .byt SFXCMD_Exec_80,            $1E
 .byt SFXCMD_SetFrameWaitCount, $05
 .byt SFXCMD_SetVibratoBuffer,  $01, $43, $80, $07
 .byt SFXCMD_SetVibratoCtrl,    $5F
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_Exec_80,            $0A
 .byt SFXCMD_SetDutyCycle,      $03*64
 .byt SFXCMD_SetVibratoCtrl,    $F1
 .byt SFXCMD_SetVolumeAndEnv,   $38
 .byt SFXCMD_Exec_80,            $64
 .byt SFXCMD_SetVolumeAndEnv,   $81
 .byt SFXCMD_SetVibratoCtrl,    $F1
 .byt SFXCMD_Exec_80,            $2F
 .byt SFXCMD_SetFrameWaitCount, $0D
 .byt SFXCMD_SetVibratoBuffer,  $01, $43, $80, $07
 .byt SFXCMD_SetVibratoCtrl,    $F1
 .byt SFXCMD_SetDutyCycle,      $02*64
 .byt SFXCMD_Exec_80,            $05
 .byt SFXCMD_EndOfData
.endscope

SFX_ThrowBoulder: ;at B97C
.scope
 .byt $B0 ;priority. Lo=music priority, Hi=sfx priority
 .byt $06 ;channel usage
: ;at B97E
 .byt SFXCMD_SetVibratoCtrl,    $0F
 .byt SFXCMD_SetDutyCycle,      $02*64
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_Exec_80,            $86
 .byt SFXCMD_SetFrameWaitCount, $05
 .byt SFXCMD_SetVolumeAndEnv,   $08
 .byt SFXCMD_SetVibratoCtrl,    $0F
 .byt SFXCMD_Exec_80,            $F0
 .byt SFXCMD_Loop, 3 
 .word :-
 .byt SFXCMD_EndOfData
.endscope

SFX_MeterRefill: ;at B993
.scope
 .byt $C0 ;priority. Lo=music priority, Hi=sfx priority
 .byt $07 ;channel usage
: ;at B995
 .byt SFXCMD_SetFrameWaitCount, $04
 .byt SFXCMD_SetVibratoBuffer,  $01, $41, $80, $00
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_SetDutyCycle,      $01*64
 .byt SFXCMD_Exec_80,            $6A
 .byt SFXCMD_SetDutyCycle,      $01*64
 .byt SFXCMD_SetVibratoBuffer,  $01, $41, $80, $00
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_Exec_80,            $54
 .byt SFXCMD_Loop, 2 
 .word :-
 .byt SFXCMD_EndOfData
.endscope

SFX_HittingGround: ;at B9B2
.scope
 .byt $60 ;priority. Lo=music priority, Hi=sfx priority
 .byt $02 ;channel usage
: ;at B9B4
 .byt SFXCMD_SetFrameWaitCount, $04
 .byt SFXCMD_SetDutyCycle,      $01*64
 .byt SFXCMD_SetVibratoCtrl,    $8B
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_SetVibratoBuffer,  $01, $46, $82, $01
 .byt SFXCMD_Exec_80,            $3F
 .byt SFXCMD_Loop, 1 
 .word :-
 .byt SFXCMD_EndOfData
.endscope

SFX_BonusPearlPickup: ;at B9C8
.scope
 .byt $D0 ;priority. Lo=music priority, Hi=sfx priority
 .byt $03 ;channel usage
: ;at B9CA
 .byt SFXCMD_SetFrameWaitCount, $04
 .byt SFXCMD_SetDutyCycle,      $02*64
 .byt SFXCMD_SetVibratoCtrl,    $FE
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_Exec_80,            $BE
 .byt SFXCMD_SetDutyCycle,      $02*64
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_SetVibratoCtrl,    $FE
 .byt SFXCMD_Exec_80,            $97
 .byt SFXCMD_SetFrameWaitCount, $04
 .byt SFXCMD_SetDutyCycle,      $02*64
 .byt SFXCMD_SetVibratoCtrl,    $FE
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_Exec_80,            $97
 .byt SFXCMD_SetDutyCycle,      $02*64
 .byt SFXCMD_SetVibratoCtrl,    $FE
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_Exec_80,            $7F
 .byt SFXCMD_SetFrameWaitCount, $04
 .byt SFXCMD_SetDutyCycle,      $02*64
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_Exec_80,            $3F
 .byt SFXCMD_SetDutyCycle,      $02*64
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_Exec_80,            $5F
 .byt SFXCMD_SetFrameWaitCount, $04
 .byt SFXCMD_SetDutyCycle,      $02*64
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_Exec_80,            $4D
 .byt SFXCMD_SetDutyCycle,      $02*64
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_Exec_80,            $4B
 .byt SFXCMD_Loop, 1 
 .word :-
 .byt SFXCMD_EndOfData
.endscope

SFX_BossHit: ;at BA0F
.scope
 .byt $E0 ;priority. Lo=music priority, Hi=sfx priority
 .byt $0E ;channel usage
 .byt SFXCMD_SetDutyCycle,      $01*64
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_SetVibratoBuffer,  $00, $05, $86, $07
 .byt SFXCMD_SetVibratoCtrl,    $40
 .byt SFXCMD_Exec_80,            $0E
 .byt SFXCMD_SetVolumeAndEnv,   $81
 .byt SFXCMD_SetVibratoCtrl,    $40
 .byt SFXCMD_SetVibratoBuffer,  $00, $05, $86, $07
 .byt SFXCMD_Exec_80,            $1A
 .byt SFXCMD_SetFrameWaitCount, $15
 .byt SFXCMD_SetDutyCycle,      $02*64
 .byt SFXCMD_SetVibratoCtrl,    $4F
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_Exec_80,            $0F
 .byt SFXCMD_EndOfData
.endscope

SFX_StartPressedAtTitle: ;at BA34
.scope
 .byt $E0 ;priority. Lo=music priority, Hi=sfx priority
 .byt $03 ;channel usage
 .byt SFXCMD_SetDutyCycle,      $03*64
 .byt SFXCMD_SetVibratoBuffer,  $00, $41, $82, $03
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_Exec_80,            $1A
 .byt SFXCMD_SetDutyCycle,      $03*64
 .byt SFXCMD_SetVibratoBuffer,  $00, $41, $82, $03
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_Exec_80,            $35
 .byt SFXCMD_SetFrameWaitCount, $04
 .byt SFXCMD_SetDutyCycle,      $01*64
 .byt SFXCMD_SetVibratoBuffer,  $01, $41, $86, $02
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_Exec_80,            $17
 .byt SFXCMD_SetDutyCycle,      $01*64
 .byt SFXCMD_SetVibratoBuffer,  $01, $41, $86, $02
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_Exec_80,            $1C
 .byt SFXCMD_SetFrameWaitCount, $08
 .byt SFXCMD_SetDutyCycle,      $03*64
 .byt SFXCMD_SetVibratoBuffer,  $00, $41, $82, $03
 .byt SFXCMD_SetVolumeAndEnv,   $3A
 .byt SFXCMD_Exec_80,            $1A
 .byt SFXCMD_SetDutyCycle,      $03*64
 .byt SFXCMD_SetVibratoBuffer,  $00, $41, $82, $03
 .byt SFXCMD_SetVolumeAndEnv,   $3A
 .byt SFXCMD_Exec_80,            $35
 .byt SFXCMD_SetFrameWaitCount, $04
 .byt SFXCMD_SetDutyCycle,      $01*64
 .byt SFXCMD_SetVibratoBuffer,  $01, $41, $86, $02
 .byt SFXCMD_SetVolumeAndEnv,   $3A
 .byt SFXCMD_Exec_80,            $17
 .byt SFXCMD_SetDutyCycle,      $01*64
 .byt SFXCMD_SetVibratoBuffer,  $01, $41, $86, $02
 .byt SFXCMD_SetVolumeAndEnv,   $3A
 .byt SFXCMD_Exec_80,            $1C
 .byt SFXCMD_SetFrameWaitCount, $24
 .byt SFXCMD_EndOfData
.endscope

SFX_1D: ;at BA97
.scope
 .byt $D0 ;priority. Lo=music priority, Hi=sfx priority
 .byt $07 ;channel usage
 .byt SFXCMD_SetVibratoCtrl,    $81
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_Exec_80,            $11
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_SetVibratoCtrl,    $81
 .byt SFXCMD_Exec_80,            $1E
 .byt SFXCMD_SetFrameWaitCount, $04
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_Exec_80,            $0F
 .byt SFXCMD_EndOfData
.endscope

SFX_1E: ;at BAAC
.scope
 .byt $C0 ;priority. Lo=music priority, Hi=sfx priority
 .byt $0E ;channel usage
 .byt SFXCMD_SetDutyCycle,      $01*64
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_SetVibratoBuffer,  $01, $E4, $80, $00
 .byt SFXCMD_Exec_80,            $43
 .byt SFXCMD_SetVibratoBuffer,  $01, $E2, $80, $00
 .byt SFXCMD_SetVolumeAndEnv,   $7F
 .byt SFXCMD_Exec_80,            $43
 .byt SFXCMD_SetFrameWaitCount, $18
 .byt SFXCMD_SetDutyCycle,      $02*64
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_SetVibratoBuffer,  $01, $E2, $80, $00
 .byt SFXCMD_Exec_80,            $0E
 .byt SFXCMD_EndOfData
.endscope

SFX_MenuMove: ;at BAD0
.scope
 .byt $90 ;priority. Lo=music priority, Hi=sfx priority
 .byt $03 ;channel usage
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_SetDutyCycle,      $01*64
 .byt SFXCMD_Exec_80,            $89
 .byt SFXCMD_SetFrameWaitCount, $05
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_SetDutyCycle,      $03*64
 .byt SFXCMD_Exec_80,            $86
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_SetVibratoBuffer,  $00, $E0, $80, $00
 .byt SFXCMD_SetDutyCycle,      $01*64
 .byt SFXCMD_Exec_80,            $CC
 .byt SFXCMD_SetFrameWaitCount, $08
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_SetVibratoBuffer,  $00, $E0, $80, $00
 .byt SFXCMD_SetDutyCycle,      $01*64
 .byt SFXCMD_Exec_80,            $C9
 .byt SFXCMD_EndOfData
.endscope

SFX_Transform: ;at BAF9
.scope
 .byt $90 ;priority. Lo=music priority, Hi=sfx priority
 .byt $0A ;channel usage
 .byt SFXCMD_SetFrameWaitCount, $04
 .byt SFXCMD_SetDutyCycle,      $03*64
 .byt SFXCMD_SetVibratoCtrl,    $25
 .byt SFXCMD_SetVibratoBuffer,  $01, $62, $82, $04
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_Exec_80,            $4D
 .byt SFXCMD_Exec_80,            $05
 .byt SFXCMD_Exec_80,            $0F
 .byt SFXCMD_SetFrameWaitCount, $05
 .byt SFXCMD_SetDutyCycle,      $03*64
 .byt SFXCMD_SetVibratoCtrl,    $F0
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_Exec_80,            $4D
 .byt SFXCMD_Exec_80,            $05
 .byt SFXCMD_Exec_80,            $0F
 .byt SFXCMD_EndOfData
.endscope

SFX_21: ;at BB1D
.scope
 .byt $A0 ;priority. Lo=music priority, Hi=sfx priority
 .byt $02 ;channel usage
 .byt SFXCMD_SetFrameWaitCount, $04
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_SetDutyCycle,      $01*64
 .byt SFXCMD_Exec_80,            $8E
 .byt SFXCMD_SetFrameWaitCount, $04
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_SetDutyCycle,      $01*64
 .byt SFXCMD_Exec_80,            $7F
 .byt SFXCMD_EndOfData
.endscope

SFX_PauseSound: ;at BB30
.scope
 .byt $E0 ;priority. Lo=music priority, Hi=sfx priority
 .byt $03 ;channel usage
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_SetDutyCycle,      $02*64
 .byt SFXCMD_SetNotePeriod    + $1,$00 ;hi,lo (C-0)
 .byt SFXCMD_SetFrameWaitCount, $05
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_SetDutyCycle,      $02*64
 .byt SFXCMD_Exec_80,            $FE
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_SetDutyCycle,      $02*64
 .byt SFXCMD_SetNotePeriod    + $1,$55 ;hi,lo (C-0)
 .byt SFXCMD_SetFrameWaitCount, $05
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_SetDutyCycle,      $02*64
 .byt SFXCMD_SetNotePeriod    + $1,$53 ;hi,lo (E-3)
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_SetDutyCycle,      $02*64
 .byt SFXCMD_SetNotePeriod    + $1,$95 ;hi,lo (C-0)
 .byt SFXCMD_SetFrameWaitCount, $05
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_SetDutyCycle,      $02*64
 .byt SFXCMD_SetNotePeriod    + $1,$93 ;hi,lo (C#3)
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_SetDutyCycle,      $02*64
 .byt SFXCMD_Exec_80,            $81
 .byt SFXCMD_SetFrameWaitCount, $05
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_SetDutyCycle,      $02*64
 .byt SFXCMD_Exec_80,            $7F
 .byt SFXCMD_EndOfData
.endscope

SFX_GutsmanLift: ;at BB6B
.scope
 .byt $E0 ;priority. Lo=music priority, Hi=sfx priority
 .byt $0A ;channel usage
: ;at BB6D
 .byt SFXCMD_SetDutyCycle,      $01*64
 .byt SFXCMD_SetVibratoCtrl,    $D0
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_SetVibratoBuffer,  $01, $E5, $82, $04
 .byt SFXCMD_SetNotePeriod    + $1,$FC ;hi,lo (A-2)
 .byt SFXCMD_SetFrameWaitCount, $07
 .byt SFXCMD_SetDutyCycle,      $02*64
 .byt SFXCMD_SetVibratoBuffer,  $01, $E5, $82, $04
 .byt SFXCMD_Exec_80,            $0F
 .byt SFXCMD_Loop, 0 
 .word :-
 .byt SFXCMD_EndOfData
.endscope

SFX_BossDoor: ;at BB8A
.scope
 .byt $D0 ;priority. Lo=music priority, Hi=sfx priority
 .byt $0A ;channel usage
: ;at BB8C
 .byt SFXCMD_SetDutyCycle,      $02*64
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_SetVibratoBuffer,  $03, $85, $81, $02
 .byt SFXCMD_SetVibratoCtrl,    $B1
 .byt SFXCMD_Exec_80,            $B3
 .byt SFXCMD_SetFrameWaitCount, $06
 .byt SFXCMD_SetDutyCycle,      $02*64
 .byt SFXCMD_SetVibratoBuffer,  $03, $85, $81, $02
 .byt SFXCMD_Exec_80,            $09
 .byt SFXCMD_Loop, 30 
 .word :-
 .byt SFXCMD_EndOfData
.endscope

SFX_Scissor: ;at BBA9
.scope
 .byt $D0 ;priority. Lo=music priority, Hi=sfx priority
 .byt $0A ;channel usage
: ;at BBAB
 .byt SFXCMD_SetVibratoBuffer,  $01, $62, $84, $03
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_Exec_80,            $21
 .byt SFXCMD_SetFrameWaitCount, $06
 .byt SFXCMD_SetVolumeAndEnv,   $3A
 .byt SFXCMD_SetDutyCycle,      $02*64
 .byt SFXCMD_SetVibratoBuffer,  $01, $62, $84, $03
 .byt SFXCMD_Exec_80,            $04
 .byt SFXCMD_SetVibratoBuffer,  $01, $62, $84, $03
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_Exec_80,            $21
 .byt SFXCMD_SetFrameWaitCount, $20
 .byt SFXCMD_SetVolumeAndEnv,   $3A
 .byt SFXCMD_SetDutyCycle,      $02*64
 .byt SFXCMD_SetVibratoBuffer,  $01, $62, $84, $03
 .byt SFXCMD_Exec_80,            $04
 .byt SFXCMD_Loop, 2 
 .word :-
 .byt SFXCMD_EndOfData
.endscope

SFX_CutmanScissors: ;at BBDC
.scope
 .byt $A0 ;priority. Lo=music priority, Hi=sfx priority
 .byt $0A ;channel usage
: ;at BBDE
 .byt SFXCMD_SetVibratoCtrl,    $B0
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_Exec_80,            $1E
 .byt SFXCMD_SetFrameWaitCount, $04
 .byt SFXCMD_SetVolumeAndEnv,   $3A
 .byt SFXCMD_Exec_80,            $03
 .byt SFXCMD_SetVibratoCtrl,    $B0
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_Exec_80,            $32
 .byt SFXCMD_SetFrameWaitCount, $04
 .byt SFXCMD_SetVolumeAndEnv,   $3A
 .byt SFXCMD_Exec_80,            $03
 .byt SFXCMD_Loop, 1 
 .word :-
 .byt SFXCMD_EndOfData
.endscope

SFX_27: ;at BBFB
.scope
 .byt $A0 ;priority. Lo=music priority, Hi=sfx priority
 .byt $0A ;channel usage
: ;at BBFD
 .byt SFXCMD_SetDutyCycle,      $00*64
 .byt SFXCMD_SetVibratoBuffer,  $01, $22, $83, $06
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_Exec_80,            $F0
 .byt SFXCMD_SetFrameWaitCount, $02
 .byt SFXCMD_SetDutyCycle,      $02*64
 .byt SFXCMD_SetVolumeAndEnv,   $3A
 .byt SFXCMD_Exec_80,            $0E
 .byt SFXCMD_Loop, 6 
 .word :-
 .byt SFXCMD_EndOfData
.endscope

SFX_GutsmanStomp: ;at BC15
.scope
 .byt $D0 ;priority. Lo=music priority, Hi=sfx priority
 .byt $0F ;channel usage
: ;at BC17
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_SetDutyCycle,      $03*64
 .byt SFXCMD_SetVibratoBuffer,  $01, $43, $80, $03
 .byt SFXCMD_SetVibratoCtrl,    $7F
 .byt SFXCMD_SetNotePeriod    + $3,$BF ;hi,lo (A#1)
 .byt SFXCMD_SetVolumeAndEnv,   $38
 .byt SFXCMD_SetDutyCycle,      $03*64
 .byt SFXCMD_SetVibratoBuffer,  $01, $43, $80, $03
 .byt SFXCMD_SetVibratoCtrl,    $02
 .byt SFXCMD_SetNotePeriod    + $6,$4E ;hi,lo (C#1)
 .byt SFXCMD_SetVolumeAndEnv,   $81
 .byt SFXCMD_SetVibratoBuffer,  $01, $43, $80, $03
 .byt SFXCMD_SetVibratoCtrl,    $5F
 .byt SFXCMD_SetNotePeriod    + $6,$4E ;hi,lo (C#1)
 .byt SFXCMD_SetFrameWaitCount, $05
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_Exec_80,            $0F
 .byt SFXCMD_Loop, 8 
 .word :-
 .byt SFXCMD_EndOfData
.endscope

SFX_BigEyeJump: ;at BC47
.scope
 .byt $B0 ;priority. Lo=music priority, Hi=sfx priority
 .byt $0E ;channel usage
: ;at BC49
 .byt SFXCMD_SetDutyCycle,      $03*64
 .byt SFXCMD_SetVibratoCtrl,    $B1
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_SetVibratoBuffer,  $02, $A7, $82, $05
 .byt SFXCMD_SetNotePeriod    + $1,$FC ;hi,lo (A-2)
 .byt SFXCMD_SetVibratoCtrl,    $81
 .byt SFXCMD_SetVolumeAndEnv,   $81
 .byt SFXCMD_SetNotePeriod    + $1,$AB ;hi,lo (C-3)
 .byt SFXCMD_SetFrameWaitCount, $04
 .byt SFXCMD_SetDutyCycle,      $02*64
 .byt SFXCMD_Exec_80,            $0D
 .byt SFXCMD_Loop, 1 
 .word :-
 .byt SFXCMD_EndOfData
.endscope

SFX_Icebeam: ;at BC67
.scope
 .byt $80 ;priority. Lo=music priority, Hi=sfx priority
 .byt $0E ;channel usage
 .byt SFXCMD_SetVibratoCtrl,    $B0
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_Exec_80,            $3C
 .byt SFXCMD_SetVolumeAndEnv,   $7F
 .byt SFXCMD_Exec_80,            $0F
 .byt SFXCMD_SetFrameWaitCount, $04
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_Exec_80,            $03
 .byt SFXCMD_SetVibratoCtrl,    $B0
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_Exec_80,            $3C
 .byt SFXCMD_SetVolumeAndEnv,   $7F
 .byt SFXCMD_Exec_80,            $0F
 .byt SFXCMD_SetFrameWaitCount, $04
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_Exec_80,            $03
 .byt SFXCMD_EndOfData
.endscope

SFX_JumpIntoWater: ;at BC8A
.scope
 .byt $C0 ;priority. Lo=music priority, Hi=sfx priority
 .byt $0A ;channel usage
 .byt SFXCMD_SetVibratoCtrl,    $60
 .byt SFXCMD_SetVibratoBuffer,  $01, $21, $82, $05
 .byt SFXCMD_SetVolumeAndEnv,   $39
 .byt SFXCMD_SetNotePeriod    + $1,$FC ;hi,lo (A-2)
 .byt SFXCMD_SetFrameWaitCount, $06
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_Exec_80,            $07
 .byt SFXCMD_SetVibratoCtrl,    $30
 .byt SFXCMD_SetVibratoBuffer,  $01, $41, $82, $05
 .byt SFXCMD_SetVolumeAndEnv,   $39
 .byt SFXCMD_Exec_80,            $38
 .byt SFXCMD_SetFrameWaitCount, $17
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_Exec_80,            $03
 .byt SFXCMD_EndOfData
.endscope

SFX_BlockReplicating: ;at BCAF
.scope
 .byt $A0 ;priority. Lo=music priority, Hi=sfx priority
 .byt $0C ;channel usage
 .byt SFXCMD_SetVolumeAndEnv,   $81
 .byt SFXCMD_SetVibratoCtrl,    $FE
 .byt SFXCMD_Exec_80,            $D5
 .byt SFXCMD_SetFrameWaitCount, $30
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_SetVibratoBuffer,  $04, $43, $80, $00
 .byt SFXCMD_SetVibratoCtrl,    $FE
 .byt SFXCMD_Exec_80,            $0A
 .byt SFXCMD_EndOfData
.endscope

SFX_2D: ;at BCC5
.scope
 .byt $90 ;priority. Lo=music priority, Hi=sfx priority
 .byt $0E ;channel usage
 .byt SFXCMD_SetVibratoCtrl,    $28
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_SetNotePeriod    + $2,$3B ;hi,lo (G-2)
 .byt SFXCMD_SetVolumeAndEnv,   $7F
 .byt SFXCMD_SetVibratoCtrl,    $28
 .byt SFXCMD_SetNotePeriod    + $1,$1D ;hi,lo (G-3)
 .byt SFXCMD_SetFrameWaitCount, $25
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_Exec_80,            $0E
 .byt SFXCMD_EndOfData
.endscope

SFX_2E: ;at BCDA
.scope
 .byt $90 ;priority. Lo=music priority, Hi=sfx priority
 .byt $0E ;channel usage
 .byt SFXCMD_SetVibratoCtrl,    $18
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_SetNotePeriod    + $2,$3B ;hi,lo (G-2)
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_SetVibratoCtrl,    $14
 .byt SFXCMD_SetNotePeriod    + $1,$1D ;hi,lo (G-3)
 .byt SFXCMD_SetFrameWaitCount, $10
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_Exec_80,            $0E
 .byt SFXCMD_EndOfData
.endscope

SFX_2F: ;at BCEF
.scope
 .byt $90 ;priority. Lo=music priority, Hi=sfx priority
 .byt $02 ;channel usage
: ;at BCF1
 .byt SFXCMD_SetFrameWaitCount, $04
 .byt SFXCMD_SetDutyCycle,      $00*64
 .byt SFXCMD_SetVibratoCtrl,    $33
 .byt SFXCMD_SetVibratoBuffer,  $01, $C1, $80, $00
 .byt SFXCMD_SetVolumeAndEnv,   $3E
 .byt SFXCMD_Exec_80,            $BE
 .byt SFXCMD_Loop, 2 
 .word :-
 .byt SFXCMD_EndOfData
.endscope

SFX_Wind: ;at BD05
.scope
 .byt $E0 ;priority. Lo=music priority, Hi=sfx priority
 .byt $08 ;channel usage
: ;at BD07
 .byt SFXCMD_SetFrameWaitCount, $04
 .byt SFXCMD_SetVolumeAndEnv,   $36
 .byt SFXCMD_Exec_80,            $04
 .byt SFXCMD_Loop, 2 
 .word :-
: ;at BD11
 .byt SFXCMD_SetFrameWaitCount, $04
 .byt SFXCMD_SetVolumeAndEnv,   $38
 .byt SFXCMD_Exec_80,            $04
 .byt SFXCMD_Loop, 2 
 .word :-
: ;at BD1B
 .byt SFXCMD_SetFrameWaitCount, $04
 .byt SFXCMD_SetVolumeAndEnv,   $3A
 .byt SFXCMD_Exec_80,            $04
 .byt SFXCMD_Loop, 2 
 .word :-
: ;at BD25
 .byt SFXCMD_SetFrameWaitCount, $10
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_Exec_80,            $04
 .byt SFXCMD_Loop, 4 
 .word :-
: ;at BD2F
 .byt SFXCMD_SetFrameWaitCount, $04
 .byt SFXCMD_SetVolumeAndEnv,   $3A
 .byt SFXCMD_Exec_80,            $04
 .byt SFXCMD_Loop, 2 
 .word :-
: ;at BD39
 .byt SFXCMD_SetFrameWaitCount, $04
 .byt SFXCMD_SetVolumeAndEnv,   $38
 .byt SFXCMD_Exec_80,            $04
 .byt SFXCMD_Loop, 2 
 .word :-
: ;at BD43
 .byt SFXCMD_SetFrameWaitCount, $04
 .byt SFXCMD_SetVolumeAndEnv,   $36
 .byt SFXCMD_Exec_80,            $04
 .byt SFXCMD_Loop, 2 
 .word :-
 .byt SFXCMD_EndOfData
.endscope

SFX_MegamanKilled: ;at BD4E
.scope
 .byt $F0 ;priority. Lo=music priority, Hi=sfx priority
 .byt $03 ;channel usage
 .byt SFXCMD_SetDutyCycle,      $02*64
 .byt SFXCMD_SetVibratoCtrl,    $2F
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_Exec_80,            $35
 .byt SFXCMD_SetFrameWaitCount, $10
 .byt SFXCMD_SetDutyCycle,      $00*64
 .byt SFXCMD_SetVibratoCtrl,    $2F
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_Exec_80,            $3C
 .byt SFXCMD_SetDutyCycle,      $02*64
 .byt SFXCMD_SetVibratoCtrl,    $2F
 .byt SFXCMD_SetVolumeAndEnv,   $3C
 .byt SFXCMD_Exec_80,            $35
 .byt SFXCMD_SetFrameWaitCount, $10
 .byt SFXCMD_SetDutyCycle,      $00*64
 .byt SFXCMD_SetVibratoCtrl,    $2F
 .byt SFXCMD_SetVolumeAndEnv,   $3C
 .byt SFXCMD_Exec_80,            $3C
 .byt SFXCMD_SetDutyCycle,      $02*64
 .byt SFXCMD_SetVibratoCtrl,    $2F
 .byt SFXCMD_SetVolumeAndEnv,   $39
 .byt SFXCMD_Exec_80,            $35
 .byt SFXCMD_SetFrameWaitCount, $10
 .byt SFXCMD_SetDutyCycle,      $00*64
 .byt SFXCMD_SetVibratoCtrl,    $2F
 .byt SFXCMD_SetVolumeAndEnv,   $39
 .byt SFXCMD_Exec_80,            $3C
 .byt SFXCMD_SetDutyCycle,      $02*64
 .byt SFXCMD_SetVibratoCtrl,    $2F
 .byt SFXCMD_SetVolumeAndEnv,   $36
 .byt SFXCMD_Exec_80,            $35
 .byt SFXCMD_SetFrameWaitCount, $10
 .byt SFXCMD_SetDutyCycle,      $00*64
 .byt SFXCMD_SetVibratoCtrl,    $2F
 .byt SFXCMD_SetVolumeAndEnv,   $36
 .byt SFXCMD_Exec_80,            $3C
 .byt SFXCMD_SetDutyCycle,      $02*64
 .byt SFXCMD_SetVibratoCtrl,    $2F
 .byt SFXCMD_SetVolumeAndEnv,   $34
 .byt SFXCMD_Exec_80,            $35
 .byt SFXCMD_SetFrameWaitCount, $10
 .byt SFXCMD_SetDutyCycle,      $00*64
 .byt SFXCMD_SetVibratoCtrl,    $2F
 .byt SFXCMD_SetVolumeAndEnv,   $34
 .byt SFXCMD_Exec_80,            $3C
 .byt SFXCMD_SetDutyCycle,      $02*64
 .byt SFXCMD_SetVibratoCtrl,    $2F
 .byt SFXCMD_SetVolumeAndEnv,   $32
 .byt SFXCMD_Exec_80,            $35
 .byt SFXCMD_SetFrameWaitCount, $10
 .byt SFXCMD_SetDutyCycle,      $00*64
 .byt SFXCMD_SetVibratoCtrl,    $2F
 .byt SFXCMD_SetVolumeAndEnv,   $32
 .byt SFXCMD_Exec_80,            $3C
 .byt SFXCMD_EndOfData
.endscope

SFX_ExtraLife: ;at BDBD
.scope
 .byt $F0 ;priority. Lo=music priority, Hi=sfx priority
 .byt $01 ;channel usage
 .byt SFXCMD_SetFrameWaitCount, $03
 .byt SFXCMD_SetDutyCycle,      $03*64
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_Exec_80,            $64
 .byt SFXCMD_SetFrameWaitCount, $03
 .byt SFXCMD_SetDutyCycle,      $03*64
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_Exec_80,            $59
 .byt SFXCMD_SetFrameWaitCount, $03
 .byt SFXCMD_SetDutyCycle,      $03*64
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_Exec_80,            $50
 .byt SFXCMD_SetFrameWaitCount, $03
 .byt SFXCMD_SetDutyCycle,      $03*64
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_Exec_80,            $4B
 .byt SFXCMD_SetFrameWaitCount, $03
 .byt SFXCMD_SetDutyCycle,      $03*64
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_Exec_80,            $43
 .byt SFXCMD_SetFrameWaitCount, $03
 .byt SFXCMD_SetDutyCycle,      $03*64
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_Exec_80,            $3C
 .byt SFXCMD_SetFrameWaitCount, $03
 .byt SFXCMD_SetDutyCycle,      $03*64
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_Exec_80,            $35
 .byt SFXCMD_SetFrameWaitCount, $03
 .byt SFXCMD_SetDutyCycle,      $03*64
 .byt SFXCMD_SetVolumeAndEnv,   $3F
 .byt SFXCMD_Exec_80,            $32
 .byt SFXCMD_SetFrameWaitCount, $03
 .byt SFXCMD_SetDutyCycle,      $03*64
 .byt SFXCMD_SetVolumeAndEnv,   $38
 .byt SFXCMD_Exec_80,            $64
 .byt SFXCMD_SetFrameWaitCount, $03
 .byt SFXCMD_SetDutyCycle,      $03*64
 .byt SFXCMD_SetVolumeAndEnv,   $38
 .byt SFXCMD_Exec_80,            $59
 .byt SFXCMD_SetFrameWaitCount, $03
 .byt SFXCMD_SetDutyCycle,      $03*64
 .byt SFXCMD_SetVolumeAndEnv,   $38
 .byt SFXCMD_Exec_80,            $50
 .byt SFXCMD_SetFrameWaitCount, $03
 .byt SFXCMD_SetDutyCycle,      $03*64
 .byt SFXCMD_SetVolumeAndEnv,   $38
 .byt SFXCMD_Exec_80,            $4B
 .byt SFXCMD_SetFrameWaitCount, $03
 .byt SFXCMD_SetDutyCycle,      $03*64
 .byt SFXCMD_SetVolumeAndEnv,   $38
 .byt SFXCMD_Exec_80,            $43
 .byt SFXCMD_SetFrameWaitCount, $03
 .byt SFXCMD_SetDutyCycle,      $03*64
 .byt SFXCMD_SetVolumeAndEnv,   $38
 .byt SFXCMD_Exec_80,            $3C
 .byt SFXCMD_SetFrameWaitCount, $03
 .byt SFXCMD_SetDutyCycle,      $03*64
 .byt SFXCMD_SetVolumeAndEnv,   $38
 .byt SFXCMD_Exec_80,            $35
 .byt SFXCMD_SetFrameWaitCount, $03
 .byt SFXCMD_SetDutyCycle,      $03*64
 .byt SFXCMD_SetVolumeAndEnv,   $38
 .byt SFXCMD_Exec_80,            $32
 .byt SFXCMD_EndOfData
 .byt $1A,$02,$C0,$05,$00,$41,$82,$03,$03,$3F,$80,$35,$00,$04,$02,$40 ;error
 .byt $05,$01,$41,$86,$02,$03,$3F,$80,$17,$02,$40,$05,$01,$41,$86,$02 ;error
 .byt $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF ;error
 .byt $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF ;error
 .byt $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF ;error
 .byt $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF ;error
 .byt $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF ;error
 .byt $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF ;error
 .byt $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF ;error
 .byt $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF ;error
 .byt $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF ;error
 .byt $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF ;error
 .byt $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF ;error
 .byt $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF ;error
 .byt $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF ;error
 .byt $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF ;error
 .byt $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF ;error
 .byt $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF ;error
 .byt $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF ;error
 .byt $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF ;error
 .byt $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF ;error
 .byt $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF ;error
 .byt $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF ;error
 .byt $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF ;error
 .byt $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF ;error
 .byt $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF ;error
 .byt $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF ;error
 .byt $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF ;error
.endscope