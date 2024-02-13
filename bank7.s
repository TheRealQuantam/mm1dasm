.include "globals.inc"

/*
	MEGAMAN 1 BANK 7 ROM MAP

	This bank's disassembly is a MIXTURE of Rockman (J) and Mega Man (U)
 */

.segment "BANK7"

BankTable: .byte 0,1,2,3,4,5,6,7

Reset:
/* 1C008: 78 */        sei
/* 1C009: A9 01 */     lda #$01
/* 1C00B: 8D 00 20 */  sta $2000
/* 1C00E: A9 06 */     lda #$06
/* 1C010: 8D 01 20 */  sta $2001
/* 1C013: A9 05 */     lda #$05
/* 1C015: 8D 05 C0 */  sta BankTable+5
/* 1C018: 4C 00 90 */  jmp Reset2                                      ; $9000


;
; if MegamanStallTimer != 0 then don't get joypad input in this frame
;
; if ForcedInputFlag != 0 then JoyPad0 = ForcedInputData
;


NextFrame:
/* 1C01B: A5 95 */     lda MegamanStallTimer
/* 1C01D: D0 08 */     bne L1C027 ; +                                       ; $C027
/* 1C01F: A5 14 */     lda JoyPad0
/* 1C021: 85 16 */     sta JoyPad0old
/* 1C023: A5 15 */     lda JoyPad1
/* 1C025: 85 17 */     sta JoyPad1old
L1C027: ; +


; Wait until our NMI's done what an NMI's gotta do...
/* 1C027: A9 00 */     lda #$00
/* 1C029: 85 1F */     sta NMI_GfxUpdateDone
L1C02B: ; -
/* 1C02B: A5 1F */     lda NMI_GfxUpdateDone
/* 1C02D: F0 FC */     beq L1C02B ; -                                       ; $0001C02B

/* 1C02F: A5 68 */     lda ForcedInputFlag
/* 1C031: F0 07 */     beq L1C03A ; +                                       ; $0001C03A
/* 1C033: A5 69 */     lda ForcedInputData
/* 1C035: 85 14 */     sta JoyPad0
/* 1C037: 4C 4B C0 */  jmp L1C04B ; ++                                      ; $C04B

L1C03A: ; +
/* 1C03A: A5 95 */     lda MegamanStallTimer
/* 1C03C: D0 1B */     bne L1C059 ; +++                                     ; $0001C059
/* 1C03E: 98 */        tya
/* 1C03F: 48 */        pha
/* 1C040: 8A */        txa
/* 1C041: 48 */        pha
/* 1C042: 20 8D D5 */  jsr ReadJoypads                             ; $D58D
/* 1C045: 68 */        pla
/* 1C046: AA */        tax
/* 1C047: 68 */        pla

/* 1C048: A8 */        tay
/* 1C049: A5 14 */     lda JoyPad0

L1C04B: ; ++
/* 1C04B: 45 16 */     eor JoyPad0old
/* 1C04D: 25 14 */     and JoyPad0
/* 1C04F: 85 18 */     sta JoyD0
/* 1C051: A5 15 */     lda JoyPad1
/* 1C053: 45 17 */     eor JoyPad1old
/* 1C055: 25 15 */     and JoyPad1
/* 1C057: 85 19 */     sta JoyD1

L1C059: ; +++
/* 1C059: 60 */        rts


StageClear:
; What happens in regular stages.
;1. Play item sound
;2. Play stage clear music
;3. Load levelend gfx
;4. Remove all objects except Megaman
;5. Delay 383 frames
;6. Hide energy bars
;7. Draw first patch of text
;8. Halt Megaman
;9. Calculate score, 1000 points per frame
;10. Delay 64 frames
;11. Draw second patch of text
;12. Calculate bonus pearls, one per frame (3000 points each)
;13. Delay 383 frames
;14. Reboot game without clearing game status
;
/* 1C05A: A9 00 */     lda #$00
/* 1C05C: 85 3E */     sta BossCurrentStrategy

/* 1C05E: A9 FE */     lda #$FE
/* 1C060: 20 77 C4 */  jsr IssueSound                              ; $C477

/* 1C063: A5 AC */     lda FightingBossNum
/* 1C065: C9 0A */     cmp #$0A
/* 1C067: D0 08 */     bne PlayStageClearMusic                     ; $0001C071

/* 1C069: A9 00 */     lda #$00
/* 1C06B: 20 77 C4 */  jsr IssueSound                              ; $C477

/* 1C06E: 4C 79 C0 */  jmp L1C079 ; ++                                      ; $C079

; Play stage clear melody
PlayStageClearMusic:
/* 1C071: A9 04 */     lda #$04
/* 1C073: 20 77 C4 */  jsr IssueSound                              ; $C477

/* 1C076: 20 04 C2 */  jsr ClearAllObjectsExceptMegaman            ; $C204

L1C079: ; ++
/* 1C079: A9 00 */     lda #$00
/* 1C07B: 85 5C */     sta $5C
/* 1C07D: A9 B7 */     lda #>LxB700
/* 1C07F: A2 0B */     ldx #$0B
/* 1C081: A0 08 */     ldy #$08
/* 1C083: 20 BE F0 */  jsr DoPPUtransferRoutineF0BE
/* 1C086: A5 AC */     lda FightingBossNum
/* 1C088: C9 0A */     cmp #$0A
/* 1C08A: D0 0A */     bne L1C096 ; +                                       ; $C096
; Whole game cleared?
/* 1C08C: A9 FF */     lda #$FF
/* 1C08E: 20 16 D1 */  jsr TimeDelayWithAllObjectsHalted
/* 1C091: A9 FF */     lda #$FF
/* 1C093: 20 16 D1 */  jsr TimeDelayWithAllObjectsHalted
L1C096: ; + ;Nope
/* 1C096: A9 80 */     lda #$80
/* 1C098: 20 16 D1 */  jsr TimeDelayWithAllObjectsHalted
/* 1C09B: A9 FF */     lda #$FF
/* 1C09D: 20 16 D1 */  jsr TimeDelayWithAllObjectsHalted
/* 1C0A0: A5 AC */     lda FightingBossNum
/* 1C0A2: C9 0A */     cmp #$0A
/* 1C0A4: D0 08 */     bne L1C0AE ; +                                       ; $C0AE
/* 1C0A6: 20 04 C2 */  jsr ClearAllObjectsExceptMegaman            ; $C204

/* 1C0A9: A9 F8 */     lda #$F8    ; Disable Megaman object
/* 1C0AB: 8D 00 06 */  sta ObjectPosY+0                             ; $0600

L1C0AE: ; +
/* 1C0AE: E6 BB */     inc DrawScoreAndMetersFlag
/* 1C0B0: A9 00 */     lda #$00
/* 1C0B2: 85 2F */     sta RefObjectNum
/* 1C0B4: A2 02 */     ldx #$02 ;into the given slot
/* 1C0B6: 20 63 C1 */  jsr SetNObjects

/* 1C0B9: A2 05 */     ldx #$05
L1C0BB: ; -
/* 1C0BB: 95 B5 */     sta $B5,x
/* 1C0BD: CA */        dex
/* 1C0BE: 10 FB */     bpl L1C0BB ; -                                           ; $C0BB

/* 1C0C0: EE A0 06 */  inc ObjectLifeCycleCounter+0

L1C0C3: ; -
/* 1C0C3: A2 00 */     ldx #$00
/* 1C0C5: 20 81 C1 */  jsr LevelEndAddThousandPoints
/* 1C0C8: 20 AC C1 */  jsr F1C1AC
/* 1C0CB: 20 1B C0 */  jsr NextFrame                                  ; $C01B
/* 1C0CE: C6 3D */     dec LevelClearScoreInThousands
/* 1C0D0: D0 F1 */     bne L1C0C3 ; -                                           ; $C0C3

; wait 1 second
/* 1C0D2: A9 40 */     lda #$40
/* 1C0D4: 20 0F C2 */  jsr TimeDelayNoMove                             ; $C20F

/* 1C0D7: A9 00 */     lda #$00
/* 1C0D9: 85 2F */     sta RefObjectNum
/* 1C0DB: A2 05 */     ldx #$05
/* 1C0DD: 20 63 C1 */  jsr SetNObjects
/* 1C0E0: EE A0 06 */  inc ObjectLifeCycleCounter+0
/* 1C0E3: A5 AE */     lda BonusPearlCount
/* 1C0E5: F0 12 */     beq L1C0F9 ; +                                           ; $C0F9
L1C0E7: ; -
/* 1C0E7: A2 03 */     ldx #$03
/* 1C0E9: 20 81 C1 */  jsr LevelEndAddThousandPoints
/* 1C0EC: 20 AC C1 */  jsr F1C1AC
/* 1C0EF: 20 BE C1 */  jsr F1C1BE
/* 1C0F2: 20 1B C0 */  jsr NextFrame                                   ; $C01B
/* 1C0F5: C6 AE */     dec BonusPearlCount
/* 1C0F7: D0 EE */     bne L1C0E7 ; -                                           ; $C0E7
L1C0F9: ; +
/* 1C0F9: 20 31 D1 */  jsr UpdateGraphics                              ; $D131
/* 1C0FC: 20 AC C1 */  jsr F1C1AC
/* 1C0FF: 20 BE C1 */  jsr F1C1BE

; Wait 6-7 seconds
/* 1C102: A9 FF */     lda #$FF
/* 1C104: 20 0F C2 */  jsr TimeDelayNoMove                               ; $C20F
/* 1C107: A9 80 */     lda #$80
/* 1C109: 20 0F C2 */  jsr TimeDelayNoMove                               ; $C20F

L1C10C:
/* 1C10C: 20 95 D4 */  jsr DisableNMIandPPU                          ; $D495
/* 1C10F: C6 BB */     dec DrawScoreAndMetersFlag

/* 1C111: A2 FF */     ldx #$FF
/* 1C113: 9A */        txs

/* 1C114: A9 05 */     lda #$05
/* 1C116: 8D 05 C0 */  sta BankTable+5
/* 1C119: 85 42 */     sta CurrentBank

/* 1C11B: A6 31 */     ldx CurrentStage
/* 1C11D: E0 09 */     cpx #$09
/* 1C11F: F0 16 */     beq L1C137 ; +                                   ; $0001C137
/* 1C121: E0 06 */     cpx #$06
/* 1C123: B0 16 */     bcs L1C13B ; ++                                  ; $0001C13B

; Add new weapon/set boss defeated flag
/* 1C125: A5 5D */     lda WeaponsOwned
/* 1C127: 1D 48 C1 */  ora WeaponsOwnedTab,x
/* 1C12A: 85 5D */     sta WeaponsOwned

/* 1C12C: A9 00 */     lda #$00
/* 1C12E: 85 BC */     sta IgnoreStageSelection
/* 1C130: 85 55 */     sta MegamanBlinkState
/* 1C132: 85 68 */     sta ForcedInputFlag
/* 1C134: 4C 5A 90 */  jmp L1505A

; Prepare for ending
L1C137: ; +
/* 1C137: E6 31 */     inc CurrentStage
/* 1C139: C6 BB */     dec DrawScoreAndMetersFlag

; Prepare for Wilystage
L1C13B: ; ++
/* 1C13B: A9 00 */     lda #$00
/* 1C13D: 85 B4 */     sta UseTempScrollX
/* 1C13F: 85 AB */     sta LastRestartPointType
/* 1C141: 85 68 */     sta ForcedInputFlag
/* 1C143: E6 31 */     inc CurrentStage
/* 1C145: 4C 6A 90 */  jmp L1506A

;C148                    C    I    B    F    E    G
WeaponsOwnedTab:  .byte $20, $10, $02, $40, $04, $08 ;See also: Lbl_b50d
;C14E
IndexIntoTheActiveThatPreventsEscapingRematch:
    .byte 0*6, 5*6, 3*6, 4*6, 1*6, 6*6 ;indexes into RoomActiveTable

; Objects in order:
;    0: HP bar
;    1: Score
XPosTable1:      .byte $80, $A0, $80, $88, $A0 ; C154
YPosTable1:      .byte $40, $58, $68, $80, $80 ; C159
SpriteNumTable1: .byte $87, $89, $88, $8A, $89 ; C15e

SetNObjects:
/* 1C163: 20 7B F6 */  jsr InitActor
/* 1C166: BD 53 C1 */  lda XPosTable1-1,x
/* 1C169: 9D 80 04 */  sta ObjectPosX,x
/* 1C16C: BD 58 C1 */  lda YPosTable1-1,x
/* 1C16F: 9D 00 06 */  sta ObjectPosY,x
/* 1C172: BD 5D C1 */  lda SpriteNumTable1-1,x
/* 1C175: 9D 00 04 */  sta ObjectSpriteNum,x
/* 1C178: A9 00 */     lda #$00
/* 1C17A: 9D 20 04 */  sta ObjectFlags,x
/* 1C17D: CA */        dex
/* 1C17E: D0 E3 */     bne SetNObjects

/* 1C180: 60 */        rts


; X = index to the digits
LevelEndAddThousandPoints:
/* 1C181: A0 03 */     ldy #$03
/* 1C183: 18 */        clc
/* 1C184: A9 01 */     lda #$01
L1C186: ; -
/* 1C186: 75 B5 */     adc $B5,x
/* 1C188: C9 0A */     cmp #$0A
/* 1C18A: 90 02 */     bcc L1C18E ; +                                       ; $0001C18E
/* 1C18C: E9 0A */     sbc #$0A
L1C18E: ; +
/* 1C18E: 95 B5 */     sta $B5,x
/* 1C190: A9 00 */     lda #$00
/* 1C192: E8 */        inx
/* 1C193: 88 */        dey
/* 1C194: D0 F0 */     bne L1C186 ; -                                       ; $0001C186

/* 1C196: A9 10 */     lda #$10        ; Add 1,000 points to score
/* 1C198: 85 05 */     sta $05
/* 1C19A: 20 40 D4 */  jsr IncreaseScore                           ; $D440

/* 1C19D: 20 31 D1 */  jsr UpdateGraphics                          ; $D131
/* 1C1A0: A5 23 */     lda FrameCounter
/* 1C1A2: 29 03 */     and #$03
/* 1C1A4: D0 05 */     bne L1C1AB ; +                                       ; $C1AB
/* 1C1A6: A9 21 */     lda #$21        ; SFX for points
/* 1C1A8: 20 77 C4 */  jsr IssueSound                              ; $C477
L1C1AB: ; +
/* 1C1AB: 60 */        rts




F1C1AC:
/* 1C1AC: A2 02 */     ldx #$02    ; 3 sprites
/* 1C1AE: 86 0E */     stx $0E
/* 1C1B0: A0 C0 */     ldy #$C0    ; spritepage index = #$C4
/* 1C1B2: A9 68 */     lda #$68    ; X position = #$68
/* 1C1B4: 85 0C */     sta $0C
/* 1C1B6: A9 50 */     lda #$50    ; Y position = #$50
/* 1C1B8: 85 0D */     sta $0D
/* 1C1BA: 20 DD C1 */  jsr ProcessSprites                          ; $C1DD
/* 1C1BD: 60 */        rts

;
;
;

F1C1BE:
/* 1C1BE: A2 04 */     ldx #$04
/* 1C1C0: A9 01 */     lda #$01    ; 2 sprites
/* 1C1C2: 85 0E */     sta $0E
/* 1C1C4: A9 78 */     lda #$78
/* 1C1C6: 85 0D */     sta $0D
/* 1C1C8: A9 70 */     lda #$70
/* 1C1CA: 85 0C */     sta $0C
/* 1C1CC: 20 DD C1 */  jsr ProcessSprites                          ; $C1DD

/* 1C1CF: A2 04 */     ldx #$04
/* 1C1D1: A9 01 */     lda #$01
/* 1C1D3: 85 0E */     sta $0E
/* 1C1D5: A9 60 */     lda #$60
/* 1C1D7: 85 0D */     sta $0D
/* 1C1D9: A9 90 */     lda #$90
/* 1C1DB: 85 0C */     sta $0C
                    ;continues to ProcessSprites

;
;
; Process sprite information (write to spritepage)
;
; $0C = X position
; $0D = Y position
;
; $0E = Number of sprites - 1
;
; Palette # = 1
;
; X = index into $B5
;


ProcessSprites:
L1C1DD: ; -
/* 1C1DD: B5 B5 */     lda $B5,x       ; Get tile number
/* 1C1DF: 09 F0 */     ora #$F0
/* 1C1E1: 99 05 02 */  sta CurrentSpriteData+1,y
/* 1C1E4: A5 0C */     lda $0C
/* 1C1E6: 99 07 02 */  sta CurrentSpriteData+3,y
/* 1C1E9: A5 0D */     lda $0D
/* 1C1EB: 99 04 02 */  sta CurrentSpriteData+0,y
/* 1C1EE: A9 01 */     lda #$01
/* 1C1F0: 99 06 02 */  sta CurrentSpriteData+2,y
/* 1C1F3: C8 */        iny
/* 1C1F4: C8 */        iny
/* 1C1F5: C8 */        iny
/* 1C1F6: C8 */        iny
/* 1C1F7: CA */        dex
/* 1C1F8: 18 */        clc
/* 1C1F9: A5 0C */     lda $0C         ; X position += 8
/* 1C1FB: 69 08 */     adc #$08
/* 1C1FD: 85 0C */     sta $0C
/* 1C1FF: C6 0E */     dec $0E
/* 1C201: 10 DA */     bpl L1C1DD ; -                                           ; $C1DD

/* 1C203: 60 */        rts


; Clears all objects except Megaman

ClearAllObjectsExceptMegaman:
/* 1C204: A9 F8 */     lda #$F8
/* 1C206: A2 1F */     ldx #$1F
L1C208: ; -
/* 1C208: 9D 00 06 */  sta ObjectPosY,x
/* 1C20B: CA */        dex
/* 1C20C: D0 FA */     bne L1C208 ; -                               ; $0001C208
/* 1C20E: 60 */        rts


;
; A = number of frames to freeze gameplay for
;

TimeDelayNoMove:
/* 1C20F: 85 3C */     sta MiscCounter1
L1C211: ; -
/* 1C211: 20 1B C0 */  jsr NextFrame                      ; $C01B
/* 1C214: C6 3C */     dec MiscCounter1
/* 1C216: D0 F9 */     bne L1C211 ; -                              ; $C211
/* 1C218: 60 */        rts


MegaManKilled:
/* 1C219: A9 FE */     lda #$FE
/* 1C21B: 20 77 C4 */  jsr IssueSound                      ; $C477
/* 1C21E: A9 FF */     lda #$FF
/* 1C220: 20 77 C4 */  jsr IssueSound                      ; $C477
/* 1C223: A9 20 */     lda #$20
/* 1C225: 20 16 D1 */  jsr TimeDelayWithAllObjectsHalted

/* 1C228: A9 31 */     lda #$31        ; Mega Man exploding
/* 1C22A: 20 77 C4 */  jsr IssueSound                      ; $C477

/* 1C22D: A9 00 */     lda #$00
/* 1C22F: 85 59 */     sta $59
/* 1C231: A9 7D */     lda #$7D
/* 1C233: 85 5A */     sta $5A
/* 1C235: 20 2A C3 */  jsr AtomicExplodeActor

/* 1C238: A9 00 */     lda #$00
/* 1C23A: 85 45 */     sta NumIssuedSounds

/* 1C23C: A9 F8 */     lda #$F8        ; Hide Megaman
/* 1C23E: 8D 00 06 */  sta ObjectPosY+0                                 ; $0600

/* 1C241: A9 C0 */     lda #$C0
/* 1C243: 85 3C */     sta MiscCounter1
L1C245: ; -
/* 1C245: 20 17 C3 */  jsr TimeDelayC317
/* 1C248: D0 FB */     bne L1C245 ; -                                       ; $0001C245

/* 1C24A: 20 95 D4 */  jsr DisableNMIandPPU                          ; $D495

/* 1C24D: A9 00 */     lda #$00
/* 1C24F: 85 47 */     sta GutsmanStompCounter
/* 1C251: 85 B4 */     sta UseTempScrollX
/* 1C253: 85 55 */     sta MegamanBlinkState
/* 1C255: 85 94 */     sta MegamanWalkTimer
/* 1C257: 85 95 */     sta MegamanStallTimer
/* 1C259: C6 A6 */     dec ExtraLives


/* 1C25B: A5 A6 */     lda ExtraLives  ; Branch if no lives left
/* 1C25D: 30 72 */     bmi L1C2D1 ; +++                                     ; $C2D1

/* 1C25F: 20 5B C7 */  jsr WriteChr                                ; $C75B
/* 1C262: A9 00 */     lda #$00
/* 1C264: 85 AB */     sta LastRestartPointType
/* 1C266: 85 59 */     sta $59

/* 1C268: AD 60 04 */  lda ObjectPosScreen+0                     ; $0460
/* 1C26B: A6 31 */     ldx CurrentStage
/* 1C26D: DD D4 C2 */  cmp StageCheckPointA,x
/* 1C270: 90 35 */     bcc L1C2A7 ; ++                                      ; $C2A7
/* 1C272: A0 0C */     ldy #$0C
/* 1C274: DD E0 C2 */  cmp StageCheckPointB,x
/* 1C277: 90 02 */     bcc L1C27B ; +                                       ; $C27B
/* 1C279: A0 18 */     ldy #$18
L1C27B: ; +
/* 1C27B: 98 */        tya
/* 1C27C: 85 AB */     sta LastRestartPointType
/* 1C27E: 18 */        clc
/* 1C27F: 65 31 */     adc CurrentStage
/* 1C281: AA */        tax
/* 1C282: BD EB C2 */  lda FirstScreenScreenTable,x
/* 1C285: 8D 60 04 */  sta ObjectPosScreen+0                     ; $0460
/* 1C288: 20 58 C6 */  jsr SetupEnemyGraphicsPointer
L1C28B: ; -
/* 1C28B: 20 C4 C6 */  jsr LoadEnemyGraphics
/* 1C28E: 20 AF D5 */  jsr DoRawPPUtransfer
/* 1C291: A5 59 */     lda $59
/* 1C293: C9 28 */     cmp #$28
/* 1C295: D0 F4 */     bne L1C28B ; -                                           ; $C28B

/* 1C297: 20 C4 C6 */  jsr LoadEnemyGraphics
/* 1C29A: A9 2C */     lda #$2C
/* 1C29C: 8D E1 03 */  sta SpritePalettes + $1
/* 1C29F: A9 11 */     lda #$11
/* 1C2A1: 8D E2 03 */  sta SpritePalettes + $2
/* 1C2A4: 20 0A D6 */  jsr UpdatePalettes                              ; $D60A

L1C2A7: ; ++
/* 1C2A7: A9 00 */     lda #$00
/* 1C2A9: 85 19 */     sta JoyD1
/* 1C2AB: A2 FF */     ldx #$FF
/* 1C2AD: 9A */        txs
/* 1C2AE: A9 05 */     lda #$05
/* 1C2B0: 85 42 */     sta CurrentBank
/* 1C2B2: 8D 05 C0 */  sta BankTable+5
/* 1C2B5: A5 AB */     lda LastRestartPointType
/* 1C2B7: C9 0C */     cmp #$0C
/* 1C2B9: B0 03 */     bcs L1C2BE ; +                                   ; $C2BE
L1C2BB: ; -
/* 1C2BB: 4C 7E 90 */  jmp StageBegin

L1C2BE: ; +
/* 1C2BE: A5 31 */     lda CurrentStage
/* 1C2C0: C9 07 */     cmp #$07
/* 1C2C2: F0 04 */     beq L1C2C8 ; +                                   ; $0001C2C8
/* 1C2C4: C9 09 */     cmp #$09
/* 1C2C6: D0 06 */     bne L1C2CE ; ++                                  ; $0001C2CE
L1C2C8: ; +
/* 1C2C8: A5 AB */     lda LastRestartPointType
/* 1C2CA: C9 0D */     cmp #$0D
/* 1C2CC: B0 ED */     bcs L1C2BB ; -                                   ; $0001C2BB
L1C2CE: ; ++
/* 1C2CE: 4C 81 90 */  jmp StageBeginFromDeath

;Game over
L1C2D1: ; +++
/* 1C2D1: 4C 38 90 */  jmp GameOver


; Table that says what screen# Megaman's current screen# must be above or
; equal to in order for him to restart from it...


; Cutman   #0
; Iceman   #1
; Bombman  #2
; Fireman  #3
; Elecman  #4
; Gutsman  #5


StageCheckPointA: ;at C2D4
    .byte $0A,$0A,$0E,$08,$07,$0A
    .byte $22,$24,$2F,$1D,$1D,$1D
StageCheckPointB:
    .byte $14,$14,$14,$10,$14,$0F
    .byte $27,$29,$2F,$21,$1C
FirstScreenScreenTable: ;at C2EB
; Initial screen start?
    .byte $00,$00,$00,$00,$00,$00
    .byte $18,$18,$18,$14,$18,$1E  ;Stored into ObjectPosScreen+0 or CurrentBeginScreen
; Restart point A
    .byte $0A,$0A,$0E,$08,$07,$0A
    .byte $22,$24,$2E,$1D,$1D,$1D
; Restart point B
    .byte $14,$14,$14,$10,$14,$0F
    .byte $26,$28,$2E,$21,$20

C30E_table:
    .byte $FB,$05,$00 ;X increment of some kind
C311_table:
    .byte $FF,$00,$00 ;screen increment of some kind
C314_table:
    .byte $F8,$F4,$06 ;Y increment of some kind


TimeDelayC317:
/* 1C317: A5 */        lda RefObjectNum
/* 1C319: D0 03 */     bne L1C31E ; +                                           ; $C31E
/* 1C31B: 20 8C DB */  jsr RunBossAI

L1C31E: ; +
/* 1C31E: 20 EA 98 */  jsr RunEnemyAI                                   ; $98EA
/* 1C321: 20 31 D1 */  jsr UpdateGraphics                              ; $D131
/* 1C324: 20 1B C0 */  jsr NextFrame                                   ; $C01B
/* 1C327: C6 3C */     dec MiscCounter1
/* 1C329: 60 */        rts


AtomicExplodeActor:  ;Ah, explosions probably!

/* 1C32A: A9 0C */     lda #$0C   ;How many atoms to create?
/* 1C32C: 85 3C */     sta MiscCounter1
L1C32E: ; -
/* 1C32E: A5 3C */     lda MiscCounter1
/* 1C330: 29 03 */     and #$03
/* 1C332: D0 43 */     bne L1C377 ; +                                           ; $C377
/* 1C334: A5 59 */     lda $59
/* 1C336: 85 2F */     sta RefObjectNum
/* 1C338: A2 02 */     ldx #$02
/* 1C33A: 20 76 C5 */  jsr FindFreeObject                              ; $C576
/* 1C33D: B0 38 */     bcs L1C377 ; +                                           ; $C377
/* 1C33F: 20 7B F6 */  jsr InitActor
/* 1C342: A5 5A */     lda $5A
/* 1C344: 9D 00 04 */  sta ObjectSpriteNum,x
/* 1C347: A9 08 */     lda #$08
/* 1C349: 9D 20 04 */  sta ObjectFlags,x
/* 1C34C: A9 10 */     lda #$10
/* 1C34E: 9D 40 04 */  sta ObjectUnknown440,x
/* 1C351: A5 3C */     lda MiscCounter1
/* 1C353: 4A */        lsr a
/* 1C354: 4A */        lsr a
/* 1C355: A8 */        tay

/* 1C356: BD 80 04 */  lda ObjectPosX,x
/* 1C359: 79 0D C3 */  adc C30E_table-1,y
/* 1C35C: 9D 80 04 */  sta ObjectPosX,x

/* 1C35F: BD 60 04 */  lda ObjectPosScreen,x
/* 1C362: 79 10 C3 */  adc C311_table-1,y
/* 1C365: 9D 60 04 */  sta ObjectPosScreen,x

/* 1C368: 18 */        clc
/* 1C369: BD 00 06 */  lda ObjectPosY,x
/* 1C36C: 79 13 C3 */  adc C314_table-1,y
/* 1C36F: 9D 00 06 */  sta ObjectPosY,x

/* 1C372: A0 08 */     ldy #$08
/* 1C374: 20 33 F5 */  jsr InitObjectDefaultSpeed
L1C377: ; +
/* 1C377: 20 17 C3 */  jsr TimeDelayC317
/* 1C37A: D0 B2 */     bne L1C32E ; -                                           ; $C32E

/* 1C37C: A9 0B */     lda #$0B
/* 1C37E: 85 0C */     sta $0C
/* 1C380: A5 5A */     lda $5A
/* 1C382: 85 0D */     sta $0D
/* 1C384: A9 00 */     lda #$00
/* 1C386: 85 0E */     sta $0E
L1C388: ; -
/* 1C388: A4 59 */     ldy $59
/* 1C38A: A2 02 */     ldx #$02
/* 1C38C: 20 76 C5 */  jsr FindFreeObject                              ; FindFreeObject
/* 1C38F: B0 05 */     bcs L1C396
/* 1C391: 20 41 F8 */  jsr CreateExplosionObject
/* 1C394: 10 F2 */     bpl L1C388 ; -

L1C396:
/* 1C396: A9 12 */     lda #$12        ; Explosion
/* 1C398: 20 77 C4 */  jsr IssueSound                              ; $C477
/* 1C39B: 60 */        rts


;
;
;

SwitchBankStage: ;doesn't modify P,A. Modifies X.
/* 1C39C: 08 */        php
/* 1C39D: 48 */        pha
/* 1C39E: 8A */        txa
/* 1C39F: 48 */        pha
/* 1C3A0: A5 31 */     lda CurrentStage

/* 1C3A2: C9 06 */     cmp #$06        ; bank=(stage%6)
/* 1C3A4: 90 02 */     bcc L1C3A8 ; +                                       ; $C3A8
/* 1C3A6: E9 06 */     sbc #$06
L1C3A8: ; +

/* 1C3A8: AA */        tax
/* 1C3A9: 85 42 */     sta CurrentBank
/* 1C3AB: 9D 00 C0 */  sta BankTable,x
/* 1C3AE: 68 */        pla
/* 1C3AF: AA */        tax
/* 1C3B0: 68 */        pla
/* 1C3B1: 28 */        plp
/* 1C3B2: 60 */        rts


SwitchBank05:
/* 1C3B3: 08 */        php
/* 1C3B4: 48 */        pha
/* 1C3B5: A9 05 */     lda #$05
/* 1C3B7: 85 42 */     sta CurrentBank
/* 1C3B9: 8D 05 C0 */  sta BankTable+5
/* 1C3BC: 68 */        pla
/* 1C3BD: 28 */        plp
/* 1C3BE: 60 */        rts


; Handle weapon select screen?
InvokeWeaponSelectDialog:
/* 1C3BF: A9 06 */     lda #$06
/* 1C3C1: 85 42 */     sta CurrentBank
/* 1C3C3: 8D 06 C0 */  sta BankTable+6

/* 1C3C6: A5 AC */     lda FightingBossNum
/* 1C3C8: C9 0A */     cmp #$0A
/* 1C3CA: F0 0E */     beq L1C3DA ; ++                                      ; $C3DA

/* 1C3CC: A5 31 */     lda CurrentStage
/* 1C3CE: C9 09 */     cmp #$09
/* 1C3D0: D0 04 */     bne L1C3D6 ; +                                       ; $C3D6
/* 1C3D2: C5 AC */     cmp FightingBossNum
/* 1C3D4: F0 04 */     beq L1C3DA ; ++                                      ; $C3DA

L1C3D6: ; +
/* 1C3D6: C9 06 */     cmp #$06
/* 1C3D8: D0 06 */     bne L1C3E0 ; +                                       ; $C3E0

L1C3DA: ; ++
/* 1C3DA: A5 3E */     lda BossCurrentStrategy
/* 1C3DC: F0 02 */     beq L1C3E0 ; +                                       ; $C3E0
/* 1C3DE: E6 1B */     inc ScrollPosScreen
L1C3E0: ; +
/* 1C3E0: 4C F3 BF */  jmp DoWeaponSelectDialog

; Arrived from bank 6
Lbl_c3e3:
/* 1C3E3: A5 AC */     lda FightingBossNum
/* 1C3E5: C9 0A */     cmp #$0A
/* 1C3E7: F0 0E */     beq L1C3F7 ; ++                                      ; $C3F7
/* 1C3E9: A5 31 */     lda CurrentStage
/* 1C3EB: C9 09 */     cmp #$09
/* 1C3ED: D0 04 */     bne L1C3F3 ; +                                       ; $C3F3
/* 1C3EF: C5 AC */     cmp FightingBossNum
/* 1C3F1: F0 04 */     beq L1C3F7 ; ++                                      ; $C3F7
L1C3F3: ; +
/* 1C3F3: C9 06 */     cmp #$06
/* 1C3F5: D0 06 */     bne L1C3FD ; +++                                     ; $C3FD
L1C3F7: ; ++
/* 1C3F7: A5 3E */     lda BossCurrentStrategy
/* 1C3F9: F0 02 */     beq L1C3FD ; +++                                     ; $C3FD
/* 1C3FB: C6 1A */     dec ScrollPosX
L1C3FD: ; +++
/* 1C3FD: 4C B3 C3 */  jmp SwitchBank05                            ; $C3B3


; Arrived from bank 6
F1C400:
/* 1C400: AA */        tax
/* 1C401: 85 42 */     sta CurrentBank
/* 1C403: 9D 00 C0 */  sta BankTable,x
/* 1C406: A2 00 */     ldx #$00
L1C408: ; -
/* 1C408: B1 06 */     lda (CurrentRoomPointer),y
/* 1C40A: 9D 82 03 */  sta RawPPUtransferBuf,x
/* 1C40D: C8 */        iny
/* 1C40E: E8 */        inx
/* 1C40F: E0 10 */     cpx #$10
/* 1C411: D0 F5 */     bne L1C408 ; -                                       ; $C408
SwitchBank6_a:
/* 1C413: A9 06 */     lda #$06
/* 1C415: 85 42 */     sta CurrentBank
/* 1C417: 8D 06 C0 */  sta BankTable+6
/* 1C41A: 60 */        rts

DrawBlockFromActiveLevelMap_Bank06callback:
/* 1C41B: 20 E8 CD */  jsr DrawBlockFromActiveLevelMap
SwitchBank6_b:
/* 1C41E: A9 06 */     lda #$06
/* 1C420: 85 42 */     sta CurrentBank
/* 1C422: 8D 06 C0 */  sta BankTable+6
/* 1C425: 60 */        rts


InitEndGameScene:
/* 1C426: A9 06 */     lda #$06
/* 1C428: 85 42 */     sta CurrentBank
/* 1C42A: 8D 06 C0 */  sta BankTable+6
/* 1C42D: 4C F0 BF */  jmp Lbl_bff0

TeleportToStage_Bank06callback:
/* 1C430: 20 E0 C4 */  jsr TeleportToStage                             ; $C4E0
/* 1C433: 4C 56 C4 */  jmp SwitchBank06_c

F1C436:
/* 1C436: 4C 7B F6 */  jmp InitActor

F1C439:
/* 1C439: 20 B3 C3 */  jsr SwitchBank05                                ; $C3B3
/* 1C43C: 20 6D 9E */  jsr ObjectUpdateMovementLeft
/* 1C43F: 20 8F 98 */  jsr AutoCenterScreen
F1C442:
/* 1C442: 20 31 D1 */  jsr UpdateGraphics                              ; $D131
/* 1C445: 20 1B C0 */  jsr NextFrame                                   ; $C01B
/* 1C448: E6 BD */     inc $BD
/* 1C44A: A5 BD */     lda $BD
/* 1C44C: C9 3E */     cmp #$3E
/* 1C44E: D0 06 */     bne SwitchBank06_c
/* 1C450: E6 BE */     inc $BE
/* 1C452: A9 00 */     lda #$00
/* 1C454: 85 BD */     sta $BD
SwitchBank06_c:
/* 1C456: A9 06 */     lda #$06
/* 1C458: 85 42 */     sta CurrentBank
/* 1C45A: 8D 06 C0 */  sta BankTable+6
/* 1C45D: 60 */        rts

Lbl_c45e:
; reset the current stage number?
/* 1C45E: A9 00 */     lda #$00
/* 1C460: 85 31 */     sta CurrentStage
/* 1C462: 4C 0C C1 */  jmp L1C10C



; inc $6A0 for every object except Megaman

LifeCycleTick_forEnemies:
/* 1C465: A2 01 */     ldx #$01
L1C467: ; -
/* 1C467: BD 00 06 */  lda ObjectPosY,x
/* 1C46A: C9 F8 */     cmp #$F8
/* 1C46C: F0 03 */     beq L1C471 ; +                                           ; $C471
/* 1C46E: FE A0 06 */  inc ObjectLifeCycleCounter,x
L1C471: ; +
/* 1C471: E8 */        inx
/* 1C472: E0 20 */     cpx #$20
/* 1C474: D0 F1 */     bne L1C467 ; -                                           ; $C467
/* 1C476: 60 */        rts


;
; Adds the sound with sound ID = A to the list of sounds to be issued
;

IssueSound:
/* 1C477: A6 45 */     ldx NumIssuedSounds     ; Max 16 sounds in a frame
/* 1C479: E0 10 */     cpx #$10
/* 1C47B: F0 05 */     beq L1C482 ; +                                   ; $0001C482
/* 1C47D: 9D 80 05 */  sta IssuedSoundsList,x
/* 1C480: E6 45 */     inc NumIssuedSounds
L1C482: ; +
/* 1C482: 60 */        rts


InitStagePaletteAndActives:
/* 1C483: 20 9C C3 */  jsr SwitchBankStage                     ; $C39C
/* 1C486: A5 31 */     lda CurrentStage
/* 1C488: C9 06 */     cmp #$06
/* 1C48A: 90 0B */     bcc L1C497 ; +                                   ; $C497
/* 1C48C: A9 D0 */     lda #<RoomSpritePalettes2
/* 1C48E: 85 35 */     sta $35
/* 1C490: A9 8C */     lda #(>RoomSpritePalettes2)
/* 1C492: 85 36 */     sta $36
/* 1C494: 4C 9F C4 */  jmp L1C49F ; ++                                  ; $C49F
L1C497: ; +
/* 1C497: A9 A0 */     lda #<RoomSpritePalettes1
/* 1C499: 85 35 */     sta $35
/* 1C49B: A9 8C */     lda #>RoomSpritePalettes1
/* 1C49D: 85 36 */     sta $36
L1C49F: ; ++

; Write both BG and sprite palette
/* 1C49F: A9 20 */     lda #$20
/* 1C4A1: 20 47 D6 */  jsr WritePalette                                ; $D647
;
; Copies palette into RAM for continous updates? (only affects BG palette)
;
/* 1C4A4: A0 2F */     ldy #$2F
L1C4A6: ; -
/* 1C4A6: B1 35 */     lda ($35),y
/* 1C4A8: 99 D0 03 */  sta BGPalettes,y
/* 1C4AB: 88 */        dey
/* 1C4AC: 10 F8 */     bpl L1C4A6 ; -                                           ; $C4A6

/* 1C4AE: A5 31 */     lda CurrentStage
/* 1C4B0: C9 06 */     cmp #$06
/* 1C4B2: 90 0A */     bcc L1C4BE ; ++                                          ; $C4BE
    ; 8E9F (8EA0-1)
/* 1C4B4: A9 9F */     lda #<(RoomActives2-1)
/* 1C4B6: 85 04 */     sta $04
/* 1C4B8: A9 8E */     lda #>(RoomActives2-1)
/* 1C4BA: 85 05 */     sta $05
/* 1C4BC: D0 08 */     bne L1C4C6 ; +                                           ; $C4C6

L1C4BE: ; ++  ; 8DFF (8E00-1)
/* 1C4BE: A9 FF */     lda #<(RoomActives1-1)
/* 1C4C0: 85 04 */     sta $04
/* 1C4C2: A9 8D */     lda #>(RoomActives1-1)
/* 1C4C4: 85 05 */     sta $05

L1C4C6: ; +
/* 1C4C6: A0 01 */     ldy #$01
/* 1C4C8: B1 04 */     lda ($04),y
/* 1C4CA: 0A */        asl a
/* 1C4CB: 8D 20 07 */  sta RoomActiveTable     ; tmp = count*2
/* 1C4CE: 0A */        asl a
/* 1C4CF: 18 */        clc
/* 1C4D0: 6D 20 07 */  adc RoomActiveTable     ; A = tmp + count*4  (count*6)
/* 1C4D3: A8 */        tay
/* 1C4D4: C8 */        iny

L1C4D5: ; -
/* 1C4D5: B1 04 */     lda ($04),y
/* 1C4D7: 99 1F 07 */  sta RoomActiveTable-1,y
/* 1C4DA: 88 */        dey
/* 1C4DB: D0 F8 */     bne L1C4D5 ; -                                           ; $C4D5

/* 1C4DD: 4C B3 C3 */  jmp SwitchBank05                                ; $C3B3


;
; This routine handles the "teleportation" of Megaman, i.e. when he sweeps
; down to the stage from the sky after the "READY" message
;

TeleportToStage:
/* 1C4E0: 18 */        clc
/* 1C4E1: A5 31 */     lda CurrentStage
/* 1C4E3: 65 AB */     adc LastRestartPointType
/* 1C4E5: AA */        tax

; Let megaman transform
/* 1C4E6: A9 14 */     lda #$14
/* 1C4E8: 8D 00 04 */  sta ObjectSpriteNum+0                                ; $0400
/* 1C4EB: A9 41 */     lda #$41
/* 1C4ED: 8D 20 04 */  sta ObjectFlags+0

; Move Megaman down 16 pixels/frame until his Y position equals starting spot

/* 1C4F0: BD 1A C5 */  lda TeleportYcoord,x
/* 1C4F3: 85 59 */     sta $59
L1C4F5: ; -
/* 1C4F5: A2 00 */     ldx #$00
/* 1C4F7: 8E 40 04 */  stx ObjectUnknown440+0

/* 1C4FA: 18 */        clc
/* 1C4FB: AD 00 06 */  lda ObjectPosY+0                                ; $0600
/* 1C4FE: 69 10 */     adc #$10
/* 1C500: 8D 00 06 */  sta ObjectPosY+0                                 ; ObjectPosY+0

/* 1C503: 20 31 D1 */  jsr UpdateGraphics                              ; $D131

/* 1C506: 20 1B C0 */  jsr NextFrame                                  ; $C01B
/* 1C509: AD 00 06 */  lda ObjectPosY+0                                ; ObjectPosY+0
/* 1C50C: A6 31 */     ldx CurrentStage
/* 1C50E: C5 59 */     cmp $59 ; Until the Y coord matches.
/* 1C510: D0 E3 */     bne L1C4F5 ; -                                           ; $C4F5


/* 1C512: A9 00 */     lda #$00
/* 1C514: 8D 80 06 */  sta ObjectYSpeed+0
/* 1C517: 4C B3 C3 */  jmp SwitchBank05                                ; $C3B3


;At C51A
TeleportYcoord:
; Start of stage
    .byte $94,$B4,$B4,$B4,$C4,$B4
    .byte $B4,$74,$B4,$94,$84,$64
; Restart point A
    .byte $94,$C4,$B4,$94,$94,$54
    .byte $24,$64,$B4,$B4,$B4,$B4
; Restart point B
    .byte $94,$94,$94,$94,$94,$94
    .byte $B4,$94,$B4,$B4,$B4


C53D_routine:
/* 1C53D: A9 06 */     lda #$06
/* 1C53F: 85 */        sta CurrentBank
/* 1C541: 8D 06 C0 */  sta BankTable+6

/* 1C544: BD 00 04 */  lda ObjectSpriteNum,x
/* 1C547: A0 06 */     ldy #$06
L1C549: ; -
/* 1C549: D9 44 86 */  cmp Lbl_8644,y
/* 1C54C: F0 08 */     beq L1C556 ; +                                           ; $C556
/* 1C54E: 88 */        dey
/* 1C54F: 10 F8 */     bpl L1C549 ; -                                           ; $C549

/* 1C551: A0 00 */     ldy #$00
/* 1C553: 4C 64 C5 */  jmp L1C564 ; ++                                          ; $C564
L1C556: ; +
/* 1C556: B9 4A 86 */  lda Lbl_864a,y
/* 1C559: A8 */        tay
/* 1C55A: 4C 64 C5 */  jmp L1C564 ; ++                                          ; $C564
F1C55D:
/* 1C55D: A9 06 */     lda #$06
/* 1C55F: 85 42 */     sta CurrentBank
/* 1C561: 8D 06 C0 */  sta BankTable+6
L1C564: ; ++
/* 1C564: B9 3A 86 */  lda Lbl_863a,y
/* 1C567: 48 */        pha
/* 1C568: 29 F0 */      and #$F0
/* 1C56A: 9D E0 04 */   sta ObjectXSpeedFraction,x
/* 1C56D: 68 */        pla
/* 1C56E: 29 0F */     and #$0F
/* 1C570: 9D C0 04 */  sta ObjectXSpeed,x
/* 1C573: 4C B3 C3 */  jmp SwitchBank05                                ; $C3B3



; This routine clears carry if #$F8 is found between $600+X and $600+TotalObjects
; and sets it otherwise

; C = 0 if found, 1 if not

FindFreeObject:
/* 1C576: A9 F8 */     lda #$F8
L1C578: ; -
/* 1C578: DD 00 06 */  cmp ObjectPosY,x
/* 1C57B: F0 07 */     beq L1C584 ; +                                           ; $C584
/* 1C57D: E8 */        inx
/* 1C57E: E4 54 */     cpx TotalObjects
/* 1C580: D0 F6 */     bne L1C578 ; -                                           ; $C578
/* 1C582: 38 */        sec
/* 1C583: 60 */        rts
L1C584: ; +
/* 1C584: 18 */        clc
/* 1C585: 60 */        rts


FindLastObjectOfType:
L1C586: ; -
/* 1C586: DD 00 04 */  cmp ObjectSpriteNum,x
/* 1C589: F0 07 */     beq L1C592 ; +                                           ; $C592
L1C58B: ; --
/* 1C58B: E8 */        inx
/* 1C58C: E4 54 */     cpx TotalObjects
/* 1C58E: D0 F6 */     bne L1C586 ; -
/* 1C590: 38 */        sec
/* 1C591: 60 */        rts
L1C592: ; +
/* 1C592: A9 F8 */     lda #$F8
/* 1C594: DD 00 06 */  cmp ObjectPosY,x
/* 1C597: 08 */        php
/* 1C598: BD 00 04 */   lda ObjectSpriteNum,x
/* 1C59B: 28 */        plp
/* 1C59C: F0 ED */     beq L1C58B ; --                                          ; $C58B
/* 1C59E: 18 */        clc
/* 1C59F: 60 */        rts



; Random function
;   A = max value
RandomFunc: ;In Megaman2, C84E (and it's much different)
/* 1C5A0: 85 40 */     sta $40
/* 1C5A2: A5 46 */     lda RandomSeed
/* 1C5A4: 38 */        sec
L1C5A5: ; -
/* 1C5A5: E5 40 */     sbc $40
/* 1C5A7: B0 FC */     bcs L1C5A5 ; -                                           ; $C5A5
/* 1C5A9: 65 40 */     adc $40
/* 1C5AB: 60 */        rts


EnemyCalculateJumpCurveToHitMegaman:
; It does some math (input: A,X,Y); output: $04,$05
; Uses $4A,$4B,$4C,$4D as temps
/* 1C5AC: 85 4D */     sta $4D
/* 1C5AE: 86 4B */     stx $4B
/* 1C5B0: 84 4C */     sty $4C
/* 1C5B2: A9 00 */     lda #$00
/* 1C5B4: 85 4A */     sta $4A
/* 1C5B6: A0 10 */     ldy #$10
L1C5B8: ; -
/* 1C5B8: 06 4C */     asl $4C
/* 1C5BA: 26 4B */     rol $4B
/* 1C5BC: 26 4A */     rol $4A
/* 1C5BE: A5 4A */     lda $4A
/* 1C5C0: 38 */        sec
/* 1C5C1: E5 4D */     sbc $4D
/* 1C5C3: 90 04 */     bcc L1C5C9 ; +                                       ; $C5C9
/* 1C5C5: 85 4A */     sta $4A
/* 1C5C7: E6 4C */     inc $4C
L1C5C9: ; +
/* 1C5C9: 88 */        dey
/* 1C5CA: D0 EC */     bne L1C5B8 ; -                                       ; $C5B8
/* 1C5CC: A5 4B */     lda $4B
/* 1C5CE: 85 05 */     sta $05
/* 1C5D0: A5 4C */     lda $4C
/* 1C5D2: 85 04 */     sta $04
/* 1C5D4: 60 */        rts



F1C5D5:
/* 1C5D5: A5 05 */     lda $05
/* 1C5D7: 85 4B */     sta $4B
/* 1C5D9: A5 04 */     lda $04
/* 1C5DB: 85 4C */     sta $4C
/* 1C5DD: A5 07 */     lda CurrentRoomPointer+1
/* 1C5DF: 85 4E */     sta $4E
/* 1C5E1: A5 06 */     lda CurrentRoomPointer
/* 1C5E3: 85 11 */     sta $11
/* 1C5E5: A9 00 */     lda #$00
/* 1C5E7: 85 4A */     sta $4A
/* 1C5E9: 85 4D */     sta $4D
/* 1C5EB: A0 10 */     ldy #$10

L1C5ED: ; -
/* 1C5ED: 06 4D */     asl $4D
/* 1C5EF: 26 4C */     rol $4C
/* 1C5F1: 26 4B */     rol $4B
/* 1C5F3: 26 4A */     rol $4A
/* 1C5F5: 38 */        sec
/* 1C5F6: A5 4B */     lda $4B
/* 1C5F8: E5 4F */     sbc $4F
/* 1C5FA: AA */        tax
/* 1C5FB: A5 4A */     lda $4A
/* 1C5FD: E5 4E */     sbc $4E
/* 1C5FF: 90 06 */     bcc L1C607 ; +                                       ; $C607
/* 1C601: 86 4B */     stx $4B
/* 1C603: 85 4A */     sta $4A
/* 1C605: E6 4D */     inc $4D
L1C607: ; +
/* 1C607: 88 */        dey
/* 1C608: D0 E3 */     bne L1C5ED ; -                                       ; $C5ED

/* 1C60A: A5 4D */     lda $4D
/* 1C60C: 85 08 */     sta CurrentRoomPointer+2
/* 1C60E: A5 4C */     lda $4C
/* 1C610: 85 09 */     sta CurrentRoomPointer+3
/* 1C612: 60 */        rts

RoomLayoutLoadRoomNum:
/* 1C613: 20 9C C3 */  jsr SwitchBankStage                         ; $C39C
/* 1C616: C0 00 */     cpy #$00
/* 1C618: 30 06 */     bmi L1C620 ; +                                       ; $C620
/* 1C61A: B9 71 8C */  lda RoomLayoutTable+1,y
/* 1C61D: 4C B3 C3 */  jmp SwitchBank05                            ; $C3B3
L1C620: ; +
/* 1C620: A9 00 */     lda #$00
/* 1C622: 4C B3 C3 */  jmp SwitchBank05                                ; $C3B3

LoadActiveByIndexAndSetBlockingness:
; Input: A = index into the active table.
/* 1C625: 20 9C C3 */  jsr SwitchBankStage                             ; $C39C
/* 1C628: AA */        tax
/* 1C629: A5 0C */     lda $0C
/* 1C62B: 9D 21 07 */  sta RoomActiveTable+1,x ;type
/* 1C62E: BD 23 07 */  lda RoomActiveTable+3,x ;x1
/* 1C631: 09 08 */     ora #$08
/* 1C633: 85 0C */     sta $0C
/* 1C635: BD 24 07 */  lda RoomActiveTable+4,x ;y1
/* 1C638: 09 0B */     ora #$0B
/* 1C63A: 85 0D */     sta $0D
/* 1C63C: 4C B3 C3 */  jmp SwitchBank05                                ; $C3B3

F1C63F:
/* 1C63F: 20 9C C3 */  jsr SwitchBankStage                             ; $C39C
/* 1C642: A4 0C */     ldy $0C
/* 1C644: 99 24 07 */  sta RoomActiveTable+4,y
/* 1C647: 4C B3 C3 */  jmp SwitchBank05                                ; $C3B3


F1C64A:
/* 1C64A: 20 9C C3 */  jsr SwitchBankStage                             ; $C39C
/* 1C64D: A4 0C */     ldy $0C
/* 1C64F: B9 26 07 */  lda RoomActiveTable+6,y
/* 1C652: 99 24 07 */  sta RoomActiveTable+4,y
/* 1C655: 4C B3 C3 */  jmp SwitchBank05                                ; $C3B3

SetupEnemyGraphicsPointer:
/* 1C658: 20 9C C3 */  jsr SwitchBankStage                             ; $C39C
/* 1C65B: AE 60 04 */  ldx ObjectPosScreen+0                         ; $0460
/* 1C65E: BD 40 8D */  lda RoomMonsterIndex,x
/* 1C661: 85 7A */     sta CurrentRoomMonsterGraphicsIndex ; Only read at C6E8ö
/* 1C663: 4C B3 C3 */  jmp SwitchBank05                                ; $C3B3

SpawnObject_TypeIsFF:
;
; Input:
;   $00 = posx, $01 = posy, $05 = screen number
;
; Objects of type FF are actually calls to remap pattern tables of PPU.
; The Y position is an index into ObjectFF_defs, which tells which tiles
; will be redefined.
;
/* 1C666: A5 42 */     lda CurrentBank
/* 1C668: 48 */        pha
/* 1C669: A5 92 */      lda AutoSpawnObjectFFcounter
/* 1C66B: D0 08 */      bne L1C675 ; +                                           ; $C675
/* 1C66D: A6 01 */       ldx $01
/* 1C66F: 86 93 */       stx $93
/* 1C671: A9 10 */       lda #$10
/* 1C673: 85 92 */       sta AutoSpawnObjectFFcounter
L1C675: ; +
/* 1C675: A5 04 */      lda $04
/* 1C677: 48 */         pha
/* 1C678: A5 05 */       lda $05
/* 1C67A: 48 */          pha
/* 1C67B: C6 92 */        dec AutoSpawnObjectFFcounter
/* 1C67D: A6 93 */        ldx $93

/* 1C67F: BD B0 C6 */     lda ObjectFF_defs+0,x
/* 1C682: 8D 80 03 */     sta RawPPUtransferAddress+0

/* 1C685: BD B1 C6 */     lda ObjectFF_defs+1,x
/* 1C688: 20 AE C7 */     jsr SwitchBankTile                                   ; $C7AE

/* 1C68B: A5 92 */        lda AutoSpawnObjectFFcounter
/* 1C68D: 0A */           asl a
/* 1C68E: 0A */           asl a
/* 1C68F: 0A */           asl a
/* 1C690: 0A */           asl a
/* 1C691: 85 04 */        sta $04
/* 1C693: 8D 81 03 */     sta RawPPUtransferAddress+1

/* 1C696: A0 0F */        ldy #$0F
L1C698: ; -
/* 1C698: B1 04 */         lda ($04),y
/* 1C69A: 99 82 03 */      sta RawPPUtransferBuf,y
/* 1C69D: 88 */            dey
/* 1C69E: 10 F8 */        bpl L1C698 ; -                                           ; $C698

/* 1C6A0: A9 10 */        lda #$10
/* 1C6A2: 85 5E */        sta RawPPUtransferSize
/* 1C6A4: 68 */          pla
/* 1C6A5: 85 05 */       sta $05
/* 1C6A7: 68 */         pla
/* 1C6A8: 85 04 */      sta $04
/* 1C6AA: 68 */        pla
/* 1C6AB: AA */        tax
/* 1C6AC: 9D 00 C0 */  sta BankTable,x ; switch back to the original bank
/* 1C6AF: 60 */        rts



ObjectFF_defs: ;at C6B0
    ; The first byte is the sprite pattern table index to change,
    ; the second byte is the sprite address (as used by SwitchBankTile).
    .byte $08,$6E ; block $1B at page 2 (00- used in Iceman water-end - loads enemy [1] 6E) (Blader etc)
    .byte $08,$A2 ; block $28 at page 2 (02- used in Iceman water-start-loads enemy [1] A2) (HeadLosingRobot)
    .byte $09,$82 ; block $20 at page 2 (04- used in Gutsman screen 6 - loads enemy [2] 82) (Axe-throwers)
    .byte $0A,$86 ; block $21 at page 2 (06- used in Gutsman screen 6 - loads enemy [3] 86) (Axe-throwers)
    .byte $09,$AE ; block $2B at page 2 (08- used in Gutsman screen 5 - loads enemy [2] AE) (Lift data)
    .byte $0A,$B2 ; block $2C at page 2 (0A- used in Gutsman screen 5 - loads enemy [3] B2) (Lift data)
    .byte $0B,$62 ; block $18 at page 2 (0C- used in Wily3 - loads enemy [4] 62)
    .byte $00,$FA ; block $3E at page 2 used in Ending. Elvis hair.
    .byte $01,$CE ; block $33 at page 2 used in Ending. Elvis running.
    .byte $02,$D2 ; block $34 at page 2 used in Ending. Elvis running.


LoadEnemyGraphics:
/* 1C6C4: A5 59 */     lda $59
/* 1C6C6: C9 29 */     cmp #$29
/* 1C6C8: D0 01 */     bne L1C6CB ; +
/* 1C6CA: 60 */        rts
L1C6CB: ; +
/* 1C6CB: E6 59 */     inc $59
/* 1C6CD: 20 9C C3 */  jsr SwitchBankStage                             ; $C39C
/* 1C6D0: 4A */        lsr a
/* 1C6D1: 6E 81 03 */  ror RawPPUtransferAddress+1
/* 1C6D4: 4A */        lsr a
/* 1C6D5: 6E 81 03 */  ror RawPPUtransferAddress+1
/* 1C6D8: 4A */        lsr a
/* 1C6D9: 6E 81 03 */  ror RawPPUtransferAddress+1
/* 1C6DC: 48 */        pha
/* 1C6DD: 18 */         clc
/* 1C6DE: 69 08 */      adc #$08
/* 1C6E0: 8D 80 03 */   sta RawPPUtransferAddress+0
/* 1C6E3: 68 */        pla
/* 1C6E4: C9 05 */     cmp #$05
/* 1C6E6: 08 */        php
/* 1C6E7: 18 */         clc
/* 1C6E8: 65 7A */      adc CurrentRoomMonsterGraphicsIndex  ;Only written to in C661, loaded from RoomMonsterIndex.
/* 1C6EA: AA */         tax
/* 1C6EB: 28 */        plp
/* 1C6EC: F0 21 */     beq L1C70F ; +                                           ; $C70F
/* 1C6EE: BD 80 8D */  lda RoomMonsterGraphics,x
; Load bank.

/* 1C6F1: 20 AE C7 */  jsr SwitchBankTile                                   ; $C7AE
/* 1C6F4: AD 81 03 */  lda RawPPUtransferAddress+1
/* 1C6F7: 29 E0 */     and #$E0
/* 1C6F9: 8D 81 03 */  sta RawPPUtransferAddress+1
/* 1C6FC: 85 04 */     sta $04
/* 1C6FE: A0 1F */     ldy #$1F
L1C700: ; -
/* 1C700: B1 04 */     lda ($04),y
/* 1C702: 99 82 03 */  sta RawPPUtransferBuf,y
/* 1C705: 88 */        dey
/* 1C706: 10 F8 */     bpl L1C700 ; -                                           ; $C700

/* 1C708: A9 20 */     lda #$20
/* 1C70A: 85 5E */     sta RawPPUtransferSize
/* 1C70C: 4C B3 C3 */  jmp SwitchBank05                                ; $C3B3

L1C70F: ; +
/* 1C70F: A0 00 */     ldy #$00
        ; Copies 6 bytes of palette.
        ; Y = 0,1,2,4,5,6 (skips 3)
L1C711: ; -
/* 1C711: BD 80 8D */  lda RoomMonsterGraphics,x
/* 1C714: 99 E9 03 */  sta SpritePalettes + $9,y
/* 1C717: E8 */        inx
/* 1C718: C8 */        iny
/* 1C719: C0 07 */     cpy #$07
/* 1C71B: F0 07 */     beq L1C724 ; +                                           ; $C724
/* 1C71D: C0 03 */     cpy #$03
/* 1C71F: D0 F0 */     bne L1C711 ; -                                           ; $C711
/* 1C721: C8 */        iny
/* 1C722: D0 ED */     bne L1C711 ; -                                           ; $C711
L1C724: ; +
/* 1C724: 20 2A C7 */  jsr PaletteSetupForSprites
/* 1C727: 4C B3 C3 */  jmp SwitchBank05                                ; $C3B3

PaletteSetupForSprites:
/* 1C72A: A9 03 */     lda #>SpritePalettes
/* 1C72C: 85 39 */     sta $39
/* 1C72E: 85 3B */     sta $3B
/* 1C730: A9 E0 */     lda #<SpritePalettes
/* 1C732: 85 38 */     sta $38
/* 1C734: 85 3A */     sta $3A
/* 1C736: A9 18 */     lda #$18 ; Setup 1 sprite palette
L1C738: ; -
/* 1C738: 85 34 */     sta WritePaletteParam
/* 1C73A: A9 01 */     lda #$01
/* 1C73C: 85 37 */     sta PaletteUpdateDelay
/* 1C73E: 60 */        rts
PaletteSetupForBG:
/* 1C73F: A9 03 */     lda #>BGPalettes
/* 1C741: 85 39 */     sta $39
/* 1C743: 85 3B */     sta $3B
/* 1C745: A9 D0 */     lda #<BGPalettes
/* 1C747: 85 38 */     sta $38
/* 1C749: 85 3A */     sta $3A
/* 1C74B: A9 10 */     lda #$10 ; Setup 1 BG palette
/* 1C74D: D0 E9 */     bne L1C738   ; unconditional jump to C738

PaletteSetupForBGwith3F0:
/* 1C74F: 20 3F C7 */  jsr PaletteSetupForBG
/* 1C752: A9 03 */     lda #>UnknownPalettes
/* 1C754: 85 39 */     sta $39
/* 1C756: A9 F0 */     lda #<UnknownPalettes
/* 1C758: 85 38 */     sta $38
/* 1C75A: 60 */        rts



; This routine writes the sprite chr
; It is initialized in the beginning of each stage / forcescrolling

WriteChr:
/* 1C75B: 20 9C C3 */  jsr SwitchBankStage                             ; $C39C

/* 1C75E: A5 31 */     lda CurrentStage
/* 1C760: C9 06 */     cmp #$06
/* 1C762: 90 05 */     bcc L1C769 ; +                                           ; $C769
/* 1C764: AD 20 8D */  lda RoomTileTable2
/* 1C767: D0 03 */     bne L1C76C ; ++                                          ; $C76C
L1C769: ; +
/* 1C769: AD 00 8D */  lda RoomTileTable1
L1C76C: ; ++
/* 1C76C: 85 0C */     sta $0C ;Number of tiles in the tile table.

/* 1C76E: A9 00 */     lda #$00
/* 1C770: 8D 06 20 */  sta $2006
/* 1C773: 8D 06 20 */  sta $2006
/* 1C776: 85 0D */     sta $0D

L1C778: ; ---
/* 1C778: 20 9C C3 */  jsr SwitchBankStage                             ; $C39C

/* 1C77B: A4 0D */     ldy $0D

/* 1C77D: A5 31 */     lda CurrentStage
/* 1C77F: C9 06 */     cmp #$06
/* 1C781: 90 08 */     bcc L1C78B ; ++                                          ; $C78B
/* 1C783: B9 21 8D */  lda RoomTileTable2+1,y ; tile bank & index
/* 1C786: BE 22 8D */  ldx RoomTileTable2+2,y ; number of 256-bytes to copy
/* 1C789: D0 06 */     bne L1C791 ; +                                           ; $C791
L1C78B: ; ++
/* 1C78B: BE 02 8D */  ldx RoomTileTable1+2,y ; number of 256-bytes to copy
/* 1C78E: B9 01 8D */  lda RoomTileTable1+1,y ; tile bank & index
L1C791: ; +
/* 1C791: 20 AE C7 */  jsr SwitchBankTile                                   ; $C7AE
L1C794: ; --
/* 1C794: A0 00 */     ldy #$00        ; Copy 256 bytes to VRAM
L1C796: ; -
/* 1C796: B1 04 */     lda ($04),y
/* 1C798: 8D 07 20 */  sta $2007
/* 1C79B: C8 */        iny
/* 1C79C: D0 F8 */     bne L1C796 ; -                                           ; $C796

/* 1C79E: E6 05 */     inc $05
/* 1C7A0: CA */        dex
/* 1C7A1: D0 F1 */     bne L1C794 ; --  ;more 256-bytes?

/* 1C7A3: E6 0D */     inc $0D
/* 1C7A5: E6 0D */     inc $0D
/* 1C7A7: C6 0C */     dec $0C
/* 1C7A9: D0 CD */     bne L1C778 ; --- ; more tile setups?

/* 1C7AB: 4C B3 C3 */  jmp SwitchBank05                                ; $C3B3


; SwitchBankTile.
;  Input:
;     A:
;       low 2 bits = bank number
;       high 6 bits = block offset (16..63)
;  Output:
;     Bank switched
;     $04,$05 = pointer to the blockdata (0x8000 + (blocknumber)*0x100, 0x9000..0xBF00)
SwitchBankTile:
/* 1C7AE: 48 */        pha
/* 1C7AF: 29 FC */     and #$FC
/* 1C7B1: 4A */        lsr a
/* 1C7B2: 4A */        lsr a
/* 1C7B3: 09 80 */     ora #>RoomBlockData
/* 1C7B5: 85 05 */     sta $05
/* 1C7B7: A9 00 */     lda #<RoomBlockData
/* 1C7B9: 85 04 */     sta $04
/* 1C7BB: 68 */        pla

/* 1C7BC: 29 03 */     and #$03
/* 1C7BE: A8 */        tay
/* 1C7BF: 85 42 */     sta CurrentBank
/* 1C7C1: 99 00 C0 */  sta BankTable,y
/* 1C7C4: 60 */        rts



F1C7C5:
/* 1C7C5: A9 4C */     lda #$4C
/* 1C7C7: 85 0C */     sta $0C
/* 1C7C9: A9 00 */     lda #$00
/* 1C7CB: 85 0D */     sta $0D
/* 1C7CD: A9 08 */     lda #$08
/* 1C7CF: 85 0E */     sta $0E
L1C7D1: ; -
/* 1C7D1: A2 02 */     ldx #$02
/* 1C7D3: A5 0C */     lda $0C
/* 1C7D5: 20 86 C5 */  jsr FindLastObjectOfType
/* 1C7D8: B0 03 */     bcs L1C7DD ; +                                           ; $C7DD
/* 1C7DA: 20 FA C7 */  jsr C7FA_routine
L1C7DD: ; +
/* 1C7DD: E6 0C */     inc $0C
/* 1C7DF: C6 0E */     dec $0E
/* 1C7E1: D0 EE */     bne L1C7D1 ; -                                           ; $C7D1
/* 1C7E3: 60 */        rts



C7E4_routine:
/* 1C7E4: A2 05 */     ldx #$05
/* 1C7E6: A9 00 */     lda #$00
/* 1C7E8: 85 0D */     sta $0D
L1C7EA: ; -
/* 1C7EA: BD 00 06 */  lda ObjectPosY,x
/* 1C7ED: C9 F8 */     cmp #$F8
/* 1C7EF: F0 03 */     beq L1C7F4 ; +                                           ; $C7F4
/* 1C7F1: 20 FA C7 */  jsr C7FA_routine
L1C7F4: ; +
/* 1C7F4: E8 */        inx
/* 1C7F5: E0 0A */     cpx #$0A
/* 1C7F7: D0 F1 */     bne L1C7EA ; -                                           ; $C7EA
/* 1C7F9: 60 */        rts


C7FA_routine:
/* 1C7FA: BD 20 04 */  lda ObjectFlags,x     ; Flip direction of object (and?...)
/* 1C7FD: 49 40 */     eor #$40
/* 1C7FF: 29 F7 */     and #$F7
/* 1C801: 9D 20 04 */  sta ObjectFlags,x

/* 1C804: A4 0D */     ldy $0D

/* 1C806: B9 24 C8 */  lda C824_table,y
/* 1C809: 9D 60 06 */  sta ObjectYSpeedFraction,x
/* 1C80C: B9 29 C8 */  lda C829_table,y
/* 1C80F: 9D 80 06 */  sta ObjectYSpeed,x

/* 1C812: B9 2E C8 */  lda C82E_table,y
/* 1C815: 48 */        pha
/* 1C816: 29 F0 */      and #$F0
/* 1C818: 9D E0 04 */   sta ObjectXSpeedFraction,x
/* 1C81B: 68 */        pla

/* 1C81C: 29 0F */     and #$0F
/* 1C81E: 9D C0 04 */  sta ObjectXSpeed,x
/* 1C821: E6 0D */     inc $0D
/* 1C823: 60 */        rts


C824_table: ; gives values for ObjectYSpeedFraction
    .byte $54,$80,$00,$80,$AC
C829_table:
    .byte $04,$02,$00,$BD,$FB
C82E_table: ; - gives values for ObjectXSpeed and ObjectXSpeedFraction
    .byte $82,$54,$05,$54,$82

;
; A = Type of item in some way... (#$42 = Magnet Beam)
;

GotItem:
; Bonus items range is #$3C - #$46
/* 1C833: 38 */        sec
/* 1C834: E9 3C */     sbc #$3C
/* 1C836: 0A */        asl a
/* 1C837: A8 */        tay

/* 1C838: A9 F8 */     lda #$F8    ; Remove the object from the objects list
/* 1C83A: 9D 00 06 */  sta ObjectPosY,x

; Lookup jumptable
/* 1C83D: B9 9E C8 */  lda GotItemJumpTable,y                          ; $C89E,Y
/* 1C840: 85 04 */     sta $04
/* 1C842: B9 9F C8 */  lda GotItemJumpTable+1,y                        ; $C89F,y
/* 1C845: 85 05 */     sta $05
/* 1C847: 6C 04 00 */  jmp ($0004)


GotBonusPearl:
/* 1C84A: 18 */        clc
/* 1C84B: A5 AE */     lda BonusPearlCount
/* 1C84D: C9 63 */     cmp #$63
/* 1C84F: F0 2C */     beq PlayBonusPearlSound                         ; $C87D
/* 1C851: E6 AE */     inc BonusPearlCount
/* 1C853: D0 28 */     bne PlayBonusPearlSound                         ; $C87D

GotSmallWeaponCapsule:
/* 1C855: A9 82 */     lda #$82
/* 1C857: D0 0A */     bne L1C863 ; +                                           ; $C863

GotSmallLifeCapsule:
/* 1C859: A9 02 */     lda #$02
/* 1C85B: D0 06 */     bne L1C863 ; +                                           ; $C863

GotLargeWeaponCapsule:
/* 1C85D: A9 8A */     lda #$8A
/* 1C85F: D0 02 */     bne L1C863 ; +                                           ; $C863


GotLargeLifeCapsule:
/* 1C861: A9 0A */     lda #$0A
L1C863: ; +
/* 1C863: 85 AD */     sta CapsuleObtained
/* 1C865: 60 */        rts


; If ExtraLives < 99 then Extralives++, play ExtraLife sound

GotExtraLife:
/* 1C866: A9 32 */     lda #$32
/* 1C868: 18 */        clc
/* 1C869: A6 A6 */     ldx ExtraLives
/* 1C86B: E0 63 */     cpx #$63
/* 1C86D: B0 10 */     bcs L1C87F ; +                                           ; $C87F
/* 1C86F: E6 A6 */     inc ExtraLives
/* 1C871: D0 0C */     bne L1C87F ; +                                           ; $C87F

GotMagnetBeam:
/* 1C873: A9 80 */     lda #$80            ; Obtain magnet beam
/* 1C875: 05 5D */     ora WeaponsOwned
/* 1C877: 85 5D */     sta WeaponsOwned
/* 1C879: A9 1C */     lda #$1C
/* 1C87B: 85 71 */     sta $71

PlayBonusPearlSound:
/* 1C87D: A9 1A */     lda #$1A        ; Bonus pearl pickup
L1C87F: ; +
/* 1C87F: 20 77 C4 */  jsr IssueSound                                  ; $C477
/* 1C882: 60 */        rts

; Set scrolling=up
; Set TeleportEnteredFlag=1
GotTeleportation:
/* 1C883: A9 01 */     lda #$01
/* 1C885: 85 26 */     sta CurrentStripeEndType
/* 1C887: 85 B0 */     sta TeleportEnteredFlag
/* 1C889: D0 F2 */     bne PlayBonusPearlSound                         ; $C87D

GotLevelEndItem:
/* 1C88B: A9 1A */     lda #$1A        ; Bonus pearl pickup
/* 1C88D: 20 77 C4 */  jsr IssueSound                                  ; $C477

/* 1C890: 4C 5A C0 */  jmp StageClear                                  ; $C05A


; Set all meters to max and play bonus pearl pickup sound


; Could this be Yashichi (should also give 100,000 points...)
GotYashichi:
/* 1C893: A9 1C */     lda #$1C
/* 1C895: A2 07 */     ldx #$07
L1C897: ; -
/* 1C897: 95 6A */     sta Meters,x
/* 1C899: CA */        dex
/* 1C89A: 10 FB */     bpl L1C897 ; -                                           ; $C897

/* 1C89C: 30 DF */     bmi PlayBonusPearlSound                         ; $C87D


GotItemJumpTable:
.word GotBonusPearl           ; $3C = Bonus Pearl
.word GotSmallWeaponCapsule   ; $3D
.word GotSmallLifeCapsule     ; $3E
.word GotLargeWeaponCapsule   ; $3F
.word GotLargeLifeCapsule     ; $40 = Large life capsule
.word GotExtraLife            ; $41 = Extralife
.word GotMagnetBeam           ; $42 = Magnet beam
.word GotTeleportation
.word GotLevelEndItem
.word GotYashichi

TestShotHit:
; Input: X = object number

/* 1C8B2: A6 2F */     ldx RefObjectNum
/* 1C8B4: BD 20 04 */  lda ObjectFlags,x
/* 1C8B7: 29 20 */     and #$20
/* 1C8B9: F0 02 */     beq L1C8BD ; +                                       ; $C8BD
L1C8BB: ; -
/* 1C8BB: 18 */        clc
/* 1C8BC: 60 */        rts
L1C8BD: ; +
/* 1C8BD: BC 00 04 */  ldy ObjectSpriteNum,x

/* 1C8C0: B9 E9 FA */  lda TableObjectXWidthTable2,y             ; *
/* 1C8C3: 85 01 */     sta $01
/* 1C8C5: B9 B7 FB */  lda TableObjectYHeightTable2,y             ; *
/* 1C8C8: 85 02 */     sta $02

/* 1C8CA: BD 00 06 */  lda ObjectPosY,x
/* 1C8CD: C9 F8 */     cmp #$F8
/* 1C8CF: F0 EA */     beq L1C8BB ; -                                       ; $C8BB
/* 1C8D1: 38 */        sec
/* 1C8D2: E5 02 */     sbc $02

/* 1C8D4: 85 03 */     sta $03
/* 1C8D6: 06 02 */     asl $02
/* 1C8D8: 18 */        clc
/* 1C8D9: 65 02 */     adc $02
/* 1C8DB: 85 02 */     sta $02
/* 1C8DD: 38 */        sec

/* 1C8DE: BD 80 04 */  lda ObjectPosX,x
/* 1C8E1: E5 1A */     sbc ScrollPosX
/* 1C8E3: 38 */        sec
/* 1C8E4: E5 01 */     sbc $01
/* 1C8E6: 85 00 */     sta $00
/* 1C8E8: 06 01 */     asl $01
/* 1C8EA: 18 */        clc
/* 1C8EB: 65 01 */     adc $01
/* 1C8ED: 85 01 */     sta $01
/* 1C8EF: A5 3E */     lda BossCurrentStrategy
/* 1C8F1: C9 05 */     cmp #$05
/* 1C8F3: 90 29 */     bcc L1C91E ; +++                                         ; $C91E
/* 1C8F5: A5 56 */     lda BossBlinkState
/* 1C8F7: D0 25 */     bne L1C91E ; +++                                         ; $C91E

; Test if boss has been hit with any of Megaman's weapons
/* 1C8F9: A2 01 */     ldx #$01
/* 1C8FB: 20 C2 C9 */  jsr TestCollisionWithWeapon                     ; $C9C2
/* 1C8FE: 90 1E */     bcc L1C91E ; +++                                         ; $C91E

; It was a boss. Store object number into $0C
/* 1C900: 86 0C */     stx $0C
; Subtract from boss's energy depending on which weapon is used

/* 1C902: A5 AC */     lda FightingBossNum     ; $AC = which boss we're fighting
/* 1C904: 0A */        asl a
/* 1C905: 0A */        asl a
/* 1C906: 0A */        asl a
/* 1C907: 18 */        clc
/* 1C908: 65 5F */     adc WeaponSelect
/* 1C90A: AA */        tax
/* 1C90B: 38 */        sec
/* 1C90C: AD C1 06 */  lda ObjectLifeMeter+1
/* 1C90F: FD 22 FE */  sbc WeaponDamageOnBoss,x     ; *

/* 1C912: F0 02 */     beq L1C916 ; +                                       ; $C916
/* 1C914: B0 0B */     bcs L1C921 ; ++                                      ; $C921
L1C916: ; +

; Boss's life meter emptied
/* 1C916: A9 00 */     lda #$00
/* 1C918: 8D C1 06 */  sta ObjectLifeMeter+1
/* 1C91B: 4C 2D CA */  jmp BossKilled

L1C91E: ; +++
/* 1C91E: 4C 6D C9 */  jmp TestEnemyDamages
L1C921: ; ++
/* 1C921: 8D C1 06 */  sta ObjectLifeMeter+1
/* 1C924: A5 31 */     lda CurrentStage

/* 1C926: C9 06 */     cmp #$06
/* 1C928: F0 39 */     beq L1C963 ; +                                           ; $C963
/* 1C92A: C9 09 */     cmp #$09
/* 1C92C: F0 35 */     beq L1C963 ; +                                           ; $C963
/* 1C92E: A2 16 */     ldx #$16
/* 1C930: 20 76 C5 */  jsr FindFreeObject                              ; FindFreeObject
/* 1C933: B0 2E */     bcs L1C963 ; +                                           ; $C963
/* 1C935: A5 2F */     lda RefObjectNum
/* 1C937: 48 */        pha
/* 1C938: A9 01 */     lda #$01
/* 1C93A: 85 2F */     sta RefObjectNum
/* 1C93C: 20 7B F6 */  jsr InitActor

/* 1C93F: A9 19 */     lda #$19
/* 1C941: 9D 00 04 */  sta ObjectSpriteNum,x

/* 1C944: FE 40 04 */  inc ObjectUnknown440,x

/* 1C947: A0 08 */     ldy #$08
/* 1C949: 20 33 F5 */  jsr InitObjectDefaultSpeed

/* 1C94C: A9 08 */     lda #$08
/* 1C94E: 9D 20 04 */  sta ObjectFlags,x
/* 1C951: 9D 60 06 */  sta ObjectYSpeedFraction,x

/* 1C954: BD 00 06 */  lda ObjectPosY,x
/* 1C957: A4 31 */     ldy CurrentStage
/* 1C959: 38 */        sec
/* 1C95A: F9 89 C9 */  sbc TableC989,y
/* 1C95D: 9D 00 06 */  sta ObjectPosY,x
/* 1C960: 68 */        pla



/* 1C961: 85 2F */     sta RefObjectNum
L1C963: ; +
/* 1C963: A4 AC */     ldy FightingBossNum
/* 1C965: B9 94 C9 */  lda BossBlinkLengthTable,y
/* 1C968: 85 56 */     sta BossBlinkState
/* 1C96A: 4C A9 C9 */  jmp L1C9A9


TestEnemyDamages: ; Test enemies 10,12,14,.. on even frames and 11,13,15,.. on odd frames
/* 1C96D: A5 23 */     lda FrameCounter
/* 1C96F: 29 01 */     and #$01
/* 1C971: 18 */        clc
/* 1C972: 69 10 */     adc #$10
/* 1C974: AA */        tax
L1C975: ; -
/* 1C975: BD 20 04 */  lda ObjectFlags,x
/* 1C978: 29 80 */     and #$80
/* 1C97A: D0 05 */     bne L1C981 ; +                                           ; $C981
/* 1C97C: 20 C2 C9 */  jsr TestCollisionWithWeapon                     ; $C9C2
/* 1C97F: B0 1E */     bcs L1C99F ; ++                                          ; $C99F
L1C981: ; +
/* 1C981: E8 */        inx
/* 1C982: E8 */        inx
/* 1C983: E4 54 */     cpx TotalObjects
/* 1C985: 90 EE */     bcc L1C975 ; -                                           ; $C975
/* 1C987: 18 */        clc
/* 1C988: 60 */        rts


TableC989:
    .byt 4,0,0,8,0,4,0,0,0,0,0

BossBlinkLengthTable: ;at C994. Values for BossBlinkState for each boss.
    .byte $3F, $1E, $1E, $1E, $3F, $1E
    .byte $09, $3F, $1E, $09, $09

L1C99F: ; ++
/* 1C99F: 86 0C */     stx $0C

; Make object get hit
/* 1C9A1: BD 20 04 */  lda ObjectFlags,x
/* 1C9A4: 09 80 */     ora #$80
/* 1C9A6: 9D 20 04 */  sta ObjectFlags,x

L1C9A9:
/* 1C9A9: A9 1B */     lda #$1B        ; Big noisy explosion
/* 1C9AB: 20 77 C4 */  jsr IssueSound                              ; $C477

/* 1C9AE: A6 2F */     ldx RefObjectNum
/* 1C9B0: BD 20 04 */  lda ObjectFlags,x
/* 1C9B3: 29 04 */     and #$04
/* 1C9B5: D0 05 */     bne L1C9BC ; +                                       ; $C9BC
; This object cannot collide with Megaman's bullets
/* 1C9B7: A9 F8 */     lda #$F8
/* 1C9B9: 9D 00 06 */  sta ObjectPosY,x
L1C9BC: ; +
/* 1C9BC: 38 */        sec
/* 1C9BD: 60 */        rts


; Collision detection. C = 1 if colliding
TestCollisionWithMegaman:
/* 1C9BE: A9 02 */     lda #$02
/* 1C9C0: D0 02 */     bne L1C9C4 ; +                                           ; $C9C4
TestCollisionWithWeapon:
/* 1C9C2: A9 04 */     lda #$04
L1C9C4: ; +
/* 1C9C4: 3D 20 04 */  and ObjectFlags,x
/* 1C9C7: D0 02 */     bne L1C9CB ; +                                           ; $C9CB
/* 1C9C9: 18 */        clc
/* 1C9CA: 60 */        rts

L1C9CB: ; +
/* 1C9CB: BD 00 06 */  lda ObjectPosY,x
/* 1C9CE: C9 F8 */     cmp #$F8
/* 1C9D0: F0 59 */     beq L1CA2B ; +++                                         ; $CA2B

/* 1C9D2: BC 00 04 */  ldy ObjectSpriteNum,x
/* 1C9D5: C0 FF */     cpy #$FF
/* 1C9D7: D0 0D */     bne L1C9E6 ; +                                           ; $C9E6

; if TableObjectYHeightTable1,Y != 0
;   $0D,$0E = TableObjectYHeightTable1,Y, TableObjectXWidthTable1,Y
; else
;   $0D,$0E = TableObjectYHeightTable2,Y, TableObjectXWidthTable2,Y
/* 1C9D9: BC E0 06 */  ldy ObjectType,x
/* 1C9DC: B9 38 FB */  lda TableObjectXWidthTable1,y
/* 1C9DF: 85 0E */     sta $0E
/* 1C9E1: B9 3A FC */  lda TableObjectYHeightTable1,y     ; *
/* 1C9E4: D0 08 */     bne L1C9EE ; ++                                          ; $C9EE
L1C9E6: ; +
/* 1C9E6: B9 E9 FA */  lda TableObjectXWidthTable2,y     ; *
/* 1C9E9: 85 0E */     sta $0E
/* 1C9EB: B9 B7 FB */  lda TableObjectYHeightTable2,y     ; *
L1C9EE: ; ++
/* 1C9EE: 85 0D */     sta $0D


; if ObjectPosY,X - $0D >= $02 then there can be no collision
/* 1C9F0: 38 */        sec
/* 1C9F1: BD 00 06 */  lda ObjectPosY,x
/* 1C9F4: E5 0D */     sbc $0D

/* 1C9F6: C5 02 */     cmp $02
/* 1C9F8: B0 31 */     bcs L1CA2B ; +++                                     ; $CA2B

; if ObjectPosY,X + $0D < $03 then there can be no collision
/* 1C9FA: 18 */        clc
/* 1C9FB: BD 00 06 */  lda ObjectPosY,x
/* 1C9FE: 65 0D */     adc $0D
/* 1CA00: C5 03 */     cmp $03
/* 1CA02: 90 27 */     bcc L1CA2B ; +++                                     ; $CA2B


; if ObjectPosScreen,X != ScrollPosScreen then there can be no collision
/* 1CA04: 38 */        sec
/* 1CA05: BD 80 04 */  lda ObjectPosX,x
/* 1CA08: E5 1A */     sbc ScrollPosX
/* 1CA0A: 85 0C */     sta $0C
/* 1CA0C: BD 60 04 */  lda ObjectPosScreen,x
/* 1CA0F: E5 1B */     sbc ScrollPosScreen
/* 1CA11: D0 18 */     bne L1CA2B ; +++                                     ; $CA2B


; if ObjectPosX - ScrollPosX < $0E then there can be no collision
/* 1CA13: A5 0C */     lda $0C
/* 1CA15: 38 */        sec
/* 1CA16: E5 0E */     sbc $0E
/* 1CA18: 90 11 */     bcc L1CA2B ; +++                                     ; $CA2B


; if ObjectPosX - ScrollPosX - $0E == $01 then a collision has occured!
/* 1CA1A: C5 01 */     cmp $01
/* 1CA1C: F0 0B */     beq L1CA29 ; +                                       ; $CA29

; if ObjectPosX - ScrollPosX - $0E > $01 then there can be no collision
/* 1CA1E: B0 0B */     bcs L1CA2B ; +++                                     ; $CA2B

; if ObjectPosX - ScrollPosX + $0E >= $00 then a collision has occured!
/* 1CA20: 18 */        clc
/* 1CA21: A5 0C */     lda $0C
/* 1CA23: 65 0E */     adc $0E
/* 1CA25: C5 00 */     cmp $00
/* 1CA27: 90 02 */     bcc L1CA2B ; +++                                     ; $CA2B
L1CA29: ; +
/* 1CA29: 38 */        sec
/* 1CA2A: 60 */        rts

L1CA2B: ; +++
/* 1CA2B: 18 */        clc
/* 1CA2C: 60 */        rts


BossKilled:
/* 1CA2D: A9 20 */     lda #$20
/* 1CA2F: 20 16 D1 */  jsr TimeDelayWithAllObjectsHalted
/* 1CA32: A9 01 */     lda #$01
/* 1CA34: 85 59 */     sta $59
/* 1CA36: A9 5F */     lda #$5F
/* 1CA38: 85 5A */     sta $5A
/* 1CA3A: 20 2A C3 */  jsr AtomicExplodeActor
/* 1CA3D: A5 AC */     lda FightingBossNum
/* 1CA3F: C9 09 */     cmp #$09
/* 1CA41: F0 3B */     beq BossKilledCondition1
/* 1CA43: C5 31 */     cmp CurrentStage
/* 1CA45: D0 37 */     bne BossKilledCondition1

/* 1CA47: A9 FF */     lda #$FF
/* 1CA49: 20 77 C4 */  jsr IssueSound                              ; $C477

/* 1CA4C: A9 12 */     lda #$12        ; Explosion
/* 1CA4E: 20 77 C4 */  jsr IssueSound                              ; $C477

/* 1CA51: A5 31 */     lda CurrentStage
/* 1CA53: C9 06 */     cmp #$06
/* 1CA55: B0 27 */     bcs BossKilledCondition1

; This portion only applies for CIBFEG enemies, not wily stages
/* 1CA57: A9 01 */     lda #$01
/* 1CA59: 85 2F */     sta RefObjectNum
/* 1CA5B: A9 44 */     lda #$44 ;Create the level-end-item
/* 1CA5D: A2 1F */     ldx #$1F
/* 1CA5F: 20 7B F6 */  jsr InitActor

; Make this object collidable with Megaman and BG
/* 1CA62: A9 13 */     lda #$13
/* 1CA64: 9D 20 04 */  sta ObjectFlags,x

/* 1CA67: A9 28 */     lda #$28
/* 1CA69: 9D 00 06 */  sta ObjectPosY,x

/* 1CA6C: A9 80 */     lda #$80
/* 1CA6E: 9D 80 04 */  sta ObjectPosX,x

/* 1CA71: A0 C4 */     ldy #$C4
/* 1CA73: 20 33 F5 */  jsr InitObjectDefaultSpeed

/* 1CA76: A9 F8 */     lda #$F8    ; Disable boss
/* 1CA78: 8D 01 06 */  sta ObjectPosY+1

/* 1CA7B: 4C CE CA */  jmp L1CACE

BossKilledCondition1:
; Arrived here if
; - it's last boss
; - it's a rematch
; - it's wily stage
/* 1CA7E: A5 AC */     lda FightingBossNum
/* 1CA80: C9 09 */     cmp #$09
/* 1CA82: D0 03 */     bne L1CA87
; Handle last boss killed
/* 1CA84: 4C 8E F2 */  jmp L1F28E
L1CA87:
/* 1CA87: C9 0A */     cmp #$0A
/* 1CA89: F0 78 */     beq L1CB03
/* 1CA8B: C5 31 */     cmp CurrentStage
/* 1CA8D: F0 4E */     beq L1CADD

; Add 10,000 points to score if it was a rematch

/* 1CA8F: A9 0A */     lda #$0A
/* 1CA91: 85 0C */     sta $0C
L1CA93: ; -
/* 1CA93: A9 10 */     lda #$10
/* 1CA95: 85 05 */     sta $05
/* 1CA97: 20 40 D4 */  jsr IncreaseScore                           ; $D440
/* 1CA9A: C6 0C */     dec $0C
/* 1CA9C: D0 F5 */     bne L1CA93 ; -                                           ; $CA93

/* 1CA9E: A9 00 */     lda #$00
/* 1CAA0: 85 0C */     sta $0C

/* 1CAA2: A6 AC */     ldx FightingBossNum
/* 1CAA4: BD 4E C1 */  lda IndexIntoTheActiveThatPreventsEscapingRematch,x
/* 1CAA7: 20 25 C6 */  jsr LoadActiveByIndexAndSetBlockingness
                        ; Remove the block that prevents exiting the room

/* 1CAAA: A5 31 */     lda CurrentStage
/* 1CAAC: C9 07 */     cmp #$07
/* 1CAAE: F0 1E */     beq L1CACE ; +                                           ; $CACE
/* 1CAB0: A9 01 */     lda #$01
/* 1CAB2: 85 2F */     sta RefObjectNum
/* 1CAB4: A9 43 */     lda #$43 ;Spawn a teleport if it wasn't Wily2
/* 1CAB6: A2 1F */     ldx #$1F
/* 1CAB8: 20 7B F6 */  jsr InitActor

/* 1CABB: A9 08 */     lda #$08
/* 1CABD: 9D 80 04 */  sta ObjectPosX,x

/* 1CAC0: A8 */        tay
/* 1CAC1: 20 33 F5 */  jsr InitObjectDefaultSpeed
/* 1CAC4: A9 B0 */     lda #$B0
/* 1CAC6: 9D 00 06 */  sta ObjectPosY,x

; Make object invisible and collidable with Megaman
/* 1CAC9: A9 22 */     lda #$22
/* 1CACB: 9D 20 04 */  sta ObjectFlags,x
L1CACE: ; +
/* 1CACE: A9 00 */     lda #$00
/* 1CAD0: 85 3E */     sta BossCurrentStrategy

/* 1CAD2: A9 F8 */     lda #$F8    ; Disable boss
/* 1CAD4: 8D 01 06 */  sta ObjectPosY+1

/* 1CAD7: A2 FF */     ldx #$FF
/* 1CAD9: 9A */        txs
/* 1CADA: 4C 5E 91 */  jmp L1515E

; Arrived here if it wasn't a rematch

L1CADD:
/* 1CADD: C9 06 */     cmp #$06
/* 1CADF: D0 0E */     bne L1CAEF ; +                                           ; $CAEF
; Arrived here if it was Boss6
/* 1CAE1: A2 02 */     ldx #$02
L1CAE3: ; -
/* 1CAE3: BD 00 CB */  lda Boss6KilledPalette,x
/* 1CAE6: 9D D5 03 */  sta BGPalettes + 5,x
/* 1CAE9: CA */        dex
/* 1CAEA: 10 F7 */     bpl L1CAE3 ; -                                           ; $CAE3
/* 1CAEC: 20 3F C7 */  jsr PaletteSetupForBG

L1CAEF: ; +
/* 1CAEF: A9 F8 */     lda #$F8
/* 1CAF1: 8D 01 06 */  sta ObjectPosY+1

/* 1CAF4: A9 FF */     lda #$FF
/* 1CAF6: 85 3C */     sta MiscCounter1
L1CAF8: ; -
/* 1CAF8: 20 17 C3 */  jsr TimeDelayC317
/* 1CAFB: D0 FB */     bne L1CAF8 ; -                                           ; $CAF8

/* 1CAFD: 4C 5A C0 */  jmp StageClear                                  ; $C05A


Boss6KilledPalette:; colours for some purpose
    .byte $00,$10,$0F


L1CB03:
/* 1CB03: A2 10 */     ldx #$10
/* 1CB05: 86 2F */     stx RefObjectNum
/* 1CB07: 20 FD AE */  jsr ReplaceObjectWithExplosion
/* 1CB0A: A9 F8 */     lda #$F8
/* 1CB0C: 8D 10 06 */  sta $0610
/* 1CB0F: 8D 01 06 */  sta ObjectPosY+1
L1CB12: ; +

/* 1CB12: A2 03 */     ldx #$03
L1CB14: ; -
/* 1CB14: 9D 12 06 */  sta ObjectPosY+$12,x
/* 1CB17: CA */        dex
/* 1CB18: 10 FA */     bpl L1CB14 ; -                                           ; $CB14

/* 1CB1A: A9 0F */     lda #$0F
/* 1CB1C: A2 07 */     ldx #$07
L1CB1E: ; -
/* 1CB1E: 9D D4 03 */  sta BGPalettes + $4,x
/* 1CB21: CA */        dex
/* 1CB22: 10 FA */     bpl L1CB1E ; -                                           ; $CB1E

/* 1CB24: A9 00 */     lda #$00
/* 1CB26: 8D D5 03 */  sta BGPalettes + $5
/* 1CB29: 8D D9 03 */  sta BGPalettes + $9
/* 1CB2C: 20 3F C7 */  jsr PaletteSetupForBG
/* 1CB2F: 20 31 D1 */  jsr UpdateGraphics                              ; $D131
/* 1CB32: A9 02 */     lda #$02
/* 1CB34: 85 5C */     sta $5C
/* 1CB36: A9 B9 */     lda #>LxB900
/* 1CB38: A2 09 */     ldx #$09
/* 1CB3A: A0 08 */     ldy #$08
/* 1CB3C: 20 BE F0 */  jsr DoPPUtransferRoutineF0BE
/* 1CB3F: A9 B2 */     lda #>LxB200
/* 1CB41: A2 0A */     ldx #$0A
/* 1CB43: A0 08 */     ldy #$08
/* 1CB45: 20 BE F0 */  jsr DoPPUtransferRoutineF0BE
/* 1CB48: A9 80 */     lda #$80
/* 1CB4A: 85 3C */     sta MiscCounter1
/* 1CB4C: EE A0 06 */  inc ObjectLifeCycleCounter+0
/* 1CB4F: EE B1 06 */  inc $06B1
L1CB52:
/* 1CB52: 20 17 C3 */  jsr TimeDelayC317
/* 1CB55: D0 FB */     bne L1CB52
/* 1CB57: A2 11 */     ldx #$11
/* 1CB59: 86 2F */     stx RefObjectNum
/* 1CB5B: A9 4A */     lda #$4A
/* 1CB5D: 20 7B F6 */  jsr InitActor
/* 1CB60: A9 11 */     lda #$11
/* 1CB62: 9D 20 04 */  sta ObjectFlags,x
/* 1CB65: A9 02 */     lda #$02
/* 1CB67: 9D 80 06 */  sta ObjectYSpeed,x
/* 1CB6A: E6 68 */     inc ForcedInputFlag
/* 1CB6C: 4C CE CA */  jmp L1CACE

ObjectVerifyBackgroundCollision:
/* 1CB6F: 20 9C C3 */  jsr SwitchBankStage                             ; $C39C
/* 1CB72: A5 01 */     lda $01
/* 1CB74: 85 0C */     sta $0C
/* 1CB76: A5 00 */     lda $00
/* 1CB78: 85 0D */     sta $0D
/* 1CB7A: A6 2F */     ldx RefObjectNum
/* 1CB7C: D0 1B */     bne L1CB99 ; +                                           ; $CB99
/* 1CB7E: A2 02 */     ldx #$02
L1CB80: ; -
/* 1CB80: 18 */        clc
/* 1CB81: A5 03 */     lda $03
/* 1CB83: 7D 96 CB */  adc CB96_table,x
/* 1CB86: 85 0E */     sta $0E
/* 1CB88: 20 B7 CC */  jsr ReadCurrentStageMap

/* 1CB8B: 95 2A */     sta $2A,x
/* 1CB8D: CA */        dex
/* 1CB8E: 10 F0 */     bpl L1CB80 ; -                                           ; $CB80

/* 1CB90: 20 90 D7 */  jsr AnalyzeCurrentTile
/* 1CB93: 4C B3 C3 */  jmp SwitchBank05                                ; $C3B3

CB96_table: .byte $F4, $FC, $0B
L1CB99: ; +
/* 1CB99: A6 2F */     ldx RefObjectNum
/* 1CB9B: BC 00 04 */  ldy ObjectSpriteNum,x
/* 1CB9E: C0 FF */     cpy #$FF
/* 1CBA0: D0 08 */     bne L1CBAA
/* 1CBA2: BC E0 06 */  ldy ObjectType,x
/* 1CBA5: B9 3A FC */  lda TableObjectYHeightTable1,y     ; *
/* 1CBA8: D0 03 */     bne L1CBAD ; +                                           ; $CBAD
L1CBAA:
/* 1CBAA: B9 B7 FB */  lda TableObjectYHeightTable2,y     ; *
L1CBAD: ; +
/* 1CBAD: 48 */        pha

/* 1CBAE: 49 FF */     eor #$FF    ; A = -A
/* 1CBB0: 18 */        clc
/* 1CBB1: 69 01 */     adc #$01

/* 1CBB3: 18 */        clc
/* 1CBB4: 65 03 */     adc $03
L1CBB6: ; -
/* 1CBB6: 85 0E */     sta $0E
/* 1CBB8: 20 B7 CC */  jsr ReadCurrentStageMap
/* 1CBBB: C9 01 */     cmp #$01
/* 1CBBD: F0 17 */     beq L1CBD6 ; -                                           ; $CBD6
/* 1CBBF: 68 */        pla

/* 1CBC0: 38 */        sec
/* 1CBC1: E9 01 */     sbc #$01
/* 1CBC3: 18 */        clc
/* 1CBC4: 65 03 */     adc $03
/* 1CBC6: 85 0E */     sta $0E
/* 1CBC8: 20 B7 CC */  jsr ReadCurrentStageMap
/* 1CBCB: C9 01 */     cmp #$01
/* 1CBCD: F0 08 */     beq L1CBD7
/* 1CBCF: A9 00 */     lda #$00
/* 1CBD1: 85 2A */     sta $2A
/* 1CBD3: 4C B3 C3 */  jmp SwitchBank05                                ; $C3B3
L1CBD6:
/* 1CBD6: 68 */        pla

L1CBD7:
/* 1CBD7: A9 01 */     lda #$01
/* 1CBD9: 85 2A */     sta $2A
/* 1CBDB: 4C B3 C3 */  jmp SwitchBank05                                ; $C3B3


UpdateCurrentTileState:
/* 1CBDE: A9 00 */     lda #$00
/* 1CBE0: 85 30 */     sta CurrentTileState

/* 1CBE2: AD 80 04 */  lda ObjectPosX+0
/* 1CBE5: 85 00 */     sta $00
/* 1CBE7: AD 60 04 */  lda ObjectPosScreen+0                         ; $0460
/* 1CBEA: 85 01 */     sta $01
/* 1CBEC: AD 00 06 */  lda ObjectPosY+0                                 ; ObjectPosY+0
/* 1CBEF: 85 03 */     sta $03
/* 1CBF1: 20 6F CB */  jsr ObjectVerifyBackgroundCollision

/* 1CBF4: 38 */        sec
/* 1CBF5: AD 20 06 */  lda ObjectPosYfraction+0
/* 1CBF8: ED 60 06 */  sbc ObjectYSpeedFraction+0
/* 1CBFB: AD 00 06 */  lda ObjectPosY+0                                 ; ObjectPosY+0
/* 1CBFE: ED 80 06 */  sbc ObjectYSpeed+0
/* 1CC01: AE 80 06 */  ldx ObjectYSpeed+0
/* 1CC04: 30 06 */     bmi L1CC0C ; +    ;Is he moving up?
/* 1CC06: 38 */        sec
/* 1CC07: E9 0C */     sbc #$0C ;Down.
/* 1CC09: 4C 0F CC */  jmp L1CC0F ; ++
L1CC0C: ; +
/* 1CC0C: 18 */        clc
/* 1CC0D: 69 0C */     adc #$0C ;Up.
L1CC0F: ; ++
/* 1CC0F: 85 0E */     sta $0E
/* 1CC11: 20 9C C3 */  jsr SwitchBankStage                             ; $C39C
/* 1CC14: 20 B7 CC */  jsr ReadCurrentStageMap
/* 1CC17: C9 02 */     cmp #$02
/* 1CC19: D0 0F */     bne L1CC2A ; +++
/* 1CC1B: A5 30 */     lda CurrentTileState
/* 1CC1D: AE 80 06 */  ldx ObjectYSpeed+0
/* 1CC20: 30 04 */     bmi L1CC26 ; + ;Moving up?

/* 1CC22: 09 10 */     ora #$10  ; Force climbing up
/* 1CC24: D0 02 */     bne L1CC28 ; ++ ;unconditional jump
L1CC26: ; +
/* 1CC26: 09 01 */     ora #$01  ; Enable climbing down
L1CC28: ; ++
/* 1CC28: 85 30 */     sta CurrentTileState
L1CC2A: ; +++
/* 1CC2A: 4C B3 C3 */  jmp SwitchBank05                                ; $C3B3


DoCollisionCheckFor:
/* 1CC2D: 20 9C C3 */  jsr SwitchBankStage                             ; $C39C

/* 1CC30: A5 03 */     lda $03
/* 1CC32: 85 0E */     sta $0E

/* 1CC34: A6 2F */     ldx RefObjectNum
/* 1CC36: D0 28 */     bne L1CC60 ; +                                           ; $CC60

/* 1CC38: A9 00 */     lda #$00
/* 1CC3A: 85 2B */     sta $2B
/* 1CC3C: A2 03 */     ldx #$03
L1CC3E: ; -
; $00,$01 + $CC5C,X,$CC5B,X => $0D,$0C
/* 1CC3E: 18 */        clc
/* 1CC3F: A5 00 */     lda $00
/* 1CC41: 7D 5C CC */  adc CC5C_table+0,x
/* 1CC44: 85 0D */     sta $0D
/* 1CC46: A5 01 */     lda $01
/* 1CC48: 7D 5B CC */  adc CC5C_table-1,x
/* 1CC4B: 85 0C */     sta $0C

/* 1CC4D: 20 B7 CC */  jsr ReadCurrentStageMap
/* 1CC50: CA */        dex
/* 1CC51: 95 2A */     sta $2A,x
/* 1CC53: CA */        dex
/* 1CC54: 10 E8 */     bpl L1CC3E ; -                                           ; $CC3E

/* 1CC56: 20 E1 D7 */  jsr F1D7E1
/* 1CC59: 4C B3 C3 */  jmp SwitchBank05                                ; $C3B3

CC5C_table:
    .byte $00,$07 ;+7
    .byte $FF,$F9 ;-7

L1CC60: ; +
/* 1CC60: A6 2F */     ldx RefObjectNum
/* 1CC62: BC 00 04 */  ldy ObjectSpriteNum,x
/* 1CC65: C0 FF */     cpy #$FF
/* 1CC67: D0 08 */     bne L1CC71 ; +
/* 1CC69: BC E0 06 */  ldy ObjectType,x
/* 1CC6C: B9 38 FB */  lda TableObjectXWidthTable1,y     ; *
/* 1CC6F: D0 03 */     bne L1CC74 ; ++
L1CC71: ; +
/* 1CC71: B9 B5 FA */  lda TableObjectXWidthTable2,y     ; *
L1CC74: ; ++
/* 1CC74: 38 */        sec
/* 1CC75: E9 01 */     sbc #$01
/* 1CC77: 85 0F */     sta $0F
/* 1CC79: 18 */        clc
/* 1CC7A: A5 00 */     lda $00
/* 1CC7C: 65 0F */     adc $0F
/* 1CC7E: 85 0D */     sta $0D
/* 1CC80: A5 01 */     lda $01
/* 1CC82: 69 00 */     adc #$00
/* 1CC84: 85 0C */     sta $0C
/* 1CC86: 20 B7 CC */  jsr ReadCurrentStageMap
/* 1CC89: C9 01 */     cmp #$01
/* 1CC8B: F0 23 */     beq L1CCB0 ; +                                           ; $CCB0
/* 1CC8D: C9 04 */     cmp #$04
/* 1CC8F: F0 1F */     beq L1CCB0 ; +                                           ; $CCB0
/* 1CC91: 38 */        sec
/* 1CC92: A5 00 */     lda $00
/* 1CC94: E5 0F */     sbc $0F
/* 1CC96: 85 0D */     sta $0D
/* 1CC98: A5 01 */     lda $01
/* 1CC9A: E9 00 */     sbc #$00
/* 1CC9C: 85 0C */     sta $0C
/* 1CC9E: 20 B7 CC */  jsr ReadCurrentStageMap
/* 1CCA1: C9 01 */     cmp #$01
/* 1CCA3: F0 0B */     beq L1CCB0 ; +                                           ; $CCB0
/* 1CCA5: C9 04 */     cmp #$04
/* 1CCA7: F0 07 */     beq L1CCB0 ; +                                           ; $CCB0
/* 1CCA9: A9 00 */     lda #$00
/* 1CCAB: 85 2B */     sta $2B
/* 1CCAD: 4C B3 C3 */  jmp SwitchBank05                                ; $C3B3
L1CCB0: ; +
/* 1CCB0: A9 01 */     lda #$01
/* 1CCB2: 85 2B */     sta $2B
/* 1CCB4: 4C B3 C3 */  jmp SwitchBank05                                ; $C3B3


ReadCurrentStageMap:
/* 1CCB7: A4 8E */     ldy ActivesLowerIndex
/* 1CCB9: 20 8A CD */  jsr CheckCollisionAgainstActives
/* 1CCBC: C9 00 */     cmp #$00
/* 1CCBE: F0 01 */     beq L1CCC1 ; +                                           ; $CCC1
/* 1CCC0: 60 */        rts
L1CCC1: ; +
/* 1CCC1: A5 0E */     lda $0E
/* 1CCC3: C9 F0 */     cmp #$F0
/* 1CCC5: 90 11 */     bcc L1CCD8 ; ++
/* 1CCC7: A4 2F */     ldy RefObjectNum
/* 1CCC9: B9 00 06 */  lda ObjectPosY,y
/* 1CCCC: C9 F1 */     cmp #$F1
/* 1CCCE: B0 04 */     bcs L1CCD4 ; +                                           ; $0001CCD4
/* 1CCD0: C9 80 */     cmp #$80
/* 1CCD2: B0 04 */     bcs L1CCD8 ; ++                                          ; $0001CCD8
L1CCD4: ; + ;When Ypos < #$80 or Ypos >= #$F1 and $0E != #$F0
/* 1CCD4: A9 00 */     lda #$00
/* 1CCD6: 85 0E */     sta $0E
; This seems to be the map
L1CCD8: ; ++
/* 1CCD8: A4 0C */     ldy $0C
/* 1CCDA: B9 00 8C */  lda RoomOrderTable,y
/* 1CCDD: 0A */        asl a
/* 1CCDE: A8 */        tay
; $04,$05 = address of room to draw
/* 1CCDF: B9 30 8C */  lda RoomPointerTable+0,y
/* 1CCE2: 85 04 */     sta $04
/* 1CCE4: B9 31 8C */  lda RoomPointerTable+1,y
/* 1CCE7: 85 05 */     sta $05
/* 1CCE9: A5 0D */     lda $0D
/* 1CCEB: 4A */        lsr a
/* 1CCEC: 4A */        lsr a
/* 1CCED: 29 38 */     and #$38
/* 1CCEF: 85 07 */     sta CurrentRoomPointer+1
/* 1CCF1: A5 0E */     lda $0E
/* 1CCF3: 2A */        rol a
/* 1CCF4: 2A */        rol a
/* 1CCF5: 2A */        rol a
/* 1CCF6: 2A */        rol a
/* 1CCF7: 29 07 */     and #$07
/* 1CCF9: 05 07 */     ora CurrentRoomPointer+1
/* 1CCFB: A8 */        tay
; Read 32x32 tile index
/* 1CCFC: B1 04 */     lda ($04),y
/* 1CCFE: A0 00 */     ldy #$00
/* 1CD00: 84 09 */     sty $09
/* 1CD02: 0A */        asl a
/* 1CD03: 26 09 */     rol $09
/* 1CD05: 0A */        asl a
/* 1CD06: 26 09 */     rol $09
/* 1CD08: A8 */        tay

/* 1CD09: A5 0E */     lda $0E
/* 1CD0B: 29 10 */     and #$10
/* 1CD0D: F0 01 */     beq L1CD10 ; +                                           ; $CD10
/* 1CD0F: C8 */        iny
L1CD10: ; +
/* 1CD10: A5 0D */     lda $0D
/* 1CD12: 29 10 */     and #$10
/* 1CD14: F0 02 */     beq L1CD18 ; +                                           ; $CD18
/* 1CD16: C8 */        iny
/* 1CD17: C8 */        iny
L1CD18: ; +
/* 1CD18: A9 00 */     lda #<RoomBlockData
/* 1CD1A: 85 08 */     sta CurrentRoomPointer+2

/* 1CD1C: A9 80 */     lda #>RoomBlockData
/* 1CD1E: 05 09 */     ora CurrentRoomPointer+3
/* 1CD20: 85 09 */     sta CurrentRoomPointer+3

            ; CurrentRoomPointer[2] = 8000 + (byte3 << 8)

/* 1CD22: B1 08 */     lda (CurrentRoomPointer+2),y
/* 1CD24: 29 C0 */     and #$C0

/* 1CD26: A4 31 */     ldy CurrentStage
/* 1CD28: 84 06 */     sty CurrentRoomPointer
/* 1CD2A: 0A */        asl a
/* 1CD2B: 26 06 */     rol CurrentRoomPointer
/* 1CD2D: 0A */        asl a
/* 1CD2E: 26 06 */     rol CurrentRoomPointer
/* 1CD30: A4 06 */     ldy CurrentRoomPointer
/* 1CD32: B9 46 CD */  lda BlockTransparencyMap,y

/* 1CD35: C9 02 */     cmp #$02 ;climbable?
/* 1CD37: D0 0C */     bne L1CD45 ; ++                                          ; $CD45

/* 1CD39: A5 2F */     lda RefObjectNum
/* 1CD3B: D0 06 */     bne L1CD43 ; +                                           ; $CD43
; For Rockman (and him only), it means a climbable
/* 1CD3D: A5 0D */     lda $0D
/* 1CD3F: 29 F0 */     and #$F0
/* 1CD41: 85 2E */     sta $2E
L1CD43: ; +
/* 1CD43: A9 02 */     lda #$02
L1CD45: ; ++
/* 1CD45: 60 */        rts

; According to Rock&Roll (editor), TSAsettings
; is about which is blocks are air and transparent and so on
BlockTransparencyMap: ; at CD46
    .byte 0,1,2,3 ;0 C
    .byte 0,1,4,5 ;1 I
    .byte 0,1,2,3 ;2 B
    .byte 0,1,2,6 ;3 F
    .byte 0,1,2,0 ;4 E
    .byte 0,1,3,0 ;5 G
    .byte 0,1,3,2 ;6 W1
    .byte 0,1,2,3 ;7 W2
    .byte 0,1,0,0 ;8 W3
    .byte 0,1,3,2 ;9 W4
    .byte 0,1,0,0 ;A gruu

RecalculateActivesLowerIndex:
/* 1CD72: 20 9C C3 */  jsr SwitchBankStage                             ; $C39C
/* 1CD75: A0 00 */     ldy #$00
L1CD77: ; -
/* 1CD77: B9 22 07 */  lda RoomActiveTable+2,y ;is it in same screen?
/* 1CD7A: C5 1B */     cmp ScrollPosScreen
/* 1CD7C: B0 07 */     bcs L1CD85 ; +                                           ; $CD85
/* 1CD7E: 98 */        tya
/* 1CD7F: 18 */        clc
/* 1CD80: 69 06 */     adc #$06
/* 1CD82: A8 */        tay
/* 1CD83: D0 F2 */     bne L1CD77 ; -                                           ; $CD77
L1CD85: ; +
/* 1CD85: 84 8E */     sty ActivesLowerIndex
/* 1CD87: 4C B3 C3 */  jmp SwitchBank05                                ; $C3B3



CheckCollisionAgainstActives:
; Input:
;   $0C,$0D,$0E = screen#,X,Y
;
/* 1CD8A: A5 68 */     lda ForcedInputFlag
/* 1CD8C: D0 57 */     bne L1CDE5 ; +++
; Ignore these tests if forced input is active.
; This rule is here to allow Megaman to walk through the Actives that
; normally prevent him from exiting the room through the leftside shutter.
; Incidentally, it also means that Gutsblocks will be air for Megaman
; during the time when a boss battle starts.
/* 1CD8E: AD 20 07 */  lda RoomActiveTable
L1CD91: ; -
/* 1CD91: B9 21 07 */  lda RoomActiveTable+1,y ;is it a blocking object?
/* 1CD94: F0 48 */     beq L1CDDE ; ++
/* 1CD96: B9 22 07 */  lda RoomActiveTable+2,y ;is it in the same screen?
/* 1CD99: C5 0C */     cmp $0C
/* 1CD9B: F0 05 */     beq L1CDA2 ; +                                           ; $CDA2
/* 1CD9D: 90 3F */     bcc L1CDDE ; ++
/* 1CD9F: 4C E5 CD */  jmp L1CDE5 ; +++ ;Ignore the active if the screen number is ahead current point.
L1CDA2: ; +
/* 1CDA2: B9 23 07 */  lda RoomActiveTable+3,y ;check x1
/* 1CDA5: C5 0D */     cmp $0D
/* 1CDA7: F0 02 */     beq L1CDAB ; +                                           ; $CDAB
/* 1CDA9: B0 33 */     bcs L1CDDE ; ++
L1CDAB: ; +
/* 1CDAB: B9 25 07 */  lda RoomActiveTable+5,y ;check x2
/* 1CDAE: F0 06 */     beq L1CDB6 ; +
/* 1CDB0: C5 0D */     cmp $0D
/* 1CDB2: F0 2A */     beq L1CDDE ; ++
/* 1CDB4: 90 28 */     bcc L1CDDE ; ++
L1CDB6: ; +
/* 1CDB6: B9 24 07 */  lda RoomActiveTable+4,y ;check y1
/* 1CDB9: C5 0E */     cmp $0E
/* 1CDBB: F0 02 */     beq L1CDBF ; +
/* 1CDBD: B0 1F */     bcs L1CDDE ; ++
L1CDBF: ; +
/* 1CDBF: B9 26 07 */  lda RoomActiveTable+6,y ;check y2
/* 1CDC2: F0 06 */     beq L1CDCA ; +
/* 1CDC4: C5 0E */     cmp $0E
/* 1CDC6: F0 16 */     beq L1CDDE ; ++
/* 1CDC8: 90 14 */     bcc L1CDDE ; ++
L1CDCA: ; +
/* 1CDCA: A5 2F */     lda RefObjectNum
/* 1CDCC: D0 06 */     bne L1CDD4 ; +
/* 1CDCE: B9 21 07 */  lda RoomActiveTable+1,y
/* 1CDD1: 09 80 */     ora #$80
/* 1CDD3: 60 */        rts
L1CDD4: ; +
/* 1CDD4: B9 21 07 */  lda RoomActiveTable+1,y
/* 1CDD7: C9 04 */     cmp #$04 ;if it's a disintegrated gutsblock, it's no longer there
/* 1CDD9: F0 0A */     beq L1CDE5 ; +++
/* 1CDDB: A9 01 */     lda #$01
/* 1CDDD: 60 */        rts
L1CDDE: ; ++
/* 1CDDE: 98 */        tya
/* 1CDDF: 18 */        clc
/* 1CDE0: 69 06 */     adc #$06
/* 1CDE2: A8 */        tay
/* 1CDE3: D0 AC */     bne L1CD91 ; -
L1CDE5: ; +++
/* 1CDE5: A9 00 */     lda #$00
/* 1CDE7: 60 */        rts


; This routine seems to write the transfer buffer when scrolling...
; It checks if the given location has been replaced with a picked
; up gutsblock. If it hasn't, it reads the level map.
; Then it sets up the TSA transfer data.
DrawBlockFromActiveLevelMap:
/* 1CDE8: 20 9C C3 */  jsr SwitchBankStage                         ; $C39C
/* 1CDEB: E6 1C */     inc TSAPPUtransferSize

/* 1CDED: 20 9A CE */  jsr CalculateNametableAddress

/* 1CDF0: A5 0C */     lda $0C
/* 1CDF2: 48 */        pha
/* 1CDF3: A5 0D */      lda $0D
/* 1CDF5: 48 */         pha
/* 1CDF6: A5 0E */      lda $0E
/* 1CDF8: 48 */         pha
/* 1CDF9: A5 05 */      lda $05
/* 1CDFB: 48 */         pha
/* 1CDFC: 85 0C */      sta $0C
/* 1CDFE: A5 04 */      lda $04
/* 1CE00: 29 E0 */      and #$E0
/* 1CE02: 85 0D */      sta $0D
/* 1CE04: A5 04 */      lda $04
/* 1CE06: 29 1F */      and #$1F
/* 1CE08: 0A */         asl a
/* 1CE09: 0A */         asl a
/* 1CE0A: 0A */         asl a
/* 1CE0B: 85 0E */      sta $0E
/* 1CE0D: A0 00 */      ldy #$00
/* 1CE0F: 20 8A CD */   jsr CheckCollisionAgainstActives
/* 1CE12: A8 */         tay
/* 1CE13: 68 */         pla
/* 1CE14: 85 05 */      sta $05
/* 1CE16: 68 */         pla
/* 1CE17: 85 0E */      sta $0E
/* 1CE19: 68 */         pla
/* 1CE1A: 85 0D */      sta $0D
/* 1CE1C: 68 */        pla
/* 1CE1D: 85 0C */     sta $0C
/* 1CE1F: C0 84 */     cpy #$84 ; $84 = hit a decimated gutsblock
/* 1CE21: D0 35 */     bne L1CE58 ; ++                  ; $CE58
/* 1CE23: A4 31 */     ldy CurrentStage
/* 1CE25: A5 05 */     lda $05
/* 1CE27: D9 49 CE */  cmp RoomBossOrder,y
/* 1CE2A: D0 27 */     bne L1CE53 ; +                   ; $CE53
/* 1CE2C: B9 31 CE */  lda SecondGutsblockReplacement,y
/* 1CE2F: D0 3F */     bne L1CE70 ; +++                 ; $CE70

SecondGutsblockReplacement: ;CE31
    ; Block used to replace gutsblocks in boss room
    ; If 0 (and gutsblock is picked up in the boss room), the game crashes
    .byte $5B,$00,$00,$00,$60,$2B
    .byte $8D,$01,$9C,$94,$01,$01
FirstGutsblockReplacement: ;CE3D
    .byte $01,$01,$01,$01,$01,$01
    .byte $8D,$01,$9C,$94,$01,$01
RoomBossOrder: ;CE49 - rooms where boss will appear
    .byte $17,$17,$17,$13,$17,$12
    .byte $27,$28,$2F,$23

L1CE53: ; +
/* 1CE53: B9 3D CE */  lda FirstGutsblockReplacement,y
/* 1CE56: D0 18 */     bne L1CE70 ; +++                                         ; $CE70
L1CE58: ; ++
/* 1CE58: A4 05 */     ldy $05

/* 1CE5A: B9 00 8C */  lda RoomOrderTable,y
/* 1CE5D: 0A */        asl a
/* 1CE5E: A8 */        tay

            ; Load the address of the particular room

/* 1CE5F: B9 30 8C */  lda RoomPointerTable+0,y
/* 1CE62: 85 06 */     sta CurrentRoomPointer
/* 1CE64: B9 31 8C */  lda RoomPointerTable+1,y
/* 1CE67: 85 07 */     sta CurrentRoomPointer+1

/* 1CE69: A5 04 */     lda $04
/* 1CE6B: 4A */        lsr a
/* 1CE6C: 4A */        lsr a
/* 1CE6D: A8 */        tay
/* 1CE6E: B1 06 */     lda (CurrentRoomPointer),y ; Read 4x4 block index
L1CE70: ; +++
/* 1CE70: 48 */        pha
/* 1CE71: A0 00 */      ldy #$00
/* 1CE73: 84 07 */      sty CurrentRoomPointer+1
/* 1CE75: 0A */         asl a
/* 1CE76: 26 07 */      rol CurrentRoomPointer+1
/* 1CE78: 0A */         asl a
/* 1CE79: 26 07 */      rol CurrentRoomPointer+1
/* 1CE7B: A8 */         tay

/* 1CE7C: A9 00 */      lda #<RoomBlockData
/* 1CE7E: 85 06 */      sta CurrentRoomPointer

/* 1CE80: A9 80 */      lda #>RoomBlockData
/* 1CE82: 05 07 */      ora CurrentRoomPointer+1
/* 1CE84: 85 07 */      sta CurrentRoomPointer+1
/* 1CE86: 20 D7 CE */   jsr Write32x32BlockToBuffer                     ; $CED7
/* 1CE89: 20 06 CF */   jsr Adjust32x32BlockAddress                 ; $CF06
/* 1CE8C: 68 */        pla

/* 1CE8D: A8 */        tay
/* 1CE8E: B9 00 83 */  lda RoomBlockPals,y
/* 1CE91: 9D 00 03 */  sta TSAPPUtransfer0NTdata-2,x
/* 1CE94: E8 */        inx
/* 1CE95: 86 0D */     stx $0D
/* 1CE97: 4C B3 C3 */  jmp SwitchBank05                                ; $C3B3

;
; If $05 is even then $09,$10 = #$2023 else $09,$10 = #$2427
;
CalculateNametableAddress:
/* 1CE9A: A2 20 */     ldx #$20
/* 1CE9C: A0 23 */     ldy #$23

/* 1CE9E: A5 05 */     lda $05
/* 1CEA0: 29 01 */     and #$01
/* 1CEA2: F0 04 */     beq L1CEA8 ; +                                           ; $CEA8
/* 1CEA4: A2 24 */     ldx #$24
/* 1CEA6: A0 27 */     ldy #$27
L1CEA8: ; +
/* 1CEA8: 86 10 */     stx $10
/* 1CEAA: 84 09 */     sty $09
;
; $04 = nnnYYYYY
;
;       000000YY YYYnnn00
;
/* 1CEAC: A9 00 */     lda #$00
/* 1CEAE: 85 07 */     sta CurrentRoomPointer+1

/* 1CEB0: A5 04 */     lda $04
/* 1CEB2: 29 1F */     and #$1F
/* 1CEB4: 0A */        asl a
/* 1CEB5: 0A */        asl a
/* 1CEB6: 0A */        asl a
/* 1CEB7: 0A */        asl a
/* 1CEB8: 26 07 */     rol CurrentRoomPointer+1
/* 1CEBA: 0A */        asl a
/* 1CEBB: 26 07 */     rol CurrentRoomPointer+1
/* 1CEBD: 85 06 */     sta CurrentRoomPointer

/* 1CEBF: A5 04 */     lda $04
/* 1CEC1: 29 E0 */     and #$E0
/* 1CEC3: 4A */        lsr a
/* 1CEC4: 4A */        lsr a
/* 1CEC5: 4A */        lsr a
/* 1CEC6: 05 06 */     ora CurrentRoomPointer
/* 1CEC8: A6 0D */     ldx $0D
/* 1CECA: 9D 01 03 */  sta TSAPPUtransfer0NTaddress+1,x

/* 1CECD: A5 10 */     lda $10
/* 1CECF: 05 07 */     ora CurrentRoomPointer+1
/* 1CED1: 9D 00 03 */  sta TSAPPUtransfer0NTaddress+0,x

/* 1CED4: E8 */        inx
/* 1CED5: E8 */        inx

/* 1CED6: 60 */        rts


;
; this routine writes the transfer buffer with the 32x32 block at (CurrentRoomPointer),Y
;

;
; 0   2   8   A
;  (0)     (2)
; 1   3   9   B
;
; 4   6   C   E
;  (1)     (3)
; 5   7   D   F
;
; [16x16] [16x16]
;
; [16x16] [16x16]


Write32x32BlockToBuffer:
/* 1CED7: A9 02 */     lda #$02
/* 1CED9: 85 0E */     sta $0E
L1CEDB: ; --
/* 1CEDB: A9 02 */     lda #$02
/* 1CEDD: 85 0F */     sta $0F
L1CEDF: ; -
; One 16x16 block
/* 1CEDF: B1 06 */     lda (CurrentRoomPointer),y
/* 1CEE1: 0A */        asl a
/* 1CEE2: 0A */        asl a
/* 1CEE3: 18 */        clc
/* 1CEE4: 9D 00 03 */  sta TSAPPUtransfer0NTaddress+0,x
/* 1CEE7: 69 01 */     adc #$01
/* 1CEE9: 9D 01 03 */  sta TSAPPUtransfer0NTaddress+1,x
/* 1CEEC: 69 01 */     adc #$01
/* 1CEEE: 9D 04 03 */  sta TSAPPUtransfer0NTdata+2,x
/* 1CEF1: 69 01 */     adc #$01
/* 1CEF3: 9D 05 03 */  sta TSAPPUtransfer0NTdata+3,x

/* 1CEF6: E8 */        inx
/* 1CEF7: E8 */        inx
/* 1CEF8: C8 */        iny
/* 1CEF9: C6 0F */     dec $0F
/* 1CEFB: D0 E2 */     bne L1CEDF ; -                                           ; $CEDF

/* 1CEFD: E8 */        inx
/* 1CEFE: E8 */        inx
/* 1CEFF: E8 */        inx
/* 1CF00: E8 */        inx
/* 1CF01: C6 0E */     dec $0E
/* 1CF03: D0 D6 */     bne L1CEDB ; --                                          ; $CEDB
/* 1CF05: 60 */        rts



; attribute table writing

Adjust32x32BlockAddress:
/* 1CF06: A5 04 */     lda $04
/* 1CF08: 2A */        rol a

/* 1CF09: 48 */        pha
/* 1CF0A: 2A */         rol a
/* 1CF0B: 2A */         rol a
/* 1CF0C: 2A */         rol a
/* 1CF0D: 29 07 */      and #$07
/* 1CF0F: 85 08 */      sta CurrentRoomPointer+2
/* 1CF11: 68 */        pla

/* 1CF12: 29 38 */     and #$38
/* 1CF14: 05 08 */     ora CurrentRoomPointer+2
/* 1CF16: 09 C0 */     ora #$C0
/* 1CF18: 9D 01 03 */  sta TSAPPUtransfer0NTaddress+1,x
/* 1CF1B: A5 09 */     lda CurrentRoomPointer+3
/* 1CF1D: 9D 00 03 */  sta TSAPPUtransfer0NTaddress+0,x
/* 1CF20: E8 */        inx
/* 1CF21: E8 */        inx
/* 1CF22: 60 */        rts



F1CF23:
/* 1CF23: 20 9C C3 */  jsr SwitchBankStage                             ; $C39C
/* 1CF26: A5 33 */     lda $33
/* 1CF28: 4A */        lsr a
/* 1CF29: 4A */        lsr a
/* 1CF2A: 4A */        lsr a
/* 1CF2B: 4A */        lsr a
/* 1CF2C: 8D 00 03 */  sta TSAPPUtransfer0NTaddress+0
/* 1CF2F: A5 33 */     lda $33
/* 1CF31: 0A */        asl a
/* 1CF32: 0A */        asl a
/* 1CF33: 0A */        asl a
/* 1CF34: 48 */        pha
/* 1CF35: 29 18 */     and #$18
/* 1CF37: 8D 01 03 */  sta TSAPPUtransfer0NTaddress+1
/* 1CF3A: 68 */        pla

/* 1CF3B: 0A */        asl a
/* 1CF3C: 29 C0 */     and #$C0
/* 1CF3E: 0D 01 03 */  ora TSAPPUtransfer0NTaddress+1
/* 1CF41: 8D 01 03 */  sta TSAPPUtransfer0NTaddress+1

/* 1CF44: A5 33 */     lda $33
/* 1CF46: 29 F8 */     and #$F8
/* 1CF48: 09 C0 */     ora #$C0
/* 1CF4A: 8D 13 03 */  sta TSAPPUtransfer0AttrAddress+1

/* 1CF4D: A5 33 */     lda $33
/* 1CF4F: 29 03 */     and #$03
/* 1CF51: 0A */        asl a
/* 1CF52: 0D 13 03 */  ora TSAPPUtransfer0AttrAddress+1
/* 1CF55: 8D 13 03 */  sta TSAPPUtransfer0AttrAddress+1

/* 1CF58: A2 20 */     ldx #$20
/* 1CF5A: A5 1B */     lda ScrollPosScreen
/* 1CF5C: 29 01 */     and #$01
/* 1CF5E: F0 02 */     beq L1CF62 ; +                                           ; $CF62
/* 1CF60: A2 24 */     ldx #$24
L1CF62: ; +
/* 1CF62: 8A */        txa
/* 1CF63: 0D 00 03 */  ora TSAPPUtransfer0NTaddress+0
/* 1CF66: 8D 00 03 */  sta TSAPPUtransfer0NTaddress+0
/* 1CF69: 8A */        txa

/* 1CF6A: 09 03 */     ora #$03
/* 1CF6C: 8D 12 03 */  sta TSAPPUtransfer0AttrAddress+0
/* 1CF6F: A9 00 */     lda #$00
/* 1CF71: 85 0C */     sta $0C
/* 1CF73: A5 33 */     lda $33
/* 1CF75: 29 3B */     and #$3B
/* 1CF77: 4A */        lsr a
/* 1CF78: 66 0C */     ror $0C
/* 1CF7A: 4A */        lsr a
/* 1CF7B: 66 0C */     ror $0C
/* 1CF7D: 4A */        lsr a
/* 1CF7E: 66 0C */     ror $0C
/* 1CF80: 46 0C */     lsr $0C
/* 1CF82: 05 0C */     ora $0C
/* 1CF84: 85 0C */     sta $0C

/* 1CF86: AE 60 04 */  ldx ObjectPosScreen+0                         ; $0460
/* 1CF89: BD 00 8C */  lda RoomOrderTable,x
/* 1CF8C: 0A */        asl a
/* 1CF8D: AA */        tax
/* 1CF8E: BD 30 8C */  lda RoomPointerTable+0,x
/* 1CF91: 85 04 */     sta $04
/* 1CF93: BD 31 8C */  lda RoomPointerTable+1,x
/* 1CF96: 85 05 */     sta $05
/* 1CF98: A2 00 */     ldx #$00
/* 1CF9A: 86 0D */     stx $0D

L1CF9C: ; --
/* 1CF9C: A9 00 */     lda #$00
/* 1CF9E: 85 07 */     sta CurrentRoomPointer+1
/* 1CFA0: A5 0D */     lda $0D

/* 1CFA2: 48 */        pha
/* 1CFA3: A5 0E */     lda $0E
/* 1CFA5: 48 */        pha
/* 1CFA6: A5 05 */     lda $05
/* 1CFA8: 48 */        pha
/* 1CFA9: A5 0C */     lda $0C
/* 1CFAB: 48 */        pha
/* 1CFAC: 29 38 */     and #$38
/* 1CFAE: 0A */        asl a
/* 1CFAF: 0A */        asl a
/* 1CFB0: 85 0D */     sta $0D
/* 1CFB2: A5 0C */     lda $0C
/* 1CFB4: 29 07 */     and #$07
/* 1CFB6: 4A */        lsr a
/* 1CFB7: 6A */        ror a
/* 1CFB8: 6A */        ror a
/* 1CFB9: 6A */        ror a
/* 1CFBA: 85 0E */     sta $0E
/* 1CFBC: AD 60 04 */  lda ObjectPosScreen+0                         ; $0460
/* 1CFBF: 85 0C */     sta $0C
/* 1CFC1: A0 00 */     ldy #$00
/* 1CFC3: 20 8A CD */  jsr CheckCollisionAgainstActives
/* 1CFC6: A8 */        tay
/* 1CFC7: 68 */        pla
/* 1CFC8: 85 0C */     sta $0C
/* 1CFCA: 68 */        pla
/* 1CFCB: 85 05 */     sta $05
/* 1CFCD: 68 */        pla
/* 1CFCE: 85 0E */     sta $0E
/* 1CFD0: 68 */        pla

/* 1CFD1: 85 0D */     sta $0D
/* 1CFD3: C0 84 */     cpy #$84
/* 1CFD5: D0 14 */     bne L1CFEB ; ++
/* 1CFD7: A4 31 */     ldy CurrentStage
/* 1CFD9: AD 60 04 */  lda ObjectPosScreen+0                         ; $0460
/* 1CFDC: DD 49 CE */  cmp RoomBossOrder,x
/* 1CFDF: D0 05 */     bne L1CFE6 ; +
/* 1CFE1: B9 31 CE */  lda SecondGutsblockReplacement,y ; tile to replace shutter with
/* 1CFE4: D0 09 */     bne L1CFEF ; +++
L1CFE6: ; +
/* 1CFE6: B9 3D CE */  lda FirstGutsblockReplacement,y ; well, another tile to replace shutter with
/* 1CFE9: D0 04 */     bne L1CFEF ; +++
L1CFEB: ; ++
/* 1CFEB: A4 0C */     ldy $0C
/* 1CFED: B1 04 */     lda ($04),y ; 4x4 block to be written (index into RoomBlockPals and RoomBlockData)
L1CFEF: ; +++
/* 1CFEF: 85 0F */     sta $0F
/* 1CFF1: 0A */        asl a
/* 1CFF2: 26 07 */     rol CurrentRoomPointer+1
/* 1CFF4: 0A */        asl a
/* 1CFF5: 26 07 */     rol CurrentRoomPointer+1
/* 1CFF7: A8 */        tay
/* 1CFF8: A9 80 */     lda #>RoomBlockData
/* 1CFFA: 05 07 */     ora CurrentRoomPointer+1
/* 1CFFC: 85 07 */     sta CurrentRoomPointer+1
/* 1CFFE: A9 00 */     lda #<RoomBlockData
/* 1D000: 85 06 */     sta CurrentRoomPointer
/* 1D002: A5 33 */     lda $33
/* 1D004: 29 04 */     and #$04
/* 1D006: F0 01 */     beq L1D009 ; +                                           ; $D009
/* 1D008: C8 */        iny
L1D009: ; +
/* 1D009: A9 02 */     lda #$02
/* 1D00B: 85 0E */     sta $0E
L1D00D: ; -
/* 1D00D: B1 06 */     lda (CurrentRoomPointer),y
/* 1D00F: 0A */        asl a
/* 1D010: 0A */        asl a
/* 1D011: 18 */        clc
/* 1D012: 9D 02 03 */  sta TSAPPUtransfer0NTdata+0,x
/* 1D015: 69 01 */     adc #$01
/* 1D017: 9D 0A 03 */  sta TSAPPUtransfer0NTdata+8,x
/* 1D01A: 69 01 */     adc #$01
/* 1D01C: 9D 03 03 */  sta TSAPPUtransfer0NTdata+1,x
/* 1D01F: 69 01 */     adc #$01
/* 1D021: 9D 0B 03 */  sta TSAPPUtransfer0NTdata+9,x
/* 1D024: E8 */        inx
/* 1D025: E8 */        inx
/* 1D026: C8 */        iny
/* 1D027: C8 */        iny
/* 1D028: C6 0E */     dec $0E
/* 1D02A: D0 E1 */     bne L1D00D ; -                                           ; $D00D

/* 1D02C: A5 33 */     lda $33
/* 1D02E: A0 0F */     ldy #$0F
/* 1D030: 29 04 */     and #$04
/* 1D032: F0 02 */     beq L1D036 ; +                                           ; $D036
/* 1D034: A0 F0 */     ldy #$F0
L1D036: ; +
/* 1D036: 8C 14 03 */  sty TSAPPUtransfer0AttrData
/* 1D039: A4 0F */     ldy $0F
/* 1D03B: B9 00 83 */  lda RoomBlockPals,y
/* 1D03E: 2D 14 03 */  and TSAPPUtransfer0AttrAndMask
/* 1D041: A4 0D */     ldy $0D
/* 1D043: 99 15 03 */  sta TSAPPUtransfer0AttrOrMask,y
/* 1D046: A5 0C */     lda $0C
/* 1D048: 09 08 */     ora #$08
/* 1D04A: 85 0C */     sta $0C
/* 1D04C: E6 0D */     inc $0D
/* 1D04E: A5 0D */     lda $0D
/* 1D050: C9 02 */     cmp #$02
/* 1D052: F0 03 */     beq L1D057 ; +                                           ; $D057
/* 1D054: 4C 9C CF */  jmp L1CF9C ; --                                          ; $CF9C
L1D057: ; +
/* 1D057: A9 40 */     lda #$40
/* 1D059: 85 1C */     sta TSAPPUtransferSize

/* 1D05B: A9 FF */     lda #$FF
/* 1D05D: 4D 14 03 */  eor TSAPPUtransfer0AttrData
/* 1D060: 8D 14 03 */  sta TSAPPUtransfer0AttrData
/* 1D063: 4C B3 C3 */  jmp SwitchBank05                                ; $C3B3


OpenFirstDoor:
/* 1D066: A6 31 */     ldx CurrentStage
/* 1D068: BD 6E D0 */  lda FirstDoorShutterDataIndex,x
/* 1D06B: 4C 9B D0 */  jmp AnimateDoor

FirstDoorShutterDataIndex: ;at D06E; indexed by stage number
    .byte $00,$00,$00,$00,$00,$00

OpenSecondDoor:
/* 1D074: A6 31 */     ldx CurrentStage
/* 1D076: BD 7C D0 */  lda SecondDoorShutterDataIndex,x
/* 1D079: 4C 9B D0 */  jmp AnimateDoor

; These values are used as index into RoomShutterInfo.
;
; RoomShutterInfo format:
;
;   first byte: number of edits the shutter does to the scene
;   consequent bytes are pairs of:
;        first byte:  location of the edit on the nametable
;        second byte: four-tile block to put into that position
;   the four-tile blocks are described in RoomShutterBlockData
;   and RoomShutterBlockPals.
;
;
SecondDoorShutterDataIndex: ;at D07C; indexed by stage number
    .byte 9,9,0,9,0,9, 27,0,0,27,18
BossRoomShutterDataIndex: ;at D087; indexed by stage number
    .byte 18,18,9,18,9,18, 27,27,18,27,27

; boss door close (shutter)
CloseBossDoor:
/* 1D092: A6 31 */     ldx CurrentStage
/* 1D094: E0 09 */     cpx #$09 ;don't close door at Wily4 stage
/* 1D096: F0 6E */     beq L1D106 ; ++                                          ; $D106
/* 1D098: BD 87 D0 */  lda BossRoomShutterDataIndex,x

AnimateDoor:
/* 1D09B: EE A0 06 */  inc ObjectLifeCycleCounter+0 ;ensure Megaman won't do anything funny in the meantime
/* 1D09E: 20 9C C3 */  jsr SwitchBankStage                             ; $C39C

/* 1D0A1: 48 */        pha
/* 1D0A2: A9 24 */      lda #$24        ; Machine sound
/* 1D0A4: 20 77 C4 */   jsr IssueSound                                  ; $C477
/* 1D0A7: 68 */        pla

/* 1D0A8: AA */        tax
/* 1D0A9: BD 80 8F */  lda RoomShutterInfo,x
/* 1D0AC: 85 5A */     sta $5A
/* 1D0AE: E8 */        inx
/* 1D0AF: 86 59 */     stx $59
L1D0B1: ; -
/* 1D0B1: 20 9C C3 */  jsr SwitchBankStage                             ; $C39C

/* 1D0B4: A6 59 */     ldx $59
/* 1D0B6: A9 00 */     lda #$00
/* 1D0B8: 85 0D */     sta $0D

/* 1D0BA: AD 60 04 */  lda ObjectPosScreen+0                         ; $0460
/* 1D0BD: 85 05 */     sta $05
/* 1D0BF: BD 80 8F */  lda RoomShutterInfo,x
/* 1D0C2: 85 04 */     sta $04
/* 1D0C4: E8 */        inx
/* 1D0C5: 86 59 */     stx $59

/* 1D0C7: 20 9A CE */  jsr CalculateNametableAddress
/* 1D0CA: A4 59 */     ldy $59
/* 1D0CC: B9 80 8F */  lda RoomShutterInfo,y
/* 1D0CF: 0A */        asl a
/* 1D0D0: 0A */        asl a
/* 1D0D1: 69 40 */     adc #<RoomShutterBlockData
/* 1D0D3: 85 06 */     sta CurrentRoomPointer
/* 1D0D5: A9 8F */     lda #>RoomShutterBlockData
/* 1D0D7: 85 07 */     sta CurrentRoomPointer+1
/* 1D0D9: A0 00 */     ldy #$00
/* 1D0DB: 20 D7 CE */  jsr Write32x32BlockToBuffer                     ; $CED7
/* 1D0DE: 20 06 CF */  jsr Adjust32x32BlockAddress                 ; $CF06

/* 1D0E1: A6 59 */     ldx $59
/* 1D0E3: BC 80 8F */  ldy RoomShutterInfo,x
/* 1D0E6: B9 70 8F */  lda RoomShutterBlockPals,y
/* 1D0E9: 8D 14 03 */  sta TSAPPUtransfer0AttrData
/* 1D0EC: E8 */        inx
/* 1D0ED: 86 59 */     stx $59

; 1 32x32 block to be rendered
/* 1D0EF: A9 01 */     lda #$01
/* 1D0F1: 85 1C */     sta TSAPPUtransferSize

; Pause for 6 frames
/* 1D0F3: A9 06 */     lda #$06
/* 1D0F5: 20 09 D1 */  jsr TimeDelayWithSpriteUpdates                  ; $D109

/* 1D0F8: C6 5A */     dec $5A
/* 1D0FA: D0 B5 */     bne L1D0B1 ; -                                           ; $D0B1

/* 1D0FC: A9 FE */     lda #$FE
/* 1D0FE: 20 77 C4 */  jsr IssueSound                                  ; $C477
/* 1D101: A9 00 */     lda #$00
/* 1D103: 8D A0 06 */  sta ObjectLifeCycleCounter+0 ;restore life to Megaman
L1D106: ; ++
/* 1D106: 4C B3 C3 */  jmp SwitchBank05                                ; $C3B3



TimeDelayWithSpriteUpdates:
/* 1D109: 85 3C */     sta MiscCounter1
L1D10B: ; -
/* 1D10B: 20 31 D1 */  jsr UpdateGraphics                              ; $D131
/* 1D10E: 20 1B C0 */  jsr NextFrame                                   ; $C01B
/* 1D111: C6 3C */     dec MiscCounter1
/* 1D113: D0 F6 */     bne L1D10B ; -                                           ; $D10B
/* 1D115: 60 */        rts




TimeDelayWithAllObjectsHalted:
/* 1D116: 85 3C */     sta MiscCounter1
L1D118: ; -
/* 1D118: EE A0 06 */  inc ObjectLifeCycleCounter+0 ;stall Megaman
/* 1D11B: 20 65 C4 */  jsr LifeCycleTick_forEnemies ;stall everyone else
/* 1D11E: 20 31 D1 */  jsr UpdateGraphics                              ; $D131
/* 1D121: 20 6A D4 */  jsr LifeCycleUntick_forEveryone ;unstall everyone (?)
/* 1D124: 20 1B C0 */  jsr NextFrame                                   ; $C01B
/* 1D127: C6 3C */     dec MiscCounter1
/* 1D129: D0 ED */     bne L1D118 ; -                                           ; $D118

/* 1D12B: A9 00 */     lda #$00
/* 1D12D: 8D A0 06 */  sta ObjectLifeCycleCounter+0 ;bring Megaman back into life
/* 1D130: 60 */        rts



; Updates gameplay? (or more likely only graphics?...)

UpdateGraphics:
/* 1D131: A9 06 */     lda #$06
/* 1D133: 85 42 */     sta CurrentBank
/* 1D135: 8D 06 C0 */  sta BankTable+6

; Hide *all* sprites
/* 1D138: A0 00 */     ldy #$00
/* 1D13A: 84 0D */     sty SpriteCounter
/* 1D13C: A2 40 */     ldx #$40
/* 1D13E: 20 78 D4 */  jsr HideSprites                                 ; $D478

/* 1D141: A5 23 */     lda FrameCounter
/* 1D143: 29 01 */     and #$01
/* 1D145: F0 15 */     beq L1D15C ; +                                           ; $D15C

; Odd frame
/* 1D147: 20 6E D3 */  jsr DrawScoreAndMeters                          ; $D36E


; loop thru objects from #0 to TotalObjects-#1
/* 1D14A: A9 00 */     lda #$00
/* 1D14C: 85 0C */     sta $0C
L1D14E: ; -
/* 1D14E: 20 6E D1 */  jsr DrawObject                                  ; $D16E
/* 1D151: E6 0C */     inc $0C
/* 1D153: A5 54 */     lda TotalObjects
/* 1D155: C5 0C */     cmp $0C
/* 1D157: D0 F5 */     bne L1D14E ; -                                           ; $D14E

/* 1D159: 4C B3 C3 */  jmp SwitchBank05                                ; $C3B3

; Even frame
L1D15C: ; +

; Loop thru objects from TotalObjects-#1 to #0 (backwards)
/* 1D15C: A6 54 */     ldx TotalObjects
/* 1D15E: CA */        dex
/* 1D15F: 86 0C */     stx $0C
L1D161: ; -
/* 1D161: 20 6E D1 */  jsr DrawObject                                  ; $D16E
/* 1D164: C6 0C */     dec $0C
/* 1D166: 10 F9 */     bpl L1D161 ; -                                           ; $D161

/* 1D168: 20 6E D3 */  jsr DrawScoreAndMeters                          ; $D36E
/* 1D16B: 4C B3 C3 */  jmp SwitchBank05                                ; $C3B3


DrawObject:
/* 1D16E: A6 0C */     ldx $0C
/* 1D170: BD 00 06 */  lda ObjectPosY,x
/* 1D173: C9 F8 */     cmp #$F8
/* 1D175: D0 01 */     bne L1D178 ; +                                           ; $D178
/* 1D177: 60 */        rts


L1D178: ; +
/* 1D178: 85 10 */     sta $10
/* 1D17A: BD 00 04 */  lda ObjectSpriteNum,x
/* 1D17D: C9 FF */     cmp #$FF
/* 1D17F: D0 03 */     bne L1D184 ; +                                           ; $D184
/* 1D181: 4C AB D2 */  jmp L1D2AB
L1D184: ; +
/* 1D184: BD 40 06 */  lda ObjectFireDelay,x
/* 1D187: 4A */        lsr a
/* 1D188: 4A */        lsr a
/* 1D189: 4A */        lsr a
/* 1D18A: 4A */        lsr a
/* 1D18B: 18 */        clc
/* 1D18C: 7D 00 04 */  adc ObjectSpriteNum,x
/* 1D18F: 0A */        asl a
/* 1D190: A8 */        tay

/* 1D191: A9 81 */     lda #>MajObjFrameTableAddrs
/* 1D193: 69 00 */     adc #$00
/* 1D195: 85 05 */     sta $05
/* 1D197: A9 86 */     lda #<MajObjFrameTableAddrs
/* 1D199: 85 04 */     sta $04

/* 1D19B: B1 04 */     lda ($04),y
/* 1D19D: 85 00 */     sta $00
/* 1D19F: C8 */        iny
/* 1D1A0: B1 04 */     lda ($04),y
/* 1D1A2: 85 01 */     sta $01



/* 1D1A4: BD 40 04 */  lda ObjectUnknown440,x
/* 1D1A7: 4A */        lsr a
/* 1D1A8: 4A */        lsr a
/* 1D1A9: 4A */        lsr a
/* 1D1AA: 4A */        lsr a
/* 1D1AB: A8 */        tay
/* 1D1AC: C8 */        iny

/* 1D1AD: E0 01 */     cpx #$01
/* 1D1AF: D0 18 */     bne L1D1C9 ; ++                                          ; $D1C9
/* 1D1B1: A5 56 */     lda BossBlinkState
/* 1D1B3: F0 14 */     beq L1D1C9 ; ++                                          ; $D1C9
/* 1D1B5: C6 56 */     dec BossBlinkState
/* 1D1B7: 29 02 */     and #$02
/* 1D1B9: F0 0E */     beq L1D1C9 ; ++                                          ; $D1C9
/* 1D1BB: A5 31 */     lda CurrentStage
/* 1D1BD: C9 05 */     cmp #$05
/* 1D1BF: F0 04 */     beq L1D1C5 ; +                                           ; $D1C5
/* 1D1C1: A9 1A */     lda #$1A
/* 1D1C3: D0 06 */     bne L1D1CB ; +++                                         ; $D1CB
L1D1C5: ; +
/* 1D1C5: A9 75 */     lda #$75
/* 1D1C7: D0 02 */     bne L1D1CB ; +++                                         ; $D1CB
L1D1C9: ; ++
/* 1D1C9: B1 00 */     lda ($00),y     ; Get frame number?
L1D1CB: ; +++
/* 1D1CB: 48 */        pha
/* 1D1CC: BD A0 06 */  lda ObjectLifeCycleCounter,x
/* 1D1CF: F0 03 */     beq L1D1D4 ; +                                           ; $D1D4
/* 1D1D1: 4C EC D1 */  jmp L1D1EC ; +++                                         ; $D1EC
L1D1D4: ; +
/* 1D1D4: BD 40 06 */  lda ObjectFireDelay,x
/* 1D1D7: F0 10 */     beq L1D1E9 ; +                                           ; $D1E9
/* 1D1D9: 29 0F */     and #$0F
/* 1D1DB: A8 */        tay
/* 1D1DC: 88 */        dey
/* 1D1DD: F0 05 */     beq L1D1E4 ; +                                           ; $D1E4
/* 1D1DF: DE 40 06 */  dec ObjectFireDelay,x
/* 1D1E2: D0 05 */     bne L1D1E9 ; ++                                          ; $D1E9
L1D1E4: ; +
/* 1D1E4: A9 00 */     lda #$00
/* 1D1E6: 9D 40 06 */  sta ObjectFireDelay,x
L1D1E9: ; ++
/* 1D1E9: 20 F8 D2 */  jsr F1D2F8

L1D1EC: ; +++
/* 1D1EC: A9 00 */     lda #$00
/* 1D1EE: 85 05 */     sta $05
/* 1D1F0: 68 */        pla
/* 1D1F1: 0A */        asl a
/* 1D1F2: 26 05 */     rol $05
/* 1D1F4: A8 */        tay

/* 1D1F5: A9 80 */     lda #>MajObjFrameAddrs
; To support MajObjFrameAddrs & $100, change this to adc $05
/* 1D1F7: 05 05 */     ora $05
/* 1D1F9: 85 05 */     sta $05
/* 1D1FB: A9 00 */     lda #<MajObjFrameAddrs
/* 1D1FD: 85 04 */     sta $04

/* 1D1FF: B1 04 */     lda ($04),y ; Get tile/attribute data pointer
/* 1D201: 85 00 */     sta $00
/* 1D203: C8 */        iny
/* 1D204: B1 04 */     lda ($04),y
/* 1D206: 85 01 */     sta $01



L1D208:
/* 1D208: A0 00 */     ldy #$00
/* 1D20A: 84 12 */     sty $12

/* 1D20C: B1 00 */     lda ($00),y ; Number of 8x8 sprites
/* 1D20E: 85 0E */     sta $0E
/* 1D210: C8 */        iny

; Get x/y coordinates pointer
/* 1D211: B1 00 */     lda ($00),y
/* 1D213: 0A */        asl a
/* 1D214: A8 */        tay
/* 1D215: B9 AA 82 */  lda TileOffsTableAddrs,y
/* 1D218: 85 02 */     sta $02
/* 1D21A: B9 AB 82 */  lda TileOffsTableAddrs+1,y
/* 1D21D: 85 03 */     sta $03
/* 1D21F: A0 02 */     ldy #$02
/* 1D221: 38 */        sec

; Transpose object x position by x scrolling value so it's drawn where it should
/* 1D222: BD 80 04 */  lda ObjectPosX,x
/* 1D225: E5 1A */     sbc ScrollPosX
/* 1D227: 85 0F */     sta $0F

; Make sprites get flipped horizontally if object's flipped
/* 1D229: BD 20 04 */  lda ObjectFlags,x
/* 1D22C: 29 40 */     and #$40
/* 1D22E: 85 11 */     sta $11

/* 1D230: 20 28 D3 */  jsr BehindBackgroundHack                        ; $D328

/* 1D233: A6 0D */     ldx $0D

/* 1D235: A5 0C */     lda $0C
/* 1D237: D0 09 */     bne L1D242 ; +                                           ; $D242
/* 1D239: A5 55 */     lda MegamanBlinkState
/* 1D23B: 29 02 */     and #$02
/* 1D23D: F0 03 */     beq L1D242 ; +                                           ; $D242
/* 1D23F: 4C A3 D2 */  jmp L1D2A3
            ; Megaman is blinking, so skip it
; +

;
; Loop thru all sprites at spritepage except sprite#0, copying contents
; of ($00),Y to spritepage
; (could this be the main sprite writing routine? YES!)
;
;
; $0F = x coordinate of object
; $10 = y coordinate of object
;
; $00,$01 = pointer to tile data and attribute data
; $02,$03 = pointer to y offset data and x offset data
;
; Y = sprite type
;

L1D242: ; ---
/* 1D242: B1 00 */     lda ($00),y     ; Tile#
/* 1D244: 9D 05 02 */  sta CurrentSpriteData+1,x
/* 1D247: C8 */        iny

/* 1D248: B1 00 */     lda ($00),y     ; Attributes
/* 1D24A: 45 11 */     eor $11
/* 1D24C: 05 13 */     ora $13
/* 1D24E: 9D 06 02 */  sta CurrentSpriteData+2,x
/* 1D251: C8 */        iny
/* 1D252: 84 0D */     sty $0D

/* 1D254: A4 12 */     ldy $12

/* 1D256: B1 02 */     lda ($02),y     ; Get ID of x/y pair
/* 1D258: A8 */        tay
/* 1D259: 18 */        clc

/* 1D25A: B9 3A 83 */  lda SpriteTileYOffs,y ; Get local y coordinate of sprite
/* 1D25D: 30 06 */     bmi L1D265 ; +                                           ; $D265
/* 1D25F: 65 10 */     adc $10
/* 1D261: 90 10 */     bcc L1D273 ; +++                                         ; $D273
/* 1D263: B0 04 */     bcs SkipThisSprite                              ; $D269
L1D265: ; +
; add screen y coordinate to local coordinate and skip sprite
; if still outside screen
/* 1D265: 65 10 */     adc $10
/* 1D267: B0 0A */     bcs L1D273 ; +++                                         ; $D273

; Handle sprite coordinates
; ++
SkipThisSprite:
/* 1D269: A9 F8 */     lda #$F8
/* 1D26B: 9D 04 02 */  sta CurrentSpriteData+0,x
/* 1D26E: A4 0D */     ldy $0D
/* 1D270: 4C 9D D2 */  jmp SkipSprite                                  ; $D29D

L1D273: ; +++
/* 1D273: 9D 04 02 */  sta CurrentSpriteData+0,x
/* 1D276: A5 11 */     lda $11
/* 1D278: F0 06 */     beq L1D280 ; +                                           ; $D280

; Object facing right
/* 1D27A: B9 3A 85 */  lda SpriteTileRightXOffs,y ; Get local x coordinate of sprite
/* 1D27D: 4C 83 D2 */  jmp L1D283

; Object facing left
L1D280: ; +
/* 1D280: B9 3A 84 */  lda SpriteTileLeftXOffs,y ; Get local x coordinate of sprite
L1D283:
/* 1D283: 18 */        clc
/* 1D284: 30 06 */     bmi L1D28C ; +                                           ; $D28C
/* 1D286: 65 0F */     adc $0F
/* 1D288: 90 06 */     bcc L1D290 ; ++                                          ; $D290
/* 1D28A: B0 DD */     bcs SkipThisSprite                              ; $D269
L1D28C: ; +
/* 1D28C: 65 0F */     adc $0F
/* 1D28E: 90 D9 */     bcc SkipThisSprite                              ; $D269
L1D290: ; ++
/* 1D290: 9D 07 02 */  sta CurrentSpriteData+3,x
/* 1D293: A4 0D */     ldy $0D
/* 1D295: E8 */        inx
/* 1D296: E8 */        inx
/* 1D297: E8 */        inx
/* 1D298: E8 */        inx
/* 1D299: E0 FC */     cpx #$FC
/* 1D29B: F0 09 */     beq L1D2A6 ; +                                           ; $D2A6

SkipSprite:
/* 1D29D: E6 12 */     inc $12
/* 1D29F: C6 0E */     dec $0E
/* 1D2A1: D0 9F */     bne L1D242 ; ---                                         ; $D242

;;;;;; This write may affect to random number generator:
L1D2A3:
/* 1D2A3: 86 0D */     stx $0D
/* 1D2A5: 60 */        rts

L1D2A6: ; +
/* 1D2A6: 68 */        pla
/* 1D2A7: 68 */        pla
/* 1D2A8: 4C B3 C3 */  jmp SwitchBank05                                ; $C3B3



L1D2AB:
/* 1D2AB: BD 20 04 */  lda ObjectFlags,x     ; Draw object if not invisible
/* 1D2AE: 29 20 */     and #$20
/* 1D2B0: F0 01 */     beq L1D2B3 ; +                                           ; $D2B3
/* 1D2B2: 60 */        rts
L1D2B3: ; +
/* 1D2B3: BD E0 06 */  lda ObjectType,x
/* 1D2B6: 0A */        asl a
/* 1D2B7: A8 */        tay

; Where is this table? Likely on Bank 6.
/* 1D2B8: B9 70 87 */  lda MinObjFrameTableAddrs,y
/* 1D2BB: 85 00 */     sta $00
/* 1D2BD: B9 71 87 */  lda MinObjFrameTableAddrs+1,y
/* 1D2C0: 85 01 */     sta $01

/* 1D2C2: BD 40 04 */  lda ObjectUnknown440,x
/* 1D2C5: 4A */        lsr a
/* 1D2C6: 4A */        lsr a
/* 1D2C7: 4A */        lsr a
/* 1D2C8: 4A */        lsr a
/* 1D2C9: A8 */        tay
/* 1D2CA: C8 */        iny

/* 1D2CB: B1 00 */     lda ($00),y
/* 1D2CD: 48 */        pha

; jsr F1D2F8 if $6A0,X = 0
/* 1D2CE: BD A0 06 */  lda ObjectLifeCycleCounter,x
/* 1D2D1: F0 03 */     beq L1D2D6 ; +                                           ; $D2D6
/* 1D2D3: 4C D9 D2 */  jmp L1D2D9

L1D2D6: ; +
/* 1D2D6: 20 F8 D2 */  jsr F1D2F8

L1D2D9:
/* 1D2D9: 68 */        pla

/* 1D2DA: 0A */        asl a
/* 1D2DB: A8 */        tay


; if carry set then $00,$01 = $8750,$8751, else $00,$01 = $8650,$8651
/* 1D2DC: B0 0D */     bcs L1D2EB ; +                                           ; $D2EB
/* 1D2DE: B9 50 86 */  lda MinObjFrameAddrs,y
/* 1D2E1: 85 00 */     sta $00
/* 1D2E3: B9 51 86 */  lda MinObjFrameAddrs+1,y
/* 1D2E6: 85 01 */     sta $01
/* 1D2E8: 4C 08 D2 */  jmp L1D208
L1D2EB: ; +
/* 1D2EB: B9 50 87 */  lda MinObjFrameAddrs+$100,y
/* 1D2EE: 85 00 */     sta $00
/* 1D2F0: B9 51 87 */  lda MinObjFrameAddrs+$101,y
/* 1D2F3: 85 01 */     sta $01
/* 1D2F5: 4C 08 D2 */  jmp L1D208



F1D2F8:
/* 1D2F8: A0 00 */     ldy #$00
/* 1D2FA: B1 00 */     lda ($00),y

/* 1D2FC: 48 */        pha
/* 1D2FD: 29 0F */     and #$0F
/* 1D2FF: 85 0F */     sta $0F
/* 1D301: 68 */        pla

/* 1D302: 29 F0 */     and #$F0
/* 1D304: 85 11 */     sta $11
/* 1D306: BD 40 04 */  lda ObjectUnknown440,x
/* 1D309: 29 0F */     and #$0F
/* 1D30B: C5 0F */     cmp $0F
/* 1D30D: B0 05 */     bcs L1D314 ; +                                           ; $D314
/* 1D30F: FE 40 04 */  inc ObjectUnknown440,x
/* 1D312: D0 13 */     bne L1D327 ; +++                                         ; $D327
L1D314: ; +
/* 1D314: BD 40 04 */  lda ObjectUnknown440,x
/* 1D317: 29 F0 */     and #$F0
/* 1D319: C5 11 */     cmp $11
/* 1D31B: B0 05 */     bcs L1D322 ; ++                                          ; $D322
/* 1D31D: 18 */        clc
/* 1D31E: 69 10 */     adc #$10
/* 1D320: D0 02 */     bne L1D324 ; +                                           ; $D324
L1D322: ; ++
/* 1D322: A9 00 */     lda #$00
L1D324: ; +
/* 1D324: 9D 40 04 */  sta ObjectUnknown440,x
L1D327: ; +++
/* 1D327: 60 */        rts



BehindBackgroundHack:
/* 1D328: A5 31 */     lda CurrentStage
/* 1D32A: C9 03 */     cmp #$03 ;Fireman?
/* 1D32C: F0 09 */     beq L1D337 ; +                                           ; $D337
/* 1D32E: C9 07 */     cmp #$07 ;Wily2?
/* 1D330: F0 1D */     beq L1D34F ; ++                                          ; $D34F
L1D332: ; -
; Set object in front of screen
/* 1D332: A9 00 */     lda #$00
/* 1D334: 85 13 */     sta $13
/* 1D336: 60 */        rts

; We're at Fireman's stage
L1D337: ; +

; Set object behind screen if screen# = 8 or 9 and y pos is 76 - 104
; This makes object appear BEHIND the fence on the bridges in Fireman's stage

; Skip this object if screen# < 8 or screen# >= 10
/* 1D337: BD 60 04 */  lda ObjectPosScreen,x
/* 1D33A: C9 08 */     cmp #$08
/* 1D33C: 90 F4 */     bcc L1D332 ; -                                           ; $D332
/* 1D33E: C9 0A */     cmp #$0A
/* 1D340: B0 F0 */     bcs L1D332 ; -                                           ; $D332

; Skip this object if y position < 76 or Y coordinate >= 105
/* 1D342: BD 00 06 */  lda ObjectPosY,x
/* 1D345: C9 69 */     cmp #$69
/* 1D347: B0 E9 */     bcs L1D332 ; -                                           ; $D332
/* 1D349: C9 4C */     cmp #$4C
/* 1D34B: 90 E5 */     bcc L1D332 ; -                                           ; $D332
/* 1D34D: B0 1A */     bcs SetPriorityBehind                           ; $D369

; We're at Wily's second stage
L1D34F: ; ++

; Set object behind screen if screen# = #$1B, #$1C, #$1F or #$20 and y > 185
/* 1D34F: BD 60 04 */  lda ObjectPosScreen,x
/* 1D352: C9 1B */     cmp #$1B
/* 1D354: F0 0C */     beq L1D362 ; +                                           ; $D362
/* 1D356: C9 1C */     cmp #$1C
/* 1D358: F0 08 */     beq L1D362 ; +                                           ; $D362
/* 1D35A: C9 1F */     cmp #$1F
/* 1D35C: F0 04 */     beq L1D362 ; +                                           ; $D362
/* 1D35E: C9 20 */     cmp #$20
/* 1D360: D0 D0 */     bne L1D332 ; -                                           ; $D332
L1D362: ; +
/* 1D362: BD 00 06 */  lda ObjectPosY,x
/* 1D365: C9 B9 */     cmp #$B9
/* 1D367: 90 C9 */     bcc L1D332 ; -                                           ; $D332


; Set object behind screen
SetPriorityBehind:
/* 1D369: A9 20 */     lda #$20
/* 1D36B: 85 13 */     sta $13
/* 1D36D: 60 */        rts


DrawScoreAndMeters:
/* 1D36E: A4 0D */     ldy SpriteCounter
/* 1D370: A5 BB */     lda DrawScoreAndMetersFlag
/* 1D372: 30 75 */     bmi DrawScoreAndMetersRTS                       ; $D3E9

; Draw score...

/* 1D374: A9 68 */     lda #$68        ; start drawing score at x coordinate 104
/* 1D376: 85 0C */     sta $0C
/* 1D378: A2 06 */     ldx #$06
/* 1D37A: A9 01 */     lda #$01
/* 1D37C: 85 12 */     sta $12
L1D37E: ; -
/* 1D37E: A9 10 */     lda #$10        ; y coordinate = 16
/* 1D380: 85 0E */     sta $0E

/* 1D382: B5 72 */     lda Score,x     ; digits use tiles $F0-$F9
/* 1D384: 09 F0 */     ora #$F0
/* 1D386: 85 0F */     sta $0F
/* 1D388: 20 1A D4 */  jsr WriteSprite                                 ; $D41A

/* 1D38B: 18 */        clc             ; x coordinate += 8
/* 1D38C: A5 0C */     lda $0C
/* 1D38E: 69 08 */     adc #$08
/* 1D390: 85 0C */     sta $0C
/* 1D392: CA */        dex
/* 1D393: 10 E9 */     bpl L1D37E ; -                                           ; $D37E

/* 1D395: A5 BB */     lda DrawScoreAndMetersFlag
/* 1D397: D0 50 */     bne DrawScoreAndMetersRTS                       ; $D3E9


; Draw life meter
/* 1D399: A9 FE 85 05 */   lda #$FE
	sta $05 ; index of empty tile
/* 1D39D: A9 FA 85 04 */   lda #$FA
	sta $04 ; index of full tile
/* 1D3A1: A9 01 85 12 */   lda #$01
	sta $12 ;sprite attributes
/* 1D3A5: A9 18 85 0E */   lda #$18
	sta $0C ;Xcoord
/* 1D3A9: A9 48 85 0E */   lda #$48
	sta $0E ;Ycoord
/* 1D3AD: A5 6A */     lda Meters+0
/* 1D3AF: 20 EC D3 */  jsr DrawMeter                                   ; $D3EC

/* 1D3B2: A9 00 85 12 */   lda #$00
	sta $12
/* 1D3B6: A9 10 85 0C */   lda #$10
	sta $0C ;Xcoord


; Draw weapon meter
/* 1D3BA: A6 5F */     ldx WeaponSelect
/* 1D3BC: F0 04 */     beq L1D3C2 ; +                                       ; $D3C2
/* 1D3BE: A9 48 */     lda #$48 ; weapon != P
/* 1D3C0: D0 02 */     bne L1D3C4 ; ++                                      ; $D3C4
L1D3C2: ; +
/* 1D3C2: A9 F8 */     lda #$F8 ; weapon == P
L1D3C4: ; ++
/* 1D3C4: 85 0E */     sta $0E            ; Ycoord
/* 1D3C6: A9 DF 85 05 */   lda #$DF
	sta $05 ; index of empty tile
/* 1D3CA: A9 DB 85 04 */   lda #$DB
	sta $04 ; index of full tile
/* 1D3CE: B5 6A */     lda Meters,x
/* 1D3D0: 20 EC D3 */  jsr DrawMeter                               ; $D3EC

; Draw boss meter if fighting boss
/* 1D3D3: A5 3E */     lda BossCurrentStrategy
/* 1D3D5: F0 12 */     beq DrawScoreAndMetersRTS ; +                                       ; $D3E9
/* 1D3D7: A9 02 85 12 */   lda #$02
	sta $12 ;sprite attributes
/* 1D3DB: A9 28 85 0C */   lda #$28
	sta $0C ;Xcoord
/* 1D3DF: A9 48 85 0E */   lda #$48
	sta $0E ;Ycoord
/* 1D3E3: AD C1 06 */  lda ObjectLifeMeter+1
/* 1D3E6: 20 EC D3 */  jsr DrawMeter                               ; $D3EC
; +
DrawScoreAndMetersRTS:
;;;;;; This write may affect to random number generator:
/* 1D3E9: 84 0D */     sty SpriteCounter
/* 1D3EB: 60 */        rts


;
; A = meter value
; $0C,$0E = x,y coordinates of meter's lowest component
;
; $12 = sprite attribute flags
;
;   Y = index into sprite page
;
; $05 = index of empty tile, also used for fractions (3/4, 1/2, 1/4, empty)
; $04 = index of full tile
;

DrawMeter:
; $10 = number of full 8x8 blocks in meter
; $11 = fractional part of meter
/* 1D3EC: 48 */        pha
/* 1D3ED: 4A */        lsr a
/* 1D3EE: 4A */        lsr a
/* 1D3EF: 85 10 */     sta $10
/* 1D3F1: 68 */        pla
/* 1D3F2: 29 03 */     and #$03
/* 1D3F4: 85 11 */     sta $11

/* 1D3F6: 38 */        sec
/* 1D3F7: A5 05 */     lda $05
/* 1D3F9: E5 11 */     sbc $11
/* 1D3FB: 85 11 */     sta $11

/* 1D3FD: A2 00 */     ldx #$00
L1D3FF: ; -

; if sprite# < number of full blocks, tile#=$04
; else if sprite# = fractional block tile#=$11
; else tile# = $05

/* 1D3FF: E4 10 */     cpx $10
/* 1D401: B0 04 */     bcs L1D407 ; +                                           ; $D407

/* 1D403: A5 04 */     lda $04 ; if $04 != 0 then tile# = $04
/* 1D405: D0 08 */     bne L1D40F ; ++                                          ; $D40F
L1D407: ; +
/* 1D407: D0 04 */     bne L1D40D ; +                                           ; $D40D
/* 1D409: A5 11 */     lda $11
/* 1D40B: D0 02 */     bne L1D40F ; ++                                          ; $D40F
L1D40D: ; +
/* 1D40D: A5 05 */     lda $05
L1D40F: ; ++
/* 1D40F: 85 0F */     sta $0F ;tile #
/* 1D411: 20 1A D4 */  jsr WriteSprite                                 ; $D41A
/* 1D414: E8 */        inx
/* 1D415: E0 07 */     cpx #$07
/* 1D417: D0 E6 */     bne L1D3FF ; -                                           ; $D3FF
/* 1D419: 60 */        rts


;
; $0C,$0E = x,y coordinates of 8x8 sprite
;
; $0F = tile #
;
; $12 = sprite attribute flags
;
;   Y = index into sprite page
;
; unless the second last index has been used, $0E contains y-8
;

WriteSprite:
/* 1D41A: A5 0C */     lda $0C
/* 1D41C: 99 07 02 */  sta CurrentSpriteData+3,y
/* 1D41F: A5 0F */     lda $0F
/* 1D421: 99 05 02 */  sta CurrentSpriteData+1,y
/* 1D424: A5 12 */     lda $12
/* 1D426: 99 06 02 */  sta CurrentSpriteData+2,y
/* 1D429: A5 0E */     lda $0E
/* 1D42B: 99 04 02 */  sta CurrentSpriteData+0,y
/* 1D42E: C0 F8 */     cpy #$F8
/* 1D430: F0 0D */     beq L1D43F ; +                                           ; $D43F
/* 1D432: C8 */        iny
/* 1D433: C8 */        iny
/* 1D434: C8 */        iny
/* 1D435: C8 */        iny
/* 1D436: C9 F8 */     cmp #$F8
/* 1D438: F0 05 */     beq L1D43F ; +                                           ; $D43F
/* 1D43A: 38 */        sec
/* 1D43B: E9 08 */     sbc #$08
/* 1D43D: 85 0E */     sta $0E
L1D43F: ; +
/* 1D43F: 60 */        rts



IncreaseScore:
/* 1D440: A5 05 */     lda $05     ; $05 = xy where xy00 are the points earned
/* 1D442: 29 0F */     and #$0F
/* 1D444: 85 4A */     sta $4A
/* 1D446: A5 05 */     lda $05
/* 1D448: 4A */        lsr a
/* 1D449: 4A */        lsr a
/* 1D44A: 4A */        lsr a
/* 1D44B: 4A */        lsr a
/* 1D44C: 85 4B */     sta $4B
/* 1D44E: A2 00 */     ldx #$00
/* 1D450: 86 4C */     stx $4C
/* 1D452: 86 4D */     stx $4D
/* 1D454: 86 4E */     stx $4E

/* 1D456: A0 05 */     ldy #$05
/* 1D458: 18 */        clc
L1D459: ; -
/* 1D459: B5 74 */     lda Score+2,x
/* 1D45B: 75 4A */     adc $4A,x

/* 1D45D: C9 0A */     cmp #$0A        ; Check if digit > 9
/* 1D45F: 90 02 */     bcc L1D463 ; +                                       ; $D463
/* 1D461: E9 0A */     sbc #$0A

L1D463: ; +
/* 1D463: 95 74 */     sta Score+2,x
/* 1D465: E8 */        inx
/* 1D466: 88 */        dey
/* 1D467: D0 F0 */     bne L1D459 ; -                                       ; $D459
/* 1D469: 60 */        rts


LifeCycleUntick_forEveryone:
/* 1D46A: A2 1F */     ldx #$1F
L1D46C: ; -
/* 1D46C: BD A0 06 */  lda ObjectLifeCycleCounter,x
/* 1D46F: F0 03 */     beq L1D474 ; +                                       ; $0001D474
/* 1D471: DE A0 06 */  dec ObjectLifeCycleCounter,x
L1D474: ; +
/* 1D474: CA */        dex
/* 1D475: 10 F5 */     bpl L1D46C ; -                                       ; $0001D46C
/* 1D477: 60 */        rts


;
; Y = starting sprite index
; X = number of consecutive sprites to hide
;

HideSprites:
/* 1D478: A9 F8 */     lda #$F8

L1D47A: ; -
/* 1D47A: 99 00 02 */  sta SpriteTable,y
/* 1D47D: C8 */        iny
/* 1D47E: C8 */        iny
/* 1D47F: C8 */        iny
/* 1D480: C8 */        iny
/* 1D481: CA */        dex
/* 1D482: D0 F6 */     bne L1D47A ; -                                       ; $0001D47A
/* 1D484: 60 */        rts


MegamanPalette: ;at D485
    .byte $2C,$11 ;P
    .byte $30,$00 ;C
    .byte $30,$12 ;I
    .byte $30,$19 ;B
    .byte $28,$16 ;F
    .byte $38,$00 ;E
    .byte $30,$17 ;G
    .byte $2C,$11 ;M

DisableNMIandPPU:
/* 1D495: A5 FF */     lda PPU2000value
/* 1D497: 29 7F */     and #$7F    ; disables NMI
/* 1D499: 85 FF */     sta PPU2000value
/* 1D49B: 8D 00 20 */  sta $2000

/* 1D49E: A5 FE */     lda PPU2001value
/* 1D4A0: 29 E7 */     and #$E7    ; hides sprites and background
/* 1D4A2: 85 FE */     sta PPU2001value
/* 1D4A4: 8D 01 20 */  sta $2001   ; it's restored automatically by NMI.

/* 1D4A7: 60 */        rts


NMI:
/* 1D4A8: 48 */        pha                     ; Save registers
/* 1D4A9: 8A */        txa
/* 1D4AA: 48 */        pha
/* 1D4AB: 98 */        tya
/* 1D4AC: 48 */        pha


; If game code has not finished its work yet, then just update sound
/* 1D4AD: A5 1F */     lda NMI_GfxUpdateDone
/* 1D4AF: F0 03 */     beq L1D4B4 ; +                   ; $0001D4B4
/* 1D4B1: 4C 4C D5 */  jmp CallSoundCode       ; $D54C
L1D4B4: ; +

/* 1D4B4: 20 95 D4 */  jsr DisableNMIandPPU      ; $D495
/* 1D4B7: AD 02 20 */  lda $2002

/* 1D4BA: A9 00 */     lda #<SpriteTable      ; Do sprite DMA
/* 1D4BC: 8D 03 20 */  sta $2003
/* 1D4BF: A9 02 */     lda #>SpriteTable
/* 1D4C1: 8D 14 40 */  sta $4014

/* 1D4C4: 20 73 D6 */  jsr DoTSAPPUtransfer                 ; $D673

; Update palette if PaletteUpdateDelay != 0
/* 1D4C7: A5 37 */     lda PaletteUpdateDelay
/* 1D4C9: F0 03 */     beq L1D4CE ; +                                       ; $D4CE
/* 1D4CB: 20 0A D6 */  jsr UpdatePalettes                          ; $D60A
L1D4CE: ; +

; Do buffer $380 transfer if RawPPUtransferSize != 0
/* 1D4CE: A5 5E */     lda RawPPUtransferSize
/* 1D4D0: F0 03 */     beq L1D4D5 ; +                                       ; $D4D5
/* 1D4D2: 20 AB D5 */  jsr DoRawPPUtransferWithA                    ; $D5AB
L1D4D5: ; +

/* 1D4D5: A5 1B */     lda ScrollPosScreen
/* 1D4D7: 48 */        pha
/* 1D4D8: A5 1A */     lda ScrollPosX
/* 1D4DA: 48 */        pha

/* 1D4DB: A5 B4 */     lda UseTempScrollX
/* 1D4DD: F0 08 */     beq L1D4E7 ; +                                       ; $D4E7
/* 1D4DF: A5 B3 */     lda TempScrollPosScreen
/* 1D4E1: 85 1B */     sta ScrollPosScreen
/* 1D4E3: A5 B2 */     lda TempScrollPosX
/* 1D4E5: 85 1A */     sta ScrollPosX
L1D4E7: ; +

/* 1D4E7: A5 FF */     lda PPU2000value
/* 1D4E9: 29 FC */     and #$FC
/* 1D4EB: 85 FF */     sta PPU2000value
/* 1D4ED: A5 1B */     lda ScrollPosScreen
/* 1D4EF: 29 01 */     and #$01
/* 1D4F1: 05 FF */     ora PPU2000value
/* 1D4F3: 8D 00 20 */  sta $2000



; Handle scrolling (shake ground/Megaman if Gutsman's landing)
/* 1D4F6: 48 */        pha
/* 1D4F7: AD 02 20 */  lda $2002
/* 1D4FA: A2 04 */     ldx #$04
/* 1D4FC: A5 47 */     lda GutsmanStompCounter
/* 1D4FE: F0 1D */     beq L1D51D ; +++                                     ; $D51D
/* 1D500: C6 47 */     dec GutsmanStompCounter
/* 1D502: 4A */        lsr a
/* 1D503: 90 03 */     bcc L1D508 ; +                                       ; $D508
/* 1D505: 29 03 */     and #$03
/* 1D507: AA */        tax

L1D508: ; +
; Shake Megaman if he's touching ground
/* 1D508: AD 00 04 */  lda ObjectSpriteNum+0                            ; ObjectSpriteNum+0
/* 1D50B: C9 09 */     cmp #$09
/* 1D50D: F0 0E */     beq L1D51D ; +++                                     ; $D51D
/* 1D50F: C9 6F */     cmp #$6F
/* 1D511: F0 0A */     beq L1D51D ; +++                                     ; $D51D
/* 1D513: AD 20 04 */  lda ObjectFlags+0
/* 1D516: 29 F0 */     and #$F0
/* 1D518: 09 02 */     ora #$02
/* 1D51A: 8D 20 04 */  sta ObjectFlags+0
L1D51D: ; +++
/* 1D51D: 18 */        clc
/* 1D51E: A5 1A */     lda ScrollPosX
/* 1D520: 7D 83 D5 */  adc ScreenShakeX,x
/* 1D523: 8D 05 20 */  sta $2005
/* 1D526: 18 */        clc
/* 1D527: A5 1E */     lda ScrollPosY
/* 1D529: 7D 88 D5 */  adc ScreenShakeY,x
/* 1D52C: 8D 05 20 */  sta $2005


/* 1D52F: A5 FE */     lda PPU2001value       ; Enable PPU
/* 1D531: 09 1E */     ora #$1E
/* 1D533: 85 FE */     sta PPU2001value
/* 1D535: 8D 01 20 */  sta $2001
/* 1D538: 68 */        pla


/* 1D539: 09 80 */     ora #$80        ; Enable NMI
/* 1D53B: 85 FF */     sta PPU2000value
/* 1D53D: 8D 00 20 */  sta $2000
/* 1D540: 68 */        pla
/* 1D541: 85 1A */     sta ScrollPosX
/* 1D543: 68 */        pla
/* 1D544: 85 1B */     sta ScrollPosScreen


; BCC = JB
; BCS = JAE

/* 1D546: A9 01 */     lda #$01
/* 1D548: 85 1F */     sta NMI_GfxUpdateDone

/* 1D54A: E6 23 */     inc FrameCounter


CallSoundCode:
/* 1D54C: A9 04 */     lda #$04        ; Switch in sound code bank
/* 1D54E: 8D 04 C0 */  sta BankTable+4

/* 1D551: 20 00 90 */  jsr SoundCodePlay                       ; $9000

; Initialize all newly issued sounds

L1D554: ; -
/* 1D554: A6 45 */     ldx NumIssuedSounds         ; Number of sounds to initialize
/* 1D556: F0 16 */     beq L1D56E ; +++                                 ; $0001D56E

/* 1D558: BD 7F 05 */  lda IssuedSoundsList-1,x    ; Get sound ID


; Only play this sound if sound ID is #$00-#$32 or #$FD-#$FF (commands?)

/* 1D55B: C9 FD */     cmp #$FD        ; Skip SoundCodeInit if sound ID > #$FD
/* 1D55D: B0 04 */     bcs L1D563 ; +                                   ; $0001D563
/* 1D55F: C9 33 */     cmp #$33        ; Skip SoundCodeInit if sound ID > #$33
/* 1D561: B0 07 */     bcs L1D56A ; ++                                  ; $0001D56A
L1D563: ; +
/* 1D563: D0 02 */     bne L1D567 ; +                                   ; $0001D567
/* 1D565: A4 A7 */     ldy SoundCodeParameter     ; Y = SoundCodeParameter if sound ID = #$FD
L1D567: ; +
/* 1D567: 20 03 90 */  jsr SoundCodeInit                       ; $9003
L1D56A: ; ++
/* 1D56A: C6 45 */     dec NumIssuedSounds
/* 1D56C: D0 E6 */     bne L1D554 ; -                                   ; $0001D554

L1D56E: ; +++

/* 1D56E: A5 42 */     lda CurrentBank
/* 1D570: AA */        tax
/* 1D571: 9D 00 C0 */  sta BankTable,x

/* 1D574: A5 0D */     lda $0D
/* 1D576: 45 46 */     eor RandomSeed
/* 1D578: 65 23 */     adc FrameCounter
/* 1D57A: 4A */        lsr a
/* 1D57B: 85 46 */     sta RandomSeed

/* 1D57D: 68 */        pla
/* 1D57E: A8 */        tay
/* 1D57F: 68 */        pla
/* 1D580: AA */        tax
/* 1D581: 68 */        pla
/* 1D582: 40 */        rti


ScreenShakeX: ;At 1D583
.byte $00,$08,$00,$F8,$00

ScreenShakeY: ;At 1D588
.byte $E8,$00,$08,$00,$00


ReadJoypads:
/* 1D58D: A2 01 */     ldx #$01
/* 1D58F: 8E 16 40 */  stx $4016
/* 1D592: CA */        dex
/* 1D593: 8E 16 40 */  stx $4016
/* 1D596: E8 */        inx
L1D597: ; --
/* 1D597: A0 08 */     ldy #$08
L1D599: ; -
/* 1D599: BD 16 40 */  lda $4016,x
/* 1D59C: 85 18 */     sta JoyD0
/* 1D59E: 4A */        lsr a
/* 1D59F: 05 18 */     ora JoyD0
/* 1D5A1: 4A */        lsr a
/* 1D5A2: 76 14 */     ror JoyPads,x
/* 1D5A4: 88 */        dey
/* 1D5A5: D0 F2 */     bne L1D599 ; -                               ; $0001D599

/* 1D5A7: CA */        dex
/* 1D5A8: 10 ED */     bpl L1D597 ; --                              ; $0001D597

/* 1D5AA: 60 */        rts

;
; This is called when nametable scrolls *up*
;


;
; $380 contains data to be transferred to PPU in this format:
;
;
; $00: PPU address high
; $01: PPU address low
; $02: raw data, attribute table?...
; $14: data 2 bytes at a time consisting of the 16x16 block column to fill
;
; A = control flag. If #$FF, DoRawPPUtransferWith394, regular otherwise.
;
; This routine also writes sprite chr every time Megaman changes moves
; up/down to a new screen, transferring 32 bytes at a time

DoRawPPUtransferWithA:
/* 1D5AB: C9 FF */     cmp #$FF
/* 1D5AD: F0 1A */     beq DoRawPPUtransferWith394         ; $0001D5C9

DoRawPPUtransfer:
/* 1D5AF: AD 80 03 */  lda RawPPUtransferAddress+0
/* 1D5B2: 8D 06 20 */  sta $2006
/* 1D5B5: AD 81 03 */  lda RawPPUtransferAddress+1
/* 1D5B8: 8D 06 20 */  sta $2006

/* 1D5BB: A2 00 */     ldx #$00

L1D5BD: ; -
/* 1D5BD: BD 82 03 */  lda RawPPUtransferBuf,x
/* 1D5C0: 8D 07 20 */  sta $2007
/* 1D5C3: E8 */        inx
/* 1D5C4: C6 5E */     dec RawPPUtransferSize
/* 1D5C6: D0 F5 */     bne L1D5BD ; -                               ; $0001D5BD

/* 1D5C8: 60 */        rts
;
; $380 contains data to be transferred to PPU in this format:
;
; $00: PPU address high
; $01: PPU address low
; $14: data 2 bytes at a time consisting of the 16x16 block column to fill

; This is NOT the routine called when scrolling... in fact, it never
; seems to get called at all(???)
DoRawPPUtransferWith394:

/* 1D5C9: A2 00 */     ldx #$00
/* 1D5CB: A0 00 */     ldy #$00

L1D5CD: ; -
/* 1D5CD: BD 80 03 */  lda RawPPUtransferAddress+0,x
/* 1D5D0: 8D 06 20 */  sta $2006
/* 1D5D3: BD 81 03 */  lda RawPPUtransferAddress+1,x
/* 1D5D6: 8D 06 20 */  sta $2006


/* 1D5D9: B9 94 03 */  lda $0394,y
/* 1D5DC: 8D 07 20 */  sta $2007
/* 1D5DF: B9 95 03 */  lda $0395,y
/* 1D5E2: 8D 07 20 */  sta $2007
/* 1D5E5: C8 */        iny
/* 1D5E6: C8 */        iny
/* 1D5E7: 98 */        tya
/* 1D5E8: 29 03 */     and #$03
/* 1D5EA: F0 13 */     beq L1D5FF ; +                               ; $0001D5FF

/* 1D5EC: 18 */        clc                                 ; PPU addr += 32
/* 1D5ED: BD 81 03 */  lda RawPPUtransferAddress+1,x
/* 1D5F0: 69 20 */     adc #$20
/* 1D5F2: 9D 81 03 */  sta RawPPUtransferAddress+1,x
/* 1D5F5: BD 80 03 */  lda RawPPUtransferAddress+0,x
/* 1D5F8: 69 00 */     adc #$00
/* 1D5FA: 9D 80 03 */  sta RawPPUtransferAddress+0,x
/* 1D5FD: D0 CE */     bne L1D5CD ; -                               ; $0001D5CD

L1D5FF: ; +
/* 1D5FF: E8 */        inx
/* 1D600: E8 */        inx
/* 1D601: C6 AA */     dec $AA
/* 1D603: D0 C8 */     bne L1D5CD ; -                               ; $0001D5CD

/* 1D605: A9 00 */     lda #$00
/* 1D607: 85 5E */     sta RawPPUtransferSize
/* 1D609: 60 */        rts



;
; WritePaletteParam =
;

UpdatePalettes:
/* 1D60A: A5 37 */     lda PaletteUpdateDelay     ; Only write palettes every 8th frame(?)
/* 1D60C: 29 07 */     and #$07
/* 1D60E: D0 1B */     bne L1D62B ; ++                              ; $0001D62B



; if bit3 of PaletteUpdateDelay is set then write palette at $38,$39   else    $3A,$3B
/* 1D610: A6 3A */     ldx $3A
/* 1D612: A4 3B */     ldy $3B

/* 1D614: A5 37 */     lda PaletteUpdateDelay
/* 1D616: 29 08 */     and #$08
/* 1D618: F0 04 */     beq L1D61E ; +                               ; $0001D61E
/* 1D61A: A6 38 */     ldx $38
/* 1D61C: A4 39 */     ldy $39
L1D61E: ; +
/* 1D61E: 86 35 */     stx $35
/* 1D620: 84 36 */     sty $36


/* 1D622: A5 34 */     lda WritePaletteParam

/* 1D624: 48 */        pha
/* 1D625: 20 47 D6 */  jsr WritePalette                    ; $D647
/* 1D628: 68 */        pla

/* 1D629: 85 34 */     sta WritePaletteParam

L1D62B: ; ++
; if PaletteUpdateDelay - #1 = 0 then write palette at $3A,$3B
/* 1D62B: C6 37 */     dec PaletteUpdateDelay
/* 1D62D: D0 17 */     bne L1D646 ; +                               ; $0001D646

/* 1D62F: A5 3A */     lda $3A
/* 1D631: 85 35 */     sta $35
/* 1D633: A5 3B */     lda $3B
/* 1D635: 85 36 */     sta $36

/* 1D637: A5 34 */     lda WritePaletteParam
/* 1D639: 20 47 D6 */  jsr WritePalette                    ; $D647


; if BossCurrentStrategy !=0 and BossCurrentStrategy < 5 then BossCurrentStrategy++
/* 1D63C: A5 3E */     lda BossCurrentStrategy
/* 1D63E: F0 06 */     beq L1D646 ; +                               ; $0001D646

/* 1D640: C9 05 */     cmp #$05
/* 1D642: B0 02 */     bcs L1D646 ; +                               ; $0001D646

/* 1D644: E6 3E */     inc BossCurrentStrategy

L1D646: ; +
/* 1D646: 60 */        rts


;
; A = $ssssp000
; p = palette (=0 for BG, =1 for sprites)
; s = palette size (16-240. Use ssss = 0001 for 1 palette, ssss = 0010 for 2)
;
;
;   10 = 1 BG pal
;   18 = 1 sprite pal
;   20 = both pals at once, first BG, then sprite
;
; $35,$36 = palette address
;
;

WritePalette:
/* 1D647: 48 */        pha
/* 1D648: 29 F0 */     and #$F0
/* 1D64A: AA */        tax
/* 1D64B: A9 3F */     lda #$3F
/* 1D64D: 8D 06 20 */  sta $2006
/* 1D650: 68 */        pla

/* 1D651: 0A */        asl a
/* 1D652: 29 10 */     and #$10 ; choose 3F00 or 3F10.
/* 1D654: 8D 06 20 */  sta $2006
/* 1D657: A0 00 */     ldy #$00

L1D659: ; -
/* 1D659: B1 35 */     lda ($35),y
/* 1D65B: 8D 07 20 */  sta $2007
/* 1D65E: C8 */        iny
/* 1D65F: CA */        dex
/* 1D660: D0 F7 */     bne L1D659 ; -                           ; $0001D659


; Meaningless code here?...
/* 1D662: A9 3F */     lda #$3F
/* 1D664: 8D 06 20 */  sta $2006
/* 1D667: 8E 06 20 */  stx $2006 ;write 3F00

/* 1D66A: 8E 06 20 */  stx $2006 ;write 0000
/* 1D66D: 8E 06 20 */  stx $2006
/* 1D670: 86 34 */     stx WritePaletteParam
/* 1D672: 60 */        rts




;
; $300 contains data to be transferred to PPU in this format (size = $15 bytes):
;
; 00-01 : PPU name table address of 32x32 block to write (hi; lo)
; 02-11 : 8x8 tiles, one column at a time, each column consists of 4 rows
; 12-13 : PPU attribute table address of 32x32 block to write (hi; lo)
; 14    : attribute byte of 32x32 block
;
; TSAPPUtransferSize
;   = #%11000000  ; do DoTSABitManipulation
;   = #%00nnnnnn  ; n = number of 32x32 blocks to write
;
;

DoTSAPPUtransfer:
/* 1D673: A5 1C */     lda TSAPPUtransferSize
/* 1D675: D0 03 */     bne L1D67A
/* 1D677: 4C FD D6 */  jmp EndOfTransfer                               ; $D6FD
L1D67A:
/* 1D67A: 29 C0 */     and #$C0
/* 1D67C: F0 03 */     beq L1D681
/* 1D67E: 4C 07 D7 */  jmp DoTSABitManipulation ;$D707
L1D681:
/* 1D681: A5 FF */     lda PPU2000value   ; Set PPU autoincrement to 32 (not 1)
/* 1D683: 09 04 */     ora #$04
/* 1D685: 85 FF */     sta PPU2000value
/* 1D687: 8D 00 20 */  sta $2000

; Initialize the pointer to the data to be written
/* 1D68A: A9 02 */     lda #<TSAPPUtransfer0NTdata
/* 1D68C: 85 0E */     sta $0E
/* 1D68E: A9 03 */     lda #>TSAPPUtransfer0NTdata
/* 1D690: 85 0F */     sta $0F


/* 1D692: A9 00 */     lda #<TSAPPUtransfer0NTaddress
L1D694: ; ---
/* 1D694: 85 0C */     sta $0C
/* 1D696: AA */        tax
/* 1D697: A0 00 */     ldy #$00

/* 1D699: A9 03 */     lda #>TSAPPUtransfer0NTaddress
;;;;;; This write may affect to random number generator:
/* 1D69B: 85 0D */     sta $0D

; if $300,X,$301,X = #$0380 then $0C,$0D = #$0100  (write 2 bytes)
; (so we don't write beyond the vertical boundaries of the nametable)
/* 1D69D: BD 01 03 */  lda TSAPPUtransfer0NTaddress+1,x
/* 1D6A0: 29 80 */     and #$80
/* 1D6A2: 1D 00 03 */  ora TSAPPUtransfer0NTaddress+0,x
/* 1D6A5: 29 83 */     and #$83
/* 1D6A7: C9 83 */     cmp #$83 ;Compare address to $0380
/* 1D6A9: D0 04 */     bne L1D6AF ; +                                           ; $D6AF

/* 1D6AB: A9 01 */     lda #$01
/* 1D6AD: 85 0D */     sta $0D
; +
L1D6AF: ; --
/* 1D6AF: BD 00 03 */  lda TSAPPUtransfer0NTaddress+0,x
/* 1D6B2: 8D 06 20 */  sta $2006
/* 1D6B5: BD 01 03 */  lda TSAPPUtransfer0NTaddress+1,x
/* 1D6B8: 8D 06 20 */  sta $2006
L1D6BB: ; -
/* 1D6BB: B1 0E */     lda ($0E),y
/* 1D6BD: 8D 07 20 */  sta $2007
/* 1D6C0: C8 */        iny
/* 1D6C1: 98 */        tya
/* 1D6C2: 25 0D */     and $0D
/* 1D6C4: D0 F5 */     bne L1D6BB ; -                       ; $0001D6BB

; Compensate if blocks skipped to make sure we're at the right address
/* 1D6C6: A5 0D */     lda $0D
/* 1D6C8: C9 03 */     cmp #$03
/* 1D6CA: F0 02 */     beq L1D6CE ; +                       ; $0001D6CE
/* 1D6CC: C8 */        iny
/* 1D6CD: C8 */        iny
L1D6CE: ; +

; Loop until Y == 16
/* 1D6CE: 98 */        tya
/* 1D6CF: 29 0F */     and #$0F
/* 1D6D1: F0 06 */     beq L1D6D9 ; +                       ; $0001D6D9
/* 1D6D3: FE 01 03 */  inc TSAPPUtransfer0NTaddress+1,x
/* 1D6D6: 4C AF D6 */  jmp L1D6AF ; --                      ; $D6AF
L1D6D9: ; +
; Write attribute byte for the whole 32x32 block
/* 1D6D9: BD 12 03 */  lda TSAPPUtransfer0AttrAddress+0,x
/* 1D6DC: 8D 06 20 */  sta $2006

/* 1D6DF: BD 13 03 */  lda TSAPPUtransfer0AttrAddress+1,x
/* 1D6E2: 8D 06 20 */  sta $2006

/* 1D6E5: BD 14 03 */  lda TSAPPUtransfer0AttrData,x
/* 1D6E8: 8D 07 20 */  sta $2007

/* 1D6EB: C6 1C */     dec TSAPPUtransferSize
/* 1D6ED: F0 0E */     beq EndOfTransfer                               ; $D6FD

/* 1D6EF: 18 */        clc         ; $0E += #21
/* 1D6F0: A5 0E */     lda $0E
/* 1D6F2: 69 15 */     adc #$15
/* 1D6F4: 85 0E */     sta $0E

/* 1D6F6: 18 */        clc         ; $0C += #21
/* 1D6F7: A5 0C */     lda $0C
/* 1D6F9: 69 15 */     adc #$15
/* 1D6FB: D0 97 */     bne L1D694 ; ---                                         ; $D694


EndOfTransfer:
/* 1D6FD: A5 FF */     lda PPU2000value       ; clear #$04, that is, set PPU inc by 1
/* 1D6FF: 29 FB */     and #$FB
/* 1D701: 85 FF */     sta PPU2000value
/* 1D703: 8D 00 20 */  sta $2000
/* 1D706: 60 */        rts

DoTSABitManipulation:
; if TSAPPUtransferSize != #$40 then set bits in PPU-ram as specified in bitset buffer
/* 1D707: A5 1C */     lda TSAPPUtransferSize
/* 1D709: C9 40 */     cmp #$40
/* 1D70B: F0 03 */     beq L1D710
/* 1D70D: 4C 50 D7 */  jmp PPUramBitSet               ; $D750
L1D710:
/* 1D710: A2 00 */     ldx #$00
/* 1D712: 86 1C */     stx TSAPPUtransferSize
L1D714: ; --
/* 1D714: AD 00 03 */  lda TSAPPUtransfer0NTaddress+0
/* 1D717: 8D 06 20 */  sta $2006
/* 1D71A: AD 01 03 */  lda TSAPPUtransfer0NTaddress+1
/* 1D71D: 8D 06 20 */  sta $2006
; Transfer 8 bytes from buffer to PPU-ram
L1D720: ; -
/* 1D720: BD 02 03 */  lda TSAPPUtransfer0NTdata,x
/* 1D723: 8D 07 20 */  sta $2007
/* 1D726: E8 */        inx
/* 1D727: 8A */        txa
/* 1D728: 29 07 */     and #$07
/* 1D72A: D0 F4 */     bne L1D720 ; -                           ; $0001D720

/* 1D72C: 18 */        clc                             ; CPU-ram address += 32
/* 1D72D: AD 01 03 */  lda TSAPPUtransfer0NTaddress+1
/* 1D730: 69 20 */     adc #$20
/* 1D732: 8D 01 03 */  sta TSAPPUtransfer0NTaddress+1

/* 1D735: A4 1C */     ldy TSAPPUtransferSize     ; Move OR mask from pos. Y to pos. 0
/* 1D737: B9 15 03 */  lda TSAPPUtransfer0AttrOrMask,y
/* 1D73A: 8D 15 03 */  sta TSAPPUtransfer0AttrOrMask
/* 1D73D: A0 00 */     ldy #$00
/* 1D73F: 20 66 D7 */  jsr PPUramBitSetSingle          ; $D766

/* 1D742: E6 1C */     inc TSAPPUtransferSize
/* 1D744: EE 13 03 */  inc TSAPPUtransfer0AttrAddress+1

/* 1D747: E0 10 */     cpx #$10
/* 1D749: D0 C9 */     bne L1D714 ; --                          ; $0001D714

/* 1D74B: A9 00 */     lda #$00
/* 1D74D: 85 1C */     sta TSAPPUtransferSize
/* 1D74F: 60 */        rts



;
; TSAPPUtransferSize = %0?00nnnn
;
;   n =  number of bytes to perform bit operations on
;
; Among other things, this is used to flash boss selection in stage select
;

PPUramBitSet:
/* 1D750: A5 1C */     lda TSAPPUtransferSize
/* 1D752: 29 0F */     and #$0F
/* 1D754: AA */        tax
/* 1D755: A0 00 */     ldy #$00

L1D757: ; -
/* 1D757: 20 66 D7 */  jsr PPUramBitSetSingle      ; $D766
/* 1D75A: C8 */        iny
/* 1D75B: C8 */        iny
/* 1D75C: C8 */        iny
/* 1D75D: C8 */        iny
/* 1D75E: CA */        dex
/* 1D75F: D0 F6 */     bne L1D757 ; -                       ; $0001D757

/* 1D761: A9 00 */     lda #$00    ;
/* 1D763: 85 1C */     sta TSAPPUtransferSize
/* 1D765: 60 */        rts


; PPU addr = word at $0312[Y]
; It will read 1 byte
; And then read another, and write it (back & $0314[Y]) | ($0315[Y])
PPUramBitSetSingle:
/* 1D766: B9 12 03 */  lda TSAPPUtransfer0AttrAddress+0,y
/* 1D769: 8D 06 20 */  sta $2006
/* 1D76C: B9 13 03 */  lda TSAPPUtransfer0AttrAddress+1,y
/* 1D76F: 8D 06 20 */  sta $2006

/* 1D772: AD 07 20 */  lda $2007
/* 1D775: AD 07 20 */  lda $2007
/* 1D778: 39 14 03 */  and TSAPPUtransfer0AttrAndMask,y
/* 1D77B: 19 15 03 */  ora TSAPPUtransfer0AttrOrMask,y

/* 1D77E: 48 */        pha
/* 1D77F: B9 12 03 */  lda TSAPPUtransfer0AttrAddress+0,y
/* 1D782: 8D 06 20 */  sta $2006
/* 1D785: B9 13 03 */  lda TSAPPUtransfer0AttrAddress+1,y
/* 1D788: 8D 06 20 */  sta $2006
/* 1D78B: 68 */        pla

/* 1D78C: 8D 07 20 */  sta $2007
/* 1D78F: 60 */        rts


AnalyzeCurrentTile:
/* 1D790: A2 02 */     ldx #$02
/* 1D792: A0 00 */     ldy #$00
/* 1D794: AD 20 04 */  lda ObjectFlags+0
/* 1D797: 29 DF */     and #$DF          ;~0x20
/* 1D799: 8D 20 04 */  sta ObjectFlags+0
L1D79C: ; -
/* 1D79C: B5 2A */     lda $2A,x
/* 1D79E: 10 12 */     bpl L1D7B2
; Negative tiles:
/* 1D7A0: 29 7F */     and #$7F
/* 1D7A2: C9 03 */     cmp #$03
/* 1D7A4: D0 06 */     bne L1D7AC ; +
; Set scrolling = left (huh?) --- THIS ACTUALLY CAUSES A GLITCH IN ROCKMAN 2
/* 1D7A6: A9 02 */     lda #$02
/* 1D7A8: 85 26 */     sta CurrentStripeEndType
/* 1D7AA: D0 0E */     bne L1D7BA ;unconditional jump
L1D7AC: ; +
/* 1D7AC: C9 04 */     cmp #$04
/* 1D7AE: F0 26 */     beq L1D7D6
/* 1D7B0: D0 08 */     bne L1D7BA
; Positive tiles:
L1D7B2:
/* 1D7B2: C9 01 */     cmp #$01
/* 1D7B4: F0 04 */     beq L1D7BA
/* 1D7B6: C9 04 */     cmp #$04
/* 1D7B8: D0 04 */     bne L1D7BE
L1D7BA:
/* 1D7BA: A0 01 */     ldy #$01
/* 1D7BC: D0 1B */     bne L1D7D9 ;unconditional jump

; Spike?
; Spike kills in all three positions.
L1D7BE:
/* 1D7BE: C9 03 */     cmp #$03
/* 1D7C0: F0 1C */     beq SpikeKill1

; Ladder?
/* 1D7C2: C9 02 */     cmp #$02
/* 1D7C4: D0 0A */     bne L1D7D0 ; +
/* 1D7C6: A5 30 */     lda CurrentTileState
/* 1D7C8: 1D DB D7 */  ora CurrentTileStateBits,x
/* 1D7CB: 85 30 */     sta CurrentTileState
/* 1D7CD: 4C D6 D7 */  jmp L1D7D6
L1D7D0: ; +
/* 1D7D0: C9 05 */     cmp #$05
/* 1D7D2: D0 02 */     bne L1D7D6
/* 1D7D4: 85 94 */     sta MegamanWalkTimer
L1D7D6:
/* 1D7D6: CA */        dex
/* 1D7D7: 10 C3 */     bpl L1D79C ; -
L1D7D9:
/* 1D7D9: 98 */        tya
/* 1D7DA: 60 */        rts

CurrentTileStateBits:
    .byte $08 ;ladder here
    .byte $04 ;ladder above
    .byte $02 ;ladder above that

SpikeKill1:
/* 1D7DE: 4C 19 C2 */  jmp MegaManKilled                               ; $C219

F1D7E1:
/* 1D7E1: A2 02 */     ldx #$02
/* 1D7E3: A9 00 */     lda #$00
/* 1D7E5: 85 96 */     sta $96
L1D7E7:
/* 1D7E7: B4 2A */     ldy $2A,x
/* 1D7E9: 10 06 */     bpl L1D7F1
/* 1D7EB: C0 84 */     cpy #$84
/* 1D7ED: D0 10 */     bne L1D7FF
/* 1D7EF: F0 10 */     beq L1D801

L1D7F1:
/* 1D7F1: C0 03 */     cpy #$03
/* 1D7F3: F0 26 */     beq SpikeKill2
/* 1D7F5: C0 01 */     cpy #$01
/* 1D7F7: F0 06 */     beq L1D7FF
/* 1D7F9: C0 04 */     cpy #$04
/* 1D7FB: D0 04 */     bne L1D801
/* 1D7FD: 85 96 */     sta $96
L1D7FF:
/* 1D7FF: 09 01 */     ora #$01
L1D801:
/* 1D801: CA */        dex
/* 1D802: 10 E3 */     bpl L1D7E7
/* 1D804: A8 */        tay
/* 1D805: A6 2F */     ldx RefObjectNum
/* 1D807: D0 10 */     bne L1D819 ; ++                                          ; $D819
/* 1D809: 98 */        tya
/* 1D80A: D0 0D */     bne L1D819 ; ++                                          ; $D819
/* 1D80C: AD 80 06 */  lda ObjectYSpeed+0
/* 1D80F: 10 08 */     bpl L1D819 ; ++                                          ; $D819
/* 1D811: A5 30 */     lda CurrentTileState
/* 1D813: C9 01 */     cmp #$01
/* 1D815: D0 02 */     bne L1D819 ; ++                                          ; $D819
/* 1D817: A0 01 */     ldy #$01

L1D819: ; ++
/* 1D819: 98 */        tya
/* 1D81A: 60 */        rts


SpikeKill2:
/* 1D81B: 4C 19 C2 */  jmp MegaManKilled                               ; $C219

; First byte is bank number, second byte is number of 256-bytes to write
StageSelectionGFXinfo: ; at $D81E
    .byte $00,$02
    .byte $00,$02
    .byte $00,$02
    .byte $00,$01
    .byte $00,$01
    .byte $00,$03
    .byte $00,$03
    .byte $02,$01
    .byte $02,$01
    .byte $00,$08
; Addresses corresponding to $D81E, telling which 256-byte sequences are sourced.
StageSelectionGFXsource: ; at $D822
    .word $9800
    .word $A800
    .word $A000
    .word $A400
    .word $A700
    .word $AC00
    .word $9C00
    .word $BA00
    .word $9700
    .word $B800

;; Begin the stage selection screen.
ExecStageSelectionScreen:
/* 1D846: A9 06 */     lda #$06
/* 1D848: 85 42 */     sta CurrentBank
/* 1D84A: 8D 06 C0 */  sta BankTable+6
/* 1D84D: A9 00 */     lda #$00
/* 1D84F: 8D 06 20 */  sta $2006 ;PPU address=$0000
/* 1D852: 8D 06 20 */  sta $2006
/* 1D855: A9 00 */     lda #$00
/* 1D857: 85 0C */     sta $0C
L1D859: ; ---
/* 1D859: 0A */        asl a
/* 1D85A: AA */        tax
/* 1D85B: BD 1E D8 */  lda StageSelectionGFXinfo+0,x
/* 1D85E: A8 */        tay
/* 1D85F: 85 42 */     sta CurrentBank
/* 1D861: 99 00 C0 */  sta BankTable,y

/* 1D864: BD 32 D8 */  lda StageSelectionGFXsource+0,x
/* 1D867: 85 04 */     sta $04
/* 1D869: BD 33 D8 */  lda StageSelectionGFXsource+1,x
/* 1D86C: 85 05 */     sta $05
/* 1D86E: BD 1F D8 */  lda StageSelectionGFXinfo+1,x
/* 1D871: AA */        tax

L1D872: ; --
/* 1D872: A0 00 */     ldy #$00
L1D874: ; -
/* 1D874: B1 04 */     lda ($04),y
/* 1D876: 8D 07 20 */  sta $2007
/* 1D879: C8 */        iny
/* 1D87A: D0 F8 */     bne L1D874 ; -                                           ; $D874

/* 1D87C: E6 05 */     inc $05
/* 1D87E: CA */        dex
/* 1D87F: D0 F1 */     bne L1D872 ; --
/* 1D881: E6 0C */     inc $0C
/* 1D883: A5 0C */     lda $0C
/* 1D885: C9 0A */     cmp #$0A
/* 1D887: D0 D0 */     bne L1D859 ; ---
/* 1D889: A9 06 */     lda #$06
/* 1D88B: 85 42 */     sta CurrentBank
/* 1D88D: 8D 06 C0 */  sta BankTable+6
/* 1D890: 20 F6 BF */  jsr RunStageSelectionScreen
/* 1D893: 4C B3 C3 */  jmp SwitchBank05                                ; $C3B3

LoadEnemies:
/* 1D896: A5 3E */     lda BossCurrentStrategy ; No enemy loading if fighting boss
/* 1D898: F0 01 */     beq L1D89B ; +
L1D89A: ; -
/* 1D89A: 60 */        rts
L1D89B: ; +
/* 1D89B: A9 06 */     lda #$06
/* 1D89D: 8D 06 C0 */  sta BankTable+6
/* 1D8A0: 85 42 */     sta CurrentBank
/* 1D8A2: A5 8F */     lda ZigZagFireStatus
/* 1D8A4: F0 03 */     beq L1D8A9 ; +
/* 1D8A6: 20 02 DB */  jsr UpdateZigZagFire
L1D8A9: ; +
/* 1D8A9: A5 92 */     lda AutoSpawnObjectFFcounter
/* 1D8AB: F0 03 */     beq L1D8B0 ; +
/* 1D8AD: 20 66 C6 */  jsr SpawnObject_TypeIsFF
L1D8B0: ; +
/* 1D8B0: A5 8D */     lda ScreenMovedFlag
/* 1D8B2: 29 01 */     and #$01
/* 1D8B4: F0 E4 */     beq L1D89A ; - ; Quit if nothing has changed
/* 1D8B6: A5 8D */     lda ScreenMovedFlag
/* 1D8B8: 29 FE */     and #$FE
/* 1D8BA: D0 03 */     bne LoadEnemies_Forward
/* 1D8BC: 4C 21 D9 */  jmp LoadEnemies_Backward

LoadEnemies_Forward:
/* 1D8BF: 18 */        clc
/* 1D8C0: A5 1A */     lda ScrollPosX
/* 1D8C2: 69 FF */     adc #$FF
/* 1D8C4: 85 04 */     sta $04        ;current screen X position -1

/* 1D8C6: A5 1B */     lda ScrollPosScreen
/* 1D8C8: 69 00 */     adc #$00
/* 1D8CA: 85 05 */     sta $05        ;current screen number -0

/* 1D8CC: A5 8C */     lda CurrentEnemyIndex
/* 1D8CE: 20 89 D9 */  jsr LoadEnemyNumber
/* 1D8D1: A0 00 */     ldy #$00
L1D8D3: ; -
/* 1D8D3: B1 06 */     lda (CurrentRoomPointer),y ; load screen number
/* 1D8D5: C5 05 */     cmp $05
/* 1D8D7: 90 0C */     bcc L1D8E5 ; +                                           ; $D8E5
/* 1D8D9: D0 21 */     bne L1D8FC ; ++                                          ; $D8FC
/* 1D8DB: C8 */        iny
/* 1D8DC: B1 06 */     lda (CurrentRoomPointer),y ; load X coordinate for testing
/* 1D8DE: 88 */        dey
/* 1D8DF: C5 04 */     cmp $04
/* 1D8E1: F0 02 */     beq L1D8E5 ; +                                           ; $D8E5
/* 1D8E3: B0 17 */     bcs L1D8FC ; ++                                          ; $D8FC
L1D8E5: ; +
/* 1D8E5: C8 */        iny
/* 1D8E6: B1 06 */     lda (CurrentRoomPointer),y
/* 1D8E8: 85 00 */     sta $00 ;store X coordinate
/* 1D8EA: C8 */        iny
/* 1D8EB: B1 06 */     lda (CurrentRoomPointer),y
/* 1D8ED: 85 01 */     sta $01 ;store Y coordinate
/* 1D8EF: C8 */        iny
/* 1D8F0: B1 06 */     lda (CurrentRoomPointer),y ;load enemy number
/* 1D8F2: C8 */        iny
/* 1D8F3: A6 8C */     ldx CurrentEnemyIndex
/* 1D8F5: 20 AD D9 */  jsr SpawnObject
/* 1D8F8: E6 8C */     inc CurrentEnemyIndex
/* 1D8FA: D0 D7 */     bne L1D8D3 ; -                                           ; $D8D3
L1D8FC: ; ++
/* 1D8FC: A5 8B */     lda PreviousEnemyIndex
/* 1D8FE: 18 */        clc
/* 1D8FF: 69 01 */     adc #$01
/* 1D901: 20 89 D9 */  jsr LoadEnemyNumber
L1D904: ; -
/* 1D904: B1 06 */     lda (CurrentRoomPointer),y
/* 1D906: C5 1B */     cmp ScrollPosScreen
/* 1D908: 90 0C */     bcc L1D916 ; ++
/* 1D90A: D0 12 */     bne L1D91E ; +++
/* 1D90C: C8 */        iny
/* 1D90D: B1 06 */     lda (CurrentRoomPointer),y
/* 1D90F: C5 1A */     cmp ScrollPosX
/* 1D911: 90 02 */     bcc L1D915 ; +
/* 1D913: D0 09 */     bne L1D91E ; +++
L1D915: ; +
/* 1D915: 88 */        dey
L1D916: ; ++
/* 1D916: C8 */        iny
/* 1D917: C8 */        iny
/* 1D918: C8 */        iny
/* 1D919: C8 */        iny
/* 1D91A: E6 8B */     inc PreviousEnemyIndex
/* 1D91C: D0 E6 */     bne L1D904 ; -
L1D91E: ; +++
/* 1D91E: 4C B3 C3 */  jmp SwitchBank05                                ; $C3B3

LoadEnemies_Backward:
/* 1D921: A5 1B */     lda ScrollPosScreen
/* 1D923: 85 05 */     sta $05
/* 1D925: A5 1A */     lda ScrollPosX
/* 1D927: 85 04 */     sta $04
L1D929: ; -
/* 1D929: A5 8B */     lda PreviousEnemyIndex
/* 1D92B: C9 FF */     cmp #$FF
/* 1D92D: F0 2B */     beq L1D95A ; ++
/* 1D92F: 20 89 D9 */  jsr LoadEnemyNumber
/* 1D932: B1 06 */     lda (CurrentRoomPointer),y
/* 1D934: C5 1B */     cmp ScrollPosScreen
/* 1D936: 90 22 */     bcc L1D95A ; ++
/* 1D938: D0 08 */     bne L1D942 ; +
/* 1D93A: C8 */        iny
/* 1D93B: B1 06 */     lda (CurrentRoomPointer),y
/* 1D93D: 88 */        dey
/* 1D93E: C5 1A */     cmp ScrollPosX
/* 1D940: 90 18 */     bcc L1D95A ; ++
L1D942: ; +
/* 1D942: C8 */        iny
/* 1D943: B1 06 */     lda (CurrentRoomPointer),y
/* 1D945: 85 00 */     sta $00
/* 1D947: C8 */        iny
/* 1D948: B1 06 */     lda (CurrentRoomPointer),y
/* 1D94A: 85 01 */     sta $01
/* 1D94C: C8 */        iny
/* 1D94D: B1 06 */     lda (CurrentRoomPointer),y
/* 1D94F: C8 */        iny
/* 1D950: A6 8B */     ldx PreviousEnemyIndex
/* 1D952: 20 AD D9 */  jsr SpawnObject
/* 1D955: C6 8B */     dec PreviousEnemyIndex
/* 1D957: 4C 29 D9 */  jmp L1D929 ; -
L1D95A: ; ++
/* 1D95A: 18 */        clc
/* 1D95B: A5 1A */     lda ScrollPosX
/* 1D95D: 69 FF */     adc #$FF
/* 1D95F: 85 04 */     sta $04
/* 1D961: A5 1B */     lda ScrollPosScreen
/* 1D963: 69 00 */     adc #$00
/* 1D965: 85 05 */     sta $05
L1D967:
/* 1D967: A5 8C */     lda CurrentEnemyIndex
/* 1D969: F0 1B */     beq L1D986 ; ++
/* 1D96B: 38 */        sec
/* 1D96C: E9 01 */     sbc #$01
/* 1D96E: 20 89 D9 */  jsr LoadEnemyNumber
/* 1D971: B1 06 */     lda (CurrentRoomPointer),y
/* 1D973: C5 05 */     cmp $05
/* 1D975: 90 0F */     bcc L1D986 ; ++
/* 1D977: D0 09 */     bne L1D982 ; +
/* 1D979: C8 */        iny
/* 1D97A: B1 06 */     lda (CurrentRoomPointer),y
/* 1D97C: C5 04 */     cmp $04
/* 1D97E: 90 06 */     bcc L1D986 ; ++
/* 1D980: F0 04 */     beq L1D986 ; ++
L1D982: ; +
/* 1D982: C6 8C */     dec CurrentEnemyIndex
/* 1D984: D0 E1 */     bne L1D967
L1D986: ; ++
/* 1D986: 4C B3 C3 */  jmp SwitchBank05                                ; $C3B3


LoadEnemyNumber:
/* 1D989: A6 00 */     ldx $00
/* 1D98B: 86 07 */     stx CurrentRoomPointer+1
/* 1D98D: 0A */        asl a
/* 1D98E: 26 07 */     rol CurrentRoomPointer+1
/* 1D990: 0A */        asl a
/* 1D991: 26 07 */     rol CurrentRoomPointer+1
/* 1D993: 85 06 */     sta CurrentRoomPointer
/* 1D995: A5 31 */     lda CurrentStage
/* 1D997: 0A */        asl a
/* 1D998: AA */        tax
/* 1D999: 18 */        clc
/* 1D99A: BD 52 A4 */  lda EnemyDataPointers+0,x
/* 1D99D: 65 06 */     adc CurrentRoomPointer
/* 1D99F: 85 06 */     sta CurrentRoomPointer
/* 1D9A1: A5 07 */     lda CurrentRoomPointer+1
/* 1D9A3: 29 03 */     and #$03
/* 1D9A5: 7D 53 A4 */  adc EnemyDataPointers+1,x
/* 1D9A8: 85 07 */     sta CurrentRoomPointer+1
/* 1D9AA: A0 00 */     ldy #$00
/* 1D9AC: 60 */        rts


SpawnObject:
;; Spawn a new object
;;;;  A = object type
;;;;  Y = ?
;;;;  X = weapon power?
;;;;  $00 = object pos x
;;;;  $01 = object pos y
;;;;  $05 = object screen number
/* 1D9AD: 85 02 */     sta $02
/* 1D9AF: 84 03 */     sty $03
/* 1D9B1: C9 FF */     cmp #$FF
/* 1D9B3: D0 06 */     bne L1D9BB ; +
; If the object type is $FF
/* 1D9B5: 20 66 C6 */  jsr SpawnObject_TypeIsFF
/* 1D9B8: 4C 2F DA */  jmp EndSpawnObject
L1D9BB: ; +
/* 1D9BB: 86 0C */     stx $0C
/* 1D9BD: A5 01 */     lda $01
/* 1D9BF: C9 FE */     cmp #$FE
/* 1D9C1: D0 03 */     bne L1D9C6
/* 1D9C3: 4C 32 DA */  jmp SpawnObjectYisFE
L1D9C6:
/* 1D9C6: 8A */        txa
/* 1D9C7: A2 0F */     ldx #$0F
L1D9C9: ; -
/* 1D9C9: D5 7B */     cmp $7B,x
/* 1D9CB: F0 05 */     beq L1D9D2 ; +
/* 1D9CD: CA */        dex
/* 1D9CE: 10 F9 */     bpl L1D9C9 ; -
/* 1D9D0: 30 03 */     bmi L1D9D5 ; ++
; -
L1D9D2: ; +
/* 1D9D2: A4 03 */     ldy $03
/* 1D9D4: 60 */        rts
L1D9D5: ; ++
/* 1D9D5: A2 10 */     ldx #$10
/* 1D9D7: 20 76 C5 */  jsr FindFreeObject                              ; FindFreeObject
/* 1D9DA: B0 F6 */     bcs L1D9D2 ; -                                           ; $D9D2

/* 1D9DC: A5 0C */     lda $0C
/* 1D9DE: 95 6B */     sta Meters+1,x
F1D9E0:
/* 1D9E0: A9 FF */     lda #$FF
/* 1D9E2: 9D 00 04 */  sta ObjectSpriteNum,x

/* 1D9E5: A5 02 */     lda $02  ;object type
/* 1D9E7: 9D E0 06 */  sta ObjectType,x
/* 1D9EA: A5 05 */     lda $05  ;screen number
/* 1D9EC: 9D 60 04 */  sta ObjectPosScreen,x
/* 1D9EF: A5 00 */     lda $00  ;x coordinate
/* 1D9F1: 9D 80 04 */  sta ObjectPosX,x

/* 1D9F4: A5 01 */     lda $01  ;y coordinate
/* 1D9F6: C9 FF */     cmp #$FF
/* 1D9F8: D0 03 */     bne L1D9FD ; +                                           ; $D9FD
/* 1D9FA: 4C 64 DA */  jmp SpawnObject_YisFF ; $DA64
L1D9FD: ; +
/* 1D9FD: 9D 00 06 */  sta ObjectPosY,x

/* 1DA00: A4 02 */     ldy $02  ;object type
/* 1DA02: B9 80 A3 */  lda DefaultObjectFlags,y
/* 1DA05: 9D 20 04 */  sta ObjectFlags,x
/* 1DA08: 98 */        tya
/* 1DA09: 48 */        pha

/* 1DA0A: B9 C6 A3 */   lda DefaultObjectSpeedCtrl,y
/* 1DA0D: A8 */         tay
/* 1DA0E: 20 33 F5 */   jsr InitObjectDefaultSpeed
/* 1DA11: 68 */        pla

/* 1DA12: A8 */        tay
/* 1DA13: B9 0C A4 */  lda DefaultObjectFireDelay,y
/* 1DA16: 9D 40 06 */  sta ObjectFireDelay,x

/* 1DA19: A9 14 */     lda #$14
/* 1DA1B: 9D C0 06 */  sta ObjectLifeMeter,x

/* 1DA1E: A9 00 */     lda #$00
/* 1DA20: 9D 40 04 */  sta ObjectUnknown440,x
/* 1DA23: 9D A0 06 */  sta ObjectLifeCycleCounter,x
/* 1DA26: 9D 20 06 */  sta ObjectPosYfraction,x
/* 1DA29: 9D A0 04 */  sta ObjectPosXfraction,x
/* 1DA2C: 9D 80 05 */  sta IssuedSoundsList,x
EndSpawnObject:
/* 1DA2F: A4 03 */     ldy $03
/* 1DA31: 60 */        rts

SpawnObjectYisFE:
/* 1DA32: A5 02 */     lda $02 ;object type
/* 1DA34: 85 0C */     sta $0C
/* 1DA36: A2 10 */     ldx #$10
L1DA38: ; -
/* 1DA38: 20 16 F5 */  jsr FindObjectOfSelectedType
/* 1DA3B: B0 24 */     bcs L1DA61 ; +++ ; If the object was not found.
/* 1DA3D: BD 20 04 */  lda ObjectFlags,x
/* 1DA40: 09 08 */     ora #$08
/* 1DA42: 9D 20 04 */  sta ObjectFlags,x ;Set unknown flag
/* 1DA45: A5 02 */     lda $02
/* 1DA47: C9 1F */     cmp #$1F
/* 1DA49: F0 09 */     beq L1DA54 ; +
/* 1DA4B: C9 09 */     cmp #$09
/* 1DA4D: D0 0F */     bne L1DA5E ; ++
; ;Object type: 09
/* 1DA4F: A9 40 */     lda #$40
/* 1DA51: 9D 40 06 */  sta ObjectFireDelay,x
L1DA54: ; + ;Object type: 1F
/* 1DA54: A9 04 */     lda #$04
/* 1DA56: 9D 80 06 */  sta ObjectYSpeed,x
/* 1DA59: A9 00 */     lda #$00
/* 1DA5B: 9D 60 06 */  sta ObjectYSpeedFraction,x
L1DA5E: ; ++
/* 1DA5E: E8 */        inx
/* 1DA5F: D0 D7 */     bne L1DA38 ; -
L1DA61: ; +++
/* 1DA61: A4 03 */     ldy $03
/* 1DA63: 60 */        rts


SpawnObject_YisFF:
/* 1DA64: 8A */        txa
/* 1DA65: 48 */        pha
/* 1DA66: A9 00 */     lda #$00
/* 1DA68: 85 8F */     sta ZigZagFireStatus
/* 1DA6A: A5 02 */     lda $02 ;object type
/* 1DA6C: C9 2C */     cmp #$2C
/* 1DA6E: F0 16 */     beq L1DA86 ; ++
/* 1DA70: 85 90 */     sta $90
/* 1DA72: A2 01 */     ldx #$01
/* 1DA74: C9 06 */     cmp #$06
/* 1DA76: D0 06 */     bne L1DA7E ; +
; ; "WATCHERS" MANAGER
/* 1DA78: A2 7E */     ldx #$7E
/* 1DA7A: A9 04 */     lda #$04
/* 1DA7C: 85 8F */     sta ZigZagFireStatus
L1DA7E: ; + ; Probably, ZIGZAG FIRE
/* 1DA7E: 86 91 */     stx $91
/* 1DA80: 20 02 DB */  jsr UpdateZigZagFire
/* 1DA83: 68 */        pla
/* 1DA84: AA */        tax
/* 1DA85: 60 */        rts
L1DA86: ; ++ ; LIFT MANAGER
/* 1DA86: A5 31 */     lda CurrentStage
/* 1DA88: C9 05 */     cmp #$05
/* 1DA8A: D0 06 */     bne L1DA92 ; +                                           ; $DA92
/* 1DA8C: A0 02 */     ldy #$02 ; Gutsman stage. Eat three.
/* 1DA8E: A2 00 */     ldx #$00
/* 1DA90: F0 04 */     beq L1DA96 ; ++                                          ; $DA96
L1DA92: ; +
/* 1DA92: A2 15 */     ldx #$15 ; If it's not Gutsman stage.
/* 1DA94: A0 00 */     ldy #$00
L1DA96: ; ++
/* 1DA96: 86 0C */     stx $0C ; index into LiftData.
/* 1DA98: 84 0D */     sty $0D ; number of elements to read from that table.
L1DA9A: ; -
/* 1DA9A: A9 2C */     lda #$2C
/* 1DA9C: 20 63 F6 */  jsr CreateEnemy   ;create a lift.

/* 1DA9F: A4 0C */     ldy $0C
/* 1DAA1: B9 EC DA */  lda LiftData+6,y
/* 1DAA4: 95 6B */     sta Meters+1,x

/* 1DAA6: A9 20 */     lda #$20    ; Make object invisible and non-collidable
/* 1DAA8: 9D 20 04 */  sta ObjectFlags,x
/* 1DAAB: B9 E6 DA */  lda LiftData+0,y
/* 1DAAE: 9D 80 04 */  sta ObjectPosX,x
/* 1DAB1: B9 E7 DA */  lda LiftData+1,y
/* 1DAB4: 9D 60 04 */  sta ObjectPosScreen,x
/* 1DAB7: B9 E8 DA */  lda LiftData+2,y
/* 1DABA: 9D 00 06 */  sta ObjectPosY,x
/* 1DABD: B9 E9 DA */  lda LiftData+3,y
/* 1DAC0: 9D 40 06 */  sta ObjectFireDelay,x
/* 1DAC3: B9 EA DA */  lda LiftData+4,y
/* 1DAC6: 9D 60 06 */  sta ObjectYSpeedFraction,x
/* 1DAC9: B9 EB DA */  lda LiftData+5,y
/* 1DACC: 9D C0 06 */  sta ObjectLifeMeter,x

/* 1DACF: 98 */        tya
/* 1DAD0: 18 */        clc
/* 1DAD1: 69 07 */     adc #$07
/* 1DAD3: 85 0C */     sta $0C ;next lift pointer
/* 1DAD5: A9 00 */     lda #$00 ;Make the lift move 1 pix/s to the right
/* 1DAD7: 9D E0 04 */  sta ObjectXSpeedFraction,x
/* 1DADA: A9 01 */     lda #$01
/* 1DADC: 9D C0 04 */  sta ObjectXSpeed,x
/* 1DADF: C6 0D */     dec $0D ;number of lifts
/* 1DAE1: 10 B7 */     bpl L1DA9A ; -
/* 1DAE3: 68 */        pla
/* 1DAE4: AA */        tax
/* 1DAE5: 60 */        rts

LiftData: ; at DAE6. Contains 7-byte records.
        ; Bytes:
        ;  0=ObjectPosX
        ;  1=ObjectPosScreen (screen number)
        ;  2=ObjectPosY
        ;  3=ObjectFireDelay (#$40 = moves left, #$00 = moves right)
        ;  4=ObjectYSpeedFraction (?)
        ;  5=ObjectLifeMeter (lift index)
        ;  6=Meters+1 (huh?)
    .byte $F0,$03,$47,$40,$00,$00,$00 ; In Gutsman stages, these three are used at once.
    .byte $88,$03,$87,$40,$00,$01,$00
    .byte $20,$04,$C7,$40,$08,$02,$00
    .byte $78,$1B,$87,$40,$00,$03,$02 ; Otherwise, this only will be used.


UpdateZigZagFire:
/* 1DB02: C6 91 */     dec $91
/* 1DB04: F0 01 */     beq L1DB07 ; +
/* 1DB06: 60 */        rts
L1DB07: ; +
/* 1DB07: A2 10 */     ldx #$10
/* 1DB09: 20 76 C5 */  jsr FindFreeObject                              ; FindFreeObject
/* 1DB0C: B0 44 */     bcs L1DB52
/* 1DB0E: A0 03 */     ldy #$03
/* 1DB10: A5 90 */     lda $90
/* 1DB12: 85 02 */     sta $02
L1DB14: ; -
/* 1DB14: D9 53 DB */  cmp DB53_table,y
/* 1DB17: F0 03 */     beq L1DB1C ; +                                           ; $DB1C
/* 1DB19: 88 */        dey
/* 1DB1A: 10 F8 */     bpl L1DB14 ; -                                           ; $DB14
L1DB1C: ; +
/* 1DB1C: 98 */        tya
/* 1DB1D: 0A */        asl a
/* 1DB1E: A8 */        tay
/* 1DB1F: B9 57 DB */  lda DB57_table+0,y
/* 1DB22: 85 04 */     sta $04
/* 1DB24: B9 58 DB */  lda DB57_table+1,y
/* 1DB27: 85 05 */     sta $05
/* 1DB29: A4 8F */     ldy ZigZagFireStatus
/* 1DB2B: B1 04 */     lda ($04),y
/* 1DB2D: 85 91 */     sta $91
/* 1DB2F: C8 */        iny
/* 1DB30: B1 04 */     lda ($04),y
/* 1DB32: 85 00 */     sta $00
/* 1DB34: C8 */        iny
/* 1DB35: B1 04 */     lda ($04),y
/* 1DB37: 85 01 */     sta $01
/* 1DB39: C8 */        iny
/* 1DB3A: 84 8F */     sty ZigZagFireStatus

/* 1DB3C: 18 */        clc
/* 1DB3D: A5 1A */     lda ScrollPosX
/* 1DB3F: 65 00 */     adc $00
/* 1DB41: 85 00 */     sta $00

/* 1DB43: A5 1B */     lda ScrollPosScreen
/* 1DB45: 69 00 */     adc #$00
/* 1DB47: 85 05 */     sta $05
/* 1DB49: 20 E0 D9 */  jsr F1D9E0
/* 1DB4C: A5 91 */     lda $91
/* 1DB4E: D0 02 */     bne L1DB52 ; +                                           ; $DB52
/* 1DB50: 85 8F */     sta ZigZagFireStatus
L1DB52: ; +
/* 1DB52: 60 */        rts


DB53_table: ; at DB53
    .byte 0,9,6,14
DB57_table: ; at DB57
    .word Loc_DB5F
    .word Loc_DB68
    .word Loc_DB6A
    .word Loc_DB80
Loc_DB5F:
    .byte $31,$04,$4C
    .byte $23,$04,$8C
    .byte $00,$04,$5C
Loc_DB68:
    .byte $02,$78
Loc_DB6A:
    .byte $08,$00,$88
    .byte $08,$02,$58,$08,$30,$A8,$E0,$30,$38,$08,$18,$C8
    .byte $E0,$18,$50,$08,$00,$98,$E0
Loc_DB80:
    .byte $1F,$B0,$04
    .byte $1F,$B0,$04
    .byte $1F,$B0,$04
    .byte $00,$B0,$04

;
; Do boss AI?...
;
RunBossAI:
/* 1DB8C: A5 3E */     lda BossCurrentStrategy
;
; 0 or 1: kill boss (?)
; 2: close boss door, then call init
; 3: pose (in BossAI_Begin)
; 4: still something (BossAI_Begin)
; 5+: play!
;
/* 1DB8E: C9 02 */     cmp #$02
/* 1DB90: B0 09 */     bcs L1DB9B ; +                                       ; $DB9B
/* 1DB92: A9 00 */     lda #$00
/* 1DB94: 8D C1 06 */  sta ObjectLifeMeter+1
/* 1DB97: 60 */        rts
L1DB98: ; -
/* 1DB98: 4C 21 DC */  jmp BossAI_Begin
L1DB9B: ; +
/* 1DB9B: D0 FB */     bne L1DB98 ; -                                       ; $DB98

/* 1DB9D: A5 31 */     lda CurrentStage
/* 1DB9F: C5 AC */     cmp FightingBossNum
/* 1DBA1: D0 0C */     bne L1DBAF ; ++                                      ; $DBAF
/* 1DBA3: A5 3C */     lda MiscCounter1
/* 1DBA5: F0 08 */     beq L1DBAF ; ++                                      ; $DBAF
/* 1DBA7: C6 3C */     dec MiscCounter1
/* 1DBA9: D0 03 */     bne L1DBAE ; +                                       ; $DBAE
/* 1DBAB: 20 92 D0 */  jsr CloseBossDoor
L1DBAE: ; +
/* 1DBAE: 60 */        rts
L1DBAF: ; ++
; Not native boss battle, or boss door closed.
/* 1DBAF: A5 AC */     lda FightingBossNum
/* 1DBB1: C9 06 */     cmp #$06
/* 1DBB3: 90 0F */     bcc F1DBC4
/* 1DBB5: F0 0A */     beq L1DBC1
/* 1DBB7: C9 08 */     cmp #$08
/* 1DBB9: 90 03 */     bcc L1DBBE
; Boss 8 or 9
/* 1DBBB: 4C 05 F1 */  jmp Boss8and9init
; Boss 7 (Rockman clone)
L1DBBE:
/* 1DBBE: 4C 57 EF */  jmp Boss7Init
; Boss 6 (Statue)
L1DBC1:
/* 1DBC1: 4C 94 F0 */  jmp Boss6Init
; Bosses 0-5
F1DBC4:
/* 1DBC4: A2 00 */     ldx #$00
/* 1DBC6: 86 2F */     stx RefObjectNum
/* 1DBC8: 86 43 */     stx BossVariable43
/* 1DBCA: 86 44 */     stx BossVariable44
/* 1DBCC: E8 */        inx
/* 1DBCD: A0 C4 */     ldy #$C4
/* 1DBCF: 20 33 F5 */  jsr InitObjectDefaultSpeed
/* 1DBD2: 20 7B F6 */  jsr InitActor
/* 1DBD5: CA */        dex
/* 1DBD6: 8E C1 06 */  stx ObjectLifeMeter+1
/* 1DBD9: A6 AC */     ldx FightingBossNum
/* 1DBDB: BD 91 ED */  lda BossInitialStatus,x
/* 1DBDE: 8D 01 04 */  sta ObjectSpriteNum+1
/* 1DBE1: BD 9A ED */  lda BossInitialXcoord,x
/* 1DBE4: 8D 81 04 */  sta ObjectPosX+1
/* 1DBE7: BD A3 ED */  lda BossInitialYcoord,x
/* 1DBEA: 8D 01 06 */  sta ObjectPosY+1
/* 1DBED: A9 17 */     lda #$17
/* 1DBEF: 8D 21 04 */  sta ObjectFlags+1
/* 1DBF2: EE 41 04 */  inc ObjectUnknown440+1
/* 1DBF5: E6 3E */     inc BossCurrentStrategy
/* 1DBF7: A9 5E */     lda #$5E
/* 1DBF9: 85 3F */     sta BossVariable3F
/* 1DBFB: E6 2F */     inc RefObjectNum
/* 1DBFD: 20 3D C5 */  jsr C53D_routine
/* 1DC00: A5 AC */     lda FightingBossNum
/* 1DC02: D0 05 */     bne L1DC09 ; +
/* 1DC04: A9 26 */     lda #$26        ; Issue "Cutman sound" for boss 0
/* 1DC06: 20 77 C4 */  jsr IssueSound                                  ; $C477
L1DC09: ; +
/* 1DC09: C5 31 */     cmp CurrentStage ; Is this the boss's natural battle?
/* 1DC0B: F0 13 */     beq L1DC20 ; +
; No, it's a rematch
/* 1DC0D: A5 31 */     lda CurrentStage
/* 1DC0F: C9 07 */     cmp #$07
/* 1DC11: D0 0D */     bne L1DC20 ; +
; So it's in Wily2, not Wily4. Move the boss to the screen left side.
/* 1DC13: A9 40 */     lda #$40
/* 1DC15: 8D 81 04 */  sta ObjectPosX+1
/* 1DC18: AD 21 04 */  lda ObjectFlags+1
/* 1DC1B: 09 40 */     ora #$40
/* 1DC1D: 8D 21 04 */  sta ObjectFlags+1
L1DC20: ; +
/* 1DC20: 60 */        rts


BossAI_Begin:
; A = BossCurrentStrategy (5-n = actual boss strategies)
/* 1DC21: C9 03 */     cmp #$03
/* 1DC23: D0 3A */     bne L1DC5F ; +++
; Pose
/* 1DC25: A5 AC */     lda FightingBossNum
/* 1DC27: C9 02 */     cmp #$02
/* 1DC29: D0 21 */     bne L1DC4C ; ++               ;DC4C
/* 1DC2B: AD 41 04 */  lda ObjectUnknown440+1 ;bombman only
/* 1DC2E: C9 3C */     cmp #$3C
/* 1DC30: D0 11 */     bne L1DC43 ; +
/* 1DC32: A9 0C */      lda #$0C
/* 1DC34: 20 17 EE */   jsr Boss_FindAmmoSlot ;Bombman poses with a bomb.
/* 1DC37: A9 30 */      lda #$30
/* 1DC39: 9D 60 06 */   sta ObjectYSpeedFraction,x
/* 1DC3C: A9 03 */      lda #$03
/* 1DC3E: 9D 80 06 */   sta ObjectYSpeed,x
/* 1DC41: E6 44 */      inc BossVariable44
L1DC43: ; +
/* 1DC43: A5 3F */     lda BossVariable3F
/* 1DC45: C9 3A */     cmp #$3A
/* 1DC47: B0 03 */     bcs L1DC4C ; ++                                          ; $DC4C
/* 1DC49: 20 9B F3 */  jsr IncObjectLifeMeter                            ; $F39B
L1DC4C: ; ++
; Not bombman, or bombman's pose done
/* 1DC4C: C6 3F */     dec BossVariable3F
/* 1DC4E: AD 41 04 */  lda ObjectUnknown440+1
/* 1DC51: D0 0B */     bne L1DC5E ; +                                           ; $DC5E
; Done pose
/* 1DC53: E6 3E */     inc BossCurrentStrategy
/* 1DC55: A5 AC */     lda FightingBossNum
/* 1DC57: C9 06 */     cmp #$06
/* 1DC59: B0 03 */     bcs L1DC5E ; +                                           ; $DC5E
/* 1DC5B: CE 01 04 */  dec ObjectSpriteNum+1
L1DC5E: ; +
/* 1DC5E: 60 */        rts
L1DC5F: ; +++
/* 1DC5F: A5 3E */     lda BossCurrentStrategy
/* 1DC61: C9 04 */     cmp #$04
/* 1DC63: D0 0A */     bne L1DC6F ; ++                                      ; $DC6F
/* 1DC65: 20 9B F3 */  jsr IncObjectLifeMeter                        ; $F39B
/* 1DC68: C6 3F */     dec BossVariable3F
/* 1DC6A: D0 02 */     bne L1DC6E ; +                                       ; $DC6E
/* 1DC6C: E6 3E */     inc BossCurrentStrategy
L1DC6E: ; +
/* 1DC6E: 60 */        rts
L1DC6F: ; ++
/* 1DC6F: A9 00 */     lda #$00
/* 1DC71: 85 68 */     sta ForcedInputFlag   ; Let the player start playing!
/* 1DC73: 38 */        sec
/* 1DC74: A5 3E */     lda BossCurrentStrategy
/* 1DC76: E9 05 */     sbc #$05
/* 1DC78: 0A */        asl a
/* 1DC79: A8 */        tay
/* 1DC7A: A5 AC */     lda FightingBossNum
/* 1DC7C: 0A */        asl a
/* 1DC7D: AA */        tax
/* 1DC7E: BD AC ED */  lda BossAI_Table+0,x
/* 1DC81: 85 00 */     sta $00
/* 1DC83: BD AD ED */  lda BossAI_Table+1,x
/* 1DC86: 85 01 */     sta $01
/* 1DC88: 6C 00 00 */  jmp ($0000)

BossAI_Cutman:
/* 1DC8B: A5 56 */     lda BossBlinkState
/* 1DC8D: F0 06 */     beq L1DC95 ; +
/* 1DC8F: C9 1F */     cmp #$1F
/* 1DC91: 90 02 */     bcc L1DC95 ; +
/* 1DC93: A0 28 */     ldy #$28
L1DC95: ; +
/* 1DC95: B9 C0 DF */  lda CutmanStrategies+0,y
/* 1DC98: 48 */        pha
/* 1DC99: B9 C1 DF */  lda CutmanStrategies+1,y
/* 1DC9C: 48 */        pha
/* 1DC9D: A5 44 */     lda BossVariable44
/* 1DC9F: D0 03 */     bne L1DCA4 ; +
/* 1DCA1: 4C 55 DD */  jmp CutmanGotoStrategy
L1DCA4: ; +
/* 1DCA4: A9 12 */     lda #$12
/* 1DCA6: 8D 41 06 */  sta ObjectFireDelay+1
/* 1DCA9: A9 45 */     lda #$45
/* 1DCAB: A2 04 */     ldx #$04
/* 1DCAD: 20 86 C5 */  jsr FindLastObjectOfType
/* 1DCB0: A5 44 */     lda BossVariable44
/* 1DCB2: C9 02 */     cmp #$02
/* 1DCB4: F0 41 */     beq CutmanCutterReturning

CutmanCutterGoingOut:
/* 1DCB6: BD 00 06 */  lda ObjectPosY,x
/* 1DCB9: C9 18 */     cmp #$18
/* 1DCBB: 90 12 */     bcc CutmanCutterFlipDirection
/* 1DCBD: C9 D8 */     cmp #$D8
/* 1DCBF: B0 0E */     bcs CutmanCutterFlipDirection
/* 1DCC1: BD 80 04 */  lda ObjectPosX,x
/* 1DCC4: C9 18 */     cmp #$18
/* 1DCC6: 90 07 */     bcc CutmanCutterFlipDirection
/* 1DCC8: C9 E8 */     cmp #$E8
/* 1DCCA: B0 03 */     bcs CutmanCutterFlipDirection
/* 1DCCC: 4C 55 DD */  jmp CutmanGotoStrategy

CutmanCutterFlipDirection:
/* 1DCCF: E6 44 */     inc BossVariable44
; Flip object's direction
/* 1DCD1: BD 20 04 */  lda ObjectFlags,x
/* 1DCD4: 49 40 */     eor #$40
/* 1DCD6: 9D 20 04 */  sta ObjectFlags,x

/* 1DCD9: BD 80 06 */  lda ObjectYSpeed,x
/* 1DCDC: 49 FF */     eor #$FF
/* 1DCDE: 9D 80 06 */  sta ObjectYSpeed,x
/* 1DCE1: BD 60 06 */  lda ObjectYSpeedFraction,x
/* 1DCE4: 49 FF */     eor #$FF                   ;  value = -value
/* 1DCE6: 18 */        clc
/* 1DCE7: 69 01 */     adc #$01
/* 1DCE9: 9D 60 06 */  sta ObjectYSpeedFraction,x ; negated now
/* 1DCEC: A9 60 */     lda #$60
/* 1DCEE: 85 48 */     sta $48
/* 1DCF0: A9 02 */     lda #$02
/* 1DCF2: 85 49 */     sta $49
/* 1DCF4: 4C 55 DD */  jmp CutmanGotoStrategy

CutmanCutterReturning:
/* 1DCF7: A5 23 */     lda FrameCounter
/* 1DCF9: 29 03 */     and #$03
/* 1DCFB: D0 58 */     bne CutmanGotoStrategy
/* 1DCFD: BD 20 04 */  lda ObjectFlags,x
/* 1DD00: 29 BF */     and #$BF            ;clears #$40
/* 1DD02: 9D 20 04 */  sta ObjectFlags,x
/* 1DD05: 38 */        sec


/* 1DD06: AD 81 04 */  lda ObjectPosX+1           ; Compare to Cutman's X location
/* 1DD09: FD 80 04 */  sbc ObjectPosX,x
/* 1DD0C: 90 0C */     bcc L1DD1A ; +                                           ; $DD1A
/* 1DD0E: 85 02 */     sta $02

; Turn object
/* 1DD10: BD 20 04 */  lda ObjectFlags,x
/* 1DD13: 09 40 */     ora #$40            ;sets #$40
/* 1DD15: 9D 20 04 */  sta ObjectFlags,x

/* 1DD18: D0 06 */     bne L1DD20 ; ++                                          ; $DD20
L1DD1A: ; +
/* 1DD1A: 49 FF */     eor #$FF
/* 1DD1C: 69 01 */     adc #$01
/* 1DD1E: 85 02 */     sta $02      ;Distance of cutter from boss
L1DD20: ; ++
/* 1DD20: 18 */        clc
/* 1DD21: A5 48 */     lda $48      ;This was #$60, right?
/* 1DD23: 69 04 */     adc #$04
/* 1DD25: 85 48 */     sta $48
/* 1DD27: 85 00 */     sta $00      ;New value for XSpeedFraction
/* 1DD29: A5 49 */     lda $49      ;This was #$02, right?
/* 1DD2B: 69 00 */     adc #$00
/* 1DD2D: 85 49 */     sta $49
/* 1DD2F: 85 01 */     sta $01      ;New value for XSpeed

/* 1DD31: 38 */        sec
/* 1DD32: BD 00 06 */  lda ObjectPosY,x
/* 1DD35: ED 01 06 */  sbc ObjectPosY+1
/* 1DD38: 85 03 */     sta $03      ;Compare to Cutman's Y location
/* 1DD3A: 20 C6 F8 */  jsr F8C6_routine
/* 1DD3D: A5 03 */     lda $03
/* 1DD3F: C9 08 */     cmp #$08     ;If it's Y-wise farther than 8 pixels, let it live
/* 1DD41: B0 12 */     bcs CutmanGotoStrategy
/* 1DD43: A5 02 */     lda $02
/* 1DD45: C9 08 */     cmp #$08     ;If it's X-wise farther than 8 pixels, let it live
/* 1DD47: B0 0C */     bcs CutmanGotoStrategy
/* 1DD49: A9 F8 */     lda #$F8     ;The cutter reached Cutman's hand. Instant-kill it.
/* 1DD4B: 9D 00 06 */  sta ObjectPosY,x
/* 1DD4E: A9 00 */     lda #$00
/* 1DD50: 85 44 */     sta BossVariable44          ;Scissor be gone.
/* 1DD52: 8D 41 06 */  sta ObjectFireDelay+1

CutmanGotoStrategy:
/* 1DD55: 68 */        pla
/* 1DD56: 85 05 */     sta $05
/* 1DD58: 68 */        pla
/* 1DD59: 85 04 */     sta $04
/* 1DD5B: AD A1 06 */  lda ObjectLifeCycleCounter+1
/* 1DD5E: F0 01 */     beq L1DD61 ; +                                           ; $DD61
/* 1DD60: 60 */        rts
L1DD61: ; +
/* 1DD61: 6C 04 00 */  jmp ($0004)

CutmanStrategyDefault:
/* 1DD64: A2 06 */     ldx #$06
/* 1DD66: 20 D5 ED */  jsr BossSearchMegaman
/* 1DD69: C9 40 */     cmp #$40
/* 1DD6B: B0 12 */     bcs L1DD7F ; +                                           ; $DD7F
/* 1DD6D: E8 */        inx
/* 1DD6E: AD 01 06 */  lda ObjectPosY+1
/* 1DD71: CD 00 06 */  cmp ObjectPosY+0                                ; ObjectPosY+0
/* 1DD74: F0 09 */     beq L1DD7F ; +                                           ; $DD7F
/* 1DD76: AD 00 04 */  lda ObjectSpriteNum+0                           ; ObjectSpriteNum+0
/* 1DD79: C9 09 */     cmp #$09
/* 1DD7B: F0 02 */     beq L1DD7F ; +                                           ; $DD7F
/* 1DD7D: A2 18 */     ldx #$18
L1DD7F: ; +
/* 1DD7F: 86 3E */     stx BossCurrentStrategy
/* 1DD81: 60 */        rts


CutmanStrategyWalkTowardsMegaman:
/* 1DD82: 20 C2 ED */  jsr BossFuncEDC2
/* 1DD85: A9 3F */     lda #$3F
/* 1DD87: CD 01 04 */  cmp ObjectSpriteNum+1
/* 1DD8A: F0 1E */     beq L1DDAA ; +
/* 1DD8C: 8D 01 04 */  sta ObjectSpriteNum+1
/* 1DD8F: A9 00 */     lda #$00
/* 1DD91: 8D 41 04 */  sta ObjectUnknown440+1
/* 1DD94: 8D 41 06 */  sta ObjectFireDelay+1
/* 1DD97: A9 20 */     lda #$20
/* 1DD99: 8D E1 04 */  sta ObjectXSpeedFraction+1
/* 1DD9C: A9 01 */     lda #$01
/* 1DD9E: 8D C1 04 */  sta ObjectXSpeed+1
/* 1DDA1: A5 44 */     lda BossVariable44
/* 1DDA3: F0 05 */     beq L1DDAA ; +
/* 1DDA5: A9 11 */     lda #$11
/* 1DDA7: 8D 41 06 */  sta ObjectFireDelay+1
L1DDAA: ; +
/* 1DDAA: 20 F8 ED */  jsr BossFuncWalkTowardsMegaman
/* 1DDAD: A5 2A */     lda $2A
/* 1DDAF: D0 03 */     bne L1DDB4 ; +
/* 1DDB1: 4C 64 DD */  jmp CutmanStrategyDefault
L1DDB4: ; +
/* 1DDB4: A9 07 */     lda #$07
/* 1DDB6: 85 3E */     sta BossCurrentStrategy
/* 1DDB8: 60 */        rts


CutmanStrategy2:
/* 1DDB9: 20 C2 ED */  jsr BossFuncEDC2
/* 1DDBC: A9 41 */     lda #$41
/* 1DDBE: 8D 01 04 */  sta ObjectSpriteNum+1
/* 1DDC1: A9 00 */     lda #$00
/* 1DDC3: 8D 41 06 */  sta ObjectFireDelay+1
/* 1DDC6: 8D 41 04 */  sta ObjectUnknown440+1
/* 1DDC9: A5 44 */     lda BossVariable44
/* 1DDCB: F0 05 */     beq L1DDD2
/* 1DDCD: A9 1F */     lda #$1F
/* 1DDCF: 8D 41 06 */  sta ObjectFireDelay+1
L1DDD2:
/* 1DDD2: A9 06 */     lda #$06
/* 1DDD4: 20 A0 C5 */  jsr RandomFunc
/* 1DDD7: A0 02 */     ldy #$02
/* 1DDD9: 4A */        lsr a
/* 1DDDA: B0 05 */     bcs L1DDE1 ; +
/* 1DDDC: 88 */        dey
/* 1DDDD: 4A */        lsr a
/* 1DDDE: 90 01 */     bcc L1DDE1 ; +
/* 1DDE0: 88 */        dey
L1DDE1: ; +
/* 1DDE1: B9 EA DF */  lda DFEA_table,y  ;8,9,12
/* 1DDE4: 85 3E */     sta BossCurrentStrategy
/* 1DDE6: A9 06 */     lda #$06
/* 1DDE8: 8D 81 06 */  sta ObjectYSpeed+1
/* 1DDEB: A9 40 */     lda #$40
/* 1DDED: 8D 61 06 */  sta ObjectYSpeedFraction+1
/* 1DDF0: 20 D5 ED */  jsr BossSearchMegaman
/* 1DDF3: 18 */        clc
/* 1DDF4: 79 ED DF */  adc DFED_table,y
/* 1DDF7: 10 05 */     bpl L1DDFE
/* 1DDF9: 49 FF */     eor #$FF
/* 1DDFB: 18 */        clc
/* 1DDFC: 69 01 */     adc #$01
L1DDFE:
/* 1DDFE: AA */        tax
/* 1DDFF: A9 32 */     lda #$32
/* 1DE01: A0 00 */     ldy #$00
/* 1DE03: 20 AC C5 */  jsr EnemyCalculateJumpCurveToHitMegaman
/* 1DE06: A5 04 */     lda $04
/* 1DE08: 8D E1 04 */  sta ObjectXSpeedFraction+1
/* 1DE0B: A5 05 */     lda $05
/* 1DE0D: 8D C1 04 */  sta ObjectXSpeed+1
/* 1DE10: 60 */        rts


CutmanStrategy3:
/* 1DE11: 20 90 DE */  jsr F1DE90
/* 1DE14: A2 0E */     ldx #$0E
/* 1DE16: 29 01 */     and #$01
/* 1DE18: D0 0A */     bne L1DE24
/* 1DE1A: A5 44 */     lda BossVariable44
/* 1DE1C: D0 06 */     bne L1DE24
/* 1DE1E: A2 10 */     ldx #$10
/* 1DE20: A9 08 */     lda #$08
/* 1DE22: 85 3F */     sta BossVariable3F
L1DE24:
/* 1DE24: 86 3E */     stx BossCurrentStrategy
/* 1DE26: 60 */        rts


CutmanStrategy4:
/* 1DE27: 20 90 DE */  jsr F1DE90
/* 1DE2A: E6 3E */     inc BossCurrentStrategy
/* 1DE2C: 29 01 */     and #$01
/* 1DE2E: D0 0C */     bne L1DE3C ; +
/* 1DE30: A5 44 */     lda BossVariable44
/* 1DE32: D0 08 */     bne L1DE3C ; +
/* 1DE34: A9 12 */     lda #$12
/* 1DE36: 85 3E */     sta BossCurrentStrategy
/* 1DE38: A9 12 */     lda #$12
/* 1DE3A: 85 3F */     sta BossVariable3F
L1DE3C: ; +
/* 1DE3C: 60 */        rts



CutmanStrategyConsiderJumpingPeacefully:
/* 1DE3D: 20 F8 ED */  jsr BossFuncWalkTowardsMegaman
/* 1DE40: F0 37 */     beq L1DE79 ; ++
/* 1DE42: E6 3E */     inc BossCurrentStrategy
/* 1DE44: 20 D5 ED */  jsr BossSearchMegaman
/* 1DE47: C9 0E */     cmp #$0E
/* 1DE49: 90 05 */     bcc L1DE50 ; +
/* 1DE4B: A9 05 */     lda #$05
/* 1DE4D: 85 3E */     sta BossCurrentStrategy
/* 1DE4F: 60 */        rts
L1DE50: ; +
/* 1DE50: A9 03 */     lda #$03
/* 1DE52: 20 A0 C5 */  jsr RandomFunc
/* 1DE55: 29 01 */     and #$01
/* 1DE57: F0 0C */     beq L1DE65 ; +
/* 1DE59: A5 44 */     lda BossVariable44
/* 1DE5B: D0 08 */     bne L1DE65 ; +
/* 1DE5D: A9 14 */     lda #$14
/* 1DE5F: 85 3E */     sta BossCurrentStrategy
/* 1DE61: A9 12 */     lda #$12
/* 1DE63: 85 3F */     sta BossVariable3F
L1DE65: ; +
/* 1DE65: A9 E0 */     lda #($100-$20) ;negative integer
/* 1DE67: 8D 61 06 */  sta ObjectYSpeedFraction+1
/* 1DE6A: A9 04 */     lda #$04
/* 1DE6C: 8D 81 06 */  sta ObjectYSpeed+1
/* 1DE6F: A9 C0 */     lda #$C0
/* 1DE71: 8D E1 04 */  sta ObjectXSpeedFraction+1
/* 1DE74: A9 00 */     lda #$00
/* 1DE76: 8D C1 04 */  sta ObjectXSpeed+1
L1DE79: ; ++
/* 1DE79: 60 */        rts


CutmanStrategy7:
/* 1DE7A: 20 90 DE */  jsr F1DE90 ; ++
/* 1DE7D: A2 0D */     ldx #$0D
/* 1DE7F: 29 01 */     and #$01
/* 1DE81: D0 0A */     bne L1DE8D ; +
/* 1DE83: A5 44 */     lda BossVariable44
/* 1DE85: D0 06 */     bne L1DE8D ; +
/* 1DE87: A2 16 */     ldx #$16
/* 1DE89: A9 15 */     lda #$15
/* 1DE8B: 85 3F */     sta BossVariable3F
L1DE8D: ; +
/* 1DE8D: 86 3E */     stx BossCurrentStrategy
/* 1DE8F: 60 */        rts
F1DE90: ; ++
/* 1DE90: 20 F8 ED */  jsr BossFuncWalkTowardsMegaman
/* 1DE93: A9 03 */     lda #$03
/* 1DE95: 20 A0 C5 */  jsr RandomFunc
/* 1DE98: 60 */        rts



CutmanStrategy6:
/* 1DE99: 20 F8 ED */  jsr BossFuncWalkTowardsMegaman
/* 1DE9C: F0 04 */     beq L1DEA2
L1DE9E:
/* 1DE9E: A9 05 */     lda #$05
/* 1DEA0: 85 3E */     sta BossCurrentStrategy
L1DEA2:
/* 1DEA2: 60 */        rts



CutmanStrategy8:
/* 1DEA3: 20 F8 ED */  jsr BossFuncWalkTowardsMegaman
/* 1DEA6: F0 08 */     beq L1DEB0 ; +
/* 1DEA8: A5 46 */     lda RandomSeed
/* 1DEAA: 29 01 */     and #$01
/* 1DEAC: D0 F0 */     bne L1DE9E
/* 1DEAE: E6 3E */     inc BossCurrentStrategy
L1DEB0: ; +
/* 1DEB0: 60 */        rts



CutmanStrategyPeacefulJump:
/* 1DEB1: 20 F8 ED */  jsr BossFuncWalkTowardsMegaman
/* 1DEB4: F0 2A */     beq L1DEE0 ; ++
/* 1DEB6: A5 44 */     lda BossVariable44
/* 1DEB8: D0 2F */     bne L1DEE9
/* 1DEBA: AD 01 06 */  lda ObjectPosY+1
/* 1DEBD: CD 00 06 */  cmp ObjectPosY+0                                 ; ObjectPosY+0
/* 1DEC0: F0 0C */     beq L1DECE ; +
/* 1DEC2: AD 00 04 */  lda ObjectSpriteNum+0                            ; ObjectSpriteNum+0
/* 1DEC5: C9 09 */     cmp #$09
/* 1DEC7: F0 05 */     beq L1DECE ; +
/* 1DEC9: A9 18 */     lda #$18
/* 1DECB: 85 3E */     sta BossCurrentStrategy
/* 1DECD: 60 */        rts
L1DECE: ; +
/* 1DECE: A9 43 */     lda #$43
/* 1DED0: 8D 01 04 */  sta ObjectSpriteNum+1
/* 1DED3: A9 00 */     lda #$00
/* 1DED5: 8D E1 04 */  sta ObjectXSpeedFraction+1
/* 1DED8: 8D C1 04 */  sta ObjectXSpeed+1
/* 1DEDB: 8D 41 06 */  sta ObjectFireDelay+1
/* 1DEDE: E6 3E */     inc BossCurrentStrategy
L1DEE0: ; ++
/* 1DEE0: 60 */        rts


CutmanStrategyThrowWhileStanding:
/* 1DEE1: 20 F8 ED */  jsr BossFuncWalkTowardsMegaman
/* 1DEE4: AD 41 04 */  lda ObjectUnknown440+1
/* 1DEE7: D0 05 */     bne L1DEEE ; +
L1DEE9:
/* 1DEE9: A9 05 */     lda #$05
/* 1DEEB: 85 3E */     sta BossCurrentStrategy
/* 1DEED: 60 */        rts
L1DEEE: ; +
/* 1DEEE: C9 10 */     cmp #$10
/* 1DEF0: D0 05 */     bne L1DEF7
/* 1DEF2: A9 06 */     lda #$06
/* 1DEF4: 20 8F DF */  jsr CutmanThrowScissor
L1DEF7:
/* 1DEF7: 60 */        rts



CutmanStrategyJumpThenThrowAfterDelay:
/* 1DEF8: 20 F8 ED */  jsr BossFuncWalkTowardsMegaman
/* 1DEFB: C6 3F */     dec BossVariable3F
/* 1DEFD: D0 0F */     bne L1DF0E ; +
/* 1DEFF: A9 43 */     lda #$43
/* 1DF01: 8D 01 04 */  sta ObjectSpriteNum+1
/* 1DF04: A9 00 */     lda #$00
/* 1DF06: 8D 41 04 */  sta ObjectUnknown440+1
/* 1DF09: 8D 41 06 */  sta ObjectFireDelay+1
/* 1DF0C: E6 3E */     inc BossCurrentStrategy
L1DF0E: ; +
/* 1DF0E: 60 */        rts


CutmanStrategyThrowWhileJumping:
/* 1DF0F: AD 41 04 */  lda ObjectUnknown440+1
/* 1DF12: D0 07 */     bne L1DF1B
/* 1DF14: A9 41 */     lda #$41
/* 1DF16: 8D 01 04 */  sta ObjectSpriteNum+1
/* 1DF19: D0 07 */     bne L1DF22
L1DF1B:
/* 1DF1B: C9 10 */     cmp #$10
/* 1DF1D: D0 03 */     bne L1DF22
/* 1DF1F: 20 8F DF */  jsr CutmanThrowScissor
L1DF22:
/* 1DF22: 20 F8 ED */  jsr BossFuncWalkTowardsMegaman
/* 1DF25: F0 1F */     beq L1DF46
/* 1DF27: AD 01 06 */  lda ObjectPosY+1
/* 1DF2A: CD 00 06 */  cmp ObjectPosY+0                                 ; ObjectPosY+0
/* 1DF2D: F0 0C */     beq L1DF3B
/* 1DF2F: AD 00 04 */  lda ObjectSpriteNum+0                            ; ObjectSpriteNum+0
/* 1DF32: C9 09 */     cmp #$09
/* 1DF34: F0 05 */     beq L1DF3B
/* 1DF36: A9 18 */     lda #$18
/* 1DF38: 85 3E */     sta BossCurrentStrategy
/* 1DF3A: 60 */        rts



L1DF3B:
/* 1DF3B: A5 3E */     lda BossCurrentStrategy
/* 1DF3D: 29 07 */     and #$07
/* 1DF3F: 4A */        lsr a
/* 1DF40: AA */        tax
/* 1DF41: BD 47 DF */  lda DF47_table,x
/* 1DF44: 85 3E */     sta BossCurrentStrategy
L1DF46:
/* 1DF46: 60 */        rts


DF47_table: ; at $DF47
    .byte $0E, $0A, $05, $05

CutmanStrategeStandMakeNoise:
/* 1DF4B: A9 46 */     lda #$46
/* 1DF4D: CD 01 04 */  cmp ObjectSpriteNum+1
/* 1DF50: F0 17 */     beq L1DF69
/* 1DF52: 8D 01 04 */  sta ObjectSpriteNum+1
/* 1DF55: A9 3F */     lda #$3F
/* 1DF57: 85 3F */     sta BossVariable3F
/* 1DF59: A9 00 */     lda #$00
/* 1DF5B: 8D 41 04 */  sta ObjectUnknown440+1
/* 1DF5E: 8D E1 04 */  sta ObjectXSpeedFraction+1
/* 1DF61: 8D C1 04 */  sta ObjectXSpeed+1

/* 1DF64: A9 26 */     lda #$26        ; Door???
/* 1DF66: 20 77 C4 */  jsr IssueSound                                  ; $C477

L1DF69:
/* 1DF69: 20 F8 ED */  jsr BossFuncWalkTowardsMegaman
/* 1DF6C: AD 01 06 */  lda ObjectPosY+1
/* 1DF6F: CD 00 06 */  cmp ObjectPosY+0                                 ; ObjectPosY+0
/* 1DF72: D0 07 */     bne L1DF7B
/* 1DF74: AD 00 04 */  lda ObjectSpriteNum+0                            ; ObjectSpriteNum+0
/* 1DF77: C9 09 */     cmp #$09
/* 1DF79: D0 08 */     bne L1DF83
L1DF7B:
/* 1DF7B: A5 3F */     lda BossVariable3F
/* 1DF7D: F0 04 */     beq L1DF83
/* 1DF7F: C6 3F */     dec BossVariable3F
/* 1DF81: D0 0B */     bne L1DF8E
L1DF83:
/* 1DF83: A5 44 */     lda BossVariable44
/* 1DF85: D0 07 */     bne L1DF8E
/* 1DF87: A2 0E */     ldx #$0E
/* 1DF89: 86 3E */     stx BossCurrentStrategy
/* 1DF8B: 4C CE DE */  jmp L1DECE
L1DF8E:
/* 1DF8E: 60 */        rts


CutmanThrowScissor:
/* 1DF8F: A9 06 */     lda #$06
/* 1DF91: 20 17 EE */  jsr Boss_FindAmmoSlot
/* 1DF94: B0 23 */     bcs L1DFB9 ; +
/* 1DF96: 20 D5 ED */  jsr BossSearchMegaman
/* 1DF99: 85 02 */     sta $02
/* 1DF9B: A9 02 */     lda #$02
/* 1DF9D: 85 01 */     sta $01
/* 1DF9F: A9 60 */     lda #$60
/* 1DFA1: 85 00 */     sta $00
/* 1DFA3: 38 */        sec
/* 1DFA4: AD 01 06 */  lda ObjectPosY+1
/* 1DFA7: E9 0A */     sbc #$0A
/* 1DFA9: 38 */        sec
/* 1DFAA: ED 00 06 */  sbc ObjectPosY+0                                 ; ObjectPosY+0
/* 1DFAD: 85 03 */     sta $03
/* 1DFAF: 20 C6 F8 */  jsr F8C6_routine
/* 1DFB2: E6 44 */     inc BossVariable44 ;scissor exists

/* 1DFB4: A9 25 */     lda #$25        ; scissor machine sound
/* 1DFB6: 20 77 C4 */  jsr IssueSound                                  ; $C477
L1DFB9: ; +
/* 1DFB9: 60 */        rts

CutmanStrategy20:
/* 1DFBA: A9 57 */     lda #$57
/* 1DFBC: 20 16 EF */  jsr BossFuncEF16
/* 1DFBF: 60 */        rts

CutmanStrategies: ; at DFC0
    .word CutmanStrategyDefault                  ;5 = default
    .word CutmanStrategyWalkTowardsMegaman       ;6 = walk towards Megaman, when close goto phase 7
    .word CutmanStrategy2                        ;7 = may lead to phase 13 or phase 22
    .word CutmanStrategy3                        ;8
    .word CutmanStrategy4                        ;9
    .word CutmanStrategyConsiderJumpingPeacefully ;10 = may lead to phase 14
    .word CutmanStrategy6                        ;11
    .word CutmanStrategy7                        ;12 = jump phase 2?
    .word CutmanStrategy8                        ;13 = jump phase 3 A ? jump again, towards megaman
    .word CutmanStrategyPeacefulJump             ;14 = jump phase 3 D ? jump straight up, don't throw scissor
    .word CutmanStrategyThrowWhileStanding       ;15 = throw scissor
    .word CutmanStrategyJumpThenThrowAfterDelay  ;16
    .word CutmanStrategyThrowWhileJumping        ;17 = throw scissor while jump
    .word CutmanStrategyJumpThenThrowAfterDelay  ;18 = jump towards megaman, throw scissor
    .word CutmanStrategyThrowWhileJumping        ;19 = throw scissor while jumping?
    .word CutmanStrategyJumpThenThrowAfterDelay  ;20
    .word CutmanStrategyThrowWhileJumping        ;21
    .word CutmanStrategyJumpThenThrowAfterDelay  ;22 = jump phase 3 B ? jump, throw scissor at some time
    .word CutmanStrategyThrowWhileJumping        ;23 = throw scissor while jumping?
    .word CutmanStrategeStandMakeNoise           ;24 = make scissor noise and stand still
    .word CutmanStrategy20    ;25

DFEA_table:
    .byte 8,9,12
DFED_table:
    .byte $E0,$00,$20

BossAI_Bombman:
/* 1DFF0: AD A1 06 */  lda ObjectLifeCycleCounter+1
/* 1DFF3: F0 01 */     beq L1DFF6 ; +
/* 1DFF5: 60 */        rts
L1DFF6: ; +
/* 1DFF6: B9 ED E0 */  lda BombmanStrategies+0,y
/* 1DFF9: 85 04 */     sta $04
/* 1DFFB: B9 EE E0 */  lda BombmanStrategies+1,y
/* 1DFFE: 85 05 */     sta $05
/* 1E000: 6C 04 00 */  jmp ($0004)


BombmanStrategyDefault:
/* 1E003: 20 C2 ED */  jsr BossFuncEDC2
/* 1E006: A9 00 */     lda #$00
/* 1E008: 8D 41 04 */  sta ObjectUnknown440+1
/* 1E00B: 8D 41 06 */  sta ObjectFireDelay+1
/* 1E00E: A2 00 */     ldx #$00
/* 1E010: A5 46 */     lda RandomSeed
/* 1E012: 29 01 */     and #$01
/* 1E014: F0 01 */     beq L1E017 ; +
/* 1E016: E8 */    inx
L1E017: ; +
/* 1E017: BD 3B E0 */  lda E03B_table,x
/* 1E01A: 85 3E */     sta BossCurrentStrategy
/* 1E01C: BD 3E E0 */  lda E03E_table,x
/* 1E01F: 8D 01 04 */  sta ObjectSpriteNum+1
/* 1E022: BD 41 E0 */  lda E041_table,x
/* 1E025: 8D E1 04 */  sta ObjectXSpeedFraction+1
/* 1E028: BD 44 E0 */  lda E044_table,x
/* 1E02B: 8D C1 04 */  sta ObjectXSpeed+1
/* 1E02E: BD 47 E0 */  lda E047_table,x
/* 1E031: 8D 61 06 */  sta ObjectYSpeedFraction+1
/* 1E034: BD 4A E0 */  lda E04A_table,x
/* 1E037: 8D 81 06 */  sta ObjectYSpeed+1
/* 1E03A: 60 */        rts

E03B_table: .byte $0D,$16,$0D ;stored to BossCurrentStrategy
E03E_table: .byte $28,$29,$2A ;Gives values for boss's SpriteNum
E041_table: .byte $90,$09,$09 ;Gives values for boss's XSpeedFraction
E044_table: .byte $02,$01,$01 ;Gives values for boss's XSpeed
E047_table: .byte $60,$B0,$B0 ;Gives values for boss's YSpeedFraction
E04A_table: .byte $07,$05,$05 ;Gives values for boss's YSpeed

BombmanStrategyBackwardsJump:
/* 1E04D: C6 3E */     dec BossCurrentStrategy
/* 1E04F: 20 C2 ED */  jsr BossFuncEDC2
/* 1E052: A5 43 */     lda BossVariable43
/* 1E054: 49 40 */     eor #$40
/* 1E056: 85 43 */     sta BossVariable43
/* 1E058: A9 00 */     lda #$00
/* 1E05A: 8D 41 04 */  sta ObjectUnknown440+1
/* 1E05D: 8D 41 06 */  sta ObjectFireDelay+1
/* 1E060: A2 02 */     ldx #$02
/* 1E062: 4C 17 E0 */  jmp L1E017

BombmanStrategyContinueThrowingBombs:
/* 1E065: 20 DF E0 */  jsr BossResetStrategyIfMegamanIsClose
/* 1E068: A5 44 */     lda BossVariable44
/* 1E06A: D0 09 */     bne L1E075
/* 1E06C: 4C B8 E0 */  jmp L1E0B8

BombmanStrategyWaitCounter:
/* 1E06F: A5 44 */     lda BossVariable44
/* 1E071: D0 02 */     bne L1E075
/* 1E073: C6 3E */     dec BossCurrentStrategy
L1E075:
/* 1E075: 60 */        rts



BombmanStrategyThrowBombAfterDelay:
/* 1E076: 20 DF E0 */  jsr BossResetStrategyIfMegamanIsClose
/* 1E079: AE 41 06 */  ldx ObjectFireDelay+1
/* 1E07C: D0 23 */     bne L1E0A1 ; ++
/* 1E07E: C6 3E */     dec BossCurrentStrategy
/* 1E080: AA */        tax
/* 1E081: 38 */        sec
/* 1E082: E9 18 */     sbc #$18
/* 1E084: 90 01 */     bcc L1E087 ; +
/* 1E086: AA */        tax
L1E087: ; +
/* 1E087: A0 00 */     ldy #$00
/* 1E089: A9 33 */     lda #$33
/* 1E08B: 20 AC C5 */  jsr EnemyCalculateJumpCurveToHitMegaman
/* 1E08E: A9 00 */     lda #$00
/* 1E090: 20 17 EE */  jsr Boss_FindAmmoSlot
/* 1E093: B0 0C */     bcs L1E0A1 ; ++
/* 1E095: A5 04 */     lda $04
/* 1E097: 9D E0 04 */  sta ObjectXSpeedFraction,x
/* 1E09A: A5 05 */     lda $05
/* 1E09C: 9D C0 04 */  sta ObjectXSpeed,x
/* 1E09F: E6 44 */     inc BossVariable44
L1E0A1: ; ++
/* 1E0A1: 60 */        rts


BombmanStrategyDecideNextAction:
/* 1E0A2: 20 DF E0 */  jsr BossResetStrategyIfMegamanIsClose
/* 1E0A5: A9 06 */     lda #$06
/* 1E0A7: 20 A0 C5 */  jsr RandomFunc
/* 1E0AA: 4A */        lsr a
/* 1E0AB: B0 0B */     bcs L1E0B8 ; +
/* 1E0AD: C6 3E */     dec BossCurrentStrategy
/* 1E0AF: C6 3E */     dec BossCurrentStrategy
/* 1E0B1: 4A */        lsr a
/* 1E0B2: 90 04 */     bcc L1E0B8 ; +
/* 1E0B4: C6 3E */     dec BossCurrentStrategy
/* 1E0B6: C6 3E */     dec BossCurrentStrategy
L1E0B8: ; +
/* 1E0B8: C6 3E */     dec BossCurrentStrategy
/* 1E0BA: A9 2B */     lda #$2B
/* 1E0BC: 8D 01 04 */  sta ObjectSpriteNum+1
/* 1E0BF: A9 00 */     lda #$00
/* 1E0C1: 8D 41 04 */  sta ObjectUnknown440+1
/* 1E0C4: A9 1F */     lda #$1F
/* 1E0C6: 8D 41 06 */  sta ObjectFireDelay+1
/* 1E0C9: 60 */        rts



BombmanStrategyMoveUntilHitWall:
/* 1E0CA: 20 F8 ED */  jsr BossFuncWalkTowardsMegaman
/* 1E0CD: F0 0F */     beq L1E0DE ; +
/* 1E0CF: A9 1D */     lda #$1D
/* 1E0D1: 8D 01 04 */  sta ObjectSpriteNum+1
/* 1E0D4: A9 00 */     lda #$00
/* 1E0D6: 8D E1 04 */  sta ObjectXSpeedFraction+1
/* 1E0D9: 8D C1 04 */  sta ObjectXSpeed+1
/* 1E0DC: C6 3E */     dec BossCurrentStrategy
L1E0DE: ; +
/* 1E0DE: 60 */        rts

BossResetStrategyIfMegamanIsClose:
/* 1E0DF: 20 D5 ED */  jsr BossSearchMegaman
/* 1E0E2: C9 30 */     cmp #$30
/* 1E0E4: B0 06 */     bcs L1E0EC ; +
/* 1E0E6: A9 05 */     lda #$05
/* 1E0E8: 85 3E */     sta BossCurrentStrategy
/* 1E0EA: 68 */        pla ; performs a DOUBLE RETURN (return to caller's caller)
/* 1E0EB: 68 */        pla
L1E0EC: ; +
/* 1E0EC: 60 */        rts


BombmanStrategies:
    .word BombmanStrategyDefault               ;5 = choose jump
    .word BombmanStrategyWaitCounter           ;6
    .word BombmanStrategyThrowBombAfterDelay   ;7
    .word BombmanStrategyContinueThrowingBombs ;8
    .word BombmanStrategyThrowBombAfterDelay   ;9
    .word BombmanStrategyContinueThrowingBombs ;10
    .word BombmanStrategyThrowBombAfterDelay   ;11
    .word BombmanStrategyDecideNextAction      ;12
    .word BombmanStrategyMoveUntilHitWall      ;13 = big jump
    .word BombmanStrategyBackwardsJump         ;14
    .word BombmanStrategyWaitCounter           ;15
    .word BombmanStrategyThrowBombAfterDelay   ;16
    .word BombmanStrategyContinueThrowingBombs ;17
    .word BombmanStrategyThrowBombAfterDelay   ;18
    .word BombmanStrategyContinueThrowingBombs ;19
    .word BombmanStrategyThrowBombAfterDelay   ;20
    .word BombmanStrategyDecideNextAction      ;21
    .word BombmanStrategyMoveUntilHitWall      ;22 = small jump

BossAI_Iceman:
/* 1E111: AD A1 06 */  lda ObjectLifeCycleCounter+1
/* 1E114: F0 01 */     beq L1E117 ; +
/* 1E116: 60 */        rts
L1E117: ; +
/* 1E117: B9 A2 E2 */  lda IcemanStrategies+0,y
/* 1E11A: 85 04 */     sta $04
/* 1E11C: B9 A3 E2 */  lda IcemanStrategies+1,y
/* 1E11F: 85 05 */     sta $05
/* 1E121: 6C 04 00 */  jmp ($0004)


IcemanStrategy0:
/* 1E124: A9 00 */     lda #$00
/* 1E126: 8D 41 04 */  sta ObjectUnknown440+1
/* 1E129: 8D 41 06 */  sta ObjectFireDelay+1
/* 1E12C: A9 3B */     lda #$3B
/* 1E12E: 8D 01 04 */  sta ObjectSpriteNum+1
/* 1E131: A9 20 */     lda #$20
/* 1E133: 8D E1 04 */  sta ObjectXSpeedFraction+1
/* 1E136: A9 01 */     lda #$01
/* 1E138: 8D C1 04 */  sta ObjectXSpeed+1
/* 1E13B: AD 80 04 */  lda ObjectPosX+0
/* 1E13E: 48 */        pha
/* 1E13F: A5 43 */      lda BossVariable43
/* 1E141: 29 0F */      and #$0F
/* 1E143: AA */         tax
/* 1E144: BD C8 E2 */   lda E2C8_table,x
/* 1E147: 8D 80 04 */   sta ObjectPosX+0
/* 1E14A: 20 C2 ED */   jsr BossFuncEDC2
/* 1E14D: 68 */        pla
/* 1E14E: 8D 80 04 */  sta ObjectPosX+0
/* 1E151: E6 3E */     inc BossCurrentStrategy
/* 1E153: 60 */        rts

IcemanStrategy1:
/* 1E154: AD 80 04 */  lda ObjectPosX+0
/* 1E157: 48 */        pha
/* 1E158: A5 43 */      lda BossVariable43
/* 1E15A: 29 0F */      and #$0F
/* 1E15C: AA */         tax
/* 1E15D: BD C8 E2 */   lda E2C8_table,x
/* 1E160: 8D 80 04 */   sta ObjectPosX+0
/* 1E163: 20 D5 ED */   jsr BossSearchMegaman
/* 1E166: AA */         tax
/* 1E167: 68 */        pla
/* 1E168: 8D 80 04 */  sta ObjectPosX+0
/* 1E16B: E0 02 */     cpx #$02
/* 1E16D: 90 04 */     bcc L1E173
/* 1E16F: 20 FB ED */  jsr F1EDFB
/* 1E172: 60 */        rts



L1E173:
/* 1E173: EE 01 04 */  inc ObjectSpriteNum+1
/* 1E176: A9 1F */     lda #$1F
/* 1E178: 8D 41 06 */  sta ObjectFireDelay+1
/* 1E17B: A9 00 */     lda #$00
/* 1E17D: 8D 41 04 */  sta ObjectUnknown440+1
/* 1E180: A9 10 */     lda #$10
/* 1E182: 8D E1 04 */  sta ObjectXSpeedFraction+1
/* 1E185: A9 00 */     lda #$00
/* 1E187: 8D C1 04 */  sta ObjectXSpeed+1
/* 1E18A: E6 3E */     inc BossCurrentStrategy
/* 1E18C: 60 */        rts


IcemanStrategy2:
/* 1E18D: AD 41 06 */  lda ObjectFireDelay+1
/* 1E190: F0 04 */     beq L1E196 ; +
/* 1E192: 20 FB ED */  jsr F1EDFB
/* 1E195: 60 */        rts
L1E196: ; +
/* 1E196: E6 3E */     inc BossCurrentStrategy
/* 1E198: 60 */        rts

IcemanStrategy3:
/* 1E199: 20 D5 ED */  jsr BossSearchMegaman
/* 1E19C: A9 00 */     lda #$00
/* 1E19E: 8D E1 04 */  sta ObjectXSpeedFraction+1
/* 1E1A1: 8D C1 04 */  sta ObjectXSpeed+1
/* 1E1A4: A9 DF */     lda #NEG{33} ;signed integer
/* 1E1A6: 8D 61 06 */  sta ObjectYSpeedFraction+1
/* 1E1A9: A9 04 */     lda #$04
/* 1E1AB: 8D 81 06 */  sta ObjectYSpeed+1
/* 1E1AE: E6 3E */     inc BossCurrentStrategy
/* 1E1B0: 60 */        rts


IcemanStrategy4:
/* 1E1B1: 20 F8 ED */  jsr BossFuncWalkTowardsMegaman
/* 1E1B4: AD 81 06 */  lda ObjectYSpeed+1
/* 1E1B7: 10 31 */     bpl L1E1EA
/* 1E1B9: A5 3E */     lda BossCurrentStrategy
/* 1E1BB: 29 07 */     and #$07
/* 1E1BD: 4A */        lsr a
/* 1E1BE: AA */        tax
/* 1E1BF: AD 01 06 */  lda ObjectPosY+1
/* 1E1C2: DD EB E1 */  cmp E1EB_table,x
/* 1E1C5: 90 23 */     bcc L1E1EA
/* 1E1C7: A6 44 */     ldx BossVariable44
/* 1E1C9: A0 00 */     ldy #$00
/* 1E1CB: A9 03 */     lda #$03
/* 1E1CD: 20 AC C5 */  jsr EnemyCalculateJumpCurveToHitMegaman
/* 1E1D0: A5 05 */     lda $05
/* 1E1D2: 0A */        asl a
/* 1E1D3: AA */        tax
/* 1E1D4: BD DC E2 */  lda E2DC_table+0,x
/* 1E1D7: 8D 81 06 */  sta ObjectYSpeed+1
/* 1E1DA: BD DD E2 */  lda E2DC_table+1,x
/* 1E1DD: 8D 61 06 */  sta ObjectYSpeedFraction+1
/* 1E1E0: AD 21 04 */  lda ObjectFlags+1
/* 1E1E3: 29 EF */     and #$EF
/* 1E1E5: 8D 21 04 */  sta ObjectFlags+1
/* 1E1E8: E6 3E */     inc BossCurrentStrategy
L1E1EA:
/* 1E1EA: 60 */        rts


E1EB_table:
	.byte $94,$A4,$B4

IcemanStrategy10:
/* 1E1EE: E6 3E */     inc BossCurrentStrategy
/* 1E1F0: A6 44 */     ldx BossVariable44
/* 1E1F2: A0 00 */     ldy #$00
/* 1E1F4: A9 03 */     lda #$03
/* 1E1F6: 20 AC C5 */  jsr EnemyCalculateJumpCurveToHitMegaman
/* 1E1F9: A6 05 */     ldx $05
/* 1E1FB: BD D8 E2 */  lda E2D8_table,x
/* 1E1FE: 85 3F */     sta BossVariable3F
/* 1E200: 60 */        rts


IcemanStrategy5:
/* 1E201: 20 D5 ED */  jsr BossSearchMegaman
/* 1E204: A9 05 */     lda #$05
/* 1E206: 20 17 EE */  jsr Boss_FindAmmoSlot
/* 1E209: B0 27 */     bcs L1E232
/* 1E20B: 86 0D */     stx $0D
/* 1E20D: A6 44 */     ldx BossVariable44
/* 1E20F: A0 00 */     ldy #$00
/* 1E211: A9 03 */     lda #$03
/* 1E213: 20 AC C5 */  jsr EnemyCalculateJumpCurveToHitMegaman
/* 1E216: A5 05 */     lda $05
/* 1E218: 0A */        asl a
/* 1E219: AA */        tax
/* 1E21A: A4 0D */     ldy $0D
/* 1E21C: BD CC E2 */  lda E2C8_table+4,x
/* 1E21F: 99 C0 04 */  sta ObjectXSpeed,y
/* 1E222: BD CD E2 */  lda E2C8_table+5,x
/* 1E225: 99 E0 04 */  sta ObjectXSpeedFraction,y
/* 1E228: A9 26 */     lda #$26
/* 1E22A: 8D 41 06 */  sta ObjectFireDelay+1

/* 1E22D: A9 2A */     lda #$2A        ; Metal sound
/* 1E22F: 20 77 C4 */  jsr IssueSound                                  ; $C477
L1E232:
/* 1E232: E6 3E */     inc BossCurrentStrategy
/* 1E234: 60 */        rts


IcemanStrategy11:
/* 1E235: 20 C2 ED */  jsr BossFuncEDC2
/* 1E238: C6 3F */     dec BossVariable3F
/* 1E23A: D0 15 */     bne L1E251
/* 1E23C: AD 81 06 */  lda ObjectYSpeed+1
/* 1E23F: 49 FF */     eor #$FF
/* 1E241: 8D 81 06 */  sta ObjectYSpeed+1
/* 1E244: AD 61 06 */  lda ObjectYSpeedFraction+1 ;negates the value
/* 1E247: 49 FF */     eor #$FF
/* 1E249: 18 */        clc
/* 1E24A: 69 01 */     adc #$01
/* 1E24C: 8D 61 06 */  sta ObjectYSpeedFraction+1
/* 1E24F: E6 3E */     inc BossCurrentStrategy
L1E251:
/* 1E251: 60 */        rts


IcemanStrategy13:
/* 1E252: 20 F8 ED */  jsr BossFuncWalkTowardsMegaman
/* 1E255: A5 3E */     lda BossCurrentStrategy
/* 1E257: 29 07 */     and #$07
/* 1E259: 4A */        lsr a
/* 1E25A: 4A */        lsr a
/* 1E25B: AA */        tax
/* 1E25C: AD 01 06 */  lda ObjectPosY+1
/* 1E25F: DD 67 E2 */  cmp E267_table,x
/* 1E262: B0 02 */     bcs L1E266
/* 1E264: E6 3E */     inc BossCurrentStrategy
L1E266:
/* 1E266: 60 */        rts
E267_table:
    .byte $A5,$95

IcemanStrategy15:
/* 1E269: AD 21 04 */  lda ObjectFlags+1
/* 1E26C: 09 10 */     ora #$10
/* 1E26E: 8D 21 04 */  sta ObjectFlags+1
/* 1E271: A9 C0 */     lda #($100-$40)
/* 1E273: 8D 61 06 */  sta ObjectYSpeedFraction+1
/* 1E276: A9 02 */     lda #$02
/* 1E278: 8D 81 06 */  sta ObjectYSpeed+1
/* 1E27B: E6 3E */     inc BossCurrentStrategy
/* 1E27D: 60 */        rts


IcemanStrategy16:
/* 1E27E: 20 F8 ED */  jsr BossFuncWalkTowardsMegaman
/* 1E281: A5 2B */     lda $2B
/* 1E283: F0 1C */     beq L1E2A1
/* 1E285: A9 05 */     lda #$05
/* 1E287: 85 3E */     sta BossCurrentStrategy
/* 1E289: A5 44 */     lda BossVariable44
/* 1E28B: C9 09 */     cmp #$09
/* 1E28D: F0 02 */     beq L1E291
/* 1E28F: E6 44 */     inc BossVariable44
L1E291:
/* 1E291: E6 43 */     inc BossVariable43
/* 1E293: A5 43 */     lda BossVariable43
/* 1E295: 29 0F */     and #$0F
/* 1E297: C9 04 */     cmp #$04
/* 1E299: D0 06 */     bne L1E2A1
/* 1E29B: A5 43 */     lda BossVariable43
/* 1E29D: 29 F0 */     and #$F0
/* 1E29F: 85 43 */     sta BossVariable43
L1E2A1:
/* 1E2A1: 60 */        rts

IcemanStrategies: ; at E2A2
    .word IcemanStrategy0
    .word IcemanStrategy1
    .word IcemanStrategy2
    .word IcemanStrategy3
    .word IcemanStrategy4
    .word IcemanStrategy5
    .word IcemanStrategy4
    .word IcemanStrategy5
    .word IcemanStrategy4
    .word IcemanStrategy5
    .word IcemanStrategy10
    .word IcemanStrategy11
    .word IcemanStrategy5
    .word IcemanStrategy13
    .word IcemanStrategy5
    .word IcemanStrategy13
    .word IcemanStrategy5
    .word IcemanStrategy15
    .word IcemanStrategy16

E2C8_table: ; at E2C8
    .byte $D0,$A8,$80,$A8, $01,$60,$01,$80, $02,$00,$02,$E0, $0D,$0C,$09,$06
E2D8_table: ; at E2D8
/* 1E2D8: 30 2E */     .byte $30,$2E
/* 1E2DA: 26 1B */     rol $1B

E2DC_table: ;YSpeed and YSpeedFraction
    .byte $FF,$60
    .byte $FF,$50
    .byte $FF,$2C
    .byte $FE,NEG{$20}

BossAI_Fireman:
/* 1E2E4: A5 56 */     lda BossBlinkState
/* 1E2E6: C9 10 */     cmp #$10
/* 1E2E8: D0 06 */     bne L1E2F0 ; +
/* 1E2EA: A0 04 */     ldy #$04
/* 1E2EC: A9 07 */     lda #$07
/* 1E2EE: 85 3E */     sta BossCurrentStrategy
L1E2F0: ; +
/* 1E2F0: B9 1C E4 */  lda FiremanStrategies+0,y
/* 1E2F3: 48 */        pha
/* 1E2F4: B9 1D E4 */  lda FiremanStrategies+1,y
/* 1E2F7: 48 */        pha
/* 1E2F8: AD 81 04 */  lda ObjectPosX+1
/* 1E2FB: 48 */        pha
/* 1E2FC: AD 21 04 */  lda ObjectFlags+1
/* 1E2FF: 48 */        pha
/* 1E300: A2 10 */     ldx #$10
L1E302: ; -
/* 1E302: A9 37 */     lda #$37
/* 1E304: 20 86 C5 */  jsr FindLastObjectOfType
/* 1E307: B0 31 */     bcs L1E33A ; +++
/* 1E309: 86 05 */     stx $05
/* 1E30B: BD 80 04 */  lda ObjectPosX,x
/* 1E30E: 8D 81 04 */  sta ObjectPosX+1
/* 1E311: 20 D5 ED */  jsr BossSearchMegaman
/* 1E314: C9 08 */     cmp #$08
/* 1E316: B0 1D */     bcs L1E335 ; ++
/* 1E318: A9 38 */     lda #$38
/* 1E31A: A2 10 */     ldx #$10
/* 1E31C: 20 86 C5 */  jsr FindLastObjectOfType
/* 1E31F: 90 08 */     bcc L1E329 ; +
/* 1E321: A9 04 */     lda #$04
/* 1E323: 20 17 EE */  jsr Boss_FindAmmoSlot
/* 1E326: 4C 3A E3 */  jmp L1E33A ; +++
L1E329: ; +
/* 1E329: A0 1C */     ldy #$1C
/* 1E32B: A9 04 */     lda #$04
/* 1E32D: 85 0D */     sta $0D
/* 1E32F: 20 27 EE */  jsr F1EE27
/* 1E332: 4C 3A E3 */  jmp L1E33A ; +++
L1E335: ; ++
/* 1E335: A6 05 */     ldx $05
/* 1E337: E8 */        inx
/* 1E338: D0 C8 */     bne L1E302 ; -
L1E33A: ; +++
/* 1E33A: 68 */        pla
/* 1E33B: 8D 21 04 */  sta ObjectFlags+1
/* 1E33E: 68 */        pla
/* 1E33F: 8D 81 04 */  sta ObjectPosX+1
/* 1E342: 68 */        pla
/* 1E343: 85 05 */     sta $05
/* 1E345: 68 */        pla
/* 1E346: 85 04 */     sta $04
/* 1E348: AD A1 06 */  lda ObjectLifeCycleCounter+1
/* 1E34B: F0 01 */     beq L1E34E ; +
/* 1E34D: 60 */        rts
L1E34E: ; +
/* 1E34E: 6C 04 00 */  jmp ($0004)

FiremanStrategy0:
/* 1E351: A2 07 */     ldx #$07
/* 1E353: 20 D5 ED */  jsr BossSearchMegaman
/* 1E356: C9 61 */     cmp #$61
/* 1E358: B0 0F */     bcs L1E369
/* 1E35A: C9 5F */     cmp #$5F
/* 1E35C: 90 03 */     bcc L1E361
/* 1E35E: 86 3E */     stx BossCurrentStrategy
/* 1E360: 60 */        rts



L1E361:
/* 1E361: AD 21 04 */  lda ObjectFlags+1
/* 1E364: 49 40 */     eor #$40
/* 1E366: 8D 21 04 */  sta ObjectFlags+1

L1E369:
/* 1E369: AD 21 04 */  lda ObjectFlags+1
/* 1E36C: 29 40 */     and #$40
/* 1E36E: 85 43 */     sta BossVariable43
/* 1E370: A9 34 */     lda #$34
/* 1E372: CD 01 04 */  cmp ObjectSpriteNum+1
/* 1E375: F0 15 */     beq L1E38C
/* 1E377: 8D 01 04 */  sta ObjectSpriteNum+1
/* 1E37A: A9 00 */     lda #$00
/* 1E37C: 8D 41 04 */  sta ObjectUnknown440+1
/* 1E37F: 8D 41 06 */  sta ObjectFireDelay+1
/* 1E382: A9 20 */     lda #$20
/* 1E384: 8D E1 04 */  sta ObjectXSpeedFraction+1
/* 1E387: A9 01 */     lda #$01
/* 1E389: 8D C1 04 */  sta ObjectXSpeed+1
L1E38C:
/* 1E38C: 20 FB ED */  jsr F1EDFB
/* 1E38F: A5 2A */     lda $2A
/* 1E391: F0 05 */     beq L1E398
/* 1E393: 20 C2 ED */  jsr BossFuncEDC2
/* 1E396: E6 3E */     inc BossCurrentStrategy
L1E398:
/* 1E398: 60 */        rts


FiremanStrategy1:
/* 1E399: AD 21 04 */  lda ObjectFlags+1
/* 1E39C: 48 */        pha
/* 1E39D: 20 D5 ED */  jsr BossSearchMegaman
/* 1E3A0: C9 61 */     cmp #$61
/* 1E3A2: B0 12 */     bcs L1E3B6
/* 1E3A4: 68 */        pla
/* 1E3A5: 8D 21 04 */  sta ObjectFlags+1
/* 1E3A8: 20 FB ED */  jsr F1EDFB
/* 1E3AB: A5 2A */     lda $2A
/* 1E3AD: D0 01 */     bne L1E3B0
/* 1E3AF: 60 */        rts



L1E3B0:
/* 1E3B0: 20 C2 ED */  jsr BossFuncEDC2
/* 1E3B3: C6 3E */     dec BossCurrentStrategy
/* 1E3B5: 60 */        rts



L1E3B6:
/* 1E3B6: 68 */        pla
/* 1E3B7: C6 3E */     dec BossCurrentStrategy
/* 1E3B9: 60 */        rts


FiremanStrategy2:
/* 1E3BA: A9 35 */     lda #$35
/* 1E3BC: 8D 01 04 */  sta ObjectSpriteNum+1
/* 1E3BF: A9 00 */     lda #$00
/* 1E3C1: 8D 41 04 */  sta ObjectUnknown440+1
/* 1E3C4: 8D 41 06 */  sta ObjectFireDelay+1
FiremanStrategy4:
/* 1E3C7: 20 D5 ED */  jsr BossSearchMegaman
/* 1E3CA: A9 01 */     lda #$01
/* 1E3CC: 20 17 EE */  jsr Boss_FindAmmoSlot
/* 1E3CF: B0 14 */     bcs L1E3E5
/* 1E3D1: A9 3F */     lda #$3F
/* 1E3D3: 85 44 */     sta BossVariable44
/* 1E3D5: A2 10 */     ldx #$10
/* 1E3D7: A9 38 */     lda #$38
/* 1E3D9: 20 86 C5 */  jsr FindLastObjectOfType
/* 1E3DC: B0 05 */     bcs L1E3E3
/* 1E3DE: A9 F8 */     lda #$F8
/* 1E3E0: 9D 00 06 */  sta ObjectPosY,x
L1E3E3:
/* 1E3E3: E6 3E */     inc BossCurrentStrategy
L1E3E5:
/* 1E3E5: 60 */        rts



FiremanStrategy3:
/* 1E3E6: A5 44 */     lda BossVariable44
/* 1E3E8: C9 30 */     cmp #$30
/* 1E3EA: B0 10 */     bcs L1E3FC
/* 1E3EC: 20 D5 ED */  jsr BossSearchMegaman
/* 1E3EF: C9 61 */     cmp #$61
/* 1E3F1: B0 04 */     bcs L1E3F7
/* 1E3F3: C9 5F */     cmp #$5F
/* 1E3F5: B0 05 */     bcs L1E3FC
L1E3F7:
/* 1E3F7: A9 05 */     lda #$05
/* 1E3F9: 85 3E */     sta BossCurrentStrategy
/* 1E3FB: 60 */        rts

L1E3FC:
/* 1E3FC: C6 44 */     dec BossVariable44
/* 1E3FE: D0 02 */     bne L1E402
/* 1E400: E6 3E */     inc BossCurrentStrategy
L1E402:
/* 1E402: 60 */        rts

FiremanStrategy11:
/* 1E403: 20 D5 ED */  jsr BossSearchMegaman
/* 1E406: C6 44 */     dec BossVariable44
/* 1E408: D0 05 */     bne L1E40F
/* 1E40A: A2 33 */     ldx #$33
/* 1E40C: 8E 01 04 */  stx ObjectSpriteNum+1
L1E40F:
/* 1E40F: C9 61 */     cmp #$61
/* 1E411: B0 E4 */     bcs L1E3F7
/* 1E413: C9 5F */     cmp #$5F
/* 1E415: 90 E0 */     bcc L1E3F7
/* 1E417: A5 14 */     lda JoyPad0
/* 1E419: D0 DC */     bne L1E3F7
/* 1E41B: 60 */        rts


FiremanStrategies:
    .word FiremanStrategy0
    .word FiremanStrategy1
    .word FiremanStrategy2
    .word FiremanStrategy3
    .word FiremanStrategy4
    .word FiremanStrategy3
    .word FiremanStrategy4
    .word FiremanStrategy3
    .word FiremanStrategy4
    .word FiremanStrategy3
    .word FiremanStrategy4
    .word FiremanStrategy11

BossAI_Elecman:
/* 1E434: A5 56 */     lda BossBlinkState
/* 1E436: F0 0C */     beq L1E444 ; +
/* 1E438: C9 1F */     cmp #$1F
/* 1E43A: 90 08 */     bcc L1E444 ; +
/* 1E43C: A0 12 */     ldy #$12
/* 1E43E: AD A1 06 */  lda ObjectLifeCycleCounter+1
/* 1E441: F0 01 */     beq L1E444 ; +
/* 1E443: 60 */        rts
L1E444: ; +
/* 1E444: B9 5A E5 */  lda ElecmanStrategies+0,y
/* 1E447: 85 04 */     sta $04
/* 1E449: B9 5B E5 */  lda ElecmanStrategies+1,y
/* 1E44C: 85 05 */     sta $05
/* 1E44E: 6C 04 00 */  jmp ($0004)

ElecmanStrategy0:
/* 1E451: A9 48 */     lda #$48
F1E453:
/* 1E453: CD 01 04 */  cmp ObjectSpriteNum+1
/* 1E456: F0 13 */     beq L1E46B
/* 1E458: 8D 01 04 */  sta ObjectSpriteNum+1
/* 1E45B: A9 00 */     lda #$00
/* 1E45D: 8D 41 04 */  sta ObjectUnknown440+1
/* 1E460: 8D 41 06 */  sta ObjectFireDelay+1
/* 1E463: 8D E1 04 */  sta ObjectXSpeedFraction+1
/* 1E466: A9 02 */     lda #$02
/* 1E468: 8D C1 04 */  sta ObjectXSpeed+1
L1E46B:
/* 1E46B: A5 43 */     lda BossVariable43
/* 1E46D: 29 0F */     and #$0F
/* 1E46F: AA */        tax
/* 1E470: AD 80 04 */  lda ObjectPosX+0
/* 1E473: 48 */        pha
/* 1E474: BD 50 E5 */  lda E550_table,x
/* 1E477: 8D 80 04 */  sta ObjectPosX+0
/* 1E47A: 20 D5 ED */  jsr BossSearchMegaman
/* 1E47D: C9 03 */     cmp #$03
/* 1E47F: B0 02 */     bcs L1E483
/* 1E481: E6 3E */     inc BossCurrentStrategy
L1E483:
/* 1E483: 20 C2 ED */  jsr BossFuncEDC2
/* 1E486: 68 */        pla
/* 1E487: 8D 80 04 */  sta ObjectPosX+0
/* 1E48A: 20 FB ED */  jsr F1EDFB
/* 1E48D: A5 2A */     lda $2A
/* 1E48F: D0 06 */     bne L1E497

/* 1E491: A5 18 */     lda JoyD0       ; If B has been pressed
/* 1E493: 29 02 */     and #$02
/* 1E495: F0 04 */     beq L1E49B ; +                                       ; $0001E49B
L1E497:
/* 1E497: A9 09 */     lda #$09
/* 1E499: 85 3E */     sta BossCurrentStrategy
L1E49B: ; +
/* 1E49B: 60 */        rts

ElecmanStrategy1:
/* 1E49C: A9 00 */     lda #$00
/* 1E49E: 8D E1 04 */  sta ObjectXSpeedFraction+1
/* 1E4A1: 8D C1 04 */  sta ObjectXSpeed+1
F1E4A4:
/* 1E4A4: 8D 41 04 */  sta ObjectUnknown440+1
/* 1E4A7: A9 49 */     lda #$49
/* 1E4A9: 8D 01 04 */  sta ObjectSpriteNum+1
/* 1E4AC: A9 1F */     lda #$1F
/* 1E4AE: 8D 41 06 */  sta ObjectFireDelay+1
/* 1E4B1: E6 3E */     inc BossCurrentStrategy
/* 1E4B3: 20 F8 ED */  jsr BossFuncWalkTowardsMegaman
/* 1E4B6: 60 */        rts



ElecmanStrategy2:
/* 1E4B7: AD 41 06 */  lda ObjectFireDelay+1
/* 1E4BA: D0 0C */     bne L1E4C8 ; +                                           ; $E4C8
/* 1E4BC: 20 ED EB */  jsr Boss7_WeaponAI_E
/* 1E4BF: B0 07 */     bcs L1E4C8 ; +                                           ; $E4C8
/* 1E4C1: E6 3E */     inc BossCurrentStrategy
/* 1E4C3: A9 1E */     lda #$1E        ; Electricity shooting?
/* 1E4C5: 20 77 C4 */  jsr IssueSound                                  ; $C477
L1E4C8: ; +
/* 1E4C8: 20 F8 ED */  jsr BossFuncWalkTowardsMegaman
/* 1E4CB: 60 */        rts



ElecmanStrategy3:
/* 1E4CC: 20 F8 ED */  jsr BossFuncWalkTowardsMegaman
/* 1E4CF: A5 44 */     lda BossVariable44
/* 1E4D1: D0 14 */     bne L1E4E7 ; ++                                          ; $E4E7
L1E4D3:
/* 1E4D3: E6 43 */     inc BossVariable43
/* 1E4D5: A5 43 */     lda BossVariable43
/* 1E4D7: 29 0F */     and #$0F
/* 1E4D9: C9 04 */     cmp #$04
/* 1E4DB: D0 06 */     bne L1E4E3 ; +                                           ; $E4E3
/* 1E4DD: A5 43 */     lda BossVariable43
/* 1E4DF: 29 F0 */     and #$F0
/* 1E4E1: 85 43 */     sta BossVariable43
L1E4E3: ; +
/* 1E4E3: A9 05 */     lda #$05
/* 1E4E5: 85 3E */     sta BossCurrentStrategy
L1E4E7: ; ++
/* 1E4E7: 60 */        rts



ElecmanStrategy4:
/* 1E4E8: A9 4B */     lda #$4B
L1E4EA:
/* 1E4EA: 8D 01 04 */  sta ObjectSpriteNum+1
/* 1E4ED: A9 00 */     lda #$00
/* 1E4EF: 8D 41 04 */  sta ObjectUnknown440+1
/* 1E4F2: 8D 41 06 */  sta ObjectFireDelay+1
/* 1E4F5: 8D 61 06 */  sta ObjectYSpeedFraction+1
/* 1E4F8: A9 E0 */     lda #$E0
/* 1E4FA: 8D E1 04 */  sta ObjectXSpeedFraction+1
/* 1E4FD: A9 01 */     lda #$01
/* 1E4FF: 8D C1 04 */  sta ObjectXSpeed+1
/* 1E502: A9 04 */     lda #$04
/* 1E504: 8D 81 06 */  sta ObjectYSpeed+1
/* 1E507: 20 C2 ED */  jsr BossFuncEDC2
/* 1E50A: A5 46 */     lda RandomSeed
/* 1E50C: 29 03 */     and #$03
/* 1E50E: D0 06 */     bne L1E516
/* 1E510: A5 43 */     lda BossVariable43
/* 1E512: 49 40 */     eor #$40
/* 1E514: 85 43 */     sta BossVariable43
L1E516:
/* 1E516: E6 3E */     inc BossCurrentStrategy
/* 1E518: 60 */        rts


ElecmanStrategy5:
/* 1E519: 20 F8 ED */  jsr BossFuncWalkTowardsMegaman
/* 1E51C: F0 05 */     beq L1E523 ; +
/* 1E51E: A9 05 */     lda #$05
/* 1E520: 85 3E */     sta BossCurrentStrategy
/* 1E522: 60 */        rts
L1E523: ; +
/* 1E523: A5 18 */     lda JoyD0       ; If B has been pressed then inc BossCurrentStrategy
/* 1E525: 29 02 */     and #$02
/* 1E527: F0 02 */     beq L1E52B ; +                                       ; $0001E52B
/* 1E529: E6 3E */     inc BossCurrentStrategy
L1E52B: ; +
/* 1E52B: 60 */        rts

ElecmanStrategy6:
/* 1E52C: A9 00 */     lda #$00
/* 1E52E: 20 A4 E4 */  jsr F1E4A4
/* 1E531: F0 08 */     beq L1E53B
L1E533:
/* 1E533: A9 00 */     lda #$00
/* 1E535: 8D E1 04 */  sta ObjectXSpeedFraction+1
/* 1E538: 8D C1 04 */  sta ObjectXSpeed+1
L1E53B:
/* 1E53B: 60 */        rts




ElecmanStrategy7:
/* 1E53C: 20 B7 E4 */  jsr ElecmanStrategy2
/* 1E53F: D0 F2 */     bne L1E533
/* 1E541: 60 */        rts


ElecmanStrategy8:
/* 1E542: A5 44 */     lda BossVariable44
/* 1E544: D0 04 */     bne L1E54A
/* 1E546: A9 05 */     lda #$05
/* 1E548: 85 3E */     sta BossCurrentStrategy
L1E54A:
/* 1E54A: 20 F8 ED */  jsr BossFuncWalkTowardsMegaman
/* 1E54D: D0 E4 */     bne L1E533
/* 1E54F: 60 */        rts

E550_table: ;at E550
    .byte $B0, $80, $30, $80
    ; Table of Megaman X positions. Used for what?
    ; Right edge, middle, left edge, middle

ElecmanStrategy9:
/* 1E554: A9 59 */     lda #$59
/* 1E556: 20 16 EF */  jsr BossFuncEF16
/* 1E559: 60 */        rts


ElecmanStrategies: ;at E55A
    .word ElecmanStrategy0
    .word ElecmanStrategy1
    .word ElecmanStrategy2
    .word ElecmanStrategy3
    .word ElecmanStrategy4
    .word ElecmanStrategy5
    .word ElecmanStrategy6
    .word ElecmanStrategy7
    .word ElecmanStrategy8
    .word ElecmanStrategy9

BossAI_Gutsman:
/* 1E56E: AD A1 06 */  lda ObjectLifeCycleCounter+1
/* 1E571: F0 01 */     beq L1E574 ; +
/* 1E573: 60 */        rts
L1E574: ; +
/* 1E574: B9 CA E6 */  lda GutsmanStrategies+0,y
/* 1E577: 85 04 */     sta $04
/* 1E579: B9 CB E6 */  lda GutsmanStrategies+1,y
/* 1E57C: 85 05 */     sta $05
/* 1E57E: 6C 04 00 */  jmp ($0004)

GutsmanStrategy0:
F1E581:
/* 1E581: A9 1F */     lda #$1F
/* 1E583: 8D 41 06 */  sta ObjectFireDelay+1
/* 1E586: A9 30 */     lda #$30
/* 1E588: 8D 01 04 */  sta ObjectSpriteNum+1
/* 1E58B: A9 00 */     lda #$00
/* 1E58D: 8D E1 04 */  sta ObjectXSpeedFraction+1
/* 1E590: 8D C1 04 */  sta ObjectXSpeed+1
/* 1E593: 8D 41 04 */  sta ObjectUnknown440+1
/* 1E596: A9 30 */     lda #$30
/* 1E598: 8D 61 06 */  sta ObjectYSpeedFraction+1
/* 1E59B: A9 04 */     lda #$04
/* 1E59D: 8D 81 06 */  sta ObjectYSpeed+1
/* 1E5A0: A9 19 */     lda #$19
/* 1E5A2: 85 3E */     sta BossCurrentStrategy
/* 1E5A4: 60 */        rts



GutsmanStrategy5:
/* 1E5A5: A5 47 */     lda GutsmanStompCounter
/* 1E5A7: D0 0D */     bne L1E5B6 ; +                                           ; $E5B6
/* 1E5A9: A5 46 */     lda RandomSeed
/* 1E5AB: 29 01 */     and #$01
/* 1E5AD: F0 42 */     beq L1E5F1
/* 1E5AF: 20 E0 E5 */  jsr F1E5E0
/* 1E5B2: A9 09 */     lda #$09
/* 1E5B4: 85 3E */     sta BossCurrentStrategy
L1E5B6: ; +
/* 1E5B6: 60 */        rts


GutsmanStrategy3:
/* 1E5B7: A5 47 */     lda GutsmanStompCounter
/* 1E5B9: D0 07 */     bne L1E5C2 ; +                                           ; $E5C2
/* 1E5BB: 20 E0 E5 */  jsr F1E5E0
/* 1E5BE: A9 07 */     lda #$07
/* 1E5C0: 85 3E */     sta BossCurrentStrategy
L1E5C2: ; +
/* 1E5C2: 60 */        rts



GutsmanStrategy7:
/* 1E5C3: A5 47 */     lda GutsmanStompCounter
/* 1E5C5: D0 0E */     bne L1E5D5 ; +                                           ; $E5D5
/* 1E5C7: A5 3E */     lda BossCurrentStrategy
/* 1E5C9: 48 */        pha
/* 1E5CA: 20 C2 ED */  jsr BossFuncEDC2
/* 1E5CD: 20 81 E5 */  jsr F1E581
/* 1E5D0: 68 */        pla
/* 1E5D1: 85 3E */     sta BossCurrentStrategy
/* 1E5D3: C6 3E */     dec BossCurrentStrategy
L1E5D5: ; +
/* 1E5D5: 60 */        rts


GutsmanStrategy15:
/* 1E5D6: A5 47 */     lda GutsmanStompCounter
/* 1E5D8: D0 16 */     bne L1E5F0
/* 1E5DA: A5 46 */     lda RandomSeed
/* 1E5DC: 29 01 */     and #$01
/* 1E5DE: D0 15 */     bne L1E5F5
F1E5E0:
/* 1E5E0: 20 C2 ED */  jsr BossFuncEDC2
/* 1E5E3: 20 81 E5 */  jsr F1E581
/* 1E5E6: A5 43 */     lda BossVariable43
/* 1E5E8: 49 40 */     eor #$40
/* 1E5EA: 85 43 */     sta BossVariable43
/* 1E5EC: A9 13 */     lda #$13
/* 1E5EE: 85 3E */     sta BossCurrentStrategy
L1E5F0:
/* 1E5F0: 60 */        rts

GutsmanStrategy1:
L1E5F1:
/* 1E5F1: A5 47 */     lda GutsmanStompCounter
/* 1E5F3: D0 FB */     bne L1E5F0
L1E5F5:
/* 1E5F5: A9 05 */     lda #$05
/* 1E5F7: 85 3E */     sta BossCurrentStrategy
/* 1E5F9: 60 */        rts

GutsmanStrategy2:
/* 1E5FA: A9 09 */     lda #$09
/* 1E5FC: 8D E1 04 */  sta ObjectXSpeedFraction+1
/* 1E5FF: A9 01 */     lda #$01
/* 1E601: 8D C1 04 */  sta ObjectXSpeed+1
F1E604:
/* 1E604: AD 41 06 */  lda ObjectFireDelay+1
/* 1E607: D0 1E */     bne L1E627
/* 1E609: A5 23 */     lda FrameCounter
/* 1E60B: 29 03 */     and #$03
/* 1E60D: F0 18 */     beq L1E627
/* 1E60F: 20 F8 ED */  jsr BossFuncWalkTowardsMegaman

/* 1E612: A5 2B */     lda $2B
/* 1E614: F0 11 */     beq L1E627 ; +                                           ; $E627

; Make gutsman stomp
/* 1E616: C6 3E */     dec BossCurrentStrategy
/* 1E618: A9 25 */     lda #$25
/* 1E61A: 8D 01 04 */  sta ObjectSpriteNum+1

; Shake screen for 32 frames
/* 1E61D: A9 20 */     lda #$20
/* 1E61F: 85 47 */     sta GutsmanStompCounter

/* 1E621: A9 28 */     lda #$28        ; Gutsman stomping sound
/* 1E623: 20 77 C4 */  jsr IssueSound                                  ; $C477
/* 1E626: 60 */        rts

L1E627: ; +
/* 1E627: A9 00 */     lda #$00
/* 1E629: 60 */        rts


GutsmanStrategy17:
/* 1E62A: A5 44 */     lda BossVariable44
/* 1E62C: D0 27 */     bne L1E655 ; +++
/* 1E62E: 20 C2 ED */  jsr BossFuncEDC2
/* 1E631: A9 03 */     lda #$03
/* 1E633: 20 A0 C5 */  jsr RandomFunc
/* 1E636: C9 01 */     cmp #$01
/* 1E638: 90 17 */     bcc L1E651 ; ++
/* 1E63A: 08 */        php
/* 1E63B: 20 81 E5 */  jsr F1E581
/* 1E63E: 28 */        plp
/* 1E63F: D0 05 */     bne L1E646 ; +
/* 1E641: A9 0F */     lda #$0F
/* 1E643: 85 3E */     sta BossCurrentStrategy
/* 1E645: 60 */        rts
L1E646: ; +
/* 1E646: A9 15 */     lda #$15
/* 1E648: 85 3E */     sta BossCurrentStrategy
/* 1E64A: A5 43 */     lda BossVariable43
/* 1E64C: 49 40 */     eor #$40
/* 1E64E: 85 43 */     sta BossVariable43
/* 1E650: 60 */        rts
L1E651: ; ++
/* 1E651: A9 05 */     lda #$05
/* 1E653: 85 3E */     sta BossCurrentStrategy
L1E655: ; +++
/* 1E655: 60 */        rts



GutsmanStrategy18:
/* 1E656: AE 41 06 */  ldx ObjectFireDelay+1
/* 1E659: D0 FA */     bne L1E655
/* 1E65B: 20 D5 ED */  jsr BossSearchMegaman
/* 1E65E: 48 */        pha
/* 1E65F: C6 3E */     dec BossCurrentStrategy
/* 1E661: A9 02 */     lda #$02
/* 1E663: 20 17 EE */  jsr Boss_FindAmmoSlot
/* 1E666: 68 */        pla
/* 1E667: B0 21 */     bcs L1E68A
/* 1E669: 85 02 */     sta $02
/* 1E66B: A9 04 */     lda #$04
/* 1E66D: 85 01 */     sta $01
/* 1E66F: A9 00 */     lda #$00
/* 1E671: 85 00 */     sta $00
/* 1E673: 18 */        clc
/* 1E674: AD 00 06 */  lda ObjectPosY+0                                 ; ObjectPosY+0
/* 1E677: 69 0C */     adc #$0C
/* 1E679: 85 0C */     sta $0C
/* 1E67B: 38 */        sec
/* 1E67C: AD 01 06 */  lda ObjectPosY+1
/* 1E67F: E9 18 */     sbc #$18
/* 1E681: E5 0C */     sbc $0C
/* 1E683: 85 03 */     sta $03
/* 1E685: 20 C6 F8 */  jsr F8C6_routine
/* 1E688: E6 44 */     inc BossVariable44
L1E68A:
/* 1E68A: 60 */        rts


GutsmanStrategy19:
/* 1E68B: A5 44 */     lda BossVariable44
/* 1E68D: D0 10 */     bne L1E69F ; +
/* 1E68F: A9 2E */     lda #$2E
/* 1E691: 8D 01 04 */  sta ObjectSpriteNum+1
/* 1E694: A9 1F */     lda #$1F
/* 1E696: 8D 41 06 */  sta ObjectFireDelay+1
/* 1E699: A5 47 */     lda GutsmanStompCounter
/* 1E69B: D0 02 */     bne L1E69F ; +                                           ; $E69F
/* 1E69D: C6 3E */     dec BossCurrentStrategy
L1E69F: ; +
/* 1E69F: 60 */        rts


GutsmanStrategy20:
/* 1E6A0: 20 04 E6 */  jsr F1E604
/* 1E6A3: F0 FA */     beq L1E69F
/* 1E6A5: A9 2D */     lda #$2D
/* 1E6A7: 8D 01 04 */  sta ObjectSpriteNum+1
/* 1E6AA: A9 00 */     lda #$00
/* 1E6AC: 8D 41 04 */  sta ObjectUnknown440+1
/* 1E6AF: A9 03 */     lda #$03
/* 1E6B1: 20 17 EE */  jsr Boss_FindAmmoSlot
/* 1E6B4: B0 13 */     bcs L1E6C9
/* 1E6B6: AD 01 06 */  lda ObjectPosY+1
/* 1E6B9: C9 C0 */     cmp #$C0
/* 1E6BB: F0 05 */     beq L1E6C2
/* 1E6BD: A9 19 */     lda #$19
/* 1E6BF: 9D 40 04 */  sta ObjectUnknown440,x
L1E6C2:
/* 1E6C2: A9 20 */     lda #$20
/* 1E6C4: 9D 00 06 */  sta ObjectPosY,x
/* 1E6C7: E6 44 */     inc BossVariable44
L1E6C9:
/* 1E6C9: 60 */        rts



GutsmanStrategies: ;at E6CA
    .word GutsmanStrategy0
    .word GutsmanStrategy1
    .word GutsmanStrategy2
    .word GutsmanStrategy3
    .word GutsmanStrategy2
    .word GutsmanStrategy5
    .word GutsmanStrategy2
    .word GutsmanStrategy7
    .word GutsmanStrategy2
    .word GutsmanStrategy7
    .word GutsmanStrategy2
    .word GutsmanStrategy1
    .word GutsmanStrategy2
    .word GutsmanStrategy7
    .word GutsmanStrategy2
    .word GutsmanStrategy15
    .word GutsmanStrategy2
    .word GutsmanStrategy17
    .word GutsmanStrategy18
    .word GutsmanStrategy19
    .word GutsmanStrategy20

BossAI_Boss6:
/* 1E6F4: A5 56 */     lda BossBlinkState
/* 1E6F6: F0 14 */     beq L1E70C ; ++
/* 1E6F8: C9 04 */     cmp #$04
/* 1E6FA: F0 0C */     beq L1E708 ; +
/* 1E6FC: 20 4F C7 */  jsr PaletteSetupForBGwith3F0
/* 1E6FF: A9 08 */     lda #$08
/* 1E701: 85 37 */     sta PaletteUpdateDelay
/* 1E703: A9 00 */     lda #$00
/* 1E705: 8D A1 06 */  sta ObjectLifeCycleCounter+1
L1E708: ; +
/* 1E708: A9 05 */     lda #$05
/* 1E70A: 85 56 */     sta BossBlinkState
L1E70C: ; ++
/* 1E70C: B9 69 EA */  lda Boss6Strategies+0,y
/* 1E70F: 85 04 */     sta $04
/* 1E711: B9 6A EA */  lda Boss6Strategies+1,y
/* 1E714: 85 05 */     sta $05
/* 1E716: 6C 04 00 */  jmp ($0004)

Boss6Strategy0:
/* 1E719: C6 3F */     dec BossVariable3F
/* 1E71B: F0 01 */     beq L1E71E ; +
/* 1E71D: 60 */        rts
L1E71E: ; +
/* 1E71E: A2 02 */     ldx #$02
L1E720: ; -
/* 1E720: BD 44 E7 */  lda E744_table,x
/* 1E723: 9D D5 03 */  sta BGPalettes + $5,x
/* 1E726: CA */        dex
/* 1E727: 10 F7 */     bpl L1E720 ; -
/* 1E729: 20 3F C7 */  jsr PaletteSetupForBG
/* 1E72C: E6 3E */     inc BossCurrentStrategy
/* 1E72E: AD 60 04 */  lda ObjectPosScreen+0                         ; $0460
/* 1E731: 8D 61 04 */  sta ObjectPosScreen+1
/* 1E734: A9 7A */     lda #$7A
/* 1E736: 8D 01 04 */  sta ObjectSpriteNum+1
/* 1E739: A9 00 */     lda #$00
/* 1E73B: 8D 41 06 */  sta ObjectFireDelay+1
/* 1E73E: 8D A1 06 */  sta ObjectLifeCycleCounter+1
/* 1E741: 85 44 */     sta BossVariable44
/* 1E743: 60 */        rts


E744_table: ;colours for some purpose
    .byte $07,$17,$27


Boss6Strategy1:
/* 1E747: A5 3F */     lda BossVariable3F
/* 1E749: C9 13 */     cmp #$13
/* 1E74B: F0 20 */     beq L1E76D
/* 1E74D: A5 44 */     lda BossVariable44
/* 1E74F: C9 19 */     cmp #$19
/* 1E751: F0 03 */     beq Boss6_CallCreatePiece1
/* 1E753: E6 44 */     inc BossVariable44
/* 1E755: 60 */        rts




Boss6_CallCreatePiece1:
/* 1E756: A9 00 */     lda #$00
/* 1E758: 85 44 */     sta BossVariable44
/* 1E75A: 20 2A E8 */  jsr Boss6_CreatePiece
/* 1E75D: A9 08 */     lda #$08
/* 1E75F: 9D 80 04 */  sta ObjectPosX,x
/* 1E762: A9 31 */     lda #$31
/* 1E764: 9D 40 04 */  sta ObjectUnknown440,x
/* 1E767: A0 24 */     ldy #$24
/* 1E769: 20 33 F5 */  jsr InitObjectDefaultSpeed
/* 1E76C: 60 */        rts





L1E76D:
/* 1E76D: E6 3E */     inc BossCurrentStrategy
/* 1E76F: A9 40 */     lda #$40
/* 1E771: 85 44 */     sta BossVariable44
/* 1E773: 60 */        rts





Boss6Strategy4:
/* 1E774: A5 3F */     lda BossVariable3F
/* 1E776: C9 26 */     cmp #$26
/* 1E778: F0 11 */     beq L1E78B
L1E77A:
/* 1E77A: A5 44 */     lda BossVariable44
/* 1E77C: C9 19 */     cmp #$19
/* 1E77E: F0 03 */     beq Boss6_CallCreatePiece2
/* 1E780: E6 44 */     inc BossVariable44
/* 1E782: 60 */        rts




Boss6_CallCreatePiece2:
/* 1E783: A9 00 */     lda #$00
/* 1E785: 85 44 */     sta BossVariable44
/* 1E787: 20 2A E8 */  jsr Boss6_CreatePiece
/* 1E78A: 60 */        rts



L1E78B:
/* 1E78B: E6 3E */     inc BossCurrentStrategy
/* 1E78D: A9 30 */     lda #$30
/* 1E78F: 85 44 */     sta BossVariable44
/* 1E791: A9 00 */     lda #$00
/* 1E793: 85 3F */     sta BossVariable3F
/* 1E795: 60 */        rts



Boss6Strategy7:
/* 1E796: A5 3F */     lda BossVariable3F
/* 1E798: C9 13 */     cmp #$13
/* 1E79A: D0 DE */     bne L1E77A
/* 1E79C: A9 30 */     lda #$30
/* 1E79E: 85 44 */     sta BossVariable44
/* 1E7A0: A9 07 */     lda #$07
/* 1E7A2: 85 3E */     sta BossCurrentStrategy
/* 1E7A4: 60 */        rts


Boss6Strategy2:
/* 1E7A5: C6 44 */     dec BossVariable44
/* 1E7A7: D0 2A */     bne L1E7D3
/* 1E7A9: A9 05 */     lda #$05
/* 1E7AB: 20 A0 C5 */  jsr RandomFunc
; Specify the location for the eye
/* 1E7AE: A8 */        tay
/* 1E7AF: 0A */        asl a
/* 1E7B0: AA */        tax
/* 1E7B1: A5 3E */     lda BossCurrentStrategy
/* 1E7B3: C9 07 */     cmp #$07
/* 1E7B5: F0 01 */     beq L1E7B8
/* 1E7B7: E8 */        inx
L1E7B8:
/* 1E7B8: BD 1B E8 */  lda Boss6_Xtable,x
/* 1E7BB: 8D 81 04 */  sta ObjectPosX+1
/* 1E7BE: B9 25 E8 */  lda Boss6_Ytable,y
/* 1E7C1: 8D 01 06 */  sta ObjectPosY+1

/* 1E7C4: A9 06 */     lda #$06
/* 1E7C6: 8D 21 04 */  sta ObjectFlags+1

/* 1E7C9: A9 00 */     lda #$00
/* 1E7CB: 8D 41 04 */  sta ObjectUnknown440+1
/* 1E7CE: 20 D5 ED */  jsr BossSearchMegaman
/* 1E7D1: E6 3E */     inc BossCurrentStrategy
L1E7D3:
/* 1E7D3: 60 */        rts




Boss6Strategy3:
/* 1E7D4: AD 41 04 */  lda ObjectUnknown440+1
/* 1E7D7: F0 36 */     beq Boss6_HideEye
/* 1E7D9: C9 20 */     cmp #$20
/* 1E7DB: 90 3D */     bcc L1E81A
/* 1E7DD: F0 10 */     beq Boss6_Shoot
/* 1E7DF: C9 30 */     cmp #$30
/* 1E7E1: B0 37 */     bcs L1E81A
/* 1E7E3: A0 21 */     ldy #$21
/* 1E7E5: C6 44 */     dec BossVariable44
/* 1E7E7: D0 02 */     bne L1E7EB
/* 1E7E9: A0 30 */     ldy #$30
L1E7EB:
/* 1E7EB: 8C 41 04 */  sty ObjectUnknown440+1
/* 1E7EE: 60 */        rts


Boss6_Shoot:
/* 1E7EF: 20 D5 ED */  jsr BossSearchMegaman
/* 1E7F2: 85 0C */     sta $0C
/* 1E7F4: A9 01 */     lda #$01
/* 1E7F6: 85 2F */     sta RefObjectNum
/* 1E7F8: A9 2D */     lda #$2D
/* 1E7FA: 20 63 F6 */  jsr CreateEnemy                                   ; $F663

/* 1E7FD: B0 0B */     bcs L1E80A
/* 1E7FF: A9 07 */     lda #$07
/* 1E801: 85 01 */     sta $01
/* 1E803: A9 00 */     lda #$00
/* 1E805: 85 00 */     sta $00
/* 1E807: 20 28 F6 */  jsr F1F628
L1E80A:
/* 1E80A: A9 3F */     lda #$3F
/* 1E80C: 85 44 */     sta BossVariable44
/* 1E80E: 60 */        rts

Boss6_HideEye:
/* 1E80F: E6 3E */     inc BossCurrentStrategy
/* 1E811: A9 F8 */     lda #$F8
/* 1E813: 8D 01 06 */  sta ObjectPosY+1
/* 1E816: A9 00 */     lda #$00
/* 1E818: 85 56 */     sta BossBlinkState
L1E81A:
/* 1E81A: 60 */        rts

Boss6_Xtable: ;at E81B
    .byte $B8,$48,$B5,$45,$B3,$43,$B4,$44,$B5,$45
Boss6_Ytable: ;at E825
    .byte $55,$5A,$60,   $66,$6C ; ?


Boss6_CreatePiece:
/* 1E82A: A9 00 */     lda #$00
/* 1E82C: 85 2F */     sta RefObjectNum
/* 1E82E: A9 38 */     lda #$38 ;Boss 6 piece
/* 1E830: 20 63 F6 */  jsr CreateEnemy                                   ; $F663

/* 1E833: B0 3D */     bcs L1E872
/* 1E835: A4 3F */     ldy BossVariable3F
/* 1E837: C0 13 */     cpy #$13
/* 1E839: B0 0A */     bcs L1E845
/* 1E83B: A9 42 */     lda #$42
/* 1E83D: 9D 20 04 */  sta ObjectFlags,x
/* 1E840: B9 76 E9 */  lda E976_table,y ; Y = 0-$13
/* 1E843: D0 08 */     bne L1E84D
L1E845:
/* 1E845: A9 02 */     lda #$02
/* 1E847: 9D 20 04 */  sta ObjectFlags,x
/* 1E84A: B9 50 E9 */  lda E963_table-$13,y ; Y = $13-$25
L1E84D:
/* 1E84D: 29 F0 */     and #$F0
/* 1E84F: 9D 80 04 */  sta ObjectPosX,x
/* 1E852: B9 63 E9 */  lda E963_table,y ; Y = 0-$25
/* 1E855: 48 */        pha
/* 1E856: 29 F0 */     and #$F0
/* 1E858: 9D C0 06 */  sta ObjectLifeMeter,x
/* 1E85B: 68 */        pla
/* 1E85C: 0A */        asl a
/* 1E85D: 0A */        asl a
/* 1E85E: 0A */        asl a
/* 1E85F: 0A */        asl a
/* 1E860: 09 08 */     ora #$08
/* 1E862: 9D 00 06 */  sta ObjectPosY,x
/* 1E865: 98 */        tya
/* 1E866: 0A */        asl a
/* 1E867: 0A */        asl a
/* 1E868: 9D 40 06 */  sta ObjectFireDelay,x
/* 1E86B: A0 08 */     ldy #$08
/* 1E86D: 20 33 F5 */  jsr InitObjectDefaultSpeed
/* 1E870: E6 3F */     inc BossVariable3F
L1E872:
/* 1E872: 60 */        rts





F1E873:
/* 1E873: BC 40 06 */  ldy ObjectFireDelay,x
/* 1E876: B9 89 E9 */  lda E989_table,y
/* 1E879: C9 FF */     cmp #$FF
/* 1E87B: D0 22 */     bne L1E89F
/* 1E87D: B9 8B E9 */  lda E989_table+2,y
/* 1E880: 85 05 */     sta $05
/* 1E882: B9 8C E9 */  lda E989_table+3,y
/* 1E885: 85 04 */     sta $04
/* 1E887: BE 8A E9 */  ldx E989_table+1,y
/* 1E88A: 20 B3 E8 */  jsr F1E8B3
/* 1E88D: A6 2F */     ldx RefObjectNum
/* 1E88F: 20 44 E9 */  jsr F1E944
/* 1E892: BC 40 06 */  ldy ObjectFireDelay,x
/* 1E895: B9 8A E9 */  lda E989_table+1,y
/* 1E898: 09 04 */     ora #$04
/* 1E89A: AA */        tax
/* 1E89B: 20 B3 E8 */  jsr F1E8B3
/* 1E89E: 60 */        rts






L1E89F:
/* 1E89F: 20 44 E9 */  jsr F1E944
/* 1E8A2: A2 00 */     ldx #$00
L1E8A4:
/* 1E8A4: B9 89 E9 */  lda E989_table,y
/* 1E8A7: 95 06 */     sta CurrentRoomPointer,x
/* 1E8A9: C8 */        iny
/* 1E8AA: E8 */        inx
/* 1E8AB: E0 04 */     cpx #$04
/* 1E8AD: D0 F5 */     bne L1E8A4
/* 1E8AF: 20 1D E9 */  jsr F1E91D
/* 1E8B2: 60 */        rts






F1E8B3:
/* 1E8B3: A0 00 */     ldy #$00
L1E8B5:
/* 1E8B5: BD 21 EA */  lda EA21_table,x
/* 1E8B8: 99 06 00 */  sta $0006,y
/* 1E8BB: E8 */        inx
/* 1E8BC: C8 */        iny
/* 1E8BD: C0 04 */     cpy #$04
/* 1E8BF: D0 F4 */     bne L1E8B5
/* 1E8C1: 20 1D E9 */  jsr F1E91D
/* 1E8C4: 60 */        rts








F1E8C5:
/* 1E8C5: BD 40 06 */  lda ObjectFireDelay,x
/* 1E8C8: C9 4C */     cmp #$4C
/* 1E8CA: B0 05 */     bcs L1E8D1 ; +
/* 1E8CC: 18 */        clc
/* 1E8CD: 69 4C */     adc #$4C
/* 1E8CF: D0 03 */     bne L1E8D4 ; ++
L1E8D1: ; +
/* 1E8D1: 38 */        sec
/* 1E8D2: E9 4C */     sbc #$4C
L1E8D4: ; ++
/* 1E8D4: 85 0C */     sta $0C
/* 1E8D6: A8 */        tay
/* 1E8D7: B9 89 E9 */  lda E989_table,y
/* 1E8DA: C9 FF */     cmp #$FF
/* 1E8DC: D0 2A */     bne L1E908 ; +++
/* 1E8DE: B9 8B E9 */  lda E989_table+2,y
/* 1E8E1: 85 05 */     sta $05
/* 1E8E3: B9 8C E9 */  lda E989_table+3,y
/* 1E8E6: 85 04 */     sta $04
/* 1E8E8: C0 08 */     cpy #$08
/* 1E8EA: D0 08 */     bne L1E8F4 ; +
/* 1E8EC: A2 40 */     ldx #$40
/* 1E8EE: 20 B3 E8 */  jsr F1E8B3
/* 1E8F1: 4C 03 E9 */  jmp L1E903 ; ++
L1E8F4: ; +
/* 1E8F4: C0 54 */     cpy #$54
/* 1E8F6: D0 08 */     bne L1E900 ; +
/* 1E8F8: A2 44 */     ldx #$44
/* 1E8FA: 20 B3 E8 */  jsr F1E8B3
/* 1E8FD: 4C 03 E9 */  jmp L1E903 ; ++
L1E900: ; +
/* 1E900: 20 12 E9 */  jsr F1E912
L1E903: ; ++
/* 1E903: 20 1D E9 */  jsr F1E91D
/* 1E906: A4 0C */     ldy $0C
L1E908: ; +++
/* 1E908: 20 44 E9 */  jsr F1E944
/* 1E90B: 20 12 E9 */  jsr F1E912
/* 1E90E: 20 1D E9 */  jsr F1E91D
/* 1E911: 60 */        rts


F1E912:
/* 1E912: A9 00 */     lda #$00
/* 1E914: 85 06 */     sta CurrentRoomPointer
/* 1E916: 85 07 */     sta CurrentRoomPointer+1
/* 1E918: 85 08 */     sta CurrentRoomPointer+2
/* 1E91A: 85 09 */     sta CurrentRoomPointer+3
/* 1E91C: 60 */        rts


F1E91D:
/* 1E91D: A5 AA */     lda $AA
/* 1E91F: 0A */        asl a
/* 1E920: A8 */        tay
/* 1E921: A5 05 */     lda $05
/* 1E923: 99 80 03 */  sta RawPPUtransferAddress+0,y
/* 1E926: A5 04 */     lda $04
/* 1E928: 99 81 03 */  sta RawPPUtransferAddress+1,y
/* 1E92B: 98 */        tya
/* 1E92C: 0A */        asl a
/* 1E92D: A8 */        tay
/* 1E92E: A2 00 */     ldx #$00
L1E930:
/* 1E930: B5 06 */     lda CurrentRoomPointer,x
/* 1E932: 99 94 03 */  sta $0394,y
/* 1E935: C8 */        iny
/* 1E936: E8 */        inx
/* 1E937: E0 04 */     cpx #$04
/* 1E939: D0 F5 */     bne L1E930
/* 1E93B: A6 2F */     ldx RefObjectNum
/* 1E93D: E6 AA */     inc $AA

/* 1E93F: A9 FF */     lda #$FF
/* 1E941: 85 5E */     sta RawPPUtransferSize
/* 1E943: 60 */        rts






F1E944:
/* 1E944: A9 09 */     lda #$09
/* 1E946: 85 05 */     sta $05
/* 1E948: BD 00 06 */  lda ObjectPosY,x
/* 1E94B: 29 F0 */     and #$F0
/* 1E94D: 0A */        asl a
/* 1E94E: 26 05 */     rol $05
/* 1E950: 0A */        asl a
/* 1E951: 26 05 */     rol $05
/* 1E953: 85 04 */     sta $04
/* 1E955: 38 */        sec
/* 1E956: BD 80 04 */  lda ObjectPosX,x
/* 1E959: E9 08 */     sbc #$08
/* 1E95B: 4A */        lsr a
/* 1E95C: 4A */        lsr a
/* 1E95D: 4A */        lsr a
/* 1E95E: 05 04 */     ora $04
/* 1E960: 85 04 */     sta $04
/* 1E962: 60 */        rts

E963_table:
    .byte $E8,$E7,$E9,$E6
    .byte $D7,$D8,$D6,$D5
    .byte $C9,$C8,$C7,$C5,$C6
    .byte $B8,$B6,$B7,$B5
    .byte $B9,$A7
E976_table:
    .byte $28,$27,$29,$26
    .byte $37,$38,$36,$35
    .byte $49,$48,$47,$45,$46
    .byte $58,$56,$57,$55,$59
    .byte $67
E989_table:
    .byte $A1,$00,$A8,$A9
    .byte $96,$97,$9D,$00
    .byte $FF,$00,$26,$5D
    .byte $FF,$08,$25,$5B
    .byte $87,$95,$87,$9C
    .byte $FF,$10,$26,$59
    .byte $87,$8B,$87,$8F
    .byte $84,$85,$87,$88
    .byte $AC,$00,$B3,$00
    .byte $87,$87,$A4,$A5
    .byte $87,$87,$87,$87
    .byte $82,$83,$87,$87
    .byte $87,$87,$87,$87
    .byte $9E,$9F,$A2,$A3
    .byte $8A,$87,$8E,$87
    .byte $94,$87,$9A,$9B
    .byte $80,$81,$86,$87
    .byte $FF,$18,$26,$53
    .byte $92,$93,$98,$99
    .byte $00,$EE,$E6,$E7
    .byte $D8,$D9,$00,$D2
    .byte $FF,$20,$26,$41
    .byte $FF,$28,$25,$43
    .byte $DA,$87,$D3,$87
    .byte $FF,$30,$26,$45
    .byte $C4,$87,$C0,$87
    .byte $CA,$CB,$C7,$87
    .byte $00,$E3,$00,$BA
    .byte $87,$87,$EA,$EB
    .byte $87,$87,$87,$87
    .byte $CC,$CD,$87,$87
    .byte $87,$87,$87,$87
    .byte $D0,$D1,$EC,$ED
    .byte $87,$C5,$87,$C1
    .byte $87,$DB,$D4,$D5
    .byte $CE,$CF,$87,$C9
    .byte $FF,$38,$26,$4B
    .byte $DC,$DD,$D6,$D7
EA21_table:
    .byte $00,$40,$B6,$41
    .byte $AE,$AF,$B4,$B5
    .byte $00,$00,$89,$00
    .byte $8C,$8D,$90,$91
    .byte $00,$AD,$00,$00
    .byte $87,$A0,$A6,$A7
    .byte $00,$00,$00,$B0
    .byte $AA,$AB,$B1,$B2
    .byte $18,$00,$19,$B7
    .byte $E0,$E1,$B8,$B9
    .byte $00,$00,$00,$C6
    .byte $C2,$C3,$DE,$DF
    .byte $E2,$00,$00,$00
    .byte $EF,$87,$E8,$E9
    .byte $00,$00,$BD,$00
    .byte $E4,$E5,$BB,$BC
    .byte $00,$40,$00,$41
    .byte $18,$00,$19,$00

Boss6Strategies: ;at EA69
    .word Boss6Strategy0
    .word Boss6Strategy1
    .word Boss6Strategy2
    .word Boss6Strategy3
    .word Boss6Strategy4
    .word Boss6Strategy2
    .word Boss6Strategy3
    .word Boss6Strategy7

BossAI_Boss7:
/* 1EA79: A5 56 */     lda BossBlinkState
/* 1EA7B: F0 0C */     beq L1EA89
/* 1EA7D: C9 1F */     cmp #$1F
/* 1EA7F: 90 08 */     bcc L1EA89
/* 1EA81: A0 12 */     ldy #$12
/* 1EA83: AD A1 06 */  lda ObjectLifeCycleCounter+1
/* 1EA86: F0 01 */     beq L1EA89 ; +
/* 1EA88: 60 */        rts
L1EA89: ; +
/* 1EA89: B9 3B EC */  lda Boss7Strategies+0,y
/* 1EA8C: 85 04 */     sta $04
/* 1EA8E: B9 3C EC */  lda Boss7Strategies+1,y
/* 1EA91: 85 05 */     sta $05
/* 1EA93: 6C 04 00 */  jmp ($0004)
/* 1EA96: 60 */        rts






Boss7Strategy0:
/* 1EA97: A9 06 */     lda #$06
/* 1EA99: 20 53 E4 */  jsr F1E453
/* 1EA9C: A9 60 */     lda #$60
/* 1EA9E: 8D E1 04 */  sta ObjectXSpeedFraction+1
/* 1EAA1: A9 01 */     lda #$01
/* 1EAA3: 8D C1 04 */  sta ObjectXSpeed+1
/* 1EAA6: 60 */        rts





Boss7Strategy1:
/* 1EAA7: 20 4D EB */  jsr F1EB4D
/* 1EAAA: 90 07 */     bcc L1EAB3
/* 1EAAC: 20 9C E4 */  jsr ElecmanStrategy1
/* 1EAAF: 20 B8 EA */  jsr F1EAB8
/* 1EAB2: 60 */        rts

L1EAB3:
/* 1EAB3: A9 08 */     lda #$08
/* 1EAB5: 85 3E */     sta BossCurrentStrategy
/* 1EAB7: 60 */        rts





; If selected weapon is cutman or bombman then $401 = #$6E, else $401 = #$01
F1EAB8:
/* 1EAB8: A0 6E */     ldy #$6E
/* 1EABA: A5 5F */     lda WeaponSelect
/* 1EABC: C9 01 */     cmp #$01
/* 1EABE: F0 06 */     beq L1EAC6 ; +                                       ; $EAC6
/* 1EAC0: C9 03 */     cmp #$03
/* 1EAC2: F0 02 */     beq L1EAC6 ; +                                       ; $EAC6
/* 1EAC4: A0 01 */     ldy #$01
L1EAC6: ; +
/* 1EAC6: 8C 01 04 */  sty ObjectSpriteNum+1

/* 1EAC9: A9 00 */     lda #$00
/* 1EACB: 8D 41 06 */  sta ObjectFireDelay+1
/* 1EACE: 60 */        rts




Boss7Strategy2:
/* 1EACF: 20 6B EB */  jsr Boss7_DoWeaponAI
/* 1EAD2: B0 0A */     bcs L1EADE ; +
/* 1EAD4: A9 08 */     lda #$08
L1EAD6: ; -
/* 1EAD6: E6 3E */     inc BossCurrentStrategy
/* 1EAD8: 85 3F */     sta BossVariable3F
/* 1EADA: 20 F8 ED */  jsr BossFuncWalkTowardsMegaman
/* 1EADD: 60 */        rts
L1EADE: ; +
/* 1EADE: A9 00 */     lda #$00
/* 1EAE0: F0 F4 */     beq L1EAD6 ; - ;unconditional jump


Boss7Strategy3:
/* 1EAE2: 20 F8 ED */  jsr BossFuncWalkTowardsMegaman
/* 1EAE5: A5 3F */     lda BossVariable3F
/* 1EAE7: D0 04 */     bne L1EAED ; +
/* 1EAE9: 4C D3 E4 */  jmp L1E4D3
/* 1EAEC: 60 */        rts
L1EAED: ; +
/* 1EAED: C6 3F */     dec BossVariable3F
/* 1EAEF: 60 */        rts



Boss7Strategy4:
/* 1EAF0: A9 09 */     lda #$09
/* 1EAF2: 4C EA E4 */  jmp L1E4EA

Boss7Strategy5:
/* 1EAF5: 4C 19 E5 */  jmp ElecmanStrategy5

Boss7Strategy6:
/* 1EAF8: 20 4D EB */  jsr F1EB4D
/* 1EAFB: 90 2A */     bcc L1EB27 ; +++ ; EB27
/* 1EAFD: A9 00 */     lda #$00
/* 1EAFF: 20 A4 E4 */  jsr F1E4A4
/* 1EB02: D0 17 */     bne F1EB1B ; ++ ; $EB1B
; If selected weapon is cutman or bombman then $401 = #$6F, else $401 = #$09
/* 1EB04: A0 6F */     ldy #$6F
/* 1EB06: A5 5F */     lda WeaponSelect
/* 1EB08: C9 01 */     cmp #$01
/* 1EB0A: F0 06 */     beq L1EB12 ; +                                       ; $EB12
/* 1EB0C: C9 03 */     cmp #$03
/* 1EB0E: F0 02 */     beq L1EB12 ; +                                       ; $EB12
/* 1EB10: A0 09 */     ldy #$09
L1EB12: ; +
/* 1EB12: 8C 01 04 */  sty ObjectSpriteNum+1
/* 1EB15: A9 00 */     lda #$00
/* 1EB17: 8D 41 06 */  sta ObjectFireDelay+1
/* 1EB1A: 60 */        rts
F1EB1B: ; ++
/* 1EB1B: A9 00 */     lda #$00
/* 1EB1D: 8D E1 04 */  sta ObjectXSpeedFraction+1
/* 1EB20: 8D C1 04 */  sta ObjectXSpeed+1
/* 1EB23: 20 B8 EA */  jsr F1EAB8
/* 1EB26: 60 */        rts
L1EB27: ; +++
/* 1EB27: A9 0D */     lda #$0D
/* 1EB29: 85 3E */     sta BossCurrentStrategy
/* 1EB2B: 60 */        rts


Boss7Strategy7:
/* 1EB2C: 20 CF EA */  jsr Boss7Strategy2
/* 1EB2F: D0 EA */     bne F1EB1B
/* 1EB31: 60 */        rts

Boss7Strategy8:
/* 1EB32: 20 F8 ED */  jsr BossFuncWalkTowardsMegaman
/* 1EB35: F0 0F */     beq L1EB46 ; ++
/* 1EB37: 20 1B EB */  jsr F1EB1B
/* 1EB3A: A5 3F */     lda BossVariable3F
/* 1EB3C: F0 04 */     beq L1EB42 ; +
/* 1EB3E: C6 3F */     dec BossVariable3F
/* 1EB40: D0 04 */     bne L1EB46 ; ++
L1EB42: ; +
/* 1EB42: A9 05 */     lda #$05
/* 1EB44: 85 3E */     sta BossCurrentStrategy
L1EB46: ; ++
/* 1EB46: 60 */        rts



Boss7Strategy9:
/* 1EB47: A9 12 */     lda #$12
/* 1EB49: 20 16 EF */  jsr BossFuncEF16
/* 1EB4C: 60 */        rts


; If selected weapon is magnet beam or ??? (fireman/iceman) then clear carry
; else
F1EB4D:
/* 1EB4D: A5 5F */     lda WeaponSelect
/* 1EB4F: C9 06 */     cmp #$06
/* 1EB51: 90 02 */     bcc L1EB55 ; +                                       ; $EB55
/* 1EB53: 18 */        clc
/* 1EB54: 60 */        rts
L1EB55: ; +
/* 1EB55: C9 01 */     cmp #$01
/* 1EB57: F0 06 */     beq L1EB5F ; +                                       ; $EB5F
/* 1EB59: C9 03 */     cmp #$03
/* 1EB5B: F0 06 */     beq L1EB63 ; ++                                      ; $EB63
/* 1EB5D: 38 */        sec
/* 1EB5E: 60 */        rts
L1EB5F: ; +
/* 1EB5F: A9 33 */     lda #$33
/* 1EB61: D0 02 */     bne L1EB65 ; +++ ;$EB65
L1EB63: ; ++
/* 1EB63: A9 35 */     lda #$35
L1EB65: ; +++
/* 1EB65: 85 0C */     sta $0C
/* 1EB67: 20 16 F5 */  jsr FindObjectOfSelectedType
/* 1EB6A: 60 */        rts


Boss7_DoWeaponAI:
/* 1EB6B: A9 01 */     lda #$01
/* 1EB6D: 85 2F */     sta RefObjectNum
/* 1EB6F: A6 5F */     ldx WeaponSelect
/* 1EB71: B5 6A */     lda Meters,x
/* 1EB73: F0 5A */     beq Boss7_NoWeaponAI
/* 1EB75: 8A */        txa
/* 1EB76: 0A */        asl a
/* 1EB77: A8 */        tay
/* 1EB78: B9 2B EC */  lda Boss7WeaponAItable+0,y
/* 1EB7B: 85 04 */     sta $04
/* 1EB7D: B9 2C EC */  lda Boss7WeaponAItable+1,y
/* 1EB80: 85 05 */     sta $05
/* 1EB82: 6C 04 00 */  jmp ($0004)


Boss7_WeaponAI_P:
/* 1EB85: A9 0D */     lda #$0D
/* 1EB87: 20 17 EE */  jsr Boss_FindAmmoSlot
/* 1EB8A: B0 43 */     bcs Boss7_NoWeaponAI
/* 1EB8C: A9 00 */     lda #$00
/* 1EB8E: 9D E0 04 */  sta ObjectXSpeedFraction,x
/* 1EB91: A9 04 */     lda #$04
/* 1EB93: 9D C0 04 */  sta ObjectXSpeed,x
/* 1EB96: 18 */        clc
/* 1EB97: 60 */        rts

Boss7_WeaponAI_C:
/* 1EB98: A9 33 */     lda #$33
/* 1EB9A: 20 63 F6 */  jsr CreateEnemy                                   ; $F663
/* 1EB9D: B0 30 */     bcs Boss7_NoWeaponAI
/* 1EB9F: A0 78 */     ldy #$78
Boss7_FinishShot:
/* 1EBA1: 20 33 F5 */  jsr InitObjectDefaultSpeed
/* 1EBA4: 18 */        clc
/* 1EBA5: 60 */        rts

Boss7_WeaponAI_I:
/* 1EBA6: A9 34 */     lda #$34
/* 1EBA8: 20 63 F6 */  jsr CreateEnemy                                   ; $F663
/* 1EBAB: B0 22 */     bcs Boss7_NoWeaponAI
/* 1EBAD: A0 24 */     ldy #$24
/* 1EBAF: D0 F0 */     bne Boss7_FinishShot

Boss7_WeaponAI_B:
/* 1EBB1: A9 35 */     lda #$35
/* 1EBB3: 20 63 F6 */  jsr CreateEnemy                                   ; $F663
/* 1EBB6: B0 17 */     bcs Boss7_NoWeaponAI
/* 1EBB8: A9 03 */     lda #$03
/* 1EBBA: 85 49 */     sta $49
/* 1EBBC: BD 20 04 */  lda ObjectFlags,x
/* 1EBBF: 29 40 */     and #$40
/* 1EBC1: 09 11 */     ora #$11
/* 1EBC3: 9D 20 04 */  sta ObjectFlags,x
/* 1EBC6: A9 90 */     lda #$90
/* 1EBC8: 9D 40 06 */  sta ObjectFireDelay,x
/* 1EBCB: A0 7C */     ldy #$7C
/* 1EBCD: D0 D2 */     bne Boss7_FinishShot

Boss7_NoWeaponAI:
/* 1EBCF: 38 */        sec
/* 1EBD0: 60 */        rts

Boss7_WeaponAI_F:
/* 1EBD1: A9 36 */     lda #$36
/* 1EBD3: 20 63 F6 */  jsr CreateEnemy                                   ; $F663
/* 1EBD6: B0 F7 */     bcs Boss7_NoWeaponAI
/* 1EBD8: A0 24 */     ldy #$24
/* 1EBDA: 20 33 F5 */  jsr InitObjectDefaultSpeed
/* 1EBDD: A9 37 */     lda #$37
/* 1EBDF: 20 63 F6 */  jsr CreateEnemy                                   ; $F663
/* 1EBE2: B0 EB */     bcs Boss7_NoWeaponAI
/* 1EBE4: A9 20 */     lda #$20
/* 1EBE6: 9D 40 06 */  sta ObjectFireDelay,x
/* 1EBE9: A0 08 */     ldy #$08
/* 1EBEB: D0 B4 */     bne Boss7_FinishShot
Boss7_WeaponAI_E:
/* 1EBED: A9 07 */     lda #$07
L1EBEF:
/* 1EBEF: 85 0C */     sta $0C
/* 1EBF1: 20 17 EE */  jsr Boss_FindAmmoSlot
/* 1EBF4: B0 D9 */     bcs Boss7_NoWeaponAI
/* 1EBF6: A9 50 */     lda #$50
/* 1EBF8: 9D E0 04 */  sta ObjectXSpeedFraction,x
/* 1EBFB: A9 01 */     lda #$01
/* 1EBFD: 9D C0 04 */  sta ObjectXSpeed,x
/* 1EC00: E6 44 */     inc BossVariable44
/* 1EC02: E6 0C */     inc $0C
/* 1EC04: A5 0C */     lda $0C
/* 1EC06: C9 0C */     cmp #$0C
/* 1EC08: D0 E5 */     bne L1EBEF
/* 1EC0A: A5 AC */     lda FightingBossNum
/* 1EC0C: C9 07 */     cmp #$07
/* 1EC0E: D0 19 */     bne L1EC29
/* 1EC10: A9 0E */     lda #$0E
L1EC12:
/* 1EC12: 85 0C */     sta $0C
/* 1EC14: 20 17 EE */  jsr Boss_FindAmmoSlot
/* 1EC17: B0 B6 */     bcs Boss7_NoWeaponAI
/* 1EC19: A9 00 */     lda #$00
/* 1EC1B: 9D E0 04 */  sta ObjectXSpeedFraction,x
/* 1EC1E: 9D C0 04 */  sta ObjectXSpeed,x
/* 1EC21: E6 0C */     inc $0C
/* 1EC23: A5 0C */     lda $0C
/* 1EC25: C9 10 */     cmp #$10
/* 1EC27: D0 E9 */     bne L1EC12
L1EC29:
/* 1EC29: 18 */        clc
/* 1EC2A: 60 */        rts


Boss7WeaponAItable: ; at EC2B
    .word Boss7_WeaponAI_P ;P
    .word Boss7_WeaponAI_C ;C
    .word Boss7_WeaponAI_I ;I
    .word Boss7_WeaponAI_B ;B
    .word Boss7_WeaponAI_F ;F
    .word Boss7_WeaponAI_E ;E
    .word Boss7_NoWeaponAI ;G (idle)
    .word Boss7_NoWeaponAI ;M (idle)

Boss7Strategies: ; at EC3B
    .word Boss7Strategy0
    .word Boss7Strategy1
    .word Boss7Strategy2
    .word Boss7Strategy3
    .word Boss7Strategy4
    .word Boss7Strategy5
    .word Boss7Strategy6
    .word Boss7Strategy7
    .word Boss7Strategy8
    .word Boss7Strategy9

BossAI_Boss8:
/* 1EC4F: A5 3E */     lda BossCurrentStrategy
/* 1EC51: C9 06 */     cmp #$06
/* 1EC53: B0 10 */     bcs L1EC65 ; +
/* 1EC55: C6 44 */     dec BossVariable44
/* 1EC57: D0 0E */     bne L1EC67 ; ++
/* 1EC59: A9 00 */     lda #$00
/* 1EC5B: 85 44 */     sta BossVariable44
/* 1EC5D: E6 3E */     inc BossCurrentStrategy
/* 1EC5F: 85 2F */     sta RefObjectNum
/* 1EC61: 20 A6 F4 */  jsr F1F4A6
/* 1EC64: 60 */        rts
L1EC65: ; +
/* 1EC65: D0 01 */     bne L1EC68 ; +++
; -
L1EC67: ; ++
/* 1EC67: 60 */        rts
L1EC68: ; +++
/* 1EC68: AD C1 06 */  lda ObjectLifeMeter+1
/* 1EC6B: D0 03 */     bne L1EC70 ; +                                           ; $EC70
/* 1EC6D: 4C 5A C0 */  jmp StageClear                                  ; $C05A
L1EC70: ; +
/* 1EC70: C6 3F */     dec BossVariable3F
/* 1EC72: D0 F3 */     bne L1EC67 ; -
/* 1EC74: 20 A6 F4 */  jsr F1F4A6
/* 1EC77: B0 EE */     bcs L1EC67 ; -
/* 1EC79: C6 3E */     dec BossCurrentStrategy
/* 1EC7B: 60 */        rts





BossAI_Boss9:
/* 1EC7C: EE B1 06 */  inc $06B1
/* 1EC7F: 20 B8 EC */  jsr BossAI_funcECB8
/* 1EC82: A9 00 */     lda #$00
/* 1EC84: 85 0C */     sta $0C
/* 1EC86: 20 D5 ED */  jsr BossSearchMegaman
/* 1EC89: 38 */        sec
/* 1EC8A: E9 08 */     sbc #$08
/* 1EC8C: 90 02 */     bcc L1EC90 ; +
/* 1EC8E: 85 0C */     sta $0C
L1EC90: ; +
/* 1EC90: C6 44 */     dec BossVariable44
/* 1EC92: D0 23 */     bne L1ECB7 ; +
/* 1EC94: A9 20 */     lda #$20
/* 1EC96: 85 44 */     sta BossVariable44
/* 1EC98: A6 0C */     ldx $0C
/* 1EC9A: A0 00 */     ldy #$00
/* 1EC9C: A9 33 */     lda #$33
/* 1EC9E: 20 AC C5 */  jsr EnemyCalculateJumpCurveToHitMegaman
/* 1ECA1: A2 16 */     ldx #$16
/* 1ECA3: 20 76 C5 */  jsr FindFreeObject                              ; FindFreeObject
/* 1ECA6: B0 0F */     bcs L1ECB7 ; +
/* 1ECA8: A0 70 */     ldy #$70
/* 1ECAA: 20 27 EE */  jsr F1EE27
/* 1ECAD: A5 05 */     lda $05
/* 1ECAF: 9D C0 04 */  sta ObjectXSpeed,x
/* 1ECB2: A5 04 */     lda $04
/* 1ECB4: 9D E0 04 */  sta ObjectXSpeedFraction,x
L1ECB7: ; +
/* 1ECB7: 60 */        rts




BossAI_funcECB8:
/* 1ECB8: A9 01 */     lda #$01
/* 1ECBA: 85 2F */     sta RefObjectNum
/* 1ECBC: A5 56 */     lda BossBlinkState
/* 1ECBE: F0 0B */     beq L1ECCB
/* 1ECC0: C9 04 */     cmp #$04
/* 1ECC2: D0 07 */     bne L1ECCB
/* 1ECC4: 20 4F C7 */  jsr PaletteSetupForBGwith3F0
/* 1ECC7: A9 08 */     lda #$08
/* 1ECC9: 85 37 */     sta PaletteUpdateDelay
L1ECCB:
/* 1ECCB: A6 3E */     ldx BossCurrentStrategy
/* 1ECCD: 18 */        clc
/* 1ECCE: BD 34 ED */  lda ED39_table-5+0,x
/* 1ECD1: 85 0C */     sta $0C
/* 1ECD3: BD 36 ED */  lda ED39_table-5+2,x
/* 1ECD6: 85 0D */     sta $0D
/* 1ECD8: BD 38 ED */  lda ED39_table-5+4,x
/* 1ECDB: 85 0E */     sta $0E
/* 1ECDD: A2 01 */     ldx #$01
/* 1ECDF: 20 1F ED */  jsr UpdateObjectMoveHorizontally ; Update object $01
/* 1ECE2: A2 10 */     ldx #$10
/* 1ECE4: A0 00 */     ldy #$00
L1ECE6:
/* 1ECE6: 20 1F ED */  jsr UpdateObjectMoveHorizontally ; Update object $10
/* 1ECE9: B9 3F ED */  lda ED3F_table,y
/* 1ECEC: 9D 00 06 */  sta ObjectPosY,x
/* 1ECEF: E8 */        inx
/* 1ECF0: C8 */        iny
/* 1ECF1: C0 06 */     cpy #$06
/* 1ECF3: D0 F1 */     bne L1ECE6
/* 1ECF5: AD 90 04 */  lda $0490
/* 1ECF8: A6 3E */     ldx BossCurrentStrategy
/* 1ECFA: E0 05 */     cpx #$05
/* 1ECFC: D0 08 */     bne L1ED06
/* 1ECFE: C9 6C */     cmp #$6C
/* 1ED00: B0 0A */     bcs L1ED0C
/* 1ED02: E6 3E */     inc BossCurrentStrategy
/* 1ED04: D0 06 */     bne L1ED0C
L1ED06:
/* 1ED06: C9 EC */     cmp #$EC
/* 1ED08: 90 02 */     bcc L1ED0C
/* 1ED0A: C6 3E */     dec BossCurrentStrategy

L1ED0C:
/* 1ED0C: 38 */        sec
/* 1ED0D: A9 AC */     lda #$AC
/* 1ED0F: ED 90 04 */  sbc $0490
/* 1ED12: 85 B2 */     sta TempScrollPosX

/* 1ED14: A5 1B */     lda ScrollPosScreen
/* 1ED16: E9 00 */     sbc #$00
/* 1ED18: 85 B3 */     sta TempScrollPosScreen

/* 1ED1A: A9 01 */     lda #$01
/* 1ED1C: 85 B4 */     sta UseTempScrollX
/* 1ED1E: 60 */        rts




; Adds the value of $0C into ObjectPosXfraction
; Adds the value of $0D into ObjectPosX
; Adds the value of $0E into ObjectPosScreen
UpdateObjectMoveHorizontally:  ; Different from ObjectRelocateHorizontally
/* 1ED1F: 18 */        clc
/* 1ED20: BD A0 04 */  lda ObjectPosXfraction,x
/* 1ED23: 65 0C */     adc $0C
/* 1ED25: 9D A0 04 */  sta ObjectPosXfraction,x
/* 1ED28: BD 80 04 */  lda ObjectPosX,x
/* 1ED2B: 65 0D */     adc $0D
/* 1ED2D: 9D 80 04 */  sta ObjectPosX,x
/* 1ED30: BD 60 04 */  lda ObjectPosScreen,x
/* 1ED33: 65 0E */     adc $0E
/* 1ED35: 9D 60 04 */  sta ObjectPosScreen,x
/* 1ED38: 60 */        rts

ED39_table:
    .byte $AE,$52
    .byte $FF,$00
    .byte $FF,$00
ED3F_table:
    .byte $46,$F8,$7B,$63,$93,$77

BossAI_Boss10:
/* 1ED45: 20 B8 EC */  jsr BossAI_funcECB8
/* 1ED48: A9 73 */     lda #$73
/* 1ED4A: 8D 11 06 */  sta $0611
/* 1ED4D: C6 44 */     dec BossVariable44
/* 1ED4F: D0 3A */     bne L1ED8B
/* 1ED51: A9 7E */     lda #$7E
/* 1ED53: 85 44 */     sta BossVariable44
/* 1ED55: 20 D5 ED */  jsr BossSearchMegaman
/* 1ED58: 85 0C */     sta $0C
/* 1ED5A: A9 01 */     lda #$01
/* 1ED5C: 85 2F */     sta RefObjectNum
/* 1ED5E: A2 16 */     ldx #$16
/* 1ED60: 20 76 C5 */  jsr FindFreeObject                              ; FindFreeObject
/* 1ED63: B0 26 */     bcs L1ED8B
/* 1ED65: A0 77 */     ldy #$77
/* 1ED67: 20 27 EE */  jsr F1EE27
/* 1ED6A: A9 49 */     lda #$49
/* 1ED6C: 9D E0 06 */  sta ObjectType,x
/* 1ED6F: 86 2F */     stx RefObjectNum
/* 1ED71: A9 02 */     lda #$02
/* 1ED73: 85 01 */     sta $01
/* 1ED75: A9 00 */     lda #$00
/* 1ED77: 85 00 */     sta $00
/* 1ED79: 20 28 F6 */  jsr F1F628
; ?
/* 1ED7C: A2 16 */     ldx #$16
/* 1ED7E: 20 76 C5 */  jsr FindFreeObject                              ; FindFreeObject
; ??
/* 1ED81: A9 48 */     lda #$48
/* 1ED83: 20 7B F6 */  jsr InitActor
; Object $48 stores the index of object $49 in ObjectLifeMeter.
/* 1ED86: A5 2F */     lda RefObjectNum
/* 1ED88: 9D C0 06 */  sta ObjectLifeMeter,x

L1ED8B:
/* 1ED8B: A9 06 */     lda #$06
/* 1ED8D: 8D 21 04 */  sta ObjectFlags+1

/* 1ED90: 60 */        rts


; Table of boss $0401 values. Sprite number? Or something
BossInitialStatus: ; at ED91
    .byte $1C,$20,$1E,$22,$24,$26
    .byte $00,$00,$00
BossInitialXcoord: ; at ED9A
    .byte $C0,$C0,$C0,$C0,$C0,$C0
    .byte $C0,$B0,$00
BossInitialYcoord:
    .byte $B4,$B4,$B4,$B4,$B4,$B0
    .byte $B4,$B4,$00

; Table of boss strategy routines?
BossAI_Table: ;at EDAC
    .word BossAI_Cutman
    .word BossAI_Iceman
    .word BossAI_Bombman
    .word BossAI_Fireman
    .word BossAI_Elecman
    .word BossAI_Gutsman
    .word BossAI_Boss6
    .word BossAI_Boss7
    .word BossAI_Boss8
    .word BossAI_Boss9
    .word BossAI_Boss10

BossFuncEDC2:
/* 1EDC2: A5 43 */     lda BossVariable43
/* 1EDC4: 29 BF */     and #$BF
/* 1EDC6: 85 43 */     sta BossVariable43
/* 1EDC8: 20 D5 ED */  jsr BossSearchMegaman
/* 1EDCB: AD 21 04 */  lda ObjectFlags+1
/* 1EDCE: 29 40 */     and #$40
/* 1EDD0: 05 43 */     ora BossVariable43
/* 1EDD2: 85 43 */     sta BossVariable43
/* 1EDD4: 60 */        rts


BossSearchMegaman:
/* 1EDD5: 38 */        sec
/* 1EDD6: AD 80 04 */  lda ObjectPosX+0
/* 1EDD9: ED 81 04 */  sbc ObjectPosX+1
/* 1EDDC: 90 0B */     bcc L1EDE9 ; +
/* 1EDDE: 48 */        pha
/* 1EDDF: AD 21 04 */  lda ObjectFlags+1
/* 1EDE2: 09 40 */     ora #$40
/* 1EDE4: 8D 21 04 */  sta ObjectFlags+1
/* 1EDE7: 68 */        pla
/* 1EDE8: 60 */        rts
L1EDE9: ; +
/* 1EDE9: 49 FF */     eor #$FF
/* 1EDEB: 69 01 */     adc #$01
/* 1EDED: 48 */        pha
/* 1EDEE: AD 21 04 */  lda ObjectFlags+1
/* 1EDF1: 29 BF */     and #$BF
/* 1EDF3: 8D 21 04 */  sta ObjectFlags+1
/* 1EDF6: 68 */        pla
/* 1EDF7: 60 */        rts

BossFuncWalkTowardsMegaman:
/* 1EDF8: 20 D5 ED */  jsr BossSearchMegaman
F1EDFB:
/* 1EDFB: A2 01 */     ldx #$01
/* 1EDFD: 86 2F */     stx RefObjectNum
/* 1EDFF: A5 43 */     lda BossVariable43
/* 1EE01: 29 40 */     and #$40
/* 1EE03: F0 06 */     beq L1EE0B ; +
/* 1EE05: 20 F1 9D */  jsr ObjectMoveToTheRight
/* 1EE08: 4C 0E EE */  jmp L1EE0E ; ++
L1EE0B: ; +
/* 1EE0B: 20 6F 9E */  jsr ObjectMoveToTheLeft
L1EE0E: ; ++
/* 1EE0E: 20 D8 98 */  jsr ObjectRelocateHorizontally
/* 1EE11: 20 C4 9B */  jsr ObjectDoCollisionChecksAndAvoidWalls
/* 1EE14: A5 2B */     lda $2B
/* 1EE16: 60 */        rts


Boss_FindAmmoSlot:
/* 1EE17: 85 0D */     sta $0D
/* 1EE19: 0A */        asl a
/* 1EE1A: 0A */        asl a
/* 1EE1B: 0A */        asl a
/* 1EE1C: 38 */        sec
/* 1EE1D: E5 0D */     sbc $0D
/* 1EE1F: A8 */        tay
/* 1EE20: A2 10 */     ldx #$10
/* 1EE22: 20 76 C5 */  jsr FindFreeObject                              ; FindFreeObject
/* 1EE25: B0 70 */     bcs L1EE97
; Reset object (param: Y - values: 1C, 70 or 77)
F1EE27:
/* 1EE27: A9 00 */     lda #$00
/* 1EE29: 9D A0 06 */  sta ObjectLifeCycleCounter,x
/* 1EE2C: 9D 20 06 */  sta ObjectPosYfraction,x
/* 1EE2F: 9D A0 04 */  sta ObjectPosXfraction,x
/* 1EE32: 9D 40 06 */  sta ObjectFireDelay,x
/* 1EE35: B9 98 EE */  lda EE98_table+0,y
/* 1EE38: 9D 00 04 */  sta ObjectSpriteNum,x
/* 1EE3B: AD 21 04 */  lda ObjectFlags+1
/* 1EE3E: 29 40 */     and #$40
/* 1EE40: 08 */        php
/* 1EE41: 19 99 EE */   ora EE98_table+1,y
/* 1EE44: 9D 20 04 */   sta ObjectFlags,x
/* 1EE47: B9 9A EE */   lda EE98_table+2,y
/* 1EE4A: 9D 40 04 */   sta ObjectUnknown440,x
/* 1EE4D: B9 9B EE */   lda EE98_table+3,y
/* 1EE50: 9D 60 06 */   sta ObjectYSpeedFraction,x
/* 1EE53: B9 9C EE */   lda EE98_table+4,y
/* 1EE56: 9D 80 06 */   sta ObjectYSpeed,x
/* 1EE59: 18 */         clc
/* 1EE5A: AD 01 06 */   lda ObjectPosY+1
/* 1EE5D: 79 9D EE */   adc EE98_table+5,y
/* 1EE60: 9D 00 06 */   sta ObjectPosY,x
/* 1EE63: AD 81 04 */   lda ObjectPosX+1
/* 1EE66: 28 */        plp
/* 1EE67: F0 0F */     beq L1EE78 ; +
/* 1EE69: 18 */        clc
/* 1EE6A: 79 9E EE */  adc EE98_table+6,y
/* 1EE6D: 9D 80 04 */  sta ObjectPosX,x
/* 1EE70: AD 61 04 */  lda ObjectPosScreen+1
/* 1EE73: 69 00 */     adc #$00
/* 1EE75: 4C 84 EE */  jmp L1EE84 ; ++
L1EE78: ; +
/* 1EE78: 38 */        sec
/* 1EE79: F9 9E EE */  sbc EE98_table+6,y
/* 1EE7C: 9D 80 04 */  sta ObjectPosX,x
/* 1EE7F: AD 61 04 */  lda ObjectPosScreen+1
/* 1EE82: E9 00 */     sbc #$00
L1EE84: ; ++
/* 1EE84: 9D 60 04 */  sta ObjectPosScreen,x
/* 1EE87: 20 3D C5 */  jsr C53D_routine
/* 1EE8A: A5 0D */     lda $0D
/* 1EE8C: C9 03 */     cmp #$03
/* 1EE8E: D0 06 */     bne L1EE96
/* 1EE90: AD 61 04 */  lda ObjectPosScreen+1
/* 1EE93: 9D 60 04 */  sta ObjectPosScreen,x
L1EE96:
/* 1EE96: 18 */        clc
L1EE97:
/* 1EE97: 60 */        rts

EE98_table:
    ; Values for
    ;   SpriteNum
    ;   Flags
    ;   Unknown440
    ;   YSpeedFraction
    ;   YSpeed
    ;   YPos increment
    .byte $27,$13,$01,$B0,$05,$F5
    .byte $08,$36,$0A,$01,$00,$00
    .byte $F8,$08,$32,$03,$00,$00
    .byte $00,$E8,$10,$32,$18,$14
    .byte $40,$FF,$00,$F6,$38,$0A
    .byte $01,$00,$00,$04,$00,$3A
    .byte $02,$00,$00,$00,$FE,$08
    .byte $45,$02,$00,$00,$00,$F6
    .byte $08,$4C,$0A,$40,$00,$00
    .byte $FC,$14,$4D,$0A,$30,$00
    .byte $00,$08,$18,$4E,$0A,$20
    .byte $00,$00,$FC,$1C,$4F,$0A
    .byte $10,$00,$00,$EC,$24,$50
    .byte $0A,$01,$00,$00,$EC,$2C
    .byte $27,$18,$01,$30,$03,$EC
    .byte $06,$1A,$02,$00,$00,$00
    .byte $00,$10,$77,$02,$00,$00
    .byte $06,$F0,$10,$78,$02,$00
    .byte $00,$FA,$10,$10,$86,$12
    .byte $00,$E3,$04,$F8,$08,$FF
    .byte $20,$00,$00,$00,$04,$08
	
BossFuncEF16:
/* 1EF16: CD 01 04 */  cmp ObjectSpriteNum+1
/* 1EF19: F0 17 */     beq L1EF32 ; +++
/* 1EF1B: 8D 01 04 */  sta ObjectSpriteNum+1
/* 1EF1E: 20 36 EF */  jsr F1EF36 ; ++++
/* 1EF21: A5 AC */     lda FightingBossNum
/* 1EF23: F0 09 */     beq L1EF2E ; +
/* 1EF25: A9 00 */     lda #$00
/* 1EF27: 8D 41 06 */  sta ObjectFireDelay+1
/* 1EF2A: A9 05 */     lda #$05
/* 1EF2C: D0 02 */     bne L1EF30 ; ++
L1EF2E: ; +
/* 1EF2E: A9 0E */     lda #$0E
L1EF30: ; ++
/* 1EF30: 85 3E */     sta BossCurrentStrategy
L1EF32: ; +++
/* 1EF32: 20 F8 ED */  jsr BossFuncWalkTowardsMegaman
/* 1EF35: 60 */        rts
F1EF36: ; ++++
/* 1EF36: 20 C2 ED */  jsr BossFuncEDC2
/* 1EF39: A5 43 */     lda BossVariable43
/* 1EF3B: 49 40 */     eor #$40
/* 1EF3D: 85 43 */     sta BossVariable43
/* 1EF3F: A9 40 */     lda #$40
/* 1EF41: 8D 61 06 */  sta ObjectYSpeedFraction+1
/* 1EF44: A9 01 */     lda #$01
/* 1EF46: 8D 81 06 */  sta ObjectYSpeed+1
/* 1EF49: 8D C1 04 */  sta ObjectXSpeed+1
/* 1EF4C: A9 20 */     lda #$20
/* 1EF4E: 8D E1 04 */  sta ObjectXSpeedFraction+1
/* 1EF51: A9 00 */     lda #$00
/* 1EF53: 8D 41 04 */  sta ObjectUnknown440+1
/* 1EF56: 60 */        rts






Boss7Init:
; Boss 7 init.
;   Wait until Megaman is standing.
;   Then flash the palette until palette delay is over.
;   Then move him to the right until his X position equals #$50.

/* 1EF57: AD 00 04 */  lda ObjectSpriteNum+0                                ; ObjectSpriteNum+0
/* 1EF5A: F0 01 */     beq L1EF5D ; +                                           ; $EF5D
; No delay if Megaman isn't standing
/* 1EF5C: 60 */        rts
; +
; Pause gameplay (not sound!) until PaletteUpdateDelay = 0
L1EF5D: ; -
/* 1EF5D: A5 37 */     lda PaletteUpdateDelay
/* 1EF5F: F0 06 */     beq L1EF67 ; +                                           ; $EF67
/* 1EF61: 20 1B C0 */  jsr NextFrame                                   ; $C01B
/* 1EF64: 4C 5D EF */  jmp L1EF5D ; -                                           ; $EF5D
; +
; Make Megaman turn right
L1EF67: ; -
/* 1EF67: A9 40 */     lda #$40
/* 1EF69: 8D 20 04 */  sta ObjectFlags+0

/* 1EF6C: 18 */        clc
/* 1EF6D: AD 80 04 */  lda ObjectPosX+0  ;Move Megaman to the right
/* 1EF70: 69 01 */     adc #$01
L1EF72:
/* 1EF72: 8D 80 04 */  sta ObjectPosX+0
/* 1EF75: C9 50 */     cmp #$50         ;Until Xpos = #$50
/* 1EF77: F0 0B */     beq L1EF84 ; +                                           ; $0001EF84
/* 1EF79: 20 31 D1 */  jsr UpdateGraphics                              ; $D131
/* 1EF7C: 20 1B C0 */  jsr NextFrame                                   ; $C01B
/* 1EF7F: 4C 67 EF */  jmp L1EF67 ; -                                           ; $EF67

EF82_table: .byte $50, $B0
L1EF84: ; +
/* 1EF84: A9 80 */     lda #$80
/* 1EF86: 20 09 D1 */  jsr TimeDelayWithSpriteUpdates                  ; $D109

/* 1EF89: A2 00 */     ldx #$00
/* 1EF8B: 86 2F */     stx RefObjectNum
/* 1EF8D: E8 */        inx
/* 1EF8E: 86 0C */     stx $0C

/* 1EF90: A2 10 */     ldx #$10
L1EF92: ; -
/* 1EF92: A9 00 */     lda #$00
/* 1EF94: 20 7B F6 */  jsr InitActor
/* 1EF97: A0 08 */     ldy #$08
/* 1EF99: 20 33 F5 */  jsr InitObjectDefaultSpeed

/* 1EF9C: A9 00 */     lda #$00
/* 1EF9E: 9D 20 04 */  sta ObjectFlags,x
/* 1EFA1: A4 0C */     ldy $0C
/* 1EFA3: B9 82 EF */  lda EF82_table,y
/* 1EFA6: 9D 80 04 */  sta ObjectPosX,x
/* 1EFA9: A9 79 */     lda #$79
/* 1EFAB: 9D 00 04 */  sta ObjectSpriteNum,x

/* 1EFAE: A9 08 */     lda #$08
/* 1EFB0: 9D 00 06 */  sta ObjectPosY,x
/* 1EFB3: E8 */        inx
/* 1EFB4: C6 0C */     dec $0C
/* 1EFB6: 10 DA */     bpl L1EF92 ; -                                           ; $EF92

; Move objects 16 and 17 down until y coordinate of 16 = #$98
L1EFB8: ; --
/* 1EFB8: A2 10 */     ldx #$10
L1EFBA: ; -
/* 1EFBA: 18 */        clc
/* 1EFBB: BD 00 06 */  lda ObjectPosY,x
/* 1EFBE: 69 01 */     adc #$01
/* 1EFC0: 9D 00 06 */  sta ObjectPosY,x
/* 1EFC3: E8 */        inx
/* 1EFC4: E0 12 */     cpx #$12
/* 1EFC6: D0 F2 */     bne L1EFBA ; -                                           ; $EFBA

/* 1EFC8: 20 31 D1 */  jsr UpdateGraphics                              ; $D131
/* 1EFCB: 20 1B C0 */  jsr NextFrame                                   ; $C01B

/* 1EFCE: AD 10 06 */  lda $0610
/* 1EFD1: C9 98 */     cmp #$98
/* 1EFD3: D0 E3 */     bne L1EFB8 ; --                                          ; $EFB8



/* 1EFD5: A9 30 */     lda #$30
/* 1EFD7: 20 09 D1 */  jsr TimeDelayWithSpriteUpdates                  ; $D109

/* 1EFDA: A9 00 */     lda #$00
/* 1EFDC: 85 2F */     sta RefObjectNum
/* 1EFDE: A9 01 */     lda #$01
/* 1EFE0: 85 0C */     sta $0C
/* 1EFE2: A2 12 */     ldx #$12
L1EFE4:
/* 1EFE4: A9 00 */     lda #$00
/* 1EFE6: 20 7B F6 */  jsr InitActor
/* 1EFE9: A9 77 */     lda #$77
/* 1EFEB: 9D 00 04 */  sta ObjectSpriteNum,x
/* 1EFEE: A4 0C */     ldy $0C
/* 1EFF0: B9 82 EF */  lda EF82_table,y
/* 1EFF3: 9D 80 04 */  sta ObjectPosX,x
/* 1EFF6: A9 B0 */     lda #$B0
/* 1EFF8: 9D 00 06 */  sta ObjectPosY,x
/* 1EFFB: A0 08 */     ldy #$08
/* 1EFFD: 20 33 F5 */  jsr InitObjectDefaultSpeed
/* 1F000: E8 */        inx
/* 1F001: C6 0C */     dec $0C
/* 1F003: 10 DF */     bpl L1EFE4
/* 1F005: A9 00 */     lda #$00
/* 1F007: 8D 54 06 */  sta $0654
/* 1F00A: A9 80 */     lda #$80
/* 1F00C: 20 6C F0 */  jsr TimeDelay_WithPaletteRotation
/* 1F00F: 20 C4 DB */  jsr F1DBC4
/* 1F012: A9 80 */     lda #$80
/* 1F014: 20 6C F0 */  jsr TimeDelay_WithPaletteRotation
/* 1F017: A9 F8 */     lda #$F8
/* 1F019: 8D 12 06 */  sta $0612
/* 1F01C: 8D 13 06 */  sta $0613
/* 1F01F: 20 80 F0 */  jsr MaximizeObjectLifeMeter                       ; $F080

L1F022: ; --
/* 1F022: A2 10 */     ldx #$10
L1F024: ; -
/* 1F024: 38 */        sec
/* 1F025: BD 00 06 */  lda ObjectPosY,x
/* 1F028: E9 01 */     sbc #$01
/* 1F02A: 9D 00 06 */  sta ObjectPosY,x
/* 1F02D: E8 */        inx
/* 1F02E: E0 12 */     cpx #$12
/* 1F030: D0 F2 */     bne L1F024 ; -                                           ; $F024

/* 1F032: 20 31 D1 */  jsr UpdateGraphics                              ; $D131
/* 1F035: 20 1B C0 */  jsr NextFrame                                   ; $C01B
/* 1F038: AD 10 06 */  lda $0610
/* 1F03B: C9 08 */     cmp #$08
/* 1F03D: D0 E3 */     bne L1F022 ; --                                          ; $F022


/* 1F03F: A2 02 */     ldx #$02

L1F041:
/* 1F041: A9 F8 */     lda #$F8
/* 1F043: 9D 00 06 */  sta ObjectPosY,x
/* 1F046: E8 */        inx
/* 1F047: E0 20 */     cpx #$20
/* 1F049: D0 F6 */     bne L1F041
/* 1F04B: A2 02 */     ldx #$02
L1F04D:
/* 1F04D: BD 69 F0 */  lda F069_table,x
/* 1F050: 9D ED 03 */  sta SpritePalettes + $D,x
/* 1F053: CA */        dex
/* 1F054: 10 F7 */     bpl L1F04D
/* 1F056: 20 2A C7 */  jsr PaletteSetupForSprites
/* 1F059: A9 10 */     lda #$10
/* 1F05B: 20 09 D1 */  jsr TimeDelayWithSpriteUpdates                  ; $D109


/* 1F05E: A9 00 */     lda #$00        ; Reset joypad data
/* 1F060: 85 14 */     sta JoyPad0
/* 1F062: 85 16 */     sta JoyPad0old
/* 1F064: A9 05 */     lda #$05
/* 1F066: 85 3E */     sta BossCurrentStrategy
/* 1F068: 60 */        rts


F069_table: ; colours of some purpose
    .byte $30,$00,$0F


TimeDelay_WithPaletteRotation:
/* 1F06C: 85 3C */     sta MiscCounter1
L1F06E: ; -
/* 1F06E: A2 14 */     ldx #$14
/* 1F070: 86 2F */     stx RefObjectNum
/* 1F072: 20 BD F3 */  jsr AI_Object32
/* 1F075: 20 31 D1 */  jsr UpdateGraphics                              ; $D131
/* 1F078: 20 1B C0 */  jsr NextFrame                                   ; $C01B
/* 1F07B: C6 3C */     dec MiscCounter1
/* 1F07D: D0 EF */     bne L1F06E ; -                                           ; $F06E
/* 1F07F: 60 */        rts

; Grow boss's life meter to max
MaximizeObjectLifeMeter:
L1F080: ; -
/* 1F080: AD C1 06 */  lda ObjectLifeMeter+1
/* 1F083: C9 1C */     cmp #$1C
/* 1F085: F0 0C */     beq L1F093 ; +                                           ; $F093
/* 1F087: 20 9B F3 */  jsr IncObjectLifeMeter                            ; $F39B
/* 1F08A: 20 31 D1 */  jsr UpdateGraphics                              ; $D131
/* 1F08D: 20 1B C0 */  jsr NextFrame                                   ; $C01B
/* 1F090: 4C 80 F0 */  jmp L1F080 ; -                                           ; $F080
L1F093: ; +
/* 1F093: 60 */        rts


Boss6Init:
; Pause until PaletteUpdateDelay = 0
L1F094: ; -
/* 1F094: A5 37 */     lda PaletteUpdateDelay
/* 1F096: F0 06 */     beq L1F09E ; +                                           ; $F09E
/* 1F098: 20 1B C0 */  jsr NextFrame                                   ; $C01B
/* 1F09B: 4C 94 F0 */  jmp L1F094 ; -                                           ; $F094
L1F09E: ; +
/* 1F09E: A9 03 */     lda #$03
/* 1F0A0: 85 5C */     sta $5C

/* 1F0A2: A9 B0 */     lda #>LxB000
/* 1F0A4: A2 18 */     ldx #$18
/* 1F0A6: A0 40 */     ldy #$40
/* 1F0A8: 20 BE F0 */  jsr DoPPUtransferRoutineF0BE
/* 1F0AB: 20 80 F0 */  jsr MaximizeObjectLifeMeter                       ; $F080

/* 1F0AE: A9 05 */     lda #$05
/* 1F0B0: 85 3E */     sta BossCurrentStrategy
/* 1F0B2: A9 40 */     lda #$40
/* 1F0B4: 8D 21 04 */  sta ObjectFlags+1
/* 1F0B7: 85 3F */     sta BossVariable3F

/* 1F0B9: A9 00 */     lda #$00
/* 1F0BB: 85 14 */     sta JoyPad0
/* 1F0BD: 60 */        rts



DoPPUtransferRoutineF0BE:
; Input:
;   A = hi byte of ROM address (B7 = B700)
;   X = lo byte of VRAM address
;   Y = number of tiles
;
/* 1F0BE: 84 5B */     sty $5B
/* 1F0C0: 85 5A */     sta $5A
/* 1F0C2: 8E 80 03 */  stx RawPPUtransferAddress+0
/* 1F0C5: A2 00 */     ldx #$00
L1F0C7: ; --
/* 1F0C7: 86 59 */     stx $59
/* 1F0C9: 8E 81 03 */  stx RawPPUtransferAddress+1
/* 1F0CC: A6 5C */     ldx $5C
/* 1F0CE: 8A */        txa
/* 1F0CF: 85 42 */     sta CurrentBank
/* 1F0D1: 9D 00 C0 */  sta BankTable,x

/* 1F0D4: A0 20 */     ldy #$20
/* 1F0D6: 84 5E */     sty RawPPUtransferSize

L1F0D8: ; -
/* 1F0D8: B1 59 */     lda ($59),y
/* 1F0DA: 99 82 03 */  sta RawPPUtransferBuf,y
/* 1F0DD: 88 */        dey
/* 1F0DE: 10 F8 */     bpl L1F0D8 ; -                                           ; $F0D8

/* 1F0E0: A9 05 */     lda #$05
/* 1F0E2: 85 42 */     sta CurrentBank
/* 1F0E4: 8D 05 C0 */  sta BankTable+5
/* 1F0E7: 20 1B C0 */  jsr NextFrame                                   ; $C01B

/* 1F0EA: 18 */        clc
/* 1F0EB: A5 59 */     lda $59
/* 1F0ED: 69 20 */     adc #$20
/* 1F0EF: AA */        tax
/* 1F0F0: 08 */        php
/* 1F0F1: A5 5A */      lda $5A
/* 1F0F3: 69 00 */      adc #$00
/* 1F0F5: 85 5A */      sta $5A
/* 1F0F7: 28 */        plp
/* 1F0F8: AD 80 03 */  lda RawPPUtransferAddress+0
/* 1F0FB: 69 00 */     adc #$00
/* 1F0FD: 8D 80 03 */  sta RawPPUtransferAddress+0
/* 1F100: C6 5B */     dec $5B
/* 1F102: D0 C3 */     bne L1F0C7 ; --                                          ; $F0C7
/* 1F104: 60 */        rts




Boss8and9init:
L1F105: ; -
/* 1F105: A5 37 */     lda PaletteUpdateDelay
/* 1F107: F0 09 */     beq L1F112 ; +                                           ; $F112
/* 1F109: 20 31 D1 */  jsr UpdateGraphics                              ; $D131
/* 1F10C: 20 1B C0 */  jsr NextFrame                                   ; $C01B
/* 1F10F: 4C 05 F1 */  jmp L1F105 ; -                                           ; $F105
L1F112: ; +
/* 1F112: A5 31 */     lda CurrentStage
/* 1F114: C9 09 */     cmp #$09
/* 1F116: F0 10 */     beq L1F128 ; +                                           ; $F128
L1F118:
/* 1F118: 20 80 F0 */  jsr MaximizeObjectLifeMeter                       ; $F080
/* 1F11B: A9 3F */     lda #$3F
/* 1F11D: 85 44 */     sta BossVariable44
/* 1F11F: A9 05 */     lda #$05
/* 1F121: 85 3E */     sta BossCurrentStrategy

/* 1F123: A9 00 */     lda #$00
/* 1F125: 85 14 */     sta JoyPad0
/* 1F127: 60 */        rts
L1F128: ; +
/* 1F128: A9 03 */     lda #$03
/* 1F12A: 85 5C */     sta $5C
/* 1F12C: A9 B8 */     lda #>LxB800
/* 1F12E: A2 19 */     ldx #$19
/* 1F130: A0 38 */     ldy #$38
/* 1F132: 20 BE F0 */  jsr DoPPUtransferRoutineF0BE
/* 1F135: C6 5C */     dec $5C
/* 1F137: A9 BB */     lda #>LxBB00
/* 1F139: A2 12 */     ldx #$12
/* 1F13B: A0 10 */     ldy #$10
/* 1F13D: 20 BE F0 */  jsr DoPPUtransferRoutineF0BE
/* 1F140: A9 05 */     lda #$05
/* 1F142: 8D 05 C0 */  sta BankTable+5
/* 1F145: 85 42 */     sta CurrentBank
/* 1F147: A2 07 */     ldx #$07
/* 1F149: A9 0F */     lda #$0F
L1F14B:
/* 1F14B: 9D D4 03 */  sta BGPalettes + $4,x
/* 1F14E: CA */        dex
/* 1F14F: 10 FA */     bpl L1F14B
/* 1F151: 20 3F C7 */  jsr PaletteSetupForBG
/* 1F154: A9 0C */     lda #$0C
/* 1F156: 85 59 */     sta $59
/* 1F158: A2 00 */     ldx #$00
L1F15A:
/* 1F15A: BC A1 8E */  ldy RoomActives2+1,x
/* 1F15D: 84 5E */     sty RawPPUtransferSize
/* 1F15F: C8 */        iny
/* 1F160: C8 */        iny
/* 1F161: 84 0C */     sty $0C
/* 1F163: E8 */        inx
/* 1F164: A0 00 */     ldy #$00
L1F166:
/* 1F166: BD A1 8E */  lda RoomActives2+1,x
/* 1F169: 99 80 03 */  sta RawPPUtransferAddress+0,y
/* 1F16C: E8 */        inx
/* 1F16D: C8 */        iny
/* 1F16E: C4 0C */     cpy $0C
/* 1F170: D0 F4 */     bne L1F166
/* 1F172: 86 5A */     stx $5A
/* 1F174: 20 31 D1 */  jsr UpdateGraphics                              ; $D131
/* 1F177: 20 1B C0 */  jsr NextFrame                                   ; $C01B
/* 1F17A: A6 5A */     ldx $5A
/* 1F17C: C6 59 */     dec $59
/* 1F17E: D0 DA */     bne L1F15A
/* 1F180: A9 00 */     lda #$00
/* 1F182: 85 2F */     sta RefObjectNum
/* 1F184: A2 10 */     ldx #$10
L1F186:
/* 1F186: 20 7B F6 */  jsr InitActor
/* 1F189: 8A */        txa
/* 1F18A: 29 0F */     and #$0F
/* 1F18C: A8 */        tay
/* 1F18D: B9 5E F2 */  lda F25E_table,y
/* 1F190: 9D 00 04 */  sta ObjectSpriteNum,x
/* 1F193: A9 08 */     lda #$08
/* 1F195: 9D 80 04 */  sta ObjectPosX,x
/* 1F198: B9 60 F2 */  lda F260_table,y
/* 1F19B: 9D 00 06 */  sta ObjectPosY,x
/* 1F19E: A0 94 */     ldy #$94
/* 1F1A0: 20 33 F5 */  jsr InitObjectDefaultSpeed
/* 1F1A3: E8 */        inx
/* 1F1A4: E0 12 */     cpx #$12
/* 1F1A6: D0 DE */     bne L1F186

L1F1A8: ; -
/* 1F1A8: 20 EA 98 */  jsr RunEnemyAI                                   ; $98EA
/* 1F1AB: AD 90 04 */  lda $0490
/* 1F1AE: C9 AB */     cmp #$AB
/* 1F1B0: B0 09 */     bcs L1F1BB ; +                                           ; $F1BB
/* 1F1B2: 20 31 D1 */  jsr UpdateGraphics                              ; $D131
/* 1F1B5: 20 1B C0 */  jsr NextFrame                                   ; $C01B
/* 1F1B8: 4C A8 F1 */  jmp L1F1A8 ; -                                           ; $F1A8
L1F1BB: ; +
/* 1F1BB: A2 10 */     ldx #$10
L1F1BD:
/* 1F1BD: A9 AC */     lda #$AC
/* 1F1BF: 9D 80 04 */  sta ObjectPosX,x
/* 1F1C2: A9 02 */     lda #$02
/* 1F1C4: 9D 20 04 */  sta ObjectFlags,x
/* 1F1C7: A0 08 */     ldy #$08
/* 1F1C9: 20 33 F5 */  jsr InitObjectDefaultSpeed
/* 1F1CC: E8 */        inx
/* 1F1CD: E0 12 */     cpx #$12
/* 1F1CF: D0 EC */     bne L1F1BD
/* 1F1D1: A0 00 */     ldy #$00
/* 1F1D3: 85 2F */     sta RefObjectNum
/* 1F1D5: A2 12 */     ldx #$12
L1F1D7:
/* 1F1D7: 84 0C */     sty $0C
/* 1F1D9: 20 7B F6 */  jsr InitActor
/* 1F1DC: A0 08 */     ldy #$08
/* 1F1DE: 20 33 F5 */  jsr InitObjectDefaultSpeed
/* 1F1E1: A4 0C */     ldy $0C
/* 1F1E3: B9 82 F2 */  lda F282_table,y
/* 1F1E6: 9D 00 04 */  sta ObjectSpriteNum,x
/* 1F1E9: B9 86 F2 */  lda F286_table,y
/* 1F1EC: 9D 80 04 */  sta ObjectPosX,x
/* 1F1EF: B9 8A F2 */  lda F28A_table,y
/* 1F1F2: 9D 00 06 */  sta ObjectPosY,x
/* 1F1F5: A9 02 */     lda #$02
/* 1F1F7: 9D 20 04 */  sta ObjectFlags,x
/* 1F1FA: C8 */        iny
/* 1F1FB: E8 */        inx
/* 1F1FC: E0 16 */     cpx #$16
/* 1F1FE: D0 D7 */     bne L1F1D7
/* 1F200: EE B1 06 */  inc $06B1

/* 1F203: A9 7F */     lda #$7F
/* 1F205: 85 3C */     sta MiscCounter1
L1F207: ; --
/* 1F207: A5 3C */     lda MiscCounter1
/* 1F209: 29 1F */     and #$1F
/* 1F20B: D0 3F */     bne L1F24C ; +                                           ; $F24C
/* 1F20D: A5 3C */     lda MiscCounter1
/* 1F20F: 4A */        lsr a
/* 1F210: 4A */        lsr a
/* 1F211: AA */        tax
/* 1F212: A0 00 */     ldy #$00
L1F214: ; -
/* 1F214: BD 62 F2 */  lda F262_table,x
/* 1F217: 99 D4 03 */  sta BGPalettes + $4,y
/* 1F21A: C8 */        iny
/* 1F21B: E8 */        inx
/* 1F21C: C0 08 */     cpy #$08
/* 1F21E: D0 F4 */     bne L1F214 ; -                                           ; $F214
/* 1F220: 20 3F C7 */  jsr PaletteSetupForBG
/* 1F223: A5 3C */     lda MiscCounter1

/* 1F225: C9 60 */     cmp #$60
/* 1F227: D0 23 */     bne L1F24C ; +                                           ; $F24C
/* 1F229: A2 01 */     ldx #$01
/* 1F22B: 20 7B F6 */  jsr InitActor
/* 1F22E: A9 7E */     lda #$7E
/* 1F230: 8D 01 04 */  sta ObjectSpriteNum+1
/* 1F233: A9 00 */     lda #$00
/* 1F235: 8D C1 06 */  sta ObjectLifeMeter+1
/* 1F238: A9 06 */     lda #$06
/* 1F23A: 8D 21 04 */  sta ObjectFlags+1
/* 1F23D: A9 84 */     lda #$84
/* 1F23F: 8D 81 04 */  sta ObjectPosX+1
/* 1F242: A9 87 */     lda #$87
/* 1F244: 8D 01 06 */  sta ObjectPosY+1
/* 1F247: A0 08 */     ldy #$08
/* 1F249: 20 33 F5 */  jsr InitObjectDefaultSpeed
L1F24C: ; +
/* 1F24C: 20 31 D1 */  jsr UpdateGraphics                              ; $D131
/* 1F24F: 20 1B C0 */  jsr NextFrame                                   ; $C01B
/* 1F252: C6 3C */     dec MiscCounter1
/* 1F254: 10 B1 */     bpl L1F207 ; --                                          ; $F207

/* 1F256: A9 F8 */     lda #$F8
/* 1F258: 8D 11 06 */  sta $0611
/* 1F25B: 4C 18 F1 */  jmp L1F118

F25E_table:
    .byte $74,$75 ;ObjectSpriteNum
F260_table:
    .byte $46,$55 ;ObjectPosY
F262_table:       ; Colours of some kind.
    .byte $0F,$16,$26,$0F,$0F,$07,$37,$17
    .byte $0F,$06,$16,$0F,$0F,$07,$27,$07
    .byte $0F,$0F,$06,$0F,$0F,$0F,$17,$0F
    .byte $0F,$0F,$0F,$0F,$0F,$0F,$07,$0F

F282_table:
    .byte $80,$81,$82,$7F ;states
F286_table:
    .byte $AC,$B4,$C4,$D8 ;Xpositions
F28A_table:
    .byte $7B,$63,$93,$77 ;Ypositions

L1F28E:
/* 1F28E: 20 80 F0 */  jsr MaximizeObjectLifeMeter                       ; $F080
/* 1F291: A9 07 */     lda #$07
/* 1F293: 85 59 */     sta $59
/* 1F295: A2 00 */     ldx #$00

L1F297: ; --
/* 1F297: BC 48 F3 */  ldy F348_table,x
/* 1F29A: 84 5E */     sty RawPPUtransferSize
/* 1F29C: C8 */        iny
/* 1F29D: C8 */        iny
/* 1F29E: 84 0C */     sty $0C
/* 1F2A0: E8 */        inx
/* 1F2A1: A0 00 */     ldy #$00
L1F2A3: ; -
/* 1F2A3: BD 48 F3 */  lda F348_table,x
/* 1F2A6: 99 80 03 */  sta RawPPUtransferAddress+0,y
/* 1F2A9: E8 */        inx
/* 1F2AA: C8 */        iny
/* 1F2AB: C6 0C */     dec $0C
/* 1F2AD: D0 F4 */     bne L1F2A3 ; -                                           ; $F2A3



/* 1F2AF: 86 5A */     stx $5A


/* 1F2B1: A2 16 */     ldx #$16
/* 1F2B3: 20 76 C5 */  jsr FindFreeObject                              ; FindFreeObject
/* 1F2B6: B0 2E */     bcs L1F2E6 ; +                                           ; $F2E6

/* 1F2B8: A9 01 */     lda #$01
/* 1F2BA: 85 2F */     sta RefObjectNum
/* 1F2BC: 20 7B F6 */  jsr InitActor
/* 1F2BF: A9 84 */     lda #$84
/* 1F2C1: 9D 00 04 */  sta ObjectSpriteNum,x
/* 1F2C4: A4 59 */     ldy $59
/* 1F2C6: 18 */        clc
/* 1F2C7: BD 80 04 */  lda ObjectPosX,x
/* 1F2CA: 79 83 F3 */  adc F383_table,y
/* 1F2CD: 9D 80 04 */  sta ObjectPosX,x
/* 1F2D0: 38 */        sec
/* 1F2D1: BD 00 06 */  lda ObjectPosY,x
/* 1F2D4: F9 8A F3 */  sbc F38A_table,y
/* 1F2D7: 9D 00 06 */  sta ObjectPosY,x
/* 1F2DA: B9 91 F3 */  lda F391_table,y
/* 1F2DD: A8 */        tay
/* 1F2DE: 20 33 F5 */  jsr InitObjectDefaultSpeed
/* 1F2E1: A9 50 */     lda #$50
/* 1F2E3: 9D 20 04 */  sta ObjectFlags,x
L1F2E6: ; +
/* 1F2E6: 20 EA 98 */  jsr RunEnemyAI                                   ; $98EA
/* 1F2E9: 20 31 D1 */  jsr UpdateGraphics                              ; $D131
/* 1F2EC: 20 1B C0 */  jsr NextFrame                                   ; $C01B

/* 1F2EF: A6 5A */     ldx $5A
/* 1F2F1: C6 59 */     dec $59
/* 1F2F3: D0 A2 */     bne L1F297 ; --                                          ; $F297

/* 1F2F5: A2 02 */     ldx #$02
L1F2F7: ; -
/* 1F2F7: BD 98 F3 */  lda F398_table,x
/* 1F2FA: 9D D5 03 */  sta BGPalettes + $5,x
/* 1F2FD: CA */        dex
/* 1F2FE: 10 F7 */     bpl L1F2F7 ; -                                           ; $F2F7


/* 1F300: 20 3F C7 */  jsr PaletteSetupForBG
/* 1F303: E6 AC */     inc FightingBossNum
/* 1F305: 18 */        clc
/* 1F306: AD 81 04 */  lda ObjectPosX+1
/* 1F309: 69 04 */     adc #$04
/* 1F30B: 8D 81 04 */  sta ObjectPosX+1
/* 1F30E: 69 1C */     adc #$1C
/* 1F310: 8D 91 04 */  sta $0491
/* 1F313: 38 */        sec
/* 1F314: AD 01 06 */  lda ObjectPosY+1
/* 1F317: E9 10 */     sbc #$10
/* 1F319: 8D 01 06 */  sta ObjectPosY+1
/* 1F31C: E9 04 */     sbc #$04
/* 1F31E: 8D 11 06 */  sta $0611
/* 1F321: A9 85 */     lda #$85
/* 1F323: 8D 01 04 */  sta ObjectSpriteNum+1
/* 1F326: A9 00 */     lda #$00
/* 1F328: 8D 31 04 */  sta $0431
/* 1F32B: 8D 51 04 */  sta $0451
/* 1F32E: A9 83 */     lda #$83
/* 1F330: 8D 11 04 */  sta $0411
/* 1F333: A2 11 */     ldx #$11
/* 1F335: A0 08 */     ldy #$08
/* 1F337: 20 33 F5 */  jsr InitObjectDefaultSpeed
/* 1F33A: A9 80 */     lda #$80
/* 1F33C: 85 44 */     sta BossVariable44
/* 1F33E: A9 05 */     lda #$05
/* 1F340: 85 3E */     sta BossCurrentStrategy
/* 1F342: A2 FF */     ldx #$FF
/* 1F344: 9A */        txs
/* 1F345: 4C 5E 91 */  jmp L1515E


F348_table:
    ; first byte: number of tiles
    ; second byte: probably X coordinate
    ; consequent bytes: data
    .byte $05,$25,  $71, $00, $00, $E0, $E1, $E2
    .byte $06,$25,  $90, $00, $00, $00, $E3, $00, $E4
    .byte $07,$25,  $AF, $00, $00, $E5, $E6, $00, $00, $00
    .byte $07,$25,  $CF, $00, $E7, $E8, $E9, $00, $00, $00
    .byte $07,$25,  $EF, $EA, $EB, $EC, $ED, $EE, $EF, $DA
    .byte $03,$26,            $0F, $DB, $DC, $DD
    .byte $03,$26,            $2F, $DE, $DF, $F6

F383_table: ; alters PosX
    .byte $00, $20, $10, $00, $20, $14, $08
F38A_table: ;alters PosY
    .byte $00, $14, $14, $14, $24, $24, $24
F391_table:
    .byte $D4, $78, $D8, $D4, $28, $D4, $34 ;params for InitObjectDefaultSpeed
F398_table: ; colours for some purpose
    .byte $07, $37, $17


IncObjectLifeMeter:
/* 1F39B: A5 23 */     lda FrameCounter    ; Only inc meter every second frame
/* 1F39D: 29 01 */     and #$01
/* 1F39F: D0 15 */     bne L1F3B6 ; +                                           ; $F3B6

/* 1F3A1: AD C1 06 */  lda ObjectLifeMeter+1
/* 1F3A4: C9 1C */     cmp #$1C
/* 1F3A6: F0 0F */     beq L1F3B7 ; ++                                          ; $F3B7
/* 1F3A8: EE C1 06 */  inc ObjectLifeMeter+1

/* 1F3AB: A5 23 */     lda FrameCounter    ; Only play sound every 8th frame
/* 1F3AD: 29 07 */     and #$07
/* 1F3AF: D0 05 */     bne L1F3B6 ; +                                           ; $F3B6

/* 1F3B1: A9 18 */     lda #$18        ; Weapons/Energy bar?
/* 1F3B3: 20 77 C4 */  jsr IssueSound                                  ; $C477
L1F3B6: ; +
/* 1F3B6: 60 */        rts

L1F3B7: ; ++
/* 1F3B7: A9 FE */     lda #$FE
/* 1F3B9: 20 77 C4 */  jsr IssueSound                                  ; $C477
/* 1F3BC: 60 */        rts



AI_Object32:
/* 1F3BD: A5 23 */     lda FrameCounter
/* 1F3BF: 29 07 */     and #$07
/* 1F3C1: D0 2E */     bne L1F3F1 ; +++                                         ; $F3F1
/* 1F3C3: DE 40 06 */  dec ObjectFireDelay,x
/* 1F3C6: BD 40 06 */  lda ObjectFireDelay,x
/* 1F3C9: C9 03 */     cmp #$03
/* 1F3CB: 90 02 */     bcc L1F3CF ; +                                           ; $F3CF
/* 1F3CD: A9 02 */     lda #$02
L1F3CF: ; +
/* 1F3CF: 9D 40 06 */  sta ObjectFireDelay,x
/* 1F3D2: A8 */        tay

; if we're not at Fireman's stage, then Y += 5
/* 1F3D3: A5 31 */     lda CurrentStage
/* 1F3D5: C9 03 */     cmp #$03
/* 1F3D7: F0 05 */     beq L1F3DE ; +                                           ; $F3DE
/* 1F3D9: 18 */        clc
/* 1F3DA: 98 */        tya
/* 1F3DB: 69 05 */     adc #$05
/* 1F3DD: A8 */        tay
L1F3DE: ; +
/* 1F3DE: A2 00 */     ldx #$00
L1F3E0: ; -
/* 1F3E0: B9 F2 F3 */  lda Object32_paletteTable,y
/* 1F3E3: 9D DD 03 */  sta BGPalettes + $D,x
/* 1F3E6: C8 */        iny
/* 1F3E7: E8 */        inx
/* 1F3E8: E0 03 */     cpx #$03
/* 1F3EA: D0 F4 */     bne L1F3E0 ; -                                           ; $F3E0

/* 1F3EC: A6 2F */     ldx RefObjectNum
/* 1F3EE: 20 3F C7 */  jsr PaletteSetupForBG
L1F3F1: ; +++
/* 1F3F1: 60 */        rts

Object32_paletteTable: ;colours for some purpose
    .byte $26, $16, $06, $26, $16 ;tones of Red
    .byte $21, $25, $2A, $21, $25 ;blue, red, green

AI_Object3A:
/* 1F3FC: BD 40 04 */  lda ObjectUnknown440,x
/* 1F3FF: 29 F0 */     and #$F0
/* 1F401: C9 30 */     cmp #$30
/* 1F403: D0 03 */     bne L1F408
/* 1F405: 9D 40 04 */  sta ObjectUnknown440,x
L1F408:
/* 1F408: BD A0 06 */  lda ObjectLifeCycleCounter,x
/* 1F40B: D0 3E */     bne L1F44B
/* 1F40D: DE 40 06 */  dec ObjectFireDelay,x
/* 1F410: BD 40 06 */  lda ObjectFireDelay,x
/* 1F413: C9 60 */     cmp #$60
/* 1F415: B0 34 */     bcs L1F44B
/* 1F417: 29 1F */     and #$1F
/* 1F419: D0 30 */     bne L1F44B
/* 1F41B: BD 20 04 */  lda ObjectFlags,x
/* 1F41E: 48 */        pha
/* 1F41F: 20 3B F6 */  jsr EnemySearchMegaman
/* 1F422: 85 0C */     sta $0C
/* 1F424: A9 00 */     lda #$00
/* 1F426: 20 63 F6 */  jsr CreateEnemy                                   ; $F663
/* 1F429: B0 10 */     bcs L1F43B
/* 1F42B: A9 7C */     lda #$7C
/* 1F42D: 9D 00 04 */  sta ObjectSpriteNum,x
/* 1F430: A9 02 */     lda #$02
/* 1F432: 85 01 */     sta $01
/* 1F434: A9 00 */     lda #$00
/* 1F436: 85 00 */     sta $00
/* 1F438: 20 28 F6 */  jsr F1F628
L1F43B:
/* 1F43B: A6 2F */     ldx RefObjectNum
/* 1F43D: 68 */        pla
/* 1F43E: 9D 20 04 */  sta ObjectFlags,x
/* 1F441: BD 40 06 */  lda ObjectFireDelay,x
/* 1F444: D0 05 */     bne L1F44B
/* 1F446: A9 DE */     lda #$DE
/* 1F448: 9D 40 06 */  sta ObjectFireDelay,x
L1F44B:
/* 1F44B: 20 49 BE */  jsr EnemyAI_MovementsAndDamageCheck
/* 1F44E: BD E0 06 */  lda ObjectType,x
/* 1F451: C9 1B */     cmp #$1B
/* 1F453: D0 03 */     bne L1F458
                    ;Go and decrease the bubblebot energy meter.
/* 1F455: 4C D8 F4 */  jmp L1F4D8
L1F458:
/* 1F458: BD 80 06 */  lda ObjectYSpeed,x
/* 1F45B: F0 24 */     beq L1F481
/* 1F45D: 30 0B */     bmi L1F46A
/* 1F45F: BD 00 06 */  lda ObjectPosY,x
/* 1F462: C9 40 */     cmp #$40
/* 1F464: B0 1A */     bcs L1F480
L1F466:
/* 1F466: A9 46 */     lda #$46
/* 1F468: D0 09 */     bne L1F473
L1F46A:
/* 1F46A: BD 00 06 */  lda ObjectPosY,x
/* 1F46D: C9 A0 */     cmp #$A0
/* 1F46F: 90 0F */     bcc L1F480
L1F471:
/* 1F471: A9 06 */     lda #$06
L1F473:
/* 1F473: 9D 20 04 */  sta ObjectFlags,x
/* 1F476: A4 44 */     ldy BossVariable44
/* 1F478: B9 F5 F4 */  lda F4F5_table,y
/* 1F47B: A8 */        tay
L1F47C:
/* 1F47C: 20 33 F5 */  jsr InitObjectDefaultSpeed
/* 1F47F: 18 */        clc
L1F480:
/* 1F480: 60 */        rts
L1F481:
/* 1F481: BD 20 04 */  lda ObjectFlags,x
/* 1F484: 29 40 */     and #$40
/* 1F486: D0 0F */     bne L1F497
/* 1F488: BD 80 04 */  lda ObjectPosX,x
/* 1F48B: C9 30 */     cmp #$30
/* 1F48D: B0 F1 */     bcs L1F480
/* 1F48F: A4 44 */     ldy BossVariable44
/* 1F491: B9 FC F4 */  lda F4FC_table,y
/* 1F494: A8 */        tay
/* 1F495: D0 E5 */     bne L1F47C
L1F497:
/* 1F497: BD 80 04 */  lda ObjectPosX,x
/* 1F49A: C9 D0 */     cmp #$D0
/* 1F49C: 90 E2 */     bcc L1F480
L1F49E:
/* 1F49E: A4 44 */     ldy BossVariable44
/* 1F4A0: B9 03 F5 */  lda F503_table,y
/* 1F4A3: A8 */        tay
/* 1F4A4: D0 D6 */     bne L1F47C
; Create a bubblebot in random of positions
F1F4A6:
/* 1F4A6: A9 03 */     lda #$03
/* 1F4A8: 20 A0 C5 */  jsr RandomFunc
/* 1F4AB: 85 0C */     sta $0C
/* 1F4AD: A9 3A */     lda #$3A
/* 1F4AF: 20 63 F6 */  jsr CreateEnemy                                   ; $F663
/* 1F4B2: B0 22 */     bcs L1F4D6
/* 1F4B4: A9 06 */     lda #$06
/* 1F4B6: 9D 20 04 */  sta ObjectFlags,x
/* 1F4B9: A4 0C */     ldy $0C
/* 1F4BB: B9 0A F5 */  lda BubbleBotXTable,y
/* 1F4BE: 9D 80 04 */  sta ObjectPosX,x
/* 1F4C1: B9 0D F5 */  lda BubbleBotYTable,y
/* 1F4C4: 9D 00 06 */  sta ObjectPosY,x
/* 1F4C7: A9 DE */     lda #$DE
/* 1F4C9: 9D 40 06 */  sta ObjectFireDelay,x
/* 1F4CC: A5 0C */     lda $0C
/* 1F4CE: F0 CE */     beq L1F49E
/* 1F4D0: C9 01 */     cmp #$01
/* 1F4D2: F0 92 */     beq L1F466
/* 1F4D4: D0 9B */     bne L1F471
L1F4D6:
/* 1F4D6: 38 */        sec
/* 1F4D7: 60 */        rts
;When a bubblebot was killed
L1F4D8:
/* 1F4D8: E6 44 */     inc BossVariable44
/* 1F4DA: E6 3E */     inc BossCurrentStrategy
/* 1F4DC: A4 44 */     ldy BossVariable44
/* 1F4DE: B9 0F F5 */  lda BubbleBotColourTable-1,y
/* 1F4E1: 8D EA 03 */  sta SpritePalettes + $A
/* 1F4E4: A9 7E */     lda #$7E
/* 1F4E6: 85 3F */     sta BossVariable3F
/* 1F4E8: 38 */        sec
/* 1F4E9: AD C1 06 */  lda ObjectLifeMeter+1
/* 1F4EC: E9 04 */     sbc #$04
/* 1F4EE: 8D C1 06 */  sta ObjectLifeMeter+1
/* 1F4F1: 20 2A C7 */  jsr PaletteSetupForSprites
/* 1F4F4: 60 */        rts



F4F5_table:
    .byte $88, $88, $94, $94, $A0, $AC, $24
F4FC_table:
    .byte $80, $80, $8C, $8C, $98, $A4, $1C
F503_table:
    .byte $84, $84, $90, $90, $9C, $A8, $2C
BubbleBotXTable:
    .byte $80,$10,$F0
BubbleBotYTable:
    .byte $10,$70,$70

BubbleBotColourTable:
    .byte $22, $2A, $24, $26, $17, $15


FindObjectOfSelectedType:
; Input: $0C = object type to find
;          X = first object to search
; Output:
;    carry clear: X = object index
;    carry set: not found
/* 1F516: BD 00 04 */  lda ObjectSpriteNum,x
/* 1F519: C9 FF */     cmp #$FF
/* 1F51B: D0 07 */     bne L1F524 ; +
/* 1F51D: A5 0C */     lda $0C
/* 1F51F: DD E0 06 */  cmp ObjectType,x
/* 1F522: F0 06 */     beq L1F52A ; ++
; -
L1F524: ; +
/* 1F524: E8 */        inx
/* 1F525: E0 20 */     cpx #$20
/* 1F527: 90 ED */     bcc FindObjectOfSelectedType
/* 1F529: 60 */        rts
L1F52A: ; ++
/* 1F52A: BD 00 06 */  lda ObjectPosY,x
/* 1F52D: C9 F8 */     cmp #$F8  ;Skip the object if its Y == #$F8
/* 1F52F: F0 F3 */     beq L1F524 ; -
/* 1F531: 18 */        clc
/* 1F532: 60 */        rts


; Function F533 is called with:
;   X = object number
;   Y = parameter
;         values seen are:
;          #$08
;          #$C4
;          DefaultObjectSpeedCtrl[$02]
;          #$24
;          #$78
;          #$94
;          $F391[$59]
;          $F4F5[BossVariable44]
;          #$4C + ( $0C << 2)
;          #$0C + ( rnd(1) << 2)
;          #$40
;          #$1C..#$38
;
; Y might be enemy number, but then again, it might not be.

InitObjectDefaultSpeed:
/* 1F533: B9 4C F5 */  lda F54C_table+0,y
/* 1F536: 9D 60 06 */  sta ObjectYSpeedFraction,x
/* 1F539: B9 4D F5 */  lda F54C_table+1,y
/* 1F53C: 9D 80 06 */  sta ObjectYSpeed,x
/* 1F53F: B9 4E F5 */  lda F54C_table+2,y
/* 1F542: 9D E0 04 */  sta ObjectXSpeedFraction,x
/* 1F545: B9 4F F5 */  lda F54C_table+3,y
/* 1F548: 9D C0 04 */  sta ObjectXSpeed,x
/* 1F54B: 60 */        rts


F54C_table:
    ; YSpeedFraction, YSpeed, XSpeedFraction and XSpeed
    .byte $E0,$04,$30,$01 ;00 Flea act 0
    .byte $00,$04,$00,$03 ;01 Flea act 1
    .byte $00,$00,$00,$00 ;02 atomic explosion and many other things
    .byte $2C,$02,$2C,$02 ;03 Beak shot 0
    .byte $68,$00,$0F,$03 ;04 Beak shot 1
    .byte $98,$FF,$0F,$03 ;05 Beak shot 2
    .byte $D4,$FD,$2C,$02 ;06 Beak shot 3
    .byte $00,$04,$00,$00 ;07 Unknown, used at $05AE61, also TackleFire, also bonus drop
    .byte $D0,$02,$D0,$02 ;08
    .byte $00,$00,$00,$04 ;09 Boss6 piece1 (dir: right), also Boss7 I/F-weapon, also [6] from F4F5_table, also Sniper Joe, also Object1E
    .byte $30,$FD,$D0,$02 ;0A [4] from F391_table
    .byte $00,$FC,$00,$00 ;0B used by ZigZagFire, default setting for enemy 0E
    .byte $30,$FD,$D0,$02 ;0C
    .byte $00,$00,$00,$04 ;0D [6] from F391_table
    .byte $D0,$02,$D0,$02
    .byte $A0,$FF,$80,$00
    .byte $00,$FE,$20,$01 ;10 Sinewave flier
    .byte $80,$03,$C0,$01 ;11 Bombomb act 0
    .byte $40,$03,$00,$01 ;12 Bombomb act 1
    .byte $00,$00,$47,$01 ;13 Metto shot 0 , Bombomb act 2?
    .byte $BB,$00,$0C,$01 ;14 Metto shot 1 , Bombomb act 3?
    .byte $45,$FF,$0C,$01 ;15 Metto shot 2 , Bombomb act 4?
    .byte $00,$03,$00,$00 ;16 Bombomb act 5? Also [4] from B391_table
    .byte $2C,$02,$2C,$02 ;17 Bombomb act 6? Also [2,6] from B391_table
    .byte $00,$00,$00,$03 ;18 Bombomb act 7? Also [0-1,8-9] from B391_table
    .byte $D4,$FD,$2C,$02 ;19 [3,7] from B391_table
    .byte $00,$FD,$00,$00 ;1A [5] from B391_table, also used by ZigZagFire
    .byte $51,$03,$34,$01 ;1B BigEye act 0
    .byte $E6,$04,$D1,$00 ;1C BigEye act 1, also Picketman, also Object1C
    .byte $10,$FF,$57,$00 ;1D Unknown, used at $05BBC1
    .byte $00,$02,$00,$02 ;1E Boss7 C-weapon and [1] from F391_table
    .byte $00,$03,$80,$01 ;1F Boss7 B-weapon
    .byte $47,$01,$00,$00 ;20
    .byte $B9,$FE,$00,$00
    .byte $00,$00,$47,$01 ;22 [0,1] from F4F5_table
    .byte $99,$01,$00,$00
    .byte $67,$FE,$00,$00 ;24
    .byte $00,$00,$99,$01 ;25 used in Boss8and9Init, also [2,3] from F4F5_table
    .byte $66,$02,$00,$00
    .byte $9A,$FD,$00,$00
    .byte $00,$00,$66,$02 ;28 [4] from F4F5_table
    .byte $33,$03,$00,$00
    .byte $CD,$FC,$00,$00
    .byte $00,$00,$33,$03 ;2B [5] from F4F5_table
    .byte $00,$00,$20,$01 ;2C Enemy 00,0A,14: bunby heri, hamburger, feet-losing robots
    .byte $C0,$FF,$60,$00 ;2D Enemy 03: landmine
    .byte $00,$02,$00,$00 ;2E Enemy 04: vertical eyecube
    .byte $A0,$00,$00,$00 ;2F Enemy 06: Unknown (watchers?)
    .byte $00,$FE,$20,$01 ;30 Enemy 07,08: Sinewave fliers
    .byte $C0,$FF,$00,$00 ;31 level-end item, enemy 09: jumping telephone post
    .byte $C0,$FF,$90,$01 ;32 Enemy 12: green guardian, axe-throwing robot, ROCKMAN?
    .byte $00,$00,$80,$00 ;33 Object3B; Enemy 13: Foot holder
    .byte $00,$00,$00,$02 ;34 Foot holder
    .byte $00,$02,$00,$05 ;35 [0,3,5] from F391_table
    .byte $00,$03,$00,$04 ;36 [2] from F391_table

; In: $00, $01 are something
;
F1F628:
/* 1F628: A5 0C */     lda $0C
/* 1F62A: 85 02 */     sta $02
/* 1F62C: A4 2F */     ldy RefObjectNum
/* 1F62E: 38 */        sec
/* 1F62F: B9 00 06 */  lda ObjectPosY,y
/* 1F632: ED 00 06 */  sbc ObjectPosY+0                                 ; ObjectPosY+0
/* 1F635: 85 03 */     sta $03
/* 1F637: 20 C6 F8 */  jsr F8C6_routine
/* 1F63A: 60 */        rts




EnemySearchMegaman:
; Makes the object face towards Megaman, and calculates the (horizontal) distance too
;
; Sets object facing.
; Return value; signed integer: distance to Megaman in pixels
;
/* 1F63B: BD 20 04 */  lda ObjectFlags,x
/* 1F63E: 29 BF */     and #$BF
/* 1F640: 9D 20 04 */  sta ObjectFlags,x ;Makes the object face left
/* 1F643: 38 */        sec
/* 1F644: AD 80 04 */  lda ObjectPosX+0
/* 1F647: FD 80 04 */  sbc ObjectPosX,x
/* 1F64A: A8 */        tay
/* 1F64B: AD 60 04 */  lda ObjectPosScreen+0                         ; $0460
/* 1F64E: FD 60 04 */  sbc ObjectPosScreen,x
/* 1F651: 90 0A */     bcc L1F65D ; +                                           ; $F65D

/* 1F653: BD 20 04 */  lda ObjectFlags,x
/* 1F656: 09 40 */     ora #$40
/* 1F658: 9D 20 04 */  sta ObjectFlags,x ;Makes the object face right
/* 1F65B: 98 */        tya
/* 1F65C: 60 */        rts
L1F65D: ; +
/* 1F65D: 98 */        tya
/* 1F65E: 49 FF */     eor #$FF
/* 1F660: 69 01 */     adc #$01
/* 1F662: 60 */        rts



; Issue bullet sound if object type is #$1A or #$2D
CreateEnemy:
/* 1F663: 48 */        pha
/* 1F664: C9 1A */     cmp #$1A
/* 1F666: F0 04 */     beq L1F66C ; +                                           ; $F66C
/* 1F668: C9 2D */     cmp #$2D
/* 1F66A: D0 05 */     bne L1F671 ; ++                                          ; $F671
L1F66C: ; +
/* 1F66C: A9 15 */     lda #$15        ; Enemy bullet
/* 1F66E: 20 77 C4 */  jsr IssueSound                                  ; $C477
L1F671: ; ++
/* 1F671: 68 */        pla

/* 1F672: 48 */        pha
/* 1F673: A2 10 */     ldx #$10
/* 1F675: 20 76 C5 */  jsr FindFreeObject                              ; FindFreeObject
/* 1F678: B0 40 */     bcs L1F6BA ; ++                                          ; $F6BA
/* 1F67A: 68 */        pla

InitActor:
; A               = enemy type
; X               = target enemy (from FindFreeObject, hopefully)
; RefObjectNum = source enemy
/* 1F67B: 9D E0 06 */  sta ObjectType,x
/* 1F67E: A9 FF */     lda #$FF
/* 1F680: 9D 00 04 */  sta ObjectSpriteNum,x
/* 1F683: A4 2F */     ldy RefObjectNum

/* 1F685: B9 00 06 */  lda ObjectPosY,y
/* 1F688: 9D 00 06 */  sta ObjectPosY,x

/* 1F68B: B9 80 04 */  lda ObjectPosX,y
/* 1F68E: 9D 80 04 */  sta ObjectPosX,x

/* 1F691: B9 60 04 */  lda ObjectPosScreen,y
/* 1F694: 9D 60 04 */  sta ObjectPosScreen,x

; Make bullet move in the direction the enemy
; is facing and set collision detection ON
/* 1F697: B9 20 04 */  lda ObjectFlags,y
/* 1F69A: 29 40 */     and #$40
/* 1F69C: 09 02 */     ora #$02
/* 1F69E: 9D 20 04 */  sta ObjectFlags,x

; Give the bullet #20 life units... =)
/* 1F6A1: A9 14 */     lda #$14
/* 1F6A3: 9D C0 06 */  sta ObjectLifeMeter,x

/* 1F6A6: A9 00 */     lda #$00
/* 1F6A8: 9D 40 04 */  sta ObjectUnknown440,x
/* 1F6AB: 9D 40 06 */  sta ObjectFireDelay,x
/* 1F6AE: 9D A0 06 */  sta ObjectLifeCycleCounter,x

/* 1F6B1: E0 10 */     cpx #$10
/* 1F6B3: 90 03 */     bcc L1F6B8 ; +                                           ; $F6B8
/* 1F6B5: 9D 80 05 */  sta  IssuedSoundsList,x
L1F6B8: ; +
/* 1F6B8: 18 */        clc
/* 1F6B9: 60 */        rts

L1F6BA: ; ++
/* 1F6BA: 68 */        pla
/* 1F6BB: 60 */        rts

CheckMagnetBeamCollisionWithBG:
/* 1F6BC: BD A0 05 */  lda MagnetBeamLength,x
/* 1F6BF: 29 7C */     and #$7C
/* 1F6C1: 0A */        asl a
/* 1F6C2: 69 08 */     adc #$08
/* 1F6C4: 09 02 */     ora #$02
/* 1F6C6: 85 0C */     sta $0C
/* 1F6C8: BD 20 04 */  lda ObjectFlags,x
/* 1F6CB: 29 40 */     and #$40
/* 1F6CD: F0 10 */     beq L1F6DF ; +
; Points to RIGHT?
/* 1F6CF: 18 */        clc
/* 1F6D0: BD C0 05 */  lda MagnetBeamPosX,x
/* 1F6D3: 65 0C */     adc $0C
/* 1F6D5: 85 0D */     sta $0D
/* 1F6D7: BD D0 05 */  lda MagnetBeamPosScreen,x
/* 1F6DA: 69 00 */     adc #$00
/* 1F6DC: 4C EC F6 */  jmp L1F6EC ; ++
L1F6DF: ; + ;Points to LEFT?
/* 1F6DF: 38 */        sec
/* 1F6E0: BD C0 05 */  lda MagnetBeamPosX,x
/* 1F6E3: E5 0C */     sbc $0C
/* 1F6E5: 85 0D */     sta $0D
/* 1F6E7: BD D0 05 */  lda MagnetBeamPosScreen,x
/* 1F6EA: E9 00 */     sbc #$00
L1F6EC: ; ++
/* 1F6EC: 85 0C */     sta $0C
/* 1F6EE: BD F0 05 */  lda MagnetBeamPosY,x
/* 1F6F1: 85 0E */     sta $0E
/* 1F6F3: 20 9C C3 */  jsr SwitchBankStage                             ; $C39C
/* 1F6F6: 20 B7 CC */  jsr ReadCurrentStageMap
/* 1F6F9: A8 */        tay
/* 1F6FA: 4C B3 C3 */  jmp SwitchBank05                                ; $C3B3


RoutineF6FD_GutsmanWeapon:
/* 1F6FD: A5 60 */     lda WeaponFiring
/* 1F6FF: D0 37 */     bne L1F738
/* 1F701: AD 20 04 */  lda ObjectFlags+0
/* 1F704: A2 00 */     ldx #$00
/* 1F706: 29 40 */     and #$40
/* 1F708: F0 02 */     beq L1F70C ; +                                           ; $F70C
/* 1F70A: A2 02 */     ldx #$02
L1F70C: ; +
/* 1F70C: 18 */        clc
/* 1F70D: AD 80 04 */  lda ObjectPosX+0
/* 1F710: 7D 8B F7 */  adc F78B_table+0,x
/* 1F713: 85 0D */     sta $0D
/* 1F715: AD 60 04 */  lda ObjectPosScreen+0                         ; $0460
/* 1F718: 7D 8C F7 */  adc F78B_table+1,x
/* 1F71B: 85 0C */     sta $0C
/* 1F71D: AD 00 06 */  lda ObjectPosY+0                                 ; ObjectPosY+0
/* 1F720: 85 0E */     sta $0E
/* 1F722: A4 8E */     ldy ActivesLowerIndex
/* 1F724: 20 8A CD */  jsr CheckCollisionAgainstActives
/* 1F727: C9 82 */     cmp #$82
/* 1F729: F0 0E */     beq L1F739 ; ++
/* 1F72B: A0 FF */     ldy #$FF
/* 1F72D: C4 52 */     cpy GutsmanWeaponTargetActive
/* 1F72F: F0 07 */     beq L1F738 ; +
/* 1F731: 84 52 */     sty GutsmanWeaponTargetActive
L1F733:
/* 1F733: A9 F8 */     lda #$F8
/* 1F735: 8D 05 06 */  sta ObjectPosY+5
L1F738: ; +
/* 1F738: 60 */        rts
L1F739: ; ++
/* 1F739: 84 52 */     sty GutsmanWeaponTargetActive
/* 1F73B: A5 23 */     lda FrameCounter
/* 1F73D: 29 04 */     and #$04
/* 1F73F: D0 F2 */     bne L1F733
/* 1F741: A2 05 */     ldx #$05
; Launch debris
/* 1F743: A9 02 */     lda #$02
/* 1F745: 20 13 A9 */  jsr LaunchMegamanWeaponShot
/* 1F748: A4 52 */     ldy GutsmanWeaponTargetActive
/* 1F74A: 18 */        clc
/* 1F74B: B9 23 07 */  lda RoomActiveTable+3,y ;the x1 of the gutsblock
/* 1F74E: 69 10 */     adc #$10
/* 1F750: 9D 80 04 */  sta ObjectPosX,x
/* 1F753: B9 24 07 */  lda RoomActiveTable+4,y ;the y1 of the gutsblock
/* 1F756: 18 */        clc
/* 1F757: 69 0F */     adc #$0F
/* 1F759: 9D 00 06 */  sta ObjectPosY,x
/* 1F75C: A9 6C */     lda #$6C
/* 1F75E: 9D 00 04 */  sta ObjectSpriteNum,x
/* 1F761: BD 20 04 */  lda ObjectFlags,x
/* 1F764: 29 BE */     and #$BE
/* 1F766: 9D 20 04 */  sta ObjectFlags,x
/* 1F769: 60 */        rts


LoadGutsblockPosition:
/* 1F76A: A5 1C */     lda TSAPPUtransferSize
/* 1F76C: 0A */        asl a
/* 1F76D: 0A */        asl a
/* 1F76E: 85 0D */     sta $0D
/* 1F770: 0A */        asl a
/* 1F771: 0A */        asl a
/* 1F772: 18 */        clc
/* 1F773: 65 1C */     adc TSAPPUtransferSize
/* 1F775: 18 */        clc
/* 1F776: 65 0D */     adc $0D
/* 1F778: 85 0D */     sta $0D
/* 1F77A: B9 24 07 */  lda RoomActiveTable+4,y ;the y1 of the gutsblock
/* 1F77D: 4A */        lsr a
/* 1F77E: 4A */        lsr a
/* 1F77F: 4A */        lsr a
/* 1F780: 19 23 07 */  ora RoomActiveTable+3,y ;the x1 of the gutsblock
/* 1F783: 85 04 */     sta $04
/* 1F785: B9 22 07 */  lda RoomActiveTable+2,y ;the screen# of the gutsblock
/* 1F788: 85 05 */     sta $05
/* 1F78A: 60 */        rts

F78B_table:
    .byte $F7,$FF
        .byte $08,$00

GutsblockHitByElecBeam:
/* 1F78F: A9 FF */     lda #$FF
/* 1F791: 85 61 */     sta NumberOfFramesSinceShooting
/* 1F793: A9 04 */     lda #$04
/* 1F795: 99 21 07 */  sta RoomActiveTable+1,y ;set the gutsblock as disintegrated
/* 1F798: A9 00 */     lda #$00
/* 1F79A: 85 2F */     sta RefObjectNum
/* 1F79C: 98 */        tya
/* 1F79D: 48 */        pha
/* 1F79E: 20 6A F7 */  jsr LoadGutsblockPosition
/* 1F7A1: 20 E8 CD */  jsr DrawBlockFromActiveLevelMap
/* 1F7A4: 68 */        pla
/* 1F7A5: A8 */        tay
/* 1F7A6: B9 24 07 */  lda RoomActiveTable+4,y ;load y1
/* 1F7A9: 85 03 */     sta $03
/* 1F7AB: B9 23 07 */  lda RoomActiveTable+3,y ;load x1
/* 1F7AE: 85 00 */     sta $00
/* 1F7B0: B9 22 07 */  lda RoomActiveTable+2,y ;load screen#
/* 1F7B3: 85 01 */     sta $01
F1F7B5:
/* 1F7B5: AD 25 04 */  lda ObjectFlags+5
/* 1F7B8: 29 40 */     and #$40
/* 1F7BA: 09 04 */     ora #$04
/* 1F7BC: 85 02 */     sta $02
/* 1F7BE: A2 05 */     ldx #$05
/* 1F7C0: A0 00 */     ldy #$00
L1F7C2: ; -
/* 1F7C2: 84 0C */     sty $0C
/* 1F7C4: 20 E2 F7 */  jsr CreateGutsblockPieces
/* 1F7C7: E6 0C */     inc $0C
/* 1F7C9: A4 0C */     ldy $0C
/* 1F7CB: E8 */        inx
/* 1F7CC: E0 09 */     cpx #$09
/* 1F7CE: D0 F2 */     bne L1F7C2 ; -
/* 1F7D0: A2 09 */     ldx #$09
/* 1F7D2: A9 F8 */     lda #$F8
L1F7D4: ; -
/* 1F7D4: 9D 00 06 */  sta ObjectPosY,x
/* 1F7D7: E8 */        inx
/* 1F7D8: E0 10 */     cpx #$10
/* 1F7DA: D0 F8 */     bne L1F7D4 ; -

/* 1F7DC: A9 17 */     lda #$17        ; Throw boulder sound
/* 1F7DE: 20 77 C4 */  jsr IssueSound                                  ; $C477
/* 1F7E1: 60 */        rts


CreateGutsblockPieces:
; In: X=object number
;     Y=0-3 (index of the piece)
/* 1F7E2: 18 */        clc
/* 1F7E3: A5 00 */     lda $00
/* 1F7E5: 79 26 F8 */  adc F826_table,y
/* 1F7E8: 9D 80 04 */  sta ObjectPosX,x
/* 1F7EB: A5 01 */     lda $01
/* 1F7ED: 69 00 */     adc #$00
/* 1F7EF: 9D 60 04 */  sta ObjectPosScreen,x
/* 1F7F2: 18 */        clc
/* 1F7F3: A5 03 */     lda $03
/* 1F7F5: 79 2A F8 */  adc F82A_table,y
/* 1F7F8: 9D 00 06 */  sta ObjectPosY,x
/* 1F7FB: B9 2E F8 */  lda F82E_table,y
/* 1F7FE: 9D C0 04 */  sta ObjectXSpeed,x
/* 1F801: B9 32 F8 */  lda F832_table,y
/* 1F804: 9D 80 06 */  sta ObjectYSpeed,x
/* 1F807: A5 02 */     lda $02
/* 1F809: 9D 20 04 */  sta ObjectFlags,x
/* 1F80C: A4 31 */     ldy CurrentStage
/* 1F80E: B9 36 F8 */  lda F836_table,y
/* 1F811: 9D 00 04 */  sta ObjectSpriteNum,x
/* 1F814: A9 00 */     lda #$00
/* 1F816: 9D E0 04 */  sta ObjectXSpeedFraction,x
/* 1F819: 9D 60 06 */  sta ObjectYSpeedFraction,x
/* 1F81C: 9D 40 04 */  sta ObjectUnknown440,x
/* 1F81F: 9D 40 06 */  sta ObjectFireDelay,x
/* 1F822: 9D A0 06 */  sta ObjectLifeCycleCounter,x
/* 1F825: 60 */        rts

F826_table: .byte $08,$18,$08,$18 ;PosX modifier
F82A_table: .byte $08,$08,$18,$18 ;PosY modifier
F82E_table: .byte $04,$04,$03,$03 ;XSpeed for gutsblock
F832_table: .byte $02,$00,$02,$00 ;YSpeed for gutsblock
F836_table: .byte $6D,$6D,$6D,$6D,$6D
            .byte $56,$6D,$6D,$6D,$56,$6D ;SpriteNum

CreateExplosionObject:
/* 1F841: B9 00 06 */  lda ObjectPosY,y
/* 1F844: 9D 00 06 */  sta ObjectPosY,x
/* 1F847: B9 80 04 */  lda ObjectPosX,y
/* 1F84A: 9D 80 04 */  sta ObjectPosX,x
/* 1F84D: B9 60 04 */  lda ObjectPosScreen,y
/* 1F850: 9D 60 04 */  sta ObjectPosScreen,x
F1F853:
/* 1F853: A5 0D */     lda $0D
/* 1F855: 9D 00 04 */  sta ObjectSpriteNum,x
/* 1F858: A4 0C */     ldy $0C
/* 1F85A: B9 8A F8 */  lda F88A_table,y
/* 1F85D: 9D E0 04 */  sta ObjectXSpeedFraction,x
/* 1F860: B9 96 F8 */  lda F896_table,y
/* 1F863: 9D C0 04 */  sta ObjectXSpeed,x
/* 1F866: B9 A2 F8 */  lda F8A2_table,y
/* 1F869: 9D 60 06 */  sta ObjectYSpeedFraction,x
/* 1F86C: B9 AE F8 */  lda F8AE_table,y
/* 1F86F: 9D 80 06 */  sta ObjectYSpeed,x
/* 1F872: B9 BA F8 */  lda F8BA_table,y
/* 1F875: 05 0E */     ora $0E
/* 1F877: 9D 20 04 */  sta ObjectFlags,x
/* 1F87A: A9 10 */     lda #$10
/* 1F87C: 9D 40 04 */  sta ObjectUnknown440,x
/* 1F87F: A9 00 */     lda #$00
/* 1F881: 9D A0 06 */  sta ObjectLifeCycleCounter,x
/* 1F884: 9D 40 06 */  sta ObjectFireDelay,x
/* 1F887: C6 0C */     dec $0C
/* 1F889: 60 */        rts

F88A_table: .byte $00,$00,$00,$00,$60,$60 ;ObjectXSpeedFraction for explosion
            .byte $60,$60,$00,$C0,$00,$E0
F896_table: .byte $00,$02,$00,$02,$01,$01 ;ObjectXSpeed for explosion
            .byte $01,$01,$00,$00,$00,$00
F8A2_table: .byte $00,$00,$00,$00,$60,$A0 ;ObjectYSpeedFraction for explosion
            .byte $A0,$60,$C0,$00,$40,$00
F8AE_table: .byte 2,0,NEG{2},0,1,NEG{2}   ;ObjectYSpeed for explosion
            .byte NEG{2},1,0,0,NEG{1},0
F8BA_table: .byte $00,$40,$00,$00,$40,$40 ;ObjectFlags for explosion
            .byte $00,$00,$00,$40,$00,$00

F8C6_routine:
/* 1F8C6: 08 */        php
/* 1F8C7: B0 06 */     bcs L1F8CF ; +
/* 1F8C9: A5 03 */     lda $03
/* 1F8CB: 49 FF */     eor #$FF
/* 1F8CD: 85 03 */     sta $03
L1F8CF: ; +
/* 1F8CF: A5 02 */     lda $02
/* 1F8D1: 86 0C */     stx $0C
/* 1F8D3: C5 03 */     cmp $03
/* 1F8D5: 90 2F */     bcc L1F906 ; +  ; $0001F906
/* 1F8D7: A5 01 */     lda $01
/* 1F8D9: 9D C0 04 */  sta ObjectXSpeed,x
/* 1F8DC: 85 07 */     sta CurrentRoomPointer+1
/* 1F8DE: A5 00 */     lda $00
/* 1F8E0: 9D E0 04 */  sta ObjectXSpeedFraction,x
/* 1F8E3: 85 06 */     sta CurrentRoomPointer
/* 1F8E5: A5 02 */     lda $02
/* 1F8E7: 20 4C F9 */  jsr F1F94C
/* 1F8EA: A5 09 */     lda CurrentRoomPointer+3
/* 1F8EC: 85 07 */     sta CurrentRoomPointer+1
/* 1F8EE: A5 08 */     lda CurrentRoomPointer+2
/* 1F8F0: 85 06 */     sta CurrentRoomPointer
/* 1F8F2: A5 03 */     lda $03
/* 1F8F4: 20 4C F9 */  jsr F1F94C
/* 1F8F7: A6 0C */     ldx $0C
/* 1F8F9: A5 09 */     lda CurrentRoomPointer+3
/* 1F8FB: 9D 80 06 */  sta ObjectYSpeed,x
/* 1F8FE: A5 08 */     lda CurrentRoomPointer+2
/* 1F900: 9D 60 06 */  sta ObjectYSpeedFraction,x
/* 1F903: 4C 32 F9 */  jmp L1F932
L1F906: ; +
/* 1F906: A5 01 */     lda $01
/* 1F908: 9D 80 06 */  sta ObjectYSpeed,x
/* 1F90B: 85 07 */     sta CurrentRoomPointer+1
/* 1F90D: A5 00 */     lda $00
/* 1F90F: 9D 60 06 */  sta ObjectYSpeedFraction,x
/* 1F912: 85 06 */     sta CurrentRoomPointer
/* 1F914: A5 03 */     lda $03
/* 1F916: 20 4C F9 */  jsr F1F94C
/* 1F919: A5 09 */     lda CurrentRoomPointer+3
/* 1F91B: 85 07 */     sta CurrentRoomPointer+1
/* 1F91D: A5 08 */     lda CurrentRoomPointer+2
/* 1F91F: 85 06 */     sta CurrentRoomPointer
/* 1F921: A5 02 */     lda $02
/* 1F923: 20 4C F9 */  jsr F1F94C
/* 1F926: A6 0C */     ldx $0C
/* 1F928: A5 09 */     lda CurrentRoomPointer+3
/* 1F92A: 9D C0 04 */  sta ObjectXSpeed,x
/* 1F92D: A5 08 */     lda CurrentRoomPointer+2
/* 1F92F: 9D E0 04 */  sta ObjectXSpeedFraction,x
L1F932:
/* 1F932: 28 */        plp
/* 1F933: B0 16 */     bcs L1F94B
/* 1F935: BD 80 06 */  lda ObjectYSpeed,x
/* 1F938: 49 FF */     eor #$FF
/* 1F93A: 48 */        pha
/* 1F93B: BD 60 06 */  lda ObjectYSpeedFraction,x
/* 1F93E: 49 FF */     eor #$FF
/* 1F940: 69 01 */     adc #$01
/* 1F942: 9D 60 06 */  sta ObjectYSpeedFraction,x
/* 1F945: 68 */        pla
/* 1F946: 69 00 */     adc #$00
/* 1F948: 9D 80 06 */  sta ObjectYSpeed,x
L1F94B:
/* 1F94B: 60 */        rts

F1F94C:
/* 1F94C: 85 05 */     sta $05
/* 1F94E: A9 00 */     lda #$00
/* 1F950: 85 04 */     sta $04
/* 1F952: 20 D5 C5 */  jsr F1C5D5
/* 1F955: 60 */        rts




;
; This routine seems to use the ExtraLives variable for something else...
;
; (Title screen?...)
;

DrawAtReset:

/* 1F956: 20 9C C3 */  jsr SwitchBankStage     ; $C39C

/* 1F959: A9 20 */     lda #$20
/* 1F95B: 8D 06 20 */  sta $2006 ;mem addr $2000
/* 1F95E: A9 00 */     lda #$00
/* 1F960: 8D 06 20 */  sta $2006
/* 1F963: A2 0E */     ldx #$0E

.ifdef J_VERSION
/* 1F965: A9 80 */     lda #$80
.else
/* 1F965: A9 7f */     lda #$7f
.endif

/* 1F967: A4 A6 */     ldy ExtraLives
/* 1F969: 10 02 */     bpl L1F96D ; +                                           ; $F96D
/* 1F96B: A9 A0 */     lda #$A0 ;decide tile $A0
; +
L1F96D: ; --
/* 1F96D: A0 40 */     ldy #$40
L1F96F: ; -
/* 1F96F: 8D 07 20 */  sta $2007 ;write data
/* 1F972: 88 */        dey
/* 1F973: D0 FA */     bne L1F96F ; -                                           ; $F96F
/* 1F975: CA */        dex
/* 1F976: 10 F5 */     bpl L1F96D ; --                                          ; $F96D



/* 1F978: A5 A6 */     lda ExtraLives
/* 1F97A: 10 03 */       bpl L1F97F ; +
/* 1F97C: 4C 5C FA */    jmp L1FA5C ; ++

/*
Contents of the data for game over screen:
(located at 0EC7 in bank 4):
(Note: $0EC7 equals $EA1 + $26)

0000 attribute data (8 bytes) for 8 rows
0008 7 records of this type (begins at ECF):
  0000 First byte of VRAM address
  0007 Second byte of VRAM address
  000E Number of tiles to write to this address
   (the tiles written are values of Y, consequent integers)
001D text data in this format (begins at 0EE4):
  0000 word nametable address (ex. 222B)
  0002 byte number of bytes (ex. 0B)
  0003 data                 (ex. "PRESS START")
  Total number of entries is $04 (hardcoded at $F9D5).
*/


L1F97F: ; +
/* 1F97F: A2 07 */       ldx #$07
L1F981: ; --
/* 1F981: A0 08 */       ldy #$08
L1F983: ; -
/* 1F983: BD C7 8E */    lda L10EC7,x ;startup screen data
/* 1F986: 8D 07 20 */    sta $2007
/* 1F989: 88 */          dey
/* 1F98A: D0 F7 */       bne L1F983 ; -
/* 1F98C: CA */          dex
/* 1F98D: 10 F2 */       bpl L1F981 ; --

.ifdef J_VERSION
/* 1F98F: A2 0F */       ldx #$0F
L1F991: ; --
/* 1F991: BD CF 8E */    lda L10ECF,x
/* 1F994: 8D 06 20 */    sta $2006
/* 1F997: BD DF 8E */    lda L10EDF,x
/* 1F99A: 8D 06 20 */    sta $2006
/* 1F99D: 86 0C */       stx $0C
/* 1F99F: BD DD 8E */    lda L10EEF,x
/* 1F9A2: AA */          tax
L1F9A3: ; -
/* 1F9A3: 8C 07 20 */    sty $2007
/* 1F9A6: C8 */          iny
/* 1F9A7: CA */          dex
/* 1F9A8: D0 F9 */       bne L1F9A3 ; -
/* 1F9AA: A6 0C */       ldx $0C
/* 1F9AC: C0 80    */    cpy #$80
/* 1F9AE: D0 02    */    bne L1F9B2
/* 1F9B0: A0 DA    */    ldy #$DA
L1F9B2:
/* 1F9B2: CA       */    dex
/* 1F9B3: 10 DC    */    bpl L1F991
/* 1F9B5: A2 03    */    ldx #$03
L1F9B7:
/* 1F9B7: A9 22    */    lda #$22
/* 1F9B9: 8D 06 20 */    sta $2006
/* 1F9BC: BD FF 8E */    lda L10EFF,X
/* 1F9BF: 8D 06 20 */    sta $2006
/* 1F9C2: A0 0B    */    ldy #$0B
/* 1F9C4: BD 03 8F */    lda L10F03,X
L1F9C7:
/* 1F9C7: 8D 07 20 */    sta $2007
/* 1F9CA: 88       */    dey
/* 1F9CB: D0 FA    */    bne L1F9C7
/* 1F9CD: CA       */    dex
/* 1F9CE: 10 E7    */    bpl L1F9B7
/* 1F9D0: E8       */    inx
/* 1F9D1: A9 03    */    lda #$03
/* 1F9D3: 85 0C    */    sta $0C
L1F9D5:
/* 1F9D5: BD 1D 8F */    lda L10F1D,X
/* 1F9D8: 8D 06 20 */    sta $2006
/* 1F9DB: E8       */    inx
/* 1F9DC: BD 1D 8F */    lda L10F1D,X
/* 1F9DF: 8D 06 20 */    sta $2006
/* 1F9E2: E8       */    inx
/* 1F9E3: BC 1D 8F */    ldy L10F1D,X
/* 1F9E6: E8       */    inx
L1F9E7:
/* 1F9E7: BD 1D 8F */    lda L10F1D,X
/* 1F9EA: 8D 07 20 */    sta $2007
/* 1F9ED: E8       */    inx
/* 1F9EE: 88       */    dey
/* 1F9EF: D0 F6    */    bne L1F9E7
/* 1F9F1: C6 0C    */    dec $0C
/* 1F9F3: D0 E0    */    bne L1F9D5
/* 1F9F5: A9 23    */    lda #$23
/* 1F9F7: 8D 06 20 */    sta $2006
/* 1F9FA: A9 DE    */    lda #$DE
/* 1F9FC: 8D 06 20 */    sta $2006
/* 1F9FF: A9 FF    */    lda #$FF
/* 1FA01: 8D 07 20 */    sta $2007
WaitForStartScreenInput:
/* 1FA04: A0 00    */    ldy #$00
/* 1FA06: 84 BC    */    sty IgnoreStageSelection
/* 1FA08: A2 40    */    ldx #$40
/* 1FA0A: 20 78 D4 */    jsr HideSprites
/* 1FA0D: A5 A6    */    lda ExtraLives
/* 1FA0F: 30 20    */    bmi L1FA31
/* 1FA11: A0 0A    */    ldy #$0A
/* 1FA13: A2 28    */    ldx #$28
L1FA15:
/* 1FA15: 98       */    tya
/* 1FA16: 9D 05 02 */    sta SpriteTable+5,X
/* 1FA19: A9 01    */    lda #$01
/* 1FA1B: 9D 06 02 */    sta SpriteTable+6,X
/* 1FA1E: B9 07 8F */    lda L10F07,Y
/* 1FA21: 9D 07 02 */    sta SpriteTable+7,X
/* 1FA24: B9 12 8F */    lda L10F12,Y
/* 1FA27: 9D 04 02 */    sta SpriteTable+4,X
/* 1FA2A: CA       */    dex
/* 1FA2B: CA       */    dex
/* 1FA2C: CA       */    dex
/* 1FA2D: CA       */    dex
/* 1FA2E: 88       */    dey
/* 1FA2F: 10 E4    */    bpl L1FA15

L1FA31:
.else
/* 1F98F: A2 06 */       ldx #$06
L1F991: ; --
/* 1F991: BD CF 8E */    lda L10ECF,x
/* 1F994: 8D 06 20 */    sta $2006
/* 1F997: BD D6 8E */    lda L10ED6,x
/* 1F99A: 8D 06 20 */    sta $2006
/* 1F99D: 86 0C */       stx $0C
/* 1F99F: BD DD 8E */    lda L10EDD,x
/* 1F9A2: AA */          tax
L1F9A3: ; -
/* 1F9A3: 8C 07 20 */    sty $2007
/* 1F9A6: C8 */          iny
/* 1F9A7: CA */          dex
/* 1F9A8: D0 F9 */       bne L1F9A3 ; -
/* 1F9AA: A6 0C */       ldx $0C
/* 1F9AC: CA */          dex
/* 1F9AD: 10 E2 */       bpl L1F991 ; --
/* 1F9AF: A9 23 */       lda #$23  ;mem addr $23DE
/* 1F9B1: 8D 06 20 */    sta $2006
/* 1F9B4: A9 DE */       lda #$DE
/* 1F9B6: 8D 06 20 */    sta $2006
/* 1F9B9: A9 A0 */       lda #$A0
/* 1F9BB: 8D 07 20 */    sta $2007 ;write $A0
/* 1F9BE: A9 21 */       lda #$21
/* 1F9C0: 8D 06 20 */    sta $2006 ;mem addr $21D9
/* 1F9C3: A9 D9 */       lda #$D9
/* 1F9C5: 8D 06 20 */    sta $2006
/* 1F9C8: A9 94 */       lda #$94
/* 1F9CA: 8D 07 20 */    sta $2007 ;write $94, $8D
/* 1F9CD: A9 8D */       lda #$8D
/* 1F9CF: 8D 07 20 */    sta $2007
/* 1F9D2: E8 */          inx
/* 1F9D3: A9 04 */       lda #$04
/* 1F9D5: 85 0C */       sta $0C
L1F9D7: ; --
/* 1F9D7: BD E4 8E */    lda L10EE4,x
/* 1F9DA: 8D 06 20 */    sta $2006
/* 1F9DD: E8 */          inx
/* 1F9DE: BD E4 8E */    lda L10EE4,x
/* 1F9E1: 8D 06 20 */    sta $2006
/* 1F9E4: E8 */          inx
/* 1F9E5: BC E4 8E */    ldy L10EE4,x
/* 1F9E8: E8 */          inx
L1F9E9: ; -
/* 1F9E9: BD E4 8E */    lda L10EE4,x
/* 1F9EC: 8D 07 20 */    sta $2007

/* 1F9EF: E8 */          inx
/* 1F9F0: 88 */          dey
/* 1F9F1: D0 F6 */       bne L1F9E9 ; -
/* 1F9F3: C6 0C */       dec $0C
/* 1F9F5: D0 E0 */       bne L1F9D7 ; --

 ; Graphics setup completed. Wait for input.
WaitForStartScreenInput: ; ----
/* 1F9F7: A0 00 */       ldy #$00
/* 1F9F9: 84 BC */       sty IgnoreStageSelection
/* 1F9FB: A2 40 */       ldx #$40
/* 1F9FD: 20 78 D4 */    jsr HideSprites
.endif

/* 1FA00: A5 FF */       lda PPU2000value
/* 1FA02: 09 80 */       ora #$80
/* 1FA04: 85 FF */       sta PPU2000value
/* 1FA06: 8D 00 20 */    sta $2000
/* 1FA09: A5 A6 */       lda ExtraLives
/* 1FA0B: 30 21 */       bmi L1FA2E ; +

 ; Read input for logo screen.
L1FA0D: ; -
/* 1FA0D: 20 1B C0 */    jsr NextFrame
/* 1FA10: A5 14 */       lda JoyPad0
/* 1FA12: C9 08 */       cmp #$08
/* 1FA14: D0 F7 */       bne L1FA0D ; -

.ifdef J_VERSION
/* 1FA47: A9 1C */       lda #$1C
/* 1FA49: 20 77 C4 */    jsr IssueSound
.endif

/* 1FA16: 20 4F C7 */    jsr PaletteSetupForBGwith3F0
/* 1FA19: A9 78 */       lda #$78
/* 1FA1B: 85 37 */       sta PaletteUpdateDelay
/* 1FA1D: A9 80 */       lda #$80
/* 1FA1F: 85 3C */       sta MiscCounter1
L1FA21: ; -
/* 1FA21: 20 1B C0 */    jsr NextFrame
/* 1FA24: C6 3C */       dec $3C
/* 1FA26: D0 F9 */       bne L1FA21 ; -
L1FA28: ; ---
/* 1FA28: 20 95 D4 */    jsr DisableNMIandPPU
/* 1FA2B: 4C B3 C3 */    jmp SwitchBank05

 ; Read input for "game over" screen
L1FA2E: ; +
/* 1FA2E: A2 03 */       ldx #$03
L1FA30: ; -
/* 1FA30: BD 58 FA */    lda FA57_table+1,x
/* 1FA33: 9D 04 02 */    sta CurrentSpriteData+0,x
/* 1FA36: CA */          dex
/* 1FA37: 10 F7 */       bpl L1FA30 ; -
/* 1FA39: A2 01 */       ldx #$01
L1FA3B: ; --
/* 1FA3B: 86 BC */       stx IgnoreStageSelection
L1FA3D: ; -
/* 1FA3D: 20 1B C0 */    jsr NextFrame
/* 1FA40: A5 18 */       lda JoyD0
/* 1FA42: 29 0C */       and #$0C
/* 1FA44: F0 F7 */       beq L1FA3D ; -
/* 1FA46: 29 08 */       and #$08
/* 1FA48: D0 DE */       bne L1FA28 ; ---
/* 1FA4A: A5 BC */       lda IgnoreStageSelection
/* 1FA4C: 49 01 */       eor #$01
/* 1FA4E: AA */          tax
/* 1FA4F: BD 57 FA */    lda FA57_table,x
/* 1FA52: 8D 04 02 */    sta CurrentSpriteData
/* 1FA55: D0 E4 */       bne L1FA3B ; --

; Data for the arrow cursor.

FA57_table:
    .byte $90,$80 ; Y position for "new game" and "continue"
        .byte $DA     ; Tile numer probably
        .byte $01     ; Enabled or palette or something like that
        .byte $50     ; X position probably

L1FA5C: ; ++

.ifdef J_VERSION
/* 1FA92: A9 FF */       lda #$FF
.else
/* 1FA5C: A9 AA */       lda #$AA
.endif

/* 1FA5E: A2 40 */       ldx #$40
L1FA60: ; -
/* 1FA60: 8D 07 20 */    sta $2007
/* 1FA63: CA */          dex
/* 1FA64: D0 FA */       bne L1FA60 ; -
/* 1FA66: A9 11 */       lda #$11

.ifdef J_VERSION
/* 1FA9E: 8D DF 03 */    sta $03DF
/* 1FAA1: 20 3F C7 */    jsr PaletteSetupForBG
/* 1FAA4: 20 0A D6 */    jsr UpdatePalettes
.else
/* 1FA68: 8D DB 03 */    sta $03DB
/* 1FA6B: 20 3F C7 */    jsr PaletteSetupForBG
/* 1FA6E: 20 0A D6 */    jsr UpdatePalettes
/* 1FA71: A2 00 */       ldx #$00
 .endif

/*
Contents of the data for game over screen:
(located at 0EA1 in bank 4):

0000 word nametable address (ex. 212C)
0002 byte number of bytes (ex. 09)
0003 data                 (ex. "GAME OVER")
Total number of bytes is $26 (hardcoded at $FA8F).
*/

L1FA73: ; --
/* 1FA73: BD A1 8E */    lda L10EA1,x
/* 1FA76: 8D 06 20 */    sta $2006
/* 1FA79: E8 */          inx
/* 1FA7A: BD A1 8E */    lda L10EA1,x
/* 1FA7D: 8D 06 20 */    sta $2006
/* 1FA80: E8 */          inx
/* 1FA81: BC A1 8E */    ldy L10EA1,x
/* 1FA84: E8 */          inx
L1FA85: ; -
/* 1FA85: BD A1 8E */    lda L10EA1,x
/* 1FA88: 8D 07 20 */    sta $2007
/* 1FA8B: E8 */          inx
/* 1FA8C: 88 */          dey
/* 1FA8D: D0 F6 */       bne L1FA85 ; -
/* 1FA8F: E0 26 */       cpx #$26
/* 1FA91: D0 E0 */       bne L1FA73 ; --
/* 1FA93: A9 21 */       lda #$21
/* 1FA95: 8D 06 20 */    sta $2006
/* 1FA98: A9 6D */       lda #$6D
/* 1FA9A: 8D 06 20 */    sta $2006
/* 1FA9D: A2 06 */       ldx #$06
L1FA9F: ; -
/* 1FA9F: B5 72 */       lda Score,x
/* 1FAA1: 09 D0 */       ora #$D0
/* 1FAA3: 8D 07 20 */    sta $2007
/* 1FAA6: CA */          dex
/* 1FAA7: 10 F6 */       bpl L1FA9F ; -
/* 1FAA9: 84 1A */       sty $1A
/* 1FAAB: 84 1B */       sty $1B
/* 1FAAD: A9 03 */       lda #$03        ;game over-music
/* 1FAAF: 20 77 C4 */    jsr IssueSound
/* 1FAB2: 4C F7 F9 */    jmp WaitForStartScreenInput ; ----

TableObjectXWidthTable2: ; at FAB5 / FAE9 J
    .byte $08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08
    .byte $08,$08,$08,$08,$08,$08,$08,$08,$08,$00,$04,$08,$08,$08,$08,$08
    .byte $08,$08,$08,$08,$08,$10,$10,$08,$08,$08,$08,$08,$08,$10,$10,$10
    .byte $10,$10,$0E,$08,$08,$08,$08,$08,$08,$08,$04,$08,$08,$08,$08,$08
    .byte $08,$08,$08,$08,$08,$06,$08,$08,$08,$08,$08,$08,$04,$08,$04,$04
    .byte $04,$08,$08,$04,$08,$04,$04,$08,$08,$08,$08,$08,$08,$08,$08,$08
    .byte $08,$08,$04,$08,$04,$04,$04,$08,$08,$04,$08,$08,$10,$04,$08,$08
    .byte $08,$18,$18,$18,$18,$18,$18,$06,$06,$10,$08,$04,$03,$01,$0A,$04
    .byte $0B,$01,$04
TableObjectXWidthTable1: ;at FB38 / FB6C J
    .byte $08,$08,$08,$08,$07,$08,$08,$0A,$06,$0C,$08,$02,$1E,$08,$07,$06
    .byte $08,$08,$08,$08,$08,$09,$08,$02,$04,$02,$01,$08,$02,$0C,$07,$0C
    .byte $04,$07,$02,$02,$1E,$02,$04,$08,$08,$08,$08,$0C,$01,$01,$06,$01
    .byte $01,$06,$01,$06,$06,$08,$07,$07,$07,$03,$1E,$01,$04,$06,$06,$04
    .byte $04,$04,$04,$02,$08,$08,$04,$04,$06,$04,$09
TableObjectYHeightTable2: ;at FB83 / FBB7 J
    .byte $0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C
    .byte $0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$00,$04,$0C,$0C,$0C,$0C,$0C
    .byte $0C,$0C,$0C,$0C,$0C,$10,$10,$08,$0C,$0C,$0C,$0C,$0C,$10,$10,$10
    .byte $10,$10,$0E,$0C,$0C,$0C,$10,$10,$08,$08,$04,$0C,$0C,$0C,$0C,$0C
    .byte $0C,$0C,$0C,$0C,$0C,$06,$0C,$0C,$0C,$0C,$0C,$0C,$08,$04,$08,$08
    .byte $08,$04,$04,$08,$08,$04,$04,$0C,$0C,$0C,$08,$08,$08,$08,$08,$08
    .byte $08,$08,$08,$04,$08,$08,$08,$04,$04,$08,$10,$10,$10,$04,$0C,$0C
    .byte $0C,$01,$01,$01,$01,$01,$01,$10,$10,$08,$04,$04,$04,$01,$0A,$0C
    .byte $31,$01,$0C
TableObjectYHeightTable1: ;at FC06 / FC3A J
    .byte $08,$08,$08,$04,$07,$08,$0C,$08,$08,$14,$09,$08,$06,$08,$04,$1E
    .byte $08,$08,$0C,$08,$10,$0C,$08,$08,$04,$08,$02,$08,$02,$04,$08,$07
    .byte $04,$07,$08,$08,$06,$08,$04,$08,$08,$08,$18,$04,$01,$01,$06,$01
    .byte $01,$06,$01,$06,$06,$08,$07,$07,$07,$28,$1E,$01,$04,$06,$06,$08
    .byte $08,$08,$08,$10,$08,$08,$04,$04,$06,$04,$09
WeaponDamageP: ;at FC51 / FC85 J
    .byte $14,$14,$14,$00,$04,$07,$14,$14,$14,$01,$14,$14,$14,$00,$14,$00
    .byte $14,$04,$02,$00,$07,$02,$07,$14,$14,$14,$00,$00,$02,$14,$07,$14
    .byte $00,$04,$14,$14,$14,$14,$00,$00,$00,$04,$14,$00,$00,$00,$00,$00
    .byte $00,$14,$00,$00,$00,$00,$00,$00,$00,$00,$02
WeaponDamageC: ;at FC8C / FCC0 J
    .byte $14,$14,$14,$14,$14,$14,$14,$14,$14,$01,$14,$14,$14,$00,$14,$00
    .byte $14,$04,$04,$00,$07,$02,$14,$14,$14,$14,$00,$00,$02,$14,$07,$14
    .byte $00,$14,$14,$14,$14,$14,$00,$00,$00,$04,$14,$00,$00,$00,$00,$00
    .byte $00,$14,$00,$00,$00,$00,$00,$00,$00,$00,$02
WeaponDamageI:  ;at FCC7 / FCFB J
    .res $3b, 0
WeaponDamageB:  ;at FD02 / FD36 J
    .byte $14,$14,$14,$14,$14,$14,$14,$14,$14,$04,$14,$14,$14,$00,$14,$00
    .byte $14,$14,$14,$00,$14,$14,$14,$14,$14,$14,$00,$00,$14,$14,$14,$14
    .byte $00,$14,$14,$14,$14,$14,$00,$00,$00,$14,$14,$00,$00,$00,$00,$00
    .byte $00,$14,$00,$00,$00,$00,$00,$00,$00,$00,$07
WeaponDamageF:  ;at FD3D / FD71 J
    .byte $14,$14,$14,$14,$14,$14,$14,$14,$14,$01,$14,$14,$14,$00,$14,$00
    .byte $14,$04,$04,$00,$14,$14,$14,$14,$14,$14,$00,$00,$02,$14,$14,$14
    .byte $00,$14,$14,$14,$14,$14,$00,$00,$00,$14,$14,$00,$00,$00,$00,$00
    .byte $00,$14,$00,$00,$00,$00,$00,$00,$00,$00,$02
WeaponDamageE:  ;at FD78 / FDAC J
    .byte $14,$14,$14,$14,$14,$14,$14,$14,$14,$04,$14,$14,$14,$00,$14,$00
    .byte $14,$02,$07,$00,$14,$04,$14,$14,$14,$14,$00,$00,$01,$14,$14,$14
    .byte $00,$14,$14,$14,$14,$14,$00,$00,$00,$14,$14,$00,$00,$00,$00,$00
    .byte $00,$14,$00,$00,$00,$00,$00,$00,$00,$00,$04
WeaponDamageG:  ;at FDB3 / FDE7 J
    .byte $14,$14,$14,$14,$14,$14,$14,$14,$14,$07,$14,$14,$14,$00,$14,$00
    .byte $14,$14,$14,$00,$14,$14,$14,$14,$14,$14,$00,$00,$14,$14,$14,$14
    .byte $00,$14,$14,$14,$14,$14,$00,$00,$00,$14,$14,$00,$00,$00,$00,$00
    .byte $00,$14,$00,$00,$00,$00,$00,$00,$00,$00,$14


; 0 = "P" (just the old megablaster)
; 1 = Cutman
; 2 = Iceman
; 3 = Bombman
; 4 = Fireman
; 5 = Elecman
; 6 = Gutsman
; 7 = Magnet beam


WeaponDamageOnBoss:                                                  ; $FE22
    ;      P C I B F E G  M
; Cutman
    .byte  3,1,0,2,3,1,14,0
; Iceman
    .byte  1,2,0,4,1,10,0,0
; Bombman
    .byte  2,2,0,1,4,2,0,0
; Fireman
    .byte  2,2,4,1,1,1,0,0
; Elecman
    .byte  1,10,0,2,1,1,4,0
; Gutsman
    .byte  2,1,0,10,2,1,1,0
; Stone giant
    .byte  2,2,0,0,2,4,0,0
; Wily2
    .byte  1,1,0,2,2,2,0,0
; Wily3
    .byte  0,0,0,0,0,0,0,0
; Wily4
    .byte  1,1,1,1,4,1,1,0
    .byte  1,1,0,1,1,1,1,0
    .byte  0,0,0,0,0,0,0,0

EnemyKillScoreTable: ;at $FE82 / FE4E U
    .byte  $5, $5, $3, $2, $3, $5, $8, $5
    .byte  $8,$90, $8, $2, $3, $0, $3, $5
    .byte  $8,$15,$50, $0, $5,$15, $5, $2
    .byte  $3, $3, $0, $0,$15, $5, $5, $5
    .byte  $0, $3, $2, $2, $3, $3, $0, $8
    .byte  $0, $5, $4, $0, $0, $0, $0, $0
    .byte  $0, $2, $0, $0, $0, $0, $0, $0
    .byte  $0, $0, $0

EnemyHitTable: ;at $FEBD / FE89 U
    .byte  3, 1, 2, 3, 4, 1, 4, 4
    .byte  3,10, 1, 1, 4, 1, 3, 3
    .byte  4, 4, 4, 3, 1, 3, 2, 1
    .byte  3, 4, 2, 1, 4, 3, 3, 3
    .byte  3, 4, 1, 1, 4, 4, 1, 4
    .byte  3, 3, 3, 3, 0, 2, 2, 0
    .byte  0, 2
	
L1FEEF: ;at $FEEF / $FEBB U
	.byte  4,10, 4, 4,10, 4, 4, 4
    .byte  4, 4, 4
	
.ifdef J_VERSION
	.res 6, $FF
	
	.res $E0, 0
	
	; Nintendo header
	.byte "         ROCKMAN"
	.dbyt $27B9
.else
	; Extra entries from indices 3d-70
    .byte 10, 1, 1, 4, 1, 3, 3, 4
    .byte  4, 4, 3, 1, 3, 2, 1, 3
    .byte  4, 2, 1, 4, 3, 3, 3, 3
    .byte  4, 1, 1, 4, 4, 1, 4, 3
    .byte  3, 3, 3, 0, 2, 2, 0, 0
    .byte  2, 4,10, 4, 4,10, 4, 4
    .byte  4, 4, 4, 4
	.res 6, $ff
	
	.res $e0, 0
	
	.byte "         MEGAMAN"
	.dbyt $872B
.endif

	; Nintendo header + $12
	.dbyt 0
	.byte (3 << 4) | (1 << 3) | 0
	.byte (0 << 7) | 2
	.byte $01
	.byte $06
	.byte $08
	.byte $B7
	.word NMI, Reset, Reset
