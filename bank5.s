.include "globals.inc"

/*
	MEGAMAN 1 BANK 5 ROM MAP

	This bank's disassembly is based on Rockman (J)
*/

.segment "BANK5"

inc_bank_part 5, 0, $1000

Reset2:
/* 15000: A2 FF */    LDX #$ff    ; Reinitialize stack pointer. Clean start!
/* 15002: 9A */       TXS

; Wait two frames
/* 15003: A2 01 */    LDX #$01
; --
L15005: ; -
/* 15005: AD 02 20 */ LDA $2002
/* 15008: 10 FB */    BPL L15005 ; -                                               ; $9005
L1500A: ; -
/* 1500A: AD 02 20 */ LDA $2002
/* 1500D: 30 FB */    BMI L1500A ; -                                               ; $900A


/* 1500F: CA */       DEX
/* 15010: 10 F3 */    BPL L15005 ; --                                              ; $9005

/* 15012: 9A */       TXS


; Clear zeropage RAM (can be optimized down)
/* 15013: A9 00 */    LDA #$00
/* 15015: A0 00 */    LDY #$00
L15017: ; -
/* 15017: 99 00 00 */ STA $0000,Y
/* 1501A: C8 */       INY
/* 1501B: D0 FA */    BNE L15017 ; -                                               ; $9017

; Clear rest of CPU-RAM

/* 1501D: A9 01 */    LDA #$01
/* 1501F: 85 05 */    STA $05
L15021: ; --
/* 15021: A9 00 */    LDA #$00
L15023: ; -
/* 15023: 91 04 */    STA ($04),Y
/* 15025: C8 */       INY
/* 15026: D0 FB */    BNE L15023 ; -                                               ; $9023
/* 15028: E6 05 */    INC $05
/* 1502A: A5 05 */    LDA $05
/* 1502C: C9 08 */    CMP #$08
/* 1502E: D0 F1 */    BNE L15021 ; --                                              ; $9021


/* 15030: A9 10 */    LDA #$10    ; BG chartable = $1000, SPR chartable = $0000
/* 15032: 85 FF */    STA PPU2000value
/* 15034: A9 06 */    LDA #$06    ; No clipping
/* 15036: 85 FE */    STA PPU2001value

; Game over
GameOver:
/* 15038: A5 31 */    LDA CurrentStage
/* 1503A: 85 AC */    STA FightingBossNum         ;backup the stage number

; Draw title screen
/* 1503C: A9 0A */    LDA #$0A
/* 1503E: 85 31 */    STA CurrentStage
/* 15040: 20 5B C7 */ JSR WriteChr                                        ; $C75B

; Set palette
/* 15043: 20 83 C4 */ JSR InitStagePaletteAndActives

; This also handles title screen code (wait until START pressed)
/* 15046: 20 56 F9 */ JSR DrawAtReset                                     ; $F956


; Clear score (and something else?...)

/* 15049: A9 00 */    LDA #$00
/* 1504B: A2 07 */    LDX #$07
L1504D: ; -
/* 1504D: 95 72 */    STA Score,X
/* 1504F: CA */       DEX
/* 15050: 10 FB */    BPL L1504D ; -                                               ; $904D

/* 15052: A9 02 */    LDA #$02
/* 15054: 85 A6 */    STA ExtraLives


/* 15056: A5 AC */    LDA FightingBossNum
/* 15058: 85 31 */    STA CurrentStage


; Set all meters to max
;

; Launch the stage selection screen
L1505A:
/* 1505A: A9 1C */    LDA #$1c
/* 1505C: A2 07 */    LDX #$07
L1505E: ; -
/* 1505E: 95 6A */    STA Meters,X
/* 15060: CA */       DEX
/* 15061: 10 FB */    BPL L1505E ; -                                               ; $905E

/* 15063: A5 BC */    LDA IgnoreStageSelection
/* 15065: D0 03 */    BNE L1506A ; +                                               ; $906A
/* 15067: 20 46 D8 */ JSR ExecStageSelectionScreen
L1506A: ; +

/* 1506A: A6 31 */    LDX CurrentStage
/* 1506C: E0 06 */    CPX #$06
/* 1506E: 90 05 */    BCC L15075 ; +                                               ; $9075
; For Wily stages, the score is selected manually from this table.
/* 15070: BD 24 94 */ LDA WilyStageClearScoreTable-6,X      ;table contents (from 6+): $64,$46,$C8,$C8
/* 15073: 85 3D */    STA LevelClearScoreInThousands
L15075: ; +
/* 15075: A9 00 */    LDA #$00
/* 15077: 85 AE */    STA BonusPearlCount
/* 15079: 85 AB */    STA LastRestartPointType
/* 1507B: 20 5B C7 */ JSR WriteChr                                        ; $C75B

StageBegin:
/* 1507E: 20 83 C4 */ JSR InitStagePaletteAndActives

StageBeginFromDeath:
/* 15081: 18 */       CLC
/* 15082: A5 AB */    LDA LastRestartPointType
/* 15084: 65 31 */    ADC CurrentStage
/* 15086: 48 */       PHA
/* 15087: AA */        TAX
/* 15088: BD EB C2 */  LDA FirstScreenScreenTable,X
/* 1508B: 85 27 */     STA CurrentBeginScreen
/* 1508D: 48 */        PHA
/* 1508E: A5 AB */     LDA LastRestartPointType
/* 15090: F0 05 */     BEQ L15097 ; +
/* 15092: BD EE 93 */  LDA Label93FA-12,X
/* 15095: 85 27 */     STA CurrentBeginScreen
L15097: ; +
/* 15097: A5 27 */     LDA CurrentBeginScreen
/* 15099: 18 */        CLC
/* 1509A: 69 01 */     ADC #$01
/* 1509C: 85 1B */     STA ScrollPosScreen
/* 1509E: 20 D8 A0 */  JSR F160D8
/* 150A1: 68 */       PLA
/* 150A2: 8D 60 04 */ STA ObjectPosScreen+0                             ; $0460
/* 150A5: 20 D8 A0 */ JSR F160D8
/* 150A8: 68 */       PLA
/* 150A9: AA */       TAX
/* 150AA: BD C3 93 */ LDA FirstScreenEnemyPointer,X
/* 150AD: 85 8B */    STA PreviousEnemyIndex
/* 150AF: 18 */       CLC
/* 150B0: 69 01 */    ADC #$01
/* 150B2: 85 8C */    STA CurrentEnemyIndex
/* 150B4: BD A0 93 */ LDA FirstScreenOrderTable,X
/* 150B7: 85 29 */    STA CurrentOrderNum
/* 150B9: A8 */       TAY
/* 150BA: 20 13 C6 */ JSR RoomLayoutLoadRoomNum
/* 150BD: 29 1F */    AND #$1f
/* 150BF: 18 */       CLC
/* 150C0: 65 27 */    ADC CurrentBeginScreen
/* 150C2: 85 28 */    STA CurrentEndScreen
/* 150C4: AD 60 04 */ LDA ObjectPosScreen+0                             ; $0460
/* 150C7: 85 1B */    STA ScrollPosScreen
/* 150C9: A9 00 */    LDA #$00
/* 150CB: 85 1A */    STA ScrollPosX
/* 150CD: 85 5F */    STA WeaponSelect                                    ; $5F
/* 150CF: 85 3E */    STA BossCurrentStrategy
/* 150D1: 85 8F */    STA ZigZagFireStatus
/* 150D3: 85 98 */    STA $98
/* 150D5: 85 99 */    STA $99
/* 150D7: 8D C1 06 */ STA ObjectLifeMeter+1

/* 150DA: A2 0F */    LDX #$0f
L150DC: ; -
/* 150DC: 95 7B */    STA $7B,X
/* 150DE: 9D A0 05 */ STA MagnetBeamLength,X
/* 150E1: 9D B0 05 */ STA MagnetBeamAge,X
/* 150E4: CA */       DEX
/* 150E5: 10 F5 */    BPL L150DC ; -                                               ; $90DC

/* 150E7: A5 AB */    LDA LastRestartPointType
/* 150E9: F0 0B */    BEQ L150F6 ; +                                               ; $90F6
/* 150EB: 18 */       CLC
/* 150EC: 65 31 */    ADC CurrentStage
/* 150EE: AA */       TAX
/* 150EF: BD 06 94 */ LDA Label9412-12,X
/* 150F2: 85 28 */    STA CurrentEndScreen
/* 150F4: E0 08 */    CPX #$08
L150F6: ; +
/* 150F6: A0 00 */    LDY #$00
/* 150F8: A2 40 */    LDX #$40
/* 150FA: 20 78 D4 */ JSR HideSprites                                     ; $D478
/* 150FD: A9 20 */    LDA #$20
/* 150FF: 85 54 */    STA TotalObjects
/* 15101: 20 05 A1 */ JSR ForgetRoomObjects

; Start playing stage music?
/* 15104: A6 31 */    LDX CurrentStage
/* 15106: BD 94 93 */ LDA StageMusicTable,X
/* 15109: 20 77 C4 */ JSR IssueSound                                      ; $C477

; Megaman's coordinate when he starts a stage!
/* 1510C: A9 80 */    LDA #$80
/* 1510E: 8D 80 04 */ STA ObjectPosX+0
/* 15111: 85 22 */    STA $22

/* 15113: A9 04 */    LDA #$04
/* 15115: 8D 00 06 */ STA  ObjectPosY+0


; Enable NMI and let it start playing sound...
/* 15118: A5 FF */    LDA PPU2000value
/* 1511A: 09 80 */    ORA #$80
/* 1511C: 85 FF */    STA PPU2000value
/* 1511E: 8D 00 20 */ STA $2000
/* 15121: 85 1F */    STA NMI_GfxUpdateDone

/* 15123: A5 31 */    LDA CurrentStage
/* 15125: C9 0B */    CMP #$0b
/* 15127: D0 03 */    BNE L1512C ; +                                               ; $912C
/* 15129: 4C 26 C4 */ JMP InitEndGameScene


L1512C: ; +
/* 1512C: A9 1C */    LDA #$1C        ; Set lifemeter to max
/* 1512E: 85 6A */    STA Meters+0



; "READY" printing routine
/* 15130: A2 13 */    LDX #$13        ; Write "READY" text to sprites 1-5
L15132: ; -
/* 15132: BD E6 93 */ LDA READY_TEXT,X                                    ; $93E6,X
/* 15135: 9D 04 02 */ STA CurrentSpriteData+0,X
/* 15138: CA */       DEX
/* 15139: 10 F7 */    BPL L15132 ; -                                               ; $9132



; Show "READY" message for 3 seconds
/* 1513B: A9 C0 */    LDA #$c0
/* 1513D: 85 3C */    STA MiscCounter1
L1513F: ; -
/* 1513F: 20 1B C0 */ JSR NextFrame                                      ; $C01B
/* 15142: C6 3C */    DEC MiscCounter1
/* 15144: D0 F9 */    BNE L1513F ; -                                               ; $913F

/* 15146: A0 00 */    LDY #$00
/* 15148: 84 55 */    STY MegamanBlinkState
/* 1514A: 8C 40 06 */ STY ObjectFireDelay+0
/* 1514D: A2 40 */    LDX #$40
/* 1514F: 20 78 D4 */ JSR HideSprites                                     ; $D478
/* 15152: 20 E0 C4 */ JSR TeleportToStage                                 ; $C4E0

/* 15155: A9 01 */    LDA #$01
/* 15157: 85 8D */    STA ScreenMovedFlag

/* 15159: A9 00 */    LDA #$00    ; Make Megaman stand still at start
/* 1515B: 8D 00 04 */ STA ObjectSpriteNum+0

MainLoop:
; Did we pick up a capsule in the last frame?
; Both life and weapon energy capsules
L1515E:
/* 1515E: A5 AD */    LDA CapsuleObtained
/* 15160: F0 03 */    BEQ L15165 ; +                                               ; $9165
/* 15162: 20 53 94 */ JSR MeterRefill                                     ; $9453
L15165: ; +

/* 15165: A5 18 */    LDA JoyD
/* 15167: 29 0C */    AND #$0c    ; Has SELECT or START been pushed?
/* 15169: F0 1C */    BEQ SelectOrStartNotPushed                          ; $9187



/* 1516B: 29 04 */    AND #$04
/* 1516D: D0 60 */    BNE SelectPushed                                    ; $91CF

StartPushed:
/* 1516F: A5 60 */    LDA WeaponFiring    ; We can't access the ws-dlg if shots
                                    ; from the current weapon are on screen
/* 15171: D0 0D */    BNE L15180 ; +                                               ; $9180
/* 15173: 20 BF C3 */ JSR InvokeWeaponSelectDialog                        ; $C3BF

; Make Megaman transform
ResumeFromPause:
/* 15176: AD 20 04 */ LDA ObjectFlags+0
/* 15179: 29 F0 */    AND #$f0
/* 1517B: 09 01 */    ORA #$01
/* 1517D: 8D 20 04 */ STA ObjectFlags+0
L15180: ; +

; Make Megaman stand still
/* 15180: A9 00 */    LDA #$00
/* 15182: 85 41 */    STA $41
/* 15184: 8D 00 04 */ STA ObjectSpriteNum+0

SelectOrStartNotPushed:
/* 15187: 20 72 CD */ JSR RecalculateActivesLowerIndex
/* 1518A: 20 A8 94 */ JSR MegamanAI
/* 1518D: A5 94 */    LDA MegamanWalkTimer
/* 1518F: F0 09 */    BEQ L1519A ; +
/* 15191: A5 B1 */    LDA $B1
/* 15193: D0 05 */    BNE L1519A ; +                                               ; $919A
/* 15195: A9 2B */    LDA #$2B        ; Some kind of shooting SFX
/* 15197: 20 77 C4 */ JSR IssueSound                                      ; $C477
L1519A: ; +
/* 1519A: A5 94 */    LDA MegamanWalkTimer
/* 1519C: 85 B1 */    STA $B1

/* 1519E: AD 20 04 */ LDA ObjectFlags+0
/* 151A1: 29 0F */    AND #$0f
/* 151A3: C9 01 */    CMP #$01
/* 151A5: F0 15 */    BEQ L151BC ; ++                                              ; $91BC

; Do this part UNLESS Megaman is transforming
/* 151A7: 20 5C A3 */ JSR CheckHoldingBkey
/* 151AA: 20 8C DB */ JSR RunBossAI

/* 151AD: A5 55 */    LDA MegamanBlinkState
/* 151AF: F0 02 */    BEQ L151B3 ; +                                               ; $91B3
/* 151B1: C6 55 */    DEC MegamanBlinkState
L151B3: ; +
/* 151B3: 20 8B A1 */ JSR RunCollisionChecks
/* 151B6: 20 61 A2 */ JSR RunWeaponAI
/* 151B9: 20 EA 98 */ JSR RunEnemyAI                                       ; $98EA

L151BC: ; ++
/* 151BC: 20 96 D8 */ JSR LoadEnemies
/* 151BF: 20 31 D1 */ JSR UpdateGraphics                                  ; $D131
/* 151C2: 20 6A D4 */ JSR LifeCycleUntick_forEveryone

/* 151C5: A5 26 */    LDA CurrentStripeEndType
/* 151C7: D0 3E */    BNE CheckStripeEnding
EndCurrentFrame:
/* 151C9: 20 1B C0 */ JSR NextFrame                                      ; $C01B
/* 151CC: 4C 5E 91 */ JMP MainLoop                                        ; $915E


SelectPushed:
/* 151CF: A9 22 */    LDA #$22        ; Pause jingle
/* 151D1: 20 77 C4 */ JSR IssueSound                                      ; $C477

/* 151D4: A9 01 */    LDA #$01
/* 151D6: 85 41 */    STA $41

; Save GutsmanStompCounter,MegamanWalkTimer,MegamanStallTimer to stack
/* 151D8: A5 47 */    LDA GutsmanStompCounter
/* 151DA: 48 */       PHA
/* 151DB: A5 94 */    LDA MegamanWalkTimer
/* 151DD: 48 */       PHA
/* 151DE: A5 95 */    LDA MegamanStallTimer
/* 151E0: 48 */       PHA

; Clear GutsmanStompCounter,MegamanWalkTimer,MegamanStallTimer
/* 151E1: A9 00 */    LDA #$00
/* 151E3: 85 47 */    STA GutsmanStompCounter
/* 151E5: 85 94 */    STA MegamanWalkTimer
/* 151E7: 85 95 */    STA MegamanStallTimer

; Loop until SELECT or START are pushed
L151E9: ; -
/* 151E9: 20 98 94 */ JSR PauseFrameTick
/* 151EC: A5 18 */    LDA JoyD0
/* 151EE: 29 0C */    AND #$0c
/* 151F0: F0 F7 */    BEQ L151E9 ; -                                               ; $91E9

; Restore GutsmanStompCounter,MegamanWalkTimer,MegamanStallTimer from stack
/* 151F2: 68 */       PLA
/* 151F3: 85 95 */    STA MegamanStallTimer
/* 151F5: 68 */       PLA
/* 151F6: 85 94 */    STA MegamanWalkTimer
/* 151F8: 68 */       PLA
/* 151F9: 85 47 */    STA GutsmanStompCounter

/* 151FB: A5 18 */    LDA JoyD0
/* 151FD: 29 04 */    AND #$04
/* 151FF: F0 03 */    BEQ L15204 ; +                                               ; $9204
/* 15201: 4C 76 91 */ JMP ResumeFromPause
L15204: ; +
/* 15204: 4C 6F 91 */ JMP StartPushed                                     ; $916F


CheckStripeEnding:
/* 15207: C9 01 */    CMP #$01
/* 15209: D0 10 */    BNE L1521B
; Teleport check is only done in rooms that scroll up.
/* 1520B: A5 B0 */    LDA TeleportEnteredFlag
/* 1520D: F0 03 */    BEQ L15212 ; +                                               ; $9212
/* 1520F: 20 2F 94 */ JSR EnterTeleport
L15212: ; +
/* 15212: AD 20 04 */ LDA ObjectFlags+0
/* 15215: 29 10 */    AND #$10
/* 15217: F0 29 */    BEQ L15242 ; ++                                              ; $9242
; In ladder
/* 15219: A5 26 */    LDA CurrentStripeEndType
L1521B:
/* 1521B: A6 1A */    LDX ScrollPosX
/* 1521D: D0 23 */    BNE L15242 ; ++                                              ; $9242
/* 1521F: A6 1B */    LDX ScrollPosScreen
/* 15221: E4 27 */    CPX CurrentBeginScreen
/* 15223: D0 0D */    BNE L15232 ; +
/* 15225: A4 29 */    LDY CurrentOrderNum
/* 15227: 88 */       DEY
/* 15228: 20 13 C6 */ JSR RoomLayoutLoadRoomNum
/* 1522B: A4 26 */    LDY CurrentStripeEndType
/* 1522D: 39 8B 93 */ AND bank5_938B_table,Y
/* 15230: D0 29 */    BNE ScrollPreviousRoom
L15232: ; +
/* 15232: E4 28 */    CPX CurrentEndScreen
/* 15234: D0 0C */    BNE L15242 ; ++                                              ; $9242
/* 15236: A4 29 */    LDY CurrentOrderNum
/* 15238: 20 13 C6 */ JSR RoomLayoutLoadRoomNum
/* 1523B: A4 26 */    LDY CurrentStripeEndType
/* 1523D: 39 8F 93 */ AND bank5_938F_table,Y
/* 15240: D0 64 */    BNE ScrollNextRoom
L15242: ; ++
; Not in ladder, or scrolling position is not even (Xlo != 0)
/* 15242: A5 26 */    LDA CurrentStripeEndType
/* 15244: C9 03 */    CMP #$03
/* 15246: D0 0C */    BNE L15254
/* 15248: A9 00 */    LDA #$00
/* 1524A: 85 26 */    STA CurrentStripeEndType
/* 1524C: A9 F8 */    LDA #$f8
/* 1524E: 8D 00 06 */ STA  ObjectPosY+0
/* 15251: 4C 19 C2 */ JMP MegaManKilled                                   ; $C219




L15254: ; - ;part of ScrollPreviousRoom
/* 15254: A9 00 */    LDA #$00
/* 15256: 85 26 */    STA CurrentStripeEndType
/* 15258: 4C C9 91 */ JMP EndCurrentFrame

; Scrolling to previous room, I think.
ScrollPreviousRoom:
/* 1525B: A6 27 */    LDX CurrentBeginScreen
/* 1525D: CA */       DEX
/* 1525E: 86 28 */    STX CurrentEndScreen
/* 15260: C6 29 */    DEC CurrentOrderNum
/* 15262: A4 29 */    LDY CurrentOrderNum
/* 15264: 20 13 C6 */ JSR RoomLayoutLoadRoomNum
/* 15267: 29 1F */    AND #$1f
/* 15269: 85 27 */    STA CurrentBeginScreen
/* 1526B: 8A */       TXA
/* 1526C: 38 */       SEC
/* 1526D: E5 27 */    SBC CurrentBeginScreen
/* 1526F: 85 27 */    STA CurrentBeginScreen
/* 15271: 20 05 A1 */ JSR ForgetRoomObjects
/* 15274: A5 28 */    LDA CurrentEndScreen
/* 15276: 20 D8 A0 */ JSR F160D8
/* 15279: CE 60 04 */ DEC ObjectPosScreen+0                             ; $0460
/* 1527C: 20 58 C6 */ JSR SetupEnemyGraphicsPointer
/* 1527F: A5 26 */    LDA CurrentStripeEndType
/* 15281: C9 04 */    CMP #$04
/* 15283: 08 */       PHP
/* 15284: D0 02 */    BNE L15288
/* 15286: C6 1B */    DEC ScrollPosScreen
L15288:
/* 15288: 20 DB 9F */ JSR DoScrolling
/* 1528B: 28 */       PLP
/* 1528C: F0 02 */    BEQ L15290 ; +
/* 1528E: C6 1B */    DEC ScrollPosScreen
L15290: ; +
/* 15290: 20 1B C0 */ JSR NextFrame                                      ; $C01B
/* 15293: A5 28 */    LDA CurrentEndScreen
/* 15295: 38 */       SEC
/* 15296: E9 01 */    SBC #$01
/* 15298: 20 D8 A0 */ JSR F160D8
/* 1529B: A9 00 */    LDA #$00
/* 1529D: 85 8F */    STA ZigZagFireStatus
/* 1529F: A9 01 */    LDA #$01
/* 152A1: 85 8D */    STA ScreenMovedFlag
/* 152A3: 4C 54 92 */ JMP L15254 ; -

; Scrolling to next screen, I think
ScrollNextRoom:
/* 152A6: 20 05 A1 */ JSR ForgetRoomObjects
/* 152A9: A6 28 */    LDX CurrentEndScreen
/* 152AB: E8 */       INX
/* 152AC: 8A */       TXA
/* 152AD: 48 */       PHA
/* 152AE: 20 D8 A0 */ JSR F160D8
; Test if it's first door
/* 152B1: A6 31 */    LDX CurrentStage
/* 152B3: BD 02 9F */ LDA FirstDoorLocations,X
/* 152B6: CD 60 04 */ CMP ObjectPosScreen+0                             ; $0460
/* 152B9: D0 22 */    BNE L152DD ; +
/* 152BB: 8A */       TXA
/* 152BC: 0A */       ASL A
/* 152BD: 0A */       ASL A
/* 152BE: 0A */       ASL A
/* 152BF: 0A */       ASL A
/* 152C0: AA */       TAX
/* 152C1: A0 00 */    LDY #$00
; Setup AfterDoorsPalette if we just opened the first door
L152C3: ; -
/* 152C3: BD 0D 9F */  LDA AfterDoorsPalette,X
/* 152C6: 99 D0 03 */  STA BGPalettes,Y
/* 152C9: E8 */        INX
/* 152CA: C8 */        INY
/* 152CB: C0 10 */     CPY #$10
/* 152CD: D0 F4 */     BNE L152C3 ; -
/* 152CF: 20 3F C7 */ JSR PaletteSetupForBG
/* 152D2: A9 02 */    LDA #$02
/* 152D4: 85 37 */    STA PaletteUpdateDelay
/* 152D6: A9 AF */    LDA #$AF
/* 152D8: 85 A7 */    STA SoundCodeParameter
/* 152DA: 20 66 D0 */ JSR OpenFirstDoor
L152DD: ; + ; Test if it's secod door
/* 152DD: A6 31 */    LDX CurrentStage
/* 152DF: BD F7 9E */ LDA SecondDoorLocations,X
/* 152E2: CD 60 04 */ CMP ObjectPosScreen+0                             ; $0460
/* 152E5: D0 03 */    BNE L152EA ; +
/* 152E7: 20 74 D0 */ JSR OpenSecondDoor
L152EA: ; +
/* 152EA: EE 60 04 */ INC ObjectPosScreen+0                             ; $0460
/* 152ED: 20 58 C6 */ JSR SetupEnemyGraphicsPointer
/* 152F0: A5 26 */    LDA CurrentStripeEndType
/* 152F2: C9 04 */    CMP #$04
/* 152F4: 08 */       PHP
/* 152F5: D0 02 */    BNE L152F9
/* 152F7: E6 1B */    INC ScrollPosScreen
L152F9:
/* 152F9: 20 DB 9F */ JSR DoScrolling
/* 152FC: 28 */       PLP
/* 152FD: F0 02 */    BEQ L15301
/* 152FF: E6 1B */    INC ScrollPosScreen
L15301:
/* 15301: 20 1B C0 */ JSR NextFrame                                      ; $C01B
/* 15304: A5 28 */    LDA CurrentEndScreen
/* 15306: 18 */       CLC
/* 15307: 69 02 */    ADC #$02
/* 15309: 20 D8 A0 */ JSR F160D8
/* 1530C: E6 29 */    INC CurrentOrderNum
/* 1530E: A4 29 */    LDY CurrentOrderNum
/* 15310: 20 13 C6 */ JSR RoomLayoutLoadRoomNum
/* 15313: 48 */       PHA
/* 15314: 29 E0 */    AND #$e0
/* 15316: D0 2B */    BNE L15343

; It's the last room of this stripe.
/* 15318: 20 4F C7 */ JSR PaletteSetupForBGwith3F0
/* 1531B: A9 48 */    LDA #$48
/* 1531D: 85 37 */    STA PaletteUpdateDelay      ; Flashing duration for boss room.
/* 1531F: A9 01 */    LDA #$01
/* 15321: 85 3E */    STA BossCurrentStrategy
/* 15323: A9 70 */    LDA #$70
/* 15325: 85 3C */    STA MiscCounter1
/* 15327: A5 31 */    LDA CurrentStage
/* 15329: 85 AC */    STA FightingBossNum ;Select boss music
/* 1532B: C9 06 */    CMP #$06
/* 1532D: B0 04 */    BCS L15333 ; +
/* 1532F: A9 0D */    LDA #$0d        ; Normal boss music
/* 15331: D0 02 */    BNE L15335 ; ++
L15333: ; +
/* 15333: A9 0B */    LDA #$0b        ; Wily stages boss music
L15335: ; ++
/* 15335: 20 77 C4 */ JSR IssueSound                                      ; $C477

/* 15338: A6 31 */    LDX CurrentStage
/* 1533A: BD 80 93 */ LDA BossRoomArrivalKeymask,X
/* 1533D: 85 69 */    STA ForcedInputData ; Assumed keys
/* 1533F: A9 A6 */    LDA #$A6            ; Possibly, number of frames to disable control
/* 15341: 85 68 */    STA ForcedInputFlag
; Also arrived from 9316
L15343:
/* 15343: 68 */       PLA
/* 15344: 29 1F */     AND #$1F
/* 15346: 85 27 */     STA CurrentBeginScreen
/* 15348: 68 */       PLA
/* 15349: AA */       TAX
/* 1534A: 18 */       CLC
/* 1534B: 65 27 */    ADC CurrentBeginScreen
/* 1534D: 85 28 */    STA CurrentEndScreen
/* 1534F: 86 27 */    STX CurrentBeginScreen
/* 15351: A9 00 */    LDA #$00
/* 15353: 85 8F */    STA ZigZagFireStatus
/* 15355: 85 14 */    STA JoyPad0
/* 15357: 85 16 */    STA JoyPad0old
/* 15359: A9 41 */    LDA #$41
/* 1535B: 85 8D */    STA ScreenMovedFlag
/* 1535D: A5 B0 */    LDA TeleportEnteredFlag
/* 1535F: F0 1C */    BEQ L1537D ; +

; Megaman's coordinate when he enters a teleport!

/* 15361: A9 10 */    LDA #$10
/* 15363: 8D 80 04 */ STA ObjectPosX+0
/* 15366: A9 B4 */    LDA #$B4
/* 15368: 8D 00 06 */ STA ObjectPosY+0

; Make Megaman transform
/* 1536B: A9 41 */    LDA #$41
/* 1536D: 8D 20 04 */ STA ObjectFlags+0
/* 15370: A9 00 */    LDA #$00
/* 15372: 8D 00 04 */ STA ObjectSpriteNum+0

/* 15375: 8D 40 04 */ STA ObjectUnknown440+0
/* 15378: 8D 40 06 */ STA ObjectFireDelay+0
/* 1537B: 85 B0 */    STA TeleportEnteredFlag
L1537D: ; +
/* 1537D: 4C 54 92 */ JMP L15254


BossRoomArrivalKeymask: ; at $9380
    .byte $00, $00, $20, $00, $10, $00
    .byte $00, $20, $00, $00, $00

bank5_938B_table: ;right, up, left, down.
    ; down=up, up=down
    .byte $00, $40, $00, $80
bank5_938F_table:
    ; shutter=right, up=up, shutter=left, down=down
    .byte $20, $80, $20, $40

/* 15393: 00 */       BRK

StageMusicTable: ; at 9394
    .byte $05,$0F,$08,$07,$09,$0A
    .byte $0C,$0C,$10,$10,$10,$0E

FirstScreenOrderTable: ;at 93A0
    .byte $00,$00,$00,$00,$00,$00
    .byte $0E,$07,$09,$09,$16,$07
    ; checkpoint 1
    .byte $05,$01,$03,$04,$07,$01
    .byte $11,$0C,$0D,$0C,$0C,$0C
    ; checkpoint 2
    .byte $0C,$05,$05,$07,$12,$05
    .byte $14,$10,$0D,$10,$17

FirstScreenEnemyPointer: ;at 93C3
    .byte $FF,$FF,$FF,$03,$02,$01 ;Number of enemies on the startup screen minus one!
    .byte $FF,$FF,$02,$00,$FF,$FF
    ; checkpoint 1
    .byte $1B,$11,$1C,$22,$13,$14
    .byte $0E,$1C,$15,$0E,$0F,$0F
    ; checkpoint 2
    .byte $2D,$27,$21,$33,$34,$1D
    .byte $14,$2B,$15,$11,$FF


READY_TEXT: ;at 93E6
.byte $60,$E6,$01,$6C
.byte $60,$E7,$01,$74
.byte $60,$E8,$01,$7C
.byte $60,$E9,$01,$84
.byte $60,$EA,$01,$8C

Label93FA:
    .byte $0A,$0A,$0E,$08,$07,$0A
    .byte $22,$24,$1C,$1D,$14,$14
    .byte $14,$14,$14,$10,$14,$0F
    .byte $26,$28,$1C,$21,$19,$14  ;Stored into CurrentBeginScreen
Label9412:
    .byte $0A,$0A,$0E,$0A,$08,$0A
    .byte $22,$24,$2E,$1D,$1D,$1D
    .byte $16,$16,$14,$12,$14,$11
    .byte $26,$28,$2E,$22,$20,$1E  ;Stored into CurrentEndScreen
WilyStageClearScoreTable: ;at $9424
    .byte 100,100,70,200,200    ; Level clear score for Wily stages *1000.

EnterTeleport:
/* 1542F: 20 05 A1 */ JSR ForgetRoomObjects

; Make Megaman transform
/* 15432: A9 14 */    LDA #$14
/* 15434: 8D 00 04 */ STA ObjectSpriteNum+0

/* 15437: A9 00 */    LDA #$00
/* 15439: 8D 40 04 */ STA ObjectUnknown440+0
L1543C: ; -
/* 1543C: A9 01 */    LDA #$01
/* 1543E: 20 09 D1 */ JSR TimeDelayWithSpriteUpdates                      ; $D109
/* 15441: AD 40 04 */ LDA ObjectUnknown440+0
/* 15444: D0 F6 */    BNE L1543C ; -                                               ; $943C

; Disable Megaman
/* 15446: A9 F8 */    LDA #$f8
/* 15448: 8D 00 06 */ STA ObjectPosY+0

/* 1544B: A9 01 */    LDA #$01
/* 1544D: 20 09 D1 */ JSR TimeDelayWithSpriteUpdates                      ; $D109
/* 15450: 4C A6 92 */ JMP ScrollNextRoom


;
; A = CapsuleObtained = tnnnnnnn
;
; t = type of capsule picked up (0 if life, 1 if weapon)
;
; n = value of capsule
;

MeterRefill:
/* 15453: A2 00 */    LDX #$00        ; If this is a life capsule, then fill life
/* 15455: 29 80 */    AND #$80
/* 15457: F0 04 */    BEQ FillMeter                                       ; $945D

; Weapon capsule
/* 15459: A6 5F */    LDX WeaponSelect                                    ; If Megablaster selected then skip filling
/* 1545B: F0 34 */    BEQ MeterFilled                                     ; $9491

FillMeter:
/* 1545D: A5 AD */    LDA CapsuleObtained
/* 1545F: 29 7F */    AND #$7f
/* 15461: 85 3C */    STA MiscCounter1
L15463: ; --

; Add 1 to meter every 4th frame and play meter fill sound every 8th frame
/* 15463: A5 23 */    LDA FrameCounter
/* 15465: 29 03 */    AND #$03
/* 15467: D0 1B */    BNE L15484 ; ++                                              ; $9484
/* 15469: A5 23 */    LDA FrameCounter
/* 1546B: 29 07 */    AND #$07
/* 1546D: D0 09 */    BNE L15478 ; +                                               ; $9478


/* 1546F: 8A */       TXA
/* 15470: 48 */       PHA
/* 15471: A9 18 */    LDA #$18        ; Meter fillup sound
/* 15473: 20 77 C4 */ JSR IssueSound                                      ; $C477
/* 15476: 68 */       PLA
/* 15477: AA */       TAX

L15478: ; +
/* 15478: B5 6A */    LDA Meters,X
/* 1547A: C9 1C */    CMP #$1c        ; Break out of loop when meter is full
/* 1547C: F0 13 */    BEQ MeterFilled                                     ; $9491
/* 1547E: F6 6A */    INC Meters,X
/* 15480: C6 3C */    DEC MiscCounter1
/* 15482: F0 0A */    BEQ L1548E ; +                                               ; $948E

L15484: ; ++
/* 15484: 8A */       TXA
/* 15485: 48 */       PHA
/* 15486: 20 98 94 */ JSR PauseFrameTick
/* 15489: 68 */       PLA
/* 1548A: AA */       TAX

/* 1548B: 4C 63 94 */ JMP L15463 ; --                                              ; $9463
L1548E: ; +
/* 1548E: 20 98 94 */ JSR PauseFrameTick

MeterFilled:
; Reset joypad data (and?...)
/* 15491: A9 00 */    LDA #$00
/* 15493: 85 AD */    STA CapsuleObtained
/* 15495: 85 14 */    STA JoyPad0
/* 15497: 60 */       RTS


PauseFrameTick:
/* 15498: EE A0 06 */ INC ObjectLifeCycleCounter+0 ;inc 6A0 for Megaman
/* 1549B: 20 65 C4 */ JSR LifeCycleTick_forEnemies
/* 1549E: 20 31 D1 */ JSR UpdateGraphics                                  ; $D131
/* 154A1: 20 6A D4 */ JSR LifeCycleUntick_forEveryone
/* 154A4: 20 1B C0 */ JSR NextFrame                                       ; $C01B
/* 154A7: 60 */       RTS


MegamanAI:
/* 154A8: A2 00 */    LDX #$00
/* 154AA: 86 2F */    STX RefObjectNum

; Make Megaman stop moving
/* 154AC: AD 20 04 */ LDA ObjectFlags+0
/* 154AF: 29 7F */    AND #$7f
/* 154B1: 8D 20 04 */ STA ObjectFlags+0

/* 154B4: 29 0F */    AND #$0f
/* 154B6: F0 03 */    BEQ L154BB ; +                                               ; $94BB
/* 154B8: 4C DB 96 */ JMP L156DB
L154BB:
/* 154BB: AD 40 06 */ LDA ObjectFireDelay+0
L154BE: ; +
/* 154BE: 29 F0 */    AND #$f0
/* 154C0: C9 20 */    CMP #$20
/* 154C2: F0 23 */    BEQ NotClimbing
/* 154C4: AD 20 04 */ LDA ObjectFlags+0
/* 154C7: 29 10 */    AND #$10
/* 154C9: D0 19 */    BNE L154E4 ;When climbing. Go to Ladder_Handler.
/* 154CB: A5 30 */    LDA CurrentTileState
/* 154CD: 29 7F */    AND #$7F
/* 154CF: F0 16 */    BEQ NotClimbing ;Not climbing, and not a ladder.

; Well well, there's a ladder somewhere! Check if up/down is pressed.
/* 154D1: A5 14 */    LDA JoyPad0
/* 154D3: 29 30 */    AND #$30
/* 154D5: F0 10 */    BEQ NotClimbing

; Holding up/down key
/* 154D7: 05 30 */    ORA CurrentTileState
/* 154D9: C9 11 */    CMP #$11        ;up, but there's only ladder on bottom
/* 154DB: F0 0A */    BEQ NotClimbing
/* 154DD: C9 2E */    CMP #$2E        ;down, but there's no ladder on bottom
/* 154DF: F0 06 */    BEQ NotClimbing
/* 154E1: 4C BD 97 */ JMP L157BD

; From 94C9
L154E4:
/* 154E4: 4C DE 97 */ JMP Ladder_Handler

NotClimbing:
L154E7:
/* 154E7: A5 94 */    LDA MegamanWalkTimer
/* 154E9: F0 0E */    BEQ L154F9 ; +                                               ; $94F9
/* 154EB: A5 23 */    LDA FrameCounter
/* 154ED: 29 03 */    AND #$03
/* 154EF: D0 08 */    BNE L154F9 ; +                                               ; $94F9
/* 154F1: A9 01 */    LDA #$01
/* 154F3: 85 95 */    STA MegamanStallTimer
/* 154F5: EE A0 06 */ INC ObjectLifeCycleCounter+0
/* 154F8: 60 */       RTS
L154F9: ; +
/* 154F9: A9 00 */    LDA #$00
/* 154FB: 85 95 */    STA MegamanStallTimer
/* 154FD: 85 94 */    STA MegamanWalkTimer

; if left/right is held in this frame and was not held in previous frame
; then Make Megaman move slowly

/* 154FF: A5 14 */    LDA JoyPad0
/* 15501: 29 C0 */    AND #$c0
/* 15503: F0 1B */    BEQ L15520 ; ++                                              ; $9520
/* 15505: A5 16 */    LDA JoyPad0old
/* 15507: 29 C0 */    AND #$c0
/* 15509: D0 0D */    BNE L15518 ; +                                               ; $9518
/* 1550B: A9 03 */    LDA #$03
/* 1550D: 8D 00 04 */ STA ObjectSpriteNum+0
/* 15510: A9 00 */    LDA #$00
/* 15512: 8D 40 04 */ STA ObjectUnknown440+0
/* 15515: 8D 40 06 */ STA ObjectFireDelay+0
L15518: ; +
/* 15518: 20 7B 98 */ JSR SetMegamanFacing

; Make Megaman move
/* 1551B: 09 80 */    ORA #$80
/* 1551D: 8D 20 04 */ STA ObjectFlags+0

L15520: ; ++
/* 15520: 20 E7 9C */ JSR F15CE7
/* 15523: A5 9B */    LDA LiftUnknown9B
/* 15525: 29 40 */    AND #$40
/* 15527: F0 06 */    BEQ L1552F ; +
/* 15529: 20 EF 9D */ JSR ObjectUpdateMovementRight
/* 1552C: 4C 32 95 */ JMP L15532 ; ++
L1552F: ; +
/* 1552F: 20 6D 9E */ JSR ObjectUpdateMovementLeft
L15532: ; ++
/* 15532: 20 8F 98 */ JSR AutoCenterScreen
/* 15535: 20 DE CB */ JSR UpdateCurrentTileState
/* 15538: A5 18 */    LDA JoyD0
/* 1553A: 29 02 */    AND #$02  ; If B pressed
/* 1553C: F0 05 */    BEQ L15543 ; +
/* 1553E: 20 1E A7 */ JSR MegamanWeaponFire
/* 15541: D0 00 */    BNE L15543 ; +
L15543: ; +
/* 15543: AD 80 06 */ LDA ObjectYSpeed+0
/* 15546: 30 29 */    BMI L15571
/* 15548: 20 C4 9B */ JSR ObjectDoCollisionChecksAndAvoidWalls
/* 1554B: B0 09 */    BCS L15556
/* 1554D: A5 14 */    LDA JoyPad0
/* 1554F: 29 01 */    AND #$01
/* 15551: F0 06 */    BEQ L15559
/* 15553: 4C 29 96 */ JMP L15629
L15556:
/* 15556: 4C 33 96 */ JMP L15633
L15559:
/* 15559: AD 80 06 */ LDA ObjectYSpeed+0
/* 1555C: 30 10 */    BMI L1556E
/* 1555E: C9 01 */    CMP #$01
/* 15560: 90 0C */    BCC L1556E
/* 15562: F0 0A */    BEQ L1556E
/* 15564: A9 01 */    LDA #$01
/* 15566: 8D 80 06 */ STA ObjectYSpeed+0
/* 15569: A9 00 */    LDA #$00
/* 1556B: 8D 60 06 */ STA ObjectYSpeedFraction+0
L1556E:
/* 1556E: 4C 29 96 */ JMP L15629
L15571:
/* 15571: 20 34 96 */ JSR TestIsRiding
/* 15574: 48 */       PHA
/* 15575: 20 C4 9B */ JSR ObjectDoCollisionChecksAndAvoidWalls
/* 15578: B0 0B */    BCS L15585
/* 1557A: 68 */       PLA
/* 1557B: F0 05 */    BEQ L15582
/* 1557D: 8D 00 06 */ STA ObjectPosY+0
/* 15580: D0 04 */    BNE L15586
L15582:
/* 15582: 4C 29 96 */ JMP L15629


L15585:
/* 15585: 68 */       PLA

; Make megaman jump unless he is already jumping/falling???
L15586:
/* 15586: AD 00 04 */ LDA ObjectSpriteNum+0
/* 15589: C9 09 */    CMP #$09
/* 1558B: F0 09 */    BEQ L15596 ; +                                               ; $9596
/* 1558D: C9 6F */    CMP #$6f
/* 1558F: D0 0A */    BNE L1559B ; ++                                              ; $959B

/* 15591: A9 09 */    LDA #$09
/* 15593: 8D 00 04 */ STA ObjectSpriteNum+0

L15596: ; +
/* 15596: A9 19 */    LDA #$19        ; Megaman touching ground sound
/* 15598: 20 77 C4 */ JSR IssueSound                                      ; $C477
L1559B: ; ++
/* 1559B: A5 2D */    LDA $2D
/* 1559D: CD 00 06 */ CMP ObjectPosY+0
/* 155A0: B0 16 */    BCS L155B8 ; +                                               ; $95B8
/* 155A2: A5 15 */    LDA $15
/* 155A4: 29 00 */    AND #$00
/* 155A6: F0 10 */    BEQ L155B8 ; +                                               ; $95B8

; Shake Megaman
/* 155A8: AD 20 04 */ LDA ObjectFlags+0
/* 155AB: 29 40 */    AND #$40
/* 155AD: 09 02 */    ORA #$02
/* 155AF: 8D 20 04 */ STA ObjectFlags+0

/* 155B2: AD 00 06 */ LDA ObjectPosY+0
/* 155B5: 85 2D */    STA $2D
/* 155B7: 60 */       RTS
L155B8: ; +
/* 155B8: AD 00 06 */ LDA ObjectPosY+0                                     ; ObjectPosY+0
/* 155BB: 85 2D */    STA $2D

/* 155BD: A5 18 */    LDA JoyD0
/* 155BF: 29 01 */    AND #$01
/* 155C1: D0 5B */    BNE L1561E ; ++++                                            ; $961E

; If button A is not pressed
/* 155C3: AD 20 04 */ LDA ObjectFlags+0
/* 155C6: 29 80 */    AND #$80
/* 155C8: D0 37 */    BNE L15601 ; ++                                              ; $9601

; if moving...
/* 155CA: A5 16 */    LDA JoyPad0old
/* 155CC: 29 C0 */    AND #$c0
/* 155CE: D0 11 */    BNE L155E1 ; +                                               ; $95E1

; if left/right wasn't pressed the last frame
/* 155D0: AD 00 04 */ LDA ObjectSpriteNum+0
/* 155D3: C9 09 */    CMP #$09
/* 155D5: D0 1B */    BNE L155F2

; if in air...
/* 155D7: A9 00 */    LDA #$00
/* 155D9: 85 98 */    STA $98
/* 155DB: 85 99 */    STA $99
/* 155DD: A9 0F */    LDA #$0f
/* 155DF: D0 09 */    BNE L155EA
L155E1: ; +
/* 155E1: AD 00 04 */ LDA ObjectSpriteNum+0
/* 155E4: C9 06 */    CMP #$06
/* 155E6: D0 0F */    BNE L155F7

; Make Megaman slow down after running
/* 155E8: A9 0C */    LDA #$0C
L155EA:
/* 155EA: 8D 00 04 */ STA ObjectSpriteNum+0

/* 155ED: A9 01 */    LDA #$01
/* 155EF: 8D 40 04 */ STA ObjectUnknown440+0

; from 95D5: was not in a jumping state
L155F2:
/* 155F2: AD 40 04 */ LDA ObjectUnknown440+0
/* 155F5: D0 3C */    BNE L15633


; Make Megaman stand still if he wasn't running
L155F7:
/* 155F7: A9 00 */    LDA #$00
/* 155F9: 8D 00 04 */ STA ObjectSpriteNum+0
/* 155FC: 8D 40 04 */ STA ObjectUnknown440+0
/* 155FF: F0 32 */    BEQ L15633
L15601: ; ++
/* 15601: AD 00 04 */ LDA ObjectSpriteNum+0
/* 15604: C9 09 */    CMP #$09
/* 15606: F0 0B */    BEQ L15613
/* 15608: C9 03 */    CMP #$03
/* 1560A: D0 27 */    BNE L15633
/* 1560C: AD 40 04 */ LDA ObjectUnknown440+0
/* 1560F: C9 22 */    CMP #$22
/* 15611: D0 20 */    BNE L15633

; Make Megaman run
L15613:
/* 15613: A9 06 */    LDA #$06
/* 15615: 8D 00 04 */ STA ObjectSpriteNum+0

/* 15618: A9 00 */    LDA #$00
/* 1561A: 8D 40 04 */ STA ObjectUnknown440+0
/* 1561D: 60 */       RTS

; if button A pressed...
L1561E: ; ++++
/* 1561E: A9 04 */    LDA #$04
/* 15620: 8D 80 06 */ STA ObjectYSpeed+0
/* 15623: A9 DF */    LDA #$df
/* 15625: 8D 60 06 */ STA ObjectYSpeedFraction+0
/* 15628: 60 */       RTS

; NOTE! This is written not once, but repeatedly as long as Megaman
; is jumping/falling

L15629:
/* 15629: A9 09 */    LDA #$09
/* 1562B: 8D 00 04 */ STA ObjectSpriteNum+0

/* 1562E: A9 00 */    LDA #$00
/* 15630: 8D 40 04 */ STA ObjectUnknown440+0


L15633:
/* 15633: 60 */       RTS

TestIsRiding:
/* 15634: A5 60 */    LDA WeaponFiring
/* 15636: C9 C7 */    CMP #$C7
/* 15638: D0 2A */    BNE TestLiftRiding    ; Don't test magnet beams if none is being used
TestMagnetBeamRiding:
/* 1563A: A2 00 */    LDX #$00
L1563C: ; -
/* 1563C: BD B0 05 */ LDA MagnetBeamAge,X
/* 1563F: F0 1E */    BEQ L1565F ; +
/* 15641: BD D0 05 */ LDA MagnetBeamPosScreen,X
/* 15644: 85 07 */    STA CurrentRoomPointer+1
/* 15646: BD C0 05 */ LDA MagnetBeamPosX,X
/* 15649: 85 06 */    STA CurrentRoomPointer+0
/* 1564B: BD A0 05 */ LDA MagnetBeamLength,X
/* 1564E: 29 7C */    AND #$7C
/* 15650: 0A */       ASL A
/* 15651: 69 10 */    ADC #$10
/* 15653: 85 09 */    STA CurrentRoomPointer+3
/* 15655: BD F0 05 */ LDA MagnetBeamPosY,X
/* 15658: 85 08 */    STA CurrentRoomPointer+2
/* 1565A: 20 91 96 */ JSR TestRidingWithCoordinates
/* 1565D: B0 31 */    BCS L15690 ; +++
L1565F: ; +
/* 1565F: E8 */       INX
/* 15660: E0 10 */    CPX #$10
/* 15662: D0 D8 */    BNE L1563C ; -
; Test collision with lifts
TestLiftRiding:
/* 15664: A6 9A */    LDX LiftIndex
/* 15666: F0 1C */    BEQ L15684 ; +
/* 15668: CA */       DEX
L15669: ; -
/* 15669: BD 00 07 */ LDA LiftPosScreen,X
/* 1566C: 85 07 */    STA CurrentRoomPointer+1
/* 1566E: BD 08 07 */ LDA LiftPosX,X
/* 15671: 85 06 */    STA CurrentRoomPointer+0
/* 15673: A9 14 */    LDA #$14
/* 15675: 85 09 */    STA CurrentRoomPointer+3
/* 15677: BD 10 07 */ LDA LiftPosY,X
/* 1567A: 85 08 */    STA CurrentRoomPointer+2
/* 1567C: 20 91 96 */ JSR TestRidingWithCoordinates
/* 1567F: B0 06 */    BCS L15687 ; ++
/* 15681: CA */       DEX
/* 15682: 10 E5 */    BPL L15669 ; -
L15684: ; +
/* 15684: A9 00 */    LDA #$00
/* 15686: 60 */       RTS
L15687: ; ++
/* 15687: 48 */       PHA
/* 15688: BD 18 07 */ LDA LiftDirection,X
/* 1568B: 09 80 */    ORA #$80
/* 1568D: 85 9E */    STA LiftUnknown9E
/* 1568F: 68 */       PLA
L15690: ; +++
/* 15690: 60 */       RTS


TestRidingWithCoordinates:
;
; Input:
;   CurrentRoomPointer+0 = MIDDLE Xpos
;   CurrentRoomPointer+1 = MIDDLE screen number
;   CurrentRoomPointer+2 = MIDDLE Ypos
;   CurrentRoomPointer+3 = HALF OF THE width of the lift
;

/* 15691: 18 */       CLC
/* 15692: AD 00 06 */ LDA ObjectPosY+0
/* 15695: 69 0E */    ADC #$0E
/* 15697: 85 04 */    STA $04
/* 15699: C5 08 */    CMP CurrentRoomPointer+2
/* 1569B: B0 3C */    BCS L156D9 ; +++                    ;If Megaman is below, he's not on the lift.
/* 1569D: 38 */       SEC
/* 1569E: AD 20 06 */ LDA ObjectPosYfraction+0
/* 156A1: ED 60 06 */ SBC ObjectYSpeedFraction+0
/* 156A4: A5 04 */    LDA $04
/* 156A6: ED 80 06 */ SBC ObjectYSpeed+0
/* 156A9: C5 08 */    CMP CurrentRoomPointer+2   ;If, after moving, he's above, he's not on the lift.
/* 156AB: 90 2C */    BCC L156D9 ; +++
/* 156AD: 38 */       SEC
/* 156AE: AD 80 04 */ LDA ObjectPosX+0
/* 156B1: E5 06 */    SBC CurrentRoomPointer
/* 156B3: 85 04 */    STA $04
/* 156B5: AD 60 04 */ LDA ObjectPosScreen+0
/* 156B8: E5 07 */    SBC CurrentRoomPointer+1
/* 156BA: 90 04 */    BCC L156C0 ; +                      ;Previous screen?
/* 156BC: D0 1B */    BNE L156D9 ; +++                    ;If he's not in the same screen, no match.
/* 156BE: F0 0C */    BEQ L156CC ; ++
L156C0: ; +
/* 156C0: 49 FF */    EOR #$ff                   ;Compensate fancily
/* 156C2: D0 15 */    BNE L156D9 ; +++
/* 156C4: A5 04 */    LDA $04
/* 156C6: 49 FF */    EOR #$ff
/* 156C8: 69 01 */    ADC #$01
/* 156CA: 85 04 */    STA $04
L156CC: ; ++
/* 156CC: A5 09 */    LDA CurrentRoomPointer+3 ;Now compare if the width is too far
/* 156CE: C5 04 */    CMP $04
/* 156D0: 90 07 */    BCC L156D9 ; +++
/* 156D2: 38 */       SEC
/* 156D3: A5 08 */    LDA CurrentRoomPointer+2 ;And the X position...
/* 156D5: E9 10 */    SBC #$10
/* 156D7: 38 */       SEC
/* 156D8: 60 */       RTS
L156D9: ; +++
/* 156D9: 18 */       CLC
/* 156DA: 60 */       RTS



L156DB:
/* 156DB: 0A */       ASL A
/* 156DC: AA */       TAX
/* 156DD: BC 40 97 */ LDY L15742-2,X
/* 156E0: BD 41 97 */ LDA L15742-1,X
/* 156E3: 48 */       PHA
/* 156E4: 29 F0 */    AND #$f0
/* 156E6: 85 00 */    STA $00
/* 156E8: 68 */       PLA
/* 156E9: 29 0F */    AND #$0f
/* 156EB: 85 01 */    STA $01
/* 156ED: 20 4A 97 */ JSR F1574A
/* 156F0: AD 40 04 */ LDA ObjectUnknown440+0
/* 156F3: F0 3B */    BEQ L15730
/* 156F5: 20 E7 9C */ JSR F15CE7
/* 156F8: A5 9B */    LDA LiftUnknown9B

/* 156FA: AE 00 04 */ LDX ObjectSpriteNum+0
/* 156FD: E0 6E */    CPX #$6E ;throwing something?
/* 156FF: F0 0C */    BEQ L1570D ; +                                               ; $970D
/* 15701: E0 14 */    CPX #$14 ;transforming?
/* 15703: D0 0A */    BNE L1570F
/* 15705: A2 00 */    LDX #$00
/* 15707: 8E E0 04 */ STX ObjectXSpeedFraction+0
/* 1570A: 8E C0 04 */ STX ObjectXSpeed+0

; Megaman throwing something (while standing)

L1570D: ; +
/* 1570D: 49 40 */    EOR #$40
L1570F:
/* 1570F: 29 40 */    AND #$40
/* 15711: D0 06 */    BNE L15719 ; +                                               ; $9719
/* 15713: 20 EF 9D */ JSR ObjectUpdateMovementRight
/* 15716: 4C 1C 97 */ JMP L1571C ; ++                                              ; $971C
L15719: ; +
/* 15719: 20 6D 9E */ JSR ObjectUpdateMovementLeft
L1571C: ; ++
/* 1571C: 20 8F 98 */ JSR AutoCenterScreen
/* 1571F: 20 DE CB */ JSR UpdateCurrentTileState
/* 15722: 20 34 96 */ JSR TestIsRiding
/* 15725: 48 */       PHA
/* 15726: 20 C4 9B */  JSR ObjectDoCollisionChecksAndAvoidWalls
/* 15729: 68 */       PLA
/* 1572A: F0 03 */    BEQ L1572F ; +                                               ; $972F
/* 1572C: 8D 00 06 */ STA ObjectPosY+0
L1572F: ; +
/* 1572F: 60 */       RTS

; Make Megaman stand still facing right and clear joypad data
L15730:
/* 15730: AD 20 04 */ LDA ObjectFlags+0
/* 15733: 29 40 */    AND #$40
/* 15735: 8D 20 04 */ STA ObjectFlags+0
/* 15738: A9 00 */    LDA #$00
/* 1573A: 8D 00 04 */ STA ObjectSpriteNum+0
/* 1573D: 85 16 */    STA JoyPad0old

/* 1573F: 4C E7 94 */ JMP L154E7

L15742:
/* 15742: 14 */       .byte $14
/* 15743: 00 */       BRK
/* 15744: 13 */       .byte $13
/* 15745: E0 12 */    CPX #$12
/* 15747: 41 6E */    EOR ($6E,X)
/* 15749: 00 */       BRK



F1574A:
/* 1574A: CC 00 04 */ CPY ObjectSpriteNum+0
/* 1574D: F0 47 */    BEQ L15796
/* 1574F: 8C 00 04 */ STY ObjectSpriteNum+0
/* 15752: C0 14 */    CPY #$14  ;transforming?
/* 15754: D0 05 */    BNE L1575B ; +                                               ; $975B
/* 15756: A9 20 */    LDA #$20        ; Megaman transforming sound
/* 15758: 20 77 C4 */ JSR IssueSound                                      ; $C477
L1575B: ; +
/* 1575B: C0 12 */    CPY #$12    ;getting hit by enemy?
/* 1575D: D0 2B */    BNE L1578A
/* 1575F: A2 16 */    LDX #$16
/* 15761: 20 76 C5 */ JSR FindFreeObject
/* 15764: B0 1F */    BCS L15785
/* 15766: A0 08 */    LDY #$08
/* 15768: 20 33 F5 */ JSR InitObjectDefaultSpeed
/* 1576B: A9 00 */    LDA #$00
/* 1576D: 85 2F */    STA RefObjectNum
/* 1576F: 20 7B F6 */ JSR InitActor
/* 15772: A9 19 */    LDA #$19
/* 15774: 9D 00 04 */ STA ObjectSpriteNum,X
/* 15777: FE 40 04 */ INC ObjectUnknown440,X
/* 1577A: A9 08 */    LDA #$08
/* 1577C: 9D 20 04 */ STA ObjectFlags,X
/* 1577F: 9D 60 06 */ STA ObjectYSpeedFraction,X
/* 15782: 20 3D C5 */ JSR C53D_routine

L15785:
/* 15785: A9 16 */    LDA #$16        ; Some kind of explosion
/* 15787: 20 77 C4 */ JSR IssueSound                                      ; $C477

L1578A:
/* 1578A: A9 01 */    LDA #$01
/* 1578C: 8D 40 04 */ STA ObjectUnknown440+0
/* 1578F: A9 00 */    LDA #$00
/* 15791: 8D 40 06 */ STA ObjectFireDelay+0
/* 15794: F0 0D */    BEQ L157A3
L15796:
/* 15796: C0 13 */    CPY #$13
/* 15798: D0 1B */    BNE L157B5
/* 1579A: AD 40 04 */ LDA ObjectUnknown440+0
/* 1579D: 29 1F */    AND #$1f
/* 1579F: C9 01 */    CMP #$01
/* 157A1: D0 12 */    BNE L157B5
L157A3:
/* 157A3: C0 14 */    CPY #$14
/* 157A5: F0 0E */    BEQ L157B5
/* 157A7: C0 6E */    CPY #$6e
/* 157A9: F0 0A */    BEQ L157B5
/* 157AB: A5 00 */    LDA $00
/* 157AD: 8D 60 06 */ STA ObjectYSpeedFraction+0
/* 157B0: A5 01 */    LDA $01
/* 157B2: 8D 80 06 */ STA ObjectYSpeed+0
L157B5:
/* 157B5: C0 14 */    CPY #$14
/* 157B7: D0 03 */    BNE L157BC
/* 157B9: 20 65 C4 */ JSR LifeCycleTick_forEnemies
L157BC:
/* 157BC: 60 */       RTS



; Make Megaman turn and...
L157BD:
/* 157BD: AD 20 04 */ LDA ObjectFlags+0
/* 157C0: 09 10 */    ORA #$10
/* 157C2: 49 40 */    EOR #$40
/* 157C4: 8D 20 04 */ STA ObjectFlags+0

/* 157C7: A9 00 */    LDA #$00
/* 157C9: 8D 40 04 */ STA ObjectUnknown440+0
/* 157CC: 8D 40 06 */ STA ObjectFireDelay+0
/* 157CF: A5 2E */    LDA $2E
/* 157D1: 18 */       CLC
/* 157D2: 69 08 */    ADC #$08
/* 157D4: 85 22 */    STA $22
/* 157D6: AD 60 04 */ LDA ObjectPosScreen+0                             ; $0460
/* 157D9: 85 20 */    STA $20
/* 157DB: 20 8F 98 */ JSR AutoCenterScreen

Ladder_Handler:
; Make Megaman grab ladder
/* 157DE: A9 15 */    LDA #$15      ;on ladder
/* 157E0: 8D 00 04 */ STA ObjectSpriteNum+0

/* 157E3: A5 18 */    LDA JoyD0
/* 157E5: 29 02 */    AND #$02
/* 157E7: D0 6B */    BNE Ladder_B_Pressed

/* 157E9: A5 14 */    LDA JoyPad0
/* 157EB: 29 31 */    AND #$31
/* 157ED: F0 76 */    BEQ Ladder_HoldStill ;no up,down,A
/* 157EF: 29 30 */    AND #$30
/* 157F1: F0 7B */    BEQ Ladder_Release   ;no up,down so it must be A pressed
/* 157F3: 29 10 */    AND #$10             ;either up or down pressed
/* 157F5: F0 23 */    BEQ Ladder_ClimbDown ;if up is not pressed
Ladder_ClimbUp:                       ;well, up is pressed.
/* 157F7: A0 00 */    LDY #$00             ;XSpeed
/* 157F9: A2 C0 */    LDX #$c0             ;XSpeedFraction (speed is +0.75)
/* 157FB: A5 30 */    LDA CurrentTileState
/* 157FD: 29 0C */    AND #$0c
/* 157FF: D0 0E */    BNE L1580F ; +
; Adjust Rockman's position above the ladder so that he'll not fall
; If he's little too high, this may place him in ceiling. -Bisqwit
/* 15801: AD 00 06 */ LDA ObjectPosY+0
/* 15804: 29 F0 */    AND #$f0
/* 15806: 38 */       SEC
/* 15807: E9 0C */    SBC #$0c
/* 15809: 8D 00 06 */ STA ObjectPosY+0
/* 1580C: 4C 6E 98 */ JMP Ladder_Release
L1580F: ; +
/* 1580F: 29 08 */    AND #$08
/* 15811: D0 25 */    BNE Ladder_Climb

/* 15813: A9 17 */    LDA #$17  ;getting off the ladder
/* 15815: 8D 00 04 */ STA ObjectSpriteNum+0
/* 15818: D0 1E */    BNE Ladder_Climb ;unconditional jump.

Ladder_ClimbDown:
/* 1581A: A5 30 */    LDA CurrentTileState
/* 1581C: C9 01 */    CMP #$01
/* 1581E: D0 09 */    BNE L15829 ; +
/* 15820: AD 00 06 */ LDA ObjectPosY+0

/* 15823: 18 */       CLC
/* 15824: 69 0C */    ADC #$0c
/* 15826: 8D 00 06 */ STA ObjectPosY+0
L15829: ; +
/* 15829: A0 FF */    LDY #$ff        ;YSpeed
/* 1582B: A2 40 */    LDX #$40        ;YSpeedFraction  (Speed is -0.75)
/* 1582D: A5 30 */    LDA CurrentTileState
/* 1582F: 29 0C */    AND #$0c
/* 15831: D0 05 */    BNE Ladder_Climb

/* 15833: A9 17 */    LDA #$17  ;getting off the ladder
/* 15835: 8D 00 04 */ STA ObjectSpriteNum+0

Ladder_Climb:
/* 15838: AD 40 06 */ LDA ObjectFireDelay+0
/* 1583B: F0 04 */    BEQ L15841 ; +     ;don't climb if weapon fire delay is active
/* 1583D: A2 00 */    LDX #$00
/* 1583F: A0 00 */    LDY #$00
L15841: ; +
/* 15841: 8C 80 06 */ STY ObjectYSpeed+0
/* 15844: 8E 60 06 */ STX ObjectYSpeedFraction+0
/* 15847: 20 DE CB */ JSR UpdateCurrentTileState
/* 1584A: A5 30 */    LDA CurrentTileState
/* 1584C: F0 20 */    BEQ Ladder_Release
/* 1584E: 20 C4 9B */ JSR ObjectDoCollisionChecksAndAvoidWalls
/* 15851: B0 1B */    BCS Ladder_Release
/* 15853: 60 */       RTS

Ladder_B_Pressed:
/* 15854: A5 14 */    LDA JoyPad0
/* 15856: 29 C0 */    AND #$c0
/* 15858: F0 03 */    BEQ L1585D ; +
/* 1585A: 20 7B 98 */ JSR SetMegamanFacing
L1585D: ; +
/* 1585D: A9 1F */    LDA #$1f
/* 1585F: 8D 40 06 */ STA ObjectFireDelay+0 ;; weapon fire delay in ladder
/* 15862: 20 1E A7 */ JSR MegamanWeaponFire
Ladder_HoldStill:
/* 15865: AD 40 04 */ LDA ObjectUnknown440+0
/* 15868: 29 F0 */    AND #$f0
/* 1586A: 8D 40 04 */ STA ObjectUnknown440+0
/* 1586D: 60 */       RTS

Ladder_Release:
/* 1586E: A9 40 */    LDA #$40
/* 15870: 8D 60 06 */ STA ObjectYSpeedFraction+0
/* 15873: A9 FF */    LDA #$ff
/* 15875: 8D 80 06 */ STA ObjectYSpeed+0
/* 15878: 4C 30 97 */ JMP L15730

SetMegamanFacing:
; Make Megaman face left
/* 1587B: AD 20 04 */ LDA ObjectFlags+0
/* 1587E: 29 BF */    AND #$bf
/* 15880: 8D 20 04 */ STA ObjectFlags+0
; Make Megaman face right if "right" button is held
/* 15883: A5 14 */    LDA JoyPad0
/* 15885: 29 80 */    AND #$80
/* 15887: 4A */       LSR A
/* 15888: 0D 20 04 */ ORA ObjectFlags+0
/* 1588B: 8D 20 04 */ STA ObjectFlags+0
/* 1588E: 60 */       RTS


AutoCenterScreen:
/* 1588F: 38 */       SEC
/* 15890: A5 22 */    LDA $22
/* 15892: E9 80 */    SBC #$80
/* 15894: 85 1A */    STA ScrollPosX
/* 15896: A5 20 */    LDA $20
/* 15898: E9 00 */    SBC #$00
/* 1589A: C5 27 */    CMP CurrentBeginScreen
/* 1589C: 30 02 */    BMI L158A0 ; +                                               ; $98A0
/* 1589E: B0 07 */    BCS L158A7 ; ++                                              ; $98A7
L158A0: ; +
/* 158A0: A5 27 */    LDA CurrentBeginScreen
/* 158A2: 85 1B */    STA ScrollPosScreen
/* 158A4: 4C AD 98 */ JMP L158AD ; +++
L158A7: ; ++
/* 158A7: 85 1B */    STA ScrollPosScreen
/* 158A9: C5 28 */    CMP CurrentEndScreen
/* 158AB: D0 06 */    BNE L158B3 ; +                                               ; $98B3
L158AD: ; +++
/* 158AD: A9 00 */    LDA #$00
/* 158AF: 85 1A */    STA ScrollPosX
/* 158B1: F0 25 */    BEQ ObjectRelocateHorizontally
L158B3: ; +
/* 158B3: 38 */       SEC
/* 158B4: AD 80 04 */ LDA ObjectPosX+0
/* 158B7: E5 22 */    SBC $22
/* 158B9: 85 0C */    STA $0C
/* 158BB: AD 60 04 */ LDA ObjectPosScreen+0                             ; $0460
/* 158BE: E5 20 */    SBC $20
/* 158C0: 10 0F */    BPL L158D1 ; +                                               ; $98D1
/* 158C2: 38 */       SEC
/* 158C3: A9 00 */    LDA #$00
/* 158C5: E5 0C */    SBC $0C
/* 158C7: F0 0F */    BEQ ObjectRelocateHorizontally
/* 158C9: 85 0C */    STA $0C
/* 158CB: 20 6D 9F */ JSR Function9F6D
/* 158CE: 4C D8 98 */ JMP ObjectRelocateHorizontally
L158D1: ; +
/* 158D1: A5 0C */    LDA $0C
/* 158D3: F0 03 */    BEQ ObjectRelocateHorizontally
/* 158D5: 20 9E 9F */ JSR Function9F9E
;passthru

ObjectRelocateHorizontally: ; different from UpdateObjectMoveHorizontally
;
; Input
;   $20 = New ObjectPosScreen
;   $21 = New ObjectPosXfraction
;   $22 = New ObjectPosX
;
/* 158D8: A6 2F */    LDX RefObjectNum

/* 158DA: A5 20 */    LDA $20
/* 158DC: 9D 60 04 */ STA ObjectPosScreen,X
/* 158DF: A5 22 */    LDA $22
/* 158E1: 9D 80 04 */ STA ObjectPosX,X

/* 158E4: A5 21 */    LDA $21
/* 158E6: 9D A0 04 */ STA ObjectPosXfraction,X
/* 158E9: 60 */       RTS

;
; Note: does *not* perform AI for bosses (or Megaman, for that matter)
;

RunEnemyAI:
/* 158EA: A9 00 */    LDA #$00
/* 158EC: 85 9A */    STA LiftIndex

/* 158EE: A2 02 */    LDX #$02
/* 158F0: 86 2F */    STX RefObjectNum

L158F2: ; ---
/* 158F2: A6 2F */    LDX RefObjectNum
/* 158F4: BD 00 06 */ LDA ObjectPosY,X
/* 158F7: C9 F8 */    CMP #$f8
/* 158F9: D0 03 */    BNE L158FE ; +                                               ; $98FE
/* 158FB: 4C 8A 99 */ JMP NextObject                                      ; NextObject
L158FE: ; +
/* 158FE: BD 00 04 */ LDA ObjectSpriteNum,X
/* 15901: C9 FF */    CMP #$ff
/* 15903: D0 06 */    BNE L1590B ; +                                               ; $990B
/* 15905: 20 29 AA */ JSR DoEnemyAI                                      ; $AA29
/* 15908: 4C 8A 99 */ JMP NextObject                                      ; NextObject
L1590B: ; +
/* 1590B: BD A0 06 */ LDA ObjectLifeCycleCounter,X
/* 1590E: D0 36 */    BNE L15946 ; +++    ; Skip the expensive stuff if the enemy is sleeping

/* 15910: BD 20 04 */ LDA ObjectFlags,X
/* 15913: 29 80 */    AND #$80
/* 15915: F0 05 */    BEQ L1591C ; +                                               ; $991C
; Object is hit by Megaman's weapon
/* 15917: A9 05 */    LDA #$05
/* 15919: 4C 96 99 */ JMP ObjectShouldBeDestroyed                              ; ObjectShouldBeDestroyed
L1591C: ; +
/* 1591C: BD 20 04 */ LDA ObjectFlags,X
/* 1591F: 29 08 */    AND #$08
/* 15921: F0 0A */    BEQ L1592D ; +                                               ; $992D
/* 15923: BD 40 04 */ LDA ObjectUnknown440,X
/* 15926: D0 05 */    BNE L1592D ; +                                               ; $992D
/* 15928: A9 00 */    LDA #$00
/* 1592A: 4C 96 99 */ JMP ObjectShouldBeDestroyed                              ; ObjectShouldBeDestroyed
L1592D: ; +
/* 1592D: A9 00 */    LDA #$00
/* 1592F: 85 2A */    STA $2A
/* 15931: 85 2B */    STA $2B
; if object is facing right then jsr ObjectUpdateMovementRight, else jsr ObjectUpdateMovementLeft
/* 15933: BD 20 04 */ LDA ObjectFlags,X
/* 15936: 29 40 */    AND #$40
/* 15938: F0 06 */    BEQ L15940 ; +                                               ; $9940
/* 1593A: 20 EF 9D */ JSR ObjectUpdateMovementRight
/* 1593D: 4C 43 99 */ JMP L15943 ; ++                                              ; $9943
L15940: ; +
/* 15940: 20 6D 9E */ JSR ObjectUpdateMovementLeft
L15943: ; ++
/* 15943: 20 D8 98 */ JSR ObjectRelocateHorizontally
L15946: ; +++ ;If the enemy is sleeping, the above part was skipped
/* 15946: 38 */       SEC
/* 15947: BD 80 04 */ LDA ObjectPosX,X
/* 1594A: E5 1A */    SBC ScrollPosX
/* 1594C: BD 60 04 */ LDA ObjectPosScreen,X
/* 1594F: E5 1B */    SBC ScrollPosScreen
/* 15951: F0 05 */    BEQ L15958 ; +                                               ; $9958
/* 15953: A9 01 */    LDA #$01
/* 15955: 4C 96 99 */ JMP ObjectShouldBeDestroyed
L15958: ; +
/* 15958: BD A0 06 */ LDA ObjectLifeCycleCounter,X
/* 1595B: D0 2D */    BNE NextObject
/* 1595D: A5 2A */    LDA $2A
/* 1595F: F0 05 */    BEQ L15966 ; +                                               ; $9966
/* 15961: A9 03 */    LDA #$03
/* 15963: 4C 96 99 */ JMP ObjectShouldBeDestroyed
L15966: ; +
/* 15966: BD 20 04 */ LDA ObjectFlags,X
/* 15969: 29 11 */    AND #$11
/* 1596B: D0 11 */    BNE L1597E ; +
/* 1596D: 20 A1 9C */ JSR ObjectCheckIfOutScreenVertically
/* 15970: B0 13 */    BCS L15985 ; ++

/* 15972: A5 24 */    LDA $24
/* 15974: 9D 20 06 */ STA ObjectPosYfraction,X
/* 15977: A5 25 */    LDA $25
/* 15979: 9D 00 06 */ STA ObjectPosY,X

/* 1597C: 90 0C */    BCC NextObject
L1597E: ; +
/* 1597E: 20 C4 9B */ JSR ObjectDoCollisionChecksAndAvoidWalls
/* 15981: A5 2B */    LDA $2B
/* 15983: F0 05 */    BEQ NextObject
L15985: ; ++
/* 15985: A9 04 */    LDA #$04
/* 15987: 4C 96 99 */ JMP ObjectShouldBeDestroyed

NextObject:
L1598A:
/* 1598A: E6 2F */    INC RefObjectNum
/* 1598C: A5 54 */    LDA TotalObjects
/* 1598E: C5 2F */    CMP RefObjectNum
/* 15990: F0 03 */    BEQ L15995 ; +                                               ; $9995
/* 15992: 4C F2 98 */ JMP L158F2 ; ---                                             ; $98F2
L15995: ; +
/* 15995: 60 */       RTS

ObjectShouldBeDestroyed:
; A = Hit type
;           0 = hit because ObjectFlags had bit #8 clear and Unknown440=0
;           1 = scrolled out from screen (at $9953)
;           3 = $2A was nonzero (at $9961)
;           4 = dropped in a pit (ObjectCheckIfOutScreenVertically returned carry set)
;           5 = hit by weapon
;
/* 15996: 85 53 */    STA ObjectReceivedHitType
/* 15998: A0 2D */    LDY #$2d
/* 1599A: A6 2F */    LDX RefObjectNum
/* 1599C: BD 00 04 */ LDA ObjectSpriteNum,X
L1599F: ; -
/* 1599F: D9 BB 99 */ CMP ObjectHitRoutineTable+0,Y
/* 159A2: F0 07 */    BEQ L159AB ; +                                               ; $99AB
/* 159A4: 88 */       DEY
/* 159A5: 88 */       DEY
/* 159A6: 88 */       DEY
/* 159A7: 10 F6 */    BPL L1599F ; -                                               ; $999F

/* 159A9: A0 00 */    LDY #$00
L159AB: ; +
/* 159AB: B9 BC 99 */ LDA ObjectHitRoutineTable+1,Y
/* 159AE: 85 04 */    STA $04
/* 159B0: B9 BD 99 */ LDA ObjectHitRoutineTable+2,Y
/* 159B3: 85 05 */    STA $05
/* 159B5: 6C 04 00 */ JMP ($0004)

/* 159B8: 4C 8A 99 */ JMP NextObject                                      ; $998A


ObjectHitRoutineTable:
    .byte $19
    .word Sprite19ShouldBeDestroyedAndDefault ; default
    .byte $27
    .word Sprite27ShouldBeDestroyed
    .byte $32
    .word Sprite32ShouldBeDestroyed
    .byte $36
    .word Sprite36ShouldBeDestroyed
    .byte $4C
    .word Sprite4Cto53ShouldBeDestroyed
    .byte $4D
    .word Sprite4Cto53ShouldBeDestroyed
    .byte $4E
    .word Sprite4Cto53ShouldBeDestroyed
    .byte $4F
    .word Sprite4Cto53ShouldBeDestroyed
    .byte $50
    .word Sprite4Cto53ShouldBeDestroyed
    .byte $51
    .word Sprite4Cto53ShouldBeDestroyed
    .byte $52
    .word Sprite4Cto53ShouldBeDestroyed
    .byte $53
    .word Sprite4Cto53ShouldBeDestroyed
    .byte $39
    .word Sprite39ShouldBeDestroyed
    .byte $5A
    .word Sprite5AShouldBeDestroyed
    .byte $6C
    .word Sprite6CShouldBeDestroyed
    .byte $5D
    .word Sprite5DShouldBeDestroyed

Sprite19ShouldBeDestroyedAndDefault:
/* 159EB: A5 53 */    LDA ObjectReceivedHitType
/* 159ED: C9 05 */    CMP #$05         ;Was it a weapon hit?
/* 159EF: D0 0B */    BNE DeleteObjectThatWasScrolledOut ; +
L159F1:
/* 159F1: BD 20 04 */ LDA ObjectFlags,X
/* 159F4: 29 7F */    AND #$7f
/* 159F6: 9D 20 04 */ STA ObjectFlags,X
/* 159F9: 4C 1C 99 */ JMP L1591C
; +
DeleteObjectThatWasScrolledOut:
L159FC:
/* 159FC: A6 2F */    LDX RefObjectNum
/* 159FE: A9 F8 */    LDA #$f8
/* 15A00: 9D 00 06 */ STA ObjectPosY,X
/* 15A03: 4C 8A 99 */ JMP NextObject                                      ; $998A

Sprite32ShouldBeDestroyed:
/* 15A06: E0 05 */    CPX #$05     ;
/* 15A08: D0 03 */    BNE L15A0D ; +
/* 15A0A: 4C 1B 9B */ JMP L15B1B
L15A0D: ; +
/* 15A0D: C6 44 */    DEC BossVariable44
/* 15A0F: BD 20 04 */ LDA ObjectFlags,X
/* 15A12: 29 08 */    AND #$08
/* 15A14: D0 E6 */    BNE DeleteObjectThatWasScrolledOut
/* 15A16: BD 60 04 */ LDA ObjectPosScreen,X
/* 15A19: 85 01 */    STA $01

/* 15A1B: BD 80 04 */ LDA ObjectPosX,X
/* 15A1E: 85 00 */    STA $00
/* 15A20: BD 00 06 */ LDA ObjectPosY,X
/* 15A23: 85 03 */    STA $03
/* 15A25: BD 20 04 */ LDA ObjectFlags,X
/* 15A28: 29 40 */    AND #$40
/* 15A2A: 09 02 */    ORA #$02
/* 15A2C: 85 02 */    STA $02

/* 15A2E: A9 F8 */    LDA #$f8
/* 15A30: 9D 00 06 */ STA ObjectPosY,X

/* 15A33: A0 00 */    LDY #$00
L15A35: ; -
/* 15A35: 84 0C */    STY $0C
/* 15A37: A2 10 */    LDX #$10
/* 15A39: 20 76 C5 */ JSR FindFreeObject
/* 15A3C: 20 E2 F7 */ JSR CreateGutsblockPieces
/* 15A3F: A4 0C */    LDY $0C
/* 15A41: C8 */       INY
/* 15A42: C0 04 */    CPY #$04
/* 15A44: D0 EF */    BNE L15A35 ; -                                               ; $9A35

/* 15A46: 4C 8A 99 */ JMP NextObject                                      ; $998A

Sprite36ShouldBeDestroyed:
/* 15A49: A5 53 */    LDA ObjectReceivedHitType
/* 15A4B: C9 05 */    CMP #$05
/* 15A4D: D0 03 */    BNE L15A52
/* 15A4F: 4C F1 99 */ JMP L159F1
L15A52:
/* 15A52: A6 2F */    LDX RefObjectNum
/* 15A54: FE 00 04 */ INC ObjectSpriteNum,X
/* 15A57: BD 20 04 */ LDA ObjectFlags,X
/* 15A5A: 29 F7 */    AND #$f7
/* 15A5C: 9D 20 04 */ STA ObjectFlags,X
/* 15A5F: 20 3D C5 */ JSR C53D_routine

/* 15A62: A9 2D */    LDA #$2d        ; Low-pitch noise (fire?...)
/* 15A64: 20 77 C4 */ JSR IssueSound                                      ; $C477

/* 15A67: 4C 8A 99 */ JMP NextObject                                      ; $998A


; Decrease BossVariable44 and disable object if BossCurrentStrategy >= #5
Sprite27ShouldBeDestroyed:
/* 15A6A: C6 44 */    DEC BossVariable44
/* 15A6C: A5 3E */    LDA BossCurrentStrategy
/* 15A6E: C9 05 */    CMP #$05
/* 15A70: B0 08 */    BCS L15A7A ; +                                               ; $9A7A
/* 15A72: A9 F8 */    LDA #$f8
/* 15A74: 9D 00 06 */ STA ObjectPosY,X
/* 15A77: 4C 8A 99 */ JMP NextObject                                      ; $998A
L15A7A: ; +
/* 15A7A: A9 0B */    LDA #$0b
/* 15A7C: 85 0C */    STA $0C
/* 15A7E: A9 39 */    LDA #$39
/* 15A80: 85 0D */    STA $0D
/* 15A82: A9 0A */    LDA #$0a
/* 15A84: 85 0E */    STA $0E
/* 15A86: 20 53 F8 */ JSR F1F853
L15A89:
/* 15A89: A2 10 */    LDX #$10
/* 15A8B: 20 76 C5 */ JSR FindFreeObject
/* 15A8E: A4 2F */    LDY RefObjectNum
/* 15A90: 20 41 F8 */ JSR CreateExplosionObject
/* 15A93: 10 F4 */    BPL L15A89

/* 15A95: A9 12 */    LDA #$12        ; Explosion
/* 15A97: 20 77 C4 */ JSR IssueSound                                      ; $C477

/* 15A9A: 4C 8A 99 */ JMP NextObject                                      ; $998A

Sprite4Cto53ShouldBeDestroyed:

/* 15A9D: A5 53 */    LDA ObjectReceivedHitType
/* 15A9F: C9 05 */    CMP #$05
/* 15AA1: D0 03 */    BNE L15AA6 ; +                                               ; $9AA6
/* 15AA3: 4C F1 99 */ JMP L159F1
L15AA6: ; +
/* 15AA6: C9 00 */    CMP #$00
/* 15AA8: D0 40 */    BNE L15AEA ; +
/* 15AAA: BD 00 04 */ LDA ObjectSpriteNum,X
/* 15AAD: 38 */       SEC
L15AAE: ; +
/* 15AAE: E9 4C */    SBC #$4c
/* 15AB0: A8 */       TAY
/* 15AB1: BD 20 04 */ LDA ObjectFlags,X
/* 15AB4: 29 40 */    AND #$40
/* 15AB6: F0 13 */    BEQ L15ACB ; +
/* 15AB8: 18 */       CLC
/* 15AB9: BD A0 04 */ LDA ObjectPosXfraction,X
/* 15ABC: 69 50 */    ADC #$50
/* 15ABE: 9D A0 04 */ STA ObjectPosXfraction,X
/* 15AC1: BD 80 04 */ LDA ObjectPosX,X
/* 15AC4: 79 F4 9A */ ADC Table9AF4,Y
/* 15AC7: B0 21 */    BCS L15AEA ; +++
/* 15AC9: 90 09 */    BCC L15AD4 ; ++
L15ACB: ; +
/* 15ACB: 38 */       SEC
/* 15ACC: BD 80 04 */ LDA ObjectPosX,X
/* 15ACF: F9 F4 9A */ SBC Table9AF4,Y
/* 15AD2: 90 16 */    BCC L15AEA ; +++
L15AD4: ; ++
/* 15AD4: 9D 80 04 */ STA ObjectPosX,X
/* 15AD7: 18 */       CLC
/* 15AD8: BD 00 06 */ LDA ObjectPosY,X
/* 15ADB: 79 FC 9A */ ADC Table9AFC,Y
/* 15ADE: 9D 00 06 */ STA ObjectPosY,X
/* 15AE1: B9 04 9B */ LDA Table9B04,Y
/* 15AE4: 9D 00 04 */ STA ObjectSpriteNum,X
/* 15AE7: 4C 8A 99 */ JMP NextObject                                      ; $998A
L15AEA: ; +++
; BossVariable44--, disable Object
/* 15AEA: C6 44 */    DEC BossVariable44
L15AEC:
/* 15AEC: A9 F8 */    LDA #$f8
/* 15AEE: 9D 00 06 */ STA ObjectPosY,X
/* 15AF1: 4C 8A 99 */ JMP NextObject                                      ; $998A

Table9AF4:
    .byt $1D,$21,$21,$1D,$19,$1D,$1D,$1D ;X increment/decrement
Table9AFC:
    .byt $FC,$E8,$00,$1C,$10,$F4,$FC,$FC ;Y increment
Table9B04:
    .byt $51,$52,$53,$4D,$4E,$4F,$50,$51 ;new spritenum

Sprite39ShouldBeDestroyed:
/* 15B0C: A5 53 */    LDA ObjectReceivedHitType
/* 15B0E: D0 DC */    BNE L15AEC
/* 15B10: A9 54 */    LDA #$54
/* 15B12: 9D 00 04 */ STA ObjectSpriteNum,X
/* 15B15: 4C 8A 99 */ JMP L1598A

Sprite5AShouldBeDestroyed:
/* 15B18: 4C FC 99 */ JMP L159FC

Sprite6CShouldBeDestroyed:
L15B1B:
/* 15B1B: BD 20 04 */ LDA ObjectFlags,X
/* 15B1E: 29 20 */    AND #$20
/* 15B20: D0 42 */    BNE L15B64
/* 15B22: A5 53 */    LDA ObjectReceivedHitType
/* 15B24: C9 04 */    CMP #$04
/* 15B26: D0 1F */    BNE L15B47
/* 15B28: A5 2B */    LDA $2B
/* 15B2A: C9 FF */    CMP #$ff
/* 15B2C: D0 1D */    BNE L15B4B
/* 15B2E: BD 80 06 */ LDA ObjectYSpeed,X
/* 15B31: 30 2C */    BMI L15B5F
L15B33:
/* 15B33: 38 */       SEC
/* 15B34: BD 60 06 */ LDA ObjectYSpeedFraction,X
/* 15B37: E9 40 */    SBC #$40
/* 15B39: 9D 60 06 */ STA ObjectYSpeedFraction,X
/* 15B3C: BD 80 06 */ LDA ObjectYSpeed,X
/* 15B3F: E9 00 */    SBC #$00
/* 15B41: 9D 80 06 */ STA ObjectYSpeed,X
/* 15B44: 4C 8A 99 */ JMP L1598A
L15B47:
/* 15B47: C9 03 */    CMP #$03
/* 15B49: D0 14 */    BNE L15B5F
L15B4B:
/* 15B4B: BD 00 06 */ LDA ObjectPosY,X
/* 15B4E: C9 E0 */    CMP #$e0
/* 15B50: 90 07 */    BCC L15B59
/* 15B52: A9 04 */    LDA #$04
/* 15B54: 9D 00 06 */ STA ObjectPosY,X
/* 15B57: D0 DA */    BNE L15B33
L15B59:
/* 15B59: 20 35 A3 */ JSR F16335
/* 15B5C: 4C 8A 99 */ JMP L1598A

L15B5F:
/* 15B5F: A9 F8 */    LDA #$f8
/* 15B61: 9D 00 06 */ STA ObjectPosY,X
L15B64:
/* 15B64: 4C 8A 99 */ JMP L1598A

Sprite5DShouldBeDestroyed:
/* 15B67: A5 53 */    LDA ObjectReceivedHitType
/* 15B69: C9 04 */    CMP #$04
/* 15B6B: D0 13 */    BNE L15B80
/* 15B6D: A5 2B */    LDA $2B
/* 15B6F: C9 FF */    CMP #$ff
/* 15B71: D0 22 */    BNE L15B95
/* 15B73: BD 80 06 */ LDA ObjectYSpeed,X
/* 15B76: 30 44 */    BMI L15BBC
/* 15B78: A9 04 */    LDA #$04
/* 15B7A: 9D 00 06 */ STA ObjectPosY,X
L15B7D:
/* 15B7D: 4C 33 9B */ JMP L15B33
L15B80:
/* 15B80: C9 03 */    CMP #$03
/* 15B82: D0 38 */    BNE L15BBC
/* 15B84: BD 00 06 */ LDA ObjectPosY,X
/* 15B87: C9 F0 */    CMP #$f0
/* 15B89: 90 07 */    BCC L15B92
/* 15B8B: A9 04 */    LDA #$04
/* 15B8D: 9D 00 06 */ STA ObjectPosY,X
/* 15B90: D0 EB */    BNE L15B7D
L15B92:
/* 15B92: 4C 66 99 */ JMP L15966
L15B95:
/* 15B95: BD 80 06 */ LDA ObjectYSpeed,X
/* 15B98: 30 0D */    BMI L15BA7
/* 15B9A: A9 FF */    LDA #$ff
/* 15B9C: 9D 80 06 */ STA ObjectYSpeed,X
/* 15B9F: A9 40 */    LDA #$40
/* 15BA1: 9D 60 06 */ STA ObjectYSpeedFraction,X
/* 15BA4: 4C 8A 99 */ JMP L1598A
L15BA7:
/* 15BA7: A5 62 */    LDA $62
/* 15BA9: F0 02 */    BEQ L15BAD
/* 15BAB: C6 62 */    DEC $62
L15BAD:
/* 15BAD: 9D 80 06 */ STA ObjectYSpeed,X
/* 15BB0: 9D 60 06 */ STA ObjectYSpeedFraction,X
/* 15BB3: 5E C0 04 */ LSR ObjectXSpeed,X
/* 15BB6: 7E E0 04 */ ROR ObjectXSpeedFraction,X
/* 15BB9: 4C 8A 99 */ JMP L1598A
L15BBC:
/* 15BBC: A9 F8 */    LDA #$f8
/* 15BBE: 9D 00 06 */ STA ObjectPosY,X
/* 15BC1: 4C 8A 99 */ JMP L1598A


ObjectDoCollisionChecksAndAvoidWalls:
/* 15BC4: A6 2F */    LDX RefObjectNum
/* 15BC6: 20 A1 9C */ JSR ObjectCheckIfOutScreenVertically
/* 15BC9: 90 12 */    BCC L15BDD ; ++
/* 15BCB: A6 2F */    LDX RefObjectNum
/* 15BCD: D0 09 */    BNE L15BD8 ; +
; Megaman is falling out of screen.
/* 15BCF: AD 20 04 */ LDA ObjectFlags+0
/* 15BD2: 29 10 */    AND #$10         ;Is Megaman climbing?
/* 15BD4: F0 12 */    BEQ L15BE8 ; +++
/* 15BD6: 18 */       CLC              ;If climbing, he's safe.
/* 15BD7: 60 */       RTS
L15BD8: ; +
/* 15BD8: A9 FF */    LDA #$ff
/* 15BDA: 85 2B */    STA $2B
/* 15BDC: 60 */       RTS
L15BDD: ; ++
/* 15BDD: E0 00 */    CPX #$00
/* 15BDF: F0 07 */    BEQ L15BE8 ; +++
/* 15BE1: BD 20 04 */ LDA ObjectFlags,X
/* 15BE4: 29 01 */    AND #$01
/* 15BE6: F0 71 */    BEQ L15C59 ; ++++
L15BE8: ; +++
/* 15BE8: BC 00 04 */ LDY ObjectSpriteNum,X
/* 15BEB: C0 FF */    CPY #$ff
/* 15BED: D0 08 */    BNE L15BF7 ; +
/* 15BEF: BC E0 06 */ LDY ObjectType,X
/* 15BF2: B9 3A FC */ LDA TableObjectYHeightTable1,Y
/* 15BF5: D0 03 */    BNE L15BFA ; ++
L15BF7: ; +
/* 15BF7: B9 B7 FB */ LDA TableObjectYHeightTable2,Y
L15BFA: ; ++
/* 15BFA: 85 10 */    STA $10
/* 15BFC: BD 80 06 */ LDA ObjectYSpeed,X
/* 15BFF: 30 30 */    BMI L15C31 ; +
/* 15C01: A5 10 */    LDA $10
/* 15C03: 49 FF */    EOR #$ff
/* 15C05: 18 */       CLC
/* 15C06: 69 01 */    ADC #$01
/* 15C08: 20 8E 9C */ JSR ObjectCollisionCheckHelper
/* 15C0B: F0 4C */    BEQ L15C59 ; ++++
/* 15C0D: A6 2F */    LDX RefObjectNum
/* 15C0F: C9 20 */    CMP #$20
/* 15C11: F0 3A */    BEQ L15C4D ; ++
/* 15C13: A5 03 */    LDA $03
/* 15C15: 29 F0 */    AND #$f0
/* 15C17: 18 */       CLC
/* 15C18: 69 10 */    ADC #$10
/* 15C1A: 18 */       CLC
/* 15C1B: 65 10 */    ADC $10
/* 15C1D: 9D 00 06 */ STA ObjectPosY,X
/* 15C20: A9 00 */    LDA #$00
/* 15C22: 9D 20 06 */ STA ObjectPosYfraction,X
/* 15C25: A9 FF */    LDA #$ff
/* 15C27: 9D 80 06 */ STA ObjectYSpeed,X
/* 15C2A: A9 40 */    LDA #$40
/* 15C2C: 9D 60 06 */ STA ObjectYSpeedFraction,X
/* 15C2F: D0 1C */    BNE L15C4D ; ++
L15C31: ; +
/* 15C31: A5 10 */    LDA $10
/* 15C33: 20 8E 9C */ JSR ObjectCollisionCheckHelper
/* 15C36: F0 21 */    BEQ L15C59 ; ++++
/* 15C38: A6 2F */    LDX RefObjectNum
/* 15C3A: C9 20 */    CMP #$20
/* 15C3C: F0 0F */    BEQ L15C4D ; ++
/* 15C3E: A5 03 */    LDA $03
/* 15C40: 29 F0 */    AND #$f0
/* 15C42: 38 */       SEC
/* 15C43: E5 10 */    SBC $10
/* 15C45: 9D 00 06 */ STA ObjectPosY,X
/* 15C48: A9 00 */    LDA #$00
/* 15C4A: 9D 20 06 */ STA ObjectPosYfraction,X
L15C4D: ; ++
/* 15C4D: A9 FF */    LDA #$ff
/* 15C4F: 9D 80 06 */ STA ObjectYSpeed,X
/* 15C52: A9 40 */    LDA #$40
/* 15C54: 9D 60 06 */ STA ObjectYSpeedFraction,X
/* 15C57: 38 */       SEC
/* 15C58: 60 */       RTS
L15C59: ; ++++
/* 15C59: 38 */       SEC
/* 15C5A: A6 2F */    LDX RefObjectNum
/* 15C5C: F0 07 */    BEQ L15C65 ; +
/* 15C5E: BD 20 04 */ LDA ObjectFlags,X
/* 15C61: 29 10 */    AND #$10
/* 15C63: F0 1D */    BEQ L15C82 ; ++
L15C65: ; +
/* 15C65: BD 60 06 */ LDA ObjectYSpeedFraction,X
/* 15C68: E9 40 */    SBC #$40
/* 15C6A: 9D 60 06 */ STA ObjectYSpeedFraction,X
/* 15C6D: BD 80 06 */ LDA ObjectYSpeed,X
/* 15C70: E9 00 */    SBC #$00
/* 15C72: 10 0B */    BPL L15C7F ; +
/* 15C74: C9 F4 */    CMP #$f4
/* 15C76: B0 07 */    BCS L15C7F ; +
/* 15C78: A9 00 */    LDA #$00
/* 15C7A: 9D 60 06 */ STA ObjectYSpeedFraction,X
/* 15C7D: A9 F4 */    LDA #$f4
L15C7F: ; +
/* 15C7F: 9D 80 06 */ STA ObjectYSpeed,X
L15C82: ; ++
/* 15C82: A5 25 */    LDA $25
/* 15C84: 9D 00 06 */ STA ObjectPosY,X
/* 15C87: A5 24 */    LDA $24
/* 15C89: 9D 20 06 */ STA ObjectPosYfraction,X
/* 15C8C: 18 */       CLC
/* 15C8D: 60 */       RTS

ObjectCollisionCheckHelper:
/* 15C8E: 18 */       CLC
/* 15C8F: 65 25 */    ADC $25
/* 15C91: 85 03 */    STA $03
/* 15C93: BD 60 04 */ LDA ObjectPosScreen,X
/* 15C96: 85 01 */    STA $01
/* 15C98: BD 80 04 */ LDA ObjectPosX,X
/* 15C9B: 85 00 */    STA $00
/* 15C9D: 20 2D CC */ JSR DoCollisionCheckFor
/* 15CA0: 60 */       RTS

ObjectCheckIfOutScreenVertically:
/* 15CA1: 38 */       SEC
/* 15CA2: BD 20 06 */ LDA ObjectPosYfraction,X
/* 15CA5: FD 60 06 */ SBC ObjectYSpeedFraction,X
/* 15CA8: 85 24 */    STA $24
/* 15CAA: BD 00 06 */ LDA ObjectPosY,X
/* 15CAD: FD 80 06 */ SBC ObjectYSpeed,X
/* 15CB0: 85 25 */    STA $25
/* 15CB2: C9 E8 */    CMP #$E8
/* 15CB4: 90 08 */    BCC L15CBE ; +
; Y >= #$E8
/* 15CB6: C9 F8 */    CMP #$F8
/* 15CB8: B0 0A */    BCS L15CC4 ; ++
; Y >= #$E8 but <= #$F8
/* 15CBA: A9 03 */    LDA #$03
/* 15CBC: D0 1F */    BNE L15CDD ; +++
; Y < #$E8
L15CBE: ; +
/* 15CBE: C9 04 */    CMP #$04
/* 15CC0: 90 19 */    BCC L15CDB ; +                                               ; $9CDB
; Y >= #$04 and < #$E8
/* 15CC2: B0 21 */    BCS L15CE5 ; ++++
L15CC4: ; ++
; Y >= #$F8
/* 15CC4: C9 FC */    CMP #$FC
/* 15CC6: B0 1D */    BCS L15CE5 ; ++++
; Y >= #$F8 but < #$FC
/* 15CC8: E0 00 */    CPX #$00
/* 15CCA: D0 0F */    BNE L15CDB ; +                                               ; $9CDB
; but it's Megaman
/* 15CCC: AD 80 06 */ LDA ObjectYSpeed+0
/* 15CCF: 30 0A */    BMI L15CDB ; +                                               ; $9CDB
/* 15CD1: AD 20 06 */ LDA ObjectPosYfraction+0
/* 15CD4: 85 24 */    STA $24
/* 15CD6: AD 00 06 */ LDA ObjectPosY+0
/* 15CD9: 85 25 */    STA $25
L15CDB:
/* 15CDB: A9 01 */    LDA #$01
L15CDD: ; +++
/* 15CDD: E0 00 */    CPX #$00
/* 15CDF: D0 02 */    BNE L15CE3 ; +                                               ; $9CE3
/* 15CE1: 85 26 */    STA CurrentStripeEndType
L15CE3: ; +
/* 15CE3: 38 */       SEC
/* 15CE4: 60 */       RTS
L15CE5: ; ++++
/* 15CE5: 18 */       CLC
/* 15CE6: 60 */       RTS


F15CE7:
/* 15CE7: A0 00 */    LDY #$00
/* 15CE9: AD 00 04 */ LDA ObjectSpriteNum+0
/* 15CEC: A2 08 */    LDX #$08
L15CEE: ; -
/* 15CEE: DD DD 9D */ CMP Table9DDD,X
/* 15CF1: F0 0D */    BEQ L15D00 ; +                                               ; $9D00
/* 15CF3: CA */       DEX
/* 15CF4: 10 F8 */    BPL L15CEE ; -                                               ; $9CEE
/* 15CF6: A5 14 */    LDA JoyPad0
/* 15CF8: 29 C0 */    AND #$c0
/* 15CFA: F0 07 */    BEQ L15D03 ; ++                                              ; $9D03
/* 15CFC: A0 07 */    LDY #$07
/* 15CFE: D0 03 */    BNE L15D03 ; ++                                              ; $9D03
L15D00: ; +
/* 15D00: BC E6 9D */ LDY Table9DE6,X
L15D03: ; ++
/* 15D03: A2 00 */    LDX #$00
/* 15D05: 20 5D C5 */ JSR F1C55D
/* 15D08: A5 96 */    LDA $96
/* 15D0A: D0 08 */    BNE L15D14
/* 15D0C: A5 98 */    LDA $98
/* 15D0E: 05 99 */    ORA $99
/* 15D10: F0 60 */    BEQ L15D72
/* 15D12: D0 14 */    BNE L15D28
L15D14:
/* 15D14: A5 14 */    LDA JoyPad0
/* 15D16: 29 C0 */    AND #$c0
/* 15D18: F0 0E */    BEQ L15D28
/* 15D1A: 29 80 */    AND #$80
/* 15D1C: F0 06 */    BEQ L15D24
/* 15D1E: A5 97 */    LDA $97
/* 15D20: D0 39 */    BNE L15D5B
/* 15D22: F0 04 */    BEQ L15D28
L15D24:
/* 15D24: A5 97 */    LDA $97
/* 15D26: F0 33 */    BEQ L15D5B



L15D28:
/* 15D28: A2 00 */    LDX #$00
/* 15D2A: AD 00 04 */ LDA ObjectSpriteNum+0
/* 15D2D: C9 09 */    CMP #$09
/* 15D2F: D0 02 */    BNE L15D33
/* 15D31: A2 02 */    LDX #$02
L15D33:
/* 15D33: 38 */       SEC
/* 15D34: A5 98 */    LDA $98
/* 15D36: FD D9 9D */ SBC Table9DD9+0,X
/* 15D39: 85 98 */    STA $98
/* 15D3B: A5 99 */    LDA $99
/* 15D3D: FD DA 9D */ SBC Table9DD9+1,X
/* 15D40: 85 99 */    STA $99
/* 15D42: A2 00 */    LDX #$00
/* 15D44: B0 08 */    BCS L15D4E
/* 15D46: A9 00 */    LDA #$00
/* 15D48: 85 98 */    STA $98
/* 15D4A: 85 99 */    STA $99
/* 15D4C: F0 24 */    BEQ L15D72


L15D4E:
/* 15D4E: A5 99 */    LDA $99
/* 15D50: 8D C0 04 */ STA ObjectXSpeed+0
/* 15D53: A5 98 */    LDA $98
/* 15D55: 8D E0 04 */ STA ObjectXSpeedFraction+0
/* 15D58: 4C 79 9D */ JMP L15D79



L15D5B:
/* 15D5B: 38 */       SEC
/* 15D5C: AD E0 04 */ LDA ObjectXSpeedFraction+0
/* 15D5F: E5 98 */    SBC $98
/* 15D61: AD C0 04 */ LDA ObjectXSpeed+0
/* 15D64: E5 99 */    SBC $99
/* 15D66: 90 C0 */    BCC L15D28
/* 15D68: AD C0 04 */ LDA ObjectXSpeed+0
/* 15D6B: 85 99 */    STA $99
/* 15D6D: AD E0 04 */ LDA ObjectXSpeedFraction+0
/* 15D70: 85 98 */    STA $98

L15D72:
/* 15D72: AD 20 04 */ LDA ObjectFlags+0
/* 15D75: 29 40 */    AND #$40
/* 15D77: 85 97 */    STA $97



L15D79:
/* 15D79: A5 9E */    LDA LiftUnknown9E
/* 15D7B: 10 57 */    BPL L15DD4
/* 15D7D: 29 7F */    AND #$7f
/* 15D7F: 85 9B */    STA LiftUnknown9B

/* 15D81: AD 20 04 */ LDA ObjectFlags+0
/* 15D84: 29 40 */    AND #$40
/* 15D86: C5 9B */    CMP LiftUnknown9B
/* 15D88: F0 34 */    BEQ L15DBE
/* 15D8A: 38 */       SEC
/* 15D8B: AD E0 04 */ LDA ObjectXSpeedFraction+0
/* 15D8E: E5 9C */    SBC LiftXSpeedFraction
/* 15D90: 8D E0 04 */ STA ObjectXSpeedFraction+0
/* 15D93: AD C0 04 */ LDA ObjectXSpeed+0
/* 15D96: E5 9D */    SBC LiftXSpeed
/* 15D98: 8D C0 04 */ STA ObjectXSpeed+0
/* 15D9B: B0 17 */    BCS L15DB4
/* 15D9D: AD E0 04 */ LDA ObjectXSpeedFraction+0
/* 15DA0: 49 FF */    EOR #$ff
/* 15DA2: 69 01 */    ADC #$01
/* 15DA4: 8D E0 04 */ STA ObjectXSpeedFraction+0
/* 15DA7: AD C0 04 */ LDA ObjectXSpeed+0
/* 15DAA: 49 FF */    EOR #$ff
/* 15DAC: 69 00 */    ADC #$00
/* 15DAE: 8D C0 04 */ STA ObjectXSpeed+0
/* 15DB1: 4C CF 9D */ JMP L15DCF
L15DB4:
/* 15DB4: BD 20 04 */ LDA ObjectFlags,X
/* 15DB7: 29 40 */    AND #$40
/* 15DB9: 85 9B */    STA LiftUnknown9B
/* 15DBB: 4C CF 9D */ JMP L15DCF


L15DBE:
/* 15DBE: 18 */       CLC
/* 15DBF: AD E0 04 */ LDA ObjectXSpeedFraction+0
/* 15DC2: 65 9C */    ADC LiftXSpeedFraction
/* 15DC4: 8D E0 04 */ STA ObjectXSpeedFraction+0
/* 15DC7: AD C0 04 */ LDA ObjectXSpeed+0
/* 15DCA: 65 9D */    ADC LiftXSpeed
/* 15DCC: 8D C0 04 */ STA ObjectXSpeed+0


L15DCF:
/* 15DCF: A9 00 */    LDA #$00
/* 15DD1: 85 9E */    STA LiftUnknown9E
/* 15DD3: 60 */       RTS

L15DD4:
/* 15DD4: A5 97 */    LDA $97
/* 15DD6: 85 9B */    STA LiftUnknown9B
/* 15DD8: 60 */       RTS

Table9DD9:
    .byte $04,$00
    .byte $80,$00

Table9DDD:
    .byte $00,$03,$06,$0C,$0F,$12,$13,$14,$6E
Table9DE6:
    .byte $00,$01,$08,$03,$00,$09,$01,$00,$00

ObjectUpdateMovementRight:
;
; Output (these variables will go to AutoCenterScreen):
;   $20 = New ObjectPosScreen
;   $21 = New ObjectPosXfraction
;   $22 = New ObjectPosX
;
/* 15DEF: A6 2F */    LDX RefObjectNum

ObjectMoveToTheRight:
/* 15DF1: 18 */       CLC
/* 15DF2: BD A0 04 */ LDA ObjectPosXfraction,X
/* 15DF5: 7D E0 04 */ ADC ObjectXSpeedFraction,X
/* 15DF8: 85 21 */    STA $21

/* 15DFA: BD 80 04 */ LDA ObjectPosX,X
/* 15DFD: 7D C0 04 */ ADC ObjectXSpeed,X
/* 15E00: A8 */       TAY

/* 15E01: BD 60 04 */ LDA ObjectPosScreen,X
/* 15E04: 69 00 */    ADC #$00

/* 15E06: A6 2F */    LDX RefObjectNum ;Is it Megaman?
/* 15E08: F0 0C */    BEQ L15E16 ; ++
/* 15E0A: 85 20 */    STA $20
/* 15E0C: 84 22 */    STY $22
/* 15E0E: BD 20 04 */ LDA ObjectFlags,X
/* 15E11: 29 01 */    AND #$01         ; Collides with background?
/* 15E13: D0 1B */    BNE L15E30 ; ++++
/* 15E15: 60 */       RTS
; If it's Megaman
L15E16: ; ++
/* 15E16: C5 28 */    CMP CurrentEndScreen
/* 15E18: F0 04 */    BEQ L15E1E ; +                                               ; $9E1E
/* 15E1A: B0 06 */    BCS L15E22 ; ++                                              ; $9E22
/* 15E1C: D0 0E */    BNE L15E2C ; +++                                             ; $9E2C
L15E1E: ; +
/* 15E1E: C0 EF */    CPY #$EF
/* 15E20: 90 0A */    BCC L15E2C ; +++                                             ; $9E2C
L15E22: ; ++
/* 15E22: A5 28 */    LDA CurrentEndScreen
/* 15E24: A0 EF */    LDY #$EF
/* 15E26: A2 02 */    LDX #$02
/* 15E28: 86 26 */    STX CurrentStripeEndType
/* 15E2A: A2 00 */    LDX #$00
L15E2C: ; +++
/* 15E2C: 85 20 */    STA $20
/* 15E2E: 84 22 */    STY $22
L15E30: ; ++++
; Do background collision checks
/* 15E30: BC 00 04 */ LDY ObjectSpriteNum,X
/* 15E33: C0 FF */    CPY #$FF
/* 15E35: D0 08 */    BNE L15E3F ; +
/* 15E37: BC E0 06 */ LDY ObjectType,X
/* 15E3A: B9 6C FB */ LDA TableObjectXWidthTable1,Y
/* 15E3D: D0 03 */    BNE L15E42 ; ++
L15E3F: ; +
/* 15E3F: B9 E9 FA */ LDA TableObjectXWidthTable2,Y
L15E42: ; ++
/* 15E42: 85 10 */    STA $10
/* 15E44: 18 */       CLC
/* 15E45: A5 22 */    LDA $22
/* 15E47: 65 10 */    ADC $10
/* 15E49: 85 00 */    STA $00
/* 15E4B: A5 20 */    LDA $20
/* 15E4D: 69 00 */    ADC #$00
/* 15E4F: 85 01 */    STA $01
/* 15E51: BD 00 06 */ LDA ObjectPosY,X
/* 15E54: 85 03 */    STA $03
/* 15E56: 20 6F CB */ JSR ObjectVerifyBackgroundCollision
/* 15E59: F0 11 */    BEQ L15E6C ; +
/* 15E5B: A5 00 */     LDA $00
/* 15E5D: 29 F0 */     AND #$f0
/* 15E5F: A6 2F */     LDX RefObjectNum
/* 15E61: 38 */        SEC
/* 15E62: E5 10 */     SBC $10
/* 15E64: 85 22 */     STA $22
/* 15E66: A5 01 */     LDA $01
/* 15E68: E9 00 */     SBC #$00
/* 15E6A: 85 20 */     STA $20
L15E6C: ; +
/* 15E6C: 60 */       RTS



ObjectUpdateMovementLeft:
;
; Output (these variables will go to ObjectRelocateHorizontally (and possibly to AutoCenterScreen)):
;   $20 = New ObjectPosScreen
;   $21 = New ObjectPosXfraction
;   $22 = New ObjectPosX
;
/* 15E6D: A6 2F */    LDX RefObjectNum

ObjectMoveToTheLeft:
/* 15E6F: 38 */       SEC
/* 15E70: BD A0 04 */ LDA ObjectPosXfraction,X
/* 15E73: FD E0 04 */ SBC ObjectXSpeedFraction,X
/* 15E76: 85 21 */    STA $21

/* 15E78: BD 80 04 */ LDA ObjectPosX,X
/* 15E7B: FD C0 04 */ SBC ObjectXSpeed,X
/* 15E7E: A8 */       TAY
/* 15E7F: BD 60 04 */ LDA ObjectPosScreen,X
/* 15E82: E9 00 */    SBC #$00

/* 15E84: A6 2F */    LDX RefObjectNum ;Is it Megaman?
/* 15E86: F0 0C */    BEQ L15E94 ; +
/* 15E88: 85 20 */    STA $20
/* 15E8A: 84 22 */    STY $22
/* 15E8C: BD 20 04 */ LDA ObjectFlags,X
/* 15E8F: 29 01 */    AND #$01            ;Does it collide with background?
/* 15E91: D0 1D */    BNE L15EB0 ; ++++
/* 15E93: 60 */       RTS
L15E94: ; + ;was Megaman
/* 15E94: C5 27 */    CMP CurrentBeginScreen
/* 15E96: 30 0A */    BMI L15EA2 ; ++
/* 15E98: F0 04 */    BEQ L15E9E ; +
/* 15E9A: 90 06 */    BCC L15EA2 ; ++
/* 15E9C: D0 0E */    BNE L15EAC ; +++
L15E9E: ; +
/* 15E9E: C0 10 */    CPY #$10
/* 15EA0: B0 0A */    BCS L15EAC ; +++
L15EA2: ; ++
/* 15EA2: A5 27 */    LDA CurrentBeginScreen
/* 15EA4: A0 10 */    LDY #$10
/* 15EA6: A2 04 */    LDX #$04
/* 15EA8: 86 26 */    STX CurrentStripeEndType
/* 15EAA: A2 00 */    LDX #$00
L15EAC: ; +++
/* 15EAC: 85 20 */    STA $20
/* 15EAE: 84 22 */    STY $22
L15EB0: ; ++++;Do background collisions
/* 15EB0: BC 00 04 */ LDY ObjectSpriteNum,X
/* 15EB3: C0 FF */    CPY #$ff
/* 15EB5: D0 08 */    BNE L15EBF ; +
/* 15EB7: BC E0 06 */ LDY ObjectType,X
/* 15EBA: B9 6C FB */ LDA TableObjectXWidthTable1,Y
/* 15EBD: D0 03 */    BNE L15EC2 ; ++
L15EBF: ; +
/* 15EBF: B9 E9 FA */ LDA TableObjectXWidthTable2,Y
L15EC2: ; ++
/* 15EC2: 85 10 */    STA $10
/* 15EC4: 38 */       SEC
/* 15EC5: A5 22 */    LDA $22
/* 15EC7: E5 10 */    SBC $10
/* 15EC9: 85 00 */    STA $00
/* 15ECB: A5 20 */    LDA $20
/* 15ECD: E9 00 */    SBC #$00
/* 15ECF: 85 01 */    STA $01
/* 15ED1: BD 00 06 */ LDA ObjectPosY,X
/* 15ED4: 85 03 */    STA $03
/* 15ED6: 20 6F CB */ JSR ObjectVerifyBackgroundCollision
/* 15ED9: F0 1B */    BEQ L15EF6 ; +
/* 15EDB: 18 */        CLC
/* 15EDC: A6 2F */     LDX RefObjectNum
/* 15EDE: BC 00 04 */  LDY ObjectSpriteNum,X
/* 15EE1: A5 10 */     LDA $10
/* 15EE3: 69 10 */     ADC #$10
/* 15EE5: 85 0F */     STA $0F
/* 15EE7: A5 00 */     LDA $00
/* 15EE9: 29 F0 */     AND #$f0
/* 15EEB: 18 */        CLC
/* 15EEC: 65 0F */     ADC $0F
/* 15EEE: 85 22 */     STA $22
/* 15EF0: A5 01 */     LDA $01
/* 15EF2: 69 00 */     ADC #$00
/* 15EF4: 85 20 */     STA $20
L15EF6: ; +
/* 15EF6: 60 */       RTS


SecondDoorLocations: ;at 9EF7
    .byte $16,$16,$FF,$12,$FF,$11
    .byte $FF,$FF,$FF,$22,$20
FirstDoorLocations: ;at 9F02
    .byte $13,$13,$13,$0F,$13,$0E
    .byte $FF,$FF,$FF,$FF,$FF

AfterDoorsPalette: ; At 9F0D
    ;Stage 0
    .byte $0F,$30,$2B,$06,$0F,$30,$2B,$2C
    .byte $0F,$20,$10,$00,$0F,$20,$10,$2C
    ;Stage 1
    .byte $0F,$20,$21,$11,$0F,$20,$3C,$2C
    .byte $0F,$20,$21,$2C,$0F,$20,$09,$1C
    ;Stage 2
    .byte $0F,$20,$10,$11,$0F,$20,$10,$21
    .byte $0F,$29,$19,$07,$0F,$27,$06,$21
    ;Stage 3
    .byte $0F,$30,$10,$06,$0F,$1B,$0B,$00
    .byte $0F,$27,$16,$06,$0F,$26,$16,$06
    ;Stage 4
    .byte $0F,$20,$27,$17,$0F,$20,$27,$21
    .byte $0F,$0F,$0F,$0C,$0F,$28,$29,$18
    ;Stage 5
    .byte $0F,$26,$17,$07,$0F,$30,$10,$02
    .byte $0F,$26,$17,$02,$0F,$37,$27,$17

Function9F6D: ;called only from 98CB
/* 15F6D: A9 41 */    LDA #$41
/* 15F6F: 85 8D */    STA ScreenMovedFlag

; Render nothing to nametable
/* 15F71: A2 00 */    LDX #$00
/* 15F73: 86 1C */    STX TSAPPUtransferSize
/* 15F75: 86 0D */    STX $0D

/* 15F77: AE 60 04 */ LDX ObjectPosScreen+0                             ; $0460
/* 15F7A: E8 */       INX
/* 15F7B: 86 05 */    STX $05
/* 15F7D: AD 80 04 */ LDA ObjectPosX+0
/* 15F80: 85 04 */    STA $04
L15F82: ; -
/* 15F82: A5 05 */    LDA $05
/* 15F84: C5 28 */    CMP CurrentEndScreen
/* 15F86: F0 02 */    BEQ L15F8A ; +                                               ; $9F8A
/* 15F88: B0 13 */    BCS L15F9D ; ++                                              ; $9F9D
L15F8A: ; +
/* 15F8A: A5 04 */    LDA $04
/* 15F8C: 29 03 */    AND #$03
/* 15F8E: D0 03 */    BNE L15F93 ; +                                               ; $9F93
/* 15F90: 20 E8 CD */ JSR DrawBlockFromActiveLevelMap
L15F93: ; +
/* 15F93: E6 04 */    INC $04
/* 15F95: D0 02 */    BNE L15F99 ; +                                               ; $9F99
/* 15F97: E6 05 */    INC $05
L15F99: ; +
/* 15F99: C6 0C */    DEC $0C
/* 15F9B: D0 E5 */    BNE L15F82 ; -                                               ; $9F82
L15F9D: ; ++
/* 15F9D: 60 */       RTS


Function9F9E: ;only called from 98D5
/* 15F9E: A9 01 */    LDA #$01
/* 15FA0: 85 8D */    STA ScreenMovedFlag

; Render nothing to nametable
/* 15FA2: A9 00 */    LDA #$00
/* 15FA4: 85 1C */    STA TSAPPUtransferSize
/* 15FA6: 85 0D */    STA $0D

/* 15FA8: AE 60 04 */ LDX ObjectPosScreen+0                             ; $0460
/* 15FAB: CA */       DEX
/* 15FAC: 86 05 */    STX $05
/* 15FAE: AD 80 04 */ LDA ObjectPosX+0
/* 15FB1: 85 04 */    STA $04

Function9FB3: ;only called from A0EC
/* 15FB3: A5 05 */    LDA $05
/* 15FB5: 30 E6 */    BMI L15F9D ; +++        ;Actually 9F9D, but still an RTS
/* 15FB7: C5 27 */    CMP CurrentBeginScreen
/* 15FB9: 90 1F */    BCC L15FDA ; +++
/* 15FBB: A5 04 */    LDA $04
/* 15FBD: 29 03 */    AND #$03
/* 15FBF: D0 0D */    BNE L15FCE ; ++                                              ; $9FCE
/* 15FC1: A5 04 */    LDA $04
/* 15FC3: 29 10 */    AND #$10
/* 15FC5: F0 04 */    BEQ L15FCB ; +                                               ; $9FCB
/* 15FC7: A5 BF */    LDA $BF
/* 15FC9: D0 03 */    BNE L15FCE ; ++                                              ; $9FCE
L15FCB: ; +
/* 15FCB: 20 E8 CD */ JSR DrawBlockFromActiveLevelMap
L15FCE: ; ++
/* 15FCE: A5 04 */    LDA $04
/* 15FD0: D0 02 */    BNE L15FD4 ; +
/* 15FD2: C6 05 */    DEC $05
L15FD4: ; +
/* 15FD4: C6 04 */    DEC $04
/* 15FD6: C6 0C */    DEC $0C
/* 15FD8: D0 D9 */    BNE Function9FB3
L15FDA: ; +++
/* 15FDA: 60 */       RTS

; Input:
;   CurrentStripeEndType &3:
;        0: horiz: right
;        1: vert:  up
;        2: horiz: left
;        3: vert:  down
DoScrolling:
/* 15FDB: A9 00 */    LDA #$00
/* 15FDD: 85 59 */    STA $59
/* 15FDF: A5 26 */    LDA CurrentStripeEndType
/* 15FE1: 29 01 */    AND #$01
/* 15FE3: F0 03 */    BEQ HorizontalScrollPoint
/* 15FE5: 4C 72 A0 */ JMP VerticalScrollPoint

HorizontalScrollPoint:
/* 15FE8: AD 00 04 */ LDA ObjectSpriteNum+0
/* 15FEB: C9 09 */    CMP #$09        ;jumping/falling?
/* 15FED: F0 0A */    BEQ L15FF9 ; +
/* 15FEF: AD 40 04 */ LDA ObjectUnknown440+0
/* 15FF2: 29 30 */    AND #$30
/* 15FF4: 8D 40 04 */ STA ObjectUnknown440+0
; Make Megaman run
/* 15FF7: A9 06 */    LDA #$06
L15FF9: ; +
/* 15FF9: 8D 00 04 */ STA ObjectSpriteNum+0

/* 15FFC: A5 26 */    LDA CurrentStripeEndType
/* 15FFE: 38 */       SEC
/* 15FFF: E9 01 */    SBC #$01
/* 16001: 4A */       LSR A
/* 16002: AA */       TAX
/* 16003: BD 6A A0 */ LDA A06A_table,X ;Screen scrolls backward
/* 16006: 85 5A */    STA $5A          ;This is added to PosXfraction each frame..
/* 16008: BD 6C A0 */ LDA A06C_table,X
/* 1600B: 85 5B */    STA $5B          ;The increment Megaman moves in (X)
/* 1600D: AD 80 04 */ LDA ObjectPosX+0
/* 16010: C9 E0 */    CMP #$E0
/* 16012: 90 0A */    BCC L1601E ; +
/* 16014: BD 6E A0 */ LDA A06E_table,X ;Screen scrolls forward
/* 16017: 85 5A */    STA $5A
/* 16019: BD 70 A0 */ LDA A070_table,X
/* 1601C: 85 5B */    STA $5B          ;The increment Megaman moves in (X)
L1601E: ; +
/* 1601E: A9 00 */    LDA #$00
/* 16020: 85 1A */    STA ScrollPosX
/* 16022: 85 1E */    STA ScrollPosY
/* 16024: BC 66 A0 */ LDY HorizNumberOfFramesToScroll,X
L16027: ; - ;more scrolling to do
/* 16027: 18 */       CLC
/* 16028: AD A0 04 */ LDA ObjectPosXfraction+0
/* 1602B: 65 5A */    ADC $5A
/* 1602D: 8D A0 04 */ STA ObjectPosXfraction+0
/* 16030: AD 80 04 */ LDA ObjectPosX+0
/* 16033: 65 5B */    ADC $5B
/* 16035: 8D 80 04 */ STA ObjectPosX+0
/* 16038: 18 */       CLC
/* 16039: A5 1A */    LDA ScrollPosX
/* 1603B: 7D 68 A0 */ ADC HorizScrollIncrement,X
/* 1603E: 85 1A */    STA ScrollPosX
/* 16040: 98 */       TYA
/* 16041: 48 */       PHA  ;phy
/* 16042: 8A */        TXA
/* 16043: 48 */        PHA  ;phx
/* 16044: 20 31 D1 */   JSR UpdateGraphics                                  ; $D131
/* 16047: 20 C4 C6 */   JSR LoadEnemyGraphics
/* 1604A: 20 1B C0 */   JSR NextFrame                                       ; $C01B
/* 1604D: 68 */        PLA
/* 1604E: AA */        TAX  ;plx
/* 1604F: 68 */       PLA
/* 16050: A8 */       TAY  ;ply
/* 16051: 88 */       DEY
/* 16052: D0 D3 */    BNE L16027 ; -
; done scrolling
/* 16054: A9 18 */    LDA #$18
/* 16056: 8D 80 04 */ STA ObjectPosX+0 ;This fixes Megaman's position after horizontal scrolling
/* 16059: A9 00 */    LDA #$00
/* 1605B: 85 1E */    STA ScrollPosY
/* 1605D: 85 1A */    STA ScrollPosX
/* 1605F: 8D 00 04 */ STA ObjectSpriteNum+0 ;megaman standing idle
/* 16062: 8D 40 04 */ STA ObjectUnknown440+0
/* 16065: 60 */       RTS


HorizNumberOfFramesToScroll: .byte $3F, $40 ;at A066
HorizScrollIncrement:   .byte +4, NEG{4}  ;at A068
A06A_table: .byte $00, $00 ;backward scrolling, $5A value, increments ObjectPosXfraction
A06C_table: .byte $01, $FF ;backward scrolling, megaman X increment
A06E_table: .byte $B0, $50 ;forward scrolling, $5A value, increments ObjectPosXfraction
A070_table: .byte $00, $FF ;forward scrolling, megaman X increment

VerticalScrollPoint:
/* 16072: A5 26 */    LDA CurrentStripeEndType
/* 16074: 4A */       LSR A
/* 16075: AA */       TAX
/* 16076: BD CC A0 */ LDA A0CC_table,X
/* 16079: 85 33 */    STA $33
/* 1607B: BD D6 A0 */ LDA VertScrollBeginValue,X
/* 1607E: 85 1E */    STA ScrollPosY
L16080: ; -
/* 16080: 8A */       TXA
/* 16081: 48 */       PHA  ; phx
/* 16082: 20 31 D1 */  JSR UpdateGraphics                                  ; $D131
/* 16085: 20 23 CF */  JSR F1CF23
/* 16088: 20 C4 C6 */  JSR LoadEnemyGraphics
/* 1608B: 20 1B C0 */  JSR NextFrame                                       ; $C01B
/* 1608E: 68 */       PLA
/* 1608F: AA */       TAX
/* 16090: A5 B0 */    LDA TeleportEnteredFlag
/* 16092: D0 13 */    BNE L160A7 ; +                                               ; $A0A7
/* 16094: 18 */       CLC
/* 16095: AD 20 06 */ LDA ObjectPosYfraction+0
/* 16098: 7D D0 A0 */ ADC A0D0_table,X      ;-65 or +65
/* 1609B: 8D 20 06 */ STA ObjectPosYfraction+0
/* 1609E: AD 00 06 */ LDA ObjectPosY+0
/* 160A1: 7D D2 A0 */ ADC A0D2_table,X      ;+3 or -4 (!)
/* 160A4: 8D 00 06 */ STA ObjectPosY+0
L160A7: ; +
/* 160A7: 18 */       CLC
/* 160A8: A5 1E */    LDA ScrollPosY
/* 160AA: 7D D4 A0 */ ADC VertScrollIncrement,X      ;-4 or +4
/* 160AD: 85 1E */    STA ScrollPosY
/* 160AF: 18 */       CLC
/* 160B0: A5 33 */    LDA $33
/* 160B2: 7D CE A0 */ ADC A0CE_table,X      ;-1 or +1
/* 160B5: 85 33 */    STA $33
/* 160B7: 30 06 */    BMI L160BF ; +                                               ; $A0BF
/* 160B9: C9 3C */    CMP #$3c
/* 160BB: F0 02 */    BEQ L160BF ; +                                               ; $A0BF
/* 160BD: D0 C1 */    BNE L16080 ; -                                               ; $A080
L160BF: ; +
/* 160BF: A9 00 */    LDA #$00
/* 160C1: 85 1D */    STA $1D
/* 160C3: 85 1E */    STA ScrollPosY
/* 160C5: 8D 20 06 */ STA ObjectPosYfraction+0
/* 160C8: 20 31 D1 */ JSR UpdateGraphics                                  ; $D131
/* 160CB: 60 */       RTS


A0CC_table: .byte $3B, $00 ; Starting value for $33
A0CE_table: .byte $FF, $01 ; Increment for $33
A0D0_table: .byte $BF, $41 ; Increment of ObjectPosYfraction+0
A0D2_table: .byte $03, $FC ; Increment of ObjectPosY+0
VertScrollIncrement:  .byte  NEG{4}, 4
VertScrollBeginValue: .byte NEG{17}, 0



F160D8:
/* 160D8: 85 05 */    STA $05
/* 160DA: A2 FF */    LDX #$ff
/* 160DC: 86 04 */    STX $04
/* 160DE: A9 00 */    LDA #$00
/* 160E0: 85 2F */    STA RefObjectNum

; Render nothing to nametable
L160E2: ; -
/* 160E2: A9 00 */    LDA #$00
/* 160E4: 85 0D */    STA $0D
/* 160E6: 85 1C */    STA TSAPPUtransferSize

/* 160E8: A9 08 */    LDA #$08
/* 160EA: 85 0C */    STA $0C
/* 160EC: 20 B3 9F */ JSR Function9FB3

/* 160EF: A5 FF */    LDA PPU2000value       ; Branch if NMI off
/* 160F1: 29 80 */    AND #$80
/* 160F3: F0 06 */    BEQ L160FB ; +                                               ; $A0FB
/* 160F5: 20 1B C0 */ JSR NextFrame                                       ; $C01B
/* 160F8: 4C FE A0 */ JMP L160FE ; ++                                              ; $A0FE
L160FB: ; +
/* 160FB: 20 73 D6 */ JSR DoTSAPPUtransfer                         ; $D673
L160FE: ; ++
/* 160FE: A5 04 */    LDA $04
/* 16100: C9 FF */    CMP #$ff
/* 16102: D0 DE */    BNE L160E2 ; -                                               ; $A0E2
/* 16104: 60 */       RTS


ForgetRoomObjects:
/* 16105: A2 1F */    LDX #$1f
/* 16107: A9 F8 */    LDA #$f8
L16109: ; -
/* 16109: 9D 01 06 */ STA ObjectPosY+1,X
/* 1610C: CA */       DEX
/* 1610D: 10 FA */    BPL L16109 ; -
/* 1610F: A2 0F */    LDX #$0f
/* 16111: A9 FF */    LDA #$ff
L16113: ; -
/* 16113: 95 7B */    STA $7B,X
/* 16115: CA */       DEX
/* 16116: 10 FB */    BPL L16113 ; -
/* 16118: A5 31 */    LDA CurrentStage
/* 1611A: C9 01 */    CMP #$01        ;Ice
/* 1611C: F0 0D */    BEQ L1612B
/* 1611E: C9 04 */    CMP #$04        ;Elec
/* 16120: F0 0D */    BEQ L1612F
/* 16122: C9 03 */    CMP #$03        ;Fire
/* 16124: F0 31 */    BEQ L16157
/* 16126: C9 06 */    CMP #$06        ;Wily1
/* 16128: F0 1A */    BEQ L16144
/* 1612A: 60 */       RTS

;  Because of how this works, apparently there can be no Gutsblocks
;  before all TemporaryBlocks are seen.
; In stage 1: (Iceman)
L1612B:
/* 1612B: A9 66 */    LDA #17*6
/* 1612D: D0 02 */    BNE L16131 ; +                                               ; $A131
; In stage 4: (Elecman)
L1612F:
/* 1612F: A9 2A */    LDA #7*6  ;Load Actives (the buzzblock occurance points)
; +
L16131: ; -
/* 16131: 85 0E */    STA $0E
/* 16133: A9 00 */    LDA #$00
/* 16135: 85 0C */    STA $0C         ;Set the type as 0
/* 16137: A5 0E */    LDA $0E
/* 16139: 20 25 C6 */ JSR LoadActiveByIndexAndSetBlockingness       ;does stores into $0C,$0D
/* 1613C: 38 */       SEC
/* 1613D: A5 0E */    LDA $0E
/* 1613F: E9 06 */    SBC #$06
/* 16141: 10 EE */    BPL L16131 ; -                                               ; $A131
/* 16143: 60 */       RTS

; In stage 6:
L16144:
/* 16144: A9 1E */    LDA #$1E
L16146: ; -
/* 16146: 85 0C */     STA $0C
/* 16148: 20 4A C6 */  JSR F1C64A
/* 1614B: A5 0C */     LDA $0C
/* 1614D: C9 2A */     CMP #$2A
/* 1614F: F0 05 */     BEQ L16156 ; +
/* 16151: 18 */        CLC
/* 16152: 69 06 */     ADC #$06
/* 16154: D0 F0 */     BNE L16146
L16156: ; +
/* 16156: 60 */       RTS

; In stage 3: ;fireman stage
L16157:
/* 16157: A9 54 */    LDA #$54
L16159: ; -
/* 16159: 85 0C */     STA $0C
/* 1615B: 20 4A C6 */  JSR F1C64A
/* 1615E: 38 */        SEC
/* 1615F: A5 0C */     LDA $0C
/* 16161: E9 06 */     SBC #$06
/* 16163: 10 F4 */    BPL L16159 ; -
/* 16165: A5 1B */    LDA ScrollPosScreen
/* 16167: C9 0F */    CMP #$0f
/* 16169: B0 1F */    BCS L1618A ; +
/* 1616B: A9 FF */     LDA #$ff
/* 1616D: 8D 10 04 */  STA ObjectSpriteNum+$10
/* 16170: A9 32 */     LDA #$32
/* 16172: 8D F0 06 */  STA ObjectType+$10
/* 16175: A9 20 */     LDA #$20
/* 16177: 8D 30 04 */  STA ObjectFlags+$10
/* 1617A: A2 10 */     LDX #$10
/* 1617C: 20 45 B0 */  JSR ClearObjectMem                                  ; $B045
/* 1617F: 8D 50 04 */  STA ObjectUnknown440+$10
/* 16182: 8D 50 06 */  STA ObjectFireDelay+$10
/* 16185: A9 E0 */     LDA #$E0
/* 16187: 8D 10 06 */  STA ObjectPosY+$10
L1618A: ; +
/* 1618A: 60 */       RTS


RunCollisionChecks:
/* 1618B: 38 */       SEC
/* 1618C: AD 80 04 */ LDA ObjectPosX+0
/* 1618F: E5 1A */    SBC ScrollPosX
/* 16191: 38 */       SEC
/* 16192: E9 07 */    SBC #$07
/* 16194: 85 00 */    STA $00
/* 16196: 18 */       CLC
/* 16197: 69 0E */    ADC #$0e
/* 16199: 85 01 */    STA $01
/* 1619B: 38 */       SEC
/* 1619C: AD 00 06 */ LDA ObjectPosY+0
/* 1619F: E9 0B */    SBC #$0b
/* 161A1: 85 03 */    STA $03
/* 161A3: 18 */       CLC
/* 161A4: 69 16 */    ADC #$16
/* 161A6: 85 02 */    STA $02
/* 161A8: A5 55 */    LDA MegamanBlinkState
/* 161AA: D0 16 */    BNE L161C2 ; +                                               ; $A1C2
/* 161AC: A5 3E */    LDA BossCurrentStrategy
/* 161AE: C9 05 */    CMP #$05
/* 161B0: 90 10 */    BCC L161C2 ; +
/* 161B2: A2 01 */    LDX #$01
/* 161B4: 20 BE C9 */ JSR TestCollisionWithMegaman                        ; $C9BE
/* 161B7: 90 09 */    BCC L161C2 ; +
/* 161B9: A9 04 */    LDA #$04
/* 161BB: 85 0C */    STA $0C
/* 161BD: A5 43 */    LDA BossVariable43
/* 161BF: 4C 3D A2 */ JMP RegisterMegamanCollision                        ; $A23D
L161C2: ; +
/* 161C2: A5 23 */    LDA FrameCounter
/* 161C4: 29 01 */    AND #$01
/* 161C6: 18 */       CLC
/* 161C7: 69 10 */    ADC #$10
/* 161C9: AA */       TAX
L161CA: ; -
/* 161CA: 20 BE C9 */ JSR TestCollisionWithMegaman                        ; $C9BE
/* 161CD: B0 07 */    BCS L161D6 ; +                                               ; $A1D6

; next object...
L161CF:
/* 161CF: E8 */       INX
/* 161D0: E8 */       INX
/* 161D1: E4 54 */    CPX TotalObjects
/* 161D3: 90 F5 */    BCC L161CA ; -                                               ; $A1CA
/* 161D5: 60 */       RTS


L161D6: ; +

; Item can only be picked up if ObjectSpriteNum,X = #$FF
/* 161D6: BD 00 04 */ LDA ObjectSpriteNum,X
/* 161D9: C9 FF */    CMP #$ff
/* 161DB: D0 0E */    BNE L161EB ; +                                               ; $A1EB

; if ID >= #$3C and ID < #$48 then this object is a bonus item
/* 161DD: BD E0 06 */ LDA ObjectType,X
/* 161E0: C9 48 */    CMP #$48
/* 161E2: B0 07 */    BCS L161EB ; +                                               ; $A1EB
/* 161E4: C9 3C */    CMP #$3c
/* 161E6: 90 03 */    BCC L161EB ; +                                               ; $A1EB
/* 161E8: 4C 33 C8 */ JMP GotItem                                         ; $C833
L161EB: ; +
/* 161EB: A5 55 */    LDA MegamanBlinkState
; If Megaman is blinking, ignore this collision and go handle the next object.
/* 161ED: D0 E0 */    BNE L161CF

; Collision.
/* 161EF: BD 00 04 */ LDA ObjectSpriteNum,X
/* 161F2: C9 FF */    CMP #$ff
/* 161F4: F0 37 */    BEQ L1622D
/* 161F6: C9 27 */    CMP #$27
/* 161F8: F0 24 */    BEQ L1621E
/* 161FA: C9 32 */    CMP #$32
/* 161FC: F0 20 */    BEQ L1621E
/* 161FE: C9 4C */    CMP #$4c
/* 16200: 90 24 */    BCC L16226
/* 16202: C9 54 */    CMP #$54
/* 16204: B0 20 */    BCS L16226
/* 16206: A5 5F */    LDA WeaponSelect
/* 16208: C9 01 */    CMP #$01
/* 1620A: D0 1A */    BNE L16226

/* 1620C: 8A */       TXA
/* 1620D: 48 */       PHA
/* 1620E: 20 C5 C7 */ JSR F1C7C5
/* 16211: 68 */       PLA
/* 16212: AA */       TAX
/* 16213: A9 0A */    LDA #$0a
/* 16215: 85 0C */    STA $0C
/* 16217: BD 20 04 */ LDA ObjectFlags,X
/* 1621A: 49 40 */    EOR #$40
/* 1621C: D0 1F */    BNE RegisterMegamanCollision                        ; $A23D
L1621E:
/* 1621E: BD 20 04 */ LDA ObjectFlags,X
/* 16221: 09 80 */    ORA #$80
/* 16223: 9D 20 04 */ STA ObjectFlags,X

L16226:
/* 16226: A4 AC */    LDY FightingBossNum
/* 16228: B9 EF FE */ LDA L1FEEF,Y
/* 1622B: D0 0B */    BNE L16238
L1622D:
/* 1622D: A5 3E */    LDA BossCurrentStrategy
/* 1622F: D0 F5 */    BNE L16226

; Load enemy hit damage from table at $FEBD into $0C

/* 16231: BD E0 06 */ LDA ObjectType,X
/* 16234: A8 */       TAY
/* 16235: B9 BD FE */ LDA EnemyHitTable,Y
L16238:
/* 16238: 85 0C */    STA $0C

/* 1623A: BD 20 04 */ LDA ObjectFlags,X ; Get object flags

; Reached from various locations
RegisterMegamanCollision:
/* 1623D: 85 0D */    STA $0D


; if ((LifeMeter-=$0C)<=0) then Kill Megaman
/* 1623F: 38 */       SEC
/* 16240: A5 6A */    LDA Meters+0
/* 16242: E5 0C */    SBC $0C
/* 16244: 85 6A */    STA Meters+0
/* 16246: F0 02 */    BEQ L1624A ; +                                               ; $A24A
/* 16248: B0 07 */    BCS L16251 ; ++                                              ; $A251
L1624A: ; +
/* 1624A: A9 00 */    LDA #$00
/* 1624C: 85 6A */    STA Meters+0
/* 1624E: 4C 19 C2 */ JMP MegaManKilled                                   ; $C219
L16251: ; ++

; Make sure he gets hit towards in the same direction as the hitting object

/* 16251: A5 0D */    LDA $0D
/* 16253: 29 40 */    AND #$40
/* 16255: 49 40 */    EOR #$40

; Make Megaman get hit
/* 16257: 09 03 */    ORA #$03
/* 16259: 8D 20 04 */ STA ObjectFlags+0

/* 1625C: A9 6F */    LDA #$6f
/* 1625E: 85 55 */    STA MegamanBlinkState
/* 16260: 60 */       RTS

RunWeaponAI:
/* 16261: A5 23 */    LDA FrameCounter
/* 16263: 29 01 */    AND #$01
/* 16265: 18 */       CLC
/* 16266: 69 10 */    ADC #$10
/* 16268: AA */       TAX
; At even frames, it tests objects 10,12,14,16,18,1A,1C,1E
; At odd  frames, it tests objects 11,13,15,17,19,1B,1D,1F

; Make object turn left
L16269: ; -
/* 16269: BD 20 04 */ LDA ObjectFlags,X
/* 1626C: 29 7F */    AND #$7f
/* 1626E: 9D 20 04 */ STA ObjectFlags,X

/* 16271: E8 */       INX
/* 16272: E8 */       INX
/* 16273: E0 20 */    CPX #$20
/* 16275: 90 F2 */    BCC L16269 ; -
/* 16277: A5 5F */    LDA WeaponSelect
/* 16279: 0A */       ASL A
/* 1627A: A8 */       TAY
/* 1627B: B9 88 A2 */ LDA WeaponAItable+0,Y
/* 1627E: 85 04 */    STA $04
/* 16280: B9 89 A2 */ LDA WeaponAItable+1,Y
/* 16283: 85 05 */    STA $05
/* 16285: 6C 04 00 */ JMP ($0004)

; See also: MegamanWeaponFire_*
WeaponAItable: ; at A288
    .word WeaponAI_P, WeaponAI_C, WeaponAI_I, WeaponAI_B
    .word WeaponAI_F, WeaponAI_E, WeaponAI_G, WeaponAI_M

WeaponAI_P:
/* 16298: A2 02 */    LDX #$02
/* 1629A: A9 05 */    LDA #$05
WeaponAI_TestShotHit:  ;Input: A(5=P,6=C,7=F,8=E,$0E=B) X(2=P/B, 5=C/F/I, 5-8=G, 5-$0B=E)
/* 1629C: 85 59 */    STA $59               ; A = maximum shot obj index
L1629E:
/* 1629E: 86 2F */    STX RefObjectNum   ; X = minimum shot obj index
/* 162A0: 20 B2 C8 */ JSR TestShotHit
/* 162A3: A6 2F */    LDX RefObjectNum
/* 162A5: E8 */       INX
/* 162A6: E4 59 */    CPX $59
/* 162A8: D0 F4 */    BNE L1629E
/* 162AA: 60 */       RTS

WeaponAI_C:
/* 162AB: A2 05 */    LDX #$05
/* 162AD: A9 06 */    LDA #$06
/* 162AF: D0 EB */    BNE WeaponAI_TestShotHit
WeaponAI_I:
/* 162B1: A2 05 */    LDX #$05
/* 162B3: 86 2F */    STX RefObjectNum
/* 162B5: 20 B2 C8 */ JSR TestShotHit
/* 162B8: B0 01 */    BCS L162BB ; +                                               ; $A2BB
/* 162BA: 60 */       RTS
L162BB: ; +
/* 162BB: A5 3E */    LDA BossCurrentStrategy
/* 162BD: D0 07 */    BNE L162C6 ; +
/* 162BF: A6 0C */    LDX $0C
/* 162C1: A9 FF */    LDA #$ff
/* 162C3: 9D A0 06 */ STA ObjectLifeCycleCounter,X
L162C6: ; +
/* 162C6: 60 */       RTS

WeaponAI_B:
/* 162C7: A5 61 */    LDA NumberOfFramesSinceShooting
/* 162C9: C9 92 */    CMP #$92
/* 162CB: 90 06 */    BCC L162D3 ; +
/* 162CD: A2 02 */    LDX #$02
/* 162CF: A9 0E */    LDA #$0e
/* 162D1: D0 C9 */    BNE WeaponAI_TestShotHit
L162D3: ; +
/* 162D3: 60 */       RTS

WeaponAI_F:
/* 162D4: A2 05 */    LDX #$05
/* 162D6: A9 07 */    LDA #$07
/* 162D8: D0 C2 */    BNE WeaponAI_TestShotHit
WeaponAI_E:
/* 162DA: A2 05 */    LDX #$05
/* 162DC: A5 61 */    LDA NumberOfFramesSinceShooting
/* 162DE: C9 FF */    CMP #$ff
/* 162E0: F0 32 */    BEQ L16314 ; +++                                             ; $A314
L162E2: ; -
/* 162E2: 86 2F */    STX RefObjectNum
/* 162E4: 20 B2 C8 */ JSR TestShotHit
/* 162E7: 90 23 */    BCC L1630C ; ++                                              ; $A30C
/* 162E9: A6 0C */    LDX $0C
/* 162EB: BD 00 04 */ LDA ObjectSpriteNum,X
/* 162EE: C9 3F */    CMP #$3f
/* 162F0: 90 1A */    BCC L1630C ; ++                                              ; $A30C
/* 162F2: C9 48 */    CMP #$48
/* 162F4: 90 0F */    BCC L16305 ; +                                               ; $A305
/* 162F6: C9 FF */    CMP #$ff
/* 162F8: D0 12 */    BNE L1630C ; ++                                              ; $A30C

/* 162FA: BD E0 06 */ LDA ObjectType,X
/* 162FD: C9 11 */    CMP #$11
/* 162FF: F0 04 */    BEQ L16305 ; +                                               ; $A305

; $1C = scissor machine
/* 16301: C9 1C */    CMP #$1c
/* 16303: D0 07 */    BNE L1630C ; ++                                              ; $A30C

L16305: ; +
/* 16305: 20 E4 C7 */ JSR C7E4_routine
/* 16308: A9 FE */    LDA #$fe
/* 1630A: 85 61 */    STA NumberOfFramesSinceShooting

L1630C: ; ++
/* 1630C: A6 2F */    LDX RefObjectNum
/* 1630E: E8 */       INX
/* 1630F: E0 0C */    CPX #$0c
/* 16311: D0 CF */    BNE L162E2 ; -                                               ; $A2E2
/* 16313: 60 */       RTS

L16314: ; +++
/* 16314: A9 08 */    LDA #$08
/* 16316: D0 84 */    BNE WeaponAI_TestShotHit

WeaponAI_G:
/* 16318: A2 05 */    LDX #$05
L1631A: ; -
/* 1631A: 86 2F */    STX RefObjectNum
/* 1631C: 20 B2 C8 */ JSR TestShotHit
/* 1631F: B0 08 */    BCS L16329 ; +
/* 16321: A6 2F */    LDX RefObjectNum
L16323:
/* 16323: E8 */       INX
/* 16324: E0 08 */    CPX #$08
/* 16326: D0 F2 */    BNE L1631A ; -
/* 16328: 60 */       RTS
L16329: ; +
/* 16329: A6 2F */    LDX RefObjectNum
/* 1632B: A4 31 */    LDY CurrentStage
/* 1632D: BD 00 04 */ LDA ObjectSpriteNum,X
/* 16330: D9 88 A8 */ CMP GutsblockObjectSpritenum,Y
/* 16333: D0 EE */    BNE L16323
F16335:
/* 16335: 38 */       SEC
/* 16336: BD 00 06 */ LDA ObjectPosY,X
/* 16339: E9 10 */    SBC #$10
/* 1633B: 85 03 */    STA $03
/* 1633D: 38 */       SEC
/* 1633E: BD 80 04 */ LDA ObjectPosX,X
/* 16341: E9 10 */    SBC #$10
/* 16343: 85 00 */    STA $00
/* 16345: BD 60 04 */ LDA ObjectPosScreen,X
/* 16348: E9 00 */    SBC #$00
/* 1634A: 85 01 */    STA $01
/* 1634C: 20 B5 F7 */ JSR F1F7B5
/* 1634F: A5 61 */    LDA NumberOfFramesSinceShooting
/* 16351: C9 10 */    CMP #$10
/* 16353: B0 04 */    BCS L16359
/* 16355: A9 1F */    LDA #$1f
/* 16357: 85 61 */    STA NumberOfFramesSinceShooting
L16359:
/* 16359: 60 */       RTS

WeaponAI_M:
/* 1635A: 18 */       CLC
/* 1635B: 60 */       RTS

CheckHoldingBkey:
/* 1635C: A5 60 */    LDA WeaponFiring
/* 1635E: 30 0A */    BMI L1636A ; ++                                               ; $A36A
/* 16360: A5 5F */    LDA WeaponSelect
/* 16362: C9 06 */    CMP #$06
/* 16364: D0 03 */    BNE L16369 ; +
/* 16366: 20 FD F6 */ JSR RoutineF6FD_GutsmanWeapon
L16369: ; +
/* 16369: 60 */       RTS
L1636A: ; ++
; Check if B key is being held
/* 1636A: A5 60 */    LDA WeaponFiring
/* 1636C: 09 40 */    ORA #$40
/* 1636E: 85 60 */    STA WeaponFiring
/* 16370: A5 5F */    LDA WeaponSelect
/* 16372: 29 3F */    AND #$3f
/* 16374: 0A */       ASL A
/* 16375: AA */       TAX
/* 16376: BD 83 A3 */ LDA BKeyHoldingAI+0,X
/* 16379: 85 04 */    STA $04
/* 1637B: BD 84 A3 */ LDA BKeyHoldingAI+1,X
/* 1637E: 85 05 */    STA $05
/* 16380: 6C 04 00 */ JMP ($0004)

BKeyHoldingAI:; at A383
    .word BKeyHoldingAI_PandI ;P
    .word BKeyHoldingAI_C
    .word BKeyHoldingAI_PandI ;I
    .word BKeyHoldingAI_B
    .word BKeyHoldingAI_F
    .word BKeyHoldingAI_E
    .word BKeyHoldingAI_G
    .word BKeyHoldingAI_M

BKeyHoldingAI_C:
/* 16393: A5 61 */    LDA NumberOfFramesSinceShooting
/* 16395: C9 0F */    CMP #$0F
/* 16397: B0 16 */    BCS L163AF ; ++
/* 16399: AD 00 04 */ LDA ObjectSpriteNum+0
/* 1639C: C9 09 */    CMP #$09          ;jumping/falling?
/* 1639E: F0 08 */    BEQ L163A8 ; +
/* 163A0: C9 15 */    CMP #$15          ;on ladder?
/* 163A2: F0 04 */    BEQ L163A8 ; +
/* 163A4: C9 17 */    CMP #$17          ;getting off a ladder?
/* 163A6: D0 07 */    BNE L163AF ; ++
L163A8: ; +
/* 163A8: A9 6F */    LDA #$6F          ;throwing something while jumping/falling
/* 163AA: 8D 00 04 */ STA ObjectSpriteNum+0
/* 163AD: D0 0E */    BNE L163BD ; +
L163AF: ; ++
/* 163AF: A9 00 */    LDA #$00
/* 163B1: 8D 40 06 */ STA ObjectFireDelay+0
/* 163B4: A5 61 */    LDA NumberOfFramesSinceShooting
/* 163B6: C9 2A */    CMP #$2A
/* 163B8: B0 17 */    BCS L163D1 ; ++
/* 163BA: 20 09 A7 */ JSR BKeyHoldingAI_PandI
L163BD: ; +
/* 163BD: E6 61 */    INC NumberOfFramesSinceShooting
/* 163BF: 38 */       SEC
/* 163C0: AD 65 06 */ LDA ObjectYSpeedFraction+5
/* 163C3: E9 1A */    SBC #$1a
/* 163C5: 8D 65 06 */ STA ObjectYSpeedFraction+5
/* 163C8: AD 85 06 */ LDA ObjectYSpeed+5
/* 163CB: E9 00 */    SBC #$00
/* 163CD: 8D 85 06 */ STA ObjectYSpeed+5
/* 163D0: 60 */       RTS
L163D1: ; ++
/* 163D1: D0 0A */    BNE L163DD ; +
/* 163D3: A9 80 */    LDA #$80
/* 163D5: 85 65 */    STA $65
/* 163D7: A9 02 */    LDA #$02
/* 163D9: 85 66 */    STA $66
/* 163DB: E6 61 */    INC NumberOfFramesSinceShooting
L163DD: ; +
/* 163DD: AD 25 04 */ LDA ObjectFlags+5
/* 163E0: 29 BF */    AND #$bf
/* 163E2: 8D 25 04 */ STA ObjectFlags+5
/* 163E5: 38 */       SEC
/* 163E6: AD 80 04 */ LDA ObjectPosX+0
/* 163E9: ED 85 04 */ SBC ObjectPosX+5
/* 163EC: 85 02 */    STA $02
/* 163EE: AD 60 04 */ LDA ObjectPosScreen+0                             ; $0460
/* 163F1: ED 65 04 */ SBC ObjectPosScreen+5
/* 163F4: 90 0A */    BCC L16400 ; +
/* 163F6: AD 25 04 */ LDA ObjectFlags+5
/* 163F9: 09 40 */    ORA #$40
/* 163FB: 8D 25 04 */ STA ObjectFlags+5
/* 163FE: D0 08 */    BNE L16408 ; ++
L16400: ; +
/* 16400: A5 02 */    LDA $02
/* 16402: 49 FF */    EOR #$ff
/* 16404: 69 01 */    ADC #$01
/* 16406: 85 02 */    STA $02
L16408: ; ++
/* 16408: 18 */       CLC
/* 16409: A5 65 */    LDA $65
/* 1640B: 69 08 */    ADC #$08
/* 1640D: 85 65 */    STA $65
/* 1640F: 85 00 */    STA $00
/* 16411: A5 66 */    LDA $66
/* 16413: 69 00 */    ADC #$00
/* 16415: 85 66 */    STA $66
/* 16417: 85 01 */    STA $01
/* 16419: 38 */       SEC
/* 1641A: AD 05 06 */ LDA ObjectPosY+5
/* 1641D: ED 00 06 */ SBC ObjectPosY+0
/* 16420: 85 03 */    STA $03
/* 16422: A2 05 */    LDX #$05
/* 16424: 20 C6 F8 */ JSR F8C6_routine
/* 16427: A5 03 */    LDA $03
/* 16429: C9 0C */    CMP #$0c
/* 1642B: B0 06 */    BCS L16433 ; +
/* 1642D: A5 02 */    LDA $02
/* 1642F: C9 0C */    CMP #$0c
/* 16431: 90 28 */    BCC L1645B ; ++
L16433: ; +
/* 16433: A5 63 */    LDA $63
/* 16435: 05 64 */    ORA $64
/* 16437: F0 1E */    BEQ L16457 ; +
/* 16439: 38 */       SEC
/* 1643A: A5 63 */    LDA $63
/* 1643C: E9 20 */    SBC #$20
/* 1643E: 85 63 */    STA $63
/* 16440: A5 64 */    LDA $64
/* 16442: E9 00 */    SBC #$00
/* 16444: 85 64 */    STA $64
/* 16446: 38 */       SEC
/* 16447: AD 65 06 */ LDA ObjectYSpeedFraction+5
/* 1644A: E5 63 */    SBC $63
/* 1644C: 8D 65 06 */ STA ObjectYSpeedFraction+5
/* 1644F: AD 85 06 */ LDA ObjectYSpeed+5
/* 16452: E5 64 */    SBC $64
/* 16454: 8D 85 06 */ STA ObjectYSpeed+5
L16457: ; +
/* 16457: 20 09 A7 */ JSR BKeyHoldingAI_PandI
/* 1645A: 60 */       RTS
L1645B: ; ++
/* 1645B: A9 F8 */    LDA #$f8
/* 1645D: 8D 05 06 */ STA ObjectPosY+5
/* 16460: A9 00 */    LDA #$00
/* 16462: 85 60 */    STA WeaponFiring
/* 16464: 60 */       RTS

BKeyHoldingAI_B:
/* 16465: E6 61 */    INC NumberOfFramesSinceShooting
/* 16467: A5 61 */    LDA NumberOfFramesSinceShooting
/* 16469: C9 0F */    CMP #$0f
/* 1646B: B0 15 */    BCS L16482 ; ++
/* 1646D: AD 00 04 */ LDA ObjectSpriteNum+0
/* 16470: C9 09 */    CMP #$09         ;jumping?
/* 16472: F0 08 */    BEQ L1647C ; +
/* 16474: C9 15 */    CMP #$15         ;climbing?
/* 16476: F0 04 */    BEQ L1647C ; +
/* 16478: C9 17 */    CMP #$17         ;unclimbing?
/* 1647A: D0 06 */    BNE L16482 ; ++
L1647C: ; +
/* 1647C: A9 6F */    LDA #$6f         ;throwing something while jumping
/* 1647E: 8D 00 04 */ STA ObjectSpriteNum+0
/* 16481: 60 */       RTS
L16482: ; ++
/* 16482: A9 00 */    LDA #$00
/* 16484: 8D 40 06 */ STA ObjectFireDelay+0
/* 16487: A5 61 */    LDA NumberOfFramesSinceShooting
/* 16489: C9 92 */    CMP #$92
/* 1648B: B0 04 */    BCS L16491 ; +
/* 1648D: 20 09 A7 */ JSR BKeyHoldingAI_PandI
/* 16490: 60 */       RTS
L16491: ; +
/* 16491: D0 1F */    BNE L164B2 ; +
/* 16493: A9 0B */    LDA #$0b
/* 16495: 85 0C */    STA $0C
/* 16497: A9 5F */    LDA #$5f
/* 16499: 85 0D */    STA $0D
/* 1649B: A9 00 */    LDA #$00
/* 1649D: 85 0E */    STA $0E
/* 1649F: A2 01 */    LDX #$01
L164A1: ; -
/* 164A1: A0 05 */    LDY #$05
/* 164A3: E8 */       INX
/* 164A4: 20 41 F8 */ JSR CreateExplosionObject
/* 164A7: 10 F8 */    BPL L164A1 ; -
/* 164A9: A9 12 */    LDA #$12        ; Explosion
/* 164AB: 20 77 C4 */ JSR IssueSound                                      ; $C477
L164AE: ; -
/* 164AE: 20 09 A7 */ JSR BKeyHoldingAI_PandI
/* 164B1: 60 */       RTS
L164B2: ; + ;It has exploded now
/* 164B2: C9 A0 */    CMP #$a0
/* 164B4: D0 F8 */    BNE L164AE ; -
/* 164B6: A9 F8 */    LDA #$f8
/* 164B8: A2 02 */    LDX #$02
L164BA: ; -
/* 164BA: 9D 00 06 */ STA ObjectPosY,X
/* 164BD: E8 */       INX
/* 164BE: E0 10 */    CPX #$10
/* 164C0: D0 F8 */    BNE L164BA ; -                                               ; $A4BA
/* 164C2: A9 00 */    LDA #$00
/* 164C4: 85 60 */    STA WeaponFiring
/* 164C6: 60 */       RTS

BKeyHoldingAI_F:
/* 164C7: E6 61 */    INC NumberOfFramesSinceShooting
/* 164C9: A5 61 */    LDA NumberOfFramesSinceShooting
/* 164CB: C9 20 */    CMP #$20
/* 164CD: D0 0A */    BNE L164D9 ; +
/* 164CF: A9 F8 */    LDA #$F8
/* 164D1: 8D 06 06 */ STA ObjectPosY+6
/* 164D4: A9 00 */    LDA #$00
/* 164D6: 85 60 */    STA WeaponFiring
/* 164D8: 60 */       RTS
L164D9: ; +
/* 164D9: 29 07 */    AND #$07
/* 164DB: AA */       TAX
/* 164DC: 18 */       CLC
/* 164DD: AD 00 06 */ LDA ObjectPosY+0
/* 164E0: 7D FA A4 */ ADC FireShieldYPosTable,X
/* 164E3: 8D 06 06 */ STA ObjectPosY+6
/* 164E6: 18 */       CLC
/* 164E7: AD 80 04 */ LDA ObjectPosX+0
/* 164EA: 7D 02 A5 */ ADC FireShieldXPosTable,X
/* 164ED: 8D 86 04 */ STA ObjectPosX+6
/* 164F0: AD 60 04 */ LDA ObjectPosScreen+0
/* 164F3: 7D 0A A5 */ ADC FireShieldPosScreenTable,X
/* 164F6: 8D 66 04 */ STA ObjectPosScreen+6
/* 164F9: 60 */       RTS

; These tables describe how the fire ball revolves around Megaman.
FireShieldYPosTable: .byte 20,14,0,NEG{14},NEG{20},NEG{14},0,14 ; at A4FA
FireShieldXPosTable: .byte 0,NEG{14},NEG{20},NEG{14},0,14,20,14 ; at A502
FireShieldPosScreenTable: .byte 0,NEG{1},NEG{1},NEG{1},0,0,0,0 ; at A50A

; Something related to Elecman shots.
BKeyHoldingAI_E:
/* 16512: A5 61 */    LDA NumberOfFramesSinceShooting
/* 16514: C9 FF */    CMP #$FF
/* 16516: F0 04 */    BEQ L1651C ; +
/* 16518: C9 FE */    CMP #$FE
/* 1651A: D0 03 */    BNE L1651F ; ++
L1651C: ; +
/* 1651C: 4C A6 A5 */ JMP L165A6 ; ++++
L1651F: ; ++
/* 1651F: A2 05 */    LDX #$05
L16521: ; --
/* 16521: BD 40 04 */ LDA ObjectUnknown440,X
/* 16524: F0 07 */    BEQ L1652D ; +
L16526: ; -
/* 16526: E8 */       INX
/* 16527: E0 0A */    CPX #$0a
/* 16529: D0 F6 */    BNE L16521 ; --
/* 1652B: F0 4C */    BEQ L16579 ; +++
L1652D: ; +
/* 1652D: BD 00 06 */ LDA ObjectPosY,X
/* 16530: C9 F8 */    CMP #$f8
/* 16532: F0 F2 */    BEQ L16526 ; -
/* 16534: BD 00 04 */ LDA ObjectSpriteNum,X
/* 16537: 38 */       SEC
/* 16538: E9 62 */    SBC #$62
/* 1653A: A8 */       TAY
/* 1653B: B9 B9 A5 */ LDA ElecBeamSpriteNumTable,Y
/* 1653E: 9D 00 04 */ STA ObjectSpriteNum,X
/* 16541: 18 */       CLC

/* 16542: BD 00 06 */ LDA ObjectPosY,X
/* 16545: 79 B1 A5 */ ADC ElecBeamYMovementTable,Y
/* 16548: 9D 00 06 */ STA ObjectPosY,X

/* 1654B: BD 20 04 */ LDA ObjectFlags,X
/* 1654E: 29 40 */    AND #$40
/* 16550: F0 12 */    BEQ L16564 ; +
/* 16552: 18 */       CLC

/* 16553: BD 80 04 */ LDA ObjectPosX,X
/* 16556: 79 A9 A5 */ ADC ElecBeamXMovementTable,Y
/* 16559: 9D 80 04 */ STA ObjectPosX,X

/* 1655C: BD 60 04 */ LDA ObjectPosScreen,X
/* 1655F: 69 00 */    ADC #$00
/* 16561: 4C 73 A5 */ JMP L16573 ; ++
L16564: ; +
/* 16564: 38 */       SEC
/* 16565: BD 80 04 */ LDA ObjectPosX,X
/* 16568: F9 A9 A5 */ SBC ElecBeamXMovementTable,Y
/* 1656B: 9D 80 04 */ STA ObjectPosX,X

/* 1656E: BD 60 04 */ LDA ObjectPosScreen,X
/* 16571: E9 00 */    SBC #$00
L16573: ; ++
/* 16573: 9D 60 04 */ STA ObjectPosScreen,X
/* 16576: 4C 26 A5 */ JMP L16526 ; -
L16579: ; +++
/* 16579: A5 23 */    LDA FrameCounter
/* 1657B: 29 01 */    AND #$01
/* 1657D: 18 */       CLC
/* 1657E: 69 05 */    ADC #$05
/* 16580: AA */       TAX
L16581: ; -
/* 16581: BD 00 06 */ LDA ObjectPosY,X
/* 16584: C9 F8 */    CMP #$f8
/* 16586: F0 18 */    BEQ L165A0 ; +
/* 16588: 85 0E */    STA $0E
/* 1658A: BD 60 04 */ LDA ObjectPosScreen,X
/* 1658D: 85 0C */    STA $0C
/* 1658F: BD 80 04 */ LDA ObjectPosX,X
/* 16592: 85 0D */    STA $0D
/* 16594: A4 8E */    LDY ActivesLowerIndex
/* 16596: 20 8A CD */ JSR CheckCollisionAgainstActives
/* 16599: C9 82 */    CMP #$82
/* 1659B: D0 03 */    BNE L165A0 ; +
/* 1659D: 4C 8F F7 */ JMP GutsblockHitByElecBeam
L165A0: ; +
/* 165A0: E8 */       INX
/* 165A1: E8 */       INX
/* 165A2: E0 0C */    CPX #$0C
/* 165A4: 90 DB */    BCC L16581 ; -
L165A6: ; ++++
/* 165A6: 4C 09 A7 */ JMP BKeyHoldingAI_PandI

; These tables define how Elecmanshots travel.
ElecBeamXMovementTable: .byte 28,32,32,28,24,28,28,28 ;At A5A9
ElecBeamYMovementTable: .byte NEG{4},NEG{24},0,28,16,NEG{12},NEG{4},NEG{4} ;At A5B1
ElecBeamSpriteNumTable: .byte $67,$68,$69,$63,$64,$65,$66,$67 ;At A5B9

BKeyHoldingAI_G:
/* 165C1: A5 61 */    LDA NumberOfFramesSinceShooting
/* 165C3: D0 03 */    BNE L165C8 ; +
/* 165C5: E6 61 */    INC NumberOfFramesSinceShooting
/* 165C7: 60 */       RTS
L165C8: ; +
/* 165C8: C9 10 */    CMP #$10
/* 165CA: B0 3C */    BCS L16608 ; ++++
/* 165CC: AD 00 04 */ LDA ObjectSpriteNum+0
/* 165CF: C9 12 */    CMP #$12         ;getting hit by damage?
/* 165D1: F0 04 */    BEQ L165D7 ; +
/* 165D3: C9 13 */    CMP #$13         ;getting stun by earthquake?
/* 165D5: D0 04 */    BNE L165DB ; ++
L165D7: ; +
/* 165D7: A9 00 */    LDA #$00
/* 165D9: F0 02 */    BEQ L165DD ; +++
L165DB: ; ++
/* 165DB: A9 2F */    LDA #$2f   ; Give Megaman a delay when he throws it
L165DD: ; +++
/* 165DD: 8D 40 06 */ STA ObjectFireDelay+0

; Make Megaman throw something...
/* 165E0: AD 20 04 */ LDA ObjectFlags+0
/* 165E3: 29 40 */    AND #$40
/* 165E5: 09 24 */    ORA #$24

/* 165E7: 8D 25 04 */ STA ObjectFlags+5
/* 165EA: AD 60 04 */ LDA ObjectPosScreen+0                             ; $0460
/* 165ED: 8D 65 04 */ STA ObjectPosScreen+5
/* 165F0: AD 80 04 */ LDA ObjectPosX+0
/* 165F3: 8D 85 04 */ STA ObjectPosX+5
/* 165F6: AD 00 06 */ LDA ObjectPosY+0

/* 165F9: 38 */       SEC
/* 165FA: E9 1A */    SBC #$1a
/* 165FC: 8D 05 06 */ STA ObjectPosY+5
/* 165FF: A9 00 */    LDA #$00
/* 16601: 8D 65 06 */ STA ObjectYSpeedFraction+5
/* 16604: 8D 85 06 */ STA ObjectYSpeed+5
/* 16607: 60 */       RTS
L16608: ; ++++
/* 16608: AD 00 04 */ LDA ObjectSpriteNum+0
/* 1660B: C9 09 */    CMP #$09         ;jumping?
/* 1660D: D0 0D */    BNE L1661C ; +
/* 1660F: A5 61 */    LDA NumberOfFramesSinceShooting
/* 16611: C9 1F */    CMP #$1f
/* 16613: F0 07 */    BEQ L1661C ; +
/* 16615: E6 61 */    INC NumberOfFramesSinceShooting
/* 16617: A9 6F */    LDA #$6f         ;throwing something while jumping
/* 16619: 8D 00 04 */ STA ObjectSpriteNum+0
L1661C: ; +
/* 1661C: 20 09 A7 */ JSR BKeyHoldingAI_PandI
/* 1661F: 60 */       RTS


BKeyHoldingAI_M:
/* 16620: A9 05 */    LDA #$05
/* 16622: 85 2F */    STA RefObjectNum
/* 16624: AE A0 05 */ LDX MagnetBeamLength+0
/* 16627: F0 63 */    BEQ L1668C ; ++++
/* 16629: A5 14 */    LDA JoyPad0
/* 1662B: 29 02 */    AND #$02
/* 1662D: F0 58 */    BEQ L16687 ; +++
/* 1662F: A9 1F */    LDA #$1f
/* 16631: 8D 40 06 */ STA ObjectFireDelay+0   ;delay for Megaman (why?)
/* 16634: BD A0 05 */ LDA MagnetBeamLength,X
/* 16637: C9 41 */    CMP #$41
/* 16639: F0 03 */    BEQ L1663E ; +
/* 1663B: FE A0 05 */ INC MagnetBeamLength,X
L1663E: ; +
/* 1663E: 0A */       ASL A
/* 1663F: 69 10 */    ADC #$10
/* 16641: 85 0C */    STA $0C
/* 16643: 29 07 */    AND #$07
/* 16645: D0 07 */    BNE L1664E ; +
/* 16647: 18 */       CLC
/* 16648: A5 0C */    LDA $0C
/* 1664A: 69 08 */    ADC #$08
/* 1664C: 85 0C */    STA $0C
L1664E: ; +
/* 1664E: AD 20 04 */ LDA ObjectFlags+0 ; Make this object face the same direction as Megaman
/* 16651: 29 40 */    AND #$40
/* 16653: 9D 20 04 */ STA ObjectFlags,X
/* 16656: F0 11 */    BEQ L16669 ; +
; RIGHT
/* 16658: 18 */       CLC
/* 16659: AD 80 04 */ LDA ObjectPosX+0
/* 1665C: 65 0C */    ADC $0C
/* 1665E: 9D C0 05 */ STA MagnetBeamPosX,X
/* 16661: AD 60 04 */ LDA ObjectPosScreen+0                             ; $0460
/* 16664: 69 00 */    ADC #$00
/* 16666: 4C 77 A6 */ JMP L16677 ; ++
L16669: ; + ;LEFT
/* 16669: 38 */       SEC
/* 1666A: AD 80 04 */ LDA ObjectPosX+0
/* 1666D: E5 0C */    SBC $0C
/* 1666F: 9D C0 05 */ STA MagnetBeamPosX,X
/* 16672: AD 60 04 */ LDA ObjectPosScreen+0                             ; $0460
/* 16675: E9 00 */    SBC #$00
L16677: ; ++
/* 16677: 9D D0 05 */ STA MagnetBeamPosScreen,X
/* 1667A: AD 00 06 */ LDA ObjectPosY+0
/* 1667D: 9D F0 05 */ STA MagnetBeamPosY,X
/* 16680: 20 BC F6 */ JSR CheckMagnetBeamCollisionWithBG
/* 16683: C0 01 */    CPY #$01
/* 16685: D0 05 */    BNE L1668C ; ++++
L16687: ; +++
/* 16687: A9 00 */    LDA #$00
/* 16689: 8D A0 05 */ STA MagnetBeamLength
L1668C: ; ++++
/* 1668C: A2 05 */    LDX #$05
L1668E: ; --
/* 1668E: EC A0 05 */ CPX MagnetBeamLength
/* 16691: F0 12 */    BEQ L166A5 ; +
/* 16693: BD B0 05 */ LDA MagnetBeamAge,X
/* 16696: F0 58 */    BEQ L166F0
/* 16698: DE B0 05 */ DEC MagnetBeamAge,X
/* 1669B: D0 08 */    BNE L166A5 ; +
/* 1669D: A9 F8 */    LDA #$f8
/* 1669F: 9D 00 06 */ STA ObjectPosY,X
/* 166A2: 4C F0 A6 */ JMP L166F0 ; +++
L166A5: ; +
/* 166A5: BD A0 05 */ LDA MagnetBeamLength,X
/* 166A8: 29 7C */    AND #$7C
/* 166AA: 0A */       ASL A
/* 166AB: 85 0D */    STA $0D
/* 166AD: 69 08 */    ADC #$08
/* 166AF: 85 0C */    STA $0C
/* 166B1: 38 */       SEC
/* 166B2: BD C0 05 */ LDA MagnetBeamPosX,X
/* 166B5: E5 0C */    SBC $0C
/* 166B7: 9D 80 04 */ STA ObjectPosX,X
/* 166BA: BD D0 05 */ LDA MagnetBeamPosScreen,X
/* 166BD: E9 00 */    SBC #$00
/* 166BF: 9D 60 04 */ STA ObjectPosScreen,X
/* 166C2: BD E0 05 */ LDA MagnetBeamCyclePos,X
/* 166C5: 0A */       ASL A
/* 166C6: 0A */       ASL A
/* 166C7: 0A */       ASL A
/* 166C8: 48 */       PHA
/* 166C9: C5 0D */     CMP $0D
/* 166CB: F0 05 */     BEQ L166D2 ; +
/* 166CD: FE E0 05 */  INC MagnetBeamCyclePos,X
/* 166D0: D0 05 */     BNE L166D7 ; ++
L166D2: ; +
/* 166D2: A9 00 */     LDA #$00
/* 166D4: 9D E0 05 */  STA MagnetBeamCyclePos,X
L166D7: ; ++
/* 166D7: 68 */       PLA
/* 166D8: 0A */       ASL A
/* 166D9: 09 08 */    ORA #$08
/* 166DB: 18 */       CLC
/* 166DC: 7D 80 04 */ ADC ObjectPosX,X
/* 166DF: 9D 80 04 */ STA ObjectPosX,X        ;adjust beam piece pos
/* 166E2: BD 60 04 */ LDA ObjectPosScreen,X
/* 166E5: 69 00 */    ADC #$00
/* 166E7: 9D 60 04 */ STA ObjectPosScreen,X
/* 166EA: BD F0 05 */ LDA MagnetBeamPosY,X
/* 166ED: 9D 00 06 */ STA ObjectPosY,X
L166F0: ; +++
/* 166F0: E8 */       INX
/* 166F1: E0 10 */    CPX #$10
/* 166F3: D0 11 */    BNE L16706 ; +
/* 166F5: A2 05 */    LDX #$05
L166F7: ; -
/* 166F7: BD B0 05 */ LDA MagnetBeamAge,X
/* 166FA: D0 09 */    BNE L16705
/* 166FC: E8 */       INX
/* 166FD: E0 10 */    CPX #$10
/* 166FF: D0 F6 */    BNE L166F7 ; -
/* 16701: A9 00 */    LDA #$00
/* 16703: 85 60 */    STA WeaponFiring
L16705:
/* 16705: 60 */       RTS
L16706: ; +
/* 16706: 4C 8E A6 */ JMP L1668E ; --


BKeyHoldingAI_PandI:
/* 16709: A2 02 */    LDX #$02
L1670B: ; -
/* 1670B: BD 00 06 */ LDA ObjectPosY,X
/* 1670E: C9 F8 */    CMP #$f8
/* 16710: D0 0B */    BNE L1671D ; +
/* 16712: E8 */       INX
/* 16713: E0 10 */    CPX #$10
/* 16715: D0 F4 */    BNE L1670B ; -
/* 16717: A9 00 */    LDA #$00
/* 16719: 85 60 */    STA WeaponFiring
/* 1671B: 85 61 */    STA NumberOfFramesSinceShooting
L1671D: ; +
/* 1671D: 60 */       RTS



MegamanWeaponFire:
/* 1671E: A5 5F */    LDA WeaponSelect
/* 16720: AA */       TAX
/* 16721: 0A */       ASL A
/* 16722: A8 */       TAY
/* 16723: F0 05 */    BEQ L1672A ; +     ; P doesn't have a weapon meter
/* 16725: B5 6A */    LDA Meters,X
/* 16727: D0 01 */    BNE L1672A ; +     ; If there's still juice in that weapon
/* 16729: 60 */       RTS
L1672A: ; +
/* 1672A: B9 37 A7 */ LDA WeaponSelectTable+0,Y
/* 1672D: 85 04 */    STA $04
/* 1672F: B9 38 A7 */ LDA WeaponSelectTable+1,Y
/* 16732: 85 05 */    STA $05
/* 16734: 6C 04 00 */ JMP ($0004)

; See also: WeaponAI_*
WeaponSelectTable: ; at $A737
    .word MegamanWeaponFire_P
    .word MegamanWeaponFire_C
    .word MegamanWeaponFire_I
    .word MegamanWeaponFire_B
    .word MegamanWeaponFire_F
    .word MegamanWeaponFire_E
    .word MegamanWeaponFire_G
    .word MegamanWeaponFire_M

MegamanWeaponFire_P:
/* 16747: A2 02 */    LDX #$02
L16749: ; -
/* 16749: BD 00 06 */ LDA ObjectPosY,X
/* 1674C: C9 F8 */    CMP #$f8
/* 1674E: F0 07 */    BEQ L16757 ; +                                               ; $A757
/* 16750: E8 */       INX
/* 16751: E0 05 */    CPX #$05
/* 16753: D0 F4 */    BNE L16749 ; -                                               ; $A749

/* 16755: F0 13 */    BEQ L1676A ; ++                                              ; $A76A
L16757: ; +
/* 16757: A9 04 */    LDA #$04
/* 16759: 20 13 A9 */ JSR LaunchMegamanWeaponShot
/* 1675C: A9 1F */    LDA #$1f
/* 1675E: 8D 40 06 */ STA ObjectFireDelay+0  ;delay for Megaman if he's standing

/* 16761: A9 14 */    LDA #$14        ; Shooting Megablaster bullet
/* 16763: 20 77 C4 */ JSR IssueSound                                      ; $C477

/* 16766: A9 C0 */    LDA #$c0
/* 16768: 85 60 */    STA WeaponFiring
L1676A: ; ++
/* 1676A: A6 2F */    LDX RefObjectNum
/* 1676C: 60 */       RTS

MegamanWeaponFire_C:
/* 1676D: A9 0E */    LDA #$0e  ;scissor index in MegamanWeaponFireData
/* 1676F: 85 0C */    STA $0C
/* 16771: 20 AF A7 */ JSR MegamanThrowWeapon
/* 16774: A9 80 */    LDA #$80
/* 16776: 85 63 */    STA $63
/* 16778: A9 02 */    LDA #$02
/* 1677A: 85 64 */    STA $64

/* 1677C: A9 25 */    LDA #$25 ; scissor sound
/* 1677E: 20 77 C4 */ JSR IssueSound                                      ; $C477
/* 16781: 4C C0 A8 */ JMP MegamanWeaponConsumePower

MegamanWeaponFire_I:
/* 16784: A5 5F */    LDA WeaponSelect
/* 16786: 09 C0 */    ORA #$c0
/* 16788: 85 60 */    STA WeaponFiring
/* 1678A: A9 05 */    LDA #$05
/* 1678C: A2 05 */    LDX #$05
/* 1678E: 20 13 A9 */ JSR LaunchMegamanWeaponShot
/* 16791: B0 0D */    BCS L167A0 ; +                                               ; $A7A0
/* 16793: A9 1F */    LDA #$1f
/* 16795: 8D 40 06 */ STA ObjectFireDelay+0

/* 16798: A9 2A */    LDA #$2A ; ice sound
/* 1679A: 20 77 C4 */ JSR IssueSound                                      ; $C477
/* 1679D: 4C C0 A8 */ JMP MegamanWeaponConsumePower
L167A0: ; +
/* 167A0: 60 */       RTS

MegamanWeaponFire_B:
/* 167A1: A9 0D */    LDA #$0d ;bomb index in MegamanWeaponFireData
/* 167A3: 85 0C */    STA $0C
/* 167A5: 20 AF A7 */ JSR MegamanThrowWeapon
/* 167A8: A9 03 */    LDA #$03
/* 167AA: 85 62 */    STA $62
/* 167AC: 20 C0 A8 */ JSR MegamanWeaponConsumePower
MegamanThrowWeapon:
/* 167AF: A5 60 */    LDA WeaponFiring
/* 167B1: D0 17 */    BNE L167CA ; +
/* 167B3: A5 5F */    LDA WeaponSelect
/* 167B5: 09 C0 */    ORA #$C0
/* 167B7: 85 60 */    STA WeaponFiring
/* 167B9: A5 0C */    LDA $0C
/* 167BB: A2 05 */    LDX #$05
/* 167BD: 20 13 A9 */ JSR LaunchMegamanWeaponShot
/* 167C0: B0 08 */    BCS L167CA ; +
/* 167C2: A9 00 */    LDA #$00
/* 167C4: 85 61 */    STA NumberOfFramesSinceShooting
/* 167C6: 20 E6 A8 */ JSR MegamanWeaponThrow
/* 167C9: 60 */       RTS
L167CA: ; +
/* 167CA: 68 */       PLA
/* 167CB: 68 */       PLA
/* 167CC: 60 */       RTS


MegamanWeaponFire_F:
/* 167CD: A5 5F */    LDA WeaponSelect ; f shot
/* 167CF: 09 C0 */    ORA #$c0
/* 167D1: 85 60 */    STA WeaponFiring
/* 167D3: A9 00 */    LDA #$00
/* 167D5: A2 05 */    LDX #$05
/* 167D7: 20 13 A9 */ JSR LaunchMegamanWeaponShot
/* 167DA: B0 18 */    BCS L167F4 ; +
/* 167DC: A9 01 */    LDA #$01         ; f shield?
/* 167DE: A2 06 */    LDX #$06
/* 167E0: 20 13 A9 */ JSR LaunchMegamanWeaponShot
/* 167E3: A9 1F */    LDA #$1f
/* 167E5: 8D 40 06 */ STA ObjectFireDelay+0
/* 167E8: A9 00 */    LDA #$00
/* 167EA: 85 61 */    STA NumberOfFramesSinceShooting

/* 167EC: A9 2E */    LDA #$2E ; fire sound
/* 167EE: 20 77 C4 */ JSR IssueSound                                      ; $C477

/* 167F1: 4C C0 A8 */ JMP MegamanWeaponConsumePower
L167F4: ; +
/* 167F4: A9 00 */    LDA #$00
/* 167F6: 8D 40 06 */ STA ObjectFireDelay+0
L167F9: ; -
/* 167F9: 60 */       RTS


MegamanWeaponFire_E:
/* 167FA: A5 60 */    LDA WeaponFiring
/* 167FC: D0 28 */    BNE L16826 ; +
/* 167FE: A5 5F */    LDA WeaponSelect
/* 16800: 09 C0 */    ORA #$c0
/* 16802: 85 60 */    STA WeaponFiring
/* 16804: A9 06 */    LDA #$06          ;Elecman item index
L16806: ; -
/* 16806: 85 0C */    STA $0C
/* 16808: AA */       TAX
/* 16809: CA */       DEX
/* 1680A: 20 13 A9 */ JSR LaunchMegamanWeaponShot
/* 1680D: E6 0C */    INC $0C
/* 1680F: A5 0C */    LDA $0C
/* 16811: C9 0D */    CMP #$0d         ;Done elecman indexes?
/* 16813: D0 F1 */    BNE L16806 ; -
/* 16815: A9 00 */    LDA #$00
/* 16817: 85 61 */    STA NumberOfFramesSinceShooting
/* 16819: A9 1F */    LDA #$1f
/* 1681B: 8D 40 06 */ STA ObjectFireDelay+0

/* 1681E: A9 1E */    LDA #$1E ;elec sound.
/* 16820: 20 77 C4 */ JSR IssueSound                                      ; $C477

/* 16823: 4C C0 A8 */ JMP MegamanWeaponConsumePower
L16826: ; +
/* 16826: 60 */       RTS


MegamanWeaponFire_G:
/* 16827: A5 60 */    LDA WeaponFiring
/* 16829: 29 40 */    AND #$40
/* 1682B: D0 36 */    BNE L16863 ; ++
/* 1682D: A4 52 */    LDY GutsmanWeaponTargetActive
/* 1682F: C0 FF */    CPY #$ff
/* 16831: F0 2A */    BEQ L1685D ; +
/* 16833: A5 5F */    LDA WeaponSelect
/* 16835: 09 80 */    ORA #$80
/* 16837: 85 60 */    STA WeaponFiring
/* 16839: A9 00 */    LDA #$00
/* 1683B: 85 61 */    STA NumberOfFramesSinceShooting
/* 1683D: A9 04 */    LDA #$04
/* 1683F: 99 21 07 */ STA RoomActiveTable+1,Y    ;modify the gutsblock as disintegrated
/* 16842: 20 6A F7 */ JSR LoadGutsblockPosition
/* 16845: 20 E8 CD */ JSR DrawBlockFromActiveLevelMap
/* 16848: A9 F8 */    LDA #$f8
/* 1684A: 8D 05 06 */ STA ObjectPosY+5
/* 1684D: A9 02 */    LDA #$02                   ;guts debris
/* 1684F: A2 05 */    LDX #$05
/* 16851: 20 13 A9 */ JSR LaunchMegamanWeaponShot
/* 16854: A4 31 */    LDY CurrentStage
/* 16856: B9 88 A8 */ LDA GutsblockObjectSpritenum,Y
/* 16859: 9D 00 04 */ STA ObjectSpriteNum,X
/* 1685C: 60 */       RTS
L1685D: ; + ; Not able to do anything
/* 1685D: A9 00 */    LDA #$00
/* 1685F: 8D 40 06 */ STA ObjectFireDelay+0
/* 16862: 60 */       RTS
L16863: ; ++ ; Carrying a gutsblock and now pressed B. Launch the shot.
/* 16863: A5 61 */    LDA NumberOfFramesSinceShooting
/* 16865: C9 10 */    CMP #$10
/* 16867: B0 1E */    BCS L16887
/* 16869: A9 F8 */    LDA #$f8
/* 1686B: A2 05 */    LDX #$05
/* 1686D: 9D 00 06 */ STA ObjectPosY,X
/* 16870: A9 03 */    LDA #$03                      ;guts block being carried
/* 16872: 20 13 A9 */ JSR LaunchMegamanWeaponShot
/* 16875: A9 10 */    LDA #$10
/* 16877: 85 61 */    STA NumberOfFramesSinceShooting
/* 16879: 20 E6 A8 */ JSR MegamanWeaponThrow
/* 1687C: A4 31 */    LDY CurrentStage
/* 1687E: B9 88 A8 */ LDA GutsblockObjectSpritenum,Y
/* 16881: 9D 00 04 */ STA ObjectSpriteNum,X
/* 16884: 4C C0 A8 */ JMP MegamanWeaponConsumePower
L16887:
/* 16887: 60 */       RTS

GutsblockObjectSpritenum: ;indexed by stage. Stages I,B,F don't have gutsblocks.
    .byte $6C, $00, $00, $00, $6C, $32
    .byte $6C, $6C, $6C, $6C, $6C

MegamanWeaponFire_M:
/* 16893: A2 05 */    ldx #$05 ;find a slot for the beam
L16895: ; -
/* 16895: BD B0 05 */ LDA MagnetBeamAge,X
/* 16898: F0 06 */    BEQ L168A0 ; +
/* 1689A: E8 */       INX
/* 1689B: E0 0A */    CPX #$0a
/* 1689D: D0 F6 */    BNE L16895 ; -
/* 1689F: 60 */       RTS
L168A0: ; +
/* 168A0: 8E A0 05 */ STX MagnetBeamLength
/* 168A3: A9 0F */    LDA #$0f             ;magnetbeam index in MegamanWeaponFireData
/* 168A5: 20 13 A9 */ JSR LaunchMegamanWeaponShot
/* 168A8: A9 00 */    LDA #$00
/* 168AA: 9D A0 05 */ STA MagnetBeamLength,X
/* 168AD: 9D E0 05 */ STA MagnetBeamCyclePos,X
/* 168B0: A9 9C */    LDA #$9C
/* 168B2: 9D B0 05 */ STA MagnetBeamAge,X

/* 168B5: A5 5F */    LDA WeaponSelect
/* 168B7: 09 C0 */    ORA #$C0
/* 168B9: 85 60 */    STA WeaponFiring

/* 168BB: A9 13 */    LDA #$13 ;Bzzt sound
/* 168BD: 20 77 C4 */ JSR IssueSound                                      ; $C477
MegamanWeaponConsumePower:
/* 168C0: A6 5F */    LDX WeaponSelect

/* 168C2: F6 9E */    INC MetersFraction,X
/* 168C4: B5 9E */    LDA MetersFraction,X
/* 168C6: DD D7 A8 */ CMP WeaponPowerConsumptionDenominator-1,X
/* 168C9: D0 0C */    BNE L168D7 ; +

/* 168CB: A9 00 */    LDA #$00
/* 168CD: 95 9E */    STA MetersFraction,X
/* 168CF: 38 */       SEC
/* 168D0: B5 6A */    LDA Meters,X
/* 168D2: FD DE A8 */ SBC WeaponPowerConsumptionNominator-1,X
/* 168D5: 95 6A */    STA Meters,X
L168D7: ; +
/* 168D7: 60 */       RTS

; At A8D8
WeaponPowerConsumptionDenominator: ; If it says "5" here, it'll take 5 shots until power is consumed
    ;     C,I,B,F,E,G,M
    .byte 1,1,1,1,1,1,1
WeaponPowerConsumptionNominator: ; At A8DF
    ;     C,I,B,F,E,G,M
    .byte 1,1,2,1,1,2,2

MegamanWeaponThrow:
/* 168E6: A9 00 */    LDA #$00
/* 168E8: 8D 40 04 */ STA ObjectUnknown440+0
/* 168EB: AD 00 04 */ LDA ObjectSpriteNum+0
/* 168EE: C9 09 */    CMP #$09         ;jumping?
/* 168F0: D0 07 */    BNE L168F9 ; +
/* 168F2: A9 00 */    LDA #$00
/* 168F4: 8D 40 06 */ STA ObjectFireDelay+0
/* 168F7: F0 19 */    BEQ L16912 ; +++
L168F9: ; +
/* 168F9: C9 15 */    CMP #$15        ;climbing?
/* 168FB: F0 04 */    BEQ L16901 ; +
/* 168FD: C9 17 */    CMP #$17        ;unclimbing?
/* 168FF: D0 07 */    BNE L16908 ; ++
L16901: ; +
/* 16901: A9 1F */    LDA #$1f
/* 16903: 8D 40 06 */ STA ObjectFireDelay+0
/* 16906: D0 0A */    BNE L16912 ; +++
L16908: ; ++
; Make Megaman throw something
/* 16908: AD 20 04 */ LDA ObjectFlags+0
/* 1690B: 29 F0 */    AND #$f0
/* 1690D: 09 04 */    ORA #$04
/* 1690F: 8D 20 04 */ STA ObjectFlags+0
L16912: ; +++
/* 16912: 60 */       RTS


LaunchMegamanWeaponShot:
;     X = object number
;     A = fire type
;            0 = F shot
;            1 = F shield (or vice versa)
;            2 = Guts (debris)
;            3 = Guts (?)
;            4 = P
;            5 = I
;      6..#$0C = Elec (each parts of them...)
;         #$0D = Bomb
;         #$0E = C
;         #$0F = M

; Y = A*9

/* 16913: 85 0C */    STA $0C
/* 16915: 0A */       ASL A
/* 16916: 0A */       ASL A
/* 16917: 0A */       ASL A
/* 16918: 18 */       CLC
/* 16919: 65 0C */    ADC $0C
/* 1691B: A8 */       TAY
/* 1691C: BD 00 06 */ LDA ObjectPosY,X
/* 1691F: C9 F8 */    CMP #$f8
/* 16921: F0 02 */    BEQ L16925 ; +
/* 16923: 38 */       SEC
/* 16924: 60 */       RTS
L16925: ; +
/* 16925: B9 99 A9 */ LDA MegamanWeaponFireData+0,Y
/* 16928: 9D 00 04 */ STA ObjectSpriteNum,X
/* 1692B: B9 9A A9 */ LDA MegamanWeaponFireData+1,Y
/* 1692E: 9D 40 04 */ STA ObjectUnknown440,X
/* 16931: AD 20 04 */ LDA ObjectFlags+0
/* 16934: 29 40 */    AND #$40
/* 16936: 08 */       PHP
/* 16937: 19 9B A9 */  ORA MegamanWeaponFireData+2,Y
/* 1693A: 9D 20 04 */  STA ObjectFlags,X
/* 1693D: A9 00 */     LDA #$00
/* 1693F: 9D A0 06 */  STA ObjectLifeCycleCounter,X
/* 16942: 9D 40 06 */  STA ObjectFireDelay,X
/* 16945: 9D A0 04 */  STA ObjectPosXfraction,X
/* 16948: 9D 20 06 */  STA ObjectPosYfraction,X
/* 1694B: B9 9C A9 */  LDA MegamanWeaponFireData+3,Y
/* 1694E: 9D 60 06 */  STA ObjectYSpeedFraction,X
/* 16951: B9 9D A9 */  LDA MegamanWeaponFireData+4,Y
/* 16954: 9D 80 06 */  STA ObjectYSpeed,X
/* 16957: B9 9E A9 */  LDA MegamanWeaponFireData+5,Y
/* 1695A: 9D E0 04 */  STA ObjectXSpeedFraction,X
/* 1695D: B9 9F A9 */  LDA MegamanWeaponFireData+6,Y
/* 16960: 9D C0 04 */  STA ObjectXSpeed,X
/* 16963: 18 */        CLC
/* 16964: AD 00 06 */  LDA ObjectPosY+0
/* 16967: 79 A0 A9 */  ADC MegamanWeaponFireData+7,Y
/* 1696A: C9 F8 */     CMP #$f8
/* 1696C: D0 02 */     BNE L16970 ; +
/* 1696E: A9 F9 */     LDA #$f9
L16970: ; +
/* 16970: 9D 00 06 */  STA ObjectPosY,X
/* 16973: AD 80 04 */  LDA ObjectPosX+0
/* 16976: 28 */       PLP
/* 16977: F0 0F */    BEQ L16988 ; +
/* 16979: 18 */       CLC
/* 1697A: 79 A1 A9 */ ADC MegamanWeaponFireData+8,Y
/* 1697D: 9D 80 04 */ STA ObjectPosX,X
/* 16980: AD 60 04 */ LDA ObjectPosScreen+0                             ; $0460
/* 16983: 69 00 */    ADC #$00
/* 16985: 4C 94 A9 */ JMP L16994 ; ++
L16988: ; +
/* 16988: 38 */       SEC
/* 16989: F9 A1 A9 */ SBC MegamanWeaponFireData+8,Y
/* 1698C: 9D 80 04 */ STA ObjectPosX,X
/* 1698F: AD 60 04 */ LDA ObjectPosScreen+0                             ; $0460
/* 16992: E9 00 */    SBC #$00
L16994: ; ++
/* 16994: 9D 60 04 */ STA ObjectPosScreen,X
/* 16997: 18 */       CLC
/* 16998: 60 */       RTS

MegamanWeaponFireData: ; at A999
        ; 0->ObjectSpriteNum
        ; 1->ObjectUnknown440
        ; 2->ObjectFlags
        ; 3->ObjectYSpeedFraction
        ; 4->ObjectYSpeed
        ; 5->ObjectXSpeedFraction
        ; 6->ObjectXSpeed
        ; 7->ObjectPosY
        ; 8->ObjectPosX
    .byte $60,$00,$04,$00,$00,$00,$04,$00,$10 ;f shot
    .byte $61,$00,$04,$00,$00,$00,$00,$EC,$00 ;f shield (?)
    .byte $00,$00,$24,$00,$00,$00,$00,$F0,$10 ;guts debris
    .byte $6C,$00,$15,$00,$03,$A0,$03,$F0,$10 ;guts block being carried(?)
    .byte $1A,$00,$00,$00,$00,$00,$04,$00,$10 ;P
    .byte $5C,$00,$04,$00,$00,$00,$04,$00,$10 ;I
    .byte $62,$40,$04,$00,$00,$50,$01,$FC,$10 ;elec
    .byte $63,$30,$04,$00,$00,$50,$01,$08,$14 ;elec
    .byte $64,$20,$04,$00,$00,$50,$01,$FC,$18 ;elec
    .byte $65,$10,$04,$00,$00,$50,$01,$EC,$20 ;elec
    .byte $66,$01,$04,$00,$00,$50,$01,$EC,$28 ;elec
    .byte $6A,$00,$04,$00,$06,$00,$00,$F0,$10 ;elec
    .byte $6B,$00,$04,$00,$FA,$00,$00,$10,$10 ;elec
    .byte $5D,$00,$11,$00,$03,$80,$01,$FC,$0C ;B
    .byte $5B,$00,$04,$00,$02,$00,$02,$FC,$0C ;C
    .byte $5E,$00,$00,$00,$00,$00,$00,$00,$00 ;M

; Seems to animate the object

DoEnemyAI:
/* 16A29: BD E0 06 */ LDA ObjectType,X
/* 16A2C: 0A */       ASL A
/* 16A2D: A8 */       TAY
/* 16A2E: B9 3B AA */ LDA EnemyAIaddr+0,Y
/* 16A31: 85 04 */    STA $04
/* 16A33: B9 3C AA */ LDA EnemyAIaddr+1,Y
/* 16A36: 85 05 */    STA $05
/* 16A38: 6C 04 00 */ JMP ($0004)

EnemyAIaddr: ;at AA77
.word AI_Blader               ; 00    Blader/Bunbi Heri
.word AI_Met                  ; 01    Met/Metall/Hard Hat
.word AI_Flea                 ; 02    Flea
.word AI_Spine                ; 03    Spine/Gabyoall
.word AI_Eyecube              ; 04    Octopus Battery/Suzy (vertical moving)
.word AI_ScrewBomberFloor     ; 05    Screw bomber (on floor)
.word AI_Watcher              ; 06    Watcher
.word AI_SinewaveFlier        ; 07    Peng
.word AI_SinewaveFlier        ; 08    Killer bullet
.word AI_BigEye               ; 09    Big Eye
.word AI_FlyingShell          ; 0A    Flying Shell/Shellgun/Hamburger
.word AI_Beak                 ; 0B    Beak/wallgun Right 1
.word AI_Electricity          ; 0C    Damaging electricity instance 1
.word AI_Bombomb              ; 0D    Bombomb cluster
.word AI_ZigzagFire           ; 0E    Zig-zag fire
.word AI_VerticalFire         ; 0F    Vertical fire
.word AI_TemporaryBlockManager ;10
.word AI_Generic              ; 11
.word AI_SniperJoe            ; 12    Sniper Joe
.word AI_FootHolder           ; 13    Foot holder
.word AI_HeadLosingRobot      ; 14    Crazy Razy spawner
.word AI_PicketMan            ; 15    Picket Man
.word AI_ScrewBomberCeiling   ; 16    Screw bomber (on ceiling)
.word AI_Beak                 ; 17    Beak/wallgun Left 1
.word AI_GenericWithLifeTimer ; 18    Zig-zag flame thrower
.word AI_Electricity          ; 19    Tube electricity instance 1?
.word AI_Generic              ; 1A    Crazy Razy shot
.word AI_Object1B             ; 1B    Enemy death explosion
.word AI_Object1C             ; 1C    Scissor machine?
.word AI_Object1D             ; 1D    Crazy Razy combined/separating top
.word AI_Object1E             ; 1E    Crazy Razy combined bottom
.word AI_RogueRobotHead       ; 1F    Crazy Razy separated top
.word AI_Object20             ; 20
.word AI_Eyecube              ; 21    Octopus Battery/Suzy (horizontal moving)
.word AI_Beak                 ; 22    Beak/wallgun Right 2
.word AI_Beak                 ; 23    Beak/wallgun Left 2
.word AI_Electricity          ; 24    Damaging electricity instance 2
.word AI_Electricity          ; 25    Tube electricity instance 2
.word AI_BombombCluster       ; 26    Bombomb individual
.word AI_TemporaryBlock       ; 27    Temporary block
.word AI_Object28             ; 28    (Bomb explodes) individual explosion part (lives for 14 frames)
.word AI_Object29             ; 29    Crazy Razy separated bottom
.word AI_Object2A             ; 2A
.word AI_Generic              ; 2B
.word AI_LiftAnimator         ; 2C    Lift
.word AI_Generic              ; 2D    Diagonal bullet dowm (from Met)
.word AI_Generic              ; 2E
.word AI_Object2F             ; 2F Gutsman stage: Enable lift sound. Wily3: Control waterpipe
.word AI_Object30             ; 30 Gutsman stage: Stop SFX. Wily3: Setup BG palette
.word AI_TackleFire           ; 31    Tackle fire
.word AI_Object32             ; 32 Rotates palettes (Fireman and Wily2 battle)
.word AI_Object33             ; 33  Scissor weapon?
.word AI_Generic              ; 34  Ice weapon?
.word AI_Object35             ; 35  Bomb weapon?
.word AI_Generic              ; 36  Fire weapon?
.word AI_Object37             ; 37  Fire shield weapon?
.word AI_Object38             ; 38
.word AI_Object39             ; 39
.word AI_Object3A             ; 3A  Boss8 (Wily3 boss)
.word AI_Object3B             ; 3B

.word AI_GenericWithLifeTimer            ; 3C    Bonus pearl
.word AI_GenericWithLifeTimer            ; 3D    Small weapon capsule
.word AI_GenericWithLifeTimer            ; 3E    Small life capsule
.word AI_GenericWithLifeTimer            ; 3F    Large weapon capsule
.word AI_GenericWithLifeTimer            ; 40    Large Life Capsule
.word AI_GenericWithLifeTimer            ; 41    ExtraLife

.word AI_Generic   ; 42    Magnet beam adapter
.word AI_Generic   ; 43    Teleport
.word AI_Generic   ; 44    Level-end
.word AI_Generic   ; 45    Yashichi
.word AI_Generic   ; 46
.word AI_Generic   ; 47

.word AI_Object48  ; 48
.word AI_Generic   ; 49    Random powerup (?)
.word AI_Object4A  ; 4A    Wily falling/begging

AI_Blader:
/* 16AD1: BD 40 06 */ LDA ObjectFireDelay,X
/* 16AD4: D0 3F */    BNE L16B15 ; ++
/* 16AD6: 20 3B F6 */ JSR EnemySearchMegaman
/* 16AD9: C9 28 */    CMP #$28
/* 16ADB: 90 03 */    BCC L16AE0 ; +                                               ; $AAE0
/* 16ADD: 4C 5F AB */ JMP L16B5F ; +++
L16AE0: ; +
/* 16AE0: 38 */       SEC
/* 16AE1: AD 00 06 */ LDA ObjectPosY+0
/* 16AE4: FD 00 06 */ SBC ObjectPosY,X
/* 16AE7: 08 */       PHP
/* 16AE8: B0 04 */    BCS L16AEE ; +
/* 16AEA: 49 FF */    EOR #$ff
/* 16AEC: 69 01 */    ADC #$01
L16AEE: ; +
/* 16AEE: AA */       TAX
/* 16AEF: A0 00 */    LDY #$00
/* 16AF1: A9 18 */    LDA #$18
/* 16AF3: 20 AC C5 */ JSR EnemyCalculateJumpCurveToHitMegaman
/* 16AF6: A6 2F */    LDX RefObjectNum
/* 16AF8: A5 04 */    LDA $04
/* 16AFA: 9D 60 06 */ STA ObjectYSpeedFraction,X
/* 16AFD: A5 05 */    LDA $05
/* 16AFF: 9D 80 06 */ STA ObjectYSpeed,X
/* 16B02: 28 */       PLP
/* 16B03: 90 03 */    BCC L16B08 ; +
/* 16B05: 20 63 AB */  JSR ObjectFlipYmovement
L16B08: ; +
/* 16B08: A9 33 */    LDA #$33
/* 16B0A: 9D 40 06 */ STA ObjectFireDelay,X
/* 16B0D: A9 00 */    LDA #$00
/* 16B0F: 9D E0 04 */ STA ObjectXSpeedFraction,X
/* 16B12: 9D C0 04 */ STA ObjectXSpeed,X
L16B15: ; ++
/* 16B15: BD 40 06 */ LDA ObjectFireDelay,X
/* 16B18: C9 1B */    CMP #$1B
/* 16B1A: 90 14 */    BCC L16B30 ; +
/* 16B1C: 18 */        CLC
/* 16B1D: BD E0 04 */  LDA ObjectXSpeedFraction,X
/* 16B20: 69 18 */     ADC #$18
/* 16B22: 9D E0 04 */  STA ObjectXSpeedFraction,X
/* 16B25: BD C0 04 */  LDA ObjectXSpeed,X
/* 16B28: 69 00 */     ADC #$00
/* 16B2A: 9D C0 04 */  STA ObjectXSpeed,X
/* 16B2D: 4C 48 AB */  JMP L16B48 ; ++
L16B30: ; +
/* 16B30: C9 1A */    CMP #$1A
/* 16B32: D0 03 */    BNE L16B37 ; +
/* 16B34: 20 63 AB */ JSR ObjectFlipYmovement
L16B37: ; +
/* 16B37: 18 */       CLC
/* 16B38: BD E0 04 */ LDA ObjectXSpeedFraction,X
/* 16B3B: 69 04 */    ADC #$04
/* 16B3D: 9D E0 04 */ STA ObjectXSpeedFraction,X
/* 16B40: BD C0 04 */ LDA ObjectXSpeed,X
/* 16B43: 69 00 */    ADC #$00
/* 16B45: 9D C0 04 */ STA ObjectXSpeed,X
L16B48: ; ++
/* 16B48: DE 40 06 */ DEC ObjectFireDelay,X
/* 16B4B: D0 12 */    BNE L16B5F ; +++
/* 16B4D: A9 00 */    LDA #$00
/* 16B4F: 9D 80 06 */ STA ObjectYSpeed,X
/* 16B52: 9D 60 06 */ STA ObjectYSpeedFraction,X
/* 16B55: A9 01 */    LDA #$01
/* 16B57: 9D C0 04 */ STA ObjectXSpeed,X
/* 16B5A: A9 20 */    LDA #$20
/* 16B5C: 9D E0 04 */ STA ObjectXSpeedFraction,X
L16B5F: ; +++
/* 16B5F: 20 49 BE */ JSR EnemyAI_MovementsAndDamageCheck
/* 16B62: 60 */       RTS


ObjectFlipYmovement:
/* 16B63: 18 */       CLC
/* 16B64: BD 60 06 */ LDA ObjectYSpeedFraction,X
/* 16B67: 49 FF */    EOR #$ff
/* 16B69: 69 01 */    ADC #$01
/* 16B6B: 9D 60 06 */ STA ObjectYSpeedFraction,X
/* 16B6E: BD 80 06 */ LDA ObjectYSpeed,X
/* 16B71: 49 FF */    EOR #$ff
/* 16B73: 69 00 */    ADC #$00
/* 16B75: 9D 80 06 */ STA ObjectYSpeed,X
/* 16B78: 60 */       RTS


AI_Met:
/* 16B79: 20 A5 B3 */ JSR F173A5
/* 16B7C: BD 40 04 */ LDA ObjectUnknown440,X
/* 16B7F: 29 F0 */    AND #$f0
/* 16B81: F0 45 */    BEQ L16BC8
/* 16B83: A0 10 */    LDY #$10
/* 16B85: BD 40 06 */ LDA ObjectFireDelay,X
/* 16B88: F0 2C */    BEQ L16BB6 ; ++                                              ; $ABB6
/* 16B8A: C9 10 */    CMP #$10
/* 16B8C: D0 53 */    BNE L16BE1

/* 16B8E: A9 02 */    LDA #$02
/* 16B90: 85 0C */    STA $0C
L16B92: ; -
/* 16B92: A9 2D */    LDA #$2d
/* 16B94: 20 63 F6 */ JSR CreateEnemy                                       ; $F663
/* 16B97: B0 17 */    BCS L16BB0 ; +                                               ; $ABB0

; Make this bullet appear 4 pixels above the Met object
/* 16B99: 38 */       SEC
/* 16B9A: BD 00 06 */ LDA ObjectPosY,X
/* 16B9D: E9 04 */    SBC #$04
/* 16B9F: 9D 00 06 */ STA ObjectPosY,X

/* 16BA2: A5 0C */    LDA $0C
/* 16BA4: 0A */       ASL A
/* 16BA5: 0A */       ASL A
/* 16BA6: 69 4C */    ADC #$4c
/* 16BA8: A8 */       TAY

/* 16BA9: 20 33 F5 */ JSR InitObjectDefaultSpeed
/* 16BAC: C6 0C */    DEC $0C
/* 16BAE: 10 E2 */    BPL L16B92 ; -                                               ; $AB92
L16BB0: ; +
/* 16BB0: A6 2F */    LDX RefObjectNum
/* 16BB2: A0 10 */    LDY #$10
/* 16BB4: D0 2B */    BNE L16BE1
L16BB6: ; ++
/* 16BB6: A9 03 */    LDA #$03
/* 16BB8: 20 A0 C5 */ JSR RandomFunc
/* 16BBB: A6 2F */    LDX RefObjectNum
/* 16BBD: A8 */       TAY
/* 16BBE: B9 EF AB */ LDA L16BEF,Y
/* 16BC1: 9D 40 06 */ STA ObjectFireDelay,X
/* 16BC4: A0 00 */    LDY #$00
/* 16BC6: F0 19 */    BEQ L16BE1



L16BC8:
/* 16BC8: 20 3B F6 */ JSR EnemySearchMegaman
/* 16BCB: 85 0C */    STA $0C
/* 16BCD: A0 00 */    LDY #$00
/* 16BCF: BD 40 06 */ LDA ObjectFireDelay,X
/* 16BD2: D0 0D */    BNE L16BE1
/* 16BD4: A5 0C */    LDA $0C
/* 16BD6: C9 58 */    CMP #$58
/* 16BD8: B0 0A */    BCS L16BE4
/* 16BDA: A9 20 */    LDA #$20
/* 16BDC: 9D 40 06 */ STA ObjectFireDelay,X
/* 16BDF: A0 10 */    LDY #$10


L16BE1:
/* 16BE1: DE 40 06 */ DEC ObjectFireDelay,X
L16BE4:
/* 16BE4: 98 */       TYA
/* 16BE5: 9D 40 04 */ STA ObjectUnknown440,X
/* 16BE8: 20 A3 AF */ JSR F16FA3
/* 16BEB: 20 49 BE */ JSR EnemyAI_MovementsAndDamageCheck
/* 16BEE: 60 */       RTS

L16BEF:
/* 16BEF: 1F */       .byte $1f
/* 16BF0: 3E */       .byte $3E
/* 16BF1: 9C */       .byte $9C


; Flea and Big Eye both use the same AI routine, but with different parameter

AI_BigEye:
/* 16BF2: A9 6C */    LDA #$6C ;index into F54C_table for BigEye
/* 16BF4: D0 02 */    BNE L16BF8 ; +                                               ; $ABF8

AI_Flea:
/* 16BF6: A9 00 */    LDA #$00 ;index into F54C_table for Flea
L16BF8: ; +
/* 16BF8: 85 0C */    STA $0C
/* 16BFA: BD 40 04 */ LDA ObjectUnknown440,X
/* 16BFD: C9 10 */    CMP #$10
/* 16BFF: 90 3E */    BCC L16C3F ; ++++                                            ; $AC3F


/* 16C01: A5 23 */    LDA FrameCounter    ; Skip every 4th frame
/* 16C03: 29 03 */    AND #$03
/* 16C05: F0 37 */    BEQ L16C3E ; +++                                             ; $AC3E

/* 16C07: BD 80 06 */ LDA ObjectYSpeed,X
/* 16C0A: 08 */       PHP
/* 16C0B: 20 49 BE */  JSR EnemyAI_MovementsAndDamageCheck
/* 16C0E: 28 */       PLP
/* 16C0F: 10 21 */    BPL L16C32 ; ++                                              ; $AC32
/* 16C11: A5 2B */    LDA $2B
/* 16C13: F0 1D */    BEQ L16C32 ; ++                                              ; $AC32

; If ObjectType = Big Eye then issue sound
/* 16C15: BD E0 06 */ LDA ObjectType,X
/* 16C18: C9 09 */    CMP #$09
/* 16C1A: D0 07 */    BNE L16C23 ; +                                               ; $AC23
/* 16C1C: A9 29 */    LDA #$29        ; Big Eye sound
/* 16C1E: 20 77 C4 */ JSR IssueSound                                      ; $C477
/* 16C21: A6 2F */    LDX RefObjectNum
L16C23: ; +
/* 16C23: A9 0F */    LDA #$0f
/* 16C25: 9D 40 06 */ STA ObjectFireDelay,X
/* 16C28: A9 00 */    LDA #$00
/* 16C2A: 9D E0 04 */ STA ObjectXSpeedFraction,X
/* 16C2D: 9D C0 04 */ STA ObjectXSpeed,X
/* 16C30: F0 09 */    BEQ L16C3B ; +                                               ; $AC3B
L16C32: ; ++
/* 16C32: BD E0 06 */ LDA ObjectType,X
/* 16C35: C9 1B */    CMP #$1b
/* 16C37: F0 05 */    BEQ L16C3E ; +++                                             ; $AC3E
/* 16C39: A9 10 */    LDA #$10
L16C3B: ; +
/* 16C3B: 9D 40 04 */ STA ObjectUnknown440,X
L16C3E: ; +++
/* 16C3E: 60 */       RTS
L16C3F: ; ++++
/* 16C3F: A9 00 */    LDA #$00
/* 16C41: DE 40 06 */ DEC ObjectFireDelay,X
/* 16C44: D0 11 */    BNE L16C57 ; +
/* 16C46: 20 3B F6 */ JSR EnemySearchMegaman
/* 16C49: A5 46 */    LDA RandomSeed
/* 16C4B: 29 01 */    AND #$01
/* 16C4D: 0A */       ASL A
/* 16C4E: 0A */       ASL A
/* 16C4F: 65 0C */    ADC $0C
/* 16C51: A8 */       TAY
/* 16C52: 20 33 F5 */ JSR InitObjectDefaultSpeed
/* 16C55: A9 10 */    LDA #$10
L16C57: ; +
/* 16C57: 9D 40 04 */ STA ObjectUnknown440,X
/* 16C5A: 20 49 BE */ JSR EnemyAI_MovementsAndDamageCheck
/* 16C5D: 60 */       RTS

AI_Spine:
; if spine stunned #$7E => $6A0,X (disable for 126 frames)
/* 16C5E: BD 20 04 */ LDA ObjectFlags,X
/* 16C61: 29 80 */    AND #$80
/* 16C63: F0 05 */    BEQ L16C6A ; +                                               ; $AC6A
/* 16C65: A9 7E */    LDA #$7e
/* 16C67: 9D A0 06 */ STA ObjectLifeCycleCounter,X
L16C6A: ; +

; Handle spine's chasing of Megaman
; Y = 1 if spine and Megaman are are at the same Y position, 0 otherwise
/* 16C6A: A0 00 */    LDY #$00
/* 16C6C: 38 */       SEC
/* 16C6D: BD 00 06 */ LDA ObjectPosY,X
/* 16C70: E9 08 */    SBC #$08
/* 16C72: CD 00 06 */ CMP ObjectPosY+0
/* 16C75: D0 01 */    BNE L16C78 ; +                                               ; $AC78
/* 16C77: C8 */       INY
L16C78: ; +

; $4E0 = spine speed
/* 16C78: B9 C6 AC */ LDA SpineSpeedTableFractions,Y
/* 16C7B: 9D E0 04 */ STA ObjectXSpeedFraction,X

/* 16C7E: B9 C8 AC */ LDA SpineSpeedTable,Y
/* 16C81: 9D C0 04 */ STA ObjectXSpeed,X


/* 16C84: 20 49 BE */ JSR EnemyAI_MovementsAndDamageCheck


/* 16C87: A5 2A */    LDA $2A
/* 16C89: F0 09 */    BEQ L16C94 ; +                                               ; $AC94

L16C8B: ; -
; Flip direction
/* 16C8B: BD 20 04 */ LDA ObjectFlags,X
/* 16C8E: 49 40 */    EOR #$40
/* 16C90: 9D 20 04 */ STA ObjectFlags,X
/* 16C93: 60 */       RTS
L16C94: ; +

; #$00 => Y if facing right, #$02 => Y if facing left
/* 16C94: A0 00 */    LDY #$00
/* 16C96: BD 20 04 */ LDA ObjectFlags,X
/* 16C99: 29 40 */    AND #$40
/* 16C9B: D0 02 */    BNE L16C9F ; +                                               ; $AC9F
/* 16C9D: A0 02 */    LDY #$02
L16C9F: ; +

; ObjectPosX + #$10 => $00,$01 if facing right
; ObjectPosX - #$10 => $00,$01 if facing left

/* 16C9F: 18 */       CLC
/* 16CA0: BD 80 04 */ LDA ObjectPosX,X
/* 16CA3: 79 C3 AC */ ADC SpineCollisionTestTable+1,Y
/* 16CA6: 85 00 */    STA $00
/* 16CA8: BD 60 04 */ LDA ObjectPosScreen,X
/* 16CAB: 79 C2 AC */ ADC SpineCollisionTestTable+0,Y
/* 16CAE: 85 01 */    STA $01

; ObjectPosY + #$06 => $03
/* 16CB0: 18 */       CLC
/* 16CB1: BD 00 06 */ LDA ObjectPosY,X
/* 16CB4: 69 06 */    ADC #$06
/* 16CB6: 85 03 */    STA $03

/* 16CB8: 20 2D CC */ JSR DoCollisionCheckFor
/* 16CBB: A5 2B */    LDA $2B
/* 16CBD: C9 01 */    CMP #$01
/* 16CBF: D0 CA */    BNE L16C8B ; -                                               ; $AC8B

/* 16CC1: 60 */       RTS

SpineCollisionTestTable:
    .byte $00,$10 ; +16
    .byte $FF,$F0 ; -16
SpineSpeedTableFractions:
    .byte $61  ;slow speed: 0.379 pix/sec
    .byte $E0  ;fast speed: 1.875 pix/sec
SpineSpeedTable:
    .byte $00
    .byte $01

AI_Eyecube:
/* 16CCA: BD 80 06 */ LDA ObjectYSpeed,X
/* 16CCD: 48 */       PHA
/* 16CCE: 20 49 BE */  JSR EnemyAI_MovementsAndDamageCheck
/* 16CD1: B0 51 */     BCS AI_EyecubeRTS                            ; $AD24
/* 16CD3: BD E0 06 */  LDA ObjectType,X
/* 16CD6: C9 1B */     CMP #$1b
/* 16CD8: F0 4A */     BEQ AI_EyecubeRTS                            ; $AD24
/* 16CDA: 68 */       PLA
/* 16CDB: 9D 80 06 */ STA ObjectYSpeed,X  ;This prevents Eyecube from starting falling

/* 16CDE: A9 00 */    LDA #$00
/* 16CE0: 9D 60 06 */ STA ObjectYSpeedFraction,X

/* 16CE3: BD 40 06 */ LDA ObjectFireDelay,X
/* 16CE6: F0 16 */    BEQ L16CFE ; +                                               ; $ACFE
/* 16CE8: DE 40 06 */ DEC ObjectFireDelay,X
/* 16CEB: D0 27 */    BNE L16D14 ; ++                                              ; $AD14
/* 16CED: A9 30 */    LDA #$30
/* 16CEF: 9D 40 04 */ STA ObjectUnknown440,X

; Make object turn direction
/* 16CF2: BD 20 04 */ LDA ObjectFlags,X
/* 16CF5: 49 40 */    EOR #$40
/* 16CF7: 9D 20 04 */ STA ObjectFlags,X

/* 16CFA: 20 63 AB */ JSR ObjectFlipYmovement
/* 16CFD: 60 */       RTS
L16CFE: ; +
/* 16CFE: A5 2A */    LDA $2A
/* 16D00: 05 2B */    ORA $2B
/* 16D02: F0 10 */    BEQ L16D14 ; ++                                              ; $AD14
/* 16D04: A9 10 */    LDA #$10
/* 16D06: 9D 40 04 */ STA ObjectUnknown440,X
/* 16D09: A5 46 */    LDA RandomSeed
/* 16D0B: 29 01 */    AND #$01
/* 16D0D: A8 */       TAY
/* 16D0E: B9 26 AD */ LDA EyecubeDelayTable,Y
/* 16D11: 9D 40 06 */ STA ObjectFireDelay,X
L16D14: ; ++
/* 16D14: BD 40 04 */ LDA ObjectUnknown440,X
/* 16D17: 29 10 */    AND #$10
/* 16D19: D0 08 */    BNE L16D23 ; +
/* 16D1B: BD 40 04 */ LDA ObjectUnknown440,X
/* 16D1E: 29 F0 */    AND #$f0
/* 16D20: 9D 40 04 */ STA ObjectUnknown440,X
L16D23: ; +
/* 16D23: 60 */       RTS

AI_EyecubeRTS:
/* 16D24: 68 */       PLA
/* 16D25: 60 */       RTS

EyecubeDelayTable: .byt $3F, $7E

AI_Watcher:
/* 16D28: BD 40 04 */ LDA $440,X
/* 16D2B: 29 F0 */    AND #$f0
/* 16D2D: D0 2F */    BNE L16D5E
/* 16D2F: BD 20 04 */ LDA ObjectFlags,X
/* 16D32: 29 08 */    AND #$08
/* 16D34: D0 23 */    BNE L16D59
/* 16D36: A0 00 */    LDY #$00
/* 16D38: 38 */       SEC
/* 16D39: AD 00 06 */ LDA ObjectPosY+0
/* 16D3C: FD 00 06 */ SBC ObjectPosY,X
/* 16D3F: B0 06 */    BCS L16D47
/* 16D41: 49 FF */    EOR #$ff
/* 16D43: 69 01 */    ADC #$01
/* 16D45: A0 02 */    LDY #$02
L16D47:
/* 16D47: C9 28 */    CMP #$28
/* 16D49: 08 */       PHP
/* 16D4A: B9 73 AD */ LDA AD73_table+0,Y
/* 16D4D: 9D 60 06 */ STA ObjectYSpeedFraction,X
/* 16D50: B9 74 AD */ LDA AD73_table+1,Y
/* 16D53: 9D 80 06 */ STA ObjectYSpeed,X
/* 16D56: 28 */       PLP
/* 16D57: 90 05 */    BCC L16D5E
L16D59:
/* 16D59: A9 00 */    LDA #$00
/* 16D5B: 9D 40 04 */ STA ObjectUnknown440,X
L16D5E:
/* 16D5E: 20 49 BE */ JSR EnemyAI_MovementsAndDamageCheck
/* 16D61: BD 40 04 */ LDA ObjectUnknown440,X
/* 16D64: C9 20 */    CMP #$20
/* 16D66: D0 0A */    BNE L16D72
/* 16D68: A9 2A */    LDA #$2a
/* 16D6A: 9D E0 06 */ STA ObjectType,X
/* 16D6D: A9 00 */    LDA #$00
/* 16D6F: 9D 40 04 */ STA ObjectUnknown440,X
L16D72:
/* 16D72: 60 */       RTS

AD73_table:
    .byte $60,$FF ;Yspeed & fraction
    .byte $A0,$00 ;-"-

AI_SinewaveFlier:
/* 16D77: BD 20 04 */ LDA ObjectFlags,X
/* 16D7A: 29 20 */    AND #$20
/* 16D7C: D0 37 */    BNE L16DB5
/* 16D7E: 20 A6 AE */ JSR CloneObject
/* 16D81: B0 32 */    BCS L16DB5
/* 16D83: A0 00 */    LDY #$00
/* 16D85: BD 40 06 */ LDA ObjectFireDelay,X
/* 16D88: C9 40 */    CMP #$40
/* 16D8A: 90 0E */    BCC L16D9A ; ++                                              ; $AD9A
/* 16D8C: C9 80 */    CMP #$80
/* 16D8E: 90 08 */    BCC L16D98 ; +                                               ; $AD98
/* 16D90: A9 00 */    LDA #$00
/* 16D92: 9D 40 06 */ STA ObjectFireDelay,X
/* 16D95: A8 */       TAY
/* 16D96: F0 02 */    BEQ L16D9A ; ++                                              ; $AD9A
L16D98: ; +
/* 16D98: A0 02 */    LDY #$02
L16D9A: ; ++
/* 16D9A: 18 */       CLC
/* 16D9B: BD 60 06 */ LDA ObjectYSpeedFraction,X
/* 16D9E: 79 B1 AD */ ADC ADB1_table+0,Y
/* 16DA1: 9D 60 06 */ STA ObjectYSpeedFraction,X
/* 16DA4: BD 80 06 */ LDA ObjectYSpeed,X
/* 16DA7: 79 B2 AD */ ADC ADB1_table+1,Y
/* 16DAA: 9D 80 06 */ STA ObjectYSpeed,X
/* 16DAD: FE 40 06 */ INC ObjectFireDelay,X
/* 16DB0: 60 */       RTS
ADB1_table:
    .byte $10,$00  ;yspeed & yspeed fraction
    .byte $F0,$FF  ;-"-
L16DB5:
/* 16DB5: 20 D4 AD */ JSR F16DD4
/* 16DB8: A0 10 */    LDY #$10
/* 16DBA: BD E0 06 */ LDA ObjectType,X
/* 16DBD: C9 07 */    CMP #$07
/* 16DBF: F0 02 */    BEQ L16DC3
/* 16DC1: A0 16 */    LDY #$16
L16DC3:
/* 16DC3: 38 */       SEC
/* 16DC4: 84 0C */    STY $0C
/* 16DC6: AD 00 06 */ LDA ObjectPosY+0
/* 16DC9: E5 0C */    SBC $0C
/* 16DCB: 9D 00 06 */ STA ObjectPosY,X
/* 16DCE: A0 40 */    LDY #$40
/* 16DD0: 20 33 F5 */ JSR InitObjectDefaultSpeed
/* 16DD3: 60 */       RTS



F16DD4:
/* 16DD4: BD 20 04 */ LDA ObjectFlags,X
/* 16DD7: 29 08 */    AND #$08
/* 16DD9: D0 2E */    BNE L16E09 ; ++                                              ; $AE09
/* 16DDB: DE 40 06 */ DEC ObjectFireDelay,X
/* 16DDE: D0 15 */    BNE L16DF5 ; +                                               ; $ADF5
/* 16DE0: 18 */       CLC
/* 16DE1: A9 06 */    LDA #$06
/* 16DE3: 9D 20 04 */ STA ObjectFlags,X
/* 16DE6: A5 1A */    LDA ScrollPosX
/* 16DE8: 69 F8 */    ADC #$f8
/* 16DEA: 9D 80 04 */ STA ObjectPosX,X
/* 16DED: A5 1B */    LDA ScrollPosScreen
/* 16DEF: 69 00 */    ADC #$00
/* 16DF1: 9D 60 04 */ STA ObjectPosScreen,X
/* 16DF4: 60 */       RTS

L16DF5: ; +
/* 16DF5: A9 D8 */    LDA #$d8             ;Create this object below Rockman at Y = #$D8
/* 16DF7: 9D 00 06 */ STA ObjectPosY,X
/* 16DFA: AD 80 04 */ LDA ObjectPosX+0
/* 16DFD: 9D 80 04 */ STA ObjectPosX,X
/* 16E00: AD 60 04 */ LDA ObjectPosScreen+0                             ; $0460
/* 16E03: 9D 60 04 */ STA ObjectPosScreen,X
/* 16E06: 68 */       PLA
/* 16E07: 68 */       PLA
/* 16E08: 60 */       RTS

L16E09: ; ++
/* 16E09: A9 F8 */    LDA #$f8
/* 16E0B: 9D 00 06 */ STA ObjectPosY,X
/* 16E0E: 68 */       PLA
/* 16E0F: 68 */       PLA
/* 16E10: 60 */       RTS

AI_FlyingShell:
/* 16E11: 20 A5 B3 */ JSR F173A5
/* 16E14: BD 20 04 */ LDA ObjectFlags,X
/* 16E17: 29 20 */    AND #$20
/* 16E19: D0 0B */    BNE L16E26
/* 16E1B: FE 40 06 */ INC ObjectFireDelay,X
/* 16E1E: 20 A3 AF */ JSR F16FA3
/* 16E21: 20 A6 AE */ JSR CloneObject
/* 16E24: 90 1B */    BCC L16E41
L16E26:
/* 16E26: 20 D4 AD */ JSR F16DD4

/* 16E29: A9 80 */    LDA #$80
/* 16E2B: 9D 00 06 */ STA ObjectPosY,X

/* 16E2E: A9 20 */    LDA #$20
/* 16E30: 9D E0 04 */ STA ObjectXSpeedFraction,X
/* 16E33: A9 01 */    LDA #$01
/* 16E35: 9D C0 04 */ STA ObjectXSpeed,X
/* 16E38: A9 00 */    LDA #$00
/* 16E3A: 9D 60 06 */ STA ObjectYSpeedFraction,X
/* 16E3D: 9D 80 06 */ STA ObjectYSpeed,X
/* 16E40: 60 */       RTS

L16E41:
/* 16E41: BD 40 06 */ LDA ObjectFireDelay,X
/* 16E44: C9 38 */    CMP #$38
/* 16E46: 90 55 */    BCC L16E9D
/* 16E48: D0 0C */    BNE L16E56
/* 16E4A: A9 00 */    LDA #$00
/* 16E4C: 9D C0 04 */ STA ObjectXSpeed,X
/* 16E4F: 9D E0 04 */ STA ObjectXSpeedFraction,X
/* 16E52: A9 10 */    LDA #$10
/* 16E54: D0 4C */    BNE L16EA2
L16E56:
/* 16E56: C9 48 */    CMP #$48
/* 16E58: 90 43 */    BCC L16E9D
/* 16E5A: D0 2C */    BNE L16E88
/* 16E5C: BD A0 06 */ LDA ObjectLifeCycleCounter,X
/* 16E5F: D0 26 */    BNE L16E87
/* 16E61: A9 1C */    LDA #$1c
/* 16E63: 85 0D */    STA $0D
L16E65:
/* 16E65: A9 2D */    LDA #$2d
/* 16E67: 20 63 F6 */ JSR CreateEnemy                                       ; $F663
/* 16E6A: B0 1B */    BCS L16E87
/* 16E6C: A4 0D */    LDY $0D
/* 16E6E: 20 33 F5 */ JSR InitObjectDefaultSpeed
/* 16E71: 98 */       TYA
/* 16E72: 18 */       CLC
/* 16E73: 69 10 */    ADC #$10
/* 16E75: 29 40 */    AND #$40
/* 16E77: 5D 20 04 */ EOR ObjectFlags,X
/* 16E7A: 9D 20 04 */ STA ObjectFlags,X
/* 16E7D: C8 */       INY
/* 16E7E: C8 */       INY
/* 16E7F: C8 */       INY
/* 16E80: C8 */       INY
/* 16E81: 84 0D */    STY $0D
/* 16E83: C0 3C */    CPY #$3c
/* 16E85: 90 DE */    BCC L16E65
L16E87:
/* 16E87: 60 */       RTS


L16E88:
/* 16E88: C9 58 */    CMP #$58
/* 16E8A: 90 11 */    BCC L16E9D
/* 16E8C: A9 20 */    LDA #$20
/* 16E8E: 9D E0 04 */ STA ObjectXSpeedFraction,X
/* 16E91: A9 01 */    LDA #$01
/* 16E93: 9D C0 04 */ STA ObjectXSpeed,X
/* 16E96: A9 00 */    LDA #$00
/* 16E98: 9D 40 06 */ STA ObjectFireDelay,X
/* 16E9B: F0 05 */    BEQ L16EA2
L16E9D:
/* 16E9D: BD 40 04 */ LDA ObjectUnknown440,X
/* 16EA0: 29 F0 */    AND #$f0
L16EA2:
/* 16EA2: 9D 40 04 */ STA ObjectUnknown440,X
/* 16EA5: 60 */       RTS


CloneObject:
; Creates a small explosion, big explosion, or hamburger's shots
;
/* 16EA6: B5 6B */    LDA Meters+1,X
/* 16EA8: 48 */       PHA
/* 16EA9: BD E0 06 */  LDA ObjectType,X
/* 16EAC: 48 */        PHA
/* 16EAD: 20 49 BE */   JSR EnemyAI_MovementsAndDamageCheck
/* 16EB0: BD 20 04 */   LDA ObjectFlags,X
/* 16EB3: 29 08 */      AND #$08
/* 16EB5: D0 37 */      BNE L16EEE
/* 16EB7: B0 1D */      BCS L16ED6
/* 16EB9: BD E0 06 */   LDA ObjectType,X
/* 16EBC: C9 1B */      CMP #$1b
/* 16EBE: D0 2E */      BNE L16EEE
/* 16EC0: 68 */        PLA
/* 16EC1: 48 */        PHA
/* 16EC2: C9 08 */      CMP #$08
/* 16EC4: F0 31 */      BEQ L16EF7               ; Create explosion, then resume next line
L16EC6:
/* 16EC6: A9 00 */      LDA #$00
/* 16EC8: 20 63 F6 */   JSR CreateEnemy                                       ; $F663
/* 16ECB: B0 25 */      BCS L16EF2
/* 16ECD: A9 20 */      LDA #$20
/* 16ECF: 9D 20 04 */   STA ObjectFlags,X
/* 16ED2: A9 48 */      LDA #$48
/* 16ED4: D0 02 */      BNE L16ED8
L16ED6:
/* 16ED6: A9 01 */      LDA #$01
L16ED8:
/* 16ED8: 9D 40 06 */   STA ObjectFireDelay,X
/* 16EDB: A9 00 */      LDA #$00
/* 16EDD: 9D A0 06 */   STA ObjectLifeCycleCounter,X
/* 16EE0: 68 */        PLA
/* 16EE1: 9D E0 06 */  STA ObjectType,X
/* 16EE4: 68 */       PLA
/* 16EE5: 95 6B */    STA Meters+1,X
/* 16EE7: A9 14 */    LDA #$14
/* 16EE9: 9D C0 06 */ STA ObjectLifeMeter,X
/* 16EEC: 38 */       SEC
/* 16EED: 60 */       RTS
L16EEE:
/* 16EEE: 68 */       PLA
/* 16EEF: 68 */       PLA
/* 16EF0: 18 */       CLC
/* 16EF1: 60 */       RTS
L16EF2:
/* 16EF2: 68 */       PLA
/* 16EF3: 68 */       PLA
/* 16EF4: 68 */       PLA
/* 16EF5: 68 */       PLA
/* 16EF6: 60 */       RTS
L16EF7:
/* 16EF7: 20 FD AE */ JSR ReplaceObjectWithExplosion
/* 16EFA: 4C C6 AE */ JMP L16EC6

ReplaceObjectWithExplosion:
/* 16EFD: A9 0B */    LDA #$0b
/* 16EFF: 85 0C */    STA $0C
/* 16F01: A9 FF */    LDA #$ff
/* 16F03: 85 0D */    STA $0D
/* 16F05: A9 02 */    LDA #$02
/* 16F07: 85 0E */    STA $0E
/* 16F09: D0 07 */    BNE L16F12
L16F0B: ; -
/* 16F0B: A2 10 */    LDX #$10
/* 16F0D: 20 76 C5 */ JSR FindFreeObject
/* 16F10: B0 0E */    BCS L16F20 ; +
L16F12:
/* 16F12: A4 2F */    LDY RefObjectNum
/* 16F14: 20 41 F8 */ JSR CreateExplosionObject
/* 16F17: A9 28 */    LDA #$28
/* 16F19: 9D E0 06 */ STA ObjectType,X
/* 16F1C: A5 0C */    LDA $0C
/* 16F1E: 10 EB */    BPL L16F0B ; -                                               ; $AF0B
L16F20: ; +
/* 16F20: A9 12 */    LDA #$12        ; Explosion
/* 16F22: 20 77 C4 */ JSR IssueSound                                      ; $C477
/* 16F25: 60 */       RTS

AI_Beak:
/* 16F26: 20 A5 B3 */ JSR F173A5
/* 16F29: 20 A3 AF */ JSR F16FA3
/* 16F2C: 20 49 BE */ JSR EnemyAI_MovementsAndDamageCheck
/* 16F2F: FE 40 06 */ INC ObjectFireDelay,X
/* 16F32: BD 40 04 */ LDA ObjectUnknown440,X
/* 16F35: C9 40 */    CMP #$40
/* 16F37: B0 51 */    BCS L16F8A
/* 16F39: C9 30 */    CMP #$30
/* 16F3B: B0 09 */    BCS L16F46
/* 16F3D: BD 40 06 */ LDA ObjectFireDelay,X
/* 16F40: C9 4D */    CMP #$4d
/* 16F42: 90 59 */    BCC L16F9D
/* 16F44: B0 5C */    BCS L16FA2
L16F46:
/* 16F46: D0 05 */    BNE L16F4D
/* 16F48: A9 00 */    LDA #$00
/* 16F4A: 9D 40 06 */ STA ObjectFireDelay,X
L16F4D:
/* 16F4D: BD 40 06 */ LDA ObjectFireDelay,X
/* 16F50: 85 0C */    STA $0C
/* 16F52: 29 1F */    AND #$1f
/* 16F54: D0 2E */    BNE L16F84
/* 16F56: A9 2D */    LDA #$2d
/* 16F58: 20 63 F6 */ JSR CreateEnemy                                       ; $F663
/* 16F5B: B0 1E */    BCS L16F7B
/* 16F5D: A0 F9 */    LDY #$f9
/* 16F5F: BD 20 04 */ LDA ObjectFlags,X
/* 16F62: 29 40 */    AND #$40
/* 16F64: F0 02 */    BEQ L16F68
/* 16F66: A0 07 */    LDY #$07

L16F68:
/* 16F68: 18 */       CLC
/* 16F69: 98 */       TYA
/* 16F6A: 7D 80 04 */ ADC ObjectPosX,X
/* 16F6D: 9D 80 04 */ STA ObjectPosX,X
/* 16F70: A5 0C */    LDA $0C
/* 16F72: 4A */       LSR A
/* 16F73: 4A */       LSR A
/* 16F74: 4A */       LSR A
/* 16F75: 69 0C */    ADC #$0c
/* 16F77: A8 */       TAY
/* 16F78: 20 33 F5 */ JSR InitObjectDefaultSpeed
L16F7B:
/* 16F7B: A6 2F */    LDX RefObjectNum
/* 16F7D: BD 40 06 */ LDA ObjectFireDelay,X
/* 16F80: C9 60 */    CMP #$60
/* 16F82: F0 13 */    BEQ L16F97
L16F84:
/* 16F84: A9 31 */    LDA #$31
/* 16F86: 9D 40 04 */ STA ObjectUnknown440,X
/* 16F89: 60 */       RTS



L16F8A:
/* 16F8A: C9 60 */    CMP #$60
/* 16F8C: 90 14 */    BCC L16FA2
/* 16F8E: A9 00 */    LDA #$00
/* 16F90: 9D 40 04 */ STA ObjectUnknown440,X
/* 16F93: 9D 40 06 */ STA ObjectFireDelay,X
/* 16F96: 60 */       RTS



L16F97:
/* 16F97: A9 40 */    LDA #$40
/* 16F99: 9D 40 04 */ STA ObjectUnknown440,X
/* 16F9C: 60 */       RTS

L16F9D:
/* 16F9D: A9 00 */    LDA #$00
/* 16F9F: 9D 40 04 */ STA ObjectUnknown440,X
L16FA2:
/* 16FA2: 60 */       RTS

F16FA3:
/* 16FA3: BD 40 04 */ LDA ObjectUnknown440,X
/* 16FA6: 29 F0 */    AND #$f0
/* 16FA8: D0 0A */    BNE L16FB4
/* 16FAA: BD 20 04 */ LDA ObjectFlags,X
/* 16FAD: 29 80 */    AND #$80
/* 16FAF: F0 03 */    BEQ L16FB4
/* 16FB1: 20 52 BF */ JSR F17F52
L16FB4:
/* 16FB4: 60 */       RTS


AI_Bombomb:
/* 16FB5: BD 40 06 */ LDA ObjectFireDelay,X
/* 16FB8: D0 4F */    BNE L17009
/* 16FBA: 20 49 BE */ JSR EnemyAI_MovementsAndDamageCheck
/* 16FBD: BD 80 06 */ LDA ObjectYSpeed,X
/* 16FC0: 10 46 */    BPL L17008

/* 16FC2: A9 1B */    LDA #$1b
/* 16FC4: 20 63 F6 */ JSR CreateEnemy                                       ; $F663
/* 16FC7: B0 06 */    BCS L16FCF ; +                                               ; $AFCF
/* 16FC9: FE 40 04 */ INC ObjectUnknown440,X
/* 16FCC: 20 45 B0 */ JSR ClearObjectMem                                  ; $B045
L16FCF: ; +
/* 16FCF: A6 2F */    LDX RefObjectNum
/* 16FD1: A9 03 */    LDA #$03
/* 16FD3: 85 0C */    STA $0C
L16FD5:
/* 16FD5: A9 26 */    LDA #$26
/* 16FD7: 20 63 F6 */ JSR CreateEnemy                                       ; $F663
/* 16FDA: 20 45 B0 */ JSR ClearObjectMem                                  ; $B045
/* 16FDD: A5 0C */    LDA $0C
/* 16FDF: 29 0E */    AND #$0e
/* 16FE1: 0A */       ASL A
/* 16FE2: 69 44 */    ADC #$44
/* 16FE4: A8 */       TAY
/* 16FE5: 20 33 F5 */ JSR InitObjectDefaultSpeed
/* 16FE8: A5 0C */    LDA $0C
/* 16FEA: 29 01 */    AND #$01
/* 16FEC: A8 */       TAY
/* 16FED: B9 43 B0 */ LDA L17043,Y
/* 16FF0: 9D 20 04 */ STA ObjectFlags,X
/* 16FF3: C6 0C */    DEC $0C
/* 16FF5: 10 DE */    BPL L16FD5
/* 16FF7: A6 2F */    LDX RefObjectNum
/* 16FF9: A9 BB */    LDA #$bb
/* 16FFB: 9D 40 06 */ STA ObjectFireDelay,X
/* 16FFE: A9 22 */    LDA #$22
/* 17000: 9D 20 04 */ STA ObjectFlags,X
/* 17003: A9 D8 */    LDA #$d8
/* 17005: 9D 00 06 */ STA ObjectPosY,X
L17008:
/* 17008: 60 */       RTS
L17009:
/* 17009: DE 40 06 */ DEC ObjectFireDelay,X
/* 1700C: D0 28 */    BNE L17036
/* 1700E: A9 0D */    LDA #$0d
/* 17010: 85 0C */    STA $0C
/* 17012: A2 10 */    LDX #$10
L17014:
/* 17014: 20 16 F5 */ JSR FindObjectOfSelectedType
/* 17017: B0 0C */    BCS L17025
/* 17019: BD 20 04 */ LDA ObjectFlags,X
/* 1701C: 29 20 */    AND #$20
/* 1701E: F0 1A */    BEQ L1703A
/* 17020: E8 */       INX
/* 17021: E0 20 */    CPX #$20
/* 17023: D0 EF */    BNE L17014
L17025:
/* 17025: A6 2F */    LDX RefObjectNum
/* 17027: A9 13 */    LDA #$13
/* 17029: 9D 20 04 */ STA ObjectFlags,X
/* 1702C: A9 F1 */    LDA #$f1
/* 1702E: 9D 60 06 */ STA ObjectYSpeedFraction,X
/* 17031: A9 08 */    LDA #$08
/* 17033: 9D 80 06 */ STA ObjectYSpeed,X
L17036:
/* 17036: 20 49 BE */ JSR EnemyAI_MovementsAndDamageCheck
/* 17039: 60 */       RTS
L1703A:
/* 1703A: A6 2F */    LDX RefObjectNum
/* 1703C: FE 40 06 */ INC ObjectFireDelay,X
/* 1703F: 20 49 BE */ JSR EnemyAI_MovementsAndDamageCheck
/* 17042: 60 */       RTS

L17043:
/* 17043: 13 */       .byte $13
/* 17044: 53 */       .byte $53


ClearObjectMem:
/* 17045: A9 00 */    LDA #$00
/* 17047: 9D 60 06 */ STA ObjectYSpeedFraction,X
/* 1704A: 9D 80 06 */ STA ObjectYSpeed,X
/* 1704D: 9D E0 04 */ STA ObjectXSpeedFraction,X
/* 17050: 9D C0 04 */ STA ObjectXSpeed,X
/* 17053: 60 */       RTS


AI_SniperJoe:
/* 17054: BD 40 04 */ LDA ObjectUnknown440,X
/* 17057: 29 F0 */    AND #$f0
/* 17059: C9 30 */    CMP #$30
/* 1705B: D0 15 */    BNE L17072
/* 1705D: BD 20 04 */ LDA ObjectFlags,X
/* 17060: 29 80 */    AND #$80
/* 17062: F0 08 */    BEQ L1706C
/* 17064: A9 20 */    LDA #$20
/* 17066: 9D 40 06 */ STA ObjectFireDelay,X
/* 17069: 20 52 BF */ JSR F17F52
L1706C:
/* 1706C: 20 E0 B0 */ JSR F170E0
/* 1706F: 4C 92 B0 */ JMP L17092
L17072:
/* 17072: C9 00 */    CMP #$00
/* 17074: F0 25 */    BEQ L1709B
/* 17076: C9 20 */    CMP #$20
/* 17078: 08 */       PHP
/* 17079: 20 E0 B0 */ JSR F170E0
/* 1707C: 28 */       PLP
/* 1707D: D0 5A */    BNE L170D9
/* 1707F: BD 40 06 */ LDA ObjectFireDelay,X
/* 17082: C9 10 */    CMP #$10
/* 17084: D0 0C */    BNE L17092
/* 17086: A9 1A */    LDA #$1a
/* 17088: 20 63 F6 */ JSR CreateEnemy                                       ; $F663
/* 1708B: B0 05 */    BCS L17092
/* 1708D: A0 24 */    LDY #$24
/* 1708F: 20 33 F5 */ JSR InitObjectDefaultSpeed
L17092:
/* 17092: A6 2F */    LDX RefObjectNum
/* 17094: DE 40 06 */ DEC ObjectFireDelay,X
/* 17097: D0 38 */    BNE L170D1
/* 17099: F0 0F */    BEQ L170AA
L1709B:
/* 1709B: 20 49 BE */ JSR EnemyAI_MovementsAndDamageCheck
/* 1709E: A5 2B */    LDA $2B
/* 170A0: F0 2F */    BEQ L170D1
/* 170A2: A9 00 */    LDA #$00
/* 170A4: 9D C0 04 */ STA ObjectXSpeed,X
/* 170A7: 9D E0 04 */ STA ObjectXSpeedFraction,X
L170AA:
/* 170AA: A9 06 */    LDA #$06
/* 170AC: 20 A0 C5 */ JSR RandomFunc
/* 170AF: A6 2F */    LDX RefObjectNum
/* 170B1: A0 00 */    LDY #$00
/* 170B3: 4A */       LSR A
/* 170B4: B0 0F */    BCS L170C5
/* 170B6: C8 */       INY
/* 170B7: 4A */       LSR A
/* 170B8: 90 0B */    BCC L170C5
/* 170BA: C8 */       INY
/* 170BB: A9 CC */    LDA #$cc
/* 170BD: 9D 60 06 */ STA ObjectYSpeedFraction,X
/* 170C0: A9 04 */    LDA #$04
/* 170C2: 9D 80 06 */ STA ObjectYSpeed,X
L170C5:
/* 170C5: B9 DA B0 */ LDA L170DA,Y
/* 170C8: 9D 40 04 */ STA ObjectUnknown440,X
/* 170CB: B9 DD B0 */ LDA L170DD,Y
/* 170CE: 9D 40 06 */ STA ObjectFireDelay,X
L170D1:
/* 170D1: BD 40 04 */ LDA ObjectUnknown440,X
/* 170D4: 29 F0 */    AND #$f0
/* 170D6: 9D 40 04 */ STA ObjectUnknown440,X
L170D9:
/* 170D9: 60 */       RTS


L170DA:
	.byte $30,$10,$00

L170DD:
/* 170DD: 20 20 20 */ JSR $2020

F170E0:
/* 170E0: 20 3B F6 */ JSR EnemySearchMegaman
/* 170E3: 18 */       CLC
/* 170E4: 69 38 */    ADC #$38
/* 170E6: 85 0C */    STA $0C
/* 170E8: BD 20 04 */ LDA ObjectFlags,X
/* 170EB: 29 40 */    AND #$40
/* 170ED: F0 24 */    BEQ L17113
/* 170EF: A9 00 */    LDA #$00
/* 170F1: 9D 40 04 */ STA ObjectUnknown440,X
/* 170F4: A9 85 */    LDA #$85
/* 170F6: 9D 60 06 */ STA ObjectYSpeedFraction,X
/* 170F9: A9 05 */    LDA #$05
/* 170FB: 9D 80 06 */ STA ObjectYSpeed,X
/* 170FE: A6 0C */    LDX $0C
/* 17100: A0 00 */    LDY #$00
/* 17102: A9 2A */    LDA #$2a
/* 17104: 20 AC C5 */ JSR EnemyCalculateJumpCurveToHitMegaman
/* 17107: A6 2F */    LDX RefObjectNum
/* 17109: A5 04 */    LDA $04
/* 1710B: 9D E0 04 */ STA ObjectXSpeedFraction,X
/* 1710E: A5 05 */    LDA $05
/* 17110: 9D C0 04 */ STA ObjectXSpeed,X
L17113:
/* 17113: 20 49 BE */ JSR EnemyAI_MovementsAndDamageCheck
/* 17116: 60 */       RTS


AI_FootHolder:
/* 17117: BD 40 06 */ LDA ObjectFireDelay,X
/* 1711A: 29 3F */    AND #$3f
/* 1711C: D0 78 */    BNE L17196 ; +++
/* 1711E: A9 03 */    LDA #$03
/* 17120: 20 A0 C5 */ JSR RandomFunc
/* 17123: A6 2F */    LDX RefObjectNum
/* 17125: 85 0C */    STA $0C
/* 17127: BD 00 06 */ LDA ObjectPosY,X
/* 1712A: C9 58 */    CMP #$58
/* 1712C: B0 0B */    BCS L17139 ; +
/* 1712E: A5 0C */    LDA $0C
/* 17130: C9 02 */    CMP #$02
/* 17132: F0 11 */    BEQ L17145 ; ++
/* 17134: 06 0C */    ASL $0C
/* 17136: 4C 45 B1 */ JMP L17145 ; ++
L17139: ; +
/* 17139: C9 C0 */    CMP #$c0
/* 1713B: 90 08 */    BCC L17145 ; ++
/* 1713D: A5 0C */    LDA $0C
/* 1713F: C9 01 */    CMP #$01
/* 17141: F0 02 */    BEQ L17145 ; ++
/* 17143: 46 0C */    LSR $0C
L17145: ; ++
/* 17145: A4 0C */    LDY $0C
/* 17147: B9 A2 B1 */ LDA L171A2,Y
/* 1714A: 9D 80 06 */ STA ObjectYSpeed,X
/* 1714D: B9 A5 B1 */ LDA L171A5,Y
/* 17150: 9D 60 06 */ STA ObjectYSpeedFraction,X
/* 17153: BD 40 06 */ LDA ObjectFireDelay,X
/* 17156: D0 3E */    BNE L17196 ; +++
/* 17158: A9 80 */    LDA #$80
/* 1715A: 9D 40 06 */ STA ObjectFireDelay,X
/* 1715D: BD 20 04 */ LDA ObjectFlags,X
/* 17160: 49 40 */    EOR #$40
/* 17162: 9D 20 04 */ STA ObjectFlags,X
/* 17165: A9 01 */    LDA #$01
/* 17167: 85 0C */    STA $0C
L17169: ; -
/* 17169: A9 2D */     LDA #$2d
/* 1716B: 20 63 F6 */  JSR CreateEnemy                                       ; $F663
/* 1716E: B0 24 */     BCS L17194 ; +
/* 17170: A4 0C */     LDY $0C
/* 17172: B9 A8 B1 */  LDA L171A8,Y
/* 17175: 9D 20 04 */  STA ObjectFlags,X
/* 17178: 18 */        CLC
/* 17179: BD 80 04 */  LDA ObjectPosX,X
/* 1717C: 79 AA B1 */  ADC L171AA,Y
/* 1717F: 9D 80 04 */  STA ObjectPosX,X
/* 17182: BD 60 04 */  LDA ObjectPosScreen,X
/* 17185: 79 AC B1 */  ADC L171AC,Y
/* 17188: 9D 60 04 */  STA ObjectPosScreen,X
/* 1718B: A0 D0 */     LDY #$d0
/* 1718D: 20 33 F5 */  JSR InitObjectDefaultSpeed
/* 17190: C6 0C */    DEC $0C
/* 17192: 10 D5 */    BPL L17169 ; -
L17194: ; +
/* 17194: A6 2F */    LDX RefObjectNum
L17196: ; +++
/* 17196: DE 40 06 */ DEC ObjectFireDelay,X
/* 17199: 20 49 BE */ JSR EnemyAI_MovementsAndDamageCheck
/* 1719C: B0 03 */    BCS L171A1 ; +
/* 1719E: 20 AE B1 */ JSR StoreLiftData
L171A1: ; +
/* 171A1: 60 */       RTS

L171A2:
    .byte $00,$00,$FF
L171A5:
    .byte $00,$60,$A0
L171A8:
    .byte $02,$42
L171AA:
    .byte $0C,$F4
L171AC:
    .byte $00,$FF

StoreLiftData:
;
; Input:
;   LiftIndex = index into $0700
; Output:
;   LiftPosScreen,Y <- screen
;   LiftPosX,Y      <- Xpos
;   LiftPosY,Y      <- Ypos-8
;   LiftDirection,Y <- Direction (zero=left, nonzero=right)
;   LiftXSpeedFraction <- XSpeedFraction
;   LiftXSpeed <- XSpeed
;   LiftIndex++
;
; Lift width is always assumed to be 20 pix (this constant is at $9673)
;
/* 171AE: A4 9A */    LDY LiftIndex
/* 171B0: BD 60 04 */ LDA ObjectPosScreen,X
/* 171B3: 99 00 07 */ STA LiftPosScreen,Y
/* 171B6: BD 80 04 */ LDA ObjectPosX,X
/* 171B9: 99 08 07 */ STA LiftPosX,Y
/* 171BC: 38 */       SEC
/* 171BD: BD 00 06 */ LDA ObjectPosY,X
/* 171C0: E9 08 */    SBC #$08
/* 171C2: 99 10 07 */ STA LiftPosY,Y
/* 171C5: BD E0 04 */ LDA ObjectXSpeedFraction,X
/* 171C8: 85 9C */    STA LiftXSpeedFraction
/* 171CA: BD C0 04 */ LDA ObjectXSpeed,X
/* 171CD: 85 9D */    STA LiftXSpeed
/* 171CF: BD 20 04 */ LDA ObjectFlags,X
/* 171D2: 29 40 */    AND #$40
/* 171D4: 99 18 07 */ STA LiftDirection,Y
/* 171D7: E6 9A */    INC LiftIndex
/* 171D9: 60 */       RTS

AI_HeadLosingRobot:
/* 171DA: 20 3B F6 */ JSR EnemySearchMegaman
/* 171DD: A9 1E */    LDA #$1e
/* 171DF: 9D E0 06 */ STA ObjectType,X       ;Object1E
/* 171E2: 18 */       CLC
/* 171E3: BD 00 06 */ LDA ObjectPosY,X
/* 171E6: 69 04 */    ADC #$04
/* 171E8: 9D 00 06 */ STA ObjectPosY,X
/* 171EB: A9 1D */    LDA #$1d
/* 171ED: 20 63 F6 */ JSR CreateEnemy                                       ; $F663
/* 171F0: B0 3E */    BCS L17230
/* 171F2: A4 2F */    LDY RefObjectNum
/* 171F4: B9 6B 00 */ LDA Meters+1,Y
/* 171F7: 95 6B */    STA Meters+1,X
/* 171F9: BD 20 04 */ LDA ObjectFlags,X
/* 171FC: 09 04 */    ORA #$04
/* 171FE: 9D 20 04 */ STA ObjectFlags,X
/* 17201: 38 */       SEC
/* 17202: BD 00 06 */ LDA ObjectPosY,X
/* 17205: E9 10 */    SBC #$10
/* 17207: 9D 00 06 */ STA ObjectPosY,X
/* 1720A: 20 45 B0 */ JSR ClearObjectMem                                  ; $B045
/* 1720D: A4 2F */    LDY RefObjectNum
/* 1720F: B9 20 04 */ LDA ObjectFlags,Y
/* 17212: 29 40 */    AND #$40
/* 17214: F0 19 */    BEQ L1722F
/* 17216: A9 29 */    LDA #$29          ;Object29
/* 17218: 99 E0 06 */ STA ObjectType,Y
/* 1721B: A9 1F */    LDA #$1f
/* 1721D: 99 40 06 */ STA ObjectFireDelay,Y
/* 17220: A9 1D */    LDA #$1d
/* 17222: 9D E0 06 */ STA ObjectType,X
/* 17225: A9 09 */    LDA #$09
/* 17227: 9D 40 06 */ STA ObjectFireDelay,X
/* 1722A: A9 04 */    LDA #$04
/* 1722C: 9D 80 06 */ STA ObjectYSpeed,X
L1722F:
/* 1722F: 60 */       RTS
L17230:
/* 17230: A6 2F */    LDX RefObjectNum
/* 17232: A9 F8 */    LDA #$f8
/* 17234: 9D 00 06 */ STA ObjectPosY,X
/* 17237: A9 FF */    LDA #$ff
/* 17239: 95 6B */    STA Meters+1,X
/* 1723B: 60 */       RTS


AI_PicketMan:
/* 1723C: 20 3B F6 */ JSR EnemySearchMegaman
/* 1723F: 85 0C */    STA $0C
/* 17241: BD 40 04 */ LDA ObjectUnknown440,X
/* 17244: 29 F0 */    AND #$f0
/* 17246: F0 58 */    BEQ L172A0
/* 17248: BD 40 04 */ LDA ObjectUnknown440,X
/* 1724B: C9 20 */    CMP #$20
/* 1724D: D0 32 */    BNE L17281
/* 1724F: A9 2E */    LDA #$2e
/* 17251: 20 63 F6 */ JSR CreateEnemy                                       ; $F663
/* 17254: B0 24 */    BCS L1727A
/* 17256: BD 20 04 */ LDA ObjectFlags,X
/* 17259: 09 10 */    ORA #$10
/* 1725B: 9D 20 04 */ STA ObjectFlags,X
/* 1725E: A0 70 */    LDY #$70
/* 17260: 20 33 F5 */ JSR InitObjectDefaultSpeed
/* 17263: 8A */       TXA
/* 17264: 48 */       PHA
/* 17265: A6 0C */    LDX $0C
/* 17267: A0 00 */    LDY #$00
/* 17269: A9 27 */    LDA #$27
/* 1726B: 20 AC C5 */ JSR EnemyCalculateJumpCurveToHitMegaman
/* 1726E: 68 */       PLA
/* 1726F: AA */       TAX
/* 17270: A5 04 */    LDA $04
/* 17272: 9D E0 04 */ STA ObjectXSpeedFraction,X
/* 17275: A5 05 */    LDA $05
/* 17277: 9D C0 04 */ STA ObjectXSpeed,X
L1727A:
/* 1727A: A6 2F */    LDX RefObjectNum
/* 1727C: DE 40 06 */ DEC ObjectFireDelay,X
/* 1727F: D0 37 */    BNE L172B8
L17281:
/* 17281: C9 30 */    CMP #$30
/* 17283: D0 33 */    BNE L172B8
/* 17285: BD 40 06 */ LDA ObjectFireDelay,X
/* 17288: D0 0F */    BNE L17299
/* 1728A: A5 46 */    LDA RandomSeed
/* 1728C: 29 01 */    AND #$01
/* 1728E: A8 */       TAY
/* 1728F: B9 C8 B2 */ LDA L172C8,Y
/* 17292: 9D 40 06 */ STA ObjectFireDelay,X
/* 17295: A9 00 */    LDA #$00
/* 17297: F0 07 */    BEQ L172A0
L17299:
/* 17299: A9 10 */    LDA #$10
/* 1729B: 9D 40 04 */ STA ObjectUnknown440,X
/* 1729E: D0 18 */    BNE L172B8
L172A0:
/* 172A0: 9D 40 04 */ STA ObjectUnknown440,X
/* 172A3: DE 40 06 */ DEC ObjectFireDelay,X
/* 172A6: D0 10 */    BNE L172B8
/* 172A8: A9 10 */    LDA #$10
/* 172AA: 9D 40 04 */ STA ObjectUnknown440,X
/* 172AD: A5 46 */    LDA RandomSeed
/* 172AF: 29 01 */    AND #$01
/* 172B1: A8 */       TAY
/* 172B2: B9 C6 B2 */ LDA L172C6,Y
/* 172B5: 9D 40 06 */ STA ObjectFireDelay,X
L172B8:
/* 172B8: 20 A3 AF */ JSR F16FA3
/* 172BB: 20 49 BE */ JSR EnemyAI_MovementsAndDamageCheck
/* 172BE: A5 2B */    LDA $2B
/* 172C0: F0 03 */    BEQ L172C5
/* 172C2: 20 45 B0 */ JSR ClearObjectMem                                  ; $B045
L172C5:
/* 172C5: 60 */       RTS

L172C6:
	.byte $05,$0A

L172C8:
	.byte $3F,$BB


AI_ScrewBomberFloor:
/* 172CA: A9 08 */    LDA #$08
/* 172CC: 85 0D */    STA $0D
/* 172CE: 85 0E */    STA $0E
/* 172D0: A9 04 */    LDA #$04
/* 172D2: 85 0F */    STA $0F
/* 172D4: D0 0C */    BNE L172E2 ; +                                               ; $B2E2


AI_ScrewBomberCeiling:
/* 172D6: A9 F8 */    LDA #$f8
/* 172D8: 85 0D */    STA $0D
/* 172DA: A9 09 */    LDA #$09
/* 172DC: 85 0E */    STA $0E
/* 172DE: A9 FC */    LDA #$fc
/* 172E0: 85 0F */    STA $0F
L172E2: ; +
/* 172E2: 20 A5 B3 */ JSR F173A5
/* 172E5: BD 40 04 */ LDA ObjectUnknown440,X
/* 172E8: F0 04 */    BEQ L172EE
/* 172EA: C9 01 */    CMP #$01
/* 172EC: D0 39 */    BNE L17327
L172EE:
/* 172EE: BD 40 06 */ LDA ObjectFireDelay,X
/* 172F1: F0 07 */    BEQ L172FA
/* 172F3: A9 00 */    LDA #$00
/* 172F5: 9D 40 04 */ STA ObjectUnknown440,X
/* 172F8: F0 7D */    BEQ L17377
L172FA:
/* 172FA: 20 3B F6 */ JSR EnemySearchMegaman
/* 172FD: 48 */       PHA
/* 172FE: BD 20 04 */ LDA ObjectFlags,X
/* 17301: 29 BF */    AND #$bf
/* 17303: 9D 20 04 */ STA ObjectFlags,X
/* 17306: 68 */       PLA
/* 17307: C9 50 */    CMP #$50
/* 17309: 90 07 */    BCC L17312
/* 1730B: A9 00 */    LDA #$00
/* 1730D: 9D 40 04 */ STA ObjectUnknown440,X
/* 17310: F0 78 */    BEQ L1738A
L17312:
/* 17312: A9 10 */    LDA #$10
/* 17314: 9D 40 04 */ STA ObjectUnknown440,X
/* 17317: 38 */       SEC
/* 17318: BD 00 06 */ LDA ObjectPosY,X
/* 1731B: E5 0D */    SBC $0D
/* 1731D: 9D 00 06 */ STA ObjectPosY,X
/* 17320: A9 60 */    LDA #$60
/* 17322: 9D 40 06 */ STA ObjectFireDelay,X
/* 17325: D0 50 */    BNE L17377
L17327:
/* 17327: C9 50 */    CMP #$50
/* 17329: D0 05 */    BNE L17330
/* 1732B: A9 20 */    LDA #$20
/* 1732D: 9D 40 04 */ STA ObjectUnknown440,X
L17330:
/* 17330: B0 45 */    BCS L17377
/* 17332: BD 40 06 */ LDA ObjectFireDelay,X
/* 17335: F0 2D */    BEQ L17364
/* 17337: 29 1F */    AND #$1f
/* 17339: D0 4C */    BNE L17387
L1733B:
/* 1733B: A9 2D */    LDA #$2d
/* 1733D: 20 63 F6 */ JSR CreateEnemy                                       ; $F663
/* 17340: B0 1E */    BCS L17360
/* 17342: 38 */       SEC
/* 17343: BD 00 06 */ LDA ObjectPosY,X
/* 17346: E5 0F */    SBC $0F
/* 17348: 9D 00 06 */ STA ObjectPosY,X
/* 1734B: A4 0E */    LDY $0E
/* 1734D: B9 9B B3 */ LDA B39B_table,Y
/* 17350: 9D 20 04 */ STA ObjectFlags,X
/* 17353: B9 91 B3 */ LDA B391_table,Y
/* 17356: A8 */       TAY
/* 17357: 20 33 F5 */ JSR InitObjectDefaultSpeed
/* 1735A: C6 0E */    DEC $0E
/* 1735C: C6 0E */    DEC $0E
/* 1735E: 10 DB */    BPL L1733B
L17360:
/* 17360: A6 2F */    LDX RefObjectNum
/* 17362: D0 23 */    BNE L17387
L17364:
/* 17364: A9 03 */    LDA #$03
/* 17366: 20 A0 C5 */ JSR RandomFunc
/* 17369: AA */       TAX
/* 1736A: BD 8E B3 */ LDA Table_B38E,X
/* 1736D: A6 2F */    LDX RefObjectNum
/* 1736F: 9D 40 06 */ STA ObjectFireDelay,X
/* 17372: A9 50 */    LDA #$50
/* 17374: 9D 40 04 */ STA ObjectUnknown440,X
L17377:
/* 17377: BD 40 04 */ LDA ObjectUnknown440,X
/* 1737A: C9 60 */    CMP #$60
/* 1737C: D0 09 */    BNE L17387
/* 1737E: 18 */       CLC
/* 1737F: BD 00 06 */ LDA ObjectPosY,X
/* 17382: 65 0D */    ADC $0D
/* 17384: 9D 00 06 */ STA ObjectPosY,X
L17387:
/* 17387: DE 40 06 */ DEC ObjectFireDelay,X
L1738A:
/* 1738A: 20 49 BE */ JSR EnemyAI_MovementsAndDamageCheck
/* 1738D: 60 */       RTS

Table_B38E: .byte $20, $3F, $7D

B391_table: .byte $60,$60,$5C,$64, $58,$68,$5C,$64, $60,$60
B39B_table: .byte $42,$42,$42,$42, $02,$02,$02,$02, $02,$02

F173A5:
/* 173A5: BD A0 06 */ LDA ObjectLifeCycleCounter,X
/* 173A8: F0 05 */    BEQ L173AF
/* 173AA: 20 49 BE */ JSR EnemyAI_MovementsAndDamageCheck
/* 173AD: 68 */       PLA
/* 173AE: 68 */       PLA
L173AF:
/* 173AF: 60 */       RTS


AI_Electricity:
/* 173B0: BD 20 04 */ LDA ObjectFlags,X
/* 173B3: 29 20 */    AND #$20
/* 173B5: D0 18 */    BNE L173CF ; ++                                              ; $B3CF
/* 173B7: DE 40 06 */ DEC ObjectFireDelay,X
/* 173BA: D0 0F */    BNE L173CB ; +                                               ; $B3CB
/* 173BC: BD 20 04 */ LDA ObjectFlags,X
/* 173BF: 09 20 */    ORA #$20
/* 173C1: 29 F0 */    AND #$f0
/* 173C3: 9D 20 04 */ STA ObjectFlags,X
/* 173C6: A9 50 */    LDA #$50
/* 173C8: 9D 40 06 */ STA ObjectFireDelay,X
; +
L173CB: ; -
/* 173CB: 20 49 BE */ JSR EnemyAI_MovementsAndDamageCheck
/* 173CE: 60 */       RTS

L173CF: ; ++
/* 173CF: DE 40 06 */ DEC ObjectFireDelay,X
/* 173D2: D0 F7 */    BNE L173CB ; -                                               ; $B3CB
/* 173D4: BD 20 04 */ LDA ObjectFlags,X
/* 173D7: 29 0F */    AND #$0f
/* 173D9: 09 02 */    ORA #$02
/* 173DB: 9D 20 04 */ STA ObjectFlags,X
/* 173DE: A9 3F */    LDA #$3f
/* 173E0: 9D 40 06 */ STA ObjectFireDelay,X
/* 173E3: 20 49 BE */ JSR EnemyAI_MovementsAndDamageCheck
/* 173E6: BD E0 06 */ LDA ObjectType,X
/* 173E9: 29 01 */    AND #$01
/* 173EB: D0 0B */    BNE L173F8 ; +                                               ; $B3F8
/* 173ED: A5 31 */    LDA CurrentStage
/* 173EF: C9 04 */    CMP #$04
/* 173F1: D0 05 */    BNE L173F8 ; +                                               ; $B3F8
/* 173F3: A9 13 */    LDA #$13        ; Electricity (at Elecman stage)
/* 173F5: 20 77 C4 */ JSR IssueSound                                      ; $C477
L173F8: ; +
/* 173F8: 60 */       RTS



AI_TemporaryBlockManager:
/* 173F9: BD 40 06 */ LDA ObjectFireDelay,X
/* 173FC: C9 BB */    CMP #$BB
/* 173FE: D0 05 */    BNE L17405 ; +
/* 17400: A9 00 */    LDA #$00
/* 17402: 9D C0 06 */ STA ObjectLifeMeter,X
L17405: ; +
/* 17405: DE 40 06 */ DEC ObjectFireDelay,X
/* 17408: D0 78 */    BNE L17482 ; +++
/* 1740A: A0 00 */    LDY #$00
/* 1740C: A5 31 */    LDA CurrentStage
/* 1740E: C9 01 */    CMP #$01          ;Iceman stage?
/* 17410: D0 0A */    BNE L1741C ; +
/* 17412: A5 1B */    LDA ScrollPosScreen
/* 17414: C9 0A */    CMP #$0A
/* 17416: F0 0E */    BEQ L17426 ; ++
/* 17418: A0 02 */    LDY #$02
/* 1741A: D0 0A */    BNE L17426 ; ++
L1741C: ; +
/* 1741C: A0 04 */    LDY #$04
/* 1741E: A5 1B */    LDA ScrollPosScreen
/* 17420: C9 05 */    CMP #$05
/* 17422: F0 02 */    BEQ L17426 ; ++
/* 17424: A0 06 */    LDY #$06
L17426: ; ++
/* 17426: B9 86 B4 */ LDA TemporaryBlockControlTable+0,Y
/* 17429: 85 04 */    STA $04
/* 1742B: B9 87 B4 */ LDA TemporaryBlockControlTable+1,Y
/* 1742E: 85 05 */    STA $05
/* 17430: BD C0 06 */ LDA ObjectLifeMeter,X     ;ObjectLifeMeter is the index
/* 17433: 0A */       ASL A                     ;into the block table.
/* 17434: A8 */       TAY
/* 17435: FE C0 06 */ INC ObjectLifeMeter,X
/* 17438: B1 04 */    LDA ($04),Y
/* 1743A: D0 00 */    BNE L1743C ; +
L1743C: ; +
/* 1743C: C9 FF */    CMP #$ff
/* 1743E: D0 07 */    BNE L17447 ; +
/* 17440: A9 00 */    LDA #$00                 ;Reset the loop
/* 17442: 9D C0 06 */ STA ObjectLifeMeter,X
/* 17445: A9 3F */    LDA #$3F
L17447: ; +
/* 17447: 9D 40 06 */ STA ObjectFireDelay,X
/* 1744A: C8 */       INY
/* 1744B: B1 04 */    LDA ($04),Y
/* 1744D: 85 0E */    STA $0E
/* 1744F: A9 01 */    LDA #$01
/* 17451: 85 0C */    STA $0C
/* 17453: A5 0E */    LDA $0E
/* 17455: 20 25 C6 */ JSR LoadActiveByIndexAndSetBlockingness
/* 17458: A9 27 */    LDA #$27         ;Create the temporary block
/* 1745A: 20 63 F6 */ JSR CreateEnemy                                       ; $F663
/* 1745D: B0 21 */    BCS L17480 ; +
/* 1745F: 20 45 B0 */  JSR ClearObjectMem                                  ; $B045
/* 17462: A9 00 */     LDA #$00
/* 17464: 9D 20 04 */  STA ObjectFlags,X
/* 17467: A9 70 */     LDA #$70
/* 17469: 9D 40 06 */  STA ObjectFireDelay,X
/* 1746C: A5 0C */     LDA $0C
/* 1746E: 9D 80 04 */  STA ObjectPosX,X
/* 17471: A5 0D */     LDA $0D
/* 17473: 9D 00 06 */  STA ObjectPosY,X
/* 17476: A5 0E */     LDA $0E
/* 17478: 9D C0 06 */  STA ObjectLifeMeter,X

/* 1747B: A9 2C */    LDA #$2C        ; Temporary block appearing
/* 1747D: 20 77 C4 */ JSR IssueSound                                      ; $C477
L17480: ; +
/* 17480: A6 2F */    LDX RefObjectNum
L17482: ; +++
/* 17482: 20 49 BE */ JSR EnemyAI_MovementsAndDamageCheck
/* 17485: 60 */       RTS

TemporaryBlockControlTable: ;At B486
    .word TemporaryBlockControl0 ; Iceman stage room 10
    .word TemporaryBlockControl1 ; Iceman stage other room
    .word TemporaryBlockControl2 ; Other stage room 5
    .word TemporaryBlockControl3 ; Other stage other room

; Format: delay, index into the active table (times 6).
TemporaryBlockControl0: ;Iceman stage room 10 (at B48E)
    .byte $3F,$12
    .byte $3F,$00
    .byte $3F,$1E
    .byte $3F,$24
    .byte $3F,$0C
    .byte $3F,$18
    .byte $FF,$06
TemporaryBlockControl1: ;Iceman stage other room (at B49C)
    .byte $01,$2A
    .byte $3F,$30
    .byte $01,$3C
    .byte $3F,$48
    .byte $01,$42
    .byte $3F,$54
    .byte $01,$60
    .byte $3F,$66
    .byte $3F,$5A
    .byte $3F,$4E
    .byte $FF,$36
TemporaryBlockControl2: ; Other stage room 5 (at B4B2)
    .byte $3F,$0C
    .byte $3F,$06
    .byte $3F,$12
    .byte $FF,$00
TemporaryBlockControl3: ; Other stage other room (at B4BA)
    .byte $3F,$2A
    .byte $3F,$24
    .byte $3F,$1E
    .byte $FF,$18

AI_TemporaryBlock:
/* 174C2: BD 40 04 */ LDA ObjectUnknown440,X
/* 174C5: 29 F0 */    AND #$f0
/* 174C7: C9 50 */    CMP #$50
/* 174C9: D0 03 */    BNE L174CE ; +                                               ; $B4CE
/* 174CB: 9D 40 04 */ STA ObjectUnknown440,X
L174CE: ; +
/* 174CE: DE 40 06 */ DEC ObjectFireDelay,X
/* 174D1: F0 06 */    BEQ L174D9 ; +                                               ; $B4D9
/* 174D3: 20 49 BE */ JSR EnemyAI_MovementsAndDamageCheck
/* 174D6: B0 01 */    BCS L174D9 ; +                                               ; $B4D9
/* 174D8: 60 */       RTS
L174D9: ; +
/* 174D9: A9 00 */    LDA #$00
/* 174DB: 85 0C */    STA $0C
/* 174DD: BD C0 06 */ LDA ObjectLifeMeter,X
/* 174E0: 20 25 C6 */ JSR LoadActiveByIndexAndSetBlockingness
/* 174E3: 4C B1 BE */ JMP RemoveObject                                    ; $BEB1


AI_ZigzagFire:
/* 174E6: BD 60 04 */ LDA ObjectPosScreen,X
/* 174E9: C9 0A */    CMP #$0a
/* 174EB: D0 05 */    BNE L174F2
/* 174ED: A9 06 */    LDA #$06
/* 174EF: 9D 40 06 */ STA ObjectFireDelay,X
L174F2:
/* 174F2: BD 40 06 */ LDA ObjectFireDelay,X
/* 174F5: 29 01 */    AND #$01
/* 174F7: F0 41 */    BEQ L1753A
/* 174F9: BD 40 04 */ LDA ObjectUnknown440,X
/* 174FC: D0 05 */    BNE L17503
/* 174FE: A9 30 */    LDA #$30
/* 17500: 9D 40 04 */ STA ObjectUnknown440,X
L17503:
/* 17503: BD 40 06 */ LDA ObjectFireDelay,X
/* 17506: A8 */       TAY
/* 17507: BD 20 04 */ LDA ObjectFlags,X
/* 1750A: 29 40 */    AND #$40
/* 1750C: F0 0A */    BEQ L17518
/* 1750E: B9 92 B5 */ LDA B592_table,Y
/* 17511: DD 80 04 */ CMP ObjectPosX,X
/* 17514: B0 56 */    BCS L1756C
/* 17516: 90 08 */    BCC L17520
L17518:
/* 17518: B9 92 B5 */ LDA B592_table,Y
/* 1751B: DD 80 04 */ CMP ObjectPosX,X
/* 1751E: 90 4C */    BCC L1756C
L17520:
/* 17520: 9D 80 04 */ STA ObjectPosX,X
/* 17523: A9 00 */    LDA #$00
/* 17525: 9D 40 04 */ STA ObjectUnknown440,X
/* 17528: BD 20 04 */ LDA ObjectFlags,X
/* 1752B: 09 10 */    ORA #$10
/* 1752D: 9D 20 04 */ STA ObjectFlags,X
/* 17530: A0 68 */    LDY #$68
/* 17532: 20 33 F5 */ JSR InitObjectDefaultSpeed
/* 17535: FE 40 06 */ INC ObjectFireDelay,X
/* 17538: D0 32 */    BNE L1756C
L1753A:
/* 1753A: BD 40 04 */ LDA ObjectUnknown440,X
/* 1753D: C9 30 */    CMP #$30
/* 1753F: D0 05 */    BNE L17546
/* 17541: A9 00 */    LDA #$00
/* 17543: 9D 40 04 */ STA ObjectUnknown440,X
L17546:
/* 17546: BD 40 06 */ LDA ObjectFireDelay,X
/* 17549: A8 */       TAY
/* 1754A: B9 92 B5 */ LDA B592_table,Y
/* 1754D: DD 00 06 */ CMP ObjectPosY,X
/* 17550: B0 1A */    BCS L1756C ; +
/* 17552: 9D 00 06 */ STA ObjectPosY,X
/* 17555: A9 30 */    LDA #$30
/* 17557: 9D 40 04 */ STA ObjectUnknown440,X
/* 1755A: BD 20 04 */ LDA ObjectFlags,X
/* 1755D: 49 40 */    EOR #$40
/* 1755F: 29 EF */    AND #$ef
/* 17561: 9D 20 04 */ STA ObjectFlags,X
/* 17564: A0 60 */    LDY #$60
/* 17566: 20 33 F5 */ JSR InitObjectDefaultSpeed
/* 17569: FE 40 06 */ INC ObjectFireDelay,X
L1756C: ; +
/* 1756C: BD 00 06 */ LDA ObjectPosY,X
/* 1756F: C9 D8 */    CMP #$d8
/* 17571: 90 1B */    BCC L1758E
/* 17573: BD 60 04 */ LDA ObjectPosScreen,X
/* 17576: E9 0A */    SBC #$0a
/* 17578: A8 */       TAY
/* 17579: B9 99 B5 */ LDA B599_table,Y
/* 1757C: 9D 80 04 */ STA ObjectPosX,X
/* 1757F: A9 04 */    LDA #$04
/* 17581: 9D 00 06 */ STA ObjectPosY,X
/* 17584: A9 00 */    LDA #$00
/* 17586: 9D 40 06 */ STA ObjectFireDelay,X
/* 17589: A0 2C */    LDY #$2c
/* 1758B: 20 33 F5 */ JSR InitObjectDefaultSpeed
L1758E:
/* 1758E: 20 49 BE */ JSR EnemyAI_MovementsAndDamageCheck
/* 17591: 60 */       RTS

B592_table: .byte $60,$50,$A0,$D0,$FF,$FF,$FF
B599_table: .byte $D0,$B0


AI_VerticalFire:
/* 1759B: BD 20 04 */ LDA ObjectFlags,X
/* 1759E: 29 20 */    AND #$20
/* 175A0: D0 4D */    BNE L175EF
/* 175A2: BD 20 04 */ LDA ObjectFlags,X
/* 175A5: 29 80 */    AND #$80
/* 175A7: D0 66 */    BNE L1760F
L175A9:
/* 175A9: BD A0 06 */ LDA ObjectLifeCycleCounter,X
/* 175AC: F0 03 */    BEQ L175B1
/* 175AE: 4C 57 B6 */ JMP L17657
L175B1:
/* 175B1: A9 06 */    LDA #$06
/* 175B3: 9D 20 04 */ STA ObjectFlags,X
/* 175B6: BD 40 04 */ LDA ObjectUnknown440,X
/* 175B9: C9 30 */    CMP #$30
/* 175BB: D0 05 */    BNE L175C2
/* 175BD: A9 00 */    LDA #$00
/* 175BF: 9D 40 04 */ STA ObjectUnknown440,X
L175C2:
/* 175C2: DE 40 06 */ DEC ObjectFireDelay,X
/* 175C5: D0 3C */    BNE L17603
/* 175C7: BD 80 06 */ LDA ObjectYSpeed,X
/* 175CA: F0 17 */    BEQ L175E3
/* 175CC: 10 0E */    BPL L175DC
/* 175CE: A9 00 */    LDA #$00
/* 175D0: 9D 80 06 */ STA ObjectYSpeed,X
/* 175D3: A9 20 */    LDA #$20
/* 175D5: 9D 20 04 */ STA ObjectFlags,X
/* 175D8: A9 18 */    LDA #$18
/* 175DA: D0 0E */    BNE L175EA
L175DC:
/* 175DC: A9 00 */    LDA #$00
/* 175DE: 9D 80 06 */ STA ObjectYSpeed,X
/* 175E1: F0 05 */    BEQ L175E8
L175E3:
/* 175E3: A9 FE */    LDA #$fe
/* 175E5: 9D 80 06 */ STA ObjectYSpeed,X
L175E8:
/* 175E8: A9 20 */    LDA #$20
L175EA:
/* 175EA: 9D 40 06 */ STA ObjectFireDelay,X
/* 175ED: D0 14 */    BNE L17603
L175EF:
/* 175EF: DE 40 06 */ DEC ObjectFireDelay,X
/* 175F2: D0 0F */    BNE L17603
/* 175F4: A9 06 */    LDA #$06
/* 175F6: 9D 20 04 */ STA ObjectFlags,X
/* 175F9: A9 02 */    LDA #$02
/* 175FB: 9D 80 06 */ STA ObjectYSpeed,X
/* 175FE: A9 20 */    LDA #$20
/* 17600: 9D 40 06 */ STA ObjectFireDelay,X
L17603:
/* 17603: 20 49 BE */ JSR EnemyAI_MovementsAndDamageCheck
/* 17606: 90 06 */    BCC L1760E
/* 17608: 20 6B B6 */ JSR F1766B
/* 1760B: 20 4A C6 */ JSR F1C64A
L1760E:
/* 1760E: 60 */       RTS



L1760F:
/* 1760F: A9 00 */    LDA #$00
/* 17611: 9D 20 04 */ STA ObjectFlags,X
/* 17614: A5 5F */    LDA WeaponSelect
/* 17616: C9 02 */    CMP #$02
/* 17618: D0 8F */    BNE L175A9
/* 1761A: BD 00 06 */ LDA ObjectPosY,X
/* 1761D: 48 */       PHA
/* 1761E: 29 F0 */    AND #$f0
/* 17620: 9D 00 06 */ STA ObjectPosY,X
/* 17623: BD 80 06 */ LDA ObjectYSpeed,X
/* 17626: 10 14 */    BPL L1763C
/* 17628: 68 */       PLA
/* 17629: 29 0F */    AND #$0f
/* 1762B: F0 1E */    BEQ L1764B
/* 1762D: 18 */       CLC
/* 1762E: 49 0F */    EOR #$0f
/* 17630: 69 01 */    ADC #$01
/* 17632: 48 */       PHA
/* 17633: 18 */       CLC
/* 17634: BD 00 06 */ LDA ObjectPosY,X
/* 17637: 69 10 */    ADC #$10
/* 17639: 9D 00 06 */ STA ObjectPosY,X
L1763C:
/* 1763C: 68 */       PLA
/* 1763D: 29 0F */    AND #$0f
/* 1763F: 4A */       LSR A
/* 17640: 85 0C */    STA $0C
/* 17642: 38 */       SEC
/* 17643: BD 40 06 */ LDA ObjectFireDelay,X
/* 17646: E5 0C */    SBC $0C
/* 17648: 9D 40 06 */ STA ObjectFireDelay,X
L1764B:
/* 1764B: 20 6B B6 */ JSR F1766B
/* 1764E: 38 */       SEC
/* 1764F: BD 00 06 */ LDA ObjectPosY,X
/* 17652: E9 20 */    SBC #$20
/* 17654: 20 3F C6 */ JSR F1C63F
L17657:
/* 17657: BD A0 06 */ LDA ObjectLifeCycleCounter,X
/* 1765A: C9 01 */    CMP #$01
/* 1765C: D0 06 */    BNE L17664
/* 1765E: 20 6B B6 */ JSR F1766B
/* 17661: 20 4A C6 */ JSR F1C64A
L17664:
/* 17664: A9 30 */    LDA #$30
/* 17666: 9D 40 04 */ STA ObjectUnknown440,X
/* 17669: D0 98 */    BNE L17603
F1766B:
/* 1766B: BD 80 04 */ LDA ObjectPosX,X
/* 1766E: 29 F0 */    AND #$f0
/* 17670: 1D 60 04 */ ORA ObjectPosScreen,X
/* 17673: A0 00 */    LDY #$00
/* 17675: 48 */       PHA
/* 17676: A5 31 */    LDA CurrentStage
/* 17678: C9 06 */    CMP #$06
/* 1767A: F0 13 */    BEQ L1768F
/* 1767C: 68 */       PLA
L1767D:
/* 1767D: D9 98 B6 */ CMP L17698,Y
/* 17680: F0 03 */    BEQ L17685
/* 17682: C8 */       INY
/* 17683: D0 F8 */    BNE L1767D
L17685:
/* 17685: 98 */       TYA
/* 17686: 0A */       ASL A
/* 17687: 85 0C */    STA $0C
/* 17689: 0A */       ASL A
/* 1768A: 65 0C */    ADC $0C
/* 1768C: 85 0C */    STA $0C
/* 1768E: 60 */       RTS
L1768F:
/* 1768F: 68 */       PLA
L17690:
/* 17690: D9 A7 B6 */ CMP L176A7,Y
/* 17693: F0 F0 */    BEQ L17685
/* 17695: C8 */       INY
/* 17696: D0 F8 */    BNE L17690

L17698:
	.byte $72,$33,$53,$74,$94,$56,$76,$97,$D7,$58,$98,$F9,$3A,$BF,$DF

L176A7:
	.byte $00,$00,$00,$00,$00,$7E,$9E,$DE

AI_GenericWithLifeTimer:

; Remove object is passed certain time, else jsr EnemyAI_MovementsAndDamageCheck
/* 176AF: BD 20 04 */ LDA ObjectFlags,X
/* 176B2: 29 10 */    AND #$10
/* 176B4: F0 0D */    BEQ AI_Generic
/* 176B6: FE 40 06 */ INC ObjectFireDelay,X
/* 176B9: BD 40 06 */ LDA ObjectFireDelay,X
/* 176BC: C9 F0 */    CMP #$f0
/* 176BE: D0 03 */    BNE AI_Generic
/* 176C0: 4C B1 BE */ JMP RemoveObject                                    ; $BEB1

AI_Generic:
/* 176C3: 20 49 BE */ JSR EnemyAI_MovementsAndDamageCheck
/* 176C6: 60 */       RTS

AI_Object1B: ;small explosion?
/* 176C7: BD 40 04 */ LDA ObjectUnknown440,X
/* 176CA: D0 F7 */    BNE AI_Generic
/* 176CC: 4C B1 BE */ JMP RemoveObject                                    ; $BEB1

AI_Object1C:
/* 176CF: BD 40 06 */ LDA ObjectFireDelay,X
/* 176D2: D0 34 */    BNE L17708 ; ++
/* 176D4: 20 3B F6 */ JSR EnemySearchMegaman
/* 176D7: C9 50 */    CMP #$50
/* 176D9: B0 30 */    BCS L1770B ; +++
/* 176DB: AA */       TAX
/* 176DC: A0 00 */    LDY #$00
/* 176DE: A9 33 */    LDA #$33
/* 176E0: 20 AC C5 */ JSR EnemyCalculateJumpCurveToHitMegaman
/* 176E3: A9 11 */    LDA #$11
/* 176E5: 20 63 F6 */ JSR CreateEnemy                                       ; $F663
/* 176E8: B0 17 */    BCS L17701 ; +
/* 176EA: BD 20 04 */ LDA ObjectFlags,X
/* 176ED: 09 14 */    ORA #$14
/* 176EF: 9D 20 04 */ STA ObjectFlags,X
/* 176F2: A0 70 */    LDY #$70
/* 176F4: 20 33 F5 */ JSR InitObjectDefaultSpeed
/* 176F7: A5 05 */    LDA $05
/* 176F9: 9D C0 04 */ STA ObjectXSpeed,X
/* 176FC: A5 04 */    LDA $04
/* 176FE: 9D E0 04 */ STA ObjectXSpeedFraction,X
L17701: ; +
/* 17701: A6 2F */    LDX RefObjectNum
/* 17703: A9 20 */    LDA #$20
/* 17705: 9D 40 06 */ STA ObjectFireDelay,X
L17708: ; ++
/* 17708: DE 40 06 */ DEC ObjectFireDelay,X
L1770B: ; +++
/* 1770B: 20 49 BE */ JSR EnemyAI_MovementsAndDamageCheck
/* 1770E: 60 */       RTS

AI_Object1D:
/* 1770F: B5 6B */    LDA Meters+1,X
/* 17711: 48 */       PHA
/* 17712: 20 49 BE */  JSR EnemyAI_MovementsAndDamageCheck
/* 17715: 68 */       PLA
/* 17716: 85 0D */    STA $0D
/* 17718: BD 40 06 */ LDA ObjectFireDelay,X
/* 1771B: D0 1F */    BNE L1773C ; +++
/* 1771D: BD E0 06 */ LDA ObjectType,X
/* 17720: C9 1B */    CMP #$1b
/* 17722: D0 17 */    BNE L1773B ; ++                                              ; $B73B
/* 17724: A9 1E */    LDA #$1e
/* 17726: 85 0C */    STA $0C
/* 17728: A2 10 */    LDX #$10
L1772A: ; -
/* 1772A: 20 16 F5 */ JSR FindObjectOfSelectedType
/* 1772D: B0 0C */    BCS L1773B ; ++                                              ; $B73B
/* 1772F: B5 6B */    LDA Meters+1,X
/* 17731: C5 0D */    CMP $0D
/* 17733: F0 03 */    BEQ L17738 ; +                                               ; $B738
/* 17735: E8 */       INX
/* 17736: D0 F2 */    BNE L1772A ; -                                               ; $B72A
L17738: ; +
/* 17738: 20 E9 BE */ JSR EnemyKilled                                     ; $BEE9
L1773B: ; ++
/* 1773B: 60 */       RTS
L1773C: ; +++
/* 1773C: DE 40 06 */ DEC ObjectFireDelay,X
/* 1773F: D0 FA */    BNE L1773B
/* 17741: A9 00 */    LDA #$00
/* 17743: 9D 80 06 */ STA ObjectYSpeed,X
/* 17746: A9 01 */    LDA #$01
/* 17748: 9D C0 04 */ STA ObjectXSpeed,X
/* 1774B: A9 20 */    LDA #$20
/* 1774D: 9D E0 04 */ STA ObjectXSpeedFraction,X
/* 17750: A9 1F */    LDA #$1f
/* 17752: 9D E0 06 */ STA ObjectType,X
/* 17755: 60 */       RTS


AI_Object1E:
/* 17756: 20 3B F6 */ JSR EnemySearchMegaman
/* 17759: BD 20 04 */ LDA ObjectFlags,X
/* 1775C: 29 40 */    AND #$40
/* 1775E: F0 0F */    BEQ L1776F ; +
/* 17760: A9 29 */    LDA #$29
/* 17762: 9D E0 06 */ STA ObjectType,X
/* 17765: A9 1F */    LDA #$1f
/* 17767: 9D 40 06 */ STA ObjectFireDelay,X
/* 1776A: A9 17 */    LDA #$17
/* 1776C: 9D 20 04 */ STA ObjectFlags,X
L1776F: ; +
/* 1776F: B5 6B */    LDA Meters+1,X
/* 17771: 48 */       PHA
/* 17772: 20 49 BE */  JSR EnemyAI_MovementsAndDamageCheck
/* 17775: 68 */       PLA
/* 17776: 85 0D */    STA $0D
/* 17778: A9 1D */    LDA #$1d
/* 1777A: 85 0C */    STA $0C
/* 1777C: A2 10 */    LDX #$10
L1777E: ; -
/* 1777E: 20 16 F5 */ JSR FindObjectOfSelectedType
/* 17781: B0 5B */    BCS L177DE ; ++
/* 17783: B5 6B */    LDA Meters+1,X
/* 17785: C5 0D */    CMP $0D
/* 17787: F0 03 */    BEQ L1778C ; +
/* 17789: E8 */       INX
/* 1778A: D0 F2 */    BNE L1777E ; -
L1778C: ; +
/* 1778C: A4 2F */    LDY RefObjectNum
/* 1778E: 38 */       SEC
/* 1778F: B9 00 06 */ LDA ObjectPosY,Y      ;Put this object 16 pixels above the ref object
/* 17792: E9 10 */    SBC #$10
/* 17794: 9D 00 06 */ STA ObjectPosY,X
/* 17797: B9 80 04 */ LDA ObjectPosX,Y      ;At same X coordinate
/* 1779A: 9D 80 04 */ STA ObjectPosX,X
/* 1779D: B9 60 04 */ LDA ObjectPosScreen,Y
/* 177A0: 9D 60 04 */ STA ObjectPosScreen,X
/* 177A3: BD A0 06 */ LDA ObjectLifeCycleCounter,X
/* 177A6: F0 03 */    BEQ L177AB ; +
/* 177A8: 99 A0 06 */ STA ObjectLifeCycleCounter,Y
L177AB: ; +
/* 177AB: B9 A0 06 */ LDA ObjectLifeCycleCounter,Y
/* 177AE: F0 03 */    BEQ L177B3 ; +
/* 177B0: 9D A0 06 */ STA ObjectLifeCycleCounter,X
L177B3: ; +
/* 177B3: B9 E0 06 */ LDA ObjectType,Y
/* 177B6: C9 1B */    CMP #$1b
/* 177B8: F0 07 */    BEQ L177C1 ; +
/* 177BA: B9 E0 06 */ LDA ObjectType,Y
/* 177BD: C9 29 */    CMP #$29
/* 177BF: D0 1D */    BNE L177DE ; ++
L177C1: ; +
/* 177C1: A9 1D */    LDA #$1d
/* 177C3: 9D E0 06 */ STA ObjectType,X
/* 177C6: A9 09 */    LDA #$09
/* 177C8: 9D 40 06 */ STA ObjectFireDelay,X
/* 177CB: A9 00 */    LDA #$00
/* 177CD: 9D 40 04 */ STA ObjectUnknown440,X
/* 177D0: 9D E0 04 */ STA ObjectXSpeedFraction,X
/* 177D3: 9D C0 04 */ STA ObjectXSpeed,X
/* 177D6: 9D 60 06 */ STA ObjectYSpeedFraction,X
/* 177D9: A9 04 */    LDA #$04
/* 177DB: 9D 80 06 */ STA ObjectYSpeed,X
L177DE: ; ++
/* 177DE: A6 2F */    LDX RefObjectNum
/* 177E0: FE 40 06 */ INC ObjectFireDelay,X
/* 177E3: BD 40 06 */ LDA ObjectFireDelay,X
/* 177E6: C9 3F */    CMP #$3f
/* 177E8: D0 1F */    BNE L17809 ; +
/* 177EA: A9 00 */    LDA #$00
/* 177EC: 9D 40 06 */ STA ObjectFireDelay,X
/* 177EF: BD A0 06 */ LDA ObjectLifeCycleCounter,X
/* 177F2: D0 15 */    BNE L17809 ; +
/* 177F4: A9 1A */    LDA #$1A
/* 177F6: 20 63 F6 */ JSR CreateEnemy ;bullet sound?
/* 177F9: B0 0E */    BCS L17809 ; +
/* 177FB: 38 */       SEC
/* 177FC: BD 00 06 */ LDA ObjectPosY,X
/* 177FF: E9 0C */    SBC #$0c
/* 17801: 9D 00 06 */ STA ObjectPosY,X
/* 17804: A0 24 */    LDY #$24
/* 17806: 20 33 F5 */ JSR InitObjectDefaultSpeed
L17809: ; +
/* 17809: 60 */       RTS

AI_RogueRobotHead:
/* 1780A: BD 20 04 */ LDA ObjectFlags,X
/* 1780D: 29 08 */    AND #$08
/* 1780F: F0 07 */    BEQ L17818 ; +
/* 17811: 20 49 BE */ JSR EnemyAI_MovementsAndDamageCheck
/* 17814: A9 00 */    LDA #$00
/* 17816: F0 48 */    BEQ L17860 ; +++
L17818: ; +
/* 17818: 20 D1 AA */ JSR AI_Blader
/* 1781B: BD 40 06 */ LDA ObjectFireDelay,X
/* 1781E: C9 32 */    CMP #$32
/* 17820: D0 05 */    BNE L17827 ; +
/* 17822: A9 10 */    LDA #$10
/* 17824: 9D 40 04 */ STA ObjectUnknown440,X
L17827: ; +
/* 17827: BD 40 04 */ LDA ObjectUnknown440,X
/* 1782A: 29 F0 */    AND #$f0
/* 1782C: F0 32 */    BEQ L17860 ; +++
/* 1782E: C9 20 */    CMP #$20
/* 17830: D0 2D */    BNE L1785F ; ++
/* 17832: A9 20 */    LDA #$20
/* 17834: 20 63 F6 */ JSR CreateEnemy                                       ; $F663
/* 17837: B0 26 */    BCS L1785F ; ++
/* 17839: 18 */       CLC
/* 1783A: BD 00 06 */ LDA ObjectPosY,X
/* 1783D: 69 10 */    ADC #$10
/* 1783F: 9D 00 06 */ STA ObjectPosY,X
/* 17842: A0 00 */    LDY #$00
/* 17844: BD 20 04 */ LDA ObjectFlags,X
/* 17847: 29 40 */    AND #$40
/* 17849: F0 01 */    BEQ L1784C ; +
/* 1784B: C8 */       INY
L1784C: ; +
/* 1784C: 18 */       CLC
/* 1784D: BD 80 04 */ LDA ObjectPosX,X
/* 17850: 79 66 B8 */ ADC RogueRobotHead_XloTable,Y
/* 17853: 9D 80 04 */ STA ObjectPosX,X
/* 17856: BD 60 04 */ LDA ObjectPosScreen,X
/* 17859: 79 64 B8 */ ADC RogueRobotHead_XhiTable,Y
/* 1785C: 9D 60 04 */ STA ObjectPosScreen,X
L1785F: ; ++
/* 1785F: 60 */       RTS
L17860: ; +++
/* 17860: 9D 40 04 */ STA ObjectUnknown440,X
/* 17863: 60 */       RTS

RogueRobotHead_XhiTable: .byte  NEG{1},0
RogueRobotHead_XloTable: .byte NEG{11},11

AI_Object20:
/* 17868: BD 40 04 */ LDA ObjectUnknown440,X
/* 1786B: F0 05 */    BEQ L17872 ; +
/* 1786D: A9 F8 */    LDA #$f8
/* 1786F: 9D 00 06 */ STA ObjectPosY,X
L17872: ; +
/* 17872: 20 49 BE */ JSR EnemyAI_MovementsAndDamageCheck
/* 17875: 60 */       RTS



AI_BombombCluster:
/* 17876: 20 49 BE */ JSR EnemyAI_MovementsAndDamageCheck

/* 17879: A5 2A */    LDA $2A
/* 1787B: 05 2B */    ORA $2B
/* 1787D: F0 18 */    BEQ L17897 ; +                                               ; $B897
L1787F:
/* 1787F: A9 02 */    LDA #$02
/* 17881: 9D 20 04 */ STA ObjectFlags,X
/* 17884: A9 1B */    LDA #$1b
/* 17886: 9D E0 06 */ STA ObjectType,X

/* 17889: 20 45 B0 */ JSR ClearObjectMem                                  ; $B045
/* 1788C: 9D 40 04 */ STA ObjectUnknown440,X
/* 1788F: 9D 40 06 */ STA ObjectFireDelay,X
/* 17892: A9 12 */    LDA #$12        ; Explosion sound
/* 17894: 20 77 C4 */ JSR IssueSound                                      ; $C477
L17897: ; +
/* 17897: 60 */       RTS


AI_Object28: ; Individual explosion part
/* 17898: FE 40 06 */ INC ObjectFireDelay,X
/* 1789B: BD 40 06 */ LDA ObjectFireDelay,X
/* 1789E: C9 0E */    CMP #$0e
/* 178A0: D0 D0 */    BNE L17872               ; If life is less than 14 frames, just idle
/* 178A2: A9 F8 */    LDA #$f8
/* 178A4: 9D 00 06 */ STA ObjectPosY,X        ; Then die.
/* 178A7: 60 */       RTS

AI_Object29:
/* 178A8: DE 40 06 */ DEC ObjectFireDelay,X
/* 178AB: D0 C5 */    BNE L17872
/* 178AD: F0 D0 */    BEQ L1787F
AI_Object2A:
/* 178AF: 20 3B F6 */ JSR EnemySearchMegaman
/* 178B2: 38 */       SEC
/* 178B3: AD 00 06 */ LDA ObjectPosY+0
/* 178B6: FD 00 06 */ SBC ObjectPosY,X
/* 178B9: B0 04 */    BCS L178BF
/* 178BB: 49 FF */    EOR #$ff
/* 178BD: 69 01 */    ADC #$01
L178BF:
/* 178BF: 85 0C */    STA $0C
/* 178C1: C9 28 */    CMP #$28
/* 178C3: B0 4F */    BCS L17914
/* 178C5: BD 40 06 */ LDA ObjectFireDelay,X
/* 178C8: C9 10 */    CMP #$10
/* 178CA: D0 41 */    BNE L1790D
/* 178CC: A5 0C */    LDA $0C
/* 178CE: C9 1C */    CMP #$1c
/* 178D0: B0 3E */    BCS L17910
/* 178D2: A9 01 */    LDA #$01
/* 178D4: 85 0C */    STA $0C
L178D6:
/* 178D6: A9 2B */    LDA #$2b
/* 178D8: 20 63 F6 */ JSR CreateEnemy                                       ; $F663
/* 178DB: B0 18 */    BCS L178F5
/* 178DD: 20 45 B0 */ JSR ClearObjectMem
/* 178E0: A4 0C */    LDY $0C
/* 178E2: 18 */       CLC
/* 178E3: BD 00 06 */ LDA ObjectPosY,X
/* 178E6: 79 1F B9 */ ADC L1791F,Y
/* 178E9: 9D 00 06 */ STA ObjectPosY,X
/* 178EC: A9 04 */    LDA #$04
/* 178EE: 9D C0 04 */ STA ObjectXSpeed,X
/* 178F1: C6 0C */    DEC $0C
/* 178F3: 10 E1 */    BPL L178D6
L178F5:
/* 178F5: A6 2F */    LDX RefObjectNum
/* 178F7: BD 40 06 */ LDA ObjectFireDelay,X
/* 178FA: 20 63 AB */ JSR ObjectFlipYmovement
/* 178FD: 1E 60 06 */ ASL ObjectYSpeedFraction,X
/* 17900: 3E 80 06 */ ROL ObjectYSpeed,X
/* 17903: BD 20 04 */ LDA ObjectFlags,X
/* 17906: 09 08 */    ORA #$08
/* 17908: 9D 20 04 */ STA ObjectFlags,X
/* 1790B: D0 07 */    BNE L17914
L1790D:
/* 1790D: FE 40 06 */ INC ObjectFireDelay,X
L17910:
/* 17910: 20 49 BE */ JSR EnemyAI_MovementsAndDamageCheck
/* 17913: 60 */       RTS



L17914:
/* 17914: A9 30 */    LDA #$30
/* 17916: 9D 40 04 */ STA ObjectUnknown440,X
/* 17919: A9 06 */    LDA #$06
/* 1791B: 9D E0 06 */ STA ObjectType,X
/* 1791E: 60 */       RTS


L1791F:
/* 1791F: 14 */       .byte $14
/* 17920: EC */       .byte $EC

AI_LiftAnimator:
/* 17921: A0 00 */    LDY #$00          ;moves left
/* 17923: BD 40 06 */ LDA ObjectFireDelay,X
/* 17926: 29 40 */    AND #$40
/* 17928: 08 */       PHP
/* 17929: F0 02 */     BEQ L1792D ; +
/* 1792B: A0 03 */     LDY #$03         ;moves right
L1792D: ; +
/* 1792D: 18 */        CLC
/* 1792E: BD A0 04 */  LDA ObjectPosXfraction,X
/* 17931: 79 2C BA */  ADC LiftMovementData+0,Y
/* 17934: 9D A0 04 */  STA ObjectPosXfraction,X
/* 17937: BD 80 04 */  LDA ObjectPosX,X
/* 1793A: 79 2D BA */  ADC LiftMovementData+1,Y
/* 1793D: 9D 80 04 */  STA ObjectPosX,X
/* 17940: BD 60 04 */  LDA ObjectPosScreen,X
/* 17943: 79 2E BA */  ADC LiftMovementData+2,Y
/* 17946: 9D 60 04 */  STA ObjectPosScreen,X
/* 17949: 38 */        SEC
/* 1794A: BD 80 04 */  LDA ObjectPosX,X
/* 1794D: E5 1A */     SBC ScrollPosX
/* 1794F: A8 */        TAY
/* 17950: BD 60 04 */  LDA ObjectPosScreen,X
/* 17953: E5 1B */     SBC ScrollPosScreen ;Disappearing on another screen?
/* 17955: D0 0C */     BNE L17963 ; +
/* 17957: C0 F8 */     CPY #$F8      ;Disappearing on the right?
/* 17959: B0 08 */     BCS L17963 ; +
/* 1795B: C0 10 */     CPY #$10      ;Disappearing on the left?
/* 1795D: 90 04 */     BCC L17963 ; +
/* 1795F: A9 00 */     LDA #$00      ;Make visible
/* 17961: F0 02 */     BEQ L17965 ; ++
L17963: ; +
/* 17963: A9 20 */     LDA #$20      ;Make invisible
L17965: ; ++
/* 17965: 9D 20 04 */  STA ObjectFlags,X
/* 17968: BD C0 06 */  LDA ObjectLifeMeter,X    ;Load lift index
/* 1796B: 0A */        ASL A
/* 1796C: A8 */        TAY             ;lift index*2
/* 1796D: 48 */        PHA
/* 1796E: B9 42 BA */   LDA BA42_table+0,Y
/* 17971: 85 04 */      STA $04
/* 17973: B9 43 BA */   LDA BA42_table+1,Y
/* 17976: 85 05 */      STA $05
/* 17978: 68 */        PLA
/* 17979: 0A */        ASL A           ;lift index*4
/* 1797A: 28 */       PLP
/* 1797B: 48 */       PHA
/* 1797C: D0 3D */     BNE AI_Lift_MovesRight
AI_Lift_MovesLeft:
/* 1797E: BD 60 06 */  LDA ObjectYSpeedFraction,X      ;8 for lift 3, 0 for others
/* 17981: F0 25 */     BEQ L179A8 ; ++
/* 17983: 38 */        SEC
/* 17984: E9 01 */     SBC #$01
/* 17986: 0A */        ASL A
/* 17987: A8 */        TAY
/* 17988: B1 04 */     LDA ($04),Y
/* 1798A: 85 07 */     STA LiftXScreenMovement
/* 1798C: C8 */        INY
/* 1798D: B1 04 */     LDA ($04),Y
/* 1798F: 85 06 */     STA LiftXMovement
/* 17991: 20 20 BA */  JSR LiftMove
/* 17994: 90 12 */     BCC L179A8 ; ++
/* 17996: BD 60 06 */  LDA ObjectYSpeedFraction,X
/* 17999: DE 60 06 */  DEC ObjectYSpeedFraction,X
/* 1799C: A0 10 */     LDY #$10
/* 1799E: 29 01 */     AND #$01
/* 179A0: F0 02 */     BEQ L179A4 ; +
/* 179A2: A0 30 */     LDY #$30
L179A4: ; +
/* 179A4: 98 */        TYA
/* 179A5: 9D 40 04 */  STA ObjectUnknown440,X
L179A8: ; ++
/* 179A8: 68 */       PLA
/* 179A9: A8 */       TAY
/* 179AA: B9 32 BA */ LDA BA32_table+0,Y
/* 179AD: 85 06 */    STA LiftXMovement+0
/* 179AF: B9 33 BA */ LDA BA32_table+1,Y
/* 179B2: 85 07 */    STA LiftXScreenMovement
/* 179B4: 20 20 BA */ JSR LiftMove
/* 179B7: B0 38 */    BCS AI_Lift_Reverse
/* 179B9: 90 3E */    BCC AI_Lift_Common
AI_Lift_MovesRight:
/* 179BB: BD 60 06 */  LDA ObjectYSpeedFraction,X
/* 179BE: 0A */        ASL A
/* 179BF: A8 */        TAY
/* 179C0: B1 04 */     LDA ($04),Y
/* 179C2: 85 07 */     STA LiftXScreenMovement
/* 179C4: C8 */        INY
/* 179C5: B1 04 */     LDA ($04),Y
/* 179C7: 85 06 */     STA LiftXMovement
/* 179C9: 20 20 BA */  JSR LiftMove
/* 179CC: B0 12 */     BCS L179E0 ; ++
/* 179CE: A0 10 */     LDY #$10
/* 179D0: BD 60 06 */  LDA ObjectYSpeedFraction,X
/* 179D3: FE 60 06 */  INC ObjectYSpeedFraction,X
/* 179D6: 29 01 */     AND #$01
/* 179D8: F0 02 */     BEQ L179DC ; +
/* 179DA: A0 30 */     LDY #$30
L179DC: ; +
/* 179DC: 98 */        TYA
/* 179DD: 9D 40 04 */  STA ObjectUnknown440,X
L179E0: ; ++
/* 179E0: 68 */       PLA
/* 179E1: A8 */       TAY
/* 179E2: B9 34 BA */ LDA BA34_table+0,Y
/* 179E5: 85 06 */    STA LiftXMovement+0
/* 179E7: B9 35 BA */ LDA BA34_table+1,Y
/* 179EA: 85 07 */    STA LiftXScreenMovement
/* 179EC: 20 20 BA */ JSR LiftMove
/* 179EF: B0 08 */    BCS AI_Lift_Common
AI_Lift_Reverse:
/* 179F1: BD 40 06 */ LDA ObjectFireDelay,X      ;changes the direction
/* 179F4: 49 40 */    EOR #$40
/* 179F6: 9D 40 06 */ STA ObjectFireDelay,X
AI_Lift_Common:
/* 179F9: BD 40 04 */ LDA ObjectUnknown440,X
/* 179FC: 29 10 */    AND #$10
/* 179FE: D0 08 */    BNE L17A08 ; +
/* 17A00: BD 40 04 */ LDA ObjectUnknown440,X
/* 17A03: 29 F0 */    AND #$f0
/* 17A05: 9D 40 04 */ STA ObjectUnknown440,X
L17A08: ; +
/* 17A08: BD 40 04 */ LDA ObjectUnknown440,X
/* 17A0B: 29 F0 */    AND #$f0
/* 17A0D: D0 10 */    BNE L17A1F ; +
/* 17A0F: BD 20 04 */ LDA ObjectFlags,X
/* 17A12: 29 20 */    AND #$20
/* 17A14: D0 09 */    BNE L17A1F ; +             ;Don't store lift data if it's invisible
/* 17A16: 20 AE B1 */ JSR StoreLiftData
/* 17A19: BD 40 06 */ LDA ObjectFireDelay,X
/* 17A1C: 99 18 07 */ STA LiftDirection,Y
L17A1F: ; +
/* 17A1F: 60 */       RTS

LiftMove:
/* 17A20: 38 */       SEC
/* 17A21: A5 06 */    LDA LiftXMovement
/* 17A23: FD 80 04 */ SBC ObjectPosX,X
/* 17A26: A5 07 */    LDA LiftXScreenMovement
/* 17A28: FD 60 04 */ SBC ObjectPosScreen,X
/* 17A2B: 60 */       RTS

; Various Lift data, not quite analyzed yet

LiftMovementData: ;at BA2C
    ; byte 0: added to ObjectPosXfraction
    ; byte 1: added to ObjectPosX
    ; byte 2: added to ObjectPosScreen
    .byte $00,$FF,$FF ;moving left
    .byte $00,$01,$00 ;moving right
BA32_table: ;stored into CurrentRoomPointer
    .word $0208
BA34_table: ;stored into CurrentRoomPointer
    .word $0318
    .word $0288,$0398
    .word $0308,$0468
    .word $1B58,$1CF0
BA42_table: ;pointers... at BA42
    .word BA4A
    .word BA4B
    .word BA54
    .word BA69
BA4A:
    .byte $FF
BA4B:
    .byte $02
     .word $03F0
     .word $0300
     .word $0370
     .word $FF80
BA54:
    .byte $03
    .word $0340
    .word $0350
    .word $0380
    .word $0390
    .word $03C0
    .word $04D0
    .word $0400
    .word $0410
    .word $0440
    .word $FF50
BA69:
    .word $C01B
    .word $201C
    .byte $FF

AI_Object2F:
/* 17A6E: A5 31 */    lda CurrentStage
/* 17A70: C9 05 */    CMP #$05
/* 17A72: F0 52 */    BEQ Object2F_liftsound

/* 17A74: BD 40 06 */ LDA ObjectFireDelay,X
/* 17A77: D0 0B */    BNE L17A84 ; +

/* 17A79: A9 30 */    LDA #$30        ; Wind starting(?)
/* 17A7B: 20 77 C4 */ JSR IssueSound                                      ; $C477

/* 17A7E: A6 2F */    LDX RefObjectNum
/* 17A80: A9 00 */    LDA #$00
/* 17A82: F0 04 */    BEQ L17A88 ; ++
L17A84: ; +
/* 17A84: C9 20 */    CMP #$20
/* 17A86: F0 31 */    BEQ L17AB9 ; +++
L17A88: ; ++
/* 17A88: 48 */       PHA
/* 17A89: 29 F0 */    AND #$f0
/* 17A8B: 4A */       LSR A
/* 17A8C: 4A */       LSR A
/* 17A8D: 09 23 */    ORA #$23
/* 17A8F: 8D 80 03 */ STA RawPPUtransferAddress+0
/* 17A92: 68 */       PLA
/* 17A93: 29 0F */    AND #$0f
/* 17A95: 4A */       LSR A
/* 17A96: 90 04 */    BCC L17A9C ; +                                               ; $BA9C
/* 17A98: 09 D8 */    ORA #$d8
/* 17A9A: D0 02 */    BNE L17A9E ; ++                                              ; $BA9E
L17A9C: ; +
/* 17A9C: 09 E0 */    ORA #$e0
L17A9E: ; ++
/* 17A9E: 8D 81 03 */ STA RawPPUtransferAddress+1
/* 17AA1: A9 AA */    LDA #$aa
/* 17AA3: 8D 82 03 */ STA RawPPUtransferBuf

/* 17AA6: A9 01 */    LDA #$01
/* 17AA8: 85 5E */    STA RawPPUtransferSize

/* 17AAA: FE 40 06 */ INC ObjectFireDelay,X
/* 17AAD: BD 40 06 */ LDA ObjectFireDelay,X
/* 17AB0: C9 0A */    CMP #$0A
/* 17AB2: F0 49 */    BEQ Object2F_createWaterFront
/* 17AB4: C9 10 */    CMP #$10
/* 17AB6: B0 01 */    BCS L17AB9 ; +++
/* 17AB8: 60 */       RTS
L17AB9: ; +++
/* 17AB9: A9 01 */    LDA #$01
/* 17ABB: 85 9D */    STA LiftXSpeed
/* 17ABD: A9 80 */    LDA #$80
/* 17ABF: 85 9C */    STA LiftXSpeedFraction
/* 17AC1: A9 C0 */    LDA #$c0
/* 17AC3: 85 9E */    STA LiftUnknown9E
/* 17AC5: 60 */       RTS

Object2F_liftsound:
/* 17AC6: A9 23 */    LDA #$23        ; Moving platform (Gutsman stage)
L17AC8:
/* 17AC8: 20 77 C4 */ JSR IssueSound                                      ; $C477

/* 17ACB: A6 2F */    LDX RefObjectNum
/* 17ACD: 4C B1 BE */ JMP RemoveObject                                    ; $BEB1

AI_Object30:
/* 17AD0: A5 31 */    LDA CurrentStage
/* 17AD2: C9 05 */    CMP #$05
/* 17AD4: F0 23 */    BEQ Object30_stopSFX
/* 17AD6: A2 02 */    LDX #$02
L17AD8: ; -
/* 17AD8: BD F6 BA */ LDA Wily3colorsetup,X
/* 17ADB: 9D D5 03 */ STA BGPalettes + 5,X
/* 17ADE: CA */       DEX
/* 17ADF: 10 F7 */    BPL L17AD8 ; -
/* 17AE1: 20 3F C7 */ JSR PaletteSetupForBG
/* 17AE4: A6 2F */    LDX RefObjectNum
/* 17AE6: A5 AB */    LDA LastRestartPointType
/* 17AE8: F0 09 */    BEQ L17AF3 ; +
/* 17AEA: DE E0 06 */ DEC ObjectType,X
/* 17AED: A9 20 */    LDA #$20
/* 17AEF: 9D 40 06 */ STA ObjectFireDelay,X
/* 17AF2: 60 */       RTS
L17AF3: ; +
/* 17AF3: 4C B1 BE */ JMP RemoveObject                                    ; $BEB1

Wily3colorsetup: ;BAF6
    .byte $2C,$1C,$07
Object30_stopSFX:
/* 17AF9: A9 FE */    LDA #$fE
/* 17AFB: D0 CB */    BNE L17AC8    ;stop special effects

Object2F_createWaterFront:
/* 17AFD: A9 04 */    LDA #$04
/* 17AFF: 85 0C */    STA $0C
/* 17B01: DE 60 04 */ DEC ObjectPosScreen,X

/* 17B04: A9 88 */    LDA #$88
/* 17B06: 9D 80 04 */ STA ObjectPosX,X
/* 17B09: A9 68 */    LDA #$68
/* 17B0B: 9D 00 06 */ STA ObjectPosY,X
L17B0E: ; -
/* 17B0E: A9 00 */    LDA #$00
/* 17B10: 20 63 F6 */ JSR CreateEnemy                                       ; $F663
/* 17B13: B0 29 */    BCS L17B3E ; +
/* 17B15: A9 7B */    LDA #$7b
/* 17B17: 9D 00 04 */ STA ObjectSpriteNum,X
/* 17B1A: A9 40 */    LDA #$40
/* 17B1C: 9D 20 04 */ STA ObjectFlags,X
/* 17B1F: 20 45 B0 */ JSR ClearObjectMem
/* 17B22: A9 10 */    LDA #$10
/* 17B24: 9D C0 04 */ STA ObjectXSpeed,X
/* 17B27: A6 2F */    LDX RefObjectNum
/* 17B29: 18 */       CLC

; Move object right by 8 pixels, down by 16 pixels
/* 17B2A: BD 80 04 */ LDA ObjectPosX,X
/* 17B2D: 69 08 */    ADC #$08
/* 17B2F: 9D 80 04 */ STA ObjectPosX,X

/* 17B32: BD 00 06 */ LDA ObjectPosY,X
/* 17B35: 69 10 */    ADC #$10
/* 17B37: 9D 00 06 */ STA ObjectPosY,X

/* 17B3A: C6 0C */    DEC $0C
/* 17B3C: D0 D0 */    BNE L17B0E ; -
L17B3E: ; +
/* 17B3E: 60 */       RTS


AI_TackleFire:
/* 17B3F: BD 40 06 */ LDA ObjectFireDelay,X
/* 17B42: F0 38 */    BEQ L17B7C
/* 17B44: BD 80 06 */ LDA ObjectYSpeed,X
/* 17B47: 10 03 */    BPL L17B4C
/* 17B49: 4C E9 BB */ JMP L17BE9
L17B4C:
/* 17B4C: BD 40 06 */ LDA ObjectFireDelay,X
/* 17B4F: C9 FF */    CMP #$ff
/* 17B51: F0 2D */    BEQ L17B80
/* 17B53: C9 83 */    CMP #$83
/* 17B55: B0 46 */    BCS L17B9D
/* 17B57: C9 08 */    CMP #$08
/* 17B59: D0 0C */    BNE L17B67
/* 17B5B: A9 00 */    LDA #$00
/* 17B5D: 9D 40 06 */ STA ObjectFireDelay,X
/* 17B60: A9 02 */    LDA #$02
/* 17B62: 9D 20 04 */ STA ObjectFlags,X
/* 17B65: D0 0B */    BNE L17B72
L17B67:
/* 17B67: 29 07 */    AND #$07
/* 17B69: D0 0C */    BNE L17B77
/* 17B6B: A9 31 */    LDA #$31
/* 17B6D: 20 63 F6 */ JSR CreateEnemy                                       ; $F663
/* 17B70: B0 05 */    BCS L17B77
L17B72:
/* 17B72: A0 1C */    LDY #$1c
/* 17B74: 20 33 F5 */ JSR InitObjectDefaultSpeed
L17B77:
/* 17B77: A6 2F */    LDX RefObjectNum
/* 17B79: DE 40 06 */ DEC ObjectFireDelay,X
L17B7C:
/* 17B7C: 20 49 BE */ JSR EnemyAI_MovementsAndDamageCheck
/* 17B7F: 60 */       RTS



L17B80:
/* 17B80: BD 00 06 */ LDA ObjectPosY,X
/* 17B83: C9 07 */    CMP #$07
/* 17B85: B0 12 */    BCS L17B99
/* 17B87: 20 45 B0 */ JSR ClearObjectMem
/* 17B8A: A9 04 */    LDA #$04
/* 17B8C: 9D 00 06 */ STA ObjectPosY,X
/* 17B8F: A9 20 */    LDA #$20
/* 17B91: 9D 20 04 */ STA ObjectFlags,X
/* 17B94: A9 83 */    LDA #$83
/* 17B96: 9D 40 06 */ STA ObjectFireDelay,X
L17B99:
/* 17B99: 20 49 BE */ JSR EnemyAI_MovementsAndDamageCheck
/* 17B9C: 60 */       RTS



L17B9D:
/* 17B9D: C9 FE */    CMP #$fe
/* 17B9F: F0 05 */    BEQ L17BA6
/* 17BA1: FE 40 06 */ INC ObjectFireDelay,X
/* 17BA4: D0 D6 */    BNE L17B7C
L17BA6:
/* 17BA6: A9 FF */    LDA #$ff
/* 17BA8: 9D 80 06 */ STA ObjectYSpeed,X
/* 17BAB: A9 10 */    LDA #$10
/* 17BAD: 9D 60 06 */ STA ObjectYSpeedFraction,X
/* 17BB0: A9 02 */    LDA #$02
/* 17BB2: 85 0C */    STA $0C
/* 17BB4: A5 46 */    LDA RandomSeed
/* 17BB6: 85 0D */    STA $0D
L17BB8:
/* 17BB8: A9 31 */    LDA #$31
/* 17BBA: 20 63 F6 */ JSR CreateEnemy                                       ; $F663
/* 17BBD: B0 26 */    BCS L17BE5
/* 17BBF: A0 74 */    LDY #$74
/* 17BC1: 20 33 F5 */ JSR InitObjectDefaultSpeed
/* 17BC4: A4 0C */    LDY $0C
/* 17BC6: 18 */       CLC
/* 17BC7: A5 1A */    LDA ScrollPosX
/* 17BC9: 79 E6 BB */ ADC L17BE6,Y
/* 17BCC: 9D 80 04 */ STA ObjectPosX,X
/* 17BCF: A5 1B */    LDA ScrollPosScreen
/* 17BD1: 69 00 */    ADC #$00
/* 17BD3: 9D 60 04 */ STA ObjectPosScreen,X
/* 17BD6: A9 00 */    LDA #$00
/* 17BD8: 46 0D */    LSR $0D
/* 17BDA: 6A */       ROR A
/* 17BDB: 4A */       LSR A
/* 17BDC: 09 06 */    ORA #$06
/* 17BDE: 9D 20 04 */ STA ObjectFlags,X
/* 17BE1: C6 0C */    DEC $0C
/* 17BE3: 10 D3 */    BPL L17BB8
L17BE5:
/* 17BE5: 60 */       RTS


L17BE6:
	.byte $7E,$A8,$D2
	
L17BE9:
/* 17BE9: BD 00 06 */ LDA ObjectPosY,X
/* 17BEC: C9 DC */    CMP #$dc
/* 17BEE: 90 0D */    BCC L17BFD
/* 17BF0: A9 E0 */    LDA #$e0
/* 17BF2: 9D 00 06 */ STA ObjectPosY,X
/* 17BF5: 20 45 B0 */ JSR ClearObjectMem
/* 17BF8: A9 1B */    LDA #$1b
/* 17BFA: 9D 40 06 */ STA ObjectFireDelay,X
L17BFD:
/* 17BFD: 20 49 BE */ JSR EnemyAI_MovementsAndDamageCheck
/* 17C00: 60 */       RTS


AI_Object33:
/* 17C01: BD 40 06 */ LDA ObjectFireDelay,X
/* 17C04: C9 2A */    CMP #$2a
/* 17C06: B0 18 */    BCS L17C20
/* 17C08: FE 40 06 */ INC ObjectFireDelay,X
/* 17C0B: 38 */       SEC
/* 17C0C: BD 60 06 */ LDA ObjectYSpeedFraction,X
/* 17C0F: E9 1A */    SBC #$1a
/* 17C11: 9D 60 06 */ STA ObjectYSpeedFraction,X
/* 17C14: BD 80 06 */ LDA ObjectYSpeed,X
/* 17C17: E9 00 */    SBC #$00
/* 17C19: 9D 80 06 */ STA ObjectYSpeed,X
/* 17C1C: 20 49 BE */ JSR EnemyAI_MovementsAndDamageCheck
/* 17C1F: 60 */       RTS



L17C20:
/* 17C20: D0 0F */    BNE L17C31
/* 17C22: A9 80 */    LDA #$80
/* 17C24: 85 48 */    STA $48
/* 17C26: 85 A8 */    STA $A8
/* 17C28: A9 02 */    LDA #$02
/* 17C2A: 85 49 */    STA $49
/* 17C2C: 85 A9 */    STA $A9
/* 17C2E: FE 40 06 */ INC ObjectFireDelay,X
L17C31:
/* 17C31: AD 80 04 */ LDA ObjectPosX+0
/* 17C34: 48 */       PHA
/* 17C35: AD 00 06 */ LDA ObjectPosY+0
/* 17C38: 48 */       PHA
/* 17C39: AD 81 04 */ LDA ObjectPosX+1
/* 17C3C: 8D 80 04 */ STA ObjectPosX+0
/* 17C3F: AD 01 06 */ LDA ObjectPosY+1
/* 17C42: 8D 00 06 */ STA ObjectPosY+0
/* 17C45: 20 3B F6 */ JSR EnemySearchMegaman
/* 17C48: 85 0C */    STA $0C
/* 17C4A: 18 */       CLC
/* 17C4B: A5 48 */    LDA $48
/* 17C4D: 69 08 */    ADC #$08
/* 17C4F: 85 48 */    STA $48
/* 17C51: 85 00 */    STA $00
/* 17C53: A5 49 */    LDA $49
/* 17C55: 69 00 */    ADC #$00
/* 17C57: 85 49 */    STA $49
/* 17C59: 85 01 */    STA $01
/* 17C5B: 20 28 F6 */ JSR F1F628
/* 17C5E: A5 03 */    LDA $03
/* 17C60: C9 0C */    CMP #$0c
/* 17C62: B0 06 */    BCS L17C6A
/* 17C64: A5 02 */    LDA $02
/* 17C66: C9 0C */    CMP #$0c
/* 17C68: 90 32 */    BCC L17C9C
L17C6A:
/* 17C6A: A6 2F */    LDX RefObjectNum
/* 17C6C: A5 A8 */    LDA $A8
/* 17C6E: 05 A9 */    ORA $A9
/* 17C70: F0 1E */    BEQ L17C90
/* 17C72: 38 */       SEC
/* 17C73: A5 A8 */    LDA $A8
/* 17C75: E9 20 */    SBC #$20
/* 17C77: 85 A8 */    STA $A8
/* 17C79: A5 A9 */    LDA $A9
/* 17C7B: E9 00 */    SBC #$00
/* 17C7D: 85 A9 */    STA $A9
/* 17C7F: 38 */       SEC
/* 17C80: BD 60 06 */ LDA ObjectYSpeedFraction,X
/* 17C83: E5 A8 */    SBC $A8
/* 17C85: 9D 60 06 */ STA ObjectYSpeedFraction,X
/* 17C88: BD 80 06 */ LDA ObjectYSpeed,X
/* 17C8B: E5 A9 */    SBC $A9
/* 17C8D: 9D 80 06 */ STA ObjectYSpeed,X
L17C90:
/* 17C90: 20 49 BE */ JSR EnemyAI_MovementsAndDamageCheck
L17C93:
/* 17C93: 68 */       PLA
/* 17C94: 8D 00 06 */ STA ObjectPosY+0
/* 17C97: 68 */       PLA
/* 17C98: 8D 80 04 */ STA ObjectPosX+0
/* 17C9B: 60 */       RTS



L17C9C:
/* 17C9C: A6 2F */    LDX RefObjectNum
/* 17C9E: A9 F8 */    LDA #$f8
/* 17CA0: 9D 00 06 */ STA ObjectPosY,X
/* 17CA3: D0 EE */    BNE L17C93
AI_Object35:
/* 17CA5: DE 40 06 */ DEC ObjectFireDelay,X
/* 17CA8: F0 1A */    BEQ L17CC4
/* 17CAA: 20 49 BE */ JSR EnemyAI_MovementsAndDamageCheck
/* 17CAD: A5 2B */    LDA $2B
/* 17CAF: F0 12 */    BEQ L17CC3
/* 17CB1: A5 49 */    LDA $49
/* 17CB3: F0 02 */    BEQ L17CB7
/* 17CB5: C6 49 */    DEC $49
L17CB7:
/* 17CB7: 9D 80 06 */ STA ObjectYSpeed,X
/* 17CBA: 9D 60 06 */ STA ObjectYSpeedFraction,X
/* 17CBD: 5E C0 04 */ LSR ObjectXSpeed,X
/* 17CC0: 7E E0 04 */ ROR ObjectXSpeedFraction,X
L17CC3:
/* 17CC3: 60 */       RTS

L17CC4:
/* 17CC4: 20 FD AE */ JSR ReplaceObjectWithExplosion
/* 17CC7: 60 */       RTS

AI_Object37:
/* 17CC8: DE 40 06 */ DEC ObjectFireDelay,X
/* 17CCB: F0 24 */    BEQ L17CF1
/* 17CCD: BD 40 06 */ LDA ObjectFireDelay,X
/* 17CD0: 29 07 */    AND #$07
/* 17CD2: A8 */       TAY
/* 17CD3: 18 */       CLC
/* 17CD4: AD 01 06 */ LDA ObjectPosY+1
/* 17CD7: 79 FA A4 */ ADC FireShieldYPosTable,Y
/* 17CDA: 9D 00 06 */ STA ObjectPosY,X
/* 17CDD: 18 */       CLC
/* 17CDE: AD 81 04 */ LDA ObjectPosX+1
/* 17CE1: 79 02 A5 */ ADC FireShieldXPosTable,Y
/* 17CE4: 9D 80 04 */ STA ObjectPosX,X
/* 17CE7: AD 61 04 */ LDA ObjectPosScreen+1
/* 17CEA: 79 0A A5 */ ADC FireShieldPosScreenTable,Y
/* 17CED: 9D 60 04 */ STA ObjectPosScreen,X
/* 17CF0: 60 */       RTS

L17CF1:
/* 17CF1: 4C B1 BE */ JMP RemoveObject                                    ; $BEB1

AI_Object38:
/* 17CF4: 20 49 BE */ JSR EnemyAI_MovementsAndDamageCheck
/* 17CF7: BD 40 04 */ LDA ObjectUnknown440,X
/* 17CFA: C9 30 */    CMP #$30
/* 17CFC: 90 7C */    BCC L17D7A
/* 17CFE: F0 26 */    BEQ L17D26
/* 17D00: C9 70 */    CMP #$70
/* 17D02: B0 39 */    BCS L17D3D
/* 17D04: C9 64 */    CMP #$64
/* 17D06: D0 05 */    BNE L17D0D
/* 17D08: A9 30 */    LDA #$30
/* 17D0A: 9D 40 04 */ STA ObjectUnknown440,X
L17D0D:
/* 17D0D: BD 20 04 */ LDA ObjectFlags,X
/* 17D10: 29 40 */    AND #$40
/* 17D12: F0 09 */    BEQ L17D1D
/* 17D14: BD 80 04 */ LDA ObjectPosX,X
/* 17D17: DD C0 06 */ CMP ObjectLifeMeter,X
/* 17D1A: B0 13 */    BCS L17D2F
/* 17D1C: 60 */       RTS
; same routine continued
L17D1D:
/* 17D1D: BD 80 04 */ LDA ObjectPosX,X
/* 17D20: DD C0 06 */ CMP ObjectLifeMeter,X
/* 17D23: 90 0A */    BCC L17D2F
/* 17D25: 60 */       RTS
L17D26:
/* 17D26: A9 04 */    LDA #$04
/* 17D28: 9D C0 04 */ STA ObjectXSpeed,X
/* 17D2B: 20 C5 E8 */ JSR F1E8C5
/* 17D2E: 60 */       RTS
; same routine continued
L17D2F:
/* 17D2F: BD C0 06 */ LDA ObjectLifeMeter,X
/* 17D32: 9D 80 04 */ STA ObjectPosX,X
/* 17D35: A9 70 */    LDA #$70
/* 17D37: 9D 40 04 */ STA ObjectUnknown440,X
/* 17D3A: 20 45 B0 */ JSR ClearObjectMem
L17D3D:
/* 17D3D: C9 93 */    CMP #$93
/* 17D3F: D0 32 */    BNE L17D73
/* 17D41: BD 40 06 */ LDA ObjectFireDelay,X
/* 17D44: A0 07 */    LDY #$07
L17D46:
/* 17D46: D9 7B BD */ CMP L17D7B,Y
/* 17D49: F0 05 */    BEQ L17D50
/* 17D4B: 88 */       DEY
/* 17D4C: 10 F8 */    BPL L17D46
/* 17D4E: 30 1D */    BMI L17D6D
L17D50:
/* 17D50: 84 0C */    STY $0C
/* 17D52: A9 39 */    LDA #$39
/* 17D54: 20 63 F6 */  JSR CreateEnemy                                       ; $F663
/* 17D57: B0 14 */    BCS L17D6D
/* 17D59: A9 22 */    LDA #$22
/* 17D5B: 9D 20 04 */  STA ObjectFlags,X
/* 17D5E: A0 08 */    LDY #$08
/* 17D60: 20 33 F5 */ JSR InitObjectDefaultSpeed
/* 17D63: A9 78 */    LDA #$78
/* 17D65: 9D 00 06 */ STA ObjectPosY,X
/* 17D68: A5 0C */    LDA $0C
/* 17D6A: 9D 40 06 */ STA ObjectFireDelay,X
L17D6D:
/* 17D6D: A6 2F */    LDX RefObjectNum
/* 17D6F: 20 73 E8 */ JSR F1E873
/* 17D72: 60 */       RTS
; same routine continued
L17D73:
/* 17D73: C9 94 */    CMP #$94
/* 17D75: D0 03 */    BNE L17D7A
/* 17D77: 4C B1 BE */ JMP RemoveObject                                    ; $BEB1
L17D7A:
/* 17D7A: 60 */       RTS

L17D7B:
/* 17D7B: 54 */       .byte $54
/* 17D7C: 64 */       .byte $64
/* 17D7D: 70 84 */    .byte $70,$84
/* 17D7F: 04 */       .byte $04
/* 17D80: 14 */       .byte $14
/* 17D81: 24 99 */    BIT $99

AI_Object39:
/* 17D83: BC 40 06 */ LDY ObjectFireDelay,X
/* 17D86: A5 3F */    LDA BossVariable3F
/* 17D88: D9 90 BD */ CMP L17D90,Y
/* 17D8B: D0 ED */    BNE L17D7A
/* 17D8D: 4C B1 BE */ JMP RemoveObject                                    ; $BEB1

L17D90:
/* 17D90: 01 05 */    ORA ($05,X)
/* 17D92: 09 11 */    ORA #$11
/* 17D94: 15 19 */    ORA $19,X
/* 17D96: 1C */       .byte $1c

/* 17D97: 1C */       .byte $1c

AI_Object3B:
/* 17D98: A5 31 */    LDA CurrentStage
/* 17D9A: C9 07 */    CMP #$07
/* 17D9C: D0 13 */    BNE L17DB1 ; ++                                              ; $BDB1
/* 17D9E: A0 00 */    LDY #$00
/* 17DA0: A5 1B */    LDA ScrollPosScreen
/* 17DA2: C9 1C */    CMP #$1c
/* 17DA4: F0 02 */    BEQ L17DA8 ; +                                               ; $BDA8
/* 17DA6: A0 04 */    LDY #$04
L17DA8: ; +
/* 17DA8: 84 AC */    STY FightingBossNum
/* 17DAA: A9 02 */    LDA #$02
/* 17DAC: 85 3E */    STA BossCurrentStrategy
/* 17DAE: 4C BC BD */ JMP L17DBC
L17DB1: ; ++
/* 17DB1: C9 06 */    CMP #$06
/* 17DB3: F0 0E */    BEQ L17DC3
/* 17DB5: A6 1B */    LDX ScrollPosScreen
/* 17DB7: BC CA BD */ LDY L17DE7-$1d,X
/* 17DBA: D0 EC */    BNE L17DA8
L17DBC:
/* 17DBC: A9 01 */    LDA #$01
/* 17DBE: 85 68 */    STA ForcedInputFlag
/* 17DC0: 4C B1 BE */ JMP RemoveObject                                    ; $BEB1
L17DC3:
/* 17DC3: A9 42 */    LDA #$42
/* 17DC5: 9D 20 04 */ STA ObjectFlags,X

/* 17DC8: A0 CC */    LDY #$cc
/* 17DCA: 20 33 F5 */ JSR InitObjectDefaultSpeed
/* 17DCD: A9 13 */    LDA #$13
/* 17DCF: 9D E0 06 */ STA ObjectType,X      ;Footholder
/* 17DD2: 20 63 F6 */ JSR CreateEnemy                                       ; $F663

/* 17DD5: B0 0F */    BCS L17DE6
/* 17DD7: A9 02 */    LDA #$02
/* 17DD9: 9D 20 04 */ STA ObjectFlags,X
/* 17DDC: A0 CC */    LDY #$cc
/* 17DDE: 20 33 F5 */ JSR InitObjectDefaultSpeed
/* 17DE1: A9 4C */    LDA #$4c
/* 17DE3: 9D 80 04 */ STA ObjectPosX,X
L17DE6:
/* 17DE6: 60 */       RTS


L17DE7:
/* 17DE7: 02 */       .byte $02
/* 17DE8: 03 */       .byte $03
/* 17DE9: 01 05 */    ORA ($05,X)

AI_Object48:
; Load the index of the $49 object from ObjectLifeMeter[]
/* 17DEB: BC C0 06 */ LDY ObjectLifeMeter,X
/* 17DEE: B9 80 04 */ LDA ObjectPosX,Y
/* 17DF1: 85 00 */    STA $00
/* 17DF3: B9 60 04 */ LDA ObjectPosScreen,Y
/* 17DF6: 85 01 */    STA $01
/* 17DF8: B9 00 06 */ LDA ObjectPosY,Y
/* 17DFB: 85 0D */    STA $0D
/* 17DFD: DE 40 06 */ DEC ObjectFireDelay,X
/* 17E00: BD 40 06 */ LDA ObjectFireDelay,X
/* 17E03: 4A */       LSR A
/* 17E04: 4A */       LSR A
/* 17E05: 29 07 */    AND #$07
/* 17E07: A8 */       TAY
/* 17E08: 18 */       CLC
/* 17E09: A5 0D */    LDA $0D
/* 17E0B: 79 FA A4 */ ADC FireShieldYPosTable,Y
/* 17E0E: 9D 00 06 */ STA ObjectPosY,X
/* 17E11: 18 */       CLC
/* 17E12: A5 00 */    LDA $00
/* 17E14: 79 02 A5 */ ADC FireShieldXPosTable,Y
/* 17E17: 9D 80 04 */ STA ObjectPosX,X
/* 17E1A: A5 01 */    LDA $01
/* 17E1C: 79 0A A5 */ ADC FireShieldPosScreenTable,Y
/* 17E1F: 9D 60 04 */ STA ObjectPosScreen,X
/* 17E22: 20 49 BE */ JSR EnemyAI_MovementsAndDamageCheck
/* 17E25: 60 */       RTS


AI_Object4A:
/* 17E26: BD 40 04 */ LDA ObjectUnknown440,X
/* 17E29: 29 F0 */    AND #$f0
/* 17E2B: D0 11 */    BNE L17E3E
/* 17E2D: 9D 40 04 */ STA ObjectUnknown440,X
/* 17E30: 20 49 BE */ JSR EnemyAI_MovementsAndDamageCheck
/* 17E33: A5 2B */    LDA $2B
/* 17E35: D0 01 */    BNE L17E38
/* 17E37: 60 */       RTS



L17E38:
/* 17E38: A9 10 */    LDA #$10
/* 17E3A: 9D 40 04 */ STA ObjectUnknown440,X
/* 17E3D: 60 */       RTS
L17E3E:
/* 17E3E: C9 B0 */    CMP #$b0
/* 17E40: F0 01 */    BEQ L17E43
/* 17E42: 60 */       RTS

L17E43:
/* 17E43: 9D 40 04 */ STA ObjectUnknown440,X
/* 17E46: 4C 5A C0 */ JMP StageClear                                      ; $C05A


EnemyAI_MovementsAndDamageCheck:

/* 17E49: A6 2F */    LDX RefObjectNum

/* 17E4B: A9 00 */    LDA #$00
/* 17E4D: 85 2A */    STA $2A
/* 17E4F: 85 2B */    STA $2B

/* 17E51: BD 20 04 */ LDA ObjectFlags,X
/* 17E54: 29 80 */    AND #$80
/* 17E56: F0 03 */    BEQ L17E5B ; +                                               ; $BE5B
/* 17E58: 20 C2 BE */ JSR AI_ObjectHit                                    ; $BEC2
L17E5B: ; +
/* 17E5B: 9D 80 05 */ STA IssuedSoundsList,X

/* 17E5E: BD A0 06 */ LDA ObjectLifeCycleCounter,X
/* 17E61: D0 13 */    BNE L17E76 ; +++                                             ; $BE76

; if object is facing right then jsr ObjectUpdateMovementRight, else jsr ObjectUpdateMovementLeft
/* 17E63: BD 20 04 */ LDA ObjectFlags,X
/* 17E66: 29 40 */    AND #$40
/* 17E68: F0 06 */    BEQ L17E70 ; +                                               ; $BE70
/* 17E6A: 20 EF 9D */ JSR ObjectUpdateMovementRight
/* 17E6D: 4C 73 BE */ JMP L17E73 ; ++                                              ; $BE73
L17E70: ; +
/* 17E70: 20 6D 9E */ JSR ObjectUpdateMovementLeft
L17E73: ; ++
/* 17E73: 20 D8 98 */ JSR ObjectRelocateHorizontally

; if ObjectPosX,Hi - ScrollPosX,ScrollPosScreen != 0 then remove this object
L17E76: ; +++
/* 17E76: 38 */       SEC
/* 17E77: BD 80 04 */ LDA ObjectPosX,X
/* 17E7A: E5 1A */    SBC ScrollPosX
/* 17E7C: BD 60 04 */ LDA ObjectPosScreen,X
/* 17E7F: E5 1B */    SBC ScrollPosScreen
/* 17E81: F0 03 */    BEQ L17E86 ; +                                               ; $BE86
; If it's too far to be seen, remove it.
/* 17E83: 4C B1 BE */ JMP RemoveObject                                    ; $BEB1
L17E86: ; +

; ObjectPosX,ObjectPosScreen = ScrollPosX,ScrollPosScreen
/* 17E86: BD A0 06 */ LDA ObjectLifeCycleCounter,X
/* 17E89: D0 24 */    BNE L17EAF ; +++                                             ; $BEAF

/* 17E8B: BD 20 04 */ LDA ObjectFlags,X
/* 17E8E: 29 11 */    AND #$11
/* 17E90: D0 11 */    BNE L17EA3 ; +                                               ; $BEA3
; It's not a temporary object, or it doesn't collide with BG.
/* 17E92: 20 A1 9C */ JSR ObjectCheckIfOutScreenVertically
/* 17E95: B0 15 */    BCS L17EAC ; ++                                              ; $BEAC
/* 17E97: A5 24 */    LDA $24
/* 17E99: 9D 20 06 */ STA ObjectPosYfraction,X
/* 17E9C: A5 25 */    LDA $25
/* 17E9E: 9D 00 06 */ STA ObjectPosY,X
/* 17EA1: 90 0C */    BCC L17EAF ; +++                                             ; $BEAF
L17EA3: ; +
/* 17EA3: 20 C4 9B */ JSR ObjectDoCollisionChecksAndAvoidWalls
/* 17EA6: A5 2B */    LDA $2B
/* 17EA8: C9 FF */    CMP #$ff
/* 17EAA: D0 03 */    BNE L17EAF ; +++                                             ; $BEAF
L17EAC: ; ++
/* 17EAC: 4C B1 BE */ JMP RemoveObject                                    ; $BEB1
L17EAF: ; +++
/* 17EAF: 18 */       CLC
/* 17EB0: 60 */       RTS



; if Object# >= 16 then Meters+1,Object# = #$FF

RemoveObject:
/* 17EB1: A6 2F */    LDX RefObjectNum
/* 17EB3: A9 F8 */    LDA #$f8
/* 17EB5: 9D 00 06 */ STA ObjectPosY,X
/* 17EB8: E0 10 */    CPX #$10
/* 17EBA: 90 04 */    BCC L17EC0 ; +                                               ; $BEC0
/* 17EBC: A9 FF */    LDA #$ff
/* 17EBE: 95 6B */    STA Meters+1,X
L17EC0: ; +
/* 17EC0: 38 */       SEC
/* 17EC1: 60 */       RTS


AI_ObjectHit:
/* 17EC2: BD 80 05 */ LDA IssuedSoundsList,X
/* 17EC5: F0 01 */    BEQ L17EC8 ; +                                               ; $BEC8
/* 17EC7: 60 */       RTS
L17EC8: ; +

; Decrease enemy life meter
/* 17EC8: A5 5F */    LDA WeaponSelect
/* 17ECA: 0A */       ASL A
/* 17ECB: A8 */       TAY
/* 17ECC: B9 44 BF */ LDA WeaponDamageAddr,Y                              ; $BF44,Y
/* 17ECF: 85 04 */    STA $04
/* 17ED1: B9 45 BF */ LDA WeaponDamageAddr+1,Y                            ; $BF45,Y
/* 17ED4: 85 05 */    STA $05
/* 17ED6: BC E0 06 */ LDY ObjectType,X

/* 17ED9: 38 */       SEC
/* 17EDA: BD C0 06 */ LDA ObjectLifeMeter,X
/* 17EDD: F1 04 */    SBC ($04),Y
/* 17EDF: 90 08 */    BCC EnemyKilled                                     ; $BEE9
/* 17EE1: F0 06 */    BEQ EnemyKilled                                     ; $BEE9
/* 17EE3: 9D C0 06 */ STA ObjectLifeMeter,X

/* 17EE6: A9 01 */    LDA #$01
/* 17EE8: 60 */       RTS

; EnemyKilled:
;    Y = weapon number*2
;    X = enemy number (object number)
EnemyKilled:
/* 17EE9: BD E0 06 */ LDA ObjectType,X
/* 17EEC: 48 */       PHA
/* 17EED: A8 */        TAY
/* 17EEE: A9 1B */     LDA #$1b
/* 17EF0: 9D E0 06 */  STA ObjectType,X

/* 17EF3: A9 FF */     LDA #$ff
/* 17EF5: 95 6B */     STA Meters+1,X

/* 17EF7: 20 45 B0 */  JSR ClearObjectMem

/* 17EFA: 9D 40 04 */  STA ObjectUnknown440,X
/* 17EFD: 9D 40 06 */  STA ObjectFireDelay,X
/* 17F00: 9D 20 04 */  STA ObjectFlags,X
/* 17F03: 9D A0 06 */  STA ObjectLifeCycleCounter,X

/* 17F06: B9 82 FE */  LDA EnemyKillScoreTable,Y
/* 17F09: 85 05 */     STA $05
/* 17F0B: 20 40 D4 */  JSR IncreaseScore                                   ; $D440
/* 17F0E: 68 */       PLA

/* 17F0F: C9 3A */    CMP #$3a
/* 17F11: F0 26 */    BEQ L17F39

; CREATE A RANDOM DROP

/* 17F13: A9 64 */    LDA #$64
/* 17F15: 20 A0 C5 */ JSR RandomFunc

; $3B = no drop
/* 17F18: A0 3B */    LDY #$3b
/* 17F1A: A2 05 */    LDX #$05
L17F1C: ; -
/* 17F1C: DD 3E BF */ CMP BonusProbabilityTable,X
/* 17F1F: 90 04 */    BCC L17F25 ; +                                               ; $BF25
/* 17F21: C8 */       INY
/* 17F22: CA */       DEX
/* 17F23: 10 F7 */    BPL L17F1C ; -                                               ; $BF1C
L17F25: ; +
/* 17F25: C0 3B */    CPY #$3b
/* 17F27: F0 10 */    BEQ L17F39 ; +                                               ; $BF39
; Spawn the bonus drop
/* 17F29: 98 */       TYA
/* 17F2A: 20 63 F6 */ JSR CreateEnemy                                       ; $F663
/* 17F2D: B0 0A */    BCS L17F39 ; +                                               ; $BF39
/* 17F2F: A9 13 */    LDA #$13
/* 17F31: 9D 20 04 */ STA ObjectFlags,X
/* 17F34: A0 1C */    LDY #$1c
/* 17F36: 20 33 F5 */ JSR InitObjectDefaultSpeed
L17F39: ; +
/* 17F39: A6 2F */    LDX RefObjectNum
/* 17F3B: A9 00 */    LDA #$00
/* 17F3D: 60 */       RTS

BonusProbabilityTable:
.byte 99, 97, 95, 80, 65, 12
; Probabilities for
; 1UP, Big Energy, Big Weapon, Small Energy, Small Weapon, Score Ball

;ROCKMAN 1:
;  none= 00-0B,64-6F       (24/128)
;  bonus=0C-40,70-7F       (69/128)
;  wpn2 =41-4F             (15/128)
;  nrj2 =50-5E             (15/128)
;  wpn10=5F-60             (2/128)
;  nrj10=61-62             (2/128)
;  1up  =63-63             (1/128)
;ROCKMAN 2: (code is at $F241)
;  none =00-2F,61,63, 64-7F
;  wpn2 =30-48
;  nrj2 =49-57
;  wpn10=58-5C
;  nrj10=5D-60
;  1up  =62-62
;  [E]  =nothing (code exists the though)


; 0 = "P" (just the old megablaster)
; 1 = Cutman
; 2 = Iceman
; 3 = Bombman
; 4 = Fireman
; 5 = Elecman
; 6 = Gutsman
; 7 = Magnet beam


WeaponDamageAddr:
.word WeaponDamageP   ; P
.word WeaponDamageC   ; C
.word WeaponDamageI   ; I
.word WeaponDamageB   ; B
.word WeaponDamageF   ; F
.word WeaponDamageE   ; E
.word WeaponDamageG   ; G




F17F52:
/* 17F52: BD 20 04 */ LDA ObjectFlags,X
/* 17F55: 29 7F */    AND #$7f
/* 17F57: 9D 20 04 */ STA ObjectFlags,X
/* 17F5A: A6 45 */    LDX NumIssuedSounds
/* 17F5C: F0 0D */    BEQ L17F6B
/* 17F5E: CA */       DEX
/* 17F5F: A9 1B */    LDA #$1b
L17F61:
/* 17F61: DD 80 05 */ CMP IssuedSoundsList,X
/* 17F64: F0 05 */    BEQ L17F6B
/* 17F66: CA */       DEX
/* 17F67: 10 F8 */    BPL L17F61
/* 17F69: A6 45 */    LDX $45
L17F6B:
/* 17F6B: A9 1D */    LDA #$1d
/* 17F6D: 9D 80 05 */ STA IssuedSoundsList,X
/* 17F70: E8 */       INX
/* 17F71: 86 45 */    STX NumIssuedSounds
/* 17F73: A6 2F */    LDX RefObjectNum
/* 17F75: 60 */       RTS



L17F76: ;; empty space to the end of the bank, filled with $00
    .res 138, 0
