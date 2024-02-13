; Common include file defining macros and global variables

.feature c_comments

/*
	build.inc is normally created by the makefile and must define:
	- SRC_ROM: A string containing the path of the original ROM to include data banks from
	- J_VERSION: If defined, builds the Japan version; if not defined, builds the US version
*/
.include "build.inc"

BANK_SIZE = $4000

.define SRC_OFFS(bank, offs) ((BANK_SIZE * (bank)) + (offs) + $10)
.define SRC_BOFFS(bank) SRC_OFFS (bank), 0

.macro inc_bank_part bank_idx, offs, size
	.incbin SRC_ROM, SRC_OFFS bank_idx, offs, size
.endmacro

.macro inc_bank_range bank_idx, start_offs, end_offs
	.incbin SRC_ROM, SRC_OFFS (bank_idx), (start_offs), (end_offs) - (start_offs)
.endmacro

.macro inc_banks start_bank, end_bank, new_start_bank
	.ifnblank new_start_bank
	tgt_bank .set (new_start_bank)
	.else
	tgt_bank .set (start_bank)
	.endif

	.repeat (end_bank) - (start_bank), bank_idx
	.segment .sprintf("BANK%X", bank_idx + tgt_bank)
	;.out .sprintf("BANK%X", bank_idx + tgt_bank)
	.incbin SRC_ROM, SRC_BOFFS bank_idx + (start_bank), BANK_SIZE
	.endrepeat
.endmacro

.define NEG(value) ($100 - (value))

; $420 = MD?L?tRT
;
;    M = 1 if moving, 0 if still
;    D = direction (0 = facing left, 1 = facing right)
;    ? = ?
;    L = on ladder
;    ?A = set when Megaman is getting off ladder at top
;    t = set when throwing something while standing
;    R = set to make Megaman get stunned by Gutsman's eart-shaking jump
;    T = set to make Megaman transform
;
;    R+T both set = megaman getting hit
;
; for enemies it works as follows
;
; $420,X = HDIT?BMC  (ObjectFlags)
;
;   H = object getting hit by Megaman's bullet
;   D = direction (0 = facing left, 1 = facing right)
;   I = set if invisible
;   T = Temporary object that disappears after some time (bonus items etc)
;   ? = ?
;   B = set if object can collide with Megaman's bullets (ghostlike otherwise)
;   M = set if object can collide with Megaman (ghostlike otherwise)
;   C = set if object collides with BG
;
; freezing $640 for enemies freezes them in a certain state...
; freezing $440 for enemies freezes them in a certain frame...
;
; MegamanSpriteNum ($400):
;
; These are sprite frames.
; These values correspond to what Megaman does.
; If you set it to a weird value like $1A, it will appear as a shot instead.
; Examples of these weird values are shown in MegamanWeaponFireData.
; It is the same for ObjectSpriteNum and MegamanSpriteNum.
;
; #$00 = 00000000 = standing
; #$01 = 00000001 = firing megablaster (while standing)
; #$02 = 00000010 = standing while carrying gutsman rock
; #$03 = 00000011 = moving slowly (also causes slow movement!)
; #$04 = 00000100 = firing megablaster (while standing)
; #$05 = 00000101 = moving slowly while carrying gutsman rock
; #$06 = 00000110 = running
; #$07 = 00000111 = running while firing megablaster
; #$08 = 00000100 = running while carrying gutsman rock
; #$09 = 00001001 = jumping/falling
; #$0A = 00001010 = firing megablaster while jumping
; #$0B = 00001011 = jumping while carrying gutsman rock
; #$0C = 00001100 = slowing down after running (also causes movement)
; #$0F = 00001111 = hitting ground
; #$10 = 00010000 = walking with gun
; #$12 = 00010010 = getting hit by enemy
; #$13 = 00010011 = Getting stun by Gutsman's earth-shaking jump
; #$14 = 00010100 = transforming (when entering stage or after pause)
; #$15 = 00010101 = On ladder
; #$16 = 00010110 = Firing while on ladder
; #$17 = 00010111 = Getting off at top of ladder
; #$18 = 00011000 = Also firing on the ladder?
; #$19 = 00011001 = Getting damage (sweatdrops)
; #$31 = 00110001 = "READY"
; #$6E = 01101110 = Throwing something
; #$6F = 01101111 = Throwing something while jumping/falling
; #$70 = 01110000 = Throwing something while on ladder
;
;
;#$C0 = "P"
;#$C1 = Cutman
;#$C2 = Iceman
;#$C3 = Bombman
;#$C4 = Fireman(?)
;#$C5 = Elecman
;#$C6 = Gutsman (rock either carried by megaman or airborne)
;#$C7 = Magnet beam
;

; $41 = #1 if game paused with SELECT, #0 otherwise



; New weapon capsule uses $61F

MiscCounter1                := $3C ;Counter used for various purposes



; RLDUSsBA     (s = select, S = START)

; Keys held down on this frame
JoyPads                     := $14
JoyPad0                     := JoyPads
JoyPad1                     := JoyPads+1

JoyPadsold                  := $16
JoyPad0old                  := JoyPadsold
JoyPad1old                  := JoyPadsold+1

ForcedInputFlag             := $68
ForcedInputData             := $69

; Keys pressed on this frame
JoyD                        := $18
JoyD0                       := JoyD
JoyD1                       := JoyD+1


ScrollPosX                  := $1A
ScrollPosScreen             := $1B
ScrollPosY                  := $1E

; $0D is used for plenty of purposes. Here's one.
SpriteCounter               := $0D

NMI_GfxUpdateDone           := $1F ;in Rockman2, at $1D
FrameCounter                := $23 ;In Rockman2, at $1C

RefObjectNum                := $2F

NumberOfFramesSinceShooting := $61

WritePaletteParam           := $34
PaletteUpdateDelay          := $37

LastRestartPointType        := $AB  ;0=level beginning, #$0C = point A, #$18 = point B

PreviousEnemyIndex          := $8B
CurrentEnemyIndex           := $8C

ZigZagFireStatus            := $8F
AutoSpawnObjectFFcounter    := $92 ;unknown

CurrentBeginScreen          := $27 ;In Rockman2, $14
        ; ^The screen that begins the current strip. Only updated in transitions.
CurrentEndScreen            := $28 ;In Rockman2, $15
        ; ^The screen where the current strip ends. Only updated in transitions.
CurrentOrderNum             := $29 ;In Rockman2, $38
        ; ^Pointer to the order table. Only updated in transitions.

ObjectReceivedHitType       := $53

UseTempScrollX              := $B4
TempScrollPosX              := $B2
TempScrollPosScreen         := $B3

ScreenMovedFlag   := $8D
;Bit 0: (&01) = New enemies may need to be loaded (screen has moved)
;Bit 6: (&40) = Movement forward? 0=backward

ActivesLowerIndex := $8E
;ActivesLowerIndex points to the first Active that may need to be cared of.
;Actives before that index are of no concern.

RoomActiveTable             := $720

IgnoreStageSelection        := $BC

SoundCodeParameter          := $A7

CurrentRoomMonsterGraphicsIndex := $7A


BonusPearlCount             := $AE ; Max: 99

RandomSeed                  := $46 ; In Rockman2, at $4A

LevelClearScoreInThousands  := $3D

CurrentBank                 := $42


ExtraLives                  := $A6



DrawScoreAndMetersFlag      := $BB


WeaponsOwned                := $5D               ; (MFCIGEB0)
WeaponSelect                := $5F

Meters                      := $6A            ; (from 0-28)
; 0 = "P" (just the old megablaster)
; 1 = Cutman
; 2 = Iceman
; 3 = Bombman
; 4 = Fireman
; 5 = Elecman
; 6 = Gutsman
; 7 = Magnet beam
MetersFraction              := $9E  ; (index 1 not used)


WeaponFiring                := $60
; $60 = non-zero if any of Megaman's weapons are at the screen
; &7 = weapon number
; |$40 = phase A
; |$80 = phase B
; Usually both phases are used at same time, but Gutsblock has two phases.


TotalObjects                := $54

ObjectSpriteNum             := $400
ObjectFlags                 := $420 ;also used for boss moves in stage select
ObjectUnknown440            := $440 ; low 4 bits: >0 if taken damage this frame
                                   ; high 4 bits: possibly invulnerability timer?
ObjectPosScreen             := $460 ; aka. screen ID
ObjectPosX                  := $480
ObjectPosXfraction          := $4A0 ;Xpos fractional part
ObjectXSpeed                := $4C0
ObjectXSpeedFraction        := $4E0
ObjectPosY                  := $600
ObjectPosYfraction          := $620
ObjectFireDelay             := $640 ;uncertain!! but delays objects somehow
ObjectYSpeedFraction        := $660
ObjectYSpeed                := $680
ObjectLifeCycleCounter      := $6A0
ObjectLifeMeter             := $6C0
ObjectType                  := $6E0 ; enemy ID

LiftIndex         := $9A ;number of active lifts
LiftUnknown9B     := $9B ;not table
LiftXSpeedFraction:= $9C ;not table
LiftXSpeed        := $9D ;not table
LiftUnknown9E     := $9E ;not table
LiftPosScreen     := $700 ;table
LiftPosX          := $708 ;table
LiftPosY          := $710 ;table
LiftDirection     := $718 ;table

LiftXMovement       := $06 ;(temporary meaning..)
LiftXScreenMovement := $07


MagnetBeamLength        := $5A0
MagnetBeamAge           := $5B0 ;0:=not active, $9C := initial age
MagnetBeamPosX          := $5C0
MagnetBeamPosScreen     := $5D0
MagnetBeamCyclePos      := $5E0
MagnetBeamPosY          := $5F0

IssuedSoundsList        := $580
NumIssuedSounds         := $45

MegamanBlinkState           := $55       ; Timer: 0 = not blinking. #$6F = max
BossBlinkState              := $56

SpriteTable             := $0200
CurrentSpriteData       := SpriteTable+4 ; (array of 4-byte records)
; Structure:
;   0 = Y coordinate
;   1 = Tile number
;   2 = Sprite attributes
;   3 = X coordinate


RawPPUtransferSize      := $5E
RawPPUtransferAddress   := $0380
RawPPUtransferBuf       := $0382

TSAPPUtransferSize      := $1C
TSAPPUtransfer0NTaddress   := $0300
TSAPPUtransfer0NTdata      := $0302
TSAPPUtransfer0AttrAddress := $0312
TSAPPUtransfer0AttrData    := $0314
TSAPPUtransfer0AttrAndMask := $0314
TSAPPUtransfer0AttrOrMask  := $0315

PPU2000value                       := $FF
PPU2001value                       := $FE


SpritePalettes  := $03E0
BGPalettes      := $03D0
UnknownPalettes := $03F0

GutsmanStompCounter     := $47
MegamanWalkTimer        := $94 ;perhaps this meaning.
MegamanStallTimer       := $95 ;perhaps this meaning.

CurrentTileState        := $30
;CurrentTileState bits are:
;  #$10: (bit 4) - Rockman is climbing up
;  #$08: (bit 3) - Ladder above that
;  #$04: (bit 2) - Ladder above
;  #$02: (bit 1) - Ladder here
;  #$01: (bit 0) - there is a ladder below this tile
;
; If both bits 2,3 are clear, this is a ladder top (or not ladder)
; If the value is #$01, it's a ladder top


CurrentStage            := $31
BossCurrentStrategy     := $3E ;doubles as the boss number
BossVariable3F          := $3F ;unknown meaning
BossVariable43          := $43 ; unknown meaning, indicates strategy somehow
BossVariable44          := $44 ; counter of some kind

FightingBossNum         := $AC
; $AC = boss we're fighting (value gets set at boss fight, invalid otherwise)
;   0 = Cutman
;   1 = Iceman
;   2 = Bombman
;   3 = Fireman
;   4 = Elecman(?)
;   5 = Gutsman
;   6 = Stone giant


CurrentRoomPointer      := $06 ;two 16-bit pointers, occupying 06 07 08 09
CurrentStripeEndType    := $26
; &3:
;   0=right
;   1=up
;   2=left
;   3=down


CapsuleObtained             := $AD
TeleportEnteredFlag         := $B0

GutsmanWeaponTargetActive := $52

; In weapon select menu (accessed by START button),
; $5F gets set as follows:

;
; 0 = "P"
; 1 = Bombman
; 2 = Elecman
; 3 = Gutsman
; 4 =
; 5 = Cutman
; 6 =
; 7 = Magnet beam

Score                       := $72

RoomBlockData        := $8000
RoomBlockPals        := $8300
RoomOrderTable       := $8C00
RoomPointerTable     := $8C30
RoomLayoutTable      := $8C70
RoomSpritePalettes1  := $8CA0 ;normal stages
RoomSpritePalettes2  := $8CD0 ;wily stages
RoomTileTable1       := $8D00 ;normal stages
RoomTileTable2       := $8D20 ;wily stages
RoomMonsterIndex     := $8D40 ;something common to both
RoomMonsterGraphics  := $8D80 ;something common to both
RoomActives1         := $8E00 ;normal stages
RoomActives2         := $8EA0 ;wily stages
RoomShutterBlockData := $8F40
RoomShutterBlockPals := $8F70
RoomShutterInfo      := $8F80

; Graphics bank (0-3) stuff to be identified. These MUST be page-aligned.
LxB000 := $B000
LxB200 := $B200
LxB700 := $B700
LxB800 := $B800
LxB900 := $B900
LxBB00 := $BB00

; Title/continue screen graphics data
L10EA1 := $8EA1
L10EC7 := $8EC7
L10ECF := $8ECF

.ifdef J_VERSION
	L10F03 := $8F03
	L10F07 := $8F07
	L10F12 := $8F12
	L10F1D := $8F1D
	L10EDF := $8EDF
	L10EEF := $8EEF
	L10EFF := $8EFF
.else
	L10ED6 := $8ED6
	L10EDD := $8EDD
	L10EE4 := $8EE4
.endif

; Bank 4 exports

.global SoundCodeInit, SoundCodePlay

; Bank 5 exports

.global Reset2, L1505A, L1506A, StageBegin, StageBeginFromDeath, GameOver, RunEnemyAI, DoWeaponSelectDialog, ObjectUpdateMovementLeft, AutoCenterScreen, L1515E, ReplaceObjectWithExplosion, ObjectMoveToTheRight, ObjectMoveToTheLeft, ObjectRelocateHorizontally, EnemyAI_MovementsAndDamageCheck, LaunchMegamanWeaponShot, ObjectDoCollisionChecksAndAvoidWalls

; Bank 6 exports

.global Lbl_bff0, MajObjFrameAddrs, MajObjFrameTableAddrs, TileOffsTableAddrs, SpriteTileYOffs, SpriteTileLeftXOffs, SpriteTileRightXOffs, Lbl_863a, Lbl_8644
.global Lbl_864a, MinObjFrameAddrs, MinObjFrameTableAddrs, DefaultObjectFlags
.global DefaultObjectSpeedCtrl, DefaultObjectFireDelay, EnemyDataPointers
.global RunStageSelectionScreen

; Bank 7 exports

.global WriteChr, InitStagePaletteAndActives, DrawAtReset, ExecStageSelectionScreen, FirstScreenScreenTable, RoomLayoutLoadRoomNum, HideSprites, IssueSound, InitEndGameScene, NextFrame, TeleportToStage, InvokeWeaponSelectDialog, RecalculateActivesLowerIndex, RunBossAI, LoadEnemies, UpdateGraphics, LifeCycleUntick_forEveryone, MegaManKilled, SetupEnemyGraphicsPointer, PaletteSetupForBG, OpenFirstDoor, OpenSecondDoor, PaletteSetupForBGwith3F0, TimeDelayWithSpriteUpdates, LifeCycleTick_forEnemies, UpdateCurrentTileState, FindFreeObject, InitObjectDefaultSpeed, InitActor, C53D_routine, CreateGutsblockPieces, F1F853, CreateExplosionObject, TableObjectYHeightTable1, TableObjectYHeightTable2, DoCollisionCheckFor, F1C55D, TableObjectXWidthTable1, TableObjectXWidthTable2, ObjectVerifyBackgroundCollision, DrawBlockFromActiveLevelMap, LoadEnemyGraphics, F1CF23, DoTSAPPUtransfer, LoadActiveByIndexAndSetBlockingness, F1C64A, TestCollisionWithMegaman, GotItem, F1C7C5, L1FEEF, EnemyHitTable, TestShotHit, C7E4_routine, F1F7B5, RoutineF6FD_GutsmanWeapon, F8C6_routine, CheckCollisionAgainstActives, GutsblockHitByElecBeam, CheckMagnetBeamCollisionWithBG, LoadGutsblockPosition, AI_Object32, AI_Object3A, EnemySearchMegaman, EnemyCalculateJumpCurveToHitMegaman, CreateEnemy, RandomFunc, FindObjectOfSelectedType, F1C63F, F1F628, F1E8C5, F1E873, StageClear, EnemyKillScoreTable, IncreaseScore, WeaponDamageP, WeaponDamageC, WeaponDamageI, WeaponDamageB, WeaponDamageF, WeaponDamageE, WeaponDamageG
.global F1C436, F1C442, TeleportToStage_Bank06callback, SpawnObject_TypeIsFF, F1C439, Lbl_c45e, CalculateNametableAddress, Adjust32x32BlockAddress, DrawBlockFromActiveLevelMap_Bank06callback, F1C400, Lbl_c3e3, MegamanPalette, PaletteSetupForSprites, WritePalette, DrawObject
