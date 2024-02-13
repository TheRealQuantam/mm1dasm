.include "globals.inc"

/*
	MEGAMAN 1 BANK 6 ROM MAP

	This bank's disassembly is from Mega Man (U)
*/

.segment "BANK6"

; Major object metasprite frame data addresses
; Unless the assembly is modified, this table MUST be located such that addr & $100 == 0. See 1D1F5.
MajObjFrameAddrs: ; 8000
	.word  MajObj_Frame_0,  MajObj_Frame_1,  MajObj_Frame_2,  MajObj_Frame_3
	.word  MajObj_Frame_4,  MajObj_Frame_5,  MajObj_Frame_6,  MajObj_Frame_7
	.word  MajObj_Frame_8,  MajObj_Frame_9,  MajObj_Frame_A,  MajObj_Frame_B
	.word  MajObj_Frame_C,  MajObj_Frame_D,  MajObj_Frame_E,  MajObj_Frame_F
	.word MajObj_Frame_10, MajObj_Frame_11, MajObj_Frame_12, MajObj_Frame_13
	.word MajObj_Frame_14, MajObj_Frame_15, MajObj_Frame_16, MajObj_Frame_17
	.word MajObj_Frame_18, MajObj_Frame_19, MajObj_Frame_1A, MajObj_Frame_1B
	.word MajObj_Frame_1C, MajObj_Frame_1D, MajObj_Frame_1E, MajObj_Frame_1F
	.word MajObj_Frame_20, MajObj_Frame_21, MajObj_Frame_22, MajObj_Frame_23
	.word MajObj_Frame_24, MajObj_Frame_25, MajObj_Frame_26, MajObj_Frame_27
	.word MajObj_Frame_28, MajObj_Frame_29, MajObj_Frame_2A, MajObj_Frame_2B
	.word MajObj_Frame_2C, MajObj_Frame_2D, MajObj_Frame_2E, MajObj_Frame_2F
	.word MajObj_Frame_30, MajObj_Frame_31, MajObj_Frame_32, MajObj_Frame_33
	.word MajObj_Frame_34, MajObj_Frame_35, MajObj_Frame_36, MajObj_Frame_37
	.word MajObj_Frame_38, MajObj_Frame_39, MajObj_Frame_3A, MajObj_Frame_3B
	.word MajObj_Frame_3C, MajObj_Frame_3D, MajObj_Frame_3E, MajObj_Frame_3F
	.word MajObj_Frame_40, MajObj_Frame_41, MajObj_Frame_42, MajObj_Frame_43
	.word MajObj_Frame_44, MajObj_Frame_45, MajObj_Frame_46, MajObj_Frame_47
	.word MajObj_Frame_48, MajObj_Frame_49, MajObj_Frame_4A, MajObj_Frame_4B
	.word MajObj_Frame_4C, MajObj_Frame_4D, MajObj_Frame_4E, MajObj_Frame_4F
	.word MajObj_Frame_50, MajObj_Frame_51, MajObj_Frame_52, MajObj_Frame_53
	.word MajObj_Frame_54, MajObj_Frame_55, MajObj_Frame_56, MajObj_Frame_57
	.word MajObj_Frame_58, MajObj_Frame_59, MajObj_Frame_5A, MajObj_Frame_5B
	.word MajObj_Frame_5C, MajObj_Frame_5D, MajObj_Frame_5E, MajObj_Frame_5F
	.word MajObj_Frame_60, MajObj_Frame_61, MajObj_Frame_62, MajObj_Frame_63
	.word MajObj_Frame_64, MajObj_Frame_65, MajObj_Frame_66, MajObj_Frame_67
	.word MajObj_Frame_68, MajObj_Frame_69, MajObj_Frame_6A, MajObj_Frame_6B
	.word MajObj_Frame_6C, MajObj_Frame_6D, MajObj_Frame_6E, MajObj_Frame_6F
	.word MajObj_Frame_70, MajObj_Frame_71, MajObj_Frame_72, MajObj_Frame_73
	.word MajObj_Frame_74, MajObj_Frame_75, MajObj_Frame_76, MajObj_Frame_77
	.word MajObj_Frame_78, MajObj_Frame_79, MajObj_Frame_7A, MajObj_Frame_7B
	.word MajObj_Frame_7C, MajObj_Frame_7D, MajObj_Frame_7E, MajObj_Frame_7F
	.word MajObj_Frame_80, MajObj_Frame_81, MajObj_Frame_82, MajObj_Frame_83
	.word MajObj_Frame_84, MajObj_Frame_85, MajObj_Frame_86, MajObj_Frame_87
	.word MajObj_Frame_88, MajObj_Frame_89, MajObj_Frame_8A, MajObj_Frame_8B
	.word MajObj_Frame_8C, MajObj_Frame_8D, MajObj_Frame_8E, MajObj_Frame_8F
	.word MajObj_Frame_90, MajObj_Frame_91, MajObj_Frame_92, MajObj_Frame_93
	.word MajObj_Frame_94, MajObj_Frame_95, MajObj_Frame_96, MajObj_Frame_97
	.word MajObj_Frame_98, MajObj_Frame_99, MajObj_Frame_9A, MajObj_Frame_9B
	.word MajObj_Frame_9C, MajObj_Frame_9D, MajObj_Frame_9E, MajObj_Frame_9F
	.word MajObj_Frame_A0, MajObj_Frame_A1, MajObj_Frame_A2, MajObj_Frame_A3
	.word MajObj_Frame_A4, MajObj_Frame_A5, MajObj_Frame_A6, MajObj_Frame_A7
	.word MajObj_Frame_A8, MajObj_Frame_A9, MajObj_Frame_AA, MajObj_Frame_AB
	.word MajObj_Frame_AC, MajObj_Frame_AD, MajObj_Frame_AE, MajObj_Frame_AF
	.word MajObj_Frame_B0, MajObj_Frame_B1, MajObj_Frame_B2, MajObj_Frame_B3
	.word MajObj_Frame_B4, MajObj_Frame_B5, MajObj_Frame_B6, MajObj_Frame_B7
	.word MajObj_Frame_B8, MajObj_Frame_B9, MajObj_Frame_BA, MajObj_Frame_BB
	.word MajObj_Frame_BC, MajObj_Frame_BD, MajObj_Frame_BE, MajObj_Frame_BF
	.word MajObj_Frame_C0, MajObj_Frame_C1, MajObj_Frame_C2

; Major object metasprite state frame index table addresses
MajObjFrameTableAddrs: ; 8186
	.word  MajObj_FrameIdcs_0,  MajObj_FrameIdcs_1
	.word  MajObj_FrameIdcs_2,  MajObj_FrameIdcs_3
	.word  MajObj_FrameIdcs_4,  MajObj_FrameIdcs_5
	.word  MajObj_FrameIdcs_6,  MajObj_FrameIdcs_7
	.word  MajObj_FrameIdcs_8,  MajObj_FrameIdcs_9
	.word  MajObj_FrameIdcs_A,  MajObj_FrameIdcs_B
	.word  MajObj_FrameIdcs_C,  MajObj_FrameIdcs_D
	.word  MajObj_FrameIdcs_E,  MajObj_FrameIdcs_F
	.word MajObj_FrameIdcs_10, MajObj_FrameIdcs_11
	.word MajObj_FrameIdcs_12, MajObj_FrameIdcs_13
	.word MajObj_FrameIdcs_14, MajObj_FrameIdcs_15
	.word MajObj_FrameIdcs_16, MajObj_FrameIdcs_17
	.word MajObj_FrameIdcs_18, MajObj_FrameIdcs_19
	.word MajObj_FrameIdcs_1A, MajObj_FrameIdcs_1B
	.word MajObj_FrameIdcs_1C, MajObj_FrameIdcs_1D
	.word MajObj_FrameIdcs_1E, MajObj_FrameIdcs_1F
	.word MajObj_FrameIdcs_20, MajObj_FrameIdcs_21
	.word MajObj_FrameIdcs_22, MajObj_FrameIdcs_23
	.word MajObj_FrameIdcs_24, MajObj_FrameIdcs_25
	.word MajObj_FrameIdcs_26, MajObj_FrameIdcs_27
	.word MajObj_FrameIdcs_28, MajObj_FrameIdcs_29
	.word MajObj_FrameIdcs_2A, MajObj_FrameIdcs_2B
	.word MajObj_FrameIdcs_2C, MajObj_FrameIdcs_2D
	.word MajObj_FrameIdcs_2E, MajObj_FrameIdcs_2F
	.word MajObj_FrameIdcs_30, MajObj_FrameIdcs_31
	.word MajObj_FrameIdcs_32, MajObj_FrameIdcs_33
	.word MajObj_FrameIdcs_34, MajObj_FrameIdcs_35
	.word MajObj_FrameIdcs_36, MajObj_FrameIdcs_37
	.word MajObj_FrameIdcs_38, MajObj_FrameIdcs_39
	.word MajObj_FrameIdcs_3A, MajObj_FrameIdcs_3B
	.word MajObj_FrameIdcs_3C, MajObj_FrameIdcs_3D
	.word MajObj_FrameIdcs_3E, MajObj_FrameIdcs_3F
	.word MajObj_FrameIdcs_40, MajObj_FrameIdcs_41
	.word MajObj_FrameIdcs_42, MajObj_FrameIdcs_43
	.word MajObj_FrameIdcs_44, MajObj_FrameIdcs_45
	.word MajObj_FrameIdcs_46, MajObj_FrameIdcs_47
	.word MajObj_FrameIdcs_48, MajObj_FrameIdcs_49
	.word MajObj_FrameIdcs_4A, MajObj_FrameIdcs_4B
	.word MajObj_FrameIdcs_4C, MajObj_FrameIdcs_4D
	.word MajObj_FrameIdcs_4E, MajObj_FrameIdcs_4F
	.word MajObj_FrameIdcs_50, MajObj_FrameIdcs_51
	.word MajObj_FrameIdcs_52, MajObj_FrameIdcs_53
	.word MajObj_FrameIdcs_54, MajObj_FrameIdcs_55
	.word MajObj_FrameIdcs_56, MajObj_FrameIdcs_57
	.word MajObj_FrameIdcs_58, MajObj_FrameIdcs_59
	.word MajObj_FrameIdcs_5A, MajObj_FrameIdcs_5B
	.word MajObj_FrameIdcs_5C, MajObj_FrameIdcs_5D
	.word MajObj_FrameIdcs_5E, MajObj_FrameIdcs_5F
	.word MajObj_FrameIdcs_60, MajObj_FrameIdcs_61
	.word MajObj_FrameIdcs_62, MajObj_FrameIdcs_63
	.word MajObj_FrameIdcs_64, MajObj_FrameIdcs_65
	.word MajObj_FrameIdcs_66, MajObj_FrameIdcs_67
	.word MajObj_FrameIdcs_68, MajObj_FrameIdcs_69
	.word MajObj_FrameIdcs_6A, MajObj_FrameIdcs_6B
	.word MajObj_FrameIdcs_6C, MajObj_FrameIdcs_6D
	.word MajObj_FrameIdcs_6E, MajObj_FrameIdcs_6F
	.word MajObj_FrameIdcs_70, MajObj_FrameIdcs_71
	.word MajObj_FrameIdcs_72, MajObj_FrameIdcs_73
	.word MajObj_FrameIdcs_74, MajObj_FrameIdcs_75
	.word MajObj_FrameIdcs_76, MajObj_FrameIdcs_77
	.word MajObj_FrameIdcs_78, MajObj_FrameIdcs_79
	.word MajObj_FrameIdcs_7A, MajObj_FrameIdcs_7B
	.word MajObj_FrameIdcs_7C, MajObj_FrameIdcs_7D
	.word MajObj_FrameIdcs_7E, MajObj_FrameIdcs_7F
	.word MajObj_FrameIdcs_80, MajObj_FrameIdcs_81
	.word MajObj_FrameIdcs_82, MajObj_FrameIdcs_83
	.word MajObj_FrameIdcs_84, MajObj_FrameIdcs_85
	.word MajObj_FrameIdcs_86, MajObj_FrameIdcs_87
	.word MajObj_FrameIdcs_88, MajObj_FrameIdcs_89
	.word MajObj_FrameIdcs_8A, MajObj_FrameIdcs_8B
	.word MajObj_FrameIdcs_8C, MajObj_FrameIdcs_8D
	.word MajObj_FrameIdcs_8E, MajObj_FrameIdcs_8F
	.word MajObj_FrameIdcs_90, MajObj_FrameIdcs_91

; Metasprite tile offset (shape) index table addresses
TileOffsTableAddrs: ; 82AA
	.word  TileOffsTable_0,  TileOffsTable_1
	.word  TileOffsTable_2,  TileOffsTable_3
	.word  TileOffsTable_4,  TileOffsTable_5
	.word  TileOffsTable_6,  TileOffsTable_7
	.word  TileOffsTable_8,  TileOffsTable_9
	.word  TileOffsTable_A,  TileOffsTable_B
	.word  TileOffsTable_C,  TileOffsTable_D
	.word  TileOffsTable_E,  TileOffsTable_F
	.word TileOffsTable_10, TileOffsTable_11
	.word TileOffsTable_12, TileOffsTable_13
	.word TileOffsTable_14, TileOffsTable_15
	.word TileOffsTable_16, TileOffsTable_17
	.word TileOffsTable_18, TileOffsTable_19
	.word TileOffsTable_1A, TileOffsTable_1B
	.word TileOffsTable_1C, TileOffsTable_1D
	.word TileOffsTable_1E, TileOffsTable_1F
	.word TileOffsTable_20, TileOffsTable_21
	.word TileOffsTable_22, TileOffsTable_23
	.word TileOffsTable_24, TileOffsTable_25
	.word TileOffsTable_26, TileOffsTable_27
	.word TileOffsTable_28, TileOffsTable_29
	.word TileOffsTable_2A, TileOffsTable_2B
	.word TileOffsTable_2C, TileOffsTable_2D
	.word TileOffsTable_2E, TileOffsTable_2F
	.word TileOffsTable_30, TileOffsTable_31
	.word TileOffsTable_32, TileOffsTable_33
	.word TileOffsTable_34, TileOffsTable_35
	.word TileOffsTable_36, TileOffsTable_37
	.word TileOffsTable_38, TileOffsTable_39
	.word TileOffsTable_3A, TileOffsTable_3B
	.word TileOffsTable_3C, TileOffsTable_3D
	.word TileOffsTable_3E, TileOffsTable_3F
	.word TileOffsTable_40, TileOffsTable_41
	.word TileOffsTable_42, TileOffsTable_43
	.word TileOffsTable_44,            $0000
	.word            $0000,            $0000

; Y components of metasprite tile offsets
SpriteTileYOffs:
    .byte $f4,$f4,$f4,$fc,$fc,$fc,$04,$04,$04,$fa,$f4,$f4,$fc,$fc,$fc,$fc
    .byte $04,$04,$04,$fc,$f5,$f5,$04,$0c,$fb,$f8,$fa,$0c,$fb,$fb,$fb,$fa
    .byte $ec,$0c,$0c,$ec,$e8,$ec,$f4,$ec,$ec,$ec,$ec,$ec,$ed,$ed,$ec,$ec
    .byte $fd,$fc,$0c,$fc,$08,$fc,$0c,$f4,$f4,$fc,$fc,$fc,$04,$04,$04,$fb
    .byte $f9,$f9,$fb,$fb,$f9,$fd,$fd,$fa,$fa,$f4,$f8,$f9,$fc,$f7,$f7,$f8
    .byte $f7,$f0,$f0,$f0,$f0,$f8,$f8,$f8,$00,$00,$00,$00,$08,$08,$08,$08
    .byte $f3,$f3,$f3,$f3,$fb,$03,$03,$03,$03,$0b,$0b,$0b,$0b,$f0,$f0,$f0
    .byte $f0,$f8,$f8,$f8,$f8,$00,$00,$00,$00,$f7,$fa,$ec,$ec,$ed,$ed,$ee
    .byte $ee,$ef,$ef,$f4,$ed,$f3,$f3,$e4,$e4,$ec,$ec,$ed,$eb,$eb,$eb,$eb
    .byte $e3,$e3,$e3,$e3,$db,$db,$db,$db,$d3,$d3,$d3,$d3,$0c,$08,$08,$08
    .byte $08,$f5,$f8,$00,$04,$f8,$f8,$f8,$f8,$f8,$00,$00,$00,$00,$f0,$f0
    .byte $00,$00,$f8,$fc,$fc,$f8,$f8,$f8,$00,$00,$00,$f4,$f4,$f4,$f4,$fc
    .byte $fc,$fc,$fc,$04,$04,$04,$04,$f9,$f0,$e8,$10,$08,$f9,$f8,$00,$f8
    .byte $00,$f8,$00,$f8,$00,$f4,$04,$0c,$ec,$e4,$e4,$e4,$e4,$ec,$0c,$0c
    .byte $10,$10,$18,$18,$e0,$e0,$e8,$e8,$e0,$e0,$e0,$e0,$e8,$e8,$e8,$e8
    .byte $e8,$f0,$f0,$f0,$08,$08,$08,$10,$10,$10,$10,$10,$18,$18,$18,$18

; X components of metasprite tile offsets for left-facing sprites
SpriteTileLeftXOffs:
    .byte $f4,$fc,$04,$f4,$fc,$04,$f4,$fc,$04,$fb,$f8,$00,$f0,$f8,$00,$08
    .byte $f8,$00,$08,$fb,$f0,$08,$f0,$ff,$08,$f0,$f0,$fe,$f0,$ec,$04,$f7
    .byte $fc,$f8,$00,$f0,$fc,$08,$08,$f9,$01,$04,$f8,$00,$f4,$fc,$f6,$fe
    .byte $ed,$fd,$fc,$0b,$09,$0c,$f2,$f5,$fd,$f5,$fd,$05,$f5,$fd,$05,$fa
    .byte $fc,$f9,$f9,$f8,$ff,$0c,$fc,$fd,$fa,$ec,$fb,$fa,$ec,$fa,$f9,$f6
    .byte $fd,$f0,$f8,$00,$08,$f8,$00,$08,$f0,$f8,$00,$08,$f0,$f8,$00,$08
    .byte $f0,$f8,$00,$08,$00,$f0,$f8,$00,$08,$f0,$f8,$00,$08,$ea,$f2,$fa
    .byte $02,$ea,$f2,$fa,$02,$ea,$f2,$fa,$02,$fc,$fc,$fb,$03,$fc,$04,$f8
    .byte $00,$f6,$fe,$f0,$f0,$f0,$f8,$f2,$fa,$f2,$fa,$f8,$fa,$02,$0a,$12
    .byte $fa,$02,$0a,$12,$fa,$02,$0a,$12,$fa,$02,$0a,$12,$ff,$ea,$f2,$fa
    .byte $02,$04,$fc,$fc,$ec,$ec,$f4,$04,$0c,$14,$ec,$f4,$04,$0c,$ec,$0c
    .byte $e4,$14,$e4,$f9,$01,$f7,$ff,$07,$f7,$ff,$07,$f2,$fa,$02,$0a,$f2
    .byte $fa,$02,$0a,$f2,$fa,$02,$0a,$0a,$fc,$fc,$fc,$fc,$10,$e8,$e8,$10
    .byte $10,$e0,$e0,$18,$18,$0c,$ef,$ef,$f4,$f4,$fc,$04,$0c,$0c,$f4,$04
    .byte $f8,$00,$f8,$00,$f8,$00,$f8,$00,$e8,$f0,$08,$10,$e0,$e8,$f0,$10
    .byte $18,$e0,$e8,$18,$e0,$e8,$18,$e0,$e8,$f0,$10,$18,$e8,$f0,$08,$10

; X components of metasprite tile offsets for right-facing sprites
SpriteTileRightXOffs:
    .byte $04,$fc,$f4,$04,$fc,$f4,$04,$fc,$f4,$fd,$00,$f8,$08,$00,$f8,$f0
    .byte $00,$f8,$f0,$fd,$08,$f0,$08,$f9,$f0,$08,$08,$fa,$08,$0c,$f4,$01
    .byte $fc,$00,$f8,$08,$fc,$f0,$f0,$ff,$f7,$f4,$00,$f8,$04,$fc,$02,$fa
    .byte $0b,$fb,$fc,$ed,$ef,$ec,$06,$03,$fb,$03,$fb,$f3,$03,$fb,$f3,$fe
    .byte $fc,$ff,$ff,$00,$f9,$ec,$fc,$fb,$fe,$0c,$fd,$fe,$0c,$fe,$ff,$02
    .byte $fb,$08,$00,$f8,$f0,$00,$f8,$f0,$08,$00,$f8,$f0,$08,$00,$f8,$f0
    .byte $08,$00,$f8,$f0,$f8,$08,$00,$f8,$f0,$08,$00,$f8,$f0,$0e,$06,$fe
    .byte $f6,$0e,$06,$fe,$f6,$0e,$06,$fe,$f6,$fc,$fc,$fd,$f5,$fc,$f4,$00
    .byte $f8,$02,$fa,$08,$08,$08,$00,$06,$fe,$06,$fe,$00,$fe,$f6,$ee,$e6
    .byte $fe,$f6,$ee,$e6,$fe,$f6,$ee,$e6,$fe,$f6,$ee,$e6,$f9,$0e,$06,$fe
    .byte $f6,$f4,$fc,$fc,$0c,$0c,$04,$f4,$ec,$e4,$0c,$04,$f4,$ec,$0c,$ec
    .byte $14,$e4,$14,$ff,$f7,$01,$f9,$f1,$01,$f9,$f1,$06,$fe,$f6,$ee,$06
    .byte $fe,$f6,$ee,$06,$fe,$f6,$ee,$ee,$fc,$fc,$fc,$fc,$e8,$10,$10,$e8
    .byte $e8,$18,$18,$e0,$e0,$ec,$11,$11,$04,$04,$fc,$f4,$ec,$ec,$04,$f4
    .byte $00,$f8,$00,$f8,$00,$f8,$00,$f8,$10,$08,$f0,$e8,$18,$10,$08,$e8
    .byte $e0,$18,$10,$e0,$18,$10,$e0,$18,$10,$08,$e8,$e0,$10,$08,$f0,$e8

Lbl_863a: ; Used at C564 - gives values for ObjectXSpeed and ObjectXSpeedFraction
    .byte $00,$20,$21,$80,$01,$04,$15,$51,$61,$90

Lbl_8644: ; Used at C549 - sprite number compared to
    .byte $19,$1A,$27,$34,$36,$37

Lbl_864a: ; Used at C556 - gives values for ObjectXSpeed and ObjectXSpeedFraction
    .byte $00,$05,$00,$02,$01,$06

; Minor object metasprite frame data addresses
MinObjFrameAddrs: ; 8650
	.word  MinObj_Frame_0,  MinObj_Frame_1,  MinObj_Frame_2,  MinObj_Frame_3
	.word  MinObj_Frame_4,  MinObj_Frame_5,  MinObj_Frame_6,  MinObj_Frame_7
	.word  MinObj_Frame_8,  MinObj_Frame_9,  MinObj_Frame_A,  MinObj_Frame_B
	.word  MinObj_Frame_C,  MinObj_Frame_D,  MinObj_Frame_E,  MinObj_Frame_F
	.word MinObj_Frame_10, MinObj_Frame_11, MinObj_Frame_12, MinObj_Frame_13
	.word MinObj_Frame_14, MinObj_Frame_15, MinObj_Frame_16, MinObj_Frame_17
	.word MinObj_Frame_18, MinObj_Frame_19, MinObj_Frame_1A, MinObj_Frame_1B
	.word MinObj_Frame_1C, MinObj_Frame_1D, MinObj_Frame_1E, MinObj_Frame_1F
	.word MinObj_Frame_20, MinObj_Frame_21, MinObj_Frame_22, MinObj_Frame_23
	.word MinObj_Frame_24, MinObj_Frame_25, MinObj_Frame_26, MinObj_Frame_27
	.word MinObj_Frame_28, MinObj_Frame_29, MinObj_Frame_2A, MinObj_Frame_2B
	.word MinObj_Frame_2C, MinObj_Frame_2D, MinObj_Frame_2E, MinObj_Frame_2F
	.word MinObj_Frame_30, MinObj_Frame_31, MinObj_Frame_32, MinObj_Frame_33
	.word MinObj_Frame_34, MinObj_Frame_35, MinObj_Frame_36, MinObj_Frame_37
	.word MinObj_Frame_38, MinObj_Frame_39, MinObj_Frame_3A, MinObj_Frame_3B
	.word MinObj_Frame_3C, MinObj_Frame_3D, MinObj_Frame_3E, MinObj_Frame_3F
	.word MinObj_Frame_40, MinObj_Frame_41, MinObj_Frame_42, MinObj_Frame_43
	.word MinObj_Frame_44, MinObj_Frame_45, MinObj_Frame_46, MinObj_Frame_47
	.word MinObj_Frame_48, MinObj_Frame_49, MinObj_Frame_4A, MinObj_Frame_4B
	.word MinObj_Frame_4C, MinObj_Frame_4D, MinObj_Frame_4E, MinObj_Frame_4E
	.word MinObj_Frame_4E, MinObj_Frame_51, MinObj_Frame_52, MinObj_Frame_53
	.word MinObj_Frame_54, MinObj_Frame_55, MinObj_Frame_56, MinObj_Frame_57
	.word MinObj_Frame_58, MinObj_Frame_59, MinObj_Frame_5A, MinObj_Frame_5B
	.word MinObj_Frame_5B, MinObj_Frame_5D, MinObj_Frame_5E, MinObj_Frame_5F
	.word MinObj_Frame_60, MinObj_Frame_61, MinObj_Frame_62, MinObj_Frame_63
	.word MinObj_Frame_64, MinObj_Frame_65, MinObj_Frame_66, MinObj_Frame_67
	.word MinObj_Frame_68, MinObj_Frame_69, MinObj_Frame_6A, MinObj_Frame_6B
	.word MinObj_Frame_6C, MinObj_Frame_6D, MinObj_Frame_6E, MinObj_Frame_6F
	.word MinObj_Frame_70, MinObj_Frame_71, MinObj_Frame_72, MinObj_Frame_73
	.word MinObj_Frame_74, MinObj_Frame_75, MinObj_Frame_76, MinObj_Frame_77
	.word MinObj_Frame_78, MinObj_Frame_79, MinObj_Frame_7A, MinObj_Frame_7B
	.word MinObj_Frame_7C, MinObj_Frame_7D, MinObj_Frame_7E, MinObj_Frame_7F
	.word MinObj_Frame_80, MinObj_Frame_81, MinObj_Frame_82, MinObj_Frame_83
	.word MinObj_Frame_84, MinObj_Frame_85, MinObj_Frame_86, MinObj_Frame_87
	.word MinObj_Frame_88, MinObj_Frame_89, MinObj_Frame_8A, MinObj_Frame_8B
	.word MinObj_Frame_8C, MinObj_Frame_8D, MinObj_Frame_8E, TileOffsTable_0

; Minor object metasprite state frame index table addresses
MinObjFrameTableAddrs: ; 8770
	.word  MinObj_FrameIdcs_0,  MinObj_FrameIdcs_1
	.word  MinObj_FrameIdcs_2,  MinObj_FrameIdcs_3
	.word  MinObj_FrameIdcs_4,  MinObj_FrameIdcs_5
	.word  MinObj_FrameIdcs_6,  MinObj_FrameIdcs_7
	.word  MinObj_FrameIdcs_8,  MinObj_FrameIdcs_9
	.word  MinObj_FrameIdcs_A,  MinObj_FrameIdcs_B
	.word  MinObj_FrameIdcs_C,  MinObj_FrameIdcs_D
	.word  MinObj_FrameIdcs_E,  MinObj_FrameIdcs_F
	.word MinObj_FrameIdcs_10, MinObj_FrameIdcs_11
	.word MinObj_FrameIdcs_12, MinObj_FrameIdcs_13
	.word MinObj_FrameIdcs_14, MinObj_FrameIdcs_15
	.word MinObj_FrameIdcs_16,  MinObj_FrameIdcs_B
	.word MinObj_FrameIdcs_18, MinObj_FrameIdcs_19
	.word MinObj_FrameIdcs_1A, MinObj_FrameIdcs_1B
	.word MinObj_FrameIdcs_1C, MinObj_FrameIdcs_1D
	.word MinObj_FrameIdcs_1E, MinObj_FrameIdcs_1F
	.word MinObj_FrameIdcs_20,  MinObj_FrameIdcs_4
	.word  MinObj_FrameIdcs_B,  MinObj_FrameIdcs_B
	.word  MinObj_FrameIdcs_C, MinObj_FrameIdcs_19
	.word MinObj_FrameIdcs_26, MinObj_FrameIdcs_10
	.word MinObj_FrameIdcs_1B, MinObj_FrameIdcs_1E
	.word MinObj_FrameIdcs_2A, MinObj_FrameIdcs_2B
	.word MinObj_FrameIdcs_2C, MinObj_FrameIdcs_2D
	.word MinObj_FrameIdcs_2E, MinObj_FrameIdcs_2F
	.word MinObj_FrameIdcs_2F, MinObj_FrameIdcs_31
	.word MinObj_FrameIdcs_2F, MinObj_FrameIdcs_33
	.word MinObj_FrameIdcs_34, MinObj_FrameIdcs_35
	.word MinObj_FrameIdcs_36, MinObj_FrameIdcs_37
	.word MinObj_FrameIdcs_38, MinObj_FrameIdcs_2F
	.word MinObj_FrameIdcs_3A, MinObj_FrameIdcs_2F
	.word MinObj_FrameIdcs_3C, MinObj_FrameIdcs_3D
	.word MinObj_FrameIdcs_3E, MinObj_FrameIdcs_3F
	.word MinObj_FrameIdcs_40, MinObj_FrameIdcs_41
	.word MinObj_FrameIdcs_42, MinObj_FrameIdcs_2F
	.word MinObj_FrameIdcs_44, MinObj_FrameIdcs_45
	.word MinObj_FrameIdcs_2F, MinObj_FrameIdcs_2F
	.word MinObj_FrameIdcs_48, MinObj_FrameIdcs_2F
	.word MinObj_FrameIdcs_4A

/*	
Metasprite frame data format:
	0: Number of tiles in metasprite
	1: Index into TileOffsTableAddrs of offset table to use
	2i+2: PPU tile number for tile i
	2i+3: Base PPU tile attributes for tile i in left-facing pose
*/

; Major object metasprite frame data
MajObj_Frame_0: ; 8806
	.byte $0A, $00
	.byte $1A,$00, $29,$00, $2A,$00, $2B,$00
	.byte $09,$00, $0A,$00, $0B,$00, $19,$00
	.byte $1B,$00, $20,$01
MajObj_Frame_1: ; 881C
	.byte $0A, $00
	.byte $1A,$00, $29,$00, $2A,$00, $2B,$00
	.byte $09,$00, $0A,$00, $0B,$00, $19,$00
	.byte $1B,$00, $00,$01
MajObj_Frame_2: ; 8832
	.byte $0A, $00
	.byte $1A,$00, $03,$00, $04,$00, $2B,$00
	.byte $09,$00, $0A,$00, $0B,$00, $19,$00
	.byte $1B,$00, $00,$01
MajObj_Frame_3: ; 8848
	.byte $0A, $01
	.byte $01,$00, $02,$00, $11,$00, $12,$00
	.byte $21,$00, $22,$00, $23,$00, $00,$01
	.byte $13,$00, $10,$00
MajObj_Frame_4: ; 885E
	.byte $09, $01
	.byte $01,$00, $05,$00, $14,$00, $15,$00
	.byte $24,$00, $25,$00, $26,$00, $00,$01
	.byte $16,$00
MajObj_Frame_5: ; 8872
	.byte $08, $02
	.byte $07,$00, $08,$00, $17,$00, $18,$00
	.byte $27,$00, $28,$00, $06,$00, $00,$01
MajObj_Frame_6: ; 8884
	.byte $0B, $04
	.byte $31,$00, $32,$00, $41,$00, $42,$00
	.byte $51,$00, $52,$00, $30,$40, $50,$00
	.byte $40,$00, $2F,$01, $30,$00
MajObj_Frame_7: ; 889C
	.byte $0A, $03
	.byte $0C,$00, $0D,$00, $47,$00, $1D,$00
	.byte $57,$00, $38,$00, $2E,$01, $1E,$00
	.byte $46,$00, $56,$00
MajObj_Frame_8: ; 88B2
	.byte $0A, $00
	.byte $1A,$00, $49,$00, $2A,$00, $2B,$00
	.byte $48,$00, $0A,$00, $0B,$00, $58,$00
	.byte $1B,$00, $20,$01
MajObj_Frame_9: ; 88C8
	.byte $0A, $00
	.byte $1A,$00, $49,$00, $2A,$00, $2B,$00
	.byte $48,$00, $0A,$00, $0B,$00, $58,$00
	.byte $1B,$00, $00,$01
MajObj_Frame_A: ; 88DE
	.byte $0A, $00
	.byte $1A,$00, $39,$00, $04,$00, $2B,$00
	.byte $48,$00, $0A,$00, $0B,$00, $58,$00
	.byte $1B,$00, $00,$01
MajObj_Frame_B: ; 88F4
	.byte $09, $01
	.byte $5B,$00, $02,$00, $5C,$00, $12,$00
	.byte $4B,$00, $22,$00, $23,$00, $00,$01
	.byte $13,$00
MajObj_Frame_C: ; 8908
	.byte $08, $01
	.byte $5B,$00, $05,$00, $5A,$00, $3B,$00
	.byte $24,$00, $25,$00, $4D,$00, $00,$01
MajObj_Frame_D: ; 891A
	.byte $08, $02
	.byte $3C,$00, $08,$00, $4C,$00, $18,$00
	.byte $27,$00, $28,$00, $06,$00, $00,$01
MajObj_Frame_E: ; 892C
	.byte $0A, $04
	.byte $3C,$00, $32,$00, $3D,$00, $42,$00
	.byte $51,$00, $52,$00, $30,$40, $50,$00
	.byte $40,$00, $2F,$01
MajObj_Frame_F: ; 8942
	.byte $0B, $05
	.byte $0C,$00, $0D,$00, $1C,$00, $1D,$00
	.byte $2C,$00, $2D,$00, $1E,$00, $50,$00
	.byte $0E,$00, $2E,$01, $1E,$40
MajObj_Frame_10: ; 895A
	.byte $0A, $07
	.byte $07,$00, $0D,$00, $43,$00, $44,$00
	.byte $53,$00, $54,$00, $55,$00, $45,$00
	.byte $0F,$00, $00,$01
MajObj_Frame_11: ; 8970
	.byte $0A, $01
	.byte $01,$00, $02,$00, $4A,$00, $12,$00
	.byte $4B,$00, $22,$00, $23,$00, $00,$01
	.byte $13,$00, $0F,$00
MajObj_Frame_12: ; 8986
	.byte $09, $08
	.byte $01,$00, $05,$00, $3A,$00, $3B,$00
	.byte $24,$00, $25,$00, $00,$01, $0F,$00
	.byte $4D,$00
MajObj_Frame_13: ; 899A
	.byte $09, $02
	.byte $07,$00, $08,$00, $59,$00, $18,$00
	.byte $27,$00, $28,$00, $06,$00, $00,$01
	.byte $0F,$00
MajObj_Frame_14: ; 89AE
	.byte $0B, $06
	.byte $07,$00, $32,$00, $6C,$00, $42,$00
	.byte $51,$00, $52,$00, $30,$40, $50,$00
	.byte $40,$00, $2F,$01, $0F,$00
MajObj_Frame_15: ; 89C6
	.byte $04, $09
	.byte $33,$00, $33,$00, $33,$00, $33,$00
MajObj_Frame_16: ; 89D0
	.byte $0A, $09
	.byte $1F,$00, $36,$00, $36,$00, $37,$00
	.byte $35,$00, $35,$00, $34,$00, $35,$40
	.byte $35,$40, $34,$40
MajObj_Frame_17: ; 89E6
	.byte $04, $00
	.byte $1F,$00, $34,$00, $37,$00, $34,$40
MajObj_Frame_18: ; 89F0
	.byte $07, $0B
	.byte $3E,$00, $3F,$00, $4E,$00, $4F,$00
	.byte $5F,$00, $5E,$00, $5D,$00
MajObj_Frame_19: ; 8A00
	.byte $07, $0A
	.byte $3F,$40, $3E,$40, $4F,$40, $4E,$40
	.byte $5F,$40, $5E,$40, $5D,$40
MajObj_Frame_1A: ; 8A10
	.byte $09, $00
	.byte $7B,$01, $79,$01, $7A,$81, $79,$41
	.byte $77,$01, $7A,$01, $77,$41, $78,$01
	.byte $78,$41
MajObj_Frame_1B: ; 8A24
	.byte $09, $0A
	.byte $6D,$00, $6E,$00, $7D,$00, $7E,$00
	.byte $6F,$00, $5E,$40, $5D,$40, $0F,$00
	.byte $7C,$01
MajObj_Frame_1C: ; 8A38
	.byte $05, $0B
	.byte $60,$00, $60,$40, $62,$40, $61,$40
	.byte $63,$40
MajObj_Frame_1D: ; 8A44
	.byte $05, $0A
	.byte $60,$00, $60,$40, $61,$00, $62,$00
	.byte $63,$00
MajObj_Frame_1E: ; 8A50
	.byte $03, $0C
	.byte $74,$01, $74,$01, $74,$01
MajObj_Frame_1F: ; 8A58
	.byte $03, $0C
	.byte $75,$01, $75,$01, $75,$01
MajObj_Frame_20: ; 8A60
	.byte $03, $0C
	.byte $76,$01, $76,$01, $76,$01
MajObj_Frame_21: ; 8A68
	.byte $01, $00
	.byte $69,$01
MajObj_Frame_22: ; 8A6C
	.byte $0B, $0D
	.byte $82,$42, $81,$42, $80,$42, $85,$42
	.byte $84,$42, $83,$42, $88,$42, $87,$42
	.byte $86,$42, $95,$02, $95,$42
MajObj_Frame_23: ; 8A84
	.byte $09, $0D
	.byte $82,$42, $81,$42, $80,$42, $85,$42
	.byte $84,$42, $83,$42, $88,$42, $87,$42
	.byte $86,$42
MajObj_Frame_24: ; 8A98
	.byte $0B, $0E
	.byte $82,$42, $81,$42, $80,$42, $8B,$42
	.byte $8A,$42, $89,$42, $88,$42, $87,$42
	.byte $86,$42, $97,$42, $96,$42
MajObj_Frame_25: ; 8AB0
	.byte $09, $0E
	.byte $82,$42, $81,$42, $80,$42, $8B,$42
	.byte $8A,$42, $89,$42, $88,$42, $87,$42
	.byte $86,$42
MajObj_Frame_26: ; 8AC4
	.byte $0B, $0D
	.byte $B2,$42, $B1,$42, $B0,$42, $B4,$42
	.byte $84,$42, $B3,$42, $88,$42, $87,$42
	.byte $86,$42, $95,$02, $95,$42
MajObj_Frame_27: ; 8ADC
	.byte $09, $0F
	.byte $B7,$42, $B6,$42, $B9,$42, $B8,$42
	.byte $BD,$42, $BC,$42, $BA,$42, $B5,$42
	.byte $BB,$42
MajObj_Frame_28: ; 8AF0
	.byte $0B, $10
	.byte $8C,$02, $8D,$02, $8E,$02, $8F,$02
	.byte $90,$02, $91,$02, $93,$02, $92,$02
	.byte $94,$02, $95,$02, $95,$42
MajObj_Frame_29: ; 8B08
	.byte $09, $10
	.byte $8C,$02, $8D,$02, $8E,$02, $8F,$02
	.byte $90,$02, $91,$02, $93,$02, $92,$02
	.byte $94,$02
MajObj_Frame_2A: ; 8B1C
	.byte $0B, $11
	.byte $A4,$02, $A5,$02, $10,$02, $AE,$02
	.byte $AF,$02, $13,$02, $21,$02, $22,$02
	.byte $23,$02, $95,$02, $95,$42
MajObj_Frame_2B: ; 8B34
	.byte $09, $11
	.byte $A4,$02, $A5,$02, $10,$02, $AE,$02
	.byte $AF,$02, $13,$02, $21,$02, $22,$02
	.byte $23,$02
MajObj_Frame_2C: ; 8B48
	.byte $0A, $12
	.byte $A2,$02, $A3,$02, $AC,$02, $AD,$02
	.byte $16,$02, $24,$02, $25,$02, $26,$02
	.byte $95,$02, $95,$42
MajObj_Frame_2D: ; 8B5E
	.byte $08, $12
	.byte $A2,$02, $A3,$02, $AC,$02, $AD,$02
	.byte $16,$02, $24,$02, $25,$02, $26,$02
MajObj_Frame_2E: ; 8B70
	.byte $09, $13
	.byte $A0,$02, $A1,$02, $AA,$02, $AB,$02
	.byte $06,$02, $27,$02, $28,$02, $96,$02
	.byte $97,$02
MajObj_Frame_2F: ; 8B84
	.byte $07, $13
	.byte $A0,$02, $A1,$02, $AA,$02, $AB,$02
	.byte $06,$02, $27,$02, $28,$02
MajObj_Frame_30: ; 8B94
	.byte $0A, $00
	.byte $84,$02, $86,$02, $87,$02, $88,$02
	.byte $80,$02, $81,$02, $82,$02, $83,$02
	.byte $85,$02, $8F,$01
MajObj_Frame_31: ; 8BAA
	.byte $0A, $00
	.byte $84,$02, $86,$02, $87,$02, $88,$02
	.byte $89,$02, $81,$02, $82,$02, $8A,$02
	.byte $85,$02, $8F,$01
MajObj_Frame_32: ; 8BC0
	.byte $0E, $27
	.byte $8B,$02, $8C,$02, $8D,$02, $92,$02
	.byte $93,$02, $94,$02, $90,$02, $87,$02
	.byte $91,$02, $8E,$01, $9D,$01, $9E,$01
	.byte $9F,$01, $9F,$41
MajObj_Frame_33: ; 8BDE
	.byte $0A, $14
	.byte $A5,$02, $A6,$02, $A7,$02, $A8,$02
	.byte $A9,$02, $AA,$02, $B7,$02, $B6,$02
	.byte $A4,$02, $8F,$01
MajObj_Frame_34: ; 8BF4
	.byte $10, $28
	.byte $80,$02, $81,$02, $9C,$02, $83,$02
	.byte $84,$02, $B8,$02, $A0,$02, $A1,$02
	.byte $A2,$02, $8F,$01, $9D,$01, $9E,$01
	.byte $9F,$01, $9F,$41, $95,$02, $A3,$02
MajObj_Frame_35: ; 8C16
	.byte $0D, $00
	.byte $84,$02, $97,$02, $98,$02, $99,$02
	.byte $80,$02, $81,$02, $9C,$02, $83,$02
	.byte $96,$02, $8F,$01, $95,$02, $9A,$02
	.byte $9B,$02
MajObj_Frame_36: ; 8C32
	.byte $11, $15
	.byte $AB,$02, $AC,$02, $AD,$02, $AE,$02
	.byte $AF,$02, $B0,$02, $95,$02, $B1,$02
	.byte $B2,$02, $B3,$02, $8F,$01, $9D,$01
	.byte $9E,$01, $9F,$01, $9F,$41, $B4,$02
	.byte $B5,$02
MajObj_Frame_37: ; 8C56
	.byte $0A, $16
	.byte $80,$02, $81,$02, $82,$02, $83,$02
	.byte $84,$02, $85,$02, $86,$02, $87,$02
	.byte $88,$02, $98,$01
MajObj_Frame_38: ; 8C6C
	.byte $0A, $16
	.byte $89,$02, $8A,$02, $8B,$02, $8C,$02
	.byte $8D,$02, $8E,$02, $8F,$02, $90,$02
	.byte $91,$02, $99,$01
MajObj_Frame_39: ; 8C82
	.byte $0A, $16
	.byte $92,$02, $81,$02, $82,$02, $93,$02
	.byte $94,$02, $95,$02, $96,$02, $87,$02
	.byte $97,$02, $9A,$01
MajObj_Frame_3A: ; 8C98
	.byte $0A, $17
	.byte $B7,$02, $B8,$02, $B9,$02, $BA,$02
	.byte $BB,$02, $BC,$02, $BD,$02, $BE,$02
	.byte $BF,$02, $9A,$01
MajObj_Frame_3B: ; 8CAE
	.byte $0A, $18
	.byte $9B,$02, $9C,$02, $9D,$02, $9E,$02
	.byte $9F,$02, $A0,$02, $A1,$02, $A2,$02
	.byte $A3,$02, $98,$01
MajObj_Frame_3C: ; 8CC4
	.byte $0A, $19
	.byte $A4,$02, $A5,$02, $A6,$02, $A7,$02
	.byte $A8,$02, $A9,$02, $AA,$02, $AB,$02
	.byte $AC,$02, $98,$01
MajObj_Frame_3D: ; 8CDA
	.byte $0B, $1A
	.byte $A6,$42, $AE,$02, $AF,$02, $B0,$02
	.byte $B1,$02, $B2,$02, $B3,$02, $B4,$02
	.byte $B5,$02, $98,$01, $B6,$02
MajObj_Frame_3E: ; 8CF2
	.byte $0C, $16
	.byte $80,$02, $81,$02, $82,$02, $83,$02
	.byte $84,$02, $85,$02, $86,$02, $87,$02
	.byte $88,$02, $8A,$01, $8C,$03, $8D,$03
MajObj_Frame_3F: ; 8D0C
	.byte $0C, $16
	.byte $80,$02, $81,$02, $82,$02, $83,$02
	.byte $84,$02, $85,$02, $86,$02, $87,$02
	.byte $88,$02, $8A,$01, $8E,$03, $8F,$03
MajObj_Frame_40: ; 8D26
	.byte $0C, $1B
	.byte $99,$02, $9A,$02, $9B,$02, $9C,$02
	.byte $9D,$02, $9E,$02, $9F,$02, $87,$02
	.byte $90,$02, $8B,$01, $8E,$03, $8F,$03
MajObj_Frame_41: ; 8D40
	.byte $0C, $1C
	.byte $91,$02, $92,$02, $93,$02, $94,$02
	.byte $95,$02, $96,$02, $97,$02, $98,$02
	.byte $89,$02, $8A,$01, $8C,$03, $8D,$03
MajObj_Frame_42: ; 8D5A
	.byte $0C, $1D
	.byte $9A,$02, $9B,$02, $9C,$02, $9D,$02
	.byte $9E,$02, $9F,$02, $A0,$02, $A1,$02
	.byte $B8,$01, $BB,$03, $BC,$03, $BA,$02
MajObj_Frame_43: ; 8D74
	.byte $0B, $1D
	.byte $9A,$02, $A2,$02, $A3,$02, $A4,$02
	.byte $A5,$02, $A6,$02, $A7,$02, $A8,$02
	.byte $B9,$01, $BB,$03, $BC,$03
MajObj_Frame_44: ; 8D8C
	.byte $0A, $1E
	.byte $A9,$02, $AA,$02, $AB,$02, $AC,$02
	.byte $AF,$02, $AD,$02, $AE,$02, $B8,$01
	.byte $BD,$03, $BE,$03
MajObj_Frame_45: ; 8DA2
	.byte $0D, $0D
	.byte $90,$02, $91,$02, $92,$02, $93,$02
	.byte $94,$02, $95,$02, $96,$02, $97,$02
	.byte $98,$02, $8C,$03, $8D,$03, $8A,$01
	.byte $99,$02
MajObj_Frame_46: ; 8DBE
	.byte $0D, $0D
	.byte $90,$02, $91,$02, $92,$02, $93,$02
	.byte $94,$02, $95,$02, $96,$02, $97,$02
	.byte $98,$02, $8E,$03, $8F,$03, $8A,$01
	.byte $99,$02
MajObj_Frame_47: ; 8DDA
	.byte $0A, $1F
	.byte $80,$02, $81,$02, $82,$02, $83,$02
	.byte $84,$02, $85,$02, $86,$02, $87,$02
	.byte $88,$02, $97,$01
MajObj_Frame_48: ; 8DF0
	.byte $0A, $1F
	.byte $80,$02, $81,$02, $89,$02, $8A,$02
	.byte $84,$02, $8B,$02, $AE,$42, $87,$02
	.byte $96,$02, $97,$01
MajObj_Frame_49: ; 8E06
	.byte $0B, $21
	.byte $8D,$02, $8E,$02, $8F,$02, $90,$02
	.byte $91,$02, $92,$02, $93,$02, $94,$02
	.byte $95,$02, $97,$01, $8C,$02
MajObj_Frame_4A: ; 8E1E
	.byte $0A, $1F
	.byte $80,$02, $81,$02, $82,$02, $B8,$02
	.byte $84,$02, $85,$02, $98,$02, $99,$02
	.byte $9A,$02, $97,$01
MajObj_Frame_4B: ; 8E34
	.byte $0A, $20
	.byte $9B,$02, $9C,$02, $9D,$02, $9E,$02
	.byte $9F,$02, $A0,$02, $A1,$02, $A2,$02
	.byte $A3,$02, $97,$01
MajObj_Frame_4C: ; 8E4A
	.byte $0B, $0B
	.byte $A5,$02, $A6,$02, $A8,$02, $A9,$02
	.byte $AC,$02, $AB,$02, $AD,$02, $97,$01
	.byte $A4,$02, $A7,$02, $AA,$02
MajObj_Frame_4D: ; 8E62
	.byte $0C, $23
	.byte $B0,$02, $B1,$02, $B2,$02, $B3,$02
	.byte $B4,$02, $B5,$02, $B6,$02, $B7,$02
	.byte $BD,$42, $97,$01, $BF,$02, $BF,$02
MajObj_Frame_4E: ; 8E7C
	.byte $09, $22
	.byte $B9,$02, $BA,$02, $BB,$02, $BC,$02
	.byte $BD,$02, $BE,$02, $97,$01, $AF,$02
	.byte $AE,$02
MajObj_Frame_4F: ; 8E90
	.byte $10, $24
	.byte $95,$01, $96,$01, $99,$02, $9A,$02
	.byte $91,$02, $92,$02, $9D,$02, $9E,$02
	.byte $90,$02, $93,$02, $94,$02, $97,$02
	.byte $98,$02, $9B,$02, $9C,$02, $9F,$02
MajObj_Frame_50: ; 8EB2
	.byte $10, $24
	.byte $83,$01, $84,$01, $87,$02, $9A,$02
	.byte $91,$02, $92,$02, $9D,$02, $9E,$02
	.byte $80,$02, $81,$02, $82,$02, $85,$02
	.byte $86,$02, $88,$02, $89,$02, $8A,$02
MajObj_Frame_51: ; 8ED4
	.byte $10, $24
	.byte $95,$01, $96,$01, $99,$02, $9A,$02
	.byte $91,$02, $8B,$02, $9D,$02, $9E,$02
	.byte $90,$02, $8C,$02, $94,$02, $8D,$02
	.byte $98,$02, $8E,$02, $9C,$02, $8F,$02
MajObj_Frame_52: ; 8EF6
	.byte $20, $25
	.byte $95,$01, $96,$01, $99,$02, $A7,$02
	.byte $90,$02, $91,$02, $8B,$02, $8C,$02
	.byte $94,$02, $8D,$02, $98,$02, $A8,$02
	.byte $A9,$02, $AA,$02, $AB,$02, $AC,$02
	.byte $C9,$03, $CB,$03, $CD,$03, $CF,$03
	.byte $C8,$03, $CA,$03, $CC,$03, $CE,$03
	.byte $C1,$03, $C3,$03, $C5,$03, $C7,$03
	.byte $C0,$03, $C2,$03, $C4,$03, $C6,$03
MajObj_Frame_53: ; 8F38
	.byte $10, $24
	.byte $83,$01, $84,$01, $A1,$02, $A2,$02
	.byte $91,$02, $92,$02, $A4,$02, $A5,$02
	.byte $80,$02, $81,$02, $82,$02, $85,$02
	.byte $A0,$02, $88,$02, $A3,$02, $A6,$02
MajObj_Frame_54: ; 8F5A
	.byte $10, $26
	.byte $BB,$02, $BC,$02, $B0,$02, $B1,$02
	.byte $B6,$02, $B7,$02, $B8,$02, $B9,$02
	.byte $BA,$02, $BD,$02, $BE,$02, $B2,$02
	.byte $BF,$02, $B3,$02, $B4,$02, $B5,$02
MajObj_Frame_55: ; 8F7C
	.byte $10, $25
	.byte $95,$01, $96,$01, $99,$02, $9A,$02
	.byte $90,$02, $91,$02, $92,$02, $93,$02
	.byte $94,$02, $97,$02, $98,$02, $9B,$02
	.byte $A9,$02, $AA,$02, $AB,$02, $AC,$02
MajObj_Frame_56: ; 8F9E
	.byte $04, $24
	.byte $DD,$02, $DE,$02, $DF,$02, $DF,$42
MajObj_Frame_57: ; 8FA8
	.byte $0E, $28
	.byte $80,$02, $81,$02, $82,$02, $83,$02
	.byte $84,$02, $85,$02, $86,$02, $87,$02
	.byte $88,$02, $8F,$01, $9D,$01, $9E,$01
	.byte $9F,$01, $9F,$41
MajObj_Frame_58: ; 8FC6
	.byte $0E, $27
	.byte $89,$02, $81,$02, $82,$02, $8A,$02
	.byte $84,$02, $85,$02, $86,$02, $87,$02
	.byte $88,$02, $8F,$01, $9D,$01, $9E,$01
	.byte $9F,$01, $9F,$41
MajObj_Frame_59: ; 8FE4
	.byte $11, $28
	.byte $80,$02, $81,$02, $9C,$02, $83,$02
	.byte $84,$02, $96,$02, $97,$02, $98,$02
	.byte $99,$02, $8F,$01, $9D,$01, $9E,$01
	.byte $9F,$01, $9F,$41, $95,$02, $9A,$02
	.byte $9B,$02
MajObj_Frame_5A: ; 9008
	.byte $04, $24
	.byte $CA,$01, $CB,$01, $CC,$01, $CC,$41
MajObj_Frame_5B: ; 9012
	.byte $10, $24
	.byte $C3,$03, $C5,$03, $CA,$03, $CC,$03
	.byte $C2,$03, $C4,$03, $CB,$03, $CD,$03
	.byte $C0,$03, $C6,$03, $C1,$03, $C7,$03
	.byte $C8,$03, $CE,$03, $C9,$03, $CF,$03
MajObj_Frame_5C: ; 9034
	.byte $0B, $29
	.byte $80,$02, $81,$02, $82,$02, $83,$02
	.byte $84,$02, $85,$02, $7C,$02, $7D,$02
	.byte $7E,$02, $98,$01, $7F,$02
MajObj_Frame_5D: ; 904C
	.byte $0D, $29
	.byte $80,$02, $81,$02, $82,$02, $83,$02
	.byte $84,$02, $85,$02, $38,$02, $39,$02
	.byte $3A,$02, $8A,$01, $3B,$02, $8C,$03
	.byte $8D,$03
MajObj_Frame_5E: ; 9068
	.byte $04, $24
	.byte $B0,$03, $B1,$03, $B2,$03, $B3,$03
MajObj_Frame_5F: ; 9072
	.byte $04, $24
	.byte $B4,$03, $B5,$03, $B6,$03, $B7,$03
MajObj_Frame_60: ; 907C
	.byte $01, $00
	.byte $D6,$01
MajObj_Frame_61: ; 9080
	.byte $04, $24
	.byte $D7,$01, $D7,$41, $D7,$81, $D7,$C1
MajObj_Frame_62: ; 908A
	.byte $04, $24
	.byte $D8,$01, $D8,$41, $D8,$81, $D8,$C1
MajObj_Frame_63: ; 9094
	.byte $04, $24
	.byte $D9,$01, $D9,$41, $D9,$81, $D9,$C1
MajObj_Frame_64: ; 909E
	.byte $08, $24
	.byte $C2,$03, $C3,$03, $C2,$83, $C3,$83
	.byte $C0,$03, $C1,$03, $C0,$83, $C1,$83
MajObj_Frame_65: ; 90B0
	.byte $08, $24
	.byte $C6,$03, $C7,$03, $C6,$83, $C7,$83
	.byte $C4,$03, $C5,$03, $C4,$83, $C5,$83
MajObj_Frame_66: ; 90C2
	.byte $08, $24
	.byte $CA,$03, $CB,$03, $CA,$83, $CB,$83
	.byte $C8,$03, $C9,$03, $C8,$83, $C9,$83
MajObj_Frame_67: ; 90D4
	.byte $04, $24
	.byte $C0,$02, $C1,$02, $C2,$02, $C3,$02
MajObj_Frame_68: ; 90DE
	.byte $04, $24
	.byte $C4,$02, $C5,$02, $C6,$02, $C7,$02
MajObj_Frame_69: ; 90E8
	.byte $04, $24
	.byte $CC,$02, $CC,$42, $CD,$02, $CD,$42
MajObj_Frame_6A: ; 90F2
	.byte $04, $24
	.byte $CF,$42, $CE,$42, $CF,$C2, $CE,$C2
MajObj_Frame_6B: ; 90FC
	.byte $04, $24
	.byte $CD,$82, $CD,$C2, $CC,$82, $CC,$C2
MajObj_Frame_6C: ; 9106
	.byte $04, $24
	.byte $CF,$42, $CE,$42, $CF,$C2, $CE,$C2
MajObj_Frame_6D: ; 9110
	.byte $02, $2B
	.byte $C0,$01, $C1,$01
MajObj_Frame_6E: ; 9116
	.byte $02, $2A
	.byte $C3,$01, $C2,$01
MajObj_Frame_6F: ; 911C
	.byte $02, $2B
	.byte $C4,$81, $C4,$01
MajObj_Frame_70: ; 9122
	.byte $02, $2B
	.byte $C2,$81, $C5,$01
MajObj_Frame_71: ; 9128
	.byte $02, $2B
	.byte $C3,$81, $C6,$81
MajObj_Frame_72: ; 912E
	.byte $02, $2A
	.byte $C8,$01, $C2,$01
MajObj_Frame_73: ; 9134
	.byte $02, $2A
	.byte $C9,$01, $C7,$01
MajObj_Frame_74: ; 913A
	.byte $02, $2B
	.byte $C1,$01, $C6,$01
MajObj_Frame_75: ; 9140
	.byte $10, $24
	.byte $7B,$01, $7B,$01, $7B,$01, $7B,$01
	.byte $7A,$01, $7A,$01, $7A,$81, $7A,$81
	.byte $77,$01, $77,$41, $78,$01, $78,$41
	.byte $78,$01, $78,$41, $79,$01, $79,$41
MajObj_Frame_76: ; 9162
	.byte $01, $00
	.byte $FF,$03
MajObj_Frame_77: ; 9166
	.byte $04, $24
	.byte $FF,$03, $FF,$03, $FF,$03, $FF,$03
MajObj_Frame_78: ; 9170
	.byte $0C, $1C
	.byte $91,$02, $92,$02, $93,$02, $94,$02
	.byte $95,$02, $96,$02, $97,$02, $98,$02
	.byte $89,$02, $8A,$01, $8E,$03, $8F,$03
MajObj_Frame_79: ; 918A
	.byte $04, $24
	.byte $EC,$00, $EC,$40, $ED,$00, $ED,$40
MajObj_Frame_7A: ; 9194
	.byte $04, $24
	.byte $EE,$00, $EF,$00, $EE,$80, $EF,$80
MajObj_Frame_7B: ; 919E
	.byte $04, $24
	.byte $ED,$80, $ED,$C0, $EC,$80, $EC,$C0
MajObj_Frame_7C: ; 91A8
	.byte $04, $24
	.byte $EF,$40, $EE,$40, $EF,$C0, $EE,$C0
MajObj_Frame_7D: ; 91B2
	.byte $04, $24
	.byte $E0,$00, $E1,$00, $E2,$00, $E3,$00
MajObj_Frame_7E: ; 91BC
	.byte $04, $24
	.byte $E4,$00, $E5,$00, $E6,$00, $E7,$00
MajObj_Frame_7F: ; 91C6
	.byte $04, $24
	.byte $EA,$00, $EB,$00, $EC,$00, $EC,$40
MajObj_Frame_80: ; 91D0
	.byte $02, $2A
	.byte $E8,$00, $E8,$00
MajObj_Frame_81: ; 91D6
	.byte $04, $24
	.byte $FF,$01, $FF,$01, $FF,$01, $FF,$01
MajObj_Frame_82: ; 91E0
	.byte $08, $24
	.byte $CE,$01, $CF,$01, $CA,$01, $CB,$01
	.byte $CC,$01, $CD,$01, $CC,$01, $CD,$01
MajObj_Frame_83: ; 91F2
	.byte $04, $24
	.byte $CC,$01, $CD,$01, $CE,$01, $CF,$01
	.byte $CA,$01, $CB,$01, $CA,$01, $CB,$01
MajObj_Frame_84: ; 9204
	.byte $04, $24
	.byte $E0,$00, $E1,$00, $E0,$80, $E1,$80
MajObj_Frame_85: ; 920E
	.byte $04, $24
	.byte $E4,$00, $E5,$00, $E4,$80, $E5,$80
MajObj_Frame_86: ; 9218
	.byte $04, $24
	.byte $E8,$00, $E9,$00, $E8,$80, $E9,$80
MajObj_Frame_87: ; 9222
	.byte $04, $24
	.byte $EC,$00, $EC,$40, $EC,$80, $EC,$C0
MajObj_Frame_88: ; 922C
	.byte $04, $24
	.byte $ED,$00, $ED,$40, $ED,$80, $ED,$C0
MajObj_Frame_89: ; 9236
	.byte $04, $24
	.byte $EE,$00, $EE,$40, $EE,$80, $EE,$C0
MajObj_Frame_8A: ; 9240
	.byte $02, $2B
	.byte $E0,$01, $E1,$01
MajObj_Frame_8B: ; 9246
	.byte $02, $2A
	.byte $E3,$01, $E2,$01
MajObj_Frame_8C: ; 924C
	.byte $02, $2B
	.byte $E4,$81, $E4,$01
MajObj_Frame_8D: ; 9252
	.byte $02, $2B
	.byte $E2,$81, $E5,$01
MajObj_Frame_8E: ; 9258
	.byte $02, $2B
	.byte $E3,$81, $E6,$01
MajObj_Frame_8F: ; 925E
	.byte $02, $2A
	.byte $E8,$01, $E2,$01
MajObj_Frame_90: ; 9264
	.byte $02, $2A
	.byte $E9,$01, $E7,$01
MajObj_Frame_91: ; 926A
	.byte $02, $2B
	.byte $E1,$01, $E6,$01
MajObj_Frame_92: ; 9270
	.byte $08, $24
	.byte $EE,$01, $EF,$01, $EA,$01, $EB,$01
	.byte $EC,$01, $ED,$01, $EC,$01, $ED,$01
MajObj_Frame_93: ; 9282
	.byte $08, $24
	.byte $EC,$01, $ED,$01, $EE,$01, $EF,$01
	.byte $EA,$01, $EB,$01, $EA,$01, $EB,$01
MajObj_Frame_94: ; 9294
	.byte $08, $24
	.byte $EA,$01, $EB,$01, $EC,$01, $ED,$01
	.byte $EE,$01, $EF,$01, $EE,$01, $EF,$01
MajObj_Frame_95: ; 92A6
	.byte $10, $24
	.byte $E3,$01, $E5,$01, $EA,$01, $EC,$01
	.byte $E2,$01, $E4,$01, $EB,$01, $ED,$01
	.byte $E0,$01, $E6,$01, $E1,$01, $E7,$01
	.byte $E8,$01, $EE,$01, $E9,$01, $EF,$01
MajObj_Frame_96: ; 92C8
	.byte $01, $00
	.byte $CA,$01, $CB,$01, $CC,$01, $CD,$01
	.byte $CE,$01, $CF,$01, $CE,$01, $CF,$01
MajObj_Frame_97: ; 92DA
	.byte $0B, $2C
	.byte $07,$00, $08,$00, $70,$00, $44,$00
	.byte $71,$00, $54,$00, $55,$00, $45,$00
	.byte $72,$00, $00,$01, $73,$00
MajObj_Frame_98: ; 92F2
	.byte $0B, $06
	.byte $07,$00, $32,$00, $6C,$00, $42,$00
	.byte $51,$00, $52,$00, $30,$40, $50,$00
	.byte $40,$00, $2F,$01, $64,$00
MajObj_Frame_99: ; 930A
	.byte $09, $0A
	.byte $6D,$00, $6E,$00, $7D,$00, $7E,$00
	.byte $6F,$00, $5E,$40, $5D,$40, $7F,$00
	.byte $7C,$01
MajObj_Frame_9A: ; 931E
	.byte $09, $00
	.byte $84,$01, $86,$02, $87,$02, $88,$02
	.byte $80,$01, $81,$01, $82,$01, $83,$01
	.byte $85,$01
MajObj_Frame_9B: ; 9332
	.byte $09, $00
	.byte $84,$01, $86,$02, $87,$02, $88,$02
	.byte $89,$01, $8A,$01, $82,$01, $83,$01
	.byte $85,$01
MajObj_Frame_9C: ; 9346
	.byte $09, $00
	.byte $84,$01, $86,$02, $87,$02, $9B,$02
	.byte $89,$01, $8A,$01, $8B,$01, $83,$01
	.byte $8C,$01
MajObj_Frame_9D: ; 935A
	.byte $0C, $2D
	.byte $9C,$02, $9D,$02, $9E,$02, $9D,$42
	.byte $9C,$42, $9F,$02, $A0,$02, $A1,$02
	.byte $A0,$02, $A1,$02, $A0,$02, $9F,$42
MajObj_Frame_9E: ; 9374
	.byte $0C, $2B
	.byte $A4,$01, $A7,$01, $A4,$01, $A6,$01
	.byte $A4,$01, $A6,$41, $A3,$01, $A5,$01
	.byte $A3,$41, $A5,$41, $A2,$01, $A2,$41
MajObj_Frame_9F: ; 938E
	.byte $0C, $2B
	.byte $AA,$01, $A7,$01, $AA,$01, $A6,$01
	.byte $AA,$01, $A6,$41, $A9,$01, $A5,$01
	.byte $AB,$01, $A5,$41, $A8,$01, $AC,$01
MajObj_Frame_A0: ; 93A8
	.byte $0C, $2B
	.byte $AF,$01, $A7,$01, $AF,$01, $A6,$01
	.byte $AF,$01, $A6,$41, $AE,$01, $A5,$01
	.byte $AE,$41, $A5,$41, $AD,$01, $AD,$41
MajObj_Frame_A1: ; 93C2
	.byte $0E, $2B
	.byte $AF,$01, $A7,$01, $AF,$01, $A6,$01
	.byte $AF,$01, $A6,$41, $AE,$01, $A5,$01
	.byte $AE,$41, $A5,$41, $AD,$01, $AD,$41
	.byte $9A,$01, $9A,$41
MajObj_Frame_A2: ; 93E0
	.byte $08, $3C
	.byte $82,$03, $84,$03, $83,$03, $85,$03
	.byte $80,$03, $81,$03, $86,$03, $87,$03
MajObj_Frame_A3: ; 93F2
	.byte $02, $2A
	.byte $86,$02, $87,$02
MajObj_Frame_A4: ; 93F8
	.byte $02, $2A
	.byte $88,$02, $89,$02
MajObj_Frame_A5: ; 93FE
	.byte $02, $2A
	.byte $8A,$02, $8B,$02
MajObj_Frame_A6: ; 9404
	.byte $04, $24
	.byte $9C,$41, $9B,$41, $9E,$41, $9D,$41
MajObj_Frame_A7: ; 940E
	.byte $04, $24
	.byte $A0,$41, $9F,$41, $A2,$41, $A1,$41
MajObj_Frame_A8: ; 9418
	.byte $01, $00
	.byte $BF,$02
MajObj_Frame_A9: ; 941C
	.byte $01, $00
	.byte $D6,$00
MajObj_Frame_AA: ; 9420
	.byte $04, $24
	.byte $D7,$00, $D7,$40, $D7,$80, $D7,$C0
MajObj_Frame_AB: ; 942A
	.byte $04, $24
	.byte $D8,$00, $D8,$40, $D8,$80, $D8,$C0
MajObj_Frame_AC: ; 9434
	.byte $04, $24
	.byte $D9,$00, $D9,$40, $D9,$80, $D9,$C0
MajObj_Frame_AD: ; 943E
	.byte $09, $00
	.byte $CB,$03, $CD,$03, $CE,$03, $CF,$03
	.byte $C7,$03, $C8,$03, $C9,$03, $CA,$03
	.byte $CC,$03
MajObj_Frame_AE: ; 9452
	.byte $02, $42
	.byte $B9,$02, $BA,$02
MajObj_Frame_AF: ; 9458
	.byte $02, $43
	.byte $BB,$02, $B9,$82
MajObj_Frame_B0: ; 945E
	.byte $01, $00
	.byte $98,$02
MajObj_Frame_B1: ; 9462
	.byte $01, $00
	.byte $B0,$03
MajObj_Frame_B2: ; 9466
	.byte $01, $00
	.byte $BC,$03
MajObj_Frame_B3: ; 946A
	.byte $08, $0D
	.byte $89,$01, $8A,$01, $82,$01, $C0,$01
	.byte $C1,$01, $C2,$01, $C3,$01, $C4,$01
MajObj_Frame_B4: ; 947C
	.byte $08, $0D
	.byte $80,$01, $81,$01, $82,$01, $B1,$01
	.byte $B2,$01, $B3,$01, $B4,$01, $B5,$01
MajObj_Frame_B5: ; 948E
	.byte $08, $0D
	.byte $96,$02, $90,$02, $91,$02, $92,$02
	.byte $93,$02, $94,$02, $97,$02, $95,$02
MajObj_Frame_B6: ; 94A0
	.byte $02, $2B
	.byte $B7,$02, $B8,$02
MajObj_Frame_B7: ; 94A6
	.byte $01, $00
	.byte $C6,$01
MajObj_Frame_B8: ; 94AA
	.byte $0C, $44
	.byte $B0,$01, $B2,$01, $B4,$01, $B6,$01
	.byte $B8,$01, $BF,$01, $B1,$01, $B3,$01
	.byte $B5,$01, $B7,$01, $B9,$01, $BB,$01
MajObj_Frame_B9: ; 94C4
	.byte $0B, $44
	.byte $D3,$03, $BD,$01, $F0,$01, $F0,$01
	.byte $F0,$01, $BE,$01, $BA,$01, $B3,$01
	.byte $B7,$01, $BC,$01, $BB,$01
MajObj_Frame_BA: ; 94DC
	.byte $03, $44
	.byte $F0,$01, $F0,$01, $F0,$01
MajObj_Frame_BB: ; 94E4
	.byte $01, $44
	.byte $F0,$01
MajObj_Frame_BC: ; 94E8
	.byte $02, $2B
	.byte $AE,$01, $AF,$01
MajObj_Frame_BD: ; 94EE
	.byte $0C, $3C
	.byte $8A,$22, $8A,$62, $96,$22, $96,$22
	.byte $89,$22, $8C,$22, $89,$62, $8C,$62
	.byte $D9,$22, $8B,$22, $D9,$22, $8B,$62
MajObj_Frame_BE: ; 9508
	.byte $10, $3C
	.byte $96,$22, $96,$22, $96,$22, $96,$22
	.byte $96,$22, $96,$22, $96,$22, $96,$22
	.byte $8E,$22, $96,$22, $8E,$62, $96,$22
	.byte $8D,$22, $8F,$22, $8D,$62, $8F,$62
MajObj_Frame_BF: ; 952A
	.byte $10, $3C
	.byte $98,$62, $98,$22, $96,$22, $96,$22
	.byte $98,$22, $98,$22, $96,$22, $98,$62
	.byte $97,$22, $98,$22, $98,$62, $98,$62
	.byte $D9,$22, $97,$22, $97,$62, $97,$62
MajObj_Frame_C0: ; 954C
	.byte $0A, $1F
	.byte $E9,$03, $EA,$03, $EB,$03, $EC,$02
	.byte $ED,$02, $EE,$02, $EF,$02, $FD,$02
	.byte $FE,$02, $FF,$01
MajObj_Frame_C1: ; 9562
	.byte $09, $1F
	.byte $E0,$01, $E1,$01, $E2,$01, $E3,$01
	.byte $E4,$01, $E5,$01, $E6,$01, $E7,$01
	.byte $E8,$01
MajObj_Frame_C2: ; 9576
	.byte $0B, $04
	.byte $BA,$00, $BB,$00, $CA,$00, $CB,$00
	.byte $BE,$00, $BF,$00, $BC,$00, $BD,$00
	.byte $CF,$00, $00,$01, $B9,$00

; Minor object metasprite frame data
MinObj_Frame_0: ; 958E
	.byte $03, $2E
	.byte $B1,$03, $B2,$03, $B0,$03
MinObj_Frame_1: ; 9596
	.byte $02, $2E
	.byte $B3,$03, $B4,$03
MinObj_Frame_2: ; 959C
	.byte $03, $2E
	.byte $B6,$03, $B7,$03, $B5,$03
MinObj_Frame_3: ; 95A4
	.byte $03, $2E
	.byte $B9,$03, $BA,$03, $B8,$03
MinObj_Frame_4: ; 95AC
	.byte $02, $2A
	.byte $B0,$01, $B1,$01
MinObj_Frame_5: ; 95B2
	.byte $02, $2A
	.byte $BE,$01, $BF,$01
MinObj_Frame_6: ; 95B8
	.byte $04, $24
	.byte $A0,$02, $A1,$02, $A2,$02, $A3,$02
MinObj_Frame_7: ; 95C2
	.byte $04, $24
	.byte $A4,$02, $A5,$02, $A6,$02, $A7,$02
MinObj_Frame_8: ; 95CC
	.byte $10, $3C
	.byte $B9,$02, $B4,$02, $BB,$02, $B6,$02
	.byte $B8,$02, $BA,$02, $B5,$02, $B7,$02
	.byte $B5,$02, $B7,$02, $B8,$02, $BA,$02
	.byte $B4,$02, $B6,$02, $B9,$02, $BB,$02
MinObj_Frame_9: ; 95EE
	.byte $10, $3C
	.byte $B5,$02, $B8,$02, $B7,$02, $BA,$02
	.byte $B4,$02, $B6,$02, $B9,$02, $BB,$02
	.byte $B9,$02, $BB,$02, $B4,$02, $B6,$02
	.byte $B8,$02, $BA,$02, $B5,$02, $B7,$02
MinObj_Frame_A: ; 9610
	.byte $01, $00
	.byte $6A,$03
MinObj_Frame_B: ; 9614
	.byte $06, $01
	.byte $80,$02, $81,$02, $82,$02, $83,$02
	.byte $84,$02, $84,$02
MinObj_Frame_C: ; 9622
	.byte $06, $01
	.byte $8A,$02, $8B,$02, $8C,$02, $8D,$02
	.byte $8E,$02, $8F,$02
MinObj_Frame_D: ; 9630
	.byte $06, $01
	.byte $8B,$42, $8A,$42, $8C,$02, $8D,$02
	.byte $8E,$02, $8F,$02
MinObj_Frame_E: ; 963E
	.byte $16, $3B
	.byte $A3,$02, $9C,$02, $A3,$02, $98,$02
	.byte $99,$02, $9A,$02, $9D,$02, $9E,$02
	.byte $9F,$02, $A1,$02, $A0,$02, $A2,$02
	.byte $94,$02, $95,$02, $96,$02, $90,$02
	.byte $91,$02, $92,$02, $93,$02, $97,$02
	.byte $9B,$02, $A3,$02
MinObj_Frame_F: ; 966C
	.byte $16, $3B
	.byte $98,$02, $99,$02, $9A,$02, $94,$02
	.byte $95,$02, $96,$02, $9D,$02, $9E,$02
	.byte $9F,$02, $A1,$02, $A0,$02, $A2,$02
	.byte $90,$02, $91,$02, $92,$02, $A3,$02
	.byte $A3,$02, $A3,$02, $A3,$02, $93,$02
	.byte $97,$02, $9B,$02
MinObj_Frame_10: ; 969A
	.byte $06, $01
	.byte $85,$02, $86,$02, $87,$02, $88,$02
	.byte $89,$02, $89,$02
MinObj_Frame_11: ; 96A8
	.byte $06, $01
	.byte $85,$02, $86,$02, $87,$02, $8A,$02
	.byte $89,$02, $89,$02
MinObj_Frame_12: ; 96B6
	.byte $06, $01
	.byte $8C,$01, $8D,$01, $8E,$01, $8F,$01
	.byte $89,$01, $89,$01
MinObj_Frame_13: ; 96C4
	.byte $06, $01
	.byte $8B,$02, $86,$02, $87,$02, $88,$02
	.byte $89,$02, $89,$02
MinObj_Frame_14: ; 96D2
	.byte $02, $2F
	.byte $CF,$03, $BC,$03
MinObj_Frame_15: ; 96D8
	.byte $04, $24
	.byte $C1,$03, $C2,$03, $C3,$03, $C4,$03
MinObj_Frame_16: ; 96E2
	.byte $03, $2F
	.byte $BF,$03, $C0,$03, $CE,$03
MinObj_Frame_17: ; 96EA
	.byte $03, $2F
	.byte $BD,$03, $BE,$03, $CE,$03
MinObj_Frame_18: ; 96F2
	.byte $03, $2F
	.byte $CF,$03, $BC,$03, $CE,$03
MinObj_Frame_19: ; 96FA
	.byte $10, $40
	.byte $80,$23, $81,$23, $82,$23, $83,$23
	.byte $82,$23, $83,$23, $82,$23, $83,$23
	.byte $82,$23, $83,$23, $82,$23, $83,$23
	.byte $82,$23, $83,$23, $82,$23, $83,$23
MinObj_Frame_1A: ; 971C
	.byte $08, $3C
	.byte $93,$A2, $93,$E2, $91,$A2, $91,$E2
	.byte $92,$A2, $90,$A2, $92,$E2, $90,$E2
MinObj_Frame_1B: ; 972E
	.byte $08, $3C
	.byte $97,$A2, $97,$E2, $95,$A2, $95,$E2
	.byte $96,$A2, $94,$A2, $96,$E2, $94,$E2
MinObj_Frame_1C: ; 9740
	.byte $08, $3C
	.byte $9B,$A2, $9B,$E2, $99,$A2, $99,$E2
	.byte $9A,$A2, $98,$A2, $9A,$E2, $98,$E2
MinObj_Frame_31: ; 9752
	.byte $08, $24
	.byte $C2,$22, $C3,$22, $C2,$A2, $C3,$A2
	.byte $C0,$22, $C1,$22, $C0,$A2, $C1,$A2
MinObj_Frame_32: ; 9764
	.byte $08, $24
	.byte $C6,$22, $C7,$22, $C6,$A2, $C7,$A2
	.byte $C4,$22, $C5,$22, $C4,$A2, $C5,$A2
MinObj_Frame_33: ; 9776
	.byte $08, $24
	.byte $CA,$22, $CB,$22, $CA,$A2, $CB,$A2
	.byte $C8,$22, $C9,$22, $C8,$A2, $C9,$A2
MinObj_Frame_1D: ; 9788
	.byte $04, $24
	.byte $95,$02, $95,$42, $96,$02, $96,$42
MinObj_Frame_1E: ; 9792
	.byte $04, $24
MinObj_Frame_1F: ; 9794
	.byte $05, $30
	.byte $97,$02, $98,$02, $9B,$02, $9C,$02
	.byte $9D,$02
MinObj_Frame_20: ; 97A0
	.byte $04, $24
	.byte $80,$03, $80,$43, $80,$83, $80,$C3
MinObj_Frame_21: ; 97AA
	.byte $04, $24
	.byte $81,$03, $81,$43, $82,$03, $82,$43
MinObj_Frame_22: ; 97B4
	.byte $04, $24
	.byte $83,$03, $83,$43, $84,$03, $84,$43
MinObj_Frame_23: ; 97BE
	.byte $04, $24
	.byte $8B,$03, $8C,$03, $8D,$03, $8E,$03
MinObj_Frame_24: ; 97C8
	.byte $01, $00
	.byte $8A,$03
MinObj_Frame_25: ; 97CC
	.byte $04, $24
	.byte $C0,$03, $C1,$03, $C2,$03, $C3,$03
MinObj_Frame_26: ; 97D6
	.byte $06, $31
	.byte $C6,$03, $C7,$03, $C4,$03, $C5,$03
	.byte $C2,$03, $C3,$03
MinObj_Frame_27: ; 97E4
	.byte $05, $32
	.byte $C2,$03, $CE,$03, $CF,$03, $C0,$03
	.byte $C1,$03
MinObj_Frame_28: ; 97F0
	.byte $06, $32
	.byte $C8,$03, $C9,$03, $CB,$03, $CC,$03
	.byte $CD,$03, $CA,$03
MinObj_Frame_29: ; 97FE
	.byte $04, $24
	.byte $BB,$03, $BC,$03, $BD,$03, $BE,$03
MinObj_Frame_2A: ; 9808
	.byte $06, $2B
	.byte $B9,$01, $BB,$01, $B7,$01, $B8,$01
	.byte $BA,$01, $BC,$01
MinObj_Frame_2B: ; 9816
	.byte $06, $2B
	.byte $B9,$01, $BB,$01, $BD,$01, $BE,$01
	.byte $BA,$01, $BC,$01
MinObj_Frame_2C: ; 9824
	.byte $02, $2A
	.byte $CA,$03, $CB,$03
MinObj_Frame_2D: ; 982A
	.byte $02, $2A
	.byte $CC,$03, $CF,$03
MinObj_Frame_2E: ; 9830
	.byte $0C, $33
	.byte $90,$03, $91,$03, $92,$03, $94,$03
	.byte $95,$03, $96,$03, $98,$03, $99,$03
	.byte $9A,$03, $9B,$03, $97,$03, $93,$03
MinObj_Frame_2F: ; 984A
	.byte $0A, $34
	.byte $9C,$03, $9D,$03, $9E,$03, $A0,$03
	.byte $A1,$03, $A2,$03, $A4,$03, $A5,$03
	.byte $A6,$03, $A3,$03
MinObj_Frame_30: ; 9860
	.byte $09, $35
	.byte $A7,$03, $A8,$03, $A9,$03, $AA,$03
	.byte $AB,$03, $AC,$03, $AD,$03, $AE,$03
	.byte $AF,$03
MinObj_Frame_34: ; 9874
	.byte $03, $2F
	.byte $B5,$03, $B6,$03, $B7,$03
MinObj_Frame_35: ; 987C
	.byte $03, $36
	.byte $B8,$03, $B9,$03, $BA,$03
MinObj_Frame_36: ; 9884
	.byte $03, $37
	.byte $B5,$83, $B6,$83, $B7,$83
MinObj_Frame_37: ; 988C
	.byte $03, $2E
	.byte $B8,$43, $B9,$43, $BA,$43
MinObj_Frame_38: ; 9894
	.byte $0B, $33
	.byte $80,$02, $81,$02, $82,$02, $83,$02
	.byte $84,$02, $85,$02, $87,$02, $AD,$02
	.byte $AE,$02, $AF,$02, $86,$02
MinObj_Frame_39: ; 98AC
	.byte $09, $33
	.byte $90,$02, $91,$02, $92,$02, $93,$02
	.byte $94,$02, $95,$02, $96,$02, $97,$02
	.byte $98,$02
MinObj_Frame_3A: ; 98C0
	.byte $09, $33
	.byte $99,$02, $9A,$02, $9B,$02, $9C,$02
	.byte $9D,$02, $9E,$02, $9F,$02, $A0,$02
	.byte $A1,$02
MinObj_Frame_3B: ; 98D4
	.byte $0B, $3A
	.byte $A2,$02, $A3,$02, $A4,$02, $A6,$02
	.byte $A7,$02, $A8,$02, $A9,$02, $AA,$02
	.byte $AB,$02, $AC,$02, $A5,$02
MinObj_Frame_3C: ; 98EC
	.byte $08, $38
	.byte $80,$02, $81,$02, $83,$02, $84,$02
	.byte $82,$02, $85,$02, $86,$02, $89,$02
MinObj_Frame_3D: ; 98FE
	.byte $0A, $38
	.byte $8F,$02, $90,$02, $93,$02, $94,$02
	.byte $92,$02, $95,$02, $96,$02, $99,$02
	.byte $8E,$02, $91,$02
MinObj_Frame_3E: ; 9914
	.byte $08, $38
	.byte $80,$02, $81,$02, $A0,$02, $A1,$02
	.byte $82,$02, $85,$02, $86,$02, $89,$02
MinObj_Frame_3F: ; 9926
	.byte $09, $38
	.byte $A6,$02, $81,$02, $A0,$02, $A1,$02
	.byte $A7,$02, $85,$02, $A8,$02, $89,$02
	.byte $A5,$02
MinObj_Frame_40: ; 993A
	.byte $08, $38
	.byte $80,$02, $81,$02, $A0,$02, $A1,$02
	.byte $A2,$02, $85,$02, $A3,$02, $89,$02
MinObj_Frame_41: ; 994C
	.byte $06, $38
	.byte $87,$02, $88,$02, $8B,$02, $8C,$02
	.byte $8A,$02, $8D,$02
MinObj_Frame_42: ; 995A
	.byte $04, $38
	.byte $97,$02, $98,$02, $9A,$02, $9B,$02
MinObj_Frame_43: ; 9964
	.byte $04, $38
	.byte $9C,$02, $9D,$02, $9E,$02, $9F,$02
MinObj_Frame_44: ; 996E
	.byte $06, $39
	.byte $91,$02, $92,$02, $93,$02, $94,$02
	.byte $90,$02, $95,$02
MinObj_Frame_45: ; 997C
	.byte $06, $39
	.byte $97,$02, $98,$02, $93,$02, $94,$02
	.byte $96,$02, $99,$02
MinObj_Frame_46: ; 998A
	.byte $08, $39
	.byte $9C,$02, $9D,$02, $93,$02, $94,$02
	.byte $9B,$02, $9E,$02, $9A,$02, $9F,$02
MinObj_Frame_47: ; 999C
	.byte $10, $40
	.byte $80,$22, $81,$22, $82,$22, $83,$22
	.byte $82,$22, $83,$22, $82,$22, $83,$22
	.byte $82,$22, $83,$22, $82,$22, $83,$22
	.byte $82,$22, $83,$22, $82,$22, $83,$22
MinObj_Frame_48: ; 99BE
	.byte $10, $40
	.byte $84,$22, $85,$22, $86,$22, $87,$22
	.byte $86,$22, $87,$22, $86,$22, $87,$22
	.byte $86,$22, $87,$22, $86,$22, $87,$22
	.byte $86,$22, $87,$22, $86,$22, $87,$22
MinObj_Frame_49: ; 99E0
	.byte $10, $40
	.byte $88,$22, $89,$22, $8A,$22, $8B,$22
	.byte $8A,$22, $8B,$22, $8A,$22, $8B,$22
	.byte $8A,$22, $8B,$22, $8A,$22, $8B,$22
	.byte $8A,$22, $8B,$22, $8A,$22, $8B,$22
MinObj_Frame_4A: ; 9A02
	.byte $01, $00
	.byte $BC,$02
MinObj_Frame_4B: ; 9A06
	.byte $01, $00
	.byte $BD,$02
MinObj_Frame_4C: ; 9A0A
	.byte $08, $24
	.byte $A5,$03, $A6,$03, $A5,$83, $A6,$83
	.byte $A3,$03, $A4,$03, $A3,$83, $A4,$83
MinObj_Frame_4D: ; 9A1C
	.byte $04, $24
	.byte $AB,$03, $AC,$03, $AD,$03, $AE,$03
MinObj_Frame_4E: ; 9A26
	.byte $04, $24
	.byte $8D,$02, $8E,$02, $8E,$C2, $8D,$C2
MinObj_Frame_51: ; 9A30
	.byte $01, $00
	.byte $6A,$02
MinObj_Frame_52: ; 9A34
	.byte $01, $00
	.byte $D6,$01
MinObj_Frame_53: ; 9A38
	.byte $04, $24
	.byte $D7,$01, $D7,$41, $D7,$81, $D7,$C1
MinObj_Frame_54: ; 9A42
	.byte $04, $24
	.byte $D8,$01, $D8,$41, $D8,$81, $D8,$C1
MinObj_Frame_55: ; 9A4C
	.byte $04, $24
	.byte $D9,$01, $D9,$41, $D9,$81, $D9,$C1
MinObj_Frame_56: ; 9A56
	.byte $02, $37
	.byte $BC,$C3, $CF,$C3
MinObj_Frame_57: ; 9A5C
	.byte $04, $24
	.byte $C4,$C3, $C3,$C3, $C2,$C3, $C1,$C3
MinObj_Frame_58: ; 9A66
	.byte $03, $37
	.byte $C0,$C3, $BF,$C3, $CE,$C3
MinObj_Frame_59: ; 9A6E
	.byte $03, $37
	.byte $BE,$C3, $BD,$C3, $CE,$C3
MinObj_Frame_5A: ; 9A76
	.byte $03, $37
	.byte $BC,$C3, $CF,$C3, $CE,$C3
MinObj_Frame_5B: ; 9A7E
	.byte $03, $3B
	.byte $AC,$01, $AC,$81, $AC,$01
MinObj_Frame_5D: ; 9A86
	.byte $0E, $35
	.byte $82,$02, $83,$02, $84,$02, $86,$02
	.byte $87,$02, $88,$02, $8A,$02, $8B,$02
	.byte $8C,$02, $85,$02, $89,$02, $8D,$02
	.byte $80,$02, $81,$02
MinObj_Frame_5E: ; 9AA4
	.byte $01, $00
	.byte $A4,$02
MinObj_Frame_5F: ; 9AA8
	.byte $03, $3B
	.byte $AC,$81, $AC,$01, $AC,$81
MinObj_Frame_60: ; 9AB0
	.byte $09, $00
	.byte $A8,$03, $9E,$03, $9F,$03, $A0,$03
	.byte $A4,$03, $A5,$03, $A6,$03, $A7,$03
	.byte $A9,$03
MinObj_Frame_61: ; 9AC4
	.byte $09, $00
	.byte $A8,$03, $A1,$03, $A2,$03, $A3,$03
	.byte $A4,$03, $A5,$03, $A6,$03, $A7,$03
	.byte $A9,$03
MinObj_Frame_62: ; 9AD8
	.byte $09, $00
	.byte $9C,$03, $9E,$03, $9F,$03, $A0,$03
	.byte $A4,$03, $A5,$03, $A6,$03, $9B,$03
	.byte $9D,$03
MinObj_Frame_63: ; 9AEC
	.byte $09, $00
	.byte $9C,$03, $A1,$03, $A2,$03, $A3,$03
	.byte $A4,$03, $A5,$03, $A6,$03, $9B,$03
	.byte $9D,$03
MinObj_Frame_64: ; 9B00
	.byte $05, $3D
	.byte $9E,$02, $9A,$02, $9B,$02, $9C,$02
	.byte $9D,$02
MinObj_Frame_65: ; 9B0C
	.byte $07, $3E
	.byte $91,$02, $9F,$02, $90,$02, $92,$02
	.byte $93,$02, $94,$02, $95,$02
MinObj_Frame_66: ; 9B1C
	.byte $06, $3F
	.byte $97,$02, $9F,$02, $96,$02, $98,$02
	.byte $99,$02, $AF,$02
MinObj_Frame_67: ; 9B2A
	.byte $04, $24
	.byte $9C,$03, $9C,$43, $9D,$03, $9D,$43
MinObj_Frame_68: ; 9B34
	.byte $04, $24
	.byte $9E,$03, $9F,$03, $9E,$83, $9F,$83
MinObj_Frame_69: ; 9B3E
	.byte $04, $24
	.byte $9D,$83, $9D,$C3, $9C,$83, $9C,$C3
MinObj_Frame_6A: ; 9B48
	.byte $04, $24
	.byte $9F,$43, $9E,$43, $9F,$C3, $9E,$C3
MinObj_Frame_6B: ; 9B52
	.byte $04, $24
	.byte $90,$00, $91,$00, $92,$00, $93,$00
MinObj_Frame_6C: ; 9B5C
	.byte $04, $24
	.byte $94,$00, $95,$00, $96,$00, $97,$00
MinObj_Frame_6D: ; 9B66
	.byte $04, $24
	.byte $AA,$00, $AB,$00, $AC,$00, $AC,$40
MinObj_Frame_6E: ; 9B70
	.byte $04, $24
	.byte $B0,$00, $B1,$00, $B0,$80, $B1,$80
MinObj_Frame_6F: ; 9B7A
	.byte $04, $24
	.byte $B4,$00, $B5,$00, $B4,$80, $B5,$80
MinObj_Frame_70: ; 9B84
	.byte $04, $24
	.byte $B8,$00, $B9,$00, $B8,$80, $B9,$80
MinObj_Frame_71: ; 9B8E
	.byte $04, $24
	.byte $BC,$00, $BC,$40, $BC,$80, $BC,$C0
MinObj_Frame_72: ; 9B98
	.byte $04, $24
	.byte $BD,$00, $BD,$40, $BD,$80, $BD,$C0
MinObj_Frame_73: ; 9BA2
	.byte $04, $24
	.byte $BE,$00, $BE,$40, $BE,$80, $BE,$C0
MinObj_Frame_74: ; 9BAC
	.byte $04, $24
	.byte $80,$03, $80,$43, $80,$83, $80,$C3
MinObj_Frame_75: ; 9BB6
	.byte $04, $24
	.byte $81,$03, $82,$03, $81,$83, $82,$83
MinObj_Frame_76: ; 9BC0
	.byte $04, $24
	.byte $83,$03, $83,$43, $83,$83, $83,$C3
MinObj_Frame_77: ; 9BCA
	.byte $04, $24
	.byte $82,$43, $81,$43, $82,$C3, $81,$C3
MinObj_Frame_78: ; 9BD4
	.byte $04, $24
	.byte $84,$03, $84,$43, $84,$83, $84,$C3
MinObj_Frame_79: ; 9BDE
	.byte $04, $24
	.byte $85,$03, $85,$43, $85,$83, $85,$C3
MinObj_Frame_7A: ; 9BE8
	.byte $0C, $41
	.byte $81,$03, $82,$03, $83,$03, $84,$03
	.byte $85,$03, $86,$03, $87,$03, $88,$02
	.byte $89,$03, $8D,$03, $8E,$03, $8F,$03
MinObj_Frame_7B: ; 9C02
	.byte $10, $41
	.byte $81,$03, $82,$03, $83,$03, $84,$03
	.byte $85,$03, $86,$03, $87,$03, $88,$02
	.byte $89,$03, $8D,$03, $8E,$03, $8F,$03
	.byte $93,$01, $94,$01, $95,$01, $96,$01
MinObj_Frame_7C: ; 9C24
	.byte $18, $41
	.byte $81,$03, $82,$03, $83,$03, $84,$03
	.byte $85,$03, $86,$03, $87,$03, $88,$02
	.byte $89,$03, $8D,$03, $8E,$03, $8F,$03
	.byte $A8,$01, $B6,$01, $A8,$81, $B6,$01
	.byte $97,$01, $A5,$01, $A6,$01, $A7,$01
	.byte $A9,$01, $A7,$81, $97,$81, $A5,$81
MinObj_Frame_7D: ; 9C56
	.byte $30, $41
	.byte $81,$03, $82,$03, $83,$03, $84,$03
	.byte $85,$03, $86,$03, $87,$03, $88,$02
	.byte $89,$03, $8D,$03, $8E,$03, $8F,$03
	.byte $CD,$01, $B6,$01, $CD,$81, $B6,$01
	.byte $CE,$01, $CF,$01, $B3,$01, $B4,$01
	.byte $B6,$01, $B6,$01, $B5,$C1, $B6,$01
	.byte $CB,$01, $CC,$01, $CC,$81, $CB,$81
	.byte $CA,$01, $C9,$01, $C8,$01, $C8,$41
	.byte $C9,$41, $CA,$41, $B5,$01, $CB,$41
	.byte $CC,$41, $CD,$41, $CD,$C1, $CC,$C1
	.byte $CB,$C1, $B5,$81, $CA,$81, $C9,$81
	.byte $C8,$81, $C8,$C1, $C9,$C1, $CA,$C1
MinObj_Frame_7E: ; 9CB8
	.byte $01, $00
	.byte $D3,$03
MinObj_Frame_7F: ; 9CBC
	.byte $02, $2A
	.byte $D2,$00, $D2,$40
MinObj_Frame_80: ; 9CC2
	.byte $04, $24
	.byte $67,$01, $67,$41, $67,$81, $67,$C1
MinObj_Frame_81: ; 9CCC
	.byte $04, $24
	.byte $68,$00, $68,$40, $68,$80, $68,$C0
MinObj_Frame_82: ; 9CD6
	.byte $04, $24
	.byte $6B,$00, $6B,$40, $6B,$80, $6B,$C0
MinObj_Frame_83: ; 9CE0
	.byte $04, $24
	.byte $65,$01, $65,$41, $65,$81, $65,$C1
MinObj_Frame_84: ; 9CEA
	.byte $04, $24
	.byte $66,$01, $66,$41, $66,$81, $66,$C1
MinObj_Frame_85: ; 9CF4
	.byte $04, $24
	.byte $D0,$00, $D0,$40, $D1,$01, $D1,$41
MinObj_Frame_86: ; 9CFE
	.byte $04, $24
	.byte $AE,$00, $AF,$00, $AE,$80, $AF,$80
MinObj_Frame_87: ; 9D08
	.byte $04, $24
	.byte $BD,$01, $BD,$41, $BD,$81, $BD,$C1
MinObj_Frame_88: ; 9D12
	.byte $04, $24
	.byte $BE,$01, $BE,$41, $BE,$81, $BE,$C1
MinObj_Frame_89: ; 9D1C
	.byte $04, $24
	.byte $BF,$01, $BF,$41, $BF,$81, $BF,$C1
MinObj_Frame_8A: ; 9D26
	.byte $04, $24
	.byte $D4,$02, $D4,$42, $D4,$82, $D4,$C2
MinObj_Frame_8B: ; 9D30
	.byte $04, $24
	.byte $D4,$01, $D4,$41, $D4,$81, $D4,$C1
MinObj_Frame_8C: ; 9D3A
	.byte $09, $00
	.byte $94,$01, $96,$01, $97,$01, $98,$01
	.byte $90,$01, $91,$01, $92,$01, $93,$01
	.byte $95,$91
MinObj_Frame_8D: ; 9D4E
	.byte $08, $3C
	.byte $9A,$01, $9B,$01, $9E,$01, $9F,$01
	.byte $99,$01, $9D,$01, $9C,$01, $A0,$01
MinObj_Frame_8E: ; 9D60
	.byte $08, $3C
	.byte $A2,$01, $A3,$01, $A5,$01, $9F,$01
	.byte $A1,$01, $A4,$01, $9C,$01, $A0,$01

; Metasprite tile offset (shape) index tables
TileOffsTable_0: ; 9D72
	.byte $04,$06,$07,$08,$00,$01,$02,$03,$05,$09,$33,$32,$34
TileOffsTable_1: ; 9D7F
	.byte $0A,$0B,$0D,$0E,$10,$11,$12,$13,$0F,$0C
TileOffsTable_2: ; 9D89
	.byte $0A,$0B,$0D,$0E,$10,$11,$0F,$09,$1A
TileOffsTable_3: ; 9D92
	.byte $0A,$0B,$0D,$0E,$10,$11,$09,$18,$0C,$16
TileOffsTable_4: ; 9D9C
	.byte $0A,$0B,$0D,$0E,$10,$11,$15,$16,$17,$09,$14
TileOffsTable_5: ; 9DA7
	.byte $0A,$0B,$0D,$0E,$10,$11,$18,$16,$1B,$09,$1C
TileOffsTable_6: ; 9DB2
	.byte $0A,$0B,$0D,$0E,$10,$11,$15,$16,$17,$09,$1A
TileOffsTable_7: ; 9DBD
	.byte $00,$01,$03,$04,$06,$07,$08,$1E,$1D,$1F
TileOffsTable_8: ; 9DC7
	.byte $0A,$0B,$0D,$0E,$10,$11,$13,$0C,$12
TileOffsTable_9: ; 9DD0
	.byte $20,$01,$04,$07,$00,$03,$06,$02,$05,$08
TileOffsTable_A: ; 9DDA
	.byte $0A,$0B,$0D,$0E,$10,$11,$21,$1C,$09
TileOffsTable_B: ; 9DE3
	.byte $0A,$0B,$0D,$0E,$11,$10,$22,$79,$83,$26,$0F
TileOffsTable_C: ; 9DEE
	.byte $23,$24,$25
TileOffsTable_D: ; 9DF1
	.byte $00,$01,$02,$03,$04,$05,$06,$07,$08,$27,$28,$4B,$4C
TileOffsTable_E: ; 9DFE
	.byte $00,$01,$02,$03,$04,$05,$06,$07,$08,$20,$29
TileOffsTable_F: ; 9E09
	.byte $00,$01,$03,$04,$06,$07,$1D,$A1,$08
TileOffsTable_10: ; 9E12
	.byte $00,$01,$02,$03,$04,$05,$06,$07,$08,$2E,$2F
TileOffsTable_11: ; 9E1D
	.byte $0A,$0B,$0C,$0D,$0E,$0F,$10,$11,$12,$81,$82
TileOffsTable_12: ; 9E28
	.byte $0A,$0B,$0D,$0E,$0F,$10,$11,$12,$81,$82
TileOffsTable_13: ; 9E32
	.byte $0A,$0B,$0D,$0E,$0F,$10,$11,$2C,$2D
TileOffsTable_14: ; 9E3B
	.byte $37,$38,$39,$3A,$3B,$3C,$3D,$3E,$30,$3F
TileOffsTable_15: ; 9E45
	.byte $00,$01,$02,$03,$04,$05,$35,$06,$07,$08,$7A,$87,$88,$89,$8A,$36
	.byte $32
TileOffsTable_16: ; 9E56
	.byte $00,$01,$02,$03,$04,$05,$06,$07,$08,$40,$7B,$7C
TileOffsTable_17: ; 9E62
	.byte $00,$01,$02,$03,$04,$05,$06,$07,$08,$41
TileOffsTable_18: ; 9E6C
	.byte $00,$01,$02,$03,$04,$05,$06,$07,$08,$42
TileOffsTable_19: ; 9E76
	.byte $00,$01,$02,$03,$04,$05,$06,$07,$08,$43
TileOffsTable_1A: ; 9E80
	.byte $00,$01,$02,$03,$04,$05,$06,$07,$08,$44,$45
TileOffsTable_1B: ; 9E8B
	.byte $00,$01,$02,$03,$04,$05,$06,$07,$08,$46,$7B,$7C
TileOffsTable_1C: ; 9E97
	.byte $00,$01,$02,$03,$04,$05,$06,$07,$08,$47,$7D,$7E
TileOffsTable_1D: ; 9EA3
	.byte $0A,$0B,$0D,$0E,$0F,$10,$11,$12,$13,$7F,$80,$0C
TileOffsTable_1E: ; 9EAF
	.byte $0A,$0B,$0D,$0E,$0F,$10,$11,$48,$2A,$2B
TileOffsTable_1F: ; 9EB9
	.byte $00,$01,$02,$03,$04,$05,$06,$07,$08,$4D
TileOffsTable_20: ; 9EC3
	.byte $00,$01,$02,$03,$04,$05,$06,$07,$08,$4E
TileOffsTable_21: ; 9ECD
	.byte $00,$01,$02,$03,$04,$05,$06,$07,$08,$4A,$49
TileOffsTable_22: ; 9ED8
	.byte $00,$01,$03,$04,$06,$07,$4F,$1D,$08
TileOffsTable_23: ; 9EE1
	.byte $00,$01,$02,$03,$04,$05,$06,$07,$08,$50,$2A,$29
TileOffsTable_24: ; 9EED
	.byte $55,$56,$59,$5A,$52,$53,$5D,$5E,$51,$54,$19,$57,$58,$5B,$5C,$5F
TileOffsTable_25: ; 9EFD
	.byte $43,$64,$66,$67,$60,$61,$62,$63,$1C,$18,$65,$68,$69,$6A,$6B,$6C
	.byte $8C,$8D,$8E,$8F,$90,$91,$92,$93,$94,$95,$96,$97,$98,$99,$9A,$9B
TileOffsTable_26: ; 9F1D
	.byte $72,$73,$76,$77,$6D,$6E,$6F,$70,$71,$74,$75,$78,$9D,$9E,$9F,$A0
TileOffsTable_27: ; 9F2D
	.byte $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$84,$8B,$85,$86
TileOffsTable_28: ; 9F3B
	.byte $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$87,$88,$89,$8A,$33,$32
	.byte $34
TileOffsTable_29: ; 9F4C
	.byte $00,$01,$02,$03,$04,$05,$06,$07,$08,$40,$9C,$7B,$7C
TileOffsTable_2A: ; 9F59
	.byte $0D,$0E
TileOffsTable_2B: ; 9F5B
	.byte $A2,$A3,$A6,$AB,$A7,$AC,$A5,$AA,$A8,$AD,$B2,$A9,$AE,$AF
TileOffsTable_2C: ; 9F69
	.byte $00,$01,$03,$04,$06,$07,$08,$1E,$4C,$1F,$A4
TileOffsTable_2D: ; 9F74
	.byte $A5,$A6,$A2,$A7,$A8,$B0,$AA,$AB,$A3,$AC,$AD,$B1
TileOffsTable_2E: ; 9F80
	.byte $56,$5A,$0D
TileOffsTable_2F: ; 9F83
	.byte $55,$56,$A3
TileOffsTable_30: ; 9F86
	.byte $55,$56,$59,$5A,$CB
TileOffsTable_31: ; 9F8B
	.byte $0A,$0B,$B3,$B4,$10,$11
TileOffsTable_32: ; 9F91
	.byte $B5,$B6,$B8,$B9,$BA,$B7
TileOffsTable_33: ; 9F97
	.byte $BB,$BC,$BD,$BF,$C0,$C1,$C3,$C4,$C5,$C6,$C2,$BE
TileOffsTable_34: ; 9FA3
	.byte $BB,$BC,$BD,$BF,$C0,$C1,$C3,$C4,$C5,$C7
TileOffsTable_35: ; 9FAD
	.byte $83,$0A,$0B,$0C,$0D,$0E,$16,$10,$11,$26,$0F,$12,$2A,$2B
TileOffsTable_36: ; 9FBB
	.byte $55,$59,$0E
TileOffsTable_37: ; 9FBE
	.byte $59,$5A,$A2
TileOffsTable_38: ; 9FC1
	.byte $55,$56,$59,$5A,$58,$5B,$5C,$5F,$19,$57
TileOffsTable_39: ; 9FCB
	.byte $55,$56,$59,$5A,$C8,$CB,$C9,$CA
TileOffsTable_3A: ; 9FD3
	.byte $0A,$0B,$26,$0D,$0E,$0F,$10,$11,$21,$22,$CC
TileOffsTable_3B: ; 9FDE
	.byte $03,$04,$05,$00,$01,$02,$06,$07,$08,$32,$DE,$DF,$D8,$20,$29,$D9
	.byte $DA,$DB,$DC,$DD,$D5,$35
TileOffsTable_3C: ; 9FF4
	.byte $55,$56,$59,$5A,$19,$58,$57,$5B,$CD,$CE,$CF,$D0,$D1,$D2,$D3,$D4
TileOffsTable_3D: ; A004
	.byte $4C,$49,$00,$01,$02
TileOffsTable_3E: ; A009
	.byte $4C,$49,$00,$03,$04,$06,$07
TileOffsTable_3F: ; A010
	.byte $4C,$49,$00,$03,$D6,$D7
TileOffsTable_40: ; A016
	.byte $E4,$E5,$E6,$E7,$52,$53,$55,$56,$59,$5A,$5D,$5E,$E0,$E1,$E2,$E3
TileOffsTable_41: ; A026
	.byte $52,$53,$19,$55,$56,$57,$58,$59,$5A,$5B,$5D,$5E,$D1,$CD,$D2,$CE
	.byte $ED,$EE,$F2,$51,$F5,$5C,$F8,$F9,$EC,$F1,$F4,$F7,$E8,$E9,$E4,$E5
	.byte $EA,$EB,$EF,$F0,$F3,$D3,$D4,$F6,$FB,$FA,$FC,$FD,$E2,$E3,$FE,$FF
TileOffsTable_42: ; A056
	.byte $01,$04
TileOffsTable_43: ; A058
	.byte $04,$07
TileOffsTable_44: ; A05A
	.byte $D1,$CD,$19,$55,$56,$57,$58,$59,$5A,$5B,$D0,$D4

; Major object metasprite state frame index tables
MajObj_FrameIdcs_0: ; A066
	.byte $A8,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00
MajObj_FrameIdcs_1: ; A072
	.byte $A8,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
MajObj_FrameIdcs_2: ; A07E
	.byte $A8,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$08
MajObj_FrameIdcs_3: ; A08A
	.byte $22,$01,$02,$02
MajObj_FrameIdcs_4: ; A08E
	.byte $22,$10,$10,$10
MajObj_FrameIdcs_5: ; A092
	.byte $22,$09,$0A,$0A
MajObj_FrameIdcs_6: ; A096
	.byte $36,$03,$05,$04,$05
MajObj_FrameIdcs_7: ; A09B
	.byte $36,$11,$13,$12,$13
MajObj_FrameIdcs_8: ; A0A0
	.byte $36,$0B,$0D,$0C,$0D
MajObj_FrameIdcs_9: ; A0A5
	.byte $00,$06
MajObj_FrameIdcs_A: ; A0A7
	.byte $00,$14
MajObj_FrameIdcs_B: ; A0A9
	.byte $00,$0E
MajObj_FrameIdcs_C: ; A0AB
	.byte $16,$05,$02
MajObj_FrameIdcs_D: ; A0AE
	.byte $17,$13,$10
MajObj_FrameIdcs_E: ; A0B1
	.byte $17,$0D,$0A
MajObj_FrameIdcs_F: ; A0B4
	.byte $11,$04,$03
MajObj_FrameIdcs_10: ; A0B7
	.byte $11,$12,$11
MajObj_FrameIdcs_11: ; A0BA
	.byte $11,$0C,$0B
MajObj_FrameIdcs_12: ; A0BD
	.byte $92,$1A,$0F,$0F,$1A,$0F,$0F,$1A,$0F,$0F,$1A
MajObj_FrameIdcs_13: ; A0C8
	.byte $57,$07,$07,$07,$07,$07,$07
MajObj_FrameIdcs_14: ; A0CF
	.byte $32,$15,$16,$17,$15
MajObj_FrameIdcs_15: ; A0D4
	.byte $18,$18,$19
MajObj_FrameIdcs_16: ; A0D7
	.byte $18,$1B,$1B
MajObj_FrameIdcs_17: ; A0DA
	.byte $1A,$1C,$1D
MajObj_FrameIdcs_18: ; A0DD
	.byte $1A,$1B,$1B
MajObj_FrameIdcs_19: ; A0E0
	.byte $28,$1E,$1F,$20
MajObj_FrameIdcs_1A: ; A0E4
	.byte $00,$21
MajObj_FrameIdcs_1B: ; A0E6
	.byte $18,$22,$24
MajObj_FrameIdcs_1C: ; A0E9
	.byte $02,$22
MajObj_FrameIdcs_1D: ; A0EB
	.byte $0F,$57
MajObj_FrameIdcs_1E: ; A0ED
	.byte $5C,$57,$57,$58,$58,$30,$30
MajObj_FrameIdcs_1F: ; A0F4
	.byte $0F,$39
MajObj_FrameIdcs_20: ; A0F6
	.byte $28,$37,$37,$38
MajObj_FrameIdcs_21: ; A0FA
	.byte $28,$3F,$3E,$3E
MajObj_FrameIdcs_22: ; A0FE
	.byte $28,$3F,$3E,$3E
MajObj_FrameIdcs_23: ; A102
	.byte $0F,$49
MajObj_FrameIdcs_24: ; A104
	.byte $28,$47,$47,$48
MajObj_FrameIdcs_25: ; A108
	.byte $1E,$4F,$50
MajObj_FrameIdcs_26: ; A10B
	.byte $02,$4F
MajObj_FrameIdcs_27: ; A10D
	.byte $1D,$5A,$5A
MajObj_FrameIdcs_28: ; A110
	.byte $00,$59
MajObj_FrameIdcs_29: ; A112
	.byte $00,$34
MajObj_FrameIdcs_2A: ; A114
	.byte $00,$36
MajObj_FrameIdcs_2B: ; A116
	.byte $0F,$33
MajObj_FrameIdcs_2C: ; A118
	.byte $0F,$32
MajObj_FrameIdcs_2D: ; A11A
	.byte $00,$51
MajObj_FrameIdcs_2E: ; A11C
	.byte $00,$54
MajObj_FrameIdcs_2F: ; A11E
	.byte $00,$52
MajObj_FrameIdcs_30: ; A120
	.byte $00,$53
MajObj_FrameIdcs_31: ; A122
	.byte $00,$55
MajObj_FrameIdcs_32: ; A124
	.byte $2F,$5B,$5B,$5B
MajObj_FrameIdcs_33: ; A128
	.byte $28,$3F,$3F,$3E
MajObj_FrameIdcs_34: ; A12C
	.byte $36,$42,$44,$43,$44
MajObj_FrameIdcs_35: ; A131
	.byte $28,$46,$45,$45
MajObj_FrameIdcs_36: ; A135
	.byte $27,$64,$65,$66
MajObj_FrameIdcs_37: ; A139
	.byte $26,$64,$65,$66
MajObj_FrameIdcs_38: ; A13D
	.byte $BA,$5F,$5E,$5E,$5F,$5E,$5E,$5F,$5E,$5E,$5F,$5E,$5E
MajObj_FrameIdcs_39: ; A14A
	.byte $80,$60,$60,$61,$62,$63,$60,$61,$62,$63
MajObj_FrameIdcs_3A: ; A154
	.byte $18,$67,$68
MajObj_FrameIdcs_3B: ; A157
	.byte $1C,$3B,$3C
MajObj_FrameIdcs_3C: ; A15A
	.byte $00,$37
MajObj_FrameIdcs_3D: ; A15C
	.byte $00,$3D
MajObj_FrameIdcs_3E: ; A15E
	.byte $00,$3A
MajObj_FrameIdcs_3F: ; A160
	.byte $36,$2A,$2E,$2C,$2E
MajObj_FrameIdcs_40: ; A165
	.byte $36,$2B,$2F,$2D,$2F
MajObj_FrameIdcs_41: ; A16A
	.byte $00,$28
MajObj_FrameIdcs_42: ; A16C
	.byte $00,$29
MajObj_FrameIdcs_43: ; A16E
	.byte $1F,$26,$27
MajObj_FrameIdcs_44: ; A171
	.byte $1F,$27,$27
MajObj_FrameIdcs_45: ; A174
	.byte $34,$69,$6A,$6B,$6C
MajObj_FrameIdcs_46: ; A179
	.byte $18,$22,$24
MajObj_FrameIdcs_47: ; A17C
	.byte $18,$23,$25
MajObj_FrameIdcs_48: ; A17F
	.byte $16,$4A,$4B
MajObj_FrameIdcs_49: ; A182
	.byte $00,$4E
MajObj_FrameIdcs_4A: ; A184
	.byte $00,$4D
MajObj_FrameIdcs_4B: ; A186
	.byte $00,$4C
MajObj_FrameIdcs_4C: ; A188
	.byte $41,$6D,$6D,$6D,$6D,$6D
MajObj_FrameIdcs_4D: ; A18E
	.byte $41,$6E,$6E,$6E,$6E,$6E
MajObj_FrameIdcs_4E: ; A194
	.byte $41,$6F,$6F,$6F,$6F,$6F
MajObj_FrameIdcs_4F: ; A19A
	.byte $41,$70,$70,$70,$70,$70
MajObj_FrameIdcs_50: ; A1A0
	.byte $41,$71,$71,$71,$71,$71
MajObj_FrameIdcs_51: ; A1A6
	.byte $41,$72,$72,$72,$72,$72
MajObj_FrameIdcs_52: ; A1AC
	.byte $41,$73,$73,$73,$73,$73
MajObj_FrameIdcs_53: ; A1B2
	.byte $41,$74,$74,$74,$74,$74
MajObj_FrameIdcs_54: ; A1B8
	.byte $70,$61,$62,$63,$60,$61,$62,$63,$60
MajObj_FrameIdcs_55: ; A1C1
	.byte $00,$76
MajObj_FrameIdcs_56: ; A1C3
	.byte $00,$77
MajObj_FrameIdcs_57: ; A1C5
	.byte $00,$2E
MajObj_FrameIdcs_58: ; A1C7
	.byte $00,$2F
MajObj_FrameIdcs_59: ; A1C9
	.byte $00,$4B
MajObj_FrameIdcs_5A: ; A1CB
	.byte $1C,$5A,$5A
MajObj_FrameIdcs_5B: ; A1CE
	.byte $34,$79,$7C,$7B,$7A
MajObj_FrameIdcs_5C: ; A1D3
	.byte $18,$7D,$7E
MajObj_FrameIdcs_5D: ; A1D6
	.byte $00,$7F
MajObj_FrameIdcs_5E: ; A1D8
	.byte $00,$80
MajObj_FrameIdcs_5F: ; A1DA
	.byte $40,$60,$61,$62,$63,$60
MajObj_FrameIdcs_60: ; A1E0
	.byte $26,$84,$85,$86
MajObj_FrameIdcs_61: ; A1E4
	.byte $30,$87,$88,$89,$88
MajObj_FrameIdcs_62: ; A1E9
	.byte $41,$8A,$8A,$8A,$8A,$8A
MajObj_FrameIdcs_63: ; A1EF
	.byte $41,$8B,$8B,$8B,$8B,$8B
MajObj_FrameIdcs_64: ; A1F5
	.byte $41,$8C,$8C,$8C,$8C,$8C
MajObj_FrameIdcs_65: ; A1FB
	.byte $41,$8D,$8D,$8D,$8D,$8D
MajObj_FrameIdcs_66: ; A201
	.byte $41,$8E,$8E,$8E,$8E,$8E
MajObj_FrameIdcs_67: ; A207
	.byte $41,$8F,$8F,$8F,$8F,$8F
MajObj_FrameIdcs_68: ; A20D
	.byte $41,$90,$90,$90,$90,$90
MajObj_FrameIdcs_69: ; A213
	.byte $41,$91,$91,$91,$91,$91
MajObj_FrameIdcs_6A: ; A219
	.byte $20,$92,$93,$94
MajObj_FrameIdcs_6B: ; A21D
	.byte $21,$92,$94,$93
MajObj_FrameIdcs_6C: ; A221
	.byte $00,$95
MajObj_FrameIdcs_6D: ; A223
	.byte $00,$81
MajObj_FrameIdcs_6E: ; A225
	.byte $18,$97,$97
MajObj_FrameIdcs_6F: ; A228
	.byte $00,$98
MajObj_FrameIdcs_70: ; A22A
	.byte $00,$99
MajObj_FrameIdcs_71: ; A22C
	.byte $00,$9A
MajObj_FrameIdcs_72: ; A22E
	.byte $18,$9A,$9B
MajObj_FrameIdcs_73: ; A231
	.byte $1F,$9A,$9C
MajObj_FrameIdcs_74: ; A234
	.byte $00,$9D
MajObj_FrameIdcs_75: ; A236
	.byte $28,$9E,$9F,$A0
MajObj_FrameIdcs_76: ; A23A
	.byte $00,$A1
MajObj_FrameIdcs_77: ; A23C
	.byte $20,$82,$83,$96
MajObj_FrameIdcs_78: ; A240
	.byte $20,$82,$96,$83
MajObj_FrameIdcs_79: ; A244
	.byte $00,$A2
MajObj_FrameIdcs_7A: ; A246
	.byte $44,$A5,$A4,$A3,$A4,$A5
MajObj_FrameIdcs_7B: ; A24C
	.byte $12,$A6,$A7
MajObj_FrameIdcs_7C: ; A24F
	.byte $00,$A8
MajObj_FrameIdcs_7D: ; A251
	.byte $40,$A9,$AA,$AB,$AC,$A9
MajObj_FrameIdcs_7E: ; A257
	.byte $00,$AD
MajObj_FrameIdcs_7F: ; A259
	.byte $11,$AE,$AF
MajObj_FrameIdcs_80: ; A25C
	.byte $00,$B0
MajObj_FrameIdcs_81: ; A25E
	.byte $00,$B1
MajObj_FrameIdcs_82: ; A260
	.byte $00,$B2
MajObj_FrameIdcs_83: ; A262
	.byte $1F,$B3,$B4
MajObj_FrameIdcs_84: ; A265
	.byte $00,$B5
MajObj_FrameIdcs_85: ; A267
	.byte $00,$B6
MajObj_FrameIdcs_86: ; A269
	.byte $00,$B7
MajObj_FrameIdcs_87: ; A26B
	.byte $00,$B8
MajObj_FrameIdcs_88: ; A26D
	.byte $00,$B9
MajObj_FrameIdcs_89: ; A26F
	.byte $00,$BA
MajObj_FrameIdcs_8A: ; A271
	.byte $00,$BB
MajObj_FrameIdcs_8B: ; A273
	.byte $00,$BC
MajObj_FrameIdcs_8C: ; A275
	.byte $00,$BD
MajObj_FrameIdcs_8D: ; A277
	.byte $00,$BE
MajObj_FrameIdcs_8E: ; A279
	.byte $00,$BF
MajObj_FrameIdcs_8F: ; A27B
	.byte $00,$C0
MajObj_FrameIdcs_90: ; A27D
	.byte $00,$C1
MajObj_FrameIdcs_91: ; A27F
	.byte $00,$C2

; Minor object metasprite state frame index tables
MinObj_FrameIdcs_0: ; A281
	.byte $12,$0C,$0D
MinObj_FrameIdcs_1: ; A284
	.byte $1F,$27,$28
MinObj_FrameIdcs_2: ; A287
	.byte $14,$1D,$1F
MinObj_FrameIdcs_3: ; A28A
	.byte $14,$2C,$2D
MinObj_FrameIdcs_4: ; A28D
	.byte $38,$20,$21,$22,$21
MinObj_FrameIdcs_5: ; A292
	.byte $64,$14,$15,$16,$17,$18,$15,$14
MinObj_FrameIdcs_6: ; A29A
	.byte $38,$44,$45,$46,$45
MinObj_FrameIdcs_7: ; A29F
	.byte $12,$2A,$2B
MinObj_FrameIdcs_8: ; A2A2
	.byte $00,$29
MinObj_FrameIdcs_9: ; A2A4
	.byte $14,$0F,$0E
MinObj_FrameIdcs_A: ; A2A7
	.byte $1F,$25,$26
MinObj_FrameIdcs_18: ; A2AA
	.byte $16,$4A,$4B
MinObj_FrameIdcs_19: ; A2AD
	.byte $12,$04,$05
MinObj_FrameIdcs_B: ; A2B0
	.byte $66,$00,$01,$02,$03,$02,$01,$00
MinObj_FrameIdcs_C: ; A2B8
	.byte $12,$08,$09
MinObj_FrameIdcs_D: ; A2BB
	.byte $00,$23
MinObj_FrameIdcs_E: ; A2BD
	.byte $54,$1A,$1B,$1C,$31,$32,$33
MinObj_FrameIdcs_F: ; A2C4
	.byte $34,$47,$48,$49,$19
MinObj_FrameIdcs_10: ; A2C9
	.byte $54,$0B,$10,$11,$12,$13,$10
MinObj_FrameIdcs_11: ; A2D0
	.byte $19,$4D,$4C
MinObj_FrameIdcs_12: ; A2D3
	.byte $36,$3B,$39,$3A,$38
MinObj_FrameIdcs_13: ; A2D8
	.byte $B4,$60,$61,$60,$61,$60,$61,$62,$63,$62,$63,$62,$63
MinObj_FrameIdcs_14: ; A2E5
	.byte $00,$5D
MinObj_FrameIdcs_15: ; A2E7
	.byte $3F,$2E,$2F,$30,$2F
MinObj_FrameIdcs_16: ; A2EC
	.byte $64,$56,$57,$58,$59,$5A,$57,$56
MinObj_FrameIdcs_1A: ; A2F4
	.byte $00,$51
MinObj_FrameIdcs_1B: ; A2F6
	.byte $32,$52,$53,$54,$55
MinObj_FrameIdcs_1C: ; A2FB
	.byte $00,$50
MinObj_FrameIdcs_1D: ; A2FD
	.byte $36,$3C,$3D,$3C,$3D
MinObj_FrameIdcs_1E: ; A302
	.byte $36,$41,$42,$43,$42
MinObj_FrameIdcs_1F: ; A307
	.byte $2F,$3E,$3F,$40
MinObj_FrameIdcs_20: ; A30B
	.byte $08,$5E
MinObj_FrameIdcs_26: ; A30D
	.byte $03,$24
MinObj_FrameIdcs_2A: ; A30F
	.byte $00,$46
MinObj_FrameIdcs_2B: ; A311
	.byte $12,$5C,$5F
MinObj_FrameIdcs_2C: ; A314
	.byte $32,$64,$65,$66,$65
MinObj_FrameIdcs_2D: ; A319
	.byte $00,$0A
MinObj_FrameIdcs_2E: ; A31B
	.byte $32,$34,$35,$36,$37
MinObj_FrameIdcs_45: ; A320
	.byte $00,$50
MinObj_FrameIdcs_2F: ; A322
	.byte $00,$00
MinObj_FrameIdcs_31: ; A324
	.byte $18,$06,$07
MinObj_FrameIdcs_33: ; A327
	.byte $34,$67,$68,$69,$6A
MinObj_FrameIdcs_34: ; A32C
	.byte $18,$6B,$6C
MinObj_FrameIdcs_35: ; A32F
	.byte $00,$6D
MinObj_FrameIdcs_36: ; A331
	.byte $24,$6E,$6F,$70
MinObj_FrameIdcs_37: ; A335
	.byte $30,$71,$72,$73,$71
MinObj_FrameIdcs_38: ; A33A
	.byte $94,$79,$78,$74,$74,$75,$76,$77,$74,$78,$79
MinObj_FrameIdcs_3A: ; A345
	.byte $34,$7A,$7B,$7C,$7D
MinObj_FrameIdcs_3C: ; A34A
	.byte $00,$7E
MinObj_FrameIdcs_3D: ; A34C
	.byte $00,$7F
MinObj_FrameIdcs_3E: ; A34E
	.byte $00,$80
MinObj_FrameIdcs_3F: ; A350
	.byte $18,$81,$82
MinObj_FrameIdcs_40: ; A353
	.byte $18,$83,$84
MinObj_FrameIdcs_41: ; A356
	.byte $00,$85
MinObj_FrameIdcs_42: ; A358
	.byte $00,$86
MinObj_FrameIdcs_44: ; A35A
	.byte $18,$8A,$8B
MinObj_FrameIdcs_48: ; A35D
	.byte $00,$87
MinObj_FrameIdcs_4A: ; A35F
	.byte $BF,$8C,$8D,$8E,$8D,$8E,$8D,$8E,$8D,$8E,$8D,$8E,$8D,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00
	
DefaultObjectFlags: ;at A380
    .byte $06,$06,$17,$17,$07,$06,$06,$06,$06,$17,$06,$06,$20,$22,$52,$22
    .byte $20,$20,$17,$42,$17,$17,$06,$46,$02,$20,$02,$00,$20,$20,$17,$06
    .byte $06,$07,$06,$46,$20,$20,$00,$00,$00,$00,$00,$00,$00,$00,$00,$20
    .byte $20,$20,$20,$00,$00,$00,$00,$00,$00,$00,$00,$20,$00,$02,$02,$02
    .byte $02,$02,$02,$22,$02,$02

DefaultObjectSpeedCtrl: ;at A3C6
    .byte $b0,$08,$08,$b4,$b8,$08,$bc,$c0,$c0,$c4,$b0,$08,$08,$08,$2c,$08
    .byte $08,$08,$c8,$cc,$b0,$c8,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08
    .byte $08,$d0,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08
    .byte $08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08
    .byte $08,$08,$08,$08,$08,$08

DefaultObjectFireDelay: ;at A40C
    .byte $00,$1f,$01,$00,$00,$00,$00,$00,$00,$0f,$00,$00,$3f,$01,$00,$10
    .byte $bb,$00,$20,$00,$00,$20,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$26,$26,$20,$3f,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$18,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00

EnemyDataPointers: ; Enemy data pointers at A452 in bank 6
	.word Lbl_a468, Lbl_a529, Lbl_a5ce, Lbl_a677
	.word Lbl_a754, Lbl_a841, Lbl_a8c6, Lbl_a91b
	.word Lbl_a9cc, Lbl_aa25, Lbl_aa72

Lbl_a468:
    .byte $01,$10,$4C,$00,$01,$40,$8C,$00,$01,$50,$5C,$00,$02,$38,$4C,$00
    .byte $02,$70,$8C,$00,$02,$80,$5C,$00,$03,$38,$48,$0B,$03,$B8,$78,$0B
    .byte $04,$78,$58,$0B,$04,$98,$38,$22,$04,$D8,$98,$0B,$05,$48,$B8,$17
    .byte $05,$78,$78,$0B,$05,$B8,$38,$22,$06,$78,$D8,$17,$06,$98,$28,$0B
    .byte $06,$B8,$58,$22,$07,$90,$B4,$1C,$08,$38,$B8,$02,$08,$58,$98,$02
    .byte $08,$78,$78,$02,$09,$48,$A8,$04,$09,$68,$68,$04,$09,$B8,$88,$21
    .byte $0A,$48,$48,$21,$0A,$58,$88,$04,$0A,$78,$68,$21,$0A,$D8,$88,$21
    .byte $0B,$78,$A8,$21,$0B,$88,$B8,$04,$0B,$B8,$68,$21,$0B,$D8,$48,$21
    .byte $0C,$28,$48,$21,$0C,$38,$78,$04,$0C,$38,$A8,$21,$0C,$88,$88,$21
    .byte $0C,$C8,$28,$04,$0D,$90,$B4,$1C,$0F,$28,$B8,$40,$0F,$60,$4C,$00
    .byte $0F,$90,$8C,$00,$0F,$A0,$5C,$00,$10,$F8,$80,$0A,$11,$F8,$80,$0A
    .byte $12,$E0,$AC,$09,$14,$D8,$A0,$05,$15,$C8,$60,$16,$16,$50,$A0,$05
    .byte $FF
	
Lbl_a529:
    .byte $01,$00,$B4,$14,$02,$30,$94,$14,$02,$B0,$74,$14,$03,$30,$54,$14
    .byte $04,$80,$02,$FF,$04,$82,$FE,$1F,$04,$98,$BC,$03,$04,$F0,$90,$07
    .byte $05,$98,$BC,$03,$06,$98,$BC,$03,$07,$78,$9C,$03,$08,$00,$00,$FF
    .byte $08,$80,$FE,$07,$09,$08,$C8,$40,$09,$28,$C8,$04,$09,$48,$A8,$04
    .byte $0A,$B8,$DC,$03,$0A,$E0,$80,$10,$0B,$98,$DC,$03,$0B,$E0,$80,$10
    .byte $0D,$4C,$54,$13,$0D,$A4,$9C,$13,$0D,$F4,$9C,$13,$0E,$64,$B4,$13
    .byte $0E,$90,$98,$3F,$0E,$EC,$9C,$13,$0F,$4C,$AC,$13,$0F,$70,$70,$07
    .byte $0F,$A4,$84,$13,$0F,$F4,$9C,$13,$10,$01,$FE,$07,$10,$48,$C8,$41
    .byte $11,$30,$7C,$3D,$11,$48,$7C,$3D,$11,$60,$7C,$3D,$11,$78,$79,$3E
    .byte $11,$90,$79,$3E,$11,$A8,$79,$3E,$12,$E0,$AC,$09,$14,$F0,$A0,$07
    .byte $16,$E0,$FE,$07,$FF

Lbl_a5ce:
    .byte $01,$30,$98,$02,$01,$68,$78,$02,$01,$A0,$58,$02,$01,$D0,$78,$02
    .byte $01,$F0,$98,$02,$03,$50,$E0,$0D,$03,$D0,$E0,$0D,$04,$50,$E0,$0D
    .byte $04,$D0,$E0,$0D,$05,$A0,$C0,$05,$06,$08,$C0,$05,$06,$28,$48,$3E
    .byte $06,$38,$48,$3E,$06,$68,$C0,$05,$06,$C8,$A8,$3F,$07,$68,$58,$17
    .byte $07,$68,$78,$23,$07,$68,$98,$17,$07,$68,$B8,$23,$07,$C8,$B8,$40
    .byte $09,$00,$84,$12,$0A,$80,$A4,$08,$0A,$98,$98,$0B,$0B,$18,$98,$0B
    .byte $0B,$98,$98,$0B,$0C,$C8,$7C,$03,$0D,$28,$4C,$03,$0D,$A8,$BC,$03
    .byte $0E,$F0,$80,$08,$0F,$F0,$80,$0A,$11,$B0,$FE,$0A,$11,$D0,$44,$12
    .byte $11,$F8,$78,$41,$13,$D0,$84,$12,$15,$68,$48,$21,$15,$98,$88,$21
    .byte $15,$68,$B8,$21,$15,$98,$D8,$21,$16,$68,$48,$21,$16,$98,$88,$21
    .byte $16,$68,$B8,$21,$16,$98,$D8,$21,$FF

Lbl_a677:
    .byte $00,$80,$E0,$32,$00,$78,$80,$05,$00,$78,$40,$05,$00,$F0,$40,$05
    .byte $01,$58,$04,$05,$01,$68,$80,$05,$01,$B8,$40,$05,$02,$48,$38,$40
    .byte $02,$50,$E0,$31,$02,$78,$C0,$0F,$03,$28,$79,$3E,$03,$38,$A0,$0F
    .byte $03,$48,$B9,$3E,$03,$58,$E0,$0F,$04,$78,$C0,$0F,$04,$98,$E0,$0F
    .byte $04,$E0,$38,$3F,$04,$F0,$38,$3F,$05,$14,$B6,$18,$05,$24,$86,$18
    .byte $05,$30,$B8,$0C,$05,$40,$88,$0C,$05,$C4,$76,$18,$05,$E0,$78,$0C
    .byte $06,$58,$A0,$0F,$06,$78,$80,$0F,$06,$88,$C9,$3E,$06,$98,$C9,$3E
    .byte $06,$A8,$C9,$3E,$07,$68,$E0,$31,$07,$98,$E0,$0F,$07,$D8,$E0,$0F
    .byte $08,$58,$E0,$0F,$08,$98,$C0,$0F,$08,$B0,$E0,$31,$09,$50,$E0,$31
    .byte $09,$F8,$E0,$0F,$0A,$38,$C0,$0F,$0A,$48,$E0,$31,$0A,$D0,$04,$0E
    .byte $0B,$B0,$FF,$0E,$0C,$E0,$A0,$08,$0D,$18,$7C,$03,$0D,$D8,$3C,$03
    .byte $0D,$F0,$BC,$03,$0E,$28,$38,$3F,$0E,$80,$FE,$08,$0E,$88,$E0,$31
    .byte $0E,$C8,$E0,$31,$0F,$B8,$C0,$0F,$0F,$D8,$C0,$0F,$10,$B0,$60,$16
    .byte $11,$50,$60,$16,$11,$E0,$60,$16,$12,$60,$60,$16,$FF

Lbl_a754:
    .byte $00,$28,$5C,$03,$00,$68,$9C,$03,$00,$D8,$7C,$03,$01,$58,$7C,$03
    .byte $01,$98,$9C,$03,$01,$D8,$58,$40,$02,$90,$88,$0C,$02,$C0,$A8,$24
    .byte $03,$10,$FF,$06,$04,$10,$FF,$06,$05,$10,$80,$10,$06,$10,$80,$10
    .byte $06,$30,$77,$19,$06,$50,$CC,$3D,$06,$60,$CC,$3D,$06,$70,$CC,$3D
    .byte $06,$D0,$37,$25,$07,$70,$B7,$19,$07,$D0,$D7,$25,$07,$F0,$C8,$40
    .byte $09,$10,$FF,$06,$0A,$28,$3C,$03,$0A,$38,$7C,$03,$0A,$68,$5C,$03
    .byte $0A,$90,$97,$25,$0A,$C0,$38,$0C,$0A,$C0,$78,$24,$0B,$10,$FF,$06
    .byte $0B,$90,$37,$19,$0B,$90,$B7,$19,$0C,$D0,$78,$42,$0D,$38,$7C,$03
    .byte $0D,$68,$5C,$03,$0D,$68,$BC,$03,$0D,$90,$37,$19,$0D,$C0,$58,$0C
    .byte $0D,$C0,$98,$24,$0E,$30,$58,$0C,$0E,$30,$98,$24,$0E,$C0,$38,$0C
    .byte $0F,$40,$78,$0C,$0F,$40,$B8,$24,$0F,$98,$78,$02,$0F,$D8,$58,$02
    .byte $10,$70,$D7,$19,$10,$D0,$D7,$25,$12,$60,$88,$0C,$12,$80,$A8,$24
    .byte $12,$D0,$37,$19,$12,$D0,$57,$19,$13,$90,$D7,$19,$13,$B0,$8C,$09
    .byte $13,$D0,$B7,$25,$15,$80,$38,$0C,$15,$80,$78,$24,$15,$80,$B8,$0C
    .byte $16,$80,$38,$0C,$16,$80,$78,$24,$16,$80,$B8,$0C,$FF

Lbl_a841:
    .byte $00,$80,$FF,$2C,$00,$D0,$98,$01,$00,$F8,$80,$30,$01,$30,$78,$01
    .byte $01,$70,$58,$01,$01,$FC,$80,$2F,$04,$58,$80,$2F,$05,$01,$08,$FF
    .byte $05,$40,$0A,$FF,$05,$60,$80,$30,$05,$6C,$4C,$00,$05,$A4,$8C,$00
    .byte $05,$B4,$5C,$00,$06,$10,$04,$FF,$06,$50,$06,$FF,$06,$B0,$78,$40
    .byte $07,$C0,$84,$15,$08,$70,$44,$15,$09,$04,$44,$15,$09,$D0,$44,$15
    .byte $0A,$E0,$FF,$00,$0B,$28,$78,$3E,$0B,$38,$78,$3E,$0B,$70,$98,$3D
    .byte $0B,$B0,$58,$40,$0B,$E0,$FF,$00,$0C,$30,$58,$41,$0C,$A0,$B9,$3E
    .byte $0D,$E0,$AC,$09,$0F,$E0,$98,$01,$10,$88,$98,$01,$11,$28,$98,$01
    .byte $11,$B0,$98,$01,$FF

Lbl_a8c6:
    .byte $19,$08,$AC,$09,$1A,$F8,$AC,$09,$1C,$70,$6C,$09,$1E,$68,$E0,$0F
    .byte $1E,$98,$C0,$0F,$1E,$C8,$A0,$0F,$1F,$90,$38,$40,$20,$70,$A8,$02
    .byte $20,$B0,$A8,$02,$20,$F0,$A8,$02,$21,$08,$38,$40,$21,$38,$38,$02
    .byte $21,$78,$38,$02,$21,$D8,$78,$02,$22,$F0,$80,$08,$23,$E4,$AC,$3B
    .byte $24,$30,$AC,$13,$24,$84,$AC,$13,$24,$B8,$BC,$3D,$24,$C8,$BC,$3D
    .byte $80,$00,$00,$00,$FF

Lbl_a91b:
    .byte $19,$10,$4C,$00,$19,$40,$8C,$00,$19,$50,$5C,$00,$19,$C8,$A8,$3E
    .byte $19,$F0,$4C,$00,$1A,$20,$8C,$00,$1A,$30,$5C,$00,$1A,$F0,$4C,$00
    .byte $1B,$20,$8C,$00,$1B,$30,$5C,$00,$1B,$C8,$BC,$3D,$1B,$D8,$BC,$3D
    .byte $1C,$80,$80,$3B,$1D,$C8,$A8,$3E,$1D,$D8,$B0,$05,$1E,$D8,$B0,$05
    .byte $1F,$C8,$BC,$3D,$1F,$D8,$BC,$3D,$20,$80,$80,$3B,$21,$78,$E0,$0D
    .byte $21,$C8,$AC,$3D,$21,$F8,$E0,$0D,$22,$68,$E0,$0D,$22,$C8,$AC,$3D
    .byte $22,$E8,$E0,$0D,$24,$28,$28,$41,$24,$A8,$58,$04,$24,$C8,$88,$04
    .byte $24,$D0,$B8,$21,$25,$D8,$78,$21,$25,$D8,$88,$21,$25,$D8,$B8,$04
    .byte $26,$88,$B8,$21,$26,$D0,$A8,$21,$26,$D8,$C8,$21,$27,$28,$78,$3F
    .byte $27,$D8,$48,$0B,$27,$D8,$68,$22,$27,$D8,$88,$0B,$27,$D8,$A8,$22
    .byte $28,$48,$88,$17,$28,$88,$38,$21,$28,$D8,$48,$21,$28,$D8,$98,$21
    .byte $FF

Lbl_a9cc:
    .byte $18,$88,$68,$04,$18,$98,$B8,$04,$18,$A8,$C8,$21,$19,$58,$60,$16
    .byte $19,$28,$B8,$21,$19,$28,$D8,$21,$19,$58,$C8,$21,$1A,$A8,$60,$16
    .byte $1A,$A8,$C8,$21,$1A,$D8,$A8,$21,$1A,$D8,$B8,$21,$1B,$38,$C8,$21
    .byte $1B,$28,$D8,$21,$1B,$58,$E8,$21,$1B,$98,$60,$16,$1B,$D8,$50,$16
    .byte $1D,$80,$80,$2F,$20,$01,$80,$07,$24,$E0,$FE,$07,$25,$E0,$0C,$FF
    .byte $28,$08,$80,$08,$2E,$80,$80,$30,$FF

Lbl_aa25:
    .byte $14,$10,$FF,$06,$15,$10,$FF,$06,$16,$10,$FF,$2C,$16,$90,$58,$3F
    .byte $17,$78,$70,$16,$17,$F8,$70,$16,$18,$78,$50,$16,$18,$B8,$50,$16
    .byte $19,$78,$70,$16,$19,$78,$D0,$05,$19,$F8,$70,$16,$1C,$68,$98,$41
    .byte $1C,$B8,$98,$45,$1C,$D8,$30,$43,$1D,$80,$80,$3B,$1E,$80,$80,$3B
    .byte $1F,$80,$80,$3B,$20,$80,$80,$3B,$22,$C8,$B8,$3F,$FF

Lbl_aa72:
	.res $8e, $ff

RunEndGameScene: ; at ab00
    lda #$f8
    sta ObjectPosY+0
    lda #$00
    sta ObjectLifeCycleCounter
    sta $bd
    sta $be
    sta $8b
    lda #$14
    sta CurrentBeginScreen
    lda #$1E
    sta CurrentEndScreen
    lda #$30
    sta $0c
    ldx #$04

Lbl_ab1e:
    jsr F1C436
    lda #$8b
    sta ObjectSpriteNum,x
    lda #$00
    sta ObjectFlags,x
    lda #$77
    sta ObjectPosY,x
    lda $0c
    sta ObjectPosX,x
    adc #$40
    sta $0c
    dex
    bne Lbl_ab1e

Lbl_ab3c:
    jsr F1C442
    lda $be
    cmp #$01
    bne Lbl_ab3c
    lda #$03
    sta $3c
    lda #$00
    sta $be
    sta $59
    lda #<CreditsText
    sta $5a
    lda #>CreditsText
    sta $5b

Lbl_ab57:
    ldy $59
    lda ($5a),y
    sta $5c
    iny
    bne Lbl_ab62
    inc $5b

Lbl_ab62:
    jsr F1AE2B
    jsr F1C442
    ldy $59
    dec $5c
    bne Lbl_ab62
    lda #$02
    sta $5c

Lbl_ab72:
    lda $23
    and #$07
    bne Lbl_ab87
    ldx $5c
    lda Lbl_ab8d,x
    sta $03d1
    jsr PaletteSetupForBG
    dec $5c
    bmi Lbl_ab91

Lbl_ab87:
    jsr F1C442
    jmp Lbl_ab72

Lbl_ab8d:
    .byte $21,$11,$01,$0f

Lbl_ab91:
    jsr F1C442
    lda $be
    cmp #$08
    bne Lbl_ab91
    lda #$00
    sta $be
    sta $bd
    lda #$00
    sta $5c

Lbl_aba4:
    lda $23
    and #$07
    bne Lbl_abbb
    inc $5c
    ldx $5c
    lda Lbl_ab8d,x
    sta $03d1
    jsr PaletteSetupForBG
    cpx #$03
    beq Lbl_abc1

Lbl_abbb:
    jsr F1C442
    jmp Lbl_aba4

Lbl_abc1:
    lda #$05
    sta $5c
    lda #$22
    sta RawPPUtransferAddress+0
    lda #$42
    sta RawPPUtransferAddress+1

Lbl_abcf:
    ldx #$1c
    stx $5e
    lda #$04

Lbl_abd5:
    sta RawPPUtransferAddress+1,x
    dex
    bne Lbl_abd5
    jsr F1C442
    clc
    lda RawPPUtransferAddress+1
    adc #$40
    sta RawPPUtransferAddress+1
    lda RawPPUtransferAddress+0
    adc #$00
    sta RawPPUtransferAddress+0
    dec $5c
    bne Lbl_abcf
    dec $3c
    beq Lbl_abfa
    jmp Lbl_ab57

Lbl_abfa:
    lda $59
    pha
    lda #$04
    sta ObjectPosY+0
    jsr TeleportToStage_Bank06callback
    pla
    sta $59
    inc $bf

Lbl_ac0a:
    jsr F1C442
    lda ObjectUnknown440+0
    bne Lbl_ac0a
    sta ObjectFlags+0
    lda #$06
    sta ObjectSpriteNum+0
    lda #$21
    sta $03d1
    jsr PaletteSetupForBG
    lda #$fe
    sta $3c

Lbl_ac26:
    ldx #$04
: ; -
    ; Move objects 1,2,3,4 to the right at speed $00.B0 (0.6875)
    lda ObjectPosXfraction,x
    adc #$b0
    sta ObjectPosXfraction,x
    lda ObjectPosX,x
    adc #$00
    sta ObjectPosX,x
    dex
    bne :-
    lda ObjectPosY+5
    cmp #$f8
    beq Lbl_ac45
    jmp Lbl_add3

Lbl_ac45:
    ldx $8b
    lda EndingData,x
    pha
    and #$0f
    ora #$10
    sta $05
    pla
    and #$f0
    sta $04
    sec
    lda $1a
    sbc $04
    sta $0c
    lda $1b
    sbc $05
    bcc Lbl_ac69
    bne Lbl_ac97
    lda $0c
    bne Lbl_ac97

Lbl_ac69:
    inx
    lda EndingData,x
    cmp #$fc
    bcc Lbl_ac74
    jmp Lbl_ad63

Lbl_ac74:
    sta $0c
    inx
    ldy EndingData,x
    inx

Lbl_ac7b:
    lda EndingData,x
    sta BGPalettes,y
    inx
    iny
    dec $0c
    bne Lbl_ac7b
    lda EndingData,x
    sta SpritePalettes
    inx
    stx $8b
    jsr PaletteSetupForBG
    lda #$20
    sta WritePaletteParam

Lbl_ac97:
    lda $c0
    beq :+
    lda $93
    sta $01
    jsr SpawnObject_TypeIsFF
    lda AutoSpawnObjectFFcounter
    bne :+
    inc $93
    inc $93
    dec $c0
: ; +
    lda #$00
    sta $2f
    sta ObjectXSpeed+0
    lda #$b0
    sta ObjectXSpeedFraction+0
    jsr F1C439
    lda $1a
    beq Lbl_acc9
    cmp $3c
    bne Lbl_acc6
    jsr F1AE0F

Lbl_acc6:
    jmp Lbl_ac26

Lbl_acc9:
    lda $1b
    cmp #$14
    beq Lbl_acd2
    jmp Lbl_ac26

Lbl_acd2:
    lda #$91
    sta ObjectSpriteNum+0
    lda #$04
    sta ObjectYSpeed+0
    lda #$df
    sta ObjectYSpeedFraction+0
    lda #$00
    sta ObjectUnknown440+0

Lbl_ace6:
    sec
    lda ObjectPosYfraction+0
    sbc ObjectYSpeedFraction+0
    sta ObjectPosYfraction+0
    lda ObjectPosY+0
    sbc ObjectYSpeed+0
    sta ObjectPosY+0
    sec
    lda ObjectYSpeedFraction+0
    sbc #$40
    sta ObjectYSpeedFraction+0
    lda ObjectYSpeed+0
    sbc #$00
    sta ObjectYSpeed+0
    bmi Lbl_ad12
    jsr F1C442
    jmp Lbl_ace6

Lbl_ad12:
    inc $06b1
    lda #$02
    sta $59
    ldx #$00

Lbl_ad1b:
    lda PresentedText,x
    sta $5b
    inx
    lda PresentedText,x
    sta RawPPUtransferAddress+0
    inx
    lda PresentedText,x
    sta RawPPUtransferAddress+1
    inx

Lbl_ad2f:
    lda PresentedText,x
    sta RawPPUtransferBuf
    inc $5e
    inx
    stx $5a

Lbl_ad3a:
    jsr F1C442
    lda $23
    and #$07
    bne Lbl_ad3a
    inc RawPPUtransferAddress+1
    ldx $5a
    dec $5b
    bne Lbl_ad2f
    dec $59
    bne Lbl_ad1b

Lbl_ad50:
    jsr F1C442
    lda $14
    and #$08
    beq Lbl_ad50
    ldx #$00
    stx $bf
    inx
    stx $bb
    jmp Lbl_c45e

Lbl_ad63:
    inx
    stx $8b
    cmp #$fe
    beq Lbl_ada9
    cmp #$fd
    beq Lbl_adaf
    bcs Lbl_ad7b
    lda #$0e
    sta $93
    lda #$03
    sta $c0
    jmp Lbl_ac97

Lbl_ad7b:
    lda #$10
    sta $0c
    ldy #$8c
    ldx #$05

Lbl_ad83:
    tya
    sta ObjectSpriteNum,x
    lda #$80
    sta ObjectPosX,x
    lda $0c
    sta ObjectPosY,x
    clc
    adc #$10
    sta $0c
    lda #$00
    sta ObjectFlags,x
    sta ObjectPosYfraction,x
    sta ObjectPosXfraction,x
    iny
    inx
    cpx #$08
    bne Lbl_ad83
    beq Lbl_add0

Lbl_ada9:
    ldx #$10
    lda #$8f
    bne Lbl_adb3

Lbl_adaf:
    ldx #$11
    lda #$90

Lbl_adb3:
    sta ObjectSpriteNum,x
    lda $1a
    sta ObjectPosX,x
    lda #$64
    sta ObjectPosY,x
    lda #$40
    sta ObjectFlags,x
    lda #$00
    sta ObjectUnknown440,x
    sta ObjectFireDelay,x
    sta ObjectLifeCycleCounter,x

Lbl_add0:
    jmp Lbl_ac97

Lbl_add3:
    ldx #$05

Lbl_add5:
    lda ObjectPosY,x
    cmp #$f8
    beq Lbl_ae07
    ; Move an object to the left at speed $00.B0
    sec
    lda ObjectPosXfraction,x
    sbc #$b0
    sta ObjectPosXfraction,x
    lda ObjectPosX,x
    sbc #$00
    sta ObjectPosX,x
    ; Move an object to the down at speed $00.0E
    clc
    lda ObjectPosYfraction,x
    adc #$0e
    sta ObjectPosYfraction,x
    lda ObjectPosY,x
    adc #$00
    sta ObjectPosY,x
    cmp #$58
    bcc Lbl_ae07
    lda #$f8
    sta ObjectPosY,x

Lbl_ae07:
    inx
    cpx #$08
    bne Lbl_add5
    jmp Lbl_ac45

F1AE0F:
    dec $3c
    dec $3c
    cmp #$fc
    bne Lbl_ae1b
    lda #$fe
    sta $3c

Lbl_ae1b:
    ldy $59
    lda ($5a),y
    cmp #$ff
    bne F1AE2B
    iny
    bne Lbl_ae28
    inc $5b

Lbl_ae28:
    sty $59
    rts

F1AE2B:
    lda ($5a),y
    sta $5e
    clc
    adc #$02
    sta $0c
    ldx #$ff

Lbl_ae36:
    iny
    bne Lbl_ae3b
    inc $5b

Lbl_ae3b:
    inx
    cpx $0c
    beq Lbl_ae48
    lda ($5a),y
    sta RawPPUtransferAddress+0,x
    jmp Lbl_ae36

Lbl_ae48:
    sty $59
    rts

EndingData:
    .byte $8b,$0b,$05,$28,$18,$26,$0f,$18,$0f,$bc,$0f,$26,$16,$06,$15,$0b
    .byte $0a,$06,$17,$25,$0f,$17,$0f,$3c,$0f,$25,$15,$05,$15,$fa,$ff,$ea
    .byte $06,$11,$0f,$0f,$0f,$0f,$0f,$0f,$15,$da,$fc,$99,$06,$11,$38,$26
    .byte $0f,$0f,$30,$38,$15,$89,$01,$0b,$37,$05,$88,$01,$00,$06,$06,$87
    .byte $07,$05,$27,$07,$0f,$0f,$06,$0f,$37,$06,$47,$07,$05,$16,$06,$0f
    .byte $0f,$07,$0f,$27,$06,$07,$0b,$05,$10,$0f,$0f,$0f,$00,$0f,$02,$0f
    .byte $01,$00,$0f,$0b,$45,$03,$0d,$11,$0f,$0f,$0b,$44,$fe,$24,$fd,$00
	
.charmap $20, $04 ; Space
.charmap $52, $7b ; R -> 'r.'
.charmap $2e, $7c ; .
.charmap $2c, $7d ; , -> ', '
.charmap $27, $7e ; '
.charmap $21, $7f ; !

.ifdef J_VERSION
.define MEGAMAN_NAME "rockman"
.else
.define MEGAMAN_NAME "megaman"
.endif

.macro cr_fline num_lines, ppu_addr, text
	.byte num_lines, .strlen(text), >ppu_addr, <ppu_addr, text
.endmacro

.macro cr_line ppu_addr, text
	.byte .strlen(text), >ppu_addr, <ppu_addr, text
.endmacro

CreditsText: ;at aebb U
	cr_fline $05, $2248, .concat(MEGAMAN_NAME, " has ended")
		cr_line $2287, "the evil domination"
		cr_line $22cc, "of dRwily"
		cr_line $230a, "and restored"
		cr_line $2348, "the world to peace"
	cr_fline $04, $2244, "however,the never ending"
		cr_line $2288, "battle continues"
		cr_line $22c2, "until all destructive forces"
		cr_line $230a, "are defeated."
	cr_fline $02, $2289, .concat("fight,", MEGAMAN_NAME, "!")
		cr_line $22c5, "for everlasting peace!"
	cr_fline $ff, $268e, "staff"
		cr_line $228d, "planner"
		cr_line $22ef, "a.k"
		cr_line $2687, "character designer"
		cr_line $26ec, "yasukichi"
		cr_line $2287, "character designer"
		cr_line $22ed, "tom pon"
	cr_fline $ff, $26ec, "inafking "
	cr_fline $ff, $22ed, "  a.k  "
		cr_line $2687, "    programmer    "
		cr_line $26ec, " h.m.d.  "
		cr_line $2287, " sound programmer "
		cr_line $22e7, "chanchacorin manami"
		cr_line $2688, "sound programmer"
		cr_line $26e8, "yuukichan's papa"
		cr_line $2288, "                "
		cr_line $22e7, "                   "

PresentedText:
		cr_line $228a, "presented by"
.ifdef J_VERSION
		cr_line $22ed, "capcom"
.else
		cr_line $22ea, "capcom u.s.a.,"
.endif

WeaponSelectDialog: ;at B0CB
    clc
    lda ScrollPosX
    adc #$80
    sta $57
    lda ScrollPosScreen
    adc #$00
    sta $58
    sta $05
    lda $57
    and #$e0
    ora #$04
    sta $57
    sta $04
    lda FrameCounter
    pha
    lda GutsmanStompCounter
    pha
    lda UseTempScrollX
    pha
    lda MegamanWalkTimer
    pha
    lda MegamanStallTimer
    pha
    lda $b1
    pha
    ldy #$04
    ldx #$3f
    jsr HideSprites
    lda #$f8
    sta ObjectPosY+5
    lda #$00
    jsr ChangeMegamanPalette
    lda #$22
    jsr IssueSound  ;play pause sound
    jsr NextFrame
    lda #$0e
    sta RawPPUtransferAddress+0
    lda #$00
    sta RawPPUtransferAddress+1
    sta GutsmanStompCounter
    sta $5b
    sta MiscCounter1
    sta UseTempScrollX
    sta MegamanWalkTimer
    sta MegamanStallTimer
    sta $b1
    lda WeaponsOwned
    sta $5a
.scope
DrawWeaponSelectMenuLineLoop:
    ldx #$00
DrawWeaponSelectMenuColumnLoop:
    stx $59
    lda #$00
    sta $0d
    jsr CalculateNametableAddress
    ldx #$12
    jsr Adjust32x32BlockAddress
    ldy CurrentStage
    lda Lbl_b4e4,y
    sta TSAPPUtransfer0NTdata-2,x
    lda #$01
    sta $1c
    jsr F1B3D1
    jsr NextFrame
    lda RawPPUtransferAddress+1
    clc
    adc #$10
    sta RawPPUtransferAddress+1
    ldx $59
    inx
    cpx #$03
    beq :+
    clc
    lda $04
    adc #$20
    sta $04
    lda $05
    adc #$00
    sta $05
    jmp DrawWeaponSelectMenuColumnLoop
: ; +
    inc MiscCounter1
    lda MiscCounter1
    cmp #$05
    beq Lbl_b183
    asl a
    asl a
    clc
    adc $57
    sta $04
    lda $58
    sta $05
    jmp DrawWeaponSelectMenuLineLoop
.endscope

Lbl_b183:
    lda #$20
    sta $0f
    ldx #$01
    stx $0c

Lbl_b18b:
    lda ScrollPosX
    and #$1f
    sta $0d
    lda #$9b
    sec
    sbc $0d
    sta $0d
    ldy WeaponSelectTab2,x
    lda WeaponsOwned
    and WeaponsOwnedMask,x
    beq Lbl_b1f4
    txa
    asl a
    tax
    lda Lbl_b4ef,x
    sta $0e
    lda Meters,y
    pha
     lsr a
     lsr a
     sta $10
    pla
    and #$03
    sta $11
    sec
    lda #$ec
    sbc $11
    sta $11
    ldx #$00

Lbl_b1c0:
    cpx $10
    bcs Lbl_b1c8
    lda #$e8
    bne Lbl_b1d0

Lbl_b1c8:
    bne Lbl_b1ce
    lda $11
    bne Lbl_b1d0

Lbl_b1ce:
    lda #$ec

Lbl_b1d0:
    ldy $0F
    sta CurrentSpriteData+1,y
    lda $0E
    sta CurrentSpriteData+0,y
    lda #1
    sta CurrentSpriteData+2,y
    lda $0D
    sta CurrentSpriteData+3,y
    iny
    iny
    iny
    iny
    sty $0f
    clc
    adc #$08
    sta $0d
    inx
    cpx #$07
    bne Lbl_b1c0

Lbl_b1f4:
    inc $0c
    ldx $0c
    cpx #$08
    bne Lbl_b18b
    lda ScrollPosX
    and #$1f
    sta $0c
    sec
    lda #$a8
    sbc $0c
    sta $0c
    ldx #$17

Lbl_b20b:
    lda Lbl_b52f,x
    sta $02e8,x
    txa
    and #$03
    cmp #$03
    bne Lbl_b221
    clc
    lda $0c
    adc $02e8,x
    sta $02e8,x

Lbl_b221:
    dex
    bpl Lbl_b20b
    ldx $a6
    ldy #$00
    lda #$0a
    jsr EnemyCalculateJumpCurveToHitMegaman
    lda $05
    pha
     ora #$f0
     sta $02f9
    pla
    asl a
    sta $05
    asl a
    asl a
    adc $05
    sta $05
    sec
    lda $a6
    sbc $05
    ora #$f0
    sta $02fd
    ldy WeaponSelect
    lda WeaponSelectTab,y
    sta WeaponSelect

Lbl_b250:
    ldx #$1c
    lda #$00

Lbl_b254:
    sta CurrentSpriteData+2,x
    dex
    dex
    dex
    dex
    bpl Lbl_b254
    lda WeaponSelect
    sec
    sbc #$01
    and #$07
    asl a
    asl a
    tax
    lda #$01
    sta CurrentSpriteData+2,x

Lbl_b26c:
    lda FrameCounter
    and #$07
    bne Lbl_b279
    lda FrameCounter
    and #$08
    jsr F1B3A7

Lbl_b279:
    jsr NextFrame
    lda JoyD0
    and #$38
    beq Lbl_b26c
    pha
     lda #$01
     jsr F1B3A7
    pla
    and #$30
    beq Lbl_b2bf
    and #$10
    beq Lbl_b2a5

Lbl_b291:
    dec WeaponSelect
    lda WeaponSelect
    and #$07
    sta WeaponSelect
    beq Lbl_b2b7
    tay
    lda WeaponsOwned
    and WeaponsOwnedMask,y
    beq Lbl_b291
    bne Lbl_b2b7

Lbl_b2a5:
    inc WeaponSelect
    lda WeaponSelect
    and #$07
    sta WeaponSelect
    beq Lbl_b2b7
    tay
    lda WeaponsOwned
    and WeaponsOwnedMask,y
    beq Lbl_b2a5

Lbl_b2b7:
    lda #$1F
    jsr IssueSound  ;play menu move sound
    jmp Lbl_b250

Lbl_b2bf:
    ldx WeaponSelect
    lda WeaponSelectTab2,x
    sta WeaponSelect
    ldy #$04
    ldx #$3f
    jsr HideSprites
    lda #$00
    sta $2f
    ldx WeaponSelect
    cpx #$06
    bne Lbl_b2f7
    ldx CurrentStage
    lda Lbl_b2e1,x
    ldy Lbl_b2ec,x
    bne Lbl_b2fc

Lbl_b2e1: ;indexed by current stage
    .byte $96,$96,$96,$96,$b6,$be,$9e,$96,$96,$9e,$96

Lbl_b2ec: ;indexed by current stage
    .byte $01,$01,$01,$01,$01,$01,$03,$01,$01,$03,$01

Lbl_b2f7:
    ldy #$02
    lda Lbl_b39f,x

Lbl_b2fc:
    sta $5a
    sty $5b
    lda $57
    sta $04
    lda $58
    sta $05
    lda #$0e
    sta RawPPUtransferAddress+0
    lda #$00
    sta RawPPUtransferAddress+1
    sta MiscCounter1

Lbl_b314:
    ldx #$00

Lbl_b316:
    stx $59
    lda #$00
    sta $0d
    sta $1c
    jsr DrawBlockFromActiveLevelMap_Bank06callback
    lda $5a
    sta $07
    lda #$00
    sta $06
    lda $5b
    ldy RawPPUtransferAddress+1
    jsr F1C400
    lda #$10
    sta $5e
    jsr NextFrame
    lda RawPPUtransferAddress+1
    clc
    adc #$10
    sta RawPPUtransferAddress+1
    clc
    lda $04
    adc #$20
    sta $04
    lda $05
    adc #$00
    sta $05
    ldx $59
    inx
    cpx #$03
    bne Lbl_b316
    inc MiscCounter1
    lda MiscCounter1
    asl a
    asl a
    clc
    adc $57
    sta $04
    lda $58
    sta $05
    lda MiscCounter1
    cmp #$05
    bne Lbl_b314
    lda $5a
    sta $07
    lda #$00
    sta $06
    lda $5b
    ldy RawPPUtransferAddress+1
    jsr F1C400
    lda #$10
    sta $5e
    lda WeaponSelect
    jsr ChangeMegamanPalette
    jsr NextFrame
    lda #$ff
    sta GutsmanWeaponTargetActive
    pla
    sta $b1
    pla
    sta MegamanStallTimer
    pla
    sta MegamanWalkTimer
    pla
    sta UseTempScrollX
    pla
    sta GutsmanStompCounter
    pla
    sta FrameCounter
    jmp Lbl_c3e3

Lbl_b39f:
    .byte $91,$91,$91,$92,$90,$94,$95,$91

F1B3A7:
    php
    lda WeaponSelect
    tax
    sec
    sbc #$01
    and #$07
    asl a
    asl a
    tay
    lda #$f8
    plp
    beq Lbl_b3bb
    lda Lbl_b527,x

Lbl_b3bb:
    sta CurrentSpriteData+0,y
    rts

ChangeMegamanPalette: ;At B3BF
    asl a
    tax
    lda MegamanPalette+0,x
    sta $03E1
    lda MegamanPalette+1,x
    sta $03E2
    jsr PaletteSetupForSprites
    rts

F1B3D1:
    ldx #$00

Lbl_b3d3:
    ldy $5b
    lda Lbl_b4c6,y
    asl a
    asl a
    asl a
    tay

Lbl_b3dc:
    lda Lbl_b44e,y
    sta TSAPPUtransfer0NTdata,x
    iny
    inx
    txa
    and #$07
    bne Lbl_b3dc
    inc $5b
    lda $5b
    and #$01
    bne Lbl_b3d3
    lda #$00
    sta $06
    lda #$ac
    sta $07
    lda #$02
    ldx #$00
    ldy RawPPUtransferAddress+1
    jsr F1C400
    lda #$10
    sta $5e
    lda #$01
    sta $0c
    lda MiscCounter1
    asl a
    sta $0d

Lbl_b410:
    lda $0d
    tax
    lda WeaponsOwned
    ora #$01
    and WeaponsOwnedMask,x
    beq Lbl_b445
    lda $0d
    asl a
    tay
    lda Lbl_b503,x
    tax
    lda Lbl_b4ef,y
    sta CurrentSpriteData+0,x
    lda Lbl_b4ef+1,y
    sta CurrentSpriteData+1,x
    lda #$20
    sta CurrentSpriteData+2,x
    lda ScrollPosX
    and #$1f
    sta CurrentSpriteData+3,x
    lda #$8e
    sec
    sbc CurrentSpriteData+3,x
    sta CurrentSpriteData+3,x

Lbl_b445:
    inc $0d
    dec $0c
    bpl Lbl_b410
    ldx #$12
    rts

Lbl_b44e:
    .byte $60,$60,$60,$60,$60,$61,$61,$61,$60,$61,$62,$63,$60,$61,$64,$65
    .byte $60,$61,$61,$61,$60,$61,$61,$61,$60,$61,$61,$61,$60,$60,$60,$60
    .byte $60,$60,$60,$60,$61,$61,$61,$61,$61,$61,$61,$61,$61,$61,$61,$61
    .byte $61,$61,$61,$61,$60,$60,$60,$60,$60,$60,$60,$60,$61,$61,$61,$60
    .byte $62,$63,$61,$60,$64,$65,$61,$60,$61,$61,$61,$60,$61,$61,$61,$60
    .byte $61,$61,$61,$60,$61,$6e,$61,$60,$61,$61,$61,$60,$60,$60,$60,$60
    .byte $62,$63,$61,$61,$64,$65,$61,$61,$61,$61,$62,$63,$61,$61,$64,$65
    .byte $62,$63,$62,$63,$64,$65,$64,$65

Lbl_b4c6:
    .byte $00,$02,$02,$02,$02,$03,$04,$05,$05,$05,$05,$06,$04,$05,$05,$05
    .byte $05,$06,$04,$05,$05,$05,$05,$06,$07,$09,$09,$0a,$09,$0b

Lbl_b4e4: ;indexed by "current stage"
    .byte $aa,$ff,$aa,$00,$00,$00,$00,$aa,$00,$55,$55

Lbl_b4ef:
	.byte $F8,$E0, $33,$E0, $43,$E1, $53,$E2
	.byte $63,$E3, $73,$E4, $83,$E5, $93,$E6
	.byte $A3,$E7, $A3,$E7

Lbl_b503:
    .byte $00,$00,$04,$08,$0c,$10,$14,$18,$1c,$1c

WeaponSelectTab: ;at B50D
    .byte 0,5,4,1,6,2,3,7 ;See also: WeaponsOwnedTab

WeaponsOwnedMask: ; at B515
    .byte $ff,$02,$04,$08,$10,$20,$40,$80,$ff,$ff ;

WeaponSelectTab2: ;at B51F
    .byte $00,$03,$05,$06,$02,$01,$04,$07

Lbl_b527:
    .byte $a3,$33,$43,$53,$63,$73,$83,$93

Lbl_b52f:
    .byte $a0,$ed,$00,$00,$a0,$ed,$40,$08,$a8,$ee,$01,$00,$a8,$ee,$41,$08
    .byte $a8,$f0,$01,$1c,$a8,$f0,$01,$24

StageSelectionScreenMain: ;at $B547 (bank 6)
    lda #$02
    sta $0c
    lda #$20  ;addr $2000
    sta $2006
    lda #$00
    sta $2006
Lbl_B555: ; --- ;   at $B555
    lda #$02
    sta $0d
    lda #$20
    ldy #$0f
Lbl_B55D: ; --  ;   at $B55D
    ldx #$40
Lbl_B55F: ; -   ;   at $B55F
    sta $2007
    dex
    bne Lbl_B55F ; -
    dey
    bne Lbl_B55D ; --
    lda #$00
    ldy #$01
    dec $0d
    bne Lbl_B55D ; --
    dec $0c
    bne Lbl_B555 ; ---
    lda #$27
    sta $2006 ;addr $27D0
    lda #$d0
    sta $2006
    lda #$50
    ldx #$08
Lbl_B582: ; -   ;   at $B582
    sta $2007
    dex
    bne Lbl_B582 ; -
    lda #$55
    ldx #$08
Lbl_B58C: ; -   ;   at $B58C
    sta $2007
    dex
    bne Lbl_B58C ; -
    lda #$05
    ldx #$08
Lbl_B596: ; -   ;   at $B596
    sta $2007
    dex
    bne Lbl_B596 ; -
    lda #$00
    sta $0c
    sta $0d

Lbl_b5a2:
    asl a
    tax
    lda Lbl_be87,x
    sta $04
    lda Lbl_be88,x
    sta $05
    ldx $0c
    cpx #$06
    bne Lbl_b5b8
    lda #$00
    beq Lbl_b5bd

Lbl_b5b8:
    lda WeaponsOwned
    and WeaponsOwnedBits,x

Lbl_b5bd:
    sta $0e
    ldy #$00

Lbl_b5c1:
    lda $05
    sta $2006
    lda $04
    sta $2006
    ldx #$06

Lbl_b5cd:
    lda $0c
    cmp #$05
    bcs Lbl_b5d8
    lda Lbl_be1b,y
    bne Lbl_b5e2

Lbl_b5d8:
    bne Lbl_b5df
    lda Lbl_be3f,y
    bne Lbl_b5e2

Lbl_b5df:
    lda Lbl_be63,y

Lbl_b5e2:
    cmp #$21
    bne Lbl_b5f0
    lda $0e
    beq Lbl_b5ee
    lda #$00
    beq Lbl_b5f0

Lbl_b5ee:
    lda #$21

Lbl_b5f0:
    sta $2007
    iny
    dex
    bne Lbl_b5cd
    clc
    lda $04
    adc #$20
    sta $04
    lda $05
    adc #$00
    sta $05
    cpy #$24
    bne Lbl_b5c1
    sta $2006
    lda $04
    sta $2006
    ldx $0d
    ldy #$07

Lbl_b614:
    lda Lbl_bf6f,x
    sta $2007
    inx
    dey
    bne Lbl_b614
    stx $0d
    ldx $0c
    lda Lbl_be95,x
    clc
    adc #$08
    ldx #$23
    stx $2006
    sta $2006
    lda #$88
    sta $2007
    lda #$22
    sta $2007
    inc $0c
    lda WeaponsOwned
    and #$7e
    cmp #$7e
    bne Lbl_b64c
    lda $0c
    cmp #$07
    beq Lbl_b655
    bne Lbl_b652

Lbl_b64c:
    lda $0c
    cmp #$06
    beq Lbl_b655

Lbl_b652:
    jmp Lbl_b5a2

Lbl_b655:
    stx $2006
    lda #$cc
    sta $2006
    sta $2007
    lda #$33
    sta $2007
    lda WeaponsOwned
    and #$7e
    cmp #$7e
    beq Lbl_b68e
    ldy #$04
    ldx #$00

Lbl_b671:
    lda Lbl_bfac,x
    sta $2006
    inx
    lda Lbl_bfac,x
    sta $2006
    inx

Lbl_b67f:
    lda Lbl_bfac,x
    sta $2007
    inx
    txa
    and #$07
    bne Lbl_b67f
    dey
    bne Lbl_b671

Lbl_b68e:
    lda #<Lbl_bdab
    sta $35
    lda #>Lbl_bdab
    sta $36
    lda #$20
    jsr WritePalette
    lda WeaponsOwned
    and #$08
    beq Lbl_b6ae
    lda #<Lbl_bddb
    sta $35
    lda #>Lbl_bddb
    sta $36
    lda #$10
    jsr WritePalette

Lbl_b6ae:
    ldy #$00
    ldx #$40
    jsr HideSprites
    ldx #$04
    lda #$00
    sta CurrentStage

Lbl_b6bb:
    asl a
    asl a
    tay
    stx $0c
    ldx #$00
Lbl_b6c2: ; - ;at b6c2
    lda Lbl_beca+0,y
    sta $0d,x
    inx
    iny
    cpx #$04
    bne Lbl_b6c2 ; -
    ldx $0c
    jsr F1BC5A
    inc CurrentStage
    lda CurrentStage
    cmp #$06
    bne Lbl_b6bb
    lda #$01
    jsr IssueSound  ;play stage select music
    lda #$00
    sta ScrollPosScreen
    sta ScrollPosX
    lda $ff
    ora #$80
    sta $ff
    sta $2000
    lda #$00
    sta CurrentStage
    sta FrameCounter
    tay

Lbl_b6f5:
    tya
    asl a
    asl a
    tax
    lda #$23
    sta TSAPPUtransfer0AttrAddress+0,x
    lda Lbl_be9c,y
    sta TSAPPUtransfer0AttrData,x
    iny
    cpy #$0a
    bne Lbl_b6f5
    ldx #$00
    ldy #$00

Lbl_b70d:
    lda Lbl_bea6,y
    sta $032b,x
    inx
    inx
    inx
    inx
    iny
    cpy #$04
    bne Lbl_b70d
    lda #$55
    jsr F1BB02

Lbl_b721:
    jsr NextFrame
    lda JoyD0
    and #$c8
    beq Lbl_b75e
    and #$c0
    beq Lbl_b775
    ldx CurrentStage
    pha
    lda #$00
    jsr F1BB02
    lda CurrentStage
    asl a
    tay
    pla

Lbl_b73b:
    asl a
    bcs Lbl_b741
    iny
    bne Lbl_b73b

Lbl_b741:
    lda WeaponsOwned
    and #$7e
    cmp #$7e
    bne Lbl_b74e
    clc
    tya
    adc #$0c
    tay

Lbl_b74e:
    lda Lbl_beb0,y
    sta CurrentStage
    lda #$ff
    sta FrameCounter
    lda #$1f
    jsr IssueSound  ; play menu move sound
    bne Lbl_b721

Lbl_b75e:
    ldy #$00
    lda FrameCounter
    and #$0f
    bne Lbl_b772
    lda FrameCounter
    and #$10
    bne Lbl_b76e
    ldy #$55

Lbl_b76e:
    tya
    jsr F1BB02

Lbl_b772:
    jmp Lbl_b721

Lbl_b775: ;boss selected!
    lda #$1c
    jsr IssueSound  ; play select boss effect
    lda #$02
    jsr IssueSound  ; play boss selected music
    inc ScrollPosScreen
    ldy #$00    ; delay = 0.
    sty MiscCounter1
    lda #$48
    sta $37
    lda #<Lbl_bdcb
    sta $38
    lda #>Lbl_bdcb
    sta $39
    lda #<Lbl_be0b
    sta $3a
    lda #>Lbl_be0b
    sta $3b
    lda #$10
    sta WritePaletteParam
    lda CurrentStage
    asl a
    asl a
    tax
    lda Lbl_bee6+0,x
    sta ObjectYSpeed+0
    lda Lbl_bee6+1,x
    sta ObjectYSpeedFraction+0
    lda Lbl_bee6+2,x
    sta ObjectPosX+1
    lda Lbl_bee6+3,x
    sta ObjectPosXfraction+1
    lda Lbl_beca+1,x
    sta ObjectPosY+0
    sty ObjectPosYfraction+0
    lda Lbl_beca+2,x
    sta ObjectPosX+0
    sty ObjectPosXfraction+0
    lda Lbl_beca+3,x
    sta ObjectFlags+0
    ldx CurrentStage
    lda Lbl_bf02,x
    sta ObjectSpriteNum+0

Lbl_b7da:
    lda CurrentStage
    cmp #$05
    bne Lbl_b7f3    ;Gutsman?
    lda MiscCounter1        ;counter for various purposes
    cmp #$08
    bcs Lbl_b7ee
    ldy #$00
    jsr F1BC39
    jmp Lbl_b7f6

Lbl_b7ee:
    lda #$53
    sta ObjectSpriteNum+0

Lbl_b7f3:
    jsr F1BC00    ;Iteration: moves the boss to its proper location

Lbl_b7f6:
    jsr NextFrame
    inc MiscCounter1
    lda MiscCounter1
    cmp #$40
    bne Lbl_b7da
    ; Boss has now jumped into its location.
    lda #$03
    sta MiscCounter1
    ldx CurrentStage
    lda Lbl_bf09,x
    sta ObjectSpriteNum+0
    lda #$40
    sta ObjectFlags+0

Lbl_b812:
    ldy #$00
    jsr F1BC39
    jsr NextFrame
    dec MiscCounter1
    bne Lbl_b812 ;This loop only uses 3 frames... whatever it does.
    lda #$07
    sta RawPPUtransferAddress+0
    lda #$b9
    sta $5b
    lda #$f0
    sta RawPPUtransferAddress+1
    sta $5a
    lda #$00
    sta $59
    sta ObjectUnknown440
    sta ObjectFireDelay
    lda #$78  ;This is how much time the boss has to pose itself.
    sta MiscCounter1

Lbl_b83c: ;Do boss pose
    lda CurrentStage
    asl a
    tax
    lda BossPoseTable,x
    sta $04
    lda BossPoseTable+1,x
    sta $05
    ; Load instruction of current pose (or something)
    lda ObjectFireDelay
    asl a
    tay
    lda ($04),y
    cmp #$ff
    bne Lbl_b85c
    ldy #$00
    sty ObjectFireDelay
    lda ($04),y

Lbl_b85c:
    sta ObjectSpriteNum+0
    inc ObjectUnknown440
    lda ObjectUnknown440
    iny
    cmp ($04),y
    bne Lbl_b872
    lda #$00
    sta ObjectUnknown440
    inc ObjectFireDelay

Lbl_b872:
    ldy #$00
    jsr F1BC39
    lda CurrentStage
    cmp #$02
    bne Lbl_b883
    jsr F1BBAD ;Bombman has also a bomb!
    jmp Lbl_b88a

Lbl_b883:
    cmp #$06
    bne Lbl_b88a
    jsr F1BCF1 ;This is for Wily...

Lbl_b88a:
    jsr NextFrame
    dec MiscCounter1         ;Done with boss pose? Do more frames if not
    bne Lbl_b83c
    ; The boss has now posed.
    lda #$25
    sta TSAPPUtransfer0AttrAddress+0
    lda #$90
    sta TSAPPUtransfer0AttrAddress+1
    lda #$00
    sta TSAPPUtransfer0AttrData
    lda CurrentStage
    ldx #$07    ; Text length
    jsr F1BB4A    ; Print text
    lda #$d0
    sta TSAPPUtransfer0AttrAddress+1
    lda #$09
    ldx #$0c
    jsr F1BB4A
    ldx #$44
    ldy #$03
    lda #$b0
    sta ObjectPosX+0
    lda #$80
    sta ObjectPosY+0
    lda #$f0
    sta $0d
    sta $0e
    sta $0f
    jsr F1BB88
    lda FrameCounter
    and #$0f
    adc #$28
    sta MiscCounter1
    lda #$ff
    sta ObjectUnknown440

Lbl_b8d9:
    inc ObjectUnknown440
    lda ObjectUnknown440
    cmp #$05
    bne Lbl_b8e7
    lda #$fb
    bne Lbl_b8f0

Lbl_b8e7:
    cmp #$06
    bne Lbl_b8f0
    lda #$00
    sta ObjectUnknown440

Lbl_b8f0:
    clc
    adc #$f5
    sta $0d
    lda #$f0
    ldy #$02
    jsr F1BB81
    lda #$f8
    sta CurrentSpriteData+0,x
    lda FrameCounter
    and #$03
    bne Lbl_b90c
    lda #$21
    jsr IssueSound  ;play Points SFX (shuffle score)

Lbl_b90c:
    jsr NextFrame
    dec MiscCounter1
    bne Lbl_b8d9
    lda CurrentStage
    cmp #$06
    bne Lbl_b926
    lda #$0f
    sta ObjectUnknown440
    lda #$f0
    sta $0e
    lda #$f2
    bne Lbl_b92f

Lbl_b926:
    lda ObjectUnknown440
    cmp #$05
    bne Lbl_b936
    lda #$f1

Lbl_b92f:
    sta $0d
    ldy #$03
    jsr F1BB81

Lbl_b936:
    clc
    lda ObjectUnknown440
    adc #$05
    asl a
    sta $3d
    asl a
    asl a
    adc $3d
    sta $3d
    ldx #$48

Lbl_b947:
    jsr NextFrame
    dex
    bne Lbl_b947
    lda CurrentStage
    cmp #$06
    beq Lbl_b978 ;Is it Wily?

Lbl_b953:
    lda #$fe
    jsr IssueSound  ; stop sounds
    lda #$ff
    jsr IssueSound  ; stop sounds
    jsr NextFrame
    jsr NextFrame
    dec ScrollPosScreen
    lda $ff
    and #$7f
    sta $ff
    sta $2000
    lda $fe
    and #$06
    sta $fe
    sta $2001
    rts

Lbl_b978:   ; Wily and saucer show
    lda #$f8
    ldx #$1f

Lbl_b97c:
    sta ObjectPosY,x
    dex
    bpl Lbl_b97c
    lda #<Lbl_bdeb
    sta $38
    sta $3a
    lda #>Lbl_bdeb
    sta $39
    sta $3b
    lda #$20
    sta WritePaletteParam
    lda #$01
    sta $37
    lda #$73
    sta ObjectSpriteNum+2
    lda #$60
    sta ObjectPosX+2
    lda #$74
    sta ObjectPosY+2
    lda #$00
    sta ObjectFlags+2
    sta ObjectUnknown440+2
    sta ObjectFireDelay+2
    sta ObjectLifeCycleCounter+2
    lda #$60
    sta MiscCounter1

Lbl_b9b7:
    jsr F1BD27
    jsr NextFrame
    dec MiscCounter1
    bne Lbl_b9b7
    lda #$00
    sta ObjectUnknown440+2
    sta ObjectUnknown440+0
    sta ObjectUnknown440+1
    sta ObjectFireDelay+0
    sta ObjectFireDelay+1
    sta ObjectLifeCycleCounter+0
    sta ObjectLifeCycleCounter+1
    sta ObjectLifeCycleCounter+2
    sta ObjectFlags+0
    sta ObjectFlags+0
    sta ObjectPosXfraction+0
    sta ObjectPosYfraction+0
    lda #$71
    sta ObjectSpriteNum+2
    lda #$e4
    sta ObjectPosX+0
    lda #$48
    sta ObjectPosY+0
    lda #$74
    sta ObjectSpriteNum+0
    lda #$75
    sta ObjectSpriteNum+1
    lda #$11
    jsr IssueSound  ; play wily saucer music

Lbl_ba05:
    lda #$60 ;COULD BE ROCKMAN WALKING SPEED
    sta $04
    lda #$01
    sta $05
    lda #$00
    sta $06
    sta $07
    jsr F1BD40
    lda ObjectPosX+0
    cmp #$5f
    bcc Lbl_ba05
    cmp #$61
    bcs Lbl_ba05

Lbl_ba21:
    lda #$00
    sta $04
    sta $05
    sta $07
    lda #$80
    sta $06
    jsr F1BD40
    lda ObjectPosY+0
    cmp #$6c
    bcc Lbl_ba21
    lda #$00
    sta ObjectUnknown440+1
    lda #$76
    sta ObjectSpriteNum+1
    lda #$40
    sta ObjectFlags+2

Lbl_ba46:
    sec
    lda ObjectPosYfraction+0
    sbc #$80
    sta ObjectPosYfraction+0
    lda ObjectPosY+0
    sbc #$00
    sta ObjectPosY+0
    jsr F1BD27
    jsr NextFrame
    lda ObjectPosY+0
    cmp #$50
    bcs Lbl_ba46
    lda #$40
    sta MiscCounter1

Lbl_ba68:
    lda MiscCounter1
    and #$07
    bne Lbl_ba81
    lda MiscCounter1
    and #$08
    bne Lbl_ba78
    lda #$fc
    bne Lbl_ba7a

Lbl_ba78:
    lda #$02

Lbl_ba7a:
    clc
    adc ObjectPosY+2
    sta ObjectPosY+2

Lbl_ba81:
    jsr F1BD27
    jsr NextFrame
    dec MiscCounter1
    bne Lbl_ba68
    lda #$70
    sta ObjectPosY+2
    lda #$72
    sta ObjectSpriteNum+2
    lda #$20
    sta MiscCounter1

Lbl_ba99:
    jsr F1BD27
    jsr NextFrame
    dec MiscCounter1
    bne Lbl_ba99

Lbl_baa3:
    clc
    lda ObjectPosYfraction+0
    adc #$80
    sta ObjectPosYfraction+0
    lda ObjectPosY+0
    adc #$00
    sta ObjectPosY+0
    jsr F1BD27
    jsr NextFrame
    lda ObjectPosY+0
    cmp #$6c
    bcc Lbl_baa3
    lda #$f8
    sta ObjectPosY+2
    lda #$75
    sta ObjectSpriteNum+1

Lbl_bacb:
    lda #$00
    sta $04
    sta $05
    lda #$ff
    sta $07
    lda #$80
    sta $06
    jsr F1BD40
    lda ObjectPosY+0
    cmp #$48
    bcs Lbl_bacb

Lbl_bae3:
    lda #$60
    sta $04
    lda #$01
    sta $05
    lda #$00
    sta $06
    sta $07
    jsr F1BD5B
    lda ObjectPosX+0
    cmp #$1b
    bcc Lbl_bae3
    cmp #$1d
    bcs Lbl_bae3
    jmp Lbl_b953

F1BB02:
    sta $0c
    ldx CurrentStage
    lda Lbl_be95,x
    sta $0d
    ldy #$00

Lbl_bb0d:
    tya
    asl a
    asl a
    tax
    clc
    lda $0d
    adc Lbl_beaa,y
    sta TSAPPUtransfer0AttrAddress+1,x
    lda TSAPPUtransfer0AttrAndMask,x
    eor #$ff
    and $0c
    sta TSAPPUtransfer0AttrOrMask,x
    iny
    cpy #$06
    bne Lbl_bb0d
    lda $0c
    sta $032d
    sta $0331
    and #$0f
    sta $0335
    sta $0339
    lda WeaponsOwned
    and #$7e
    cmp #$7e
    bne Lbl_bb45
    lda #$86
    bne Lbl_bb47

Lbl_bb45:
    lda #$8a

Lbl_bb47:
    sta $1c
    rts

F1BB4A:
    sta ObjectFlags+0
    stx ObjectUnknown440
    ldy #$00

Lbl_bb52:
    dec ObjectUnknown440
    bmi Lbl_bb80 ; End of text?
    lda #$05
    sta MiscCounter1      ; Frames per letter
    lda ObjectFlags+0
    asl a
    tax
    lda TextTable,x
    sta $04
    lda TextTable+1,x
    sta $05
    lda ($04),y
    sta TSAPPUtransfer0AttrOrMask
    lda #$81
    sta $1c
    inc TSAPPUtransfer0AttrAddress+1
    iny

Lbl_bb77:
    jsr NextFrame
    dec MiscCounter1
    beq Lbl_bb52
    bne Lbl_bb77

Lbl_bb80:
    rts

F1BB81: ;display promised score.. maybe text, too
    ldx #$50
    lda #$98
    sta ObjectPosX+0

F1BB88:
    lda ObjectPosY+0
    sta CurrentSpriteData+0,x
    lda ObjectPosX+0
    sta CurrentSpriteData+3,x
    sec
    sbc #$08
    sta ObjectPosX+0
    lda $000c,y
    sta CurrentSpriteData+1,x
    lda #$01
    sta CurrentSpriteData+2,x
    inx
    inx
    inx
    inx
    dey
    bne F1BB88
    rts

F1BBAD:
    dec CurrentStage
    dec CurrentStage
    lda MiscCounter1
    cmp #$48
    beq Lbl_bbd8
    bcs Lbl_bbbf
    cmp #$28
    bcc Lbl_bbbf
    bcs Lbl_bbf6

Lbl_bbbf:
    lda #$56
    sta $0d
    lda #$67
    sta $0e
    lda #$68
    sta $0f
    sta ObjectPosX+2
    lda #$40
    sta $10
    jsr F1BC5A
    jmp Lbl_bbfb

Lbl_bbd8:
    lda #$56
    sta ObjectSpriteNum+2
    lda #$00
    sta ObjectPosYfraction+2
    lda #$64
    sta ObjectPosY+2
    lda #$02
    sta ObjectYSpeed+2
    lda #$00
    sta ObjectYSpeedFraction+2
    lda #$40
    sta ObjectFlags+2

Lbl_bbf6:
    ldy #$02
    jsr F1BC15

Lbl_bbfb:
    inc CurrentStage
    inc CurrentStage
    rts

F1BC00:
    ldy #$00
    clc
    lda ObjectPosXfraction+0
    adc ObjectPosXfraction+1
    sta ObjectPosXfraction+0
    lda ObjectPosX+0
    adc ObjectPosX+1
    sta ObjectPosX+0

F1BC15:
    sec
    lda ObjectPosYfraction,y
    sbc ObjectYSpeedFraction,y
    sta ObjectPosYfraction,y
    lda ObjectPosY,y
    sbc ObjectYSpeed,y
    sta ObjectPosY,y
    sec
    lda ObjectYSpeedFraction,y
    sbc #$20
    sta ObjectYSpeedFraction,y
    lda ObjectYSpeed,y
    sbc #$00
    sta ObjectYSpeed,y

F1BC39:
    lda ObjectSpriteNum,y
    sta $0d
    lda ObjectPosY,y
    sta $0e
    lda ObjectPosX,y
    sta $0f
    lda ObjectFlags,y
    sta $10
    cpy #$00
    bne F1BC5A
    ldy #$00
    ldx #$40
    jsr HideSprites
    ldx #$00

F1BC5A:
    lda $0d
    asl a
    rol $12
    tay
    lda #<MajObjFrameAddrs
    sta $11
    lda $12
    and #$01
	; To support MajObjFrameAddrs & $100, change this to adc? But what will carry be??
    ora #>MajObjFrameAddrs
    sta $12
    lda ($11),y
    sta $04
    iny
    lda ($11),y
    sta $05
    ldy #$00
    sty $12
    lda ($04),y
    sta $11
    iny
    lda ($04),y
    asl a
    tay
    lda TileOffsTableAddrs,y
    sta $06
    lda TileOffsTableAddrs+1,y
    sta $07
    ldy #$02
    sty $0c

Lbl_bc90:
    ldy $0c
    lda ($04),y
    clc
    ldy CurrentStage
    adc Lbl_bf10,y
    sta CurrentSpriteData+1,x
    ldy $0c
    iny
    lda $10
    and #$40
    eor ($04),y
    iny
    sty $0c
    sta CurrentSpriteData+2,x
    and #$03
    cmp #$02
    bne Lbl_bcbf
    lda CurrentSpriteData+2,x
    and #$fc
    ldy CurrentStage
    ora Lbl_bf17,y
    sta CurrentSpriteData+2,x

Lbl_bcbf:
    ldy $12
    lda ($06),y
    tay
    clc
    lda $0e
    adc SpriteTileYOffs,y
    sta CurrentSpriteData+0,x
    clc
    lda $10
    bne Lbl_bcd7
    lda SpriteTileLeftXOffs,y
    bcc Lbl_bcda

Lbl_bcd7:
    lda SpriteTileRightXOffs,y

Lbl_bcda:
    adc $0f
    sta CurrentSpriteData+3,x
    inx
    inx
    inx
    inx
    cpx #$fc
    beq Lbl_bcf0
    dec $11
    beq Lbl_bcf0
    inc $12
    jmp Lbl_bc90

Lbl_bcf0:
    rts

F1BCF1: ;something specific to Wily
    lda $59
    cmp #$30
    beq Lbl_bd26
    clc
    lda $5a
    adc #$10
    sta $5a
    sta $06
    lda $5b
    adc #$00
    sta $5b
    sta $07
    clc
    lda RawPPUtransferAddress+1
    adc #$10
    sta RawPPUtransferAddress+1
    lda RawPPUtransferAddress+0
    adc #$00
    sta RawPPUtransferAddress+0
    ldy #$00
    lda #$02
    jsr F1C400
    lda #$10
    sta $5e
    inc $59

Lbl_bd26:
    rts

F1BD27:
    ldy #$00
    sty $0d
    ldx #$40
    jsr HideSprites
    lda #$00
    sta $0c

Lbl_bd34:
    jsr DrawObject
    inc $0c
    lda #$03
    cmp $0c
    bne Lbl_bd34
    rts

F1BD40:
    jsr F1BD76
    ldx #$6c

Lbl_bd45:
    lda CurrentSpriteData+3,x
    cmp #$c8
    bcc Lbl_bd51
    lda #$f8
    sta CurrentSpriteData+0,x

Lbl_bd51:
    dex
    dex
    dex
    dex
    bpl Lbl_bd45
    jsr NextFrame
    rts

F1BD5B:
    jsr F1BD76
    ldx #$6c

Lbl_bd60:
    lda CurrentSpriteData+3,x
    cmp #$38
    bcs Lbl_bd6c
    lda #$f8
    sta CurrentSpriteData+0,x

Lbl_bd6c:
    dex
    dex
    dex
    dex
    bpl Lbl_bd60
    jsr NextFrame
    rts

F1BD76:
    clc
    lda ObjectPosXfraction+0
    adc $04
    sta ObjectPosXfraction+0
    sta ObjectPosXfraction+1
    lda ObjectPosX+0
    adc $05
    sta ObjectPosX+0
    sta ObjectPosX+1
    clc
    lda ObjectPosYfraction+0
    adc $06
    sta ObjectPosYfraction+0
    sta ObjectPosYfraction+1
    lda ObjectPosY+0
    adc $07
    sta ObjectPosY+0
    clc
    adc #$10
    sta ObjectPosY+1
    jsr F1BD27
    rts

    ; Palettes?
Lbl_bdab:
    .byte $0f,$3c,$21,$11,$0f,$30,$35,$11,$0f,$28,$0f,$0f,$0f,$28,$15,$27
    .byte $0f,$20,$21,$0f,$0f,$20,$38,$0f,$0f,$20,$15,$0f,$0f,$27,$15,$0f

Lbl_bdcb:
    .byte $0f,$30,$30,$30,$0f,$20,$20,$20,$0f,$18,$0f,$0f,$0f,$18,$15,$27

Lbl_bddb:
    .byte $0f,$3c,$21,$11,$0f,$30,$35,$11,$0f,$28,$30,$37,$0f,$0f,$15,$27
	
Lbl_bdeb:
    .byte $0f,$11,$11,$11,$0f,$0c,$0c,$0c,$0f,$11,$11,$11,$0f,$11,$11,$11
    .byte $0f,$20,$21,$0f,$0f,$20,$38,$0f,$0f,$30,$15,$0f,$0f,$20,$38,$0f

Lbl_be0b:
    .byte $0f,$3c,$21,$11,$0f,$30,$30,$0c,$0f,$28,$0f,$0f,$0f,$28,$15,$27

Lbl_be1b:
    .byte $22,$24,$24,$24,$24,$26,$23,$21,$21,$21,$21,$27,$23,$21,$21,$21
    .byte $21,$27,$23,$21,$21,$21,$21,$27,$23,$21,$21,$21,$21,$27,$28,$25
    .byte $25,$25,$25,$29

Lbl_be3f:
    .byte $22,$24,$24,$24,$24,$26,$23,$30,$32,$34,$36,$27,$23,$31,$33,$35
    .byte $37,$27,$23,$38,$3a,$3c,$3e,$27,$23,$39,$3b,$3d,$3f,$27,$28,$25
    .byte $25,$25,$25,$29

Lbl_be63:
    .byte $22,$24,$24,$24,$24,$26,$23,$21,$21,$21,$21,$27,$23,$41,$43,$45
    .byte $47,$27,$23,$21,$4a,$4c,$4e,$27,$23,$49,$4b,$4d,$4f,$27,$28,$25
    .byte $25,$25,$25,$29

Lbl_be87:
    .byte $69

Lbl_be88:
    .byte $20,$75,$21,$71,$22,$69,$22,$65,$21,$71,$20,$6d,$21

Lbl_be95:
    .byte $c2,$d5,$e4,$e2,$d1,$c4,$d3

Lbl_be9c:
    .byte $0f,$0f,$cc,$33,$f0,$f0,$00,$00,$f0,$f0

Lbl_bea6:
    .byte $db,$dc,$e3,$e4

Lbl_beaa:
    .byte $00,$01,$08,$09,$10,$11

Lbl_beb0:
    .byte $05,$04,$02,$05,$03,$01,$04,$02,$00,$03,$01,$00,$05,$06,$02,$05
    .byte $03,$01,$04,$02,$06,$03,$01,$00,$00,$04

Lbl_beca: ;indexed by current stage*4
    .byte $22,$34,$60,$40
    .byte $37,$74,$c0,$00
    .byte $30,$b4,$a0,$00
    .byte $3e,$b4,$60,$40
    .byte $47,$74,$40,$40
    .byte $4f,$2f,$a0,$00
    .byte $9a,$74,$80,$00
    ; $400, $600,$480,$420.

    ; $400 = megaman state
    ; $600 = megaman Y pos
    ; $480 = megaman X pos
    ; $420 = megaman flags

    ; These four bytes are also consumed at $0D..$10 by Lbl_b5ca

    ; Probably the frames and locations of bosses.

Lbl_bee6: ; indexed by current stage*4
    .byte $02,$f0,$00,$00
    .byte $03,$f0,$fe,$80
    .byte $04,$f0,$ff,$00
    .byte $04,$f0,$00,$00
    .byte $03,$f0,$00,$80
    .byte $02,$44,$fe,$dc
    .byte $03,$f0,$ff,$80
    ; $680,$660,$481,$4A1.

Lbl_bf02: ;indexed by current stage
    .byte $28,$5c,$35,$5d,$4c,$55,$9a
    ; goes to $400
    ; ObjectSpriteNum+0. Possibly his different shots?? (Unverified)

Lbl_bf09: ;indexed by current stage
    .byte $24,$38,$31,$40,$49,$55,$9a

Lbl_bf10:
    .byte $80,$a0,$c0,$e0,$00,$30,$60

Lbl_bf17:
    .byte $02,$00,$03,$02,$03,$03,$02

BossPoseTable: ;indexed by current stage*2 -- at BF1E
    .word Lbl_bf2c
    .word Lbl_bf31
    .word Lbl_bf37
    .word Lbl_bf3c
    .word Lbl_bf4e
    .word Lbl_bf54
    .word Lbl_bf59
Lbl_bf2c: ;C
    .byte $22,$08,$24,$08,$ff
Lbl_bf31: ;I
    .byte $37,$10,$38,$08,$39,$ff
Lbl_bf37: ;B
    .byte $31,$30,$30,$20,$ff
Lbl_bf3c: ;F
    .byte $3e,$10,$40,$08,$78,$10,$41,$08
    .byte $78,$10,$41,$08,$78,$10,$41,$08,$78,$ff
Lbl_bf4e: ;E
    .byte $47,$10,$48,$08,$49,$ff
Lbl_bf54: ;G
    .byte $4f,$0e,$50,$0e,$ff
Lbl_bf59: ;wily
    .byte $9a,$ff

TextTable:
    .word Lbl_bf6f, Lbl_bf76, Lbl_bf7d, Lbl_bf84, Lbl_bf8b, Lbl_bf92, Lbl_bf99
	.word Lbl_bfbc, Lbl_bfc4, Lbl_bfa0

Lbl_bf6f:   .byte $03,$15,$14,$0d,$01,$0e,$20 ; CUTMAN_
Lbl_bf76:   .byte $09,$03,$05,$0d,$01,$0e,$20 ; ICEMAN_
Lbl_bf7d:   .byte $02,$0f,$0d,$02,$0d,$01,$0e ; BOMBMAN
Lbl_bf84:   .byte $06,$09,$12,$05,$0d,$01,$0e ; FIREMAN
Lbl_bf8b:   .byte $05,$0c,$05,$03,$0d,$01,$0e ; ELECMAN
Lbl_bf92:   .byte $07,$15,$14,$13,$0d,$01,$0e ; GUTSMAN
Lbl_bf99:   .byte $04,$1b,$17,$09,$0c,$19,$20 ; D?WILY_
Lbl_bfa0:   .byte $03,$0c,$05,$01,$12,$20,$10 ; CLEAR_P
            .byte $0f,$09,$0e,$14,$13         ; OINTS
			
Lbl_bfac:   .byte $21,$8d, $13,$05,$0c,$05,$03,$14 ; Addr SELECT
Lbl_bfb4:   .byte $21,$ad, $13,$14,$01,$07,$05,$20 ; Addr STAGE
Lbl_bfbc:   .byte $21,$ed, $10,$12,$05,$13,$13,$20 ; Addr PRESS
Lbl_bfc4:   .byte $22,$0d, $13,$14,$01,$12,$14,$20 ; Addr START

WeaponsOwnedBits:
    .byte $20,$10,$02,$40,$04,$08
	
Lbl_bfd2:
	.byte $77,$e6,$02,$74
	.byte $77,$e7,$02,$7c
	.byte $77,$e8,$02,$84
.ifdef J_VERSION
	.byte $77,$E7,$02,$7C
	.byte $77,$E8,$02,$84
.endif

	.res $12, 0

Lbl_bff0:
    jmp RunEndGameScene

DoWeaponSelectDialog: ;Lbl_bff3
    jmp WeaponSelectDialog

RunStageSelectionScreen:
    jmp StageSelectionScreenMain

Lbl_bff9:
    .res 7, 0
