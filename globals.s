.include "globals.inc"

; ROM data banks
inc_banks 0, 4

.segment "HDR"
.byte "NES", $1a ; Signature
.byte $20000 / $4000 ; Num 16-KB PRG-ROM banks
.byte 0 / $2000 ; Num 8-KB CHR-ROM banks
.byte (2 << 4) | 1 ; MMC1, vertical mirroring
.byte 0
.byte 0 / $2000 ; Num 8-KB PRG-RAM banks
.byte 0 ; NTSC
.res 6, 0

.assert MajObjFrameAddrs & $100 = 0, lderror, "MajObjFrameAddrs & $100 must be 0"

.assert <LxB000 = 0, lderror, "LxB000 must be page-aligned"
.assert <LxB200 = 0, lderror, "LxB200 must be page-aligned"
.assert <LxB700 = 0, lderror, "LxB700 must be page-aligned"
.assert <LxB800 = 0, lderror, "LxB800 must be page-aligned"
.assert <LxB900 = 0, lderror, "LxB900 must be page-aligned"
.assert <LxBB00 = 0, lderror, "LxBB00 must be page-aligned"
