.include "globals.inc"

; ROM data banks
inc_banks 0, 5

.segment "HDR"
.byte "NES", $1a ; Signature
.byte $20000 / $4000 ; Num 16-KB PRG-ROM banks
.byte 0 / $2000 ; Num 8-KB CHR-ROM banks
.byte (2 << 4) | 1 ; MMC1, vertical mirroring
.byte 0
.byte 0 / $2000 ; Num 8-KB PRG-RAM banks
.byte 0 ; NTSC
.res 6, 0