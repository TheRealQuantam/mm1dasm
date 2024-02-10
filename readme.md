# Mega Man 1 Disassembly

Credits:
- Original documentation: [MEGAMAN 1 NES ROM TECH DOC](http://worldofwarcraftbeta.0catch.com/nes/megm1rom.txt) ([alternate link](https://www.nesdev.org/megm1rom.txt))
- Additional changes: [Joel Yliluoma](http://bisqwit.iki.fi/jutut/megamansource/)
- Conversion to compilable assembly: Justin Olbrantz (Quantam)
- Further updates: All contributors

# Building
This disassembly is for the [cc65 toolchain](https://cc65.github.io/), and requires make (e.g. for Windows: [MinGW](https://sourceforge.net/projects/mingw/)). Banks 0-3, as well as the level data portion of banks 4/5, are taken from a copy of the original game ROM. From the command line:
- US: `make BASE_ROM=path_to_original_mm_rom us`
- Japan: `make BASE_ROM=path_to_original_rm_rom japan`

Rest of readme to be written
