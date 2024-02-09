.PHONY: clean dir us japan

SRCDIR := $(CURDIR)
CFG_FILE := mm1dasm.cfg
ASM_FILES := $(wildcard $(SRCDIR)/*.s)

USBUILDDIR := build/us
US_O_FILES := $(patsubst $(SRCDIR)/%.s,$(USBUILDDIR)/%.o,$(ASM_FILES))
US_STEM := us

JBUILDDIR := build/japan
J_O_FILES := $(patsubst $(SRCDIR)/%.s,$(JBUILDDIR)/%.o,$(ASM_FILES))
J_STEM := japan

#all: us japan

dir:
	-@mkdir -p build
	-@mkdir -p "$(USBUILDDIR)"
	-@mkdir -p "$(JBUILDDIR)"

clean:
	-@rm -rf build
	-@rm -f $(US_STEM).nes
	-@rm -f $(US_STEM).dbg
	-@rm -f $(J_STEM).nes
	-@rm -f $(J_STEM).dbg

us: dir $(US_STEM).nes

japan: dir $(J_STEM).nes

$(US_STEM).nes: $(CFG_FILE) $(US_O_FILES)
	ld65 -vm -m $(USBUILDDIR)/map.txt -Ln $(USBUILDDIR)/labels.txt --dbgfile $(US_STEM).dbg -o $@ -C $^

$(USBUILDDIR)/%.o: $(SRCDIR)/%.s globals.inc
	$(file > $(USBUILDDIR)/build.inc,.define SRC_ROM "$(BASE_ROM)")
	ca65 -g -I $(USBUILDDIR) -o $@ $<

$(J_STEM).nes: $(CFG_FILE) $(J_O_FILES)
	ld65 -vm -m $(JBUILDDIR)/map.txt -Ln $(JBUILDDIR)/labels.txt --dbgfile $(J_STEM).dbg -o $@ -C $^

$(JBUILDDIR)/%.o: $(SRCDIR)/%.s globals.inc
	$(file > $(JBUILDDIR)/build.inc,.define SRC_ROM "$(BASE_ROM)")
	ca65 -g -DJ_VERSION -I $(JBUILDDIR) -o $@ $<
