all: stage1.bin

.SUFFIXES: .asm .bin

.asm.bin:
	as31 -l $<
	hex2bin $(shell echo $< | sed -e 's@\.asm@\.hex@')
