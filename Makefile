all: stage1.bin example/payload.bin

.SUFFIXES: .asm .bin

stage1.bin: stage1.asm
	as31 -Fbin -l $<
	mv $@ $@.tmp
	dd if=$@.tmp of=$@ count=1 skip=15
	rm $@.tmp

.asm.bin:
	as31 -Fbin -l $<
