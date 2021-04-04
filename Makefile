all: check

%.p %.lst: %.asm
	asl -cpu 6502 -L $<

%.bin: %.p
	p2bin -r '$$c000-$$dfff' $<

check: ramfactor14.bin
	echo '3369e5e9838db52df7924e79e65451267953172390d30308a28c688a886989ce ramfactor14.bin' | sha256sum -c
