; Applied Engineering RamFactor firmware version 1.4

; Partially reverse-engineered by Eric Smith <spacewar@gmail.com>

; Cross-assemble with Macro Assembler AS:
;   http://john.ccac.rwth-aachen.de:8000/as/
; Depends on AS "section" pseudo-op to provide separate namespaces
; for each slot ROM, and for diagnostics and partition manager


fillto	macro	endaddr,value,{noexpand}
	ifnb	value
v	set	value
	else
v	set	$00
	endif
	while	*<endaddr
	if	(endaddr-*)>1024
	fcb	[1024] v
	else
	fcb	[endaddr-*] v
	endif
	endm
	endm


L00	equ	$00
Z39	equ	$39
Z3e	equ	$3e
Z3f	equ	$3f
Z41	equ	$41
Z42	equ	$42	; ProDOS interface command
Z43	equ	$43	; ProDOS interface unit number
Z44	equ	$44	; ProDOS interface buffer pointer (two bytes)
Z45	equ	$45
Z46	equ	$46	; ProDOS interface block number (two bytes)
Z47	equ	$47
Z48	equ	$48
Z49	equ	$49
Z4a	equ	$4a
Z4b	equ	$4b

D010b	equ	$010b
D010c	equ	$010c
D01a9	equ	$01a9
D0200	equ	$0200

; screen holes, global
mslot	equ	$07f8

; screen holes, slot-indexed
D0478	equ	$0478	; # blocks of whole card, divided by 256
D04f8	equ	$04f8	; index to partition data
D0578	equ	$0578	; partition base address, high bbyte
D05f8	equ	$05f8	; partition base address, mid byte
D0678	equ	$0678	; current partition size, pages, high byte
D06f8	equ	$06f8	; current partition size, pages, low byte
D0778	equ	$0778	; operating system code
D07f8	equ	$07f8	; operating system check code

D0800	equ	$0800
L0801	equ	$0801
D0900	equ	$0900

L0a00	equ	$0a00

Dbd12	equ	$bd12	; RWTS patch loc, three bytes

proflag	equ	$bf00	; used to detect OS
			; $00 - Pascal
			; $4C - ProDOS
			; $33 - DOS 3.3
Dbff8	equ	$bff8
Dbff9	equ	$bff9
Dbffa	equ	$bffa
Dbffb	equ	$bffb

kbd		equ	$c000	; read keyboard
rdmainram	equ	$c002	; IIe read main RAM
rdcardram	equ	$c003	; IIe read auxilliary RAM
wrmainram	equ	$c004	; IIe write main RAM
wrcardram	equ	$c005	; IIe write auxilliary RAM
clr80vid	equ	$c00c	; IIe disable 80col
clraltchar	equ	$c00e	; IIe disable altchar
kbdstrb		equ	$c010	; clea rkeyboard strobe
rdramrd		equ	$c013	; IIe MSB reads 1 if main RAM read enabled
rdramwrt	equ	$c014	; IIe MSB reads 1 if main RAM write enabled

Dc0nf	equ	$c08f	; bank select, add slot*$10
	
Dcfff	equ	$cfff	; turn off C800 shared ROM space

sloop	equ	$faba
init	equ	$fb2f
bell12	equ	$fbe2
clreop	equ	$fc42
home	equ	$fc58
rdkey	equ	$fd0c
crout	equ	$fd8e
prbyte	equ	$fdda
cout	equ	$fded
setkbd	equ	$fe89
setvid	equ	$fe93
resetvec	equ	$fffc


pc_err_bad_cmd		equ	$01
pc_err_bad_pcnt		equ	$04
pc_err_bus_err		equ	$06
pc_err_bad_unit_num	equ	$11
pc_err_bad_ctl		equ	$21
pc_err_bad_ctl_param	equ	$22
pc_err_io_error		equ	$27
pc_err_no_drive		equ	$28
pc_err_no_write		equ	$2b	; write-protected
pc_err_bad_block_num	equ	$2d
pc_err_offline		equ	$2f
; Protocol Converter error codes $30..$3f are device-specific errors
; Protocol Converter error codes $50..$7f are device-specific non-fatal errors


	irp	banknum,0,1

	org	$c000+(banknum*$1000)
	fcb	"COPYRIGHT (C) 1986-89 APPLIED ENGINEERING",$00
	fcb	"BOB SANDER-CEDERLOF",$00
	fcb	"MICHAEL WILKS",$00
	fcb	"STEVEN MALECHEK",$00
	fillto	$c100+(banknum*$1000),$ae

	irp	slotnum,1,2,3,4,5,6,7

; per-slot ROM code
	section	bank_banknum_slot_slotnum
	org	$c000+(banknum*$1000)+(slotnum*$0100)
	phase	$c000+(slotnum*$0100)

; Bytes at Cn0{1,3,5} = $20, $00, $03 identify card as a block device
; Byte at Cn07 = $3C for Disk II,
;                $00 for SmartPort interface supported
; Byte at CnFF = $00 for 16-sector Disk II, $ff for 13-sector Disk II,
;   other value $xx = ProDOS block device, entry point = Cnxx, in this case
;   $Cn2A
; For ProDOS block device:
;   word at CnFC = device size in blocks,
;   byte at CnFE = STATUS byte

slot0:	cmp	#$20	; $Cn01 = $20
	cmp	#$00	; $Cn03 = $00
	cmp	#$03	; $Cn05 = $03
	cmp	#$00	; $cn07 = $00
	bcs	boot4	; always taken

; cn0a - RamFactor diagnostic (same entry point as Slinky memory test)
	jsr	Scn16
	jmp	Lcn9b

; cn10 - RamFactor Partion Manager
	jsr	Scn16
	jmp	Lcn9f

Scn16:	sta	clr80vid
	sta	kbd
	sta	clraltchar
	jsr	init
	jsr	setvid
	jsr	setkbd

romsel:	ldy	#$c0+slotnum
	ldx	Dcfff	; turn off all shared (c800) ROMs, except our
	asl	Dc0nf+(slotnum*$10)	; set bank 0
	ldx	#$88+(slotnum*$10)
	sty	mslot
	stx	D0778
	jmp	Lc9df

boot4:	lda	#$c0+slotnum
	cmp	Z39
	php
	jsr	Scn16
	plp
	bne	Lcn60
	jsr	Scdec
	beq	Lcn60

	lda	#$20	; Patch RWTS with jsr $eaea ???
	sta	Dbd12
	lda	#$ea
	sta	Dbd12+1
	sty	Dbd12+2

	ldx	#$00
	lda	#$8d
	sta	D0200
	rts

Lcn60:	lda	L00
	bne	Lcn6e
	cpy	L00+1
	bne	Lcn6e
	jsr	Scn7b
	jmp	sloop

Lcn6e:	lda	D04f8-$c0,Y
	cmp	#$01
	bne	Lcn9f
	jsr	Scn7b
	clc
	bcc	Lcn9f

Scn7b:	jsr	Sca9a
	jsr	Scaeb
	ldy	#$c0+slotnum	; high byte of slot ROM base c100
	lda	L0801
	beq	Lcn9a
	lda	D07f8-$c0,Y
	eor	#$5a
	cmp	D0778-$c0,Y
	bne	Lcn9a
	sta	proflag
	ldx	#(slotnum*$10)
	jmp	L0801

Lcn9a:	rts

; diagnostic
Lcn9b:	ldy	#$03
	bne	Lcna1

; partition managere
Lcn9f:	ldy	#$05

; copy four bytes from table to Z42..Z45
; gets download routine start address in Z42, end+1 in Z44
Lcna1:	ldx	#$03
Lcna3:	lda	Dcnd5,Y
	sta	Z42,X
	dey
	dex
	bpl	Lcna3

	php
	sei
	stx	Dc0nf+(slotnum*$10)	; set bank 1 ?
	ldx	#$88+(slotnum*$10)

	ldy	#$00	; set Z3e to point to RAM where diag/partmgr load
	sty	Z3e
	lda	#$0a
	sta	Z3f

Lcnbb:	lda	(Z42),Y	; copy down to RAM
	sta	(Z3e),Y
	iny
	bne	Lcnc6
	inc	Z3f
	inc	Z43
Lcnc6:	cpy	Z44
	lda	Z43
	sbc	Z45
	bcc	Lcnbb

	asl	Dc0nf+(slotnum*$10)	; set bank 0
	plp
	jmp	L0a00

; table of bank 1 regions
Dcnd5:	fdb	b1_diag		; start of diag code
	fdb	b1_partmgr	; end of diag, start of partition manager
	fdb	b1_partmgr_end	; end of partition manager


slot_prodos:
	clv
	bvc	slot_prodos_x

slot_protocol_converter
	jsr	romsel
	jmp	protocol_converter

slot_prodos_x:
	jsr	romsel
	jmp	prodos
	
; cnea
	cmp	#(slotnum*$10)
	beq	Lcnf2
	tax
	ldy	#$0f
	rts

Lcnf2:	pla
	pla
	jsr	romsel
	jmp	Lcc6f

	fcb	$ae	; distinguishes vendor of mem exp card
			; $ae = Applied Engineering

	fcb	$01	; low bit set = ramdisk

	fdb	$0000	; block count (must be obtained by a STATUS request)
	fcb	$4f	; status byte
			;    7 = 0: non-removable
			;    6 = 1: interruptible
			; 5..4 = 0: 0 volumes on device
			;    3 = 1: supports format
			;    2 = 1: supports write
			;    1 = 1: supports read
			;    0 = 1: supports status
	fcb	slot_prodos & $db	; ProDOS entry point pointer

	dephase
	endsection	bank_banknum_slot_slotnum

	endm	; irp slotnum,...
	endm	; irp banknum,...


; C800 shared ROM code

	org	$c800

protocol_converter:
	jsr	Scd99

	ldx	#$09	; save ten bytes from Z42 on stack
Lc805:	lda	Z42,X
	pha
	dex
	bpl	Lc805
	tsx

	lda	D010c,X
	sta	Z46
	lda	D010b,X
	sta	Z45

	ldy	#$03
Lc818:	lda	(Z45),Y
	sta	Z41,Y
	inc	D010b,X
	bne	Lc825
	inc	D010c,X
Lc825:	dey
	bne	Lc818
	sty	D04f8
	sty	D0578
	sty	D05f8
	tax
	cmp	#$0a
	bcs	ret_pc_err_bad_cmd
	lda	Dc887,X
	cmp	(Z43),Y
	bne	ret_pc_err_bad_pcnt

	ldy	#$08
Lc83f:	lda	(Z43),Y
	sta	Z43,Y
	dey
	bne	Lc83f

	lsr		; first param is unit num, only allow 0 and 1
	bne	ret_pc_err_bad_unit_num

	ldx	D0778

	ldy	Z42	; protocol converter command
	lda	#$c8
	pha
	lda	Dc87d,Y
	pha
	lsr	Z44
	lda	Z47
	rts

ret_pc_err_bad_unit_num:
	lda	#$11
	fcb	$2c	; BIT abs opcode to skip two bytes
ret_pc_err_bad_cmd:
	lda	#pc_err_bad_cmd
	fcb	$2c	; BIT abs opcode to skip two bytes
ret_pc_err_bad_pcnt:
	lda	#pc_err_bad_pcnt
LC863:	sta	D04f8
Lc866:	ldx	#$00
Lc868:	pla
	sta	Z42,X
	inx
	cpx	#$0a
	bcc	Lc868
	ldy	D05f8
	ldx	D0578
	lda	D04f8
	bne	Lc87c
	clc
Lc87c:	rts

; dispatch table for Protocol Converter calls
Dc87d:	fcb	(pc_cmd_status-1)&$ff	; status
	fcb	(pc_cmd_read_block-1)&$ff	; read block
	fcb	(pc_cmd_write_block-1)&$ff	; write block
	fcb	(pc_cmd_format_init-1)&$ff	; format (no-op)
	fcb	(pc_cmd_control-1)&$ff	; control
	fcb	(pc_cmd_format_init-1)&$ff	; init
	fcb	(pc_cmd_rw_char-1)&$ff	; open character
	fcb	(pc_cmd_rw_char-1)&$ff	; close character
	fcb	(pc_cmd_read_bytes-1)&$ff	; read bytes
	fcb	(pc_cmd_write_bytes-1)&$ff	; write bytes

; parameter count for Protocol Converter calls
Dc887:	fcb	3	; status
	fcb	3	; read block
	fcb	3	; write block
	fcb	1	; format
	fcb	3	; control
	fcb	1	; init
	fcb	1	; open character
	fcb	1	; close character
	fcb	4	; read bytes
	fcb	4	; write bytes

pc_cmd_format_init:
	jmp	Lc866

pc_cmd_rw_char:
	jmp	ret_pc_err_bad_cmd

pc_cmd_write_bytes:
	bcs	Lc8c5

pc_cmd_read_bytes:
	bcs	Lc8b3
	bcc	pc_cmd_rw_char

pc_cmd_control:
	beq	pc_cmd_format_init

Lc89f:	lda	#$21
	jmp	Lc863

pc_cmd_read_block:
	bcc	pc_cmd_rw_char
	jsr	Sc90c
	bcs	Lc8c8
	lda	rdramrd
	asl	Z4b
	asl
	ror	Z4b
Lc8b3:	jmp	Lc923

pc_cmd_write_block:
	bcc	pc_cmd_rw_char
	jsr	Sc90c
	bcs	Lc8c8
	lda	rdramwrt
	asl	Z4b
	asl
	ror	Z4b
Lc8c5:	jmp	Lc982

Lc8c8:	jmp	Lc97d

pc_cmd_status:
	bcs	Lc8e0
	bne	Lc89f
	ldy	#$08
	sty	D0578
Lc8d4:	dey
	sta	(Z45),Y
	bne	Lc8d4
	lda	#$01
	sta	(Z45),Y
	jmp	Lc866

Lc8e0:	beq	Lc8e9
	ldy	#$19
	cmp	#$03
	bne	Lc89f
	fcb	$2c	; BIT abs opcode to skip two bytes
Lc8e9:	ldy	#$04
	sty	D0578
	dey
Lc8ef:	lda	Dc9c6,Y
	cpy	#$02
	bne	Lc904
	ldx	D07f8
	lda	D0678-$c0,X
	lsr
	sta	(Z45),Y
	dey
	lda	D06f8-$c0,X
	ror
Lc904:	sta	(Z45),Y
	dey
	bpl	Lc8ef
	jmp	Lc866

Sc90c:	asl
	sta	Z4a
	lda	Z48
	rol
	bcs	Lc922
	sta	Z4b
	lda	Z49
	cmp	#$01
	bcs	Lc922
	sta	Z47
	lda	#$02
	sta	Z48
Lc922:	rts

Lc923:	jsr	Sc964
	bcs	Lc97d
	bit	rdramwrt
	php
	asl
	bcc	Lc932
	sta	wrcardram
Lc932:	lda	Z48
	sta	D05f8
	beq	Lc947
Lc939:	lda	Dbffb,X
	sta	(Z45),Y
	iny
	bne	Lc939
	inc	Z46
	dec	Z48
	bne	Lc939
Lc947:	lda	Z47
	beq	Lc958
	sta	D0578
Lc94e:	lda	Dbffb,X
	sta	(Z45),Y
	iny
	cpy	Z47
	bne	Lc94e
Lc958:	sta	wrmainram
	plp
	bpl	Lc961
	sta	wrcardram
Lc961:	jmp	Lc866

Sc964:	lda	Z49
	sta	Dbff8,X
	lda	Z4a
	sta	Dbff9,X
	lda	Z4b
	and	#$7f
	sta	Dbffa,X
	jsr	Sca9d
	ldy	#$00
	lda	Z4b
	rts
Lc97d:	lda	#$2d
	jmp	Lc863

Lc982:	jsr	Sc964
	bcs	Lc97d
	bit	rdramrd
	php
	sta	rdmainram
	asl
	bcc	Lc994
	sta	rdcardram
Lc994:	lda	Z48
	sta	D05f8
	beq	Lc9a9
Lc99b:	lda	(Z45),Y
	sta	Dbffb,X
	iny
	bne	Lc99b
	inc	Z46
	dec	Z48
	bne	Lc99b
Lc9a9:	lda	Z47
	sta	D0578
	beq	Lc9ba
Lc9b0:	lda	(Z45),Y
	sta	Dbffb,X
	iny
	cpy	Z47
	bne	Lc9b0
Lc9ba:	sta	rdmainram
	plp
	bpl	Lc9c3
	sta	rdcardram
Lc9c3:	jmp	Lc866

Dc9c6:	fcb	$f8,$00,$00,$00

	fcb	$07,"RAMCARD"
	fcb	"         "
	
	fcb	$00,$00,$00,$00

Lc9df:	ldy	D07f8
	jsr	Sca8e
	lda	Dbffb,X
	cmp	#$ae
	bne	Lca31
	eor	Dbffb,X
	cmp	#$5a
	bne	Lca31
	lda	Dbffb,X
	sta	D04f8-$c0,Y
	eor	#$5a
	cmp	Dbffb,X
	bne	Lca31
	lda	Dbffb,X
	sta	D0478-$c0,Y
	lda	D04f8-$c0,Y
	sta	Dbff8,X
	lda	Dbffb,X
	sta	D0578-$c0,Y
	lda	Dbffb,X
	sta	D05f8-$c0,Y
	lda	Dbffb,X
	sta	D0678-$c0,Y
	lda	Dbffb,X
	sta	D06f8-$c0,Y
	lda	Dbffb,X
	sta	D0778-$c0,Y
	lda	Dbffb,X
	sta	D07f8-$c0,Y
	rts

Lca31:	lda	D0778-$c0,Y
	eor	D07f8-$c0,Y
	cmp	#$5a
	beq	Lca73
	jsr	Sca8e
	lda	#$04
	sta	Dbff9,X
	lda	Dbffb,X
	eor	Dbffb,X
	eor	Dbffb,X
	eor	Dbffb,X
	ldx	#$4c
	cmp	#$03
	beq	Lca61
	ldx	#$00
	cmp	#$06
	beq	Lca61
	ldx	#$33
	cmp	#$be
	bne	Lca6a
Lca61:	txa
	sta	D0778-$c0,Y
	eor	#$5a
	sta	D07f8-$c0,Y
Lca6a:	jsr	Scb00
	ldy	D07f8
	sta	D0478-$c0,Y
Lca73:	lda	D0478-$c0,Y
	asl
	sta	D0678-$c0,Y
	lda	#$00
	sta	D06f8-$c0,Y
	sta	D05f8-$c0,Y
	sta	D0578-$c0,Y
	lda	#$01
	sta	D04f8-$c0,Y
	rts

Sca8b:	ldx	D0778

Sca8e:	lda	#$00
	sta	Dbff8,X
	sta	Dbff9,X
	sta	Dbffa,X
	rts

Sca9a:	jsr	Sca8e

Sca9d:	ldy	D07f8
	lda	D0478-$c0,Y
	cmp	#$09
	lda	Dbffa,X
	bcs	Lcaac
	and	#$0f
Lcaac:	pha
	bne	Lcacd
	lda	D04f8-$c0,Y
	cmp	#$08
	bne	Lcacd
	lda	Dbff9,X
	cmp	#$02
	bcs	Lcacd
	ora	#$fe
	sta	Dbff9,X
	lda	D0478-$c0,Y
	sbc	#$00
	rol
	sta	Dbffa,X
Lcacb:	pla
	rts

Lcacd:	lda	Dbff9,X
	cmp	D06f8-$c0,Y
	pla
	pha
	sbc	D0678-$c0,Y
	bcs	Lcacb
	lda	Dbff9,X
	adc	D05f8-$c0,Y
	sta	Dbff9,X
	pla
	adc	D0578-$c0,Y
	sta	Dbffa,X
	rts

Scaeb:	ldy	#$00
Lcaed:	lda	Dbffb,X
	sta	D0800,Y
	iny
	bne	Lcaed
Lcaf6:	lda	Dbffb,X
	sta	D0900,Y
	iny
	bne	Lcaf6
	rts

Scb00:	jsr	Sca8b
	tay
	cmp	Dbffa,X
	bne	Lcb0b
	ldy	#$02
Lcb0b:	ora	Dcb95,Y
Lcb0e:	sta	Z3f
	sta	Z3e
	sta	Dbffa,X
	lda	Dbffb,X
	dec	Dbff8,X
	pha
	lda	Z3f
	sta	Dbffb,X
	dec	Dbff8,X
	and	Dcb95,Y
	beq	Lcb3d
	lda	Z3f
	sec
	sbc	Dcb98,Y
	jmp	Lcb0e

Lcb32:	clc
	lda	Z3f
	adc	Dcb98,Y
	sta	Z3f
	sta	Dbffa,X
Lcb3d:	lda	Dbffb,X
	cmp	Z3f
	bne	Lcb83
	eor	#$ff
	dec	Dbff8,X
	sta	Dbffb,X
	dec	Dbff8,X
	cmp	Dbffb,X
	bne	Lcb83
	dec	Dbff8,X
	lda	Z3f
	and	Dcb95,Y
	cmp	Dcb95,Y
	bne	Lcb32
Lcb61:	lda	Z3e
	sta	Dbffa,X
	pla
	sta	Dbffb,X
	dec	Dbff8,X
	clc
	lda	Z3e
	adc	Dcb98,Y
	sta	Z3e
	and	Dcb95,Y
	bne	Lcb61
	lda	Z3f
	dey
	bpl	Lcb0b
	lsr
	adc	#$02
	rts

Lcb83:	dec	Dbff8,X
	sec
	lda	Z3f
	beq	Lcb61
	sbc	Dcb98,Y
	sta	Z3f
	jmp	Lcb61

	fcb	$ea,$ea

Dcb95:	fcb	$0c,$30,$c0
Dcb98:	fcb	$04,$10,$40

Scb9b:
	fcb	$a2,$04,$a0,$00,$a9
	fcb	$00,$48,$38,$a5,$3e,$fd,$de,$cb
	fcb	$48,$a5,$3f,$fd,$e3,$cb,$90,$0a
	fcb	$85,$3f,$68,$85,$3e,$68,$69,$00
	fcb	$d0,$e7,$68,$68,$d0,$08,$88,$10
	fcb	$05,$8a,$f0,$02,$a9,$10,$c8,$49
	fcb	$b0,$20,$ed,$fd,$ca,$10,$d0,$a9
	fcb	$cb,$4c,$ed,$fd,$08,$08,$20,$38
	fcb	$50,$68,$80,$98,$b0,$c8,$04,$28
	fcb	$90,$a0,$40,$00,$00,$01,$0f,$9c

prodos:
	ldy	Z42
	iny
	beq	Scc58
	jsr	Scd99
	ldy	#$2b
	bcs	Lcc0c
	ldy	#$28
	lda	Z43
	bmi	Lcc0c
	ldy	Z42
	beq	Lcc0f
	dey
	beq	Lcc1f
	dey
	beq	Lcc3d
	dey
	beq	Lcc1b
	ldy	#$01
	fcb	$2c	; BIT abs opcode to skip two bytes
lcc0a:	ldy	#$27
Lcc0c:	tya
	sec
	rts

Lcc0f:	ldy	D07f8
	jsr	Scfe4
	ldy	D0478
	ldx	D04f8
Lcc1b:	lda	#$00
	clc
	rts

Lcc1f:	jsr	Scc58
	bcs	Lcc0a
	ldy	#$00
Lcc26:	lda	Dbffb,X
	sta	(Z44),Y
	iny
	bne	Lcc26
	inc	Z45
Lcc30:	lda	Dbffb,X
	sta	(Z44),Y
	iny
	bne	Lcc30
Lcc38:	dec	Z45
	tya
	clc
	rts

Lcc3d:	jsr	Scc58
	bcs	Lcc0a
	ldy	#$00
Lcc44:	lda	(Z44),Y
	sta	Dbffb,X
	iny
	bne	Lcc44
	inc	Z45
Lcc4e:	lda	(Z44),Y
	sta	Dbffb,X
	iny
	bne	Lcc4e
	beq	Lcc38

Scc58:	lda	#$00
	sta	Dbff8,X
	lda	Z46
	asl
	sta	Dbff9,X
	lda	Z47
	rol
	bcs	Lcc6e
	sta	Dbffa,X
	jsr	Sca9d
Lcc6e:	rts

Lcc6f:	jsr	Scd56
	ldy	#$10
	bcs	Lcce0
	jsr	Sccfb
	ldy	#$02
	lda	(Z48),Y
	sta	Z3e
	eor	#$01
	beq	Lcc88
	bcc	Lccdd
	jsr	Scd16
Lcc88:	ldy	#$0e
	lda	#$fe
	sta	(Z48),Y
	ldy	#$04
	sty	Z3f
	lda	(Z48),Y
	cmp	Dcd35,X
	bcs	Lccdd
	lsr
	ror	Z3f
	lsr
	ror	Z3f
	lsr
	ror	Z3f
	cpx	#$01
	beq	Lcca9
	lsr
	ror	Z3f
Lcca9:	pha
	iny
	lda	(Z48),Y
	cmp	Dcd37,X
	bcs	Lccdd
	ora	Z3f
	pha
	jsr	Sca8b
	pla
	sta	Dbff9,X
	pla
	sta	Dbffa,X
	jsr	Sca9d
	bcs	Lccdd
	ldy	#$08
	lda	(Z48),Y
	sta	Z3e
	iny
	lda	(Z48),Y
	sta	Z3f
	ldy	#$0c
	lda	(Z48),Y
	tay
	beq	Lccf4
	dey
	beq	Lccec
	dey
	beq	Lcce2
Lccdd:	sec
	ldy	#$80
Lcce0:	bne	Lccf5
Lcce2:	lda	(Z3e),Y
	sta	Dbffb,X
	iny
	bne	Lcce2
	beq	Lccf4
Lccec:	lda	Dbffb,X
	sta	(Z3e),Y
	iny
	bne	Lccec
Lccf4:	clc
Lccf5:	tya
	ldy	#$0d
	sta	(Z48),Y
	rts

Sccfb:	ldy	D07f8
	ldx	#$03
Lcd00:	lda	D06f8-$c0,Y
	cmp	Dcd31,X
	lda	D0678-$c0,Y
	sbc	Dcd2d,X
	bcs	Lcd12
	dex
	bpl	Lcd00
	rts

Lcd12:	txa
	lsr
	tax
	rts

Scd16:	clc
	ldy	D07f8
	lda	D05f8-$c0,Y
	adc	Dcd3b,X
	sta	D05f8-$c0,Y
	lda	D0578-$c0,Y
	adc	Dcd39,X
	sta	D0578-$c0,Y
	rts

Dcd2d:	fcb	$02,$04,$06,$0c
Dcd31:	fcb	$30,$60,$40,$80
Dcd35:	fcb	$23,$32
Dcd37:	fcb	$10,$20
Dcd39:	fcb	$02,$06
Dcd3b:	fcb	$30,$40


Scd3d:	ldy	D07f8
	jsr	Scfe4
	cmp	#$10
	bcs	Lcd4c
	lda	D0478
	beq	Lcd55
Lcd4c:	lda	D07f8-$c0,Y
	eor	#$5a
	cmp	D0778-$c0,Y
	sec
Lcd55:	rts

Scd56:	jsr	Scd3d
	bcc	Lcd97
	bne	Lcd63
	cmp	#$33
	bne	Lcd97
	clc
	rts

Lcd63:	lda	#$33
	sta	D0778-$c0,Y
	jsr	Sccfb
	bmi	Lcd97
	bcc	Lcd91
	lda	D05f8-$c0,Y
	pha
	lda	D0578-$c0,Y
	pha
	stx	D04f8
	jsr	Scd16
	ldy	Dcfe2,X
	jsr	Scdfb
	ldx	D04f8
	ldy	D07f8
	pla
	sta	D0578-$c0,Y
	pla
	sta	D05f8-$c0,Y
Lcd91:	ldy	Dcfe2,X
	jmp	Lcdc1

Lcd97:	sec
	rts

Scd99:	jsr	Scd3d
	bcc	Lcdb0
	bne	Lcdb2
	cmp	#$cd
	bne	Lcda6
Lcda4:	clc
	rts

Lcda6:	jsr	Scdec
	bne	Lcdb0
	cmp	D0778-$c0,Y
	beq	Lcda4
Lcdb0:	sec
	rts

Lcdb2:	jsr	Scdec
	bne	Lcdb0
	sta	D0778-$c0,Y
	ldy	#$00
	asl
	bne	Lcdc1
	ldy	#$87
Lcdc1:	jsr	Scdfb
	jsr	Sca8e
	ldy	D07f8
	lda	D04f8-$c0,Y
	cmp	#$01
	beq	Lcde2
	clc
	adc	#$04
	sta	Dbff8,X
	lda	D0778-$c0,Y
	sta	Dbffb,X
	eor	#$5a
	sta	Dbffb,X
Lcde2:	lda	D0778-$c0,Y
	eor	#$5a
	sta	D07f8-$c0,Y
	clc
	rts

Scdec:	lda	proflag
	beq	Lcdf3
	cmp	#$4c
Lcdf3:	rts

Lcdf4:	ldx	D0778
	jsr	Sce67
	iny

Scdfb:	lda	Dcf30,Y
	bne	Lcdf4
	rts

Lce01:	and	#$0f
	sta	Dbffb,X

Sce06:	lda	#$00
	sta	Dbffb,X
	rts

Lce0c:	tya
	pha
	lda	D04f8
	cmp	#$01
	lda	D0478
	sbc	#$00
	lsr
	lsr
	lsr
	lsr
	tay
	lda	#$ff
	sta	Z3e
	lda	#$03
Lce23:	lsr
	ror	Z3e
	dey
	bpl	Lce23
	sta	Dbffb,X
	lda	Z3e
	sta	Dbffb,X
	pla
	tay
	rts

Lce34:	jsr	Sce53
	lda	#$01
Lce39:	pha
	jsr	Sce06
	lda	#$11
	sta	Dbffb,X
	pla
	pha
	sta	Dbffb,X
	jsr	Sce53
	pla
	clc
	adc	#$01
	cmp	#$1f
	bcc	Lce39
	rts

Sce53:	lda	#$00
	fcb	$2c	; BIT abs opcode to skip two bytes
Lce56:	lda	#$ff
Lce58:	sta	Dbffb,X
	cmp	Dbff8,X
	bne	Lce58
	ror
	bcc	Lce66
	sta	Dbffb,X
Lce66:	rts

Sce67:	cmp	#$40
	bcs	Lce8e
	cmp	#$30
	bcs	Lcead
	cmp	#$20
	bcs	Lce01
	cmp	#$10
	bcs	Lce0c
	cmp	#$04
	beq	Lcec5
	bcs	Lce85
	cmp	#$02
	bcc	Lced9
	beq	Sce53
	bcs	Lce56
Lce85:	cmp	#$06
	bcc	Lced2
	beq	Lce34
	jmp	Lcef4

Lce8e:	pha
	and	#$3f
	beq	Lceab
	pla
	pha
	and	#$c0
	asl
	beq	Lcea2
	lda	#$ff
	bcs	Lcea2
	iny
	lda	Dcf30,Y
Lcea2:	sta	Dbffb,X
	pla
	sec
	sbc	#$01
	bne	Lce8e
Lceab:	pla
	rts

Lcead:	and	#$0f
Lceaf:	pha
	lda	#$ff
	sta	Dbffb,X
	sta	Dbffb,X
	jsr	Sce06
	sta	Dbffb,X
	pla
	sec
	sbc	#$01
	bne	Lceaf
	rts

Lcec5:	lda	D04f8
	sta	Dbffb,X
	lda	D0478
Lcece:	sta	Dbffb,X
	rts

Lced2:	lda	D07f8
	eor	#$f0
	bne	Lcece
Lced9:	lda	#$00
	sta	Dbff8,X
	iny
	lda	Dcf30,Y
	sta	Dbff9,X
	iny
	lda	Dcf30,Y
	sta	Dbffa,X
	tya
	pha
	jsr	Sca9d
	pla
	tay
	rts

Lcef4:	tya
	pha
	ldy	D0478
	beq	Lcf08
	nop
Lcefc:	tya
	pha
	ldy	#$20
	jsr	Scf27
	pla
	tay
	dey
	bne	Lcefc
Lcf08:	lda	D04f8
	pha
	lsr
	lsr
	lsr
	beq	Lcf15
	tay
	jsr	Scf27
Lcf15:	pla
	and	#$07
	tay
	lda	#$00
Lcf1b:	sec
	ror
	dey
	bpl	Lcf1b
	asl
	sta	Dbffb,X
	pla
	tay
	rts

Scf27:	lda	#$ff
Lcf29:	sta	Dbffb,X
	dey
	bne	Lcf29
	rts

Dcf30:	fcb	$01,$00,$00,$02,$02,$01,$02,$00
	fcb	$02,$02,$20,$23,$44,$f4
	fcb	"RAM"
	fcb	$05,$99,$43,$c3,$27,$0d,$20
	fcb	$26,$04,$02,$02,$22,$24,$02,$02
	fcb	$23,$25,$02,$02,$24,$20,$02,$02
	fcb	$07,$02,$02,$01,$0c,$00,$10,$00
	fcb	$01,$00,$00,$02,$01,$04,$00,$44
	fcb	$02,$0f,$20,$93,$01,$10,$01,$47
	fcb	$33,$11,$0f,$04,$00,$00,$fe,$a0
	fcb	$41,$7a,$88,$48,$11,$01,$00,$00
	fcb	$23,$10,$00,$01,$88,$3f,$84,$3f
	fcb	$32,$02,$06,$00,$01,$00,$00,$02
	fcb	$01,$04,$00,$44,$02,$0f,$20,$93
	fcb	$01,$20,$02,$47,$33,$11,$1f,$04
	fcb	$00,$00,$fe,$a0,$41,$7a,$88,$48
	fcb	$11,$01,$00,$00,$32,$20,$00,$01
	fcb	$84,$c8,$f8,$84,$03,$06,$00,$01
	fcb	$00,$00,$02,$02,$01,$02,$00,$02
	fcb	$02,$82,$41,$06,$83,$44,$04
	fcb	"RAM"
	fcb	$05,$83,$04,$02,$02,$02
	fcb	$02,$02,$02,$02,$02,$02,$02,$02
	fcb	$02,$00,$44,$ae,$f4,$08,$52,$02
	fcb	$02,$00

Dcfe2:	fcb	$30,$5c

Scfe4:	lda	D0678-$c0,Y
	lsr
	sta	D0478
	lda	D06f8-$c0,Y
	ror
	sta	D04f8
	rts

	fillto	$d000,$ff

; bank 1 common space ($c800-$cfff)
	org	$d800

b1_diag	equ	*-$1000

	section	diag
	phase	$0a00

Z24	equ	$24
Z28	equ	$28
Z29	equ	$29

D0400	equ	$0400
	
D0d29	equ	$0d29
D0d2a	equ	$0d2a
D0d2b	equ	$0d2b
D0d2f	equ	$0d2f

diag:	stx	D0d2b
	jsr	home
	ldy	#$00
	jsr	S0c7d
	jsr	Scb00
	sta	D0d2a
	ldy	#$00
	sty	D0d2f
	asl
	bcc	L0a1b
	dey
	tya
L0a1b:	sty	Z3e
	sta	Z3f
	jsr	Scb9b
	ldy	#$21
	jsr	S0c7d
	jsr	Sca8b
	sta	Dbffb,X
	lda	#$04
	sta	Dbff9,X
	lda	#$ff
	sta	Dbffb,X
	ldy	D07f8
	sta	D0778-$c0,Y
L0a3d:	lda	#$00
	sta	Z24
	ldy	#$27
	jsr	S0c7d
	lda	D0d2f
	jsr	prbyte
	ldy	#$2c
	jsr	S0c7d
	lda	Z24
	clc
	adc	Z28
	sta	S0ae4+1
	lda	#$00
	adc	Z29
	sta	S0ae4+2
	ldy	#$08
L0a62:	dey
	dey
	sty	D0d29
	jsr	S0ae4
	jsr	S0ae8
	bcs	L0a80
	ldy	D0d29
	bne	L0a62
	inc	D0d2f
	lda	kbd
	bpl	L0a3d
	sta	kbdstrb
	rts

L0a80:	tya
	pha
	ldy	#$35
	jsr	S0c7d
	lda	#$08
	sec
	sbc	D0d29
	lsr
	tay
	ora	#$b0
	jsr	cout
	cpy	#$03
	bcs	L0a9a
	pla
	rts

L0a9a:	ldy	#$42
	jsr	S0c7d
	ldx	D0778
	lda	Dbff8,X
	dec	Dbff8,X
	and	#$7f
	bne	L0ab9
	lda	Dbff9,X
	dec	Dbff9,X
	and	#$7f
	bne	L0ab9
	dec	Dbffa,X
L0ab9:	lda	Dbffa,X
	ldy	D0d2a
	cpy	#$09
	bcs	L0ac5
	and	#$0f
L0ac5:	jsr	prbyte
	lda	Dbff9,X
	jsr	prbyte
	lda	Dbff8,X
	jsr	prbyte
	lda	#$ad
	jsr	cout
	pla
	eor	Dbffb,X
	jsr	prbyte
	jsr	crout
	rts

S0ae4:	inc	D0400
	rts

S0ae8:	lda	D0af5,Y
	pha
	lda	D0af4,Y
	pha
	ldx	D0d2b
	rts

D0af4:	fcb	$e3
D0af5:	fcb	$0b,$80,$0b,$1d,$0b,$fb,$0a,$a0
	fcb	$00,$98,$20,$0a,$0d,$5d,$fa,$bf
	fcb	$29,$0f,$d0,$13,$98,$dd,$f9,$bf
	fcb	$d0,$0d,$dd,$f8,$bf,$d0,$08,$20
	fcb	$e4,$0a,$c8,$d0,$e4,$18,$60,$38
	fcb	$60,$a9,$00,$a0,$f0,$20,$0a,$0d
	fcb	$ad,$28,$0d,$20,$6b,$0b,$8d,$28
	fcb	$0d,$ad,$27,$0d,$20,$6b,$0b,$8d
	fcb	$27,$0d,$20,$13,$0d,$ee,$28,$0d
	fcb	$d0,$0c,$ee,$27,$0d,$d0,$07,$ee
	fcb	$26,$0d,$d0,$02,$18,$60,$20,$e4
	fcb	$0a,$bd,$fb,$bf,$bd,$fa,$bf,$4d
	fcb	$26,$0d,$d0,$10,$bd,$f9,$bf,$cd
	fcb	$27,$0d,$d0,$08,$bd,$f8,$bf,$cd
	fcb	$28,$0d,$f0,$bc,$38,$60,$c9,$10
	fcb	$90,$06,$c9,$70,$b0,$03,$a9,$70
	fcb	$60,$c9,$90,$90,$fb,$c9,$f0,$b0
	fcb	$f7,$a9,$f0,$60,$ad,$2a,$0d,$f0
	fcb	$0e,$a9,$00,$8d,$30,$0d,$20,$96
	fcb	$0b,$20,$96,$0b,$20,$96,$0b,$18
	fcb	$60,$20,$8e,$ca,$a8,$98,$9d,$fb
	fcb	$bf,$20,$be,$0b,$d0,$f7,$20,$e4
	fcb	$0a,$98,$20,$0a,$0d,$98,$dd,$fb
	fcb	$bf,$d0,$0a,$20,$be,$0b,$d0,$f5
	fcb	$38,$6e,$30,$0d,$60,$68,$68,$38
	fcb	$60,$2c,$30,$0d,$10,$18,$ee,$26
	fcb	$0d,$fe,$fa,$bf,$ad,$26,$0d,$4a
	fcb	$cd,$2a,$0d,$90,$09,$a0,$00,$60
	fcb	$ee,$27,$0d,$fe,$f9,$bf,$ee,$28
	fcb	$0d,$c8,$60,$00,$55,$aa,$ff,$ad
	fcb	$2a,$0d,$f0,$13,$a0,$03,$b9,$e0
	fcb	$0b,$8d,$2c,$0d,$20,$fe,$0b,$20
	fcb	$2e,$0c,$b0,$04,$88,$10,$ef,$18
	fcb	$60,$20,$6d,$0c,$20,$0a,$0c,$ce
	fcb	$2d,$0d,$d0,$f8,$60,$98,$48,$ad
	fcb	$2c,$0d,$a0,$00,$8c,$2e,$0d,$9d
	fcb	$fb,$bf,$9d,$fb,$bf,$9d,$fb,$bf
	fcb	$9d,$fb,$bf,$c8,$d0,$f1,$20,$e4
	fcb	$0a,$ee,$2e,$0d,$d0,$e9,$68,$a8
	fcb	$60,$20,$6d,$0c,$20,$3c,$0c,$b0
	fcb	$05,$ce,$2d,$0d,$d0,$f6,$60,$98
	fcb	$48,$ad,$2c,$0d,$a0,$00,$8c,$2e
	fcb	$0d,$dd,$fb,$bf,$d0,$1e,$dd,$fb
	fcb	$bf,$d0,$19,$dd,$fb,$bf,$d0,$14
	fcb	$dd,$fb,$bf,$d0,$0f,$c8,$d0,$e9
	fcb	$20,$e4,$0a,$ee,$2e,$0d,$d0,$e1
	fcb	$68,$a8,$18,$60,$a8,$68,$38,$60
	fcb	$ad,$2a,$0d,$4a,$8d,$2d,$0d,$4c
	fcb	$8e,$ca

L0c77:	php
	jsr	cout
	plp
	fcb	$24	; BIT zp opcode to skip one byte
S0c7d:	clc
	jsr	S0c8f
	lda	D0ca1,X
	bne	L0c77
	jsr	S0c8f
	lda	D0cb0,X
	bne	L0c77
	rts

S0c8f:	lda	D0cc0,Y
	bcs	L0c9b
	lsr
	lsr
	lsr
	lsr
	tax
	sec
	rts

L0c9b:	iny
	and	#$0f
	tax
	clc
	rts

D0ca1:	fcb	$00,$8d,$d2,$c1,$cd,$ae,$d4,$c9	; "..RAM.TI"
	fcb	$d3,$a0,$c7,$cf,$cc,$c4,$c5   	; "S GOLDE"
D0cb0:	fcb	$ce,$00,$c2,$c3,$c6,$c8,$d0,$d9	; "N.BCFHPY"
	fcb	$da,$cd,$d1,$d5,$d6,$b1,$b4,$ba	; "ZMQUV14:"
D0cc0:	fcb	$30,$60,$6c,$7e,$d9,$ef,$a7,$fe
	fcb	$e2,$7f,$a9,$23,$40,$43,$03,$6b
	fcb	$29,$6e,$86,$90,$c0,$d5,$0e,$11
	fcb	$4e,$4b,$20,$79,$87,$08,$e0,$f9
	fcb	$01,$90,$20,$76,$e8,$11,$01,$06
	fcb	$38,$80,$f9,$01,$99,$99,$6e,$86
	fcb	$7f,$a9,$55,$50,$19,$11,$03,$32
	fcb	$d9,$04,$37,$c0,$b2,$e5,$97,$d0
	fcb	$f9,$01,$99,$93,$dd,$2e,$88,$0f
	fcb	$90,$19,$8d,$28,$0d,$8d,$27,$0d
	fcb	$8c,$26,$0d,$ad,$28,$0d,$9d,$f8
	fcb	$bf,$ad,$27,$0d,$9d,$f9,$bf,$ad
	fcb	$26,$0d,$9d,$fa,$bf,$60

	dephase
	endsection diag

b1_diag_end	equ	*-$1000

b1_partmgr	equ	*-$1000

	section partmgr
	phase	$0a00

Z24	equ	$24
Z25	equ	$25
Z32	equ	$32
Z3e	equ	$3e
Z3f	equ	$3f

D03b8	equ	$03b8
D06b8	equ	$06b8
D0738	equ	$0738
D0778	equ	$0778
nnD07f8	equ	$07f8

D0800	equ	$0800
D0802	equ	$0802
D0803	equ	$0803
D0804	equ	$0804
D0805	equ	$0805
D0808	equ	$0808
D0900	equ	$0900
D0901	equ	$0901
D0902	equ	$0902
D0903	equ	$0903
D0908	equ	$0908

Dbff8	equ	$bff8
Dbffb	equ	$bffb

Lc9df	equ	$c9df
Sca8b	equ	$ca8b
Sca8e	equ	$ca8e
Scaeb	equ	$caeb
Scb9b	equ	$cb9b
Scdfb	equ	$cdfb

bell12	equ	$fbe2
clreop	equ	$fc42
home	equ	$fc58
rdkey	equ	$fd0c
crout	equ	$fd8e
cout	equ	$fded

partmgr:
	jsr	Sca8b
	lda	Dbffb,X
	cmp	#$ae
	bne	L0a1b
	eor	Dbffb,X
	cmp	#$5a
	bne	L0a1b
	lda	Dbffb,X
	eor	Dbffb,X
	cmp	#$5a
	beq	L0a6b
L0a1b:	ldy	D07f8
	lda	D06b8,Y
	eor	D0738,Y
	cmp	#$5a
	bne	L0a3f
	jsr	home
	ldy	#$88
	jsr	S0c8e
	lda	#'?'+$80
	jsr	cout
	jsr	rdkey_uc
	cmp	#'Y'+$80
	beq	L0a3f
	jmp	L0e06

L0a3f:	jsr	Sca8b
	ldy	#$aa
	jsr	Scdfb
	jsr	Sca8b
	lda	#$04
	sta	Dbff8,X
	ldy	D07f8
	lda	D03b8,Y
	pha
	sta	Dbffb,X
	lda	#$0a
	sta	Dbff8,X
	pla
	asl
	sec
	sbc	#$01
	sta	Dbffb,X
	lda	#$fc
	sta	Dbffb,X
L0a6b:	jsr	Sca8e
	jsr	Scaeb
	lda	D0802
	sta	D0901
	jsr	home
	lsr	D0900
L0a7d:	lda	#$00
	sta	Z24
	sta	Z25
	ldy	#$00
	jsr	S0c8e
	lda	D07f8
	eor	#$70
	jsr	cout
	jsr	crout
	jsr	crout
	ldy	#$00
L0a98:	jsr	S0c03
	iny
	cpy	#$09
	bcc	L0a98
	ldy	#$14
	jsr	S0c8e
	ldy	#$59
	bit	D0900
	bpl	L0aae
	ldy	#$27
L0aae:	jsr	S0c8e
	jsr	clreop

	jsr	rdkey_uc
	cmp	#'1'+$80
	bcc	L0acf		; not a digit
	cmp	#'9'+1+$80
	bcs	L0acf		; not a digit

	sbc	#$b0		; ASCII to binary digit
	jsr	S0c81
	sta	D0901
	bit	D0900
	bmi	L0a7d
	jmp	L0df6

L0acf:	sta	D0902
	ldy	#$06
	bit	D0900
	bpl	L0adb
	ldy	#$fd	; 253 }
L0adb:	iny
	iny
	iny
	lda	D0af7,Y
	beq	L0af1
	cmp	D0902
	bne	L0adb
	lda	D0af9,Y
	pha
	lda	D0af8,Y
	pha
	rts

L0af1:	jsr	bell12
	jmp	L0a7d

D0af7:	fcb	$ce                     	; "N"
D0af8:	fcb	$50                     	; "P"
D0af9:	fcb	$0b,$c3,$f4,$0b,$d3,$70,$0b,$8d	; ".Ct.Sp.."
	fcb	$15,$0b,$88,$38,$0b,$8b,$38,$0b	; "...8..8."
	fcb	$8a,$48,$0b,$95,$48,$0b,$9b,$2a	; ".H..H..*"
	fcb	$0b,$d2,$23,$0b,$00,$2c,$00,$09	; ".R#..,.."
	fcb	$30,$03,$4c,$f6,$0d,$20,$e8,$0d	; "0.Lv. h."
	fcb	$4c,$7a,$0a,$38,$6e,$00,$09,$4c	; "Lz.8n..L"
	fcb	$7d,$0a,$2c,$00,$09,$10,$03,$4c	; "}.,....L"
	fcb	$00,$0a,$20,$d7,$0d,$4c,$36,$0e	; ".. W.L6."
	fcb	$ad,$01,$09,$38,$e9,$18,$c9,$e0	; "-..8i.I`"
	fcb	$b0,$03,$8d,$01,$09,$4c,$7d,$0a	; "0....L}."
	fcb	$ad,$01,$09,$18,$69,$18,$90,$ee	; "-...i..n"
	fcb	$a0,$76,$20,$8e,$0c,$a2,$10,$20	; " v ..". "
	fcb	$4f,$0e,$b0,$11,$a2,$00,$ac,$01	; "O.0.".,."
	fcb	$09,$bd,$08,$09,$99,$08,$08,$c8	; ".=.....H"
	fcb	$e8,$e0,$10,$90,$f4,$4c,$7d,$0a	; "h`..tL}."
	fcb	$20,$43,$0e,$f0,$74,$98,$18,$69	; " C.pt..i"
	fcb	$18,$a8,$c0,$e0,$b0,$6b,$20,$46	; ".(@`0k F"
	fcb	$0e,$f0,$66,$a0,$7f,$20,$8e,$0c	; ".pf . .."
	fcb	$a2,$06,$20,$4f,$0e,$20,$a1,$0e	; "". O. !."
	fcb	$0e,$04,$09,$2e,$05,$09,$0e,$04	; "........"
	fcb	$09,$2e,$05,$09,$18,$ac,$01,$09	; ".....,.."
	fcb	$b9,$03,$08,$79,$1b,$08,$8d,$06	; "9..y...."
	fcb	$09,$b9,$02,$08,$79,$1a,$08,$8d	; ".9..y..."
	fcb	$07,$09,$38,$ad,$06,$09,$ed,$04	; "..8-..m."
	fcb	$09,$48,$ad,$07,$09,$ed,$05,$09	; ".H-..m.."
	fcb	$90,$23,$99,$1a,$08,$68,$99,$1b	; ".#...h.."
	fcb	$08,$18,$ad,$04,$09,$99,$03,$08	; "..-....."
	fcb	$79,$01,$08,$99,$19,$08,$ad,$05	; "y.....-."
	fcb	$09,$99,$02,$08,$79,$00,$08,$99	; "....y..."
	fcb	$18,$08,$4c,$7d,$0a,$68,$a0,$d5	; "..L}.h U"
	fcb	$2c,$a0,$c9,$20,$8e,$0c,$20,$0c	; ", I .. ."
	fcb	$fd,$4c,$7d,$0a,$20,$43,$0e,$d0	; "}L}. C.P"
	fcb	$06,$ae,$01,$09,$fe,$05,$08,$4c	; "....~..L"
	fcb	$7d,$0a                  	; "}."

S0c03:	tya
	pha
	jsr	S0c81
	pha
	ldx	#$ff
	cmp	D0901
	bne	L0c12
	ldx	#$3f
L0c12:	stx	Z32
	jsr	S0c79
	tya
	clc
	adc	#$b1
	jsr	cout
	jsr	S0c79
	pla
	pha
	tay
	ldx	#$10
L0c26:	lda	D0808,Y
	bne	L0c2d
	lda	#' '+$80
L0c2d:	jsr	cout
	iny
	dex
	bne	L0c26
	jsr	S0c79
	pla
	pha
	tay
	lda	D0802,Y
	sta	Z3f
	lda	D0803,Y
	sta	Z3e
	jsr	Scb9b
	lda	#$ff
	sta	Z32
	jsr	S0c79
	pla
	tay
	lda	D0805,Y
	eor	#$5a
	cmp	D0804,Y
	beq	L0c5c
	lda	#$01
L0c5c:	ldy	#$04
L0c5e:	cmp	D0c6f,Y
	beq	L0c66
	dey
	bne	L0c5e
L0c66:	lda	D0c74,Y
	tay
	jsr	S0c8e
	pla
	tay
D0c6f:	rts

	fcb	$4c,$00,$33,$cd            	; "L.3M"
D0c74:	fcb	$df,$e9,$f5,$e4,$ef         	; "_iudo"

S0c79:	lda	#' '+$80
	jsr	cout
	jmp	cout

S0c81:	sta	D0902
	asl
	adc	D0902
	asl
	asl
	asl
	adc	#$08
	rts

S0c8e:	clc
L0c8f:	jsr	S0cac
	lda	D0cbe,X
	bne	L0ca0
	jsr	S0cac
	lda	D0ccd,X
	bne	L0ca0
	rts

L0ca0:	php
	bmi	L0ca6
	jsr	S0c79
L0ca6:	jsr	cout
	plp
	bne	L0c8f

S0cac:	lda	D0cdd,Y
	bcs	L0cb8
	lsr
	lsr
	lsr
	lsr
	tax
	sec
	rts

L0cb8:	iny
	and	#$0f
	tax
	clc
	rts

D0cbe:	fcb	$00,$8d,$01,$c7,$cc,$c9,$d4,$c3	; "...GLITC"
	fcb	$c8,$a0,$d2,$c5,$c1,$d3,$cf   	; "H REASO"
D0ccd:	fcb	$ce,$00,$c6,$d0,$d5,$c4,$cd,$ad	; "N.FPUDM-"
	fcb	$b1,$b9,$bd,$c2,$d1,$d7,$da,$d9	; "19=BQWZY"
D0cdd:	fcb	$1a,$c0,$60,$2c,$76,$ea,$90,$3c	; ".@`,vj.<"
	fcb	$a6,$56,$5e,$fd,$22,$29,$9d,$4e	; "&V^}").N"
	fcb	$69,$0a,$90,$19,$10,$4d,$b9,$ca	; "i....M9J"
	fcb	$ae,$0d,$d9,$ea,$90,$80,$70,$99	; "..Yj..p."
	fcb	$6e,$9d,$b4,$b7,$61,$10,$19,$f0	; "n.47a..p"
	fcb	$af,$c0,$6b,$97,$8c,$f3,$b2,$29	; "/@k..s2)"
	fcb	$ab,$60,$a5,$fd,$6c,$44,$97,$8c	; "+`%}lD.."
	fcb	$f3,$bd,$1d,$0a,$d5,$0e,$b9,$78	; "s=..U.9x"
	fcb	$cf,$3b,$22,$9b,$d7,$0a,$02,$ea	; "O;".W..j"
	fcb	$3b,$69,$78,$cf,$3b,$d1,$70,$a7	; ";ixO;Qp'"
	fcb	$4b,$ca,$90,$3c,$a6,$56,$5e,$f0	; "KJ.<&V^p"
	fcb	$19,$ab,$60,$a0,$be,$e6,$96,$8b	; ".+` >f.."
	fcb	$90,$3c,$a6,$56,$5e,$f2,$9a,$0a	; ".<&V^r.."
	fcb	$ab,$7e,$f0,$25,$30,$4a,$b1,$bd	; "+~p%0J1="
	fcb	$70,$a0,$c0,$45,$60,$19,$11,$fb	; "p @E`..{"
	fcb	$0d,$9f,$c0,$6b,$90,$a9,$01,$11	; "..@k.).."
	fcb	$fb,$0d,$9d,$50,$eb,$90,$a9,$01	; "{..Pk.)."
	fcb	$10,$dc,$af,$5f,$30,$79,$5f,$d6	; ".\/_0y_V"
	fcb	$c4,$45,$f3,$90,$3c,$a6,$56,$5e	; "DEs.<&V^"
	fcb	$fd,$90,$5b,$d6,$ae,$0f,$d1,$68	; "}.[V..Qh"
	fcb	$b9,$05,$5a,$b7,$6e,$a0,$f0,$79	; "9.Z7n py"
	fcb	$3e,$9c,$8b,$c0,$50,$19,$11,$7c	; ">..@P..|"
	fcb	$ff,$e6,$90,$be,$e6,$96,$8c,$69	; ".f.>f..i"
	fcb	$03,$ca,$65,$65,$ef,$10,$be,$e6	; ".Jeeo.>f"
	fcb	$90,$2a,$e0,$69,$d4,$e6,$90,$a9	; ".*`iTf.)"
	fcb	$01,$11,$7c,$ff,$e6,$97,$8c,$f3	; "..|.f..s"
	fcb	$b9,$d5,$0e,$b0,$19,$11,$d5,$0e	; "9U.0..U."
	fcb	$b9,$6e,$e9,$4c,$a3,$b0,$19,$74	; "9niL#0.t"
	fcb	$bc,$a9,$10,$19,$05,$ed,$99,$91	; "<)...m.."
	fcb	$01,$03,$ae,$05,$ed,$10,$19,$70	; "....m..p"
	fcb	$30,$70,$69,$91,$01,$03,$cd,$7c	; "0pi...M|"
	fcb	$41,$01                  	; "A."

S0dd7:	lda	D0901
	sta	D0802
	eor	#$5a
	sta	D0803
	jsr	S0de8
	jmp	Lc9df

S0de8:	jsr	Sca8b
	tay
L0dec:	lda	D0800,Y
	sta	Dbffb,X
	iny
	bne	L0dec
	rts

L0df6:	jsr	S0dd7
	jsr	S0e43
	bne	L0e09
	lda	D0802,Y
	ora	D0803,Y
	beq	L0e09
L0e06:	jsr	S0e27
L0e09:	ldy	#$ae
	jsr	S0c8e
L0e0e:	ldx	#$01
	jsr	S0e4f
	bcs	L0e36
	lda	D0908
	cmp	#$b1
	bcc	L0e0e
	cmp	#$b8
	bcs	L0e0e
	adc	#$10
	tay
	lda	#$00
	beq	L0e2f

S0e27:	ldx	D0778
	ldy	D07f8
	lda	#$7b
L0e2f:	sty	L00+1
	sta	L00
	jmp	(L00)

L0e36:	ldx	#$02
	lda	#$00
L0e3a:	sta	D0800,X
	dex
	bpl	L0e3a
	jmp	(resetvec)

S0e43:	ldy	D0901
	lda	D0804,Y
	eor	D0805,Y
	cmp	#$5a
	rts

S0e4f:	stx	D0903
	lda	#$a0
L0e54:	sta	D0908,X
	dex
	bpl	L0e54
	inx
L0e5b:	jsr	rdkey_uc
	cmp	#$88
	beq	L0e84
	cmp	#$8d
	beq	L0e82
	cmp	#$9b
	beq	L0e83
	cmp	#$a0
	bcs	L0e74
L0e6e:	jsr	bell12
	jmp	L0e5b

L0e74:	cpx	D0903
	bcs	L0e6e
	sta	D0908,X
	jsr	cout
	inx
	bne	L0e5b
L0e82:	clc
L0e83:	rts

L0e84:	txa
	beq	L0e5b
	dex
	dec	Z24
	lda	#$a0
	sta	D0908,X
	jsr	cout
	dec	Z24
	jmp	L0e5b

rdkey_uc:
	jsr	rdkey
	cmp	#$e0	; lower case?
	bcc	L0ea0	;   no
	and	#$df	;   yes, convert to upper case
L0ea0:	rts

	fcb	$a9,$00,$a8,$aa,$8d,$04,$09,$8e	; ").(*...."
	fcb	$05,$09,$a9,$0a,$8d,$02,$09,$a2	; "..)....""
	fcb	$00,$b9,$08,$09,$49,$b0,$c9,$0a	; ".9..I0I."
	fcb	$90,$01,$60,$6d,$04,$09,$48,$8a	; "..`m..H."
	fcb	$6d,$05,$09,$aa,$68,$ce,$02,$09	; "m..*hN.."
	fcb	$d0,$f1,$c8,$d0,$d7         	; "PqHPW"

	dephase
	endsection partmgr

b1_partmgr_end	equ	*-$1000

	fillto	$dfff,$ff
