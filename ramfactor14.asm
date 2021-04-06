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

fcsm	macro	s
	irpc	c,s
	fcb	'c'|$80
	endm
	endm

skip1	macro
	fcb	$24	; BIT zero page opcoe
	endm

skip2	macro
	fcb	$2c	; BIT absolute opcode
	endm


; macro for command table, used in partition manager

cmd_ent	macro	char,addr
	fcb	char+$80
	fdb	addr-1	; -1 because RTS will do the jump
	endm


; macros to handle the text compression

enc_nib	macro	nibval
	if	nib_msb
nib_save set nibval
	else
	 fcb	(nib_save<<4)|nibval
	endif
nib_msb	set	1-nib_msb
	endm

enc_ch	macro	ch
idx	set	strstr(tbl1,ch)
	if	idx >= 0
	 enc_nib idx
	else
idx	 set	strstr(tbl2,ch)
	 if	idx >= 0
	  enc_nib 0
	  enc_nib idx
	 else
	  error "unable to encode character"
	 endif
	endif
	endm

enc_end	macro
idx	set	strstr(tbl1,"\xff")
	if	idx >= 0
	 enc_nib	idx
	else
	 error	"no zero entry in tbl1"
	endif
idx	set	strstr(tbl2,"\xff")
	if	idx >= 0
	 enc_nib	idx
	else
	 error	"no zero entry in tbl2"
	endif
	if	nib_msb == 0
	 enc_nib	9  ; if there's a leftover nibble at the end, fill with 9
	endif
	endm

;encode2	macro	arg
;	if	"arg"<>""
;	 irpc	char,arg
;	  enc_ch "char"
;	 endm
;         shift
;	 encode2 ALLARGS
;	endif
;	endm

;encode	macro	arg
;nib_msb	set	1
;	encode2	allargs
;	enc_end
;	endm

encode	macro	arg
nib_msb	set	1
	irpc	char,arg
	 enc_ch "char"
	endm
	enc_end
	endm


ch_bs		equ	$08
ch_lf		equ	$0a
ch_vt		equ	$0b
ch_cr		equ	$0d
ch_nak		equ	$15
ch_esc		equ	$1b

key_left	equ	ch_bs
key_right	equ	ch_nak
key_up		equ	ch_vt
key_down	equ	ch_lf



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
D0400	equ	$0400

; screen holes, global (not indexed)
; mslot appears to be the only global screen hole for which Apple
; has defined a function.
shg_0478	equ	$0478
shg_04f8	equ	$04f8
shg_0578	equ	$0578
shg_05f8	equ	$05f8
shg_0778	equ	$0778
mslot		equ	$07f8

; screen holes, indexed by $Cn, where n is slot number (1 through 7)
shs_card_block_count	equ	$0478-$c0	; # blocks of whole card, divided by 256
shs_idx_part_data	equ	$04f8-$c0	; index to partition data
shs_part_base_high	equ	$0578-$c0	; partition base address, high bbyte
shs_part_base_mid	equ	$05f8-$c0	; partition base address, mid byte
shs_cur_part_size_high	equ	$0678-$c0	; current partition size, pages, high byte
shs_cur_part_size_low	equ	$06f8-$c0	; current partition size, pages, low byte
shs_os_code		equ	$0778-$c0	; operating system code
shs_os_check		equ	$07f8-$c0	; operating system check code

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
rdauxram	equ	$c003	; IIe read auxilliary RAM
wrmainram	equ	$c004	; IIe write main RAM
wrauxmem	equ	$c005	; IIe write auxilliary RAM
clr80vid	equ	$c00c	; IIe disable 80col
clraltchar	equ	$c00e	; IIe disable altchar
kbdstrb		equ	$c010	; clea rkeyboard strobe
rdramrd		equ	$c013	; IIe MSB reads 1 if aux RAM read enabled
rdramwrt	equ	$c014	; IIe MSB reads 1 if aux RAM write enabled

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
	stx	shg_0778
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

Lcn6e:	lda	shs_idx_part_data,Y
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
	lda	shs_os_check,Y
	eor	#$5a
	cmp	shs_os_code,Y
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

	section	common

; C800 shared ROM code

	org	$c800

	public	protocol_converter	; referenced from slot
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
	sty	shg_04f8
	sty	shg_0578
	sty	shg_05f8
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

	ldx	shg_0778

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
	skip2
ret_pc_err_bad_cmd:
	lda	#pc_err_bad_cmd
	skip2
ret_pc_err_bad_pcnt:
	lda	#pc_err_bad_pcnt
LC863:	sta	shg_04f8
Lc866:	ldx	#$00
Lc868:	pla
	sta	Z42,X
	inx
	cpx	#$0a
	bcc	Lc868
	ldy	shg_05f8
	ldx	shg_0578
	lda	shg_04f8
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
	sty	shg_0578
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
	skip2
Lc8e9:	ldy	#$04
	sty	shg_0578
	dey
Lc8ef:	lda	Dc9c6,Y
	cpy	#$02
	bne	Lc904
	ldx	mslot
	lda	shs_cur_part_size_high,X
	lsr
	sta	(Z45),Y
	dey
	lda	shs_cur_part_size_low,X
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
	sta	wrauxmem
Lc932:	lda	Z48
	sta	shg_05f8
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
	sta	shg_0578
Lc94e:	lda	Dbffb,X
	sta	(Z45),Y
	iny
	cpy	Z47
	bne	Lc94e
Lc958:	sta	wrmainram
	plp
	bpl	Lc961
	sta	wrauxmem
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
	sta	rdauxram
Lc994:	lda	Z48
	sta	shg_05f8
	beq	Lc9a9
Lc99b:	lda	(Z45),Y
	sta	Dbffb,X
	iny
	bne	Lc99b
	inc	Z46
	dec	Z48
	bne	Lc99b
Lc9a9:	lda	Z47
	sta	shg_0578
	beq	Lc9ba
Lc9b0:	lda	(Z45),Y
	sta	Dbffb,X
	iny
	cpy	Z47
	bne	Lc9b0
Lc9ba:	sta	rdmainram
	plp
	bpl	Lc9c3
	sta	rdauxram
Lc9c3:	jmp	Lc866

Dc9c6:	fcb	$f8,$00,$00,$00

	fcb	$07,"RAMCARD"
	fcb	"         "
	
	fcb	$00,$00,$00,$00

	public	Lc9df	; referenced by slot, partmgr
Lc9df:	ldy	mslot
	jsr	Sca8e
	lda	Dbffb,X
	cmp	#$ae
	bne	Lca31
	eor	Dbffb,X
	cmp	#$5a
	bne	Lca31
	lda	Dbffb,X
	sta	shs_idx_part_data,Y
	eor	#$5a
	cmp	Dbffb,X
	bne	Lca31
	lda	Dbffb,X
	sta	shs_card_block_count,Y
	lda	shs_idx_part_data,Y
	sta	Dbff8,X
	lda	Dbffb,X
	sta	shs_part_base_high,Y
	lda	Dbffb,X
	sta	shs_part_base_mid,Y
	lda	Dbffb,X
	sta	shs_cur_part_size_high,Y
	lda	Dbffb,X
	sta	shs_cur_part_size_low,Y
	lda	Dbffb,X
	sta	shs_os_code,Y
	lda	Dbffb,X
	sta	shs_os_check,Y
	rts

Lca31:	lda	shs_os_code,Y
	eor	shs_os_check,Y
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
	sta	shs_os_code,Y
	eor	#$5a
	sta	shs_os_check,Y
Lca6a:	jsr	Scb00
	ldy	mslot
	sta	shs_card_block_count,Y
Lca73:	lda	shs_card_block_count,Y
	asl
	sta	shs_cur_part_size_high,Y
	lda	#$00
	sta	shs_cur_part_size_low,Y
	sta	shs_part_base_mid,Y
	sta	shs_part_base_high,Y
	lda	#$01
	sta	shs_idx_part_data,Y
	rts

	public	Sca8b	; referenced from diag, pmgr
Sca8b:	ldx	shg_0778

	public	Sca8e	; referenced from diag, pmgr
Sca8e:	lda	#$00
	sta	Dbff8,X
	sta	Dbff9,X
	sta	Dbffa,X
	rts

	public	Sca9a	; referenced from slot
Sca9a:	jsr	Sca8e

Sca9d:	ldy	mslot
	lda	shs_card_block_count,Y
	cmp	#$09
	lda	Dbffa,X
	bcs	Lcaac
	and	#$0f
Lcaac:	pha
	bne	Lcacd
	lda	shs_idx_part_data,Y
	cmp	#$08
	bne	Lcacd
	lda	Dbff9,X
	cmp	#$02
	bcs	Lcacd
	ora	#$fe
	sta	Dbff9,X
	lda	shs_card_block_count,Y
	sbc	#$00
	rol
	sta	Dbffa,X
Lcacb:	pla
	rts

Lcacd:	lda	Dbff9,X
	cmp	shs_cur_part_size_low,Y
	pla
	pha
	sbc	shs_cur_part_size_high,Y
	bcs	Lcacb
	lda	Dbff9,X
	adc	shs_part_base_mid,Y
	sta	Dbff9,X
	pla
	adc	shs_part_base_high,Y
	sta	Dbffa,X
	rts

	public	Scaeb	; referenced from slot, pmgr
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

	public	Scb00	; referenced from diag
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

	public	Scb9b	; referenced from diag, pmgr
Scb9b:	ldx	#$04
	ldy	#$00
Lcb9f:	lda	#$00
Lcba1:	pha
	sec
	lda	Z3e
	sbc	Dcbde,X
	pha
	lda	Z3f
	sbc	Dcbe3,X
	bcc	Lcbba
	sta	Z3f
	pla
	sta	Z3e
	pla
	adc	#$00
	bne	Lcba1
Lcbba:	pla
	pla
	bne	Lcbc6
	dey
	bpl	Lcbc6
	txa
	beq	Lcbc6
	lda	#$10
Lcbc6:	iny
	eor	#$b0
	jsr	cout
	dex
	bpl	Lcb9f
	lda	#$cb
	jmp	cout

; cbd4:
	fcb	$08,$08,$20,$38,$50,$68,$80,$98
	fcb	$b0,$c8

Dcbde:	fcb	$04,$28,$90,$a0,$40

Dcbe3:	fcb	$00,$00,$01,$0f,$9c

	public	prodos	; reference from slot
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
	skip2
lcc0a:	ldy	#$27
Lcc0c:	tya
	sec
	rts

Lcc0f:	ldy	mslot
	jsr	Scfe4
	ldy	shg_0478
	ldx	shg_04f8
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

	public	Lcc6f	; referenced from slot
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

Sccfb:	ldy	mslot
	ldx	#$03
Lcd00:	lda	shs_cur_part_size_low,Y
	cmp	Dcd31,X
	lda	shs_cur_part_size_high,Y
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
	ldy	mslot
	lda	shs_part_base_mid,Y
	adc	Dcd3b,X
	sta	shs_part_base_mid,Y
	lda	shs_part_base_high,Y
	adc	Dcd39,X
	sta	shs_part_base_high,Y
	rts

Dcd2d:	fcb	$02,$04,$06,$0c
Dcd31:	fcb	$30,$60,$40,$80
Dcd35:	fcb	$23,$32
Dcd37:	fcb	$10,$20
Dcd39:	fcb	$02,$06
Dcd3b:	fcb	$30,$40


Scd3d:	ldy	mslot
	jsr	Scfe4
	cmp	#$10
	bcs	Lcd4c
	lda	shg_0478
	beq	Lcd55
Lcd4c:	lda	shs_os_check,Y
	eor	#$5a
	cmp	shs_os_code,Y
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
	sta	shs_os_code,Y
	jsr	Sccfb
	bmi	Lcd97
	bcc	Lcd91
	lda	shs_part_base_mid,Y
	pha
	lda	shs_part_base_high,Y
	pha
	stx	shg_04f8
	jsr	Scd16
	ldy	Dcfe2,X
	jsr	Scdfb
	ldx	shg_04f8
	ldy	mslot
	pla
	sta	shs_part_base_high,Y
	pla
	sta	shs_part_base_mid,Y
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
	cmp	shs_os_code,Y
	beq	Lcda4
Lcdb0:	sec
	rts

Lcdb2:	jsr	Scdec
	bne	Lcdb0
	sta	shs_os_code,Y
	ldy	#$00
	asl
	bne	Lcdc1
	ldy	#$87
Lcdc1:	jsr	Scdfb
	jsr	Sca8e
	ldy	mslot
	lda	shs_idx_part_data,Y
	cmp	#$01
	beq	Lcde2
	clc
	adc	#$04
	sta	Dbff8,X
	lda	shs_os_code,Y
	sta	Dbffb,X
	eor	#$5a
	sta	Dbffb,X
Lcde2:	lda	shs_os_code,Y
	eor	#$5a
	sta	shs_os_check,Y
	clc
	rts

	public	Scdec	; referenced from slot
Scdec:	lda	proflag
	beq	Lcdf3
	cmp	#$4c
Lcdf3:	rts

Lcdf4:	ldx	shg_0778
	jsr	Sce67
	iny

	public	Scdfb	; referenced from pmgr
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
	lda	shg_04f8
	cmp	#$01
	lda	shg_0478
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
	skip2
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

Lcec5:	lda	shg_04f8
	sta	Dbffb,X
	lda	shg_0478
Lcece:	sta	Dbffb,X
	rts

Lced2:	lda	mslot
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
	ldy	shg_0478
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
Lcf08:	lda	shg_04f8
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

Scfe4:	lda	shs_cur_part_size_high,Y
	lsr
	sta	shg_0478
	lda	shs_cur_part_size_low,Y
	ror
	sta	shg_04f8
	rts

	fillto	$d000,$ff

	endsection	common


; bank 1 common space ($c800-$cfff)
	org	$d800

b1_diag	equ	*-$1000

	section	diag
	phase	$0a00

Z24	equ	$24
Z28	equ	$28
Z29	equ	$29
	
diag:	stx	D0d2b
	jsr	home
	ldy	#d_msg_idx_banner
	jsr	d_msgout
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
	ldy	#d_msg_idx_bytes
	jsr	d_msgout
	jsr	Sca8b
	sta	Dbffb,X
	lda	#$04
	sta	Dbff9,X
	lda	#$ff
	sta	Dbffb,X
	ldy	mslot
	sta	shs_os_code,Y
L0a3d:	lda	#$00
	sta	Z24
	ldy	#d_msg_idx_pass
	jsr	d_msgout
	lda	D0d2f
	jsr	prbyte
	ldy	#d_msg_idx_testing
	jsr	d_msgout
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
	ldy	#d_msg_idx_card_failure
	jsr	d_msgout
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

L0a9a:	ldy	#d_msg_idx_address
	jsr	d_msgout
	ldx	shg_0778
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

S0ae8:	lda	D0af4+1,Y
	pha
	lda	D0af4,Y
	pha
	ldx	D0d2b
	rts

D0af4:	fdb	L0be4-1
	fdb	L0b81-1
	fdb	L0b1e-1
	fdb	L0afc-1


l0afc:	ldy	#$00
L0afe:	tya
	jsr	S0d0a
	eor	Dbffa,X
	and	#$0f
	bne	L0b1c
	tya
	cmp	Dbff9,X
	bne	L0b1c
	cmp	Dbff8,X
	bne	L0b1c
	jsr	S0ae4
	iny
	bne	L0afe
	clc
	rts

L0b1c:	sec
	rts

l0b1e:	lda	#$00
	ldy	#$f0
	jsr	S0d0a
L0b25:	lda	D0d28
	jsr	S0b6b
	sta	D0d28
	lda	D0d27
	jsr	S0b6b
	sta	D0d27
	jsr	S0d13
	inc	D0d28
	bne	L0b4b
	inc	D0d27
	bne	L0b4b
	inc	D0d26
	bne	L0b4b
	clc
	rts

L0b4b:	jsr	S0ae4
	lda	Dbffb,X
	lda	Dbffa,X
	eor	D0d26
	bne	L0b69
	lda	Dbff9,X
	cmp	D0d27
	bne	L0b69
	lda	Dbff8,X
	cmp	D0d28
	beq	L0b25
L0b69:	sec
	rts

S0b6b:	cmp	#$10
	bcc	L0b75
	cmp	#$70
	bcs	L0b76
	lda	#$70
L0b75:	rts

L0b76:	cmp	#$90
	bcc	L0b75
	cmp	#$f0
	bcs	L0b75
	lda	#$f0
	rts

L0b81:	lda	D0d2a
	beq	L0b94
	lda	#$00
	sta	D0d30
	jsr	S0b96
	jsr	S0b96
	jsr	S0b96
L0b94:	clc
	rts

S0b96:	jsr	Sca8e
	tay
L0b9a:	tya
	sta	Dbffb,X
	jsr	S0bbe
	bne	L0b9a
	jsr	S0ae4
	tya
	jsr	S0d0a
L0baa:	tya
	cmp	Dbffb,X
	bne	L0bba
	jsr	S0bbe
	bne	L0baa
	sec
	ror	D0d30
	rts

L0bba:	pla
	pla
	sec
	rts

S0bbe:	bit	D0d30
	bpl	L0bdb
	inc	D0d26
	inc	Dbffa,X
	lda	D0d26
	lsr
	cmp	D0d2a
	bcc	L0bdb
	ldy	#$00
	rts

	fcb	$ee,$27,$0d,$fe,$f9,$bf

L0bdb:	inc	D0d28
	iny
	rts

D0be0:	fcb	$00,$55,$aa,$ff

L0be4:	lda	D0d2a
	beq	L0bfc
	ldy	#$03
L0beb:	lda	D0be0,Y
	sta	D0d2c
	jsr	S0bfe
	jsr	S0c2e
	bcs	L0bfd
	dey
	bpl	L0beb
L0bfc:	clc
L0bfd:	rts

S0bfe:	jsr	S0c6d
L0c01:	jsr	S0c0a
	dec	D0d2d
	bne	L0c01
	rts

S0c0a:	tya
	pha
	lda	D0d2c
	ldy	#$00
	sty	D0d2e
L0c14:	sta	Dbffb,X
	sta	Dbffb,X
	sta	Dbffb,X
	sta	Dbffb,X
	iny
	bne	L0c14
	jsr	S0ae4
	inc	D0d2e
	bne	L0c14
	pla
	tay
	rts

S0c2e:	jsr	S0c6d
L0c31:	jsr	S0c3c
	bcs	L0c3b
	dec	D0d2d
	bne	L0c31
L0c3b:	rts

S0c3c:	tya
	pha
	lda	D0d2c
	ldy	#$00
	sty	D0d2e
L0c46:	cmp	Dbffb,X
	bne	L0c69
	cmp	Dbffb,X
	bne	L0c69
	cmp	Dbffb,X
	bne	L0c69
	cmp	Dbffb,X
	bne	L0c69
	iny
	bne	L0c46
	jsr	S0ae4
	inc	D0d2e
	bne	L0c46
	pla
	tay
	clc
	rts

L0c69:	tay
	pla
	sec
	rts

S0c6d:	lda	D0d2a
	lsr
	sta	D0d2d
	jmp	Sca8e


; d_msgout decompresses and outputs a message
; Y indexes d_msgtab, carry alternates 0 = high nibble, 1 = low nibble

L0c77:	php
	jsr	cout
	plp
	skip1
d_msgout:
	clc		; start with high nibble

	jsr	d_getnib		; get nibble
	lda	d_msg_dec_tab_1,X			; index first table
	bne	L0c77			; if table content non-zero, it's a char

	jsr	d_getnib		; get nibble
	lda	d_msg_dec_tab_2,X	; index second table
	bne	L0c77			; if table content non-zero, it's a char

	rts		; if both table entries were zero, end of message


; get one nibble
d_getnib:
	lda	d_msgtab,Y	; get byte from message table
	bcs	L0c9b	; carry clear?
	lsr		; yes, get high nibble
	lsr
	lsr
	lsr
	tax
	sec		; set carry so next call will get low nibble
	rts

; carry was set, 
L0c9b:	iny		; advance pointer
	and	#$0f
	tax
	clc		; clear carry so next call will get high nibble
	rts

; nibble to character decode table 1
; note that this table includes the first byte of decode table 2
; table includes all but three of the letters ETAOINSHRDLC, the 13
; most common letters of the English language, in an arbitrary
; permutation that might be intended to confuse reverse-engineers.
d_msg_dec_tab_1:
	fcb	$00		; escabe to second table
	fcb	ch_cr+$80	; carriage return
	fcsm	"RAM.TIS GOLDE"

; mibble to character decode table 2
d_msg_dec_tab_2:
	fcsm	"N"
	fcb	$00	; end of message
	fcsm	"BCFHPYZMQUV14:"

; Strings used at assembly time to perform the encoding
; These MUST match the decoding tables defined above
; \xff is a placeholder for a zero entry in the table, because AS doesn't support
; null bytes in strings
; \x01 is a special value that expands to two spaces
tbl1	set	"\xff\x0dRAM.TIS GOLDEN"
tbl2	set	"N\xffBCFHPYZMQUV14:"

; compressed message table
d_msgtab:
d_msg_idx_banner	equ	*-d_msgtab
	encode	"APPLIED ENGINEERING RAMFACTOR TEST V1.4\x0d\x0dMEMORY SIZE: "

d_msg_idx_bytes	equ	*-d_msgtab
	encode	" BYTES\x0d\x0d"

d_msg_idx_pass	equ	*-d_msgtab
	encode	"PASS: "

d_msg_idx_testing	equ	*-d_msgtab
	encode	"    TESTING ..."

d_msg_idx_card_failure	equ	*-d_msgtab
	encode	"\x0d\x0dCARD FAILURE. ID: "

d_msg_idx_address	equ	*-d_msgtab
	encode	"   ADDRESS: "


S0d0a:	sta	D0d28
	sta	D0d27
	sty	D0d26

S0d13:	lda	D0d28
	sta	Dbff8,X
	lda	D0d27
	sta	Dbff9,X
	lda	D0d26
	sta	Dbffa,X
	rts

D0d26:	equ	*
D0d27:	equ	*+1
D0d28:	equ	*+2
D0d29:	equ	*+3
D0d2a:	equ	*+4
D0d2b:	equ	*+5
D0d2c:	equ	*+6
D0d2d:	equ	*+7
D0d2e:	equ	*+8
D0d2f:	equ	*+9
D0d30:	equ	*+10

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

D0800	equ	$0800
D0801	equ	$0801
D0802	equ	$0802
D0803	equ	$0803
D0804	equ	$0804
D0805	equ	$0805
D0808	equ	$0808
D0818	equ	$0818
D0819	equ	$0819
D081a	equ	$081a
D081b	equ	$081b

D0900	equ	$0900
D0901	equ	$0901
D0902	equ	$0902
D0903	equ	$0903
D0904	equ	$0904
D0905	equ	$0905
D0906	equ	$0906
D0907	equ	$0907
D0908	equ	$0908

Dbff8	equ	$bff8
Dbffb	equ	$bffb

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
L0a1b:	ldy	mslot
	lda	shs_os_code,Y
	eor	shs_os_check,Y
	cmp	#$5a
	bne	L0a3f
	jsr	home
	ldy	#p_msg_idx_warning_installing
	jsr	p_msgout
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
	ldy	mslot
	lda	shs_card_block_count,Y
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
L0a7a:	lsr	D0900
L0a7d:	lda	#$00
	sta	Z24
	sta	Z25
	ldy	#p_msg_idx_heading
	jsr	p_msgout
	lda	mslot
	eor	#$70
	jsr	cout
	jsr	crout
	jsr	crout
	ldy	#$00
L0a98:	jsr	S0c03
	iny
	cpy	#$09
	bcc	L0a98
	ldy	#p_msg_idx_help_1
	jsr	p_msgout
	ldy	#$59
	bit	D0900
	bpl	L0aae
	ldy	#p_msg_idx_help_2
L0aae:	jsr	p_msgout
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
	lda	D0af7+2,Y
	pha
	lda	D0af7+1,Y
	pha
	rts

L0af1:	jsr	bell12
	jmp	L0a7d


D0af7:	cmd_ent	'N',p_cmd_name
	cmd_ent	'C',p_cmd_clear		; Clear a partition
	cmd_ent	'S',p_cmd_size		; change Size of a partition
	cmd_ent ch_cr,p_cmd_boot	; boot partition
	cmd_ent	key_left,p_cmd_up
	cmd_ent	key_up,p_cmd_up
	cmd_ent key_down,p_cmd_down
	cmd_ent	key_right,p_cmd_down
	cmd_ent	ch_esc,p_cmd_exit	; quit
	cmd_ent	'R',p_cmd_reconfigure	; Reconfigure
	fcb	$00			; end of table


p_cmd_boot:
	bit	D0900
	bmi	L0b1e
	jmp	L0df6

L0b1e:	jsr	S0de8
	jmp	L0a7a

p_cmd_reconfigure:
	sec
	ror	D0900
	jmp	L0a7d

p_cmd_exit:
	bit	D0900
	bpl	L0b33
	jmp	L0a00

L0b33:	jsr	S0dd7
	jmp	L0e36

p_cmd_up:
	lda	D0901
	sec
	sbc	#$18
L0b3f:	cmp	#$e0
	bcs	L0b46
	sta	D0901
L0b46:	jmp	L0a7d

p_cmd_down:
	lda	D0901
	clc
	adc	#$18
	bcc	L0b3f

p_cmd_name:
	ldy	#p_msg_idx_new_name
	jsr	p_msgout
	ldx	#$10
	jsr	S0e4f
	bcs	L0b6e
	ldx	#$00
	ldy	D0901
L0b62:	lda	D0908,X
	sta	D0808,Y
	iny
	inx
	cpx	#$10
	bcc	L0b62
L0b6e:	jmp	L0a7d

p_cmd_size:
	jsr	S0e43
	beq	L0bea
	tya
	clc
	adc	#$18
	tay
	cpy	#$e0
	bcs	L0bea
	jsr	S0e46
	beq	L0bea
	ldy	#p_msg_idx_new_size
	jsr	p_msgout
	ldx	#$06
	jsr	S0e4f
	jsr	S0ea1
	asl	D0904
	rol	D0905
	asl	D0904
	rol	D0905
	clc
	ldy	D0901
	lda	D0803,Y
	adc	D081b,Y
	sta	D0906
	lda	D0802,Y
	adc	D081a,Y
	sta	D0907
	sec
	lda	D0906
	sbc	D0904
	pha
	lda	D0907
	sbc	D0905
	bcc	L0be6
	sta	D081a,Y
	pla
	sta	D081b,Y
	clc
	lda	D0904
	sta	D0803,Y
	adc	D0801,Y
	sta	D0819,Y
	lda	D0905
	sta	D0802,Y
	adc	D0800,Y
	sta	D0818,Y
	jmp	L0a7d

L0be6:	pla
	ldy	#$d5
	skip2
L0bea:	ldy	#$c9
	jsr	p_msgout
	jsr	rdkey
	jmp	L0a7d

p_cmd_clear:
	jsr	S0e43
	bne	L0c00
	ldx	D0901
	inc	D0805,X
L0c00:	jmp	L0a7d

S0c03:	tya
	pha
	jsr	S0c81
	pha
	ldx	#$ff
	cmp	D0901
	bne	L0c12
	ldx	#$3f
L0c12:	stx	Z32
	jsr	print_two_spaces
	tya
	clc
	adc	#$b1
	jsr	cout
	jsr	print_two_spaces
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
	jsr	print_two_spaces
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
	jsr	print_two_spaces
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
	jsr	p_msgout
	pla
	tay
D0c6f:	rts

	fcb	$4c,$00,$33,$cd

D0c74:	fcb	p_msg_idx_clear
	fcb	p_msg_idx_prodos
	fcb	p_msg_idx_pascal
	fcb	p_msg_idx_dos
	fcb	p_msg_idx_cpm


print_two_spaces:
	lda	#' '+$80
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

; Decompress and output a message
; Y indexes p_msgtab, carry alternates 0 = high nibble, 1 = low nibble
p_msgout:
	clc
L0c8f:	jsr	p_getnib	; get nibble
	lda	D0cbe,X		; index first table
	bne	L0ca0		; if table content non-zero, it's a char

	jsr	p_getnib	; it was zero, get next nibble
	lda	D0ccd,X		; index second table
	bne	L0ca0		; if table content non-zero, it's a char

	rts		; if both table entries were zero, end of message


; output a decoded character, except if MSB is zero, output two spaces
L0ca0:	php
	bmi	L0ca6
	jsr	print_two_spaces

L0ca6:	jsr	cout
	plp
	bne	L0c8f


; get one nibble
p_getnib:
	lda	p_msgtab,Y	; get byte from message table
	bcs	L0cb8	; carry clear?
	lsr		; yes, get high nibble
	lsr
	lsr
	lsr
	tax
	sec		; set carry so next call will get low nibble
	rts

; carry was set, getting low nibble
L0cb8:	iny		; advance pointer
	and	#$0f
	tax
	clc		; clear carry so next call will get high nibble
	rts

; nibble to character decode table 1
; note that this table includes the first byte of decode table 2
; table includes all but one of the letters ETAOINSHRDLC, the 13
; most common letters of the English language, in an arbitrary
; permutation that might be intended to confuse reverse-engineers.
D0cbe:	fcb	$00		; escape to second table
	fcb	ch_cr+$80	; carriage return
	fcb	$01		; two spaces
	fcsm	"GLITCH REASO"

; nibble to character decode table 2
D0ccd:	fcsm	"N"
	fcb	$00		; end of message
	fcsm	"FPUDM-19=BQWZY"

; Strings used at assembly time to perform the encoding
; These MUST match the decoding tables defined above
; \xff is a placeholder for a zero entry in the table, because AS doesn't support
; null bytes in strings
; \x01 is a special value that expands to two spaces
tbl1	set	"\xff\x0d\x01GLITCH REASON"
tbl2	set	"N\xffFPUDM-19=BQWZY"

; compressed message table
p_msgtab:
p_msg_idx_heading	equ	*-p_msgtab
	encode	"\x0dRAMFACTOR PARTITIONS\x01\x01\x01  SLOT = "

p_msg_idx_help_1	equ	*-p_msgtab
	encode	"\x0dUSE ARROWS OR 1-9 TO SELECT\x0d\x0d"

p_msg_idx_help_2	equ	*-p_msgtab
	encode	"N=NAME CHANGE\x01\x01 RET=INSTALL CHANGES\x0dS=SIZE CHANGE\x01\x01 ESC=FORGET CHANGES\x0dC=CLEAR PARTITION"

; XXX referenced from where?
p_msg_idx_help_3	equ	*-p_msgtab
	encode	"RET=BOOT THE PARTITION\x01 R=RECONFIGURE\x0dESC=QUIT"

p_msg_idx_new_name	equ	*-p_msgtab
	encode	"\x0d\x0dNEW NAME = "

p_msg_idx_new_size	equ	*-p_msgtab
	encode	"\x0d\x0dNEW SIZE = "
;	fcb	$11,$fb,$0d,$9d,$50,$eb,$90,$a9
;	fcb	$01
; "NEW SIZE = "

p_msg_idx_warning_installing	equ	*-p_msgtab
	encode	"\x0dWARNING- INSTALLING PARTITIONS DESTROYS\x0dTHE DIRECTORY- GO AHEAD"

p_msg_idx_cannot_boot	equ	*-p_msgtab
	encode	"\x0d\x0dCANNOT BOOT THAT PARTITION\x0dBOOT FROM SLOT = "

; XXX referenced from where?
p_msg_idx_cannot_change_size	equ	*-p_msgtab
	encode	"\x0d\x0dCANNOT CHANGE SIZE"

; XXX referenced from where?
p_msg_idx_size_too_large	equ	*-p_msgtab
	encode	"\x0d\x0dSIZE TOO LARGE"

p_msg_idx_clear	equ	*-p_msgtab
	encode	"CLEAR \x0d"

p_msg_idx_dos	equ	*-p_msgtab
	encode	"DOS   \x0d"

p_msg_idx_prodos	equ	*-p_msgtab
	encode	"PRODOS\x0d"

p_msg_idx_cpm	equ	*-p_msgtab
	encode	"CP-M  \x0d"

p_msg_idx_pascal	equ	*-p_msgtab
	encode	"PASCAL\x0d"


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
L0e09:	ldy	#p_msg_idx_cannot_boot
	jsr	p_msgout
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

S0e27:	ldx	shg_0778
	ldy	mslot
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
S0e46:	lda	D0804,Y
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
	cmp	#ch_cr+$80
	beq	L0e82
	cmp	#ch_esc+$80
	beq	L0e83
	cmp	#' '+$80
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

S0ea1:	lda	#$00
	tay
	tax
L0ea5:	sta	D0904
	stx	D0905
	lda	#$0a
	sta	D0902
	ldx	#$00
	lda	D0908,Y
	eor	#$b0
	cmp	#$0a
	bcc	L0ebc
	rts

L0ebc:	adc	D0904
	pha
	txa
	adc	D0905
	tax
	pla
	dec	D0902
	bne	L0ebc
	iny
	bne	L0ea5

	dephase
	endsection partmgr

b1_partmgr_end	equ	*-$1000

	fillto	$dfff,$ff
