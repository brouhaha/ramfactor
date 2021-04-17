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

; skip macros - NOTE: these alter the Z, N, and V flags

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


char_bs		equ	$08
char_lf		equ	$0a
char_vt		equ	$0b
char_cr		equ	$0d
char_nak	equ	$15
char_esc		equ	$1b

key_left	equ	char_bs
key_right	equ	char_nak
key_up		equ	char_vt
key_down	equ	char_lf



; zero page locations
L00	equ	$00

; Apple II monitor zero page locations
ch	equ	$24
CV	equ	$25
invflg	equ	$32	; $
kswl	equ	$38

; ProDOS and disk driver zero page locations
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

rwtsiob	equ	Z48
rwtsbuf	equ	Z3e

rwts_iob_idx_slot	equ	$01
rwts_iob_idx_drive	equ	$02
rwts_iob_idx_volume	equ	$03
rwts_iob_idx_track	equ	$04
rwts_iob_idx_sector	equ	$05
rwts_iob_idx_buffer	equ	$08
rwts_iob_idx_command	equ	$0c
rwts_iob_idx_result	equ	$0d
rwts_iob_idx_prev_vol	equ	$0e
rwts_iob_idx_prev_slot	equ	$0f
rwts_iob_idx_prev_drive	equ	$10

rwts_err_none		equ	$00
rwts_err_init		equ	$08
rwts_err_write_prot	equ	$10
rwts_err_vol_mismatch	equ	$20
rwts_err_drive		equ	$40
rwts_err_read		equ	$80	; Obsolete according to Beneath Apple DOS


D010b	equ	$010b
D010c	equ	$010c
D01a9	equ	$01a9

klinbuf	equ	$0200

D0400	equ	$0400


; card HW regs typically would be addressed using a
; base address of $c00r or $c084, with an index
; register value of $n0, where n is the slot number,
; or the slot number + $80.  However, with the 6502
; this would result in false reads of
; Apple IIe/IIc/IIgs soft switches or Language Card
; soft switches (integral to the IIe/IIc/IIgs).
;
; Instead, we use base addresses at the end of Apple DRAM
; the $88 offset is used so that the "false read"
; of an indexed address will be to DRAM, and not
; screw up the Language Card switches.
;
; For example for a card in slot 4, with a register
; at $coc3:
;
;    base      index      register    false read
;    -----     -----      --------    ----------
;    $c003     $c0        $c0c3       $c003 RDCARDRAM
;    $c083     $40        $c0c3       $c083 LCBANK2
;    $bffb     $c8        $c0c3       $bffb (no problem)

hw_reg_offset	equ	$88


; screen holes, global (not indexed)
; Since these are not slot-specific, they are only suitable for scratch
; use.
; BEWARE: Even scratch use is questionable if interrupts are in use,
; because an interrupt hander might use these.
shg_0478		equ	$0478
shg_04f8		equ	$04f8

shg_0578		equ	$0578
shg_05f8		equ	$05f8

mslot1088		equ	$0778		; stores (slot*$10)+hw_reg_offset


; mslot appears to be the only global screen hole for which Apple
; has defined a function.
mslot			equ	$07f8		; stores $Cn, where n is the slot number

; screen holes, indexed by $Cn, where n is slot number (1 through 7)
shs_card_block_count	equ	$0478-$c0	; # blocks of whole card, divided by 256
shs_idx_part_data	equ	$04f8-$c0	; index to partition data
shs_part_base_high	equ	$0578-$c0	; partition base address, high bbyte
shs_part_base_mid	equ	$05f8-$c0	; partition base address, mid byte
shs_cur_part_size_high	equ	$0678-$c0	; current partition size, pages, high byte
shs_cur_part_size_low	equ	$06f8-$c0	; current partition size, pages, low byte
shs_os_code		equ	$0778-$c0	; operating system code
shs_os_check		equ	$07f8-$c0	; operating system check code


bootstrap_load_addr	equ	$0800
bootstrap_entry_addr	equ	bootstrap_load_addr+1

ram_application_base	equ	$0a00

rwts_patch		equ	$bd12	; RWTS patch loc, three bytes

proflag		equ	$bf00	; used to detect OS
os_pascal	equ	$00
os_prodos	equ	$4c
os_dos		equ	$33
os_cpm		equ	$cd


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


; hardware registers
hw_reg_addr_low		equ	$c080	; low byte of hardware RAM address, add slot*$10
hw_reg_addr_mid		equ	$c081	; low byte of hardware RAM address, add slot*$10
hw_reg_addr_high	equ	$c082	; low byte of hardware RAM address, add slot*$10
hw_reg_data		equ	$c083	; low byte of hardware RAM address, add slot*$10
hw_reg_rom_bank		equ	$c08f	; bank select, add slot*$10


Dcfff	equ	$cfff	; turn off C800 shared ROM space

sloop		equ	$faba
init		equ	$fb2f
bell12		equ	$fbe2
clreop		equ	$fc42
home		equ	$fc58
rdkey		equ	$fd0c
crout		equ	$fd8e
prbyte		equ	$fdda
cout		equ	$fded
setkbd		equ	$fe89
setvid		equ	$fe93
resetvec	equ	$fffc


err_bad_cmd		equ	$01
err_bad_pcnt		equ	$04
err_bus_err		equ	$06
err_bad_unit_num	equ	$11
err_bad_ctl		equ	$21
err_bad_ctl_param	equ	$22
err_io_error		equ	$27
err_no_drive		equ	$28
err_no_write		equ	$2b	; write-protected
err_bad_block_num	equ	$2d
err_offline		equ	$2f
; Protocol Converter error codes $30..$3f are device-specific errors
; Protocol Converter error codes $50..$7f are device-specific non-fatal errors


check_xor_val equ	$5a	; value XORed with various things to generate a check byte


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

; cn0a - RamFactor diagnostic (same entry point as Apple's Slinky memory test)
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
	asl	hw_reg_rom_bank+(slotnum*$10)	; set bank 0
	ldx	#(slotnum*$10)+hw_reg_offset
	sty	mslot
	stx	mslot1088
	jmp	Lc9df

boot4:	lda	#$c0+slotnum	; did we get here from IN# or equiv?
	cmp	kswl+1
	php
	jsr	Scn16
	plp
	bne	Lcn60

; we got here from IN# or equivalent
	jsr	Scdec		; are we running Pascal or ProDOS?
	beq	Lcn60		;   yes

; not Pascal or ProDOS, so patch RWTS
	lda	#$20
	sta	rwts_patch
	lda	#slot_rwts&$ff
	sta	rwts_patch+1
	sty	rwts_patch+2

	ldx	#$00
	lda	#char_cr+$80	; return with CR, and also store in input buffer
	sta	klinbuf
	rts

; IN# or equivalent, but running Pascal or ProDOS
Lcn60:	lda	L00
	bne	Lcn6e
	cpy	L00+1
	bne	Lcn6e
	jsr	Scn7b
	jmp	sloop

Lcn6e:	lda	shs_idx_part_data,y
	cmp	#$01
	bne	Lcn9f
	jsr	Scn7b
	clc
	bcc	Lcn9f

Scn7b:	jsr	Sca9a
	jsr	Scaeb
	ldy	#$c0+slotnum	; high byte of slot ROM base c100
	lda	bootstrap_entry_addr
	beq	Lcn9a
	lda	shs_os_check,y
	eor	#check_xor_val
	cmp	shs_os_code,y
	bne	Lcn9a
	sta	proflag
	ldx	#(slotnum*$10)
	jmp	bootstrap_entry_addr

Lcn9a:	rts

; diagnostic
Lcn9b:	ldy	#$03
	bne	Lcna1

; partition managere
Lcn9f:	ldy	#$05

; copy four bytes from table to Z42..Z45
; gets download routine start address in Z42, end+1 in Z44
Lcna1:	ldx	#$03
Lcna3:	lda	Dcnd5,y
	sta	Z42,x
	dey
	dex
	bpl	Lcna3

	php
	sei
	stx	hw_reg_rom_bank+(slotnum*$10)	; set bank 1
	ldx	#(slotnum*$10)+hw_reg_offset

	ldy	#$00	; set Z3e to point to RAM where diag/partmgr load
	sty	Z3e
	lda	#$0a
	sta	Z3f

Lcnbb:	lda	(Z42),y	; copy down to RAM
	sta	(Z3e),y
	iny
	bne	Lcnc6
	inc	Z3f
	inc	Z43
Lcnc6:	cpy	Z44
	lda	Z43
	sbc	Z45
	bcc	Lcnbb

	asl	hw_reg_rom_bank+(slotnum*$10)	; set bank 0
	plp
	jmp	ram_application_base

; table of bank 1 regions
Dcnd5:	fdb	b1_diag		; start of diag code
	fdb	b1_partmgr	; end of diag, start of partition manager
	fdb	b1_partmgr_end	; end of partition manager


slot_prodos:
	clv
	bvc	slot_prodos_x

slot_protocol_converter
	jsr	romsel
	jmp	pconv

slot_prodos_x:
	jsr	romsel
	jmp	prodos
	

; on entry, RWTS code has stored IOB pointer into ?, and has loaded
; the requested slot*$10 into A
slot_rwts:
	cmp	#(slotnum*$10)	; A = our slot * 10?
	beq	Lcnf2		;   yes
	tax			; no, execute the instructions the
	ldy	#$0f		; patch overwrote
	rts			; and return

Lcnf2:	pla			; discard the patch return address
	pla
	jsr	romsel
	jmp	rwts


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

	public	pconv	; referenced from slot
pconv:
	jsr	Scd99

	ldx	#$09	; save ten bytes from Z42 on stack
Lc805:	lda	Z42,x
	pha
	dex
	bpl	Lc805
	tsx

	lda	D010c,x
	sta	Z46
	lda	D010b,x
	sta	Z45

	ldy	#$03
Lc818:	lda	(Z45),y
	sta	Z41,y
	inc	D010b,x
	bne	Lc825
	inc	D010c,x
Lc825:	dey
	bne	Lc818
	sty	shg_04f8
	sty	shg_0578
	sty	shg_05f8
	tax
	cmp	#$0a
	bcs	ret_err_bad_cmd
	lda	Dc887,x
	cmp	(Z43),y
	bne	ret_err_bad_pcnt

	ldy	#$08
Lc83f:	lda	(Z43),y
	sta	Z43,y
	dey
	bne	Lc83f

	lsr		; first param is unit num, only allow 0 and 1
	bne	ret_err_bad_unit_num

	ldx	mslot1088

	ldy	Z42	; protocol converter command
	lda	#$c8
	pha
	lda	Dc87d,y
	pha
	lsr	Z44
	lda	Z47
	rts

ret_err_bad_unit_num:
	lda	#$11
	skip2
ret_err_bad_cmd:
	lda	#err_bad_cmd
	skip2
ret_err_bad_pcnt:
	lda	#err_bad_pcnt
LC863:	sta	shg_04f8
Lc866:	ldx	#$00
Lc868:	pla
	sta	Z42,x
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
	jmp	ret_err_bad_cmd

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
	sta	(Z45),y
	bne	Lc8d4
	lda	#$01
	sta	(Z45),y
	jmp	Lc866

Lc8e0:	beq	Lc8e9
	ldy	#$19
	cmp	#$03
	bne	Lc89f
	skip2
Lc8e9:	ldy	#$04
	sty	shg_0578
	dey
Lc8ef:	lda	Dc9c6,y
	cpy	#$02
	bne	Lc904
	ldx	mslot
	lda	shs_cur_part_size_high,x
	lsr
	sta	(Z45),y
	dey
	lda	shs_cur_part_size_low,x
	ror
Lc904:	sta	(Z45),y
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
Lc939:	lda	hw_reg_data-hw_reg_offset,x
	sta	(Z45),y
	iny
	bne	Lc939
	inc	Z46
	dec	Z48
	bne	Lc939
Lc947:	lda	Z47
	beq	Lc958
	sta	shg_0578
Lc94e:	lda	hw_reg_data-hw_reg_offset,x
	sta	(Z45),y
	iny
	cpy	Z47
	bne	Lc94e
Lc958:	sta	wrmainram
	plp
	bpl	Lc961
	sta	wrauxmem
Lc961:	jmp	Lc866

Sc964:	lda	Z49
	sta	hw_reg_addr_low-hw_reg_offset,x
	lda	Z4a
	sta	hw_reg_addr_mid-hw_reg_offset,x
	lda	Z4b
	and	#$7f
	sta	hw_reg_addr_high-hw_reg_offset,x
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
Lc99b:	lda	(Z45),y
	sta	hw_reg_data-hw_reg_offset,x
	iny
	bne	Lc99b
	inc	Z46
	dec	Z48
	bne	Lc99b
Lc9a9:	lda	Z47
	sta	shg_0578
	beq	Lc9ba
Lc9b0:	lda	(Z45),y
	sta	hw_reg_data-hw_reg_offset,x
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
	jsr	clear_ram_ptr
	lda	hw_reg_data-hw_reg_offset,x
	cmp	#$ae
	bne	Lca31
	eor	hw_reg_data-hw_reg_offset,x
	cmp	#check_xor_val
	bne	Lca31
	lda	hw_reg_data-hw_reg_offset,x
	sta	shs_idx_part_data,y
	eor	#check_xor_val
	cmp	hw_reg_data-hw_reg_offset,x
	bne	Lca31
	lda	hw_reg_data-hw_reg_offset,x
	sta	shs_card_block_count,y
	lda	shs_idx_part_data,y
	sta	hw_reg_addr_low-hw_reg_offset,x
	lda	hw_reg_data-hw_reg_offset,x
	sta	shs_part_base_high,y
	lda	hw_reg_data-hw_reg_offset,x
	sta	shs_part_base_mid,y
	lda	hw_reg_data-hw_reg_offset,x
	sta	shs_cur_part_size_high,y
	lda	hw_reg_data-hw_reg_offset,x
	sta	shs_cur_part_size_low,y
	lda	hw_reg_data-hw_reg_offset,x
	sta	shs_os_code,y
	lda	hw_reg_data-hw_reg_offset,x
	sta	shs_os_check,y
	rts

Lca31:	lda	shs_os_code,y		; is the OS code valid?
	eor	shs_os_check,y
	cmp	#check_xor_val
	beq	Lca73			; yes, don't have to probe

	jsr	clear_ram_ptr
	lda	#$04
	sta	hw_reg_addr_mid-hw_reg_offset,x

	lda	hw_reg_data-hw_reg_offset,x
	eor	hw_reg_data-hw_reg_offset,x
	eor	hw_reg_data-hw_reg_offset,x
	eor	hw_reg_data-hw_reg_offset,x

	ldx	#os_prodos
	cmp	#$03
	beq	Lca61
	ldx	#os_pascal
	cmp	#$06
	beq	Lca61
	ldx	#os_dos
	cmp	#$be
	bne	Lca6a
Lca61:	txa				; store OS code
	sta	shs_os_code,y
	eor	#check_xor_val
	sta	shs_os_check,y

Lca6a:	jsr	Scb00
	ldy	mslot
	sta	shs_card_block_count,y

Lca73:	lda	shs_card_block_count,y
	asl
	sta	shs_cur_part_size_high,y
	lda	#$00
	sta	shs_cur_part_size_low,y
	sta	shs_part_base_mid,y
	sta	shs_part_base_high,y
	lda	#$01
	sta	shs_idx_part_data,y
	rts

	public	Sca8b	; referenced from diag, pmgr
Sca8b:	ldx	mslot1088

	public	clear_ram_ptr	; referenced from diag, pmgr
; apparently X reg must contain (slot*$10)+$88
clear_ram_ptr:
	lda	#$00
	sta	hw_reg_addr_low-hw_reg_offset,x
	sta	hw_reg_addr_mid-hw_reg_offset,x
	sta	hw_reg_addr_high-hw_reg_offset,x
	rts

	public	Sca9a	; referenced from slot
Sca9a:	jsr	clear_ram_ptr

Sca9d:	ldy	mslot
	lda	shs_card_block_count,y
	cmp	#$09
	lda	hw_reg_addr_high-hw_reg_offset,x
	bcs	Lcaac
	and	#$0f
Lcaac:	pha
	bne	Lcacd
	lda	shs_idx_part_data,y
	cmp	#$08
	bne	Lcacd
	lda	hw_reg_addr_mid-hw_reg_offset,x
	cmp	#$02
	bcs	Lcacd
	ora	#$fe
	sta	hw_reg_addr_mid-hw_reg_offset,x
	lda	shs_card_block_count,y
	sbc	#$00
	rol
	sta	hw_reg_addr_high-hw_reg_offset,x
Lcacb:	pla
	rts

Lcacd:	lda	hw_reg_addr_mid-hw_reg_offset,x
	cmp	shs_cur_part_size_low,y
	pla
	pha
	sbc	shs_cur_part_size_high,y
	bcs	Lcacb
	lda	hw_reg_addr_mid-hw_reg_offset,x
	adc	shs_part_base_mid,y
	sta	hw_reg_addr_mid-hw_reg_offset,x
	pla
	adc	shs_part_base_high,y
	sta	hw_reg_addr_high-hw_reg_offset,x
	rts

	public	Scaeb	; referenced from slot, pmgr
Scaeb:	ldy	#$00
Lcaed:	lda	hw_reg_data-hw_reg_offset,x
	sta	bootstrap_load_addr,y
	iny
	bne	Lcaed
Lcaf6:	lda	hw_reg_data-hw_reg_offset,x
	sta	bootstrap_load_addr+$0100,y
	iny
	bne	Lcaf6
	rts

	public	Scb00	; referenced from diag
Scb00:	jsr	Sca8b
	tay
	cmp	hw_reg_addr_high-hw_reg_offset,x
	bne	Lcb0b
	ldy	#$02
Lcb0b:	ora	Dcb95,y
Lcb0e:	sta	Z3f
	sta	Z3e
	sta	hw_reg_addr_high-hw_reg_offset,x
	lda	hw_reg_data-hw_reg_offset,x
	dec	hw_reg_addr_low-hw_reg_offset,x
	pha
	lda	Z3f
	sta	hw_reg_data-hw_reg_offset,x
	dec	hw_reg_addr_low-hw_reg_offset,x
	and	Dcb95,y
	beq	Lcb3d
	lda	Z3f
	sec
	sbc	Dcb98,y
	jmp	Lcb0e

Lcb32:	clc
	lda	Z3f
	adc	Dcb98,y
	sta	Z3f
	sta	hw_reg_addr_high-hw_reg_offset,x
Lcb3d:	lda	hw_reg_data-hw_reg_offset,x
	cmp	Z3f
	bne	Lcb83
	eor	#$ff
	dec	hw_reg_addr_low-hw_reg_offset,x
	sta	hw_reg_data-hw_reg_offset,x
	dec	hw_reg_addr_low-hw_reg_offset,x
	cmp	hw_reg_data-hw_reg_offset,x
	bne	Lcb83
	dec	hw_reg_addr_low-hw_reg_offset,x
	lda	Z3f
	and	Dcb95,y
	cmp	Dcb95,y
	bne	Lcb32
Lcb61:	lda	Z3e
	sta	hw_reg_addr_high-hw_reg_offset,x
	pla
	sta	hw_reg_data-hw_reg_offset,x
	dec	hw_reg_addr_low-hw_reg_offset,x
	clc
	lda	Z3e
	adc	Dcb98,y
	sta	Z3e
	and	Dcb95,y
	bne	Lcb61
	lda	Z3f
	dey
	bpl	Lcb0b
	lsr
	adc	#$02
	rts

Lcb83:	dec	hw_reg_addr_low-hw_reg_offset,x
	sec
	lda	Z3f
	beq	Lcb61
	sbc	Dcb98,y
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
	sbc	Dcbde,x
	pha
	lda	Z3f
	sbc	Dcbe3,x
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
	beq	prodos_status
	dey
	beq	prodos_read
	dey
	beq	prodos_write
	dey
	beq	prodos_format
	ldy	#err_bad_cmd
	skip2
lcc0a:	ldy	#err_io_error
Lcc0c:	tya
	sec
	rts

prodos_status:
	ldy	mslot
	jsr	copy_s2g_part_size
	ldy	shg_0478
	ldx	shg_04f8
prodos_format
:	lda	#$00
	clc
	rts

prodos_read:
	jsr	Scc58
	bcs	Lcc0a
	ldy	#$00
Lcc26:	lda	hw_reg_data-hw_reg_offset,x
	sta	(Z44),y
	iny
	bne	Lcc26
	inc	Z45
Lcc30:	lda	hw_reg_data-hw_reg_offset,x
	sta	(Z44),y
	iny
	bne	Lcc30
Lcc38:	dec	Z45
	tya
	clc
	rts

prodos_write:
	jsr	Scc58
	bcs	Lcc0a
	ldy	#$00
Lcc44:	lda	(Z44),y
	sta	hw_reg_data-hw_reg_offset,x
	iny
	bne	Lcc44
	inc	Z45
Lcc4e:	lda	(Z44),y
	sta	hw_reg_data-hw_reg_offset,x
	iny
	bne	Lcc4e
	beq	Lcc38

Scc58:	lda	#$00
	sta	hw_reg_addr_low-hw_reg_offset,x
	lda	Z46
	asl
	sta	hw_reg_addr_mid-hw_reg_offset,x
	lda	Z47
	rol
	bcs	Lcc6e
	sta	hw_reg_addr_high-hw_reg_offset,x
	jsr	Sca9d
Lcc6e:	rts


; The RWTS IOB pointer is in Z48
	public	rwts	; referenced from slot
rwts:	jsr	Scd56
	ldy	#$10
	bcs	Lcce0

	jsr	dos_check_partition_size

	ldy	#rwts_iob_idx_drive
	lda	(rwtsiob),y
	sta	Z3e
	eor	#$01
	beq	Lcc88
	bcc	rwts_bad_cmd

	jsr	Scd16

Lcc88:	ldy	#rwts_iob_idx_prev_vol
	lda	#$fe
	sta	(rwtsiob),y

	ldy	#rwts_iob_idx_track	; check track number against maximum
	sty	Z3f
	lda	(rwtsiob),y
	cmp	dos_max_track,x
	bcs	rwts_bad_cmd

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
	iny				; check sector number
	lda	(rwtsiob),y
	cmp	dos_max_sector,x
	bcs	rwts_bad_cmd

	ora	Z3f
	pha
	jsr	Sca8b
	pla
	sta	hw_reg_addr_mid-hw_reg_offset,x
	pla
	sta	hw_reg_addr_high-hw_reg_offset,x
	jsr	Sca9d
	bcs	rwts_bad_cmd

	ldy	#rwts_iob_idx_buffer	; copy buffer address to ZP
	lda	(rwtsiob),y
	sta	Z3e
	iny
	lda	(rwtsiob),y
	sta	Z3f

	ldy	#rwts_iob_idx_command
	lda	(rwtsiob),y
	tay
	beq	rwts_seek
	dey
	beq	rwts_read
	dey
	beq	rwts_write

rwts_bad_cmd:
	sec
	ldy	#rwts_err_read
Lcce0:	bne	rwts_done

rwts_write:
	lda	(Z3e),y
	sta	hw_reg_data-hw_reg_offset,x
	iny
	bne	rwts_write
	beq	rwts_seek

rwts_read:
	lda	hw_reg_data-hw_reg_offset,x
	sta	(Z3e),y
	iny
	bne	rwts_read
rwts_seek:
	clc

rwts_done:
	tya
	ldy	#rwts_iob_idx_result
	sta	(rwtsiob),y
	rts


; check DOS parition size
; returns with 0 in X for 140 KiB drive(s), 1 in X for 400 KiB drive(s)
dos_check_partition_size:
	ldy	mslot
	ldx	#$03
Lcd00:	lda	shs_cur_part_size_low,y
	cmp	valid_dos_part_size_low,x
	lda	shs_cur_part_size_high,y
	sbc	valid_dos_part_size_high,x
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
	lda	shs_part_base_mid,y
	adc	Dcd3b,x
	sta	shs_part_base_mid,y
	lda	shs_part_base_high,y
	adc	Dcd39,x
	sta	shs_part_base_high,y
	rts


; valid DOS parition sizes
valid_dos_part_size_high:
	fcb	(140*4)>>8	; one 140 KiB
	fcb	(280*4)>>8	; two 140 KiB
	fcb	(400*4)>>8	; one 400 KiB
	fcb	(800*4)>>8	; two 400 KiB

valid_dos_part_size_low:
	fcb	(140*4)&$ff	; one 140 KiB
	fcb	(280*4)&$ff	; two 140 KiB
	fcb	(400*4)&$ff	; one 400 KiB
	fcb	(800*4)&$ff	; two 400 KiB

dos_max_track:
	fcb	35		; for 140 KiB drives
	fcb	50		; for 400 KiB drives

dos_max_sector:
	fcb	16		; for 140 KiB drives
	fcb	32		; for 400 KiB drives

Dcd39:	fcb	$02,$06
Dcd3b:	fcb	$30,$40


Scd3d:
; is partition size at least 4KB?
; return:
;   carry clear if paritition too small
;   carry set, zero clear if size OK but unknwon OS
;   carry set, zero set if size and OS both OK, A = OS
	ldy	mslot
	jsr	copy_s2g_part_size
	cmp	#$10
	bcs	Lcd4c
	lda	shg_0478
	beq	Lcd55

Lcd4c:	lda	shs_os_check,y
	eor	#check_xor_val
	cmp	shs_os_code,y
	sec
Lcd55:	rts


Scd56:	jsr	Scd3d		; check parition size and OS
	bcc	Lcd97		;   parititon too small
	bne	Lcd63		;   parition size OK, but OS unknown
	cmp	#os_dos
	bne	Lcd97		;   not a DOS partition
	clc
	rts

Lcd63:	lda	#os_dos		; set OS to DOS (but don't yet set OS check)
	sta	shs_os_code,y
	jsr	dos_check_partition_size
	bmi	Lcd97
	bcc	Lcd91
	lda	shs_part_base_mid,y
	pha
	lda	shs_part_base_high,y
	pha
	stx	shg_04f8
	jsr	Scd16
	ldy	Dcfe2,x
	jsr	Scdfb
	ldx	shg_04f8
	ldy	mslot
	pla
	sta	shs_part_base_high,y
	pla
	sta	shs_part_base_mid,y
Lcd91:	ldy	Dcfe2,x
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
	cmp	shs_os_code,y
	beq	Lcda4
Lcdb0:	sec
	rts

Lcdb2:	jsr	Scdec
	bne	Lcdb0
	sta	shs_os_code,y
	ldy	#$00
	asl
	bne	Lcdc1
	ldy	#$87
Lcdc1:	jsr	Scdfb
	jsr	clear_ram_ptr
	ldy	mslot
	lda	shs_idx_part_data,y
	cmp	#$01
	beq	Lcde2
	clc
	adc	#$04
	sta	hw_reg_addr_low-hw_reg_offset,x
	lda	shs_os_code,y
	sta	hw_reg_data-hw_reg_offset,x
	eor	#check_xor_val
	sta	hw_reg_data-hw_reg_offset,x
Lcde2:	lda	shs_os_code,y
	eor	#check_xor_val
	sta	shs_os_check,y
	clc
	rts

	public	Scdec	; referenced from slot
Scdec:	lda	proflag
	beq	Lcdf3	; Pascal
	cmp	#os_prodos
Lcdf3:	rts

Lcdf4:	ldx	mslot1088
	jsr	Sce67
	iny

	public	Scdfb	; referenced from pmgr
Scdfb:	lda	Dcf30,y
	bne	Lcdf4
	rts

Lce01:	and	#$0f
	sta	hw_reg_data-hw_reg_offset,x

Sce06:	lda	#$00
	sta	hw_reg_data-hw_reg_offset,x
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
	sta	hw_reg_data-hw_reg_offset,x
	lda	Z3e
	sta	hw_reg_data-hw_reg_offset,x
	pla
	tay
	rts

Lce34:	jsr	Sce53
	lda	#$01
Lce39:	pha
	jsr	Sce06
	lda	#$11
	sta	hw_reg_data-hw_reg_offset,x
	pla
	pha
	sta	hw_reg_data-hw_reg_offset,x
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
Lce58:	sta	hw_reg_data-hw_reg_offset,x
	cmp	hw_reg_addr_low-hw_reg_offset,x
	bne	Lce58
	ror
	bcc	Lce66
	sta	hw_reg_data-hw_reg_offset,x
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
	lda	Dcf30,y
Lcea2:	sta	hw_reg_data-hw_reg_offset,x
	pla
	sec
	sbc	#$01
	bne	Lce8e
Lceab:	pla
	rts

Lcead:	and	#$0f
Lceaf:	pha
	lda	#$ff
	sta	hw_reg_data-hw_reg_offset,x
	sta	hw_reg_data-hw_reg_offset,x
	jsr	Sce06
	sta	hw_reg_data-hw_reg_offset,x
	pla
	sec
	sbc	#$01
	bne	Lceaf
	rts

Lcec5:	lda	shg_04f8
	sta	hw_reg_data-hw_reg_offset,x
	lda	shg_0478
Lcece:	sta	hw_reg_data-hw_reg_offset,x
	rts

Lced2:	lda	mslot
	eor	#$f0
	bne	Lcece
Lced9:	lda	#$00
	sta	hw_reg_addr_low-hw_reg_offset,x
	iny
	lda	Dcf30,y
	sta	hw_reg_addr_mid-hw_reg_offset,x
	iny
	lda	Dcf30,y
	sta	hw_reg_addr_high-hw_reg_offset,x
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
	sta	hw_reg_data-hw_reg_offset,x
	pla
	tay
	rts

Scf27:	lda	#$ff
Lcf29:	sta	hw_reg_data-hw_reg_offset,x
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


copy_s2g_part_size:
	lda	shs_cur_part_size_high,y
	lsr
	sta	shg_0478
	lda	shs_cur_part_size_low,y
	ror
	sta	shg_04f8		; leaves part_size_low in A
	rts

	fillto	$d000,$ff

	endsection	common


; bank 1 common space ($c800-$cfff)
	org	$d800

b1_diag	equ	*-$1000

	section	diag
	phase	ram_application_base

ch	equ	$24
Z28	equ	$28
Z29	equ	$29
	
diag:	stx	dD0d2b
	jsr	home
	ldy	#d_msg_idx_banner
	jsr	d_msgout
	jsr	Scb00
	sta	dD0d2a
	ldy	#$00
	sty	dD0d2f
	asl
	bcc	dL0a1b
	dey
	tya
dL0a1b:	sty	Z3e
	sta	Z3f
	jsr	Scb9b
	ldy	#d_msg_idx_bytes
	jsr	d_msgout
	jsr	Sca8b
	sta	hw_reg_data-hw_reg_offset,x
	lda	#$04
	sta	hw_reg_addr_mid-hw_reg_offset,x
	lda	#$ff
	sta	hw_reg_data-hw_reg_offset,x
	ldy	mslot
	sta	shs_os_code,y
dL0a3d:	lda	#$00
	sta	ch
	ldy	#d_msg_idx_pass
	jsr	d_msgout
	lda	dD0d2f
	jsr	prbyte
	ldy	#d_msg_idx_testing
	jsr	d_msgout
	lda	ch
	clc
	adc	Z28
	sta	dS0ae4+1
	lda	#$00
	adc	Z29
	sta	dS0ae4+2
	ldy	#$08
dL0a62:	dey
	dey
	sty	dD0d29
	jsr	dS0ae4
	jsr	dS0ae8
	bcs	dL0a80
	ldy	dD0d29
	bne	dL0a62
	inc	dD0d2f
	lda	kbd
	bpl	dL0a3d
	sta	kbdstrb
	rts

dL0a80:	tya
	pha
	ldy	#d_msg_idx_card_failure
	jsr	d_msgout
	lda	#$08
	sec
	sbc	dD0d29
	lsr
	tay
	ora	#$b0
	jsr	cout
	cpy	#$03
	bcs	dL0a9a
	pla
	rts

dL0a9a:	ldy	#d_msg_idx_address
	jsr	d_msgout
	ldx	mslot1088
	lda	hw_reg_addr_low-hw_reg_offset,x
	dec	hw_reg_addr_low-hw_reg_offset,x
	and	#$7f
	bne	dL0ab9
	lda	hw_reg_addr_mid-hw_reg_offset,x
	dec	hw_reg_addr_mid-hw_reg_offset,x
	and	#$7f
	bne	dL0ab9
	dec	hw_reg_addr_high-hw_reg_offset,x
dL0ab9:	lda	hw_reg_addr_high-hw_reg_offset,x
	ldy	dD0d2a
	cpy	#$09
	bcs	dL0ac5
	and	#$0f
dL0ac5:	jsr	prbyte
	lda	hw_reg_addr_mid-hw_reg_offset,x
	jsr	prbyte
	lda	hw_reg_addr_low-hw_reg_offset,x
	jsr	prbyte
	lda	#$ad
	jsr	cout
	pla
	eor	hw_reg_data-hw_reg_offset,x
	jsr	prbyte
	jsr	crout
	rts

dS0ae4:	inc	D0400
	rts

dS0ae8:	lda	dD0af4+1,y
	pha
	lda	dD0af4,y
	pha
	ldx	dD0d2b
	rts

dD0af4:	fdb	dL0be4-1
	fdb	dL0b81-1
	fdb	dL0b1e-1
	fdb	dL0afc-1


dL0afc:	ldy	#$00
dL0afe:	tya
	jsr	dS0b0a
	eor	hw_reg_addr_high-hw_reg_offset,x
	and	#$0f
	bne	dL0b1c
	tya
	cmp	hw_reg_addr_mid-hw_reg_offset,x
	bne	dL0b1c
	cmp	hw_reg_addr_low-hw_reg_offset,x
	bne	dL0b1c
	jsr	dS0ae4
	iny
	bne	dL0afe
	clc
	rts

dL0b1c:	sec
	rts

dL0b1e:	lda	#$00
	ldy	#$f0
	jsr	dS0b0a
dL0b25:	lda	dD0d28
	jsr	dS0b6b
	sta	dD0d28
	lda	dD0d27
	jsr	dS0b6b
	sta	dD0d27
	jsr	dS0d13
	inc	dD0d28
	bne	dL0b4b
	inc	dD0d27
	bne	dL0b4b
	inc	dD0d26
	bne	dL0b4b
	clc
	rts

dL0b4b:	jsr	dS0ae4
	lda	hw_reg_data-hw_reg_offset,x
	lda	hw_reg_addr_high-hw_reg_offset,x
	eor	dD0d26
	bne	dL0b69
	lda	hw_reg_addr_mid-hw_reg_offset,x
	cmp	dD0d27
	bne	dL0b69
	lda	hw_reg_addr_low-hw_reg_offset,x
	cmp	dD0d28
	beq	dL0b25
dL0b69:	sec
	rts

dS0b6b:	cmp	#$10
	bcc	dL0b75
	cmp	#$70
	bcs	dL0b76
	lda	#$70
dL0b75:	rts

dL0b76:	cmp	#$90
	bcc	dL0b75
	cmp	#$f0
	bcs	dL0b75
	lda	#$f0
	rts

dL0b81:	lda	dD0d2a
	beq	dL0b94
	lda	#$00
	sta	dD0d30
	jsr	dS0b96
	jsr	dS0b96
	jsr	dS0b96
dL0b94:	clc
	rts

dS0b96:	jsr	clear_ram_ptr
	tay
dL0b9a:	tya
	sta	hw_reg_data-hw_reg_offset,x
	jsr	dS0bbe
	bne	dL0b9a
	jsr	dS0ae4
	tya
	jsr	dS0b0a
dL0baa:	tya
	cmp	hw_reg_data-hw_reg_offset,x
	bne	dL0bba
	jsr	dS0bbe
	bne	dL0baa
	sec
	ror	dD0d30
	rts

dL0bba:	pla
	pla
	sec
	rts

dS0bbe:	bit	dD0d30
	bpl	dL0bdb
	inc	dD0d26
	inc	hw_reg_addr_high-hw_reg_offset,x
	lda	dD0d26
	lsr
	cmp	dD0d2a
	bcc	dL0bdb
	ldy	#$00
	rts

	fcb	$ee,$27,$0d,$fe,$f9,$bf

dL0bdb:	inc	dD0d28
	iny
	rts

dD0be0:	fcb	$00,$55,$aa,$ff

dL0be4:	lda	dD0d2a
	beq	dL0bfc
	ldy	#$03
dL0beb:	lda	dD0be0,y
	sta	dD0d2c
	jsr	dS0bfe
	jsr	dS0c2e
	bcs	dL0bfd
	dey
	bpl	dL0beb
dL0bfc:	clc
dL0bfd:	rts

dS0bfe:	jsr	dS0c6d
dL0c01:	jsr	dS0c0a
	dec	dD0d2d
	bne	dL0c01
	rts

dS0c0a:	tya
	pha
	lda	dD0d2c
	ldy	#$00
	sty	dD0d2e
dL0c14:	sta	hw_reg_data-hw_reg_offset,x
	sta	hw_reg_data-hw_reg_offset,x
	sta	hw_reg_data-hw_reg_offset,x
	sta	hw_reg_data-hw_reg_offset,x
	iny
	bne	dL0c14
	jsr	dS0ae4
	inc	dD0d2e
	bne	dL0c14
	pla
	tay
	rts

dS0c2e:	jsr	dS0c6d
dL0c31:	jsr	dS0c3c
	bcs	dL0c3b
	dec	dD0d2d
	bne	dL0c31
dL0c3b:	rts

dS0c3c:	tya
	pha
	lda	dD0d2c
	ldy	#$00
	sty	dD0d2e
dL0c46:	cmp	hw_reg_data-hw_reg_offset,x
	bne	dL0c69
	cmp	hw_reg_data-hw_reg_offset,x
	bne	dL0c69
	cmp	hw_reg_data-hw_reg_offset,x
	bne	dL0c69
	cmp	hw_reg_data-hw_reg_offset,x
	bne	dL0c69
	iny
	bne	dL0c46
	jsr	dS0ae4
	inc	dD0d2e
	bne	dL0c46
	pla
	tay
	clc
	rts

dL0c69:	tay
	pla
	sec
	rts

dS0c6d:	lda	dD0d2a
	lsr
	sta	dD0d2d
	jmp	clear_ram_ptr


; d_msgout decompresses and outputs a message
; Y indexes d_msgtab, carry alternates 0 = high nibble, 1 = low nibble

dL0c77:	php
	jsr	cout
	plp
	skip1
d_msgout:
	clc		; start with high nibble

	jsr	d_getnib		; get nibble
	lda	d_msg_dec_tab_1,x			; index first table
	bne	dL0c77			; if table content non-zero, it's a char

	jsr	d_getnib		; get nibble
	lda	d_msg_dec_tab_2,x	; index second table
	bne	dL0c77			; if table content non-zero, it's a char

	rts		; if both table entries were zero, end of message


; get one nibble
d_getnib:
	lda	d_msgtab,y	; get byte from message table
	bcs	dL0c9b	; carry clear?
	lsr		; yes, get high nibble
	lsr
	lsr
	lsr
	tax
	sec		; set carry so next call will get low nibble
	rts

; carry was set, 
dL0c9b:	iny		; advance pointer
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
	fcb	char_cr+$80	; carriage return
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


dS0b0a:	sta	dD0d28
	sta	dD0d27
	sty	dD0d26

dS0d13:	lda	dD0d28
	sta	hw_reg_addr_low-hw_reg_offset,x
	lda	dD0d27
	sta	hw_reg_addr_mid-hw_reg_offset,x
	lda	dD0d26
	sta	hw_reg_addr_high-hw_reg_offset,x
	rts

dD0d26:	equ	*
dD0d27:	equ	*+1
dD0d28:	equ	*+2
dD0d29:	equ	*+3
dD0d2a:	equ	*+4
dD0d2b:	equ	*+5
dD0d2c:	equ	*+6
dD0d2d:	equ	*+7
dD0d2e:	equ	*+8
dD0d2f:	equ	*+9
dD0d30:	equ	*+10

	dephase
	endsection diag

b1_diag_end	equ	*-$1000

b1_partmgr	equ	*-$1000

	section partmgr
	phase	ram_application_base


; parition table buffer
pt_buffer  equ	 $0800
D0802	equ	pt_buffer+$02
D0803	equ	pt_buffer+$03


part_table_entry_size	equ	$18

; partition table entry addresses, must be indexed
pte_ofs_base_addr_high	equ	$00	; parition base address high
pte_ofs_base_addr_mid	equ	$01	; parition base address mid
pte_ofs_size_high	equ	$02	; partition size high
pte_ofs_size_mid	equ	$03	; parititon size mid
pte_ofs_os_code		equ	$04	; OS code
pte_ofs_os_check	equ	$05	; OS check
pte_ofs_name		equ	$08	; partition name

D0900	equ	$0900
D0901	equ	$0901
D0902	equ	$0902
D0903	equ	$0903
D0904	equ	$0904
D0905	equ	$0905
D0906	equ	$0906
D0907	equ	$0907
D0908	equ	$0908


partition_table_entry_size	equ	24


partmgr:
; check for valid parition table
	jsr	Sca8b
	lda	hw_reg_data-hw_reg_offset,x
	cmp	#$ae
	bne	pL0a1b
	eor	hw_reg_data-hw_reg_offset,x
	cmp	#$5a
	bne	pL0a1b
	lda	hw_reg_data-hw_reg_offset,x
	eor	hw_reg_data-hw_reg_offset,x
	cmp	#$5a
	beq	pL0a6b

; no valid partition table found, will need to create a partition table
pL0a1b:	ldy	mslot			; are the screen holes valid?
	lda	shs_os_code,y
	eor	shs_os_check,y
	cmp	#$5a
	bne	pL0a3f			; screen holes were not valid

; screen holes were valid, ask user if partition table should be installed
	jsr	home
	ldy	#p_msg_idx_warning_installing
	jsr	p_msgout
	lda	#'?'+$80
	jsr	cout
	jsr	rdkey_uc
	cmp	#'Y'+$80
	beq	pL0a3f
	jmp	pL0e06			; user says no; reboot?

; install a parititon table
pL0a3f:	jsr	Sca8b
	ldy	#$aa
	jsr	Scdfb
	jsr	Sca8b
	lda	#$04
	sta	hw_reg_addr_low-hw_reg_offset,x
	ldy	mslot
	lda	shs_card_block_count,y
	pha
	sta	hw_reg_data-hw_reg_offset,x
	lda	#$0a
	sta	hw_reg_addr_low-hw_reg_offset,x
	pla
	asl
	sec
	sbc	#$01
	sta	hw_reg_data-hw_reg_offset,x
	lda	#$fc
	sta	hw_reg_data-hw_reg_offset,x

pL0a6b:	jsr	clear_ram_ptr
	jsr	Scaeb
	lda	D0802
	sta	D0901
	jsr	home
pL0a7a:	lsr	D0900

pm_main_loop:	lda	#$00	; home the cursor without clearing the screen
	sta	ch
	sta	Cv

	ldy	#p_msg_idx_heading
	jsr	p_msgout
	lda	mslot
	eor	#$70
	jsr	cout
	jsr	crout
	jsr	crout
	ldy	#$00
pL0a98:	jsr	pS0c03
	iny
	cpy	#$09
	bcc	pL0a98
	ldy	#p_msg_idx_help_1
	jsr	p_msgout
	ldy	#$59
	bit	D0900
	bpl	pL0aee
	ldy	#p_msg_idx_help_2
pL0aee:	jsr	p_msgout
	jsr	clreop

	jsr	rdkey_uc
	cmp	#'1'+$80
	bcc	pL0acf		; not a digit
	cmp	#'9'+1+$80
	bcs	pL0acf		; not a digit

	sbc	#$b0		; ASCII to binary digit
	jsr	part_num_to_part_table_offset
	sta	D0901
	bit	D0900
	bmi	pm_main_loop
	jmp	pL0df6

pL0acf:	sta	D0902
	ldy	#$06
	bit	D0900
	bpl	pL0adb
	ldy	#$fd	; 253 }
pL0adb:	iny
	iny
	iny
	lda	pD0af7,y
	beq	pL0af1
	cmp	D0902
	bne	pL0adb
	lda	pD0af7+2,y
	pha
	lda	pD0af7+1,y
	pha
	rts

pL0af1:	jsr	bell12
	jmp	pm_main_loop


pD0af7:	cmd_ent	'N',p_cmd_name
	cmd_ent	'C',p_cmd_clear		; Clear a partition
	cmd_ent	'S',p_cmd_size		; change Size of a partition
	cmd_ent char_cr,p_cmd_boot	; boot partition
	cmd_ent	key_left,p_cmd_up
	cmd_ent	key_up,p_cmd_up
	cmd_ent key_down,p_cmd_down
	cmd_ent	key_right,p_cmd_down
	cmd_ent	char_esc,p_cmd_exit	; quit
	cmd_ent	'R',p_cmd_reconfigure	; Reconfigure
	fcb	$00			; end of table


p_cmd_boot:
	bit	D0900
	bmi	pL0b1e
	jmp	pL0df6

pL0b1e:	jsr	pS0de8
	jmp	pL0a7a

p_cmd_reconfigure:
	sec
	ror	D0900
	jmp	pm_main_loop

p_cmd_exit:
	bit	D0900
	bpl	pL0b33
	jmp	ram_application_base

pL0b33:	jsr	pS0dd7
	jmp	pL0e36

p_cmd_up:
	lda	D0901
	sec
	sbc	#partition_table_entry_size
pL0b3f:	cmp	#$e0
	bcs	L0b46
	sta	D0901
L0b46:	jmp	pm_main_loop

p_cmd_down:
	lda	D0901
	clc
	adc	#partition_table_entry_size
	bcc	pL0b3f

p_cmd_name:
	ldy	#p_msg_idx_new_name
	jsr	p_msgout
	ldx	#$10
	jsr	pS0e4f
	bcs	pL0b6e
	ldx	#$00
	ldy	D0901
pL0b62:	lda	D0908,x
	sta	pt_buffer+pte_ofs_name,y
	iny
	inx
	cpx	#$10
	bcc	pL0b62
pL0b6e:	jmp	pm_main_loop

p_cmd_size:
	jsr	pS0e43
	beq	pL0bea
	tya
	clc
	adc	#partition_table_entry_size
	tay
	cpy	#$e0
	bcs	pL0bea
	jsr	pS0e46
	beq	pL0bea
	ldy	#p_msg_idx_new_size
	jsr	p_msgout
	ldx	#$06
	jsr	pS0e4f
	jsr	pS0ea1
	asl	D0904
	rol	D0905
	asl	D0904
	rol	D0905
	clc
	ldy	D0901
	lda	pt_buffer+pte_ofs_size_mid,y
	adc	pt_buffer+part_table_entry_size+pte_ofs_size_mid,y
	sta	D0906
	lda	pt_buffer+pte_ofs_size_high,y
	adc	pt_buffer+part_table_entry_size+pte_ofs_size_high,y
	sta	D0907
	sec
	lda	D0906
	sbc	D0904
	pha
	lda	D0907
	sbc	D0905
	bcc	pL0be6
	sta	pt_buffer+part_table_entry_size+pte_ofs_size_high,y
	pla
	sta	pt_buffer+part_table_entry_size+pte_ofs_size_mid,y
	clc
	lda	D0904
	sta	pt_buffer+pte_ofs_size_mid,y
	adc	pt_buffer+pte_ofs_base_addr_mid,y
	sta	pt_buffer+part_table_entry_size+pte_ofs_base_addr_mid,y
	lda	D0905
	sta	pt_buffer+pte_ofs_size_high,y
	adc	pt_buffer+pte_ofs_base_addr_high,y
	sta	pt_buffer+part_table_entry_size+pte_ofs_base_addr_high,y
	jmp	pm_main_loop

pL0be6:	pla
	ldy	#$d5
	skip2
pL0bea:	ldy	#$c9
	jsr	p_msgout
	jsr	rdkey
	jmp	pm_main_loop

p_cmd_clear:
	jsr	pS0e43
	bne	pL0c00
	ldx	D0901
	inc	pt_buffer+pte_ofs_os_check,x
pL0c00:	jmp	pm_main_loop


; display one parition table entry
pS0c03:	tya

	pha
	jsr	part_num_to_part_table_offset
	pha

; set inverse if this parition is active
	ldx	#$ff
	cmp	D0901
	bne	pL0c12
	ldx	#$3f
pL0c12:	stx	invflg

; indent two spaces
	jsr	print_two_spaces

; output paritition number, two more spaces
	tya
	clc
	adc	#$b1
	jsr	cout
	jsr	print_two_spaces
	pla

; output paritition name, followed by two spaces
	pha
	tay
	ldx	#$10
pL0c26:	lda	pt_buffer+pte_ofs_name,y
	bne	pL0c2d
	lda	#' '+$80	; NUL chars converted to spaces
pL0c2d:	jsr	cout
	iny
	dex
	bne	pL0c26
	jsr	print_two_spaces
	pla

; output partition size in KiB, followed by two spaces
	pha
	tay
	lda	pt_buffer+pte_ofs_size_high,y
	sta	Z3f
	lda	pt_buffer+pte_ofs_size_mid,y
	sta	Z3e
	jsr	Scb9b
	lda	#$ff
	sta	invflg
	jsr	print_two_spaces
	pla

; output paritition type
	tay
	lda	pt_buffer+pte_ofs_os_check,y
	eor	#$5a
	cmp	pt_buffer+pte_ofs_os_code,y
	beq	pL0c5c
	lda	#$01		; if the parition type is invalid (code xor check != 0x5a), consider it clear ($01 not in table)

pL0c5c:	ldy	#$04		; search paritiion type table for the code
pL0c5e:	cmp	parition_type_code_table-1,y
	beq	pL0c66
	dey
	bne	pL0c5e
; if we fall through here, Y=00, which means that there was no match, and that the parititon is "clear"

pL0c66:	lda	partition_type_msg_idx_table,y
	tay
	jsr	p_msgout
	pla
	tay
	rts

parition_type_code_table:
	fcb	os_prodos
	fcb	os_pascal
	fcb	os_dos
	fcb	os_cpm

partition_type_msg_idx_table:
	fcb	p_msg_idx_clear
	fcb	p_msg_idx_prodos
	fcb	p_msg_idx_pascal
	fcb	p_msg_idx_dos
	fcb	p_msg_idx_cpm


print_two_spaces:
	lda	#' '+$80
	jsr	cout
	jmp	cout


; get offset into first RAMdisk page of a parition table entry
part_num_to_part_table_offset:
	sta	D0902
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
pL0c8f:	jsr	p_getnib	; get nibble
	lda	pD0cbe,x		; index first table
	bne	pL0ca0		; if table content non-zero, it's a char

	jsr	p_getnib	; it was zero, get next nibble
	lda	pD0ccd,x		; index second table
	bne	pL0ca0		; if table content non-zero, it's a char

	rts		; if both table entries were zero, end of message


; output a decoded character, except if MSB is zero, output two spaces
pL0ca0:	php
	bmi	pL0ca6
	jsr	print_two_spaces

pL0ca6:	jsr	cout
	plp
	bne	pL0c8f


; get one nibble
p_getnib:
	lda	p_msgtab,y	; get byte from message table
	bcs	pL0cb8	; carry clear?
	lsr		; yes, get high nibble
	lsr
	lsr
	lsr
	tax
	sec		; set carry so next call will get low nibble
	rts

; carry was set, getting low nibble
pL0cb8:	iny		; advance pointer
	and	#$0f
	tax
	clc		; clear carry so next call will get high nibble
	rts

; nibble to character decode table 1
; note that this table includes the first byte of decode table 2
; table includes all but one of the letters ETAOINSHRDLC, the 13
; most common letters of the English language, in an arbitrary
; permutation that might be intended to confuse reverse-engineers.
pD0cbe:	fcb	$00		; escape to second table
	fcb	char_cr+$80	; carriage return
	fcb	$01		; two spaces
	fcsm	"GLITCH REASO"

; nibble to character decode table 2
pD0ccd:	fcsm	"N"
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


pS0dd7:	lda	D0901
	sta	D0802
	eor	#$5a
	sta	D0803
	jsr	pS0de8
	jmp	Lc9df

; write parition table back to card memory
pS0de8:	jsr	Sca8b
	tay
pL0dec:	lda	pt_buffer+pte_ofs_base_addr_high,y
	sta	hw_reg_data-hw_reg_offset,x
	iny
	bne	pL0dec
	rts

pL0df6:	jsr	pS0dd7
	jsr	pS0e43
	bne	pL0e09
	lda	pt_buffer+pte_ofs_size_high,y
	ora	pt_buffer+pte_ofs_size_mid,y
	beq	pL0e09
pL0e06:	jsr	pS0e27
pL0e09:	ldy	#p_msg_idx_cannot_boot
	jsr	p_msgout
pL0e0e:	ldx	#$01
	jsr	pS0e4f
	bcs	pL0e36
	lda	D0908
	cmp	#$b1
	bcc	pL0e0e
	cmp	#$b8
	bcs	pL0e0e
	adc	#$10
	tay
	lda	#$00
	beq	pL0e2f

pS0e27:	ldx	mslot1088
	ldy	mslot
	lda	#$7b
pL0e2f:	sty	L00+1
	sta	L00
	jmp	(L00)

pL0e36:	ldx	#$02
	lda	#$00
pL0e3a:	sta	pt_buffer+pte_ofs_base_addr_high,x
	dex
	bpl	pL0e3a
	jmp	(resetvec)

pS0e43:	ldy	D0901
pS0e46:	lda	pt_buffer+pte_ofs_os_code,y
	eor	pt_buffer+pte_ofs_os_check,y
	cmp	#$5a
	rts

pS0e4f:	stx	D0903
	lda	#$a0
pL0e54:	sta	D0908,x
	dex
	bpl	pL0e54
	inx
pL0e5b:	jsr	rdkey_uc
	cmp	#$88
	beq	pL0e84
	cmp	#char_cr+$80
	beq	pL0e82
	cmp	#char_esc+$80
	beq	pL0e83
	cmp	#' '+$80
	bcs	pL0e74
pL0e6e:	jsr	bell12
	jmp	pL0e5b

pL0e74:	cpx	D0903
	bcs	pL0e6e
	sta	D0908,x
	jsr	cout
	inx
	bne	pL0e5b
pL0e82:	clc
pL0e83:	rts

pL0e84:	txa
	beq	pL0e5b
	dex
	dec	ch
	lda	#$a0
	sta	D0908,x
	jsr	cout
	dec	ch
	jmp	pL0e5b

rdkey_uc:
	jsr	rdkey
	cmp	#$e0	; lower case?
	bcc	pL0ea0	;   no
	and	#$df	;   yes, convert to upper case
pL0ea0:	rts

pS0ea1:	lda	#$00
	tay
	tax
pL0ea5:	sta	D0904
	stx	D0905
	lda	#$0a
	sta	D0902
	ldx	#$00
	lda	D0908,y
	eor	#$b0
	cmp	#$0a
	bcc	pL0ebc
	rts

pL0ebc:	adc	D0904
	pha
	txa
	adc	D0905
	tax
	pla
	dec	D0902
	bne	pL0ebc
	iny
	bne	pL0ea5

	dephase
	endsection partmgr

b1_partmgr_end	equ	*-$1000

	fillto	$dfff,$ff
