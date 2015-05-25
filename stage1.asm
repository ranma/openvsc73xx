;-----------------------------------------------------------------------------
; Custom VSC73xx 8051 iCPU bootloader.
;
; Copyright (C) 2015 Tobias Diedrich
; GNU GENERAL PUBLIC LICENSE Version 2 or later.
;
; This was written specifically for the D-Link DGS-1008D switches using this
; chipset.  Even for these it may not work on just any model, YMMV.
;
; Tested on:
; Model No    P/N                H/W Ver
; DGS-1008D   EGS1008DE....E1G   E1
;
; To flash this, it is necessary to unsolder the SPI flash chip.
; This is left as an exercise to the interested reader. ;)
;
; Once installed, new code can easily be uploaded over the serial port
; connector (J1) on the DGS-1008D.
;
; Assumptions made:
;   - 25MHz crystal and 156.25MHz PLL clock.
;   - iCPU configured to boot from 8KiB SPI flash, no external ram or rom.
;   - Atmel AT25640 8KiB SPI flash (32 byte pages, 3MHz clock).
;   - Flash image layout: [2-byte size] [image data] [simple checksum]
;
; DGS-1008D J1 pinout ([1] towards LEDS: [1]23456]):
;   1: VCC
;   2: VCC
;   3: TX
;   4: RX
;   5: GND
;   6: GND
;-----------------------------------------------------------------------------

; Additional iCPU SFRs.
.equ DPL1, 0x84
.equ DPH1, 0x85
.equ DPS, 0x86
.equ GPIO_OUT, 0x90
.equ GPIO_OE, 0xa0
.equ RA_DONE, 0xf8
.equ RA_BLK, 0xf9
.equ RA_AD_RD, 0xfa
.equ RA_AD_WR, 0xfb
.equ RA_DA0, 0xfc
.equ RA_DA1, 0xfd
.equ RA_DA2, 0xfe
.equ RA_DA3, 0xff

; Some local IRAM vars (TODO: we could instead use regs here).
.equ SPI_STATE, 0x20
.equ XMODEM_STATE, 0x41

; Constants.
.equ XMODEM_WAIT_TIME, 10 ; About 2 seconds.

.equ XMODEM_SOH, 0x01
.equ XMODEM_EOT, 0x04
.equ XMODEM_ACK, 0x06
.equ XMODEM_NAK, 0x15

; 512 bytes minus the 2 bytes size header and 1 byte checksum, so the payload is
; aligned on an erase boundary (really every 32 bytes) in the flash.
.equ STAGE1_MAX_SIZE, (512 - 3)

; By default 8KiB of ram are mapped @0x0000 in code space and @0xe000 in xram space.
; We also enable the xram mapping @0x0000 during init.
.equ SAFE_XRAM_BASE, 0xe000
.equ XRAM_BASE, 0x0000
; Relocate code to last 512 byte of ram
.equ RELOC_ADDR, 0x1e00
.equ RELOC_END, (RELOC_ADDR + STAGE1_MAX_SIZE)
.equ XRAM_DST, (XRAM_BASE + RELOC_ADDR)
.equ CRAM_SRC, 0x0000

.equ STAGE2_SPI_ADDR,  0x200
.equ STAGE2_LOAD_XADDR, XRAM_BASE
.equ STAGE2_LOAD_CADDR, 0x0000
.equ STAGE2_SIZE,      0x1e00

.equ ACALL_MASK, 0x7ff ; 2KiB boundary
.equ ACALL_OFS, (RELOC_ADDR & ACALL_MASK)

;-----------------------------------------------------------------------------
; Init code, ok to overwrite later.
;
; Initialize basic hw, clear memory, set up serial and relocate code to top of
; memory.
;
; For the assembler this is located at RELOC_ADDR, but it really is running at
; @0x0000 on boot.  Since there is no relative call instruction, we need to do
; some arithmetic and substract ACALL_OFS on any acall done before the code
; relocation is done.
;-----------------------------------------------------------------------------

.org RELOC_ADDR
hw_init:
	; original fw does this, may improve boot stability?
	setb GPIO_OE.1
	clr GPIO_OUT.1

	; Set up stack and essential registers
	clr	EA
	clr	PSW
	mov	DPS, #0
	mov	SP, #0xc0

	; Clear iram
	mov	R0, #0
	clr	A
clear_iram_loop:
	mov	@R0, A
	djnz	R0, clear_iram_loop

	; Clear remaining xram
	mov	DPTR, #(SAFE_XRAM_BASE + stage1_end - RELOC_ADDR)
clear_xram_loop:
	movx	@DPTR, A
	inc	DPTR
	mov	A, DPL
	orl	A, DPH
	jnz clear_xram_loop

	; Set up serial
	mov	A, #0xff
	mov	TL2, A
	mov	TH2, A
	mov	R1, A
	mov	RCAP2H, A
	mov	RCAP2L, #0xeb ; 115200 (116257, ~0.9% error) baud @ 78.125MHz iCPU clk
	mov	T2CON, #0x34
	mov	SCON, #0x52
	clr	RI
	mov	A, SBUF

	; Switch to fast clock, deliberately not the first thing we do (or it doesn't startup reliably?)
	mov	RA_BLK, #0xe0   ; system block
	mov	A, #0x10 ; ICPU_CTRL
	acall chipreg_read - ACALL_OFS

	; R/W seems to be different HW regs, need to copy.
	; Reading from RA_DAx also clears the RA_DONE flag for reads.
	clr	A
	mov	RA_DA3, A
	mov	RA_DA2, A
	inc	A ; 0x01: 156.25MHz / 2 => 78.125MHz (highest allowed)
	mov	RA_DA1, A
	mov	A, RA_DA0  ; Don't touch the remaining state.
	mov	RA_DA0, A
	mov	A, #0x10 ; ICPU_CTRL
	acall chipreg_write - ACALL_OFS

	; Serial is set up, write 'S1\r\n' as hello message.
	; Writes are deliberately spread out, so if it dies in between we can
	; see at which point.
	mov	A, #'S'
	acall serial_write - ACALL_OFS

	mov	RA_DA0, #0x81  ; Map RAM into 0x0000-0x1fff and 0xe000-0xffff
	mov	A, #0x1b ; ICPU_RAM_MAP
	acall chipreg_write - ACALL_OFS

	mov	A, #'1'
	acall serial_write - ACALL_OFS

	; Set up timer0 in 16-bit mode and start it.
	mov	TMOD,#1
	setb	TR0

	; Delay a little to let the clocks stabilize.
startup_delay1:
	jb	TF0, startup_delay1
	clr	TF0
startup_delay2:
	jb	TF0, startup_delay2

	mov	A, #13
	acall serial_write - ACALL_OFS

	; Copy code
	mov	DPTR, #XRAM_DST
	inc	DPS
	mov	DPTR, #CRAM_SRC

	; Copy 2x256 bytes.
	clr	0
	mov	1, #2
copy_loop:
	clr	A
	movc	A, @A + DPTR
	inc	DPTR
	inc	DPS
	movx	@DPTR, A
	inc	DPTR
	inc	DPS
	djnz	R0, copy_loop
	djnz	R1, copy_loop
	; Restore DPS
	mov	DPS, #0

	mov	A, #10
	acall serial_write - ACALL_OFS

	; Explicitly tranfer to relocated code address
	ljmp stage1_main

;-----------------------------------------------------------------------------
; Main code, after relocation.
;
; Waits some time for an XMODEM firmware upload to start, then once this times
;  out reads the regular firmware from flash and boots it.
;-----------------------------------------------------------------------------


	; Wait some time for an XMODEM code upload on the serial port
stage1_main:
	mov	TH1, #XMODEM_WAIT_TIME
	mov	XMODEM_STATE, #0
	mov	DPTR, #STAGE2_LOAD_XADDR
stage1_wait:
	jbc	RI, serial_recv_buf_full
	jbc	TF0, timer0_overflow
	sjmp stage1_wait
serial_recv_buf_full:
	mov	A, SBUF
	jbc PSW.1, stage1_serial_read_resume
	acall serial_recv
	sjmp stage1_wait

stage1_serial_read:
	setb PSW.1
	sjmp stage1_wait
stage1_serial_read_resume:
	mov	TL1, #0  ; reset the wait time until sending NAK
	ret

timer0_overflow:
	mov	A, TL1
	add	A, #-1
	mov	TL1, A
	jnz stage1_wait
	mov	TL1, #20
	mov	A, TH1
	add	A, #-1
	mov	TH1, A
	; Timeout, boot stage2
	jz boot_stage2

	add	A, #'0'
	acall serial_write

	mov	SBUF, #XMODEM_NAK
	sjmp stage1_wait

serial_recv:
	cjne	A, #XMODEM_SOH, serial_recv_not_soh
	sjmp serial_recv_got_soh

serial_recv_not_soh:
	cjne	A, #XMODEM_EOT, serial_recv_not_eot
	mov	A, XMODEM_STATE
	cjne	A, #0, serial_recv_got_eot

serial_recv_not_eot:
	ret


serial_recv_got_soh:
	acall stage1_serial_read
	setb	C
	subb	A, XMODEM_STATE
	jz serial_recv_wait_block2_loop
	; protocol error, invalid block number
	ret

serial_recv_wait_block2_loop:
	acall stage1_serial_read
	cpl	A
	setb	C
	subb	A, XMODEM_STATE
	jz serial_recv_do_load_bytes
	; protocol error, invalid block number
	ret

serial_recv_do_load_bytes:
	mov	R0, #128
	mov	R1, #0

serial_recv_load_bytes_loop:
	acall stage1_serial_read
	movx	@DPTR, A
	add	A, R1
	mov	R1, A
	inc	DPTR
	djnz	R0, serial_recv_load_bytes_loop

	acall stage1_serial_read
	cjne	A, 1, serial_recv_csum_mismatch
	inc	XMODEM_STATE
	mov	SBUF, #XMODEM_ACK
serial_recv_csum_mismatch:
	; TODO: Roll back DPTR to really recover.
	ret

serial_recv_got_eot:
	mov	SBUF, #XMODEM_ACK

	; Start XMODEM payload
	ljmp STAGE2_LOAD_CADDR

chipreg_read:
	mov RA_AD_RD, A
	sjmp chipreg_wait
chipreg_write:
	mov RA_AD_WR, A
chipreg_wait:
	jnb RA_DONE.0, chipreg_wait
	setb RA_DONE.0
	ret

boot_stage2:
	mov A, #'B'
	acall serial_write
	mov A, #13
	acall serial_write
	mov A, #10
	acall serial_write

	mov	SPI_STATE, #0
	mov	DPTR, #STAGE2_LOAD_XADDR
	inc	DPS
	mov	DPTR, #-STAGE2_SIZE
	mov	R0, #(STAGE2_SPI_ADDR >> 8)
	mov	R1, #(STAGE2_SPI_ADDR & 0xff)

eeprom_read:
	acall spi_cs_low
	mov A, #3 ; EEPROM read cmd
	acall spi_xfer
	mov A, R0
	acall spi_xfer
	mov A, R1
	acall spi_xfer

eeprom_read_loop:
	clr A
	acall spi_xfer
	inc DPS
	movx @DPTR, A
	inc DPTR
	inc DPS
	inc DPTR
	; Danger, will robinson, this depends on the state of DPS:
	mov	A, DPL1
	orl	A, DPH1
	cjne A, #0, eeprom_read_loop
	acall spi_cs_high

	; Restore DPS
	mov DPS, #0

	; Execute the code we just read
	ljmp STAGE2_LOAD_CADDR

	; TODO: Verify effective clock rate and add waits limit to <=3MHz if it is bigger.
spi_xfer:
	mov R7, A
	mov R6, #8
spi_xfer_loop:
	setb SPI_STATE.0
	mov A, R7
	jb ACC.7, spi_xfer_do_bit_set
	clr SPI_STATE.0
spi_xfer_do_bit_set:
	acall spi_update_state
	; clk high
	setb SPI_STATE.2
	acall spi_update_state
	setb C
	jb SPI_STATE.4, spi_xfer_di_bit_set
	clr C
spi_xfer_di_bit_set:
	mov A, R7
	rlc A
	mov R7, A
	; clk low
	clr SPI_STATE.2
	acall spi_update_state
	djnz R6, spi_xfer_loop
	mov A, R7
	ret

spi_cs_high:
	setb SPI_STATE.1 ; Set nCS high
	acall spi_update_state ; Seperate from disable to actively drive it high
	setb SPI_STATE.3 ; Tristate pins
	sjmp spi_update_state

spi_cs_low:
	clr SPI_STATE.1 ; Set nCS low
	setb SPI_STATE.3 ; Enable pins

spi_update_state:
	mov A, SPI_STATE
	mov RA_DA0, A
	mov RA_BLK, #0xe0  ; System block.
	mov A, #0x35  ; SIMASTER
	acall chipreg_write
	acall chipreg_read
	mov A, RA_DA0
	mov SPI_STATE, A
	ret

serial_hex:
	xch A, B
	mov A, B
	swap A
	anl A, #0xf
	lcall serial_hex_nibble
	mov A, B
	anl A, #0xf
	; fall through to serial_hex_nibble

serial_hex_nibble:
	clr C
	subb A, #0xa
	jc serial_hex_digit
	add A, #0x27 ; 'a' - '0' - 0xa
serial_hex_digit:
	add A, #0x3a ; '0' + 0xa
	; fall through to serial_write

serial_write:
	clr TI
	mov SBUF, A
serial_write_wait:
	jnb TI,serial_write_wait
	ret

stage1_end:
