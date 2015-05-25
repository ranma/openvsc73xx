;-----------------------------------------------------------------------------
; Example VSC73xx 8051 iCPU firmware payload.
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
; Best used with the stage1 bootloader, but should work fine stand-alone.
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

; Local vars
.equ SPI_STATE, 0x20
.equ TX_CNT, 0x40
.equ IP_LEN, 0x41
.equ DPL3, 0x42
.equ DPH3, 0x43
.equ BOOTP, 0x44
.equ BOOTP_TIMER, 0x45
.equ TX_PORT, 0x46

; Stack
.equ MAIN_STACK, 0xb0 ; 1 x 32 ( 0xb0 - 0xcf )
.equ CO_STACK, 0xc0 ; 4 x 16

; Additional SFRs
.equ DPL2, 0x84
.equ DPH2, 0x85
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

.equ RAM_END, 0x2000

.org 0
start:
	ljmp system_init
; convenience jump table for loadable code bits.
sys_puts:
	ljmp serial_puts
sys_packet_tx:
	ljmp packet_sg_tx
sys_recv_copy:
	ljmp recv_copy
sys_copy_bytes:
	ljmp copy_bytes
sys_compare_bytes:
	ljmp compare_bytes
sys_ip_checksum:
	ljmp checksum_sg_iphdr

.byte "CODE"

main:
	mov DPTR, #message_clear_init
	lcall serial_puts

	mov DPTR, #initlen
	mov A, DPH
	mov R1, A
	lcall serial_hex
	mov A, DPL
	mov R0, A
	lcall serial_hex
	mov DPTR, #system_init
clear_loop:
	clr A
	movx @DPTR, A
	mov A, #'.'
	lcall serial_write
	inc DPTR
	dec R0
	mov A, R0
	jnz clear_loop
	mov A, R1
	jz clear_done
	dec R1
	sjmp clear_loop

clear_done:
	mov DPTR, #message_init_done
	lcall serial_puts

repeat_mii_outer:
	mov R3, #0
	mov DPTR, #message_crnl
	lcall serial_puts
	mov DPTR, #message_mii_read
	lcall serial_puts

repeat_mii:
	mov A, R3
	lcall serial_hex
	mov A, #':'
	lcall serial_write

	mov A, R3
	mov R0, A
	lcall setup_link_state

	mov A, R3
	mov R0, A
	mov R1, #0x1c
	lcall mii_read

	mov A, RA_DA1
	lcall serial_hex
	mov A, RA_DA0
	lcall serial_hex
	mov A, #' '
	lcall serial_write

	inc R3
	mov A, R3
	anl A, #0x08
	jz repeat_mii

	lcall maybe_send_bootp
	lcall recv_poll

	sjmp repeat_mii_outer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

co_setup:
	; Sets up coroutine:
	; - Reset saved SP
	; - Clone regs
	; - Put entry point from DPTR onto stack
	mov A, PSW
	anl A, #0x18
	mov R0, A
	xrl A, #0x08
	mov R1, A
	rl A
	add A, #CO_STACK
	inc A
	mov R7 ,A

	; copy R0-R7
co_setup_clone_loop:
	mov A, @R0
	mov @R1, A
	inc R0
	inc R1
	mov A, R0
	anl A, #0x07
	jnz co_setup_clone_loop

	; set return address on stack to entry point
	mov A, R7
	mov R0, A
	mov A, DPH
	mov @R0, A
	dec R0
	mov A, DPL
	mov @R0, A
	ret

co_call:
	; Transfer control to coroutine:
	; - Preserves A and Flags (except P)
	; - Switches stack & register bank
	xch A, SP
	mov R7, A
	jbc PSW.3, co_call_exit
	setb PSW.3
co_call_exit:
	mov A, R7
	xch A, SP
	; fall through to xchg_dptrs

xchg_dptrs:
	xch A, DPL2
	xch A, DPL
	xch A, DPL2
	xch A, DPH2
	xch A, DPH
	xch A, DPH2
	ret

xchg_dptr3:
	xch A, DPL3
	xch A, DPL
	xch A, DPL3
	xch A, DPH3
	xch A, DPH
	xch A, DPH3
	ret

load_blk_ptr:
	movx A, @DPTR
	mov TX_CNT, A
	inc DPTR
	jz load_blk_ptr_eof
	movx A, @DPTR
	mov DPH3, A
	inc DPTR
	movx A, @DPTR
	mov DPL3, A
	inc DPTR
	lcall xchg_dptr3
	ret
load_blk_ptr_eof:
	clr A
	mov DPL, A
	mov DPH, A
	ret

load_byte:
	mov A, DPL
	orl A, DPH
	jnz load_byte_not_eof
	; EOF, return padding.
	clr A
	ret
load_byte_not_eof:
	mov A, TX_CNT
	jnz load_byte_not_next_block
	; load next block ptr
	lcall xchg_dptr3
	lcall load_blk_ptr
	; retry
	sjmp load_byte
load_byte_not_next_block:
	dec TX_CNT
	mov A, DPL
	anl A, DPH
	inc A
	jz load_byte_cnt_zeroes
	movx A, @DPTR
	inc DPTR
load_byte_cnt_zeroes:
	ret

chipreg_read:
	mov RA_AD_RD, A
	sjmp chipreg_wait
chipreg_write:
	mov RA_AD_WR, A
chipreg_wait:
	jnb RA_DONE.0, chipreg_wait
	setb RA_DONE.0
	ret

packet_sg_loadquad:
	lcall load_byte
	mov RA_DA3, A
	lcall load_byte
	mov RA_DA2, A
	lcall load_byte
	mov RA_DA1, A
	lcall load_byte
	mov RA_DA0, A

packet_sg_writequad:
	mov A, #0x20
	add A, R0
	mov RA_BLK, A ; Block 1, subblock n
	mov A, #0xc0 ; CPUTXDAT
	sjmp chipreg_write

packet_sg_tx:
	mov A, TX_PORT
	anl A, #0x7
	mov R0, A
	acall serial_hex
	; send length (+ 4 bytes for FCS)
	mov A, R2
	jnz packet_sg_tx_nopad
	mov A, R1
	clr C
	subb A, #64
	jnc packet_sg_tx_nopad
	mov R1, #64
packet_sg_tx_nopad:
	mov A, R1
	add A, #4
	mov R1, A
	mov RA_DA2, A
	mov A, R2
	addc A, #0
	mov R2, A
	mov RA_DA3, A
	clr A
	mov RA_DA1, A
	mov RA_DA0, A
	lcall packet_sg_writequad

	; send signature
	clr A
	mov RA_DA3, A
	mov RA_DA2, A
	mov RA_DA1, #0x05
	mov RA_DA0, #0x20
	lcall packet_sg_writequad

	; round up to multiples of 8
	mov A, R1
	add A, #7
	mov R1, A
	clr A
	addc A, R2
	mov R2, A
	mov A, R1
	anl A, #0xf8
	mov R1, A

	; send data
	lcall load_blk_ptr
packet_sg_tx_loop:
	lcall packet_sg_loadquad
	lcall packet_sg_loadquad
	mov A, R1
	clr C
	subb A, #8
	mov R1, A
	mov A, R2
	subb A, #0
	mov R2, A
	orl A, R1
	jnz packet_sg_tx_loop

	; check status
	mov A, #0x20
	add A, R0
	mov RA_BLK, A ; Block 1, subblock n
	mov A, #0xc8 ; MISCSTAT
	lcall chipreg_read
	mov A, RA_DA1
	lcall serial_hex
	mov A, RA_DA0
	lcall serial_hex

	; start transmission
	clr A
	mov RA_DA3, A
	mov RA_DA2, A
	mov RA_DA1, A
	mov RA_DA0, #1
	mov A, #0x20
	add A, R0
	mov RA_BLK, A ; Block 1, subblock n
	mov A, #0xc4 ; MISCFIFO
	ljmp chipreg_write

serial_sg_puts:
	lcall load_blk_ptr
serial_sg_loop:
	lcall load_byte
	jz serial_sg_end
	lcall serial_write
	sjmp serial_sg_loop
serial_sg_end:
	ret

checksum_sg_iphdr:
	clr A
	mov R4, A
	mov R5, A
	lcall load_blk_ptr

	; asssumes byte count is even
checksum_sg_iphdr_loop:
	lcall load_byte

	mov B, A
	mov A, R5
	add A, B
	mov R5, A
	clr A
	addc A, R4
	mov R4, A

	; swap high & low checksum
	xch A, R4
	xch A, R5
	xch A, R4

	mov A, R1
	dec A
	mov R1, A
	jnz checksum_sg_iphdr_loop

checksum_sg_iphdr_end:
	xch A, R4
	cpl A
	xch A, R4
	xch A, R5
	cpl A
	xch A, R5
	ret

compare_bytes:
	; compare R3 bytes from DPTR against DPTR2
	; returns: Z clear if equal, Z set if different
	lcall load_blk_ptr
compare_loop:
	lcall load_byte
	lcall xchg_dptrs
	mov B, A
	movx A, @DPTR
	inc DPTR
	clr C
	subb A, B
	lcall xchg_dptrs
	jnz compare_fail
	dec R3
	mov A, R3
	jnz compare_loop
compare_fail:
	ret

copy_bytes:
	; copy R3 bytes from intmem DPTR2 to DPTR
	lcall xchg_dptrs
	movx A, @DPTR
	inc DPTR
	lcall xchg_dptrs
	movx @DPTR, A
	inc DPTR
	dec R3
	mov A, R3
	jnz copy_bytes
	ret

recv_copy:
	; copy R3 bytes from quad R2 ofs R0 into intmem @R1
	mov A, #0x80
	mov RA_BLK, A ; Block 4, subblock 0
recv_copy_next_quad:
	mov A, R2
	inc R2
	lcall chipreg_read
	mov R4, RA_DA3
	mov R5, RA_DA2
	mov R6, RA_DA1
	mov R7, RA_DA0
	mov A, R0
	anl A, #3
	add A, #4
	mov R0, A

recv_copy_loop:
	mov A, @R0
	movx @DPTR, A
	inc R0
	inc DPTR
	dec R3
	mov A, R3
	jz recv_copy_done
	mov A, R0
	anl A, #7
	jnz recv_copy_loop
	mov R0, A
	sjmp recv_copy_next_quad

recv_copy_done:
	ret

recv_poll:
	mov A, #0xe0
	mov RA_BLK, A ; Block 7, subblock 0
	mov A, #0x31 ; CAPCTRL
	lcall chipreg_read

	mov A, RA_DA3
	lcall serial_hex
	mov A, RA_DA2
	lcall serial_hex
	mov A, RA_DA1
	lcall serial_hex
	mov A, RA_DA0
	lcall serial_hex

	mov A, RA_DA3
	anl A, #0x40 ; Queue 0 ready?
	jnz recv_loop
	ret

recv_loop:
	mov DPTR, #message_crnl
	lcall serial_puts
	mov DPTR, #message_rx
	lcall serial_puts

	; Get internal frame header
	mov R2, #0 ; internal header byte 0-
	mov R0, #0
	mov R3, #8 ; copy 8 bytes
	mov DPTR, #recv_int_hdr
	lcall recv_copy

	mov DPTR, #(recv_int_hdr + 3)
	movx A, @DPTR
	anl A, #0x7
	mov TX_PORT, A ; Override tx port to be the rx port
	lcall serial_hex

	; Get original src mac
	mov R2, #3 ; frames bytes 6-
	mov R0, #2
	mov R3, #6
	mov DPTR, #dest_mac
	lcall recv_copy

	mov R2, #5 ; frames bytes 12-
	mov R0, #0
	mov R3, #26
	mov DPTR, #rx_buffer
	lcall recv_copy

	mov DPTR, #rx_buffer
	mov R3, #10
	lcall serial_hex_mem

	mov DPTR, #rx_buffer
	lcall xchg_dptrs
	mov DPTR, #sg_arp_request_header
	mov R3, #9
	lcall compare_bytes
	jz recv_arp

	mov DPTR, #rx_buffer
	lcall xchg_dptrs
	mov DPTR, #sg_ipv4_preamble
	mov R3, #3
	lcall compare_bytes
	jnz recv_exit
	ljmp recv_ip

recv_exit:
recv_release:
	; release packet
	mov A, #0x84
	mov RA_BLK, A ; Block 4, subblock 4
	clr A ; CAPREADP
	lcall chipreg_write

	mov A, #3 ; CAPWRP
	lcall chipreg_write
	mov A, RA_DA2
	anl A, #0x02 ; another frame ready?
	jnz recv_loop
	ret

recv_arp:
	; Got ARP frame!
	mov DPTR, #message_arp
	lcall serial_puts

	mov DPTR, #(rx_buffer + 9)
	movx A, @DPTR
	add A, #-2 ; check against reply opcode
	jz recv_arp_ip64_reply
	inc A ; check against request opcode
	jnz recv_arp_exit_hoop

	mov R2, #7 ; frames bytes 22-
	mov R0, #2
	mov R3, #20
	mov DPTR, #rx_buffer
	lcall recv_copy

	mov DPTR, #(rx_buffer + 16)
	lcall xchg_dptrs
	mov DPTR, #sg_my_ip
	mov R3, #4
	lcall compare_bytes
	jnz recv_arp_exit_hoop

	; ARP for me!

	; Copy mac/ip of requesting host.
	mov DPTR, #rx_buffer
	lcall xchg_dptrs
	mov DPTR, #dest_mac_ip
	mov R3, #10
	lcall copy_bytes

	; send ARP reply
	mov DPTR, #sg_arp_reply_packet
	mov R1, #46
	mov R2, #0
	lcall packet_sg_tx
recv_arp_exit_hoop:
	ljmp recv_arp_exit

recv_arp_ip64_reply:
	mov R2, #7 ; frames bytes 22-
	mov R0, #2
	mov R3, #20
	mov DPTR, #rx_buffer
	lcall recv_copy

	mov DPTR, #(rx_buffer + 6)
	lcall xchg_dptrs
	mov DPTR, #sg_tftp_ip
	mov R3, #4
	lcall compare_bytes
	jnz recv_arp_exit

	; tftp server ARP reply for me!

	mov DPTR, #message_arp_reply
	lcall serial_puts

	; Copy mac of responding host.
	mov DPTR, #rx_buffer
	lcall xchg_dptrs
	mov DPTR, #tftp_mac
	mov R3, #10
	lcall copy_bytes

	mov DPTR, #rx_buffer
	mov R3, #6
	lcall serial_hex_mem

	orl BOOTP, #0x40  ; Mark TFTP ARP as done

recv_arp_exit:
	ljmp recv_release

recv_ip:
	; Got IPv4 frame!
	mov DPTR, #message_ipv4
	lcall serial_puts

	mov DPTR, #(rx_buffer + 11)
	movx A, @DPTR
	lcall serial_hex
	movx A, @DPTR
	cjne A, #1, recv_not_ip_icmp

recv_ip_icmp:
	mov DPTR, #(rx_buffer + 4)
	movx A, @DPTR
	lcall serial_hex
	movx A, @DPTR
	jz recv_ip_icmp_not_too_big
	ljmp recv_ip_icmp_too_big
recv_ip_icmp_not_too_big:
	inc DPTR
	movx A, @DPTR
	lcall serial_hex
	movx A, @DPTR
	mov IP_LEN, A
	clr C
	subb A, #100
	jc recv_ip_icmp_not_too_big2
	ljmp recv_ip_icmp_too_big
recv_ip_icmp_not_too_big2:

	; Copy IP packet to rx buffer
	mov A, IP_LEN
	mov R3, A
	mov R2, #5 ; frames bytes 14-
	mov R0, #2
	mov DPTR, #rx_buffer
	lcall recv_copy

	; echo the mirrored ICMP packet back at host
	mov DPTR, #sg_ipv4_icmp_reply_hack
	mov A, IP_LEN
	add A, #14
	mov R1, A
	mov R2, #0
	lcall packet_sg_tx
	ljmp recv_release

recv_not_ip_udp_hoop:
	ljmp recv_not_ip_udp
recv_not_ip_udp_0to255_hoop:
	ljmp recv_not_ip_udp_0to255
recv_not_ip_udp_bootp_hoop:
	ljmp recv_not_ip_udp_bootp
recv_not_ip_udp_bootp_reply_hoop:
	ljmp recv_not_ip_udp_bootp_reply

recv_not_ip_icmp:
	cjne A, #0x11, recv_not_ip_udp_hoop

	; Copy sport/dport
	mov DPTR, #(rx_buffer + 22)
	lcall xchg_dptrs
	mov DPTR, #udp_reply_header
	mov R3, #4
	lcall copy_bytes

	mov DPTR, #(rx_buffer + 24)
	movx A, @DPTR
	mov R1, A
	lcall serial_hex
	inc DPTR
	movx A, @DPTR
	mov R2, A
	lcall serial_hex

	; check destination port, is it 31c3?
	cjne R1, #0x31, recv_not_ip_udp_31c3
	cjne R2, #0xc3, recv_not_ip_udp_31c3

	ljmp recv_ip_udp_31c3

recv_not_ip_udp_31c3:
	; check destination port, is it 68?
	cjne R1, #0, recv_not_ip_udp_0to255_hoop
	cjne R2, #68, recv_not_ip_udp_bootp_hoop

	mov DPTR, #message_bootp
	lcall serial_puts

	mov R2, #12 ; frames bytes 42-
	mov R0, #2
	mov R3, #128
	mov DPTR, #rx_buffer
	lcall recv_copy

	mov DPTR, #rx_buffer
	movx A, @DPTR
	cjne A, #2, recv_not_ip_udp_bootp_reply_hoop

	; Copy new ip.
	mov DPTR, #(rx_buffer + 16) ; my new ip
	lcall xchg_dptrs
	mov DPTR, #my_ip
	mov R3, #4
	lcall copy_bytes

	; Copy tftp ip.
	mov DPTR, #(rx_buffer + 20) ; my new ip
	lcall xchg_dptrs
	mov DPTR, #tftp_ip
	mov R3, #4
	lcall copy_bytes

	orl BOOTP, #0x80  ; Mark BOOTP as done
	ljmp recv_release

recv_not_ip_udp_bootp:
	cjne R2, #69, recv_not_ip_udp_tftp

	mov DPTR, #message_tftp
	lcall serial_puts

	mov R2, #12 ; frames bytes 42-
	mov R0, #2
	mov R3, #4
	mov DPTR, #rx_buffer
	lcall recv_copy

	mov DPTR, #rx_buffer
	movx A, @DPTR
	lcall serial_hex
	movx A, @DPTR
	cjne A, #0, recv_not_ip_udp_tftp
	inc DPTR
	movx A, @DPTR
	lcall serial_hex
	movx A, @DPTR
	cjne A, #5, recv_not_tftp_notfound

	orl BOOTP, #0x20  ; Stop sending TFTP requests
	ljmp recv_release

recv_not_tftp_notfound:
	cjne A, #3, recv_not_tftp_data

	; Jackpot, we got data incoming!
	orl BOOTP, #0x20  ; Stop sending TFTP requests

	mov R2, #13 ; frames bytes 46-
	mov R0, #2
	mov R3, #224
	mov DPTR, #(rx_buffer + 128)
	lcall recv_copy
	mov DPTR, #(rx_buffer + 128)
	mov R3, #224
	lcall serial_hex_mem

	mov R2, #69 ; frames bytes 270-
	mov R0, #2
	mov R3, #224
	mov DPTR, #(rx_buffer + 128 + 224)
	lcall recv_copy
	mov DPTR, #(rx_buffer + 128 + 224)
	mov R3, #224
	lcall serial_hex_mem

	mov R2, #125 ; frames bytes 494-
	mov R0, #2
	mov R3, #(512 - 224 - 224)
	mov DPTR, #(rx_buffer + 128 + 224 + 224)
	lcall recv_copy
	mov DPTR, #(rx_buffer + 128 + 224 + 224)
	mov R3, #(512 - 224 - 224)
	lcall serial_hex_mem

recv_not_tftp_data:
recv_not_ip_udp_tftp:
recv_not_ip_udp_bootp_reply:
recv_not_ip_udp_0to255:
recv_not_ip_udp:
recv_ip_icmp_too_big:
	ljmp recv_exit

recv_ip_udp_31c3:
	mov R2, #12 ; frames bytes 42-
	mov R0, #2
	mov R3, #224
	mov DPTR, #(RAM_END - 512)
	lcall recv_copy

	mov R2, #68 ; frames bytes 266-
	mov R0, #2
	mov R3, #224
	mov DPTR, #(RAM_END - 512 + 224)
	lcall recv_copy

	mov DPTR, #(RAM_END - 512)
	movx A, @DPTR
	cjne A, #0x55, recv_ip_udp_31c3_magic_mismatch_exit
	inc DPTR
	movx A, @DPTR
	cjne A, #0xaa, recv_ip_udp_31c3_magic_mismatch2

	lcall (RAM_END - 512 + 2)
	ljmp recv_exit

recv_ip_udp_31c3_magic_mismatch2:
	cjne A, #'R', recv_ip_udp_31c3_magic_mismatch3

	inc DPTR
	movx A, @DPTR
	lcall serial_hex
	movx A, @DPTR
	mov R0, A
	inc DPTR
	movx A, @DPTR
	anl A, #0xe0 ; align to multiple of 32 bytes
	lcall serial_hex
	movx A, @DPTR
	anl A, #0xe0 ; align to multiple of 32 bytes
	mov R1, A
	mov R2, #32 ; read 32 bytes at a time
	mov DPTR, #rx_buffer
	lcall eeprom_read

	clr A
	mov DPTR, #ipv4_header_cksum
	movx @DPTR, A
	inc DPTR
	movx @DPTR, A

	mov DPTR, #sg_ipv4_udp_reply_ip_hdr
	mov R1, #20
	lcall checksum_sg_iphdr

	mov DPTR, #ipv4_header_cksum
	mov A, R5
	movx @DPTR, A
	inc DPTR
	mov A, R4
	movx @DPTR, A

	mov DPTR, #sg_ipv4_udp_reply_packet
	mov R1, #86
	mov R2, #1
	lcall packet_sg_tx
	ljmp recv_exit

recv_ip_udp_31c3_magic_mismatch3:
recv_ip_udp_31c3_magic_mismatch_exit:
	ljmp recv_exit

maybe_send_bootp:
	inc BOOTP_TIMER
	mov A, BOOTP_TIMER
	jnz dont_send_bootp

	mov A, BOOTP
	anl A, #0x80
	jz do_send_bootp

	mov A, BOOTP
	anl A, #0x40
	jz do_send_arp

	mov A, BOOTP
	anl A, #0x20
	jz do_send_tftp

dont_send_bootp:
	ret

do_send_bootp:
	mov DPTR, #sg_bootp_request_packet
	mov R1, #86
	mov R2, #1
	ljmp packet_sg_tx

do_send_arp:
	mov DPTR, #sg_tftp_arp_request_packet
	mov R1, #46
	mov R2, #0
	ljmp packet_sg_tx

do_send_tftp:
	clr A
	mov DPTR, #ipv4_header_cksum
	movx @DPTR, A
	inc DPTR
	movx @DPTR, A

	mov DPTR, #sg_tftp_rrq_ip_hdr
	mov R1, #20
	lcall checksum_sg_iphdr

	mov DPTR, #ipv4_header_cksum
	mov A, R5
	movx @DPTR, A
	inc DPTR
	mov A, R4
	movx @DPTR, A

	mov DPTR, #sg_tftp_rrq_packet
	mov R1, #86
	mov R2, #1
	ljmp packet_sg_tx

send_test_packet:
	mov DPTR, #sg_arp_reply_packet
	mov R1, #46
	mov R2, #0
	ljmp packet_sg_tx

setup_link_state: ; r0 mii addr
	mov R1, #0x1c ; phy aux control & status
	              ; bit15: autneg complete
	              ; bit5: full-duplex
	              ; bit4-3: 0=>10base-t, 1=>100base-t, 2=>1000base-t
	lcall mii_read

	mov A, RA_DA1
	anl A, #0x80  ; autoneg ok?
	jnz not_link_down

	; link down
	mov RA_DA3, #0x20
	clr A
	mov B, A
	mov RA_DA2, A
	mov RA_DA1, A
	mov RA_DA0, #0x30
	mov A, #'d'
	lcall serial_write
	sjmp write_mac_config

not_link_down:
	mov A, RA_DA0
	mov RA_DA3, #0x10
	mov B, A
	anl A, #0x18
	cjne A, #0x10, link_not_gige

link_gige:
	mov A, #'G'
	lcall serial_write
	mov RA_DA2, #0x07
	mov RA_DA1, #0xc1
	mov RA_DA0, #0x84
	ajmp write_mac_config

link_not_gige:
	mov RA_DA1, #0xc4
	mov RA_DA0, #0x44

	mov A, B
	anl A, #0x20
	jnz link_fullduplex

	mov A, #'H'
	lcall serial_write
	mov RA_DA3, #0x90
	mov RA_DA2, #0x01
	sjmp write_mac_config

link_fullduplex:
	mov A, #'F'
	lcall serial_write
	mov RA_DA2, #0x05

write_mac_config:
	mov A, #0x20  ; mac block
	orl A, R0
	mov RA_BLK, A
	clr A ; MACCONF
	lcall chipreg_write

	; enable crc update for cpu tx
	clr A
	mov RA_DA3, A
	mov RA_DA2, A
	mov RA_DA1, A
	mov RA_DA0, #2
	mov A, #0x24 ; TXUPDCFG
	lcall chipreg_write

	mov A, R0
	inc A
	mov R7, A
	clr A
	setb C
write_recvmask_shift_bit:
	rlc A
	djnz R7, write_recvmask_shift_bit

	xch A, B
	orl A, #0
	jz write_recvmask_port_down
	mov A, R6
	orl A, B
	mov R6, A
	sjmp write_recvmask_port_up
write_recvmask_port_down:
	mov A, R6
	cpl A
	orl A, B
	cpl A
	mov R6, A
write_recvmask_port_up:

	mov RA_DA0, A
	clr A
	mov RA_DA3, A
	mov RA_DA2, A
	mov RA_DA1, A
	mov RA_BLK, #0x40  ; frame analyzer block, subblock 0
	mov A, #0x10 ; RECVMASK
	ljmp chipreg_write

mii_cmd: ; r0 mii addr, r1 mii reg, r2 flag, RA_DA0/RA_DA1 data
	mov A, R0
	lcall mii_xlate
	rr A
	rr A
	rr A
	mov B, A
	anl A, #0xe0
	orl A, R1
	mov RA_DA2, A
	mov A, B
	anl A, #0x03
	orl A, R2
	mov RA_DA3, A
	mov RA_BLK, #0x60   ; mii bus block
	mov A, #0x01 ; MIIMCMD
	ljmp chipreg_write

mii_xlate:
	mov B, A
	mov A, #7
	clr C
	subb A, B
	jnc mii_do_xlate
	mov A, B
	ret
mii_do_xlate:
	add A, #(mii_xlate_table - mii_cmd_movc_pc)
	movc A, @A + PC
mii_cmd_movc_pc:
	ret

mii_xlate_table:
	.byte 3, 5, 7, 6, 4, 2, 1, 0

mii_wait:
	mov R7, #64
mii_read_wait_ready:
	mov RA_BLK, #0x60   ; mii bus block
	clr A ; MIISTAT
	lcall chipreg_read
	mov A, RA_DA0
	anl A, #0xb
	jz mii_read_wait_exit
	djnz R7, mii_read_wait_ready
mii_read_wait_exit:
	ret

mii_write: ; r0 mii addr, r1 mii reg, RA_DA0/RA_DA1 data
	mov R2, #0
	lcall mii_cmd
	ljmp mii_wait

mii_read: ; r0 mii addr, r1 mii reg
	mov R2, #4
	lcall mii_cmd
	lcall mii_wait

	mov RA_BLK, #0x60   ; mii bus block
	mov A, #0x02 ; MIIMDATA
	lcall chipreg_read
	mov C, RA_DA2
	jc mii_read_fail

	ret

mii_read_fail:
	mov DPTR, #message_mii_fail
	ljmp serial_puts

delay_ms:
	lcall delay_1_ms
	dec A
	cjne A, #0, delay_ms
	ret

delay_1_ms:
	mov R4, #(0x6e + 1)
	mov R5, #(0x19 + 1)
delay_1_ms_loop:
	djnz R4, delay_1_ms_loop
	djnz R5, delay_1_ms_loop
	ret

serial_hex_mem:
	movx A, @DPTR
	inc DPTR
	dec R3
	lcall serial_hex
	mov A, R3
	jnz serial_hex_mem
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

serial_puts:
	clr A
	movc A, @A + DPTR
	jz serial_end
	inc DPTR
	lcall serial_write
	sjmp serial_puts
serial_end:
	ret

serial_test:
	clr TI
	mov SBUF, #0x5a
serial_test_wait:
	jnb TI,serial_test_wait
	sjmp serial_test

eeprom_read:
	lcall spi_read_state
	lcall spi_cs_low
	mov A, #3 ; EEPROM read cmd
	lcall spi_xfer
	mov A, R0
	lcall spi_xfer
	mov A, R1
	lcall spi_xfer
eeprom_read_loop:
	clr A
	lcall spi_xfer
	movx @DPTR, A
	lcall serial_hex
	inc DPTR
	djnz R2, eeprom_read_loop
	sjmp spi_cs_high

spi_xfer:
	mov R7, A
	mov R6, #8
spi_xfer_loop:
	setb SPI_STATE.0
	mov A, R7
	jb ACC.7, spi_xfer_do_bit_set
	clr SPI_STATE.0
spi_xfer_do_bit_set:
	lcall spi_update_state
	; clk high
	setb SPI_STATE.2
	lcall spi_update_state
	setb C
	jb SPI_STATE.4, spi_xfer_di_bit_set
	clr C
spi_xfer_di_bit_set:
	mov A, R7
	rlc A
	mov R7, A
	; clk low
	clr SPI_STATE.2
	lcall spi_update_state
	djnz R6, spi_xfer_loop
	mov A, R7
	ret

spi_cs_high:
	setb SPI_STATE.1 ; Set nCS high
	lcall spi_update_state ; Seperate from disable to actively drive it high
	clr SPI_STATE.3 ; Tristate pins
	sjmp spi_update_state

spi_cs_low:
	clr SPI_STATE.1 ; Set nCS low
	setb SPI_STATE.3 ; Enable pins

spi_update_state:
	mov A, SPI_STATE
	mov RA_DA0, A
	mov RA_BLK, #0xe0
	mov A, #0x35 ; SIMASTER
	lcall chipreg_write

spi_read_state:
	mov RA_BLK, #0xe0
	mov A, #0x35
	lcall chipreg_read
	mov A, RA_DA0
	mov SPI_STATE, A
	ret

halt:
	sjmp halt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Runtime data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.byte "DATA"

sg_ipv4_preamble:
.byte 3
.word ipv4_preamble
.byte 0

sg_arp_request_header:
.byte 8
.word arp_preamble
.byte 0

sg_bootp_reply_payload:
.byte 1
.word bootp_reply_type
.byte 11
.word bootp_proto_header+1
.byte 0

tftp_udp_rrq_header:
.word 69 ; sport
.word 69 ; dport
.word tftp_udp_rrq_request_len
.word 0 ; checksum (disabled)
.word 1 ; tftp RRQ
.byte "vsc7388-v1", 0
.byte "octet", 0
tftp_udp_rrq_request_end:

sg_tftp_rrq_ip_hdr:
.byte 12
.word ipv4_header
.byte 4
.word my_ip
.byte 4
.word tftp_ip

sg_tftp_rrq_packet:
.byte 6
.word tftp_mac
.byte 6
.word my_mac
.byte 14
.word ipv4_preamble
.byte 4
.word my_ip
.byte 4
.word tftp_ip
.byte tftp_udp_rrq_request_len
.word tftp_udp_rrq_header
.byte 0

bootp_udp_header:
.word 68 ; sport
.word 67 ; dport
.word 308 ; length
.word 0 ; checksum (disabled)

sg_bootp_request_packet:
.byte 6
.word brcast_mac
.byte 6
.word my_mac
.byte 14
.word ipv4_preamble
.byte 4
.word 0xffff
.byte 4
.word brcast_ip
.byte 8
.word bootp_udp_header
.byte 12
.word bootp_proto_header
.byte 4
.word my_ip
.byte 12
.word 0xffff
.byte 6
.word my_mac
.byte 0

sg_tftp_arp_request_packet:
.byte 6
.word tftp_mac
.byte 6
.word my_mac
.byte 8
.word arp_preamble
.byte 2
.word arp_opcode_request
.byte 10
.word my_mac_ip
.byte 6
.word 0xffff
.byte 4
.word tftp_ip
.byte 0

sg_arp_reply_packet:
.byte 6
.word dest_mac
.byte 6
.word my_mac
.byte 10
.word arp_preamble
.byte 10
.word my_mac_ip
.byte 10
.word dest_mac_ip
.byte 0

sg_ipv4_icmp_reply_hack:
.byte 6
.word dest_mac
.byte 6
.word my_mac
.byte 2
.word ipv4_preamble
.byte 12
.word rx_buffer
.byte 4
.word rx_buffer + 16 ; swapping dst and src should leave checksum unchanged
.byte 4
.word rx_buffer + 12
.byte 100 - 20
.word rx_buffer + 20 ; payload
.byte 0

udp_reply_header:
udp_dport:
.word 0x31c3 ; sport
udp_sport:
.word 1234 ; dport
udp_length:
.word 308 ; length
.word 0 ; checksum (disabled)

sg_ipv4_udp_reply_ip_hdr:
.byte 12
.word ipv4_header
.byte 4
.word my_ip
.byte 4
.word dest_ip

sg_ipv4_udp_reply_packet:
.byte 6
.word dest_mac
.byte 6
.word my_mac
.byte 14
.word ipv4_preamble
.byte 4
.word my_ip
.byte 4
.word dest_ip
.byte 2
.word udp_sport
.byte 2
.word udp_dport
.byte 4
.word udp_length
sg_ipv4_udp_reply_payload_len:
.byte 224
.word rx_buffer
.byte 0

sg_my_ip:
.byte 4
.word my_ip
.byte 0

sg_tftp_ip:
.byte 4
.word tftp_ip
.byte 0

ipv4_preamble:
.word 0x0800
ipv4_header:
.byte 0x45
.byte 0x00
.word 328
.word 0
.byte 0x40
.byte 0
.byte 64
.byte 17
ipv4_header_cksum:
.word 0x39a6

bootp_reply_type:
.byte 2
bootp_proto_header:
.byte 1, 1, 6, 0
.byte "BOOT" ; transaction id
.word 0, 0

arp_preamble:
.word 0x0806
arp_opcode_request:
.word 0x0001, 0x0800
.byte 6, 4
arp_opcode_reply:
.word 0x0002

brcast_mac:
brcast_ip:
.byte 0xff, 0xff, 0xff, 0xff, 0xff, 0xff
my_mac_ip:
my_mac:
.byte 0x00, 0x01, 0x5c, 0x5a, 0xa5, 0x41
my_ip:
.byte 192, 168, 0, 1
dest_mac_ip:
dest_mac:
.byte 0xb8, 0x22, 0x33, 0x44, 0x56, 0x6c
dest_ip:
.byte 192, 168, 0, 2
tftp_mac_ip:
tftp_mac:
.byte 0xff, 0xff, 0xff, 0xff, 0xff, 0xff
tftp_ip:
.byte 0, 0, 0, 0

recv_int_hdr:
.word 0, 0, 0, 0

message_crnl:
.byte "\r\n", 0

message_rx:
.byte " RX: ", 0
message_arp:
.byte " ARP ", 0
message_arp_reply:
.byte " REPLY ", 0
message_ipv4:
.byte " IP ", 0
message_bootp:
.byte " BOOTP ", 0
message_tftp:
.byte " TFTP ", 0
message_mii_read:
.byte " Read MII ", 0
message_mii_fail:
.byte " failed\r\n", 0


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Buffer space for network RX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
rx_buffer:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Discardable but not explicitly cleared
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
message_init_done:
.byte "INITDONE!\r\n", 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Discardable initcode and initdata after this point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.byte "INIT"

system_init:
	setb GPIO_OE.1
	clr GPIO_OUT.1

	mov sp, #MAIN_STACK

	mov RA_BLK, #0xe0   ; system block
	mov A, #0x10 ; ICPU_CTRL
	lcall chipreg_read

	mov A, RA_DA3
	mov RA_DA3, A
	mov A, RA_DA2
	mov RA_DA2, A
	mov A, RA_DA1
	anl A, #0xe0
	orl A, #0x01 ; 156.25MHz / 2
	mov RA_DA1, A
	mov A, RA_DA0
	mov RA_DA0, A
	mov A, #0x10 ; ICPU_CTRL
	lcall chipreg_write

	mov RCAP2H, #0xff
	mov RCAP2L, #0xeb ; 115200 baud @ div 2 78.125MHz
	mov T2CON, #0x34
	mov SCON, #0x52
	mov TH2, #0xff
	mov TL2, #0xff

	mov DPTR, #message_hello
	lcall serial_puts

	mov R1, #20
startup_delay:
	mov A, #100
	lcall delay_ms
	djnz R1, startup_delay

	; enable xram mapping at address 0 for convenience.
	clr A
	mov RA_DA3, A
	mov RA_DA2, A
	mov RA_DA1, A
	mov RA_DA0, #0xd1   ; map intram to 0x0000, 0x8000, 0xc000, 0xe000
	mov RA_BLK, #0xe0   ; system block
	mov A, #0x1b ; ICPU_RAM_MAP
	lcall chipreg_write

	mov R2, #0x10
meminit_loop:
	mov RA_DA3, #1
	mov RA_DA2, #1
	mov RA_DA1, #4
	mov A, #0x10
	clr C
	subb A, R2
	cjne A, #6, meminit_not6
	djnz R2, meminit_loop
meminit_not6:
	cjne A, #7, meminit_not7
	djnz R2, meminit_loop
meminit_not7:
	mov RA_DA0, A
	mov RA_BLK, #0x62  ; block 3, subblock 2 (MEMINIT)
	mov A, #0    ; MEMINIT
	lcall chipreg_write
	mov A, #1
	lcall delay_ms
	djnz R2, meminit_loop

	mov A, #30
	lcall delay_ms

	clr A
	mov RA_DA3, A
	mov RA_DA2, A
	mov RA_DA1, A
	mov RA_DA0, #5
	mov RA_BLK, #0x40  ; frame analyzer block, subblock 0
	mov A, #0xb0 ; MACACCESS
	lcall chipreg_write

	clr A
	mov RA_DA3, A
	mov RA_DA2, A
	mov RA_DA1, A
	mov RA_DA0, #3
	mov RA_BLK, #0x40  ; frame analyzer block, subblock 0
	mov A, #0xd0 ; VLANACCESS
	lcall chipreg_write

	mov A, #40
	lcall delay_ms

	; lock own MAC address in MAC table
	mov RA_BLK, #0x40  ; frame analyzer block, subblock 0
	mov DPTR, #my_mac
	clr A
	mov RA_DA3, A ; vid 0
	mov RA_DA2, A ; vid 0
	movc A, @A + DPTR
	inc DPTR
	mov RA_DA1, A
	clr A
	movc A, @A + DPTR
	inc DPTR
	mov RA_DA0, A
	mov A, #0x06 ; MACHDATA
	lcall chipreg_write

	clr A
	movc A, @A + DPTR
	inc DPTR
	mov RA_DA3, A
	clr A
	movc A, @A + DPTR
	inc DPTR
	mov RA_DA2, A
	clr A
	movc A, @A + DPTR
	inc DPTR
	mov RA_DA1, A
	clr A
	movc A, @A + DPTR
	inc DPTR
	mov RA_DA0, A
	mov A, #0x07 ; MACHDATA
	lcall chipreg_write

	clr A
	mov RA_DA3, A
	mov RA_DA2, A
	mov RA_DA1, #0x56 ; CPU_COPY | IGNORE_VLAN | VALID | LOCKED
	mov RA_DA0, #1 ; Learn MAC
	mov A, #0xb0 ; MACACCESS
	lcall chipreg_write

	mov RA_BLK, #0xe0   ; system block
	mov A, #0x14 ; GLORESET
	lcall chipreg_read

	mov A, RA_DA3
	mov RA_DA3, A
	mov A, RA_DA2
	mov RA_DA2, A
	mov A, RA_DA1
	mov RA_DA1, A
	mov A, RA_DA0
	orl A, #0x02
	mov RA_DA0, A
	mov A, #0x14 ; GLORESET
	lcall chipreg_write

	mov A, #1
	lcall delay_ms

	clr A
	mov RA_DA3, A
	mov RA_DA2, A
	mov RA_DA1, A
	mov RA_DA0, #0xff
	mov RA_BLK, #0x40  ; block 2, subblock 0
	mov A, #0x04 ; IFLODMSK  ; multicast flooding
	lcall chipreg_write

	; Enable capture of ARP packets
	clr A
	mov RA_DA3, A
	mov RA_DA2, #0x08
	mov RA_DA1, A
	mov RA_DA0, A
	mov RA_BLK, #0x40  ; block 2, subblock 0
	mov A, #0xa0 ; CAPENAB
	lcall chipreg_write

; phy dsp optimization
	mov R0, #0
repeat_dsp_outer:
	mov DPTR, #phy_dsp_init_data

repeat_dsp_inner:
	clr A
	mov R2, A
	mov R3, A
	movc A, @A + DPTR
	inc DPTR
	mov R1, A
	anl A, #0x1f
	xch A, R1
	add A, #1
	jc phy_dsp_one_done
	dec A
	anl A, #0x80
	jz phy_dsp_simple_write

phy_dsp_masked_write:
	lcall mii_read
	clr A
	movc A, @A + DPTR
	inc DPTR
	anl A, RA_DA1
	xch A, R2 ; clears A as side-effect since R2 is 0
	movc A, @A + DPTR
	inc DPTR
	anl A, RA_DA0
	mov R3, A

phy_dsp_simple_write:
	clr A
	movc A, @A + DPTR
	inc DPTR
	orl A, R2
	mov RA_DA1, A
	clr A
	movc A, @A + DPTR
	inc DPTR
	orl A, R3
	mov RA_DA0, A
	lcall mii_write
	sjmp repeat_dsp_inner

phy_dsp_one_done:
	inc R0
	cjne R0, #0x08, repeat_dsp_outer

	mov DPTR, #message_eeprom
	lcall serial_puts

	mov DPTR, #(0xe000 + my_mac_ip)
	mov R0, #0x1f
	mov R1, #0x75
	mov R2, #(6 + 4)
	lcall eeprom_read

	mov DPTR, #message_crnl
	lcall serial_puts

	clr A
	mov BOOTP, A

	ljmp main

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; initdata
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.byte "INITDATA"

message_hello:
.byte "Hello world!", 0
message_eeprom:
.byte "\r\nRead EEP MAC/IP: ", 0
message_clear_init:
.byte "Clearing initcode/data", 0

phy_dsp_init_data:
; phy setup
.byte 0x1f
.word 0x2a30
.byte 0x88
.word 0xfdff
.word 0x0200
.byte 0x1f
.word 0x52b5
.byte 0x10
.word 0xaf8a
.byte 0x91
.word 0xfff3
.word 0x0008
.byte 0x92
.word 0xffff
.word 0x0000
.byte 0x10
.word 0x8f8a
.byte 0x10
.word 0xaf86
.byte 0x91
.word 0xffff
.word 0x0000
.byte 0x92
.word 0xfff3
.word 0x0008
.byte 0x10
.word 0x8f86
.byte 0x10
.word 0xaf82
.byte 0x91
.word 0xfe7f
.word 0x0100
.byte 0x92
.word 0xffff
.word 0x0000
.byte 0x10
.word 0x8f82
.byte 0x1f
.word 0x2a30
.byte 0x88
.word 0xfdff
.word 0x0000
.byte 0x1f
.word 0x0000
; enable actiphy pwrmgmt
.byte 0x1c
.word 0x0045
; led setup
.byte 0x1b
.word 0x0210
; EOF maker
.byte 0xff

end_of_data:

.equ initlen, (end_of_data - system_init)
.equ tftp_udp_rrq_request_len, (tftp_udp_rrq_request_end - tftp_udp_rrq_header)
