# openvsc73xx
Open Firmware for VSC73XX-based switches

This project was started in August 2013 when I found out my old D-Link 8-port
desktop switch using this chipset has a built-in 8051 CPU with 8KiB of built-in
RAM and was intrigued by how much functionality would be possible to cram into
this limited space.

"we do what we must, because we can" - GLaDOS

I still need to to clean up most of the code and tools I've written so far, but
I am putting the current state of the payload into examples/payload.asm.

The original switch firmware:
- Is named "Luton 2.31b" (only string in the fw)
- Is almost 8KiB in size.
- Doesn't do much (dumb switch), but has built-in cable diagnostic on bootup
  (LED will blink if the cable is bad).
- No bootloader (need to unsolder flash to replace).

The custom example payload currently:
- Is only 2.7KiB in size so far (without the bootloader).
- Does basic switch & phy setup.
- No MAC aging implemented (should be really simple, but I had no need for it
  in my experiments).
- Some diagnostic output on the serial console.
- Tries to get an IP using BOOTP (only on one port).
- Basic ARP and (very hacky) ICMP echo, responds to ICMP ping.
- After BOOTP success, tries to get a TFTP image (partially implemented).
- 31C3 addition: Can send udp packet with code to execute to port 31C3.

Tools needed to build:
- as31 (Intel 8031/8051 assembler, I used version 2.3.1)
