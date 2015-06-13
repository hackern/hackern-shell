BINARIES=Main
include standard.mk

run: Main
	rm -f new.disk
	dd if=/dev/zero of=new.disk bs=1M count=64
	sudo xl create Hackern.config
	sudo xl dmesg -c

clean::
	find . -name "*.hi" -delete
	find . -name "*.o" -delete
	find . -name "*~" -delete
