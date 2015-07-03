BINARIES=Main
include standard.mk

run:
	dd if=/dev/zero of=new.disk bs=1M count=64
	sudo xl create Main.config
	sleep 1
	sudo xl dmesg -c

clean::
	find . -name "*.hi" -delete
	find . -name "*.o" -delete
	rm new.disk
