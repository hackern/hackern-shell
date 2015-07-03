BINARIES=Main
include standard.mk

run: $(BINARIES)
	dd if=/dev/zero of=VM-1.disk bs=1M count=64
	dd if=/dev/zero of=VM-2.disk bs=1M count=64
	sudo xl create VM-1.config
	sudo xl create VM-2.config
	sleep 1
	sudo xl dmesg -c

clean::
	find . -name "*.hi" -delete
	find . -name "*.o" -delete
	find . -name "*~" -delete
	rm *.disk
