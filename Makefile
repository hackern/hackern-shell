BINARIES=Main
include standard.mk

run: Main
	sudo xl create Hackern.config -c 'extra="count=10"'

clean::
	find . -name "*.hi" -delete
	find . -name "*.o" -delete
	find . -name "*~" -delete
