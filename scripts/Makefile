SHELL=/bin/sh
MAKE=make

all: default

Makefile.local: ../config/system ../config/site
	$(SHELL) ./build

default: Makefile.local
	$(MAKE) -f Makefile.local

install: Makefile.local
	$(MAKE) -f Makefile.local install

localize: Makefile.local
	$(MAKE) -f Makefile.local localize

lint:

clean:

distclean: Makefile.local
	$(MAKE) -f Makefile.local distclean
