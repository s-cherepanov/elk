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

lint: Makefile.local
	$(MAKE) -f Makefile.local lint

clean: Makefile.local
	$(MAKE) -f Makefile.local clean

distclean: Makefile.local
	$(MAKE) -f Makefile.local distclean
