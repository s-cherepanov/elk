# SUBDIRS lists the components of Elk that are compiled and installed by
# running "make" and "make install".  The subdirectory "src" holds the
# interpreter proper; a mininum configuration requires the SUBDIRS include,
# scripts, src, and scm.
#
# Subdirectories if lib/ hold the standard extensions.  Delete them or
# parts of them from SUBDIRS if you don't want them to be compiled and
# installed; delete lib/xm and lib/xm/xt if you don't have Motif on your
# system.

SUBDIRS= include\
	 scripts\
	 src\
	 scm\
	 lib/misc\
	 lib/unix\
	 lib/xlib\
	 lib/xt\
	 lib/xaw\
	 lib/xm\
	 lib/xm/xt

# ----------------------------------------------------------------------

SHELL= /bin/sh
MAKE=  make
GTAR=  gtar
TAR=   tar
GZIP=  gzip
ZIP=   zip

default:
	@for i in $(SUBDIRS) ;\
	do \
	    echo Making $$i...; \
	    ( cd $$i ; $(MAKE) ) || exit $$?; \
	done

install:
	@for i in $(SUBDIRS) ;\
	do \
	    echo Installing $$i...; \
	    ( cd $$i ; $(MAKE) install ) || exit $$?; \
	done

localize:
	@for i in $(SUBDIRS) ;\
	do \
	    echo Localizing $$i...; \
	    ( cd $$i ; $(MAKE) localize ) || exit $$?; \
	done

lint:
	@for i in $(SUBDIRS) ;\
	do \
	    echo Linting $$i...; \
	    ( cd $$i ; $(MAKE) lint ) || exit $$?; \
	done

clean:
	@for i in $(SUBDIRS) ;\
	do \
	    echo Cleaning $$i...; \
	    ( cd $$i ; $(MAKE) clean ) || exit $$?; \
	done

distclean:
	@for i in $(SUBDIRS) ;\
	do \
	    echo Cleaning $$i...; \
	    ( cd $$i ; $(MAKE) distclean ) || exit $$?; \
	done


# Package up all localized files (Makefile.local, include files, etc.)
# and source files into a zip file (to be compiled on a DOS system).
# The X11 extensions are not included.

LOCALF= Makefile config/system config/site include/*.h lib/misc/Makefile*\
        lib/misc/*.c scm/[a-z]* src/Makefile* `ls -1 src/*.c |grep -v hp9k`

localized.zip:
	$(MAKE) distclean
	$(MAKE) localize
	$(ZIP) -kr $@ $(LOCALF)


# Make a full distribution

DISTF= README ROADMAP CHANGES INSTALL MACHINES COPYRIGHT CONTRIBUTORS\
       PATCHLEVEL TODO BUGS MIGRATE Makefile config doc examples include lib\
       scm scripts src util

dist:
	echo elk-`util/getversion README'` > .rel
	rm -rf `cat .rel`
	mkdir `cat .rel`
	for i in $(DISTF) ;\
	do \
	    (cd `cat .rel`; ln -s ../$$i) \
	done
	if [ -f config/site.dist ]; then \
	    cp config/site config/site.old; \
	    cp config/site.dist config/site; \
	fi
	if [ ! -f ExcludeFiles ]; then \
	    $(TAR) -cvf `cat .rel`.tar -h `cat .rel`; \
	else \
	    $(GTAR) -cvf `cat .rel`.tar -h -X ExcludeFiles `cat .rel`; \
	fi
	$(GZIP) -f `cat .rel`.tar
	rm -rf `cat .rel` .rel
	if [ -f config/site.old ]; then \
	    mv config/site.old config/site; \
	fi
