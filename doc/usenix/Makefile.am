EXTRA_DIST = usenix.ms
CLEANFILES = tmp.ref usenix.ps

if HAVE_GROFF
doc_DATA = usenix.ps
docdir = $(datadir)/doc/@PACKAGE@/ps
endif

TROFF= groff -ms
UNROFF= unroff -ms

usenix.ps: usenix.ms tmp.ref
	sed -f tmp.ref usenix.ms | $(TROFF) 2> /dev/null > usenix.ps

usenix.html: usenix.ms tmp.ref
	sed -f tmp.ref usenix.ms | $(UNROFF) document=usenix

tmp.ref: usenix.ms
	$(TROFF) usenix.ms 2> tmp.ref >/dev/null

