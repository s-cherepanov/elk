EXTRA_DIST = usenix.ms

if HAVE_GROFF
doc_DATA = usenix.ps
docdir = $(datadir)/doc/@PACKAGE@
endif

TROFF= groff -ms
UNROFF= unroff -ms

usenix.ps: usenix.ms tmp.ref
	sed -f tmp.ref usenix.ms | $(TROFF) 2> /dev/null > usenix.ps

usenix.html: usenix.ms tmp.ref
	sed -f tmp.ref usenix.ms | $(UNROFF) document=usenix

tmp.ref: usenix.ms
	$(TROFF) usenix.ms 2> tmp.ref >/dev/null

clean:
	rm -f tmp.ref usenix.ps

