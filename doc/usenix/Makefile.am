EXTRA_DIST = usenix.ms
CLEANFILES = tmp.ref usenix.ps

if HAVE_GROFF
doc_DATA = usenix.ps
docdir = $(datadir)/doc/@PACKAGE@/ps
endif

MANUAL = $(srcdir)/usenix.ms
TROFF = groff -ms
UNROFF = unroff -ms

usenix.ps: $(MANUAL) tmp.ref
	sed -f tmp.ref $(MANUAL) | $(TROFF) 2> /dev/null > usenix.ps

usenix.html: $(MANUAL) tmp.ref
	sed -f tmp.ref $(MANUAL) | $(UNROFF) document=usenix

tmp.ref: $(MANUAL)
	$(TROFF) $(MANUAL) 2> tmp.ref >/dev/null

