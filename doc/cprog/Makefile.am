EXTRA_DIST = cprog.ms
CLEANFILES = side side.ref side.inx cprog.ps cprog.html

if HAVE_GROFF
doc_DATA = cprog.ps
docdir = $(datadir)/doc/@PACKAGE@/ps
endif

TROFF= groff -ms -t
UNROFF= unroff -ms
MKINDEX= awk -f ../util/mkindex.awk

cprog.ps: cprog.ms side.ref side.inx
	$(MKINDEX) cprog.ms | sed -f side.ref | $(TROFF) 2> /dev/null > $@

side.ref: side
	sed -ne '/^[sS]/{s/\.\//\//;p}' < side > side.ref

side.inx: side
	egrep -v '^s' side | sort -f -t'#' +1 -3 +0n | \
	    awk -f ../util/fixindex.awk | awk -f ../util/block.awk \
	    > side.inx

side: cprog.ms
	rm -f side side.inx && printf "" > side.inx
	$(MKINDEX) $? | $(TROFF) 2>side >/dev/null

cprog.html: cprog.ms side.ref
	$(MKINDEX) cprog.ms | sed -f side.ref | $(UNROFF) document=cprog

check:
	cknr -c.Tc -a.Es.Ee.Cs.Ce cprog.ms

