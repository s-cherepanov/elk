EXTRA_DIST = cprog.ms
CLEANFILES = side side.ref side.inx cprog.ps cprog.html

if HAVE_GROFF
docs_DATA = cprog.ps
docsdir = $(datadir)/doc/@PACKAGE@/ps
endif

MANUAL = $(srcdir)/cprog.ms
TROFF = groff -ms -t
UNROFF = unroff -ms
MKINDEX = $(AWK) -f $(srcdir)/../util/mkindex.awk

cprog.ps: $(MANUAL) side.ref side.inx
	$(MKINDEX) $(MANUAL) | sed -f side.ref | $(TROFF) 2> /dev/null > $@

side.ref: side
	grep '^[sS]' < side | sed -e 's/\.\//\//' > side.ref

side.inx: side
	egrep -v '^s' side | sort -f -t'#' +1 -3 +0n | \
	    $(AWK) -f $(srcdir)/../util/fixindex.awk | \
	    $(AWK) -f $(srcdir)/../util/block.awk \
	    > side.inx

side: $(MANUAL)
	rm -f side side.inx && printf "" > side.inx
	$(MKINDEX) $? | $(TROFF) 2>side >/dev/null

cprog.html: $(MANUAL) side.ref
	$(MKINDEX) $(MANUAL) | sed -f side.ref | $(UNROFF) document=cprog

check:
	cknr -c.Tc -a.Es.Ee.Cs.Ce $(MANUAL)

