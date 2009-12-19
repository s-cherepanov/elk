EXTRA_DIST = oops.ms
CLEANFILES = index.raw index.ms oops.ps oops.html

if HAVE_GROFF
docs_DATA = oops.ps
docsdir = $(datadir)/doc/@PACKAGE@/ps
endif

MANUAL = $(srcdir)/oops.ms
TROFF = groff -ms -t
UNROFF = unroff -ms

oops.ps: $(MANUAL) index.ms
	(cat $(MANUAL) $(srcdir)/../util/tmac.index index.ms; echo ".Tc") \
	    | $(TROFF) 2> /dev/null > oops.ps

oops.html: $(MANUAL)
	(cat $?; echo ".Tc") | $(UNROFF) document=oops

index.ms: $(MANUAL) index.raw
	sort -f -t'#' +1 -3 +0n index.raw \
	    | $(AWK) -f $(srcdir)/../util/fixindex.awk \
	    | $(AWK) -f $(srcdir)/../util/block.awk >index.ms

index.raw: $(MANUAL)
	$(TROFF) $(MANUAL) 2> index.raw >/dev/null

#check:
#	checknr -c.Ul.Pr.Sy.Va.Sh.Ix.Id.Ch -a.Ss.Se.[[.]] $(MANUAL) \
#	    | grep -v "Empty command"

