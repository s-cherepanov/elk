EXTRA_DIST = kernel.ms
CLEANFILES = index.raw index.ms kernel.ps kernel.html

if HAVE_GROFF
docs_DATA = kernel.ps
docsdir = $(datadir)/doc/@PACKAGE@/ps
endif

MANUAL = $(srcdir)/kernel.ms
TROFF = groff -ms -t
UNROFF = unroff -ms

kernel.ps: $(MANUAL) index.ms
	(cat $(MANUAL) $(srcdir)/../util/tmac.index index.ms; echo ".Tc") \
	    | $(TROFF) 2> /dev/null > kernel.ps

kernel.html: $(MANUAL)
	(cat $?; echo ".Tc") | $(UNROFF) document=kernel

index.ms: $(MANUAL) index.raw
	sort -f -t'#' +1 -3 +0n index.raw \
	    | $(AWK) -f $(srcdir)/../util/fixindex.awk \
	    | $(AWK) -f $(srcdir)/../util/block.awk >index.ms

index.raw: $(MANUAL)
	$(TROFF) $(MANUAL) 2> index.raw >/dev/null

#check:
#	checknr -c.Ul.Pr.Sy.Va.Sh.Ix.Id.Ch -a.Ss.Se.[[.]] $(MANUAL) \
#	    | grep -v "Empty command"

