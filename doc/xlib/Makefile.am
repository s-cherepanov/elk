EXTRA_DIST = xlib.ms
CLEANFILES = index.raw index.ms xlib.ps xlib.html

if HAVE_GROFF
doc_DATA = xlib.ps
docdir = $(datadir)/doc/@PACKAGE@/ps
endif

MANUAL = $(srcdir)/xlib.ms
TROFF = groff -ms -t
UNROFF = unroff -ms

xlib.ps: $(MANUAL) index.ms
	(cat $(MANUAL) $(srcdir)/../util/tmac.index index.ms; echo ".Tc") \
	    | $(TROFF) 2> /dev/null > xlib.ps

xlib.html: $(MANUAL)
	(cat $?; echo ".Tc") | $(UNROFF) document=xlib

index.ms: $(MANUAL) index.raw
	sort -f -t'#' +1 -3 +0n index.raw \
	    | $(AWK) -f $(srcdir)/../util/fixindex.awk \
	    | $(AWK) -f $(srcdir)/../util/block.awk >index.ms

index.raw: $(MANUAL)
	$(TROFF) $(MANUAL) 2> index.raw >/dev/null

check:
	checknr -c.Ul.Pr.Sy.Va.Sh.Ix.Id.Ch -a.Ss.Se.[[.]] $(MANUAL) \
	    | grep -v "Empty command"

