EXTRA_DIST = xlib.ms

if HAVE_GROFF
doc_DATA = xlib.ps
docdir = $(datadir)/doc/@PACKAGE@/ps
endif

MANUAL= xlib
TROFF= groff -ms -t
UNROFF= unroff -ms

xlib.ps: xlib.ms index.ms
	(cat xlib.ms ../util/tmac.index index.ms; echo ".Tc")\
	    | $(TROFF) 2> /dev/null > xlib.ps

xlib.html: xlib.ms
	(cat $?; echo ".Tc") | $(UNROFF) document=xlib

index.ms: xlib.ms index.raw
	sort -f -t'#' +1 -3 +0n index.raw | awk -f ../util/fixindex.awk\
	    | awk -f ../util/block.awk >index.ms

index.raw: xlib.ms
	$(TROFF) xlib.ms 2> index.raw >/dev/null

check:
	checknr -c.Ul.Pr.Sy.Va.Sh.Ix.Id.Ch -a.Ss.Se.[[.]] xlib.ms |\
	    grep -v "Empty command"

clean:
	rm -f index.raw index.ms xlib.ps xlib.html
