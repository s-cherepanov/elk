EXTRA_DIST = xt.ms

if HAVE_GROFF
doc_DATA = xt.ps
docdir = $(datadir)/doc/@PACKAGE@
endif

MANUAL= xt
TROFF= groff -ms -t
UNROFF= unroff -ms

xt.ps: xt.ms index.ms
	(cat xt.ms ../util/tmac.index index.ms; echo ".Tc")\
	    | $(TROFF) 2> /dev/null > xt.ps

xt.html: xt.ms
	(cat $?; echo ".Tc") | $(UNROFF) document=xt

index.ms: xt.ms index.raw
	sort -f -t'#' +1 -3 +0n index.raw | awk -f ../util/fixindex.awk\
	    | awk -f ../util/block.awk >index.ms

index.raw: xt.ms
	$(TROFF) xt.ms 2> index.raw >/dev/null

check:
	checknr -c.Ul.Pr.Sy.Va.Sh.Ix.Id.Ch -a.Ss.Se.[[.]] xt.ms |\
	    grep -v "Empty command"

clean:
	rm -f index.raw index.ms xt.ps xt.html
