EXTRA_DIST = oops.ms
CLEANFILES = index.raw index.ms oops.ps oops.html

if HAVE_GROFF
doc_DATA = oops.ps
docdir = $(datadir)/doc/@PACKAGE@/ps
endif

MANUAL= oops
TROFF= groff -ms -t
UNROFF= unroff -ms

oops.ps: oops.ms index.ms
	(cat oops.ms ../util/tmac.index index.ms; echo ".Tc")\
	    | $(TROFF) 2> /dev/null > oops.ps

oops.html: oops.ms
	(cat $?; echo ".Tc") | $(UNROFF) document=oops

index.ms: oops.ms index.raw
	sort -f -t'#' +1 -3 +0n index.raw | $(AWK) -f ../util/fixindex.awk\
	    | $(AWK) -f ../util/block.awk >index.ms

index.raw: oops.ms
	$(TROFF) oops.ms 2> index.raw >/dev/null

check:
	checknr -c.Ul.Pr.Sy.Va.Sh.Ix.Id.Ch -a.Ss.Se.[[.]] oops.ms |\
	    grep -v "Empty command"

