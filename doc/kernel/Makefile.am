EXTRA_DIST = kernel.ms
CLEANFILES = index.raw index.ms kernel.ps kernel.html

if HAVE_GROFF
doc_DATA = kernel.ps
docdir = $(datadir)/doc/@PACKAGE@/ps
endif

MANUAL= kernel
TROFF= groff -ms -t
UNROFF= unroff -ms

kernel.ps: kernel.ms index.ms
	(cat kernel.ms ../util/tmac.index index.ms; echo ".Tc")\
	    | $(TROFF) 2> /dev/null > kernel.ps

kernel.html: kernel.ms
	(cat $?; echo ".Tc") | $(UNROFF) document=kernel

index.ms: kernel.ms index.raw
	sort -f -t'#' +1 -3 +0n index.raw | awk -f ../util/fixindex.awk\
	    | awk -f ../util/block.awk >index.ms

index.raw: kernel.ms
	$(TROFF) kernel.ms 2> index.raw >/dev/null

check:
	checknr -c.Ul.Pr.Sy.Va.Sh.Ix.Id.Ch -a.Ss.Se.[[.]] kernel.ms |\
	    grep -v "Empty command"

