EXTRA_DIST = unix.ms
CLEANFILES = index.raw index.ms unix.ps unix.html

if HAVE_GROFF
doc_DATA = unix.ps
docdir = $(datadir)/doc/@PACKAGE@/ps
endif

MANUAL= unix
TROFF= groff -ms -t
UNROFF= unroff -ms

unix.ps: unix.ms index.ms
	(cat unix.ms ../util/tmac.index index.ms; echo ".Tc")\
	    | $(TROFF) 2> /dev/null > unix.ps

unix.html: unix.ms
	(cat $?; echo ".Tc") | $(UNROFF) document=unix

index.ms: unix.ms index.raw
	sort -f -t'#' +1 -3 +0n index.raw | $(AWK) -f ../util/fixindex.awk\
	    | $(AWK) -f ../util/block.awk >index.ms

index.raw: unix.ms
	$(TROFF) unix.ms 2> index.raw >/dev/null

check:
	checknr -c.Ul.Pr.Sy.Va.Sh.Ix.Id.Ch -a.Ss.Se.[[.]] unix.ms |\
	    grep -v "Empty command"

