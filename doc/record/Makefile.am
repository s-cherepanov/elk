EXTRA_DIST = record.ms
CLEANFILES = index.raw index.ms record.ps record.html

if HAVE_GROFF
doc_DATA = record.ps
docdir = $(datadir)/doc/@PACKAGE@/ps
endif

MANUAL= record
TROFF= groff -ms -t
UNROFF= unroff -ms

record.ps: record.ms index.ms
	(cat record.ms ../util/tmac.index index.ms; echo ".Tc")\
	    | $(TROFF) 2> /dev/null > record.ps

record.html: record.ms
	(cat $?; echo ".Tc") | $(UNROFF) document=record

index.ms: record.ms index.raw
	sort -f -t'#' +1 -3 +0n index.raw | $(AWK) -f ../util/fixindex.awk\
	    | $(AWK) -f ../util/block.awk >index.ms

index.raw: record.ms
	$(TROFF) record.ms 2> index.raw >/dev/null

check:
	checknr -c.Ul.Pr.Sy.Va.Sh.Ix.Id.Ch -a.Ss.Se.[[.]] record.ms |\
	    grep -v "Empty command"

