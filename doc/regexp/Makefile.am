EXTRA_DIST = regexp.ms

if HAVE_GROFF
doc_DATA = regexp.ps
docdir = $(datadir)/doc/@PACKAGE@/ps
endif

MANUAL= regexp
TROFF= groff -ms -t
UNROFF= unroff -ms

regexp.ps: regexp.ms index.ms
	(cat regexp.ms ../util/tmac.index index.ms; echo ".Tc")\
	    | $(TROFF) 2> /dev/null > regexp.ps

regexp.html: regexp.ms
	(cat $?; echo ".Tc") | $(UNROFF) document=regexp

index.ms: regexp.ms index.raw
	sort -f -t'#' +1 -3 +0n index.raw | awk -f ../util/fixindex.awk\
	    | awk -f ../util/block.awk >index.ms

index.raw: regexp.ms
	$(TROFF) regexp.ms 2> index.raw >/dev/null

check:
	checknr -c.Ul.Pr.Sy.Va.Sh.Ix.Id.Ch -a.Ss.Se.[[.]] regexp.ms |\
	    grep -v "Empty command"

clean:
	rm -f index.raw index.ms regexp.ps regexp.html
