EXTRA_DIST = bitstring.ms
CLEANFILES = index.raw index.ms bitstring.ps bitstring.html

if HAVE_GROFF
doc_DATA = bitstring.ps
docdir = $(datadir)/doc/@PACKAGE@/ps
endif

MANUAL= bitstring
TROFF= groff -ms -t
UNROFF= unroff -ms

bitstring.ps: bitstring.ms index.ms
	(cat bitstring.ms ../util/tmac.index index.ms; echo ".Tc")\
	    | $(TROFF) 2> /dev/null > bitstring.ps

bitstring.html: bitstring.ms
	(cat $?; echo ".Tc") | $(UNROFF) document=bitstring

index.ms: bitstring.ms index.raw
	sort -f -t'#' +1 -3 +0n index.raw | $(AWK) -f ../util/fixindex.awk\
	    | $(AWK) -f ../util/block.awk >index.ms

index.raw: bitstring.ms
	$(TROFF) bitstring.ms 2> index.raw >/dev/null

check:
	checknr -c.Ul.Pr.Sy.Va.Sh.Ix.Id.Ch -a.Ss.Se.[[.]] bitstring.ms |\
	    grep -v "Empty command"

