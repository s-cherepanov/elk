EXTRA_DIST = bitstring.ms
CLEANFILES = index.raw index.ms bitstring.ps bitstring.html

if HAVE_GROFF
docs_DATA = bitstring.ps
docsdir = $(datadir)/doc/@PACKAGE@/ps
endif

MANUAL = $(srcdir)/bitstring.ms
TROFF = groff -ms -t
UNROFF = unroff -ms

bitstring.ps: $(MANUAL) index.ms
	(cat $(MANUAL) $(srcdir)/../util/tmac.index index.ms; echo ".Tc") \
	    | $(TROFF) 2> /dev/null > bitstring.ps

bitstring.html: $(MANUAL)
	(cat $?; echo ".Tc") | $(UNROFF) document=bitstring

index.ms: $(MANUAL) index.raw
	sort -f -t'#' +1 -3 +0n index.raw \
	    | $(AWK) -f $(srcdir)/../util/fixindex.awk \
	    | $(AWK) -f $(srcdir)/../util/block.awk >index.ms

index.raw: $(MANUAL)
	$(TROFF) $(MANUAL) 2> index.raw >/dev/null

#check:
#	checknr -c.Ul.Pr.Sy.Va.Sh.Ix.Id.Ch -a.Ss.Se.[[.]] $(MANUAL) \
#	    | grep -v "Empty command"

