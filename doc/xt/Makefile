MANUAL= xt
TROFF= groff -ms -t
UNROFF= unroff -ms

$(MANUAL).ps:	$(MANUAL).ms index.ms
	(cat $(MANUAL).ms ../util/tmac.index index.ms; echo ".Tc")\
	    | $(TROFF) 2> /dev/null > $(MANUAL).ps

$(MANUAL).html:	$(MANUAL).ms
	(cat $?; echo ".Tc") | $(UNROFF) document=$(MANUAL)

index.ms:	$(MANUAL).ms index.raw
	sort -f -t# +1 -3 +0n index.raw | awk -f ../util/fixindex.awk\
	    | awk -f ../util/block.awk >index.ms

index.raw:	$(MANUAL).ms
	$(TROFF) $(MANUAL).ms 2> index.raw >/dev/null

check:
	checknr -c.Ul.Pr.Sy.Va.Sh.Ix.Id.Ch -a.Ss.Se.[[.]] $(MANUAL).ms |\
	    grep -v "Empty command"

clean:
	rm -f index.raw index.ms $(MANUAL).ps $(MANUAL).html
