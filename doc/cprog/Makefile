TROFF= groff -ms -t
UNROFF=  unroff -ms
MKINDEX= ../util/mkindex

cprog.ps:	cprog.ms side.ref side.inx
	$(MKINDEX) cprog.ms | sed -f side.ref | $(TROFF) 2> /dev/null > $@

side.ref:	side
	-cp side.ref side.ref.last
	egrep "^s" side | sed "s/\.\//\//" >side.ref

side.inx:	side
	egrep -v "^s" side | sort -f -t# +1 -3 +0n | \
	    awk -f ../util/fixindex.awk | awk -f ../util/block.awk \
	    >side.inx

#	    | sed -e 's/[^"]*()/\\s-1\\f5&\\fP\\s0/' >side.inx

side:	cprog.ms
	touch side.inx
	$(MKINDEX) $? | sed -f side.ref.last | $(TROFF) 2>side >/dev/null

cprog.html:	cprog.ms side.ref
	$(MKINDEX) cprog.ms |sed -f side.ref | $(UNROFF) document=cprog

check:
	cknr -c.Tc -a.Es.Ee.Cs.Ce cprog.ms

clean:
	rm -f side side.ref side.inx cprog.ps cprog.html
