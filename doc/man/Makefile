TROFF= groff -man -t
UNROFF= unroff -man

elk.ps:	elk.1
	$(TROFF) $? > $@

elk.1.html: elk.1
	$(UNROFF) $?

clean:
	rm -f elk.ps elk.1.html
