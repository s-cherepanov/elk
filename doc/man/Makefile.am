EXTRA_DIST = $(man_MANS)

man_MANS = elk.1

if HAVE_GROFF
doc_DATA = elk.ps
docdir = $(datadir)/doc/@PACKAGE@/ps
endif

TROFF= groff -man -t
UNROFF= unroff -man

elk.ps: elk.1
	$(TROFF) $? > $@

elk.1.html: elk.1
	$(UNROFF) $?

clean:
	rm -f elk.ps elk.1.html

