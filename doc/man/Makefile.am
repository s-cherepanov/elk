EXTRA_DIST = $(man_MANS)
CLEANFILES = elk.ps elk.1.html

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

install-data-hook:
	rm -f "$(DESTDIR)$(mandir)/man1/scheme-elk.1"
	ln -s elk.1 "$(DESTDIR)$(mandir)/man1/scheme-elk.1"

