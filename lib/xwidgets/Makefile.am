SUBDIRS = xaw motif

NULL = 

include ../xlib/Include.am
include ../xt/Include.am

EXTRA_DIST = MISSING mkwidget.scm

pkglib_LTLIBRARIES = xt.la xt-motif.la

xt_la_SOURCES = $(SOURCES_XT) $(SOURCES_XLIB)
xt_la_CFLAGS = -I$(srcdir)/../xlib @XAW_CFLAGS@
xt_la_LDFLAGS = -module -avoid-version
xt_la_LIBADD = $(top_builddir)/src/libelk.la @XAW_LIBS@

xt_motif_la_SOURCES = $(SOURCES_XT) $(SOURCES_XLIB)
xt_motif_la_CFLAGS = -I$(srcdir)/../xlib @XM_CFLAGS@
xt_motif_la_LDFLAGS = -module -avoid-version
xt_motif_la_LIBADD = $(top_builddir)/src/libelk.la @XM_LIBS@

extensions_HEADERS = xt.h
extensionsdir = $(pkgincludedir)/extensions

