NULL = 

include Include.am

EXTRA_DIST = MISSING

pkglib_LTLIBRARIES = xlib.la

xlib_la_SOURCES = $(SOURCES_XLIB)
xlib_la_CFLAGS = @X_CFLAGS@
xlib_la_LDFLAGS = -module -avoid-version
xlib_la_LIBADD = $(top_builddir)/src/libelk.la @X_LIBS@

extensions_HEADERS = xlib.h
extensionsdir = $(pkgincludedir)/extensions

