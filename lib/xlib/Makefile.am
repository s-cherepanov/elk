NULL = 

include Include.am

EXTRA_DIST = MISSING

pkglib_LTLIBRARIES = xlib.la

xlib_la_SOURCES = $(SOURCES_XLIB)
xlib_la_CFLAGS = -I/usr/X11R6/include
xlib_la_LDFLAGS = -module -avoid-version
xlib_la_LIBADD = $(top_builddir)/src/libelk.la -L/usr/X11R6/lib -lX11

extensions_HEADERS = xlib.h
extensionsdir = $(pkgincludedir)/extensions

