NULL = 

include ../xlib/Include.am
include ../xt/Include.am

AUTOMAKE_OPTIONS = subdir-objects
EXTRA_DIST = MISSING

pkglib_LTLIBRARIES = xt.la

xt_la_SOURCES = $(SOURCES_XT) $(SOURCES_XLIB)
xt_la_CFLAGS = -I/usr/X11R6/include -I$(srcdir)/../xlib
xt_la_LDFLAGS = -module -avoid-version
xt_la_LIBADD = $(top_builddir)/src/libelk.la -L/usr/X11R6/lib -lXaw -lXmu -lXt -lSM -lICE -lXext -lX11

extensions_HEADERS = xt.h
extensionsdir = $(pkgincludedir)/extensions

