NULL = 

EXTRA_DIST = MISSING

lib_LTLIBRARIES = $(libelk_xlib_la)
pkglib_LTLIBRARIES = $(xlib_la)

if HAVE_X11
libelk_xlib_la = libelk-xlib.la
xlib_la = xlib.la
endif

libelk_xlib_la_SOURCES = \
	client.c \
	color.c \
	colormap.c \
	cursor.c \
	display.c \
	error.c \
	event.c \
	extension.c \
	font.c \
	gcontext.c \
	grab.c \
	graphics.c \
	key.c \
	objects.c \
	pixel.c \
	pixmap.c \
	property.c \
	text.c \
	type.c \
	util.c \
	window.c \
	wm.c \
	$(NULL)
libelk_xlib_la_CFLAGS = @X_CFLAGS@
libelk_xlib_la_LDFLAGS = -no-undefined
libelk_xlib_la_LIBADD = $(top_builddir)/src/libelk.la @X_LIBS@

xlib_la_SOURCES = init.c
xlib_la_CFLAGS = @X_CFLAGS@
xlib_la_LDFLAGS = -module -avoid-version -no-undefined
xlib_la_LIBADD = $(top_builddir)/src/libelk.la libelk-xlib.la @X_LIBS@

extensions_HEADERS = xlib.h
extensionsdir = $(pkgincludedir)/extensions

