NULL = 

EXTRA_DIST = MISSING

pkglib_LTLIBRARIES = xlib.la

xlib_la_SOURCES = \
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
	init.c \
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
xlib_la_CFLAGS = -I/usr/X11R6/include
xlib_la_LDFLAGS = -module -avoid-version
xlib_la_LIBADD = $(top_builddir)/src/libelk.la -L/usr/X11R6/lib -lX11

extensions_HEADERS = xlib.h
extensionsdir = $(pkgincludedir)/extensions

