NULL = 

EXTRA_DIST = MISSING

pkglib_LTLIBRARIES = $(xlib_la)

if HAVE_X11
xlib_la = xlib.la
endif

xlib_la_SOURCES = \
	client.c \
	color.c \
	colormap.c \
	cursor.c \
	display.c \
	event.c \
	extension.c \
	font.c \
	gcontext.c \
	grab.c \
	graphics.c \
	key.c \
	pixel.c \
	pixmap.c \
	property.c \
	text.c \
	type.c \
	util.c \
	window.c \
	wm.c \
	xerror.c \
	xinit.c \
	xobjects.c \
	$(NULL)
xlib_la_CFLAGS = @X_CFLAGS@
xlib_la_LDFLAGS = -module -avoid-version -no-undefined
xlib_la_LIBADD = $(top_builddir)/src/libelk.la @X_LIBS@

extensions_HEADERS = xlib.h
extensionsdir = $(pkgincludedir)/extensions

