SUBDIRS = xaw motif

NULL = 

include ../xlib/Include.am

EXTRA_DIST = MISSING mkwidget.scm

pkglib_LTLIBRARIES = xaw.la motif.la

xaw_la_SOURCES = $(SOURCES_XT) $(SOURCES_XLIB)
xaw_la_CFLAGS = -I$(srcdir)/../xlib @XAW_CFLAGS@
xaw_la_LDFLAGS = -module -avoid-version
xaw_la_LIBADD = $(top_builddir)/src/libelk.la @XAW_LIBS@

motif_la_SOURCES = $(SOURCES_XT) $(SOURCES_XLIB)
motif_la_CFLAGS = -I$(srcdir)/../xlib @XM_CFLAGS@
motif_la_LDFLAGS = -module -avoid-version
motif_la_LIBADD = $(top_builddir)/src/libelk.la @XM_LIBS@

extensions_HEADERS = xt.h
extensionsdir = $(pkgincludedir)/extensions

SOURCES_XT = \
	accelerator.c \
	action.c \
	callback.c \
	class.c \
	classname.c \
	context.c \
	converter.c \
	function.c \
	identifier.c \
	popup.c \
	resource.c \
	translation.c \
	widget.c \
	xterror.c \
	xtinit.c \
	xtobjects.c \
	$(NULL)

