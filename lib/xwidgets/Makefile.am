NULL = 

SUBDIRS = . xaw motif

EXTRA_DIST = MISSING mkwidget.scm

lib_LTLIBRARIES = $(libelk_xt_la)

if HAVE_XMU
libelk_xt_la = libelk-xt.la
endif

libelk_xt_la_SOURCES = $(SOURCES_XT)
libelk_xt_la_CFLAGS = -I$(srcdir)/../xlib @XMU_CFLAGS@
libelk_xt_la_LDFLAGS = -no-undefined
libelk_xt_la_LIBADD = $(top_builddir)/src/libelk.la ../xlib/libelk-xlib.la @XMU_LIBS@

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

