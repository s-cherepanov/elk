NULL = 

lib_LTLIBRARIES = $(libelk_gobject_la)
pkglib_LTLIBRARIES = $(gobject_la)

if HAVE_GOBJECT
libelk_gobject_la = libelk-gobject.la
gobject_la = gobject.la
endif

libelk_gobject_la_SOURCES = \
	$(NULL)
libelk_gobject_la_CFLAGS = @GOBJECT_CFLAGS@
libelk_gobject_la_LDFLAGS = -no-undefined
libelk_gobject_la_LIBADD = $(top_builddir)/src/libelk.la @GOBJECT_LIBS@

gobject_la_SOURCES = init.c
gobject_la_CFLAGS = @GOBJECT_CFLAGS@
gobject_la_LDFLAGS = -module -avoid-version -no-undefined
gobject_la_LIBADD = $(top_builddir)/src/libelk.la libelk-gobject.la @GOBJECT_LIBS@
