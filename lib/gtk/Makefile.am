NULL = 

lib_LTLIBRARIES = $(libelk_gtk_la)
pkglib_LTLIBRARIES = $(gtk_la)

if HAVE_GTK
libelk_gtk_la = libelk-gtk.la
gtk_la = gtk.la
endif

libelk_gtk_la_SOURCES = \
	gtkmain.c \
	gtkwidget.c \
	$(NULL)
libelk_gtk_la_CFLAGS = @GTK_CFLAGS@
libelk_gtk_la_LDFLAGS = -no-undefined
libelk_gtk_la_LIBADD = $(top_builddir)/src/libelk.la @GTK_LIBS@

gtk_la_SOURCES = init.c
gtk_la_CFLAGS = @GTK_CFLAGS@
gtk_la_LDFLAGS = -module -avoid-version -no-undefined
gtk_la_LIBADD = $(top_builddir)/src/libelk.la libelk-gtk.la @GTK_LIBS@
