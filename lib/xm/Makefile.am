NULL = 

AUTOMAKE_OPTIONS = subdir-objects

include ../xlib/Include.am
include ../xt/Include.am

pkglib_xm_DATA = ALIASES
pkglib_xm_LTLIBRARIES = xm.la
pkglib_xmdir = $(pkglibdir)/xm

xm_la_SOURCES = \
	arrow-button.c \
	bulletin-brd.c \
	cascade-btn.c \
	command.c \
	drawing-area.c \
	drawn-button.c \
	file-selectn.c \
	form.c \
	frame.c \
	label.c \
	list.c \
	main-window.c \
	message-box.c \
	paned-window.c \
	push-button.c \
	row-column.c \
	scale.c \
	scroll-bar.c \
	scrolled-win.c \
	selectn-box.c \
	separator.c \
	shell.c \
	support.c \
	text.c \
	toggle-btn.c \
	$(NULL)
xm_la_CFLAGS = -I/usr/X11R6/include -I/usr/include/Xm -I$(srcdir)/../xlib
xm_la_LDFLAGS = -module -avoid-version -u XmIsMotifWMRunning
xm_la_LIBADD = $(top_builddir)/src/libelk.la -L/usr/X11R6/lib -lXm -lXmu -lXt -lSM -lICE -lXext -lX11

pkglib_LTLIBRARIES = xt-motif.la

xt_motif_la_SOURCES = $(SOURCES_XT) $(SOURCES_XLIB)
xt_motif_la_CFLAGS = -I/usr/include/Xm -I/usr/X11R6/include -I$(srcdir)/../xlib
xt_motif_la_LDFLAGS = -module -avoid-version
xt_motif_la_LIBADD = $(top_builddir)/src/libelk.la -L/usr/X11R6/lib -lXm -lXmu -lXt -lSM -lICE -lXext -lX11

.d.c:
	$(top_builddir)/src/elk -p .:$(top_srcdir)/scm:$(srcdir)/../xt -l mkwidget.scm $< $@ xm

clean: clean-local
clean-local:
	-rm -f *.c

