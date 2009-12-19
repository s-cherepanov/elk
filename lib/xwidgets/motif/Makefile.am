NULL = 

EXTRA_DIST = $(SOURCES_MOTIF_D)
CLEANFILES = $(SOURCES_MOTIF_C)

pkglib_LTLIBRARIES = $(motif_la)

if HAVE_MOTIF
motif_la = motif.la
endif

motif_la_SOURCES = init.c $(SOURCES_MOTIF_C)
motif_la_CFLAGS = -I$(srcdir)/../../xlib @MOTIF_CFLAGS@ -DUSING_MOTIF
motif_la_LDFLAGS = -module -avoid-version -no-undefined -u XmIsMotifWMRunning
motif_la_LIBADD = $(top_builddir)/src/libelk.la ../../xlib/libelk-xlib.la ../libelk-xt.la @MOTIF_LIBS@

.d.c:
	$(top_builddir)/src/elk -g -p .:$(top_srcdir)/scm:$(srcdir)/.. -l mkwidget.scm $< $@ motif

SOURCES_MOTIF_C = \
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

SOURCES_MOTIF_D = \
	arrow-button.d \
	bulletin-brd.d \
	cascade-btn.d \
	command.d \
	drawing-area.d \
	drawn-button.d \
	file-selectn.d \
	form.d \
	frame.d \
	label.d \
	list.d \
	main-window.d \
	message-box.d \
	paned-window.d \
	push-button.d \
	row-column.d \
	scale.d \
	scroll-bar.d \
	scrolled-win.d \
	selectn-box.d \
	separator.d \
	shell.d \
	support.d \
	text.d \
	toggle-btn.d \
	$(NULL)

echo-sources: ; echo $(SOURCES)

