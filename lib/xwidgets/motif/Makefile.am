NULL = 

EXTRA_DIST = $(SOURCES_D)
CLEANFILES = $(SOURCES_C)

pkglib_LTLIBRARIES = $(motif_widgets_la)

if HAVE_MOTIF
motif_widgets_la = motif-widgets.la
endif

nodist_motif_widgets_la_SOURCES = $(SOURCES_C)
motif_widgets_la_SOURCES = init.c
motif_widgets_la_CFLAGS = -I$(srcdir)/../../xlib @MOTIF_CFLAGS@
motif_widgets_la_LDFLAGS = -module -avoid-version -u XmIsMotifWMRunning
motif_widgets_la_LIBADD = $(top_builddir)/src/libelk.la @MOTIF_LIBS@

.d.c:
	$(top_builddir)/src/elk -p .:$(top_srcdir)/scm:$(srcdir)/.. -l mkwidget.scm $< $@ motif

SOURCES_C = \
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

SOURCES_D = \
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

