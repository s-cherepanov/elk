NULL = 

EXTRA_DIST = $(DATA) $(D_FILES)

pkglib_xm_DATA = ALIASES
pkglib_xm_LTLIBRARIES = xm.la
pkglib_xmdir = $(pkglibdir)/xm

nodist_xm_la_SOURCES = $(C_FILES)
xm_la_CFLAGS = -I$(srcdir)/../xlib @XM_LIBS@
xm_la_LDFLAGS = -module -avoid-version -u XmIsMotifWMRunning
xm_la_LIBADD = $(top_builddir)/src/libelk.la @XM_LIBS@

.d.c:
	$(top_builddir)/src/elk -p .:$(top_srcdir)/scm:$(srcdir)/../xt -l mkwidget.scm $< $@ xm

clean: clean-local
clean-local:
	-rm -f $(C_FILES)

C_FILES = \
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

D_FILES = \
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

