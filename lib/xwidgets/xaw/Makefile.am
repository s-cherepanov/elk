NULL = 

EXTRA_DIST = $(SOURCES_D)

pkglib_LTLIBRARIES = xaw-widgets.la

nodist_xaw_widgets_la_SOURCES = $(SOURCES_C)
xaw_widgets_la_SOURCES = init.c
xaw_widgets_la_CFLAGS = -I$(srcdir)/../../xlib @XAW_CFLAGS@
xaw_widgets_la_LDFLAGS = -module -avoid-version
xaw_widgets_la_LIBADD = $(top_builddir)/src/libelk.la @XAW_LIBS@

.d.c:
	$(top_builddir)/src/elk -p .:$(top_srcdir)/scm:$(srcdir)/.. -l mkwidget.scm $< $@ xaw

clean: clean-local
clean-local:
	-rm -f $(SOURCES_C)

SOURCES_C = \
	ascii.c \
	box.c \
	command.c \
	dialog.c \
	form.c \
	grip.c \
	label.c \
	list.c \
	menubutton.c \
	paned.c \
	panner.c \
	porthole.c \
	repeater.c \
	scrollbar.c \
	shell.c \
	simplemenu.c \
	sme.c \
	smebsb.c \
	smeline.c \
	stripchart.c \
	toggle.c \
	tree.c \
	viewport.c \
	$(NULL)

SOURCES_D = \
	ascii.d \
	box.d \
	command.d \
	dialog.d \
	form.d \
	grip.d \
	label.d \
	list.d \
	menubutton.d \
	paned.d \
	panner.d \
	porthole.d \
	repeater.d \
	scrollbar.d \
	shell.d \
	simplemenu.d \
	sme.d \
	smebsb.d \
	smeline.d \
	stripchart.d \
	toggle.d \
	tree.d \
	viewport.d \
	$(NULL)

