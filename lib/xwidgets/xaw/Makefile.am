NULL = 

EXTRA_DIST = $(SOURCES_XAW_D)
CLEANFILES = $(SOURCES_XAW_C)

pkglib_LTLIBRARIES = $(xaw_la)

if HAVE_XAW
xaw_la = xaw.la
endif

xaw_la_SOURCES = init.c $(SOURCES_XAW_C)
xaw_la_CFLAGS = -I$(srcdir)/../../xlib @XAW_CFLAGS@ -DUSING_XAW
xaw_la_LDFLAGS = -module -avoid-version -no-undefined
xaw_la_LIBADD = $(top_builddir)/src/libelk.la ../../xlib/libelk-xlib.la ../libelk-xt.la @XAW_LIBS@

.d.c:
	$(top_builddir)/src/elk -g -p .:$(top_srcdir)/scm:$(srcdir)/.. -l mkwidget.scm $< $@ xaw

SOURCES_XAW_C = \
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

SOURCES_XAW_D = \
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

