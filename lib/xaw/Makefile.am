NULL = 

pkglib_xaw_DATA = ALIASES
pkglib_xaw_LTLIBRARIES = xaw.la
pkglib_xawdir = $(pkglibdir)/xaw

xaw_la_SOURCES = \
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
xaw_la_CFLAGS = -I/usr/X11R6/include -I$(srcdir)/../xlib
xaw_la_LDFLAGS = -module -avoid-version
xaw_la_LIBADD = $(top_builddir)/src/libelk.la -L/usr/X11R6/lib -lXaw -lXmu -lXt -lSM -lICE -lXext -lX11

.d.c:
	$(top_builddir)/src/elk -p .:$(top_srcdir)/scm:$(srcdir)/../xt -l mkwidget.scm $< $@ xaw

clean: clean-local
clean-local:
	-rm -f *.c

