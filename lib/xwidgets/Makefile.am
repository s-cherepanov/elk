NULL = 

AUTOMAKE_OPTIONS = subdir-objects
EXTRA_DIST = MISSING mkwidget.scm $(SOURCES_XAW_D) $(SOURCES_MOTIF_D)
CLEANFILES = $(SOURCES_XAW_C) $(SOURCES_MOTIF_C)

pkglib_LTLIBRARIES = $(xaw_la) $(motif_la)

if HAVE_XAW
xaw_la = xaw.la
endif

if HAVE_MOTIF
motif_la = motif.la
endif

xaw_la_SOURCES = $(SOURCES_XT) $(SOURCES_XAW_C)
xaw_la_CFLAGS = -I$(srcdir)/../xlib @XAW_CFLAGS@ -DUSING_XAW
xaw_la_LDFLAGS = -module -avoid-version -no-undefined
xaw_la_LIBADD = $(top_builddir)/src/libelk.la @XAW_LIBS@

motif_la_SOURCES = $(SOURCES_XT) $(SOURCES_MOTIF_C)
motif_la_CFLAGS = -I$(srcdir)/../xlib @MOTIF_CFLAGS@ -DUSING_MOTIF
motif_la_LDFLAGS = -module -avoid-version -no-undefined -u XmIsMotifWMRunning
motif_la_LIBADD = $(top_builddir)/src/libelk.la @MOTIF_LIBS@

extensions_HEADERS = xt.h
extensionsdir = $(pkgincludedir)/extensions

.d.c:
	$(top_builddir)/src/elk -g -p .:$(top_srcdir)/scm:$(srcdir)/.. -l mkwidget.scm $< $@

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

SOURCES_XAW_C = \
	xaw/ascii.c \
	xaw/box.c \
	xaw/command.c \
	xaw/dialog.c \
	xaw/form.c \
	xaw/grip.c \
	xaw/label.c \
	xaw/list.c \
	xaw/menubutton.c \
	xaw/paned.c \
	xaw/panner.c \
	xaw/porthole.c \
	xaw/repeater.c \
	xaw/scrollbar.c \
	xaw/shell.c \
	xaw/simplemenu.c \
	xaw/sme.c \
	xaw/smebsb.c \
	xaw/smeline.c \
	xaw/stripchart.c \
	xaw/toggle.c \
	xaw/tree.c \
	xaw/viewport.c \
	$(NULL)

SOURCES_XAW_D = \
	xaw/ascii.d \
	xaw/box.d \
	xaw/command.d \
	xaw/dialog.d \
	xaw/form.d \
	xaw/grip.d \
	xaw/label.d \
	xaw/list.d \
	xaw/menubutton.d \
	xaw/paned.d \
	xaw/panner.d \
	xaw/porthole.d \
	xaw/repeater.d \
	xaw/scrollbar.d \
	xaw/shell.d \
	xaw/simplemenu.d \
	xaw/sme.d \
	xaw/smebsb.d \
	xaw/smeline.d \
	xaw/stripchart.d \
	xaw/toggle.d \
	xaw/tree.d \
	xaw/viewport.d \
	$(NULL)

SOURCES_MOTIF_C = \
	motif/arrow-button.c \
	motif/bulletin-brd.c \
	motif/cascade-btn.c \
	motif/command.c \
	motif/drawing-area.c \
	motif/drawn-button.c \
	motif/file-selectn.c \
	motif/form.c \
	motif/frame.c \
	motif/label.c \
	motif/list.c \
	motif/main-window.c \
	motif/message-box.c \
	motif/paned-window.c \
	motif/push-button.c \
	motif/row-column.c \
	motif/scale.c \
	motif/scroll-bar.c \
	motif/scrolled-win.c \
	motif/selectn-box.c \
	motif/separator.c \
	motif/shell.c \
	motif/support.c \
	motif/text.c \
	motif/toggle-btn.c \
	$(NULL)

SOURCES_MOTIF_D = \
	motif/arrow-button.d \
	motif/bulletin-brd.d \
	motif/cascade-btn.d \
	motif/command.d \
	motif/drawing-area.d \
	motif/drawn-button.d \
	motif/file-selectn.d \
	motif/form.d \
	motif/frame.d \
	motif/label.d \
	motif/list.d \
	motif/main-window.d \
	motif/message-box.d \
	motif/paned-window.d \
	motif/push-button.d \
	motif/row-column.d \
	motif/scale.d \
	motif/scroll-bar.d \
	motif/scrolled-win.d \
	motif/selectn-box.d \
	motif/separator.d \
	motif/shell.d \
	motif/support.d \
	motif/text.d \
	motif/toggle-btn.d \
	$(NULL)


