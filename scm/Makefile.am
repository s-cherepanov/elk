NULL = 

EXTRA_DIST = $(SCM_FILES) $(SCM_DIST) slib.scm.in

pkgdata_DATA = $(SCM_FILES) $(SCM_MAYBE) slib.scm

SCM_FILES = \
	apropos.scm \
	bitstring.scm \
	cscheme.scm \
	debug.scm \
	describe.scm \
	initscheme.scm \
	oops.scm \
	parse.scm \
	pp.scm \
	qsort.scm \
	record.scm \
	recordutil.scm \
	regexp.scm \
	safe-env.scm \
	setf.scm \
	struct.scm \
	toplevel.scm \
	trace.scm \
	unix.scm \
	$(NULL)

SCM_MAYBE = \
	$(gdbmtest_scm) \
	$(motif_scm) \
	$(xlib_scm) \
	$(xaw_scm) \
	$(xwidgets_scm) \
	$(NULL)

SCM_DIST = \
	gdbmtest.scm \
	motif.scm \
	xlib.scm \
	xaw.scm \
	xwidgets.scm \
	$(NULL)

if HAVE_GDBM
gdbmtest_scm = gdbmtest.scm
endif
if HAVE_X11
xlib_scm = xlib.scm
endif
if HAVE_XAW
xaw_scm = xaw.scm
xwidgets_scm = xwidgets.scm
endif
if HAVE_MOTIF
motif_scm = motif.scm
xwidgets_scm = xwidgets.scm
endif

