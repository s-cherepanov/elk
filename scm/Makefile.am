NULL = 

EXTRA_DIST = $(SCM_FILES) slib.scm.in

pkgdata_DATA = $(SCM_FILES) slib.scm

SCM_FILES = \
	apropos.scm \
	bitstring.scm \
	cscheme.scm \
	debug.scm \
	describe.scm \
	$(gdbmtest_scm) \
	initscheme.scm \
	$(motif_scm) \
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
	$(xlib_scm) \
	$(xaw_scm) \
	$(xwidgets_scm) \
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

