NULL = 

pkglib_LTLIBRARIES = \
	bitstring.la \
	elk-eval.la \
	hack.la \
	regexp.la \
	debug.la \
	$(gdbm_la) \
	$(monitor_la) \
	record.la \
	struct.la \
	$(newhandler_la) \
	$(NULL)

bitstring_la_SOURCES = bitstring.c
bitstring_la_LDFLAGS = -module -avoid-version -no-undefined
bitstring_la_LIBADD = $(top_builddir)/src/libelk.la

elk_eval_la_SOURCES = elk-eval.c
elk_eval_la_LDFLAGS = -module -avoid-version -no-undefined
elk_eval_la_LIBADD = $(top_builddir)/src/libelk.la

hack_la_SOURCES = hack.c
hack_la_LDFLAGS = -module -avoid-version -no-undefined
hack_la_LIBADD = $(top_builddir)/src/libelk.la

regexp_la_SOURCES = regexp.c
regexp_la_LDFLAGS = -module -avoid-version -no-undefined
regexp_la_LIBADD = $(top_builddir)/src/libelk.la

debug_la_SOURCES = debug.c
debug_la_LDFLAGS = -module -avoid-version -no-undefined
debug_la_LIBADD = $(top_builddir)/src/libelk.la

if HAVE_GDBM
gdbm_la = gdbm.la
endif
gdbm_la_SOURCES = gdbm.c
gdbm_la_LDFLAGS = -module -avoid-version -no-undefined
gdbm_la_LIBADD = $(top_builddir)/src/libelk.la -lgdbm

if HAVE_MONITOR
monitor_la = monitor.la
endif
monitor_la_SOURCES = monitor.c
monitor_la_LDFLAGS = -module -avoid-version -no-undefined
monitor_la_LIBADD = $(top_builddir)/src/libelk.la

record_la_SOURCES = record.c
record_la_LDFLAGS = -module -avoid-version -no-undefined
record_la_LIBADD = $(top_builddir)/src/libelk.la

struct_la_SOURCES = struct.c
struct_la_LDFLAGS = -module -avoid-version -no-undefined
struct_la_LIBADD = $(top_builddir)/src/libelk.la

if HAVE_CXX
newhandler_la = newhandler.la
endif
newhandler_la_SOURCES = newhandler.cpp
newhandler_la_LDFLAGS = -module -avoid-version -no-undefined
newhandler_la_LIBADD = $(top_builddir)/src/libelk.la

