NULL = 

libelk_LTLIBRARIES = \
	libbitstring.la \
	libelk-eval.la \
	libhack.la \
	libnewhandler.la \
	libregexp.la \
	libdebug.la \
	libgdbm.la \
	libmonitor.la \
	librecord.la \
	libstruct.la \
	$(NULL)
libelkdir = /usr/lib/elk

libbitstring_la_SOURCES = bitstring.c
libbitstring_la_LIBADD = $(top_builddir)/src/libelk.la

libelk_eval_la_SOURCES = elk-eval.c
libelk_eval_la_LIBADD = $(top_builddir)/src/libelk.la

libhack_la_SOURCES = hack.c
libhack_la_LIBADD = $(top_builddir)/src/libelk.la

libnewhandler_la_SOURCES = newhandler.c
libnewhandler_la_LIBADD = $(top_builddir)/src/libelk.la

libregexp_la_SOURCES = regexp.c
libregexp_la_LIBADD = $(top_builddir)/src/libelk.la

libdebug_la_SOURCES = debug.c
libdebug_la_LIBADD = $(top_builddir)/src/libelk.la

libgdbm_la_SOURCES = gdbm.c
libgdbm_la_LIBADD = $(top_builddir)/src/libelk.la -lgdbm

libmonitor_la_SOURCES = monitor.c
libmonitor_la_LIBADD = $(top_builddir)/src/libelk.la

librecord_la_SOURCES = record.c
librecord_la_LIBADD = $(top_builddir)/src/libelk.la

libstruct_la_SOURCES = struct.c
libstruct_la_LIBADD = $(top_builddir)/src/libelk.la

