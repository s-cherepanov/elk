NULL = 

EXTRA_DIST = TODO

pkglib_LTLIBRARIES = unix.la

unix_la_SOURCES = \
	error.c \
	fdescr.c \
	file.c \
	lock.c \
	misc.c \
	passwd.c \
	process.c \
	signal.c \
	system.c \
	temp.c \
	time.c \
	unix.c \
	wait.c \
	$(NULL)
unix_la_LDFLAGS = -module -avoid-version -no-undefined
unix_la_LIBADD = $(top_builddir)/src/libelk.la

extensions_HEADERS = unix.h
extensionsdir = $(pkgincludedir)/extensions

echo-sources: ; echo $(SOURCES)

