NULL = 

lib_LTLIBRARIES = libelk.la
libelk_la_CFLAGS = -I/usr/include/libelf -DINIT_OBJECTS -DNOMAIN
libelk_la_LDFLAGS = 
libelk_la_LIBADD = -lm -lelf -ldl
libelk_la_SOURCES = \
	autoload.c \
	bignum.c \
	bool.c \
	char.c \
	cont.c \
	cstring.c \
	debug.c \
	dump.c \
	env.c \
	error.c \
	exception.c \
	feature.c \
	heap.c \
	io.c \
	libelk.c \
	list.c \
	load.c \
	malloc.c \
	math.c \
	onfork.c \
	prim.c \
	print.c \
	proc.c \
	promise.c \
	read.c \
	special.c \
	stab.c \
	stkmem.c \
	string.c \
	symbol.c \
	terminate.c \
	type.c \
	vector.c \
	$(NULL)

bin_PROGRAMS = elk
elk_LDADD = $(top_builddir)/src/libelk.la
elk_SOURCES = main.c

