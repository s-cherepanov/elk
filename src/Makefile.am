NULL = 

lib_LTLIBRARIES = libelk.la
libelk_la_CFLAGS = -I/usr/include/libelf
libelk_la_LDFLAGS = 
libelk_la_LIBADD = @ELK_LIBS@
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
	loadlib.c \
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
	$(stab_elf_c) \
	$(stab_vanilla_c) \
	stkmem.c \
	string.c \
	symbol.c \
	terminate.c \
	type.c \
	vector.c \
	$(NULL)

if HAVE_LIBELF
stab_elf_c = stab-elf.c
else
stab_vanilla_c = stab-vanilla.c
endif

EXTRA_DIST = \
	dump-ecoff.c \
	dump-elf.c \
	dump-hp9k.c \
	dump-vanilla.c \
	heap-gen.c \
	heap-sc.c \
	stab-bsd.c \
	stab-coff.c \
	stab-convex.c \
	stab-ecoff.c \
	stab-hp9k300.c \
	stab-hp9k800.c \
	stab-macho.c \
	$(NULL)

bin_PROGRAMS = elk
elk_LDADD = $(top_builddir)/src/libelk.la
elk_SOURCES = main.c

install-exec-local:
	rm -f "$(DESTDIR)$(bindir)/scheme-elk"
	ln -s elk "$(DESTDIR)$(bindir)/scheme-elk"

