
CLEANFILES = $(allstamps) tmp1 tmp2 tmp3

allstamps = stamp-r4rs

if NATIVE_BUILD
all-local: $(allstamps)
endif

stamp-r4rs: $(top_builddir)/src/elk
	rm -f $@
	-$(top_builddir)/src/elk -p .:$(top_srcdir)/scm -l r4rstest.scm
	rm -f tmp1 tmp2 tmp3
	printf "" > $@

