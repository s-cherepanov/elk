
CLEANFILES = $(allstamps) mytest.scm tmp1 tmp2 tmp3

allstamps = stamp-r4rs

if NATIVE_BUILD
all-local: $(allstamps)
endif

stamp-r4rs: $(top_builddir)/src/elk
	rm -f $@ mytest.scm

	sed -e 's/r4rstest/mytest/g' < $(srcdir)/r4rstest.scm > mytest.scm
	echo '(test-cont) (test-sc4) (test-delay)' >> mytest.scm

	-$(top_builddir)/src/elk -p .:$(top_srcdir)/scm -l mytest.scm
	rm -f mytest.scm tmp1 tmp2 tmp3
	printf "" > $@

