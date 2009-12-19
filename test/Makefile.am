# $Id$

EXTRA_DIST = README check-build r4rstest.scm \
             check-gc.in check-gc2.in check-r4rs.in
DISTCLEANFILES = check-gc check-gc2 check-r4rs

if NATIVE_BUILD
native_tests = check-gc check-gc2 check-r4rs
endif

TESTS = check-build $(native_tests)

