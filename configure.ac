AC_INIT(src/main.c)

AC_PREREQ(2.50)
AC_CONFIG_AUX_DIR(autotools)
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE(cfingerd, 1.5.0)
AM_CONFIG_HEADER(config.h)

AC_PROG_CC
AC_PROG_LIBTOOL
AC_STDC_HEADERS

AC_C_CONST
AC_C_INLINE
AC_TYPE_SIZE_T

AC_CHECK_HEADERS(unistd.h pwd.h)

dnl
dnl  Check for available warning flags
dnl
CFLAGS_save="${CFLAGS}"

AC_CACHE_CHECK([if \$CC accepts -Wall],
    [ac_cv_c_Wall],
    [CFLAGS="-Wall ${CFLAGS_save}"
     AC_TRY_COMPILE([],,ac_cv_c_Wall=yes, ac_cv_c_Wall=no)])
if test "x${ac_cv_c_Wall}" != "xno"; then
    CFLAGS_save="-Wall ${CFLAGS_save}"
fi

AC_CACHE_CHECK([if \$CC accepts -Wsign-compare],
    [ac_cv_c_Wsign_compare],
    [CFLAGS="-Wsign-compare ${CFLAGS_save}"
     AC_TRY_COMPILE([],,ac_cv_c_Wsign_compare=yes, ac_cv_c_Wsign_compare=no)])
if test "x${ac_cv_c_Wsign_compare}" != "xno"; then
    CFLAGS_save="-Wsign-compare ${CFLAGS_save}"
fi

CFLAGS="${CFLAGS_save}"
INCLUDES="${INCLUDES} -I\$(top_srcdir)/include"

dnl
dnl  Default values
dnl
AC_DEFINE(BLAH, 1, [Define if blah])

dnl  Export variables
AC_SUBST(INCLUDES)

AC_OUTPUT([
  Makefile
  autotools/Makefile
  config/Makefile
  debian/Makefile
  doc/Makefile
  examples/Makefile
  lib/Makefile
  scm/Makefile
  scripts/Makefile
  src/Makefile
  util/Makefile
])

