AC_INIT(src/main.c)

AC_PREREQ(2.50)
AC_CONFIG_AUX_DIR(autotools)
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE(elk, 3.99.0)
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
AC_DEFINE(REGCOMP, 1, [Foo])
AC_DEFINE(WAITPID, 1, [Foo])
AC_DEFINE(WAIT3, 1, [Foo])
AC_DEFINE(WAIT4, 1, [Foo])
AC_DEFINE(MKTEMP, 1, [Foo])
AC_DEFINE(TMPNAM, 1, [Foo])
AC_DEFINE(TEMPNAM, 1, [Foo])
AC_DEFINE(GETCWD, 1, [Foo])
AC_DEFINE(GETWD, 1, [Foo])
AC_DEFINE(RENAME, 1, [Foo])
AC_DEFINE(UNAME, 1, [Foo])
AC_DEFINE(GETHOSTNAME, 1, [Foo])
AC_DEFINE(GETTIMEOFDAY, 1, [Foo])
AC_DEFINE(FTIME, 1, [Foo])
AC_DEFINE(VFORK, 1, [Foo])
AC_DEFINE(VPRINTF, 1, [Foo])
AC_DEFINE(DIRENT, 1, [Foo])
AC_DEFINE(RANDOM, 1, [Foo])
AC_DEFINE(INCLUDE_UNISTD_H, 1, [Foo])
if false; then
  AC_DEFINE(SYSCONF_OPEN_MAX, 1, [Bar])
fi
AC_DEFINE(GETDTABLESIZE, 1, [Foo])
AC_DEFINE(PATHCONF_PATH_MAX, 1, [Foo])
AC_DEFINE(GETPAGESIZE, 1, [Foo])
if false; then
  AC_DEFINE(SYSCONF_PAGESIZE, 1, [Bar])
fi
if false; then
  AC_DEFINE(BSD_SIGNALS, 1, [Bar])
fi
AC_DEFINE(POSIX_SIGNALS, 1, [Foo])
if false; then
  AC_DEFINE(ALIGN_8BYTE, 1, [Bar])
fi
if false; then
  AC_DEFINE(COFF, 1, [Bar])
fi
if false; then
  AC_DEFINE(ECOFF, 1, [Bar])
fi
if false; then
  AC_DEFINE(XCOFF, 1, [Bar])
fi
AC_DEFINE(ELF, 1, [Foo])
if false; then
  AC_DEFINE(MACH_O, 1, [Bar])
fi
if false; then
  AC_DEFINE(CONVEX_AOUT, 1, [Bar])
fi
if false; then
  AC_DEFINE(HP9K, 1, [Bar])
fi
AC_DEFINE(HPSHLIB, 1, [Foo])
AC_DEFINE(DEBUG_DUMP, 1, [Foo])
AC_DEFINE(CAN_LOAD_OBJ, 1, [Foo])
AC_DEFINE(CAN_LOAD_LIB, 1, [Foo])
if false; then
  AC_DEFINE(USE_LD, 1, [Bar])
fi
if false; then
  AC_DEFINE(USE_RLD, 1, [Bar])
fi
if false; then
  AC_DEFINE(USE_SHL, 1, [Bar])
fi
AC_DEFINE(USE_DLOPEN, 1, [Foo])
AC_DEFINE(LOAD_LIBRARIES, "", [Foo])
AC_DEFINE(CACHECTL_H, <sys/cachectl.h>, [Foo])
AC_DEFINE(FIONREAD_H, <termios.h>, [Foo])
AC_DEFINE(ATEXIT, 1, [Foo])
if false; then
  AC_DEFINE(SYMS_BEGIN_WITH, 1, [Bar])
fi
AC_DEFINE(CAN_DUMP, 1, [Foo])
AC_DEFINE(SEG_SIZ, 1024, [Foo])
AC_DEFINE(FILE_TEXT_START, N_TXTOFF(hdr), [Foo])
AC_DEFINE(MEM_TEXT_START, 0, [Foo])
AC_DEFINE(TEXT_LENGTH_ADJ, 0, [Foo])
AC_DEFINE(COFF_PAGESIZE, 4096, [Foo])
if false; then
  AC_DEFINE(FCHMOD_BROKEN, 1, [Bar])
fi
AC_DEFINE(TERMIO, 1, [Foo])
if false; then
  AC_DEFINE(FLUSH_BSD, 1, [Bar])
fi
if false; then
  AC_DEFINE(FLUSH_FPURGE, 1, [Bar])
fi
if false; then
  AC_DEFINE(FLUSH_TIOCFLUSH, 1, [Bar])
fi
AC_DEFINE(FLUSH_TCFLSH, 1, [Foo])
AC_DEFINE(USE_ALLOCA, 1, [Foo])
AC_DEFINE(INCLUDE_ALLOCA_H, 1, [Foo])
if false; then
  AC_DEFINE(PRAGMA_ALLOCA, 1, [Bar])
fi
if false; then
  AC_DEFINE(MAX_STACK_SIZE, 1, [Bar])
fi
AC_DEFINE(GENERATIONAL_GC, 1, [Foo])
if false; then
  AC_DEFINE(HAS_MPROTECT, 1, [Bar])
fi
if false; then
  AC_DEFINE(MPROTECT_MMAP, 1, [Bar])
fi
if false; then
  AC_DEFINE(SIGSEGV_SIGINFO, 1, [Bar])
fi
if false; then
  AC_DEFINE(SIGSEGV_SIGCONTEXT, 1, [Bar])
fi
if false; then
  AC_DEFINE(SIGSEGV_ARG4, 1, [Bar])
fi
if false; then
  AC_DEFINE(SIGSEGV_AIX, 1, [Bar])
fi
if false; then
  AC_DEFINE(SIGSEGV_HPUX, 1, [Bar])
fi

AC_DEFINE(AOUT_H, <a.out.h>, [Foo])
AC_DEFINE(SCM_DIR, "/usr/share/elk", [Foo])
AC_DEFINE(OBJ_DIR, "/usr/lib/elk", [Foo])
AC_DEFINE(HEAP_SIZE, 1024, [Foo])
AC_DEFINE(FIND_AOUT, 1, [Foo])
AC_DEFINE(ANSI_CPP, 1, [Foo])
AC_DEFINE(SYSTEMTYPE, "i486-linux-gcc", [Foo])
AC_DEFINE(GETGROUPS_TYPE, gid_t, [Foo])
AC_DEFINE(LD_NAME, "ld", [Foo])
AC_DEFINE(LDFLAGS_SHARED, "-shared", [Foo])
AC_DEFINE(INC_LDFLAGS, "-x -static", [Foo])
AC_DEFINE(UTIME_H, 1, [Foo])
AC_DEFINE(INIT_PREFIX, "elk_init_", [Foo])
AC_DEFINE(FINIT_PREFIX, "elk_finit_", [Foo])
AC_DEFINE(ELK_MAJOR, 3, [Foo])
AC_DEFINE(ELK_MINOR, 0, [Foo])

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
  lib/misc/Makefile
  scm/Makefile
  scripts/Makefile
  src/Makefile
  util/Makefile
])

