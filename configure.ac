# $Id$

AC_INIT(src/main.c)

AC_PREREQ(2.50)
AC_CONFIG_AUX_DIR(autotools)
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE(elk, 3.99.5)
AM_CONFIG_HEADER(config.h)

AC_PROG_CC
AM_PROG_CC_C_O
AC_PROG_CXX
AC_LIBTOOL_WIN32_DLL
AC_PROG_LIBTOOL
AC_STDC_HEADERS

AC_C_CONST
AC_C_INLINE
AC_TYPE_SIZE_T

# Check whether we are doing a native build
AM_CONDITIONAL(NATIVE_BUILD, test "${cross_compiling}" = "no")

AC_CHECK_HEADERS(inttypes.h,
  [STDINT_HEADER="<inttypes.h>"],
  [AC_CHECK_HEADERS(sys/inttypes.h,
    [STDINT_HEADER="<sys/inttypes.h>"],
    [AC_CHECK_HEADERS(sys/_inttypes.h,
      [STDINT_HEADER="<sys/_inttypes.h>"],
      [AC_CHECK_HEADERS(stdint.h,
        [STDINT_HEADER="<stdint.h>"],
        [AC_MSG_ERROR([cannot find C99 integer headers])])])])])
AC_DEFINE_UNQUOTED(STDINT_HEADER, [${STDINT_HEADER}], [Define the C99 integer types header])
AC_SUBST(STDINT_HEADER)

# $system should contain the name of this file.  It may be used by some
# of the build scripts to do things that are specific to one single
# type of system.
AC_DEFINE_UNQUOTED(SYSTEMTYPE, ["${target_os}"], [Our operating system])

MATH_LIBS="-lm"
case "${target_os}" in
  *hpux*)
    AC_DEFINE(ARRAY_BROKEN, 1, [Define if a-=1000; a[1000] doesn't work])
    ;;
  *mingw32*)
    ELK_CFLAGS="${ELK_CFLAGS} -DELK_BUILD_DLL"
    ELK_LDFLAGS="${ELK_LDFLAGS} -no-undefined"
    MATH_LIBS=""
    ;;
  *darwin*)
    AC_DEFINE(SYS_DARWIN, 1, Define if the system is Darwin)
    ;;
esac
ELK_LIBS="${ELK_LIBS} ${MATH_LIBS}"

# Does the system support the vprintf library function?  If not,
# availability of the (non-portable) _doprnt function is assumed.
AC_CHECK_FUNCS(vprintf)

# Does the directory(3) library follow the POSIX conventions (i.e.
# requires the <dirent.h> include file and uses "struct dirent")?
# If not, the (obsolete) BSD-style interface with <sys/dir.h> and
# "struct direct" is assumed.
AC_CHECK_FUNCS(dirent)

# Does the system have the random/srandom library functions?  If not,
# rand/srand will be used instead.
AC_CHECK_FUNCS(random)

# Does the system have the index library function?  If not, strchr
# will be used.
AC_CHECK_FUNCS(index)

# Does the system have the bcopy, bzero, and bcmp library functions?
# If not, memcpy/memset/memcmp will be used.
dnl FIXME

# Does using the access system call require <unistd.h> to be included?
# (Look into the manual page for access if in doubt.)
AC_CHECK_HEADERS(unistd.h)

# If the FIONREAD ioctl command is defined, which file must be included?
AC_CHECK_HEADERS(termios.h sys/ioctl.h sys/filio.h)

# If getdtablesize() is available to determine the maximum number of open
# files per process, set getdtablesize=yes.
# Alternatively, if POSIX-style sysconf() can be called with _SC_OPEN_MAX,
# set sysconf_open_max=yes.
# If neither is set to "yes", an educated guess will be made.
AC_CHECK_FUNCS(getdtablesize)

# If POSIX-style pathconf() can be invoked with _PC_PATH_MAX to determine
# the maximum pathname length, set pathconf_path_max=yes.

# If the system page size can be determined by calling getpagesize()
# set getpagesize=yes.
# Alternatively, if sysconf() can be invoked with _SC_PAGESIZE, set
# sysconf_pagesize=yes.
# These two variables are only required if the generational garbage
# collector is used.
AC_CHECK_FUNCS(getpagesize)

# Set reliable_signals=bsd if your system supports BSD-style reliable
# signals (has sigblock and related functions); set reliable_signals=posix
# for POSIX-style signals (sigprocmask, sigsets); otherwise old V7/SysV
# signal semantics are assumed.
AC_CHECK_HEADERS(signal.h)
AC_CHECK_FUNCS(sigprocmask sigblock)

# To support dynamic loading of object files and "dump", the system's
# a.out format has to be known.  Choose one of the following:
#
#     coff  ecoff  xcoff  elf  macho  hp9k  convex
#
# Other values of "aout_format" are interpreted as BSD-style a.out format.
if false; then
  AC_DEFINE(COFF, 1, [FIXME HARD])
fi

if false; then
  AC_DEFINE(ECOFF, 1, [FIXME HARD])
fi

if false; then
  AC_DEFINE(XCOFF, 1, [FIXME HARD])
fi

AC_CHECK_LIB(elf, elf_begin, ac_cv_my_have_elf=yes, ac_cv_my_have_elf=no)
AM_CONDITIONAL(HAVE_LIBELF, test "${ac_cv_my_have_elf}" = "yes")

if false; then
  AC_DEFINE(MACH_O, 1, [FIXME HARD])
fi

if false; then
  AC_DEFINE(CONVEX_AOUT, 1, [FIXME HARD])
fi

if false; then
  AC_DEFINE(HP9K, 1, [FIXME HARD])
fi

if test "${ac_cv_my_have_elf}" = "yes"; then
  AC_DEFINE(HAVE_LIBELF, 1, Define if you have libelf.)
  ELK_LIBS="${ELK_LIBS} -lelf"
  ac_cv_my_can_dump=yes
else
  ac_cv_my_can_dump=no
  dnl AC_MSG_ERROR([could not handle the native object format, if you are running Linux please install the libelf development headers])
fi

# Which mechanism should be used to dynamically load object files?
ac_cv_my_can_load_lib=no

# OS X style
AC_CHECK_HEADERS(mach-o/dyld.h,
  [AC_CHECK_FUNCS(NSLinkModule,
    [AC_DEFINE(HAVE_DL_DYLD, 1, [Define if you have the Darwin dyld API])
     AC_DEFINE(SYMS_BEGIN_WITH, ['_'], [Define if symbols start with '_'])
     ac_cv_my_can_load_lib=yes])])

# HP-UX style
if test "${ac_cv_my_can_load_lib}" = "no"; then
  AC_CHECK_HEADERS(dl.h)
  ac_cv_my_have_shl_load=no
  AC_CHECK_FUNC(shl_load,
   [ac_cv_my_have_shl_load=yes,
    AC_CHECK_LIB(dld, shl_load,
     [ac_cv_my_have_shl_load=yes
      ELK_LIBS="${ELK_LIBS} -ldld"])])
  if test "${ac_cv_my_have_shl_load}" = "yes"; then
    AC_DEFINE(HAVE_DL_SHL_LOAD, 1, [Define if you have the shl_load API])
    ac_cv_my_can_load_lib=yes
  fi
fi

# Whatever style
if test "${ac_cv_my_can_load_lib}" = "no"; then
  AC_CHECK_LIB(dld, dld_link,
   [ELK_LIBS="${ELK_LIBS} -ldld"
    AC_DEFINE(HAVE_DL_DLD_LINK, 1, [Define if you have the GNU dld library])
    ac_cv_my_can_load_lib=yes])
fi

# Win32 style
if test "${ac_cv_my_can_load_lib}" = "no"; then
  AC_CHECK_LIB(kernel32, main,
   [ELK_LIBS="${ELK_LIBS} -lkernel32"
    ac_cv_my_can_load_lib=yes])
fi

AC_CHECK_HEADERS(a.out.h)

# Only test for dlopen() if the others didn't work
if test "${ac_cv_my_can_load_lib}" = "no"; then
  AC_CHECK_HEADERS(dlfcn.h sys/dl.h)
  ac_cv_my_have_dlopen=no
  AC_CHECK_FUNC(dlopen,
    ac_cv_my_have_dlopen=yes,
    AC_CHECK_LIB(dl, dlopen,
      ac_cv_my_have_dlopen=yes
      ELK_LIBS="${ELK_LIBS} -ldl",
      AC_CHECK_LIB(svld, dlopen,
        ac_cv_my_have_dlopen=yes
        ELK_LIBS="${ELK_LIBS} -lsvld")))
  if test "${ac_cv_my_have_dlopen}" = "yes"; then
    AC_DEFINE(HAVE_DL_DLOPEN, 1, [Define if you have the dlopen API])
    ac_cv_my_can_load_lib=yes
  fi
fi

if test "${ac_cv_my_can_load_lib}" = "yes"; then
  AC_DEFINE(CAN_LOAD_LIB, 1, [Define if dynamic loading is supported])
fi

# Systems with "aout_format=ecoff" may require a call to the cacheflush
# system call after an object file has been loaded.  Which include file
# has to be included in this case?
AC_DEFINE(CACHECTL_H, <sys/cachectl.h>, [FIXME HARD])

# Is the ANSI-C atexit function supported to register an exit handler?
# If not, the exit library function will be redefined and will end in
# a call to _exit.
AC_CHECK_FUNCS(atexit)

# The symbol prefixes of extension initialization and finalization
# functions (without the initial $syms_begin_with).  Do not change
# these unless the compiler or linker restricts the length of symbols!
AC_DEFINE(INIT_PREFIX, "elk_init_", [FIXME HARD])
AC_DEFINE(FINIT_PREFIX, "elk_finit_", [FIXME HARD])

# Is the "dump" function supported?
if test "${ac_cv_my_can_dump}" = "yes"; then
  AC_DEFINE(CAN_DUMP, 1, [FIXME HARD])
fi

    # Is the fchmod system call broken or unavailable?
    if false; then
      AC_DEFINE(FCHMOD_BROKEN, 1, [FIXME HARD])
    fi

    # These four variables are only relevant if the system has the BSD-style
    # a.out format.
    # segment_size is the segment size of the system's memory management
    # unit, i.e. the number to a multiple of which the size of an a.out
    # segment (e.g. .text) is rounded up.
    # file_text_start is the file offset at which the text segment starts
    # in an a.out file.
    # mem_text_start is the starting address of the text segment in memory.
    # text_length_adj must be set to "sizeof (struct exec)" if the length of
    # the text segment stored in the a.out header includes the a.out header
    # itself.
    AC_DEFINE(SEG_SIZ, 1024, [FIXME HARD])
    AC_DEFINE(FILE_TEXT_START, N_TXTOFF(hdr), [FIXME HARD])
    AC_DEFINE(MEM_TEXT_START, 0, [FIXME HARD])
    AC_DEFINE(TEXT_LENGTH_ADJ, 0, [FIXME HARD])

    # Only relevant if "aout_format=coff": the system's pagesize.
    AC_DEFINE(COFF_PAGESIZE, 4096, [FIXME HARD])

    # Only relevant if "aout_format=hp9k" and "load_obj=shl"
    AC_DEFINE(HPSHLIB, 1, [FIXME HARD])

    # Print debug messages when dumping
    AC_DEFINE(DEBUG_DUMP, 1, [FIXME HARD])

# Is the "termio" terminal interface supported by the system?  If not,
# BSD-style tty handling will be used.
AC_DEFINE(TERMIO, 1, [FIXME HARD])

# flush_stdio and flush_tty indicate how clear-input/output-port can
# flush (purge) a FILE pointer and a TTY file descriptor.
# Possible values of flush_stdio:
#    bsd         assume old BSD-style FILE* (with _cnt, _ptr, _base)
#    fpurge      use 4.4BSD-style fpurge stdio library function
# Possible values of flush_tty:
#    tiocflush   use TIOCFLUSH ioctl from <sys/ioctl.h>
#    tcflsh      use TCFLSH ioctl from <termio.h>
# Leave the variable(s) empty if flushing is not supported.
AC_CHECK_FUNCS(fpurge)

AC_CACHE_CHECK([for BSD-style flushing],
  [ac_cv_have_bsd_flush],
  [AC_TRY_COMPILE(
    [#include <stdio.h>],
    [FILE *f;
     f->_cnt = 0;
     f->_ptr = f->_base;],
    ac_cv_have_bsd_flush=yes,
    ac_cv_have_bsd_flush=no)])
if test "${ac_cv_have_bsd_flush}" = "yes"; then
  AC_DEFINE(HAVE_BSD_FLUSH, 1, [Define if you have BSD-style flushing])
fi

AC_CHECK_HEADERS(termio.h termios.h)

# The interpreter uses the getrlimit function to determine the maximum
# stack size of the running program.  If this function is not supported,
# set max_stack_size to a (fixed) maximum stack size (in bytes).
AC_CHECK_FUNCS(getrlimit)
AC_DEFINE(DEFAULT_MAX_STACK_SIZE, 1024*1024, [Define default max stack size])

# Is the mprotect system call supported?  The generational garbage collector
# requires mprotect to implement incremental GC.  $mprotect is ignored if
# generational_gc is set to "no" in the site file.  Set mprotect=mmap if
# mprotect is supported, but only for mmap()ed memory.
AC_CHECK_FUNCS(mprotect)
if false; then
  AC_DEFINE(MPROTECT_SIG, 1, [FIXME HARD])
fi
if false; then
  AC_DEFINE(MPROTECT_MMAP, 1, [FIXME HARD])
fi

# How can a SIGSEGV or SIGBUS signal handler find out the address of
# the faulting memory reference?  This variable is only used if
# $mprotect is "yes" or "mmap".  Possible values are:
#
#   siginfo     handler is called with siginfo_t structure (enabled
#               by a call to sigaction)
#   sigcontext  address is in the sigcontext structure (3rd arg, sc_badvaddr)
#   arg4        address is delivered to handler as argument #4
#   aix         use an AIX-specific hack to get hold of the bad address
#   hpux        use a HP-UX-specific hack
if false; then
  AC_DEFINE(SIGSEGV_SIGINFO, 1, [FIXME HARD])
fi
if false; then
  AC_DEFINE(SIGSEGV_SIGCONTEXT, 1, [FIXME HARD])
fi
if false; then
  AC_DEFINE(SIGSEGV_ARG4, 1, [FIXME HARD])
fi
if false; then
  AC_DEFINE(SIGSEGV_AIX, 1, [FIXME HARD])
fi
if false; then
  AC_DEFINE(SIGSEGV_HPUX, 1, [FIXME HARD])
fi

# Does the system support the alloca library function, and does this
# function actually extend the stack?  If in doubt, extract alloca.o
# from the C library and check if it contains the symbols malloc and free.
# If this is the case, forget it.
AC_FUNC_ALLOCA
dnl AC_CHECK_FUNCS(alloca)

# Must <alloca.h> be included to use alloca?  Is "#pragma alloca" required?
AC_CHECK_HEADERS(alloca.h)
if false; then
  AC_DEFINE(PRAGMA_ALLOCA, 1, [FIXME HARD])
fi

# Does the system (or compiler) require certain objects (e.g. doubles)
# to be aligned at 8-byte boundaries?  If not, 4-byte alignment will
# be assumed.
if false; then
  AC_DEFINE(ALIGN_8BYTE, 1, [FIXME HARD])
fi

# The name of the linker.  This is usually just "ld", or /usr/ccs/bin/ld
# in SVR4-based systems.
AC_DEFINE(LD_NAME, "ld", [FIXME HARD])

# Does your C preprocessor support the ANSI-C ## operator, although
# __STDC__ is not defined?
AC_DEFINE(ANSI_CPP, 1, [FIXME HARD])

# The UNIX extension likes to know which of the following system calls,
# library functions, and include files are supported by the system.
AC_CHECK_HEADERS(utime.h sys/utime.h sys/wait.h sys/times.h dirent.h netdb.h)
AC_CHECK_FUNCS(waitpid wait3 wait4 vfork uname gethostname gettimeofday ftime)
AC_CHECK_FUNCS(mktemp tmpnam tempnam getcwd getwd rename regcomp environ)
AC_MSG_CHECKING(for environ in unistd.h)
AC_EGREP_HEADER(__environ, unistd.h,
 [AC_MSG_RESULT(yes)
  AC_DEFINE(__ENVIRON_IN_UNISTD_H, 1, Define if <unistd.h> defines __environ)],
 [AC_MSG_RESULT(no)])
AC_MSG_CHECKING(for environ in unistd.h)
AC_EGREP_HEADER(environ, unistd.h,
 [AC_MSG_RESULT(yes)
  AC_DEFINE(ENVIRON_IN_UNISTD_H, 1, Define if <unistd.h> defines environ)],
 [AC_MSG_RESULT(no)])

# Element type of the gidset argument of getgroups(); typically int
# or gid_t.  Only needed by the UNIX extension.
AC_DEFINE(GETGROUPS_TYPE, gid_t, [FIXME HARD])

# Do you want to use the generational garbage collector?  If not, the
# stop-and-copy garbage collector will be used.
AC_DEFINE(GENERATIONAL_GC, 1, [FIXME HARD])

# The default heap size of the Scheme interpreter in KBytes (if the
# stop-and-copy garbage collector is used).
AC_DEFINE(HEAP_SIZE, 1024, [FIXME HARD])

# The directory where all files are installed by running "make install".
# The subdirectories bin, lib, include, and runtime (with various
# subdirectories) are created automatically, but $install_dir isn't.
# Make sure $install_dir doesn't point to the top of the source tree
# (i.e. choose a subdirectory or a directory outside the source tree).
AC_DEFINE_UNQUOTED(SCM_DIR, "${prefix}/share/elk", [Data directory])
AC_DEFINE_UNQUOTED(LIB_DIR, "${prefix}/lib/elk", [Plugins directory])

#define FIND_AOUT defined(USE_LD) || defined(CAN_DUMP) || defined(INIT_OBJECTS)
AC_DEFINE(FIND_AOUT, 1, [FIXME HARD])

AC_CHECK_HEADERS(pwd.h grp.h sys/resource.h)

dnl
dnl  Check for available compiler features
dnl
CFLAGS_save="${CFLAGS}"
LDFLAGS_save="${LDFLAGS}"

AC_CACHE_CHECK([if \$CXX really works],
  [ac_cv_my_have_cxx],
  [AC_LANG_PUSH(C++)
   AC_TRY_COMPILE([],,ac_cv_my_have_cxx=yes, ac_cv_my_have_cxx=no)
   AC_LANG_POP(C++)])
AM_CONDITIONAL(HAVE_CXX, test "${ac_cv_my_have_cxx}" = "yes")

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

dnl
dnl  Check for available headers and libraries
dnl
CFLAGS="${CFLAGS_save}"
LDFLAGS="${LDFLAGS_save}"

AM_CONDITIONAL(HAVE_MONITOR, false)

AC_CHECK_LIB(gdbm, gdbm_open, ac_cv_my_have_gdbm=yes, ac_cv_my_have_gdbm=no)
AM_CONDITIONAL(HAVE_GDBM, test "${ac_cv_my_have_gdbm}" = "yes")

AC_PATH_X

AC_CHECK_LIB(X11, XOpenDisplay,
 [ac_cv_my_have_x11=yes
  X_CFLAGS="-I${x_includes}"
  X_LIBS="-lX11 -L${x_libraries}"],
 [ac_cv_my_have_x11=no],
 [[-lXt -L${x_libraries}]])
AM_CONDITIONAL(HAVE_X11, test "${ac_cv_my_have_x11}" = "yes")
AC_SUBST(X_CFLAGS)
AC_SUBST(X_LIBS)

if test "${cross_compiling}" = "no"; then
  AC_CHECK_LIB(Xmu, XmuDrawLogo,
   [ac_cv_my_have_xmu=yes
    XMU_CFLAGS="-I${x_includes}"
    XMU_LIBS="-L${x_libraries} -lXt -lSM -lICE -lXext -lX11"],
   [ac_cv_my_have_xmu=no],
   [[-lXt -lSM -lICE -lXext -lX11 -L${x_libraries}]])
else
  ac_cv_my_have_xmu="no (cross-compiling)"
fi
AM_CONDITIONAL(HAVE_XMU, test "${ac_cv_my_have_xmu}" = "yes")
AC_SUBST(XMU_CFLAGS)
AC_SUBST(XMU_LIBS)

if test "${cross_compiling}" = "no"; then
  AC_CHECK_LIB(Xaw, XawTextSearch,
   [ac_cv_my_have_xaw=yes
    XAW_CFLAGS="-I${x_includes}"
    XAW_LIBS="-L${x_libraries} -lXaw ${XMU_LIBS}"],
   [ac_cv_my_have_xaw=no],
   [[${XMU_LIBS}]])
else
  ac_cv_my_have_xaw="no (cross-compiling)"
fi
AM_CONDITIONAL(HAVE_XAW, test "${ac_cv_my_have_xaw}" = "yes")
AC_SUBST(XAW_CFLAGS)
AC_SUBST(XAW_LIBS)

if test "${cross_compiling}" = "no"; then
  AC_CHECK_LIB(Xm, XmStringConcat,
   [ac_cv_my_have_motif=yes
    MOTIF_CFLAGS="-I${x_includes} -I/usr/include/Xm"
    MOTIF_LIBS="-L${x_libraries} -lXm ${XMU_LIBS}"],
   [ac_cv_my_have_motif=no],
   [[${XMU_LIBS}]])
else
  ac_cv_my_have_motif="no (cross-compiling)"
fi
AM_CONDITIONAL(HAVE_MOTIF, test "${ac_cv_my_have_motif}" = "yes")
AC_SUBST(MOTIF_CFLAGS)
AC_SUBST(MOTIF_LIBS)

INCLUDES="${INCLUDES} -I\$(top_srcdir)/include -I\$(top_builddir)/include"
AC_SUBST(INCLUDES)

dnl  Export variables
AC_SUBST(ELK_CFLAGS)
AC_SUBST(ELK_LDFLAGS)
AC_SUBST(ELK_LIBS)

dnl
dnl  Check for available programs
dnl
AC_PROG_AWK
AC_MSG_CHECKING(for groff -ms)
if echo foo | groff -ms >/dev/null 2>&1; then
  ac_cv_my_have_groff=yes
else
  ac_cv_my_have_groff=no
fi
AC_MSG_RESULT([${ac_cv_my_have_groff}])
AM_CONDITIONAL(HAVE_GROFF, test "${ac_cv_my_have_groff}" = "yes")


dnl
dnl  Finished!
dnl
AC_OUTPUT([
  Makefile
  autotools/Makefile
  debian/Makefile
  doc/Makefile
  doc/bitstring/Makefile
  doc/cprog/Makefile
  doc/kernel/Makefile
  doc/man/Makefile
  doc/oops/Makefile
  doc/record/Makefile
  doc/regexp/Makefile
  doc/unix/Makefile
  doc/usenix/Makefile
  doc/util/Makefile
  doc/xlib/Makefile
  doc/xt/Makefile
  examples/Makefile
  examples/tests/Makefile
  include/Makefile
  lib/Makefile
  lib/misc/Makefile
  lib/unix/Makefile
  lib/xlib/Makefile
  lib/xwidgets/Makefile
  lib/xwidgets/xaw/Makefile
  lib/xwidgets/motif/Makefile
  scm/Makefile
  src/Makefile

  scm/slib.scm
  include/site.h
])

cat << EOF

Elk configuration summary
-------------------------
build C++ plugins: ${ac_cv_my_have_cxx}
libgdbm support: ${ac_cv_my_have_gdbm}
X11 support: ${ac_cv_my_have_x11}
Xaw support: ${ac_cv_my_have_xaw}
Motif support: ${ac_cv_my_have_motif}
build documentation: ${ac_cv_my_have_groff}
EOF

