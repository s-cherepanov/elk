# $Id$

AC_INIT(src/main.c)

AC_PREREQ(2.50)
AC_CONFIG_AUX_DIR(autotools)
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE(elk, 3.99.0)
AM_CONFIG_HEADER(config.h)

AC_PROG_CC
AM_PROG_CC_C_O
AC_PROG_CXX
AM_CONDITIONAL(HAVE_CXX, test -n "$CXX")
AC_PROG_LIBTOOL
AC_STDC_HEADERS

AC_C_CONST
AC_C_INLINE
AC_TYPE_SIZE_T

AC_CHECK_HEADERS(stdint.h inttypes.h sys/_inttypes.h)

# $system should contain the name of this file.  It may be used by some
# of the build scripts to do things that are specific to one single
# type of system.
AC_DEFINE(SYSTEMTYPE, "i486-linux-gcc", [FIXME HARD])

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
dnl FIXME

# Does the system have the bcopy, bzero, and bcmp library functions?
# If not, memcpy/memset/memcmp will be used.
dnl FIXME

# Does using the access system call require <unistd.h> to be included?
# (Look into the manual page for access if in doubt.)
AC_CHECK_HEADERS(unistd.h)

# If the FIONREAD ioctl command is defined, which file must be included?
AC_DEFINE(FIONREAD_H, <termios.h>, [FIXME HARD])

# What is the name of the a.out include file?
AC_DEFINE(AOUT_H, <a.out.h>, [FIXME HARD])

# If getdtablesize() is available to determine the maximum number of open
# files per process, set getdtablesize=yes.
# Alternatively, if POSIX-style sysconf() can be called with _SC_OPEN_MAX,
# set sysconf_open_max=yes.
# If neither is set to "yes", an educated guess will be made.
AC_CHECK_FUNCS(getdtablesize)
AC_MSG_CHECKING(for _SC_OPEN_MAX in unistd.h)
AC_EGREP_HEADER(_SC_OPEN_MAX, unistd.h, [
  AC_MSG_RESULT(yes) AC_DEFINE(SC_OPEN_MAX_IN_UNISTD_H, 1, Define if <unistd.h> defines _SC_OPEN_MAX)
],[
  AC_MSG_RESULT(no)
])

# If POSIX-style pathconf() can be invoked with _PC_PATH_MAX to determine
# the maximum pathname length, set pathconf_path_max=yes.
AC_MSG_CHECKING(for _PC_PATH_MAX in unistd.h)
AC_EGREP_HEADER(_PC_PATH_MAX, unistd.h, [
  AC_MSG_RESULT(yes) AC_DEFINE(PC_PATH_MAX_IN_UNISTD_H, 1, Define if <unistd.h> defines _PC_PATH_MAX)
],[
  AC_MSG_RESULT(no)
])

# If the system page size can be determined by calling getpagesize()
# set getpagesize=yes.
# Alternatively, if sysconf() can be invoked with _SC_PAGESIZE, set
# sysconf_pagesize=yes.
# These two variables are only required if the generational garbage
# collector is used.
AC_CHECK_FUNCS(getpagesize)
AC_MSG_CHECKING(for _SC_PAGESIZE in unistd.h)
AC_EGREP_HEADER(_SC_PAGESIZE, unistd.h, [
  AC_MSG_RESULT(yes) AC_DEFINE(SC_PAGESIZE_IN_UNISTD_H, 1, Define if <unistd.h> defines _SC_PAGESIZE)
],[
  AC_MSG_RESULT(no)
])

# Set reliable_signals=bsd if your system supports BSD-style reliable
# signals (has sigblock and related functions); set reliable_signals=posix
# for POSIX-style signals (sigprocmask, sigsets); otherwise old V7/SysV
# signal semantics are assumed.
if false; then
  AC_DEFINE(BSD_SIGNALS, 1, [FIXME HARD])
fi
AC_DEFINE(POSIX_SIGNALS, 1, [FIXME HARD])

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
AC_DEFINE(ELF, 1, [FIXME HARD])
if false; then
  AC_DEFINE(MACH_O, 1, [FIXME HARD])
fi
if false; then
  AC_DEFINE(CONVEX_AOUT, 1, [FIXME HARD])
fi
if false; then
  AC_DEFINE(HP9K, 1, [FIXME HARD])
fi

# Which mechanism should be used to dynamically load object files?
# Possible values currently are:
#
#    ld        BSD-style incremental loading based on ld -A
#    rld       NeXT-style rld_load()
#    shl       HP-UX shl_load()
#    dl        SysVR4/SunOS5 dlopen()
#
# Leave load_obj empty if dynamic loading is not supported.
AC_DEFINE(CAN_LOAD_OBJ, 1, [FIXME HARD])
AC_DEFINE(CAN_LOAD_LIB, 1, [FIXME HARD])
if false; then
  AC_DEFINE(USE_LD, 1, [FIXME HARD])
fi
if false; then
  AC_DEFINE(USE_RLD, 1, [FIXME HARD])
fi
if false; then
  AC_DEFINE(USE_SHL, 1, [FIXME HARD])
fi
AC_DEFINE(USE_DLOPEN, 1, [FIXME HARD])

    # Linker options to produce a shared object from a .o file.
    # Only used if load_obj=dl.
    AC_DEFINE(LDFLAGS_SHARED, "-shared", [FIXME HARD])

    # The libraries against which dynamically loaded files are resolved
    # at the time they are loaded.
    AC_DEFINE(LOAD_LIBRARIES, "", [FIXME HARD])

    # Additional flags to be passed to the linker for an incremental
    # linker run (ld -A).  Ignored unless load_obj=ld.
    AC_DEFINE(INC_LDFLAGS, "-x -static", [FIXME HARD])

    # Systems with "aout_format=ecoff" may require a call to the cacheflush
    # system call after an object file has been loaded.  Which include file
    # has to be included in this case?
    AC_DEFINE(CACHECTL_H, <sys/cachectl.h>, [FIXME HARD])

    # Is the ANSI-C atexit function supported to register an exit handler?
    # If not, the exit library function will be redefined and will end in
    # a call to _exit.
    AC_CHECK_FUNCS(atexit)

# Do the names of external functions in the symbol table always begin
# with a special character (such as underline)?  If so, syms_begin_with
# should hold this character, otherwise leave it empty.
if false; then
  AC_DEFINE(SYMS_BEGIN_WITH, 1, [FIXME HARD])
fi

# The symbol prefixes of extension initialization and finalization
# functions (without the initial $syms_begin_with).  Do not change
# these unless the compiler or linker restricts the length of symbols!
AC_DEFINE(INIT_PREFIX, "elk_init_", [FIXME HARD])
AC_DEFINE(FINIT_PREFIX, "elk_finit_", [FIXME HARD])

# Is the "dump" function supported?
AC_DEFINE(CAN_DUMP, 1, [FIXME HARD])

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
if false; then
  AC_DEFINE(FLUSH_BSD, 1, [FIXME HARD])
fi
if false; then
  AC_DEFINE(FLUSH_FPURGE, 1, [FIXME HARD])
fi
if false; then
  AC_DEFINE(FLUSH_TIOCFLUSH, 1, [FIXME HARD])
fi
AC_DEFINE(FLUSH_TCFLSH, 1, [FIXME HARD])

# The interpreter uses the getrlimit function to determine the maximum
# stack size of the running program.  If this function is not supported,
# set max_stack_size to a (fixed) maximum stack size (in bytes).
if false; then
  AC_DEFINE(MAX_STACK_SIZE, 1, [FIXME HARD])
fi

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
AC_DEFINE(USE_ALLOCA, 1, [FIXME HARD])

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
AC_CHECK_HEADERS(utime.h)
AC_CHECK_FUNCS(waitpid wait3 wait4 vfork uname gethostname gettimeofday ftime)
AC_CHECK_FUNCS(mktemp tmpnam tempnam getcwd getwd rename regcomp)

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
AC_DEFINE(SCM_DIR, "/usr/share/elk", [FIXME HARD])
AC_DEFINE(OBJ_DIR, "/usr/lib/elk", [FIXME HARD])

#define FIND_AOUT defined(USE_LD) || defined(CAN_DUMP) || defined(INIT_OBJECTS)
AC_DEFINE(FIND_AOUT, 1, [FIXME HARD])

dnl
dnl  Check for available libraries
dnl
ac_cv_have_gdbm=no
AC_CHECK_LIB(gdbm, gdbm_open, ac_cv_have_gdbm=yes)
AM_CONDITIONAL(HAVE_GDBM, test "${ac_cv_have_gdbm}" = "yes")

dnl
dnl  Check for available compiler features
dnl
CFLAGS_save="${CFLAGS}"

AC_CACHE_CHECK([__attribute__ ((noreturn)) support with function pointers],
  [ac_cv_c_attribute_noreturn],
  [ac_cv_c_attribute_noreturn=no
    CFLAGS="${CFLAGS_save} -Werror"
    AC_TRY_COMPILE([],
    [extern void foobar() __attribute__ ((noreturn));],
    [ac_cv_c_attribute_noreturn=yes])
    CFLAGS="${CFLAGS_save}"])
if test "${ac_cv_c_attribute_noreturn}" != "no"; then
  AC_DEFINE(HAVE_ATTRIBUTE_NORETURN, 1, Support for __attribute__((noreturn)))
fi

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

X_CFLAGS="-I/usr/X11R6/include"
X_LIBS="-L/usr/X11R6/lib -lX11"

XAW_CFLAGS="-I/usr/X11R6/include"
XAW_LIBS="-L/usr/X11R6/lib -lXaw -lXmu -lXt -lSM -lICE -lXext -lX11"

XM_CFLAGS="-I/usr/X11R6/include -I/usr/include/Xm"
XM_LIBS="-L/usr/X11R6/lib -lXm -lXmu -lXt -lSM -lICE -lXext -lX11"


dnl  Export variables
AC_SUBST(INCLUDES)
AC_SUBST(X_CFLAGS)
AC_SUBST(X_LIBS)
AC_SUBST(XAW_CFLAGS)
AC_SUBST(XAW_LIBS)
AC_SUBST(XM_CFLAGS)
AC_SUBST(XM_LIBS)

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
  include/Makefile
  lib/Makefile
  lib/misc/Makefile
  lib/unix/Makefile
  lib/xlib/Makefile
  lib/xwidgets/Makefile
  lib/xwidgets/xaw/Makefile
  lib/xwidgets/motif/Makefile
  scm/Makefile
  scm/slib.scm
  src/Makefile
])

