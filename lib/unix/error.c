/* error.c
 *
 * $Id$
 *
 * Copyright 1990, 1991, 1992, 1993, 1994, 1995, Oliver Laumann, Berlin
 * Copyright 2002, 2003 Sam Hocevar <sam@zoy.org>, Paris
 *
 * This software was derived from Elk 1.2, which was Copyright 1987, 1988,
 * 1989, Nixdorf Computer AG and TELES GmbH, Berlin (Elk 1.2 has been written
 * by Oliver Laumann for TELES Telematic Services, Berlin, in a joint project
 * between TELES and Nixdorf Microprocessor Engineering, Berlin).
 *
 * Oliver Laumann, TELES GmbH, Nixdorf Computer AG and Sam Hocevar, as co-
 * owners or individual owners of copyright in this software, grant to any
 * person or company a worldwide, royalty free, license to
 *
 *    i) copy this software,
 *   ii) prepare derivative works based on this software,
 *  iii) distribute copies of this software or derivative works,
 *   iv) perform this software, or
 *    v) display this software,
 *
 * provided that this notice is not removed and that neither Oliver Laumann
 * nor Teles nor Nixdorf are deemed to have made any representations as to
 * the suitability of this software for any purpose nor are held responsible
 * for any defects of this software.
 *
 * THERE IS ABSOLUTELY NO WARRANTY FOR THIS SOFTWARE.
 */

#include "unix.h"

/* We can't know which error codes exist on a given platform.  The
 * following are the POSIX codes plus a few more that are available
 * almost everywhere.
 * Unfortunately, "(unix-errno)" has to return other error codes as
 * plain integers; they have to be dealt with in Scheme.
 */
static SYMDESCR Errno_Syms[] = {
    { "e2big",    E2BIG},
    { "eacces",   EACCES},
    { "eagain",   EAGAIN},
    { "ebadf",    EBADF},
    { "ebusy",    EBUSY},
    { "echild",   ECHILD},
#ifdef EDEADLK
    { "edeadlk",  EDEADLK},
#endif
    { "edom",     EDOM},
    { "eexist",   EEXIST},
    { "efault",   EFAULT},
    { "efbig",    EFBIG},
    { "eintr",    EINTR},
    { "einval",   EINVAL},
    { "eio",      EIO},
    { "eisdir",   EISDIR},
    { "emfile",   EMFILE},
    { "emlink",   EMLINK},
    { "enametoolong",   ENAMETOOLONG},
    { "enfile",   ENFILE},
    { "enodev",   ENODEV},
    { "enoent",   ENOENT},
    { "enoexec",  ENOEXEC},
#ifdef ENOLCK
    { "enolck",   ENOLCK},
#endif
    { "enomem",   ENOMEM},
    { "enospc",   ENOSPC},
#ifdef ENOSYS
    { "enosys",   ENOSYS},
#endif
    { "enotdir",  ENOTDIR},
    { "enotempty",   ENOTEMPTY},
    { "enotty",   ENOTTY},
    { "enxio",    ENXIO},
    { "eperm",    EPERM},
    { "epipe",    EPIPE},
    { "erange",   ERANGE},
    { "erofs",    EROFS},
    { "espipe",   ESPIPE},
    { "esrch",    ESRCH},
    { "exdev",    EXDEV},
#ifdef EWOULDBLOCK
    { "ewouldblock",   EWOULDBLOCK },
#endif
#ifdef ELOOP
    { "eloop",    ELOOP },
#endif
#ifdef EDQUOT
    { "edquot",   EDQUOT },
#endif
    { 0, 0 }
};

Object Unix_Errobj, V_Call_Errhandler;

static Object P_Errorp(Object x) {
    return EQ(x, Unix_Errobj) ? True : False;
}

static Object P_Errno() {
    Object sym;

    sym = Bits_To_Symbols(Saved_Errno, 0, Errno_Syms);
    return Nullp(sym) ? Make_Integer(Saved_Errno) : sym;
}

void elk_init_unix_error() {
    Unix_Errobj = Intern("*unix-error-object*");
    Unix_Errobj = Const_Cons(Unix_Errobj, Null);
    Global_GC_Link(Unix_Errobj);

    Define_Variable(&V_Call_Errhandler, "unix-call-standard-error-handler?",
        True);

    Def_Prim(P_Errorp,              "unix-error?",               1, 1, EVAL);
    Def_Prim(P_Errno,               "unix-errno",                0, 0, EVAL);
}
