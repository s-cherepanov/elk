/* system.c
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

#include <string.h>

#if defined(HAVE_UNAME) && !defined(HAVE_GETHOSTNAME)
#  include <sys/utsname.h>
#endif

#define L_LINK_MAX     0
#define L_NAME_MAX     1
#define L_PATH_MAX     2
#define L_PIPE_BUF     3
#define L_NO_TRUNC     4

static SYMDESCR Limit_Syms[] = {
    { "max-links",     L_LINK_MAX },
    { "max-name",      L_NAME_MAX },
    { "max-path",      L_PATH_MAX },
    { "pipe-buf",      L_PIPE_BUF },
    { "no-trunc",      L_NO_TRUNC },
    { 0, 0 }
};

static Object P_File_Limit(Object lim, Object f) {
    int op = 0, fd = -1;
    long ret;
    char *fn = 0;

    switch (Symbols_To_Bits(lim, 0, Limit_Syms)) {
    case L_LINK_MAX:
#ifdef LINK_MAX
        return Make_Integer(LINK_MAX);
#else
#ifdef _PC_LINK_MAX
        op = _PC_LINK_MAX;
#       define HAVEOP
#else
        return Make_Integer(sizeof(short) * 8);   /* guess */
#endif
#endif
        break;

    case L_NAME_MAX:
#ifdef NAME_MAX
        return Make_Integer(NAME_MAX);
#else
#ifdef _PC_NAME_MAX
        op = _PC_NAME_MAX;
#       define HAVEOP
#else
        return Make_Integer(255);   /* guess */
#endif
#endif
        break;

    case L_PATH_MAX:
#ifdef MAX_PATH
        return Make_Integer(MAX_PATH);
#else
#ifdef _PC_PATH_MAX
        op = _PC_PATH_MAX;
#       define HAVEOP
#else
#ifdef MAXPATHLEN
        return Make_Integer(MAXPATHLEN);
#else
        return Make_Integer(1024);   /* guess */
#endif
#endif
#endif
        break;

    case L_PIPE_BUF:
#ifdef PIPE_BUF
        return Make_Integer(PIPE_BUF);
#else
#ifdef _PC_PIPE_BUF
        op = _PC_PIPE_BUF;
#       define HAVEOP
#else
        return Make_Integer(512);   /* guess */
#endif
#endif
        break;

    case L_NO_TRUNC:
#ifdef _PC_NO_TRUNC
        op = _PC_NO_TRUNC;
#       define HAVEOP
#else
        return False;   /* guess */
#endif
        break;
    }
#ifdef HAVEOP
    /* If we get here, we have a _PC_XXX symbol in `op' and can invoke
     * either pathconf() or fpathconf().
     */
    Get_Filename_Or_Filedescr(f, fd, fn);
    errno = 0;
    ret = fn ? pathconf(fn, op) : fpathconf(fd, op);
    if (ret == -1) {
#ifdef _PC_NO_TRUNC
        if (op == _PC_NO_TRUNC && errno == 0)
            return False;
#endif
        Raise_System_Error1("~s: ~E", f);
    }
#ifdef _PC_NO_TRUNC
    if (op == _PC_NO_TRUNC)
        return ret ? True : False;
    else
#endif
        return Make_Long(ret);
#endif
}

static Object P_List_File_Limits() {
    return Syms_To_List(Limit_Syms);
}

static Object P_Job_Controlp() {
#ifdef _POSIX_JOB_CONTROL
    return True;
#else
#ifdef _SC_JOB_CONTROL
    return sysconf(_SC_JOB_CONTROL) == 1 ? True : False;
#else
#ifdef SIGTSTP
    return True;
#else
    return False;
#endif
#endif
#endif
}

static Object P_System_Info(Object ret) {
#ifdef HAVE_GETHOSTNAME
    char hostname[MAXHOSTNAMELEN];
    char *p = hostname;
#else
#ifdef HAVE_UNAME
    struct utsname uts;
    char *p = uts.nodename;
#else
    char *p = "unknown-hostname";
#endif
#endif
    char systype[64], *q;
    Object x;
    GC_Node;

    Check_Result_Vector(ret, 3);
#ifdef HAVE_GETHOSTNAME
    (void)gethostname(hostname, sizeof(hostname));
#else
#ifdef HAVE_UNAME
    (void)uname(&uts);
#endif
#endif
    GC_Link(ret);
    x = Make_String(p, strlen(p)); VECTOR(ret)->data[0] = x;
    strcpy(systype, SYSTEMTYPE);
    if ((p = strchr(systype, '-')) && (q = strchr(p+1, '-'))) {
        *p++ = 0; *q = 0;
    } else p = "?";
    x = Make_String(systype, strlen(systype)); VECTOR(ret)->data[1] = x;
    x = Make_String(p, strlen(p)); VECTOR(ret)->data[2] = x;
    return Void;
}

void elk_init_unix_system() {
    Def_Prim(P_File_Limit,       "unix-file-limit",               2, 2, EVAL);
    Def_Prim(P_List_File_Limits, "unix-list-file-limits",         0, 0, EVAL);
    Def_Prim(P_Job_Controlp,     "unix-job-control?",             0, 0, EVAL);
    Def_Prim(P_System_Info,      "unix-system-info-vector-fill!", 1, 1, EVAL);
}
