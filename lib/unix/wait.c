/* wait.c
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

#ifdef __MACH__
#  define _POSIX_SOURCE
#endif

#include "unix.h"

#include <string.h>

#if defined(HAVE_WAITPID) || defined(HAVE_WAIT4)
#  define WAIT_PROCESS
#endif

#if defined(HAVE_WAITPID) || defined(HAVE_WAIT3) || defined(HAVE_WAIT4)
#  define WAIT_OPTIONS
#endif

#if defined(HAVE_WAIT3) || defined(HAVE_WAIT4)
#  define WAIT_RUSAGE
#  include <sys/time.h>
#  include <sys/resource.h>
#endif

#ifdef WAIT_OPTIONS
static SYMDESCR Wait_Flags[] = {
    { "nohang",    WNOHANG },
    { "untraced",  WUNTRACED },
    { 0, 0 }
};
#endif

#ifndef WEXITSTATUS
#  define WEXITSTATUS(stat)       ((int)((stat >> 8) & 0xFF))
#endif
#ifndef WTERMSIG
#  define WTERMSIG(stat)          ((int)(stat & 0x7F))
#endif
#ifndef WSTOPSIG
#  define WSTOPSIG(stat)          ((int)((stat >> 8) & 0xFF))
#endif
#ifndef WIFSIGNALED
#  define WIFSIGNALED(stat)       ((int)(stat & 0x7F))
#endif
#ifndef WIFSTOPPED
#  define WIFSTOPPED(stat)        ((int)(stat & 0x7F) == 0x7F)
#endif


static Object General_Wait(Object ret, Object ruret,
                           int haspid, int pid, int options) {
#ifndef WIN32
    int retpid, st, code;
    char *status;
#ifdef WAIT_RUSAGE
    struct rusage ru;
    Object sec;
#endif
    Object x;
    GC_Node3;

    x = Null;
    Check_Result_Vector(ret, 5);
    Check_Result_Vector(ruret, 2);
    if (haspid) {
#ifdef HAVE_WAIT4
        retpid = wait4(pid, &st, options, &ru);
#else
#ifdef HAVE_WAITPID
        retpid = waitpid(pid, &st, options);
#endif
#endif
    } else {
#ifdef HAVE_WAIT3
        retpid = wait3(&st, options, &ru);
#else
        retpid = wait(&st);
#endif
    }
    if (retpid == -1 && errno != ECHILD)
        Raise_System_Error("~E");
    GC_Link3(ret, ruret, x);
    x = Make_Integer(retpid); VECTOR(ret)->data[0] = x;
    if (retpid == 0 || retpid == -1) {
         status = "none";
         st = code = 0;
#ifdef WAIT_RUSAGE
         memset((char *)&ru, 0, sizeof(ru));
#endif
    } else if (WIFSTOPPED(st)) {
        status = "stopped";  code = WSTOPSIG(st);
    } else if (WIFSIGNALED(st)) {
        status = "signaled"; code = WTERMSIG(st);
    } else {
        status = "exited";   code = WEXITSTATUS(st);
    }
    x = Intern(status);     VECTOR(ret)->data[1] = x;
    x = Make_Integer(code); VECTOR(ret)->data[2] = x;
    VECTOR(ret)->data[3] = st & 0200 ? True : False;
#ifdef WAIT_RUSAGE
    x = Cons(Null, Make_Unsigned_Long((unsigned long)ru.ru_utime.tv_usec
        * 1000));
    sec = Make_Unsigned_Long((unsigned long)ru.ru_utime.tv_sec);
    Car(x) = sec;
    VECTOR(ruret)->data[0] = x;
    x = Cons(Null, Make_Unsigned_Long((unsigned long)ru.ru_stime.tv_usec
        * 1000));
    sec = Make_Unsigned_Long((unsigned long)ru.ru_stime.tv_sec);
    Car(x) = sec;
    VECTOR(ruret)->data[1] = x;
#endif
    GC_Unlink;
#endif
    return Void;
}

static Object P_Wait(int argc, Object *argv) {
    int flags = 0;

    if (argc == 3)
#ifdef WAIT_OPTIONS
        flags = (int)Symbols_To_Bits(argv[2], 1, Wait_Flags);
#else
        Primitive_Error("wait options not supported");
#endif
    return General_Wait(argv[0], argv[1], 0, 0, flags);
}

#ifdef WAIT_PROCESS
/* If WAIT_PROCESS is supported, then WAIT_OPTIONS is supported as well,
 * because both waitpid() and wait4() accept options.
 */
static Object P_Wait_Process(int argc, Object *argv) {
    return General_Wait(argv[0], argv[1], 1, Get_Integer(argv[2]),
        argc == 4 ? (int)Symbols_To_Bits(argv[3], 1, Wait_Flags) : 0);
}
#endif

void elk_init_unix_wait() {
    Def_Prim(P_Wait,         "unix-wait-vector-fill!",         2, 3, VARARGS);
#ifdef WAIT_PROCESS
    Def_Prim(P_Wait_Process, "unix-wait-process-vector-fill!", 3, 4, VARARGS);
    P_Provide(Intern("unix:wait-process"));
#endif
#ifdef WAIT_OPTIONS
    P_Provide(Intern("unix:wait-options"));
#endif
}
