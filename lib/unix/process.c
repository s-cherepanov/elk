/* process.c
 *
 * $Id$
 *
 * Copyright 1990, 1991, 1992, 1993, 1994, 1995, Oliver Laumann, Berlin
 * Copyright 2002, 2003 Sam Hocevar <sam@hocevar.net>, Paris
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
#include <stdio.h>

#ifdef HAVE_SYS_TIMES_H
#   include <sys/times.h>
#endif

#ifndef WIN32

#if defined(__APPLE__)
#   include <crt_externs.h>
#   define environ (* _NSGetEnviron())
#elif defined(__ENVIRON_IN_UNISTD_H)
#   define environ __environ
#elif defined(ENVIRON_IN_UNISTD_H)
/* Nothing to do */
#else
/* "extern" in front of the next declaration causes the Ultrix 4.2 linker
 * to fail, but omitting it no longer works with modern C compilers: */
extern char **environ;
#endif

#if defined(HAVE_ENVIRON)
static Object P_Environ() {
    Object ret, cell, str;
    char *p, **ep;
    static char c[2];
    GC_Node2;

    cell = ret = Null;
    GC_Link2(ret, cell);
    for (ep = environ; *ep; ep++) {
        cell = Cons(Null, Null);
        p = strchr(*ep, '=');
        if (p)
            *p++ = 0;
        else p = c+1;
        str = Make_String(p, strlen(p));
        Cdr(cell) = str;
        str = Make_String(*ep, strlen(*ep));
        Car(cell) = str;
        ret = Cons(cell, ret);
        *--p = '=';
    }
    GC_Unlink;
    return P_Reverse(ret);
}
#endif

static Object General_Exec(int argc, Object *argv, int path) {
    Object fn, args,  p, e;
    char *fnp, **argp, **envp;
    int i, len;
    Alloca_Begin;

    fn = argv[0], args = argv[1];
    fnp = Get_Strsym(fn);
    Check_List(args);
    len = Fast_Length(args);
    Alloca(argp, char**, (len+1) * sizeof(char*));
    for (i = 0, p = args; i < len; i++, p = Cdr(p)) {
        e = Car(p);
        Get_String_Stack(e, argp[i]);
    }
    argp[i] = 0;
    if (argc == 3) {
        args = argv[2];
        Check_List(args);
        len = Fast_Length(args);
        Alloca(envp, char**, (len+1) * sizeof(char*));
        for (i = 0, p = args; i < len; i++, p = Cdr(p)) {
            struct S_String *s1, *s2;

            e = Car(p);
            Check_Type(e, T_Pair);
            Check_Type(Car(e), T_String);
            Check_Type(Cdr(e), T_String);
            s1 = STRING(Car(e));
            s2 = STRING(Cdr(e));
            Alloca(envp[i], char*, s1->size + 1 + s2->size + 1);
            sprintf(envp[i], "%.*s=%.*s", s1->size, s1->data,
                s2->size, s2->data);
        }
        envp[i] = 0;
        Exit_Handler();
#if 0
        if (path)
            (void)execvpe(fnp, argp, envp);   /* ... doesn't exist */
        else
#endif
            (void)execve(fnp, argp, envp);
    } else {
        Exit_Handler();
        if (path)
            (void)execvp(fnp, argp);
        else
            (void)execv(fnp, argp);
    }
    Alloca_End;
    Raise_System_Error1("~s: ~E", fn);
}

static Object P_Exec(int argc, Object *argv) {
    return General_Exec(argc, argv, 0);
}

static Object P_Exec_Path(int argc, Object *argv) {
    if (argc == 3)   /* There is no execvpe (yet?). */
        Primitive_Error("environment argument not supported");
    return General_Exec(argc, argv, 1);
}

static Object P_Fork() {
    int pid;

    switch (pid = fork()) {
    case -1:
        Raise_System_Error("~E");
    case 0:
        Call_Onfork();
    }
    return Make_Integer(pid);
}

static Object P_Getenv(Object e) {
    extern char *getenv();
    char *s;

    return (s = getenv(Get_String(e))) ? Make_String(s, strlen(s)) : False;
}

static Object P_Getlogin() {
    extern char *getlogin();
    char *s;

    Disable_Interrupts;
    s = getlogin();
    Enable_Interrupts;
    if (s == 0)
        Raise_Error("cannot get login name");
    return Make_String(s, strlen(s));
}

static Object P_Getgids() { return Integer_Pair(getgid(), getegid()); }

static Object P_Getpids() { return Integer_Pair(getpid(), getppid()); }

static Object P_Getuids() { return Integer_Pair(getuid(), geteuid()); }

static Object P_Getgroups() {
    int i, n;
    GETGROUPS_TYPE *p;
    Object ret, next;
    Alloca_Begin;
    GC_Node2;

    /* gcc may complain under SunOS 4.x, because the prototype says
     * that the type of the 2nd argument is unsigned short*, not int.
     * But int is right.
     */
    if ((n = getgroups(0, (GETGROUPS_TYPE *)0)) == -1)
#ifdef NGROUPS
        n = NGROUPS;    /* 4.3BSD */
#else
        Raise_System_Error("~E");
#endif
    Alloca(p, GETGROUPS_TYPE*, n*sizeof(GETGROUPS_TYPE));
    (void)getgroups(n, p);
    next = ret = P_Make_List(Make_Integer(n), Null);
    GC_Link2(ret, next);
    for (i = 0; i < n; i++, next = Cdr(next)) {
        Object x;

        x = Make_Unsigned((unsigned)p[i]);
        Car(next) = x;
    }
    GC_Unlink;
    Alloca_End;
    return ret;
}

static Object P_Nice(Object incr) {
    int ret;

    errno = 0;
    if ((ret = nice(Get_Integer(incr))) == -1 && errno != 0)
        Raise_System_Error("~E");
    return Make_Integer(ret);
}

static Object Open_Pipe(Object cmd, int flags) {
    FILE *fp;
    Object ret;

    Disable_Interrupts;
    if ((fp = popen(Get_String(cmd), flags == P_INPUT ? "r" : "w")) == 0) {
        Enable_Interrupts;
        Raise_Error("cannot open pipe to process");
    }
    ret = Make_Port(flags, fp, Make_String("pipe-port", 9));
    PORT(ret)->closefun = pclose;
    Register_Object(ret, (GENERIC)0, Terminate_File, 0);
    Enable_Interrupts;
    return ret;
}

static Object P_Open_Input_Pipe(Object cmd) {
    return Open_Pipe(cmd, P_INPUT);
}

static Object P_Open_Output_Pipe(Object cmd) {
    return Open_Pipe(cmd, 0);
}

static Object P_Process_Resources(Object ret1, Object ret2) {
    static int hzval;
    struct tms tms;
    Object x;
    GC_Node2;

    if (hzval == 0) {
#ifdef HZ
        hzval = HZ;
#else
#ifdef CLK_TCK
        hzval = CLK_TCK;
#else
#ifdef _SC_CLK_TCK
        hzval = (int)sysconf(_SC_CLK_TCK);
#else
        hzval = 60;    /* Fallback for 4.3BSD.  I don't have a better idea. */
#endif
#endif
#endif
    }
    Check_Result_Vector(ret1, 2);
    Check_Result_Vector(ret2, 2);
    (void)times(&tms);
    GC_Link2(ret1, ret2);
    x = Make_Unsigned_Long((unsigned long)tms.tms_utime);
    VECTOR(ret1)->data[0] = x;
    x = Make_Unsigned_Long((unsigned long)tms.tms_stime);
    VECTOR(ret1)->data[1] = x;
    x = Make_Unsigned_Long((unsigned long)tms.tms_cutime);
    VECTOR(ret2)->data[0] = x;
    x = Make_Unsigned_Long((unsigned long)tms.tms_cstime);
    VECTOR(ret2)->data[1] = x;
    GC_Unlink;
    return Make_Integer(hzval);
}

static Object P_Sleep(Object s) {
    (void)sleep(Get_Unsigned(s));
    return Void;
}

static Object P_System(Object cmd) {
    int n, pid, status;
    char *s = Get_String(cmd);

#ifdef HAVE_VFORK
    switch (pid = vfork()) {
#else
    switch (pid = fork()) {
#endif
    case -1:
        Raise_System_Error("fork: ~E");
    case 0:
        for (n = Num_Filedescriptors(); n >= 3; n--)
            (void)close(n);
        execl("/bin/sh", "sh", "-c", s, (char *)0);
        _exit(127);
    default:
        Disable_Interrupts;
        while ((n = wait(&status)) != pid && n != -1)
                ;
        Enable_Interrupts;
    }
    /* Can this happen?
    if (n == -1)
        return False;
    */
    n = status & 0377;
    if (n)
        return Cons(Make_Integer(n), Null);
    return Make_Integer((status >> 8) & 0377);
}

static Object P_Umask(Object mask) {
    return Make_Integer(umask(Get_Integer(mask)));
}

static Object P_Working_Directory() {
    char *buf;
    int max = Path_Max()+2;   /* getcwd() needs two extra bytes */
    Object ret;
#if !defined(HAVE_GETCWD) && !defined(HAVE_GETWD)
    FILE *fp;
    char *p;
#endif
    Alloca_Begin;

    Alloca(buf, char*, max);
    Disable_Interrupts;
#ifdef HAVE_GETCWD
    if (getcwd(buf, max) == 0) {
        Saved_Errno = errno;
        Alloca_End;
        Enable_Interrupts;
        Raise_System_Error("~E");
    }
#else
#ifdef HAVE_GETWD
    if (getwd(buf) == 0) {
        Alloca_End;
        Enable_Interrupts;
        Raise_Error(buf);
    }
#else
    if ((fp = popen("pwd", "r")) == 0) {
err:
        Alloca_End;
        Enable_Interrupts;
        Raise_Error("cannot get output from pwd");
    }
    if (fgets(buf, max, fp) == 0)
        goto err;
    if (p = strchr(buf, '\n')) *p = '\0';
    (void)pclose(fp);
#endif
#endif
    Enable_Interrupts;
    ret = Make_String(buf, strlen(buf));
    Alloca_End;
    return ret;
}

void elk_init_unix_process() {
#if defined(HAVE_ENVIRON)
    Def_Prim(P_Environ,             "unix-environ",              0, 0, EVAL);
#endif
    Def_Prim(P_Exec,                "unix-exec",                 2, 3, VARARGS);
    Def_Prim(P_Exec_Path,           "unix-exec-path",            2, 3, VARARGS);
    Def_Prim(P_Fork,                "unix-fork",                 0, 0, EVAL);
    Def_Prim(P_Getenv,              "unix-getenv",               1, 1, EVAL);
    Def_Prim(P_Getlogin,            "unix-getlogin",             0, 0, EVAL);
    Def_Prim(P_Getgids,             "unix-getgids",              0, 0, EVAL);
    Def_Prim(P_Getpids,             "unix-getpids",              0, 0, EVAL);
    Def_Prim(P_Getuids,             "unix-getuids",              0, 0, EVAL);
    Def_Prim(P_Getgroups,           "unix-getgroups",            0, 0, EVAL);
    Def_Prim(P_Nice,                "unix-nice",                 1, 1, EVAL);
    Def_Prim(P_Open_Input_Pipe,     "unix-open-input-pipe",      1, 1, EVAL);
    Def_Prim(P_Open_Output_Pipe,    "unix-open-output-pipe",     1, 1, EVAL);
    Def_Prim(P_Process_Resources,   "unix-process-resources-vector-fill!",
                                                                 2, 2, EVAL);
    Def_Prim(P_Sleep,               "unix-sleep",                1, 1, EVAL);
    Def_Prim(P_System,              "unix-system",               1, 1, EVAL);
    Def_Prim(P_Umask,               "unix-umask",                1, 1, EVAL);
    Def_Prim(P_Working_Directory,   "unix-working-directory",    0, 0, EVAL);
}
#endif
