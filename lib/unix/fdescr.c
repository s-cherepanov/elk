/* fdescr.c
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

static SYMDESCR Open_Syms[] = {
    { "read",       1 },
    { "write",      2 },
    { "append",     O_APPEND },
    { "create",     O_CREAT },
    { "truncate",   O_TRUNC },
    { "exclusive",  O_EXCL },
#ifdef O_SYNC
    { "sync",       O_SYNC },
#endif
#ifdef O_NOCTTY
    { "noctty",     O_NOCTTY },
#endif
#ifdef O_NDELAY
    { "ndelay",     O_NDELAY },
#endif
#ifdef O_NONBLOCK
    { "nonblock",   O_NONBLOCK },
#endif
    { 0, 0 }
};

static SYMDESCR Fcntl_Flags[] = {
    { "append",     O_APPEND },
#ifdef O_SYNC
    { "sync",       O_SYNC },
#endif
#ifdef O_SYNCIO
    { "syncio",     O_SYNCIO },
#endif
    { "ndelay",     O_NDELAY },
#ifdef O_NONBLOCK
    { "nonblock",   O_NONBLOCK },
#endif
#ifdef O_LARGEFILE
    { "largefile",  O_LARGEFILE },
#endif
#ifdef FASYNC
    { "async",      FASYNC },
#endif
    { 0, 0 }
};

SYMDESCR Lseek_Syms[] = {
    { "set",        0 },         /* Should use symbolic constants, but   */
    { "current",    1 },         /* how do we know whether it's SEEK_    */
    { "end",        2 },         /* or L_ (BSD), and what include files  */
    { 0, 0 }                     /* are to be used?                      */
};

/* Dangerous: may be used to close the filedescriptor of a port.
 */
static Object P_Close(Object fd) {
    if (close(Get_Integer(fd)) == -1)
        Raise_System_Error("~E");
    return Void;
}

static Object P_Close_On_Exec(int argc, Object *argv) {
    int flags, fd;

    fd = Get_Integer(argv[0]);
    if ((flags = fcntl(fd, F_GETFD, 0)) == -1)
        Raise_System_Error("fcntl(F_GETFD): ~E");
    if (argc == 2) {
        Check_Type(argv[1], T_Boolean);
        if (fcntl(fd, F_SETFD, Truep(argv[1])) == -1)
            Raise_System_Error("fcntl(F_SETFD): ~E");
    }
    return flags & 1 ? True : False;
}

static Object P_Dup(int argc, Object *argv) {
    int fd = Get_Integer(argv[0]), ret;

    if ((ret = (argc == 1 ? dup(fd) : dup2(fd, Get_Integer(argv[1])))) == -1)
        Raise_System_Error("~E");
    return Make_Integer(ret);
}

static Object P_Filedescriptor_Flags(int argc, Object *argv) {
    int flags, fd;

    fd = Get_Integer(argv[0]);
    if ((flags = fcntl(fd, F_GETFL, 0)) == -1)
        Raise_System_Error("fcntl(F_GETFL): ~E");
    if (argc == 2) {
        if (fcntl(fd, F_SETFL, Symbols_To_Bits(argv[1], 1, Fcntl_Flags)) == -1)
            Raise_System_Error("fcntl(F_SETFL): ~E");
    }
    return Bits_To_Symbols((unsigned long)flags, 1, Fcntl_Flags);
}

static Object P_Fildescriptor_Port(Object fd, Object mode) {
    int n, flags;
    FILE *fp;
    Object ret;
    char *m, buf[32];

    m = Get_String(mode);
    switch (m[0]) {
    case 'r':
        flags = P_INPUT; break;
    case 'w': case 'a':
        flags = 0; break;
    default:
        Primitive_Error("invalid mode: ~s", mode);
    }
    if (m[1] == '+')
        flags = P_BIDIR;
    Disable_Interrupts;
    if ((fp = fdopen(n = Get_Integer(fd), m)) == 0) {
        Saved_Errno = errno;
        Enable_Interrupts;
        Raise_System_Error("~E");
    }
    sprintf(buf, "unix-fildescriptor[%d]", n);
    ret = Make_Port(flags, fp, Make_String(buf, strlen(buf)));
    Register_Object(ret, (GENERIC)0, Terminate_File, 0);
    Enable_Interrupts;
    return ret;
}

static Object P_Isatty(Object fd) {
    return isatty(Get_Integer(fd)) ? True : False;
}

static Object P_List_Filedescriptor_Flags() {
    return Syms_To_List(Fcntl_Flags);
}

static Object P_List_Open_Modes() {
    return Syms_To_List(Open_Syms);
}

/* Bad assumption: off_t fits into an unsigned int.
 */
static Object P_Lseek(Object fd, Object off, Object whence) {
    off_t ret;

    if ((ret = lseek(Get_Integer(fd), (off_t)Get_Long(off),
            (int)Symbols_To_Bits(whence, 0, Lseek_Syms))) == (off_t)-1)
        Raise_System_Error("~E");
    return Make_Unsigned_Long((unsigned long)ret);
}

int Num_Filedescriptors() {
    int ret;
#if defined(OPEN_MAX)
    ret = OPEN_MAX;
#elif defined(HAVE_GETDTABLESIZE)
    ret = getdtablesize();
#elif defined(SC_OPEN_MAX_IN_UNISTD_H)
    static r;
    if (r == 0) {
        if ((r = sysconf(_SC_OPEN_MAX)) == -1)
            r = 256;
    }
    ret = r;
#elif defined(NOFILE)
    ret = NOFILE;
#else
    ret = 256;
#endif
    return ret;
}

static Object P_Num_Filedescriptors() {
    return Make_Integer(Num_Filedescriptors());
}

static Object P_Open(int argc, Object *argv) {
    Object fn;
    int mode, n;

    fn = argv[0];
    mode = (int)Symbols_To_Bits(argv[1], 1, Open_Syms);
    if (!(n = mode & 3))
        Primitive_Error("mode must include 'read or 'write");
    mode &= ~3; mode |= n-1;
    if (mode & O_CREAT && argc == 2)
        Primitive_Error("third argument required for 'create");
    if ((n = open(Get_Strsym(fn), mode, argc == 3 ? Get_Integer(argv[2]) : 0))
            == -1)
        Raise_System_Error1("~s: ~E", fn);
    return Make_Integer(n);
}

static Object P_Pipe() {
    int fd[2];

    if (pipe(fd) == -1)
        Raise_System_Error("~E");
    return Integer_Pair(fd[0], fd[1]);
}

static Object P_Port_Filedescriptor(Object port) {
    Check_Type(port, T_Port);
    if ((PORT(port)->flags & (P_STRING|P_OPEN)) != P_OPEN)
        Primitive_Error("~s: invalid port", port);
    return Make_Integer(fileno(PORT(port)->file));
}

static Object Read_Write(int argc, Object *argv, int readflg) {
    struct S_String *sp;
    int len, fd;

    fd = Get_Integer(argv[0]);
    Check_Type(argv[1], T_String);
    sp = STRING(argv[1]);
    if (argc == 3) {
        len = Get_Integer(argv[2]);
        if (len < 0 || (unsigned int)len > sp->size)
            Range_Error(argv[2]);
    } else len = sp->size;
    if (readflg)
        len = read(fd, sp->data, len);
    else
        len = write(fd, sp->data, len);
    if (len == -1)
        Raise_System_Error("~E");
    return Make_Integer(len);
}

/* Avoid name clash with P_Read/P_Write of interpreter kernel
 */
static Object P_Readx(int argc, Object *argv) {
    return Read_Write(argc, argv, 1);
}

static Object P_Writex(int argc, Object *argv) {
    return Read_Write(argc, argv, 0);
}

static Object P_Ttyname(Object fd) {
    char *ret;
    extern char *ttyname();

    Disable_Interrupts;
    ret = ttyname(Get_Integer(fd));
    Enable_Interrupts;
    return ret ? Make_String(ret, strlen(ret)) : False;
}

void elk_init_unix_fdescr() {
    Def_Prim(P_Close,               "unix-close",               1, 1, EVAL);
    Def_Prim(P_Close_On_Exec,       "unix-close-on-exec",       1, 2, VARARGS);
    Def_Prim(P_Dup,                 "unix-dup",                 1, 2, VARARGS);
    Def_Prim(P_Filedescriptor_Flags,"unix-filedescriptor-flags",1, 2, VARARGS);
    Def_Prim(P_Fildescriptor_Port,  "unix-filedescriptor->port",2, 2, EVAL);
    Def_Prim(P_Isatty,              "unix-isatty?",             1, 1, EVAL);
    Def_Prim(P_List_Filedescriptor_Flags,
                              "unix-list-filedescriptor-flags", 0, 0, EVAL);
    Def_Prim(P_List_Open_Modes,     "unix-list-open-modes",     0, 0, EVAL);
    Def_Prim(P_Lseek,               "unix-lseek",               3, 3, EVAL);
    Def_Prim(P_Num_Filedescriptors, "unix-num-filedescriptors", 0, 0, EVAL);
    Def_Prim(P_Open,                "unix-open",                2, 3, VARARGS);
    Def_Prim(P_Pipe,                "unix-pipe",                0, 0, EVAL);
    Def_Prim(P_Port_Filedescriptor, "unix-port-filedescriptor", 1, 1, EVAL);
    Def_Prim(P_Readx,               "unix-read-string-fill!",   2, 3, VARARGS);
    Def_Prim(P_Ttyname,             "unix-ttyname",             1, 1, EVAL);
    Def_Prim(P_Writex,              "unix-write",               2, 3, VARARGS);
}
