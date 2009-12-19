/* lock.c
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

#include <sys/file.h>

#ifdef F_SETLK
#  define LOCKS
#  define RECORD_LOCKS
#else
#ifdef LOCK_SH
#  define LOCKS
#endif
#endif

#ifdef LOCKS

static Object P_Internal_Lock_Operation(Object fd, Object lck, Object block,
                                        Object what, Object ret) {
#ifdef RECORD_LOCKS
    struct flock fl;
#else
    int mode;
#endif
    int op;
    Object *vp;

    Check_Result_Vector(lck, 4);
    Check_Type(what, T_Character);
    op = CHAR(what);
    if (op == 'q')
        Check_Result_Vector(ret, 4);

#ifdef RECORD_LOCKS
    Check_Type(block, T_Boolean);
    vp = VECTOR(lck)->data;
    Check_Type(*vp, T_Boolean);
    fl.l_type = op == 'r' ? F_UNLCK : (Truep(*vp) ? F_WRLCK : F_RDLCK);
    fl.l_whence = (short)Symbols_To_Bits(*++vp, 0, Lseek_Syms);
    fl.l_start = Get_Long(*++vp);
    fl.l_len = Get_Long(*++vp);
    if (fcntl(Get_Integer(fd), op == 'q' ? F_GETLK :
            (Truep(block) ? F_SETLKW : F_SETLK), &fl) == -1) {
        if (op == 's' && !Truep(block) && (errno == EAGAIN || errno == EACCES))
            return False;
        Raise_System_Error("fcntl: ~E");
    }
    if (op == 'q') {
        Object x;
        GC_Node;

        if (fl.l_type == F_UNLCK)
            return False;
        GC_Link(ret);
        VECTOR(ret)->data[0] = fl.l_type == F_WRLCK ? True : False;
        x = Bits_To_Symbols((unsigned long)fl.l_whence, 0, Lseek_Syms);
        VECTOR(ret)->data[1] = x;
        x = Make_Long(fl.l_start); VECTOR(ret)->data[2] = x;
        x = Make_Long(fl.l_len);   VECTOR(ret)->data[3] = x;
        GC_Unlink;
        return Make_Integer(fl.l_pid);
    }
#else
    Check_Type(block, T_Boolean);
    if (op == 'q')
        return False;
    vp = VECTOR(lck)->data;
    Check_Type(*vp, T_Boolean);
    mode = op == 'r' ? LOCK_UN : (Truep(*vp) ? LOCK_EX : LOCK_SH);
    if (op != 'r' && !Truep(block))
        mode |= LOCK_NB;
    if (flock(Get_Integer(fd), mode) == -1) {
        if (op == 's' && !Truep(block) && errno == EWOULDBLOCK)
            return False;
        Raise_System_Error("flock: ~E");
    }
#endif
    return op == 's' ? True : Void;
}

#endif

void elk_init_unix_lock() {
#ifdef LOCKS
    Def_Prim(P_Internal_Lock_Operation, "unix-internal-lock-operation",
        5, 5, EVAL);
    P_Provide(Intern("unix:file-locking"));
#ifdef RECORD_LOCKS
    P_Provide(Intern("unix:record-locks"));
#endif
#endif
}
