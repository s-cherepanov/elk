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

static Object P_Internal_Lock_Operation(fd, lck, block, what, ret)
        Object fd, lck, block, what, ret; {
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

elk_init_unix_lock() {
#ifdef LOCKS
    Def_Prim(P_Internal_Lock_Operation, "unix-internal-lock-operation",
        5, 5, EVAL);
    P_Provide(Intern("unix:file-locking"));
#ifdef RECORD_LOCKS
    P_Provide(Intern("unix:record-locks"));
#endif
#endif
}
