/* unix.h
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

#include "config.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <time.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <errno.h>

#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif

#include "scheme.h"

extern int errno;
extern char *index();

extern Object Unix_Errobj, V_Call_Errhandler;
extern SYMDESCR Lseek_Syms[];

Object Integer_Pair P_((int, int));
Object Syms_To_List P_((SYMDESCR*));

#define Get_Filename_Or_Filedescr(obj,fd,fn) \
    switch (TYPE(obj)) {\
    case T_String: case T_Symbol:\
        (fn) = Get_Strsym(obj); break;\
    case T_Fixnum: case T_Bignum:\
        (fd) = Get_Integer(obj); break;\
    default:\
        Wrong_Type_Combination(obj, "symbol, string, or integer");\
    }

#define Raise_System_Error(msg) {\
    Saved_Errno = errno;\
    Raise_Error(msg);\
}

#define Raise_Error(msg) {\
    if (Var_Is_True(V_Call_Errhandler))\
        Primitive_Error(msg);\
    return Unix_Errobj;\
}

#define Raise_System_Error1(msg,a1) {\
    Saved_Errno = errno;\
    Raise_Error1(msg,a1);\
}

#define Raise_Error1(msg,a1) {\
    if (Var_Is_True(V_Call_Errhandler))\
        Primitive_Error(msg,a1);\
    return Unix_Errobj;\
}

#define Raise_System_Error2(msg,a1,a2) {\
    Saved_Errno = errno;\
    Raise_Error2(msg,a1,a2);\
}

#define Raise_Error2(msg,a1,a2) {\
    if (Var_Is_True(V_Call_Errhandler))\
        Primitive_Error(msg,a1,a2);\
    return Unix_Errobj;\
}

#define Def_Prim Define_Primitive
