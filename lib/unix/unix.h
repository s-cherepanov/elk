#include "scheme.h"

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
