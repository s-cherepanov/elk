#include "unix.h"

Object Integer_Pair(a, b) int a, b; {
    Object x, y;
    GC_Node2;

    x = y = Null;
    GC_Link2(x, y);
    x = Make_Integer(a);
    y = Make_Integer(b);
    x = Cons(x, y);
    GC_Unlink;
    return x;
}

Object Syms_To_List(p) SYMDESCR *p; {
    Object ret, mode;
    GC_Node;

    ret = Null;
    GC_Link(ret);
    for ( ; p->name; p++) {
	mode = Intern(p->name);
	ret = Cons(mode, ret);
    }
    GC_Unlink;
    return P_Reverse(ret);
}

void Check_Result_Vector(x, len) Object x; {
    Check_Type(x, T_Vector);
    if (VECTOR(x)->size != len)
	Primitive_Error("argument vector has the wrong length");
}

elk_init_unix_unix() {
    P_Provide(Intern("unix.o"));
}
