#include "xt.h"

static max_functions = 512;
static Object Functions;

int Register_Function (x) Object x; {
    register i;
    Object v;
    GC_Node;

    for (i = 0; i < max_functions; i++)
	if (Nullp (VECTOR(Functions)->data[i])) break;
    if (i == max_functions) {
	max_functions *= 2;
	GC_Link (x);
	v = Make_Vector (max_functions, Null);
	GC_Unlink;
	bcopy ((char *)VECTOR(Functions)->data, (char *)VECTOR(v)->data,
	    i * sizeof (Object));
	Functions = v;
    }
    VECTOR(Functions)->data[i] = x;
    return i;
}

Object Get_Function (i) int i; {
    return VECTOR(Functions)->data[i];
}

void Deregister_Function (i) int i; {
    VECTOR(Functions)->data[i] = Null;
}

elk_init_xt_function () {
    Functions = Make_Vector (max_functions, Null);
    Global_GC_Link (Functions);
}
