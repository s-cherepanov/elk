#include "kernel.h"

#include <string.h>

extern int Get_Index (Object, Object);

Object General_Make_Vector (int len, Object fill, int konst) {
    Object vec;
    register Object *op;
    GC_Node;

    GC_Link (fill);
    vec = Alloc_Object ((len-1) * sizeof (Object) + sizeof (struct S_Vector),
	T_Vector, konst);
    VECTOR(vec)->tag = Null;
    VECTOR(vec)->size = len;
    for (op = VECTOR(vec)->data; len--; op++)
	*op = fill;
    GC_Unlink;
    return vec;
}

Object Make_Vector (int len, Object fill) {
    return General_Make_Vector (len, fill, 0);
}

Object Make_Const_Vector (int len, Object fill) {
    return General_Make_Vector (len, fill, 1);
}

Object P_Make_Vector (int argc, Object *argv) {
    register int len;

    if ((len = Get_Exact_Integer (argv[0])) < 0)
	Range_Error (argv[0]);
    return Make_Vector (len, argc == 1 ? Null : argv[1]);
}

Object P_Vector (int argc, Object *argv) {
    Object vec;
    register int i;

    vec = Make_Vector (argc, Null);
    for (i = 0; i < argc; i++)
	VECTOR(vec)->data[i] = *argv++;
    return vec;
}

Object P_Vectorp (Object x) {
    return TYPE(x) == T_Vector ? True : False;
}

Object P_Vector_Length (Object x) {
    Check_Type (x, T_Vector);
    return Make_Integer (VECTOR(x)->size);
}

Object P_Vector_Ref (Object vec, Object n) {
    Check_Type (vec, T_Vector);
    return VECTOR(vec)->data[Get_Index (n, vec)];
}

Object P_Vector_Set (Object vec, Object n, Object new) {
    Object old;
    register int i;

    Check_Type (vec, T_Vector);
    Check_Mutable (vec);
    old = VECTOR(vec)->data[i = Get_Index (n, vec)];
    VECTOR(vec)->data[i] = new;
    return old;
}

/* We cannot simply call P_List with vec->size and vec->data here,
 * because the latter can change during GC.
 */
Object P_Vector_To_List (Object vec) {
    register int i;
    Object list, tail, cell;
    GC_Node3;

    Check_Type (vec, T_Vector);
    list = tail = Null;
    GC_Link3 (vec, list, tail);
    for (i = 0; i < VECTOR(vec)->size; i++, tail = cell) {
	cell = Cons (VECTOR(vec)->data[i], Null);
	if (Nullp (list))
	    list = cell;
	else
	    (void)P_Set_Cdr (tail, cell);
    }
    GC_Unlink;
    return list;
}

Object List_To_Vector (Object list, int konst) {
    Object vec, len;
    register int i;
    GC_Node;

    GC_Link (list);
    len = P_Length (list);
    if (konst)
	vec = Make_Const_Vector (FIXNUM(len), Null);
    else
	vec = Make_Vector (FIXNUM(len), Null);
    for (i = 0; i < FIXNUM(len); i++, list = Cdr (list))
	VECTOR(vec)->data[i] = Car (list);
    GC_Unlink;
    return vec;
}

Object P_List_To_Vector (Object list) {
    return List_To_Vector (list, 0);
}

Object P_Vector_Fill (Object vec, Object fill) {
    register int i;

    Check_Type (vec, T_Vector);
    Check_Mutable (vec);
    for (i = 0; i < VECTOR(vec)->size; i++)
	VECTOR(vec)->data[i] = fill;
    return vec;
}

Object P_Vector_Copy (Object vec) {
    Object new;
    GC_Node;

    Check_Type (vec, T_Vector);
    GC_Link (vec);
    new = Make_Vector (VECTOR(vec)->size, Null);
    memcpy (POINTER(new), POINTER(vec),
	(VECTOR(vec)->size-1) * sizeof (Object) + sizeof (struct S_Vector));
    GC_Unlink;
    return new;
}
