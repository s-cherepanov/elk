/* Delay and force.
 */

#include "kernel.h"

Object P_Promisep (Object x) {
    return TYPE(x) == T_Promise ? True : False;
}

Object P_Delay (Object argl) {
    Object d;
    GC_Node;

    GC_Link (argl);
    d = Alloc_Object (sizeof (struct S_Promise), T_Promise, 0);
    GC_Unlink;
    PROMISE(d)->done = 0;
    PROMISE(d)->env = The_Environment;
    PROMISE(d)->thunk = Car (argl);
    return d;
}

Object P_Force (Object d) {
    Object ret, a[2];
    GC_Node;
    TC_Prolog;

    Check_Type (d, T_Promise);
    if (PROMISE(d)->done)
	return PROMISE(d)->thunk;
    GC_Link (d);
    a[0] = PROMISE(d)->thunk; a[1] = PROMISE(d)->env;
    TC_Disable;
    ret = P_Eval (2, a);
    TC_Enable;
    GC_Unlink;
    if (PROMISE(d)->done)    /* take care of recursive force calls */
	return PROMISE(d)->thunk;
    PROMISE(d)->thunk = ret;
    PROMISE(d)->done = 1;
    return ret;
}

Object P_Promise_Environment (Object p) {
    Check_Type (p, T_Promise);
    return PROMISE(p)->env;
}
