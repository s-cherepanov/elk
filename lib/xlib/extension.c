#include "xlib.h"

static Object P_List_Extensions (d) Object d; {
    Object ret;
    int n;
    register i;
    register char **p;
    GC_Node;

    Check_Type (d, T_Display);
    Disable_Interrupts;
    p = XListExtensions (DISPLAY(d)->dpy, &n);
    Enable_Interrupts;
    ret = Make_Vector (n, Null);
    GC_Link (ret);
    for (i = 0; i < n; i++) {
	Object e;
	
	e = Make_String (p[i], strlen (p[i]));
	VECTOR(ret)->data[i] = e;
    }
    GC_Unlink;
    XFreeExtensionList (p);
    return ret;
}

static Object P_Query_Extension (d, name) Object d, name; {
    int opcode, event, error;
    Object ret, t;
    GC_Node2;

    Check_Type (d, T_Display);
    if (!XQueryExtension (DISPLAY(d)->dpy, Get_Strsym (name), &opcode,
	    &event, &error))
	return False;
    t = ret = P_Make_List (Make_Integer (3), Null);
    GC_Link2 (ret, t);
    Car (t) = (opcode ? Make_Integer (opcode) : False); t = Cdr (t);
    Car (t) = (event ? Make_Integer (event) : False); t = Cdr (t);
    Car (t) = (error ? Make_Integer (error) : False);
    GC_Unlink;
    return ret;
}

elk_init_xlib_extension () {
    Define_Primitive (P_List_Extensions,    "list-extensions",   1, 1, EVAL);
    Define_Primitive (P_Query_Extension,    "query-extension",   2, 2, EVAL);
}
