/* provide, require, and related primitives.
 */

#include "kernel.h"

static Object Features;

Init_Features () {
    Features = Null;
    Global_GC_Link (Features);
#ifdef CAN_DUMP
    P_Provide (Intern ("elk:dump"));
#endif
#ifdef CAN_LOAD_OBJ
    P_Provide (Intern ("elk:load-object"));
#endif
}

Object P_Features () {
    return Features;
}

Object P_Featurep (sym) Object sym; {
    Object member;

    Check_Type (sym, T_Symbol);
    member = P_Memq (sym, Features);
    return Truep (member) ? True : False;
}

Object P_Provide (sym) Object sym; {
    Object member;

    Check_Type (sym, T_Symbol);
    member = P_Memq (sym, Features);
    if (!Truep (member))
	Features = Cons (sym, Features);
    return Void;
}

static Object Feature_Filename (str) Object str; {
    struct S_String *sp = STRING(str);
    int len = sp->size;
    char *p;
    Object s;
    GC_Node;

    for (p = sp->data+len-1; p >= sp->data && *p != '.'; p--)
	;
    if (p >= sp->data)
       return str;
    GC_Link (str);
    s = Make_String ((char *)0, len+4);
    bcopy (STRING(str)->data, STRING(s)->data, len);
    bcopy (".scm", STRING(s)->data+len, 4);
    GC_Unlink;
    return s;
}

Object P_Require (argc, argv) Object *argv; {
    Object sym, a[1], isfeature;
    GC_Node;

    sym = argv[0];
    GC_Link (sym);
    isfeature = P_Featurep (sym);
    if (!Truep (isfeature)) {
	if (argc == 3)
	    Check_Type (argv[2], T_Environment);
	a[0] = argc == 1 ? Feature_Filename (SYMBOL(sym)->name) : argv[1];
	if (Var_Is_True (V_Autoload_Notifyp))
	    Format (Standard_Output_Port, "[Autoloading ~a]~%", 18, 1, a);
	(void)General_Load (a[0], argc == 3 ? argv[2] : The_Environment);
	isfeature = P_Featurep (sym);
	if (!Truep (isfeature))
	    Primitive_Error ("feature ~s was not provided", sym);
    }
    GC_Unlink;
    return Void;
}
