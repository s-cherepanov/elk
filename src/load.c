#include "kernel.h"

Object V_Load_Path, V_Load_Noisilyp, V_Load_Libraries;

#ifdef CAN_LOAD_OBJ
#  define Default_Load_Libraries LOAD_LIBRARIES
#else
#  define Default_Load_Libraries ""
#endif

char *Loader_Input;  /* tmp file name used by load.xx.c */

extern void Switch_Environment (Object);
void Load_Source (Object);

#ifdef CAN_LOAD_OBJ
  void Fork_Load();
#endif

#ifdef USE_LD
#  include "load-ld.c"
#else
#ifdef USE_RLD
#  include "load-rld.c"
#else
#ifdef USE_SHL
#  include "load-shl.c"
#else
#ifdef USE_DLOPEN
#  include "load-dl.c"
#endif
#endif
#endif
#endif

void Init_Load () {
    Define_Variable (&V_Load_Path, "load-path",
	Cons (Make_String (".", 1),
	Cons (Make_String (SCM_DIR, sizeof (SCM_DIR) - 1),
	Cons (Make_String (OBJ_DIR, sizeof (OBJ_DIR) - 1), Null))));
    Define_Variable (&V_Load_Noisilyp, "load-noisily?", False);
    Define_Variable (&V_Load_Libraries, "load-libraries",
	Make_String (Default_Load_Libraries, sizeof Default_Load_Libraries-1));
#ifdef CAN_LOAD_OBJ
    Register_Onfork (Fork_Load);
#endif
}

void Init_Loadpath (char *s) {     /* No GC possible here */
    register char *p;
    Object path;

    path = Null;
    if (s[0] == '\0')
	return;
    while (1) {
	for (p = s; *p && *p != ':'; p++)
	    ;
	path = Cons (Make_String (s, p-s), path);
	if (*p == '\0')
	    break;
	s = ++p;
    }
    Var_Set (V_Load_Path, P_Reverse (path));
}

int Is_O_File (Object name) {
    register char *p;
    register struct S_String *str;

    if (TYPE(name) == T_Symbol)
	name = SYMBOL(name)->name;
    str = STRING(name);
    p = str->data + str->size;
    return str->size >= 2 && *--p == 'o' && *--p == '.';
}

void Check_Loadarg (Object x) {
    Object tail;
    register int t = TYPE(x);

    if (t == T_Symbol || t == T_String)
	return;
    if (t != T_Pair)
	Wrong_Type_Combination (x, "string, symbol, or list");
    for (tail = x; !Nullp (tail); tail = Cdr (tail)) {
	Object f;

	f = Car (tail);
	if (TYPE(f) != T_Symbol && TYPE(f) != T_String)
	    Wrong_Type_Combination (f, "string or symbol");
	if (!Is_O_File (f))
	    Primitive_Error ("~s: not an object file", f);
    }
}

Object General_Load (Object what, Object env) {
    Object oldenv;
    GC_Node;

    Check_Type (env, T_Environment);
    oldenv = The_Environment;
    GC_Link (oldenv);
    Switch_Environment (env);
    Check_Loadarg (what);
    if (TYPE(what) == T_Pair)
#ifdef CAN_LOAD_OBJ
	Load_Object (what)
#endif
	;
    else if (Is_O_File (what))
#ifdef CAN_LOAD_OBJ
	Load_Object (Cons (what, Null))
#endif
	;
    else
	Load_Source (what);
    Switch_Environment (oldenv);
    GC_Unlink;
    return Void;
}

Object P_Load (int argc, Object *argv) {
    return General_Load (argv[0], argc == 1 ? The_Environment : argv[1]);
}

void Load_Source_Port (Object port) {
    Object val;
    GC_Node;
    TC_Prolog;

    GC_Link (port);
    while (1) {
	val = General_Read (port, 1);
	if (TYPE(val) == T_End_Of_File)
	    break;
	TC_Disable;
	val = Eval (val);
	TC_Enable;
	if (Var_Is_True (V_Load_Noisilyp)) {
	    Print (val);
	    (void)P_Newline (0, (Object *)0);
	}
    }
    GC_Unlink;
}

void Load_Source (Object name) {
    Object port;
    GC_Node;

    port = General_Open_File (name, P_INPUT, Var_Get (V_Load_Path));
    GC_Link (port);
    Load_Source_Port (port);
    (void)P_Close_Input_Port (port);
    GC_Unlink;
}

/* Interface to P_Load() for use by applications.
 */
void Load_File (char *name) {
    Object arg;

    arg = Make_String(name, strlen(name));
    (void)P_Load(1, &arg);
}
