#include <ctype.h>
#include <varargs.h>

#include "kernel.h"

Object Arg_True;

static Object V_Error_Handler, V_Top_Level_Control_Point;

/* Error_Tag should be static and users should only use the functions
 * Set_Error_Tag() and Get_Error_Tag().  However, in older versions
 * the variable was manipulated directly, therefore it will remain global
 * for some time for backwards compatibility.
 */
const char *Error_Tag;

char *appname;

Init_Error () {
    Arg_True = Cons (True, Null);
    Global_GC_Link (Arg_True);
    Define_Variable (&V_Error_Handler, "error-handler", Null);
    Define_Variable (&V_Top_Level_Control_Point, "top-level-control-point",
	Null);
}

const char *Get_Error_Tag () {
    return Error_Tag;
}

void Set_Error_Tag (tag) const char *tag; {
    Error_Tag = tag;
}

void Set_App_Name (name) char *name; {
    appname = name;
}

#ifdef lint
/*VARARGS1*/
Fatal_Error (foo) char *foo; { foo = foo; }
#else
Fatal_Error (va_alist) va_dcl {
    va_list args;
    char *fmt;
    
    Disable_Interrupts;
    va_start (args);
    fmt = va_arg (args, char *);
    (void)fflush (stdout);
    if (appname)
	fprintf (stderr, "\n%s: fatal error: ", appname);
    else
	fprintf (stderr, "\nFatal error: ");
    vfprintf (stderr, fmt, args);
    fprintf (stderr, ".\n");
    va_end (args);
    exit (1);
}
#endif

Panic (msg) const char *msg; {
    Disable_Interrupts;
    (void)fflush (stdout);
    if (appname)
	fprintf (stderr, "\n%s: panic: ", appname);
    else
	fprintf (stderr, "\nPanic: ");
    fprintf (stderr, "%s (dumping core).\n", msg);
    abort ();
}

Uncatchable_Error (errmsg) char *errmsg; {
    Disable_Interrupts;
    Reset_IO (0);
    /*
     * The message can be sent to stdout, as Reset_IO() resets the
     * current output port back to the Standard_Output_Port:
     */
    if (appname) {
	printf ("%s: %c", appname, tolower (errmsg[0]));
	errmsg++;
    }
    printf("%s\n", errmsg);
    Reset ();
}

#ifdef lint
/*VARARGS1*/
Primitive_Error (foo) char *foo; { foo = foo; }
#else
Primitive_Error (va_alist) va_dcl {
    va_list args;
    register char *p, *fmt;
    register i, n;
    Object msg, sym, argv[10];
    GC_Node; GCNODE gcv;

    va_start (args);
    fmt = va_arg (args, char *);
    for (n = 0, p = fmt; *p; p++)
	if (*p == '~' && p[1] != '~' && p[1] != '%'
		&& p[1] != 'E' && p[1] != 'e')
	    n++;
    if (n > 10)
	Panic ("Primitive_Error args");
    for (i = 0; i < n; i++)
	argv[i] = va_arg (args, Object);
    sym = Null;
    GC_Link (sym);
    gcv.gclen = 1 + i; gcv.gcobj = argv; gcv.next = &gc1; GC_List = &gcv;
    sym = Intern (Error_Tag);
    msg = Make_String (fmt, p - fmt);
    Err_Handler (sym, msg, i, argv);
    /*NOTREACHED*/
}
#endif

Object P_Error (argc, argv) Object *argv; {
    Check_Type (argv[1], T_String);
    Err_Handler (argv[0], argv[1], argc-2, argv+2);
    /*NOTREACHED*/
}

Err_Handler (sym, fmt, argc, argv) Object sym, fmt, *argv; {
    Object fun, args, a[1];
    GC_Node3;

    Reset_IO (0);
    args = Null;
    GC_Link3 (args, sym, fmt);
    args = P_List (argc, argv);
    args = Cons (fmt, args);
    args = Cons (sym, args);
    fun = Var_Get (V_Error_Handler);
    if (TYPE(fun) == T_Compound)
	(void)Funcall (fun, args, 0);
    a[0] = sym;
    Format (Curr_Output_Port, "~s: ", 4, 1, a);
    Format (Curr_Output_Port, STRING(fmt)->data, STRING(fmt)->size,
	argc, argv);
    (void)P_Newline (0, (Object *)0);
    GC_Unlink;
    Reset ();
    /*NOTREACHED*/
}

Reset () {
    Object cp;

    cp = Var_Get (V_Top_Level_Control_Point);
    if (TYPE(cp) == T_Control_Point)
	(void)Funcall_Control_Point (cp, Arg_True, 0);
    (void)fflush (stdout);
    exit (1);
}

Object P_Reset () {
    Reset_IO (0);
    Reset ();
    /*NOTREACHED*/
}

Range_Error (i) Object i; {
    Primitive_Error ("argument out of range: ~s", i);
}
