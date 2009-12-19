/* context.c
 *
 * $Id$
 *
 * Copyright 1990, 1991, 1992, 1993, 1994, 1995, Oliver Laumann, Berlin
 * Copyright 2002, 2003 Sam Hocevar <sam@hocevar.net>, Paris
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

#include "xt.h"

static SYMDESCR XtIM_Syms[] = {
    { "x-event",         XtIMXEvent },
    { "timer",           XtIMTimer },
    { "alternate-input", XtIMAlternateInput },
    { 0, 0 }
};

static SYMDESCR XtInputMask_Syms[] = {
    { "read",            XtInputReadMask },
    { "write",           XtInputWriteMask },
    { "exception",       XtInputExceptMask },
    { 0, 0 }
};

static Object P_Destroy_Context();

Generic_Predicate (Context)

Generic_Equal (Context, CONTEXT, context)

Generic_Print (Context, "#[context %lu]", (unsigned int)(uintptr_t)POINTER(x))

static Object Internal_Make_Context (int finalize, XtAppContext context) {
    Object c;

    c = Find_Object (T_Context, (GENERIC)0, Match_Xt_Obj, context);
    if (Nullp (c)) {
        c = Alloc_Object (sizeof (struct S_Context), T_Context, 0);
        CONTEXT(c)->tag = Null;
        CONTEXT(c)->context = context;
        CONTEXT(c)->free = 0;
        Register_Object (c, (GENERIC)0,
            finalize ? P_Destroy_Context : (PFO)0, 1);
        XtAppSetWarningHandler (context, Xt_Warning);
        XtAppAddActionHook (context, (XtActionHookProc)Action_Hook,
            (XtPointer)0);
    }
    return c;
}

/* Backwards compatibility: */
Object Make_Context (XtAppContext context) {
    return Internal_Make_Context (1, context);
}

Object Make_Context_Foreign (XtAppContext context) {
    return Internal_Make_Context (0, context);
}

void Check_Context (Object c) {
    Check_Type (c, T_Context);
    if (CONTEXT(c)->free)
        Primitive_Error ("invalid context: ~s", c);
}

static Object P_Create_Context () {
    return Make_Context (XtCreateApplicationContext ());
}

static Object P_Destroy_Context (Object c) {
    Check_Context (c);
    Free_Actions (CONTEXT(c)->context);
    XtDestroyApplicationContext (CONTEXT(c)->context);
    CONTEXT(c)->free = 1;
    Deregister_Object (c);
    return Void;
}

static Object P_Initialize_Display (Object c, Object d, Object name,
                                    Object class) {
    register char *sn = 0, *sc = "", *sd = 0;
    Display *dpy;
    extern char **Argv;
    extern int First_Arg, Argc;
    int argc = Argc - First_Arg + 1;

    Argv[First_Arg-1] = "elk";
    Check_Context (c);
    if (!EQ(name, False))
        sn = Get_Strsym (name);
    if (!EQ(class, False))
        sc = Get_Strsym (class);
    if (TYPE(d) == T_Display) {
        XtDisplayInitialize (CONTEXT(c)->context, DISPLAY(d)->dpy,
            sn, sc, (XrmOptionDescRec *)0, 0, &argc, &Argv[First_Arg-1]);
        Argc = First_Arg + argc;
        return Void;
    }
    if (!EQ(d, False))
        sd = Get_Strsym (d);
    dpy = XtOpenDisplay (CONTEXT(c)->context, sd, sn, sc,
        (XrmOptionDescRec *)0, 0, &argc, &Argv[First_Arg-1]);
    Argc = First_Arg + argc - 1;
    if (dpy == 0)
    {
        if (sd)
            Primitive_Error ("cannot open display ~s", d);
        else
            Primitive_Error ("cannot open display");
    }
    return Make_Display (0, dpy);
}

/* Due to a bug in Xt this function drops core when invoked with a
 * display not owned by Xt.
 */
static Object P_Display_To_Context (Object d) {
    Check_Type (d, T_Display);
    return
        Make_Context_Foreign (XtDisplayToApplicationContext (DISPLAY(d)->dpy));
}

static Object P_Set_Context_Fallback_Resources (int argc, Object *argv) {
    register char **p = 0;
    register int i;
    struct S_String *sp;
    Object con;

    con = argv[0];
    Check_Context (con);
    if (argc > 1) {
        argv++; argc--;
        p = (char **)XtMalloc ((argc+1) * sizeof (char *));
        for (i = 0; i < argc; i++) {
            Check_Type (argv[i], T_String);
            sp = STRING(argv[i]);
            p[i] = XtMalloc (sp->size + 1);
            memcpy (p[i], sp->data, sp->size);
            p[i][sp->size] = 0;
        }
        p[i] = 0;
    }
    XtAppSetFallbackResources (CONTEXT(con)->context, p);
    return Void;
}

static Object P_Context_Main_Loop (Object c) {
    Check_Context (c);
    XtAppMainLoop (CONTEXT(c)->context);
    /*NOTREACHED*/
    return Void;
}

static Object P_Context_Pending (Object c) {
    Check_Context (c);
    return Bits_To_Symbols ((unsigned long)XtAppPending (CONTEXT(c)->context),
        1, XtIM_Syms);
}

static Object P_Context_Process_Event (int argc, Object *argv) {
    XtInputMask mask = XtIMAll;

    Check_Context (argv[0]);
    if (argc == 2)
        mask = (XtInputMask)Symbols_To_Bits (argv[1], 1, XtIM_Syms);
    XtAppProcessEvent (CONTEXT(argv[0])->context, mask);
    return Void;
}

static Boolean Work_Proc (XtPointer client_data) {
    Object ret;

    ret = Funcall (Get_Function ((int)client_data), Null, 0);
    if (Truep (ret))
        Deregister_Function ((int)client_data);
    return Truep (ret);
}

static Object P_Context_Add_Work_Proc (Object c, Object p) {
    XtWorkProcId id;
    register int i;

    Check_Context (c);
    Check_Procedure (p);
    i = Register_Function (p);
    id = XtAppAddWorkProc (CONTEXT(c)->context, Work_Proc, (XtPointer)i);
    return Make_Id ('w', (XtPointer)id, i);
}

static Object P_Remove_Work_Proc (Object id) {
    XtRemoveWorkProc ((XtWorkProcId)Use_Id (id, 'w'));
    Deregister_Function (IDENTIFIER(id)->num);
    return Void;
}

static void Timeout_Proc (XtPointer client_data, XtIntervalId *id) {
    Object proc, args;
    register int i = (int)client_data;

    args = Cons (Make_Id ('t', (XtPointer)*id, i), Null);
    proc = Get_Function (i);
    Deregister_Function (i);
    (void)Funcall (proc, args, 0);
}

static Object P_Context_Add_Timeout (Object c, Object n, Object p) {
    XtIntervalId id;
    register int i;

    Check_Context (c);
    Check_Procedure (p);
    i = Register_Function (p);
    id = XtAppAddTimeOut (CONTEXT(c)->context, (unsigned long)Get_Long (n),
        Timeout_Proc, (XtPointer)i);
    return Make_Id ('t', (XtPointer)id, i);
}

static Object P_Remove_Timeout (Object id) {
    XtRemoveTimeOut ((XtIntervalId)Use_Id (id, 't'));
    Deregister_Function (IDENTIFIER(id)->num);
    return Void;
}

/*ARGSUSED*/
static void Input_Proc (XtPointer client_data, int *src, XtInputId *id) {
    Object p, args;
    GC_Node2;

    p = Get_Function ((int)client_data);
    args = Null;
    GC_Link2 (p, args);
    args = Cons (Make_Id ('i', (XtPointer)*id, (int)client_data), Null);
    args = Cons (Car (p), args);
    GC_Unlink;
    (void)Funcall (Cdr (p), args, 0);
}

static Object P_Context_Add_Input (int argc, Object *argv) {
    Object c, src, p;
    XtInputId id;
    XtInputMask m;
    register int i;

    c = argv[0], src = argv[1], p = argv[2];
    Check_Context (c);
    Check_Procedure (p);
    Check_Type (src, T_Port);
    if (!(PORT(src)->flags & P_OPEN))
        Primitive_Error ("port has been closed: ~s", src);
    if (PORT(src)->flags & P_STRING)
        Primitive_Error ("invalid port: ~s", src);
    if (argc == 4) {
        m = Symbols_To_Bits (argv[3], 1, XtInputMask_Syms);
    } else {
        switch (PORT(src)->flags & (P_INPUT|P_BIDIR)) {
        case 0:       m = XtInputWriteMask;                 break;
        case P_INPUT: m = XtInputReadMask;                  break;
        default:      m = XtInputReadMask|XtInputWriteMask; break;
        }
    }
    i = Register_Function (Cons (src, p));
    id = XtAppAddInput (CONTEXT(c)->context, fileno (PORT(src)->file),
        (XtPointer)m, Input_Proc, (XtPointer)i);
    return Make_Id ('i', (XtPointer)id, i);
}

static Object P_Remove_Input (Object id) {
    XtRemoveInput ((XtInputId)Use_Id (id, 'i'));
    Deregister_Function (IDENTIFIER(id)->num);
    return Void;
}

void elk_init_xt_context () {
    Generic_Define (Context, "context", "context?");
    Define_Primitive (P_Create_Context,     "create-context",     0, 0, EVAL);
    Define_Primitive (P_Destroy_Context,    "destroy-context",    1, 1, EVAL);
    Define_Primitive (P_Initialize_Display, "initialize-display", 4, 4, EVAL);
    Define_Primitive (P_Display_To_Context, "display->context",   1, 1, EVAL);
    Define_Primitive (P_Set_Context_Fallback_Resources,
                        "set-context-fallback-resources!",   1, MANY, VARARGS);
    Define_Primitive (P_Context_Main_Loop,  "context-main-loop",  1, 1, EVAL);
    Define_Primitive (P_Context_Pending,    "context-pending",    1, 1, EVAL);
    Define_Primitive (P_Context_Process_Event,
                        "context-process-event",                1, 2, VARARGS);
    Define_Primitive (P_Context_Add_Work_Proc,
                        "context-add-work-proc",                  2, 2, EVAL);
    Define_Primitive (P_Remove_Work_Proc,   "remove-work-proc",   1, 1, EVAL);
    Define_Primitive (P_Context_Add_Timeout,"context-add-timeout",3, 3, EVAL);
    Define_Primitive (P_Remove_Timeout,     "remove-timeout",     1, 1, EVAL);
    Define_Primitive (P_Context_Add_Input,  "context-add-input",3, 4, VARARGS);
    Define_Primitive (P_Remove_Input,       "remove-input",       1, 1, EVAL);
}
