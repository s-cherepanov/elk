/* proc.c: Eval, funcall, apply, map, lambda, etc. The main-loop of the
 * Scheme evaluator.
 *
 * $Id$
 *
 * Copyright 1990, 1991, 1992, 1993, 1994, 1995, Oliver Laumann, Berlin
 * Copyright 2002, 2003 Sam Hocevar <sam@zoy.org>, Paris
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

#include "config.h"

#include "kernel.h"

#ifdef USE_ALLOCA
#  define MAX_ARGS_ON_STACK  4
#else
#  define MAX_ARGS_ON_STACK  8
#endif

#define Get_Arglist_Length(_cnt, _lst, _x) \
    for (_cnt = 0, _x = _lst; TYPE(_x) == T_Pair; _x = Cdr (_x), _cnt++) \
        ; \
    if (!Nullp(_x)) \
        Primitive_Error("improper argument list"); \

#define Funcall_Switch(t,func,args,eval) \
    if (t == T_Primitive) {\
        return Funcall_Primitive (func, args, eval);\
    } else if (t == T_Compound) {\
        return Funcall_Compound (func, args, eval);\
    } else if (t == T_Control_Point) {\
        Funcall_Control_Point (func, args, eval);\
    } else Primitive_Error ("application of non-procedure: ~s", func);\

extern void Switch_Environment (Object);
extern unsigned int Stack_Size ();
extern void Uncatchable_Error (char *);
#ifdef HAVE_ATTRIBUTE_NORETURN
extern void Funcall_Control_Point (Object, Object, int)
        __attribute__ ((__noreturn__));
#else
extern void Funcall_Control_Point (Object, Object, int);
#endif
extern void Pop_Frame ();
extern void Push_Frame (Object);

/* Tail_Call indicates whether we are executing the last form in a
 * sequence of forms.  If it is true and we are about to call a compound
 * procedure, we are allowed to check whether a tail-call can be
 * performed instead.
 */
int Tail_Call = 0;

Object Sym_Lambda,
       Sym_Macro;

static Object tc_fun, tc_argl, tc_env;

Object Macro_Expand(), Funcall_Primitive(), Funcall_Compound();

void Init_Proc () {
    Define_Symbol (&Sym_Lambda, "lambda");
    Define_Symbol (&Sym_Macro, "macro");
}

void Check_Procedure (Object x) {
    register int t = TYPE(x);

    if (t != T_Primitive && t != T_Compound)
        Wrong_Type_Combination (x, "procedure");
    if (t == T_Primitive && PRIM(x)->disc == NOEVAL)
        Primitive_Error ("invalid procedure: ~s", x);
}

Object P_Procedurep (Object x) {
    register int t = TYPE(x);
    return t == T_Primitive || t == T_Compound || t == T_Control_Point
         ? True : False;
}

Object P_Primitivep (Object x) {
    return TYPE(x) == T_Primitive ? True : False;
}

Object P_Compoundp (Object x) {
    return TYPE(x) == T_Compound ? True : False;
}

Object P_Macrop (Object x) {
    return TYPE(x) == T_Macro ? True : False;
}

Object Make_Compound () {
    Object proc;

    proc = Alloc_Object (sizeof (struct S_Compound), T_Compound, 0);
    COMPOUND(proc)->closure = COMPOUND(proc)->env = COMPOUND(proc)->name = Null;
    return proc;
}

Object Make_Primitive (Object (*fun)(), char const *name, int min, int max,
        enum discipline disc) {
    Object prim;
    register struct S_Primitive *pr;

    prim = Alloc_Object (sizeof (struct S_Primitive), T_Primitive, 0);
    pr = PRIM(prim);
    pr->tag = Null;
    pr->fun = fun;
    pr->name = name;
    pr->minargs = min;
    pr->maxargs = max;
    pr->disc = disc;
    return prim;
}

Object Eval (Object form) {
    register int t;
    register struct S_Symbol *sym;
    Object fun, binding, ret;
    static unsigned int tick;
    GC_Node;
    TC_Prolog;

again:
    t = TYPE(form);
    if (t == T_Symbol) {
        sym = SYMBOL(form);
        if (TYPE(sym->value) == T_Unbound) {
            binding = Lookup_Symbol (form, 1);
            sym->value = Cdr (binding);
        }
        ret = sym->value;
        if (TYPE(ret) == T_Autoload)
            ret = Do_Autoload (form, ret);
        return ret;
    }
    if (t != T_Pair) {
        if (t == T_Null)
            Primitive_Error ("no subexpression in procedure call");
        if (t == T_Vector)
            Primitive_Error ("unevaluable object: ~s", form);
        return form;
    }
    if ((tick++ & 7) == 0)
        if (Stack_Size () > Max_Stack)
            Uncatchable_Error ("Out of stack space");
    /*
     * Avoid recursive Eval() for the most common case:
     */
    fun = Car (form);
    if (TYPE(fun) != T_Symbol ||
            (fun = SYMBOL(fun)->value, TYPE(fun) == T_Unbound) ||
            TYPE(fun) == T_Autoload) {
        GC_Link (form);
        TC_Disable;
        fun = Eval (Car (form));
        TC_Enable;
        GC_Unlink;
    }
    form = Cdr (form);
    t = TYPE(fun);
    if (t == T_Macro) {
        form = Macro_Expand (fun, form);
        goto again;
    }
    Funcall_Switch (t, fun, form, 1);
    /*NOTREACHED*/
}

Object P_Eval (int argc, Object *argv) {
    Object ret, oldenv;
    GC_Node;

    if (argc == 1)
        return Eval (argv[0]);
    Check_Type (argv[1], T_Environment);
    oldenv = The_Environment;
    GC_Link (oldenv);
    Switch_Environment (argv[1]);
    ret = Eval (argv[0]);
    Switch_Environment (oldenv);
    GC_Unlink;
    return ret;
}

Object P_Apply (int argc, Object *argv) {
    Object ret, list, tail, cell, last;
    register int i;
    GC_Node3;

    Check_Procedure (argv[0]);
    /* Make a list of all args but the last, then append the
     * last arg (which must be a proper list) to this list.
     */
    list = tail = last = Null;
    GC_Link3 (list, tail, last);
    for (i = 1; i < argc-1; i++, tail = cell) {
        cell = Cons (argv[i], Null);
        if (Nullp (list))
            list = cell;
        else
            (void)P_Set_Cdr (tail, cell);
    }
    for (last = argv[argc-1]; !Nullp (last); last = Cdr (last), tail = cell) {
        cell = Cons (P_Car (last), Null);
        if (Nullp (list))
            list = cell;
        else
            (void)P_Set_Cdr (tail, cell);
    }
    ret = Funcall (argv[0], list, 0);
    GC_Unlink;
    return ret;
}

Object Funcall_Primitive (Object fun, Object argl, int eval) {
    register struct S_Primitive *prim;
    register int argc, i;
    char const *last_tag;
    register Object *argv = NULL;
    Object abuf[MAX_ARGS_ON_STACK], r, e;
    GC_Node4; GCNODE gcv;
    TC_Prolog;
    Alloca_Begin;

    prim = PRIM(fun);
    last_tag = Error_Tag;   /* avoid function calls here */
    Error_Tag = prim->name;
    Get_Arglist_Length (argc, argl, r);  /* r is temporary variable */
    if (argc < prim->minargs
            || (prim->maxargs != MANY && argc > prim->maxargs))
        Primitive_Error ("wrong number of arguments");

    e = The_Environment;
    GC_Link4_Tag_Primitive (argl, fun, e, r);

    if (prim->disc == NOEVAL) {
        r = (prim->fun)(argl);
    } else {
        TC_Disable;
        /*
         * Skip the loop if argc==0 or argc==1 (special case below).
         */
        if (prim->disc != EVAL || argc >= 2) {
            if (argc <= MAX_ARGS_ON_STACK)
                argv = abuf;
            else
                Alloca (argv, Object*, argc * sizeof (Object));
            gcv.gclen = 1; gcv.gcobj = argv; gcv.next = &gc4; GC_List = &gcv;
            for (r = argl, i = 0; i < argc; i++, r = Cdr (r)) {
                argv[i] = eval ? Eval (Car (r)) : Car (r);
                gcv.gclen++;
            }
            TC_Enable;
            prim = PRIM(fun);   /* fun has possibly been moved during gc */
        }
        if (prim->disc == VARARGS) {
            r = (prim->fun)(argc, argv);
        } else {
            switch (argc) {
            case 0:
                r = (prim->fun)(); break;
            case 1:
                TC_Disable;
                r = eval ? Eval (Car (argl)) : Car (argl);
                TC_Enable;
                r = (PRIM(fun)->fun)(r);
                break;
            case 2:
                r = (prim->fun)(argv[0], argv[1]); break;
            case 3:
                r = (prim->fun)(argv[0], argv[1], argv[2]); break;
            case 4:
                r = (prim->fun)(argv[0], argv[1], argv[2], argv[3]); break;
            case 5:
                r = (prim->fun)(argv[0], argv[1], argv[2], argv[3], argv[4]);
                break;
            case 6:
                r = (prim->fun)(argv[0], argv[1], argv[2], argv[3], argv[4],
                                argv[5]); break;
            case 7:
                r = (prim->fun)(argv[0], argv[1], argv[2], argv[3], argv[4],
                                argv[5], argv[6]); break;
            case 8:
                r = (prim->fun)(argv[0], argv[1], argv[2], argv[3], argv[4],
                                argv[5], argv[6], argv[7]); break;
            case 9:
                r = (prim->fun)(argv[0], argv[1], argv[2], argv[3], argv[4],
                                argv[5], argv[6], argv[7], argv[8]); break;
            case 10:
                r = (prim->fun)(argv[0], argv[1], argv[2], argv[3], argv[4],
                                argv[5], argv[6], argv[7], argv[8], argv[9]);
                break;
            default:
                Panic ("too many args for primitive");
            }
        }
        Alloca_End;
    }
    GC_Unlink;
    Error_Tag = last_tag;
    return r;
}

/* This macro is used by Funcall_Compound() below.  Note that
 * if we are in a tail recursion, we are reusing the old procedure
 * frame; we just assign new values to the formal parameters.
 * Add_Binding() has been inlined here for speed.  r is used as
 * a temporary variable.
 */
#define Lambda_Bind(var,val) {\
    r = Cons (var, val);\
    if (tail_calling)\
        newframe = Cons (r, newframe);\
    else\
        frame = Cons (r, frame);\
}

Object Funcall_Compound (Object fun, Object argl, int eval) {
    register int argc, min, max, i, tail_calling = 0;
    register Object *argv = NULL;
    Object abuf[MAX_ARGS_ON_STACK], rest, r, frame, tail,
        tail_call_env, oldenv, newframe;
    register GCNODE *p;
    GC_Node7; GCNODE gcv;
    Alloca_Begin;

    if (Tail_Call && eval) {
        for (p = GC_List; p && p->gclen != TAG_FUN; p = p->next) {
            if (p->gclen == TAG_TCFUN && EQ(*(p->gcobj), fun)) {
                SET(r, T_Special, 0);
                tc_fun = fun; tc_argl = argl; tc_env = The_Environment;
                return r;
            }
        }
    }
    r = frame = tail = newframe = Null;
    oldenv = The_Environment;
    GC_Link7_Tag_Compound (argl, fun, oldenv, frame, tail, newframe, r);
again:
    Get_Arglist_Length (argc, argl, r);   /* r is temporary variable here */
    min = COMPOUND(fun)->min_args;
    max = COMPOUND(fun)->max_args;
    if (argc < min)
        Primitive_Error ("too few arguments for ~s", fun);
    if (max >= 0 && argc > max)
        Primitive_Error ("too many arguments for ~s", fun);
    if (tail_calling) {
        tail = The_Environment;
        Switch_Environment (tail_call_env);
    } else {
        if (argc <= MAX_ARGS_ON_STACK)
            argv = abuf;
        else
            Alloca (argv, Object*, argc * sizeof (Object));
    }
    Tail_Call = 0;
    gcv.gclen = 1; gcv.gcobj = argv; gcv.next = &gc7; GC_List = &gcv;
    for (r = argl, i = 0; i < argc; i++, r = Cdr (r)) {
        argv[i] = eval ? Eval (Car (r)) : Car (r);
        gcv.gclen++;
    }
    if (tail_calling)
        Switch_Environment (tail);
    tail = Car (Cdr (COMPOUND(fun)->closure));
    for (i = 0; i < min; i++, tail = Cdr (tail))
        Lambda_Bind (Car (tail), argv[i]);
    if (max == -1) {
        rest = P_List (argc-i, argv+i);
        Lambda_Bind (tail, rest);
    }
    if (tail_calling) {
        Pop_Frame ();
        Push_Frame (newframe);
    } else {
        Switch_Environment (COMPOUND(fun)->env);
        Push_Frame (frame);
    }
    tail = Cdr (Cdr (COMPOUND(fun)->closure));
    for (i = COMPOUND(fun)->numforms; i > 1; i--, tail = Cdr (tail))
        (void)Eval (Car (tail));
    Tail_Call = 1;
    r = Eval (Car (tail));
    /*
     * If evaluation of the function body returned a T_Special object,
     * a tail-call has been taken place.  If it is a tail-call to a
     * different function, just return, otherwise unpack new arguments
     * and environment and jump to the beginning.
     */
    if (TYPE(r) == T_Special && EQ(fun, tc_fun)) {
        argl = tc_argl;
        tail_call_env = tc_env;
        tail_calling = 1;
        eval = 1;
        newframe = Null;
        goto again;
    }
    Tail_Call = 0;
    Pop_Frame ();
    Switch_Environment (oldenv);
    GC_Unlink;
    Alloca_End;
    return r;
}

Object Funcall (Object fun, Object argl, int eval) {
    register int t = TYPE(fun);
    Funcall_Switch (t, fun, argl, eval);
    /*NOTREACHED*/
}

void Check_Formals (Object x, int *min, int *max) {
    Object s, t1, t2;

    *min = *max = 0;
    for (t1 = Car (x); !Nullp (t1); t1 = Cdr (t1)) {
        s = TYPE(t1) == T_Pair ? Car (t1) : t1;
        Check_Type (s, T_Symbol);
        for (t2 = Car (x); !EQ(t2, t1); t2 = Cdr (t2))
            if (EQ(s, Car (t2)))
                Primitive_Error ("~s: duplicate variable binding", s);
        if (TYPE(t1) != T_Pair)
            break;
        (*min)++; (*max)++;
    }
    if (TYPE(t1) == T_Symbol)
        *max = -1;
    else if (!Nullp (t1))
        Wrong_Type_Combination (t1, "list or symbol");
}

Object P_Lambda (Object argl) {
    Object proc, closure;
    GC_Node2;

    proc = Null;
    GC_Link2 (argl, proc);
    proc = Make_Compound ();
    closure = Cons (Sym_Lambda, argl);
    COMPOUND(proc)->closure = closure;
    COMPOUND(proc)->env = The_Environment;
    COMPOUND(proc)->numforms = Fast_Length (Cdr (argl));
    Check_Formals (argl, &COMPOUND(proc)->min_args,
        &COMPOUND(proc)->max_args);
    GC_Unlink;
    return proc;
}

Object P_Procedure_Lambda (Object p) {
    Check_Type (p, T_Compound);
    return Copy_List (COMPOUND(p)->closure);
}

Object P_Procedure_Environment (Object p) {
    Check_Type (p, T_Compound);
    return COMPOUND(p)->env;
}

Object General_Map (int argc, Object *argv, register int accum) {
    register int i;
    Object *args;
    Object head, list, tail, cell, arglist, val;
    GC_Node2; GCNODE gcv;
    TC_Prolog;
    Alloca_Begin;

    Check_Procedure (argv[0]);
    Alloca (args, Object*, (argc-1) * sizeof (Object));
    list = tail = Null;
    GC_Link2 (list, tail);
    gcv.gclen = argc; gcv.gcobj = args; gcv.next = &gc2; GC_List = &gcv;
    while (1) {
        for (i = 1; i < argc; i++) {
            head = argv[i];
            if (Nullp (head)) {
                GC_Unlink;
                Alloca_End;
                return list;
            }
            Check_Type (head, T_Pair);
            args[i-1] = Car (head);
            argv[i] = Cdr (head);
        }
        arglist = P_List (argc-1, args);
        TC_Disable;
        val = Funcall (argv[0], arglist, 0);
        TC_Enable;
        if (!accum)
            continue;
        cell = Cons (val, Null);
        if (Nullp (list))
            list = cell;
        else
            (void)P_Set_Cdr (tail, cell);
        tail = cell;
    }
    /*NOTREACHED*/
}

Object P_Map (int argc, Object *argv) {
    return General_Map (argc, argv, 1);
}

Object P_For_Each (int argc, Object *argv) {
    return General_Map (argc, argv, 0);
}

Object Make_Macro () {
    Object mac;

    mac = Alloc_Object (sizeof (struct S_Macro), T_Macro, 0);
    MACRO(mac)->body = MACRO(mac)->name = Null;
    return mac;
}

Object P_Macro (Object argl) {
    Object mac, body;
    GC_Node2;

    mac = Null;
    GC_Link2 (argl, mac);
    mac = Make_Macro ();
    body = Cons (Sym_Macro, argl);
    MACRO(mac)->body = body;
    Check_Formals (argl, &MACRO(mac)->min_args, &MACRO(mac)->max_args);
    GC_Unlink;
    return mac;
}

Object P_Macro_Body (Object m) {
    Check_Type (m, T_Macro);
    return Copy_List (MACRO(m)->body);
}

Object Macro_Expand (Object mac, Object argl) {
    register int argc, min, max, i;
    Object frame, r, tail;
    GC_Node4;
    TC_Prolog;

    frame = tail = Null;
    GC_Link4 (argl, frame, tail, mac);
    Get_Arglist_Length (argc, argl, r);
    min = MACRO(mac)->min_args;
    max = MACRO(mac)->max_args;
    if (argc < min)
        Primitive_Error ("too few arguments for ~s", mac);
    if (max >= 0 && argc > max)
        Primitive_Error ("too many arguments for ~s", mac);
    tail = Car (Cdr (MACRO(mac)->body));
    for (i = 0; i < min; i++, tail = Cdr (tail), argl = Cdr (argl))
        frame = Add_Binding (frame, Car (tail), Car (argl));
    if (max == -1)
        frame = Add_Binding (frame, tail, argl);
    Push_Frame (frame);
    TC_Disable;
    r = Begin (Cdr (Cdr (MACRO(mac)->body)));
    TC_Enable;
    Pop_Frame ();
    GC_Unlink;
    return r;
}

Object P_Macro_Expand (Object form) {
    Object ret, mac;
    GC_Node;

    Check_Type (form, T_Pair);
    GC_Link (form);
    mac = Eval (Car (form));
    if (TYPE(mac) != T_Macro)
        ret = form;
    else
        ret = Macro_Expand (mac, Cdr (form));
    GC_Unlink;
    return ret;
}
