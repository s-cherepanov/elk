/* Code that is common to both garbage collectors.
 */

#include "kernel.h"

int GC_In_Progress;

GCNODE *GC_List;

static GCNODE *Global_GC_Obj;

static FUNCT *Before_GC_Funcs, *After_GC_Funcs;

static Object V_Garbage_Collect_Notifyp;
static Object Sym_Stop_And_Copy_GC, Sym_Generational_GC, Sym_Incremental_GC;

Init_Heap () {
    Define_Variable (&V_Garbage_Collect_Notifyp, "garbage-collect-notify?",
	True);

    Define_Symbol (&Sym_Stop_And_Copy_GC, "stop-and-copy");
    Define_Symbol (&Sym_Generational_GC, "generational");
    Define_Symbol (&Sym_Incremental_GC, "incremental");
}

Register_Before_GC (f) void (*f)(); {
    FUNCT *p;

    p = (FUNCT *)Safe_Malloc (sizeof (*p));
    p->func = f;
    p->next = Before_GC_Funcs;
    Before_GC_Funcs = p;
}

Call_Before_GC () {
    FUNCT *p;

    for (p = Before_GC_Funcs; p; p = p->next)
	p->func();
}

Register_After_GC (f) void (*f)(); {
    FUNCT *p;

    p = (FUNCT *)Safe_Malloc (sizeof (*p));
    p->func = f;
    p->next = After_GC_Funcs;
    After_GC_Funcs = p;
}

Call_After_GC () {
    FUNCT *p;

    for (p = After_GC_Funcs; p; p = p->next)
	p->func();
}

Visit_GC_List (list, delta) GCNODE *list; {
    register GCNODE *gp, *p;
    register n;
    register Object *vec;

    for (gp = list; gp; gp = p->next) {
	p = (GCNODE *)NORM(gp);
	if (p->gclen <= 0) {
	    Visit ((Object *)NORM(p->gcobj));
	} else {
	    vec = (Object *)NORM(p->gcobj);
	    for (n = 0; n < p->gclen-1; n++)
		Visit (&vec[n]);
	}
    }
}

Visit_Wind (list, delta) WIND *list; unsigned delta; {
    register WIND *wp, *p;

    for (wp = list; wp; wp = p->next) {
	p = (WIND *)NORM(wp);
	Visit (&p->inout);
    }
}

Func_Global_GC_Link (x) Object *x; {
    GCNODE *p;

    p = (GCNODE *)Safe_Malloc (sizeof (*p));
    p->gclen = 0;
    p->gcobj = x;
    p->next = Global_GC_Obj;
    Global_GC_Obj = p;
}

#define GC_STRAT_SAC   1
#define GC_STRAT_GEN   2

#define GC_FLAGS_INCR  1

Object Internal_GC_Status();

Object P_Garbage_Collect_Status (argc, argv) Object* argv; {
    int strat = 0, flags = 0;

    if (argc > 0) {
	Check_Type (argv[0], T_Symbol);
        if (EQ (argv[0], Sym_Stop_And_Copy_GC))
	    strat = GC_STRAT_SAC;
        else if (EQ (argv[0], Sym_Generational_GC))
	    strat = GC_STRAT_GEN;
	else Primitive_Error ("unknown GC strategy: ~s", argv[0]);
	if (argc == 2) {
	    Check_Type (argv[1], T_Symbol);
	    if (EQ (argv[1], Sym_Incremental_GC))
		flags = GC_FLAGS_INCR;
	    else Primitive_Error ("unknown GC strategy: ~s", argv[1]);
	}
    }
    return Internal_GC_Status (strat, flags);
}

#ifdef GENERATIONAL_GC
#  include "heap-gen.c"
#else
#  include "heap-sc.c"
#endif
