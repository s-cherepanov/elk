/* heap.c: Code that is common to both garbage collectors.
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

int GC_In_Progress;

GCNODE *GC_List;

static GCNODE *Global_GC_Obj;

static FUNCT *Before_GC_Funcs, *After_GC_Funcs;

static Object V_Garbage_Collect_Notifyp;
static Object Sym_Stop_And_Copy_GC, Sym_Generational_GC, Sym_Incremental_GC;

void Init_Heap () {
    Define_Variable (&V_Garbage_Collect_Notifyp, "garbage-collect-notify?",
        False);

    Define_Symbol (&Sym_Stop_And_Copy_GC, "stop-and-copy");
    Define_Symbol (&Sym_Generational_GC, "generational");
    Define_Symbol (&Sym_Incremental_GC, "incremental");
}

void Register_Before_GC (void (*f)(void)) {
    FUNCT *p;

    p = (FUNCT *)Safe_Malloc (sizeof (*p));
    p->func = f;
    p->next = Before_GC_Funcs;
    Before_GC_Funcs = p;
}

void Call_Before_GC () {
    FUNCT *p;

    for (p = Before_GC_Funcs; p; p = p->next)
        p->func();
}

void Register_After_GC (void (*f)(void)) {
    FUNCT *p;

    p = (FUNCT *)Safe_Malloc (sizeof (*p));
    p->func = f;
    p->next = After_GC_Funcs;
    After_GC_Funcs = p;
}

void Call_After_GC () {
    FUNCT *p;

    for (p = After_GC_Funcs; p; p = p->next)
        p->func();
}

void Visit_GC_List (GCNODE *list, ptrdiff_t delta) {
    register GCNODE *gp, *p;
    register int n;
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

void Visit_Wind (WIND *list, ptrdiff_t delta) {
    register WIND *wp, *p;

    for (wp = list; wp; wp = p->next) {
        p = (WIND *)NORM(wp);
        Visit (&p->inout);
    }
}

void Func_Global_GC_Link (Object *x) {
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

Object P_Garbage_Collect_Status (int argc, Object* argv) {
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
