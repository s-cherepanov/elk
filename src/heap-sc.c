/* heap-sc.c: Stop-and-copy garbage collector.
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

#include <string.h>

extern void Uncatchable_Error (char *);
extern unsigned int Stack_Size ();
extern void *sbrk();

#define Recursive_Visit(p) {\
    register Object *pp = p;\
    if (Stack_Size () > Max_Stack)\
	Fatal_Error("stack overflow during GC (increase stack limit)");\
    if (Types[TYPE(*pp)].haspointer) Visit (pp);\
}

char *Heap_Start,
     *Hp,                     /* First free byte */
     *Heap_End,               /* Points behind free bytes */
     *Free_Start,             /* Start of free area */
     *Free_End;               /* Points behind free area */

static char *To;

void Make_Heap (int size) {
    register unsigned int k = 1024 * size;
    register unsigned int s = 2 * k;

    if ((Hp = Heap_Start = (char *)sbrk (s)) == (char *)-1)
	Fatal_Error ("cannot allocate heap (%u KBytes)", 2*size);
    Heap_End = Heap_Start + k;
    Free_Start = Heap_End;
    Free_End = Free_Start + k;
}

void Free_Heap () {
    /* Do nothing. */
}

Object Alloc_Object (int size, int type, int konst) {
    register char *p = Hp;
    Object ret;

    if (GC_Debug) {
	(void)P_Collect ();
	p = Hp;
    }
    ALIGN(p);
    if (p + size > Heap_End) {
	(void)P_Collect ();
	p = Hp;
	ALIGN(p);
	if (p + size > Heap_End - HEAP_MARGIN)
	    Uncatchable_Error ("Out of heap space");
    }
    Hp = p + size;
    *(Object *)p = Null;
    SET(ret, type, p);
    if (konst)
	SETCONST(ret);
    return ret;
}

Object P_Collect () {
    register char *tmp;
    register int msg = 0;
    Object a[2];

    if (!Interpreter_Initialized)
	Fatal_Error ("heap too small (increase heap size)");
    if (GC_In_Progress)
	Fatal_Error ("GC while GC in progress");
    Disable_Interrupts;
    GC_In_Progress = 1;
    Call_Before_GC ();
    if (GC_Debug) {
	printf ("."); (void)fflush (stdout);
    } else if (Var_Is_True (V_Garbage_Collect_Notifyp)) {
	msg++;
	Format (Standard_Output_Port, "[Garbage collecting... ", 23, 0,
	    (Object *)0);
	(void)fflush (stdout);
    }
    To = Free_Start;
    Visit_GC_List (Global_GC_Obj, 0);
    Visit_GC_List (GC_List, 0);
    Visit_Wind (First_Wind, 0);
    Hp = To;
    tmp = Heap_Start; Heap_Start = Free_Start; Free_Start = tmp;
    tmp = Heap_End; Heap_End = Free_End; Free_End = tmp;
    if (!GC_Debug) {
	if (msg) {
	    a[0] = Make_Integer ((Hp-Heap_Start) / 1024);
	    a[1] = Make_Integer ((Heap_End-Heap_Start) / 1024);
	    Format (Standard_Output_Port, "~sK of ~sK]~%", 13, 2, a);
	}
    }
    Call_After_GC ();
    GC_In_Progress = 0;
    Enable_Interrupts;
    return Void;
}

int Visit (register Object *p) {
    register Object *tag;
    register int t, size, reloc = 0;

again:
    t = TYPE(*p);
    if (!Types[t].haspointer)
	return 0;
    tag = (Object *)POINTER(*p);
    if ((char *)tag >= Free_Start && (char *)tag < Free_End)
	return 0;
    if (TYPE(*tag) == T_Broken_Heart) {
	SETPOINTER(*p, POINTER(*tag));
	return 0;
    }
    ALIGN(To);
    switch (t) {
    case T_Bignum:
	size = sizeof (struct S_Bignum) - sizeof (gran_t)
	       + BIGNUM(*p)->size * sizeof (gran_t);
	memcpy (To, tag, size);
	break;
    case T_Flonum:
	size = sizeof (struct S_Flonum);
	*(struct S_Flonum *)To = *(struct S_Flonum *)tag;
	break;
    case T_Symbol:
	size = sizeof (struct S_Symbol);
	*(struct S_Symbol *)To = *(struct S_Symbol *)tag;
	break;
    case T_Pair:
    case T_Environment:
	size = sizeof (struct S_Pair);
	*(struct S_Pair *)To = *(struct S_Pair *)tag;
	break;
    case T_String:
	size = sizeof (struct S_String) + STRING(*p)->size - 1;
	memcpy (To, tag, size);
	break;
    case T_Vector:
	size = sizeof (struct S_Vector) + (VECTOR(*p)->size - 1) *
	    sizeof (Object);
	memcpy (To, tag, size);
	break;
    case T_Primitive:
	size = sizeof (struct S_Primitive);
	*(struct S_Primitive *)To = *(struct S_Primitive *)tag;
	break;
    case T_Compound:
	size = sizeof (struct S_Compound);
	*(struct S_Compound *)To = *(struct S_Compound *)tag;
	break;
    case T_Control_Point:
	size = sizeof (struct S_Control) + CONTROL(*p)->size - 1;
	reloc = To - (char *)tag;
	memcpy (To, tag, size);
	break;
    case T_Promise:
	size = sizeof (struct S_Promise);
	*(struct S_Promise *)To = *(struct S_Promise *)tag;
	break;
    case T_Port:
	size = sizeof (struct S_Port);
	*(struct S_Port *)To = *(struct S_Port *)tag;
	break;
    case T_Autoload:
	size = sizeof (struct S_Autoload);
	*(struct S_Autoload *)To = *(struct S_Autoload *)tag;
	break;
    case T_Macro:
	size = sizeof (struct S_Macro);
	*(struct S_Macro *)To = *(struct S_Macro *)tag;
	break;
    case T_Broken_Heart:
	Panic ("broken heart in GC");
    default:
	if (t < 0 || t >= Num_Types)
	    Panic ("bad type in GC");
	if (Types[t].size == NOFUNC)
	    size = Types[t].const_size;
	else
	    size = (Types[t].size)(*p);
	memcpy (To, tag, size);
    }
    SETPOINTER(*p, To);
    SET(*tag, T_Broken_Heart, To);
    To += size;
    if (To > Free_End)
	Panic ("free exhausted in GC");
    switch (t) {
    case T_Symbol:
	Recursive_Visit (&SYMBOL(*p)->next);
	Recursive_Visit (&SYMBOL(*p)->name);
	Recursive_Visit (&SYMBOL(*p)->value);
	p = &SYMBOL(*p)->plist;
	goto again;
    case T_Pair:
    case T_Environment:
	Recursive_Visit (&PAIR(*p)->car);
	p = &PAIR(*p)->cdr;
	goto again;
    case T_Vector: {
	    register int i, n;
	    for (i = 0, n = VECTOR(*p)->size; i < n; i++)
		Recursive_Visit (&VECTOR(*p)->data[i]);
	    break;
	}
    case T_Compound:
	Recursive_Visit (&COMPOUND(*p)->closure);
	Recursive_Visit (&COMPOUND(*p)->env);
	p = &COMPOUND(*p)->name;
	goto again;
    case T_Control_Point:
	Recursive_Visit (&CONTROL(*p)->memsave);
	CONTROL(*p)->delta += reloc;
#ifdef USE_ALLOCA
	Visit_GC_List (CONTROL(*p)->gclist, CONTROL(*p)->delta);
#else
	Recursive_Visit (&CONTROL(*p)->gcsave);
#endif
	Visit_Wind (CONTROL(*p)->firstwind, CONTROL(*p)->delta);
	p = &CONTROL(*p)->env;
	goto again;
    case T_Promise:
	Recursive_Visit (&PROMISE(*p)->env);
	p = &PROMISE(*p)->thunk;
	goto again;
    case T_Port:
	p = &PORT(*p)->name;
	goto again;
    case T_Autoload:
	Recursive_Visit (&AUTOLOAD(*p)->files);
	p = &AUTOLOAD(*p)->env;
	goto again;
    case T_Macro:
	Recursive_Visit (&MACRO(*p)->body);
	p = &MACRO(*p)->name;
	goto again;
    default:
	if (Types[t].visit)
	    (Types[t].visit)(p, Visit);
    }

    return 0;
}

Object Internal_GC_Status (strat, flags) {
    return (Cons (Sym_Stop_And_Copy_GC, Null));
}
