/* Continuations and dynamic-wind.
 */

#include "kernel.h"

/* The C library versions of longjmp on the VAX and the Convex unwind
 * the stack.  As Jump_Cont below installs a new stack before calling
 * longjmp, the standard version cannot be used.  The following simplistic
 * version of setjmp/longjmp is used instead:
 */

#if defined(vax) || defined(__vax__)
  __asm__("    .globl  _setjmp");
  __asm__("_setjmp:");
  __asm__("    .word   0");
  __asm__("    movl    4(ap),r0");
  __asm__("    movq    r2,(r0)+");
  __asm__("    movq    r4,(r0)+");
  __asm__("    movq    r6,(r0)+");
  __asm__("    movq    r8,(r0)+");
  __asm__("    movq    r10,(r0)+");
  __asm__("    movl    fp,(r0)+");
  __asm__("    movq    4(fp),(r0)+");
  __asm__("    movq    12(fp),(r0)+");
  __asm__("    movq    20(fp),(r0)");
  __asm__("    clrl    r0");
  __asm__("    ret");

  __asm__("    .globl  _longjmp");
  __asm__("_longjmp:");
  __asm__("    .word   0");
  __asm__("    movl    4(ap),r0");
  __asm__("    movq    (r0)+,r2");
  __asm__("    movq    (r0)+,r4");
  __asm__("    movq    (r0)+,r6");
  __asm__("    movq    (r0)+,r8");
  __asm__("    movq    (r0)+,r10");
  __asm__("    movl    (r0)+,r1");
  __asm__("    movq    (r0)+,4(r1)");
  __asm__("    movq    (r0)+,12(r1)");
  __asm__("    movq    (r0),20(r1)");
  __asm__("    movl    8(ap),r0");
  __asm__("    movl    r1,fp");
  __asm__("    ret");
#endif

#if defined(convex) || defined(__convex__)
convex_longjmp (p, i) char *p; {
    __asm__("ld.w    4(ap),s0");
    __asm__("ld.w    0(ap),a1");
    __asm__("ld.w    12(a1),a7");
    __asm__("ld.w    16(a1),a0");
    __asm__("ld.w    8(a1),a3");
    __asm__("mov     a3,psw");
    __asm__("ld.w    4(a1),a2");
    __asm__("jmp     0(a2)");
}
#define longjmp convex_longjmp
#endif


WIND *First_Wind, *Last_Wind;

static Object Cont_Value;
#ifndef USE_ALLOCA
static Object Cont_GCsave;
#endif

Check_Stack_Grows_Down () {
    char foo;

    return &foo < stkbase;
}

/* Stack_Size returns the current stack size relative to stkbase.
 * It works independent of the direction into which the stack grows
 * (the stack grows upwards on HP-PA based machines and Pyramids).
 */
int Stack_Size () {
    char foo;

    return Stack_Grows_Down ? stkbase-&foo : &foo-stkbase;
}

Grow_Stack (cp, val) struct S_Control *cp; Object val; {
    char buf[100];

    /* Prevent the optimizer from optimizing buf away:
     */
    bzero (buf, 1);

    Jump_Cont (cp, val);
}

Jump_Cont (cp, val) struct S_Control *cp; Object val; {
    static struct S_Control *p;
    static char *from, *to;      /* Must not be allocated on stack */
    static i;                    /* Ditto */
    char foo;
    
    /* Reinstall the saved stack contents; take stack direction
     * into account.  cp must be put into a static variable, as
     * variables living on the stack cannot be referenced any
     * longer after the new stack has been installed.
     *
     * (The asm below must not be the first statement in the function
     * to prevent buggy Sun ANSI SPARCompiler C 2.0.1 from emitting
     * it at the wrong position.)
     */
    p = cp;
    Cont_Value = val;
    if (Stack_Grows_Down) {
	if (stkbase - &foo < p->size) Grow_Stack (cp, val);
	to = stkbase - p->size;
    } else {
	if (stkbase + p->size > &foo) Grow_Stack (cp, val);
	to = stkbase;
    }
    from = p->stack;
#if defined(sparc) || defined(__sparc__)
    __asm__("t 0x3");   /* Flush register window */
#endif
    for (i = p->size; i > 0; i--)
	*to++ = *from++;
    longjmp (p->j, 1);
}

#ifndef USE_ALLOCA
Object Terminate_Cont (cont) Object cont; {
    Free_Mem_Nodes (CONTROL(cont)->memlist);
    return Void;
}
#endif

Object P_Control_Pointp (x) Object x; {
    return TYPE(x) == T_Control_Point ? True : False;
}

Object P_Call_With_Current_Continuation (proc) Object proc; {
    register t;

    t = TYPE(proc);
    if (t != T_Primitive && t != T_Compound && t != T_Control_Point)
	Wrong_Type_Combination (proc, "procedure");
    return Internal_Call_CC (0, proc);
}

Object Internal_Call_CC (from_dump, proc) int from_dump; Object proc; {
    Object control, ret, gcsave;
    register struct S_Control *cp;
    register char *p, *to;
    register size;
    GC_Node3;

    control = gcsave = Null;
    GC_Link3 (proc, control, gcsave);
#ifndef USE_ALLOCA
    gcsave = Save_GC_Nodes ();
#endif

    size = Stack_Size ();
    size = (size + 7) & ~7;
    control = Alloc_Object (size + sizeof (struct S_Control) - 1,
	T_Control_Point, 0);
    cp = CONTROL(control);
    cp->env = The_Environment;
    cp->gclist = GC_List;
    cp->firstwind = First_Wind;
    cp->lastwind = Last_Wind;
    cp->tailcall = Tail_Call;
    cp->intrlevel = Intr_Level;
    cp->size = size;
    cp->memsave = Null;
    cp->gcsave = gcsave;
#if defined(sparc) || defined(__sparc__)
    __asm__("t 0x3");   /* Flush register window */
#endif
    /* Save the current stack contents; take stack direction
     * into account.  delta holds the number of bytes by which
     * the stack contents has been moved in memory (it is required
     * to access variables on the saved stack later):
     */
    p = Stack_Grows_Down ? stkbase - cp->size : stkbase;
    to = cp->stack;
    bcopy (p, to, cp->size);
    cp->delta = to - p;
#ifndef USE_ALLOCA
    Register_Object (control, (GENERIC)0, Terminate_Cont, 0);
    Save_Mem_Nodes (control);
#endif
    if (setjmp (CONTROL(control)->j) != 0) {
#ifndef USE_ALLOCA
	Restore_GC_Nodes (Cont_GCsave);
#endif
	if (Intr_Level == 0) {
	    Force_Enable_Interrupts;
	} else {
	    Force_Disable_Interrupts;
	}
	return Cont_Value;
    }
    if (from_dump) {
#ifdef CAN_DUMP
	Dump_Control_Point = control;
#endif
	ret = False;
    } else {
	control = Cons (control, Null);
	ret = Funcall (proc, control, 0);
    }
    GC_Unlink;
    return ret;
}

Funcall_Control_Point (control, argl, eval) Object control, argl; {
    Object val, len;
    register struct S_Control *cp;
    register WIND *w, *wp, *cwp, *p;
    register delta = 0;
    GC_Node3;

    if (GC_In_Progress)
	Fatal_Error ("jumping out of GC");
    val = Null;
    GC_Link3 (argl, control, val);
    len = P_Length (argl);
    if (FIXNUM(len) != 1)
	Primitive_Error ("control point expects one argument");
    val = Car (argl);
    if (eval)
	val = Eval (val);
    delta = CONTROL(control)->delta;
    wp = First_Wind;
    cwp = CONTROL(control)->firstwind;
    while (wp && cwp) {
	p = (WIND *)NORM(wp);
	if (!EQ(wp->inout,p->inout)) break;
	wp = wp->next;
	cwp = p->next;
    }
    if (wp) {
	for (w = Last_Wind; w != wp->prev; w = w->prev)
	    Do_Wind (Cdr (w->inout));
    }
    while (cwp) {
	delta = CONTROL(control)->delta;
	p = (WIND *)NORM(cwp);
	cwp = p->next;
	Do_Wind (Car (p->inout));
    }
    GC_Unlink;
    Disable_Interrupts;
    cp = CONTROL(control);
    Switch_Environment (cp->env);
    GC_List = cp->gclist;
#ifndef USE_ALLOCA
    Restore_Mem_Nodes (control);
    Cont_GCsave = CONTROL(control)->gcsave;
#endif
    First_Wind = cp->firstwind;
    Last_Wind = cp->lastwind;
    Intr_Level = cp->intrlevel;
    Jump_Cont (cp, val);
    /*NOTREACHED*/
}

Do_Wind (w) Object w; {
    Object oldenv, b, tmp;

    if (TYPE(w) == T_Vector) {          /* fluid-let */
	oldenv = The_Environment;
	Switch_Environment (VECTOR(w)->data[1]);
	b = Lookup_Symbol (VECTOR(w)->data[0], 0);
	if (Nullp (b))
	    Panic ("fluid-let");
	tmp = VECTOR(w)->data[2];
	VECTOR(w)->data[2] = Cdr (b);
	Cdr (b) = tmp;
	SYMBOL(Car (b))->value = tmp;
	VECTOR(w)->data[1] = oldenv;
	Switch_Environment (oldenv);
    } else {                            /* dynamic-wind */
	(void)Funcall (w, Null, 0);
    }
}

Add_Wind (w, in, out) register WIND *w; Object in, out; {
    Object inout;
    GC_Node2;

    GC_Link2 (in, out);
    inout = Cons (in, out);
    w->inout = inout;
    w->next = 0;
    if (First_Wind == 0)
	First_Wind = w;
    else
	Last_Wind->next = w;
    w->prev = Last_Wind;
    Last_Wind = w;
    GC_Unlink;
}

Object P_Dynamic_Wind (in, body, out) Object in, body, out; {
    WIND w, *first = First_Wind;
    Object ret;
    GC_Node4;

    Check_Procedure (in);
    Check_Procedure (body);
    Check_Procedure (out);
    ret = Null;
    GC_Link4 (in, body, out, ret);
    Add_Wind (&w, in, out);
    (void)Funcall (in, Null, 0);
    ret = Funcall (body, Null, 0);
    (void)Funcall (out, Null, 0);
    if (Last_Wind = w.prev)
	Last_Wind->next = 0;
    First_Wind = first;
    GC_Unlink;
    return ret;
}

Object P_Control_Point_Environment (c) Object c; {
    Check_Type (c, T_Control_Point);
    return CONTROL(c)->env;
}
