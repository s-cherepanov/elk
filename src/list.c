/* list.c
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

extern unsigned int Stack_Size ();
extern void Uncatchable_Error (char *);

Object Const_Cons (Object car, Object cdr) {
    Object ret;

    ret = P_Cons (car, cdr);
    SETCONST(ret);
    return ret;
}

Object P_Cons (Object car, Object cdr) {
    Object cell;
    GC_Node2;

#ifdef GENERATIONAL_GC
    GC_Link2 (car, cdr);
    cell = Alloc_Object (sizeof (struct S_Pair), T_Pair, 0);
    GC_Unlink;
#else
    /* This is an optimization (it duplicates parts of Alloc_Object()):
     */

    register char *p;

    p = Hp;
    ALIGN(p);
    if (p + sizeof (struct S_Pair) <= Heap_End && !GC_Debug) {
        Hp = p + sizeof (struct S_Pair);
        SET(cell, T_Pair, (struct S_Pair *)p);
    } else {
        GC_Link2 (car, cdr);
        cell = Alloc_Object (sizeof (struct S_Pair), T_Pair, 0);
        GC_Unlink;
    }
#endif
    Car (cell) = car;
    Cdr (cell) = cdr;
    return cell;
}

Object P_Car (Object x) {
    Check_Type (x, T_Pair);
    return Car (x);
}

Object P_Cdr (Object x) {
    Check_Type (x, T_Pair);
    return Cdr (x);
}

Object Cxr (Object x, register char *pat, register int len) {
    Object ret;

    for (ret = x, pat += len; len > 0; len--)
        switch (*--pat) {
        case 'a': ret = P_Car (ret); break;
        case 'd': ret = P_Cdr (ret); break;
        default: Primitive_Error ("invalid pattern");
        }
    return ret;
}

Object P_Cddr   (Object x) { return Cxr (x,  "dd", 2); }
Object P_Cdar   (Object x) { return Cxr (x,  "da", 2); }
Object P_Cadr   (Object x) { return Cxr (x,  "ad", 2); }
Object P_Caar   (Object x) { return Cxr (x,  "aa", 2); }

Object P_Cdddr  (Object x) { return Cxr (x, "ddd", 3); }
Object P_Cddar  (Object x) { return Cxr (x, "dda", 3); }
Object P_Cdadr  (Object x) { return Cxr (x, "dad", 3); }
Object P_Cdaar  (Object x) { return Cxr (x, "daa", 3); }
Object P_Caddr  (Object x) { return Cxr (x, "add", 3); }
Object P_Cadar  (Object x) { return Cxr (x, "ada", 3); }
Object P_Caadr  (Object x) { return Cxr (x, "aad", 3); }
Object P_Caaar  (Object x) { return Cxr (x, "aaa", 3); }

Object P_Caaaar (Object x) { return Cxr (x, "aaaa", 4); }
Object P_Caaadr (Object x) { return Cxr (x, "aaad", 4); }
Object P_Caadar (Object x) { return Cxr (x, "aada", 4); }
Object P_Caaddr (Object x) { return Cxr (x, "aadd", 4); }
Object P_Cadaar (Object x) { return Cxr (x, "adaa", 4); }
Object P_Cadadr (Object x) { return Cxr (x, "adad", 4); }
Object P_Caddar (Object x) { return Cxr (x, "adda", 4); }
Object P_Cadddr (Object x) { return Cxr (x, "addd", 4); }
Object P_Cdaaar (Object x) { return Cxr (x, "daaa", 4); }
Object P_Cdaadr (Object x) { return Cxr (x, "daad", 4); }
Object P_Cdadar (Object x) { return Cxr (x, "dada", 4); }
Object P_Cdaddr (Object x) { return Cxr (x, "dadd", 4); }
Object P_Cddaar (Object x) { return Cxr (x, "ddaa", 4); }
Object P_Cddadr (Object x) { return Cxr (x, "ddad", 4); }
Object P_Cdddar (Object x) { return Cxr (x, "ddda", 4); }
Object P_Cddddr (Object x) { return Cxr (x, "dddd", 4); }

Object P_Cxr (Object x, Object pat) {
    Check_List (x);
    if (TYPE(pat) == T_Symbol)
        pat = SYMBOL(pat)->name;
    else if (TYPE(pat) != T_String)
        Wrong_Type_Combination (pat, "string or symbol");
    return Cxr (x, STRING(pat)->data, STRING(pat)->size);
}

Object P_Nullp (Object x) {
    return Nullp (x) ? True : False;
}

Object P_Pairp (Object x) {
    return TYPE(x) == T_Pair ? True : False;
}

Object P_Listp (Object x) {
    Object s;
    register int f;

    for (s = x, f = 0; !Nullp (x); f ^= 1) {
        if (TYPE(x) != T_Pair)
            return False;
        x = Cdr (x);
        if (EQ(x, s))
            return False;
        if (f) s = Cdr (s);
    }
    return True;
}

Object P_Set_Car (Object x, Object new) {
    Check_Type (x, T_Pair);
    Check_Mutable (x);
    Car (x) = new;
    return new;
}

Object P_Set_Cdr (Object x, Object new) {
    Check_Type (x, T_Pair);
    Check_Mutable (x);
    Cdr (x) = new;
    return new;
}

Object General_Member (Object key, Object list, register int comp) {
    register int r;

    for ( ; !Nullp (list); list = Cdr (list)) {
        Check_List (list);
        if (comp == 0)
            r = EQ(Car (list), key);
        else if (comp == 1)
            r = Eqv (Car (list), key);
        else
            r = Equal (Car (list), key);
        if (r) return list;
    }
    return False;
}

Object P_Memq (Object key, Object list) {
    return General_Member (key, list, 0);
}

Object P_Memv (Object key, Object list) {
    return General_Member (key, list, 1);
}

Object P_Member (Object key, Object list) {
    return General_Member (key, list, 2);
}

Object General_Assoc (Object key, Object alist, register int comp) {
    Object elem;
    register int r;

    for ( ; !Nullp (alist); alist = Cdr (alist)) {
        Check_List (alist);
        elem = Car (alist);
        if (TYPE(elem) != T_Pair)
            continue;
        if (comp == 0)
            r = EQ(Car (elem), key);
        else if (comp == 1)
            r = Eqv (Car (elem), key);
        else
            r = Equal (Car (elem), key);
        if (r) return elem;
    }
    return False;
}

Object P_Assq (Object key, Object alist) {
    return General_Assoc (key, alist, 0);
}

Object P_Assv (Object key, Object alist) {
    return General_Assoc (key, alist, 1);
}

Object P_Assoc (Object key, Object alist) {
    return General_Assoc (key, alist, 2);
}

int Fast_Length (Object list) {
    Object tail;
    register int i;

    for (i = 0, tail = list; TYPE(tail) == T_Pair; tail = Cdr (tail), i++)
        ;
    return i;
}

Object P_Length (Object list) {
    Object tail;
    register int i;

    for (i = 0, tail = list; !Nullp (tail); tail = Cdr (tail), i++)
        Check_List (tail);
    return Make_Integer (i);
}

Object P_Make_List (Object n, Object init) {
    register int len;
    Object list;
    GC_Node;

    if ((len = Get_Exact_Integer (n)) < 0)
        Range_Error (n);
    list = Null;
    GC_Link (init);
    while (len-- > 0)
        list = Cons (init, list);
    GC_Unlink;
    return list;
}

Object P_List (int argc, Object *argv) {
    Object list, tail, cell;
    GC_Node2;

    GC_Link2 (list, tail);
    for (list = tail = Null; argc-- > 0; tail = cell) {
        cell = Cons (*argv++, Null);
        if (Nullp (list))
            list = cell;
        else
            (void)P_Set_Cdr (tail, cell);
    }
    GC_Unlink;
    return list;
}

Object P_Last_Pair (Object x) {
    Check_Type (x, T_Pair);
    for ( ; TYPE(Cdr (x)) == T_Pair; x = Cdr (x)) ;
    return x;
}

Object P_Append (int argc, Object *argv) {
    Object list, last, tail, cell;
    register int i;
    GC_Node3;

    list = last = Null;
    GC_Link3 (list, last, tail);
    for (i = 0; i < argc-1; i++) {
        for (tail = argv[i]; !Nullp (tail); tail = Cdr (tail)) {
            Check_List (tail);
            cell = Cons (Car (tail), Null);
            if (Nullp (list))
                list = cell;
            else
                (void)P_Set_Cdr (last, cell);
            last = cell;
        }
    }
    if (argc) {
        if (Nullp (list))
            list = argv[i];
        else
            (void)P_Set_Cdr (last, argv[i]);
    }
    GC_Unlink;
    return list;
}

Object P_Append_Set (int argc, Object *argv) {
    register int i, j;

    for (i = j = 0; i < argc; i++)
        if (!Nullp (argv[i]))
            argv[j++] = argv[i];
    if (j == 0)
        return Null;
    for (i = 0; i < j-1; i++)
        (void)P_Set_Cdr (P_Last_Pair (argv[i]), argv[i+1]);
    return *argv;
}

Object P_Reverse (Object x) {
    Object ret;
    GC_Node;

    GC_Link (x);
    for (ret = Null; !Nullp (x); x = Cdr (x)) {
        Check_List (x);
        ret = Cons (Car (x), ret);
    }
    GC_Unlink;
    return ret;
}

Object P_Reverse_Set (Object x) {
    Object prev, tail;

    for (prev = Null; !Nullp (x); prev = x, x = tail) {
        Check_List (x);
        tail = Cdr (x);
        (void)P_Set_Cdr (x, prev);
    }
    return prev;
}

Object P_List_Tail (Object x, Object num) {
    register int n;

    for (n = Get_Exact_Integer (num); n > 0 && !Nullp (x); n--, x = P_Cdr (x))
        ;
    return x;
}

Object P_List_Ref (Object x, Object num) {
    return P_Car (P_List_Tail (x, num));
}

Object Copy_List (Object x) {
    Object car, cdr;
    GC_Node3;

    if (TYPE(x) == T_Pair) {
        if (Stack_Size () > Max_Stack)
            Uncatchable_Error ("Out of stack space");
        car = cdr = Null;
        GC_Link3 (x, car, cdr);
        car = Copy_List (Car (x));
        cdr = Copy_List (Cdr (x));
        x = Cons (car, cdr);
        GC_Unlink;
    }
    return x;
}
