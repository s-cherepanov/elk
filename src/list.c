#include "kernel.h"

Object Const_Cons (car, cdr) Object car, cdr; {
    Object ret;
    
    ret = P_Cons (car, cdr);
    SETCONST(ret);
    return ret;
}

Object P_Cons (car, cdr) Object car, cdr; {
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

Object P_Car (x) Object x; {
    Check_Type (x, T_Pair);
    return Car (x);
}

Object P_Cdr (x) Object x; {
    Check_Type (x, T_Pair);
    return Cdr (x);
}

Object Cxr (x, pat, len) Object x; register char *pat; register len; {
    Object ret;

    for (ret = x, pat += len; len > 0; len--)
	switch (*--pat) {
	case 'a': ret = P_Car (ret); break;
	case 'd': ret = P_Cdr (ret); break;
	default: Primitive_Error ("invalid pattern");
	}
    return ret;
}

Object P_Cddr   (x) Object x; { return Cxr (x,  "dd", 2); }
Object P_Cdar   (x) Object x; { return Cxr (x,  "da", 2); }
Object P_Cadr   (x) Object x; { return Cxr (x,  "ad", 2); }
Object P_Caar   (x) Object x; { return Cxr (x,  "aa", 2); }

Object P_Cdddr  (x) Object x; { return Cxr (x, "ddd", 3); }
Object P_Cddar  (x) Object x; { return Cxr (x, "dda", 3); }
Object P_Cdadr  (x) Object x; { return Cxr (x, "dad", 3); }
Object P_Cdaar  (x) Object x; { return Cxr (x, "daa", 3); }
Object P_Caddr  (x) Object x; { return Cxr (x, "add", 3); }
Object P_Cadar  (x) Object x; { return Cxr (x, "ada", 3); }
Object P_Caadr  (x) Object x; { return Cxr (x, "aad", 3); }
Object P_Caaar  (x) Object x; { return Cxr (x, "aaa", 3); }

Object P_Caaaar (x) Object x; { return Cxr (x, "aaaa", 4); }
Object P_Caaadr (x) Object x; { return Cxr (x, "aaad", 4); }
Object P_Caadar (x) Object x; { return Cxr (x, "aada", 4); }
Object P_Caaddr (x) Object x; { return Cxr (x, "aadd", 4); }
Object P_Cadaar (x) Object x; { return Cxr (x, "adaa", 4); }
Object P_Cadadr (x) Object x; { return Cxr (x, "adad", 4); }
Object P_Caddar (x) Object x; { return Cxr (x, "adda", 4); }
Object P_Cadddr (x) Object x; { return Cxr (x, "addd", 4); }
Object P_Cdaaar (x) Object x; { return Cxr (x, "daaa", 4); }
Object P_Cdaadr (x) Object x; { return Cxr (x, "daad", 4); }
Object P_Cdadar (x) Object x; { return Cxr (x, "dada", 4); }
Object P_Cdaddr (x) Object x; { return Cxr (x, "dadd", 4); }
Object P_Cddaar (x) Object x; { return Cxr (x, "ddaa", 4); }
Object P_Cddadr (x) Object x; { return Cxr (x, "ddad", 4); }
Object P_Cdddar (x) Object x; { return Cxr (x, "ddda", 4); }
Object P_Cddddr (x) Object x; { return Cxr (x, "dddd", 4); }

Object P_Cxr (x, pat) Object x, pat; {
    Check_List (x);
    if (TYPE(pat) == T_Symbol)
	pat = SYMBOL(pat)->name;
    else if (TYPE(pat) != T_String)
	Wrong_Type_Combination (pat, "string or symbol");
    return Cxr (x, STRING(pat)->data, STRING(pat)->size);
}

Object P_Nullp (x) Object x; {
    return Nullp (x) ? True : False;
}

Object P_Pairp (x) Object x; {
    return TYPE(x) == T_Pair ? True : False;
}

Object P_Listp (x) Object x; {
    Object s;
    register f;

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

Object P_Set_Car (x, new) Object x, new; {
    Check_Type (x, T_Pair);
    Check_Mutable (x);
    Car (x) = new;
    return new;
}

Object P_Set_Cdr (x, new) Object x, new; {
    Check_Type (x, T_Pair);
    Check_Mutable (x);
    Cdr (x) = new;
    return new;
}

Object General_Member (key, list, comp) Object key, list; register comp; {
    register r;

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

Object P_Memq (key, list) Object key, list; {
    return General_Member (key, list, 0);
}

Object P_Memv (key, list) Object key, list; {
    return General_Member (key, list, 1);
}

Object P_Member (key, list) Object key, list; {
    return General_Member (key, list, 2);
}

Object General_Assoc (key, alist, comp) Object key, alist; register comp; {
    Object elem;
    register r;

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

Object P_Assq (key, alist) Object key, alist; {
    return General_Assoc (key, alist, 0);
}

Object P_Assv (key, alist) Object key, alist; {
    return General_Assoc (key, alist, 1);
}

Object P_Assoc (key, alist) Object key, alist; {
    return General_Assoc (key, alist, 2);
}

Fast_Length (list) Object list; {
    Object tail;
    register i;

    for (i = 0, tail = list; TYPE(tail) == T_Pair; tail = Cdr (tail), i++)
	;
    return i;
}

Object P_Length (list) Object list; {
    Object tail;
    register i;

    for (i = 0, tail = list; !Nullp (tail); tail = Cdr (tail), i++)
	Check_List (tail);
    return Make_Integer (i);
}

Object P_Make_List (n, init) Object n, init; {
    register len;
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

Object P_List (argc, argv) Object *argv; {
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

Object P_Last_Pair (x) Object x; {
    Check_Type (x, T_Pair);
    for ( ; TYPE(Cdr (x)) == T_Pair; x = Cdr (x)) ;
    return x;
}

Object P_Append (argc, argv) Object *argv; {
    Object list, last, tail, cell;
    register i;
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
    if (argc)
	if (Nullp (list))
	    list = argv[i];
	else
	    (void)P_Set_Cdr (last, argv[i]);
    GC_Unlink;
    return list;
}

Object P_Append_Set (argc, argv) Object *argv; {
    register i, j;

    for (i = j = 0; i < argc; i++)
	if (!Nullp (argv[i]))
	    argv[j++] = argv[i];
    if (j == 0)
	return Null;
    for (i = 0; i < j-1; i++)
	(void)P_Set_Cdr (P_Last_Pair (argv[i]), argv[i+1]);
    return *argv;
}

Object P_Reverse (x) Object x; {
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

Object P_Reverse_Set (x) Object x; {
    Object prev, tail;

    for (prev = Null; !Nullp (x); prev = x, x = tail) {
	Check_List (x);
	tail = Cdr (x);
	(void)P_Set_Cdr (x, prev);
    }
    return prev;
}

Object P_List_Tail (x, num) Object x, num; {
    register n;

    for (n = Get_Exact_Integer (num); n > 0 && !Nullp (x); n--, x = P_Cdr (x))
	;
    return x;
}

Object P_List_Ref (x, num) Object x, num; {
    return P_Car (P_List_Tail (x, num));
}

Object Copy_List (x) Object x; {
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
