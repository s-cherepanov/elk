#include <ctype.h>

#include "kernel.h"

Object Obarray;

Object Null,
       True,
       False,
       False2,
       Unbound,
       Special,
       Void,
       Newline,
       Eof,
       Zero,
       One;

Init_Symbol () {
    SET(Null, T_Null, 0);
    SET(True, T_Boolean, 1);
    SET(False, T_Boolean, 0);
    False2 = False;
    SET(Unbound, T_Unbound, 0);
    SET(Special, T_Special, 0);
    SET(Eof, T_End_Of_File, 0);
    Newline = Make_Char ('\n');
    Zero = Make_Integer (0);
    One = Make_Integer (1);
    Obarray = Make_Vector (OBARRAY_SIZE, Null);
    Global_GC_Link (Obarray);
    Define_Symbol (&Void, "");
}

Object Make_Symbol (name) Object name; {
    Object sym;
    register struct S_Symbol *sp;
    GC_Node;

    GC_Link (name);
    sym = Alloc_Object (sizeof (struct S_Symbol), T_Symbol, 0);
    sp = SYMBOL(sym);
    sp->name = name;
    sp->value = Unbound;
    sp->plist = Null;
    GC_Unlink;
    return sym;
}

Object P_Symbolp (x) Object x; {
    return TYPE(x) == T_Symbol ? True : False;
}

Object P_Symbol_To_String (x) Object x; {
    Check_Type (x, T_Symbol);
    return SYMBOL(x)->name;
}

Object Obarray_Lookup (str, len) register char *str; register len; {
    register h;
    register struct S_String *s;
    register struct S_Symbol *sym;
    Object p;

    h = Hash (str, len) % OBARRAY_SIZE;
    for (p = VECTOR(Obarray)->data[h]; !Nullp (p); p = sym->next) {
	sym = SYMBOL(p);
	s = STRING(sym->name);
	if (s->size == len && bcmp (s->data, str, len) == 0)
	    return p;
    }
    return Make_Integer (h);
}

Object CI_Intern (str) const char *str; {
    Object s, *p, sym, ostr;
    register len;
    register const char *src;
    char *dst;
    char buf[128];
    Alloca_Begin;

    len = strlen (str);
    if (len > sizeof (buf)) {
	Alloca (dst, char*, len);
    } else 
	dst = buf;
    src = str;
    str = dst;
    for ( ; *src; src++, dst++)
	*dst = isupper (*src) ? tolower (*src) : *src;
    s = Obarray_Lookup (str, len);
    if (TYPE(s) != T_Fixnum) {
	Alloca_End;
	return s;
    }
    ostr = Make_Const_String (str, len);
    sym = Make_Symbol (ostr);
    p = &VECTOR(Obarray)->data[FIXNUM(s)];
    SYMBOL(sym)->next = *p;
    Alloca_End;
    *p = sym;
    return sym;
}

Object Intern (str) const char *str; {
    Object s, *p, sym, ostr;
    register len;

    if (Case_Insensitive)
	return CI_Intern (str);
    len = strlen (str);
    s = Obarray_Lookup (str, len);
    if (TYPE(s) != T_Fixnum)
	return s;
    ostr = Make_Const_String (str, len);
    sym = Make_Symbol (ostr);
    p = &VECTOR(Obarray)->data[FIXNUM(s)];
    SYMBOL(sym)->next = *p;
    *p = sym;
    return sym;
}

Object P_String_To_Symbol (str) Object str; {
    Object s, *p, sym;

    Check_Type (str, T_String);
    s = Obarray_Lookup (STRING(str)->data, STRING(str)->size);
    if (TYPE(s) != T_Fixnum)
	return s;
    str = Make_String (STRING(str)->data, STRING(str)->size);
    sym = Make_Symbol (str);
    p = &VECTOR(Obarray)->data[FIXNUM(s)];
    SYMBOL(sym)->next = *p;
    *p = sym;
    return sym;
}

Object P_Oblist () {
    register i;
    Object p, list, bucket;
    GC_Node2;

    p = list = Null;
    GC_Link2 (p, list);
    for (i = 0; i < OBARRAY_SIZE; i++) {
	bucket = Null;
	for (p = VECTOR(Obarray)->data[i]; !Nullp (p); p = SYMBOL(p)->next)
	    bucket = Cons (p, bucket);
	if (!Nullp (bucket))
	    list = Cons (bucket, list);
    }
    GC_Unlink;
    return list;
}

Object P_Put (argc, argv) Object *argv; {
    Object sym, key, last, tail, prop;
    GC_Node3;

    sym = argv[0];
    key = argv[1];
    Check_Type (sym, T_Symbol);
    Check_Type (key, T_Symbol);
    last = Null;
    for (tail = SYMBOL(sym)->plist; !Nullp (tail); tail = Cdr (tail)) {
	prop = Car (tail);
	if (EQ(Car (prop), key)) {
	    if (argc == 3)
		Cdr (prop) = argv[2];
	    else if (Nullp (last))
		SYMBOL(sym)->plist = Cdr (tail);
	    else
		Cdr (last) = Cdr (tail);
	    return key;
	}
	last = tail;
    }
    if (argc == 2)
	return False;
    GC_Link3 (sym, last, key);
    tail = Cons (key, argv[2]);
    tail = Cons (tail, Null);
    if (Nullp (last))
	SYMBOL(sym)->plist = tail;
    else
	Cdr (last) = tail;
    GC_Unlink;
    return key;
}

Object P_Get (sym, key) Object sym, key; {
    Object prop;

    Check_Type (sym, T_Symbol);
    Check_Type (key, T_Symbol);
    prop = Assq (key, SYMBOL(sym)->plist);
    if (!Truep (prop))
	return False;
	/*
	 * Do we want to signal an error or return #f?
	 *
	 * Primitive_Error ("~s has no such property: ~s", sym, key);
	 */
    return Cdr (prop);
}

Object P_Symbol_Plist (sym) Object sym; {
    Check_Type (sym, T_Symbol);
    return Copy_List (SYMBOL(sym)->plist);
}

Hash (str, len) char *str; {
    register h;
    register char *p, *ep;

    h = 5 * len;
    if (len > 5)
	len = 5;
    for (p = str, ep = p+len; p < ep; ++p)
	h = (h << 2) ^ *p;
    return h & 017777777777;
}

void Define_Symbol (sym, name) Object *sym; const char *name; {
    *sym = Intern (name);
    Func_Global_GC_Link (sym);
}

void Define_Variable (var, name, init) Object *var, init; const char *name; {
    Object frame, sym;
    GC_Node;

    GC_Link (init);
    sym = Intern (name);
    SYMBOL(sym)->value = init;
    frame = Add_Binding (Car (The_Environment), sym, init);
    *var = Car (frame);
    Car (The_Environment) = frame;
    Func_Global_GC_Link (var);
    GC_Unlink;
}

Object Var_Get (var) Object var; {
    return Cdr (var);
}

void Var_Set (var, val) Object var, val; {
    Cdr (var) = val;
    SYMBOL (Car (var))->value = val;
}

int Var_Is_True (var) Object var; {
    var = Var_Get (var);
    return Truep (var);
}

unsigned long Symbols_To_Bits (x, mflag, stab) Object x; SYMDESCR *stab; {
    register SYMDESCR *syms;
    register unsigned long mask = 0;
    Object l, s;
    register char *p;
    register n;

    if (!mflag) Check_Type (x, T_Symbol);
    for (l = x; !Nullp (l); l = Cdr (l)) {
	if (mflag) {
	    Check_Type (l, T_Pair);
	    x = Car (l);
	}
	Check_Type (x, T_Symbol);
	s = SYMBOL(x)->name;
	p = STRING(s)->data;
	n = STRING(s)->size;
	for (syms = stab; syms->name; syms++)
	    if (n && strncmp (syms->name, p, n) == 0) break;
	if (syms->name == 0)
	    Primitive_Error ("invalid argument: ~s", x);
	mask |= syms->val;
	if (!mflag) break;
    }
    return mask;
}

Object Bits_To_Symbols (x, mflag, stab) unsigned long x; SYMDESCR *stab; {
    register SYMDESCR *syms;
    Object list, tail, cell;
    GC_Node2;

    if (mflag) {
	GC_Link2 (list, tail);
	for (list = tail = Null, syms = stab; syms->name; syms++)
	    if ((x & syms->val) && syms->val != ~0) {
		Object z;
		
		z = Intern (syms->name);
		cell = Cons (z, Null);
		if (Nullp (list))
		    list = cell;
		else
		    P_Set_Cdr (tail, cell);
		tail = cell;
	    }
	GC_Unlink;
	return list;
    }
    for (syms = stab; syms->name; syms++)
	if (syms->val == x)
	    return Intern (syms->name);
    return Null;
}
