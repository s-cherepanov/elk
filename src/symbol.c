/* symbol.c
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

#include <ctype.h>
#include <string.h>

#include "kernel.h"

int Hash (char const *, int);

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

void Init_Symbol () {
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

Object Make_Symbol (Object name) {
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

Object P_Symbolp (Object x) {
    return TYPE(x) == T_Symbol ? True : False;
}

Object P_Symbol_To_String (Object x) {
    Check_Type (x, T_Symbol);
    return SYMBOL(x)->name;
}

Object Obarray_Lookup (register char const *str, register int len) {
    register int h;
    register struct S_String *s;
    register struct S_Symbol *sym;
    Object p;

    h = Hash (str, len) % OBARRAY_SIZE;
    for (p = VECTOR(Obarray)->data[h]; !Nullp (p); p = sym->next) {
        sym = SYMBOL(p);
        s = STRING(sym->name);
        if (s->size == len && memcmp (s->data, str, len) == 0)
            return p;
    }
    return Make_Integer (h);
}

Object CI_Intern (char const *str) {
    Object s, *p, sym, ostr;
    register int len;
    register char const *src;
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

Object Intern (char const *str) {
    Object s, *p, sym, ostr;
    register int len;

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

Object P_String_To_Symbol (Object str) {
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
    register int i;
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

Object P_Put (int argc, Object *argv) {
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

Object P_Get (Object sym, Object key) {
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

Object P_Symbol_Plist (Object sym) {
    Check_Type (sym, T_Symbol);
    return Copy_List (SYMBOL(sym)->plist);
}

int Hash (char const *str, int len) {
    register int h;
    register char const *p, *ep;

    h = 5 * len;
    if (len > 5)
        len = 5;
    for (p = str, ep = p+len; p < ep; ++p)
        h = (h << 2) ^ *p;
    return h & 017777777777;
}

void Define_Symbol (Object *sym, char const *name) {
    *sym = Intern (name);
    Func_Global_GC_Link (sym);
}

void Define_Variable (Object *var, char const *name, Object init) {
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

Object Var_Get (Object var) {
    return Cdr (var);
}

void Var_Set (Object var, Object val) {
    Cdr (var) = val;
    SYMBOL (Car (var))->value = val;
}

int Var_Is_True (Object var) {
    var = Var_Get (var);
    return Truep (var);
}

unsigned long int Symbols_To_Bits (Object x, int mflag, SYMDESCR *stab) {
    register SYMDESCR *syms;
    register unsigned long int mask = 0;
    Object l, s;
    register char *p;
    register int n;

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

Object Bits_To_Symbols (unsigned long int x, int mflag, SYMDESCR *stab) {
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
