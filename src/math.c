/* math.c: Generic math functions.
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

#include <math.h>
#include <errno.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

#include "kernel.h"

extern int Bignum_To_Integer (Object);

Object Generic_Multiply(), Generic_Divide();

void Init_Math () {
#ifdef HAVE_RANDOM
    srandom (getpid ());
#else
    srand (getpid ());
#endif
}

Object Make_Integer (register int n) {
    Object num;

    SET(num, T_Fixnum, n);
    return num;
}

Object Make_Unsigned (register unsigned int n) {
    if (UFIXNUM_FITS(n))
        return Make_Integer (n);
    else
        return Unsigned_To_Bignum (n);
}

Object Make_Long (register long int n) {
    if (n < 0 ? (n < (long)INT_MIN) : (n > (long)INT_MAX))
        return Long_To_Bignum (n);
    else
        return Make_Integer ((int)n);
}

Object Make_Unsigned_Long (register unsigned long int n) {
    if ((n & ~((unsigned long int)SIGNBIT-1)) == 0)
        return Make_Integer ((int)n);
    else
        return Unsigned_Long_To_Bignum (n);
}

Object Fixnum_To_String (Object x, int radix) {
    char buf[32];
    register char *p;
    register int n = FIXNUM(x), neg = 0;

    if (n == 0)
        return Make_String ("0", 1);
    if (n < 0) {
        neg++;
        n = -n;
    }
    p = buf+31;
    *p = '\0';
    while (n > 0) {
        *--p = '0' + n % radix;
        if (*p > '9')
            *p = 'A' + (*p - '9') - 1;
        n /= radix;
    }
    if (neg)
        *--p = '-';
    return Make_String (p, strlen (p));
}

char *Flonum_To_String (Object x) {
    static char buf[64];
    char *p;

    sprintf (buf, "%.31g", FLONUM(x)->val);
    for (p = buf; *p; p++)
        if (*p == '.' || *p == 'e' || *p == 'N' || *p == 'i')
            return buf;
    *p++ = '.', *p++ = '0', *p++ = '\0';
    return buf;
}

Object P_Number_To_String (int argc, Object *argv) {
    int radix = 10;
    Object x;
    char *s;

    x = argv[0];
    if (argc == 2) {
        radix = Get_Exact_Integer (argv[1]);
        switch (radix) {
        case 2: case 8: case 10: case 16:
            break;
        default:
            Primitive_Error ("invalid radix: ~s", argv[1]);
        }
    }
    Check_Number (x);
    switch (TYPE(x)) {
    case T_Fixnum:
        return Fixnum_To_String (x, radix);
    case T_Bignum:
        return Bignum_To_String (x, radix);
    case T_Flonum:
        if (radix != 10)
            Primitive_Error ("radix for reals must be 10");   /* bleah! */
        s = Flonum_To_String (x);
        return Make_String (s, strlen (s));
    default: /* Just to avoid compiler warnings */
        return Null;
    }
    /*NOTREACHED*/
}

int Get_Integer (Object x) {
    double d;
    int expo;

    switch (TYPE(x)) {
    case T_Fixnum:
        return FIXNUM(x);
    case T_Bignum:
        return Bignum_To_Integer (x);
    case T_Flonum:
        d = FLONUM(x)->val;
        if (d != floor (d))
            Wrong_Type (x, T_Fixnum);
        (void)frexp (d, &expo);
        if (expo <= 8 * (int)sizeof(int) - 1)
            return d;
        Primitive_Error ("integer out of range: ~s", x);
    default:
        Wrong_Type (x, T_Fixnum);
    }
    /*NOTREACHED*/
}

unsigned int Get_Unsigned (Object x) {
    double d;
    int expo;

    switch (TYPE(x)) {
    case T_Fixnum:
        if (FIXNUM(x) < 0)
            goto err;
        return FIXNUM(x);
    case T_Bignum:
        return Bignum_To_Unsigned (x);
    case T_Flonum:
        d = FLONUM(x)->val;
        if (d < 0)
            goto err;
        if (d != floor (d))
            Wrong_Type (x, T_Fixnum);
        (void)frexp (d, &expo);
        if (expo <= 8 * (int)sizeof(int))
            return d;
err:
        Primitive_Error ("integer out of range: ~s", x);
    default:
        Wrong_Type (x, T_Fixnum);
    }
    /*NOTREACHED*/
}

long int Get_Long (Object x) {
    double d;
    int expo;

    switch (TYPE(x)) {
    case T_Fixnum:
        return FIXNUM(x);
    case T_Bignum:
        return Bignum_To_Long (x);
    case T_Flonum:
        d = FLONUM(x)->val;
        if (d != floor (d))
            Wrong_Type (x, T_Fixnum);
        (void)frexp (d, &expo);
        if (expo <= 8 * (int)sizeof(long) - 1)
            return d;
        Primitive_Error ("integer out of range: ~s", x);
    default:
        Wrong_Type (x, T_Fixnum);
    }
    /*NOTREACHED*/
}

unsigned long int Get_Unsigned_Long (Object x) {
    double d;
    int expo;

    switch (TYPE(x)) {
    case T_Fixnum:
        if (FIXNUM(x) < 0)
            goto err;
        return (unsigned long int)FIXNUM(x);
    case T_Bignum:
        return Bignum_To_Unsigned_Long (x);
    case T_Flonum:
        d = FLONUM(x)->val;
        if (d < 0)
            goto err;
        if (d != floor (d))
            Wrong_Type (x, T_Fixnum);
        (void)frexp (d, &expo);
        if (expo <= 8 * (int)sizeof(long))
            return d;
err:
        Primitive_Error ("integer out of range: ~s", x);
    default:
        Wrong_Type (x, T_Fixnum);
    }
    /*NOTREACHED*/
}

int Get_Exact_Integer (Object x) {
    switch (TYPE(x)) {
    case T_Fixnum:
        return FIXNUM(x);
    case T_Bignum:
        return Bignum_To_Integer (x);
    default:
        Wrong_Type (x, T_Fixnum);
    }
    /*NOTREACHED*/
}

unsigned int Get_Exact_Unsigned (Object x) {
    switch (TYPE(x)) {
    case T_Fixnum:
        if (FIXNUM(x) < 0)
            Primitive_Error ("integer out of range: ~s", x);
        return FIXNUM(x);
    case T_Bignum:
        return Bignum_To_Unsigned (x);
    default:
        Wrong_Type (x, T_Fixnum);
    }
    /*NOTREACHED*/
}

long int Get_Exact_Long (Object x) {
    switch (TYPE(x)) {
    case T_Fixnum:
        return FIXNUM(x);
    case T_Bignum:
        return Bignum_To_Long (x);
    default:
        Wrong_Type (x, T_Fixnum);
    }
    /*NOTREACHED*/
}

unsigned long int Get_Exact_Unsigned_Long (Object x) {
    switch (TYPE(x)) {
    case T_Fixnum:
        if (FIXNUM(x) < 0)
            Primitive_Error ("integer out of range: ~s", x);
        return FIXNUM(x);
    case T_Bignum:
        return Bignum_To_Unsigned_Long (x);
    default:
        Wrong_Type (x, T_Fixnum);
    }
    /*NOTREACHED*/
}

int Get_Index (Object n, Object obj) {
    register int size, i;

    i = Get_Exact_Integer (n);
    size = TYPE(obj) == T_Vector ? VECTOR(obj)->size : STRING(obj)->size;
    if (i < 0 || i >= size)
        Range_Error (n);
    return i;
}

Object Make_Flonum (double d) {
    Object num;

    num = Alloc_Object (sizeof (struct S_Flonum), T_Flonum, 0);
    FLONUM(num)->tag = Null;
    FLONUM(num)->val = d;
    return num;
}

Object Make_Reduced_Flonum (double d) {
    Object num;
    int expo;

    if (floor (d) == d) {
        if (d == 0)
            return Zero;
        (void)frexp (d, &expo);
        if (expo <= FIXBITS-1)
            return Make_Integer ((int)d);
    }
    num = Alloc_Object (sizeof (struct S_Flonum), T_Flonum, 0);
    FLONUM(num)->tag = Null;
    FLONUM(num)->val = d;
    return num;
}

int Fixnum_Add (int a, int b, int *fits) {
    int ret = a + b;

    *fits = 1;
    if (a > 0 && b > 0) {
        if (ret < 0) *fits = 0;
    } else if (a < 0 && b < 0) {
        if (ret > 0) *fits = 0;
    }
    return ret;
}

int Fixnum_Sub (int a, int b, int *fits) {
    int ret = a - b;

    *fits = 1;
    if (a < 0 && b > 0) {
        if (ret > 0) *fits = 0;
    } else if (a > 0 && b < 0) {
        if (ret < 0) *fits = 0;
    }
    return ret;
}

/* This function assumes 32bit integers.  This doesn't really matter,
 * because if the `*' primitive resorts to bignum multiplication, the
 * resulting bignum gets reduced to a fixnum (if it fits) anyway.
 * (This should be fixed, though...)
 */
Object Fixnum_Multiply (int a, int b) {
    register unsigned int aa = a;
    register unsigned int ab = b;
    register unsigned int prod, prod2;
    register int sign = 1;
    if (a < 0) {
        aa = -a;
        sign = -1;
    }
    if (b < 0) {
        ab = -b;
        sign = -sign;
    }
    prod = (aa & 0xFFFF) * (ab & 0xFFFF);
    if (aa & 0xFFFF0000) {
        if (ab & 0xFFFF0000)
            return Null;
        prod2 = (aa >> 16) * ab;
    } else {
        prod2 = aa * (ab >> 16);
    }
    prod2 += prod >> 16;
    prod &= 0xFFFF;
    if (prod2 > (1 << (FIXBITS - 1 - 16)) - 1) {
        if (sign == 1 || prod2 != (1 << (FIXBITS - 1 - 16)) || prod != 0)
            return Null;
        return Make_Integer (-(unsigned int)SIGNBIT);
    }
    prod += prod2 << 16;
    if (sign == -1)
        prod = - prod;
    return Make_Integer (prod);
}

Object P_Integerp (Object x) {
    double d;

    switch (TYPE(x)) {
    case T_Fixnum: case T_Bignum:
        return True;
    case T_Flonum:
        d = FLONUM(x)->val;
        return d == floor(d) ? True : False;
    }
    return False;
}

Object P_Rationalp (Object x) {
    return P_Integerp (x);
}

Object P_Realp (Object x) {
    register int t = TYPE(x);
    return t == T_Flonum || t == T_Fixnum  || t == T_Bignum ? True : False;
}

Object P_Complexp (Object x) {
    return P_Realp (x);
}

Object P_Numberp (Object x) {
    return P_Complexp (x);
}

Object P_Exactp (Object n) {
    Check_Number (n);
    return TYPE(n) == T_Flonum ? False : True;
}

Object P_Inexactp (Object n) {
    Check_Number (n);
    return TYPE(n) == T_Flonum ? True : False;
}

Object P_Exact_To_Inexact (Object n) {
    Check_Number (n);
    switch (TYPE(n)) {
    case T_Fixnum:
        return Make_Flonum ((double)FIXNUM(n));
    case T_Flonum:
        return n;
    case T_Bignum:
        return Make_Flonum (Bignum_To_Double (n));
    default: /* Just to avoid compiler warnings */
        return Null;
    }
    /*NOTREACHED*/
}

Object P_Inexact_To_Exact (Object n) {
    double d;
    int i;

    Check_Number (n);
    switch (TYPE(n)) {
    case T_Fixnum:
    case T_Bignum:
        return n;
    case T_Flonum:
        d = floor (FLONUM(n)->val + 0.5);
        (void)frexp (d, &i);
        return (i <= FIXBITS-1) ? Make_Integer ((int)d) : Double_To_Bignum (d);
    default: /* Just to avoid compiler warnings */
        return Null;
    }
    /*NOTREACHED*/
}

#define General_Generic_Predicate(prim,op,bigop)\
Object prim (Object x) {\
    register int ret;\
    Check_Number (x);\
    switch (TYPE(x)) {\
    case T_Flonum:\
        ret = FLONUM(x)->val op 0; break;\
    case T_Fixnum:\
        ret = FIXNUM(x) op 0; break;\
    case T_Bignum:\
        ret = bigop (x); break;\
    default: /* Just to avoid compiler warnings */\
        return False;\
    }\
    return ret ? True : False;\
}

General_Generic_Predicate (P_Zerop, ==, Bignum_Zero)
General_Generic_Predicate (P_Negativep, <, Bignum_Negative)
General_Generic_Predicate (P_Positivep, >, Bignum_Positive)

Object P_Evenp (Object x) {
    register int ret;
    double d;

    switch (TYPE(x)) {
    case T_Fixnum:
        ret = !(FIXNUM(x) & 1); break;
    case T_Bignum:
        ret = Bignum_Even (x); break;
    case T_Flonum:
        d = FLONUM(x)->val;
        if (floor (d) == d) {
            d /= 2;
            ret = floor (d) == d;
            break;
        }
        /*FALLTHROUGH*/
    default:
        Wrong_Type (x, T_Fixnum);
        /*NOTREACHED*/
    }
    return ret ? True : False;
}

Object P_Oddp (Object x) {
    Object tmp;
    tmp = P_Evenp (x);
    return EQ(tmp,True) ? False : True;
}

#define General_Generic_Compare(name,op)\
int Generic_##name (Object x, Object y) {\
    Object b; register int ret;\
    GC_Node;\
    \
    switch (TYPE(x)) {\
    case T_Fixnum:\
        switch (TYPE(y)) {\
        case T_Fixnum:\
            return FIXNUM(x) op FIXNUM(y);\
        case T_Flonum:\
            return FIXNUM(x) op FLONUM(y)->val;\
        case T_Bignum:\
            GC_Link (y);\
            b = Integer_To_Bignum (FIXNUM(x));\
            ret = Bignum_##name (b, y);\
            GC_Unlink;\
            return ret;\
        default: /* Just to avoid compiler warnings */\
            return 0;\
        }\
    case T_Flonum:\
        switch (TYPE(y)) {\
        case T_Fixnum:\
            return FLONUM(x)->val op FIXNUM(y);\
        case T_Flonum:\
            return FLONUM(x)->val op FLONUM(y)->val;\
        case T_Bignum:\
            GC_Link(y);\
            b = Double_To_Bignum(FLONUM(x)->val);\
            ret = Bignum_##name (b, y);\
            GC_Unlink;\
            return ret;\
            /*return FLONUM(x)->val op Bignum_To_Double (y);*/\
        default: /* Just to avoid compiler warnings */\
            return 0;\
        }\
    case T_Bignum:\
        switch (TYPE(y)) {\
        case T_Fixnum:\
            GC_Link (x);\
            b = Integer_To_Bignum (FIXNUM(y));\
            ret = Bignum_##name (x, b);\
            GC_Unlink;\
            return ret;\
        case T_Flonum:\
            GC_Link(x);\
            b = Double_To_Bignum(FLONUM(y)->val);\
            ret = Bignum_##name (x, b);\
            GC_Unlink;\
            return ret;\
            /*return Bignum_To_Double (x) op FLONUM(y)->val;*/\
        case T_Bignum:\
            return Bignum_##name (x, y);\
        default: /* Just to avoid compiler warnings */\
            return 0;\
        }\
    default: /* Just to avoid compiler warnings */\
        return 0;\
    }\
    /*NOTREACHED*/ /* ...but lint never sees it */\
}

General_Generic_Compare (Equal, ==)
General_Generic_Compare (Less, <)
General_Generic_Compare (Greater, >)
General_Generic_Compare (Eq_Less, <=)
General_Generic_Compare (Eq_Greater, >=)

Object General_Compare (int argc, Object *argv, register int (*op)()) {
    register int i;

    Check_Number (argv[0]);
    for (i = 1; i < argc; i++) {
        Check_Number (argv[i]);
        if (!(*op) (argv[i-1], argv[i]))
            return False;
    }
    return True;
}

Object P_Generic_Equal (int argc, Object *argv) {
    return General_Compare (argc, argv, Generic_Equal);
}

Object P_Generic_Less (int argc, Object *argv) {
    return General_Compare (argc, argv, Generic_Less);
}

Object P_Generic_Greater (int argc, Object *argv) {
    return General_Compare (argc, argv, Generic_Greater);
}

Object P_Generic_Eq_Less (int argc, Object *argv) {
    return General_Compare (argc, argv, Generic_Eq_Less);
}

Object P_Generic_Eq_Greater (int argc, Object *argv) {
    return General_Compare (argc, argv, Generic_Eq_Greater);
}

#define General_Generic_Operator(name,op,fixop,bigop) Object name (Object x,\
        Object y) {\
    Object b1, b2, ret; register int i;\
    int fits;\
    GC_Node2;\
    \
    switch (TYPE(x)) {\
    case T_Fixnum:\
        switch (TYPE(y)) {\
        case T_Fixnum:\
            i = fixop (FIXNUM(x), FIXNUM(y), &fits);\
            if (fits)\
                return Make_Integer (i);\
            b1 = b2 = Null;\
            GC_Link2 (b1, b2);\
            b1 = Integer_To_Bignum (FIXNUM(x));\
            b2 = Integer_To_Bignum (FIXNUM(y));\
            ret = bigop (b1, b2);\
            GC_Unlink;\
            return ret;\
        case T_Flonum:\
            return Make_Flonum (FIXNUM(x) op FLONUM(y)->val);\
        case T_Bignum:\
            GC_Link (y);\
            b1 = Integer_To_Bignum (FIXNUM(x));\
            ret = bigop (b1, y);\
            GC_Unlink;\
            return ret;\
        default: /* Just to avoid compiler warnings */\
            return False;\
        }\
    case T_Flonum:\
        switch (TYPE(y)) {\
        case T_Fixnum:\
            return Make_Flonum (FLONUM(x)->val op FIXNUM(y));\
        case T_Flonum:\
            return Make_Flonum (FLONUM(x)->val op FLONUM(y)->val);\
        case T_Bignum:\
            return Make_Flonum (FLONUM(x)->val op Bignum_To_Double (y));\
        default: /* Just to avoid compiler warnings */\
            return False;\
        }\
    case T_Bignum:\
        switch (TYPE(y)) {\
        case T_Fixnum:\
            GC_Link (x);\
            b1 = Integer_To_Bignum (FIXNUM(y));\
            ret = bigop (x, b1);\
            GC_Unlink;\
            return ret;\
        case T_Flonum:\
            return Make_Flonum (Bignum_To_Double (x) op FLONUM(y)->val);\
        case T_Bignum:\
            return bigop (x, y);\
        default: /* Just to avoid compiler warnings */\
            return False;\
        }\
    default: /* Just to avoid compiler warnings */\
        return False;\
    }\
    /*NOTREACHED*/ /* ...but lint never sees it */\
}

General_Generic_Operator (Generic_Plus,  +, Fixnum_Add, Bignum_Plus)
General_Generic_Operator (Generic_Minus, -, Fixnum_Sub, Bignum_Minus)

Object P_Inc (Object x) {
    Check_Number (x);
    return Generic_Plus (x, One);
}

Object P_Dec (Object x) {
    Check_Number (x);
    return Generic_Minus (x, One);
}

Object General_Operator (int argc, Object *argv, Object start,
        register Object (*op)()) {
    register int i;
    Object accum;

    if (argc > 0)
        Check_Number (argv[0]);
    accum = start;
    switch (argc) {
    case 0:
        break;
    case 1:
        accum = (*op) (accum, argv[0]); break;
    default:
        for (accum = argv[0], i = 1; i < argc; i++) {
            Check_Number (argv[i]);
            accum = (*op) (accum, argv[i]);
        }
    }
    return accum;
}

Object P_Generic_Plus (int argc, Object *argv) {
    return General_Operator (argc, argv, Zero, Generic_Plus);
}

Object P_Generic_Minus (int argc, Object *argv) {
    return General_Operator (argc, argv, Zero, Generic_Minus);
}

Object P_Generic_Multiply (int argc, Object *argv) {
    return General_Operator (argc, argv, One, Generic_Multiply);
}

Object P_Generic_Divide (int argc, Object *argv) {
    return General_Operator (argc, argv, One, Generic_Divide);
}

Object Generic_Multiply (Object x, Object y) {
    Object b, ret;

    switch (TYPE(x)) {
    case T_Fixnum:
        switch (TYPE(y)) {
        case T_Fixnum:
            ret = Fixnum_Multiply (FIXNUM(x), FIXNUM(y));
            if (Nullp (ret)) {
                b = Integer_To_Bignum (FIXNUM(x));
                return Bignum_Fixnum_Multiply (b, y);
            }
            return ret;
        case T_Flonum:
            return Make_Flonum (FIXNUM(x) * FLONUM(y)->val);
        case T_Bignum:
            return Bignum_Fixnum_Multiply (y, x);
        default: /* Just to avoid compiler warnings */
            return Null;
        }
    case T_Flonum:
        switch (TYPE(y)) {
        case T_Fixnum:
            return Make_Flonum (FLONUM(x)->val * FIXNUM(y));
        case T_Flonum:
            return Make_Flonum (FLONUM(x)->val * FLONUM(y)->val);
        case T_Bignum:
            return Make_Flonum (FLONUM(x)->val * Bignum_To_Double (y));
        default: /* Just to avoid compiler warnings */
            return Null;
        }
    case T_Bignum:
        switch (TYPE(y)) {
        case T_Fixnum:
            return Bignum_Fixnum_Multiply (x, y);
        case T_Flonum:
            return Make_Flonum (Bignum_To_Double (x) * FLONUM(y)->val);
        case T_Bignum:
            return Bignum_Multiply (x, y);
        default: /* Just to avoid compiler warnings */
            return Null;
        }
    default: /* Just to avoid compiler warnings */
        return Null;
    }
    /*NOTREACHED*/
}

Object Generic_Divide (Object x, Object y) {
    register int t = TYPE(y);
    Object b, ret;
    GC_Node2;

    if (t == T_Fixnum ? FIXNUM(y) == 0 :
        (t == T_Flonum ? FLONUM(y) == 0 : Bignum_Zero (y)))
        Range_Error (y);
    switch (TYPE(x)) {
    case T_Fixnum:
        switch (t) {
        case T_Fixnum:
            return Make_Reduced_Flonum ((double)FIXNUM(x) / (double)FIXNUM(y));
        case T_Flonum:
            return Make_Flonum ((double)FIXNUM(x) / FLONUM(y)->val);
        case T_Bignum:
            GC_Link (y);
            b = Integer_To_Bignum (FIXNUM(x));
            ret = Bignum_Divide (b, y);
            GC_Unlink;
            if (EQ(Cdr (ret),Zero))
                return Car (ret);
            return Make_Reduced_Flonum ((double)FIXNUM(x)
                / Bignum_To_Double (y));
        default: /* Just to avoid compiler warnings */
            return Null;
        }
    case T_Flonum:
        switch (t) {
        case T_Fixnum:
            return Make_Flonum (FLONUM(x)->val / (double)FIXNUM(y));
        case T_Flonum:
            return Make_Flonum (FLONUM(x)->val / FLONUM(y)->val);
        case T_Bignum:
            return Make_Flonum (FLONUM(x)->val / Bignum_To_Double (y));
        default: /* Just to avoid compiler warnings */
            return Null;
        }
    case T_Bignum:
        switch (t) {
        case T_Fixnum:
            GC_Link (x);
            ret = Bignum_Fixnum_Divide (x, y);
            GC_Unlink;
            if (EQ(Cdr (ret),Zero))
                return Car (ret);
            return Make_Reduced_Flonum (Bignum_To_Double (x)
                / (double)FIXNUM(y));
        case T_Flonum:
            return Make_Flonum (Bignum_To_Double (x) / FLONUM(y)->val);
        case T_Bignum:
            GC_Link2 (x, y);
            ret = Bignum_Divide (x, y);
            GC_Unlink;
            if (EQ(Cdr (ret),Zero))
                return Car (ret);
            return Make_Reduced_Flonum (Bignum_To_Double (x)
                / Bignum_To_Double (y));
        default: /* Just to avoid compiler warnings */
            return Null;
        }
    default: /* Just to avoid compiler warnings */
        return Null;
    }
    /*NOTREACHED*/
}

Object P_Abs (Object x) {
    register int i;

    Check_Number (x);
    switch (TYPE(x)) {
    case T_Fixnum:
        i = FIXNUM(x);
        return i < 0 ? Make_Integer (-i) : x;
    case T_Flonum:
        return Make_Flonum (fabs (FLONUM(x)->val));
    case T_Bignum:
        return Bignum_Abs (x);
    default: /* Just to avoid compiler warnings */
        return Null;
    }
    /*NOTREACHED*/
}

Object General_Integer_Divide (Object x, Object y, int rem) {
    register int fx = FIXNUM(x), fy = FIXNUM(y);
    Object b, ret;
    GC_Node;

    if (TYPE(y) == T_Fixnum ? FIXNUM(y) == 0 : Bignum_Zero (y))
        Range_Error (y);
    switch (TYPE(x)) {
    case T_Fixnum:
        switch (TYPE(y)) {
        case T_Fixnum:
            return Make_Integer (rem ? (fx % fy) : (fx / fy));
        case T_Bignum:
            GC_Link (y);
            b = Integer_To_Bignum (fx);
            GC_Unlink;
            ret = Bignum_Divide (b, y);
done:
            return rem ? Cdr (ret) : Car (ret);
        default: /* Just to avoid compiler warnings */
            return Null;
        }
    case T_Bignum:
        switch (TYPE(y)) {
        case T_Fixnum:
            ret = Bignum_Fixnum_Divide (x, y);
            goto done;
        case T_Bignum:
            ret = Bignum_Divide (x, y);
            goto done;
        default: /* Just to avoid compiler warnings */
            return Null;
        }
    default: /* Just to avoid compiler warnings */
        return Null;
    }
    /*NOTREACHED*/
}

Object Exact_Quotient (Object x, Object y) {
    return General_Integer_Divide (x, y, 0);
}

Object Exact_Remainder (Object x, Object y) {
    return General_Integer_Divide (x, y, 1);
}

Object Exact_Modulo (Object x, Object y) {
    Object rem, xneg, yneg;
    GC_Node2;

    GC_Link2 (x, y);
    rem = General_Integer_Divide (x, y, 1);
    if (!EQ(rem,Zero)) {
        xneg = P_Negativep (x);
        yneg = P_Negativep (y);
        if (!EQ(xneg,yneg))
            rem = Generic_Plus (rem, y);
    }
    GC_Unlink;
    return rem;
}

Object With_Exact_Ints (Object x, Object y, Object (*fun)()) {
    Object i, ret;
    int inex = 0;
    GC_Node3;

    ret = Null;
    GC_Link3 (x, y, ret);
    i = P_Integerp (x);
    if (!EQ(i,True))
        Wrong_Type (x, T_Fixnum);
    i = P_Integerp (y);
    if (!EQ(i,True))
        Wrong_Type (y, T_Fixnum);
    if (TYPE(x) == T_Flonum) {
        x = P_Inexact_To_Exact (x); inex++;
    }
    if (TYPE(y) == T_Flonum) {
        y = P_Inexact_To_Exact (y); inex++;
    }
    ret = fun (x, y);
    if (inex)
        ret = P_Exact_To_Inexact (ret);
    GC_Unlink;
    return ret;
}

Object P_Quotient (Object x, Object y) {
    return With_Exact_Ints (x, y, Exact_Quotient);
}

Object P_Remainder (Object x, Object y) {
    return With_Exact_Ints (x, y, Exact_Remainder);
}

Object P_Modulo (Object x, Object y) {
    return With_Exact_Ints (x, y, Exact_Modulo);
}

Object Exact_Gcd (Object x, Object y) {
    Object r, z;
    GC_Node2;

    GC_Link2 (x, y);
    while (1) {
        z = P_Zerop (x);
        if (EQ(z,True)) {
            r = y;
            break;
        }
        z = P_Zerop (y);
        if (EQ(z,True)) {
            r = x;
            break;
        }
        r = General_Integer_Divide (x, y, 1);
        x = y;
        y = r;
    }
    GC_Unlink;
    return r;
}

Object General_Gcd (Object x, Object y) {
    return With_Exact_Ints (x, y, Exact_Gcd);
}

Object P_Gcd (int argc, Object *argv) {
    return P_Abs (General_Operator (argc, argv, Zero, General_Gcd));
}

Object Exact_Lcm (Object x, Object y) {
    Object ret, p, z;
    GC_Node3;

    ret = Null;
    GC_Link3 (x, y, ret);
    ret = Exact_Gcd (x, y);
    z = P_Zerop (ret);
    if (!EQ(z,True)) {
        p = Generic_Multiply (x, y);
        ret = General_Integer_Divide (p, ret, 0);
    }
    GC_Unlink;
    return ret;
}

Object General_Lcm (Object x, Object y) {
    return With_Exact_Ints (x, y, Exact_Lcm);
}

Object P_Lcm (int argc, Object *argv) {
    return P_Abs (General_Operator (argc, argv, One, General_Lcm));
}

#define General_Conversion(name,op) Object name (Object x) {\
    double d, i;\
\
    Check_Number (x);\
    if (TYPE(x) != T_Flonum)\
        return x;\
    d = FLONUM(x)->val;\
    (void)modf (op (d), &i);\
    return Make_Flonum (i);\
}

#define trunc(x) (x)

General_Conversion (P_Floor, floor)
General_Conversion (P_Ceiling, ceil)
General_Conversion (P_Truncate, trunc)

Object P_Round (Object x) {
    double d, y, f;
    Object ret, isodd;

    Check_Number (x);
    if (TYPE(x) != T_Flonum)
        return x;
    d = FLONUM(x)->val;
    y = d + 0.5;
    f = floor (y);
    ret = Make_Flonum (f);
    if (y == f) {
        isodd = P_Oddp (ret);
        if (Truep (isodd))
            FLONUM(ret)->val--;
    }
    return ret;
}

double Get_Double (Object x) {
    Check_Number (x);
    switch (TYPE(x)) {
    case T_Fixnum:
        return (double)FIXNUM(x);
    case T_Flonum:
        return FLONUM(x)->val;
    case T_Bignum:
        return Bignum_To_Double (x);
    default: /* Just to avoid compiler warnings */
        return 0.0;
    }
    /*NOTREACHED*/
}

Object General_Function (Object x, Object y, double (*fun)()) {
    double d, ret;

    d = Get_Double (x);
    errno = 0;
    if (Nullp (y))
        ret = (*fun) (d);
    else
        ret = (*fun) (d, Get_Double (y));
    if (errno == ERANGE || errno == EDOM)
        Range_Error (x);
    return Make_Flonum (ret);
}

Object P_Sqrt (Object x) { return General_Function (x, Null, sqrt); }

Object P_Exp (Object x) { return General_Function (x, Null, exp); }

Object P_Pow (Object x, Object y) { return General_Function (x, y, pow); }

Object P_Log (Object x) { return General_Function (x, Null, log); }

Object P_Sin (Object x) { return General_Function (x, Null, sin); }

Object P_Cos (Object x) { return General_Function (x, Null, cos); }

Object P_Tan (Object x) { return General_Function (x, Null, tan); }

Object P_Asin (Object x) { return General_Function (x, Null, asin); }

Object P_Acos (Object x) { return General_Function (x, Null, acos); }

Object P_Atan (int argc, Object *argv) {
    register int a2 = argc == 2;
    return General_Function (argv[0], a2 ? argv[1] : Null, a2 ?
        (double(*)())atan2 : (double(*)())atan);
}

Object Min (Object x, Object y) {
    Object ret;

    ret = Generic_Less (x, y) ? x : y;
    if (TYPE(x) == T_Flonum || TYPE(y) == T_Flonum)
        ret = P_Exact_To_Inexact (ret);
    return ret;
}

Object Max (Object x, Object y) {
    Object ret;

    ret = Generic_Less (x, y) ? y : x;
    if (TYPE(x) == T_Flonum || TYPE(y) == T_Flonum)
        ret = P_Exact_To_Inexact (ret);
    return ret;
}

Object P_Min (int argc, Object *argv) {
    return General_Operator (argc, argv, argv[0], Min);
}

Object P_Max (int argc, Object *argv) {
    return General_Operator (argc, argv, argv[0], Max);
}

Object P_Random () {
#ifdef HAVE_RANDOM
    return Make_Long (random ());
#else
    return Make_Integer (rand ());
#endif
}

Object P_Srandom (Object x) {
#ifdef HAVE_RANDOM
    srandom (Get_Unsigned (x));
#else
    srand (Get_Unsigned (x));
#endif
    return x;
}
