/* Generic math functions.
 */

#include <math.h>
#include <errno.h>
#include <limits.h>

#include "kernel.h"

extern int errno;

Object Generic_Multiply(), Generic_Divide();

Init_Math () {
#ifdef RANDOM
    srandom (getpid ());
#else
    srand (getpid ());
#endif
}

Object Make_Integer (n) register n; {
    Object num;

    SET(num, T_Fixnum, n);
    return num;
}

Object Make_Unsigned (n) register unsigned n; {
    if (UFIXNUM_FITS(n))
	return Make_Integer (n);
    else
	return Unsigned_To_Bignum (n);
}

Object Make_Long (n) register long n; {
    if (n < 0 ? (n < (long)INT_MIN) : (n > (long)INT_MAX))
	return Long_To_Bignum (n);
    else
	return Make_Integer ((int)n);
}

Object Make_Unsigned_Long (n) register unsigned long n; {
    if ((n & ~((unsigned long)SIGNBIT-1)) == 0)
	return Make_Integer ((int)n);
    else
	return Unsigned_Long_To_Bignum (n);
}

Object Fixnum_To_String (x, radix) Object x; {
    char buf[32];
    register char *p;
    register n = FIXNUM(x), neg = 0;

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

char *Flonum_To_String (x) Object x; {
    static char buf[32];
    char *p;

    sprintf (buf, "%.15g", FLONUM(x)->val);
    for (p = buf; *p; p++)
	if (*p == '.' || *p == 'e' || *p == 'N' || *p == 'i')
	    return buf;
    *p++ = '.', *p++ = '0', *p++ = '\0';
    return buf;
}

Object P_Number_To_String (argc, argv) Object *argv; {
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
    }
    /*NOTREACHED*/
}

Get_Integer (x) Object x; {
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
	if (expo <= 8 * sizeof(int) - 1)
	    return d;
	Primitive_Error ("integer out of range: ~s", x);
    default:
	Wrong_Type (x, T_Fixnum);
    }
    /*NOTREACHED*/
}

unsigned Get_Unsigned (x) Object x; {
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
	if (expo <= 8 * sizeof(int))
	    return d;
err:
	Primitive_Error ("integer out of range: ~s", x);
    default:
	Wrong_Type (x, T_Fixnum);
    }
    /*NOTREACHED*/
}

long Get_Long (x) Object x; {
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
	if (expo <= 8 * sizeof(long) - 1)
	    return d;
	Primitive_Error ("integer out of range: ~s", x);
    default:
	Wrong_Type (x, T_Fixnum);
    }
    /*NOTREACHED*/
}

unsigned long Get_Unsigned_Long (x) Object x; {
    double d;
    int expo;

    switch (TYPE(x)) {
    case T_Fixnum:
	if (FIXNUM(x) < 0)
	    goto err;
	return (unsigned long)FIXNUM(x);
    case T_Bignum:
	return Bignum_To_Unsigned_Long (x);
    case T_Flonum:
	d = FLONUM(x)->val;
	if (d < 0)
	    goto err;
	if (d != floor (d))
	    Wrong_Type (x, T_Fixnum);
	(void)frexp (d, &expo);
	if (expo <= 8 * sizeof(long))
	    return d;
err:
	Primitive_Error ("integer out of range: ~s", x);
    default:
	Wrong_Type (x, T_Fixnum);
    }
    /*NOTREACHED*/
}

Get_Exact_Integer (x) Object x; {
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

unsigned Get_Exact_Unsigned (x) Object x; {
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

long Get_Exact_Long (x) Object x; {
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

unsigned long Get_Exact_Unsigned_Long (x) Object x; {
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

Get_Index (n, obj) Object n, obj; {
    register size, i;

    i = Get_Exact_Integer (n);
    size = TYPE(obj) == T_Vector ? VECTOR(obj)->size : STRING(obj)->size;
    if (i < 0 || i >= size)
	Range_Error (n);
    return i;
}

Object Make_Flonum (d) double d; {
    Object num;

    num = Alloc_Object (sizeof (struct S_Flonum), T_Flonum, 0);
    FLONUM(num)->tag = Null;
    FLONUM(num)->val = d;
    return num;
}

Object Make_Reduced_Flonum (d) double d; {
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

Fixnum_Add (a, b, fits) int *fits; {
    int ret = a + b;

    *fits = 1;
    if (a > 0 && b > 0) {
	if (ret < 0) *fits = 0;
    } else if (a < 0 && b < 0) {
	if (ret > 0) *fits = 0;
    }
    return ret;
}

Fixnum_Sub (a, b, fits) int *fits; {
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
Object Fixnum_Multiply (a, b) {
    register unsigned aa = a;
    register unsigned ab = b;
    register unsigned prod, prod2;
    register sign = 1;
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
	return Make_Integer (-(unsigned)SIGNBIT);
    }
    prod += prod2 << 16;
    if (sign == -1)
	prod = - prod;
    return Make_Integer (prod);
}

Object P_Integerp (x) Object x; {
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

Object P_Rationalp (x) Object x; {
    return P_Integerp (x);
}

Object P_Realp (x) Object x; {
    register t = TYPE(x);
    return t == T_Flonum || t == T_Fixnum  || t == T_Bignum ? True : False;
}

Object P_Complexp (x) Object x; {
    return P_Realp (x);
}

Object P_Numberp (x) Object x; {
    return P_Complexp (x);
}

Object P_Exactp (n) Object n; {
    Check_Number (n);
    return TYPE(n) == T_Flonum ? False : True;
}

Object P_Inexactp (n) Object n; {
    Check_Number (n);
    return TYPE(n) == T_Flonum ? True : False;
}

Object P_Exact_To_Inexact (n) Object n; {
    Check_Number (n);
    switch (TYPE(n)) {
    case T_Fixnum:
	return Make_Flonum ((double)FIXNUM(n));
    case T_Flonum:
	return n;
    case T_Bignum:
	return Make_Flonum (Bignum_To_Double (n));
    }
    /*NOTREACHED*/
}

Object P_Inexact_To_Exact (n) Object n; {
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
    }
    /*NOTREACHED*/
}

#define General_Generic_Predicate(prim,op,bigop) Object prim (x) Object x; {\
    register ret;\
    Check_Number (x);\
    switch (TYPE(x)) {\
    case T_Flonum:\
	ret = FLONUM(x)->val op 0; break;\
    case T_Fixnum:\
	ret = FIXNUM(x) op 0; break;\
    case T_Bignum:\
	ret = bigop (x); break;\
    }\
    return ret ? True : False;\
}

General_Generic_Predicate (P_Zerop, ==, Bignum_Zero)
General_Generic_Predicate (P_Negativep, <, Bignum_Negative)
General_Generic_Predicate (P_Positivep, >, Bignum_Positive)

Object P_Evenp (x) Object x; {
    register ret;
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

Object P_Oddp (x) Object x; {
    Object tmp;
    tmp = P_Evenp (x);
    return EQ(tmp,True) ? False : True;
}

#define General_Generic_Compare(name,op,bigop) name (x, y) Object x, y; {\
    Object b; register ret;\
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
	    ret = bigop (b, y);\
	    GC_Unlink;\
	    return ret;\
	}\
    case T_Flonum:\
	switch (TYPE(y)) {\
	case T_Fixnum:\
	    return FLONUM(x)->val op FIXNUM(y);\
	case T_Flonum:\
	    return FLONUM(x)->val op FLONUM(y)->val;\
	case T_Bignum:\
	    return FLONUM(x)->val op Bignum_To_Double (y);\
	}\
    case T_Bignum:\
	switch (TYPE(y)) {\
	case T_Fixnum:\
	    GC_Link (x);\
	    b = Integer_To_Bignum (FIXNUM(y));\
	    ret = bigop (x, b);\
	    GC_Unlink;\
	    return ret;\
	case T_Flonum:\
	    return Bignum_To_Double (x) op FLONUM(y)->val;\
	case T_Bignum:\
	    return bigop (x, y);\
	}\
    }\
    /*NOTREACHED*/ /* ...but lint never sees it */\
}

General_Generic_Compare (Generic_Equal,      ==, Bignum_Equal)
General_Generic_Compare (Generic_Less,        <, Bignum_Less)
General_Generic_Compare (Generic_Greater,     >, Bignum_Greater)
General_Generic_Compare (Generic_Eq_Less,    <=, Bignum_Eq_Less)
General_Generic_Compare (Generic_Eq_Greater, >=, Bignum_Eq_Greater)

Object General_Compare (argc, argv, op) Object *argv; register (*op)(); {
    register i;

    Check_Number (argv[0]);
    for (i = 1; i < argc; i++) {
	Check_Number (argv[i]);
	if (!(*op) (argv[i-1], argv[i]))
	    return False;
    }
    return True;
}

Object P_Generic_Equal (argc, argv) Object *argv; {
    return General_Compare (argc, argv, Generic_Equal);
}

Object P_Generic_Less (argc, argv) Object *argv; {
    return General_Compare (argc, argv, Generic_Less);
}

Object P_Generic_Greater (argc, argv) Object *argv; {
    return General_Compare (argc, argv, Generic_Greater);
}

Object P_Generic_Eq_Less (argc, argv) Object *argv; {
    return General_Compare (argc, argv, Generic_Eq_Less);
}

Object P_Generic_Eq_Greater (argc, argv) Object *argv; {
    return General_Compare (argc, argv, Generic_Eq_Greater);
}

#define General_Generic_Operator(name,op,fixop,bigop) Object name (x, y)\
	Object x, y; {\
    Object b1, b2, ret; register i;\
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
	}\
    case T_Flonum:\
	switch (TYPE(y)) {\
	case T_Fixnum:\
	    return Make_Flonum (FLONUM(x)->val op FIXNUM(y));\
	case T_Flonum:\
	    return Make_Flonum (FLONUM(x)->val op FLONUM(y)->val);\
	case T_Bignum:\
	    return Make_Flonum (FLONUM(x)->val op Bignum_To_Double (y));\
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
	}\
    }\
    /*NOTREACHED*/ /* ...but lint never sees it */\
}

General_Generic_Operator (Generic_Plus,  +, Fixnum_Add, Bignum_Plus)
General_Generic_Operator (Generic_Minus, -, Fixnum_Sub, Bignum_Minus)

Object P_Inc (x) Object x; {
    Check_Number (x);
    return Generic_Plus (x, One);
}

Object P_Dec (x) Object x; {
    Check_Number (x);
    return Generic_Minus (x, One);
}

Object General_Operator (argc, argv, start, op) Object *argv, start;
	register Object (*op)(); {
    register i;
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

Object P_Generic_Plus (argc, argv) Object *argv; {
    return General_Operator (argc, argv, Zero, Generic_Plus);
}

Object P_Generic_Minus (argc, argv) Object *argv; {
    return General_Operator (argc, argv, Zero, Generic_Minus);
}

Object P_Generic_Multiply (argc, argv) Object *argv; {
    return General_Operator (argc, argv, One, Generic_Multiply);
}

Object P_Generic_Divide (argc, argv) Object *argv; {
    return General_Operator (argc, argv, One, Generic_Divide);
}

Object Generic_Multiply (x, y) Object x, y; {
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
	}
    case T_Flonum:
	switch (TYPE(y)) {
	case T_Fixnum:
	    return Make_Flonum (FLONUM(x)->val * FIXNUM(y));
	case T_Flonum:
	    return Make_Flonum (FLONUM(x)->val * FLONUM(y)->val);
	case T_Bignum:
	    return Make_Flonum (FLONUM(x)->val * Bignum_To_Double (y));
	}
    case T_Bignum:
	switch (TYPE(y)) {
	case T_Fixnum:
	    return Bignum_Fixnum_Multiply (x, y);
	case T_Flonum:
	    return Make_Flonum (Bignum_To_Double (x) * FLONUM(y)->val);
	case T_Bignum:
	    return Bignum_Multiply (x, y);
	}
    }
    /*NOTREACHED*/
}

Object Generic_Divide (x, y) Object x, y; {
    register t = TYPE(y);
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
	}
    case T_Flonum:
	switch (t) {
	case T_Fixnum:
	    return Make_Flonum (FLONUM(x)->val / (double)FIXNUM(y));
	case T_Flonum:
	    return Make_Flonum (FLONUM(x)->val / FLONUM(y)->val);
	case T_Bignum:
	    return Make_Flonum (FLONUM(x)->val / Bignum_To_Double (y));
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
	}
    }
    /*NOTREACHED*/
}

Object P_Abs (x) Object x; {
    register i;

    Check_Number (x);
    switch (TYPE(x)) {
    case T_Fixnum:
	i = FIXNUM(x);
	return i < 0 ? Make_Integer (-i) : x;
    case T_Flonum:
	return Make_Flonum (fabs (FLONUM(x)->val));
    case T_Bignum:
	return Bignum_Abs (x);
    }
    /*NOTREACHED*/
}

Object General_Integer_Divide (x, y, rem) Object x, y; {
    register fx = FIXNUM(x), fy = FIXNUM(y);
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
	}
    case T_Bignum:
	switch (TYPE(y)) {
	case T_Fixnum:
	    ret = Bignum_Fixnum_Divide (x, y);
	    goto done;
	case T_Bignum:
	    ret = Bignum_Divide (x, y);
	    goto done;
	}
    }
    /*NOTREACHED*/
}

Object Exact_Quotient (x, y) Object x, y; {
    return General_Integer_Divide (x, y, 0);
}

Object Exact_Remainder (x, y) Object x, y; {
    return General_Integer_Divide (x, y, 1);
}

Object Exact_Modulo (x, y) Object x, y; {
    Object rem, xneg, yneg;
    GC_Node2;

    GC_Link2 (x, y);
    rem = General_Integer_Divide (x, y, 1);
    xneg = P_Negativep (x);
    yneg = P_Negativep (y);
    if (!EQ(xneg,yneg))
	rem = Generic_Plus (rem, y);
    GC_Unlink;
    return rem;
}

Object With_Exact_Ints (x, y, fun) Object x, y, (*fun)(); {
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

Object P_Quotient (x, y) Object x, y; {
    return With_Exact_Ints (x, y, Exact_Quotient);
}

Object P_Remainder (x, y) Object x, y; {
    return With_Exact_Ints (x, y, Exact_Remainder);
}

Object P_Modulo (x, y) Object x, y; {
    return With_Exact_Ints (x, y, Exact_Modulo);
}

Object Exact_Gcd (x, y) Object x, y; {
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

Object General_Gcd (x, y) Object x, y; {
    return With_Exact_Ints (x, y, Exact_Gcd);
}

Object P_Gcd (argc, argv) Object *argv; {
    return P_Abs (General_Operator (argc, argv, Zero, General_Gcd));
}

Object Exact_Lcm (x, y) Object x, y; {
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

Object General_Lcm (x, y) Object x, y; {
    return With_Exact_Ints (x, y, Exact_Lcm);
}

Object P_Lcm (argc, argv) Object *argv; {
    return P_Abs (General_Operator (argc, argv, One, General_Lcm));
}

#define General_Conversion(name,op) Object name (x) Object x; {\
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

Object P_Round (x) Object x; {
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

double Get_Double (x) Object x; {
    Check_Number (x);
    switch (TYPE(x)) {
    case T_Fixnum:
	return (double)FIXNUM(x);
    case T_Flonum:
	return FLONUM(x)->val;
    case T_Bignum:
	return Bignum_To_Double (x);
    }
    /*NOTREACHED*/
}

Object General_Function (x, y, fun) Object x, y; double (*fun)(); {
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

Object P_Sqrt (x) Object x; { return General_Function (x, Null, sqrt); }

Object P_Exp (x) Object x; { return General_Function (x, Null, exp); }

Object P_Log (x) Object x; { return General_Function (x, Null, log); }

Object P_Sin (x) Object x; { return General_Function (x, Null, sin); }

Object P_Cos (x) Object x; { return General_Function (x, Null, cos); }

Object P_Tan (x) Object x; { return General_Function (x, Null, tan); }

Object P_Asin (x) Object x; { return General_Function (x, Null, asin); }

Object P_Acos (x) Object x; { return General_Function (x, Null, acos); }

Object P_Atan (argc, argv) Object *argv; {
    register a2 = argc == 2;
    return General_Function (argv[0], a2 ? argv[1] : Null, a2 ? 
	(double(*)())atan2 : (double(*)())atan);
}

Object Min (x, y) Object x, y; {
    Object ret;
    
    ret = Generic_Less (x, y) ? x : y;
    if (TYPE(x) == T_Flonum || TYPE(y) == T_Flonum)
	ret = P_Exact_To_Inexact (ret);
    return ret;
}

Object Max (x, y) Object x, y; {
    Object ret;
    
    ret = Generic_Less (x, y) ? y : x;
    if (TYPE(x) == T_Flonum || TYPE(y) == T_Flonum)
	ret = P_Exact_To_Inexact (ret);
    return ret;
}

Object P_Min (argc, argv) Object *argv; {
    return General_Operator (argc, argv, argv[0], Min);
}

Object P_Max (argc, argv) Object *argv; {
    return General_Operator (argc, argv, argv[0], Max);
}

Object P_Random () {
#ifdef RANDOM
    extern long random();
    return Make_Long (random ());
#else
    return Make_Integer (rand ());
#endif
}

Object P_Srandom (x) Object x; {
#ifdef RANDOM
    srandom (Get_Unsigned (x));
#else
    srand (Get_Unsigned (x));
#endif
    return x;
}
