#include <math.h>
#include <ctype.h>

#include "kernel.h"

Object Make_Uninitialized_Bignum (size) {
    Object big;
    
    big = Alloc_Object ((sizeof (struct S_Bignum) - sizeof (gran_t)) +
		   (size * sizeof (gran_t)), T_Bignum, 0);
    BIGNUM(big)->minusp = False;
    BIGNUM(big)->size = size;
    BIGNUM(big)->usize = 0;
    return big;
}

Object Copy_Bignum (x) Object x; {
    Object big;
    register size;
    GC_Node;

    GC_Link (x);
    big = Make_Uninitialized_Bignum (size = BIGNUM(x)->usize);
    BIGNUM(big)->minusp = BIGNUM(x)->minusp;
    BIGNUM(big)->usize = size;
    bcopy ((char *)BIGNUM(x)->data, (char *)BIGNUM(big)->data,
	  size * sizeof (gran_t));
    GC_Unlink;
    return big;
}

Object Copy_S_Bignum (s) struct S_Bignum *s; {
    Object big;
    register size;

    big = Make_Uninitialized_Bignum (size = s->usize);
    BIGNUM(big)->minusp = s->minusp;
    BIGNUM(big)->usize = size;
    bcopy ((char *)s->data, (char *)BIGNUM(big)->data,
	  size * sizeof (gran_t));
    return big;
}

Object Make_Bignum (buf, neg, radix) const char *buf; {
    Object big;
    register const char *p;
    register c;
    register size = (strlen (buf) + 4) / 4;

    big = Make_Uninitialized_Bignum (size);
    BIGNUM(big)->minusp = neg ? True : False;
    p = buf;
    while (c = *p++) {
	Bignum_Mult_In_Place (BIGNUM(big), radix);
	if (radix == 16) {
	    if (isupper (c))
		c = tolower (c);
	    if (c >= 'a')
		c = '9' + c - 'a' + 1;
	}
	Bignum_Add_In_Place (BIGNUM(big), c - '0');
    }
    Bignum_Normalize_In_Place (BIGNUM(big)); /* to avoid -0 */
    return big;
}

Object Reduce_Bignum (x) Object x; {
    unsigned ret = 0;
    int i, shift = 0, size = BIGNUM(x)->usize;
    int digits = sizeof(int)/2;

    if (size > digits)
	return x;
    for (i = 0; i < digits && i < size; i++, shift += 16)
	ret |= (unsigned)BIGNUM(x)->data[i] << shift;
    if (Truep (BIGNUM(x)->minusp)) {
	if (ret > (~(unsigned)0 >> 1) + 1)
	    return x;
	return Make_Integer (-ret);
    } else {
	if (ret > ~(unsigned)0 >> 1)
	    return x;
	return Make_Integer (ret);
    }
}

Bignum_Mult_In_Place (x, n) register struct S_Bignum *x; {
    register i = x->usize;
    register gran_t *p = x->data;
    register j;
    register unsigned k = 0;
    
    for (j = 0; j < i; ++j) {
	k += n * *p;
        *p++ = k;
	k >>= 16;
    }
    if (k) {
	if (i >= x->size)
	    Panic ("Bignum_Mult_In_Place");
	*p++ = k;
	x->usize++;
    }
}

Bignum_Add_In_Place (x, n) register struct S_Bignum *x; {
    register i = x->usize;
    register gran_t *p = x->data;
    register j = 0;
    register unsigned k = n;

    if (i == 0) goto extend;
    k += *p;
    *p++ = k;
    while (k >>= 16) {
	if (++j >= i) {
 extend:
	    if (i >= x->size)
		Panic ("Bignum_Add_In_Place");
	    *p++ = k;
	    x->usize++;
	    return;
	}
	k += *p;
	*p++ = k;
    }
}

Bignum_Div_In_Place (x, n) register struct S_Bignum *x; {
    register i = x->usize;
    register gran_t *p = x->data + i;
    register unsigned k = 0;
    for ( ; i; --i) {
	k <<= 16;
	k += *--p;
	*p = k / n;
	k %= n;
    }
    Bignum_Normalize_In_Place (x);
    return k;
}

Bignum_Normalize_In_Place (x) register struct S_Bignum *x; {
    register i = x->usize;
    register gran_t *p = x->data + i;
    while (i && !*--p)
	--i;
    x->usize = i;
    if (!i)
	x->minusp = False;
}

Print_Bignum (port, x) Object port, x; {
    register char *p;
    char *buf;
    register size;
    struct S_Bignum *big;
    Alloca_Begin;
    
    if (Bignum_Zero (x)) {
	Printf (port, "0");
	return;
    }
    
    size = BIGNUM(x)->usize * 5 + 3;
    Alloca (buf, char*, size + 1);
    p = buf + size;
    *p = 0;

    size = (sizeof (struct S_Bignum) - sizeof (gran_t))
	+ BIGNUM(x)->usize * sizeof (gran_t);
    Alloca (big, struct S_Bignum*, size);
    bcopy ((char *)POINTER(x), (char *)big, size);
    big->size = BIGNUM(x)->usize;

    while (big->usize) {
	register unsigned bigdig = Bignum_Div_In_Place (big, 10000);
	*--p = '0' + bigdig % 10;
	bigdig /= 10;
	*--p = '0' + bigdig % 10;
	bigdig /= 10;
	*--p = '0' + bigdig % 10;
	bigdig /= 10;
	*--p = '0' + bigdig;
    }
    while (*p == '0')
	++p;
    if (Truep (BIGNUM(x)->minusp))
	Printf (port, "-");
    Format (port, p, strlen (p), 0, (Object *)0);
    Alloca_End;
}

Object Bignum_To_String (x, radix) Object x; {
    register char *p;
    char *buf;
    register unsigned div, ndig, size;
    struct S_Bignum *big;
    Object ret;
    Alloca_Begin;
    
    if (Bignum_Zero (x))
	return Make_String ("0", 1);
    
    size = BIGNUM(x)->usize * (radix == 2 ? 17 : 6) + 3;
    Alloca (buf, char*, size + 1);
    p = buf + size;
    *p = 0;

    size = (sizeof (struct S_Bignum) - sizeof (gran_t))
	+ BIGNUM(x)->usize * sizeof (gran_t);
    Alloca (big, struct S_Bignum*, size);
    bcopy ((char *)POINTER(x), (char *)big, size);
    big->size = BIGNUM(x)->usize;

    switch (radix) {
    case 2:
	div = 65536; ndig = 16; break;
    case 8:
	div = 32768; ndig = 5; break;
    case 10:
	div = 10000; ndig = 4; break;
    case 16:
	div = 65536; ndig = 4; break;
    }

    while (big->usize) {
	register unsigned bigdig = Bignum_Div_In_Place (big, div);
	register i;
	for (i = 0; i < ndig; i++) {
	    *--p = '0' + bigdig % radix;
	    if (*p > '9')
		*p = 'A' + (*p - '9') - 1;
	    bigdig /= radix;
	}
    }
    while (*p == '0')
	++p;
    if (Truep (BIGNUM(x)->minusp))
	*--p = '-';
    ret = Make_String (p, strlen (p));
    Alloca_End;
    return ret;
}

Bignum_To_Integer (x) Object x; {
    unsigned ret = 0;
    int i, shift = 0, size = BIGNUM(x)->usize;
    int digits = sizeof(int)/2;

    if (size > digits)
err:
	Primitive_Error ("integer out of range: ~s", x);
    for (i = 0; i < digits && i < size; i++, shift += 16)
	ret |= (unsigned)BIGNUM(x)->data[i] << shift;
    if (Truep (BIGNUM(x)->minusp)) {
	if (ret > (~(unsigned)0 >> 1) + 1)
	    goto err;
	return -ret;
    } else {
	if (ret > ~(unsigned)0 >> 1)
	    goto err;
	return ret;
    }
}

unsigned Bignum_To_Unsigned (x) Object x; {
    unsigned ret = 0;
    int i, shift = 0, size = BIGNUM(x)->usize;
    int digits = sizeof(int)/2;

    if (size > digits || Truep (BIGNUM(x)->minusp))
	Primitive_Error ("integer out of range: ~s", x);
    for (i = 0; i < digits && i < size; i++, shift += 16)
	ret |= (unsigned)BIGNUM(x)->data[i] << shift;
    return ret;
}

long Bignum_To_Long (x) Object x; {
    unsigned long ret = 0;
    int i, shift = 0, size = BIGNUM(x)->usize;
    int digits = sizeof(long)/2;

    if (size > digits)
err:
	Primitive_Error ("integer out of range: ~s", x);
    for (i = 0; i < digits && i < size; i++, shift += 16)
	ret |= (unsigned long)BIGNUM(x)->data[i] << shift;
    if (Truep (BIGNUM(x)->minusp)) {
	if (ret > (~(unsigned long)0 >> 1) + 1)
	    goto err;
	return -ret;
    } else {
	if (ret > ~(unsigned long)0 >> 1)
	    goto err;
	return ret;
    }
}

unsigned long Bignum_To_Unsigned_Long (x) Object x; {
    unsigned long ret = 0;
    int i, shift = 0, size = BIGNUM(x)->usize;
    int digits = sizeof(long)/2;

    if (size > digits || Truep (BIGNUM(x)->minusp))
	Primitive_Error ("integer out of range: ~s", x);
    for (i = 0; i < digits && i < size; i++, shift += 16)
	ret |= (unsigned long)BIGNUM(x)->data[i] << shift;
    return ret;
}

Object Integer_To_Bignum (i) {
    int k, digits = sizeof(int)/2;
    Object big; 
    unsigned n = i;

    big = Make_Uninitialized_Bignum (digits);
    if (i < 0) {
	BIGNUM(big)->minusp = True;
	n = -i;
    }
    for (k = 0; k < digits; k++, n >>= 16)
	BIGNUM(big)->data[k] = n & 0xffff;
    BIGNUM(big)->usize = k;
    Bignum_Normalize_In_Place (BIGNUM(big));
    return big;
}

Object Unsigned_To_Bignum (i) unsigned i; {
    int k, digits = sizeof(int)/2;
    Object big;
    
    big = Make_Uninitialized_Bignum (digits);
    for (k = 0; k < digits; k++, i >>= 16)
	BIGNUM(big)->data[k] = i & 0xffff;
    BIGNUM(big)->usize = k;
    Bignum_Normalize_In_Place (BIGNUM(big));
    return big;
}

Object Long_To_Bignum (i) long i; {
    int k, digits = sizeof(long)/2;
    Object big;
    unsigned long n = i;

    big = Make_Uninitialized_Bignum (digits);
    if (i < 0) {
	BIGNUM(big)->minusp = True;
	n = -i;
    }
    for (k = 0; k < digits; k++, n >>= 16)
	BIGNUM(big)->data[k] = n & 0xffff;
    BIGNUM(big)->usize = k;
    Bignum_Normalize_In_Place (BIGNUM(big));
    return big;
}

Object Unsigned_Long_To_Bignum (i) unsigned long i; {
    int k, digits = sizeof(long)/2;
    Object big;
    
    big = Make_Uninitialized_Bignum (digits);
    for (k = 0; k < digits; k++, i >>= 16)
	BIGNUM(big)->data[k] = i & 0xffff;
    BIGNUM(big)->usize = k;
    Bignum_Normalize_In_Place (BIGNUM(big));
    return big;
}

Object Double_To_Bignum (d) double d; {         /* Truncates the double */
    Object big;
    int expo, size;
    double mantissa = frexp (d, &expo);
    register gran_t *p;
    
    if (expo <= 0 || mantissa == 0.0)
	return Make_Uninitialized_Bignum (0);
    size = (expo + (16-1)) / 16;
    big = Make_Uninitialized_Bignum (size);
    BIGNUM(big)->usize = size;
    if (mantissa < 0.0) {
	BIGNUM(big)->minusp = True;
	mantissa = -mantissa;
    }
    p = BIGNUM(big)->data;
    bzero ((char *)p, size * sizeof (gran_t));
    p += size;
    if (expo &= (16-1))
	mantissa = ldexp (mantissa, expo - 16);
    while (mantissa != 0.0) {
	if (--size < 0)
	    break;		/* inexact */
	mantissa *= 65536.0;
	*--p = (int)mantissa;
	mantissa -= *p;
    }
    Bignum_Normalize_In_Place (BIGNUM(big)); /* Probably not needed */
    return Reduce_Bignum (big);
}

double Bignum_To_Double (x) Object x; {   /* error if it ain't fit */
    double rx = 0.0;
    register i = BIGNUM(x)->usize;
    register gran_t *p = BIGNUM(x)->data + i;

    for (i = BIGNUM(x)->usize; --i >= 0; ) {
	if (rx >= HUGE / 65536.0)
	    Primitive_Error ("cannot coerce to real: ~s", x);
	rx *= 65536.0;
	rx += *--p;
    }
    if (Truep (BIGNUM(x)->minusp))
	rx = -rx;
    return rx;
}

Bignum_Zero (x) Object x; {
    return BIGNUM(x)->usize == 0;
}

Bignum_Negative (x) Object x; {
    return Truep (BIGNUM(x)->minusp);
}

Bignum_Positive (x) Object x; {
    return !Truep (BIGNUM(x)->minusp) && BIGNUM(x)->usize != 0;
}

Bignum_Even (x) Object x; {
    return BIGNUM(x)->usize == 0 || (BIGNUM(x)->data[0] & 1) == 0;
}

Object Bignum_Abs (x) Object x; {
    Object big;

    big = Copy_Bignum (x);
    BIGNUM(big)->minusp = False;
    return big;
}

Bignum_Mantissa_Cmp (x, y) register struct S_Bignum *x, *y; {
    register i = x->usize;
    if (i < y->usize)
	return -1;
    else if (i > y->usize)
	return 1;
    else {
	register gran_t *xbuf = x->data + i;
	register gran_t *ybuf = y->data + i;
	for ( ; i; --i) {
	    register n;
	    if (n = (int)*--xbuf - (int)*--ybuf)
		return n;
	}
	return 0;
    }
}

Bignum_Cmp (x, y) register struct S_Bignum *x, *y; {
    register xm = Truep (x->minusp);
    register ym = Truep (y->minusp);
    if (xm) {
	if (ym)
	    return -Bignum_Mantissa_Cmp (x, y);
	else return -1;
    } else {
	if (ym)
	    return 1;
	else return Bignum_Mantissa_Cmp (x, y);
    }
}

Bignum_Equal (x, y) Object x, y; {
    return Bignum_Cmp (BIGNUM(x), BIGNUM(y)) == 0;
}

Bignum_Less (x, y) Object x, y; {
    return Bignum_Cmp (BIGNUM(x), BIGNUM(y)) < 0;
}

Bignum_Greater (x, y) Object x, y; {
    return Bignum_Cmp (BIGNUM(x), BIGNUM(y)) > 0;
}

Bignum_Eq_Less (x, y) Object x, y; {
    return Bignum_Cmp (BIGNUM(x), BIGNUM(y)) <= 0;
}

Bignum_Eq_Greater (x, y) Object x, y; {
    return Bignum_Cmp (BIGNUM(x), BIGNUM(y)) >= 0;
}

Object General_Bignum_Plus_Minus (x, y, neg) Object x, y; {
    Object big;
    int size, xsize, ysize, xminusp, yminusp;
    GC_Node2;

    GC_Link2 (x,y);
    xsize = BIGNUM(x)->usize;
    ysize = BIGNUM(y)->usize;
    xminusp = Truep (BIGNUM(x)->minusp);
    yminusp = Truep (BIGNUM(y)->minusp);
    if (neg)
	yminusp = !yminusp;
    size = xsize > ysize ? xsize : ysize;
    if (xminusp == yminusp)
	size++;
    big = Make_Uninitialized_Bignum (size);
    BIGNUM(big)->usize = size;
    GC_Unlink;

    if (xminusp == yminusp) {
	/* Add x and y */
	register unsigned k = 0;
	register i;
	register gran_t *xbuf = BIGNUM(x)->data;
	register gran_t *ybuf = BIGNUM(y)->data;
	register gran_t *zbuf = BIGNUM(big)->data;
	for (i = 0; i < size; ++i) {
	    if (i < xsize)
		k += *xbuf++;
	    if (i < ysize)
		k += *ybuf++;
	    *zbuf++ = k;
	    k >>= 16;
	}
    } else {
	if (Bignum_Mantissa_Cmp (BIGNUM(x), BIGNUM(y)) < 0) {
	    Object temp;
	    
	    temp = x; x = y; y = temp;
	    xsize = ysize;
	    ysize = BIGNUM(y)->usize;
	    xminusp = yminusp;
	}
	/* Subtract y from x */
    {
	register unsigned k = 1;
	register i;
	register gran_t *xbuf = BIGNUM(x)->data;
	register gran_t *ybuf = BIGNUM(y)->data;
	register gran_t *zbuf = BIGNUM(big)->data;
	for (i = 0; i < size; ++i) {
	    if (i < xsize)
		k += *xbuf++;
	    else Panic ("General_Bignum_Plus_Minus");
	    if (i < ysize)
		k += ~*ybuf++ & 0xFFFF;
	    else k += 0xFFFF;
	    *zbuf++ = k;
	    k >>= 16;
	}
    }
    }
    BIGNUM(big)->minusp = xminusp ? True : False;
    Bignum_Normalize_In_Place (BIGNUM(big));
    return Reduce_Bignum (big);
}

Object Bignum_Plus (x, y) Object x, y; {   /* bignum + bignum */
    return General_Bignum_Plus_Minus (x, y, 0);
}

Object Bignum_Minus (x, y) Object x, y; {   /* bignum - bignum */
    return General_Bignum_Plus_Minus (x, y, 1);
}

Object Bignum_Fixnum_Multiply (x, y) Object x, y; {   /* bignum * fixnum */
    Object big;
    register size, xsize, i;
    register gran_t *xbuf, *zbuf;
    int fix = FIXNUM(y);
    register unsigned yl, yh;
    GC_Node;
    
    GC_Link (x);
    xsize = BIGNUM(x)->usize;
    size = xsize + 2;
    big = Make_Uninitialized_Bignum (size);
    BIGNUM(big)->usize = size;
    if (Truep (BIGNUM(x)->minusp) != (fix < 0))
	BIGNUM(big)->minusp = True;
    bzero ((char *)BIGNUM(big)->data, size * sizeof (gran_t));
    xbuf = BIGNUM(x)->data;
    if (fix < 0)
	fix = -fix;
    yl = fix & 0xFFFF;
    yh = fix >> 16;
    zbuf = BIGNUM(big)->data;
    for (i = 0; i < xsize; ++i) {
	register unsigned xf = xbuf[i];
	register unsigned k = 0;
	register gran_t *r = zbuf + i;
	k += xf * yl + *r;
	*r++ = k;
	k >>= 16;
	k += xf * yh + *r;
	*r++ = k;
	k >>= 16;
	*r = k;
    }
    GC_Unlink;
    Bignum_Normalize_In_Place (BIGNUM(big));
    return Reduce_Bignum (big);
}

Object Bignum_Multiply (x, y) Object x, y; {   /* bignum * bignum */
    Object big;
    register size, xsize, ysize, i, j;
    register gran_t *xbuf, *ybuf, *zbuf;
    GC_Node2;
    
    GC_Link2 (x, y);
    xsize = BIGNUM(x)->usize;
    ysize = BIGNUM(y)->usize;
    size = xsize + ysize;
    big = Make_Uninitialized_Bignum (size);
    BIGNUM(big)->usize = size;
    if (!EQ(BIGNUM(x)->minusp, BIGNUM(y)->minusp))
	BIGNUM(big)->minusp = True;
    bzero ((char *)BIGNUM(big)->data, size * sizeof (gran_t));
    xbuf = BIGNUM(x)->data;
    ybuf = BIGNUM(y)->data;
    zbuf = BIGNUM(big)->data;
    for (i = 0; i < xsize; ++i) {
	register unsigned xf = xbuf[i];
	register unsigned k = 0;
	register gran_t *p = ybuf;
	register gran_t *r = zbuf + i;
	for (j = 0; j < ysize; ++j) {
	    k += xf * *p++ + *r;
	    *r++ = k;
	    k >>= 16;
	}
	*r = k;
    }
    GC_Unlink;
    Bignum_Normalize_In_Place (BIGNUM(big));
    return Reduce_Bignum (big);
}

/* Returns cons cell (quotient . remainder); cdr is a fixnum
 */
Object Bignum_Fixnum_Divide (x, y) Object x, y; {   /* bignum / fixnum */
    Object big;
    register xsize, i;
    register gran_t *xbuf, *zbuf;
    int fix = FIXNUM(y);
    int xminusp, yminusp = 0;
    register unsigned rem;
    GC_Node;

    GC_Link (x);
    if (fix < 0) {
	fix = -fix;
	yminusp = 1;
    }
    if (fix > 0xFFFF) {
	big = Integer_To_Bignum (FIXNUM(y));
	GC_Unlink;
	return Bignum_Divide (x, big);
    }
    xsize = BIGNUM(x)->usize;
    big = Make_Uninitialized_Bignum (xsize);
    BIGNUM(big)->usize = xsize;
    xminusp = Truep (BIGNUM(x)->minusp);
    if (xminusp != yminusp)
	BIGNUM(big)->minusp = True;
    xbuf = BIGNUM(x)->data;
    zbuf = BIGNUM(big)->data;
    rem = 0;
    for (i = xsize; --i >= 0; ) {
	rem <<= 16;
	rem += xbuf[i];
	zbuf[i] = rem / fix;
	rem %= fix;
    }
    GC_Unlink;
    Bignum_Normalize_In_Place (BIGNUM(big));
    if (xminusp)
	rem = -(int)rem;
    return Cons (Reduce_Bignum (big), Make_Integer ((int)rem));
}

/* Returns cons cell (quotient . remainder); cdr is a fixnum
 */
Object Bignum_Divide (x, y) Object x, y; {   /* bignum / bignum */
    struct S_Bignum *dend, *dor;
    int quotsize, dendsize, dorsize, scale;
    unsigned dor1, dor2;
    Object quot, rem;
    register gran_t *qp, *dendp;
    GC_Node2;
    Alloca_Begin;

    if (BIGNUM(y)->usize < 2)
	return Bignum_Fixnum_Divide (x, Make_Integer (Bignum_To_Integer (y)));

    GC_Link2 (x, y);
    quotsize = BIGNUM(x)->usize - BIGNUM(y)->usize + 1;
    if (quotsize < 0)
	quotsize = 0;
    quot = Make_Uninitialized_Bignum (quotsize);
    GC_Unlink;

    dendsize = (sizeof (struct S_Bignum) - sizeof (gran_t))
	+ (BIGNUM(x)->usize + 1) * sizeof (gran_t);
    Alloca (dend, struct S_Bignum*, dendsize);
    bcopy ((char *)POINTER(x), (char *)dend, dendsize);
    dend->size = BIGNUM(x)->usize + 1;

    if (quotsize == 0 || Bignum_Mantissa_Cmp (dend, BIGNUM(y)) < 0)
	goto zero;

    dorsize = (sizeof (struct S_Bignum) - sizeof (gran_t))
	+ BIGNUM (y)->usize * sizeof (gran_t);
    Alloca (dor, struct S_Bignum*, dorsize);
    bcopy ((char *)POINTER(y), (char *)dor, dorsize);
    dor->size = dorsize = BIGNUM(y)->usize;

    scale = 65536 / (unsigned int)(dor->data[dor->usize - 1] + 1);
    Bignum_Mult_In_Place (dend, scale);
    if (dend->usize < dend->size)
	dend->data[dend->usize++] = 0;
    Bignum_Mult_In_Place (dor, scale);

    BIGNUM(quot)->usize = BIGNUM(quot)->size;
    qp = BIGNUM(quot)->data + BIGNUM(quot)->size;
    dendp = dend->data + dend->usize;
    dor1 = dor->data[dor->usize - 1];
    dor2 = dor->data[dor->usize - 2];
        
    while (qp > BIGNUM(quot)->data) {
	unsigned msw, guess;
	int k;
	register gran_t *dep, *dop, *edop;
	
	msw = dendp[-1] << 16 | dendp[-2];
	guess = msw / dor1;
	if (guess >= 65536)	/* [65535, 0, 0] / [65535, 65535] */
	    guess = 65535;
	for (;;) {
	    unsigned d1, d2, d3;
	    d3 = dor2 * guess;
	    d2 = dor1 * guess + (d3 >> 16);
	    d3 &= 0xFFFF;
	    d1 = d2 >> 16;
	    d2 &= 0xFFFF;
	    if (d1 < dendp[-1] || (d1 == dendp[-1] &&
				   (d2 < dendp[-2] || (d2 == dendp[-2] &&
						       d3 <= dendp[-3]))))
		break;
	    --guess;
	}
	--dendp;
	k = 0;
	dep = dendp - dorsize;
	for (dop = dor->data, edop = dop + dor->usize; dop < edop; ) {
	    register unsigned prod = *dop++ * guess;
	    k += *dep;
	    k -= prod & 0xFFFF;
	    *dep++ = k;
	    ASR(k, 16);
	    k -= prod >> 16;
	}
	k += *dep;
	*dep = k;
	if (k < 0) {
	    k = 0;
	    dep = dendp - dorsize;
	    for (dop = dor->data, edop = dop + dor->usize; dop < edop; ) {
		k += *dep + *dop++;
		*dep++ = k;
		ASR(k, 16);
	    }
	    k += *dep;
	    *dep = k;
	    --guess;
	}
	*--qp = guess;
    }
    
    if (Bignum_Div_In_Place (dend, scale))
	Panic ("Bignum_Div scale");
 zero:
    dend->minusp = BIGNUM(x)->minusp;
    if (Truep (dend->minusp) != Truep (BIGNUM(y)->minusp))
	BIGNUM(quot)->minusp = True;
    Bignum_Normalize_In_Place (BIGNUM(quot));
    Bignum_Normalize_In_Place (dend);
    GC_Link (quot);
    rem = Reduce_Bignum (Copy_S_Bignum (dend));
    GC_Unlink;
    Alloca_End;
    return Cons (Reduce_Bignum (quot), rem);
}
