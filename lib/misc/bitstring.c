/* bitstring.c
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

#include <string.h>
#include <stdlib.h>

#include "scheme.h"

#define BITSTRING(x)     ((struct S_Bitstring *)POINTER(x))

struct S_Bitstring {
    Object tag;
    unsigned len;           /* # of used bits; unused bits in MSB always 0 */
    unsigned char data[1];  /* data[0] == LSB */
};

#define bits_to_bytes(b) (((b)+7)/8)

static int masks[]  = { 0, 0x1, 0x2, 0x4, 0x8, 0x10, 0x20, 0x40, 0x80 };
static int masks2[] = { 0, 0x1, 0x3, 0x7, 0xF, 0x1F, 0x3F, 0x7F, 0xFF };

int T_Bitstring;

static Object P_Bitstringp(Object x) {
    return TYPE(x) == T_Bitstring ? True : False;
}

static int Bitstring_Size(Object b) {
    return sizeof(struct S_Bitstring) + bits_to_bytes(BITSTRING(b)->len) - 1;
}

static int Bitstring_Equal(Object b1, Object b2) {
    struct S_Bitstring *a = BITSTRING(b1), *b = BITSTRING(b2);

    if (a->len != b->len)
        return 0;
    return !memcmp(a->data, b->data, bits_to_bytes(a->len));
}

static Object P_Bitstring_Equalp(Object a, Object b) {
    return Bitstring_Equal(a, b) ? True : False;
}

static char *Digits(unsigned char c, int n) {
    static char buf[9];
    int i = 0;

    for (; n > 0; n--)
        buf[i++] = c & masks[n] ? '1' : '0';
    buf[i] = '\0';
    return buf;
}

/* Print starting with MSB
 */
static int Bitstring_Print(Object x, Object port, int raw, int depth,
                           int length) {
    int i, rem;
    struct S_Bitstring *b = BITSTRING(x);
    GC_Node2;

    GC_Link2(x, port);
    Printf(port, "#*");
    i = bits_to_bytes(b->len) - 1;
    rem = b->len;
    if (rem % 8)
        Printf(port, Digits(b->data[i--], rem));
    for ( ; i >= 0; i--)
        Printf(port, Digits(b->data[i], 8));
    GC_Unlink;
    return 0;
}

static Object Make_Bitstring(unsigned int len) {
    Object b;
    int nbytes = bits_to_bytes(len);

    b = Alloc_Object(sizeof(struct S_Bitstring) + nbytes-1, T_Bitstring, 0);
    memset((char *)BITSTRING(b)->data, 0, nbytes);
    BITSTRING(b)->tag = Null;
    BITSTRING(b)->len = len;
    return b;
}

static void Fill_Bitstring(Object bs, int fill) {
    struct S_Bitstring *b = BITSTRING(bs);
    int i, rem;
    unsigned char val = fill ? ~0 : 0;

    i = bits_to_bytes(b->len) - 1;
    if (val && (rem = b->len % 8))
        b->data[i--] |= masks2[rem];
    for ( ; i >= 0; i--)
        b->data[i] = val;
}

static Object P_Make_Bitstring(Object len, Object init) {
    Object ret;
    int n, fill;

    if ((n = Get_Integer(len)) < 0)
        Range_Error(len);
    Check_Type(init, T_Boolean);
    fill = Truep(init);
    ret = Make_Bitstring((unsigned)n);
    if (fill)
        Fill_Bitstring(ret, 1);
    return ret;
}

static Object P_Bitstring_Length(Object bs) {
    Check_Type(bs, T_Bitstring);
    return Make_Unsigned(BITSTRING(bs)->len);
}

static int Ulong_Size(unsigned long ul) {
    int n;

    for (n = 0; ul; ul >>= 1, n++)
        ;
    return n;
}

static Object Ulong_To_Bitstring(unsigned long ul, unsigned int len) {
    Object ret;
    struct S_Bitstring *b;
    unsigned int i, siz = Ulong_Size(ul);
    char buf[50];

    ret = Make_Bitstring(len);
    b = BITSTRING(ret);
    if (siz > len) {
        sprintf(buf, "length %u too small for integer %lu", len, ul);
        Primitive_Error(buf);
    }
    for (i = 0; ul; ul >>= 8, i++)
        b->data[i] = ul & 0xFF;
    return ret;
}

static unsigned int Bigbits(struct S_Bignum *b) {
    return b->usize ? (Ulong_Size((unsigned long)b->data[b->usize-1]) +
            (b->usize-1) * sizeof(gran_t) * 8) : 0;
}

static Object Bignum_To_Bitstring(Object big, unsigned int len) {
    char buf[50];
    Object ret;
    struct S_Bitstring *b;
    struct S_Bignum *bn;
    unsigned int k, i, n;
    GC_Node;

    if (Bigbits(BIGNUM(big)) > len) {
        sprintf(buf, "length %u too small for integer ~s", len);
        Primitive_Error(buf, big);
    }
    GC_Link(big);
    ret = Make_Bitstring(len);
    GC_Unlink;
    b = BITSTRING(ret);
    bn = BIGNUM(big);
    n = bits_to_bytes(len);
    for (i = k = 0; k < bn->usize; k++, i++) {
        b->data[i] = bn->data[k] & 0xFF;
        if (i < n)
            b->data[++i] = bn->data[k] >> 8 & 0xFF;
    }
    return ret;
}

static Object P_Int_To_Bitstring(Object len, Object i) {
    Object isneg;
    int ilen;

    if ((ilen = Get_Integer(len)) < 0)
        Range_Error(len);
    Check_Integer(i);
    isneg = P_Negativep(i);
    if (Truep(isneg))
        Range_Error(i);
    if (TYPE(i) == T_Fixnum)
        return Ulong_To_Bitstring((unsigned long)FIXNUM(i), (unsigned)ilen);
    return Bignum_To_Bitstring(i, (unsigned)ilen);
}

static Object Bitstring_To_Bignum (Object bs) {
    struct S_Bitstring *b;
    Object big;
    int i, n, k;
    gran_t digit;
    GC_Node;

    n = bits_to_bytes(BITSTRING(bs)->len);
    GC_Link(bs);
    big = Make_Uninitialized_Bignum((n+1)/2); /* assume sizeof(gran_t)==2 */
    GC_Unlink;
    b = BITSTRING(bs);
    for (i = k = 0; i < n; k++, i++) {
        digit = b->data[i];
        if (!(i & 1))
            digit |= (unsigned)b->data[++i] << 8;
        BIGNUM(big)->data[k] = digit;
    }
    BIGNUM(big)->usize = k;
    Bignum_Normalize_In_Place (BIGNUM(big));
    return big;
}

static Object P_Bitstring_To_Int(Object bs) {
    struct S_Bitstring *b;
    unsigned u = 0;
    int i;

    Check_Type(bs, T_Bitstring);
    b = BITSTRING(bs);

    for (i = bits_to_bytes(b->len) - 1; i >= 0; i--) {
        u = u << 8 | b->data[i];
        if (!UFIXNUM_FITS(u))
            return Bitstring_To_Bignum(bs);
    }
    return Make_Integer(u);
}

static Object P_Bitstring_Ref(Object bs, Object inx) {
    struct S_Bitstring *b;
    int i;

    Check_Type(bs, T_Bitstring);
    b = BITSTRING(bs);
    i = Get_Integer(inx);
    if (i < 0 || i >= (int)b->len)
        Range_Error(inx);
    return b->data[i/8] & 1 << i % 8 ? True : False;
}

static Object P_Bitstring_Set(Object bs, Object inx, Object val) {
    int old, i, j, mask;
    struct S_Bitstring *b;

    Check_Type(bs, T_Bitstring);
    Check_Type(val, T_Boolean);
    b = BITSTRING(bs);
    i = Get_Integer(inx);
    if (i < 0 || i >= (int)b->len)
        Range_Error(inx);
    j = i/8;
    mask = 1 << i%8;
    old = b->data[j] & mask;
    if (Truep(val))
        b->data[j] |= mask;
    else
        b->data[j] &= ~mask;
    return old ? True : False;
}

static Object P_Bitstring_Zerop(Object bs) {
    struct S_Bitstring *b;
    int i;

    Check_Type(bs, T_Bitstring);
    b = BITSTRING(bs);
    for (i = bits_to_bytes(b->len); --i >= 0 && b->data[i] == 0 ;)
        ;
    return i < 0 ? True : False;
}

static Object P_Bitstring_Fill(Object bs, Object fill) {
    Check_Type(bs, T_Bitstring);
    Check_Type(fill, T_Boolean);
    Fill_Bitstring(bs, Truep(fill));
    return Void;
}

#define bitop(name, op) static void name(struct S_Bitstring *a,\
                                         struct S_Bitstring *b) {\
    int i, rem;\
\
    if (a->len != b->len) {\
        printf("bitstrings must be of same length\n"); exit(1);\
    }\
    i = bits_to_bytes(a->len) - 1;\
    rem = a->len % 8;\
    if (rem % 8) {\
        a->data[i] op b->data[i];\
        a->data[i--] &= masks2[rem];\
    }\
    for ( ; i >= 0; i--)\
        a->data[i] op b->data[i];\
}

bitop(bmove, =)
bitop(bnot, = ~)
bitop(band, &=)
bitop(bor, |=)
bitop(bandnot, &= ~)
bitop(bxor, ^=)

static Object Bit_Operation(Object b1, Object b2, void (*fun)()) {
    struct S_Bitstring *a, *b;

    Check_Type(b1, T_Bitstring);
    Check_Type(b2, T_Bitstring);
    a = BITSTRING(b1);
    b = BITSTRING(b2);
    if (a->len != b->len)
        Primitive_Error("bitstrings must have identical length");
    fun(a, b);
    return Void;
}

static Object P_Bitstring_Move(Object a, Object b) {
    return Bit_Operation(a, b, bmove);
}

static Object P_Bitstring_Not(Object a, Object b) {
    return Bit_Operation(a, b, bnot);
}

static Object P_Bitstring_And(Object a, Object b) {
    return Bit_Operation(a, b, band);
}

static Object P_Bitstring_Or(Object a, Object b) {
    return Bit_Operation(a, b, bor);
}

static Object P_Bitstring_Andnot(Object a, Object b) {
    return Bit_Operation(a, b, bandnot);
}

static Object P_Bitstring_Xor(Object a, Object b) {
    return Bit_Operation(a, b, bxor);
}

static Object P_Substring_Move(Object b1, Object from, Object to,
                               Object b2, Object dst) {
    struct S_Bitstring *a, *b;
    int start1, end1, start2, end2, len, off1, off2, i, j;
    unsigned char mask;

    Check_Type(b1, T_Bitstring);
    Check_Type(b2, T_Bitstring);
    a = BITSTRING(b1);
    b = BITSTRING(b2);
    start1 = Get_Integer(from);
    end1 = Get_Integer(to);
    start2 = Get_Integer(dst);
    len = end1 - start1;
    end2 = start2 + len;

    if (start1 < 0 || start1 > end1)
        Range_Error(from);
    if (end1 > (int)a->len)
        Range_Error(to);
    if (start2 < 0 || end2 > (int)b->len)
        Range_Error(dst);

    if (a == b && start2 < start1) { /* copy forward (LSB to MSB) */
        off1 = start1 % 8;
        off2 = start2 % 8;
        i = start1 / 8;
        j = start2 / 8;
        if (off1 == off2) {
            if (off1) {
                mask = 0xFF & ~masks2[off1];
                if (off1 + len < 8)
                    mask &= masks2[off1+len];
                b->data[j] = (b->data[j] & ~mask) | (a->data[i] & mask);
                len -= 8 - off1; i++; j++;
            }
            for (; len >= 8; len -= 8)
                b->data[j++] = a->data[i++];
            if (len > 0) {
                mask = masks2[len];
                b->data[j] = (b->data[j] & ~mask) | (a->data[i] & mask);
            }
        } else {
            unsigned char dmask;
            int n, delta, shift;

            while (len > 0) {
                shift = delta = off2 - off1;
                if (shift < 0)
                    shift = -shift;
                n = 8 - off1;
                mask = 0xFF & ~masks2[off1];
                if (len < n) {
                    n = len;
                    mask &= masks2[off1+len];
                }
                if (8 - off2 >= n) {  /* rest of src byte fits into dst byte */

                    if (delta > 0) {
                        dmask = mask << shift;
                        b->data[j] = (b->data[j] & ~dmask) |
                            (a->data[i] & mask) << shift;
                    } else {
                        dmask = mask >> shift;
                        b->data[j] = (b->data[j] & ~dmask) |
                            (unsigned int)(a->data[i] & mask) >> shift;
                    }
                } else {  /* nope, copy as many bits as fit into dst bye */

                    n = 8 - off2;
                    mask &= masks2[off1+n];
                    dmask = mask << shift;
                    b->data[j] = (b->data[j] & ~dmask) |
                        (a->data[i] & mask) << shift;
                }

                if (off1 + n >= 8) i++;
                if (off2 + n >= 8) j++;
                off1 = (off1 + n) % 8;
                off2 = (off2 + n) % 8;
                len -= n;
            }
        }
    } else {  /* copy backwards (MSB to LSB) */

        if ((off1 = end1 % 8 - 1) < 0) off1 = 7;
        if ((off2 = end2 % 8 - 1) < 0) off2 = 7;
        i = (end1 - 1) / 8;
        j = (end2 - 1) / 8;
        if (off1 == off2) {
            if (off1 < 7) {
                if (len <= off1)
                    mask = masks2[len] << (off1-len+1);
                else
                    mask = masks2[off1+1];
                b->data[j] = (b->data[j] & ~mask) | (a->data[i] & mask);
                len -= off1+1; i--; j--;
            }
            for (; len >= 8; len -= 8)
                b->data[j--] = a->data[i--];
            if (len > 0) {
                mask = masks2[len] << (8 - len);
                b->data[j] = (b->data[j] & ~mask) | (a->data[i] & mask);
            }
        } else {
            unsigned char dmask;
            int n, delta, shift;

            while (len > 0) {
                shift = delta = off2 - off1;
                if (shift < 0)
                    shift = -shift;
                n = off1 + 1;
                mask = masks2[n];
                if (len < n) {
                    mask = masks2[len] << (n-len);
                    n = len;
                }
                if (off2 + 1 >= n) { /* rest of src byte fits into dst byte */

                    if (delta > 0) {
                        dmask = mask << shift;
                        b->data[j] = (b->data[j] & ~dmask) |
                            (a->data[i] & mask) << shift;
                    } else {
                        dmask = mask >> shift;
                        b->data[j] = (b->data[j] & ~dmask) |
                            (unsigned int)(a->data[i] & mask) >> shift;
                    }
                } else {  /* nope, copy as many bits as fit into dst bye */

                    n = off2 + 1;
                    mask = masks2[n] << (off1-n+1);
                    dmask = mask >> shift;
                    b->data[j] = (b->data[j] & ~dmask) |
                        (unsigned int)(a->data[i] & mask) >> shift;
                }

                if (off1 - n < 0) i--;
                if (off2 - n < 0) j--;
                if ((off1 -= n) < 0) off1 += 8;
                if ((off2 -= n) < 0) off2 += 8;
                len -= n;
            }
        }
    }
    return Void;
}

/*ARGSUSED*/
static Object Bitstring_Read(Object port, int chr, int konst) {
    int c, str, i;
    FILE *f;
    char buf[1024], *p = buf;
    Object ret;

    f = PORT(port)->file;
    str = PORT(port)->flags & P_STRING;
    while (1) {
        Reader_Getc;
        if (c == EOF)
            Reader_Sharp_Eof;
        if (Whitespace (c) || Delimiter (c))
            break;
        if (p == buf+1024)
            Reader_Error(port, "bitstring constant too long for reader");
        if (c != '0' && c != '1')
            Reader_Error(port, "bad digit in bitstring constant");
        *p++ = c;
    }
    Reader_Ungetc;
    ret = Make_Bitstring(p-buf);
    for (i = 0; p > buf; i++)
        if (*--p == '1')
            BITSTRING(ret)->data[i/8] |= 1 << i%8;
    return ret;
}

#define Def_Prim Define_Primitive

void elk_init_lib_bitstring() {
    T_Bitstring = Define_Type(0, "bitstring", Bitstring_Size, 0,
        Bitstring_Equal, Bitstring_Equal, Bitstring_Print, NOFUNC);
    Define_Reader('*', Bitstring_Read);
    Def_Prim(P_Bitstringp,       "bitstring?",                  1, 1, EVAL);
    Def_Prim(P_Bitstring_Equalp, "bitstring=?",                 2, 2, EVAL);
    Def_Prim(P_Make_Bitstring,   "make-bitstring",              2, 2, EVAL);
    Def_Prim(P_Bitstring_Length, "bitstring-length",            1, 1, EVAL);
    Def_Prim(P_Int_To_Bitstring, "unsigned-integer->bitstring", 2, 2, EVAL);
    Def_Prim(P_Bitstring_To_Int, "bitstring->unsigned-integer", 1, 1, EVAL);
    Def_Prim(P_Bitstring_Ref,    "bitstring-ref",               2, 2, EVAL);
    Def_Prim(P_Bitstring_Set,    "bitstring-set!",              3, 3, EVAL);
    Def_Prim(P_Bitstring_Zerop,  "bitstring-zero?",             1, 1, EVAL);
    Def_Prim(P_Bitstring_Fill,   "bitstring-fill!",             2, 2, EVAL);
    Def_Prim(P_Bitstring_Move,   "bitstring-move!",             2, 2, EVAL);
    Def_Prim(P_Bitstring_Not,    "bitstring-not!",              2, 2, EVAL);
    Def_Prim(P_Bitstring_And,    "bitstring-and!",              2, 2, EVAL);
    Def_Prim(P_Bitstring_Or,     "bitstring-or!",               2, 2, EVAL);
    Def_Prim(P_Bitstring_Andnot, "bitstring-andnot!",           2, 2, EVAL);
    Def_Prim(P_Bitstring_Xor,    "bitstring-xor!",              2, 2, EVAL);
    Def_Prim(P_Substring_Move,   "bitstring-substring-move!",   5, 5, EVAL);
    P_Provide (Intern ("bitstring.so"));
}
