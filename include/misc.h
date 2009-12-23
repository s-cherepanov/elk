/* misc.h
 *
 * $Id$
 *
 * Copyright 1990, 1991, 1992, 1993, 1994, 1995, Oliver Laumann, Berlin
 * Copyright 2002, 2003 Sam Hocevar <sam@hocevar.net>, Paris
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

extern_c Object elk_import False2;

#define Nullp(x)    (TYPE(x) == T_Null)
#define Truep(x)    (!EQ(x,False) && !EQ(x,False2))
#define Car(x)      PAIR(x)->car
#define Cdr(x)      PAIR(x)->cdr
#define Cons        P_Cons
#define Begin       P_Begin
#define Assq(x,y)   General_Assoc(x,y,0)
#define Print(x)    General_Print_Object (x, Curr_Output_Port, 0)
#define Numeric(t)  (t == T_Fixnum || t == T_Flonum || t == T_Bignum)

#define Whitespace(c) (c == ' ' || c == '\t' || c == '\014' || c == '\n' || c == '\r')
#define Delimiter(c) (c == ';' || c == ')' || c == '(' || c == '[' || c == ']' || c == '"')


/* Align heap addresses */
#ifdef ALIGN_8BYTE
#  define ELK_ALIGN(ptr) ((ptr) = (void *)(((intptr_t)(ptr) + 7) & ~7))
#else
#  define ELK_ALIGN(ptr) ((ptr) = (void *)(((intptr_t)(ptr) + 3) & ~3))
#endif

/* Normalize stack addresses */
#define NORM(addr)  ((intptr_t)(addr) + delta)


/* Used in special forms: */
extern int Tail_Call;

#define TC_Prolog   register int _t = Tail_Call
#define TC_Disable  Tail_Call = 0
#define TC_Enable   Tail_Call = _t


/* Macros to be used by readers registered with Define_Reader().
 * They operate on variables c, port, f, and str.
 */
#define Reader_Getc {\
    c = str ? String_Getc (port) : getc (f);\
    if (c == '\n') PORT(port)->lno++;\
}

#define Reader_Ungetc {\
    if (str) String_Ungetc (port,c); else (void)ungetc (c,f);\
    if (c == '\n') if (PORT(port)->lno > 1) PORT(port)->lno--;\
}

#define Reader_Tweak_Stream {\
    if (!str && (feof (f) || ferror (f))) clearerr (f);\
}

#define Reader_Sharp_Eof {\
    Reader_Tweak_Stream;\
    Reader_Error (port, "end of file after `#'");\
}
