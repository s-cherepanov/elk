/* read.c: Input functions and primitives; the Scheme reader/parser.
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

#include "config.h"

#include <ctype.h>
#include <limits.h>
#include <string.h>

#if defined(HAVE_TERMIO_H)
#   include <termio.h>
#elif defined(HAVE_TERMIOS_H)
#   include <termios.h>
#endif

#if defined(HAVE_SYS_IOCTL_H)
#   include <sys/ioctl.h>
#endif

#if defined(HAVE_SYS_FILIO_H)
#   include <sys/filio.h>
#endif

#include "kernel.h"

extern void Flush_Output (Object);

extern double atof();

int Skip_Comment (Object);
void Reader_Error (Object, char *) elk_attribute(__noreturn__);

Object Sym_Quote,
       Sym_Quasiquote,
       Sym_Unquote,
       Sym_Unquote_Splicing;

#define Octal(c) ((c) >= '0' && (c) <= '7')

static READFUN Readers[256];

static char *Read_Buf;
static int Read_Size, Read_Max;

#define Read_Reset()   (Read_Size = 0)
#define Read_Store(c)  (Read_Size == Read_Max ? \
    (Read_Grow(), Read_Buf[Read_Size++] = (c)) : (Read_Buf[Read_Size++] = (c)))

static void Read_Grow () {
    Read_Max *= 2;
    Read_Buf = Safe_Realloc (Read_Buf, Read_Max);
}

Object General_Read(), Read_Sequence(), Read_Atom(), Read_Special();
Object Read_String(), Read_Sharp(), Read_True(), Read_False(), Read_Void();
Object Read_Kludge(), Read_Vector_Paren(), Read_Vector_Bracket(), Read_Radix(), Read_Char();

void Init_Read () {
    Define_Symbol (&Sym_Quote, "quote");
    Define_Symbol (&Sym_Quasiquote, "quasiquote");
    Define_Symbol (&Sym_Unquote, "unquote");
    Define_Symbol (&Sym_Unquote_Splicing, "unquote-splicing");

    Readers['t'] = Readers['T'] = Read_True;
    Readers['f'] = Readers['F'] = Read_False;
    Readers['v'] = Readers['V'] = Read_Void;
    Readers['!'] = Read_Kludge;  /* for interpreter files */
    Readers['('] = Read_Vector_Paren;
    Readers['['] = Read_Vector_Bracket;
    Readers['b'] = Readers['B'] =
    Readers['o'] = Readers['O'] =
    Readers['d'] = Readers['D'] =
    Readers['x'] = Readers['X'] =
    Readers['e'] = Readers['E'] =
    Readers['i'] = Readers['I'] = Read_Radix;
    Readers['\\'] = Read_Char;

    Read_Max = 128;
    Read_Buf = Safe_Malloc (Read_Max);
}

int String_Getc (Object port) {
    register struct S_Port *p;
    register struct S_String *s;

    p = PORT(port);
    if (p->flags & P_UNREAD) {
        p->flags &= ~P_UNREAD;
        return p->unread;
    }
    s = STRING(p->name);
    return p->ptr >= s->size ? EOF : s->data[p->ptr++];
}

void String_Ungetc (Object port, register int c) {
    PORT(port)->flags |= P_UNREAD;
    PORT(port)->unread = c;
}

void Check_Input_Port (Object port) {
    Check_Type (port, T_Port);
    if (!(PORT(port)->flags & P_OPEN))
        Primitive_Error ("port has been closed: ~s", port);
    if (!IS_INPUT(port))
        Primitive_Error ("not an input port: ~s", port);
}

Object P_Clear_Input_Port (int argc, Object *argv) {
    Discard_Input (argc == 1 ? argv[0] : Curr_Input_Port);
    return Void;
}

void Discard_Input (Object port) {
    register FILE *f;

    Check_Input_Port (port);
    if (PORT(port)->flags & P_STRING)
        return;
    f = PORT(port)->file;
#if defined(HAVE_FPURGE)
    (void)fpurge (f);
#elif defined(HAVE_BSD_FLUSH)
    f->_cnt = 0;
    f->_ptr = f->_base;
#endif

#if defined(TIOCFLUSH)
    (void)ioctl (fileno (f), TIOCFLUSH, (char *)0);
#elif defined(TCFLSH)
    (void)ioctl (fileno (f), TCFLSH, (char *)0);
#endif
}

Object P_Unread_Char (int argc, Object *argv) {
    Object port, ch;
    register struct S_Port *p;

    ch = argv[0];
    Check_Type (ch, T_Character);
    port = argc == 2 ? argv[1] : Curr_Input_Port;
    Check_Input_Port (port);
    p = PORT(port);
    if (p->flags & P_STRING) {
        if (p->flags & P_UNREAD)
            Primitive_Error ("cannot push back more than one char");
        String_Ungetc (port, CHAR(ch));
    } else {
        if (ungetc (CHAR(ch), p->file) == EOF)
            Primitive_Error ("failed to push back char");
    }
    if (CHAR(ch) == '\n' && PORT(port)->lno > 1) PORT(port)->lno--;
    return ch;
}

Object P_Read_Char (int argc, Object *argv) {
    Object port;
    register FILE *f;
    register int c, str, flags;

    port = argc == 1 ? argv[0] : Curr_Input_Port;
    Check_Input_Port (port);
    f = PORT(port)->file;
    flags = PORT(port)->flags;
    str = flags & P_STRING;
    Reader_Getc;
    Reader_Tweak_Stream;
    return c == EOF ? Eof : Make_Char (c);
}

Object P_Peek_Char (int argc, Object *argv) {
    Object a[2];

    a[0] = P_Read_Char (argc, argv);
    if (argc == 1)
        a[1] = argv[0];
    return EQ(a[0], Eof) ? Eof : P_Unread_Char (argc+1, a);
}

/* char-ready? cannot be implemented correctly based on FILE pointers.
 * The following is only an approximation; even if FIONREAD is supported,
 * the primitive may return #f although a call to read-char would not block.
 */
Object P_Char_Readyp (int argc, Object *argv) {
    Object port;

    port = argc == 1 ? argv[0] : Curr_Input_Port;
    Check_Input_Port (port);
    if (PORT(port)->flags & P_STRING || feof (PORT(port)->file))
        return True;
#ifdef FIONREAD
    {
        long num = 0;
        (void)ioctl (fileno (PORT(port)->file), FIONREAD, (char *)&num);
        if (num != 0)
            return True;
    }
#endif
    return False;
}

Object P_Read_String (int argc, Object *argv) {
    Object port;
    register FILE *f;
    register int c, str;

    port = argc == 1 ? argv[0] : Curr_Input_Port;
    Check_Input_Port (port);
    f = PORT(port)->file;
    str = PORT(port)->flags & P_STRING;
    Read_Reset ();
    while (1) {
        Reader_Getc;
        if (c == '\n' || c == EOF)
            break;
        Read_Store (c);
    }
    Reader_Tweak_Stream;
    return c == EOF ? Eof : Make_String (Read_Buf, Read_Size);
}

Object P_Read (int argc, Object *argv) {
    return General_Read (argc == 1 ? argv[0] : Curr_Input_Port, 0);
}

Object General_Read (Object port, int konst) {
    register FILE *f;
    register int c, str;
    Object ret;

    Check_Input_Port (port);
    Flush_Output (Curr_Output_Port);
    f = PORT(port)->file;
    str = PORT(port)->flags & P_STRING;
    while (1) {
        Reader_Getc;
        if (c == EOF) {
            ret = Eof;
            break;
        }
        if (Whitespace (c))
            continue;
        if (c == ';') {
comment:
            if (Skip_Comment (port) == EOF) {
                ret = Eof;
                break;
            }
            continue;
        }
        if (c == '(' || c == '[') {
            ret = Read_Sequence (port, 0, konst, c);
        } else if (c == '#') {
            ret = Read_Sharp (port, konst);
            if (TYPE(ret) == T_Special)      /* it was a #! */
                goto comment;
        } else {
            Reader_Ungetc;
            ret = Read_Atom (port, konst);
        }
        break;
    }
    Reader_Tweak_Stream;
    return ret;
}

int Skip_Comment (Object port) {
    register FILE *f;
    register int c, str;

    f = PORT(port)->file;
    str = PORT(port)->flags & P_STRING;
    do {
        Reader_Getc;
    } while (c != '\n' && c != EOF);
    return c;
}

Object Read_Atom (Object port, int konst) {
    Object ret;

    ret = Read_Special (port, konst);
    if (TYPE(ret) == T_Special)
        Reader_Error (port, "syntax error");
    return ret;
}

Object Read_Special (Object port, int konst) {
    Object ret;
    register int c, str;
    register FILE *f;

#define READ_QUOTE(sym) \
    ( ret = Read_Atom (port, konst),\
      konst ? (ret = Const_Cons (ret, Null), Const_Cons (sym, ret))\
           : (ret = Cons (ret, Null), Cons (sym, ret)))

    f = PORT(port)->file;
    str = PORT(port)->flags & P_STRING;
again:
    Reader_Getc;
    switch (c) {
    case EOF:
eof:
        Reader_Tweak_Stream;
        Reader_Error (port, "premature end of file");
    case ';':
        if (Skip_Comment (port) == EOF)
            goto eof;
        goto again;
    case ']':
    case ')':
        SET(ret, T_Special, c);
        return ret;
    case '[':
    case '(':
        return Read_Sequence (port, 0, konst, c);
    case '\'':
        return READ_QUOTE(Sym_Quote);
    case '`':
        return READ_QUOTE(Sym_Quasiquote);
    case ',':
        Reader_Getc;
        if (c == EOF)
            goto eof;
        if (c == '@') {
            return READ_QUOTE(Sym_Unquote_Splicing);
        } else {
            Reader_Ungetc;
            return READ_QUOTE(Sym_Unquote);
        }
    case '"':
        return Read_String (port, konst);
    case '#':
        ret = Read_Sharp (port, konst);
        if (TYPE(ret) == T_Special)
            goto again;
        return ret;
    default:
        if (Whitespace (c))
            goto again;
        Read_Reset ();
        if (c == '.') {
            Reader_Getc;
            if (c == EOF)
                goto eof;
            if (Whitespace (c)) {
                Reader_Ungetc;
                SET(ret, T_Special, '.');
                return ret;
            }
            Read_Store ('.');
        }
        while (!Whitespace (c) && !Delimiter (c) && c != EOF) {
            if (c == '\\') {
                Reader_Getc;
                if (c == EOF)
                    break;
            }
            Read_Store (c);
            Reader_Getc;
        }
        Read_Store ('\0');
        if (c != EOF)
            Reader_Ungetc;
        ret = Parse_Number (port, Read_Buf, 10);
        if (Nullp (ret))
            ret = Intern (Read_Buf);
        return ret;
    }
    /*NOTREACHED*/
}

Object Read_Sequence (Object port, int vec, int konst, int start_chr) {
    Object ret, e, tail, t;
    GC_Node3;

    ret = tail = Null;
    GC_Link3 (ret, tail, port);
    while (1) {
        e = Read_Special (port, konst);
        if (TYPE(e) == T_Special) {
            if (CHAR(e) == ')' || CHAR(e) == ']') {
                if ((start_chr == '(' && CHAR(e) == ']')
                      || (start_chr == '[' && CHAR(e) == ')')) {
                    char buf[64];
                    sprintf(buf, "expression starts with '%c' but ends "
                                 "with '%c'", start_chr, CHAR(e));
                    Reader_Error (port, buf);
                }
                GC_Unlink;
                return ret;
            }
            if (vec)
                Reader_Error (port, "wrong syntax in vector");
            if (CHAR(e) == '.') {
                if (Nullp (tail)) {
                    ret = Read_Atom (port, konst);
                } else {
                    e = Read_Atom (port, konst);
                    /*
                     * Possibly modifying pure cons.  Must be fixed!
                     */
                    Cdr (tail) = e;
                }
                e = Read_Special (port, konst);
                if (TYPE(e) == T_Special && (CHAR(e) == ')' || CHAR(e) == ']')) {
                    GC_Unlink;
                    return ret;
                }
                Reader_Error (port, "dot in wrong context");
            }
            Reader_Error (port, "syntax error");
        }
        if (konst) t = Const_Cons (e, Null); else t = Cons (e, Null);
        if (!Nullp (tail))
            /*
             * Possibly modifying pure cons.  Must be fixed!
             */
            Cdr (tail) = t;
        else
            ret = t;
        tail = t;
    }
    /*NOTREACHED*/
}

Object Read_String (Object port, int konst) {
    register FILE *f;
    register int n, c, oc, str;

    Read_Reset ();
    f = PORT(port)->file;
    str = PORT(port)->flags & P_STRING;
    while (1) {
        Reader_Getc;
        if (c == EOF) {
eof:
            Reader_Tweak_Stream;
            Reader_Error (port, "end of file in string");
        }
        if (c == '\\') {
            Reader_Getc;
            switch (c) {
            case EOF: goto eof;
            case 'b': c = '\b'; break;
            case 't': c = '\t'; break;
            case 'r': c = '\r'; break;
            case 'n': c = '\n'; break;
            case '0': case '1': case '2': case '3':
            case '4': case '5': case '6': case '7':
                oc = n = 0;
                do {
                    oc <<= 3; oc += c - '0';
                    Reader_Getc;
                    if (c == EOF) goto eof;
                } while (Octal (c) && ++n <= 2);
                Reader_Ungetc;
                c = oc;
            }
        } else if (c == '"')
            break;
        Read_Store (c);
    }
    return General_Make_String (Read_Buf, Read_Size, konst);
}

Object Read_Sharp (Object port, int konst) {
    int c, str;
    FILE *f;
    char buf[32];

    f = PORT(port)->file;
    str = PORT(port)->flags & P_STRING;
    Reader_Getc;
    if (c == EOF)
        Reader_Sharp_Eof;
    if (!Readers[c]) {
        sprintf (buf, "no reader for syntax #%c", c);
        Reader_Error (port, buf);
    }
    return Readers[c](port, c, konst);
}

/*ARGSUSED*/
Object Read_True (Object port, int chr, int konst) {
    return True;
}

/*ARGSUSED*/
Object Read_False (Object port, int chr, int konst) {
    return False;
}

/*ARGSUSED*/
Object Read_Void (Object port, int chr, int konst) {
    Object ret;

    ret = Const_Cons (Void, Null);
    return Const_Cons (Sym_Quote, ret);
}

/*ARGSUSED*/
Object Read_Kludge (Object port, int chr, int konst) {
    return Special;
}

/*ARGSUSED*/
Object Read_Vector_Paren (Object port, int chr, int konst) {
    return List_To_Vector (Read_Sequence (port, 1, konst, '('), konst);
}

/*ARGSUSED*/
Object Read_Vector_Bracket (Object port, int chr, int konst) {
    return List_To_Vector (Read_Sequence (port, 1, konst, '['), konst);
}

/*ARGSUSED*/
Object Read_Radix (Object port, int chr, int konst) {
    int c, str;
    FILE *f;
    Object ret;

    Read_Reset ();
    f = PORT(port)->file;
    str = PORT(port)->flags & P_STRING;
    Read_Store ('#'); Read_Store (chr);
    while (1) {
        Reader_Getc;
        if (c == EOF)
            Reader_Sharp_Eof;
        if (Whitespace (c) || Delimiter (c))
            break;
        Read_Store (c);
    }
    Reader_Ungetc;
    Read_Store ('\0');
    ret = Parse_Number (port, Read_Buf, 10);
    if (Nullp (ret))
        Reader_Error (port, "radix not followed by a valid number");
    return ret;
}

/*ARGSUSED*/
Object Read_Char (Object port, int chr, int konst) {
    int c, str;
    FILE *f;
    char buf[10], *p = buf;

    f = PORT(port)->file;
    str = PORT(port)->flags & P_STRING;
    Reader_Getc;
    if (c == EOF)
        Reader_Sharp_Eof;
    *p++ = c;
    while (1) {
        Reader_Getc;
        if (c == EOF)
            Reader_Sharp_Eof;
        if (Whitespace (c) || Delimiter (c))
            break;
        if (p == buf+9)
            Reader_Error (port, "syntax error in character constant");
        *p++ = c;
    }
    Reader_Ungetc;
    if (p == buf+1)
        return Make_Char (*buf);
    *p = '\0';
    if (p == buf+3) {
        for (c = 0, p = buf; p < buf+3 && Octal (*p); p++)
            c = c << 3 | (*p - '0');
        if (p == buf+3)
            return Make_Char (c);
    }
    for (p = buf; *p; p++)
        if (isupper (*p))
            *p = tolower (*p);
    if (strcmp (buf, "space") == 0)
        return Make_Char (' ');
    if (strcmp (buf, "newline") == 0)
        return Make_Char ('\n');
    if (strcmp (buf, "return") == 0)
        return Make_Char ('\r');
    if (strcmp (buf, "tab") == 0)
        return Make_Char ('\t');
    if (strcmp (buf, "formfeed") == 0)
        return Make_Char ('\f');
    if (strcmp (buf, "backspace") == 0)
        return Make_Char ('\b');
    Reader_Error (port, "syntax error in character constant");
    /*NOTREACHED*/
}

void Define_Reader (int c, READFUN fun) {
    if (Readers[c] && Readers[c] != fun)
        Primitive_Error ("reader for `~a' already defined", Make_Char (c));
    Readers[c] = fun;
}

Object Parse_Number (Object port, char const *buf, int radix) {
    char const *p;
    int c, i;
    int mdigit = 0, edigit = 0, expo = 0, neg = 0, point = 0;
    int gotradix = 0, exact = 0, inexact = 0;
    unsigned int max;
    int maxdig;
    Object ret;

    for ( ; *buf == '#'; buf++) {
        switch (*++buf) {
        case 'b': case 'B':
            if (gotradix++) return Null;
            radix = 2;
            break;
        case 'o': case 'O':
            if (gotradix++) return Null;
            radix = 8;
            break;
        case 'd': case 'D':
            if (gotradix++) return Null;
            radix = 10;
            break;
        case 'x': case 'X':
            if (gotradix++) return Null;
            radix = 16;
            break;
        case 'e': case 'E':
            if (exact++ || inexact) return Null;
            break;
        case 'i': case 'I':
            if (inexact++ || exact) return Null;
            break;
        default:
            return Null;
        }
    }
    p = buf;
    if (*p == '+' || (neg = *p == '-'))
        p++;
    for ( ; (c = *p); p++) {
        if (c == '.') {
            if (expo || point++)
                return Null;
        } else if (radix != 16 && (c == 'e' || c == 'E')) {
            if (expo++)
                return Null;
            if (p[1] == '+' || p[1] == '-')
                p++;
#ifdef HAVE_INDEX
        } else if (radix == 16 && !index ("0123456789abcdefABCDEF", c)) {
#else
        } else if (radix == 16 && !strchr ("0123456789abcdefABCDEF", c)) {
#endif
            return Null;
        } else if (radix < 16 && (c < '0' || c > '0' + radix-1)) {
            return Null;
        } else {
            if (expo) edigit++; else mdigit++;
        }
    }
    if (!mdigit || (expo && !edigit))
        return Null;
    if (point || expo) {
        if (radix != 10) {
            if (Nullp (port))
                return Null;
            Reader_Error (port, "reals must be given in decimal");
        }
        /* Lacking ratnums, there's nothing we can do if #e has been
         * specified-- just return the inexact number.
         */
        return Make_Flonum (atof (buf));
    }
    max = (neg ? -(unsigned int)INT_MIN : INT_MAX);
    maxdig = max % radix;
    max /= radix;
    for (i = 0, p = buf; (c = *p); p++) {
        if (c == '-' || c == '+') {
            buf++;
            continue;
        }
        if (radix == 16) {
            if (isupper (c))
                c = tolower (c);
            if (c >= 'a')
                c = '9' + c - 'a' + 1;
        }
        c -= '0';
        if ((unsigned int)i > max || ((unsigned int)i == max && c > maxdig)) {
            ret = Make_Bignum (buf, neg, radix);
            return inexact ? Make_Flonum (Bignum_To_Double (ret)) : ret;
        }
        i *= radix; i += c;
    }
    if (neg)
        i = -i;
    return inexact ? Make_Flonum ((double)i) : Make_Integer (i);
}

void Reader_Error (Object port, char *msg) {
    char buf[100];

    if (PORT(port)->flags & P_STRING) {
        sprintf (buf, "[string-port]: %u: %s", PORT(port)->lno, msg);
        Primitive_Error (buf);
    } else {
        sprintf (buf, "~s: %u: %s", PORT(port)->lno, msg);
        Primitive_Error (buf, PORT(port)->name);
    }
}
