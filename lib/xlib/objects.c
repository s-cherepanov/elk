/* objects.c
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

#include <stdarg.h>

#include "xlib.h"

Object Sym_None;

int Match_X_Obj (Object x, va_list v) {
    register int type = TYPE(x);

    if (type == T_Display) {
        return 1;
    } else if (type == T_Gc) {
        return va_arg (v, GC) == GCONTEXT(x)->gc;
    } else if (type == T_Pixel) {
        return va_arg (v, unsigned long) == PIXEL(x)->pix;
    } else if (type == T_Pixmap) {
        return va_arg (v, Pixmap) == PIXMAP(x)->pm;
    } else if (type == T_Window) {
        return va_arg (v, Window) == WINDOW(x)->win;
    } else if (type == T_Font) {
        return va_arg (v, Font) == FONT(x)->id;
    } else if (type == T_Colormap) {
        return va_arg (v, Colormap) == COLORMAP(x)->cm;
    } else if (type == T_Color) {
        return va_arg (v, unsigned int) == COLOR(x)->c.red
            && va_arg (v, unsigned int) == COLOR(x)->c.green
            && va_arg (v, unsigned int) == COLOR(x)->c.blue;
    } else if (type == T_Cursor) {
        return va_arg (v, Cursor) == CURSOR(x)->cursor;
    } else if (type == T_Atom) {
        return va_arg (v, Atom) == ATOM(x)->atom;
    } else Panic ("Match_X_Obj");
    return 0;
}

void elk_init_xlib_objects () {
    Define_Symbol (&Sym_None, "none");
}
