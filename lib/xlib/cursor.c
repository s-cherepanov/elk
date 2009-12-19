/* cursor.c
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

#include "xlib.h"

Generic_Predicate (Cursor)

Generic_Equal_Dpy (Cursor, CURSOR, cursor)

Generic_Print (Cursor, "#[cursor %lu]", CURSOR(x)->cursor)

Generic_Get_Display (Cursor, CURSOR)

static Object Internal_Make_Cursor (int finalize, Display *dpy, Cursor cursor) {
    Object c;

    if (cursor == None)
        return Sym_None;
    c = Find_Object (T_Cursor, (GENERIC)dpy, Match_X_Obj, cursor);
    if (Nullp (c)) {
        c = Alloc_Object (sizeof (struct S_Cursor), T_Cursor, 0);
        CURSOR(c)->tag = Null;
        CURSOR(c)->cursor = cursor;
        CURSOR(c)->dpy = dpy;
        CURSOR(c)->free = 0;
        Register_Object (c, (GENERIC)dpy,
            finalize ? P_Free_Cursor : (PFO)0, 0);
    }
    return c;
}

/* Backwards compatibility: */
Object Make_Cursor (Display *dpy, Cursor cursor) {
    return Internal_Make_Cursor (1, dpy, cursor);
}

Object Make_Cursor_Foreign (Display *dpy, Cursor cursor) {
    return Internal_Make_Cursor (0, dpy, cursor);
}

Cursor Get_Cursor (Object c) {
    if (EQ(c, Sym_None))
        return None;
    Check_Type (c, T_Cursor);
    return CURSOR(c)->cursor;
}

Object P_Free_Cursor (Object c) {
    Check_Type (c, T_Cursor);
    if (!CURSOR(c)->free)
        XFreeCursor (CURSOR(c)->dpy, CURSOR(c)->cursor);
    Deregister_Object (c);
    CURSOR(c)->free = 1;
    return Void;
}

static Object P_Create_Cursor (Object srcp, Object maskp, Object x, Object y,
                               Object f, Object b) {
    Pixmap sp = Get_Pixmap (srcp), mp;
    Display *d = PIXMAP(srcp)->dpy;

    mp = EQ(maskp, Sym_None) ? None : Get_Pixmap (maskp);
    return Make_Cursor (d, XCreatePixmapCursor (d, sp, mp,
        Get_Color (f), Get_Color (b), Get_Integer (x), Get_Integer (y)));
}

static Object P_Create_Glyph_Cursor (Object srcf, Object srcc, Object maskf,
                                     Object maskc, Object f, Object b) {
    Font sf = Get_Font (srcf), mf;
    Display *d = FONT(srcf)->dpy;

    mf = EQ(maskf, Sym_None) ? None : Get_Font (maskf);
    return Make_Cursor (d, XCreateGlyphCursor (d, sf, mf,
        Get_Integer (srcc), mf == None ? 0 : Get_Integer (maskc),
        Get_Color (f), Get_Color (b)));
}

static Object P_Recolor_Cursor (Object c, Object f, Object b) {
    Check_Type (c, T_Cursor);
    XRecolorCursor (CURSOR(c)->dpy, CURSOR(c)->cursor, Get_Color (f),
        Get_Color (b));
    return Void;
}

void elk_init_xlib_cursor () {
    Generic_Define (Cursor, "cursor", "cursor?");
    Define_Primitive (P_Cursor_Display, "cursor-display", 1, 1, EVAL);
    Define_Primitive (P_Free_Cursor,    "free-cursor",    1, 1, EVAL);
    Define_Primitive (P_Create_Cursor,  "create-cursor",  6, 6, EVAL);
    Define_Primitive (P_Create_Glyph_Cursor, "create-glyph-cursor",
                                                          6, 6, EVAL);
    Define_Primitive (P_Recolor_Cursor, "recolor-cursor", 3, 3, EVAL);
}
