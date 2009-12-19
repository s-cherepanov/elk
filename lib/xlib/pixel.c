/* pixel.c
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

Generic_Predicate (Pixel)

Generic_Simple_Equal (Pixel, PIXEL, pix)

Generic_Print (Pixel, "#[pixel 0x%lx]", PIXEL(x)->pix)

Object Make_Pixel (unsigned long val) {
    Object pix;

    pix = Find_Object (T_Pixel, (GENERIC)0, Match_X_Obj, val);
    if (Nullp (pix)) {
        pix = Alloc_Object (sizeof (struct S_Pixel), T_Pixel, 0);
        PIXEL(pix)->tag = Null;
        PIXEL(pix)->pix = val;
        Register_Object (pix, (GENERIC)0, (PFO)0, 0);
    }
    return pix;
}

unsigned long Get_Pixel (Object p) {
    Check_Type (p, T_Pixel);
    return PIXEL(p)->pix;
}

static Object P_Pixel_Value (Object p) {
    return Make_Unsigned_Long (Get_Pixel (p));
}

static Object P_Black_Pixel (Object d) {
    Check_Type (d, T_Display);
    return Make_Pixel (BlackPixel (DISPLAY(d)->dpy,
        DefaultScreen (DISPLAY(d)->dpy)));
}

static Object P_White_Pixel (Object d) {
    Check_Type (d, T_Display);
    return Make_Pixel (WhitePixel (DISPLAY(d)->dpy,
        DefaultScreen (DISPLAY(d)->dpy)));
}

void elk_init_xlib_pixel () {
    Generic_Define (Pixel, "pixel", "pixel?");
    Define_Primitive (P_Pixel_Value,   "pixel-value",    1, 1, EVAL);
    Define_Primitive (P_Black_Pixel,   "black-pixel",    1, 1, EVAL);
    Define_Primitive (P_White_Pixel,   "white-pixel",    1, 1, EVAL);
}
