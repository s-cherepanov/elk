/* pixmap.c
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

#include "xlib.h"

Generic_Predicate (Pixmap)

Generic_Equal_Dpy (Pixmap, PIXMAP, pm)

Generic_Print (Pixmap, "#[pixmap %lu]", PIXMAP(x)->pm)

Generic_Get_Display (Pixmap, PIXMAP)

static Object Internal_Make_Pixmap (int finalize, Display *dpy, Pixmap pix) {
    Object pm;

    if (pix == None)
        return Sym_None;
    pm = Find_Object (T_Pixmap, (GENERIC)dpy, Match_X_Obj, pix);
    if (Nullp (pm)) {
        pm = Alloc_Object (sizeof (struct S_Pixmap), T_Pixmap, 0);
        PIXMAP(pm)->tag = Null;
        PIXMAP(pm)->pm = pix;
        PIXMAP(pm)->dpy = dpy;
        PIXMAP(pm)->free = 0;
        Register_Object (pm, (GENERIC)dpy,
            finalize ? P_Free_Pixmap : (PFO)0, 0);
    }
    return pm;
}

/* Backwards compatibility: */
Object Make_Pixmap (Display *dpy, Pixmap pix) {
    return Internal_Make_Pixmap (1, dpy, pix);
}

Object Make_Pixmap_Foreign (Display *dpy, Pixmap pix) {
    return Internal_Make_Pixmap (0, dpy, pix);
}

Pixmap Get_Pixmap (Object p) {
    Check_Type (p, T_Pixmap);
    return PIXMAP(p)->pm;
}

Object P_Free_Pixmap (Object p) {
    Check_Type (p, T_Pixmap);
    if (!PIXMAP(p)->free)
        XFreePixmap (PIXMAP(p)->dpy, PIXMAP(p)->pm);
    Deregister_Object (p);
    PIXMAP(p)->free = 1;
    return Void;
}

static Object P_Create_Pixmap (Object d, Object w, Object h, Object depth) {
    Display *dpy;
    Drawable dr = Get_Drawable (d, &dpy);

    return Make_Pixmap (dpy, XCreatePixmap (dpy, dr, Get_Integer (w),
        Get_Integer (h), Get_Integer (depth)));
}

static Object P_Create_Bitmap_From_Data (Object win, Object data, Object pw,
                                         Object ph) {
    register int w, h;

    Check_Type (win, T_Window);
    Check_Type (data, T_String);
    w = Get_Integer (pw);
    h = Get_Integer (ph);
    if (w * h > 8 * STRING(data)->size)
        Primitive_Error ("bitmap too small");
    return Make_Pixmap (WINDOW(win)->dpy,
        XCreateBitmapFromData (WINDOW(win)->dpy, WINDOW(win)->win,
            STRING(data)->data, w, h));
}

static Object P_Create_Pixmap_From_Bitmap_Data (Object win, Object data,
                                                Object pw, Object ph,
                                                Object fg, Object bg,
                                                Object depth) {
    register int w, h;

    Check_Type (win, T_Window);
    Check_Type (data, T_String);
    w = Get_Integer (pw);
    h = Get_Integer (ph);
    if (w * h > 8 * STRING(data)->size)
        Primitive_Error ("bitmap too small");
    return Make_Pixmap (WINDOW(win)->dpy,
        XCreatePixmapFromBitmapData (WINDOW(win)->dpy, WINDOW(win)->win,
            STRING(data)->data, w, h, Get_Pixel (fg), Get_Pixel (bg),
                Get_Integer (depth)));
}

static Object P_Read_Bitmap_File (Object d, Object fn) {
    Display *dpy;
    Drawable dr = Get_Drawable (d, &dpy);
    unsigned width, height;
    int r, xhot, yhot;
    Pixmap bitmap;
    Object t, ret, x;
    GC_Node2;

    Disable_Interrupts;
    r = XReadBitmapFile (dpy, dr, Get_Strsym (fn), &width, &height, &bitmap,
        &xhot, &yhot);
    Enable_Interrupts;
    if (r != BitmapSuccess)
        return Bits_To_Symbols ((unsigned long)r, 0, Bitmapstatus_Syms);
    t = ret = P_Make_List (Make_Integer (5), Null);
    GC_Link2 (ret, t);
    x = Make_Pixmap (dpy, bitmap);
    Car (t) = x; t = Cdr (t);
    Car (t) = Make_Integer (width); t = Cdr (t);
    Car (t) = Make_Integer (height); t = Cdr (t);
    Car (t) = Make_Integer (xhot); t = Cdr (t);
    Car (t) = Make_Integer (yhot);
    GC_Unlink;
    return ret;
}

static Object P_Write_Bitmap_File (int argc, Object *argv) {
    Pixmap pm;
    int ret, xhot = -1, yhot = -1;

    pm = Get_Pixmap (argv[1]);
    if (argc == 5)
        Primitive_Error ("both x-hot and y-hot must be specified");
    if (argc == 6) {
        xhot = Get_Integer (argv[4]);
        yhot = Get_Integer (argv[5]);
    }
    Disable_Interrupts;
    ret = XWriteBitmapFile (PIXMAP(argv[1])->dpy, Get_Strsym (argv[0]), pm,
        Get_Integer (argv[2]), Get_Integer (argv[3]), xhot, yhot);
    Enable_Interrupts;
    return Bits_To_Symbols ((unsigned long)ret, 0, Bitmapstatus_Syms);
}

void elk_init_xlib_pixmap () {
    Generic_Define (Pixmap, "pixmap", "pixmap?");
    Define_Primitive (P_Pixmap_Display,    "pixmap-display",    1, 1, EVAL);
    Define_Primitive (P_Free_Pixmap,       "free-pixmap",       1, 1, EVAL);
    Define_Primitive (P_Create_Pixmap,     "create-pixmap",     4, 4, EVAL);
    Define_Primitive (P_Create_Bitmap_From_Data,
                        "create-bitmap-from-data",              4, 4, EVAL);
    Define_Primitive (P_Create_Pixmap_From_Bitmap_Data,
                        "create-pixmap-from-bitmap-data",       7, 7, EVAL);
    Define_Primitive (P_Read_Bitmap_File,  "read-bitmap-file",  2, 2, EVAL);
    Define_Primitive (P_Write_Bitmap_File, "write-bitmap-file", 4, 6, VARARGS);
}
