#include "xlib.h"

Generic_Predicate (Pixmap)

Generic_Equal_Dpy (Pixmap, PIXMAP, pm)

Generic_Print (Pixmap, "#[pixmap %lu]", PIXMAP(x)->pm)

Generic_Get_Display (Pixmap, PIXMAP)

static Object Internal_Make_Pixmap (finalize, dpy, pix)
        Display *dpy; Pixmap pix; {
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
Object Make_Pixmap (dpy, pix) Display *dpy; Pixmap pix; {
    return Internal_Make_Pixmap (1, dpy, pix);
}

Object Make_Pixmap_Foreign (dpy, pix) Display *dpy; Pixmap pix; {
    return Internal_Make_Pixmap (0, dpy, pix);
}

Pixmap Get_Pixmap (p) Object p; {
    Check_Type (p, T_Pixmap);
    return PIXMAP(p)->pm;
}

Object P_Free_Pixmap (p) Object p; {
    Check_Type (p, T_Pixmap);
    if (!PIXMAP(p)->free)
        XFreePixmap (PIXMAP(p)->dpy, PIXMAP(p)->pm);
    Deregister_Object (p);
    PIXMAP(p)->free = 1;
    return Void;
}

static Object P_Create_Pixmap (d, w, h, depth) Object d, w, h, depth; {
    Display *dpy;
    Drawable dr = Get_Drawable (d, &dpy);

    return Make_Pixmap (dpy, XCreatePixmap (dpy, dr, Get_Integer (w),
        Get_Integer (h), Get_Integer (depth)));
}

static Object P_Create_Bitmap_From_Data (win, data, pw, ph)
        Object win, data, pw, ph; {
    register w, h;

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

static Object P_Create_Pixmap_From_Bitmap_Data (win, data, pw, ph, fg, bg,
        depth) Object win, data, pw, ph, fg, bg, depth; {
    register w, h;

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

static Object P_Read_Bitmap_File (d, fn) Object d, fn; {
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

static Object P_Write_Bitmap_File (argc, argv) Object *argv; {
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

elk_init_xlib_pixmap () {
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
