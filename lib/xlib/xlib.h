#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

#undef True
#undef False

#ifndef NeedFunctionPrototypes          /* Kludge */
 #error "X11 Release 3 (or earlier) no longer supported"
#endif

#if XlibSpecificationRelease >= 5
#  define XLIB_RELEASE_5_OR_LATER
#endif

#if XlibSpecificationRelease >= 6
#  define XLIB_RELEASE_6_OR_LATER
#endif

#include "scheme.h"

extern int T_Display;
extern int T_Gc;
extern int T_Pixel;
extern int T_Pixmap;
extern int T_Window;
extern int T_Font;
extern int T_Colormap;
extern int T_Color;
extern int T_Cursor;
extern int T_Atom;

#define DISPLAY(x)   ((struct S_Display *)POINTER(x))
#define GCONTEXT(x)  ((struct S_Gc *)POINTER(x))
#define PIXEL(x)     ((struct S_Pixel *)POINTER(x))
#define PIXMAP(x)    ((struct S_Pixmap *)POINTER(x))
#define WINDOW(x)    ((struct S_Window *)POINTER(x))
#define FONT(x)      ((struct S_Font *)POINTER(x))
#define COLORMAP(x)  ((struct S_Colormap *)POINTER(x))
#define COLOR(x)     ((struct S_Color *)POINTER(x))
#define CURSOR(x)    ((struct S_Cursor *)POINTER(x))
#define ATOM(x)      ((struct S_Atom *)POINTER(x))

struct S_Display {
    Object after;
    Display *dpy;
    char free;
};

struct S_Gc {
    Object tag;
    GC gc;
    Display *dpy;
    char free;
};

struct S_Pixel {
    Object tag;
    unsigned long pix;
};

struct S_Pixmap {
    Object tag;
    Pixmap pm;
    Display *dpy;
    char free;
};

struct S_Window {
    Object tag;
    Window win;
    Display *dpy;
    char free;
    char finalize;
};

struct S_Font {
    Object name;
    Font id;
    XFontStruct *info;
    Display *dpy;
};

struct S_Colormap {
    Object tag;
    Colormap cm;
    Display *dpy;
    char free;
};

struct S_Color {
    Object tag;
    XColor c;
};

struct S_Cursor {
    Object tag;
    Cursor cursor;
    Display *dpy;
    char free;
};

struct S_Atom {
    Object tag;
    Atom atom;
};

enum Type {
    T_NONE,
    T_INT, T_CHAR, T_PIXEL, T_PIXMAP, T_BOOL, T_FONT, T_COLORMAP, T_CURSOR,
    T_WINDOW, T_MASK, T_SYM, T_SHORT, T_BACKGROUND, T_BORDER
};

typedef struct {
    char *slot;
    char *name;
    enum Type type;
    SYMDESCR *syms;
    int mask;
} RECORD;

typedef struct {
    Window root;
    int x, y, width, height, border_width, depth;
} GEOMETRY;

C_LINKAGE_BEGIN

extern Colormap Get_Colormap P_((Object));
extern Cursor Get_Cursor P_((Object));
extern Drawable Get_Drawable P_((Object, Display**));
extern Font Get_Font P_((Object));
extern int Get_Screen_Number P_((Display*, Object));
extern Object Get_Event_Args P_((XEvent*));
extern Pixmap Get_Pixmap P_((Object));
extern Time Get_Time P_((Object));
extern Window Get_Window P_((Object));
extern XColor *Get_Color P_((Object));
extern unsigned long Get_Pixel P_((Object));
extern void Destroy_Event_Args P_((Object));
extern int Encode_Event P_((Object));
extern int Match_X_Obj P_((ELLIPSIS));
extern void Open_Font_Maybe P_((Object));
extern Object Make_Atom P_((Atom));
extern Object Make_Color P_((unsigned int, unsigned int, unsigned int));
extern Object Make_Colormap P_((int, Display*, Colormap));
extern Object Make_Cursor P_((Display*, Cursor));
extern Object Make_Cursor_Foreign P_((Display*, Cursor));
extern Object Make_Display P_((int, Display*));
extern Object Make_Font P_((Display*, Object, Font, XFontStruct*));
extern Object Make_Font_Foreign P_((Display*, Object, Font, XFontStruct*));
extern Object Make_Gc P_((int, Display*, GC));
extern Object Make_Pixel P_((unsigned long));
extern Object Make_Pixmap P_((Display*, Pixmap));
extern Object Make_Pixmap_Foreign P_((Display*, Pixmap));
extern Object Make_Window P_((int, Display*, Window));
extern Object P_Close_Display P_((Object));
extern Object P_Close_Font P_((Object));
extern Object P_Destroy_Window P_((Object));
extern Object P_Free_Colormap P_((Object));
extern Object P_Free_Cursor P_((Object));
extern Object P_Free_Gc P_((Object));
extern Object P_Free_Pixmap P_((Object));
extern Object P_Window_Unique_Id P_((Object));
extern Object Record_To_Vector
    P_((RECORD*, int, Object, Display*, unsigned long));
extern unsigned long Vector_To_Record P_((Object, int, Object, RECORD*));

C_LINKAGE_END

extern XSetWindowAttributes SWA;
extern XWindowChanges WC;
extern XGCValues GCV;
extern GEOMETRY GEO;
extern XWindowAttributes WA;
extern XFontStruct FI;
extern XCharStruct CI;
extern XWMHints WMH;
extern XSizeHints SZH;

extern Set_Attr_Size, Conf_Size, GC_Size, Geometry_Size, Win_Attr_Size,
    Font_Info_Size, Char_Info_Size, Wm_Hints_Size, Size_Hints_Size;
extern RECORD Set_Attr_Rec[], Conf_Rec[], GC_Rec[], Geometry_Rec[],
    Win_Attr_Rec[], Font_Info_Rec[], Char_Info_Rec[], Wm_Hints_Rec[],
    Size_Hints_Rec[];

extern SYMDESCR Func_Syms[], Bit_Grav_Syms[], Event_Syms[], Error_Syms[],
    Grav_Syms[], Backing_Store_Syms[], Class_Syms[], Stack_Mode_Syms[],
    Line_Style_Syms[], State_Syms[], Cap_Style_Syms[], Join_Style_Syms[],
    Map_State_Syms[], Fill_Style_Syms[], Fill_Rule_Syms[], Arc_Mode_Syms[],
    Subwin_Mode_Syms[], Button_Syms[], Cross_Mode_Syms[], Cross_Detail_Syms[],
    Focus_Detail_Syms[], Place_Syms[], Visibility_Syms[], Prop_Syms[],
    Mapping_Syms[], Direction_Syms[], Shape_Syms[], Propmode_Syms[],
    Grabstatus_Syms[], Allow_Events_Syms[], Revert_Syms[], Polyshape_Syms[],
    Initial_State_Syms[], Bitmapstatus_Syms[], Circulate_Syms[],
    Ordering_Syms[], Byte_Order_Syms[], Saveset_Syms[], Closemode_Syms[];

extern Object Sym_None, Sym_Now, Sym_Char_Info, Sym_Conf;


#if __STDC__ || defined(ANSI_CPP)
#  define conc(a,b) a##b
#  define conc3(a,b,c) a##b##c
#else
#  define _identity(x) x
#  define conc(a,b) _identity(a)b
#  define conc3(a,b,c) conc(conc(a,b),c)
#endif


/* Generic_Predicate (Pixmap) generates:
 *
 *   int T_Pixmap;
 *
 *   static Object P_Pixmapp (x) Object x; {
 *        return TYPE(x) == T_Pixmap ? True : False;
 *   }
 */
#define Generic_Predicate(type) int conc(T_,type);\
\
static Object conc3(P_,type,p) (x) Object x; {\
    return TYPE(x) == conc(T_,type) ? True : False;\
}

/* Generic_Equal (Pixmap, PIXMAP, pm) generates:
 *
 *    static Pixmap_Equal (x, y) Object x, y; {
 *        return PIXMAP(x)->pm == PIXMAP(y)->field
 *            && !PIXMAP(x)->free && !PIXMAP(y)->free;
 *    }
 */
#define Generic_Equal(type,cast,field) static conc(type,_Equal) (x, y)\
	Object x, y; {\
    return cast(x)->field == cast(y)->field\
	&& !cast(x)->free && !cast(y)->free;\
}

/* Same as above, but doesn't check for ->free:
 */
#define Generic_Simple_Equal(type,cast,field) static conc(type,_Equal) (x, y)\
	Object x, y; {\
    return cast(x)->field == cast(y)->field;\
}

/* Same as above, but also checks ->dpy
 */
#define Generic_Equal_Dpy(type,cast,field) static conc(type,_Equal)\
	    (x, y)\
	Object x, y; {\
    return cast(x)->field == cast(y)->field && cast(x)->dpy == cast(y)->dpy\
	&& !cast(x)->free && !cast(y)->free;\
}

/* Generic_Print (Pixmap, "#[pixmap %u]", PIXMAP(x)->pm) generates:
 *
 *    static Pixmap_Print (x, port, raw, depth, len) Object x, port; {
 *        Printf (port, "#[pixmap %u]", PIXMAP(x)->pm);
 *    }
 */
#define Generic_Print(type,fmt,how) static conc(type,_Print)\
	(x, port, raw, depth, len) Object x, port; {\
    Printf (port, fmt, (unsigned)how);\
}

/* Generic_Define (Pixmap, "pixmap", "pixmap?") generates:
 *
 *    T_Pixmap = Define_Type (0, "pixmap", NOFUNC, sizeof (struct S_Pixmap),
 *        Pixmap_Equal, Pixmap_Equal, Pixmap_Print, NOFUNC);
 *    Define_Primitive (P_Pixmapp, "pixmap?", 1, 1, EVAL);
 */
#define Generic_Define(type,name,pred) conc(T_,type) =\
    Define_Type (0, name, NOFUNC, sizeof (struct conc(S_,type)),\
	conc(type,_Equal), conc(type,_Equal), conc(type,_Print), NOFUNC);\
    Define_Primitive (conc3(P_,type,p), pred, 1, 1, EVAL);

/* Generic_Get_Display (Pixmap, PIXMAP) generates:
 *
 *    static Object P_Pixmap_Display (x) Object x; {
 *        Check_Type (x, T_Pixmap);
 *        return Make_Display (PIXMAP(x)->dpy);
 *    }
 */
#define Generic_Get_Display(type,cast) static Object conc3(P_,type,_Display)\
	(x) Object x; {\
    Check_Type (x, conc(T_,type));\
    return Make_Display (0, cast(x)->dpy);\
}
