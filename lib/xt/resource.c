#include "xt.h"

#include <ctype.h>

#define   XtRChar              "Char"
#define   XtRGC                "GC"
#define   XtRBackingStore      "BackingStore"

#define T_Unknown            -1
#define T_String_Or_Symbol   -2
#define T_Callbacklist       -3
#define T_Float              -4
#define T_Backing_Store      -5
#define T_Dimension          -6
#define T_Translations       -7
#define T_Position           -8
#define T_Bitmap             -9
#define T_Cardinal           -10
#define T_Accelerators       -11

static Resource_To_Scheme_Type (t) register char *t; {
    if (streq (XtRAcceleratorTable, t))
	return T_Accelerators;
    else if (streq (XtRBackingStore, t))
	return T_Backing_Store;
    else if (streq (XtRBitmap, t))
	return T_Bitmap;
    else if (streq (XtRBoolean, t))
	return T_Boolean;
    else if (streq (XtRCallback, t))
	return T_Callbacklist;
    else if (streq (XtRCardinal, t))
	return T_Cardinal;
    else if (streq (XtRColormap, t))
	return T_Colormap;
    else if (streq (XtRCursor, t))
	return T_Cursor;
    else if (streq (XtRDimension, t))
	return T_Dimension;
    else if (streq (XtRDisplay, t))
	return T_Display;
    else if (streq (XtRFloat, t))
	return T_Float;
    else if (streq (XtRFont, t))
	return T_Font;
    else if (streq (XtRFontStruct, t))
	return T_Font;
    else if (streq (XtRGC, t))
	return T_Gc;
    else if (streq (XtRInt, t))
	return T_Fixnum;
    else if (streq (XtRPixel, t))
	return T_Pixel;
    else if (streq (XtRPixmap, t))
	return T_Pixmap;
    else if (streq (XtRPosition, t))
	return T_Position;
    else if (streq (XtRShort, t))
	return T_Fixnum;
    else if (streq (XtRString, t))
	return T_String_Or_Symbol;
    else if (streq (XtRTranslationTable, t))
	return T_Translations;
    else if (streq (XtRUnsignedChar, t))
	return T_Character;
    else if (streq (XtRChar, t))
	return T_Character;
    else if (streq (XtRWidget, t))
	return T_Widget;
    else if (streq (XtRWindow, t))
	return T_Window;
    return T_Unknown;
}

void Get_All_Resources (sub, w, c, rp, np, cp) Widget w; WidgetClass c;
	XtResource **rp; int *np, *cp; {
    XtResource *r, *sr, *cr;
    int nr, snr = 0, cnr = 0;

    XtGetResourceList (c, &r, (Cardinal *)&nr);
    if (sub)
	Get_Sub_Resource_List (c, &sr, (Cardinal *)&snr);
    if (w && XtParent (w))
	XtGetConstraintResourceList (XtClass (XtParent (w)), &cr,
	    (Cardinal *)&cnr);
    *np = nr + snr + cnr;
    *cp = cnr;
    *rp = (XtResource *)XtMalloc (*np * sizeof (XtResource));
    bcopy ((char *)r, (char *)*rp, nr * sizeof (XtResource));
    XtFree ((char *)r);
    if (snr)
	bcopy ((char *)sr, (char *)(*rp + nr), snr * sizeof (XtResource));
    if (cnr) {
	bcopy ((char *)cr, (char *)(*rp + nr+snr), cnr * sizeof (XtResource));
	XtFree ((char *)cr);
    }
}

void Convert_Args (ac, av, to, widget, class) Object *av; ArgList to;
	Widget widget; WidgetClass class; {
    register char *name, *res;
    register i, j, k;
    Object arg, val;
    XtResource *r;
    int nr, nc;
    int st, dt;
    char key[128];
    PFS2X converter;
    char *stmp;
    XrmValue src, dst;
    Alloca_Begin;

    if (ac & 1)
	Primitive_Error ("missing argument value");
    Get_All_Resources (1, widget, class, &r, &nr, &nc);
    /* Note:
     * `r' is not freed in case of error.
     */
    for (i = k = 0; k < ac; i++, k++) {
	arg = av[k];
	Get_Strsym_Stack (arg, name);
	Make_Resource_Name (name);
	for (j = 0; j < nr && !streq (name, r[j].resource_name); j++)
	    ;
	if (j == nr)
	    Primitive_Error ("no such resource: ~s", arg);
	if (streq (r[j].resource_class, XtCReadOnly))
	    Primitive_Error ("resource is read-only: ~s", arg);
	res = r[j].resource_name;
	val = av[++k];
	st = TYPE(val);
	dt = Resource_To_Scheme_Type (r[j].resource_type);

	/* First look for widget class specific converter for
	 * this resource, then look for a general converter
	 * (first try the name of the resource, then the type):
	 */
	if (widget && j >= nr-nc)
	    class = XtClass (XtParent (widget));
	sprintf (key, "%s-%s", Class_Name (class), name);
	converter = Find_Converter_To_C (key);
	if (converter || (converter = Find_Converter_To_C (res))
		|| (converter = Find_Converter_To_C (r[j].resource_type))) {
	    XtArgVal ret = converter (val);
	    XtSetArg (to[i], res, ret);
	} else if (dt == T_String_Or_Symbol) {
	    Get_Strsym_Stack (val, stmp);
	    XtSetArg (to[i], res, XtNewString (stmp));  /* Never freed! */
	} else if (dt == T_Callbacklist) {
	    int n;
	    XtCallbackList callbacks;

	    Check_Callback_List (val);
	    n = Fast_Length (val);
	    callbacks = (XtCallbackRec *)  /* Never freed! */
		    XtMalloc ((n+1) * sizeof (XtCallbackRec));
	    callbacks[n].callback = 0;
	    callbacks[n].closure = 0;
	    Fill_Callbacks (val, callbacks, n,
		Find_Callback_Converter (class, name, arg));
	    XtSetArg (to[i], res, callbacks);
	} else if (dt == T_Float) {
	    float f = (float)Get_Double (val);
	    to[i].name = res;
	    bcopy ((char *)&f, (char *)&to[i].value, sizeof f);
	} else if (dt == T_Dimension || dt == T_Position || dt == T_Cardinal
		|| dt == T_Fixnum) {
	    XtSetArg (to[i], res, Get_Integer (val));
	} else if (dt == T_Backing_Store) {
	    XtSetArg (to[i], res, Symbols_To_Bits (val, 0,
		Backing_Store_Syms));
	} else if (dt == T_Translations) {
	    XtSetArg (to[i], res, Get_Translations (val));
	} else if (dt == T_Accelerators) {
	    XtSetArg (to[i], res, Get_Accelerators (val));
	} else if ((dt == T_Bitmap || dt == T_Pixmap) && EQ(val, Sym_None)) {
	    XtSetArg (to[i], res, None);
	} else if (dt == T_Bitmap) {
	    /* Should check depth here (must be 1), but how? */
	    XtSetArg (to[i], res, Get_Pixmap (val));
	} else {
	    if (st != dt) {
		char msg[128];

		/* Try to let XtConvert() do the conversion.
		 */
		if (widget && (st == T_String || st == T_Symbol)) {
		    Get_Strsym_Stack (val, stmp);
		    src.size = strlen (stmp);
		    src.addr = (caddr_t)stmp;
		    XtConvert (widget, (String)XtRString, &src,
			r[j].resource_type, &dst);
		    if (dst.addr) {
			if (dst.size == (sizeof (unsigned char))) {
			    XtSetArg (to[i], res, *(unsigned char *)dst.addr);
			} else if (dst.size == sizeof (int)) {
			    XtSetArg (to[i], res, *(int *)dst.addr);
			} else if (dst.size == sizeof (XtArgVal)) {
			    XtSetArg (to[i], res, *(XtArgVal *)dst.addr);
			} else {
			    sprintf (msg,
				"%s: converter for %s returned weird size %d",
				name, r[j].resource_type, dst.size);
			    Primitive_Error (msg);
			}
			goto done;
		    }
		}
		sprintf (msg, "%s: can't convert %s ~s to %s", name,
		    Types[st].name, r[j].resource_type);
		Primitive_Error (msg, val);
	    }
	    if (dt == T_Boolean) {
		XtSetArg (to[i], res, EQ(val, True));
	    } else if (dt == T_Colormap) {
		XtSetArg (to[i], res, COLORMAP(val)->cm);
	    } else if (dt == T_Cursor) {
		XtSetArg (to[i], res, CURSOR(val)->cursor);
	    } else if (dt == T_Display) {
		XtSetArg (to[i], res, DISPLAY(val)->dpy);
	    } else if (dt == T_Font) {
		Open_Font_Maybe (val);
		if (streq (r[j].resource_type, XtRFontStruct))
		    XtSetArg (to[i], res, FONT(val)->info);
		else
		    XtSetArg (to[i], res, FONT(val)->id);
	    } else if (dt == T_Pixel) {
		XtSetArg (to[i], res, PIXEL(val)->pix);
	    } else if (dt == T_Pixmap) {
		XtSetArg (to[i], res, PIXMAP(val)->pm);
	    } else if (dt == T_Gc) {
		XtSetArg (to[i], res, GCONTEXT(val)->gc);
	    } else if (dt == T_Character) {
		XtSetArg (to[i], res, CHAR(val));
	    } else if (dt == T_Widget) {
		XtSetArg (to[i], res, WIDGET(val)->widget);
	    } else if (dt == T_Window) {
		XtSetArg (to[i], res, WINDOW(val)->win);
	    } else Panic ("bad conversion type");
	}
done: ;
    }
    Alloca_End;
    XtFree ((char *)r);
}

Object Get_Values (w, ac, av) Widget w; Object *av; {
    register char *name;
    register i, j;
    Object arg;
    XtResource *r;
    int nr, nc;
    int t;
    ArgList argl;
    Object ret, tail;
    Display *dpy;
    char key[128];
    PFX2S converter;
    Widget w2;
    GC_Node2;
    Alloca_Begin;

    Alloca (argl, Arg*, ac * sizeof (Arg));
    Get_All_Resources (0, w, XtClass (w), &r, &nr, &nc);
    /* Note:
     * `r' is not freed in case of error.
     */
    for (i = 0; i < ac; i++) {
	XtArgVal argval;

	arg = av[i];
	Check_Type (arg, T_Symbol);
	Get_Strsym_Stack (arg, name);
	Make_Resource_Name (name);
	for (j = 0; j < nr && !streq (name, r[j].resource_name); j++)
	    ;
	if (j == nr)
	    Primitive_Error ("no such resource: ~s", arg);
	argl[i].name = name;
	Alloca (argval, XtArgVal, r[j].resource_size);
	argl[i].value = argval;
    }
    XtGetValues (w, argl, (Cardinal)ac);
    ret = tail = P_Make_List (Make_Integer (ac), Null);
    GC_Link2 (ret, tail);
    /*
     * Display is needed for resources like cursor and pixmap.
     * XtDisplayOfObject(w) is not necessarily the right one!
     */
    dpy = XtDisplayOfObject (w);
    for (i = 0; i < ac; i++, tail = Cdr (tail)) {
	Object o;
	XtArgVal val = argl[i].value;
	for (j = 0; j < nr && !streq (argl[i].name, r[j].resource_name); j++)
	    ;
	t = Resource_To_Scheme_Type (r[j].resource_type);

	/* Look for a widget class specific converter, then for a
	 * general converter (first try the resource name, then the type):
	 */
	w2 = (j >= nr-nc) ? XtParent (w) : w;
	sprintf (key, "%s-%s", Class_Name (XtClass (w2)), argl[i].name);
	converter = Find_Converter_To_Scheme (key);

	if (converter) {
	    o = converter (*(XtArgVal *)val);
	} else if (converter = Find_Converter_To_Scheme (argl[i].name)) {
	    o = converter (*(XtArgVal *)val);
	} else if (converter = Find_Converter_To_Scheme (r[j].resource_type)) {
	    o = converter (*(XtArgVal *)val);
	} else if (t == T_String_Or_Symbol) {
	    char *s = *(char **)val;

	    if (s == 0) s = "";
	    o = Make_String (s, strlen (s));
	} else if (t == T_Callbacklist) {
	    register i, n;
	    Object ret, tail;
	    XtCallbackList callbacks = *(XtCallbackList *)val;
	    GC_Node;

	    for (n = 0; callbacks[n].callback; n++)
		;
	    ret = tail = P_Make_List (Make_Integer (n), Null);
	    GC_Link2 (ret, tail);
	    for (i = 0; i < n; i++, tail = Cdr (tail))
		Car (tail) = Get_Callbackfun (callbacks[i].closure);
	    GC_Unlink;
	    o = ret;
	} else if (t == T_Float) {
	    o = Make_Reduced_Flonum ((double)*(float *)val);
	} else if (t == T_Backing_Store) {
	    o = Bits_To_Symbols ((unsigned long)*(int *)val, 0,
		Backing_Store_Syms);
	    if (Nullp (o))
		Primitive_Error ("invalid backing-store (Xt bug)");
	} else if (t == T_Boolean) {
	    o = (Boolean)*(Boolean *)val ? True : False;
	} else if (t == T_Colormap) {
	    o = Make_Colormap (0, dpy, *(Colormap *)val);
	} else if (t == T_Cursor) {
	    o = Make_Cursor_Foreign (dpy, *(Cursor *)val);
	} else if (t == T_Gc) {
	    o = Make_Gc (0, dpy, *(GC *)val);
	} else if (t == T_Dimension) {
	    o = Make_Integer (*(Dimension *)val);
	} else if (t == T_Position) {
	    o = Make_Integer (*(Position *)val);
	} else if (t == T_Cardinal) {
	    o = Make_Unsigned (*(Cardinal *)val);
	} else if (t == T_Fixnum) {
	    if (streq (r[j].resource_type, XtRInt))
		o = Make_Integer (*(int *)val);
	    else
		o = Make_Integer (*(short *)val);
	} else if (t == T_Display) {
	    o = Make_Display (0, dpy);
	} else if (t == T_Font) {
	    if (streq (r[j].resource_type, XtRFontStruct)) {
		o = Make_Font_Foreign (dpy, False, (Font)0,
			*(XFontStruct **)val);
	    } else {
		XFontStruct *info;
		Disable_Interrupts;
		info = XQueryFont (dpy, *(Font *)val);
		Enable_Interrupts;
		o = Make_Font_Foreign (dpy, False, *(Font *)val, info);
	    }
	} else if (t == T_Pixel) {
	    o = Make_Pixel (*(unsigned long *)val);
	} else if (t == T_Pixmap || t == T_Bitmap) {
	    o = Make_Pixmap_Foreign (dpy, *(Pixmap *)val);
	} else if (t == T_Character) {
	    o = Make_Char (*(unsigned char *)val);
	} else if (t == T_Widget) {
	    o = Make_Widget_Foreign (*(Widget *)val);
	} else if (t == T_Window) {
	    o = Make_Window (0, dpy, *(Window *)val);
	} else {
	    char s[128];

	    sprintf (s, "%s: no converter for %s", argl[i].name,
		r[j].resource_type);
	    Primitive_Error (s);
	}
	Car (tail) = o;
    }
    XtFree ((char *)r);
    GC_Unlink;
    return ret;
}

/* Convert `mapped-when-managed' to `mappedWhenManaged'.
 */
void Make_Resource_Name (s) register char *s; {
    register char *p;

    for (p = s; *s; ) {
	if (*s == '-') {
	    if (*++s) {
		if (islower (*s))
		    *s = toupper (*s);
		*p++ = *s++;
	    }
	} else *p++ = *s++;
    }
    *p = '\0';
}

Object Get_Resources (c, fun, freeit) WidgetClass c; void (*fun)(); {
    XtResource *r;
    register XtResource *p;
    int nr;
    Object ret, tail, tail2, x;
    GC_Node3;

    fun (c, &r, &nr);
    /* Note:
     * `r' is not freed in case of error.
     */
    ret = tail = tail2 = P_Make_List (Make_Integer (nr), Null);
    GC_Link3 (ret, tail, tail2);
    for (p = r; p < r+nr; p++, tail = Cdr (tail)) {
	x = tail2 = P_Make_List (Make_Integer (3), Null);
	Car (tail) = tail2 = x;
	x = Intern (p->resource_name);
	Car (tail2) = x; tail2 = Cdr (tail2);
	x = Intern (p->resource_class);
	Car (tail2) = x; tail2 = Cdr (tail2);
	x = Intern (p->resource_type);
	Car (tail2) = x;
    }
    GC_Unlink;
    if (freeit) XtFree ((char *)r);
    return ret;
}
