#include "xt.h"

extern void XtManageChildren(), XtUnmanageChildren();

static Object P_Destroy_Widget();

Generic_Predicate (Widget)

Generic_Equal (Widget, WIDGET, widget)

Generic_Print (Widget, "#[widget %lu]", POINTER(x))

static Object Internal_Make_Widget (finalize, widget) Widget widget; {
    Object w;

    if (widget == 0)
        return Sym_None;
    w = Find_Object (T_Widget, (GENERIC)0, Match_Xt_Obj, widget);
    if (Nullp (w)) {
        w = Alloc_Object (sizeof (struct S_Widget), T_Widget, 0);
        WIDGET(w)->tag = Null;
        WIDGET(w)->widget = widget;
        WIDGET(w)->free = 0;
        XtAddCallback (widget, XtNdestroyCallback, Destroy_Callback_Proc,
            (XtPointer)0);
        Register_Object (w, (GENERIC)0,
            finalize ? P_Destroy_Widget : (PFO)0, 0);
    }
    return w;
}

/* Backwards compatibility: */
Object Make_Widget (widget) Widget widget; {
    return Internal_Make_Widget (1, widget);
}

Object Make_Widget_Foreign (widget) Widget widget; {
    return Internal_Make_Widget (0, widget);
}

void Check_Widget (w) Object w; {
    Check_Type (w, T_Widget);
    if (WIDGET(w)->free)
        Primitive_Error ("invalid widget: ~s", w);
}

void Check_Widget_Class (w, class) Object w; WidgetClass class; {
    Check_Widget (w);
    if (XtClass (WIDGET(w)->widget) != class)
        Primitive_Error ("widget not of expected class: ~s", w);
}

static Object P_Destroy_Widget (w) Object w; {
    Check_Widget (w);
    XtDestroyWidget (WIDGET(w)->widget);
    return Void;
}

static Object P_Create_Shell (argc, argv) Object *argv; {
    register char *sn = 0, *sc = 0;
    ArgList a;
    Object name, class, w, d, ret;
    Alloca_Begin;

    name = argv[0], class = argv[1], w = argv[2], d = argv[3];
    if (!EQ(name, False))
        sn = Get_Strsym (name);
    if (!EQ(class, False))
        sc = Get_Strsym (class);
    Check_Type (w, T_Class);
    Check_Type (d, T_Display);
    Encode_Arglist (argc-4, argv+4, a, (Widget)0, CLASS(w)->wclass);
    ret =  Make_Widget (XtAppCreateShell (sn, sc, CLASS(w)->wclass,
        DISPLAY(d)->dpy, a, (Cardinal)(argc-4)/2));
    Alloca_End;
    return ret;
}

static Object P_Create_Widget (argc, argv) Object *argv; {
    ArgList a;
    char *name = 0;
    Object x, class, parent, ret;
    Alloca_Begin;

    x = argv[0];
    if (TYPE(x) != T_Class) {
        name = Get_Strsym (x);
        argv++; argc--;
    }
    class = argv[0];
    parent = argv[1];
    Check_Type (class, T_Class);
    Check_Widget (parent);
    if (name == 0)
        name = CLASS(class)->name;
    Encode_Arglist (argc-2, argv+2, a, WIDGET(parent)->widget,
        CLASS(class)->wclass);
    ret =  Make_Widget (XtCreateWidget ((String)name, CLASS(class)->wclass,
        WIDGET(parent)->widget, a, (Cardinal)(argc-2)/2));
    Alloca_End;
    return ret;
}

static Object P_Realize_Widget (w) Object w; {
    Check_Widget (w);
    XtRealizeWidget (WIDGET(w)->widget);
    return Void;
}

static Object P_Unrealize_Widget (w) Object w; {
    Check_Widget (w);
    XtUnrealizeWidget (WIDGET(w)->widget);
    return Void;
}

static Object P_Widget_Realizedp (w) Object w; {
    Check_Widget (w);
    return XtIsRealized (WIDGET(w)->widget) ? True : False;
}

static Object P_Widget_Display (w) Object w; {
    Check_Widget (w);
    return Make_Display (0, XtDisplayOfObject (WIDGET(w)->widget));
}

static Object P_Widget_Parent (w) Object w; {
    Check_Widget (w);
    return Make_Widget_Foreign (XtParent (WIDGET(w)->widget));
}

static Object P_Widget_Name (w) Object w; {
    char *s;

    Check_Widget (w);
    s = XtName (WIDGET(w)->widget);
    return Make_String (s, strlen (s));
}

static Object P_Widget_To_Window (w) Object w; {
    Check_Widget (w);
    return Make_Window (0, XtDisplayOfObject (WIDGET(w)->widget),
        XtWindow (WIDGET(w)->widget));
}

static Object P_Widget_Compositep (w) Object w; {
    Check_Widget (w);
    return XtIsComposite (WIDGET(w)->widget) ? True : False;
}

static Object Manage_Unmanage (children, f) Object children; void (*f)(); {
    register i, n;
    Widget *buf;
    Object tail;
    Alloca_Begin;

    Check_List (children);
    n = Fast_Length (children);
    Alloca (buf, Widget*, n * sizeof (Widget));
    for (i = 0, tail = children; i < n; i++, tail = Cdr (tail)) {
        Object w;

        w = Car (tail);
        Check_Widget (w);
        buf[i] = WIDGET(w)->widget;
    }
    f (buf, n);
    Alloca_End;
    return Void;
}

static Object P_Manage_Children (children) Object children; {
    return Manage_Unmanage (children, XtManageChildren);
}

static Object P_Unmanage_Children (children) Object children; {
    return Manage_Unmanage (children, XtUnmanageChildren);
}

static Object P_Widget_Managedp (w) Object w; {
    Check_Widget (w);
    return XtIsManaged (WIDGET(w)->widget) ? True : False;
}

static Object P_Widget_Class (w) Object w; {
    Check_Widget (w);
    return Make_Widget_Class (XtClass (WIDGET(w)->widget));
}

static Object P_Widget_Superclass (w) Object w; {
    Check_Widget (w);
    if (XtClass (WIDGET(w)->widget) == widgetClass)
        return Sym_None;
    return Make_Widget_Class (XtSuperclass (WIDGET(w)->widget));
}

static Object P_Widget_Subclassp (w, c) Object w, c; {
    Check_Widget (w);
    Check_Type (c, T_Class);
    return XtIsSubclass (WIDGET(w)->widget, CLASS(c)->wclass) ? True : False;
}

static Object P_Set_Mapped_When_Managed (w, m) Object w, m; {
    Check_Widget (w);
    Check_Type (m, T_Boolean);
    XtSetMappedWhenManaged (WIDGET(w)->widget, EQ(m, True));
    return Void;
}

static Object P_Map_Widget (w) Object w; {
    Check_Widget (w);
    XtMapWidget (WIDGET(w)->widget);
    return Void;
}

static Object P_Unmap_Widget (w) Object w; {
    Check_Widget (w);
    XtUnmapWidget (WIDGET(w)->widget);
    return Void;
}

static Object P_Set_Values (argc, argv) Object *argv; {
    ArgList a;
    Widget w;
    register i, n = (argc-1)/2;
    Alloca_Begin;

    Check_Widget (argv[0]);
    w = WIDGET(argv[0])->widget;
    Encode_Arglist (argc-1, argv+1, a, w, XtClass (w));
    XtSetValues (w, a, (Cardinal)n);
    for (i = 0; i < n; i++)
        if (streq (a[i].name, XtNdestroyCallback))
            Fiddle_Destroy_Callback (w);
    Alloca_End;
    return Void;
}

static Object P_Get_Values (argc, argv) Object *argv; {
    Widget w;

    Check_Widget (argv[0]);
    w = WIDGET(argv[0])->widget;
    return Get_Values (w, argc-1, argv+1);
}

static Object P_Widget_Context (w) Object w; {
    Check_Widget (w);
    return
        Make_Context_Foreign (XtWidgetToApplicationContext (WIDGET(w)->widget));
}

static Object P_Set_Sensitive (w, s) Object w, s; {
    Check_Widget (w);
    Check_Type (s, T_Boolean);
    XtSetSensitive (WIDGET(w)->widget, EQ(s, True));
    return Void;
}

static Object P_Sensitivep (w) Object w; {
    Check_Widget (w);
    return XtIsSensitive (WIDGET(w)->widget) ? True : False;
}

static Object P_Window_To_Widget (w) Object w; {
    Check_Type (w, T_Window);
    return Make_Widget_Foreign (XtWindowToWidget (WINDOW(w)->dpy,
        WINDOW(w)->win));
}

static Object P_Name_To_Widget (root, name) Object root, name; {
    register char *s;

    Check_Widget (root);
    return Make_Widget_Foreign (XtNameToWidget (WIDGET(root)->widget,
        Get_Strsym (name)));
}

static Object P_Widget_Translate_Coordinates (w, x, y) Object w, x, y; {
    Position root_x, root_y;

    Check_Widget (w);
    XtTranslateCoords (WIDGET(w)->widget, Get_Integer (x), Get_Integer (y),
        &root_x, &root_y);
    return Cons (Make_Integer (root_x), Make_Integer (root_y));
}

/* The GC-visit function for widgets.  Visit the children of composite
 * widgets and all the parents of a widget.
 * Based on code contributed by Ken Fox <fox@pt0204.pto.ford.com>.
 */

#include <X11/IntrinsicP.h>
#include <X11/CoreP.h>
#include <X11/CompositeP.h>
#undef XtIsComposite

static Widget_Visit (root, func) Object *root; int (*func)(); {
    Object obj;
    Widget w = WIDGET(*root)->widget;

    if (WIDGET(*root)->free == 0 && XtIsComposite (w)) {
        int i;
        CompositeRec *comp = (CompositeRec *)w;

        for (i = 0; i < comp->composite.num_children; i++) {
            obj = Find_Object (T_Widget, (GENERIC)0, Match_Xt_Obj,
                comp->composite.children[i]);
            if (TYPE(obj) == T_Widget)
                func (&obj);
        }
        while (w = XtParent (w)) {
            obj = Find_Object (T_Widget, (GENERIC)0, Match_Xt_Obj, w);
            if (TYPE(obj) == T_Widget)
                func (&obj);
        }
    }
}

elk_init_xt_widget () {
    T_Widget = Define_Type (0, "widget", NOFUNC, sizeof (struct S_Widget),
        Widget_Equal, Widget_Equal, Widget_Print, Widget_Visit);
    Define_Primitive (P_Widgetp,           "widget?",           1, 1, EVAL);
    Define_Primitive (P_Destroy_Widget,    "destroy-widget",    1, 1, EVAL);
    Define_Primitive (P_Create_Shell,      "create-shell",  4, MANY, VARARGS);
    Define_Primitive (P_Create_Widget,     "create-widget", 2, MANY, VARARGS);
    Define_Primitive (P_Realize_Widget,    "realize-widget",    1, 1, EVAL);
    Define_Primitive (P_Unrealize_Widget,  "unrealize-widget",  1, 1, EVAL);
    Define_Primitive (P_Widget_Realizedp,  "widget-realized?",  1, 1, EVAL);
    Define_Primitive (P_Widget_Display,    "widget-display",    1, 1, EVAL);
    Define_Primitive (P_Widget_Parent,     "widget-parent",     1, 1, EVAL);
    Define_Primitive (P_Widget_Name,       "widget-name",       1, 1, EVAL);
    Define_Primitive (P_Widget_To_Window,  "widget->window",    1, 1, EVAL);
    Define_Primitive (P_Widget_Compositep, "widget-composite?", 1, 1, EVAL);
    Define_Primitive (P_Manage_Children,   "manage-children",   1, 1, EVAL);
    Define_Primitive (P_Unmanage_Children, "unmanage-children", 1, 1, EVAL);
    Define_Primitive (P_Widget_Managedp,   "widget-managed?",   1, 1, EVAL);
    Define_Primitive (P_Widget_Class,      "widget-class",      1, 1, EVAL);
    Define_Primitive (P_Widget_Superclass, "widget-superclass", 1, 1, EVAL);
    Define_Primitive (P_Widget_Subclassp,  "widget-subclass?",  2, 2, EVAL);
    Define_Primitive (P_Set_Mapped_When_Managed,
                                  "set-mapped-when-managed!",   2, 2, EVAL);
    Define_Primitive (P_Map_Widget,        "map-widget",        1, 1, EVAL);
    Define_Primitive (P_Unmap_Widget,      "unmap-widget",      1, 1, EVAL);
    Define_Primitive (P_Set_Values,        "set-values!",   1, MANY, VARARGS);
    Define_Primitive (P_Get_Values,        "get-values",    1, MANY, VARARGS);
    Define_Primitive (P_Widget_Context,    "widget-context",    1, 1, EVAL);
    Define_Primitive (P_Set_Sensitive,     "set-sensitive!",    2, 2, EVAL);
    Define_Primitive (P_Sensitivep,        "widget-sensitive?", 1, 1, EVAL);
    Define_Primitive (P_Window_To_Widget,  "window->widget",    1, 1, EVAL);
    Define_Primitive (P_Name_To_Widget,    "name->widget",      2, 2, EVAL);
    Define_Primitive (P_Widget_Translate_Coordinates,
                                "widget-translate-coordinates", 3, 3, EVAL);
}
