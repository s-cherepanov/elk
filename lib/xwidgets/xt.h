/* xt.h
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

#define Object FOO
#  include <X11/Intrinsic.h>
#  include <X11/Core.h>
#  include <X11/StringDefs.h>
#undef Object

#include <stdarg.h>

#if XtSpecificationRelease < 4
 #error "Xt Release 3 or earlier no longer supported"
#endif

#if XtSpecificationRelease >= 5
#  define XT_RELEASE_5_OR_LATER
#endif

#if XtSpecificationRelease >= 6
#  define XT_RELEASE_6_OR_LATER
#endif

typedef XtArgVal (*PFS2X) (Object);
typedef Object (*PFX2S) (XtArgVal);

extern int T_Context;
extern int T_Class;
extern int T_Widget;
extern int T_Identifier;

#define CONTEXT(x)      ((struct S_Context *)POINTER(x))
#define CLASS(x)        ((struct S_Class *)POINTER(x))
#define WIDGET(x)       ((struct S_Widget *)POINTER(x))
#define IDENTIFIER(x)   ((struct S_Identifier *)POINTER(x))

struct S_Context {
    Object tag;
    XtAppContext context;
    char free;
};

struct S_Class {
    Object tag;
    WidgetClass wclass;
    char *name;
};

struct S_Widget {
    Object tag;
    Widget widget;
    char free;
};

struct S_Identifier {
    Object tag;
    char type;
    XtPointer val;
    int num;
    char free;
};

extern WidgetClass widgetClass;    /* The `core' class */
extern WidgetClass constraintWidgetClass;
extern WidgetClass compositeWidgetClass;

C_LINKAGE_BEGIN

extern void Check_Callback_List (Object);
extern void Check_Context (Object);
extern void Check_Widget (Object);
extern void Check_Widget_Class (Object, WidgetClass);
extern void Convert_Args (int, Object*, ArgList, Widget, WidgetClass);
extern void Define_Callback (char*, char*, int);
extern void Define_Class (char *, WidgetClass, XtResourceList, int);
extern void Define_Converter_To_C (char*, PFS2X);
extern void Define_Converter_To_Scheme (char*, PFX2S);
extern void Fiddle_Destroy_Callback (Widget);
extern void Fill_Callbacks (Object, XtCallbackList, int, PFX2S);
extern void Free_Actions (XtAppContext);
extern void Get_All_Resources
    (int, Widget, WidgetClass, XtResource**, int*, int*);
extern void Make_Resource_Name (char*);
extern int Match_Xt_Obj (Object, va_list);
extern Object Get_Callbackfun (XtPointer);
extern Object Get_Function (int);
extern Object Get_Resources
    (WidgetClass, void (*)(WidgetClass, XtResourceList*, Cardinal*), int);
extern Object Get_Values (Widget, int, Object*);
extern Object Make_Class (WidgetClass, char*);
extern Object Make_Context (XtAppContext);
extern Object Make_Context_Foreign (XtAppContext);
extern Object Make_Id (int, XtPointer, int);
extern Object Make_Widget (Widget);
extern Object Make_Widget_Foreign (Widget);
extern Object Make_Widget_Class (WidgetClass);
extern PFX2S Find_Callback_Converter (WidgetClass, char*, Object);
extern PFX2S Find_Converter_To_Scheme (char*);
extern PFS2X Find_Converter_To_C (char*);
extern int Register_Function (Object);
extern void Deregister_Function (int);
extern XtAccelerators Get_Accelerators (Object);
extern XtTranslations Get_Translations (Object);
extern XtPointer Use_Id (Object, int);
extern void Xt_Warning (char*);
extern char *Class_Name (WidgetClass);
extern void Action_Hook (Widget, XtPointer, char*, XEvent*, char**, int*);
extern void Destroy_Callback_Proc (Widget, XtPointer, XtPointer);
extern void Get_Sub_Resource_List (WidgetClass, XtResourceList*, Cardinal*);
extern Object Xt_Class_Name (WidgetClass);
extern Object Get_Selection_CB (void *); /* xm/support.d */
extern Object Get_Any_CB (void *); /* xm/support.d */

extern void elk_init_xt_accelerator ();
extern void elk_init_xt_action ();
extern void elk_init_xt_callback ();
extern void elk_init_xt_class ();
extern void elk_init_xt_context ();
extern void elk_init_xt_function ();
extern void elk_init_xt_identifier ();
extern void elk_init_xt_popup ();
extern void elk_init_xt_translation ();
extern void elk_init_xt_widget ();
extern void elk_init_xt_error ();
extern void elk_init_xt_init ();

C_LINKAGE_END

#define Encode_Arglist(ac,av,to,widget,class) {\
    Alloca (to, Arg*, ((ac)+1)/2 * sizeof (Arg));\
    Convert_Args (ac, av, to, widget, class);\
}

#define streq(a,b) (strcmp ((a), (b)) == 0)
