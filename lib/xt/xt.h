#include "xlib.h"

#define Object FOO
#  include <X11/Intrinsic.h>
#  include <X11/Core.h>
#  include <X11/StringDefs.h>
#undef Object

#if XtSpecificationRelease < 4
 #error "Xt Release 3 or earlier no longer supported"
#endif

#if XtSpecificationRelease >= 5
#  define XT_RELEASE_5_OR_LATER
#endif

#if XtSpecificationRelease >= 6
#  define XT_RELEASE_6_OR_LATER
#endif

typedef XtArgVal (*PFS2X) P_((Object));
typedef Object (*PFX2S) P_((XtArgVal));

extern int T_Context;
extern int T_Class;
extern int T_Widget;
extern int T_Identifier;

#define CONTEXT(x)	((struct S_Context *)POINTER(x))
#define CLASS(x)	((struct S_Class *)POINTER(x))
#define WIDGET(x)	((struct S_Widget *)POINTER(x))
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

extern void Check_Callback_List P_((Object));
extern void Check_Context P_((Object));
extern void Check_Widget P_((Object));
extern void Check_Widget_Class P_((Object, WidgetClass));
extern void Convert_Args P_((int, Object*, ArgList, Widget, WidgetClass));
extern void Define_Callback P_((char*, char*, int));
extern void Define_Class
    P_((char *, WidgetClass, XtResourceList, int));
extern void Define_Converter_To_C P_((char*, PFS2X));
extern void Define_Converter_To_Scheme P_((char*, PFX2S));
extern void Fiddle_Destroy_Callback P_((Widget));
extern void Fill_Callbacks P_((Object, XtCallbackList, int, PFX2S));
extern void Free_Actions P_((XtAppContext));
extern void Get_All_Resources
    P_((int, Widget, WidgetClass, XtResource**, int*, int*));
extern void Make_Resource_Name P_((char*));
extern int Match_Xt_Obj P_((ELLIPSIS));
extern Object Get_Callbackfun P_((XtPointer));
extern Object Get_Function P_((int));
extern Object Get_Resources
    P_((WidgetClass, void (*)(WidgetClass, XtResourceList*, Cardinal*), int));
extern Object Get_Values P_((Widget, int, Object*));
extern Object Make_Class P_((WidgetClass, char*));
extern Object Make_Context P_((XtAppContext));
extern Object Make_Context_Foreign P_((XtAppContext));
extern Object Make_Id P_((int, XtPointer, int));
extern Object Make_Widget P_((Widget));
extern Object Make_Widget_Foreign P_((Widget));
extern Object Make_Widget_Class P_((WidgetClass));
extern PFX2S Find_Callback_Converter P_((WidgetClass, char*, Object));
extern PFX2S Find_Converter_To_Scheme P_((char*));
extern PFS2X Find_Converter_To_C P_((char*));
extern int Register_Function P_((Object));
extern void Deregister_Function P_((int));
extern XtAccelerators Get_Accelerators P_((Object));
extern XtTranslations Get_Translations P_((Object));
extern XtPointer Use_Id P_((Object, int));
extern void Xt_Warning P_((char*));
extern char *Class_Name P_((WidgetClass));
extern void Action_Hook P_((Widget, XtPointer, char*, XEvent*, char**, int*));
extern void Destroy_Callback_Proc P_((Widget, XtPointer, XtPointer));
extern void Get_Sub_Resource_List P_((WidgetClass, XtResourceList*, Cardinal*));
extern Object Xt_Class_Name P_((WidgetClass));
extern Object Get_Selection_CB P_((ELLIPSIS));    /* xm/support.d */
extern Object Get_Any_CB P_((ELLIPSIS));          /* xm/support.d */

C_LINKAGE_END

#define Encode_Arglist(ac,av,to,widget,class) {\
    Alloca (to, Arg*, ((ac)+1)/2 * sizeof (Arg));\
    Convert_Args (ac, av, to, widget, class);\
}

#define streq(a,b) (strcmp ((a), (b)) == 0)
