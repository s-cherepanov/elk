#include "xt.h"

static Object P_Xt_Release_4_Or_Laterp () {
    return True;
}

static Object P_Xt_Release_5_Or_Laterp () {
#ifdef XT_RELEASE_5_OR_LATER
    return True;
#else
    return False;
#endif
}

static Object P_Xt_Release_6_Or_Laterp () {
#ifdef XT_RELEASE_6_OR_LATER
    return True;
#else
    return False;
#endif
}

extern WidgetClass vendorShellWidgetClass;

/* The reference to vendorShellWidgetClass is required to make sure
 * that the linker pulls the vendor shell definition from libXaw,
 * not from libXt.  It's passed to a dummy function to make sure that
 * it isn't removed by the optimizer.
 */

static dummy (w) WidgetClass w; {
}

elk_init_xt_init () {
    extern WidgetClass vendorShellWidgetClass;

    dummy(vendorShellWidgetClass);

    Define_Primitive (P_Xt_Release_4_Or_Laterp, "xt-release-4-or-later?",
	0, 0, EVAL);
    Define_Primitive (P_Xt_Release_5_Or_Laterp, "xt-release-5-or-later?",
	0, 0, EVAL);
    Define_Primitive (P_Xt_Release_6_Or_Laterp, "xt-release-6-or-later?",
	0, 0, EVAL);
    XtToolkitInitialize ();
    P_Provide (Intern ("xt.so"));
    P_Provide (Intern ("xt.o"));
}
