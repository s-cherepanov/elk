/* xtinit.c
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

static void dummy (WidgetClass w) {
}

void elk_init_xt_init () {
    extern WidgetClass vendorShellWidgetClass;

    dummy(vendorShellWidgetClass);

    Define_Primitive (P_Xt_Release_4_Or_Laterp, "xt-release-4-or-later?",
        0, 0, EVAL);
    Define_Primitive (P_Xt_Release_5_Or_Laterp, "xt-release-5-or-later?",
        0, 0, EVAL);
    Define_Primitive (P_Xt_Release_6_Or_Laterp, "xt-release-6-or-later?",
        0, 0, EVAL);
    XtToolkitInitialize ();
    P_Provide (Intern ("xt.la"));
}
