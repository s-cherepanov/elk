#include "xlib.h"

static Object P_Xlib_Release_4_Or_Laterp () {
    return True;
}

static Object P_Xlib_Release_5_Or_Laterp () {
#ifdef XLIB_RELEASE_5_OR_LATER
    return True;
#else
    return False;
#endif
}

static Object P_Xlib_Release_6_Or_Laterp () {
#ifdef XLIB_RELEASE_6_OR_LATER
    return True;
#else
    return False;
#endif
}

elk_init_xlib_init () {
    Define_Primitive (P_Xlib_Release_4_Or_Laterp,
			"xlib-release-4-or-later?",               0, 0, EVAL);
    Define_Primitive (P_Xlib_Release_5_Or_Laterp,
			"xlib-release-5-or-later?",               0, 0, EVAL);
    Define_Primitive (P_Xlib_Release_6_Or_Laterp,
			"xlib-release-6-or-later?",               0, 0, EVAL);
    P_Provide (Intern ("xlib.o"));
}

#if defined(XLIB_RELEASE_5_OR_LATER) && (defined(sun) || defined(__sun__)) &&\
    defined(__svr4__)
/*
 * Stub interface to dynamic linker routines
 * that SunOS uses but didn't ship with 4.1.
 *
 * The C library routine wcstombs in SunOS 4.1 tries to dynamically
 * load some routines using the dlsym interface, described in dlsym(3x).
 * Unfortunately SunOS 4.1 does not include the necessary library, libdl.
 */

void *dlopen() { return 0; }

void *dlsym() { return 0; }

int dlclose() { return -1; }

#endif
