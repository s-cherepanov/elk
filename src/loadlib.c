/* load-dl.c
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

#include "config.h"

#ifdef CAN_LOAD_LIB

#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#if defined(HAVE_MACH_O_DYLD_H)
#   include <mach-o/dyld.h>
#elif defined(HAVE_DL_DLOPEN)
#   if defined(HAVE_DLFCN_H)
#       include <dlfcn.h>
#   endif
#   if defined(HAVE_SYS_DL_H)
#       include <sys/dl.h>
#   endif
#elif defined(HAVE_DL_SHL_LOAD)
#   if defined(HAVE_DL_H)
#       include <dl.h>
#   endif
#endif

#include "kernel.h"

extern void Free_Symbols (SYMTAB *);
extern void Call_Initializers (SYMTAB *, char *, int);

void Dlopen_File (char *fn) {
    SYM *sp;

#if defined(HAVE_DL_DYLD)
    NSModule handle;
    NSObjectFileImage image;
    NSObjectFileImageReturnCode ret;

    if (Verb_Load)
        printf ("[dyld %s]\n", fn);

    ret = NSCreateObjectFileImageFromFile (fn, &image);

    if (ret != NSObjectFileImageSuccess)
        Primitive_Error ("could not map `~%~s'",
                         Make_String (fn, strlen (fn)));

    /* Open the dynamic module */
    handle = NSLinkModule (image, fn, NSLINKMODULE_OPTION_RETURN_ON_ERROR);

    if (!handle) {
        NSLinkEditErrors errors;
        const char *file, *err;
        int errnum;
        NSLinkEditError (&errors, &errnum, &file, &err);
        Primitive_Error ("could not dyld `~%~s': ~%~s",
                         Make_String (file, strlen (file)),
                         Make_String (err, strlen (err)));
    }

    /* Destroy our image, we won't need it */
    NSDestroyObjectFileImage (image);

    /* NSUnLinkModule (handle, FALSE); */

#elif defined(HAVE_DL_DLOPEN)
    void *handle;

    if (Verb_Load)
        printf ("[dlopen %s]\n", fn);

#if defined(RTLD_GLOBAL)
    handle = dlopen (fn, RTLD_NOW | RTLD_GLOBAL);
#elif defined(DL_GLOBAL)
    handle = dlopen (fn, DL_NOW | DL_GLOBAL);
#else
    handle = dlopen (fn, 0);
#endif

    if (handle == NULL) {
        char *err = dlerror ();
        Primitive_Error ("dlopen failed: ~%~s",
                         Make_String (err, strlen (err)));
    }

#elif defined(HAVE_DL_SHL_LOAD)
    shl_t handle;

    if (Verb_Load)
        printf ("[shl_load %s]\n", fn);

    handle = shl_load (fn, BIND_IMMEDIATE | BIND_NONFATAL, NULL);

    if (handle == NULL) {
        char *err = strerror (errno);
        Primitive_Error ("shl_load failed: ~%~s",
                         Make_String (err, strlen (err)));
    }

#else
#   error "No dynamic plugins API"
#endif

    if (The_Symbols)
        Free_Symbols (The_Symbols);

    The_Symbols = Open_File_And_Snarf_Symbols (fn);
    for (sp = The_Symbols->first; sp; sp = sp->next) {
#if defined(HAVE_DL_DYLD)
        NSSymbol sym = NSLookupSymbolInModule (handle, sp->name);
        if (sym)
            sp->value = (unsigned long int)(intptr_t)NSAddressOfSymbol (sym);

#elif defined(HAVE_DL_DLOPEN)
        /* dlsym() may fail for symbols not exported by object file;
         * this can be safely ignored. */
        sp->value = (unsigned long int)(intptr_t)dlsym (handle, sp->name);

#elif defined(HAVE_DL_SHL_LOAD)
        void *sym;
        shl_findsym (&handle, "share", TYPE_UNDEFINED, &sym);
        sp->value = (unsigned long int)(intptr_t)sym;
#endif
    }

    Call_Initializers (The_Symbols, 0, PR_CONSTRUCTOR);
    Call_Initializers (The_Symbols, 0, PR_EXTENSION);
}

static void Load_Lib (Object libs) {
    Object port, name;

    if (Nullp (libs))
        return;

    Load_Lib (Cdr (libs));

    GC_Node2;
    port = name = Null;
    GC_Link2 (port, name);
    port = General_Open_File (Car (libs), P_INPUT, Var_Get (V_Load_Path));
    name = PORT(port)->name;
    Dlopen_File (STRING(name)->data);
    (void)P_Close_Input_Port (port);
    GC_Unlink;
}

void Load_Library (Object libs) {
    Disable_Interrupts;
    Load_Lib (libs);
    Enable_Interrupts;
}

#endif /* CAN_LOAD_LIB */

