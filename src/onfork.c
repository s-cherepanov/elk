/* onfork.c: This module allows code to register `onfork' handlers, similar
 * to the way exit handlers are registered in C with atexit().
 * The interpreter kernel proper never calls the onfork handlers,
 * but the fork primitive of the UNIX extension does in the newly
 * created child process (other extensions may also call the handlers).
 *
 * The dynamic loading implementation of the interpreter kernel
 * may register onfork handlers to add links to the temp files.
 * User-supplied extensions may want to register onfork handlers
 * as well.
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

#include "kernel.h"

static FUNCT *Onfork_Funcs;

void Register_Onfork (void (*f)()) {
    FUNCT *p;

    p = (FUNCT *)Safe_Malloc (sizeof (*p));
    p->func = f;
    p->next = Onfork_Funcs;
    Onfork_Funcs = p;
}

void Call_Onfork () {
    FUNCT *p;

    for (p = Onfork_Funcs; p; p = p->next)
        p->func();
}
