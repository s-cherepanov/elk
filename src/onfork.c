/* This module allows code to register `onfork' handlers, similar to
 * the way exit handlers are registered in C with atexit().
 * The interpreter kernel proper never calls the onfork handlers,
 * but the fork primitive of the UNIX extension does in the newly
 * created child process (other extensions may also call the handlers).
 *
 * The dynamic loading implementation of the interpreter kernel
 * may register onfork handlers to add links to the temp files.
 * User-supplied extensions may want to register onfork handlers
 * as well.
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
