/* exception.c
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

#include <stdlib.h>

#ifdef HAVE_ATTRIBUTE_NORETURN
extern void Reset () __attribute__ ((__noreturn__));
#else
extern void Reset ();
#endif

int Intr_Was_Ignored;
unsigned long int Intr_Level;

#ifdef POSIX_SIGNALS
sigset_t Sigset_Old, Sigset_Block;
#else
#ifdef BSD_SIGNALS
int Sigmask_Old, Sigmask_Block;
#endif
#endif

static Object V_Interrupt_Handler;

/* Make sure temp files are removed on hangup and broken pipe.
 */
/*ARGSUSED*/
void Signal_Exit (int sig) {
    Exit_Handler ();
    exit (1);
}

void Init_Exception () {
    Define_Variable (&V_Interrupt_Handler, "interrupt-handler", Null);
#ifdef POSIX_SIGNALS
    sigemptyset (&Sigset_Block);
    sigaddset (&Sigset_Block, SIGINT);
    (void)sigprocmask (0, (sigset_t *)0, &Sigset_Old);
#else
#ifdef BSD_SIGNALS
    Sigmask_Block = sigmask (SIGINT);
    Sigmask_Old = sigblock (0);
#endif
#endif
    (void)signal (SIGHUP, Signal_Exit);
    (void)signal (SIGPIPE, Signal_Exit);
}

/*ARGSUSED*/
void Intr_Handler (int sig) {
    Object fun;

#ifndef BSD_SIGNALS
    (void)signal (SIGINT, Intr_Handler);
#endif
    Set_Error_Tag ("interrupt-handler");
    Reset_IO (1);
    fun = Var_Get (V_Interrupt_Handler);
    if (TYPE(fun) == T_Compound && COMPOUND(fun)->min_args == 0)
        (void)Funcall (fun, Null, 0);
    Format (Curr_Output_Port, "~%\7Interrupt!~%", 15, 0, (Object *)0);
    Reset ();
    /*NOTREACHED*/
}

void Install_Intr_Handler () {
    if (signal (SIGINT, SIG_IGN) == SIG_IGN)
        Intr_Was_Ignored = 1;
    else
        (void)signal (SIGINT, Intr_Handler);
}

Object P_Disable_Interrupts () {
    Disable_Interrupts;
    return Make_Unsigned_Long (Intr_Level);
}

Object P_Enable_Interrupts () {
    Enable_Interrupts;
    return Make_Unsigned_Long (Intr_Level);
}
