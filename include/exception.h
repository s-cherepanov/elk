/* exception.h
 *
 * $Id$
 *
 * Copyright 1990, 1991, 1992, 1993, 1994, 1995, Oliver Laumann, Berlin
 * Copyright 2002, 2003 Sam Hocevar <sam@hocevar.net>, Paris
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

#ifdef HAVE_SIGNAL_H
#   include <signal.h>
#endif

extern int Intr_Was_Ignored;
extern unsigned long Intr_Level;

#if defined(HAVE_SIGPROCMASK)
    extern sigset_t Sigset_Old, Sigset_Block;
#elif defined(HAVE_SIGBLOCK)
    extern int Sigmask_Old, Sigmask_Block;
#else
    C_LINKAGE_BEGIN
    extern void Intr_Handler (int);
    C_LINKAGE_END
#endif

#if defined(HAVE_SIGPROCMASK) || ! defined(HAVE_SIGBLOCK)
#  ifndef sigmask
#    define sigmask(n)  (1 << ((n)-1))
#  endif
#endif

#define Disable_Interrupts {\
    if (Intr_Level++ == 0) Force_Disable_Interrupts;\
}

#define Enable_Interrupts {\
    if (Intr_Level > 0 && --Intr_Level == 0) Force_Enable_Interrupts;\
}

#ifdef HAVE_SIGPROCMASK
#define Force_Disable_Interrupts  (void)sigprocmask (SIG_BLOCK, &Sigset_Block,\
                                      (sigset_t *)0)
#define Force_Enable_Interrupts   (void)sigprocmask (SIG_SETMASK, &Sigset_Old,\
                                      (sigset_t *)0)
#elif defined(HAVE_SIGBLOCK)
#define Force_Disable_Interrupts  (void)sigblock (Sigmask_Block)
#define Force_Enable_Interrupts   (void)sigsetmask (Sigmask_Old)
#else
#define Force_Disable_Interrupts  {\
    if (!Intr_Was_Ignored) (void)signal (SIGINT, SIG_IGN);\
}
#define Force_Enable_Interrupts   {\
    if (!Intr_Was_Ignored) (void)signal (SIGINT, Intr_Handler);\
}
#endif

