/* stab.c: Read and manage symbol tables from object modules.
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

#include <string.h>
#include <stdlib.h>

#include "kernel.h"

void Free_Symbols (SYMTAB *);

#if defined(CAN_LOAD_LIB) || defined (INIT_OBJECTS)

/*#if defined(MACH_O)
#  include "stab-macho.c"
#elif defined(HAVE_LIBELF)
#  include "stab-elf.c"
#elif defined(COFF) || defined(XCOFF)
#  include "stab-coff.c"
#elif defined(ECOFF)
#  include "stab-ecoff.c"
#elif defined(CONVEX_AOUT)
#  include "stab-convex.c"
#elif defined(hp9000s300) || defined(__hp9000s300) || defined(__hp9000s300__)
#  include "stab-hp9k300.c"
#elif defined(hp9000s800) || defined(__hp9000s800) || defined(__hp9000s800__)
#  include "stab-hp9k800.c"
#elif defined(HAVE_A_OUT_H)
#  include "stab-bsd.c"
#else
#  include "stab-unix.c"
#endif*/

static SYMPREFIX Ignore_Prefixes[] =  {
    /* Currently none */
    { 0, 0 }
};
static SYMPREFIX Init_Prefixes[] =  {
    { INIT_PREFIX,    PR_EXTENSION },
    { "_GLOBAL_.I.",  PR_CONSTRUCTOR },    /* SVR4.2/g++ */
    { "__sti__",      PR_CONSTRUCTOR },
    { "_STI",         PR_CONSTRUCTOR },
    { "_GLOBAL_$I$",  PR_CONSTRUCTOR },
    { 0, 0 }
};
static SYMPREFIX Finit_Prefixes[] = {
    { FINIT_PREFIX,   PR_EXTENSION },
    { "_GLOBAL_.D.",  PR_CONSTRUCTOR },
    { "__std__",      PR_CONSTRUCTOR },
    { "_STD",         PR_CONSTRUCTOR },
    { "_GLOBAL_$D$",  PR_CONSTRUCTOR },
    { 0, 0 }
};

static FUNCT *Finalizers;

static void Call (unsigned long int l) {
#ifdef XCOFF
    unsigned long int vec[3];
    extern main();

    memcpy (vec, main, sizeof vec);
    vec[0] = (l & ~0xF0000000) + (vec[0] & 0xF0000000);
    ((void (*)())vec)();
#else
    ((void (*)())l)();
#endif
}

void Call_Initializers (SYMTAB *tab, char *addr, int which) {
    SYM *sp;
    char *p;
    SYMPREFIX *pp;
    FUNCT *fp, **fpp;

    /* Set pointer to end of list of finalizers; extension finalization
     * functions and C++ static destructors will be appended to this list:
     */
    for (fpp = &Finalizers; *fpp; fpp = &(*fpp)->next)
        ;

    for (sp = tab->first; sp; sp = sp->next) {
        if ((char *)sp->value < addr)
            continue;
        p = sp->name;
#ifdef SYMS_BEGIN_WITH
        if (*p == SYMS_BEGIN_WITH)
            p++;
        else
            continue;
#endif
        for (pp = Ignore_Prefixes; pp->name; pp++)
            if (strncmp (p, pp->name, strlen (pp->name)) == 0)
                goto next;
        for (pp = Init_Prefixes; pp->name; pp++) {
            if (pp->type == which
                    && strncmp (p, pp->name, strlen (pp->name)) == 0) {
                if (Verb_Init)
                    printf ("[calling %s]\n", p);
                Call (sp->value);
            }
        }
        /* Append to list of finalizers (to be invoked on exit):
         */
        for (pp = Finit_Prefixes; pp->name; pp++) {
            if (pp->type == which
                    && strncmp (p, pp->name, strlen (pp->name)) == 0) {
                fp = (FUNCT *)Safe_Malloc (sizeof (FUNCT));
                fp->func = (void (*)())sp->value;
                fp->name = Safe_Malloc (strlen (p) + 1);
                strcpy (fp->name, p);
                fp->next = 0;
                *fpp = fp;
                fpp = &fp->next;
            }
        }
next: ;
    }
}

/* Call the finialization functions and C++ static destructors.  Make sure
 * that calling exit() from a function doesn't cause endless recursion.
 */
void Call_Finalizers () {
    while (Finalizers) {
        FUNCT *fp = Finalizers;
        Finalizers = fp->next;
        if (Verb_Init)
            printf ("[calling %s]\n", fp->name);
        Call ((unsigned long int)fp->func);
    }
}

void Free_Symbols (SYMTAB *tab) {
    register SYM *sp, *nextp;

    for (sp = tab->first; sp; sp = nextp) {
        nextp = sp->next;
#if defined(COFF) || defined(ECOFF)
        free (sp->name);
#endif
        free ((char *)sp);
    }
    if (tab->strings)
        free (tab->strings);
    free ((char *)tab);
}
#endif /* CAN_LOAD_LIB || INIT_OBJECTS */
