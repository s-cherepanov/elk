/* Read and manage symbol tables from object modules.
 */

#include "kernel.h"

#include <string.h>
#include <stdlib.h>

void Free_Symbols (SYMTAB *);

#if defined(CAN_LOAD_OBJ) || defined (INIT_OBJECTS)

#ifdef MACH_O
#  include "stab-macho.c"
#else
#ifdef ELF
#  include "stab-elf.c"
#else
#if defined(COFF) || defined(XCOFF)
#  include "stab-coff.c"
#else
#ifdef ECOFF
#  include "stab-ecoff.c"
#else
#ifdef CONVEX_AOUT
#  include "stab-convex.c"
#else
#if defined(hp9000s300) || defined(__hp9000s300) || defined(__hp9000s300__)
#  include "stab-hp9k300.c"
#else
#if defined(hp9000s800) || defined(__hp9000s800) || defined(__hp9000s800__)
#  include "stab-hp9k800.c"
#else
#  include "stab-bsd.c"
#endif
#endif
#endif
#endif
#endif
#endif
#endif

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
#endif /* CAN_LOAD_OBJ || INIT_OBJECTS */
