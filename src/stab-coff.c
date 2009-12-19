/* stab-coff.c
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

#include <filehdr.h>
#include <syms.h>
#undef TYPE         /* ldfnc.h defines a TYPE macro. */
#include <ldfcn.h>

SYMTAB *Snarf_Symbols (LDFILE *lf, int ep) {
    SYMTAB *tab;
    register SYM *sp, **nextp;
    SYMENT sym;
    long inx;
    char *p;
    extern char *ldgetname();

    if (ldtbseek (lf) == FAILURE) {
        ldclose (lf);
        Primitive_Error ("can't ldtbseek");
    }
    tab = (SYMTAB *)Safe_Malloc (sizeof (SYMTAB));
    tab->first = 0;
    tab->strings = 0;
    nextp = &tab->first;
    while (1) {
        inx = ldtbindex (lf);
        if (ldtbread (lf, inx, &sym) == FAILURE)
            break;
        if (sym.n_scnum == N_UNDEF || sym.n_scnum == N_DEBUG
                || sym.n_scnum > HEADER(lf).f_nscns || sym.n_sclass != C_EXT)
            continue;
        if ((p = ldgetname (lf, &sym)) == NULL)
            continue;
        sp = (SYM *)Safe_Malloc (sizeof (SYM));
        sp->name = Safe_Malloc (strlen (p) + 1);
        strcpy (sp->name, p);
        sp->value = sym.n_value;
        *nextp = sp;
        nextp = &sp->next;
        *nextp = 0;
    }
    return tab;
}

#ifdef INIT_OBJECTS
SYMTAB *Open_File_And_Snarf_Symbols (char *name) {
    LDFILE *f;
    SYMTAB *tab;

    if ((f = ldopen (name, NULL)) == FAILURE)
        Primitive_Error ("can't ldopen a.out file");
    tab = Snarf_Symbols (f);
    ldclose (f);
    return tab;
}
#endif /* INIT_OBJECTS */
