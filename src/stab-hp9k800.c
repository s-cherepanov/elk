/* stab-hp9k800.c
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

#include AOUT_H
#include <sys/types.h>

extern void Free_Symbols (SYMTAB *);

SYMTAB *Snarf_Symbols (FILE *f, struct header *hp) {
    SYMTAB *tab;
    register SYM *sp, **nextp;
    register int n;
    struct symbol_dictionary_record r;

    tab = (SYMTAB *)Safe_Malloc (sizeof (SYMTAB));
    tab->first = 0;
    nextp = &tab->first;
    tab->strings = Safe_Malloc (hp->symbol_strings_size);
    (void)fseek (f, (long)hp->symbol_strings_location, SEEK_SET);
    if (fread (tab->strings, hp->symbol_strings_size, 1, f) != 1) {
        (void)fclose (f);
        Free_Symbols (tab);
        Primitive_Error ("corrupt string table in object file");
    }
    (void)fseek (f, (long)hp->symbol_location, SEEK_SET);
    for (n = hp->symbol_total; n > 0; n--) {
        if (fread ((char *)&r, sizeof r, 1, f) != 1) {
            (void)fclose (f);
            Free_Symbols (tab);
            Primitive_Error ("corrupt symbol table in object file");
        }
        if (r.symbol_type != ST_CODE)
            continue;
        if (r.symbol_scope != SS_UNIVERSAL)
            continue;
        sp = (SYM *)Safe_Malloc (sizeof (SYM));
        sp->name = tab->strings + r.name.n_strx;
        sp->value = r.symbol_value & ~3;  /* mask out privilege level */
        *nextp = sp;
        nextp = &sp->next;
        *nextp = 0;
    }
    return tab;
}

SYMTAB *Open_File_And_Snarf_Symbols (char *name) {
    struct header hdr;
    FILE *f;
    SYMTAB *tab;

    if ((f = fopen (name, "r")) == NULL)
        Primitive_Error ("can't open a.out file");
    if (fread ((char *)&hdr, sizeof hdr, 1, f) != 1) {
        (void)fclose (f);
        Primitive_Error ("can't read a.out header");
    }
    tab = Snarf_Symbols (f, &hdr);
    (void)fclose (f);
    return tab;
}
