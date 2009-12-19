/* stab-convex.c
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

#include <errno.h>
#include <fcntl.h>
#include <convex/filehdr.h>
#include <convex/opthdr.h>
#include <convex/scnhdr.h>
#include <nlist.h>
#include <convex/reloc.h>
#include <sys/types.h>
#include <sys/mman.h>

#ifdef INIT_OBJECTS
SYMTAB *Open_File_And_Snarf_Symbols (char *name) {
    int f, n, len = 0;
    char *base;
    struct filehdr *fhp;
    struct opthdr *ohp;
    struct nlist *np;
    SYMTAB *tab;
    SYM *sp, **nextp;

    if ((f = open (name, O_RDONLY)) == -1) {
        Saved_Errno = errno;
        Primitive_Error ("can't open a.out file: ~E");
    }
    if ((base = mmap (0xc0000000, &len, PROT_READ, MAP_FILE, f, (off_t)0))
            == (char *)-1) {
        Saved_Errno = errno;
        Primitive_Error ("can't mmap a.out file: ~E");
    }
    close (f);
    fhp = (struct filehdr *)base;

    tab = (SYMTAB *)Safe_Malloc (sizeof (SYMTAB));
    tab->first = 0;
    tab->strings = Safe_Malloc ((unsigned int)fhp->h_strsiz);
    memcpy (tab->strings, base + fhp->h_strptr, (unsigned int)fhp->h_strsiz);
    nextp = &tab->first;

    ohp = (struct opthdr *)(base + sizeof *fhp);
    np = (struct nlist *)(base + ohp->o_symptr);
    for (n = 0; n < ohp->o_nsyms; n++, np++) {
        if (np->n_un.n_strx == 0 || np->n_type & N_STAB)
            continue;
        if ((np->n_type & N_TYPE) != N_TEXT)
            continue;
        sp = (SYM *)Safe_Malloc (sizeof (SYM));
        sp->name = tab->strings + np->n_un.n_strx;
        sp->value = np->n_value;
        *nextp = sp;
        nextp = &sp->next;
        *nextp = 0;
    }
    (void)munmap (base, len);
    return tab;
}
#endif
