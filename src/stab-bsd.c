/* stab-bsd.c
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

#include AOUT_H
#include <fcntl.h>
#include <errno.h>
#include <sys/types.h>

extern int errno;

#ifndef O_BINARY
#  define O_BINARY 0
#endif

SYMTAB *Snarf_Symbols (FILE *f, struct exec *ep) {
    SYMTAB *tab;
    register SYM *sp, **nextp;
    int nsyms, strsiz;
    struct nlist nl;

    tab = (SYMTAB *)Safe_Malloc (sizeof (SYMTAB));
    tab->first = 0;
    tab->strings = 0;
    nextp = &tab->first;
    (void)fseek (f, (long)N_SYMOFF(*ep), 0);
    for (nsyms = ep->a_syms / sizeof (nl); nsyms > 0; nsyms--) {
	if (fread ((char *)&nl, sizeof (nl), 1, f) != 1) {
	    Free_Symbols (tab);
	    (void)fclose (f);
	    Primitive_Error ("corrupt symbol table in object file");
	}
	if (nl.n_un.n_strx == 0 || nl.n_type & N_STAB)
	    continue;
#ifndef ibm023
	if ((nl.n_type & N_TYPE) != N_TEXT)
	    continue;
#endif
	sp = (SYM *)Safe_Malloc (sizeof (SYM));
	sp->name = (char *)nl.n_un.n_strx;
	sp->value = nl.n_value;
	*nextp = sp;
	nextp = &sp->next;
	*nextp = 0;
    }
    if (fread ((char *)&strsiz, sizeof (strsiz), 1, f) != 1) {
strerr:
	Free_Symbols (tab);
	(void)fclose (f);
	Primitive_Error ("corrupt string table in object file");
    }
    if (strsiz <= 4)
	goto strerr;
    tab->strings = Safe_Malloc (strsiz);
    strsiz -= 4;
    if (fread (tab->strings+4, 1, strsiz, f) != strsiz)
	goto strerr;
    for (sp = tab->first; sp; sp = sp->next)
	sp->name = tab->strings + (long)sp->name;
    return tab;
}

SYMTAB *Open_File_And_Snarf_Symbols (char *name) {
    struct exec hdr;
    int fd;
    FILE *fp;
    SYMTAB *tab;

    if ((fd = open (name, O_RDONLY|O_BINARY)) == -1) {
	Saved_Errno = errno;
	Primitive_Error ("can't open a.out file: ~E");
    }
    if (read (fd, (char *)&hdr, sizeof hdr) != sizeof hdr) {
	close (fd);
	Primitive_Error ("can't read a.out header");
    }
    if ((fp = fdopen (fd, O_BINARY ? "rb" : "r")) == NULL) {
	close (fd);
	Primitive_Error ("can't fdopen a.out file");
    }
    tab = Snarf_Symbols (fp, &hdr);
    (void)fclose (fp);
    return tab;
}
