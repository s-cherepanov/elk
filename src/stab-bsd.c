#include AOUT_H
#include <fcntl.h>
#include <errno.h>
#include <sys/types.h>

extern int errno;

#ifndef O_BINARY
#  define O_BINARY 0
#endif

SYMTAB *Snarf_Symbols (f, ep) FILE *f; struct exec *ep; {
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

SYMTAB *Open_File_And_Snarf_Symbols (name) char *name; {
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
