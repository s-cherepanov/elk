#include AOUT_H
#include <sys/types.h>

/* On the HP9000 an nlist entry contains a fixed length
 * part consisting of the symbol information, plus a variable
 * length part, the name without a '\0' terminator.
 * We don't know how much space to allocate for the names
 * until we have read them all.
 * The solution here is to save all the names on the fly
 * in a table that is grown in units of STRING_BLOCK bytes,
 * using realloc to expand it on demand.
 */

#define STRING_BLOCK 8192

SYMTAB *Snarf_Symbols (FILE *f, struct exec *ep) {
    SYMTAB       *tab;
    register SYM *sp;
    register SYM **nextp;
    int          strsiz = 0; /* running total length of names read, */
			     /*   each '\0' terminated */
    int          nread = 0;  /* running total of bytes read from symbol table */
    int          max = 0;    /* current maximum size of name table */
    char         *names = 0; /* the name table */
    struct nlist_ nl;

    tab = (SYMTAB *)Safe_Malloc (sizeof (SYMTAB));
    tab->first = 0;
    tab->strings = 0;
    nextp = &tab->first;

    (void)fseek (f, (long)LESYM_OFFSET(*ep), 0);

    while (nread < ep->a_lesyms) {
	if (fread ((char *)&nl, sizeof (nl), 1, f) != 1) {
	    Free_Symbols (tab);
	    (void)fclose (f);
	    Primitive_Error ("corrupt symbol table in object file");
	}

        nread += sizeof (nl);

	if (nl.n_length == 0) {
            continue;
        }
        else if (nl.n_length + strsiz + 1 > max) {
            max += STRING_BLOCK;
            names = Safe_Realloc (names, max);
        }

        if (fread (names + strsiz, 1, nl.n_length, f) != nl.n_length) {
	    Free_Symbols (tab);
	    (void)fclose (f);
	    Primitive_Error ("corrupt symbol table in object file");
	}
        else {
            nread += nl.n_length;
            names[ strsiz + nl.n_length ] = '\0';
        }
	if ((nl.n_type & N_TYPE) != N_TEXT) {
	    strsiz += nl.n_length +1;
	    continue;
	}
	sp = (SYM *)Safe_Malloc (sizeof (SYM));
	sp->name = (char *)strsiz;
        strsiz += (nl.n_length + 1);
	sp->value = nl.n_value;
	*nextp = sp;
	nextp = &sp->next;
	*nextp = 0;
    }

    tab->strings = names;

    for (sp = tab->first; sp; sp = sp->next)
	sp->name += (unsigned int)names;

    return tab;
}

#ifdef INIT_OBJECTS
SYMTAB *Open_File_And_Snarf_Symbols (char *name) {
    struct exec hdr;
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
#endif /* INIT_OBJECTS */
