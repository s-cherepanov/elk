#include AOUT_H
#include <sys/types.h>

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
