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
