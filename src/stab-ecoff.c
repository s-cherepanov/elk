/* stab-ecoff.c
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

/* On the SGI, <a.out.h> includes a file that defines a variable named
 * auxtemp.  This causes the linker to complain about this variable
 * being multiply defined, because <a.out.h> was already included by
 * load.vanilla.c.
 */
#define _auxtemp Auxtemp

#include AOUT_H

SYMTAB *Snarf_Symbols (FILE *fp) {
    long fdi;                   /* a counter for the file desc table */
    FDR *file_desc;             /* pointer to the filedesc table */
    struct filehdr file_hdr;    /* pointer to the file header */
    char *strbase;
    HDRR sym_hdr;               /* pointer to symbolic header */
    long symi;                  /* a counter for the local symbol table */
    SYMR *symbol;               /* pointer to symbol table */

    SYMTAB *tab;
    char *p;
    SYM *sp, **nextp;

    Alloca_Begin;

    /* Read file header and symbolic header
     */
    (void)rewind (fp);
    if (fread ((char *)&file_hdr, sizeof (file_hdr), 1, fp) == 0) {
        fclose (fp);
        Primitive_Error ("cannot read a.out file header");
    }
    (void)fseek (fp, file_hdr.f_symptr, SEEK_SET);
    if (fread ((char *)&sym_hdr, sizeof (sym_hdr), 1, fp) == 0) {
        fclose (fp);
        Primitive_Error ("cannot read a.out symbolic header");
    }

    tab = (SYMTAB *)Safe_Malloc (sizeof (SYMTAB));
    tab->first = 0;
    tab->strings = 0;
    nextp = &tab->first;

    /* Read symbol table
     */
    Alloca (symbol, SYMR*, sym_hdr.isymMax * sizeof (SYMR));
    (void)fseek (fp, sym_hdr.cbSymOffset, SEEK_SET);
    if (fread ((char *)symbol, sizeof (SYMR), sym_hdr.isymMax, fp) == 0) {
symerr:
        fclose (fp);
        Free_Symbols (tab);
        Primitive_Error ("cannot read symbol/string/fd table");
    }

    /* Read string table
     */
    tab->strings = Safe_Malloc (sym_hdr.issMax);
    (void)fseek (fp, sym_hdr.cbSsOffset, SEEK_SET);
    if (fread (tab->strings, sym_hdr.issMax, 1, fp) == 0)
        goto symerr;

    /* Read file descriptor table
     */
    Alloca (file_desc, FDR*, sym_hdr.ifdMax * sizeof (FDR));
    (void)fseek (fp, sym_hdr.cbFdOffset, SEEK_SET);
    if (fread ((char *)file_desc, sizeof (FDR), sym_hdr.ifdMax, fp) == 0)
        goto symerr;

    /* For each file in the file descriptor table do:
     */
    for (fdi = 0; fdi < sym_hdr.ifdMax; fdi++) {
        strbase = tab->strings + file_desc[fdi].issBase;
        for (symi = file_desc[fdi].isymBase;
                symi < file_desc[fdi].csym + file_desc[fdi].isymBase;
                symi++) {
            if (symbol[symi].st == stProc && symbol[symi].sc == scText) {
                p = symbol[symi].iss + strbase;

                /* Allocate and initialize node in the symbol table list;
                 * link node into list
                 */
                sp = (SYM *)Safe_Malloc (sizeof (SYM));
                sp->name = Safe_Malloc (strlen (p) + 1);
                strcpy (sp->name, p);
                sp->value = symbol[symi].value;
                *nextp = sp;
                nextp = &sp->next;
                *nextp = 0;
            }
        }
    }
    Alloca_End;
    return tab;
}

SYMTAB *Open_File_And_Snarf_Symbols (char *name) {
    FILE *fp;
    SYMTAB *tab;

    if ((fp = fopen (name, "r")) == NULL)
        Primitive_Error ("can't open a.out file");
    tab = Snarf_Symbols (fp);
    (void)fclose (fp);
    return tab;
}
