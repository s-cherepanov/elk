/* stab-macho.c
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

#include <nlist.h>
#include <mach-o/rld.h>

static SYMTAB *Grovel_Over_Nlist (symcmd, nl, strtab, text_sect)
    struct symtab_command *symcmd; /* ptr to MACH-O symtab command */
    struct nlist nl[];             /* ptr to symbol table */
    char *strtab;                  /* ptr to string table */
    long text_sect;                /* # of text section */ {

    SYMTAB *tab;
    register SYM *sp, **nextp;
    long i;

    tab = (SYMTAB *) Safe_Malloc (sizeof (SYMTAB));
    tab->first = 0;
    tab->strings = 0;
    nextp = &tab->first;

    /* Grovel over the file's nlist, extracting global symbols that
     * have a section mumber equal to the number of the text section:
     */
    for (i = 0; i < symcmd->nsyms; i++) {
        if ((nl[i].n_type & (N_TYPE|N_EXT)) == (N_SECT|N_EXT) &&
                nl[i].n_sect == text_sect) {
            sp = (SYM *)Safe_Malloc (sizeof (SYM));
            sp->name = strtab + nl[i].n_un.n_strx;
            sp->value = nl[i].n_value;
            sp->next = 0;
            *nextp = sp;
            nextp = &sp->next;
        }
    }
    return tab;
}

SYMTAB *Snarf_Symbols (struct mach_header *mhdr) {
    struct load_command *ld_cmd;
    struct symtab_command *sym_cmd;
    struct segment_command *seg_cmd;
    struct section *sp;
    struct nlist *symtab = 0;
    char *cmdptr, *strtab;
    long i, j, text_sect = 0;

    /* Loop through the load commands, find the symbol table and
     * the segment command carrying the text section to determine
     * the number of the text section:
     */
    cmdptr = (char *)mhdr + sizeof (struct mach_header);
    for (i = 0; i < mhdr->ncmds; i++) {
        ld_cmd = (struct load_command *)cmdptr;
        if (ld_cmd->cmd == LC_SYMTAB && !symtab) {
            sym_cmd = (struct symtab_command *)ld_cmd;
            symtab = (struct nlist *)((char *)mhdr + sym_cmd->symoff);
            strtab = (char *)mhdr + sym_cmd->stroff;
        } else if (ld_cmd->cmd == LC_SEGMENT && !text_sect) {
            seg_cmd = (struct segment_command *)ld_cmd;
            sp = (struct section *)
                ((char *)ld_cmd + sizeof (struct segment_command));
            for (j = 1; j <= seg_cmd->nsects && !text_sect; j++, sp++)
                if (strcmp (sp->sectname, SECT_TEXT) == 0)
                    text_sect = j;
        }
        cmdptr += ld_cmd->cmdsize;
    }
    if (!symtab)
        Primitive_Error ("couldn't find symbol table in object file");
    if (!text_sect)
        Primitive_Error ("couldn't find text section in object file");
    return Grovel_Over_Nlist (sym_cmd, symtab, strtab, text_sect);
}

#ifdef INIT_OBJECTS
SYMTAB *Open_File_And_Snarf_Symbols (char *name) {
    extern char *_mh_execute_header;
    return Snarf_Symbols ((struct mach_header *)&_mh_execute_header);
}
#endif /* INIT_OBJECTS */
