/* stab-elf.c
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

#include "config.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <libelf.h>
#include <unistd.h>
#include <string.h>

#include "kernel.h"

SYMTAB *
Snarf_Symbols (lf)
     int        lf;
{
    SYMTAB      *tab = NULL;
    register SYM *sp, **nextp;
    Elf         *elf_ptr;
    Elf_Scn     *elf_scn_ptr = NULL, *symtab_scn_ptr = NULL;
    Elf_Data    *elf_data_ptr = NULL;
    Elf32_Ehdr  *elf_ehdr_ptr = NULL;
    Elf32_Shdr  *elf_shdr_ptr = NULL,
                *symtab_ptr = NULL;
    size_t      elf_str_index = 0, shstrndx;
    char        *section_name;

    if (elf_version (EV_CURRENT) == EV_NONE)
      Primitive_Error ("a.out file Elf version out of date");
    if ((elf_ptr = elf_begin (lf, ELF_C_READ, (Elf *)NULL)) == NULL)
      Primitive_Error ("can't elf_begin() a.out file");

    /*
     * get the elf header, so we'll know where to look for the section
     * names.
     */
    if ((elf_ehdr_ptr = elf32_getehdr (elf_ptr)) == NULL) {
        Primitive_Error ("no elf header in a.out file");
    }
    shstrndx = elf_ehdr_ptr->e_shstrndx;
    /* look for the symbol and string tables */
    while ((elf_scn_ptr = elf_nextscn (elf_ptr, elf_scn_ptr))) {
        if ((elf_shdr_ptr = elf32_getshdr (elf_scn_ptr)) == NULL)
          Primitive_Error ("can't get section header in a.out file");
        if (elf_shdr_ptr->sh_type == SHT_STRTAB) {
            /*
             * save the index to the string table for later use by
             * elf_strptr().
             */
            section_name = elf_strptr (elf_ptr, shstrndx,
                                      (size_t)elf_shdr_ptr->sh_name);
            if (strcmp (section_name, ".strtab") == 0 ||
                    strcmp (section_name, ".dynstr") == 0) {
                elf_str_index = elf_ndxscn (elf_scn_ptr);
            }
        }
        else if (elf_shdr_ptr->sh_type == SHT_SYMTAB ||
                 elf_shdr_ptr->sh_type == SHT_DYNSYM) {
            symtab_ptr = elf_shdr_ptr;
            symtab_scn_ptr = elf_scn_ptr;
        }
    }
    if (!symtab_ptr)
      Primitive_Error ("no symbol table in a.out file");
    if (!elf_str_index)
      Primitive_Error ("no string table in a.out file");
    /*
     * we've located the symbol table -- go through it and save the names
     * of the interesting symbols.
     */
    while ((elf_data_ptr = elf_getdata (symtab_scn_ptr, elf_data_ptr))) {
        char    *name = NULL;
        int     symbol_count;
        Elf32_Sym       *symbol_ptr = elf_data_ptr->d_buf;
        Elf32_Sym       *current_symbol;

        tab = (SYMTAB *)Safe_Malloc (sizeof (SYMTAB));
        tab->first = 0;
        tab->strings = 0;
        nextp = &tab->first;
        for (symbol_count = 1;
             /* < was <= in the version I received from the author, but
              * the last entry is always undefined:
              */
             symbol_count < symtab_ptr->sh_size / symtab_ptr->sh_entsize;
             symbol_count++) {
            current_symbol = symbol_ptr + symbol_count;
            if (ELF32_ST_TYPE(current_symbol->st_info) != STT_FUNC ||
                ELF32_ST_BIND(current_symbol->st_info) != STB_GLOBAL) {
                continue;
            }
            if ((name = elf_strptr (elf_ptr, elf_str_index,
                                   (size_t)current_symbol->st_name)) == NULL) {
                Free_Symbols (tab);
                (void)close (lf);
                Primitive_Error (elf_errmsg (elf_errno ()));
            }
            sp = (SYM *)Safe_Malloc (sizeof (SYM));
            sp->name = Safe_Malloc (strlen (name) + 1);
            strcpy (sp->name, name);
            sp->value = current_symbol->st_value;
            *nextp = sp;
            nextp = &sp->next;
            *nextp = 0;
        }
    }
    return tab;
}

SYMTAB *
Open_File_And_Snarf_Symbols (name)
     char *name;
{
    int         f;
    SYMTAB      *tab;

    if ((f = open (name, O_RDONLY)) == -1)
        Primitive_Error ("can't open a.out file");
    tab = Snarf_Symbols (f);
    (void)close (f);
    return tab;
}

