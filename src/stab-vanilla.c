/* stab-vanilla.c: naive symbol finder for shared objects; simply finds all
 * strings in the object file, without asking questions.
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

#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>

#include "kernel.h"

SYMTAB *Snarf_Symbols (FILE *f) {
    SYMTAB *tab;
    register SYM *sp, **nextp;
    unsigned char buffer[BUFSIZ];
    int i = 0, start = 0, end = 0;

    tab = (SYMTAB *)Safe_Malloc (sizeof (SYMTAB));
    tab->first = 0;
    tab->strings = 0;
    nextp = &tab->first;

    while (start != end || !feof(f)) {
        if (i == end) {
            /* If we filled our buffer without finding a string, we set
             * start = end so that the buffer is completely emptied */
            if(start == 0 && end != 0)
                start = end;

            /* Fill our buffer some more */
            memmove(buffer, buffer + start, end - start);
            end -= start;
            start = 0;
            i = fread(buffer + end, 1, BUFSIZ - end, f);
            if(i == 0) break;
            end += i;
        }

        for (i = start; i < end; i++)
            if(buffer[i] >= 0x20 && buffer[i] < 0x80) break;

        if (i == end)
            continue;

        start = i;

        for (i = start + 1; i < end; i++)
            if(buffer[i] == 0) break;

        if (i == end)
            continue;

        /* If this string contains INIT_PREFIX or FINIT_PREFIX, it's
         * potentially a valid symbol. */
        if (strstr(buffer + start, INIT_PREFIX)
             || strstr(buffer + start, FINIT_PREFIX)) {

            sp = tab->first;
            while(sp) {
                if(!strcmp(sp->name, buffer + start)) {
                    /* We have already seen this symbol; abort */
                    goto next_string;
                }
                sp = sp->next;
            }

            /* We found a valid string, link it to the symbol list */
            sp = (SYM *)Safe_Malloc (sizeof (SYM));
            sp->name = Safe_Malloc (strlen (buffer + start) + 1);
            strcpy (sp->name, buffer + start);
            sp->value = 0;
            *nextp = sp;
            nextp = &sp->next;
            *nextp = NULL;
        }

next_string:
        start = i + 1;
    }

    return tab;
}

SYMTAB *Open_File_And_Snarf_Symbols (char *name) {
    int fd;
    FILE *fp;
    SYMTAB *tab;

#ifdef O_BINARY
    fd = open (name, O_RDONLY|O_BINARY);
#else
    fd = open (name, O_RDONLY);
#endif
    if (fd == -1) {
        Saved_Errno = errno;
        Primitive_Error ("can't open file: ~E");
    }
    fp = fdopen (fd, "rb");
    if (fp == NULL) {
        close (fd);
        Primitive_Error ("can't fdopen file");
    }
    tab = Snarf_Symbols (fp);
    (void)fclose (fp);
    return tab;
}

