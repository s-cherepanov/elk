/* stab-unix.c
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

#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/types.h>

#include "kernel.h"

SYMTAB *Snarf_Symbols (FILE *f) {
    SYMTAB *tab;
    register SYM *sp, **nextp;

    tab = (SYMTAB *)Safe_Malloc (sizeof (SYMTAB));
    tab->first = 0;
    tab->strings = 0;

    return tab;
}

SYMTAB *Open_File_And_Snarf_Symbols (char *name) {
    int fd;
    FILE *fp;
    SYMTAB *tab;

    fprintf (stderr, "trying to find symbols in %s\n", name);

#ifdef O_BINARY
    fd = open (name, O_RDONLY|O_BINARY);
#else
    fd = open (name, O_RDONLY);
#endif
    if (fd == -1) {
        Saved_Errno = errno;
        Primitive_Error ("can't open file: ~E");
    }
    if ((fp = fdopen (fd, "rb")) == NULL) {
        close (fd);
        Primitive_Error ("can't fdopen file");
    }
    tab = Snarf_Symbols (fp);
    (void)fclose (fp);
    return tab;
}

