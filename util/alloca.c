/* alloca.c: Check if the system's alloca() function actually extends the
 * stack. If it doesn't, it's not usable for Elk.
 *
 * The second value printed should be about 100 larger (or smaller,
 * depending on the stack growing direction) than the first value.
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

/* On some systems you may have to enable the #include and delete the line
 * declaring alloca().
 */

/* #include <alloca.h> */

extern char *alloca();

char *stkbase;

prstk(char *s) {
    char foo;

    printf("stack %s calling alloca(100): %lu\n", s, (long)(stkbase - &foo));
}

main(int ac, **av) {
    char *foo;

    stkbase = (char *)&foo;
    prstk("before");
    foo = alloca(100);
    prstk(" after");
    return 0;
}
