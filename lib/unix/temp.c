/* temp.c
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

#include "unix.h"

#ifdef HAVE_TEMPNAM    /* Make sure only one of these is defined (if any) */
#  undef HAVE_TMPNAM   /* Order of preference: tempnam, mktemp, tmpnam */
#  undef HAVE_MKTEMP
#endif
#ifdef HAVE_MKTEMP
#  undef HAVE_TMPNAM
#  undef HAVE_TEMPNAM
#endif
#ifdef HAVE_TMPNAM
#  undef HAVE_TEMPNAM
#  undef HAVE_MKTEMP
#endif

static Object P_Tempname(argc, argv) int argc; Object *argv; {
    char *name, *dir = 0, *pref = 0;
    Object ret;
#ifdef HAVE_TMPNAM
    extern char *tmpnam();
#else
#ifdef HAVE_TEMPNAM
    extern char *tempnam();
#else
    char buf[1024];
#ifdef HAVE_MKTEMP
    extern char *mktemp();
#else
    char *p, *q;
    struct stat st;
#endif
#endif
#endif

    if (argc > 0)
        dir = Get_Strsym(argv[0]);
    if (argc > 1)
        pref = Get_Strsym(argv[1]);
#ifdef HAVE_TMPNAM
    name = tmpnam((char *)0);
#else
#ifdef HAVE_TEMPNAM
    Disable_Interrupts;        /* Make sure result gets freed */
    name = tempnam(dir, pref);
#else
    if (!dir) dir = "/tmp";
    if (!pref) pref = "elk";
    if (strlen(dir) + strlen(pref) > 1000)
        Primitive_Error("directory/prefix argument too long");
#ifdef HAVE_MKTEMP
    sprintf(buf, "%s/%sXXXXXX", dir, pref);
    name = mktemp(buf);
#else
    name = 0;
    sprintf(buf, "%s/%sa%d", dir, pref, getpid());
    p = buf+strlen(dir)+strlen(pref)+1;
    while (stat(buf, &st) == 0) {         /* Simple ersatz mktemp */
        q = p;
        while (1) {
            if (*q == '\0') goto fail;
            if (*q == 'z') {
                *q++ = 'a';
            } else {
                if (*q >= '0' && *q <= '9')
                    *q = 'a';
                else
                    *++q;
                break;
            }
        }
    }
    if (errno == ENOENT)
        name = buf;
fail: ;
#endif
#endif
#endif
    if (name == 0 || name[0] == '\0') {
        Enable_Interrupts;
        Raise_Error("cannot create temp file name");
    }
    ret = Make_String(name, strlen(name));
#ifdef HAVE_TEMPNAM
    free(name);
    Enable_Interrupts;
#endif
    return ret;
}

elk_init_unix_temp() {
    Def_Prim(P_Tempname,           "unix-tempname",             0, 2, VARARGS);
}
