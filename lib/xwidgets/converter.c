/* converter.c
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

#include "xt.h"

#define MAX_CONVERTER   512

typedef struct {
    char *name;
    int scheme_to_c;
    PFX2S to_scheme;
    PFS2X to_c;
} CONVERTER;

static CONVERTER ctab[MAX_CONVERTER], *clast = ctab;

void Define_Converter_To_Scheme (char *name, PFX2S c) {
    Set_Error_Tag ("c->scheme");
    if (clast == ctab+MAX_CONVERTER)
        Primitive_Error ("too many converters");
    clast->name = name;
    clast->scheme_to_c = 0;
    clast->to_scheme = c;
    clast++;
}

void Define_Converter_To_C (char *name, PFS2X c) {
    Set_Error_Tag ("scheme->c");
    if (clast == ctab+MAX_CONVERTER)
        Primitive_Error ("too many converters");
    clast->name = name;
    clast->scheme_to_c = 1;
    clast->to_c = c;
    clast++;
}

PFX2S Find_Converter_To_Scheme (char *name) {
    register CONVERTER *p;

    for (p = ctab; p < clast; p++)
        if (!p->scheme_to_c && streq (p->name, name))
            return p->to_scheme;
    return 0;
}

PFS2X Find_Converter_To_C (char *name) {
    register CONVERTER *p;

    for (p = ctab; p < clast; p++)
        if (p->scheme_to_c && streq (p->name, name))
            return p->to_c;
    return 0;
}
