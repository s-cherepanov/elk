#include "xt.h"

#define MAX_CONVERTER   512

typedef struct {
    char *name;
    int scheme_to_c;
    PFX2S to_scheme;
    PFS2X to_c;
} CONVERTER;

static CONVERTER ctab[MAX_CONVERTER], *clast = ctab;

void Define_Converter_To_Scheme (name, c) char *name; PFX2S c; {
    Set_Error_Tag ("c->scheme");
    if (clast == ctab+MAX_CONVERTER)
	Primitive_Error ("too many converters");
    clast->name = name;
    clast->scheme_to_c = 0;
    clast->to_scheme = c;
    clast++;
}

void Define_Converter_To_C (name, c) char *name; PFS2X c; {
    Set_Error_Tag ("scheme->c");
    if (clast == ctab+MAX_CONVERTER)
	Primitive_Error ("too many converters");
    clast->name = name;
    clast->scheme_to_c = 1;
    clast->to_c = c;
    clast++;
}

PFX2S Find_Converter_To_Scheme (name) char *name; {
    register CONVERTER *p;

    for (p = ctab; p < clast; p++)
	if (!p->scheme_to_c && streq (p->name, name))
	    return p->to_scheme;
    return 0;
}

PFS2X Find_Converter_To_C (name) char *name; {
    register CONVERTER *p;

    for (p = ctab; p < clast; p++)
	if (p->scheme_to_c && streq (p->name, name))
	    return p->to_c;
    return 0;
}
