/* Built-in and user-defined Scheme types.
 */

#include "kernel.h"

#include <string.h>

#define TYPE_GROW    10

TYPEDESCR *Types;
int Num_Types, Max_Type;

char *builtin_types[] = {
    "0integer", "1integer" /* bignum */, "1real", "0null", "0boolean",
    "0unbound", "0special", "0character", "1symbol", "1pair",
    "1environment", "1string", "1vector", "1primitive", "1compound",
    "1control-point", "1promise", "1port", "0end-of-file", "1autoload",
    "1macro", "1!!broken-heart!!",
#ifdef GENERATIONAL_GC
    "0align_8byte", "0freespace",
#endif
    0
};

void Wrong_Type (Object x, register int t) {
    Wrong_Type_Combination (x, Types[t].name);
}

void Wrong_Type_Combination (Object x, register char const *name) {
    register int t = TYPE(x);
    char buf[100];

    if (t < 0 || t >= Num_Types)
	Panic ("bad type1");
    sprintf (buf, "wrong argument type %s (expected %s)",
	Types[t].name, name);
    Primitive_Error (buf);
}

Object P_Type (Object x) {
    register int t = TYPE(x);

    if (t < 0 || t >= Num_Types)
	Panic ("bad type2");
    return Intern (Types[t].name);
}

int Define_Type (register int t, char const *name,
	int (*size)(), int const_size, int (*eqv)(), int (*equal)(),
	int (*print)(), int (*visit)()) {
    register TYPEDESCR *p;

    Set_Error_Tag ("define-type");
    if (t != 0)
	Fatal_Error("first arg of Define_Type() must be 0");
    if (Num_Types == Max_Type) {
	Max_Type += TYPE_GROW;
	Types = (TYPEDESCR *)Safe_Realloc((char *)Types,
	    Max_Type * sizeof(TYPEDESCR));
    }
    Disable_Interrupts;
    p = &Types[Num_Types++];
    p->haspointer = 1;
    p->name = name;
    p->size = size;
    p->const_size = const_size;
    p->eqv = eqv;
    p->equal = equal;
    p->print = print;
    p->visit = visit;
    Enable_Interrupts;
    return Num_Types-1;
}

void Init_Type() {
    int i, bytes;
    char *p;

    Num_Types = (sizeof(builtin_types) - 1) / sizeof(char *);
    Max_Type = Num_Types + TYPE_GROW;
    bytes = Max_Type * sizeof(TYPEDESCR);
    Types = (TYPEDESCR *)Safe_Malloc(bytes);
    memset(Types, 0, bytes);
    for (i = 0; (p = builtin_types[i]); i++) {
	Types[i].haspointer = *p != '0';
	Types[i].name = ++p;
    }
}
