#include "scheme.h"

static Object P_Hack_Procedure_Environment (p, e) Object p, e; {
    Check_Type (p, T_Compound);
    Check_Type (e, T_Environment);
    COMPOUND(p)->env = e;
    return p;
}

elk_init_lib_hack () {
    Define_Primitive (P_Hack_Procedure_Environment,
	"hack-procedure-environment!", 2, 2, EVAL);
    P_Provide (Intern ("hack.o"));
}
