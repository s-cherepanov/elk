#include "scheme.h"

static Object P_Debug (on) Object on; {
    Check_Type (on, T_Boolean);
    GC_Debug = EQ(on, True);
    return Void;
}

elk_init_lib_debug () {
    Define_Primitive (P_Debug, "debug", 1, 1, EVAL);
}
