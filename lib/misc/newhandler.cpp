#include "scheme.h"

#include <new>

#ifdef USE_ATTC_PLUS_PLUS
#  define set_new_handler set_new_handler__FPFv_v
#endif

static Object New_Handler;

static void New_Handler_Proc () {
    (void)Funcall (New_Handler, Null, 0);
}

static Object P_Set_New_Handler (Object p) {
    Object old;

    Check_Procedure (p);
    old = New_Handler;
    New_Handler = p;
    return old;
}

extern "C" void elk_init_lib_cplusplus () {
    New_Handler = Null;
    Global_GC_Link (New_Handler);
    std::new_handler (New_Handler_Proc);
    Define_Primitive ((Object (*)())P_Set_New_Handler, "set-c++-new-handler!", 1, 1, EVAL);
}
