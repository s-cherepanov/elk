#include "xt.h"

XtAccelerators Get_Accelerators (a) Object a; {
    register char *s;
    XtAccelerators ret;
    Alloca_Begin;

    Get_Strsym_Stack (a, s);
    if ((ret = XtParseAcceleratorTable (s)) == 0)
        Primitive_Error ("bad accelerator table: ~s", a);
    Alloca_End;
    return ret;
}

static Object P_Install_Accelerators (dst, src) Object dst, src; {
    Check_Widget (dst);
    Check_Widget (src);
    XtInstallAccelerators (WIDGET(dst)->widget, WIDGET(src)->widget);
    return Void;
}

static Object P_Install_All_Accelerators (dst, src) Object dst, src; {
    Check_Widget (dst);
    Check_Widget (src);
    XtInstallAllAccelerators (WIDGET(dst)->widget, WIDGET(src)->widget);
    return Void;

}

elk_init_xt_accelerator () {
    Define_Primitive (P_Install_Accelerators,
                        "install-accelerators",       2, 2, EVAL);
    Define_Primitive (P_Install_All_Accelerators,
                        "install-all-accelerators",   2, 2, EVAL);
}
