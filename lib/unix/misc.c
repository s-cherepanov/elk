#include "unix.h"

static Object P_Getpass(prompt) Object prompt; {
    char *ret;
    extern char *getpass();

    Disable_Interrupts;
    ret = getpass(Get_String(prompt));
    Enable_Interrupts;
    if (ret == 0)
        Raise_Error("cannot read password from /dev/tty");
    return Make_String(ret, strlen(ret));
}

elk_init_unix_misc() {
    Def_Prim(P_Getpass,          "unix-getpass",                1, 1, EVAL);
}
