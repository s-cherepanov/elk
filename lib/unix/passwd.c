#include "unix.h"

#include <pwd.h>
#include <grp.h>

static Object P_Get_Passwd(argc, argv) int argc; Object *argv; {
    struct passwd *p;
    Object arg, x;

    Check_Result_Vector(argv[0], 7);
    Disable_Interrupts;
    if (argc == 1) {
	if ((p = getpwent()) == 0) {
	    Enable_Interrupts;
	    Raise_Error("no more passwd entries");
	}
    } else {
	arg = argv[1];
	switch (TYPE(arg)) {
	case T_Fixnum: case T_Bignum:
	    p = getpwuid(Get_Integer(arg));
	    break;
	case T_String: case T_Symbol:
	    p = getpwnam(Get_String(arg));
	    break;
	default:
	    Wrong_Type_Combination(arg, "integer, string, or symbol");
	}
	if (p == 0) {
	    Enable_Interrupts;
	    Raise_Error1("no passwd entry for ~s", arg);
	}
    }
    Enable_Interrupts;
    x = Make_String(p->pw_name, strlen(p->pw_name));
    VECTOR(argv[0])->data[0] = x;
    x = Make_String(p->pw_passwd, strlen(p->pw_passwd));
    VECTOR(argv[0])->data[1] = x;
    x = Make_Integer(p->pw_uid);
    VECTOR(argv[0])->data[2] = x;
    x = Make_Integer(p->pw_gid);
    VECTOR(argv[0])->data[3] = x;
    x = Make_String(p->pw_gecos, strlen(p->pw_gecos));
    VECTOR(argv[0])->data[4] = x;
    x = Make_String(p->pw_dir, strlen(p->pw_dir));
    VECTOR(argv[0])->data[5] = x;
    x = Make_String(p->pw_shell, strlen(p->pw_shell));
    VECTOR(argv[0])->data[6] = x;
    return Void;
}

static Object P_Rewind_Passwd() {
    Disable_Interrupts;
    setpwent();
    Enable_Interrupts;
    return Void;
}

static Object P_End_Passwd() {
    Disable_Interrupts;
    endpwent();
    Enable_Interrupts;
    return Void;
}

static Object P_Get_Group(argc, argv) int argc; Object *argv; {
    char **pp;
    struct group *p;
    Object arg, member, x;
    GC_Node;

    Check_Result_Vector(argv[0], 4);
    Disable_Interrupts;
    if (argc == 1) {
	if ((p = getgrent()) == 0) {
	    Enable_Interrupts;
	    Raise_Error("no more group entries");
	}
    } else {
	arg = argv[1];
	switch (TYPE(arg)) {
	case T_Fixnum: case T_Bignum:
	    p = getgrgid(Get_Integer(arg));
	    break;
	case T_String: case T_Symbol:
	    p = getgrnam(Get_String(arg));
	    break;
	default:
	    Wrong_Type_Combination(arg, "integer, string, or symbol");
	}
	if (p == 0) {
	    Enable_Interrupts;
	    Raise_Error1("no group entry for ~s", arg);
	}
    }
    Enable_Interrupts;
    x = Make_String(p->gr_name, strlen(p->gr_name));
    VECTOR(argv[0])->data[0] = x;
    x = Make_String(p->gr_passwd, strlen(p->gr_passwd));
    VECTOR(argv[0])->data[1] = x;
    x = Make_Integer(p->gr_gid);
    VECTOR(argv[0])->data[2] = x;
    x = Null;
    GC_Link(x);
    for (pp = p->gr_mem; *pp; pp++) {
	member = Intern(*pp);
	x = Cons(member, x);
    }
    x = P_Reverse(x);
    GC_Unlink;
    VECTOR(argv[0])->data[3] = x;
    return Void;
}

static Object P_Rewind_Group() {
    Disable_Interrupts;
    setgrent();
    Enable_Interrupts;
    return Void;
}

static Object P_End_Group() {
    Disable_Interrupts;
    endgrent();
    Enable_Interrupts;
    return Void;
}
    
elk_init_unix_passwd() {
    Def_Prim(P_Get_Passwd,    "unix-get-passwd-vector-fill!",   1, 2, VARARGS);
    Def_Prim(P_Rewind_Passwd, "unix-rewind-passwd",             0, 0, EVAL);
    Def_Prim(P_End_Passwd,    "unix-end-passwd",                0, 0, EVAL);
    Def_Prim(P_Get_Group,     "unix-get-group-vector-fill!",    1, 2, VARARGS);
    Def_Prim(P_Rewind_Group,  "unix-rewind-group",              0, 0, EVAL);
    Def_Prim(P_End_Group,     "unix-end-group",                 0, 0, EVAL);
}
