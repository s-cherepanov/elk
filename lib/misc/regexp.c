/* The regular expression extension.  It provides Scheme language
 * bindings to the POSIX regcomp/regexec functions.
 *
 * Inspired by a GNU regular expression extension contributed by
 * Stephen J. Bevan to an earlier version of Elk.
 */

#include "scheme.h"

#ifdef REGCOMP

#include <sys/types.h>
#include <regex.h>

#define REGEXP(x)   ((struct S_Regexp *)POINTER(x))
#define MATCH(x)    ((struct S_Match *)POINTER(x))

struct S_Regexp {
    Object pattern;
    regex_t r;
    int flags;
};

struct S_Match {
    Object tag;
    size_t num;
    regmatch_t matches[1];
};

int T_Regexp, T_Match;

static SYMDESCR Compile_Syms[] = {
    { "extended",    REG_EXTENDED },
    { "ignore-case", REG_ICASE },
    { "no-subexpr",  REG_NOSUB },
    { "newline",     REG_NEWLINE },
    { 0, 0 }
};

static SYMDESCR Exec_Syms[] = {
    { "not-bol",     REG_NOTBOL },
    { "not-eol",     REG_NOTEOL },
    { 0, 0 }
};

static Object P_Regexpp(x) Object x; {
    return TYPE(x) == T_Regexp ? True : False;
}

static Object P_Matchp(x) Object x; {
    return TYPE(x) == T_Match ? True : False;
}

static Regexp_Eqv(a, b) Object a, b; {
    return EQ(REGEXP(a)->pattern, REGEXP(b)->pattern)
	&& REGEXP(a)->flags == REGEXP(b)->flags;
}

static Regexp_Equal(a, b) Object a, b; {
    return Equal(REGEXP(a)->pattern, REGEXP(b)->pattern)
	&& REGEXP(a)->flags == REGEXP(b)->flags;
}

static Match_Equal(a, b) Object a, b; {
    size_t i;
    struct S_Match *ap = MATCH(a), *bp = MATCH(b);

    if (ap->num != bp->num)
	return 0;
    for (i = 0; i < ap->num; i++) {
	if (ap->matches[i].rm_so != bp->matches[i].rm_so ||
	    ap->matches[i].rm_eo != bp->matches[i].rm_eo)
		return 0;
    }
    return 1;
}

static int Match_Size(m) Object m; {
    return sizeof(struct S_Match) + (MATCH(m)->num - 1) * sizeof(regmatch_t);
}

static Regexp_Visit(p, f) Object *p; int (*f)(); {
    f(&REGEXP(*p)->pattern);
}

static Regexp_Print(x, port, raw, depth, length) Object x, port; {
    Format(port, "#[regexp ~s]", 12, 1, &REGEXP(x)->pattern);
}

static Match_Print(x, port, raw, depth, length) Object x, port; {
    Printf(port, "#[regexp-match %lu]", POINTER(x));
}

static Object Terminate_Regexp(r) Object r; {
    regfree(&REGEXP(r)->r);
    return Void;
}

static Object P_Make_Regexp(argc, argv) Object *argv; {
    Object r;
    char *s;
    char msg[256];
    int flags = 0, ret;

    Check_Type(argv[0], T_String);
    if (argc == 2)
	flags = Symbols_To_Bits(argv[1], 1, Compile_Syms);
    r = Alloc_Object(sizeof(struct S_Regexp), T_Regexp, 0);
    REGEXP(r)->pattern = argv[0];
    REGEXP(r)->flags = flags;
    ret = regcomp(&REGEXP(r)->r, Get_String(argv[0]), flags);
    if (ret != 0) {
#ifdef REG_ENOSYS
	if (ret == REG_ENOSYS)
	    Primitive_Error("function not supported by operating system");
#endif
	(void)regerror(ret, &REGEXP(r)->r, msg, sizeof(msg));
	Primitive_Error("~a", Make_String(msg, strlen(msg)));
    }
    Register_Object(r, (GENERIC)0, Terminate_Regexp, 0);
    return r;
}

static Object P_Regexp_Pattern(r) Object r; {
    Check_Type(r, T_Regexp);
    return REGEXP(r)->pattern;
}

static Object P_Regexp_Flags(r) Object r; {
    Check_Type(r, T_Regexp);
    return Bits_To_Symbols((unsigned long)REGEXP(r)->flags, 1, Compile_Syms);
}

static Object P_Regexp_Exec(argc, argv) Object *argv; {
    char *str, msg[256];
    Object r, m;
    size_t num;
    int from, flags, ret;
    GC_Node;

    r = argv[0];
    Check_Type(r, T_Regexp);
    Check_Type(argv[1], T_String);
    str = Get_String(argv[1]);
    from = Get_Unsigned(argv[2]);
    if (from > STRING(argv[1])->size)
	Range_Error(argv[2]);
    if (argc == 4)
	flags = (int)Symbols_To_Bits(argv[3], 1, Exec_Syms);
    else
	flags = 0;
    if (REGEXP(r)->flags & REG_NOSUB)
	num = 1;
    else
	num = REGEXP(r)->r.re_nsub + 1;
    GC_Link(r);
    m = Alloc_Object(sizeof(struct S_Match) + (num-1) * sizeof(regmatch_t),
	T_Match, 0);
    GC_Unlink;
    MATCH(m)->tag = Null;
    if (REGEXP(r)->flags & REG_NOSUB)
	num = 0;
    MATCH(m)->num = num;
    ret = regexec(&REGEXP(r)->r, str+from, num, MATCH(m)->matches, flags);
    if (ret == 0)
	return m;
    if (ret == REG_NOMATCH)
	return False;
    (void)regerror(ret, &REGEXP(r)->r, msg, sizeof(msg));
    Primitive_Error("~a", Make_String(msg, strlen(msg)));
    /*NOTREACHED*/
}

static Object P_Match_Number(m) Object m; {
    Check_Type(m, T_Match);
    return Make_Unsigned_Long((unsigned long)MATCH(m)->num);
}

static Object P_Match_Start(m, n) Object m, n; {
    size_t i;

    Check_Type(m, T_Match);
    i = (size_t)Get_Unsigned_Long(n);
    if (i >= MATCH(m)->num)
	Range_Error(n);
    return Make_Unsigned_Long((unsigned long)MATCH(m)->matches[i].rm_so);
}

static Object P_Match_End(m, n) Object m, n; {
    size_t i;

    Check_Type(m, T_Match);
    i = (size_t)Get_Unsigned_Long(n);
    if (i >= MATCH(m)->num)
	Range_Error(n);
    return Make_Unsigned_Long((unsigned long)MATCH(m)->matches[i].rm_eo);
}


#define Def_Prim Define_Primitive

#endif /* REGCOMP */

elk_init_lib_regexp() {
#ifdef REGCOMP
    T_Regexp = Define_Type(0, "regexp", 0, sizeof(struct S_Regexp),
	Regexp_Eqv, Regexp_Equal, Regexp_Print, Regexp_Visit);
    T_Match = Define_Type(0, "regexp-match", Match_Size, 0,
	Match_Equal, Match_Equal, Match_Print, 0);
    Def_Prim(P_Regexpp,       "regexp?",                  1, 1, EVAL);
    Def_Prim(P_Matchp,        "regexp-match?",            1, 1, EVAL);
    Def_Prim(P_Make_Regexp,   "make-regexp",              1, 2, VARARGS);
    Def_Prim(P_Regexp_Pattern,"regexp-pattern",           1, 1, EVAL);
    Def_Prim(P_Regexp_Flags,  "regexp-flags",             1, 1, EVAL);
    Def_Prim(P_Regexp_Exec,   "regexp-exec",              3, 4, VARARGS);
    Def_Prim(P_Match_Number,  "regexp-match-number",      1, 1, EVAL);
    Def_Prim(P_Match_Start,   "regexp-match-start",       2, 2, EVAL);
    Def_Prim(P_Match_End,     "regexp-match-end",         2, 2, EVAL);
    P_Provide(Intern(":regular-expressions"));
#endif
    P_Provide(Intern ("regexp.so"));
    P_Provide(Intern ("regexp.o"));
}
