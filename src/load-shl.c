#include <dl.h>
#include <string.h>

extern void Free_Symbols (SYMTAB *);
extern void Call_Initializers (SYMTAB *, char *, int);

extern int errno;

static void Load_Them (Object names) {
    char *fn;
    shl_t handle;
    SYM *sp;
    static struct obj_loaded {
	struct obj_loaded *next;
	char *name;
    } *loaded, *lp;
    GC_Node;
    Alloca_Begin;

    GC_Link(names);
    for ( ; !Nullp (names); names = Cdr (names)) {
	Get_Strsym_Stack (Car (names), fn);
	for (lp = loaded; lp; lp = lp->next)
	    if (strcmp (lp->name, fn) == 0) break;
	if (lp) continue;
	lp = (struct obj_loaded *)Safe_Malloc (sizeof (*lp));
	lp->name = strdup (fn);
	lp->next = loaded;
	loaded = lp;
	if (Verb_Load)
	    printf ("[shl_load %s]\n", fn);
	if ((handle = shl_load (fn, BIND_IMMEDIATE|BIND_VERBOSE, 0L)) == 0) {
	    Saved_Errno = errno;
	    Primitive_Error ("shl_load of ~s failed: ~E", Car (names));
	}
	if (The_Symbols)
	    Free_Symbols (The_Symbols);
	The_Symbols = Open_File_And_Snarf_Symbols (fn);
	for (sp = The_Symbols->first; sp; sp = sp->next)
	    if (shl_findsym (&handle, sp->name, TYPE_UNDEFINED, &sp->value)) {
		Saved_Errno = errno;
		Primitive_Error ("~s: shl_findsym on ~s failed: ~E",
		    Car (names),
		    Make_String (sp->name, strlen (sp->name)));
	}
	Call_Initializers (The_Symbols, 0, PR_CONSTRUCTOR);
	Call_Initializers (The_Symbols, 0, PR_EXTENSION);
    }
    GC_Unlink;
    Alloca_End;
}

Load_Object (Object names) {
    Object port, tail, fullnames, str;
    char *p, *libs = "";
    GC_Node3;
    Alloca_Begin;

    port = tail = fullnames = Null;
    GC_Link3 (port, tail, fullnames);
    for (tail = names; !Nullp (tail); tail = Cdr (tail)) {
	port = General_Open_File (Car (tail), P_INPUT, Var_Get (V_Load_Path));
	fullnames = Cons (PORT(port)->name, fullnames);
	(void)P_Close_Input_Port (port);
    }
    tail = Var_Get (V_Load_Libraries);
    if (TYPE(tail) == T_String)
	Get_Strsym_Stack (tail, libs);
    Disable_Interrupts;
    for (tail = Null; (p = strtok (libs, " \t")) != 0; libs = 0) {
	str = Make_String (p, strlen (p));
	tail = Cons (str, tail);
    }
    Load_Them (tail);
    Load_Them (fullnames);
    Enable_Interrupts;
    GC_Unlink;
    Alloca_End;
}

void Finit_Load () {
}

void Fork_Load () {
}
