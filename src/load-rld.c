#include <mach-o/rld.h>

extern void Free_Symbols (SYMTAB *);
extern void Call_Initializers (SYMTAB *, char *, int);

Load_Object (Object names) {
    long retval;
    struct mach_header *hdr;
    char **filenames, *libs;
    NXStream *err_stream;
    register int i, n;
    Object port, tail, fullnames;
    extern char *strtok();
    GC_Node3;
    Alloca_Begin;

    port = tail = fullnames = Null;
    GC_Link3 (port, tail, fullnames);
    for (n = 0, tail = names; !Nullp (tail); n++, tail = Cdr (tail)) {
	port = General_Open_File (Car (tail), P_INPUT, Var_Get (V_Load_Path));
	fullnames = Cons (PORT(port)->name, fullnames);
	(void)P_Close_Input_Port (port);
    }
    GC_Unlink;

    libs = "";
    tail = Var_Get (V_Load_Libraries);
    if (TYPE(tail) == T_String)
	Get_Strsym_Stack (tail, libs);

    Alloca (filenames, char**, (n+1 + strlen (libs)/2) * sizeof (char *));
    for (i = 0; i < n; i++, fullnames = Cdr (fullnames)) {
	Object s;

	s = Car (fullnames);
	Get_Strsym_Stack (s, filenames[i]);
    }

    /* Append the load-libraries to the end of the list of filenames
     * to be passed to rld_load:
     */
    for ( ; (filenames[i] = strtok (libs, " \t")) != 0; i++, libs = 0)
	;
    if (Verb_Load) {
	printf ("[rld_load: ");
	for (i = 0; filenames[i]; i++) printf ("%s ", filenames[i]);
	printf ("]\n");
    }

    Disable_Interrupts;
    /* Construct a stream for error logging:
     */
    if ((err_stream = NXOpenFile (fileno (stderr), NX_WRITEONLY)) == 0)
	Primitive_Error ("NXOpenFile failed");

    retval = rld_load (err_stream, /* report error messages here */
	&hdr,                      /* return header address here */
	filenames,                 /* load these */
	"/dev/null");              /* doesn't work if NULL?! */
    NXClose (err_stream);
    if (retval != 1)
	Primitive_Error ("rld_load() failed");

    /* Grab the symbol table from the just-loaded file:
     */
    if (The_Symbols)
	Free_Symbols (The_Symbols);
    The_Symbols = Snarf_Symbols (hdr);
    Call_Initializers (The_Symbols, 0, PR_CONSTRUCTOR);
    Call_Initializers (The_Symbols, 0, PR_EXTENSION);
    Enable_Interrupts;
    Alloca_End;
}

void Finit_Load () {
}

void Fork_Load () {
}
