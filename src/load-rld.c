/* load-rld.c
 *
 * $Id$
 *
 * Copyright 1990, 1991, 1992, 1993, 1994, 1995, Oliver Laumann, Berlin
 * Copyright 2002, 2003 Sam Hocevar <sam@zoy.org>, Paris
 *
 * This software was derived from Elk 1.2, which was Copyright 1987, 1988,
 * 1989, Nixdorf Computer AG and TELES GmbH, Berlin (Elk 1.2 has been written
 * by Oliver Laumann for TELES Telematic Services, Berlin, in a joint project
 * between TELES and Nixdorf Microprocessor Engineering, Berlin).
 *
 * Oliver Laumann, TELES GmbH, Nixdorf Computer AG and Sam Hocevar, as co-
 * owners or individual owners of copyright in this software, grant to any
 * person or company a worldwide, royalty free, license to
 *
 *    i) copy this software,
 *   ii) prepare derivative works based on this software,
 *  iii) distribute copies of this software or derivative works,
 *   iv) perform this software, or
 *    v) display this software,
 *
 * provided that this notice is not removed and that neither Oliver Laumann
 * nor Teles nor Nixdorf are deemed to have made any representations as to
 * the suitability of this software for any purpose nor are held responsible
 * for any defects of this software.
 *
 * THERE IS ABSOLUTELY NO WARRANTY FOR THIS SOFTWARE.
 */

#include <mach-o/rld.h>

extern void Free_Symbols (SYMTAB *);
extern void Call_Initializers (SYMTAB *, char *, int);

void Load_Object (Object names) {
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
