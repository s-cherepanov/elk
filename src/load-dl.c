/* load-dl.c
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

#include <dlfcn.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

extern void Free_Symbols (SYMTAB *);
extern void Call_Initializers (SYMTAB *, char *, int);

void Dlopen_File (char *fn) {
    void *handle;
    SYM *sp;

    if (Verb_Load)
	printf ("[dlopen %s]\n", fn);
    if ((handle = dlopen (fn, RTLD_NOW|RTLD_GLOBAL)) == 0) {
	char *errstr = dlerror ();
	Primitive_Error ("dlopen failed:~%~s",
	    Make_String (errstr, strlen (errstr)));
    }
    if (The_Symbols)
	Free_Symbols (The_Symbols);
    The_Symbols = Open_File_And_Snarf_Symbols (fn);
    /*
     * dlsym() may fail for symbols not exported by object file;
     * this can be safely ignored.
     */
    for (sp = The_Symbols->first; sp; sp = sp->next)
	sp->value = (unsigned long int)dlsym (handle, sp->name);
    Call_Initializers (The_Symbols, 0, PR_CONSTRUCTOR);
    Call_Initializers (The_Symbols, 0, PR_EXTENSION);
}

static char *tempname;
static char *tmpdir;
static int tmplen;
static int Seq_Num;

char *Temp_Name (int seq) {
    if (!tempname) {
	if (!(tmpdir = getenv ("TMPDIR")))
	    tmpdir = "/tmp";
	tempname = Safe_Malloc (tmplen = strlen (tmpdir) + 20);
	sprintf (tempname, "%s/ldXXXXXX", tmpdir);
	(void)mktemp (tempname);
	strcat (tempname, ".");
    }
    sprintf (strrchr (tempname, '.'), ".%d", seq);
    return tempname;
}

void Fork_Load () {
    int i;
    char *newtemp;

    if (!tempname)
	return;
    Disable_Interrupts;
    newtemp = Safe_Malloc (tmplen);
    sprintf (newtemp, "%s/ldXXXXXX", tmpdir);
    (void)mktemp (newtemp);
    strcat (newtemp, ".");
    for (i = 0; i < Seq_Num; i++) {
	sprintf (strrchr (newtemp, '.'), ".%d", i);
	(void)link (Temp_Name (i), newtemp);
    }
    free (tempname);
    tempname = newtemp;
    Enable_Interrupts;
}

void Load_Object (Object names) {
    Object port, tail, fullnames, libs;
    char *lp, *buf, *outfile;
    int len, liblen, i;
    GC_Node3;
    Alloca_Begin;

    port = tail = fullnames = Null;
    GC_Link3 (port, tail, fullnames);
    for (len = 0, tail = names; !Nullp (tail); tail = Cdr (tail)) {
	port = General_Open_File (Car (tail), P_INPUT, Var_Get (V_Load_Path));
	fullnames = Cons (PORT(port)->name, fullnames);
	len += STRING(Car (fullnames))->size + 1;
	(void)P_Close_Input_Port (port);
    }
    GC_Unlink;

    libs = Var_Get (V_Load_Libraries);
    if (TYPE(libs) == T_String) {
	liblen = STRING(libs)->size;
	lp = STRING(libs)->data;
    } else {
	liblen = 0;
	lp = "";
    }

    Disable_Interrupts;

    buf = Temp_Name (Seq_Num);
    Seq_Num++;
    Alloca (outfile, char*, tmplen);
    strcpy (outfile, buf);
    Alloca (buf, char*, len + liblen + Seq_Num*tmplen + 100);
    sprintf (buf, "%s %s -o %s ", LD_NAME, LDFLAGS_SHARED, outfile);

    for (tail = fullnames; !Nullp (tail); tail = Cdr (tail)) {
	register struct S_String *str = STRING(Car (tail));
	strncat (buf, str->data, str->size);
	strcat (buf, " ");
    }
    for (i = 0; i < Seq_Num-1; i++) {
	strcat (buf, Temp_Name (i));
	strcat (buf, " ");
    }
    strncat (buf, lp, liblen);

    if (Verb_Load)
	printf ("[%s]\n", buf);
    if (system (buf) != 0) {
	Seq_Num--;
	(void)unlink (outfile);
	Primitive_Error ("system linker failed");
    }
    Dlopen_File (outfile);
    Enable_Interrupts;
    Alloca_End;
}

void Load_Lib (Object libs) {
    Object port, name;

    if (Nullp (libs))
	return;

    Load_Lib (Cdr (libs));

    GC_Node2;
    port = name = Null;
    GC_Link2 (port, name);
    port = General_Open_File (Car (libs), P_INPUT, Var_Get (V_Load_Path));
    name = PORT(port)->name;
    Dlopen_File (STRING(name)->data);
    (void)P_Close_Input_Port (port);
    GC_Unlink;
}

void Load_Library (Object libs) {
    Disable_Interrupts;
    Load_Lib (libs);
    Enable_Interrupts;
}

void Finit_Load () {
    int i;

    for (i = 0; i < Seq_Num; i++)
	(void)unlink (Temp_Name (i));
    /*
     * The linker in SGI Irix 5 produces this file:
     */
    (void)unlink ("so_locations");
}
