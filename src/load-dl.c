#include <dlfcn.h>
#include <stdlib.h>

extern char *strrchr();
extern char *getenv();

Dlopen_File (fn) char *fn; {
    void *handle;
    SYM *sp;

    if (Verb_Load)
	printf ("[dlopen %s]\n", fn);
    if ((handle = dlopen (fn, RTLD_NOW)) == 0) {
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
	sp->value = (unsigned long)dlsym (handle, sp->name);
    Call_Initializers (The_Symbols, 0, PR_CONSTRUCTOR);
    Call_Initializers (The_Symbols, 0, PR_EXTENSION);
}

static char *tempname;
static char *tmpdir;
static tmplen;
static Seq_Num;

char *Temp_Name (seq) int seq; {
    if (!tempname) {
	if (!(tmpdir = getenv ("TMPDIR")))
	    tmpdir = "/tmp";
	tempname = Safe_Malloc (tmplen = strlen (tmpdir) + 20);
	sprintf (tempname, "%s/ldXXXXXX", tmpdir);
	(void)mkstemp (tempname);
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
    (void)mkstemp (newtemp);
    strcat (newtemp, ".");
    for (i = 0; i < Seq_Num; i++) {
	sprintf (strrchr (newtemp, '.'), ".%d", i);
	(void)link (Temp_Name (i), newtemp);
    }
    free (tempname);
    tempname = newtemp;
    Enable_Interrupts;
}

Load_Object (names) Object names; {
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
    } else
	liblen = 0;

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

void Finit_Load () {
    int i;

    for (i = 0; i < Seq_Num; i++)
	(void)unlink (Temp_Name (i));
    /*
     * The linker in SGI Irix 5 produces this file:
     */
    (void)unlink ("so_locations");
}
