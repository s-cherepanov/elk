/* gdbm.c: Elk/GDBM-interface.
 * Original version by Martin Stut <stut@informatik.tu-muenchen.dbp.de>.
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

/* Functions exported:
 *
 * (gdbm-file? obj)
 *
 *   Type predicate for the newly defined type gdbm-file.
 *
 * (gdbm-open filename block-size type [filemode])
 *
 *   Opens a gdbm file and returns an object of type gdbm-file.
 *   Returns #f if file cannot be opened.
 *   filename is a string or a symbol, block-size is an integer,
 *   type is one of the symbols 'reader, 'writer, 'create, and 'new,
 *   the optional file mode is an integer (default: #o644).
 *
 * (gdbm-close gf)
 *
 *   Closes a gdbm file.  Attempts to use a closed gdbm file as
 *   an argument to any gdbm-function causes the error message
 *   "invalid gdbm-file" to be displayed.
 *
 * (gdbm-store gf key data mode)
 *
 *   Stores an item in the gdbm file pointed to by gf.
 *   key and data are strings, mode is a symbol (either 'insert
 *   or 'replace).
 *   Returns -1 if called by a reader, 1 if called with 'insert and
 *   the key is already stored, 0 otherwise.
 *
 * (gdbm-fetch gf key)
 *
 *   Searches the gdbm file pointed to by gf for data stored under
 *   the given key and returns the data as a string.
 *   Returns #f if nothing is stored under that key.
 *
 * (gdbm-delete gf key)
 *
 *   Removes data stored under the specified key from the gdbm file gf.
 *   Returns #f if the key is not present in the gdbm file, #t otherwise.
 *
 * (gdbm-firstkey gf)
 * (gdbm-nextkey gf key)
 *
 *   These functions are used to access all items in a gdbm file.
 *   Both return a key.  gdbm-firstkey returns #f if the gdbm file
 *   is empty; gdbm-nextkey returns #f if there is no next key.
 *
 * (gdbm-reorganize gf)
 *
 *   Shortens the specified gdbm file (reclaims deleted space).
 *
 * (gdbm-error)
 *
 *   Returns a cons cell; the car is the last error number set by
 *   the gdbm library, the cdr is the current UNIX errno.
 *
 * (gdbm-error-text)
 *
 *   Returns the last error message passed to the fatal error
 *   function by the gdbm library (a string).
 *
 * Loading gdbm.o provides the symbol 'gdbm.o.
 */


#include "scheme.h"
#include <gdbm.h>
#include <errno.h>
#include <string.h>

extern gdbm_error gdbm_errno;
extern int errno;
static char *gdbm_error_message = "";

static SYMDESCR RW_Syms[] = {
    { "reader", GDBM_READER },
    { "writer", GDBM_WRITER },
    { "create", GDBM_WRCREAT },
    { "new",    GDBM_NEWDB },
    { 0, 0 }
};

static SYMDESCR Flag_Syms[] = {
    { "insert",  GDBM_INSERT },
    { "replace", GDBM_REPLACE },
    { 0, 0 }
};

int T_Gdbm_fh;

struct S_gdbm_fh{
    Object tag;
    GDBM_FILE fptr;
    char free;
};

#define GDBM_FH(obj) ((struct S_gdbm_fh *)POINTER(obj))

int Gdbm_fh_Equal (Object a, Object b) {
    return !GDBM_FH(a)->free && !GDBM_FH(b)->free &&
	    GDBM_FH(a)->fptr == GDBM_FH(b)->fptr;
}

/*ARGSUSED*/
int Gdbm_fh_Print (fh, port, raw, depth, len) Object fh, port;
	int /*Bool*/ raw; int depth, len; {
    Printf (port, "#[gdbm-file %lu]", GDBM_FH(fh)->fptr);
    return 0;
}

Object P_Gdbm_filep (Object x) {
    return TYPE(x) == T_Gdbm_fh ? True : False;
}

static void Fatal_Func (char *s) {
    gdbm_error_message = s;
    fprintf (stderr, "gdbm error: %s\n", s);
}

Object P_Gdbm_Open (int argc, Object *argv) {
    Object Gdbm_fh;
    GDBM_FILE dbf;

    Disable_Interrupts;
    dbf = gdbm_open (Get_Strsym (argv[0]), Get_Integer (argv[1]),
	Symbols_To_Bits (argv[2], 0, RW_Syms),
	argc == 4 ? Get_Integer (argv[3]) : 0644, Fatal_Func);
    if (dbf == 0) {
	Enable_Interrupts;
	return False;
    }
    Gdbm_fh = Alloc_Object (sizeof (struct S_gdbm_fh), T_Gdbm_fh, 0);
    GDBM_FH (Gdbm_fh)->tag = Null;
    GDBM_FH (Gdbm_fh)->fptr = dbf;
    GDBM_FH (Gdbm_fh)->free = 0;
    Enable_Interrupts;
    return Gdbm_fh;
}

void Check_Fh (Object fh) {
    Check_Type (fh, T_Gdbm_fh);
    if (GDBM_FH(fh)->free)
	Primitive_Error ("invalid gdbm-file: ~s", fh);
}

Object P_Gdbm_Close (Object fh) {
    Check_Fh (fh);
    GDBM_FH(fh)->free = 1;
    Disable_Interrupts;
    gdbm_close (GDBM_FH(fh)->fptr);
    Enable_Interrupts;
    return Void;
}

Object P_Gdbm_Store (fh, key, content, flag)
	Object fh, key, content, flag; {
    int res;
    datum k, c;

    Check_Fh (fh);
    Check_Type (key, T_String);
    Check_Type (content, T_String);
    k.dptr = STRING(key)->data;
    k.dsize = STRING(key)->size;
    c.dptr = STRING(content)->data;
    c.dsize = STRING(content)->size;
    Disable_Interrupts;
    res = gdbm_store (GDBM_FH(fh)->fptr, k, c,
	Symbols_To_Bits (flag, 0, Flag_Syms));
    Enable_Interrupts;
    return Make_Integer (res);
}

static Object Gdbm_Get (Object fh, Object key, datum (*func)()) {
    Object res;
    datum k, c;

    Check_Fh (fh);
    Check_Type (key, T_String);
    k.dptr = STRING(key)->data;
    k.dsize = STRING(key)->size;
    Disable_Interrupts;
    c = (*func) (GDBM_FH(fh)->fptr, k);
    Enable_Interrupts;
    if (c.dptr == 0)
	return False;
    res = Make_String (c.dptr, c.dsize);
    free (c.dptr);
    return res;
}

Object P_Gdbm_Fetch (Object fh, Object key) {
    return Gdbm_Get (fh, key, gdbm_fetch);
}

Object P_Gdbm_Nextkey (Object fh, Object key) {
    return Gdbm_Get (fh, key, gdbm_nextkey);
}

Object P_Gdbm_Delete (Object fh, Object key) {
    int res;
    datum k;

    Check_Fh (fh);
    Check_Type (key, T_String);
    k.dptr = STRING(key)->data;
    k.dsize = STRING(key)->size;
    Disable_Interrupts;
    res = gdbm_delete (GDBM_FH(fh)->fptr, k);
    Enable_Interrupts;
    return res == 0 ? True : False;
}

Object P_Gdbm_Firstkey (Object fh) {
    Object res;
    datum k;

    Check_Fh (fh);
    Disable_Interrupts;
    k = gdbm_firstkey (GDBM_FH(fh)->fptr);
    Enable_Interrupts;
    if (k.dptr == 0)
	return False;
    res = Make_String (k.dptr, k.dsize);
    free (k.dptr);
    return res;
}

Object P_Gdbm_Reorganize (Object fh) {
    Check_Fh (fh);
    Disable_Interrupts;
    gdbm_reorganize (GDBM_FH(fh)->fptr);
    Enable_Interrupts;
    return Void;
}

Object P_Gdbm_Error () {
    return Cons (Make_Integer ((int)gdbm_errno), Make_Integer (errno));
}

Object P_Gdbm_Error_Text () {
    return Make_String (gdbm_error_message, strlen (gdbm_error_message));
}

void elk_init_lib_gdbm () {
    Define_Primitive (P_Gdbm_Open, "gdbm-open", 3, 4, VARARGS);
    Define_Primitive (P_Gdbm_filep, "gdbm-file?", 1, 1, EVAL);
    Define_Primitive (P_Gdbm_Close, "gdbm-close", 1, 1, EVAL);
    Define_Primitive (P_Gdbm_Store, "gdbm-store", 4, 4, EVAL);
    Define_Primitive (P_Gdbm_Fetch, "gdbm-fetch", 2, 2, EVAL);
    Define_Primitive (P_Gdbm_Delete, "gdbm-delete", 2, 2, EVAL);
    Define_Primitive (P_Gdbm_Firstkey, "gdbm-firstkey", 1, 1, EVAL);
    Define_Primitive (P_Gdbm_Nextkey, "gdbm-nextkey", 2, 2, EVAL);
    Define_Primitive (P_Gdbm_Reorganize, "gdbm-reorganize", 1, 1, EVAL);
    Define_Primitive (P_Gdbm_Error, "gdbm-error", 0, 0, EVAL);
    Define_Primitive (P_Gdbm_Error_Text, "gdbm-error-text", 0, 0, EVAL);
    T_Gdbm_fh = Define_Type (0, "gdbm-file", NOFUNC,
	sizeof (struct S_gdbm_fh), Gdbm_fh_Equal, Gdbm_fh_Equal,
	Gdbm_fh_Print, NOFUNC);
    P_Provide (Intern ("gdbm.so"));
    P_Provide (Intern ("gdbm.o"));
}
