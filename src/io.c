/* io.c: Ports and I/O primitives.
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

#include "config.h"

#include <errno.h>
#include <stdio.h>
#ifdef HAVE_PWD_H
#   include <pwd.h>
#endif
#include <string.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/stat.h>

#ifdef PC_PATH_MAX_IN_UNISTD_H
#  include <unistd.h>
#endif

#include "kernel.h"

extern void Flush_Output (Object);

extern int errno;
extern char *getenv();

Object Curr_Input_Port, Curr_Output_Port;
Object Standard_Input_Port, Standard_Output_Port;

void Init_Io () {
    Standard_Input_Port = Make_Port (P_INPUT, stdin, Make_String ("stdin", 5));
    Standard_Output_Port = Make_Port (0, stdout, Make_String ("stdout", 6));
    Curr_Input_Port = Standard_Input_Port;
    Curr_Output_Port = Standard_Output_Port;
    Global_GC_Link (Standard_Input_Port);
    Global_GC_Link (Standard_Output_Port);
    Global_GC_Link (Curr_Input_Port);
    Global_GC_Link (Curr_Output_Port);
}

void Reset_IO (int destructive) {
    Discard_Input (Curr_Input_Port);
    if (destructive)
        Discard_Output (Curr_Output_Port);
    else
        Flush_Output (Curr_Output_Port);
    Curr_Input_Port = Standard_Input_Port;
    Curr_Output_Port = Standard_Output_Port;
}

Object Make_Port (int flags, FILE *f, Object name) {
    Object port;
    GC_Node;

    GC_Link (name);
    port = Alloc_Object (sizeof (struct S_Port), T_Port, 0);
    PORT(port)->flags = flags|P_OPEN;
    PORT(port)->file = f;
    PORT(port)->name = name;
    PORT(port)->ptr = 0;
    PORT(port)->lno = 1;
    PORT(port)->closefun = fclose;
    GC_Unlink;
    return port;
}

Object P_Port_File_Name (Object p) {
    Check_Type (p, T_Port);
    return (PORT(p)->flags & P_STRING) ? False : PORT(p)->name;
}

Object P_Port_Line_Number (Object p) {
    Check_Type (p, T_Port);
    return Make_Unsigned (PORT(p)->lno);
}

Object P_Eof_Objectp (Object x) {
    return TYPE(x) == T_End_Of_File ? True : False;
}

Object P_Current_Input_Port () { return Curr_Input_Port; }

Object P_Current_Output_Port () { return Curr_Output_Port; }

Object P_Input_Portp (Object x) {
    return TYPE(x) == T_Port && IS_INPUT(x) ? True : False;
}

Object P_Output_Portp (Object x) {
    return TYPE(x) == T_Port && IS_OUTPUT(x) ? True : False;
}

unsigned int Path_Max () {
#if defined(PATH_MAX) /* POSIX */
    return PATH_MAX;
#elif defined(MAXPATHLEN) /* 4.3 BSD */
    return MAXPATHLEN;
#elif defined(PC_PATH_MAX_IN_UNISTD_H)
    static int r;
    if (r == 0) {
        if ((r = pathconf ("/", _PC_PATH_MAX)) == -1)
            r = 1024;
        r++;
    }
    return r;
#else
    return 1024;
#endif
}

Object Get_File_Name (Object name) {
    register unsigned int len;

    if (TYPE(name) == T_Symbol)
        name = SYMBOL(name)->name;
    else if (TYPE(name) != T_String)
        Wrong_Type_Combination (name, "string or symbol");
    len = STRING(name)->size;
    if (len > Path_Max () || len == 0)
        Primitive_Error ("invalid file name");
    return name;
}

char *Internal_Tilde_Expand (register char *s, register char **dirp) {
    register char *p;
#ifdef HAVE_PWD_H
    struct passwd *pw, *getpwnam();
#endif

    if (*s++ != '~')
        return 0;
    for (p = s; *p && *p != '/'; p++)
        ;
    if (*p == '/') *p++ = 0;
#ifdef HAVE_PWD_H
    if (*s == '\0') {
        if ((*dirp = getenv ("HOME")) == 0)
            *dirp = "";
    } else {
        if ((pw = getpwnam (s)) == 0)
            Primitive_Error ("unknown user: ~a", Make_String (s, strlen (s)));
        *dirp = pw->pw_dir;
    }
#else
    *dirp = "";
#endif
    return p;
}

Object General_File_Operation (Object s, register int op) {
    register char *r;
    Object ret, fn;
    Alloca_Begin;

    fn = Get_File_Name (s);
    Get_Strsym_Stack (fn, r);
    switch (op) {
    case 0: {
        char *p, *dir;
        p = Internal_Tilde_Expand (r, &dir);
        if (p == 0) {
            Alloca_End;
            return s;
        }
        Alloca (r, char*, strlen (dir) + 1 + strlen (p) + 1);
        sprintf (r, "%s" SEPARATOR_STRING "%s", dir, p);
        ret = Make_String (r, strlen (r));
        Alloca_End;
        return ret;
    }
    case 1: {
        struct stat st;
        /* Doesn't make much sense to check for errno != ENOENT here:
         */
        ret = stat (r, &st) == 0 ? True : False;
        Alloca_End;
        return ret;
    }
    default: {
        return Null; /* Just to avoid compiler warnings */
    }}
    /*NOTREACHED*/
}

Object P_Tilde_Expand (Object s) {
    return General_File_Operation (s, 0);
}

Object P_File_Existsp (Object s) {
    return General_File_Operation (s, 1);
}

void Close_All_Files () {
    Terminate_Type (T_Port);
}

Object Terminate_File (Object port) {
    (void)(PORT(port)->closefun) (PORT(port)->file);
    PORT(port)->flags &= ~P_OPEN;
    return Void;
}

Object Open_File (char *name, int flags, int err) {
    register FILE *f;
    char *dir, *p;
    Object fn, port;
    struct stat st;
    Alloca_Begin;

    p = Internal_Tilde_Expand (name, &dir);
    if (p) {
        Alloca (name, char*, strlen (dir) + 1 + strlen (p) + 1);
        sprintf (name, "%s" SEPARATOR_STRING "%s", dir, p);
    }
    if (!err && stat (name, &st) == -1 &&
            (errno == ENOENT || errno == ENOTDIR)) {
        Alloca_End;
        return Null;
    }
    switch (flags & (P_INPUT|P_BIDIR)) {
    case 0:               p = "w";  break;
    case P_INPUT:         p = "r";  break;
    default:              p = "r+"; break;
    }
    fn = Make_String (name, strlen (name));
    Disable_Interrupts;
    if ((f = fopen (name, p)) == NULL) {
        Saved_Errno = errno;  /* errno valid here? */
        Primitive_Error ("~s: ~E", fn);
    }
    port = Make_Port (flags, f, fn);
    Register_Object (port, (GENERIC)0, Terminate_File, 0);
    Enable_Interrupts;
    Alloca_End;
    return port;
}

Object General_Open_File (Object name, int flags, Object path) {
    Object port, pref;
    char *buf = 0;
    register char *fn;
    register unsigned int plen, len, blen = 0, gotpath = 0;
    Alloca_Begin;

    name = Get_File_Name (name);
    len = STRING(name)->size;
    fn = STRING(name)->data;
#ifdef WIN32
    if (fn[0] < 'A' || fn[0] > 'Z' || fn[1] != ':' ) {
#else
    if (fn[0] != '/' && fn[0] != '~') {
#endif
        for ( ; TYPE(path) == T_Pair; path = Cdr (path)) {
            pref = Car (path);
            if (TYPE(pref) == T_Symbol)
                pref = SYMBOL(pref)->name;
            if (TYPE(pref) != T_String)
                continue;
            gotpath = 1;
            plen = STRING(pref)->size;
            if (plen > Path_Max () || plen == 0)
                continue;
            if (len + plen + 2 > blen) {
                blen = len + plen + 2;
                Alloca (buf, char*, blen);
            }
            memcpy (buf, STRING(pref)->data, plen);
            if (buf[plen-1] != SEPARATOR_CHAR)
                buf[plen++] = SEPARATOR_CHAR;
            memcpy (buf+plen, fn, len);
            buf[len+plen] = '\0';
            port = Open_File (buf, flags, 0);
            /* No GC has been taken place in Open_File() if it returns Null.
             */
            if (!Nullp (port)) {
                Alloca_End;
                return port;
            }
        }
    }
    if (gotpath)
        Primitive_Error ("file ~s not found", name);
    if (len + 1 > blen)
        Alloca (buf, char*, len + 1);
    memcpy (buf, fn, len);
    buf[len] = '\0';
    port = Open_File (buf, flags, 1);
    Alloca_End;
    return port;
}

Object P_Open_Input_File (Object name) {
    return General_Open_File (name, P_INPUT, Null);
}

Object P_Open_Output_File (Object name) {
    return General_Open_File (name, 0, Null);
}

Object P_Open_Input_Output_File (Object name) {
    return General_Open_File (name, P_BIDIR, Null);
}

Object General_Close_Port (Object port) {
    register int flags, err = 0;
    FILE *f;

    Check_Type (port, T_Port);
    flags = PORT(port)->flags;
    if (!(flags & P_OPEN) || (flags & P_STRING))
        return Void;
    f = PORT(port)->file;
    if (f == stdin || f == stdout)
        return Void;
    if ((PORT(port)->closefun) (f) == EOF) {
        Saved_Errno = errno;   /* errno valid here? */
        err++;
    }
    PORT(port)->flags &= ~P_OPEN;
    Deregister_Object (port);
    if (err)
        Primitive_Error ("write error on ~s: ~E", port);
    return Void;
}

Object P_Close_Input_Port (Object port) {
    return General_Close_Port (port);
}

Object P_Close_Output_Port (Object port) {
    return General_Close_Port (port);
}

#define General_With(prim,curr,flags) Object prim (Object name, Object thunk) {\
    Object old, ret;\
    GC_Node2;\
\
    Check_Procedure (thunk);\
    old = curr;\
    GC_Link2 (thunk, old);\
    curr = General_Open_File (name, flags, Null);\
    ret = Funcall (thunk, Null, 0);\
    (void)General_Close_Port (curr);\
    GC_Unlink;\
    curr = old;\
    return ret;\
}

General_With (P_With_Input_From_File, Curr_Input_Port, P_INPUT)
General_With (P_With_Output_To_File, Curr_Output_Port, 0)

Object General_Call_With (Object name, int flags, Object proc) {
    Object port, ret;
    GC_Node2;

    Check_Procedure (proc);
    GC_Link2 (proc, port);
    port = General_Open_File (name, flags, Null);
    port = Cons (port, Null);
    ret = Funcall (proc, port, 0);
    (void)General_Close_Port (Car (port));
    GC_Unlink;
    return ret;
}

Object P_Call_With_Input_File (Object name, Object proc) {
    return General_Call_With (name, P_INPUT, proc);
}

Object P_Call_With_Output_File (Object name, Object proc) {
    return General_Call_With (name, 0, proc);
}

Object P_Open_Input_String (Object string) {
    Check_Type (string, T_String);
    return Make_Port (P_STRING|P_INPUT, (FILE *)0, string);
}

Object P_Open_Output_String () {
    return Make_Port (P_STRING, (FILE *)0, Make_String ((char *)0, 0));
}
