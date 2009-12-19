/* intern.h: Functions and variables that are used by more than one source
 * file of the kernel. Not available to extensions and applications.
 *
 * $Id$
 *
 * Copyright 1990, 1991, 1992, 1993, 1994, 1995, Oliver Laumann, Berlin
 * Copyright 2002, 2003 Sam Hocevar <sam@hocevar.net>, Paris
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

C_LINKAGE_BEGIN

/* autoload.c
 */
extern Object V_Autoload_Notifyp;
extern Object Do_Autoload (Object, Object);

/* bignum.c
 */
extern int Bignum_Zero (Object);
extern int Bignum_Positive (Object);
extern int Bignum_Negative (Object);
extern int Bignum_Even (Object);
extern Object Make_Bignum (const char*, int, int);
extern Object Integer_To_Bignum (int);
extern Object Bignum_Divide (Object, Object);
extern Object Bignum_Abs (Object);
extern Object Bignum_Plus (Object, Object);
extern Object Bignum_Minus (Object, Object);
extern Object Bignum_Fixnum_Multiply (Object, Object);
extern Object Bignum_Multiply (Object, Object);
extern Object Bignum_Fixnum_Divide (Object, Object);
extern Object Double_To_Bignum (double);
extern Object Unsigned_To_Bignum (unsigned);
extern Object Long_To_Bignum (long);
extern Object Unsigned_Long_To_Bignum (unsigned long);
extern unsigned Bignum_To_Unsigned (Object);
extern unsigned long Bignum_To_Unsigned_Long (Object);
extern long Bignum_To_Long (Object);
extern Object Bignum_To_String (Object, int);
extern int Bignum_Equal (Object, Object);
extern int Bignum_Greater (Object, Object);
extern int Bignum_Less (Object, Object);
extern int Bignum_Eq_Less (Object, Object);
extern int Bignum_Eq_Greater (Object, Object);

/* cont.c
 */
extern WIND *First_Wind, *Last_Wind;
extern Object Internal_Call_CC (int, Object);

/* dump.c
 */
extern Object Dump_Control_Point;

/* env.c
 */
extern Object Add_Binding (Object, Object, Object);
extern Object Lookup_Symbol (Object, int);

/* error.c
 */
extern Object Arg_True;
extern char *appname;

/* exception.c
 */
extern void Install_Intr_Handler (void);

/* heap.c
 */
#ifndef GENERATIONAL_GC
extern char *Hp, *Heap_Start, *Heap_End, *Free_Start, *Free_End;
#endif
extern int GC_In_Progress;

/* io.c
 */
extern Object General_Open_File (Object, int, Object);

/* load.c
 */
extern char *Loader_Input;
extern Object V_Load_Path, V_Load_Noisilyp, V_Load_Libraries;
extern void Check_Loadarg (Object);
extern Object General_Load (Object, Object);

/* list.c
 */
extern Object General_Assoc (Object, Object, int);

/* libelk.c
 */
extern char *stkbase, *A_Out_Name;
extern int Stack_Grows_Down;
extern unsigned int Max_Stack;
extern int Interpreter_Initialized, Was_Dumped;
extern char *Brk_On_Dump;
extern int Verb_Load, Verb_Init, Case_Insensitive;
extern SYMTAB *The_Symbols;
extern char *Scm_Dir;
extern char *Lib_Dir;
#ifndef HAVE_ATEXIT
extern void exit (int);
#endif

/* math.c
 */
extern char *Flonum_To_String (Object);

/* proc.c
 */
extern Object Sym_Lambda, Sym_Macro;
extern void Funcall_Control_Point (Object, Object, int);
extern Object Make_Primitive
    (Object(*)(), const char*, int, int, enum discipline);

/* read.c
 */
extern Object Sym_Quote;
extern Object Sym_Quasiquote, Sym_Unquote, Sym_Unquote_Splicing;
extern Object Parse_Number (Object, const char*, int);

/* stab.c
 */
extern SYMTAB *Open_File_And_Snarf_Symbols (char *);

/* stkmem.c
 */
#ifndef HAVE_ALLOCA
extern Object Save_GC_Nodes (void);
#endif

/* string.c
 */
extern char Char_Map[];
extern Object General_Make_String (const char*, unsigned int, int);

/* symbol.c
 */
extern Object Unbound, Special, Zero, One;

/* type.c
 */
extern int Num_Types, Max_Type;

/* vector.c
 */
extern Object List_To_Vector (Object, int);

C_LINKAGE_END
