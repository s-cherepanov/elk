/* Functions and variables that are used by more than one source file of
 * the kernel.  Not available to extensions and applications.
 */

C_LINKAGE_BEGIN

/* autoload.c
 */
extern Object V_Autoload_Notifyp;
extern Object Do_Autoload P_((Object, Object));

/* bignum.c
 */
extern int Bignum_Zero P_((Object));
extern int Bignum_Positive P_((Object));
extern int Bignum_Negative P_((Object));
extern int Bignum_Even P_((Object));
extern Object Make_Bignum P_((const char*, int, int));
extern Object Integer_To_Bignum P_((int));
extern Object Bignum_Divide P_((Object, Object));
extern Object Bignum_Abs P_((Object));
extern Object Bignum_Plus P_((Object, Object));
extern Object Bignum_Minus P_((Object, Object));
extern Object Bignum_Fixnum_Multiply P_((Object, Object));
extern Object Bignum_Multiply P_((Object, Object));
extern Object Bignum_Fixnum_Divide P_((Object, Object));
extern Object Double_To_Bignum P_((double));
extern Object Unsigned_To_Bignum P_((unsigned));
extern Object Long_To_Bignum P_((long));
extern Object Unsigned_Long_To_Bignum P_((unsigned long));
extern unsigned Bignum_To_Unsigned P_((Object));
extern unsigned long Bignum_To_Unsigned_Long P_((Object));
extern long Bignum_To_Long P_((Object));
extern Object Bignum_To_String P_((Object, int));
extern double Bignum_To_Double P_((Object));
extern int Bignum_Equal P_((Object, Object));
extern int Bignum_Greater P_((Object, Object));
extern int Bignum_Less P_((Object, Object));
extern int Bignum_Eq_Less P_((Object, Object));
extern int Bignum_Eq_Greater P_((Object, Object));

/* cont.c
 */
extern WIND *First_Wind, *Last_Wind;
extern Object Internal_Call_CC P_((int, Object));

/* dump.c
 */
extern Object Dump_Control_Point;

/* env.c
 */
extern Object Add_Binding P_((Object, Object, Object));
extern Object Lookup_Symbol P_((Object, int));

/* error.c
 */
extern Object Arg_True;
extern char *appname;

/* exception.c
 */
extern void Install_Intr_Handler P_((void));

/* heap.c
 */
#ifndef GENERATIONAL_GC
extern char *Hp, *Heap_Start, *Heap_End, *Free_Start, *Free_End;
#endif
extern int GC_In_Progress;

/* io.c
 */
extern Object General_Open_File P_((Object, int, Object));

/* load.c
 */
extern char *Loader_Input;
extern Object V_Load_Path, V_Load_Noisilyp, V_Load_Libraries;
extern void Check_Loadarg P_((Object));
extern Object General_Load P_((Object, Object));

/* list.c
 */
extern Object General_Assoc P_((Object, Object, int));

/* main.c
 */
extern char *stkbase, *A_Out_Name;
extern int Stack_Grows_Down;
extern unsigned int Max_Stack;
extern int Interpreter_Initialized, Was_Dumped;
extern char *Brk_On_Dump;
extern int Verb_Load, Verb_Init, Case_Insensitive;
extern SYMTAB *The_Symbols;
extern void Exit_Handler P_((void));
#ifndef HAVE_ATEXIT
extern void exit P_((int));
#endif

/* math.c
 */
extern char *Flonum_To_String P_((Object));

/* proc.c
 */
extern Object Sym_Lambda, Sym_Macro;
extern void Funcall_Control_Point P_((Object, Object, int));
extern Object Make_Primitive
    P_((Object(*)(ELLIPSIS), const char*, int, int, enum discipline));

/* read.c
 */
extern Object Sym_Quote;
extern Object Sym_Quasiquote, Sym_Unquote, Sym_Unquote_Splicing;
extern Object Parse_Number P_((Object, const char*, int));

/* stab.c
 */
extern SYMTAB *Snarf_Symbols P_((ELLIPSIS));  /* varying args */
extern SYMTAB *Open_File_And_Snarf_Symbols P_((char *));

/* stkmem.c
 */
#ifndef USE_ALLOCA
extern Object Save_GC_Nodes P_((void));
#endif

/* string.c
 */
extern char Char_Map[];
extern Object General_Make_String P_((const char*, int, int));

/* symbol.c
 */
extern Object Unbound, Special, Zero, One;

/* type.c
 */
extern int Num_Types, Max_Type;

/* vector.c
 */
extern Object List_To_Vector P_((Object, int));

C_LINKAGE_END
