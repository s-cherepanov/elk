/* extern.h: This include file declares all symbols exported by the interpreter
 * kernel that may be used by applications or extensions.
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

C_LINKAGE_BEGIN

/* Autoloading
 */
extern Object P_Autoload (Object, Object);

/* Bignums
 */
extern Object Make_Uninitialized_Bignum (int);
extern void Bignum_Normalize_In_Place (struct S_Bignum *);
extern double Bignum_To_Double (Object);

/* Boolean operators
 */
extern Object P_Booleanp (Object);
extern Object P_Not (Object);
extern Object P_Eq (Object, Object);
extern Object P_Eqv (Object, Object);
extern Object P_Equal (Object, Object);
extern Object P_Empty_List_Is_False (Object);
extern int Eqv (Object, Object);
extern int Equal (Object, Object);

/* Characters
 */
extern Object Make_Char (int);
extern Object P_Charp (Object);
extern Object P_Char_Upcase (Object);
extern Object P_Char_Downcase (Object);
extern Object P_Char_Eq (Object, Object);
extern Object P_Char_Less (Object, Object);
extern Object P_Char_Greater (Object, Object);
extern Object P_Char_Eq_Less (Object, Object);
extern Object P_Char_Eq_Greater (Object, Object);
extern Object P_Char_CI_Eq (Object, Object);
extern Object P_Char_CI_Less (Object, Object);
extern Object P_Char_CI_Greater (Object, Object);
extern Object P_Char_CI_Eq_Less (Object, Object);
extern Object P_Char_CI_Eq_Greater (Object, Object);
extern Object P_Char_Upper_Casep (Object);
extern Object P_Char_Lower_Casep (Object);
extern Object P_Char_Alphabeticp (Object);
extern Object P_Char_Numericp (Object);
extern Object P_Char_Whitespacep (Object);
extern Object P_Char_To_Integer (Object);
extern Object P_Integer_To_Char (Object);

/* Continuations
 */
extern Object P_Call_With_Current_Continuation (Object);
extern Object P_Dynamic_Wind (Object, Object, Object);
extern Object P_Control_Pointp (Object);
extern Object P_Control_Point_Environment (Object);

/* Scheme strings --> C Strings
 */
extern char *Get_String (Object);
extern char *Get_Strsym (Object);

/* Debugging
 */
extern Object P_Backtrace_List (int, Object*);

/* Dump
 */
extern Object P_Dump (Object);

/* Lexical bindings, environments
 */
extern Object P_The_Environment (void);
extern Object P_Define (Object);
extern Object P_Set (Object);
extern Object P_Environment_To_List (Object);
extern Object P_Define_Macro (Object);
extern Object P_Boundp (Object);
extern Object P_Global_Environment (void);
extern Object P_Environmentp (Object);
extern Object The_Environment, Global_Environment;

/* Error handling
 */
extern void Primitive_Error (const char*, ...) elk_attribute(__noreturn__);
extern void Fatal_Error (const char*, ...) elk_attribute(__noreturn__);
extern void Range_Error (Object);
extern void Panic (const char*);
extern Object P_Error (int, Object*);
extern Object P_Reset (void);
extern const char *Error_Tag;    /* will be removed in the near future */
extern const char *Get_Error_Tag (void);
extern void Set_Error_Tag (const char *);
extern void Set_App_Name (char *);
extern void Reader_Error (Object, char *) elk_attribute(__noreturn__);

/* Interrupts
 */
extern Object P_Disable_Interrupts (void);
extern Object P_Enable_Interrupts (void);
extern void Signal_Exit (int);

/* Features
 */
extern Object P_Features (void);
extern Object P_Featurep (Object);
extern Object P_Provide (Object);
extern Object P_Require (int, Object*);

/* Memory allocation, garbage collection
 */
extern int GC_Debug;
extern Object Alloc_Object (int, int, int);
extern void Register_Before_GC (void (*)(void));
extern void Register_After_GC (void (*)(void));
extern Object P_Collect (void);
extern Object P_Garbage_Collect_Status (int, Object *);
#ifdef GENERATIONAL_GC
    extern Object P_Collect_Incremental (void);
#endif

/* Files and ports
 */
extern Object Curr_Input_Port, Curr_Output_Port;
extern Object Standard_Input_Port, Standard_Output_Port;
extern void Reset_IO (int);
extern Object P_Current_Input_Port (void);
extern Object P_Current_Output_Port (void);
extern Object P_Input_Portp (Object);
extern Object P_Output_Portp (Object);
extern Object P_Open_Input_File (Object);
extern Object P_Open_Output_File (Object);
extern Object P_Open_Input_Output_File (Object);
extern Object P_Eof_Objectp (Object);
extern Object P_With_Input_From_File (Object, Object);
extern Object P_With_Output_To_File (Object, Object);
extern Object P_Call_With_Input_File (Object, Object);
extern Object P_Call_With_Output_File (Object, Object);
extern Object P_Open_Input_String (Object);
extern Object P_Open_Output_String (void);
extern Object P_Port_File_Name (Object);
extern Object P_Tilde_Expand (Object);
extern Object P_File_Existsp (Object);
extern Object P_Close_Input_Port (Object);
extern Object P_Close_Output_Port (Object);
extern Object P_Port_Line_Number (Object);
extern Object Terminate_File (Object);
extern Object Make_Port (int, FILE*, Object);
extern unsigned int Path_Max (void);

/* Loading of files
 */
extern Object P_Load (int, Object*);
extern void Load_Source_Port (Object);
extern void Load_File (char *);

/* Pairs and lists
 */
extern Object P_Cons (Object, Object);
extern Object P_Car (Object);
extern Object P_Cdr (Object);
extern Object P_Set_Car (Object, Object);
extern Object P_Set_Cdr (Object, Object);
extern Object P_Listp (Object);
extern Object P_List (int, Object*);
extern Object P_Length (Object);
extern Object P_Nullp (Object);
extern Object P_Pairp (Object);
extern Object P_Cxr (Object, Object);
extern Object P_Cddr (Object);
extern Object P_Cdar (Object);
extern Object P_Cadr (Object);
extern Object P_Caar (Object);
extern Object P_Cdddr (Object);
extern Object P_Cddar (Object);
extern Object P_Cdadr (Object);
extern Object P_Cdaar (Object);
extern Object P_Caddr (Object);
extern Object P_Cadar (Object);
extern Object P_Caadr (Object);
extern Object P_Caaar (Object);
extern Object P_Caaaar (Object);
extern Object P_Caaadr (Object);
extern Object P_Caadar (Object);
extern Object P_Caaddr (Object);
extern Object P_Cadaar (Object);
extern Object P_Cadadr (Object);
extern Object P_Caddar (Object);
extern Object P_Cadddr (Object);
extern Object P_Cdaaar (Object);
extern Object P_Cdaadr (Object);
extern Object P_Cdadar (Object);
extern Object P_Cdaddr (Object);
extern Object P_Cddaar (Object);
extern Object P_Cddadr (Object);
extern Object P_Cdddar (Object);
extern Object P_Cddddr (Object);
extern Object P_Append (int, Object*);
extern Object P_Append_Set (int, Object*);
extern Object P_Last_Pair (Object);
extern Object P_Reverse (Object);
extern Object P_Reverse_Set (Object);
extern Object P_List_Tail (Object, Object);
extern Object P_List_Ref (Object, Object);
extern Object P_Assq (Object, Object);
extern Object P_Assv (Object, Object);
extern Object P_Assoc (Object, Object);
extern Object P_Memq (Object, Object);
extern Object P_Memv (Object, Object);
extern Object P_Member (Object, Object);
extern Object P_Make_List (Object, Object);
extern Object Copy_List (Object);
extern unsigned int Fast_Length (Object);
extern Object Const_Cons (Object, Object);

/* Startup and termination
 */
extern Object P_Command_Line_Args (void);
extern Object P_Exit (int, Object*);
extern void Elk_Init (int, char **av, int, char *);
extern void Exit_Handler (void);

/* malloc() and realloc()
 */
extern char *Safe_Malloc (unsigned int);
extern char *Safe_Realloc (char*, unsigned int);

/* Numbers
 */
extern Object Make_Integer (int);
extern Object Make_Unsigned (unsigned int);
extern Object Make_Long (long int);
extern Object Make_Unsigned_Long (unsigned long int);
extern Object Make_Reduced_Flonum (double);
extern Object Make_Flonum (double);
extern Object P_Numberp (Object);
extern Object P_Complexp (Object);
extern Object P_Realp (Object);
extern Object P_Rationalp (Object);
extern Object P_Integerp (Object);
extern Object P_Abs (Object);
extern Object P_Zerop (Object);
extern Object P_Positivep (Object);
extern Object P_Negativep (Object);
extern Object P_Oddp (Object);
extern Object P_Evenp (Object);
extern Object P_Exactp (Object);
extern Object P_Inexactp (Object);
extern Object P_Exact_To_Inexact (Object);
extern Object P_Inexact_To_Exact (Object);
extern Object P_Inc (Object);
extern Object P_Dec (Object);
extern Object P_Generic_Equal (int, Object*);
extern Object P_Generic_Less (int, Object*);
extern Object P_Generic_Greater (int, Object*);
extern Object P_Generic_Eq_Less (int, Object*);
extern Object P_Generic_Eq_Greater (int, Object*);
extern Object P_Generic_Plus (int, Object*);
extern Object P_Generic_Minus (int, Object*);
extern Object P_Generic_Multiply (int, Object*);
extern Object P_Generic_Divide (int, Object*);
extern Object P_Quotient (Object, Object);
extern Object P_Remainder (Object, Object);
extern Object P_Modulo (Object, Object);
extern Object P_Gcd (int, Object*);
extern Object P_Lcm (int, Object*);
extern Object P_Floor (Object);
extern Object P_Ceiling (Object);
extern Object P_Truncate (Object);
extern Object P_Round (Object);
extern Object P_Sqrt (Object);
extern Object P_Exp (Object);
extern Object P_Pow (Object, Object);
extern Object P_Log (Object);
extern Object P_Sin (Object);
extern Object P_Cos (Object);
extern Object P_Tan (Object);
extern Object P_Asin (Object);
extern Object P_Acos (Object);
extern Object P_Atan (int, Object*);
extern Object P_Min (int, Object*);
extern Object P_Max (int, Object*);
extern Object P_Random (void);
extern Object P_Srandom (Object);
extern Object P_Number_To_String (int, Object*);
extern double Get_Double (Object);
extern int Get_Integer (Object);
extern unsigned int Get_Unsigned (Object);
extern long int Get_Long (Object);
extern unsigned long int Get_Unsigned_Long (Object);
extern int Get_Exact_Integer (Object);
extern unsigned int Get_Exact_Unsigned (Object);
extern long int Get_Exact_Long (Object);
extern unsigned long int Get_Exact_Unsigned_Long (Object);

/* Onfork handlers
 */
extern void Register_Onfork (void (*)(void));
extern void Call_Onfork (void);

/* Define_Primitive()
 */
extern void Define_Primitive (Object (*)(), const char*, int, int,
    enum discipline);

/* Output
 */
extern Object P_Write (int, Object*);
extern Object P_Display (int, Object*);
extern Object P_Write_Char (int, Object*);
extern Object P_Newline (int, Object*);
extern Object P_Format (int, Object*);
extern Object P_Clear_Output_Port (int, Object*);
extern Object P_Flush_Output_Port (int, Object*);
extern Object P_Print (int, Object*);
extern Object P_Get_Output_String (Object);
extern void Check_Output_Port (Object);
extern void Discard_Output (Object);
extern void Printf (Object, const char *, ...);
extern void Print_Object (Object, Object, int, int, unsigned int);
extern void General_Print_Object (Object, Object, int);
extern void Format (Object, const char*, unsigned int, int, Object*);
extern int Saved_Errno;

/* Evaluator, procedures, macros
 */
extern Object Eval (Object);
extern Object P_Eval (int, Object*);
extern Object P_Apply (int, Object*);
extern Object Funcall (Object, Object, int);
extern Object P_Lambda (Object);
extern Object P_Map (int, Object*);
extern Object P_Procedure_Environment (Object);
extern Object P_Procedure_Lambda (Object);
extern Object P_For_Each (int, Object*);
extern Object P_Procedurep (Object);
extern Object P_Macro (Object);
extern Object P_Macro_Body (Object);
extern Object P_Macro_Expand (Object);
extern Object P_Primitivep (Object);
extern Object P_Compoundp (Object);
extern Object P_Macrop (Object);
extern void Check_Procedure (Object);

/* Delay and force
 */
extern Object P_Delay (Object);
extern Object P_Force (Object);
extern Object P_Promisep (Object);
extern Object P_Promise_Environment (Object);

/* Input
 */
extern Object P_Read (int, Object*);
extern Object P_Read_Char (int, Object*);
extern Object P_Peek_Char (int, Object*);
extern Object P_Char_Readyp (int, Object*);
extern Object P_Unread_Char (int, Object*);
extern Object P_Read_String (int, Object*);
extern Object P_Clear_Input_Port (int, Object*);
extern Object General_Read (Object, int);
extern void Check_Input_Port (Object);
extern void Discard_Input (Object);
extern void Define_Reader (int, READFUN);

/* Special forms
 */
extern Object P_Quote (Object);
extern Object P_If (Object);
extern Object P_Let (Object);
extern Object P_Letseq (Object);
extern Object P_Letrec (Object);
extern Object P_Case (Object);
extern Object P_Cond (Object);
extern Object P_And (Object);
extern Object P_Or (Object);
extern Object P_Do (Object);
extern Object P_Quasiquote (Object);
extern Object P_Fluid_Let (Object);
extern Object P_Begin (Object);
extern Object P_Begin1 (Object);

/* Strings
 */
extern Object Make_String (const char*, unsigned int);
extern Object Make_Const_String (const char*, unsigned int);
extern Object P_Make_String (int, Object*);
extern Object P_Stringp (Object);
extern Object P_String (int, Object*);
extern Object P_String_To_Number (int, Object*);
extern Object P_String_Eq (Object, Object);
extern Object P_String_Less (Object, Object);
extern Object P_String_Greater (Object, Object);
extern Object P_String_Eq_Less (Object, Object);
extern Object P_String_Eq_Greater (Object, Object);
extern Object P_String_CI_Eq (Object, Object);
extern Object P_String_CI_Less (Object, Object);
extern Object P_String_CI_Greater (Object, Object);
extern Object P_String_CI_Eq_Less (Object, Object);
extern Object P_String_CI_Eq_Greater (Object, Object);
extern Object P_String_Length (Object);
extern Object P_String_Ref (Object, Object);
extern Object P_String_Set (Object, Object, Object);
extern Object P_Substring (Object, Object, Object);
extern Object P_String_Copy (Object);
extern Object P_String_Append (int, Object*);
extern Object P_List_To_String (Object);
extern Object P_String_To_List (Object);
extern Object P_Substring_Fill (Object, Object, Object, Object);
extern Object P_String_Fill (Object, Object);
extern Object P_Substringp (Object, Object);
extern Object P_CI_Substringp (Object, Object);
extern int String_Getc (Object);
extern void String_Ungetc (Object, register int);

/* Symbols, variables, frequently used Scheme objects
 */
extern_c Object elk_import Null;
extern_c Object elk_import True;
extern_c Object elk_import False;
extern_c Object elk_import Void;
extern_c Object elk_import Newline;
extern_c Object elk_import Eof;
extern_c Object elk_import Unspecified;
extern Object Intern (const char*);
extern Object CI_Intern (const char*);
extern Object P_Oblist (void);
extern Object P_Symbolp (Object);
extern Object P_Symbol_To_String (Object);
extern Object P_String_To_Symbol (Object);
extern Object P_Put (int, Object*);
extern Object P_Get (Object, Object);
extern Object P_Symbol_Plist (Object);
extern void Define_Variable (Object*, const char*, Object);
extern void Define_Symbol (Object *, const char*);
extern Object Sym_Else;
extern Object Var_Get (Object);
extern void Var_Set (Object, Object);
extern int Var_Is_True (Object);
extern unsigned long int Symbols_To_Bits (Object, int, SYMDESCR*);
extern Object Bits_To_Symbols (unsigned long int, int, SYMDESCR*);

/* Termination functions
 */
extern void Register_Object (Object, GENERIC, PFO, int);
extern void Deregister_Object (Object);
extern Object Find_Object (int, GENERIC, MATCHFUN, ...);
extern void Terminate_Group (GENERIC);
extern void Terminate_Type (int);

/* Types, Define_Type()
 */
extern TYPEDESCR *Types;
extern Object P_Type (Object);
extern void Wrong_Type (Object, int) elk_attribute(__noreturn__);
extern void Wrong_Type_Combination (Object, const char*)
    elk_attribute(__noreturn__);
extern int Define_Type (int, const char*, int (*)(Object), int,
    int (*)(Object, Object), int (*)(Object, Object),
    int (*)(Object, Object, int, int, int),
    int (*)(Object*, int (*)(Object*)) );

/* Vectors
 */
extern Object Make_Vector (unsigned int, Object);
extern Object Make_Const_Vector (unsigned int, Object);
extern Object P_Make_Vector (int, Object*);
extern Object P_Vectorp (Object);
extern Object P_Vector (int, Object*);
extern Object P_Vector_Length (Object);
extern Object P_Vector_Ref (Object, Object);
extern Object P_Vector_Set (Object, Object, Object);
extern Object P_Vector_To_List (Object);
extern Object P_List_To_Vector (Object);
extern Object P_Vector_Fill (Object, Object);
extern Object P_Vector_Copy (Object);

C_LINKAGE_END
