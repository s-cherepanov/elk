/* This include file declares all symbols exported by the interpreter
 * kernel that may be used by applications or extensions.
 */

C_LINKAGE_BEGIN

/* Autoloading
 */
extern Object P_Autoload P_((Object, Object));

/* Bignums
 */
extern Object Make_Uninitialized_Bignum P_((int));
extern void Bignum_Normalize_In_Place P_((struct S_Bignum *));

/* Boolean operators
 */
extern Object P_Booleanp P_((Object));
extern Object P_Not P_((Object));
extern Object P_Eq P_((Object, Object));
extern Object P_Eqv P_((Object, Object));
extern Object P_Equal P_((Object, Object));
extern Object P_Empty_List_Is_False P_((Object));
extern int Eqv P_((Object, Object));
extern int Equal P_((Object, Object));

/* Characters
 */
extern Object Make_Char P_((int));
extern Object P_Charp P_((Object));
extern Object P_Char_Upcase P_((Object));
extern Object P_Char_Downcase P_((Object));
extern Object P_Char_Eq P_((Object, Object));
extern Object P_Char_Less P_((Object, Object));
extern Object P_Char_Greater P_((Object, Object));
extern Object P_Char_Eq_Less P_((Object, Object));
extern Object P_Char_Eq_Greater P_((Object, Object));
extern Object P_Char_CI_Eq P_((Object, Object));
extern Object P_Char_CI_Less P_((Object, Object));
extern Object P_Char_CI_Greater P_((Object, Object));
extern Object P_Char_CI_Eq_Less P_((Object, Object));
extern Object P_Char_CI_Eq_Greater P_((Object, Object));
extern Object P_Char_Upper_Casep P_((Object));
extern Object P_Char_Lower_Casep P_((Object));
extern Object P_Char_Alphabeticp P_((Object));
extern Object P_Char_Numericp P_((Object));
extern Object P_Char_Whitespacep P_((Object));
extern Object P_Char_To_Integer P_((Object));
extern Object P_Integer_To_Char P_((Object));

/* Continuations
 */
extern Object P_Call_With_Current_Continuation P_((Object));
extern Object P_Dynamic_Wind P_((Object, Object, Object));
extern Object P_Control_Pointp P_((Object));
extern Object P_Control_Point_Environment P_((Object));

/* Scheme strings --> C Strings
 */
extern char *Get_String P_((Object));
extern char *Get_Strsym P_((Object));

/* Debugging
 */
extern Object P_Backtrace_List P_((int, Object*));

/* Dump
 */
extern Object P_Dump P_((Object));

/* Lexical bindings, environments
 */
extern Object P_The_Environment P_((void));
extern Object P_Define P_((Object));
extern Object P_Set P_((Object));
extern Object P_Environment_To_List P_((Object));
extern Object P_Define_Macro P_((Object));
extern Object P_Boundp P_((Object));
extern Object P_Global_Environment P_((void));
extern Object P_Environmentp P_((Object));
extern Object The_Environment, Global_Environment;

/* Error handling
 */
extern void Primitive_Error P_((ELLIPSIS)) __attribute__ ((__noreturn__));
extern void Fatal_Error P_((ELLIPSIS)) __attribute__ ((__noreturn__));
extern void Range_Error P_((Object));
extern void Panic P_((const char*));
extern Object P_Error P_((int, Object*));
extern Object P_Reset P_((void));
extern const char *Error_Tag;    /* will be removed in the near future */
extern const char *Get_Error_Tag P_((void));
extern void Set_Error_Tag P_((const char *));
extern void Set_App_Name P_((char *));

/* Interrupts
 */
extern Object P_Disable_Interrupts P_((void));
extern Object P_Enable_Interrupts P_((void));
extern void Signal_Exit P_((int));

/* Features
 */
extern Object P_Features P_((void));
extern Object P_Featurep P_((Object));
extern Object P_Provide P_((Object));
extern Object P_Require P_((int, Object*));

/* Memory allocation, garbage collection
 */
extern int GC_Debug;
extern Object Alloc_Object P_((int, int, int));
extern void Register_Before_GC P_((void (*)(void)));
extern void Register_After_GC P_((void (*)(void)));
extern Object P_Collect P_((void));
extern Object P_Garbage_Collect_Status P_((int, Object *));
#ifdef GENERATIONAL_GC
    extern Object P_Collect_Incremental P_((void));
#endif

/* Files and ports
 */
extern Object Curr_Input_Port, Curr_Output_Port;
extern Object Standard_Input_Port, Standard_Output_Port;
extern void Reset_IO P_((int));
extern Object P_Current_Input_Port P_((void));
extern Object P_Current_Output_Port P_((void));
extern Object P_Input_Portp P_((Object));
extern Object P_Output_Portp P_((Object));
extern Object P_Open_Input_File P_((Object));
extern Object P_Open_Output_File P_((Object));
extern Object P_Open_Input_Output_File P_((Object));
extern Object P_Eof_Objectp P_((Object));
extern Object P_With_Input_From_File P_((Object, Object));
extern Object P_With_Output_To_File P_((Object, Object));
extern Object P_Call_With_Input_File P_((Object, Object));
extern Object P_Call_With_Output_File P_((Object, Object));
extern Object P_Open_Input_String P_((Object));
extern Object P_Open_Output_String P_((void));
extern Object P_Port_File_Name P_((Object));
extern Object P_Tilde_Expand P_((Object));
extern Object P_File_Existsp P_((Object));
extern Object P_Close_Input_Port P_((Object));
extern Object P_Close_Output_Port P_((Object));
extern Object P_Port_Line_Number P_((Object));
extern Object Terminate_File P_((Object));
extern Object Make_Port P_((int, FILE*, Object));
extern int Path_Max P_((void));

/* Loading of files
 */
extern Object P_Load P_((int, Object*));
extern void Load_Source_Port P_((Object));
extern void Load_File P_((char *));

/* Pairs and lists
 */
extern Object P_Cons P_((Object, Object));
extern Object P_Car P_((Object));
extern Object P_Cdr P_((Object));
extern Object P_Set_Car P_((Object, Object));
extern Object P_Set_Cdr P_((Object, Object));
extern Object P_Listp P_((Object));
extern Object P_List P_((int, Object*));
extern Object P_Length P_((Object));
extern Object P_Nullp P_((Object));
extern Object P_Pairp P_((Object));
extern Object P_Cxr P_((Object, Object));
extern Object P_Cddr P_((Object));
extern Object P_Cdar P_((Object));
extern Object P_Cadr P_((Object));
extern Object P_Caar P_((Object));
extern Object P_Cdddr P_((Object));
extern Object P_Cddar P_((Object));
extern Object P_Cdadr P_((Object));
extern Object P_Cdaar P_((Object));
extern Object P_Caddr P_((Object));
extern Object P_Cadar P_((Object));
extern Object P_Caadr P_((Object));
extern Object P_Caaar P_((Object));
extern Object P_Caaaar P_((Object));
extern Object P_Caaadr P_((Object));
extern Object P_Caadar P_((Object));
extern Object P_Caaddr P_((Object));
extern Object P_Cadaar P_((Object));
extern Object P_Cadadr P_((Object));
extern Object P_Caddar P_((Object));
extern Object P_Cadddr P_((Object));
extern Object P_Cdaaar P_((Object));
extern Object P_Cdaadr P_((Object));
extern Object P_Cdadar P_((Object));
extern Object P_Cdaddr P_((Object));
extern Object P_Cddaar P_((Object));
extern Object P_Cddadr P_((Object));
extern Object P_Cdddar P_((Object));
extern Object P_Cddddr P_((Object));
extern Object P_Append P_((int, Object*));
extern Object P_Append_Set P_((int, Object*));
extern Object P_Last_Pair P_((Object));
extern Object P_Reverse P_((Object));
extern Object P_Reverse_Set P_((Object));
extern Object P_List_Tail P_((Object, Object));
extern Object P_List_Ref P_((Object, Object));
extern Object P_Assq P_((Object, Object));
extern Object P_Assv P_((Object, Object));
extern Object P_Assoc P_((Object, Object));
extern Object P_Memq P_((Object, Object));
extern Object P_Memv P_((Object, Object));
extern Object P_Member P_((Object, Object));
extern Object P_Make_List P_((Object, Object));
extern Object Copy_List P_((Object));
extern int Fast_Length P_((Object));
extern Object Const_Cons P_((Object, Object));

/* Startup and termination
 */
extern Object P_Command_Line_Args P_((void));
extern Object P_Exit P_((int, Object*));
extern void Elk_Init P_((int, char **av, int, char *));

/* malloc() and realloc()
 */
extern char *Safe_Malloc P_((unsigned int));
extern char *Safe_Realloc P_((char*, unsigned int));

/* Numbers
 */
extern Object Make_Integer P_((int));
extern Object Make_Unsigned P_((unsigned int));
extern Object Make_Long P_((long int));
extern Object Make_Unsigned_Long P_((unsigned long int));
extern Object Make_Reduced_Flonum P_((double));
extern Object Make_Flonum P_((double));
extern Object P_Numberp P_((Object));
extern Object P_Complexp P_((Object));
extern Object P_Realp P_((Object));
extern Object P_Rationalp P_((Object));
extern Object P_Integerp P_((Object));
extern Object P_Abs P_((Object));
extern Object P_Zerop P_((Object));
extern Object P_Positivep P_((Object));
extern Object P_Negativep P_((Object));
extern Object P_Oddp P_((Object));
extern Object P_Evenp P_((Object));
extern Object P_Exactp P_((Object));
extern Object P_Inexactp P_((Object));
extern Object P_Exact_To_Inexact P_((Object));
extern Object P_Inexact_To_Exact P_((Object));
extern Object P_Inc P_((Object));
extern Object P_Dec P_((Object));
extern Object P_Generic_Equal P_((int, Object*));
extern Object P_Generic_Less P_((int, Object*));
extern Object P_Generic_Greater P_((int, Object*));
extern Object P_Generic_Eq_Less P_((int, Object*));
extern Object P_Generic_Eq_Greater P_((int, Object*));
extern Object P_Generic_Plus P_((int, Object*));
extern Object P_Generic_Minus P_((int, Object*));
extern Object P_Generic_Multiply P_((int, Object*));
extern Object P_Generic_Divide P_((int, Object*));
extern Object P_Quotient P_((Object, Object));
extern Object P_Remainder P_((Object, Object));
extern Object P_Modulo P_((Object, Object));
extern Object P_Gcd P_((int, Object*));
extern Object P_Lcm P_((int, Object*));
extern Object P_Floor P_((Object));
extern Object P_Ceiling P_((Object));
extern Object P_Truncate P_((Object));
extern Object P_Round P_((Object));
extern Object P_Sqrt P_((Object));
extern Object P_Exp P_((Object));
extern Object P_Log P_((Object));
extern Object P_Sin P_((Object));
extern Object P_Cos P_((Object));
extern Object P_Tan P_((Object));
extern Object P_Asin P_((Object));
extern Object P_Acos P_((Object));
extern Object P_Atan P_((int, Object*));
extern Object P_Min P_((int, Object*));
extern Object P_Max P_((int, Object*));
extern Object P_Random P_((void));
extern Object P_Srandom P_((Object));
extern Object P_Number_To_String P_((int, Object*));
extern double Get_Double P_((Object));
extern int Get_Integer P_((Object));
extern unsigned int Get_Unsigned P_((Object));
extern long int Get_Long P_((Object));
extern unsigned long int Get_Unsigned_Long P_((Object));
extern int Get_Exact_Integer P_((Object));
extern unsigned int Get_Exact_Unsigned P_((Object));
extern long int Get_Exact_Long P_((Object));
extern unsigned long int Get_Exact_Unsigned_Long P_((Object));

/* Onfork handlers
 */
extern void Register_Onfork P_((void (*)(void)));
extern void Call_Onfork P_((void));

/* Define_Primitive()
 */
extern void Define_Primitive P_((Object (*)(ELLIPSIS), const char*, int, int,
    enum discipline));

/* Output
 */
extern Object P_Write P_((int, Object*));
extern Object P_Display P_((int, Object*));
extern Object P_Write_Char P_((int, Object*));
extern Object P_Newline P_((int, Object*));
extern Object P_Format P_((int, Object*));
extern Object P_Clear_Output_Port P_((int, Object*));
extern Object P_Flush_Output_Port P_((int, Object*));
extern Object P_Print P_((int, Object*));
extern Object P_Get_Output_String P_((Object));
extern void Check_Output_Port P_((Object));
extern void Discard_Output P_((Object));
extern void Printf P_((ELLIPSIS));
extern void Print_Object P_((Object, Object, int, int, int));
extern void General_Print_Object P_((Object, Object, int));
extern void Format P_((Object, const char*, int, int, Object*));
extern int Saved_Errno;

/* Evaluator, procedures, macros
 */
extern Object Eval P_((Object));
extern Object P_Eval P_((int, Object*));
extern Object P_Apply P_((int, Object*));
extern Object Funcall P_((Object, Object, int));
extern Object P_Lambda P_((Object));
extern Object P_Map P_((int, Object*));
extern Object P_Procedure_Environment P_((Object));
extern Object P_Procedure_Lambda P_((Object));
extern Object P_For_Each P_((int, Object*));
extern Object P_Procedurep P_((Object));
extern Object P_Macro P_((Object));
extern Object P_Macro_Body P_((Object));
extern Object P_Macro_Expand P_((Object));
extern Object P_Primitivep P_((Object));
extern Object P_Compoundp P_((Object));
extern Object P_Macrop P_((Object));
extern void Check_Procedure P_((Object));

/* Delay and force
 */
extern Object P_Delay P_((Object));
extern Object P_Force P_((Object));
extern Object P_Promisep P_((Object));
extern Object P_Promise_Environment P_((Object));

/* Input
 */
extern Object P_Read P_((int, Object*));
extern Object P_Read_Char P_((int, Object*));
extern Object P_Peek_Char P_((int, Object*));
extern Object P_Char_Readyp P_((int, Object*));
extern Object P_Unread_Char P_((int, Object*));
extern Object P_Read_String P_((int, Object*));
extern Object P_Clear_Input_Port P_((int, Object*));
extern Object General_Read P_((Object, int));
extern void Check_Input_Port P_((Object));
extern void Discard_Input P_((Object));
extern void Define_Reader P_((int, READFUN));

/* Special forms
 */
extern Object P_Quote P_((Object));
extern Object P_If P_((Object));
extern Object P_Let P_((Object));
extern Object P_Letseq P_((Object));
extern Object P_Letrec P_((Object));
extern Object P_Case P_((Object));
extern Object P_Cond P_((Object));
extern Object P_And P_((Object));
extern Object P_Or P_((Object));
extern Object P_Do P_((Object));
extern Object P_Quasiquote P_((Object));
extern Object P_Fluid_Let P_((Object));
extern Object P_Begin P_((Object));
extern Object P_Begin1 P_((Object));

/* Strings
 */
extern Object Make_String P_((const char*, int));
extern Object Make_Const_String P_((const char*, int));
extern Object P_Make_String P_((int, Object*));
extern Object P_Stringp P_((Object));
extern Object P_String P_((int, Object*));
extern Object P_String_To_Number P_((int, Object*));
extern Object P_String_Eq P_((Object, Object));
extern Object P_String_Less P_((Object, Object));
extern Object P_String_Greater P_((Object, Object));
extern Object P_String_Eq_Less P_((Object, Object));
extern Object P_String_Eq_Greater P_((Object, Object));
extern Object P_String_CI_Eq P_((Object, Object));
extern Object P_String_CI_Less P_((Object, Object));
extern Object P_String_CI_Greater P_((Object, Object));
extern Object P_String_CI_Eq_Less P_((Object, Object));
extern Object P_String_CI_Eq_Greater P_((Object, Object));
extern Object P_String_Length P_((Object));
extern Object P_String_Ref P_((Object, Object));
extern Object P_String_Set P_((Object, Object, Object));
extern Object P_Substring P_((Object, Object, Object));
extern Object P_String_Copy P_((Object));
extern Object P_String_Append P_((int, Object*));
extern Object P_List_To_String P_((Object));
extern Object P_String_To_List P_((Object));
extern Object P_Substring_Fill P_((Object, Object, Object, Object));
extern Object P_String_Fill P_((Object, Object));
extern Object P_Substringp P_((Object, Object));
extern Object P_CI_Substringp P_((Object, Object));

/* Symbols, variables, frequently used Scheme objects
 */
extern Object Null, True, False, Void, Newline, Eof;
extern Object Intern P_((const char*));
extern Object CI_Intern P_((const char*));
extern Object P_Oblist P_((void));
extern Object P_Symbolp P_((Object));
extern Object P_Symbol_To_String P_((Object));
extern Object P_String_To_Symbol P_((Object));
extern Object P_Put P_((int, Object*));
extern Object P_Get P_((Object, Object));
extern Object P_Symbol_Plist P_((Object));
extern void Define_Variable P_((Object*, const char*, Object));
extern void Define_Symbol P_((Object *, const char*));
extern Object Sym_Else;
extern Object Var_Get P_((Object));
extern void Var_Set P_((Object, Object));
extern int Var_Is_True P_((Object));
extern unsigned long int Symbols_To_Bits P_((Object, int, SYMDESCR*));
extern Object Bits_To_Symbols P_((unsigned long int, int, SYMDESCR*));

/* Termination functions
 */
extern void Register_Object P_((Object, GENERIC, PFO, int));
extern void Deregister_Object P_((Object));
extern Object Find_Object P_((ELLIPSIS));
extern void Terminate_Group P_((GENERIC));
extern void Terminate_Type P_((int));

/* Types, Define_Type()
 */
extern TYPEDESCR *Types;
extern Object P_Type P_((Object));
extern void Wrong_Type P_((Object, int)) __attribute__ ((__noreturn__));
extern void Wrong_Type_Combination P_((Object, const char*))
    __attribute__ ((__noreturn__));
extern int Define_Type P_((int, const char*, int (*)(Object), int,
    int (*)(Object, Object), int (*)(Object, Object),
    int (*)(Object, Object, int, int, int),
    int (*)(Object*, int (*)(Object*)) ));

/* Vectors
 */
extern Object Make_Vector P_((int, Object));
extern Object Make_Const_Vector P_((int, Object));
extern Object P_Make_Vector P_((int, Object*));
extern Object P_Vectorp P_((Object));
extern Object P_Vector P_((int, Object*));
extern Object P_Vector_Length P_((Object));
extern Object P_Vector_Ref P_((Object, Object));
extern Object P_Vector_Set P_((Object, Object, Object));
extern Object P_Vector_To_List P_((Object));
extern Object P_List_To_Vector P_((Object));
extern Object P_Vector_Fill P_((Object, Object));
extern Object P_Vector_Copy P_((Object));

C_LINKAGE_END
