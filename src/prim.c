/* prim.c: Built-in primitives, Define_Primitive().
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

#include "kernel.h"

extern void Memoize_Frame (Object);

struct Prim_Init {
    Object (*fun)();
    char *name;
    int minargs, maxargs;
    enum discipline disc;
} Primitives[] = {

    /* autoload.c:
     */
    { P_Autoload,          "autoload",                       2, 2,    EVAL },

    /* bool.c:
     */
    { P_Booleanp,          "boolean?",                       1, 1,    EVAL },
    { P_Not,               "not",                            1, 1,    EVAL },
    { P_Eq,                "eq?",                            2, 2,    EVAL },
    { P_Eqv,               "eqv?",                           2, 2,    EVAL },
    { P_Equal,             "equal?",                         2, 2,    EVAL },
    { P_Empty_List_Is_False, "empty-list-is-false-for-backward-compatibility",
                                                             1, 1,    EVAL },

    /* char.c:
     */
    { P_Charp,             "char?",                          1, 1,    EVAL },
    { P_Char_To_Integer,   "char->integer",                  1, 1,    EVAL },
    { P_Integer_To_Char,   "integer->char",                  1, 1,    EVAL },
    { P_Char_Upper_Casep,  "char-upper-case?",               1, 1,    EVAL },
    { P_Char_Lower_Casep,  "char-lower-case?",               1, 1,    EVAL },
    { P_Char_Alphabeticp,  "char-alphabetic?",               1, 1,    EVAL },
    { P_Char_Numericp,     "char-numeric?",                  1, 1,    EVAL },
    { P_Char_Whitespacep,  "char-whitespace?",               1, 1,    EVAL },
    { P_Char_Upcase,       "char-upcase",                    1, 1,    EVAL },
    { P_Char_Downcase,     "char-downcase",                  1, 1,    EVAL },
    { P_Char_Eq,           "char=?",                         2, 2,    EVAL },
    { P_Char_Less,         "char<?",                         2, 2,    EVAL },
    { P_Char_Greater,      "char>?",                         2, 2,    EVAL },
    { P_Char_Eq_Less,      "char<=?",                        2, 2,    EVAL },
    { P_Char_Eq_Greater,   "char>=?",                        2, 2,    EVAL },
    { P_Char_CI_Eq,        "char-ci=?",                      2, 2,    EVAL },
    { P_Char_CI_Less,      "char-ci<?",                      2, 2,    EVAL },
    { P_Char_CI_Greater,   "char-ci>?",                      2, 2,    EVAL },
    { P_Char_CI_Eq_Less,   "char-ci<=?",                     2, 2,    EVAL },
    { P_Char_CI_Eq_Greater,"char-ci>=?",                     2, 2,    EVAL },

    /* cont.c:
     */
    { P_Control_Pointp,    "control-point?",                 1, 1,    EVAL },
    { P_Call_With_Current_Continuation,
                         "call-with-current-continuation", 1, 1,    EVAL },
    { P_Dynamic_Wind,      "dynamic-wind",                   3, 3,    EVAL },
    { P_Control_Point_Environment,
                         "control-point-environment",      1, 1,    EVAL },

    /* debug.c:
     */
    { P_Backtrace_List,    "backtrace-list",                 0, 1,    VARARGS },

    /* dump.c:
     */
#ifdef CAN_DUMP
    { P_Dump,              "dump",                           1, 1,    EVAL },
#endif

    /* env.c:
     */
    { P_Environmentp,      "environment?",                   1, 1,    EVAL },
    { P_The_Environment,   "the-environment",                0, 0,    EVAL },
    { P_Global_Environment,"global-environment",             0, 0,    EVAL },
    { P_Define,            "define",                         1, MANY, NOEVAL },
    { P_Define_Macro,      "define-macro",                   1, MANY, NOEVAL },
    { P_Set,               "set!",                           2, 2,    NOEVAL },
    { P_Environment_To_List,
                         "environment->list",              1, 1,    EVAL },
    { P_Boundp,            "bound?",                         1, 1,    EVAL },

    /* error.c:
     */
    { P_Error,             "error",                          2, MANY, VARARGS },
    { P_Reset,             "reset",                          0, 0,    EVAL },

    /* exception.c:
     */
    { P_Disable_Interrupts,"disable-interrupts",             0, 0,    EVAL },
    { P_Enable_Interrupts, "enable-interrupts",              0, 0,    EVAL },

    /* feature.c:
     */
    { P_Features,          "features",                       0, 0,    EVAL },
    { P_Featurep,          "feature?",                       1, 1,    EVAL },
    { P_Provide,           "provide",                        1, 1,    EVAL },
    { P_Require,           "require",                        1, 3,    VARARGS },

    /* heap.c:
     */
    { P_Collect,           "collect",                        0, 0,    EVAL },
    { P_Garbage_Collect_Status, "garbage-collect-status",    0, 2,    VARARGS },
#ifdef GENERATIONAL_GC
    { P_Collect_Incremental, "collect-incremental",          0, 0,    EVAL },
#endif


    /* io.c:
     */
    { P_Port_File_Name,    "port-file-name",                 1, 1,    EVAL },
    { P_Port_Line_Number,  "port-line-number",               1, 1,    EVAL },
    { P_Eof_Objectp,       "eof-object?",                    1, 1,    EVAL },
    { P_Current_Input_Port,
                         "current-input-port",             0, 0,    EVAL },
    { P_Current_Output_Port,
                         "current-output-port",            0, 0,    EVAL },
    { P_Input_Portp,       "input-port?",                    1, 1,    EVAL },
    { P_Output_Portp,      "output-port?",                   1, 1,    EVAL },
    { P_Open_Input_File,   "open-input-file",                1, 1,    EVAL },
    { P_Open_Output_File,  "open-output-file",               1, 1,    EVAL },
    { P_Open_Input_Output_File, "open-input-output-file",    1, 1,    EVAL },
    { P_Close_Input_Port,  "close-input-port",               1, 1,    EVAL },
    { P_Close_Output_Port, "close-output-port",              1, 1,    EVAL },
    { P_With_Input_From_File,    "with-input-from-file",     2, 2,    EVAL },
    { P_With_Output_To_File,     "with-output-to-file",      2, 2,    EVAL },
    { P_Call_With_Input_File,    "call-with-input-file",     2, 2,    EVAL },
    { P_Call_With_Output_File,   "call-with-output-file",    2, 2,    EVAL },
    { P_Open_Input_String, "open-input-string",              1, 1,    EVAL },
    { P_Open_Output_String,"open-output-string",             0, 0,    EVAL },
    { P_Tilde_Expand,      "tilde-expand",                   1, 1,    EVAL },
    { P_File_Existsp,      "file-exists?",                   1, 1,    EVAL },

    /* load.c:
     */
    { P_Load,              "load",                           1, 2,    VARARGS },

    /* list.c:
     */
    { P_Cons,              "cons",                           2, 2,    EVAL },
    { P_Car,               "car",                            1, 1,    EVAL },
    { P_Cdr,               "cdr",                            1, 1,    EVAL },
    { P_Caar,              "caar",                           1, 1,    EVAL },
    { P_Cadr,              "cadr",                           1, 1,    EVAL },
    { P_Cdar,              "cdar",                           1, 1,    EVAL },
    { P_Cddr,              "cddr",                           1, 1,    EVAL },

    { P_Caaar,             "caaar",                          1, 1,    EVAL },
    { P_Caadr,             "caadr",                          1, 1,    EVAL },
    { P_Cadar,             "cadar",                          1, 1,    EVAL },
    { P_Caddr,             "caddr",                          1, 1,    EVAL },
    { P_Cdaar,             "cdaar",                          1, 1,    EVAL },
    { P_Cdadr,             "cdadr",                          1, 1,    EVAL },
    { P_Cddar,             "cddar",                          1, 1,    EVAL },
    { P_Cdddr,             "cdddr",                          1, 1,    EVAL },

    { P_Caaaar,            "caaaar",                         1, 1,    EVAL },
    { P_Caaadr,            "caaadr",                         1, 1,    EVAL },
    { P_Caadar,            "caadar",                         1, 1,    EVAL },
    { P_Caaddr,            "caaddr",                         1, 1,    EVAL },
    { P_Cadaar,            "cadaar",                         1, 1,    EVAL },
    { P_Cadadr,            "cadadr",                         1, 1,    EVAL },
    { P_Caddar,            "caddar",                         1, 1,    EVAL },
    { P_Cadddr,            "cadddr",                         1, 1,    EVAL },
    { P_Cdaaar,            "cdaaar",                         1, 1,    EVAL },
    { P_Cdaadr,            "cdaadr",                         1, 1,    EVAL },
    { P_Cdadar,            "cdadar",                         1, 1,    EVAL },
    { P_Cdaddr,            "cdaddr",                         1, 1,    EVAL },
    { P_Cddaar,            "cddaar",                         1, 1,    EVAL },
    { P_Cddadr,            "cddadr",                         1, 1,    EVAL },
    { P_Cdddar,            "cdddar",                         1, 1,    EVAL },
    { P_Cddddr,            "cddddr",                         1, 1,    EVAL },

    { P_Cxr,               "cxr",                            2, 2,    EVAL },
    { P_Nullp,             "null?",                          1, 1,    EVAL },
    { P_Pairp,             "pair?",                          1, 1,    EVAL },
    { P_Listp,             "list?",                          1, 1,    EVAL },
    { P_Set_Car,           "set-car!",                       2, 2,    EVAL },
    { P_Set_Cdr,           "set-cdr!",                       2, 2,    EVAL },
    { P_Assq,              "assq",                           2, 2,    EVAL },
    { P_Assv,              "assv",                           2, 2,    EVAL },
    { P_Assoc,             "assoc",                          2, 2,    EVAL },
    { P_Memq,              "memq",                           2, 2,    EVAL },
    { P_Memv,              "memv",                           2, 2,    EVAL },
    { P_Member,            "member",                         2, 2,    EVAL },
    { P_Make_List,         "make-list",                      2, 2,    EVAL },
    { P_List,              "list",                           0, MANY, VARARGS },
    { P_Length,            "length",                         1, 1,    EVAL },
    { P_Append,            "append",                         0, MANY, VARARGS },
    { P_Append_Set,        "append!",                        0, MANY, VARARGS },
    { P_Last_Pair,         "last-pair",                      1, 1,    EVAL },
    { P_Reverse,           "reverse",                        1, 1,    EVAL },
    { P_Reverse_Set,       "reverse!",                       1, 1,    EVAL },
    { P_List_Tail,         "list-tail",                      2, 2,    EVAL },
    { P_List_Ref,          "list-ref",                       2, 2,    EVAL },

    /* main.c:
     */
    { P_Command_Line_Args, "command-line-args",              0, 0,    EVAL },
    { P_Exit,              "exit",                           0, 1,    VARARGS },

    /* math.c:
     */
    { P_Number_To_String,  "number->string",                 1, 2,    VARARGS },
    { P_Numberp,           "number?",                        1, 1,    EVAL },
    { P_Complexp,          "complex?",                       1, 1,    EVAL },
    { P_Realp,             "real?",                          1, 1,    EVAL },
    { P_Rationalp,         "rational?",                      1, 1,    EVAL },
    { P_Integerp,          "integer?",                       1, 1,    EVAL },
    { P_Zerop,             "zero?",                          1, 1,    EVAL },
    { P_Positivep,         "positive?",                      1, 1,    EVAL },
    { P_Negativep,         "negative?",                      1, 1,    EVAL },
    { P_Oddp,              "odd?",                           1, 1,    EVAL },
    { P_Evenp,             "even?",                          1, 1,    EVAL },
    { P_Exactp,            "exact?",                         1, 1,    EVAL },
    { P_Inexactp,          "inexact?",                       1, 1,    EVAL },
    { P_Exact_To_Inexact,  "exact->inexact",                 1, 1,    EVAL },
    { P_Inexact_To_Exact,  "inexact->exact",                 1, 1,    EVAL },
    { P_Generic_Less,      "<",                              1, MANY, VARARGS },
    { P_Generic_Greater,   ">",                              1, MANY, VARARGS },
    { P_Generic_Equal,     "=",                              1, MANY, VARARGS },
    { P_Generic_Eq_Less,   "<=",                             1, MANY, VARARGS },
    { P_Generic_Eq_Greater,">=",                             1, MANY, VARARGS },
    { P_Inc,               "1+",                             1, 1,    EVAL },
    { P_Dec,               "-1+",                            1, 1,    EVAL },
    { P_Dec,               "1-",                             1, 1,    EVAL },
    { P_Generic_Plus,      "+",                              0, MANY, VARARGS },
    { P_Generic_Minus,     "-",                              1, MANY, VARARGS },
    { P_Generic_Multiply,  "*",                              0, MANY, VARARGS },
    { P_Generic_Divide,    "/",                              1, MANY, VARARGS },
    { P_Abs,               "abs",                            1, 1,    EVAL },
    { P_Quotient,          "quotient",                       2, 2,    EVAL },
    { P_Remainder,         "remainder",                      2, 2,    EVAL },
    { P_Modulo,            "modulo",                         2, 2,    EVAL },
    { P_Gcd,               "gcd",                            0, MANY, VARARGS },
    { P_Lcm,               "lcm",                            0, MANY, VARARGS },
    { P_Floor,             "floor",                          1, 1,    EVAL },
    { P_Ceiling,           "ceiling",                        1, 1,    EVAL },
    { P_Truncate,          "truncate",                       1, 1,    EVAL },
    { P_Round,             "round",                          1, 1,    EVAL },
    { P_Sqrt,              "sqrt",                           1, 1,    EVAL },
    { P_Exp,               "exp",                            1, 1,    EVAL },
    { P_Pow,               "pow",                            2, 2,    EVAL },
    { P_Log,               "log",                            1, 1,    EVAL },
    { P_Sin,               "sin",                            1, 1,    EVAL },
    { P_Cos,               "cos",                            1, 1,    EVAL },
    { P_Tan,               "tan",                            1, 1,    EVAL },
    { P_Asin,              "asin",                           1, 1,    EVAL },
    { P_Acos,              "acos",                           1, 1,    EVAL },
    { P_Atan,              "atan",                           1, 2,    VARARGS },
    { P_Min,               "min",                            1, MANY, VARARGS },
    { P_Max,               "max",                            1, MANY, VARARGS },
    { P_Random,            "random",                         0, 0,    EVAL },
    { P_Srandom,           "srandom",                        1, 1,    EVAL },

    /* prim.c:
     */

    /* print.c:
     */
    { P_Write,             "write",                          1, 2,    VARARGS },
    { P_Display,           "display",                        1, 2,    VARARGS },
    { P_Write_Char,        "write-char",                     1, 2,    VARARGS },
    { P_Newline,           "newline",                        0, 1,    VARARGS },
    { P_Print,             "print",                          1, 2,    VARARGS },
    { P_Clear_Output_Port, "clear-output-port",              0, 1,    VARARGS },
    { P_Flush_Output_Port, "flush-output-port",              0, 1,    VARARGS },
    { P_Get_Output_String, "get-output-string",              1, 1,    EVAL },
    { P_Format,            "format",                         2, MANY, VARARGS },

    /* proc.c:
     */
    { P_Procedurep,        "procedure?",                     1, 1,    EVAL },
    { P_Primitivep,        "primitive?",                     1, 1,    EVAL },
    { P_Compoundp,         "compound?",                      1, 1,    EVAL },
    { P_Macrop,            "macro?",                         1, 1,    EVAL },
    { P_Eval,              "eval",                           1, 2,    VARARGS },
    { P_Apply,             "apply",                          2, MANY, VARARGS },
    { P_Lambda,            "lambda",                         2, MANY, NOEVAL },
    { P_Procedure_Environment,
                         "procedure-environment",          1, 1,    EVAL },
    { P_Procedure_Lambda,  "procedure-lambda",               1, 1,    EVAL },
    { P_Map,               "map",                            2, MANY, VARARGS },
    { P_For_Each,          "for-each",                       2, MANY, VARARGS },
    { P_Macro,             "macro",                          2, MANY, NOEVAL },
    { P_Macro_Body,        "macro-body",                     1, 1,    EVAL },
    { P_Macro_Expand,      "macro-expand",                   1, 1,    EVAL },

    /* promise.c:
     */
    { P_Delay,             "delay",                          1, 1,    NOEVAL },
    { P_Force,             "force",                          1, 1,    EVAL },
    { P_Promisep,          "promise?",                       1, 1,    EVAL },
    { P_Promise_Environment,
                         "promise-environment",            1, 1,    EVAL },

    /* read.c:
     */
    { P_Clear_Input_Port,  "clear-input-port",               0, 1,    VARARGS },
    { P_Read,              "read",                           0, 1,    VARARGS },
    { P_Read_Char,         "read-char",                      0, 1,    VARARGS },
    { P_Read_String,       "read-string",                    0, 1,    VARARGS },
    { P_Unread_Char,       "unread-char",                    1, 2,    VARARGS },
    { P_Peek_Char,         "peek-char",                      0, 1,    VARARGS },
    { P_Char_Readyp,       "char-ready?",                    0, 1,    VARARGS },

    /* special.c:
     */
    { P_Quote,             "quote",                          1, 1,    NOEVAL },
    { P_Quasiquote,        "quasiquote",                     1, 1,    NOEVAL },
    { P_Begin,             "begin",                          0, MANY, NOEVAL },
    { P_Begin1,            "begin1",                         0, MANY, NOEVAL },
    { P_If,                "if",                             2, MANY, NOEVAL },
    { P_Case,              "case",                           2, MANY, NOEVAL },
    { P_Cond,              "cond",                           0, MANY, NOEVAL },
    { P_Do,                "do",                             2, MANY, NOEVAL },
    { P_Let,               "let",                            2, MANY, NOEVAL },
    { P_Letseq,            "let*",                           2, MANY, NOEVAL },
    { P_Letrec,            "letrec",                         2, MANY, NOEVAL },
    { P_Fluid_Let,         "fluid-let",                      2, MANY, NOEVAL },
    { P_And,               "and",                            0, MANY, NOEVAL },
    { P_Or,                "or",                             0, MANY, NOEVAL },

    /* string.c:
     */
    { P_String,            "string",                         0, MANY, VARARGS },
    { P_Stringp,           "string?",                        1, 1,    EVAL },
    { P_Make_String,       "make-string",                    1, 2,    VARARGS },
    { P_String_Length,     "string-length",                  1, 1,    EVAL },
    { P_String_To_Number,  "string->number",                 1, 2,    VARARGS },
    { P_String_Ref,        "string-ref",                     2, 2,    EVAL },
    { P_String_Set,        "string-set!",                    3, 3,    EVAL },
    { P_Substring,         "substring",                      3, 3,    EVAL },
    { P_String_Copy,       "string-copy",                    1, 1,    EVAL },
    { P_String_Append,     "string-append",                  0, MANY, VARARGS },
    { P_List_To_String,    "list->string",                   1, 1,    EVAL },
    { P_String_To_List,    "string->list",                   1, 1,    EVAL },
    { P_String_Fill,       "string-fill!",                   2, 2,    EVAL },
    { P_Substring_Fill,    "substring-fill!",                4, 4,    EVAL },
    { P_String_Eq,         "string=?",                       2, 2,    EVAL },
    { P_String_Less,       "string<?",                       2, 2,    EVAL },
    { P_String_Greater,    "string>?",                       2, 2,    EVAL },
    { P_String_Eq_Less,    "string<=?",                      2, 2,    EVAL },
    { P_String_Eq_Greater, "string>=?",                      2, 2,    EVAL },
    { P_String_CI_Eq,      "string-ci=?",                    2, 2,    EVAL },
    { P_String_CI_Less,    "string-ci<?",                    2, 2,    EVAL },
    { P_String_CI_Greater, "string-ci>?",                    2, 2,    EVAL },
    { P_String_CI_Eq_Less, "string-ci<=?",                   2, 2,    EVAL },
    { P_String_CI_Eq_Greater,
                         "string-ci>=?",                   2, 2,    EVAL },
    { P_Substringp,        "substring?",                     2, 2,    EVAL },
    { P_CI_Substringp,     "substring-ci?",                  2, 2,    EVAL },

    /* symbol.c:
     */
    { P_String_To_Symbol,  "string->symbol",                 1, 1,    EVAL },
    { P_Oblist,            "oblist",                         0, 0,    EVAL },
    { P_Symbolp,           "symbol?",                        1, 1,    EVAL },
    { P_Symbol_To_String,  "symbol->string",                 1, 1,    EVAL },
    { P_Put,               "put",                            2, 3,    VARARGS },
    { P_Get,               "get",                            2, 2,    EVAL },
    { P_Symbol_Plist,      "symbol-plist",                   1, 1,    EVAL },

    /* type.c:
     */
    { P_Type,              "type",                           1, 1,    EVAL },

    /* vector.c:
     */
    { P_Vectorp,           "vector?",                        1, 1,    EVAL },
    { P_Make_Vector,       "make-vector",                    1, 2,    VARARGS },
    { P_Vector,            "vector",                         0, MANY, VARARGS },
    { P_Vector_Length,     "vector-length",                  1, 1,    EVAL },
    { P_Vector_Ref,        "vector-ref",                     2, 2,    EVAL },
    { P_Vector_Set,        "vector-set!",                    3, 3,    EVAL },
    { P_Vector_To_List,    "vector->list",                   1, 1,    EVAL },
    { P_List_To_Vector,    "list->vector",                   1, 1,    EVAL },
    { P_Vector_Fill,       "vector-fill!",                   2, 2,    EVAL },
    { P_Vector_Copy,       "vector-copy",                    1, 1,    EVAL },

    { 0 }
};

/* The C-compiler can't initialize unions, thus the primitive procedures
 * must be created during run-time (the problem actually is that one can't
 * provide an intializer for the "tag" component of an S_Primitive).
 */

void Init_Prim () {
    register struct Prim_Init *p;
    Object frame, prim, sym;

    for (frame = Car (The_Environment), p = Primitives; p->fun; p++) {
        prim = Make_Primitive (p->fun, p->name, p->minargs, p->maxargs,
            p->disc);
        sym = Intern (p->name);
        frame = Add_Binding (frame, sym, prim);
    }
    Car (The_Environment) = frame;
    Memoize_Frame (frame);
}

void Define_Primitive (Object (*fun)(), char const *name, int min, int max,
        enum discipline disc) {
    Object prim, sym, frame;
    GC_Node2;

    Set_Error_Tag ("define-primitive");
    prim = Make_Primitive (fun, name, min, max, disc);
    sym = Null;
    GC_Link2 (prim, sym);
    sym = Intern (name);
    if (disc == EVAL && min != max)
        Primitive_Error ("~s: number of arguments must be fixed", sym);
    frame = Add_Binding (Car (The_Environment), sym, prim);
    SYMBOL(sym)->value = prim;
    Car (The_Environment) = frame;
    GC_Unlink;
}
