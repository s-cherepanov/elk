/* These must be defined as macros, because they use Alloca().
 */

#define Get_String_Stack(_from,_to) {\
    int _len;\
    Check_Type(_from, T_String);\
    _len = STRING(_from)->size;\
    Alloca ((_to), char*, _len+1);\
    bcopy (STRING(_from)->data, (_to), _len);\
    (_to)[_len] = '\0';\
}

#define Get_Strsym_Stack(_from,_to) {\
    int _len;\
    if (TYPE(_from) == T_Symbol)\
	(_from) = SYMBOL(_from)->name;\
    else if (TYPE(_from) != T_String)\
	Wrong_Type_Combination ((_from), "string or symbol");\
    _len = STRING(_from)->size;\
    Alloca ((_to), char*, _len+1);\
    bcopy (STRING(_from)->data, (_to), _len);\
    (_to)[_len] = '\0';\
}
