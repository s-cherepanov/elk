/* Miscellaneous macros for type-checking Scheme objects.
 */

#define Check_Type(x,t) {\
    if (TYPE(x) != t) Wrong_Type (x, t);\
}

#define Check_List(x) {\
    if (TYPE(x) != T_Pair && !Nullp (x)) Wrong_Type_Combination (x, "list");\
}

#define Check_Number(x) {\
    register int t = TYPE(x);\
    if (!Numeric (t)) Wrong_Type_Combination (x, "number");\
}

/* This should be renamed; it checks whether x is an *exact* integer.
 */
#define Check_Integer(x) {\
    register int t = TYPE(x);\
    if (t != T_Fixnum && t != T_Bignum) Wrong_Type (x, T_Fixnum);\
}

#define Check_Mutable(x) {\
    if (ISCONST(x))\
	Primitive_Error ("attempt to modify constant");\
}
