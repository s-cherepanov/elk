#include <ctype.h>

#include "kernel.h"

Object Make_Char (c) register c; {
    Object ch;

    SET(ch, T_Character, (unsigned char)c);
    return ch;
}

Object P_Charp (c) Object c; {
    return TYPE(c) == T_Character ? True : False;
}

Object P_Char_To_Integer (c) Object c; {
    Check_Type (c, T_Character);
    return Make_Integer (CHAR(c));
}

Object P_Integer_To_Char (n) Object n; {
    register i;

    if ((i = Get_Exact_Integer (n)) < 0 || i > 255)
	Range_Error (n);
    return Make_Char (i);
}

Object P_Char_Upper_Casep (c) Object c; {
    Check_Type (c, T_Character);
    return isupper (CHAR(c)) ? True : False;
}

Object P_Char_Lower_Casep (c) Object c; {
    Check_Type (c, T_Character);
    return islower (CHAR(c)) ? True : False;
}

Object P_Char_Alphabeticp (c) Object c; {
    Check_Type (c, T_Character);
    return isalpha (CHAR(c)) ? True : False;
}

Object P_Char_Numericp (c) Object c; {
    Check_Type (c, T_Character);
    return isdigit (CHAR(c)) ? True : False;
}

Object P_Char_Whitespacep (c) Object c; {
    register x;

    Check_Type (c, T_Character);
    x = CHAR(c);
    return Whitespace (x) ? True : False;
}

Object P_Char_Upcase (c) Object c; {
    Check_Type (c, T_Character);
    return islower (CHAR(c)) ? Make_Char (toupper (CHAR(c))) : c;
}

Object P_Char_Downcase (c) Object c; {
    Check_Type (c, T_Character);
    return isupper (CHAR(c)) ? Make_Char (tolower (CHAR(c))) : c;
}

General_Chrcmp (c1, c2, ci) Object c1, c2; register ci; {
    Check_Type (c1, T_Character);
    Check_Type (c2, T_Character);
    if (ci)
	return Char_Map[CHAR(c1)] - Char_Map[CHAR(c2)];
    return CHAR(c1) - CHAR(c2);
}

Object P_Char_Eq (c1, c2) Object c1, c2; {
    return General_Chrcmp (c1, c2, 0) ? False : True;
}

Object P_Char_Less (c1, c2) Object c1, c2; {
    return General_Chrcmp (c1, c2, 0) < 0 ? True : False;
}

Object P_Char_Greater (c1, c2) Object c1, c2; {
    return General_Chrcmp (c1, c2, 0) > 0 ? True : False;
}

Object P_Char_Eq_Less (c1, c2) Object c1, c2; {
    return General_Chrcmp (c1, c2, 0) <= 0 ? True : False;
}

Object P_Char_Eq_Greater (c1, c2) Object c1, c2; {
    return General_Chrcmp (c1, c2, 0) >= 0 ? True : False;
}

Object P_Char_CI_Eq (c1, c2) Object c1, c2; {
    return General_Chrcmp (c1, c2, 1) ? False : True;
}

Object P_Char_CI_Less (c1, c2) Object c1, c2; {
    return General_Chrcmp (c1, c2, 1) < 0 ? True : False;
}

Object P_Char_CI_Greater (c1, c2) Object c1, c2; {
    return General_Chrcmp (c1, c2, 1) > 0 ? True : False;
}

Object P_Char_CI_Eq_Less (c1, c2) Object c1, c2; {
    return General_Chrcmp (c1, c2, 1) <= 0 ? True : False;
}

Object P_Char_CI_Eq_Greater (c1, c2) Object c1, c2; {
    return General_Chrcmp (c1, c2, 1) >= 0 ? True : False;
}
