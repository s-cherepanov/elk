#include "scheme.h"

#define RTD(x)     ((struct S_Rtd *)POINTER(x))
#define RECORD(x)  ((struct S_Record *)POINTER(x))

struct S_Rtd {
    Object name;
    Object fields;
};

struct S_Record {
    Object rtd;
    Object values;
};

int T_Rtd, T_Record;

static Object P_Rtdp (x) Object x; {
    return TYPE(x) == T_Rtd ? True : False;
}

static Object P_Recordp (x) Object x; {
    return TYPE(x) == T_Record ? True : False;
}

static Object P_Rtd_Name (x) Object x; {
    Check_Type (x, T_Rtd);
    return RTD(x)->name;
}

static Object P_Rtd_Field_Names (x) Object x; {
    Check_Type (x, T_Rtd);
    return RTD(x)->fields;
}

static Object P_Make_Record_Type (name, fields) Object name, fields; {
    Object s, ismem;
    GC_Node2;

    if (TYPE(name) == T_Symbol)
	name = SYMBOL(name)->name;
    else if (TYPE(name) != T_String)
	Wrong_Type_Combination (name, "string or symbol");
    Check_List (fields);
    for (s = fields; !Nullp (s); s = Cdr (s)) {
	Check_Type (Car (s), T_Symbol);
	ismem = P_Memq (Car (s), Cdr (s));
	if (Truep (ismem))
	    Primitive_Error ("duplicate field name");
    }
    GC_Link2 (name, fields);
    s = Alloc_Object (sizeof (struct S_Rtd), T_Rtd, 0);
    RTD(s)->name = name;
    RTD(s)->fields = fields;
    GC_Unlink;
    return s;
}

static Object P_Record_Type (x) Object x; {
    Check_Type (x, T_Record);
    return RECORD(x)->rtd;
}

static Object P_Record_Values (x) Object x; {
    Check_Type (x, T_Record);
    return RECORD(x)->values;
}

static Object P_Make_Record (rtd, values) Object rtd, values; {
    Object s;
    GC_Node2;

    Check_Type (rtd, T_Rtd);
    Check_Type (values, T_Vector);
    if (VECTOR(values)->size != Fast_Length (RTD(rtd)->fields))
	Primitive_Error ("wrong number of fields for record type");
    GC_Link2 (rtd, values);
    s = Alloc_Object (sizeof (struct S_Record), T_Record, 0);
    RECORD(s)->rtd = rtd;
    RECORD(s)->values = values;
    GC_Unlink;
    return s;
}

static Rtd_Eqv (a, b) Object a, b; { return EQ(a,b); }
#define Record_Eqv Rtd_Eqv

static Rtd_Equal (a, b) Object a, b; {
    return EQ(RTD(a)->name, RTD(b)->name) &&
	   Equal (RTD(a)->fields, RTD(b)->fields);
}

static Record_Equal (a, b) Object a, b; {
    return EQ(RECORD(a)->rtd, RECORD(b)->rtd) &&
	   Equal (RECORD(a)->values, RECORD(b)->values);
}

static Rtd_Print (x, port, raw, depth, length) Object x, port; {
    struct S_String *s = STRING(RTD(x)->name);
    Printf (port, "#[%.*s-record-type %lu]", s->size, s->data, POINTER(x));
}

static Record_Print (x, port, raw, depth, length) Object x, port; {
    struct S_String *s = STRING(RTD(RECORD(x)->rtd)->name);
    Printf (port, "#[%.*s-record-type %lu]", s->size, s->data, POINTER(x));
}

static Rtd_Visit (sp, f) register Object *sp; register (*f)(); {
    (*f)(&RTD(*sp)->name);
    (*f)(&RTD(*sp)->fields);
}

static Record_Visit (sp, f) register Object *sp; register (*f)(); {
    (*f)(&RECORD(*sp)->rtd);
    (*f)(&RECORD(*sp)->values);
}

#define Def_Prim Define_Primitive

elk_init_lib_record () {
    T_Rtd = Define_Type (0, "record-type", NOFUNC, sizeof (struct S_Rtd),
	Rtd_Eqv, Rtd_Equal, Rtd_Print, Rtd_Visit);
    Def_Prim (P_Rtdp,             "record-type?",             1, 1, EVAL);
    Def_Prim (P_Rtd_Name,         "record-type-name",         1, 1, EVAL);
    Def_Prim (P_Rtd_Field_Names,  "record-type-field-names",  1, 1, EVAL);
    Def_Prim (P_Make_Record_Type, "make-record-type",         2, 2, EVAL);

    T_Record = Define_Type (0, "record", NOFUNC, sizeof (struct S_Record),
	Record_Eqv, Record_Equal, Record_Print, Record_Visit);
    Def_Prim (P_Recordp,          "record?",                  1, 1, EVAL);
    Def_Prim (P_Record_Type,      "record-type-descriptor",   1, 1, EVAL);
    Def_Prim (P_Record_Values,    "record-values",            1, 1, EVAL);
    Def_Prim (P_Make_Record,      "make-record",              2, 2, EVAL);
    P_Provide (Intern ("record.o"));
}
