/* class.c
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

#include "xt.h"

#define MAX_CLASS                128
#define MAX_CALLBACK_PER_CLASS   10

typedef struct {
    char *name;
    int has_arg;
} CALLBACK_INFO;

typedef struct {
    WidgetClass class;
    char *name;
    CALLBACK_INFO cb[MAX_CALLBACK_PER_CLASS], *cblast;
    XtResourceList sub_resources;
    int num_resources;
} CLASS_INFO;

static CLASS_INFO ctab[MAX_CLASS], *clast = ctab;

Generic_Predicate (Class)

Generic_Simple_Equal (Class, CLASS, wclass)

Generic_Print (Class, "#[class %s]", CLASS(x)->name)

Object Make_Class (WidgetClass class, char *name) {
    Object c;

    c = Find_Object (T_Class, (GENERIC)0, Match_Xt_Obj, class);
    if (Nullp (c)) {
        c = Alloc_Object (sizeof (struct S_Class), T_Class, 0);
        CLASS(c)->tag = Null;
        CLASS(c)->wclass = class;
        CLASS(c)->name = name;
        Register_Object (c, (GENERIC)0, (PFO)0, 0);
        /* See comment in Define_Class below */
        XtInitializeWidgetClass (class);
    }
    return c;
}

Object Make_Widget_Class (WidgetClass class) {
    register CLASS_INFO *p;

    for (p = ctab; p < clast; p++)
        if (p->class == class)
            return Make_Class (class, p->name);
    Primitive_Error ("undefined widget class ~s", Xt_Class_Name (class));
    /*NOTREACHED*/
}

static Object P_Find_Class (Object name) {
    register CLASS_INFO *p;
    register char *s = Get_Strsym (name);

    for (p = ctab; p < clast; p++) {
        if (streq (p->name, s))
            return Make_Class (p->class, p->name);
    }
    Primitive_Error ("no such widget class: ~s", name);
    /*NOTREACHED*/
}

static Object P_Class_Existsp (Object name) {
    register CLASS_INFO *p;
    register char *s = Get_Strsym (name);

    for (p = ctab; p < clast; p++) {
        if (streq (p->name, s))
            return True;
    }
    return False;
}

char *Class_Name (WidgetClass class) {
    register CLASS_INFO *p;

    for (p = ctab; p < clast && p->class != class; p++)
        ;
    if (p == clast)
        return "unknown";
    return p->name;
}

void Get_Sub_Resource_List (WidgetClass class, XtResourceList *rp,
                            Cardinal *np) {
    register CLASS_INFO *p;

    for (p = ctab; p < clast && p->class != class; p++)
        ;
    if (p == clast)
        Primitive_Error ("undefined widget class ~s", Xt_Class_Name (class));
    *np = p->num_resources;
    *rp = p->sub_resources;
}

static Object P_Class_Resources (Object c) {
    Check_Type (c, T_Class);
    return Get_Resources (CLASS(c)->wclass, XtGetResourceList, 1);
}

static Object P_Class_Constraint_Resources (Object c) {
    Check_Type (c, T_Class);
    return Get_Resources (CLASS(c)->wclass, XtGetConstraintResourceList, 1);
}

static Object P_Class_Sub_Resources (Object c) {
    Check_Type (c, T_Class);
    return Get_Resources (CLASS(c)->wclass, Get_Sub_Resource_List, 0);
}

void Define_Class (char *name, WidgetClass class, XtResourceList r, int nr) {
    Set_Error_Tag ("define-class");
    if (clast == ctab+MAX_CLASS)
        Primitive_Error ("too many widget classes");
    /*
     * The next line should read:
     *    XtInitializeWidgetClass (class);
     * However, there is a bug in Motif 1.1.4 that causes an application
     * to drop core if the row-column widget class is initialized before
     * the first vendor-shell widget has been created.
     * Thus, we can't initialize any classes at this point; we will do
     * it in Make_Class above instead.
     * This essentially causes a class to be initialized the first time
     * it is used.
     */
    clast->name = name;
    clast->class = class;
    clast->cb[0].name = XtNdestroyCallback;
    clast->cb[0].has_arg = 0;
    clast->cblast = clast->cb+1;
    clast->sub_resources = r;
    clast->num_resources = nr;
    clast++;
}

void Define_Callback (char *cl, char *s, int has_arg) {
    register CLASS_INFO *p;

    Set_Error_Tag ("define-callback");
    for (p = ctab; p < clast; p++)
        if (streq (p->name, cl)) {
            if (p->cblast == p->cb+MAX_CALLBACK_PER_CLASS)
                Primitive_Error ("too many callbacks for this class");
            p->cblast->name = s;
            p->cblast->has_arg = has_arg;
            p->cblast++;
            return;
        }
    Primitive_Error ("undefined class");
}

PFX2S Find_Callback_Converter (WidgetClass c, char *name, Object sname) {
    register CLASS_INFO *p;
    register CALLBACK_INFO *q;
    PFX2S conv;

    for (p = ctab; p < clast; p++)
        if (p->class == c) {
            for (q = p->cb; q < p->cblast; q++)
                if (streq (q->name, name)) {
                    if (q->has_arg) {
                        char s1[128], s2[128], msg[256];

                        /* First look for a class specific converter
                         * then for a general one.  Callback converters
                         * have a prefix "callback:" to avoid name conflicts
                         * with converters for normal resources.
                         */
                        sprintf (s1, "callback:%s-%s", p->name, name);
                        conv = Find_Converter_To_Scheme (s1);
                        if (conv == 0) {
                            sprintf(s2, "callback:%s", name);
                            conv = Find_Converter_To_Scheme (s2);
                            if (conv == 0) {
                                sprintf (msg,
                                    "no callback converter for %s or %s",
                                        s1, s2);
                                Primitive_Error (msg);
                            }
                        }
                        return conv;
                    } else return (PFX2S)0;
                }
            Primitive_Error ("no such callback: ~s", sname);
        }
    Primitive_Error ("undefined widget class ~s", Xt_Class_Name (c));
    /*NOTREACHED*/
}

void elk_init_xt_class () {
    Generic_Define (Class, "class", "class?");
    Define_Primitive (P_Find_Class,        "find-class",        1, 1, EVAL);
    Define_Primitive (P_Class_Resources,   "class-resources",   1, 1, EVAL);
    Define_Primitive (P_Class_Constraint_Resources,
                               "class-constraint-resources",    1, 1, EVAL);
    Define_Primitive (P_Class_Sub_Resources,
                               "class-sub-resources",           1, 1, EVAL);
    Define_Primitive (P_Class_Existsp,     "class-exists?",     1, 1, EVAL);
    /*
     * Doesn't work with Motif-1.1.0:
     *
    Define_Class ("simple", simpleWidgetClass, (XtResourceList)0, 0);
     */
    Define_Class ("core", widgetClass, (XtResourceList)0, 0);
    Define_Class ("constraint", constraintWidgetClass, (XtResourceList)0, 0);
    Define_Class ("composite", compositeWidgetClass, (XtResourceList)0, 0);
}
