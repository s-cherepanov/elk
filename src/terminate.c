/* terminate.c: Termination functions, weak pointers.
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

#include <stdarg.h>
#include <stdlib.h>

#include "kernel.h"

static WEAK_NODE *first;

void Call_Terminators();

void Init_Terminate () {
    Register_After_GC (Call_Terminators);
}

/* Register an object with the given group and termination function;
 * object can be marked as LEADER.
 */
void Register_Object (Object obj, GENERIC group, PFO term, int leader_flag) {
    WEAK_NODE *p;

    p = (WEAK_NODE *)Safe_Malloc (sizeof (*p));
    p->obj = obj;
    p->group = group;
    p->term = term;
    p->flags = leader_flag? WK_LEADER : 0;
    p->next = first;
    first = p;
}

void Deregister_Object (Object obj) {
    WEAK_NODE *p, **pp;

    Disable_Interrupts;
    for (pp = &first; (p = *pp); ) {
        if (WAS_FORWARDED(p->obj))
            UPDATE_OBJ(p->obj);
        if (EQ(p->obj, obj)) {
            *pp = p->next;
            free ((char *)p);
        } else pp = &p->next;
    }
    Enable_Interrupts;
}

/* Search for an object of a given type (arg 1) and group (arg 2).
 * Use the given match function (arg 3); it is called with an object
 * and the remaining arguments of Find_Object() (a va_list).
 * Null is returned when the object has not been found.
 */
/*VARARGS*/
Object Find_Object (int type, GENERIC group, MATCHFUN match, ...) {
    WEAK_NODE *p;
    va_list args;

    va_start (args, match);
    for (p = first; p; p = p->next) {
        if (TYPE(p->obj) != type || p->group != group)
            continue;
        /*
         * I believe updating the object is wrong here, as Find_Object() may
         * be called from within GC (see Widget_Visit() in lib/xt/widget.c).
         * If an object is updated here, it will no longer be regarded as
         * alive in the call to Call_Terminators() later.
         *
        if (WAS_FORWARDED(p->obj))
            UPDATE_OBJ(p->obj);
         */
        if (match (p->obj, args)) {
            va_end (args);
            REVIVE_OBJ(p->obj);
            return p->obj;
        }
    }
    va_end (args);
    return Null;
}

/* Each of the following functions terminates the objects in two passes.
 * First, they are removed from the global list and added to a temporary
 * list.  In a second pass, this list is scanned to call the terminator
 * functions and actually free the objects.
 *
 * This is to avoid that calling a terminator functions causes the global
 * list to be clobbered recursively resulting in an inconsistent data
 * structure.
 */

/* Terminate all objects belonging to the given group except leaders.
 */
void Terminate_Group (GENERIC group) {
    WEAK_NODE *p, **pp, *q = 0;

    Disable_Interrupts;
    for (pp = &first; (p = *pp); ) {
        if (p->group == group && !(p->flags & WK_LEADER)) {
            if (WAS_FORWARDED(p->obj))
                UPDATE_OBJ(p->obj);
            *pp = p->next;    /* move object to temporary list */
            p->next = q;
            q = p;
        } else pp = &p->next;
    }
    while (q) {    /* scan temporary list, call terminators and free objects */
        WEAK_NODE *tmp = q;
        if (q->term)
            (void)q->term (q->obj);
        q = q->next;
        free ((char *)tmp);
    }
    Enable_Interrupts;
}

/* Terminate all objects of a given type.
 */
void Terminate_Type (int type) {
    WEAK_NODE *p, **pp, *q = 0;

    Disable_Interrupts;
    for (pp = &first; (p = *pp); ) {
        if (TYPE(p->obj) == type) {
            if (WAS_FORWARDED(p->obj))
                UPDATE_OBJ(p->obj);
            *pp = p->next;    /* move object to temporary list */
            p->next = q;
            q = p;
        } else pp = &p->next;
    }
    while (q) {    /* scan temporary list, call terminators and free objects */
        WEAK_NODE *tmp = q;
        if (q->term)
            (void)q->term (q->obj);
        q = q->next;
        free ((char *)tmp);
    }
    Enable_Interrupts;
}

/* The after-GC function.
 */
void Call_Terminators () {
    WEAK_NODE *p, **pp, *q = 0, **qq = &q;

    Disable_Interrupts;
    for (pp = &first; (p = *pp); ) {
        if (IS_ALIVE(p->obj)) {
            if (WAS_FORWARDED(p->obj))
                UPDATE_OBJ(p->obj);
            pp = &p->next;
        } else {
            *pp = p->next;
            if (p->flags & WK_LEADER) {
                *qq = p;    /* move leader to end of temporary list */
                qq = &p->next;
                *qq = 0;
            } else {
                p->next = q;    /* move non-leader to front of list */
                if (qq == &q) qq = &p->next;
                q = p;
            }
        }
    }
    /* Scan the temporary list, call terminators and free objects.
     * As leaders have been appended to the list, they are now
     * scanned after all non-leaders have been taken care of.
     */
    while (q) {
        WEAK_NODE *tmp = q;
        if (q->term)
            (void)q->term (q->obj);
        q = q->next;
        free ((char *)tmp);
    }
    Enable_Interrupts;
}
