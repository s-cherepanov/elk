;; list.d
;;
;; $Id$
;;
;; Copyright 1990, 1991, 1992, 1993, 1994, 1995, Oliver Laumann, Berlin
;; Copyright 2002, 2003 Sam Hocevar <sam@zoy.org>, Paris
;;
;; This software was derived from Elk 1.2, which was Copyright 1987, 1988,
;; 1989, Nixdorf Computer AG and TELES GmbH, Berlin (Elk 1.2 has been written
;; by Oliver Laumann for TELES Telematic Services, Berlin, in a joint project
;; between TELES and Nixdorf Microprocessor Engineering, Berlin).
;;
;; Oliver Laumann, TELES GmbH, Nixdorf Computer AG and Sam Hocevar, as co-
;; owners or individual owners of copyright in this software, grant to any
;; person or company a worldwide, royalty free, license to
;;
;;    i) copy this software,
;;   ii) prepare derivative works based on this software,
;;  iii) distribute copies of this software or derivative works,
;;   iv) perform this software, or
;;    v) display this software,
;;
;; provided that this notice is not removed and that neither Oliver Laumann
;; nor Teles nor Nixdorf are deemed to have made any representations as to
;; the suitability of this software for any purpose nor are held responsible
;; for any defects of this software.
;;
;; THERE IS ABSOLUTELY NO WARRANTY FOR THIS SOFTWARE.

(define-widget-type 'list "List.h")

(define-widget-class 'list 'xmListWidgetClass)

(prolog

"static Object String_Table_To_Scheme (tab, len) XmString *tab; {
    Object ret, tail;
    char *text;
    GC_Node2;

    tail = ret = P_Make_List (Make_Integer (len), Null);
    GC_Link2 (ret, tail);
    for ( ; len > 0; len--, tail = Cdr (tail)) {
	if (!XmStringGetLtoR (*tab++, XmSTRING_DEFAULT_CHARSET, &text))
	    text = \"\";
	Car (tail) = Make_String (text, strlen (text));
    }
    GC_Unlink;
    return ret;
}")

(prolog

"static SYMDESCR Type_Syms[] = {
   { \"initial\",      XmINITIAL },
   { \"modification\", XmMODIFICATION },
   { \"addition\",     XmADDITION },
   { 0, 0}
};")

(prolog

"static Object Get_List_CB (p) XmListCallbackStruct *p; {
    Object ret, s;
    char *text;
    GC_Node2;

    if (!XmStringGetLtoR (p->item, XmSTRING_DEFAULT_CHARSET, &text))
	text = \"\";
    ret = s = Make_String (text, strlen (text));
    GC_Link2 (ret, s);
    ret = Cons (ret, Null);
    if (p->reason == XmCR_MULTIPLE_SELECT
	    || p->reason == XmCR_EXTENDED_SELECT) {
	s = String_Table_To_Scheme (p->selected_items, p->selected_item_count);
	ret = Cons (s, ret);
	s = Bits_To_Symbols ((unsigned long)p->selection_type, 0, Type_Syms);
	ret = Cons (s, ret);
    } else {
	ret = Cons (Make_Integer (p->item_position), ret);
    }
    s = Get_Any_CB ((XmAnyCallbackStruct *)p);
    ret = Cons (Cdr (s), ret);
    ret = Cons (Car (s), ret);
    GC_Unlink;
    return ret;
}")

(define-callback 'list 'browseSelectionCallback   #t)
(define-callback 'list 'defaultActionCallback     #t)
(define-callback 'list 'extendedSelectionCallback #t)
(define-callback 'list 'multipleSelectionCallback #t)
(define-callback 'list 'singleSelectionCallback   #t)

(define list-callback->scheme
"   return Get_List_CB ((XmListCallbackStruct *)x);")

(c->scheme 'callback:list-browseSelectionCallback   list-callback->scheme)
(c->scheme 'callback:list-defaultActionCallback     list-callback->scheme)
(c->scheme 'callback:list-extendedSelectionCallback list-callback->scheme)
(c->scheme 'callback:list-multipleSelectionCallback list-callback->scheme)
(c->scheme 'callback:list-singleSelectionCallback   list-callback->scheme)
