;;; -*-Scheme-*-

(define-widget-type 'list "List.h")

(prolog
"
static char **Get_List (x) Object x; {
    register i, n;
    register char *s, **l;
    Alloca_Begin;

    Check_List (x);
    n = Fast_Length (x);
    l = (char **)XtMalloc ((n+1) * sizeof (char *));
    for (i = 0; i < n; i++, x = Cdr (x)) {
	Get_Strsym_Stack (Car (x), s);
	l[i] = XtNewString (s);
    }
    l[i] = 0;
    Alloca_End;
    return l;
}")

(define-widget-class 'list 'listWidgetClass)

(define-callback 'list 'callback #t)

(c->scheme 'callback:list-callback
"   XawListReturnStruct *p = (XawListReturnStruct *)x;
    return Cons (Make_String (p->string, strlen (p->string)),
	Make_Integer (p->list_index));")

(scheme->c 'list-list
"   return (XtArgVal)Get_List (x);")

(define-primitive 'list-change! '(w x resize)
"   Check_Widget_Class (w, listWidgetClass);
    Check_Type (resize, T_Boolean);
    XawListChange (WIDGET(w)->widget, Get_List (x), 0, 0, EQ (resize, True));
    return Void;")

(define-primitive 'list-highlight '(w i)
"   Check_Widget_Class (w, listWidgetClass);
    XawListHighlight (WIDGET(w)->widget, Get_Integer (i));
    return Void;")

(define-primitive 'list-unhighlight '(w)
"   Check_Widget_Class (w, listWidgetClass);
    XawListUnhighlight (WIDGET(w)->widget);
    return Void;")

(define-primitive 'list-current '(w)
"   XawListReturnStruct *p;

    Check_Widget_Class (w, listWidgetClass);
    p = XawListShowCurrent (WIDGET(w)->widget);
    if (p->list_index == XAW_LIST_NONE)
	return False;
    return Cons (Make_String (p->string, strlen (p->string)),
	Make_Integer (p->list_index));")
