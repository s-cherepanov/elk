;;; -*-Scheme-*-

(define-widget-type 'rowcolumn "RowColumn.h")

(prolog

"static SYMDESCR Type_Syms[] = {
   { \"work-area\",        XmWORK_AREA },
   { \"menu-bar\",         XmMENU_BAR },
   { \"menu-pulldown\",    XmMENU_PULLDOWN },
   { \"menu-popup\",       XmMENU_POPUP },
   { \"menu-option\",      XmMENU_OPTION },
   { 0, 0}
};")

(define-widget-class 'row-column 'xmRowColumnWidgetClass)

(prolog

"static void Post_Handler (w, client_data, event, unused) Widget w;
	XtPointer client_data; XEvent *event; Boolean *unused; {
    unsigned int b;
    Arg a;
    XButtonPressedEvent *ep = (XButtonPressedEvent *)event;
    Widget popup = (Widget)client_data;

    XtSetArg (a, XmNwhichButton, &b);
    XtGetValues (popup, &a, 1);
    if (ep->button != b)
	return;
    XmMenuPosition (popup, ep);
    XtManageChild (popup);
}")

(prolog

"static Object Get_Row_Column_CB (p) XmRowColumnCallbackStruct *p; {
    Object ret, s;
    GC_Node2;

    ret = s = Make_Widget_Foreign (p->widget);
    GC_Link2 (ret, s);
    ret = Cons (ret, Null);
    s = Get_Any_CB ((XmAnyCallbackStruct *)p);
    ret = Cons (Cdr (s), ret);
    ret = Cons (Car (s), ret);
    GC_Unlink;
    return ret;
}")

(define-primitive 'popup-menu-attach-to! '(m w)
"   XtPointer client_data;
    Arg a;
    Check_Widget_Class (m, xmRowColumnWidgetClass);
    Check_Widget (w);
    XtSetArg (a, XmNuserData, &client_data);
    XtGetValues (WIDGET(w)->widget, &a, 1);
    if (client_data)
	XtRemoveEventHandler (WIDGET(w)->widget, ButtonPressMask, 0,
	    Post_Handler, client_data);
    client_data = (XtPointer)WIDGET(m)->widget;
    XtAddEventHandler (WIDGET(w)->widget, ButtonPressMask, 0,
	Post_Handler, client_data);
    client_data = (XtPointer)WIDGET(m)->widget;
    XtSetValues (WIDGET(w)->widget, &a, 1);
    return Void;")

(define-callback 'row-column 'entryCallback #t)

(define row-column-callback->scheme
"   return Get_Row_Column_CB ((XmRowColumnCallbackStruct *)x);")

(c->scheme 'callback:row-column-entryCallback row-column-callback->scheme)

(define scheme->row-column-type
"   return (XtArgVal)Symbols_To_Bits (x, 0, Type_Syms);")

;;; whichButton resource is declared with a type of XtRWhichButton
;;; instead of XtRUnsignedInt.  Argh!

(define scheme->which-button
"   return (XtArgVal)Get_Integer (x);")

(define which-button->scheme
"   return Make_Integer (x);")

;;; entryClass is declared as int!  Bletch!

(define scheme->entry-class
"   Check_Type (x, T_Class); return (XtArgVal)CLASS(x)->wclass;")

(define entry-class->scheme
"   return Make_Widget_Class ((WidgetClass)x);")

(scheme->c 'row-column-rowColumnType      scheme->row-column-type)

(scheme->c 'row-column-whichButton        scheme->which-button)
(c->scheme 'row-column-whichButton        which-button->scheme)

(scheme->c 'row-column-entryClass         scheme->entry-class)
(c->scheme 'row-column-entryClass         entry-class->scheme)
