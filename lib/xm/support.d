;;; -*-Scheme-*-
;;;
;;; Used as container for random stuff

(define-widget-type 'support "")   ; No include file

(prolog

"SYMDESCR Reason_Syms[] = {
    { \"none\",                   XmCR_NONE },
    { \"help\",                   XmCR_HELP },
    { \"value-changed\",          XmCR_VALUE_CHANGED },
    { \"increment\",              XmCR_INCREMENT },
    { \"decrement\",              XmCR_DECREMENT },
    { \"page-increment\",         XmCR_PAGE_INCREMENT },
    { \"page-decrement\",         XmCR_PAGE_DECREMENT },
    { \"to-top\",                 XmCR_TO_TOP },
    { \"to-bottom\",              XmCR_TO_BOTTOM },
    { \"drag\",                   XmCR_DRAG },
    { \"activate\",               XmCR_ACTIVATE },
    { \"arm\",                    XmCR_ARM },
    { \"disarm\",                 XmCR_DISARM },
    { \"map\",                    XmCR_MAP },
    { \"unmap\",                  XmCR_UNMAP },
    { \"focus\",                  XmCR_FOCUS },
    { \"losing-focus\",           XmCR_LOSING_FOCUS },
    { \"modifying-text-value\",   XmCR_MODIFYING_TEXT_VALUE },")

(prolog
"   { \"moving-insert-cursor\",   XmCR_MOVING_INSERT_CURSOR },
    { \"execute\",                XmCR_EXECUTE },
    { \"single-select\",          XmCR_SINGLE_SELECT },
    { \"multiple-select\",        XmCR_MULTIPLE_SELECT },
    { \"extended-select\",        XmCR_EXTENDED_SELECT },
    { \"browse-select\",          XmCR_BROWSE_SELECT },
    { \"default-action\",         XmCR_DEFAULT_ACTION },
    { \"clipboard-data-request\", XmCR_CLIPBOARD_DATA_REQUEST },
    { \"clipboard-data-delete\",  XmCR_CLIPBOARD_DATA_DELETE },
    { \"cascading\",              XmCR_CASCADING },
    { \"ok\",                     XmCR_OK },
    { \"cancel\",                 XmCR_CANCEL },
    { \"apply\",                  XmCR_APPLY },
    { \"no-match\",               XmCR_NO_MATCH },
    { \"command-entered\",        XmCR_COMMAND_ENTERED },
    { \"command-changed\",        XmCR_COMMAND_CHANGED },
    { \"expose\",                 XmCR_EXPOSE },
    { \"resize\",                 XmCR_RESIZE },
    { \"input\",                  XmCR_INPUT },
    { 0, 0 }
};")

(prolog

"Object Get_Any_CB (p) XmAnyCallbackStruct *p; {
    Object args, ret;
    GC_Node2;

    args = ret = Null;
    GC_Link2 (ret, args);
    if (p->event) {
        args = Get_Event_Args (p->event);
        ret = Copy_List (args);
        Destroy_Event_Args (args);
    }
    ret = Cons (Bits_To_Symbols ((unsigned long)p->reason, 0, Reason_Syms),
	      ret);
    GC_Unlink;
    return ret;
}")

(prolog

"Object Get_Selection_CB (p) XmSelectionBoxCallbackStruct *p; {
    Object ret, s;
    char *text;
    GC_Node2;

    if (!XmStringGetLtoR (p->value, XmSTRING_DEFAULT_CHARSET, &text))
	text = \"\";
    ret = s = Make_String (text, strlen (text));
    GC_Link2 (ret, s);
    ret = Cons (ret, Null);
    s = Get_Any_CB ((XmAnyCallbackStruct *)p);
    ret = Cons (Cdr (s), ret);
    ret = Cons (Car (s), ret);
    GC_Unlink;
    return ret;
}")

(prolog

"static XtArgVal Scheme_To_String_Table (x) Object x; {
    Object t;
    char *s;
    XmString *tab;
    int i = 0;
    Alloca_Begin;

    tab = (XmString *)XtMalloc (Get_Integer (P_Length (x))
	* sizeof (XmString));
    /* tab is never freed since the converter must return a new address
     * each time it is called.
     */
    for (t = x; TYPE(t) == T_Pair; t = Cdr (t)) {
	Get_Strsym_Stack (Car (t), s);
	tab[i++] = XmStringCreate (s, XmSTRING_DEFAULT_CHARSET);
    }
    Alloca_End;
    return (XtArgVal)tab;
}")


(define-primitive 'update-display '(w)
"   Check_Widget (w);
    XmUpdateDisplay (WIDGET(w)->widget);
    return Void;")


;;; Converters

(define keysym->scheme
"   return Make_Char ((int)x);")

(define scheme->keysym
"   Check_Type (x, T_Character); return (XtArgVal)CHAR(x);")

(define position->scheme
"   return Make_Integer (*(Position *)&x);")

(define scheme->position
"   return (XtArgVal)Get_Integer (x);")

(define dimension->scheme
"   return Make_Integer (*(Dimension *)&x);")

(define scheme->dimension
"   return (XtArgVal)Get_Unsigned (x);")

(define int->scheme
"   return Make_Integer (*(int *)&x);")

(define scheme->int
"   return (XtArgVal)Get_Integer (x);")

(define window->scheme
"   return Make_Widget_Foreign ((Widget)x);")

(define scheme->window
"   Check_Widget (x); return (XtArgVal)WIDGET(x)->widget;")

(define scheme->scrollbar
"   extern WidgetClass xmScrollBarWidgetClass;
    Check_Widget_Class (x, xmScrollBarWidgetClass);
    return (XtArgVal)WIDGET(x)->widget;")

(define selection-callback->scheme
"   return Get_Selection_CB ((XmSelectionBoxCallbackStruct *)x);")

(define help-callback->scheme
"   return Get_Any_CB ((XmAnyCallbackStruct *)x);")

(define button-callback->scheme
"   return Get_Any_CB ((XmAnyCallbackStruct *)x);")

(define event-callback->scheme
"   return Get_Any_CB ((XmAnyCallbackStruct *)x);")

(define xm-string->scheme
"   char *text;
    if (!XmStringGetLtoR ((XmString)x, XmSTRING_DEFAULT_CHARSET, &text))
	text = \"\";
    return Make_String (text, strlen (text));")

(define scheme->xm-string
"   char *s;
    XtArgVal ret;
    Alloca_Begin;
    Get_Strsym_Stack (x, s);
    ret = (XtArgVal)XmStringCreateLtoR (s, XmSTRING_DEFAULT_CHARSET);
    Alloca_End;
    return ret;")

(define scheme->xm-string-table
"   return Scheme_To_String_Table (x);")

(c->scheme 'KeySym              keysym->scheme)
(scheme->c 'KeySym              scheme->keysym)

(c->scheme 'HorizontalPosition  position->scheme)
(c->scheme 'VerticalPosition    position->scheme)
(c->scheme 'HorizontalDimension dimension->scheme)
(c->scheme 'VerticalDimension   dimension->scheme)
(c->scheme 'HorizontalInt       int->scheme)    ; Sigh.  Why don't they just
(c->scheme 'VerticalInt         int->scheme)    ; use plain old Int??
(scheme->c 'HorizontalPosition  scheme->position)
(scheme->c 'VerticalPosition    scheme->position)
(scheme->c 'HorizontalDimension scheme->dimension)
(scheme->c 'VerticalDimension   scheme->dimension)
(scheme->c 'HorizontalInt       scheme->int)
(scheme->c 'VerticalInt         scheme->int)

(c->scheme 'ShellHorizPos       position->scheme)
(c->scheme 'ShellVertPos        position->scheme)
(c->scheme 'ShellHorizDim       dimension->scheme)
(c->scheme 'ShellVertDim        dimension->scheme)
(scheme->c 'ShellHorizPos       scheme->position)
(scheme->c 'ShellVertPos        scheme->position)
(scheme->c 'ShellHorizDim       scheme->dimension)
(scheme->c 'ShellVertDim        scheme->dimension)

(c->scheme 'horizontalScrollBar window->scheme)  ; Some classes have resources
(c->scheme 'verticalScrollBar   window->scheme)  ; of type window instead of
(c->scheme 'workWindow          window->scheme)  ; widget.  What a crock!
(c->scheme 'commandWindow       window->scheme)
(c->scheme 'menuBar             window->scheme)
(c->scheme 'subMenuId           window->scheme)
(c->scheme 'menuHistory         window->scheme)
(c->scheme 'menuHelpWidget      window->scheme)
(c->scheme 'bottomWidget        window->scheme)
(c->scheme 'leftWidget          window->scheme)
(c->scheme 'rightWidget         window->scheme)
(c->scheme 'topWidget           window->scheme)

(scheme->c 'horizontalScrollBar scheme->scrollbar)
(scheme->c 'verticalScrollBar   scheme->scrollbar)
(scheme->c 'workWindow          scheme->window)
(scheme->c 'commandWindow       scheme->window)
(scheme->c 'menuBar             scheme->window)
(scheme->c 'subMenuId           scheme->window)
(scheme->c 'menuHistory         scheme->window)
(scheme->c 'menuHelpWidget      scheme->window)
(scheme->c 'bottomWidget        scheme->window)
(scheme->c 'leftWidget          scheme->window)
(scheme->c 'rightWidget         scheme->window)
(scheme->c 'topWidget           scheme->window)

(c->scheme 'callback:applyCallback       selection-callback->scheme)
(c->scheme 'callback:cancelCallback      selection-callback->scheme)
(c->scheme 'callback:noMatchCallback     selection-callback->scheme)
(c->scheme 'callback:okCallback          selection-callback->scheme)

(c->scheme 'callback:helpCallback        help-callback->scheme)

(c->scheme 'callback:activateCallback    button-callback->scheme)
(c->scheme 'callback:armCallback         button-callback->scheme)
(c->scheme 'callback:disarmCallback      button-callback->scheme)
(c->scheme 'callback:cascadingCallback   button-callback->scheme)

(c->scheme 'callback:exposeCallback      event-callback->scheme)
(c->scheme 'callback:inputCallback       event-callback->scheme)
(c->scheme 'callback:resizeCallback      event-callback->scheme)

(c->scheme 'XmString            xm-string->scheme)
(scheme->c 'XmString            scheme->xm-string)
(scheme->c 'XmStringTable       scheme->xm-string-table)

;;; Classes for which no .d-file exists:

(define-widget-class 'primitive 'xmPrimitiveWidgetClass)

(define-widget-class 'manager 'xmManagerWidgetClass)
