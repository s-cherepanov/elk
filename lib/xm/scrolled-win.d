;;; -*-Scheme-*-

(define-widget-type 'scrolledwindow "ScrolledW.h")

(prolog

;;; Before the converter for scrollingPolicy was introduced (which wasn't
;;; even necessary), the one provided by Xm was called and people were using
;;; "AUTOMATIC" and "APPLICATION_DEFINED".  Everything was fine.
;;;
;;; After the converter was introduced, code was required to use 'automatic
;;; and 'application-defined instead.  Thus the change broke existing code.
;;;
;;; As a temporary solution, I'm now adding AUTOMATIC etc. to the list of
;;; legal values, but clearly some kind of concept is needed here...

"static SYMDESCR Scrolling_Syms[] = {
   { \"automatic\",           XmAUTOMATIC },
   { \"application-defined\", XmAPPLICATION_DEFINED },
   { \"AUTOMATIC\",           XmAUTOMATIC },            /* see above */
   { \"APPLICATION_DEFINED\", XmAPPLICATION_DEFINED },
   { \"application_defined\", XmAPPLICATION_DEFINED },
   { 0, 0}
};")

(define-widget-class 'scrolled-window 'xmScrolledWindowWidgetClass)

(scheme->c 'scrollingPolicy
"   if (TYPE(x) == T_String) x = P_String_To_Symbol(x);
    return (XtArgVal)Symbols_To_Bits (x, 0, Scrolling_Syms);")
