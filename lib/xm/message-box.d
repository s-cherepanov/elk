;;; -*-Scheme-*-

(define-widget-type 'messagebox "MessageB.h")

(prolog

"static SYMDESCR Type_Syms[] = {
   { \"dialog-error\",        XmDIALOG_ERROR },
   { \"dialog-information\",  XmDIALOG_INFORMATION },
   { \"dialog-message\",      XmDIALOG_MESSAGE },
   { \"dialog-question\",     XmDIALOG_QUESTION },
   { \"dialog-warning\",      XmDIALOG_WARNING },
   { \"dialog-working\",      XmDIALOG_WORKING },
   { 0, 0}
};")

(define-widget-class 'message-box 'xmMessageBoxWidgetClass)

(define-callback 'message-box 'cancelCallback   #t)
(define-callback 'message-box 'okCallback       #t)

(define scheme->dialog-type
"   return (XtArgVal)Symbols_To_Bits (x, 0, Type_Syms);")

(define message-box-callback->scheme
"   return Get_Any_CB ((XmAnyCallbackStruct *)x);")

(scheme->c 'message-box-dialogType      scheme->dialog-type)

(c->scheme 'callback:message-box-cancelCallback  message-box-callback->scheme)
(c->scheme 'callback:message-box-okCallback      message-box-callback->scheme)
