;;; -*-Scheme-*-

(define-widget-type 'selectionbox "SelectioB.h")

(prolog

"static SYMDESCR Type_Syms[] = {
   { \"dialog-prompt\",       XmDIALOG_PROMPT },
   { \"dialog-selection\",    XmDIALOG_SELECTION },
   { \"dialog-work-area\",    XmDIALOG_WORK_AREA },
   { 0, 0}
};")

(define-widget-class 'selection-box 'xmSelectionBoxWidgetClass)

(define scheme->dialog-type
"   return (XtArgVal)Symbols_To_Bits (x, 0, Type_Syms);")

(scheme->c 'selection-box-dialogType scheme->dialog-type)

(define-callback 'selection-box 'applyCallback   #t)
(define-callback 'selection-box 'cancelCallback  #t)
(define-callback 'selection-box 'noMatchCallback #t)
(define-callback 'selection-box 'okCallback      #t)
(define-callback 'selection-box 'helpCallback    #t)
