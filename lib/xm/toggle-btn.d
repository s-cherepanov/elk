;;; -*-Scheme-*-

(define-widget-type 'togglebutton '("ToggleB.h" "ToggleBG.h"))

(define-widget-class 'toggle-button        'xmToggleButtonWidgetClass)
(define-widget-class 'toggle-button-gadget 'xmToggleButtonGadgetClass)

(define-callback 'toggle-button 'armCallback          #t)
(define-callback 'toggle-button 'disarmCallback       #t)
(define-callback 'toggle-button 'valueChangedCallback #t)

(define-callback 'toggle-button-gadget 'armCallback          #t)
(define-callback 'toggle-button-gadget 'disarmCallback       #t)
(define-callback 'toggle-button-gadget 'valueChangedCallback #t)

;;; Ignore the `set' field in all callback structs (can do a get-values
;;; on the widget passed to the callback function).

(define toggle-button-callback->scheme
"   return Get_Any_CB ((XmAnyCallbackStruct *)x);")

(c->scheme 'callback:toggle-button-valueChangedCallback
            toggle-button-callback->scheme)
(c->scheme 'callback:toggle-button-gadget-valueChangedCallback
	    toggle-button-callback->scheme)
