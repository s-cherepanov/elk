;;; -*-Scheme-*-

(define-widget-type 'command "Command.h")

(define-widget-class 'command 'xmCommandWidgetClass)

(define-callback 'command 'commandChangedCallback #t)
(define-callback 'command 'commandEnteredCallback #t)

(define command-callback->scheme
"   return Get_Selection_CB ((XmCommandCallbackStruct *)x);")

(c->scheme 'callback:command-commandChangedCallback   command-callback->scheme)
(c->scheme 'callback:command-commandEnteredCallback   command-callback->scheme)
