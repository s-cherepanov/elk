;;; -*-Scheme-*-

(define-widget-type 'text "Text.h")

(define-widget-class 'text 'xmTextWidgetClass
  '(pendingDelete PendingDelete Boolean)
  '(selectThreshold SelectThreshold Int)
  '(blinkRate BlinkRate Int)
  '(columns Columns Short)
  '(cursorPositionVisible CursorPositionVisible Boolean)
  '(fontList FontList FontList)
  '(resizeHeight ResizeHeight Boolean)
  '(resizeWidth ResizeWidth Boolean)
  '(rows Rows Short)
  '(wordWrap WordWrap Boolean))

(define-callback 'text 'activateCallback     #t)
(define-callback 'text 'valueChangedCallback #t)

(define text-callback->scheme
"   return Get_Any_CB ((XmAnyCallbackStruct *)x);")

(c->scheme 'callback:text-valueChangedCallback text-callback->scheme)
