;;; -*-Scheme-*-
;;;
;;; Popup menu demo

(require 'motif)
(load-widgets shell row-column cascade-button push-button label separator
              drawing-area)
(load 'menu-stuff.scm)

(define top (application-initialize 'popup))
 
(define w (create-managed-widget (find-class 'drawing-area) top))
(set-values! w 'width 350 'height 100)

(define menu (create-popup-menu w 'which-button 1))

(menu-add-label! menu 'label-string "Popup menu" 'font-list "9x15")
(menu-add-separator! menu)
(menu-add-button! menu 'label-string "item 1")
(menu-add-button! menu 'label-string "item 2")
(menu-add-button! menu 'label-string "item 3")
(menu-add-separator! menu)
(define quit-button (menu-add-button! menu 'label-string "quit"))

(add-callback quit-button 'activate-callback (lambda args (exit)))

(popup-menu-attach-to! menu w)

(realize-widget top)
(context-main-loop (widget-context top))
