;;; -*-Scheme-*-
;;;
;;; Pulldown menu demo

(require 'motif)
(load 'menu-stuff.scm)

(define top (application-initialize 'pulldown))
 
(define menu-bar (create-menu-bar top))

;;; Create pulldown menu pane with 3 push buttons and a sub-menu

(define menu-1 (create-pulldown-menu menu-bar))

(menu-add-button! menu-1 'label-string "item 1")
(menu-add-button! menu-1 'label-string "item 2")
(menu-add-button! menu-1 'label-string "item 3")
(menu-add-separator! menu-1)

(create-cascade-pulldown menu-bar menu-1 'mnemonic #\m 'label-string "menu-1")

;;; Create the sub-menu:

(define sub-menu (create-pulldown-menu menu-1))

(menu-add-label! sub-menu 'label-string "sub-menu")
(menu-add-separator! sub-menu)
(menu-add-button! sub-menu 'label-string "item 1")
(menu-add-button! sub-menu 'label-string "item 2")
(menu-add-button! sub-menu 'label-string "item 3")

(create-cascade-pulldown menu-1 sub-menu 'label-string "sub-menu")

;;; Create second pulldown menu width a quit button)

(define menu-2 (create-pulldown-menu menu-bar))

(menu-add-button! menu-2 'label-string "item 1")
(menu-add-button! menu-2 'label-string "item 2")
(menu-add-button! menu-2 'label-string "item 3" 'sensitive #f)
(menu-add-button! menu-2 'label-string "item 4")
(menu-add-button! menu-2 'label-string "quit" 'mnemonic #\q
  'activate-callback (list (lambda args (print args) (exit))))

(create-cascade-pulldown menu-bar menu-2 'label-string "menu-2")

(realize-widget top)
(context-main-loop (widget-context top))
