;;; -*-Scheme-*-
;;;
;;; Option menu demo

(require 'motif)
(load 'menu-stuff.scm)

(define top (application-initialize 'option))
 
(define rc (create-managed-widget (find-class 'row-column) top))
(set-values! rc 'orientation "horizontal")

(define menu-1 (create-pulldown-menu rc))

(define b1 (menu-add-button! menu-1 'label-string "Option 1"))
(define b2 (menu-add-button! menu-1 'label-string "Option 2"))
(define b3 (menu-add-button! menu-1 'label-string "Option 3"))
(define b4 (menu-add-button! menu-1 'label-string "Option 4"))

(define menu-2 (create-pulldown-menu rc))

(define ba (menu-add-button! menu-2 'label-string "Option A"))
(define bb (menu-add-button! menu-2 'label-string "Option B"))
(define bc (menu-add-button! menu-2 'label-string "Option C"))

(create-option-menu rc 'sub-menu-id menu-1 'menu-history b3
		    'label-string "first option" 'mnemonic #\f)

(create-option-menu rc 'sub-menu-id menu-2 'menu-history ba
		    'label-string "second option" 'mnemonic #\s)

(realize-widget top)
(context-main-loop (widget-context top))
