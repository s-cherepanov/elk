;;; -*-Scheme-*-
;;;
;;; Popup menu demo

(require 'xwidgets)
(load-widgets shell simplemenu smebsb label)

(define top (application-initialize 'popup))

(define l (create-managed-widget (find-class 'label) top))
(set-values! l 'label "Press left button")
(augment-translations l
  "<Btn1Down>: XawPositionSimpleMenu(the-menu) MenuPopup(the-menu)")

;; Due to a bug in the X11R5 SimpleMenu widget the `label' resource
;; can only be set at widget creation time:
;;
(define menu (create-popup-shell 'the-menu (find-class 'simplemenu) l
  'label 'menu))

(define (selected _)
  (print (widget-name (simplemenu-get-active-entry menu))))

(define entries (map
  (lambda (e)
    (create-managed-widget e (find-class 'smebsb) menu 'vert-space 40
                           'label e 'callback (list selected)))
  '("hamburger" "fishburger" "pommes frites" "chicken nuggets" "chicken wings"
    "milk shake")))

; (set-values! menu 'popup-on-entry (cadr entries) 'label 'menu
;   'menu-on-screen #t)

(realize-widget top)
(context-main-loop (widget-context top))
