;;; -*-Scheme-*-
;;;
;;; Pulldown menu demo

(require 'xaw)

(define top (application-initialize 'pulldown))

(define mb (create-managed-widget (find-class 'menubutton) top))
(set-values! mb 'label "Please press left button" 'menu-name 'the-menu)

;; Due to a bug in the X11R5 SimpleMenu widget the `label' resource
;; can only be set at widget creation time:
;;
(define menu (create-popup-shell 'the-menu (find-class 'simplemenu) mb
  'label 'menu))

(define data "\0\0\0\6\0\3\0\3\200\1\206\1\316\0\314\0\170\0\160\0\40\0\0\0")
(define bm (create-bitmap-from-data (display-root-window (widget-display top))
				    data 12 12))

(define (selected w)
  (format #t "~s selected~%" (widget-name w)))

(for-each
  (lambda (e)
    (if e
        (if (eqv? e "")
	    (create-managed-widget (find-class 'sme) menu 'height 10)
            (create-managed-widget e (find-class 'smebsb) menu
              'vert-space 40 'label e 'callback (list selected)))
        (create-managed-widget (find-class 'smeline) menu)))
  '("hamburger" "fishburger" "pommes frites" "" "chicken nuggets"
    "chicken wings" #f "cola" "milk shake" #f))

(define w (create-managed-widget (find-class 'smebsb) menu))
(set-values! w 'vert-space 50 'left-bitmap bm 'label "eat here"
               'left-margin 16)
(add-callback w 'callback
  (lambda (w)
    (set-values! w 'left-bitmap
      (if (eq? (car (get-values w 'left-bitmap)) 'none) bm 'none))))
	
(realize-widget top)
(context-main-loop (widget-context top))
