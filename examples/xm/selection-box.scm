;;; -*-Scheme-*-
;;;
;;; Selection box demo

(require 'motif)
(load-widgets shell selection-box)

(define top (application-initialize 'selection))

(define sb (create-managed-widget (find-class 'selection-box) top))

(define items
  '(montana washington florida california texas new\ york alaska maryland
  idaho virginia maine oregon illinois new\ jersey missouri louisiana))

(set-values! sb 'list-items items 'list-item-count (length items))
(set-values! sb 'list-visible-item-count 6)

(set-values! sb 'list-label-string "Available items:" 'must-match #t)
(set-values! sb 'label-font-list "8x13" 'button-font-list "9x15")

(for-each
  (lambda (c)
    (add-callback sb c
      (lambda r
	(case (caadr r)
	(no-match
	  (display #\007))
	(help
	  (display "No help available!") (newline)))
	(print r))))
  '(apply-callback cancel-callback no-match-callback
    ok-callback help-callback))

(realize-widget top)
(context-main-loop (widget-context top))
