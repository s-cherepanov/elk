;;; -*-Scheme-*-
;;;
;;; Trivial text widget demo (the text widget isn't fully supported
;;; by Elk)

(require 'xwidgets)
(load-widgets shell ascii box command label)

(define top (application-initialize 'text))

(define box (create-managed-widget (find-class 'box) top))

(define lab (create-managed-widget (find-class 'label) box))
(set-values! lab 'border-width 0 'label "Enter a number:")

(define txt (create-managed-widget (find-class 'ascii-text) box))
(set-values! txt 'edit-type 'edit 'resize 'width)

(define can (create-managed-widget (find-class 'command) box))
(set-values! can 'label "CANCEL")
(add-callback can 'callback (lambda foo (exit)))

(define acc (create-managed-widget (find-class 'command) box))
(set-values! acc 'label "ACCEPT")
(add-callback acc 'callback
	      (lambda foo
		(let ((s (ascii-text-string txt)))
		  (if (not (number-string? s))
		      (format #t "~s is not a number!~%" s)
		      (format #t "Result is ~a~%" s)
		      (exit)))))
      
(define (number-string? s)
  (not (or (eqv? s "") (memq #f (map char-numeric? (string->list s))))))

(realize-widget top)
(context-main-loop (widget-context top))
