;;; -*-Scheme-*-
;;;
;;; List widget demo (directory browser) for Motif

(require 'motif)
(require 'unix)
(require 'sort 'qsort.scm)

(define top (application-initialize 'list))
(set-values! top 'allow-shell-resize #t)

(define form (create-managed-widget (find-class 'form) top))

(define quit (create-managed-widget (find-class 'push-button) form))
(set-values! quit 'left-attachment "ATTACH_FORM"
	          'top-attachment "ATTACH_FORM"
		  'width 50
		  'height 30
		  'border-width 1
		  'label-string "quit")

(add-callback quit 'activate-callback (lambda x (destroy-widget top) 
				       (exit)))

(define back (create-managed-widget (find-class 'push-button) form))
(set-values! back 'left-attachment "ATTACH_WIDGET"
	          'left-widget quit
		  'top-attachment "ATTACH_FORM"
		  'width 50
		  'height 30
		  'border-width 1
		  'label-string "back")

(add-callback back 'activate-callback (lambda x (goto "..")))

(define lab (create-managed-widget (find-class 'label) form))
(set-values! lab 'border-width 0
	         'left-attachment "ATTACH_WIDGET"
		 'left-widget back
		 'top-attachment "ATTACH_FORM"
		 'right-attachment "ATTACH_FORM"
		 'right-offset 4
		 'top-offset 4
		 'height 30
		 'recompute-size #t)

(define lst (create-managed-widget (find-class 'list) form ))
(set-values! lst 'left-attachment "ATTACH_FORM"
	         'top-attachment "ATTACH_WIDGET"
		 'top-widget quit
		 'right-attachment "ATTACH_FORM"
		 'bottom-attachment "ATTACH_FORM"
		 'list-size-policy "VARIABLE"
		 'list-margin-width 5
		 'selection-policy "BROWSE_SELECT")

(add-callback lst 'browse-selection-callback
	      (lambda (w i)
		(let ((type (stat-type (unix-stat (string-append
					  where "/" (car (last-pair i)))))))
		  (set-values! lab 'label-string type)
		  (if (eq? type 'directory)
		      (goto (car (last-pair i)))))))

(define (goto dir)
  (if (string=? dir "..")
      (begin
	(if (not (string=? where "/"))
	    (begin
              (set! where
		    (substring where 0
			       (do ((i (- (string-length where) 2) (1- i)))
				   ((char=? (string-ref where i) #\/) i))))
              (if (eqv? where "")
	          (set! where "/")))))
      (if (not (or (string=? dir "/") (string=? where "/")))
	  (set! where (string-append where "/")))
      (set! where (string-append where dir)))
  (set-values! lab 'label-string where)
  (define l '())
  (for-each (lambda (d) (if (not (member d '("." "..")))
			    (set! l (cons d l))))
	    (unix-read-directory where))
  (if (null? l)
      (set-values! lst 'items l 'item-count 0 'visible-item-count 1)
      (set-values! lst 'items (sort l string<?) 'item-count (length l)
		       'visible-item-count (length l))))

(define where "")

(goto "/")

(set-values! lab 'label-string "Select directory:")

(realize-widget top)
(context-main-loop (widget-context top))
