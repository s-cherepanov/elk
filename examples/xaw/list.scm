;;; -*-Scheme-*-
;;;
;;; List widget demo (directory browser)

(require 'xwidgets)
(load-widgets shell form label command list)
(require 'unix)
(require 'sort 'qsort.scm)

(define top (application-initialize 'list))
(set-values! top 'allow-shell-resize #t)

(define form (create-managed-widget (find-class 'form) top))

(define quit (create-managed-widget (find-class 'command) form))
(set-values! quit 'label "quit")
(add-callback quit 'callback (lambda x (exit)))

(define back (create-managed-widget (find-class 'command) form))
(set-values! back 'label "back" 'from-horiz quit)
(add-callback back 'callback (lambda x (goto "..")))

(define lab (create-managed-widget (find-class 'label) form))
(set-values! lab 'border-width 0 'from-horiz back 'resizable #t)

;; List widget is broken; ``list'' resource *must* be initialized:
(define lst (create-managed-widget (find-class 'list) form 'list '()))
(set-values! lst 'from-vert lab 'resizable #t 'vertical-list #t)

(add-callback lst 'callback
  (lambda (w i)
    (let ((type (stat-type (unix-stat (string-append where "/" (car i))))))
      (set-values! lab 'label type)
      (if (eq? type 'directory)
	  (goto (car i))))))

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
  (set-values! lab 'label where)
  (define l '())
  (for-each (lambda (d) (if (not (member d '("." "..")))
			    (set! l (cons d l))))
	    (unix-read-directory where))
  (set-values! lst 'default-columns
    (max 2 (ceiling (/ (length l) 40))))
  (list-change! lst (sort l string<?) #t))

(define where "")
(goto "/")
(set-values! lab 'label "Select directory:")
(realize-widget top)
(context-main-loop (widget-context top))
