;;; -*-Scheme-*-
;;;
;;; The Scheme part of the X11 widget interface.

(require 'xt)

(define widget-subdirectory 'xaw)

(define load-always '())

(define widget-aliases #f)

(define (widget-loaded? w)
  (feature? (string->symbol (format #f "~a:~a.o" widget-subdirectory w))))

(define-macro (load-widgets . w)
  (let ((wl '()) (l '()))
    (if (null? w)
	(error 'load-widgets "no arguments"))
    (for-each
     (lambda (w)
       (if (not (symbol? w))
	   (error 'load-widgets "argument not a symbol"))
       (if (not (widget-loaded? w))
	   (set! l (cons w l))))
     w)
    (for-each
     (lambda (w)
       (if (not (widget-loaded? w))
	   (set! l (cons w l))))
     load-always)
    (if (not (null? l))
	(begin
	  (if (not widget-aliases)
	      (load (format #f "~a/ALIASES" widget-subdirectory)))
	  (format #t "[Loading ")
	  (do ((f l (cdr f))) ((null? f))
	    (let* ((file (car f))
		   (alias (assq (car f) widget-aliases)))
	      (if alias (set! file (cdr alias)))
	      (format #t "~a~a" file (if (null? (cdr f)) "" " "))
	      (set! wl (cons (format #f "~a/~a.o" widget-subdirectory file)
			     wl))))
	  (format #t "]~%")
	  `(fluid-let ((load-libraries
			 (if (feature? 'motif)
			     (string-append site-lib-xmotif " " load-libraries)
			     (string-append site-lib-xaw " " load-libraries))))
	     (load ',wl)))
	#f)))

(define load-widget load-widgets)

(provide 'xwidgets)
