;;; -*-Scheme-*-
;;;
;;; The Scheme part of the X11 widget interface.

; kludge
(define site-lib-xaw "")
(define site-lib-xmotif "")

(require 'xt)

(define widget-subdirectory 'xaw)

(define load-always '())

(define widget-aliases #f)

(define (widget-loaded? w)
  ;;(feature? (string->symbol (format #f "~a:~a.so" widget-subdirectory w))))
  (feature? (string->symbol (format #f "~a:~a.so" widget-subdirectory widget-subdirectory))))

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
	  (if autoload-notify? (format #t "[Loading "))
	  (do ((f l (cdr f))) ((null? f))
	    (let* ((file (car f))
		   (alias (assq (car f) widget-aliases)))
	      (if alias (set! file (cdr alias)))
	      (if autoload-notify?
		  (format #t "~a~a" file (if (null? (cdr f)) "" " ")))
              ;; XXX: don't load all widgets, they're all in the same lib
	      ;;(set! wl (cons (format #f "~a/~a.so" widget-subdirectory file)
		;;	     wl))))
	      (set! wl (list (format #f "~a/~a.so" widget-subdirectory widget-subdirectory)))))
	  (if autoload-notify? (format #t "]~%"))
	  `(fluid-let ((load-libraries
			 (if (feature? 'motif)
			     (string-append site-lib-xmotif " " load-libraries)
			     (string-append site-lib-xaw " " load-libraries))))
	     (load ',wl)))
	#f)))

(define load-widget load-widgets)

(provide 'xwidgets)
