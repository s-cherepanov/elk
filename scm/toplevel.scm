;;; -*-Scheme-*-
;;;
;;; Read-eval-print loop and error handler


(autoload 'pp 'pp.scm)
(autoload 'apropos 'apropos.scm)
(autoload 'sort 'qsort.scm)
(autoload 'describe 'describe.scm)
(autoload 'backtrace 'debug.scm)
(autoload 'inspect 'debug.scm)

(define ?)
(define ??)
(define ???)
(define !)
(define !!)
(define !!!)
(define &)

(define (rep-loop env)
  (define input)
  (define value)
  (let loop ()
    (set! ??? ??)
    (set! ?? ?)
    (set! ? &)
    ;;; X Windows hack
    (if (and (bound? 'display-flush-output) (bound? 'dpy) (display? dpy))
	(display-flush-output dpy))
    (if (> rep-level 0)
	(display rep-level))
    (display "> ")
    (set! input (read))
    (set! & input)
    (if (not (eof-object? input))
	(begin
	  (set! value (eval input env))
	  (set! !!! !!)
	  (set! !! !)
	  (set! ! value)
	  (write value)
	  (newline)
	  (loop)))))

(define rep-frames)
(define rep-level)

(set! interrupt-handler
  (lambda ()
    (format #t "~%\7Interrupt!~%")
    (let ((next-frame (car rep-frames)))
      (next-frame #t))))

(define-macro (push-frame control-point)
  `(begin
     (set! rep-frames (cons ,control-point rep-frames))
     (set! rep-level (1+ rep-level))))

(define-macro (pop-frame)
  '(begin
     (set! rep-frames (cdr rep-frames))
     (set! rep-level (1- rep-level))))

(define (error-print error-msg)
  (format #t "~s: " (car error-msg))
  (apply format `(#t ,@(cdr error-msg)))
  (newline))

(set! error-handler
  (lambda error-msg
    (error-print error-msg)
    (let loop ((intr-level (enable-interrupts)))
      (if (positive? intr-level)
	  (loop (enable-interrupts))))
    (let loop ()
      (if (call-with-current-continuation
	   (lambda (control-point)
	     (push-frame control-point)
	     (rep-loop (the-environment))
	     #f))
	  (begin
	    (pop-frame)
	    (loop))))
    (newline)
    (pop-frame)
    (let ((next-frame (car rep-frames)))
      (next-frame #t))))

(define top-level-environment (the-environment))

(define (top-level)
  (let loop ()
    ;;; Allow GC to free old rep-frames when we get here on "reset":
    (set! rep-frames (list top-level-control-point))
    (if (call-with-current-continuation
	 (lambda (control-point)
	   (set! rep-frames (list control-point))
	   (set! top-level-control-point control-point)
	   (set! rep-level 0)
	   (rep-loop top-level-environment)
	   #f))
	(loop))))

(define (the-top-level)
  (top-level)
  (newline)
  (exit))

(the-top-level)
