;;; -*-Scheme-*-
;;;
;;; Demonstrate pipes, fork, exec.
;;;
;;;  (calc-open)  --  Open two pipes to/from UNIX dc command
;;;  (calc expr)  --  Send expression to dc, return result as a string
;;;  (calc-close) --  Close pipes, wait for child process
;;;
;;;
;;; This program requires vanilla UNIX dc.  It does not work with GNU dc,
;;; because GNU dc uses buffered output even if standard output points to
;;; a pipe.  This means that GNU dc does not produce any output until the
;;; pipe is closed; the call to read-string therefore just hangs.


(require 'unix)

(define calc-from-dc)   ; input port: standard output of dc command
(define calc-to-dc)     ; output port: standard input of dc command
(define calc-dc-pid)    ; process-ID of child process running dc

(define calc-dc-command "/usr/bin/dc")

(define (calc-open)
  (let* ((from (unix-pipe))
	 (to (unix-pipe))
	 (redirect-fd (lambda (a b)
			(unix-dup a b) (unix-close a))))
    (set! calc-dc-pid (unix-fork))
    (if (zero? calc-dc-pid)
	(begin
	  (unix-close (car from))
	  (unix-close (cdr to))
	  (redirect-fd (car to) 0)
	  (redirect-fd (cdr from) 1)
	  (unix-exec calc-dc-command '("dc")))
	(begin
	  (unix-close (cdr from))
	  (unix-close (car to))
	  (set! calc-to-dc   (unix-filedescriptor->port (cdr to)   "w"))
	  (set! calc-from-dc (unix-filedescriptor->port (car from) "r"))))))

(define (calc expr)
  (format calc-to-dc "~a~%" expr)
  (flush-output-port calc-to-dc)
  (read-string calc-from-dc))

(define (calc-close)
  (close-output-port calc-to-dc)
  (close-input-port calc-from-dc)
  (if (feature? 'unix:wait-process)
      (unix-wait-process calc-dc-pid)
      (unix-wait)))


;;; Test -- print sqrt(2):

(calc-open)
(display (calc "10k 2v p")) (newline)
(calc-close)
