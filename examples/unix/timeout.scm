;;; -*-Scheme-*-
;;;
;;; Demonstrate signals and alarm
;;;
;;; (timeout-read fdescr seconds)  --  read with timeout

(require 'unix)

;;; Read a string from file descriptor fd and return it (maximum length
;;; 1000 characters).  Return #f on timeout (2nd arg, in seconds).

(define (timeout-read fd sec)
  (let ((str (make-string 1000))
	(old-handler 'default))
    (call/cc
      (lambda (tmo)
	(dynamic-wind
	  (lambda ()
	    (set! old-handler (unix-signal 'sigalrm (lambda _ (tmo #f))))
	    (unix-alarm sec))
	  (lambda ()
	    (substring str 0 (unix-read-string-fill! fd str)))
	  (lambda ()
	    (unix-alarm 0)
	    (unix-signal 'sigalrm old-handler)))))))


;;; Test

(display "Enter a line (timeout 5 seconds): ")
(let ((ret (timeout-read 0 5)))
  (if ret
      (format #t "Got ~s~%" ret)
      (format #t "~%Got timeout~%")))
