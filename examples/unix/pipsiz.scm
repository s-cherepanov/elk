;;; -*-Scheme-*-
;;;
;;; Demonstrate non-blocking I/O
;;;
;;;  (pipe-size)  --  Calculate capacity of pipe.

(require 'unix)

(define (pipe-size)
  (let* ((pipe (unix-pipe))
	 (flags (unix-filedescriptor-flags (cdr pipe)))
	 (len 32)                    ; assumes capacity is multiple of len
	 (noise (make-string len))
	 (flag (if (memq 'nonblock (unix-list-filedescriptor-flags))
		   'nonblock
		   'ndelay)))

    ;; enable non-blocking I/O for write end of pipe:
    (unix-filedescriptor-flags (cdr pipe) (cons flag flags))

    (unwind-protect
      (let loop ((size 0))
	   (if (unix-error? (unix-errval (unix-write (cdr pipe) noise)))
	       (if (memq (unix-errno) '(eagain ewouldblock edeadlk))
		   size
		   (error 'pipe-size "~E"))
	       (loop (+ size 32))))
      (unix-close (car pipe))
      (unix-close (cdr pipe)))))

(print (pipe-size))
