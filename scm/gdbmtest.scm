;;; -*-Scheme-*-
;;;
;;; An interactive command loop for testing the GNU gdbm extension.
;;; Contributed by Martin Stut.


(require 'gdbm.o)

(let ((gf (gdbm-open 'test.gdbm 1024 'create)) (last "nothing"))
     (if (not gf)
	 (error 'gdbm-open "cannot open test.gdbm"))
     (format #t "Type ? for help~%")
     (let loop ((op (read-char)))
	  (newline)
	  (if (not (char=? op #\newline))
	      (read-string)) ; flush rest of line
	  (case op
	  ((#\? #\h)
	    (format #t "c -- count items~%")
	    (format #t "d -- delete item~%")
	    (format #t "f -- fetch item~%")
	    (format #t "s -- store item~%")
	    (format #t "n -- next key~%")
	    (format #t "1 -- first key~%")
	    (format #t "2 -- next key of last n, 1, or 2~%")
	    (format #t "r -- reorganize~%")
	    (format #t "q -- quit~%"))
	  (#\c
	    (do ((i 0 (1+ i))
		 (x (gdbm-firstkey gf) (gdbm-nextkey gf x)))
		((not x) (format #t "Number of entries: ~s~%" i))))
	  (#\d
	    (display "Key: ")
	    (if (gdbm-delete gf (read-string))
		(format #t "Deleted.~%")
		(format #t "Doesn't exist.~%")))
	  (#\f
	    (display "Key: ")
	    ((lambda (d)
	       (if d 
		   (format #t "Data: ~s~%" d)
		   (format #t "Doesn't exist.~%")))
	      (gdbm-fetch gf (read-string))))
	  (#\s
	    (display "Key: ")
	    ((lambda (k)
	       (display "Data: ")
	       (if (= 1 (gdbm-store gf k (read-string) 'insert))
		   (format #t "Already there.~%")
		   (format #t "Inserted.~%")))
	      (read-string)))
	  (#\n
	    (display "Key: ")
	    ((lambda (r)
	       (if r
		   (begin
	             (format #t "Next: ~s Data: ~s~%" r (gdbm-fetch gf r))
		     (set! last r))
		   (print #f)))
	      (gdbm-nextkey gf (read-string))))
	  (#\1
	    ((lambda (r)
	       (if r
		   (begin
		     (format #t "First: ~s Data: ~s~%" r (gdbm-fetch gf r))
		     (set! last r))
		   (print #f)))
	      (gdbm-firstkey gf)))
	  (#\2
	    ((lambda (r)
	       (if r
		   (begin
	             (format #t "Next: ~s Data: ~s~%" r (gdbm-fetch gf r))
		     (set! last r))
		   (print #f)))
	      (gdbm-nextkey gf last)))
	  (#\r
	    (gdbm-reorganize gf)
	    (format #t "Reorganized.~%"))
	  (#\q
	    (exit)))
	  (loop (read-char))))
