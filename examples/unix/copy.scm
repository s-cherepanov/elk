;;; -*-Scheme-*-
;;;
;;; Demonstrate open, stat, read, write.

(require 'unix)

(define copy-buffer-size 8192)

(define (copy-file from to)
  (let ((from-stat (unix-stat from))
        (to-stat (unix-errval (unix-stat to))))

    (if (eq? (stat-type from-stat) 'directory)        ; complain if "from"
	(error 'copy-file "~s is a directory" from))  ;   is a directory

    (if (and (not (unix-error? to-stat))              ; destination exists
	     (eq? (stat-type to-stat) 'directory))    ;   and is a directory?
	(set! to (format #f "~a/~a" to (pathname-tail from))))

    (let* ((to-fd (unix-open to '(write create exclusive)
			     (stat-mode from-stat)))
	   (from-fd (unix-open from '(read)))
	   (buf (make-string copy-buffer-size)))

      (let loop ((num-chars (unix-read-string-fill! from-fd buf)))
	   (if (positive? num-chars)
	       (begin
	         (unix-write to-fd buf num-chars)
		 (loop (unix-read-string-fill! from-fd buf)))))

      (unix-close from-fd)
      (unix-close to-fd))))

(define (string-rindex s c)
  (let loop ((i (string-length s)))
    (cond
      ((zero? i) #f)
      ((char=? (string-ref s (1- i)) c) (1- i))
      (else (loop (1- i))))))

(define (pathname-tail s)
  (let ((i (string-rindex s #\/)))
    (if i
	(substring s (1+ i) (string-length s))
	s)))

;;; Test -- copy /bin/date into a temporary file

(let ((tmp (unix-tempname)))
  (unwind-protect
    (begin
      (format #t "Copying /bin/date to ~a.~%" tmp)
      (copy-file "/bin/date" tmp)
      (format #t "Comparing files... ")
      (if (zero? (unix-system (format #f "cmp -s /bin/date ~s" tmp)))
        (format #t "OK.~%")
        (format #t "Oops, files differ.~%")))
    (format #t "Removing ~a.~%" tmp)
    (unix-unlink tmp)))
