;;; -*-Scheme-*-
;;;
;;; Demonstrate locking
;;;
;;;  (lock-vi file)  --  Starts vi on file, providing exclusive access.

(require 'unix)

(define (lock-vi file)
  (let* ((fd (unix-open file '(read write)))
	 (lock ((record-constructor lock-record) #t 'set 0 0)))

    ;; attempt to apply lock to file; print a message and go
    ;; to sleep if lock is held by somebody else:

    (let loop ()
         (if (not (unix-set-lock fd lock #f))
	     (begin
	       (format #t "Someone else is editing ~s...~%" file)
	       (unix-sleep 10)
	       (loop))))

    ;; invoke vi; remove lock when done:

    (unix-system (format #f "vi ~a" file))
    (unix-remove-lock fd lock)))
