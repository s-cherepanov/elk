(require 'unix)

;; Map file type to letter

(define type-char-map
  '((regular . #\-) (directory . #\d) (symlink . #\l) (socket . #\=)
    (fifo . #\p) (character-special . #\c) (block-special . #\b)
    (unknown . #\?)))

;; Map file mode to /bin/ls-style mode string without/with taking
;; setuid/setgid bit into account

(define perm-tab  '#("---" "--x" "-w-" "-wx" "r--" "r-x" "rw-" "rwx"))
(define perm-tab1 '#("--S" "--s" "-wS" "-ws" "r-S" "r-s" "rwS" "rws"))

;; Right justify string within field of `n' spaces

(define (rjust str n)
  (let* ((y (string-append (make-string n #\space) str))
         (l (string-length y)))
    (substring y (- l n) l)))

;; Left justify string within field of `n' spaces

(define (ljust str n)
  (let* ((y (string-append str (make-string n #\space)))
         (l (string-length y)))
    (substring y 0 n)))


(define (print-type type)
  (display (cdr (assq type type-char-map))))

(define (print-perm perm setid?)
  (let ((bits (vector-ref (if setid? perm-tab1 perm-tab) perm)))
    (display bits)))

;; This could probably be made more efficient by using Elk's bitstring
;; extension

(define (print-mode mode)
  (let ((owner 0) (group 0) (world (modulo mode 8)))
    (set! mode (quotient mode 8)) (set! group (modulo mode 8))
    (set! mode (quotient mode 8)) (set! owner (modulo mode 8))
    (set! mode (quotient mode 8))
    (print-perm owner (>= mode 4))
    (print-perm group (odd? (quotient mode 2)))
    (print-perm world #f)))

(define (print-nlink nlink)
  (display (rjust (number->string nlink) 3))
  (display #\space))

(define (print-owner uid)
  (display (ljust (passwd-name (unix-get-passwd uid)) 8)))

(define (print-size size)
  (display (rjust (number->string size) 9)))

(define (print-mtime mtime)
  (display (substring (unix-time->string mtime) 3 16))
  (display #\space))

(define (print-name name)
  (display name))

(define (print-link name)
  (display " -> ")
  (display (unix-readlink name)))

(define (list-entry name)
  (if (not (char=? (string-ref name 0) #\.))
      (let ((s (unix-lstat name)))
        (print-type (stat-type s))
        (print-mode (stat-mode s))
        (print-nlink (stat-nlink s))
        (print-owner (stat-uid s))
        (print-size (stat-size s))
        (print-mtime (stat-mtime s))
        (print-name name)
	(if (eq? (stat-type s) 'symlink)
	    (print-link name))
        (newline))))

(define (ls)
  (for-each list-entry (unix-read-directory ".")))

(ls)
