;;; -*-Scheme-*-
;;;
;;; Demonstrate usage of accelerators
;;;
;;; Based on an example program (xtryaccel.c) from the O'Reilly
;;; book `X Toolkit Intrinsics Programming Manual'

(require 'xaw)

(define top (application-initialize 'accel
  "*bye.label:    Goodbye"
  "*hello.label:  Hello"
  "*font:  *courier-bold-r*18*iso8859-1"))

(define box (create-managed-widget (find-class 'box) top))

(define bye (create-managed-widget 'bye (find-class 'command) box
  'accelerators "<KeyPress>q: set() notify()"))
(add-callback bye 'callback (lambda _ (exit)))

(define hello (create-managed-widget 'hello (find-class 'command) box
  'accelerators "<KeyPress>p: set() notify() reset()"))
(add-callback hello 'callback (lambda _ (display "Hello world!\n")))

(install-accelerators box bye)
(install-accelerators box hello)

(realize-widget top)
(display "Press 'p' for Hello, 'q' for Goodbye.\n")
(context-main-loop (widget-context top))
