;;; -*-Scheme-*-
;;;
;;; Stripchart widget demo

(require 'xaw)

(define top (application-initialize 'stripchart))

(define s (create-managed-widget (find-class 'stripchart) top))
(set-values! s 'update 1 'jump-scroll 2)

(define id
  (stripchart-set-sampler s
    (let ((x -.1))
      (lambda ()
        (set! x (+ x .1))
        (1+ (sin x))))))

(realize-widget top)
(context-main-loop (widget-context top))
