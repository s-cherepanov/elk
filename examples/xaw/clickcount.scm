;;; -*-Scheme-*-
;;;
;;; Demonstrate usage of translations and actions.
;;;
;;; Based on an example program (xclickcount.c) from the O'Reilly
;;; collection of Xt example programs.

(require 'xaw)

(define top (application-initialize 'clickcount))
(define con (widget-context top))

(define increment-count
  (let ((count 0))
    (lambda (w event . args)
      (set! count (1+ count))
      (set-values! w 'label (format #f "# of clicks: ~s" count)))))

(context-add-action con 'increment-count increment-count)

(define label (create-managed-widget (find-class 'label) top
  'width 150 'label "Click here"))
(set-values! label 'translations "<BtnDown>: increment-count()")

(realize-widget top)
(context-main-loop con)
