;;; -*-Scheme-*-
;;;
;;; Main window demo

(require 'motif)

(define top (application-initialize 'main-window))
(set-values! top 'width 500 'height 800)

(define mw (create-managed-widget (find-class 'main-window) top))
(set-values! mw 'show-separator #t)

(define sb (create-managed-widget (find-class 'scroll-bar) mw))
(define sb2 (create-managed-widget (find-class 'scroll-bar) mw
   'orientation "horizontal"))
(define dr (create-managed-widget (find-class 'drawing-area) mw))

(set-values! mw 'vertical-scroll-bar sb 'horizontal-scroll-bar sb2)
(set-values! mw 'work-window dr)

(define mb (create-managed-widget (find-class 'row-column) mw
   'row-column-type 'menu-bar))

(define b1 (create-managed-widget (find-class 'cascade-button) mb))
(set-values! b1 'label-string 'File)
(define b2 (create-managed-widget (find-class 'cascade-button) mb))
(set-values! b2 'label-string 'Edit)
(define b2 (create-managed-widget (find-class 'cascade-button) mb))
(set-values! b2 'label-string 'Properties)
(define b4 (create-managed-widget (find-class 'cascade-button) mb))
(set-values! b4 'label-string 'Help)

(set-values! mb 'menu-help-widget b4)

(set-values! mw 'menu-bar mb)

(define cmd (create-managed-widget (find-class 'command) mw))
(set-values! cmd 'prompt-string "What next:" 'history-visible-item-count 5)
(set-values! cmd 'history-item-count 7 'history-items
  '(ls\ -l\ /bin write\ fred pwd mail\ dmr man\ 8\ crash shutdown\ +k
  echo\ "hello"))

(realize-widget top)
(context-main-loop (widget-context top))
