;;; -*-Scheme-*-
;;;
;;; Scroll box demo

(require 'xaw)

(define items (list 'Helvetica 'Courier 'Times 'Palatino 'Zapf\ Chancery
		    'Zapf\ Dingbats))
(set-cdr! (last-pair items) items)

(define top (application-initialize 'scrollbox))
(define dpy (widget-display top))

(define dia-bits "\0\0\100\0\340\0\360\1\370\3\374\7\376\17\374\7\370\3\360\1\340\0\100\0\0\0")
(define dia (create-bitmap-from-data (display-root-window dpy) dia-bits 13 13))

(define box (create-managed-widget (find-class 'box) top))
(set-values! box 'width 200)

(define button (create-managed-widget (find-class 'command) box))
(set-values! button 'bitmap dia)

(define label (create-managed-widget (find-class 'label) box))
(set-values! label 'width 130 'label (car items) 'resize #f 'justify 'left
                   'font (open-font dpy "*courier-bold-r-normal--14*"))
(add-callback button 'callback
  (lambda (w)
    (set! items (cdr items))
    (set-values! label 'label (car items))))

(realize-widget top)
(context-main-loop (widget-context top))
