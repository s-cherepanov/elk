;;; -*-Scheme-*-
;;;
;;; Tree widget demo

(define (make-tree tree parent x)
  (let ((p (create-managed-widget (find-class 'label) tree 'label (car x))))
    (if parent (set-values! p 'tree-parent parent))
    (do ((l (cdr x) (cdr l))) ((null? l))
	(if (pair? (car l))
	    (make-tree tree p (car l))
	    (let ((w (create-managed-widget (find-class 'label) tree
					    'label (car l))))
	      (set-values! w 'tree-parent p))))))

(require 'xwidgets)
(load-widgets shell label tree)

(define top (application-initialize 'tree))

(define tree (create-managed-widget (find-class 'tree) top))

(make-tree tree #f
  '(world
     (america
       (north
	 usa canada)
       (middle
	 mexico cuba)
       (south
	 brasilia ecuador chile))
     (europe
       france britain germany)
     (asia
       japan korea)
     (antarctica)))

(realize-widget top)
(context-main-loop (widget-context top))
