;;; -*-Scheme-*-
;;;
;;; Auxiliary definitions for the menu demos

(define (create-menu-bar parent)
  (create-managed-widget (find-class 'row-column) parent
    'row-column-type 'menu-bar))

(define (create-menu type parent args)
  (define grand-parent (widget-parent parent))
  (if (and (not (eq? grand-parent 'none))
	   (eq? (widget-class grand-parent) (find-class 'menu-shell)))
      (set! parent grand-parent))
  (let ((shell (create-popup-shell (find-class 'menu-shell)
				   parent 'width 100 'height 100)))
    (apply create-widget (find-class 'row-column) shell
	                 'row-column-type type args)))

(define (create-popup-menu parent . args)
  (create-menu 'menu-popup parent args))

(define (create-pulldown-menu parent . args)
  (create-menu 'menu-pulldown parent args))

(define (create-option-menu parent . args)
    (apply create-managed-widget (find-class 'row-column) parent
				 'row-column-type 'menu-option args))

(define (create-cascade-pulldown parent pulldown . args)
  (let ((button (create-managed-widget (find-class 'cascade-button) parent)))
    (set-values! button 'sub-menu-id pulldown)
    (apply set-values! button args)
    button))

(define (menu-add-item! type menu args)
  (let ((item (create-managed-widget (find-class type) menu)))
    (apply set-values! item args)
    item))

(define (menu-add-label! menu . args)
  (menu-add-item! 'label menu args))

(define (menu-add-separator! menu . args)
  (menu-add-item! 'separator menu args))

(define (menu-add-button! menu . args)
  (menu-add-item! 'push-button menu args))
