;;; -*-Scheme-*-
;;;
;;; The Scheme part of the Xlib extension.

(require 'siteinfo)

(fluid-let ((load-libraries (string-append site-lib-xlib " " load-libraries)))
  (require 'xlib.o))

(define (create-window . args)
  (apply-with-keywords
    'create-window xlib-create-window
    '((parent) (x 0) (y 0) (width) (height) (border 2))
    'set-window-attributes set-window-attributes-slots args))

(define (create-gcontext . args)
  (apply-with-keywords
    'create-gcontext xlib-create-gcontext
    '((window))
    'gcontext gcontext-slots args))

(define (set-wm-hints! . args)
  (apply-with-keywords
    'set-wm-hints! xlib-set-wm-hints!
    '((window))
    'wm-hints wm-hints-slots args))

(define (wm-hints w)
  (cdr (vector->list (xlib-wm-hints w))))

(define (set-wm-normal-hints! . args)
  (apply-with-keywords
    'set-wm-normal-hints! xlib-set-wm-normal-hints!
    '((window))
    'size-hints size-hints-slots args))

(define (wm-normal-hints w)
  (cdr (vector->list (xlib-wm-normal-hints w))))
    
(define (reconfigure-wm-window . args)
  (apply-with-keywords
    'reconfigure-wm-window xlib-reconfigure-wm-window
    '((window) (screen))
    'window-configuration window-configuration-slots args))


(define (apply-with-keywords name function formals tag slots args)
  (let* ((v (make-vector (1+ (length slots)) '()))
 	 (empty '(empty))
	 (l (make-list (1+ (length formals)) empty))
	 (slot '()))
    (vector-set! v 0 tag)
    (do ((a args (cddr a))) ((null? a))
      (if (not (symbol? (car a)))
	  (error name "even-numbered argument must be a symbol"))
      (if (null? (cdr a))
	  (error name "missing value for ~s" (car a)))
      (set! slot (assq (car a) slots))
      (if slot
	  (vector-set! v (cdr slot) (cadr a))
	  (let loop ((f formals) (g l))
	    (if (null? f)
		(error name "unknown argument ~s" (car a)))
	    (if (eq? (car a) (caar f))
		(set-car! g (cadr a))
		(loop (cdr f) (cdr g))))))
    (set-car! (last-pair l) v)
    (do ((f formals (cdr f)) (a l (cdr a))) ((null? f))
      (if (eq? (car a) empty)
	  (if (pair? (cdar f))
	      (set-car! a (cadar f))
	      (error name "you must specify a value for ~s" (caar f)))))
    (apply function l)))


;;; Definition of the access and update functions for window attributes,
;;; geometry, gcontexts, etc.

(define-macro (define-functions definer type fun pref)
  (let ((slots (string->symbol (format #f "~s-slots" type))))
    `(for-each eval (map (lambda (s)
       (,definer ',type (1+ (length ,slots)) ,fun s ,pref)) ,slots))))

(define (define-accessor-with-cache type num-slots fun slot pref)
  (let ((name (string->symbol (format #f pref (car slot)))))
    `(define (,name object)
       (general-accessor object ',type ,fun ,(cdr slot)))))

(define (define-mutator-with-cache type num-slots fun slot pref)
  (let ((name (string->symbol (format #f pref (car slot)))))
    `(define (,name object val)
       (general-mutator object val ',type ,num-slots ,fun ,(cdr slot)))))

(define (define-accessor type num-slots fun slot pref)
  (let ((name (string->symbol (format #f pref (car slot)))))
    `(define (,name . args)
       (vector-ref (apply ,fun args) ,(cdr slot)))))


(define-functions define-accessor-with-cache
  get-window-attributes xlib-get-window-attributes "window-~s")

(define-functions define-mutator-with-cache
  set-window-attributes xlib-change-window-attributes "set-window-~s!")

(define-functions define-mutator-with-cache
  window-configuration xlib-configure-window "set-window-~s!")

(define-functions define-accessor-with-cache
  geometry xlib-get-geometry "drawable-~s")

(define-functions define-mutator-with-cache
  gcontext xlib-change-gcontext "set-gcontext-~s!")

;; Note:  gcontext-clip-mask and gcontext-dashes are bogus.

(define gcontext-values-slots gcontext-slots)

(define-functions define-accessor-with-cache
  gcontext-values xlib-get-gcontext-values "gcontext-~s")

(define-functions define-accessor-with-cache
  font-info xlib-font-info "font-~s")

(define-functions define-accessor
  char-info xlib-char-info "char-~s")

(define (min-char-info c) (xlib-char-info c 'min))
(define (max-char-info c) (xlib-char-info c 'max))

;; Note:  min-char-attributes, max-char-attributes, and
;; text-extents-attributes are bogus.

(define-functions define-accessor
  char-info min-char-info "min-char-~s")

(define-functions define-accessor
  char-info max-char-info "max-char-~s")

(define-functions define-accessor
  char-info xlib-text-extents "extents-~s")


;;; ``cache'' is an a-list of (drawable-or-gcontext-or-font . state) pairs,
;;; where state is a vector of buffers as listed below.  Each slot in
;;; a vector can be #f to indicate that the cache is empty.  The cache
;;; is manipulated by the ``with'' macro.

(define cache '())

(define num-slots 7)

(put 'set-window-attributes 'cache-slot 0)
(put 'get-window-attributes 'cache-slot 1)
(put 'window-configuration  'cache-slot 2)
(put 'geometry              'cache-slot 3)
(put 'gcontext              'cache-slot 4)
(put 'font-info             'cache-slot 5)
(put 'gcontext-values       'cache-slot 6)


;;; List of buffers that are manipulated by mutator functions and must
;;; be flushed using the associated update function when a ``with'' is
;;; left (e.g., a set-window-attributes buffer is manipulated by
;;; set-window-FOO functions; the buffer is flushed by a call to
;;; (change-window-attributes WINDOW BUFFER)):

(define mutable-types '(set-window-attributes window-configuration gcontext))

(put 'set-window-attributes 'update-function xlib-change-window-attributes)
(put 'window-configuration  'update-function xlib-configure-window)
(put 'gcontext              'update-function xlib-change-gcontext)


;;; Some types of buffers in the cache are invalidated when other
;;; buffers are written to.  For instance, a get-window-attributes
;;; buffer for a window must be filled again when the window's
;;; set-window-attributes or window-configuration buffers have been
;;; written to.

(put 'get-window-attributes 'invalidated-by
     '(set-window-attributes window-configuration))
(put 'geometry              'invalidated-by
     '(set-window-attributes window-configuration))
(put 'gcontext-values       'invalidated-by
     '(gcontext))

;;; Within the scope of a ``with'', the first call to a OBJECT-FOO
;;; function causes the result of the corresponding Xlib function to
;;; be retained in the cache; subsequent calls just read from the cache.
;;; Similarly, calls to Xlib functions for set-OBJECT-FOO! functions are
;;; delayed until exit of the ``with'' body or until a OBJECT-FOO
;;; is called and the cached data for this accessor function has been
;;; invalidated by the call to the mutator function (see ``invalidated-by''
;;; property above).

(define-macro (with object . body)
  `(if (assq ,object cache)          ; if it's already in the cache, just
       (begin ,@body)                ;   execute the body.
       (dynamic-wind
	(lambda ()
	  (set! cache (cons (cons ,object (make-vector num-slots #f)) cache)))
	(lambda ()
	  ,@body)
	(lambda ()
	  (for-each (lambda (x) (flush-cache (car cache) x)) mutable-types)
	  (set! cache (cdr cache))))))

;;; If a mutator function has been called on an entry in the cache
;;; of the given type, flush it by calling the right update function.

(define (flush-cache entry type)
  (let* ((slot (get type 'cache-slot))
	 (buf (vector-ref (cdr entry) slot)))
    (if buf
	(begin
	  ((get type 'update-function) (car entry) buf)
	  (vector-set! (cdr entry) slot #f)))))

;;; General accessor function (OBJECT-FOO).  See if the data in the
;;; cache have been invalidated.  If this is the case, or if the cache
;;; has not yet been filled, fill it.

(define (general-accessor object type fun slot)
  (let ((v) (entry (assq object cache)))
    (if entry
	(let ((cache-slot (get type 'cache-slot))
	      (inval (get type 'invalidated-by)))
	  (if inval
	      (let ((must-flush #f))
		(for-each
		 (lambda (x)
		   (if (vector-ref (cdr entry) (get x 'cache-slot))
		       (set! must-flush #t)))
		 inval)
		(if must-flush
		    (begin
		      (for-each (lambda (x) (flush-cache entry x)) inval)
		      (vector-set! (cdr entry) cache-slot #f)))))
	  (if (not (vector-ref (cdr entry) cache-slot))
	      (vector-set! (cdr entry) cache-slot (fun object)))
	  (set! v (vector-ref (cdr entry) cache-slot)))
	(set! v (fun object)))
    (vector-ref v slot)))


;;; General mutator function (set-OBJECT-FOO!).  If the cache is empty,
;;; put a new buffer of the given type and size into it.  Write VAL
;;; into the buffer.

(define (general-mutator object val type num-slots fun slot)
  (let ((entry (assq object cache)))
    (if entry
	(let ((cache-slot (get type 'cache-slot)))
	  (if (not (vector-ref (cdr entry) cache-slot))
	      (let ((v (make-vector num-slots '())))
		(vector-set! v 0 type)
		(vector-set! (cdr entry) cache-slot v)
		(vector-set! v slot val))
	      (vector-set! (vector-ref (cdr entry) cache-slot) slot val)))
	(let ((v (make-vector num-slots '())))
	  (vector-set! v 0 type)
	  (vector-set! v slot val)
	  (fun object v)))))



(define (translate-text string)
  (list->vector (map char->integer (string->list string))))

(define (drawable? d)
  (or (window? d) (pixmap? d)))

(define (clear-window w)
  (clear-area w 0 0 0 0 #f))

(define (raise-window w)
  (set-window-stack-mode! w 'above))

(define (lower-window w)
    (set-window-stack-mode! w 'below))

(define (restack-windows l)
  (let loop ((w (car l)) (t (cdr l)))
    (if t
	(begin
	  (set-window-sibling! (car t) w)
	  (set-window-stack-mode! (car t) 'below)
	  (loop (car t) (cdr t))))))

(define (define-cursor w c)
  (set-window-cursor! w c))

(define (undefine-cursor w)
  (set-window-cursor! w 'none))

(define (create-font-cursor dpy which)
  (let ((font (open-font dpy 'cursor)))
    (unwind-protect
     (create-glyph-cursor font which font (1+ which)
			  (make-color 0 0 0) (make-color 1 1 1))
     (close-font font))))

(define (synchronize d)
  (set-after-function! d (lambda (d) (display-wait-output d #f))))

(define (font-property font prop)
  (let* ((dpy (font-display font))
	(atom (intern-atom dpy prop))
	(properties (vector->list (font-properties font)))
	(result (assq atom properties)))
    (if result
	(cdr result)
	result)))

(define-macro (with-server-grabbed dpy . body)
  `(dynamic-wind
    (lambda () (grab-server ,dpy))
    (lambda () ,@body)
    (lambda () (ungrab-server ,dpy))))

(define (warp-pointer dst dst-x dst-y)
  (general-warp-pointer (window-display dst) dst dst-x dst-y 'none 0 0 0 0))

(define (warp-pointer-relative dpy x-off y-off)
  (general-warp-pointer dpy 'none x-off y-off 'none 0 0 0 0))

(define (query-best-cursor dpy w h)
  (query-best-size dpy w h 'cursor))

(define (query-best-tile dpy w h)
  (query-best-size dpy w h 'tile))

(define (query-best-stipple dpy w h)
  (query-best-size dpy w h 'stipple))

(define store-buffer)
(define store-bytes)
(define fetch-buffer)
(define fetch-bytes)
(define rotate-buffers)

(let ((xa-string (make-atom 31))
      (xa-cut-buffers
       (vector (make-atom 9) (make-atom 10) (make-atom 11) (make-atom 12)
	       (make-atom 13) (make-atom 14) (make-atom 15) (make-atom 16))))

(set! store-buffer (lambda (dpy bytes buf)
  (if (<= 0 buf 7)
      (change-property
       (display-root-window dpy)
       (vector-ref xa-cut-buffers buf) xa-string 8 'replace bytes))))

(set! store-bytes (lambda (dpy bytes)
  (store-buffer dpy bytes 0)))

(set! fetch-buffer (lambda (dpy buf)
  (if (<= 0 buf 7)
      (multiple-value-bind (type format data bytes-left)
	(get-property
	  (display-root-window dpy)
	  (vector-ref xa-cut-buffers buf) xa-string 0 100000 #f)
	(if (and (eq? type xa-string) (< format 32)) data ""))
	"")))

(set! fetch-bytes (lambda (dpy)
  (fetch-buffer dpy 0)))

(set! rotate-buffers (lambda (dpy delta)
  (rotate-properties (display-root-window dpy) xa-cut-buffers delta))))


(define xa-wm-normal-hints (make-atom 40))

(define (xlib-wm-normal-hints w)
  (xlib-wm-size-hints w xa-wm-normal-hints))

(define (xlib-set-wm-normal-hints! w h)
  (xlib-set-wm-size-hints! w xa-wm-normal-hints h))


(define xa-wm-name (make-atom 39))
(define xa-wm-icon-name (make-atom 37))
(define xa-wm-client-machine (make-atom 36))

(define (wm-name w)
  (get-text-property w xa-wm-name))

(define (wm-icon-name w)
  (get-text-property w xa-wm-icon-name))

(define (wm-client-machine w)
  (get-text-property w xa-wm-client-machine))

(define (set-wm-name! w s)
  (set-text-property! w s xa-wm-name))

(define (set-wm-icon-name! w s)
  (set-text-property! w s xa-wm-icon-name))

(define (set-wm-client-machine! w s)
  (set-text-property! w s xa-wm-client-machine))


;; Backwards compatibility:

(define display-root-window display-default-root-window)

(define display-colormap display-default-colormap)

;; Backwards compatibility hack for old-style make-* functions:

(define-macro (make-compat make-macro create-function)
  `(define-macro (,make-macro . args)
     (let ((cargs 
	    (let loop ((a args) (v '()))
	      (if (null? a)
		  v
		  (loop (cdr a) `(',(caar a) ,(cadar a) ,@v))))))
       (cons ,create-function cargs))))

(make-compat make-gcontext create-gcontext)
(make-compat make-window create-window)


;;; Describe functions go here:


(provide 'xlib)
