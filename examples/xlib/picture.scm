;;; -*-Scheme-*-

;;; -*- Mode:Lisp; Package:XLIB; Syntax:COMMON-LISP; Base:10; Lowercase:T -*-

;;; CLX - Point Graphing demo program

;;; Copyright (C) 1988 Michael O. Newton (newton@csvax.caltech.edu)

;;; Permission is granted to any individual or institution to use, copy,
;;; modify, and distribute this software, provided that this complete
;;; copyright and permission notice is maintained, intact, in all copies and
;;; supporting documentation.

;;; The author provides this software "as is" without express or
;;; implied warranty.

;;; This routine plots the recurrance
;;;      x <- y(1+sin(0.7x)) - 1.2(|x|)^.5
;;;      y <- .21 - x
;;; As described in a ?? 1983 issue of the Mathematical Intelligencer
;;; It has ONLY been tested under X.V11R2 on a Sun3 running KCL

(require 'xlib)

(define (picture point-count)
  (let* ((dpy (open-display))
	 (width 600)
	 (height 600)
	 (black (black-pixel dpy))
	 (white (white-pixel dpy))
	 (root (display-root-window dpy))
	 (win (create-window 'parent root 'background-pixel white
			   'event-mask '(exposure button-press)
			   'width width 'height height))
	 (gc (create-gcontext 'window win
			    'background white 'foreground black)))
    (map-window win)
    (unwind-protect
     (handle-events dpy #t #f
       (expose
	(lambda ignore
	  (clear-window win)
	  (draw-points win gc point-count 0.0 0.0 (* width 0.5) (* height 0.5))
	  (draw-poly-text win gc 10 10 (translate "Click a button to exit")
			  '1-byte)
	  #f))
       (else (lambda ignore #t)))
     (close-display dpy))))

;;; Draw points.  These should maybe be put into a an array so that they do
;;; not have to be recomputed on exposure.  X assumes points are in the range
;;; of width x height, with 0,0 being upper left and 0,H being lower left.
;;;      x <- y(1+sin(0.7x)) - 1.2(|x|)^.5
;;;      y <- .21 - x
;;; hw and hh are half-width and half-height of screen

(define (draw-points win gc count x y hw hh)
  (if (zero? (modulo count 100))
      (display-flush-output (window-display win)))
  (if (not (zero? count))
      (let ((xf (floor (* (+ 1.2 x) hw ))) ; These lines center the picture
	    (yf (floor (* (+ 0.5 y) hh ))))
	(draw-point win gc xf yf)
	(draw-points win gc (1- count)
		     (- (* y (1+ (sin (* 0.7 x)))) (* 1.2 (sqrt (abs x))))
		     (- 0.21 x)
		     hw hh))))

(define (translate string)
  (list->vector (map char->integer (string->list string))))

(picture 10000)
