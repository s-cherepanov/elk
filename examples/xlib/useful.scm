;;; -*-Scheme-*-

(require 'xlib)

(define dpy
  (open-display))

(define (f)
  (display-wait-output dpy #t))

(define root
  (display-root-window dpy))

(define cmap
  (display-colormap dpy))

(define white (white-pixel dpy))
(define black (black-pixel dpy))

(define rgb-white (query-color cmap white))
(define rgb-black (query-color cmap black))

(define win
  (create-window
    'parent root
    'width 300 'height 300
    'background-pixel white))

(define gc
  (create-gcontext
    'window win
    'background white 'foreground black))

(map-window win)
