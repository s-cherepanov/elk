;;; -*-Scheme-*-
;;;
;;; This file is `required' in place of `xwidgets' when the Motif widgets
;;; are to be used.

(provide 'motif)

(require 'xwidgets)

(set! widget-subdirectory 'xm)
(set! load-always '(support))
