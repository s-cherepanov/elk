;; toggle-btn.d
;;
;; $Id$
;;
;; Copyright 1990, 1991, 1992, 1993, 1994, 1995, Oliver Laumann, Berlin
;; Copyright 2002, 2003 Sam Hocevar <sam@hocevar.net>, Paris
;;
;; This software was derived from Elk 1.2, which was Copyright 1987, 1988,
;; 1989, Nixdorf Computer AG and TELES GmbH, Berlin (Elk 1.2 has been written
;; by Oliver Laumann for TELES Telematic Services, Berlin, in a joint project
;; between TELES and Nixdorf Microprocessor Engineering, Berlin).
;;
;; Oliver Laumann, TELES GmbH, Nixdorf Computer AG and Sam Hocevar, as co-
;; owners or individual owners of copyright in this software, grant to any
;; person or company a worldwide, royalty free, license to
;;
;;    i) copy this software,
;;   ii) prepare derivative works based on this software,
;;  iii) distribute copies of this software or derivative works,
;;   iv) perform this software, or
;;    v) display this software,
;;
;; provided that this notice is not removed and that neither Oliver Laumann
;; nor Teles nor Nixdorf are deemed to have made any representations as to
;; the suitability of this software for any purpose nor are held responsible
;; for any defects of this software.
;;
;; THERE IS ABSOLUTELY NO WARRANTY FOR THIS SOFTWARE.

(define-widget-type 'togglebutton '("ToggleB.h" "ToggleBG.h"))

(define-widget-class 'toggle-button        'xmToggleButtonWidgetClass)
(define-widget-class 'toggle-button-gadget 'xmToggleButtonGadgetClass)

(define-callback 'toggle-button 'armCallback          #t)
(define-callback 'toggle-button 'disarmCallback       #t)
(define-callback 'toggle-button 'valueChangedCallback #t)

(define-callback 'toggle-button-gadget 'armCallback          #t)
(define-callback 'toggle-button-gadget 'disarmCallback       #t)
(define-callback 'toggle-button-gadget 'valueChangedCallback #t)

;;; Ignore the `set' field in all callback structs (can do a get-values
;;; on the widget passed to the callback function).

(define toggle-button-callback->scheme
"   return Get_Any_CB ((XmAnyCallbackStruct *)x);")

(c->scheme 'callback:toggle-button-valueChangedCallback
            toggle-button-callback->scheme)
(c->scheme 'callback:toggle-button-gadget-valueChangedCallback
            toggle-button-callback->scheme)
