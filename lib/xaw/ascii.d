;; ascii.d
;;
;; $Id$
;;
;; Copyright 1990, 1991, 1992, 1993, 1994, 1995, Oliver Laumann, Berlin
;; Copyright 2002, 2003 Sam Hocevar <sam@zoy.org>, Paris
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

(define-widget-type 'asciitext "AsciiText.h")

(define-widget-class 'ascii-text 'asciiTextWidgetClass
  '(font Font FontStruct)
  '(foreground Foreground Pixel)
  '(dataCompression DataCompression Boolean)
  '(displayNonprinting Output Boolean)
  '(echo Output Boolean)
  '(editType EditType EditMode)
  '(length Length Int)
  '(pieceSize PieceSize Int)
  '(string String String)
  '(type Type AsciiType))

(define bad-resource
"    Primitive_Error (\"cannot get or set sink/source\");")

(scheme->c 'ascii-text-textSink   bad-resource)
(scheme->c 'ascii-text-textSource bad-resource)
(c->scheme 'ascii-text-textSink   bad-resource)
(c->scheme 'ascii-text-textSource bad-resource)

(define-primitive 'ascii-text-string '(w)
"   Arg a[1];
    char *s;

    Check_Widget_Class (w, asciiTextWidgetClass);
    XtSetArg (a[0], XtNstring, &s);
    XtGetValues (WIDGET(w)->widget, a, 1);
    return Make_String (s, strlen (s));")

