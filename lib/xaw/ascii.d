;;; -*-Scheme-*-

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

