;; stripchart.d
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

(define-widget-type 'stripchart "StripChart.h")

(prolog

"static void Get_Value (w, client_data, value) Widget w;
	XtPointer client_data; XtPointer value; {
    Object ret;

    ret = Funcall (Get_Function ((int)client_data), Null, 0);
    switch (TYPE(ret)) {
    case T_Fixnum: *(double *)value = (double)FIXNUM(ret); break;
    case T_Flonum: *(double *)value = FLONUM(ret)->val; break;
    case T_Bignum: *(double *)value = Bignum_To_Double (ret); break;
    default: Primitive_Error (\"stripchart sampler must return number\");
    }
}")

(define-widget-class 'stripchart 'stripChartWidgetClass)

(define-primitive 'stripchart-set-sampler '(w p)
"   int i;
    Arg a[1];
    XtCallbackList c;

    Check_Widget_Class (w, stripChartWidgetClass);
    Check_Procedure (p);
    XtSetArg (a[0], XtNgetValue, &c);
    XtGetValues (WIDGET(w)->widget, a, 1);
    if (c[0].callback)
	Primitive_Error (\"stripchart already has a sampler\");
    i = Register_Function (p);
    XtAddCallback (WIDGET(w)->widget, XtNgetValue, Get_Value, (XtPointer)i);
    return Make_Id ('s', (XtPointer)WIDGET(w)->widget, i);")

(define-primitive 'stripchart-remove-sampler '(i)
"   Widget w;

    w = (Widget)Use_Id (i, 's');
    XtRemoveCallback (w, XtNgetValue, Get_Value,
	(XtPointer)IDENTIFIER(i)->num);
    Deregister_Function (IDENTIFIER(i)->num);
    return Void;")
