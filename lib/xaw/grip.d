;;; -*-Scheme-*-

(define-widget-type 'grip "Grip.h")

(define-widget-class 'grip 'gripWidgetClass)

(define-callback 'grip 'callback #t)

(c->scheme 'callback:grip-callback
"   Object args, ret, t;
    register i;
    GripCallData p = (GripCallData)x;
    GC_Node3;

    args = ret = t = Null;
    GC_Link3 (args, ret, t);
    args = Get_Event_Args (p->event);
    ret = Cons (Copy_List (args), Null);
    Destroy_Event_Args (args);
    t = P_Make_List (Make_Integer (p->num_params), Null);
    for (i = 0, Cdr (ret) = t; i < p->num_params; i++, t = Cdr (t)) {
	Object s;
	
	s = Make_String (p->params[i], strlen (p->params[i]));
	Car (t) = s;
    }
    GC_Unlink;
    return ret;")
