#include "xt.h"

#include <X11/IntrinsicP.h>
#include <X11/CoreP.h>

Object Xt_Class_Name (class) WidgetClass class; {
    return Make_String (class->core_class.class_name,
        strlen (class->core_class.class_name));
}
