#include "kernel.h"

#include <stdlib.h>

char *Safe_Malloc (unsigned int size) {
    char *ret;

    Disable_Interrupts;
    if ((ret = malloc (size)) == 0) {
	if (Interpreter_Initialized)
	    Primitive_Error ("not enough memory to malloc ~s bytes",
		Make_Integer (size));
	else
	    Fatal_Error ("not enough memory to malloc %u bytes", size);
    }
    Enable_Interrupts;
    return ret;
}

char *Safe_Realloc (char *ptr, unsigned int size) {
    char *ret;

    Disable_Interrupts;
    if ((ret = ptr ? realloc (ptr, size) : malloc (size)) == 0) {
	if (Interpreter_Initialized)
	    Primitive_Error ("not enough memory to malloc ~s bytes",
		Make_Integer (size));
	else
	    Fatal_Error ("not enough memory to malloc %u bytes", size);
    }
    Enable_Interrupts;
    return ret;
}
