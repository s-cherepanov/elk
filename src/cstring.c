/* Convert Scheme strings to C strings.  The contents of strings has to
 * be copied, because strings in Elk do not have a terminating null-byte
 * (strings may _contain_ null-bytes).
 *
 * Get_String() and Get_Strsym() allocate memory in NUMSTRBUFS cyclically
 * reused buffers in the C heap.
 * The macros Get_String_Stack() and Get_Strsym_Stack() in include/cstring.h
 * allocate memory on the stack.  They have to be used whenever more than
 * NUMSTRBUFS strings are active in a function at the same time.
 */

#include "kernel.h"

static char *heapstr[NUMSTRBUFS];
static int heaplen[NUMSTRBUFS];
static int nextstr;

Init_Cstring() {  /* Preallocate memory to avoid fragmentation */
    int i;

    for (i = 0; i < NUMSTRBUFS; i++)
	heapstr[i] = Safe_Malloc (heaplen[i] = 512);
}

char *Get_String (str) Object str; {
    char **pp = &heapstr[nextstr];
    int len;
    
    Check_Type (str, T_String);
    if ((len = STRING(str)->size+1) > heaplen[nextstr]) {
	Disable_Interrupts;
	*pp = Safe_Realloc (*pp, len);
	heaplen[nextstr] = len;
	Enable_Interrupts;
    }
    bcopy (STRING(str)->data, *pp, --len);
    (*pp)[len] = '\0';
    if (++nextstr == NUMSTRBUFS) nextstr = 0;
    return *pp;
}

char *Get_Strsym (str) Object str; {
    if (TYPE(str) == T_Symbol)
	str = SYMBOL(str)->name;
    else if (TYPE(str) != T_String)
	Wrong_Type_Combination (str, "string or symbol");
    return Get_String (str);
}
