/* Alloca() simulation.
 */

#include "kernel.h"

#ifndef USE_ALLOCA

extern char *malloc();

MEM_NODE *Mem_List;

char *Mem_Alloc (unsigned int size) {
    char *ret;

    Disable_Interrupts;
    if ((ret = malloc (size)) == 0)
	Fatal_Error ("not enough memory to malloc %u bytes", size);
    Enable_Interrupts;
    return ret;
}

Free_Mem_Nodes (MEM_NODE *first) {
    MEM_NODE *p;

    Disable_Interrupts;
    while (p = first) {
	first = first->next;
	if (--p->refcnt == 0)
	    free ((char *)p);
    }
    Enable_Interrupts;
}

Save_Mem_Nodes (Object cont) {
    unsigned int sum = 0;
    char *s;
    MEM_NODE *p;
    Object str;
    GC_Node;

    CONTROL(cont)->memlist = Mem_List;
    for (p = Mem_List; p; p = p->next)
	sum += p->len;
    GC_Link (cont);
    str = Make_String ((char *)0, sum);
    CONTROL(cont)->memsave = str;
    GC_Unlink;
    for (p = Mem_List, s = STRING(str)->data; p; s += p->len, p = p->next) {
	memcpy (s, p+1, p->len);
	p->refcnt++;
    }
}

Restore_Mem_Nodes (Object cont) {
    MEM_NODE *p;
    char *s;
    Object str;

    Free_Mem_Nodes (Mem_List);
    Mem_List = CONTROL(cont)->memlist;
    str = CONTROL(cont)->memsave;
    for (p = Mem_List, s = STRING(str)->data; p; s += p->len, p = p->next) {
	p->refcnt++;
	memcpy (p+1, s, p->len);
    }
}

Object Save_GC_Nodes () {
    Object vec;
    register unsigned int sum = 0, i = 0, n;
    register GCNODE *p;

    for (p = GC_List; p; p = p->next)
	sum += p->gclen <= 0 ? 1 : p->gclen-1;
    vec = Make_Vector (sum, Null);
    for (p = GC_List; p; p = p->next, i += n) {
	n = p->gclen <= 0 ? 1 : p->gclen-1;
	memcpy (&(VECTOR(vec)->data[i]), p->gcobj, n * sizeof (Object));
    }
    return vec;
}

Restore_GC_Nodes (Object vec) {
    register int i = 0, n;
    register GCNODE *p;

    for (p = GC_List; p; p = p->next, i += n) {
	n = p->gclen <= 0 ? 1 : p->gclen-1;
	memcpy (p->gcobj, &(VECTOR(vec)->data[i]), n * sizeof (Object));
    }
}

#endif
