#ifdef USE_ALLOCA

#ifdef INCLUDE_ALLOCA_H
#  include <alloca.h>
#endif

/* #pragma must be indented to prevent some C-compilers from complaining
 * about "undefined control".
 */
#ifdef PRAGMA_ALLOCA
   #pragma alloca
#endif

#if !defined(alloca) && !defined(__GNUC__)
C_LINKAGE_BEGIN
extern char *alloca P_((int));
C_LINKAGE_END
#endif

/* MIPS cc under Ultrix 4.2 requires argument to alloca() to be
 * parenthesized if it's a compound expression.
 *
 * Declare variable in Alloca_Begin and reference it in Alloca to make
 * sure users won't forget Alloca_Begin when using macros like
 * Get_String_Stack().
 */
#define Alloca_Begin int _Check_Alloca_Begin
#define Alloca(ret,type,size) (_Check_Alloca_Begin = 0,\
    (ret) = (type)alloca((size)))
#define Alloca_End

#else /* USE_ALLOCA */

extern MEM_NODE *Mem_List;
extern char *Mem_Alloc P_((unsigned));

#define Alloca_Begin MEM_NODE *_mem_first = 0
#define Alloca(ret,type,size) {\
    register MEM_NODE *_p;\
    _p = (MEM_NODE*)Mem_Alloc ((unsigned)(size) + sizeof(MEM_NODE));\
    _p->next = Mem_List;\
    _p->len = (size);\
    _p->refcnt = 1;\
    Mem_List = _p;\
    if (_mem_first == 0) _mem_first = _p;\
    (ret) = (type)(_p+1);\
}
#define Alloca_End {\
    register MEM_NODE *_p, *_q;\
    if (_mem_first != 0) {\
	_p = Mem_List;\
	do {\
	    _q = _p;\
	    _p = _p->next;\
	    if (--_q->refcnt == 0)\
		free ((char *)_q);\
	} while (_q != _mem_first);\
	Mem_List = _p;\
    }\
}

#endif
