/* object.h: The Scheme object representation, and a few other important
 * data types.
 *
 * $Id$
 *
 * Copyright 1990, 1991, 1992, 1993, 1994, 1995, Oliver Laumann, Berlin
 * Copyright 2002, 2003 Sam Hocevar <sam@zoy.org>, Paris
 *
 * This software was derived from Elk 1.2, which was Copyright 1987, 1988,
 * 1989, Nixdorf Computer AG and TELES GmbH, Berlin (Elk 1.2 has been written
 * by Oliver Laumann for TELES Telematic Services, Berlin, in a joint project
 * between TELES and Nixdorf Microprocessor Engineering, Berlin).
 *
 * Oliver Laumann, TELES GmbH, Nixdorf Computer AG and Sam Hocevar, as co-
 * owners or individual owners of copyright in this software, grant to any
 * person or company a worldwide, royalty free, license to
 *
 *    i) copy this software,
 *   ii) prepare derivative works based on this software,
 *  iii) distribute copies of this software or derivative works,
 *   iv) perform this software, or
 *    v) display this software,
 *
 * provided that this notice is not removed and that neither Oliver Laumann
 * nor Teles nor Nixdorf are deemed to have made any representations as to
 * the suitability of this software for any purpose nor are held responsible
 * for any defects of this software.
 *
 * THERE IS ABSOLUTELY NO WARRANTY FOR THIS SOFTWARE.
 */

#include <stdint.h>
#include <stdlib.h>

typedef struct {
    int64_t data;
    int tag;
} Object;

#define FIXBITS         (8 * (int)sizeof(int))
#define SIGNBIT         ((unsigned)1 << (FIXBITS-1))
#define CONSTBIT        1
#define TYPEBITS        (8 * (int)sizeof(int) - 1)
#define MAX_TYPE        ((1 << TYPEBITS) - 1)

#define UFIXNUM_FITS(i) (((i) & SIGNBIT) == 0)
#define FIXNUM_FITS(i)  1

#define TYPE(x)         ((x).tag >> 1)

#define FIXNUM(x)       ((int)(x).data)
#define CHAR(x)         ((int)(x).data)

#define POINTER(x)      ((void *)(ptrdiff_t)(x).data)
#define SETPOINTER(x,p) ((x).data = (ptrdiff_t)(void *)(p))
#define SET(x,t,p)      ((x).tag = (int)t << 1, (x).data = (p))

#define ISCONST(x)      ((x).tag & CONSTBIT)
#define SETCONST(x)     ((x).tag |= CONSTBIT)

#define EQ(x,y)         ((x).data == (y).data && (x).tag == (y).tag)

/* GC related macros:
 */
#define WAS_FORWARDED(obj) (TYPE(*(Object *)POINTER(obj)) == T_Broken_Heart)
#define UPDATE_OBJ(obj)    SETPOINTER(obj, POINTER(*(Object *)POINTER(obj)))

#ifdef GENERATIONAL_GC

   typedef int gcspace_t;                 /* type for space and type arrays */
   typedef ptrdiff_t gcptr_t;             /* type for pointers */
   typedef long int pageno_t;             /* type for page numbers */
   typedef ptrdiff_t addrarith_t;         /* type for address arithmetic */

   extern gcspace_t *space;
   extern gcspace_t current_space;
   C_LINKAGE_BEGIN
   extern int Visit P_((Object*));    /* required for REVIVE_OBJ below */
   C_LINKAGE_END

#  ifdef ARRAY_BROKEN
   extern pageno_t pagebase;
#  else
#  define pagebase ((pageno_t)0)
#  endif

#  define PAGEBYTES        512
#  define PAGE_TO_OBJ(p)   ((Object *) (((p) + pagebase) * PAGEBYTES))
#  define OBJ_TO_PAGE(p)   ((((gcptr_t)POINTER(p)) / PAGEBYTES) - pagebase)
#  define STABLE(x)        ((~space[(x)]) & 1)
#  define MAKEOBJ(o,t,p)   (SET(o, t, p))
#  define IS_ALIVE(obj)    ((WAS_FORWARDED(obj)) || \
                            (STABLE(OBJ_TO_PAGE(obj))) || \
                            (space[OBJ_TO_PAGE(obj)] == current_space))
#  define REVIVE_OBJ(obj)  Visit (&obj);
#else
   C_LINKAGE_BEGIN
   extern int Visit P_((Object*));    /* required in heap.c */
   C_LINKAGE_END
#  define IS_ALIVE(obj)    WAS_FORWARDED(obj)
#  define REVIVE_OBJ(obj)
#endif

/* Fixed types.  Cannot use enum, because the set of types is extensible:
 */
#define T_Fixnum          0      /* Must be 0 */
#define T_Bignum          1
#define T_Flonum          2
#define T_Null            3      /* empty list */
#define T_Boolean         4      /* #t (1) and #f (0) */
#define T_Unbound         5      /* only used internally */
#define T_Special         6      /* only used internally */
#define T_Character       7
#define T_Symbol          8
#define T_Pair            9
#define T_Environment    10      /* A pair */
#define T_String         11
#define T_Vector         12
#define T_Primitive      13      /* Primitive procedure */
#define T_Compound       14      /* Compound procedure */
#define T_Control_Point  15
#define T_Promise        16      /* Result of (delay expression) */
#define T_Port           17
#define T_End_Of_File    18
#define T_Autoload       19
#define T_Macro          20
#define T_Broken_Heart   21      /* only used internally */
#ifdef GENERATIONAL_GC
#  define T_Align_8Byte  22      /* only used internally */
#  define T_Freespace    23      /* only used internally */
#endif

#define BIGNUM(x)   ((struct S_Bignum *)POINTER(x))
#define FLONUM(x)   ((struct S_Flonum *)POINTER(x))
#define STRING(x)   ((struct S_String *)POINTER(x))
#define VECTOR(x)   ((struct S_Vector *)POINTER(x))
#define SYMBOL(x)   ((struct S_Symbol *)POINTER(x))
#define PAIR(x)     ((struct S_Pair *)POINTER(x))
#define PRIM(x)     ((struct S_Primitive *)POINTER(x))
#define COMPOUND(x) ((struct S_Compound *)POINTER(x))
#define CONTROL(x)  ((struct S_Control *)POINTER(x))
#define PROMISE(x)  ((struct S_Promise *)POINTER(x))
#define PORT(x)     ((struct S_Port *)POINTER(x))
#define AUTOLOAD(x) ((struct S_Autoload *)POINTER(x))
#define MACRO(x)    ((struct S_Macro *)POINTER(x))

typedef unsigned short gran_t;  /* Granularity of bignums */

struct S_Bignum {
    Object minusp;
    unsigned int size;          /* Number of ushorts allocated */
    unsigned int usize;         /* Number of ushorts actually used */
    gran_t data[1];             /* Data, lsw first */
};

struct S_Flonum {
    Object tag;               /* Each S_Foo must start with an Object */
    double val;
};

struct S_Symbol {
    Object value;
    Object next;
    Object name;               /* A string */
    Object plist;
};

struct S_Pair {
    Object car, cdr;
};

struct S_String {
    Object tag;
    int size;
    char data[1];
};

struct S_Vector {
    Object tag;
    int size;
    Object data[1];
};

enum discipline { EVAL, NOEVAL, VARARGS };
struct S_Primitive {
    Object tag;
    Object (*fun) P_((ELLIPSIS));
    const char *name;
    int minargs;
    int maxargs;    /* Or MANY */
    enum discipline disc;
};
#define MANY    100

struct S_Compound {
    Object closure;     /* (lambda (args) form ...) */
    Object env;         /* Procedure's environment */
    int min_args, max_args;
    int numforms;
    Object name;
};

typedef struct wind {
    struct wind *next, *prev;
    Object inout;                  /* Pair of thunks */
} WIND;

typedef struct funct {
    struct funct *next;
    char *name;
    void (*func) P_((void));
} FUNCT;

typedef struct gcnode {
    struct gcnode *next;
    int gclen;
    Object *gcobj;
} GCNODE;

typedef struct mem_node {
    struct mem_node *next;
    unsigned int len;
    unsigned long int refcnt;
} MEM_NODE;

#if defined(vax) || defined(__vax__)
   typedef int jmp_buf[17];
#else
#  include <setjmp.h>
#endif

struct S_Control {
    Object env;
    GCNODE *gclist;
    MEM_NODE *memlist;
    Object memsave;             /* string */
    Object gcsave;              /* vector */
    WIND *firstwind, *lastwind;
    int tailcall;
    ptrdiff_t delta;
#ifdef GENERATIONAL_GC
    int reloc;
#endif
    jmp_buf j;
    int size;
    unsigned long int intrlevel;
    char stack[1];    /* must be word aligned */
};

struct S_Promise {
    Object env;
    Object thunk;
    int done;
};

struct S_Port {
    Object name;    /* string */
    short flags;
    char unread;
    int ptr;
    FILE *file;
    unsigned int lno;
    int (*closefun) P_((FILE*));
};
#define P_OPEN    1 /* flags */
#define P_INPUT   2
#define P_STRING  4
#define P_UNREAD  8
#define P_BIDIR  16

#define IS_INPUT(port)   (PORT(port)->flags & (P_INPUT|P_BIDIR))
#define IS_OUTPUT(port) ((PORT(port)->flags & (P_INPUT|P_BIDIR)) != P_INPUT)

struct S_Autoload {
    Object files;
    Object env;
};

struct S_Macro {
    Object body;
    int min_args, max_args;
    Object name;
};


/* "size" is called with one object and returns the size of the object.
 *    If "size" is NOFUNC, then "const_size" is taken instead.
 * "eqv" and "equal" are called with two objects and return 0 or 1.
 *    NOFUNC may be passed instead (then eqv and equal always return #f).
 * "print" is called with an object, a port, a flag indicating whether
 *    the object is to be printed "raw" (a la display), the print-depth,
 *    and the print-length.
 * "visit" is called with a pointer to an object and a function.
 *    For each component of the object, the function must be called with
 *    a pointer to the component.  NOFUNC may be supplied.
 */
typedef struct {
    int haspointer;
    const char *name;
    int (*size) P_((Object));
    int const_size;
    int (*eqv) P_((Object, Object));
    int (*equal) P_((Object, Object));
    int (*print) P_((Object, Object, int, int, int));
    int (*visit) P_((Object*, int (*)(Object*)));
} TYPEDESCR;

#ifdef ELK_USE_PROTOTYPES
#  define NOFUNC 0
#else
#  define NOFUNC ((int (*)())0)
#endif


typedef struct sym {
    struct sym *next;
    char *name;
    unsigned long int value;
} SYM;

typedef struct {
    SYM *first;
    char *strings;
} SYMTAB;

typedef struct {
    char *name;
    int type;
} SYMPREFIX;

#define PR_EXTENSION     0   /* Elk extension initializers/finalizers */
#define PR_CONSTRUCTOR   1   /* C++ static constructors/destructors */


/* PFO, GENERIC, and MATCHFUN exist for backwards compatibility
 */
typedef Object (*PFO) P_((Object));
typedef int (*MATCHFUN) P_((ELLIPSIS));
#define GENERIC char*

typedef struct weak_node {
    struct weak_node *next;
    Object obj;
    PFO term;
    GENERIC group;
    char flags;
} WEAK_NODE;

/* flags */
#define WK_LEADER 1


typedef struct {
    char *name;
    unsigned long int val;
} SYMDESCR;


/* Function that can be registered as a reader by Define_Reader():
 */
typedef Object (*READFUN) P_((Object, int, int));
