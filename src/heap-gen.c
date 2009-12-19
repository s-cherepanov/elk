/* heap-gen.c: The generational, incremental garbage collector.
 * Written by Marco Scheibe. Fixes provided by Craig McPheeters,
 * Carsten Bormann, Jon Hartlaub, Charlie Xiaoli Huang, Gal Shalif.
 *
 * This garbage collector is still experimental and probably needs to be
 * rewritten at least in parts.  See also ../BUGS.  If your application
 * does not work correctly and you suspect the generational garbage
 * collector to be the culprit, try the stop-and-copy GC instead.
 *
 * $Id$
 *
 * Copyright 1990, 1991, 1992, 1993, 1994, 1995, Oliver Laumann, Berlin
 * Copyright 2002, 2003 Sam Hocevar <sam@hocevar.net>, Paris
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

#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#if defined(MPROTECT_SIG) || defined(MPROTECT_MMAP)
#  include <sys/mman.h>
#endif
#if defined(HAVE_UNISTD_H)
#  include <unistd.h>
#  if defined(_SC_PAGE_SIZE) && !defined(_SC_PAGESIZE)   /* Wrong in HP-UX */
#    define _SC_PAGESIZE _SC_PAGE_SIZE
#  endif
#endif
#ifdef SIGSEGV_SIGINFO
#  include <siginfo.h>
#  include <ucontext.h>
#endif

/* The following variables may be set from outside the collector to
 * fine-tune some used parameters.
 */

int tuneable_forward_region = 5;   /* fraction of heap pages that are tried
                                    * to allocate as forward region when
                                    * collecting.
                                    */
int tuneable_force_total = 35;     /* % newly allocated during collection
                                    * to force total collection
                                    */
int tuneable_newly_expand = 25;    /* % of heap newly allocated during
                                    * a total collection to force heap
                                    * expansion.
                                    */
int tuneable_force_expand = 20;    /* % stable to force heap expansion
                                    */

/* ------------------------------------------------------------------------

defined in object.h:

typedef int gcspace_t;          // type used for space and type arrays
typedef unsigned int gcptr_t;   // type used for pointers

   ------------------------------------------------------------------------ */

static int percent = 0;
static pageno_t old_logical_pages;

static int inc_collection = 0;

static int incomplete_msg = 0;

static pageno_t logical_pages, spanning_pages, physical_pages;

/* pagebase is #defined in object.h if ARRAY_BROKEN is not defined. */

#ifdef ARRAY_BROKEN
  pageno_t pagebase;
#endif

static pageno_t firstpage, lastpage;

static char *saved_heap_ptr;
gcspace_t *space;
static gcspace_t *types, *pmap;
static pageno_t *linked;

static pageno_t current_pages, forwarded_pages;
static pageno_t protected_pages, allocated_pages;

static addrarith_t bytes_per_pp, pp_shift; /* bytes per physical page */
static addrarith_t hp_per_pp;      /* number of heap pages per physical page */
static addrarith_t pp_mask;        /* ANDed with a virtual address gives
                                    * base address of physical page
                                    */
static addrarith_t hp_per_pp_mask; /* ANDed with heap page number gives
                                    * first page number in the physical
                                    * page the heap page belongs to.
                                    */
#define SAME_PHYSPAGE(a,b) (((a) & pp_mask) == ((b) & pp_mask))

gcspace_t current_space; /* has to be exported because IS_ALIVE depends on it */

static gcspace_t forward_space, previous_space;
static pageno_t current_freepage, current_free;
static pageno_t forward_freepage, forward_free;
static pageno_t last_forward_freepage;

static Object *current_freep, *forward_freep;

static int scanning = 0; /* set to true if scanning a
                          * physical page is in progress */
static Object *scanpointer;
static Object *scanfirst, *scanlast;
#define IN_SCANREGION(addr) ((Object*)(addr) >= scanfirst \
                             && (Object*)(addr) <= scanlast)
#define IS_SCANNED(addr) ((Object *)(addr) < scanpointer)
#define MAXRESCAN 10
static pageno_t rescan[MAXRESCAN];
static int rescanpages = 0;
static int allscan = 0;

static pageno_t stable_queue, stable_tail; /* head and tail of the queue
                                            * of stable pages */

#define DIRTYENTRIES 20
struct dirty_rec {
    pageno_t pages[DIRTYENTRIES];
    struct dirty_rec *next;
};

static struct dirty_rec *dirtylist, *dirtyhead;
static int dirtyentries;

static int ScanCluster ();
static int Scanner ();
static void TerminateGC ();

/*****************************************************************************/

/* PAGEBYTES is defined in object.h */

#define PAGEWORDS      ((addrarith_t)(PAGEBYTES / sizeof (Object)))
#define HEAPPAGEMASK   ~((gcptr_t)PAGEBYTES-1)

#ifdef ALIGN_8BYTE
#  define MAX_OBJECTWORDS       (PAGEWORDS - 1)
#  define NEEDED_PAGES(size)    (((size) + PAGEWORDS) / PAGEWORDS)
#else
#  define MAX_OBJECTWORDS       PAGEWORDS
#  define NEEDED_PAGES(size)    (((size) + PAGEWORDS - 1) / PAGEWORDS)
#endif

#define MAKE_HEADER(obj,words,type)     (SET(obj, type, words))
#define HEADER_TO_TYPE(header)          ((unsigned int)TYPE(header))
#define HEADER_TO_WORDS(header)         ((unsigned int)FIXNUM(header))

/* some conversion stuff. PHYSPAGE converts a logical page number into the
 * start address of the physical page the logical page lies on.
 * If ARRAY_BROKEN is defined, page numbering will start at 0 for the
 * first heap page. Not that this will introduce some extra overhead.
 * Note that PAGE_TO_ADDR(0) == 0 if ARRAY_BROKEN is not defined...
 */

#define OBJ_TO_PPADDR(obj) ((gcptr_t)POINTER(obj) & pp_mask)
#define PTR_TO_PPADDR(ptr) ((gcptr_t)(ptr) & pp_mask)
#define ADDR_TO_PAGE(addr) ((((addr) & HEAPPAGEMASK) / PAGEBYTES) - pagebase)
#define PAGE_TO_ADDR(page) (((page) + pagebase) * PAGEBYTES)
#define PHYSPAGE(page)     ((((page) + pagebase) * PAGEBYTES) & pp_mask)

#define UNALLOCATED_PAGE   (gcspace_t)(-2)
#define FREE_PAGE          1

#define OBJECTPAGE         0
#define CONTPAGE           1

#define PERCENT(x, y)  (((x) * 100) / (y))
#define HEAPPERCENT(x)  PERCENT(x, logical_pages)

#define IS_CLUSTER(a,b) (SAME_PHYSPAGE (PAGE_TO_ADDR ((a)), \
                                        PAGE_TO_ADDR ((b))) || \
                         (space[a] == space[b] && \
                          types[(a)&hp_per_pp_mask] == OBJECTPAGE && \
                          types[((b)&hp_per_pp_mask)+hp_per_pp] == OBJECTPAGE))

/* check whether the (physical) page starting at address addr is protected
 * or not. SET_PROTECT and SET_UNPROTECT are used to set or clear the flag
 * for the page starting at address addr in the pmap array. The job of
 * protecting a page (by calling mprotect) is done in PROTECT/UNPROTECT.
 */

#define PMAP(addr)           pmap[((addr) - PAGE_TO_ADDR(0)) >> pp_shift]

#define IS_PROTECTED(addr)   ( PMAP (addr) )
#define SET_PROTECT(addr)    { PMAP (addr) = 1; protected_pages++; }
#define SET_UNPROTECT(addr)  { PMAP (addr) = 0; protected_pages--; }

#if defined(MPROTECT_SIG) || defined(MPROTECT_MMAP)
#  ifndef PROT_RW
#    define PROT_RW   (PROT_READ | PROT_WRITE)
#  endif
#  ifndef PROT_NONE
#    define PROT_NONE 0
#  endif
#  define MPROTECT(addr,len,prot) { if (inc_collection) \
                                        mprotect ((caddr_t)(addr), (len), \
                                                  (prot)); }
#else
#  define PROT_RW
#  define PROT_NONE
#  define MPROTECT(addr,len,prot)
#endif

#define PROTECT(addr)   { if (!IS_PROTECTED (addr)) {                         \
                              if (!scanning) {                                \
                                  SET_PROTECT (addr);                         \
                                  MPROTECT ((addr), bytes_per_pp, PROT_NONE); \
                              } else                                          \
                                  AddDirty ((addr));                          \
                          } }

#define UNPROTECT(addr) { if (IS_PROTECTED (addr)) {                          \
                              SET_UNPROTECT (addr);                           \
                              MPROTECT ((addr), bytes_per_pp, PROT_RW);       \
                          } }

/*****************************************************************************/

/* the following functions maintain a linked list to remember pages that
 * are "endangered" while scanning goes on. The list elements are arrays,
 * each one containing some page addresses. If an array is filled, a new
 * one is appended to the list (dynamically).
 * An address is not added to the list if the most recently added entry
 * is the same address. It is not necessary to add an address if it is in
 * the list anywhere, but searching would be too time-consuming.
 */

static void SetupDirtyList () {
    dirtylist = (struct dirty_rec *) malloc (sizeof (struct dirty_rec));
    if (dirtylist == (struct dirty_rec *)0)
        Fatal_Error ("SetupDirtyList: unable to allocate memory");
    memset (dirtylist->pages, 0, sizeof (dirtylist->pages));
    dirtylist->next = (struct dirty_rec *)0;
    dirtyhead = dirtylist;
    dirtyentries = 0;
}

static void AddDirty (pageno_t addr) {
    struct dirty_rec *p;

    if (dirtyentries != 0 &&
        dirtylist->pages[(dirtyentries-1) % DIRTYENTRIES] == addr)
            return;
    else
        dirtylist->pages[dirtyentries++ % DIRTYENTRIES] = addr;

    if (dirtyentries % DIRTYENTRIES == 0) {
        p = (struct dirty_rec *) malloc (sizeof (struct dirty_rec));
        if (p == (struct dirty_rec *)0)
            Fatal_Error ("AddDirty: unable to allocate memory");
        memset (p->pages, 0, sizeof (p->pages));
        p->next = (struct dirty_rec *)0;
        dirtylist->next = p;
        dirtylist = p;
    }
}

static void ReprotectDirty () {
    int i;

    dirtylist = dirtyhead;
    while (dirtylist) {
        for (i = 0; i < DIRTYENTRIES && dirtyentries--; i++)
            PROTECT (dirtylist->pages[i]);
        dirtylist = dirtylist->next;
    }

    dirtyentries = 0;
    dirtylist = dirtyhead;
    dirtylist->next = (struct dirty_rec *)0;
}


/* register a page which has been promoted into the scan region by the
 * Visit function. If that page has not been scanned yet, return, else
 * remember the page to be scanned later. If there is not enough space
 * to remember pages, set a flag to rescan the whole scan region.
 */

static void RegisterPage (pageno_t page) {
    if (allscan)
        return;

    if (IS_SCANNED (PAGE_TO_ADDR (page))) {
        if (rescanpages < MAXRESCAN)
            rescan[rescanpages++] = page;
        else
            allscan = 1;
    }
}

/* determine a physical page cluster. Search backward until the beginning
 * of the cluster is found, then forward until the length of the cluster
 * is determined. The first parameter is the address of the first physical
 * page in the cluster, the second one is the length in physical pages.
 * Note that these parameters are value-result parameters !
 */

static void DetermineCluster (gcptr_t *addr, int *len) {
    gcptr_t addr1;

    *len = 1;
    while (types[ADDR_TO_PAGE (*addr)] != OBJECTPAGE) {
        *addr -= bytes_per_pp;
        (*len)++;
    }
    addr1 = *addr + ((*len) << pp_shift);

    while (ADDR_TO_PAGE(addr1) <= lastpage &&
            space[ADDR_TO_PAGE(addr1)] > 0 &&
            types[ADDR_TO_PAGE(addr1)] != OBJECTPAGE) {
        addr1 += bytes_per_pp;
        (*len)++;
    }
}


/* the following two functions are used to protect or unprotect a page
 * cluster. The first parameter is the address of the first page of the
 * cluster, the second one is the length in physical pages. If the length
 * is 0, DetermineCluster is called to set length accordingly.
 */

static void ProtectCluster (gcptr_t addr, unsigned int len) {
    if (!len) DetermineCluster (&addr, &len);
    if (len > 1) {
        while (len) {
            if (!IS_PROTECTED (addr)) {
                MPROTECT (addr, len << pp_shift, PROT_NONE);
                break;
            }
            len--;
            addr += bytes_per_pp;
        }
        while (len--) {
            if (!IS_PROTECTED (addr)) SET_PROTECT (addr);
            addr += bytes_per_pp;
        }
    } else {
        if (!IS_PROTECTED (addr)) {
            MPROTECT (addr, bytes_per_pp, PROT_NONE);
            SET_PROTECT (addr);
        }
    }
}


static void UnprotectCluster (gcptr_t addr, unsigned int len) {
    if (!len) DetermineCluster (&addr, &len);
    MPROTECT (addr, len << pp_shift, PROT_RW);
    while (len--) {
        if (IS_PROTECTED (addr)) SET_UNPROTECT (addr);
        addr += bytes_per_pp;
    }
}


/* add one page to the stable set queue */

static void AddQueue (pageno_t page) {

    if (stable_queue != (pageno_t)-1)
        linked[stable_tail] = page;
    else
        stable_queue = page;
    linked[page] = (pageno_t)-1;
    stable_tail = page;
}


/* the following function promotes all heap pages in the stable set queue
 * into current space. After this, there are no more forwarded pages in the
 * heap.
 */

static void PromoteStableQueue () {
    Object *p;
    int pcount, size;
    pageno_t start;

    while (stable_queue != (pageno_t)-1) {
        p = PAGE_TO_OBJ (stable_queue);
#ifdef ALIGN_8BYTE
        p++;
#endif
        size = HEADER_TO_WORDS (*p);
        pcount = NEEDED_PAGES (size);

        start = stable_queue;
        while (pcount--)
            space[start++] = current_space;
        stable_queue = linked[stable_queue];
    }
    current_pages = allocated_pages;
    forwarded_pages = 0;
}

/* calculate the logarithm (base 2) for arguments == 2**n
 */

static int Logbase2 (addrarith_t psize) {
    int shift = 0;

#if LONG_BITS-64 == 0
    if (psize & 0xffffffff00000000) shift += 32;
    if (psize & 0xffff0000ffff0000) shift += 16;
    if (psize & 0xff00ff00ff00ff00) shift += 8;
    if (psize & 0xf0f0f0f0f0f0f0f0) shift += 4;
    if (psize & 0xcccccccccccccccc) shift += 2;
    if (psize & 0xaaaaaaaaaaaaaaaa) shift += 1;
#else
    if (psize & 0xffff0000) shift += 16;
    if (psize & 0xff00ff00) shift += 8;
    if (psize & 0xf0f0f0f0) shift += 4;
    if (psize & 0xcccccccc) shift += 2;
    if (psize & 0xaaaaaaaa) shift += 1;
#endif

    return (shift);
}

/* return next heap page number, wrap around at the end of the heap. */

static pageno_t next (pageno_t page) {
    return ((page < lastpage) ? page+1 : firstpage);
}

/*****************************************************************************/

#ifdef MPROTECT_MMAP

static char *heapmalloc (int s) {
    char *ret = mmap (0, s, PROT_READ|PROT_WRITE, MAP_ANON, -1, 0);

    if (ret == (char*)-1)
        ret = 0;

    return ret;
}

#else

#  define heapmalloc(size)  (char *)malloc ((size))

#endif

/*
 * make a heap of size kilobytes. It is divided into heappages of
 * PAGEBYTES byte and is aligned at a physical page boundary. The
 * heapsize is rounded up to the nearest multiple of the physical
 * pagesize. Checked by sam@hocevar.net on Apr 1, 2003.
 */

void Make_Heap (int size) {
    addrarith_t heapsize = size * 2 * 1024;
    char *heap_ptr, *aligned_heap_ptr;
    Object heap_obj;
    pageno_t i;

#if defined(MPROTECT_SIG) || defined(MPROTECT_MMAP)
    InstallHandler ();
#endif

    /* calculate number of logical heappages and of used physical pages.
     * First, round up to the nearest multiple of the physical pagesize,
     * then calculate the resulting number of heap pages.
     */

#if defined(_SC_PAGESIZE)
    if ((bytes_per_pp = sysconf (_SC_PAGESIZE)) == -1)
        Fatal_Error ("sysconf(_SC_PAGESIZE) failed; can't get pagesize");
#elif defined(HAVE_GETPAGESIZE)
    bytes_per_pp = getpagesize ();
#elif defined(MPROTECT_SIG) || defined(MPROTECT_MMAP)
#   error "mprotect requires getpagesize or sysconf_pagesize"
#else
    bytes_per_pp = 4096;
#endif
    physical_pages = (heapsize+bytes_per_pp-1)/bytes_per_pp;
    hp_per_pp = bytes_per_pp / PAGEBYTES;
    hp_per_pp_mask = ~(hp_per_pp - 1);
    logical_pages = spanning_pages = physical_pages * hp_per_pp;
    pp_mask = ~(bytes_per_pp-1);
    pp_shift = Logbase2 (bytes_per_pp);

    heap_ptr = heapmalloc (logical_pages*PAGEBYTES+bytes_per_pp-1);
    /* FIXME: add heap_ptr to a list of pointers to free */
    saved_heap_ptr = heap_ptr;

    if (heap_ptr == NULL)
        Fatal_Error ("cannot allocate heap (%u KBytes)", size);

    /* Align heap at a memory page boundary */

    if ((gcptr_t)heap_ptr & (bytes_per_pp-1))
        aligned_heap_ptr = (char*)(((gcptr_t)heap_ptr+bytes_per_pp-1)
            & ~(bytes_per_pp-1));
    else
        aligned_heap_ptr = heap_ptr;

    SET(heap_obj, 0, (intptr_t)aligned_heap_ptr);

#ifdef ARRAY_BROKEN
    pagebase = ((gcptr_t)POINTER (heap_obj)) / PAGEBYTES;
#endif
    firstpage = OBJ_TO_PAGE (heap_obj);
    lastpage = firstpage+logical_pages-1;

    space = (gcspace_t *)malloc (logical_pages*sizeof (gcspace_t));
    types = (gcspace_t *)malloc ((logical_pages + 1)*sizeof (gcspace_t));
    pmap = (gcspace_t *)malloc (physical_pages*sizeof (gcspace_t));
    linked = (pageno_t *)malloc (logical_pages*sizeof (pageno_t));
    if (!space || !types || !pmap || !linked) {
        free (heap_ptr);
        if (space) free ((char*)space);
        if (types) free ((char*)types);
        if (pmap) free ((char*)pmap);
        if (linked) free ((char*)linked);
        Fatal_Error ("cannot allocate heap maps");
    }

    memset (types, 0, (logical_pages + 1)*sizeof (gcspace_t));
    memset (pmap, 0, physical_pages*sizeof (gcspace_t));
    memset (linked, 0, logical_pages*sizeof (unsigned int));
    space -= firstpage; /* to index the arrays with the heap page number */
    types -= firstpage;
    types[lastpage+1] = OBJECTPAGE;
    linked -= firstpage;
#ifndef ARRAY_BROKEN
    pmap -= (PAGE_TO_ADDR (firstpage) >> pp_shift);
#endif

    for (i = firstpage; i <= lastpage; i++)
        space[i] = FREE_PAGE;

    allocated_pages = 0;
    forwarded_pages = 0;
    current_pages = 0;
    protected_pages = 0;
    stable_queue = (pageno_t)-1;
    SetupDirtyList ();

    current_space = forward_space = previous_space = 3;
    current_freepage = firstpage; current_free = 0;
}

/*
 * increment the heap by 1024 KB. Checked by sam@hocevar.net on Apr 1, 2003.
 */

static int ExpandHeap (char *reason) {
    int increment = (1024 * 1024 + bytes_per_pp - 1) / bytes_per_pp;
    int incpages = increment * hp_per_pp;
    addrarith_t heapinc = incpages * PAGEBYTES;
    pageno_t new_first, inc_first;
    pageno_t new_last, inc_last;
    pageno_t new_logpages, new_physpages;
    pageno_t new_spanpages;
    gcptr_t addr;
    gcspace_t *new_space, *new_type, *new_pmap;
    pageno_t *new_link, i;
    char *heap_ptr, *aligned_heap_ptr;
    Object heap_obj;
#ifdef ARRAY_BROKEN
    pageno_t new_pagebase, offset;
    pageno_t new_firstpage, new_lastpage;
#else
#   define offset 0
#endif

    /* FIXME: this pointer is lost */
    heap_ptr = heapmalloc (heapinc+bytes_per_pp/*-1*/);

    if (heap_ptr == NULL) {
        if (Var_Is_True (V_Garbage_Collect_Notifyp)) {
            char buf[243];
            sprintf(buf, "[Heap expansion failed (%s)]~%%", reason);
            Format (Standard_Output_Port, buf,
                    strlen(buf), 0, (Object *)0);
            (void)fflush (stdout);
        }
        return (0);
    }

    /* Align heap at a memory page boundary */

    if ((gcptr_t)heap_ptr & (bytes_per_pp-1))
        aligned_heap_ptr = (char*)(((gcptr_t)heap_ptr+bytes_per_pp-1)
            & ~(bytes_per_pp-1));
    else
        aligned_heap_ptr = heap_ptr;

    SET(heap_obj, 0, (intptr_t)aligned_heap_ptr);

    new_first = firstpage;
    new_last = lastpage;

#ifdef ARRAY_BROKEN
    new_pagebase = ((gcptr_t)POINTER (heap_obj)) / PAGEBYTES;
    inc_first = 0; /* = OBJ_TO_PAGE (heap_obj) - new_pagebase */

    new_firstpage = (pagebase > new_pagebase)
        ? new_pagebase : pagebase;

    new_lastpage = (pagebase > new_pagebase)
        ? pagebase + lastpage
        : new_pagebase + incpages - 1;

    offset = pagebase - new_firstpage;
#else
    inc_first = OBJ_TO_PAGE (heap_obj);
#endif

    inc_last = inc_first+incpages-1;
    if (inc_last > lastpage)
        new_last = inc_last;
    if (inc_first < firstpage)
        new_first = inc_first;
    new_logpages = logical_pages+incpages;
#ifdef ARRAY_BROKEN
    new_spanpages = new_lastpage-new_firstpage+1;
    new_last = new_spanpages-1;
#else
    new_spanpages = new_last-new_first+1;
#endif
    new_physpages = new_spanpages / hp_per_pp;

    new_space = (gcspace_t *)malloc (new_spanpages*sizeof (gcspace_t));
    new_type = (gcspace_t *)malloc ((new_spanpages + 1)*sizeof (gcspace_t));
    new_pmap = (gcspace_t *)malloc (new_physpages*sizeof (gcspace_t));
    new_link = (pageno_t *)malloc (new_spanpages*sizeof (pageno_t));
    if (!new_space || !new_type || !new_pmap || !new_link) {
        free (heap_ptr);
        if (new_space) free ((char*)new_space);
        if (new_type) free ((char*)new_type);
        if (new_pmap) free ((char*)new_pmap);
        if (new_link) free ((char*)new_link);
        if (Var_Is_True (V_Garbage_Collect_Notifyp)) {
            Format (Standard_Output_Port, "[Heap expansion failed]~%",
                    25, 0, (Object *)0);
            (void)fflush (stdout);
        }
        return (0);
    }


    /* new_first will be 0 if ARRAY_BROKEN is defined. */

    new_space -= new_first;
    new_type -= new_first;
    new_link -= new_first;

    memset (new_pmap, 0, new_physpages * sizeof (gcspace_t));
#ifndef ARRAY_BROKEN
    new_pmap -= (PHYSPAGE (new_first) >> pp_shift);
#endif

    memset (new_type+inc_first+offset, 0, (incpages+1)*sizeof (gcspace_t));
    memset (new_link+inc_first+offset, 0, incpages*sizeof (unsigned int));

    /* FIXME: memmove! */
    for (i = firstpage; i <= lastpage; i++) {
        new_link[i + offset] = linked[i] + offset;
        new_type[i + offset] = types[i];
    }
    for (addr = PAGE_TO_ADDR (firstpage); addr <= PAGE_TO_ADDR (lastpage);
         addr += bytes_per_pp) {
        new_pmap[((addr - PAGE_TO_ADDR(0)) >> pp_shift) + offset] =
            IS_PROTECTED (addr);
    }

#ifdef ARRAY_BROKEN
    for (i = 0; i < new_spanpages; i++) new_space[i] = UNALLOCATED_PAGE;
    for (i = firstpage; i <= lastpage; i++) new_space[i+offset] = space[i];
    offset = offset ? 0 : new_pagebase - pagebase;
    for (i = offset; i <= offset + inc_last; i++) new_space[i] = FREE_PAGE;
    new_type[new_spanpages] = OBJECTPAGE;
#else
    for (i = new_first; i < firstpage; i++) new_space[i] = UNALLOCATED_PAGE;
    for (i = firstpage; i <= lastpage; i++) new_space[i] = space[i];

    for (i = lastpage+1; i <= new_last; i++) new_space[i] = UNALLOCATED_PAGE;
    for (i = inc_first; i <= inc_last; i++) new_space[i] = FREE_PAGE;
    new_type[new_last+1] = OBJECTPAGE;
#endif

    current_freepage += offset;
    forward_freepage += offset;
    last_forward_freepage += offset;

    free ((char*)(linked+firstpage));
    free ((char*)(types+firstpage));
    free ((char*)(space+firstpage));

#ifndef ARRAY_BROKEN
    free ((char*)(pmap+(PAGE_TO_ADDR (firstpage) >> pp_shift)));
#else
    free ((char*)pmap);
#endif

    linked = new_link;
    types = new_type;
    space = new_space;
    pmap = new_pmap;
    firstpage = new_first;
    lastpage = new_last;
    logical_pages = new_logpages;
    spanning_pages = new_spanpages;
    physical_pages = new_physpages;

    if (Var_Is_True (V_Garbage_Collect_Notifyp)) {
        int a = (logical_pages * PAGEBYTES) >> 10;
        char buf[243];

        sprintf(buf, "[Heap expanded to %dK (%s)]~%%", a, reason);
        Format (Standard_Output_Port, buf, strlen(buf), 0, (Object *)0);
        (void)fflush (stdout);
    }
    return (1);
}


/*
 * free the heap.
 */

void Free_Heap () {
    free (saved_heap_ptr);

    free ((char*)(linked+firstpage));
    free ((char*)(types+firstpage));
    free ((char*)(space+firstpage));

#ifndef ARRAY_BROKEN
    free ((char*)(pmap+(PAGE_TO_ADDR (firstpage) >> pp_shift)));
#else
    free ((char*)pmap);
#endif
}

/* allocate new logical heappages. npg is the number of pages to allocate.
 * If there is not enough space left, the heap will be expanded if possible.
 * The new page is allocated in current space.
 */

static int ProtectedInRegion (pageno_t start, pageno_t npages) {
    gcptr_t beginpage = PHYSPAGE (start);
    gcptr_t endpage = PHYSPAGE (start+npages-1);

    do {
        if (IS_PROTECTED (beginpage))
            return (1);
        beginpage += bytes_per_pp;
    } while (beginpage <= endpage);

    return (0);
}

static void AllocPage (pageno_t npg) {
    pageno_t first_freepage = 0;/* first free heap page */
    pageno_t cont_free;         /* contiguous free pages */
    pageno_t n, p;

    if (current_space != forward_space) {
        (void)Scanner ((pageno_t)1);
        if (!protected_pages)
            TerminateGC ();
    } else {
        if (inc_collection) {
            if (allocated_pages+npg >= logical_pages/3)
                P_Collect_Incremental ();
        } else {
            if (allocated_pages+npg >= logical_pages/2)
                P_Collect ();
        }
    }

    /* now look for a cluster of npg free pages. cont_free counts the
     * number of free pages found, first_freepage is the number of the
     * first free heap page in the cluster. */
    for (p = spanning_pages, cont_free = 0; p; p--) {

        /* If we have more space than before, or if the current page is
         * stable, start again with the next page. */
        if (space[current_freepage] >= previous_space
             || STABLE (current_freepage)) {
            current_freepage = next (current_freepage);
            cont_free = 0;
            continue;
        }

        if (cont_free == 0) {
            /* This is our first free page, first check that we have a
             * continuous cluster of pages (we'll check later that they
             * are free). Otherwise, go to the next free page. */
            if ((current_freepage+npg-1) > lastpage
                || !IS_CLUSTER (current_freepage, current_freepage+npg-1)) {
                current_freepage = next ((current_freepage&hp_per_pp_mask)
                                          +hp_per_pp-1);
                continue;
            }

            first_freepage = current_freepage;
        }

        cont_free++;

        if (cont_free == npg) {
            space[first_freepage] = current_space;
            types[first_freepage] = OBJECTPAGE;
            for (n = 1; n < npg; n++) {
                space[first_freepage+n] = current_space;
                types[first_freepage+n] = CONTPAGE;
            }
            current_freep = PAGE_TO_OBJ (first_freepage);
            current_free = npg*PAGEWORDS;
            current_pages += npg;
            allocated_pages += npg;
            current_freepage = next (first_freepage+npg-1);
            if (ProtectedInRegion (first_freepage, npg))
                (void)ScanCluster (PHYSPAGE (first_freepage));
            return;
        }

        /* check the next free page. If we warped, reset cont_free to 0. */
        current_freepage = next (current_freepage);
        if (current_freepage == firstpage) cont_free = 0;
    }

    /* no space available, try to expand heap */

    if (ExpandHeap ("to allocate new object")) {
        AllocPage (npg);
        return;
    }

    Fatal_Error ("unable to allocate %lu bytes in heap", npg*PAGEBYTES);

    /*NOTREACHED*/
}


/* allocate an object in the heap. size is the size of the new object
 * in bytes, type describes the object's type (see object.h), and konst
 * determines whether the object is immutable.
 */

Object Alloc_Object (size, type, konst) {
    Object obj;
    register addrarith_t s = /* size in words */
        ((size + sizeof(Object) - 1) / sizeof(Object)) + 1;
    int big = 0;

    if (GC_Debug) {
        if (inc_collection)
            P_Collect_Incremental ();
        else
            P_Collect ();
    }

    /* if there is not enough space left on the current page, discard
     * the left space and allocate a new page. Space is discarded by
     * writing a T_Freespace object.
     */

    if (s > current_free) {
        if (current_free) {
            MAKE_HEADER (*current_freep, current_free, T_Freespace);
            current_free = 0;
        }

        /* If we are about to allocate an object bigger than one heap page,
         * set a flag. The space behind big objects is discarded, see below.
         */

#ifdef ALIGN_8BYTE
        if (s < PAGEWORDS-1)
            AllocPage ((pageno_t)1);
        else {
            AllocPage ((pageno_t)(s+PAGEWORDS)/PAGEWORDS);
            big = 1;
        }
        MAKE_HEADER (*current_freep, 1, T_Align_8Byte);
        current_freep++;
        current_free--;
#else
        if (s < PAGEWORDS)
            AllocPage ((pageno_t)1);
        else {
            AllocPage ((pageno_t)(s+PAGEWORDS-1)/PAGEWORDS);
            big = 1;
        }
#endif
    }

    /* now write a header for the object into the heap and update the
     * pointer to the next free location and the counter of free words
     * in the current heappage.
     */

    MAKE_HEADER (*current_freep, s, type);
    current_freep++;
    *current_freep = Null;
    SET (obj, type, (intptr_t)current_freep);
    if (big)
        current_freep = (Object*)0, current_free = 0;
    else
        current_freep += (s-1), current_free -= s;
#ifdef ALIGN_8BYTE
    if (!((gcptr_t)current_freep & 7) && current_free) {
        MAKE_HEADER (*current_freep, 1, T_Align_8Byte);
        current_freep++;
        current_free--;
    }
#endif
    if (type == T_Control_Point)
        CONTROL(obj)->reloc = 0;

    if (konst) SETCONST (obj);
    return (obj);
}


/* allocate a page in forward space. If there is no space left, the heap
 * is expanded. The argument prevents allocation of a heap page which lies
 * on the same physical page the referenced object lies on.
 */

static void AllocForwardPage (Object bad) {
    Object *badaddr = (Object *)POINTER (bad);
    pageno_t whole_heap = spanning_pages;
    pageno_t tpage;

    while (whole_heap--) {
        if (space[forward_freepage] < previous_space
            && !STABLE (forward_freepage)
            && !SAME_PHYSPAGE ((gcptr_t)badaddr,
                    PAGE_TO_ADDR (forward_freepage))
            && !IN_SCANREGION (PAGE_TO_ADDR (forward_freepage))) {

            allocated_pages++;
            forwarded_pages++;
            space[forward_freepage] = forward_space;
            types[forward_freepage] = OBJECTPAGE;
            forward_freep = PAGE_TO_OBJ (forward_freepage);
            forward_free = PAGEWORDS;
            AddQueue (forward_freepage);

            tpage = last_forward_freepage;
            last_forward_freepage = next (forward_freepage);
            forward_freepage = tpage;
            return;
        } else {
            forward_freepage = next (forward_freepage);
        }
    }

    if (ExpandHeap ("to allocate forward page")) {
        AllocForwardPage (bad);
        return;
    }

    Fatal_Error ("unable to allocate forward page in %lu KBytes heap",
                 (logical_pages * PAGEBYTES) >> 10);

    /*NOTREACHED*/
}


/* Visit an object and move it into forward space.  The forwarded
 * object must be protected because it is to be scanned later.
 */

int Visit (register Object *cp) {
    register pageno_t page = OBJ_TO_PAGE (*cp);
    register Object *obj_ptr = (Object *)POINTER (*cp);
    int tag = TYPE (*cp);
    int konst = ISCONST (*cp);
    addrarith_t objwords;
    pageno_t objpages, pcount;
    gcptr_t ffreep, pageaddr = 0;
    int outside;

    /* if the Visit function is called via the REVIVE_OBJ macro and we are
     * not inside an incremental collection, exit immediately.
     */

    if (current_space == forward_space)
        return 0;

    if (page < firstpage || page > lastpage || STABLE (page)
        || space[page] == current_space  || space[page] == UNALLOCATED_PAGE
        || !Types[tag].haspointer)
        return 0;

    if (space[page] != previous_space) {
        char buf[100];
        sprintf (buf, "Visit: object not in prev space at %p ('%s') %d %d",
            obj_ptr, Types[tag].name, space[page], previous_space);
        Panic (buf);
    }

    if (!IN_SCANREGION (obj_ptr) && IS_PROTECTED ((gcptr_t)obj_ptr)) {
        pageaddr = OBJ_TO_PPADDR (*cp);
        UNPROTECT (pageaddr);
    }

    if (WAS_FORWARDED (*cp)) {
        if (pageaddr != 0)
            PROTECT (pageaddr);
        MAKEOBJ (*cp, tag, (intptr_t)POINTER(*obj_ptr));
        if (konst)
            SETCONST (*cp);
        return 0;
    }

    ffreep = PTR_TO_PPADDR (forward_freep);
    outside = !IN_SCANREGION (forward_freep);
    objwords = HEADER_TO_WORDS (*(obj_ptr - 1));
    if (objwords >= forward_free) {
#ifdef ALIGN_8BYTE
        if (objwords >= PAGEWORDS - 1) {
            objpages = (objwords + PAGEWORDS) / PAGEWORDS;
#else
        if (objwords >= PAGEWORDS) {
            objpages = (objwords + PAGEWORDS - 1) / PAGEWORDS;
#endif
            forwarded_pages += objpages;
            for (pcount = 0; pcount < objpages; pcount++)
                space[page + pcount] = forward_space;
            AddQueue (page);
            if (IN_SCANREGION (PAGE_TO_ADDR (page)))
                RegisterPage (page);
            else
                ProtectCluster (PHYSPAGE (page), 0);

            if (pageaddr != 0)
                PROTECT (pageaddr);

            return 0;
        }

        if (forward_free) {
            if (outside && IS_PROTECTED (ffreep)
                && !SAME_PHYSPAGE ((gcptr_t)obj_ptr, ffreep)) {

                UNPROTECT (ffreep);
                MAKE_HEADER (*forward_freep, forward_free, T_Freespace);
                forward_free = 0;
                PROTECT (ffreep);
            } else {
                MAKE_HEADER (*forward_freep, forward_free, T_Freespace);
                forward_free = 0;
            }
        }

        AllocForwardPage (*cp);
        outside = !IN_SCANREGION (forward_freep);
        ffreep = PTR_TO_PPADDR (forward_freep); /* re-set ffreep ! */
#ifdef ALIGN_8BYTE
        if (outside && IS_PROTECTED (ffreep))
            UNPROTECT (ffreep);
        MAKE_HEADER (*forward_freep, 1, T_Align_8Byte);
        forward_freep++;
        forward_free--;
        goto do_forward;
#endif
    }

    if (outside && IS_PROTECTED (ffreep))
        UNPROTECT (ffreep);

#ifdef ALIGN_8BYTE
do_forward:
#endif
    if (tag == T_Control_Point) {
        CONTROL (*cp)->reloc =
            (char*)(forward_freep + 1) - (char*)obj_ptr;
    }

    MAKE_HEADER (*forward_freep, objwords, tag);
    forward_freep++;
    memcpy (forward_freep, obj_ptr, (objwords-1)*sizeof(Object));
    SET (*obj_ptr, T_Broken_Heart, (intptr_t)forward_freep);
    MAKEOBJ (*cp, tag, (intptr_t)forward_freep);
    if (konst)
        SETCONST (*cp);
    forward_freep += (objwords - 1);
    forward_free -= objwords;

#ifdef ALIGN_8BYTE
    if (!((gcptr_t)forward_freep & 7) && forward_free) {
        MAKE_HEADER (*forward_freep, 1, T_Align_8Byte);
        forward_freep++;
        forward_free--;
    }
#endif

    if (outside)
        PROTECT (ffreep);

    if (pageaddr != 0)
        PROTECT (pageaddr);

    return 0;
}


/* Scan a page and visit all objects referenced by objects lying on the
 * page. This will possibly forward the referenced objects.
 */

static void ScanPage (Object *currentp, Object *nextcp) {
    Object *cp = currentp, obj;
    addrarith_t len, m, n;
    int t;

    while (cp < nextcp && (cp != forward_freep || forward_free == 0)) {
        t = HEADER_TO_TYPE (*cp);
        len = HEADER_TO_WORDS (*cp);
        cp++;

        /* cp now points to the real Scheme object in the heap. t denotes
         * the type of the object, len its length inclusive header in
         * words.
         */

        SET(obj, t, (intptr_t)cp);

        switch (t) {
        case T_Symbol:
            Visit (&SYMBOL(obj)->next);
            Visit (&SYMBOL(obj)->name);
            Visit (&SYMBOL(obj)->value);
            Visit (&SYMBOL(obj)->plist);
            break;

        case T_Pair:
        case T_Environment:
            Visit (&PAIR(obj)->car);
            Visit (&PAIR(obj)->cdr);
            break;

        case T_Vector:
            for (n = 0, m = VECTOR(obj)->size; n < m; n++ )
                Visit (&VECTOR(obj)->data[n]);
            break;

        case T_Compound:
            Visit (&COMPOUND(obj)->closure);
            Visit (&COMPOUND(obj)->env);
            Visit (&COMPOUND(obj)->name);
            break;

        case T_Control_Point:
            (CONTROL(obj)->delta) += CONTROL(obj)->reloc;

#ifdef HAVE_ALLOCA
            Visit_GC_List (CONTROL(obj)->gclist, CONTROL(obj)->delta);
#else
            Visit (&CONTROL(obj)->gcsave);
#endif
            Visit_Wind (CONTROL(obj)->firstwind,
                        (CONTROL(obj)->delta) );

            Visit (&CONTROL(obj)->env);
            break;

        case T_Promise:
            Visit (&PROMISE(obj)->env);
            Visit (&PROMISE(obj)->thunk);
            break;

        case T_Port:
            Visit (&PORT(obj)->name);
            break;

        case T_Autoload:
            Visit (&AUTOLOAD(obj)->files);
            Visit (&AUTOLOAD(obj)->env);
            break;

        case T_Macro:
            Visit (&MACRO(obj)->body);
            Visit (&MACRO(obj)->name);
            break;

        default:
            if (Types[t].visit)
                (Types[t].visit) (&obj, Visit);
        }
        cp += (len - 1);
    }
}


/* rescan all pages remembered by the RegisterPage function. */

static void RescanPages () {
    register Object *cp;
    register int i;
    int pages = rescanpages;

    rescanpages = 0;
    for (i = 0; i < pages; i++) {
        cp = PAGE_TO_OBJ (rescan[i]);
#ifdef ALIGN_8BYTE
        ScanPage (cp + 1, cp + PAGEWORDS);
#else
        ScanPage (cp, cp + PAGEWORDS);
#endif
    }
}

static int ScanCluster (gcptr_t addr) {
    register pageno_t page, last;
    pageno_t npages;
    int n = 0;

    scanning = 1;
    DetermineCluster (&addr, &n);
    npages = n;
    scanfirst = (Object *)addr;
    scanlast = (Object *)(addr + (npages << pp_shift) - sizeof (Object));
    UnprotectCluster ((gcptr_t)scanfirst, (int)npages);

 rescan_cluster:
    last = ADDR_TO_PAGE ((gcptr_t)scanlast);
    for (page = ADDR_TO_PAGE ((gcptr_t)scanfirst); page <= last; page++) {
        if (STABLE (page) && types[page] == OBJECTPAGE) {
            scanpointer = PAGE_TO_OBJ (page);
#ifdef ALIGN_8BYTE
            ScanPage (scanpointer + 1, scanpointer + PAGEWORDS);
#else
            ScanPage (scanpointer, scanpointer + PAGEWORDS);
#endif
        }
    }

    while (rescanpages) {
        if (allscan) {
            allscan = 0;
            goto rescan_cluster;
        } else
            RescanPages ();
    }

    scanfirst = (Object *)0;
    scanlast = (Object *)0;
    scanning = 0;
    ReprotectDirty ();

    return (npages); /* return number of scanned pages */
}


static int Scanner (pageno_t npages) {
    register gcptr_t addr, lastaddr;
    pageno_t spages;
    pageno_t scanned = 0;

    while (npages > 0 && protected_pages) {
        lastaddr = PAGE_TO_ADDR (lastpage);
        for (addr = PAGE_TO_ADDR(firstpage); addr < lastaddr && npages > 0;
             addr += bytes_per_pp) {

            if (IS_PROTECTED (addr)) {
                if (space[ADDR_TO_PAGE (addr)] == UNALLOCATED_PAGE)
                    Panic ("Scanner: found incorrect heap page");
                spages = ScanCluster (addr);
                scanned += spages;
                npages -= spages;
            }
        }
    }

    scanfirst = (Object *)0;
    scanlast = scanfirst;

    return (scanned);
}

#if defined(MPROTECT_SIG) || defined(MPROTECT_MMAP)
/* the following function handles a page fault. If the fault was caused
 * by the mutator and incremental collection is enabled, this will result
 * in scanning the physical page the fault occured on.
 */

#ifdef SIGSEGV_SIGCONTEXT

static void PagefaultHandler (int sig, int code, struct sigcontext *scp) {
    char *addr = (char *)(scp->sc_badvaddr);

#else
#ifdef SIGSEGV_AIX

static void PagefaultHandler (int sig, int code, struct sigcontext *scp) {
    char *addr = (char *)scp->sc_jmpbuf.jmp_context.except[3];
    /*
     * Or should that be .jmp_context.o_vaddr?
     */

#else
#ifdef SIGSEGV_SIGINFO

static void PagefaultHandler (int sig, siginfo_t *sip, ucontext_t *ucp) {
    char *addr;

#else
#ifdef SIGSEGV_ARG4

static void PagefaultHandler (int sig, int code, struct sigcontext *scp,
    char *addr) {

#else
#ifdef SIGSEGV_HPUX

static void PagefaultHandler (int sig, int code, struct sigcontext *scp) {

#else
#  include "HAVE_MPROTECT defined, but missing SIGSEGV_xxx"
#endif
#endif
#endif
#endif
#endif

    pageno_t page;
    gcptr_t ppage;
    char *errmsg = 0;

#ifdef SIGSEGV_AIX
    if ((char *)scp->sc_jmpbuf.jmp_context.except[0] != addr)
        Panic ("except");
#endif

#ifdef SIGSEGV_SIGINFO
    if (sip == 0)
        Fatal_Error ("SIGSEGV handler got called with zero siginfo_t");
    addr = sip->si_addr;
#endif

#ifdef SIGSEGV_HPUX
    char *addr;

    if (scp == 0)
        Fatal_Error ("SIGSEGV handler got called with zero sigcontext");
    addr = (char *)scp->sc_sl.sl_ss.ss_cr21;
#endif

    ppage = PTR_TO_PPADDR(addr);
    page = ADDR_TO_PAGE((gcptr_t)addr);

    if (!inc_collection)
        errmsg = "SIGSEGV signal received";
    else if (current_space == forward_space)
        errmsg = "SIGSEGV signal received while not garbage collecting";
    else if (page < firstpage || page > lastpage)
        errmsg = "SIGSEV signal received; address outside of heap";
    if (errmsg) {
        fprintf (stderr, "\n[%s]\n", errmsg);
        abort ();
    }

    GC_In_Progress = 1;
    (void)ScanCluster (ppage);
    GC_In_Progress = 0;
#ifdef SIGSEGV_AIX
    InstallHandler ();
#endif
    return;
}

void InstallHandler () {
#ifdef SIGSEGV_SIGINFO
    struct sigaction sact;
    sigset_t mask;

    sact.sa_handler = (void (*)())PagefaultHandler;
    sigemptyset (&mask);
    sact.sa_mask = mask;
    sact.sa_flags = SA_SIGINFO;
    if (sigaction (SIGSEGV, &sact, 0) == -1) {
        perror ("sigaction"); exit (1);
    }
#else
    (void)signal (SIGSEGV, (void (*)())PagefaultHandler);
#endif
}
#endif

static void TerminateGC () {
    int save_force_total;

    forward_space = current_space;
    previous_space = current_space;

    if (protected_pages)
        Panic ("TerminateGC: protected pages after collection");

    allocated_pages = current_pages + forwarded_pages;
    current_pages = 0;

    if (forward_free) {
        MAKE_HEADER (*forward_freep, forward_free, T_Freespace);
        forward_free = 0;
    }
    forward_freep = (Object *)0;

    Call_After_GC();
    GC_In_Progress = 0;
    Enable_Interrupts;

    if (Var_Is_True (V_Garbage_Collect_Notifyp) && !GC_Debug) {
        int foo = percent - HEAPPERCENT (allocated_pages);
        Object bar;

        bar = Make_Integer (foo);
        if (!incomplete_msg)
            Format (Standard_Output_Port, "[", 1, 0, (Object *)0);

        if (foo >= 0)
            Format (Standard_Output_Port, "~s% reclaimed]~%", 16, 1, &bar);
        else
            Format (Standard_Output_Port, "finished]~%", 11, 0, (Object *)0);
        (void)fflush (stdout);
        incomplete_msg = 0;
    }

    if (PERCENT (allocated_pages, old_logical_pages) >= tuneable_force_total) {
        PromoteStableQueue ();
        save_force_total = tuneable_force_total;
        tuneable_force_total = 100;
        if (inc_collection)
            P_Collect_Incremental ();
        else
            P_Collect ();
        tuneable_force_total = save_force_total;
        if (HEAPPERCENT (allocated_pages) >= tuneable_newly_expand)
            /* return value should not be ignore here: */
            (void)ExpandHeap ("after full collection");
    }
}


static void Finish_Collection () {
    register gcptr_t addr;

    do {
        for (addr = PAGE_TO_ADDR(firstpage);
             addr < PAGE_TO_ADDR(lastpage);
             addr += bytes_per_pp) {

            if (IS_PROTECTED (addr)) {
                (void)ScanCluster (addr);
                if (protected_pages == 0) TerminateGC ();
            }
        }
    } while (protected_pages);

    return;
}


static void General_Collect (int initiate) {
    pageno_t fpage, free_fpages, i;
    pageno_t page;
    pageno_t fregion_pages;
    Object obj;

    if (!Interpreter_Initialized)
        Fatal_Error ("Out of heap space (increase heap size)");

    if (current_space != forward_space && !inc_collection) {
        Format (Standard_Output_Port, "GC while GC in progress~%",
                25, 0, (Object*)0);
        return;
    }

    /* Call all user-registered functions to be executed just before GC. */

    Disable_Interrupts;
    GC_In_Progress = 1;
    Call_Before_GC();
    percent = HEAPPERCENT (allocated_pages);
    old_logical_pages = logical_pages;

    if (Var_Is_True (V_Garbage_Collect_Notifyp) && !GC_Debug) {
        if (initiate) {
            Format (Standard_Output_Port, "[Garbage collecting...]~%",
                    25, 0, (Object *)0);
            incomplete_msg = 0;
        } else {
            Format (Standard_Output_Port, "[Garbage collecting... ",
                    23, 0, (Object *)0);
            incomplete_msg = 1;
        }
        (void)fflush (stdout);
    }

    if (GC_Debug) {
        printf ("."); (void)fflush (stdout);
    }

    /* discard any remaining portion of the current heap page */

    if (current_free) {
        MAKE_HEADER (*current_freep, current_free, T_Freespace);
        current_free = 0;
    }

    /* partition regions for forwarded and newly-allocated objects. Then
     * advance the current free pointer so that - if possible - there will
     * be RESERVEDPAGES free heap pages in the forward region.
     */

    forward_freepage = current_freepage;
    last_forward_freepage = forward_freepage;

    current_freep = PAGE_TO_OBJ (current_freepage);
    forward_freep = current_freep;

    fpage = forward_freepage;
    free_fpages = 0;
    fregion_pages = logical_pages / tuneable_forward_region;

    for (i = 0; free_fpages <= fregion_pages && i < spanning_pages; i++) {
        if (space[fpage] != current_space && !STABLE (fpage))
            free_fpages++;
        fpage = next (fpage);
    }
    current_freep = (Object *)PHYSPAGE (fpage);
    SET(obj, 0, (intptr_t)current_freep);
    current_freepage = OBJ_TO_PAGE (obj);

    /* advance spaces. Then forward all objects directly accessible
     * via the global GC lists and the WIND list.
     */

    current_pages = 0;
    forward_space = current_space + 1;
    current_space = current_space + 2;

    Visit_GC_List (Global_GC_Obj, 0);
    Visit_GC_List (GC_List, 0);
    Visit_Wind (First_Wind, 0);

    /* If collecting in a non-incremental manner, scan all heap pages which
     * have been protected, else check whether to expand the heap because
     * the stable set has grown too big.
     */

    page = stable_queue;
    while (page != (pageno_t)-1) {
        ProtectCluster (PHYSPAGE (page), 0);
        page = linked[page];
    }

    if (!initiate) {
        Finish_Collection ();
    } else
        if (HEAPPERCENT (forwarded_pages) > tuneable_force_expand)
            /* return value should not be ignored here: */
            (void)ExpandHeap ("large stable set");

    GC_In_Progress = 0;
    return;
}


Object P_Collect_Incremental () {
    /* if already collecting, scan a few pages and return */

    if (!inc_collection) {
        if (current_space == forward_space)
            Primitive_Error ("incremental garbage collection not enabled");
        else {
            inc_collection = 1;
            Finish_Collection ();
            inc_collection = 0;
            return (True);
        }
    } else {
        if (current_space != forward_space) {
            (void)Scanner ((pageno_t)1);
            GC_In_Progress = 0;
            if (protected_pages == 0)
                TerminateGC ();
            return (protected_pages ? False : True);
        } else {
            General_Collect (1);
            return (False);
        }
    }
    /*NOTREACHED*/
}

Object P_Collect () {
    /* Check the inc_collection flag. If an incremental GC is in
     * progress and the flag has been changed to false, finish
     * the collection.
     */

    if (!inc_collection && current_space != forward_space) {
        inc_collection = 1;
        Finish_Collection ();
        inc_collection = 0;
        return (Void);
    }

    if (current_space != forward_space) {
        Finish_Collection ();
        return (Void);
    } else {
        General_Collect (0);
        return (Void);
    }
}

void Generational_GC_Finalize () {
    if (current_space != forward_space)
        Finish_Collection ();
}

void Generational_GC_Reinitialize () {
#if defined(MPROTECT_SIG) || defined(MPROTECT_MMAP)
    InstallHandler ();
#endif
}


Object Internal_GC_Status (int strat, int flags) {
    Object list;
#if defined(MPROTECT_SIG) || defined(MPROTECT_MMAP)
    Object cell;
#endif
    GC_Node;

    list = Cons (Sym_Generational_GC, Null);
    GC_Link (list);
    switch (strat) {
    default:            /* query or stop-and-copy */
#if defined(MPROTECT_SIG) || defined(MPROTECT_MMAP)
        if (inc_collection) {
            cell = Cons (Sym_Incremental_GC, Null);
            (void)P_Set_Cdr (list, cell);
        }
#endif
        break;
    case GC_STRAT_GEN:
        if (flags == GC_FLAGS_INCR) {
#if defined(MPROTECT_SIG) || defined(MPROTECT_MMAP)
            inc_collection = 1;
            cell = Cons (Sym_Incremental_GC, Null);
            (void)P_Set_Cdr (list, cell);
#endif
        } else inc_collection = 0;
        break;
    }
    GC_Unlink;
    return (list);
}
