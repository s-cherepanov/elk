/* dump-hp9k.c: Dump for the HP-PA. It needs some work; for instance, it
 * currently assumes that the data space is the last space in the a.out
 * file. If it weren't the last space, the code would have to adjust
 * pointers (such as header.space_location) that point into spaces beyond
 * the data space, as the data space in the new a.out is larger than that
 * in the original a.out.
 *
 * Also, it is unclear how the checksum field in the a.out header has
 * to be computed.
 *
 * An a.out file must not have holes in HP-UX (or exec(2) would complain
 * about an invalid data segment), therefore we cannot lseek over the
 * ununsed parts of the heap.
 *
 * The code to support dump with a dynamically linked a.out is a hack.
 * I have no idea why it works and if it will continue to work in
 * newer OS releases.
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

#include AOUT_H

#define copy(from,to,size) {\
    char buf[4096];\
    unsigned int len = (size), n;\
    \
    while (len > 0) {\
        if ((n = read (from, buf, 4096)) == -1) {\
            Dump_Finalize;\
            Primitive_Error ("error reading old a.out: ~E");\
        }\
        if (write (to, buf, n) == -1) {\
            Dump_Finalize;\
            Primitive_Error ("error writing new a.out: ~E");\
        }\
        len -= n;\
    }\
}

Object P_Dump (Object ofile) {
    struct header hdr;
    struct som_exec_auxhdr auxhdr;
    unsigned int data_size;
    int delta;
    struct stat stat;
    extern void *sbrk();

    Dump_Prolog;

    /* Read a.out header and first aux header
     */
    if (read (afd, (char *)&hdr, sizeof (hdr)) != sizeof (hdr) ||
            lseek (afd, (off_t)hdr.aux_header_location, SEEK_SET) == -1 ||
            read (afd, (char *)&auxhdr, sizeof (auxhdr)) != sizeof (auxhdr)) {
        Dump_Finalize;
        Primitive_Error ("can't read a.out headers");
    }
    if (hdr.a_magic != EXEC_MAGIC && hdr.a_magic != SHARE_MAGIC &&
            hdr.a_magic != DEMAND_MAGIC) {
        Dump_Finalize;
        Primitive_Error ("bad magic number ~s in a.out",
            Make_Integer (hdr.a_magic));
    }
    if (auxhdr.som_auxhdr.type != HPUX_AUX_ID) {
        Dump_Finalize;
        Primitive_Error ("bad aux header id ~s in a.out",
            Make_Integer (auxhdr.som_auxhdr.type));
    }

    /* Copy old file up to beginning of data space
     */
    (void)lseek (afd, (off_t)0, SEEK_SET);
    copy (afd, ofd, auxhdr.exec_dfile);

#ifdef HPSHLIB
    /* Save data segments of shared libraries
     */
    Save_Shared_Data ();
#endif

    /* Write data space (doesn't skip holes in heap yet)
     */
    Was_Dumped = 1;
    Brk_On_Dump = sbrk (0);
    data_size = Brk_On_Dump - (char *)auxhdr.exec_dmem;
    if (write (ofd, (char *)auxhdr.exec_dmem, data_size) != data_size) {
        Dump_Finalize;
        Primitive_Error ("error writing data space: ~E");
    }

    /* Check if data space was last space in a.out file.
     * Should not just quit, but adjust all pointers that point
     * beyond end of data space
     */
    (void)fstat (afd, &stat);
    if (lseek (afd, (off_t)auxhdr.exec_dsize, SEEK_CUR) != stat.st_size)
        Primitive_Error ("$DATA$ not last space in a.out file");

    /* Write new headers.
     * Do we have to recalculate the checksum?  The manual doesn't
     * say how the checksum is calculated.
     */
    delta = data_size - auxhdr.exec_dsize;
    hdr.som_length += delta;
    auxhdr.exec_dsize = data_size;
    auxhdr.exec_bsize = 0;
    (void)lseek (ofd, (off_t)0, SEEK_SET);
    if (write (ofd, (char *)&hdr, sizeof (hdr)) == -1 ||
            lseek (ofd, (off_t)hdr.aux_header_location, SEEK_SET) == -1 ||
            write (ofd, (char *)&auxhdr, sizeof (auxhdr)) == -1) {
        Dump_Finalize;
        Primitive_Error ("error writing a.out headers: ~E");
    }

    Dump_Epilog;
}

#ifdef HPSHLIB

/* Save and restore data segments of shared libraries.
 *
 * When the running program is dumped, we copy the data segment of
 * each shared library into a malloced area so that it gets saved
 * into the newly created a.out.
 *
 * On startup of the new a.out, we have to restore the data segments.
 * To do so, we first have to re-load all libraries that were present
 * in the old a.out when dump was called.
 * As the libraries may now get mapped to different locations, we
 * call mmap with an anonymous file to make the memory at the old
 * locations accessible again.
 */

#include <dl.h>
#include <sys/mman.h>

#define MAX_SHARED 1024

struct shared_data {
    struct shl_descriptor desc;
    char *oldaddr;                /* Start of data segment */
    char *saved;                  /* Saved contents of data segment */
} shared_data[MAX_SHARED], *lsp = shared_data;

#ifdef DEBUG_DUMP
   static char Z[1024];
#  define W write(1,Z,strlen(Z))
#endif

Save_Shared_Data () {
    struct shl_descriptor *p;
    struct shared_data *sp;
    int i;

    /* Assumptions:   1st shared library has index 1;
     *                we can stop as soon as shl_get fails
     */
    for (i = 1; shl_get (i, &p) != -1 ; i++) {
#ifdef DEBUG_DUMP
        sprintf (Z, "Saving shared lib %s\n", p->filename); W;
#endif
        for (sp = shared_data; sp != lsp &&
                strcmp (sp->desc.filename, p->filename) != 0; sp++)
            ;
        if (sp == lsp) {
#ifdef DEBUG_DUMP
            sprintf (Z, "   (new library)\n"); W;
#endif
            if (sp == shared_data + MAX_SHARED)
                Primitive_Error ("too many shared libraries");
            lsp++;
            sp->desc = *p;
            sp->saved = Safe_Malloc (p->dend - p->dstart);
            sp->oldaddr = (char *)p->dstart;
        }
    }
    for (sp = shared_data; sp != lsp; sp++) {
#ifdef DEBUG_DUMP
        sprintf (Z, "   copy data seg from %x to %x len %d\n",
            sp->oldaddr, sp->saved, sp->desc.dend - sp->desc.dstart); W;
#endif
        memcpy (sp->saved, sp->oldaddr, sp->desc.dend - sp->desc.dstart);
    }
}

Restore_Shared_Data () {
    struct shared_data *sp;
    struct shl_descriptor *p;
    shl_t tmp;

    for (sp = shared_data; sp != lsp; sp++) {
        /*
         * Assumption:  libraries whose names start with /lib/ or
         * with /usr/lib/ were present in the original a.out and
         * need not be re-loaded
         */
#ifdef DEBUG_DUMP
        sprintf (Z, "Restoring shared lib %s\n", sp->desc.filename); W;
#endif
        if (strncmp (sp->desc.filename, "/lib/", 5) != 0 &&
                strncmp (sp->desc.filename, "/usr/lib/", 8) != 0) {
            /*
             * Re-load the library and make sure memory locations
             * at the old start of data segment are mapped
             */
#ifdef DEBUG_DUMP
            sprintf (Z, "   (re-loading)\n"); W;
#endif
            tmp = shl_load (sp->desc.filename, BIND_IMMEDIATE|BIND_VERBOSE, 0L);
            if (tmp == 0)
                exit (1);   /* There's nothing we can do... */
            (void)shl_gethandle (tmp, &p);
            sp->desc = *p;

            /* Try to mnumap the region in any case.  If MAP_REPLACE is
             * there, use it.
             */
            (void)munmap (sp->oldaddr, sp->desc.dend - sp->desc.dstart);
#ifndef MAP_REPLACE
#  define MAP_REPLACE 0
#endif
            if (mmap (sp->oldaddr, sp->desc.dend - sp->desc.dstart,
                PROT_READ|PROT_WRITE,
                MAP_REPLACE|MAP_PRIVATE|MAP_ANONYMOUS|MAP_FIXED,
                -1, 0) == (char *)-1) {
                    sprintf (Z, "mmap failed[%d]\n", errno); W;
                    exit (1);
            }
        }
#ifdef DEBUG_DUMP
        sprintf (Z, "   copy data seg from %x to %x len %d\n", sp->saved,
            sp->oldaddr, sp->desc.dend-sp->desc.dstart); W;
#endif
        memcpy (sp->oldaddr, sp->saved, sp->desc.dend - sp->desc.dstart);
        /*
         * Initial break must be set as soon as data segment of
         * C library is restored
         */
        if (strcmp (sp->desc.filename, "/lib/libc.sl") == 0)
            (void)brk (Brk_On_Dump);
    }
}
#endif /* HPSHLIB */
