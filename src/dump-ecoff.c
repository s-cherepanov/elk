/* dump-ecoff.c
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

#include <fcntl.h>
#include <sys/types.h>
#include <filehdr.h>
#include <aouthdr.h>
#include <scnhdr.h>
#include <sym.h>

extern char *sbrk();

/* Find section header of section with given name.
 */
#define FIND_SECTHDR(name,ptr) {\
    char err[100];\
    int _i;\
    for (_i = 0; _i < fhdr.f_nscns; _i++)\
        if (strncmp (sect[_i].s_name, (name), sizeof(sect[_i].s_name)) == 0)\
            break;\
    if (_i == fhdr.f_nscns) {\
        Dump_Finalize;\
        sprintf (err, "running a.out doesn't have %s section", (name));\
        Primitive_Error (err);\
    }\
    (ptr) = sect+_i;\
}

#define MAX_SECTS  20

Object P_Dump (Object ofile) {
    struct filehdr fhdr;
    struct aouthdr ahdr;
    struct scnhdr sect[MAX_SECTS];
    struct scnhdr *sp, *datap;
    unsigned long int data_start, data_end, delta;
    int mask, n;
    HDRR shdr;
    char buf[4096];

    Dump_Prolog;

    /* Read file header, optional header, and section headers from
     * running a.out; locate .data section.
     * Reading the headers is not really necessary, as they get
     * mapped into the address space on startup.
     * However, we do not know where exactly they get mapped and
     * whether they haven't been modified.
     */
    if (read (afd, (char *)&fhdr, sizeof (fhdr)) != sizeof (fhdr) ||
            read (afd, (char *)&ahdr, sizeof (ahdr)) != sizeof (ahdr)) {
        Dump_Finalize;
        Primitive_Error ("error reading a.out headers: ~E");
    }
    if (fhdr.f_nscns > MAX_SECTS) {
        Dump_Finalize;
        Primitive_Error ("too many sections in a.out");
    }
    if (read (afd, (char *)sect, fhdr.f_nscns * sizeof (sect[0])) !=
            fhdr.f_nscns * sizeof (sect[0])) {
        Dump_Finalize;
        Primitive_Error ("error reading section headers: ~E");
    }
    FIND_SECTHDR (_DATA, datap);

    /* Adjust optional header and size of data segment
     */
    data_start = datap->s_vaddr;
    mask = getpagesize () - 1;
    data_end = (unsigned long int)sbrk (0) + mask & ~mask;
    delta = data_end - data_start - datap->s_size;

    ahdr.dsize = data_end - ahdr.data_start;
    ahdr.bsize = 0;
    ahdr.bss_start = ahdr.data_start + ahdr.dsize;
    datap->s_size += delta;

    /* Deactivate sections that aren't really needed (such as bss).
     * Actually, the section type should be set to STYP_DSECT (dummy section),
     * but this causes the linker to complain next time an object file is
     * loaded.
     * Adjust offsets in section headers of all other sections (such as
     * .comment).  (XXX: Should s_lnnoptr be adjusted as well?)
     */
    for (sp = datap+1; sp < sect+fhdr.f_nscns; sp++) {
        switch (sp->s_flags & ~0xf) {
        case STYP_BSS:
        case STYP_SBSS:
        case STYP_LIT4:
        case STYP_LIT8:
        case STYP_SDATA:
#ifdef DEBUG_DUMP
            /* .comment is not null-terminated */
            printf ("nuking %.8s\n", sp->s_name);
#endif
            sp->s_paddr = sp->s_vaddr = sp->s_scnptr = sp->s_lnnoptr = 0;
            sp->s_size = 0;
            break;
        default:
#ifdef DEBUG_DUMP
            printf ("adjusting %.8s\n", sp->s_name);
#endif
            sp->s_paddr += delta;
            sp->s_vaddr += delta;
            if (sp->s_scnptr) sp->s_scnptr += delta;
        }
    }
    delta = ahdr.tsize + ahdr.dsize - fhdr.f_symptr;
    fhdr.f_symptr += delta;

    /* Write headers
     */
    n = fhdr.f_nscns * sizeof (sect[0]);
    if (write (ofd, (char *)&fhdr, sizeof (fhdr)) != sizeof (fhdr) ||
            write (ofd, (char *)&ahdr, sizeof (ahdr)) != sizeof (ahdr) ||
            write (ofd, (char *)sect, n) != n) {
        Dump_Finalize;
        Primitive_Error ("error writing a.out/section headers: ~E");
    }

    /* Write sections
     */
    Was_Dumped = 1;
    n += sizeof (fhdr) + sizeof (ahdr);
#ifdef DEBUG_DUMP
    printf ("writing text 0x%x bytes and data 0x%x bytes\n",
        ahdr.tsize-n, ahdr.dsize);
#endif
    if (write (ofd, (char *)ahdr.text_start+n, ahdr.tsize-n) != ahdr.tsize-n) {
        Dump_Finalize;
        Primitive_Error ("error writing text section: ~E");
    }
    if (write (ofd, (char *)ahdr.data_start, ahdr.dsize) != ahdr.dsize) {
        Dump_Finalize;
        Primitive_Error ("error writing data sections: ~E");
    }

    /* Copy the symbol table.  If an object file has been loaded into
     * this invocation, copy the symbols from the ld temp file,
     * otherwise from the running a.out.
     * Adjust various offsets in the symbolic header.
     * (XXX: Are there any offsets to be adjusted in the table proper?)
     */
    if (Loader_Input) {
        close (afd);
        if ((afd = open (Loader_Input, O_RDONLY)) == -1) {
            Dump_Finalize;
            Primitive_Error ("cannot open symbol table file: ~E");
        }
        delta = fhdr.f_symptr;
        if (read (afd, (char *)&fhdr, sizeof (fhdr)) != sizeof (fhdr)) {
            Dump_Finalize;
            Primitive_Error ("error reading a.out header: ~E");
        }
        delta -= fhdr.f_symptr;
        (void)lseek (afd, (off_t)fhdr.f_symptr, SEEK_SET);
    } else
        (void)lseek (afd, (off_t)fhdr.f_symptr-delta, SEEK_SET);

#ifdef DEBUG_DUMP
    printf ("copying symbols from %s\n", Loader_Input ? Loader_Input :
        A_Out_Name);
#endif
    if (read (afd, (char *)&shdr, sizeof (shdr)) != sizeof (shdr)) {
symrerr:
        Dump_Finalize;
        Primitive_Error ("error reading symbol table: ~E");
    }
#define ADJUST(what) if (shdr.what > 0) shdr.what += delta
    ADJUST (cbLineOffset);
    ADJUST (cbDnOffset);
    ADJUST (cbPdOffset);
    ADJUST (cbSymOffset);
    ADJUST (cbOptOffset);
    ADJUST (cbAuxOffset);
    ADJUST (cbSsOffset);
    ADJUST (cbSsExtOffset);
    ADJUST (cbFdOffset);
    ADJUST (cbRfdOffset);
    ADJUST (cbExtOffset);

    if (write (ofd, (char *)&shdr, sizeof (shdr)) != sizeof (shdr)) {
symwerr:
        Dump_Finalize;
        Primitive_Error ("error writing symbol table: ~E");
    }
    while ((n = read (afd, buf, 4096)) > 0)
        if (write (ofd, buf, n) != n) goto symwerr;
    if (n < 0) goto symrerr;

    Dump_Epilog;
}
