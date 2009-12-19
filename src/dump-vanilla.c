/* dump-vanilla.c
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

#ifdef COFF
#  include <filehdr.h>
#  include <aouthdr.h>
#  include <scnhdr.h>
#  include <syms.h>
#  ifndef N_BADMAG
#    define N_BADMAG(x) (0)
#  endif
#else
#  include AOUT_H
#endif

extern void *sbrk();

#if defined(hp9000s300) || defined(__hp9000s300) || defined(__hp9000s300__)
static int getpagesize () {
    return EXEC_PAGESIZE;
}
#endif

Object P_Dump (Object ofile) {
#ifdef COFF
    static struct scnhdr thdr, dhdr, bhdr, scn;
    static struct filehdr hdr;
    static struct aouthdr ohdr;
    unsigned int bias;
    unsigned int lnno_start, syms_start;
    unsigned int text_scn_start, data_scn_start;
    unsigned int data_end;
    int pagemask = COFF_PAGESIZE-1;
#else
    struct exec hdr, shdr;
    unsigned int data_start, data_end;
    int pagemask = getpagesize () - 1;
#endif
    char *afn;
    register int n;
    char buf[BUFSIZ];

    Dump_Prolog;

    if (read (afd, (char *)&hdr, sizeof (hdr)) != sizeof (hdr)
            || N_BADMAG(hdr)) {
#ifdef COFF
badaout:
#endif
        Dump_Finalize;
        Primitive_Error ("corrupt a.out file");
    }
#ifdef COFF
    data_end = ((unsigned int)sbrk (0) + pagemask) & ~pagemask;
    syms_start = sizeof (hdr);
    if (hdr.f_opthdr > 0) {
        if (read (afd, (char *)&ohdr, sizeof (ohdr)) != sizeof (ohdr))
            goto badaout;
    }
    for (n = 0; n < hdr.f_nscns; n++) {
        if (read (afd, (char *)&scn, sizeof (scn)) != sizeof (scn))
            goto badaout;
        if (scn.s_scnptr > 0 && syms_start < scn.s_scnptr + scn.s_size)
            syms_start = scn.s_scnptr + scn.s_size;
        if (strcmp (scn.s_name, ".text") == 0)
            thdr = scn;
        else if (strcmp (scn.s_name, ".data") == 0)
            dhdr = scn;
        else if (strcmp (scn.s_name, ".bss") == 0)
            bhdr = scn;
    }
    hdr.f_flags |= (F_RELFLG|F_EXEC);
    ohdr.dsize = data_end - ohdr.data_start;
    ohdr.bsize = 0;
    thdr.s_size = ohdr.tsize;
    thdr.s_scnptr = sizeof (hdr) + sizeof (ohdr)
        + hdr.f_nscns * sizeof (thdr);
    lnno_start = thdr.s_lnnoptr;
    text_scn_start = thdr.s_scnptr;
    dhdr.s_paddr = dhdr.s_vaddr = ohdr.data_start;
    dhdr.s_size = ohdr.dsize;
    dhdr.s_scnptr = thdr.s_scnptr + thdr.s_size;
    data_scn_start = dhdr.s_scnptr;
    bhdr.s_paddr = bhdr.s_vaddr = ohdr.data_start + ohdr.dsize;
    bhdr.s_size = ohdr.bsize;
    bhdr.s_scnptr = 0;

    bias = dhdr.s_scnptr + dhdr.s_size - syms_start;
    if (hdr.f_symptr > 0)
        hdr.f_symptr += bias;
    if (thdr.s_lnnoptr > 0)
        thdr.s_lnnoptr += bias;

    if (write (ofd, (char *)&hdr, sizeof (hdr)) != sizeof (hdr)) {
badwrite:
        Dump_Finalize;
        Primitive_Error ("error writing dump file: ~E");
    }
    if (write (ofd, (char *)&ohdr, sizeof (ohdr)) != sizeof (ohdr))
        goto badwrite;
    if (write (ofd, (char *)&thdr, sizeof (thdr)) != sizeof (thdr))
        goto badwrite;
    if (write (ofd, (char *)&dhdr, sizeof (dhdr)) != sizeof (dhdr))
        goto badwrite;
    if (write (ofd, (char *)&bhdr, sizeof (bhdr)) != sizeof (bhdr))
        goto badwrite;
    lseek (ofd, (off_t)text_scn_start, 0);
    if (write (ofd, (char *)ohdr.text_start, ohdr.tsize) != ohdr.tsize)
        goto badwrite;
    Was_Dumped = 1;
    lseek (ofd, (off_t)data_scn_start, 0);
    if (write (ofd, (char *)ohdr.data_start, ohdr.dsize) != ohdr.dsize)
        goto badwrite;
    lseek (afd, lnno_start ? (off_t)lnno_start : (off_t)syms_start, 0);
#else
    close (afd);
#if defined(__bsdi__)
    data_start = N_DATADDR(hdr);
#else
    data_start = hdr.a_text;
#if defined(sun) || defined(__sun__)
    data_start += pagemask+1;
#endif
    data_start = (data_start + SEG_SIZ-1) & ~(SEG_SIZ-1);
#endif
    data_end = (unsigned int)sbrk (0);
#if !defined(__bsdi__)
    data_end = (data_end + pagemask) & ~pagemask;
#endif
    hdr.a_data = data_end - data_start;
    hdr.a_bss = 0;
    hdr.a_trsize = hdr.a_drsize = 0;

    afn = Loader_Input;
    if (!afn)
        afn = A_Out_Name;
    if ((afd = open (afn, O_RDONLY|O_BINARY)) == -1) {
        Dump_Finalize;
        Primitive_Error ("cannot open symbol table file: ~E");
    }
    if (read (afd, (char *)&shdr, sizeof (shdr)) != sizeof (shdr)
        || N_BADMAG(shdr)) {
        Dump_Finalize;
        Primitive_Error ("corrupt symbol table file");
    }
#if defined(hp9000s300) || defined(__hp9000s300) || defined(__hp9000s300__)
    hdr.a_lesyms = shdr.a_lesyms;
#else
    hdr.a_syms = shdr.a_syms;
#endif

    (void)lseek (ofd, (off_t)FILE_TEXT_START, 0);
    n = hdr.a_text - TEXT_LENGTH_ADJ;
    if (write (ofd, (char *)MEM_TEXT_START, n) != n) {
badwrite:
        Dump_Finalize;
        Primitive_Error ("error writing dump file: ~E");
    }
    Was_Dumped = 1;

#if defined(hp9000s300) || defined(__hp9000s300) || defined(__hp9000s300__)
    (void)lseek (ofd, (off_t)DATA_OFFSET(hdr), 0);
#endif
#if defined(__bsdi__)
    (void)lseek (ofd, (off_t)N_DATOFF(hdr), 0);
#endif
#ifdef GENERATIONAL_GC
    n = data_end - data_start;
    if (write (ofd, (char *)data_start, n) != n)
        goto badwrite;
#else

    if (Heap_Start > Free_Start) {
        n = (unsigned int)Free_Start - data_start;
        if (write (ofd, (char *)data_start, n) != n)
            goto badwrite;
        (void)lseek (ofd, (off_t)(Free_End - Free_Start), 1);
        n = Hp - Heap_Start;
        if (write (ofd, Heap_Start, n) != n)
            goto badwrite;
        (void)lseek (ofd, (off_t)(Heap_End - Hp), 1);
        n = data_end - (unsigned int)Heap_End;
        if (write (ofd, Heap_End, n) != n)
            goto badwrite;
    } else {
        n = (unsigned int)Hp - data_start;
        if (write (ofd, (char *)data_start, n) != n)
            goto badwrite;
        (void)lseek (ofd, (off_t)(Free_End - Hp), 1);
        n = data_end - (unsigned int)Free_End;
        if (write (ofd, Free_End, n) != n)
            goto badwrite;
    }
#endif
#if defined(hp9000s300) || defined(__hp9000s300) || defined(__hp9000s300__)
    (void)lseek (afd, (off_t)LESYM_OFFSET(shdr), 0);
    (void)lseek (ofd, (off_t)LESYM_OFFSET(hdr), 0);
#else
    (void)lseek (afd, (off_t)N_SYMOFF(shdr), 0);
#if defined(__bsdi__)
    (void)lseek (ofd, (off_t)N_SYMOFF(hdr), 0);
#endif
#endif
#endif /* !COFF */
    while ((n = read (afd, buf, BUFSIZ)) > 0) {
        if (write (ofd, buf, n) != n)
            goto badwrite;
    }
    if (n < 0) {
        Dump_Finalize;
        Primitive_Error ("error reading symbol table: ~E");
    }
#if !defined(COFF)
    (void)lseek (ofd, (off_t)0L, 0);
    if (write (ofd, (char *)&hdr, sizeof (hdr)) != sizeof (hdr))
        goto badwrite;
#endif

    Dump_Epilog;
}
