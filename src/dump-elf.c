#include <elf.h>
#include <memory.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <malloc.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>

/* Find section header of section with given name.
 */
#define FIND_SECTHDR(name,ndx) {\
    char err[100];\
    unsigned int _i;\
    for (_i = 0; _i < ohdr->e_shnum; _i++)\
	if (strcmp (sectstr+osecthdr[_i].sh_name, (name)) == 0) break;\
    if (_i == ohdr->e_shnum) {\
	Dump_Finalize;\
	sprintf (err, "running a.out doesn't have %s section", (name));\
	Primitive_Error (err);\
    }\
    (ndx) = _i;\
}

/* Find section header of section with given name (if present, else
 * set to -1).
 */
#define FIND_SECTHDR_MAYBE(name,ndx) {\
    int _i;\
    for ((ndx) = -1, _i = 0; _i < ohdr->e_shnum; _i++)\
	if (strcmp (sectstr+osecthdr[_i].sh_name, (name)) == 0) {\
	    (ndx) = _i;\
	    break;\
	}\
}

/* If a new section was inserted, adjust section index if it points behind
 * old .bss section
 */
#define UPDATE_SHNDX(ndx) if (sect_created && (ndx) >= obssndx) (ndx)++;


/* Bug: the mmapped regions are never munmapped again.
 */

Object P_Dump (Object ofile) {
    /*
     * ELF header, section header table, program header table of running
     * a.out and new a.out
     */
    Elf32_Ehdr *ohdr, *nhdr;
    Elf32_Shdr *osecthdr, *nsecthdr;
    Elf32_Phdr *oproghdr, *nproghdr;
    /*
     * .bss section index and section header pointer of running a.out
     */
    unsigned int obssndx;
    Elf32_Shdr *obssp;
    /*
     * .mdebug section index
     */
    int mdebugndx;
    /*
     * Pointers to section headers of new .bss and new .data
     */
    Elf32_Shdr *nbssp, *ndatap;
    /*
     * Memory address, size, and file offset of newly created .data section
     */
    Elf32_Addr ndata;
    Elf32_Word ndatasize;
    Elf32_Off ndataoff;
    /*
     * Start of .shstrtab section of running a.out
     */
    char *sectstr;
    /*
     * Memory address of running a.out and new a.out (mmap() return value)
     */
    char *oaddr, *naddr;

    struct stat st;
    unsigned int i;
    int sect_created = !Was_Dumped;

    Dump_Prolog;

    /* mmap running a.out, setup pointers to ELF header, section header
     * table, program header table, section names, and old .bss
     * XXX: call munmap later.
     */
    if (fstat (afd, &st) == -1) {
	Dump_Finalize;
	Primitive_Error ("cannot fstat running a.out: ~E");
    }
    oaddr = (char *)mmap ((caddr_t)0, st.st_size, PROT_READ, MAP_SHARED,
	afd, 0);
    if (oaddr == (char *)-1) {
	Dump_Finalize;
	Primitive_Error ("cannot mmap running a.out: ~E");
    }
    ohdr     = (Elf32_Ehdr *)(oaddr);
    osecthdr = (Elf32_Shdr *)(oaddr + ohdr->e_shoff);
    oproghdr = (Elf32_Phdr *)(oaddr + ohdr->e_phoff);
    if (ohdr->e_shstrndx == SHN_UNDEF) {
	Dump_Finalize;
	Primitive_Error ("running a.out doesn't have section names");
    }
    sectstr = oaddr + osecthdr[ohdr->e_shstrndx].sh_offset;
    FIND_SECTHDR (".bss", obssndx);
    obssp = osecthdr+obssndx;

    FIND_SECTHDR_MAYBE (".mdebug", mdebugndx);

    /* Determine size of newly created .data section; address and file
     * offset are that of the old .bss section
     */
    if ((Brk_On_Dump = sbrk (0)) == (char *)-1) {
	Dump_Finalize;
	Primitive_Error ("sbrk(0) failed: ~E");
    }
    ndata     = obssp->sh_addr;
    ndatasize = (Elf32_Addr)((ptrdiff_t)Brk_On_Dump - (ptrdiff_t)ndata);
    ndataoff  = obssp->sh_offset;

    /* mmap new a.out file, setup pointers to ELF header, section header
     * table, and program header table
     * XXX: munmap missing
     */
    st.st_size += ndatasize;
    if (!Was_Dumped)
	st.st_size += sizeof (osecthdr[0]);
    if (ftruncate (ofd, st.st_size) == -1) {
	Dump_Finalize;
	Primitive_Error ("cannot ftruncate new a.out: ~E");
    }
    naddr = (char *)mmap ((caddr_t)0, st.st_size, PROT_READ|PROT_WRITE,
	MAP_SHARED, ofd, 0);
    if (naddr == (char *)-1) {
	Dump_Finalize;
	Primitive_Error ("cannot mmap new a.out: ~E");
    }
    nhdr     = (Elf32_Ehdr *)(naddr);
    nsecthdr = (Elf32_Shdr *)(naddr + ohdr->e_shoff + ndatasize);
    nproghdr = (Elf32_Phdr *)(naddr + ohdr->e_phoff);

    /* Copy and adjust ELF header, copy program header table
     */
    *nhdr = *ohdr;
    if (!Was_Dumped)
	nhdr->e_shnum++;
    UPDATE_SHNDX (nhdr->e_shstrndx);
    nhdr->e_shoff += ndatasize;
    memcpy ((void *)nproghdr, (void *)oproghdr,
	ohdr->e_phnum * sizeof (oproghdr[0]));

    /* Scan program header table and search for a loadable segment that
     * ends immediately below the .bss section.  Extend this segment so
     * that it encompasses the newly created .data section.
     * There must not exist any segment above the new .data.
     */
#define max(a,b) ((a) > (b) ? (a) : (b))
    for (i = 0; i < nhdr->e_phnum; i++) {
	Elf32_Phdr *pp = nproghdr+i;
	unsigned int mask = max(pp->p_align, obssp->sh_addralign) - 1;
	Elf32_Addr ends_at = (pp->p_vaddr + pp->p_filesz + mask) & ~mask;
	Elf32_Addr bssend = (obssp->sh_addr + mask) & ~mask;
#ifndef __sgi
	if (pp->p_vaddr + pp->p_filesz > obssp->sh_addr) {
	    Dump_Finalize;
	    Primitive_Error ("running a.out has segment above .bss");
	}
#endif
	if (pp->p_type == PT_LOAD && ends_at == bssend)
	    break;
    }

    nproghdr[i].p_filesz += ndatasize;
    nproghdr[i].p_memsz = nproghdr[i].p_filesz;  /* load entire segment */

#ifdef __sgi
    for (i = 0; i < nhdr->e_phnum; i++) {
	Elf32_Phdr *pp = nproghdr+i;

	if (pp->p_vaddr >= ndata)
	    pp->p_vaddr += ndatasize - obssp->sh_size;
	if (pp->p_offset >= ndataoff)
	    pp->p_offset += ndatasize;
    }
#endif

    if (Was_Dumped) {
	/* No need to insert a new data section header.  Just copy
	 * section header table.  Data segment to be adjusted must
	 * be immediately before .bss
	 */
	memcpy ((void*)nsecthdr, (void *)osecthdr,
	    nhdr->e_shnum * sizeof (osecthdr[0]));
	nbssp = nsecthdr + obssndx;
	ndatap = nbssp - 1;
	if (strcmp (sectstr+ndatap->sh_name, ".data")) {
	    Dump_Finalize;
	    Primitive_Error ("missing .data section in dumped a.out");
	}
	ndatap->sh_size += ndatasize;
    } else {
	/* Copy section headers up to old .bss, then copy remaining section
	 * headers shifted by one position to make room for new .data
	 */
	memcpy ((void *)nsecthdr, (void *)osecthdr,
	    obssndx * sizeof (osecthdr[0]));
	ndatap = nsecthdr + obssndx;
	nbssp = ndatap + 1;
	memcpy ((void *)nbssp, (void *)obssp,
	    (nhdr->e_shnum-obssndx) * sizeof (osecthdr[0]));

	/* Initialize section header for new .data section with values
	 * from old .data section; set new address, size, and file offset
	 */
	FIND_SECTHDR (".data", i);
	ndatap[0] = osecthdr[i];
	ndatap->sh_addr = ndata;
	ndatap->sh_size = ndatasize;
	ndatap->sh_offset = ndataoff;
    }
    nbssp->sh_size = 0;
    nbssp->sh_addr += ndatasize;

    /* Now copy the contents of the sections.  If section is in memory
     * and writable, copy from memory, else copy from a.out file.
     * Skip sections that are inactive or occupy no space in file.
     * Adjust file offset of sections behind new .data section.
     */
    Was_Dumped = 1;
    for (i = 1; i < nhdr->e_shnum; i++) {
	void *from;
	Elf32_Shdr *sp = nsecthdr+i;
#ifdef DEBUG_DUMP
	printf ("%s (from %s)", sectstr+sp->sh_name, (sp->sh_flags &
	    (SHF_ALLOC|SHF_WRITE)) == (SHF_ALLOC|SHF_WRITE) ?
	    "memory" : "file"); (void)fflush (stdout);
#endif
	if ((sp->sh_flags & (SHF_ALLOC|SHF_WRITE)) == (SHF_ALLOC|SHF_WRITE))
	    from = (void *)(ptrdiff_t)sp->sh_addr;
	else
	    from = (void *)(oaddr + sp->sh_offset);
	if (sp != ndatap && sp->sh_offset >= ndataoff)
	    sp->sh_offset += ndatasize;
	if (sp->sh_type != SHT_NULL && sp->sh_type != SHT_NOBITS) {
#ifdef DEBUG_DUMP
	    printf (" copy from %p to %p size %x", from, naddr+sp->sh_offset,
		sp->sh_size); (void)fflush (stdout);
#endif
	    memcpy ((void *)(naddr + sp->sh_offset), from, sp->sh_size);
	}
#ifdef DEBUG_DUMP
	printf ("\n");
#endif
    }

    /* Go through all section headers and fixup sh_link and sh_info fields
     * that point behind new .data section, also fixup st_shndx fields in
     * symbol table entries
     */
    for (i = 1; i < nhdr->e_shnum; i++) {
	Elf32_Shdr *sp = nsecthdr+i;

	UPDATE_SHNDX (sp->sh_link);
	if (sp->sh_type != SHT_DYNSYM && sp->sh_type != SHT_SYMTAB)
	    UPDATE_SHNDX (sp->sh_info);

	if (sp->sh_type == SHT_SYMTAB || sp->sh_type == SHT_DYNSYM) {
	    Elf32_Sym *p = (Elf32_Sym *)(naddr + sp->sh_offset),
		     *ep = p + sp->sh_size / sp->sh_entsize;
	    for ( ; p < ep; p++) switch (p->st_shndx) {
		case SHN_UNDEF: case SHN_ABS: case SHN_COMMON:
		    break;
		default:
		    UPDATE_SHNDX (p->st_shndx);
	    }
	}
    }

#ifdef __sgi
    /* If the .mdebug section is located after the newly inserted section,
     * update the offsets.
     */
    if (mdebugndx >= obssndx) {
	HDRR *mp;
	mdebugndx++;
	mp = (HDRR *)(naddr + nsecthdr[mdebugndx].sh_offset);
	if (mp->cbLine > 0)
	    mp->cbLineOffset += ndatasize;
	if (mp->idnMax > 0)
	    mp->cbDnOffset += ndatasize;
	if (mp->ipdMax > 0)
	    mp->cbPdOffset += ndatasize;
	if (mp->isymMax > 0)
	    mp->cbSymOffset += ndatasize;
	if (mp->ioptMax > 0)
	    mp->cbOptOffset += ndatasize;
	if (mp->iauxMax > 0)
	    mp->cbAuxOffset += ndatasize;
	if (mp->issMax > 0)
	    mp->cbSsOffset += ndatasize;
	if (mp->issExtMax > 0)
	    mp->cbSsExtOffset += ndatasize;
	if (mp->ifdMax > 0)
	    mp->cbFdOffset += ndatasize;
	if (mp->crfd > 0)
	    mp->cbRfdOffset += ndatasize;
	if (mp->iextMax > 0)
	    mp->cbExtOffset += ndatasize;
    }
#endif

    Dump_Epilog;
}
