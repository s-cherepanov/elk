#include "kernel.h"

#ifdef CAN_DUMP

#include <errno.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifndef O_BINARY
#  define O_BINARY 0
#endif

extern void Check_If_Dump_Works ();
extern void Flush_Output (Object);
extern void Close_All_Files ();
extern void Generational_GC_Finalize ();

extern int errno;

void Set_File_Executable (int, char *);

Object Dump_Control_Point;

void Init_Dump () {
    Dump_Control_Point = Null;
    Global_GC_Link (Dump_Control_Point);
}

#ifdef GENERATIONAL_GC
#  define GC_FINALIZE Generational_GC_Finalize()
#else
#  define GC_FINALIZE
#endif

#define Dump_Prolog \
    Object ret;\
    int ofd, afd;\
    char *ofn;\
    GC_Node;\
\
    Check_If_Dump_Works ();\
    if (!EQ (Curr_Input_Port, Standard_Input_Port) ||\
	    !EQ (Curr_Output_Port, Standard_Output_Port))\
	Primitive_Error ("cannot dump with current ports redirected");\
    Flush_Output (Curr_Output_Port);\
    Close_All_Files ();\
    GC_FINALIZE;\
\
    GC_Link (ofile);\
    ret = Internal_Call_CC (1, Null);\
    if (Truep (ret))\
	return ret;\
    GC_Unlink;\
\
    Disable_Interrupts;\
\
    ofn = Get_Strsym (ofile);\
    if ((ofd = open (ofn, O_RDWR|O_CREAT|O_TRUNC|O_BINARY, 0666)) == -1) {\
	Saved_Errno = errno;\
	Primitive_Error ("cannot open ~s: ~E", ofile);\
    }\
    if ((afd = open (A_Out_Name, O_RDONLY|O_BINARY)) == -1) {\
	Saved_Errno = errno;\
	close (ofd);\
	Primitive_Error ("cannot open a.out file: ~E");\
    }

#define Dump_Finalize    Saved_Errno = errno; close (afd); close (ofd)


#define Dump_Epilog {\
    close (afd);\
    Set_File_Executable (ofd, ofn);\
    close (ofd);\
    Enable_Interrupts;\
    return False;\
}

#ifdef ELF
#  include "dump-elf.c"
#else
#ifdef ECOFF
#  include "dump-ecoff.c"
#else
#ifdef HP9K
#  include "dump-hp9k.c"
#else
#  include "dump-vanilla.c"
#endif
#endif
#endif

/*ARGSUSED1*/
void Set_File_Executable (int fd, char *fn) {
    struct stat st;

    if (fstat (fd, &st) != -1) {
	int omask = umask (0);
	(void)umask (omask);
#ifdef FCHMOD_BROKEN
	(void)chmod (fn, (st.st_mode & 0777) | (0111 & ~omask));
#else
	(void)fchmod (fd, (st.st_mode & 0777) | (0111 & ~omask));
#endif
    }
}

#endif /* CAN_DUMP */
