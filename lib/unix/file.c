#include "unix.h"

#ifdef HAVE_UTIME_H
#  include <utime.h>
#else
struct utimbuf {
    time_t actime, modtime;
};
#endif

#ifdef HAVE_DIRENT
#  include <dirent.h>
#else
#  include <sys/dir.h>
#endif

#if defined(ELOOP)
#  define SYMLINKS
#endif

static SYMDESCR Access_Syms[] = {
    { "read",       R_OK },      /* Nothing == F_OK */
    { "write",      W_OK },
    { "execute",    X_OK },
    { 0, 0 }
};

static Object P_Accessp(fn, mode) Object fn, mode; {
    if (access(Get_Strsym(fn), (int)Symbols_To_Bits(mode, 1, Access_Syms))
	    == 0)
	return True;
    Saved_Errno = errno;
    return False;
}

static Object P_Chdir(fn) Object fn; {
    if (chdir(Get_Strsym(fn)) == -1)
	Raise_System_Error1("~s: ~E", fn);
    return Void;
}

static Object P_Chmod(fn, mode) Object fn, mode; {
    if (chmod(Get_Strsym(fn), Get_Integer(mode)) == -1)
	Raise_System_Error1("~s: ~E", fn);
    return Void;
}

static Object P_Chown(fn, uid, gid) Object fn, uid, gid; {
    if (chown(Get_Strsym(fn), Get_Integer(uid), Get_Integer(gid)) == -1)
	Raise_System_Error1("~s: ~E", fn);
    return Void;
}

static Object P_Link(fn1, fn2) Object fn1, fn2; {
    if (link(Get_Strsym(fn1), Get_Strsym(fn2)) == -1)
	Raise_System_Error2("(~s ~s): ~E", fn1, fn2);
    return Void;
}

static Object P_Mkdir(fn, mode) Object fn, mode; {
    if (mkdir(Get_Strsym(fn), Get_Integer(mode)) == -1)
	Raise_System_Error1("~s: ~E", fn);
    return Void;
}

static Object P_Read_Directory(fn) Object fn; {
    DIR *d;
#ifdef HAVE_DIRENT
    struct dirent *dp;
#else
    struct direct *dp;
#endif
    Object ret;
    GC_Node;

    ret = Null;
    GC_Link(ret);
    Disable_Interrupts;
    if ((d = opendir(Get_Strsym(fn))) == NULL) {
	Saved_Errno = errno;
	Enable_Interrupts;
	Raise_System_Error1("~s: cannot open", fn);
    }
    while ((dp = readdir(d)) != NULL) {
	Object x;

	x = Make_String(dp->d_name, strlen(dp->d_name));
	ret = Cons(x, ret);
    }
    /* closedir() is void under 4.3BSD, should check result elsewhere.
     */
    (void)closedir(d);
    Enable_Interrupts;
    GC_Unlink;
    return ret;
}

static Object P_Rename(fromfn, tofn) Object fromfn, tofn; {
#ifdef HAVE_RENAME
    if (rename(Get_Strsym(fromfn), Get_Strsym(tofn)) == -1)
	Raise_System_Error2("(~s ~s): ~E", fromfn, tofn);
#else
    char *from = Get_Strsym(fromfn), *to = Get_Strsym(tofn);

    Disable_Interrupts;
    if (link(from, to) == -1) {
	Saved_Errno = errno;
	Enable_Interrupts;
	Raise_System_Error2("(~s ~s): ~E", fromfn, tofn);
    }
    if (unlink(from) == -1) {
	Saved_Errno = errno;
	(void)unlink(to);
	Enable_Interrupts;
	Raise_Error1("~s: ~E", fromfn);
    }
    Enable_Interrupts;
#endif
    return Void;
}

static Object General_Stat(obj, ret, l) Object obj, ret; int l; {
    Object x;
    struct stat st;
    char *s, *fn = 0;
    int fd, result;
    GC_Node;

    Check_Result_Vector(ret, 11);
    if (l) {
#ifdef SYMLINKS
	result = lstat(Get_Strsym(obj), &st);
#endif
    } else {
	Get_Filename_Or_Filedescr(obj, fd, fn);
        result = fn ? stat(fn, &st) : fstat(fd, &st);
    }
    if (result == -1)
	Raise_System_Error1("~s: ~E", obj);
    switch (st.st_mode & S_IFMT) {
    case S_IFDIR:  s = "directory";         break;
    case S_IFCHR:  s = "character-special"; break;
    case S_IFBLK:  s = "block-special";     break;
    case S_IFREG:  s = "regular";           break;
#ifdef S_IFLNK
    case S_IFLNK:  s = "symlink";           break;
#endif
#ifdef S_IFSOCK
    case S_IFSOCK: s = "socket";            break;
#endif
#ifdef S_IFFIFO
    case S_IFFIFO: s = "fifo";              break;
#endif
    default:       s = "unknown";           break;
    }
    /* Bad assumption: any of the st_ fields fits into an unsigned int.
     */
    GC_Link(ret);
    x = Intern(s);
    VECTOR(ret)->data[0] = x;
    x = Make_Unsigned((unsigned)st.st_mode & ~S_IFMT);
    VECTOR(ret)->data[1] = x;
    x = Make_Unsigned_Long((unsigned long)st.st_ino);
    VECTOR(ret)->data[2] = x;
    x = Make_Unsigned((unsigned)st.st_dev);
    VECTOR(ret)->data[3] = x;
    x = Make_Unsigned((unsigned)st.st_nlink);
    VECTOR(ret)->data[4] = x;
    x = Make_Unsigned((unsigned)st.st_uid);
    VECTOR(ret)->data[5] = x;
    x = Make_Unsigned((unsigned)st.st_gid);
    VECTOR(ret)->data[6] = x;
    x = Make_Long((long)st.st_size);
    VECTOR(ret)->data[7] = x;
    x = Make_Unsigned_Long((unsigned long)st.st_atime);
    VECTOR(ret)->data[8] = x;
    x = Make_Unsigned_Long((unsigned long)st.st_mtime);
    VECTOR(ret)->data[9] = x;
    x = Make_Unsigned_Long((unsigned long)st.st_ctime);
    VECTOR(ret)->data[10] = x;
    GC_Unlink;
    return Void;
}

static Object P_Stat(obj, ret) Object obj, ret; {
    return General_Stat(obj, ret, 0);
}

#ifdef SYMLINKS
static Object P_Lstat(obj, ret) Object obj, ret; {
    return General_Stat(obj, ret, 1);
}

static Object P_Readlink(fn) Object fn; {
    char *buf;
    int len;
    Object ret;
    Alloca_Begin;

    len = Path_Max();
    Alloca(buf, char*, len);
    if ((len = readlink(Get_Strsym(fn), buf, len)) == -1) {
	Alloca_End;
	Raise_System_Error1("~s: ~E", fn);
    }
    ret = Make_String(buf, len);
    Alloca_End;
    return ret;
}

static Object P_Rmdir(fn) Object fn; {
    if (rmdir(Get_Strsym(fn)) == -1)
	Raise_System_Error1("~s: ~E", fn);
    return Void;
}

static Object P_Symlink(fn1, fn2) Object fn1, fn2; {
    if (symlink(Get_Strsym(fn1), Get_Strsym(fn2)) == -1)
	Raise_System_Error2("(~s ~s): ~E", fn1, fn2);
    return Void;
}
#endif

static Object P_Unlink(fn) Object fn; {
    if (unlink(Get_Strsym(fn)) == -1)
	Raise_System_Error1("~s: ~E", fn);
    return Void;
}

static Object P_Utime(argc, argv) int argc; Object *argv; {
    struct utimbuf ut;

    if (argc == 2)
	Primitive_Error("wrong number of arguments");
    if (argc == 3) {
	ut.actime = (time_t)Get_Unsigned_Long(argv[1]);
	ut.modtime = (time_t)Get_Unsigned_Long(argv[2]);
    }
    if (utime(Get_Strsym(argv[0]), argc == 1 ? (struct utimbuf *)0 : &ut)
	    == -1)
	Raise_System_Error1("~s: ~E", argv[0]);
    return Void;
}

elk_init_unix_file() {
    Def_Prim(P_Accessp,            "unix-access?",              2, 2, EVAL);
    Def_Prim(P_Chdir,              "unix-chdir",                1, 1, EVAL);
    Def_Prim(P_Chmod,              "unix-chmod",                2, 2, EVAL);
    Def_Prim(P_Chown,              "unix-chown",                3, 3, EVAL);
    Def_Prim(P_Link,               "unix-link",                 2, 2, EVAL);
    Def_Prim(P_Mkdir,              "unix-mkdir",                2, 2, EVAL);
    Def_Prim(P_Read_Directory,     "unix-read-directory",       1, 1, EVAL);
    Def_Prim(P_Rename,             "unix-rename",               2, 2, EVAL);
    Def_Prim(P_Stat,               "unix-stat-vector-fill!",    2, 2, EVAL);
#ifdef SYMLINKS
    Def_Prim(P_Lstat,              "unix-lstat-vector-fill!",   2, 2, EVAL);
    Def_Prim(P_Readlink,           "unix-readlink",             1, 1, EVAL);
    Def_Prim(P_Rmdir,              "unix-rmdir",                1, 1, EVAL);
    Def_Prim(P_Symlink,            "unix-symlink",              2, 2, EVAL);
    P_Provide(Intern("unix:symlinks"));
#endif
    Def_Prim(P_Unlink,             "unix-unlink",               1, 1, EVAL);
    Def_Prim(P_Utime,              "unix-utime",                1, 3, VARARGS);
}
