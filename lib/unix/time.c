#include "unix.h"

#if !defined(GETTIMEOFDAY) && defined(FTIME)
#  include <sys/timeb.h>
#endif

#ifdef GETTIMEOFDAY
#  include <sys/time.h>
#endif

extern time_t time();

static Object P_Decode_Time(t, ret, utc) Object t, ret, utc; {
    time_t tt;
    struct tm *tp;
    Object *op;

    Check_Result_Vector(ret, 9);
    Check_Type(utc, T_Boolean);
    tt = (time_t)Get_Unsigned_Long(t);
    tp = Truep(utc) ? gmtime(&tt) : localtime(&tt);
    op = VECTOR(ret)->data;
    *op++ = Make_Integer(tp->tm_sec);
    *op++ = Make_Integer(tp->tm_min);
    *op++ = Make_Integer(tp->tm_hour);
    *op++ = Make_Integer(tp->tm_mday);
    *op++ = Make_Integer(tp->tm_mon);
    *op++ = Make_Integer(tp->tm_year);
    *op++ = Make_Integer(tp->tm_wday);
    *op++ = Make_Integer(tp->tm_yday);
    *op++ = Make_Integer(tp->tm_isdst);
    return Void;
}

static Object P_Nanotime(ret) Object ret; {
    Object x, y;
#ifdef GETTIMEOFDAY
    struct timeval tv;
    struct timezone tz;
#else
#ifdef FTIME
    struct timeb tb;
#else
    time_t now;
    int i;
#endif
#endif
    GC_Node2;

    x = Null;
    Check_Result_Vector(ret, 3);
    GC_Link2(ret, x);
#ifdef GETTIMEOFDAY
    (void)gettimeofday(&tv, &tz);
    x = Cons(Null, Make_Unsigned_Long((unsigned long)tv.tv_usec * 1000));
    y = Make_Unsigned_Long((unsigned long)tv.tv_sec);
    Car(x) = y;
    VECTOR(ret)->data[0] = x;
    VECTOR(ret)->data[1] = Make_Integer(tz.tz_minuteswest);
    VECTOR(ret)->data[2] = Make_Integer(tz.tz_dsttime);
#else
#ifdef FTIME
    (void)ftime(&tb);
    x = Cons(Null, Make_Unsigned_Long((unsigned long)tb.millitm * 1000000));
    y = Make_Unsigned_Long((unsigned long)tb.time);
    Car(x) = y;
    VECTOR(ret)->data[0] = x;
    VECTOR(ret)->data[1] = Make_Integer(tb.timezone);
    VECTOR(ret)->data[2] = Make_Integer(tb.dstflag);
#else
    (void)time(&now);
    x = Cons(Make_Unsigned_Long((unsigned long)now), Make_Integer(0));
    VECTOR(ret)->data[0] = x;
    VECTOR(ret)->data[1] = Make_Integer(0);
    VECTOR(ret)->data[2] = Make_Integer(0);
#endif
#endif
    GC_Unlink;
    return Void;
}

static Object P_Time() {
    time_t t = time((time_t *)0);
    return Make_Unsigned_Long((unsigned long)t);
}

static struct tm *Get_Tm(v) Object v; {
    static struct tm tm;
    int i, n;
    Object *op;
    static struct { int min, max; } bounds[] = {
	{ 0, 61 },       /* sec */
	{ 0, 59 },       /* min */
	{ 0, 23 },       /* hour */
	{ 1, 31 },       /* mday */
	{ 0, 11 },       /* mon */
	{ 0, 65535 },  /* year */
	{ 0, 7 },        /* wday */
	{ 0, 365 }       /* yday */
    };

    Check_Result_Vector(v, 9);
    for (op = VECTOR(v)->data, i = 0; i < 7; i++, op++)
	if ((n = Get_Integer(*op)) < bounds[i].min || n > bounds[i].max)
	    Range_Error(*op);
    op = VECTOR(v)->data;
    tm.tm_sec   = Get_Integer(*op++);
    tm.tm_min   = Get_Integer(*op++);
    tm.tm_hour  = Get_Integer(*op++);
    tm.tm_mday  = Get_Integer(*op++);
    tm.tm_mon   = Get_Integer(*op++);
    tm.tm_year  = Get_Integer(*op++);
    tm.tm_wday  = Get_Integer(*op++);
    tm.tm_yday  = Get_Integer(*op++);
    tm.tm_isdst = Get_Integer(*op);
    return &tm;
}

static Object P_Time_To_String(t) Object t; {
    time_t tt;
    char *ret;

    switch (TYPE(t)) {
    case T_Fixnum: case T_Bignum:
	tt = (time_t)Get_Unsigned_Long(t);
	ret = ctime(&tt);
	break;
    case T_Vector:
	ret = asctime(Get_Tm(t));
	break;
    default:
	Wrong_Type_Combination(t, "integer or vector");
	/*NOTREACHED*/
    }
    return Make_String(ret, strlen(ret));
}

elk_init_unix_time() {
    Def_Prim(P_Time,            "unix-time",                     0, 0, EVAL);
    Def_Prim(P_Decode_Time,     "unix-decode-time-vector-fill!", 3, 3, EVAL);
    Def_Prim(P_Time_To_String,  "unix-time->string-internal",    1, 1, EVAL);
    Def_Prim(P_Nanotime,        "unix-nanotime-vector-fill!",    1, 1, EVAL);
}
