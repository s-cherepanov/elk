extern GCNODE *GC_List;

#define TAG_FUN    -1
#define TAG_TCFUN  -2
#define TAG_ARGS   -3
#define TAG_ENV    -4

#define GC_Node     GCNODE gc1
#define GC_Node2    GCNODE gc1, gc2
#define GC_Node3    GCNODE gc1, gc2, gc3
#define GC_Node4    GCNODE gc1, gc2, gc3, gc4
#define GC_Node5    GCNODE gc1, gc2, gc3, gc4, gc5
#define GC_Node6    GCNODE gc1, gc2, gc3, gc4, gc5, gc6
#define GC_Node7    GCNODE gc1, gc2, gc3, gc4, gc5, gc6, gc7

#define GC_Link(x) {\
    gc1.gclen = 0; gc1.gcobj = &x; gc1.next = GC_List; GC_List = &gc1;\
}

#define GC_Link2(x1,x2) {\
    gc1.gclen = 0; gc1.gcobj = &x1; gc1.next = GC_List;\
    gc2.gclen = 0; gc2.gcobj = &x2; gc2.next = &gc1; GC_List = &gc2;\
}

#define GC_Link3(x1,x2,x3) {\
    gc1.gclen = 0; gc1.gcobj = &x1; gc1.next = GC_List;\
    gc2.gclen = 0; gc2.gcobj = &x2; gc2.next = &gc1;\
    gc3.gclen = 0; gc3.gcobj = &x3; gc3.next = &gc2; GC_List = &gc3;\
}

#define GC_Link4(x1,x2,x3,x4) {\
    gc1.gclen = 0; gc1.gcobj = &x1; gc1.next = GC_List;\
    gc2.gclen = 0; gc2.gcobj = &x2; gc2.next = &gc1;\
    gc3.gclen = 0; gc3.gcobj = &x3; gc3.next = &gc2;\
    gc4.gclen = 0; gc4.gcobj = &x4; gc4.next = &gc3; GC_List = &gc4;\
}

/* see src/proc.c */
#define GC_Link4_Tag_Primitive(args,fun,env,x4) {\
    gc1.gclen = TAG_ARGS; gc1.gcobj = &args; gc1.next = GC_List;\
    gc2.gclen = Tail_Call ? TAG_TCFUN : TAG_FUN;\
			  gc2.gcobj = &fun;  gc2.next = &gc1;\
    gc3.gclen = TAG_ENV;  gc3.gcobj = &env;  gc3.next = &gc2;\
    gc4.gclen = 0;        gc4.gcobj = &x4;   gc4.next = &gc3; GC_List = &gc4;\
}

#define GC_Link5(x1,x2,x3,x4,x5) {\
    gc1.gclen = 0; gc1.gcobj = &x1; gc1.next = GC_List;\
    gc2.gclen = 0; gc2.gcobj = &x2; gc2.next = &gc1;\
    gc3.gclen = 0; gc3.gcobj = &x3; gc3.next = &gc2;\
    gc4.gclen = 0; gc4.gcobj = &x4; gc4.next = &gc3;\
    gc5.gclen = 0; gc5.gcobj = &x5; gc5.next = &gc4; GC_List = &gc5;\
}

#define GC_Link6(x1,x2,x3,x4,x5,x6) {\
    gc1.gclen = 0; gc1.gcobj = &x1; gc1.next = GC_List;\
    gc2.gclen = 0; gc2.gcobj = &x2; gc2.next = &gc1;\
    gc3.gclen = 0; gc3.gcobj = &x3; gc3.next = &gc2;\
    gc4.gclen = 0; gc4.gcobj = &x4; gc4.next = &gc3;\
    gc5.gclen = 0; gc5.gcobj = &x5; gc5.next = &gc4;\
    gc6.gclen = 0; gc6.gcobj = &x6; gc6.next = &gc5; GC_List = &gc6;\
}

#define GC_Link7(x1,x2,x3,x4,x5,x6,x7) {\
    gc1.gclen = 0; gc1.gcobj = &x1; gc1.next = GC_List;\
    gc2.gclen = 0; gc2.gcobj = &x2; gc2.next = &gc1;\
    gc3.gclen = 0; gc3.gcobj = &x3; gc3.next = &gc2;\
    gc4.gclen = 0; gc4.gcobj = &x4; gc4.next = &gc3;\
    gc5.gclen = 0; gc5.gcobj = &x5; gc5.next = &gc4;\
    gc6.gclen = 0; gc6.gcobj = &x6; gc6.next = &gc5;\
    gc7.gclen = 0; gc7.gcobj = &x7; gc7.next = &gc6; GC_List = &gc7;\
}

/* see src/proc.c */
#define GC_Link7_Tag_Compound(args,fun,env,x4,x5,x6,x7) {\
    gc1.gclen = TAG_ARGS;\
		   gc1.gcobj = &args; gc1.next = GC_List;\
    gc2.gclen = Tail_Call ? TAG_TCFUN : TAG_FUN;\
		   gc2.gcobj = &fun; gc2.next = &gc1;\
    gc3.gclen = TAG_ENV;\
		   gc3.gcobj = &env; gc3.next = &gc2;\
    gc4.gclen = 0; gc4.gcobj = &x4; gc4.next = &gc3;\
    gc5.gclen = 0; gc5.gcobj = &x5; gc5.next = &gc4;\
    gc6.gclen = 0; gc6.gcobj = &x6; gc6.next = &gc5;\
    gc7.gclen = 0; gc7.gcobj = &x7; gc7.next = &gc6; GC_List = &gc7;\
}

#define GC_Unlink (GC_List = gc1.next)

C_LINKAGE_BEGIN
extern Func_Global_GC_Link P_((Object*));
C_LINKAGE_END

#define Global_GC_Link(x) Func_Global_GC_Link(&x)
