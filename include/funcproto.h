/* These definitions are not tunable.  Do not change them.
 */


#if __STDC__ || defined(__cplusplus)
#  undef WANT_PROTOTYPES
#endif

#ifdef WANT_PROTOTYPES
#  define ELK_USE_PROTOTYPES
#  define ELLIPSIS
#endif

#ifdef __cplusplus
#  define ELK_USE_PROTOTYPES
#  define ELLIPSIS ...
#  define C_LINKAGE_BEGIN extern "C" {
#  define C_LINKAGE_END   }
#else
#  define C_LINKAGE_BEGIN
#  define C_LINKAGE_END
#endif

#if __STDC__ && !defined(__cplusplus)
#  define ELK_USE_PROTOTYPES
#  define ELLIPSIS
#endif

#ifdef NO_PROTOTYPES
#  undef ELK_USE_PROTOTYPES
#endif

#ifdef ELK_USE_PROTOTYPES
#  define P_(args) args
#else
#  define P_(args) ()
#  define ELLIPSIS
#  define const
#endif
