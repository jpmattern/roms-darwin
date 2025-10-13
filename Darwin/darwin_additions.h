#if defined DARWIN
# define ANA_SPFLUX
# define ANA_BPFLUX
# if ! defined NO_POSITIVEDEF
#  define POSITIVEDEF
# endif

# if ! defined DARWIN_MINVAL_X
#  define DARWIN_MINVAL_X 1.0e-9_r8
# endif

# define DARWIN_AUTO_FIX

# if defined DARWIN_6P4Z
/* 2018-07-23 6P4Z setup from Stephanie Dutkiewicz */
#  define DARWIN_CHLQUOTA
#  define DARWIN_CDOM
#  define DARWIN_CARBON

#  define DARWIN_DENIT

#  define DARWIN_READ_PAR  /*might not make sense to have in ROMS*/
#  define DARWIN_GEIDER

/* initialize chl with radtrans as in darwin2 */

#  define DARWIN_GEIDER_RHO_SYNTH

#  define DARWIN_TEMP_VERSION 2

#  define DARWIN_MINFE
#  define DARWIN_PART_SCAV

/* variables to be read from netCDF initial file */
#  define DARWIN_READ_PALAT
#  define DARWIN_READ_ASSEFF
#  define DARWIN_READ_EXPORTFRACMORT
#  define DARWIN_READ_EXPORTFRACMORT2
#  define DARWIN_READ_EXPORTFRACPREYPRED
#  if ! defined DARWIN_NOREAD_MORT2
#   define DARWIN_READ_MORT2
#  endif
#  if ! defined DARWIN_NOREAD_GRAZEMAX
#   define DARWIN_READ_GRAZEMAX
#  endif

/* specify dimensions */
#  if ! defined DARWIN_DIM_NPLANK
#   define DARWIN_DIM_NPLANK  10
#  endif
#  if ! defined DARWIN_DIM_NGROUP
#   define DARWIN_DIM_NGROUP   7
#  endif
#  if ! defined DARWIN_DIM_NLAM
#   define DARWIN_DIM_NLAM     1
#  endif
#  if ! defined DARWIN_DIM_NOPT
#   define DARWIN_DIM_NOPT     1
#  endif
#  if ! defined DARWIN_DIM_NPHOTO
#   define DARWIN_DIM_NPHOTO   6
#  endif
#  if ! defined DARWIN_DIM_NPPPLANK
#   if defined DIAGNOSTICS_BIO
#    define DARWIN_DIM_NPPPLANK DARWIN_DIM_NPLANK
#   else
#    define DARWIN_DIM_NPPPLANK 0
#   endif
#  endif
#  if ! defined DARWIN_DIM_NGRPLANK
#   if defined DIAGNOSTICS_BIO
#    define DARWIN_DIM_NGRPLANK DARWIN_DIM_NPLANK
#   else
#    define DARWIN_DIM_NGRPLANK 0
#   endif
#  endif
#  if ! defined DARWIN_INDEX_MINBACT
#   define DARWIN_INDEX_MINBACT 101
#  endif
#  if ! defined DARWIN_INDEX_MAXBACT
#   define DARWIN_INDEX_MAXBACT 100
#  endif
#  if ! defined DARWIN_INDEX_MINPREY
#   define DARWIN_INDEX_MINPREY 1
#  endif
#  if ! defined DARWIN_INDEX_MAXPREY
#   define DARWIN_INDEX_MAXPREY DARWIN_DIM_NPLANK
#  endif
#  if ! defined DARWIN_INDEX_MINPRED
#   define DARWIN_INDEX_MINPRED 6
#  endif
#  if ! defined DARWIN_INDEX_MAXPRED
#   define DARWIN_INDEX_MAXPRED DARWIN_DIM_NPLANK
#  endif

# endif /* DARWIN_6P4Z */

#endif /* DARWIN */
