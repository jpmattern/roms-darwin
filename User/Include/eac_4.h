/*
** svn $Id: eac_4.h 1210 2024-01-03 22:03:03Z arango $
*******************************************************************************
** Copyright (c) 2002-2024 The ROMS/TOMS Group                               **
**   Licensed under a MIT/X style license                                    **
**   See License_ROMS.md                                                     **
*******************************************************************************
**
** Options for East Australia Current, 1/4 degree resolution
**
** Application flag:   EAC_4
** Input script:       ocean_eac_4.in
*/


#undef  AFT_EIGENMODES          /* Adjoint Finite Time Eigenmodes */
#undef  CORRELATION             /* Background-error Correlation Check */
#undef  FORCING_SV              /* Forcing Singular Vectors */
#undef  FT_EIGENMODES           /* Finite Time Eigenmodes */
#define I4DVAR                  /* Incremental, strong constraint 4D-Var */
#undef  NLM_DRIVER              /* Nonlinear Basic State trajectory */
#undef  OPT_PERTURBATION        /* Optimal perturbations */
#undef  PICARD_TEST             /* Picard Iterations Test */
#undef  RBL4DVAR                /* Strong/Weak constraint RBL4D-Var */
#undef  R4DVAR                  /* Strong/Weak constraint R4D-Var */
#undef  R_SYMMETRY              /* Representer Matrix Symmetry Test */
#undef  SANITY_CHECK            /* Sanity Check */
#undef  SO_SEMI                 /* Stochastic Optimals: Semi-norm */
#undef  TLM_CHECK               /* Tangent Linear Model Check */

#define UV_ADV
#define UV_COR
#ifdef NLM_DRIVER
# define UV_QDRAG
#else
# define UV_LDRAG
#endif
#define DJ_GRADPS
#define UV_VIS2
#define MIX_S_UV
#define SPLINES_VDIFF
#define SPLINES_VVISC
#define TS_U3HADVECTION
#define TS_C4VADVECTION
#undef  TS_DIF2
#undef  MIX_GEO_TS
#define SOLVE3D
#define SALINITY
#define NONLIN_EOS
#define CURVGRID
#define MASKING
#ifdef NLM_DRIVER
# define SOLAR_SOURCE
#endif

#define ANA_BSFLUX
#define ANA_BTFLUX
#define ANA_SSFLUX

#undef  BULK_FLUXES
#ifdef BULK_FLUXES
# undef LONGWAVE
# ifdef LONGWAVE
#  define ANA_CLOUD
# endif
# undef  SRELAXATION
# define ANA_RAIN
# define ANA_SSFLUX
#endif

#ifdef NLM_DRIVER
# define LMD_MIXING
#endif
#ifdef LMD_MIXING
# define LMD_RIMIX
# define LMD_CONVEC
# define LMD_SKPP
# define LMD_NONLOCAL
# define RI_SPLINES
#endif

#ifndef NLM_DRIVER
# undef  FULL_GRID
# define VCONVOLUTION
# define IMPLICIT_VCONV
#endif
#define FORWARD_WRITE
#define FORWARD_MIXING
#ifndef NLM_DRIVER
# define FORWARD_READ
#endif
#define OUT_DOUBLE
