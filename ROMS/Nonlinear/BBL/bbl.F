#include "cppdefs.h"
#if defined NONLINEAR && defined BBL_MODEL
!!
!!svn $Id: bbl.F 1210 2024-01-03 22:03:03Z arango $
!!================================================= Hernan G. Arango ===
!! Copyright (c) 2002-2024 The ROMS/TOMS Group                         !
!!   Licensed under a MIT/X style license                              !
!!   See License_ROMS.md                                               !
!!======================================================================
!!                                                                     !
!! This routine computes bottom momentum stress via a bottom boundary  !
!! layer formulation.                                                  !
!!                                                                     !
!!======================================================================
!!
# if defined SSW_BBL
#  include "ssw_bbl.h"
# elif defined MB_BBL
#  include "mb_bbl.h"
# elif defined SG_BBL
#  include "sg_bbl.h"
# else
      MODULE bbl_mod
      END MODULE bbl_mod
# endif
#else
      MODULE bbl_mod
      END MODULE bbl_mod
#endif
