#include "cppdefs.h"
      MODULE positive_mod
#if defined NONLINEAR && defined POSITIVEDEF
!
!svn $Id: positive.F 523 2011-01-05 03:21:38Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2011 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  Although positive definite property is applied in the NPZD model,   !
!  negative values appear due to the advection scheme.                 !
!  Thus, let's apply positive definite property again after step3d_t.  !
!                                                                      !
!=======================================================================
!
      implicit none

      PRIVATE
      PUBLIC  :: positive

      CONTAINS
# if defined NPZD_IRON
#  include <positive_npzd_iron.h>
# else
!
!  Note that this generic procedure does not conserve mass.
!
#  include <positive.h>
# endif

#endif
      END MODULE positive_mod
