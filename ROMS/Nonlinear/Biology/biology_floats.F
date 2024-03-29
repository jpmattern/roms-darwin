#include "cppdefs.h"
#if defined NONLINEAR && defined FLOATS && defined FLOAT_BIOLOGY
!
!svn $Id: biology_floats.F 1210 2024-01-03 22:03:03Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2024 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.md                                               !
!=======================================================================
!                                                                      !
!  These routine computes the behavior for biological floats.          !
!                                                                      !
!=======================================================================
!
# ifdef FLOAT_OYSTER
#  include <oyster_floats.h>
# endif
#else
      MODULE biology_floats_mod
      END MODULE biology_floats_mod
#endif
