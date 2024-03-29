#include "cppdefs.h"
/*
** svn $Id: coupler.F 1210 2024-01-03 22:03:03Z arango $
*************************************************** Hernan G. Arango ***
** Copyright (c) 2002-2024 The ROMS/TOMS Group                        **
**   Licensed under a MIT/X style license                             **
**   See License_ROMS.md                                              **
************************************************************************
**                                                                    **
**  Module include several routines used for the couplig of ROMS/TOMS **
**  to other models using the ESMF/NUOPC or MCT libraries.            **
**                                                                    **
************************************************************************
*/

#if defined MODEL_COUPLING && !defined CMEPS
# if defined MCT_LIB
#  include "mct_coupler.h"
# elif defined ESMF_LIB
#  include "esmf_coupler.h"
# endif
#else
      MODULE coupler_mod
      END MODULE coupler_mod
#endif
