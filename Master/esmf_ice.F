#include "cppdefs.h"
#if defined MODEL_COUPLING && defined ESMF_LIB
/*
** svn $Id: esmf_ice.F 1210 2024-01-03 22:03:03Z arango $
*************************************************** Hernan G. Arango ***
** Copyright (c) 2002-2024 The ROMS/TOMS Group                        **
**   Licensed under a MIT/X style license                             **
**   See License_ROMS.md                                              **
************************************************************************
**                                                                    **
**  Seaice model gridded component coupled to ROMS using the ESMF     **
**  library with the NUOPC layer.                                     **
**                                                                    **
**  ESMF:   Earth System Modeling Framework (Version 7 or higher)     **
**            https://www.earthsystemcog.org/projects/esmf            **
**                                                                    **
**  NUOPC:  National Unified Operational Prediction Capability        **
**           https://www.earthsystemcog.org/projects/nuopc            **
**                                                                    **
************************************************************************
*/

# if defined ICE_COUPLING
#  if defined CICE_COUPLING_NOT_YET
#   include "esmf_ice_cice.h"
#  else
#   include "esmf_ice_void.h"
#  endif
# else
      MODULE esmf_ice_mod
      END MODULE esmf_ice_mod
# endif
#else
      MODULE esmf_ice_mod
      END MODULE esmf_ice_mod
#endif
