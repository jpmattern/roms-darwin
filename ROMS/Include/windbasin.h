/*
** svn $Id: windbasin.h 995 2020-01-10 04:01:28Z arango $
*******************************************************************************
** Copyright (c) 2002-2020 The ROMS/TOMS Group                               **
**   Licensed under a MIT/X style license                                    **
**   See License_ROMS.txt                                                    **
*******************************************************************************
**
** Options for Wind-Driven Constant Coriolis Basin Test.
**
** Application flag:   WINDBASIN
** Input script:       roms_windbasin.in
*/

#undef UV_ADV
#define UV_COR
#define UV_QDRAG
#define SPLINES_VDIFF
#define SPLINES_VVISC
#define SOLVE3D
#define AVERAGES
#define ANA_GRID
#define ANA_INITIAL
#define ANA_SMFLUX
#define ANA_STFLUX
#define ANA_BTFLUX

