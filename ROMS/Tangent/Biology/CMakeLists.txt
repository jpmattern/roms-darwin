# svn $Id: CMakeLists.txt 1210 2024-01-03 22:03:03Z arango $
#:::::::::::::::::::::::::::::::::::::::::::::::::::::: David Robertson :::
# Copyright (c) 2002-2024 The ROMS/TOMS Group                           :::
#   Licensed under a MIT/X style license                                :::
#   See License_ROMS.md                                                 :::
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
# Source code list for sub-directory "ROMS/Tangent/Biology"

list( APPEND _files
      ROMS/Tangent/Biology/tl_biology.F
)

set( ROMS_Tangent_Biology_files
     ${_files}
     PARENT_SCOPE
)
