!
! created by create_darwin_headerfiles (written by J. Paul Mattern)
!

/*
************************************************************************
**                                                                    **
**  Defines Darwin ecosystem model input parameters.                  **
**  This file is included in the file "def_info.F".                   **
**                                                                    **
************************************************************************
*/

!
!  Define Darwin ecosystem model parameters.
!

      Vinfo( 1)='isPhoto'
      Vinfo( 2)='plankton is photosynthetic'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='bactType'
      Vinfo( 2)='bacteria type'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='isAerobic'
      Vinfo( 2)='plankton is aerobic'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='isDenit'
      Vinfo( 2)='plankton is denitrifier'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='hasSi'
      Vinfo( 2)='plankton has silicate'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='hasPIC'
      Vinfo( 2)='plankton has PIC'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='diazo'
      Vinfo( 2)='plankton is diazotroph'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='useNH4'
      Vinfo( 2)='plankton uses NH4'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='useNO2'
      Vinfo( 2)='plankton uses NO2'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='useNO3'
      Vinfo( 2)='plankton uses NO3'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='combNO'
      Vinfo( 2)='plankton uses combined NOx'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='Xmin'
      Vinfo( 2)='plankton X min value'
      Vinfo( 3)='mmol C m-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='amminhib'
      Vinfo( 2)='plankton NH4 inhibition of NO uptake'
      Vinfo( 3)='(mmol N/m3)-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='acclimtimescl'
      Vinfo( 2)='inverse time scale for Chl acclimation'
      Vinfo( 3)='s^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='mort'
      Vinfo( 2)='linear mortality rate'
      Vinfo( 3)='s^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='mort2'
      Vinfo( 2)='quadratic mortality coefficient'
      Vinfo( 3)='(mmol C m^-3)^-1 s^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='tempMort'
      Vinfo( 2)='1: mort temperature dependent 0: not'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='tempMort2'
      Vinfo( 2)='1: mort2 temperature dependent 0: not'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='ExportFracMort'
      Vinfo( 2)='fraction of linear mortality to POM'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='ExportFracMort2'
      Vinfo( 2)='fraction of quadratic mortality to POM'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='ExportFrac'
      Vinfo( 2)='fraction of exudation to POM'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='phytoTempCoeff'
      Vinfo( 2)='phytoplankton temperature coefficient for photoFun'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='phytoTempExp1'
      Vinfo( 2)='phytoplankton temperature coefficient for photoFun'
      Vinfo( 3)='ln(degree)'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='phytoTempExp2'
      Vinfo( 2)='phytoplankton temperature coefficient for photoFun'
      Vinfo( 3)='(degree C)^-phytoDecayPower'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='phytoTempOptimum'
      Vinfo( 2)='phytoplankton optimal temperature'
      Vinfo( 3)='degree C'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='phytoDecayPower'
      Vinfo( 2)='phytoplankton temperature coefficient for photoFun'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='R_NC'
      Vinfo( 2)='plankton N:C ratio'
      Vinfo( 3)='mmol N (mmol C)-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='R_PC'
      Vinfo( 2)='plankton P:C ratio'
      Vinfo( 3)='mmol P (mmol C)-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='R_SiC'
      Vinfo( 2)='plankton Si:C ratio'
      Vinfo( 3)='mmol Si (mmol C)-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='R_FeC'
      Vinfo( 2)='plankton Fe:C ratio'
      Vinfo( 3)='mmol Fe (mmol C)-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='R_ChlC'
      Vinfo( 2)='Chl:C ratio'
      Vinfo( 3)='mg Chl (mmol C)^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='R_PICPOC'
      Vinfo( 2)='plankton PIC:POC ratio'
      Vinfo( 3)='mmol C (mmol C)-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='wsink'
      Vinfo( 2)='sinking rate'
      Vinfo( 3)='m s^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='wswim'
      Vinfo( 2)='upward swimming rate'
      Vinfo( 3)='m s^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='respiration'
      Vinfo( 2)='respiration rate'
      Vinfo( 3)='s^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='PCmax'
      Vinfo( 2)='maximum growth rate'
      Vinfo( 3)='s-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='Qnmax'
      Vinfo( 2)='maximum N quota'
      Vinfo( 3)='mmol N (mmol C)^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='Qnmin'
      Vinfo( 2)='minimum N quota'
      Vinfo( 3)='mmol N (mmol C)^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='Qpmax'
      Vinfo( 2)='maximum P quota'
      Vinfo( 3)='mmol P (mmol C)^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='Qpmin'
      Vinfo( 2)='minimum P quota'
      Vinfo( 3)='mmol P (mmol C)^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='Qsimax'
      Vinfo( 2)='maximum Si quota'
      Vinfo( 3)='mmol Si (mmol C)^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='Qsimin'
      Vinfo( 2)='minimum Si quota'
      Vinfo( 3)='mmol Si (mmol C)^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='Qfemax'
      Vinfo( 2)='maximum Fe quota'
      Vinfo( 3)='mmol Fe (mmol C)^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='Qfemin'
      Vinfo( 2)='minimum Fe quota'
      Vinfo( 3)='mmol Fe (mmol C)^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='Vmax_NH4'
      Vinfo( 2)='maximum NH4 uptake rate'
      Vinfo( 3)='mmol N (mmol C)^-1 s^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='Vmax_NO2'
      Vinfo( 2)='maximum NO2 uptake rate'
      Vinfo( 3)='mmol N (mmol C)^-1 s^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='Vmax_NO3'
      Vinfo( 2)='maximum NO3 uptake rate'
      Vinfo( 3)='mmol N (mmol C)^-1 s^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='Vmax_N'
      Vinfo( 2)='maximum N uptake rate'
      Vinfo( 3)='mmol N (mmol C)^-1 s^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='Vmax_PO4'
      Vinfo( 2)='maximum PO4 uptake rate'
      Vinfo( 3)='mmol P (mmol C)^-1 s^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='Vmax_SiO2'
      Vinfo( 2)='maximum SiO2 uptake rate'
      Vinfo( 3)='mmol Si (mmol C)^-1 s^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='Vmax_FeT'
      Vinfo( 2)='maximum Fe uptake rate'
      Vinfo( 3)='mmol Fe (mmol C)^-1 s^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='ksatNH4'
      Vinfo( 2)='plankton NH4 half-saturation concentration'
      Vinfo( 3)='mmol N m-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='ksatNO2'
      Vinfo( 2)='plankton NO2 half-saturation concentration'
      Vinfo( 3)='mmol N m-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='ksatNO3'
      Vinfo( 2)='plankton NO3 half-saturation concentration'
      Vinfo( 3)='mmol N m-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='ksatPO4'
      Vinfo( 2)='plankton PO4 half-saturation concentration'
      Vinfo( 3)='mmol P m-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='ksatSiO2'
      Vinfo( 2)='plankton SiO2 half-saturation concentration'
      Vinfo( 3)='mmol Si m-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='ksatFeT'
      Vinfo( 2)='plankton FeT half-saturation concentration'
      Vinfo( 3)='mmol Fe m-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='kexcC'
      Vinfo( 2)='C excretion rate'
      Vinfo( 3)='s^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='kexcN'
      Vinfo( 2)='N excretion rate'
      Vinfo( 3)='s^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='kexcP'
      Vinfo( 2)='P excretion rate'
      Vinfo( 3)='s^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='kexcSi'
      Vinfo( 2)='Si excretion rate'
      Vinfo( 3)='s^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='kexcFe'
      Vinfo( 2)='Fe excretion rate'
      Vinfo( 3)='s^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#if defined DARWIN_GEIDER
      Vinfo( 1)='inhibcoef_geid'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
#if ! defined DARWIN_GEIDER
      Vinfo( 1)='ksatPAR'
      Vinfo( 2)='plankton PAR saturation coefficient'
      Vinfo( 3)='(uEin m-2 s-1)-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='kinhPAR'
      Vinfo( 2)='plankton PAR inhibition coefficient'
      Vinfo( 3)='(uEin m-2 s-1)-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
      Vinfo( 1)='mQyield'
      Vinfo( 2)='maximum quantum yield'
      Vinfo( 3)='mmol C (uEin)^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='chl2cmax'
      Vinfo( 2)='maximum chl:C ratio'
      Vinfo( 3)='mg Chl (mmol C)^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='grazemax'
      Vinfo( 2)='plankton maximum grazing rate'
      Vinfo( 3)='s-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='kgrazesat'
      Vinfo( 2)='plankton grazing saturation'
      Vinfo( 3)='mmol C m-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='palat'
      Vinfo( 2)='grazing palatability'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  2, (/plankdim,plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='asseff'
      Vinfo( 2)='assimilation efficiency'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  2, (/plankdim,plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='ExportFracPreyPred'
      Vinfo( 2)='grazing export fraction'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  2, (/plankdim,plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='yield'
      Vinfo( 2)='bacterial growth yield for all organic matter'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='yieldO2'
      Vinfo( 2)='bacterial growth yield for oxygen'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='yieldNO3'
      Vinfo( 2)='bacterial growth yield for nitrate'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='ksatPON'
      Vinfo( 2)='plankton PON half-saturation concentration'
      Vinfo( 3)='mmol N m-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='ksatPOC'
      Vinfo( 2)='plankton POC half-saturation concentration'
      Vinfo( 3)='mmol C m-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='ksatPOP'
      Vinfo( 2)='plankton POP half-saturation concentration'
      Vinfo( 3)='mmol P m-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='ksatPOFe'
      Vinfo( 2)='plankton POFe half-saturation concentration'
      Vinfo( 3)='mmol Fe m-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='ksatDON'
      Vinfo( 2)='plankton DON half-saturation concentration'
      Vinfo( 3)='mmol N m-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='ksatDOC'
      Vinfo( 2)='plankton DOC half-saturation concentration'
      Vinfo( 3)='mmol C m-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='ksatDOP'
      Vinfo( 2)='plankton DOP half-saturation concentration'
      Vinfo( 3)='mmol P m-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='ksatDOFe'
      Vinfo( 2)='plankton DOFe half-saturation concentration'
      Vinfo( 3)='mmol Fe m-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#if defined DARWIN_RADTRANS
      Vinfo( 1)='aphy_chl'
      Vinfo( 2)='chlorophyll absorption'
      Vinfo( 3)='m^-1 (mg Chl m^-3)^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  2, (/plankdim,lamdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='aphy_chl_ps'
      Vinfo( 2)='chlorophyll PS absorption'
      Vinfo( 3)='m^-1 (mg Chl m^-3)^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  2, (/plankdim,lamdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='bphy_mgC'
      Vinfo( 2)='phytoplankton backscatter'
      Vinfo( 3)='m^-1 (mg C m^-3)^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  2, (/plankdim,lamdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='bbphy_mgC'
      Vinfo( 2)='phytoplankton backscatter'
      Vinfo( 3)='m^-1 (mg C m^-3)^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  2, (/plankdim,lamdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
#if ! defined DARWIN_GEIDER
      Vinfo( 1)='normI'
      Vinfo( 2)='plankton light limit coefficient'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
#if defined DARWIN_RADTRANS
      Vinfo( 1)='ap_type'
      Vinfo( 2)='absorption spectra type'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
#if ! defined DARWIN_RANDOM_TRAITS
      Vinfo( 1)='biovol'
      Vinfo( 2)='cell volume'
      Vinfo( 3)='um^3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
      Vinfo( 1)='group'
      Vinfo( 2)='group membership'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#if ! defined DARWIN_RANDOM_TRAITS
      Vinfo( 1)='biovol_bygroup'
      Vinfo( 2)='cell volume'
      Vinfo( 3)='um^3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  2, (/plankdim,groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
      Vinfo( 1)='alphachl'
      Vinfo( 2)='coefficient for initial slope of P-I curve'
      Vinfo( 3)='mmol C s-1 (uEin m^-2 s^-1)^-1 (mg Chl)^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  2, (/plankdim,lamdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='alpha_mean'
      Vinfo( 2)='mean alpha_I per waveband'
      Vinfo( 3)='mmol C s-1 (uEin m^-2 s^-1)^-1 (mg Chl)^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='chl2cmin'
      Vinfo( 2)='minimum chl:C ratio'
      Vinfo( 3)='mg Chl (mmol C)^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='mortTempFuncMin'
      Vinfo( 2)='minimum mortTempFunc threshold'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='mort2TempFuncMin'
      Vinfo( 2)='minimum mort2TempFunc threshold'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#if defined DARWIN_RANDOM_TRAITS
      Vinfo( 1)='Smallgrow'
      Vinfo( 2)='small phytoplankton growth rate'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='Biggrow'
      Vinfo( 2)='big phytoplankton growth rates'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='Smallgrowrange'
      Vinfo( 2)='small phytoplankton growth rate threshold'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='Biggrowrange'
      Vinfo( 2)='big phytoplankton growth rate threshold'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='diaz_growfac'
      Vinfo( 2)='diazotroph growth multiplier'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='cocco_growfac'
      Vinfo( 2)='coccolithophore growth multiplier'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='diatom_growfac'
      Vinfo( 2)='diatom growth multiplier'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='Smallmort'
      Vinfo( 2)='small phytoplankton mortality rate'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='Bigmort'
      Vinfo( 2)='big phytoplankton mortality rate'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='Smallmortrange'
      Vinfo( 2)='small phytoplankton mortality rate threshold'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='Bigmortrange'
      Vinfo( 2)='big phytoplankton mortality rate threshold'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='Smallexport'
      Vinfo( 2)='small phytoplankton mortality export fraction'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='Bigexport'
      Vinfo( 2)='big phytoplankton mortality export fraction'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='tempcoeff1'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='tempcoeff2_small'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='tempcoeff2_big'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='tempcoeff3'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
#if defined DARWIN_RANDOM_TRAITS && defined DARWIN_TEMP_RANGE
      Vinfo( 1)='tempmax'
      Vinfo( 2)='32. _d 0'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='temprange'
      Vinfo( 2)='30. _d 0'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='tempdecay'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
#if defined DARWIN_RANDOM_TRAITS
      Vinfo( 1)='val_R_NC'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='val_R_NC_diaz'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='val_R_PC'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='val_R_SiC_diatom'
      Vinfo( 2)='32 for Fannys runs'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='val_R_FeC'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='val_R_FeC_diaz'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='val_R_PICPOC'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
#if defined DARWIN_RANDOM_TRAITS && ! defined DARWIN_GEIDER
      Vinfo( 1)='val_R_ChlC'
      Vinfo( 2)='for atten_chl   = atten_p * 16'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
#if defined DARWIN_RANDOM_TRAITS
      Vinfo( 1)='val_R_NC_zoo'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='val_R_PC_zoo'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='val_R_SiC_zoo'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='val_R_FeC_zoo'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='val_R_PICPOC_zoo'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='val_R_ChlC_zoo'
      Vinfo( 2)='for atten_chl = atten_p * 16'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='SmallSink_pday'
      Vinfo( 2)='small phytoplankton sinking rate'
      Vinfo( 3)='m d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='BigSink_pday'
      Vinfo( 2)='big phytoplankton sinking rate'
      Vinfo( 3)='m d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='SmallPsat'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='BigPsat'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='ProcPsat'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='UniDzPsat'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='CoccoPsat'
      Vinfo( 2)='by default same as big'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='SmallPsatrange'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='BigPsatrange'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='ProcPsatrange'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='UniDzPsatrange'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='CoccoPsatrange'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='ksatNH4fac'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='ksatNO2fac'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='val_amminhib'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='coefficient for NH4 inhibition of NO uptake ((mmol N/m3)-1)'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='val_ksatsio2'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
#if defined DARWIN_RANDOM_TRAITS && ! defined DARWIN_GEIDER
      Vinfo( 1)='smallksatpar'
      Vinfo( 2)='0.8 _d 0'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='smallksatparstd'
      Vinfo( 2)='0.3 _d 0'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='smallkinhpar'
      Vinfo( 2)='2.0 _d 0'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='smallkinhparstd'
      Vinfo( 2)='0.5 _d 0'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='Bigksatpar'
      Vinfo( 2)='0.35 _d 0'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='Bigksatparstd'
      Vinfo( 2)='0.1 _d 0'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='Bigkinhpar'
      Vinfo( 2)='0.5 _d 0'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='Bigkinhparstd'
      Vinfo( 2)='0.1 _d 0'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
#if defined DARWIN_RANDOM_TRAITS && defined DARWIN_NINE_SPECIES_SETUP
      Vinfo( 1)='LLProkinhpar'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='Coccokinhpar'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
#if defined DARWIN_RANDOM_TRAITS
      Vinfo( 1)='inhibcoef_geid_val'
      Vinfo( 2)='DUMMY VAL'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='smallmQyield'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='mmol C (uEin)-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='smallmQyieldrange'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='mmol C (uEin)-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='BigmQyield'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='mmol C (uEin)-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='BigmQyieldrange'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='mmol C (uEin)-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='smallchl2cmax'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='mg Chl (mmol C)'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='smallchl2cmaxrange'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='mg Chl (mmol C)'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='Bigchl2cmax'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='mg Chl (mmol C)'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='Bigchl2cmaxrange'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='mg Chl (mmol C)'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
#if ! defined DARWIN_RADTRANS
      Vinfo( 1)='aphy_chl_ave'
      Vinfo( 2)='multiplication with chla gives absorption m-1'
      Vinfo( 3)='m^2 (mg_chl)^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
#if defined DARWIN_RANDOM_TRAITS
      Vinfo( 1)='val_acclimtimescl'
      Vinfo( 2)='inverse time scale for Chl acclimation'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='oldTwoGrazers'
      Vinfo( 2)='old defaults for 2 grazers'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='GrazeFast_pday'
      Vinfo( 2)='maximum grazing rate'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='ZooexfacSmall'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='ZooexfacBig'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='ZoomortSmall_pday'
      Vinfo( 2)='small zooplankton mortality rate'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='ZoomortBig_pday'
      Vinfo( 2)='big zooplankton mortality rate'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='ZoomortSmall2'
      Vinfo( 2)='small zooplankton quadratic mortality rate'
      Vinfo( 3)='(mmol C m^-3)^-1 s^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='ZoomortBig2'
      Vinfo( 2)='big zooplankton quadratic mortality rate'
      Vinfo( 3)='(mmol C m^-3)^-1 s^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='ExGrazfracbig'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='ExGrazfracsmall'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='palathi'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='palatlo'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='diatomgraz'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='coccograz'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='olargegraz'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='GrazeEfflow'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='GrazeEffmod'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='GrazeEffhi'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='GrazeRate_pday'
      Vinfo( 2)='grazing rate'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='ExGrazfrac'
      Vinfo( 2)='fraction of sloppy feeding that goes to particulate'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='val_palat'
      Vinfo( 2)='need to set in data.traits'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='val_ass_eff'
      Vinfo( 2)='grazing efficiency'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='kgrazesat_val'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='= 0.1 mmol P m-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='Zoomort_pday'
      Vinfo( 2)='zooplankton mortality rate'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='Zoomort2'
      Vinfo( 2)='zooplankton quadratic mortality rate'
      Vinfo( 3)='(mmol C m^-3)^-1 s^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='Zooexfac'
      Vinfo( 2)='fraction of dead zoo that goes to particulate'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='ZooDM'
      Vinfo( 2)='diameter (not used so far)'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
#if ! defined DARWIN_RANDOM_TRAITS
      Vinfo( 1)='gud_sort_biovol'
      Vinfo( 2)='sort plankton types by biovol'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
      Vinfo( 1)='GUD_effective_ksat'
      Vinfo( 2)='compute effective half sat for uptake of non-quota elements'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='gud_select_kn_allom'
      Vinfo( 2)='1: use Ward et al formulation, 2: use Follett et al'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='logvolbase'
      Vinfo( 2)='biovol = 10**(logvolbase+...)'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#if ! defined DARWIN_RANDOM_TRAITS
      Vinfo( 1)='logvolinc'
      Vinfo( 2)='used to compute biovol (not always active)'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
      Vinfo( 1)='biovol0'
      Vinfo( 2)='used to compute biovol (not always active)'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='biovolfac'
      Vinfo( 2)='used to compute biovol (not always active)'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#if ! defined DARWIN_RANDOM_TRAITS
      Vinfo( 1)='logvol0ind'
      Vinfo( 2)='used to compute biovol (not always active)'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='grp_logvolind'
      Vinfo( 2)='group value used to determine biovol'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  2, (/plankdim,groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='grp_biovol'
      Vinfo( 2)='group value for biovol'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  2, (/plankdim,groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='grp_nplank'
      Vinfo( 2)='number of plankton variables in group'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='grp_photo'
      Vinfo( 2)='group perform photosynthesis'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='grp_bacttype'
      Vinfo( 2)='1: particle-associated, 2: free-living'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='grp_aerobic'
      Vinfo( 2)='group value for isAerobic'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='grp_denit'
      Vinfo( 2)='group is denitrifier'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='grp_pred'
      Vinfo( 2)='group is predator'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='grp_prey'
      Vinfo( 2)='group is prey'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='grp_hasSi'
      Vinfo( 2)='group has silicate (diatom)'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='grp_hasPIC'
      Vinfo( 2)='group has PIC (coccolithophore)'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='grp_diazo'
      Vinfo( 2)='group is diazotroph'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='grp_useNH4'
      Vinfo( 2)='group uses NH4'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='grp_useNO2'
      Vinfo( 2)='group uses NO2'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='grp_useNO3'
      Vinfo( 2)='group uses NO3'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='grp_combNO'
      Vinfo( 2)='group value for combNO'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
#if defined DARWIN_RADTRANS && ! defined DARWIN_RANDOM_TRAITS
      Vinfo( 1)='grp_aptype'
      Vinfo( 2)='group absorption spectra type'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
#if ! defined DARWIN_RANDOM_TRAITS
      Vinfo( 1)='grp_tempMort'
      Vinfo( 2)='group value for tempmort'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='grp_tempMort2'
      Vinfo( 2)='group value for tempmort2'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='grp_Xmin'
      Vinfo( 2)='group value for Xmin'
      Vinfo( 3)='mmol C m^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='grp_R_NC'
      Vinfo( 2)='group value N:C ratio'
      Vinfo( 3)='mmol N (mmol C)^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='grp_R_PC'
      Vinfo( 2)='group value P:C ratio'
      Vinfo( 3)='mmol P (mmol C)^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='grp_R_SiC'
      Vinfo( 2)='group value Si:C ratio'
      Vinfo( 3)='mmol Si (mmol C)^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='grp_R_FeC'
      Vinfo( 2)='group value Fe:C ratio'
      Vinfo( 3)='mmol Fe (mmol C)^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='grp_R_ChlC'
      Vinfo( 2)='group value Chl:C ratio'
      Vinfo( 3)='mg Chl (mmol C)^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='grp_R_PICPOC'
      Vinfo( 2)='group value PIC:POC ratio'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='grp_ExportFracMort'
      Vinfo( 2)='group value for fraction of linear mortality to POM'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='grp_ExportFracMort2'
      Vinfo( 2)='group value for fraction of quadratic mortality to POM'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='grp_ExportFrac'
      Vinfo( 2)='group value for fraction of exudation to POM'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='grp_mort_pday'
      Vinfo( 2)='group mortality rate'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='grp_mort2'
      Vinfo( 2)='group quadratic mortality rate'
      Vinfo( 3)='(mmol C m^-3)^-1 s^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='grp_tempcoeff1'
      Vinfo( 2)='group value for phytoTempCoeff value'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='grp_tempcoeff2'
      Vinfo( 2)='group value for phytoTempExp2'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='grp_tempcoeff3'
      Vinfo( 2)='group value for phytoTempExp1'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='grp_tempopt'
      Vinfo( 2)='group value for phytoTempOptimum'
      Vinfo( 3)='deg_C'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='grp_tempdecay'
      Vinfo( 2)='group value for phytoDecayPower'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
#if defined DARWIN_ALLOMETRIC_PALAT && ! defined DARWIN_RANDOM_TRAITS
      Vinfo( 1)='grp_pp_sig'
      Vinfo( 2)='standard deviation of size preference'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
#if ! defined DARWIN_RANDOM_TRAITS && defined DARWIN_GEIDER
      Vinfo( 1)='grp_mQyield'
      Vinfo( 2)='group value for mQyield'
      Vinfo( 3)='mmol C (uEin)^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='grp_chl2cmax'
      Vinfo( 2)='group value max chl:C ratio'
      Vinfo( 3)='mg Chl (mmol C)^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='grp_inhibcoef_geid'
      Vinfo( 2)='group value for Geider light inhibition coefficient'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
#if ! defined DARWIN_RANDOM_TRAITS && ! defined DARWIN_GEIDER
      Vinfo( 1)='grp_ksatPAR'
      Vinfo( 2)='group value for ksatPAR'
      Vinfo( 3)='(uEin m^-2 s^-1)^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='grp_kinhPAR'
      Vinfo( 2)='group value plankton PAR inhibition coefficient'
      Vinfo( 3)='(uEin m^-2 s^-1)^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
#if ! defined DARWIN_RANDOM_TRAITS
      Vinfo( 1)='grp_ksatNH4fac'
      Vinfo( 2)='group value for ksatNH4 multiplier'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='grp_ksatNO2fac'
      Vinfo( 2)='group value for ksatNO2 multiplier'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='grp_amminhib'
      Vinfo( 2)='group coefficient for NH4 inhibition of NO uptake'
      Vinfo( 3)='(mmol N/m^3)^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='grp_acclimtimescl_pday'
      Vinfo( 2)='inverse time scale for Chl acclimation'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='a_graz_pday'
      Vinfo( 2)='a-coefficient for grazemax'
      Vinfo( 3)='d^-1 um^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='b_graz'
      Vinfo( 2)='b-coefficient for grazemax'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='a_kg'
      Vinfo( 2)='a-coefficient for kgrazesat'
      Vinfo( 3)='mmol C m^-3 um^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='b_kg'
      Vinfo( 2)='b-coefficient for kgrazesat'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='a_biosink_pday'
      Vinfo( 2)='a-coefficient for wsink'
      Vinfo( 3)='m d^-1 um^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='b_biosink'
      Vinfo( 2)='b-coefficient for wsink'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='a_bioswim_pday'
      Vinfo( 2)='a-coefficient for wswim'
      Vinfo( 3)='m d^-1 um^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='b_bioswim'
      Vinfo( 2)='b-coefficient for wswim'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
#if defined DARWIN_ALLOMETRIC_PALAT && ! defined DARWIN_RANDOM_TRAITS
      Vinfo( 1)='a_prdpry'
      Vinfo( 2)='a-coefficient for pp_opt'
      Vinfo( 3)='um^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='b_prdpry'
      Vinfo( 2)='b-coefficient for pp_opt'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
#if ! defined DARWIN_RANDOM_TRAITS
      Vinfo( 1)='a_vmax_DIC_pday'
      Vinfo( 2)='a-coefficient for PCmax'
      Vinfo( 3)='d^-1 um^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='b_vmax_DIC'
      Vinfo( 2)='b-coefficient for PCmax'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='a_qcarbon'
      Vinfo( 2)='a-coefficient for qcarbon'
      Vinfo( 3)='um^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='b_qcarbon'
      Vinfo( 2)='b-coefficient for qcarbon'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='a_respir'
      Vinfo( 2)='a-coefficient for respiration'
      Vinfo( 3)='mmol C cell^-1 s^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='b_respir'
      Vinfo( 2)='b-coefficient for respiration'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='a_kexc_c'
      Vinfo( 2)='a-coefficient for kexcC'
      Vinfo( 3)='s^-1 um^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='b_kexc_c'
      Vinfo( 2)='b-coefficient for kexcC'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='a_vmax_NO3_pday'
      Vinfo( 2)='a-coefficient for Vmax_NO3'
      Vinfo( 3)='mmol N (mmol C)^-1 d^-1 um^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='b_vmax_NO3'
      Vinfo( 2)='b-coefficient for Vmax_NO3'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='a_kn_NO3'
      Vinfo( 2)='a-coefficient for ksatNO3'
      Vinfo( 3)='mmol N m-3 um^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='b_kn_NO3'
      Vinfo( 2)='b-coefficient for ksatNO3'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='a_qmin_n'
      Vinfo( 2)='a-coefficient for Qnmin'
      Vinfo( 3)='mmol N (mmol C)^-1 um^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='b_qmin_n'
      Vinfo( 2)='b-coefficient for Qnmin'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='a_qmax_n'
      Vinfo( 2)='a-coefficient for Qnmax'
      Vinfo( 3)='mmol N (mmol C)^-1 um^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='b_qmax_n'
      Vinfo( 2)='b-coefficient for Qnmax'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='a_kexc_n'
      Vinfo( 2)='a-coefficient for kexcN'
      Vinfo( 3)='s^-1 um^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='b_kexc_n'
      Vinfo( 2)='b-coefficient for kexcN'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='a_vmax_NO2_pday'
      Vinfo( 2)='a-coefficient for Vmax_NO2'
      Vinfo( 3)='mmol N (mmol C)^-1 d^-1 um^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='b_vmax_NO2'
      Vinfo( 2)='b-coefficient for Vmax_NO2'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='a_kn_NO2'
      Vinfo( 2)='a-coefficient for ksatNO2'
      Vinfo( 3)='mmol N m-3 um^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='b_kn_NO2'
      Vinfo( 2)='b-coefficient for ksatNO2'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='a_vmax_NH4_pday'
      Vinfo( 2)='a-coefficient for Vmax_NH4'
      Vinfo( 3)='mmol N (mmol C)^-1 d^-1 um^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='b_vmax_NH4'
      Vinfo( 2)='b-coefficient for Vmax_NH4'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='a_kn_NH4'
      Vinfo( 2)='a-coefficient for ksatNH4'
      Vinfo( 3)='mmol N m-3 um^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='b_kn_NH4'
      Vinfo( 2)='b-coefficient for ksatNH4'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='a_vmax_N_pday'
      Vinfo( 2)='a-coefficient for Vmax_N'
      Vinfo( 3)='mmol N (mmol C)^-1 d^-1 um^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='b_vmax_N'
      Vinfo( 2)='b-coefficient for Vmax_N'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='a_vmax_PO4_pday'
      Vinfo( 2)='a-coefficient for Vmax_PO4'
      Vinfo( 3)='mmol P (mmol C)^-1 d^-1 um^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='b_vmax_PO4'
      Vinfo( 2)='b-coefficient for Vmax_PO4'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='a_kn_PO4'
      Vinfo( 2)='a-coefficient for ksatPO4'
      Vinfo( 3)='mmol P m-3 um^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='b_kn_PO4'
      Vinfo( 2)='b-coefficient for ksatPO4'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='a_qmin_p'
      Vinfo( 2)='a-coefficient for Qpmin'
      Vinfo( 3)='mmol P (mmol C)^-1 um^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='b_qmin_p'
      Vinfo( 2)='b-coefficient for Qpmin'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='a_qmax_p'
      Vinfo( 2)='a-coefficient for Qpmax'
      Vinfo( 3)='mmol P (mmol C)^-1 um^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='b_qmax_p'
      Vinfo( 2)='b-coefficient for Qpmax'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='a_kexc_p_pday'
      Vinfo( 2)='a-coefficient for kexcP'
      Vinfo( 3)='d^-1 um^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='b_kexc_p'
      Vinfo( 2)='b-coefficient for kexcP'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='a_vmax_SiO2_pday'
      Vinfo( 2)='a-coefficient for Vmax_SiO2'
      Vinfo( 3)='mmol Si (mmol C)^-1 d^-1 um^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='b_vmax_SiO2'
      Vinfo( 2)='b-coefficient for Vmax_SiO2'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='a_kn_SiO2'
      Vinfo( 2)='a-coefficient for ksatSiO2'
      Vinfo( 3)='mmol Si m-3 um^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='b_kn_SiO2'
      Vinfo( 2)='b-coefficient for ksatSiO2'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='a_qmin_si'
      Vinfo( 2)='a-coefficient for Qsimin'
      Vinfo( 3)='mmol Si (mmol C)^-1 um^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='b_qmin_si'
      Vinfo( 2)='b-coefficient for Qsimin'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='a_qmax_si'
      Vinfo( 2)='a-coefficient for Qsimax'
      Vinfo( 3)='mmol Si (mmol C)^-1 um^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='b_qmax_si'
      Vinfo( 2)='b-coefficient for Qsimax'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='a_kexc_si_pday'
      Vinfo( 2)='a-coefficient for kexcSi'
      Vinfo( 3)='d^-1 um^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='b_kexc_si'
      Vinfo( 2)='b-coefficient for kexcSi'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='a_vmax_FeT_pday'
      Vinfo( 2)='a-coefficient for Vmax_FeT'
      Vinfo( 3)='mmol Fe (mmol C)^-1 d^-1 um^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='b_vmax_FeT'
      Vinfo( 2)='b-coefficient for Vmax_FeT'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='a_kn_feT'
      Vinfo( 2)='a-coefficient for ksatFeT'
      Vinfo( 3)='mmol Fe m-3 um^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='b_kn_FeT'
      Vinfo( 2)='b-coefficient for ksatFeT'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='a_qmin_fe'
      Vinfo( 2)='a-coefficient for Qfemin'
      Vinfo( 3)='mmol Fe (mmol C)^-1 um^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='b_qmin_fe'
      Vinfo( 2)='b-coefficient for Qfemin'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='a_qmax_fe'
      Vinfo( 2)='a-coefficient for Qfemax'
      Vinfo( 3)='mmol Fe (mmol C)^-1 um^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='b_qmax_fe'
      Vinfo( 2)='b-coefficient for Qfemax'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='a_kexc_fe_pday'
      Vinfo( 2)='a-coefficient for kexcFe'
      Vinfo( 3)='d^-1 um^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='b_kexc_fe'
      Vinfo( 2)='b-coefficient for kexcFe'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='grp_ExportFracPreyPred'
      Vinfo( 2)='group value for ExportFracPreyPred'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  2, (/groupdim,groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='grp_ass_eff'
      Vinfo( 2)='group value for asseff'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  2, (/groupdim,groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
#if defined DARWIN_USE_PLOAD
      Vinfo( 1)='Pa2Atm'
      Vinfo( 2)='conversion factor for atmospheric pressure'
      Vinfo( 3)='Pa/atm'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
      Vinfo( 1)='ptr2mol'
      Vinfo( 2)='convert ptracers (in mmol/m3) to mol/m3'
      Vinfo( 3)='mol/mmol'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='sca1'
      Vinfo( 2)='coefficient for Schmidt No computation'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='sca2'
      Vinfo( 2)='coefficient for Schmidt No computation'
      Vinfo( 3)='deg_C^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='sca3'
      Vinfo( 2)='coefficient for Schmidt No computation'
      Vinfo( 3)='deg_C^-2'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='sca4'
      Vinfo( 2)='coefficient for Schmidt No computation'
      Vinfo( 3)='deg_C^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='sox1'
      Vinfo( 2)='coefficient for O2 Schmidt No computation'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='sox2'
      Vinfo( 2)='coefficient for O2 Schmidt No computation'
      Vinfo( 3)='deg_C^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='sox3'
      Vinfo( 2)='coefficient for O2 Schmidt No computation'
      Vinfo( 3)='deg_C^-2'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='sox4'
      Vinfo( 2)='coefficient for O2 Schmidt No computation'
      Vinfo( 3)='deg_C^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='oA0'
      Vinfo( 2)='coefficient for O2 saturation computation'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='oA1'
      Vinfo( 2)='coefficient for O2 saturation computation'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='oA2'
      Vinfo( 2)='coefficient for O2 saturation computation'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='oA3'
      Vinfo( 2)='coefficient for O2 saturation computation'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='oA4'
      Vinfo( 2)='coefficient for O2 saturation computation'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='oA5'
      Vinfo( 2)='coefficient for O2 saturation computation'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='oB0'
      Vinfo( 2)='coefficient for O2 saturation computation'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='oB1'
      Vinfo( 2)='coefficient for O2 saturation computation'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='oB2'
      Vinfo( 2)='coefficient for O2 saturation computation'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='oB3'
      Vinfo( 2)='coefficient for O2 saturation computation'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='oC0'
      Vinfo( 2)='coefficient for O2 saturation computation'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#if defined DARWIN_RANDOM_TRAITS
      Vinfo( 1)='phymin'
      Vinfo( 2)='minimum phyto (below which grazing and mortality doesnt happen)'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
      Vinfo( 1)='katten_w'
      Vinfo( 2)='atten coefficient water'
      Vinfo( 3)='m-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='katten_chl'
      Vinfo( 2)='atten coefficient chl'
      Vinfo( 3)='(mmol chl/m3)-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='parfrac'
      Vinfo( 2)='fraction Qsw that is PAR'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#if DARWIN_TEMP_VERSION == 1
      Vinfo( 1)='tempnorm'
      Vinfo( 2)='phytoplankton temperature coefficient for photoFun'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
#if DARWIN_TEMP_VERSION == 2
      Vinfo( 1)='TempAeArr'
      Vinfo( 2)='coefficient for pseudo-Arrhenius'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='TemprefArr'
      Vinfo( 2)='coefficient for pseudo-Arrhenius'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='TempCoeffArr'
      Vinfo( 2)='coefficient for pseudo-Arrhenius'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
      Vinfo( 1)='alpfe'
      Vinfo( 2)='solubility of Fe dust'
      Vinfo( 3)='m s^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#if ! defined DARWIN_PART_SCAV_POP && ! defined DARWIN_PART_SCAV
      Vinfo( 1)='scav_pyear'
      Vinfo( 2)='iron chem scavenging rate'
      Vinfo( 3)='year^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
      Vinfo( 1)='ligand_tot'
      Vinfo( 2)='total ligand'
      Vinfo( 3)='mmol m^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='ligand_stab'
      Vinfo( 2)='ligand stability rate ratio'
      Vinfo( 3)='m^3 mol^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#if defined DARWIN_MINFE
      Vinfo( 1)='freefemax'
      Vinfo( 2)='maximum value for free iron'
      Vinfo( 3)='mmol Fe m^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
#if defined DARWIN_PART_SCAV_POP || defined DARWIN_PART_SCAV
      Vinfo( 1)='scav_rat_pday'
      Vinfo( 2)='iron chem scavenging rate'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='scav_inter'
      Vinfo( 2)='iron chem scavenging coefficient'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='scav_exp'
      Vinfo( 2)='iron chem scavenging coefficient'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
#if defined DARWIN_PART_SCAV_POP
      Vinfo( 1)='scav_R_POPPOC'
      Vinfo( 2)='scavenging POP:POC ratio'
      Vinfo( 3)='mmol P/mmol C'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
#if ! defined DARWIN_IRON_SED_SOURCE_VARIABLE
      Vinfo( 1)='fesedflux_pday'
      Vinfo( 2)='iron flux'
      Vinfo( 3)='mmol Fe m^-2 d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
#if defined DARWIN_IRON_SED_SOURCE_VARIABLE
      Vinfo( 1)='fesedflux_pcm'
      Vinfo( 2)='iron flux relative to POC flux'
      Vinfo( 3)='mmol Fe/mmol C'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='R_CP_fesed'
      Vinfo( 2)='sedimentary C:P ratio for iron flux computation'
      Vinfo( 3)='mmol C/mmol P'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
      Vinfo( 1)='Knita_pday'
      Vinfo( 2)='oxidation rates for ammonium'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='Knitb_pday'
      Vinfo( 2)='oxidation rates for nitrite'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='PAR_oxi'
      Vinfo( 2)='critical light level after which oxidation starts'
      Vinfo( 3)='uEin/m2/s'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='Kdoc_pday'
      Vinfo( 2)='DOC remin rate'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='Kdop_pday'
      Vinfo( 2)='DOP remin rate'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='Kdon_pday'
      Vinfo( 2)='DON remin rate'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='KdoFe_pday'
      Vinfo( 2)='DOFe remin rate'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='KPOC_pday'
      Vinfo( 2)='POC remin rate'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='KPOP_pday'
      Vinfo( 2)='POP remin rate'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='KPON_pday'
      Vinfo( 2)='PON remin rate'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='KPOFe_pday'
      Vinfo( 2)='POFe remin rate'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='KPOSi_pday'
      Vinfo( 2)='POSi remin rate'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='wC_sink_pday'
      Vinfo( 2)='POC sinking rate'
      Vinfo( 3)='m/s s d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='wP_sink_pday'
      Vinfo( 2)='POP sinking rate'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='wN_sink_pday'
      Vinfo( 2)='PON sinking rate'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='wFe_sink_pday'
      Vinfo( 2)='POFe sinking rate'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='wSi_sink_pday'
      Vinfo( 2)='POSi sinking rate'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='wPIC_sink_pday'
      Vinfo( 2)='PIC sinking rate'
      Vinfo( 3)='m/s s d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='Kdissc_pday'
      Vinfo( 2)='PIC dissociation rate'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#if defined DARWIN_CARBON
      Vinfo( 1)='gud_atmos_pCO2'
      Vinfo( 2)='CO2 fraction of air'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='R_OP'
      Vinfo( 2)='O:P ratio'
      Vinfo( 3)='mmol O (mmol P)-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='m3perkg'
      Vinfo( 2)='m^-3 per kg seawater conversion'
      Vinfo( 3)='m^3/kg'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='surfSiMinInit'
      Vinfo( 2)='surface minimum Si for pH computation'
      Vinfo( 3)='mmol Si m^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='surfSaltMin'
      Vinfo( 2)='surface minimum salinity for pH computation'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='surfSaltMax'
      Vinfo( 2)='surface maximum salinity for pH computation'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='surfTempMin'
      Vinfo( 2)='surface minimum temperature for pH computation'
      Vinfo( 3)='deg C'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='surfTempMax'
      Vinfo( 2)='surface maximum temperature for pH computation'
      Vinfo( 3)='deg C'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='surfDICMin'
      Vinfo( 2)='surface minimum DIC for pH computation'
      Vinfo( 3)='mmol C m^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='surfDICMax'
      Vinfo( 2)='surface maximum DIC for pH computation'
      Vinfo( 3)='mmol C m^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='surfALKMin'
      Vinfo( 2)='surface minimum alkalinity for pH computation'
      Vinfo( 3)='mmol C m^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='surfALKMax'
      Vinfo( 2)='surface maximum alkalinity for pH computation'
      Vinfo( 3)='mmol C m^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='surfPO4Min'
      Vinfo( 2)='surface minimum PO4 for pH computation'
      Vinfo( 3)='mmol P m^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='surfPO4Max'
      Vinfo( 2)='surface maximum PO4 for pH computation'
      Vinfo( 3)='mmol P m^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='surfSiMax'
      Vinfo( 2)='surface maximum Si for pH computation'
      Vinfo( 3)='mmol Si m^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='O2crit'
      Vinfo( 2)='critical oxygen for O2/NO3 remineralization'
      Vinfo( 3)='mmol O'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
#if defined DARWIN_DENIT
      Vinfo( 1)='denit_NP'
      Vinfo( 2)='ratio of n to p in denitrification process'
      Vinfo( 3)='mmol N/(mmol P)'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='denit_NO3'
      Vinfo( 2)='ratio no3 used relative to all n in denitrification process'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='NO3crit'
      Vinfo( 2)='critical nitrate below which no denit (or remin) happens'
      Vinfo( 3)='mmol N m^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
      Vinfo( 1)='PARmin'
      Vinfo( 2)='minimum light for photosynthesis'
      Vinfo( 3)='uEin/m2/s'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#if defined DARWIN_GEIDER && defined DARWIN_CHLQUOTA && defined DARWIN_NQUOTA
      Vinfo( 1)='chl2nmax'
      Vinfo( 2)='maximum chl:N ratio'
      Vinfo( 3)='mg chl/mmol N'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='synthcost'
      Vinfo( 2)='cost of biosynthesis'
      Vinfo( 3)='mmol C/mmol N'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
#if defined DARWIN_ALLOMETRIC_PALAT && ! defined DARWIN_RANDOM_TRAITS
      Vinfo( 1)='palat_min'
      Vinfo( 2)='mimimum palatability threshold'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
      Vinfo( 1)='inhib_graz'
      Vinfo( 2)='for quota-style grazing'
      Vinfo( 3)='(mmol C m-3)-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='inhib_graz_exp'
      Vinfo( 2)='grazing inhib. exponent (0.0 turns it off)'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='hillnum'
      Vinfo( 2)='exponent for limiting quota uptake in grazing'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='hollexp'
      Vinfo( 2)='grazing exponential 1=holling 2, 2=holling 3'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='phygrazmin'
      Vinfo( 2)='minimum total prey conc'
      Vinfo( 3)='mmol C m-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='pmaxPON_pday'
      Vinfo( 2)='max growth rate for bacteria'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='pmaxDON_pday'
      Vinfo( 2)='max growth rate for bacteria'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='pcoefO2_pday'
      Vinfo( 2)='max growth coefficient for bacteria and aerobic growth'
      Vinfo( 3)='s^-1/(mmol oxygen m^-3) s d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='pmaxDIN_pday'
      Vinfo( 2)='max growth rate for bacteria'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#if ! defined DARWIN_RANDOM_TRAITS
      Vinfo( 1)='ksatPOM'
      Vinfo( 2)='ksatPON base value'
      Vinfo( 3)='mmol N m^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='ksatDOM'
      Vinfo( 2)='ksatDON base value'
      Vinfo( 3)='mmol N m^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
      Vinfo( 1)='ksatDIN'
      Vinfo( 2)='DIN half-saturation concentration'
      Vinfo( 3)='mmol N m-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='alpha_hydrol'
      Vinfo( 2)='fraction of POM that is hydrolized into DOM for bacteria'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#if ! defined DARWIN_RANDOM_TRAITS
      Vinfo( 1)='yod'
      Vinfo( 2)='yield value for aerobic bacteria'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='yoe'
      Vinfo( 2)='yield02 value for aerobic bacteria'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='ynd'
      Vinfo( 2)='yield value for denitrifying bacteria'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='yne'
      Vinfo( 2)='yieldNO3 value for denitrifying bacteria'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
#if defined DARWIN_RADTRANS
      Vinfo( 1)='gud_selectSolz'
      Vinfo( 2)='how to compute solar zenith angle, 0: local noon, 1: daytime average, 2: irradiance average'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='gud_refract_water'
      Vinfo( 2)='refractive index of seawater'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='gud_rmud_max'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='gud_radtrans_kmax'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='gud_part_size_P'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='mmol P per particle'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='gud_waveband_edges'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/lamdimplus/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='gud_waveband_centers'
      Vinfo( 2)='representative wavelengths'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/lamdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='gud_radmodThresh'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='gud_rmus'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='gud_rmuu'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='gud_bbmin'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='gud_bbw'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='gud_lambda_aCDOM'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='gud_Sdom'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='gud_aCDOM_fac'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='gud_allomSpectra'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='gud_aCarCell'
      Vinfo( 2)='mg C per cell (from Montagnes et al 1994)'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='gud_bCarCell'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='gud_absorpSlope'
      Vinfo( 2)='slope for scaled absorption spectra'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='gud_bbbSlope'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='gud_scatSwitchSizeLog'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/lamdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='gud_scatSlopeSmall'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/lamdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='gud_scatSlopeLarge'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/lamdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
#if defined DARWIN_CDOM
      Vinfo( 1)='fracCDOM'
      Vinfo( 2)='organic P CDOM fraction'
      Vinfo( 3)='mmol CDOM/mmol P'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='CDOMdegrd_pday'
      Vinfo( 2)='CDOM degradation rate'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='CDOMbleach_pday'
      Vinfo( 2)='CDOM bleaching rate'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='PARCDOM'
      Vinfo( 2)='light normalization coeff for CDOM bleaching'
      Vinfo( 3)='uEin/m2/s'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='R_NP_CDOM'
      Vinfo( 2)='CDOM N:P ratio'
      Vinfo( 3)='mmol N/mmol P'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='R_FeP_CDOM'
      Vinfo( 2)='CDOM Fe:P ratio'
      Vinfo( 3)='mmol Fe/mmol P'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='R_CP_CDOM'
      Vinfo( 2)='CDOM C:P ratio'
      Vinfo( 3)='mmol C/mmol P'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
#if defined DARWIN_CDOM && defined DARWIN_RADTRANS
      Vinfo( 1)='CDOMcoeff'
      Vinfo( 2)='unknown description'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
      Vinfo( 1)='BioMin'
      Vinfo( 2)='MinVal used in the biological code'
      Vinfo( 3)='unknown'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#if defined DARWIN_RANDOM_TRAITS
      Vinfo( 1)='seed_phytoplankton'
      Vinfo( 2)='phytoplankton random number generation seed'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &  1, (/chldim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
#if defined DARWIN_DEBUGVARS
      Vinfo( 1)='darwin_debug_1di'
      Vinfo( 2)='Darwin 1D integer'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &  1, (/0/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

      Vinfo( 1)='darwin_debug_2df'
      Vinfo( 2)='Darwin 2D float'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &  1, (/lamdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
#if ! defined DARWIN_RANDOM_TRAITS && defined DARWIN_PLANK_BUOYCTRL
      Vinfo( 1)='grp_buoyctrl'
      Vinfo( 2)='allow group to adjust buoyancy'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &  1, (/groupdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif
#if defined DARWIN_PLANK_BUOYCTRL
      Vinfo( 1)='buoyctrl'
      Vinfo( 2)='allow plankton to adjust buoyancy'
      Vinfo( 3)='dimensionless'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &  1, (/plankdim/), Aval, Vinfo, ncname, SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, __FILE__)) RETURN

#endif

