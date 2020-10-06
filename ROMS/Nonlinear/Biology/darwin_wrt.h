!
! created by create_darwin_headerfiles (written by J. Paul Mattern)
!

/*
************************************************************************
**                                                                    **
**  Writes Darwin ecosystem model input parameters into output        **
**  netCDF files.                                                     **
**  This file is included in the file "wrt_info.F".                   **
**                                                                    **
************************************************************************
*/

!
!  Write out Darwin ecosystem model parameters.
!

      CALL netcdf_put_ivar (ng, model, ncname, 'isPhoto',               &
     &  isPhoto(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_ivar (ng, model, ncname, 'bactType',              &
     &  bactType(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_ivar (ng, model, ncname, 'isAerobic',             &
     &  isAerobic(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_ivar (ng, model, ncname, 'isDenit',               &
     &  isDenit(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_ivar (ng, model, ncname, 'hasSi',                 &
     &  hasSi(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_ivar (ng, model, ncname, 'hasPIC',                &
     &  hasPIC(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_ivar (ng, model, ncname, 'diazo',                 &
     &  diazo(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_ivar (ng, model, ncname, 'useNH4',                &
     &  useNH4(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_ivar (ng, model, ncname, 'useNO2',                &
     &  useNO2(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_ivar (ng, model, ncname, 'useNO3',                &
     &  useNO3(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_ivar (ng, model, ncname, 'combNO',                &
     &  combNO(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Xmin',                  &
     &  Xmin(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'amminhib',              &
     &  amminhib(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'acclimtimescl',         &
     &  acclimtimescl(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'mort',                  &
     &  mort(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'mort2',                 &
     &  mort2(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_ivar (ng, model, ncname, 'tempMort',              &
     &  tempMort(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_ivar (ng, model, ncname, 'tempMort2',             &
     &  tempMort2(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'ExportFracMort',        &
     &  ExportFracMort(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'ExportFracMort2',       &
     &  ExportFracMort2(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'ExportFrac',            &
     &  ExportFrac(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'phytoTempCoeff',        &
     &  phytoTempCoeff(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'phytoTempExp1',         &
     &  phytoTempExp1(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'phytoTempExp2',         &
     &  phytoTempExp2(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'phytoTempOptimum',      &
     &  phytoTempOptimum(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'phytoDecayPower',       &
     &  phytoDecayPower(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'R_NC',                  &
     &  R_NC(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'R_PC',                  &
     &  R_PC(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'R_SiC',                 &
     &  R_SiC(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'R_FeC',                 &
     &  R_FeC(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'R_ChlC',                &
     &  R_ChlC(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'R_PICPOC',              &
     &  R_PICPOC(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'wsink',                 &
     &  wsink(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'wswim',                 &
     &  wswim(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'respiration',           &
     &  respiration(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'PCmax',                 &
     &  PCmax(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Qnmax',                 &
     &  Qnmax(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Qnmin',                 &
     &  Qnmin(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Qpmax',                 &
     &  Qpmax(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Qpmin',                 &
     &  Qpmin(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Qsimax',                &
     &  Qsimax(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Qsimin',                &
     &  Qsimin(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Qfemax',                &
     &  Qfemax(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Qfemin',                &
     &  Qfemin(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Vmax_NH4',              &
     &  Vmax_NH4(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Vmax_NO2',              &
     &  Vmax_NO2(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Vmax_NO3',              &
     &  Vmax_NO3(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Vmax_N',                &
     &  Vmax_N(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Vmax_PO4',              &
     &  Vmax_PO4(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Vmax_SiO2',             &
     &  Vmax_SiO2(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Vmax_FeT',              &
     &  Vmax_FeT(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'ksatNH4',               &
     &  ksatNH4(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'ksatNO2',               &
     &  ksatNO2(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'ksatNO3',               &
     &  ksatNO3(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'ksatPO4',               &
     &  ksatPO4(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'ksatSiO2',              &
     &  ksatSiO2(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'ksatFeT',               &
     &  ksatFeT(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'kexcC',                 &
     &  kexcC(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'kexcN',                 &
     &  kexcN(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'kexcP',                 &
     &  kexcP(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'kexcSi',                &
     &  kexcSi(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'kexcFe',                &
     &  kexcFe(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#if defined DARWIN_GEIDER
      CALL netcdf_put_fvar (ng, model, ncname, 'inhibcoef_geid',        &
     &  inhibcoef_geid(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
#if ! defined DARWIN_GEIDER
      CALL netcdf_put_fvar (ng, model, ncname, 'ksatPAR',               &
     &  ksatPAR(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'kinhPAR',               &
     &  kinhPAR(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
      CALL netcdf_put_fvar (ng, model, ncname, 'mQyield',               &
     &  mQyield(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'chl2cmax',              &
     &  chl2cmax(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'grazemax',              &
     &  grazemax(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'kgrazesat',             &
     &  kgrazesat(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'palat',                 &
     &  palat(:,:,ng), (/1,1/), (/nplank,nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'asseff',                &
     &  asseff(:,:,ng), (/1,1/), (/nplank,nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'ExportFracPreyPred',    &
     &  ExportFracPreyPred(:,:,ng), (/1,1/), (/nplank,nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'yield',                 &
     &  yield(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'yieldO2',               &
     &  yieldO2(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'yieldNO3',              &
     &  yieldNO3(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'ksatPON',               &
     &  ksatPON(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'ksatPOC',               &
     &  ksatPOC(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'ksatPOP',               &
     &  ksatPOP(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'ksatPOFe',              &
     &  ksatPOFe(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'ksatDON',               &
     &  ksatDON(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'ksatDOC',               &
     &  ksatDOC(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'ksatDOP',               &
     &  ksatDOP(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'ksatDOFe',              &
     &  ksatDOFe(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#if defined DARWIN_RADTRANS
      CALL netcdf_put_fvar (ng, model, ncname, 'aphy_chl',              &
     &  aphy_chl(:,:,ng), (/1,1/), (/nplank,nlam/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'aphy_chl_ps',           &
     &  aphy_chl_ps(:,:,ng), (/1,1/), (/nplank,nlam/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'bphy_mgC',              &
     &  bphy_mgC(:,:,ng), (/1,1/), (/nplank,nlam/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'bbphy_mgC',             &
     &  bbphy_mgC(:,:,ng), (/1,1/), (/nplank,nlam/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
#if ! defined DARWIN_GEIDER
      CALL netcdf_put_fvar (ng, model, ncname, 'normI',                 &
     &  normI(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
#if defined DARWIN_RADTRANS
      CALL netcdf_put_ivar (ng, model, ncname, 'ap_type',               &
     &  ap_type(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
#if ! defined DARWIN_RANDOM_TRAITS
      CALL netcdf_put_fvar (ng, model, ncname, 'biovol',                &
     &  biovol(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
      CALL netcdf_put_ivar (ng, model, ncname, 'group',                 &
     &  group(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#if ! defined DARWIN_RANDOM_TRAITS
      CALL netcdf_put_fvar (ng, model, ncname, 'biovol_bygroup',        &
     &  biovol_bygroup(:,:,ng), (/1,1/), (/nplank,ngroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
      CALL netcdf_put_fvar (ng, model, ncname, 'alphachl',              &
     &  alphachl(:,:,ng), (/1,1/), (/nplank,nlam/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'alpha_mean',            &
     &  alpha_mean(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'chl2cmin',              &
     &  chl2cmin(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'mortTempFuncMin',       &
     &  mortTempFuncMin(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'mort2TempFuncMin',      &
     &  mort2TempFuncMin(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#if defined DARWIN_RANDOM_TRAITS
      CALL netcdf_put_fvar (ng, model, ncname, 'Smallgrow',             &
     &  Smallgrow(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Biggrow',               &
     &  Biggrow(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Smallgrowrange',        &
     &  Smallgrowrange(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Biggrowrange',          &
     &  Biggrowrange(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'diaz_growfac',          &
     &  diaz_growfac(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'cocco_growfac',         &
     &  cocco_growfac(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'diatom_growfac',        &
     &  diatom_growfac(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Smallmort',             &
     &  Smallmort(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Bigmort',               &
     &  Bigmort(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Smallmortrange',        &
     &  Smallmortrange(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Bigmortrange',          &
     &  Bigmortrange(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Smallexport',           &
     &  Smallexport(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Bigexport',             &
     &  Bigexport(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'tempcoeff1',            &
     &  tempcoeff1(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'tempcoeff2_small',      &
     &  tempcoeff2_small(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'tempcoeff2_big',        &
     &  tempcoeff2_big(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'tempcoeff3',            &
     &  tempcoeff3(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
#if defined DARWIN_RANDOM_TRAITS && defined DARWIN_TEMP_RANGE
      CALL netcdf_put_fvar (ng, model, ncname, 'tempmax',               &
     &  tempmax(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'temprange',             &
     &  temprange(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'tempdecay',             &
     &  tempdecay(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
#if defined DARWIN_RANDOM_TRAITS
      CALL netcdf_put_fvar (ng, model, ncname, 'val_R_NC',              &
     &  val_R_NC(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'val_R_NC_diaz',         &
     &  val_R_NC_diaz(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'val_R_PC',              &
     &  val_R_PC(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'val_R_SiC_diatom',      &
     &  val_R_SiC_diatom(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'val_R_FeC',             &
     &  val_R_FeC(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'val_R_FeC_diaz',        &
     &  val_R_FeC_diaz(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'val_R_PICPOC',          &
     &  val_R_PICPOC(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
#if defined DARWIN_RANDOM_TRAITS && ! defined DARWIN_GEIDER
      CALL netcdf_put_fvar (ng, model, ncname, 'val_R_ChlC',            &
     &  val_R_ChlC(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
#if defined DARWIN_RANDOM_TRAITS
      CALL netcdf_put_fvar (ng, model, ncname, 'val_R_NC_zoo',          &
     &  val_R_NC_zoo(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'val_R_PC_zoo',          &
     &  val_R_PC_zoo(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'val_R_SiC_zoo',         &
     &  val_R_SiC_zoo(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'val_R_FeC_zoo',         &
     &  val_R_FeC_zoo(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'val_R_PICPOC_zoo',      &
     &  val_R_PICPOC_zoo(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'val_R_ChlC_zoo',        &
     &  val_R_ChlC_zoo(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'SmallSink_pday',        &
     &  SmallSink_pday(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'BigSink_pday',          &
     &  BigSink_pday(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'SmallPsat',             &
     &  SmallPsat(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'BigPsat',               &
     &  BigPsat(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'ProcPsat',              &
     &  ProcPsat(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'UniDzPsat',             &
     &  UniDzPsat(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'CoccoPsat',             &
     &  CoccoPsat(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'SmallPsatrange',        &
     &  SmallPsatrange(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'BigPsatrange',          &
     &  BigPsatrange(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'ProcPsatrange',         &
     &  ProcPsatrange(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'UniDzPsatrange',        &
     &  UniDzPsatrange(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'CoccoPsatrange',        &
     &  CoccoPsatrange(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'ksatNH4fac',            &
     &  ksatNH4fac(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'ksatNO2fac',            &
     &  ksatNO2fac(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'val_amminhib',          &
     &  val_amminhib(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'val_ksatsio2',          &
     &  val_ksatsio2(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
#if defined DARWIN_RANDOM_TRAITS && ! defined DARWIN_GEIDER
      CALL netcdf_put_fvar (ng, model, ncname, 'smallksatpar',          &
     &  smallksatpar(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'smallksatparstd',       &
     &  smallksatparstd(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'smallkinhpar',          &
     &  smallkinhpar(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'smallkinhparstd',       &
     &  smallkinhparstd(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Bigksatpar',            &
     &  Bigksatpar(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Bigksatparstd',         &
     &  Bigksatparstd(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Bigkinhpar',            &
     &  Bigkinhpar(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Bigkinhparstd',         &
     &  Bigkinhparstd(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
#if defined DARWIN_RANDOM_TRAITS && defined DARWIN_NINE_SPECIES_SETUP
      CALL netcdf_put_fvar (ng, model, ncname, 'LLProkinhpar',          &
     &  LLProkinhpar(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Coccokinhpar',          &
     &  Coccokinhpar(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
#if defined DARWIN_RANDOM_TRAITS
      CALL netcdf_put_fvar (ng, model, ncname, 'inhibcoef_geid_val',    &
     &  inhibcoef_geid_val(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'smallmQyield',          &
     &  smallmQyield(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'smallmQyieldrange',     &
     &  smallmQyieldrange(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'BigmQyield',            &
     &  BigmQyield(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'BigmQyieldrange',       &
     &  BigmQyieldrange(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'smallchl2cmax',         &
     &  smallchl2cmax(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'smallchl2cmaxrange',    &
     &  smallchl2cmaxrange(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Bigchl2cmax',           &
     &  Bigchl2cmax(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Bigchl2cmaxrange',      &
     &  Bigchl2cmaxrange(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
#if ! defined DARWIN_RADTRANS
      CALL netcdf_put_fvar (ng, model, ncname, 'aphy_chl_ave',          &
     &  aphy_chl_ave(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
#if defined DARWIN_RANDOM_TRAITS
      CALL netcdf_put_fvar (ng, model, ncname, 'val_acclimtimescl',     &
     &  val_acclimtimescl(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_lvar (ng, model, ncname, 'oldTwoGrazers',         &
     &  oldTwoGrazers(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'GrazeFast_pday',        &
     &  GrazeFast_pday(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'ZooexfacSmall',         &
     &  ZooexfacSmall(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'ZooexfacBig',           &
     &  ZooexfacBig(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'ZoomortSmall_pday',     &
     &  ZoomortSmall_pday(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'ZoomortBig_pday',       &
     &  ZoomortBig_pday(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'ZoomortSmall2',         &
     &  ZoomortSmall2(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'ZoomortBig2',           &
     &  ZoomortBig2(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'ExGrazfracbig',         &
     &  ExGrazfracbig(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'ExGrazfracsmall',       &
     &  ExGrazfracsmall(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'palathi',               &
     &  palathi(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'palatlo',               &
     &  palatlo(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'diatomgraz',            &
     &  diatomgraz(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'coccograz',             &
     &  coccograz(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'olargegraz',            &
     &  olargegraz(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'GrazeEfflow',           &
     &  GrazeEfflow(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'GrazeEffmod',           &
     &  GrazeEffmod(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'GrazeEffhi',            &
     &  GrazeEffhi(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'GrazeRate_pday',        &
     &  GrazeRate_pday(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'ExGrazfrac',            &
     &  ExGrazfrac(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'val_palat',             &
     &  val_palat(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'val_ass_eff',           &
     &  val_ass_eff(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'kgrazesat_val',         &
     &  kgrazesat_val(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Zoomort_pday',          &
     &  Zoomort_pday(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Zoomort2',              &
     &  Zoomort2(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Zooexfac',              &
     &  Zooexfac(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'ZooDM',                 &
     &  ZooDM(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
#if ! defined DARWIN_RANDOM_TRAITS
      CALL netcdf_put_lvar (ng, model, ncname, 'gud_sort_biovol',       &
     &  gud_sort_biovol(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
      CALL netcdf_put_lvar (ng, model, ncname, 'GUD_effective_ksat',    &
     &  GUD_effective_ksat(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_ivar (ng, model, ncname, 'gud_select_kn_allom',   &
     &  gud_select_kn_allom(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'logvolbase',            &
     &  logvolbase(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#if ! defined DARWIN_RANDOM_TRAITS
      CALL netcdf_put_fvar (ng, model, ncname, 'logvolinc',             &
     &  logvolinc(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
      CALL netcdf_put_fvar (ng, model, ncname, 'biovol0',               &
     &  biovol0(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'biovolfac',             &
     &  biovolfac(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#if ! defined DARWIN_RANDOM_TRAITS
      CALL netcdf_put_ivar (ng, model, ncname, 'logvol0ind',            &
     &  logvol0ind(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'grp_logvolind',         &
     &  grp_logvolind(:,:,ng), (/1,1/), (/nPlank,nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'grp_biovol',            &
     &  grp_biovol(:,:,ng), (/1,1/), (/nPlank,nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_ivar (ng, model, ncname, 'grp_nplank',            &
     &  grp_nplank(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_ivar (ng, model, ncname, 'grp_photo',             &
     &  grp_photo(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_ivar (ng, model, ncname, 'grp_bacttype',          &
     &  grp_bacttype(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_ivar (ng, model, ncname, 'grp_aerobic',           &
     &  grp_aerobic(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_ivar (ng, model, ncname, 'grp_denit',             &
     &  grp_denit(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_ivar (ng, model, ncname, 'grp_pred',              &
     &  grp_pred(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_ivar (ng, model, ncname, 'grp_prey',              &
     &  grp_prey(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_ivar (ng, model, ncname, 'grp_hasSi',             &
     &  grp_hasSi(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_ivar (ng, model, ncname, 'grp_hasPIC',            &
     &  grp_hasPIC(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_ivar (ng, model, ncname, 'grp_diazo',             &
     &  grp_diazo(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_ivar (ng, model, ncname, 'grp_useNH4',            &
     &  grp_useNH4(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_ivar (ng, model, ncname, 'grp_useNO2',            &
     &  grp_useNO2(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_ivar (ng, model, ncname, 'grp_useNO3',            &
     &  grp_useNO3(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_ivar (ng, model, ncname, 'grp_combNO',            &
     &  grp_combNO(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
#if defined DARWIN_RADTRANS && ! defined DARWIN_RANDOM_TRAITS
      CALL netcdf_put_ivar (ng, model, ncname, 'grp_aptype',            &
     &  grp_aptype(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
#if ! defined DARWIN_RANDOM_TRAITS
      CALL netcdf_put_ivar (ng, model, ncname, 'grp_tempMort',          &
     &  grp_tempMort(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_ivar (ng, model, ncname, 'grp_tempMort2',         &
     &  grp_tempMort2(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'grp_Xmin',              &
     &  grp_Xmin(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'grp_R_NC',              &
     &  grp_R_NC(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'grp_R_PC',              &
     &  grp_R_PC(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'grp_R_SiC',             &
     &  grp_R_SiC(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'grp_R_FeC',             &
     &  grp_R_FeC(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'grp_R_ChlC',            &
     &  grp_R_ChlC(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'grp_R_PICPOC',          &
     &  grp_R_PICPOC(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'grp_ExportFracMort',    &
     &  grp_ExportFracMort(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'grp_ExportFracMort2',   &
     &  grp_ExportFracMort2(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'grp_ExportFrac',        &
     &  grp_ExportFrac(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'grp_mort_pday',         &
     &  grp_mort_pday(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'grp_mort2',             &
     &  grp_mort2(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'grp_tempcoeff1',        &
     &  grp_tempcoeff1(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'grp_tempcoeff2',        &
     &  grp_tempcoeff2(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'grp_tempcoeff3',        &
     &  grp_tempcoeff3(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'grp_tempopt',           &
     &  grp_tempopt(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'grp_tempdecay',         &
     &  grp_tempdecay(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
#if defined DARWIN_ALLOMETRIC_PALAT && ! defined DARWIN_RANDOM_TRAITS
      CALL netcdf_put_fvar (ng, model, ncname, 'grp_pp_sig',            &
     &  grp_pp_sig(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
#if ! defined DARWIN_RANDOM_TRAITS && defined DARWIN_GEIDER
      CALL netcdf_put_fvar (ng, model, ncname, 'grp_mQyield',           &
     &  grp_mQyield(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'grp_chl2cmax',          &
     &  grp_chl2cmax(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'grp_inhibcoef_geid',    &
     &  grp_inhibcoef_geid(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
#if ! defined DARWIN_RANDOM_TRAITS && ! defined DARWIN_GEIDER
      CALL netcdf_put_fvar (ng, model, ncname, 'grp_ksatPAR',           &
     &  grp_ksatPAR(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'grp_kinhPAR',           &
     &  grp_kinhPAR(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
#if ! defined DARWIN_RANDOM_TRAITS
      CALL netcdf_put_fvar (ng, model, ncname, 'grp_ksatNH4fac',        &
     &  grp_ksatNH4fac(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'grp_ksatNO2fac',        &
     &  grp_ksatNO2fac(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'grp_amminhib',          &
     &  grp_amminhib(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'grp_acclimtimescl_pday',&
     &  grp_acclimtimescl_pday(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'a_graz_pday',           &
     &  a_graz_pday(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'b_graz',                &
     &  b_graz(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'a_kg',                  &
     &  a_kg(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'b_kg',                  &
     &  b_kg(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'a_biosink_pday',        &
     &  a_biosink_pday(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'b_biosink',             &
     &  b_biosink(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'a_bioswim_pday',        &
     &  a_bioswim_pday(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'b_bioswim',             &
     &  b_bioswim(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
#if defined DARWIN_ALLOMETRIC_PALAT && ! defined DARWIN_RANDOM_TRAITS
      CALL netcdf_put_fvar (ng, model, ncname, 'a_prdpry',              &
     &  a_prdpry(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'b_prdpry',              &
     &  b_prdpry(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
#if ! defined DARWIN_RANDOM_TRAITS
      CALL netcdf_put_fvar (ng, model, ncname, 'a_vmax_DIC_pday',       &
     &  a_vmax_DIC_pday(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'b_vmax_DIC',            &
     &  b_vmax_DIC(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'a_qcarbon',             &
     &  a_qcarbon(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'b_qcarbon',             &
     &  b_qcarbon(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'a_respir',              &
     &  a_respir(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'b_respir',              &
     &  b_respir(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'a_kexc_c',              &
     &  a_kexc_c(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'b_kexc_c',              &
     &  b_kexc_c(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'a_vmax_NO3_pday',       &
     &  a_vmax_NO3_pday(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'b_vmax_NO3',            &
     &  b_vmax_NO3(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'a_kn_NO3',              &
     &  a_kn_NO3(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'b_kn_NO3',              &
     &  b_kn_NO3(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'a_qmin_n',              &
     &  a_qmin_n(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'b_qmin_n',              &
     &  b_qmin_n(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'a_qmax_n',              &
     &  a_qmax_n(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'b_qmax_n',              &
     &  b_qmax_n(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'a_kexc_n',              &
     &  a_kexc_n(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'b_kexc_n',              &
     &  b_kexc_n(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'a_vmax_NO2_pday',       &
     &  a_vmax_NO2_pday(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'b_vmax_NO2',            &
     &  b_vmax_NO2(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'a_kn_NO2',              &
     &  a_kn_NO2(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'b_kn_NO2',              &
     &  b_kn_NO2(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'a_vmax_NH4_pday',       &
     &  a_vmax_NH4_pday(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'b_vmax_NH4',            &
     &  b_vmax_NH4(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'a_kn_NH4',              &
     &  a_kn_NH4(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'b_kn_NH4',              &
     &  b_kn_NH4(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'a_vmax_N_pday',         &
     &  a_vmax_N_pday(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'b_vmax_N',              &
     &  b_vmax_N(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'a_vmax_PO4_pday',       &
     &  a_vmax_PO4_pday(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'b_vmax_PO4',            &
     &  b_vmax_PO4(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'a_kn_PO4',              &
     &  a_kn_PO4(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'b_kn_PO4',              &
     &  b_kn_PO4(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'a_qmin_p',              &
     &  a_qmin_p(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'b_qmin_p',              &
     &  b_qmin_p(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'a_qmax_p',              &
     &  a_qmax_p(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'b_qmax_p',              &
     &  b_qmax_p(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'a_kexc_p_pday',         &
     &  a_kexc_p_pday(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'b_kexc_p',              &
     &  b_kexc_p(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'a_vmax_SiO2_pday',      &
     &  a_vmax_SiO2_pday(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'b_vmax_SiO2',           &
     &  b_vmax_SiO2(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'a_kn_SiO2',             &
     &  a_kn_SiO2(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'b_kn_SiO2',             &
     &  b_kn_SiO2(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'a_qmin_si',             &
     &  a_qmin_si(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'b_qmin_si',             &
     &  b_qmin_si(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'a_qmax_si',             &
     &  a_qmax_si(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'b_qmax_si',             &
     &  b_qmax_si(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'a_kexc_si_pday',        &
     &  a_kexc_si_pday(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'b_kexc_si',             &
     &  b_kexc_si(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'a_vmax_FeT_pday',       &
     &  a_vmax_FeT_pday(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'b_vmax_FeT',            &
     &  b_vmax_FeT(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'a_kn_feT',              &
     &  a_kn_feT(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'b_kn_FeT',              &
     &  b_kn_FeT(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'a_qmin_fe',             &
     &  a_qmin_fe(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'b_qmin_fe',             &
     &  b_qmin_fe(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'a_qmax_fe',             &
     &  a_qmax_fe(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'b_qmax_fe',             &
     &  b_qmax_fe(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'a_kexc_fe_pday',        &
     &  a_kexc_fe_pday(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'b_kexc_fe',             &
     &  b_kexc_fe(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'grp_ExportFracPreyPred',&
     &  grp_ExportFracPreyPred(:,:,ng), (/1,1/), (/nGroup,nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'grp_ass_eff',           &
     &  grp_ass_eff(:,:,ng), (/1,1/), (/nGroup,nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
#if defined DARWIN_USE_PLOAD
      CALL netcdf_put_fvar (ng, model, ncname, 'Pa2Atm',                &
     &  Pa2Atm(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
      CALL netcdf_put_fvar (ng, model, ncname, 'ptr2mol',               &
     &  ptr2mol(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'sca1',                  &
     &  sca1(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'sca2',                  &
     &  sca2(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'sca3',                  &
     &  sca3(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'sca4',                  &
     &  sca4(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'sox1',                  &
     &  sox1(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'sox2',                  &
     &  sox2(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'sox3',                  &
     &  sox3(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'sox4',                  &
     &  sox4(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'oA0',                   &
     &  oA0(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'oA1',                   &
     &  oA1(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'oA2',                   &
     &  oA2(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'oA3',                   &
     &  oA3(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'oA4',                   &
     &  oA4(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'oA5',                   &
     &  oA5(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'oB0',                   &
     &  oB0(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'oB1',                   &
     &  oB1(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'oB2',                   &
     &  oB2(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'oB3',                   &
     &  oB3(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'oC0',                   &
     &  oC0(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#if defined DARWIN_RANDOM_TRAITS
      CALL netcdf_put_fvar (ng, model, ncname, 'phymin',                &
     &  phymin(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
      CALL netcdf_put_fvar (ng, model, ncname, 'katten_w',              &
     &  katten_w(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'katten_chl',            &
     &  katten_chl(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'parfrac',               &
     &  parfrac(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#if DARWIN_TEMP_VERSION == 1
      CALL netcdf_put_fvar (ng, model, ncname, 'tempnorm',              &
     &  tempnorm(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
#if DARWIN_TEMP_VERSION == 2
      CALL netcdf_put_fvar (ng, model, ncname, 'TempAeArr',             &
     &  TempAeArr(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'TemprefArr',            &
     &  TemprefArr(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'TempCoeffArr',          &
     &  TempCoeffArr(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
      CALL netcdf_put_fvar (ng, model, ncname, 'alpfe',                 &
     &  alpfe(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#if ! defined DARWIN_PART_SCAV_POP && ! defined DARWIN_PART_SCAV
      CALL netcdf_put_fvar (ng, model, ncname, 'scav_pyear',            &
     &  scav_pyear(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
      CALL netcdf_put_fvar (ng, model, ncname, 'ligand_tot',            &
     &  ligand_tot(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'ligand_stab',           &
     &  ligand_stab(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#if defined DARWIN_MINFE
      CALL netcdf_put_fvar (ng, model, ncname, 'freefemax',             &
     &  freefemax(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
#if defined DARWIN_PART_SCAV_POP || defined DARWIN_PART_SCAV
      CALL netcdf_put_fvar (ng, model, ncname, 'scav_rat_pday',         &
     &  scav_rat_pday(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'scav_inter',            &
     &  scav_inter(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'scav_exp',              &
     &  scav_exp(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
#if defined DARWIN_PART_SCAV_POP
      CALL netcdf_put_fvar (ng, model, ncname, 'scav_R_POPPOC',         &
     &  scav_R_POPPOC(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
#if ! defined DARWIN_IRON_SED_SOURCE_VARIABLE
      CALL netcdf_put_fvar (ng, model, ncname, 'fesedflux_pday',        &
     &  fesedflux_pday(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
#if defined DARWIN_IRON_SED_SOURCE_VARIABLE
      CALL netcdf_put_fvar (ng, model, ncname, 'fesedflux_pcm',         &
     &  fesedflux_pcm(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'R_CP_fesed',            &
     &  R_CP_fesed(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
      CALL netcdf_put_fvar (ng, model, ncname, 'Knita_pday',            &
     &  Knita_pday(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Knitb_pday',            &
     &  Knitb_pday(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'PAR_oxi',               &
     &  PAR_oxi(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Kdoc_pday',             &
     &  Kdoc_pday(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Kdop_pday',             &
     &  Kdop_pday(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Kdon_pday',             &
     &  Kdon_pday(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'KdoFe_pday',            &
     &  KdoFe_pday(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'KPOC_pday',             &
     &  KPOC_pday(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'KPOP_pday',             &
     &  KPOP_pday(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'KPON_pday',             &
     &  KPON_pday(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'KPOFe_pday',            &
     &  KPOFe_pday(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'KPOSi_pday',            &
     &  KPOSi_pday(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'wC_sink_pday',          &
     &  wC_sink_pday(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'wP_sink_pday',          &
     &  wP_sink_pday(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'wN_sink_pday',          &
     &  wN_sink_pday(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'wFe_sink_pday',         &
     &  wFe_sink_pday(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'wSi_sink_pday',         &
     &  wSi_sink_pday(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'wPIC_sink_pday',        &
     &  wPIC_sink_pday(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Kdissc_pday',           &
     &  Kdissc_pday(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#if defined DARWIN_CARBON
      CALL netcdf_put_fvar (ng, model, ncname, 'gud_atmos_pCO2',        &
     &  gud_atmos_pCO2(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'R_OP',                  &
     &  R_OP(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'm3perkg',               &
     &  m3perkg(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'surfSiMinInit',         &
     &  surfSiMinInit(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'surfSaltMin',           &
     &  surfSaltMin(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'surfSaltMax',           &
     &  surfSaltMax(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'surfTempMin',           &
     &  surfTempMin(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'surfTempMax',           &
     &  surfTempMax(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'surfDICMin',            &
     &  surfDICMin(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'surfDICMax',            &
     &  surfDICMax(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'surfALKMin',            &
     &  surfALKMin(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'surfALKMax',            &
     &  surfALKMax(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'surfPO4Min',            &
     &  surfPO4Min(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'surfPO4Max',            &
     &  surfPO4Max(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'surfSiMax',             &
     &  surfSiMax(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'O2crit',                &
     &  O2crit(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
#if defined DARWIN_DENIT
      CALL netcdf_put_fvar (ng, model, ncname, 'denit_NP',              &
     &  denit_NP(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'denit_NO3',             &
     &  denit_NO3(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'NO3crit',               &
     &  NO3crit(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
      CALL netcdf_put_fvar (ng, model, ncname, 'PARmin',                &
     &  PARmin(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#if defined DARWIN_GEIDER && defined DARWIN_CHLQUOTA && defined DARWIN_NQUOTA
      CALL netcdf_put_fvar (ng, model, ncname, 'chl2nmax',              &
     &  chl2nmax(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'synthcost',             &
     &  synthcost(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
#if defined DARWIN_ALLOMETRIC_PALAT && ! defined DARWIN_RANDOM_TRAITS
      CALL netcdf_put_fvar (ng, model, ncname, 'palat_min',             &
     &  palat_min(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
      CALL netcdf_put_fvar (ng, model, ncname, 'inhib_graz',            &
     &  inhib_graz(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'inhib_graz_exp',        &
     &  inhib_graz_exp(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'hillnum',               &
     &  hillnum(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'hollexp',               &
     &  hollexp(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'phygrazmin',            &
     &  phygrazmin(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'pmaxPON_pday',          &
     &  pmaxPON_pday(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'pmaxDON_pday',          &
     &  pmaxDON_pday(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'pcoefO2_pday',          &
     &  pcoefO2_pday(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'pmaxDIN_pday',          &
     &  pmaxDIN_pday(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#if ! defined DARWIN_RANDOM_TRAITS
      CALL netcdf_put_fvar (ng, model, ncname, 'ksatPOM',               &
     &  ksatPOM(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'ksatDOM',               &
     &  ksatDOM(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
      CALL netcdf_put_fvar (ng, model, ncname, 'ksatDIN',               &
     &  ksatDIN(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'alpha_hydrol',          &
     &  alpha_hydrol(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#if ! defined DARWIN_RANDOM_TRAITS
      CALL netcdf_put_fvar (ng, model, ncname, 'yod',                   &
     &  yod(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'yoe',                   &
     &  yoe(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'ynd',                   &
     &  ynd(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'yne',                   &
     &  yne(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
#if defined DARWIN_RADTRANS
      CALL netcdf_put_ivar (ng, model, ncname, 'gud_selectSolz',        &
     &  gud_selectSolz(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'gud_refract_water',     &
     &  gud_refract_water(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'gud_rmud_max',          &
     &  gud_rmud_max(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_ivar (ng, model, ncname, 'gud_radtrans_kmax',     &
     &  gud_radtrans_kmax(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'gud_part_size_P',       &
     &  gud_part_size_P(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'gud_waveband_edges',    &
     &  gud_waveband_edges(:,ng), (/1/), (/nlam+1/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'gud_waveband_centers',  &
     &  gud_waveband_centers(:,ng), (/1/), (/nlam/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'gud_radmodThresh',      &
     &  gud_radmodThresh(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'gud_rmus',              &
     &  gud_rmus(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'gud_rmuu',              &
     &  gud_rmuu(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'gud_bbmin',             &
     &  gud_bbmin(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'gud_bbw',               &
     &  gud_bbw(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'gud_lambda_aCDOM',      &
     &  gud_lambda_aCDOM(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'gud_Sdom',              &
     &  gud_Sdom(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'gud_aCDOM_fac',         &
     &  gud_aCDOM_fac(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_lvar (ng, model, ncname, 'gud_allomSpectra',      &
     &  gud_allomSpectra(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'gud_aCarCell',          &
     &  gud_aCarCell(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'gud_bCarCell',          &
     &  gud_bCarCell(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'gud_absorpSlope',       &
     &  gud_absorpSlope(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'gud_bbbSlope',          &
     &  gud_bbbSlope(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'gud_scatSwitchSizeLog', &
     &  gud_scatSwitchSizeLog(:,ng), (/1/), (/nlam/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'gud_scatSlopeSmall',    &
     &  gud_scatSlopeSmall(:,ng), (/1/), (/nlam/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'gud_scatSlopeLarge',    &
     &  gud_scatSlopeLarge(:,ng), (/1/), (/nlam/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
#if defined DARWIN_CDOM
      CALL netcdf_put_fvar (ng, model, ncname, 'fracCDOM',              &
     &  fracCDOM(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'CDOMdegrd_pday',        &
     &  CDOMdegrd_pday(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'CDOMbleach_pday',       &
     &  CDOMbleach_pday(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'PARCDOM',               &
     &  PARCDOM(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'R_NP_CDOM',             &
     &  R_NP_CDOM(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'R_FeP_CDOM',            &
     &  R_FeP_CDOM(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'R_CP_CDOM',             &
     &  R_CP_CDOM(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
#if defined DARWIN_CDOM && defined DARWIN_RADTRANS
      CALL netcdf_put_fvar (ng, model, ncname, 'CDOMcoeff',             &
     &  CDOMcoeff(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
      CALL netcdf_put_fvar (ng, model, ncname, 'BioMin',                &
     &  BioMin(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#if defined DARWIN_RANDOM_TRAITS
      CALL netcdf_put_ivar (ng, model, ncname, 'seed_phytoplankton',    &
     &  seed_phytoplankton(:,ng), (/1/), (/nChl/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
#if defined DARWIN_DEBUGVARS
      CALL netcdf_put_ivar (ng, model, ncname, 'darwin_debug_1di',      &
     &  darwin_debug_1di(ng), (/0/), (/0/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'darwin_debug_2df',      &
     &  darwin_debug_2df(:,ng), (/1/), (/nlam/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
#if ! defined DARWIN_RANDOM_TRAITS && defined DARWIN_PLANK_BUOYCTRL
      CALL netcdf_put_lvar (ng, model, ncname, 'grp_buoyctrl',          &
     &  grp_buoyctrl(:,ng), (/1/), (/nGroup/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif
#if defined DARWIN_PLANK_BUOYCTRL
      CALL netcdf_put_lvar (ng, model, ncname, 'buoyctrl',              &
     &  buoyctrl(:,ng), (/1/), (/nplank/), ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &  __FILE__)) RETURN

#endif

