!
! created by create_darwin_headerfiles (written by J. Paul Mattern)
!


!
!  Read in gud_selectSolz.
!
        CALL netcdf_get_ivar(ng, model, DRT(ng)%name,                   &
     &    'selectSolz',                                                 &
     &    gud_selectSolz(ng),                                           &
     &    ncid = DRT(ng)%ncid)
!
!  Read in gud_refract_water.
!
        CALL netcdf_get_fvar(ng, model, DRT(ng)%name,                   &
     &    'refract_water',                                              &
     &    gud_refract_water(ng),                                        &
     &    ncid = DRT(ng)%ncid)
!
!  Read in gud_rmud_max.
!
        CALL netcdf_get_fvar(ng, model, DRT(ng)%name,                   &
     &    'rmud_max',                                                   &
     &    gud_rmud_max(ng),                                             &
     &    ncid = DRT(ng)%ncid)
!
!  Read in gud_radtrans_kmax.
!
        CALL netcdf_get_ivar(ng, model, DRT(ng)%name,                   &
     &    'radtrans_kmax',                                              &
     &    gud_radtrans_kmax(ng),                                        &
     &    ncid = DRT(ng)%ncid)
!
!  Read in gud_part_size_P.
!
        CALL netcdf_get_fvar(ng, model, DRT(ng)%name,                   &
     &    'part_size_P',                                                &
     &    gud_part_size_P(ng),                                          &
     &    ncid = DRT(ng)%ncid)
!
!  Read in gud_waveband_edges.
!
        CALL netcdf_get_fvar(ng, model, DRT(ng)%name,                   &
     &    'waveband_edges',                                             &
     &    ncid = DRT(ng)%ncid,                                          &
     &    gud_waveband_edges(:,ng),                                     &
     &    start = (/1/),                                                &
     &    total = (/nlam+1/))
!
!  Read in gud_waveband_centers.
!
        CALL netcdf_get_fvar(ng, model, DRT(ng)%name,                   &
     &    'waveband_centers',                                           &
     &    ncid = DRT(ng)%ncid,                                          &
     &    gud_waveband_centers(:,ng),                                   &
     &    start = (/1/),                                                &
     &    total = (/nlam/))
!
!  Read in gud_radmodThresh.
!
        CALL netcdf_get_fvar(ng, model, DRT(ng)%name,                   &
     &    'radmodThresh',                                               &
     &    gud_radmodThresh(ng),                                         &
     &    ncid = DRT(ng)%ncid)
!
!  Read in gud_rmus.
!
        CALL netcdf_get_fvar(ng, model, DRT(ng)%name,                   &
     &    'rmus',                                                       &
     &    gud_rmus(ng),                                                 &
     &    ncid = DRT(ng)%ncid)
!
!  Read in gud_rmuu.
!
        CALL netcdf_get_fvar(ng, model, DRT(ng)%name,                   &
     &    'rmuu',                                                       &
     &    gud_rmuu(ng),                                                 &
     &    ncid = DRT(ng)%ncid)
!
!  Read in gud_bbmin.
!
        CALL netcdf_get_fvar(ng, model, DRT(ng)%name,                   &
     &    'bbmin',                                                      &
     &    gud_bbmin(ng),                                                &
     &    ncid = DRT(ng)%ncid)
!
!  Read in gud_bbw.
!
        CALL netcdf_get_fvar(ng, model, DRT(ng)%name,                   &
     &    'bbw',                                                        &
     &    gud_bbw(ng),                                                  &
     &    ncid = DRT(ng)%ncid)
!
!  Read in gud_lambda_aCDOM.
!
        CALL netcdf_get_fvar(ng, model, DRT(ng)%name,                   &
     &    'lambda_aCDOM',                                               &
     &    gud_lambda_aCDOM(ng),                                         &
     &    ncid = DRT(ng)%ncid)
!
!  Read in gud_Sdom.
!
        CALL netcdf_get_fvar(ng, model, DRT(ng)%name,                   &
     &    'Sdom',                                                       &
     &    gud_Sdom(ng),                                                 &
     &    ncid = DRT(ng)%ncid)
!
!  Read in gud_aCDOM_fac.
!
        CALL netcdf_get_fvar(ng, model, DRT(ng)%name,                   &
     &    'aCDOM_fac',                                                  &
     &    gud_aCDOM_fac(ng),                                            &
     &    ncid = DRT(ng)%ncid)
!
!  Read in gud_allomSpectra.
!
        CALL netcdf_get_lvar(ng, model, DRT(ng)%name,                   &
     &    'allomSpectra',                                               &
     &    gud_allomSpectra(ng),                                         &
     &    ncid = DRT(ng)%ncid)
!
!  Read in gud_aCarCell.
!
        CALL netcdf_get_fvar(ng, model, DRT(ng)%name,                   &
     &    'aCarCell',                                                   &
     &    gud_aCarCell(ng),                                             &
     &    ncid = DRT(ng)%ncid)
!
!  Read in gud_bCarCell.
!
        CALL netcdf_get_fvar(ng, model, DRT(ng)%name,                   &
     &    'bCarCell',                                                   &
     &    gud_bCarCell(ng),                                             &
     &    ncid = DRT(ng)%ncid)
!
!  Read in gud_absorpSlope.
!
        CALL netcdf_get_fvar(ng, model, DRT(ng)%name,                   &
     &    'absorpSlope',                                                &
     &    gud_absorpSlope(ng),                                          &
     &    ncid = DRT(ng)%ncid)
!
!  Read in gud_bbbSlope.
!
        CALL netcdf_get_fvar(ng, model, DRT(ng)%name,                   &
     &    'bbbSlope',                                                   &
     &    gud_bbbSlope(ng),                                             &
     &    ncid = DRT(ng)%ncid)
!
!  Read in gud_scatSwitchSizeLog.
!
        CALL netcdf_get_fvar(ng, model, DRT(ng)%name,                   &
     &    'scatSwitchSizeLog',                                          &
     &    ncid = DRT(ng)%ncid,                                          &
     &    gud_scatSwitchSizeLog(:,ng),                                  &
     &    start = (/1/),                                                &
     &    total = (/nlam/))
!
!  Read in gud_scatSlopeSmall.
!
        CALL netcdf_get_fvar(ng, model, DRT(ng)%name,                   &
     &    'scatSlopeSmall',                                             &
     &    ncid = DRT(ng)%ncid,                                          &
     &    gud_scatSlopeSmall(:,ng),                                     &
     &    start = (/1/),                                                &
     &    total = (/nlam/))
!
!  Read in gud_scatSlopeLarge.
!
        CALL netcdf_get_fvar(ng, model, DRT(ng)%name,                   &
     &    'scatSlopeLarge',                                             &
     &    ncid = DRT(ng)%ncid,                                          &
     &    gud_scatSlopeLarge(:,ng),                                     &
     &    start = (/1/),                                                &
     &    total = (/nlam/))
