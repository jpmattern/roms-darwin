!
! created by create_darwin_headerfiles (written by J. Paul Mattern)
!

#if defined DARWIN_RADTRANS
! not reporting values for 3d parameter aphy_chl
! not reporting values for 3d parameter aphy_chl_ps
! not reporting values for 3d parameter bphy_mgC
! not reporting values for 3d parameter bbphy_mgC
#endif
#if defined DARWIN_RANDOM_TRAITS
            WRITE (out,70) Smallgrow(ng), 'Smallgrow',                  &
     &        'small phytoplankton growth rate (d^-1).'
            WRITE (out,70) Biggrow(ng), 'Biggrow',                      &
     &        'big phytoplankton growth rates (d^-1).'
            WRITE (out,70) Smallgrowrange(ng), 'Smallgrowrange',        &
     &        'small phytoplankton growth rate threshold (d^-1).'
            WRITE (out,70) Biggrowrange(ng), 'Biggrowrange',            &
     &        'big phytoplankton growth rate threshold (d^-1).'
            WRITE (out,70) diaz_growfac(ng), 'diaz_growfac',            &
     &        'diazotroph growth multiplier (dimensionless).'
            WRITE (out,70) cocco_growfac(ng), 'cocco_growfac',          &
     &        'coccolithophore growth multiplier (dimensionless).'
            WRITE (out,70) diatom_growfac(ng), 'diatom_growfac',        &
     &        'diatom growth multiplier (dimensionless).'
            WRITE (out,70) Smallmort(ng), 'Smallmort',                  &
     &        'small phytoplankton mortality rate (d^-1).'
            WRITE (out,70) Bigmort(ng), 'Bigmort',                      &
     &        'big phytoplankton mortality rate (d^-1).'
            WRITE (out,80) Smallmortrange(ng), 'Smallmortrange',        &
     &        'small phytoplankton mortality rate threshold',           &
     &        '(d^-1).'
            WRITE (out,70) Bigmortrange(ng), 'Bigmortrange',            &
     &        'big phytoplankton mortality rate threshold (d^-1).'
            WRITE (out,80) Smallexport(ng), 'Smallexport',              &
     &        'small phytoplankton mortality export fraction',          &
     &        '(dimensionless).'
            WRITE (out,80) Bigexport(ng), 'Bigexport',                  &
     &        'big phytoplankton mortality export fraction',            &
     &        '(dimensionless).'
            WRITE (out,70) tempcoeff1(ng), 'tempcoeff1',                &
     &        'unknown description (unknown).'
            WRITE (out,70) tempcoeff2_small(ng), 'tempcoeff2_small',    &
     &        'unknown description (unknown).'
            WRITE (out,70) tempcoeff2_big(ng), 'tempcoeff2_big',        &
     &        'unknown description (unknown).'
            WRITE (out,70) tempcoeff3(ng), 'tempcoeff3',                &
     &        'unknown description (unknown).'
#endif
#if defined DARWIN_RANDOM_TRAITS && defined DARWIN_TEMP_RANGE
            WRITE (out,70) tempmax(ng), 'tempmax',                      &
     &        '32. _d 0 (unknown).'
            WRITE (out,70) temprange(ng), 'temprange',                  &
     &        '30. _d 0 (unknown).'
            WRITE (out,70) tempdecay(ng), 'tempdecay',                  &
     &        'unknown description (unknown).'
#endif
#if defined DARWIN_RANDOM_TRAITS
            WRITE (out,70) val_R_NC(ng), 'val_R_NC',                    &
     &        'unknown description (unknown).'
            WRITE (out,70) val_R_NC_diaz(ng), 'val_R_NC_diaz',          &
     &        'unknown description (unknown).'
            WRITE (out,70) val_R_PC(ng), 'val_R_PC',                    &
     &        'unknown description (unknown).'
            WRITE (out,70) val_R_SiC_diatom(ng), 'val_R_SiC_diatom',    &
     &        '32 for Fannys runs (unknown).'
            WRITE (out,70) val_R_FeC(ng), 'val_R_FeC',                  &
     &        'unknown description (unknown).'
            WRITE (out,70) val_R_FeC_diaz(ng), 'val_R_FeC_diaz',        &
     &        'unknown description (unknown).'
            WRITE (out,70) val_R_PICPOC(ng), 'val_R_PICPOC',            &
     &        'unknown description (unknown).'
#endif
#if defined DARWIN_RANDOM_TRAITS && ! defined DARWIN_GEIDER
            WRITE (out,70) val_R_ChlC(ng), 'val_R_ChlC',                &
     &        'for atten_chl   = atten_p * 16 (unknown).'
#endif
#if defined DARWIN_RANDOM_TRAITS
            WRITE (out,70) val_R_NC_zoo(ng), 'val_R_NC_zoo',            &
     &        'unknown description (unknown).'
            WRITE (out,70) val_R_PC_zoo(ng), 'val_R_PC_zoo',            &
     &        'unknown description (unknown).'
            WRITE (out,70) val_R_SiC_zoo(ng), 'val_R_SiC_zoo',          &
     &        'unknown description (unknown).'
            WRITE (out,70) val_R_FeC_zoo(ng), 'val_R_FeC_zoo',          &
     &        'unknown description (unknown).'
            WRITE (out,70) val_R_PICPOC_zoo(ng), 'val_R_PICPOC_zoo',    &
     &        'unknown description (unknown).'
            WRITE (out,70) val_R_ChlC_zoo(ng), 'val_R_ChlC_zoo',        &
     &        'for atten_chl = atten_p * 16 (unknown).'
            WRITE (out,70) SmallSink_pday(ng), 'SmallSink_pday',        &
     &        'small phytoplankton sinking rate (m d^-1).'
            WRITE (out,70) BigSink_pday(ng), 'BigSink_pday',            &
     &        'big phytoplankton sinking rate (m d^-1).'
            WRITE (out,70) SmallPsat(ng), 'SmallPsat',                  &
     &        'unknown description (unknown).'
            WRITE (out,70) BigPsat(ng), 'BigPsat',                      &
     &        'unknown description (unknown).'
            WRITE (out,70) ProcPsat(ng), 'ProcPsat',                    &
     &        'unknown description (unknown).'
            WRITE (out,70) UniDzPsat(ng), 'UniDzPsat',                  &
     &        'unknown description (unknown).'
            WRITE (out,70) CoccoPsat(ng), 'CoccoPsat',                  &
     &        'by default same as big (unknown).'
            WRITE (out,70) SmallPsatrange(ng), 'SmallPsatrange',        &
     &        'unknown description (unknown).'
            WRITE (out,70) BigPsatrange(ng), 'BigPsatrange',            &
     &        'unknown description (unknown).'
            WRITE (out,70) ProcPsatrange(ng), 'ProcPsatrange',          &
     &        'unknown description (unknown).'
            WRITE (out,70) UniDzPsatrange(ng), 'UniDzPsatrange',        &
     &        'unknown description (unknown).'
            WRITE (out,70) CoccoPsatrange(ng), 'CoccoPsatrange',        &
     &        'unknown description (unknown).'
            WRITE (out,70) ksatNH4fac(ng), 'ksatNH4fac',                &
     &        'unknown description (unknown).'
            WRITE (out,70) ksatNO2fac(ng), 'ksatNO2fac',                &
     &        'unknown description (unknown).'
            WRITE (out,80) val_amminhib(ng), 'val_amminhib',            &
     &        'unknown description',                                    &
     &        '(coefficient for NH4 inhibition of NO uptake ((mmol N/m3)-1)).'
            WRITE (out,70) val_ksatsio2(ng), 'val_ksatsio2',            &
     &        'unknown description (unknown).'
#endif
#if defined DARWIN_RANDOM_TRAITS && ! defined DARWIN_GEIDER
            WRITE (out,70) smallksatpar(ng), 'smallksatpar',            &
     &        '0.8 _d 0 (unknown).'
            WRITE (out,70) smallksatparstd(ng), 'smallksatparstd',      &
     &        '0.3 _d 0 (unknown).'
            WRITE (out,70) smallkinhpar(ng), 'smallkinhpar',            &
     &        '2.0 _d 0 (unknown).'
            WRITE (out,70) smallkinhparstd(ng), 'smallkinhparstd',      &
     &        '0.5 _d 0 (unknown).'
            WRITE (out,70) Bigksatpar(ng), 'Bigksatpar',                &
     &        '0.35 _d 0 (unknown).'
            WRITE (out,70) Bigksatparstd(ng), 'Bigksatparstd',          &
     &        '0.1 _d 0 (unknown).'
            WRITE (out,70) Bigkinhpar(ng), 'Bigkinhpar',                &
     &        '0.5 _d 0 (unknown).'
            WRITE (out,70) Bigkinhparstd(ng), 'Bigkinhparstd',          &
     &        '0.1 _d 0 (unknown).'
#endif
#if defined DARWIN_RANDOM_TRAITS && defined DARWIN_NINE_SPECIES_SETUP
            WRITE (out,70) LLProkinhpar(ng), 'LLProkinhpar',            &
     &        'unknown description (unknown).'
            WRITE (out,70) Coccokinhpar(ng), 'Coccokinhpar',            &
     &        'unknown description (unknown).'
#endif
#if defined DARWIN_RANDOM_TRAITS
            WRITE (out,70) inhibcoef_geid_val(ng), 'inhibcoef_geid_val',&
     &        'DUMMY VAL (unknown).'
            WRITE (out,70) smallmQyield(ng), 'smallmQyield',            &
     &        'unknown description (mmol C (uEin)-1).'
            WRITE (out,70) smallmQyieldrange(ng), 'smallmQyieldrange',  &
     &        'unknown description (mmol C (uEin)-1).'
            WRITE (out,70) BigmQyield(ng), 'BigmQyield',                &
     &        'unknown description (mmol C (uEin)-1).'
            WRITE (out,70) BigmQyieldrange(ng), 'BigmQyieldrange',      &
     &        'unknown description (mmol C (uEin)-1).'
            WRITE (out,70) smallchl2cmax(ng), 'smallchl2cmax',          &
     &        'unknown description (mg Chl (mmol C)).'
            WRITE (out,70) smallchl2cmaxrange(ng), 'smallchl2cmaxrange',&
     &        'unknown description (mg Chl (mmol C)).'
            WRITE (out,70) Bigchl2cmax(ng), 'Bigchl2cmax',              &
     &        'unknown description (mg Chl (mmol C)).'
            WRITE (out,70) Bigchl2cmaxrange(ng), 'Bigchl2cmaxrange',    &
     &        'unknown description (mg Chl (mmol C)).'
#endif
#if ! defined DARWIN_RADTRANS
            WRITE (out,80) aphy_chl_ave(ng), 'aphy_chl_ave',            &
     &        'multiplication with chla gives absorption m-1',          &
     &        '(m^2 (mg_chl)^-1).'
#endif
#if defined DARWIN_RANDOM_TRAITS
            WRITE (out,70) val_acclimtimescl(ng), 'val_acclimtimescl',  &
     &        'inverse time scale for Chl acclimation (unknown).'
            WRITE (out,72) oldTwoGrazers(ng), 'oldTwoGrazers',          &
     &        'old defaults for 2 grazers (unknown).'
            WRITE (out,70) GrazeFast_pday(ng), 'GrazeFast_pday',        &
     &        'maximum grazing rate (d^-1).'
            WRITE (out,70) ZooexfacSmall(ng), 'ZooexfacSmall',          &
     &        'unknown description (unknown).'
            WRITE (out,70) ZooexfacBig(ng), 'ZooexfacBig',              &
     &        'unknown description (unknown).'
            WRITE (out,70) ZoomortSmall_pday(ng), 'ZoomortSmall_pday',  &
     &        'small zooplankton mortality rate (d^-1).'
            WRITE (out,70) ZoomortBig_pday(ng), 'ZoomortBig_pday',      &
     &        'big zooplankton mortality rate (d^-1).'
            WRITE (out,80) ZoomortSmall2(ng), 'ZoomortSmall2',          &
     &        'small zooplankton quadratic mortality rate',             &
     &        '((mmol C m^-3)^-1 s^-1).'
            WRITE (out,80) ZoomortBig2(ng), 'ZoomortBig2',              &
     &        'big zooplankton quadratic mortality rate',               &
     &        '((mmol C m^-3)^-1 s^-1).'
            WRITE (out,70) ExGrazfracbig(ng), 'ExGrazfracbig',          &
     &        'unknown description (unknown).'
            WRITE (out,70) ExGrazfracsmall(ng), 'ExGrazfracsmall',      &
     &        'unknown description (unknown).'
            WRITE (out,70) palathi(ng), 'palathi',                      &
     &        'unknown description (unknown).'
            WRITE (out,70) palatlo(ng), 'palatlo',                      &
     &        'unknown description (unknown).'
            WRITE (out,70) diatomgraz(ng), 'diatomgraz',                &
     &        'unknown description (unknown).'
            WRITE (out,70) coccograz(ng), 'coccograz',                  &
     &        'unknown description (unknown).'
            WRITE (out,70) olargegraz(ng), 'olargegraz',                &
     &        'unknown description (unknown).'
            WRITE (out,70) GrazeEfflow(ng), 'GrazeEfflow',              &
     &        'unknown description (unknown).'
            WRITE (out,70) GrazeEffmod(ng), 'GrazeEffmod',              &
     &        'unknown description (unknown).'
            WRITE (out,70) GrazeEffhi(ng), 'GrazeEffhi',                &
     &        'unknown description (unknown).'
            WRITE (out,70) GrazeRate_pday(ng), 'GrazeRate_pday',        &
     &        'grazing rate (d^-1).'
            WRITE (out,80) ExGrazfrac(ng), 'ExGrazfrac',                &
     &        'fraction of sloppy feeding that goes to particulate',    &
     &        '(unknown).'
            WRITE (out,70) val_palat(ng), 'val_palat',                  &
     &        'need to set in data.traits (unknown).'
            WRITE (out,70) val_ass_eff(ng), 'val_ass_eff',              &
     &        'grazing efficiency (unknown).'
            WRITE (out,70) kgrazesat_val(ng), 'kgrazesat_val',          &
     &        'unknown description (= 0.1 mmol P m-3).'
            WRITE (out,70) Zoomort_pday(ng), 'Zoomort_pday',            &
     &        'zooplankton mortality rate (d^-1).'
            WRITE (out,80) Zoomort2(ng), 'Zoomort2',                    &
     &        'zooplankton quadratic mortality rate',                   &
     &        '((mmol C m^-3)^-1 s^-1).'
            WRITE (out,80) Zooexfac(ng), 'Zooexfac',                    &
     &        'fraction of dead zoo that goes to particulate',          &
     &        '(unknown).'
            WRITE (out,70) ZooDM(ng), 'ZooDM',                          &
     &        'diameter (not used so far) (unknown).'
#endif
#if ! defined DARWIN_RANDOM_TRAITS
            WRITE (out,72) gud_sort_biovol(ng), 'gud_sort_biovol',      &
     &        'sort plankton types by biovol (dimensionless).'
#endif
            WRITE (out,82) GUD_effective_ksat(ng), 'GUD_effective_ksat',&
     &        'compute eff half sat for uptake of non-quota elem',      &
     &        '(dimensionless).'
            WRITE (out,81) gud_select_kn_allom(ng),                     &
     &        'gud_select_kn_allom',                                    &
     &        '1: use Ward et al formulation, 2: use Follett et al',    &
     &        '(dimensionless).'
            WRITE (out,70) logvolbase(ng), 'logvolbase',                &
     &        'biovol = 10**(logvolbase+...) (dimensionless).'
#if ! defined DARWIN_RANDOM_TRAITS
            WRITE (out,80) logvolinc(ng), 'logvolinc',                  &
     &        'used to compute biovol (not always active)',             &
     &        '(dimensionless).'
#endif
            WRITE (out,130) 'biovol0',                                  &
     &        'used to compute biovol (not always active)',             &
     &        '(dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) biovol0(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,130) 'biovolfac',                                &
     &        'used to compute biovol (not always active)',             &
     &        '(dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) biovolfac(is,ng), TRIM(grp_names(is))
            END DO
#if ! defined DARWIN_RANDOM_TRAITS
            WRITE (out,130) 'logvol0ind',                               &
     &        'used to compute biovol (not always active)',             &
     &        '(dimensionless).'
            DO is=1,nGroup
              WRITE (out,141) logvol0ind(is,ng), TRIM(grp_names(is))
            END DO
! not reporting values for 3d parameter grp_logvolind
! not reporting values for 3d parameter grp_biovol
            WRITE (out,130) 'grp_nplank',                               &
     &        'number of plankton variables in group',                  &
     &        '(dimensionless).'
            DO is=1,nGroup
              WRITE (out,141) grp_nplank(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'grp_photo',                                &
     &        'group perform photosynthesis (dimensionless).'
            DO is=1,nGroup
              WRITE (out,141) grp_photo(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,130) 'grp_bacttype',                             &
     &        '1: particle-associated, 2: free-living',                 &
     &        '(dimensionless).'
            DO is=1,nGroup
              WRITE (out,141) grp_bacttype(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'grp_aerobic',                              &
     &        'group value for isAerobic (dimensionless).'
            DO is=1,nGroup
              WRITE (out,141) grp_aerobic(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'grp_denit',                                &
     &        'group is denitrifier (dimensionless).'
            DO is=1,nGroup
              WRITE (out,141) grp_denit(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'grp_pred',                                 &
     &        'group is predator (dimensionless).'
            DO is=1,nGroup
              WRITE (out,141) grp_pred(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'grp_prey',                                 &
     &        'group is prey (dimensionless).'
            DO is=1,nGroup
              WRITE (out,141) grp_prey(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'grp_hasSi',                                &
     &        'group has silicate (diatom) (dimensionless).'
            DO is=1,nGroup
              WRITE (out,141) grp_hasSi(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'grp_hasPIC',                               &
     &        'group has PIC (coccolithophore) (dimensionless).'
            DO is=1,nGroup
              WRITE (out,141) grp_hasPIC(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'grp_diazo',                                &
     &        'group is diazotroph (dimensionless).'
            DO is=1,nGroup
              WRITE (out,141) grp_diazo(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'grp_useNH4',                               &
     &        'group uses NH4 (dimensionless).'
            DO is=1,nGroup
              WRITE (out,141) grp_useNH4(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'grp_useNO2',                               &
     &        'group uses NO2 (dimensionless).'
            DO is=1,nGroup
              WRITE (out,141) grp_useNO2(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'grp_useNO3',                               &
     &        'group uses NO3 (dimensionless).'
            DO is=1,nGroup
              WRITE (out,141) grp_useNO3(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'grp_combNO',                               &
     &        'group value for combNO (dimensionless).'
            DO is=1,nGroup
              WRITE (out,141) grp_combNO(is,ng), TRIM(grp_names(is))
            END DO
#endif
#if defined DARWIN_RADTRANS && ! defined DARWIN_RANDOM_TRAITS
            WRITE (out,120) 'grp_aptype',                               &
     &        'group absorption spectra type (dimensionless).'
            DO is=1,nGroup
              WRITE (out,141) grp_aptype(is,ng), TRIM(grp_names(is))
            END DO
#endif
#if ! defined DARWIN_RANDOM_TRAITS
            WRITE (out,120) 'grp_tempMort',                             &
     &        'group value for tempmort (dimensionless).'
            DO is=1,nGroup
              WRITE (out,141) grp_tempMort(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'grp_tempMort2',                            &
     &        'group value for tempmort2 (dimensionless).'
            DO is=1,nGroup
              WRITE (out,141) grp_tempMort2(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'grp_Xmin',                                 &
     &        'group value for Xmin (mmol C m^-3).'
            DO is=1,nGroup
              WRITE (out,140) grp_Xmin(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'grp_R_NC',                                 &
     &        'group value N:C ratio (mmol N (mmol C)^-1).'
            DO is=1,nGroup
              WRITE (out,140) grp_R_NC(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'grp_R_PC',                                 &
     &        'group value P:C ratio (mmol P (mmol C)^-1).'
            DO is=1,nGroup
              WRITE (out,140) grp_R_PC(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'grp_R_SiC',                                &
     &        'group value Si:C ratio (mmol Si (mmol C)^-1).'
            DO is=1,nGroup
              WRITE (out,140) grp_R_SiC(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'grp_R_FeC',                                &
     &        'group value Fe:C ratio (mmol Fe (mmol C)^-1).'
            DO is=1,nGroup
              WRITE (out,140) grp_R_FeC(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'grp_R_ChlC',                               &
     &        'group value Chl:C ratio (mg Chl (mmol C)^-1).'
            DO is=1,nGroup
              WRITE (out,140) grp_R_ChlC(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'grp_R_PICPOC',                             &
     &        'group value PIC:POC ratio (dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) grp_R_PICPOC(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,130) 'grp_ExportFracMort',                       &
     &        'group value for fraction of linear mortality to POM',    &
     &        '(dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) grp_ExportFracMort(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,130) 'grp_ExportFracMort2',                      &
     &        'group value for frac of quad mort to POM',               &
     &        '(dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) grp_ExportFracMort2(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,130) 'grp_ExportFrac',                           &
     &        'group value for fraction of exudation to POM',           &
     &        '(dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) grp_ExportFrac(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'grp_mort_pday',                            &
     &        'group mortality rate (d^-1).'
            DO is=1,nGroup
              WRITE (out,140) grp_mort_pday(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,130) 'grp_mort2',                                &
     &        'group quadratic mortality rate',                         &
     &        '((mmol C m^-3)^-1 s^-1).'
            DO is=1,nGroup
              WRITE (out,140) grp_mort2(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,130) 'grp_tempcoeff1',                           &
     &        'group value for phytoTempCoeff value',                   &
     &        '(dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) grp_tempcoeff1(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'grp_tempcoeff2',                           &
     &        'group value for phytoTempExp2 (dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) grp_tempcoeff2(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'grp_tempcoeff3',                           &
     &        'group value for phytoTempExp1 (dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) grp_tempcoeff3(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'grp_tempopt',                              &
     &        'group value for phytoTempOptimum (deg_C).'
            DO is=1,nGroup
              WRITE (out,140) grp_tempopt(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'grp_tempdecay',                            &
     &        'group value for phytoDecayPower (dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) grp_tempdecay(is,ng), TRIM(grp_names(is))
            END DO
#endif
#if defined DARWIN_ALLOMETRIC_PALAT && ! defined DARWIN_RANDOM_TRAITS
            WRITE (out,130) 'grp_pp_sig',                               &
     &        'standard deviation of size preference',                  &
     &        '(dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) grp_pp_sig(is,ng), TRIM(grp_names(is))
            END DO
#endif
#if ! defined DARWIN_RANDOM_TRAITS && defined DARWIN_GEIDER
            WRITE (out,120) 'grp_mQyield',                              &
     &        'group value for mQyield (mmol C (uEin)^-1).'
            DO is=1,nGroup
              WRITE (out,140) grp_mQyield(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'grp_chl2cmax',                             &
     &        'group value max chl:C ratio (mg Chl (mmol C)^-1).'
            DO is=1,nGroup
              WRITE (out,140) grp_chl2cmax(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,130) 'grp_inhibcoef_geid',                       &
     &        'group value for Geider light inhibition coefficient',    &
     &        '(dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) grp_inhibcoef_geid(is,ng), TRIM(grp_names(is))
            END DO
#endif
#if ! defined DARWIN_RANDOM_TRAITS && ! defined DARWIN_GEIDER
            WRITE (out,120) 'grp_ksatPAR',                              &
     &        'group value for ksatPAR ((uEin m^-2 s^-1)^-1).'
            DO is=1,nGroup
              WRITE (out,140) grp_ksatPAR(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,130) 'grp_kinhPAR',                              &
     &        'group value plankton PAR inhibition coefficient',        &
     &        '((uEin m^-2 s^-1)^-1).'
            DO is=1,nGroup
              WRITE (out,140) grp_kinhPAR(is,ng), TRIM(grp_names(is))
            END DO
#endif
#if ! defined DARWIN_RANDOM_TRAITS
            WRITE (out,120) 'grp_ksatNH4fac',                           &
     &        'group value for ksatNH4 multiplier (dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) grp_ksatNH4fac(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'grp_ksatNO2fac',                           &
     &        'group value for ksatNO2 multiplier (dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) grp_ksatNO2fac(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,130) 'grp_amminhib',                             &
     &        'group coefficient for NH4 inhibition of NO uptake',      &
     &        '((mmol N/m^3)^-1).'
            DO is=1,nGroup
              WRITE (out,140) grp_amminhib(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'grp_acclimtimescl_pday',                   &
     &        'inverse time scale for Chl acclimation (d^-1).'
            DO is=1,nGroup
              WRITE (out,140) grp_acclimtimescl_pday(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'a_graz_pday',                              &
     &        'a-coefficient for grazemax (d^-1 um^-3).'
            DO is=1,nGroup
              WRITE (out,140) a_graz_pday(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'b_graz',                                   &
     &        'b-coefficient for grazemax (dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) b_graz(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'a_kg',                                     &
     &        'a-coefficient for kgrazesat (mmol C m^-3 um^-3).'
            DO is=1,nGroup
              WRITE (out,140) a_kg(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'b_kg',                                     &
     &        'b-coefficient for kgrazesat (dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) b_kg(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'a_biosink_pday',                           &
     &        'a-coefficient for wsink (m d^-1 um^-3).'
            DO is=1,nGroup
              WRITE (out,140) a_biosink_pday(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'b_biosink',                                &
     &        'b-coefficient for wsink (dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) b_biosink(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'a_bioswim_pday',                           &
     &        'a-coefficient for wswim (m d^-1 um^-3).'
            DO is=1,nGroup
              WRITE (out,140) a_bioswim_pday(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'b_bioswim',                                &
     &        'b-coefficient for wswim (dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) b_bioswim(is,ng), TRIM(grp_names(is))
            END DO
#endif
#if defined DARWIN_ALLOMETRIC_PALAT && ! defined DARWIN_RANDOM_TRAITS
            WRITE (out,120) 'a_prdpry',                                 &
     &        'a-coefficient for pp_opt (um^-3).'
            DO is=1,nGroup
              WRITE (out,140) a_prdpry(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'b_prdpry',                                 &
     &        'b-coefficient for pp_opt (dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) b_prdpry(is,ng), TRIM(grp_names(is))
            END DO
#endif
#if ! defined DARWIN_RANDOM_TRAITS
            WRITE (out,120) 'a_vmax_DIC_pday',                          &
     &        'a-coefficient for PCmax (d^-1 um^-3).'
            DO is=1,nGroup
              WRITE (out,140) a_vmax_DIC_pday(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'b_vmax_DIC',                               &
     &        'b-coefficient for PCmax (dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) b_vmax_DIC(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'a_qcarbon',                                &
     &        'a-coefficient for qcarbon (um^-3).'
            DO is=1,nGroup
              WRITE (out,140) a_qcarbon(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'b_qcarbon',                                &
     &        'b-coefficient for qcarbon (dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) b_qcarbon(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,130) 'a_respir',                                 &
     &        'a-coefficient for respiration',                          &
     &        '(mmol C cell^-1 s^-1).'
            DO is=1,nGroup
              WRITE (out,140) a_respir(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'b_respir',                                 &
     &        'b-coefficient for respiration (dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) b_respir(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'a_kexc_c',                                 &
     &        'a-coefficient for kexcC (s^-1 um^-3).'
            DO is=1,nGroup
              WRITE (out,140) a_kexc_c(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'b_kexc_c',                                 &
     &        'b-coefficient for kexcC (dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) b_kexc_c(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,130) 'a_vmax_NO3_pday',                          &
     &        'a-coefficient for Vmax_NO3',                             &
     &        '(mmol N (mmol C)^-1 d^-1 um^-3).'
            DO is=1,nGroup
              WRITE (out,140) a_vmax_NO3_pday(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'b_vmax_NO3',                               &
     &        'b-coefficient for Vmax_NO3 (dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) b_vmax_NO3(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'a_kn_NO3',                                 &
     &        'a-coefficient for ksatNO3 (mmol N m-3 um^-3).'
            DO is=1,nGroup
              WRITE (out,140) a_kn_NO3(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'b_kn_NO3',                                 &
     &        'b-coefficient for ksatNO3 (dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) b_kn_NO3(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'a_qmin_n',                                 &
     &        'a-coefficient for Qnmin (mmol N (mmol C)^-1 um^-3).'
            DO is=1,nGroup
              WRITE (out,140) a_qmin_n(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'b_qmin_n',                                 &
     &        'b-coefficient for Qnmin (dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) b_qmin_n(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'a_qmax_n',                                 &
     &        'a-coefficient for Qnmax (mmol N (mmol C)^-1 um^-3).'
            DO is=1,nGroup
              WRITE (out,140) a_qmax_n(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'b_qmax_n',                                 &
     &        'b-coefficient for Qnmax (dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) b_qmax_n(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'a_kexc_n',                                 &
     &        'a-coefficient for kexcN (s^-1 um^-3).'
            DO is=1,nGroup
              WRITE (out,140) a_kexc_n(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'b_kexc_n',                                 &
     &        'b-coefficient for kexcN (dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) b_kexc_n(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,130) 'a_vmax_NO2_pday',                          &
     &        'a-coefficient for Vmax_NO2',                             &
     &        '(mmol N (mmol C)^-1 d^-1 um^-3).'
            DO is=1,nGroup
              WRITE (out,140) a_vmax_NO2_pday(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'b_vmax_NO2',                               &
     &        'b-coefficient for Vmax_NO2 (dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) b_vmax_NO2(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'a_kn_NO2',                                 &
     &        'a-coefficient for ksatNO2 (mmol N m-3 um^-3).'
            DO is=1,nGroup
              WRITE (out,140) a_kn_NO2(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'b_kn_NO2',                                 &
     &        'b-coefficient for ksatNO2 (dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) b_kn_NO2(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,130) 'a_vmax_NH4_pday',                          &
     &        'a-coefficient for Vmax_NH4',                             &
     &        '(mmol N (mmol C)^-1 d^-1 um^-3).'
            DO is=1,nGroup
              WRITE (out,140) a_vmax_NH4_pday(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'b_vmax_NH4',                               &
     &        'b-coefficient for Vmax_NH4 (dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) b_vmax_NH4(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'a_kn_NH4',                                 &
     &        'a-coefficient for ksatNH4 (mmol N m-3 um^-3).'
            DO is=1,nGroup
              WRITE (out,140) a_kn_NH4(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'b_kn_NH4',                                 &
     &        'b-coefficient for ksatNH4 (dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) b_kn_NH4(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,130) 'a_vmax_N_pday',                            &
     &        'a-coefficient for Vmax_N',                               &
     &        '(mmol N (mmol C)^-1 d^-1 um^-3).'
            DO is=1,nGroup
              WRITE (out,140) a_vmax_N_pday(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'b_vmax_N',                                 &
     &        'b-coefficient for Vmax_N (dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) b_vmax_N(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,130) 'a_vmax_PO4_pday',                          &
     &        'a-coefficient for Vmax_PO4',                             &
     &        '(mmol P (mmol C)^-1 d^-1 um^-3).'
            DO is=1,nGroup
              WRITE (out,140) a_vmax_PO4_pday(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'b_vmax_PO4',                               &
     &        'b-coefficient for Vmax_PO4 (dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) b_vmax_PO4(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'a_kn_PO4',                                 &
     &        'a-coefficient for ksatPO4 (mmol P m-3 um^-3).'
            DO is=1,nGroup
              WRITE (out,140) a_kn_PO4(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'b_kn_PO4',                                 &
     &        'b-coefficient for ksatPO4 (dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) b_kn_PO4(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'a_qmin_p',                                 &
     &        'a-coefficient for Qpmin (mmol P (mmol C)^-1 um^-3).'
            DO is=1,nGroup
              WRITE (out,140) a_qmin_p(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'b_qmin_p',                                 &
     &        'b-coefficient for Qpmin (dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) b_qmin_p(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'a_qmax_p',                                 &
     &        'a-coefficient for Qpmax (mmol P (mmol C)^-1 um^-3).'
            DO is=1,nGroup
              WRITE (out,140) a_qmax_p(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'b_qmax_p',                                 &
     &        'b-coefficient for Qpmax (dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) b_qmax_p(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'a_kexc_p_pday',                            &
     &        'a-coefficient for kexcP (d^-1 um^-3).'
            DO is=1,nGroup
              WRITE (out,140) a_kexc_p_pday(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'b_kexc_p',                                 &
     &        'b-coefficient for kexcP (dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) b_kexc_p(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,130) 'a_vmax_SiO2_pday',                         &
     &        'a-coefficient for Vmax_SiO2',                            &
     &        '(mmol Si (mmol C)^-1 d^-1 um^-3).'
            DO is=1,nGroup
              WRITE (out,140) a_vmax_SiO2_pday(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'b_vmax_SiO2',                              &
     &        'b-coefficient for Vmax_SiO2 (dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) b_vmax_SiO2(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'a_kn_SiO2',                                &
     &        'a-coefficient for ksatSiO2 (mmol Si m-3 um^-3).'
            DO is=1,nGroup
              WRITE (out,140) a_kn_SiO2(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'b_kn_SiO2',                                &
     &        'b-coefficient for ksatSiO2 (dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) b_kn_SiO2(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,130) 'a_qmin_si',                                &
     &        'a-coefficient for Qsimin',                               &
     &        '(mmol Si (mmol C)^-1 um^-3).'
            DO is=1,nGroup
              WRITE (out,140) a_qmin_si(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'b_qmin_si',                                &
     &        'b-coefficient for Qsimin (dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) b_qmin_si(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,130) 'a_qmax_si',                                &
     &        'a-coefficient for Qsimax',                               &
     &        '(mmol Si (mmol C)^-1 um^-3).'
            DO is=1,nGroup
              WRITE (out,140) a_qmax_si(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'b_qmax_si',                                &
     &        'b-coefficient for Qsimax (dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) b_qmax_si(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'a_kexc_si_pday',                           &
     &        'a-coefficient for kexcSi (d^-1 um^-3).'
            DO is=1,nGroup
              WRITE (out,140) a_kexc_si_pday(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'b_kexc_si',                                &
     &        'b-coefficient for kexcSi (dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) b_kexc_si(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,130) 'a_vmax_FeT_pday',                          &
     &        'a-coefficient for Vmax_FeT',                             &
     &        '(mmol Fe (mmol C)^-1 d^-1 um^-3).'
            DO is=1,nGroup
              WRITE (out,140) a_vmax_FeT_pday(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'b_vmax_FeT',                               &
     &        'b-coefficient for Vmax_FeT (dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) b_vmax_FeT(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'a_kn_feT',                                 &
     &        'a-coefficient for ksatFeT (mmol Fe m-3 um^-3).'
            DO is=1,nGroup
              WRITE (out,140) a_kn_feT(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'b_kn_FeT',                                 &
     &        'b-coefficient for ksatFeT (dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) b_kn_FeT(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,130) 'a_qmin_fe',                                &
     &        'a-coefficient for Qfemin',                               &
     &        '(mmol Fe (mmol C)^-1 um^-3).'
            DO is=1,nGroup
              WRITE (out,140) a_qmin_fe(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'b_qmin_fe',                                &
     &        'b-coefficient for Qfemin (dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) b_qmin_fe(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,130) 'a_qmax_fe',                                &
     &        'a-coefficient for Qfemax',                               &
     &        '(mmol Fe (mmol C)^-1 um^-3).'
            DO is=1,nGroup
              WRITE (out,140) a_qmax_fe(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'b_qmax_fe',                                &
     &        'b-coefficient for Qfemax (dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) b_qmax_fe(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'a_kexc_fe_pday',                           &
     &        'a-coefficient for kexcFe (d^-1 um^-3).'
            DO is=1,nGroup
              WRITE (out,140) a_kexc_fe_pday(is,ng), TRIM(grp_names(is))
            END DO
            WRITE (out,120) 'b_kexc_fe',                                &
     &        'b-coefficient for kexcFe (dimensionless).'
            DO is=1,nGroup
              WRITE (out,140) b_kexc_fe(is,ng), TRIM(grp_names(is))
            END DO
! not reporting values for 3d parameter grp_ExportFracPreyPred
! not reporting values for 3d parameter grp_ass_eff
#endif
#if defined DARWIN_RANDOM_TRAITS
            WRITE (out,80) phymin(ng), 'phymin',                        &
     &        'min phyto (below which grazing and mort doesnt happ',    &
     &        '(unknown).'
#endif
            WRITE (out,70) katten_w(ng), 'katten_w',                    &
     &        'atten coefficient water (m-1).'
            WRITE (out,70) katten_chl(ng), 'katten_chl',                &
     &        'atten coefficient chl ((mmol chl/m3)-1).'
            WRITE (out,70) parfrac(ng), 'parfrac',                      &
     &        'fraction Qsw that is PAR (dimensionless).'
#if DARWIN_TEMP_VERSION == 1
            WRITE (out,80) tempnorm(ng), 'tempnorm',                    &
     &        'phytoplankton temperature coefficient for photoFun',     &
     &        '(dimensionless).'
#endif
            WRITE (out,70) alpfe(ng), 'alpfe',                          &
     &        'solubility of Fe dust (m s^-1).'
#if ! defined DARWIN_PART_SCAV_POP && ! defined DARWIN_PART_SCAV
            WRITE (out,70) scav_pyear(ng), 'scav_pyear',                &
     &        'iron chem scavenging rate (year^-1).'
#endif
            WRITE (out,70) ligand_tot(ng), 'ligand_tot',                &
     &        'total ligand (mmol m^-3).'
            WRITE (out,70) ligand_stab(ng), 'ligand_stab',              &
     &        'ligand stability rate ratio (m^3 mol^-1).'
#if defined DARWIN_MINFE
            WRITE (out,70) freefemax(ng), 'freefemax',                  &
     &        'maximum value for free iron (mmol Fe m^-3).'
#endif
#if defined DARWIN_PART_SCAV_POP || defined DARWIN_PART_SCAV
            WRITE (out,70) scav_rat_pday(ng), 'scav_rat_pday',          &
     &        'iron chem scavenging rate (d^-1).'
            WRITE (out,70) scav_inter(ng), 'scav_inter',                &
     &        'iron chem scavenging coefficient (dimensionless).'
            WRITE (out,70) scav_exp(ng), 'scav_exp',                    &
     &        'iron chem scavenging coefficient (dimensionless).'
#endif
#if defined DARWIN_PART_SCAV_POP
            WRITE (out,70) scav_R_POPPOC(ng), 'scav_R_POPPOC',          &
     &        'scavenging POP:POC ratio (mmol P/mmol C).'
#endif
#if ! defined DARWIN_IRON_SED_SOURCE_VARIABLE
            WRITE (out,70) fesedflux_pday(ng), 'fesedflux_pday',        &
     &        'iron flux (mmol Fe m^-2 d^-1).'
#endif
#if defined DARWIN_IRON_SED_SOURCE_VARIABLE
            WRITE (out,70) fesedflux_pcm(ng), 'fesedflux_pcm',          &
     &        'iron flux relative to POC flux (mmol Fe/mmol C).'
            WRITE (out,80) R_CP_fesed(ng), 'R_CP_fesed',                &
     &        'sedimentary C:P ratio for iron flux computation',        &
     &        '(mmol C/mmol P).'
#endif
            WRITE (out,70) Knita_pday(ng), 'Knita_pday',                &
     &        'oxidation rates for ammonium (d^-1).'
            WRITE (out,70) Knitb_pday(ng), 'Knitb_pday',                &
     &        'oxidation rates for nitrite (d^-1).'
            WRITE (out,80) PAR_oxi(ng), 'PAR_oxi',                      &
     &        'critical light level after which oxidation starts',      &
     &        '(uEin/m2/s).'
            WRITE (out,70) Kdoc_pday(ng), 'Kdoc_pday',                  &
     &        'DOC remin rate (d^-1).'
            WRITE (out,70) Kdop_pday(ng), 'Kdop_pday',                  &
     &        'DOP remin rate (d^-1).'
            WRITE (out,70) Kdon_pday(ng), 'Kdon_pday',                  &
     &        'DON remin rate (d^-1).'
            WRITE (out,70) KdoFe_pday(ng), 'KdoFe_pday',                &
     &        'DOFe remin rate (d^-1).'
            WRITE (out,70) KPOC_pday(ng), 'KPOC_pday',                  &
     &        'POC remin rate (d^-1).'
            WRITE (out,70) KPOP_pday(ng), 'KPOP_pday',                  &
     &        'POP remin rate (d^-1).'
            WRITE (out,70) KPON_pday(ng), 'KPON_pday',                  &
     &        'PON remin rate (d^-1).'
            WRITE (out,70) KPOFe_pday(ng), 'KPOFe_pday',                &
     &        'POFe remin rate (d^-1).'
            WRITE (out,70) KPOSi_pday(ng), 'KPOSi_pday',                &
     &        'POSi remin rate (d^-1).'
            WRITE (out,70) wC_sink_pday(ng), 'wC_sink_pday',            &
     &        'POC sinking rate (m/s s d^-1).'
            WRITE (out,70) wP_sink_pday(ng), 'wP_sink_pday',            &
     &        'POP sinking rate (d^-1).'
            WRITE (out,70) wN_sink_pday(ng), 'wN_sink_pday',            &
     &        'PON sinking rate (d^-1).'
            WRITE (out,70) wFe_sink_pday(ng), 'wFe_sink_pday',          &
     &        'POFe sinking rate (d^-1).'
            WRITE (out,70) wSi_sink_pday(ng), 'wSi_sink_pday',          &
     &        'POSi sinking rate (d^-1).'
            WRITE (out,70) wPIC_sink_pday(ng), 'wPIC_sink_pday',        &
     &        'PIC sinking rate (m/s s d^-1).'
            WRITE (out,70) Kdissc_pday(ng), 'Kdissc_pday',              &
     &        'PIC dissociation rate (d^-1).'
#if defined DARWIN_CARBON
            WRITE (out,70) gud_atmos_pCO2(ng), 'gud_atmos_pCO2',        &
     &        'CO2 fraction of air (dimensionless).'
            WRITE (out,70) R_OP(ng), 'R_OP',                            &
     &        'O:P ratio (mmol O (mmol P)-1).'
            WRITE (out,70) m3perkg(ng), 'm3perkg',                      &
     &        'm^-3 per kg seawater conversion (m^3/kg).'
            WRITE (out,80) surfSiMinInit(ng), 'surfSiMinInit',          &
     &        'surface minimum Si for pH computation',                  &
     &        '(mmol Si m^-3).'
            WRITE (out,80) surfSaltMin(ng), 'surfSaltMin',              &
     &        'surface minimum salinity for pH computation',            &
     &        '(dimensionless).'
            WRITE (out,80) surfSaltMax(ng), 'surfSaltMax',              &
     &        'surface maximum salinity for pH computation',            &
     &        '(dimensionless).'
            WRITE (out,80) surfTempMin(ng), 'surfTempMin',              &
     &        'surface minimum temperature for pH computation',         &
     &        '(deg C).'
            WRITE (out,80) surfTempMax(ng), 'surfTempMax',              &
     &        'surface maximum temperature for pH computation',         &
     &        '(deg C).'
            WRITE (out,80) surfDICMin(ng), 'surfDICMin',                &
     &        'surface minimum DIC for pH computation',                 &
     &        '(mmol C m^-3).'
            WRITE (out,80) surfDICMax(ng), 'surfDICMax',                &
     &        'surface maximum DIC for pH computation',                 &
     &        '(mmol C m^-3).'
            WRITE (out,80) surfALKMin(ng), 'surfALKMin',                &
     &        'surface minimum alkalinity for pH computation',          &
     &        '(mmol C m^-3).'
            WRITE (out,80) surfALKMax(ng), 'surfALKMax',                &
     &        'surface maximum alkalinity for pH computation',          &
     &        '(mmol C m^-3).'
            WRITE (out,80) surfPO4Min(ng), 'surfPO4Min',                &
     &        'surface minimum PO4 for pH computation',                 &
     &        '(mmol P m^-3).'
            WRITE (out,80) surfPO4Max(ng), 'surfPO4Max',                &
     &        'surface maximum PO4 for pH computation',                 &
     &        '(mmol P m^-3).'
            WRITE (out,80) surfSiMax(ng), 'surfSiMax',                  &
     &        'surface maximum Si for pH computation',                  &
     &        '(mmol Si m^-3).'
            WRITE (out,80) O2crit(ng), 'O2crit',                        &
     &        'critical oxygen for O2/NO3 remineralization',            &
     &        '(mmol O).'
#endif
#if defined DARWIN_DENIT
            WRITE (out,80) denit_NP(ng), 'denit_NP',                    &
     &        'ratio of n to p in denitrification process',             &
     &        '(mmol N/(mmol P)).'
            WRITE (out,80) denit_NO3(ng), 'denit_NO3',                  &
     &        'ratio no3 used relative to all n in denit process',      &
     &        '(dimensionless).'
            WRITE (out,80) NO3crit(ng), 'NO3crit',                      &
     &        'critical NO3 below which no denit (or remin) happen',    &
     &        '(mmol N m^-3).'
#endif
            WRITE (out,70) PARmin(ng), 'PARmin',                        &
     &        'minimum light for photosynthesis (uEin/m2/s).'
#if defined DARWIN_GEIDER && defined DARWIN_CHLQUOTA && defined DARWIN_NQUOTA
            WRITE (out,70) chl2nmax(ng), 'chl2nmax',                    &
     &        'maximum chl:N ratio (mg chl/mmol N).'
            WRITE (out,70) synthcost(ng), 'synthcost',                  &
     &        'cost of biosynthesis (mmol C/mmol N).'
#endif
#if defined DARWIN_ALLOMETRIC_PALAT && ! defined DARWIN_RANDOM_TRAITS
            WRITE (out,70) palat_min(ng), 'palat_min',                  &
     &        'mimimum palatability threshold (dimensionless).'
#endif
            WRITE (out,70) inhib_graz(ng), 'inhib_graz',                &
     &        'for quota-style grazing ((mmol C m-3)-1).'
            WRITE (out,80) inhib_graz_exp(ng), 'inhib_graz_exp',        &
     &        'grazing inhib. exponent (0.0 turns it off)',             &
     &        '(dimensionless).'
            WRITE (out,80) hillnum(ng), 'hillnum',                      &
     &        'exponent for limiting quota uptake in grazing',          &
     &        '(dimensionless).'
            WRITE (out,80) hollexp(ng), 'hollexp',                      &
     &        'grazing exponential 1=holling 2, 2=holling 3',           &
     &        '(dimensionless).'
            WRITE (out,70) phygrazmin(ng), 'phygrazmin',                &
     &        'minimum total prey conc (mmol C m-1).'
            WRITE (out,70) pmaxPON_pday(ng), 'pmaxPON_pday',            &
     &        'max growth rate for bacteria (d^-1).'
            WRITE (out,70) pmaxDON_pday(ng), 'pmaxDON_pday',            &
     &        'max growth rate for bacteria (d^-1).'
            WRITE (out,80) pcoefO2_pday(ng), 'pcoefO2_pday',            &
     &        'max growth coeff for bact and aerobic growth',           &
     &        '(s^-1/(mmol oxygen m^-3) s d^-1).'
            WRITE (out,70) pmaxDIN_pday(ng), 'pmaxDIN_pday',            &
     &        'max growth rate for bacteria (d^-1).'
#if ! defined DARWIN_RANDOM_TRAITS
            WRITE (out,70) ksatPOM(ng), 'ksatPOM',                      &
     &        'ksatPON base value (mmol N m^-3).'
            WRITE (out,70) ksatDOM(ng), 'ksatDOM',                      &
     &        'ksatDON base value (mmol N m^-3).'
#endif
            WRITE (out,70) ksatDIN(ng), 'ksatDIN',                      &
     &        'DIN half-saturation concentration (mmol N m-3).'
            WRITE (out,80) alpha_hydrol(ng), 'alpha_hydrol',            &
     &        'frac of POM that is hydrolized into DOM for bact',       &
     &        '(dimensionless).'
#if ! defined DARWIN_RANDOM_TRAITS
            WRITE (out,70) yod(ng), 'yod',                              &
     &        'yield value for aerobic bacteria (dimensionless).'
            WRITE (out,70) yoe(ng), 'yoe',                              &
     &        'yield02 value for aerobic bacteria (dimensionless).'
            WRITE (out,80) ynd(ng), 'ynd',                              &
     &        'yield value for denitrifying bacteria',                  &
     &        '(dimensionless).'
            WRITE (out,80) yne(ng), 'yne',                              &
     &        'yieldNO3 value for denitrifying bacteria',               &
     &        '(dimensionless).'
#endif
#if defined DARWIN_RADTRANS
            WRITE (out,81) gud_selectSolz(ng), 'gud_selectSolz',        &
     &        'how to compute solar zenith angle, 0: local noon, 1',    &
     &        '(unknown).'
            WRITE (out,70) gud_refract_water(ng), 'gud_refract_water',  &
     &        'refractive index of seawater (unknown).'
            WRITE (out,70) gud_rmud_max(ng), 'gud_rmud_max',            &
     &        'unknown description (unknown).'
            WRITE (out,71) gud_radtrans_kmax(ng), 'gud_radtrans_kmax',  &
     &        'unknown description (unknown).'
            WRITE (out,70) gud_part_size_P(ng), 'gud_part_size_P',      &
     &        'unknown description (mmol P per particle).'
            WRITE (out,120) 'gud_waveband_edges',                       &
     &        'unknown description (unknown).'
            DO is=1,nlam+1
              WRITE (out,140) gud_waveband_edges(is,ng), ' '
            END DO
            WRITE (out,120) 'gud_waveband_centers',                     &
     &        'representative wavelengths (unknown).'
            DO is=1,nlam
              WRITE (out,140) gud_waveband_centers(is,ng), ' '
            END DO
            WRITE (out,70) gud_radmodThresh(ng), 'gud_radmodThresh',    &
     &        'unknown description (unknown).'
            WRITE (out,70) gud_rmus(ng), 'gud_rmus',                    &
     &        'unknown description (unknown).'
            WRITE (out,70) gud_rmuu(ng), 'gud_rmuu',                    &
     &        'unknown description (unknown).'
            WRITE (out,70) gud_bbmin(ng), 'gud_bbmin',                  &
     &        'unknown description (unknown).'
            WRITE (out,70) gud_bbw(ng), 'gud_bbw',                      &
     &        'unknown description (unknown).'
            WRITE (out,70) gud_lambda_aCDOM(ng), 'gud_lambda_aCDOM',    &
     &        'unknown description (unknown).'
            WRITE (out,70) gud_Sdom(ng), 'gud_Sdom',                    &
     &        'unknown description (unknown).'
            WRITE (out,70) gud_aCDOM_fac(ng), 'gud_aCDOM_fac',          &
     &        'unknown description (unknown).'
            WRITE (out,72) gud_allomSpectra(ng), 'gud_allomSpectra',    &
     &        'unknown description (unknown).'
            WRITE (out,80) gud_aCarCell(ng), 'gud_aCarCell',            &
     &        'mg C per cell (from Montagnes et al 1994)',              &
     &        '(unknown).'
            WRITE (out,70) gud_bCarCell(ng), 'gud_bCarCell',            &
     &        'unknown description (unknown).'
            WRITE (out,70) gud_absorpSlope(ng), 'gud_absorpSlope',      &
     &        'slope for scaled absorption spectra (unknown).'
            WRITE (out,70) gud_bbbSlope(ng), 'gud_bbbSlope',            &
     &        'unknown description (unknown).'
            WRITE (out,120) 'gud_scatSwitchSizeLog',                    &
     &        'unknown description (unknown).'
            DO is=1,nlam
              WRITE (out,140) gud_scatSwitchSizeLog(is,ng), ' '
            END DO
            WRITE (out,120) 'gud_scatSlopeSmall',                       &
     &        'unknown description (unknown).'
            DO is=1,nlam
              WRITE (out,140) gud_scatSlopeSmall(is,ng), ' '
            END DO
            WRITE (out,120) 'gud_scatSlopeLarge',                       &
     &        'unknown description (unknown).'
            DO is=1,nlam
              WRITE (out,140) gud_scatSlopeLarge(is,ng), ' '
            END DO
#endif
#if defined DARWIN_CDOM
            WRITE (out,70) fracCDOM(ng), 'fracCDOM',                    &
     &        'organic P CDOM fraction (mmol CDOM/mmol P).'
            WRITE (out,70) CDOMdegrd_pday(ng), 'CDOMdegrd_pday',        &
     &        'CDOM degradation rate (d^-1).'
            WRITE (out,70) CDOMbleach_pday(ng), 'CDOMbleach_pday',      &
     &        'CDOM bleaching rate (d^-1).'
            WRITE (out,80) PARCDOM(ng), 'PARCDOM',                      &
     &        'light normalization coeff for CDOM bleaching',           &
     &        '(uEin/m2/s).'
            WRITE (out,70) R_NP_CDOM(ng), 'R_NP_CDOM',                  &
     &        'CDOM N:P ratio (mmol N/mmol P).'
            WRITE (out,70) R_FeP_CDOM(ng), 'R_FeP_CDOM',                &
     &        'CDOM Fe:P ratio (mmol Fe/mmol P).'
            WRITE (out,70) R_CP_CDOM(ng), 'R_CP_CDOM',                  &
     &        'CDOM C:P ratio (mmol C/mmol P).'
#endif
#if defined DARWIN_CDOM && defined DARWIN_RADTRANS
            WRITE (out,70) CDOMcoeff(ng), 'CDOMcoeff',                  &
     &        'unknown description (unknown).'
#endif
            WRITE (out,70) BioMin(ng), 'BioMin',                        &
     &        'MinVal used in the biological code (unknown).'
#if defined DARWIN_RANDOM_TRAITS
            WRITE (out,130) 'seed_phytoplankton',                       &
     &        'phytoplankton random number generation seed',            &
     &        '(dimensionless).'
            DO is=1,nChl
              WRITE (out,141) seed_phytoplankton(is,ng), TRIM(plankname(is))
            END DO
#endif
#if defined DARWIN_DEBUGVARS
            WRITE (out,71) darwin_debug_1di(ng), 'darwin_debug_1di',    &
     &        'Darwin 1D integer (dimensionless).'
            WRITE (out,120) 'darwin_debug_2df',                         &
     &        'Darwin 2D float (dimensionless).'
            DO is=1,nlam
              WRITE (out,140) darwin_debug_2df(is,ng), ' '
            END DO
#endif
#if ! defined DARWIN_RANDOM_TRAITS && defined DARWIN_PLANK_BUOYCTRL
            WRITE (out,120) 'grp_buoyctrl',                             &
     &        'allow group to adjust buoyancy (dimensionless).'
            DO is=1,nGroup
              WRITE (out,142) grp_buoyctrl(is,ng), TRIM(grp_names(is))
            END DO
#endif

