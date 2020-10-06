!
! created by create_darwin_headerfiles (written by J. Paul Mattern)
!

      integer, allocatable :: isPhoto(:,:)            ! dimensionless
      integer, allocatable :: bactType(:,:)           ! dimensionless
      integer, allocatable :: isAerobic(:,:)          ! dimensionless
      integer, allocatable :: isDenit(:,:)            ! dimensionless
      integer, allocatable :: hasSi(:,:)              ! dimensionless
      integer, allocatable :: hasPIC(:,:)             ! dimensionless
      integer, allocatable :: diazo(:,:)              ! dimensionless
      integer, allocatable :: useNH4(:,:)             ! dimensionless
      integer, allocatable :: useNO2(:,:)             ! dimensionless
      integer, allocatable :: useNO3(:,:)             ! dimensionless
      integer, allocatable :: combNO(:,:)             ! dimensionless
      real(r8), allocatable :: Xmin(:,:)              ! mmol C m-3
      real(r8), allocatable :: amminhib(:,:)          ! (mmol N/m3)-1
      real(r8), allocatable :: acclimtimescl(:,:)     ! s^-1
      real(r8), allocatable :: mort(:,:)              ! s^-1
      real(r8), allocatable :: mort2(:,:)             ! (mmol C m^-3)^-1 s^-1
      integer, allocatable :: tempMort(:,:)           ! dimensionless
      integer, allocatable :: tempMort2(:,:)          ! dimensionless
      real(r8), allocatable :: ExportFracMort(:,:)    ! dimensionless
      real(r8), allocatable :: ExportFracMort2(:,:)   ! dimensionless
      real(r8), allocatable :: ExportFrac(:,:)        ! dimensionless
      real(r8), allocatable :: phytoTempCoeff(:,:)    ! dimensionless
      real(r8), allocatable :: phytoTempExp1(:,:)     ! ln(degree)
      real(r8), allocatable :: phytoTempExp2(:,:)     ! (degree C)^-phytoDecayPower
      real(r8), allocatable :: phytoTempOptimum(:,:)  ! degree C
      real(r8), allocatable :: phytoDecayPower(:,:)   ! dimensionless
      real(r8), allocatable :: R_NC(:,:)              ! mmol N (mmol C)-1
      real(r8), allocatable :: R_PC(:,:)              ! mmol P (mmol C)-1
      real(r8), allocatable :: R_SiC(:,:)             ! mmol Si (mmol C)-1
      real(r8), allocatable :: R_FeC(:,:)             ! mmol Fe (mmol C)-1
      real(r8), allocatable :: R_ChlC(:,:)            ! mg Chl (mmol C)^-1
      real(r8), allocatable :: R_PICPOC(:,:)          ! mmol C (mmol C)-1
      real(r8), allocatable :: wsink(:,:)             ! m s^-1
      real(r8), allocatable :: wswim(:,:)             ! m s^-1
      real(r8), allocatable :: respiration(:,:)       ! s^-1
      real(r8), allocatable :: PCmax(:,:)             ! s-1
      real(r8), allocatable :: Qnmax(:,:)             ! mmol N (mmol C)^-1
      real(r8), allocatable :: Qnmin(:,:)             ! mmol N (mmol C)^-1
      real(r8), allocatable :: Qpmax(:,:)             ! mmol P (mmol C)^-1
      real(r8), allocatable :: Qpmin(:,:)             ! mmol P (mmol C)^-1
      real(r8), allocatable :: Qsimax(:,:)            ! mmol Si (mmol C)^-1
      real(r8), allocatable :: Qsimin(:,:)            ! mmol Si (mmol C)^-1
      real(r8), allocatable :: Qfemax(:,:)            ! mmol Fe (mmol C)^-1
      real(r8), allocatable :: Qfemin(:,:)            ! mmol Fe (mmol C)^-1
      real(r8), allocatable :: Vmax_NH4(:,:)          ! mmol N (mmol C)^-1 s^-1
      real(r8), allocatable :: Vmax_NO2(:,:)          ! mmol N (mmol C)^-1 s^-1
      real(r8), allocatable :: Vmax_NO3(:,:)          ! mmol N (mmol C)^-1 s^-1
      real(r8), allocatable :: Vmax_N(:,:)            ! mmol N (mmol C)^-1 s^-1
      real(r8), allocatable :: Vmax_PO4(:,:)          ! mmol P (mmol C)^-1 s^-1
      real(r8), allocatable :: Vmax_SiO2(:,:)         ! mmol Si (mmol C)^-1 s^-1
      real(r8), allocatable :: Vmax_FeT(:,:)          ! mmol Fe (mmol C)^-1 s^-1
      real(r8), allocatable :: ksatNH4(:,:)           ! mmol N m-3
      real(r8), allocatable :: ksatNO2(:,:)           ! mmol N m-3
      real(r8), allocatable :: ksatNO3(:,:)           ! mmol N m-3
      real(r8), allocatable :: ksatPO4(:,:)           ! mmol P m-3
      real(r8), allocatable :: ksatSiO2(:,:)          ! mmol Si m-3
      real(r8), allocatable :: ksatFeT(:,:)           ! mmol Fe m-3
      real(r8), allocatable :: kexcC(:,:)             ! s^-1
      real(r8), allocatable :: kexcN(:,:)             ! s^-1
      real(r8), allocatable :: kexcP(:,:)             ! s^-1
      real(r8), allocatable :: kexcSi(:,:)            ! s^-1
      real(r8), allocatable :: kexcFe(:,:)            ! s^-1
#if defined DARWIN_GEIDER
      real(r8), allocatable :: inhibcoef_geid(:,:)    ! dimensionless
#endif
#if ! defined DARWIN_GEIDER
      real(r8), allocatable :: ksatPAR(:,:)           ! (uEin m-2 s-1)-1
      real(r8), allocatable :: kinhPAR(:,:)           ! (uEin m-2 s-1)-1
#endif
      real(r8), allocatable :: mQyield(:,:)           ! mmol C (uEin)^-1
      real(r8), allocatable :: chl2cmax(:,:)          ! mg Chl (mmol C)^-1
      real(r8), allocatable :: grazemax(:,:)          ! s-1
      real(r8), allocatable :: kgrazesat(:,:)         ! mmol C m-3
      real(r8), allocatable :: palat(:,:,:)           ! dimensionless
      real(r8), allocatable :: asseff(:,:,:)          ! dimensionless
      real(r8), allocatable :: ExportFracPreyPred(:,:,:) ! dimensionless
      real(r8), allocatable :: yield(:,:)             ! dimensionless
      real(r8), allocatable :: yieldO2(:,:)           ! dimensionless
      real(r8), allocatable :: yieldNO3(:,:)          ! dimensionless
      real(r8), allocatable :: ksatPON(:,:)           ! mmol N m-3
      real(r8), allocatable :: ksatPOC(:,:)           ! mmol C m-3
      real(r8), allocatable :: ksatPOP(:,:)           ! mmol P m-3
      real(r8), allocatable :: ksatPOFe(:,:)          ! mmol Fe m-3
      real(r8), allocatable :: ksatDON(:,:)           ! mmol N m-3
      real(r8), allocatable :: ksatDOC(:,:)           ! mmol C m-3
      real(r8), allocatable :: ksatDOP(:,:)           ! mmol P m-3
      real(r8), allocatable :: ksatDOFe(:,:)          ! mmol Fe m-3
#if defined DARWIN_RADTRANS
      real(r8), allocatable :: aphy_chl(:,:,:)        ! m^-1 (mg Chl m^-3)^-1
      real(r8), allocatable :: aphy_chl_ps(:,:,:)     ! m^-1 (mg Chl m^-3)^-1
      real(r8), allocatable :: bphy_mgC(:,:,:)        ! m^-1 (mg C m^-3)^-1
      real(r8), allocatable :: bbphy_mgC(:,:,:)       ! m^-1 (mg C m^-3)^-1
#endif
#if ! defined DARWIN_GEIDER
      real(r8), allocatable :: normI(:,:)             ! dimensionless
#endif
#if defined DARWIN_RADTRANS
      integer, allocatable :: ap_type(:,:)            ! dimensionless
#endif
#if ! defined DARWIN_RANDOM_TRAITS
      real(r8), allocatable :: biovol(:,:)            ! um^3
#endif
      integer, allocatable :: group(:,:)              ! dimensionless
#if ! defined DARWIN_RANDOM_TRAITS
      real(r8), allocatable :: biovol_bygroup(:,:,:)  ! um^3
#endif
      real(r8), allocatable :: alphachl(:,:,:)        ! mmol C s-1 (uEin m^-2 s^-1)^-1 (mg Chl)^-1
      real(r8), allocatable :: alpha_mean(:,:)        ! mmol C s-1 (uEin m^-2 s^-1)^-1 (mg Chl)^-1
      real(r8), allocatable :: chl2cmin(:,:)          ! mg Chl (mmol C)^-1
      real(r8), allocatable :: mortTempFuncMin(:,:)   ! dimensionless
      real(r8), allocatable :: mort2TempFuncMin(:,:)  ! dimensionless
#if defined DARWIN_RANDOM_TRAITS
      real(r8), allocatable :: Smallgrow(:)           ! d^-1
      real(r8), allocatable :: Biggrow(:)             ! d^-1
      real(r8), allocatable :: Smallgrowrange(:)      ! d^-1
      real(r8), allocatable :: Biggrowrange(:)        ! d^-1
      real(r8), allocatable :: diaz_growfac(:)        ! dimensionless
      real(r8), allocatable :: cocco_growfac(:)       ! dimensionless
      real(r8), allocatable :: diatom_growfac(:)      ! dimensionless
      real(r8), allocatable :: Smallmort(:)           ! d^-1
      real(r8), allocatable :: Bigmort(:)             ! d^-1
      real(r8), allocatable :: Smallmortrange(:)      ! d^-1
      real(r8), allocatable :: Bigmortrange(:)        ! d^-1
      real(r8), allocatable :: Smallexport(:)         ! dimensionless
      real(r8), allocatable :: Bigexport(:)           ! dimensionless
      real(r8), allocatable :: tempcoeff1(:)          ! unknown
      real(r8), allocatable :: tempcoeff2_small(:)    ! unknown
      real(r8), allocatable :: tempcoeff2_big(:)      ! unknown
      real(r8), allocatable :: tempcoeff3(:)          ! unknown
#endif
#if defined DARWIN_RANDOM_TRAITS && defined DARWIN_TEMP_RANGE
      real(r8), allocatable :: tempmax(:)             ! unknown
      real(r8), allocatable :: temprange(:)           ! unknown
      real(r8), allocatable :: tempdecay(:)           ! unknown
#endif
#if defined DARWIN_RANDOM_TRAITS
      real(r8), allocatable :: val_R_NC(:)            ! unknown
      real(r8), allocatable :: val_R_NC_diaz(:)       ! unknown
      real(r8), allocatable :: val_R_PC(:)            ! unknown
      real(r8), allocatable :: val_R_SiC_diatom(:)    ! unknown
      real(r8), allocatable :: val_R_FeC(:)           ! unknown
      real(r8), allocatable :: val_R_FeC_diaz(:)      ! unknown
      real(r8), allocatable :: val_R_PICPOC(:)        ! unknown
#endif
#if defined DARWIN_RANDOM_TRAITS && ! defined DARWIN_GEIDER
      real(r8), allocatable :: val_R_ChlC(:)          ! unknown
#endif
#if defined DARWIN_RANDOM_TRAITS
      real(r8), allocatable :: val_R_NC_zoo(:)        ! unknown
      real(r8), allocatable :: val_R_PC_zoo(:)        ! unknown
      real(r8), allocatable :: val_R_SiC_zoo(:)       ! unknown
      real(r8), allocatable :: val_R_FeC_zoo(:)       ! unknown
      real(r8), allocatable :: val_R_PICPOC_zoo(:)    ! unknown
      real(r8), allocatable :: val_R_ChlC_zoo(:)      ! unknown
      real(r8), allocatable :: SmallSink(:)           ! m s^-1
      real(r8), allocatable :: SmallSink_pday(:)      ! m d^-1
      real(r8), allocatable :: BigSink(:)             ! m s^-1
      real(r8), allocatable :: BigSink_pday(:)        ! m d^-1
      real(r8), allocatable :: SmallPsat(:)           ! unknown
      real(r8), allocatable :: BigPsat(:)             ! unknown
      real(r8), allocatable :: ProcPsat(:)            ! unknown
      real(r8), allocatable :: UniDzPsat(:)           ! unknown
      real(r8), allocatable :: CoccoPsat(:)           ! unknown
      real(r8), allocatable :: SmallPsatrange(:)      ! unknown
      real(r8), allocatable :: BigPsatrange(:)        ! unknown
      real(r8), allocatable :: ProcPsatrange(:)       ! unknown
      real(r8), allocatable :: UniDzPsatrange(:)      ! unknown
      real(r8), allocatable :: CoccoPsatrange(:)      ! unknown
      real(r8), allocatable :: ksatNH4fac(:)          ! unknown
      real(r8), allocatable :: ksatNO2fac(:)          ! unknown
      real(r8), allocatable :: val_amminhib(:)        ! coefficient for NH4 inhibition of NO uptake ((mmol N/m3)-1)
      real(r8), allocatable :: val_ksatsio2(:)        ! unknown
#endif
#if defined DARWIN_RANDOM_TRAITS && ! defined DARWIN_GEIDER
      real(r8), allocatable :: smallksatpar(:)        ! unknown
      real(r8), allocatable :: smallksatparstd(:)     ! unknown
      real(r8), allocatable :: smallkinhpar(:)        ! unknown
      real(r8), allocatable :: smallkinhparstd(:)     ! unknown
      real(r8), allocatable :: Bigksatpar(:)          ! unknown
      real(r8), allocatable :: Bigksatparstd(:)       ! unknown
      real(r8), allocatable :: Bigkinhpar(:)          ! unknown
      real(r8), allocatable :: Bigkinhparstd(:)       ! unknown
#endif
#if defined DARWIN_RANDOM_TRAITS && defined DARWIN_NINE_SPECIES_SETUP
      real(r8), allocatable :: LLProkinhpar(:)        ! unknown
      real(r8), allocatable :: Coccokinhpar(:)        ! unknown
#endif
#if defined DARWIN_RANDOM_TRAITS
      real(r8), allocatable :: inhibcoef_geid_val(:)  ! unknown
      real(r8), allocatable :: smallmQyield(:)        ! mmol C (uEin)-1
      real(r8), allocatable :: smallmQyieldrange(:)   ! mmol C (uEin)-1
      real(r8), allocatable :: BigmQyield(:)          ! mmol C (uEin)-1
      real(r8), allocatable :: BigmQyieldrange(:)     ! mmol C (uEin)-1
      real(r8), allocatable :: smallchl2cmax(:)       ! mg Chl (mmol C)
      real(r8), allocatable :: smallchl2cmaxrange(:)  ! mg Chl (mmol C)
      real(r8), allocatable :: Bigchl2cmax(:)         ! mg Chl (mmol C)
      real(r8), allocatable :: Bigchl2cmaxrange(:)    ! mg Chl (mmol C)
#endif
#if ! defined DARWIN_RADTRANS
      real(r8), allocatable :: aphy_chl_ave(:)        ! m^2 (mg_chl)^-1
#endif
#if defined DARWIN_RANDOM_TRAITS
      real(r8), allocatable :: val_acclimtimescl(:)   ! unknown
      logical, allocatable :: oldTwoGrazers(:)        ! unknown
      real(r8), allocatable :: GrazeFast(:)           ! s^-1
      real(r8), allocatable :: GrazeFast_pday(:)      ! d^-1
      real(r8), allocatable :: ZooexfacSmall(:)       ! unknown
      real(r8), allocatable :: ZooexfacBig(:)         ! unknown
      real(r8), allocatable :: ZoomortSmall(:)        ! s^-1
      real(r8), allocatable :: ZoomortSmall_pday(:)   ! d^-1
      real(r8), allocatable :: ZoomortBig(:)          ! s^-1
      real(r8), allocatable :: ZoomortBig_pday(:)     ! d^-1
      real(r8), allocatable :: ZoomortSmall2(:)       ! (mmol C m^-3)^-1 s^-1
      real(r8), allocatable :: ZoomortBig2(:)         ! (mmol C m^-3)^-1 s^-1
      real(r8), allocatable :: ExGrazfracbig(:)       ! unknown
      real(r8), allocatable :: ExGrazfracsmall(:)     ! unknown
      real(r8), allocatable :: palathi(:)             ! unknown
      real(r8), allocatable :: palatlo(:)             ! unknown
      real(r8), allocatable :: diatomgraz(:)          ! unknown
      real(r8), allocatable :: coccograz(:)           ! unknown
      real(r8), allocatable :: olargegraz(:)          ! unknown
      real(r8), allocatable :: GrazeEfflow(:)         ! unknown
      real(r8), allocatable :: GrazeEffmod(:)         ! unknown
      real(r8), allocatable :: GrazeEffhi(:)          ! unknown
      real(r8), allocatable :: GrazeRate(:)           ! s^-1
      real(r8), allocatable :: GrazeRate_pday(:)      ! d^-1
      real(r8), allocatable :: ExGrazfrac(:)          ! unknown
      real(r8), allocatable :: val_palat(:)           ! unknown
      real(r8), allocatable :: val_ass_eff(:)         ! unknown
      real(r8), allocatable :: kgrazesat_val(:)       ! = 0.1 mmol P m-3
      real(r8), allocatable :: Zoomort(:)             ! s^-1
      real(r8), allocatable :: Zoomort_pday(:)        ! d^-1
      real(r8), allocatable :: Zoomort2(:)            ! (mmol C m^-3)^-1 s^-1
      real(r8), allocatable :: Zooexfac(:)            ! unknown
      real(r8), allocatable :: ZooDM(:)               ! unknown
#endif
#if ! defined DARWIN_RANDOM_TRAITS
      logical, allocatable :: gud_sort_biovol(:)      ! dimensionless
#endif
      logical, allocatable :: GUD_effective_ksat(:)   ! dimensionless
      integer, allocatable :: gud_select_kn_allom(:)  ! dimensionless
      real(r8), allocatable :: logvolbase(:)          ! dimensionless
#if ! defined DARWIN_RANDOM_TRAITS
      real(r8), allocatable :: logvolinc(:)           ! dimensionless
#endif
      real(r8), allocatable :: biovol0(:,:)           ! dimensionless
      real(r8), allocatable :: biovolfac(:,:)         ! dimensionless
#if ! defined DARWIN_RANDOM_TRAITS
      integer, allocatable :: logvol0ind(:,:)         ! dimensionless
      real(r8), allocatable :: grp_logvolind(:,:,:)   ! dimensionless
      real(r8), allocatable :: grp_biovol(:,:,:)      ! dimensionless
      integer, allocatable :: grp_nplank(:,:)         ! dimensionless
      integer, allocatable :: grp_photo(:,:)          ! dimensionless
      integer, allocatable :: grp_bacttype(:,:)       ! dimensionless
      integer, allocatable :: grp_aerobic(:,:)        ! dimensionless
      integer, allocatable :: grp_denit(:,:)          ! dimensionless
      integer, allocatable :: grp_pred(:,:)           ! dimensionless
      integer, allocatable :: grp_prey(:,:)           ! dimensionless
      integer, allocatable :: grp_hasSi(:,:)          ! dimensionless
      integer, allocatable :: grp_hasPIC(:,:)         ! dimensionless
      integer, allocatable :: grp_diazo(:,:)          ! dimensionless
      integer, allocatable :: grp_useNH4(:,:)         ! dimensionless
      integer, allocatable :: grp_useNO2(:,:)         ! dimensionless
      integer, allocatable :: grp_useNO3(:,:)         ! dimensionless
      integer, allocatable :: grp_combNO(:,:)         ! dimensionless
#endif
#if defined DARWIN_RADTRANS && ! defined DARWIN_RANDOM_TRAITS
      integer, allocatable :: grp_aptype(:,:)         ! dimensionless
#endif
#if ! defined DARWIN_RANDOM_TRAITS
      integer, allocatable :: grp_tempMort(:,:)       ! dimensionless
      integer, allocatable :: grp_tempMort2(:,:)      ! dimensionless
      real(r8), allocatable :: grp_Xmin(:,:)          ! mmol C m^-3
      real(r8), allocatable :: grp_R_NC(:,:)          ! mmol N (mmol C)^-1
      real(r8), allocatable :: grp_R_PC(:,:)          ! mmol P (mmol C)^-1
      real(r8), allocatable :: grp_R_SiC(:,:)         ! mmol Si (mmol C)^-1
      real(r8), allocatable :: grp_R_FeC(:,:)         ! mmol Fe (mmol C)^-1
      real(r8), allocatable :: grp_R_ChlC(:,:)        ! mg Chl (mmol C)^-1
      real(r8), allocatable :: grp_R_PICPOC(:,:)      ! dimensionless
      real(r8), allocatable :: grp_ExportFracMort(:,:) ! dimensionless
      real(r8), allocatable :: grp_ExportFracMort2(:,:) ! dimensionless
      real(r8), allocatable :: grp_ExportFrac(:,:)    ! dimensionless
      real(r8), allocatable :: grp_mort(:,:)          ! s^-1
      real(r8), allocatable :: grp_mort_pday(:,:)     ! d^-1
      real(r8), allocatable :: grp_mort2(:,:)         ! (mmol C m^-3)^-1 s^-1
      real(r8), allocatable :: grp_tempcoeff1(:,:)    ! dimensionless
      real(r8), allocatable :: grp_tempcoeff2(:,:)    ! dimensionless
      real(r8), allocatable :: grp_tempcoeff3(:,:)    ! dimensionless
      real(r8), allocatable :: grp_tempopt(:,:)       ! deg_C
      real(r8), allocatable :: grp_tempdecay(:,:)     ! dimensionless
#endif
#if defined DARWIN_ALLOMETRIC_PALAT && ! defined DARWIN_RANDOM_TRAITS
      real(r8), allocatable :: grp_pp_sig(:,:)        ! dimensionless
#endif
#if ! defined DARWIN_RANDOM_TRAITS && defined DARWIN_GEIDER
      real(r8), allocatable :: grp_mQyield(:,:)       ! mmol C (uEin)^-1
      real(r8), allocatable :: grp_chl2cmax(:,:)      ! mg Chl (mmol C)^-1
      real(r8), allocatable :: grp_inhibcoef_geid(:,:) ! dimensionless
#endif
#if ! defined DARWIN_RANDOM_TRAITS && ! defined DARWIN_GEIDER
      real(r8), allocatable :: grp_ksatPAR(:,:)       ! (uEin m^-2 s^-1)^-1
      real(r8), allocatable :: grp_kinhPAR(:,:)       ! (uEin m^-2 s^-1)^-1
#endif
#if ! defined DARWIN_RANDOM_TRAITS
      real(r8), allocatable :: grp_ksatNH4fac(:,:)    ! dimensionless
      real(r8), allocatable :: grp_ksatNO2fac(:,:)    ! dimensionless
      real(r8), allocatable :: grp_amminhib(:,:)      ! (mmol N/m^3)^-1
      real(r8), allocatable :: grp_acclimtimescl(:,:) ! s^-1
      real(r8), allocatable :: grp_acclimtimescl_pday(:,:) ! d^-1
      real(r8), allocatable :: a_graz(:,:)            ! s^-1 um^-3
      real(r8), allocatable :: a_graz_pday(:,:)       ! d^-1 um^-3
      real(r8), allocatable :: b_graz(:,:)            ! dimensionless
      real(r8), allocatable :: a_kg(:,:)              ! mmol C m^-3 um^-3
      real(r8), allocatable :: b_kg(:,:)              ! dimensionless
      real(r8), allocatable :: a_biosink(:,:)         ! m s^-1 um^-3
      real(r8), allocatable :: a_biosink_pday(:,:)    ! m d^-1 um^-3
      real(r8), allocatable :: b_biosink(:,:)         ! dimensionless
      real(r8), allocatable :: a_bioswim(:,:)         ! m s^-1 um^-3
      real(r8), allocatable :: a_bioswim_pday(:,:)    ! m d^-1 um^-3
      real(r8), allocatable :: b_bioswim(:,:)         ! dimensionless
#endif
#if defined DARWIN_ALLOMETRIC_PALAT && ! defined DARWIN_RANDOM_TRAITS
      real(r8), allocatable :: a_prdpry(:,:)          ! um^-3
      real(r8), allocatable :: b_prdpry(:,:)          ! dimensionless
#endif
#if ! defined DARWIN_RANDOM_TRAITS
      real(r8), allocatable :: a_vmax_DIC(:,:)        ! s^-1 um^-3
      real(r8), allocatable :: a_vmax_DIC_pday(:,:)   ! d^-1 um^-3
      real(r8), allocatable :: b_vmax_DIC(:,:)        ! dimensionless
      real(r8), allocatable :: a_qcarbon(:,:)         ! um^-3
      real(r8), allocatable :: b_qcarbon(:,:)         ! dimensionless
      real(r8), allocatable :: a_respir(:,:)          ! mmol C cell^-1 s^-1
      real(r8), allocatable :: b_respir(:,:)          ! dimensionless
      real(r8), allocatable :: a_kexc_c(:,:)          ! s^-1 um^-3
      real(r8), allocatable :: b_kexc_c(:,:)          ! dimensionless
      real(r8), allocatable :: a_vmax_NO3(:,:)        ! mmol N (mmol C)^-1 s^-1 um^-3
      real(r8), allocatable :: a_vmax_NO3_pday(:,:)   ! mmol N (mmol C)^-1 d^-1 um^-3
      real(r8), allocatable :: b_vmax_NO3(:,:)        ! dimensionless
      real(r8), allocatable :: a_kn_NO3(:,:)          ! mmol N m-3 um^-3
      real(r8), allocatable :: b_kn_NO3(:,:)          ! dimensionless
      real(r8), allocatable :: a_qmin_n(:,:)          ! mmol N (mmol C)^-1 um^-3
      real(r8), allocatable :: b_qmin_n(:,:)          ! dimensionless
      real(r8), allocatable :: a_qmax_n(:,:)          ! mmol N (mmol C)^-1 um^-3
      real(r8), allocatable :: b_qmax_n(:,:)          ! dimensionless
      real(r8), allocatable :: a_kexc_n(:,:)          ! s^-1 um^-3
      real(r8), allocatable :: b_kexc_n(:,:)          ! dimensionless
      real(r8), allocatable :: a_vmax_NO2(:,:)        ! mmol N (mmol C)^-1 s^-1 um^-3
      real(r8), allocatable :: a_vmax_NO2_pday(:,:)   ! mmol N (mmol C)^-1 d^-1 um^-3
      real(r8), allocatable :: b_vmax_NO2(:,:)        ! dimensionless
      real(r8), allocatable :: a_kn_NO2(:,:)          ! mmol N m-3 um^-3
      real(r8), allocatable :: b_kn_NO2(:,:)          ! dimensionless
      real(r8), allocatable :: a_vmax_NH4(:,:)        ! mmol N (mmol C)^-1 s^-1 um^-3
      real(r8), allocatable :: a_vmax_NH4_pday(:,:)   ! mmol N (mmol C)^-1 d^-1 um^-3
      real(r8), allocatable :: b_vmax_NH4(:,:)        ! dimensionless
      real(r8), allocatable :: a_kn_NH4(:,:)          ! mmol N m-3 um^-3
      real(r8), allocatable :: b_kn_NH4(:,:)          ! dimensionless
      real(r8), allocatable :: a_vmax_N(:,:)          ! mmol N (mmol C)^-1 s^-1 um^-3
      real(r8), allocatable :: a_vmax_N_pday(:,:)     ! mmol N (mmol C)^-1 d^-1 um^-3
      real(r8), allocatable :: b_vmax_N(:,:)          ! dimensionless
      real(r8), allocatable :: a_vmax_PO4(:,:)        ! mmol P (mmol C)^-1 s^-1 um^-3
      real(r8), allocatable :: a_vmax_PO4_pday(:,:)   ! mmol P (mmol C)^-1 d^-1 um^-3
      real(r8), allocatable :: b_vmax_PO4(:,:)        ! dimensionless
      real(r8), allocatable :: a_kn_PO4(:,:)          ! mmol P m-3 um^-3
      real(r8), allocatable :: b_kn_PO4(:,:)          ! dimensionless
      real(r8), allocatable :: a_qmin_p(:,:)          ! mmol P (mmol C)^-1 um^-3
      real(r8), allocatable :: b_qmin_p(:,:)          ! dimensionless
      real(r8), allocatable :: a_qmax_p(:,:)          ! mmol P (mmol C)^-1 um^-3
      real(r8), allocatable :: b_qmax_p(:,:)          ! dimensionless
      real(r8), allocatable :: a_kexc_p(:,:)          ! s^-1 um^-3
      real(r8), allocatable :: a_kexc_p_pday(:,:)     ! d^-1 um^-3
      real(r8), allocatable :: b_kexc_p(:,:)          ! dimensionless
      real(r8), allocatable :: a_vmax_SiO2(:,:)       ! mmol Si (mmol C)^-1 s^-1 um^-3
      real(r8), allocatable :: a_vmax_SiO2_pday(:,:)  ! mmol Si (mmol C)^-1 d^-1 um^-3
      real(r8), allocatable :: b_vmax_SiO2(:,:)       ! dimensionless
      real(r8), allocatable :: a_kn_SiO2(:,:)         ! mmol Si m-3 um^-3
      real(r8), allocatable :: b_kn_SiO2(:,:)         ! dimensionless
      real(r8), allocatable :: a_qmin_si(:,:)         ! mmol Si (mmol C)^-1 um^-3
      real(r8), allocatable :: b_qmin_si(:,:)         ! dimensionless
      real(r8), allocatable :: a_qmax_si(:,:)         ! mmol Si (mmol C)^-1 um^-3
      real(r8), allocatable :: b_qmax_si(:,:)         ! dimensionless
      real(r8), allocatable :: a_kexc_si(:,:)         ! s^-1 um^-3
      real(r8), allocatable :: a_kexc_si_pday(:,:)    ! d^-1 um^-3
      real(r8), allocatable :: b_kexc_si(:,:)         ! dimensionless
      real(r8), allocatable :: a_vmax_FeT(:,:)        ! mmol Fe (mmol C)^-1 s^-1 um^-3
      real(r8), allocatable :: a_vmax_FeT_pday(:,:)   ! mmol Fe (mmol C)^-1 d^-1 um^-3
      real(r8), allocatable :: b_vmax_FeT(:,:)        ! dimensionless
      real(r8), allocatable :: a_kn_feT(:,:)          ! mmol Fe m-3 um^-3
      real(r8), allocatable :: b_kn_FeT(:,:)          ! dimensionless
      real(r8), allocatable :: a_qmin_fe(:,:)         ! mmol Fe (mmol C)^-1 um^-3
      real(r8), allocatable :: b_qmin_fe(:,:)         ! dimensionless
      real(r8), allocatable :: a_qmax_fe(:,:)         ! mmol Fe (mmol C)^-1 um^-3
      real(r8), allocatable :: b_qmax_fe(:,:)         ! dimensionless
      real(r8), allocatable :: a_kexc_fe(:,:)         ! s^-1 um^-3
      real(r8), allocatable :: a_kexc_fe_pday(:,:)    ! d^-1 um^-3
      real(r8), allocatable :: b_kexc_fe(:,:)         ! dimensionless
      real(r8), allocatable :: grp_ExportFracPreyPred(:,:,:) ! dimensionless
      real(r8), allocatable :: grp_ass_eff(:,:,:)     ! dimensionless
#endif
#if defined DARWIN_USE_PLOAD
      real(r8), allocatable :: Pa2Atm(:)              ! Pa/atm
#endif
      real(r8), allocatable :: ptr2mol(:)             ! mol/mmol
      real(r8), allocatable :: sca1(:)                ! dimensionless
      real(r8), allocatable :: sca2(:)                ! deg_C^-1
      real(r8), allocatable :: sca3(:)                ! deg_C^-2
      real(r8), allocatable :: sca4(:)                ! deg_C^-3
      real(r8), allocatable :: sox1(:)                ! dimensionless
      real(r8), allocatable :: sox2(:)                ! deg_C^-1
      real(r8), allocatable :: sox3(:)                ! deg_C^-2
      real(r8), allocatable :: sox4(:)                ! deg_C^-3
      real(r8), allocatable :: oA0(:)                 ! dimensionless
      real(r8), allocatable :: oA1(:)                 ! dimensionless
      real(r8), allocatable :: oA2(:)                 ! dimensionless
      real(r8), allocatable :: oA3(:)                 ! dimensionless
      real(r8), allocatable :: oA4(:)                 ! dimensionless
      real(r8), allocatable :: oA5(:)                 ! dimensionless
      real(r8), allocatable :: oB0(:)                 ! dimensionless
      real(r8), allocatable :: oB1(:)                 ! dimensionless
      real(r8), allocatable :: oB2(:)                 ! dimensionless
      real(r8), allocatable :: oB3(:)                 ! dimensionless
      real(r8), allocatable :: oC0(:)                 ! dimensionless
#if defined DARWIN_RANDOM_TRAITS
      real(r8), allocatable :: phymin(:)              ! unknown
#endif
      real(r8), allocatable :: katten_w(:)            ! m-1
      real(r8), allocatable :: katten_chl(:)          ! (mmol chl/m3)-1
      real(r8), allocatable :: parfrac(:)             ! dimensionless
#if DARWIN_TEMP_VERSION == 1
      real(r8), allocatable :: tempnorm(:)            ! dimensionless
#endif
#if DARWIN_TEMP_VERSION == 2
      real(r8), allocatable :: TempAeArr(:)           ! dimensionless
      real(r8), allocatable :: TemprefArr(:)          ! dimensionless
      real(r8), allocatable :: TempCoeffArr(:)        ! dimensionless
#endif
      real(r8), allocatable :: alpfe(:)               ! m s^-1
#if ! defined DARWIN_PART_SCAV_POP && ! defined DARWIN_PART_SCAV
      real(r8), allocatable :: scav(:)                ! s^-1
      real(r8), allocatable :: scav_pyear(:)          ! year^-1
#endif
      real(r8), allocatable :: ligand_tot(:)          ! mmol m^-3
      real(r8), allocatable :: ligand_stab(:)         ! m^3 mol^-1
#if defined DARWIN_MINFE
      real(r8), allocatable :: freefemax(:)           ! mmol Fe m^-3
#endif
#if defined DARWIN_PART_SCAV_POP || defined DARWIN_PART_SCAV
      real(r8), allocatable :: scav_rat(:)            ! s^-1
      real(r8), allocatable :: scav_rat_pday(:)       ! d^-1
      real(r8), allocatable :: scav_inter(:)          ! dimensionless
      real(r8), allocatable :: scav_exp(:)            ! dimensionless
#endif
#if defined DARWIN_PART_SCAV_POP
      real(r8), allocatable :: scav_R_POPPOC(:)       ! mmol P/mmol C
#endif
#if ! defined DARWIN_IRON_SED_SOURCE_VARIABLE
      real(r8), allocatable :: fesedflux(:)           ! mmol Fe m^-2 s^-1
      real(r8), allocatable :: fesedflux_pday(:)      ! mmol Fe m^-2 d^-1
#endif
#if defined DARWIN_IRON_SED_SOURCE_VARIABLE
      real(r8), allocatable :: fesedflux_pcm(:)       ! mmol Fe/mmol C
      real(r8), allocatable :: R_CP_fesed(:)          ! mmol C/mmol P
#endif
      real(r8), allocatable :: Knita(:)               ! s-1
      real(r8), allocatable :: Knita_pday(:)          ! d^-1
      real(r8), allocatable :: Knitb(:)               ! s-1
      real(r8), allocatable :: Knitb_pday(:)          ! d^-1
      real(r8), allocatable :: PAR_oxi(:)             ! uEin/m2/s
      real(r8), allocatable :: Kdoc(:)                ! s^-1
      real(r8), allocatable :: Kdoc_pday(:)           ! d^-1
      real(r8), allocatable :: Kdop(:)                ! s^-1
      real(r8), allocatable :: Kdop_pday(:)           ! d^-1
      real(r8), allocatable :: Kdon(:)                ! s^-1
      real(r8), allocatable :: Kdon_pday(:)           ! d^-1
      real(r8), allocatable :: KdoFe(:)               ! s^-1
      real(r8), allocatable :: KdoFe_pday(:)          ! d^-1
      real(r8), allocatable :: KPOC(:)                ! s^-1
      real(r8), allocatable :: KPOC_pday(:)           ! d^-1
      real(r8), allocatable :: KPOP(:)                ! s^-1
      real(r8), allocatable :: KPOP_pday(:)           ! d^-1
      real(r8), allocatable :: KPON(:)                ! s^-1
      real(r8), allocatable :: KPON_pday(:)           ! d^-1
      real(r8), allocatable :: KPOFe(:)               ! s^-1
      real(r8), allocatable :: KPOFe_pday(:)          ! d^-1
      real(r8), allocatable :: KPOSi(:)               ! s^-1
      real(r8), allocatable :: KPOSi_pday(:)          ! d^-1
      real(r8), allocatable :: wC_sink(:)             ! m/s
      real(r8), allocatable :: wC_sink_pday(:)        ! m/s s d^-1
      real(r8), allocatable :: wP_sink(:)             ! s-1
      real(r8), allocatable :: wP_sink_pday(:)        ! d^-1
      real(r8), allocatable :: wN_sink(:)             ! s-1
      real(r8), allocatable :: wN_sink_pday(:)        ! d^-1
      real(r8), allocatable :: wFe_sink(:)            ! s-1
      real(r8), allocatable :: wFe_sink_pday(:)       ! d^-1
      real(r8), allocatable :: wSi_sink(:)            ! s-1
      real(r8), allocatable :: wSi_sink_pday(:)       ! d^-1
      real(r8), allocatable :: wPIC_sink(:)           ! m/s
      real(r8), allocatable :: wPIC_sink_pday(:)      ! m/s s d^-1
      real(r8), allocatable :: Kdissc(:)              ! s^-1
      real(r8), allocatable :: Kdissc_pday(:)         ! d^-1
#if defined DARWIN_CARBON
      real(r8), allocatable :: gud_atmos_pCO2(:)      ! dimensionless
      real(r8), allocatable :: R_OP(:)                ! mmol O (mmol P)-1
      real(r8), allocatable :: m3perkg(:)             ! m^3/kg
      real(r8), allocatable :: surfSiMinInit(:)       ! mmol Si m^-3
      real(r8), allocatable :: surfSaltMin(:)         ! dimensionless
      real(r8), allocatable :: surfSaltMax(:)         ! dimensionless
      real(r8), allocatable :: surfTempMin(:)         ! deg C
      real(r8), allocatable :: surfTempMax(:)         ! deg C
      real(r8), allocatable :: surfDICMin(:)          ! mmol C m^-3
      real(r8), allocatable :: surfDICMax(:)          ! mmol C m^-3
      real(r8), allocatable :: surfALKMin(:)          ! mmol C m^-3
      real(r8), allocatable :: surfALKMax(:)          ! mmol C m^-3
      real(r8), allocatable :: surfPO4Min(:)          ! mmol P m^-3
      real(r8), allocatable :: surfPO4Max(:)          ! mmol P m^-3
      real(r8), allocatable :: surfSiMax(:)           ! mmol Si m^-3
      real(r8), allocatable :: O2crit(:)              ! mmol O
#endif
#if defined DARWIN_DENIT
      real(r8), allocatable :: denit_NP(:)            ! mmol N/(mmol P)
      real(r8), allocatable :: denit_NO3(:)           ! dimensionless
      real(r8), allocatable :: NO3crit(:)             ! mmol N m^-3
#endif
      real(r8), allocatable :: PARmin(:)              ! uEin/m2/s
#if defined DARWIN_GEIDER && defined DARWIN_CHLQUOTA && defined DARWIN_NQUOTA
      real(r8), allocatable :: chl2nmax(:)            ! mg chl/mmol N
      real(r8), allocatable :: synthcost(:)           ! mmol C/mmol N
#endif
#if defined DARWIN_ALLOMETRIC_PALAT && ! defined DARWIN_RANDOM_TRAITS
      real(r8), allocatable :: palat_min(:)           ! dimensionless
#endif
      real(r8), allocatable :: inhib_graz(:)          ! (mmol C m-3)-1
      real(r8), allocatable :: inhib_graz_exp(:)      ! dimensionless
      real(r8), allocatable :: hillnum(:)             ! dimensionless
      real(r8), allocatable :: hollexp(:)             ! dimensionless
      real(r8), allocatable :: phygrazmin(:)          ! mmol C m-1
      real(r8), allocatable :: pmaxPON(:)             ! s^-1
      real(r8), allocatable :: pmaxPON_pday(:)        ! d^-1
      real(r8), allocatable :: pmaxDON(:)             ! s^-1
      real(r8), allocatable :: pmaxDON_pday(:)        ! d^-1
      real(r8), allocatable :: pcoefO2(:)             ! s^-1/(mmol oxygen m^-3)
      real(r8), allocatable :: pcoefO2_pday(:)        ! s^-1/(mmol oxygen m^-3) s d^-1
      real(r8), allocatable :: pmaxDIN(:)             ! s^-1
      real(r8), allocatable :: pmaxDIN_pday(:)        ! d^-1
#if ! defined DARWIN_RANDOM_TRAITS
      real(r8), allocatable :: ksatPOM(:)             ! mmol N m^-3
      real(r8), allocatable :: ksatDOM(:)             ! mmol N m^-3
#endif
      real(r8), allocatable :: ksatDIN(:)             ! mmol N m-3
      real(r8), allocatable :: alpha_hydrol(:)        ! dimensionless
#if ! defined DARWIN_RANDOM_TRAITS
      real(r8), allocatable :: yod(:)                 ! dimensionless
      real(r8), allocatable :: yoe(:)                 ! dimensionless
      real(r8), allocatable :: ynd(:)                 ! dimensionless
      real(r8), allocatable :: yne(:)                 ! dimensionless
#endif
#if defined DARWIN_RADTRANS
      integer, allocatable :: gud_selectSolz(:)       ! unknown
      real(r8), allocatable :: gud_refract_water(:)   ! unknown
      real(r8), allocatable :: gud_rmud_max(:)        ! unknown
      integer, allocatable :: gud_radtrans_kmax(:)    ! unknown
      real(r8), allocatable :: gud_part_size_P(:)     ! mmol P per particle
      real(r8), allocatable :: gud_waveband_edges(:,:) ! unknown
      real(r8), allocatable :: gud_waveband_centers(:,:) ! unknown
      real(r8), allocatable :: gud_radmodThresh(:)    ! unknown
      real(r8), allocatable :: gud_rmus(:)            ! unknown
      real(r8), allocatable :: gud_rmuu(:)            ! unknown
      real(r8), allocatable :: gud_bbmin(:)           ! unknown
      real(r8), allocatable :: gud_bbw(:)             ! unknown
      real(r8), allocatable :: gud_lambda_aCDOM(:)    ! unknown
      real(r8), allocatable :: gud_Sdom(:)            ! unknown
      real(r8), allocatable :: gud_aCDOM_fac(:)       ! unknown
      logical, allocatable :: gud_allomSpectra(:)     ! unknown
      real(r8), allocatable :: gud_aCarCell(:)        ! unknown
      real(r8), allocatable :: gud_bCarCell(:)        ! unknown
      real(r8), allocatable :: gud_absorpSlope(:)     ! unknown
      real(r8), allocatable :: gud_bbbSlope(:)        ! unknown
      real(r8), allocatable :: gud_scatSwitchSizeLog(:,:) ! unknown
      real(r8), allocatable :: gud_scatSlopeSmall(:,:) ! unknown
      real(r8), allocatable :: gud_scatSlopeLarge(:,:) ! unknown
#endif
#if defined DARWIN_CDOM
      real(r8), allocatable :: fracCDOM(:)            ! mmol CDOM/mmol P
      real(r8), allocatable :: CDOMdegrd(:)           ! s^-1
      real(r8), allocatable :: CDOMdegrd_pday(:)      ! d^-1
      real(r8), allocatable :: CDOMbleach(:)          ! s^-1
      real(r8), allocatable :: CDOMbleach_pday(:)     ! d^-1
      real(r8), allocatable :: PARCDOM(:)             ! uEin/m2/s
      real(r8), allocatable :: R_NP_CDOM(:)           ! mmol N/mmol P
      real(r8), allocatable :: R_FeP_CDOM(:)          ! mmol Fe/mmol P
      real(r8), allocatable :: R_CP_CDOM(:)           ! mmol C/mmol P
#endif
#if defined DARWIN_CDOM && defined DARWIN_RADTRANS
      real(r8), allocatable :: CDOMcoeff(:)           ! unknown
#endif
      real(r8), allocatable :: BioMin(:)              ! unknown
#if defined DARWIN_RANDOM_TRAITS
      integer, allocatable :: seed_phytoplankton(:,:) ! dimensionless
#endif
#if defined DARWIN_DEBUGVARS
      integer, allocatable :: darwin_debug_1di(:)     ! dimensionless
      real(r8), allocatable :: darwin_debug_2df(:,:)  ! dimensionless
#endif
#if ! defined DARWIN_RANDOM_TRAITS && defined DARWIN_PLANK_BUOYCTRL
      logical, allocatable :: grp_buoyctrl(:,:)       ! dimensionless
#endif
#if defined DARWIN_PLANK_BUOYCTRL
      logical, allocatable :: buoyctrl(:,:)           ! dimensionless
#endif
