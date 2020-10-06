!
! created by create_darwin_headerfiles (written by J. Paul Mattern)
!

#if defined DARWIN_RADTRANS
            CASE ('aphy_chl')
              Npts=load_r(Nval, Rval, nplank*nlam*Ngrids, aphy_chl)
            CASE ('aphy_chl_ps')
              Npts=load_r(Nval, Rval, nplank*nlam*Ngrids, aphy_chl_ps)
            CASE ('bphy_mgC')
              Npts=load_r(Nval, Rval, nplank*nlam*Ngrids, bphy_mgC)
            CASE ('bbphy_mgC')
              Npts=load_r(Nval, Rval, nplank*nlam*Ngrids, bbphy_mgC)
#endif
#if defined DARWIN_RANDOM_TRAITS
            CASE ('Smallgrow')
              Npts=load_r(Nval, Rval, Ngrids, Smallgrow)
            CASE ('Biggrow')
              Npts=load_r(Nval, Rval, Ngrids, Biggrow)
            CASE ('Smallgrowrange')
              Npts=load_r(Nval, Rval, Ngrids, Smallgrowrange)
            CASE ('Biggrowrange')
              Npts=load_r(Nval, Rval, Ngrids, Biggrowrange)
            CASE ('diaz_growfac')
              Npts=load_r(Nval, Rval, Ngrids, diaz_growfac)
            CASE ('cocco_growfac')
              Npts=load_r(Nval, Rval, Ngrids, cocco_growfac)
            CASE ('diatom_growfac')
              Npts=load_r(Nval, Rval, Ngrids, diatom_growfac)
            CASE ('Smallmort')
              Npts=load_r(Nval, Rval, Ngrids, Smallmort)
            CASE ('Bigmort')
              Npts=load_r(Nval, Rval, Ngrids, Bigmort)
            CASE ('Smallmortrange')
              Npts=load_r(Nval, Rval, Ngrids, Smallmortrange)
            CASE ('Bigmortrange')
              Npts=load_r(Nval, Rval, Ngrids, Bigmortrange)
            CASE ('Smallexport')
              Npts=load_r(Nval, Rval, Ngrids, Smallexport)
            CASE ('Bigexport')
              Npts=load_r(Nval, Rval, Ngrids, Bigexport)
            CASE ('tempcoeff1')
              Npts=load_r(Nval, Rval, Ngrids, tempcoeff1)
            CASE ('tempcoeff2_small')
              Npts=load_r(Nval, Rval, Ngrids, tempcoeff2_small)
            CASE ('tempcoeff2_big')
              Npts=load_r(Nval, Rval, Ngrids, tempcoeff2_big)
            CASE ('tempcoeff3')
              Npts=load_r(Nval, Rval, Ngrids, tempcoeff3)
#endif
#if defined DARWIN_RANDOM_TRAITS && defined DARWIN_TEMP_RANGE
            CASE ('tempmax')
              Npts=load_r(Nval, Rval, Ngrids, tempmax)
            CASE ('temprange')
              Npts=load_r(Nval, Rval, Ngrids, temprange)
            CASE ('tempdecay')
              Npts=load_r(Nval, Rval, Ngrids, tempdecay)
#endif
#if defined DARWIN_RANDOM_TRAITS
            CASE ('val_R_NC')
              Npts=load_r(Nval, Rval, Ngrids, val_R_NC)
            CASE ('val_R_NC_diaz')
              Npts=load_r(Nval, Rval, Ngrids, val_R_NC_diaz)
            CASE ('val_R_PC')
              Npts=load_r(Nval, Rval, Ngrids, val_R_PC)
            CASE ('val_R_SiC_diatom')
              Npts=load_r(Nval, Rval, Ngrids, val_R_SiC_diatom)
            CASE ('val_R_FeC')
              Npts=load_r(Nval, Rval, Ngrids, val_R_FeC)
            CASE ('val_R_FeC_diaz')
              Npts=load_r(Nval, Rval, Ngrids, val_R_FeC_diaz)
            CASE ('val_R_PICPOC')
              Npts=load_r(Nval, Rval, Ngrids, val_R_PICPOC)
#endif
#if defined DARWIN_RANDOM_TRAITS && ! defined DARWIN_GEIDER
            CASE ('val_R_ChlC')
              Npts=load_r(Nval, Rval, Ngrids, val_R_ChlC)
#endif
#if defined DARWIN_RANDOM_TRAITS
            CASE ('val_R_NC_zoo')
              Npts=load_r(Nval, Rval, Ngrids, val_R_NC_zoo)
            CASE ('val_R_PC_zoo')
              Npts=load_r(Nval, Rval, Ngrids, val_R_PC_zoo)
            CASE ('val_R_SiC_zoo')
              Npts=load_r(Nval, Rval, Ngrids, val_R_SiC_zoo)
            CASE ('val_R_FeC_zoo')
              Npts=load_r(Nval, Rval, Ngrids, val_R_FeC_zoo)
            CASE ('val_R_PICPOC_zoo')
              Npts=load_r(Nval, Rval, Ngrids, val_R_PICPOC_zoo)
            CASE ('val_R_ChlC_zoo')
              Npts=load_r(Nval, Rval, Ngrids, val_R_ChlC_zoo)
            CASE ('SmallSink_pday')
              Npts=load_r(Nval, Rval, Ngrids, SmallSink_pday)
              DO ng=1,Ngrids
                SmallSink(ng)=SmallSink_pday(ng)*sec2day
              END DO
            CASE ('BigSink_pday')
              Npts=load_r(Nval, Rval, Ngrids, BigSink_pday)
              DO ng=1,Ngrids
                BigSink(ng)=BigSink_pday(ng)*sec2day
              END DO
            CASE ('SmallPsat')
              Npts=load_r(Nval, Rval, Ngrids, SmallPsat)
            CASE ('BigPsat')
              Npts=load_r(Nval, Rval, Ngrids, BigPsat)
            CASE ('ProcPsat')
              Npts=load_r(Nval, Rval, Ngrids, ProcPsat)
            CASE ('UniDzPsat')
              Npts=load_r(Nval, Rval, Ngrids, UniDzPsat)
            CASE ('CoccoPsat')
              Npts=load_r(Nval, Rval, Ngrids, CoccoPsat)
            CASE ('SmallPsatrange')
              Npts=load_r(Nval, Rval, Ngrids, SmallPsatrange)
            CASE ('BigPsatrange')
              Npts=load_r(Nval, Rval, Ngrids, BigPsatrange)
            CASE ('ProcPsatrange')
              Npts=load_r(Nval, Rval, Ngrids, ProcPsatrange)
            CASE ('UniDzPsatrange')
              Npts=load_r(Nval, Rval, Ngrids, UniDzPsatrange)
            CASE ('CoccoPsatrange')
              Npts=load_r(Nval, Rval, Ngrids, CoccoPsatrange)
            CASE ('ksatNH4fac')
              Npts=load_r(Nval, Rval, Ngrids, ksatNH4fac)
            CASE ('ksatNO2fac')
              Npts=load_r(Nval, Rval, Ngrids, ksatNO2fac)
            CASE ('val_amminhib')
              Npts=load_r(Nval, Rval, Ngrids, val_amminhib)
            CASE ('val_ksatsio2')
              Npts=load_r(Nval, Rval, Ngrids, val_ksatsio2)
#endif
#if defined DARWIN_RANDOM_TRAITS && ! defined DARWIN_GEIDER
            CASE ('smallksatpar')
              Npts=load_r(Nval, Rval, Ngrids, smallksatpar)
            CASE ('smallksatparstd')
              Npts=load_r(Nval, Rval, Ngrids, smallksatparstd)
            CASE ('smallkinhpar')
              Npts=load_r(Nval, Rval, Ngrids, smallkinhpar)
            CASE ('smallkinhparstd')
              Npts=load_r(Nval, Rval, Ngrids, smallkinhparstd)
            CASE ('Bigksatpar')
              Npts=load_r(Nval, Rval, Ngrids, Bigksatpar)
            CASE ('Bigksatparstd')
              Npts=load_r(Nval, Rval, Ngrids, Bigksatparstd)
            CASE ('Bigkinhpar')
              Npts=load_r(Nval, Rval, Ngrids, Bigkinhpar)
            CASE ('Bigkinhparstd')
              Npts=load_r(Nval, Rval, Ngrids, Bigkinhparstd)
#endif
#if defined DARWIN_RANDOM_TRAITS && defined DARWIN_NINE_SPECIES_SETUP
            CASE ('LLProkinhpar')
              Npts=load_r(Nval, Rval, Ngrids, LLProkinhpar)
            CASE ('Coccokinhpar')
              Npts=load_r(Nval, Rval, Ngrids, Coccokinhpar)
#endif
#if defined DARWIN_RANDOM_TRAITS
            CASE ('inhibcoef_geid_val')
              Npts=load_r(Nval, Rval, Ngrids, inhibcoef_geid_val)
            CASE ('smallmQyield')
              Npts=load_r(Nval, Rval, Ngrids, smallmQyield)
            CASE ('smallmQyieldrange')
              Npts=load_r(Nval, Rval, Ngrids, smallmQyieldrange)
            CASE ('BigmQyield')
              Npts=load_r(Nval, Rval, Ngrids, BigmQyield)
            CASE ('BigmQyieldrange')
              Npts=load_r(Nval, Rval, Ngrids, BigmQyieldrange)
            CASE ('smallchl2cmax')
              Npts=load_r(Nval, Rval, Ngrids, smallchl2cmax)
            CASE ('smallchl2cmaxrange')
              Npts=load_r(Nval, Rval, Ngrids, smallchl2cmaxrange)
            CASE ('Bigchl2cmax')
              Npts=load_r(Nval, Rval, Ngrids, Bigchl2cmax)
            CASE ('Bigchl2cmaxrange')
              Npts=load_r(Nval, Rval, Ngrids, Bigchl2cmaxrange)
#endif
#if ! defined DARWIN_RADTRANS
            CASE ('aphy_chl_ave')
              Npts=load_r(Nval, Rval, Ngrids, aphy_chl_ave)
#endif
#if defined DARWIN_RANDOM_TRAITS
            CASE ('val_acclimtimescl')
              Npts=load_r(Nval, Rval, Ngrids, val_acclimtimescl)
            CASE ('oldTwoGrazers')
              Npts=load_l(Nval, Cval, Ngrids, oldTwoGrazers)
            CASE ('GrazeFast_pday')
              Npts=load_r(Nval, Rval, Ngrids, GrazeFast_pday)
              DO ng=1,Ngrids
                GrazeFast(ng)=GrazeFast_pday(ng)*sec2day
              END DO
            CASE ('ZooexfacSmall')
              Npts=load_r(Nval, Rval, Ngrids, ZooexfacSmall)
            CASE ('ZooexfacBig')
              Npts=load_r(Nval, Rval, Ngrids, ZooexfacBig)
            CASE ('ZoomortSmall_pday')
              Npts=load_r(Nval, Rval, Ngrids, ZoomortSmall_pday)
              DO ng=1,Ngrids
                ZoomortSmall(ng)=ZoomortSmall_pday(ng)*sec2day
              END DO
            CASE ('ZoomortBig_pday')
              Npts=load_r(Nval, Rval, Ngrids, ZoomortBig_pday)
              DO ng=1,Ngrids
                ZoomortBig(ng)=ZoomortBig_pday(ng)*sec2day
              END DO
            CASE ('ZoomortSmall2')
              Npts=load_r(Nval, Rval, Ngrids, ZoomortSmall2)
            CASE ('ZoomortBig2')
              Npts=load_r(Nval, Rval, Ngrids, ZoomortBig2)
            CASE ('ExGrazfracbig')
              Npts=load_r(Nval, Rval, Ngrids, ExGrazfracbig)
            CASE ('ExGrazfracsmall')
              Npts=load_r(Nval, Rval, Ngrids, ExGrazfracsmall)
            CASE ('palathi')
              Npts=load_r(Nval, Rval, Ngrids, palathi)
            CASE ('palatlo')
              Npts=load_r(Nval, Rval, Ngrids, palatlo)
            CASE ('diatomgraz')
              Npts=load_r(Nval, Rval, Ngrids, diatomgraz)
            CASE ('coccograz')
              Npts=load_r(Nval, Rval, Ngrids, coccograz)
            CASE ('olargegraz')
              Npts=load_r(Nval, Rval, Ngrids, olargegraz)
            CASE ('GrazeEfflow')
              Npts=load_r(Nval, Rval, Ngrids, GrazeEfflow)
            CASE ('GrazeEffmod')
              Npts=load_r(Nval, Rval, Ngrids, GrazeEffmod)
            CASE ('GrazeEffhi')
              Npts=load_r(Nval, Rval, Ngrids, GrazeEffhi)
            CASE ('GrazeRate_pday')
              Npts=load_r(Nval, Rval, Ngrids, GrazeRate_pday)
              DO ng=1,Ngrids
                GrazeRate(ng)=GrazeRate_pday(ng)*sec2day
              END DO
            CASE ('ExGrazfrac')
              Npts=load_r(Nval, Rval, Ngrids, ExGrazfrac)
            CASE ('val_palat')
              Npts=load_r(Nval, Rval, Ngrids, val_palat)
            CASE ('val_ass_eff')
              Npts=load_r(Nval, Rval, Ngrids, val_ass_eff)
            CASE ('kgrazesat_val')
              Npts=load_r(Nval, Rval, Ngrids, kgrazesat_val)
            CASE ('Zoomort_pday')
              Npts=load_r(Nval, Rval, Ngrids, Zoomort_pday)
              DO ng=1,Ngrids
                Zoomort(ng)=Zoomort_pday(ng)*sec2day
              END DO
            CASE ('Zoomort2')
              Npts=load_r(Nval, Rval, Ngrids, Zoomort2)
            CASE ('Zooexfac')
              Npts=load_r(Nval, Rval, Ngrids, Zooexfac)
            CASE ('ZooDM')
              Npts=load_r(Nval, Rval, Ngrids, ZooDM)
#endif
#if ! defined DARWIN_RANDOM_TRAITS
            CASE ('gud_sort_biovol')
              Npts=load_l(Nval, Cval, Ngrids, gud_sort_biovol)
#endif
            CASE ('GUD_effective_ksat')
              Npts=load_l(Nval, Cval, Ngrids, GUD_effective_ksat)
            CASE ('gud_select_kn_allom')
              Npts=load_i(Nval, Rval, Ngrids, gud_select_kn_allom)
            CASE ('logvolbase')
              Npts=load_r(Nval, Rval, Ngrids, logvolbase)
#if ! defined DARWIN_RANDOM_TRAITS
            CASE ('logvolinc')
              Npts=load_r(Nval, Rval, Ngrids, logvolinc)
#endif
            CASE ('biovol0')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, biovol0)
            CASE ('biovolfac')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, biovolfac)
#if ! defined DARWIN_RANDOM_TRAITS
            CASE ('logvol0ind')
              Npts=load_i(Nval, Rval, nGroup*Ngrids, logvol0ind)
            CASE ('grp_logvolind')
              Npts=load_r(Nval, Rval, nPlank*nGroup*Ngrids, grp_logvolind)
            CASE ('grp_biovol')
              Npts=load_r(Nval, Rval, nPlank*nGroup*Ngrids, grp_biovol)
            CASE ('grp_nplank')
              Npts=load_i(Nval, Rval, nGroup*Ngrids, grp_nplank)
            CASE ('grp_photo')
              Npts=load_i(Nval, Rval, nGroup*Ngrids, grp_photo)
            CASE ('grp_bacttype')
              Npts=load_i(Nval, Rval, nGroup*Ngrids, grp_bacttype)
            CASE ('grp_aerobic')
              Npts=load_i(Nval, Rval, nGroup*Ngrids, grp_aerobic)
            CASE ('grp_denit')
              Npts=load_i(Nval, Rval, nGroup*Ngrids, grp_denit)
            CASE ('grp_pred')
              Npts=load_i(Nval, Rval, nGroup*Ngrids, grp_pred)
            CASE ('grp_prey')
              Npts=load_i(Nval, Rval, nGroup*Ngrids, grp_prey)
            CASE ('grp_hasSi')
              Npts=load_i(Nval, Rval, nGroup*Ngrids, grp_hasSi)
            CASE ('grp_hasPIC')
              Npts=load_i(Nval, Rval, nGroup*Ngrids, grp_hasPIC)
            CASE ('grp_diazo')
              Npts=load_i(Nval, Rval, nGroup*Ngrids, grp_diazo)
            CASE ('grp_useNH4')
              Npts=load_i(Nval, Rval, nGroup*Ngrids, grp_useNH4)
            CASE ('grp_useNO2')
              Npts=load_i(Nval, Rval, nGroup*Ngrids, grp_useNO2)
            CASE ('grp_useNO3')
              Npts=load_i(Nval, Rval, nGroup*Ngrids, grp_useNO3)
            CASE ('grp_combNO')
              Npts=load_i(Nval, Rval, nGroup*Ngrids, grp_combNO)
#endif
#if defined DARWIN_RADTRANS && ! defined DARWIN_RANDOM_TRAITS
            CASE ('grp_aptype')
              Npts=load_i(Nval, Rval, nGroup*Ngrids, grp_aptype)
#endif
#if ! defined DARWIN_RANDOM_TRAITS
            CASE ('grp_tempMort')
              Npts=load_i(Nval, Rval, nGroup*Ngrids, grp_tempMort)
            CASE ('grp_tempMort2')
              Npts=load_i(Nval, Rval, nGroup*Ngrids, grp_tempMort2)
            CASE ('grp_Xmin')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, grp_Xmin)
            CASE ('grp_R_NC')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, grp_R_NC)
            CASE ('grp_R_PC')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, grp_R_PC)
            CASE ('grp_R_SiC')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, grp_R_SiC)
            CASE ('grp_R_FeC')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, grp_R_FeC)
            CASE ('grp_R_ChlC')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, grp_R_ChlC)
            CASE ('grp_R_PICPOC')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, grp_R_PICPOC)
            CASE ('grp_ExportFracMort')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, grp_ExportFracMort)
            CASE ('grp_ExportFracMort2')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, grp_ExportFracMort2)
            CASE ('grp_ExportFrac')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, grp_ExportFrac)
            CASE ('grp_mort_pday')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, grp_mort_pday)
              DO ig=1,nGroup
                DO ng=1,Ngrids
                  grp_mort(ig,ng)=grp_mort_pday(ig,ng)*sec2day
                END DO
              END DO
            CASE ('grp_mort2')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, grp_mort2)
            CASE ('grp_tempcoeff1')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, grp_tempcoeff1)
            CASE ('grp_tempcoeff2')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, grp_tempcoeff2)
            CASE ('grp_tempcoeff3')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, grp_tempcoeff3)
            CASE ('grp_tempopt')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, grp_tempopt)
            CASE ('grp_tempdecay')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, grp_tempdecay)
#endif
#if defined DARWIN_ALLOMETRIC_PALAT && ! defined DARWIN_RANDOM_TRAITS
            CASE ('grp_pp_sig')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, grp_pp_sig)
#endif
#if ! defined DARWIN_RANDOM_TRAITS && defined DARWIN_GEIDER
            CASE ('grp_mQyield')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, grp_mQyield)
            CASE ('grp_chl2cmax')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, grp_chl2cmax)
            CASE ('grp_inhibcoef_geid')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, grp_inhibcoef_geid)
#endif
#if ! defined DARWIN_RANDOM_TRAITS && ! defined DARWIN_GEIDER
            CASE ('grp_ksatPAR')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, grp_ksatPAR)
            CASE ('grp_kinhPAR')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, grp_kinhPAR)
#endif
#if ! defined DARWIN_RANDOM_TRAITS
            CASE ('grp_ksatNH4fac')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, grp_ksatNH4fac)
            CASE ('grp_ksatNO2fac')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, grp_ksatNO2fac)
            CASE ('grp_amminhib')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, grp_amminhib)
            CASE ('grp_acclimtimescl_pday')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, grp_acclimtimescl_pday)
              DO ig=1,nGroup
                DO ng=1,Ngrids
                  grp_acclimtimescl(ig,ng)=grp_acclimtimescl_pday(ig,ng)*sec2day
                END DO
              END DO
            CASE ('a_graz_pday')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, a_graz_pday)
              DO ig=1,nGroup
                DO ng=1,Ngrids
                  a_graz(ig,ng)=a_graz_pday(ig,ng)*sec2day
                END DO
              END DO
            CASE ('b_graz')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, b_graz)
            CASE ('a_kg')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, a_kg)
            CASE ('b_kg')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, b_kg)
            CASE ('a_biosink_pday')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, a_biosink_pday)
              DO ig=1,nGroup
                DO ng=1,Ngrids
                  a_biosink(ig,ng)=a_biosink_pday(ig,ng)*sec2day
                END DO
              END DO
            CASE ('b_biosink')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, b_biosink)
            CASE ('a_bioswim_pday')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, a_bioswim_pday)
              DO ig=1,nGroup
                DO ng=1,Ngrids
                  a_bioswim(ig,ng)=a_bioswim_pday(ig,ng)*sec2day
                END DO
              END DO
            CASE ('b_bioswim')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, b_bioswim)
#endif
#if defined DARWIN_ALLOMETRIC_PALAT && ! defined DARWIN_RANDOM_TRAITS
            CASE ('a_prdpry')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, a_prdpry)
            CASE ('b_prdpry')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, b_prdpry)
#endif
#if ! defined DARWIN_RANDOM_TRAITS
            CASE ('a_vmax_DIC_pday')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, a_vmax_DIC_pday)
              DO ig=1,nGroup
                DO ng=1,Ngrids
                  a_vmax_DIC(ig,ng)=a_vmax_DIC_pday(ig,ng)*sec2day
                END DO
              END DO
            CASE ('b_vmax_DIC')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, b_vmax_DIC)
            CASE ('a_qcarbon')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, a_qcarbon)
            CASE ('b_qcarbon')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, b_qcarbon)
            CASE ('a_respir')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, a_respir)
            CASE ('b_respir')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, b_respir)
            CASE ('a_kexc_c')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, a_kexc_c)
            CASE ('b_kexc_c')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, b_kexc_c)
            CASE ('a_vmax_NO3_pday')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, a_vmax_NO3_pday)
              DO ig=1,nGroup
                DO ng=1,Ngrids
                  a_vmax_NO3(ig,ng)=a_vmax_NO3_pday(ig,ng)*sec2day
                END DO
              END DO
            CASE ('b_vmax_NO3')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, b_vmax_NO3)
            CASE ('a_kn_NO3')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, a_kn_NO3)
            CASE ('b_kn_NO3')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, b_kn_NO3)
            CASE ('a_qmin_n')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, a_qmin_n)
            CASE ('b_qmin_n')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, b_qmin_n)
            CASE ('a_qmax_n')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, a_qmax_n)
            CASE ('b_qmax_n')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, b_qmax_n)
            CASE ('a_kexc_n')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, a_kexc_n)
            CASE ('b_kexc_n')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, b_kexc_n)
            CASE ('a_vmax_NO2_pday')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, a_vmax_NO2_pday)
              DO ig=1,nGroup
                DO ng=1,Ngrids
                  a_vmax_NO2(ig,ng)=a_vmax_NO2_pday(ig,ng)*sec2day
                END DO
              END DO
            CASE ('b_vmax_NO2')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, b_vmax_NO2)
            CASE ('a_kn_NO2')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, a_kn_NO2)
            CASE ('b_kn_NO2')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, b_kn_NO2)
            CASE ('a_vmax_NH4_pday')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, a_vmax_NH4_pday)
              DO ig=1,nGroup
                DO ng=1,Ngrids
                  a_vmax_NH4(ig,ng)=a_vmax_NH4_pday(ig,ng)*sec2day
                END DO
              END DO
            CASE ('b_vmax_NH4')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, b_vmax_NH4)
            CASE ('a_kn_NH4')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, a_kn_NH4)
            CASE ('b_kn_NH4')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, b_kn_NH4)
            CASE ('a_vmax_N_pday')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, a_vmax_N_pday)
              DO ig=1,nGroup
                DO ng=1,Ngrids
                  a_vmax_N(ig,ng)=a_vmax_N_pday(ig,ng)*sec2day
                END DO
              END DO
            CASE ('b_vmax_N')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, b_vmax_N)
            CASE ('a_vmax_PO4_pday')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, a_vmax_PO4_pday)
              DO ig=1,nGroup
                DO ng=1,Ngrids
                  a_vmax_PO4(ig,ng)=a_vmax_PO4_pday(ig,ng)*sec2day
                END DO
              END DO
            CASE ('b_vmax_PO4')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, b_vmax_PO4)
            CASE ('a_kn_PO4')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, a_kn_PO4)
            CASE ('b_kn_PO4')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, b_kn_PO4)
            CASE ('a_qmin_p')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, a_qmin_p)
            CASE ('b_qmin_p')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, b_qmin_p)
            CASE ('a_qmax_p')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, a_qmax_p)
            CASE ('b_qmax_p')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, b_qmax_p)
            CASE ('a_kexc_p_pday')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, a_kexc_p_pday)
              DO ig=1,nGroup
                DO ng=1,Ngrids
                  a_kexc_p(ig,ng)=a_kexc_p_pday(ig,ng)*sec2day
                END DO
              END DO
            CASE ('b_kexc_p')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, b_kexc_p)
            CASE ('a_vmax_SiO2_pday')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, a_vmax_SiO2_pday)
              DO ig=1,nGroup
                DO ng=1,Ngrids
                  a_vmax_SiO2(ig,ng)=a_vmax_SiO2_pday(ig,ng)*sec2day
                END DO
              END DO
            CASE ('b_vmax_SiO2')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, b_vmax_SiO2)
            CASE ('a_kn_SiO2')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, a_kn_SiO2)
            CASE ('b_kn_SiO2')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, b_kn_SiO2)
            CASE ('a_qmin_si')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, a_qmin_si)
            CASE ('b_qmin_si')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, b_qmin_si)
            CASE ('a_qmax_si')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, a_qmax_si)
            CASE ('b_qmax_si')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, b_qmax_si)
            CASE ('a_kexc_si_pday')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, a_kexc_si_pday)
              DO ig=1,nGroup
                DO ng=1,Ngrids
                  a_kexc_si(ig,ng)=a_kexc_si_pday(ig,ng)*sec2day
                END DO
              END DO
            CASE ('b_kexc_si')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, b_kexc_si)
            CASE ('a_vmax_FeT_pday')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, a_vmax_FeT_pday)
              DO ig=1,nGroup
                DO ng=1,Ngrids
                  a_vmax_FeT(ig,ng)=a_vmax_FeT_pday(ig,ng)*sec2day
                END DO
              END DO
            CASE ('b_vmax_FeT')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, b_vmax_FeT)
            CASE ('a_kn_feT')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, a_kn_feT)
            CASE ('b_kn_FeT')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, b_kn_FeT)
            CASE ('a_qmin_fe')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, a_qmin_fe)
            CASE ('b_qmin_fe')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, b_qmin_fe)
            CASE ('a_qmax_fe')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, a_qmax_fe)
            CASE ('b_qmax_fe')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, b_qmax_fe)
            CASE ('a_kexc_fe_pday')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, a_kexc_fe_pday)
              DO ig=1,nGroup
                DO ng=1,Ngrids
                  a_kexc_fe(ig,ng)=a_kexc_fe_pday(ig,ng)*sec2day
                END DO
              END DO
            CASE ('b_kexc_fe')
              Npts=load_r(Nval, Rval, nGroup*Ngrids, b_kexc_fe)
            CASE ('grp_ExportFracPreyPred')
              Npts=load_r(Nval, Rval, nGroup*nGroup*Ngrids, grp_ExportFracPreyPred)
            CASE ('grp_ass_eff')
              Npts=load_r(Nval, Rval, nGroup*nGroup*Ngrids, grp_ass_eff)
#endif
#if defined DARWIN_RANDOM_TRAITS
            CASE ('phymin')
              Npts=load_r(Nval, Rval, Ngrids, phymin)
#endif
            CASE ('katten_w')
              Npts=load_r(Nval, Rval, Ngrids, katten_w)
            CASE ('katten_chl')
              Npts=load_r(Nval, Rval, Ngrids, katten_chl)
            CASE ('parfrac')
              Npts=load_r(Nval, Rval, Ngrids, parfrac)
#if DARWIN_TEMP_VERSION == 1
            CASE ('tempnorm')
              Npts=load_r(Nval, Rval, Ngrids, tempnorm)
#endif
            CASE ('alpfe')
              Npts=load_r(Nval, Rval, Ngrids, alpfe)
#if ! defined DARWIN_PART_SCAV_POP && ! defined DARWIN_PART_SCAV
            CASE ('scav_pyear')
              Npts=load_r(Nval, Rval, Ngrids, scav_pyear)
              DO ng=1,Ngrids
                scav(ng)=scav_pyear(ng)*sec2day/360.0_r8
              END DO
#endif
            CASE ('ligand_tot')
              Npts=load_r(Nval, Rval, Ngrids, ligand_tot)
            CASE ('ligand_stab')
              Npts=load_r(Nval, Rval, Ngrids, ligand_stab)
#if defined DARWIN_MINFE
            CASE ('freefemax')
              Npts=load_r(Nval, Rval, Ngrids, freefemax)
#endif
#if defined DARWIN_PART_SCAV_POP || defined DARWIN_PART_SCAV
            CASE ('scav_rat_pday')
              Npts=load_r(Nval, Rval, Ngrids, scav_rat_pday)
              DO ng=1,Ngrids
                scav_rat(ng)=scav_rat_pday(ng)*sec2day
              END DO
            CASE ('scav_inter')
              Npts=load_r(Nval, Rval, Ngrids, scav_inter)
            CASE ('scav_exp')
              Npts=load_r(Nval, Rval, Ngrids, scav_exp)
#endif
#if defined DARWIN_PART_SCAV_POP
            CASE ('scav_R_POPPOC')
              Npts=load_r(Nval, Rval, Ngrids, scav_R_POPPOC)
#endif
#if ! defined DARWIN_IRON_SED_SOURCE_VARIABLE
            CASE ('fesedflux_pday')
              Npts=load_r(Nval, Rval, Ngrids, fesedflux_pday)
              DO ng=1,Ngrids
                fesedflux(ng)=fesedflux_pday(ng)*sec2day
              END DO
#endif
#if defined DARWIN_IRON_SED_SOURCE_VARIABLE
            CASE ('fesedflux_pcm')
              Npts=load_r(Nval, Rval, Ngrids, fesedflux_pcm)
            CASE ('R_CP_fesed')
              Npts=load_r(Nval, Rval, Ngrids, R_CP_fesed)
#endif
            CASE ('Knita_pday')
              Npts=load_r(Nval, Rval, Ngrids, Knita_pday)
              DO ng=1,Ngrids
                Knita(ng)=Knita_pday(ng)*sec2day
              END DO
            CASE ('Knitb_pday')
              Npts=load_r(Nval, Rval, Ngrids, Knitb_pday)
              DO ng=1,Ngrids
                Knitb(ng)=Knitb_pday(ng)*sec2day
              END DO
            CASE ('PAR_oxi')
              Npts=load_r(Nval, Rval, Ngrids, PAR_oxi)
            CASE ('Kdoc_pday')
              Npts=load_r(Nval, Rval, Ngrids, Kdoc_pday)
              DO ng=1,Ngrids
                Kdoc(ng)=Kdoc_pday(ng)*sec2day
              END DO
            CASE ('Kdop_pday')
              Npts=load_r(Nval, Rval, Ngrids, Kdop_pday)
              DO ng=1,Ngrids
                Kdop(ng)=Kdop_pday(ng)*sec2day
              END DO
            CASE ('Kdon_pday')
              Npts=load_r(Nval, Rval, Ngrids, Kdon_pday)
              DO ng=1,Ngrids
                Kdon(ng)=Kdon_pday(ng)*sec2day
              END DO
            CASE ('KdoFe_pday')
              Npts=load_r(Nval, Rval, Ngrids, KdoFe_pday)
              DO ng=1,Ngrids
                KdoFe(ng)=KdoFe_pday(ng)*sec2day
              END DO
            CASE ('KPOC_pday')
              Npts=load_r(Nval, Rval, Ngrids, KPOC_pday)
              DO ng=1,Ngrids
                KPOC(ng)=KPOC_pday(ng)*sec2day
              END DO
            CASE ('KPOP_pday')
              Npts=load_r(Nval, Rval, Ngrids, KPOP_pday)
              DO ng=1,Ngrids
                KPOP(ng)=KPOP_pday(ng)*sec2day
              END DO
            CASE ('KPON_pday')
              Npts=load_r(Nval, Rval, Ngrids, KPON_pday)
              DO ng=1,Ngrids
                KPON(ng)=KPON_pday(ng)*sec2day
              END DO
            CASE ('KPOFe_pday')
              Npts=load_r(Nval, Rval, Ngrids, KPOFe_pday)
              DO ng=1,Ngrids
                KPOFe(ng)=KPOFe_pday(ng)*sec2day
              END DO
            CASE ('KPOSi_pday')
              Npts=load_r(Nval, Rval, Ngrids, KPOSi_pday)
              DO ng=1,Ngrids
                KPOSi(ng)=KPOSi_pday(ng)*sec2day
              END DO
            CASE ('wC_sink_pday')
              Npts=load_r(Nval, Rval, Ngrids, wC_sink_pday)
              DO ng=1,Ngrids
                wC_sink(ng)=wC_sink_pday(ng)*sec2day
              END DO
            CASE ('wP_sink_pday')
              Npts=load_r(Nval, Rval, Ngrids, wP_sink_pday)
              DO ng=1,Ngrids
                wP_sink(ng)=wP_sink_pday(ng)*sec2day
              END DO
            CASE ('wN_sink_pday')
              Npts=load_r(Nval, Rval, Ngrids, wN_sink_pday)
              DO ng=1,Ngrids
                wN_sink(ng)=wN_sink_pday(ng)*sec2day
              END DO
            CASE ('wFe_sink_pday')
              Npts=load_r(Nval, Rval, Ngrids, wFe_sink_pday)
              DO ng=1,Ngrids
                wFe_sink(ng)=wFe_sink_pday(ng)*sec2day
              END DO
            CASE ('wSi_sink_pday')
              Npts=load_r(Nval, Rval, Ngrids, wSi_sink_pday)
              DO ng=1,Ngrids
                wSi_sink(ng)=wSi_sink_pday(ng)*sec2day
              END DO
            CASE ('wPIC_sink_pday')
              Npts=load_r(Nval, Rval, Ngrids, wPIC_sink_pday)
              DO ng=1,Ngrids
                wPIC_sink(ng)=wPIC_sink_pday(ng)*sec2day
              END DO
            CASE ('Kdissc_pday')
              Npts=load_r(Nval, Rval, Ngrids, Kdissc_pday)
              DO ng=1,Ngrids
                Kdissc(ng)=Kdissc_pday(ng)*sec2day
              END DO
#if defined DARWIN_CARBON
            CASE ('gud_atmos_pCO2')
              Npts=load_r(Nval, Rval, Ngrids, gud_atmos_pCO2)
            CASE ('R_OP')
              Npts=load_r(Nval, Rval, Ngrids, R_OP)
            CASE ('m3perkg')
              Npts=load_r(Nval, Rval, Ngrids, m3perkg)
            CASE ('surfSiMinInit')
              Npts=load_r(Nval, Rval, Ngrids, surfSiMinInit)
            CASE ('surfSaltMin')
              Npts=load_r(Nval, Rval, Ngrids, surfSaltMin)
            CASE ('surfSaltMax')
              Npts=load_r(Nval, Rval, Ngrids, surfSaltMax)
            CASE ('surfTempMin')
              Npts=load_r(Nval, Rval, Ngrids, surfTempMin)
            CASE ('surfTempMax')
              Npts=load_r(Nval, Rval, Ngrids, surfTempMax)
            CASE ('surfDICMin')
              Npts=load_r(Nval, Rval, Ngrids, surfDICMin)
            CASE ('surfDICMax')
              Npts=load_r(Nval, Rval, Ngrids, surfDICMax)
            CASE ('surfALKMin')
              Npts=load_r(Nval, Rval, Ngrids, surfALKMin)
            CASE ('surfALKMax')
              Npts=load_r(Nval, Rval, Ngrids, surfALKMax)
            CASE ('surfPO4Min')
              Npts=load_r(Nval, Rval, Ngrids, surfPO4Min)
            CASE ('surfPO4Max')
              Npts=load_r(Nval, Rval, Ngrids, surfPO4Max)
            CASE ('surfSiMax')
              Npts=load_r(Nval, Rval, Ngrids, surfSiMax)
            CASE ('O2crit')
              Npts=load_r(Nval, Rval, Ngrids, O2crit)
#endif
#if defined DARWIN_DENIT
            CASE ('denit_NP')
              Npts=load_r(Nval, Rval, Ngrids, denit_NP)
            CASE ('denit_NO3')
              Npts=load_r(Nval, Rval, Ngrids, denit_NO3)
            CASE ('NO3crit')
              Npts=load_r(Nval, Rval, Ngrids, NO3crit)
#endif
            CASE ('PARmin')
              Npts=load_r(Nval, Rval, Ngrids, PARmin)
#if defined DARWIN_GEIDER && defined DARWIN_CHLQUOTA && defined DARWIN_NQUOTA
            CASE ('chl2nmax')
              Npts=load_r(Nval, Rval, Ngrids, chl2nmax)
            CASE ('synthcost')
              Npts=load_r(Nval, Rval, Ngrids, synthcost)
#endif
#if defined DARWIN_ALLOMETRIC_PALAT && ! defined DARWIN_RANDOM_TRAITS
            CASE ('palat_min')
              Npts=load_r(Nval, Rval, Ngrids, palat_min)
#endif
            CASE ('inhib_graz')
              Npts=load_r(Nval, Rval, Ngrids, inhib_graz)
            CASE ('inhib_graz_exp')
              Npts=load_r(Nval, Rval, Ngrids, inhib_graz_exp)
            CASE ('hillnum')
              Npts=load_r(Nval, Rval, Ngrids, hillnum)
            CASE ('hollexp')
              Npts=load_r(Nval, Rval, Ngrids, hollexp)
            CASE ('phygrazmin')
              Npts=load_r(Nval, Rval, Ngrids, phygrazmin)
            CASE ('pmaxPON_pday')
              Npts=load_r(Nval, Rval, Ngrids, pmaxPON_pday)
              DO ng=1,Ngrids
                pmaxPON(ng)=pmaxPON_pday(ng)*sec2day
              END DO
            CASE ('pmaxDON_pday')
              Npts=load_r(Nval, Rval, Ngrids, pmaxDON_pday)
              DO ng=1,Ngrids
                pmaxDON(ng)=pmaxDON_pday(ng)*sec2day
              END DO
            CASE ('pcoefO2_pday')
              Npts=load_r(Nval, Rval, Ngrids, pcoefO2_pday)
              DO ng=1,Ngrids
                pcoefO2(ng)=pcoefO2_pday(ng)*sec2day
              END DO
            CASE ('pmaxDIN_pday')
              Npts=load_r(Nval, Rval, Ngrids, pmaxDIN_pday)
              DO ng=1,Ngrids
                pmaxDIN(ng)=pmaxDIN_pday(ng)*sec2day
              END DO
#if ! defined DARWIN_RANDOM_TRAITS
            CASE ('ksatPOM')
              Npts=load_r(Nval, Rval, Ngrids, ksatPOM)
            CASE ('ksatDOM')
              Npts=load_r(Nval, Rval, Ngrids, ksatDOM)
#endif
            CASE ('ksatDIN')
              Npts=load_r(Nval, Rval, Ngrids, ksatDIN)
            CASE ('alpha_hydrol')
              Npts=load_r(Nval, Rval, Ngrids, alpha_hydrol)
#if ! defined DARWIN_RANDOM_TRAITS
            CASE ('yod')
              Npts=load_r(Nval, Rval, Ngrids, yod)
            CASE ('yoe')
              Npts=load_r(Nval, Rval, Ngrids, yoe)
            CASE ('ynd')
              Npts=load_r(Nval, Rval, Ngrids, ynd)
            CASE ('yne')
              Npts=load_r(Nval, Rval, Ngrids, yne)
#endif
#if defined DARWIN_CDOM
            CASE ('fracCDOM')
              Npts=load_r(Nval, Rval, Ngrids, fracCDOM)
            CASE ('CDOMdegrd_pday')
              Npts=load_r(Nval, Rval, Ngrids, CDOMdegrd_pday)
              DO ng=1,Ngrids
                CDOMdegrd(ng)=CDOMdegrd_pday(ng)*sec2day
              END DO
            CASE ('CDOMbleach_pday')
              Npts=load_r(Nval, Rval, Ngrids, CDOMbleach_pday)
              DO ng=1,Ngrids
                CDOMbleach(ng)=CDOMbleach_pday(ng)*sec2day
              END DO
            CASE ('PARCDOM')
              Npts=load_r(Nval, Rval, Ngrids, PARCDOM)
            CASE ('R_NP_CDOM')
              Npts=load_r(Nval, Rval, Ngrids, R_NP_CDOM)
            CASE ('R_FeP_CDOM')
              Npts=load_r(Nval, Rval, Ngrids, R_FeP_CDOM)
            CASE ('R_CP_CDOM')
              Npts=load_r(Nval, Rval, Ngrids, R_CP_CDOM)
#endif
#if defined DARWIN_CDOM && defined DARWIN_RADTRANS
            CASE ('CDOMcoeff')
              Npts=load_r(Nval, Rval, Ngrids, CDOMcoeff)
#endif
            CASE ('BioMin')
              Npts=load_r(Nval, Rval, Ngrids, BioMin)
#if defined DARWIN_RANDOM_TRAITS
            CASE ('seed_phytoplankton')
              Npts=load_i(Nval, Rval, nChl*Ngrids, seed_phytoplankton)
#endif
#if defined DARWIN_DEBUGVARS
            CASE ('darwin_debug_1di')
              Npts=load_i(Nval, Rval, Ngrids, darwin_debug_1di)
            CASE ('darwin_debug_2df')
              Npts=load_r(Nval, Rval, nlam*Ngrids, darwin_debug_2df)
#endif
#if ! defined DARWIN_RANDOM_TRAITS && defined DARWIN_PLANK_BUOYCTRL
            CASE ('grp_buoyctrl')
              Npts=load_l(Nval, Cval, nGroup*Ngrids, grp_buoyctrl)
#endif
