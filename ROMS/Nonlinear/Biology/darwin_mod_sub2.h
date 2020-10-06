!
! created by create_darwin_headerfiles (written by J. Paul Mattern)
!

      IF (.not.allocated(isPhoto)) THEN
        allocate ( isPhoto(nplank,Ngrids) )
      END IF
      isPhoto(:,:)=0
      IF (.not.allocated(bactType)) THEN
        allocate ( bactType(nplank,Ngrids) )
      END IF
      bactType(:,:)=0
      IF (.not.allocated(isAerobic)) THEN
        allocate ( isAerobic(nplank,Ngrids) )
      END IF
      isAerobic(:,:)=0
      IF (.not.allocated(isDenit)) THEN
        allocate ( isDenit(nplank,Ngrids) )
      END IF
      isDenit(:,:)=0
      IF (.not.allocated(hasSi)) THEN
        allocate ( hasSi(nplank,Ngrids) )
      END IF
      hasSi(:,:)=0
      IF (.not.allocated(hasPIC)) THEN
        allocate ( hasPIC(nplank,Ngrids) )
      END IF
      hasPIC(:,:)=0
      IF (.not.allocated(diazo)) THEN
        allocate ( diazo(nplank,Ngrids) )
      END IF
      diazo(:,:)=0
      IF (.not.allocated(useNH4)) THEN
        allocate ( useNH4(nplank,Ngrids) )
      END IF
      useNH4(:,:)=0
      IF (.not.allocated(useNO2)) THEN
        allocate ( useNO2(nplank,Ngrids) )
      END IF
      useNO2(:,:)=0
      IF (.not.allocated(useNO3)) THEN
        allocate ( useNO3(nplank,Ngrids) )
      END IF
      useNO3(:,:)=0
      IF (.not.allocated(combNO)) THEN
        allocate ( combNO(nplank,Ngrids) )
      END IF
      combNO(:,:)=0
      IF (.not.allocated(Xmin)) THEN
        allocate ( Xmin(nplank,Ngrids) )
      END IF
      Xmin(:,:)=0.0_r8
      IF (.not.allocated(amminhib)) THEN
        allocate ( amminhib(nplank,Ngrids) )
      END IF
      amminhib(:,:)=0.0_r8
      IF (.not.allocated(acclimtimescl)) THEN
        allocate ( acclimtimescl(nplank,Ngrids) )
      END IF
      acclimtimescl(:,:)=0.0_r8
      IF (.not.allocated(mort)) THEN
        allocate ( mort(nplank,Ngrids) )
      END IF
      mort(:,:)=0.0_r8
      IF (.not.allocated(mort2)) THEN
        allocate ( mort2(nplank,Ngrids) )
      END IF
      mort2(:,:)=0.0_r8
      IF (.not.allocated(tempMort)) THEN
        allocate ( tempMort(nplank,Ngrids) )
      END IF
      tempMort(:,:)=0
      IF (.not.allocated(tempMort2)) THEN
        allocate ( tempMort2(nplank,Ngrids) )
      END IF
      tempMort2(:,:)=0
      IF (.not.allocated(ExportFracMort)) THEN
        allocate ( ExportFracMort(nplank,Ngrids) )
      END IF
      ExportFracMort(:,:)=0.0_r8
      IF (.not.allocated(ExportFracMort2)) THEN
        allocate ( ExportFracMort2(nplank,Ngrids) )
      END IF
      ExportFracMort2(:,:)=0.0_r8
      IF (.not.allocated(ExportFrac)) THEN
        allocate ( ExportFrac(nplank,Ngrids) )
      END IF
      ExportFrac(:,:)=0.0_r8
      IF (.not.allocated(phytoTempCoeff)) THEN
        allocate ( phytoTempCoeff(nplank,Ngrids) )
      END IF
      phytoTempCoeff(:,:)=0.0_r8
      IF (.not.allocated(phytoTempExp1)) THEN
        allocate ( phytoTempExp1(nplank,Ngrids) )
      END IF
      phytoTempExp1(:,:)=0.0_r8
      IF (.not.allocated(phytoTempExp2)) THEN
        allocate ( phytoTempExp2(nplank,Ngrids) )
      END IF
      phytoTempExp2(:,:)=0.0_r8
      IF (.not.allocated(phytoTempOptimum)) THEN
        allocate ( phytoTempOptimum(nplank,Ngrids) )
      END IF
      phytoTempOptimum(:,:)=0.0_r8
      IF (.not.allocated(phytoDecayPower)) THEN
        allocate ( phytoDecayPower(nplank,Ngrids) )
      END IF
      phytoDecayPower(:,:)=0.0_r8
      IF (.not.allocated(R_NC)) THEN
        allocate ( R_NC(nplank,Ngrids) )
      END IF
      R_NC(:,:)=0.0_r8
      IF (.not.allocated(R_PC)) THEN
        allocate ( R_PC(nplank,Ngrids) )
      END IF
      R_PC(:,:)=0.0_r8
      IF (.not.allocated(R_SiC)) THEN
        allocate ( R_SiC(nplank,Ngrids) )
      END IF
      R_SiC(:,:)=0.0_r8
      IF (.not.allocated(R_FeC)) THEN
        allocate ( R_FeC(nplank,Ngrids) )
      END IF
      R_FeC(:,:)=0.0_r8
      IF (.not.allocated(R_ChlC)) THEN
        allocate ( R_ChlC(nplank,Ngrids) )
      END IF
      R_ChlC(:,:)=0.0_r8
      IF (.not.allocated(R_PICPOC)) THEN
        allocate ( R_PICPOC(nplank,Ngrids) )
      END IF
      R_PICPOC(:,:)=0.0_r8
      IF (.not.allocated(wsink)) THEN
        allocate ( wsink(nplank,Ngrids) )
      END IF
      wsink(:,:)=0.0_r8
      IF (.not.allocated(wswim)) THEN
        allocate ( wswim(nplank,Ngrids) )
      END IF
      wswim(:,:)=0.0_r8
      IF (.not.allocated(respiration)) THEN
        allocate ( respiration(nplank,Ngrids) )
      END IF
      respiration(:,:)=0.0_r8
      IF (.not.allocated(PCmax)) THEN
        allocate ( PCmax(nplank,Ngrids) )
      END IF
      PCmax(:,:)=0.0_r8
      IF (.not.allocated(Qnmax)) THEN
        allocate ( Qnmax(nplank,Ngrids) )
      END IF
      Qnmax(:,:)=0.0_r8
      IF (.not.allocated(Qnmin)) THEN
        allocate ( Qnmin(nplank,Ngrids) )
      END IF
      Qnmin(:,:)=0.0_r8
      IF (.not.allocated(Qpmax)) THEN
        allocate ( Qpmax(nplank,Ngrids) )
      END IF
      Qpmax(:,:)=0.0_r8
      IF (.not.allocated(Qpmin)) THEN
        allocate ( Qpmin(nplank,Ngrids) )
      END IF
      Qpmin(:,:)=0.0_r8
      IF (.not.allocated(Qsimax)) THEN
        allocate ( Qsimax(nplank,Ngrids) )
      END IF
      Qsimax(:,:)=0.0_r8
      IF (.not.allocated(Qsimin)) THEN
        allocate ( Qsimin(nplank,Ngrids) )
      END IF
      Qsimin(:,:)=0.0_r8
      IF (.not.allocated(Qfemax)) THEN
        allocate ( Qfemax(nplank,Ngrids) )
      END IF
      Qfemax(:,:)=0.0_r8
      IF (.not.allocated(Qfemin)) THEN
        allocate ( Qfemin(nplank,Ngrids) )
      END IF
      Qfemin(:,:)=0.0_r8
      IF (.not.allocated(Vmax_NH4)) THEN
        allocate ( Vmax_NH4(nplank,Ngrids) )
      END IF
      Vmax_NH4(:,:)=0.0_r8
      IF (.not.allocated(Vmax_NO2)) THEN
        allocate ( Vmax_NO2(nplank,Ngrids) )
      END IF
      Vmax_NO2(:,:)=0.0_r8
      IF (.not.allocated(Vmax_NO3)) THEN
        allocate ( Vmax_NO3(nplank,Ngrids) )
      END IF
      Vmax_NO3(:,:)=0.0_r8
      IF (.not.allocated(Vmax_N)) THEN
        allocate ( Vmax_N(nplank,Ngrids) )
      END IF
      Vmax_N(:,:)=0.0_r8
      IF (.not.allocated(Vmax_PO4)) THEN
        allocate ( Vmax_PO4(nplank,Ngrids) )
      END IF
      Vmax_PO4(:,:)=0.0_r8
      IF (.not.allocated(Vmax_SiO2)) THEN
        allocate ( Vmax_SiO2(nplank,Ngrids) )
      END IF
      Vmax_SiO2(:,:)=0.0_r8
      IF (.not.allocated(Vmax_FeT)) THEN
        allocate ( Vmax_FeT(nplank,Ngrids) )
      END IF
      Vmax_FeT(:,:)=0.0_r8
      IF (.not.allocated(ksatNH4)) THEN
        allocate ( ksatNH4(nplank,Ngrids) )
      END IF
      ksatNH4(:,:)=0.0_r8
      IF (.not.allocated(ksatNO2)) THEN
        allocate ( ksatNO2(nplank,Ngrids) )
      END IF
      ksatNO2(:,:)=0.0_r8
      IF (.not.allocated(ksatNO3)) THEN
        allocate ( ksatNO3(nplank,Ngrids) )
      END IF
      ksatNO3(:,:)=0.0_r8
      IF (.not.allocated(ksatPO4)) THEN
        allocate ( ksatPO4(nplank,Ngrids) )
      END IF
      ksatPO4(:,:)=0.0_r8
      IF (.not.allocated(ksatSiO2)) THEN
        allocate ( ksatSiO2(nplank,Ngrids) )
      END IF
      ksatSiO2(:,:)=0.0_r8
      IF (.not.allocated(ksatFeT)) THEN
        allocate ( ksatFeT(nplank,Ngrids) )
      END IF
      ksatFeT(:,:)=0.0_r8
      IF (.not.allocated(kexcC)) THEN
        allocate ( kexcC(nplank,Ngrids) )
      END IF
      kexcC(:,:)=0.0_r8
      IF (.not.allocated(kexcN)) THEN
        allocate ( kexcN(nplank,Ngrids) )
      END IF
      kexcN(:,:)=0.0_r8
      IF (.not.allocated(kexcP)) THEN
        allocate ( kexcP(nplank,Ngrids) )
      END IF
      kexcP(:,:)=0.0_r8
      IF (.not.allocated(kexcSi)) THEN
        allocate ( kexcSi(nplank,Ngrids) )
      END IF
      kexcSi(:,:)=0.0_r8
      IF (.not.allocated(kexcFe)) THEN
        allocate ( kexcFe(nplank,Ngrids) )
      END IF
      kexcFe(:,:)=0.0_r8
#if defined DARWIN_GEIDER
      IF (.not.allocated(inhibcoef_geid)) THEN
        allocate ( inhibcoef_geid(nplank,Ngrids) )
      END IF
      inhibcoef_geid(:,:)=0.0_r8
#endif
#if ! defined DARWIN_GEIDER
      IF (.not.allocated(ksatPAR)) THEN
        allocate ( ksatPAR(nplank,Ngrids) )
      END IF
      ksatPAR(:,:)=0.0_r8
      IF (.not.allocated(kinhPAR)) THEN
        allocate ( kinhPAR(nplank,Ngrids) )
      END IF
      kinhPAR(:,:)=0.0_r8
#endif
      IF (.not.allocated(mQyield)) THEN
        allocate ( mQyield(nplank,Ngrids) )
      END IF
      mQyield(:,:)=0.0_r8
      IF (.not.allocated(chl2cmax)) THEN
        allocate ( chl2cmax(nplank,Ngrids) )
      END IF
      chl2cmax(:,:)=0.0_r8
      IF (.not.allocated(grazemax)) THEN
        allocate ( grazemax(nplank,Ngrids) )
      END IF
      grazemax(:,:)=0.0_r8
      IF (.not.allocated(kgrazesat)) THEN
        allocate ( kgrazesat(nplank,Ngrids) )
      END IF
      kgrazesat(:,:)=0.0_r8
      IF (.not.allocated(palat)) THEN
        allocate ( palat(nplank,nplank,Ngrids) )
      END IF
      palat(:,:,:)=0.0_r8
      IF (.not.allocated(asseff)) THEN
        allocate ( asseff(nplank,nplank,Ngrids) )
      END IF
      asseff(:,:,:)=0.0_r8
      IF (.not.allocated(ExportFracPreyPred)) THEN
        allocate ( ExportFracPreyPred(nplank,nplank,Ngrids) )
      END IF
      ExportFracPreyPred(:,:,:)=0.0_r8
      IF (.not.allocated(yield)) THEN
        allocate ( yield(nplank,Ngrids) )
      END IF
      yield(:,:)=0.0_r8
      IF (.not.allocated(yieldO2)) THEN
        allocate ( yieldO2(nplank,Ngrids) )
      END IF
      yieldO2(:,:)=0.0_r8
      IF (.not.allocated(yieldNO3)) THEN
        allocate ( yieldNO3(nplank,Ngrids) )
      END IF
      yieldNO3(:,:)=0.0_r8
      IF (.not.allocated(ksatPON)) THEN
        allocate ( ksatPON(nplank,Ngrids) )
      END IF
      ksatPON(:,:)=0.0_r8
      IF (.not.allocated(ksatPOC)) THEN
        allocate ( ksatPOC(nplank,Ngrids) )
      END IF
      ksatPOC(:,:)=0.0_r8
      IF (.not.allocated(ksatPOP)) THEN
        allocate ( ksatPOP(nplank,Ngrids) )
      END IF
      ksatPOP(:,:)=0.0_r8
      IF (.not.allocated(ksatPOFe)) THEN
        allocate ( ksatPOFe(nplank,Ngrids) )
      END IF
      ksatPOFe(:,:)=0.0_r8
      IF (.not.allocated(ksatDON)) THEN
        allocate ( ksatDON(nplank,Ngrids) )
      END IF
      ksatDON(:,:)=0.0_r8
      IF (.not.allocated(ksatDOC)) THEN
        allocate ( ksatDOC(nplank,Ngrids) )
      END IF
      ksatDOC(:,:)=0.0_r8
      IF (.not.allocated(ksatDOP)) THEN
        allocate ( ksatDOP(nplank,Ngrids) )
      END IF
      ksatDOP(:,:)=0.0_r8
      IF (.not.allocated(ksatDOFe)) THEN
        allocate ( ksatDOFe(nplank,Ngrids) )
      END IF
      ksatDOFe(:,:)=0.0_r8
#if defined DARWIN_RADTRANS
      IF (.not.allocated(aphy_chl)) THEN
        allocate ( aphy_chl(nplank,nlam,Ngrids) )
      END IF
      IF (.not.allocated(aphy_chl_ps)) THEN
        allocate ( aphy_chl_ps(nplank,nlam,Ngrids) )
      END IF
      IF (.not.allocated(bphy_mgC)) THEN
        allocate ( bphy_mgC(nplank,nlam,Ngrids) )
      END IF
      IF (.not.allocated(bbphy_mgC)) THEN
        allocate ( bbphy_mgC(nplank,nlam,Ngrids) )
      END IF
#endif
#if ! defined DARWIN_GEIDER
      IF (.not.allocated(normI)) THEN
        allocate ( normI(nplank,Ngrids) )
      END IF
      normI(:,:)=0.0_r8
#endif
#if defined DARWIN_RADTRANS
      IF (.not.allocated(ap_type)) THEN
        allocate ( ap_type(nplank,Ngrids) )
      END IF
      ap_type(:,:)=0
#endif
#if ! defined DARWIN_RANDOM_TRAITS
      IF (.not.allocated(biovol)) THEN
        allocate ( biovol(nplank,Ngrids) )
      END IF
      biovol(:,:)=0.0_r8
#endif
      IF (.not.allocated(group)) THEN
        allocate ( group(nplank,Ngrids) )
      END IF
      group(:,:)=0
#if ! defined DARWIN_RANDOM_TRAITS
      IF (.not.allocated(biovol_bygroup)) THEN
        allocate ( biovol_bygroup(nplank,ngroup,Ngrids) )
      END IF
      biovol_bygroup(:,:,:)=0.0_r8
#endif
      IF (.not.allocated(alphachl)) THEN
        allocate ( alphachl(nplank,nlam,Ngrids) )
      END IF
      alphachl(:,:,:)=0.0_r8
      IF (.not.allocated(alpha_mean)) THEN
        allocate ( alpha_mean(nplank,Ngrids) )
      END IF
      alpha_mean(:,:)=0.0_r8
      IF (.not.allocated(chl2cmin)) THEN
        allocate ( chl2cmin(nplank,Ngrids) )
      END IF
      chl2cmin(:,:)=0.0_r8
      IF (.not.allocated(mortTempFuncMin)) THEN
        allocate ( mortTempFuncMin(nplank,Ngrids) )
      END IF
      mortTempFuncMin(:,:)=0.0_r8
      IF (.not.allocated(mort2TempFuncMin)) THEN
        allocate ( mort2TempFuncMin(nplank,Ngrids) )
      END IF
      mort2TempFuncMin(:,:)=0.0_r8
#if defined DARWIN_RANDOM_TRAITS
      IF (.not.allocated(Smallgrow)) THEN
        allocate ( Smallgrow(Ngrids) )
      END IF
      IF (.not.allocated(Biggrow)) THEN
        allocate ( Biggrow(Ngrids) )
      END IF
      IF (.not.allocated(Smallgrowrange)) THEN
        allocate ( Smallgrowrange(Ngrids) )
      END IF
      IF (.not.allocated(Biggrowrange)) THEN
        allocate ( Biggrowrange(Ngrids) )
      END IF
      IF (.not.allocated(diaz_growfac)) THEN
        allocate ( diaz_growfac(Ngrids) )
      END IF
      IF (.not.allocated(cocco_growfac)) THEN
        allocate ( cocco_growfac(Ngrids) )
      END IF
      IF (.not.allocated(diatom_growfac)) THEN
        allocate ( diatom_growfac(Ngrids) )
      END IF
      IF (.not.allocated(Smallmort)) THEN
        allocate ( Smallmort(Ngrids) )
      END IF
      IF (.not.allocated(Bigmort)) THEN
        allocate ( Bigmort(Ngrids) )
      END IF
      IF (.not.allocated(Smallmortrange)) THEN
        allocate ( Smallmortrange(Ngrids) )
      END IF
      IF (.not.allocated(Bigmortrange)) THEN
        allocate ( Bigmortrange(Ngrids) )
      END IF
      IF (.not.allocated(Smallexport)) THEN
        allocate ( Smallexport(Ngrids) )
      END IF
      IF (.not.allocated(Bigexport)) THEN
        allocate ( Bigexport(Ngrids) )
      END IF
      IF (.not.allocated(tempcoeff1)) THEN
        allocate ( tempcoeff1(Ngrids) )
      END IF
      IF (.not.allocated(tempcoeff2_small)) THEN
        allocate ( tempcoeff2_small(Ngrids) )
      END IF
      IF (.not.allocated(tempcoeff2_big)) THEN
        allocate ( tempcoeff2_big(Ngrids) )
      END IF
      IF (.not.allocated(tempcoeff3)) THEN
        allocate ( tempcoeff3(Ngrids) )
      END IF
#endif
#if defined DARWIN_RANDOM_TRAITS && defined DARWIN_TEMP_RANGE
      IF (.not.allocated(tempmax)) THEN
        allocate ( tempmax(Ngrids) )
      END IF
      IF (.not.allocated(temprange)) THEN
        allocate ( temprange(Ngrids) )
      END IF
      IF (.not.allocated(tempdecay)) THEN
        allocate ( tempdecay(Ngrids) )
      END IF
#endif
#if defined DARWIN_RANDOM_TRAITS
      IF (.not.allocated(val_R_NC)) THEN
        allocate ( val_R_NC(Ngrids) )
      END IF
      IF (.not.allocated(val_R_NC_diaz)) THEN
        allocate ( val_R_NC_diaz(Ngrids) )
      END IF
      IF (.not.allocated(val_R_PC)) THEN
        allocate ( val_R_PC(Ngrids) )
      END IF
      IF (.not.allocated(val_R_SiC_diatom)) THEN
        allocate ( val_R_SiC_diatom(Ngrids) )
      END IF
      IF (.not.allocated(val_R_FeC)) THEN
        allocate ( val_R_FeC(Ngrids) )
      END IF
      IF (.not.allocated(val_R_FeC_diaz)) THEN
        allocate ( val_R_FeC_diaz(Ngrids) )
      END IF
      IF (.not.allocated(val_R_PICPOC)) THEN
        allocate ( val_R_PICPOC(Ngrids) )
      END IF
#endif
#if defined DARWIN_RANDOM_TRAITS && ! defined DARWIN_GEIDER
      IF (.not.allocated(val_R_ChlC)) THEN
        allocate ( val_R_ChlC(Ngrids) )
      END IF
#endif
#if defined DARWIN_RANDOM_TRAITS
      IF (.not.allocated(val_R_NC_zoo)) THEN
        allocate ( val_R_NC_zoo(Ngrids) )
      END IF
      IF (.not.allocated(val_R_PC_zoo)) THEN
        allocate ( val_R_PC_zoo(Ngrids) )
      END IF
      IF (.not.allocated(val_R_SiC_zoo)) THEN
        allocate ( val_R_SiC_zoo(Ngrids) )
      END IF
      IF (.not.allocated(val_R_FeC_zoo)) THEN
        allocate ( val_R_FeC_zoo(Ngrids) )
      END IF
      IF (.not.allocated(val_R_PICPOC_zoo)) THEN
        allocate ( val_R_PICPOC_zoo(Ngrids) )
      END IF
      IF (.not.allocated(val_R_ChlC_zoo)) THEN
        allocate ( val_R_ChlC_zoo(Ngrids) )
      END IF
      IF (.not.allocated(SmallSink)) THEN
        allocate ( SmallSink(Ngrids) )
      END IF
      IF (.not.allocated(SmallSink_pday)) THEN
        allocate ( SmallSink_pday(Ngrids) )
      END IF
      IF (.not.allocated(BigSink)) THEN
        allocate ( BigSink(Ngrids) )
      END IF
      IF (.not.allocated(BigSink_pday)) THEN
        allocate ( BigSink_pday(Ngrids) )
      END IF
      IF (.not.allocated(SmallPsat)) THEN
        allocate ( SmallPsat(Ngrids) )
      END IF
      IF (.not.allocated(BigPsat)) THEN
        allocate ( BigPsat(Ngrids) )
      END IF
      IF (.not.allocated(ProcPsat)) THEN
        allocate ( ProcPsat(Ngrids) )
      END IF
      IF (.not.allocated(UniDzPsat)) THEN
        allocate ( UniDzPsat(Ngrids) )
      END IF
      IF (.not.allocated(CoccoPsat)) THEN
        allocate ( CoccoPsat(Ngrids) )
      END IF
      IF (.not.allocated(SmallPsatrange)) THEN
        allocate ( SmallPsatrange(Ngrids) )
      END IF
      IF (.not.allocated(BigPsatrange)) THEN
        allocate ( BigPsatrange(Ngrids) )
      END IF
      IF (.not.allocated(ProcPsatrange)) THEN
        allocate ( ProcPsatrange(Ngrids) )
      END IF
      IF (.not.allocated(UniDzPsatrange)) THEN
        allocate ( UniDzPsatrange(Ngrids) )
      END IF
      IF (.not.allocated(CoccoPsatrange)) THEN
        allocate ( CoccoPsatrange(Ngrids) )
      END IF
      IF (.not.allocated(ksatNH4fac)) THEN
        allocate ( ksatNH4fac(Ngrids) )
      END IF
      IF (.not.allocated(ksatNO2fac)) THEN
        allocate ( ksatNO2fac(Ngrids) )
      END IF
      IF (.not.allocated(val_amminhib)) THEN
        allocate ( val_amminhib(Ngrids) )
      END IF
      IF (.not.allocated(val_ksatsio2)) THEN
        allocate ( val_ksatsio2(Ngrids) )
      END IF
#endif
#if defined DARWIN_RANDOM_TRAITS && ! defined DARWIN_GEIDER
      IF (.not.allocated(smallksatpar)) THEN
        allocate ( smallksatpar(Ngrids) )
      END IF
      IF (.not.allocated(smallksatparstd)) THEN
        allocate ( smallksatparstd(Ngrids) )
      END IF
      IF (.not.allocated(smallkinhpar)) THEN
        allocate ( smallkinhpar(Ngrids) )
      END IF
      IF (.not.allocated(smallkinhparstd)) THEN
        allocate ( smallkinhparstd(Ngrids) )
      END IF
      IF (.not.allocated(Bigksatpar)) THEN
        allocate ( Bigksatpar(Ngrids) )
      END IF
      IF (.not.allocated(Bigksatparstd)) THEN
        allocate ( Bigksatparstd(Ngrids) )
      END IF
      IF (.not.allocated(Bigkinhpar)) THEN
        allocate ( Bigkinhpar(Ngrids) )
      END IF
      IF (.not.allocated(Bigkinhparstd)) THEN
        allocate ( Bigkinhparstd(Ngrids) )
      END IF
#endif
#if defined DARWIN_RANDOM_TRAITS && defined DARWIN_NINE_SPECIES_SETUP
      IF (.not.allocated(LLProkinhpar)) THEN
        allocate ( LLProkinhpar(Ngrids) )
      END IF
      IF (.not.allocated(Coccokinhpar)) THEN
        allocate ( Coccokinhpar(Ngrids) )
      END IF
#endif
#if defined DARWIN_RANDOM_TRAITS
      IF (.not.allocated(inhibcoef_geid_val)) THEN
        allocate ( inhibcoef_geid_val(Ngrids) )
      END IF
      IF (.not.allocated(smallmQyield)) THEN
        allocate ( smallmQyield(Ngrids) )
      END IF
      IF (.not.allocated(smallmQyieldrange)) THEN
        allocate ( smallmQyieldrange(Ngrids) )
      END IF
      IF (.not.allocated(BigmQyield)) THEN
        allocate ( BigmQyield(Ngrids) )
      END IF
      IF (.not.allocated(BigmQyieldrange)) THEN
        allocate ( BigmQyieldrange(Ngrids) )
      END IF
      IF (.not.allocated(smallchl2cmax)) THEN
        allocate ( smallchl2cmax(Ngrids) )
      END IF
      IF (.not.allocated(smallchl2cmaxrange)) THEN
        allocate ( smallchl2cmaxrange(Ngrids) )
      END IF
      IF (.not.allocated(Bigchl2cmax)) THEN
        allocate ( Bigchl2cmax(Ngrids) )
      END IF
      IF (.not.allocated(Bigchl2cmaxrange)) THEN
        allocate ( Bigchl2cmaxrange(Ngrids) )
      END IF
#endif
#if ! defined DARWIN_RADTRANS
      IF (.not.allocated(aphy_chl_ave)) THEN
        allocate ( aphy_chl_ave(Ngrids) )
      END IF
#endif
#if defined DARWIN_RANDOM_TRAITS
      IF (.not.allocated(val_acclimtimescl)) THEN
        allocate ( val_acclimtimescl(Ngrids) )
      END IF
      IF (.not.allocated(oldTwoGrazers)) THEN
        allocate ( oldTwoGrazers(Ngrids) )
      END IF
      IF (.not.allocated(GrazeFast)) THEN
        allocate ( GrazeFast(Ngrids) )
      END IF
      IF (.not.allocated(GrazeFast_pday)) THEN
        allocate ( GrazeFast_pday(Ngrids) )
      END IF
      IF (.not.allocated(ZooexfacSmall)) THEN
        allocate ( ZooexfacSmall(Ngrids) )
      END IF
      IF (.not.allocated(ZooexfacBig)) THEN
        allocate ( ZooexfacBig(Ngrids) )
      END IF
      IF (.not.allocated(ZoomortSmall)) THEN
        allocate ( ZoomortSmall(Ngrids) )
      END IF
      IF (.not.allocated(ZoomortSmall_pday)) THEN
        allocate ( ZoomortSmall_pday(Ngrids) )
      END IF
      IF (.not.allocated(ZoomortBig)) THEN
        allocate ( ZoomortBig(Ngrids) )
      END IF
      IF (.not.allocated(ZoomortBig_pday)) THEN
        allocate ( ZoomortBig_pday(Ngrids) )
      END IF
      IF (.not.allocated(ZoomortSmall2)) THEN
        allocate ( ZoomortSmall2(Ngrids) )
      END IF
      IF (.not.allocated(ZoomortBig2)) THEN
        allocate ( ZoomortBig2(Ngrids) )
      END IF
      IF (.not.allocated(ExGrazfracbig)) THEN
        allocate ( ExGrazfracbig(Ngrids) )
      END IF
      IF (.not.allocated(ExGrazfracsmall)) THEN
        allocate ( ExGrazfracsmall(Ngrids) )
      END IF
      IF (.not.allocated(palathi)) THEN
        allocate ( palathi(Ngrids) )
      END IF
      IF (.not.allocated(palatlo)) THEN
        allocate ( palatlo(Ngrids) )
      END IF
      IF (.not.allocated(diatomgraz)) THEN
        allocate ( diatomgraz(Ngrids) )
      END IF
      IF (.not.allocated(coccograz)) THEN
        allocate ( coccograz(Ngrids) )
      END IF
      IF (.not.allocated(olargegraz)) THEN
        allocate ( olargegraz(Ngrids) )
      END IF
      IF (.not.allocated(GrazeEfflow)) THEN
        allocate ( GrazeEfflow(Ngrids) )
      END IF
      IF (.not.allocated(GrazeEffmod)) THEN
        allocate ( GrazeEffmod(Ngrids) )
      END IF
      IF (.not.allocated(GrazeEffhi)) THEN
        allocate ( GrazeEffhi(Ngrids) )
      END IF
      IF (.not.allocated(GrazeRate)) THEN
        allocate ( GrazeRate(Ngrids) )
      END IF
      IF (.not.allocated(GrazeRate_pday)) THEN
        allocate ( GrazeRate_pday(Ngrids) )
      END IF
      IF (.not.allocated(ExGrazfrac)) THEN
        allocate ( ExGrazfrac(Ngrids) )
      END IF
      IF (.not.allocated(val_palat)) THEN
        allocate ( val_palat(Ngrids) )
      END IF
      IF (.not.allocated(val_ass_eff)) THEN
        allocate ( val_ass_eff(Ngrids) )
      END IF
      IF (.not.allocated(kgrazesat_val)) THEN
        allocate ( kgrazesat_val(Ngrids) )
      END IF
      IF (.not.allocated(Zoomort)) THEN
        allocate ( Zoomort(Ngrids) )
      END IF
      IF (.not.allocated(Zoomort_pday)) THEN
        allocate ( Zoomort_pday(Ngrids) )
      END IF
      IF (.not.allocated(Zoomort2)) THEN
        allocate ( Zoomort2(Ngrids) )
      END IF
      IF (.not.allocated(Zooexfac)) THEN
        allocate ( Zooexfac(Ngrids) )
      END IF
      IF (.not.allocated(ZooDM)) THEN
        allocate ( ZooDM(Ngrids) )
      END IF
#endif
#if ! defined DARWIN_RANDOM_TRAITS
      IF (.not.allocated(gud_sort_biovol)) THEN
        allocate ( gud_sort_biovol(Ngrids) )
      END IF
#endif
      IF (.not.allocated(GUD_effective_ksat)) THEN
        allocate ( GUD_effective_ksat(Ngrids) )
      END IF
      IF (.not.allocated(gud_select_kn_allom)) THEN
        allocate ( gud_select_kn_allom(Ngrids) )
      END IF
      IF (.not.allocated(logvolbase)) THEN
        allocate ( logvolbase(Ngrids) )
      END IF
#if ! defined DARWIN_RANDOM_TRAITS
      IF (.not.allocated(logvolinc)) THEN
        allocate ( logvolinc(Ngrids) )
      END IF
#endif
      IF (.not.allocated(biovol0)) THEN
        allocate ( biovol0(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(biovolfac)) THEN
        allocate ( biovolfac(nGroup,Ngrids) )
      END IF
#if ! defined DARWIN_RANDOM_TRAITS
      IF (.not.allocated(logvol0ind)) THEN
        allocate ( logvol0ind(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(grp_logvolind)) THEN
        allocate ( grp_logvolind(nPlank,nGroup,Ngrids) )
      END IF
      IF (.not.allocated(grp_biovol)) THEN
        allocate ( grp_biovol(nPlank,nGroup,Ngrids) )
      END IF
      IF (.not.allocated(grp_nplank)) THEN
        allocate ( grp_nplank(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(grp_photo)) THEN
        allocate ( grp_photo(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(grp_bacttype)) THEN
        allocate ( grp_bacttype(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(grp_aerobic)) THEN
        allocate ( grp_aerobic(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(grp_denit)) THEN
        allocate ( grp_denit(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(grp_pred)) THEN
        allocate ( grp_pred(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(grp_prey)) THEN
        allocate ( grp_prey(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(grp_hasSi)) THEN
        allocate ( grp_hasSi(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(grp_hasPIC)) THEN
        allocate ( grp_hasPIC(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(grp_diazo)) THEN
        allocate ( grp_diazo(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(grp_useNH4)) THEN
        allocate ( grp_useNH4(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(grp_useNO2)) THEN
        allocate ( grp_useNO2(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(grp_useNO3)) THEN
        allocate ( grp_useNO3(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(grp_combNO)) THEN
        allocate ( grp_combNO(nGroup,Ngrids) )
      END IF
#endif
#if defined DARWIN_RADTRANS && ! defined DARWIN_RANDOM_TRAITS
      IF (.not.allocated(grp_aptype)) THEN
        allocate ( grp_aptype(nGroup,Ngrids) )
      END IF
#endif
#if ! defined DARWIN_RANDOM_TRAITS
      IF (.not.allocated(grp_tempMort)) THEN
        allocate ( grp_tempMort(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(grp_tempMort2)) THEN
        allocate ( grp_tempMort2(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(grp_Xmin)) THEN
        allocate ( grp_Xmin(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(grp_R_NC)) THEN
        allocate ( grp_R_NC(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(grp_R_PC)) THEN
        allocate ( grp_R_PC(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(grp_R_SiC)) THEN
        allocate ( grp_R_SiC(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(grp_R_FeC)) THEN
        allocate ( grp_R_FeC(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(grp_R_ChlC)) THEN
        allocate ( grp_R_ChlC(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(grp_R_PICPOC)) THEN
        allocate ( grp_R_PICPOC(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(grp_ExportFracMort)) THEN
        allocate ( grp_ExportFracMort(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(grp_ExportFracMort2)) THEN
        allocate ( grp_ExportFracMort2(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(grp_ExportFrac)) THEN
        allocate ( grp_ExportFrac(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(grp_mort)) THEN
        allocate ( grp_mort(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(grp_mort_pday)) THEN
        allocate ( grp_mort_pday(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(grp_mort2)) THEN
        allocate ( grp_mort2(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(grp_tempcoeff1)) THEN
        allocate ( grp_tempcoeff1(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(grp_tempcoeff2)) THEN
        allocate ( grp_tempcoeff2(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(grp_tempcoeff3)) THEN
        allocate ( grp_tempcoeff3(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(grp_tempopt)) THEN
        allocate ( grp_tempopt(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(grp_tempdecay)) THEN
        allocate ( grp_tempdecay(nGroup,Ngrids) )
      END IF
#endif
#if defined DARWIN_ALLOMETRIC_PALAT && ! defined DARWIN_RANDOM_TRAITS
      IF (.not.allocated(grp_pp_sig)) THEN
        allocate ( grp_pp_sig(nGroup,Ngrids) )
      END IF
#endif
#if ! defined DARWIN_RANDOM_TRAITS && defined DARWIN_GEIDER
      IF (.not.allocated(grp_mQyield)) THEN
        allocate ( grp_mQyield(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(grp_chl2cmax)) THEN
        allocate ( grp_chl2cmax(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(grp_inhibcoef_geid)) THEN
        allocate ( grp_inhibcoef_geid(nGroup,Ngrids) )
      END IF
#endif
#if ! defined DARWIN_RANDOM_TRAITS && ! defined DARWIN_GEIDER
      IF (.not.allocated(grp_ksatPAR)) THEN
        allocate ( grp_ksatPAR(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(grp_kinhPAR)) THEN
        allocate ( grp_kinhPAR(nGroup,Ngrids) )
      END IF
#endif
#if ! defined DARWIN_RANDOM_TRAITS
      IF (.not.allocated(grp_ksatNH4fac)) THEN
        allocate ( grp_ksatNH4fac(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(grp_ksatNO2fac)) THEN
        allocate ( grp_ksatNO2fac(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(grp_amminhib)) THEN
        allocate ( grp_amminhib(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(grp_acclimtimescl)) THEN
        allocate ( grp_acclimtimescl(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(grp_acclimtimescl_pday)) THEN
        allocate ( grp_acclimtimescl_pday(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(a_graz)) THEN
        allocate ( a_graz(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(a_graz_pday)) THEN
        allocate ( a_graz_pday(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(b_graz)) THEN
        allocate ( b_graz(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(a_kg)) THEN
        allocate ( a_kg(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(b_kg)) THEN
        allocate ( b_kg(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(a_biosink)) THEN
        allocate ( a_biosink(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(a_biosink_pday)) THEN
        allocate ( a_biosink_pday(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(b_biosink)) THEN
        allocate ( b_biosink(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(a_bioswim)) THEN
        allocate ( a_bioswim(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(a_bioswim_pday)) THEN
        allocate ( a_bioswim_pday(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(b_bioswim)) THEN
        allocate ( b_bioswim(nGroup,Ngrids) )
      END IF
#endif
#if defined DARWIN_ALLOMETRIC_PALAT && ! defined DARWIN_RANDOM_TRAITS
      IF (.not.allocated(a_prdpry)) THEN
        allocate ( a_prdpry(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(b_prdpry)) THEN
        allocate ( b_prdpry(nGroup,Ngrids) )
      END IF
#endif
#if ! defined DARWIN_RANDOM_TRAITS
      IF (.not.allocated(a_vmax_DIC)) THEN
        allocate ( a_vmax_DIC(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(a_vmax_DIC_pday)) THEN
        allocate ( a_vmax_DIC_pday(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(b_vmax_DIC)) THEN
        allocate ( b_vmax_DIC(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(a_qcarbon)) THEN
        allocate ( a_qcarbon(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(b_qcarbon)) THEN
        allocate ( b_qcarbon(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(a_respir)) THEN
        allocate ( a_respir(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(b_respir)) THEN
        allocate ( b_respir(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(a_kexc_c)) THEN
        allocate ( a_kexc_c(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(b_kexc_c)) THEN
        allocate ( b_kexc_c(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(a_vmax_NO3)) THEN
        allocate ( a_vmax_NO3(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(a_vmax_NO3_pday)) THEN
        allocate ( a_vmax_NO3_pday(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(b_vmax_NO3)) THEN
        allocate ( b_vmax_NO3(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(a_kn_NO3)) THEN
        allocate ( a_kn_NO3(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(b_kn_NO3)) THEN
        allocate ( b_kn_NO3(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(a_qmin_n)) THEN
        allocate ( a_qmin_n(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(b_qmin_n)) THEN
        allocate ( b_qmin_n(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(a_qmax_n)) THEN
        allocate ( a_qmax_n(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(b_qmax_n)) THEN
        allocate ( b_qmax_n(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(a_kexc_n)) THEN
        allocate ( a_kexc_n(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(b_kexc_n)) THEN
        allocate ( b_kexc_n(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(a_vmax_NO2)) THEN
        allocate ( a_vmax_NO2(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(a_vmax_NO2_pday)) THEN
        allocate ( a_vmax_NO2_pday(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(b_vmax_NO2)) THEN
        allocate ( b_vmax_NO2(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(a_kn_NO2)) THEN
        allocate ( a_kn_NO2(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(b_kn_NO2)) THEN
        allocate ( b_kn_NO2(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(a_vmax_NH4)) THEN
        allocate ( a_vmax_NH4(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(a_vmax_NH4_pday)) THEN
        allocate ( a_vmax_NH4_pday(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(b_vmax_NH4)) THEN
        allocate ( b_vmax_NH4(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(a_kn_NH4)) THEN
        allocate ( a_kn_NH4(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(b_kn_NH4)) THEN
        allocate ( b_kn_NH4(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(a_vmax_N)) THEN
        allocate ( a_vmax_N(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(a_vmax_N_pday)) THEN
        allocate ( a_vmax_N_pday(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(b_vmax_N)) THEN
        allocate ( b_vmax_N(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(a_vmax_PO4)) THEN
        allocate ( a_vmax_PO4(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(a_vmax_PO4_pday)) THEN
        allocate ( a_vmax_PO4_pday(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(b_vmax_PO4)) THEN
        allocate ( b_vmax_PO4(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(a_kn_PO4)) THEN
        allocate ( a_kn_PO4(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(b_kn_PO4)) THEN
        allocate ( b_kn_PO4(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(a_qmin_p)) THEN
        allocate ( a_qmin_p(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(b_qmin_p)) THEN
        allocate ( b_qmin_p(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(a_qmax_p)) THEN
        allocate ( a_qmax_p(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(b_qmax_p)) THEN
        allocate ( b_qmax_p(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(a_kexc_p)) THEN
        allocate ( a_kexc_p(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(a_kexc_p_pday)) THEN
        allocate ( a_kexc_p_pday(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(b_kexc_p)) THEN
        allocate ( b_kexc_p(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(a_vmax_SiO2)) THEN
        allocate ( a_vmax_SiO2(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(a_vmax_SiO2_pday)) THEN
        allocate ( a_vmax_SiO2_pday(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(b_vmax_SiO2)) THEN
        allocate ( b_vmax_SiO2(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(a_kn_SiO2)) THEN
        allocate ( a_kn_SiO2(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(b_kn_SiO2)) THEN
        allocate ( b_kn_SiO2(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(a_qmin_si)) THEN
        allocate ( a_qmin_si(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(b_qmin_si)) THEN
        allocate ( b_qmin_si(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(a_qmax_si)) THEN
        allocate ( a_qmax_si(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(b_qmax_si)) THEN
        allocate ( b_qmax_si(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(a_kexc_si)) THEN
        allocate ( a_kexc_si(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(a_kexc_si_pday)) THEN
        allocate ( a_kexc_si_pday(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(b_kexc_si)) THEN
        allocate ( b_kexc_si(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(a_vmax_FeT)) THEN
        allocate ( a_vmax_FeT(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(a_vmax_FeT_pday)) THEN
        allocate ( a_vmax_FeT_pday(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(b_vmax_FeT)) THEN
        allocate ( b_vmax_FeT(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(a_kn_feT)) THEN
        allocate ( a_kn_feT(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(b_kn_FeT)) THEN
        allocate ( b_kn_FeT(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(a_qmin_fe)) THEN
        allocate ( a_qmin_fe(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(b_qmin_fe)) THEN
        allocate ( b_qmin_fe(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(a_qmax_fe)) THEN
        allocate ( a_qmax_fe(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(b_qmax_fe)) THEN
        allocate ( b_qmax_fe(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(a_kexc_fe)) THEN
        allocate ( a_kexc_fe(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(a_kexc_fe_pday)) THEN
        allocate ( a_kexc_fe_pday(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(b_kexc_fe)) THEN
        allocate ( b_kexc_fe(nGroup,Ngrids) )
      END IF
      IF (.not.allocated(grp_ExportFracPreyPred)) THEN
        allocate ( grp_ExportFracPreyPred(nGroup,nGroup,Ngrids) )
      END IF
      IF (.not.allocated(grp_ass_eff)) THEN
        allocate ( grp_ass_eff(nGroup,nGroup,Ngrids) )
      END IF
#endif
#if defined DARWIN_USE_PLOAD
      IF (.not.allocated(Pa2Atm)) THEN
        allocate ( Pa2Atm(Ngrids) )
      END IF
      Pa2Atm(:)=101325.0_r8
#endif
      IF (.not.allocated(ptr2mol)) THEN
        allocate ( ptr2mol(Ngrids) )
      END IF
      ptr2mol(:)=0.001_r8
      IF (.not.allocated(sca1)) THEN
        allocate ( sca1(Ngrids) )
      END IF
      sca1(:)=2073.1_r8
      IF (.not.allocated(sca2)) THEN
        allocate ( sca2(Ngrids) )
      END IF
      sca2(:)=-125.62_r8
      IF (.not.allocated(sca3)) THEN
        allocate ( sca3(Ngrids) )
      END IF
      sca3(:)=3.6276_r8
      IF (.not.allocated(sca4)) THEN
        allocate ( sca4(Ngrids) )
      END IF
      sca4(:)=-0.043219_r8
      IF (.not.allocated(sox1)) THEN
        allocate ( sox1(Ngrids) )
      END IF
      sox1(:)=1638.0_r8
      IF (.not.allocated(sox2)) THEN
        allocate ( sox2(Ngrids) )
      END IF
      sox2(:)=-81.83_r8
      IF (.not.allocated(sox3)) THEN
        allocate ( sox3(Ngrids) )
      END IF
      sox3(:)=1.483_r8
      IF (.not.allocated(sox4)) THEN
        allocate ( sox4(Ngrids) )
      END IF
      sox4(:)=-0.008004_r8
      IF (.not.allocated(oA0)) THEN
        allocate ( oA0(Ngrids) )
      END IF
      oA0(:)=2.00907_r8
      IF (.not.allocated(oA1)) THEN
        allocate ( oA1(Ngrids) )
      END IF
      oA1(:)=3.22014_r8
      IF (.not.allocated(oA2)) THEN
        allocate ( oA2(Ngrids) )
      END IF
      oA2(:)=4.0501_r8
      IF (.not.allocated(oA3)) THEN
        allocate ( oA3(Ngrids) )
      END IF
      oA3(:)=4.94457_r8
      IF (.not.allocated(oA4)) THEN
        allocate ( oA4(Ngrids) )
      END IF
      oA4(:)=-0.256847_r8
      IF (.not.allocated(oA5)) THEN
        allocate ( oA5(Ngrids) )
      END IF
      oA5(:)=3.88767_r8
      IF (.not.allocated(oB0)) THEN
        allocate ( oB0(Ngrids) )
      END IF
      oB0(:)=-0.00624523_r8
      IF (.not.allocated(oB1)) THEN
        allocate ( oB1(Ngrids) )
      END IF
      oB1(:)=-0.00737614_r8
      IF (.not.allocated(oB2)) THEN
        allocate ( oB2(Ngrids) )
      END IF
      oB2(:)=-0.010341_r8
      IF (.not.allocated(oB3)) THEN
        allocate ( oB3(Ngrids) )
      END IF
      oB3(:)=-0.00817083_r8
      IF (.not.allocated(oC0)) THEN
        allocate ( oC0(Ngrids) )
      END IF
      oC0(:)=-4.88682e-07_r8
#if defined DARWIN_RANDOM_TRAITS
      IF (.not.allocated(phymin)) THEN
        allocate ( phymin(Ngrids) )
      END IF
#endif
      IF (.not.allocated(katten_w)) THEN
        allocate ( katten_w(Ngrids) )
      END IF
      IF (.not.allocated(katten_chl)) THEN
        allocate ( katten_chl(Ngrids) )
      END IF
      IF (.not.allocated(parfrac)) THEN
        allocate ( parfrac(Ngrids) )
      END IF
#if DARWIN_TEMP_VERSION == 1
      IF (.not.allocated(tempnorm)) THEN
        allocate ( tempnorm(Ngrids) )
      END IF
#endif
#if DARWIN_TEMP_VERSION == 2
      IF (.not.allocated(TempAeArr)) THEN
        allocate ( TempAeArr(Ngrids) )
      END IF
      TempAeArr(:)=-4000.0_r8
      IF (.not.allocated(TemprefArr)) THEN
        allocate ( TemprefArr(Ngrids) )
      END IF
      TemprefArr(:)=293.15_r8
      IF (.not.allocated(TempCoeffArr)) THEN
        allocate ( TempCoeffArr(Ngrids) )
      END IF
      TempCoeffArr(:)=0.5882_r8
#endif
      IF (.not.allocated(alpfe)) THEN
        allocate ( alpfe(Ngrids) )
      END IF
#if ! defined DARWIN_PART_SCAV_POP && ! defined DARWIN_PART_SCAV
      IF (.not.allocated(scav)) THEN
        allocate ( scav(Ngrids) )
      END IF
      IF (.not.allocated(scav_pyear)) THEN
        allocate ( scav_pyear(Ngrids) )
      END IF
#endif
      IF (.not.allocated(ligand_tot)) THEN
        allocate ( ligand_tot(Ngrids) )
      END IF
      IF (.not.allocated(ligand_stab)) THEN
        allocate ( ligand_stab(Ngrids) )
      END IF
#if defined DARWIN_MINFE
      IF (.not.allocated(freefemax)) THEN
        allocate ( freefemax(Ngrids) )
      END IF
#endif
#if defined DARWIN_PART_SCAV_POP || defined DARWIN_PART_SCAV
      IF (.not.allocated(scav_rat)) THEN
        allocate ( scav_rat(Ngrids) )
      END IF
      IF (.not.allocated(scav_rat_pday)) THEN
        allocate ( scav_rat_pday(Ngrids) )
      END IF
      IF (.not.allocated(scav_inter)) THEN
        allocate ( scav_inter(Ngrids) )
      END IF
      IF (.not.allocated(scav_exp)) THEN
        allocate ( scav_exp(Ngrids) )
      END IF
#endif
#if defined DARWIN_PART_SCAV_POP
      IF (.not.allocated(scav_R_POPPOC)) THEN
        allocate ( scav_R_POPPOC(Ngrids) )
      END IF
#endif
#if ! defined DARWIN_IRON_SED_SOURCE_VARIABLE
      IF (.not.allocated(fesedflux)) THEN
        allocate ( fesedflux(Ngrids) )
      END IF
      IF (.not.allocated(fesedflux_pday)) THEN
        allocate ( fesedflux_pday(Ngrids) )
      END IF
#endif
#if defined DARWIN_IRON_SED_SOURCE_VARIABLE
      IF (.not.allocated(fesedflux_pcm)) THEN
        allocate ( fesedflux_pcm(Ngrids) )
      END IF
      IF (.not.allocated(R_CP_fesed)) THEN
        allocate ( R_CP_fesed(Ngrids) )
      END IF
#endif
      IF (.not.allocated(Knita)) THEN
        allocate ( Knita(Ngrids) )
      END IF
      IF (.not.allocated(Knita_pday)) THEN
        allocate ( Knita_pday(Ngrids) )
      END IF
      IF (.not.allocated(Knitb)) THEN
        allocate ( Knitb(Ngrids) )
      END IF
      IF (.not.allocated(Knitb_pday)) THEN
        allocate ( Knitb_pday(Ngrids) )
      END IF
      IF (.not.allocated(PAR_oxi)) THEN
        allocate ( PAR_oxi(Ngrids) )
      END IF
      IF (.not.allocated(Kdoc)) THEN
        allocate ( Kdoc(Ngrids) )
      END IF
      IF (.not.allocated(Kdoc_pday)) THEN
        allocate ( Kdoc_pday(Ngrids) )
      END IF
      IF (.not.allocated(Kdop)) THEN
        allocate ( Kdop(Ngrids) )
      END IF
      IF (.not.allocated(Kdop_pday)) THEN
        allocate ( Kdop_pday(Ngrids) )
      END IF
      IF (.not.allocated(Kdon)) THEN
        allocate ( Kdon(Ngrids) )
      END IF
      IF (.not.allocated(Kdon_pday)) THEN
        allocate ( Kdon_pday(Ngrids) )
      END IF
      IF (.not.allocated(KdoFe)) THEN
        allocate ( KdoFe(Ngrids) )
      END IF
      IF (.not.allocated(KdoFe_pday)) THEN
        allocate ( KdoFe_pday(Ngrids) )
      END IF
      IF (.not.allocated(KPOC)) THEN
        allocate ( KPOC(Ngrids) )
      END IF
      IF (.not.allocated(KPOC_pday)) THEN
        allocate ( KPOC_pday(Ngrids) )
      END IF
      IF (.not.allocated(KPOP)) THEN
        allocate ( KPOP(Ngrids) )
      END IF
      IF (.not.allocated(KPOP_pday)) THEN
        allocate ( KPOP_pday(Ngrids) )
      END IF
      IF (.not.allocated(KPON)) THEN
        allocate ( KPON(Ngrids) )
      END IF
      IF (.not.allocated(KPON_pday)) THEN
        allocate ( KPON_pday(Ngrids) )
      END IF
      IF (.not.allocated(KPOFe)) THEN
        allocate ( KPOFe(Ngrids) )
      END IF
      IF (.not.allocated(KPOFe_pday)) THEN
        allocate ( KPOFe_pday(Ngrids) )
      END IF
      IF (.not.allocated(KPOSi)) THEN
        allocate ( KPOSi(Ngrids) )
      END IF
      IF (.not.allocated(KPOSi_pday)) THEN
        allocate ( KPOSi_pday(Ngrids) )
      END IF
      IF (.not.allocated(wC_sink)) THEN
        allocate ( wC_sink(Ngrids) )
      END IF
      IF (.not.allocated(wC_sink_pday)) THEN
        allocate ( wC_sink_pday(Ngrids) )
      END IF
      IF (.not.allocated(wP_sink)) THEN
        allocate ( wP_sink(Ngrids) )
      END IF
      IF (.not.allocated(wP_sink_pday)) THEN
        allocate ( wP_sink_pday(Ngrids) )
      END IF
      IF (.not.allocated(wN_sink)) THEN
        allocate ( wN_sink(Ngrids) )
      END IF
      IF (.not.allocated(wN_sink_pday)) THEN
        allocate ( wN_sink_pday(Ngrids) )
      END IF
      IF (.not.allocated(wFe_sink)) THEN
        allocate ( wFe_sink(Ngrids) )
      END IF
      IF (.not.allocated(wFe_sink_pday)) THEN
        allocate ( wFe_sink_pday(Ngrids) )
      END IF
      IF (.not.allocated(wSi_sink)) THEN
        allocate ( wSi_sink(Ngrids) )
      END IF
      IF (.not.allocated(wSi_sink_pday)) THEN
        allocate ( wSi_sink_pday(Ngrids) )
      END IF
      IF (.not.allocated(wPIC_sink)) THEN
        allocate ( wPIC_sink(Ngrids) )
      END IF
      IF (.not.allocated(wPIC_sink_pday)) THEN
        allocate ( wPIC_sink_pday(Ngrids) )
      END IF
      IF (.not.allocated(Kdissc)) THEN
        allocate ( Kdissc(Ngrids) )
      END IF
      IF (.not.allocated(Kdissc_pday)) THEN
        allocate ( Kdissc_pday(Ngrids) )
      END IF
#if defined DARWIN_CARBON
      IF (.not.allocated(gud_atmos_pCO2)) THEN
        allocate ( gud_atmos_pCO2(Ngrids) )
      END IF
      IF (.not.allocated(R_OP)) THEN
        allocate ( R_OP(Ngrids) )
      END IF
      IF (.not.allocated(m3perkg)) THEN
        allocate ( m3perkg(Ngrids) )
      END IF
      IF (.not.allocated(surfSiMinInit)) THEN
        allocate ( surfSiMinInit(Ngrids) )
      END IF
      IF (.not.allocated(surfSaltMin)) THEN
        allocate ( surfSaltMin(Ngrids) )
      END IF
      IF (.not.allocated(surfSaltMax)) THEN
        allocate ( surfSaltMax(Ngrids) )
      END IF
      IF (.not.allocated(surfTempMin)) THEN
        allocate ( surfTempMin(Ngrids) )
      END IF
      IF (.not.allocated(surfTempMax)) THEN
        allocate ( surfTempMax(Ngrids) )
      END IF
      IF (.not.allocated(surfDICMin)) THEN
        allocate ( surfDICMin(Ngrids) )
      END IF
      IF (.not.allocated(surfDICMax)) THEN
        allocate ( surfDICMax(Ngrids) )
      END IF
      IF (.not.allocated(surfALKMin)) THEN
        allocate ( surfALKMin(Ngrids) )
      END IF
      IF (.not.allocated(surfALKMax)) THEN
        allocate ( surfALKMax(Ngrids) )
      END IF
      IF (.not.allocated(surfPO4Min)) THEN
        allocate ( surfPO4Min(Ngrids) )
      END IF
      IF (.not.allocated(surfPO4Max)) THEN
        allocate ( surfPO4Max(Ngrids) )
      END IF
      IF (.not.allocated(surfSiMax)) THEN
        allocate ( surfSiMax(Ngrids) )
      END IF
      IF (.not.allocated(O2crit)) THEN
        allocate ( O2crit(Ngrids) )
      END IF
#endif
#if defined DARWIN_DENIT
      IF (.not.allocated(denit_NP)) THEN
        allocate ( denit_NP(Ngrids) )
      END IF
      IF (.not.allocated(denit_NO3)) THEN
        allocate ( denit_NO3(Ngrids) )
      END IF
      IF (.not.allocated(NO3crit)) THEN
        allocate ( NO3crit(Ngrids) )
      END IF
#endif
      IF (.not.allocated(PARmin)) THEN
        allocate ( PARmin(Ngrids) )
      END IF
#if defined DARWIN_GEIDER && defined DARWIN_CHLQUOTA && defined DARWIN_NQUOTA
      IF (.not.allocated(chl2nmax)) THEN
        allocate ( chl2nmax(Ngrids) )
      END IF
      IF (.not.allocated(synthcost)) THEN
        allocate ( synthcost(Ngrids) )
      END IF
#endif
#if defined DARWIN_ALLOMETRIC_PALAT && ! defined DARWIN_RANDOM_TRAITS
      IF (.not.allocated(palat_min)) THEN
        allocate ( palat_min(Ngrids) )
      END IF
#endif
      IF (.not.allocated(inhib_graz)) THEN
        allocate ( inhib_graz(Ngrids) )
      END IF
      IF (.not.allocated(inhib_graz_exp)) THEN
        allocate ( inhib_graz_exp(Ngrids) )
      END IF
      IF (.not.allocated(hillnum)) THEN
        allocate ( hillnum(Ngrids) )
      END IF
      IF (.not.allocated(hollexp)) THEN
        allocate ( hollexp(Ngrids) )
      END IF
      IF (.not.allocated(phygrazmin)) THEN
        allocate ( phygrazmin(Ngrids) )
      END IF
      IF (.not.allocated(pmaxPON)) THEN
        allocate ( pmaxPON(Ngrids) )
      END IF
      IF (.not.allocated(pmaxPON_pday)) THEN
        allocate ( pmaxPON_pday(Ngrids) )
      END IF
      IF (.not.allocated(pmaxDON)) THEN
        allocate ( pmaxDON(Ngrids) )
      END IF
      IF (.not.allocated(pmaxDON_pday)) THEN
        allocate ( pmaxDON_pday(Ngrids) )
      END IF
      IF (.not.allocated(pcoefO2)) THEN
        allocate ( pcoefO2(Ngrids) )
      END IF
      IF (.not.allocated(pcoefO2_pday)) THEN
        allocate ( pcoefO2_pday(Ngrids) )
      END IF
      IF (.not.allocated(pmaxDIN)) THEN
        allocate ( pmaxDIN(Ngrids) )
      END IF
      IF (.not.allocated(pmaxDIN_pday)) THEN
        allocate ( pmaxDIN_pday(Ngrids) )
      END IF
#if ! defined DARWIN_RANDOM_TRAITS
      IF (.not.allocated(ksatPOM)) THEN
        allocate ( ksatPOM(Ngrids) )
      END IF
      IF (.not.allocated(ksatDOM)) THEN
        allocate ( ksatDOM(Ngrids) )
      END IF
#endif
      IF (.not.allocated(ksatDIN)) THEN
        allocate ( ksatDIN(Ngrids) )
      END IF
      IF (.not.allocated(alpha_hydrol)) THEN
        allocate ( alpha_hydrol(Ngrids) )
      END IF
#if ! defined DARWIN_RANDOM_TRAITS
      IF (.not.allocated(yod)) THEN
        allocate ( yod(Ngrids) )
      END IF
      IF (.not.allocated(yoe)) THEN
        allocate ( yoe(Ngrids) )
      END IF
      IF (.not.allocated(ynd)) THEN
        allocate ( ynd(Ngrids) )
      END IF
      IF (.not.allocated(yne)) THEN
        allocate ( yne(Ngrids) )
      END IF
#endif
#if defined DARWIN_RADTRANS
      IF (.not.allocated(gud_selectSolz)) THEN
        allocate ( gud_selectSolz(Ngrids) )
      END IF
      IF (.not.allocated(gud_refract_water)) THEN
        allocate ( gud_refract_water(Ngrids) )
      END IF
      IF (.not.allocated(gud_rmud_max)) THEN
        allocate ( gud_rmud_max(Ngrids) )
      END IF
      IF (.not.allocated(gud_radtrans_kmax)) THEN
        allocate ( gud_radtrans_kmax(Ngrids) )
      END IF
      IF (.not.allocated(gud_part_size_P)) THEN
        allocate ( gud_part_size_P(Ngrids) )
      END IF
      IF (.not.allocated(gud_waveband_edges)) THEN
        allocate ( gud_waveband_edges(nlam+1,Ngrids) )
      END IF
      IF (.not.allocated(gud_waveband_centers)) THEN
        allocate ( gud_waveband_centers(nlam,Ngrids) )
      END IF
      IF (.not.allocated(gud_radmodThresh)) THEN
        allocate ( gud_radmodThresh(Ngrids) )
      END IF
      IF (.not.allocated(gud_rmus)) THEN
        allocate ( gud_rmus(Ngrids) )
      END IF
      IF (.not.allocated(gud_rmuu)) THEN
        allocate ( gud_rmuu(Ngrids) )
      END IF
      IF (.not.allocated(gud_bbmin)) THEN
        allocate ( gud_bbmin(Ngrids) )
      END IF
      IF (.not.allocated(gud_bbw)) THEN
        allocate ( gud_bbw(Ngrids) )
      END IF
      IF (.not.allocated(gud_lambda_aCDOM)) THEN
        allocate ( gud_lambda_aCDOM(Ngrids) )
      END IF
      IF (.not.allocated(gud_Sdom)) THEN
        allocate ( gud_Sdom(Ngrids) )
      END IF
      IF (.not.allocated(gud_aCDOM_fac)) THEN
        allocate ( gud_aCDOM_fac(Ngrids) )
      END IF
      IF (.not.allocated(gud_allomSpectra)) THEN
        allocate ( gud_allomSpectra(Ngrids) )
      END IF
      IF (.not.allocated(gud_aCarCell)) THEN
        allocate ( gud_aCarCell(Ngrids) )
      END IF
      IF (.not.allocated(gud_bCarCell)) THEN
        allocate ( gud_bCarCell(Ngrids) )
      END IF
      IF (.not.allocated(gud_absorpSlope)) THEN
        allocate ( gud_absorpSlope(Ngrids) )
      END IF
      IF (.not.allocated(gud_bbbSlope)) THEN
        allocate ( gud_bbbSlope(Ngrids) )
      END IF
      IF (.not.allocated(gud_scatSwitchSizeLog)) THEN
        allocate ( gud_scatSwitchSizeLog(nlam,Ngrids) )
      END IF
      IF (.not.allocated(gud_scatSlopeSmall)) THEN
        allocate ( gud_scatSlopeSmall(nlam,Ngrids) )
      END IF
      IF (.not.allocated(gud_scatSlopeLarge)) THEN
        allocate ( gud_scatSlopeLarge(nlam,Ngrids) )
      END IF
#endif
#if defined DARWIN_CDOM
      IF (.not.allocated(fracCDOM)) THEN
        allocate ( fracCDOM(Ngrids) )
      END IF
      IF (.not.allocated(CDOMdegrd)) THEN
        allocate ( CDOMdegrd(Ngrids) )
      END IF
      IF (.not.allocated(CDOMdegrd_pday)) THEN
        allocate ( CDOMdegrd_pday(Ngrids) )
      END IF
      IF (.not.allocated(CDOMbleach)) THEN
        allocate ( CDOMbleach(Ngrids) )
      END IF
      IF (.not.allocated(CDOMbleach_pday)) THEN
        allocate ( CDOMbleach_pday(Ngrids) )
      END IF
      IF (.not.allocated(PARCDOM)) THEN
        allocate ( PARCDOM(Ngrids) )
      END IF
      IF (.not.allocated(R_NP_CDOM)) THEN
        allocate ( R_NP_CDOM(Ngrids) )
      END IF
      IF (.not.allocated(R_FeP_CDOM)) THEN
        allocate ( R_FeP_CDOM(Ngrids) )
      END IF
      IF (.not.allocated(R_CP_CDOM)) THEN
        allocate ( R_CP_CDOM(Ngrids) )
      END IF
#endif
#if defined DARWIN_CDOM && defined DARWIN_RADTRANS
      IF (.not.allocated(CDOMcoeff)) THEN
        allocate ( CDOMcoeff(Ngrids) )
      END IF
#endif
      IF (.not.allocated(BioMin)) THEN
        allocate ( BioMin(Ngrids) )
      END IF
#if defined DARWIN_RANDOM_TRAITS
      IF (.not.allocated(seed_phytoplankton)) THEN
        allocate ( seed_phytoplankton(nChl,Ngrids) )
      END IF
#endif
#if defined DARWIN_DEBUGVARS
      IF (.not.allocated(darwin_debug_1di)) THEN
        allocate ( darwin_debug_1di(Ngrids) )
      END IF
      IF (.not.allocated(darwin_debug_2df)) THEN
        allocate ( darwin_debug_2df(nlam,Ngrids) )
      END IF
#endif
#if ! defined DARWIN_RANDOM_TRAITS && defined DARWIN_PLANK_BUOYCTRL
      IF (.not.allocated(grp_buoyctrl)) THEN
        allocate ( grp_buoyctrl(nGroup,Ngrids) )
      END IF
#endif
#if defined DARWIN_PLANK_BUOYCTRL
      IF (.not.allocated(buoyctrl)) THEN
        allocate ( buoyctrl(nplank,Ngrids) )
      END IF
      buoyctrl(:,:)=.FALSE.
#endif
