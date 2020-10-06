!
!  converted from "gud_generate_random.F" using darwinfortran_to_romsfortran
!  (written by J. Paul Mattern)
!
! length of day (seconds)
        pday = 86400.0_r8

        DO np = 1, nPlank
         kexcC(np,ng) = 0.0_r8
         kexcN(np,ng) = 0.0_r8
         kexcP(np,ng) = 0.0_r8
         kexcSi(np,ng) = 0.0_r8
         kexcFe(np,ng) = 0.0_r8

         Qnmax(np,ng) = 0.0_r8
         Qnmin(np,ng) = 0.0_r8
         Qpmax(np,ng) = 0.0_r8
         Qpmin(np,ng) = 0.0_r8
         Qsimax(np,ng) = 0.0_r8
         Qsimin(np,ng) = 0.0_r8
         Qfemax(np,ng) = 0.0_r8
         Qfemin(np,ng) = 0.0_r8

         Vmax_FeT(np,ng) = 0.0_r8
         Vmax_NH4(np,ng) = 0.0_r8
         Vmax_NO2(np,ng) = 0.0_r8
         Vmax_NO3(np,ng) = 0.0_r8
         Vmax_N(np,ng) = 0.0_r8
         Vmax_PO4(np,ng) = 0.0_r8
         Vmax_SiO2(np,ng) = 0.0_r8

! initialize discrete traits to "unset"
         physize(np) = -1
         diacoc(np) = -1
         diazo(np,ng) = -1
         nsource(np) = -1

         isPhoto(np,ng) = 0
        ENDDO

! ======================================================================
! phytoplankton
! ======================================================================

        DO np = 1, nPhoto
          CALL ran_seed(seed_phytoplankton(np,ng))

        isPhoto(np,ng) = 1

        Xmin(np,ng) = phymin(ng)

! ======================================================================
! RANDOM NUMBERS

! pre-compute random numbers and discrete traits

! phyto either "small" (physize(np)=0.0_r8) or "big" (physize(np)=1.0_r8)
! at this point independent of whether diatom or coccolithophor or not
        CALL ran1(RandNoSize)
#if defined DARWIN_VERBOSE_RAND
          write(*,'(a,i1.1,a,1x,f7.4,)') '>>>RandNoSize(',np,'):', RandNoSize
#endif
        IF (physize(np).LT.0) THEN
         IF(RandNoSize .GT. 0.500_r8)then
          physize(np) = 1
         ELSE
          physize(np) = 0
         ENDIF
        ENDIF
#if defined DARWIN_TWO_SPECIES_SETUP
        IF (np.EQ.1) physize(np) = 1
        IF (np.EQ.2) physize(np) = 0
#endif
#if defined DARWIN_NINE_SPECIES_SETUP
        IF (np.LT.3.or.np.EQ.6.or.np.EQ.9) then
          physize(np) = 1
        ELSE
          physize(np) = 0
        ENDIF
#endif
!
! phyto either diatoms (diacoc=1.0_r8) and use silica or cocolithophor
! (diacoc=2.0_r8) and produce PIC or neither (diacoc=0.0_r8)
! if they are large
        IF (physize(np).EQ.1) then
          CALL ran1(RandNoDiatom)
#if defined DARWIN_VERBOSE_RAND
            write(*,'(a,i1.1,a,1x,f7.4,)') '>>>RandNoDiatom(',np,'):', RandNoDiatom
#endif
          IF (diacoc(np) .LT. 0) THEN
           IF(RandNoDiatom .GT. 0.500_r8)then
            diacoc(np) = 1
           ELSE
            diacoc(np) = 0
           ENDIF
! IF(RandNo .GT. 0.670_r8)then
! diacoc(np) = 1
! ENDIF
! IF(RandNo .GT. 0.330_r8 .AND. RandNo. le. 0.67_r8)then
! diacoc(np) = 2
! ENDIF
! IF (RandNo .LE. 0.330_r8) then
! diacoc(np) = 0
! ENDIF
          ENDIF
        ELSE
           diacoc(np) = 0
        ENDIF
#if defined DARWIN_TWO_SPECIES_SETUP
        diacoc(np) = 0
#endif
#if defined DARWIN_NINE_SPECIES_SETUP
        IF (np.EQ.1) then
          diacoc(np) = 1
        ELSE
          diacoc(np) = 0
        ENDIF
        IF (np.EQ.9) then
          diacoc(np) = 2
        ENDIF
#endif

! phyto either diazotrophs (diazo(ng)=1.0_r8) or not (diazo(ng)=0.0_r8)
        CALL ran1(RandNoDiazo)
#if defined DARWIN_VERBOSE_RAND
          write(*,'(a,i1.1,a,1x,f7.4,)') '>>>RandNoDiazo(',np,'):', RandNoDiazo
#endif
        IF (diazo(np,ng) .LT. 0) THEN
         IF(RandNoDiazo .GT. 0.6700_r8)then
          diazo(np,ng) = 1
         ELSE
          diazo(np,ng) = 0
         ENDIF
        ENDIF
! TEST ...........................................
#if ! defined DARWIN_DIAZ
        diazo(np,ng) = 0
#endif
! TEST ...........................................
#if defined DARWIN_TWO_SPECIES_SETUP
        diazo(np,ng) = 0
#endif
#if defined DARWIN_NINE_SPECIES_SETUP
        IF (np.GT.5.AND.np.LT.8) then
           diazo(np,ng) = 1
        ELSE
           diazo(np,ng) = 0
        ENDIF
#endif

        CALL ran1(RandNoGrow)
#if defined DARWIN_VERBOSE_RAND
          write(*,'(a,i1.1,a,1x,f7.4,)') '>>>RandNoGrow(',np,'):', RandNoGrow
#endif
        CALL ran1(RandNoMort)
#if defined DARWIN_VERBOSE_RAND
          write(*,'(a,i1.1,a,1x,f7.4,)') '>>>RandNoMort(',np,'):', RandNoMort
#endif
! nutrient source 
        IF(diazo(np,ng) .ne. 1)then
           CALL ran1(RandNoNsrc)
#if defined DARWIN_VERBOSE_RAND
             write(*,'(a,i1.1,a,1x,f7.4,)') '>>>RandNoNsrc(',np,'):', RandNoNsrc
#endif
           IF (nsource(np) .LT. 0) THEN
            IF (physize(np).EQ.1) then 
             nsource(np) = 3
            ELSE
             IF(RandNoNsrc .GT. 0.670_r8)then
               nsource(np) = 1
             ELSEif(RandNoNsrc .LT. 0.33_r8)then
               nsource(np) = 2
             ELSE
               nsource(np) = 3
             ENDIF
! ANNA shift bias away from pros. Now equal chance of being HL, LL, Syn, Euk.
! ANNA i.e. now 50% chance of being Pro (nsource 1 or 2, with 50% change of each being HL)
! ANNA i.e. and 50% chance of being non-Pro (nsource 3, with 50% chance of non-pro being Syn)
! IF(RandNo .GT. 0.50_r8)then
! nsource(np) = 3
! ELSEif(RandNo .LT. 0.25_r8)then
! nsource(np) = 2
! ELSE
! nsource(np) = 1
! ENDIF 
            ENDIF
           ENDIF
        ELSE
           nsource(np) = 0
        ENDIF 
#if defined DARWIN_TWO_SPECIES_SETUP
        nsource(np) = 3
#endif
#if defined DARWIN_NINE_SPECIES_SETUP
        IF (np.LT.4) then
          nsource(np) = 3
        ENDIF
        nsource(4)=2
        nsource(5)=1
        IF (np.GT.5.AND.np.LT.8) then
          nsource(np) = 0
        ENDIF
        IF (np.GT.7) then
          nsource(np) = 3
        ENDIF
#endif

!.....................................................
! ANNA make selections for RADTRANS 
!.....................................................
#if defined DARWIN_RADTRANS
! for now, choice of four absorption spectra types
! pros get either 'HL' or 'LL'
! small others get 'syn' or 'euk'
! large get 'euk'
! each 'type', once assigned, gets given actual values in_ wavebands_init_vari.F

! ANNA_Q could use tricho abs and scattering spectra (Subramanian et al. 1999)
! ANNA_Q think diaz is turned off for now
! Diaz will be 0 if not defined, and will have nsource = 0.0_r8 
        !print*,'nopt',nopt,np,nsource(np),ap_type(np,ng)
        IF (nOpt.EQ.4) then
         CALL ran1(RandNoAPType3)
#if defined DARWIN_VERBOSE_RAND
           write(*,'(a,i1.1,a,1x,f7.4,)') '>>>RandNoAPType3(',np,'):', RandNoAPType3
#endif
         CALL ran1(RandNoAPType2)
#if defined DARWIN_VERBOSE_RAND
           write(*,'(a,i1.1,a,1x,f7.4,)') '>>>RandNoAPType2(',np,'):', RandNoAPType2
#endif
         CALL ran1(RandNoAPType1)
#if defined DARWIN_VERBOSE_RAND
           write(*,'(a,i1.1,a,1x,f7.4,)') '>>>RandNoAPType1(',np,'):', RandNoAPType1
#endif
         IF (nsource(np).EQ.0) then !if diazo(ng)
          IF (physize(np).EQ.1.0d0) then !if BIG
          ap_type(np,ng) = 1 !euk (assume diatom association)
          ELSE !or
          ap_type(np,ng) = 2 !syn (for now - tricho has billins)
          ENDIF
         ENDIF 

         IF (nsource(np).EQ.3) then !if all three sources (NO3)
          IF (physize(np).EQ.1.0d0) then !if BIG 
          ap_type(np,ng) = 1 !euk
          ELSE !if SMALL
           IF (RandNoAPType3.GT.0.500d0) then
           ap_type(np,ng) = 1 !euk
           ELSE !or
           ap_type(np,ng) = 2 !Syn 
           ENDIF
          ENDIF
         ENDIF
   
         IF (nsource(np).EQ.2) then !if NH4 only
          IF (RandNoAPType2.GT.0.500d0) then
          ap_type(np,ng) = 3 !Pro HL 
          ELSE !or
          ap_type(np,ng) = 4 !Pro LL 
          ENDIF
         ENDIF

         IF (nsource(np).EQ.1) then !if NH4 & NO2
          IF (RandNoAPType1.GT.0.500d0) then
          ap_type(np,ng) = 3 !Pro HL 
          ELSE !or
          ap_type(np,ng) = 4 !Pro LL 
          ENDIF
         ENDIF
         !print*,'ap',np,nsource(np),ap_type(np,ng)
        ENDIF
!
        IF (nOpt.EQ.12) then
         CALL ran1(RandNoAPType4)
#if defined DARWIN_VERBOSE_RAND
           write(*,'(a,i1.1,a,1x,f7.4,)') '>>>RandNoAPType4(',np,'):', RandNoAPType4
#endif
         IF (nsource(np).EQ.0) then !if diazo(ng)
          IF (physize(np).EQ.1.0d0) then !if BIG
           IF (diacoc(np).EQ.1.0d0) then
             ap_type(np,ng) = 5 !diatom association
           ENDIF
           IF (diacoc(np).EQ.0.0d0) then
             ap_type(np,ng) = 7 !tricho
           ENDIF
           IF (diacoc(np).EQ.2.0d0) then
             ap_type(np,ng) = 6 !coccolithopher(?)
           ENDIF
          ELSE !or
           ap_type(np,ng) = 1 !unicellular (whould be 8 -
                                          !but currently zero)
          ENDIF
         ENDIF

         IF (nsource(np).EQ.3) then !if all three sources (NO3)
          IF (physize(np).EQ.1.0d0) then !if BIG
           IF (diacoc(np).EQ.1.0d0) then
             ap_type(np,ng) = 5 !diatom
           ENDIF
           IF (diacoc(np).EQ.0.0d0) then
             ap_type(np,ng) = 9 !Lg Euk
           ENDIF
           IF (diacoc(np).EQ.2.0d0) then
             ap_type(np,ng) = 6 !coccolithopher
           ENDIF
          ELSE !if SMALL
           IF (RandNoAPType4.GT.0.500d0) then
           ap_type(np,ng) = 1 !euk
           ELSE !or
           ap_type(np,ng) = 2 !Syn
           ENDIF
          ENDIF
         ENDIF
        ENDIF

#if defined DARWIN_TWO_SPECIES_SETUP
        IF (np.EQ.1) ap_type(np,ng) = 10
        IF (np.EQ.2) ap_type(np,ng) = 10
#endif
#if defined DARWIN_NINE_SPECIES_SETUP
        IF (np.EQ.1) ap_type(np,ng) = 5
        IF (np.EQ.2) ap_type(np,ng) = 9
        IF (np.EQ.3) ap_type(np,ng) = 2
        IF (np.EQ.4) ap_type(np,ng) = 3
        IF (np.EQ.5) ap_type(np,ng) = 4
        IF (np.EQ.6) ap_type(np,ng) = 7
        IF (np.EQ.7) ap_type(np,ng) = 8
        IF (np.EQ.8) ap_type(np,ng) = 1
        IF (np.EQ.9) ap_type(np,ng) = 6
        ap_type(np,ng) = 10
#endif

        !print*,'ap_type(ng)',np,ap_type(np,ng)
        iopt = ap_type(np,ng)
        IF (1 .LE. iopt .AND. iopt .LE. nOpt) THEN
          DO l = 1, nlam
           aphy_chl(np,l,ng) = aphy_chl_type(iopt,l)
           aphy_chl_ps(np,l,ng) = aphy_chl_ps_type(iopt,l)
           bphy_mgC(np,l,ng) = bphy_mgC_type(iopt,l)
           bbphy_mgC(np,l,ng) = bbphy_mgC_type(iopt,l)
          ENDDO
        ELSE
          IF (Master) WRITE(out,'(A,2I4)')'invalid optical phyto type:',np,iopt
          exit_flag=5
          RETURN
        ENDIF

#else
! ANNA number of RandNo's carreid out MUST MATCH regardless of wavebands or not.
! ANNA the number of RandNo statements here MUST MATCH the number done above

! RandNo = gud_random(myThid)
! RandNo = gud_random(myThid)
! RandNo = gud_random(myThid)

#endif
! ANNA ENDIF


        CALL ran1(RandNoTemp)
#if defined DARWIN_VERBOSE_RAND
          write(*,'(a,i1.1,a,1x,f7.4,)') '>>>RandNoTemp(',np,'):', RandNoTemp
#endif
        CALL ran1(RandNoKsat)
#if defined DARWIN_VERBOSE_RAND
          write(*,'(a,i1.1,a,1x,f7.4,)') '>>>RandNoKsat(',np,'):', RandNoKsat
#endif
#if ! defined DARWIN_GEIDER
        IF(physize(np) .EQ. 1)then
           CALL gasdev(RandNoKsatPAR)
#if defined DARWIN_VERBOSE_RAND
             write(*,'(a,i1.1,a,1x,f7.4,)') '>>>RandNoKsatPAR(',np,'):', RandNoKsatPAR
#endif
           CALL gasdev(RandNoKinhPAR)
#if defined DARWIN_VERBOSE_RAND
             write(*,'(a,i1.1,a,1x,f7.4,)') '>>>RandNoKinhPAR(',np,'):', RandNoKinhPAR
#endif
        ELSE
! QQ remove someday
           CALL ran1(RandNoDummy)
#if defined DARWIN_VERBOSE_RAND
             write(*,'(a,i1.1,a,1x,f7.4,)') '>>>RandNoDummy(',np,'):', RandNoDummy
#endif
           CALL gasdev(RandNoKsatPAR)
#if defined DARWIN_VERBOSE_RAND
             write(*,'(a,i1.1,a,1x,f7.4,)') '>>>RandNoKsatPAR(',np,'):', RandNoKsatPAR
#endif
           CALL gasdev(RandNoKinhPAR)
#if defined DARWIN_VERBOSE_RAND
             write(*,'(a,i1.1,a,1x,f7.4,)') '>>>RandNoKinhPAR(',np,'):', RandNoKinhPAR
#endif
        ENDIF
#endif
#if defined DARWIN_GEIDER
        CALL ran1(RandNoGrowGeider)
#if defined DARWIN_VERBOSE_RAND
          write(*,'(a,i1.1,a,1x,f7.4,)') '>>>RandNoGrowGeider(',np,'):', RandNoGrowGeider
#endif
        CALL ran1(RandNoYield)
#if defined DARWIN_VERBOSE_RAND
          write(*,'(a,i1.1,a,1x,f7.4,)') '>>>RandNoYield(',np,'):', RandNoYield
#endif
        CALL ran1(RandNoChl2C)
#if defined DARWIN_VERBOSE_RAND
          write(*,'(a,i1.1,a,1x,f7.4,)') '>>>RandNoChl2C(',np,'):', RandNoChl2C
#endif
#endif

! ======================================================================

! size of phytoplankton
        IF(physize(np).EQ. 1)then
          dm = 10.0_r8 ! diameter (micrometer)
        ELSE
          dm = 1.0_r8 ! diameter (micrometer)
        ENDIF
! phytoplankton volume in_ micrometers cubed
        volp=4.0_r8/3.0_r8 *PI*(dm/2.0_r8)**3.0_r8
!
! common block variables (in_ m and m3)
        phyto_esd(np)=dm* 1.e-6_r8
        phyto_vol(np)=volp* 1.e-18_r8

! growth rates
! big/small phyto growth rates..
        IF(physize(np) .EQ. 1)then
          growthdays = Biggrow(ng) +RandNoGrow*Biggrowrange(ng)
        ELSE
          growthdays = Smallgrow(ng) +RandNoGrow*Smallgrowrange(ng)
        ENDIF
#if defined DARWIN_TWO_SPECIES_SETUP
        IF(physize(np) .EQ. 1)then
          growthdays = Biggrow(ng) 
        ELSE
          growthdays = Smallgrow(ng)
        ENDIF
#endif
#if defined DARWIN_NINE_SPECIES_SETUP
        IF(physize(np) .EQ. 1)then
          growthdays = Biggrow(ng)
        ELSE
          growthdays = Smallgrow(ng)
        ENDIF
#endif
! but diazotrophs always slower due to energetics
        IF(diazo(np,ng) .EQ. 1) then
            growthdays = growthdays * diaz_growfac(ng)
        ENDIF
! cocco have slower growth than other large
        IF (diacoc(np).EQ.2._r8) then
           growthdays= growthdays * cocco_growfac(ng)
        ENDIF
! diatom has faster thatn other large
        IF (diacoc(np).EQ.1._r8) then
           growthdays= growthdays * diatom_growfac(ng)
        ENDIF
! now convert to a growth rate
        IF (growthdays.GT.0._r8) then
         PCmax(np,ng) = 1.0_r8/(growthdays*pday)
        ELSE
         PCmax(np,ng) = 0.0_r8
        ENDIF

! mortality and export fraction rates
! big/small phyto mortality rates..
        IF(physize(np) .EQ. 1)then
          mortdays = Bigmort(ng) +RandnoMort*Bigmortrange(ng)
          ExportFracMort(np,ng)=Bigexport(ng)
          ExportFracMort2(np,ng)=Bigexport(ng)
#if defined DARWIN_EXUDE
          ExportFrac(np,ng)=Bigexport(ng)
#endif
        ELSE
          mortdays = Smallmort(ng) +RandNoMort*Smallmortrange(ng)
          ExportFracMort(np,ng)=Smallexport(ng)
          ExportFracMort2(np,ng)=Smallexport(ng)
#if defined DARWIN_EXUDE
          ExportFrac(np,ng)=Smallexport(ng)
#endif
        ENDIF
#if defined DARWIN_TWO_SPECIES_SETUP
        IF(physize(np) .EQ. 1)then
          mortdays = Bigmort(ng)
        ELSE
          mortdays = Smallmort(ng)
        ENDIF
#endif
#if defined DARWIN_NINE_SPECIES_SETUP
        IF(physize(np) .EQ. 1)then
          mortdays = Bigmort(ng)
        ELSE
          mortdays = Smallmort(ng)
        ENDIF
#endif

! now convert to a mortality rate
        IF (mortdays.GT.0._r8) then
          mort(np,ng) = 1.0_r8/(mortdays*pday)
        ELSE
          mort(np,ng) = 0.0_r8
        ENDIF
        mort2(np,ng) = 0.0_r8

! phytoplankton do not have temperature-dependent mortality
        tempMort(np,ng) = 0
        tempMort2(np,ng) = 0

        respiration(np,ng) = 0.0_r8


!..........................................................
! generate phyto Temperature Function parameters 
!.......................................................
        phytoTempCoeff(np,ng) = tempcoeff1(ng)
        phytoTempExp1(np,ng) = tempcoeff3(ng)
        IF(physize(np) .EQ. 1)then
          phytoTempExp2(np,ng) = tempcoeff2_big(ng)
        ELSE
          phytoTempExp2(np,ng) = tempcoeff2_small(ng)
        ENDIF

#if defined DARWIN_TEMP_RANGE
!swd phytoTempOptimum(np,ng) = 30.0_r8 - RandNo*28.0_r8 
        phytoTempOptimum(np,ng) = tempmax(ng) - RandNoTemp*temprange(ng)
        phytoDecayPower(np,ng) = tempdecay(ng)
#else
        phytoTempOptimum(np,ng) = 0.0_r8
        phytoDecayPower(np,ng) = 0.0_r8
#endif
        
! stoichiometric ratios for each functional group(ng) of phyto 
! relative to phosphorus - the base currency nutrient
! set Si:P
        IF(diacoc(np) .EQ. 1)then
          R_SiC(np,ng) = val_R_SiC_diatom(ng)
        ELSE
          R_SiC(np,ng) = 0.0_r8
        ENDIF
        IF(diacoc(np) .EQ. 2)then
          R_PICPOC(np,ng) = val_R_PICPOC(ng)
        ELSE
          R_PICPOC(np,ng) = 0.0_r8
        ENDIF
! set N:P and iron requirement according to diazotroph status
        IF(diazo(np,ng) .EQ. 1)then
          R_NC(np,ng) = val_R_NC_diaz(ng)
          R_FeC(np,ng) = val_R_FeC_diaz(ng)
        ELSE
          R_NC(np,ng) = val_R_NC(ng)
          R_FeC(np,ng) = val_R_FeC(ng)
        ENDIF
! set C:P ratio
          R_PC(np,ng) = val_R_PC(ng)
! set sinking rates according to allometry
        IF(physize(np) .EQ. 1)then
           wsink(np,ng) = BigSink(ng)
        ELSE 
           wsink(np,ng) = SmallSink(ng)
        ENDIF 
        wswim(np,ng) = 0.0_r8
! half-saturation coeffs 

        IF(physize(np) .EQ. 1)then
           ksatPO4(np,ng) = BigPsat(ng) + RandNoKsat*BigPsatrange(ng)
        ELSE
! ksatPO4(np,ng) = SmallPsat(ng) + RandNoKsat*SmallPsatrange(ng)
! if (nsource(np).LT.3) then
! ksatPO4(np,ng) = ksatPO4(np,ng)*prochlPsat
! ENDIF
           IF (nsource(np).EQ.3) then
             ksatPO4(np,ng) = SmallPsat(ng) + RandNoKsat*SmallPsatrange(ng)
           ENDIF
           IF (nsource(np).EQ..0) then
! ksatPO4(np,ng) = SmallPsat(ng) + RandNoKsat*SmallPsatrange(ng)
             ksatPO4(np,ng) = UniDzPsat(ng) + RandNoKsat*UniDzPsatrange(ng) 
           ENDIF
           IF (nsource(np).EQ.2.or.nsource(np).EQ.1) then
             ksatPO4(np,ng) = ProcPsat(ng) + RandNoKsat*ProcPsatrange(ng)
           ENDIF
        ENDIF
        IF (diacoc(np) .EQ. 2) THEN
           ksatPO4(np,ng) = CoccoPsat(ng) + RandNoKsat*CoccoPsatrange(ng)
        ENDIF
#if defined DARWIN_TWO_SPECIES_SETUP
        IF(physize(np) .EQ. 1)then
           ksatPO4(np,ng) = BigPsat(ng) 
        ELSE
           ksatPO4(np,ng) = SmallPsat(ng)
        ENDIF
#endif
#if defined DARWIN_NINE_SPECIES_SETUP
        IF(physize(np) .EQ. 1)then
           ksatPO4(np,ng) = BigPsat(ng)
        ELSE
           ksatPO4(np,ng) = SmallPsat(ng)
        ENDIF
        IF (nsource(np).EQ.2.or.nsource(np).EQ.1) then
           ksatPO4(np,ng) = ProcPsat(ng) 
        ENDIF
        IF (diacoc(np) .EQ. 2) then
           ksatPO4(np,ng) = ksatPO4(np,ng)/0.8_r8 
        ENDIF
#endif

        ksatNO3(np,ng) = ksatPO4(np,ng)*R_NC(np,ng)/R_PC(np,ng)
        ksatNO2(np,ng) = ksatNO3(np,ng)*ksatNO2fac(ng) 
! Made ksatNH4(ng) smaller since it is the preferred source
        ksatNH4(np,ng) = ksatNO3(np,ng)*ksatNH4fac(ng)
        ksatFeT(np,ng) = ksatPO4(np,ng)*R_FeC(np,ng)/R_PC(np,ng)
        ksatSiO2(np,ng) = val_ksatsio2(ng)
        amminhib(np,ng) = val_amminhib(ng)

        acclimtimescl(np,ng) = val_acclimtimescl(ng)

#if ! defined DARWIN_GEIDER
        R_ChlC(np,ng) = val_R_ChlC(ng)

!NEW Light parameters:
! ksatPAR(ng) {0.1_r8 - 1.3_r8}
! 0.35_r8=Av High Light Adapted, 0.8_r8=Av Low Light Adapted
! kinhPAR(ng) {0.0_r8 - 3.0_r8}
! 0.5_r8 =Av High Light Adapted, 2.0_r8=Av Low Light Adapted
! High Light Groups for Large size:
        IF(physize(np) .EQ. 1)then
           ksatPAR(np,ng) = abs(Bigksatpar(ng)+Bigksatparstd(ng)*RandNoKsatPAR)

           kinhPAR(np,ng) = abs(Bigkinhpar(ng)+Bigkinhparstd(ng)*RandNoKinhPAR)
        ELSE
! QQ remove someday
! Low Light Groups for Small size:
           ksatPAR(np,ng) = abs(smallksatpar(ng)+smallksatparstd(ng)*RandNoKsatPAR)

           kinhPAR(np,ng) = abs(smallkinhpar(ng)+smallkinhparstd(ng)*RandNoKinhPAR)
        ENDIF
#if defined DARWIN_TWO_SPECIES_SETUP
        IF(physize(np) .EQ. 1)then
           ksatPAR(np,ng) = abs(Bigksatpar(ng))
           kinhPAR(np,ng) = abs(Bigkinhpar(ng))
        ELSE
           ksatPAR(np,ng) = abs(smallksatpar(ng))
           kinhPAR(np,ng) = abs(smallkinhpar(ng))
        ENDIF
#endif
#if defined DARWIN_NINE_SPECIES_SETUP
        IF(physize(np) .EQ. 1)then
           ksatPAR(np,ng) = abs(Bigksatpar(ng))
           kinhPAR(np,ng) = abs(Bigkinhpar(ng))
        ELSE
           ksatPAR(np,ng) = abs(smallksatpar(ng))
           kinhPAR(np,ng) = abs(smallkinhpar(ng))
        ENDIF
        IF (np.EQ.5) then
          kinhPAR(np,ng) = abs(LLProkinhpar(ng))
        ENDIF
        IF (np.EQ.9) then
          kinhPAR(np,ng) = abs(Coccokinhpar(ng))
        ENDIF
#endif
#endif

#if defined DARWIN_GEIDER
! big/small phyto growth rates..
        IF(physize(np) .EQ. 1)then
          growthdays = Biggrow(ng) +RandNoGrowGeider*Biggrowrange(ng)
        ELSE
          growthdays = Smallgrow(ng) +RandNoGrowGeider*Smallgrowrange(ng)
        ENDIF
#if defined DARWIN_TWO_SPECIES_SETUP
        IF(physize(np) .EQ. 1)then
          growthdays = Biggrow(ng) 
        ELSE
          growthdays = Smallgrow(ng) 
        ENDIF
#endif
#if defined DARWIN_NINE_SPECIES_SETUP
        IF(physize(np) .EQ. 1)then
          growthdays = Biggrow(ng)
        ELSE
          growthdays = Smallgrow(ng)
        ENDIF
#endif
! but diazotrophs always slower due to energetics
        IF(diazo(np,ng) .EQ. 1) then
            growthdays = growthdays * diaz_growfac(ng)
        ENDIF
! cocco have slower growth than other large
        IF (diacoc(np).EQ.2._r8) then
           growthdays= growthdays * cocco_growfac(ng)
        ENDIF
! diatom has faster thatn other large
        IF (diacoc(np).EQ.1._r8) then
           growthdays= growthdays * diatom_growfac(ng)
        ENDIF
! now convert to a growth rate
        IF (growthdays.GT.0._r8) then
         PCmax(np,ng) = 1.0_r8/(growthdays*pday)
        ELSE
         PCmax(np,ng) = 0.0_r8
        ENDIF
!
! photo-inhibition 
#if defined DARWIN_RADTRANS
! only LL Pro are inhibited
         IF (ap_type(np,ng).EQ.4) then
            inhibcoef_geid(np,ng) = inhibcoef_geid_val(ng)
         ELSE
            inhibcoef_geid(np,ng) = 0.0_r8
         ENDIF
#else
! no inhibition
         IF(physize(np) .EQ. 1)then
           inhibcoef_geid(np,ng) = 0.0_r8
         ELSE
           inhibcoef_geid(np,ng) = 0.0_r8 !inhibcoef_geid_val(ng)
         ENDIF
#endif
!

! big/small phyto PI slope (chl specific)
! IF(physize(np) .EQ. 1)then
! alphachl(np,ng) = Bigalphachl +Randno*Bigalphachlrange
! ELSE
! alphachl(np,ng) = Smallalphachl +RandNo*Smallalphachlrange
! ENDIF

! ANNA gieder via mQyield(ng) instead of alpha
! big/small phyto Maximum Quantum yield(ng)
        IF(physize(np) .EQ. 1)then
          mQyield(np,ng) = BigmQyield(ng) +RandNoYield*BigmQyieldrange(ng)
        ELSE
          mQyield(np,ng) = smallmQyield(ng) +RandNoYield*smallmQyieldrange(ng)
        ENDIF
#if defined DARWIN_TWO_SPECIES_SETUP
        IF(physize(np) .EQ. 1)then
          mQyield(np,ng) = BigmQyield(ng)
        ELSE
          mQyield(np,ng) = smallmQyield(ng)
        ENDIF
#endif
#if defined DARWIN_NINE_SPECIES_SETUP
        IF(physize(np) .EQ. 1)then
          mQyield(np,ng) = BigmQyield(ng)
        ELSE
          mQyield(np,ng) = smallmQyield(ng)
        ENDIF
#endif
#if defined DARWIN_RADTRANS
! ANNA for wavebands only, re-set mQyield(ng) to be constant for all np's
! ANNA i.e. let alpha vary only with aphy_chl_ps(ng)
! ANNA value is mean of vals for big and small.
          mQyield(np,ng) = 4.0e-5_r8
#endif


! big/small phyto C:Chl max
        IF(physize(np) .EQ. 1)then
          chl2cmax(np,ng) = Bigchl2cmax(ng) +RandnoChl2C*Bigchl2cmaxrange(ng)
        ELSE
          chl2cmax(np,ng) = smallchl2cmax(ng) +RandNoChl2C*smallchl2cmaxrange(ng)
        ENDIF
#if defined DARWIN_TWO_SPECIES_SETUP
        IF(physize(np) .EQ. 1)then
          chl2cmax(np,ng) = Bigchl2cmax(ng) 
        ELSE
          chl2cmax(np,ng) = smallchl2cmax(ng) 
        ENDIF
#endif
#if defined DARWIN_NINE_SPECIES_SETUP
        IF(physize(np) .EQ. 1)then
          chl2cmax(np,ng) = Bigchl2cmax(ng)
        ELSE
          chl2cmax(np,ng) = smallchl2cmax(ng)
        ENDIF
#endif
! ANNA chl2cmin(ng) added
! chl2cmin(np,ng) = 0.003_r8 * 12.0_r8 ! mg Chl a/mmol C

#endif /* DARWIN_GEIDER */

        !print*,'nsource',np,nsource(np)
        IF (nsource(np) .EQ. 3) THEN
          useNH4(np,ng) = 1
          useNO2(np,ng) = 1
          useNO3(np,ng) = 1
          combNO(np,ng) = 1
        ELSEIF (nsource(np) .EQ. 2) THEN
          useNH4(np,ng) = 1
          useNO2(np,ng) = 0
          useNO3(np,ng) = 0
          combNO(np,ng) = 0
        ELSEIF (nsource(np) .EQ. 1) THEN
          useNH4(np,ng) = 1
          useNO2(np,ng) = 1
          useNO3(np,ng) = 0
          combNO(np,ng) = 0
        ELSE
          useNH4(np,ng) = 0
          useNO2(np,ng) = 0
          useNO3(np,ng) = 0
          combNO(np,ng) = 0
        ENDIF

        IF (diacoc(np) .NE. 1) THEN
          ksatSiO2(np,ng) = 0.0_r8
          Vmax_SiO2(np,ng) = 0.0_r8
          R_SiC(np,ng) = 0.0_r8
        ENDIF

        IF (diacoc(np) .EQ. 1) THEN
          hasSi(np,ng) = 1
          hasPIC(np,ng) = 0
        ELSEIF (diacoc(np) .EQ. 2) THEN
          hasSi(np,ng) = 0
          hasPIC(np,ng) = 1
        ELSE
          hasSi(np,ng) = 0
          hasPIC(np,ng) = 0
        ENDIF

        ENDDO ! np

        DO np = 1, nplank

          bactType(np,ng) = 0
          isAerobic(np,ng) = 0
          isDenit(np,ng) = 0

          yield(np,ng) = 1.0_r8
          yieldO2(np,ng) = 1.0_r8
          yieldNO3(np,ng) = 1.0_r8

          ksatPON(np,ng) = 1.0_r8
          ksatPOC(np,ng) = 1.0_r8
          ksatPOP(np,ng) = 1.0_r8
          ksatPOFe(np,ng) = 1.0_r8
          ksatDON(np,ng) = 1.0_r8
          ksatDOC(np,ng) = 1.0_r8
          ksatDOP(np,ng) = 1.0_r8
          ksatDOFe(np,ng) = 1.0_r8

        ENDDO ! np

#if defined DARWIN_RADTRANS
        DO np = nPhoto + 1, nplank
          DO l = 1, nlam
           bphy_mgC(np,l,ng) = 0.0_r8
           bbphy_mgC(np,l,ng) = 0.0_r8
          ENDDO
        ENDDO
#endif


! ======================================================================
! zooplankton
! ======================================================================

        DO nz = 1, nplank
         DO np = 1, nplank
          palat(np,nz,ng) = 0.0_r8
          asseff(np,nz,ng) = 0.0_r8
          ExportFracPreyPred(np,nz,ng) = 0.0_r8
         ENDDO
        ENDDO

        IF ( oldTwoGrazers(ng) ) THEN
! assume zoo(1) = small, zoo(2) = big

         IF ( iMaxPred-iMinPred .NE. 1 ) THEN
          IF (Master) WRITE(out,'(2A)') 'DARWIN_GENERATE: ','must have exactly 2 predators when oldTwoGrazers(ng)=.TRUE.'
          exit_flag=5
          RETURN
         ENDIF
         physize(iMinPred) = 0
         physize(iMaxPred) = 1
         grazemax(iMinPred,ng) = GrazeFast(ng)
         grazemax(iMaxPred,ng) = GrazeFast(ng)
         ExportFracMort(iMinPred,ng) = ZooexfacSmall(ng)
         ExportFracMort(iMaxPred,ng) = ZooexfacBig(ng)
         ExportFracMort2(iMinPred,ng) = ZooexfacSmall(ng)
         ExportFracMort2(iMaxPred,ng) = ZooexfacBig(ng)
#if defined DARWIN_EXUDE
         ExportFrac(iMinPred,ng) = ZooexfacSmall(ng)
         ExportFrac(iMaxPred,ng) = ZooexfacBig(ng)
#endif
         mort(iMinPred,ng) = ZoomortSmall(ng)
         mort(iMaxPred,ng) = ZoomortBig(ng)
         mort2(iMinPred,ng) = ZoomortSmall2(ng)
         mort2(iMaxPred,ng) = ZoomortBig2(ng)
         DO np = iMinPrey, iMaxPrey
          ExportFracPreyPred(np,iMinPred,ng) = ExGrazfracsmall(ng)
          ExportFracPreyPred(np,iMaxPred,ng) = ExGrazfracbig(ng)
         ENDDO
         dmzoo(iMinPred) = 30.0_r8 ! diameter (micrometer)
         dmzoo(iMaxPred) = 300.0_r8 ! diameter (micrometer)
! palatibity according to "allometry"
! big grazers preferentially eat big phyto etc...
         DO nz = iMinPred, iMaxPred
          DO np = iMinPrey, iMaxPrey
            IF (physize(nz).EQ.physize(np)) then
              palat(np,nz,ng) = palathi(ng)
              asseff(np,nz,ng) = GrazeEffmod(ng)
            ELSE
              palat(np,nz,ng) = palatlo(ng)
              IF (physize(np).EQ.0._r8) then
                asseff(np,nz,ng) = GrazeEffhi(ng)
              ELSE
                asseff(np,nz,ng) = GrazeEfflow(ng)
              ENDIF
            ENDIF
! diatoms even less palatible
            IF (diacoc(np).EQ.1._r8) then
              palat(np,nz,ng)= palat(np,nz,ng)*diatomgraz(ng)
            ENDIF
! coccolithophes less palatible
            IF (diacoc(np).EQ.2._r8) then
              palat(np,nz,ng)= palat(np,nz,ng)*coccograz(ng)
            ENDIF
! other large phyto less palatible
            IF (diacoc(np).EQ.0._r8 .AND.physize(np).EQ.1._r8) then
              palat(np,nz,ng)= palat(np,nz,ng)*olargegraz(ng)
            ENDIF
! need something in_ here for tricho
          ENDDO
         ENDDO

! not oldTwoGrazers(ng)
        ELSE

         DO nz = iMinPred, iMaxPred
          grazemax(nz,ng) = GrazeRate(ng)
          ExportFracMort(nz,ng) = Zooexfac(ng)
          ExportFracMort2(nz,ng) = Zooexfac(ng)
#if defined DARWIN_EXUDE
          ExportFrac(nz,ng) = Zooexfac(ng)
#endif
          mort(nz,ng) = Zoomort(ng)
          mort2(nz,ng) = Zoomort2(ng)
          dmzoo(nz) = ZooDM(ng)
          DO np = iMinPrey, iMaxPrey
           palat(np,nz,ng) = val_palat(ng)
           asseff(np,nz,ng) = val_ass_eff(ng)
           ExportFracPreyPred(np,nz,ng) = ExGrazfrac(ng)
          ENDDO
         ENDDO

! oldTwoGrazers(ng)
        ENDIF

!
        DO nz = iMinPred, iMaxPred
          R_NC(nz,ng) = val_R_NC_zoo(ng)
          R_PC(nz,ng) = val_R_PC_zoo(ng)
          R_SiC(nz,ng) = val_R_SiC_zoo(ng)
          R_FeC(nz,ng) = val_R_FeC_zoo(ng)
          R_ChlC(nz,ng) = val_R_ChlC_zoo(ng)
          R_PICPOC(nz,ng) = val_R_PICPOC_zoo(ng)

          Xmin(nz,ng) = 0.0_r8

          kgrazesat(nz,ng) = kgrazesat_val(ng)
! zooplankton do have temperature-dependent mortality
          tempMort(nz,ng) = 1
          tempMort2(nz,ng) = 1

          respiration(nz,ng) = 0.0_r8
          wsink(nz,ng) = 0.0_r8
          wswim(nz,ng) = 0.0_r8

! zooplankton volume in_ micrometers cubed
          volp = 4.0_r8/3.0_r8 *PI*(dmzoo(nz)/2.0_r8)**3.0_r8
!
! common block variables (in_ m and m3)
          phyto_esd(nz) = dmzoo(nz)* 1.e-6_r8
          phyto_vol(nz) = volp* 1.e-18_r8
        ENDDO


