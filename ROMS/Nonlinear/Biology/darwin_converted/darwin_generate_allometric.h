!
!  converted from "gud_generate_allometric.F" using darwinfortran_to_romsfortran
!  (written by J. Paul Mattern)
!
! compute cell volumes in_ micrometer^3
!
! in_ decreasing precedence (if bold quantity is set):
!
! V = grp_biovol(ic,ig,ng)
! V = 10**(logvolbase(ng)+(GRP_BIOVOLIND(ic,ig)-1)*logvolinc(ng))
! V = 10**(logvolbase(ng)+(logvol0ind(ig,ng)+ic-2)*logvolinc(ng))
! V = biovol0(ig,ng)*biovolfac(ig,ng)**(ic-1)
!
! if logvol0ind(ng) is set, use it to compute biovol0(ng)
        DO ig=1,nGroup
          IF (logvol0ind(ig,ng) .GT. 0) THEN
            IF (biovol0(ig,ng) .NE. 0.0_r8) THEN
              IF (Master) WRITE(out,'(2A)')                             &
     &          'DARWIN_GENERATE_ALLOMETRIC: ',                         &
              'cannot set both biovol0(ng) and logvol0ind(ng)'
              exit_flag=5
              RETURN
            ENDIF
            logvol = logvolbase(ng) + (logvol0ind(ig,ng)-1)*logvolinc(ng)
            biovol0(ig,ng) = 10.0_r8 ** logvol
            IF (biovolfac(ig,ng).gt.0) THEN
              ! JPM 2022-08-26: added this to permit biovolfac to allow
              ! different spacing for different groups
              biovolfac(ig,ng) = 10.0_r8 **                             &
     &                           (logvolinc(ng)*biovolfac(ig,ng))
            ELSE
              biovolfac(ig,ng) = 10.0_r8 ** logvolinc(ng)
            ENDIF
          ENDIF
        ENDDO

        DO ig=1,nGroup
         DO ip=1,grp_nplank(ig,ng)
          IF (grp_logvolind(ip,ig,ng) .GT. 0 .AND.grp_biovol(ip,ig,ng) .GT. 0.0_r8) THEN
            IF (Master) WRITE(out,'(2A)') 'DARWIN_GENERATE_ALLOMETRIC:',&
     &        ' cannot set both grp_biovol(ng) and grp_logvolind(ng)'
            exit_flag=5
            RETURN
          ELSEIF (grp_logvolind(ip,ig,ng) .GT. 0) THEN
! pick from logrange
            logvol = logvolbase(ng) + (grp_logvolind(ip,ig,ng)-1)*logvolinc(ng)
            grp_biovol(ip,ig,ng) = 10.0_r8 ** logvol
          ELSEIF (grp_biovol(ip,ig,ng) .EQ. 0) THEN
            IF (biovol0(ig,ng) .GT. 0.0_r8) THEN
             grp_biovol(ip,ig,ng) = biovol0(ig,ng) * biovolfac(ig,ng)**(ip-1)
            ELSE
              IF (Master) WRITE(out,'(3A)')                             &
     &          'DARWIN_GENERATE_ALLOMETRIC: ',                         &
     &          'Need to set one of grp_biovol(ng), grp_logvolind(ng),  &
     &','biovol0(ng), logvol0ind(ng)'
              IF (Master) THEN
                WRITE(out,'(A,I3,A,I3,A)')                              &
                'Error occurred at ip=',ip,' ig=',ig,'.'
              ENDIF
             exit_flag=5
             RETURN
            ENDIF
          ENDIF
          biovol_bygroup(ip,ig,ng) = grp_biovol(ip,ig,ng)
         ENDDO
         DO ip=grp_nplank(ig,ng)+1,nplank
          IF (grp_biovol(ip,ig,ng) .NE. 0.0_r8) THEN
            IF (Master) WRITE(out,'(2A,I5,A,I5,A)')                     &
     &        'DARWIN_GENERATE_ALLOMETRIC: ','index',ip,'group(ng)',ig, &
     &        'out of range for grp_biovol(ng)'
            exit_flag=5
            RETURN
          ENDIF
          IF (grp_logvolind(ip,ig,ng) .NE. 0) THEN
            IF (Master) WRITE(out,'(2A,I5,A,I5,A)')                     &
     &        'DARWIN_GENERATE_ALLOMETRIC: ','index',ip,'group(ng)',ig, &
     &        'out of range for grp_logvolind(ng)'
            exit_flag=5
            RETURN
          ENDIF
          biovol_bygroup(ip,ig,ng) = 0.0_r8
         ENDDO
        ENDDO

! set up single list of plankton types from groups
! type order is either by group(ng) or, if gud_sort_biovol(ng), by biovol(ng).

        IF (gud_sort_biovol(ng)) THEN
! sort by volume

         DO ig=1,nGroup
          jpg(ig) = 1
         ENDDO
         DO ip=1,nplank
          volmin = 1e38_r8
          gmin = 0
! first check phototrophs
          DO ig=1,ngroup
           IF (grp_photo(ig,ng).NE.0) THEN
            vol = grp_biovol(jpg(ig),ig,ng)
            IF (jpg(ig).LE.grp_nplank(ig,ng) .AND. vol.LT.volmin) THEN
             gmin = ig
             volmin = vol
            ENDIF
           ENDIF
          ENDDO
! then pure heterotrophs
          IF (gmin.EQ.0) THEN
           DO ig=1,ngroup
            IF (grp_photo(ig,ng).EQ.0) THEN
             vol = grp_biovol(jpg(ig),ig,ng)
             IF (jpg(ig).LE.grp_nplank(ig,ng) .AND. vol.LT.volmin) THEN
              gmin = ig
              volmin = vol
             ENDIF
            ENDIF
           ENDDO
          ENDIF
          IF (gmin.EQ.0) THEN
           IF (Master) WRITE(out,'(2a)') 'Error:',  'gmin'
           exit_flag=5
           RETURN
          ENDIF
          group(ip,ng) = gmin
          biovol(ip,ng) = volmin
          igroup(ip) = jpg(gmin)
          jpg(gmin) = jpg(gmin) + 1
         ENDDO
         DO ig=1,ngroup
          IF (jpg(ig).NE.grp_nplank(ig,ng)+1) THEN
           IF (Master) WRITE(out,'(2a)') 'Error:',  'grp_nplank(ng)'
           exit_flag=5
           RETURN
          ENDIF
         ENDDO

        ELSE
! sort by group(ng)

         ip = 1
         DO ig=1,ngroup
          DO ip2=1,grp_nplank(ig,ng)
           IF (ip .GT. nPlank) THEN
             IF (Master) WRITE(out,'(2A)')                              &
     &         'DARWIN_GENERATE_ALLOMETRIC: ',                          &
     &         'need SUM(grp_nplank(ng)) = nPlank, nPlank too small'
            exit_flag=5
            RETURN
           ENDIF
           group(ip,ng) = ig
           igroup(ip) = ip2
           biovol(ip,ng) = grp_biovol(ip2,ig,ng)
           ip = ip + 1
          ENDDO
         ENDDO
         IF (ip .NE. nPlank + 1) THEN
           IF (Master) WRITE(out,'(2A,2I4)')                            &
     &       'DARWIN_GENERATE_ALLOMETRIC: ',                            &
     &       'need SUM(grp_nplank(ng)) = nPlank, not ',ip-1,nPlank
          exit_flag=5
          RETURN
         ENDIF

! endif sort order
        ENDIF

! ======================================================================
! compute traits from trait parameters
        DO ip=1,nplank
          ig = group(ip,ng)

! ----------------------------------------------------------------------
! non-allometric traits (same within group(ng))

          isPhoto(ip,ng) = grp_photo(ig,ng)
          hasSi(ip,ng) = grp_hasSi(ig,ng)
          hasPIC(ip,ng) = grp_hasPIC(ig,ng)
          diazo(ip,ng) = grp_diazo(ig,ng)
          useNH4(ip,ng) = grp_useNH4(ig,ng)
          useNO2(ip,ng) = grp_useNO2(ig,ng)
          useNO3(ip,ng) = grp_useNO3(ig,ng)
          combNO(ip,ng) = grp_combNO(ig,ng)
          tempMort(ip,ng) = grp_tempMort(ig,ng)
          tempMort2(ip,ng) = grp_tempMort2(ig,ng)

          Xmin(ip,ng) = grp_Xmin(ig,ng)
          amminhib(ip,ng) = grp_amminhib(ig,ng)
          acclimtimescl(ip,ng) = grp_acclimtimescl(ig,ng)

! mortality
          mort(ip,ng) = grp_mort(ig,ng)
          mort2(ip,ng) = grp_mort2(ig,ng)
! if 0, temperature dependent, if 1, not.
          ExportFracMort(ip,ng) = grp_ExportFracMort(ig,ng)
          ExportFracMort2(ip,ng) = grp_ExportFracMort2(ig,ng)
          ExportFrac(ip,ng) = grp_ExportFrac(ig,ng)

! temperature function parameters
          phytoTempCoeff(ip,ng) = grp_tempcoeff1(ig,ng)
          phytoTempExp1(ip,ng) = grp_tempcoeff3(ig,ng)
          phytoTempExp2(ip,ng) = grp_tempcoeff2(ig,ng)
          phytoTempOptimum(ip,ng) = grp_tempopt(ig,ng)
          phytoDecayPower(ip,ng) = grp_tempdecay(ig,ng)

! plankton elemental ratios
          R_NC(ip,ng) = grp_R_NC(ig,ng)
          R_PC(ip,ng) = grp_R_PC(ig,ng)
          R_SiC(ip,ng) = grp_R_SiC(ig,ng)
          R_FeC(ip,ng) = grp_R_FeC(ig,ng)
          R_ChlC(ip,ng) = grp_R_ChlC(ig,ng)
          R_PICPOC(ip,ng) = grp_R_PICPOC(ig,ng)

! plankton sinking and swimming
          wsink(ip,ng) = a_biosink(ig,ng) * biovol(ip,ng)**b_biosink(ig,ng)
          wswim(ip,ng) = a_bioswim(ig,ng) * biovol(ip,ng)**b_bioswim(ig,ng)

! respiration(ng) rate is given in_ terms of carbon content
          qcarbon(ip) = a_qcarbon(ig,ng) * biovol(ip,ng)**b_qcarbon(ig,ng)
          respiration(ip,ng) = a_respir(ig,ng)* (12.e9_r8 * qcarbon(ip))**b_respir(ig,ng)/ qcarbon(ip)

! parameters relating to inorganic nutrients
          PCmax(ip,ng)= a_vmax_DIC(ig,ng) * biovol(ip,ng)**b_vmax_DIC(ig,ng)
#if defined DARWIN_VERBOSE_PLANK
          if(Master) then
            write(out,'(a,i2.2,6(a,f16.6))')                            &
     &        'plankton',ip,                                            &
     &        ': PCmax=   ',PCmax(ip,ng),                               &
     &        ', a_vmax_DIC=',a_vmax_DIC(ig,ng),                        &
     &        ', biovol=',biovol(ip,ng),                                &
     &        ', b_vmax_DIC=',b_vmax_DIC(ig,ng)
          end if
#endif

          Vmax_NH4(ip,ng) = a_vmax_NH4(ig,ng) * biovol(ip,ng)**b_vmax_NH4(ig,ng)
          Vmax_NO2(ip,ng) = a_vmax_NO2(ig,ng) * biovol(ip,ng)**b_vmax_NO2(ig,ng)
          Vmax_NO3(ip,ng) = a_vmax_NO3(ig,ng) * biovol(ip,ng)**b_vmax_NO3(ig,ng)
#if defined DARWIN_VERBOSE_PLANK
          if(Master) then
            write(out,'(a,i2.2,6(a,f12.6))')                            &
     &        'plankton',ip,                                            &
     &        ': Vmax_NO3=',Vmax_NO3(ip,ng),                            &
     &        ', a_vmax_NO3=',a_vmax_NO3(ig,ng),                        &
     &        ', biovol=',biovol(ip,ng),                                &
     &        ', b_vmax_NO3=',b_vmax_NO3(ig,ng)
          end if
#endif
          Vmax_N(ip,ng) = a_vmax_N(ig,ng) * biovol(ip,ng)**b_vmax_N(ig,ng)
          Vmax_PO4(ip,ng) = a_vmax_PO4(ig,ng) * biovol(ip,ng)**b_vmax_PO4(ig,ng)
          Vmax_SiO2(ip,ng) = a_vmax_SiO2(ig,ng) * biovol(ip,ng)**b_vmax_SiO2(ig,ng)
          Vmax_FeT(ip,ng) = a_vmax_FeT(ig,ng) * biovol(ip,ng)**b_vmax_FeT(ig,ng)

          Qnmin(ip,ng) = a_qmin_n(ig,ng) * biovol(ip,ng)**b_qmin_n(ig,ng)
          Qnmax(ip,ng) = a_qmax_n(ig,ng) * biovol(ip,ng)**b_qmax_n(ig,ng)

          Qpmin(ip,ng) = a_qmin_p(ig,ng) * biovol(ip,ng)**b_qmin_p(ig,ng)
          Qpmax(ip,ng) = a_qmax_p(ig,ng) * biovol(ip,ng)**b_qmax_p(ig,ng)

          Qsimin(ip,ng) = a_qmin_si(ig,ng) * biovol(ip,ng)**b_qmin_si(ig,ng)
          Qsimax(ip,ng) = a_qmax_si(ig,ng) * biovol(ip,ng)**b_qmax_si(ig,ng)

          Qfemin(ip,ng) = a_qmin_fe(ig,ng) * biovol(ip,ng)**b_qmin_fe(ig,ng)
          Qfemax(ip,ng) = a_qmax_fe(ig,ng) * biovol(ip,ng)**b_qmax_fe(ig,ng)

          ksatNH4(ip,ng) = a_kn_NH4(ig,ng) * biovol(ip,ng)**b_kn_NH4(ig,ng)
          ksatNO2(ip,ng) = a_kn_NO2(ig,ng) * biovol(ip,ng)**b_kn_NO2(ig,ng)
          ksatNO3(ip,ng) = a_kn_NO3(ig,ng) * biovol(ip,ng)**b_kn_NO3(ig,ng)
          ksatPO4(ip,ng) = a_kn_PO4(ig,ng) * biovol(ip,ng)**b_kn_PO4(ig,ng)
          ksatSiO2(ip,ng) = a_kn_SiO2(ig,ng) * biovol(ip,ng)**b_kn_SiO2(ig,ng)
          ksatFeT(ip,ng) = a_kn_feT(ig,ng) * biovol(ip,ng)**b_kn_FeT(ig,ng)

! parameters relating to quota nutrients
! EXCRETION
          kexcC(ip,ng) = a_kexc_c(ig,ng) * biovol(ip,ng)**b_kexc_c(ig,ng)
          kexcN(ip,ng) = a_kexc_n(ig,ng) * biovol(ip,ng)**b_kexc_n(ig,ng)
          kexcP(ip,ng) = a_kexc_p(ig,ng) * biovol(ip,ng)**b_kexc_p(ig,ng)
          kexcSi(ip,ng) = a_kexc_si(ig,ng) * biovol(ip,ng)**b_kexc_si(ig,ng)
          kexcFe(ip,ng) = a_kexc_fe(ig,ng) * biovol(ip,ng)**b_kexc_fe(ig,ng)

          IF (GUD_effective_ksat(ng)) THEN
! compute effective half sat for uptake of non-quota elements
! we compute it for NO3 and scale for others
           IF (gud_select_kn_allom(ng).EQ.1) THEN
! following Ward et al.
             kappa=(ksatNO3(ip,ng)*PCmax(ip,ng)*Qnmin(ip,ng)*(Qnmax(ip, &
     &         ng)-Qnmin(ip,ng)))/(Vmax_NO3(ip,ng)*Qnmax(ip,ng) +       &
     &         PCmax(ip,ng)*Qnmin(ip,ng)*(Qnmax(ip,ng)-Qnmin(ip,ng)))
           ELSEIF (gud_select_kn_allom(ng).EQ.2) THEN
! following Follett et al.
            kappa = (ksatNO3(ip,ng)*PCmax(ip,ng)*Qnmin(ip,ng))/Vmax_NO3(ip,ng)
           ELSE
             IF (Master) WRITE(out,'(2A)')'DARWIN_GENERATE_ALLOMETRIC:  &
     &','illegal value for gud_select_kn_allom(ng)'
            exit_flag=5
            RETURN
           ENDIF
#if ! defined DARWIN_NQUOTA
           ksatNO3(ip,ng) = kappa
           ksatNO2(ip,ng) = kappa*grp_ksatNO2fac(ig,ng)
           ksatNH4(ip,ng) = kappa*grp_ksatNH4fac(ig,ng)
#endif
#if ! defined DARWIN_PQUOTA
           ksatPO4(ip,ng) = kappa/R_NC(ip,ng)*R_PC(ip,ng)
#endif
#if ! defined DARWIN_SIQUOTA
           ksatSiO2(ip,ng) = kappa/R_NC(ip,ng)*R_SiC(ip,ng)
#endif
#if ! defined DARWIN_FEQUOTA
           ksatFeT(ip,ng) = kappa/R_NC(ip,ng)*R_FeC(ip,ng)
#endif
          ENDIF

! parameters for bacteria

          bactType(ip,ng) = grp_bacttype(ig,ng)
          isAerobic(ip,ng) = grp_aerobic(ig,ng)
          isDenit(ip,ng) = grp_denit(ig,ng)

          yieldO2(ip,ng) = 1.0_r8
          yieldNO3(ip,ng) = 1.0_r8
          IF (isAerobic(ip,ng) .NE. 0) THEN
            yield(ip,ng) = yod(ng)
            yieldO2(ip,ng) = yoe(ng)
          ELSEIF (isDenit(ip,ng) .NE. 0) THEN
            yield(ip,ng) = ynd(ng)
            yieldNO3(ip,ng) = yne(ng)
          ENDIF

          ksatPON(ip,ng) = ksatPOM(ng)
          ksatDON(ip,ng) = ksatDOM(ng)
          ksatPOC(ip,ng) = ksatPON(ip,ng)/R_NC(ip,ng)
          ksatPOP(ip,ng) = ksatPON(ip,ng)/R_NC(ip,ng)*R_PC(ip,ng)
          ksatPOFe(ip,ng) = ksatPON(ip,ng)/R_NC(ip,ng)*R_FeC(ip,ng)
          ksatDOC(ip,ng) = ksatDON(ip,ng)/R_NC(ip,ng)
          ksatDOP(ip,ng) = ksatDON(ip,ng)/R_NC(ip,ng)*R_PC(ip,ng)
          ksatDOFe(ip,ng) = ksatDON(ip,ng)/R_NC(ip,ng)*R_FeC(ip,ng)

#if defined DARWIN_GEIDER
          mQyield(ip,ng) = grp_mQyield(ig,ng)
          chl2cmax(ip,ng) = grp_chl2cmax(ig,ng)
          inhibcoef_geid(ip,ng) = grp_inhibcoef_geid(ig,ng)
#else
          ksatPAR(ip,ng) = grp_ksatPAR(ig,ng)
          kinhPAR(ip,ng) = grp_kinhPAR(ig,ng)
#endif /* DARWIN_GEIDER */

#if defined DARWIN_RADTRANS
          ap_type(ip,ng) = grp_aptype(ig,ng)
          iopt = ap_type(ip,ng)
          IF (1 .LE. iopt .AND. iopt .LE. nOpt) THEN
           IF (gud_allomSpectra(ng)) THEN

! FOR ABSORPTION
! in_ terms of volume
             volmeas = PI / 6.0_r8 * asize(iopt)**3
             scalefac = (biovol(ip,ng)/volmeas)**gud_absorpSlope(ng)
! size specific absorption spectra
             DO l = 1, nlam
               aphy_chl(ip,l,ng) = aphy_chl_type(iopt,l)*scalefac
               aphy_chl_ps(ip,l,ng) = aphy_chl_ps_type(iopt,l)*scalefac
             ENDDO

! TOTAL SCATTER
! in_ terms of diameter
             volmeas = PI / 6.0_r8 * bsize(iopt)**3
             dmmeas = bsize(iopt)
             dmac = (6.0_r8 * biovol(ip,ng)/PI)**(1.0_r8 / 3.0_r8)
             dmratio = dmac/dmmeas
             carpcellmeas = gud_aCarCell(ng)*volmeas**gud_bCarCell(ng)
             carpcellac = gud_aCarCell(ng)*biovol(ip,ng)**gud_bCarCell(ng)
! size specific scattering spectra
             DO l = 1, nlam
! convert scatter spectrum from m2/mgC to m2/celll
               bphy_cell_type = bphy_mgC_type(iopt,l)*carpcellmeas
               dmint = 10.0_r8 ** gud_scatSwitchSizeLog(l,ng)
               IF (dmmeas.GE.dmint) THEN
                 slope = gud_scatSlopeLarge(l,ng)
               ELSE
                 slope = gud_scatSlopeSmall(l,ng)
               ENDIF
               bphy_mgC(ip,l,ng) = bphy_cell_type*dmratio**slope/carpcellac
             ENDDO

! BACK SCATTER
! calculate mean scatter
             bmean = 0.0_r8
             bbmean = 0.0_r8
             DO l = 1, nlam
               bmean = bmean + bphy_mgC_type(iopt,l)*wb_width(l)
               bbmean = bbmean + bbphy_mgC_type(iopt,l)*wb_width(l)
             ENDDO
             bmean = bmean/wb_totalWidth
             bbmean = bbmean/wb_totalWidth
! scale mean backscattering ratio
             bbbratiomeas = bbmean/bmean
             bbbratioac = bbbratiomeas*dmratio**gud_bbbSlope(ng)
             DO l = 1, nlam
               bbphy_mgC(ip,l,ng) = bphy_mgC(ip,l,ng)*bbbratioac
             ENDDO
!c
           ELSE
! OR use read in_ absorbtion spectra
            DO l = 1, nlam
             aphy_chl(ip,l,ng) = aphy_chl_type(iopt,l)
             aphy_chl_ps(ip,l,ng) = aphy_chl_ps_type(iopt,l)
             bphy_mgC(ip,l,ng) = bphy_mgC_type(iopt,l)
             bbphy_mgC(ip,l,ng) = bbphy_mgC_type(iopt,l)
            ENDDO
           ENDIF
          ELSEIF (ip .LE. nPhoto) THEN
            IF (Master) WRITE(out,'(A,2I4)')                            'invalid optical phyto type:',ip,iopt
            exit_flag=5
            RETURN
          ENDIF
#endif

! ip
        ENDDO

! ======================================================================
! grazing

        DO iz=1,nplank
          gz = group(iz,ng)
! maximum grazing rate
          grazemax(iz,ng) = a_graz(gz,ng) * biovol(iz,ng)**b_graz(gz,ng)
! grazing half-saturation
          kgrazesat(iz,ng) = a_kg(gz,ng) * biovol(iz,ng)**b_kg(gz,ng)
          DO ip=1,nplank
            ig = group(ip,ng)
            IF (grp_pred(gz,ng).GT.0 .AND. grp_prey(ig,ng).GT.0) THEN
#if defined DARWIN_ALLOMETRIC_PALAT
! assign grazing preference according to predator/prey radius ratio
! grazing size preference ratio
              pp_opt(iz) = a_prdpry(gz,ng) * biovol(iz,ng)**b_prdpry(gz,ng)
! standard deviation of size preference
              pp_sig(iz) = grp_pp_sig(gz,ng)
              prd_pry = biovol(iz,ng) / biovol(ip,ng)
              palat(ip,iz,ng) =EXP(-(LOG(prd_pry/pp_opt(iz))**2) / (2*  &
     &          pp_sig(iz)**2))/ pp_sig(iz)/2.0_r8
              IF (palat(ip,iz,ng).LT.palat_min(ng)) THEN
                palat(ip,iz,ng) = 0.0_r8
              ENDIF
#else
              palat(ip,iz,ng) = 0.0_r8
#endif
              asseff(ip,iz,ng) = grp_ass_eff(ig,gz,ng)
              ExportFracPreyPred(ip,iz,ng) = grp_ExportFracPreyPred(ig,gz,ng)
#if defined DARWIN_VERBOSE_PLANK_OLD
              IF(Master) THEN
                write(*,'(a,i2,1x,a,i2,1x,a16,2x,10(f,1x,a,1x))')       &
     &            'PLANK',ic_+iz-1,'-> PLANK',ic_+ip-1,'(a) palat=',    &
     &            palat(ip,iz,ng), ' asseff=',asseff(ip,iz,ng),         &
     &            ' ExportFracPreyPred=', ExportFracPreyPred(ig,gz,ng)
              END IF
#endif
            ELSE
              palat(ip,iz,ng) = 0.0_r8
              asseff(ip,iz,ng) = 0.0_r8
              ExportFracPreyPred(ip,iz,ng) = 0.0_r8
#if defined DARWIN_VERBOSE_PLANK_OLD
              IF(Master) THEN
                write(*,'(a,i2,1x,a,i2,1x,a16,2x,10(f,1x,a,1x))')       &
     &            'PLANK',ic_+iz-1,'-> PLANK',ic_+ip-1,'(b) palat=',    &
     &            palat(ip,iz,ng), ' asseff=',asseff(ip,iz,ng),         &
     &            ' ExportFracPreyPred=', ExportFracPreyPred(ig,gz,ng)
              END IF
#endif
            ENDIF
          ENDDO
        ENDDO
