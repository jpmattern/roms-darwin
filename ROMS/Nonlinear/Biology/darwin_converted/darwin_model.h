!
!  converted from "gud_model.F" using darwinfortran_to_romsfortran
!  (written by J. Paul Mattern)
!
          chlout = 0.0_r8

          consumDIC = 0.0_r8
          consumDIC_PIC = 0.0_r8
          consumNH4 = 0.0_r8
          consumNO2 = 0.0_r8
          consumNO3 = 0.0_r8
          consumPO4 = 0.0_r8
          consumSiO2 = 0.0_r8
          consumFeT = 0.0_r8
          consumPON = 0.0_r8
          consumPOP = 0.0_r8
          consumPOC = 0.0_r8
          consumPOFe = 0.0_r8
          consumPOSi = 0.0_r8
          consumDON = 0.0_r8
          consumDOP = 0.0_r8
          consumDOC = 0.0_r8
          consumDOFe = 0.0_r8
          consumO2 = 0.0_r8
          reminPON = 0.0_r8
          reminPOP = 0.0_r8
          reminPOC = 0.0_r8
          reminPOFe = 0.0_r8
          reminPOSi = 0.0_r8
          reminDON = 0.0_r8
          reminDOP = 0.0_r8
          reminDOC = 0.0_r8
          reminDOFe = 0.0_r8
          solubilPON = 0.0_r8
          solubilPOP = 0.0_r8
          solubilPOC = 0.0_r8
          solubilPOFe = 0.0_r8
          prodNO2 = 0.0_r8
          prodNO3 = 0.0_r8
#if defined DARWIN_DEACTIVATED
          DO l=1,darwin_nDiag
            diags(i,k,l) = 0.0_r8
          ENDDO
#endif

!=======================================================================
!==== phytoplankton ====================================================

          DO ic = 1, nPhoto
! fixed carbon quota, for now 1.0_r8 (may change later)
! other elements: get quota from corresponding ptracer or set to fixed
! ratio if not variable.
#if defined DARWIN_MINVAL_X
            X = MAX(DARWIN_MINVAL_X, Bio(i,k,ic_+ic-1))
#else
            X = MAX(0.0_r8, Bio(i,k,ic_+ic-1))
#endif
#if defined DARWIN_VERBOSE_PLANK
            IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,'(a,i2.2,a,f10.6,a)') 'plankton',ic,              &
     &          ' (initial value: ',X,')'
            END IF
#endif
            Qc = 1.0_r8

!==== uptake and nutrient limitation ===================================
! for quota elements, growth is limiteed by available quota,
! for non-quota elements, by available nutrients in_ medium

! to not use PO4, ..., set ksatPO4(ng)=0 and Vmax_PO4(ng)=0 (if DARWIN_PQUOTA)
! or R_PC(ng)=0 (if not)
! the result will be limitp = 1, uptakePO4 = 0

! phosphorus
            limitp = Bio(i,k,iPO4)/(Bio(i,k,iPO4) + ksatPO4(ic,ng))
#if defined DARWIN_PQUOTA
            Qp = MAX(BioMin(ng)*R_PC(ic,ng), Bio(i,k,ip_+ic-1)) / MAX(BioMin(ng), X)
            regQ = MAX(0.0_r8, MIN(1.0_r8, (Qpmax(ic,ng)-Qp)/(Qpmax(ic,ng)-Qpmin(ic,ng)) ))
            uptakePO4 = Vmax_PO4(ic,ng) * limitp * regQ *uptakeTempFunc(i,k) * X
! normalized Droop limitation
            limitp = MAX(0.0_r8, MIN(1.0_r8, (1.0_r8-Qpmin(ic,ng)/Qp)/(1.0_r8-Qpmin(ic,ng)/Qpmax(ic,ng))))
#endif

! silica
#if defined DARWIN_AUTO_FIX
! Things can go wrong if Bio(i,k,iSiO2) == 0 and ksatSiO2(ic,ng) == 0
! This fix explicitly sets limitsi=1.0 if ksatSiO2(ic,ng) == 0, which
! would normally happen implicitly (see "for quota elements" comment
! above).
            IF (ksatSiO2(ic,ng)==0.0_r8) THEN
            limitsi = 1.0_r8
# if defined DARWIN_SIQUOTA
            uptakeSiO2 = 0.0_r8
# endif
            ELSE
#endif /*DARWIN_AUTO_FIX*/
            limitsi = Bio(i,k,iSiO2)/(Bio(i,k,iSiO2) + ksatSiO2(ic,ng))
#if defined DARWIN_SIQUOTA
            Qsi = MAX(BioMin(ng)*R_SiC(ic,ng), Bio(i,k,isi+ic-1)) / MAX(BioMin(ng), X)
            regQ = MAX(0.0_r8, MIN(1.0_r8, (Qsimax(ic,ng) - Qsi)/(Qsimax(ic,ng) - Qsimin(ic,ng)) ))
            uptakeSiO2 = Vmax_SiO2(ic,ng) * limitsi * regQ *uptakeTempFunc(i,k) * X

! linear limitation
            limitsi = MAX(0.0_r8, MIN(1.0_r8, (Qsi - Qsimin(ic,ng))/(Qsimax(ic,ng) - Qsimin(ic,ng)) ))
#endif
#if defined DARWIN_AUTO_FIX
            ENDIF
#endif

! iron
            limitfe = Bio(i,k,iFeT)/(Bio(i,k,iFeT) + ksatFeT(ic,ng))
#if defined DARWIN_FEQUOTA
            Qfe = MAX(BioMin(ng)*R_FeC(ic,ng), Bio(i,k,ife+ic-1)) / MAX(BioMin(ng), X)
            regQ = MAX(0.0_r8, MIN(1.0_r8, (Qfemax(ic,ng)-Qfe)/(Qfemax(ic,ng)-Qfemin(ic,ng)) ))
            uptakeFeT = Vmax_FeT(ic,ng) * limitfe * regQ *uptakeTempFunc(i,k) * X

! normalized Droop limitation
            limitfe = MAX(0.0_r8, MIN(1.0_r8, (1.0_r8-Qfemin(ic,ng)/Qfe)/(1.0_r8-Qfemin(ic,ng)/Qfemax(ic,ng))))
#endif

! nitrogen
#if defined DARWIN_NQUOTA
! have nitrogen quota
            inhibNH4 = EXP(-amminhib(ic,ng)*Bio(i,k,iNH4))
            limitNH4 = Bio(i,k,iNH4)/(Bio(i,k,iNH4) + ksatNH4(ic,ng))
            limitNO2 = Bio(i,k,iNO2)/(Bio(i,k,iNO2) + ksatNO2(ic,ng))*inhibNH4
            limitNO3 = Bio(i,k,iNO3)/(Bio(i,k,iNO3) + ksatNO3(ic,ng))*inhibNH4
            Qn = MAX(BioMin(ng)*R_NC(ic,ng), Bio(i,k,in_+ic-1)) / MAX(BioMin(ng), X)
            regQ = MAX(0.0_r8, MIN(1.0_r8, (Qnmax(ic,ng)-Qn)/(Qnmax(ic,ng)-Qnmin(ic,ng)) ))
            uptakeNH4 = Vmax_NH4(ic,ng)*limitNH4*regQ*uptakeTempFunc(i,k)*X
            uptakeNO2 = Vmax_NO2(ic,ng)*limitNO2*regQ*uptakeTempFunc(i,k)*X
            uptakeNO3 = Vmax_NO3(ic,ng)*limitNO3*regQ*uptakeTempFunc(i,k)*X
#if defined DARWIN_FEQUOTA
            uptakeNO3 = uptakeNO3 * limitfe
#endif
            uptakeN = MAX(uptakeNH4 + uptakeNO2 + uptakeNO3,Vmax_N(ic,ng)*regQ*uptakeTempFunc(i,k)*X*diazo(ic,ng))

! linear limitation
            limitn = MAX(0.0_r8, MIN(1.0_r8, (Qn - Qnmin(ic,ng))/(Qnmax(ic,ng) - Qnmin(ic,ng)) ))
#else /* not DARWIN_NQUOTA */
            Qn = R_NC(ic,ng)
            inhibNH4 = EXP(-amminhib(ic,ng)*Bio(i,k,iNH4))
            limitNH4 = useNH4(ic,ng)*Bio(i,k,iNH4)/(Bio(i,k,iNH4) + ksatNH4(ic,ng))
#if defined DARWIN_VERBOSE_PLANK
            IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,'(a20,f10.6," (",f10.6,a,f10.6,a,f12.6,a,")")')   &
     &          'limitNH4 = ',limitNH4,                                 &
     &          useNH4(ic,ng)*Bio(i,k,iNH4),                            &
     &          '/(',Bio(i,k,iNH4),'+',ksatNH4(ic,ng),')'
            END IF
#endif
            limitNO2 = useNO2(ic,ng)*Bio(i,k,iNO2)/(Bio(i,k,iNO2) +     &
     &        combNO(ic,ng)*(Bio(i,k,iNO3) + ksatNO3(ic,ng) -           &
     &        ksatNO2(ic,ng)) + ksatNO2(ic,ng))*inhibNH4
            limitNO3 = useNO3(ic,ng)*Bio(i,k,iNO3)/(combNO(ic,ng)*      &
     &        Bio(i,k,iNO2) + Bio(i,k,iNO3) + ksatNO3(ic,ng))*inhibNH4
            limitn = limitNH4 + limitNO2 + limitNO3
#if defined DARWIN_VERBOSE_PLANK
            IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,'(a20,f10.6," (",f10.6,2("+",f10.6),")",a)')      &
     &          'limitn = ',limitn,                                     &
     &          limitNH4,limitNO2,limitNO3,' (NH4,NO2,NO3)'
            END IF
#endif
! normalize to sum (approx) 1
            fracNH4 = limitNH4/(limitn + BioMin(ng))
            fracNO2 = limitNO2/(limitn + BioMin(ng))
            fracNO3 = limitNO3/(limitn + BioMin(ng))
! if diazo(ng), all fracN* == 0 but want no N limitation
            limitn = MIN(1.0_r8, limitn + diazo(ic,ng))
#endif /* DARWIN_NQUOTA */
! IF (limitn .GT. 0.0_r8) THEN
! ngrow = ((10*4+2)/(10*4 + 2*limitNH4/limitn +
! & 8*limitNO2/limitn + 10*limitNO3/limitn))
! ELSE
            ngrow = 1.0_r8
! ENDIF

            limitnut = MIN(limitn, limitp, limitsi)
#if defined DARWIN_VERBOSE_PLANK
            IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
# if ! defined DARWIN_FEQUOTA
              write(*,'(a20,f10.6," MIN(",f10.6,3(",",f10.6),")",a)')   &
     &          'limitnut = ',limitnut,                                 &
     &          limitn, limitp, limitsi, limitfe, ' (N,P,Si,Fe)'
# else
              write(*,'(a20,f10.6," MIN(",f10.6,2(",",f10.6),")",a)')   &
     &          'limitnut = ',limitnut,                                 &
     &          limitn, limitp, limitsi, ' (N,P,Si)'
# endif
            END IF
#endif
#if ! defined DARWIN_FEQUOTA
            limitnut = MIN(limitnut, limitfe)
#endif

            limitpCO2 = 1.0_r8

!==== growth ===========================================================
#if defined DARWIN_GEIDER

            alpha_I = 0.0_r8
            DO l = 1, nlam
#if defined DARWIN_VERBOSE_PLANK_OLD
            IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,'(a,i2,1x,a16,2x,10(f,1x,a,1x))') 'PLANK', ic_+ic-1, 'LAM alpha_I=', alphachl(ic,l,ng), '*', PAR(i,k,l)
            END IF
#endif
              alpha_I = alpha_I + alphachl(ic,l,ng)*PAR(i,k,l)
            ENDDO
#if defined DARWIN_PLANK_BUOYCTRL
            limitnut_save(i,k,ic)=limitnut
#endif
! NB: for quota, PCmax(ic,ng) = Vmax_c(ic)
            PCm = PCmax(ic,ng)*limitnut*photoTempFunc(i,k,ic)*limitpCO2
#if defined DARWIN_VERBOSE_PLANK
            IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,'(a20,f10.6," (",f10.6,3("*",f10.6),a,f10.4")")') &
     &          'PCm = ',PCm,                                           &
     &          PCmax(ic,ng),limitnut,photoTempFunc(i,k,ic),limitpCO2,  &
     &          '; PAR=',PAR(i,k,1)
            END IF
#endif

            IF (PCm .GT. 0.0_r8) THEN
              acclim = MAX(chl2cmin(ic,ng), MIN(chl2cmax(ic,ng),chl2cmax(ic,ng)/(1+(chl2cmax(ic,ng)*alpha_I)/(2*PCm)) ))
            ELSE
              acclim = chl2cmin(ic,ng)
            ENDIF
#if defined DARWIN_VERBOSE_PLANK
            IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,'(a20,f14.10," (",4(f14.10,a),")",a,f14.10,a)')   &
     &          'acclim = ',acclim,                                     &
     &          chl2cmax(ic,ng),'/(1+(',chl2cmax(ic,ng),'*',alpha_I,    &
     &          ')/(2*',PCm,'))', ' (chl2cmin=',chl2cmin(ic,ng),')'
            END IF
#endif

#if defined DARWIN_CHLQUOTA
            QChl=MAX(BioMin(ng)*R_ChlC(ic,ng),Bio(i,k,ichl+ic-1))/MAX(BioMin(ng),X)
! quotas are already relative to carbon
            chl2c = QChl
#else
            chl2c = acclim
#endif

            alpha_I_growth = alpha_I
! a la quota
#if defined DARWIN_FEQUOTA
            alpha_I_growth = alpha_I_growth*limitfe
#endif

! carbon-specific growth rate
! PC = PCm*(1-EXP(-alpha_I_growth*chl2c/MAX(BioMin(ng), PCm)))
            IF (PCm .GT. 0.0_r8 .AND. PARtot .GT. PARmin(ng)) THEN
#if defined DARWIN_VERBOSE_PLANK_OLD
            IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,'(a,i2,1x,a16,2x,10(f,1x,a,1x))') 'PLANK', ic_+ic-1, '(a) PC=', PCm, '*(1-EXP(', -alpha_I_growth, '*', chl2c, '/', PCm, '))'
            END IF
#endif
              PC = PCm*(1.0_r8-EXP(-alpha_I_growth*chl2c/PCm))
#if defined DARWIN_PLANK_BUOYCTRL
              limitlight_save(i,k,ic)=photoTempFunc(i,k,ic)*(1.0_r8-EXP(-alpha_I_growth*chl2c/PCm))
#endif
#if defined DARWIN_VERBOSE_PLANK
            IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,'(a20,f10.6," (",3(a,f13.6),"))")')               &
     &          'light limit : ', 1.0_r8-EXP(-alpha_I_growth*chl2c/PCm),&
     &          '1-EXP(-',alpha_I_growth,'*',chl2c,'/',PCm
              write(*,'(a20,f10.6," (",f10.6,3(a,f13.6),")))")')        &
     &          'PC = ',PC,                                             &
     &          PCm,'*(1-EXP(-',alpha_I_growth,'*',chl2c,'/',PCm
            END IF
#endif
            ELSE
#if defined DARWIN_VERBOSE_PLANK_OLD
            IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,'(a,i2,1x,a16,2x,10(f,1x,a,1x))') 'PLANK', ic_+ic-1, '(b) PC=', 0.0_r8, 'PCm=', PCm, 'PARtot=', PARtot, ''
            END IF
#endif
              PC = 0.0_r8
#if defined DARWIN_PLANK_BUOYCTRL
              limitlight_save(i,k,ic)=0.0_r8
#endif
#if defined DARWIN_VERBOSE_PLANK
            IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,'(a20,f10.6)')                                    &
     &          'PC = ',PC
            END IF
#endif
            ENDIF

            IF (inhibcoef_geid(ic,ng) .GT. 0.0_r8) THEN
! "total" PAR:
              tmp = alpha_I/alpha_mean(ic,ng)
              Ek = PCm/(chl2c*alpha_mean(ic,ng))
              EkoverE = Ek / tmp
              IF (tmp .GE. Ek) THEN
                PC = PC*EkoverE*inhibcoef_geid(ic,ng)
              ENDIF
            ENDIF

#else /* not DARWIN_GEIDER */

            IF (PARtot .GT. PARmin(ng)) THEN
! only 1 waveband without DARWIN_GEIDER
              limitI = (1.0_r8 - EXP(-PARtot*ksatPAR(ic,ng)))*EXP(-PARtot*kinhPAR(ic,ng)) * normI(ic,ng)
              PC = PCmax(ic,ng)*limitnut*limitI*photoTempFunc(i,k,ic)*limitpCO2
            ELSE
              PC = 0.0_r8
            ENDIF
            synthChl = 0.0_r8

#endif /* DARWIN_GEIDER */

#if defined DARWIN_VERBOSE_PLANK_OLD
            IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,'(a,i2,1x,a16,2x,10(f,1x,a,1x))') 'PLANK', ic_+ic-1, 'growth=', PC, '*', ngrow, '*', X, ''
            END IF
#endif
            growth = PC*ngrow*X
#if defined DARWIN_DIAG_RATES
            diags(i,j,irategrow+ic-1)=PC*ngrow*dtdays
#endif
#if defined DARWIN_VERBOSE_PLANK
            IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,'(a20,f10.6," (",f10.6,2("*",f10.6),")")')        &
     &          'growth = ',growth,                                     &
     &          PC,ngrow,X
              write(*,'(a20,f10.6," (",f10.6,1("*",f12.6),")")')        &
     &          'grow rate (d-1): ',PC*86400.0_r8,                      &
     &          PC,86400.0_r8
            END IF
#endif

            uptakeDIC = growth

! non-quota elements are taken up with fixed stoichiometry
#if ! defined DARWIN_NQUOTA
            uptakeN = growth*R_NC(ic,ng)
            uptakeNH4 = uptakeN*fracNH4
            uptakeNO2 = uptakeN*fracNO2
            uptakeNO3 = uptakeN*fracNO3
#endif
#if ! defined DARWIN_PQUOTA
            uptakePO4 = growth*R_PC(ic,ng)
#endif
#if ! defined DARWIN_SIQUOTA
            uptakeSiO2 = growth*R_SiC(ic,ng)
#endif
#if ! defined DARWIN_FEQUOTA
            uptakeFeT = growth*R_FeC(ic,ng)
#endif

!==== chlorophyll ======================================================
#if defined DARWIN_GEIDER
#if defined DARWIN_CHLQUOTA
#if defined DARWIN_NQUOTA
! Geider 1998
            IF (alpha_I*chl2c .GT. 0.0_r8) THEN
! rhochl = chl2nmax(ng)/(alpha_I*chl2c)*ngrow ???
              rhochl = chl2nmax(ng)*PC*ngrow/(alpha_I*chl2c)
            ELSE
              rhochl = chl2nmax(ng)
            ENDIF
            uptakeDIC = uptakeDIC - synthcost(ng)*uptakeN
            synthChl = rhochl*uptakeN
#else
#if defined DARWIN_GEIDER_RHO_SYNTH
            IF (alpha_I .GT. 0.0_r8 .AND. acclim .GT. 0.0_r8) THEN
              rhochl = chl2cmax(ic,ng)*PC*ngrow/(alpha_I*acclim)
            ELSE
              rhochl = 0.0_r8 ! should be chl2cmax(ic,ng) ?????
            ENDIF
            synthChl = rhochl*growth +acclimtimescl(ic,ng)*(acclim-chl2c)*X
#if defined DARWIN_VERBOSE_PLANK
            IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,'(a20,f14.10," (",f14.10,5(a,f15.10),")")')       &
     &          'synthChl = ',synthChl,                                 &
     &          rhochl,'*',growth,'+',acclimtimescl(ic,ng),'*(',        &
     &          acclim,'-',chl2c,')*',X
            END IF
#endif
#else
            synthChl = acclim*growth +acclimtimescl(ic,ng)*(acclim-chl2c)*X
#endif
#endif /* DARWIN_NQUOTA */
#else /* DARWIN_CHLQUOTA */
            chlout(ic) = X*Qc*chl2c
            synthChl = 0.0_r8
#endif /* DARWIN_CHLQUOTA */
#endif /* DARWIN_GEIDER */
!=======================================================================
            consumDIC_PIC = consumDIC_PIC + uptakeDIC*R_PICPOC(ic,ng)
            consumDIC = consumDIC + uptakeDIC
            consumNH4 = consumNH4 + uptakeNH4
            consumNO2 = consumNO2 + uptakeNO2
            consumNO3 = consumNO3 + uptakeNO3
            consumPO4 = consumPO4 + uptakePO4
            consumSiO2 = consumSiO2 + uptakeSiO2
            consumFeT = consumFeT + uptakeFeT

            diags(i,k,iPP) = diags(i,k,iPP) + growth
            IF (ic .LE. nPPplank) THEN
              diags(i,k,iPPplank+ic-1) = diags(i,k,iPPplank+ic-1) + growth
            ENDIF
            IF (diazo(ic,ng) .GT. 0.0_r8) THEN
             diags(i,k,iNfix)=diags(i,k,iNfix)+uptakeN-uptakeNH4-uptakeNO2-uptakeNO3
            ENDIF

!=======================================================================
#if defined DARWIN_VERBOSE_PLANK_OLD
            IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,'(a,i2,1x,a16,2x,f)') 'PLANK', ic_+ic-1, '+uptakeDIC', dtbio(ng)*(+ uptakeDIC)
            END IF
#endif
#if defined DARWIN_VERBOSE_PLANK
            IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,'(a20,f10.6," (",f10.6,"+",f10.6,"*",f10.6,")")') &
     &          'new value = ',Bio(i,k,ic_+ic-1)+ dtbio(ng)*(+ uptakeDIC), &
     &          Bio(i,k,ic_+ic-1),dtbio(ng),uptakeDIC
              write(*,'(a20,f14.10)') 'change : ',                      &
     &           dtbio(ng)*(+ uptakeDIC)
            END IF
#endif
            Bio(i,k,ic_+ic-1)=Bio(i,k,ic_+ic-1)  + dtbio(ng)*(+ uptakeDIC)
#if defined DARWIN_NQUOTA
            Bio(i,k,in_+ic-1)=Bio(i,k,in_+ic-1)  + dtbio(ng)*(+ uptakeN)
#endif
#if defined DARWIN_PQUOTA
            Bio(i,k,ip_+ic-1)=Bio(i,k,ip_+ic-1)  + dtbio(ng)*(+ uptakePO4)
#endif
#if defined DARWIN_SIQUOTA
            Bio(i,k,isi+ic-1)=Bio(i,k,isi+ic-1)  + dtbio(ng)*(+ uptakeSiO2)
#endif
#if defined DARWIN_FEQUOTA
            Bio(i,k,ife+ic-1)=Bio(i,k,ife+ic-1)  + dtbio(ng)*(+ uptakeFeT)
#endif
#if defined DARWIN_CHLQUOTA
            Bio(i,k,iChl+ic-1)=Bio(i,k,iChl+ic-1)  + dtbio(ng)*(+ synthChl)
#endif

#if defined DARWIN_DEBUG
            IF (i.eq.iDEBUG.and.ic.eq.jDEBUG) THEN
             print*,'uptake',myiter,k,ic,uptakeDIC,uptakeNH4,uptakeNO2,uptakeNO3,uptakeN,uptakePO4,uptakeSiO2,uptakeFeT
            ENDIF
#endif

! ic
          ENDDO

!=======================================================================
!==== bacteria =========================================================

          DO ic = iMinBact, iMaxBact
           IF (bactType(ic,ng) .NE. 0) THEN

#if defined DARWIN_MINVAL_X
            X = MAX(DARWIN_MINVAL_X, Bio(i,k,ic_+ic-1))
#else
            X = MAX(0.0_r8, Bio(i,k,ic_+ic-1))
#endif

            uptakeO2 = 0.0_r8
            uptakeNO3 = 0.0_r8
            uptakePOC = 0.0_r8
            uptakePON = 0.0_r8
            uptakePOP = 0.0_r8
            uptakePOFe = 0.0_r8
            uptakeDOC = 0.0_r8
            uptakeDON = 0.0_r8
            uptakeDOP = 0.0_r8
            uptakeDOFe = 0.0_r8
            hydrolPOC = 0.0_r8
            hydrolPON = 0.0_r8
            hydrolPOP = 0.0_r8
            hydrolPOFe = 0.0_r8
            respPOC = 0.0_r8
            respPON = 0.0_r8
            respPOP = 0.0_r8
            respPOFe = 0.0_r8
            respDOC = 0.0_r8
            respDON = 0.0_r8
            respDOP = 0.0_r8
            respDOFe = 0.0_r8
            growth = 0.0_r8

            IF (isAerobic(ic,ng) .NE. 0) THEN
              muO = yieldO2(ic,ng)*pcoefO2(ng)*Bio(i,k,iO2)
            ELSEIF (isDenit(ic,ng) .NE. 0) THEN
              muO = yieldNO3(ic,ng)*pmaxDIN(ng)*Bio(i,k,iNO3)/(Bio(i,k,iNO3) + ksatDIN(ng))*reminTempFunc(i,k)
            ENDIF

! POM-consuming (particle-associated)
            IF (bactType(ic,ng) .EQ. 1) THEN

              PCm = yield(ic,ng)*pmaxPON(ng)*reminTempFunc(i,k)
              muPON = PCm*Bio(i,k,iPON)/(Bio(i,k,iPON) + ksatPON(ic,ng))
              muPOC = PCm*Bio(i,k,iPOC)/(Bio(i,k,iPOC) + ksatPOC(ic,ng))
              muPOP = PCm*Bio(i,k,iPOP)/(Bio(i,k,iPOP) + ksatPOP(ic,ng))
              muPOFe = PCm*Bio(i,k,iPOFe)/(Bio(i,k,iPOFe) + ksatPOFe(ic,ng))
              mu = MIN(muPON, muPOC, muPOP, muPOFe, muO)

              growth = mu*X

              uptakePOC = alpha_hydrol(ng)*growth/yield(ic,ng)
              uptakePON = uptakePOC*R_NC(ic,ng)
              uptakePOP = uptakePOC*R_PC(ic,ng)
              uptakePOFe = uptakePOC*R_FeC(ic,ng)
! O2/NO3 is only used for the part of POC that is metabolized:
              uptakeO2 = isAerobic(ic,ng)*growth/yieldO2(ic,ng)
              uptakeNO3 = isDenit(ic,ng)*growth/yieldNO3(ic,ng)

! This is the part of POM that is hydrolized into DOM:
              hydrolPOC = (alpha_hydrol(ng)-1)*growth/yield(ic,ng)
              hydrolPON = hydrolPOC*R_NC(ic,ng)
              hydrolPOP = hydrolPOC*R_PC(ic,ng)
              hydrolPOFe = hydrolPOC*R_FeC(ic,ng)

! These are the bacteria products for remineralization of POM:
              respPOC = growth*(1/yield(ic,ng)-1)
              respPON = respPOC*R_NC(ic,ng)
              respPOP = respPOC*R_PC(ic,ng)
              respPOFe = respPOC*R_FeC(ic,ng)

! DOM-consuming (free-living):
            ELSEIF (bactType(ic,ng) .EQ. 2) THEN

              PCm = yield(ic,ng)*pmaxDON(ng)*reminTempFunc(i,k)
              muDON = PCm*Bio(i,k,iDON)/(Bio(i,k,iDON) + ksatDON(ic,ng))
              muDOC = PCm*Bio(i,k,iDOC)/(Bio(i,k,iDOC) + ksatDOC(ic,ng))
              muDOP = PCm*Bio(i,k,iDOP)/(Bio(i,k,iDOP) + ksatDOP(ic,ng))
              muDOFe = PCm*Bio(i,k,iDOFe)/(Bio(i,k,iDOFe) + ksatDOFe(ic,ng))
              mu = MIN(muDON, muDOC, muDOP, muDOFe, muO)

              growth = mu*X

              uptakeDOC = growth/yield(ic,ng)
              uptakeDON = uptakeDOC*R_NC(ic,ng)
              uptakeDOP = uptakeDOC*R_PC(ic,ng)
              uptakeDOFe = uptakeDOC*R_FeC(ic,ng)
              uptakeO2 = isAerobic(ic,ng)*growth/yieldO2(ic,ng)
              uptakeNO3 = isDenit(ic,ng)*growth/yieldNO3(ic,ng)

! DOC respired to DIC
              respDOC = growth*(1/yield(ic,ng)-1)
              respDON = respDOC*R_NC(ic,ng)
              respDOP = respDOC*R_PC(ic,ng)
              respDOFe = respDOC*R_FeC(ic,ng)

            ENDIF

            IF (ic .LE. nPPplank) THEN
              diags(i,k,iPPplank+ic-1) = diags(i,k,iPPplank+ic-1) + growth
            ENDIF

#if defined DARWIN_VERBOSE_PLANK_OLD
            IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,'(a,i2,1x,a16,2x,f)') 'PLANK', ic_+ic-1, '+growth', dtbio(ng)*(+ growth)
            END IF
#endif
            Bio(i,k,ic_+ic-1)=Bio(i,k,ic_+ic-1)  + dtbio(ng)*(+ growth)

!==== Cumulative consum, remin, and prod ===============================
            consumNO3 = consumNO3 + uptakeNO3

! add B consum and accumulating remin, and prod:
            consumO2 = consumO2 + uptakeO2

            consumDOC = consumDOC + uptakeDOC
            consumDON = consumDON + uptakeDON
            consumDOP = consumDOP + uptakeDOP
            consumDOFe = consumDOFe + uptakeDOFe

            consumPOC = consumPOC + uptakePOC
            consumPON = consumPON + uptakePON
            consumPOP = consumPOP + uptakePOP
            consumPOFe = consumPOFe + uptakePOFe

            reminPOC = reminPOC + respPOC
            reminPON = reminPON + respPON
            reminPOP = reminPOP + respPOP
            reminPOFe = reminPOFe + respPOFe

            solubilPOC = solubilPOC + hydrolPOC
            solubilPON = solubilPON + hydrolPON
            solubilPOP = solubilPOP + hydrolPOP
            solubilPOFe = solubilPOFe + hydrolPOFe

            reminDOC = reminDOC + respDOC
            reminDON = reminDON + respDON
            reminDOP = reminDOP + respDOP
            reminDOFe = reminDOFe + respDOFe

           ENDIF
! ic loop end
          ENDDO

!=======================================================================
!=======================================================================

          Bio(i,k,iDIC )=Bio(i,k,iDIC )  + dtbio(ng)*(- consumDIC - consumDIC_PIC)
          Bio(i,k,iNH4 )=Bio(i,k,iNH4 )  + dtbio(ng)*(- consumNH4)
          Bio(i,k,iNO2 )=Bio(i,k,iNO2 )  + dtbio(ng)*(- consumNO2)
          Bio(i,k,iNO3 )=Bio(i,k,iNO3 )  + dtbio(ng)*(- consumNO3)
          Bio(i,k,iPO4 )=Bio(i,k,iPO4 )  + dtbio(ng)*(- consumPO4)
          Bio(i,k,iSiO2)=Bio(i,k,iSiO2)  + dtbio(ng)*(- consumSiO2)
          Bio(i,k,iFeT )=Bio(i,k,iFeT )  + dtbio(ng)*(- consumFeT)
#if defined DARWIN_VERBOSE_NUT
            IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,*) 'consumption'
              write(*,'(a20,f18.12,9(a,f18.12))')                       &
     &          'DIC = ',Bio(i,k,iDIC ),                                &
     &          ' (',Bio(i,k,iDIC )-dtbio(ng)*(consumDIC - consumDIC_PIC), &
     &          '+',dtbio(ng),'*(-',consumDIC,'-',consumDIC_PIC,')'
              write(*,'(a20,f18.12,9(a,f18.12))')                       &
     &          'NH4 = ',Bio(i,k,iNH4 ),                                &
     &          ' (',Bio(i,k,iNH4 )-dtbio(ng)*(- consumNH4),               &
     &          '+',dtbio(ng),'*(-',consumNH4,')'
              write(*,'(a20,f18.12,9(a,f18.12))')                       &
     &          'NO2 = ',Bio(i,k,iNO2 ),                                &
     &          ' (',Bio(i,k,iNO2 )-dtbio(ng)*(- consumNO2),               &
     &          '+',dtbio(ng),'*(-',consumNO2,')'
              write(*,'(a20,f18.12,9(a,f18.12))')                       &
     &          'NO3 = ',Bio(i,k,iNO3 ),                                &
     &          ' (',Bio(i,k,iNO3 )-dtbio(ng)*(- consumNO3),               &
     &          '+',dtbio(ng),'*(-',consumNO3,')'
            END IF
#endif

! parameterized remineralization; want to set all K except KPOSi(ng) to zero
! if running with bacteria
          respDOC = reminTempFunc(i,k)*Kdoc(ng) *Bio(i,k,iDOC)
          respDON = reminTempFunc(i,k)*Kdon(ng) *Bio(i,k,iDON)
          respDOP = reminTempFunc(i,k)*Kdop(ng) *Bio(i,k,iDOP)
          respDOFe = reminTempFunc(i,k)*KdoFe(ng)*Bio(i,k,iDOFe)
          respPOC = reminTempFunc(i,k)*KPOC(ng) *Bio(i,k,iPOC)
          respPON = reminTempFunc(i,k)*KPON(ng) *Bio(i,k,iPON)
          respPOP = reminTempFunc(i,k)*KPOP(ng) *Bio(i,k,iPOP)
          respPOSi = reminTempFunc(i,k)*KPOSi(ng)*Bio(i,k,iPOSi)
          respPOFe = reminTempFunc(i,k)*KPOFe(ng)*Bio(i,k,iPOFe)

          consumDOC = consumDOC + respDOC
          consumDON = consumDON + respDON
          consumDOP = consumDOP + respDOP
          consumDOFe = consumDOFe + respDOFe
          consumPOC = consumPOC + respPOC
          consumPON = consumPON + respPON
          consumPOP = consumPOP + respPOP
          consumPOSi = consumPOSi + respPOSi
          consumPOFe = consumPOFe + respPOFe

          reminDOC = reminDOC + respDOC
          reminDON = reminDON + respDON
          reminDOP = reminDOP + respDOP
          reminDOFe = reminDOFe + respDOFe
          reminPOC = reminPOC + respPOC
          reminPON = reminPON + respPON
          reminPOP = reminPOP + respPOP
          reminPOSi = reminPOSi + respPOSi
          reminPOFe = reminPOFe + respPOFe

#if defined DARWIN_CARBON
          consumO2 = consumO2 + respDOP*R_OP(ng)
#if ! defined DARWIN_CDOM
          consumO2 = consumO2 + respPOP*R_OP(ng)
#endif
#endif

          disscPIC = Kdissc(ng)*Bio(i,k,iPIC)

! nitrogen chemistry
! NH4 -> NO2 -> NO3 by bacterial action, parameterized
          prodNO2 = Knita(ng)*Bio(i,k,iNH4)
          prodNO3 = Knitb(ng)*Bio(i,k,iNO2)
#if defined DARWIN_VERBOSE_NUT
          IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
            write(*,*) 'bacterial production'
            write(*,'(a20,f18.12," (",9(f18.12,a))')                    &
     &        'prodNO2 = ',prodNO2,                                     &
     &        Knita(ng),'*',Bio(i,k,iNH4),')'
            write(*,'(a20,f18.12," (",9(f18.12,a))')                    &
     &        'prodNO3 = ',prodNO3,                                     &
     &        Knitb(ng),'*',Bio(i,k,iNO2),')'
          END IF
#endif
          IF (PAR_oxi(ng) .NE. 0.0_r8) THEN
            prodNO2 = prodNO2*MAX(0.0_r8, 1.0_r8 - PARtot/PAR_oxi(ng))
            prodNO3 = prodNO3*MAX(0.0_r8, 1.0_r8 - PARtot/PAR_oxi(ng))
#if defined DARWIN_VERBOSE_NUT
            IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,'(a20,f18.12," MAX(0,1-",9(f18.12,a))')           &
     &          'prodNO2 = ',prodNO2,                                   &
     &          PARtot,'/',PAR_oxi(ng),')'
              write(*,'(a20,f18.12," MAX(0,1-",9(f18.12,a))')           &
     &          'prodNO3 = ',prodNO3,                                   &
     &          PARtot,'/',PAR_oxi(ng),')'
            END IF
#endif
          ENDIF

#if defined DARWIN_CDOM
          reminPOP_CDOM = fracCDOM(ng)*reminPOP
          reminPOC_CDOM = R_CP_CDOM(ng)*reminPOP_CDOM
          reminPON_CDOM = R_NP_CDOM(ng)*reminPOP_CDOM
          reminPOFe_CDOM = R_FeP_CDOM(ng)*reminPOP_CDOM
! degradation of CDOM - high when bleached by light
          degrCDOM_DOP = reminTempFunc(i,k)*Bio(i,k,iCDOM)*(CDOMdegrd(ng)+CDOMbleach(ng)*MIN(1.0_r8, PARtot/PARCDOM(ng)))
          degrCDOM_DOC = R_CP_CDOM(ng) * degrCDOM_DOP
          degrCDOM_DON = R_NP_CDOM(ng) * degrCDOM_DOP
          degrCDOM_DOFe = R_FeP_CDOM(ng) * degrCDOM_DOP
#endif

#if defined DARWIN_DENIT
          IF (Bio(i,k,iO2) .LT. O2crit(ng) .AND. Bio(i,k,iNO3) .LT. NO3crit(ng)) THEN
            consumDOC = 0.0_r8
            consumDOP = 0.0_r8
            consumDON = 0.0_r8
            consumDOFe = 0.0_r8
            consumPOC = 0.0_r8
            consumPOP = 0.0_r8
            consumPON = 0.0_r8
            consumPOFe = 0.0_r8
            reminDOC = 0.0_r8
            reminDOP = 0.0_r8
            reminDON = 0.0_r8
            reminDOFe = 0.0_r8
            reminPOC = 0.0_r8
            reminPOP = 0.0_r8
            reminPON = 0.0_r8
            reminPOFe = 0.0_r8
#if defined DARWIN_CDOM
            reminPOC_cdom = 0.0_r8
            reminPOP_cdom = 0.0_r8
            reminPON_cdom = 0.0_r8
            reminPOFe_cdom = 0.0_r8
            degrCDOM_DOP = reminTempFunc(i,k)*Bio(i,k,iCDOM)*CDOMbleach(ng)*MIN(1.0_r8, PARtot/PARCDOM(ng))
            degrCDOM_DON = R_NP_CDOM(ng) * degrCDOM_DOP
            degrCDOM_DOFe = R_FeP_CDOM(ng) * degrCDOM_DOP
            degrCDOM_DOC = R_CP_CDOM(ng) * degrCDOM_DOP
#endif
          ENDIF
#endif /* DARWIN_DENIT */

!==== apply tendencies =================================================

#if defined DARWIN_CARBON
! production of O2 by photosynthesis
          Bio(i,k,iO2 )=Bio(i,k,iO2 )  + dtbio(ng)*(+ R_OP(ng)*consumPO4)
! loss of O2 by remineralization
          IF (Bio(i,k,iO2) .GT. O2crit(ng)) THEN
            Bio(i,k,iO2)=Bio(i,k,iO2)  + dtbio(ng)*(- consumO2)
          ENDIF

          Bio(i,k,iALK)=Bio(i,k,iALK)  + dtbio(ng)*(- (prodNO3 - consumNO3)- 2.0_r8*(consumDIC_PIC - disscPIC))
#endif /* DARWIN_CARBON */

          Bio(i,k,iDIC )=Bio(i,k,iDIC )  + dtbio(ng)*(+ reminDOC + disscPIC)
          Bio(i,k,iNH4 )=Bio(i,k,iNH4 )  + dtbio(ng)*(+ reminDON - prodNO2)
          Bio(i,k,iNO2 )=Bio(i,k,iNO2 )  + dtbio(ng)*(+ prodNO2 - prodNO3)
          Bio(i,k,iNO3 )=Bio(i,k,iNO3 )  + dtbio(ng)*(+ prodNO3)
#if defined DARWIN_VERBOSE_NUT
            IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,*) 'remineralization'
              write(*,'(a20,f18.12,9(a,f18.12))')                       &
     &          'DIC = ',Bio(i,k,iDIC ),                                &
     &          ' (',Bio(i,k,iDIC )-dtbio(ng)*(+ reminDOC + disscPIC),     &
     &          '+',dtbio(ng),'*(',reminDOC,'+',disscPIC,')'
              write(*,'(a20,f18.12,9(a,f18.12))')                       &
     &          'NH4 = ',Bio(i,k,iNH4 ),                                &
     &          ' (',Bio(i,k,iNH4 )-dtbio(ng)*(+ reminDON - prodNO2),      &
     &          '+',dtbio(ng),'*(',reminDON,'-',prodNO2,')'
              write(*,'(a20,f18.12,9(a,f18.12))')                       &
     &          'NO2 = ',Bio(i,k,iNO2 ),                                &
     &          ' (',Bio(i,k,iNO2 )-dtbio(ng)*(+ prodNO2 - prodNO3),       &
     &          '+',dtbio(ng),'*(',prodNO2,'-',prodNO3,')'
              write(*,'(a20,f18.12,9(a,f18.12))')                       &
     &          'NO3 = ',Bio(i,k,iNO3 ),                                &
     &          ' (',Bio(i,k,iNO3 )-dtbio(ng)*(+ prodNO3),                 &
     &          '+',dtbio(ng),'*',prodNO3,')'
            END IF
#endif
          diags(i,k,iDenitN) = 0.0_r8
#if defined DARWIN_DENIT
          IF (Bio(i,k,iO2) .LT. O2crit(ng)) THEN
            denitNH4 = reminDON
            denit = denit_NP(ng)*reminDOP
#if ! defined DARWIN_CDOM
            denitNH4 = denitNH4 + reminPON
            denit = denit + denit_NP(ng)*reminPOP
#endif
            diags(i,k,iDenit) = denit
            Bio(i,k,iNH4)=Bio(i,k,iNH4)  + dtbio(ng)*(- denitNH4)
            Bio(i,k,iNO3)=Bio(i,k,iNO3)  + dtbio(ng)*(- denit_NO3(ng)/denit_NP(ng)*denit)
            Bio(i,k,iALK)=Bio(i,k,iALK)  + dtbio(ng)*(+ denit_NO3(ng)/denit_NP(ng)*denit)
            diags(i,k,iDenitN) = denitNH4 + denit_NO3(ng)/denit_NP(ng)*denit
#if defined DARWIN_VERBOSE_NUT
            IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,*) 'denitrification'
              write(*,'(a20,f18.12,9(a,f18.12))')                       &
     &          'NH4 = ',Bio(i,k,iNH4 ),                                &
     &          ' (',Bio(i,k,iNH4 )-dtbio(ng)*(- denitNH4),                &
     &          '+',dtbio(ng),'*(-',denitNH4,')'
              write(*,'(a20,f18.12,9(a,f18.12))')                       &
     &          'NO3 = ',Bio(i,k,iNO3 ),                                &
     &          ' (',Bio(i,k,iNO3 )-dtbio(ng)*(-denit_NO3(ng)/denit_NP(ng) &
     &          *denit),                                                &
     &          '+',dtbio(ng),'*(-',denit_NO3(ng),'/',denit_NP(ng),        &
     &          '*',denit,')'
            END IF
#endif
          ENDIF
#endif /* DARWIN_DENIT */

          Bio(i,k,iPO4 )=Bio(i,k,iPO4 )  + dtbio(ng)*(+ reminDOP)
          Bio(i,k,iFeT )=Bio(i,k,iFeT )  + dtbio(ng)*(+ reminDOFe)
          Bio(i,k,iSiO2)=Bio(i,k,iSiO2)  + dtbio(ng)*(+ reminPOSi)

! DOC is created by #4 PA-assoc solubilization and consumed by #5
          Bio(i,k,iDOC )=Bio(i,k,iDOC )  + dtbio(ng)*(+ solubilPOC - consumDOC)
          Bio(i,k,iDON )=Bio(i,k,iDON )  + dtbio(ng)*(+ solubilPON - consumDON)
          Bio(i,k,iDOP )=Bio(i,k,iDOP )  + dtbio(ng)*(+ solubilPOP - consumDOP)
          Bio(i,k,iDOFe)=Bio(i,k,iDOFe)  + dtbio(ng)*(+ solubilPOFe - consumDOFe)

          Bio(i,k,iPIC )=Bio(i,k,iPIC )  + dtbio(ng)*(- disscPIC)
          Bio(i,k,iPOC )=Bio(i,k,iPOC )  + dtbio(ng)*(- consumPOC)
          Bio(i,k,iPON )=Bio(i,k,iPON )  + dtbio(ng)*(- consumPON)
          Bio(i,k,iPOP )=Bio(i,k,iPOP )  + dtbio(ng)*(- consumPOP)
          Bio(i,k,iPOFe)=Bio(i,k,iPOFe)  + dtbio(ng)*(- consumPOFe)
          Bio(i,k,iPOSi)=Bio(i,k,iPOSi)  + dtbio(ng)*(- consumPOSi)

#if defined DARWIN_CDOM
          Bio(i,k,iDOC )=Bio(i,k,iDOC )  + dtbio(ng)*(+ reminPOC - reminPOC_CDOM + degrCDOM_DOC)
          Bio(i,k,iDON )=Bio(i,k,iDON )  + dtbio(ng)*(+ reminPON - reminPON_CDOM + degrCDOM_DON)
          Bio(i,k,iDOP )=Bio(i,k,iDOP )  + dtbio(ng)*(+ reminPOP - reminPOP_CDOM + degrCDOM_DOP)
          Bio(i,k,iDOFe)=Bio(i,k,iDOFe)  + dtbio(ng)*(+ reminPOFe - reminPOFe_CDOM + degrCDOM_DOFe)

          Bio(i,k,iCDOM)=Bio(i,k,iCDOM)  + dtbio(ng)*(+ reminPOP_CDOM - degrCDOM_DOP)
#else
          Bio(i,k,iDIC )=Bio(i,k,iDIC )  + dtbio(ng)*(+ reminPOC)
          Bio(i,k,iNH4 )=Bio(i,k,iNH4 )  + dtbio(ng)*(+ reminPON)
          Bio(i,k,iPO4 )=Bio(i,k,iPO4 )  + dtbio(ng)*(+ reminPOP)
          Bio(i,k,iFeT )=Bio(i,k,iFeT )  + dtbio(ng)*(+ reminPOFe)
#endif /* DARWIN_CDOM */

          diags(i,k,iConsDIN) = consumNH4 + consumNO2 + consumNO3
          diags(i,k,iConsPO4) = consumPO4
          diags(i,k,iConsSi) = consumSiO2
          diags(i,k,iConsFe) = consumFeT
