!
!  converted from "gud_model.F" and "gud_grazing.F" using
!  darwinfortran_to_romsfortran (written by J. Paul Mattern)
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
            diags(l) = 0.0_r8
          ENDDO
#endif

!=======================================================================
!==== phytoplankton ====================================================

          DO ic = 1, nPhoto
! fixed carbon quota, for now 1.0_r8 (may change later)
! other elements: get quota from corresponding ptracer or set to fixed
! ratio if not variable.
#if defined DARWIN_MINVAL_X
            X = MAX(DARWIN_MINVAL_X, Bio(ic_+ic-1))
#else
            X = MAX(0.0_r8, Bio(ic_+ic-1))
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

! to not use PO4, ..., set ksatPO4(ng)=0 and VmaxPO4(ng)=0 (if DARWIN_PQUOTA)
! or R_PC(ng)=0 (if not)
! the result will be limitp = 1, uptakePO4 = 0

! phosphorus
            limitp = Bio(iPO4)/(Bio(iPO4) + ksatPO4(ic,ng))
#if defined DARWIN_PQUOTA
            Qp = MAX(BioMin(ng)*R_PC(ic,ng), Bio(ip_+ic-1)) / MAX(BioMin(ng), X)
#if defined DARWIN_FUTURE
            regQ = (MAX(0.0_r8, MIN(1.0_r8, (Qpmax(ic,ng)-Qp)/(Qpmax(ic,ng)-Qpmin(ic,ng)) )))**hillnumPO4(ic,ng)
#else
            regQ = MAX(0.0_r8, MIN(1.0_r8, (Qpmax(ic,ng)-Qp)/(Qpmax(ic,ng)-Qpmin(ic,ng)) ))
#endif
            uptakePO4 = VmaxPO4(ic,ng) * limitp * regQ *uptakeTempFunc * X
! normalized Droop limitation
            limitp = MAX(0.0_r8, MIN(1.0_r8, (1.0_r8-Qpmin(ic,ng)/Qp)/(1.0_r8-Qpmin(ic,ng)/Qpmax(ic,ng))))
#endif

! silica
#if defined DARWIN_AUTO_FIX
! Things can go wrong if Bio(iSiO2) == 0 and ksatSiO2(ic,ng) == 0
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
            limitsi = Bio(iSiO2)/(Bio(iSiO2) + ksatSiO2(ic,ng))
#if defined DARWIN_SIQUOTA
            Qsi = MAX(BioMin(ng)*R_SiC(ic,ng), Bio(isi+ic-1)) / MAX(BioMin(ng), X)
#if defined DARWIN_FUTURE
            regQ = (MAX(0.0_r8, MIN(1.0_r8, (Qsimax(ic,ng) - Qsi)/(Qsimax(ic,ng) - Qsimin(ic,ng)) )))**hillnumSiO2(ic,ng)
#else
            regQ = MAX(0.0_r8, MIN(1.0_r8, (Qsimax(ic,ng) - Qsi)/(Qsimax(ic,ng) - Qsimin(ic,ng)) ))
#endif
            uptakeSiO2 = VmaxSiO2(ic,ng) * limitsi * regQ *uptakeTempFunc * X

! linear limitation
            limitsi = MAX(0.0_r8, MIN(1.0_r8, (Qsi - Qsimin(ic,ng))/(Qsimax(ic,ng) - Qsimin(ic,ng)) ))
#endif
#if defined DARWIN_AUTO_FIX
            ENDIF
#endif

! iron
            limitfe = Bio(iFeT)/(Bio(iFeT) + ksatFeT(ic,ng))
#if defined DARWIN_FEQUOTA
            Qfe = MAX(BioMin(ng)*R_FeC(ic,ng), Bio(ife+ic-1)) / MAX(BioMin(ng), X)
#if defined DARWIN_FUTURE
            regQ = (MAX(0.0_r8, MIN(1.0_r8, (Qfemax(ic,ng)-Qfe)/(Qfemax(ic,ng)-Qfemin(ic,ng)) )))**hillnumFeT(ic,ng)
#else
            regQ = MAX(0.0_r8, MIN(1.0_r8, (Qfemax(ic,ng)-Qfe)/(Qfemax(ic,ng)-Qfemin(ic,ng)) ))
#endif
            uptakeFeT = VmaxFeT(ic,ng) * limitfe * regQ *uptakeTempFunc * X

! normalized Droop limitation
            limitfe = MAX(0.0_r8, MIN(1.0_r8, (1.0_r8-Qfemin(ic,ng)/Qfe)/(1.0_r8-Qfemin(ic,ng)/Qfemax(ic,ng))))
#endif

! nitrogen
#if defined DARWIN_NQUOTA
! have nitrogen quota
            inhibNH4 = EXP(-amminhib(ic,ng)*Bio(iNH4))
            limitNH4 = Bio(iNH4)/(Bio(iNH4) + ksatNH4(ic,ng))
            limitNO2 = Bio(iNO2)/(Bio(iNO2) + ksatNO2(ic,ng))*inhibNH4
            limitNO3 = Bio(iNO3)/(Bio(iNO3) + ksatNO3(ic,ng))*inhibNH4
            Qn = MAX(BioMin(ng)*R_NC(ic,ng), Bio(in_+ic-1)) / MAX(BioMin(ng), X)
#if defined DARWIN_FUTURE
            regQ = (MAX(0.0_r8, MIN(1.0_r8, (Qnmax(ic,ng)-Qn)/(Qnmax(ic,ng)-Qnmin(ic,ng)) )))**hillnumDIN(ic,ng)
#else
            regQ = MAX(0.0_r8, MIN(1.0_r8, (Qnmax(ic,ng)-Qn)/(Qnmax(ic,ng)-Qnmin(ic,ng)) ))
#endif
            uptakeNH4 = VmaxNH4(ic,ng)*limitNH4*regQ*uptakeTempFunc*X
            uptakeNO2 = VmaxNO2(ic,ng)*limitNO2*regQ*uptakeTempFunc*X
            uptakeNO3 = VmaxNO3(ic,ng)*limitNO3*regQ*uptakeTempFunc*X
#if defined DARWIN_FEQUOTA
            uptakeNO3 = uptakeNO3 * limitfe
#endif
            uptakeN = MAX(uptakeNH4 + uptakeNO2 + uptakeNO3,VmaxN(ic,ng)*regQ*uptakeTempFunc*X*diazo(ic,ng))

! linear limitation
            limitn = MAX(0.0_r8, MIN(1.0_r8, (Qn - Qnmin(ic,ng))/(Qnmax(ic,ng) - Qnmin(ic,ng)) ))
#else /* not DARWIN_NQUOTA */
            Qn = R_NC(ic,ng)
            inhibNH4 = EXP(-amminhib(ic,ng)*Bio(iNH4))
            limitNH4 = useNH4(ic,ng)*Bio(iNH4)/(Bio(iNH4) + ksatNH4(ic,ng))
#if defined DARWIN_VERBOSE_PLANK
            IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,'(a20,f10.6," (",f10.6,a,f10.6,a,f12.6,a,")")')   &
     &          'limitNH4 = ',limitNH4,                                 &
     &          useNH4(ic,ng)*Bio(iNH4),                            &
     &          '/(',Bio(iNH4),'+',ksatNH4(ic,ng),')'
            END IF
#endif
            limitNO2 = useNO2(ic,ng)*Bio(iNO2)/(Bio(iNO2) +     &
     &        combNO(ic,ng)*(Bio(iNO3) + ksatNO3(ic,ng) -           &
     &        ksatNO2(ic,ng)) + ksatNO2(ic,ng))*inhibNH4
            limitNO3 = useNO3(ic,ng)*Bio(iNO3)/(combNO(ic,ng)*      &
     &        Bio(iNO2) + Bio(iNO3) + ksatNO3(ic,ng))*inhibNH4
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
! if diazo, all fracN* == 0 but want no N limitation
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
# if ! defined DARWIN_NO_IRONLIMIT
            limitnut = MIN(limitnut, limitfe)
# endif
#endif

            limitpCO2 = 1.0_r8

!==== growth ===========================================================
#if defined DARWIN_GEIDER

            alpha_I = 0.0_r8
            DO l = 1, nlam
#if defined DARWIN_VERBOSE_PLANK_OLD
            IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,'(a,i2,1x,a16,2x,10(f,1x,a,1x))') 'PLANK', ic_+ic-1, 'LAM alpha_I=', alphachl(ic,l,ng), '*', PAR(l)
            END IF
#endif
              alpha_I = alpha_I + alphachl(ic,l,ng)*PAR(l)
            ENDDO
#if defined DARWIN_PLANK_BUOYCTRL
            limitnut_save(ic)=limitnut
#endif
! NB: for quota, PCmax(ic,ng) = Vmax_c(ic)
            PCm = PCmax(ic,ng)*limitnut*photoTempFunc(ic)*limitpCO2
#if defined DARWIN_VERBOSE_PLANK
            IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,'(a20,f10.6," (",f10.6,3("*",f10.6),a,f10.4")")') &
     &          'PCm = ',PCm,                                           &
     &          PCmax(ic,ng),limitnut,photoTempFunc(ic),limitpCO2,  &
     &          '; PAR=',PAR(1)
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
            QChl=MAX(BioMin(ng)*R_ChlC(ic,ng),Bio(ichl+ic-1))/MAX(BioMin(ng),X)
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
              limitlight_save(ic)=photoTempFunc(ic)*(1.0_r8-EXP(-alpha_I_growth*chl2c/PCm))
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
              limitlight_save(ic)=0.0_r8
#endif
#if defined DARWIN_VERBOSE_PLANK
            IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,'(a20,f10.6)')                                    &
     &          'PC = ',PC
            END IF
#endif
            ENDIF

            IF (inhibGeider(ic,ng) .GT. 0.0_r8) THEN
! "total" PAR:
              tmp = alpha_I/alpha_mean(ic,ng)
              Ek = PCm/(chl2c*alpha_mean(ic,ng))
              EkoverE = Ek / tmp
              IF (tmp .GE. Ek) THEN
                PC = PC*EkoverE*inhibGeider(ic,ng)
              ENDIF
            ENDIF

#else /* not DARWIN_GEIDER */

            IF (PARtot .GT. PARmin(ng)) THEN
! only 1 waveband without DARWIN_GEIDER
              limitI = (1.0_r8 - EXP(-PARtot*ksatPAR(ic,ng)))*EXP(-PARtot*kinhPAR(ic,ng)) * normI(ic,ng)
              PC = PCmax(ic,ng)*limitnut*limitI*photoTempFunc(ic)*limitpCO2
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

! respiration goes straight back to DIC, so is subtracted from DIC uptake
#if defined DARWIN_FUTURE
            Xe = MAX(0.0_r8, X - Xmin(ic,ng))
            respir_c = respRate(ic,ng)*Xe*reminTempFunc
            uptakeDIC = growth - respir_c
            growth = uptakeDIC
#else
            uptakeDIC = growth
#endif

! non-quota elements are taken up with fixed stoichiometry
#if ! defined DARWIN_NQUOTA
            uptakeN = growth*R_NC(ic,ng)
#if defined DARWIN_FUTURE
! do not allow uptake of NH4/NO2 to go negative; excess respired N (net
! growth < 0) goes to NO3 instead
            IF (uptakeN .GE. 0.0_r8) THEN
              uptakeNH4 = uptakeN*fracNH4
              uptakeNO2 = uptakeN*fracNO2
              uptakeNO3 = uptakeN*fracNO3
            ELSE
              uptakeNH4 = 0.0_r8
              uptakeNO2 = 0.0_r8
              uptakeNO3 = uptakeN
            END IF
#else
            uptakeNH4 = uptakeN*fracNH4
            uptakeNO2 = uptakeN*fracNO2
            uptakeNO3 = uptakeN*fracNO3
#endif
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
            consumDIC_PIC = consumDIC_PIC + growth*R_PICPOC(ic,ng)
            consumDIC = consumDIC + uptakeDIC
            consumNH4 = consumNH4 + uptakeNH4
            consumNO2 = consumNO2 + uptakeNO2
            consumNO3 = consumNO3 + uptakeNO3
            consumPO4 = consumPO4 + uptakePO4
            consumSiO2 = consumSiO2 + uptakeSiO2
            consumFeT = consumFeT + uptakeFeT

            diags(iPP) = diags(iPP) + growth
            IF (ic .LE. nPPplank) THEN
              diags(iPPplank+ic-1) = diags(iPPplank+ic-1) + growth
            ENDIF
            IF (diazo(ic,ng) .GT. 0.0_r8) THEN
             diags(iNfix)=diags(iNfix)+uptakeN-uptakeNH4-uptakeNO2-uptakeNO3
            ENDIF
#if defined DIAGNOSTICS_BIO
            diags4d(ic,idNPP4d)=diags4d(ic,idNPP4d)+            &
     &          growth * 86400.0_r8
            ! in units of d-1
            diags4d(ic,idPGrowthRate)=diags4d(ic,idPGrowthRate)+&
     &          PC*ngrow * 86400.0_r8
            diags4d(ic,idNUpNorm)=diags4d(ic,idNUpNorm)+        &
     &          uptakeN/(Bio(iNO2)+Bio(iNO3)+Bio(iNH4))*    &
     &          86400.0_r8
#endif

!=======================================================================
#if defined DARWIN_VERBOSE_PLANK_OLD
            IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,'(a,i2,1x,a16,2x,f)') 'PLANK', ic_+ic-1, '+uptakeDIC', dtbio(ng)*(+ uptakeDIC)
            END IF
#endif
#if defined DARWIN_VERBOSE_PLANK
            IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,'(a20,f10.6," (",f10.6,"+",f10.6,"*",f10.6,")")') &
     &          'new value = ',Bio(ic_+ic-1)+ dtbio(ng)*(+ uptakeDIC), &
     &          Bio(ic_+ic-1),dtbio(ng),uptakeDIC
              write(*,'(a20,f14.10)') 'change : ',                      &
     &           dtbio(ng)*(+ uptakeDIC)
            END IF
#endif
            Bio(ic_+ic-1)=Bio(ic_+ic-1)  + dtbio(ng)*(+ uptakeDIC)
#if defined DARWIN_NQUOTA
            Bio(in_+ic-1)=Bio(in_+ic-1)  + dtbio(ng)*(+ uptakeN)
#endif
#if defined DARWIN_PQUOTA
            Bio(ip_+ic-1)=Bio(ip_+ic-1)  + dtbio(ng)*(+ uptakePO4)
#endif
#if defined DARWIN_SIQUOTA
            Bio(isi+ic-1)=Bio(isi+ic-1)  + dtbio(ng)*(+ uptakeSiO2)
#endif
#if defined DARWIN_FEQUOTA
            Bio(ife+ic-1)=Bio(ife+ic-1)  + dtbio(ng)*(+ uptakeFeT)
#endif
#if defined DARWIN_CHLQUOTA
            Bio(iChl+ic-1)=Bio(iChl+ic-1)  + dtbio(ng)*(+ synthChl)
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

          DO ic = 1, nplank
           IF (bactType(ic,ng) .NE. 0) THEN

#if defined DARWIN_MINVAL_X
            X = MAX(DARWIN_MINVAL_X, Bio(ic_+ic-1))
#else
            X = MAX(0.0_r8, Bio(ic_+ic-1))
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
              muO = yieldO2(ic,ng)*pcoefO2(ng)*Bio(iO2)
            ELSEIF (isDenit(ic,ng) .NE. 0) THEN
#if defined DARWIN_FUTURE
              muO = yieldNO3(ic,ng)*pmaxDIN(ng)*Bio(iNO3)/(Bio(iNO3) + ksatDIN(ng))*hetTempFunc
#else
              muO = yieldNO3(ic,ng)*pmaxDIN(ng)*Bio(iNO3)/(Bio(iNO3) + ksatDIN(ng))*reminTempFunc
#endif
            ENDIF

! POM-consuming (particle-associated)
            IF (bactType(ic,ng) .EQ. 1) THEN

#if defined DARWIN_FUTURE
              PCm = yield(ic,ng)*pmaxPON(ng)*hetTempFunc
#else
              PCm = yield(ic,ng)*pmaxPON(ng)*reminTempFunc
#endif
              muPON = PCm*Bio(iPON)/(Bio(iPON) + ksatPON(ic,ng))
              muPOC = PCm*Bio(iPOC)/(Bio(iPOC) + ksatPOC(ic,ng))
              muPOP = PCm*Bio(iPOP)/(Bio(iPOP) + ksatPOP(ic,ng))
              muPOFe = PCm*Bio(iPOFe)/(Bio(iPOFe) + ksatPOFe(ic,ng))
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
#if defined DARWIN_BACT_ALLOMETRIC
# if defined DARWIN_FUTURE
              PCm = yield(ic,ng)*PCmax(ic,ng)*hetTempFunc
# else
              PCm = yield(ic,ng)*PCmax(ic,ng)*reminTempFunc
# endif
#else
# if defined DARWIN_FUTURE
              PCm = yield(ic,ng)*pmaxDON(ng)*hetTempFunc
# else
              PCm = yield(ic,ng)*pmaxDON(ng)*reminTempFunc
# endif
#endif
              muDON = PCm*Bio(iDON)/(Bio(iDON) + ksatDON(ic,ng))
              muDOC = PCm*Bio(iDOC)/(Bio(iDOC) + ksatDOC(ic,ng))
              muDOP = PCm*Bio(iDOP)/(Bio(iDOP) + ksatDOP(ic,ng))
              muDOFe = PCm*Bio(iDOFe)/(Bio(iDOFe) + ksatDOFe(ic,ng))
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
              diags(iPPplank+ic-1) = diags(iPPplank+ic-1) + growth
            ENDIF

#if defined DARWIN_VERBOSE_PLANK_OLD
            IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,'(a,i2,1x,a16,2x,f)') 'PLANK', ic_+ic-1, '+growth', dtbio(ng)*(+ growth)
            END IF
#endif
            Bio(ic_+ic-1)=Bio(ic_+ic-1)  + dtbio(ng)*(+ growth)

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

          Bio(iDIC )=Bio(iDIC )  + dtbio(ng)*(- consumDIC - consumDIC_PIC)
          Bio(iNH4 )=Bio(iNH4 )  + dtbio(ng)*(- consumNH4)
          Bio(iNO2 )=Bio(iNO2 )  + dtbio(ng)*(- consumNO2)
          Bio(iNO3 )=Bio(iNO3 )  + dtbio(ng)*(- consumNO3)
          Bio(iPO4 )=Bio(iPO4 )  + dtbio(ng)*(- consumPO4)
          Bio(iSiO2)=Bio(iSiO2)  + dtbio(ng)*(- consumSiO2)
          Bio(iFeT )=Bio(iFeT )  + dtbio(ng)*(- consumFeT)
#if defined DARWIN_VERBOSE_NUT
            IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,*) 'consumption'
              write(*,'(a20,f18.12,9(a,f18.12))')                       &
     &          'DIC = ',Bio(iDIC ),                                &
     &          ' (',Bio(iDIC )-dtbio(ng)*(consumDIC - consumDIC_PIC), &
     &          '+',dtbio(ng),'*(-',consumDIC,'-',consumDIC_PIC,')'
              write(*,'(a20,f18.12,9(a,f18.12))')                       &
     &          'NH4 = ',Bio(iNH4 ),                                &
     &          ' (',Bio(iNH4 )-dtbio(ng)*(- consumNH4),               &
     &          '+',dtbio(ng),'*(-',consumNH4,')'
              write(*,'(a20,f18.12,9(a,f18.12))')                       &
     &          'NO2 = ',Bio(iNO2 ),                                &
     &          ' (',Bio(iNO2 )-dtbio(ng)*(- consumNO2),               &
     &          '+',dtbio(ng),'*(-',consumNO2,')'
              write(*,'(a20,f18.12,9(a,f18.12))')                       &
     &          'NO3 = ',Bio(iNO3 ),                                &
     &          ' (',Bio(iNO3 )-dtbio(ng)*(- consumNO3),               &
     &          '+',dtbio(ng),'*(-',consumNO3,')'
            END IF
#endif

! parameterized remineralization; want to set all K except KPOSi(ng) to zero
! if running with bacteria
! optional smooth O2 limitation (half-saturation ksatO2remin; 0 disables it)
#if defined DARWIN_CARBON
          IF (ksatO2remin(ng).GT.0.0_r8) THEN
            tmp = Bio(iO2)/(Bio(iO2) + ksatO2remin(ng))
          ELSE
            tmp = 1.0_r8
          END IF
#else
          tmp = 1.0_r8
#endif
          respDOC = reminTempFunc*Kdoc(ng) *Bio(iDOC)*tmp
          respDON = reminTempFunc*Kdon(ng) *Bio(iDON)*tmp
          respDOP = reminTempFunc*Kdop(ng) *Bio(iDOP)*tmp
          respDOFe = reminTempFunc*KdoFe(ng)*Bio(iDOFe)*tmp
          respPOC = reminTempFunc*KPOC(ng) *Bio(iPOC)*tmp
          respPON = reminTempFunc*KPON(ng) *Bio(iPON)*tmp
          respPOP = reminTempFunc*KPOP(ng) *Bio(iPOP)*tmp
          respPOSi = reminTempFunc*KPOSi(ng)*Bio(iPOSi)*tmp
          respPOFe = reminTempFunc*KPOFe(ng)*Bio(iPOFe)*tmp

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

#if defined DARWIN_FUTURE
          IF (darwin_disscSelect(ng).EQ.2) THEN
! Naviaux et al. 2019, Marine Chemistry dissolution rate law
            IF (omegaCl.LT.1.0_r8) THEN
              IF (omegaCl.GT.0.8272_r8) THEN
                disscPIC = Bio(iPIC)*5.22e-9_r8*                    &
     &            (1.0_r8 - omegaCl)**0.11_r8
              ELSE
                disscPIC = Bio(iPIC)*1.65e-5_r8*                    &
     &            (1.0_r8 - omegaCl)**4.7_r8
              END IF
            ELSE
              disscPIC = 0.0_r8
            END IF
          ELSE IF (darwin_disscSelect(ng).EQ.1) THEN
! Keir 1980
            IF (omegaCl.LT.1.0_r8) THEN
              disscPIC = Bio(iPIC)*darwin_KeirCoeff(ng)*            &
     &          (1.0_r8 - omegaCl)**darwin_KeirExp(ng)
            ELSE
              disscPIC = 0.0_r8
            END IF
          ELSE
            disscPIC = Kdissc(ng)*Bio(iPIC)
          END IF
#else
          disscPIC = Kdissc(ng)*Bio(iPIC)
#endif

! nitrogen chemistry
! NH4 -> NO2 -> NO3 by bacterial action, parameterized
          prodNO2 = Knita(ng)*Bio(iNH4)
          prodNO3 = Knitb(ng)*Bio(iNO2)
#if defined DARWIN_VERBOSE_NUT
          IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
            write(*,*) 'bacterial production'
            write(*,'(a20,f18.12," (",9(f18.12,a))')                    &
     &        'prodNO2 = ',prodNO2,                                     &
     &        Knita(ng),'*',Bio(iNH4),')'
            write(*,'(a20,f18.12," (",9(f18.12,a))')                    &
     &        'prodNO3 = ',prodNO3,                                     &
     &        Knitb(ng),'*',Bio(iNO2),')'
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
          degrCDOM_DOP = reminTempFunc*Bio(iCDOM)*(CDOMdegrd(ng)+CDOMbleach(ng)*MIN(1.0_r8, PARtot/PARCDOM(ng)))
          degrCDOM_DOC = R_CP_CDOM(ng) * degrCDOM_DOP
          degrCDOM_DON = R_NP_CDOM(ng) * degrCDOM_DOP
          degrCDOM_DOFe = R_FeP_CDOM(ng) * degrCDOM_DOP
#endif

#if defined DARWIN_DENIT
          IF (Bio(iO2) .LT. O2crit(ng) .AND. Bio(iNO3) .LT. NO3crit(ng)) THEN
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
            degrCDOM_DOP = reminTempFunc*Bio(iCDOM)*CDOMbleach(ng)*MIN(1.0_r8, PARtot/PARCDOM(ng))
            degrCDOM_DON = R_NP_CDOM(ng) * degrCDOM_DOP
            degrCDOM_DOFe = R_FeP_CDOM(ng) * degrCDOM_DOP
            degrCDOM_DOC = R_CP_CDOM(ng) * degrCDOM_DOP
#endif
          ENDIF
#endif /* DARWIN_DENIT */

!==== apply tendencies =================================================

#if defined DARWIN_CARBON
! production of O2 by photosynthesis
          Bio(iO2 )=Bio(iO2 )  + dtbio(ng)*(+ R_OP(ng)*consumPO4)
! loss of O2 by remineralization
          IF (Bio(iO2) .GT. O2crit(ng)) THEN
            Bio(iO2)=Bio(iO2)  + dtbio(ng)*(- consumO2)
          ENDIF

          Bio(iALK)=Bio(iALK)  + dtbio(ng)*(- (prodNO3 - consumNO3)- 2.0_r8*(consumDIC_PIC - disscPIC))
#endif /* DARWIN_CARBON */

          Bio(iDIC )=Bio(iDIC )  + dtbio(ng)*(+ reminDOC + disscPIC)
          Bio(iNH4 )=Bio(iNH4 )  + dtbio(ng)*(+ reminDON - prodNO2)
          Bio(iNO2 )=Bio(iNO2 )  + dtbio(ng)*(+ prodNO2 - prodNO3)
          Bio(iNO3 )=Bio(iNO3 )  + dtbio(ng)*(+ prodNO3)
#if defined DARWIN_VERBOSE_NUT
            IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,*) 'remineralization'
              write(*,'(a20,f18.12,9(a,f18.12))')                       &
     &          'DIC = ',Bio(iDIC ),                                &
     &          ' (',Bio(iDIC )-dtbio(ng)*(+ reminDOC + disscPIC),     &
     &          '+',dtbio(ng),'*(',reminDOC,'+',disscPIC,')'
              write(*,'(a20,f18.12,9(a,f18.12))')                       &
     &          'NH4 = ',Bio(iNH4 ),                                &
     &          ' (',Bio(iNH4 )-dtbio(ng)*(+ reminDON - prodNO2),      &
     &          '+',dtbio(ng),'*(',reminDON,'-',prodNO2,')'
              write(*,'(a20,f18.12,9(a,f18.12))')                       &
     &          'NO2 = ',Bio(iNO2 ),                                &
     &          ' (',Bio(iNO2 )-dtbio(ng)*(+ prodNO2 - prodNO3),       &
     &          '+',dtbio(ng),'*(',prodNO2,'-',prodNO3,')'
              write(*,'(a20,f18.12,9(a,f18.12))')                       &
     &          'NO3 = ',Bio(iNO3 ),                                &
     &          ' (',Bio(iNO3 )-dtbio(ng)*(+ prodNO3),                 &
     &          '+',dtbio(ng),'*',prodNO3,')'
            END IF
#endif
          diags(iDenitN) = 0.0_r8
#if defined DARWIN_DENIT
          IF (Bio(iO2) .LT. O2crit(ng)) THEN
            denitNH4 = reminDON
            denit = denit_NP(ng)*reminDOP
#if ! defined DARWIN_CDOM
            denitNH4 = denitNH4 + reminPON
            denit = denit + denit_NP(ng)*reminPOP
#endif
            diags(iDenit) = denit
            Bio(iNH4)=Bio(iNH4)  + dtbio(ng)*(- denitNH4)
            Bio(iNO3)=Bio(iNO3)  + dtbio(ng)*(- denit_NO3(ng)/denit_NP(ng)*denit)
            Bio(iALK)=Bio(iALK)  + dtbio(ng)*(+ denit_NO3(ng)/denit_NP(ng)*denit)
            diags(iDenitN) = denitNH4 + denit_NO3(ng)/denit_NP(ng)*denit
#if defined DARWIN_VERBOSE_NUT
            IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,*) 'denitrification'
              write(*,'(a20,f18.12,9(a,f18.12))')                       &
     &          'NH4 = ',Bio(iNH4 ),                                &
     &          ' (',Bio(iNH4 )-dtbio(ng)*(- denitNH4),                &
     &          '+',dtbio(ng),'*(-',denitNH4,')'
              write(*,'(a20,f18.12,9(a,f18.12))')                       &
     &          'NO3 = ',Bio(iNO3 ),                                &
     &          ' (',Bio(iNO3 )-dtbio(ng)*(-denit_NO3(ng)/denit_NP(ng) &
     &          *denit),                                                &
     &          '+',dtbio(ng),'*(-',denit_NO3(ng),'/',denit_NP(ng),        &
     &          '*',denit,')'
            END IF
#endif
          ENDIF
#endif /* DARWIN_DENIT */

          Bio(iPO4 )=Bio(iPO4 )  + dtbio(ng)*(+ reminDOP)
          Bio(iFeT )=Bio(iFeT )  + dtbio(ng)*(+ reminDOFe)
          Bio(iSiO2)=Bio(iSiO2)  + dtbio(ng)*(+ reminPOSi)

! DOC is created by #4 PA-assoc solubilization and consumed by #5
          Bio(iDOC )=Bio(iDOC )  + dtbio(ng)*(+ solubilPOC - consumDOC)
          Bio(iDON )=Bio(iDON )  + dtbio(ng)*(+ solubilPON - consumDON)
          Bio(iDOP )=Bio(iDOP )  + dtbio(ng)*(+ solubilPOP - consumDOP)
          Bio(iDOFe)=Bio(iDOFe)  + dtbio(ng)*(+ solubilPOFe - consumDOFe)

          Bio(iPIC )=Bio(iPIC )  + dtbio(ng)*(- disscPIC)
          Bio(iPOC )=Bio(iPOC )  + dtbio(ng)*(- consumPOC)
          Bio(iPON )=Bio(iPON )  + dtbio(ng)*(- consumPON)
          Bio(iPOP )=Bio(iPOP )  + dtbio(ng)*(- consumPOP)
          Bio(iPOFe)=Bio(iPOFe)  + dtbio(ng)*(- consumPOFe)
          Bio(iPOSi)=Bio(iPOSi)  + dtbio(ng)*(- consumPOSi)

#if defined DARWIN_CDOM
          Bio(iDOC )=Bio(iDOC )  + dtbio(ng)*(+ reminPOC - reminPOC_CDOM + degrCDOM_DOC)
          Bio(iDON )=Bio(iDON )  + dtbio(ng)*(+ reminPON - reminPON_CDOM + degrCDOM_DON)
          Bio(iDOP )=Bio(iDOP )  + dtbio(ng)*(+ reminPOP - reminPOP_CDOM + degrCDOM_DOP)
          Bio(iDOFe)=Bio(iDOFe)  + dtbio(ng)*(+ reminPOFe - reminPOFe_CDOM + degrCDOM_DOFe)

          Bio(iCDOM)=Bio(iCDOM)  + dtbio(ng)*(+ reminPOP_CDOM - degrCDOM_DOP)
#else
          Bio(iDIC )=Bio(iDIC )  + dtbio(ng)*(+ reminPOC)
          Bio(iNH4 )=Bio(iNH4 )  + dtbio(ng)*(+ reminPON)
          Bio(iPO4 )=Bio(iPO4 )  + dtbio(ng)*(+ reminPOP)
          Bio(iFeT )=Bio(iFeT )  + dtbio(ng)*(+ reminPOFe)
#endif /* DARWIN_CDOM */

          diags(iConsDIN) = consumNH4 + consumNO2 + consumNO3
          diags(iConsPO4) = consumPO4
          diags(iConsSi) = consumSiO2
          diags(iConsFe) = consumFeT
!
!  converted from "gud_grazing.F" using darwinfortran_to_romsfortran
!  (written by J. Paul Mattern)
!
!==== make all bio fields non-negative and compute quotas ==============

! fixed carbon quota, for now 1.0_r8 (may change later)
          Qcgraz(:) = 1.0_r8
#if defined DARWIN_MINVAL_X
          Xgraz(:) = MAX(DARWIN_MINVAL_X, Bio(ic_:ec_))/Qcgraz(:)
#else
          Xgraz(:) = MAX(0.0_r8, Bio(ic_:ec_))/Qcgraz(:)
#endif
! other elements: get quota from corresponding ptracer or set to fixed
! ratio if not variable.
          Xi(:) = 1.0_r8/MAX(BioMin(ng), Xgraz(:))
#if defined DARWIN_NQUOTA
          Qngraz(:) = MAX(0.0_r8, Bio(in_:en_))*Xi(:)
#else
          Qngraz(:) = R_NC(:,ng)
#endif
#if defined DARWIN_PQUOTA
          Qpgraz(:) = MAX(0.0_r8, Bio(ip_:ep_))*Xi(:)
#else
          Qpgraz(:) = R_PC(:,ng)
#endif
#if defined DARWIN_SIQUOTA
          Qsigraz(:) = MAX(0.0_r8, Bio(isi:esi))*Xi(:)
#else
          Qsigraz(:) = R_SiC(:,ng)
#endif
#if defined DARWIN_FEQUOTA
          Qfegraz(:) = MAX(0.0_r8, Bio(ife:efe))*Xi(:)
#else
          Qfegraz(:) = R_FeC(:,ng)
#endif
#if defined DARWIN_CHLQUOTA
          QChlgraz(:) = MAX(0.0_r8, Bio(ichl:echl))*Xi(1:nChl)
#endif

          preygraz(:) = 0.0_r8
          predgrazc(:) = 0.0_r8
#if defined DARWIN_NQUOTA
          predgrazn(:) = 0.0_r8
#endif
#if defined DARWIN_PQUOTA
          predgrazp(:) = 0.0_r8
#endif
#if defined DARWIN_FEQUOTA
          predgrazfe(:) = 0.0_r8
#endif
          graz2POC = 0.0_r8
          graz2PON = 0.0_r8
          graz2POP = 0.0_r8
          graz2POSI = 0.0_r8
          graz2POFE = 0.0_r8
          graz2OC = 0.0_r8
          graz2ON = 0.0_r8
          graz2OP = 0.0_r8
          graz2OFE = 0.0_r8
          graz2PIC = 0.0_r8

          regQn = 1.0_r8
          regQp = 1.0_r8
          regQfe = 1.0_r8
          regQc = 1.0_r8
#if defined DARWIN_VERBOSE_PLANK
          preygraz_rate(:) = 0.0_r8
          grazphy_all(:,:) = 0.0_r8
#endif
#if defined DIAGNOSTICS_BIO
          diags4d(:,idGrazRt) = 0.0_r8
          diags4d(:,idGrazPr) = 0.0_r8
#endif

!=======================================================================
          DO iz = 1, nplank
           IF (isPred(iz,ng).NE.0) THEN

! regulate grazing near full quota
            regQc = 1.0_r8
#if defined DARWIN_NQUOTA
            regQn = MAX(0.0_r8, MIN(1.0_r8, (Qnmax(iz,ng)-Qngraz(iz))/(Qnmax(iz,ng)-Qnmin(iz,ng)) ))
            regQc = MIN(regQc, 1.0_r8 - regQn)
            regQn = regQn**hillnumGraz(ng)
#endif
#if defined DARWIN_PQUOTA
            regQp = MAX(0.0_r8, MIN(1.0_r8, (Qpmax(iz,ng)-Qpgraz(iz))/(Qpmax(iz,ng)-Qpmin(iz,ng)) ))
            regQc = MIN(regQc, 1.0_r8 - regQp)
            regQp = regQp**hillnumGraz(ng)
#endif
#if defined DARWIN_FEQUOTA
            regQfe= MAX(0.0_r8, MIN(1.0_r8, (Qfemax(iz,ng)-Qfegraz(iz))/(Qfemax(iz,ng)-Qfemin(iz,ng)) ))
            regQc = MIN(regQc, 1.0_r8 - regQfe)
            regQfe=regQfe**hillnumGraz(ng)
#endif
            regQc = regQc**hillnumGraz(ng)

            sumprey = 0.0_r8
            sumpref = 0.0_r8
            DO ip = 1, nplank
             IF (isPrey(ip,ng).NE.0) THEN
             sumprey = sumprey + palat(ip,iz,ng)*Xgraz(ip)
#if defined DARWIN_GRAZING_SWITCH
             sumpref = sumpref + palat(ip,iz,ng)*palat(ip,iz,ng)*Xgraz(ip)*Xgraz(ip)
#else
#if defined DARWIN_VERBOSE_PLANK_OLD
            IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,'(a,i2,1x,a1,i0,a14,2x,10(f,1x,a,1x))') 'PLANK', ic_+iz-1, '(',ip,') sumpref=', sumpref, '+', palat(ip,iz,ng), '*', Xgraz(ip)
            END IF
#endif
             sumpref = sumpref + palat(ip,iz,ng)*Xgraz(ip)
#endif
             ENDIF
            ENDDO
            sumprey = MAX(0.0_r8, sumprey - phygrazmin(ng))
            sumpref = MAX(phygrazmin(ng), sumpref)
#if defined DARWIN_VERBOSE_PLANK_OLD
              IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
                write(*,'(a,i2,1x,a16,2x,10(f,1x,a,1x))') 'PLANK', ic_+iz-1, 'sumprey=', sumprey
!                write(*,'(a,i2,1x,a16,2x,10(f,1x,a,1x))') 'PLANK',      &
!     &            ic_+iz-1, 'tmp=', grazemax(iz,ng), '*',               &
!     &            grazTempFunc(iz), '*',                            &
!     &            Xgraz(iz), '*', (sumprey**hollexp(ng)/(sumprey**hollexp(ng)+kgrazesat(iz,ng)**hollexp(ng))), '*', (1.0_r8 - EXP(-inhib_graz(ng)*sumprey)), '**', inhib_graz_exp(ng)
              END IF
#endif
            tmp = grazemax(iz,ng)*grazTempFunc(iz)**tempGraz(iz,ng)*Xgraz(iz)*(sumprey**hollexp(ng)/(sumprey**hollexp(ng)+kgrazesat(iz,ng)**hollexp(ng)))*(1.0_r8 - EXP(-inhib_graz(ng)*sumprey))**inhib_graz_exp(ng)

            totkillc = 0.0_r8
            totkilln = 0.0_r8
            totkillp = 0.0_r8
            totkillsi = 0.0_r8
            totkillfe = 0.0_r8
#if defined DARWIN_CARBON
            totkillPIC= 0.0_r8
#endif
            totkillexpc = 0.0_r8
            totkillexpn = 0.0_r8
            totkillexpp = 0.0_r8
            totkillexpfe = 0.0_r8
            predexpc = 0.0_r8
            predexpn = 0.0_r8
            predexpp = 0.0_r8
            predexpfe = 0.0_r8
            DO ip = 1, nplank
             IF (isPrey(ip,ng).NE.0) THEN
!#if defined DARWIN_VERBOSE_PLANK
!              IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
!                write(*,'(a,i2.2,a,f10.6,a)') 'grazing on plankton',ip, &
!     &            ' (initial value: ',Xgraz(ip),')'
!              END IF
!#endif
#if defined DARWIN_GRAZING_SWITCH
              grazphy = tmp*palat(ip,iz,ng)*palat(ip,iz,ng)*Xgraz(ip)*Xgraz(ip)/sumpref
#else
# if defined DARWIN_VERBOSE_PLANK_OLD
              IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
                write(*,'(a,i2,1x,a16,2x,10(f,1x,a,1x))') 'PLANK', ic_+ip-1, 'grazphy=', tmp, '*', palat(ip,iz,ng), '*', Xgraz(ip), '/', sumpref
              END IF
# endif
              grazphy = tmp*palat(ip,iz,ng)*Xgraz(ip)/sumpref
!#if defined DARWIN_VERBOSE_PLANK
!              IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
!               write(*,'(a20,f10.6," (",f10.6,3(a,f10.6),")",a,i2.2,a)')&
!     &            'grazphy = ',grazphy,                                 &
!     &            tmp,'*',palat(ip,iz,ng),'*',Xgraz(ip),                    &
!     &            '/',sumpref,' (due to plankton',iz,')'
!                preygraz_rate(ip)=preygraz_rate(ip) + grazphy/Xgraz(ip)
!              END IF
!#endif
#endif

              preygraz(ip) = preygraz(ip) + grazphy
#if defined DIAGNOSTICS_BIO
              diags4d(ip,idGrazRt) = diags4d(ip,idGrazRt) +     &
     &          grazphy * 86400.0_r8
              ! in units of d-1
              diags4d(ip,idGrazPr) = diags4d(ip,idGrazPr) +     &
     &          grazphy/Xgraz(ip) * 86400.0_r8
#endif
#if defined DARWIN_VERBOSE_PLANK && ! defined DARWIN_GRAZING_SWITCH
              IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
                preygraz_rate(ip)=preygraz_rate(ip) + grazphy/Xgraz(ip)
                grazphy_all(ip,iz)=grazphy
                IF (ip.le.nPhoto.and.                                  &
     &              .not.ANY(isPred(iz+1:nplank,ng).NE.0)) THEN
                  write(*,'(a,i2.2,a,f10.6,a)')                        &
     &              'graz on plankton',ip,                              &
     &              ' (initial value: ',Xgraz(ip),')'
                  write(*,'(a20,f14.10)')                               &
     &              'preygraz = ',preygraz(ip)
                  write(*,'(a20,f14.10)') 'change : ',                  &
     &              dtbio(ng)*(- preygraz(ip))
                  write(*,'(a20,f14.10,99(", ",f14.10))') 'contrib : ', &
     &              dtbio(ng)*grazphy_all(ip,:)
                  write(*,'(a20,f10.6)')                                &
     &              'graz rate (d-1): ',preygraz_rate(ip)*86400.0_r8
                END IF
              END IF
#endif

              totkillc = totkillc + grazphy
              totkilln = totkilln + grazphy*Qngraz (ip)
              totkillp = totkillp + grazphy*Qpgraz (ip)
              totkillsi = totkillsi + grazphy*Qsigraz(ip)
              totkillfe = totkillfe + grazphy*Qfegraz(ip)
#if defined DARWIN_CARBON
              totkillPIC= totkillPIC+ grazphy*R_PICPOC(ip,ng)
#endif

              expFrac = ExportFracPreyPred(ip,iz,ng)
              totkillexpc = totkillexpc + expFrac*grazphy
              totkillexpn = totkillexpn + expFrac*grazphy*Qngraz (ip)
              totkillexpp = totkillexpp + expFrac*grazphy*Qpgraz (ip)
              totkillexpfe = totkillexpfe + expFrac*grazphy*Qfegraz(ip)

#if defined DARWIN_VERBOSE_PLANK_OLD
            IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,'(a,i2,1x,a16,2x,10(f,1x,a,1x))') 'PLANK', ic_+iz-1, 'predgrazc=', grazphy, '*',asseff(ip,iz,ng),'*',regQc
            END IF
#endif
              predgrazc(iz) = predgrazc(iz) + grazphy*asseff(ip,iz,ng)*regQc
              predexpc = predexpc + expFrac*grazphy*asseff(ip,iz,ng)*regQc
#if defined DARWIN_NQUOTA
              predgrazn(iz) = predgrazn(iz) + grazphy*asseff(ip,iz,ng)*regQn*Qngraz(ip)
              predexpn = predexpn + expFrac*grazphy*asseff(ip,iz,ng)*regQn*Qngraz(ip)
#endif
#if defined DARWIN_PQUOTA
              predgrazp(iz) = predgrazp(iz) + grazphy*asseff(ip,iz,ng)*regQp*Qpgraz(ip)
              predexpp = predexpp + expFrac*grazphy*asseff(ip,iz,ng)*regQp*Qpgraz(ip)
#endif
#if defined DARWIN_FEQUOTA
              predgrazfe(iz) = predgrazfe(iz) + grazphy*asseff(ip,iz,ng)*regQfe*Qfegraz(ip)
              predexpfe = predexpfe + expFrac*grazphy*asseff(ip,iz,ng)*regQfe*Qfegraz(ip)
#endif
             ENDIF
            ENDDO

            graz2OC = graz2OC + totkillc - predgrazc(iz)
            graz2POC = graz2POC + totkillexpc - predexpc

#if defined DARWIN_NQUOTA
            graz2ON = graz2ON + totkilln - predgrazn(iz)
            graz2PON = graz2PON + totkillexpn - predexpn
#else
            graz2ON = graz2ON + totkilln - predgrazc(iz)*Qngraz(iz)
            graz2PON = graz2PON + totkillexpn - predexpc *Qngraz(iz)
#endif

#if defined DARWIN_PQUOTA
            graz2OP = graz2OP + totkillp - predgrazp(iz)
            graz2POP = graz2POP + totkillexpp - predexpp
#else
            graz2OP = graz2OP + totkillp - predgrazc(iz)*Qpgraz(iz)
            graz2POP = graz2POP + totkillexpp - predexpc *Qpgraz(iz)
#endif

#if defined DARWIN_FEQUOTA
            graz2OFe = graz2OFe + totkillfe - predgrazfe(iz)
            graz2POFe = graz2POFe + totkillexpfe - predexpfe
#else
            graz2OFe = graz2OFe + totkillfe - predgrazc(iz)*Qfegraz(iz)
            graz2POFe = graz2POFe + totkillexpfe - predexpc *Qfegraz(iz)
#endif

            graz2POSi = graz2POSi + totkillsi

#if defined DARWIN_CARBON
            graz2PIC = graz2PIC + totkillPIC
#endif

! end predator loop
           ENDIF
          ENDDO

!==== tendencies =======================================================

          Bio(iDOC )=Bio(iDOC )  + dtbio(ng)*(+ graz2OC - graz2POC)
          Bio(iDON )=Bio(iDON )  + dtbio(ng)*(+ graz2ON - graz2PON)
          Bio(iDOP )=Bio(iDOP )  + dtbio(ng)*(+ graz2OP - graz2POP)
          Bio(iDOFe)=Bio(iDOFe)  + dtbio(ng)*(+ graz2OFe - graz2POFe)
          Bio(iPOC )=Bio(iPOC )  + dtbio(ng)*(+ graz2POC)
          Bio(iPON )=Bio(iPON )  + dtbio(ng)*(+ graz2PON)
          Bio(iPOP )=Bio(iPOP )  + dtbio(ng)*(+ graz2POP)
          Bio(iPOSi)=Bio(iPOSi)  + dtbio(ng)*(+ graz2POSi)
          Bio(iPOFe)=Bio(iPOFe)  + dtbio(ng)*(+ graz2POFe)
#if defined DARWIN_CARBON
          Bio(iPIC )=Bio(iPIC )  + dtbio(ng)*(+ graz2PIC)
#endif
#if defined DARWIN_CDOM
          graz2CDOM = fracCDOM(ng)*(graz2OP - graz2POP)
          Bio(iCDOM)=Bio(iCDOM)  + dtbio(ng)*(+ graz2CDOM)
          Bio(iDOC )=Bio(iDOC )  + dtbio(ng)*(- R_CP_CDOM(ng)*graz2CDOM)
          Bio(iDON )=Bio(iDON )  + dtbio(ng)*(- R_NP_CDOM(ng)*graz2CDOM)
          Bio(iDOP )=Bio(iDOP )  + dtbio(ng)*(- graz2CDOM)
          Bio(iDOFe)=Bio(iDOFe)  + dtbio(ng)*(- R_FeP_CDOM(ng)*graz2CDOM)
#endif
          DO ip = 1, nplank
           IF (isPrey(ip,ng).NE.0) THEN
#if defined DARWIN_VERBOSE_PLANK_OLD
            IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,'(a,i2,1x,a16,2x,f)') 'PLANK', ic_+ip-1,           '-preygraz', dtbio(ng)*(- preygraz(ip))
            END IF
#endif
           Bio(ic_+ip-1)= Bio(ic_+ip-1)  + dtbio(ng)*(- preygraz(ip))
           ENDIF
          ENDDO
          DO iz = 1, nplank
           IF (isPred(iz,ng).NE.0) THEN
#if defined DARWIN_VERBOSE_PLANK_OLD
            IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,'(a,i2,1x,a16,2x,f)') 'PLANK', ic_+iz-1,           '+predgrazc', dtbio(ng)*(+ predgrazc(iz))
            END IF
#endif
           Bio(ic_+iz-1)=Bio(ic_+iz-1)  + dtbio(ng)*(+ predgrazc(iz))
           ENDIF
          ENDDO
#if defined DARWIN_NQUOTA
          Bio(in_:en_)=Bio(in_:en_)  + dtbio(ng)*(+ predgrazn(:) - preygraz(:)*Qngraz(:))
#endif
#if defined DARWIN_PQUOTA
          Bio(ip_:ep_)=Bio(ip_:ep_)  + dtbio(ng)*(+ predgrazp(:) - preygraz(:)*Qpgraz(:))
#endif
#if defined DARWIN_SIQUOTA
          Bio(isi:esi)=Bio(isi:esi)  + dtbio(ng)*(- preygraz(:)*Qsigraz(:))
#endif
#if defined DARWIN_FEQUOTA
          Bio(ife:efe)=Bio(ife:efe)  + dtbio(ng)*(+ predgrazfe(:) - preygraz(:)*Qfegraz(:))
#endif
#if defined DARWIN_CHLQUOTA
          Bio(iChl:eChl)=Bio(iChl:eChl)  + dtbio(ng)*(- preygraz(1:nChl)*QChlgraz(:))
#endif

#if defined DIAGNOSTICS_BIO
          DO ip = 1, nGRplank
            diags(iGRplank+ip-1) = preygraz(ip)
          ENDDO
#endif

!==== mortality ========================================================
          exude_DOC = 0.0_r8
          exude_POC = 0.0_r8
          exude_DON = 0.0_r8
          exude_PON = 0.0_r8
          exude_DOFe = 0.0_r8
          exude_POFe = 0.0_r8
          exude_DOP = 0.0_r8
          exude_POP = 0.0_r8
          exude_POSi = 0.0_r8
          exude_PIC = 0.0_r8
          respir = 0.0_r8
#if defined DARWIN_FUTURE
          respirN = 0.0_r8
          respirP = 0.0_r8
          respirFe = 0.0_r8
          respirSi = 0.0_r8
#endif

          DO ip = 1, nplank
#if defined DARWIN_MINVAL_X
            Xe = MAX(DARWIN_MINVAL_X, Xgraz(ip) - Xmin(ip,ng))
#else
            Xe = MAX(0.0_r8, Xgraz(ip) - Xmin(ip,ng))
#endif
#if defined DARWIN_FUTURE
            mortX = mort(ip,ng)*Xe*mortTempFunc**tempMort(ip,ng)
            mortX2= mort2(ip,ng)*Xe*Xe*mort2TempFunc**tempMort2(ip,ng)
#else
            mortX = mort(ip,ng)*Xe*MAX(mortTempFuncMin(ip,ng), mortTempFunc)
            mortX2= mort2(ip,ng)*Xe*Xe*MAX(mort2TempFuncMin(ip,ng), mort2TempFunc)
#endif

            mort_c(ip) = mortX + mortX2

            exude_DOC = exude_DOC + (1.0_r8-ExportFracMort(ip,ng)) *mortX+ (1.0_r8-ExportFracMort2(ip,ng))*mortX2
            exude_POC = exude_POC + ExportFracMort(ip,ng) *mortX+ ExportFracMort2(ip,ng) *mortX2

            exude_DON = exude_DON + (1.0_r8-ExportFracMort(ip,ng)) *mortX *Qngraz(ip)+ (1.0_r8-ExportFracMort2(ip,ng))*mortX2*Qngraz(ip)
            exude_PON = exude_PON + ExportFracMort(ip,ng) *mortX *Qngraz(ip)+ ExportFracMort2(ip,ng) *mortX2*Qngraz(ip)

            exude_DOP = exude_DOP + (1.0_r8-ExportFracMort(ip,ng)) *mortX *Qpgraz(ip)+ (1.0_r8-ExportFracMort2(ip,ng))*mortX2*Qpgraz(ip)
            exude_POP = exude_POP + ExportFracMort(ip,ng) *mortX *Qpgraz(ip)+ ExportFracMort2(ip,ng) *mortX2*Qpgraz(ip)

            exude_DOFe= exude_DOFe+ (1.0_r8-ExportFracMort(ip,ng)) *mortX *Qfegraz(ip)+ (1.0_r8-ExportFracMort2(ip,ng))*mortX2*Qfegraz(ip)
            exude_POFe= exude_POFe+ ExportFracMort(ip,ng) *mortX *Qfegraz(ip)+ ExportFracMort2(ip,ng) *mortX2*Qfegraz(ip)

            exude_POSi = exude_POSi + mort_c(ip)*Qsigraz(ip)

            exude_PIC = exude_PIC + mort_c(ip)*R_PICPOC(ip,ng)

#if defined DARWIN_VERBOSE_PLANK_OLD
            IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,'(a,i2,1x,a16,2x,10(f,1x,a,1x))') 'PLANK', ic_+ip-1, 'respir_c=', respRate(ip,ng),'*',Xe,'*',reminTempFunc
            END IF
#endif
#if defined DARWIN_FUTURE
            respir_c = 0.0_r8
            IF (isPhoto(ip,ng).EQ.0) THEN
              respir_c = respRate(ip,ng)*Xe*reminTempFunc
              respir = respir + respir_c
! release respired N/P/Fe/Si to inorganic pools, skipping any element
! using a variable quota (matching upstream's #ifndef DARWIN_ALLOW_*QUOTA)
#if ! defined DARWIN_NQUOTA
              respirN = respirN + respir_c*R_NC(ip,ng)
#endif
#if ! defined DARWIN_PQUOTA
              respirP = respirP + respir_c*R_PC(ip,ng)
#endif
#if ! defined DARWIN_FEQUOTA
              respirFe = respirFe + respir_c*R_FeC(ip,ng)
#endif
#if ! defined DARWIN_SIQUOTA
              respirSi = respirSi + respir_c*R_SiC(ip,ng)
#endif
            ENDIF
#else
            respir_c = respRate(ip,ng)*Xe*reminTempFunc
            respir = respir + respir_c
#endif

#if defined DARWIN_VERBOSE_PLANK
            IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J.and.ip.le.nPhoto) THEN
              write(*,'(a,i2.2,a,f10.6,a)') 'mort plankton',ip,         &
     &          ' (initial value: ',Xgraz(ip),')'
              write(*,'(a20,f14.10,8(a,f14.10))')                       &
     &          'mortX = ',mortX,                                       &
     &          ' (',mort(ip,ng),'*',Xe,'*',                            &
     &          MAX(mortTempFuncMin(ip,ng),mortTempFunc),')'
              write(*,'(a20,f14.10,8(a,f14.10))')                       &
     &          'mortX2 = ',mortX2,                                     &
     &          ' (',mort2(ip,ng),'*',Xe,'*',Xe,'*',                    &
     &          MAX(mort2TempFuncMin(ip,ng), mort2TempFunc),')'
              write(*,'(a20,f14.10)')                                   &
     &          'respir_c = ',respir_c
              write(*,'(a20,f14.10)') 'change : ',                      &
     &           dtbio(ng)*(- mort_c(ip) - respir_c)
              write(*,'(a20,3(f10.6,a))')                               &
     &          '(contrib : ',mortX*dtbio(ng),', ',                        &
     &          mortX2*dtbio(ng),', ',                                     &
     &          respir_c*dtbio(ng),')'
              write(*,'(a20,f10.6)')                                    &
     &          'mort rate (d-1) :',                                    &
     &          ((mortX+mortX2+respir_c)/Xgraz(ip))*86400.0_r8
              write(*,'(a20,3(f10.6,a))')                               &
     &          '(contrib : ',mortX/Xgraz(ip)*86400.0_r8,', ',              &
     &          mortX2/Xgraz(ip)*86400.0_r8,', ',                           &
     &          respir_c/Xgraz(ip)*86400.0_r8,')'
            END IF
#endif
#if defined DARWIN_VERBOSE_PLANK_OLD
            IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,'(a,i2,1x,a16,2x,f)') 'PLANK', ic_+ip-1,           '-mort_c', dtbio(ng)*(- mort_c(ip))
              write(*,'(a,i2,1x,a16,2x,f)') 'PLANK', ic_+ip-1,           '-respir_c', dtbio(ng)*(- respir_c)
            END IF
#endif
            Bio(ic_+ip-1)=Bio(ic_+ip-1)  + dtbio(ng)*(- mort_c(ip) - respir_c)
#if defined DARWIN_NQUOTA
            Bio(in_+ip-1)=Bio(in_+ip-1)  + dtbio(ng)*(- mort_c(ip)*Qngraz(ip))
#endif
#if defined DARWIN_PQUOTA
            Bio(ip_+ip-1)=Bio(ip_+ip-1)  + dtbio(ng)*(- mort_c(ip)*Qpgraz(ip))
#endif
#if defined DARWIN_SIQUOTA
            Bio(isi+ip-1)=Bio(isi+ip-1)  + dtbio(ng)*(- mort_c(ip)*Qsigraz(ip))
#endif
#if defined DARWIN_FEQUOTA
            Bio(ife+ip-1)=Bio(ife+ip-1)  + dtbio(ng)*(- mort_c(ip)*Qfegraz(ip))
#endif

#if defined DARWIN_EXUDE
            exude_DOC = exude_DOC + (1.0_r8-ExportFracExude(ip,ng))*kexcC(ip,ng)*Xe
            exude_POC = exude_POC + ExportFracExude(ip,ng) *kexcC(ip,ng)*Xe
            exude_DON = exude_DON + (1.0_r8-ExportFracExude(ip,ng))*kexcN(ip,ng)*Xe*Qngraz(ip)
            exude_PON = exude_PON + ExportFracExude(ip,ng) *kexcN(ip,ng)*Xe*Qngraz(ip)
            exude_DOP = exude_DOP + (1.0_r8-ExportFracExude(ip,ng))*kexcP(ip,ng)*Xe*Qpgraz(ip)
            exude_POP = exude_POP + ExportFracExude(ip,ng) *kexcP(ip,ng)*Xe*Qpgraz(ip)
            exude_DOFe=exude_DOFe+(1.0_r8-ExportFracExude(ip,ng))*kexcFe(ip,ng)*Xe*Qfegraz(ip)
            exude_POFe=exude_POFe+ ExportFracExude(ip,ng) *kexcFe(ip,ng)*Xe*Qfegraz(ip)
            exude_POSi = exude_POSi + kexcSi(ip,ng)*Xe*Qsigraz(ip)
            Bio(ic_+ip-1)=Bio(ic_+ip-1)  + dtbio(ng)*(- kexcC(ip,ng)*Xe)
#if defined DARWIN_NQUOTA
            Bio(in_+ip-1)=Bio(in_+ip-1)  + dtbio(ng)*(- kexcN(ip,ng)*Xe*Qngraz(ip))
#endif
#if defined DARWIN_PQUOTA
            Bio(ip_+ip-1)=Bio(ip_+ip-1)  + dtbio(ng)*(- kexcP(ip,ng)*Xe*Qpgraz(ip))
#endif
#if defined DARWIN_SIQUOTA
            Bio(isi+ip-1)=Bio(isi+ip-1)  + dtbio(ng)*(- kexcSi(ip,ng)*Xe*Qsigraz(ip))
#endif
#if defined DARWIN_FEQUOTA
            Bio(ife+ip-1)=Bio(ife+ip-1)  + dtbio(ng)*(- kexcFe(ip,ng)*Xe*Qfegraz(ip))
#endif
#endif
          ENDDO

#if defined DARWIN_CHLQUOTA
          DO ip = 1, nChl
            Bio(iChl+ip-1)=Bio(iChl+ip-1)  + dtbio(ng)*(- mort_c(ip)*QChlgraz(ip))
          ENDDO
#endif

          Bio(iDIC )=Bio(iDIC )  + dtbio(ng)*(+ respir)
#if defined DARWIN_FUTURE
#if ! defined DARWIN_NQUOTA
          Bio(iNO3 )=Bio(iNO3 )  + dtbio(ng)*(+ respirN)
#endif
#if ! defined DARWIN_PQUOTA
          Bio(iPO4 )=Bio(iPO4 )  + dtbio(ng)*(+ respirP)
#endif
#if ! defined DARWIN_FEQUOTA
          Bio(iFeT )=Bio(iFeT )  + dtbio(ng)*(+ respirFe)
#endif
#if ! defined DARWIN_SIQUOTA
          Bio(iSiO2)=Bio(iSiO2)  + dtbio(ng)*(+ respirSi)
#endif
#endif

          Bio(iDOC )=Bio(iDOC )  + dtbio(ng)*(+ exude_DOC)
          Bio(iDON )=Bio(iDON )  + dtbio(ng)*(+ exude_DON)
          Bio(iDOP )=Bio(iDOP )  + dtbio(ng)*(+ exude_DOP)
          Bio(iDOFe)=Bio(iDOFe)  + dtbio(ng)*(+ exude_DOFe)

          Bio(iPIC )=Bio(iPIC )  + dtbio(ng)*(+ exude_PIC)
          Bio(iPOC )=Bio(iPOC )  + dtbio(ng)*(+ exude_POC)
          Bio(iPON )=Bio(iPON )  + dtbio(ng)*(+ exude_PON)
          Bio(iPOP )=Bio(iPOP )  + dtbio(ng)*(+ exude_POP)
          Bio(iPOSi)=Bio(iPOSi)  + dtbio(ng)*(+ exude_POSi)
          Bio(iPOFe)=Bio(iPOFe)  + dtbio(ng)*(+ exude_POFe)
#if defined DARWIN_CDOM
          exude_CDOM = fracCDOM(ng)*exude_DOP
          Bio(iCDOM)=Bio(iCDOM)  + dtbio(ng)*(+ exude_CDOM)
          Bio(iDOC )=Bio(iDOC )  + dtbio(ng)*(- R_CP_CDOM(ng)*exude_CDOM)
          Bio(iDON )=Bio(iDON )  + dtbio(ng)*(- R_NP_CDOM(ng)*exude_CDOM)
          Bio(iDOP )=Bio(iDOP )  + dtbio(ng)*(- exude_CDOM)
          Bio(iDOFe)=Bio(iDOFe)  + dtbio(ng)*(- R_FeP_CDOM(ng)*exude_CDOM)
#endif
