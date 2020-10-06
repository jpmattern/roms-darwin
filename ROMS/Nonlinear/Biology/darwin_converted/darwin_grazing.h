!
!  converted from "gud_grazing.F" using darwinfortran_to_romsfortran
!  (written by J. Paul Mattern)
!
!==== make all bio fields non-negative and compute quotas ==============

! fixed carbon quota, for now 1.0_r8 (may change later)
          Qc(:) = 1.0_r8
#if defined DARWIN_MINVAL_X
          X(:) = MAX(DARWIN_MINVAL_X, Bio(i,k,ic_:ec_))/Qc(:)
#else
          X(:) = MAX(0.0_r8, Bio(i,k,ic_:ec_))/Qc(:)
#endif
! other elements: get quota from corresponding ptracer or set to fixed
! ratio if not variable.
          Xi(:) = 1.0_r8/MAX(BioMin(ng), X(:))
#if defined DARWIN_NQUOTA
          Qn(:) = MAX(0.0_r8, Bio(i,k,in_:en_))*Xi(:)
#else
          Qn(:) = R_NC(:,ng)
#endif
#if defined DARWIN_PQUOTA
          Qp(:) = MAX(0.0_r8, Bio(i,k,ip_:ep_))*Xi(:)
#else
          Qp(:) = R_PC(:,ng)
#endif
#if defined DARWIN_SIQUOTA
          Qsi(:) = MAX(0.0_r8, Bio(i,k,isi:esi))*Xi(:)
#else
          Qsi(:) = R_SiC(:,ng)
#endif
#if defined DARWIN_FEQUOTA
          Qfe(:) = MAX(0.0_r8, Bio(i,k,ife:efe))*Xi(:)
#else
          Qfe(:) = R_FeC(:,ng)
#endif
#if defined DARWIN_CHLQUOTA
          QChl(:) = MAX(0.0_r8, Bio(i,k,ichl:echl))*Xi(1:nChl)
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

!=======================================================================
          DO iz = iMinPred, iMaxPred

! regulate grazing near full quota
            regQc = 1.0_r8
#if defined DARWIN_NQUOTA
            regQn = MAX(0.0_r8, MIN(1.0_r8, (Qnmax(iz,ng)-Qn(iz))/(Qnmax(iz,ng)-Qnmin(iz,ng)) ))
            regQc = MIN(regQc, 1.0_r8 - regQn)
            regQn = regQn**hillnum(ng)
#endif
#if defined DARWIN_PQUOTA
            regQp = MAX(0.0_r8, MIN(1.0_r8, (Qpmax(iz,ng)-Qp(iz))/(Qpmax(iz,ng)-Qpmin(iz,ng)) ))
            regQc = MIN(regQc, 1.0_r8 - regQp)
            regQp = regQp**hillnum(ng)
#endif
#if defined DARWIN_FEQUOTA
            regQfe= MAX(0.0_r8, MIN(1.0_r8, (Qfemax(iz,ng)-Qfe(iz))/(Qfemax(iz,ng)-Qfemin(iz,ng)) ))
            regQc = MIN(regQc, 1.0_r8 - regQfe)
            regQfe=regQfe**hillnum(ng)
#endif
            regQc = regQc**hillnum(ng)

            sumprey = 0.0_r8
            sumpref = 0.0_r8
            DO ip = iMinPrey, iMaxPrey
             sumprey = sumprey + palat(ip,iz,ng)*X(ip)
#if defined DARWIN_GRAZING_SWITCH
             sumpref = sumpref + palat(ip,iz,ng)*palat(ip,iz,ng)*X(ip)*X(ip)
#else
#if defined DARWIN_VERBOSE_PLANK_OLD
            IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,'(a,i2,1x,a1,i0,a14,2x,10(f,1x,a,1x))') 'PLANK', ic_+iz-1, '(',ip,') sumpref=', sumpref, '+', palat(ip,iz,ng), '*', X(ip)
            END IF
#endif
             sumpref = sumpref + palat(ip,iz,ng)*X(ip)
#endif
            ENDDO
            sumprey = MAX(0.0_r8, sumprey - phygrazmin(ng))
            sumpref = MAX(phygrazmin(ng), sumpref)
#if defined DARWIN_VERBOSE_PLANK_OLD
              IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
                write(*,'(a,i2,1x,a16,2x,10(f,1x,a,1x))') 'PLANK', ic_+iz-1, 'sumprey=', sumprey
!                write(*,'(a,i2,1x,a16,2x,10(f,1x,a,1x))') 'PLANK',      &
!     &            ic_+iz-1, 'tmp=', grazemax(iz,ng), '*',               &
!     &            grazTempFunc(i,k,iz), '*',                            &
!     &            X(iz), '*', (sumprey**hollexp(ng)/(sumprey**hollexp(ng)+kgrazesat(iz,ng)**hollexp(ng))), '*', (1.0_r8 - EXP(-inhib_graz(ng)*sumprey)), '**', inhib_graz_exp(ng)
              END IF
#endif
            tmp = grazemax(iz,ng)*grazTempFunc(i,k,iz)*X(iz)*(sumprey**hollexp(ng)/(sumprey**hollexp(ng)+kgrazesat(iz,ng)**hollexp(ng)))*(1.0_r8 - EXP(-inhib_graz(ng)*sumprey))**inhib_graz_exp(ng)

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
            DO ip = iMinPrey, iMaxPrey
!#if defined DARWIN_VERBOSE_PLANK
!              IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
!                write(*,'(a,i2.2,a,f10.6,a)') 'grazing on plankton',ip, &
!     &            ' (initial value: ',X(ip),')'
!              END IF
!#endif
#if defined DARWIN_GRAZING_SWITCH
              grazphy = tmp*palat(ip,iz,ng)*palat(ip,iz,ng)*X(ip)*X(ip)/sumpref
#else
#if defined DARWIN_VERBOSE_PLANK_OLD
              IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
                write(*,'(a,i2,1x,a16,2x,10(f,1x,a,1x))') 'PLANK', ic_+ip-1, 'grazphy=', tmp, '*', palat(ip,iz,ng), '*', X(ip), '/', sumpref
              END IF
#endif
              grazphy = tmp*palat(ip,iz,ng)*X(ip)/sumpref
!#if defined DARWIN_VERBOSE_PLANK
!              IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
!               write(*,'(a20,f10.6," (",f10.6,3(a,f10.6),")",a,i2.2,a)')&
!     &            'grazphy = ',grazphy,                                 &
!     &            tmp,'*',palat(ip,iz,ng),'*',X(ip),                    &
!     &            '/',sumpref,' (due to plankton',iz,')' 
!                preygraz_rate(ip)=preygraz_rate(ip) + grazphy/X(ip)
!              END IF
!#endif
#endif

              preygraz(ip) = preygraz(ip) + grazphy
#if defined DARWIN_VERBOSE_PLANK && ! defined DARWIN_GRAZING_SWITCH
              IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
                preygraz_rate(ip)=preygraz_rate(ip) + grazphy/X(ip)
                grazphy_all(ip,iz)=grazphy
                IF (ip.le.nPhoto.and.iz==iMaxPred) THEN
                  write(*,'(a,i2.2,a,f10.6,a,i0,a)')                    &
     &              'graz on plankton',ip,                              &
     &              ' (initial value: ',X(ip),', iMaxPrey=',iMaxPrey,')'
                  write(*,'(a20,f14.10)')                               &
     &              'preygraz = ',preygraz(ip)
                  write(*,'(a20,f14.10)') 'change : ',                  &
     &              dt(ng)*(- preygraz(ip))
                  write(*,'(a20,f14.10,99(", ",f14.10))') 'contrib : ', &
     &              dt(ng)*grazphy_all(ip,:)
                  write(*,'(a20,f10.6)')                                &
     &              'graz rate (d-1): ',preygraz_rate(ip)*86400.0_r8
                END IF
              END IF
#endif

              totkillc = totkillc + grazphy
              totkilln = totkilln + grazphy*Qn (ip)
              totkillp = totkillp + grazphy*Qp (ip)
              totkillsi = totkillsi + grazphy*Qsi(ip)
              totkillfe = totkillfe + grazphy*Qfe(ip)
#if defined DARWIN_CARBON
              totkillPIC= totkillPIC+ grazphy*R_PICPOC(ip,ng)
#endif

              expFrac = ExportFracPreyPred(ip,iz,ng)
              totkillexpc = totkillexpc + expFrac*grazphy
              totkillexpn = totkillexpn + expFrac*grazphy*Qn (ip)
              totkillexpp = totkillexpp + expFrac*grazphy*Qp (ip)
              totkillexpfe = totkillexpfe + expFrac*grazphy*Qfe(ip)

#if defined DARWIN_VERBOSE_PLANK_OLD
            IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,'(a,i2,1x,a16,2x,10(f,1x,a,1x))') 'PLANK', ic_+iz-1, 'predgrazc=', grazphy, '*',asseff(ip,iz,ng),'*',regQc 
            END IF
#endif
              predgrazc(iz) = predgrazc(iz) + grazphy*asseff(ip,iz,ng)*regQc
              predexpc = predexpc + expFrac*grazphy*asseff(ip,iz,ng)*regQc
#if defined DARWIN_NQUOTA
              predgrazn(iz) = predgrazn(iz) + grazphy*asseff(ip,iz,ng)*regQn*Qn(ip)
              predexpn = predexpn + expFrac*grazphy*asseff(ip,iz,ng)*regQn*Qn(ip)
#endif
#if defined DARWIN_PQUOTA
              predgrazp(iz) = predgrazp(iz) + grazphy*asseff(ip,iz,ng)*regQp*Qp(ip)
              predexpp = predexpp + expFrac*grazphy*asseff(ip,iz,ng)*regQp*Qp(ip)
#endif
#if defined DARWIN_FEQUOTA
              predgrazfe(iz) = predgrazfe(iz) + grazphy*asseff(ip,iz,ng)*regQfe*Qfe(ip)
              predexpfe = predexpfe + expFrac*grazphy*asseff(ip,iz,ng)*regQfe*Qfe(ip)
#endif
            ENDDO

            graz2OC = graz2OC + totkillc - predgrazc(iz)
            graz2POC = graz2POC + totkillexpc - predexpc

#if defined DARWIN_NQUOTA
            graz2ON = graz2ON + totkilln - predgrazn(iz)
            graz2PON = graz2PON + totkillexpn - predexpn
#else
            graz2ON = graz2ON + totkilln - predgrazc(iz)*Qn(iz)
            graz2PON = graz2PON + totkillexpn - predexpc *Qn(iz)
#endif

#if defined DARWIN_PQUOTA
            graz2OP = graz2OP + totkillp - predgrazp(iz)
            graz2POP = graz2POP + totkillexpp - predexpp
#else
            graz2OP = graz2OP + totkillp - predgrazc(iz)*Qp(iz)
            graz2POP = graz2POP + totkillexpp - predexpc *Qp(iz)
#endif

#if defined DARWIN_FEQUOTA
            graz2OFe = graz2OFe + totkillfe - predgrazfe(iz)
            graz2POFe = graz2POFe + totkillexpfe - predexpfe
#else
            graz2OFe = graz2OFe + totkillfe - predgrazc(iz)*Qfe(iz)
            graz2POFe = graz2POFe + totkillexpfe - predexpc *Qfe(iz)
#endif

            graz2POSi = graz2POSi + totkillsi

#if defined DARWIN_CARBON
            graz2PIC = graz2PIC + totkillPIC
#endif

! end predator loop
          ENDDO

!==== tendencies =======================================================

          Bio(i,k,iDOC )=Bio(i,k,iDOC )  + dt(ng)*(+ graz2OC - graz2POC)
          Bio(i,k,iDON )=Bio(i,k,iDON )  + dt(ng)*(+ graz2ON - graz2PON)
          Bio(i,k,iDOP )=Bio(i,k,iDOP )  + dt(ng)*(+ graz2OP - graz2POP)
          Bio(i,k,iDOFe)=Bio(i,k,iDOFe)  + dt(ng)*(+ graz2OFe - graz2POFe)
          Bio(i,k,iPOC )=Bio(i,k,iPOC )  + dt(ng)*(+ graz2POC)
          Bio(i,k,iPON )=Bio(i,k,iPON )  + dt(ng)*(+ graz2PON)
          Bio(i,k,iPOP )=Bio(i,k,iPOP )  + dt(ng)*(+ graz2POP)
          Bio(i,k,iPOSi)=Bio(i,k,iPOSi)  + dt(ng)*(+ graz2POSi)
          Bio(i,k,iPOFe)=Bio(i,k,iPOFe)  + dt(ng)*(+ graz2POFe)
#if defined DARWIN_CARBON
          Bio(i,k,iPIC )=Bio(i,k,iPIC )  + dt(ng)*(+ graz2PIC)
#endif
#if defined DARWIN_CDOM
          graz2CDOM = fracCDOM(ng)*(graz2OP - graz2POP)
          Bio(i,k,iCDOM)=Bio(i,k,iCDOM)  + dt(ng)*(+ graz2CDOM)
          Bio(i,k,iDOC )=Bio(i,k,iDOC )  + dt(ng)*(- R_CP_CDOM(ng)*graz2CDOM)
          Bio(i,k,iDON )=Bio(i,k,iDON )  + dt(ng)*(- R_NP_CDOM(ng)*graz2CDOM)
          Bio(i,k,iDOP )=Bio(i,k,iDOP )  + dt(ng)*(- graz2CDOM)
          Bio(i,k,iDOFe)=Bio(i,k,iDOFe)  + dt(ng)*(- R_FeP_CDOM(ng)*graz2CDOM)
#endif
          DO ip = iMinPrey, iMaxPrey
#if defined DARWIN_VERBOSE_PLANK_OLD
            IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,'(a,i2,1x,a16,2x,f)') 'PLANK', ic_+ip-1,           '-preygraz', dt(ng)*(- preygraz(ip))
            END IF
#endif
           Bio(i,k,ic_+ip-1)= Bio(i,k,ic_+ip-1)  + dt(ng)*(- preygraz(ip))
          ENDDO
          DO iz = iMinPred, iMaxPred
#if defined DARWIN_VERBOSE_PLANK_OLD
            IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,'(a,i2,1x,a16,2x,f)') 'PLANK', ic_+iz-1,           '+predgrazc', dt(ng)*(+ predgrazc(iz))
            END IF
#endif
           Bio(i,k,ic_+iz-1)=Bio(i,k,ic_+iz-1)  + dt(ng)*(+ predgrazc(iz))
          ENDDO
#if defined DARWIN_NQUOTA
          Bio(i,k,in_:en_)=Bio(i,k,in_:en_)  + dt(ng)*(+ predgrazn(:) - preygraz(:)*Qn(:))
#endif
#if defined DARWIN_PQUOTA
          Bio(i,k,ip_:ep_)=Bio(i,k,ip_:ep_)  + dt(ng)*(+ predgrazp(:) - preygraz(:)*Qp(:))
#endif
#if defined DARWIN_SIQUOTA
          Bio(i,k,isi:esi)=Bio(i,k,isi:esi)  + dt(ng)*(- preygraz(:)*Qsi(:))
#endif
#if defined DARWIN_FEQUOTA
          Bio(i,k,ife:efe)=Bio(i,k,ife:efe)  + dt(ng)*(+ predgrazfe(:) - preygraz(:)*Qfe(:))
#endif
#if defined DARWIN_CHLQUOTA
          Bio(i,k,iChl:eChl)=Bio(i,k,iChl:eChl)  + dt(ng)*(- preygraz(1:nChl)*QChl(:))
#endif

          DO ip = 1, nGRplank
            diags(i,k,iGRplank+ip-1) = preygraz(ip)
          ENDDO

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

          DO ip = 1, nplank
#if defined DARWIN_MINVAL_X
            Xe = MAX(DARWIN_MINVAL_X, X(ip) - Xmin(ip,ng))
#else
            Xe = MAX(0.0_r8, X(ip) - Xmin(ip,ng))
#endif
            mortX = mort(ip,ng)*Xe*MAX(mortTempFuncMin(ip,ng), mortTempFunc(i,k))
            mortX2= mort2(ip,ng)*Xe*Xe*MAX(mort2TempFuncMin(ip,ng), mort2TempFunc(i,k))

            mort_c(ip) = mortX + mortX2

            exude_DOC = exude_DOC + (1.0_r8-ExportFracMort(ip,ng)) *mortX+ (1.0_r8-ExportFracMort2(ip,ng))*mortX2
            exude_POC = exude_POC + ExportFracMort(ip,ng) *mortX+ ExportFracMort2(ip,ng) *mortX2

            exude_DON = exude_DON + (1.0_r8-ExportFracMort(ip,ng)) *mortX *Qn(ip)+ (1.0_r8-ExportFracMort2(ip,ng))*mortX2*Qn(ip)
            exude_PON = exude_PON + ExportFracMort(ip,ng) *mortX *Qn(ip)+ ExportFracMort2(ip,ng) *mortX2*Qn(ip)

            exude_DOP = exude_DOP + (1.0_r8-ExportFracMort(ip,ng)) *mortX *Qp(ip)+ (1.0_r8-ExportFracMort2(ip,ng))*mortX2*Qp(ip)
            exude_POP = exude_POP + ExportFracMort(ip,ng) *mortX *Qp(ip)+ ExportFracMort2(ip,ng) *mortX2*Qp(ip)

            exude_DOFe= exude_DOFe+ (1.0_r8-ExportFracMort(ip,ng)) *mortX *Qfe(ip)+ (1.0_r8-ExportFracMort2(ip,ng))*mortX2*Qfe(ip)
            exude_POFe= exude_POFe+ ExportFracMort(ip,ng) *mortX *Qfe(ip)+ ExportFracMort2(ip,ng) *mortX2*Qfe(ip)

            exude_POSi = exude_POSi + mort_c(ip)*Qsi(ip)

            exude_PIC = exude_PIC + mort_c(ip)*R_PICPOC(ip,ng)

#if defined DARWIN_VERBOSE_PLANK_OLD
            IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,'(a,i2,1x,a16,2x,10(f,1x,a,1x))') 'PLANK', ic_+ip-1, 'respir_c=', respiration(ip,ng),'*',Xe,'*',reminTempFunc(i,k)
            END IF
#endif
            respir_c = respiration(ip,ng)*Xe*reminTempFunc(i,k)
            respir = respir + respir_c

#if defined DARWIN_VERBOSE_PLANK
            IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J.and.ip.le.nPhoto) THEN
              write(*,'(a,i2.2,a,f10.6,a)') 'mort plankton',ip,         &
     &          ' (initial value: ',X(ip),')'
              write(*,'(a20,f14.10,8(a,f14.10))')                       &
     &          'mortX = ',mortX,                                       &
     &          ' (',mort(ip,ng),'*',Xe,'*',                            &
     &          MAX(mortTempFuncMin(ip,ng),mortTempFunc(i,k)),')'
              write(*,'(a20,f14.10,8(a,f14.10))')                       &
     &          'mortX2 = ',mortX2,                                     &
     &          ' (',mort2(ip,ng),'*',Xe,'*',Xe,'*',                    &
     &          MAX(mort2TempFuncMin(ip,ng), mort2TempFunc(i,k)),')'
              write(*,'(a20,f14.10)')                                   &
     &          'respir_c = ',respir_c                                
              write(*,'(a20,f14.10)') 'change : ',                      &
     &           dt(ng)*(- mort_c(ip) - respir_c)
              write(*,'(a20,3(f10.6,a))')                               &
     &          '(contrib : ',mortX*dt(ng),', ',                        &
     &          mortX2*dt(ng),', ',                                     &
     &          respir_c*dt(ng),')'
              write(*,'(a20,f10.6)')                                    &
     &          'mort rate (d-1) :',                                    &
     &          ((mortX+mortX2+respir_c)/X(ip))*86400.0_r8
              write(*,'(a20,3(f10.6,a))')                               &
     &          '(contrib : ',mortX/X(ip)*86400.0_r8,', ',              &
     &          mortX2/X(ip)*86400.0_r8,', ',                           &
     &          respir_c/X(ip)*86400.0_r8,')'
            END IF
#endif
#if defined DARWIN_VERBOSE_PLANK_OLD
            IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,'(a,i2,1x,a16,2x,f)') 'PLANK', ic_+ip-1,           '-mort_c', dt(ng)*(- mort_c(ip))
              write(*,'(a,i2,1x,a16,2x,f)') 'PLANK', ic_+ip-1,           '-respir_c', dt(ng)*(- respir_c)
            END IF
#endif
            Bio(i,k,ic_+ip-1)=Bio(i,k,ic_+ip-1)  + dt(ng)*(- mort_c(ip) - respir_c)
#if defined DARWIN_NQUOTA
            Bio(i,k,in_+ip-1)=Bio(i,k,in_+ip-1)  + dt(ng)*(- mort_c(ip)*Qn(ip))
#endif
#if defined DARWIN_PQUOTA
            Bio(i,k,ip_+ip-1)=Bio(i,k,ip_+ip-1)  + dt(ng)*(- mort_c(ip)*Qp(ip))
#endif
#if defined DARWIN_SIQUOTA
            Bio(i,k,isi+ip-1)=Bio(i,k,isi+ip-1)  + dt(ng)*(- mort_c(ip)*Qsi(ip))
#endif
#if defined DARWIN_FEQUOTA
            Bio(i,k,ife+ip-1)=Bio(i,k,ife+ip-1)  + dt(ng)*(- mort_c(ip)*Qfe(ip))
#endif

#if defined DARWIN_EXUDE
            exude_DOC = exude_DOC + (1.0_r8-ExportFrac(ip,ng))*kexcC(ip,ng)*Xe
            exude_POC = exude_POC + ExportFrac(ip,ng) *kexcC(ip,ng)*Xe
            exude_DON = exude_DON + (1.0_r8-ExportFrac(ip,ng))*kexcN(ip,ng)*Xe*Qn(ip)
            exude_PON = exude_PON + ExportFrac(ip,ng) *kexcN(ip,ng)*Xe*Qn(ip)
            exude_DOP = exude_DOP + (1.0_r8-ExportFrac(ip,ng))*kexcP(ip,ng)*Xe*Qp(ip)
            exude_POP = exude_POP + ExportFrac(ip,ng) *kexcP(ip,ng)*Xe*Qp(ip)
            exude_DOFe=exude_DOFe+(1.0_r8-ExportFrac(ip,ng))*kexcFe(ip,ng)*Xe*Qfe(ip)
            exude_POFe=exude_POFe+ ExportFrac(ip,ng) *kexcFe(ip,ng)*Xe*Qfe(ip)
            exude_POSi = exude_POSi + kexcSi(ip,ng)*Xe*Qsi(ip)
            Bio(i,k,ic_+ip-1)=Bio(i,k,ic_+ip-1)  + dt(ng)*(- kexcC(ip,ng)*Xe)
#if defined DARWIN_NQUOTA
            Bio(i,k,in_+ip-1)=Bio(i,k,in_+ip-1)  + dt(ng)*(- kexcN(ip,ng)*Xe*Qn(ip))
#endif
#if defined DARWIN_PQUOTA
            Bio(i,k,ip_+ip-1)=Bio(i,k,ip_+ip-1)  + dt(ng)*(- kexcP(ip,ng)*Xe*Qp(ip))
#endif
#if defined DARWIN_SIQUOTA
            Bio(i,k,isi+ip-1)=Bio(i,k,isi+ip-1)  + dt(ng)*(- kexcSi(ip,ng)*Xe*Qsi(ip))
#endif
#if defined DARWIN_FEQUOTA
            Bio(i,k,ife+ip-1)=Bio(i,k,ife+ip-1)  + dt(ng)*(- kexcFe(ip,ng)*Xe*Qfe(ip))
#endif
#endif
          ENDDO

#if defined DARWIN_CHLQUOTA
          DO ip = 1, nChl
            Bio(i,k,iChl+ip-1)=Bio(i,k,iChl+ip-1)  + dt(ng)*(- mort_c(ip)*QChl(ip))
          ENDDO
#endif

          Bio(i,k,iDIC )=Bio(i,k,iDIC )  + dt(ng)*(+ respir)

          Bio(i,k,iDOC )=Bio(i,k,iDOC )  + dt(ng)*(+ exude_DOC)
          Bio(i,k,iDON )=Bio(i,k,iDON )  + dt(ng)*(+ exude_DON)
          Bio(i,k,iDOP )=Bio(i,k,iDOP )  + dt(ng)*(+ exude_DOP)
          Bio(i,k,iDOFe)=Bio(i,k,iDOFe)  + dt(ng)*(+ exude_DOFe)

          Bio(i,k,iPIC )=Bio(i,k,iPIC )  + dt(ng)*(+ exude_PIC)
          Bio(i,k,iPOC )=Bio(i,k,iPOC )  + dt(ng)*(+ exude_POC)
          Bio(i,k,iPON )=Bio(i,k,iPON )  + dt(ng)*(+ exude_PON)
          Bio(i,k,iPOP )=Bio(i,k,iPOP )  + dt(ng)*(+ exude_POP)
          Bio(i,k,iPOSi)=Bio(i,k,iPOSi)  + dt(ng)*(+ exude_POSi)
          Bio(i,k,iPOFe)=Bio(i,k,iPOFe)  + dt(ng)*(+ exude_POFe)
#if defined DARWIN_CDOM
          exude_CDOM = fracCDOM(ng)*exude_DOP
          Bio(i,k,iCDOM)=Bio(i,k,iCDOM)  + dt(ng)*(+ exude_CDOM)
          Bio(i,k,iDOC )=Bio(i,k,iDOC )  + dt(ng)*(- R_CP_CDOM(ng)*exude_CDOM)
          Bio(i,k,iDON )=Bio(i,k,iDON )  + dt(ng)*(- R_NP_CDOM(ng)*exude_CDOM)
          Bio(i,k,iDOP )=Bio(i,k,iDOP )  + dt(ng)*(- exude_CDOM)
          Bio(i,k,iDOFe)=Bio(i,k,iDOFe)  + dt(ng)*(- R_FeP_CDOM(ng)*exude_CDOM)
#endif


