!
!  converted from "gud_light_radtrans.F" using darwinfortran_to_romsfortran
!  (written by J. Paul Mattern)
!
        DO k=1,N(ng)

         dz_k(k) = drF(k)*HFacC(i,ic,k,bi,bj)
         part = MAX(Ptracer(i,ic,k,bi,bj,iPOP), 0.0_r8)
         DO ip=1,nPhoto
#if defined DARWIN_CHLQUOTA
          phychl(ip)=MAX(Ptracer(i,ic,k,bi,bj,iChl+ip-1),0.0_r8)
#else
          phychl(ip)=MAX(chlPrev(i,ic,k,bi,bj,ip), 0.0_r8)
#endif
         ENDDO
         DO ip=1,nPlank
          plankcar(ip)=MAX(Ptracer(i,ic,k,bi,bj,ic_+ip-1),0.0_r8)
         ENDDO

#if defined DARWIN_CDOM
! use cdom-like tracer
         CDOM = MAX(Ptracer(i,ic,k,bi,bj,iCDOM), 0.0_r8)
         DO l = 1,nlam
          aCDOM(k,l) = CDOMcoeff(ng)*CDOM*exCDOM(l)
         ENDDO
#else
         actotref = 0.0_r8
         atotref = 0.0_r8
         DO ip = 1,nPhoto
! nb. n,k swapped from WG
          actotref = actotref +phychl(ip)*aphy_chl(ip,laCDOM,ng)
         ENDDO
         atotref = aw(laCDOM) + actotref
         DO l = 1,nlam
          aCDOM(k,l) = gud_aCDOM_fac(ng)*atotref*exCDOM(l)
         ENDDO
#endif

         DO l = 1,nlam
! absorption by phyto
          actot(k,l) = 0.0_r8
          bctot(k,l) = 0.0_r8
          bbctot(k,l) = 0.0_r8
          DO ip = 1, nPhoto
           actot(k,l) = actot(k,l) + phychl(ip)*aphy_chl(ip,l,ng)
          ENDDO
          DO ip = 1, nPlank
! convert mmol C to mg C
           bctot(k,l) = bctot(k,l) + plankcar(ip)*bphy_mgC(ip,l,ng)*12
           bbctot(k,l) = bbctot(k,l) + plankcar(ip)*bbphy_mgC(ip,l,ng)*12
          ENDDO
! add water, CDOM and particles
          aprt_k(k,l) = part*apart_P(l)
          btprt_k(k,l) = part*bpart_P(l)
          bbprt_k(k,l) = part*bbpart_P(l)
          a_k(k,l) = aw(l) + aCDOM(k,l) + actot(k,l) + aprt_k(k,l)
          bt_k(k,l) = bw(l) + bctot(k,l) + btprt_k(k,l)
          bb_k(k,l) = gud_bbw(ng)*bw(l) + bbctot(k,l) + bbprt_k(k,l)
          bb_k(k,l) = MAX(gud_bbmin(ng), bb_k(k,l))
         ENDDO

! k
        ENDDO
! ----------------------------------------------------------------------

! use read-in_ light
        DO l = 1,nlam
          Edwsf(l) = OASIM_Ed(i,ic,bi,bj,l)
          Eswsf(l) = OASIM_Es(i,ic,bi,bj,l)
        ENDDO

        IF (icefile .NE. ' ' .and. myiter .ge. 0) THEN
         DO l = 1,nlam
          Edwsf(l) = Edwsf(l)*(1.0_r8 - iceFrac(i,ic,bi,bj))
          Eswsf(l) = Eswsf(l)*(1.0_r8 - iceFrac(i,ic,bi,bj))
         ENDDO
        ENDIF

        klow = MIN(gud_radtrans_kmax(ng), kLowC(i,ic,bi,bj))

        CALL DARWIN_RADTRANS_DIRECT(                                    &
     & dz_k,rmud(i,ic),Edwsf,Eswsf,a_k,bt_k,bb_k,klow,
     O Edbot,Esbot,Eubot,Estop,Eutop,
     O PAR_k, PARF_k,
     O amp1_k,amp2_k, x_k, y_k,
     O r1_k,r2_k,kappa1_k,kappa2_k,                                     &
     & myThid)

        DO l = 1,nlam
         DO k = 1,N(ng)
          PAR(i,k,i,ic,k,l) = PAR_k(k,l)
         ENDDO
        ENDDO

#if defined DARWIN_DIAGNOSTICS
        DO l = 1,nlam
         Ed(i,ic,1,l) = Edwsf(l)
         Es(i,ic,1,l) = Eswsf(l)
         Edown = Edwsf(l) + Eswsf(l)
         IF (Edown .GT. 0) THEN
           Rirr(i,ic,l) = Eutop(1,l)/Edown
         ELSE
           Rirr(i,ic,l) = 0.0_r8
         ENDIF
#if defined DARWIN_DIAG_RADTRANS_SOLUTION
         Eub(i,ic,1,l) = 0.0_r8
#endif
         DO k = 1,N(ng)-1
          Ed(i,ic,k+1,l) = Edbot(k,l)
          Es(i,ic,k+1,l) = Esbot(k,l)
#if defined DARWIN_DIAG_RADTRANS_SOLUTION
          Eub(i,ic,k+1,l) = Eubot(k,l)
#endif
         ENDDO
         DO k = 1,N(ng)
          PARF(i,ic,k,l) = PARF_k(k,l)
          Eu(i,ic,k,l) = Eutop(k,l)
#if defined DARWIN_DIAG_IOP
          a3d(i,ic,k,l) = a_k(k,l)
          bt3d(i,ic,k,l) = bt_k(k,l)
          bb3d(i,ic,k,l) = bb_k(k,l)
          aplk3d(i,ic,k,l) = actot(k,l)
          btplk3d(i,ic,k,l) = bctot(k,l)
          bbplk3d(i,ic,k,l) = bbctot(k,l)
          aprt3d(i,ic,k,l) = aprt_k(k,l)
          btprt3d(i,ic,k,l) = btprt_k(k,l)
          bbprt3d(i,ic,k,l) = bbprt_k(k,l)
          aCDOM3d(i,ic,k,l) = aCDOM(k,l)
#endif
#if defined DARWIN_DIAG_RADTRANS_SOLUTION
          Est(i,ic,k,l) = Estop(k,l)
          amp1(i,ic,k,l) = amp1_k(k,l)
          amp2(i,ic,k,l) = amp2_k(k,l)
          x3d(i,ic,k,l) = x_k(k,l)
          y3d(i,ic,k,l) = y_k(k,l)
          r1(i,ic,k,l) = r1_k(k,l)
          r2(i,ic,k,l) = r2_k(k,l)
          kap1(i,ic,k,l) = kappa1_k(k,l)
          kap2(i,ic,k,l) = kappa2_k(k,l)
#endif
         ENDDO
        ENDDO
#endif

! i,ic
       ENDDO
      ENDDO

! ======================================================================

#if defined DARWIN_DIAGNOSTICS
      IF (useDIAGNOSTICS .AND. myIter .GE.0) THEN
       CALL DIAGNOSTICS_FILL(rmud,'rmud ',1,1,3,bi,bj,myThid)
      DO l = 1, nlam
       WRITE(diagname, '(A,I3.3)') 'Rirr', l
       CALL DIAGNOSTICS_FILL(Rirr(1,1,l),diagname,0,1,3,bi,bj,myThid)
       WRITE(diagname, '(A,I3.3)') 'Ed', l
       CALL DIAGNOSTICS_FILL(Ed(1,1,1,l),diagname,0,N(ng),3,bi,bj,myThid)
       WRITE(diagname, '(A,I3.3)') 'Es', l
       CALL DIAGNOSTICS_FILL(Es(1,1,1,l),diagname,0,N(ng),3,bi,bj,myThid)
       WRITE(diagname, '(A,I3.3)') 'Eu', l
       CALL DIAGNOSTICS_FILL(Eu(1,1,1,l),diagname,0,N(ng),3,bi,bj,myThid)
       WRITE(diagname, '(A,I3.3)') 'PARF', l
       CALL DIAGNOSTICS_FILL(PARF(1,1,1,l),diagname,0,N(ng),3,bi,bj,myThid)
#if defined DARWIN_DIAG_RADTRANS_SOLUTION
       WRITE(diagname, '(A,I3.3)') 'Estop', l
       CALL DIAGNOSTICS_FILL(Est(1,1,1,l),diagname,0,N(ng),3,bi,bj,myThid)
       WRITE(diagname, '(A,I3.3)') 'Eubot', l
       CALL DIAGNOSTICS_FILL(Eub(1,1,1,l),diagname,0,N(ng),3,bi,bj,myThid)
       WRITE(diagname, '(A,I3.3)') 'amp1_', l
       CALL DIAGNOSTICS_FILL(amp1(1,1,1,l),diagname,0,N(ng),3,bi,bj,myThid)
       WRITE(diagname, '(A,I3.3)') 'amp2_', l
       CALL DIAGNOSTICS_FILL(amp2(1,1,1,l),diagname,0,N(ng),3,bi,bj,myThid)
       WRITE(diagname, '(A,I3.3)') 'x_', l
       CALL DIAGNOSTICS_FILL(x3d(1,1,1,l),diagname,0,N(ng),3,bi,bj,myThid)
       WRITE(diagname, '(A,I3.3)') 'y_', l
       CALL DIAGNOSTICS_FILL(y3d(1,1,1,l),diagname,0,N(ng),3,bi,bj,myThid)
       WRITE(diagname, '(A,I3.3)') 'r1_', l
       CALL DIAGNOSTICS_FILL(r1(1,1,1,l),diagname,0,N(ng),3,bi,bj,myThid)
       WRITE(diagname, '(A,I3.3)') 'r2_', l
       CALL DIAGNOSTICS_FILL(r2(1,1,1,l),diagname,0,N(ng),3,bi,bj,myThid)
       WRITE(diagname, '(A,I3.3)') 'att1_', l
       CALL DIAGNOSTICS_FILL(kap1(1,1,1,l),diagname,0,N(ng),3,bi,bj,myThid)
       WRITE(diagname, '(A,I3.3)') 'att2_', l
       CALL DIAGNOSTICS_FILL(kap2(1,1,1,l),diagname,0,N(ng),3,bi,bj,myThid)
#endif
#if defined DARWIN_DIAG_IOP
       WRITE(diagname, '(A,I3.3)') 'a', l
       CALL DIAGNOSTICS_FILL(a3d(1,1,1,l),diagname,0,N(ng),3,bi,bj,myThid)
       WRITE(diagname, '(A,I3.3)') 'bt', l
       CALL DIAGNOSTICS_FILL(bt3d(1,1,1,l),diagname,0,N(ng),3,bi,bj,myThid)
       WRITE(diagname, '(A,I3.3)') 'bb', l
       CALL DIAGNOSTICS_FILL(bb3d(1,1,1,l),diagname,0,N(ng),3,bi,bj,myThid)
       WRITE(diagname, '(A,I3.3)') 'aplk', l
       CALL DIAGNOSTICS_FILL(aplk3d(1,1,1,l),diagname,0,N(ng),3,bi,bj,myThid)
       WRITE(diagname, '(A,I3.3)') 'btplk', l
       CALL DIAGNOSTICS_FILL(btplk3d(1,1,1,l),diagname,0,N(ng),3,bi,bj,myThid)
       WRITE(diagname, '(A,I3.3)') 'bbplk', l
       CALL DIAGNOSTICS_FILL(bbplk3d(1,1,1,l),diagname,0,N(ng),3,bi,bj,myThid)
       WRITE(diagname, '(A,I3.3)') 'aprt', l
       CALL DIAGNOSTICS_FILL(aprt3d(1,1,1,l),diagname,0,N(ng),3,bi,bj,myThid)
       WRITE(diagname, '(A,I3.3)') 'btprt', l
       CALL DIAGNOSTICS_FILL(btprt3d(1,1,1,l),diagname,0,N(ng),3,bi,bj,myThid)
       WRITE(diagname, '(A,I3.3)') 'bbprt', l
       CALL DIAGNOSTICS_FILL(bbprt3d(1,1,1,l),diagname,0,N(ng),3,bi,bj,myThid)
       WRITE(diagname, '(A,I3.3)') 'aCDOM', l
       CALL DIAGNOSTICS_FILL(aCDOM3d(1,1,1,l),diagname,0,N(ng),3,bi,bj,myThid)
#endif
      ENDDO
       IF (DIAGNOSTICS_IS_ON('PARF ', myThid)) THEN
        DO l = 2, nlam
         DO k = 1, N(ng)
          DO ic = 1, sNy
           DO i = 1, sNx
            PARF(i,ic,k,1) = PARF(i,ic,k,1) + PARF(i,ic,k,l)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
        WRITE(diagname, '(A)') 'PARF'
        CALL DIAGNOSTICS_FILL(PARF,diagname,0,N(ng),3,bi,bj,myThid)
       ENDIF
      ENDIF
#endif


