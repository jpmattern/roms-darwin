!
!  converted from "gud_light.F" using darwinfortran_to_romsfortran
!  (written by J. Paul Mattern)
!
      DO k=1,N(ng)

       Chl = 0.0_r8
#if defined DARWIN_GEIDER
#if defined DARWIN_CHLQUOTA
        DO ic = 1, nPhoto
         Chl = Chl + MAX(0.0_r8, Ptracer(1:sNx, 1:sNy, k, bi, bj, iChl+ic-1))
        ENDDO
#else
        Chl = ChlPrev(1:sNx, 1:sNy, k, bi, bj)
#endif
#else
       DO ic = 1, nPhoto
        Chl = Chl + MAX(0.0_r8,Ptracer(1:sNx, 1:sNy, k, bi, bj, ic_+ic-1)*R_ChlC(ic,ng))
       ENDDO
#endif

! TODO should include hFacC
       atten = (katten_w(ng) + katten_chl(ng)*Chl)*DRF(k)
#if defined DARWIN_AVPAR
       PAR(:,:,k,1) = PARF*(1.0_r8 - EXP(-atten))/atten
#else /* USE_MIDPAR */
       PAR(:,:,k,1) = PARF*EXP(-0.5_r8*atten)
#endif
       PAR(:,:,k,1) = PAR(:,:,k,1)*maskC(1:sNx,1:sNy,k,bi,bj)
#if defined DARWIN_DIAGNOSTICS
       IF (useDiagnostics .AND. myIter .GE.0) THEN
        CALL DIAGNOSTICS_FILL(PARF,'PARF ',k,1,3,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(atten,'atten ',k,1,3,bi,bj,myThid)
       ENDIF
#endif
       PARF = PARF*EXP(-atten)


