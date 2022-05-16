#include "cppdefs.h"
      MODULE mod_darwin_sms
#if defined DARWIN
      implicit none

      CONTAINS
!
!=======================================================================
!
!
      SUBROUTINE light(tile, ng, j, IminS, ImaxS, drF,                  &
     &  PAR, PARsur, Bio)
!
      USE mod_biology
      USE mod_scalars, ONLY: dt
!
!  Imported variable declarations.
!
      integer, intent(in) :: tile, ng, j, IminS, ImaxS
      real(r8),intent(in),dimension(IminS:ImaxS,N(ng)) :: drF
      real(r8),intent(out),dimension(IminS:ImaxS,N(ng),nlam) :: PAR
      real(r8),intent(in),dimension(IminS:ImaxS) :: PARsur
      real(r8),intent(in),dimension(IminS:ImaxS,N(ng),NT(ng)) :: Bio
!
!  Local variable declarations
!
#if defined DARWIN_RADTRANS && defined TODO
      real(r8),dimension(nPhoto) :: phychl
      real(r8),dimension(nplank) :: plankcar
      real(r8),dimension(N(ng),nlam) :: aCDOM, actot, bctot, bbctot
      real(r8),dimension(N(ng),nlam) :: aprt_k, btprt_k, a_k, bt_k
      real(r8),dimension(N(ng),nlam) :: bb_k, bbprt_k
      real(r8) :: actotref, atotref
      real(r8) :: part
      integer :: l, ip

      !TODO implement
      real(r8),dimension(IminS:ImaxS,N(ng),nlam) :: chlPrev
#else
      real(r8) :: Chl, atten
#endif
      real(r8) :: dz, PARF

      integer :: i, k, ic
      !integer, parameter :: itemp = 1
! instead of #include "set_bounds.h"
      integer :: Istr, Iend
      Istr   =BOUNDS(ng) % Istr   (tile)
      Iend   =BOUNDS(ng) % Iend   (tile)
#if defined DARWIN_RADTRANS
      !TODO remove
      chlPrev(:,:,:) = 0.1_r8
#endif
      DO i=Istr,Iend
#if defined DARWIN_RADTRANS
        PARF = PARsur(i)

        IF (PARsur(i).gt.0.0_r8) THEN
          DO k=N(ng),1,-1
            ! for info about HFacC see
            ! http://mitgcm.org/sealion/online_documents/node49.html
            ! for drF (delta r_F):
            ! http://mitgcm.org/sealion/online_documents/node48.html
            ! orig code:
            !dz_k(k) = drF(k)*HFacC(i,ic,k,bi,bj)
            dz=drF(i,k)
            part = MAX(Bio(i,k,iPOP), 0.0_r8)
            DO ip=1,nPhoto
#if defined DARWIN_CHLQUOTA
              phychl(ip)=MAX(Bio(i,k,iChl+ip-1),0.0_r8)
#else
              ! TODO implement
              phychl(ip)=MAX(chlPrev(i,k,ip), 0.0_r8)
#endif
            ENDDO
            DO ip=1,nPlank
              plankcar(ip)=MAX(Bio(i,k,ic_+ip-1),0.0_r8)
            ENDDO
#if defined DARWIN_CDOM
! use cdom-like tracer
            CDOM = MAX(Bio(i,k,iCDOM), 0.0_r8)
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
               bctot(k,l) = bctot(k,l) + plankcar(ip)*bphy_mgC(ip,l,ng)*12.0_r8
               bbctot(k,l) = bbctot(k,l) + plankcar(ip)*bbphy_mgC(ip,l,ng)*12.0_r8
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
          ENDDO
!#if defined XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
! ----------------------------------------------------------------------

! use read-in light
          !DO l = 1,nlam
          !  Edwsf(l) = OASIM_Ed(i,ic,bi,bj,l)
          !  Eswsf(l) = OASIM_Es(i,ic,bi,bj,l)
          !ENDDO

          !IF (icefile .NE. ' ' .and. myiter .ge. 0) THEN
          ! DO l = 1,nlam
          !  Edwsf(l) = Edwsf(l)*(1.0_r8 - iceFrac(i,ic,bi,bj))
          !  Eswsf(l) = Eswsf(l)*(1.0_r8 - iceFrac(i,ic,bi,bj))
          ! ENDDO
          !ENDIF

          klow = MIN(gud_radtrans_kmax(ng), kLowC(i,ic,bi,bj))

          CALL DARWIN_RADTRANS_DIRECT()
!        CALL DARWIN_RADTRANS_DIRECT(
!     I dz,rmud(i,ic),Edwsf,Eswsf,a_k,bt_k,bb_k,klow,
!     O Edbot,Esbot,Eubot,Estop,Eutop,
!     O PAR_k, PARF_k,
!     O amp1_k,amp2_k, x_k, y_k,
!     O r1_k,r2_k,kappa1_k,kappa2_k,
!     I myThid)

          DO l = 1,nlam
            DO k = 1,N(ng)
              PAR(i,k,l) = PAR_k(k,l)
            ENDDO
          ENDDO
! ======================================================================
#if defined DARWIN_DIAGNOSTICS
! TODO-low missing diagnostics code block
#endif
!#endif /* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*/
        ELSE
          DO k=1,N(ng)
            PAR(i,k,1) = 0.0_r8
          END DO
        END IF
#else /* DARWIN_RADTRANS */
!
! adapted from darwin_light.h / gud_light.F
! DRF is w-spacing (dz here)
! conversion from uE/s to Wm-2 is performed here

        PARF = PARsur(i)*4.5998007_r8
# if defined DARWIN_DEBUG_BOOSTLIGHT
        PARF = PARF*DARWIN_DEBUG_BOOSTLIGHT
# endif

        IF (PARsur(i).gt.0.0_r8) THEN
          DO k=N(ng),1,-1
            dz=drF(i,k)

            Chl = 0.0_r8
#if defined DARWIN_GEIDER
#if defined DARWIN_CHLQUOTA
            DO ic = 1, nPhoto
              Chl = Chl + MAX(0.0_r8, Bio(i,k,iChl+ic-1))
            ENDDO
#else /* DARWIN_CHLQUOTA */
# error "DARWIN_GEIDER + DARWIN_CHLQUOTA not yet implemented"
            ! TODO implement
            Chl = ChlPrev(1:sNx, 1:sNy, k, bi, bj)
#endif /* DARWIN_CHLQUOTA */
#else /* DARWIN_GEIDER */
            DO ic = 1, nPhoto
              Chl = Chl + MAX(0.0_r8, Bio(i,k,ic_+ic-1)*R_ChlC(ic,ng))
            ENDDO
#endif /* DARWIN_GEIDER */

            atten = (katten_w(ng) + katten_chl(ng)*Chl)*dz
#if defined DARWIN_AVPAR
            PAR(i,k,1) = PARF*(1.0_r8 - EXP(-atten))/atten
#else /* USE_MIDPAR */
            PAR(i,k,1) = PARF*EXP(-0.5_r8*atten)
#endif
#if defined DARWIN_DIAGNOSTICS
            IF (useDiagnostics .AND. myIter .GE.0) THEN
              CALL DIAGNOSTICS_FILL(PARF,'PARF ',k,1,3,bi,bj,myThid)
              CALL DIAGNOSTICS_FILL(atten,'atten ',k,1,3,bi,bj,myThid)
            ENDIF
#endif /* DARWIN_DIAGNOSTICS */
            PARF = PARF*EXP(-atten)
          END DO
        ELSE
          DO k=1,N(ng)
            PAR(i,k,1) = 0.0_r8
          END DO
        END IF
#endif /* DARWIN_RADTRANS */
      END DO
      RETURN
      END SUBROUTINE light
!
!
!
#if defined DARWIN_CARBON
      SUBROUTINE surfforcing(tile, ng, j, IminS, ImaxS, Bio,            &
     &  LBi, LBj,                                                       &
# if ! defined ASSUMED_SHAPE
     &  UBi, UBj,                                                       &
# endif
!#ifdef MASKING
!     &  rmask,                                                          &
!#endif
# if defined BULK_FLUXES
     &  Uwind, Vwind,                                                   &
# else
     &  sustr, svstr,                                                   &
# endif
# if defined DARWIN_CARBON_EXTRAOUT
     &  pH, pCO2, CO2flx,                                               &
# endif
     &  drF, diags)
!
      USE mod_biology
      USE mod_scalars, ONLY: dt,itemp,isalt
!
!  Imported variable declarations.
!
      integer, intent(in) :: tile, ng, j, IminS, ImaxS
      integer, intent(in) :: LBi, LBj
      real(r8),intent(inout),dimension(IminS:ImaxS,N(ng),NT(ng)) :: Bio
#ifdef ASSUMED_SHAPE
!# ifdef MASKING
!      real(r8), intent(in) :: rmask(LBi:,LBj:)
!# endif
# ifdef BULK_FLUXES
      real(r8), intent(in) :: Uwind(LBi:,LBj:)
      real(r8), intent(in) :: Vwind(LBi:,LBj:)
# else
      real(r8), intent(in) :: sustr(LBi:,LBj:)
      real(r8), intent(in) :: svstr(LBi:,LBj:)
# endif
# if defined DARWIN_CARBON_EXTRAOUT
      real(r8), intent(inout) :: pH(LBi:,LBj:,:)
      real(r8), intent(inout) :: CO2flx(LBi:,LBj:)
      real(r8), intent(inout) :: pCO2(LBi:,LBj:,:)
# endif
#else
      integer, intent(in) :: UBi, UBj
!# ifdef MASKING
!      real(r8), intent(in) :: rmask(LBi:UBi,LBj:UBj)
!# endif
# ifdef BULK_FLUXES
      real(r8), intent(in) :: Uwind(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: Vwind(LBi:UBi,LBj:UBj)
# else
      real(r8), intent(in) :: sustr(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: svstr(LBi:UBi,LBj:UBj)
# endif
# if defined DARWIN_CARBON_EXTRAOUT
      real(r8), intent(inout) :: pH(LBi:UBi,LBj:UBj,UBk)
      real(r8), intent(inout) :: CO2flx(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: pCO2(LBi:UBi,LBj:UBj,UBk)
# endif
#endif
      real(r8),dimension(IminS:ImaxS,1:N(ng)) :: drF
      real(r8),intent(inout),                                           &
     &  dimension(IminS:ImaxS,N(ng),darwin_nDiag) ::diags
!
!  Local variable declarations
!
      integer :: i, k, ks
      real(r8),dimension(IminS:ImaxS) :: gDIC, gALK, gO2
      real(r8) :: pisvel0, Kwexch0
      real(r8) :: SchmidtNoO2, SchmidtNoDIC
      real(r8) :: Kwexch
      real(r8) :: O2sat, FluxO2, FluxCO2, AtmosP
      real(r8) :: surftemp, surfsalt
#if ! defined DARWIN_CARBON_EXTRAOUT
      real(r8),dimension(IminS:ImaxS,1:N(ng)) :: pCO2
      real(r8),dimension(IminS:ImaxS,1:N(ng)) :: pH
#endif
#if ! defined BULK_FLUXES
      real(r8) :: cff1
#endif
      real(r8) :: windSpeed_sq
      real(r8) :: aTT, aTK, aTS, aTS2, aTS3, aTS4, aTS5
      real(r8) :: ttemp, stemp, o2s, oCnew
      real(r8) :: surfdic,surfalk,surfphos,surfsi
      real(r8) :: pCO2sat, co3dummy
      real(r8) :: AtmospCO2
      ! for call to DARWIN_CARBON_COEFFS/DARWIN_CALC_PCO2_APPROX
      real(r8) :: fugf
      real(r8) :: ff
      real(r8) :: ak0
      real(r8) :: ak1
      real(r8) :: ak2
      real(r8) :: akb
      real(r8) :: ak1p
      real(r8) :: ak2p
      real(r8) :: ak3p
      real(r8) :: aksi
      real(r8) :: akw
      real(r8) :: aks
      real(r8) :: akf
      real(r8) :: bt
      real(r8) :: st
      real(r8) :: ft
      real(r8) :: Ksp_TP_Calc

! instead of #include "set_bounds.h"
      integer :: Istr, Iend
      Istr   =BOUNDS(ng) % Istr   (tile)
      Iend   =BOUNDS(ng) % Iend   (tile)
      !cff=dtdays(ng)*VmaxS(ng)

      !TODO implement atmos file reading?
      AtmospCO2 = gud_atmos_pCO2(ng)
#if ! defined BULK_FLUXES
      cff1=rho0*550.0_r8
#endif

#if ! defined DARWIN_CARBON_EXTRAOUT
      pH(:,:) = 8.0_r8

#endif
      ks = N(ng)
      DO i=Istr,Iend
        gDIC(i) = 0.0_r8
        gALK(i) = 0.0_r8
        gO2(i) = 0.0_r8
      ENDDO

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! Compute AtmosP and Kwexch0 which are used for flux of CO2 and O2
      DO i=Istr,Iend
#if defined USE_PLOAD
! Convert anomalous pressure pLoad (in_ Pa) from atmospheric model
! to total pressure (in_ Atm)
! Note: it is assumed the reference atmospheric pressure is 1Atm=1013mb
! rather than the actual ref. pressure from Atm. model so that on
! average AtmosP is about 1 Atm.
        AtmosP= 1.0_r8 + pLoad(i,j)/Pa2Atm(ng)
#else
! taken from gud_init_varia.F
        AtmosP= 1.0_r8
#endif

# ifdef BULK_FLUXES
        windSpeed_sq=Uwind(i,j)**2+Vwind(i,j)**2
# else
        windSpeed_sq=cff1*SQRT((0.5_r8*(sustr(i,j)+sustr(i+1,j)))**2+   &
     &    (0.5_r8*(svstr(i,j)+svstr(i,j+1)))**2)
# endif
! Pre-compute part of exchange coefficient: pisvel*(1-fice)
! Schmidt number is accounted for later
        pisvel0 = 0.337_r8 * windSpeed_sq/3.6e5_r8
        !Kwexch0 = pisvel0 *(1.0_r8 - iceFrac(i))
        Kwexch0 = pisvel0
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! flux of alkalinity

#if defined DARWIN_OLD_VIRTUALFLUX
# error "DARWIN_OLD_VIRTUALFLUX not implemented."
! calculate virtual flux
! EminusPforV = dS/dt*(1/Sglob)
! NOTE: Be very careful with signs here!
! Positive EminusPforV => loss of water to atmos and increase
! in salinity. Thus, also increase in_ other surface tracers
! (i.e. positive virtual flux into surface layer)
        ! recip_drF is the reciprocal of drF
        ! with definition of drF as above
        ! "recip_drF(ks)*recip_hFacC(i)" is equivalent to "/drF(i,ks)"
        ! orig code:
        !gALK(i) = recip_drF(ks) * gsm_ALK * surfaceForcingS(i,j)/gsm_S
        gALK(i) = gsm_ALK * surfaceForcingS(i,j)/gsm_S / drF(i,ks)
#endif /* DARWIN_OLD_VIRTUALFLUX */

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! flux of O2

! calculate SCHMIDT NO. for O2
        ttemp = Bio(i,ks,itemp)
        stemp = Bio(i,ks,isalt)

        SchmidtNoO2 =sox1(ng)+ sox2(ng) * ttemp+ sox3(ng) * ttemp*ttemp+ sox4(ng) * ttemp*ttemp*ttemp

! Determine surface flux of O2
! exchange coeff accounting for ice cover and Schmidt no.
! Kwexch0= pisvel*(1-fice): previously computed in_ dic_surfforcing.F

        Kwexch = Kwexch0/ sqrt(SchmidtNoO2/660.0_r8)

! determine saturation O2
! using Garcia and Gordon (1992), L&O (mistake in_ original ?)
        aTT = 298.15_r8 -ttemp
        aTK = 273.15_r8 +ttemp
        aTS = log(aTT/aTK)
        aTS2 = aTS*aTS
        aTS3 = aTS2*aTS
        aTS4 = aTS3*aTS
        aTS5 = aTS4*aTS

        oCnew=oA0(ng)+oA1(ng)*aTS+oA2(ng)*aTS2+oA3(ng)*aTS3+oA4(ng)*aTS4+oA5(ng)*aTS5+(oB0(ng)+oB1(ng)*aTS+oB2(ng)*aTS2+oB3(ng)*aTS3)*stemp+oC0(ng)*stemp*stemp

        o2s = EXP(oCnew)

! molar volume of O2: O2mol2L = 22.391_r8 L mol-1 = 0.022391_r8 L mmol-1
! o2s in_ ml/l = l/m3
! O2sat = o2s / O2mmol2L (in_ mmol/m3)
! Convert from ml/l to mmol/m^3
        O2sat = o2s/22.3916e-3_r8

! Determine flux, inc. correction for local atmos surface pressure
        FluxO2 = Kwexch*(AtmosP*O2sat- Bio(i,ks,iO2))

        ! recip_drF is the reciprocal of drF
        ! with definition of drF as above
        ! "recip_drF(ks)*recip_hFacC(i)" is equivalent to "/drF(i,ks)"
        ! orig code:
        !gO2(i) =recip_drF(ks)*recip_hFacC(i)*FluxO2
        gO2(i) = FluxO2/drF(i,ks)
! ======================================================================
        DO k=1,N(ng)
! ======================================================================

! determine inorganic carbon chem coefficients
! put bounds on tracers so pH solver doesn't blow up
          surfsalt = MAX(surfSaltMin(ng),MIN(surfSaltMax(ng), Bio(i,k,isalt)))
          surftemp = MAX(surfTempMin(ng),MIN(surfTempMax(ng), Bio(i,k,itemp)))
#if defined DARWIN_VERBOSE_PLANK_OLD
          IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
            write(*,'(a,1x,a16,2x,10(f,1x,a,1x))') 'surfforcing',       'surftemp=',surftemp,',surfsalt=',surfsalt
          END IF
#endif

          CALL DARWIN_CARBON_COEFFS(surftemp,surfsalt,drF(i,:),         &
     &      fugf,ff,ak0,ak1,ak2,akb,ak1p,ak2p,ak3p,aksi,akw,aks,        &
     &      akf,bt,st,ft,Ksp_TP_Calc,                                   &
     &      Istr,Iend,                                                  &
#if defined DARWIN_VERBOSE_PLANK_OLD
     &      i,j,                                                        &
#endif
     &      k,ks)
!====================================================================

! pCO2 solver...
#if defined DARWIN_VERBOSE_PLANK_OLD
          IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
            write(*,'(a,1x,a16,2x,10(f,1x,a,1x))') 'surfforcing',       'ak0=',ak0
          END IF
#endif
! put bounds on tracers so pH solver doesn't blow up
          surfdic = ptr2mol(ng) * MAX(surfDICMin(ng),MIN(surfDICMax(ng), Bio(i,k,iDIC)))
          surfalk = ptr2mol(ng) * MAX(surfALKMin(ng),MIN(surfALKMax(ng), Bio(i,k,iALK)))
          surfphos = ptr2mol(ng) * MAX(surfPO4Min(ng),MIN(surfPO4Max(ng), Bio(i,k,iPO4)))
          surfsi = ptr2mol(ng) * MAX(surfSiMinInit(ng),MIN(surfSiMax(ng), Bio(i,k,iSiO2)))
          CALL DARWIN_CALC_PCO2_APPROX(ng,surftemp, surfsalt,           &
#if defined DARWIN_VERBOSE_PLANK_OLD
     &      i,j,k,                                                      &
#endif
     &      surfdic, surfphos, surfsi, surfalk,                         &
     &      ak1, ak2,                                                   &
     &      ak1p, ak2p, ak3p,                                           &
     &      aks, akb, akw,                                              &
     &      aksi, akf,                                                  &
     &      ak0, fugf,                                                  &
     &      ff, bt, st, ft,                                             &
#if defined DARWIN_CARBON_EXTRAOUT
     &      pH(i,j,k), pCO2(i,j,k),                                     &
#else
     &      pH(i,k), pCO2(i,k),                                         &
#endif
     &      co3dummy)

          IF (k .EQ. ks) THEN
            ttemp = Bio(i,k,itemp)
! calculate SCHMIDT NO. for CO2
            SchmidtNoDIC =sca1(ng) +sca2(ng)*ttemp +sca3(ng)*ttemp*ttemp +sca4(ng)*ttemp*ttemp*ttemp
! make sure Schmidt number is not negative (will happen if temp>39C)
            SchmidtNoDIC = MAX(1.0e-2_r8, SchmidtNoDIC)

! Determine surface flux (FDIC)
! first correct pCO2at for surface atmos pressure
            pCO2sat = AtmosP*AtmospCO2

! then account for Schmidt number
            Kwexch = Kwexch0/ sqrt(SchmidtNoDIC/660.0_r8)

! Flux = kw*rho*(ff*pCO2atm-k0*FugFac*pCO2ocean)
#if defined DARWIN_VERBOSE_PLANK_OLD
            IF(i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,'(a,i2,1x,a16,2x,10(f,1x,a,1x))') 'gDIC',i,       &
#if defined DARWIN_CARBON_EXTRAOUT
     &          'FluxCO2=',Kwexch,'*(',ff,'*',pCO2sat,'-',pCO2(i,j,k),  '*',fugf,'*',ak0,')'
#else
     &          'FluxCO2=',Kwexch,'*(',ff,'*',pCO2sat,'-',pCO2(i,k), '*',fugf,'*',ak0,')'
#endif
            END IF
#endif
#if defined DARWIN_CARBON_EXTRAOUT
            FluxCO2 =Kwexch*(ff*pCO2sat -pCO2(i,j,k)*fugf*ak0)
#else
            FluxCO2 =Kwexch*(ff*pCO2sat -pCO2(i,k)*fugf*ak0)
#endif
! convert flux (mol kg-1 m s-1) to (mmol m-2 s-1)
#if defined DARWIN_VERBOSE_PLANK_OLD
            IF(i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,'(a,i2,1x,a16,2x,10(f,1x,a,1x))') 'gDIC',i,       &
     &          'FluxCO2=',FluxCO2,'/',m3perkg(ng),'/',ptr2mol(ng)
            END IF
#endif
            FluxCO2 = FluxCO2/m3perkg(ng)/ptr2mol(ng)
#if defined DARWIN_CARBON_EXTRAOUT
            CO2flx(i,j) = FluxCO2
#endif
#if defined DARWIN_OLD_VIRTUALFLUX
! calculate virtual flux
! EminusPforV = dS/dt*(1/Sglob)
! NOTE: Be very careful with signs here!
! Positive EminusPforV => loss of water to atmos and increase
! in_ salinity. Thus, also increase in_ other surface tracers
! (i.e. positive virtual flux into surface layer)
! ...so here, VirtualFLux = dC/dt!
            VirtualFlux(i,j)=gsm_DIC*surfaceForcingS(i,j)/gsm_s
! OR
! let virtual flux be zero
! VirtualFlux(i,j)=0.d0
#endif /* DARWIN_OLD_VIRTUALFLUX */

! update tendency
            ! recip_drF is the reciprocal of drF
            ! with definition of drF as above
            ! "recip_drF(ks)*recip_hFacC(i)" is equivalent to "/drF(i,ks)"
            ! orig code: (without DARWIN_OLD_VIRTUALFLUX)
            ! gDIC(i) =recip_drF(i,ks)*recip_hFacC(i)*FluxCO2
#if defined DARWIN_VERBOSE_PLANK_OLD
            IF(i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,'(a,i2,1x,a16,2x,10(f,1x,a,1x))') 'gDIC',i,       &
     &          'gDIC=(',FluxCO2,                                    &
# if defined DARWIN_OLD_VIRTUALFLUX
     &          '+',VirtualFlux(i,j)                                    &
# endif
     &          ')/',drF(i,ks)
            END IF
#endif
            gDIC(i)=(FluxCO2                                         &
#if defined DARWIN_OLD_VIRTUALFLUX
     &        +VirtualFlux(i,j)                                         &
#endif
     &        )/drF(i,ks)
! k is at surface
          ENDIF
          Bio(i,N(ng),iDIC)=Bio(i,N(ng),iDIC)+ gDIC(i)
          Bio(i,N(ng),iALK)=Bio(i,N(ng),iALK)+ gALK(i)
          Bio(i,N(ng),iO2) =Bio(i,N(ng),iO2) + gO2(i)
        END DO
      END DO
      RETURN
      END SUBROUTINE surfforcing

!
!
!
#include <darwin_mod_carbon_chem.h>
!
!
!
#endif /*DARWIN_CARBON*/
      SUBROUTINE tempfunc(tile, ng, IminS, ImaxS, Bio,                  &
     &  photoFun, grazFun, reminFun, mortFun, mort2Fun, uptakeFun,      &
     &  diags)
!
      USE mod_biology
      USE mod_scalars, ONLY: dt
!
!  Imported variable declarations.
!
      integer, intent(in) :: tile, ng, IminS, ImaxS
      real(r8),intent(inout),dimension(IminS:ImaxS,N(ng),NT(ng)) :: Bio
      real(r8),intent(inout),                                           &
     &  dimension(IminS:ImaxS,N(ng),darwin_nDiag) ::diags
      real(r8), intent(out), dimension(IminS:ImaxS,N(ng), nplank) ::    &
     &  photoFun, grazFun
      real(r8), intent(out), dimension(IminS:ImaxS,N(ng)) ::            &
     &  reminFun, mortFun, mort2Fun, uptakeFun
      !real(r8),intent(in),dimension(IminS:ImaxS,N(ng)) :: Light
!
!  Local variable declarations
!
      integer :: i, k, l, ic

      integer, parameter :: itemp = 1

#if DARWIN_TEMP_VERSION == 2
      real(r8) :: Tkel
#elif DARWIN_TEMP_VERSION == 3
      real(r8) :: TempAe, Tempref
#endif
! instead of #include "set_bounds.h"
      integer :: Istr, Iend
      Istr   =BOUNDS(ng) % Istr   (tile)
      Iend   =BOUNDS(ng) % Iend   (tile)
      !cff=dtdays(ng)*VmaxS(ng)
      DO k=1,N(ng)
        DO i=Istr,Iend
# include <darwin_tempfunc.h>
        END DO
      END DO
      RETURN
      END SUBROUTINE tempfunc
!
!
!
      SUBROUTINE darwin_model(tile, ng, IminS, ImaxS, Bio, PAR,         &
#if defined DARWIN_VERBOSE_PLANK_OLD || defined DARWIN_VERBOSE_PLANK
     &  j,                                                              &
#endif
#if defined DARWIN_PLANK_BUOYCTRL
     &  limitnut_save, limitlight_save,                                 &
#endif
     &  photoTempFunc, reminTempFunc, uptakeTempFunc, diags)
!
      USE mod_biology
      USE mod_scalars, ONLY: dt
!
!  Imported variable declarations.
!
#if defined DARWIN_VERBOSE_PLANK_OLD || defined DARWIN_VERBOSE_PLANK
      integer, intent(in) :: j
#endif
      integer, intent(in) :: tile, ng, IminS, ImaxS
      real(r8),intent(inout),dimension(IminS:ImaxS,N(ng),NT(ng)) :: Bio
      real(r8),intent(in),dimension(IminS:ImaxS,N(ng),nlam) :: PAR
      real(r8),intent(inout),                                           &
     &  dimension(IminS:ImaxS,N(ng),darwin_nDiag) ::diags
      real(r8), intent(in), dimension(IminS:ImaxS,N(ng),nplank) ::      &
     &  photoTempFunc
      real(r8), intent(in), dimension(IminS:ImaxS,N(ng)) ::             &
     &  reminTempFunc, uptakeTempFunc
#if defined DARWIN_PLANK_BUOYCTRL
      real(r8), intent(out), dimension(IminS:ImaxS,N(ng),nplank) ::     &
     &  limitnut_save, limitlight_save
#endif
      !real(r8),intent(in),dimension(IminS:ImaxS,N(ng)) :: Light
!
!  Local variable declarations
!
      integer :: i, k, l, ic
      real(r8) :: PARtot

      !integer, parameter :: itemp = 1

      real(r8) :: X, Qn, Qc, PCm
      real(r8) :: inhibNH4
      real(r8) :: ngrow
      real(r8) :: chlout
      real(r8) :: fracNO2, fracNO3, fracNH4
      real(r8) :: limitp, limitsi, limitfe, limitNO2, limitNO3, limitNH4
      real(r8) :: limitn, limitnut, limitpCO2, limitI
      real(r8) :: PC
      real(r8) :: synthChl
      real(r8) :: growth
      real(r8) :: uptakeDIC, uptakeFeT, uptakeN, uptakeNH4, uptakeNO2
      real(r8) :: uptakeNO3, uptakePO4, uptakeSiO2, uptakeO2, uptakePOC
      real(r8) :: uptakePON, uptakePOP, uptakePOFe, uptakeDOC, uptakeDON
      real(r8) :: uptakeDOP, uptakeDOFe
      real(r8) :: consumDIC, consumDOC, consumDON, consumDOP, consumDOFe
      real(r8) :: consumO2, consumFeT, consumNH4, consumNO2, consumNO3
      real(r8) :: consumPO4, consumPOC, consumPOFe,consumPON, consumPOP
      real(r8) :: consumPOSi, consumSiO2
      real(r8) :: consumDIC_PIC
      real(r8) :: reminDOC, reminDOFe, reminDON, reminDOP, reminPOC
      real(r8) :: reminPOFe, reminPON, reminPOP, reminPOSi
      real(r8) :: solubilPOC, solubilPOFe, solubilPON, solubilPOP
      real(r8) :: prodNO2, prodNO3
      real(r8) :: hydrolPOC, hydrolPOFe, hydrolPON, hydrolPOP
      real(r8) :: respPOC, respPON, respPOP, respPOFe, respDOC, respDON
      real(r8) :: respDOP, respDOFe, respPOSi
      real(r8) :: mu, muO, muPON, muPOC, muPOP, muPOFe, muDON, muDOC
      real(r8) :: muDOP, muDOFe
      real(r8) :: disscPIC
#if defined DARWIN_GEIDER
      real(r8) :: alpha_I, alpha_I_growth, acclim
      real(r8) :: tmp, Ek, EkoverE, rhochl, chl2c
# if defined DARWIN_CHLQUOTA
      real(r8) :: QChl
# endif /* DARWIN_CHLQUOTA */
#endif /* DARWIN_GEIDER */
#if defined DARWIN_CDOM
      real(r8) :: reminPOP_CDOM, reminPOC_CDOM, reminPON_CDOM
      real(r8) :: reminPOFe_CDOM
      real(r8) :: degrCDOM_DOC, degrCDOM_DON, degrCDOM_DOP
      real(r8) :: degrCDOM_DOFe
#endif /* DARWIN_CDOM */
#if defined DARWIN_DENIT
      real(r8) :: denitNH4, denit
#endif /* DARWIN_DENIT */
#if defined DARWIN_PQUOTA
      real(r8) :: Qp, regQ, uptakePO4, limitp
#endif
! instead of #include "set_bounds.h"
      integer :: Istr, Iend
      Istr   =BOUNDS(ng) % Istr   (tile)
      Iend   =BOUNDS(ng) % Iend   (tile)
      !cff=dtdays(ng)*VmaxS(ng)
      DO k=1,N(ng)
        DO i=Istr,Iend
#if defined DARWIN_DEBUG_DIAG
          diags(i,k,iOnes)=diags(i,k,iOnes)+dtbio(ng)/86400.0_r8
#endif
          PARtot=SUM(PAR(i,k,:))
# include <darwin_model.h>
        END DO
      END DO
      RETURN
      END SUBROUTINE darwin_model
!
!
!
      SUBROUTINE darwin_grazing(tile, ng, IminS, ImaxS, Bio,            &
     &  grazTempFunc, reminTempFunc, mortTempFunc, mort2TempFunc,       &
#if defined DARWIN_VERBOSE_PLANK_OLD || defined DARWIN_VERBOSE_PLANK
     &  j,                                                              &
#endif
     &  diags)
!
      USE mod_biology
      USE mod_scalars, ONLY: dt
!
!  Imported variable declarations.
!
      integer, intent(in) :: tile, ng, IminS, ImaxS
#if defined DARWIN_VERBOSE_PLANK_OLD || defined DARWIN_VERBOSE_PLANK
      integer, intent(in) :: j
#endif
      real(r8), intent(in), dimension(IminS:ImaxS,N(ng),nplank) ::      &
     &  grazTempFunc
      real(r8), intent(in), dimension(IminS:ImaxS,N(ng)) ::             &
     &  reminTempFunc, mortTempFunc, mort2TempFunc

      real(r8),intent(inout),dimension(IminS:ImaxS,N(ng),NT(ng)) :: Bio
      real(r8),intent(inout),                                           &
     &  dimension(IminS:ImaxS,N(ng),darwin_nDiag) ::diags
      !real(r8),intent(in),dimension(IminS:ImaxS,N(ng)) :: Light
!
!  Local variable declarations
!
      integer :: i, k, ip, iz
      real(r8) :: tmp, Xe
      real(r8) :: graz2POC, graz2PON, graz2POP, graz2POSI, graz2PIC
      real(r8) :: graz2POFE, graz2OC, graz2ON, graz2OP, graz2OFE
      real(r8) :: regQn, regQp, regQfe, regQc
      real(r8) :: sumprey, sumpref
      real(r8) :: totkillc, totkilln, totkillp, totkillsi, totkillfe
      real(r8) :: totkillexpc, totkillexpn, totkillexpp, totkillexpfe
      real(r8) :: predexpc, predexpn, predexpp, predexpfe
      real(r8) :: grazphy, expfrac
      real(r8) :: exude_DOC, exude_POC, exude_DON, exude_PON
      real(r8) :: exude_DOFe, exude_POFe, exude_DOP, exude_POP
      real(r8) :: exude_POSi, exude_PIC, respir, respir_c
      real(r8) :: mortX, mortX2
#if defined DARWIN_CARBON
      real(r8) :: totkillPIC
#endif
#if defined DARWIN_CDOM
      real(r8) :: graz2CDOM, exude_CDOM
#endif /* DARWIN_CDOM */

      real(r8),dimension(nplank) :: X, Xi, Qc, Qn, Qp, Qsi, Qfe
      real(r8),dimension(nplank) :: mort_c
      real(r8),dimension(nplank) :: preygraz, predgrazc
#if defined DARWIN_NQUOTA
      real(r8),dimension(nplank) :: predgrazn
#endif
#if defined DARWIN_PQUOTA
      real(r8),dimension(nplank) :: predgrazp
#endif
#if defined DARWIN_FEQUOTA
      real(r8),dimension(nplank) :: predgrazfe
#endif
#if defined DARWIN_CHLQUOTA
      real(r8),dimension(nChl) :: QChl
#endif
#if defined DARWIN_VERBOSE_PLANK
      real(r8),dimension(nplank) :: preygraz_rate
      real(r8),dimension(nplank,nplank) :: grazphy_all
#endif

      !integer, parameter :: itemp = 1

! instead of #include "set_bounds.h"
      integer :: Istr, Iend
      Istr   =BOUNDS(ng) % Istr   (tile)
      Iend   =BOUNDS(ng) % Iend   (tile)
      !cff=dtdays(ng)*VmaxS(ng)
      DO k=1,N(ng)
        DO i=Istr,Iend
# include <darwin_grazing.h>
        END DO
      END DO
      RETURN
      END SUBROUTINE darwin_grazing
!
!
!
      SUBROUTINE darwin_fe_chem(tile, ng, IminS, ImaxS, Bio, inputFe,   &
     &  freefe,                                                         &
!#ifdef MASKING
!     &  rmask,                                                          &
!#endif
     &  diags)
!
      USE mod_biology
      USE mod_scalars, ONLY: dt
!
!  Imported variable declarations.
!
      integer, intent(in) :: tile, ng, IminS, ImaxS
      real(r8),intent(inout),dimension(IminS:ImaxS,N(ng),NT(ng)) :: Bio
      real(r8),intent(in),dimension(IminS:ImaxS) :: inputFe
      real(r8),intent(out),dimension(IminS:ImaxS,1:N(ng)) :: freefe
!#if defined ASSUMED_SHAPE
!#ifdef MASKING
!      real(r8), intent(in) :: rmask(LBi:,LBj:)
!#endif
!#else
!#ifdef MASKING
!      real(r8), intent(in) :: rmask(LBi:UBi,LBj:UBj)
!#endif
!#endif
      real(r8),intent(inout),                                           &
     &  dimension(IminS:ImaxS,N(ng),darwin_nDiag) ::diags
      !real(r8),intent(in),dimension(IminS:ImaxS,N(ng)) :: Light
!
!  Local variable declarations
!
      integer :: i, k
      real(r8) :: FeL, lig
      !integer, parameter :: itemp = 1

! instead of #include "set_bounds.h"
      integer :: Istr, Iend
      Istr   =BOUNDS(ng) % Istr   (tile)
      Iend   =BOUNDS(ng) % Iend   (tile)
      !cff=dtdays(ng)*VmaxS(ng)
!
!  converted from "gud_fe_chem.F" using darwinfortran_to_romsfortran
!  (written by J. Paul Mattern)
!  with manual adaptations
!
      DO k=1,N(ng)
        DO i=Istr,Iend
!          IF (maskC(i, k) .GT. 0.0_r8) THEN
            lig = (-ligand_stab(ng)*Bio(i,k,iFeT) +                     &
     & ligand_stab(ng)*ligand_tot(ng) -                                 &
     & 1.0_r8 +                                                         &
     & ( ( ligand_stab(ng)*Bio(i,k,iFeT) -                              &
     & ligand_stab(ng)*ligand_tot(ng)+1.0_r8                            &
     & )**2 +                                                           &
     & 4.0_r8*ligand_stab(ng)*ligand_tot(ng)                            &
     & )**0.5_r8                                                        &
     & )/(2.0_r8*ligand_stab(ng))

            FeL = ligand_tot(ng) - lig
            freefe(i,k) = Bio(i,k,iFeT) - FeL
#if defined DARWIN_MINFE
            freefe(i,k) = MIN(freefe(i,k), freefemax(ng))
! TODO: allow statement back in (dt correction required?)
!            IF (maskInC(i) .GT. 0.0_r8) THEN
!              Bio(i,k,iFeT)(i,k) = FeL + freefe(i,k)
!            ENDIF
#endif
!          ELSE
!            freefe(i,k) = 0.0_r8
!          ENDIF
        ENDDO
      ENDDO
      RETURN
      END SUBROUTINE darwin_fe_chem
!
!
!
      SUBROUTINE darwin_fe_chem2(tile, ng, IminS, ImaxS, Bio, inputFe,  &
     &  freefe,                                                         &
!#ifdef MASKING
!     &  rmask,                                                          &
!#endif
#if defined DARWIN_VERBOSE_IFET
     &  j,                                                              &
#endif
     &  drF, diags)
!
      USE mod_biology
      USE mod_scalars, ONLY: dt
!
!  Imported variable declarations.
!
      integer, intent(in) :: tile, ng, IminS, ImaxS
#if defined DARWIN_VERBOSE_IFET
      integer, intent(in) :: j
#endif
      real(r8),intent(inout),dimension(IminS:ImaxS,N(ng),NT(ng)) :: Bio
      real(r8),intent(in),dimension(IminS:ImaxS) :: inputFe
      real(r8),intent(in),dimension(IminS:ImaxS,1:N(ng)) :: freefe
!#if defined ASSUMED_SHAPE
!#ifdef MASKING
!      real(r8), intent(in) :: rmask(LBi:,LBj:)
!#endif
!#else
!#ifdef MASKING
!      real(r8), intent(in) :: rmask(LBi:UBi,LBj:UBj)
!#endif
!#endif
      real(r8),intent(in),dimension(IminS:ImaxS,1:N(ng)) :: drF
      real(r8),intent(inout),                                           &
     &  dimension(IminS:ImaxS,N(ng),darwin_nDiag) ::diags
!
!  Local variable declarations
!
      integer :: i, k
      real(r8) :: scv, flx
#if defined DARWIN_PART_SCAV_POP || defined DARWIN_PART_SCAV
      real(r8) :: scav_poc
#endif

! instead of #include "set_bounds.h"
      integer :: Istr, Iend
      Istr   =BOUNDS(ng) % Istr   (tile)
      Iend   =BOUNDS(ng) % Iend   (tile)
!
!  converted from "gud_forcing.F" using darwinfortran_to_romsfortran
!  (written by J. Paul Mattern)
!  with manual adaptations
!
! iron dust input
      DO i=Istr,Iend
        Bio(i,N(ng),iFet)=Bio(i,N(ng),iFet) + dtbio(ng)*(alpfe(ng) *       &
     &    inputFe(i)/drF(i,N(ng)))
      ENDDO
#if defined DARWIN_VERBOSE_IFET
        write(*,'(a,3i3,3(a4,f))') '<<2x',IminS,ImaxS,j,                &
     &    '(1)',alpfe(ng),                                              &
     &    '(2)',maxval(inputFe),                                        &
     &    '(3)',1.0_r8/minval(abs(drF(:,N(ng))))
        write(*,*) '<<2a',IminS,ImaxS,j,maxval(Bio(:,:,iFet))
#endif

! scavenging
      DO k=1,N(ng)
        DO i=IminS,ImaxS
#if defined DARWIN_PART_SCAV_POP
          scav_poc = MAX(0.0_r8, Bio(i,k,iPOP))/scav_R_POPPOC(ng)
          scv = scav_rat(ng)*scav_inter(ng)*(scav_poc**scav_exp(ng))
#elif defined DARWIN_PART_SCAV
          scav_poc = MAX(0.0_r8, Bio(i,k,iPOC))
          scv = scav_rat(ng)*scav_inter(ng)*(scav_poc**scav_exp(ng))
#else
          scv = scav(ng)
#endif
          Bio(i,k,iFet) = Bio(i,k,iFet) + dtbio(ng)*(-scv*freefe(i,k))
        ENDDO
      ENDDO
#if defined DARWIN_VERBOSE_IFET
        write(*,*) '<<2b',IminS,ImaxS,j,maxval(Bio(:,:,iFet))
#endif
! iron flux
      DO i=IminS,ImaxS
#if defined DARWIN_IRON_SED_SOURCE_VARIABLE
        !TODO this needs to be improved
        flx = fesedflux_pcm(ng)*wP_sink(ng)*R_CP_fesed(ng)*             &
     &    MAX(0.0_r8, Bio(i,2,iPOP))
#else
        flx = fesedflux(ng)
#endif
        Bio(i,1,iFet)=Bio(i,1,iFet)+dtbio(ng)*(flx/drF(i,1))
      END DO
#if defined DARWIN_VERBOSE_IFET
        write(*,*) '<<2c',IminS,ImaxS,j,maxval(Bio(:,:,iFet))
#endif
      RETURN
      END SUBROUTINE darwin_fe_chem2
#if defined DARWIN_ROMSSINKING
!
!
!
#if defined DARWIN_INTERNAL_ASSERTSINK || defined DARWIN_VERBOSE_SINKING || defined DARWIN_VERBOSE_SINKING_EXTRAOUT || defined BIO_SEDIMENTVARIABLES
      SUBROUTINE sinking(tile, ng, j, IminS, ImaxS, LBi, LBj,           &
#else
      PURE SUBROUTINE sinking(tile, ng, j, IminS, ImaxS, LBi, LBj,      &
#endif
#if ! defined ASSUMED_SHAPE
     &  UBi, UBj, UBk,                                                  &
#endif
#if defined DARWIN_PLANK_BUOYCTRL
     &  limitnut, limitlight,                                           &
#endif
     &  Hz, Hz_inv, Hz_inv2, Hz_inv3, z_w, Bio)
!
      USE mod_biology
      USE mod_scalars, ONLY: dt
#if defined BIO_SEDIMENTVARIABLES
      USE mod_biosedimentvars
#endif
!
!  Imported variable declarations.
!
      integer, intent(in) :: j, tile, ng, IminS, ImaxS
      integer, intent(in) :: LBi, LBj
      real(r8),intent(inout),dimension(IminS:ImaxS,N(ng),NT(ng)) :: Bio
# ifdef ASSUMED_SHAPE
      real(r8), intent(in) :: Hz(LBi:,LBj:,:)
      real(r8), intent(in) :: z_w(LBi:,LBj:,0:)
# else
      integer, intent(in) :: UBi,UBj, UBk
      real(r8), intent(in) :: Hz(LBi:UBi,LBj:UBj,UBk)
      real(r8), intent(in) :: z_w(LBi:UBi,LBj:UBj,0:UBk)
# endif
#if defined DARWIN_PLANK_BUOYCTRL
      real(r8), intent(in), dimension(IminS:ImaxS,N(ng),nplank) ::  &
     &  limitnut, limitlight
#endif
      real(r8), intent(in), dimension(IminS:ImaxS,N(ng)) :: Hz_inv
      real(r8), intent(in), dimension(IminS:ImaxS,N(ng)) :: Hz_inv2
      real(r8), intent(in), dimension(IminS:ImaxS,N(ng)) :: Hz_inv3
!
!  Local variable declarations
!

      real(r8) :: cff
      real(r8) :: rate
#if defined DARWIN_PLANK_BUOYCTRL
      real(r8) :: sink_k
      real(r8) :: condopt
#endif

      integer :: i, k, isink, ibio, ks
#if defined DARWIN_PLANK_BUOYCTRL
      integer :: k0, ip, kopt
#endif
      integer, dimension(IminS:ImaxS,N(ng)) :: ksource

      real(r8) :: cffL, cffR, cu, dltL, dltR
#if defined DARWIN_INTERNAL_ASSERTSINK
      real(r8) :: mass0, mass1
#endif
#if defined BIO_SEDIMENTVARIABLES
      integer :: ised
      real(r8) :: cff1
#endif
      real(r8), dimension(IminS:ImaxS,0:N(ng)+1) :: FC
      !real(r8), dimension(IminS:ImaxS,0:N(ng)) :: FC
#if defined DARWIN_VERBOSE_SINKING_EXTRAOUT
      real(r8), dimension(IminS:ImaxS,0:N(ng)+1) :: FCold
      real(r8), dimension(IminS:ImaxS,N(ng)) :: cus
#endif
      real(r8), dimension(IminS:ImaxS,N(ng)) :: WL
      real(r8), dimension(IminS:ImaxS,N(ng)) :: WR
      real(r8), dimension(IminS:ImaxS,N(ng)) :: bL
      real(r8), dimension(IminS:ImaxS,N(ng)) :: bR
      real(r8), dimension(IminS:ImaxS,N(ng)) :: qc
#if defined DARWIN_PLANK_BUOYCTRL
      real(r8), dimension(IminS:ImaxS,N(ng)) :: Bio_res
      real(r8), dimension(N(ng)) :: conditions
# if defined DARWIN_INTERNAL_ASSERTSINK
      real(r8), dimension(IminS:ImaxS,N(ng)) :: Bio_ini
# endif
#endif

! instead of #include "set_bounds.h"
      integer :: Istr, Iend
      Istr   =BOUNDS(ng) % Istr   (tile)
      Iend   =BOUNDS(ng) % Iend   (tile)
!
!-----------------------------------------------------------------------
!  Vertical sinking terms.
!-----------------------------------------------------------------------
!
!  Reconstruct vertical profile of selected biological constituents
!  "Bio(:,:,isink)" in terms of a set of parabolic segments within each
!  grid box. Then, compute semi-Lagrangian flux due to sinking.
!
#if defined DARWIN_PLANK_BUOYCTRL
          SINK_LOOP: DO isink=1,Nsink
            ibio=idsink(isink)
            rate=Wbio(ibio,ng)
#if defined BIO_SEDIMENTVARIABLES
            ised=idsed(isink)
#endif
            IF ((ibio.ge.ic_.and.ibio.le.ec_.and.                       &
     &           buoyctrl(ibio-ic_+1,ng)).or.                           &
     &          (ibio.ge.iChl.and.ibio.le.eChl.and.                     &
     &           buoyctrl(ibio-iChl+1,ng))) THEN
              IF (ibio.ge.iChl.and.ibio.le.eChl) THEN
!  ip points to the phytoplankton variable associated with chlorophyll
!  variable.
                ip=ibio-iChl+1
              ELSE
                ip=ibio-ic_+1
              END IF
#if defined DARWIN_INTERNAL_ASSERTSINK
              DO k=1,N(ng)
                DO i=Istr,Iend
                  Bio_ini(i,k)=Bio(i,k,ibio)
                END DO
              END DO
#endif
              Bio_res(:,:)=0.0_r8
              DO i=Istr,Iend
#if ! defined DARWIN_SINKTOK
!  Determine conditions (higher values == better).
                IF (limitlight(i,N(ng),ip)==0.0_r8) THEN
!  Night-time.
                  DO k=1,N(ng)
                    conditions(k)=limitnut(i,k,ip)
                  END DO
                ELSE
                  DO k=1,N(ng)
                    conditions(k)=limitnut(i,k,ip)*limitlight(i,k,ip)
                  END DO
                END IF
#endif
                K0_LOOP: DO k0=1,N(ng)
!
!  Determine grid-cell dependent sinking/buoyancy. sink_k takes on the
!  role of a depth dependent sinking/buoyancy distance for layer k. It
!  is the sinking rate multiplied by dt. Positive values indicate
!  sinking.
!
#if defined DARWIN_SINKTOK
                  sink_k=100.0_r8*(k0-DARWIN_SINKTOK)*rate*dtbio(ng)
#else
                  cff=rate*dtbio(ng)
                  sink_k=0.0_r8
                  condopt=conditions(k0)
                  kopt=k0
                  IF ((k0==N(ng).and.                                   &
     &                 conditions(k0).gt.conditions(k0-1)).or.          &
     &                (k0==1.and.conditions(k0).gt.conditions(k0+1)).or.&
     &                (conditions(k0).gt.conditions(k0+1).and.          &
     &                 conditions(k0).gt.conditions(k0-1))) THEN
                    sink_k=0.0_r8
# if defined DARWIN_VERBOSE_SINKING
            IF ((ibio.ge.ic_.and.ibio.le.ec_).or.                       &
     &          (ibio.ge.iChl.and.ibio.le.eChl)) THEN
              IF (j==DARWIN_VERBOSE_J.and.i==DARWIN_VERBOSE_I) THEN
                write(*,'(a,9(a,f16.10))')                              &
     &            ' optimal conditions locally, staying put',           &
     &            ' light limit at surface: ', limitlight(i,N(ng),ip),  &
     &            ' limitlight(k0)=',limitlight(i,k0,ip),               &
     &            ' limitnut(k0)=',limitnut(i,k0,ip)
              END IF
            END IF
# endif /* DARWIN_VERBOSE_SINKING */
                  ELSE IF (k0.ne.1.and.                                 &
     &              conditions(k0-1).gt.conditions(k0+1)) THEN
# if defined DARWIN_VERBOSE_SINKING
            IF ((ibio.ge.ic_.and.ibio.le.ec_).or.                       &
     &          (ibio.ge.iChl.and.ibio.le.eChl)) THEN
              IF (j==DARWIN_VERBOSE_J.and.i==DARWIN_VERBOSE_I) THEN
                write(*,'(a,9(a,f16.10))')                              &
     &            'better conditions below (sink)          ',           &
     &            ' light limit at surface: ', limitlight(i,N(ng),ip),  &
     &            ' limitlight(k0)=',limitlight(i,k0,ip),               &
     &            ' limitnut(k0)=',limitnut(i,k0,ip),                   &
     &            ' z_w(k0)-z_w(k0-1)=',z_w(i,j,k0)-z_w(i,j,k0-1)
              END IF
            END IF
# endif /* DARWIN_VERBOSE_SINKING */
                    DO k=k0-1,1,-1
                      IF (conditions(k).gt.condopt) THEN
                        IF ((z_w(i,j,k0)-z_w(i,j,k)).le.cff) THEN
                          ! we can make it all the way
                          sink_k=z_w(i,j,k0)-z_w(i,j,k)
                          condopt=conditions(k)
                          kopt=k
                        ELSE
                          ! we cannot reach it but go as far as possible
                          sink_k=cff
                          kopt=-k
                          ! stop the search
                          EXIT
                        END IF
                      ELSE IF ((z_w(i,j,k0)-z_w(i,j,k)).gt.cff) THEN
                        ! stop the search
                        EXIT
                      END IF
                    END DO
                  ELSE
# if defined DARWIN_VERBOSE_SINKING
            IF ((ibio.ge.ic_.and.ibio.le.ec_).or.                       &
     &          (ibio.ge.iChl.and.ibio.le.eChl)) THEN
              IF (j==DARWIN_VERBOSE_J.and.i==DARWIN_VERBOSE_I) THEN
                write(*,'(a,9(a,f16.10))')                              &
     &            'better conditions above (float)         ',           &
     &            ' light limit at surface: ', limitlight(i,N(ng),ip),  &
     &            ' limitlight(k0)=',limitlight(i,k0,ip),               &
     &            ' limitnut(k0)=',limitnut(i,k0,ip),                   &
     &            ' z_w(k0)-z_w(k0+1)=',z_w(i,j,k0)-z_w(i,j,k0+1)
              END IF
            END IF
# endif /* DARWIN_VERBOSE_SINKING */
                    DO k=k0+1,N(ng)
!# if defined DARWIN_VERBOSE_SINKING
!            IF ((ibio.ge.ic_.and.ibio.le.ec_).or.                       &
!     &          (ibio.ge.iChl.and.ibio.le.eChl)) THEN
!              IF (j==DARWIN_VERBOSE_J.and.i==DARWIN_VERBOSE_I) THEN
!                write(*,'(a,i2,9(a,f16.10))') 'k=',k,                   &
!     &            'if (',z_w(i,j,k)-z_w(i,j,k0),' <= ',cff,' && ',      &
!     &            conditions(k),' > ',condopt,')'
!              END IF
!            END IF
!# endif /* DARWIN_VERBOSE_SINKING */
                      ! use .ge. here to allow getting out of zero light
                      ! conditions
                      IF (conditions(k).ge.condopt) THEN
                        IF ((z_w(i,j,k)-z_w(i,j,k0)).le.cff) THEN
                          ! we can make it all the way
                          sink_k=z_w(i,j,k0)-z_w(i,j,k)
                          condopt=conditions(k)
                          kopt=k
                        ELSE
                          ! we cannot reach it but go as far as possible
                          sink_k=-cff
                          kopt=-k
                          ! stop the search
                          EXIT
                        END IF
                      ELSE IF ((z_w(i,j,k)-z_w(i,j,k0)).gt.cff) THEN
                        ! stop the search
                        EXIT
                      END IF
                    END DO
                  END IF
!#else
!                  sink_k=rate*dtbio(ng)
#endif
                  IF (kopt.gt.0) THEN
!  No normal sinking, instead all plankton move from layer k0 to kopt.
                    IF (kopt.ne.k0) THEN
                      Bio_res(i,k0)=Bio_res(i,k0)-Bio(i,k0,ibio)
                      Bio_res(i,kopt)=Bio_res(i,kopt)+                  &
     &                  Bio(i,k0,ibio)*Hz(i,j,k0)*Hz_inv(i,kopt)
                    END IF
# if defined DARWIN_VERBOSE_SINKING
            IF ((ibio.ge.ic_.and.ibio.le.ec_).or.                       &
     &          (ibio.ge.iChl.and.ibio.le.eChl)) THEN
                  IF (j==DARWIN_VERBOSE_J.and.i==DARWIN_VERBOSE_I) THEN
                    qc(i,:)=0.0_r8
                    qc(i,k0)=Bio(i,k0,ibio)
                    write(*,'(a,i2,a,f16.10,9(a,i3))')                  &
     &                '(y) ibio=',ibio,' sink_k=',sink_k,               &
     &                ' ip=',ip,' k0=',k0,' kopt=',kopt
                    write(*,'(a4,1x,a16,9(2x,a16))')                    &
     &                'k','z_w','Bio (before)','Bio (after)',           &
     &                'sum so far','Biomass (bef)','Biomass (aft)',     &
     &                'conditions','limitnut','limitlight'
                    DO k=N(ng),1,-1
                      IF (k==kopt) THEN
                      write(*,'(i4,1x,f16.10,9(2x,f16.10))')            &
     &                  k,z_w(i,j,k),qc(i,k),                           &
     &                  Bio(i,k0,ibio)*Hz(i,j,k0)*Hz_inv(i,kopt),       &
     &                  Bio_res(i,k),                                   &
     &                  qc(i,k)*Hz(i,j,k),                              &
     &                  Bio(i,k0,ibio)*Hz(i,j,k0),                      &
     &                  conditions(k), limitnut(i,k,ip),                &
     &                  limitlight(i,k,ip)
                      ELSE
                      write(*,'(i4,1x,f16.10,9(2x,f16.10))')            &
     &                  k,z_w(i,j,k),qc(i,k),                           &
     &                  0.0_r8,                                         &
     &                  Bio_res(i,k),                                   &
     &                  qc(i,k)*Hz(i,j,k),                              &
     &                  0.0_r8,                                         &
     &                  conditions(k), limitnut(i,k,ip),                &
     &                  limitlight(i,k,ip)
                      END IF
                    END DO
                  END IF
                END IF
# endif /* DARWIN_VERBOSE_SINKING */
                    EXIT
                  END IF
!
!  Copy concentration of biological particulates into scratch array
!  "qc" (q-central, restrict it to be positive) which is hereafter
!  interpreted as a set of grid-box averaged values for biogeochemical
!  constituent concentration.
!
                  qc(i,:)=0.0_r8
                  qc(i,k0)=Bio(i,k0,ibio)
!
                  DO k=N(ng)-1,1,-1
                    FC(i,k)=(qc(i,k+1)-qc(i,k))*Hz_inv2(i,k)
                  END DO
                  DO k=2,N(ng)-1
                    dltR=Hz(i,j,k)*FC(i,k)
                    dltL=Hz(i,j,k)*FC(i,k-1)
                    cff=Hz(i,j,k-1)+2.0_r8*Hz(i,j,k)+Hz(i,j,k+1)
                    cffR=cff*FC(i,k)
                    cffL=cff*FC(i,k-1)
!
!  Apply PPM monotonicity constraint to prevent oscillations within the
!  grid box.
!
                    IF ((dltR*dltL).le.0.0_r8) THEN
                      dltR=0.0_r8
                      dltL=0.0_r8
                    ELSE IF (ABS(dltR).gt.ABS(cffL)) THEN
                      dltR=cffL
                    ELSE IF (ABS(dltL).gt.ABS(cffR)) THEN
                      dltL=cffR
                    END IF
!
!  Compute right and left side values (bR,bL) of parabolic segments
!  within grid box Hz(k); (WR,WL) are measures of quadratic variations.
!
!  NOTE: Although each parabolic segment is monotonic within its grid
!        box, monotonicity of the whole profile is not guaranteed,
!        because bL(k+1)-bR(k) may still have different sign than
!        qc(i,k+1)-qc(i,k).  This possibility is excluded,
!        after bL and bR are reconciled using WENO procedure.
!
                    cff=(dltR-dltL)*Hz_inv3(i,k)
                    dltR=dltR-cff*Hz(i,j,k+1)
                    dltL=dltL+cff*Hz(i,j,k-1)
                    bR(i,k)=qc(i,k)+dltR
                    bL(i,k)=qc(i,k)-dltL
                    WR(i,k)=(2.0_r8*dltR-dltL)**2
                    WL(i,k)=(dltR-2.0_r8*dltL)**2
                  END DO
                  cff=1.0E-14_r8
                  DO k=2,N(ng)-2
                    dltL=MAX(cff,WL(i,k  ))
                    dltR=MAX(cff,WR(i,k+1))
                    bR(i,k)=(dltR*bR(i,k)+dltL*bL(i,k+1))/(dltR+dltL)
                    bL(i,k+1)=bR(i,k)
                  END DO
                  FC(i,N(ng))=0.0_r8            ! NO-flux boundary condition
#if defined LINEAR_CONTINUATION
                  bL(i,N(ng))=bR(i,N(ng)-1)
                  bR(i,N(ng))=2.0_r8*qc(i,N(ng))-bL(i,N(ng))
#elif defined NEUMANN
                  bL(i,N(ng))=bR(i,N(ng)-1)
                  bR(i,N(ng))=1.5_r8*qc(i,N(ng))-0.5_r8*bL(i,N(ng))
#else
                  bR(i,N(ng))=qc(i,N(ng))       ! default strictly monotonic
                  bL(i,N(ng))=qc(i,N(ng))       ! conditions
                  bR(i,N(ng)-1)=qc(i,N(ng))
#endif
#if defined LINEAR_CONTINUATION
                  bR(i,1)=bL(i,2)
                  bL(i,1)=2.0_r8*qc(i,1)-bR(i,1)
#elif defined NEUMANN
                  bR(i,1)=bL(i,2)
                  bL(i,1)=1.5_r8*qc(i,1)-0.5_r8*bR(i,1)
#else
                  bL(i,2)=qc(i,1)               ! bottom grid boxes are
                  bR(i,1)=qc(i,1)               ! re-assumed to be
                  bL(i,1)=qc(i,1)               ! piecewise constant.
#endif
!
!  Apply monotonicity constraint again, since the reconciled interfacial
!  values may cause a non-monotonic behavior of the parabolic segments
!  inside the grid box.
!
                  DO k=1,N(ng)
                    dltR=bR(i,k)-qc(i,k)
                    dltL=qc(i,k)-bL(i,k)
                    cffR=2.0_r8*dltR
                    cffL=2.0_r8*dltL
                    IF ((dltR*dltL).lt.0.0_r8) THEN
                      dltR=0.0_r8
                      dltL=0.0_r8
                    ELSE IF (ABS(dltR).gt.ABS(cffL)) THEN
                      dltR=cffL
                    ELSE IF (ABS(dltL).gt.ABS(cffR)) THEN
                      dltL=cffR
                    END IF
                    bR(i,k)=qc(i,k)+dltR
                    bL(i,k)=qc(i,k)-dltL
                  END DO
!
!  After this moment reconstruction is considered complete. The next
!  stage is to compute vertical advective fluxes, FC. It is expected
!  that sinking may occurs relatively fast, the algorithm is designed
!  to be free of CFL criterion, which is achieved by allowing
!  integration bounds for semi-Lagrangian advective flux to use as
!  many grid boxes in upstream direction as necessary.
!
!  In the two code segments below, WL is the z-coordinate of the
!  departure point for grid box interface z_w with the same indices;
!  FC is the finite volume flux; ksource(:,k) is index of vertical
!  grid box which contains the departure point (restricted by N(ng)).
!  During the search: also add in content of whole grid boxes
!  participating in FC.
!
                  IF (sink_k.gt.0.0_r8) THEN
                    cff=ABS(sink_k)
                    DO k=1,N(ng)
                      FC(i,k-1)=0.0_r8
                      WL(i,k)=z_w(i,j,k-1)+cff
                      WR(i,k)=Hz(i,j,k)*qc(i,k)
                      ksource(i,k)=k
                    END DO
                    DO k=1,N(ng)
                      DO ks=k,N(ng)-1
                        IF (WL(i,k).gt.z_w(i,j,ks)) THEN
                          ksource(i,k)=ks+1
                          FC(i,k-1)=FC(i,k-1)+WR(i,ks)
                        END IF
                      END DO
                    END DO
!
!  Finalize computation of flux: add fractional part.
!
                    DO k=1,N(ng)
                      ks=ksource(i,k)
                      cu=MIN(1.0_r8,(WL(i,k)-z_w(i,j,ks-1))*Hz_inv(i,ks))
                      FC(i,k-1)=FC(i,k-1)+                              &
     &                          Hz(i,j,ks)*cu*                          &
     &                          (bL(i,ks)+                              &
     &                           cu*(0.5_r8*(bR(i,ks)-bL(i,ks))-        &
     &                               (1.5_r8-cu)*                       &
     &                               (bR(i,ks)+bL(i,ks)-                &
     &                                2.0_r8*qc(i,ks))))
                    END DO
                    DO k=1,N(ng)
                      Bio_res(i,k)=Bio_res(i,k)+                        &
     &                  qc(i,k)+(FC(i,k)-FC(i,k-1))*Hz_inv(i,k)
                    END DO
# if defined DARWIN_VERBOSE_SINKING
            IF ((ibio.ge.ic_.and.ibio.le.ec_).or.                       &
     &          (ibio.ge.iChl.and.ibio.le.eChl)) THEN
                  IF (j==DARWIN_VERBOSE_J.and.i==DARWIN_VERBOSE_I) THEN
                    write(*,'(a,i2,a,f16.10,9(a,i3))')                  &
     &                '(x) ibio=',ibio,' sink_k=',sink_k,               &
     &                ' ip=',ip,' k0=',k0,' kopt=',kopt
                    write(*,'(a4,1x,a16,9(2x,a16))')                    &
     &                'k','z_w','Bio (before)','Bio (after)',           &
     &                'sum so far','Biomass (bef)','Biomass (aft)',     &
     &                'conditions','limitnut','limitlight'
                    DO k=N(ng),1,-1
                      write(*,'(i4,1x,f16.10,9(2x,f16.10))')            &
     &                  k,z_w(i,j,k),qc(i,k),                           &
     &                  qc(i,k)+(FC(i,k)-FC(i,k-1))*Hz_inv(i,k),        &
     &                  Bio_res(i,k),                                   &
     &                  qc(i,k)*Hz(i,j,k),                              &
     &                  qc(i,k)*Hz(i,j,k)+(FC(i,k)-FC(i,k-1)),          &
     &                  conditions(k), limitnut(i,k,ip),                &
     &                  limitlight(i,k,ip)
                    END DO
                  END IF
                END IF
# endif /* DARWIN_VERBOSE_SINKING */
#if defined BIO_SEDIMENTVARIABLES
!
!  Particulate fluxes reaching the seafloor are stored in the sediment
!  variable. Without storage here, particulate material falls out of the
!  system.
!
                    cff1=FC(i,0)*Hz_inv(i,1)
                    bio_sediment(ng)%mass(i,j,ised)=                    &
     &                bio_sediment(ng)%mass(i,j,ised)+cff1
#endif
                  ELSE ! pos/neg sinking
!
!  Permit "negative sinking".
!
                    cff=ABS(sink_k)
                    FC(i,:)=0.0_r8
                    DO k=1,N(ng)-1
                      WL(i,k)=z_w(i,j,k)-cff
                    END DO
                    DO k=1,N(ng)
                      WR(i,k)=Hz(i,j,k)*qc(i,k)
                      ksource(i,k)=k
                    END DO
                    DO k=1,N(ng)
                      DO ks=k-1,1,-1
                        IF (WL(i,k).lt.z_w(i,j,ks)) THEN
                          ksource(i,k)=ks
                          FC(i,k+1)=FC(i,k+1)+WR(i,ks+1)
                        END IF
                      END DO
                    END DO
!
!  Finalize computation of flux: add fractional part.
!
#if defined DARWIN_VERBOSE_SINKING_EXTRAOUT
                    FCold(:,:)=0.0_r8
                    cus(:,:)=0.0_r8
                    DO k=1,N(ng)
                      FCold(i,k)=FC(i,k)
                    END DO
#endif
                    DO k=1,N(ng)-1
                      ks=ksource(i,k)
                      IF (ks.gt.0) THEN
                        cu=MIN(1.0_r8,(z_w(i,j,ks)-WL(i,k))*Hz_inv(i,ks))
#if defined DARWIN_VERBOSE_SINKING_EXTRAOUT
                        cus(i,k)=cu
#endif
                        FC(i,k+1)=FC(i,k+1)+                            &
     &                          Hz(i,j,ks)*cu*                          &
     &                          (bL(i,ks)+                              &
     &                           cu*(0.5_r8*(bR(i,ks)-bL(i,ks))-        &
     &                               (1.5_r8-cu)*                       &
     &                               (bR(i,ks)+bL(i,ks)-                &
     &                                2.0_r8*qc(i,ks))))
                      END IF
                    END DO
!
!  Do not allow tracers to escape through the surface.
!  Instead, accumulate material there, achieved by setting
!  FC(i,N(ng)+1)=0.0_r8.
!
                    FC(i,N(ng)+1)=0.0_r8
                    DO k=1,N(ng)
                      Bio_res(i,k)=Bio_res(i,k)+                        &
     &                  qc(i,k)+(FC(i,k)-FC(i,k+1))*Hz_inv(i,k)
                    END DO
# if defined DARWIN_VERBOSE_SINKING
            IF ((ibio.ge.ic_.and.ibio.le.ec_).or.                       &
     &          (ibio.ge.iChl.and.ibio.le.eChl)) THEN
                  IF (j==DARWIN_VERBOSE_J.and.i==DARWIN_VERBOSE_I) THEN
                    write(*,'(a,i2,a,f16.10,9(a,i3))')                  &
     &                '(x) ibio=',ibio,' sink_k=',sink_k,               &
     &                ' ip=',ip,' k0=',k0,' kopt=',kopt
                    write(*,'(a4,1x,a16,9(2x,a16))')                    &
     &                'k','z_w','Bio (before)','Bio (after)',           &
     &                'sum so far','Biomass (bef)','Biomass (aft)',     &
     &                'conditions','limitnut','limitlight'
                    DO k=N(ng),1,-1
                      write(*,'(i4,1x,f16.10,9(2x,f16.10))')            &
     &                  k,z_w(i,j,k),qc(i,k),                           &
     &                  qc(i,k)+(FC(i,k)-FC(i,k+1))*Hz_inv(i,k),        &
     &                  Bio_res(i,k),                                   &
     &                  qc(i,k)*Hz(i,j,k),                              &
     &                  qc(i,k)*Hz(i,j,k)+(FC(i,k)-FC(i,k+1)),          &
     &                  conditions(k), limitnut(i,k,ip),                &
     &                  limitlight(i,k,ip)
                    END DO
                  END IF
                END IF
# endif /* DARWIN_VERBOSE_SINKING */
                  END IF ! pos/neg sinking
                END DO K0_LOOP
              END DO ! i-loop
# if defined DARWIN_VERBOSE_SINKING
            IF ((ibio.ge.ic_.and.ibio.le.ec_).or.                       &
     &          (ibio.ge.iChl.and.ibio.le.eChl)) THEN
                  IF (j==DARWIN_VERBOSE_J.and.DARWIN_VERBOSE_I.ge.Istr.and.DARWIN_VERBOSE_I.le.Iend) THEN
                    i=DARWIN_VERBOSE_I
                    write(*,'(a,i2,a,f16.12)')                          &
     &                '(a) ibio=',ibio,' dt*Wbio=',rate*dtbio(ng)
                    write(*,'(a4,1x,a16,2x,a16)')                       &
     &                'k','Bio (before)','Bio (after)'
                    DO k=N(ng),1,-1
                      write(*,'(i4,1x,f16.12,2x,f16.12)')               &
     &                  k,Bio(i,k,ibio),Bio_res(i,k)
                    END DO
                  END IF
                END IF
# endif /* DARWIN_VERBOSE_SINKING */
# if defined DARWIN_VERBOSE_SINKING_EXTRAOUT
                    IF (j==DARWIN_VERBOSE_J.and.i==DARWIN_VERBOSE_I) THEN
                      write(*,'(a,f16.12)') 'dt*Wbio=',rate*dtbio(ng)
                      write(*,'(a,43f20.12)') 'z_w=',z_w(i,j,:)
                      write(*,'(a4,1x,a16,2x,a16,99(1x,a16))')          &
     &                  'k','Hz','qc','Bio','FC','Hz_inv','bL','bR',    &
     &                  'FC(old)','cu','Hz_inv2','Hz_inv3'
                      DO k=1,N(ng)
                      write(*,'(i4,1x,f16.12,2x,f16.12,99(1x,f16.12))') &
     &                  k,Hz(i,j,k),qc(i,k),Bio(i,k,ibio),FC(i,k),      &
     &                  Hz_inv(i,k),bL(i,k),bR(i,k),FCold(i,k),cus(i,k),&
     &                  Hz_inv2(i,k),Hz_inv3(i,k)
                      END DO
                    END IF
# endif /* DARWIN_VERBOSE_SINKING_EXTRAOUT */
              DO k=1,N(ng)
                DO i=Istr,Iend
                  Bio(i,k,ibio)=Bio_res(i,k)
                END DO
              END DO
# if defined DARWIN_INTERNAL_ASSERTSINK
              DO i=Istr,Iend
                mass0=0.0_r8
                mass1=0.0_r8
                DO k=1,N(ng)
                  mass0=mass0+Bio_ini(i,k)*Hz(i,j,k)
                  mass1=mass1+Bio(i,k,ibio)*Hz(i,j,k)
                END DO
                IF (rate.le.0.0_r8) THEN
                  IF (ABS(mass0-mass1).gt.1.0e-12_r8) THEN
                    write(*,'(2(a,i0),a,e)')                            &
     &                'Failed TODO sinking assertion at i=',i,          &
     &                ' j=',j,': difference=',mass1-mass0
                  END IF
                END IF
              END DO
# endif /* DARWIN_INTERNAL_ASSERTSINK */
            ELSE
!
!  Regular sinking code:
!
!
!  Copy concentration of biological particulates into scratch array
!  "qc" (q-central, restrict it to be positive) which is hereafter
!  interpreted as a set of grid-box averaged values for biogeochemical
!  constituent concentration.
!
              DO k=1,N(ng)
                DO i=Istr,Iend
                  qc(i,k)=Bio(i,k,ibio)
                END DO
              END DO
!
              DO k=N(ng)-1,1,-1
                DO i=Istr,Iend
                  FC(i,k)=(qc(i,k+1)-qc(i,k))*Hz_inv2(i,k)
                END DO
              END DO
              DO k=2,N(ng)-1
                DO i=Istr,Iend
                  dltR=Hz(i,j,k)*FC(i,k)
                  dltL=Hz(i,j,k)*FC(i,k-1)
                  cff=Hz(i,j,k-1)+2.0_r8*Hz(i,j,k)+Hz(i,j,k+1)
                  cffR=cff*FC(i,k)
                  cffL=cff*FC(i,k-1)
!
!  Apply PPM monotonicity constraint to prevent oscillations within the
!  grid box.
!
                  IF ((dltR*dltL).le.0.0_r8) THEN
                    dltR=0.0_r8
                    dltL=0.0_r8
                  ELSE IF (ABS(dltR).gt.ABS(cffL)) THEN
                    dltR=cffL
                  ELSE IF (ABS(dltL).gt.ABS(cffR)) THEN
                    dltL=cffR
                  END IF
!
!  Compute right and left side values (bR,bL) of parabolic segments
!  within grid box Hz(k); (WR,WL) are measures of quadratic variations.
!
!  NOTE: Although each parabolic segment is monotonic within its grid
!        box, monotonicity of the whole profile is not guaranteed,
!        because bL(k+1)-bR(k) may still have different sign than
!        qc(i,k+1)-qc(i,k).  This possibility is excluded,
!        after bL and bR are reconciled using WENO procedure.
!
                  cff=(dltR-dltL)*Hz_inv3(i,k)
                  dltR=dltR-cff*Hz(i,j,k+1)
                  dltL=dltL+cff*Hz(i,j,k-1)
                  bR(i,k)=qc(i,k)+dltR
                  bL(i,k)=qc(i,k)-dltL
                  WR(i,k)=(2.0_r8*dltR-dltL)**2
                  WL(i,k)=(dltR-2.0_r8*dltL)**2
                END DO
              END DO
              cff=1.0E-14_r8
              DO k=2,N(ng)-2
                DO i=Istr,Iend
                  dltL=MAX(cff,WL(i,k  ))
                  dltR=MAX(cff,WR(i,k+1))
                  bR(i,k)=(dltR*bR(i,k)+dltL*bL(i,k+1))/(dltR+dltL)
                  bL(i,k+1)=bR(i,k)
                END DO
              END DO
              DO i=Istr,Iend
                FC(i,N(ng))=0.0_r8            ! NO-flux boundary condition
#if defined LINEAR_CONTINUATION
                bL(i,N(ng))=bR(i,N(ng)-1)
                bR(i,N(ng))=2.0_r8*qc(i,N(ng))-bL(i,N(ng))
#elif defined NEUMANN
                bL(i,N(ng))=bR(i,N(ng)-1)
                bR(i,N(ng))=1.5_r8*qc(i,N(ng))-0.5_r8*bL(i,N(ng))
#else
                bR(i,N(ng))=qc(i,N(ng))       ! default strictly monotonic
                bL(i,N(ng))=qc(i,N(ng))       ! conditions
                bR(i,N(ng)-1)=qc(i,N(ng))
#endif
#if defined LINEAR_CONTINUATION
                bR(i,1)=bL(i,2)
                bL(i,1)=2.0_r8*qc(i,1)-bR(i,1)
#elif defined NEUMANN
                bR(i,1)=bL(i,2)
                bL(i,1)=1.5_r8*qc(i,1)-0.5_r8*bR(i,1)
#else
                bL(i,2)=qc(i,1)               ! bottom grid boxes are
                bR(i,1)=qc(i,1)               ! re-assumed to be
                bL(i,1)=qc(i,1)               ! piecewise constant.
#endif
              END DO
!
!  Apply monotonicity constraint again, since the reconciled interfacial
!  values may cause a non-monotonic behavior of the parabolic segments
!  inside the grid box.
!
              DO k=1,N(ng)
                DO i=Istr,Iend
                  dltR=bR(i,k)-qc(i,k)
                  dltL=qc(i,k)-bL(i,k)
                  cffR=2.0_r8*dltR
                  cffL=2.0_r8*dltL
                  IF ((dltR*dltL).lt.0.0_r8) THEN
                    dltR=0.0_r8
                    dltL=0.0_r8
                  ELSE IF (ABS(dltR).gt.ABS(cffL)) THEN
                    dltR=cffL
                  ELSE IF (ABS(dltL).gt.ABS(cffR)) THEN
                    dltL=cffR
                  END IF
                  bR(i,k)=qc(i,k)+dltR
                  bL(i,k)=qc(i,k)-dltL
                END DO
              END DO
!
!  After this moment reconstruction is considered complete. The next
!  stage is to compute vertical advective fluxes, FC. It is expected
!  that sinking may occurs relatively fast, the algorithm is designed
!  to be free of CFL criterion, which is achieved by allowing
!  integration bounds for semi-Lagrangian advective flux to use as
!  many grid boxes in upstream direction as necessary.
!
!  In the two code segments below, WL is the z-coordinate of the
!  departure point for grid box interface z_w with the same indices;
!  FC is the finite volume flux; ksource(:,k) is index of vertical
!  grid box which contains the departure point (restricted by N(ng)).
!  During the search: also add in content of whole grid boxes
!  participating in FC.
!
              DO i=Istr,Iend
                IF (rate.gt.0.0_r8) THEN
                  cff=ABS(rate)*dtbio(ng)
                  DO k=1,N(ng)
                    FC(i,k-1)=0.0_r8
                    WL(i,k)=z_w(i,j,k-1)+cff
                    WR(i,k)=Hz(i,j,k)*qc(i,k)
                    ksource(i,k)=k
                  END DO
                  DO k=1,N(ng)
                    DO ks=k,N(ng)-1
                      IF (WL(i,k).gt.z_w(i,j,ks)) THEN
                        ksource(i,k)=ks+1
                        FC(i,k-1)=FC(i,k-1)+WR(i,ks)
                      END IF
                    END DO
                  END DO
!
!  Finalize computation of flux: add fractional part.
!
                  DO k=1,N(ng)
                    ks=ksource(i,k)
                    cu=MIN(1.0_r8,(WL(i,k)-z_w(i,j,ks-1))*Hz_inv(i,ks))
                    FC(i,k-1)=FC(i,k-1)+                                &
     &                        Hz(i,j,ks)*cu*                            &
     &                        (bL(i,ks)+                                &
     &                         cu*(0.5_r8*(bR(i,ks)-bL(i,ks))-          &
     &                             (1.5_r8-cu)*                         &
     &                             (bR(i,ks)+bL(i,ks)-                  &
     &                              2.0_r8*qc(i,ks))))
                  END DO
                  DO k=1,N(ng)
                    Bio(i,k,ibio)=qc(i,k)+(FC(i,k)-FC(i,k-1))*Hz_inv(i,k)
                  END DO
# if defined DARWIN_VERBOSE_SINKING
            IF ((ibio.ge.ic_.and.ibio.le.ec_).or.                       &
     &          (ibio.ge.iChl.and.ibio.le.eChl)) THEN
                  IF (j==DARWIN_VERBOSE_J.and.i==DARWIN_VERBOSE_I) THEN
                    write(*,'(a,i2,a,f16.12)')                          &
     &                '(b) ibio=',ibio,' dt*Wbio=',rate*dtbio(ng)
                    write(*,'(a4,1x,a16,2x,a16)')                       &
     &                'k','Bio (before)','Bio (after)'
                    DO k=N(ng),1,-1
                      write(*,'(i4,1x,f16.12,2x,f16.12)')               &
     &                  k,qc(i,k),Bio(i,k,ibio)
                    END DO
                  END IF
            END IF
# endif /* DARWIN_VERBOSE_SINKING */
#if defined BIO_SEDIMENTVARIABLES
!
!  Particulate fluxes reaching the seafloor are stored in the sediment
!  variable. Without storage here, particulate material falls out of the
!  system.
!
                  ! TODO verify this is correct
                  cff1=FC(i,0)*Hz_inv(i,1)
                  bio_sediment(ng)%mass(i,j,ised)=                      &
     &              bio_sediment(ng)%mass(i,j,ised)+cff1
#endif
                ELSE
!
!  Permit "negative sinking".
!
                  cff=ABS(rate)*dtbio(ng)
                  FC(i,:)=0.0_r8
                  DO k=1,N(ng)-1
                    WL(i,k)=z_w(i,j,k)-cff
                  END DO
                  DO k=1,N(ng)
                    WR(i,k)=Hz(i,j,k)*qc(i,k)
                    ksource(i,k)=k
                  END DO
                  DO k=1,N(ng)
                    DO ks=k-1,1,-1
                      IF (WL(i,k).lt.z_w(i,j,ks)) THEN
                        ksource(i,k)=ks
                        FC(i,k+1)=FC(i,k+1)+WR(i,ks+1)
                      END IF
                    END DO
                  END DO
!
!  Finalize computation of flux: add fractional part.
!
#if defined DARWIN_VERBOSE_SINKING_EXTRAOUT
                  FCold(:,:)=0.0_r8
                  cus(:,:)=0.0_r8
                  DO k=1,N(ng)
                    FCold(i,k)=FC(i,k)
                  END DO
#endif
                  DO k=1,N(ng)-1
                    ks=ksource(i,k)
                    IF (ks.gt.0) THEN
                      cu=MIN(1.0_r8,(z_w(i,j,ks)-WL(i,k))*Hz_inv(i,ks))
#if defined DARWIN_VERBOSE_SINKING_EXTRAOUT
                      cus(i,k)=cu
#endif
                      FC(i,k+1)=FC(i,k+1)+                              &
     &                        Hz(i,j,ks)*cu*                            &
     &                        (bL(i,ks)+                                &
     &                         cu*(0.5_r8*(bR(i,ks)-bL(i,ks))-          &
     &                             (1.5_r8-cu)*                         &
     &                             (bR(i,ks)+bL(i,ks)-                  &
     &                              2.0_r8*qc(i,ks))))
                    END IF
                  END DO
!
!  Do not allow tracers to escape through the surface.
!  Instead, accumulate material there, achieved by setting
!  FC(i,N(ng)+1)=0.0_r8.
!
                  FC(i,N(ng)+1)=0.0_r8
                  DO k=1,N(ng)
                    Bio(i,k,ibio)=qc(i,k)+(FC(i,k)-FC(i,k+1))*Hz_inv(i,k)
                  END DO
# if defined DARWIN_VERBOSE_SINKING_EXTRAOUT
                  IF (j==DARWIN_VERBOSE_J.and.i==DARWIN_VERBOSE_I) THEN
                    write(*,'(a,f16.12)') 'dt*Wbio=',rate*dtbio(ng)
                    write(*,'(a,43f20.12)') 'z_w=',z_w(i,j,:)
                    write(*,'(a4,1x,a16,2x,a16,99(1x,a16))')            &
     &                'k','Hz','qc','Bio','FC','Hz_inv','bL','bR',      &
     &                'FC(old)','cu','Hz_inv2','Hz_inv3'
                    DO k=1,N(ng)
                      write(*,'(i4,1x,f16.12,2x,f16.12,99(1x,f16.12))') &
     &                  k,Hz(i,j,k),qc(i,k),Bio(i,k,ibio),FC(i,k),      &
     &                  Hz_inv(i,k),bL(i,k),bR(i,k),FCold(i,k),cus(i,k),&
     &                  Hz_inv2(i,k),Hz_inv3(i,k)
                    END DO
                  END IF
# endif /* DARWIN_VERBOSE_SINKING_EXTRAOUT */
# if defined DARWIN_VERBOSE_SINKING
            IF ((ibio.ge.ic_.and.ibio.le.ec_).or.                       &
     &          (ibio.ge.iChl.and.ibio.le.eChl)) THEN
                  IF (j==DARWIN_VERBOSE_J.and.i==DARWIN_VERBOSE_I) THEN
                    write(*,'(a,i2,a,f16.12)')                          &
     &                '(c) ibio=',ibio,' dt*Wbio=',rate*dtbio(ng)
                    write(*,'(a4,1x,a16,2x,a16)')                       &
     &                'k','Bio (before)','Bio (after)'
                    DO k=N(ng),1,-1
                      write(*,'(i4,1x,f16.12,2x,f16.12)')               &
     &                  k,qc(i,k),Bio(i,k,ibio)
                    END DO
                  END IF
            END IF
# endif /* DARWIN_VERBOSE_SINKING */
# if defined DARWIN_INTERNAL_ASSERTSINK
                  mass0=0.0_r8
                  mass1=0.0_r8
                  DO k=1,N(ng)
                    mass0=mass0+qc(i,k)*Hz(i,j,k)
                    mass1=mass1+Bio(i,k,ibio)*Hz(i,j,k)
                  END DO
                  IF (ABS(mass0-mass1).gt.1.0e-12_r8) THEN
                    write(*,'(2(a,i0),a,e)')                            &
     &                'Failed sinking assertion at i=',i,               &
     &                ' j=',j,': difference=',mass1-mass0
                  END IF
# endif /* DARWIN_INTERNAL_ASSERTSINK */
                END IF ! pos/neg sinking
              END DO ! i-loop
            END IF
          END DO SINK_LOOP
#else /* DARWIN_PLANK_BUOYCTRL */
          SINK_LOOP: DO isink=1,Nsink
            ibio=idsink(isink)
            rate=Wbio(ibio,ng)
#if defined BIO_SEDIMENTVARIABLES
            ised=idsed(isink)
#endif
!
!  Copy concentration of biological particulates into scratch array
!  "qc" (q-central, restrict it to be positive) which is hereafter
!  interpreted as a set of grid-box averaged values for biogeochemical
!  constituent concentration.
!
            DO k=1,N(ng)
              DO i=Istr,Iend
                qc(i,k)=Bio(i,k,ibio)
              END DO
            END DO
!
            DO k=N(ng)-1,1,-1
              DO i=Istr,Iend
                FC(i,k)=(qc(i,k+1)-qc(i,k))*Hz_inv2(i,k)
              END DO
            END DO
            DO k=2,N(ng)-1
              DO i=Istr,Iend
                dltR=Hz(i,j,k)*FC(i,k)
                dltL=Hz(i,j,k)*FC(i,k-1)
                cff=Hz(i,j,k-1)+2.0_r8*Hz(i,j,k)+Hz(i,j,k+1)
                cffR=cff*FC(i,k)
                cffL=cff*FC(i,k-1)
!
!  Apply PPM monotonicity constraint to prevent oscillations within the
!  grid box.
!
                IF ((dltR*dltL).le.0.0_r8) THEN
                  dltR=0.0_r8
                  dltL=0.0_r8
                ELSE IF (ABS(dltR).gt.ABS(cffL)) THEN
                  dltR=cffL
                ELSE IF (ABS(dltL).gt.ABS(cffR)) THEN
                  dltL=cffR
                END IF
!
!  Compute right and left side values (bR,bL) of parabolic segments
!  within grid box Hz(k); (WR,WL) are measures of quadratic variations.
!
!  NOTE: Although each parabolic segment is monotonic within its grid
!        box, monotonicity of the whole profile is not guaranteed,
!        because bL(k+1)-bR(k) may still have different sign than
!        qc(i,k+1)-qc(i,k).  This possibility is excluded,
!        after bL and bR are reconciled using WENO procedure.
!
                cff=(dltR-dltL)*Hz_inv3(i,k)
                dltR=dltR-cff*Hz(i,j,k+1)
                dltL=dltL+cff*Hz(i,j,k-1)
                bR(i,k)=qc(i,k)+dltR
                bL(i,k)=qc(i,k)-dltL
                WR(i,k)=(2.0_r8*dltR-dltL)**2
                WL(i,k)=(dltR-2.0_r8*dltL)**2
              END DO
            END DO
            cff=1.0E-14_r8
            DO k=2,N(ng)-2
              DO i=Istr,Iend
                dltL=MAX(cff,WL(i,k  ))
                dltR=MAX(cff,WR(i,k+1))
                bR(i,k)=(dltR*bR(i,k)+dltL*bL(i,k+1))/(dltR+dltL)
                bL(i,k+1)=bR(i,k)
              END DO
            END DO
            DO i=Istr,Iend
              FC(i,N(ng))=0.0_r8            ! NO-flux boundary condition
#if defined LINEAR_CONTINUATION
              bL(i,N(ng))=bR(i,N(ng)-1)
              bR(i,N(ng))=2.0_r8*qc(i,N(ng))-bL(i,N(ng))
#elif defined NEUMANN
              bL(i,N(ng))=bR(i,N(ng)-1)
              bR(i,N(ng))=1.5_r8*qc(i,N(ng))-0.5_r8*bL(i,N(ng))
#else
              bR(i,N(ng))=qc(i,N(ng))       ! default strictly monotonic
              bL(i,N(ng))=qc(i,N(ng))       ! conditions
              bR(i,N(ng)-1)=qc(i,N(ng))
#endif
#if defined LINEAR_CONTINUATION
              bR(i,1)=bL(i,2)
              bL(i,1)=2.0_r8*qc(i,1)-bR(i,1)
#elif defined NEUMANN
              bR(i,1)=bL(i,2)
              bL(i,1)=1.5_r8*qc(i,1)-0.5_r8*bR(i,1)
#else
              bL(i,2)=qc(i,1)               ! bottom grid boxes are
              bR(i,1)=qc(i,1)               ! re-assumed to be
              bL(i,1)=qc(i,1)               ! piecewise constant.
#endif
            END DO
!
!  Apply monotonicity constraint again, since the reconciled interfacial
!  values may cause a non-monotonic behavior of the parabolic segments
!  inside the grid box.
!
            DO k=1,N(ng)
              DO i=Istr,Iend
                dltR=bR(i,k)-qc(i,k)
                dltL=qc(i,k)-bL(i,k)
                cffR=2.0_r8*dltR
                cffL=2.0_r8*dltL
                IF ((dltR*dltL).lt.0.0_r8) THEN
                  dltR=0.0_r8
                  dltL=0.0_r8
                ELSE IF (ABS(dltR).gt.ABS(cffL)) THEN
                  dltR=cffL
                ELSE IF (ABS(dltL).gt.ABS(cffR)) THEN
                  dltL=cffR
                END IF
                bR(i,k)=qc(i,k)+dltR
                bL(i,k)=qc(i,k)-dltL
              END DO
            END DO
!
!  After this moment reconstruction is considered complete. The next
!  stage is to compute vertical advective fluxes, FC. It is expected
!  that sinking may occurs relatively fast, the algorithm is designed
!  to be free of CFL criterion, which is achieved by allowing
!  integration bounds for semi-Lagrangian advective flux to use as
!  many grid boxes in upstream direction as necessary.
!
!  In the two code segments below, WL is the z-coordinate of the
!  departure point for grid box interface z_w with the same indices;
!  FC is the finite volume flux; ksource(:,k) is index of vertical
!  grid box which contains the departure point (restricted by N(ng)).
!  During the search: also add in content of whole grid boxes
!  participating in FC.
!
!TODO use of dt: make sure sinking units are correct
            IF (rate.gt.0.0_r8) THEN
              cff=ABS(rate)*dtbio(ng)
              DO k=1,N(ng)
                DO i=Istr,Iend
                  FC(i,k-1)=0.0_r8
                  WL(i,k)=z_w(i,j,k-1)+cff
                  WR(i,k)=Hz(i,j,k)*qc(i,k)
                  ksource(i,k)=k
                END DO
              END DO
              DO k=1,N(ng)
                DO ks=k,N(ng)-1
                  DO i=Istr,Iend
                    IF (WL(i,k).gt.z_w(i,j,ks)) THEN
                      ksource(i,k)=ks+1
                      FC(i,k-1)=FC(i,k-1)+WR(i,ks)
                    END IF
                  END DO
                END DO
              END DO
!
!  Finalize computation of flux: add fractional part.
!
              DO k=1,N(ng)
                DO i=Istr,Iend
                  ks=ksource(i,k)
                  cu=MIN(1.0_r8,(WL(i,k)-z_w(i,j,ks-1))*Hz_inv(i,ks))
                  FC(i,k-1)=FC(i,k-1)+                                  &
     &                      Hz(i,j,ks)*cu*                              &
     &                      (bL(i,ks)+                                  &
     &                       cu*(0.5_r8*(bR(i,ks)-bL(i,ks))-            &
     &                           (1.5_r8-cu)*                           &
     &                           (bR(i,ks)+bL(i,ks)-                    &
     &                            2.0_r8*qc(i,ks))))
                END DO
              END DO
              DO k=1,N(ng)
                DO i=Istr,Iend
                  Bio(i,k,ibio)=qc(i,k)+(FC(i,k)-FC(i,k-1))*Hz_inv(i,k)
                END DO
              END DO
# if defined DARWIN_VERBOSE_SINKING
            IF ((ibio.ge.ic_.and.ibio.le.ec_).or.                       &
     &          (ibio.ge.iChl.and.ibio.le.eChl)) THEN
              IF (j==DARWIN_VERBOSE_J.and.DARWIN_VERBOSE_I.ge.Istr.and.DARWIN_VERBOSE_I.le.Iend) THEN
                i=DARWIN_VERBOSE_I
                write(*,'(a,i2,a,f16.12)')                              &
     &            '(d) ibio=',ibio,' dt*Wbio=',rate*dtbio(ng)
                write(*,'(a4,1x,a16,5(2x,a16))')                        &
     &            'k','Bio (before)','Bio (after)','gain','loss'
                DO k=N(ng),1,-1
                  write(*,'(i4,1x,f16.12,9(2x,f16.12))')                &
     &              k,qc(i,k),Bio(i,k,ibio),FC(i,k)*Hz_inv(i,k),        &
     &              -FC(i,k-1)*Hz_inv(i,k)
                END DO
              END IF
            END IF
# endif /* DARWIN_VERBOSE_SINKING */
#if defined BIO_SEDIMENTVARIABLES
!
!  Particulate fluxes reaching the seafloor are stored in the sediment
!  variable. Without storage here, particulate material falls out of the
!  system.
!
              DO i=Istr,Iend
                ! TODO verify this is correct
                cff1=FC(i,0)*Hz_inv(i,1)
                bio_sediment(ng)%mass(i,j,ised)=                        &
     &            bio_sediment(ng)%mass(i,j,ised)+cff1
              END DO
#endif
            ELSE
!
!  Permit "negative sinking".
!
              cff=ABS(rate)*dtbio(ng)
              FC(:,:)=0.0_r8
              DO k=1,N(ng)-1
                DO i=Istr,Iend
                  WL(i,k)=z_w(i,j,k)-cff
                END DO
              END DO
              DO k=1,N(ng)
                DO i=Istr,Iend
                  WR(i,k)=Hz(i,j,k)*qc(i,k)
                  ksource(i,k)=k
                END DO
              END DO
              !DO i=Istr,Iend
              !  WL(i,N(ng))=0.0_r8
              !  ksource(i,N(ng))=N(ng)-1
              !END DO
              DO k=1,N(ng)
                DO ks=k-1,1,-1
                  DO i=Istr,Iend
                    IF (WL(i,k).lt.z_w(i,j,ks)) THEN
                      ksource(i,k)=ks
                      FC(i,k+1)=FC(i,k+1)+WR(i,ks+1)
                    END IF
                  END DO
                END DO
              END DO
!
!  Finalize computation of flux: add fractional part.
!
#if defined DARWIN_VERBOSE_SINKING_EXTRAOUT
              FCold(:,:)=0.0_r8
              cus(:,:)=0.0_r8
              DO k=1,N(ng)
                DO i=Istr,Iend
                  FCold(i,k)=FC(i,k)
                END DO
              END DO
#endif
              DO k=1,N(ng)-1
                DO i=Istr,Iend
                  ks=ksource(i,k)
                  IF (ks.gt.0) THEN
                    cu=MIN(1.0_r8,(z_w(i,j,ks)-WL(i,k))*Hz_inv(i,ks))
#if defined DARWIN_VERBOSE_SINKING_EXTRAOUT
                    cus(i,k)=cu
#endif
                    FC(i,k+1)=FC(i,k+1)+                                &
     &                      Hz(i,j,ks)*cu*                              &
     &                      (bL(i,ks)+                                  &
     &                       cu*(0.5_r8*(bR(i,ks)-bL(i,ks))-            &
     &                           (1.5_r8-cu)*                           &
     &                           (bR(i,ks)+bL(i,ks)-                    &
     &                            2.0_r8*qc(i,ks))))
                  END IF
                END DO
              END DO
!
!  Do not allow tracers to escape through the surface.
!  Instead, accumulate material there, achieved by setting
!  FC(i,N(ng)+1)=0.0_r8.
!
              DO i=Istr,Iend
                FC(i,N(ng)+1)=0.0_r8
              END DO
              DO k=1,N(ng)
                DO i=Istr,Iend
                  Bio(i,k,ibio)=qc(i,k)+(FC(i,k)-FC(i,k+1))*Hz_inv(i,k)
                END DO
              END DO
# if defined DARWIN_VERBOSE_SINKING
            IF ((ibio.ge.ic_.and.ibio.le.ec_).or.                       &
     &          (ibio.ge.iChl.and.ibio.le.eChl)) THEN
              IF (j==DARWIN_VERBOSE_J.and.DARWIN_VERBOSE_I.ge.Istr.and.DARWIN_VERBOSE_I.le.Iend) THEN
                i=DARWIN_VERBOSE_I
                write(*,'(a,i2,a,f16.12)')                              &
     &            '(e) ibio=',ibio,' dt*Wbio=',rate*dtbio(ng)
                write(*,'(a4,1x,a16,2x,a16)')                           &
     &            'k','Bio (before)','Bio (after)'
                DO k=N(ng),1,-1
                  write(*,'(i4,1x,f16.12,2x,f16.12)')                   &
     &              k,qc(i,k),Bio(i,k,ibio)
                END DO
              END IF
            END IF
# endif /* DARWIN_VERBOSE_SINKING */
# if defined DARWIN_VERBOSE_SINKING_EXTRAOUT
              IF (j==DARWIN_VERBOSE_J.and.DARWIN_VERBOSE_I.ge.Istr.and.DARWIN_VERBOSE_I.le.Iend) THEN
                i=DARWIN_VERBOSE_I
                write(*,'(a,f16.12)') 'dt*Wbio=',rate*dtbio(ng)
                write(*,'(a,43f20.12)') 'z_w=',z_w(i,j,:)
                write(*,'(a4,1x,a16,2x,a16,99(1x,a16))')                &
     &           'k','Hz','qc','Bio','FC','Hz_inv','bL','bR','FC(old)', &
     &           'cu','Hz_inv2','Hz_inv3'
                DO k=1,N(ng)
                  write(*,'(i4,1x,f16.12,2x,f16.12,99(1x,f16.12))')     &
     &              k,Hz(i,j,k),qc(i,k),Bio(i,k,ibio),FC(i,k),          &
     &              Hz_inv(i,k),bL(i,k),bR(i,k),FCold(i,k),cus(i,k),    &
     &              Hz_inv2(i,k),Hz_inv3(i,k)
                END DO
              END IF
# endif /* DARWIN_VERBOSE_SINKING_EXTRAOUT */
# if defined DARWIN_INTERNAL_ASSERTSINK
              DO i=Istr,Iend
                mass0=0.0_r8
                mass1=0.0_r8
                DO k=1,N(ng)
                  mass0=mass0+qc(i,k)*Hz(i,j,k)
                  mass1=mass1+Bio(i,k,ibio)*Hz(i,j,k)
                END DO
                IF (ABS(mass0-mass1).gt.1.0e-12_r8) THEN
                  write(*,'(2(a,i0),a,e)')                              &
     &              'Failed sinking assertion at i=',i,                 &
     &              ' j=',j,': difference=',mass1-mass0
                END IF
              END DO
# endif /* DARWIN_INTERNAL_ASSERTSINK */
            ENDIF
          END DO SINK_LOOP
#endif /* DARWIN_PLANK_BUOYCTRL */
      RETURN
      END SUBROUTINE sinking
#endif
!
!
!
#endif
      END MODULE mod_darwin_sms
