      SUBROUTINE biology (ng,tile)
!***********************************************************************
!
      USE mod_param
      USE mod_forces
      USE mod_grid
      USE mod_ncparam
      USE mod_ocean
      USE mod_stepping
#if defined DIAGNOSTICS_BIO
      USE mod_diags
#endif
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
!
!  Local variable declarations.
!
#include "tile.h"
!
!  Set header file name.
!
#if defined DISTRIBUTE
      IF (Lbiofile(iNLM)) THEN
#else /* DISTRIBUTE */
      IF (Lbiofile(iNLM).and.(tile.eq.0)) THEN
#endif /* DISTRIBUTE */
        Lbiofile(iNLM)=.FALSE.
        BIONAME(iNLM)=__FILE__
      END IF
!
#if defined PROFILE
      CALL wclock_on (ng, iNLM, 15)
#endif /* PROFILE */
      CALL biology_tile (ng, tile,                                      &
     &                   LBi, UBi, LBj, UBj, N(ng), NT(ng),             &
     &                   IminS, ImaxS, JminS, JmaxS,                    &
     &                   nstp(ng), nnew(ng),                            &
#if defined MASKING
     &                   GRID(ng) % rmask,                              &
#endif /* MASKING */
     &                   GRID(ng) % h,                                  &
     &                   GRID(ng) % Hz,                                 &
     &                   GRID(ng) % z_r,                                &
     &                   GRID(ng) % z_w,                                &
     &                   FORCES(ng) % srflx,                            &
#if defined BULK_FLUXES
     &                   FORCES(ng) % Uwind,                            &
     &                   FORCES(ng) % Vwind,                            &
#else /* BULK_FLUXES */
     &                   FORCES(ng) % sustr,                            &
     &                   FORCES(ng) % svstr,                            &
#endif /* BULK_FLUXES */
#if defined DARWIN_CARBON_EXTRAOUT
     &                   OCEAN(ng) % pH,                                &
     &                   OCEAN(ng) % CO2flx,                            &
     &                   OCEAN(ng) % pCO2,                              &
#endif
#if defined DARWIN_CONSTRUCTION
     &                   FORCES(ng) % SpecIr,                           &
     &                   FORCES(ng) % avcos,                            &
#endif
#if defined DIAGNOSTICS_BIO
!     &                   DIAGS(ng) % DiaBio2d,                          &
     &                   DIAGS(ng) % DiaBio3d,                          &
#endif
     &                   OCEAN(ng) % t)

#if defined PROFILE
      CALL wclock_off (ng, iNLM, 15)
#endif /* PROFILE */
      RETURN
      END SUBROUTINE biology
!
!-----------------------------------------------------------------------
      SUBROUTINE biology_tile (ng, tile,                                &
     &                         LBi, UBi, LBj, UBj, UBk, UBt,            &
     &                         IminS, ImaxS, JminS, JmaxS,              &
     &                         nstp, nnew,                              &
#if defined MASKING
     &                         rmask,                                   &
#endif /* MASKING */
     &                         h,                                       &
     &                         Hz, z_r, z_w,                            &
     &                         srflx,                                   &
#if defined BULK_FLUXES
     &                         Uwind, Vwind,                            &
#else /* BULK_FLUXES */
     &                         sustr, svstr,                            &
#endif /* BULK_FLUXES */
#if defined DARWIN_CARBON_EXTRAOUT
     &                         pH, CO2flx, pCO2,                        &
#endif
#if defined DARWIN_CONSTRUCTION
     &                         SpecIr, avcos,                           &
#endif
#if defined DIAGNOSTICS_BIO
!     &                         DiaBio2d,                                &
     &                         DiaBio3d,                                &
#endif
     &                         t)
!-----------------------------------------------------------------------
!
      USE mod_biology
      USE mod_scalars, ONLY: itemp,isalt,rho0,Cp
      USE mod_darwin_sms
#if defined DARWIN_VERBOSE
      USE mod_parallel, only: Master
#endif
#if defined DIAGNOSTICS_BIO_MAPPING
      USE mod_ncparam, only: Dout
#endif
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj, UBk, UBt
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
      integer, intent(in) :: nstp, nnew

#if defined ASSUMED_SHAPE
# if defined MASKING
      real(r8), intent(in) :: rmask(LBi:,LBj:)
# endif /* MASKING */
      real(r8), intent(in) :: h(LBi:,LBj:)
      real(r8), intent(in) :: Hz(LBi:,LBj:,:)
      real(r8), intent(in) :: z_r(LBi:,LBj:,:)
      real(r8), intent(in) :: z_w(LBi:,LBj:,0:)
      real(r8), intent(in) :: srflx(LBi:,LBj:)
# if defined BULK_FLUXES
      real(r8), intent(in) :: Uwind(LBi:,LBj:)
      real(r8), intent(in) :: Vwind(LBi:,LBj:)
# else /* BULK_FLUXES */
      real(r8), intent(in) :: sustr(LBi:,LBj:)
      real(r8), intent(in) :: svstr(LBi:,LBj:)
# endif /* BULK_FLUXES */
# if defined DARWIN_CARBON_EXTRAOUT
      real(r8), intent(inout) :: pH(LBi:,LBj:,:)
      real(r8), intent(inout) :: CO2flx(LBi:,LBj:)
      real(r8), intent(inout) :: pCO2(LBi:,LBj:,:)
# endif
# if defined DARWIN_CONSTRUCTION
      real(r8), intent(in) :: SpecIr(LBi:,LBj:,:)
      real(r8), intent(in) :: avcos(LBi:,LBj:,:)
# endif
# ifdef DIAGNOSTICS_BIO
!      real(r8), intent(inout) :: DiaBio2d(LBi:,LBj:,:)
      real(r8), intent(inout) :: DiaBio3d(LBi:,LBj:,:,:)
# endif
      real(r8), intent(inout) :: t(LBi:,LBj:,:,:,:)
#else /* ASSUMED_SHAPE */
# if defined MASKING
      real(r8), intent(in) :: rmask(LBi:UBi,LBj:UBj)
# endif /* MASKING */
      real(r8), intent(in) :: h(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: Hz(LBi:UBi,LBj:UBj,UBk)
      real(r8), intent(in) :: z_r(LBi:UBi,LBj:UBj,UBk)
      real(r8), intent(in) :: z_w(LBi:UBi,LBj:UBj,0:UBk)
      real(r8), intent(in) :: srflx(LBi:UBi,LBj:UBj)
# if defined BULK_FLUXES
      real(r8), intent(in) :: Uwind(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: Vwind(LBi:UBi,LBj:UBj)
# else /* BULK_FLUXES */
      real(r8), intent(in) :: sustr(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: svstr(LBi:UBi,LBj:UBj)
# endif /* BULK_FLUXES */
# if defined DARWIN_CARBON_EXTRAOUT
      real(r8), intent(inout) :: pH(LBi:UBi,LBj:UBj,UBk)
      real(r8), intent(inout) :: CO2flx(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: pCO2(LBi:UBi,LBj:UBj,UBk)
# endif
# if defined DARWIN_CONSTRUCTION
      real(r8), intent(in) :: SpecIr(LBi:UBi,LBj:UBj,nlam)
      real(r8), intent(in) :: avcos(LBi:UBi,LBj:UBj,nlam)
# endif
# ifdef DIAGNOSTICS_BIO
!      real(r8), intent(inout) :: DiaBio2d(LBi:UBi,LBj:UBj,NDbio2d)
      real(r8), intent(inout) :: DiaBio3d(LBi:UBi,LBj:UBj,UBk,NDbio3d)
# endif
      real(r8), intent(inout) :: t(LBi:UBi,LBj:UBj,UBk,3,UBt)
#endif /* ASSUMED_SHAPE */
!
!  Local variable declarations.
!
      integer :: Iter, i, ibio, isink, itime, itrc, iTrcMax, j, k, ks
#if defined DIAGNOSTICS_BIO_MAPPING
      integer :: idia
#endif
      
      real(r8), parameter :: MinVal = 1.0e-6_r8
      
      real(r8) :: cff
      real(r8), dimension(NT(ng),2) :: BioTrc
      real(r8), dimension(IminS:ImaxS,N(ng),NT(ng)) :: Bio
      real(r8), dimension(IminS:ImaxS,N(ng),NT(ng)) :: Bio_old
      real(r8), dimension(IminS:ImaxS,N(ng)) :: Hz_inv
      real(r8), dimension(IminS:ImaxS,N(ng)) :: Hz_inv2
      real(r8), dimension(IminS:ImaxS,N(ng)) :: Hz_inv3
      real(r8), dimension(IminS:ImaxS,N(ng),nlam) :: PAR
      real(r8), dimension(IminS:ImaxS) :: PARsur
      real(r8), dimension(IminS:ImaxS,N(ng)) :: drF

      real(r8), dimension(IminS:ImaxS,N(ng),darwin_nDiag) :: diags
      real(r8), dimension(IminS:ImaxS,N(ng),nplank) :: photoTempFunc
      real(r8), dimension(IminS:ImaxS,N(ng),nplank) :: grazTempFunc
      real(r8), dimension(IminS:ImaxS,N(ng)) :: reminTempFunc,          &
     &  mortTempFunc, mort2TempFunc, uptakeTempFunc
      real(r8), dimension(IminS:ImaxS,N(ng)) :: freefe
#if defined DARWIN_PLANK_BUOYCTRL
      real(r8), dimension(IminS:ImaxS,N(ng),nplank) :: limitnut_save
      real(r8), dimension(IminS:ImaxS,N(ng),nplank) :: limitlight_save
#endif
      
      !TODO read this somewhere
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: inputFe
#include "set_bounds.h"
!
!  Compute inverse thickness to avoid repeated divisions.
!
      J_LOOP : DO j=Jstr,Jend
        DO k=1,N(ng)
          DO i=Istr,Iend
            Hz_inv(i,k)=1.0_r8/Hz(i,j,k)
          END DO
        END DO
        DO k=1,N(ng)-1
          DO i=Istr,Iend
            Hz_inv2(i,k)=1.0_r8/(Hz(i,j,k)+Hz(i,j,k+1))
          END DO
        END DO
        DO k=2,N(ng)-1
          DO i=Istr,Iend
            Hz_inv3(i,k)=1.0_r8/(Hz(i,j,k-1)+Hz(i,j,k)+Hz(i,j,k+1))
          END DO
        END DO
!
!  Set diags to zero.
!
        diags(:,:,:)=0.0_r8
!
! initialize drF variable
!
        DO k=1,N(ng)
          DO i=Istr,Iend
            drF(i,k) = z_w(i,j,k)-z_w(i,j,k-1)
          END DO
        END DO
!
!  Extract biological variables from tracer arrays, place them into
!  scratch arrays, and restrict their values to be positive definite.
!  At input, all tracers (index nnew) from predictor step have
!  transport units (m Tunits) since we do not have yet the new
!  values for zeta and Hz. These are known after the 2D barotropic
!  time-stepping.
!
        DO itrc=1,NBT
          ibio=idbio(itrc)
          DO k=1,N(ng)
            DO i=Istr,Iend
              Bio_old(i,k,ibio)=MAX(0.0_r8,t(i,j,k,nstp,ibio))
              Bio(i,k,ibio)=Bio_old(i,k,ibio)
            END DO
          END DO
        END DO
!
!  Extract potential temperature and salinity.
!
        DO k=1,N(ng)
          DO i=Istr,Iend
            Bio(i,k,itemp)=t(i,j,k,nstp,itemp)
            Bio(i,k,isalt)=t(i,j,k,nstp,isalt)
          END DO
        END DO
#if defined DARWIN_VERBOSE_NUT || defined DARWIN_VERBOSE_PLANK
        IF (j==DARWIN_VERBOSE_J.and.DARWIN_VERBOSE_I.ge.Istr.and.DARWIN_VERBOSE_I.le.Iend) THEN
          i=DARWIN_VERBOSE_I
          k=DARWIN_VERBOSE_K
          write(*,'(2a,3(i0,a))') 'DARWIN_VERBOSE',                     &
     &      ' reporting values at (',i,',',j,',',k,')'
# if defined DARWIN_VERBOSE_NUT
          write(*,'(a20,f18.12)') 'initial DIC = ',Bio(i,k,iDIC)
          write(*,'(a20,f18.12)') 'initial NH4 = ',Bio(i,k,iNH4)
          write(*,'(a20,f18.12)') 'initial NO2 = ',Bio(i,k,iNO2)
          write(*,'(a20,f18.12)') 'initial NO3 = ',Bio(i,k,iNO3)
# endif
        END IF
#endif
!
!  Calculate surface Photosynthetically Available Radiation (PAR).  The
!  net shortwave radiation is scaled back to Watts/m2 and multiplied by
!  the fraction that is photosynthetically available, PARfrac.
!
        DO i=Istr,Iend
#if defined LET_THERE_BE_LIGHT
          PARsur(i)=100.0_r8
#else /* LET_THERE_BE_LIGHT */
          PARsur(i)=PARfrac(ng)*srflx(i,j)*rho0*Cp
#endif /* LET_THERE_BE_LIGHT */
        END DO
#if defined DARWIN_INPUTFE
!
!  
!
        DO i=Istr,Iend
          inputFe(i,j)=DARWIN_INPUTFE
        END DO
#else
        DO i=Istr,Iend
          inputFe(:,j)=0.0_r8
        END DO
#endif
        freefe(:,:)=0.0_r8

#if defined DARWIN_VERBOSE_LIGHT
        IF (j==DARWIN_VERBOSE_J.and.DARWIN_VERBOSE_I.ge.Istr.and.DARWIN_VERBOSE_I.le.Iend) THEN
          i=DARWIN_VERBOSE_I
          write(*,'(3x,a,i0,a,i0,a)') 'light at (',i,',',j,')' 
          DO k=N(ng),1,-1
            write(*,'(3x,a,f9.2,a,99f11.7)')                            &
     &        'depth:',0.5_r8*(z_w(i,j,k)+z_w(i,j,k-1)),                &
     &        ' PAR: ',PAR(i,k,:)
          END DO
        END IF
#endif
!
!  light
!
        CALL light(tile, ng, j, IminS, ImaxS, drF, PAR, PARsur, Bio)
#if defined DARWIN_VERBOSE_LIGHT
        IF (j==DARWIN_VERBOSE_J.and.DARWIN_VERBOSE_I.ge.Istr.and.DARWIN_VERBOSE_I.le.Iend) THEN
          i=DARWIN_VERBOSE_I
          write(*,'(3x,a,i0,a,i0,a)') 'light at (',i,',',j,')' 
          DO k=N(ng),1,-1
            write(*,'(3x,a,f9.2,a,99f11.7)')                            &
     &        'depth:',0.5_r8*(z_w(i,j,k)+z_w(i,j,k-1)),                &
     &        ' PAR: ',PAR(i,k,:)
          END DO
        END IF
#endif
#if defined DARWIN_CONSTRUCTION && defined DARWIN_VERBOSE_CONSTRUCTION
        IF (j==DARWIN_VERBOSE_J.and.DARWIN_VERBOSE_I.ge.Istr.and.DARWIN_VERBOSE_I.le.Iend) THEN
          i=DARWIN_VERBOSE_I
          write(*,'(3x,a,i0,a,i0,a)') 'avcos at (',i,',',j,')' 
          DO ibio=1,nlam
            write(*,'(3x,a,i3,a,99f11.7)')                              &
     &        'l:',ibio,                                                &
     &        ' avcos: ',avcos(i,j,ibio)
          END DO
        END IF
#endif
!
!  DIC
!
        CALL surfforcing(tile, ng, j, IminS, ImaxS, Bio,                &
     &    LBi, LBj,                                                     &
#if ! defined ASSUMED_SHAPE
     &    UBi, UBj,                                                     &
#endif /* ! ASSUMED_SHAPE */
#if defined BULK_FLUXES
     &    Uwind, Vwind,                                                 &
#else /* BULK_FLUXES */
     &    sustr, svstr,                                                 &
#endif /* BULK_FLUXES */
#if defined DARWIN_CARBON_EXTRAOUT
!     &    pH(:,j,:), pCO2(:,j,:), CO2flx(:,j),                          &
     &    pH, pCO2, CO2flx,                                             &
#endif /* DARWIN_CARBON_EXTRAOUT */
     &    drF, diags)
!
!  iron
!
        CALL darwin_fe_chem(tile, ng, IminS, ImaxS, Bio,                &
     &    inputFe(:,j), freefe,                                         &
     &    diags)
        CALL darwin_fe_chem2(tile, ng, IminS, ImaxS, Bio,               &
     &    inputFe(:,j), freefe,                                         &
#if defined DARWIN_VERBOSE_IFET
     &  j,                                                              &
#endif
     &    drF, diags)
!
!  plankton
!
        CALL tempfunc(tile, ng, IminS, ImaxS, Bio,                      &
     &    photoTempFunc, grazTempFunc, reminTempFunc, mortTempFunc,     &
     &    mort2TempFunc, uptakeTempFunc, diags)
        CALL darwin_model(tile, ng, IminS, ImaxS, Bio, PAR,             &
#if defined DARWIN_VERBOSE_PLANK_OLD || defined DARWIN_VERBOSE_PLANK
     &    j,                                                            &
#endif
#if defined DARWIN_PLANK_BUOYCTRL
     &    limitnut_save, limitlight_save,                               &
#endif
     &    photoTempFunc, reminTempFunc, uptakeTempFunc, diags)
        CALL darwin_grazing(tile, ng, IminS, ImaxS, Bio,                &
     &    grazTempFunc, reminTempFunc, mortTempFunc, mort2TempFunc,     &
#if defined DARWIN_VERBOSE_PLANK_OLD || defined DARWIN_VERBOSE_PLANK
     &    j,                                                            &
#endif
     &    diags)
!
!  sinking
!
#if defined DARWIN_ROMSSINKING
        CALL sinking(tile, ng, j, IminS, ImaxS, LBi, LBj,               &
#if ! defined ASSUMED_SHAPE
     &    UBi, UBj, UBk,                                                &
#endif
#if defined DARWIN_PLANK_BUOYCTRL
     &    limitnut_save, limitlight_save,                               &
#endif
     &    Hz, Hz_inv, Hz_inv2, Hz_inv3, z_w, Bio)
#else
# error "No other sinking schemes implemented, use DARWIN_ROMSSINKING"
#endif
!
!  iron
!  re-apply free iron limit to FeT
!
        CALL darwin_fe_chem(tile, ng, IminS, ImaxS, Bio,                &
     &    inputFe(:,j), freefe,                                         &
     &    diags)

#if defined DARWIN_TOTAL_CHLOROPHYLL
!
!  sum up all chlorophyll
!
        DO k=1,N(ng)
          DO i=Istr,Iend
            Bio(i,k,iTChl)=0.0_r8
          END DO
        END DO
        DO ibio=iChl,eChl
          DO k=1,N(ng)
            DO i=Istr,Iend
              Bio(i,k,iTChl)=Bio(i,k,iTChl)+Bio(i,k,ibio) 
            END DO
          END DO
        END DO
#endif
#if defined DIAGNOSTICS_BIO
!
!-----------------------------------------------------------------------
! Copy information from diags into ROMS diagnostic arrays.
!-----------------------------------------------------------------------
!
!      IF (((iic(ng).gt.ntsDIA(ng)).and.                                 &
!     &     (MOD(iic(ng),nDIA(ng)).eq.1)).or.                            &
!     &    ((iic(ng).ge.ntsDIA(ng)).and.(nDIA(ng).eq.1)).or.             &
!     &    ((nrrec(ng).gt.0).and.(iic(ng).eq.ntstart(ng)))) THEN
# if defined DIAGNOSTICS_BIO_MAPPING
        DO itrc=1,NDb3d_o
          IF (Dout(iDbio3(itrc),ng)) THEN
            idia=iDbiomapping(itrc,ng) 
            DO k=1,N(ng)
              DO i=Istr,Iend
                DiaBio3d(i,j,k,itrc)=DiaBio3d(i,j,k,itrc)+diags(i,k,idia)
              END DO
            END DO
          END IF
        END DO
# else
        DO itrc=1,darwin_nDiag
          DO k=1,N(ng)
            DO i=Istr,Iend
              DiaBio3d(i,j,k,itrc)=DiaBio3d(i,j,k,itrc)+diags(i,k,itrc)
            END DO
          END DO
        END DO
# endif
!      END IF
#endif
#if defined DARWIN_VERBOSE_NUT || defined DARWIN_VERBOSE_PLANK
        IF (j==DARWIN_VERBOSE_J.and.DARWIN_VERBOSE_I.ge.Istr.and.DARWIN_VERBOSE_I.le.Iend) THEN
          i=DARWIN_VERBOSE_I
          k=DARWIN_VERBOSE_K
# if defined DARWIN_VERBOSE_NUT
          write(*,'(a20,f18.12)') 'final DIC = ',Bio(i,k,iDIC)
          write(*,'(a20,f18.12)') 'final NH4 = ',Bio(i,k,iNH4)
          write(*,'(a20,f18.12)') 'final NO2 = ',Bio(i,k,iNO2)
          write(*,'(a20,f18.12)') 'final NO3 = ',Bio(i,k,iNO3)
# endif
        END IF
#endif
!
!-----------------------------------------------------------------------
!  Update global tracer variables: Add increment due to BGC processes
!  to tracer array in time index "nnew". Index "nnew" is solution after
!  advection and mixing and has transport units (m Tunits) hence the
!  increment is multiplied by Hz.  Notice that we need to subtract
!  original values "Bio_old" at the top of the routine to just account
!  for the concentractions affected by BGC processes. This also takes
!  into account any constraints (non-negative concentrations, carbon
!  concentration range) specified before entering BGC kernel. If "Bio"
!  were unchanged by BGC processes, the increment would be exactly
!  zero. Notice that final tracer values, t(:,:,:,nnew,:) are not
!  bounded >=0 so that we can preserve total inventory of nutrients
!  when advection causes tracer concentration to go negative.
!-----------------------------------------------------------------------
!
        DO itrc=1,NBT
          ibio=idbio(itrc)
          DO k=1,N(ng)
            DO i=Istr,Iend
              cff=Bio(i,k,ibio)-Bio_old(i,k,ibio)
              t(i,j,k,nnew,ibio)=t(i,j,k,nnew,ibio)+cff*Hz(i,j,k)
            END DO
          END DO
        END DO

      END DO J_LOOP

      RETURN
      END SUBROUTINE biology_tile
