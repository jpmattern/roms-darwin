      SUBROUTINE positive (ng,tile)
!
!svn $Id: positive.h 523 2011-01-05 03:21:38Z arango $
!************************************************** Hernan G. Arango ***
!  Copyright (c) 2002-2011 The ROMS/TOMS Group       Jerome Fiechter   !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!***********************************************************************
!                                                                      ! 
!  Hajoon : subroutine that applies positive definite only             !
!                                                                      !
!***********************************************************************
!
      USE mod_param
      USE mod_forces
      USE mod_grid
      USE mod_ncparam
      USE mod_ocean
      USE mod_stepping
#ifdef DIAGNOSTICS_BIO_DEACTIVATED
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
#ifdef DISTRIBUTE
      IF (Lbiofile(iNLM)) THEN
#else
      IF (Lbiofile(iNLM).and.(tile.eq.0)) THEN
#endif
        Lbiofile(iNLM)=.FALSE.
        BIONAME(iNLM)=__FILE__
      END IF
!
#ifdef PROFILE
      CALL wclock_on (ng, iNLM, 15)
#endif
      CALL positive_tile (ng, tile,                                     &
     &                   LBi, UBi, LBj, UBj, N(ng), NT(ng),             &
     &                   IminS, ImaxS, JminS, JmaxS,                    &
     &                   nstp(ng), nnew(ng),                            &
     &                   GRID(ng) % Hz,                                 &
#ifdef DIAGNOSTICS_BIO_DEACTIVATED
     &                   DIAGS(ng) % DiaBio2d,                          &
     &                   DIAGS(ng) % DiaBio3d,                          &
#endif
     &                   OCEAN(ng) % t)

#ifdef PROFILE
      CALL wclock_off (ng, iNLM, 15)
#endif
      RETURN
      END SUBROUTINE positive 
!
!-----------------------------------------------------------------------
      SUBROUTINE positive_tile (ng, tile,                               &
     &                         LBi, UBi, LBj, UBj, UBk, UBt,            &
     &                         IminS, ImaxS, JminS, JmaxS,              &
     &                         nstp, nnew,                              &
     &                         Hz,                                      &
#ifdef DIAGNOSTICS_BIO_DEACTIVATED
     &                         DiaBio2d, DiaBio3d,                      &
#endif
     &                         t)
!-----------------------------------------------------------------------
!
      USE mod_param
      USE mod_biology 
      USE mod_ncparam
      USE mod_scalars

!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj, UBk, UBt
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
      integer, intent(in) :: nstp, nnew

#ifdef ASSUMED_SHAPE
      real(r8), intent(in) :: Hz(LBi:,LBj:,:)
# ifdef DIAGNOSTICS_BIO_DEACTIVATED
      real(r8), intent(inout) :: DiaBio2d(LBi:,LBj:,:)
      real(r8), intent(inout) :: DiaBio3d(LBi:,LBj:,:,:)
# endif
      real(r8), intent(inout) :: t(LBi:,LBj:,:,:,:)
#else
      real(r8), intent(in) :: Hz(LBi:UBi,LBj:UBj,UBk)
# ifdef DIAGNOSTICS_BIO_DEACTIVATED
      real(r8), intent(inout) :: DiaBio2d(LBi:UBi,LBj:UBj,NDbio2d)
      real(r8), intent(inout) :: DiaBio3d(LBi:UBi,LBj:UBj,UBk,NDbio3d)
# endif
      real(r8), intent(inout) :: t(LBi:UBi,LBj:UBj,UBk,3,UBt)
#endif
!
!  Local variable declarations.
!
      integer :: i, ibio, itrc, j, k

      real(r8), parameter :: MinVal = 1.0e-10_r8

#ifdef DIAGNOSTICS_BIO_DEACTIVATED
      real(r8) :: cff1

#endif
      real(r8), dimension(NT(ng),2) :: BioTrc

      real(r8), dimension(IminS:ImaxS,N(ng)) :: Hz_inv
#ifdef DIAGNOSTICS_BIO_DEACTIVATED
      integer :: ivar
      real(r8), dimension(IminS:ImaxS,N(ng),NT(ng)) :: DiaTmp
#endif

!#include "set_bounds.h"
      integer :: Istr,Iend,Jstr,Jend
      Istr   =BOUNDS(ng) % Istr   (tile)
      Iend   =BOUNDS(ng) % Iend   (tile)
      Jstr   =BOUNDS(ng) % Jstr   (tile)
      Jend   =BOUNDS(ng) % Jend   (tile)
!
!-----------------------------------------------------------------------
!  Add biological Source/Sink terms.
!-----------------------------------------------------------------------
!
!  Compute inverse thickness to avoid repeated divisions.
!
      J_LOOP : DO j=Jstr,Jend
        DO k=1,N(ng)
          DO i=Istr,Iend
            Hz_inv(i,k)=1.0_r8/Hz(i,j,k)
          END DO
        END DO
!
!  Restrict biological tracer to be positive definite. If a negative
!  concentration is detected, nitrogen is drawn from the most abundant
!  pool to supplement the negative pools to a lower limit of MinVal
!  which is set to 1E-6 above.
!
        DO k=1,N(ng)
          DO i=Istr,Iend
!
!  At input, all tracers (index nnew) from predictor step have
!  transport units (m Tunits) since we do not have yet the new
!  values for zeta and Hz. These are known after the 2D barotropic
!  time-stepping.
!
#ifdef DIAGNOSTICS_BIO_DEACTIVATED
            cff1=0.0_r8
#endif
            DO itrc=1,NBT
              ibio=idbio(itrc)
              BioTrc(ibio,nstp)=t(i,j,k,nstp,ibio)
              BioTrc(ibio,nnew)=t(i,j,k,nnew,ibio)*Hz_inv(i,k)
#ifdef DIAGNOSTICS_BIO_DEACTIVATED
              DiaTmp(i,k,ibio)=BioTrc(ibio,nnew)
#endif
!
!  Impose positive definite concentrations.
!
#ifdef DIAGNOSTICS_BIO_DEACTIVATED
              cff1=cff1+MAX(0.0_r8,MinVal-BioTrc(ibio,nnew))
#endif
              BioTrc(ibio,nstp)=MAX(MinVal,BioTrc(ibio,nstp))
              BioTrc(ibio,nnew)=MAX(MinVal,BioTrc(ibio,nnew))
!
!-----------------------------------------------------------------------
!  Update global tracer variables (m Tunits).
!-----------------------------------------------------------------------
!
#if defined DARWIN_VERBOSE_POSITIVE
              IF (j==DARWIN_VERBOSE_J.and.i==DARWIN_VERBOSE_I) THEN
                write(*,'(a,i0,a,i0,a)') 'applying pos to ibio=',ibio,  &
     &            ' (itrc=',itrc,')'
              END IF
#endif /* DARWIN_VERBOSE_POSITIVE */
              t(i,j,k,nnew,ibio)=BioTrc(ibio,nnew)*Hz(i,j,k)
              ! this is new:
              !t(i,j,k,nstp,ibio)=BioTrc(ibio,nstp)
#if defined DIAGNOSTICS_BIO_DEACTIVATED 
              ivar=0
# ifdef DIAGNOSTICS_NEM_PHY
              IF (ibio.eq.iSphy) THEN
                ivar=iFudgePS
              ELSE IF (ibio.eq.iLphy) THEN
                ivar=iFudgePL
              END IF
# endif
# ifdef DIAGNOSTICS_NEM_ZOO
              IF (ibio.eq.iSzoo) THEN
                ivar=iFudgeZS
              ELSE IF (ibio.eq.iLzoo) THEN
                ivar=iFudgeZL
              ELSE IF (ibio.eq.iPzoo) THEN
                ivar=iFudgeZP
              END IF
# endif
# ifdef DIAGNOSTICS_NEM_NIT
              IF (ibio.eq.iNO3_) THEN
                ivar=iFudgeNO3
              ELSE IF (ibio.eq.iNH4_) THEN
                ivar=iFudgeNH4
              ELSE IF (ibio.eq.iPON_) THEN
                ivar=iFudgePON
              ELSE IF (ibio.eq.iDON_) THEN
                ivar=iFudgeDON
              END IF
# endif
# ifdef DIAGNOSTICS_NEM_SIL
              IF (ibio.eq.iSiOH) THEN
                ivar=iFudgeSiOH
              ELSE IF (ibio.eq.iopal) THEN
                ivar=iFudgeOpal
              END IF
# endif
              IF (ivar.ne.0) THEN
                DiaBio3d(i,j,k,ivar)=DiaBio3d(i,j,k,ivar)+              &
     &                   (BioTrc(ibio,nnew)-DiaTmp(i,k,ibio))
              END IF
#endif
            END DO
          END DO
        END DO

      END DO J_LOOP

      RETURN
      END SUBROUTINE positive_tile
