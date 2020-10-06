#include "cppdefs.h"
      MODULE mod_biosedimentvars
#if defined BIO_SEDIMENTVARIABLES
!
!=======================================================================
!                                                                      !
!=======================================================================
!
        USE mod_kinds

        implicit none

        TYPE T_BIOSED
!
!  Sediment mass of sinking tracers.
!
          real(r8), pointer :: mass(:,:,:)

        END TYPE T_BIOSED

        TYPE (T_BIOSED), allocatable :: bio_sediment(:)

      CONTAINS

      SUBROUTINE allocate_bio_sediment (ng, LBi, UBi, LBj, UBj)
!
!=======================================================================
!                                                                      !
!  This routine allocates all variables in the module for all nested   !
!  grids.                                                              !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_biology,    ONLY: Nsed
!
!  Local variable declarations.
!
      integer, intent(in) :: ng, LBi, UBi, LBj, UBj
!
!-----------------------------------------------------------------------
!  Allocate module variables.
!-----------------------------------------------------------------------
!
      IF (ng.eq.1) allocate ( bio_sediment(Ngrids) )
!
!  Sediment mass of sinking tracers.
!
      allocate ( bio_sediment(ng) % mass(LBi:UBi,LBj:UBj,Nsed) )

      RETURN
      END SUBROUTINE allocate_bio_sediment

      SUBROUTINE initialize_bio_sediment (ng, tile)
!
!=======================================================================
!                                                                      !
!  This routine initialize all variables in the module using first     !
!  touch distribution policy. In shared-memory configuration, this     !
!  operation actually performs propagation of the  "shared arrays"     !
!  across the cluster, unless another policy is specified to           !
!  override the default.                                               !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_biology,    ONLY: Nsed
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
!
!  Local variable declarations.
!
      integer :: Imin, Imax, Jmin, Jmax
      integer :: i, j, isink

      real(r8), parameter :: IniVal = 0.0_r8

#include "set_bounds.h"
!
!  Set array initialization range.
!
#ifdef DISTRIBUTE
      Imin=BOUNDS(ng)%LBi(tile)
      Imax=BOUNDS(ng)%UBi(tile)
      Jmin=BOUNDS(ng)%LBj(tile)
      Jmax=BOUNDS(ng)%UBj(tile)
#else /* DISTRIBUTE */
      IF (DOMAIN(ng)%Western_Edge(tile)) THEN
        Imin=BOUNDS(ng)%LBi(tile)
      ELSE
        Imin=Istr
      END IF
      IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
        Imax=BOUNDS(ng)%UBi(tile)
      ELSE
        Imax=Iend
      END IF
      IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
        Jmin=BOUNDS(ng)%LBj(tile)
      ELSE
        Jmin=Jstr
      END IF
      IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
        Jmax=BOUNDS(ng)%UBj(tile)
      ELSE
        Jmax=Jend
      END IF
#endif /* DISTRIBUTE */
!
!-----------------------------------------------------------------------
!  Initialize module variables.
!-----------------------------------------------------------------------
!
!  Sediment mass of sinking tracers.
!
      DO j=Jmin,Jmax
        DO i=Imin,Imax
          DO isink=1,Nsed
            bio_sediment(ng) % mass(i,j,isink) = IniVal
          END DO
        END DO
      END DO

      RETURN
      END SUBROUTINE initialize_bio_sediment
#endif /* BIO_SEDIMENTVARIABLES */
      END MODULE mod_biosedimentvars
