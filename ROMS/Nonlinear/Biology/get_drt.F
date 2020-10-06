#include "cppdefs.h"
#if defined BIOLOGY && defined DARWIN_RADTRANS_IO 
      SUBROUTINE get_drt (ng, model)
!
!=======================================================================
!                                                                      !
!  This subroutine reads in observations data when appropriate from    !
!  observations input NetCDF file.  The observations data is stored    !
!  for use elsewhere.                                                  !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_fourdvar
      USE mod_iounits
      USE mod_ncparam
      USE mod_netcdf
      USE mod_scalars
      USE mod_biology
!
      USE dateclock_mod, ONLY : time_string
      USE strings_mod,   ONLY : FoundError
!
      implicit none
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, model
!
!  Local variable declarations.
!
      logical :: readNLmod, readTLmod

      integer :: i, iobs, itrc, status

      character (len=22) :: t_code
!
      SourceFile=__FILE__

!
!  Open file.
!
      CALL netcdf_open (ng, model, DRT(ng)%name, 1, DRT(ng)%ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &    __FILE__)) THEN
        WRITE (stdout,'(3a)') 'Error opening "',TRIM(DRT(ng)%name),'".'
        RETURN
      END IF
!
!---------------------------------------------------------------------
!  Read variables needed in the Darwin radiative transfer (DRT) 
!  model.
!---------------------------------------------------------------------
!
# include <get_drt.h>
!
!  Close file.
!
        CALL netcdf_close(ng, iNLM, DRT(ng)%ncid, DRT(ng)%name, .FALSE.)
        IF (FoundError(exit_flag, NoError, __LINE__,                    &
     &    __FILE__)) THEN
          WRITE (stdout,'(3a)') 'Error closing "',                      &
     &      TRIM(DRT(ng)%name),'".'
          RETURN
        END IF

      RETURN
      END SUBROUTINE get_drt
#else
      SUBROUTINE get_drt
      RETURN
      END SUBROUTINE get_drt
#endif
