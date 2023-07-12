      MODULE mod_biology
!
!=======================================================================
!                                                                      !
!  Parameters for Darwin 3 ecosystem model                             !
!                                                                      !
!=======================================================================
!
      USE mod_param
#if defined NOROMS
      !integer, parameter :: r8 = selected_real_kind(12,300)
      USE mod_kinds
#endif

!
      implicit none
      ! TODO move elsewhere?
#if defined DARWIN_DIMS_FIXED
      integer, parameter :: nplank=2
      integer, parameter :: nGroup=2
      integer, parameter :: nlam=1
      integer, parameter :: nopt=1
      integer, parameter :: nPhoto=1
      integer, parameter :: nPPplank=0
      integer, parameter :: nGRplank=0

      integer, parameter :: iMinBact=0
      integer, parameter :: iMaxBact=0
      integer, parameter :: iMinPrey=1
      integer, parameter :: iMaxPrey=1
      integer, parameter :: iMinPred=2
      integer, parameter :: iMaxPred=2
#else
      integer, parameter :: nplank=DARWIN_DIM_NPLANK
      integer, parameter :: nGroup=DARWIN_DIM_NGROUP
      integer, parameter :: nlam=DARWIN_DIM_NLAM
      integer, parameter :: nopt=DARWIN_DIM_NOPT
      integer, parameter :: nPhoto=DARWIN_DIM_NPHOTO
      integer, parameter :: nPPplank=DARWIN_DIM_NPPPLANK
      integer, parameter :: nGRplank=DARWIN_DIM_NGRPLANK

      integer, parameter :: iMinBact=DARWIN_INDEX_MINBACT
      integer, parameter :: iMaxBact=DARWIN_INDEX_MAXBACT
      integer, parameter :: iMinPrey=DARWIN_INDEX_MINPREY
      integer, parameter :: iMaxPrey=DARWIN_INDEX_MAXPREY
      integer, parameter :: iMinPred=DARWIN_INDEX_MINPRED
      integer, parameter :: iMaxPred=DARWIN_INDEX_MAXPRED
#endif
!
!-----------------------------------------------------------------------
!  Set biological tracer identification indices.
!-----------------------------------------------------------------------
!
      integer, allocatable :: ndtbio(:) ! number of time-steps
      real(r8), allocatable :: dtbio(:) ! biology time-step in s
      integer, allocatable :: idbio(:)  ! Biological tracers
#if defined DARWIN_ROMSSINKING
      integer, allocatable :: idsink(:)
      real(r8), allocatable :: Wbio(:,:)
      integer :: Nsink
#endif
#if defined BIO_SEDIMENTVARIABLES
      integer :: Nsed
      integer, allocatable :: idsed(:)
#endif
      ! TODO move elsewhere?
! taken from GUD_SIZE.h
      !integer :: nplank
      !integer :: nGroup, nlam, nopt
      !integer :: nPhoto
      !integer :: nPPplank
      !integer :: nGRplank
      integer :: nChl
      !integer :: iMinBact, iMaxBact
      !integer :: iMinPrey, iMaxPrey
      !integer :: iMinPred, iMaxPred
! taken from GUD_INDICES.h
      integer :: nNQuota, nPQuota, nSiQuota, nFeQuota
      integer :: iDIC
      integer :: iNH4
      integer :: iNO2
      integer :: iNO3
      integer :: iPO4
      integer :: iSiO2
      integer :: iFeT
      integer :: iDOC
      integer :: iDON
      integer :: iDOP
      integer :: iDOFe
      integer :: iPOC
      integer :: iPON
      integer :: iPOP
      integer :: iPOSi
      integer :: iPOFe
      integer :: iPIC
#if defined DARWIN_CARBON
      integer :: iALK
      integer :: iO2
#endif
#if defined DARWIN_CDOM
      integer :: iCDOM
#endif
#if defined DARWIN_TOTAL_CHLOROPHYLL
      integer :: iTChl
#endif
      ! added underscore to "ic", "in", "ip"
      integer :: ic_,ec_
      integer :: in_,en_
      integer :: ip_,ep_
      integer :: isi,esi
      integer :: ife,efe
      integer :: iChl,eChl
      integer :: eCARBON
      !integer :: eCDOM ! defined but not used in current version of code
!#if defined DARWIN_DIAGNOSTICS
! taken from GUD_DIAGS.h
      integer :: iPP
      integer :: iNfix
      integer :: iDenit
      integer :: iDenitN
      integer :: iPPplank
      integer :: iGRplank
      integer :: iConsDIN
      integer :: iConsPO4
      integer :: iConsSi
      integer :: iConsFe
      integer :: darwin_nDiag
#if defined DARWIN_DEBUG_DIAG
      integer :: iOnes
#endif
!#endif
#if ! defined DARWIN_CDOM
      integer :: laCDOM
#endif
#if defined BIO_SEDIMENTVARIABLES
      integer :: isedc, isedn, isedp, isedsi, isedfe, isedchl
#endif
#if defined DIAGNOSTICS_BIO
!
!-----------------------------------------------------------------------
!  Set biological diagnostic identification indices.
!-----------------------------------------------------------------------
!

!
!  Biological 3D diagnostic variable IDs.
!
      integer, allocatable :: iDbio3(:)       ! 3D biological terms

!
!  Biological 4D diagnostic variable IDs.
!
      integer, allocatable :: iDbio4(:)       ! 4D terms with nplank
!
      integer  :: idGrazPr = 1                ! grazing pressure
# if defined DIAGNOSTICS_BIO_MAPPING
!
!  Biological 3D diagnostic variable mapping.
!
      integer, allocatable :: iDbiomapping(:,:)

# endif /*DIAGNOSTICS_BIO_MAPPING*/
#endif /*DIAGNOSTICS_BIO*/

!
!-----------------------------------------------------------------------
!  Darwin names used for standard output.
!-----------------------------------------------------------------------
!
      character (len=16), dimension(nplank) :: plankname
      character (len=26), dimension(nGroup) :: grp_names
#if defined BIO_SEDIMENTVARIABLES
      character (len=20), dimension(7+4) :: sedname
#endif
!
!  Biological parameters.
!
#include <darwin_mod_sub1.h>
!
      CONTAINS
!
      SUBROUTINE initialize_biology
!
!=======================================================================
!                                                                      !
!  This routine sets several variables needed by the biology model.    !
!  It allocates and assigns biological tracers indices.                !
!                                                                      !
!=======================================================================
!
!  Local variable declarations
!
      integer :: i, ic, idia
!
!-----------------------------------------------------------------------
!  Initialize tracer identification indices.
!-----------------------------------------------------------------------
!

      nChl=nPhoto

#ifdef DARWIN_NQUOTA
      nNQuota=nplank
#else
      nNQuota=0
#endif
#ifdef DARWIN_PQUOTA
      nPQuota=nplank
#else
      nPQuota=0
#endif
#ifdef DARWIN_SIQUOTA
      nSiQuota=nplank
#else
      nSiQuota=0
#endif
#ifdef DARWIN_FEQUOTA
      nFeQuota=nplank
#else
      nFeQuota=0
#endif
      ic=NAT+NPT+NCS+NNS
      ic=ic+1
      iDIC=ic
      ic=ic+1
      iNH4=ic
      ic=ic+1
      iNO2=ic
      ic=ic+1
      iNO3=ic
      ic=ic+1
      iPO4=ic
      ic=ic+1
      iSiO2=ic
      ic=ic+1
      iFeT=ic
      ic=ic+1
      iDOC=ic
      ic=ic+1
      iDON=ic
      ic=ic+1
      iDOP=ic
      ic=ic+1
      iDOFe=ic
      ic=ic+1
      iPOC=ic
      ic=ic+1
      iPON=ic
      ic=ic+1
      iPOP=ic
      ic=ic+1
      iPOSi=ic
      ic=ic+1
      iPOFe=ic
      ic=ic+1
      iPIC=ic
      ic=ic+1
#if defined DARWIN_TOTAL_CHLOROPHYLL
      iTChl=ic
      ic=ic+1
#endif
#ifdef DARWIN_CARBON
      iALK=ic
      ic=ic+1
      iO2=ic
      ic=ic+1
#endif
      eCARBON=ic-1
#ifdef DARWIN_CDOM
      iCDOM=ic
      ic=ic+1
#endif

      ic_=ic
      ic=ic+nplank
      ec_=ic-1

      in_=ic
      ic=ic+nNQuota
      en_=ic-1

      ip_=ic
      ic=ic+nPQuota
      ep_=ic-1

      ife=ic
      ic=ic+nFeQuota
      efe=ic-1

      isi=ic
      ic=ic+nSiQuota
      esi=ic-1

      iChl=ic
      ic=ic+nChl
      eChl=ic-1

!#if defined DARWIN_DIAGNOSTICS
! for diagnotics
! taken from GUD_DIAGS.h
      idia=1

      iPP=idia
      idia=idia+1
      iNfix=idia
      idia=idia+1
      iDenit=idia
      idia=idia+1
      iDenitN=idia
      idia=idia+1
      iConsPO4=idia
      idia=idia+1
      iConsSi=idia
      idia=idia+1
      iConsFe=idia
      idia=idia+1
      iConsDIN=idia
      idia=idia+1
#if defined DARWIN_DEBUG_DIAG
      iOnes=idia
      idia=idia+1
#endif
      iPPplank=idia
      idia=idia+nPPplank
      iGRplank=idia
      darwin_nDiag=iGRplank+nGRplank-1
!#endif
#if defined DIAGNOSTICS_BIO
!
!-----------------------------------------------------------------------
!  Set sources and sinks biology number of diagnostic terms.
!-----------------------------------------------------------------------
!
      NDbio3d=0  ! TODO at some point set to darwin_nDiag
      NDbio4d=1
!
!  Allocate biological diagnostics vectors
!
      IF (.not.allocated(iDbio3).and.NDbio3d.gt.0) THEN
        allocate ( iDbio3(NDbio3d) )
      END IF
      IF (.not.allocated(iDbio4)) THEN
        allocate ( iDbio4(NDbio4d) )
      END IF
#endif /*DIAGNOSTICS_BIO*/
#if defined DIAGNOSTICS_BIO_DEACTIVATED
!
!-----------------------------------------------------------------------
!  Interface ROMS and Darwin diagnostic terms.
!-----------------------------------------------------------------------
!
!  Set number of diagnostics terms.
!
      NDbio3d=darwin_nDiag
      NDbio2d=0
!
!  Allocate biological diagnostics vectors
!
      IF (.not.allocated(iDbio2)) THEN
        allocate ( iDbio2(NDbio2d) )
      END IF
      IF (.not.allocated(iDbio3)) THEN
        allocate ( iDbio3(NDbio3d) )
      END IF
# if defined DIAGNOSTICS_BIO_MAPPING
      IF (.not.allocated(iDbiomapping)) THEN
        allocate ( iDbiomapping(NDbio3d,Ngrids) )
      END IF
!      IF (.not.allocated(NDb3d_o)) THEN
!        allocate ( NDb3d_o(Ngrids) )
!      END IF
# endif /*DIAGNOSTICS_BIO_MAPPING*/
#endif /*DIAGNOSTICS_BIO_DEACTIVATED*/
#if ! defined DARWIN_CDOM
      laCDOM=-1
#endif
!
!-----------------------------------------------------------------------
!  Set number of biological tracers.
!-----------------------------------------------------------------------
!
      NBT=ic-1-(NAT+NPT+NCS+NNS)
      !nGud=NBT
#if defined BIO_SEDIMENTVARIABLES
!
!-----------------------------------------------------------------------
!  Determine number of sediment variables. All plankton count
!  as one.
!-----------------------------------------------------------------------
!
      isedc=0
      isedn=0
      isedp=0
      isedsi=0
      isedfe=0
!     PIC, POC, PON, POP, POSi, POFe, plankton C
      Nsed=7
      isedc=Nsed
# if defined DARWIN_NQUOTA
!     plankton N
      Nsed=Nsed+1
      isedn=Nsed
# endif
# if defined DARWIN_PQUOTA
!     plankton P
      Nsed=Nsed+1
      isedp=Nsed
# endif
# if defined DARWIN_SIQUOTA
!     plankton Si
      Nsed=Nsed+1
      isedsi=Nsed
# endif
# if defined DARWIN_FEQUOTA
!     plankton Fe
      Nsed=Nsed+1
      isedfe=Nsed
# endif
# if defined DARWIN_CHLQUOTA
!     plankton chl
      Nsed=Nsed+1
      isedchl=Nsed
# endif
#endif /*BIO_SEDIMENTVARIABLES*/
!
!-----------------------------------------------------------------------
!  Allocate various module variables.
!-----------------------------------------------------------------------
!
#include <darwin_mod_sub2.h>
!
!  Allocate biological time-stepping
!
      IF (.not.allocated(ndtbio)) THEN
        allocate ( ndtbio(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      ndtbio(1:Ngrids)=1
      IF (.not.allocated(dtbio)) THEN
        allocate ( dtbio(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      dtbio(1:Ngrids)=0.0_r8
!
!  Allocate biological tracer vector.
!
      IF (.not.allocated(idbio)) THEN
        allocate ( idbio(NBT) )
      END IF
!
!  Set values in biological tracer vector.
!
      ic=NAT+NPT+NCS+NNS
      DO i=1,NBT
        idbio(i)=ic+i
      END DO
#if defined DARWIN_ROMSSINKING
!
!  Allocate sinking variables.
!
      IF (.not.allocated(Wbio)) THEN
        ! NBT+NAT+NPT+NCS+NNS same as MT which initialized later
        allocate ( Wbio(NBT+NAT+NPT+NCS+NNS,Ngrids) )
      END IF
#endif
!
!-----------------------------------------------------------------------
!  Initialize names.
!-----------------------------------------------------------------------
!
      ic=1
      DO i=iMinPrey,nPhoto
        WRITE (plankname(i),'(a,i2.2)') 'phytoplankton', ic
        ic=ic+1
      END DO
      ic=1
      DO i=iMinBact,iMaxBact
        WRITE (plankname(i),'(a,i2.2)') 'bacteria', ic
        ic=ic+1
      END DO
!
!  Ranges can overlap, only use "zooplankton" for unnamed plankton.
!
      ic=1
      DO i=iMinPred,iMaxPred
        IF (i.gt.nPhoto .and. i.lt.iMinBact) THEN
          WRITE (plankname(i),'(a,i2.2)') 'zooplankton', ic
          ic=ic+1
        END IF
      END DO
!
!  Initialize group names. Better names are written to the variable
!  once input has been read (in darwin_inp.h).
!
      DO i=1,nGroup
        WRITE (grp_names(i),'(a,i2.2)') 'group', i
      END DO
#if defined BIO_SEDIMENTVARIABLES
      ic=1
      WRITE (sedname(ic),'(a)') 'PIC'
      ic=ic+1
      WRITE (sedname(ic),'(a)') 'POC'
      ic=ic+1
      WRITE (sedname(ic),'(a)') 'PON'
      ic=ic+1
      WRITE (sedname(ic),'(a)') 'POP'
      ic=ic+1
      WRITE (sedname(ic),'(a)') 'POSi'
      ic=ic+1
      WRITE (sedname(ic),'(a)') 'POFe'
      ic=ic+1
      WRITE (sedname(ic),'(a)') 'plankton_C'
      ic=ic+1
# if defined DARWIN_NQUOTA
      WRITE (sedname(ic),'(a)') 'plankton_N'
      ic=ic+1
# endif
# if defined DARWIN_PQUOTA
      WRITE (sedname(ic),'(a)') 'plankton_P'
      ic=ic+1
# endif
# if defined DARWIN_FEQUOTA
      WRITE (sedname(ic),'(a)') 'plankton_Fe'
      ic=ic+1
# endif
# if defined DARWIN_SIQUOTA
      WRITE (sedname(ic),'(a)') 'plankton_Si'
      ic=ic+1
# endif
#endif

      RETURN
      END SUBROUTINE initialize_biology

      END MODULE mod_biology
