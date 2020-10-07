      SUBROUTINE read_BioPar (model, inp, out, Lwrite)
!
!svn $Id: npzd_iron_inp.h 1031 2020-07-14 01:39:55Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2020 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine reads in NPZD iron (Fiechter, et al. 2009) ecosystem   !
!  model input parameters. They are specified in input script          !
!  "npzd_iron.in".                                                     !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_biology
      USE mod_ncparam
      USE mod_scalars
#if defined DARWIN_RANDOM_TRAITS
      USE ran_state,      ONLY: ran_seed
      USE nrutil,         ONLY: ran1, gasdev
# if defined DISTRIBUTE
      USE distribute_mod, ONLY : mp_bcasti
# endif
#endif
#if defined DARWIN_RADTRANS_IO
      USE mod_iounits,    ONLY: DRT
#endif
!
      USE inp_decode_mod
!
      implicit none
!
!  Imported variable declarations
!
      logical, intent(in) :: Lwrite
      integer, intent(in) :: model, inp, out
!
!  Local variable declarations.
!
      integer :: Npts, Nval
      integer :: iTrcStr, iTrcEnd
      integer :: i, ifield, igrid, is, ic, ip, itracer, itrc
      integer :: ng, nline, status

#if defined DIAGNOSTICS_BIO
      logical, dimension(Ngrids) :: Lbio
#endif
      logical, dimension(NBT,Ngrids) :: Ltrc

      real(r8), dimension(NBT,Ngrids) :: Rbio

      real(dp), dimension(nRval) :: Rval

      character (len=40 ) :: KeyWord
      character (len=256) :: line
      character (len=256), dimension(nCval) :: Cval
#if defined DARWIN_ROMSSINKING
      logical :: onegtzero
#endif /* DARWIN_ROMSSINKING */
#if defined DARWIN_RADTRANS
!  required for darwin_init_fixed.h
        !character*80 title
        !integer ios, i, l, ilambda
        !real(r8) :: planck, c, hc, oavo, hcoavo, rlamm
        !real(r8) :: lambdain, ain, apsin, bin, bbin
#endif
#if defined DARWIN_RANDOM_TRAITS
!  required for darwin_generate_random.h
      integer, dimension(nplank) :: physize, nsource, diacoc
      integer :: ig, np, nz
      integer :: seq 
      real(r8), dimension(nplank) :: phyto_esd, phyto_vol, dmzoo
      real(r8) :: pday, volp, dm, growthdays, mortdays
      real(r8) :: RandNoSize, RandNoDiatom, RandNoDiazo, RandNoGrow
      real(r8) :: RandNoMort, RandNoNsrc, RandNoTemp, RandNoKsat
      real(r8) :: RandNoKsatPAR, RandNoKinhPAR, RandNoDummy
# if defined DARWIN_GEIDER
      real(r8) :: RandNoGrowGeider, RandNoYield, RandNoChl2C
# endif /* DARWIN_GEIDER */
# if defined DARWIN_RADTRANS
      integer :: l, iopt
      real(r8) :: RandNoAPType1, RandNoAPType2, RandNoAPType3
      real(r8) :: RandNoAPType4
      real(r8) :: wb_totalwidth
      !TODO may have to move elsewhere
      real(r8), dimension(nOpt,nlam) :: aphy_chl_type, aphy_chl_ps_type
      real(r8), dimension(nOpt,nlam) :: bphy_mgc_type, bbphy_mgc_type 
      real(r8), dimension(nlam) :: wb_width
# endif /* DARWIN_RADTRANS */
#else /* DARWIN_RANDOM_TRAITS */
!  required for darwin_generate_allometric.h
      integer :: ig, ip2, iz, gmin, gz
      integer, dimension(nGroup) :: jpg
      real(r8) :: logvol, vol, volmin, kappa
      real(r8), dimension(nplank) :: pp_opt, pp_sig, igroup, qcarbon
#endif /* DARWIN_RANDOM_TRAITS */
#if defined DARWIN_RADTRANS_IO
      integer :: load_s1d
      integer :: Cdim
      logical :: find_file
      character (len=50 ) :: label
      integer, allocatable :: Nfiles(:)
#endif
!
!-----------------------------------------------------------------------
!  Initialize.
!-----------------------------------------------------------------------
!
      igrid=1                            ! nested grid counter
      itracer=0                          ! LBC tracer counter
      iTrcStr=1                          ! first LBC tracer to process
      iTrcEnd=NBT                        ! last  LBC tracer to process
      nline=0                            ! LBC multi-line counter
#if defined DARWIN_RADTRANS_IO
      Cdim=SIZE(Cval,1)
      DO i=1,LEN(label)
        label(i:i)=' '
      END DO
      IF (.not.allocated(Nfiles)) THEN
        allocate ( Nfiles(Ngrids) )
        Nfiles(1:Ngrids)=0
      END IF
#endif
!
!-----------------------------------------------------------------------
!  Read in Darwin model parameters.
!-----------------------------------------------------------------------
!
#ifdef ANA_BIOLOGY
      IF (.not.allocated(BioIni)) allocate ( BioIni(MT,Ngrids) )
#endif
      DO WHILE (.TRUE.)
        READ (inp,'(a)',ERR=10,END=20) line
        status=decode_line(line, KeyWord, Nval, Cval, Rval)
        IF (status.gt.0) THEN
          SELECT CASE (TRIM(KeyWord))
            CASE ('Lbiology')
              Npts=load_l(Nval, Cval, Ngrids, Lbiology)
#include <darwin_inp_sub1.h>
#if defined DARWIN_RADTRANS_IO
            CASE ('RADTRANSFILE')
              label='DRT - Darwin Radiative Transfer variables'
              Npts=load_s1d(Nval, Cval, Cdim, line, label, igrid,       &
     &                      Nfiles, DRT)
#endif
#if ! defined DARWIN_RANDOM_TRAITS
            CASE ('grp_names')
              ! wait till all values are loaded
              IF (Nval.ge.nGroup) THEN
                DO ig=1,nGroup
                  WRITE (grp_names(ig),'(a)') trim(Cval(ig)) 
                END DO
              END IF
              ! this does not work if multiple values are on the same line
              !WRITE (grp_names(Nval),'(a)') trim(Cval(Nval))
#endif
            CASE ('TNU2')
              Npts=load_r(Nval, Rval, NBT, Ngrids, Rbio)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  nl_tnu2(i,ng)=Rbio(itrc,ng)
                END DO
              END DO
            CASE ('TNU4')
              Npts=load_r(Nval, Rval, NBT, Ngrids, Rbio)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  nl_tnu4(i,ng)=Rbio(itrc,ng)
                END DO
              END DO
            CASE ('ad_TNU2')
              Npts=load_r(Nval, Rval, NBT, Ngrids, Rbio)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  ad_tnu2(i,ng)=Rbio(itrc,ng)
                  tl_tnu2(i,ng)=Rbio(itrc,ng)
                END DO
              END DO
            CASE ('ad_TNU4')
              Npts=load_r(Nval, Rval, NBT, Ngrids, Rbio)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  ad_tnu4(i,ng)=Rbio(itrc,ng)
                  ad_tnu4(i,ng)=Rbio(itrc,ng)
                END DO
              END DO
            CASE ('LtracerSponge')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  LtracerSponge(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('AKT_BAK')
              Npts=load_r(Nval, Rval, NBT, Ngrids, Rbio)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  Akt_bak(i,ng)=Rbio(itrc,ng)
                END DO
              END DO
            CASE ('ad_AKT_fac')
              Npts=load_r(Nval, Rval, NBT, Ngrids, Rbio)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  ad_Akt_fac(i,ng)=Rbio(itrc,ng)
                  tl_Akt_fac(i,ng)=Rbio(itrc,ng)
                END DO
              END DO
            CASE ('TNUDG')
              Npts=load_r(Nval, Rval, NBT, Ngrids, Rbio)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  Tnudg(i,ng)=Rbio(itrc,ng)
                END DO
              END DO
            CASE ('Hadvection')
              IF (itracer.lt.NBT) THEN
                itracer=itracer+1
              ELSE
                itracer=1                      ! next nested grid
              END IF
              itrc=idbio(itracer)
              Npts=load_tadv(Nval, Cval, line, nline, itrc, igrid,      &
     &                       itracer, idbio(iTrcStr), idbio(iTrcEnd),   &
     &                       Vname(1,idTvar(itrc)),                     &
     &                       Hadvection)
            CASE ('Vadvection')
              IF (itracer.lt.NBT) THEN
                itracer=itracer+1
              ELSE
                itracer=1                      ! next nested grid
              END IF
              itrc=idbio(itracer)
              Npts=load_tadv(Nval, Cval, line, nline, itrc, igrid,      &
     &                       itracer, idbio(iTrcStr), idbio(iTrcEnd),   &
     &                       Vname(1,idTvar(itrc)),                     &
     &                       Vadvection)
#if defined ADJOINT || defined TANGENT || defined TL_IOMS
            CASE ('ad_Hadvection')
              IF (itracer.lt.NBT) THEN
                itracer=itracer+1
              ELSE
                itracer=1                      ! next nested grid
              END IF
              itrc=idbio(itracer)
              Npts=load_tadv(Nval, Cval, line, nline, itrc, igrid,      &
     &                       itracer, idbio(iTrcStr), idbio(iTrcEnd),   &
     &                       Vname(1,idTvar(itrc)),                     &
     &                       ad_Hadvection)
            CASE ('Vadvection')
              IF (itracer.lt.(NBT) THEN
                itracer=itracer+1
              ELSE
                itracer=1                      ! next nested grid
              END IF
              itrc=idbio(itracer)
              Npts=load_tadv(Nval, Cval, line, nline, itrc, igrid,      &
     &                       itracer, idbio(iTrcStr), idbio(iTrcEnd),   &
     &                       Vname(1,idTvar(itrc)),                     &
     &                       ad_Vadvection)
#endif
            CASE ('LBC(isTvar)')
              IF (itracer.lt.NBT) THEN
                itracer=itracer+1
              ELSE
                itracer=1                      ! next nested grid
              END IF
              ifield=isTvar(idbio(itracer))
              Npts=load_lbc(Nval, Cval, line, nline, ifield, igrid,     &
     &                      idbio(iTrcStr), idbio(iTrcEnd),             &
     &                      Vname(1,idTvar(idbio(itracer))), LBC)
#if defined ADJOINT || defined TANGENT || defined TL_IOMS
            CASE ('ad_LBC(isTvar)')
              IF (itracer.lt.NBT) THEN
                itracer=itracer+1
              ELSE
                itracer=1                      ! next nested grid
              END IF
              ifield=isTvar(idbio(itracer))
              Npts=load_lbc(Nval, Cval, line, nline, ifield, igrid,     &
     &                      idbio(iTrcStr), idbio(iTrcEnd),             &
     &                      Vname(1,idTvar(idbio(itracer))), ad_LBC)
#endif
            CASE ('LtracerSrc')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  LtracerSrc(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('LtracerCLM')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  LtracerCLM(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('LnudgeTCLM')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  LnudgeTCLM(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('Hout(idTvar)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idTvar(idbio(itrc))
                  IF (i.eq.0) THEN
                    IF (Master) WRITE (out,30)                          &
     &                          'idTvar(idbio(', itrc, '))'
                    exit_flag=5
                    RETURN
                  END IF
                  Hout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('Hout(idTsur)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idTsur(idbio(itrc))
                  IF (i.eq.0) THEN
                    IF (Master) WRITE (out,30)                          &
     &                                'idTsur(idbio(', itrc, '))'
                    exit_flag=5
                    RETURN
                  END IF
                  Hout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
#if defined BIO_SEDIMENTVARIABLES
            CASE ('Hout(idBSMa)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,Nsed
                  i=idBSMa(itrc)
                  IF (i.eq.0) THEN
                    IF (Master) WRITE (out,30)                          &
     &                                'idBSMa(', itrc, ')'
                    exit_flag=5
                    RETURN
                  END IF
                  Hout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
#endif
            CASE ('Qout(idTvar)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idTvar(idbio(itrc))
                  Qout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('Qout(idsurT)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idsurT(idbio(itrc))
                  IF (i.eq.0) THEN
                    IF (Master) WRITE (out,30)                          &
     &                                'idsurT(idbio(', itrc, '))'
                    exit_flag=5
                    RETURN
                  END IF
                  Qout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('Qout(idTsur)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idTsur(idbio(itrc))
                  Qout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
#if defined BIO_SEDIMENTVARIABLES
            CASE ('Qout(idBSMa)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,Nsed
                  i=idBSMa(itrc)
                  IF (i.eq.0) THEN
                    IF (Master) WRITE (out,30)                          &
     &                                'idBSMa(', itrc, ')'
                    exit_flag=5
                    RETURN
                  END IF
                  Qout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
#endif
#if defined AVERAGES    || \
   (defined AD_AVERAGES && defined ADJOINT) || \
   (defined RP_AVERAGES && defined TL_IOMS) || \
   (defined TL_AVERAGES && defined TANGENT)
            CASE ('Aout(idTvar)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idTvar(idbio(itrc))
                  Aout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('Aout(idTTav)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idTTav(idbio(itrc))
                  Aout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('Aout(idUTav)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idUTav(idbio(itrc))
                  Aout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('Aout(idVTav)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idVTav(idbio(itrc))
                  Aout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('Aout(iHUTav)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=iHUTav(idbio(itrc))
                  Aout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('Aout(iHVTav)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=iHVTav(idbio(itrc))
                  Aout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
#endif
#ifdef DIAGNOSTICS_TS
            CASE ('Dout(iTrate)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iTrate),ng)=Ltrc(i,ng)
                END DO
              END DO
            CASE ('Dout(iThadv)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iThadv),ng)=Ltrc(i,ng)
                END DO
              END DO
            CASE ('Dout(iTxadv)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iTxadv),ng)=Ltrc(i,ng)
                END DO
              END DO
            CASE ('Dout(iTyadv)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iTyadv),ng)=Ltrc(i,ng)
                END DO
              END DO
            CASE ('Dout(iTvadv)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iTvadv),ng)=Ltrc(i,ng)
                END DO
              END DO
# if defined TS_DIF2 || defined TS_DIF4
            CASE ('Dout(iThdif)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iThdif),ng)=Ltrc(i,ng)
                END DO
              END DO
            CASE ('Dout(iTxdif)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iTxdif),ng)=Ltrc(i,ng)
                END DO
              END DO
            CASE ('Dout(iTydif)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iTydif),ng)=Ltrc(i,ng)
                END DO
              END DO
#  if defined MIX_GEO_TS || defined MIX_ISO_TS
            CASE ('Dout(iTsdif)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iTsdif),ng)=Ltrc(i,ng)
                END DO
              END DO
#  endif
# endif
            CASE ('Dout(iTvdif)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iTvdif),ng)=Ltrc(i,ng)
                END DO
              END DO
#endif
#if defined DIAGNOSTICS_BIO
            CASE ('Dout(iPP)')
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iPP)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iNfix)')
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iNfix)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iDenit)')
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iDenit)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iDenitN)')
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iDenitN)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iConsPO4)')
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iConsPO4)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iConsSi)')
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iConsSi)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iConsFe)')
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iConsFe)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iConsDIN)')
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iConsDIN)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iPPplank)')
              i=iDbio3(iPPplank)
              Npts=load_l(Nval, Cval, nPPplank, Ngrids,                  &
     &          Dout(i:i+nPPplank-1,1:Ngrids))
            CASE ('Dout(iGRplank)')
              i=iDbio3(iGRplank)
              Npts=load_l(Nval, Cval, nGRplank, Ngrids,                  &
     &          Dout(i:i+nGRplank-1,1:Ngrids))
#endif /*DIAGNOSTICS_BIO*/
          END SELECT
        END IF
      END DO
  10  IF (Master) WRITE (out,40) line
      exit_flag=4
      RETURN
  20  CONTINUE
!
!-----------------------------------------------------------------------
!  Report variables and input parameters.
!-----------------------------------------------------------------------
!
      IF (Lwrite) THEN
        DO ng=1,Ngrids
          IF (Lbiology(ng)) THEN
!
!  Report variables
!
            WRITE (out,'(/1x,a,i0,a)')                                  &
     &        'Darwin variable information (Grid ',ng,'):'
            WRITE (out,'(1x,a)')                                        &
     &        '====================================='
            WRITE (out,'(2x,a,i0)') 'Number of biological tracers: ',NBT
            DO itrc=1,NBT
              i=idbio(itrc)
              WRITE(out,'(4x,a,i3,"/",i0,": ",a)')                      &
     &        'biological tracer',itrc,NBT,trim(Vname(1,idTvar(i)))
            END DO
!
!  Report input parameters.
!
            WRITE (out,50) ng
#include <darwin_inp_sub2.h>
#if defined DARWIN_RADTRANS_IO
            IF (.not.find_file(ng, DRT(ng)%name, 'RADTRANSFILE')) THEN
              IF (Master) WRITE (out,'(2x,4a)') 'Error: Cannot find ',  &
     &          'radiative transfer data file: "', TRIM(DRT(ng)%name),  &
     &          '".'
              exit_flag=4
              RETURN
            END IF
            WRITE (out,'(2x,a,a)') 'Radiative transfer data file:  ',   &
     &        TRIM(DRT(ng)%name)
#endif
#ifdef TS_DIF2
            DO itrc=1,NBT
              i=idbio(itrc)
              WRITE (out,90) nl_tnu2(i,ng), 'nl_tnu2', i,               &
     &              'NLM Horizontal, harmonic mixing coefficient',      &
     &              '(m2/s) for tracer ', i, TRIM(Vname(1,idTvar(i)))
# ifdef ADJOINT
              WRITE (out,90) ad_tnu2(i,ng), 'ad_tnu2', i,               &
     &              'ADM Horizontal, harmonic mixing coefficient',      &
     &              '(m2/s) for tracer ', i, TRIM(Vname(1,idTvar(i)))
# endif
# if defined TANGENT || defined TL_IOMS
              WRITE (out,90) tl_tnu2(i,ng), 'tl_tnu2', i,               &
     &              'TLM Horizontal, harmonic mixing coefficient',      &
     &              '(m2/s) for tracer ', i, TRIM(Vname(1,idTvar(i)))
# endif
            END DO
#endif
#ifdef TS_DIF4
            DO itrc=1,NBT
              i=idbio(itrc)
              WRITE (out,90) nl_tnu4(i,ng), 'nl_tnu4', i,               &
     &              'NLM Horizontal, biharmonic mixing coefficient',    &
     &              '(m4/s) for tracer ', i, TRIM(Vname(1,idTvar(i)))
# ifdef ADJOINT
              WRITE (out,90) ad_tnu4(i,ng), 'ad_tnu4', i,               &
     &              'ADM Horizontal, biharmonic mixing coefficient',    &
     &              '(m4/s) for tracer ', i, TRIM(Vname(1,idTvar(i)))
# endif
# if defined TANGENT || defined TL_IOMS
              WRITE (out,90) tl_tnu4(i,ng), 'tl_tnu4', i,               &
     &              'TLM Horizontal, biharmonic mixing coefficient',    &
     &              '(m4/s) for tracer ', i, TRIM(Vname(1,idTvar(i)))
# endif
            END DO
#endif
            DO itrc=1,NBT
              i=idbio(itrc)
              IF (LtracerSponge(i,ng)) THEN
                WRITE (out,100) LtracerSponge(i,ng), 'LtracerSponge',   &
     &              i, 'Turning ON  sponge on tracer ', i,              &
     &              TRIM(Vname(1,idTvar(i)))
              ELSE
                WRITE (out,100) LtracerSponge(i,ng), 'LtracerSponge',   &
     &              i, 'Turning OFF sponge on tracer ', i,              &
     &              TRIM(Vname(1,idTvar(i)))
              END IF
            END DO
            DO itrc=1,NBT
              i=idbio(itrc)
              WRITE(out,90) Akt_bak(i,ng), 'Akt_bak', i,                &
     &             'Background vertical mixing coefficient (m2/s)',     &
     &             'for tracer ', i, TRIM(Vname(1,idTvar(i)))
            END DO
#ifdef FORWARD_MIXING
            DO itrc=1,NBT
              i=idbio(itrc)
# ifdef ADJOINT
              WRITE (out,90) ad_Akt_fac(i,ng), 'ad_Akt_fac', i,         &
     &              'ADM basic state vertical mixing scale factor',     &
     &              'for tracer ', i, TRIM(Vname(1,idTvar(i)))
# endif
# if defined TANGENT || defined TL_IOMS
              WRITE (out,90) tl_Akt_fac(i,ng), 'tl_Akt_fac', i,         &
     &              'TLM basic state vertical mixing scale factor',     &
     &              'for tracer ', i, TRIM(Vname(1,idTvar(i)))
# endif
            END DO
#endif
            DO itrc=1,NBT
              i=idbio(itrc)
              WRITE (out,90) Tnudg(i,ng), 'Tnudg', i,                   &
     &              'Nudging/relaxation time scale (days)',             &
     &              'for tracer ', i, TRIM(Vname(1,idTvar(i)))
            END DO
            DO itrc=1,NBT
              i=idbio(itrc)
              IF (LtracerSrc(i,ng)) THEN
                WRITE (out,100) LtracerSrc(i,ng), 'LtracerSrc',         &
     &              i, 'Turning ON  point sources/Sink on tracer ', i,  &
     &              TRIM(Vname(1,idTvar(i)))
              ELSE
                WRITE (out,100) LtracerSrc(i,ng), 'LtracerSrc',         &
     &              i, 'Turning OFF point sources/Sink on tracer ', i,  &
     &              TRIM(Vname(1,idTvar(i)))
              END IF
            END DO
            DO itrc=1,NBT
              i=idbio(itrc)
              IF (LtracerCLM(i,ng)) THEN
                WRITE (out,100) LtracerCLM(i,ng), 'LtracerCLM', i,      &
     &              'Turning ON  processing of climatology tracer ', i, &
     &              TRIM(Vname(1,idTvar(i)))
              ELSE
                WRITE (out,100) LtracerCLM(i,ng), 'LtracerCLM', i,      &
     &              'Turning OFF processing of climatology tracer ', i, &
     &              TRIM(Vname(1,idTvar(i)))
              END IF
            END DO
            DO itrc=1,NBT
              i=idbio(itrc)
              IF (LnudgeTCLM(i,ng)) THEN
                WRITE (out,100) LnudgeTCLM(i,ng), 'LnudgeTCLM', i,      &
     &              'Turning ON  nudging of climatology tracer ', i,    &
     &              TRIM(Vname(1,idTvar(i)))
              ELSE
                WRITE (out,100) LnudgeTCLM(i,ng), 'LnudgeTCLM', i,      &
     &              'Turning OFF nudging of climatology tracer ', i,    &
     &              TRIM(Vname(1,idTvar(i)))
              END IF
            END DO
            IF ((nHIS(ng).gt.0).and.ANY(Hout(:,ng))) THEN
              WRITE (out,'(1x)')
              DO itrc=1,NBT
                i=idbio(itrc)
                IF (Hout(idTvar(i),ng)) WRITE (out,110)                 &
     &              Hout(idTvar(i),ng), 'Hout(idTvar)',                 &
     &              'Write out tracer ', i, TRIM(Vname(1,idTvar(i)))
              END DO
              DO itrc=1,NBT
                i=idbio(itrc)
                IF (Hout(idTsur(i),ng)) WRITE (out,110)                 &
     &              Hout(idTsur(i),ng), 'Hout(idTsur)',                 &
     &              'Write out tracer flux ', i,                        &
     &              TRIM(Vname(1,idTvar(i)))
              END DO
            END IF
            IF ((nQCK(ng).gt.0).and.ANY(Qout(:,ng))) THEN
              WRITE (out,'(1x)')
              DO itrc=1,NBT
                i=idbio(itrc)
                IF (Qout(idTvar(i),ng)) WRITE (out,110)                 &
     &              Qout(idTvar(i),ng), 'Qout(idTvar)',                 &
     &              'Write out tracer ', i, TRIM(Vname(1,idTvar(i)))
              END DO
              DO itrc=1,NBT
                i=idbio(itrc)
                IF (Qout(idsurT(i),ng)) WRITE (out,110)                 &
     &              Qout(idsurT(i),ng), 'Qout(idsurT)',                 &
     &              'Write out surface tracer ', i,                     &
     &              TRIM(Vname(1,idTvar(i)))
              END DO
              DO itrc=1,NBT
                i=idbio(itrc)
                IF (Qout(idTsur(i),ng)) WRITE (out,110)                 &
     &              Qout(idTsur(i),ng), 'Qout(idTsur)',                 &
     &              'Write out tracer flux ', i,                        &
     &              TRIM(Vname(1,idTvar(i)))
              END DO
            END IF
#if defined AVERAGES    || \
   (defined AD_AVERAGES && defined ADJOINT) || \
   (defined RP_AVERAGES && defined TL_IOMS) || \
   (defined TL_AVERAGES && defined TANGENT)
            IF ((nAVG(ng).gt.0).and.ANY(Aout(:,ng))) THEN
              WRITE (out,'(1x)')
              DO itrc=1,NBT
                i=idbio(itrc)
                IF (Aout(idTvar(i),ng)) WRITE (out,110)                 &
     &              Aout(idTvar(i),ng), 'Aout(idTvar)',                 &
     &              'Write out averaged tracer ', i,                    &
     &              TRIM(Vname(1,idTvar(i)))
              END DO
              DO itrc=1,NBT
                i=idbio(itrc)
                IF (Aout(idTTav(i),ng)) WRITE (out,110)                 &
     &              Aout(idTTav(i),ng), 'Aout(idTTav)',                 &
     &              'Write out averaged <t*t> for tracer ', i,          &
     &              TRIM(Vname(1,idTvar(i)))
              END DO
              DO itrc=1,NBT
                i=idbio(itrc)
                IF (Aout(idUTav(i),ng)) WRITE (out,110)                 &
     &              Aout(idUTav(i),ng), 'Aout(idUTav)',                 &
     &              'Write out averaged <u*t> for tracer ', i,          &
     &              TRIM(Vname(1,idTvar(i)))
              END DO
              DO itrc=1,NBT
                i=idbio(itrc)
                IF (Aout(idVTav(i),ng)) WRITE (out,110)                 &
     &              Aout(idVTav(i),ng), 'Aout(idVTav)',                 &
     &              'Write out averaged <v*t> for tracer ', i,          &
     &              TRIM(Vname(1,idTvar(i)))
              END DO
              DO itrc=1,NBT
                i=idbio(itrc)
                IF (Aout(iHUTav(i),ng)) WRITE (out,110)                 &
     &              Aout(iHUTav(i),ng), 'Aout(iHUTav)',                 &
     &              'Write out averaged <Huon*t> for tracer ', i,       &
     &              TRIM(Vname(1,idTvar(i)))
              END DO
              DO itrc=1,NBT
                i=idbio(itrc)
                IF (Aout(iHVTav(i),ng)) WRITE (out,110)                 &
     &              Aout(iHVTav(i),ng), 'Aout(iHVTav)',                 &
     &              'Write out averaged <Hvom*t> for tracer ', i,       &
     &              TRIM(Vname(1,idTvar(i)))
              END DO
            END IF
#endif
#ifdef DIAGNOSTICS_TS
            IF ((nDIA(ng).gt.0).and.ANY(Dout(:,ng))) THEN
              WRITE (out,'(1x)')
              DO i=1,NBT
                itrc=idbio(i)
                IF (Dout(idDtrc(itrc,iTrate),ng))                       &
     &            WRITE (out,110) .TRUE., 'Dout(iTrate)',               &
     &              'Write out rate of change of tracer ', itrc,        &
     &              TRIM(Vname(1,idTvar(itrc)))
              END DO
              DO i=1,NBT
                itrc=idbio(i)
                IF (Dout(idDtrc(itrc,iThadv),ng))                       &
     &            WRITE (out,110) .TRUE., 'Dout(iThadv)',               &
     &              'Write out horizontal advection, tracer ', itrc,    &
     &              TRIM(Vname(1,idTvar(itrc)))
              END DO
              DO i=1,NBT
                itrc=idbio(i)
                IF (Dout(idDtrc(itrc,iTxadv),ng))                       &
     &            WRITE (out,110) .TRUE., 'Dout(iTxadv)',               &
     &              'Write out horizontal X-advection, tracer ', itrc,  &
     &              TRIM(Vname(1,idTvar(itrc)))
              END DO
              DO i=1,NBT
                itrc=idbio(i)
                IF (Dout(idDtrc(itrc,iTyadv),ng))                       &
     &            WRITE (out,110) .TRUE., 'Dout(iTyadv)',               &
     &              'Write out horizontal Y-advection, tracer ', itrc,  &
     &              TRIM(Vname(1,idTvar(itrc)))
              END DO
              DO i=1,NBT
                itrc=idbio(i)
                IF (Dout(idDtrc(itrc,iTvadv),ng))                       &
     &            WRITE (out,110) .TRUE., 'Dout(iTvadv)',               &
     &              'Write out vertical advection, tracer ', itrc,      &
     &              TRIM(Vname(1,idTvar(itrc)))
              END DO
# if defined TS_DIF2 || defined TS_DIF4
              DO i=1,NBT
                itrc=idbio(i)
                IF (Dout(idDtrc(itrc,iThdif),ng))                       &
     &            WRITE (out,110) .TRUE., 'Dout(iThdif)',               &
     &              'Write out horizontal diffusion, tracer ', itrc,    &
     &              TRIM(Vname(1,idTvar(itrc)))
              END DO
              DO i=1,NBT
                itrc=idbio(i)
                IF (Dout(idDtrc(i,iTxdif),ng))                          &
     &            WRITE (out,110) .TRUE., 'Dout(iTxdif)',               &
     &              'Write out horizontal X-diffusion, tracer ', itrc,  &
     &              TRIM(Vname(1,idTvar(itrc)))
              END DO
              DO i=1,NBT
                itrc=idbio(i)
                IF (Dout(idDtrc(itrc,iTydif),ng))                       &
     &            WRITE (out,110) .TRUE., 'Dout(iTydif)',               &
     &              'Write out horizontal Y-diffusion, tracer ', itrc,  &
     &              TRIM(Vname(1,idTvar(itrc)))
              END DO
#  if defined MIX_GEO_TS || defined MIX_ISO_TS
              DO i=1,NBT
                itrc=idbio(i)
                IF (Dout(idDtrc(itrc,iTsdif),ng))                       &
     &            WRITE (out,110) .TRUE., 'Dout(iTsdif)',               &
     &              'Write out horizontal S-diffusion, tracer ', itrc,  &
     &              TRIM(Vname(1,idTvar(itrc)))
              END DO
#  endif
# endif
              DO i=1,NBT
                itrc=idbio(i)
                IF (Dout(idDtrc(itrc,iTvdif),ng))                       &
     &            WRITE (out,110) .TRUE., 'Dout(iTvdif)',               &
     &              'Write out vertical diffusion, tracer ', itrc,      &
     &              TRIM(Vname(1,idTvar(itrc)))
              END DO
            END IF
#endif
          END IF
        END DO
      END IF
!
!-----------------------------------------------------------------------
!  Rescale biological tracer parameters.
!-----------------------------------------------------------------------
!
!  Take the square root of the biharmonic coefficients so it can
!  be applied to each harmonic operator.
!
      DO ng=1,Ngrids
        DO itrc=1,NBT
          i=idbio(itrc)
          nl_tnu4(i,ng)=SQRT(ABS(nl_tnu4(i,ng)))
#ifdef ADJOINT
          ad_tnu4(i,ng)=SQRT(ABS(ad_tnu4(i,ng)))
#endif
#if defined TANGENT || defined TL_IOMS
          tl_tnu4(i,ng)=SQRT(ABS(tl_tnu4(i,ng)))
#endif
!
!  Compute inverse nudging coefficients (1/s) used in various tasks.
!
          IF (Tnudg(i,ng).gt.0.0_r8) THEN
            Tnudg(i,ng)=1.0_r8/(Tnudg(i,ng)*86400.0_r8)
          ELSE
            Tnudg(i,ng)=0.0_r8
          END IF
        END DO
      END DO
!
!-----------------------------------------------------------------------
!  Initialize variables that depend on input.
!-----------------------------------------------------------------------
!
      DO ng=1,Ngrids
!TODO add back in
!#include <darwin_init_fixed.h>
#if defined DARWIN_RANDOM_TRAITS
!
!  Use FORTRAN random number generator to generate random seed for
!  ROMS random number generator.
!
        IF (Master) THEN
          CALL random_seed()
          CALL random_number(dm)
          seq=floor(dm*huge(seq))
        ELSE
          seq=0
        ENDIF
# if defined DISTRIBUTE
        CALL mp_bcasti (1, model, seq)
# endif
        CALL ran_seed(seq)
!
!  Now generate seeds for Darwin plankton.
!
        IF (Master) THEN
          DO i=1,nChl
            IF (seed_phytoplankton(i,ng)==-1) THEN
              CALL ran1(dm)
              seed_phytoplankton(i,ng) =                              &
     &          floor(dm*huge(seed_phytoplankton(i,ng)))
            END IF
          END DO
        ELSE
          DO i=1,nChl
            IF (seed_phytoplankton(i,ng)==-1) THEN
              seed_phytoplankton(i,ng)=0
            END IF
          END DO
        ENDIF
# if defined DISTRIBUTE
        CALL mp_bcasti (1, model, seed_phytoplankton)
# endif
#include <darwin_generate_random.h>
#else /* DARWIN_RANDOM_TRAITS */
#include <darwin_generate_allometric.h>
# if defined DARWIN_PLANK_BUOYCTRL
        DO ip=1,nplank
          ig = group(ip,ng)
          buoyctrl(ip,ng)=grp_buoyctrl(ig,ng)
        END DO
# endif /* DARWIN_PLANK_BUOYCTRL */
# if defined DARWIN_DEACTIVATED
!
!  Update group names.
!
        DO ig=1,nGroup
          IF (grp_hasSi(ig,ng)) THEN
            WRITE (grp_names(ig),'(a)') 'diatom'
          ELSEIF (grp_diazo(ig,ng)) THEN
            WRITE (grp_names(ig),'(a)') 'diazotroph'
          ELSEIF (grp_hasPIC(ig,ng)) THEN
            WRITE (grp_names(ig),'(a)') 'coccolithophore'
          ELSEIF (grp_pred(ig,ng).and.grp_photo(ig,ng)) THEN
            WRITE (grp_names(ig),'(a)') 'mixotrophic dinoflagellate'
          ELSEIF (grp_pred(ig,ng)) THEN
            WRITE (grp_names(ig),'(a)') 'zooplankton'
          !ELSEIF (grp_(ig,ng)) THEN
          !  WRITE (grp_names(ig),'(a)') 'prokaryote'
          !ELSEIF (grp_(ig,ng)) THEN
          !  WRITE (grp_names(ig),'(a)') 'pico-eukaryote'
          END IF
        END DO
# endif /* DARWIN_DEACTIVATED */
#endif /* DARWIN_RANDOM_TRAITS */
#include <darwin_readtraits.h>
!
!  Custom reporting of parameter values generated by 
!  darwin_generate_random/darwin_generate_allometric.
!
        IF (Master) THEN
          WRITE (out,'(/1x,a,i0,a)')                                    &
#if defined DARWIN_RANDOM_TRAITS
     &      'Summary of randomly generated traits (Grid ',ng,'):'
          WRITE (out,'(1x,a)')                                          &
     &      '=============================================='
#else
     &      'Summary of traits (Grid ',ng,'):'
          WRITE (out,'(1x,a)')                                          &
     &      '==========================='
#endif
          ic=1
          DO i=iMinPred,iMaxPred
            write(out,'(3x,a8,i4,a,1x,a)') 'predator',ic,':',           &
     &        trim(plankname(i))
            ic=ic+1
          END DO
          ic=1
          DO is=iMinPrey,iMaxPrey
            write(out,'(3x,a8,i4,a,1x,a)') 'prey',ic,':',               &
     &        trim(plankname(is))
            ic=ic+1
          END DO
#if ! defined DARWIN_RANDOM_TRAITS
!
!  Report groups.
!
          WRITE (out,'(/1x,a,i0,a)')                                    &
     &      'Group information (Grid ',ng,'):'
          WRITE (out,'(1x,a)')                                          &
     &      '==========================='
          DO ig=1,nGroup
            WRITE (out,'(3x,a,i0,3a,t32)',advance='no')                 &
     &         'group ',ig,' ("',trim(grp_names(ig)),'"):'
            IF (grp_pred(ig,ng).gt.0) THEN
              WRITE (out,'(a)',advance='no') ' grp_pred'
            END IF
            IF (grp_prey(ig,ng).gt.0) THEN
              WRITE (out,'(a)',advance='no') ' grp_prey'
            END IF
            IF (grp_photo(ig,ng).gt.0) THEN
              WRITE (out,'(a)',advance='no') ' grp_photo'
            END IF
            IF (grp_aerobic(ig,ng).gt.0) THEN
              WRITE (out,'(a)',advance='no') ' grp_aerobic'
            END IF
            IF (grp_denit(ig,ng).gt.0) THEN
              WRITE (out,'(a)',advance='no') ' grp_denit'
            END IF
            IF (grp_hasSi(ig,ng).gt.0) THEN
              WRITE (out,'(a)',advance='no') ' grp_hasSi'
            END IF
            IF (grp_hasPIC(ig,ng).gt.0) THEN
              WRITE (out,'(a)',advance='no') ' grp_hasPIC'
            END IF
            IF (grp_diazo(ig,ng).gt.0) THEN
              WRITE (out,'(a)',advance='no') ' grp_diazo'
            END IF
            IF (grp_useNH4(ig,ng).gt.0) THEN
              WRITE (out,'(a)',advance='no') ' grp_useNH4'
            END IF
            IF (grp_useNO2(ig,ng).gt.0) THEN
              WRITE (out,'(a)',advance='no') ' grp_useNO2'
            END IF
            IF (grp_useNO3(ig,ng).gt.0) THEN
              WRITE (out,'(a)',advance='no') ' grp_useNO3'
            END IF
            IF (grp_combNO(ig,ng).gt.0) THEN
              WRITE (out,'(a)',advance='no') ' grp_combNO'
            END IF
# if defined DARWIN_PLANK_BUOYCTRL
            IF (grp_buoyctrl(ig,ng)) THEN
              WRITE (out,'(a)',advance='no') ' grp_buoyctrl'
            END IF
# endif
            IF (grp_bacttype(ig,ng).gt.0) THEN
              WRITE (out,'(a,i0)',advance='no')                         &
     &          ' grp_bacttype:', grp_bacttype(ig,ng)
            END IF
# if defined DARWIN_RADTRANS
            IF (grp_aptype(ig,ng).gt.0) THEN
              WRITE (out,'(a,i0)',advance='no')                         &
     &          ' grp_aptype:', grp_aptype(ig,ng)
            END IF
# endif
            write(out,*) ''
          END DO
!
!  Report group memberships.
!
          WRITE (out,'(/1x,a,i0,a)')                                    &
     &      'Group membership (Grid ',ng,'):'
          WRITE (out,'(1x,a)')                                          &
     &      '=========================='
          DO i=1,nplank
            ig=group(i,ng)
            WRITE (out,'(a20,a,i0,3a)') trim(plankname(i)),             &
     &        ': group ',ig,' "', trim(grp_names(ig)), '"'
          END DO
#endif /* !DARWIN_RANDOM_TRAITS */
!
!  Report ksat parameters.
!
          write (out,'(/1x,a,i0,a)')                                    &
     &      'Phytoplankton feeding parameters (Grid ',ng,'):'
          write (out,'(1x,a)')                                          &
     &      '=========================================='
          write (out,'(22x,9(1x,a12))')                                 &
     &      'PCmax','PCmax (d-1)',                                      &
     &      'ksatNO3','ksatNO2','ksatNH4','ksatPO4','ksatSiO2','ksatFeT'
          DO i=1,nPhoto
            write (out,'(a20,": ",9(1x,f12.9))') trim(plankname(i)),    &
     &      PCmax(i,ng),PCmax(i,ng)*day2sec,                            &
     &      ksatNO3(i,ng),ksatNO2(i,ng),ksatNH4(i,ng),                  &
     &      ksatPO4(i,ng),ksatSiO2(i,ng),ksatFeT(i,ng)
          END DO

#if ! defined DARWIN_READ_PALAT
!
!  Report palat
!
          write(out,'(/1x,a)') 'palat:'
          write(out,'(10x,a)') 'predator'
          write(out,'(3x,a4,3x)',advance='no') 'prey'
          ic=1
          DO i=iMinPred,iMaxPred
            write(out,'(i6)',advance='no') ic
            ic=ic+1
          END DO
          write(out,*) ''
          ic=1
          DO is=iMinPrey,iMaxPrey
            write(out,'(3x,i4,3x,99(1x,f5.3))') ic,                     &
     &        palat(is,iMinPred:iMaxPred,ng)    
            ic=ic+1
          END DO
#endif
        END IF
      END DO
#if defined DARWIN_ROMSSINKING
      DO ng=1,Ngrids
!
!  Set values in sinking index vector.
!
!  Note that Wbio has dimensions of (MT,Ngrids) and not (Nsink,Ngrids)
        Wbio(:,ng) = 0.0_r8
        Wbio(iPIC,ng) =  wPIC_sink(ng)
        Wbio(iPOC,ng) =  wC_sink(ng)
        Wbio(iPON,ng) =  wN_sink(ng)
        Wbio(iPOP,ng) =  wP_sink(ng)
        Wbio(iPOSi,ng) = wSi_sink(ng)
        Wbio(iPOFe,ng) = wFe_sink(ng)
        DO i=1,nplank
          Wbio(ic_+i-1,ng) = wsink(i,ng)
#if defined DARWIN_NQUOTA
          Wbio(in_+i-1,ng) = wsink(i,ng)
#endif
#if defined DARWIN_PQUOTA
          Wbio(ip_+i-1,ng) = wsink(i,ng)
#endif
#if defined DARWIN_SIQUOTA
          Wbio(isi+i-1,ng) = wsink(i,ng)
#endif
#if defined DARWIN_FEQUOTA
          Wbio(ife+i-1,ng) = wsink(i,ng)
#endif
        END DO
        DO i=1,nChl
          Wbio(iChl+i-1,ng) = wsink(i,ng)
        END DO
      END DO
!
!  Compute Nsink by summing all nonzero values across grids.
!
      Nsink=0
      DO i=1,MT
        onegtzero=.FALSE.
        DO ng=1,Ngrids
          IF(ABS(Wbio(i,ng)).gt.BioMin(ng)) onegtzero=.TRUE.
        END DO
        IF (onegtzero) Nsink=Nsink+1
      END DO
      IF (.not.allocated(idsink)) THEN
        allocate ( idsink(Nsink) )
      END IF
#if defined BIO_SEDIMENTVARIABLES
      IF (.not.allocated(idsed)) THEN
        allocate ( idsed(Nsink) )
      END IF
#endif
!
!  Compute values in sinking index vector.
!
      idsink(:) = 0
#if defined BIO_SEDIMENTVARIABLES
      idsed(:) = 0
#endif /* BIO_SEDIMENTVARIABLES */
      itrc=1
      DO i=1,MT
        onegtzero=.FALSE.
        DO ng=1,Ngrids
          IF(ABS(Wbio(i,ng)).gt.BioMin(ng)) onegtzero=.TRUE.
        END DO
        IF (onegtzero) THEN
          idsink(itrc) = i
#if defined BIO_SEDIMENTVARIABLES
          IF (itrc.le.6) THEN
            idsed(itrc) = itrc
          ELSE IF (i.ge.ic_.and.i.le.ec_) THEN
            idsed(itrc) = isedc
          ELSE IF (i.ge.in_.and.i.le.en_) THEN
            idsed(itrc) = isedn
          ELSE IF (i.ge.ip_.and.i.le.ep_) THEN
            idsed(itrc) = isedp
          ELSE IF (i.ge.ife.and.i.le.efe) THEN
            idsed(itrc) = isedfe
          ELSE IF (i.ge.isi.and.i.le.esi) THEN
            idsed(itrc) = isedsi
          ELSE IF (i.ge.iChl.and.i.le.eChl) THEN
            idsed(itrc) = isedchl
          END IF
#endif /* BIO_SEDIMENTVARIABLES */
          itrc=itrc+1
        END IF
      END DO
      !if (master) then
      !  write(*,*) ':::Wbio', Wbio
      !  write(*,*) ':::Wbio_pday', Wbio*sec2day
      !  write(*,*) ':::Nsink', Nsink
      !  write(*,*) ':::idsink',idsink 
      !  write(*,*) ':::control',iPOC,iPON,iPOP,iPOSi,iPOFe,iPIC
      !  write(*,*) ':::idsed',idsed
      !end if
      IF (Master) THEN
        DO ng=1,Ngrids
          WRITE (out,'(/1x,a,i1.1,a)')                                  &
     &      'Sinking rates for biological tracers (Grid ',ng,'):'
          WRITE (out,'(1x,a)')                                          &
     &      '=============================================='
          DO i=1,Nsink
            WRITE(out,'(1x,f11.3,1x,a,2x,a)')                           &
     &        Wbio(idsink(i),ng)*day2sec, 'd-1',                        &
     &        TRIM(Vname(1,idTvar(idsink(i))))
          END DO
        END DO
#if defined BIO_SEDIMENTVARIABLES
        WRITE (out,'(/1x,a)')                                           &
     &    'Biological sediment variable summary (all grids):'
        WRITE (out,'(1x,a)')                                            &
     &    '================================================='
        
        DO i=1,Nsink
          WRITE(out,'(3x,4a)')                                          & 
     &      TRIM(Vname(1,idTvar(idsink(i)))),' turns into ',            &
     &      TRIM(Vname(1,idBSMa(idsed(i)))),                            &
     &      ' upon reaching the sediment'
        END DO
#endif /* BIO_SEDIMENTVARIABLES */ 
      END IF
#endif /* DARWIN_ROMSSINKING */
#if defined DIAGNOSTICS_BIO
      IF (Master) THEN
        DO ng=1,Ngrids
          WRITE (out,'(/1x,a,i1.1,a)')                                  &
     &      'Biological diagnostic variable output (Grid ',ng,'):'
          WRITE (out,'(1x,a)')                                          &
     &      '==============================================='
          DO i=1,NDbio3d
            ifield=iDbio3(i)
            IF (Dout(ifield,ng)) THEN
              write(out,'(3x,a,1x,"(",a,")")') TRIM(Vname(1,ifield)),   &
     &        TRIM(Vname(2,ifield))
            END IF
          END DO
        END DO
      END IF
# if defined DIAGNOSTICS_BIO_MAPPING
!
!-----------------------------------------------------------------------
!  Allocate diagnostic variables based on Dout.
!-----------------------------------------------------------------------
!
      DO ng=1,Ngrids
        NDb3d_o=0 
        iDbiomapping(:,ng)=0
        ip=1
        DO i=1,darwin_nDiag
          IF (Dout(iDbio3(i),ng)) THEN
            NDb3d_o=NDb3d_o+1
            !iDbiomapping(i,ng)=ip
            iDbiomapping(ip,ng)=i
            ip=ip+1
          END IF
        END DO
#  if defined DIAGNOSTICS_BIO_MAPPING_DEBUG
        IF (Master) THEN
          write(out,'(a,99(1x,i3))') 'iDbiomapping: ',iDbiomapping(:,ng)
        END IF
#  endif
!
!  Shuffle iDbio3 to make entries with Dout==true appear in front.
!
#  if defined DIAGNOSTICS_BIO_MAPPING_DEBUG
        IF (Master) THEN
          write(out,'(a,99(1x,i3))') 'iDbio3 before:',iDbio3
        END IF
#  endif
        IF (NDb3d_o.lt.NDbio3d) THEN
          ip=1
          iz=NDb3d_o+1
          DO WHILE (ip.le.NDb3d_o)
            DO WHILE (Dout(iDbio3(ip),ng))
              ip=ip+1
            END DO
            DO WHILE (iz.le.NDbio3d.and.(.not.Dout(iDbio3(iz),ng)))
              iz=iz+1
            END DO
            IF (iz.gt.NDbio3d) EXIT
            ! switch
            ifield=iDbio3(ip)
            iDbio3(ip)=iDbio3(iz)
            iDbio3(iz)=ifield
          END DO
        END IF
#  if defined DIAGNOSTICS_BIO_MAPPING_DEBUG
        IF (Master) THEN
          write(out,'(a,99(1x,i3))') 'iDbio3 after: ',iDbio3
        END IF
#  endif
      END DO
# endif /*DIAGNOSTICS_BIO_MAPPING*/
#endif /*DIAGNOSTICS_BIO*/
!
!-----------------------------------------------------------------------
!  Perform various checks.
!-----------------------------------------------------------------------
!
      DO ng=1,Ngrids
!
!  Converted from gud_readtraits.F/darwin_readtraits_checks.h
!

! check types are within type ranges
        DO ip=1,nplank
          IF (isPhoto(ip,ng) .NE. 0) THEN
            IF (ip.GT.nPhoto) THEN
              IF (Master) WRITE(out,'(2A,I4)') 'Error: ',               &
     & 'isPhoto set for type outside of photo range: ', ip
              exit_flag=5
              RETURN
            ENDIF
#if defined DARWIN_CHLQUOTA
            IF (ip.GT.nChl) THEN
              IF (Master) WRITE(out,'(2A,I4)') 'Error: ',               &
     & 'isPhoto set for type outside of chlorophyll variable range: ', ip
              exit_flag=5
              RETURN
            ENDIF
#endif
          ELSE
            PCmax(ip,ng) = 0.0_r8
            Vmax_PO4(ip,ng) = 0.0_r8
            Vmax_NO3(ip,ng) = 0.0_r8
            Vmax_NO2(ip,ng) = 0.0_r8
            Vmax_NH4(ip,ng) = 0.0_r8
            Vmax_FeT(ip,ng) = 0.0_r8
            Vmax_SiO2(ip,ng) = 0.0_r8
          ENDIF

          IF (bactType(ip,ng) .NE. 0) THEN
            IF (ip.LT.iMinBact .OR. ip.GT.iMaxBact) THEN
              IF (Master) WRITE(out,'(2A,I4)') 'Error: ',               &
     & 'bactType set for type outside of bacteria range: ', ip
              exit_flag=5
              RETURN
            ENDIF
            IF (isAerobic(ip,ng).NE.0 .AND. isDenit(ip,ng).NE.0) THEN
              IF (Master) WRITE(out,'(2A,I4)') 'Error: ',               &
     & 'isAerobic and isDenit both set: ', ip
              exit_flag=5
              RETURN
            ENDIF
          ELSE
            IF (isAerobic(ip,ng).NE.0) THEN
              IF (Master) WRITE(out,'(2A,I4)') 'Error: ',               &
     & 'isAerobic set for non-bacteria type: ', ip
              exit_flag=5
              RETURN
            ENDIF
            IF (isDenit(ip,ng).NE.0) THEN
              IF (Master) WRITE(out,'(2A,I4)') 'Error: ',               &
     & 'isDenit set for non-bacteria type: ', ip
              exit_flag=5
              RETURN
            ENDIF
          ENDIF

          DO i=1,nplank
            IF (palat(ip,i,ng) .NE. 0.0_r8) THEN
              IF (ip.LT.iMinPrey .OR. ip.GT.iMaxPrey) THEN
                IF (Master) WRITE(out,'(2A,I4)') ' Warning: ',          &
     & 'palat set for type outside of prey range: ', ip
              ENDIF
              IF (i.LT.iMinPred .OR. i.GT.iMaxPred) THEN
                IF (Master) WRITE(out,'(2A,I4)') 'Warning: ',           &
     & 'palat set for type outside of predator range: ', i
              ENDIF
            ENDIF
          ENDDO
        ENDDO
#if ! defined DARWIN_EXUDE
!
!  Converted from gud_init_fixed.F
!
        DO i=1,nPlank
          IF (ExportFrac(i,ng).ne.0.0_r8) THEN
            If (Master) WRITE(out,'(2a,/a,a)') 'Error: ',               &
     &        'ExportFrac can only be used with DARWIN_EXUDE.',         &
     &        'Use ExportFracMort and ExportFracMort2 ',                &
     &        'for export due to mortality.'
            exit_flag=5
            RETURN
          END IF
        END DO

#endif /* ! DARWIN_EXUDE */
!
!  Converted from gud_check.F
!

! check all required palat are set
        DO i=iMinPred,iMaxPred
          DO is=iMinPrey,iMaxPrey
            IF (palat(is,i,ng) .LT. 0.0_r8) THEN
              IF(Master) WRITE(out,'(a,i0,a,i0,a,i0,a)')                &
     &          'Error: Grid', ng,                                      &
     &          'palat negative for pred ',i,' prey ',is,'.'
              exit_flag=5
              RETURN
            END IF
          END DO
        END DO
! check we are not using hollexp and also inhib_graz or DARWIN_GRAZING_SWITCH
        IF (hollexp(ng).ne.1.0_r8) THEN
          IF (inhib_graz_exp(ng).ne.0) THEN
            IF (Master) WRITE(out,'(3a)') ' read_BioPar - Error: ',     &
     &        'hollexp.ne.1 and inhib_graz_exp ',                       &
     &        'should not be used together.'
            exit_flag=5
            RETURN
          END IF
#if defined DARWIN_GRAZING_SWITCH
          IF (Master) WRITE(out,'(3a)') ' read_BioPar - Error: ',       &
     &      'hollexp.ne.1 and DARWIN_GRAZING_SWITCH ',                  &
     &      'should not be used together.'
          exit_flag=5
          RETURN
#endif

        END IF
!
!  Added new checks.
!
        DO i=1,nplank
          DO is=1,nplank
            IF ((i.lt.iMinPrey.or.i.gt.iMaxPrey.or.                     &
     &           is.lt.iMinPred.or.is.gt.iMaxPred).and.                 &
     &           palat(i,is,ng).ne.0.0_r8) THEN
              IF(Master) WRITE(out,'(a,i0,a,i0,a,i0,a)')                &
     &          'Error: Grid', ng,                                      &
     &          'non-zero palat at unused index (',i,',',is,').'
              exit_flag=5
              RETURN
            END IF
          END DO
        END DO
      END DO

  30  FORMAT (/,' read_BioPar - variable info not yet loaded, ',        &
     &        a,i2.2,a)
  40  FORMAT (/,' read_BioPar - Error while processing line: ',/,a)
  50  FORMAT (/,/,' Darwin Model Parameters, Grid: ',i2.2,              &
     &        /,  ' =================================',/)
  60  FORMAT (1x,i10,2x,a,t32,a)
! x for real, x+1 for int, x+2 for bool
! for x =70,80,140
  70  FORMAT (1p,e11.4,2x,a,t32,a)
  71  FORMAT (1x,i10,2x,a,t32,a)
  72  FORMAT (10x,l1,2x,a,t32,a)
  80  FORMAT (1p,e11.4,2x,a,t32,a,/,t34,a)
  81  FORMAT (1x,i10,2x,a,t32,a,/,t34,a)
  82  FORMAT (10x,l1,2x,a,t32,a,/,t34,a)
  90  FORMAT (1p,e11.4,2x,a,'(',i2.2,')',t32,a,/,t34,a,i2.2,':',1x,a)
 100  FORMAT (10x,l1,2x,a,'(',i2.2,')',t32,a,i2.2,':',1x,a)
 110  FORMAT (10x,l1,2x,a,t32,a,i2.2,':',1x,a)
 120  FORMAT (14x,a,t32,a)
 130  FORMAT (14x,a,t32,a,/,t34,a)
 140  FORMAT (1p,e11.4,t32,a)
 141  FORMAT (1x,i10,t32,a)
 142  FORMAT (10x,l1,t32,a)

      RETURN
      END SUBROUTINE read_BioPar
