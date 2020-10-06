!
!  converted from "gud_tempfunc.F" using darwinfortran_to_romsfortran
!  (written by J. Paul Mattern)
!
#if defined DARWIN_NOTEMP

          photoFun = 1.0_r8
          grazFun = 1.0_r8
          reminFun(i,k) = 1.0_r8
          mortFun(i,k) = 1.0_r8
          mort2Fun(i,k) = 1.0_r8
          uptakeFun(i,k) = 1.0_r8

#elif DARWIN_TEMP_VERSION == 1

! +++++++++++++++++++ VERSION 1 +++++++++++++++++++++++++++++++++++++++
! steph's version 1 (pseudo-Eppley)
! plankton growth function (unitless)
          DO ic = 1, nPhoto
!swd -- this gives Eppley curve only
            photoFun(i,k,ic) = phytoTempExp1(ic,ng)**Bio(i,k,itemp)
# if defined DARWIN_TEMP_RANGE
!swd -- temperature range
            photoFun(i,k,ic) = photoFun(i,k,ic) *exp(-phytoTempExp2(ic,ng)*abs(Bio(i,k,itemp) - phytoTempOptimum(ic,ng))**phytoDecayPower(ic,ng))
# endif
            photoFun(i,k,ic) = photoFun(i,k,ic) - tempnorm(ng)
            photoFun(i,k,ic) = phytoTempCoeff(ic,ng)*max(photoFun(i,k,ic), 1.e-10_r8)
            photoFun(i,k,ic) = min(photoFun(i,k,ic),1.0_r8)
          ENDDO
          DO ic = 1,nplank
! grazFun(i,k,ic) = zooTempCoeff(ic)*EXP(
! & zooTempExp(ic)*(Temp - zooTempOptimum(ic)))
            grazFun(i,k,ic) = 1.0_r8
          ENDDO
          reminFun(i,k) = 1.0_r8
          mortFun(i,k) = 1.0_r8
          mort2Fun(i,k) = 1.0_r8
          uptakeFun(i,k) = 1.0_r8
! ++++++++++++++ END VERSION 1 +++++++++++++++++++++++++++++++++++++++

#elif DARWIN_TEMP_VERSION == 2

! +++++++++++++++++++ VERSION 2 +++++++++++++++++++++++++++++++++++++++
! steph's version 2 (pseudo-Arrenhius)
          Tkel = 273.15_r8
! TempAe = -4000.0_r8
! Tempref = 293.15_r8
! TempCoeff = 0.5882_r8
          DO ic = 1, nPhoto
!swd -- this gives Arrenhius curve only
            photoFun(i,k,ic) = exp(TempAeArr(ng)*(1.0_r8/(Bio(i,k,itemp)+Tkel) -1.0_r8/(TemprefArr(ng)) ) )
# if defined DARWIN_TEMP_RANGE
!swd -- temperature range
            photoFun(i,k,ic) = photoFun(i,k,ic) *exp(-phytoTempExp2(ic,ng)*abs(Bio(i,k,itemp) - phytoTempOptimum(ic,ng))**phytoDecayPower(ic,ng))
# endif
            photoFun(i,k,ic) = photoFun(i,k,ic)
            photoFun(i,k,ic) = TempCoeffArr(ng)*max(photoFun(i,k,ic), 1.e-10_r8)
!
          ENDDO
          reminFun(i,k) = exp(TempAeArr(ng)*(1.0_r8/(Bio(i,k,itemp)+Tkel) -1.0_r8/(TemprefArr(ng)) ) )
          reminFun(i,k) = TempCoeffArr(ng)*max(reminFun(i,k), 1.e-10_r8)
          DO ic = 1, nplank
            grazFun(i,k,ic) = reminFun(i,k)
! grazFun(i,k,ic) = 1.0_r8
          ENDDO
! reminFun(i,k) = 1.0_r8
          mortFun(i,k) = reminFun(i,k)
          mort2Fun(i,k) = reminFun(i,k)
! mortFun(i,k) = 1.0_r8
! mort2Fun(i,k) = 1.0_r8
          uptakeFun(i,k) = 1.0_r8
! ++++++++++++++ END VERSION 2 +++++++++++++++++++++++++++++++++++++++

#elif DARWIN_TEMP_VERSION == 3

! +++++++++++++++++++ VERSION 3 +++++++++++++++++++++++++++++++++++++++
! ben's version 3 from quota model
          TempAe = 0.05_r8
          Tempref = 20.0_r8
          reminFun(i,k) = MAX(1e-10_r8, EXP(TempAe*(Bio(i,k,itemp)-Tempref)))
          DO ic = 1, nplank
            photoFun(i,k,ic) = MAX(1e-10_r8, EXP(TempAe*(Bio(i,k,itemp)-Tempref)))
            grazFun(i,k,ic) = reminFun(i,k)
          ENDDO
          mortFun(i,k) = reminFun(i,k)
          mort2Fun(i,k) = reminFun(i,k)
          uptakeFun(i,k) = reminFun(i,k)
! ++++++++++++++ END VERSION 3 +++++++++++++++++++++++++++++++++++++++

#else
#error "DARWIN_TEMP_VERSION must be 1, 2 or 3"
#endif

#if defined DARWIN_NOZOOTEMP
          grazFun(:) = 1.0_r8
#endif


