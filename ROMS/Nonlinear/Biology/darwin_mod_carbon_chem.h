!
!  converted from "gud_carbon_chem.F" using darwinfortran_to_romsfortran
!  (written by J. Paul Mattern)
!  with manual adaptations
!
!BOP
! !ROUTINE: CALC_PCO2

! !INTERFACE: ==========================================================
       SUBROUTINE DARWIN_CALC_PCO2(                                     &
     & ng,donewt,inewtonmax,ibrackmax,                                  &
     & t,s,diclocal,pt,sit,ta,                                          &
     & k1local,k2local,                                                 &
     & k1plocal,k2plocal,k3plocal,                                      &
     & kslocal,kblocal,kwlocal,                                         &
     & ksilocal,kflocal,                                                &
     & k0local, fugflocal,                                              &
     & fflocal,btlocal,stlocal,ftlocal,                                 &
     & pHlocal,pCO2surfloc)

! !DESCRIPTION:
! surface ocean inorganic carbon chemistry to OCMIP2
! regulations modified from OCMIP2 code;
! Mick Follows, MIT, Oct 1999.0_r8

! Apr 2011: fix vapour bug (following Bennington)


! !USES: ===============================================================
      USE mod_param
      USE mod_biology, only: m3perkg

! == Routine arguments ==
! diclocal = total inorganic carbon (mol/m^3)
! where 1 T = 1 metric ton = 1000 kg
! ta = total alkalinity (eq/m^3)
! pt = inorganic phosphate (mol/^3)
! sit = inorganic silicate (mol/^3)
! t = temperature (degrees C)
! s = salinity (PSU)
        INTEGER,intent(in) :: ng 
        INTEGER,intent(in) :: donewt
        INTEGER,intent(in) :: inewtonmax
        INTEGER,intent(in) :: ibrackmax
        real(r8),intent(in) :: t
        real(r8),intent(in) :: s
        real(r8),intent(inout) :: pt
        real(r8),intent(inout) :: sit
        real(r8),intent(inout) :: ta
        real(r8),intent(out) :: pCO2surfloc
        real(r8),intent(inout) :: diclocal
        real(r8),intent(inout) :: pHlocal
        real(r8),intent(in) :: fflocal
        real(r8),intent(in) :: btlocal
        real(r8),intent(in) :: stlocal
        real(r8),intent(in) :: ftlocal
        real(r8),intent(in) :: k1local
        real(r8),intent(in) :: k2local
        real(r8),intent(in) :: k1plocal
        real(r8),intent(in) :: k2plocal
        real(r8),intent(in) :: k3plocal
        real(r8),intent(in) :: kslocal
        real(r8),intent(in) :: kblocal
        real(r8),intent(in) :: kwlocal
        real(r8),intent(in) :: ksilocal
        real(r8),intent(in) :: kflocal
        real(r8),intent(in) :: k0local
        real(r8),intent(in) :: fugflocal
!EOP

#if defined DARWIN_CARBON

! == Local variables ==
! INPUT
! phlo= lower limit of pH range
! phhi= upper limit of pH range
! atmpres = atmospheric pressure in atmospheres (1 atm==1013.25mbar)
! OUTPUT
! co2star = CO2*water (mol/m^3)
! pco2surf = oceanic pCO2 (ppmv)
! ---------------------------------------------------------------------
! OCMIP NOTE: Some words about units - (JCO, 4/4/1999)
! - Models carry tracers in mol/m^3 (on a per volume basis)
! - Conversely, this routine, which was written by
! observationalists (C. Sabine and R. Key), passes input
! arguments in umol/kg (i.e., on a per mass basis)
! - I have changed things slightly so that input arguments are in
! mol/m^3,
! - Thus, all input concentrations (diclocal, ta, pt, and st) should be
! given in mol/m^3; output arguments "co2star" and "dco2star"
! are likewise be in mol/m^3.0_r8
! ---------------------------------------------------------------------

        real(r8) :: phhi
        real(r8) :: phlo
        real(r8) :: c
        real(r8) :: a
        real(r8) :: a2
        real(r8) :: da
        real(r8) :: b
        real(r8) :: b2
        real(r8) :: db
        real(r8) :: fn
        real(r8) :: df
        real(r8) :: deltax
        real(r8) :: x
        real(r8) :: x2
        real(r8) :: x3
        real(r8) :: xmid
        real(r8) :: ftest
        real(r8) :: htotal
        real(r8) :: htotal2
        real(r8) :: co2star
        real(r8) :: phguess
        real(r8) :: fco2
        INTEGER :: inewton
        INTEGER :: ibrack
        INTEGER :: hstep
        real(r8),dimension(3) :: fni
        real(r8) :: xlo
        real(r8) :: xhi
        real(r8) :: xguess
        real(r8) :: k123p
        real(r8) :: k12p
        real(r8) :: k12
! ---------------------------------------------------------------------
! import donewt flag
! set donewt = 1 for newton-raphson iteration
! set donewt = 0 for bracket and bisection
! ---------------------------------------------------------------------
! Change units from the input of mol/m^3 -> mol/kg:
! (1 mol/m^3) x (1 m^3/1024.5_r8 kg)
! where the ocean mean surface density is 1024.5_r8 kg/m^3
! Note: mol/kg are actually what the body of this routine uses
! for calculations. Units are reconverted back to mol/m^3 at the
! end of this routine.
! ---------------------------------------------------------------------
! To convert input in mol/m^3 -> mol/kg
        pt=pt*m3perkg(ng)
        sit=sit*m3perkg(ng)
        ta=ta*m3perkg(ng)
        diclocal=diclocal*m3perkg(ng)
! ---------------------------------------------------------------------
! set first guess and brackets for [H+] solvers
! first guess (for newton-raphson)
        phguess = phlocal


! bracketing values (for bracket/bisection)
        phhi = 10.0_r8
        phlo = 5.0_r8
! convert to [H+]...
        xguess = 10.0_r8**(-phguess)
        xlo = 10.0_r8**(-phhi)
        xhi = 10.0_r8**(-phlo)
        xmid = (xlo + xhi)*0.5_r8


!----------------------------------------------------------------
! iteratively solve for [H+]
! (i) Newton-Raphson method with fixed number of iterations,
! use previous [H+] as first guess

! select newton-raphson, inewt=1
! else select bracket and bisection

!QQQQQ
        if( donewt .eq. 1)then
!.........................................................
! NEWTON-RAPHSON METHOD
!.........................................................
          x = xguess
!diags
! WRITE(0,*)'xguess ',xguess
!diags
          do inewton = 1, inewtonmax
! set some common combinations of parameters used in
! the iterative [H+] solvers
            x2=x*x
            x3=x2*x
            k12 = k1local*k2local
            k12p = k1plocal*k2plocal
            k123p = k12p*k3plocal
            c = 1.0_r8 + stlocal/kslocal
            a = x3 + k1plocal*x2 + k12p*x + k123p
            a2=a*a
            da = 3.0_r8*x2 + 2.0_r8*k1plocal*x + k12p
            b = x2 + k1local*x + k12
            b2=b*b
            db = 2.0_r8*x + k1local

! Evaluate f([H+]) and f_prime([H+])
! fn = hco3+co3+borate+oh+hpo4+2*po4+silicate+hfree
! +hso4+hf+h3po4-ta
            fn = k1local*x*diclocal/b +                                 &
     & 2.0_r8*diclocal*k12/b +                                          &
     & btlocal/(1.0_r8 + x/kblocal) +                                   &
     & kwlocal/x +                                                      &
     & pt*k12p*x/a +                                                    &
     & 2.0_r8*pt*k123p/a +                                              &
     & sit/(1.0_r8 + x/ksilocal) -                                      &
     & x/c -                                                            &
     & stlocal/(1.0_r8 + kslocal/x/c) -                                 &
     & ftlocal/(1.0_r8 + kflocal/x) -                                   &
     & pt*x3/a -                                                        &
     & ta

! df = dfn/dx
!diags
! WRITE(0,*)'values',b2,kblocal,x2,a2,c,x
!diags
            df = ((k1local*diclocal*b) - k1local*x*diclocal*db)/b2 -    &
     & 2.0_r8*diclocal*k12*db/b2 -                                      &
     & btlocal/kblocal/(1.0_r8+x/kblocal)**2.0_r8 -                     &
     & kwlocal/x2 +                                                     &
     & (pt*k12p*(a - x*da))/a2 -                                        &
     & 2.0_r8*pt*k123p*da/a2 -                                          &
     & sit/ksilocal/(1.0_r8+x/ksilocal)**2.0_r8 +                       &
     & 1.0_r8/c +                                                       &
     & stlocal*(1.0_r8 + kslocal/x/c)**(-2.0_r8)*(kslocal/c/x2) +       &
     & ftlocal*(1.0_r8 + kflocal/x)**(-2.0_r8)*kflocal/x2 -             &
     & pt*x2*(3.0_r8*a-x*da)/a2
! evaluate increment in [H+]
            deltax = - fn/df
! update estimate of [H+]
            x = x + deltax
!diags
! write value of x to check convergence....
! write(0,*)'inewton, x, deltax ',inewton, x, deltax
! write(6,*)
!diags

          end do
! end of newton-raphson method
!....................................................
        else
!....................................................
! BRACKET AND BISECTION METHOD
!....................................................
! (ii) If first step use Bracket and Bisection method
! with fixed, large number of iterations
          do ibrack = 1, ibrackmax
            do hstep = 1,3
              if(hstep .eq. 1)x = xhi
              if(hstep .eq. 2)x = xlo
              if(hstep .eq. 3)x = xmid
! set some common combinations of parameters used in
! the iterative [H+] solvers


              x2=x*x
              x3=x2*x
              k12 = k1local*k2local
              k12p = k1plocal*k2plocal
              k123p = k12p*k3plocal
              c = 1.0_r8 + stlocal/kslocal
              a = x3 + k1plocal*x2 + k12p*x + k123p
              a2=a*a
              da = 3.0_r8*x2 + 2.0_r8*k1plocal*x + k12p
              b = x2 + k1local*x + k12
              b2=b*b
              db = 2.0_r8*x + k1local
! evaluate f([H+]) for bracketing and mid-value cases
              fn = k1local*x*diclocal/b +                               &
     & 2.0_r8*diclocal*k12/b +                                          &
     & btlocal/(1.0_r8 + x/kblocal) +                                   &
     & kwlocal/x +                                                      &
     & pt*k12p*x/a +                                                    &
     & 2.0_r8*pt*k123p/a +                                              &
     & sit/(1.0_r8 + x/ksilocal) -                                      &
     & x/c -                                                            &
     & stlocal/(1.0_r8 + kslocal/x/c) -                                 &
     & ftlocal/(1.0_r8 + kflocal/x) -                                   &
     & pt*x3/a -                                                        &
     & ta
              fni(hstep) = fn
            end do
! now bracket solution within two of three
            ftest = fni(1)/fni(3)
            if(ftest .gt. 0.0_r8)then
              xhi = xmid
            else
              xlo = xmid
            end if
            xmid = (xlo + xhi)*0.5_r8

!diags
! write value of x to check convergence....
! WRITE(0,*)'bracket-bisection iteration ',ibrack, xmid
!diags
          end do
! last iteration gives value
          x = xmid
! end of bracket and bisection method
!....................................
        end if
! iterative [H+] solver finished
!----------------------------------------------------------------

! now determine pCO2 etc...
! htotal = [H+], hydrogen ion conc
        htotal = x
! Calculate [CO2*] as defined in DOE Methods Handbook 1994 Ver.2,
! ORNL/CDIAC-74, dickson and Goyet, eds. (Ch 2 p 10, Eq A.49)
        htotal2=htotal*htotal
        co2star=diclocal*htotal2/(htotal2 + k1local*htotal              &
     & + k1local*k2local)
        phlocal=-log10(htotal)

! ---------------------------------------------------------------
! Add two output arguments for storing pCO2surf
! Should we be using K0 or ff for the solubility here?
! ---------------------------------------------------------------
        fco2 = co2star / k0local
        pCO2surfloc = fco2/fugflocal

! ----------------------------------------------------------------
! Reconvert units back to original values for input arguments
! no longer necessary????
! ----------------------------------------------------------------
! Reconvert from mol/kg -> mol/m^3
        pt=pt/m3perkg(ng)
        sit=sit/m3perkg(ng)
        ta=ta/m3perkg(ng)
        diclocal=diclocal/m3perkg(ng)

#endif

        RETURN
        END

!---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

!BOP
! !ROUTINE: CALC_PCO2_APPROX

! !INTERFACE: ==========================================================
       SUBROUTINE DARWIN_CALC_PCO2_APPROX(                              &
     & ng,t,s,                                                          &
#if defined DARWIN_VERBOSE_PLANK_OLD
     & i,j,k,                                                           &
#endif
     & diclocal,pt,sit,ta,                                              &
     & k1local,k2local,                                                 &
     & k1plocal,k2plocal,k3plocal,                                      &
     & kslocal,kblocal,kwlocal,                                         &
     & ksilocal,kflocal,                                                &
     & k0local, fugflocal,                                              &
     & fflocal,btlocal,stlocal,ftlocal,                                 &
     & pHlocal,pCO2surfloc,co3local)

! !DESCRIPTION:
! *==========================================================*
! | SUBROUTINE DARWIN_CALC_PCO2_APPROX |
! *==========================================================*
! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! C New efficient pCO2 solver, Mick Follows CC
! C Taka Ito CC
! C Stephanie Dutkiewicz CC
! C 20 April 2003 CC
! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! Apr 2011: fix vapour bug (following Bennington)
! Oct 2011: add CO3 extimation and pass out

! !USES: ===============================================================
      USE mod_param
      USE mod_biology, only: m3perkg

! == GLobal variables ==

! == Routine arguments ==
! diclocal = total inorganic carbon (mol/m^3)
! where 1 T = 1 metric ton = 1000 kg
! ta = total alkalinity (eq/m^3)
! pt = inorganic phosphate (mol/^3)
! sit = inorganic silicate (mol/^3)
! t = temperature (degrees C)
! s = salinity (PSU)
        INTEGER,intent(in) :: ng 
        real(r8),intent(in) :: t
        real(r8),intent(in) :: s
#if defined DARWIN_VERBOSE_PLANK_OLD
        INTEGER,intent(in) :: i,j,k
#endif
        real(r8),intent(inout) :: pt
        real(r8),intent(inout) :: sit
        real(r8),intent(inout) :: ta
        real(r8),intent(out) :: pCO2surfloc
        real(r8),intent(inout) :: diclocal
        real(r8),intent(inout) :: pHlocal
        real(r8),intent(in) :: fflocal
        real(r8),intent(in) :: btlocal
        real(r8),intent(in) :: stlocal
        real(r8),intent(in) :: ftlocal
        real(r8),intent(in) :: k1local
        real(r8),intent(in) :: k2local
        real(r8),intent(in) :: k1plocal
        real(r8),intent(in) :: k2plocal
        real(r8),intent(in) :: k3plocal
        real(r8),intent(in) :: kslocal
        real(r8),intent(in) :: kblocal
        real(r8),intent(in) :: kwlocal
        real(r8),intent(in) :: ksilocal
        real(r8),intent(in) :: kflocal
        real(r8),intent(in) :: k0local
        real(r8),intent(in) :: fugflocal
        real(r8),intent(out) :: co3local
!EOP

#if defined DARWIN_CARBON

! == Local variables ==
        real(r8) :: phguess
        real(r8) :: cag
        real(r8) :: bohg
        real(r8) :: hguess
        real(r8) :: stuff
        real(r8) :: gamm
        real(r8) :: hnew
        real(r8) :: co2s
        real(r8) :: h3po4g
        real(r8) :: h2po4g
        real(r8) :: hpo4g
        real(r8) :: po4g
        real(r8) :: siooh3g
        real(r8) :: fco2


! ---------------------------------------------------------------------
! Change units from the input of mol/m^3 -> mol/kg:
! (1 mol/m^3) x (1 m^3/1024.5_r8 kg)
! where the ocean mean surface density is 1024.5_r8 kg/m^3
! Note: mol/kg are actually what the body of this routine uses
! for calculations. Units are reconverted back to mol/m^3 at the
! end of this routine.
! To convert input in mol/m^3 -> mol/kg
        pt=pt*m3perkg(ng)
        sit=sit*m3perkg(ng)
        ta=ta*m3perkg(ng)
        diclocal=diclocal*m3perkg(ng)
! ---------------------------------------------------------------------
! set first guess and brackets for [H+] solvers
! first guess (for newton-raphson)
        phguess = phlocal
!mick - new approx method
!mick - make estimate of htotal (hydrogen ion conc) using
!mick appromate estimate of CA, carbonate alkalinity
        hguess = 10.0_r8**(-phguess)
!mick - first estimate borate contribution using guess for [H+]
        bohg = btlocal*kblocal/(hguess+kblocal)

!mick - first estimate of contribution from phosphate
!mick based on Dickson and Goyet
        stuff = hguess*hguess*hguess                                    &
     & + (k1plocal*hguess*hguess)                                       &
     & + (k1plocal*k2plocal*hguess)                                     &
     & + (k1plocal*k2plocal*k3plocal)
        h3po4g = (pt*hguess*hguess*hguess) / stuff
        h2po4g = (pt*k1plocal*hguess*hguess) / stuff
        hpo4g = (pt*k1plocal*k2plocal*hguess) / stuff
        po4g = (pt*k1plocal*k2plocal*k3plocal) / stuff

!mick - estimate contribution from silicate
!mick based on Dickson and Goyet
        siooh3g = sit*ksilocal / (ksilocal + hguess)

!mick - now estimate carbonate alkalinity
        cag = ta - bohg - (kwlocal/hguess) + hguess                     &
     & - hpo4g - 2.0_r8*po4g + h3po4g                                   &
     & - siooh3g

!mick - now evaluate better guess of hydrogen ion conc
!mick htotal = [H+], hydrogen ion conc
        gamm = diclocal/cag
        stuff = (1.0_r8-gamm)*(1.0_r8-gamm)*k1local*k1local             &
     & - 4.0_r8*k1local*k2local*(1.0_r8-2.0_r8*gamm)
        hnew = 0.5_r8*( (gamm-1.0_r8)*k1local + sqrt(stuff) )
!mick - now determine [CO2*]
#if defined DARWIN_VERBOSE_PLANK_OLD
            IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,'(a,1x,a16,2x,10(f,1x,a,1x))') 'CALC_PCO2_APPROX','cag=',ta ,'-', bohg ,'-(',kwlocal,'/',hguess,') + ',hguess ,'-', hpo4g ,'- 2.0_r8*',po4g ,'+', h3po4g ,'-', siooh3g 
              write(*,'(a,1x,a16,2x,10(f,1x,a,1x))') 'CALC_PCO2_APPROX','gamm=',diclocal,'/',cag 
              write(*,'(a,1x,a16,2x,10(f,1x,a,1x))') 'CALC_PCO2_APPROX','hnew= 0.5 *((',gamm,'- 1.0) *',k1local,' + sqrt(',stuff,')' 
              write(*,'(a,1x,a16,2x,10(f,1x,a,1x))') 'CALC_PCO2_APPROX','co2s=', diclocal , '/(1+', k1local,'/',hnew,'+(',k1local,'*',k2local,')/',hnew,'**2'
            END IF
#endif
        co2s = diclocal/                                                &
     & (1.0_r8 + (k1local/hnew) + (k1local*k2local/(hnew*hnew)))
!mick - return update pH to main routine
        phlocal = -log10(MAX(hnew, 1e-14_r8))

! NOW EVALUATE CO32-, carbonate ion concentration
! used in determination of calcite compensation depth
! Karsten Friis & Mick - Sep 2004
        co3local = k1local*k2local*diclocal /                           &
     & (hnew*hnew + k1local*hnew + k1local*k2local)

! ---------------------------------------------------------------
! surface pCO2 (following Dickson and Goyet, DOE...)
        fco2 = co2s/k0local
#if defined DARWIN_VERBOSE_PLANK_OLD
            IF(k==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,'(a,1x,a16,2x,10(f,1x,a,1x))') 'CALC_PCO2_APPROX','fco2=', co2s , '/', k0local
              write(*,'(a,1x,a16,2x,10(f,1x,a,1x))') 'CALC_PCO2_APPROX','pco2surfloc=', fco2 , '/', fugflocal
            END IF
#endif
        pco2surfloc = fco2/fugflocal

! ----------------------------------------------------------------
! Reconvert from mol/kg -> mol/m^3
        pt=pt/m3perkg(ng)
        sit=sit/m3perkg(ng)
        ta=ta/m3perkg(ng)
        diclocal=diclocal/m3perkg(ng)

#endif

        RETURN
        END

!---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
!BOP
! !ROUTINE: CARBON_COEFFS
! !INTERFACE: ==========================================================
      SUBROUTINE DARWIN_CARBON_COEFFS(                                  &
     & ttemp,stemp,drF,                                                 &
     & fugf,ff,ak0,ak1,ak2,akb,ak1p,ak2p,ak3p,aksi,akw,aks,             &
     & akf,bt,st,ft,Ksp_TP_Calc,                                        &
     & Istr,Iend,                                                       &
#if defined DARWIN_VERBOSE_PLANK_OLD
     & i,j,                                                             &
#endif
     & kLevel,ksurface)

! !DESCRIPTION:
! *==========================================================*
! | SUBROUTINE DARWIN_CARBON_COEFFS |
! | determine coefficients for surface carbon chemistry |
! | adapted from OCMIP2: SUBROUTINE CO2CALC |
! | mick follows, oct 1999 |
! | minor changes to tidy, swd aug 2002 |
! *==========================================================*
! INPUT
! diclocal = total inorganic carbon (mol/m^3)
! where 1 T = 1 metric ton = 1000 kg
! ta = total alkalinity (eq/m^3)
! pt = inorganic phosphate (mol/^3)
! sit = inorganic silicate (mol/^3)
! t = temperature (degrees C)
! s = salinity (PSU)
! OUTPUT
! IMPORTANT: Some words about units - (JCO, 4/4/1999)
! - Models carry tracers in mol/m^3 (on a per volume basis)
! - Conversely, this routine, which was written by observationalists
! (C. Sabine and R. Key), passes input arguments in umol/kg
! (i.e., on a per mass basis)
! - I have changed things slightly so that input arguments are in mol/m^3,
! - Thus, all input concentrations (diclocal, ta, pt, and st) should be
! given in mol/m^3; output arguments "co2star" and "dco2star"
! are likewise be in mol/m^3.0_r8
!
! Apr 2011: fix vapour bug (following Bennington)
! Oct 2013: c NOW INCLUDES:
! PRESSURE DEPENDENCE of K1, K2, solubility product of calcite
! based on Takahashi, GEOSECS Atlantic Report, Vol. 1 (1981)
!--------------------------------------------------------------------------

! !USES: ===============================================================
        USE mod_param
! == GLobal variables ==
! == Routine arguments ==
! ttemp and stemp are local theta and salt arrays
! dont really need to pass T and S in, could use theta, salt in
! common block in DYNVARS.h, but this way keeps subroutine more
! general
        real(r8),intent(in) :: ttemp
        real(r8),intent(in) :: stemp
        real(r8),intent(in),dimension(1:ksurface) :: drF
        !real(r8),intent(in),dimension(1-OLx:sNx+OLx,1-OLy:sNy+OLy) :: ttemp
        !real(r8),intent(in),dimension(1-OLx:sNx+OLx,1-OLy:sNy+OLy) :: stemp
        real(r8),intent(out) :: fugf
        real(r8),intent(out) :: ff
        real(r8),intent(out) :: ak0
        real(r8),intent(out) :: ak1
        real(r8),intent(out) :: ak2
        real(r8),intent(out) :: akb
        real(r8),intent(out) :: ak1p
        real(r8),intent(out) :: ak2p
        real(r8),intent(out) :: ak3p
        real(r8),intent(out) :: aksi
        real(r8),intent(out) :: akw
        real(r8),intent(out) :: aks
        real(r8),intent(out) :: akf
        real(r8),intent(out) :: bt
        real(r8),intent(out) :: st
        real(r8),intent(out) :: ft
        real(r8),intent(out) :: Ksp_TP_Calc
        INTEGER,intent(in) :: Istr
        INTEGER,intent(in) :: Iend
#if defined DARWIN_VERBOSE_PLANK_OLD
        INTEGER,intent(in) :: i,j
#endif
        INTEGER,intent(in) :: kLevel
        INTEGER,intent(in) :: ksurface
!EOP

#if defined DARWIN_CARBON

! LOCAL VARIABLES
        real(r8) :: t
        real(r8) :: s
        real(r8) :: tk
        real(r8) :: tk100
        real(r8) :: tk1002
        real(r8) :: dlogtk
        real(r8) :: sqrtis
        real(r8) :: sqrts
        real(r8) :: s15
        real(r8) :: scl
        real(r8) :: s2
        real(r8) :: invtk
        real(r8) :: is
        real(r8) :: is2
! add pressure dependency
        real(r8) :: bdepth
        real(r8) :: cdepth
        real(r8) :: pressc
! calcite stuff
        real(r8) :: Ksp_T_Calc
        real(r8) :: xvalue
        real(r8) :: zdum
        real(r8) :: tmpa1
        real(r8) :: tmpa2
        real(r8) :: tmpa3
        real(r8) :: logKspc
        real(r8) :: dv
        real(r8) :: dk
        real(r8) :: pfactor
        real(r8) :: bigR
! add Bennington
        real(r8) :: P1atm
        real(r8) :: Rgas
        real(r8) :: RT
        real(r8) :: delta
        real(r8) :: B1
        real(r8) :: B
        INTEGER :: k

!.....................................................................
! OCMIP note:
! Calculate all constants needed to convert between various measured
! carbon species. References for each equation are noted in the code.
! Once calculated, the constants are
! stored and passed in the common block "const". The original version
! of this code was based on the code by dickson in Version 2 of
! Handbook of Methods C for the Analysis of the Various Parameters of
! the Carbon Dioxide System in Seawater , DOE, 1994 (SOP No. 3, p25-26).
!....................................................................

! determine pressure (bar) from depth
! 1 BAR at z=0m (atmos pressure)
! use UPPER surface of cell so top layer pressure = 0 bar
! for surface exchange coeffs

! if surface, calculate at interface pressure,
! else calculate at mid-depth pressure
        ! orig code:
        ! if (Klevel.gt.1) then
        if (Klevel.lt.ksurface) then
         bdepth = 0.0_r8
         cdepth = 0.0_r8
         pressc = 1.01325_r8
         ! orig code: (surface to Klevel)
         ! do k = 1,Klevel
         do k = ksurface,Klevel,-1
             ! for drF (delta r_F) see:
             ! http://mitgcm.org/sealion/online_documents/node48.html
             ! orig code:
             ! cdepth = bdepth + 0.5_r8*drF(k)
             ! bdepth = bdepth + drF(k)
             cdepth = bdepth + 0.5_r8*drF(k)
             bdepth = bdepth + drF(k)
             pressc = 1.0_r8 + 0.1_r8*cdepth
         end do
        else
          pressc = 1.01325_r8
        endif
        
          !in ROMS, no grid cells with 0 volume
          !if (hFacC(kLevel).gt.0._r8) then
           t = ttemp
           s = stemp
! terms used more than once
           tk = 273.15_r8 + t
           tk100 = tk/100.0_r8
           tk1002=tk100*tk100
           invtk=1.0_r8/tk
           dlogtk=log(tk)
           is=19.924_r8*s/(1000.0_r8-1.005_r8*s)
           is2=is*is
           sqrtis=sqrt(is)
           s2=s*s
           sqrts=sqrt(s)
           s15=s**1.5_r8
           scl=s/1.80655_r8
! -----------------------------------------------------------------------
! added by Val Bennington Nov 2010
! Fugacity Factor needed for non-ideality in ocean
! ff used for atmospheric correction for water vapor and pressure
! Weiss (1974) Marine Chemistry
           P1atm = pressc ! bars
           Rgas = 83.1451_r8 ! bar*cm3/(mol*K)
           RT = Rgas*tk
           delta = (57.7_r8 - 0.118_r8*tk)
           B1 = -1636.75_r8 + 12.0408_r8*tk - 0.0327957_r8*tk*tk
           B = B1 + 3.16528_r8*tk*tk*tk*(0.00001_r8)
           fugf = exp( (B+2.0_r8*delta) * P1atm / RT)
!------------------------------------------------------------------------
! f = k0(1-pH2O)*correction term for non-ideality
! Weiss & Price (1980, Mar. Chem., 8, 347-359; Eq 13 with table 6 values)
           ff = exp(-162.8301_r8 + 218.2968_r8/tk100 +             &
     & 90.9241_r8*log(tk100) - 1.47696_r8*tk1002 +                      &
     & s * (.025695_r8 - .025225_r8*tk100 +                             &
     & 0.0049867_r8*tk1002))
!------------------------------------------------------------------------
! K0 from Weiss 1974
           ak0 = exp(93.4517_r8/tk100 - 60.2409_r8 +               &
     & 23.3585_r8 * log(tk100) +                                        &
     & s * (0.023517_r8 - 0.023656_r8*tk100 +                           &
     & 0.0047036_r8*tk1002))
#if defined DARWIN_VERBOSE_PLANK_OLD
            IF(Klevel==DARWIN_VERBOSE_K.and.i==DARWIN_VERBOSE_I.and.j==DARWIN_VERBOSE_J) THEN
              write(*,'(a,1x,a16,2x,10(f,1x,a,1x))') 'CARBON_COEFFS',   'ak0=',ak0,', tk100=',tk100,', tk1002=',tk1002,', s=',s
            END IF
#endif
!------------------------------------------------------------------------
! k1 = [H][HCO3]/[H2CO3]
! k2 = [H][CO3]/[HCO3]
! Millero p.664 (1995) using Mehrbach et al. data on seawater scale
           ak1=10.0_r8**(-1._r8*(3670.7_r8*invtk -                 &
     & 62.008_r8 + 9.7944_r8*dlogtk -                                   &
     & 0.0118_r8 * s + 0.000116_r8*s2))
           ak2=10.0_r8**(-1._r8*(1394.7_r8*invtk+ 4.777_r8-        &
     & 0.0184_r8*s + 0.000118_r8*s2))
!
! NOW PRESSURE DEPENDENCE:
! Following Takahashi (1981) GEOSECS report - quoting Culberson and
! Pytkowicz (1968)
           if (kLevel.ne.ksurface) then
! pressc = pressure in bars
           ak1 = ak1*                                         &
     & exp( (24.2_r8-0.085_r8*t)*(pressc-1.0_r8)/(83.143_r8*tk) )
! FIRST GO FOR K2: According to GEOSECS (1982) report
! ak2 = ak2*
! & exp( (26.4_r8-0.040_r8*t)*(pressc-1.0_r8)/(83.143_r8*tk) )
! SECOND GO FOR K2: corrected coeff according to CO2sys documentation
! E. Lewis and D. Wallace (1998) ORNL/CDIAC-105
           ak2 = ak2*                                         &
     & exp( (16.4_r8-0.040_r8*t)*(pressc-1.0_r8)/(83.143_r8*tk) )
           endif
!------------------------------------------------------------------------
! kb = [H][BO2]/[HBO2]
! Millero p.669 (1995) using data from dickson (1990)
           akb=exp((-8966.90_r8- 2890.53_r8*sqrts -                &
     & 77.942_r8*s + 1.728_r8*s15 - 0.0996_r8*s2)*invtk +               &
     & (148.0248_r8 + 137.1942_r8*sqrts + 1.62142_r8*s) +               &
     & (-24.4344_r8 - 25.085_r8*sqrts - 0.2474_r8*s) *                  &
     & dlogtk + 0.053105_r8*sqrts*tk)
           if (kLevel.ne.ksurface) then
! Mick and Karsten - Dec 04
! ADDING pressure dependence based on Millero (1995), p675
! with additional info from CO2sys documentation (E. Lewis and
! D. Wallace, 1998 - see endnotes for commentary on Millero, 95)
            bigR = 83.145_r8
            dv = -29.48_r8 + 0.1622_r8*t + 2.608d-3*t*t
            dk = -2.84d-3
            pfactor = - (dv/(bigR*tk))*pressc                        &
     & + (0.5_r8*dk/(bigR*tk))*pressc*pressc
            akb = akb*exp(pfactor)
           endif
!------------------------------------------------------------------------
! k1p = [H][H2PO4]/[H3PO4]
! DOE(1994) eq 7.2_r8.20 with footnote using data from Millero (1974)
           ak1p = exp(-4576.752_r8*invtk + 115.525_r8 -            &
     & 18.453_r8*dlogtk +                                               &
     & (-106.736_r8*invtk + 0.69171_r8)*sqrts +                         &
     & (-0.65643_r8*invtk - 0.01844_r8)*s)
!------------------------------------------------------------------------
! k2p = [H][HPO4]/[H2PO4]
! DOE(1994) eq 7.2_r8.23 with footnote using data from Millero (1974))
           ak2p = exp(-8814.715_r8*invtk + 172.0883_r8 -           &
     & 27.927_r8*dlogtk +                                               &
     & (-160.340_r8*invtk + 1.3566_r8) * sqrts +                        &
     & (0.37335_r8*invtk - 0.05778_r8) * s)
!------------------------------------------------------------------------
! k3p = [H][PO4]/[HPO4]
! DOE(1994) eq 7.2_r8.26 with footnote using data from Millero (1974)
           ak3p = exp(-3070.75_r8*invtk - 18.141_r8 +              &
     & (17.27039_r8*invtk + 2.81197_r8) *                               &
     & sqrts + (-44.99486_r8*invtk - 0.09984_r8) * s)
!------------------------------------------------------------------------
! ksi = [H][SiO(OH)3]/[Si(OH)4]
! Millero p.671 (1995) using data from Yao and Millero (1995)
           aksi = exp(-8904.2_r8*invtk + 117.385_r8 -              &
     & 19.334_r8*dlogtk +                                               &
     & (-458.79_r8*invtk + 3.5913_r8) * sqrtis +                        &
     & (188.74_r8*invtk - 1.5998_r8) * is +                             &
     & (-12.1652_r8*invtk + 0.07871_r8) * is2 +                         &
     & log(1.0_r8-0.001005_r8*s))
!------------------------------------------------------------------------
! kw = [H][OH]
! Millero p.670 (1995) using composite data
           akw = exp(-13847.26_r8*invtk + 148.9652_r8 -            &
     & 23.6521_r8*dlogtk +                                              &
     & (118.67_r8*invtk - 5.977_r8 + 1.0495_r8 * dlogtk)                &
     & * sqrts - 0.01615_r8 * s)
!------------------------------------------------------------------------
! ks = [H][SO4]/[HSO4]
! dickson (1990, J. chem. Thermodynamics 22, 113)
           aks=exp(-4276.1_r8*invtk + 141.328_r8 -                 &
     & 23.093_r8*dlogtk +                                               &
     & (-13856._r8*invtk + 324.57_r8 - 47.986_r8*dlogtk)*sqrtis+        &
     & (35474.0_r8*invtk - 771.54_r8 + 114.723_r8*dlogtk)*is -          &
     & 2698.0_r8*invtk*is**1.5_r8 + 1776.0_r8*invtk*is2 +               &
     & log(1.0_r8 - 0.001005_r8*s))
!------------------------------------------------------------------------
! kf = [H][F]/[HF]
! dickson and Riley (1979) -- change pH scale to total
           akf=exp(1590.2_r8*invtk - 12.641_r8 +                   &
     & 1.525_r8*sqrtis + log(1.0_r8 - 0.001005_r8*s) +                  &
     & log(1.0_r8 + (0.1400_r8/96.062_r8)*(scl)/aks))
!------------------------------------------------------------------------
! Calculate concentrations for borate, sulfate, and fluoride
! Uppstrom (1974)
           bt = 0.000232_r8 * scl/10.811_r8
! Morris & Riley (1966)
           st = 0.14_r8 * scl/96.062_r8
! Riley (1965)
           ft = 0.000067_r8 * scl/18.9984_r8
!------------------------------------------------------------------------
! solubility product for calcite
!
! Following Takahashi (1982) GEOSECS handbook
! NOT SURE THIS IS WORKING???
! Ingle et al. (1973)
! Ksp_T_Calc = ( -34.452_r8 - 39.866_r8*(s**0.333333_r8)
! & + 110.21_r8*log(s) - 7.5752d-6 * (tk**2.0_r8)
! & ) * 1.0d-7
! with pressure dependence Culberson and Pytkowicz (1968)
! xvalue = (36-0.20_r8*t)*(pressc-1.0_r8)/(83.143_r8*tk)
! Ksp_TP_Calc = Ksp_T_Calc*exp(xvalue)
!
!
! Following Mucci (1983) - from Zeebe/Wolf-Gladrow equic.m
         tmpa1 = - 171.9065_r8 - (0.077993_r8*tk) + (2839.319_r8/tk)    &
     & + (71.595_r8*log10(tk))
         tmpa2 = +(-0.77712_r8 + (0.0028426_r8*tk) + (178.34_r8/tk) )*sqrts
         tmpa3 = -(0.07711_r8*s) + (0.0041249_r8*s15)
         logKspc = tmpa1 + tmpa2 + tmpa3
         Ksp_T_Calc = 10.0_r8**logKspc
! write(6,*)i,k,tmpa1,tmpa2,tmpa3,logkspc,Ksp_T_Calc
! with pressure dependence Culberson and Pytkowicz (1968)
! xvalue = (36.0_r8-0.20_r8*t)*(pressc-1.0_r8)/(83.143_r8*tk)
! Ksp_TP_Calc = Ksp_T_Calc*exp(xvalue)

! alternative pressure depdendence
! following Millero (1995) but using info from Appendix A11 of
! Zeebe and Wolf-Gladrow (2001) book
! dv = -48.6_r8 - 0.5304_r8*t
! dk = -11.76d-3 - 0.3692_r8*t
! xvalue = - (dv/(bigR*tk))*pressc
! & + (0.5_r8*dk/(bigR*tk))*pressc*pressc
! Ksp_TP_Calc = Ksp_T_Calc*exp(xvalue)

! alternative pressure dependence from Ingle (1975)

           zdum = (pressc*10.0d0 - 10.0d0)/10.0d0
           xvalue = ( (48.8d0 - 0.53d0*t)*zdum                          &
     & + (-0.00588d0 + 0.0001845d0*t)*zdum*zdum)                        &
     & / (188.93d0*(t + 273.15d0))

           Ksp_TP_Calc = Ksp_T_Calc*10**(xvalue)

!------------------------------------------------------------------------
         !else
! add Bennington
         !   fugf=0.0_r8
         !   ff=0.0_r8
         !   ak0= 0.0_r8
         !   ak1= 0.0_r8
         !   ak2= 0.0_r8
         !   akb= 0.0_r8
         !   ak1p = 0.0_r8
         !   ak2p = 0.0_r8
         !   ak3p = 0.0_r8
         !   aksi = 0.0_r8
         !   akw = 0.0_r8
         !   aks= 0.0_r8
         !   akf= 0.0_r8
         !   bt = 0.0_r8
         !   st = 0.0_r8
         !   ft = 0.0_r8
         !   Ksp_TP_Calc = 0.0_r8
         !endif

#endif

        RETURN
        END

