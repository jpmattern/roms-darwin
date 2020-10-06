!
!  converted from "gud_readtraits.F" using darwinfortran_to_romsfortran
!  (written by J. Paul Mattern)
!
! DARWIN_DEPENDENT_TRAITS
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


!COG[[[end]]] (checksum: 056ae7f9cc78f111910c1b44e7fb7910)

!COG[[[cog
!COGfor param in_ denomparams:
!COG cog.out('''
!COG {param} = {param}/{param}_denom
!COG'''.format(param=param)[1:])
!COG]]]
!COG[[[end]]] (checksum: d41d8cd98f00b204e9800998ecf8427e)

      DO ip = 1,nPlank
        Vmax_NH4(ip,ng) = Vmax_NH4(ip,ng) * useNH4(ip,ng)
        Vmax_NO2(ip,ng) = Vmax_NO2(ip,ng) * useNO2(ip,ng)
        Vmax_NO3(ip,ng) = Vmax_NO3(ip,ng) * useNO3(ip,ng)
#if ! defined DARWIN_NQUOTA
        IF (diazo(ip,ng).NE.0) THEN
          useNH4(ip,ng) = 0
          useNO2(ip,ng) = 0
          useNO3(ip,ng) = 0
        ENDIF
#endif
        IF (useNO3(ip,ng).EQ.0 .OR. useNO2(ip,ng).EQ.0) THEN
          combNO(ip,ng)=0
        ENDIF
      ! Silicate parameters to zero for non-diatoms
        IF (hasSi(ip,ng) .EQ. 0) THEN
          Vmax_SiO2(ip,ng) = 0.0_r8
          ksatSiO2(ip,ng) = 0.0_r8
          R_SiC(ip,ng) = 0.0_r8
        ENDIF
      ! only Coccolithophores have PIC
        IF (hasPIC(ip,ng) .EQ. 0) THEN
          R_PICPOC(ip,ng) = 0.0_r8
        ENDIF
      ENDDO
      
      
      mortTempFuncMin(:,ng) = MIN(1.0_r8, 1.0_r8 - tempMort(:,ng))
      mort2TempFuncMin(:,ng) = MIN(1.0_r8, 1.0_r8 - tempMort2(:,ng))
      

#if defined DARWIN_RADTRANS
      
      DO ip = 1, nPhoto
       alpha_mean(ip,ng) = 0.0_r8
       DO l = 1, nlam
        alphachl(ip,l,ng) = mQyield(ip,ng) * aphy_chl_ps(ip,l,ng)
        alpha_mean(ip,ng) = alpha_mean(ip,ng) + wb_width(l)*alphachl(ip,l,ng)
       ENDDO
       alpha_mean(ip,ng) = alpha_mean(ip,ng)/wb_totalWidth
      ENDDO
#else
      DO ip = 1, nPhoto
#if defined DARWIN_VERBOSE_PLANK_OLD
        IF(Master) THEN
          write(*,'(a,i2,1x,a16,2x,10(f,1x,a,1x))') 'PLANK', ic_+ip-1,  'INI alphachl=', mQyield(ip,ng), '*', aphy_chl_ave(ng)
        END IF
#endif
       alphachl(ip,1,ng) = mQyield(ip,ng) * aphy_chl_ave(ng)
       alpha_mean(ip,ng) = alphachl(ip,1,ng)
      ENDDO
#endif /* DARWIN_RADTRANS */

#if defined DARWIN_RADTRANS
      DO ip = 1, nplank
       chl2cmin(ip,ng)=chl2cmax(ip,ng)/                                 &
     & (1+(chl2cmax(ip,ng)* alpha_mean(ip,ng) *2000.0_r8)/(2*PCmax(ip,ng)))
      ENDDO
#else
      DO ip = 1, nPhoto
       chl2cmin(ip,ng) = 0.0_r8
      ENDDO
#endif

#if ! defined DARWIN_GEIDER
      DO ip = 1, nplank
       IF (ksatPAR(ip,ng)*kinhPAR(ip,ng) .GT. 0.0_r8) THEN
        normI(ip,ng) =                                                  &
     &    1.0_r8/(ksatPAR(ip,ng)/(ksatPAR(ip,ng)+kinhPAR(ip,ng))*       &
     & EXP(kinhPAR(ip,ng)/ksatPAR(ip,ng)*                               &
     & LOG(kinhPAR(ip,ng)/(ksatPAR(ip,ng)+kinhPAR(ip,ng)))))
       ELSE
        normI(ip,ng) = 1.0_r8
       ENDIF
      ENDDO
#endif

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

