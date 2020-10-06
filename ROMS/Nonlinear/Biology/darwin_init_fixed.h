!
!  converted from "gud_init_fixed.F" using darwinfortran_to_romsfortran
!  (written by J. Paul Mattern)
!
#if defined DARWIN_RADTRANS

! band widths used to convert OASIM data to irradiation per nm
        wb_totalWidth = 0.0_r8
        DO l=1,nlam
          wb_width(l) = gud_waveband_edges(l+1,ng) -gud_waveband_edges(l,ng)
          wb_totalWidth = wb_totalWidth + wb_width(l)
        ENDDO
        IF (wb_totalWidth.LE.0) then
          IF (Master) WRITE(out,'(2A)') 'DARWIN_INIT_FIXED: ',          &
     &      'please provide wavebabnds in gud_waveband_edges(ng).'
          exit_flag=5
          RETURN
        ENDIF
        planck = 6.6256e-34_r8 !Plancks constant J sec
        c = 2.998e8_r8 !speed of light m/sec
        hc = 1.0_r8/(planck*c)
        oavo = 1.0_r8/6.023e23_r8 ! 1/Avogadros number
        hcoavo = hc*oavo
        DO l = 1,nlam
          IF (gud_waveband_centers(l,ng) .GE. 0.0_r8) THEN
            wb_center(l) = gud_waveband_centers(l,ng)
          ELSE
            wb_center(l) = 0.5_r8*(gud_waveband_edges(l,ng)+gud_waveband_edges(l+1,ng))
          ENDIF
          rlamm = wb_center(l)*1e-9_r8 !lambda in_ m
          WtouEins(l) = 1e6_r8*rlamm*hcoavo !Watts to uEin/s conversion
        ENDDO
! write summary
        ! TODO: move summary away
        !WRITE(*,'(A)') 'DARWIN_INIT_FIXED:'
        !WRITE(*,'(A)') 'DARWIN_INIT_FIXED: wavebands:'
        !WRITE(*,'(2A)') 'DARWIN_INIT_FIXED: ',' idx low rep high width'
        !DO l=1,nlam
        !  WRITE(*,'(A,I4,F10.3,F6.0,F10.3,F9.3)')'DARWIN_INIT_FIXED: ', l, gud_waveband_edges(l,ng),wb_center(l),gud_waveband_edges(l+1),wb_width(l)
        !ENDDO
        !WRITE(*,'(A)') 'DARWIN_INIT_FIXED:'

        !TODO-low was this import?
        !IF ( myProcId.EQ.0 .AND. myThid.EQ.1 ) THEN
        !
! file for wavebands used
! open(23,file='pwaves-check.dat',status='new')
        !  CALL MDSFINDUNIT( iUnit1, myThid )
        !  open(iUnit1,file='pwaves-check.dat',status='unknown')
        !  write(iUnit1,'(F6.0)')wb_center
        !  close(iUnit1)
        !
        !ENDIF

! read water absorption data
        DO l = 1, nlam
          aw(l) = -1.0_r8
          bw(l) = -1.0_r8
        ENDDO
#if defined XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
        IF (gud_waterAbsorbFile .NE. ' ' ) THEN
          CALL MDSFINDUNIT( iUnit1, myThid )
          OPEN(iUnit1,FILE=gud_waterabsorbFile,STATUS='old',FORM='formatted')
! skip header
          DO i = 1,6
           READ(iUnit1,'(A50)')title
          ENDDO
          ios = 0
          DO WHILE (ios .EQ. 0)
           READ(iUnit1,'(I5,F15.4,F10.4)',IOSTAT=ios) ilambda,ain,bin
           IF (ios .EQ. 0) THEN
            lambdain = ilambda
            DO l = 1,nlam
             IF (lambdain .GE. gud_waveband_edges(l,ng) .AND.lambdain .LE. gud_waveband_edges(l+1)) THEN
              aw(l) = ain
              bw(l) = bin
             ENDIF
            ENDDO
           ENDIF
          ENDDO
          CLOSE(iUnit1)
        ELSE
          IF (Master) WRITE(out,'(A)')'DARWIN_INIT_FIXED: need to specify water absorption file'
          exit_flag=5
          RETURN
        ENDIF
#endif
! check that all wavebands have been read
        DO l = 1, nlam
          IF (aw(l) .LT. 0.0_r8) THEN
            WRITE(*,'(2A)') 'DARWIN_INIT_FIXED: ',"could not read water absorption data for band "
            IF (Master) WRITE(out,'(A,I3,2F8.3)') 'DARWIN_INIT_FIXED: ',l,gud_waveband_edges(l,ng),gud_waveband_edges(l+1)
            exit_flag=5
            RETURN
          ENDIF
        ENDDO
! write summary
        WRITE(*,'(A)') 'DARWIN_INIT_FIXED: water spectra:'
        WRITE(*,'(A,A)') 'DARWIN_INIT_FIXED: ',' lam aw bw'
        DO l = 1,nlam
          WRITE(*,'(A,F4.0,F15.4,F10.4)') 'DARWIN_INIT_FIXED: ',wb_center(l), aw(l), bw(l)
        ENDDO
        WRITE(*,'(A)') 'DARWIN_INIT_FIXED:'


! read phyto absorption data
! phyto input data files must have a column for absorption by PS pigs
! easiest way to 'turn off' PS for growth is to put same values in_ both abs columns
        DO i = 1, nopt
         DO l = 1, nlam
          aphy_chl_type (i,l) = -1.0_r8
          aphy_chl_ps_type(i,l) = -1.0_r8
          bphy_mgC_type (i,l) = -1.0_r8
          bbphy_mgC_type (i,l) = -1.0_r8
         ENDDO
        ENDDO
        IF (gud_phytoAbsorbFile .NE. ' ' ) THEN
          CALL MDSFINDUNIT( iUnit1, myThid )
          OPEN(iUnit1,FILE=gud_phytoAbsorbFile,STATUS='old',FORM='formatted')
! skip global header
          DO i = 1,6
           READ(iUnit1,'(A50)')title
          ENDDO
! phytoplanktontype header
          READ(iUnit1,'(A50)')title
          DO i = 1,nopt
           ios = 0
           IF (gud_allomSpectra(ng)) THEN
             READ(iUnit1,'(I4,3F10.0,F20.0)'),idummy, asize(i), apsize(i), bsize(i), bbsize(i)
           ENDIF
           DO WHILE (ios .EQ. 0)
            READ(iUnit1,'(I4,3F10.0,F20.0)',IOSTAT=ios)ilambda,ain,apsin,bin,bbin
! next phyto type header will trigger error and move on to next i
            IF (ios .EQ. 0) THEN
             lambdain = ilambda
             DO l = 1,nlam
              IF (lambdain .GE. gud_waveband_edges(l,ng) .AND.lambdain .LE. gud_waveband_edges(l+1)) THEN
               aphy_chl_type (i,l) = ain
               aphy_chl_ps_type(i,l) = apsin
               bphy_mgC_type (i,l) = bin
               bbphy_mgC_type (i,l) = bbin
              ENDIF
             ENDDO
            ENDIF
           ENDDO
          ENDDO
          CLOSE(iUnit1)
        ELSE
          IF (Master) WRITE(out,'(A)')'DARWIN_INIT_FIXED: need to specify water absorption file'
          exit_flag=5
          RETURN
        ENDIF
! check that all wavebands have been read
        DO i = 1, nopt
         DO l = 1, nlam
          IF (aphy_chl_type(i,l) .LT. 0.0_r8) THEN
            WRITE(*,'(2A)') 'DARWIN_INIT_FIXED: ',"could not read phyto absorption data for type,band "
            IF (Master) WRITE(out,'(A,2I3,2F8.3)') 'DARWIN_INIT_FIXED: ',i,l,gud_waveband_edges(l,ng),gud_waveband_edges(l+1)
            exit_flag=5
            RETURN
          ENDIF
         ENDDO
        ENDDO
! write summary
        WRITE(*,'(A)') 'DARWIN_INIT_FIXED: phyto spectra:'
        DO i = 1,nopt
          WRITE(*,'(A,I4)') 'DARWIN_INIT_FIXED: type ', i
          WRITE(*,'(A,A)') 'DARWIN_INIT_FIXED: ',' lam ap ap_ps bp bbp'
          DO l = 1,nlam
            WRITE(*,'(A,F4.0,3F10.4,F20.9)') 'DARWIN_INIT_FIXED: ',wb_center(l), aphy_chl_type(i,l), aphy_chl_ps_type(i,l),bphy_mgC_type(i,l), bbphy_mgC_type(i,l)
          ENDDO
          WRITE(*,'(A)') 'DARWIN_INIT_FIXED:'
        ENDDO


! read particle absorption data
! initialize particle absorption coefficient
        DO l = 1, nlam
          apart(l) = -1.0_r8
          bpart(l) = -1.0_r8
          bbpart(l) = -1.0_r8
        ENDDO
        IF (gud_particleAbsorbFile .NE. ' ' ) THEN
          CALL MDSFINDUNIT( iUnit1, myThid )
          OPEN(iUnit1,FILE=gud_particleAbsorbFile,STATUS='old',FORM='formatted')
! skip header
          DO i = 1,6
           READ(iUnit1,'(A50)')title
          ENDDO
          ios = 0
          DO WHILE (ios .EQ. 0)
           READ(iUnit1,'(I4,3F15.0)',IOSTAT=ios) ilambda,ain,bin,bbin
           IF (ios .EQ. 0) THEN
            lambdain = ilambda
            DO l = 1,nlam
             IF (lambdain .GE. gud_waveband_edges(l,ng) .AND.lambdain .LE. gud_waveband_edges(l+1)) THEN
              apart(l) = ain
              bpart(l) = bin
              bbpart(l) = bbin
              apart_P(l) = ain/gud_part_size_P(ng)
              bpart_P(l) = bin/gud_part_size_P(ng)
              bbpart_P(l) = bbin/gud_part_size_P(ng)
             ENDIF
            ENDDO
           ENDIF
          ENDDO
          CLOSE(iUnit1)
        ELSE
          IF (Master) WRITE(out,'(A)')'DARWIN_INIT_FIXED: need to specify particle file'
          exit_flag=5
          RETURN
        ENDIF
! check that all wavebands have been read
        DO l = 1, nlam
          IF (apart(l) .LT. 0.0_r8) THEN
            WRITE(*,'(2A)') 'DARWIN_INIT_FIXED: ',"could not read particle for band "
            IF (Master) WRITE(out,'(A,I3,2F8.3)') 'DARWIN_INIT_FIXED: ',l,gud_waveband_edges(l,ng),gud_waveband_edges(l+1)
            exit_flag=5
            RETURN
          ENDIF
        ENDDO
! write summary
        WRITE(*,'(A)') 'DARWIN_INIT_FIXED: particulate spectra:'
        WRITE(*,'(A,A)') 'DARWIN_INIT_FIXED: ',' lam apart bpart bbpart'
        DO l = 1,nlam
          WRITE(*,'(A,F4.0,1P3G15.6)')'DARWIN_INIT_FIXED: ',wb_center(l), apart(l), bpart(l), bbpart(l)
        ENDDO
        WRITE(*,'(A)') 'DARWIN_INIT_FIXED:'
!
        WRITE(*,'(2A)') 'DARWIN_INIT_FIXED: particulate spectra ','in_ phosphorus units:'
        WRITE(*,'(A,A)') 'DARWIN_INIT_FIXED: ',' lam apart_P bpart_P bbpart_P'
        DO l = 1,nlam
          WRITE(*,'(A,F4.0,2F15.9,F15.12)') 'DARWIN_INIT_FIXED: ',wb_center(l), apart_P(l), bpart_P(l), bbpart_P(l)
        ENDDO
        WRITE(*,'(A)') 'DARWIN_INIT_FIXED:'
!


        DO l = 1,nlam
          exCDOM(l)=EXP(-gud_Sdom(ng)*(wb_center(l)-gud_lambda_aCDOM(ng)))
        ENDDO
#if ! defined DARWIN_CDOM
! initialize CDOM absorption coefficient
        laCDOM = -1
        DO l = 1,nlam
          IF (gud_lambda_aCDOM(ng) .GE. gud_waveband_edges(l,ng) .AND.gud_lambda_aCDOM(ng) .LE. gud_waveband_edges(l+1)) THEN
            laCDOM = l
          ENDIF
        ENDDO
        IF (laCDOM .LE. 0) THEN
          WRITE(*,'(2A)') 'DARWIN_INIT_FIXED: ',"could not read find aCDOM reference waveband with frequency"
          IF (Master) WRITE(out,'(A,F8.3)') 'DARWIN_INIT_FIXED: ',gud_lambda_aCDOM(ng)
          exit_flag=5
          RETURN
        ELSE
          WRITE(*,'(A,I3)')'DARWIN_INIT_FIXED: laCDOM = ', laCDOM
        ENDIF
#endif

#endif /* DARWIN_RADTRANS */

