!
!-----------------------------------------------------------------------
!  Dump darwin_tile outputs for regression testing (DARWIN_DUMP_TILE).
!  Appends one self-describing "OUT" record per (i,j,k,variable) every
!  call, to the same file opened in darwin_dump_input.h.
!-----------------------------------------------------------------------
!
      DO j=Jstr,Jend
# if defined DARWIN_DUMP_TILE_JSTEP
        IF (MOD(j,DARWIN_DUMP_TILE_JSTEP).ne.0) THEN
          CYCLE
        END IF
# endif
        DO i=Istr,Iend
# if defined DARWIN_DUMP_TILE_ISTEP
          IF (MOD(i,DARWIN_DUMP_TILE_ISTEP).ne.0) THEN
            CYCLE
          END IF
# endif
# if defined DARWIN_CARBON_EXTRAOUT
          WRITE (usrout,DumpFmt) 'OUT', iic(ng), tdays(ng), ng, tile,   &
     &                           i, j, 0, 'CO2flx', CO2flx(i,j)
# endif
          DO k=1,N(ng)
# if defined DARWIN_DUMP_TILE_KSTEP
            IF (MOD(i,DARWIN_DUMP_TILE_KSTEP).ne.0) THEN
              CYCLE
            END IF
# endif
            DO itrc=1,NBT
              ibio=idbio(itrc)
              DumpVname=Vname(1,idTvar(ibio))
              WRITE (usrout,DumpFmt) 'OUT', iic(ng), tdays(ng), ng,     &
     &                               tile, i, j, k, TRIM(DumpVname),    &
     &                               t(i,j,k,bnew,ibio)
            END DO
# if defined DARWIN_CARBON_EXTRAOUT
            WRITE (usrout,DumpFmt) 'OUT', iic(ng), tdays(ng), ng, tile, &
     &                             i, j, k, 'pH', pH(i,j,k)
            WRITE (usrout,DumpFmt) 'OUT', iic(ng), tdays(ng), ng, tile, &
     &                             i, j, k, 'pCO2', pCO2(i,j,k)
# endif
# if defined DIAGNOSTICS_BIO
            DO itrc=1,NDbio3d
              WRITE (DumpVname,'(a,i2.2)') 'DiaBio3d', itrc
              WRITE (usrout,DumpFmt) 'OUT', iic(ng), tdays(ng), ng,     &
     &                               tile, i, j, k, TRIM(DumpVname),    &
     &                               DiaBio3d(i,j,k,itrc)
            END DO
            DO isink=1,nplank
              DO itrc=1,NDbio4d
                WRITE (DumpVname,'(a,i2.2,a,i2.2)') 'DiaBio4d', itrc,   &
     &                                               'p', isink
                WRITE (usrout,DumpFmt) 'OUT', iic(ng), tdays(ng), ng,   &
     &                                 tile, i, j, k, TRIM(DumpVname),  &
     &                                 DiaBio4d(i,j,k,isink,itrc)
              END DO
            END DO
# endif
          END DO
        END DO
      END DO
