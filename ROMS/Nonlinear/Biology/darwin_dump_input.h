!
!-----------------------------------------------------------------------
!  Dump darwin_tile inputs for regression testing (DARWIN_DUMP_TILE).
!  Opens the dump file once (first biology-active step) and appends one
!  self-describing "IN" record per (i,j,k,variable) every call.  See
!  darwin_dump_output.h for the matching "OUT" records.
!-----------------------------------------------------------------------
!
      IF (iic(ng).eq.ntstart(ng)) THEN
        OPEN (usrout, file=TRIM(USRname), form='formatted',             &
     &        status='unknown', IOSTAT=io_err, IOMSG=io_errmsg)
        IF (io_err.ne.0) THEN
          WRITE (stdout,'(a,a,/,12x,a,a)')                              &
     &      ' DARWIN_TILE - unable to open dump file: ', TRIM(USRname), &
     &      'ERROR: ', TRIM(io_errmsg)
          exit_flag=5
          RETURN
        END IF
      END IF
!
      DO j=Jstr,Jend
        DO i=Istr,Iend
          WRITE (usrout,DumpFmt) 'IN ', iic(ng), tdays(ng), ng, tile,   &
     &                           i, j, 0, 'h', h(i,j)
          WRITE (usrout,DumpFmt) 'IN ', iic(ng), tdays(ng), ng, tile,   &
     &                           i, j, 0, 'srflx', srflx(i,j)
# if defined MASKING
          WRITE (usrout,DumpFmt) 'IN ', iic(ng), tdays(ng), ng, tile,   &
     &                           i, j, 0, 'rmask', rmask(i,j)
# endif
# if defined BULK_FLUXES
          WRITE (usrout,DumpFmt) 'IN ', iic(ng), tdays(ng), ng, tile,   &
     &                           i, j, 0, 'Uwind', Uwind(i,j)
          WRITE (usrout,DumpFmt) 'IN ', iic(ng), tdays(ng), ng, tile,   &
     &                           i, j, 0, 'Vwind', Vwind(i,j)
# else
          WRITE (usrout,DumpFmt) 'IN ', iic(ng), tdays(ng), ng, tile,   &
     &                           i, j, 0, 'sustr', sustr(i,j)
          WRITE (usrout,DumpFmt) 'IN ', iic(ng), tdays(ng), ng, tile,   &
     &                           i, j, 0, 'svstr', svstr(i,j)
# endif
          DO k=1,N(ng)
            WRITE (usrout,DumpFmt) 'IN ', iic(ng), tdays(ng), ng, tile, &
     &                             i, j, k, 'Hz', Hz(i,j,k)
            WRITE (usrout,DumpFmt) 'IN ', iic(ng), tdays(ng), ng, tile, &
     &                             i, j, k, 'z_r', z_r(i,j,k)
            DO itrc=1,NBT
              ibio=idbio(itrc)
              DumpVname=Vname(1,idTvar(ibio))
              WRITE (usrout,DumpFmt) 'IN ', iic(ng), tdays(ng), ng,     &
     &                               tile, i, j, k, TRIM(DumpVname),    &
     &                               t(i,j,k,bstp,ibio)
            END DO
          END DO
        END DO
      END DO
