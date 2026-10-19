            SELECT CASE (TRIM(ADJUSTL(Vinfo(8))))
!
!  all plankton groups
!
              CASE ('idTvar(ic_)')
                varid=varid-1
                DO i=1,nplank
                  varid=varid+1
                  idTvar(ic_+i-1)=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a)')                          &
     &              TRIM(plankname(i))
                  WRITE (Vname(2,varid),'(a,a)')                        &
     &              TRIM(plankname(i)),' concentration'
                  WRITE (Vname(3,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                        &
     &              TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(5)))
                  WRITE (Vname(6,varid),'(a)')                          &
     &              TRIM(plankname(i))
                END DO
              CASE ('idTbry(iwest,ic_)')
                varid=varid-1
                DO i=1,nplank
                  varid=varid+1
                  idTbry(iwest,ic_+i-1)=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a)')                        &
     &              TRIM(plankname(i)),'_west'
                  WRITE (Vname(2,varid),'(a,a)')                        &
     &              TRIM(plankname(i)),' western boundary condition'
                  WRITE (Vname(3,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                        &
     &              TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(5)))
                  WRITE (Vname(6,varid),'(a,a)')                        &
     &              TRIM(plankname(i)),'_west'
                END DO
              CASE ('idTbry(ieast,ic_)')
                varid=varid-1
                DO i=1,nplank
                  varid=varid+1
                  idTbry(ieast,ic_+i-1)=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a)')                        &
     &              TRIM(plankname(i)),'_east'
                  WRITE (Vname(2,varid),'(a,a)')                        &
     &              TRIM(plankname(i)),' eastern boundary condition'
                  WRITE (Vname(3,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                        &
     &              TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(5)))
                  WRITE (Vname(6,varid),'(a,a)')                        &
     &              TRIM(plankname(i)),'_east'
                END DO
              CASE ('idTbry(isouth,ic_)')
                varid=varid-1
                DO i=1,nplank
                  varid=varid+1
                  idTbry(isouth,ic_+i-1)=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a)')                        &
     &              TRIM(plankname(i)),'_south'
                  WRITE (Vname(2,varid),'(a,a)')                        &
     &              TRIM(plankname(i)),' southern boundary condition'
                  WRITE (Vname(3,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                        &
     &              TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(5)))
                  WRITE (Vname(6,varid),'(a,a)')                        &
     &              TRIM(plankname(i)),'_south'
                END DO
              CASE ('idTbry(inorth,ic_)')
                varid=varid-1
                DO i=1,nplank
                  varid=varid+1
                  idTbry(inorth,ic_+i-1)=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a)')                        &
     &              TRIM(plankname(i)),'_north'
                  WRITE (Vname(2,varid),'(a,a)')                        &
     &              TRIM(plankname(i)),' northern boundary condition'
                  WRITE (Vname(3,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                        &
     &              TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(5)))
                  WRITE (Vname(6,varid),'(a,a)')                        &
     &              TRIM(plankname(i)),'_north'
                END DO
!
!  chlorophyll
!
              CASE ('idTvar(iChl)')
                varid=varid-1
                DO i=1,nChl
                  varid=varid+1
                  idTvar(iChl+i-1)=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                     &
     &              TRIM(ADJUSTL(Vinfo(1))),i
                  WRITE (Vname(2,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,' concentration'
                  WRITE (Vname(3,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                        &
     &              TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(5)))
                  WRITE (Vname(6,varid),'(a,i2.2)')                     &
     &              TRIM(ADJUSTL(Vinfo(1))),i
                END DO
              CASE ('idTbry(iwest,iChl)')
                varid=varid-1
                DO i=1,nChl
                  varid=varid+1
                  idTbry(iwest,iChl+i-1)=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,'_west'
                  WRITE (Vname(2,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,                          &
     &              ' western boundary condition'
                  WRITE (Vname(3,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                        &
     &              TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(5)))
                  WRITE (Vname(6,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,'_west'
                END DO
              CASE ('idTbry(ieast,iChl)')
                varid=varid-1
                DO i=1,nChl
                  varid=varid+1
                  idTbry(ieast,iChl+i-1)=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,'_east'
                  WRITE (Vname(2,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,                          &
     &              ' eastern boundary condition'
                  WRITE (Vname(3,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                        &
     &              TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(5)))
                  WRITE (Vname(6,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,'_east'
                END DO
              CASE ('idTbry(isouth,iChl)')
                varid=varid-1
                DO i=1,nChl
                  varid=varid+1
                  idTbry(isouth,iChl+i-1)=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,'_south'
                  WRITE (Vname(2,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,                          &
     &              ' southern boundary condition'
                  WRITE (Vname(3,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                        &
     &              TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(5)))
                  WRITE (Vname(6,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,'_south'
                END DO
              CASE ('idTbry(inorth,iChl)')
                varid=varid-1
                DO i=1,nChl
                  varid=varid+1
                  idTbry(inorth,iChl+i-1)=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,'_north'
                  WRITE (Vname(2,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,                          &
     &              ' northern boundary condition'
                  WRITE (Vname(3,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                        &
     &              TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(5)))
                  WRITE (Vname(6,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,'_north'
                END DO
#if defined DARWIN_NQUOTA
!
!  nitrogen quota
!
              CASE ('idTvar(in_)')
                varid=varid-1
                DO i=1,nplank
                  varid=varid+1
                  idTvar(in_+i-1)=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                     &
     &              TRIM(ADJUSTL(Vinfo(1))),i
                  WRITE (Vname(2,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,' concentration'
                  WRITE (Vname(3,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                        &
     &              TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(5)))
                  WRITE (Vname(6,varid),'(a,i2.2)')                     &
     &              TRIM(ADJUSTL(Vinfo(1))),i
                END DO
              CASE ('idTbry(iwest,in_)')
                varid=varid-1
                DO i=1,nplank
                  varid=varid+1
                  idTbry(iwest,in_+i-1)=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,'_west'
                  WRITE (Vname(2,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,                          &
     &              ' western boundary condition'
                  WRITE (Vname(3,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                        &
     &              TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(5)))
                  WRITE (Vname(6,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,'_west'
                END DO
              CASE ('idTbry(ieast,in_)')
                varid=varid-1
                DO i=1,nplank
                  varid=varid+1
                  idTbry(ieast,in_+i-1)=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,'_east'
                  WRITE (Vname(2,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,                          &
     &              ' eastern boundary condition'
                  WRITE (Vname(3,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                        &
     &              TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(5)))
                  WRITE (Vname(6,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,'_east'
                END DO
              CASE ('idTbry(isouth,in_)')
                varid=varid-1
                DO i=1,nplank
                  varid=varid+1
                  idTbry(isouth,in_+i-1)=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,'_south'
                  WRITE (Vname(2,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,                          &
     &              ' southern boundary condition'
                  WRITE (Vname(3,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                        &
     &              TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(5)))
                  WRITE (Vname(6,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,'_south'
                END DO
              CASE ('idTbry(inorth,in_)')
                varid=varid-1
                DO i=1,nplank
                  varid=varid+1
                  idTbry(inorth,in_+i-1)=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,'_north'
                  WRITE (Vname(2,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,                          &
     &              ' northern boundary condition'
                  WRITE (Vname(3,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                        &
     &              TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(5)))
                  WRITE (Vname(6,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,'_north'
                END DO
#endif /* DARWIN_NQUOTA */
#if defined DARWIN_PQUOTA
!
!  phosphorus quota
!
              CASE ('idTvar(ip_)')
                varid=varid-1
                DO i=1,nplank
                  varid=varid+1
                  idTvar(ip_+i-1)=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                     &
     &              TRIM(ADJUSTL(Vinfo(1))),i
                  WRITE (Vname(2,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,' concentration'
                  WRITE (Vname(3,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                        &
     &              TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(5)))
                  WRITE (Vname(6,varid),'(a,i2.2)')                     &
     &              TRIM(ADJUSTL(Vinfo(1))),i
                END DO
              CASE ('idTbry(iwest,ip_)')
                varid=varid-1
                DO i=1,nplank
                  varid=varid+1
                  idTbry(iwest,ip_+i-1)=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,'_west'
                  WRITE (Vname(2,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,                          &
     &              ' western boundary condition'
                  WRITE (Vname(3,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                        &
     &              TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(5)))
                  WRITE (Vname(6,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,'_west'
                END DO
              CASE ('idTbry(ieast,ip_)')
                varid=varid-1
                DO i=1,nplank
                  varid=varid+1
                  idTbry(ieast,ip_+i-1)=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,'_east'
                  WRITE (Vname(2,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,                          &
     &              ' eastern boundary condition'
                  WRITE (Vname(3,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                        &
     &              TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(5)))
                  WRITE (Vname(6,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,'_east'
                END DO
              CASE ('idTbry(isouth,ip_)')
                varid=varid-1
                DO i=1,nplank
                  varid=varid+1
                  idTbry(isouth,ip_+i-1)=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,'_south'
                  WRITE (Vname(2,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,                          &
     &              ' southern boundary condition'
                  WRITE (Vname(3,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                        &
     &              TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(5)))
                  WRITE (Vname(6,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,'_south'
                END DO
              CASE ('idTbry(inorth,ip_)')
                varid=varid-1
                DO i=1,nplank
                  varid=varid+1
                  idTbry(inorth,ip_+i-1)=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,'_north'
                  WRITE (Vname(2,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,                          &
     &              ' northern boundary condition'
                  WRITE (Vname(3,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                        &
     &              TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(5)))
                  WRITE (Vname(6,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,'_north'
                END DO
#endif /* DARWIN_PQUOTA */
#if defined DARWIN_FEQUOTA
!
!  iron quota
!
              CASE ('idTvar(ife)')
                varid=varid-1
                DO i=1,nplank
                  varid=varid+1
                  idTvar(ife+i-1)=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                     &
     &              TRIM(ADJUSTL(Vinfo(1))),i
                  WRITE (Vname(2,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,' concentration'
                  WRITE (Vname(3,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                        &
     &              TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(5)))
                  WRITE (Vname(6,varid),'(a,i2.2)')                     &
     &              TRIM(ADJUSTL(Vinfo(1))),i
                END DO
              CASE ('idTbry(iwest,ife)')
                varid=varid-1
                DO i=1,nplank
                  varid=varid+1
                  idTbry(iwest,ife+i-1)=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,'_west'
                  WRITE (Vname(2,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,                          &
     &              ' western boundary condition'
                  WRITE (Vname(3,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                        &
     &              TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(5)))
                  WRITE (Vname(6,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,'_west'
                END DO
              CASE ('idTbry(ieast,ife)')
                varid=varid-1
                DO i=1,nplank
                  varid=varid+1
                  idTbry(ieast,ife+i-1)=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,'_east'
                  WRITE (Vname(2,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,                          &
     &              ' eastern boundary condition'
                  WRITE (Vname(3,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                        &
     &              TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(5)))
                  WRITE (Vname(6,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,'_east'
                END DO
              CASE ('idTbry(isouth,ife)')
                varid=varid-1
                DO i=1,nplank
                  varid=varid+1
                  idTbry(isouth,ife+i-1)=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,'_south'
                  WRITE (Vname(2,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,                          &
     &              ' southern boundary condition'
                  WRITE (Vname(3,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                        &
     &              TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(5)))
                  WRITE (Vname(6,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,'_south'
                END DO
              CASE ('idTbry(inorth,ife)')
                varid=varid-1
                DO i=1,nplank
                  varid=varid+1
                  idTbry(inorth,ife+i-1)=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,'_north'
                  WRITE (Vname(2,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,                          &
     &              ' northern boundary condition'
                  WRITE (Vname(3,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                        &
     &              TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(5)))
                  WRITE (Vname(6,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,'_north'
                END DO
#endif /* DARWIN_FEQUOTA */
#if defined DARWIN_SIQUOTA
!
!  silica quota
!
              CASE ('idTvar(isi)')
                varid=varid-1
                DO i=1,nplank
                  varid=varid+1
                  idTvar(isi+i-1)=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                     &
     &              TRIM(ADJUSTL(Vinfo(1))),i
                  WRITE (Vname(2,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,' concentration'
                  WRITE (Vname(3,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                        &
     &              TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(5)))
                  WRITE (Vname(6,varid),'(a,i2.2)')                     &
     &              TRIM(ADJUSTL(Vinfo(1))),i
                END DO
              CASE ('idTbry(iwest,isi)')
                varid=varid-1
                DO i=1,nplank
                  varid=varid+1
                  idTbry(iwest,isi+i-1)=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,'_west'
                  WRITE (Vname(2,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,                          &
     &              ' western boundary condition'
                  WRITE (Vname(3,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                        &
     &              TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(5)))
                  WRITE (Vname(6,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,'_west'
                END DO
              CASE ('idTbry(ieast,isi)')
                varid=varid-1
                DO i=1,nplank
                  varid=varid+1
                  idTbry(ieast,isi+i-1)=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,'_east'
                  WRITE (Vname(2,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,                          &
     &              ' eastern boundary condition'
                  WRITE (Vname(3,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                        &
     &              TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(5)))
                  WRITE (Vname(6,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,'_east'
                END DO
              CASE ('idTbry(isouth,isi)')
                varid=varid-1
                DO i=1,nplank
                  varid=varid+1
                  idTbry(isouth,isi+i-1)=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,'_south'
                  WRITE (Vname(2,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,                          &
     &              ' southern boundary condition'
                  WRITE (Vname(3,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                        &
     &              TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(5)))
                  WRITE (Vname(6,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,'_south'
                END DO
              CASE ('idTbry(inorth,isi)')
                varid=varid-1
                DO i=1,nplank
                  varid=varid+1
                  idTbry(inorth,isi+i-1)=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,'_north'
                  WRITE (Vname(2,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,                          &
     &              ' northern boundary condition'
                  WRITE (Vname(3,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                        &
     &              TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(5)))
                  WRITE (Vname(6,varid),'(a,i2.2,a)')                   &
     &              TRIM(ADJUSTL(Vinfo(1))),i,'_north'
                END DO
#endif /* DARWIN_SIQUOTA */
#if defined DIAGNOSTICS_BIO
!
!  Higher-dimensional diagnostic terms.
!
              CASE ('iDbio3(iPPplank)')
#if defined DARWIN_DEBUG_DIAG
!  smuggle in an extra variable
                iDbio3(iOnes)=varid
                WRITE (Vname(1,varid),'(a)') 'Ones'
                WRITE (Vname(2,varid),'(a)') 'Should contain ones'
                WRITE (Vname(3,varid),'(a)') 'dimensionless'
                WRITE (Vname(4,varid),'(a)') 'ones, ones, ones'
                WRITE (Vname(6,varid),'(a)') 'Ones'
                DO ng=1,Ngrids
                  Dout(iDbio3(iOnes),ng)=.true.
                END DO
                varid=varid+1
#endif
                varid=varid-1
                DO i=1,nPPplank
                  varid=varid+1
                  iDbio3(iPPplank+i-1)=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(3a)')                         &
     &              TRIM(ADJUSTL(Vinfo(1))),'_',TRIM(plankname(i))
                  WRITE (Vname(2,varid),'(3a)')                         &
     &              TRIM(plankname(i)),' ',TRIM(ADJUSTL(Vinfo(2)))
                  WRITE (Vname(3,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                        &
     &              TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(5)))
                  WRITE (Vname(6,varid),'(3a)')                         &
     &              TRIM(ADJUSTL(Vinfo(1))),'_',TRIM(plankname(i))
                END DO
              CASE ('iDbio3(iGRplank)')
                varid=varid-1
                DO i=1,nGRplank
                  varid=varid+1
                  iDbio3(iGRplank+i-1)=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(3a)')                         &
     &              TRIM(ADJUSTL(Vinfo(1))),'_',TRIM(plankname(i))
                  WRITE (Vname(2,varid),'(3a)')                         &
     &              TRIM(plankname(i)),' ',TRIM(ADJUSTL(Vinfo(2)))
                  WRITE (Vname(3,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                        &
     &              TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                          &
     &              TRIM(ADJUSTL(Vinfo(5)))
                  WRITE (Vname(6,varid),'(3a)')                         &
     &              TRIM(ADJUSTL(Vinfo(1))),'_',TRIM(plankname(i))
                END DO
#endif /*DIAGNOSTICS_BIO*/
            END SELECT
