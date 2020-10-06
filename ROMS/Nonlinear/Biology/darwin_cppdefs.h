# if defined DARWIN_CARBON
!
      IF (Master) WRITE (stdout,20) 'DARWIN_CARBON',                    &
     &   'Include Darwin carbon module.'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+15)=' DARWIN_CARBON,'
# endif
# if defined DARWIN_CDOM
!
      IF (Master) WRITE (stdout,20) 'DARWIN_CDOM',                      &
     &   'Include Darwin CDOM module.'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+13)=' DARWIN_CDOM,'
# endif
# if defined DARWIN_CHLQUOTA
!
      IF (Master) WRITE (stdout,20) 'DARWIN_CHLQUOTA',                  &
     &   'Include chlorophyll quota for Darwin plankton variables.'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+17)=' DARWIN_CHLQUOTA,'
# endif
# if defined DARWIN_FEQUOTA
!
      IF (Master) WRITE (stdout,20) 'DARWIN_FEQUOTA',                   &
     &   'Include Fe quota for Darwin plankton variables.'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+16)=' DARWIN_FEQUOTA,'
# endif
# if defined DARWIN_GENERATE_ALLOMETRIC
!
      IF (Master) WRITE (stdout,20) 'DARWIN_GENERATE_ALLOMETRIC',       &
     &   'Generate Darwin parameters using allometric relationships.'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+28)=' DARWIN_GENERATE_ALLOMETRIC,'
# endif
# if defined DARWIN_INPUTFE
!
      IF (Master) WRITE (stdout,22) 'DARWIN_INPUTFE',                   &
     &   'Using a surface iron deposition of ', DARWIN_INPUTFE, '.'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+16)=' DARWIN_INPUTFE,'
# endif
# if defined DARWIN_NQUOTA
!
      IF (Master) WRITE (stdout,20) 'DARWIN_NQUOTA',                    &
     &   'Include N quota for Darwin plankton variables.'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+15)=' DARWIN_NQUOTA,'
# endif
# if defined DARWIN_PQUOTA
!
      IF (Master) WRITE (stdout,20) 'DARWIN_PQUOTA',                    &
     &   'Include P quota for Darwin plankton variables.'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+15)=' DARWIN_PQUOTA,'
# endif
# if defined DARWIN_RADTRANS
!
      IF (Master) WRITE (stdout,20) 'DARWIN_RADTRANS',                  &
     &   'Include Darwin light radiative transfer module.'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+17)=' DARWIN_RADTRANS,'
# endif
# if defined DARWIN_RANDOM_TRAITS
!
      IF (Master) WRITE (stdout,20) 'DARWIN_RANDOM_TRAITS',             &
     &   'Generate Darwin parameters randomly.'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+22)=' DARWIN_RANDOM_TRAITS,'
# endif
# if defined DARWIN_TEMP_VERSION
!
      IF (Master) WRITE (stdout,21) 'DARWIN_TEMP_VERSION',              &
     &   'Using DARWIN_TEMP_VERSION ', DARWIN_TEMP_VERSION, '.'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+21)=' DARWIN_TEMP_VERSION,'
# endif
# if defined DARWIN_TOTAL_CHLOROPHYLL
!
      IF (Master) WRITE (stdout,20) 'DARWIN_TOTAL_CHLOROPHYLL',         &
     &   'Add a total chlorophyll variable to Darwin.'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+26)=' DARWIN_TOTAL_CHLOROPHYLL,'
# endif
# if defined DARWIN_VERBOSE
!
      IF (Master) WRITE (stdout,20) 'DARWIN_VERBOSE',                   &
     &   'Create more verbose output of Darwin parametrization.'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+16)=' DARWIN_VERBOSE,'
# endif
