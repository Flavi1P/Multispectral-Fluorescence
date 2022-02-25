      program ecoconvert
!
      integer, parameter :: jpn = 10000
      integer :: ji,jn,flag,jmax,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9
      real*8, dimension(jpn) :: time,lat,lon,prs,tem,sal,oxy,chl,trs,par
      real*8, dimension(jpn) :: cn1,cn2
      real*8  :: z1,z2,z3,z4,z5,z6,z7,z8,z9,z10,z11,z12,z13,z14,z15,    &
     &           ztime,zsf1,sdc1,zsf2,zdc2
!
!*... Scale factors
      zsf1 = 1. ! 0.0073
      zdc1 = 0. ! 45.
      zsf2 = 1. ! 0.001639
      zdc2 = 0. ! 45.
!
!*... Initialisation of vectors
      prs(:)=-999.;tem(:)=-999.;sal(:)=-999.;lon(:)=-999.;lat(:)=-999.
      chl(:)=-999.;oxy(:)=-999.;trs(:)=-999.;par(:)=-999.
      cn1(:)=-999.;cn2(:)=-999.
!
!*... Read input CTD
      open(1,file='inputCTD',form='formatted')
      jn = 0
      flag = 0
      do while( flag.eq.0 )
        read(1,*,iostat=flag) a1,a2,a3,a4,a5,a6,z1,z2,z3,z4,z5,z6,z7,z8,&
     &                        z9,z10,z11,z12,z13,z14,z15
        if( flag.eq.0 ) then
        jn = jn+1
!*...   Get time values
        time(jn) = float(a1)*24.+float(a4)+float(a5)/60.                &
     &                                    +float(a6)/3600.
!*...   Get CTD values
        lat(jn)  = z1
        lon(jn)  = z2
        prs(jn)  = z3
        tem(jn)  = z4
        chl(jn)  = z6
        trs(jn)  = z7
        par(jn)  = z8
        sal(jn)  = z10
        oxy(jn)  = z12
        endif
      end do
      close(1)
      jmax = jn-1
!
!*... Read input NO3
      open(1,file='input',form='formatted')
      read(1,*)
      flag = 0
      do while( flag.eq.0 )
        read(1,*,iostat=flag) a0,a1,a2,z1,a3,a4,a5,a6,a7,a8,a9
        if( flag.eq.0 ) then
          ztime = float(a0)*24.+(float(a1)+float(a2)/60.+z1/3600.)
          kt = 0
          do jn = 1, jmax-1
            if( time(jn).le.ztime.and.time(jn+1).gt.ztime ) kt = jn
          end do
          if( kt.ne.0 ) then
            cn1(kt) = zsf1*(float(a4)-zdc1)
            cn2(kt) = zsf2*(float(a6)-zdc2)
          endif
        endif
      end do
      close(1)
!
!*... Print in output file
      zprs = 0.
      do jn = 1, jmax
        if( prs(jn).gt.zprs ) then
           a0 = jn
           zprs = prs(jn)
        endif
      end do
      open(1,file='inputCTD',form='formatted')
      open(2,file='outdw',form='formatted')
      open(3,file='outup',form='formatted')
      do jn = 1, jmax
        read(1,*) a1,a2,a3,a4,a5,a6,z1,z2,z3,z4,z5,z6,z7,z8,            &
     &                     z9,z10,z11,z12,z13,z14,z15
        if( jn.le.a0 )then
        write(2,20) a1,a2,a3,a4,a5,a6,lat(jn),lon(jn),                  &
     &   prs(jn),tem(jn),sal(jn),oxy(jn),chl(jn),trs(jn),par(jn),cn1(jn)&
     &  ,cn2(jn)
        else
        write(3,20) a1,a2,a3,a4,a5,a6,lat(jn),lon(jn),                  &
     &   prs(jn),tem(jn),sal(jn),oxy(jn),chl(jn),trs(jn),par(jn),cn1(jn)&
     &  ,cn2(jn)
        endif
      end do
      close(1)
      close(2)
      close(3)
 20   format(6I6,2F13.6,9F11.4)
!
      end program
