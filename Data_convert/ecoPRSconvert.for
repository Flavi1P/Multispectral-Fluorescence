      program ecoconvert
      integer, parameter :: jpn = 1000000
      real, dimension(jpn) :: prs,prstime,eco1time,eco2time,eco2prs,    &
     &                        eco1prs
      integer, dimension(jpn) :: eco11,eco12,eco13,eco21,eco22,eco23
      integer :: a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,jn,jt
      integer :: eco1max,prsmax,eco2max,flag
      real :: z1,z2

      open(1,file="C1")
      a0 = 0
      jn = 0
      flag = 0
      do while( flag.eq.0 )
        read(1,*,iostat=flag) a1,a2,a3,z1,z2
        if( flag.eq.0 ) then
        jn = jn+1
        if( a0.eq.0 ) a0 = a1
        prs(jn) = z2
        prstime(jn) = float(a1-a0)*3600.*24.+float(a2)*3600.            &
     &               +float(a3)*60.+z1
        endif
      end do
      close(1)
      prsmax = jn-1

      open(1,file="C2")
      a0 = 0
      jn = 0
      flag = 0
      do while( flag.eq.0 )
        read(1,*,iostat=flag) a1,a2,a3,z1,a4,a5,a6,a7,a8,a9,a10
        if( flag.eq.0 ) then
        jn = jn+1
        if( a0.eq.0 ) a0 = a1
        eco11(jn) = a5
        eco12(jn) = a7
        eco13(jn) = a9
        eco1time(jn) = float(a1-a0)*3600.*24.+float(a2)*3600.            &
     &               +float(a3)*60.+z1
        endif
      end do
      close(1)
      eco1max = jn-1

      open(1,file="C3")
      a0 = 0
      jn = 0
      flag = 0
      do while( flag.eq.0 )
        read(1,*,iostat=flag) a1,a2,a3,z1,a4,a5,a6,a7,a8,a9,a10
        if( flag.eq.0 ) then
        jn = jn+1
        if( a0.eq.0 ) a0 = a1
        eco21(jn) = a5
        eco22(jn) = a7
        eco23(jn) = a9
        eco2time(jn) = float(a1-a0)*3600.*24.+float(a2)*3600.            &
     &               +float(a3)*60.+z1
        endif
      end do
      close(1)
      eco2max = jn-1

      do jn = 1, eco1max
        do jt = 1, prsmax
          if( eco1time(jn).ge.prstime(jt)                               &
     &   .and.eco1time(jn).lt.prstime(jt+1) )then
            eco1prs(jn) = (prs(jt)*(eco1time(jn)-prstime(jt))           &
     &                    +prs(jt+1)*(prstime(jt+1)-eco1time(jn)))      &
     &                   /(prstime(jt+1)-prstime(jt))
           goto 100
          endif
        end do
 100    continue
      end do

      do jn = 1, eco2max
        do jt = 1, prsmax
          if( eco2time(jn).ge.prstime(jt)                               &
     &   .and.eco2time(jn).lt.prstime(jt+1) )then
            eco2prs(jn) = (prs(jt)*(eco2time(jn)-prstime(jt))           &
     &                    +prs(jt+1)*(prstime(jt+1)-eco2time(jn)))      &
     &                   /(prstime(jt+1)-prstime(jt))
           goto 200
          endif
        end do
 200    continue
      end do

      open(1,file='ECO1.txt')
      do jn = 1, eco1max
        write(1,10) eco1prs(jn),eco11(jn),eco12(jn),eco13(jn)
      end do
      close(1)

      open(1,file='ECO2.txt')
      do jn = 1, eco2max
        write(1,10) eco2prs(jn),eco21(jn),eco22(jn),eco23(jn)
      end do
      close(1)

 10   format(F12.4,3I8)

      end program
