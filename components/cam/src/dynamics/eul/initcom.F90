subroutine initcom

!----------------------------------------------------------------------- 
! 
! Purpose: none
!
!-----------------------------------------------------------------------

    !----------------------------------------------------------------------- 
    ! 
    ! Purpose: 
    ! Initialize Model commons, including COMCON, COMHYB, COMMAP, COMSPE,
    ! and COMTRCNM
    ! 
    ! Method: 
    ! 
    ! Author: 
    ! Original version:  CCM1
    ! Standardized:      L. Bath, Jun 1992
    !                    L. Buja, Feb 1996
    !
    !-----------------------------------------------------------------------
    !
    ! $Id$
    ! $Author$
    !
    !-----------------------------------------------------------------------
    use shr_kind_mod,    only: r8 => shr_kind_r8
    use pmgrid,          only: plon, plat
    use pspect
    use comspe
    use scanslt,         only: nlonex, platd, j1
    use gauaw_mod,       only: gauaw
    use commap,          only: sq, rsq, slat, w, cs, href, ecref, clat, clon, &
         latdeg, londeg, xm
    use scamMod,         only: scmlat,scmlon,single_column
    use cam_abortutils,  only: endrun
    use cam_logfile,     only: iulog
    !-----------------------------------------------------------------------
    implicit none
    !-----------------------------------------------------------------------

    ! Local workspace
    !
    real(r8) zsi(plat)      ! sine of latitudes
    real(r8) zw(plat)       ! Gaussian weights

    integer i           ! longitude index
    integer j           ! Latitude index
    integer k           ! Level index
    integer n           ! Index for legendre array
    integer itmp        ! Dimension of polynomial arrays temporary.
    integer iter        ! Iteration index
    real(r8)    zdt         ! Time step for settau

    integer irow        ! Latitude pair index
    integer lat         ! Latitude index

    real(r8) xlat           ! Latitude (radians)
    real(r8) pi             ! Mathematical pi (3.14...)
    !
    !-----------------------------------------------------------------------
    !
    if ( .not. single_column ) then
      !
      ! Gaussian latitude dependent arrays
      !
      call gauaw(zsi     ,zw      ,plat    )
      do irow=1,plat/2
        slat(irow) = zsi(irow)
        w(irow)              = zw(irow)
        w(plat - irow + 1)   = zw(irow)
        cs(irow)  = 1._r8 - zsi(irow)*zsi(irow)
        xlat = asin(slat(irow))
        clat(irow) = -xlat
        clat(plat - irow + 1) = xlat
      end do

      do lat=1,plat
        latdeg(lat) = clat(lat)*45._r8/atan(1._r8)
      end do
    endif

    if ( single_column ) then
      do j=1,plat
        slat(j) = 1.0_r8 * sin(4.0_r8*atan(1.0_r8)*scmlat/180._r8)
        w(j)   = 2.0_r8/plat
        cs(j)  = 10._r8 - slat(j)*slat(j)

      end do

      !
      ! Latitude array (list of latitudes in radians)
      !
      xlat = asin(slat(1))
      clat(1) = xlat

      clat(1)=scmlat*atan(1._r8)/45._r8
      latdeg(1) = clat(1)*45._r8/atan(1._r8)
      clon(1,1)   = 4.0_r8*atan(1._r8)*mod((scmlon+360._r8),360._r8)/180._r8
      londeg(1,1) = mod((scmlon+360._r8),360._r8)
    else
      !
      ! Longitude array
      !
      pi = 4.0_r8*atan(1.0_r8)
      do lat=1,plat
        do i=1,plon
          londeg(i,lat) = (i-1)*360._r8/plon
          clon(i,lat)   = (i-1)*2.0_r8*pi/plon
        end do
      end do
    endif

    return

end subroutine initcom
