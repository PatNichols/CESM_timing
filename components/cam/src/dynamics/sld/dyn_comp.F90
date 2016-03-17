module dyn_comp
!----------------------------------------------------------------------- 
! 
! Dycore interface module for SLD
!
!-----------------------------------------------------------------------

use shr_kind_mod, only: r8 => shr_kind_r8
use constituents, only: pcnst, cnst_name, cnst_longname
use constituents, only: tendnam, fixcnam, tottnam, hadvnam, vadvnam
use ppgrid,       only: pver, pverp
use pmgrid,       only: plev, plevp
use hycoef,       only: hycoef_init
use cam_history,  only: addfld, add_default, horiz_only
use pio,          only: file_desc_t


implicit none
private

public :: dyn_init, dyn_import_t, dyn_export_t

! these structures are not used in this dycore, but are included
! for source code compatibility.  
type dyn_import_t
   integer :: placeholder
end type dyn_import_t

type dyn_export_t
   integer :: placeholder
end type dyn_export_t

!#######################################################################
CONTAINS
!#######################################################################

subroutine dyn_init(file, nlfilename)

   use spmd_utils,          only: masterproc
   use sld_control_mod,     only: dyn_sld_readnl
   use phys_control,        only: phys_getopts
#if (defined SPMD)
   use spmd_dyn,            only: spmd_readnl,spmdinit_dyn
#endif
   use dyn_grid,            only: define_cam_grids

   ! ARGUMENTS:
   type(file_desc_t), intent(in) :: file       ! PIO file handle for initial or restart file
   character(len=*),  intent(in) :: nlfilename

   logical :: history_amwg       ! output for AMWG diagnostics
   ! Local workspace
   integer m

   call trunc()

   call dyn_sld_readnl(nlfilename)

#if (defined SPMD)
   call spmd_readnl(nlfilename)
   call spmdinit_dyn()
#endif 

   ! Initialize hybrid coordinate arrays
   call hycoef_init(file)

   ! Define the CAM grids (must be before addfld calls)
   call define_cam_grids()

   call addfld ('ETADOT',(/ 'ilev' /),'A','1/s','Vertical (eta) velocity',gridname='gauss_grid')
   call addfld ('U&IC',  (/ 'lev' /), 'I','m/s','Zonal wind',             gridname='gauss_grid' )
   call addfld ('V&IC',  (/ 'lev' /), 'I','m/s','Meridional wind',        gridname='gauss_grid' )
   call add_default ('U&IC',0, 'I')
   call add_default ('V&IC',0, 'I')

   call addfld ('PS&IC',horiz_only, 'I','Pa','Surface pressure',          gridname='gauss_grid' )
   call addfld ('T&IC', (/ 'lev' /),'I','K', 'Temperature',               gridname='gauss_grid' )
   call add_default ('PS&IC',0, 'I')
   call add_default ('T&IC',0, 'I')

   do m = 1, pcnst
      call addfld (trim(cnst_name(m))//'&IC',(/ 'lev' /),'I', 'kg/kg',cnst_longname(m),                      gridname='gauss_grid')
      call add_default(trim(cnst_name(m))//'&IC',0, 'I')
      call addfld (hadvnam(m),(/ 'lev' /),'A','kg/kg/s',trim(cnst_name(m))//' horizontal advection tendency',gridname='gauss_grid')
      call addfld (vadvnam(m),(/ 'lev' /),'A','kg/kg/s',trim(cnst_name(m))//' vertical advection tendency',  gridname='gauss_grid')
      call addfld (tendnam(m),(/ 'lev' /),'A','kg/kg/s',trim(cnst_name(m))//' total tendency',               gridname='gauss_grid')
      call addfld (tottnam(m),(/ 'lev' /),'A','kg/kg/s',trim(cnst_name(m))//' horz + vert + fixer tendency', gridname='gauss_grid')
      call addfld (fixcnam(m),(/ 'lev' /),'A','kg/kg/s',trim(cnst_name(m))//' tendency due to slt fixer',    gridname='gauss_grid')
   end do

   call addfld ('DUH     ',(/ 'lev' /),'A','K/s     ','U horizontal diffusive heating',              gridname='gauss_grid')
   call addfld ('DVH     ',(/ 'lev' /),'A','K/s     ','V horizontal diffusive heating',              gridname='gauss_grid')
   call addfld ('DTH     ',(/ 'lev' /),'A','K/s     ','T horizontal diffusive heating',              gridname='gauss_grid')
   call addfld ('ENGYCORR',(/ 'lev' /),'A','W/m2    ','Energy correction for over-all conservation', gridname='gauss_grid')
   call addfld ('TFIX    ',horiz_only, 'A','K/s     ','T fixer (T equivalent of Energy correction)', gridname='gauss_grid')

   call phys_getopts(history_amwg_out=history_amwg)

   if (history_amwg) call add_default ('DTH     ', 1, ' ')

   call addfld ('FU      ',(/ 'lev' /),'A','m/s2    ','Zonal wind forcing term',            gridname='gauss_grid')
   call addfld ('FV      ',(/ 'lev' /),'A','m/s2    ','Meridional wind forcing term',       gridname='gauss_grid')
   call addfld ('UTEND   ',(/ 'lev' /),'A','m/s2    ','U tendency',                         gridname='gauss_grid')
   call addfld ('VTEND   ',(/ 'lev' /),'A','m/s2    ','V tendency',                         gridname='gauss_grid')
   call addfld ('TTEND   ',(/ 'lev' /),'A','K/s     ','T tendency',                         gridname='gauss_grid')
   call addfld ('LPSTEN  ',horiz_only, 'A','Pa/s    ','Surface pressure tendency',          gridname='gauss_grid')
   call addfld ('VAT     ',(/ 'lev' /),'A','K/s     ','Vertical advective tendency of T',   gridname='gauss_grid')
   call addfld ('KTOOP   ',(/ 'lev' /),'A','K/s     ','(Kappa*T)*(omega/P)',                gridname='gauss_grid')

end subroutine dyn_init

!#######################################################################
! Private routines
!#######################################################################

subroutine trunc()

! Check consistency of truncation parameters and evaluate pointers
! and displacements for spectral arrays
!
! Original version:  CCM1

  use shr_kind_mod,   only: r8 => shr_kind_r8
  use pspect,         only: ptrm, ptrn, ptrk, pmax, pmmax
  use comspe,         only: ncoefi, nalp, nco2, nm, nstart, nlen, ncutoff, locm
  use cam_abortutils, only: endrun

  implicit none

!---------------------------Local variables-----------------------------
!
  integer n              ! Loop index over diagonals
  integer ik2            ! K+2
  integer m              ! loop index
!
!-----------------------------------------------------------------------
!
! trunc first evaluates truncation parameters for a general pentagonal 
! truncation for which the following parameter relationships are true
!
! 0 .le. |m| .le. ptrm
!
! |m| .le. n .le. |m|+ptrn for |m| .le. ptrk-ptrn
!
! |m| .le. n .le. ptrk     for (ptrk-ptrn) .le. |m| .le. ptrm
!
! Most commonly utilized truncations include:
!  1: triangular  truncation for which ptrk=ptrm=ptrn
!  2: rhomboidal  truncation for which ptrk=ptrm+ptrn
!  3: trapezoidal truncation for which ptrn=ptrk .gt. ptrm
!
! Simple sanity check
! It is necessary that ptrm .ge. ptrk-ptrn .ge. 0
!
  if (ptrm.lt.(ptrk-ptrn)) then
     call endrun ('TRUNC: Error in truncation parameters. ntrm.lt.(ptrk-ptrn)')
  end if
  if (ptrk.lt.ptrn) then
     call endrun ('TRUNC: Error in truncation parameters. ptrk.lt.ptrn')
  end if
!
! Evaluate pointers and displacement info based on truncation params
!
  ncoefi(1) = 1
  ik2 = ptrk + 2
  do n=1,pmax
     ncoefi(n+1) = ncoefi(n) + min0(pmmax,ik2-n)
     nalp(n) = ncoefi(n) - 1
     nco2(n) = ncoefi(n)*2
     nm(n) = ncoefi(n+1) - ncoefi(n)
  end do
  nstart(1) = 0
  nlen(1) = ptrn + 1
  do m=2,pmmax
     nstart(m) = nstart(m-1) + nlen(m-1)
     nlen(m) = min0(ptrn+1,ptrk+2-m)
  end do
!
! Define break-even point for vector lengths in GRCALC.  Don't implement
! for non-PVM machines
!
  ncutoff = pmax
!
! Assign wavenumbers if not SPMD.
!
#if ( ! defined SPMD )
   do m=1,pmmax
      locm(m,0) = m
   enddo
#endif

end subroutine trunc

end module dyn_comp
