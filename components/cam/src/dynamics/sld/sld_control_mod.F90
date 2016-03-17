module sld_control_mod

! Control info for SLD dycore

use shr_kind_mod, only: r8=>shr_kind_r8
use pmgrid,       only: plat, plon, plev
use pspect,       only: pnmax
use spmd_utils,   only: masterproc

implicit none
private
save

public dyn_sld_readnl                    ! read dynamics namelist 

real(r8) ,public ::  tmass(plat)  ! Mass integral for each latitude pair
real(r8) ,public ::  tmass0       ! Specified dry mass of atmosphere
real(r8) ,public ::  tmassf       ! Global mass integral
real(r8) ,public ::  qmassf       ! Global moisture integral
real(r8) ,public ::  fixmas       ! Proportionality factor for ps in dry mass fixer
real(r8) ,public ::  qmass1       ! Contribution to global moisture integral (mass
                                     !  weighting is based upon the "A" part of the hybrid grid)
real(r8) ,public ::  qmass2       ! Contribution to global moisture integral (mass
                                  !  weighting is based upon the "B" part of the hybrid grid)
real(r8) ,public ::  pdela(plon,plev) ! pressure difference between interfaces (pressure
                                     !  defined using the "A" part of hybrid grid only)
real(r8) ,public ::  zgsint       ! global integral of geopotential height

integer  ,public :: pcray                   ! length of vector register (words)
parameter (pcray=64)

real(r8) ,public :: trig (3*plon/2+1,plat)  ! trigonometric funct values used by fft
integer  ,public :: ifax(19,plat)           ! fft factorization of plon/2

real(r8), public :: cnfac                ! Courant num factor(multiply by max |V|)
real(r8), public :: cnlim                ! Maximum allowable courant number
real(r8), public :: hdfsd2(pnmax)        ! Del^2 mult. for each wave (vort-div)
real(r8), public :: hdfst2(pnmax)        ! Del^2 multiplier for each wave (t-q)
real(r8), public :: hdfsd4(pnmax)        ! Del^4 mult. for each wave (vort-div)
real(r8), public :: hdfst4(pnmax)        ! Del^4 multiplier for each wave (t-q)
real(r8), public :: hdiftq(pnmax,plev)   ! Temp-tracer diffusion factors
real(r8), public :: hdifzd(pnmax,plev)   ! Vort-div diffusion factors
integer,  public :: kmnhd4               ! Top level for del^4 diffusion
integer,  public :: kmxhd2               ! Bottom level for increased del^2 diffusion
integer,  public :: nindex(plev)         ! Starting index for spectral truncation
integer,  public :: nmaxhd               ! Maximum two dimensional wave number

! Spectral Namelist variables

real(r8), public :: dif2               ! del2 horizontal diffusion coeff.
real(r8), public :: dif4               ! del4 horizontal diffusion coeff.
real(r8) ,public :: divdampn           ! Number of days to invoke divergence damper
real(r8) ,public :: eps                ! time filter coefficient. Defaults to 0.06.
integer,  public :: kmxhdc             ! Number of levels to apply Courant limiter

!=============================================================================================
contains
!=============================================================================================

subroutine dyn_sld_readnl(nlfile)

! Read dynamics namelist group.

   use shr_kind_mod,    only: r8 => shr_kind_r8, r4 => shr_kind_r4
   use cam_abortutils,  only: endrun
   use namelist_utils,  only: find_group_name
   use units,           only: getunit, freeunit
   use mpishorthand
   use cam_logfile,     only: iulog

   ! args
   character(len=*), intent(in) :: nlfile  ! filepath for file containing namelist input
    
   ! local vars
   integer :: unitn, ierr

   real(r8) :: dyn_spectral_dif2          ! del2 horizontal diffusion coeff.
   real(r8) :: dyn_spectral_dif4          ! del4 horizontal diffusion coeff.
   real(r8) :: dyn_spectral_divdampn      ! Number of days to invoke divergence damper
   real(r8) :: dyn_spectral_eps           ! time filter coefficient. Defaults to 0.06.
   integer  :: dyn_spectral_kmxhdc        ! Number of levels to apply Courant limiter
    
   namelist /dyn_spectral_inparm/ dyn_spectral_dif2, dyn_spectral_dif4, &
      dyn_spectral_divdampn, dyn_spectral_eps, dyn_spectral_kmxhdc

   ! 
   ! Read namelist 
   !
   if (masterproc) then
      unitn = getunit()
      open( unitn, file=trim(nlfile), status='old' )
      call find_group_name(unitn, 'dyn_spectral_inparm', status=ierr)
      if (ierr == 0) then
         read(unitn, dyn_spectral_inparm, iostat=ierr)
         if (ierr /= 0) then
            call endrun('dyn_sld_readnl: ERROR reading namelist')
         end if
      end if
      close(unitn)
      call freeunit(unitn)
   end if

#ifdef SPMD
   call mpibcast (dyn_spectral_dif2,     1, mpir8,  0, mpicom)
   call mpibcast (dyn_spectral_dif4,     1, mpir8,  0, mpicom)
   call mpibcast (dyn_spectral_divdampn, 1, mpir8,  0, mpicom)
   call mpibcast (dyn_spectral_eps     , 1, mpir8,  0, mpicom)
   call mpibcast (dyn_spectral_kmxhdc  , 1, mpiint, 0, mpicom)
#endif
    
   dif2     = dyn_spectral_dif2
   dif4     = dyn_spectral_dif4
   divdampn = dyn_spectral_divdampn
   eps      = dyn_spectral_eps
   kmxhdc   = dyn_spectral_kmxhdc

   ! Write namelist variables to logfile
   if (masterproc) then

      write(iulog,*) 'SLD Dycore Parameters:'

      if (divdampn > 0._r8) then
         write(iulog,*) '  Divergence damper for spectral dycore invoked for days 0. to ',divdampn,' of this case'
      elseif (divdampn < 0._r8) then
         call endrun ('READ_NAMELIST: divdampn must be a positive number')
      else
         write(iulog,*) '  Divergence damper for spectral dycore NOT invoked'
      endif

      if (kmxhdc >= plev .or. kmxhdc < 0) then
         call endrun ('DYN_SLD_READNL:  ERROR:  KMXHDC must be between 0 and plev-1')
      end if

      write(iulog,9108) eps, dif2, dif4, kmxhdc
   end if

9108 format('   Time filter coefficient (EPS)                 ',f10.3,/,&
            '   DEL2 Horizontal diffusion coefficient (DIF2)  ',e10.3/, &
            '   DEL4 Horizontal diffusion coefficient (DIF4)  ',e10.3/, &
            '   Number of levels Courant limiter applied      ',i10)

end subroutine dyn_sld_readnl

end module sld_control_mod
