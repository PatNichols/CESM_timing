module dadadj_cam

! CAM interfaces for the dry adiabatic adjustment parameterization

use shr_kind_mod,    only: r8=>shr_kind_r8, cs=>shr_kind_cs
use ppgrid,          only: pcols, pver, pverp
use constituents,    only: pcnst
use physconst,       only: cappa, cpair, pi
use physics_types,   only: physics_state, physics_ptend, physics_ptend_init
use cam_abortutils,  only: endrun
use cam_logfile,     only: iulog
use error_messages,  only: handle_errmsg

use spmd_utils,      only: masterproc, masterprocid, mpicom, mpi_integer
use namelist_utils,  only: find_group_name
use units,           only: getunit, freeunit

use dadadj,          only: dadadj_initial, dadadj_calc

implicit none
private
save

public :: &
   dadadj_readnl, &
   dadadj_tend

! Namelist variables
integer :: dadadj_nlvdry = 3  ! number of layers from top of model to apply the adjustment

!===============================================================================
contains
!===============================================================================

subroutine dadadj_readnl(filein)

   character(len=cs), intent(in) :: filein ! Input namelist filename

   namelist /dadadj_nl/ dadadj_nlvdry

   integer :: unitn, ierr
   character(len=*), parameter :: sub='dadadj_readnl'
   !------------------------------------------------------------------

   ! Read namelist
   if (masterproc) then
      unitn = getunit()
      open(unitn, file=trim(filein), status='old')
      call find_group_name(unitn, 'dadadj_nl', status=ierr)
      if (ierr == 0) then
         read(unitn, dadadj_nl, iostat=ierr)
         if (ierr /= 0) then
            call endrun( sub//':: ERROR reading namelist')
         end if
      end if
      close(unitn)
      call freeunit(unitn)
   end if

#ifdef SPMD
   ! Broadcast namelist variables
   call mpibcast(dadadj_nlvdry, 1, mpi_integer, masterprocid, mpicom)
#endif

   call dadadj_initial(dadadj_nlvdry, cappa)

   if (masterproc) then
      write(iulog,*)'Dry adiabatic adjustment applied to top N layers; N=', &
                    dadadj_nlvdry
   end if

end subroutine dadadj_readnl

!===============================================================================

subroutine dadadj_tend(dt, state, ptend)

   real(r8),                  intent(in)  :: dt         ! Time step [s]
   type(physics_state),       intent(in)  :: state      ! Physics state variables
   type(physics_ptend),       intent(out) :: ptend      ! parameterization tendencies

   logical :: lq(pcnst)
   integer :: ncol, icol_err
   character(len=128) :: errstring  ! Error string

    ncol  = state%ncol

    lq(:) = .FALSE.
    lq(1) = .TRUE.
    call physics_ptend_init(ptend, state%psetcols, 'dadadj', ls=.true., lq=lq)

    ! use the ptend components for temporary storate and copy state info for input to
    ! dadadj_calc which directly updates the temperature and moisture input arrays.

    ptend%s(:ncol,:pver)   = state%t(:ncol,:pver)
    ptend%q(:ncol,:pver,1) = state%q(:ncol,:pver,1)

    call dadadj_calc( &
       ncol, state%pmid, state%pint, state%pdel, ptend%s, &
       ptend%q(:,:,1), icol_err)

    if (icol_err > 0) then
       ! error exit
       write(errstring, *) &
          'dadadj_calc: No convergence in column at lat,lon:', &
          state%lat(icol_err)*180._r8/pi, state%lon(icol_err)*180._r8/pi
       call handle_errmsg(errstring, subname="dadadj_tend")
    end if

    ptend%s(:ncol,:)   = (ptend%s(:ncol,:)   - state%t(:ncol,:)  )/dt * cpair
    ptend%q(:ncol,:,1) = (ptend%q(:ncol,:,1) - state%q(:ncol,:,1))/dt

end subroutine dadadj_tend

!===============================================================================
end module dadadj_cam
