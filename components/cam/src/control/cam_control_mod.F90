module cam_control_mod
!------------------------------------------------------------------------------------------------
! 
! High level control variables.  Information received from the driver/coupler is
! stored here.
! 
!------------------------------------------------------------------------------------------------

use shr_kind_mod,     only: r8=>shr_kind_r8, cs=>shr_kind_cs, cl=>shr_kind_cl
use seq_infodata_mod, only: seq_infodata_start_type_start, seq_infodata_start_type_cont, &
                            seq_infodata_start_type_brnch

use spmd_utils,       only: masterproc
use cam_logfile,      only: iulog
use cam_abortutils,   only: endrun

implicit none
public
save

! Public Routines:
!
!   cam_ctrl_init
!   cam_ctrl_set_orbit

character(len=cl), protected :: caseid  ! case ID
character(len=cl), protected :: ctitle  ! case title

logical, protected :: initial_run  ! startup mode which only requires a minimal initial file
logical, protected :: restart_run  ! continue a previous run; requires a restart file
logical, protected :: branch_run   ! branch from a previous run; requires a restart file

logical, protected :: adiabatic         ! true => no physics
logical, protected :: ideal_phys        ! true => run "idealized" model configuration
logical, protected :: aqua_planet       ! Flag to run model in "aqua planet" mode
logical, protected :: moist_physics     ! true => moist physics enabled, i.e.,
                                        ! (.not. ideal_phys) .and. (.not. adiabatic)

logical, protected :: brnch_retain_casename ! true => branch run may use same caseid as
                                            !         the run being branched from

real(r8), protected :: eccen       ! Earth's eccentricity factor (unitless) (typically 0 to 0.1)
real(r8), protected :: obliqr      ! Earth's obliquity in radians
real(r8), protected :: lambm0      ! Mean longitude of perihelion at the 
                                   ! vernal equinox (radians)
real(r8), protected :: mvelpp      ! Earth's moving vernal equinox longitude
                                   ! of perihelion plus pi (radians)

! Temporary -- will move to appropriate location in chem code
integer :: magfield_fix_year = 1995

!================================================================================================
contains
!================================================================================================

subroutine cam_ctrl_init( &
   caseid_in, ctitle_in, start_type, adiabatic_in, ideal_phys_in, &
   aqua_planet_in, brnch_retain_casename_in)

   character(len=cl), intent(in) :: caseid_in            ! case ID
   character(len=cl), intent(in) :: ctitle_in            ! case title
   character(len=cs), intent(in) :: start_type           ! start type: initial, restart, or branch
   logical,           intent(in) :: adiabatic_in         ! true => no physics
   logical,           intent(in) :: ideal_phys_in        ! true => run "idealized" model configuration
   logical,           intent(in) :: aqua_planet_in       ! Flag to run model in "aqua planet" mode
   logical,           intent(in) :: brnch_retain_casename_in ! Flag to allow a branch to use the same
                                                             ! caseid as the run being branched from.
   integer :: nphyspkg
   integer :: unitn, ierr

   character(len=*), parameter :: sub='cam_ctrl_init'
   character(len=128) :: errmsg
   !---------------------------------------------------------------------------------------------

   caseid = caseid_in
   ctitle = ctitle_in

   initial_run = .false.
   restart_run = .false.
   branch_run  = .false.
   select case (trim(start_type))
   case (seq_infodata_start_type_start)
      initial_run = .true.
   case (seq_infodata_start_type_cont)
      restart_run = .true.
   case (seq_infodata_start_type_brnch)
      branch_run = .true.
   case default
      write(errmsg,*) sub // ': FATAL: unknown start type: ', trim(start_type)
      call endrun(errmsg)
   end select

   adiabatic   = adiabatic_in
   ideal_phys  = ideal_phys_in
   aqua_planet = aqua_planet_in

   nphyspkg = 0
   if (adiabatic)   nphyspkg = nphyspkg + 1
   if (ideal_phys)  nphyspkg = nphyspkg + 1
   if (aqua_planet) nphyspkg = nphyspkg + 1
   if (nphyspkg > 1) then
      call endrun (sub//': FATAL: Only one of ADIABATIC, IDEAL_PHYS, or AQUA_PLANET can be .true.')
   end if

   moist_physics = (.not. adiabatic) .and. (.not. ideal_phys)

   brnch_retain_casename = brnch_retain_casename_in

   if (masterproc) then
      write(iulog,*)' '
      write(iulog,*)' ------------------------------------------'
      write(iulog,*)' *********** CAM LOG OUTPUT ***************'
      write(iulog,*)' ------------------------------------------'
      if (restart_run) then
         write(iulog,*) '  Restart of an earlier run'
      else if (branch_run) then
         write(iulog,*) '  Branch of an earlier run'
      else
         write(iulog,*) '         Initial run'
      end if
      write(iulog,*) ' ********** CASE = ',trim(caseid),' **********'
      write(iulog,'(1x,a)') ctitle


      if (adiabatic)   write(iulog,*) 'Run model ADIABATICALLY (i.e. no physics)'
      if (ideal_phys)  write(iulog,*) 'Run model with IDEAL physics (Held-Suarez)'
      if (aqua_planet) write(iulog,*) 'Run model in "AQUA_PLANET" mode'

   end if

end subroutine cam_ctrl_init

!--------------------------------------------------------------------------------------------------

subroutine cam_ctrl_set_orbit(eccen_in, obliqr_in, lambm0_in, mvelpp_in)

   real(r8), intent(in) :: eccen_in
   real(r8), intent(in) :: obliqr_in
   real(r8), intent(in) :: lambm0_in
   real(r8), intent(in) :: mvelpp_in

   eccen  = eccen_in
   obliqr = obliqr_in
   lambm0 = lambm0_in
   mvelpp = mvelpp_in

end subroutine cam_ctrl_set_orbit

end module cam_control_mod
