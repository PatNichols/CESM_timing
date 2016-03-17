!======================================================================
! Module implements passive test tracers.
!
! Two options:
!
! 1) Specify only the number of tracers desired by setting the -nadv_tt option to configure.
!    This results in setting up the desired number of tracers making use of the tracers_suite
!    module to generate tracer names and initialize mixing ratios.
!
! 2) Specify both the number of tracers using configure's -nadv_tt option, and specify
!    the same number of tracer names using the test_tracer_names namelist variable.  This
!    is a set of passive tracers that are initialized by reading values from the IC file.
!
!======================================================================

module tracers

use shr_kind_mod,    only: r8 => shr_kind_r8
use spmd_utils,      only: masterproc
use ppgrid,          only: pver
use namelist_utils,  only: find_group_name
use units,           only: getunit, freeunit
use mpishorthand
use physconst,       only: mwdry, cpair
use constituents,    only: cnst_add, cnst_name, cnst_longname
use tracers_suite,   only: get_tracer_name, init_cnst_tr
use cam_history,     only: addfld, add_default
use cam_logfile,     only: iulog
use cam_abortutils,  only: endrun

implicit none
private
save

public :: &
   tracers_readnl,            &! read namelist
   tracers_register,          &! register constituent
   tracers_implements_cnst,   &! true if named constituent is implemented by this package
   tracers_init_cnst,         &! initialize constituent field
   tracers_init               ! initialize history fields, datasets

integer, parameter :: num_names_max=30

! Data from namelist variables
integer           :: test_tracer_num
character(len=16) :: test_tracer_names(num_names_max)

logical :: tracers_flag       = .false.  ! true => turn on test tracer code
logical :: tracers_suite_flag = .false.  ! true => test tracers provided by tracers_suite module

integer :: ixtrct=-999                   ! index of 1st constituent
logical :: debug = .false.
  
!======================================================================
contains
!======================================================================

subroutine tracers_readnl(nlfile)

   ! args
   character(len=*), intent(in) :: nlfile  ! filepath for file containing namelist input

   ! Local variables
   integer :: unitn, ierr
   integer :: i
   integer :: num_names
   character(len=*), parameter :: subname = 'tracers_readnl'

   namelist /test_tracers_nl/ test_tracer_num, test_tracer_names
   !-----------------------------------------------------------------------------

   test_tracer_names = (/ (' ', i=1,num_names_max) /)

   if (masterproc) then
      unitn = getunit()
      open( unitn, file=trim(nlfile), status='old' )
      call find_group_name(unitn, 'test_tracers_nl', status=ierr)
      if (ierr == 0) then
         read(unitn, test_tracers_nl, iostat=ierr)
         if (ierr /= 0) then
            call endrun(subname // ':: ERROR reading namelist')
         end if
      end if
      close(unitn)
      call freeunit(unitn)
   end if

#ifdef SPMD
   call mpibcast(test_tracer_names, len(test_tracer_names)*num_names_max, mpichar, 0, mpicom)
   call mpibcast(test_tracer_num,   1,                                    mpiint,  0, mpicom)
#endif

   ! If any tracers have been specified then turn on the tracers module
   if (test_tracer_num > 0) then
      tracers_flag = .true.
   else
      return
   end if

   ! Determine the number of tracer names supplied:
   num_names = 0
   do i = 1, num_names_max
      if (len_trim(test_tracer_names(i)) > 0) then
         num_names = num_names + 1
      else
         exit
      end if
   end do

   if (num_names > 0) then
      ! If test_tracer_names have been specified, the test_tracer_num should
      ! equal the number of names supplied.
      if (num_names /= test_tracer_num) then
         write(iulog, *) subname//' number of names, number of tracers: ', num_names, test_tracer_num
         call endrun(subname // ':: number of names does not match number of tracers')
      end if
   else
      ! If no names have been supplied then
      ! the tracers will be provided by the tracers_suite module.
      tracers_suite_flag = .true.
   end if

   ! Print summary to log file
   if (masterproc) then

      write(iulog, *) 'Test Tracers Module'
      write(iulog, *) '  Number of Test Tracers:', test_tracer_num
      if (tracers_suite_flag) then
         write(iulog, *) '  Tracers will be provided by tracers_suite module.'
      else
         write(iulog, *) '  Tracers will be initialized from the IC file variables:'
         write(iulog, *) '  '//test_tracer_names(:num_names)
      end if
   end if

end subroutine  tracers_readnl

!======================================================================

subroutine tracers_register()

   ! Register advected tracers.
   
   ! Local variables
   integer  :: m, mm
   logical  :: read_from_file
   real(r8) :: minc
   character(len=16) :: name
   !-----------------------------------------------------------------------

   if (.not. tracers_flag) return

   minc = -1.e36_r8   ! min mixing ratio (disable qneg3)
      
   ! Set whether the tracer initial values are read from the IC file
   read_from_file = .true.
   if (tracers_suite_flag) read_from_file = .false.

   do m = 1, test_tracer_num 

      if (tracers_suite_flag) then
         name = get_tracer_name(m)  ! get name from suite file
      else
         name = test_tracer_names(m)
      end if
         
      ! add constituent name to list of advected, save index number ixtrct
      call cnst_add(name, mwdry, cpair, minc, mm, &  
                    readiv=read_from_file, mixtype='dry')
      if (m == 1) ixtrct = mm  ! save index number of first tracer
   end do

end subroutine tracers_register

!======================================================================

function tracers_implements_cnst(name)

   ! return true if specified constituent is implemented by this package
  
   ! Arguments
   character(len=*), intent(in) :: name   ! constituent name
   logical :: tracers_implements_cnst        ! return value

   ! Local variables
   integer :: m
   character(len=16) :: trc_name
   !-----------------------------------------------------------------------

   tracers_implements_cnst = .false.
   if (.not. tracers_flag) return

   do m = 1, test_tracer_num

      if (tracers_suite_flag) then
         trc_name = get_tracer_name(m)
      else
         trc_name = test_tracer_names(m)
      end if

      if (name == trc_name) then
         tracers_implements_cnst = .true.
         return
      end if
   end do

end function tracers_implements_cnst

!===============================================================================

subroutine tracers_init_cnst(name, q, gcid)

   ! Initialize test tracer mixing ratio

   character(len=*), intent(in)  :: name
   real(r8),         intent(out) :: q(:,:)   ! kg tracer/kg dry air (ncol,plev)
   integer,          intent(in)  :: gcid(:)  ! global column id

   ! Local
   integer m
   character(len=*), parameter :: subname = 'tracers_init_cnst'

   if (.not. tracers_flag) return

   if (.not. tracers_suite_flag) then
      ! The initial values were supposed to be read from the IC file.  This
      ! call should not have been made in that case, so it appears that a requested
      ! tracer is not on the IC file.
      write(iulog, *) subname//': ERROR: tracer ', trim(name), ' should be on IC file'
      call endrun(subname//': ERROR: tracer missing from IC file')
   else
      do m = 1, test_tracer_num
         if (name ==  get_tracer_name(m))  then
            call init_cnst_tr(m, q, gcid)
         endif
      end do
   end if
end subroutine tracers_init_cnst

!===============================================================================

subroutine tracers_init()

   ! Add tracers to history output

   ! Local
   integer m, mm
   character(len=16) :: name   ! constituent name

   if (.not. tracers_flag ) return
     
   do m = 1,test_tracer_num 
      mm = ixtrct + m - 1
      call addfld(cnst_name(mm), (/ 'lev' /), 'A', 'kg/kg', cnst_longname(mm))
      call add_default(cnst_name(mm), 1, ' ')
   end do
     
end subroutine tracers_init

!======================================================================

end module tracers
