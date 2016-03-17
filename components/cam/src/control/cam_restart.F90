module cam_restart
!----------------------------------------------------------------------- 
! 
! module to handle reading and writing of the master restart files.
!
!----------------------------------------------------------------------- 
   use shr_kind_mod,     only: r8 => shr_kind_r8, cl=>shr_kind_cl
   use spmd_utils,       only: masterproc
   use ppgrid,           only: begchunk, endchunk
   use cam_control_mod,  only: restart_run, branch_run, caseid, brnch_retain_casename
   use ioFileMod,        only: getfil, opnfil
   use camsrfexch,       only: cam_in_t, cam_out_t     
   use dyn_comp,         only: dyn_import_t, dyn_export_t

#ifdef SPMD
   use mpishorthand,     only: mpicom, mpir8, mpiint, mpilog
#endif
   use units,            only: getunit, freeunit
   use shr_kind_mod,     only: shr_kind_cm
   use cam_logfile,      only: iulog
   use cam_abortutils,   only: endrun

   use pio,              only: file_desc_t, pio_global, pio_noerr, &
                               pio_seterrorhandling, pio_bcast_error, pio_internal_error, &
                               pio_inq_att, pio_def_dim, pio_enddef, &
                               pio_get_att, pio_put_att, &
                               pio_closefile, pio_offset_kind

   implicit none
   private
   save

   ! Public interfaces
   public :: &
      cam_restart_readnl, &  ! read namelist
      cam_write_restart,  &  ! Write the master restart file out
      cam_read_restart,   &  ! Read the master restart file in
      get_restcase,       &  ! Get the caseid of the restart file being read in
      get_restartdir         ! Get the directory name of the restart file being read in

   ! Private data

   integer, parameter :: uninit_int = -999999999

   character(len=cl):: pname = ' '      ! Full restart pathname
   character(len=cl)  :: caseid_prev = ' '  ! previous case name read from restart file

   ! Filename specifiers for master restart filename
   ! (%c = caseid, $y = year, $m = month, $d = day, $s = seconds in day, %t = number)
   character(len=cl) :: rfilename_spec = '%c.cam.r.%y-%m-%d-%s.nc'

   logical :: aeres                ! true => write absorptivities/emissivities to restart file

   ! Filenames used for restart or branch
   character(len=cl) :: rest_pfile = './rpointer.atm' ! Restart pointer file contains name of most recently
                                                        ! written restart file
   character(len=cl) :: cam_branch_file = ' '         ! Filepath of primary restart file for a branch run

!-----------------------------------------------------------------------

!=========================================================================================
CONTAINS
!=========================================================================================

subroutine cam_restart_readnl(nlfile)

   use namelist_utils,  only: find_group_name
   use spmd_utils,      only: mstrid=>masterprocid, mpichar=>mpi_character, mpicom
   use cam_instance,    only: inst_suffix

   character(len=*), intent(in) :: nlfile  ! filepath for file containing namelist input

   ! Local variables
   integer :: unitn, ierr
   character(len=*), parameter :: sub = 'cam_restart_readnl'

   namelist /cam_restart_nl/ cam_branch_file
   !-----------------------------------------------------------------------------

   if (masterproc) then
      unitn = getunit()
      open( unitn, file=trim(nlfile), status='old' )
      call find_group_name(unitn, 'cam_restart_nl', status=ierr)
      if (ierr == 0) then
         read(unitn, cam_restart_nl, iostat=ierr)
         if (ierr /= 0) then
            call endrun(sub//': FATAL: reading namelist')
         end if
      end if
      close(unitn)
      call freeunit(unitn)
   end if

   call mpi_bcast(cam_branch_file, len(cam_branch_file), mpichar, mstrid, mpicom, ierr)
   if (ierr /= 0) call endrun(sub//": FATAL: mpi_bcast: cam_branch_file")

   ! If branch set restart filepath to path given on namelist
   if (branch_run) call set_restart_filepath(cam_branch_file)

   ! Set pointer file name based on instance suffix
   rest_pfile = trim(rest_pfile) // trim(inst_suffix)

   ! Set template for restart filenames based on instance suffix
   rfilename_spec = '%c.cam' // trim(inst_suffix) //'.r.%y-%m-%d-%s.nc'

   if (masterproc) then
      write(iulog,*)'Summary of restart module options:'
      write(iulog,*)'  Restart pointer file is: ',trim(rest_pfile)
      if (branch_run) &
         write(iulog,*)'  Branch run will start from: ',trim(cam_branch_file)
   end if

end subroutine cam_restart_readnl

!=========================================================================================

   subroutine cam_write_restart( cam_in, cam_out, dyn_out, pbuf2d, &
	                         yr_spec, mon_spec, day_spec, sec_spec )

!----------------------------------------------------------------------- 
! 
! Purpose: 
! Write the primary, secondary, and history buffer regeneration files.
! 
! Method: 
! The cpp SPMD definition provides for the funnelling of all program i/o
! through the master processor. Processor 0 either reads restart/history
! data from the disk and distributes it to all processors, or collects
! data from all processors and writes it to disk.
! 
! Author: 
! 
!----------------------------------------------------------------------- 
     use physics_buffer,            only: physics_buffer_desc
      use cam_history,      only: write_restart_history, init_restart_history
      use radiation,        only: radiation_do
      use filenames,        only: interpret_filename_spec

      use restart_dynamics, only: write_restart_dynamics, init_restart_dynamics
      use restart_physics,  only: write_restart_physics, init_restart_physics
      use cam_pio_utils,    only: cam_pio_createfile
      use spmd_utils,       only: iam, mpicom
      !
      ! Arguments
      !
      type(cam_in_t),      intent(in) :: cam_in(begchunk:endchunk)
      type(cam_out_t),     intent(in) :: cam_out(begchunk:endchunk)
      
      type(dyn_export_t),  intent(in) :: dyn_out
      
      type(physics_buffer_desc), pointer  :: pbuf2d(:,:)

      integer            , intent(in), optional :: yr_spec         ! Simulation year
      integer            , intent(in), optional :: mon_spec        ! Simulation month
      integer            , intent(in), optional :: day_spec        ! Simulation day
      integer            , intent(in), optional :: sec_spec        ! Seconds into current simulation day
      !
      ! Local workspace
      !
      integer ioerr                 ! write error status
      character(len=cl) :: fname  ! Restart filename
      integer :: aeres_int = 0
      integer :: ierr
      type(file_desc_t) :: File


      !-----------------------------------------------------------------------
      ! Write the primary restart datasets
      !-----------------------------------------------------------------------
#ifdef DEBUG
      write(iulog,*)'Entered CAM_WRITE_RESTART: writing nstep+1=',nstep+1, ' data to restart file'
#endif
      aeres = radiation_do('aeres')
      if ( aeres ) aeres_int = 1

      !-----------------------------------------------------------------------
      ! Write the master restart dataset
      !-----------------------------------------------------------------------

      if (present(yr_spec).and.present(mon_spec).and.present(day_spec).and.present(sec_spec)) then
         fname = interpret_filename_spec( rfilename_spec, &
              yr_spec=yr_spec, mon_spec=mon_spec, day_spec=day_spec, sec_spec= sec_spec )
      else
         fname = interpret_filename_spec( rfilename_spec )
      end if

      call cam_pio_createfile(File, trim(fname), 0)

      call init_restart_dynamics(File, dyn_out)
      call init_restart_physics(File, pbuf2d)
      call init_restart_history(File)

      ierr = PIO_Put_att(File, PIO_GLOBAL, 'caseid', caseid)
      ierr = PIO_Put_att(File, PIO_GLOBAL, 'aeres', aeres_int)

      ierr = pio_enddef(File)


      !-----------------------------------------------------------------------
      ! Dynamics, physics, History
      !-----------------------------------------------------------------------

      call write_restart_dynamics(File, dyn_out)
      call write_restart_physics(File, cam_in, cam_out, pbuf2d)

      if (present(yr_spec).and.present(mon_spec).and.&
           present(day_spec).and.present(sec_spec)) then
         call write_restart_history ( File, &
              yr_spec=yr_spec, mon_spec=mon_spec, day_spec=day_spec, sec_spec= sec_spec )
      else
         call write_restart_history( File )
      end if
      call pio_closefile(File)
      !-----------------------------------------------------------------------
      ! Close the master restart file
      !-----------------------------------------------------------------------

      if (masterproc) then
	 pname = fname
         call write_rest_pfile()
      end if



   end subroutine cam_write_restart

!#######################################################################

   subroutine cam_read_restart(cam_in, cam_out, dyn_in, dyn_out, pbuf2d, stop_ymd, stop_tod, NLFileName )

!----------------------------------------------------------------------- 
! 
! Purpose: 
! Acquire and position the restart, master, primary and secondary
! datasets for a continuation run
! 
! Method: 
! 
! Author: 
! 
!-----------------------------------------------------------------------
     use physics_buffer, only: physics_buffer_desc
      use restart_physics,  only: read_restart_physics
      use restart_dynamics, only: read_restart_dynamics
      use chem_surfvals,    only: chem_surfvals_init
      use phys_grid,        only: phys_grid_init
      use camsrfexch,       only: atm2hub_alloc, hub2atm_alloc
#if (defined SPMD)
      use spmd_dyn,         only: spmdbuf
#endif
      use cam_history,      only: read_restart_history

      use cam_pio_utils,    only: cam_pio_openfile, clean_iodesc_list
      use spmd_utils,       only: iam, mpicom
      use ref_pres,         only: ref_pres_init

!
!-----------------------------------------------------------------------
!
! Arguments
!
   type(cam_in_t),     pointer     :: cam_in(:)
   type(cam_out_t),    pointer     :: cam_out(:)
   type(dyn_import_t), intent(inout) :: dyn_in
   type(dyn_export_t), intent(inout) :: dyn_out
   type(physics_buffer_desc), pointer :: pbuf2d(:,:)
   character(len=*),   intent(in)  :: NLFileName
   integer,            intent(IN)  :: stop_ymd       ! Stop date (YYYYMMDD)
   integer,            intent(IN)  :: stop_tod       ! Stop time of day (sec)
!
! Local workspace
!
   character(len=cl) :: locfn          ! Local filename
   character(len=cl+40) :: errstr
   integer :: ierr, aeres_int, xtype
   integer(pio_offset_kind) :: slen
   type(file_desc_t) :: File
   logical :: filefound
   character(len=*), parameter :: sub = 'cam_read_restart'

   aeres = .false.

   ! Only read the restart pointer file for a restart run.
   if (restart_run) then
      call read_rest_pfile
   endif
  

   !------------------------------------------------------------------------
   ! Obtain and read the master restart dataset 
   !------------------------------------------------------------------------

   call getfil (pname, locfn)
   inquire(FILE=trim(locfn), exist=filefound)

   if(.not.filefound) then
      write(errstr,*) sub//': Could not find restart file ', trim(locfn)
      call endrun(errstr)
   end if

   call cam_pio_openfile(File, trim(locfn), 0)

   ierr = pio_inq_att(File, pio_global, 'caseid', xtype, slen)
   ierr = PIO_Get_att(File, PIO_GLOBAL, 'caseid', caseid_prev)
   caseid_prev(slen+1:len(caseid_prev))=''

   ierr = PIO_Get_att(File, PIO_GLOBAL, 'aeres', aeres_int)
   if(aeres_int==1) aeres=.true.

   if (branch_run .and. caseid_prev==caseid .and. .not.brnch_retain_casename) then
      write(iulog,*) sub//': Must change case name on branch run'
      write(iulog,*) 'Prev case = ',caseid_prev,' current case = ',caseid
      call endrun(sub//': Must change case name on branch run')
   end if

      !-----------------------------------------------------------------------
      ! Dynamics, physics, History
      !-----------------------------------------------------------------------

   call initcom ()
   call read_restart_dynamics(File, dyn_in, dyn_out, NLFileName)   

   call phys_grid_init

   call hub2atm_alloc(cam_in)
   call atm2hub_alloc(cam_out)


   ! Initialize physics grid reference pressures (needed by initialize_radbuffer)
   call ref_pres_init()

   call read_restart_physics(File, cam_in, cam_out, pbuf2d)

   if (restart_run) then
      call read_restart_history ( File )
   end if

   call pio_closefile(File)
   
   !-----------------------------------------------------------------------
   ! Allocate communication buffers for collective communications
   ! between physics and dynamics, if necessary
   !-----------------------------------------------------------------------

#if (defined SPMD)
   call spmdbuf ()
#endif

   ! Initialize ghg surface values.

   call chem_surfvals_init()
   call clean_iodesc_list()

 end subroutine cam_read_restart

!#######################################################################

   subroutine write_rest_pfile
!----------------------------------------------------------------------- 
! 
! Purpose: 
!
! Write out the restart pointer file
!
!----------------------------------------------------------------------- 
   use restart_physics, only: get_abs_restart_filepath
   use cam_history,     only: get_ptapes, get_hist_restart_filepath, &
                              hstwr, get_hfilepath, nfils, mfilt
!-----------------------------------------------------------------------
   integer t      ! Tape number
   integer mtapes ! Number of tapes that are active
   integer :: nsds

   nsds = getunit()
   call opnfil(rest_pfile, nsds, 'f')
   rewind nsds
   write (nsds,'(a)') trim(pname)
   write (nsds,'(//a,a)') '# The following lists the other files needed for restarts', &
                        ' (cam only reads the first line of this file).'
   write (nsds,'(a,a)') '# The files below refer to the files needed for the master restart file:', &
                       trim(pname)
   if ( aeres )then
      write (nsds,'(a,a)') '# ', trim(get_abs_restart_filepath())
   end if
!
! History files: Need restart history files when not a time-step to write history info
! Need: history files if they are not full
!
   mtapes = get_ptapes( )
   do t=1,mtapes
      if ( .not. hstwr(t) ) then
         write (nsds,'(a,a)') '# ', trim(get_hist_restart_filepath( t ))
      end if
      if ( nfils(t) > 0 .and. nfils(t) < mfilt(t) ) then
         write (nsds,'(a,a)') '# ', trim(get_hfilepath( t ))
      end if
   end do

   close (nsds)
   call freeunit(nsds)

   write(iulog,*)'(WRITE_REST_PFILE): successfully wrote local restart pointer file ',trim(rest_pfile)
   write(iulog,'("---------------------------------------")')

   end subroutine write_rest_pfile

!#######################################################################

   subroutine read_rest_pfile
!----------------------------------------------------------------------- 
! 
! Purpose: 
!
! Read the master restart file from the restart pointer file
!
!----------------------------------------------------------------------- 

   character(len=cl) :: locfn        ! Local pathname for restart pointer file
   integer :: nsds

   nsds = getunit()
   call opnfil (rest_pfile, nsds, 'f', status="old")
   read (nsds,'(a)') pname
   
   close(nsds)
   call freeunit(nsds)

   end subroutine read_rest_pfile

!#######################################################################

!-----------------------------------------------------------------------
! BOP
!
! !ROUTINE: set_restart_filepath
!
! !DESCRIPTION: Set the filepath of the specific type of restart file.
!
!-----------------------------------------------------------------------
! !INTERFACE:
subroutine set_restart_filepath( rgpath )
!
! !PARAMETERS:
!
  character(len=*), intent(in)  :: rgpath ! Full pathname to restart file
!
! EOP
!
  if ( trim(rgpath) == '' )then
     call endrun ('set_restart_filepath: rgpath sent into subroutine is empty')
  end if
  if ( rgpath(1:1) /= '/' )then
     call endrun ('set_restart_filepath: rgpath sent into subroutine is not an absolute pathname')
  end if
  if ( len_trim(rgpath) > cl )then
     call endrun ('set_restart_filepath: rgpath is too long :'//rgpath)
  end if
  pname = trim(rgpath)
end subroutine set_restart_filepath

!#######################################################################

character(len=cl) function get_restcase()

   ! Return the caseid of the previous case (i.e., the one read from the restart file)

   if ( len_trim(caseid_prev) == 0 )then
      call endrun ('GET_RESTCASE: caseid read in is empty, is this call after cam_read_restart?')
   end if
   get_restcase = caseid_prev

end function get_restcase

!#######################################################################

!-----------------------------------------------------------------------
! BOP
!
! !FUNCTION: get_restartdir
!
! !DESCRIPTION: Get the directory of the restart file being read in
!
!-----------------------------------------------------------------------
! !INTERFACE:
character(len=cl) function get_restartdir()
  use filenames,   only: get_dir
!
! EOP
!
  ! Uses pname, so will be updated after a restart file is written out
  if ( trim(pname) == '' )then
     call endrun ('GET_RESTDIR: restart filename is empty, is this call after cam_read_restart?')
  end if
  get_restartdir = get_dir(pname)
end function get_restartdir

!=========================================================================================

end module cam_restart
