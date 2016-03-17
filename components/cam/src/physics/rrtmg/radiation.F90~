module radiation

!---------------------------------------------------------------------------------
! Purpose:
!
! CAM interface to RRTMG
!
! Revision history:
! May  2004, D. B. Coleman,  Initial version of interface module.
! July 2004, B. Eaton,       Use interfaces from new shortwave, longwave, and ozone modules.
! Feb  2005, B. Eaton,       Add namelist variables and control of when calcs are done.
! May  2008, Mike Iacono     Initial version for RRTMG
! Nov  2010, J. Kay          Add COSP simulator calls
!---------------------------------------------------------------------------------

use shr_kind_mod,    only: r8=>shr_kind_r8
use spmd_utils,      only: masterproc
use ppgrid,          only: pcols, pver, pverp, begchunk, endchunk
use physics_types,   only: physics_state, physics_ptend
use physconst,       only: cappa
use time_manager,    only: get_nstep, is_first_restart_step, &
                           get_curr_calday, get_step_size
use cam_abortutils,  only: endrun
use error_messages,  only: handle_err
use cam_control_mod, only: lambm0, obliqr, mvelpp, eccen
use scamMod,         only: scm_crm_mode, single_column,have_cld,cldobs,&
                           have_clwp,clwpobs,have_tg,tground
use perf_mod,        only: t_startf, t_stopf
use cam_logfile,     only: iulog

use rad_constituents, only: N_DIAG, rad_cnst_get_call_list, rad_cnst_get_info
use radconstants,     only: rrtmg_sw_cloudsim_band, rrtmg_lw_cloudsim_band, nswbands, nlwbands

implicit none
private
save

public :: &
   radiation_readnl,      &! read namelist variables
   radiation_register,    &! registers radiation physics buffer fields
   radiation_nextsw_cday, &! calendar day of next radiation calculation
   radiation_do,          &! query which radiation calcs are done this timestep
   radiation_init,        &! initialization
   radiation_tend          ! compute heating rates and fluxes

integer,public, allocatable :: cosp_cnt(:)       ! counter for cosp
integer,public              :: cosp_cnt_init = 0 !initial value for cosp counter

! Namelist variables

integer :: iradsw = -1     ! freq. of shortwave radiation calc in time steps (positive)
                           ! or hours (negative).
integer :: iradlw = -1     ! frequency of longwave rad. calc. in time steps (positive)
                           ! or hours (negative).

integer :: irad_always = 0 ! Specifies length of time in timesteps (positive)
                           ! or hours (negative) SW/LW radiation will be
                           ! run continuously from the start of an
                           ! initial or restart run
logical :: use_rad_dt_cosz  = .false. ! if true, use radiation dt for all cosz calculations
logical :: spectralflux     = .false. ! calculate fluxes (up and down) per band.

! Physics buffer indices
integer :: qrs_idx      = 0 
integer :: qrl_idx      = 0 
integer :: su_idx       = 0 
integer :: sd_idx       = 0 
integer :: lu_idx       = 0 
integer :: ld_idx       = 0 
integer :: fsds_idx     = 0
integer :: fsns_idx     = 0
integer :: fsnt_idx     = 0
integer :: flns_idx     = 0
integer :: flnt_idx     = 0
integer :: cldfsnow_idx = 0 
integer :: cld_idx      = 0 

character(len=4) :: diag(0:N_DIAG) =(/'    ','_d1 ','_d2 ','_d3 ','_d4 ','_d5 ','_d6 ','_d7 ','_d8 ','_d9 ','_d10'/)

logical :: dohirs = .false. ! diagnostic  brightness temperatures at the top of the
                            ! atmosphere for 7 TOVS/HIRS channels (2,4,6,8,10,11,12) and 4 TOVS/MSU 
                            ! channels (1,2,3,4).
integer :: ihirsfq = 1      ! frequency (timesteps) of brightness temperature calcs

! averaging time interval for zenith angle
real(r8) :: dt_avg = 0._r8

!===============================================================================
contains
!===============================================================================

subroutine radiation_readnl(nlfile)

   ! Read radiation_nl namelist group.

   use namelist_utils,  only: find_group_name
   use units,           only: getunit, freeunit
   use mpishorthand

   character(len=*), intent(in) :: nlfile  ! filepath for file containing namelist input

   ! Local variables
   integer :: unitn, ierr
   integer :: dtime      ! timestep size
   character(len=*), parameter :: subname = 'radiation_readnl'

   namelist /radiation_nl/ iradsw, iradlw, irad_always, &
                           use_rad_dt_cosz, spectralflux
   !-----------------------------------------------------------------------------

   if (masterproc) then
      unitn = getunit()
      open( unitn, file=trim(nlfile), status='old' )
      call find_group_name(unitn, 'radiation_nl', status=ierr)
      if (ierr == 0) then
         read(unitn, radiation_nl, iostat=ierr)
         if (ierr /= 0) then
            call endrun(subname // ':: ERROR reading namelist')
         end if
      end if
      close(unitn)
      call freeunit(unitn)
   end if

#ifdef SPMD
   ! Broadcast namelist variables
   call mpibcast (iradsw,          1,  mpiint,  0, mpicom)
   call mpibcast (iradlw,          1,  mpiint,  0, mpicom)
   call mpibcast (irad_always,     1,  mpiint,  0, mpicom)
   call mpibcast (use_rad_dt_cosz, 1,  mpilog,  0, mpicom)
   call mpibcast (spectralflux,    1,  mpilog,  0, mpicom)
#endif

   ! Convert iradsw, iradlw and irad_always from hours to timesteps if necessary
   dtime  = get_step_size()
   if (iradsw      < 0) iradsw      = nint((-iradsw     *3600._r8)/dtime)
   if (iradlw      < 0) iradlw      = nint((-iradlw     *3600._r8)/dtime)
   if (irad_always < 0) irad_always = nint((-irad_always*3600._r8)/dtime)

   !----------------------------------------------------------------------- 
   ! Print runtime options to log.
   !-----------------------------------------------------------------------

   if (masterproc) then
      write(iulog,*) 'RRTMG radiation scheme parameters:'
      write(iulog,10) iradsw, iradlw, irad_always, use_rad_dt_cosz, spectralflux
   end if

10 format('  Frequency (timesteps) of Shortwave Radiation calc:  ',i5/, &
          '  Frequency (timesteps) of Longwave Radiation calc:   ',i5/, &
          '  SW/LW calc done every timestep for first N steps. N=',i5/, &
          '  Use average zenith angle:                           ',l5/, &
          '  Output spectrally resolved fluxes:                  ',l5/)

end subroutine radiation_readnl

!================================================================================================

subroutine radiation_register

   ! Register radiation fields in the physics buffer

   use physics_buffer, only: pbuf_add_field, dtype_r8
   use radiation_data, only: rad_data_register

   call pbuf_add_field('QRS' , 'global',dtype_r8,(/pcols,pver/), qrs_idx) ! shortwave radiative heating rate 
   call pbuf_add_field('QRL' , 'global',dtype_r8,(/pcols,pver/), qrl_idx) ! longwave  radiative heating rate 

   call pbuf_add_field('FSDS' , 'global',dtype_r8,(/pcols/), fsds_idx) ! Surface solar downward flux

   call pbuf_add_field('FSNS' , 'global',dtype_r8,(/pcols/), fsns_idx) ! Surface net shortwave flux
   call pbuf_add_field('FSNT' , 'global',dtype_r8,(/pcols/), fsnt_idx) ! Top-of-model net shortwave flux
   call pbuf_add_field('FLNS' , 'global',dtype_r8,(/pcols/), flns_idx) ! Surface net longwave flux
   call pbuf_add_field('FLNT' , 'global',dtype_r8,(/pcols/), flnt_idx) ! Top-of-model net longwave flux

   ! If the namelist has been configured for preserving the spectral fluxes, then create
   ! physics buffer variables to store the results.
   if (spectralflux) then
      call pbuf_add_field('SU'  , 'global',dtype_r8,(/pcols,pverp,nswbands/), su_idx) ! shortwave upward flux (per band)
      call pbuf_add_field('SD'  , 'global',dtype_r8,(/pcols,pverp,nswbands/), sd_idx) ! shortwave downward flux (per band)
      call pbuf_add_field('LU'  , 'global',dtype_r8,(/pcols,pverp,nlwbands/), lu_idx) ! longwave upward flux (per band)
      call pbuf_add_field('LD'  , 'global',dtype_r8,(/pcols,pverp,nlwbands/), ld_idx) ! longwave downward flux (per band)
   end if

   call rad_data_register()

end subroutine radiation_register

!================================================================================================

function radiation_do(op, timestep)
!----------------------------------------------------------------------- 
! Purpose: Returns true if the specified operation is done this timestep.
!-----------------------------------------------------------------------

   character(len=*), intent(in) :: op             ! name of operation
   integer, intent(in), optional:: timestep
   logical                      :: radiation_do   ! return value

   ! Local variables
   integer :: nstep             ! current timestep number
   !-----------------------------------------------------------------------

   if (present(timestep)) then
      nstep = timestep
   else
      nstep = get_nstep()
   end if

   select case (op)

   case ('sw') ! do a shortwave heating calc this timestep?
      radiation_do = nstep == 0  .or.  iradsw == 1                     &
                    .or. (mod(nstep-1,iradsw) == 0  .and.  nstep /= 1) &
                    .or. nstep <= irad_always

   case ('lw') ! do a longwave heating calc this timestep?
      radiation_do = nstep == 0  .or.  iradlw == 1                     &
                    .or. (mod(nstep-1,iradlw) == 0  .and.  nstep /= 1) &
                    .or. nstep <= irad_always

   case ('aeres') ! write absorptivity/emissivity to restart file this timestep?
      ! for RRTMG there is no abs/ems restart file
      radiation_do = .false.
         
   case default
      call endrun('radiation_do: unknown operation:'//op)

   end select
end function radiation_do

!================================================================================================

real(r8) function radiation_nextsw_cday()
  
!----------------------------------------------------------------------- 
! Purpose: Returns calendar day of next sw radiation calculation
!-----------------------------------------------------------------------

   ! Local variables
   integer :: nstep      ! timestep counter
   logical :: dosw       ! true => do shosrtwave calc   
   integer :: offset     ! offset for calendar day calculation
   integer :: dTime      ! integer timestep size
   real(r8):: calday     ! calendar day of 
   !-----------------------------------------------------------------------

   radiation_nextsw_cday = -1._r8
   dosw   = .false.
   nstep  = get_nstep()
   dtime  = get_step_size()
   offset = 0
   do while (.not. dosw)
      nstep = nstep + 1
      offset = offset + dtime
      if (radiation_do('sw', nstep)) then
         radiation_nextsw_cday = get_curr_calday(offset=offset) 
         dosw = .true.
      end if
   end do
   if(radiation_nextsw_cday == -1._r8) then
      call endrun('error in radiation_nextsw_cday')
   end if
        
end function radiation_nextsw_cday

!================================================================================================

  subroutine radiation_init(pbuf2d)
!-----------------------------------------------------------------------
!
! Initialize the radiation parameterization, add fields to the history buffer
! 
!-----------------------------------------------------------------------
    use physics_buffer, only: pbuf_get_index
    use cam_history,    only: addfld, add_default, horiz_only
    use physconst,      only: pstd, mwdry, mwco2, mwo3
    use phys_control,   only: phys_getopts
    use cospsimulator_intr, only: docosp, cospsimulator_intr_init
    use radsw,          only: radsw_init
    use radlw,          only: radlw_init
    use hirsbt,         only: hirsbt_init
    use hirsbtpar,      only: hirsname, msuname

    use radiation_data, only: rad_data_init
    use modal_aer_opt,  only: modal_aer_opt_init
    use rrtmg_state,    only: rrtmg_state_init
    use physics_buffer, only: physics_buffer_desc
    use time_manager,   only: get_step_size



    ! args
    type(physics_buffer_desc), pointer :: pbuf2d(:,:)

    integer :: icall, nmodes
    logical :: active_calls(0:N_DIAG)
    integer :: nstep                       ! current timestep number
    logical :: history_amwg                ! output the variables used by the AMWG diag package
    logical :: history_vdiag               ! output the variables used by the AMWG variability diag package
    logical :: history_budget              ! output tendencies and state variables for CAM4
                                           ! temperature, water vapor, cloud ice and cloud
                                           ! liquid budgets.
    integer :: history_budget_histfile_num ! output history file number for budget fields
    integer :: err

    integer  :: dtime
    !-----------------------------------------------------------------------
    
    call rrtmg_state_init()

    call rad_data_init(pbuf2d) ! initialize output fields for offline driver

    call radsw_init()
    call radlw_init()

    ! Set the radiation timestep for cosz calculations if requested using the adjusted iradsw value from radiation
    if (use_rad_dt_cosz)  then
      dtime  = get_step_size()
      dt_avg = iradsw*dtime
    end if

    call phys_getopts(history_amwg_out   = history_amwg,    &
                      history_vdiag_out  = history_vdiag,   &
                      history_budget_out = history_budget,  &
                      history_budget_histfile_num_out = history_budget_histfile_num)

    ! Determine whether modal aerosols are affecting the climate, and if so
    ! then initialize the modal aerosol optics module
    call rad_cnst_get_info(0, nmodes=nmodes)
    if (nmodes > 0) call modal_aer_opt_init()

    call hirsbt_init()

    ! "irad_always" is number of time steps to execute radiation continuously from start of
    ! initial OR restart run

    nstep = get_nstep()
    if ( irad_always > 0) then
       nstep       = get_nstep()
       irad_always = irad_always + nstep
    end if


    if (docosp) call cospsimulator_intr_init

    
    allocate(cosp_cnt(begchunk:endchunk))
    if (is_first_restart_step()) then
      cosp_cnt(begchunk:endchunk)=cosp_cnt_init
    else
      cosp_cnt(begchunk:endchunk)=0     
    end if


    ! Shortwave radiation

    call addfld('TOT_CLD_VISTAU',  (/ 'lev' /), 'A',   '1', 'Total gbx cloud extinction visible sw optical depth', &
                                                       sampling_seq='rad_lwsw', flag_xyfill=.true.)
    call addfld('TOT_ICLD_VISTAU', (/ 'lev' /), 'A',  '1', 'Total in-cloud extinction visible sw optical depth', &
                                                       sampling_seq='rad_lwsw', flag_xyfill=.true.)
    call addfld('LIQ_ICLD_VISTAU', (/ 'lev' /), 'A',  '1', 'Liquid in-cloud extinction visible sw optical depth', &
                                                       sampling_seq='rad_lwsw', flag_xyfill=.true.)
    call addfld('ICE_ICLD_VISTAU', (/ 'lev' /), 'A',  '1', 'Ice in-cloud extinction visible sw optical depth', &
                                                       sampling_seq='rad_lwsw', flag_xyfill=.true.)

    ! get list of active radiation calls
    call rad_cnst_get_call_list(active_calls)

    call addfld ('FSNR',horiz_only,    'A','W/m2','Net solar flux at tropopause', sampling_seq='rad_lwsw')
    call addfld ('FLNR',horiz_only,    'A','W/m2','Net longwave flux at tropopause', sampling_seq='rad_lwsw')

     do icall = 0, N_DIAG

       if (active_calls(icall)) then

          call addfld('SOLIN'//diag(icall),    horiz_only,   'A', 'W/m2', 'Solar insolation', sampling_seq='rad_lwsw')
          call addfld('SOLL'//diag(icall),     horiz_only,   'A', 'W/m2', 'Solar downward near infrared direct  to surface', &
                                                                                 sampling_seq='rad_lwsw')
          call addfld('SOLS'//diag(icall),     horiz_only,   'A', 'W/m2', 'Solar downward visible direct  to surface',       &
                                                                                 sampling_seq='rad_lwsw')
          call addfld('SOLLD'//diag(icall),    horiz_only,   'A', 'W/m2', 'Solar downward near infrared diffuse to surface', &
                                                                                 sampling_seq='rad_lwsw')
          call addfld('SOLSD'//diag(icall),    horiz_only,   'A', 'W/m2', 'Solar downward visible diffuse to surface',       &
                                                                                 sampling_seq='rad_lwsw')
          call addfld('QRS'//diag(icall),      (/ 'lev' /),  'A', 'K/s',  'Solar heating rate', sampling_seq='rad_lwsw')
          call addfld('QRSC'//diag(icall),     (/ 'lev' /),  'A', 'K/s',  'Clearsky solar heating rate',                     &
                                                                                 sampling_seq='rad_lwsw')
          call addfld('FSNS'//diag(icall),     horiz_only,   'A', 'W/m2', 'Net solar flux at surface',                       &
                                                                                 sampling_seq='rad_lwsw')
          call addfld('FSNT'//diag(icall),     horiz_only,   'A', 'W/m2', 'Net solar flux at top of model',                  &
                                                                                 sampling_seq='rad_lwsw')
          call addfld('FSNTOA'//diag(icall),   horiz_only,   'A', 'W/m2', 'Net solar flux at top of atmosphere',             &
                                                                                 sampling_seq='rad_lwsw')
          call addfld('FSUTOA'//diag(icall),   horiz_only,   'A', 'W/m2', 'Upwelling solar flux at top of atmosphere',       &
                                                                                 sampling_seq='rad_lwsw')
          call addfld('FSNTOAC'//diag(icall),  horiz_only,   'A', 'W/m2', 'Clearsky net solar flux at top of atmosphere',    &
                                                                                 sampling_seq='rad_lwsw')
          call addfld('FSN200'//diag(icall),   horiz_only,   'A', 'W/m2', 'Net shortwave flux at 200 mb',                    &
                                                                                 sampling_seq='rad_lwsw')
          call addfld('FSN200C'//diag(icall),  horiz_only,   'A', 'W/m2', 'Clearsky net shortwave flux at 200 mb',           &
                                                                                 sampling_seq='rad_lwsw')
          call addfld('FSNTC'//diag(icall),    horiz_only,   'A', 'W/m2', 'Clearsky net solar flux at top of model',         &
                                                                                 sampling_seq='rad_lwsw')
          call addfld('FSNSC'//diag(icall),    horiz_only,   'A', 'W/m2', 'Clearsky net solar flux at surface',              &
                                                                                 sampling_seq='rad_lwsw')
          call addfld('FSDSC'//diag(icall),    horiz_only,   'A', 'W/m2', 'Clearsky downwelling solar flux at surface',      &
                                                                                 sampling_seq='rad_lwsw')
          call addfld('FSDS'//diag(icall),     horiz_only,   'A', 'W/m2', 'Downwelling solar flux at surface',               &
                                                                                 sampling_seq='rad_lwsw')
          call addfld('FUS'//diag(icall),      (/ 'ilev' /), 'I', 'W/m2', 'Shortwave upward flux')
          call addfld('FDS'//diag(icall),      (/ 'ilev' /), 'I', 'W/m2', 'Shortwave downward flux')
          call addfld('FUSC'//diag(icall),     (/ 'ilev' /), 'I', 'W/m2', 'Shortwave clear-sky upward flux')
          call addfld('FDSC'//diag(icall),     (/ 'ilev' /), 'I', 'W/m2', 'Shortwave clear-sky downward flux')
          call addfld('FSNIRTOA'//diag(icall), horiz_only,   'A', 'W/m2',                                                    &
                  'Net near-infrared flux (Nimbus-7 WFOV) at top of atmosphere', sampling_seq='rad_lwsw')
          call addfld('FSNRTOAC'//diag(icall), horiz_only,   'A', 'W/m2',                                                    &
                      'Clearsky net near-infrared flux (Nimbus-7 WFOV) at top of atmosphere', sampling_seq='rad_lwsw')
          call addfld('FSNRTOAS'//diag(icall), horiz_only,   'A', 'W/m2',                                                    &
                 'Net near-infrared flux (>= 0.7 microns) at top of atmosphere', sampling_seq='rad_lwsw')
          call addfld('SWCF'//diag(icall),     horiz_only,   'A', 'W/m2', 'Shortwave cloud forcing', sampling_seq='rad_lwsw')

          if (history_amwg) then
             call add_default('SOLIN'//diag(icall),   1, ' ')
             call add_default('QRS'//diag(icall),     1, ' ')
             call add_default('FSNS'//diag(icall),    1, ' ')
             call add_default('FSNT'//diag(icall),    1, ' ')
             call add_default('FSNTOA'//diag(icall),  1, ' ')
             call add_default('FSUTOA'//diag(icall),  1, ' ')
             call add_default('FSNTOAC'//diag(icall), 1, ' ')
             call add_default('FSNTC'//diag(icall),   1, ' ')
             call add_default('FSNSC'//diag(icall),   1, ' ')
             call add_default('FSDSC'//diag(icall),   1, ' ')
             call add_default('FSDS'//diag(icall),    1, ' ')
             call add_default('SWCF'//diag(icall),    1, ' ')
          endif

       end if
    end do


    if (single_column .and. scm_crm_mode) then
       call add_default ('FUS     ', 1, ' ')
       call add_default ('FUSC    ', 1, ' ')
       call add_default ('FDS     ', 1, ' ')
       call add_default ('FDSC    ', 1, ' ')
    endif


    ! Longwave radiation

    do icall = 0, N_DIAG

       if (active_calls(icall)) then

          call addfld('QRL'//diag(icall),     (/ 'lev' /), 'A', 'K/s',  'Longwave heating rate', sampling_seq='rad_lwsw')
          call addfld('QRLC'//diag(icall),    (/ 'lev' /), 'A', 'K/s',  'Clearsky longwave heating rate',                   &
                                                                           sampling_seq='rad_lwsw')
          call addfld('FLDS'//diag(icall),    horiz_only,  'A', 'W/m2', 'Downwelling longwave flux at surface',             &
                                                                           sampling_seq='rad_lwsw')
          call addfld('FLDSC'//diag(icall),   horiz_only,  'A', 'W/m2', 'Clearsky Downwelling longwave flux at surface',    &
                                                                           sampling_seq='rad_lwsw')
          call addfld('FLNS'//diag(icall),    horiz_only,  'A', 'W/m2', 'Net longwave flux at surface',                     &
                                                                           sampling_seq='rad_lwsw')
          call addfld('FLNT'//diag(icall),    horiz_only,  'A', 'W/m2', 'Net longwave flux at top of model',                &
                                                                           sampling_seq='rad_lwsw')
          call addfld('FLUT'//diag(icall),    horiz_only,  'A', 'W/m2', 'Upwelling longwave flux at top of model',          &
                                                                           sampling_seq='rad_lwsw')
          call addfld('FLUTC'//diag(icall),   horiz_only,  'A', 'W/m2', 'Clearsky upwelling longwave flux at top of model', &
                                                                           sampling_seq='rad_lwsw')
          call addfld('FLNTC'//diag(icall),   horiz_only,  'A', 'W/m2', 'Clearsky net longwave flux at top of model',       &
                                                                           sampling_seq='rad_lwsw')
          call addfld('LWCF'//diag(icall),    horiz_only,  'A', 'W/m2', 'Longwave cloud forcing', sampling_seq='rad_lwsw')
          call addfld('FLN200'//diag(icall),  horiz_only,  'A', 'W/m2', 'Net longwave flux at 200 mb',                      &
                                                                           sampling_seq='rad_lwsw')
          call addfld('FLN200C'//diag(icall), horiz_only,  'A', 'W/m2', 'Clearsky net longwave flux at 200 mb',             &
                                                                           sampling_seq='rad_lwsw')
          call addfld('FLNSC'//diag(icall),   horiz_only,  'A', 'W/m2', 'Clearsky net longwave flux at surface',            &
                                                                           sampling_seq='rad_lwsw')
          call addfld('FUL'//diag(icall),     (/ 'ilev' /),'I', 'W/m2', 'Longwave upward flux')
          call addfld('FDL'//diag(icall),     (/ 'ilev' /),'I', 'W/m2', 'Longwave downward flux')
          call addfld('FULC'//diag(icall),    (/ 'ilev' /),'I', 'W/m2', 'Longwave clear-sky upward flux')
          call addfld('FDLC'//diag(icall),    (/ 'ilev' /),'I', 'W/m2', 'Longwave clear-sky downward flux')

          if (history_amwg) then
             call add_default('QRL'//diag(icall),   1, ' ')
             call add_default('FLNS'//diag(icall),  1, ' ')
             call add_default('FLDS'//diag(icall),  1, ' ')
             call add_default('FLNT'//diag(icall),  1, ' ')
             call add_default('FLUT'//diag(icall),  1, ' ')
             call add_default('FLUTC'//diag(icall), 1, ' ')
             call add_default('FLNTC'//diag(icall), 1, ' ')
             call add_default('FLNSC'//diag(icall), 1, ' ')
             call add_default('LWCF'//diag(icall),  1, ' ')
          endif

       end if
    end do

    call addfld('EMIS', (/ 'lev' /), 'A', '1', 'Cloud longwave emissivity')

    if (single_column.and.scm_crm_mode) then
       call add_default ('FUL     ', 1, ' ')
       call add_default ('FULC    ', 1, ' ')
       call add_default ('FDL     ', 1, ' ')
       call add_default ('FDLC    ', 1, ' ')
    endif

    ! HIRS/MSU diagnostic brightness temperatures
    if (dohirs) then
       call addfld (hirsname(1),horiz_only,'A','K','HIRS CH2 infra-red brightness temperature')
       call addfld (hirsname(2),horiz_only,'A','K','HIRS CH4 infra-red brightness temperature')
       call addfld (hirsname(3),horiz_only,'A','K','HIRS CH6 infra-red brightness temperature')
       call addfld (hirsname(4),horiz_only,'A','K','HIRS CH8 infra-red brightness temperature')
       call addfld (hirsname(5),horiz_only,'A','K','HIRS CH10 infra-red brightness temperature')
       call addfld (hirsname(6),horiz_only,'A','K','HIRS CH11 infra-red brightness temperature')
       call addfld (hirsname(7),horiz_only,'A','K','HIRS CH12 infra-red brightness temperature')
       call addfld (msuname(1),horiz_only,'A','K','MSU CH1 microwave brightness temperature')
       call addfld (msuname(2),horiz_only,'A','K','MSU CH2 microwave brightness temperature')
       call addfld (msuname(3),horiz_only,'A','K','MSU CH3 microwave brightness temperature')
       call addfld (msuname(4),horiz_only,'A','K','MSU CH4 microwave brightness temperature')
       call add_default (hirsname(1), 1, ' ')
       call add_default (hirsname(2), 1, ' ')
       call add_default (hirsname(3), 1, ' ')
       call add_default (hirsname(4), 1, ' ')
       call add_default (hirsname(5), 1, ' ')
       call add_default (hirsname(6), 1, ' ')
       call add_default (hirsname(7), 1, ' ')
       call add_default (msuname(1), 1, ' ')
       call add_default (msuname(2), 1, ' ')
       call add_default (msuname(3), 1, ' ')
       call add_default (msuname(4), 1, ' ')
    end if

    ! Heating rate needed for d(theta)/dt computation
    call addfld ('HR',(/ 'lev' /), 'A','K/s','Heating rate needed for d(theta)/dt computation')

    if ( history_budget .and. history_budget_histfile_num > 1 ) then
       call add_default ('QRL     ', history_budget_histfile_num, ' ')
       call add_default ('QRS     ', history_budget_histfile_num, ' ')
    end if

    if (history_vdiag) then
       call add_default('FLUT', 2, ' ')
       call add_default('FLUT', 3, ' ')
    end if

    cldfsnow_idx = pbuf_get_index('CLDFSNOW',errcode=err)
    cld_idx      = pbuf_get_index('CLD')

    if (cldfsnow_idx > 0) then
       call addfld ('CLDFSNOW',(/ 'lev' /),'I','1','CLDFSNOW',flag_xyfill=.true.)
       call addfld('SNOW_ICLD_VISTAU', (/ 'lev' /), 'A', '1', 'Snow in-cloud extinction visible sw optical depth', &
                                                       sampling_seq='rad_lwsw', flag_xyfill=.true.)
    endif

  end subroutine radiation_init

!===============================================================================
  
  subroutine radiation_tend( state, ptend, pbuf, &
       cam_out, cam_in, &
       icefrac, snowh, &
       net_flx, rd, do_output)

    !----------------------------------------------------------------------- 
    ! 
    ! Purpose: 
    ! Driver for radiation computation.
    ! 
    ! Method: 
    ! Radiation uses cgs units, so conversions must be done from
    ! model fields to radiation fields.
    !
    ! Revision history:
    ! May 2004    D.B. Coleman     Merge of code from radctl.F90 and parts of tphysbc.F90.
    ! 2004-08-09  B. Eaton         Add pointer variables for constituents.
    ! 2004-08-24  B. Eaton         Access O3 and GHG constituents from chem_get_cnst.
    ! 2004-08-30  B. Eaton         Replace chem_get_cnst by rad_constituent_get.
    ! 2007-11-05  M. Iacono        Install rrtmg_lw and sw as radiation model.
    ! 2007-12-27  M. Iacono        Modify to use CAM cloud optical properties with rrtmg.
    !-----------------------------------------------------------------------


    use physics_buffer, only : physics_buffer_desc, pbuf_get_field, pbuf_old_tim_idx
    
    use phys_grid,       only: get_rlat_all_p, get_rlon_all_p
    use physics_types,   only: physics_state, physics_ptend
    use cospsimulator_intr, only: docosp, cospsimulator_intr_run, cosp_nradsteps
    use time_manager,    only: get_curr_calday
    use camsrfexch,      only: cam_out_t, cam_in_t
    use cam_history,     only: outfld, hist_fld_active
    use cam_history_support, only: fillvalue
    use parrrtm,         only: nbndlw
    use parrrsw,         only: nbndsw
    use hirsbt,          only: hirsrtm
    use hirsbtpar,       only: pnb_hirs, pnf_msu, hirsname, msuname
    use radheat,         only: radheat_tend
    use ppgrid
    use physconst,        only: cpair, stebol
    use radconstants,     only: nlwbands,idx_sw_diag
    use radsw,            only: rad_rrtmg_sw
    use radlw,            only: rad_rrtmg_lw
    use rad_constituents, only: rad_cnst_get_gas, rad_cnst_out, oldcldoptics, &
                                liqcldoptics, icecldoptics
    use aer_rad_props,    only: aer_rad_props_sw, aer_rad_props_lw
    use interpolate_data, only: vertinterp
    use cloud_rad_props,  only: get_ice_optics_sw, get_liquid_optics_sw, liquid_cloud_get_rad_props_lw, &
               ice_cloud_get_rad_props_lw, cloud_rad_props_get_lw, snow_cloud_get_rad_props_lw, get_snow_optics_sw
    use slingo,           only: slingo_liq_get_rad_props_lw, slingo_liq_optics_sw
    use ebert_curry,      only: ec_ice_optics_sw, ec_ice_get_rad_props_lw
    use rad_solar_var,    only: get_variability
    use radiation_data,   only: rad_data_write
    use rrtmg_state,      only: rrtmg_state_create, rrtmg_state_update, rrtmg_state_destroy, rrtmg_state_t, num_rrtmg_levs
    use tropopause,       only: tropopause_find, TROP_ALG_HYBSTOB, TROP_ALG_CLIMATE
    use orbit,            only: zenith
    use radiation_utils,  only: rad_diagdata_type

    ! Arguments
    real(r8), intent(in)    :: icefrac(pcols)   ! land fraction
    real(r8), intent(in)    :: snowh(pcols)     ! Snow depth (liquid water equivalent)
    real(r8), intent(inout) :: net_flx(pcols)

    type(rad_diagdata_type), intent(inout) :: rd

    type(physics_state), intent(in), target :: state
    type(physics_ptend), intent(out)        :: ptend
    
    type(physics_buffer_desc), pointer      :: pbuf(:)
    type(cam_out_t),     intent(inout)      :: cam_out
    type(cam_in_t),      intent(in)         :: cam_in

    ! Optional variables
    logical, intent(in), optional :: do_output ! turn on/off writing output - default: output is written out

    ! Local variables
  
    logical :: dosw, dolw
    integer nstep                       ! current timestep number

    real(r8) britemp(pcols,pnf_msu)     ! Microwave brightness temperature
    real(r8) tb_ir(pcols,pnb_hirs)      ! Infrared brightness temperature
    real(r8) ts(pcols)                  ! surface temperature
    real(r8) pintmb(pcols,pverp)        ! Model interface pressures (hPa)
    real(r8) oro(pcols)                 ! Land surface flag, sea=0, land=1

    integer nmxrgn(pcols)                      ! Number of maximally overlapped regions
    real(r8) pmxrgn(pcols,pverp)               ! Maximum values of pressure for each
                                               !    maximally overlapped region.
                                               !    0->pmxrgn(i,1) is range of pressure for
                                               !    1st region,pmxrgn(i,1)->pmxrgn(i,2) for
                                               !    2nd region, etc
    real(r8) emis(pcols,pver)                  ! Cloud longwave emissivity
    real(r8) :: ftem(pcols,pver)               ! Temporary workspace for outfld variables

    ! combined cloud radiative parameters are "in cloud" not "in cell"
    real(r8) :: c_cld_tau_w  (nbndsw,pcols,pver) ! cloud single scattering albedo * tau
    real(r8) :: c_cld_tau_w_g(nbndsw,pcols,pver) ! cloud assymetry parameter * w * tau
    real(r8) :: c_cld_tau_w_f(nbndsw,pcols,pver) ! cloud forward scattered fraction * w * tau
    real(r8) :: c_cld_lw_abs (nbndlw,pcols,pver) ! cloud absorption optics depth (LW)

    ! cloud radiative parameters are "in cloud" not "in cell"
    real(r8) :: cld_tau_w  (nbndsw,pcols,pver) ! cloud single scattering albedo * tau
    real(r8) :: cld_tau_w_g(nbndsw,pcols,pver) ! cloud assymetry parameter * w * tau
    real(r8) :: cld_tau_w_f(nbndsw,pcols,pver) ! cloud forward scattered fraction * w * tau

    ! cloud radiative parameters are "in cloud" not "in cell"
    real(r8) :: ice_tau_w  (nbndsw,pcols,pver) ! ice single scattering albedo * tau
    real(r8) :: ice_tau_w_g(nbndsw,pcols,pver) ! ice assymetry parameter * tau * w
    real(r8) :: ice_tau_w_f(nbndsw,pcols,pver) ! ice forward scattered fraction * tau * w
    real(r8) :: ice_lw_abs (nbndlw,pcols,pver)   ! ice absorption optics depth (LW)

    ! cloud radiative parameters are "in cloud" not "in cell"
    real(r8) :: snow_tau_w  (nbndsw,pcols,pver) ! snow single scattering albedo * tau
    real(r8) :: snow_tau_w_g(nbndsw,pcols,pver) ! snow assymetry parameter * tau * w
    real(r8) :: snow_tau_w_f(nbndsw,pcols,pver) ! snow forward scattered fraction * tau * w
    real(r8) :: gb_snow_tau        (pcols,pver) ! grid-box mean snow_tau for COSP only
    real(r8) :: gb_snow_lw         (pcols,pver) ! grid-box mean LW snow optical depth for COSP only

    ! cloud radiative parameters are "in cloud" not "in cell"
    real(r8) :: liq_tau_w  (nbndsw,pcols,pver) ! liquid single scattering albedo * tau
    real(r8) :: liq_tau_w_g(nbndsw,pcols,pver) ! liquid assymetry parameter * tau * w
    real(r8) :: liq_tau_w_f(nbndsw,pcols,pver) ! liquid forward scattered fraction * tau * w
    real(r8) :: liq_lw_abs (nbndlw,pcols,pver) ! liquid absorption optics depth (LW)

    integer itim_old, ifld
    real(r8), pointer, dimension(:,:) :: cld      ! cloud fraction
    real(r8), pointer, dimension(:,:) :: cldfsnow ! cloud fraction of just "snow clouds- whatever they are"
    real(r8), pointer, dimension(:,:) :: qrs      ! shortwave radiative heating rate 
    real(r8), pointer, dimension(:,:) :: qrl      ! longwave  radiative heating rate 

    integer lchnk, ncol, lw
    real(r8) :: calday                        ! current calendar day
    real(r8) :: clat(pcols)                   ! current latitudes(radians)
    real(r8) :: clon(pcols)                   ! current longitudes(radians)
    real(r8) coszrs(pcols)                     ! Cosine solar zenith angle


    ! Local variables from radctl
    integer i, k                  ! index
    integer :: istat
    real(r8) fns(pcols,pverp)     ! net shortwave flux
    real(r8) fcns(pcols,pverp)    ! net clear-sky shortwave flux
    real(r8) fnl(pcols,pverp)     ! net longwave flux
    real(r8) fcnl(pcols,pverp)    ! net clear-sky longwave flux

    ! This is used by the chemistry.
    real(r8), pointer :: fsds(:)  ! Surface solar down flux

    ! This is used for the energy checker and the Eulerian dycore.
    real(r8), pointer :: fsns(:)  ! Surface solar absorbed flux
    real(r8), pointer :: fsnt(:)  ! Net column abs solar flux at model top
    real(r8), pointer :: flns(:)  ! Srf longwave cooling (up-down) flux
    real(r8), pointer :: flnt(:)  ! Net outgoing lw flux at model top

    real(r8) pbr(pcols,pver)      ! Model mid-level pressures (dynes/cm2)
    real(r8) pnm(pcols,pverp)     ! Model interface pressures (dynes/cm2)
    real(r8) eccf                 ! Earth/sun distance factor
    real(r8) lwupcgs(pcols)       ! Upward longwave flux in cgs units

    real(r8) dy                   ! Temporary layer pressure thickness
    real(r8) tint(pcols,pverp)    ! Model interface temperature
    real(r8) :: sfac(1:nswbands)  ! time varying scaling factors due to Solar Spectral Irrad at 1 A.U. per band

    real(r8), pointer, dimension(:,:) :: o3     ! Ozone mass mixing ratio
    real(r8), pointer, dimension(:,:) :: co2    ! co2   mass mixing ratio
    real(r8), dimension(pcols) :: co2_col_mean  ! co2 column mean mmr
    real(r8), pointer, dimension(:,:) :: sp_hum ! specific humidity

    real(r8), pointer, dimension(:,:,:) :: su => NULL()  ! shortwave spectral flux up
    real(r8), pointer, dimension(:,:,:) :: sd => NULL()  ! shortwave spectral flux down
    real(r8), pointer, dimension(:,:,:) :: lu => NULL()  ! longwave  spectral flux up
    real(r8), pointer, dimension(:,:,:) :: ld => NULL()  ! longwave  spectral flux down
    
    ! Aerosol radiative properties
    real(r8) :: aer_tau_w  (pcols,0:pver,nbndsw) ! aerosol single scattering albedo * tau
    real(r8) :: aer_tau_w_g(pcols,0:pver,nbndsw) ! aerosol assymetry parameter * w * tau
    real(r8) :: aer_tau_w_f(pcols,0:pver,nbndsw) ! aerosol forward scattered fraction * w * tau
    real(r8) :: aer_lw_abs (pcols,pver,nbndlw)   ! aerosol absorption optics depth (LW)

    ! Gathered indices of day and night columns 
    !  chunk_column_index = IdxDay(daylight_column_index)
    integer :: Nday                      ! Number of daylight columns
    integer :: Nnite                     ! Number of night columns
    integer, dimension(pcols) :: IdxDay  ! Indices of daylight columns
    integer, dimension(pcols) :: IdxNite ! Indices of night columns
  

    integer :: icall                     ! index through climate/diagnostic radiation calls

    logical :: active_calls(0:N_DIAG)
    logical :: write_output

    type(rrtmg_state_t), pointer :: r_state ! contains the atm concentrations in layers needed for RRTMG

    character(*), parameter :: name = 'radiation_tend'

    ! tropopause diagnostic
    integer :: troplev(pcols)
    real(r8):: p_trop(pcols)

!----------------------------------------------------------------------

    lchnk = state%lchnk
    ncol = state%ncol

    if (present(do_output)) then
       write_output = do_output
    else
       write_output = .true.
    end if

    calday = get_curr_calday()

    itim_old = pbuf_old_tim_idx()

    if (cldfsnow_idx > 0) then
       call pbuf_get_field(pbuf, cldfsnow_idx, cldfsnow, start=(/1,1,itim_old/), kount=(/pcols,pver,1/) )
    endif
    call pbuf_get_field(pbuf, cld_idx,      cld,      start=(/1,1,itim_old/), kount=(/pcols,pver,1/) )

    call pbuf_get_field(pbuf, qrs_idx,      qrs)
    call pbuf_get_field(pbuf, qrl_idx,      qrl)

    if (spectralflux) then
      call pbuf_get_field(pbuf, su_idx, su)
      call pbuf_get_field(pbuf, sd_idx, sd)
      call pbuf_get_field(pbuf, lu_idx, lu)
      call pbuf_get_field(pbuf, ld_idx, ld)
    end if

    call pbuf_get_field(pbuf, fsds_idx, fsds)

    call pbuf_get_field(pbuf, fsns_idx, fsns)
    call pbuf_get_field(pbuf, fsnt_idx, fsnt)
    call pbuf_get_field(pbuf, flns_idx, flns)
    call pbuf_get_field(pbuf, flnt_idx, flnt)

!  For CRM, make cloud equal to input observations:
    if (single_column.and.scm_crm_mode.and.have_cld) then
       do k = 1,pver
          cld(:ncol,k)= cldobs(k)
       enddo
    endif

    if (cldfsnow_idx > 0) then
       call outfld('CLDFSNOW',cldfsnow,pcols,lchnk)
    end if
    !
    ! Cosine solar zenith angle for current time step
    !
    call get_rlat_all_p(lchnk, ncol, clat)
    call get_rlon_all_p(lchnk, ncol, clon)
    call zenith (calday, clat, clon, coszrs, ncol, dt_avg)

    ! Gather night/day column indices.
    Nday = 0
    Nnite = 0
    do i = 1, ncol
       if ( coszrs(i) > 0.0_r8 ) then
          Nday = Nday + 1
          IdxDay(Nday) = i
       else
          Nnite = Nnite + 1
          IdxNite(Nnite) = i
       end if
    end do

    dosw     = radiation_do('sw')      ! do shortwave heating calc this timestep?
    dolw     = radiation_do('lw')      ! do longwave heating calc this timestep?
    
    if (hist_fld_active('FSNR').or.hist_fld_active('FLNR')) then
       call tropopause_find(state, troplev, tropP=p_trop, primary=TROP_ALG_HYBSTOB, backup=TROP_ALG_CLIMATE)
    endif

    if (dosw .or. dolw) then

       ! construct an RRTMG state object
       r_state => rrtmg_state_create( state, cam_in )

       ! For CRM, make cloud liquid water path equal to input observations
       if(single_column.and.scm_crm_mode.and.have_clwp)then
          call endrun('cloud water path must be passed through radiation interface')
          !do k=1,pver
          !   cliqwp(:ncol,k) = clwpobs(k)
          !end do
       endif


       call t_startf('cldoptics')

       if (dosw) then

          if(oldcldoptics) then
             call ec_ice_optics_sw(state, pbuf, rd%ice_tau, ice_tau_w, ice_tau_w_g, ice_tau_w_f, oldicewp=.false.)
             call slingo_liq_optics_sw(state, pbuf, rd%liq_tau, liq_tau_w, liq_tau_w_g, liq_tau_w_f, oldliqwp=.false.)
          else
             select case (icecldoptics)
             case ('ebertcurry')
                call  ec_ice_optics_sw(state, pbuf, rd%ice_tau, ice_tau_w, ice_tau_w_g, ice_tau_w_f, oldicewp=.true.)
             case ('mitchell')
                call get_ice_optics_sw(state, pbuf, rd%ice_tau, ice_tau_w, ice_tau_w_g, ice_tau_w_f)
             case default
                call endrun('iccldoptics must be one either ebertcurry or mitchell')
             end select
             select case (liqcldoptics)
             case ('slingo')
                call slingo_liq_optics_sw(state, pbuf, rd%liq_tau, liq_tau_w, liq_tau_w_g, liq_tau_w_f, oldliqwp=.true.)
             case ('gammadist')
                call get_liquid_optics_sw(state, pbuf, rd%liq_tau, liq_tau_w, liq_tau_w_g, liq_tau_w_f)
             case default
                call endrun('liqcldoptics must be either slingo or gammadist')
             end select
          endif
          rd%cld_tau    (:,1:ncol,:) =  rd%liq_tau    (:,1:ncol,:) + rd%ice_tau    (:,1:ncol,:)
          cld_tau_w  (:,1:ncol,:) =  liq_tau_w  (:,1:ncol,:) + ice_tau_w  (:,1:ncol,:)
          cld_tau_w_g(:,1:ncol,:) =  liq_tau_w_g(:,1:ncol,:) + ice_tau_w_g(:,1:ncol,:)
          cld_tau_w_f(:,1:ncol,:) =  liq_tau_w_f(:,1:ncol,:) + ice_tau_w_f(:,1:ncol,:)
 
          if (cldfsnow_idx > 0) then
            ! add in snow
             call get_snow_optics_sw(state, pbuf, rd%snow_tau, snow_tau_w, snow_tau_w_g, snow_tau_w_f)
             do i=1,ncol
                do k=1,pver
                   rd%cldfprime(i,k)=max(cld(i,k),cldfsnow(i,k))
                   if(rd%cldfprime(i,k) > 0.)then
                      rd%c_cld_tau    (1:nbndsw,i,k)= &
                           (cldfsnow(i,k)*rd%snow_tau    (1:nbndsw,i,k) + cld(i,k)*rd%cld_tau    (1:nbndsw,i,k))/rd%cldfprime(i,k)
                      c_cld_tau_w  (1:nbndsw,i,k)= &
                           (cldfsnow(i,k)*snow_tau_w  (1:nbndsw,i,k) + cld(i,k)*cld_tau_w  (1:nbndsw,i,k))/rd%cldfprime(i,k)
                      c_cld_tau_w_g(1:nbndsw,i,k)= &
                           (cldfsnow(i,k)*snow_tau_w_g(1:nbndsw,i,k) + cld(i,k)*cld_tau_w_g(1:nbndsw,i,k))/rd%cldfprime(i,k)
                      c_cld_tau_w_f(1:nbndsw,i,k)= &
                           (cldfsnow(i,k)*snow_tau_w_f(1:nbndsw,i,k) + cld(i,k)*cld_tau_w_f(1:nbndsw,i,k))/rd%cldfprime(i,k)
                   else
                      rd%c_cld_tau    (1:nbndsw,i,k)= 0._r8
                      c_cld_tau_w  (1:nbndsw,i,k)= 0._r8
                      c_cld_tau_w_g(1:nbndsw,i,k)= 0._r8
                      c_cld_tau_w_f(1:nbndsw,i,k)= 0._r8
                   endif
                enddo
             enddo
          else
             rd%c_cld_tau    (1:nbndsw,1:ncol,:)= rd%cld_tau    (:,1:ncol,:)
             c_cld_tau_w  (1:nbndsw,1:ncol,:)= cld_tau_w  (:,1:ncol,:)
             c_cld_tau_w_g(1:nbndsw,1:ncol,:)= cld_tau_w_g(:,1:ncol,:)
             c_cld_tau_w_f(1:nbndsw,1:ncol,:)= cld_tau_w_f(:,1:ncol,:)
          endif
       endif

       if (dolw) then

          if(oldcldoptics) then
             call cloud_rad_props_get_lw(state, pbuf, rd%cld_lw_abs, oldcloud=.true.)
          else
             select case (icecldoptics)
             case ('ebertcurry')
                call    ec_ice_get_rad_props_lw(state, pbuf, ice_lw_abs, oldicewp=.true.)
             case ('mitchell')
                call ice_cloud_get_rad_props_lw(state, pbuf, ice_lw_abs)
             case default
                call endrun('iccldoptics must be one either ebertcurry or mitchell')
             end select
             select case (liqcldoptics)
             case ('slingo')
                call   slingo_liq_get_rad_props_lw(state, pbuf, liq_lw_abs, oldliqwp=.true.)
             case ('gammadist')
                call liquid_cloud_get_rad_props_lw(state, pbuf, liq_lw_abs)
             case default
                call endrun('liqcldoptics must be either slingo or gammadist')
             end select
             rd%cld_lw_abs(:,1:ncol,:) = liq_lw_abs(:,1:ncol,:) + ice_lw_abs(:,1:ncol,:)
          endif
          !call cloud_rad_props_get_lw(state,  pbuf, cld_lw_abs, oldliq=.true., oldice=.true.)
          !call cloud_rad_props_get_lw(state,  pbuf, cld_lw_abs, oldcloud=.true.)
          !call cloud_rad_props_get_lw(state,  pbuf, cld_lw_abs, oldliq=.true., oldice=.true.)

          if (cldfsnow_idx > 0) then
            ! add in snow
             call snow_cloud_get_rad_props_lw(state, pbuf, rd%snow_lw_abs)
             do i=1,ncol
                do k=1,pver
                   rd%cldfprime(i,k)=max(cld(i,k),cldfsnow(i,k))
                   if(rd%cldfprime(i,k) > 0.)then
                      c_cld_lw_abs(1:nbndlw,i,k)= &
                           (cldfsnow(i,k)*rd%snow_lw_abs(1:nbndlw,i,k) + cld(i,k)*rd%cld_lw_abs(1:nbndlw,i,k))/rd%cldfprime(i,k)
                   else
                      c_cld_lw_abs(1:nbndlw,i,k)= 0._r8
                   endif
                enddo
             enddo
          else
             c_cld_lw_abs(1:nbndlw,1:ncol,:)=rd%cld_lw_abs(:,1:ncol,:)
          endif
       endif

       if (.not.(cldfsnow_idx > 0)) then
          rd%cldfprime(1:ncol,:)=cld(1:ncol,:)
       endif

       call t_stopf('cldoptics')

       ! construct cgs unit reps of pmid and pint and get "eccf" - earthsundistancefactor
       call radinp(ncol, state%pmid, state%pint, pbr, pnm, eccf)

       ! Calculate interface temperatures (following method
       ! used in radtpl for the longwave), using surface upward flux and
       ! stebol constant in mks units
       do i = 1,ncol
          tint(i,1) = state%t(i,1)
          tint(i,pverp) = sqrt(sqrt(cam_in%lwup(i)/stebol))
          do k = 2,pver
             dy = (state%lnpint(i,k) - state%lnpmid(i,k)) / (state%lnpmid(i,k-1) - state%lnpmid(i,k))
             tint(i,k) = state%t(i,k) - dy * (state%t(i,k) - state%t(i,k-1))
          end do
       end do

       ! Solar radiation computation

       if (dosw) then

          call get_variability(sfac)

          ! Get the active climate/diagnostic shortwave calculations
          call rad_cnst_get_call_list(active_calls)

          ! The climate (icall==0) calculation must occur last.
          do icall = N_DIAG, 0, -1

              if (active_calls(icall)) then

                  ! update the concentrations in the RRTMG state object
                  call  rrtmg_state_update( state, pbuf, icall, r_state )

                  call aer_rad_props_sw( icall, state, pbuf, nnite, idxnite, &
                                         rd%aer_tau, aer_tau_w, aer_tau_w_g, aer_tau_w_f)

                  call rad_rrtmg_sw( &
                       lchnk,        ncol,         num_rrtmg_levs, r_state,                    &
                       state%pmid,   rd%cldfprime,                                                &
                       rd%aer_tau,   aer_tau_w,    aer_tau_w_g,  aer_tau_w_f,                  &
                       eccf,         coszrs,       rd%solin(:,icall),        sfac,                         &
                       cam_in%asdir, cam_in%asdif, cam_in%aldir, cam_in%aldif,                 &
                       rd%qrs(:,:,icall),          rd%qrsc(:,:,icall),         rd%fsnt(:,icall),         rd%fsntc(:,icall),&
                       rd%fsntoa(:,icall), rd%fsutoa(:,icall), &
                       rd%fsntoac(:,icall),      rd%fsnirt(:,icall),       rd%fsnrtc(:,icall),       rd%fsnirtsq(:,icall),     &
                       rd%fsns(:,icall),           &
                       rd%fsnsc(:,icall),        rd%fsdsc(:,icall),   rd%fsds(:,icall),   rd%sols(:,icall), rd%soll(:,icall),   &
                       rd%solsd(:,icall),rd%solld(:,icall),fns,          fcns,                         &
                       Nday,         Nnite,        IdxDay,       IdxNite,                      &
                       su,           sd,                                                       &
                       E_cld_tau=rd%c_cld_tau, E_cld_tau_w=c_cld_tau_w, E_cld_tau_w_g=c_cld_tau_w_g, E_cld_tau_w_f=c_cld_tau_w_f, &
                       old_convert = .false.)

                  if (spectralflux) then
                     rd%su(:,:,:,icall) = su(:,:,:)
                     rd%sd(:,:,:,icall) = sd(:,:,:)
                  end if

                  !  Output net fluxes at 200 mb
                  call vertinterp(ncol, pcols, pverp, state%pint, 20000._r8, fcns, rd%fsn200c(:,icall))
                  call vertinterp(ncol, pcols, pverp, state%pint, 20000._r8, fns, rd%fsn200(:,icall))
                  if (hist_fld_active('FSNR')) then
                     do i = 1,ncol
                        call vertinterp(1, 1, pverp, state%pint(i,:), p_trop(i), fns(i,:), rd%fsnr(i,icall))
                     enddo
                  endif

                  do i=1,ncol
                     rd%swcf(i,icall)=rd%fsntoa(i,icall) - rd%fsntoac(i,icall)
                  end do

              end if ! (active_calls(icall))
          end do ! icall

          qrs(:,:) = rd%qrs(:,:,0)

          ! Assign the climate values (index=0)
          fsds = rd%fsds(:,0)
          fsns = rd%fsns(:,0)
          fsnt = rd%fsnt(:,0)
          cam_out%sols  = rd%sols(:,0)
          cam_out%soll  = rd%soll(:,0)
          cam_out%solsd = rd%solsd(:,0)
          cam_out%solld = rd%solld(:,0)
 

          ! Output cloud optical depth fields for the visible band
          rd%tot_icld_vistau(:ncol,:)  = rd%c_cld_tau(idx_sw_diag,:ncol,:)
          rd%liq_icld_vistau(:ncol,:)  = rd%liq_tau(idx_sw_diag,:ncol,:)
          rd%ice_icld_vistau(:ncol,:)  = rd%ice_tau(idx_sw_diag,:ncol,:)
          if (cldfsnow_idx > 0) then
             rd%snow_icld_vistau(:ncol,:) = rd%snow_tau(idx_sw_diag,:ncol,:)
          endif

          ! multiply by total cloud fraction to get gridbox value
          rd%tot_cld_vistau(:ncol,:) = rd%c_cld_tau(idx_sw_diag,:ncol,:)*rd%cldfprime(:ncol,:)

          ! add fillvalue for night columns
          do i = 1, Nnite
             rd%tot_cld_vistau(IdxNite(i),:)   = fillvalue
             rd%tot_icld_vistau(IdxNite(i),:)  = fillvalue
             rd%liq_icld_vistau(IdxNite(i),:)  = fillvalue
             rd%ice_icld_vistau(IdxNite(i),:)  = fillvalue
             if (cldfsnow_idx > 0) then
                rd%snow_icld_vistau(IdxNite(i),:) = fillvalue
             endif
          end do

          if (write_output) call radiation_output_sw(state, pbuf, rd)

       end if   ! dosw

       ! Output aerosol mmr
       call rad_cnst_out(0, state, pbuf)
                 
       ! Longwave radiation computation

       if (dolw) then
          !
          ! Convert upward longwave flux units to CGS
          !
          do i=1,ncol
             lwupcgs(i) = cam_in%lwup(i)*1000._r8
             if(single_column.and.scm_crm_mode.and.have_tg) &
                  lwupcgs(i) = 1000*stebol*tground(1)**4
          end do

          call rad_cnst_get_call_list(active_calls)

          ! The climate (icall==0) calculation must occur last.
          do icall = N_DIAG, 0, -1

              if (active_calls(icall)) then

                  ! update the conctrations in the RRTMG state object
                  call  rrtmg_state_update( state, pbuf, icall, r_state)

                  call aer_rad_props_lw(icall, state, pbuf,  aer_lw_abs)
                  
                  call rad_rrtmg_lw( &
                       lchnk,        ncol,         num_rrtmg_levs,  r_state,                     &
                       state%pmid,   aer_lw_abs,   rd%cldfprime,       c_cld_lw_abs,                &
                       rd%qrl(:,:,icall),          rd%qrlc(:,:,icall),                                                       &
                       rd%flns(:,icall),  rd%flnt(:,icall), rd%flnsc(:,icall),   rd%flntc(:,icall),        rd%flwds(:,icall), &
                       rd%flut(:,icall),         rd%flutc(:,icall),        fnl,             fcnl,         rd%fldsc(:,icall), &
                       lu,                ld)

                  if (spectralflux) then
                     rd%lu(:,:,:,icall) = lu(:,:,:)
                     rd%ld(:,:,:,icall) = ld(:,:,:)
                  end if


                  do i=1,ncol
                     rd%lwcf(i,icall)=rd%flutc(i,icall) - rd%flut(i,icall)
                  end do

                  !  Output fluxes at 200 mb
                  call vertinterp(ncol, pcols, pverp, state%pint, 20000._r8, fnl, rd%fln200(:,icall))
                  call vertinterp(ncol, pcols, pverp, state%pint, 20000._r8, fcnl, rd%fln200c(:,icall))
                  if (hist_fld_active('FLNR')) then
                     do i = 1,ncol
                        call vertinterp(1, 1, pverp, state%pint(i,:), p_trop(i), fnl(i,:), rd%flnr(i,icall))
                     enddo
                  endif

              end if
          end do

          qrl(:,:) = rd%qrl(:,:,0)

          ! Assign the climate values (index=0)
          cam_out%flwds(:) = rd%flwds(:,0)
          flns(:)          = rd%flns(:,0)
          flnt(:)          = rd%flnt(:,0)
 
          if (write_output) call radiation_output_lw(state, pbuf, rd)

       end if  !dolw

       ! deconstruct the RRTMG state object
       call rrtmg_state_destroy(r_state)

       ! mji/hirsrtm - Add call to HIRSRTM package
       ! HIRS brightness temperature calculation in 7 infra-red channels and 4 microwave
       ! channels as a diagnostic to compare to TOV/MSU satellite data.
       ! Done if dohirs set to .true. at time step frequency ihirsfq

       nstep = get_nstep()

       if ( dohirs .and. (mod(nstep-1,ihirsfq) .eq. 0) ) then

          do i= 1, ncol
             ts(i) = sqrt(sqrt(cam_in%lwup(i)/stebol))
             ! Set oro (land/sea flag) for compatibility with landfrac/icefrac/ocnfrac
             ! oro=0 (sea or ice); oro=1 (land)
             if (cam_in%landfrac(i).ge.0.001) then
                oro(i)=1.
             else
                oro(i)=0.
             endif
             ! Convert pressure from Pa to hPa
             do k = 1, pver
                pintmb(i,k) = state%pint(i,k)*1.e-2_r8
             end do
             pintmb(i,pverp) = state%pint(i,pverp)*1.e-2_r8
          end do

          ! Get specific humidity
          call rad_cnst_get_gas(0,'H2O', state, pbuf, sp_hum)
          ! Get ozone mass mixing ratio.
          call rad_cnst_get_gas(0,'O3',  state, pbuf, o3)
          ! Get CO2 mass mixing ratio
          call rad_cnst_get_gas(0,'CO2', state, pbuf, co2)

          call calc_col_mean(state, co2, co2_col_mean)
          call hirsrtm( lchnk  ,ncol , &
                        pintmb ,state%t  ,sp_hum ,co2_col_mean, &
                        o3     ,ts       ,oro    ,tb_ir  ,britemp )

          do i = 1, pnb_hirs
             call outfld(hirsname(i),tb_ir(1,i),pcols,lchnk)
          end do
          do i = 1, pnf_msu
             call outfld(msuname(i),britemp(1,i),pcols,lchnk)
          end do

       end if

       if (docosp) then

          !! initialize and calculate emis
          emis(:,:) = 0._r8
          emis(:ncol,:) = 1._r8 - exp(-rd%cld_lw_abs(rrtmg_lw_cloudsim_band,:ncol,:))
          call outfld('EMIS', emis, pcols, lchnk)

          !! compute grid-box mean SW and LW snow optical depth for use by COSP
          gb_snow_tau(:,:) = 0._r8
          gb_snow_lw(:,:) = 0._r8
          if (cldfsnow_idx > 0) then
             do i=1,ncol
                do k=1,pver
                   if(cldfsnow(i,k) > 0.)then
                      gb_snow_tau(i,k) = rd%snow_tau(rrtmg_sw_cloudsim_band,i,k)*cldfsnow(i,k)
                      gb_snow_lw(i,k) = rd%snow_lw_abs(rrtmg_lw_cloudsim_band,i,k)*cldfsnow(i,k)
                   end if
                enddo
             enddo
          end if

          !! cosp_cnt referenced for each chunk... cosp_cnt(lchnk)
          !! advance counter for this timestep
          cosp_cnt(lchnk) = cosp_cnt(lchnk) + 1

          !! if counter is the same as cosp_nradsteps, run cosp and reset counter
          if (cosp_nradsteps .eq. cosp_cnt(lchnk)) then
             !call should be compatible with camrt radiation.F90 interface too, should be with (in),optional
             ! N.B.: For snow optical properties, the GRID-BOX MEAN shortwave and longwave optical depths are passed.
             call cospsimulator_intr_run(state,  pbuf, cam_in, emis, coszrs, &
                cld_swtau_in=rd%cld_tau(rrtmg_sw_cloudsim_band,:,:),&
                snow_tau_in=gb_snow_tau,snow_emis_in=gb_snow_lw)
              cosp_cnt(lchnk) = 0  !! reset counter
          end if
       end if


    else   !  if (dosw .or. dolw) then

       ! convert radiative heating rates from Q*dp to Q for energy conservation
       do k =1 , pver
          do i = 1, ncol
             qrs(i,k) = qrs(i,k)/state%pdel(i,k)
             qrl(i,k) = qrl(i,k)/state%pdel(i,k)
          end do
       end do

    end if   ! if (dosw .or. dolw) then

    ! output rad inputs and resulting heating rates
    call rad_data_write(  pbuf, state, cam_in, coszrs )

    ! Compute net radiative heating tendency
    call radheat_tend(state, pbuf,  ptend, qrl, qrs, fsns, &
                      fsnt, flns, flnt, cam_in%asdir, net_flx)

    if (write_output) then
       ! Compute heating rate for dtheta/dt
       do k=1,pver
          do i=1,ncol
             ftem(i,k) = (qrs(i,k) + qrl(i,k))/cpair * (1.e5_r8/state%pmid(i,k))**cappa
          end do
       end do
       call outfld('HR      ',ftem    ,pcols   ,lchnk   )
    end if

    ! convert radiative heating rates to Q*dp for energy conservation
    do k =1 , pver
       do i = 1, ncol
          qrs(i,k) = qrs(i,k)*state%pdel(i,k)
          qrl(i,k) = qrl(i,k)*state%pdel(i,k)
       end do
    end do

    cam_out%netsw(:ncol) = fsns(:ncol)

 end subroutine radiation_tend

!===============================================================================

subroutine radiation_output_sw(state, pbuf, rd)
    use radiation_utils,  only: rad_diagdata_type
    use physics_buffer, only : physics_buffer_desc
    use physics_types,   only: physics_state
    use physconst,       only: cpair
    use cam_history,     only: outfld
    use rad_constituents, only: rad_cnst_out, rad_cnst_get_gas

    type(physics_state), intent(in), target :: state
    type(physics_buffer_desc), pointer      :: pbuf(:)
    type(rad_diagdata_type), intent(inout) :: rd  ! structure to hold diagnostic data for output

    real(r8) :: ftem(pcols,pver)               ! Temporary workspace for outfld variables

    logical :: active_calls(0:N_DIAG)

    integer :: lchnk, ncol
    integer :: icall

    ncol = state%ncol
    lchnk = state%lchnk

    call rad_cnst_get_call_list(active_calls)

    ! Solar radiation computation

    do icall = N_DIAG, 0, -1
       if (active_calls(icall)) then

           ! Dump shortwave radiation information to history tape buffer (diagnostics)

           ftem(:ncol,:pver) = rd%qrs(:ncol,:pver,icall)/cpair
           call outfld('QRS'//diag(icall),ftem  ,pcols,lchnk)
           ftem(:ncol,:pver) = rd%qrsc(:ncol,:pver,icall)/cpair
           call outfld('QRSC'//diag(icall),ftem  ,pcols,lchnk)
           call outfld('SOLIN'//diag(icall),rd%solin(:,icall) ,pcols,lchnk)
           call outfld('FSDS'//diag(icall),rd%fsds(:,icall)  ,pcols,lchnk)
           call outfld('FSNIRTOA'//diag(icall),rd%fsnirt(:,icall),pcols,lchnk)
           call outfld('FSNRTOAC'//diag(icall),rd%fsnrtc(:,icall),pcols,lchnk)
           call outfld('FSNRTOAS'//diag(icall),rd%fsnirtsq(:,icall),pcols,lchnk)
           call outfld('FSNT'//diag(icall),rd%fsnt(:,icall)  ,pcols,lchnk)
           call outfld('FSNS'//diag(icall),rd%fsns(:,icall)  ,pcols,lchnk)
           call outfld('FSNTC'//diag(icall),rd%fsntc(:,icall) ,pcols,lchnk)
           call outfld('FSNSC'//diag(icall),rd%fsnsc(:,icall) ,pcols,lchnk)
           call outfld('FSDSC'//diag(icall),rd%fsdsc(:,icall) ,pcols,lchnk)
           call outfld('FSNTOA'//diag(icall),rd%fsntoa(:,icall),pcols,lchnk)
           call outfld('FSUTOA'//diag(icall),rd%fsutoa(:,icall),pcols,lchnk)
           call outfld('FSNTOAC'//diag(icall),rd%fsntoac(:,icall),pcols,lchnk)
           call outfld('SOLS'//diag(icall),rd%sols(:,icall)  ,pcols,lchnk)
           call outfld('SOLL'//diag(icall),rd%soll(:,icall)  ,pcols,lchnk)
           call outfld('SOLSD'//diag(icall),rd%solsd(:,icall) ,pcols,lchnk)
           call outfld('SOLLD'//diag(icall),rd%solld(:,icall) ,pcols,lchnk)
           call outfld('FSN200'//diag(icall),rd%fsn200(:,icall),pcols,lchnk)
           call outfld('FSN200C'//diag(icall),rd%fsn200c(:,icall),pcols,lchnk)
           call outfld('SWCF'//diag(icall),rd%swcf(:,icall)  ,pcols,lchnk)
           call outfld('FSNR',rd%fsnr(:,icall),pcols,lchnk)
       end if
    end do

    call outfld('TOT_CLD_VISTAU', rd%tot_cld_vistau, pcols, lchnk)
    call outfld('TOT_ICLD_VISTAU', rd%tot_icld_vistau, pcols, lchnk)
    call outfld('LIQ_ICLD_VISTAU', rd%liq_icld_vistau, pcols, lchnk)
    call outfld('ICE_ICLD_VISTAU', rd%ice_icld_vistau, pcols, lchnk)
    if (cldfsnow_idx > 0) then
       call outfld('SNOW_ICLD_VISTAU', rd%snow_icld_vistau, pcols, lchnk)
    endif

end subroutine radiation_output_sw

subroutine radiation_output_lw(state, pbuf, rd)
    use radiation_utils,  only: rad_diagdata_type
    use physics_buffer, only : physics_buffer_desc
    use physics_types,   only: physics_state

    use physconst,       only: cpair
    use cam_history,     only: outfld
    use rad_constituents, only: rad_cnst_out, rad_cnst_get_gas

    type(physics_state), intent(in), target :: state
    type(physics_buffer_desc), pointer      :: pbuf(:)
    type(rad_diagdata_type), intent(inout) :: rd  ! structure to hold diagnostic data for output

    logical :: active_calls(0:N_DIAG)

    integer :: lchnk, ncol
    integer :: icall

    ncol = state%ncol
    lchnk = state%lchnk

    call rad_cnst_get_call_list(active_calls)

    do icall = N_DIAG, 0, -1
        if (active_calls(icall)) then

           ! Dump longwave radiation information to history tape buffer (diagnostics)
           call outfld('QRL'//diag(icall),rd%qrl(:ncol,:,icall)/cpair,ncol,lchnk)
           call outfld('QRLC'//diag(icall),rd%qrlc(:ncol,:,icall)/cpair,ncol,lchnk)
           call outfld('FLNT'//diag(icall),rd%flnt(:,icall)  ,pcols,lchnk)
           call outfld('FLUT'//diag(icall),rd%flut(:,icall)  ,pcols,lchnk)
           call outfld('FLUTC'//diag(icall),rd%flutc(:,icall) ,pcols,lchnk)
           call outfld('FLNTC'//diag(icall),rd%flntc(:,icall) ,pcols,lchnk)
           call outfld('FLNS'//diag(icall),rd%flns(:,icall)  ,pcols,lchnk)

           call outfld('FLDSC'//diag(icall),rd%fldsc(:,icall) ,pcols,lchnk)
           call outfld('FLNSC'//diag(icall),rd%flnsc(:,icall) ,pcols,lchnk)
           call outfld('LWCF'//diag(icall),rd%lwcf(:,icall)  ,pcols,lchnk)
           call outfld('FLN200'//diag(icall),rd%fln200(:,icall),pcols,lchnk)
           call outfld('FLN200C'//diag(icall),rd%fln200c(:,icall),pcols,lchnk)
           call outfld('FLDS'//diag(icall),rd%flwds(:,icall) ,pcols,lchnk)
           call outfld('FLNR',rd%flnr(:,icall),pcols,lchnk)
        end if
    end do

end subroutine radiation_output_lw


!===============================================================================

subroutine radinp(ncol, pmid, pint, pmidrd, pintrd, eccf)
!----------------------------------------------------------------------- 
! 
! Purpose: 
! Set latitude and time dependent arrays for input to solar
! and longwave radiation.
! Convert model pressures to cgs.
! 
! Author: CCM1, CMS Contact J. Kiehl
!-----------------------------------------------------------------------
   use shr_orb_mod
   use time_manager, only: get_curr_calday

!------------------------------Arguments--------------------------------
!
! Input arguments
!
   integer, intent(in) :: ncol                 ! number of atmospheric columns

   real(r8), intent(in) :: pmid(pcols,pver)    ! Pressure at model mid-levels (pascals)
   real(r8), intent(in) :: pint(pcols,pverp)   ! Pressure at model interfaces (pascals)
!
! Output arguments
!
   real(r8), intent(out) :: pmidrd(pcols,pver)  ! Pressure at mid-levels (dynes/cm*2)
   real(r8), intent(out) :: pintrd(pcols,pverp) ! Pressure at interfaces (dynes/cm*2)
   real(r8), intent(out) :: eccf                ! Earth-sun distance factor

!
!---------------------------Local variables-----------------------------
!
   integer i                ! Longitude loop index
   integer k                ! Vertical loop index

   real(r8) :: calday       ! current calendar day
   real(r8) :: delta        ! Solar declination angle
!-----------------------------------------------------------------------
!
   calday = get_curr_calday()
   call shr_orb_decl (calday  ,eccen     ,mvelpp  ,lambm0  ,obliqr  , &
                      delta   ,eccf)

!
! Convert pressure from pascals to dynes/cm2
!
   do k=1,pver
      do i=1,ncol
         pmidrd(i,k) = pmid(i,k)*10.0_r8
         pintrd(i,k) = pint(i,k)*10.0_r8
      end do
   end do
   do i=1,ncol
      pintrd(i,pverp) = pint(i,pverp)*10.0_r8
   end do

end subroutine radinp

!===============================================================================

subroutine calc_col_mean(state, mmr_pointer, mean_value)
!----------------------------------------------------------------------- 
! 
! Compute the column mean mass mixing ratio.  
!
!-----------------------------------------------------------------------

   use cam_logfile,  only: iulog

   type(physics_state),        intent(in)  :: state
   real(r8), dimension(:,:),   pointer     :: mmr_pointer  ! mass mixing ratio (lev)
   real(r8), dimension(pcols), intent(out) :: mean_value   ! column mean mmr

   integer  :: i, k, ncol
   real(r8) :: ptot(pcols)
   !-----------------------------------------------------------------------

   ncol         = state%ncol
   mean_value   = 0.0_r8
   ptot         = 0.0_r8

   do k=1,pver
      do i=1,ncol
         mean_value(i) = mean_value(i) + mmr_pointer(i,k)*state%pdeldry(i,k)
         ptot(i)         = ptot(i) + state%pdeldry(i,k)
      end do
   end do
   do i=1,ncol
      mean_value(i) = mean_value(i) / ptot(i)
   end do

end subroutine calc_col_mean

!===============================================================================

end module radiation

