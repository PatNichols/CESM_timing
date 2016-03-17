module scamMod

use shr_kind_mod,   only: r8 => shr_kind_r8
use pmgrid,         only: plon, plat, plev, plevp
use constituents,   only: pcnst
use shr_scam_mod,   only: shr_scam_getCloseLatLon
use dycore,         only: dycore_is
use cam_logfile,    only: iulog
use cam_abortutils, only: endrun

implicit none
private

! PUBLIC INTERFACES:

public scam_readnl   ! read SCAM namelist options 

! PUBLIC MODULE DATA:

real(r8), public ::  pressure_levels(plev)
real(r8), public ::  scmlat   ! input namelist latitude for scam
real(r8), public ::  scmlon   ! input namelist longitude for scam


integer, parameter :: num_switches = 20
integer, parameter :: max_path_len = 128

logical, public ::  single_column         ! Using IOP file or not
logical, public ::  use_iop               ! Using IOP file or not
logical, public ::  use_analysis
logical, public ::  use_saveinit
logical, public ::  use_pert_init         ! perturb initial values
logical, public ::  use_pert_frc          ! perturb forcing 
logical, public ::  scm_diurnal_avg       ! If using diurnal averaging or not
logical, public ::  scm_crm_mode          ! column radiation mode
logical, public ::  use_userdata
logical, public ::  isrestart             ! If this is a restart step or not
logical, public ::  switch(num_switches)  ! Logical flag settings from GUI
logical, public ::  l_uvphys              ! If true, update u/v after TPHYS
logical, public ::  l_uvadvect            ! If true, T, U & V will be passed to SLT
logical, public ::  l_conv                ! use flux divergence terms for T and q?     
logical, public ::  l_divtr               ! use flux divergence terms for constituents?
logical, public ::  l_diag                ! do we want available diagnostics?

integer, public ::  error_code            ! Error code from netCDF reads
integer, public ::  initTimeIdx
integer, public ::  seedval

character*(max_path_len), public ::  modelfile
character*(max_path_len), public ::  analysisfile
character*(max_path_len), public ::  sicfile
character*(max_path_len), public ::  userfile
character*(max_path_len), public ::  sstfile
character*(max_path_len), public ::  lsmpftfile
character*(max_path_len), public ::  pressfile
character*(max_path_len), public ::  topofile
character*(max_path_len), public ::  ozonefile
character*(max_path_len), public ::  iopfile
character*(max_path_len), public ::  absemsfile
character*(max_path_len), public ::  aermassfile
character*(max_path_len), public ::  aeropticsfile
character*(max_path_len), public ::  timeinvfile
character*(max_path_len), public ::  lsmsurffile
character*(max_path_len), public ::  lsminifile

real(r8), public ::  fixmascam
real(r8), public ::  betacam
real(r8), public ::  alphacam(pcnst)
real(r8), public ::  dqfxcam(plon,plev,pcnst)

real(r8), public ::      divq3d(plev,pcnst)  ! 3D q advection
real(r8), public ::      divt3d(plev)        ! 3D T advection
real(r8), public ::      vertdivq(plev,pcnst)! vertical q advection
real(r8), public ::      vertdivt(plev)      ! vertical T advection
real(r8), public ::      ptend               ! surface pressure tendency
real(r8), public ::      qdiff(plev)         ! model minus observed humidity
real(r8), public ::      qobs(plev)          ! actual W.V. Mixing ratio
real(r8), public ::      cldliqobs(plev)     ! actual W.V. Mixing ratio
real(r8), public ::      cldiceobs(plev)     ! actual W.V. Mixing ratio
real(r8), public ::      numliqobs(plev)     ! actual 
real(r8), public ::      numiceobs(plev)     ! actual 
real(r8), public ::      precobs(1)          ! observed precipitation 
real(r8), public ::      lhflxobs(1)         ! observed surface latent heat flux 
real(r8), public ::      shflxobs(1)         ! observed surface sensible heat flux
real(r8), public ::      q1obs(plev)         ! observed apparent heat source
real(r8), public ::      q2obs(plev)         ! observed apparent heat sink
real(r8), public ::      tdiff(plev)         ! model minus observed temp 
real(r8), public ::      tground(1)          ! ground temperature
real(r8), public ::      tobs(plev)          ! actual temperature
real(r8), public ::      tsair(1)            ! air temperature at the surface
real(r8), public ::      udiff(plev)         ! model minus observed uwind
real(r8), public ::      uobs(plev)          ! actual u wind
real(r8), public ::      vdiff(plev)         ! model minus observed vwind
real(r8), public ::      vobs(plev)          ! actual v wind
real(r8), public ::      cldobs(plev)        ! observed cld
real(r8), public ::      clwpobs(plev)       ! observed clwp
real(r8), public ::      aldirobs(1)         ! observed aldir
real(r8), public ::      aldifobs(1)         ! observed aldif
real(r8), public ::      asdirobs(1)         ! observed asdir
real(r8), public ::      asdifobs(1)         ! observed asdif

real(r8), public ::      wfld(plev)          ! Vertical motion (slt)
real(r8), public ::      wfldh(plevp)        ! Vertical motion (slt)
real(r8), public ::      divq(plev,pcnst)    ! Divergence of moisture
real(r8), public ::      divt(plev)          ! Divergence of temperature
real(r8), public ::      divu(plev)          ! Horiz Divergence of E/W
real(r8), public ::      divv(plev)          ! Horiz Divergence of N/S
                                             ! mo_drydep algorithm
real(r8), public, pointer :: loniop(:)
real(r8), public, pointer :: latiop(:)

integer, public ::     iopTimeIdx            ! index into iop dataset
integer, public ::     steplength            ! Length of time-step
integer, public ::     base_date             ! Date in (yyyymmdd) of start time
integer, public ::     base_secs             ! Time of day of start time (sec)

logical, public ::  doiopupdate   ! do we need to read next iop timepoint
logical, public ::  have_divq     ! dataset contains divq 
logical, public ::  have_divt     ! dataset contains divt
logical, public ::  have_divq3d   ! dataset contains divq3d 
logical, public ::  have_vertdivt ! dataset contains vertdivt
logical, public ::  have_vertdivq ! dataset contains vertdivq 
logical, public ::  have_divt3d   ! dataset contains divt3d
logical, public ::  have_divu     ! dataset contains divu
logical, public ::  have_divv     ! dataset contains divv 
logical, public ::  have_omega    ! dataset contains omega
logical, public ::  have_phis     ! dataset contains phis
logical, public ::  have_ptend    ! dataset contains ptend
logical, public ::  have_ps       ! dataset contains ps
logical, public ::  have_q        ! dataset contains q
logical, public ::  have_q1       ! dataset contains Q1
logical, public ::  have_q2       ! dataset contains Q2
logical, public ::  have_prec     ! dataset contains prec 
logical, public ::  have_lhflx    ! dataset contains lhflx 
logical, public ::  have_shflx    ! dataset contains shflx
logical, public ::  have_t        ! dataset contains t
logical, public ::  have_tg       ! dataset contains tg
logical, public ::  have_tsair    ! dataset contains tsair
logical, public ::  have_u        ! dataset contains u 
logical, public ::  have_v        ! dataset contains v 
logical, public ::  have_cld      ! dataset contains cld
logical, public ::  have_cldliq   ! dataset contains cldliq
logical, public ::  have_cldice   ! dataset contains cldice
logical, public ::  have_numliq   ! dataset contains numliq
logical, public ::  have_numice   ! dataset contains numice
logical, public ::  have_clwp     ! dataset contains clwp
logical, public ::  have_aldir    ! dataset contains aldir
logical, public ::  have_aldif    ! dataset contains aldif
logical, public ::  have_asdir    ! dataset contains asdir
logical, public ::  have_asdif    ! dataset contains asdif
logical, public ::  scm_iop_srf_prop   ! use the specified surface properties
logical, public ::  scm_relaxation! use relaxation
logical, public ::  use_camiop    ! use cam generated forcing 
logical, public ::  use_3dfrc     ! use 3d forcing

character(len=200), public ::  scm_clubb_iop_name   ! IOP name for CLUBB

!=======================================================================
contains
!=======================================================================

subroutine scam_readnl(nlfile, single_column_in, scmlat_in, scmlon_in)

   use namelist_utils,  only: find_group_name
   use units,           only: getunit, freeunit
   use spmd_utils,      only: mpicom, mstrid=>masterprocid, mpi_integer
   use dycore,          only: dycore_is
   use wrap_nf,         only: wrap_open, wrap_inq_dimid, wrap_inq_dimlen, wrap_inq_varid
   use netcdf

   character(len=*), intent(in) :: nlfile  ! filepath for file containing namelist input
   logical,          intent(in) :: single_column_in
   real(r8),         intent(in) :: scmlat_in
   real(r8),         intent(in) :: scmlon_in

   ! Local variables
   integer :: unitn, ierr
   character(len=*), parameter :: sub = 'scam_readnl'

   namelist /scam_nl/ iopfile, scm_crm_mode, scm_diurnal_avg, scm_iop_srf_prop, &
                      scm_clubb_iop_name, scm_relaxation

   integer  :: ncid
   integer  :: latdimid, londimid
   integer  :: latsiz, lonsiz
   integer  :: latid, lonid
   integer  :: iatt
   integer  :: ret
   integer  :: latidx, lonidx
   real(r8) :: ioplat,ioplon
   !-----------------------------------------------------------------------------

   single_column = single_column_in

   iopfile            = ' '
   scm_crm_mode       = .false.
   scm_diurnal_avg    = .false.
   scm_iop_srf_prop   = .false.
   scm_clubb_iop_name = ' '
   scm_relaxation     = .false.

   if (single_column) then

      if (.not. dycore_is('EUL') .or. plon /= 1 .or. plat /=1 ) then 
         call endrun(sub//': must compile model for SCAM mode when namelist '//&
            'parameter single_column is .true.')
      endif

      scmlat = scmlat_in
      scmlon = scmlon_in

      if (scmlat < -90._r8 .or. scmlat > 90._r8) then
         call endrun(sub//': ERROR: SCMLAT must be between -90. and 90. degrees.')
      else if (scmlon < 0._r8 .or. scmlon > 360._r8) then
         call endrun(sub//': ERROR: SCMLON must be between 0. and 360. degrees.')
      end if

      ! Read namelist
      unitn = getunit()
      open( unitn, file=trim(nlfile), status='old' )
      call find_group_name(unitn, 'scam_nl', status=ierr)
      if (ierr == 0) then
         read(unitn, scam_nl, iostat=ierr)
         if (ierr /= 0) then
            call endrun(sub//': FATAL: reading namelist')
         end if
      end if
      close(unitn)
      call freeunit(unitn)

      if (iopfile .ne. "") then 
         use_iop = .true.
      else
         call endrun(sub//': ERROR: must specify IOP file for single column mode')
      endif

      call wrap_open(iopfile, NF90_NOWRITE, ncid)
      call wrap_inq_dimid(ncid, 'lon', londimid)
      call wrap_inq_dimid(ncid, 'lat', latdimid)
      call wrap_inq_dimlen(ncid, londimid, lonsiz)
      call wrap_inq_dimlen(ncid, latdimid, latsiz)
      call wrap_inq_varid(ncid, 'lon', lonid)
      call wrap_inq_varid(ncid, 'lat', latid)

      if (nf90_inquire_attribute( ncid, NF90_GLOBAL, 'CAM_GENERATED_FORCING', attnum=iatt ) &
                                 == NF90_NOERR) then
         use_camiop = .true.
      else
         use_camiop = .false.
      endif

      if (latsiz==1 .and. lonsiz==1) then

         ret = nf90_get_var(ncid, lonid, ioplon)
         if (ret /= NF90_NOERR) then
            call endrun(sub//': ERROR: reading longitude variable from iopfile')
         end if
         if (ioplon < 0._r8) ioplon = ioplon + 360._r8

         ret = nf90_get_var(ncid, latid, ioplat)
         if (ret /= NF90_NOERR) then
            call endrun(sub//': ERROR: reading latitude variable from iopfile')
         end if

         call shr_scam_GetCloseLatLon(ncid, scmlat, scmlon, ioplat, ioplon, latidx, lonidx)
      else
         if (use_camiop) then
            call shr_scam_GetCloseLatLon(ncid, scmlat, scmlon, ioplat, ioplon, latidx, lonidx)
         end if
      endif

      scmlat = ioplat
      scmlon = ioplon

      write(iulog,*) 'SCAM options:'
      write(iulog,*) '  scmlat, scmlon         =', scmlat_in, scmlon_in
      write(iulog,*) '  Using closest lat, lon =', scmlat, scmlon
      if (use_camiop) then
         write(iulog,*) '  Using CAM Generated IOP file:', trim(iopfile)
      else
         write(iulog,*) '  Using IOP file              :', trim(iopfile)
      end if
      write(iulog,*) '  scm_crm_mode           =', scm_crm_mode
      write(iulog,*) '  scm_diurnal_avg        =', scm_diurnal_avg
      write(iulog,*) '  scm_iop_srf_prop       =', scm_iop_srf_prop
      write(iulog,*) '  scm_clubb_iop_name     =', trim(scm_clubb_iop_name)
      write(iulog,*) '  scm_relaxation         =', scm_relaxation

   else if (dycore_is('EUL') .and. plon ==1 .and. plat ==1) then
      call endrun(sub//': ERROR: single_column namelist option must be set to true'//&
         ' when running in single column mode')
   endif

end subroutine scam_readnl

!===============================================================================

end module scamMod
