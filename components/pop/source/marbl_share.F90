module marbl_share_mod

! !MODULE: marbl_share

!-----------------------------------------------------------------------------
!   This module contains definitions of variables, derived types, and
!   functions/subroutines that are used in ecocys_mod.F90 as well as by
!   other modules that make use of the ecosys_mod.
!
!   The variables are shared using threading with pointers, and need to be
!   pointed to in the code.
!   Note: So far the values of all of these fields are set in ecosys_mod
!   and are NOT modified in the other modules
!   A. Jahn, NCAR
!-----------------------------------------------------------------------------

  use blocks, only: nx_block
  use blocks, only: ny_block

  use domain_size, only : km
  use domain_size, only: max_blocks_clinic

  use marbl_kinds_mod, only : r8
  use marbl_kinds_mod, only : log_kind
  use marbl_kinds_mod, only : int_kind
  use marbl_kinds_mod, only : char_len
  
  implicit none

  public
  save

!-----------------------------------------------------------------------------
! number of ecosystem constituents and grazing interactions
!-----------------------------------------------------------------------------
  integer (KIND=int_kind), parameter :: &
       zooplankton_cnt = ZOOPLANKTON_CNT, &
       autotroph_cnt   = AUTOTROPH_CNT,   &
       grazer_prey_cnt = GRAZER_PREY_CNT

!-----------------------------------------------------------------------------
!   derived type for grazers
!-----------------------------------------------------------------------------

 type, public :: zooplankton_type
     character(char_len) :: sname, lname
     integer (KIND=int_kind) :: &
          C_ind                    ! tracer indices for zooplankton carbon
     real(KIND=r8) :: &
          z_mort_0,         & ! zoo linear mort rate (1/sec)
          z_mort2_0,        & ! zoo quad mort rate (1/sec/((mmol C/m3))
          loss_thres      !zoo conc. where losses go to zero
  end type zooplankton_type

  type(zooplankton_type), dimension(zooplankton_cnt) :: zooplankton

!-----------------------------------------------------------------------------
!   derived type for functional group
!-----------------------------------------------------------------------------

  type, public :: autotroph_type
     character(char_len) :: sname, lname
     logical(KIND=log_kind) :: &
        Nfixer,                             & ! flag set to true if this autotroph fixes N2
        imp_calcifier,                      & ! flag set to true if this autotroph implicitly handles calcification
        exp_calcifier                         ! flag set to true if this autotroph explicitly handles calcification
     integer (KIND=int_kind) :: &
        Chl_ind, C_ind, Fe_ind,             & ! tracer indices for Chl, C, Fe content
        Si_ind, CaCO3_ind,                  & ! tracer indices for Si, CaCO3 content
        C13_ind, C14_ind,                   & ! tracer indices for 13C, 14C
        Ca13CO3_ind, Ca14CO3_ind              ! tracer indices for 13CaCO3, 14CaCO3
     real(KIND=r8) :: &
        kFe, kPO4, kDOP, kNO3, kNH4, kSiO3, & ! nutrient uptake half-sat constants
        Qp,                                 & ! P/C ratio
        gQfe_0, gQfe_min,                   & ! initial and minimum fe/C ratio
        alphaPI,                            & ! init slope of P_I curve (GD98) (mmol C m^2/(mg Chl W sec))
        PCref,                              & ! max C-spec. grth rate at tref (1/sec)
        thetaN_max,                         & ! max thetaN (Chl/N) (mg Chl/mmol N)
        loss_thres, loss_thres2,            & ! conc. where losses go to zero
        temp_thres,                         & ! Temp. where concentration threshold and photosynth. rate drops
        mort, mort2,                        & ! linear and quadratic mortality rates (1/sec), (1/sec/((mmol C/m3))
        agg_rate_max, agg_rate_min,         & ! max and min agg. rate (1/d)
        loss_poc                              ! routing of loss term

  end type autotroph_type

  integer (KIND=int_kind), parameter :: &
     sp_ind          = 1, &  ! small phytoplankton
     diat_ind        = 2, &  ! diatoms
     diaz_ind        = 3     ! diazotrophs

  type(autotroph_type), dimension(autotroph_cnt) :: autotrophs


!-----------------------------------------------------------------------------
!   derived type for grazing
!-----------------------------------------------------------------------------

 type, public :: grazing_type
    character(char_len) :: sname, lname
     integer (KIND=int_kind) :: &
          grazing_function,                   & ! functional form of grazing parameterization
          auto_ind_cnt,                       & ! number of autotrophs in prey-clase auto_ind
          zoo_ind_cnt                             ! number of zooplankton in prey-clase zoo_ind
     real(KIND=r8) :: &
          z_umax_0,                           & ! max zoo growth rate at tref (1/sec)
          z_grz,                              & ! grazing coef. (mmol C/m^3)^2
          graze_zoo, graze_poc, graze_doc,    & ! routing of grazed term, remainder goes to dic
          f_zoo_detr                            ! fraction of zoo losses to detrital
     integer (KIND=int_kind), dimension(autotroph_cnt) :: &
          auto_ind
     integer (KIND=int_kind), dimension(zooplankton_cnt) :: &
          zoo_ind
  end type grazing_type

  type(grazing_type), dimension(grazer_prey_cnt,zooplankton_cnt) :: grazing

!-----------------------------------------------------------------------------
!  derived type for implicit handling of sinking particulate matter
!-----------------------------------------------------------------------------

   type, public :: sinking_particle
      real(r8) :: diss ! dissolution length for soft subclass
      real(r8) :: gamma ! fraction of production -> hard subclass
      real(r8) :: mass ! mass of 1e9 base units in g
      real(r8) :: rho  ! QA mass ratio of POC to this particle class

      real(r8) :: sflux_in(nx_block, ny_block, max_blocks_clinic) ! incoming flux of soft subclass (base units/cm^2/sec)
      real(r8) :: hflux_in(nx_block, ny_block, max_blocks_clinic) ! incoming flux of hard subclass (base units/cm^2/sec)
      real(r8) :: prod(nx_block, ny_block, max_blocks_clinic) ! production term (base units/cm^3/sec)
      real(r8) :: sflux_out(nx_block, ny_block, max_blocks_clinic) ! outgoing flux of soft subclass (base units/cm^2/sec)
      real(r8) :: hflux_out(nx_block, ny_block, max_blocks_clinic) ! outgoing flux of hard subclass (base units/cm^2/sec)
      real(r8) :: sed_loss(nx_block, ny_block, max_blocks_clinic) ! loss to sediments (base units/cm^s/sec)
      real(r8) :: remin(nx_block, ny_block, max_blocks_clinic)    ! remineralization term (base units/cm^3/sec)
   end type sinking_particle

   type, public :: column_sinking_particle_type
      real(r8) :: diss ! dissolution length for soft subclass
      real(r8) :: gamma ! fraction of production -> hard subclass
      real(r8) :: mass ! mass of 1e9 base units in g
      real(r8) :: rho  ! QA mass ratio of POC to this particle class

      real(r8) :: sflux_in (km) ! incoming flux of soft subclass (base units/cm^2/sec)
      real(r8) :: hflux_in (km) ! incoming flux of hard subclass (base units/cm^2/sec)
      real(r8) :: prod     (km) ! production term (base units/cm^3/sec)
      real(r8) :: sflux_out(km) ! outgoing flux of soft subclass (base units/cm^2/sec)
      real(r8) :: hflux_out(km) ! outgoing flux of hard subclass (base units/cm^2/sec)
      real(r8) :: sed_loss (km) ! loss to sediments (base units/cm^s/sec)
      real(r8) :: remin    (km) ! remineralization term (base units/cm^3/sec)
   end type column_sinking_particle_type

   !****************************************************************************
   !
   ! Shared data type definitions
   !
   !****************************************************************************

   type, public :: ecosys_interior_share_type
      real (r8) :: QA_dust_def(nx_block,ny_block,max_blocks_clinic) ! incoming deficit in the QA(dust) POC flux

      real(r8) :: DIC_loc_fields(nx_block, ny_block, max_blocks_clinic)   ! local copy of model DIC
      real(r8) :: DOC_loc_fields(nx_block, ny_block, max_blocks_clinic)   ! local copy of model DOC
      real(r8) :: O2_loc_fields(nx_block, ny_block, max_blocks_clinic)    ! local copy of model O2
      real(r8) :: NO3_loc_fields(nx_block, ny_block, max_blocks_clinic)   ! local copy of model NO3
      real(r8) :: CO3_fields(nx_block, ny_block, max_blocks_clinic)        ! carbonate ion
      real(r8) :: HCO3_fields(nx_block, ny_block, max_blocks_clinic)       ! bicarbonate ion
      real(r8) :: H2CO3_fields(nx_block, ny_block, max_blocks_clinic)      ! carbonic acid
      real(r8) :: DOC_remin_fields(nx_block, ny_block, max_blocks_clinic)  ! remineralization of 13C DOC (mmol C/m^3/sec)
   end type ecosys_interior_share_type


   type, public :: marbl_interior_share_type
      real (r8) :: QA_dust_def ! incoming deficit in the QA(dust) POC flux

      real(r8) :: DIC_loc_fields   ! local copy of model DIC
      real(r8) :: DOC_loc_fields   ! local copy of model DOC
      real(r8) :: O2_loc_fields    ! local copy of model O2
      real(r8) :: NO3_loc_fields   ! local copy of model NO3
     
      real(r8) :: CO3_fields
      real(r8) :: HCO3_fields      ! bicarbonate ion
      real(r8) :: H2CO3_fields     ! carbonic acid

      real(r8) :: DOC_remin_fields ! remineralization of 13C DOC (mmol C/m^3/sec)

   end type marbl_interior_share_type

!---------------------------------------------------------------------------

   type, public :: ecosys_zooplankton_share_type
      real(r8) :: zooC_loc_fields(nx_block, ny_block, zooplankton_cnt, max_blocks_clinic)     ! local copy of model zooC
      real(r8) :: zoo_loss_poc_fields(nx_block, ny_block, zooplankton_cnt, max_blocks_clinic) ! zoo_loss routed to large detrital (mmol C/m^3/sec)
      real(r8) :: zoo_loss_doc_fields(nx_block, ny_block, zooplankton_cnt, max_blocks_clinic) ! zoo_loss routed to doc (mmol C/m^3/sec)
      real(r8) :: zoo_loss_dic_fields(nx_block, ny_block, zooplankton_cnt, max_blocks_clinic) ! zoo_loss routed to dic (mmol C/m^3/sec)
      real(r8) :: zoo_loss_fields(nx_block, ny_block, zooplankton_cnt, max_blocks_clinic)     ! mortality & higher trophic grazing on zooplankton (mmol C/m^3/sec)
   end type ecosys_zooplankton_share_type


   type, public :: marbl_zooplankton_share_type
      real(r8) :: zooC_loc_fields     ! local copy of model zooC
      real(r8) :: zoo_loss_fields     ! mortality & higher trophic grazing on zooplankton (mmol C/m^3/sec)
      real(r8) :: zoo_loss_poc_fields ! zoo_loss routed to large detrital (mmol C/m^3/sec)
      real(r8) :: zoo_loss_doc_fields ! zoo_loss routed to doc (mmol C/m^3/sec)
      real(r8) :: zoo_loss_dic_fields ! zoo_loss routed to dic (mmol C/m^3/sec)
   end type marbl_zooplankton_share_type

!---------------------------------------------------------------------------

   type, public :: ecosys_autotroph_share_type
      real(r8) :: autotrophChl_loc_fields(nx_block, ny_block, autotroph_cnt, max_blocks_clinic) ! local copy of model autotroph Chl
      real(r8) :: autotrophC_loc_fields(nx_block, ny_block, autotroph_cnt, max_blocks_clinic) ! local copy of model autotroph C
      real(r8) :: autotrophFe_loc_fields(nx_block, ny_block, autotroph_cnt, max_blocks_clinic) ! local copy of model autotroph Fe
      real(r8) :: autotrophSi_loc_fields(nx_block, ny_block, autotroph_cnt, max_blocks_clinic) ! local copy of model autotroph Si
      real(r8) :: autotrophCaCO3_loc_fields(nx_block, ny_block, autotroph_cnt, max_blocks_clinic) ! local copy of model autotroph CaCO3
      real(r8) :: QCaCO3_fields(nx_block, ny_block, autotroph_cnt, max_blocks_clinic) ! small phyto CaCO3/C ratio (mmol CaCO3/mmol C)
      real(r8) :: auto_graze_fields(nx_block, ny_block, autotroph_cnt, max_blocks_clinic) ! autotroph grazing rate (mmol C/m^3/sec)
      real(r8) :: auto_graze_zoo_fields(nx_block, ny_block, autotroph_cnt, max_blocks_clinic) ! auto_graze routed to zoo (mmol C/m^3/sec)
      real(r8) :: auto_graze_poc_fields(nx_block, ny_block, autotroph_cnt, max_blocks_clinic) ! auto_graze routed to poc (mmol C/m^3/sec)
      real(r8) :: auto_graze_doc_fields(nx_block, ny_block, autotroph_cnt, max_blocks_clinic) ! auto_graze routed to doc (mmol C/m^3/sec)
      real(r8) :: auto_graze_dic_fields(nx_block, ny_block, autotroph_cnt, max_blocks_clinic) ! auto_graze routed to dic (mmol C/m^3/sec)
      real(r8) :: auto_loss_fields(nx_block, ny_block, autotroph_cnt, max_blocks_clinic) ! autotroph non-grazing mort (mmol C/m^3/sec)
      real(r8) :: auto_loss_poc_fields(nx_block, ny_block, autotroph_cnt, max_blocks_clinic) ! auto_loss routed to poc (mmol C/m^3/sec)
      real(r8) :: auto_loss_doc_fields(nx_block, ny_block, autotroph_cnt, max_blocks_clinic) ! auto_loss routed to doc (mmol C/m^3/sec)
      real(r8) :: auto_loss_dic_fields(nx_block, ny_block, autotroph_cnt, max_blocks_clinic) ! auto_loss routed to dic (mmol C/m^3/sec)
      real(r8) :: auto_agg_fields(nx_block, ny_block, autotroph_cnt, max_blocks_clinic) ! autotroph aggregation (mmol C/m^3/sec)
      real(r8) :: photoC_fields(nx_block, ny_block, autotroph_cnt, max_blocks_clinic) ! C-fixation (mmol C/m^3/sec)
      real(r8) :: CaCO3_PROD_fields(nx_block, ny_block, autotroph_cnt, max_blocks_clinic) ! prod. of CaCO3 by small phyto (mmol CaCO3/m^3/sec)
      real(r8) :: PCphoto_fields(nx_block, ny_block, autotroph_cnt, max_blocks_clinic) ! C-specific rate of photosynth. (1/sec)
   end type ecosys_autotroph_share_type


   type, public :: marbl_autotroph_share_type
      real(r8) :: autotrophChl_loc_fields ! local copy of model autotroph Chl
      real(r8) :: autotrophC_loc_fields ! local copy of model autotroph C
      real(r8) :: autotrophFe_loc_fields ! local copy of model autotroph Fe
      real(r8) :: autotrophSi_loc_fields ! local copy of model autotroph Si
      real(r8) :: autotrophCaCO3_loc_fields ! local copy of model autotroph CaCO3
      real(r8) :: QCaCO3_fields ! small phyto CaCO3/C ratio (mmol CaCO3/mmol C)
      real(r8) :: auto_graze_fields ! autotroph grazing rate (mmol C/m^3/sec)
      real(r8) :: auto_graze_zoo_fields ! auto_graze routed to zoo (mmol C/m^3/sec)
      real(r8) :: auto_graze_poc_fields ! auto_graze routed to poc (mmol C/m^3/sec)
      real(r8) :: auto_graze_doc_fields ! auto_graze routed to doc (mmol C/m^3/sec)
      real(r8) :: auto_graze_dic_fields ! auto_graze routed to dic (mmol C/m^3/sec)
      real(r8) :: auto_loss_fields ! autotroph non-grazing mort (mmol C/m^3/sec)
      real(r8) :: auto_loss_poc_fields ! auto_loss routed to poc (mmol C/m^3/sec)
      real(r8) :: auto_loss_doc_fields ! auto_loss routed to doc (mmol C/m^3/sec)
      real(r8) :: auto_loss_dic_fields ! auto_loss routed to dic (mmol C/m^3/sec)
      real(r8) :: auto_agg_fields ! autotroph aggregation (mmol C/m^3/sec)
      real(r8) :: photoC_fields ! C-fixation (mmol C/m^3/sec)
      real(r8) :: CaCO3_PROD_fields ! prod. of CaCO3 by small phyto (mmol CaCO3/m^3/sec)
      real(r8) :: PCphoto_fields ! C-specific rate of photosynth. (1/sec)
   end type marbl_autotroph_share_type

   !---------------------------------------------------------------------------
    
   type, public :: ecosys_particulate_share_type
      type(sinking_particle) :: POC      ! base units = nmol C
      type(sinking_particle) :: P_CaCO3  ! base units = nmol CaCO3
      type(sinking_particle) :: P_SiO2   ! base units = nmol SiO2
      type(sinking_particle) :: dust     ! base units = g
      type(sinking_particle) :: P_iron   ! base units = nmol Fe

      real(r8) :: POC_PROD_avail_fields(nx_block, ny_block, max_blocks_clinic)    ! POC production available for excess POC flux
      real(r8) :: decay_CaCO3_fields(nx_block, ny_block, max_blocks_clinic)       ! scaling factor for dissolution of CaCO3
      real(r8) :: decay_POC_E_fields(nx_block, ny_block, max_blocks_clinic)       ! scaling factor for dissolution of excess POC
      real(r8) :: poc_diss_fields(nx_block, ny_block, max_blocks_clinic)          ! diss. length used (cm)
      real(r8) :: caco3_diss_fields(nx_block, ny_block, max_blocks_clinic)        ! caco3 diss. length used (cm)
      real(r8) :: P_CaCO3_sflux_out_fields(nx_block, ny_block, max_blocks_clinic) ! P_CaCO3 sflux_out from ecosys before getting set to zero for k=KMT
      real(r8) :: P_CaCO3_hflux_out_fields(nx_block, ny_block, max_blocks_clinic) ! P_CaCO3_hflux_out from ecosys before getting set to zero for k=KMT
      real(r8) :: POC_sflux_out_fields(nx_block, ny_block, max_blocks_clinic)     ! POC_sflux_out from ecosys before getting set to zero for k=KMT
      real(r8) :: POC_hflux_out_fields(nx_block, ny_block, max_blocks_clinic)     ! POC_hflux_out from ecosys before getting set to zero for k=KMT
      real(r8) :: POC_remin_fields(nx_block, ny_block, max_blocks_clinic)         ! POC remin from ecosys before it gets modified for k=KMT
      real(r8) :: P_CaCO3_remin_fields(nx_block, ny_block, max_blocks_clinic)     ! P_CaCO3 remin from ecosys before it gets modified for k=KMT
      real(r8) :: DECAY_Hard_fields(nx_block, ny_block, max_blocks_clinic)        ! scaling factor for dissolution of Hard Ballast
   end type ecosys_particulate_share_type

   type, public :: marbl_particulate_share_type
      type(column_sinking_particle_type) :: POC      ! base units = nmol C
      type(column_sinking_particle_type) :: P_CaCO3  ! base units = nmol CaCO3
      type(column_sinking_particle_type) :: P_SiO2   ! base units = nmol SiO2
      type(column_sinking_particle_type) :: dust     ! base units = g
      type(column_sinking_particle_type) :: P_iron   ! base units = nmol Fe

      real(r8) :: POC_PROD_avail_fields(km)    ! POC production available for excess POC flux
      real(r8) :: decay_CaCO3_fields(km)       ! scaling factor for dissolution of CaCO3
      real(r8) :: decay_POC_E_fields(km)       ! scaling factor for dissolution of excess POC
      real(r8) :: poc_diss_fields(km)          ! diss. length used (cm)
      real(r8) :: caco3_diss_fields(km)        ! caco3 diss. length used (cm)
      real(r8) :: P_CaCO3_sflux_out_fields(km) ! P_CaCO3 sflux_out from ecosys before getting set to zero for k=KMT
      real(r8) :: P_CaCO3_hflux_out_fields(km) ! P_CaCO3_hflux_out from ecosys before getting set to zero for k=KMT
      real(r8) :: POC_sflux_out_fields(km)     ! POC_sflux_out from ecosys before getting set to zero for k=KMT
      real(r8) :: POC_hflux_out_fields(km)     ! POC_hflux_out from ecosys before getting set to zero for k=KMT
      real(r8) :: POC_remin_fields(km)         ! POC remin from ecosys before it gets modified for k=KMT
      real(r8) :: P_CaCO3_remin_fields(km)     ! P_CaCO3 remin from ecosys before it gets modified for k=KMT
      real(r8) :: DECAY_Hard_fields(km)        ! scaling factor for dissolution of Hard Ballast
   end type marbl_particulate_share_type

   !---------------------------------------------------------------------------
   
   type, public :: ecosys_surface_share_type
      real(r8) :: PV_SURF_fields(nx_block, ny_block, max_blocks_clinic)       ! piston velocity (cm/s)
      real(r8) :: DIC_SURF_fields(nx_block, ny_block, max_blocks_clinic)      ! surface values of DIC for solver
      real(r8) :: CO2STAR_SURF_fields(nx_block, ny_block, max_blocks_clinic)  ! CO2STAR from solver
      real(r8) :: DCO2STAR_SURF_fields(nx_block, ny_block, max_blocks_clinic) ! DCO2STAR from solver
      real(r8) :: CO3_SURF_fields(nx_block, ny_block, max_blocks_clinic)      ! Surface carbonate ion
      real(r8) :: dic_riv_flux_fields(nx_block, ny_block, max_blocks_clinic)  ! River input of DIC in ecosystem (from file)
      real(r8) :: doc_riv_flux_fields(nx_block, ny_block, max_blocks_clinic)  ! River input of DOC in ecosystem (from file)
   end type ecosys_surface_share_type

   !---------------------------------------------------------------------------

!***********************************************************************

 contains

!*****************************************************************************
! Functions and subroutines used by more than one ecosystem-related module
!*****************************************************************************
   subroutine column_sinking_particle_to_slab_sinking_particle(k, c, i, bid, column_particle, particle)
     integer(int_kind), intent(in) :: k, c, i, bid
     type(column_sinking_particle_type), intent(in) :: column_particle
     type(sinking_particle), intent(inout) :: particle

     particle%diss      = column_particle%diss  
     particle%gamma     = column_particle%gamma 
     particle%mass      = column_particle%mass  
     particle%rho       = column_particle%rho   

     particle%prod(i, c, bid)      = column_particle%prod(k)   
     particle%sflux_out(i, c, bid) = column_particle%sflux_out(k)
     particle%hflux_out(i, c, bid) = column_particle%hflux_out(k)
     particle%sflux_in(i, c, bid)  = column_particle%sflux_in(k)
     particle%hflux_in(i, c, bid)  = column_particle%hflux_in(k)
     particle%sed_loss(i, c, bid)  = column_particle%sed_loss(k)
     particle%remin(i, c, bid)     = column_particle%remin(k)

   end subroutine column_sinking_particle_to_slab_sinking_particle

   !---------------------------------------------------------------------------

   subroutine slab_sinking_particle_to_column_sinking_particle(k, c, i, bid, column_particle, particle)
     integer(int_kind), intent(in) :: k, c, i, bid
     type(column_sinking_particle_type), intent(out) :: column_particle
     type(sinking_particle), intent(in) :: particle

     column_particle%diss  = particle%diss  
     column_particle%gamma = particle%gamma 
     column_particle%mass  = particle%mass  
     column_particle%rho   = particle%rho   

     column_particle%sflux_out(k) = particle%sflux_out(i, c, bid)
     column_particle%hflux_out(k) = particle%hflux_out(i, c, bid)
     column_particle%sflux_in(k)  = particle%sflux_in(i, c, bid)
     column_particle%hflux_in(k)  = particle%hflux_in(i, c, bid)
     column_particle%sed_loss(k)  = particle%sed_loss(i, c, bid)
     column_particle%remin(k)     = particle%remin(i, c, bid)
     ! NOTE(bja, 2015-07) remin doesn't actually affect bit for bit
     ! reproducibility...?

   end subroutine slab_sinking_particle_to_column_sinking_particle

   !---------------------------------------------------------------------------

   subroutine column_interior_share_to_slab_interior_share(i, c, k, bid, &
        column_share, slab_share)

     integer(int_kind), intent(in) :: i, c, k, bid
     type(marbl_interior_share_type), intent(in) :: column_share
     type(ecosys_interior_share_type), intent(out) :: slab_share

     slab_share%QA_dust_def(i, c, bid) = column_share%QA_dust_def
     slab_share%DIC_loc_fields(i, c, bid) = column_share%DIC_loc_fields
     slab_share%DOC_loc_fields(i, c, bid) = column_share%DOC_loc_fields
     slab_share%O2_loc_fields(i, c, bid) = column_share%O2_loc_fields
     slab_share%NO3_loc_fields(i, c, bid) = column_share%NO3_loc_fields

     slab_share%CO3_fields(i, c, bid) = column_share%CO3_fields
     slab_share%HCO3_fields(i, c, bid) = column_share%HCO3_fields
     slab_share%H2CO3_fields(i, c, bid) = column_share%H2CO3_fields
     slab_share%DOC_remin_fields(i, c, bid) = column_share%DOC_remin_fields

   end subroutine column_interior_share_to_slab_interior_share

   !---------------------------------------------------------------------------

   subroutine column_zooplankton_share_to_slab_zooplankton_share(i, c, k, bid, &
        column_share, slab_share)

     integer(int_kind), intent(in) :: i, c, k, bid
     type(marbl_zooplankton_share_type), intent(in) :: column_share(zooplankton_cnt, km)
     type(ecosys_zooplankton_share_type), intent(out) :: slab_share

     integer(int_kind) :: n
     
     do n = 1, zooplankton_cnt
        slab_share%zooC_loc_fields(i, c, n, bid)     = column_share(n, k)%zooC_loc_fields
        slab_share%zoo_loss_fields(i, c, n, bid)     = column_share(n, k)%zoo_loss_fields
        slab_share%zoo_loss_poc_fields(i, c, n, bid) = column_share(n, k)%zoo_loss_poc_fields
        slab_share%zoo_loss_doc_fields(i, c, n, bid) = column_share(n, k)%zoo_loss_doc_fields
        slab_share%zoo_loss_dic_fields(i, c, n, bid) = column_share(n, k)%zoo_loss_dic_fields
     end do !n

   end subroutine column_zooplankton_share_to_slab_zooplankton_share

   !---------------------------------------------------------------------------

   subroutine column_autotroph_share_to_slab_autotroph_share(i, c, k, bid, &
        column_share, slab_share)

     integer(int_kind), intent(in) :: i, c, k, bid
     type(marbl_autotroph_share_type), intent(in) :: column_share(autotroph_cnt, km)
     type(ecosys_autotroph_share_type), intent(out) :: slab_share

     integer(int_kind) :: n

     do n = 1, autotroph_cnt
        slab_share%autotrophChl_loc_fields(i, c, n, bid) = column_share(n, k)%autotrophChl_loc_fields
        slab_share%autotrophC_loc_fields(i, c, n, bid)   = column_share(n, k)%autotrophC_loc_fields
        slab_share%autotrophFe_loc_fields(i, c, n, bid)  = column_share(n, k)%autotrophFe_loc_fields
        slab_share%autotrophSi_loc_fields(i, c, n, bid) = column_share(n, k)%autotrophSi_loc_fields
        slab_share%autotrophCaCO3_loc_fields(i, c, n, bid) = column_share(n, k)%autotrophCaCO3_loc_fields

        slab_share%QCaCO3_fields        (i, c, n, bid) = column_share(n, k)%QCaCO3_fields
        slab_share%auto_graze_fields    (i, c, n, bid) = column_share(n, k)%auto_graze_fields
        slab_share%auto_graze_zoo_fields(i, c, n, bid) = column_share(n, k)%auto_graze_zoo_fields
        slab_share%auto_graze_poc_fields(i, c, n, bid) = column_share(n, k)%auto_graze_poc_fields
        slab_share%auto_graze_doc_fields(i, c, n, bid) = column_share(n, k)%auto_graze_doc_fields
        slab_share%auto_graze_dic_fields(i, c, n, bid) = column_share(n, k)%auto_graze_dic_fields
        slab_share%auto_loss_fields     (i, c, n, bid) = column_share(n, k)%auto_loss_fields
        slab_share%auto_loss_poc_fields (i, c, n, bid) = column_share(n, k)%auto_loss_poc_fields
        slab_share%auto_loss_doc_fields (i, c, n, bid) = column_share(n, k)%auto_loss_doc_fields
        slab_share%auto_loss_dic_fields (i, c, n, bid) = column_share(n, k)%auto_loss_dic_fields
        slab_share%auto_agg_fields      (i, c, n, bid) = column_share(n, k)%auto_agg_fields
        slab_share%photoC_fields        (i, c, n, bid) = column_share(n, k)%photoC_fields
        slab_share%CaCO3_PROD_fields    (i, c, n, bid) = column_share(n, k)%CaCO3_PROD_fields
        slab_share%PCphoto_fields       (i, c, n, bid) = column_share(n, k)%PCphoto_fields
     end do ! do n
   end subroutine column_autotroph_share_to_slab_autotroph_share

   !---------------------------------------------------------------------------

   subroutine column_particulate_share_to_slab_particulate_share(i, c, k, bid, &
        column_share, slab_share)

     integer(int_kind), intent(in) :: i, c, k, bid
     type(marbl_particulate_share_type), intent(in) :: column_share
     type(ecosys_particulate_share_type), intent(out) :: slab_share

     call column_sinking_particle_to_slab_sinking_particle(k, c, i, bid, &
          column_share%POC, slab_share%POC)
     call column_sinking_particle_to_slab_sinking_particle(k, c, i, bid, &
          column_share%P_CaCO3, slab_share%P_CaCO3)
     call column_sinking_particle_to_slab_sinking_particle(k, c, i, bid, &
          column_share%P_SiO2, slab_share%P_SiO2)
     call column_sinking_particle_to_slab_sinking_particle(k, c, i, bid, &
          column_share%dust, slab_share%dust)
     call column_sinking_particle_to_slab_sinking_particle(k, c, i, bid, &
          column_share%P_iron, slab_share%P_iron)

     slab_share%POC_PROD_avail_fields(i, c, bid) = column_share%POC_PROD_avail_fields(k)
     slab_share%decay_CaCO3_fields(i, c, bid) = column_share%decay_CaCO3_fields(k)
     slab_share%decay_POC_E_fields(i, c, bid) = column_share%decay_POC_E_fields(k)
     slab_share%poc_diss_fields(i, c, bid) = column_share%poc_diss_fields(k)
     slab_share%caco3_diss_fields(i, c, bid) = column_share%caco3_diss_fields(k)
     slab_share%P_CaCO3_sflux_out_fields(i, c, bid) = column_share%P_CaCO3_sflux_out_fields(k)
     slab_share%P_CaCO3_hflux_out_fields(i, c, bid) = column_share%P_CaCO3_hflux_out_fields(k)
     slab_share%POC_sflux_out_fields(i, c, bid) = column_share%POC_sflux_out_fields(k)
     slab_share%POC_hflux_out_fields(i, c, bid) = column_share%POC_hflux_out_fields(k)
     slab_share%POC_remin_fields(i, c, bid) = column_share%POC_remin_fields(k)
     slab_share%P_CaCO3_remin_fields(i, c, bid) = column_share%P_CaCO3_remin_fields(k)
     slab_share%DECAY_Hard_fields(i, c, bid) = column_share%DECAY_Hard_fields(k)

   end subroutine column_particulate_share_to_slab_particulate_share

end module marbl_share_mod
