module history_scam
!----------------------------------------------------------------------- 
! 
! Purpose: SCAM specific history code.
!
! Public functions/subroutines:
!   bldfld, h_default
! 
! Author: anonymous from code in cam_history.F90
!-----------------------------------------------------------------------
   use shr_kind_mod, only: r8 => shr_kind_r8

   implicit none

PRIVATE

   public :: scm_intht

!#######################################################################
CONTAINS
   subroutine scm_intht()
!----------------------------------------------------------------------- 
! 
! Purpose: 
!
! add master list fields to scm
! 
! Method: Call a subroutine to add each field
! 
! Author: CCM Core Group
! 
!-----------------------------------------------------------------------
      use cam_history, only: addfld, add_default, horiz_only
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
! Local variables
!
      integer m,j        ! Indices
      real(r8) dummy
!
! Call addfld to add each field to the Master Field List.
!
      call addfld ('TDIFF',    (/ 'lev' /), 'A', 'K','difference from observed temp',                    gridname='gauss_grid')

      call addfld ('TOBS',     (/ 'lev' /), 'A', 'K','observed temp')
      call addfld ('QDIFF',    (/ 'lev' /), 'A', 'kg/kg','difference from observed water',               gridname='gauss_grid')

      call addfld ('QOBS',     (/ 'lev' /), 'A', 'kg/kg','observed water',                               gridname='physgrid')
      call addfld ('PRECOBS',  (/ 'lev' /), 'A', 'mm/day','Total (convective and large-scale) precipitation rate',             &
                                                                                                         gridname='physgrid')
      call addfld ('DIVQ',     (/ 'lev' /), 'A', 'kg/kg/s','Q advection tendency (horizontal)',          gridname='physgrid')
      call addfld ('DIVQ3D',   (/ 'lev' /), 'A', 'kg/kg/s','Q advection tendency (horiz/vert combined)', gridname='gauss_grid')
      call addfld ('DIVV',     (/ 'lev' /), 'A', 'm/s2','V advection tendency (horizontal)',             gridname='physgrid')
      call addfld ('DIVU',     (/ 'lev' /), 'A', 'm/s2','U advection tendency (horizontal)',             gridname='physgrid')
      call addfld ('DIVT',     (/ 'lev' /), 'A', 'K/s','T advection tendency (horizontal)',              gridname='physgrid')
      call addfld ('DIVT3D',   (/ 'lev' /), 'A', 'K/s','T advection tendency (horiz/vert combined)',     gridname='gauss_grid')

      call addfld ('SHFLXOBS', horiz_only,  'A', 'W/m2','Obs Surface sensible heat flux',                gridname='physgrid')
      call addfld ('LHFLXOBS', horiz_only,  'A', 'W/m2','Obs Surface latent heat flux',                  gridname='physgrid')
      call addfld ('TRELAX',   (/ 'lev' /), 'A', 'K','t relaxation amount',                              gridname='gauss_grid')
      call addfld ('QRELAX',   (/ 'lev' /), 'A', 'kg/kg','q relaxation amount',                          gridname='gauss_grid')
      call addfld ('TAURELAX', (/ 'lev' /), 'A', 'seconds','relaxation time constant',                   gridname='gauss_grid')
      call add_default ('TDIFF', 1, ' ')
      call add_default ('QDIFF', 1, ' ')
   end subroutine scm_intht

!#######################################################################
 end module history_scam
