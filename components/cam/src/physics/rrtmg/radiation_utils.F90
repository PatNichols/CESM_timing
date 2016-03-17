module radiation_utils

use shr_kind_mod,     only: r8 => shr_kind_r8
use rad_constituents, only: N_DIAG
use ppgrid,           only: pcols, pver, pverp
use parrrtm,          only: nbndlw
use parrrsw,          only: nbndsw

public :: rad_diagdata_type

type rad_diagdata_type
   real(r8) :: solin(pcols,0:N_DIAG)         ! Solar incident flux
   real(r8) :: fsntoa(pcols,0:N_DIAG)        ! Net solar flux at TOA
   real(r8) :: fsutoa(pcols,0:N_DIAG)        ! upwelling solar flux at TOA
   real(r8) :: fsntoac(pcols,0:N_DIAG)       ! Clear sky net solar flux at TOA
   real(r8) :: fsnirt(pcols,0:N_DIAG)        ! Near-IR flux absorbed at toa
   real(r8) :: fsnrtc(pcols,0:N_DIAG)        ! Clear sky near-IR flux absorbed at toa
   real(r8) :: fsnirtsq(pcols,0:N_DIAG)      ! Near-IR flux absorbed at toa >= 0.7 microns
   real(r8) :: fsntc(pcols,0:N_DIAG)         ! Clear sky total column abs solar flux
   real(r8) :: fsnsc(pcols,0:N_DIAG)         ! Clear sky surface abs solar flux
   real(r8) :: fsdsc(pcols,0:N_DIAG)         ! Clear sky surface downwelling solar flux
   real(r8) :: flut(pcols,0:N_DIAG)          ! Upward flux at top of model
   real(r8) :: flutc(pcols,0:N_DIAG)         ! Upward Clear Sky flux at top of model
   real(r8) :: flntc(pcols,0:N_DIAG)         ! Clear sky lw flux at model top
   real(r8) :: flnsc(pcols,0:N_DIAG)         ! Clear sky lw flux at srf (up-down)
   real(r8) :: fldsc(pcols,0:N_DIAG)         ! Clear sky lw flux at srf (down)
   real(r8) :: flwds(pcols,0:N_DIAG)          ! Down longwave flux at surface
   real(r8) :: fsns(pcols,0:N_DIAG)          ! Surface solar absorbed flux
   real(r8) :: fsnr(pcols,0:N_DIAG)
   real(r8) :: fsnt(pcols,0:N_DIAG)          ! Net column abs solar flux at model top
   real(r8) :: flns(pcols,0:N_DIAG)          ! Srf longwave cooling (up-down) flux
   real(r8) :: flnt(pcols,0:N_DIAG)          ! Net outgoing lw flux at model top
   real(r8) :: flnr(pcols,0:N_DIAG)
   real(r8) :: fsds(pcols,0:N_DIAG)          ! Surface solar down flux
   real(r8) :: fln200(pcols,0:N_DIAG)        ! net longwave flux interpolated to 200 mb
   real(r8) :: fln200c(pcols,0:N_DIAG)       ! net clearsky longwave flux interpolated to 200 mb
   real(r8) :: fsn200(pcols,0:N_DIAG)        ! fns interpolated to 200 mb
   real(r8) :: fsn200c(pcols,0:N_DIAG)       ! fcns interpolated to 200 mb
   real(r8) :: sols(pcols,0:N_DIAG)          ! Solar downward visible direct  to surface
   real(r8) :: soll(pcols,0:N_DIAG)          ! Solar downward near infrared direct  to surface
   real(r8) :: solsd(pcols,0:N_DIAG)         ! Solar downward visible diffuse to surface
   real(r8) :: solld(pcols,0:N_DIAG)         ! Solar downward near infrared diffuse to surface
   real(r8) :: qrs(pcols,pver,0:N_DIAG)
   real(r8) :: qrl(pcols,pver,0:N_DIAG)
   real(r8) :: qrsc(pcols,pver,0:N_DIAG)
   real(r8) :: qrlc(pcols,pver,0:N_DIAG)

   real(r8) :: su(pcols,pverp,nbndsw,0:N_DIAG) ! shortwave spectral flux up
   real(r8) :: sd(pcols,pverp,nbndsw,0:N_DIAG) ! shortwave spectral flux down
   real(r8) :: lu(pcols,pverp,nbndlw,0:N_DIAG) ! longwave  spectral flux up
   real(r8) :: ld(pcols,pverp,nbndlw,0:N_DIAG) ! longwave  spectral flux down

    ! These do not need N_DIAG dimension
    real(r8) :: c_cld_tau(nbndsw,pcols,pver) ! cloud extinction optical depth
    real(r8) :: liq_tau(nbndsw,pcols,pver)   ! liquid extinction optical depth
    real(r8) :: ice_tau(nbndsw,pcols,pver)   ! ice extinction optical depth
    real(r8) :: cld_tau(nbndsw,pcols,pver)   ! cloud extinction optical depth
    real(r8) :: snow_tau(nbndsw,pcols,pver)  ! snow extinction optical depth

    real(r8) :: aer_tau(pcols,0:pver,nbndsw) ! aerosol extinction optical depth

    real(r8) :: cld_lw_abs (nbndlw,pcols,pver) ! cloud absorption optics depth (LW)
    real(r8) :: snow_lw_abs (nbndlw,pcols,pver)! snow absorption optics depth (LW)

    real(r8) :: cldfprime(pcols,pver)       ! combined cloud fraction (snow plus regular)

    real(r8) :: tot_cld_vistau(pcols,pver)   ! gbx water+ice cloud optical depth (only during day, night = fillvalue)
    real(r8) :: tot_icld_vistau(pcols,pver)  ! in-cld water+ice cloud optical depth (only during day, night = fillvalue)
    real(r8) :: liq_icld_vistau(pcols,pver)  ! in-cld liq cloud optical depth (only during day, night = fillvalue)
    real(r8) :: ice_icld_vistau(pcols,pver)  ! in-cld ice cloud optical depth (only during day, night = fillvalue)
    real(r8) :: snow_icld_vistau(pcols,pver) ! snow in-cloud visible sw optical depth for output on history files

    real(r8) swcf(pcols,0:N_DIAG)          ! shortwave cloud forcing
    real(r8) lwcf(pcols,0:N_DIAG)          ! longwave cloud forcing

end type rad_diagdata_type

end module radiation_utils
