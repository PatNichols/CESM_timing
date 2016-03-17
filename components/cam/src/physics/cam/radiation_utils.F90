module radiation_utils

use shr_kind_mod,     only: r8 => shr_kind_r8
use ppgrid,           only: pcols, pver


public :: rad_diagdata_type

type rad_diagdata_type
   real(r8) :: solin(pcols)         ! Solar incident flux
   real(r8) :: fsntoa(pcols)        ! Net solar flux at TOA
   real(r8) :: fsutoa(pcols)        ! upwelling solar flux at TOA
   real(r8) :: fsntoac(pcols)       ! Clear sky net solar flux at TOA
   real(r8) :: fsnirt(pcols)        ! Near-IR flux absorbed at toa
   real(r8) :: fsnrtc(pcols)        ! Clear sky near-IR flux absorbed at toa
   real(r8) :: fsnirtsq(pcols)      ! Near-IR flux absorbed at toa >= 0.7 microns
   real(r8) :: fsntc(pcols)         ! Clear sky total column abs solar flux
   real(r8) :: fsnsc(pcols)         ! Clear sky surface abs solar flux
   real(r8) :: fsdsc(pcols)         ! Clear sky surface downwelling solar flux
   real(r8) :: flut(pcols)          ! Upward flux at top of model
   real(r8) :: flutc(pcols)         ! Upward Clear Sky flux at top of model
   real(r8) :: flntc(pcols)         ! Clear sky lw flux at model top
   real(r8) :: flnsc(pcols)         ! Clear sky lw flux at srf (up-down)
   real(r8) :: fldsc(pcols)         ! Clear sky lw flux at srf (down)
   real(r8) :: flwds(pcols)          ! Down longwave flux at surface
   real(r8) :: fsnr(pcols)
   real(r8) :: flnr(pcols)
   real(r8) :: fsds(pcols)          ! Surface solar down flux
   real(r8) :: fln200(pcols)        ! net longwave flux interpolated to 200 mb
   real(r8) :: fln200c(pcols)       ! net clearsky longwave flux interpolated to 200 mb
   real(r8) :: fsn200(pcols)        ! fns interpolated to 200 mb
   real(r8) :: fsn200c(pcols)       ! fcns interpolated to 200 mb
   real(r8) :: sols(pcols)          ! Solar downward visible direct  to surface
   real(r8) :: soll(pcols)          ! Solar downward near infrared direct  to surface
   real(r8) :: solsd(pcols)         ! Solar downward visible diffuse to surface
   real(r8) :: solld(pcols)         ! Solar downward near infrared diffuse to surface
   real(r8) :: qrsc(pcols,pver)                    ! clearsky shortwave radiative heating rate
   real(r8) :: qrlc(pcols,pver)                    ! clearsky longwave  radiative heating rate
   real(r8) :: fsdtoa(pcols)        ! Solar input = Flux Solar Downward Top of Atmosphere
   real(r8) :: swcf(pcols)          ! shortwave cloud forcing
   real(r8) :: lwcf(pcols)          ! longwave cloud forcing

   real(r8) :: qvrad(pcols,pver) ! rad vapor

   real(r8) :: tot_cld_vistau(pcols,pver)  
   real(r8) :: tot_icld_vistau(pcols,pver) 
   real(r8) :: liq_icld_vistau(pcols,pver)  ! in-cld liq cloud optical depth (only during day, night = fillvalue)
   real(r8) :: ice_icld_vistau(pcols,pver)  ! in-cld ice cloud optical depth (only during day, night = fillvalue)

end type rad_diagdata_type

end module radiation_utils
