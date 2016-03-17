
      module mo_setrxt

      use shr_kind_mod, only : r8 => shr_kind_r8

      private
      public :: setrxt
      public :: setrxt_hrates

      contains

      subroutine setrxt( rate, temp, m, ncol )

      use ppgrid,       only : pver, pcols
      use shr_kind_mod, only : r8 => shr_kind_r8
      use chem_mods, only : rxntot
      use mo_jpl,    only : jpl

      implicit none

!-------------------------------------------------------
!       ... dummy arguments
!-------------------------------------------------------
      integer, intent(in) :: ncol
      real(r8), intent(in)    :: temp(pcols,pver)
      real(r8), intent(in)    :: m(ncol,pver)
      real(r8), intent(inout) :: rate(ncol,pver,rxntot)

!-------------------------------------------------------
!       ... local variables
!-------------------------------------------------------
      integer  ::  n
      real(r8)  ::  itemp(ncol,pver)
      real(r8)  ::  exp_fac(ncol,pver)
      real(r8)  :: ko(ncol,pver)
      real(r8)  :: kinf(ncol,pver)

      rate(:,:,145) = 1.20e-10_r8
      rate(:,:,146) = 2.02e-10_r8
      rate(:,:,147) = 1.204e-10_r8
      rate(:,:,148) = 1.50e-10_r8
      rate(:,:,149) = 9.75e-11_r8
      rate(:,:,150) = 1.50e-11_r8
      rate(:,:,151) = 7.20e-11_r8
      rate(:,:,152) = 1.794e-10_r8
      rate(:,:,153) = 1.628e-10_r8
      rate(:,:,154) = 2.84e-10_r8
      rate(:,:,155) = 1.674e-10_r8
      rate(:,:,156) = 9.60e-11_r8
      rate(:,:,157) = 4.10e-11_r8
      rate(:,:,158) = 1.012e-10_r8
      rate(:,:,159) = 1.20e-10_r8
      rate(:,:,160) = 4.49e-10_r8
      rate(:,:,161) = 2.57e-10_r8
      rate(:,:,162) = 1.31e-10_r8
      rate(:,:,163) = 3.50e-11_r8
      rate(:,:,164) = 9.00e-12_r8
      rate(:,:,165) = 1.20e-10_r8
      rate(:,:,166) = 1.50e-10_r8
      rate(:,:,167) = 1.20e-10_r8
      rate(:,:,171) = 7.20e-11_r8
      rate(:,:,172) = 6.90e-12_r8
      rate(:,:,173) = 1.60e-12_r8
      rate(:,:,177) = 1.80e-12_r8
      rate(:,:,180) = 1.80e-12_r8
      rate(:,:,204) = 1.00e-11_r8
      rate(:,:,205) = 2.20e-11_r8
      rate(:,:,206) = 3.50e-12_r8
      rate(:,:,231) = 1.70e-13_r8
      rate(:,:,278) = 4.50e-13_r8
      rate(:,:,290) = 1.00e-14_r8
      rate(:,:,293) = 7.00e-13_r8
      rate(:,:,296) = 2.00e-13_r8
      rate(:,:,297) = 6.80e-14_r8
      rate(:,:,306) = 1.00e-12_r8
      rate(:,:,307) = 1.00e-11_r8
      rate(:,:,308) = 1.15e-11_r8
      rate(:,:,311) = 4.00e-14_r8
      rate(:,:,328) = 3.00e-12_r8
      rate(:,:,331) = 6.7e-13_r8
      rate(:,:,332) = 5.40e-11_r8
      rate(:,:,335) = 3.5e-13_r8
      rate(:,:,346) = 2.40e-12_r8
      rate(:,:,349) = 1.40e-11_r8
      rate(:,:,352) = 5.00e-12_r8
      rate(:,:,360) = 2.0e-12_r8
      rate(:,:,366) = 4.e-11_r8
      rate(:,:,367) = 4.e-11_r8
      rate(:,:,368) = 2.40e-12_r8
      rate(:,:,369) = 2.40e-12_r8
      rate(:,:,373) = 1.3e-11_r8
      rate(:,:,376) = 1.40e-11_r8
      rate(:,:,377) = 1.40e-11_r8
      rate(:,:,381) = 2.40e-12_r8
      rate(:,:,383) = 1.4e-11_r8
      rate(:,:,385) = 4.0e-11_r8
      rate(:,:,386) = 7.0e-11_r8
      rate(:,:,387) = 1.0e-10_r8
      rate(:,:,390) = 2.40e-12_r8
      rate(:,:,396) = 3.50e-12_r8
      rate(:,:,397) = 6.7e-12_r8
      rate(:,:,399) = 1.6e-12_r8
      rate(:,:,405) = 1.4e-11_r8
      rate(:,:,410) = 1e-17_r8
      rate(:,:,414) = 2.4e-12_r8
      rate(:,:,420) = 2.1e-12_r8
      rate(:,:,421) = 2.8e-13_r8
      rate(:,:,432) = 4.7e-11_r8
      rate(:,:,450) = 1.7e-11_r8
      rate(:,:,451) = 8.4e-11_r8
      rate(:,:,461) = 2.1e-10_r8
      rate(:,:,462) = 2.0e-10_r8
      rate(:,:,466) = 4.7e-16_r8
      rate(:,:,467) = 1.2e-14_r8
      rate(:,:,469) = 2.5e-12_r8
      rate(:,:,470) = 1.1e-11_r8
      rate(:,:,471) = 1.2e-11_r8
      rate(:,:,472) = 1.9e-11_r8
      rate(:,:,476) = 3.3e-11_r8
      rate(:,:,477) = 2.3e-11_r8
      rate(:,:,478) = 5.7e-11_r8
      rate(:,:,479) = 1e-12_r8
      rate(:,:,483) = 3.4e-11_r8
      rate(:,:,487) = 2.4e-12_r8
      rate(:,:,488) = 2.0e-11_r8
      rate(:,:,489) = 2.0e-11_r8
      rate(:,:,494) = 2.1e-10_r8
      rate(:,:,495) = 2.0e-10_r8
      rate(:,:,496) = 1.34e-11_r8
      rate(:,:,497) = 1.34e-11_r8
      rate(:,:,500) = 1.7e-11_r8
      rate(:,:,505) = 4.7e-16_r8
      rate(:,:,506) = 1.2e-14_r8
      rate(:,:,508) = 2.5e-12_r8
      rate(:,:,509) = 2.5e-12_r8
      rate(:,:,510) = 2.5e-12_r8
      itemp(:ncol,:) = 1._r8 / temp(:ncol,:)
      n = ncol*pver
      rate(:,:,138) = 8.00e-12_r8 * exp( -2060._r8 * itemp(:,:) )
      rate(:,:,140) = 2.15e-11_r8 * exp( 110._r8 * itemp(:,:) )
      rate(:,:,141) = 3.30e-11_r8 * exp( 55._r8 * itemp(:,:) )
      rate(:,:,142) = 1.63e-10_r8 * exp( 60._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 20._r8 * itemp(:,:) )
      rate(:,:,143) = 7.25e-11_r8 * exp_fac(:,:)
      rate(:,:,144) = 4.63e-11_r8 * exp_fac(:,:)
      exp_fac(:,:) = exp( 100._r8 * itemp(:,:) )
      rate(:,:,168) = 7.70e-11_r8 * exp_fac(:,:)
      rate(:,:,189) = 2.10e-11_r8 * exp_fac(:,:)
      rate(:,:,170) = 1.40e-10_r8 * exp( -470._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 180._r8 * itemp(:,:) )
      rate(:,:,174) = 1.80e-11_r8 * exp_fac(:,:)
      rate(:,:,288) = 4.20e-12_r8 * exp_fac(:,:)
      rate(:,:,315) = 4.20e-12_r8 * exp_fac(:,:)
      rate(:,:,320) = 4.20e-12_r8 * exp_fac(:,:)
      rate(:,:,339) = 4.20e-12_r8 * exp_fac(:,:)
      rate(:,:,364) = 4.40e-12_r8 * exp_fac(:,:)
      rate(:,:,365) = 4.40e-12_r8 * exp_fac(:,:)
      rate(:,:,473) = 4.2e-12_r8 * exp_fac(:,:)
      rate(:,:,480) = 4.2e-12_r8 * exp_fac(:,:)
      rate(:,:,484) = 4.2e-12_r8 * exp_fac(:,:)
      rate(:,:,175) = 1.70e-12_r8 * exp( -940._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 250._r8 * itemp(:,:) )
      rate(:,:,176) = 4.80e-11_r8 * exp_fac(:,:)
      rate(:,:,241) = 1.70e-11_r8 * exp_fac(:,:)
      rate(:,:,179) = 2.80e-12_r8 * exp( -1800._r8 * itemp(:,:) )
      rate(:,:,181) = 1.60e-11_r8 * exp( -4570._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 200._r8 * itemp(:,:) )
      rate(:,:,182) = 3.00e-11_r8 * exp_fac(:,:)
      rate(:,:,249) = 5.50e-12_r8 * exp_fac(:,:)
      rate(:,:,277) = 3.80e-12_r8 * exp_fac(:,:)
      rate(:,:,298) = 3.80e-12_r8 * exp_fac(:,:)
      rate(:,:,318) = 3.80e-12_r8 * exp_fac(:,:)
      rate(:,:,322) = 3.80e-12_r8 * exp_fac(:,:)
      rate(:,:,327) = 3.80e-12_r8 * exp_fac(:,:)
      rate(:,:,341) = 3.80e-12_r8 * exp_fac(:,:)
      rate(:,:,350) = 2.30e-11_r8 * exp_fac(:,:)
      rate(:,:,372) = 1.52e-11_r8 * exp_fac(:,:)
      rate(:,:,401) = 3.80e-12_r8 * exp_fac(:,:)
      rate(:,:,409) = 3.8e-12_r8 * exp_fac(:,:)
      rate(:,:,419) = 3.8e-12_r8 * exp_fac(:,:)
      rate(:,:,424) = 3.8e-12_r8 * exp_fac(:,:)
      rate(:,:,427) = 3.8e-12_r8 * exp_fac(:,:)
      rate(:,:,434) = 3.8e-12_r8 * exp_fac(:,:)
      rate(:,:,442) = 3.80e-12_r8 * exp_fac(:,:)
      rate(:,:,454) = 3.8e-12_r8 * exp_fac(:,:)
      rate(:,:,456) = 3.8e-12_r8 * exp_fac(:,:)
      rate(:,:,183) = 1.00e-14_r8 * exp( -490._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( -2000._r8 * itemp(:,:) )
      rate(:,:,185) = 1.40e-12_r8 * exp_fac(:,:)
      rate(:,:,362) = 1.05e-14_r8 * exp_fac(:,:)
      rate(:,:,501) = 1.05e-14_r8 * exp_fac(:,:)
      rate(:,:,187) = 7.80e-13_r8 * exp( -1050._r8 * itemp(:,:) )
      rate(:,:,188) = 1.50e-11_r8 * exp( -3600._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 220._r8 * itemp(:,:) )
      rate(:,:,190) = 2.90e-12_r8 * exp_fac(:,:)
      rate(:,:,191) = 1.45e-12_r8 * exp_fac(:,:)
      rate(:,:,192) = 1.45e-12_r8 * exp_fac(:,:)
      exp_fac(:,:) = exp( 270._r8 * itemp(:,:) )
      rate(:,:,194) = 3.30e-12_r8 * exp_fac(:,:)
      rate(:,:,213) = 1.40e-11_r8 * exp_fac(:,:)
      rate(:,:,218) = 7.40e-12_r8 * exp_fac(:,:)
      rate(:,:,301) = 8.10e-12_r8 * exp_fac(:,:)
      exp_fac(:,:) = exp( -1500._r8 * itemp(:,:) )
      rate(:,:,195) = 3.00e-12_r8 * exp_fac(:,:)
      rate(:,:,250) = 5.80e-12_r8 * exp_fac(:,:)
      rate(:,:,196) = 5.10e-12_r8 * exp( 210._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( -2450._r8 * itemp(:,:) )
      rate(:,:,198) = 1.20e-13_r8 * exp_fac(:,:)
      rate(:,:,224) = 3.00e-11_r8 * exp_fac(:,:)
      rate(:,:,203) = 1.50e-11_r8 * exp( 170._r8 * itemp(:,:) )
      rate(:,:,208) = 1.30e-12_r8 * exp( 380._r8 * itemp(:,:) )
      rate(:,:,210) = 2.30e-11_r8 * exp( -200._r8 * itemp(:,:) )
      rate(:,:,211) = 3.05e-11_r8 * exp( -2270._r8 * itemp(:,:) )
      rate(:,:,212) = 1.10e-11_r8 * exp( -980._r8 * itemp(:,:) )
      rate(:,:,214) = 3.60e-11_r8 * exp( -375._r8 * itemp(:,:) )
      rate(:,:,215) = 8.10e-11_r8 * exp( -30._r8 * itemp(:,:) )
      rate(:,:,216) = 7.30e-12_r8 * exp( -1280._r8 * itemp(:,:) )
      rate(:,:,217) = 2.80e-11_r8 * exp( 85._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 230._r8 * itemp(:,:) )
      rate(:,:,219) = 6.00e-13_r8 * exp_fac(:,:)
      rate(:,:,240) = 1.90e-11_r8 * exp_fac(:,:)
      rate(:,:,248) = 1.50e-12_r8 * exp_fac(:,:)
      exp_fac(:,:) = exp( 290._r8 * itemp(:,:) )
      rate(:,:,220) = 2.60e-12_r8 * exp_fac(:,:)
      rate(:,:,222) = 6.40e-12_r8 * exp_fac(:,:)
      rate(:,:,247) = 4.10e-13_r8 * exp_fac(:,:)
      rate(:,:,429) = 7.5e-12_r8 * exp_fac(:,:)
      rate(:,:,439) = 7.5e-12_r8 * exp_fac(:,:)
      rate(:,:,445) = 7.5e-12_r8 * exp_fac(:,:)
      rate(:,:,447) = 7.5e-12_r8 * exp_fac(:,:)
      rate(:,:,221) = 3.3e-12_r8 * exp( -115._r8 * itemp(:,:) )
      rate(:,:,225) = 1.00e-12_r8 * exp( -1590._r8 * itemp(:,:) )
      rate(:,:,226) = 3.50e-13_r8 * exp( -1370._r8 * itemp(:,:) )
      rate(:,:,229) = 1.80e-12_r8 * exp( -250._r8 * itemp(:,:) )
      rate(:,:,230) = 1.00e-11_r8 * exp( -3300._r8 * itemp(:,:) )
      rate(:,:,232) = 3.40e-12_r8 * exp( -130._r8 * itemp(:,:) )
      rate(:,:,233) = 3.00e-12_r8 * exp( -500._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( -840._r8 * itemp(:,:) )
      rate(:,:,234) = 3.60e-12_r8 * exp_fac(:,:)
      rate(:,:,261) = 2.00e-12_r8 * exp_fac(:,:)
      rate(:,:,235) = 1.20e-12_r8 * exp( -330._r8 * itemp(:,:) )
      rate(:,:,236) = 6.50e-12_r8 * exp( 135._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( -780._r8 * itemp(:,:) )
      rate(:,:,237) = 1.60e-11_r8 * exp_fac(:,:)
      rate(:,:,465) = 3.0e-15_r8 * exp_fac(:,:)
      rate(:,:,238) = 4.80e-12_r8 * exp( -310._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( -800._r8 * itemp(:,:) )
      rate(:,:,239) = 1.70e-11_r8 * exp_fac(:,:)
      rate(:,:,263) = 6.30e-12_r8 * exp_fac(:,:)
      rate(:,:,242) = 4.50e-12_r8 * exp( 460._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 260._r8 * itemp(:,:) )
      rate(:,:,243) = 8.80e-12_r8 * exp_fac(:,:)
      rate(:,:,246) = 2.30e-12_r8 * exp_fac(:,:)
      rate(:,:,245) = 9.50e-13_r8 * exp( 550._r8 * itemp(:,:) )
      rate(:,:,251) = 1.20e-10_r8 * exp( -430._r8 * itemp(:,:) )
      rate(:,:,252) = 1.90e-11_r8 * exp( 215._r8 * itemp(:,:) )
      rate(:,:,253) = 2.17e-11_r8 * exp( -1130._r8 * itemp(:,:) )
      rate(:,:,254) = 2.40e-12_r8 * exp( -1250._r8 * itemp(:,:) )
      rate(:,:,255) = 1.64e-12_r8 * exp( -1520._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( -1600._r8 * itemp(:,:) )
      rate(:,:,256) = 1.05e-12_r8 * exp_fac(:,:)
      rate(:,:,259) = 1.25e-12_r8 * exp_fac(:,:)
      rate(:,:,270) = 3.40e-11_r8 * exp_fac(:,:)
      exp_fac(:,:) = exp( -1300._r8 * itemp(:,:) )
      rate(:,:,257) = 2.35e-12_r8 * exp_fac(:,:)
      rate(:,:,464) = 1.7e-15_r8 * exp_fac(:,:)
      rate(:,:,503) = 1.7e-15_r8 * exp_fac(:,:)
      rate(:,:,258) = 1.40e-11_r8 * exp( -1030._r8 * itemp(:,:) )
      rate(:,:,260) = 1.30e-12_r8 * exp( -1770._r8 * itemp(:,:) )
      rate(:,:,262) = 1.35e-12_r8 * exp( -600._r8 * itemp(:,:) )
      rate(:,:,264) = 4.85e-12_r8 * exp( -850._r8 * itemp(:,:) )
      rate(:,:,265) = 2.45e-12_r8 * exp( -1775._r8 * itemp(:,:) )
      rate(:,:,268) = 6.00e-13_r8 * exp( -2058._r8 * itemp(:,:) )
      rate(:,:,269) = 5.50e-12_r8 * exp( 125._r8 * itemp(:,:) )
      rate(:,:,271) = 9.7e-15_r8 * exp( 625._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 300._r8 * itemp(:,:) )
      rate(:,:,272) = 2.80e-12_r8 * exp_fac(:,:)
      rate(:,:,324) = 2.90e-12_r8 * exp_fac(:,:)
      rate(:,:,273) = 4.10e-13_r8 * exp( 750._r8 * itemp(:,:) )
      rate(:,:,274) = 5.00e-13_r8 * exp( -424._r8 * itemp(:,:) )
      rate(:,:,275) = 1.90e-14_r8 * exp( 706._r8 * itemp(:,:) )
      rate(:,:,276) = 2.90e-12_r8 * exp( -345._r8 * itemp(:,:) )
      rate(:,:,279) = 2.40e12_r8 * exp( -7000._r8 * itemp(:,:) )
      rate(:,:,280) = 2.60e-12_r8 * exp( 265._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 700._r8 * itemp(:,:) )
      rate(:,:,281) = 7.50e-13_r8 * exp_fac(:,:)
      rate(:,:,289) = 7.50e-13_r8 * exp_fac(:,:)
      rate(:,:,295) = 7.50e-13_r8 * exp_fac(:,:)
      rate(:,:,316) = 7.50e-13_r8 * exp_fac(:,:)
      rate(:,:,321) = 7.50e-13_r8 * exp_fac(:,:)
      rate(:,:,325) = 8.60e-13_r8 * exp_fac(:,:)
      rate(:,:,340) = 7.50e-13_r8 * exp_fac(:,:)
      rate(:,:,347) = 8.00e-13_r8 * exp_fac(:,:)
      rate(:,:,370) = 8.00e-13_r8 * exp_fac(:,:)
      rate(:,:,371) = 8.00e-13_r8 * exp_fac(:,:)
      rate(:,:,382) = 8.00e-13_r8 * exp_fac(:,:)
      rate(:,:,391) = 8.00e-13_r8 * exp_fac(:,:)
      rate(:,:,400) = 7.50e-13_r8 * exp_fac(:,:)
      rate(:,:,408) = 7.5e-13_r8 * exp_fac(:,:)
      rate(:,:,418) = 7.5e-13_r8 * exp_fac(:,:)
      rate(:,:,423) = 7.5e-13_r8 * exp_fac(:,:)
      rate(:,:,426) = 7.5e-13_r8 * exp_fac(:,:)
      rate(:,:,433) = 7.5e-13_r8 * exp_fac(:,:)
      rate(:,:,441) = 7.50e-13_r8 * exp_fac(:,:)
      rate(:,:,453) = 7.5e-13_r8 * exp_fac(:,:)
      rate(:,:,455) = 7.5e-13_r8 * exp_fac(:,:)
      rate(:,:,474) = 7.5e-13_r8 * exp_fac(:,:)
      rate(:,:,481) = 7.5e-13_r8 * exp_fac(:,:)
      rate(:,:,485) = 7.5e-13_r8 * exp_fac(:,:)
      rate(:,:,284) = 7.20e-11_r8 * exp( -70._r8 * itemp(:,:) )
      rate(:,:,286) = 7.66e-12_r8 * exp( -1020._r8 * itemp(:,:) )
      rate(:,:,291) = 1.60e11_r8 * exp( -4150._r8 * itemp(:,:) )
      rate(:,:,292) = 1.2e-14_r8 * exp( -2630._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 365._r8 * itemp(:,:) )
      rate(:,:,294) = 2.60e-12_r8 * exp_fac(:,:)
      rate(:,:,403) = 2.6e-12_r8 * exp_fac(:,:)
      rate(:,:,406) = 2.6e-12_r8 * exp_fac(:,:)
      rate(:,:,413) = 2.6e-12_r8 * exp_fac(:,:)
      rate(:,:,417) = 2.6e-12_r8 * exp_fac(:,:)
      rate(:,:,422) = 2.6e-12_r8 * exp_fac(:,:)
      rate(:,:,425) = 2.6e-12_r8 * exp_fac(:,:)
      rate(:,:,435) = 2.6e-12_r8 * exp_fac(:,:)
      rate(:,:,443) = 2.6e-12_r8 * exp_fac(:,:)
      rate(:,:,452) = 2.6e-12_r8 * exp_fac(:,:)
      rate(:,:,457) = 2.6e-12_r8 * exp_fac(:,:)
      rate(:,:,299) = 4.63e-12_r8 * exp( 350._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( -1900._r8 * itemp(:,:) )
      rate(:,:,300) = 1.40e-12_r8 * exp_fac(:,:)
      rate(:,:,313) = 6.50e-15_r8 * exp_fac(:,:)
      exp_fac(:,:) = exp( 1040._r8 * itemp(:,:) )
      rate(:,:,303) = 4.30e-13_r8 * exp_fac(:,:)
      rate(:,:,353) = 4.30e-13_r8 * exp_fac(:,:)
      rate(:,:,407) = 4.3e-13_r8 * exp_fac(:,:)
      rate(:,:,412) = 4.3e-13_r8 * exp_fac(:,:)
      rate(:,:,430) = 4.3E-13_r8 * exp_fac(:,:)
      rate(:,:,440) = 4.3E-13_r8 * exp_fac(:,:)
      rate(:,:,444) = 4.3E-13_r8 * exp_fac(:,:)
      rate(:,:,446) = 4.3E-13_r8 * exp_fac(:,:)
      exp_fac(:,:) = exp( 500._r8 * itemp(:,:) )
      rate(:,:,304) = 2.00e-12_r8 * exp_fac(:,:)
      rate(:,:,305) = 2.50e-12_r8 * exp_fac(:,:)
      rate(:,:,326) = 7.10e-13_r8 * exp_fac(:,:)
      rate(:,:,354) = 2.00e-12_r8 * exp_fac(:,:)
      rate(:,:,475) = 2e-12_r8 * exp_fac(:,:)
      rate(:,:,482) = 2e-12_r8 * exp_fac(:,:)
      rate(:,:,486) = 2e-12_r8 * exp_fac(:,:)
      rate(:,:,309) = 6.90e-12_r8 * exp( -230._r8 * itemp(:,:) )
      rate(:,:,314) = 4.60e-13_r8 * exp( -1156._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( -40._r8 * itemp(:,:) )
      rate(:,:,317) = 3.75e-13_r8 * exp_fac(:,:)
      rate(:,:,404) = 3.75e-13_r8 * exp_fac(:,:)
      rate(:,:,319) = 8.70e-12_r8 * exp( -615._r8 * itemp(:,:) )
      rate(:,:,329) = 8.40e-13_r8 * exp( 830._r8 * itemp(:,:) )
      rate(:,:,330) = 1.40e-12_r8 * exp( -1860._r8 * itemp(:,:) )
      rate(:,:,333) = 4.8e-12_r8 * exp( 120._r8 * itemp(:,:) )
      rate(:,:,334) = 5.1e-14_r8 * exp( 693._r8 * itemp(:,:) )
      rate(:,:,336) = 4.13e-12_r8 * exp( 452._r8 * itemp(:,:) )
      rate(:,:,337) = 7.52e-16_r8 * exp( -1521._r8 * itemp(:,:) )
      rate(:,:,338) = 2.30e-12_r8 * exp( -170._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 175._r8 * itemp(:,:) )
      rate(:,:,342) = 1.86e-11_r8 * exp_fac(:,:)
      rate(:,:,379) = 1.86e-11_r8 * exp_fac(:,:)
      rate(:,:,388) = 1.86e-11_r8 * exp_fac(:,:)
      rate(:,:,343) = 4.40e-15_r8 * exp( -2500._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 360._r8 * itemp(:,:) )
      rate(:,:,344) = 2.70e-12_r8 * exp_fac(:,:)
      rate(:,:,345) = 1.30e-13_r8 * exp_fac(:,:)
      rate(:,:,351) = 5.30e-12_r8 * exp_fac(:,:)
      rate(:,:,380) = 2.70e-12_r8 * exp_fac(:,:)
      rate(:,:,389) = 2.7e-12_r8 * exp_fac(:,:)
      exp_fac(:,:) = exp( 400._r8 * itemp(:,:) )
      rate(:,:,348) = 5.00e-13_r8 * exp_fac(:,:)
      rate(:,:,374) = 5.00e-13_r8 * exp_fac(:,:)
      rate(:,:,375) = 5.00e-13_r8 * exp_fac(:,:)
      rate(:,:,384) = 5.e-13_r8 * exp_fac(:,:)
      rate(:,:,392) = 5.e-13_r8 * exp_fac(:,:)
      rate(:,:,460) = 4.2e-11_r8 * exp_fac(:,:)
      rate(:,:,493) = 4.2e-11_r8 * exp_fac(:,:)
      exp_fac(:,:) = exp( 530._r8 * itemp(:,:) )
      rate(:,:,355) = 4.60e-12_r8 * exp_fac(:,:)
      rate(:,:,356) = 2.30e-12_r8 * exp_fac(:,:)
      exp_fac(:,:) = exp( 410._r8 * itemp(:,:) )
      rate(:,:,361) = 2.54e-11_r8 * exp_fac(:,:)
      rate(:,:,490) = 2.54e-11_r8 * exp_fac(:,:)
      exp_fac(:,:) = exp( -446._r8 * itemp(:,:) )
      rate(:,:,363) = 3.03e-12_r8 * exp_fac(:,:)
      rate(:,:,507) = 3.03e-12_r8 * exp_fac(:,:)
      rate(:,:,378) = 1.6e9_r8 * exp( -8300._r8 * itemp(:,:) )
      rate(:,:,393) = 1.3e-12_r8 * exp( 640._r8 * itemp(:,:) )
      rate(:,:,394) = 1.90e-12_r8 * exp( 190._r8 * itemp(:,:) )
      rate(:,:,398) = 5.4e-14_r8 * exp( 870._r8 * itemp(:,:) )
      rate(:,:,402) = 8.1e-12_r8 * exp( 610._r8 * itemp(:,:) )
      rate(:,:,411) = 4.6e-14_r8 * exp( -400._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( -193._r8 * itemp(:,:) )
      rate(:,:,415) = 2.3e-12_r8 * exp_fac(:,:)
      rate(:,:,498) = 2.3e-12_r8 * exp_fac(:,:)
      rate(:,:,416) = 4.7e-13_r8 * exp( 1220._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 352._r8 * itemp(:,:) )
      rate(:,:,431) = 1.7e-12_r8 * exp_fac(:,:)
      rate(:,:,499) = 1.7e-12_r8 * exp_fac(:,:)
      rate(:,:,436) = 5.9e-12_r8 * exp( 225._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 440._r8 * itemp(:,:) )
      rate(:,:,458) = 1.2e-11_r8 * exp_fac(:,:)
      rate(:,:,491) = 1.2e-11_r8 * exp_fac(:,:)
      exp_fac(:,:) = exp( 470._r8 * itemp(:,:) )
      rate(:,:,459) = 1.6e-11_r8 * exp_fac(:,:)
      rate(:,:,492) = 1.6e-11_r8 * exp_fac(:,:)
      exp_fac(:,:) = exp( -580._r8 * itemp(:,:) )
      rate(:,:,463) = 6.3e-16_r8 * exp_fac(:,:)
      rate(:,:,502) = 6.3e-16_r8 * exp_fac(:,:)
      rate(:,:,468) = 1.2e-12_r8 * exp( 490._r8 * itemp(:,:) )
      rate(:,:,504) = 3.0e-15_r8 * exp( -7800._r8 * itemp(:,:) )
      rate(:,:,516) = 9.60e-12_r8 * exp( -234._r8 * itemp(:,:) )
      rate(:,:,518) = 1.90e-13_r8 * exp( 520._r8 * itemp(:,:) )

      itemp(:,:) = 300._r8 * itemp(:,:)

      ko(:,:) = 4.40e-32_r8 * itemp(:,:)**1.3_r8
      kinf(:,:) = 7.5e-11_r8 * itemp(:,:)**(-0.2_r8)
      call jpl( rate(1,1,169), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 6.90e-31_r8 * itemp(:,:)**1.0_r8
      kinf(:,:) = 2.60e-11_r8
      call jpl( rate(1,1,178), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 4.28e-33_r8
      kinf(:,:) = 9.30e-15_r8 * itemp(:,:)**(-4.42_r8)
      call jpl( rate(1,1,186), m, 0.8_r8, ko, kinf, n )

      ko(:,:) = 9.00e-32_r8 * itemp(:,:)**1.5_r8
      kinf(:,:) = 3.0e-11_r8
      call jpl( rate(1,1,193), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 2.50e-31_r8 * itemp(:,:)**1.8_r8
      kinf(:,:) = 2.2e-11_r8 * itemp(:,:)**0.7_r8
      call jpl( rate(1,1,197), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 2.00e-30_r8 * itemp(:,:)**4.4_r8
      kinf(:,:) = 1.4e-12_r8 * itemp(:,:)**0.7_r8
      call jpl( rate(1,1,199), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 1.80e-30_r8 * itemp(:,:)**3.0_r8
      kinf(:,:) = 2.8e-11_r8
      call jpl( rate(1,1,201), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 2.00e-31_r8 * itemp(:,:)**3.4_r8
      kinf(:,:) = 2.9e-12_r8 * itemp(:,:)**1.1_r8
      call jpl( rate(1,1,207), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 1.80e-31_r8 * itemp(:,:)**3.4_r8
      kinf(:,:) = 1.5e-11_r8 * itemp(:,:)**1.9_r8
      call jpl( rate(1,1,223), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 1.60e-32_r8 * itemp(:,:)**4.5_r8
      kinf(:,:) = 3.0e-12_r8 * itemp(:,:)**2.0_r8
      call jpl( rate(1,1,227), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 5.20e-31_r8 * itemp(:,:)**3.2_r8
      kinf(:,:) = 6.9e-12_r8 * itemp(:,:)**2.9_r8
      call jpl( rate(1,1,244), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 5.90e-33_r8 * itemp(:,:)**1.4_r8
      kinf(:,:) = 1.10e-12_r8 * itemp(:,:)**(-1.3_r8)
      call jpl( rate(1,1,267), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 5.20e-30_r8 * itemp(:,:)**2.4_r8
      kinf(:,:) = 2.2e-10_r8 * itemp(:,:)**0.7_r8
      call jpl( rate(1,1,282), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 1.60e-29_r8 * itemp(:,:)**3.3_r8
      kinf(:,:) = 3.1e-10_r8 * itemp(:,:)
      call jpl( rate(1,1,283), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 5.50e-30_r8
      kinf(:,:) = 8.3e-13_r8 * itemp(:,:)**(-2.0_r8)
      call jpl( rate(1,1,285), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 8.60e-29_r8 * itemp(:,:)**3.1_r8
      kinf(:,:) = 9.00e-12_r8 * itemp(:,:)**0.85_r8
      call jpl( rate(1,1,287), m, 0.48_r8, ko, kinf, n )

      ko(:,:) = 9.70e-29_r8 * itemp(:,:)**5.6_r8
      kinf(:,:) = 9.30e-12_r8 * itemp(:,:)**1.5_r8
      call jpl( rate(1,1,302), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 8.00e-27_r8 * itemp(:,:)**3.5_r8
      kinf(:,:) = 3.00e-11_r8
      call jpl( rate(1,1,312), m, 0.5_r8, ko, kinf, n )

      ko(:,:) = 8.00e-27_r8 * itemp(:,:)**3.5_r8
      kinf(:,:) = 3.00e-11_r8
      call jpl( rate(1,1,359), m, 0.5_r8, ko, kinf, n )

      ko(:,:) = 9.70e-29_r8 * itemp(:,:)**5.6_r8
      kinf(:,:) = 9.30e-12_r8 * itemp(:,:)**1.5_r8
      call jpl( rate(1,1,428), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 9.70e-29_r8 * itemp(:,:)**5.6_r8
      kinf(:,:) = 9.30e-12_r8 * itemp(:,:)**1.5_r8
      call jpl( rate(1,1,437), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 9.70e-29_r8 * itemp(:,:)**5.6_r8
      kinf(:,:) = 9.30e-12_r8 * itemp(:,:)**1.5_r8
      call jpl( rate(1,1,448), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 9.70e-29_r8 * itemp(:,:)**5.6_r8
      kinf(:,:) = 9.30e-12_r8 * itemp(:,:)**1.5_r8
      call jpl( rate(1,1,449), m, 0.6_r8, ko, kinf, n )

      end subroutine setrxt


      subroutine setrxt_hrates( rate, temp, m, ncol, kbot )

      use ppgrid,       only : pver, pcols
      use shr_kind_mod, only : r8 => shr_kind_r8
      use chem_mods, only : rxntot
      use mo_jpl,    only : jpl

      implicit none

!-------------------------------------------------------
!       ... dummy arguments
!-------------------------------------------------------
      integer, intent(in) :: ncol
      integer, intent(in) :: kbot
      real(r8), intent(in)    :: temp(pcols,pver)
      real(r8), intent(in)    :: m(ncol,pver)
      real(r8), intent(inout) :: rate(ncol,pver,rxntot)

!-------------------------------------------------------
!       ... local variables
!-------------------------------------------------------
      integer  ::  n
      real(r8)  ::  itemp(ncol,kbot)
      real(r8)  ::  exp_fac(ncol,kbot)
      real(r8)  :: ko(ncol,kbot)
      real(r8)  :: kinf(ncol,kbot)
      real(r8)  :: wrk(ncol,kbot)


      end subroutine setrxt_hrates

      end module mo_setrxt
