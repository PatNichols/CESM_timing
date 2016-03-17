
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

      rate(:,:,91) = 1.20e-10_r8
      rate(:,:,92) = 2.02e-10_r8
      rate(:,:,93) = 1.204e-10_r8
      rate(:,:,94) = 1.50e-10_r8
      rate(:,:,95) = 9.75e-11_r8
      rate(:,:,96) = 1.50e-11_r8
      rate(:,:,97) = 7.20e-11_r8
      rate(:,:,98) = 1.794e-10_r8
      rate(:,:,99) = 1.628e-10_r8
      rate(:,:,100) = 2.84e-10_r8
      rate(:,:,101) = 1.674e-10_r8
      rate(:,:,102) = 9.60e-11_r8
      rate(:,:,103) = 4.10e-11_r8
      rate(:,:,104) = 1.012e-10_r8
      rate(:,:,105) = 1.20e-10_r8
      rate(:,:,106) = 4.49e-10_r8
      rate(:,:,107) = 2.57e-10_r8
      rate(:,:,108) = 1.31e-10_r8
      rate(:,:,109) = 3.50e-11_r8
      rate(:,:,110) = 9.00e-12_r8
      rate(:,:,111) = 1.20e-10_r8
      rate(:,:,112) = 1.50e-10_r8
      rate(:,:,113) = 1.20e-10_r8
      rate(:,:,117) = 7.20e-11_r8
      rate(:,:,118) = 6.90e-12_r8
      rate(:,:,119) = 1.60e-12_r8
      rate(:,:,123) = 1.80e-12_r8
      rate(:,:,126) = 1.80e-12_r8
      rate(:,:,150) = 1.00e-11_r8
      rate(:,:,151) = 2.20e-11_r8
      rate(:,:,152) = 3.50e-12_r8
      rate(:,:,177) = 1.70e-13_r8
      rate(:,:,224) = 4.50e-13_r8
      rate(:,:,234) = 1.00e-14_r8
      rate(:,:,237) = 7.00e-13_r8
      rate(:,:,240) = 2.00e-13_r8
      rate(:,:,241) = 6.80e-14_r8
      rate(:,:,250) = 1.00e-12_r8
      rate(:,:,251) = 1.00e-11_r8
      rate(:,:,252) = 1.15e-11_r8
      rate(:,:,255) = 4.00e-14_r8
      rate(:,:,272) = 3.00e-12_r8
      rate(:,:,275) = 6.80e-13_r8
      rate(:,:,276) = 5.40e-11_r8
      rate(:,:,288) = 2.40e-12_r8
      rate(:,:,291) = 1.40e-11_r8
      rate(:,:,294) = 5.00e-12_r8
      rate(:,:,306) = 2.40e-12_r8
      rate(:,:,310) = 1.40e-11_r8
      rate(:,:,312) = 2.40e-12_r8
      rate(:,:,314) = 3.50e-12_r8
      rate(:,:,315) = 4.50e-11_r8
      rate(:,:,322) = 2.40e-12_r8
      rate(:,:,332) = 3.00e-12_r8
      rate(:,:,333) = 1.00e-11_r8
      rate(:,:,340) = 2.10e-6_r8
      itemp(:ncol,:) = 1._r8 / temp(:ncol,:)
      n = ncol*pver
      rate(:,:,84) = 8.00e-12_r8 * exp( -2060._r8 * itemp(:,:) )
      rate(:,:,86) = 2.15e-11_r8 * exp( 110._r8 * itemp(:,:) )
      rate(:,:,87) = 3.30e-11_r8 * exp( 55._r8 * itemp(:,:) )
      rate(:,:,88) = 1.63e-10_r8 * exp( 60._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 20._r8 * itemp(:,:) )
      rate(:,:,89) = 7.25e-11_r8 * exp_fac(:,:)
      rate(:,:,90) = 4.63e-11_r8 * exp_fac(:,:)
      exp_fac(:,:) = exp( 100._r8 * itemp(:,:) )
      rate(:,:,114) = 7.70e-11_r8 * exp_fac(:,:)
      rate(:,:,135) = 2.10e-11_r8 * exp_fac(:,:)
      rate(:,:,116) = 1.40e-10_r8 * exp( -470._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 180._r8 * itemp(:,:) )
      rate(:,:,120) = 1.80e-11_r8 * exp_fac(:,:)
      rate(:,:,232) = 4.20e-12_r8 * exp_fac(:,:)
      rate(:,:,259) = 4.20e-12_r8 * exp_fac(:,:)
      rate(:,:,264) = 4.20e-12_r8 * exp_fac(:,:)
      rate(:,:,277) = 4.20e-12_r8 * exp_fac(:,:)
      rate(:,:,281) = 4.20e-12_r8 * exp_fac(:,:)
      rate(:,:,305) = 4.40e-12_r8 * exp_fac(:,:)
      rate(:,:,318) = 4.20e-12_r8 * exp_fac(:,:)
      rate(:,:,329) = 4.20e-12_r8 * exp_fac(:,:)
      rate(:,:,337) = 4.2e-12_r8 * exp_fac(:,:)
      rate(:,:,121) = 1.70e-12_r8 * exp( -940._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 250._r8 * itemp(:,:) )
      rate(:,:,122) = 4.80e-11_r8 * exp_fac(:,:)
      rate(:,:,187) = 1.70e-11_r8 * exp_fac(:,:)
      rate(:,:,125) = 2.80e-12_r8 * exp( -1800._r8 * itemp(:,:) )
      rate(:,:,127) = 1.60e-11_r8 * exp( -4570._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 200._r8 * itemp(:,:) )
      rate(:,:,128) = 3.00e-11_r8 * exp_fac(:,:)
      rate(:,:,195) = 5.50e-12_r8 * exp_fac(:,:)
      rate(:,:,223) = 3.80e-12_r8 * exp_fac(:,:)
      rate(:,:,242) = 3.80e-12_r8 * exp_fac(:,:)
      rate(:,:,262) = 3.80e-12_r8 * exp_fac(:,:)
      rate(:,:,266) = 3.80e-12_r8 * exp_fac(:,:)
      rate(:,:,271) = 3.80e-12_r8 * exp_fac(:,:)
      rate(:,:,283) = 3.80e-12_r8 * exp_fac(:,:)
      rate(:,:,292) = 2.30e-11_r8 * exp_fac(:,:)
      rate(:,:,308) = 1.52e-11_r8 * exp_fac(:,:)
      rate(:,:,320) = 3.80e-12_r8 * exp_fac(:,:)
      rate(:,:,331) = 3.80e-12_r8 * exp_fac(:,:)
      rate(:,:,339) = 3.8e-12_r8 * exp_fac(:,:)
      rate(:,:,129) = 1.00e-14_r8 * exp( -490._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( -2000._r8 * itemp(:,:) )
      rate(:,:,131) = 1.40e-12_r8 * exp_fac(:,:)
      rate(:,:,303) = 1.05e-14_r8 * exp_fac(:,:)
      rate(:,:,133) = 7.80e-13_r8 * exp( -1050._r8 * itemp(:,:) )
      rate(:,:,134) = 1.50e-11_r8 * exp( -3600._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 220._r8 * itemp(:,:) )
      rate(:,:,136) = 2.90e-12_r8 * exp_fac(:,:)
      rate(:,:,137) = 1.45e-12_r8 * exp_fac(:,:)
      rate(:,:,138) = 1.45e-12_r8 * exp_fac(:,:)
      exp_fac(:,:) = exp( 270._r8 * itemp(:,:) )
      rate(:,:,140) = 3.30e-12_r8 * exp_fac(:,:)
      rate(:,:,159) = 1.40e-11_r8 * exp_fac(:,:)
      rate(:,:,164) = 7.40e-12_r8 * exp_fac(:,:)
      rate(:,:,245) = 8.10e-12_r8 * exp_fac(:,:)
      exp_fac(:,:) = exp( -1500._r8 * itemp(:,:) )
      rate(:,:,141) = 3.00e-12_r8 * exp_fac(:,:)
      rate(:,:,196) = 5.80e-12_r8 * exp_fac(:,:)
      rate(:,:,142) = 5.10e-12_r8 * exp( 210._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( -2450._r8 * itemp(:,:) )
      rate(:,:,144) = 1.20e-13_r8 * exp_fac(:,:)
      rate(:,:,170) = 3.00e-11_r8 * exp_fac(:,:)
      rate(:,:,149) = 1.50e-11_r8 * exp( 170._r8 * itemp(:,:) )
      rate(:,:,154) = 1.30e-12_r8 * exp( 380._r8 * itemp(:,:) )
      rate(:,:,156) = 2.30e-11_r8 * exp( -200._r8 * itemp(:,:) )
      rate(:,:,157) = 3.05e-11_r8 * exp( -2270._r8 * itemp(:,:) )
      rate(:,:,158) = 1.10e-11_r8 * exp( -980._r8 * itemp(:,:) )
      rate(:,:,160) = 3.60e-11_r8 * exp( -375._r8 * itemp(:,:) )
      rate(:,:,161) = 8.10e-11_r8 * exp( -30._r8 * itemp(:,:) )
      rate(:,:,162) = 7.30e-12_r8 * exp( -1280._r8 * itemp(:,:) )
      rate(:,:,163) = 2.80e-11_r8 * exp( 85._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 230._r8 * itemp(:,:) )
      rate(:,:,165) = 6.00e-13_r8 * exp_fac(:,:)
      rate(:,:,186) = 1.90e-11_r8 * exp_fac(:,:)
      rate(:,:,194) = 1.50e-12_r8 * exp_fac(:,:)
      exp_fac(:,:) = exp( 290._r8 * itemp(:,:) )
      rate(:,:,166) = 2.60e-12_r8 * exp_fac(:,:)
      rate(:,:,168) = 6.40e-12_r8 * exp_fac(:,:)
      rate(:,:,193) = 4.10e-13_r8 * exp_fac(:,:)
      rate(:,:,167) = 3.3e-12_r8 * exp( -115._r8 * itemp(:,:) )
      rate(:,:,171) = 1.00e-12_r8 * exp( -1590._r8 * itemp(:,:) )
      rate(:,:,172) = 3.50e-13_r8 * exp( -1370._r8 * itemp(:,:) )
      rate(:,:,175) = 1.80e-12_r8 * exp( -250._r8 * itemp(:,:) )
      rate(:,:,176) = 1.00e-11_r8 * exp( -3300._r8 * itemp(:,:) )
      rate(:,:,178) = 3.40e-12_r8 * exp( -130._r8 * itemp(:,:) )
      rate(:,:,179) = 3.00e-12_r8 * exp( -500._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( -840._r8 * itemp(:,:) )
      rate(:,:,180) = 3.60e-12_r8 * exp_fac(:,:)
      rate(:,:,207) = 2.00e-12_r8 * exp_fac(:,:)
      rate(:,:,181) = 1.20e-12_r8 * exp( -330._r8 * itemp(:,:) )
      rate(:,:,182) = 6.50e-12_r8 * exp( 135._r8 * itemp(:,:) )
      rate(:,:,183) = 1.60e-11_r8 * exp( -780._r8 * itemp(:,:) )
      rate(:,:,184) = 4.80e-12_r8 * exp( -310._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( -800._r8 * itemp(:,:) )
      rate(:,:,185) = 1.70e-11_r8 * exp_fac(:,:)
      rate(:,:,209) = 6.30e-12_r8 * exp_fac(:,:)
      rate(:,:,188) = 4.50e-12_r8 * exp( 460._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 260._r8 * itemp(:,:) )
      rate(:,:,189) = 8.80e-12_r8 * exp_fac(:,:)
      rate(:,:,192) = 2.30e-12_r8 * exp_fac(:,:)
      rate(:,:,191) = 9.50e-13_r8 * exp( 550._r8 * itemp(:,:) )
      rate(:,:,197) = 1.20e-10_r8 * exp( -430._r8 * itemp(:,:) )
      rate(:,:,198) = 1.90e-11_r8 * exp( 215._r8 * itemp(:,:) )
      rate(:,:,199) = 2.17e-11_r8 * exp( -1130._r8 * itemp(:,:) )
      rate(:,:,200) = 2.40e-12_r8 * exp( -1250._r8 * itemp(:,:) )
      rate(:,:,201) = 1.64e-12_r8 * exp( -1520._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( -1600._r8 * itemp(:,:) )
      rate(:,:,202) = 1.05e-12_r8 * exp_fac(:,:)
      rate(:,:,205) = 1.25e-12_r8 * exp_fac(:,:)
      rate(:,:,216) = 3.40e-11_r8 * exp_fac(:,:)
      rate(:,:,203) = 2.35e-12_r8 * exp( -1300._r8 * itemp(:,:) )
      rate(:,:,204) = 1.40e-11_r8 * exp( -1030._r8 * itemp(:,:) )
      rate(:,:,206) = 1.30e-12_r8 * exp( -1770._r8 * itemp(:,:) )
      rate(:,:,208) = 1.35e-12_r8 * exp( -600._r8 * itemp(:,:) )
      rate(:,:,210) = 4.85e-12_r8 * exp( -850._r8 * itemp(:,:) )
      rate(:,:,211) = 2.45e-12_r8 * exp( -1775._r8 * itemp(:,:) )
      rate(:,:,214) = 6.00e-13_r8 * exp( -2058._r8 * itemp(:,:) )
      rate(:,:,215) = 5.50e-12_r8 * exp( 125._r8 * itemp(:,:) )
      rate(:,:,217) = 9.7e-15_r8 * exp( 625._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 300._r8 * itemp(:,:) )
      rate(:,:,218) = 2.80e-12_r8 * exp_fac(:,:)
      rate(:,:,268) = 2.90e-12_r8 * exp_fac(:,:)
      rate(:,:,219) = 4.10e-13_r8 * exp( 750._r8 * itemp(:,:) )
      rate(:,:,220) = 5.00e-13_r8 * exp( -424._r8 * itemp(:,:) )
      rate(:,:,221) = 1.90e-14_r8 * exp( 706._r8 * itemp(:,:) )
      rate(:,:,222) = 2.90e-12_r8 * exp( -345._r8 * itemp(:,:) )
      rate(:,:,225) = 2.40e12_r8 * exp( -7000._r8 * itemp(:,:) )
      rate(:,:,226) = 2.60e-12_r8 * exp( 265._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 700._r8 * itemp(:,:) )
      rate(:,:,227) = 7.50e-13_r8 * exp_fac(:,:)
      rate(:,:,233) = 7.50e-13_r8 * exp_fac(:,:)
      rate(:,:,239) = 7.50e-13_r8 * exp_fac(:,:)
      rate(:,:,260) = 7.50e-13_r8 * exp_fac(:,:)
      rate(:,:,265) = 7.50e-13_r8 * exp_fac(:,:)
      rate(:,:,269) = 8.60e-13_r8 * exp_fac(:,:)
      rate(:,:,282) = 7.50e-13_r8 * exp_fac(:,:)
      rate(:,:,289) = 8.00e-13_r8 * exp_fac(:,:)
      rate(:,:,307) = 8.00e-13_r8 * exp_fac(:,:)
      rate(:,:,313) = 8.00e-13_r8 * exp_fac(:,:)
      rate(:,:,319) = 7.50e-13_r8 * exp_fac(:,:)
      rate(:,:,323) = 8.00e-13_r8 * exp_fac(:,:)
      rate(:,:,330) = 7.50e-13_r8 * exp_fac(:,:)
      rate(:,:,338) = 7.5e-13_r8 * exp_fac(:,:)
      rate(:,:,228) = 7.20e-11_r8 * exp( -70._r8 * itemp(:,:) )
      rate(:,:,230) = 7.66e-12_r8 * exp( -1020._r8 * itemp(:,:) )
      rate(:,:,235) = 1.60e11_r8 * exp( -4150._r8 * itemp(:,:) )
      rate(:,:,236) = 1.2e-14_r8 * exp( -2630._r8 * itemp(:,:) )
      rate(:,:,238) = 2.60e-12_r8 * exp( 365._r8 * itemp(:,:) )
      rate(:,:,243) = 4.63e-12_r8 * exp( 350._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( -1900._r8 * itemp(:,:) )
      rate(:,:,244) = 1.40e-12_r8 * exp_fac(:,:)
      rate(:,:,257) = 6.50e-15_r8 * exp_fac(:,:)
      exp_fac(:,:) = exp( 1040._r8 * itemp(:,:) )
      rate(:,:,247) = 4.30e-13_r8 * exp_fac(:,:)
      rate(:,:,295) = 4.30e-13_r8 * exp_fac(:,:)
      exp_fac(:,:) = exp( 500._r8 * itemp(:,:) )
      rate(:,:,248) = 2.00e-12_r8 * exp_fac(:,:)
      rate(:,:,249) = 2.50e-12_r8 * exp_fac(:,:)
      rate(:,:,270) = 7.10e-13_r8 * exp_fac(:,:)
      rate(:,:,296) = 2.00e-12_r8 * exp_fac(:,:)
      rate(:,:,253) = 6.90e-12_r8 * exp( -230._r8 * itemp(:,:) )
      rate(:,:,258) = 4.60e-13_r8 * exp( -1156._r8 * itemp(:,:) )
      rate(:,:,261) = 3.75e-13_r8 * exp( -40._r8 * itemp(:,:) )
      rate(:,:,263) = 8.70e-12_r8 * exp( -615._r8 * itemp(:,:) )
      rate(:,:,273) = 8.40e-13_r8 * exp( 830._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( -1860._r8 * itemp(:,:) )
      rate(:,:,274) = 1.40e-12_r8 * exp_fac(:,:)
      rate(:,:,316) = 1.40e-12_r8 * exp_fac(:,:)
      rate(:,:,278) = 4.13e-12_r8 * exp( 452._r8 * itemp(:,:) )
      rate(:,:,279) = 7.52e-16_r8 * exp( -1521._r8 * itemp(:,:) )
      rate(:,:,280) = 2.30e-12_r8 * exp( -170._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 175._r8 * itemp(:,:) )
      rate(:,:,284) = 1.86e-11_r8 * exp_fac(:,:)
      rate(:,:,317) = 1.86e-11_r8 * exp_fac(:,:)
      rate(:,:,285) = 4.40e-15_r8 * exp( -2500._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 360._r8 * itemp(:,:) )
      rate(:,:,286) = 2.70e-12_r8 * exp_fac(:,:)
      rate(:,:,287) = 1.30e-13_r8 * exp_fac(:,:)
      rate(:,:,293) = 5.30e-12_r8 * exp_fac(:,:)
      rate(:,:,311) = 2.70e-12_r8 * exp_fac(:,:)
      rate(:,:,321) = 2.7e-12_r8 * exp_fac(:,:)
      exp_fac(:,:) = exp( 400._r8 * itemp(:,:) )
      rate(:,:,290) = 5.00e-13_r8 * exp_fac(:,:)
      rate(:,:,309) = 5.00e-13_r8 * exp_fac(:,:)
      rate(:,:,324) = 5.e-13_r8 * exp_fac(:,:)
      exp_fac(:,:) = exp( 530._r8 * itemp(:,:) )
      rate(:,:,297) = 4.60e-12_r8 * exp_fac(:,:)
      rate(:,:,298) = 2.30e-12_r8 * exp_fac(:,:)
      rate(:,:,302) = 2.54e-11_r8 * exp( 410._r8 * itemp(:,:) )
      rate(:,:,304) = 3.03e-12_r8 * exp( -446._r8 * itemp(:,:) )
      rate(:,:,325) = 1.3e-12_r8 * exp( 640._r8 * itemp(:,:) )
      rate(:,:,326) = 1.90e-12_r8 * exp( 190._r8 * itemp(:,:) )
      rate(:,:,328) = 1.70e-12_r8 * exp( 352._r8 * itemp(:,:) )
      rate(:,:,334) = 1.2e-11_r8 * exp( 444._r8 * itemp(:,:) )
      rate(:,:,335) = 1.e-15_r8 * exp( -732._r8 * itemp(:,:) )
      rate(:,:,336) = 1.2e-12_r8 * exp( 490._r8 * itemp(:,:) )
      rate(:,:,345) = 9.60e-12_r8 * exp( -234._r8 * itemp(:,:) )
      rate(:,:,347) = 1.90e-13_r8 * exp( 520._r8 * itemp(:,:) )
      rate(:,:,348) = 1.70e-12_r8 * exp( -710._r8 * itemp(:,:) )
      rate(:,:,350) = 2.10E-11_r8 * exp( -2200.0_r8 * itemp(:,:) )
      rate(:,:,351) = 1.10E-13_r8 * exp( -1200.0_r8 * itemp(:,:) )

      itemp(:,:) = 300._r8 * itemp(:,:)

      ko(:,:) = 4.40e-32_r8 * itemp(:,:)**1.3_r8
      kinf(:,:) = 7.5e-11_r8 * itemp(:,:)**(-0.2_r8)
      call jpl( rate(1,1,115), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 6.90e-31_r8 * itemp(:,:)**1.0_r8
      kinf(:,:) = 2.60e-11_r8
      call jpl( rate(1,1,124), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 4.28e-33_r8
      kinf(:,:) = 9.30e-15_r8 * itemp(:,:)**(-4.42_r8)
      call jpl( rate(1,1,132), m, 0.8_r8, ko, kinf, n )

      ko(:,:) = 9.00e-32_r8 * itemp(:,:)**1.5_r8
      kinf(:,:) = 3.0e-11_r8
      call jpl( rate(1,1,139), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 2.50e-31_r8 * itemp(:,:)**1.8_r8
      kinf(:,:) = 2.2e-11_r8 * itemp(:,:)**0.7_r8
      call jpl( rate(1,1,143), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 2.00e-30_r8 * itemp(:,:)**4.4_r8
      kinf(:,:) = 1.4e-12_r8 * itemp(:,:)**0.7_r8
      call jpl( rate(1,1,145), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 1.80e-30_r8 * itemp(:,:)**3.0_r8
      kinf(:,:) = 2.8e-11_r8
      call jpl( rate(1,1,147), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 2.00e-31_r8 * itemp(:,:)**3.4_r8
      kinf(:,:) = 2.9e-12_r8 * itemp(:,:)**1.1_r8
      call jpl( rate(1,1,153), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 1.80e-31_r8 * itemp(:,:)**3.4_r8
      kinf(:,:) = 1.5e-11_r8 * itemp(:,:)**1.9_r8
      call jpl( rate(1,1,169), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 1.60e-32_r8 * itemp(:,:)**4.5_r8
      kinf(:,:) = 3.0e-12_r8 * itemp(:,:)**2.0_r8
      call jpl( rate(1,1,173), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 5.20e-31_r8 * itemp(:,:)**3.2_r8
      kinf(:,:) = 6.9e-12_r8 * itemp(:,:)**2.9_r8
      call jpl( rate(1,1,190), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 5.90e-33_r8 * itemp(:,:)**1.4_r8
      kinf(:,:) = 1.10e-12_r8 * itemp(:,:)**(-1.3_r8)
      call jpl( rate(1,1,213), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 5.50e-30_r8
      kinf(:,:) = 8.3e-13_r8 * itemp(:,:)**(-2.0_r8)
      call jpl( rate(1,1,229), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 8.60e-29_r8 * itemp(:,:)**3.1_r8
      kinf(:,:) = 9.00e-12_r8 * itemp(:,:)**0.85_r8
      call jpl( rate(1,1,231), m, 0.48_r8, ko, kinf, n )

      ko(:,:) = 9.70e-29_r8 * itemp(:,:)**5.6_r8
      kinf(:,:) = 9.30e-12_r8 * itemp(:,:)**1.5_r8
      call jpl( rate(1,1,246), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 8.00e-27_r8 * itemp(:,:)**3.5_r8
      kinf(:,:) = 3.00e-11_r8
      call jpl( rate(1,1,256), m, 0.5_r8, ko, kinf, n )

      ko(:,:) = 8.00e-27_r8 * itemp(:,:)**3.5_r8
      kinf(:,:) = 3.00e-11_r8
      call jpl( rate(1,1,301), m, 0.5_r8, ko, kinf, n )

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
