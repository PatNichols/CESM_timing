
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

      rate(:,:,93) = 1.20e-10_r8
      rate(:,:,94) = 2.02e-10_r8
      rate(:,:,95) = 1.204e-10_r8
      rate(:,:,96) = 1.50e-10_r8
      rate(:,:,97) = 9.75e-11_r8
      rate(:,:,98) = 1.50e-11_r8
      rate(:,:,99) = 7.20e-11_r8
      rate(:,:,100) = 1.794e-10_r8
      rate(:,:,101) = 1.628e-10_r8
      rate(:,:,102) = 2.84e-10_r8
      rate(:,:,103) = 1.674e-10_r8
      rate(:,:,104) = 9.60e-11_r8
      rate(:,:,105) = 4.10e-11_r8
      rate(:,:,106) = 1.012e-10_r8
      rate(:,:,107) = 1.20e-10_r8
      rate(:,:,108) = 4.49e-10_r8
      rate(:,:,109) = 2.57e-10_r8
      rate(:,:,110) = 1.31e-10_r8
      rate(:,:,111) = 3.50e-11_r8
      rate(:,:,112) = 9.00e-12_r8
      rate(:,:,113) = 1.20e-10_r8
      rate(:,:,114) = 1.50e-10_r8
      rate(:,:,115) = 1.20e-10_r8
      rate(:,:,119) = 7.20e-11_r8
      rate(:,:,120) = 6.90e-12_r8
      rate(:,:,121) = 1.60e-12_r8
      rate(:,:,125) = 1.80e-12_r8
      rate(:,:,128) = 1.80e-12_r8
      rate(:,:,152) = 1.00e-11_r8
      rate(:,:,153) = 2.20e-11_r8
      rate(:,:,154) = 3.50e-12_r8
      rate(:,:,179) = 1.70e-13_r8
      rate(:,:,226) = 4.50e-13_r8
      rate(:,:,238) = 1.00e-14_r8
      rate(:,:,241) = 7.00e-13_r8
      rate(:,:,244) = 2.00e-13_r8
      rate(:,:,245) = 6.80e-14_r8
      rate(:,:,254) = 1.00e-12_r8
      rate(:,:,255) = 1.00e-11_r8
      rate(:,:,256) = 1.15e-11_r8
      rate(:,:,259) = 4.00e-14_r8
      rate(:,:,276) = 3.00e-12_r8
      rate(:,:,279) = 6.80e-13_r8
      rate(:,:,280) = 5.40e-11_r8
      rate(:,:,292) = 2.40e-12_r8
      rate(:,:,295) = 1.40e-11_r8
      rate(:,:,298) = 5.00e-12_r8
      rate(:,:,310) = 2.40e-12_r8
      rate(:,:,314) = 1.40e-11_r8
      rate(:,:,316) = 2.40e-12_r8
      rate(:,:,318) = 3.50e-12_r8
      rate(:,:,319) = 4.50e-11_r8
      rate(:,:,326) = 2.40e-12_r8
      rate(:,:,336) = 3.00e-12_r8
      rate(:,:,337) = 1.00e-11_r8
      rate(:,:,341) = 2.3e-11_r8
      rate(:,:,353) = 7.10e-6_r8
      rate(:,:,359) = 7.10e-6_r8
      rate(:,:,363) = 6.34e-8_r8
      rate(:,:,364) = 6.34e-8_r8
      rate(:,:,365) = 6.34e-8_r8
      rate(:,:,366) = 6.34e-8_r8
      rate(:,:,367) = 6.34e-8_r8
      rate(:,:,368) = 6.34e-8_r8
      rate(:,:,369) = 6.34e-8_r8
      rate(:,:,370) = 6.34e-8_r8
      rate(:,:,371) = 6.34e-8_r8
      rate(:,:,372) = 6.34e-8_r8
      rate(:,:,373) = 6.34e-8_r8
      rate(:,:,374) = 6.34e-8_r8
      rate(:,:,375) = 6.34e-8_r8
      rate(:,:,376) = 6.34e-8_r8
      rate(:,:,377) = 6.34e-8_r8
      rate(:,:,378) = 6.34e-8_r8
      rate(:,:,379) = 6.34e-8_r8
      rate(:,:,380) = 6.34e-8_r8
      rate(:,:,381) = 6.34e-8_r8
      rate(:,:,382) = 6.34e-8_r8
      rate(:,:,383) = 6.34e-8_r8
      rate(:,:,401) = 2.31e-06_r8
      rate(:,:,402) = 2.31e-07_r8
      rate(:,:,403) = 2.31e-07_r8
      rate(:,:,404) = 4.63e-07_r8
      rate(:,:,405) = 4.63e-07_r8
      rate(:,:,406) = 2.31e-07_r8
      rate(:,:,407) = 1.29e-07_r8
      rate(:,:,408) = 1.29e-07_r8
      rate(:,:,409) = 1.29e-07_r8
      itemp(:ncol,:) = 1._r8 / temp(:ncol,:)
      n = ncol*pver
      rate(:,:,86) = 8.00e-12_r8 * exp( -2060._r8 * itemp(:,:) )
      rate(:,:,88) = 2.15e-11_r8 * exp( 110._r8 * itemp(:,:) )
      rate(:,:,89) = 3.30e-11_r8 * exp( 55._r8 * itemp(:,:) )
      rate(:,:,90) = 1.63e-10_r8 * exp( 60._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 20._r8 * itemp(:,:) )
      rate(:,:,91) = 7.25e-11_r8 * exp_fac(:,:)
      rate(:,:,92) = 4.63e-11_r8 * exp_fac(:,:)
      exp_fac(:,:) = exp( 100._r8 * itemp(:,:) )
      rate(:,:,116) = 7.70e-11_r8 * exp_fac(:,:)
      rate(:,:,137) = 2.10e-11_r8 * exp_fac(:,:)
      rate(:,:,118) = 1.40e-10_r8 * exp( -470._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 180._r8 * itemp(:,:) )
      rate(:,:,122) = 1.80e-11_r8 * exp_fac(:,:)
      rate(:,:,236) = 4.20e-12_r8 * exp_fac(:,:)
      rate(:,:,263) = 4.20e-12_r8 * exp_fac(:,:)
      rate(:,:,268) = 4.20e-12_r8 * exp_fac(:,:)
      rate(:,:,281) = 4.20e-12_r8 * exp_fac(:,:)
      rate(:,:,285) = 4.20e-12_r8 * exp_fac(:,:)
      rate(:,:,309) = 4.40e-12_r8 * exp_fac(:,:)
      rate(:,:,322) = 4.20e-12_r8 * exp_fac(:,:)
      rate(:,:,333) = 4.20e-12_r8 * exp_fac(:,:)
      rate(:,:,347) = 4.2e-12_r8 * exp_fac(:,:)
      rate(:,:,123) = 1.70e-12_r8 * exp( -940._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 250._r8 * itemp(:,:) )
      rate(:,:,124) = 4.80e-11_r8 * exp_fac(:,:)
      rate(:,:,189) = 1.70e-11_r8 * exp_fac(:,:)
      rate(:,:,127) = 2.80e-12_r8 * exp( -1800._r8 * itemp(:,:) )
      rate(:,:,129) = 1.60e-11_r8 * exp( -4570._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 200._r8 * itemp(:,:) )
      rate(:,:,130) = 3.00e-11_r8 * exp_fac(:,:)
      rate(:,:,197) = 5.50e-12_r8 * exp_fac(:,:)
      rate(:,:,225) = 3.80e-12_r8 * exp_fac(:,:)
      rate(:,:,246) = 3.80e-12_r8 * exp_fac(:,:)
      rate(:,:,266) = 3.80e-12_r8 * exp_fac(:,:)
      rate(:,:,270) = 3.80e-12_r8 * exp_fac(:,:)
      rate(:,:,275) = 3.80e-12_r8 * exp_fac(:,:)
      rate(:,:,287) = 3.80e-12_r8 * exp_fac(:,:)
      rate(:,:,296) = 2.30e-11_r8 * exp_fac(:,:)
      rate(:,:,312) = 1.52e-11_r8 * exp_fac(:,:)
      rate(:,:,324) = 3.80e-12_r8 * exp_fac(:,:)
      rate(:,:,335) = 3.80e-12_r8 * exp_fac(:,:)
      rate(:,:,349) = 3.8e-12_r8 * exp_fac(:,:)
      rate(:,:,131) = 1.00e-14_r8 * exp( -490._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( -2000._r8 * itemp(:,:) )
      rate(:,:,133) = 1.40e-12_r8 * exp_fac(:,:)
      rate(:,:,307) = 1.05e-14_r8 * exp_fac(:,:)
      rate(:,:,135) = 7.80e-13_r8 * exp( -1050._r8 * itemp(:,:) )
      rate(:,:,136) = 1.50e-11_r8 * exp( -3600._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 220._r8 * itemp(:,:) )
      rate(:,:,138) = 2.90e-12_r8 * exp_fac(:,:)
      rate(:,:,139) = 1.45e-12_r8 * exp_fac(:,:)
      rate(:,:,140) = 1.45e-12_r8 * exp_fac(:,:)
      exp_fac(:,:) = exp( 270._r8 * itemp(:,:) )
      rate(:,:,142) = 3.30e-12_r8 * exp_fac(:,:)
      rate(:,:,161) = 1.40e-11_r8 * exp_fac(:,:)
      rate(:,:,166) = 7.40e-12_r8 * exp_fac(:,:)
      rate(:,:,249) = 8.10e-12_r8 * exp_fac(:,:)
      exp_fac(:,:) = exp( -1500._r8 * itemp(:,:) )
      rate(:,:,143) = 3.00e-12_r8 * exp_fac(:,:)
      rate(:,:,198) = 5.80e-12_r8 * exp_fac(:,:)
      rate(:,:,144) = 5.10e-12_r8 * exp( 210._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( -2450._r8 * itemp(:,:) )
      rate(:,:,146) = 1.20e-13_r8 * exp_fac(:,:)
      rate(:,:,172) = 3.00e-11_r8 * exp_fac(:,:)
      rate(:,:,151) = 1.50e-11_r8 * exp( 170._r8 * itemp(:,:) )
      rate(:,:,156) = 1.30e-12_r8 * exp( 380._r8 * itemp(:,:) )
      rate(:,:,158) = 2.30e-11_r8 * exp( -200._r8 * itemp(:,:) )
      rate(:,:,159) = 3.05e-11_r8 * exp( -2270._r8 * itemp(:,:) )
      rate(:,:,160) = 1.10e-11_r8 * exp( -980._r8 * itemp(:,:) )
      rate(:,:,162) = 3.60e-11_r8 * exp( -375._r8 * itemp(:,:) )
      rate(:,:,163) = 8.10e-11_r8 * exp( -30._r8 * itemp(:,:) )
      rate(:,:,164) = 7.30e-12_r8 * exp( -1280._r8 * itemp(:,:) )
      rate(:,:,165) = 2.80e-11_r8 * exp( 85._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 230._r8 * itemp(:,:) )
      rate(:,:,167) = 6.00e-13_r8 * exp_fac(:,:)
      rate(:,:,188) = 1.90e-11_r8 * exp_fac(:,:)
      rate(:,:,196) = 1.50e-12_r8 * exp_fac(:,:)
      exp_fac(:,:) = exp( 290._r8 * itemp(:,:) )
      rate(:,:,168) = 2.60e-12_r8 * exp_fac(:,:)
      rate(:,:,170) = 6.40e-12_r8 * exp_fac(:,:)
      rate(:,:,195) = 4.10e-13_r8 * exp_fac(:,:)
      rate(:,:,169) = 3.3e-12_r8 * exp( -115._r8 * itemp(:,:) )
      rate(:,:,173) = 1.00e-12_r8 * exp( -1590._r8 * itemp(:,:) )
      rate(:,:,174) = 3.50e-13_r8 * exp( -1370._r8 * itemp(:,:) )
      rate(:,:,177) = 1.80e-12_r8 * exp( -250._r8 * itemp(:,:) )
      rate(:,:,178) = 1.00e-11_r8 * exp( -3300._r8 * itemp(:,:) )
      rate(:,:,180) = 3.40e-12_r8 * exp( -130._r8 * itemp(:,:) )
      rate(:,:,181) = 3.00e-12_r8 * exp( -500._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( -840._r8 * itemp(:,:) )
      rate(:,:,182) = 3.60e-12_r8 * exp_fac(:,:)
      rate(:,:,209) = 2.00e-12_r8 * exp_fac(:,:)
      rate(:,:,183) = 1.20e-12_r8 * exp( -330._r8 * itemp(:,:) )
      rate(:,:,184) = 6.50e-12_r8 * exp( 135._r8 * itemp(:,:) )
      rate(:,:,185) = 1.60e-11_r8 * exp( -780._r8 * itemp(:,:) )
      rate(:,:,186) = 4.80e-12_r8 * exp( -310._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( -800._r8 * itemp(:,:) )
      rate(:,:,187) = 1.70e-11_r8 * exp_fac(:,:)
      rate(:,:,211) = 6.30e-12_r8 * exp_fac(:,:)
      rate(:,:,190) = 4.50e-12_r8 * exp( 460._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 260._r8 * itemp(:,:) )
      rate(:,:,191) = 8.80e-12_r8 * exp_fac(:,:)
      rate(:,:,194) = 2.30e-12_r8 * exp_fac(:,:)
      rate(:,:,193) = 9.50e-13_r8 * exp( 550._r8 * itemp(:,:) )
      rate(:,:,199) = 1.20e-10_r8 * exp( -430._r8 * itemp(:,:) )
      rate(:,:,200) = 1.90e-11_r8 * exp( 215._r8 * itemp(:,:) )
      rate(:,:,201) = 2.17e-11_r8 * exp( -1130._r8 * itemp(:,:) )
      rate(:,:,202) = 2.40e-12_r8 * exp( -1250._r8 * itemp(:,:) )
      rate(:,:,203) = 1.64e-12_r8 * exp( -1520._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( -1600._r8 * itemp(:,:) )
      rate(:,:,204) = 1.05e-12_r8 * exp_fac(:,:)
      rate(:,:,207) = 1.25e-12_r8 * exp_fac(:,:)
      rate(:,:,218) = 3.40e-11_r8 * exp_fac(:,:)
      rate(:,:,205) = 2.35e-12_r8 * exp( -1300._r8 * itemp(:,:) )
      rate(:,:,206) = 1.40e-11_r8 * exp( -1030._r8 * itemp(:,:) )
      rate(:,:,208) = 1.30e-12_r8 * exp( -1770._r8 * itemp(:,:) )
      rate(:,:,210) = 1.35e-12_r8 * exp( -600._r8 * itemp(:,:) )
      rate(:,:,212) = 4.85e-12_r8 * exp( -850._r8 * itemp(:,:) )
      rate(:,:,213) = 2.45e-12_r8 * exp( -1775._r8 * itemp(:,:) )
      rate(:,:,216) = 6.00e-13_r8 * exp( -2058._r8 * itemp(:,:) )
      rate(:,:,217) = 5.50e-12_r8 * exp( 125._r8 * itemp(:,:) )
      rate(:,:,219) = 9.7e-15_r8 * exp( 625._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 300._r8 * itemp(:,:) )
      rate(:,:,220) = 2.80e-12_r8 * exp_fac(:,:)
      rate(:,:,272) = 2.90e-12_r8 * exp_fac(:,:)
      rate(:,:,221) = 4.10e-13_r8 * exp( 750._r8 * itemp(:,:) )
      rate(:,:,222) = 5.00e-13_r8 * exp( -424._r8 * itemp(:,:) )
      rate(:,:,223) = 1.90e-14_r8 * exp( 706._r8 * itemp(:,:) )
      rate(:,:,224) = 2.90e-12_r8 * exp( -345._r8 * itemp(:,:) )
      rate(:,:,227) = 2.40e12_r8 * exp( -7000._r8 * itemp(:,:) )
      rate(:,:,228) = 2.60e-12_r8 * exp( 265._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 700._r8 * itemp(:,:) )
      rate(:,:,229) = 7.50e-13_r8 * exp_fac(:,:)
      rate(:,:,237) = 7.50e-13_r8 * exp_fac(:,:)
      rate(:,:,243) = 7.50e-13_r8 * exp_fac(:,:)
      rate(:,:,264) = 7.50e-13_r8 * exp_fac(:,:)
      rate(:,:,269) = 7.50e-13_r8 * exp_fac(:,:)
      rate(:,:,273) = 8.60e-13_r8 * exp_fac(:,:)
      rate(:,:,286) = 7.50e-13_r8 * exp_fac(:,:)
      rate(:,:,293) = 8.00e-13_r8 * exp_fac(:,:)
      rate(:,:,311) = 8.00e-13_r8 * exp_fac(:,:)
      rate(:,:,317) = 8.00e-13_r8 * exp_fac(:,:)
      rate(:,:,323) = 7.50e-13_r8 * exp_fac(:,:)
      rate(:,:,327) = 8.00e-13_r8 * exp_fac(:,:)
      rate(:,:,334) = 7.50e-13_r8 * exp_fac(:,:)
      rate(:,:,339) = 1.4e-12_r8 * exp_fac(:,:)
      rate(:,:,342) = 1.4e-12_r8 * exp_fac(:,:)
      rate(:,:,348) = 7.5e-13_r8 * exp_fac(:,:)
      rate(:,:,232) = 7.20e-11_r8 * exp( -70._r8 * itemp(:,:) )
      rate(:,:,234) = 7.66e-12_r8 * exp( -1020._r8 * itemp(:,:) )
      rate(:,:,239) = 1.60e11_r8 * exp( -4150._r8 * itemp(:,:) )
      rate(:,:,240) = 1.2e-14_r8 * exp( -2630._r8 * itemp(:,:) )
      rate(:,:,242) = 2.60e-12_r8 * exp( 365._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 350._r8 * itemp(:,:) )
      rate(:,:,247) = 4.63e-12_r8 * exp_fac(:,:)
      rate(:,:,340) = 2.6e-12_r8 * exp_fac(:,:)
      rate(:,:,343) = 2.6e-12_r8 * exp_fac(:,:)
      exp_fac(:,:) = exp( -1900._r8 * itemp(:,:) )
      rate(:,:,248) = 1.40e-12_r8 * exp_fac(:,:)
      rate(:,:,261) = 6.50e-15_r8 * exp_fac(:,:)
      exp_fac(:,:) = exp( 1040._r8 * itemp(:,:) )
      rate(:,:,251) = 4.30e-13_r8 * exp_fac(:,:)
      rate(:,:,299) = 4.30e-13_r8 * exp_fac(:,:)
      exp_fac(:,:) = exp( 500._r8 * itemp(:,:) )
      rate(:,:,252) = 2.00e-12_r8 * exp_fac(:,:)
      rate(:,:,253) = 2.50e-12_r8 * exp_fac(:,:)
      rate(:,:,274) = 7.10e-13_r8 * exp_fac(:,:)
      rate(:,:,300) = 2.00e-12_r8 * exp_fac(:,:)
      rate(:,:,257) = 6.90e-12_r8 * exp( -230._r8 * itemp(:,:) )
      rate(:,:,262) = 4.60e-13_r8 * exp( -1156._r8 * itemp(:,:) )
      rate(:,:,265) = 3.75e-13_r8 * exp( -40._r8 * itemp(:,:) )
      rate(:,:,267) = 8.70e-12_r8 * exp( -615._r8 * itemp(:,:) )
      rate(:,:,277) = 8.40e-13_r8 * exp( 830._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( -1860._r8 * itemp(:,:) )
      rate(:,:,278) = 1.40e-12_r8 * exp_fac(:,:)
      rate(:,:,320) = 1.40e-12_r8 * exp_fac(:,:)
      rate(:,:,282) = 4.13e-12_r8 * exp( 452._r8 * itemp(:,:) )
      rate(:,:,283) = 7.52e-16_r8 * exp( -1521._r8 * itemp(:,:) )
      rate(:,:,284) = 2.30e-12_r8 * exp( -170._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 175._r8 * itemp(:,:) )
      rate(:,:,288) = 1.86e-11_r8 * exp_fac(:,:)
      rate(:,:,321) = 1.86e-11_r8 * exp_fac(:,:)
      rate(:,:,289) = 4.40e-15_r8 * exp( -2500._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 360._r8 * itemp(:,:) )
      rate(:,:,290) = 2.70e-12_r8 * exp_fac(:,:)
      rate(:,:,291) = 1.30e-13_r8 * exp_fac(:,:)
      rate(:,:,297) = 5.30e-12_r8 * exp_fac(:,:)
      rate(:,:,315) = 2.70e-12_r8 * exp_fac(:,:)
      rate(:,:,325) = 2.7e-12_r8 * exp_fac(:,:)
      exp_fac(:,:) = exp( 400._r8 * itemp(:,:) )
      rate(:,:,294) = 5.00e-13_r8 * exp_fac(:,:)
      rate(:,:,313) = 5.00e-13_r8 * exp_fac(:,:)
      rate(:,:,328) = 5.e-13_r8 * exp_fac(:,:)
      exp_fac(:,:) = exp( 530._r8 * itemp(:,:) )
      rate(:,:,301) = 4.60e-12_r8 * exp_fac(:,:)
      rate(:,:,302) = 2.30e-12_r8 * exp_fac(:,:)
      rate(:,:,306) = 2.54e-11_r8 * exp( 410._r8 * itemp(:,:) )
      rate(:,:,308) = 3.03e-12_r8 * exp( -446._r8 * itemp(:,:) )
      rate(:,:,329) = 1.3e-12_r8 * exp( 640._r8 * itemp(:,:) )
      rate(:,:,330) = 1.90e-12_r8 * exp( 190._r8 * itemp(:,:) )
      rate(:,:,332) = 1.70e-12_r8 * exp( 352._r8 * itemp(:,:) )
      rate(:,:,338) = 2.3e-12_r8 * exp( -193._r8 * itemp(:,:) )
      rate(:,:,344) = 1.2e-11_r8 * exp( 444._r8 * itemp(:,:) )
      rate(:,:,345) = 1.e-15_r8 * exp( -732._r8 * itemp(:,:) )
      rate(:,:,346) = 1.2e-12_r8 * exp( 490._r8 * itemp(:,:) )
      rate(:,:,355) = 9.60e-12_r8 * exp( -234._r8 * itemp(:,:) )
      rate(:,:,357) = 1.90e-13_r8 * exp( 520._r8 * itemp(:,:) )
      rate(:,:,358) = 1.70e-12_r8 * exp( -710._r8 * itemp(:,:) )
      rate(:,:,361) = 2.10E-11_r8 * exp( -2200.0_r8 * itemp(:,:) )
      rate(:,:,362) = 1.10E-13_r8 * exp( -1200.0_r8 * itemp(:,:) )

      itemp(:,:) = 300._r8 * itemp(:,:)

      ko(:,:) = 4.40e-32_r8 * itemp(:,:)**1.3_r8
      kinf(:,:) = 7.5e-11_r8 * itemp(:,:)**(-0.2_r8)
      call jpl( rate(1,1,117), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 6.90e-31_r8 * itemp(:,:)**1.0_r8
      kinf(:,:) = 2.60e-11_r8
      call jpl( rate(1,1,126), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 4.28e-33_r8
      kinf(:,:) = 9.30e-15_r8 * itemp(:,:)**(-4.42_r8)
      call jpl( rate(1,1,134), m, 0.8_r8, ko, kinf, n )

      ko(:,:) = 9.00e-32_r8 * itemp(:,:)**1.5_r8
      kinf(:,:) = 3.0e-11_r8
      call jpl( rate(1,1,141), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 2.50e-31_r8 * itemp(:,:)**1.8_r8
      kinf(:,:) = 2.2e-11_r8 * itemp(:,:)**0.7_r8
      call jpl( rate(1,1,145), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 2.00e-30_r8 * itemp(:,:)**4.4_r8
      kinf(:,:) = 1.4e-12_r8 * itemp(:,:)**0.7_r8
      call jpl( rate(1,1,147), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 1.80e-30_r8 * itemp(:,:)**3.0_r8
      kinf(:,:) = 2.8e-11_r8
      call jpl( rate(1,1,149), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 2.00e-31_r8 * itemp(:,:)**3.4_r8
      kinf(:,:) = 2.9e-12_r8 * itemp(:,:)**1.1_r8
      call jpl( rate(1,1,155), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 1.80e-31_r8 * itemp(:,:)**3.4_r8
      kinf(:,:) = 1.5e-11_r8 * itemp(:,:)**1.9_r8
      call jpl( rate(1,1,171), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 1.60e-32_r8 * itemp(:,:)**4.5_r8
      kinf(:,:) = 3.0e-12_r8 * itemp(:,:)**2.0_r8
      call jpl( rate(1,1,175), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 5.20e-31_r8 * itemp(:,:)**3.2_r8
      kinf(:,:) = 6.9e-12_r8 * itemp(:,:)**2.9_r8
      call jpl( rate(1,1,192), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 5.90e-33_r8 * itemp(:,:)**1.4_r8
      kinf(:,:) = 1.10e-12_r8 * itemp(:,:)**(-1.3_r8)
      call jpl( rate(1,1,215), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 5.20e-30_r8 * itemp(:,:)**2.4_r8
      kinf(:,:) = 2.2e-10_r8 * itemp(:,:)**0.7_r8
      call jpl( rate(1,1,230), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 1.60e-29_r8 * itemp(:,:)**3.3_r8
      kinf(:,:) = 3.1e-10_r8 * itemp(:,:)
      call jpl( rate(1,1,231), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 5.50e-30_r8
      kinf(:,:) = 8.3e-13_r8 * itemp(:,:)**(-2.0_r8)
      call jpl( rate(1,1,233), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 8.60e-29_r8 * itemp(:,:)**3.1_r8
      kinf(:,:) = 9.00e-12_r8 * itemp(:,:)**0.85_r8
      call jpl( rate(1,1,235), m, 0.48_r8, ko, kinf, n )

      ko(:,:) = 9.70e-29_r8 * itemp(:,:)**5.6_r8
      kinf(:,:) = 9.30e-12_r8 * itemp(:,:)**1.5_r8
      call jpl( rate(1,1,250), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 8.00e-27_r8 * itemp(:,:)**3.5_r8
      kinf(:,:) = 3.00e-11_r8
      call jpl( rate(1,1,260), m, 0.5_r8, ko, kinf, n )

      ko(:,:) = 8.00e-27_r8 * itemp(:,:)**3.5_r8
      kinf(:,:) = 3.00e-11_r8
      call jpl( rate(1,1,305), m, 0.5_r8, ko, kinf, n )

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
