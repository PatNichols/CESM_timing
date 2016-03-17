
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

      rate(:,:,50) = 1.20e-10_r8
      rate(:,:,52) = 1.20e-10_r8
      rate(:,:,55) = 1.31e-10_r8
      rate(:,:,56) = 3.50e-11_r8
      rate(:,:,57) = 9.00e-12_r8
      rate(:,:,61) = 7.20e-11_r8
      rate(:,:,62) = 6.90e-12_r8
      rate(:,:,63) = 1.60e-12_r8
      rate(:,:,67) = 1.80e-12_r8
      rate(:,:,70) = 1.80e-12_r8
      rate(:,:,89) = 1.00e-11_r8
      rate(:,:,90) = 2.20e-11_r8
      rate(:,:,91) = 3.50e-12_r8
      rate(:,:,108) = 4.50e-13_r8
      rate(:,:,117) = 1.00e-14_r8
      rate(:,:,120) = 7.00e-13_r8
      rate(:,:,123) = 2.00e-13_r8
      rate(:,:,124) = 6.80e-14_r8
      rate(:,:,133) = 1.00e-12_r8
      rate(:,:,134) = 1.00e-11_r8
      rate(:,:,135) = 1.15e-11_r8
      rate(:,:,138) = 4.00e-14_r8
      rate(:,:,155) = 3.00e-12_r8
      rate(:,:,158) = 6.80e-13_r8
      rate(:,:,159) = 5.40e-11_r8
      rate(:,:,171) = 2.40e-12_r8
      rate(:,:,174) = 1.40e-11_r8
      rate(:,:,177) = 5.00e-12_r8
      rate(:,:,189) = 2.40e-12_r8
      rate(:,:,193) = 1.40e-11_r8
      rate(:,:,195) = 2.40e-12_r8
      rate(:,:,197) = 3.50e-12_r8
      rate(:,:,198) = 4.50e-11_r8
      rate(:,:,205) = 2.40e-12_r8
      rate(:,:,215) = 3.00e-12_r8
      rate(:,:,216) = 1.00e-11_r8
      rate(:,:,220) = 2.3e-11_r8
      rate(:,:,229) = 2.10e-6_r8
      rate(:,:,233) = 7.10e-6_r8
      rate(:,:,239) = 7.10e-6_r8
      rate(:,:,243) = 2.31e-06_r8
      rate(:,:,244) = 2.31e-07_r8
      rate(:,:,245) = 2.31e-07_r8
      rate(:,:,246) = 4.63e-07_r8
      rate(:,:,247) = 4.63e-07_r8
      rate(:,:,248) = 2.31e-07_r8
      itemp(:ncol,:) = 1._r8 / temp(:ncol,:)
      n = ncol*pver
      rate(:,:,47) = 8.00e-12_r8 * exp( -2060._r8 * itemp(:,:) )
      rate(:,:,48) = 2.15e-11_r8 * exp( 110._r8 * itemp(:,:) )
      rate(:,:,49) = 3.30e-11_r8 * exp( 55._r8 * itemp(:,:) )
      rate(:,:,51) = 1.63e-10_r8 * exp( 60._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 20._r8 * itemp(:,:) )
      rate(:,:,53) = 7.25e-11_r8 * exp_fac(:,:)
      rate(:,:,54) = 4.63e-11_r8 * exp_fac(:,:)
      rate(:,:,58) = 7.70e-11_r8 * exp( 100._r8 * itemp(:,:) )
      rate(:,:,60) = 1.40e-10_r8 * exp( -470._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 180._r8 * itemp(:,:) )
      rate(:,:,64) = 1.80e-11_r8 * exp_fac(:,:)
      rate(:,:,115) = 4.20e-12_r8 * exp_fac(:,:)
      rate(:,:,142) = 4.20e-12_r8 * exp_fac(:,:)
      rate(:,:,147) = 4.20e-12_r8 * exp_fac(:,:)
      rate(:,:,160) = 4.20e-12_r8 * exp_fac(:,:)
      rate(:,:,164) = 4.20e-12_r8 * exp_fac(:,:)
      rate(:,:,188) = 4.40e-12_r8 * exp_fac(:,:)
      rate(:,:,201) = 4.20e-12_r8 * exp_fac(:,:)
      rate(:,:,212) = 4.20e-12_r8 * exp_fac(:,:)
      rate(:,:,226) = 4.2e-12_r8 * exp_fac(:,:)
      rate(:,:,65) = 1.70e-12_r8 * exp( -940._r8 * itemp(:,:) )
      rate(:,:,66) = 4.80e-11_r8 * exp( 250._r8 * itemp(:,:) )
      rate(:,:,69) = 2.80e-12_r8 * exp( -1800._r8 * itemp(:,:) )
      rate(:,:,71) = 1.60e-11_r8 * exp( -4570._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 200._r8 * itemp(:,:) )
      rate(:,:,72) = 3.00e-11_r8 * exp_fac(:,:)
      rate(:,:,107) = 3.80e-12_r8 * exp_fac(:,:)
      rate(:,:,125) = 3.80e-12_r8 * exp_fac(:,:)
      rate(:,:,145) = 3.80e-12_r8 * exp_fac(:,:)
      rate(:,:,149) = 3.80e-12_r8 * exp_fac(:,:)
      rate(:,:,154) = 3.80e-12_r8 * exp_fac(:,:)
      rate(:,:,166) = 3.80e-12_r8 * exp_fac(:,:)
      rate(:,:,175) = 2.30e-11_r8 * exp_fac(:,:)
      rate(:,:,191) = 1.52e-11_r8 * exp_fac(:,:)
      rate(:,:,203) = 3.80e-12_r8 * exp_fac(:,:)
      rate(:,:,214) = 3.80e-12_r8 * exp_fac(:,:)
      rate(:,:,228) = 3.8e-12_r8 * exp_fac(:,:)
      rate(:,:,73) = 1.00e-14_r8 * exp( -490._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( -2000._r8 * itemp(:,:) )
      rate(:,:,75) = 1.40e-12_r8 * exp_fac(:,:)
      rate(:,:,186) = 1.05e-14_r8 * exp_fac(:,:)
      rate(:,:,77) = 7.80e-13_r8 * exp( -1050._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 270._r8 * itemp(:,:) )
      rate(:,:,79) = 3.30e-12_r8 * exp_fac(:,:)
      rate(:,:,128) = 8.10e-12_r8 * exp_fac(:,:)
      rate(:,:,80) = 3.00e-12_r8 * exp( -1500._r8 * itemp(:,:) )
      rate(:,:,81) = 5.10e-12_r8 * exp( 210._r8 * itemp(:,:) )
      rate(:,:,83) = 1.20e-13_r8 * exp( -2450._r8 * itemp(:,:) )
      rate(:,:,88) = 1.50e-11_r8 * exp( 170._r8 * itemp(:,:) )
      rate(:,:,93) = 1.30e-12_r8 * exp( 380._r8 * itemp(:,:) )
      rate(:,:,95) = 2.45e-12_r8 * exp( -1775._r8 * itemp(:,:) )
      rate(:,:,98) = 6.00e-13_r8 * exp( -2058._r8 * itemp(:,:) )
      rate(:,:,99) = 5.50e-12_r8 * exp( 125._r8 * itemp(:,:) )
      rate(:,:,100) = 3.40e-11_r8 * exp( -1600._r8 * itemp(:,:) )
      rate(:,:,101) = 9.7e-15_r8 * exp( 625._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 300._r8 * itemp(:,:) )
      rate(:,:,102) = 2.80e-12_r8 * exp_fac(:,:)
      rate(:,:,151) = 2.90e-12_r8 * exp_fac(:,:)
      rate(:,:,103) = 4.10e-13_r8 * exp( 750._r8 * itemp(:,:) )
      rate(:,:,104) = 5.00e-13_r8 * exp( -424._r8 * itemp(:,:) )
      rate(:,:,105) = 1.90e-14_r8 * exp( 706._r8 * itemp(:,:) )
      rate(:,:,106) = 2.90e-12_r8 * exp( -345._r8 * itemp(:,:) )
      rate(:,:,109) = 2.40e12_r8 * exp( -7000._r8 * itemp(:,:) )
      rate(:,:,110) = 2.60e-12_r8 * exp( 265._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 700._r8 * itemp(:,:) )
      rate(:,:,111) = 7.50e-13_r8 * exp_fac(:,:)
      rate(:,:,116) = 7.50e-13_r8 * exp_fac(:,:)
      rate(:,:,122) = 7.50e-13_r8 * exp_fac(:,:)
      rate(:,:,143) = 7.50e-13_r8 * exp_fac(:,:)
      rate(:,:,148) = 7.50e-13_r8 * exp_fac(:,:)
      rate(:,:,152) = 8.60e-13_r8 * exp_fac(:,:)
      rate(:,:,165) = 7.50e-13_r8 * exp_fac(:,:)
      rate(:,:,172) = 8.00e-13_r8 * exp_fac(:,:)
      rate(:,:,190) = 8.00e-13_r8 * exp_fac(:,:)
      rate(:,:,196) = 8.00e-13_r8 * exp_fac(:,:)
      rate(:,:,202) = 7.50e-13_r8 * exp_fac(:,:)
      rate(:,:,206) = 8.00e-13_r8 * exp_fac(:,:)
      rate(:,:,213) = 7.50e-13_r8 * exp_fac(:,:)
      rate(:,:,218) = 1.4e-12_r8 * exp_fac(:,:)
      rate(:,:,221) = 1.4e-12_r8 * exp_fac(:,:)
      rate(:,:,227) = 7.5e-13_r8 * exp_fac(:,:)
      rate(:,:,113) = 7.66e-12_r8 * exp( -1020._r8 * itemp(:,:) )
      rate(:,:,118) = 1.60e11_r8 * exp( -4150._r8 * itemp(:,:) )
      rate(:,:,119) = 1.2e-14_r8 * exp( -2630._r8 * itemp(:,:) )
      rate(:,:,121) = 2.60e-12_r8 * exp( 365._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 350._r8 * itemp(:,:) )
      rate(:,:,126) = 4.63e-12_r8 * exp_fac(:,:)
      rate(:,:,219) = 2.6e-12_r8 * exp_fac(:,:)
      rate(:,:,222) = 2.6e-12_r8 * exp_fac(:,:)
      exp_fac(:,:) = exp( -1900._r8 * itemp(:,:) )
      rate(:,:,127) = 1.40e-12_r8 * exp_fac(:,:)
      rate(:,:,140) = 6.50e-15_r8 * exp_fac(:,:)
      exp_fac(:,:) = exp( 1040._r8 * itemp(:,:) )
      rate(:,:,130) = 4.30e-13_r8 * exp_fac(:,:)
      rate(:,:,178) = 4.30e-13_r8 * exp_fac(:,:)
      exp_fac(:,:) = exp( 500._r8 * itemp(:,:) )
      rate(:,:,131) = 2.00e-12_r8 * exp_fac(:,:)
      rate(:,:,132) = 2.50e-12_r8 * exp_fac(:,:)
      rate(:,:,153) = 7.10e-13_r8 * exp_fac(:,:)
      rate(:,:,179) = 2.00e-12_r8 * exp_fac(:,:)
      rate(:,:,136) = 6.90e-12_r8 * exp( -230._r8 * itemp(:,:) )
      rate(:,:,141) = 4.60e-13_r8 * exp( -1156._r8 * itemp(:,:) )
      rate(:,:,144) = 3.75e-13_r8 * exp( -40._r8 * itemp(:,:) )
      rate(:,:,146) = 8.70e-12_r8 * exp( -615._r8 * itemp(:,:) )
      rate(:,:,156) = 8.40e-13_r8 * exp( 830._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( -1860._r8 * itemp(:,:) )
      rate(:,:,157) = 1.40e-12_r8 * exp_fac(:,:)
      rate(:,:,199) = 1.40e-12_r8 * exp_fac(:,:)
      rate(:,:,161) = 4.13e-12_r8 * exp( 452._r8 * itemp(:,:) )
      rate(:,:,162) = 7.52e-16_r8 * exp( -1521._r8 * itemp(:,:) )
      rate(:,:,163) = 2.30e-12_r8 * exp( -170._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 175._r8 * itemp(:,:) )
      rate(:,:,167) = 1.86e-11_r8 * exp_fac(:,:)
      rate(:,:,200) = 1.86e-11_r8 * exp_fac(:,:)
      rate(:,:,168) = 4.40e-15_r8 * exp( -2500._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 360._r8 * itemp(:,:) )
      rate(:,:,169) = 2.70e-12_r8 * exp_fac(:,:)
      rate(:,:,170) = 1.30e-13_r8 * exp_fac(:,:)
      rate(:,:,176) = 5.30e-12_r8 * exp_fac(:,:)
      rate(:,:,194) = 2.70e-12_r8 * exp_fac(:,:)
      rate(:,:,204) = 2.7e-12_r8 * exp_fac(:,:)
      exp_fac(:,:) = exp( 400._r8 * itemp(:,:) )
      rate(:,:,173) = 5.00e-13_r8 * exp_fac(:,:)
      rate(:,:,192) = 5.00e-13_r8 * exp_fac(:,:)
      rate(:,:,207) = 5.e-13_r8 * exp_fac(:,:)
      exp_fac(:,:) = exp( 530._r8 * itemp(:,:) )
      rate(:,:,180) = 4.60e-12_r8 * exp_fac(:,:)
      rate(:,:,181) = 2.30e-12_r8 * exp_fac(:,:)
      rate(:,:,185) = 2.54e-11_r8 * exp( 410._r8 * itemp(:,:) )
      rate(:,:,187) = 3.03e-12_r8 * exp( -446._r8 * itemp(:,:) )
      rate(:,:,208) = 1.3e-12_r8 * exp( 640._r8 * itemp(:,:) )
      rate(:,:,209) = 1.90e-12_r8 * exp( 190._r8 * itemp(:,:) )
      rate(:,:,211) = 1.70e-12_r8 * exp( 352._r8 * itemp(:,:) )
      rate(:,:,217) = 2.3e-12_r8 * exp( -193._r8 * itemp(:,:) )
      rate(:,:,223) = 1.2e-11_r8 * exp( 444._r8 * itemp(:,:) )
      rate(:,:,224) = 1.e-15_r8 * exp( -732._r8 * itemp(:,:) )
      rate(:,:,225) = 1.2e-12_r8 * exp( 490._r8 * itemp(:,:) )
      rate(:,:,235) = 9.60e-12_r8 * exp( -234._r8 * itemp(:,:) )
      rate(:,:,237) = 1.90e-13_r8 * exp( 520._r8 * itemp(:,:) )
      rate(:,:,238) = 1.70e-12_r8 * exp( -710._r8 * itemp(:,:) )
      rate(:,:,241) = 2.10E-11_r8 * exp( -2200.0_r8 * itemp(:,:) )
      rate(:,:,242) = 1.10E-13_r8 * exp( -1200.0_r8 * itemp(:,:) )

      itemp(:,:) = 300._r8 * itemp(:,:)

      ko(:,:) = 4.40e-32_r8 * itemp(:,:)**1.3_r8
      kinf(:,:) = 7.5e-11_r8 * itemp(:,:)**(-0.2_r8)
      call jpl( rate(1,1,59), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 6.90e-31_r8 * itemp(:,:)**1.0_r8
      kinf(:,:) = 2.60e-11_r8
      call jpl( rate(1,1,68), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 4.28e-33_r8
      kinf(:,:) = 9.30e-15_r8 * itemp(:,:)**(-4.42_r8)
      call jpl( rate(1,1,76), m, 0.8_r8, ko, kinf, n )

      ko(:,:) = 9.00e-32_r8 * itemp(:,:)**1.5_r8
      kinf(:,:) = 3.0e-11_r8
      call jpl( rate(1,1,78), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 2.50e-31_r8 * itemp(:,:)**1.8_r8
      kinf(:,:) = 2.2e-11_r8 * itemp(:,:)**0.7_r8
      call jpl( rate(1,1,82), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 2.00e-30_r8 * itemp(:,:)**4.4_r8
      kinf(:,:) = 1.4e-12_r8 * itemp(:,:)**0.7_r8
      call jpl( rate(1,1,84), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 1.80e-30_r8 * itemp(:,:)**3.0_r8
      kinf(:,:) = 2.8e-11_r8
      call jpl( rate(1,1,86), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 2.00e-31_r8 * itemp(:,:)**3.4_r8
      kinf(:,:) = 2.9e-12_r8 * itemp(:,:)**1.1_r8
      call jpl( rate(1,1,92), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 5.90e-33_r8 * itemp(:,:)**1.4_r8
      kinf(:,:) = 1.10e-12_r8 * itemp(:,:)**(-1.3_r8)
      call jpl( rate(1,1,97), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 5.50e-30_r8
      kinf(:,:) = 8.3e-13_r8 * itemp(:,:)**(-2.0_r8)
      call jpl( rate(1,1,112), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 8.60e-29_r8 * itemp(:,:)**3.1_r8
      kinf(:,:) = 9.00e-12_r8 * itemp(:,:)**0.85_r8
      call jpl( rate(1,1,114), m, 0.48_r8, ko, kinf, n )

      ko(:,:) = 9.70e-29_r8 * itemp(:,:)**5.6_r8
      kinf(:,:) = 9.30e-12_r8 * itemp(:,:)**1.5_r8
      call jpl( rate(1,1,129), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 8.00e-27_r8 * itemp(:,:)**3.5_r8
      kinf(:,:) = 3.00e-11_r8
      call jpl( rate(1,1,139), m, 0.5_r8, ko, kinf, n )

      ko(:,:) = 8.00e-27_r8 * itemp(:,:)**3.5_r8
      kinf(:,:) = 3.00e-11_r8
      call jpl( rate(1,1,184), m, 0.5_r8, ko, kinf, n )

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
