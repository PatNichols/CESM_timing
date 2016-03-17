
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

      rate(:,:,48) = 1.20e-10_r8
      rate(:,:,50) = 1.20e-10_r8
      rate(:,:,53) = 1.31e-10_r8
      rate(:,:,54) = 3.50e-11_r8
      rate(:,:,55) = 9.00e-12_r8
      rate(:,:,59) = 7.20e-11_r8
      rate(:,:,60) = 6.90e-12_r8
      rate(:,:,61) = 1.60e-12_r8
      rate(:,:,65) = 1.80e-12_r8
      rate(:,:,68) = 1.80e-12_r8
      rate(:,:,87) = 1.00e-11_r8
      rate(:,:,88) = 2.20e-11_r8
      rate(:,:,89) = 3.50e-12_r8
      rate(:,:,106) = 4.50e-13_r8
      rate(:,:,115) = 1.00e-14_r8
      rate(:,:,118) = 7.00e-13_r8
      rate(:,:,121) = 2.00e-13_r8
      rate(:,:,122) = 6.80e-14_r8
      rate(:,:,131) = 1.00e-12_r8
      rate(:,:,132) = 1.00e-11_r8
      rate(:,:,133) = 1.15e-11_r8
      rate(:,:,136) = 4.00e-14_r8
      rate(:,:,153) = 3.00e-12_r8
      rate(:,:,156) = 6.80e-13_r8
      rate(:,:,157) = 5.40e-11_r8
      rate(:,:,169) = 2.40e-12_r8
      rate(:,:,172) = 1.40e-11_r8
      rate(:,:,175) = 5.00e-12_r8
      rate(:,:,187) = 2.40e-12_r8
      rate(:,:,191) = 1.40e-11_r8
      rate(:,:,193) = 2.40e-12_r8
      rate(:,:,195) = 3.50e-12_r8
      rate(:,:,196) = 4.50e-11_r8
      rate(:,:,203) = 2.40e-12_r8
      rate(:,:,213) = 3.00e-12_r8
      rate(:,:,214) = 1.00e-11_r8
      rate(:,:,221) = 2.10e-6_r8
      itemp(:ncol,:) = 1._r8 / temp(:ncol,:)
      n = ncol*pver
      rate(:,:,45) = 8.00e-12_r8 * exp( -2060._r8 * itemp(:,:) )
      rate(:,:,46) = 2.15e-11_r8 * exp( 110._r8 * itemp(:,:) )
      rate(:,:,47) = 3.30e-11_r8 * exp( 55._r8 * itemp(:,:) )
      rate(:,:,49) = 1.63e-10_r8 * exp( 60._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 20._r8 * itemp(:,:) )
      rate(:,:,51) = 7.25e-11_r8 * exp_fac(:,:)
      rate(:,:,52) = 4.63e-11_r8 * exp_fac(:,:)
      rate(:,:,56) = 7.70e-11_r8 * exp( 100._r8 * itemp(:,:) )
      rate(:,:,58) = 1.40e-10_r8 * exp( -470._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 180._r8 * itemp(:,:) )
      rate(:,:,62) = 1.80e-11_r8 * exp_fac(:,:)
      rate(:,:,113) = 4.20e-12_r8 * exp_fac(:,:)
      rate(:,:,140) = 4.20e-12_r8 * exp_fac(:,:)
      rate(:,:,145) = 4.20e-12_r8 * exp_fac(:,:)
      rate(:,:,158) = 4.20e-12_r8 * exp_fac(:,:)
      rate(:,:,162) = 4.20e-12_r8 * exp_fac(:,:)
      rate(:,:,186) = 4.40e-12_r8 * exp_fac(:,:)
      rate(:,:,199) = 4.20e-12_r8 * exp_fac(:,:)
      rate(:,:,210) = 4.20e-12_r8 * exp_fac(:,:)
      rate(:,:,218) = 4.2e-12_r8 * exp_fac(:,:)
      rate(:,:,63) = 1.70e-12_r8 * exp( -940._r8 * itemp(:,:) )
      rate(:,:,64) = 4.80e-11_r8 * exp( 250._r8 * itemp(:,:) )
      rate(:,:,67) = 2.80e-12_r8 * exp( -1800._r8 * itemp(:,:) )
      rate(:,:,69) = 1.60e-11_r8 * exp( -4570._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 200._r8 * itemp(:,:) )
      rate(:,:,70) = 3.00e-11_r8 * exp_fac(:,:)
      rate(:,:,105) = 3.80e-12_r8 * exp_fac(:,:)
      rate(:,:,123) = 3.80e-12_r8 * exp_fac(:,:)
      rate(:,:,143) = 3.80e-12_r8 * exp_fac(:,:)
      rate(:,:,147) = 3.80e-12_r8 * exp_fac(:,:)
      rate(:,:,152) = 3.80e-12_r8 * exp_fac(:,:)
      rate(:,:,164) = 3.80e-12_r8 * exp_fac(:,:)
      rate(:,:,173) = 2.30e-11_r8 * exp_fac(:,:)
      rate(:,:,189) = 1.52e-11_r8 * exp_fac(:,:)
      rate(:,:,201) = 3.80e-12_r8 * exp_fac(:,:)
      rate(:,:,212) = 3.80e-12_r8 * exp_fac(:,:)
      rate(:,:,220) = 3.8e-12_r8 * exp_fac(:,:)
      rate(:,:,71) = 1.00e-14_r8 * exp( -490._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( -2000._r8 * itemp(:,:) )
      rate(:,:,73) = 1.40e-12_r8 * exp_fac(:,:)
      rate(:,:,184) = 1.05e-14_r8 * exp_fac(:,:)
      rate(:,:,75) = 7.80e-13_r8 * exp( -1050._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 270._r8 * itemp(:,:) )
      rate(:,:,77) = 3.30e-12_r8 * exp_fac(:,:)
      rate(:,:,126) = 8.10e-12_r8 * exp_fac(:,:)
      rate(:,:,78) = 3.00e-12_r8 * exp( -1500._r8 * itemp(:,:) )
      rate(:,:,79) = 5.10e-12_r8 * exp( 210._r8 * itemp(:,:) )
      rate(:,:,81) = 1.20e-13_r8 * exp( -2450._r8 * itemp(:,:) )
      rate(:,:,86) = 1.50e-11_r8 * exp( 170._r8 * itemp(:,:) )
      rate(:,:,91) = 1.30e-12_r8 * exp( 380._r8 * itemp(:,:) )
      rate(:,:,93) = 2.45e-12_r8 * exp( -1775._r8 * itemp(:,:) )
      rate(:,:,96) = 6.00e-13_r8 * exp( -2058._r8 * itemp(:,:) )
      rate(:,:,97) = 5.50e-12_r8 * exp( 125._r8 * itemp(:,:) )
      rate(:,:,98) = 3.40e-11_r8 * exp( -1600._r8 * itemp(:,:) )
      rate(:,:,99) = 9.7e-15_r8 * exp( 625._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 300._r8 * itemp(:,:) )
      rate(:,:,100) = 2.80e-12_r8 * exp_fac(:,:)
      rate(:,:,149) = 2.90e-12_r8 * exp_fac(:,:)
      rate(:,:,101) = 4.10e-13_r8 * exp( 750._r8 * itemp(:,:) )
      rate(:,:,102) = 5.00e-13_r8 * exp( -424._r8 * itemp(:,:) )
      rate(:,:,103) = 1.90e-14_r8 * exp( 706._r8 * itemp(:,:) )
      rate(:,:,104) = 2.90e-12_r8 * exp( -345._r8 * itemp(:,:) )
      rate(:,:,107) = 2.40e12_r8 * exp( -7000._r8 * itemp(:,:) )
      rate(:,:,108) = 2.60e-12_r8 * exp( 265._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 700._r8 * itemp(:,:) )
      rate(:,:,109) = 7.50e-13_r8 * exp_fac(:,:)
      rate(:,:,114) = 7.50e-13_r8 * exp_fac(:,:)
      rate(:,:,120) = 7.50e-13_r8 * exp_fac(:,:)
      rate(:,:,141) = 7.50e-13_r8 * exp_fac(:,:)
      rate(:,:,146) = 7.50e-13_r8 * exp_fac(:,:)
      rate(:,:,150) = 8.60e-13_r8 * exp_fac(:,:)
      rate(:,:,163) = 7.50e-13_r8 * exp_fac(:,:)
      rate(:,:,170) = 8.00e-13_r8 * exp_fac(:,:)
      rate(:,:,188) = 8.00e-13_r8 * exp_fac(:,:)
      rate(:,:,194) = 8.00e-13_r8 * exp_fac(:,:)
      rate(:,:,200) = 7.50e-13_r8 * exp_fac(:,:)
      rate(:,:,204) = 8.00e-13_r8 * exp_fac(:,:)
      rate(:,:,211) = 7.50e-13_r8 * exp_fac(:,:)
      rate(:,:,219) = 7.5e-13_r8 * exp_fac(:,:)
      rate(:,:,111) = 7.66e-12_r8 * exp( -1020._r8 * itemp(:,:) )
      rate(:,:,116) = 1.60e11_r8 * exp( -4150._r8 * itemp(:,:) )
      rate(:,:,117) = 1.2e-14_r8 * exp( -2630._r8 * itemp(:,:) )
      rate(:,:,119) = 2.60e-12_r8 * exp( 365._r8 * itemp(:,:) )
      rate(:,:,124) = 4.63e-12_r8 * exp( 350._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( -1900._r8 * itemp(:,:) )
      rate(:,:,125) = 1.40e-12_r8 * exp_fac(:,:)
      rate(:,:,138) = 6.50e-15_r8 * exp_fac(:,:)
      exp_fac(:,:) = exp( 1040._r8 * itemp(:,:) )
      rate(:,:,128) = 4.30e-13_r8 * exp_fac(:,:)
      rate(:,:,176) = 4.30e-13_r8 * exp_fac(:,:)
      exp_fac(:,:) = exp( 500._r8 * itemp(:,:) )
      rate(:,:,129) = 2.00e-12_r8 * exp_fac(:,:)
      rate(:,:,130) = 2.50e-12_r8 * exp_fac(:,:)
      rate(:,:,151) = 7.10e-13_r8 * exp_fac(:,:)
      rate(:,:,177) = 2.00e-12_r8 * exp_fac(:,:)
      rate(:,:,134) = 6.90e-12_r8 * exp( -230._r8 * itemp(:,:) )
      rate(:,:,139) = 4.60e-13_r8 * exp( -1156._r8 * itemp(:,:) )
      rate(:,:,142) = 3.75e-13_r8 * exp( -40._r8 * itemp(:,:) )
      rate(:,:,144) = 8.70e-12_r8 * exp( -615._r8 * itemp(:,:) )
      rate(:,:,154) = 8.40e-13_r8 * exp( 830._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( -1860._r8 * itemp(:,:) )
      rate(:,:,155) = 1.40e-12_r8 * exp_fac(:,:)
      rate(:,:,197) = 1.40e-12_r8 * exp_fac(:,:)
      rate(:,:,159) = 4.13e-12_r8 * exp( 452._r8 * itemp(:,:) )
      rate(:,:,160) = 7.52e-16_r8 * exp( -1521._r8 * itemp(:,:) )
      rate(:,:,161) = 2.30e-12_r8 * exp( -170._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 175._r8 * itemp(:,:) )
      rate(:,:,165) = 1.86e-11_r8 * exp_fac(:,:)
      rate(:,:,198) = 1.86e-11_r8 * exp_fac(:,:)
      rate(:,:,166) = 4.40e-15_r8 * exp( -2500._r8 * itemp(:,:) )
      exp_fac(:,:) = exp( 360._r8 * itemp(:,:) )
      rate(:,:,167) = 2.70e-12_r8 * exp_fac(:,:)
      rate(:,:,168) = 1.30e-13_r8 * exp_fac(:,:)
      rate(:,:,174) = 5.30e-12_r8 * exp_fac(:,:)
      rate(:,:,192) = 2.70e-12_r8 * exp_fac(:,:)
      rate(:,:,202) = 2.7e-12_r8 * exp_fac(:,:)
      exp_fac(:,:) = exp( 400._r8 * itemp(:,:) )
      rate(:,:,171) = 5.00e-13_r8 * exp_fac(:,:)
      rate(:,:,190) = 5.00e-13_r8 * exp_fac(:,:)
      rate(:,:,205) = 5.e-13_r8 * exp_fac(:,:)
      exp_fac(:,:) = exp( 530._r8 * itemp(:,:) )
      rate(:,:,178) = 4.60e-12_r8 * exp_fac(:,:)
      rate(:,:,179) = 2.30e-12_r8 * exp_fac(:,:)
      rate(:,:,183) = 2.54e-11_r8 * exp( 410._r8 * itemp(:,:) )
      rate(:,:,185) = 3.03e-12_r8 * exp( -446._r8 * itemp(:,:) )
      rate(:,:,206) = 1.3e-12_r8 * exp( 640._r8 * itemp(:,:) )
      rate(:,:,207) = 1.90e-12_r8 * exp( 190._r8 * itemp(:,:) )
      rate(:,:,209) = 1.70e-12_r8 * exp( 352._r8 * itemp(:,:) )
      rate(:,:,215) = 1.2e-11_r8 * exp( 444._r8 * itemp(:,:) )
      rate(:,:,216) = 1.e-15_r8 * exp( -732._r8 * itemp(:,:) )
      rate(:,:,217) = 1.2e-12_r8 * exp( 490._r8 * itemp(:,:) )
      rate(:,:,226) = 9.60e-12_r8 * exp( -234._r8 * itemp(:,:) )
      rate(:,:,228) = 1.90e-13_r8 * exp( 520._r8 * itemp(:,:) )
      rate(:,:,229) = 1.70e-12_r8 * exp( -710._r8 * itemp(:,:) )
      rate(:,:,231) = 2.10E-11_r8 * exp( -2200.0_r8 * itemp(:,:) )
      rate(:,:,232) = 1.10E-13_r8 * exp( -1200.0_r8 * itemp(:,:) )

      itemp(:,:) = 300._r8 * itemp(:,:)

      ko(:,:) = 4.40e-32_r8 * itemp(:,:)**1.3_r8
      kinf(:,:) = 7.5e-11_r8 * itemp(:,:)**(-0.2_r8)
      call jpl( rate(1,1,57), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 6.90e-31_r8 * itemp(:,:)**1.0_r8
      kinf(:,:) = 2.60e-11_r8
      call jpl( rate(1,1,66), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 4.28e-33_r8
      kinf(:,:) = 9.30e-15_r8 * itemp(:,:)**(-4.42_r8)
      call jpl( rate(1,1,74), m, 0.8_r8, ko, kinf, n )

      ko(:,:) = 9.00e-32_r8 * itemp(:,:)**1.5_r8
      kinf(:,:) = 3.0e-11_r8
      call jpl( rate(1,1,76), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 2.50e-31_r8 * itemp(:,:)**1.8_r8
      kinf(:,:) = 2.2e-11_r8 * itemp(:,:)**0.7_r8
      call jpl( rate(1,1,80), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 2.00e-30_r8 * itemp(:,:)**4.4_r8
      kinf(:,:) = 1.4e-12_r8 * itemp(:,:)**0.7_r8
      call jpl( rate(1,1,82), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 1.80e-30_r8 * itemp(:,:)**3.0_r8
      kinf(:,:) = 2.8e-11_r8
      call jpl( rate(1,1,84), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 2.00e-31_r8 * itemp(:,:)**3.4_r8
      kinf(:,:) = 2.9e-12_r8 * itemp(:,:)**1.1_r8
      call jpl( rate(1,1,90), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 5.90e-33_r8 * itemp(:,:)**1.4_r8
      kinf(:,:) = 1.10e-12_r8 * itemp(:,:)**(-1.3_r8)
      call jpl( rate(1,1,95), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 5.50e-30_r8
      kinf(:,:) = 8.3e-13_r8 * itemp(:,:)**(-2.0_r8)
      call jpl( rate(1,1,110), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 8.60e-29_r8 * itemp(:,:)**3.1_r8
      kinf(:,:) = 9.00e-12_r8 * itemp(:,:)**0.85_r8
      call jpl( rate(1,1,112), m, 0.48_r8, ko, kinf, n )

      ko(:,:) = 9.70e-29_r8 * itemp(:,:)**5.6_r8
      kinf(:,:) = 9.30e-12_r8 * itemp(:,:)**1.5_r8
      call jpl( rate(1,1,127), m, 0.6_r8, ko, kinf, n )

      ko(:,:) = 8.00e-27_r8 * itemp(:,:)**3.5_r8
      kinf(:,:) = 3.00e-11_r8
      call jpl( rate(1,1,137), m, 0.5_r8, ko, kinf, n )

      ko(:,:) = 8.00e-27_r8 * itemp(:,:)**3.5_r8
      kinf(:,:) = 3.00e-11_r8
      call jpl( rate(1,1,182), m, 0.5_r8, ko, kinf, n )

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
