




      module mo_lin_matrix

      private
      public :: linmat

      contains

      subroutine linmat01( mat, y, rxt, het_rates )
!----------------------------------------------
! ... linear matrix entries for implicit species
!----------------------------------------------

      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      use shr_kind_mod, only : r8 => shr_kind_r8

      implicit none

!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      real(r8), intent(in) :: y(gas_pcnst)
      real(r8), intent(in) :: rxt(rxntot)
      real(r8), intent(in) :: het_rates(max(1,gas_pcnst))
      real(r8), intent(inout) :: mat(nzcnt)

         mat(845) = -( rxt(2) + rxt(3) + het_rates(1) )
         mat(546) = rxt(46)

         mat(540) = -( rxt(46) + het_rates(2) )
         mat(834) = rxt(3)
         mat(658) = rxt(5)
         mat(120) = rxt(7)
         mat(777) = rxt(9)
         mat(445) = rxt(48) + rxt(49)

         mat(444) = -( rxt(48) + rxt(49) + rxt(51) + rxt(53)*y(4) + rxt(54)*y(4) &
                      + rxt(55)*y(16) + rxt(56)*y(16) + rxt(57)*y(16) + het_rates(3) )
         mat(826) = rxt(2)

         mat(624) = -( het_rates(17) )
         mat(612) = rxt(14) + rxt(15)
         mat(463) = rxt(17)
         mat(505) = 1.340_r8*rxt(23)
         mat(557) = .700_r8*rxt(24)
         mat(513) = rxt(30)
         mat(456) = rxt(32)
         mat(376) = rxt(35)
         mat(282) = .450_r8*rxt(37)
         mat(309) = 2.000_r8*rxt(38)
         mat(98) = rxt(45)

         mat(285) = -( het_rates(11) )
         mat(607) = rxt(15)
         mat(442) = rxt(57)*y(16)

         mat(87) = -( het_rates(117) )

         mat(54) = -( het_rates(118) )

         mat(407) = -( rxt(59) + het_rates(15) )
         mat(145) = rxt(13)
         mat(443) = rxt(56)*y(16)

         mat(953) = -( het_rates(5) )
         mat(668) = rxt(5) + .500_r8*rxt(232)
         mat(123) = rxt(7)
         mat(790) = rxt(10)
         mat(450) = 2.000_r8*rxt(53)*y(4)

         mat(662) = -( rxt(5) + rxt(232) + het_rates(6) )
         mat(121) = rxt(6) + rxt(85)
         mat(233) = rxt(8)
         mat(784) = rxt(9)
         mat(140) = rxt(12) + rxt(94)
         mat(227) = .600_r8*rxt(20) + rxt(137)
         mat(254) = rxt(21) + rxt(183)
         mat(457) = rxt(32)

         mat(786) = -( rxt(9) + rxt(10) + rxt(231) + het_rates(7) )
         mat(122) = rxt(6) + rxt(7) + rxt(85)
         mat(142) = rxt(11)
         mat(229) = .400_r8*rxt(20)

         mat(232) = -( rxt(8) + het_rates(8) )
         mat(119) = 2.000_r8*rxt(230)
         mat(762) = rxt(231)
         mat(652) = .500_r8*rxt(232)

         mat(139) = -( rxt(11) + rxt(12) + rxt(94) + het_rates(9) )

         mat(118) = -( rxt(6) + rxt(7) + rxt(85) + rxt(230) + het_rates(10) )

         mat(750) = -( rxt(95)*y(16) + het_rates(12) )
         mat(234) = rxt(8)
         mat(141) = rxt(11)
         mat(147) = rxt(13)
         mat(108) = 2.000_r8*rxt(16)
         mat(248) = rxt(18)
         mat(214) = rxt(19)
         mat(168) = rxt(25)
         mat(76) = rxt(26)
         mat(132) = rxt(27)
         mat(137) = rxt(28)
         mat(93) = rxt(31)
         mat(320) = rxt(39)
         mat(127) = rxt(40)
         mat(185) = rxt(41)
         mat(241) = rxt(42)
         mat(447) = 2.000_r8*rxt(51) + rxt(55)*y(16)
         mat(663) = .500_r8*rxt(232)

         mat(908) = -( rxt(240) + het_rates(13) )
         mat(143) = rxt(12) + rxt(94)
         mat(468) = rxt(17)
         mat(249) = rxt(18)
         mat(510) = 1.340_r8*rxt(22) + .660_r8*rxt(23)
         mat(169) = rxt(25)
         mat(133) = rxt(27)
         mat(518) = rxt(30)
         mat(460) = rxt(32)
         mat(265) = rxt(33)
         mat(500) = rxt(34)
         mat(378) = 2.000_r8*rxt(35)
         mat(284) = .560_r8*rxt(37)
         mat(311) = 2.000_r8*rxt(38)
         mat(322) = .900_r8*rxt(39)
         mat(242) = rxt(42)
         mat(412) = rxt(59)
         mat(202) = rxt(109)
         mat(117) = rxt(117) + rxt(118)
         mat(449) = rxt(56)*y(16)

         mat(106) = -( rxt(16) + het_rates(14) )
         mat(853) = .500_r8*rxt(240)

         mat(987) = -( het_rates(18) )
         mat(469) = rxt(17)
         mat(216) = rxt(19)
         mat(231) = .400_r8*rxt(20)
         mat(565) = .300_r8*rxt(24)
         mat(335) = rxt(29)
         mat(451) = rxt(55)*y(16)
         mat(756) = rxt(95)*y(16)

         mat(144) = -( rxt(13) + het_rates(19) )

         mat(611) = -( rxt(14) + rxt(15) + het_rates(20) )
         mat(146) = rxt(13)
         mat(247) = rxt(18)
         mat(504) = 1.340_r8*rxt(22)
         mat(136) = rxt(28)
         mat(455) = rxt(32)
         mat(263) = .690_r8*rxt(33)
         mat(497) = rxt(34)
         mat(375) = rxt(35)
         mat(319) = .100_r8*rxt(39)
         mat(199) = rxt(109)
         mat(116) = 2.000_r8*rxt(118)
         mat(446) = rxt(56)*y(16) + rxt(57)*y(16)

         mat(266) = -( het_rates(21) )

         mat(110) = -( het_rates(22) )

         mat(74) = -( rxt(26) + het_rates(28) )

         mat(153) = -( het_rates(23) )

         mat(114) = -( rxt(117) + rxt(118) + het_rates(24) )
         mat(75) = rxt(26)

         mat(301) = -( het_rates(25) )

         mat(209) = -( het_rates(26) )

         mat(374) = -( rxt(35) + het_rates(27) )
         mat(115) = rxt(117)

         mat(57) = -( het_rates(29) )

         mat(365) = -( het_rates(30) )
         mat(206) = rxt(36)

         mat(165) = -( rxt(25) + het_rates(31) )

         mat(462) = -( rxt(17) + het_rates(32) )
         mat(245) = rxt(18)
         mat(167) = rxt(25)
         mat(318) = .400_r8*rxt(39)
         mat(126) = rxt(40)

         mat(811) = -( het_rates(33) )
         mat(230) = .600_r8*rxt(20) + rxt(137)
         mat(508) = 1.340_r8*rxt(22)
         mat(561) = .300_r8*rxt(24)
         mat(138) = rxt(28)
         mat(334) = rxt(29)
         mat(517) = rxt(30)
         mat(499) = rxt(34)
         mat(208) = rxt(36)
         mat(283) = .130_r8*rxt(37)
         mat(128) = rxt(40)

         mat(212) = -( rxt(19) + het_rates(34) )

         mat(428) = -( het_rates(35) )
         mat(550) = .700_r8*rxt(24)

         mat(60) = -( het_rates(36) )

         mat(381) = -( het_rates(37) )

         mat(129) = -( rxt(27) + het_rates(38) )

         mat(337) = -( het_rates(39) )

         mat(243) = -( rxt(18) + het_rates(40) )

         mat(331) = -( rxt(29) + het_rates(41) )
         mat(130) = .820_r8*rxt(27)
         mat(315) = .250_r8*rxt(39)
         mat(237) = .100_r8*rxt(42)

         mat(485) = -( het_rates(42) )

         mat(134) = -( rxt(28) + het_rates(43) )

         mat(42) = -( het_rates(44) )

         mat(170) = -( het_rates(45) )

         mat(45) = -( het_rates(49) )

         mat(350) = -( het_rates(50) )

         mat(313) = -( rxt(39) + het_rates(51) )

         mat(204) = -( rxt(36) + het_rates(46) )
         mat(312) = .800_r8*rxt(39)

         mat(324) = -( het_rates(47) )

         mat(124) = -( rxt(40) + het_rates(48) )

         mat(392) = -( het_rates(52) )

         mat(593) = -( het_rates(53) )

         mat(258) = -( rxt(33) + het_rates(54) )

         mat(554) = -( rxt(24) + het_rates(55) )
         mat(261) = .402_r8*rxt(33)
         mat(240) = rxt(42)

         mat(501) = -( rxt(22) + rxt(23) + het_rates(56) )
         mat(259) = .288_r8*rxt(33)
         mat(239) = rxt(42)

         mat(573) = -( het_rates(57) )

         mat(160) = -( het_rates(58) )

         mat(639) = -( het_rates(59) )
         mat(253) = rxt(21) + rxt(183)
         mat(506) = .660_r8*rxt(22)

         mat(177) = -( het_rates(60) )

         mat(495) = -( rxt(34) + het_rates(61) )

         mat(512) = -( rxt(30) + het_rates(62) )
         mat(281) = .180_r8*rxt(37)
         mat(184) = .450_r8*rxt(41)

         mat(525) = -( het_rates(63) )

         mat(91) = -( rxt(31) + het_rates(64) )

         mat(290) = -( het_rates(65) )

         mat(415) = -( het_rates(66) )

         mat(236) = -( rxt(42) + het_rates(67) )

         mat(63) = -( het_rates(68) )

         mat(68) = -( het_rates(69) )

         mat(271) = -( het_rates(70) )

         mat(180) = -( rxt(41) + het_rates(71) )

         mat(83) = -( het_rates(78) )

         mat(279) = -( rxt(37) + het_rates(79) )
         mat(182) = .900_r8*rxt(41)

         mat(308) = -( rxt(38) + het_rates(80) )
         mat(280) = .130_r8*rxt(37)
         mat(183) = .450_r8*rxt(41)


      end subroutine linmat01

      subroutine linmat02( mat, y, rxt, het_rates )
!----------------------------------------------
! ... linear matrix entries for implicit species
!----------------------------------------------

      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      use shr_kind_mod, only : r8 => shr_kind_r8

      implicit none

!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      real(r8), intent(in) :: y(gas_pcnst)
      real(r8), intent(in) :: rxt(rxntot)
      real(r8), intent(in) :: het_rates(max(1,gas_pcnst))
      real(r8), intent(inout) :: mat(nzcnt)

         mat(48) = -( het_rates(72) )

         mat(187) = -( het_rates(73) )

         mat(1) = -( het_rates(74) )

         mat(51) = -( het_rates(75) )

         mat(218) = -( het_rates(76) )

         mat(2) = -( het_rates(77) )

         mat(225) = -( rxt(20) + rxt(137) + het_rates(81) )

         mat(193) = -( het_rates(82) )

         mat(250) = -( rxt(21) + rxt(183) + het_rates(83) )

         mat(472) = -( het_rates(84) )

         mat(453) = -( rxt(32) + het_rates(85) )

         mat(95) = -( rxt(45) + het_rates(100) )

         mat(72) = -( het_rates(101) )

         mat(101) = -( het_rates(102) )

         mat(3) = -( het_rates(103) )

         mat(40) = -( het_rates(104) )

         mat(4) = -( het_rates(105) )

         mat(5) = -( het_rates(106) )

         mat(6) = -( het_rates(90) )

         mat(7) = -( het_rates(91) )

         mat(8) = -( het_rates(92) )

         mat(9) = -( het_rates(93) )

         mat(10) = -( het_rates(94) )

         mat(11) = -( het_rates(95) )

         mat(12) = -( het_rates(96) )

         mat(13) = -( het_rates(97) )

         mat(14) = -( het_rates(98) )

         mat(15) = -( het_rates(99) )

         mat(16) = -( rxt(233) + het_rates(86) )

         mat(18) = -( het_rates(87) )
         mat(17) = rxt(233)

         mat(19) = -( rxt(239) + het_rates(88) )

         mat(21) = -( het_rates(89) )
         mat(20) = rxt(239)

         mat(77) = -( het_rates(119) )

         mat(149) = -( het_rates(120) )

         mat(198) = -( rxt(109) + het_rates(121) )

         mat(22) = -( het_rates(107) )

         mat(23) = -( het_rates(108) )

         mat(24) = -( het_rates(109) )

         mat(25) = -( het_rates(110) )

         mat(26) = -( het_rates(111) )

         mat(27) = -( het_rates(112) )

         mat(28) = -( het_rates(113) )

         mat(29) = -( het_rates(114) )

         mat(30) = -( rxt(243) + het_rates(122) )

         mat(31) = -( rxt(244) + het_rates(123) )

         mat(32) = -( rxt(245) + het_rates(124) )

         mat(33) = -( het_rates(125) )

         mat(34) = -( rxt(246) + het_rates(126) )

         mat(35) = -( rxt(247) + het_rates(127) )

         mat(36) = -( rxt(248) + het_rates(128) )

         mat(37) = -( het_rates(129) )

         mat(38) = -( rxt(43) + het_rates(130) )

         mat(39) = -( rxt(44) + het_rates(131) )


      end subroutine linmat02

      subroutine linmat( mat, y, rxt, het_rates )
!----------------------------------------------
! ... linear matrix entries for implicit species
!----------------------------------------------

      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      use shr_kind_mod, only : r8 => shr_kind_r8

      implicit none

!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      real(r8), intent(in) :: y(gas_pcnst)
      real(r8), intent(in) :: rxt(rxntot)
      real(r8), intent(in) :: het_rates(max(1,gas_pcnst))
      real(r8), intent(inout) :: mat(nzcnt)

      call linmat01( mat, y, rxt, het_rates )
      call linmat02( mat, y, rxt, het_rates )

      end subroutine linmat

      end module mo_lin_matrix
