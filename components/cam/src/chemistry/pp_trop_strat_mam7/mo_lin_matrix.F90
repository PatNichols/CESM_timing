




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

         mat(1246) = -( rxt(2) + rxt(3) + het_rates(1) )
         mat(1135) = rxt(83)

         mat(1132) = -( rxt(83) + het_rates(2) )
         mat(1243) = rxt(3)
         mat(1183) = rxt(5)
         mat(840) = rxt(6)
         mat(118) = rxt(8)
         mat(1086) = rxt(10)
         mat(701) = rxt(19)
         mat(863) = rxt(22)
         mat(70) = rxt(23)
         mat(1204) = rxt(30)
         mat(1106) = rxt(86) + rxt(87)
         mat(85) = rxt(134)

         mat(1105) = -( rxt(86) + rxt(87) + rxt(89)*y(4) + rxt(90)*y(4) + rxt(92)*y(101) &
                      + rxt(93)*y(102) + rxt(94)*y(103) + rxt(95)*y(111) + rxt(96)*y(112) &
                      + rxt(97)*y(104) + rxt(98)*y(109) + rxt(99)*y(110) + rxt(100)*y(105) &
                      + rxt(101)*y(100) + rxt(102)*y(108) + rxt(103)*y(107) &
                      + rxt(104)*y(113) + rxt(105)*y(114) + rxt(106)*y(115) &
                      + rxt(107)*y(116) + rxt(108)*y(12) + rxt(109)*y(12) + rxt(110)*y(12) &
                 + het_rates(3) )
         mat(1242) = rxt(2)
         mat(700) = rxt(18)

         mat(508) = -( het_rates(18) )
         mat(766) = rxt(16)
         mat(694) = rxt(18)
         mat(1092) = rxt(110)*y(12)

         mat(516) = -( het_rates(17) )
         mat(767) = rxt(15) + rxt(16)
         mat(540) = rxt(56)
         mat(550) = 1.340_r8*rxt(62)
         mat(591) = .700_r8*rxt(63)
         mat(563) = rxt(69)
         mat(457) = rxt(71)
         mat(390) = rxt(74)
         mat(249) = .450_r8*rxt(76)
         mat(315) = 2.000_r8*rxt(77)
         mat(93) = rxt(82)
         mat(790) = rxt(199)*y(99)

         mat(83) = -( rxt(134) + het_rates(5) )
         mat(1136) = rxt(5)

         mat(1184) = -( rxt(5) + het_rates(6) )
         mat(841) = rxt(6) + .500_r8*rxt(343)
         mat(119) = rxt(8)
         mat(1087) = rxt(11)
         mat(86) = rxt(134)
         mat(1107) = 2.000_r8*rxt(89)*y(4)

         mat(833) = -( rxt(6) + rxt(343) + het_rates(7) )
         mat(116) = rxt(7) + rxt(146)
         mat(416) = rxt(9)
         mat(1079) = rxt(10)
         mat(191) = rxt(13) + rxt(155)
         mat(449) = rxt(28)
         mat(265) = rxt(33)
         mat(225) = .600_r8*rxt(59) + rxt(254)
         mat(273) = rxt(60) + rxt(300)
         mat(460) = rxt(71)

         mat(950) = -( rxt(200)*y(99) + rxt(201)*y(106) + rxt(202)*y(104) &
                      + rxt(203)*y(100) + rxt(205)*y(109) + rxt(206)*y(110) &
                      + rxt(207)*y(116) + rxt(208)*y(115) + rxt(211)*y(12) + het_rates(20) &
       )
         mat(417) = rxt(9)
         mat(192) = rxt(12)
         mat(200) = rxt(14)
         mat(698) = rxt(17)
         mat(301) = 2.000_r8*rxt(20)
         mat(440) = rxt(25)
         mat(384) = rxt(31)
         mat(283) = rxt(57)
         mat(233) = rxt(58)
         mat(134) = rxt(64)
         mat(64) = rxt(65)
         mat(167) = rxt(66)
         mat(174) = rxt(67)
         mat(139) = rxt(70)
         mat(327) = rxt(78)
         mat(151) = rxt(79)
         mat(188) = rxt(80)
         mat(219) = rxt(81)
         mat(835) = .500_r8*rxt(343)
         mat(1101) = rxt(108)*y(12)

         mat(1084) = -( rxt(10) + rxt(11) + rxt(342) + het_rates(8) )
         mat(117) = rxt(7) + rxt(8) + rxt(146)
         mat(194) = rxt(12)
         mat(453) = rxt(27)
         mat(266) = rxt(32)
         mat(227) = .400_r8*rxt(59)

         mat(414) = -( rxt(9) + het_rates(9) )
         mat(115) = 2.000_r8*rxt(341) + 2.000_r8*rxt(352) + 2.000_r8*rxt(358) &
                      + 2.000_r8*rxt(363)
         mat(1056) = rxt(342)
         mat(820) = .500_r8*rxt(343)
         mat(444) = rxt(353) + rxt(359) + rxt(364)
         mat(263) = rxt(354) + rxt(362) + rxt(365)

         mat(189) = -( rxt(12) + rxt(13) + rxt(155) + het_rates(10) )

         mat(114) = -( rxt(7) + rxt(8) + rxt(146) + rxt(341) + rxt(352) + rxt(358) &
                      + rxt(363) + het_rates(11) )

         mat(736) = -( het_rates(13) )
         mat(545) = rxt(56)
         mat(231) = rxt(58)
         mat(223) = .400_r8*rxt(59)
         mat(600) = .300_r8*rxt(63)
         mat(356) = rxt(68)
         mat(1095) = rxt(108)*y(12)
         mat(795) = rxt(162)*y(12)
         mat(944) = rxt(211)*y(12)

         mat(195) = -( rxt(14) + het_rates(14) )

         mat(87) = -( het_rates(39) )

         mat(44) = -( het_rates(40) )

         mat(771) = -( rxt(15) + rxt(16) + het_rates(16) )
         mat(199) = rxt(14)
         mat(282) = rxt(57)
         mat(558) = 1.340_r8*rxt(61)
         mat(173) = rxt(67)
         mat(459) = rxt(71)
         mat(258) = .690_r8*rxt(72)
         mat(536) = rxt(73)
         mat(391) = rxt(74)
         mat(326) = .100_r8*rxt(78)
         mat(209) = rxt(225)
         mat(112) = 2.000_r8*rxt(235)
         mat(1097) = rxt(109)*y(12) + rxt(110)*y(12)

         mat(704) = -( rxt(115) + het_rates(19) )
         mat(197) = rxt(14)
         mat(769) = 2.000_r8*rxt(15)
         mat(696) = rxt(17) + 2.000_r8*rxt(19)
         mat(1033) = rxt(26)
         mat(396) = rxt(34)
         mat(1094) = rxt(109)*y(12)

         mat(1018) = -( rxt(349) + het_rates(21) )
         mat(193) = rxt(13) + rxt(155)
         mat(548) = rxt(56)
         mat(284) = rxt(57)
         mat(560) = 1.340_r8*rxt(61) + .660_r8*rxt(62)
         mat(135) = rxt(64)
         mat(168) = rxt(66)
         mat(570) = rxt(69)
         mat(462) = rxt(71)
         mat(260) = rxt(72)
         mat(538) = rxt(73)
         mat(393) = 2.000_r8*rxt(74)
         mat(252) = .560_r8*rxt(76)
         mat(317) = 2.000_r8*rxt(77)
         mat(328) = .900_r8*rxt(78)
         mat(220) = rxt(81)
         mat(707) = rxt(115)
         mat(212) = rxt(225)
         mat(113) = rxt(234) + rxt(235)
         mat(1102) = rxt(109)*y(12)
         mat(802) = rxt(199)*y(99) + rxt(204)*y(100)
         mat(951) = rxt(200)*y(99) + rxt(203)*y(100)

         mat(298) = -( rxt(20) + het_rates(22) )
         mat(980) = .500_r8*rxt(349)

         mat(695) = -( rxt(17) + rxt(18) + rxt(19) + het_rates(161) )
         mat(942) = rxt(200)*y(99) + rxt(201)*y(106) + rxt(202)*y(104) + rxt(203)*y(100) &
                      + rxt(207)*y(116) + rxt(211)*y(12)

         mat(798) = -( rxt(162)*y(12) + rxt(199)*y(99) + rxt(204)*y(100) + rxt(209)*y(116) &
                      + rxt(210)*y(115) + het_rates(25) )
         mat(78) = 2.000_r8*rxt(21)
         mat(855) = rxt(22)
         mat(48) = 2.000_r8*rxt(24)
         mat(438) = rxt(25)
         mat(1036) = rxt(26)
         mat(448) = rxt(27)
         mat(105) = rxt(29)
         mat(1098) = 3.000_r8*rxt(92)*y(101) + 2.000_r8*rxt(93)*y(102) &
                      + 3.000_r8*rxt(94)*y(103) + 2.000_r8*rxt(95)*y(111) + rxt(96)*y(112) &
                      + rxt(97)*y(104) + 2.000_r8*rxt(98)*y(109) + rxt(99)*y(110) &
                      + 4.000_r8*rxt(100)*y(105) + rxt(102)*y(108)
         mat(947) = rxt(200)*y(99) + 3.000_r8*rxt(201)*y(106) + rxt(202)*y(104) &
                      + 2.000_r8*rxt(205)*y(109) + rxt(206)*y(110)

         mat(77) = -( rxt(21) + het_rates(26) )

         mat(857) = -( rxt(22) + het_rates(27) )
         mat(69) = rxt(23)
         mat(450) = rxt(28)
         mat(49) = 2.000_r8*rxt(174)

         mat(68) = -( rxt(23) + het_rates(28) )

         mat(47) = -( rxt(24) + rxt(174) + het_rates(29) )

         mat(1041) = -( rxt(26) + het_rates(30) )
         mat(803) = rxt(162)*y(12) + 2.000_r8*rxt(199)*y(99) + rxt(204)*y(100) &
                      + rxt(209)*y(116) + rxt(210)*y(115)

         mat(436) = -( rxt(25) + het_rates(31) )
         mat(445) = rxt(353) + rxt(359) + rxt(364)

         mat(446) = -( rxt(27) + rxt(28) + rxt(353) + rxt(359) + rxt(364) + het_rates(32) &
       )

         mat(103) = -( rxt(29) + het_rates(33) )

         mat(753) = -( het_rates(34) )
         mat(104) = rxt(29)
         mat(1194) = rxt(30)
         mat(382) = rxt(31)
         mat(264) = rxt(32)
         mat(397) = rxt(34)
         mat(1096) = rxt(101)*y(100) + rxt(102)*y(108) + rxt(103)*y(107) &
                      + 2.000_r8*rxt(104)*y(113) + 2.000_r8*rxt(105)*y(114) &
                      + 3.000_r8*rxt(106)*y(115) + 2.000_r8*rxt(107)*y(116)
         mat(945) = rxt(203)*y(100) + 2.000_r8*rxt(207)*y(116) + 3.000_r8*rxt(208)*y(115)
         mat(796) = rxt(204)*y(100) + 2.000_r8*rxt(209)*y(116) + 3.000_r8*rxt(210)*y(115)

         mat(1206) = -( rxt(30) + het_rates(35) )
         mat(268) = rxt(33)

         mat(394) = -( rxt(34) + het_rates(36) )

         mat(380) = -( rxt(31) + het_rates(37) )
         mat(262) = rxt(354) + rxt(362) + rxt(365)


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

         mat(261) = -( rxt(32) + rxt(33) + rxt(354) + rxt(362) + rxt(365) + het_rates(38) &
       )

         mat(480) = -( het_rates(56) )
         mat(590) = .700_r8*rxt(63)

         mat(420) = -( het_rates(80) )

         mat(359) = -( het_rates(61) )

         mat(541) = -( rxt(56) + het_rates(47) )
         mat(280) = rxt(57)
         mat(133) = rxt(64)
         mat(324) = .400_r8*rxt(78)
         mat(149) = rxt(79)

         mat(294) = -( het_rates(46) )

         mat(277) = -( rxt(57) + het_rates(62) )

         mat(683) = -( het_rates(45) )
         mat(222) = .600_r8*rxt(59) + rxt(254)
         mat(555) = 1.340_r8*rxt(61)
         mat(597) = .300_r8*rxt(63)
         mat(171) = rxt(67)
         mat(354) = rxt(68)
         mat(565) = rxt(69)
         mat(535) = rxt(73)
         mat(204) = rxt(75)
         mat(251) = .130_r8*rxt(76)
         mat(150) = rxt(79)

         mat(228) = -( rxt(58) + het_rates(51) )

         mat(221) = -( rxt(59) + rxt(254) + het_rates(55) )

         mat(175) = -( het_rates(79) )

         mat(120) = -( het_rates(42) )

         mat(155) = -( het_rates(41) )

         mat(35) = -( het_rates(68) )

         mat(269) = -( rxt(60) + rxt(300) + het_rates(78) )

         mat(38) = -( het_rates(67) )

         mat(140) = -( het_rates(70) )

         mat(341) = -( het_rates(81) )

         mat(319) = -( rxt(78) + het_rates(82) )

         mat(201) = -( rxt(75) + het_rates(69) )
         mat(318) = .800_r8*rxt(78)

         mat(330) = -( het_rates(71) )

         mat(147) = -( rxt(79) + het_rates(72) )

         mat(54) = -( het_rates(91) )

         mat(59) = -( het_rates(92) )

         mat(239) = -( het_rates(93) )

         mat(183) = -( rxt(80) + het_rates(94) )

         mat(79) = -( het_rates(95) )

         mat(466) = -( het_rates(97) )

         mat(214) = -( rxt(81) + het_rates(98) )

         mat(247) = -( rxt(76) + het_rates(83) )
         mat(185) = .900_r8*rxt(80)

         mat(314) = -( rxt(77) + het_rates(50) )
         mat(248) = .130_r8*rxt(76)
         mat(186) = .450_r8*rxt(80)

         mat(638) = -( het_rates(85) )

         mat(595) = -( rxt(63) + het_rates(74) )
         mat(256) = .402_r8*rxt(72)
         mat(218) = rxt(81)

         mat(551) = -( rxt(61) + rxt(62) + het_rates(75) )
         mat(254) = .288_r8*rxt(72)
         mat(217) = rxt(81)

         mat(616) = -( het_rates(76) )

         mat(126) = -( het_rates(77) )

         mat(656) = -( het_rates(73) )
         mat(271) = rxt(60) + rxt(300)
         mat(554) = .660_r8*rxt(61)

         mat(371) = -( het_rates(43) )
         mat(203) = rxt(75)

         mat(131) = -( rxt(64) + het_rates(44) )

         mat(305) = -( het_rates(96) )

         mat(50) = -( het_rates(57) )

         mat(403) = -( het_rates(58) )

         mat(163) = -( rxt(66) + het_rates(59) )

         mat(352) = -( rxt(68) + het_rates(60) )
         mat(164) = .820_r8*rxt(66)
         mat(322) = .250_r8*rxt(78)
         mat(215) = .100_r8*rxt(81)

         mat(169) = -( rxt(67) + het_rates(66) )

         mat(234) = -( het_rates(15) )

         mat(106) = -( het_rates(48) )

         mat(389) = -( rxt(74) + het_rates(49) )
         mat(111) = rxt(234)

         mat(533) = -( rxt(73) + het_rates(63) )

         mat(287) = -( het_rates(52) )

         mat(110) = -( rxt(234) + rxt(235) + het_rates(53) )
         mat(63) = rxt(65)

         mat(62) = -( rxt(65) + het_rates(54) )

         mat(152) = -( het_rates(84) )

         mat(522) = -( het_rates(64) )

         mat(564) = -( rxt(69) + het_rates(65) )
         mat(250) = .180_r8*rxt(76)
         mat(187) = .450_r8*rxt(80)

         mat(496) = -( het_rates(86) )

         mat(456) = -( rxt(71) + het_rates(87) )

         mat(579) = -( het_rates(88) )

         mat(136) = -( rxt(70) + het_rates(89) )

         mat(253) = -( rxt(72) + het_rates(90) )

         mat(71) = -( het_rates(118) )

         mat(179) = -( het_rates(119) )

         mat(207) = -( rxt(225) + het_rates(120) )

         mat(92) = -( rxt(82) + het_rates(121) )

         mat(66) = -( het_rates(122) )

         mat(98) = -( het_rates(123) )

         mat(41) = -( het_rates(124) )

         mat(1) = -( het_rates(129) )

         mat(2) = -( het_rates(127) )

         mat(3) = -( het_rates(128) )

         mat(4) = -( het_rates(130) )

         mat(5) = -( het_rates(131) )

         mat(6) = -( het_rates(132) )

         mat(7) = -( het_rates(133) )

         mat(8) = -( het_rates(134) )

         mat(9) = -( het_rates(135) )

         mat(10) = -( het_rates(136) )

         mat(11) = -( het_rates(137) )

         mat(12) = -( het_rates(138) )

         mat(13) = -( het_rates(139) )

         mat(14) = -( het_rates(140) )

         mat(15) = -( het_rates(141) )

         mat(16) = -( het_rates(142) )

         mat(17) = -( het_rates(143) )

         mat(18) = -( het_rates(144) )

         mat(19) = -( het_rates(145) )

         mat(20) = -( het_rates(146) )

         mat(21) = -( het_rates(147) )

         mat(22) = -( het_rates(148) )

         mat(23) = -( het_rates(149) )

         mat(24) = -( het_rates(150) )

         mat(25) = -( het_rates(151) )

         mat(26) = -( het_rates(152) )

         mat(27) = -( het_rates(153) )

         mat(28) = -( het_rates(154) )

         mat(29) = -( het_rates(155) )

         mat(30) = -( het_rates(156) )

         mat(31) = -( het_rates(157) )

         mat(32) = -( het_rates(158) )

         mat(33) = -( het_rates(159) )

         mat(34) = -( het_rates(160) )


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
