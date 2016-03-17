




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

         mat(832) = -( rxt(2) + rxt(3) + het_rates(1) )
         mat(1172) = rxt(85)

         mat(1180) = -( rxt(85) + het_rates(2) )
         mat(840) = rxt(3)
         mat(1274) = rxt(5)
         mat(874) = rxt(6)
         mat(129) = rxt(8)
         mat(935) = rxt(10)
         mat(717) = rxt(19)
         mat(1203) = rxt(22)
         mat(70) = rxt(23)
         mat(781) = rxt(30)
         mat(893) = rxt(88) + rxt(89)
         mat(80) = rxt(136)

         mat(887) = -( rxt(88) + rxt(89) + rxt(91)*y(4) + rxt(92)*y(4) + rxt(94)*y(107) &
                      + rxt(95)*y(108) + rxt(96)*y(109) + rxt(97)*y(117) + rxt(98)*y(118) &
                      + rxt(99)*y(110) + rxt(100)*y(115) + rxt(101)*y(116) &
                      + rxt(102)*y(111) + rxt(103)*y(106) + rxt(104)*y(114) &
                      + rxt(105)*y(113) + rxt(106)*y(119) + rxt(107)*y(120) &
                      + rxt(108)*y(121) + rxt(109)*y(122) + rxt(110)*y(12) &
                      + rxt(111)*y(12) + rxt(112)*y(12) + het_rates(3) )
         mat(834) = rxt(2)
         mat(713) = rxt(18)

         mat(521) = -( het_rates(18) )
         mat(789) = rxt(16)
         mat(710) = rxt(18)
         mat(880) = rxt(112)*y(12)

         mat(529) = -( het_rates(17) )
         mat(790) = rxt(15) + rxt(16)
         mat(547) = rxt(56)
         mat(563) = 1.340_r8*rxt(62)
         mat(604) = .700_r8*rxt(63)
         mat(576) = rxt(69)
         mat(484) = rxt(71)
         mat(447) = rxt(74)
         mat(295) = .450_r8*rxt(76)
         mat(379) = 2.000_r8*rxt(77)
         mat(91) = rxt(84)
         mat(1139) = rxt(201)*y(105)

         mat(78) = -( rxt(136) + het_rates(5) )
         mat(1225) = rxt(5)

         mat(1277) = -( rxt(5) + het_rates(6) )
         mat(877) = rxt(6) + .500_r8*rxt(352)
         mat(130) = rxt(8)
         mat(938) = rxt(11)
         mat(81) = rxt(136)
         mat(896) = 2.000_r8*rxt(91)*y(4)

         mat(867) = -( rxt(6) + rxt(352) + het_rates(7) )
         mat(127) = rxt(7) + rxt(148)
         mat(421) = rxt(9)
         mat(928) = rxt(10)
         mat(193) = rxt(13) + rxt(157)
         mat(456) = rxt(28)
         mat(264) = rxt(34)
         mat(212) = .600_r8*rxt(59) + rxt(258)
         mat(272) = rxt(60) + rxt(304)
         mat(487) = rxt(71)

         mat(1120) = -( rxt(202)*y(105) + rxt(203)*y(112) + rxt(204)*y(110) &
                      + rxt(205)*y(106) + rxt(207)*y(115) + rxt(208)*y(116) &
                      + rxt(209)*y(122) + rxt(210)*y(121) + rxt(213)*y(12) + het_rates(20) &
       )
         mat(423) = rxt(9)
         mat(196) = rxt(12)
         mat(202) = rxt(14)
         mat(715) = rxt(17)
         mat(312) = 2.000_r8*rxt(20)
         mat(441) = rxt(25)
         mat(398) = rxt(31)
         mat(243) = rxt(57)
         mat(235) = rxt(58)
         mat(145) = rxt(64)
         mat(65) = rxt(65)
         mat(174) = rxt(66)
         mat(180) = rxt(67)
         mat(112) = rxt(70)
         mat(329) = rxt(78)
         mat(135) = rxt(79)
         mat(161) = rxt(80)
         mat(229) = rxt(81)
         mat(872) = .500_r8*rxt(352)
         mat(891) = rxt(110)*y(12)

         mat(930) = -( rxt(10) + rxt(11) + rxt(351) + het_rates(8) )
         mat(128) = rxt(7) + rxt(8) + rxt(148)
         mat(194) = rxt(12)
         mat(457) = rxt(27)
         mat(265) = rxt(33)
         mat(213) = .400_r8*rxt(59)

         mat(419) = -( rxt(9) + het_rates(9) )
         mat(126) = 2.000_r8*rxt(350) + 2.000_r8*rxt(384) + 2.000_r8*rxt(390) &
                      + 2.000_r8*rxt(395)
         mat(905) = rxt(351)
         mat(854) = .500_r8*rxt(352)
         mat(452) = rxt(385) + rxt(391) + rxt(396)
         mat(262) = rxt(386) + rxt(394) + rxt(397)

         mat(191) = -( rxt(12) + rxt(13) + rxt(157) + het_rates(10) )

         mat(125) = -( rxt(7) + rxt(8) + rxt(148) + rxt(350) + rxt(384) + rxt(390) &
                      + rxt(395) + het_rates(11) )

         mat(752) = -( het_rates(13) )
         mat(552) = rxt(56)
         mat(233) = rxt(58)
         mat(210) = .400_r8*rxt(59)
         mat(613) = .300_r8*rxt(63)
         mat(366) = rxt(68)
         mat(883) = rxt(110)*y(12)
         mat(1144) = rxt(164)*y(12)
         mat(1111) = rxt(213)*y(12)

         mat(197) = -( rxt(14) + het_rates(14) )

         mat(82) = -( het_rates(39) )

         mat(45) = -( het_rates(40) )

         mat(793) = -( rxt(15) + rxt(16) + het_rates(16) )
         mat(201) = rxt(14)
         mat(241) = rxt(57)
         mat(571) = 1.340_r8*rxt(61)
         mat(179) = rxt(67)
         mat(486) = rxt(71)
         mat(281) = .690_r8*rxt(72)
         mat(560) = rxt(73)
         mat(448) = rxt(74)
         mat(327) = .100_r8*rxt(78)
         mat(183) = rxt(227)
         mat(107) = 2.000_r8*rxt(239)
         mat(884) = rxt(111)*y(12) + rxt(112)*y(12)

         mat(720) = -( rxt(117) + het_rates(19) )
         mat(199) = rxt(14)
         mat(792) = 2.000_r8*rxt(15)
         mat(712) = rxt(17) + 2.000_r8*rxt(19)
         mat(1017) = rxt(26)
         mat(414) = rxt(32)
         mat(882) = rxt(111)*y(12)

         mat(1002) = -( rxt(360) + het_rates(21) )
         mat(195) = rxt(13) + rxt(157)
         mat(555) = rxt(56)
         mat(242) = rxt(57)
         mat(573) = 1.340_r8*rxt(61) + .660_r8*rxt(62)
         mat(144) = rxt(64)
         mat(173) = rxt(66)
         mat(583) = rxt(69)
         mat(489) = rxt(71)
         mat(282) = rxt(72)
         mat(561) = rxt(73)
         mat(449) = 2.000_r8*rxt(74)
         mat(298) = .560_r8*rxt(76)
         mat(380) = 2.000_r8*rxt(77)
         mat(328) = .900_r8*rxt(78)
         mat(228) = rxt(81)
         mat(723) = rxt(117)
         mat(185) = rxt(227)
         mat(108) = rxt(238) + rxt(239)
         mat(889) = rxt(111)*y(12)
         mat(1150) = rxt(201)*y(105) + rxt(206)*y(106)
         mat(1118) = rxt(202)*y(105) + rxt(205)*y(106)

         mat(308) = -( rxt(20) + het_rates(22) )
         mat(962) = .500_r8*rxt(360)

         mat(711) = -( rxt(17) + rxt(18) + rxt(19) + het_rates(124) )
         mat(1109) = rxt(202)*y(105) + rxt(203)*y(112) + rxt(204)*y(110) + rxt(205)*y(106) &
                      + rxt(209)*y(122) + rxt(213)*y(12)

         mat(1153) = -( rxt(164)*y(12) + rxt(201)*y(105) + rxt(206)*y(106) &
                      + rxt(211)*y(122) + rxt(212)*y(121) + het_rates(25) )
         mat(73) = 2.000_r8*rxt(21)
         mat(1202) = rxt(22)
         mat(49) = 2.000_r8*rxt(24)
         mat(442) = rxt(25)
         mat(1026) = rxt(26)
         mat(460) = rxt(27)
         mat(87) = rxt(29)
         mat(892) = 3.000_r8*rxt(94)*y(107) + 2.000_r8*rxt(95)*y(108) &
                      + 3.000_r8*rxt(96)*y(109) + 2.000_r8*rxt(97)*y(117) + rxt(98)*y(118) &
                      + rxt(99)*y(110) + 2.000_r8*rxt(100)*y(115) + rxt(101)*y(116) &
                      + 4.000_r8*rxt(102)*y(111) + rxt(104)*y(114)
         mat(1121) = rxt(202)*y(105) + 3.000_r8*rxt(203)*y(112) + rxt(204)*y(110) &
                      + 2.000_r8*rxt(207)*y(115) + rxt(208)*y(116)

         mat(72) = -( rxt(21) + het_rates(26) )

         mat(1204) = -( rxt(22) + het_rates(27) )
         mat(71) = rxt(23)
         mat(462) = rxt(28)
         mat(50) = 2.000_r8*rxt(176)

         mat(69) = -( rxt(23) + het_rates(28) )

         mat(48) = -( rxt(24) + rxt(176) + het_rates(29) )

         mat(1024) = -( rxt(26) + het_rates(30) )
         mat(1151) = rxt(164)*y(12) + 2.000_r8*rxt(201)*y(105) + rxt(206)*y(106) &
                      + rxt(211)*y(122) + rxt(212)*y(121)

         mat(438) = -( rxt(25) + het_rates(31) )
         mat(453) = rxt(385) + rxt(391) + rxt(396)

         mat(454) = -( rxt(27) + rxt(28) + rxt(385) + rxt(391) + rxt(396) + het_rates(32) &
       )

         mat(86) = -( rxt(29) + het_rates(33) )

         mat(1223) = -( het_rates(34) )
         mat(88) = rxt(29)
         mat(783) = rxt(30)
         mat(401) = rxt(31)
         mat(418) = rxt(32)
         mat(267) = rxt(33)
         mat(895) = rxt(103)*y(106) + rxt(104)*y(114) + rxt(105)*y(113) &
                      + 2.000_r8*rxt(106)*y(119) + 2.000_r8*rxt(107)*y(120) &
                      + 3.000_r8*rxt(108)*y(121) + 2.000_r8*rxt(109)*y(122)
         mat(1124) = rxt(205)*y(106) + 2.000_r8*rxt(209)*y(122) + 3.000_r8*rxt(210)*y(121)
         mat(1156) = rxt(206)*y(106) + 2.000_r8*rxt(211)*y(122) + 3.000_r8*rxt(212)*y(121)

         mat(772) = -( rxt(30) + het_rates(35) )
         mat(263) = rxt(34)

         mat(412) = -( rxt(32) + het_rates(36) )

         mat(394) = -( rxt(31) + het_rates(37) )
         mat(261) = rxt(386) + rxt(394) + rxt(397)


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

         mat(260) = -( rxt(33) + rxt(34) + rxt(386) + rxt(394) + rxt(397) + het_rates(38) &
       )

         mat(507) = -( het_rates(56) )
         mat(603) = .700_r8*rxt(63)

         mat(465) = -( het_rates(80) )

         mat(369) = -( het_rates(61) )

         mat(548) = -( rxt(56) + het_rates(47) )
         mat(238) = rxt(57)
         mat(143) = rxt(64)
         mat(325) = .400_r8*rxt(78)
         mat(133) = rxt(79)

         mat(315) = -( het_rates(46) )

         mat(236) = -( rxt(57) + het_rates(62) )

         mat(699) = -( het_rates(45) )
         mat(209) = .600_r8*rxt(59) + rxt(258)
         mat(568) = 1.340_r8*rxt(61)
         mat(610) = .300_r8*rxt(63)
         mat(177) = rxt(67)
         mat(364) = rxt(68)
         mat(578) = rxt(69)
         mat(559) = rxt(73)
         mat(206) = rxt(75)
         mat(297) = .130_r8*rxt(76)
         mat(134) = rxt(79)

         mat(230) = -( rxt(58) + het_rates(51) )

         mat(208) = -( rxt(59) + rxt(258) + het_rates(55) )

         mat(187) = -( het_rates(79) )

         mat(119) = -( het_rates(42) )

         mat(248) = -( het_rates(41) )

         mat(30) = -( het_rates(68) )

         mat(268) = -( rxt(60) + rxt(304) + het_rates(78) )

         mat(33) = -( het_rates(67) )

         mat(146) = -( het_rates(70) )

         mat(351) = -( het_rates(81) )

         mat(320) = -( rxt(78) + het_rates(82) )

         mat(203) = -( rxt(75) + het_rates(69) )
         mat(319) = .800_r8*rxt(78)

         mat(331) = -( het_rates(71) )

         mat(131) = -( rxt(79) + het_rates(72) )

         mat(55) = -( het_rates(91) )

         mat(60) = -( het_rates(92) )

         mat(285) = -( het_rates(93) )

         mat(156) = -( rxt(80) + het_rates(94) )

         mat(74) = -( het_rates(95) )

         mat(493) = -( het_rates(103) )

         mat(223) = -( rxt(81) + het_rates(104) )

         mat(293) = -( rxt(76) + het_rates(83) )
         mat(158) = .900_r8*rxt(80)

         mat(378) = -( rxt(77) + het_rates(50) )
         mat(294) = .130_r8*rxt(76)
         mat(159) = .450_r8*rxt(80)

         mat(36) = -( het_rates(96) )

         mat(163) = -( het_rates(97) )

         mat(1) = -( het_rates(98) )

         mat(39) = -( het_rates(99) )

         mat(216) = -( het_rates(100) )

         mat(2) = -( het_rates(101) )

         mat(654) = -( het_rates(85) )

         mat(608) = -( rxt(63) + het_rates(74) )
         mat(279) = .402_r8*rxt(72)
         mat(227) = rxt(81)

         mat(564) = -( rxt(61) + rxt(62) + het_rates(75) )
         mat(277) = .288_r8*rxt(72)
         mat(226) = rxt(81)

         mat(629) = -( het_rates(76) )

         mat(136) = -( het_rates(77) )

         mat(672) = -( het_rates(73) )
         mat(270) = rxt(60) + rxt(304)
         mat(567) = .660_r8*rxt(61)

         mat(385) = -( het_rates(43) )
         mat(205) = rxt(75)

         mat(141) = -( rxt(64) + het_rates(44) )

         mat(299) = -( het_rates(102) )

         mat(51) = -( het_rates(57) )

         mat(426) = -( het_rates(58) )

         mat(169) = -( rxt(66) + het_rates(59) )

         mat(362) = -( rxt(68) + het_rates(60) )
         mat(170) = .820_r8*rxt(66)
         mat(323) = .250_r8*rxt(78)
         mat(224) = .100_r8*rxt(81)

         mat(175) = -( rxt(67) + het_rates(66) )

         mat(244) = -( het_rates(15) )

         mat(101) = -( het_rates(48) )

         mat(446) = -( rxt(74) + het_rates(49) )
         mat(106) = rxt(238)

         mat(557) = -( rxt(73) + het_rates(63) )

         mat(340) = -( het_rates(52) )

         mat(105) = -( rxt(238) + rxt(239) + het_rates(53) )
         mat(64) = rxt(65)

         mat(63) = -( rxt(65) + het_rates(54) )

         mat(153) = -( het_rates(84) )

         mat(535) = -( het_rates(64) )

         mat(577) = -( rxt(69) + het_rates(65) )
         mat(296) = .180_r8*rxt(76)
         mat(160) = .450_r8*rxt(80)

         mat(402) = -( het_rates(86) )

         mat(483) = -( rxt(71) + het_rates(87) )

         mat(592) = -( het_rates(88) )

         mat(109) = -( rxt(70) + het_rates(89) )

         mat(276) = -( rxt(72) + het_rates(90) )

         mat(113) = -( het_rates(125) )

         mat(256) = -( het_rates(126) )

         mat(181) = -( rxt(227) + het_rates(127) )

         mat(90) = -( rxt(84) + het_rates(142) )

         mat(67) = -( het_rates(143) )

         mat(96) = -( het_rates(144) )

         mat(3) = -( rxt(367) + het_rates(145) )

         mat(42) = -( het_rates(146) )

         mat(4) = -( rxt(373) + het_rates(147) )

         mat(5) = -( rxt(374) + het_rates(148) )

         mat(6) = -( rxt(368) + het_rates(132) )

         mat(7) = -( rxt(369) + het_rates(133) )

         mat(8) = -( rxt(371) + het_rates(134) )

         mat(9) = -( rxt(370) + het_rates(135) )

         mat(10) = -( rxt(372) + het_rates(136) )

         mat(11) = -( het_rates(137) )

         mat(12) = -( het_rates(138) )

         mat(13) = -( het_rates(139) )

         mat(14) = -( het_rates(140) )

         mat(15) = -( het_rates(141) )

         mat(16) = -( rxt(353) + rxt(363) + het_rates(128) )

         mat(18) = -( rxt(364) + het_rates(129) )
         mat(17) = rxt(353)

         mat(19) = -( rxt(359) + rxt(365) + het_rates(130) )

         mat(21) = -( rxt(366) + het_rates(131) )
         mat(20) = rxt(359)

         mat(22) = -( rxt(375) + het_rates(149) )

         mat(23) = -( rxt(376) + het_rates(150) )

         mat(24) = -( rxt(377) + het_rates(151) )

         mat(25) = -( rxt(378) + het_rates(152) )

         mat(26) = -( rxt(379) + het_rates(153) )

         mat(27) = -( rxt(380) + het_rates(154) )

         mat(28) = -( rxt(381) + het_rates(155) )

         mat(29) = -( rxt(382) + het_rates(156) )


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
