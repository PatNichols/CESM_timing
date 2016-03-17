




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

         mat(2008) = -( rxt(2) + rxt(3) + het_rates(1) )
         mat(1704) = rxt(137)

         mat(1698) = -( rxt(137) + het_rates(2) )
         mat(2002) = rxt(3)
         mat(1779) = rxt(5)
         mat(2041) = rxt(6)
         mat(209) = rxt(8)
         mat(1673) = rxt(10)
         mat(1325) = rxt(19)
         mat(1940) = rxt(22)
         mat(101) = rxt(23)
         mat(1400) = rxt(30)
         mat(1896) = rxt(140) + rxt(141)
         mat(126) = rxt(188)

         mat(1899) = -( rxt(140) + rxt(141) + rxt(143)*y(4) + rxt(144)*y(4) &
                      + rxt(146)*y(161) + rxt(147)*y(162) + rxt(148)*y(163) &
                      + rxt(149)*y(171) + rxt(150)*y(172) + rxt(151)*y(164) &
                      + rxt(152)*y(169) + rxt(153)*y(170) + rxt(154)*y(165) &
                      + rxt(155)*y(160) + rxt(156)*y(168) + rxt(157)*y(167) &
                      + rxt(158)*y(173) + rxt(159)*y(174) + rxt(160)*y(175) &
                      + rxt(161)*y(176) + rxt(162)*y(12) + rxt(163)*y(12) + rxt(164)*y(12) &
                 + het_rates(3) )
         mat(2005) = rxt(2)
         mat(1326) = rxt(18)

         mat(979) = -( het_rates(20) )
         mat(1413) = rxt(16)
         mat(1320) = rxt(18)
         mat(1888) = rxt(164)*y(12)

         mat(1060) = -( het_rates(17) )
         mat(1414) = rxt(15) + rxt(16)
         mat(1078) = rxt(56)
         mat(1138) = 1.340_r8*rxt(62)
         mat(1205) = .700_r8*rxt(63)
         mat(1151) = rxt(69)
         mat(1097) = .330_r8*rxt(78)
         mat(1023) = rxt(81)
         mat(552) = .450_r8*rxt(83)
         mat(838) = 2.000_r8*rxt(84)
         mat(484) = .250_r8*rxt(89)
         mat(950) = rxt(90)
         mat(1004) = 1.700_r8*rxt(91)
         mat(728) = rxt(93)
         mat(116) = 1.500_r8*rxt(95)
         mat(151) = 1.500_r8*rxt(96)
         mat(490) = .600_r8*rxt(98)
         mat(257) = rxt(99)
         mat(1442) = rxt(253)*y(159)

         mat(125) = -( rxt(188) + het_rates(5) )
         mat(1708) = rxt(5)

         mat(1780) = -( rxt(5) + het_rates(6) )
         mat(2042) = rxt(6) + .500_r8*rxt(513)
         mat(210) = rxt(8)
         mat(1674) = rxt(11)
         mat(127) = rxt(188)
         mat(1897) = 2.000_r8*rxt(143)*y(4)

         mat(2048) = -( rxt(6) + rxt(513) + het_rates(7) )
         mat(211) = rxt(7) + rxt(200)
         mat(705) = rxt(9)
         mat(1680) = rxt(10)
         mat(325) = rxt(13) + rxt(209)
         mat(784) = rxt(28)
         mat(433) = rxt(34)
         mat(382) = .600_r8*rxt(59) + rxt(310)
         mat(462) = rxt(60) + rxt(358)
         mat(99) = rxt(71)
         mat(517) = rxt(72)
         mat(344) = rxt(73)
         mat(741) = rxt(74)
         mat(413) = rxt(75)
         mat(270) = rxt(76)
         mat(772) = rxt(77)
         mat(1112) = rxt(78)
         mat(91) = rxt(438)

         mat(1607) = -( rxt(254)*y(159) + rxt(255)*y(166) + rxt(256)*y(164) &
                      + rxt(257)*y(160) + rxt(259)*y(169) + rxt(260)*y(170) &
                      + rxt(261)*y(176) + rxt(262)*y(175) + rxt(265)*y(12) + het_rates(22) &
       )
         mat(703) = rxt(9)
         mat(322) = rxt(12)
         mat(331) = rxt(14)
         mat(1324) = rxt(17)
         mat(495) = 2.000_r8*rxt(20)
         mat(710) = rxt(25)
         mat(660) = rxt(31)
         mat(445) = rxt(57)
         mat(401) = rxt(58)
         mat(240) = rxt(64)
         mat(97) = rxt(65)
         mat(292) = rxt(66)
         mat(299) = rxt(67)
         mat(201) = rxt(70)
         mat(269) = rxt(76)
         mat(540) = rxt(85)
         mat(249) = rxt(86)
         mat(529) = rxt(87)
         mat(361) = rxt(88)
         mat(486) = rxt(89)
         mat(425) = rxt(92)
         mat(179) = rxt(100)
         mat(230) = rxt(101)
         mat(205) = rxt(102)
         mat(286) = rxt(103)
         mat(254) = rxt(104)
         mat(304) = rxt(105)
         mat(626) = rxt(106)
         mat(2039) = .500_r8*rxt(513)
         mat(1894) = rxt(162)*y(12)

         mat(1672) = -( rxt(10) + rxt(11) + rxt(512) + het_rates(8) )
         mat(208) = rxt(7) + rxt(8) + rxt(200)
         mat(323) = rxt(12)
         mat(780) = rxt(27)
         mat(431) = rxt(33)
         mat(381) = .400_r8*rxt(59)

         mat(701) = -( rxt(9) + het_rates(9) )
         mat(207) = 2.000_r8*rxt(511) + 2.000_r8*rxt(519) + 2.000_r8*rxt(525) &
                      + 2.000_r8*rxt(530)
         mat(1628) = rxt(512)
         mat(2024) = .500_r8*rxt(513)
         mat(774) = rxt(520) + rxt(526) + rxt(531)
         mat(429) = rxt(521) + rxt(529) + rxt(532)

         mat(320) = -( rxt(12) + rxt(13) + rxt(209) + het_rates(10) )

         mat(206) = -( rxt(7) + rxt(8) + rxt(200) + rxt(511) + rxt(519) + rxt(525) &
                      + rxt(530) + het_rates(11) )

         mat(1376) = -( het_rates(13) )
         mat(1083) = rxt(56)
         mat(399) = rxt(58)
         mat(378) = .400_r8*rxt(59)
         mat(1214) = .300_r8*rxt(63)
         mat(958) = rxt(68)
         mat(1891) = rxt(162)*y(12)
         mat(1447) = rxt(216)*y(12)
         mat(1603) = rxt(265)*y(12)

         mat(326) = -( rxt(14) + het_rates(14) )

         mat(129) = -( het_rates(18) )

         mat(76) = -( het_rates(19) )

         mat(1417) = -( rxt(15) + rxt(16) + het_rates(16) )
         mat(330) = rxt(14)
         mat(444) = rxt(57)
         mat(1146) = 1.340_r8*rxt(61)
         mat(298) = rxt(67)
         mat(514) = .100_r8*rxt(72)
         mat(770) = rxt(77)
         mat(1107) = .330_r8*rxt(78)
         mat(561) = .690_r8*rxt(79)
         mat(1091) = rxt(80)
         mat(1024) = rxt(81)
         mat(539) = .100_r8*rxt(85)
         mat(360) = .400_r8*rxt(88)
         mat(485) = .375_r8*rxt(89)
         mat(1009) = .680_r8*rxt(91)
         mat(424) = .330_r8*rxt(92)
         mat(316) = rxt(279)
         mat(196) = 2.000_r8*rxt(291)
         mat(1892) = rxt(163)*y(12) + rxt(164)*y(12)

         mat(1330) = -( rxt(169) + het_rates(21) )
         mat(328) = rxt(14)
         mat(1416) = 2.000_r8*rxt(15)
         mat(1322) = rxt(17) + 2.000_r8*rxt(19)
         mat(1912) = rxt(26)
         mat(666) = rxt(32)
         mat(1890) = rxt(163)*y(12)

         mat(1879) = -( rxt(514) + het_rates(23) )
         mat(324) = rxt(13) + rxt(209)
         mat(1086) = rxt(56)
         mat(446) = rxt(57)
         mat(1148) = 1.340_r8*rxt(61) + .660_r8*rxt(62)
         mat(241) = rxt(64)
         mat(293) = rxt(66)
         mat(1158) = rxt(69)
         mat(516) = rxt(72)
         mat(343) = rxt(73)
         mat(740) = rxt(74)
         mat(412) = rxt(75)
         mat(1111) = .670_r8*rxt(78)
         mat(563) = rxt(79)
         mat(1093) = rxt(80)
         mat(1026) = 2.000_r8*rxt(81)
         mat(555) = .560_r8*rxt(83)
         mat(840) = 2.000_r8*rxt(84)
         mat(541) = .900_r8*rxt(85)
         mat(530) = rxt(87)
         mat(362) = rxt(88)
         mat(487) = rxt(89)
         mat(953) = rxt(90)
         mat(1011) = 1.200_r8*rxt(91)
         mat(426) = rxt(92)
         mat(731) = 2.000_r8*rxt(93)
         mat(347) = rxt(94)
         mat(117) = 1.500_r8*rxt(95)
         mat(153) = rxt(96)
         mat(214) = .600_r8*rxt(97)
         mat(491) = .600_r8*rxt(98)
         mat(260) = rxt(99)
         mat(180) = rxt(100)
         mat(231) = rxt(101)
         mat(287) = rxt(103)
         mat(255) = rxt(104)
         mat(305) = rxt(105)
         mat(628) = rxt(106)
         mat(1334) = rxt(169)
         mat(318) = rxt(279)
         mat(197) = rxt(290) + rxt(291)
         mat(1269) = rxt(378)
         mat(1898) = rxt(163)*y(12)
         mat(1454) = rxt(253)*y(159) + rxt(258)*y(160)
         mat(1611) = rxt(254)*y(159) + rxt(257)*y(160)


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

         mat(492) = -( rxt(20) + het_rates(24) )
         mat(1820) = .500_r8*rxt(514)

         mat(1321) = -( rxt(17) + rxt(18) + rxt(19) + het_rates(244) )
         mat(1601) = rxt(254)*y(159) + rxt(255)*y(166) + rxt(256)*y(164) + rxt(257)*y(160) &
                      + rxt(261)*y(176) + rxt(265)*y(12)

         mat(1449) = -( rxt(216)*y(12) + rxt(253)*y(159) + rxt(258)*y(160) &
                      + rxt(263)*y(176) + rxt(264)*y(175) + het_rates(27) )
         mat(124) = 2.000_r8*rxt(21)
         mat(1937) = rxt(22)
         mat(80) = 2.000_r8*rxt(24)
         mat(709) = rxt(25)
         mat(1914) = rxt(26)
         mat(778) = rxt(27)
         mat(188) = rxt(29)
         mat(1893) = 3.000_r8*rxt(146)*y(161) + 2.000_r8*rxt(147)*y(162) &
                      + 3.000_r8*rxt(148)*y(163) + 2.000_r8*rxt(149)*y(171) &
                      + rxt(150)*y(172) + rxt(151)*y(164) + 2.000_r8*rxt(152)*y(169) &
                      + rxt(153)*y(170) + 4.000_r8*rxt(154)*y(165) + rxt(156)*y(168)
         mat(1606) = rxt(254)*y(159) + 3.000_r8*rxt(255)*y(166) + rxt(256)*y(164) &
                      + 2.000_r8*rxt(259)*y(169) + rxt(260)*y(170)

         mat(123) = -( rxt(21) + het_rates(28) )

         mat(1945) = -( rxt(22) + het_rates(29) )
         mat(102) = rxt(23)
         mat(783) = rxt(28)
         mat(81) = 2.000_r8*rxt(228)

         mat(100) = -( rxt(23) + het_rates(30) )

         mat(79) = -( rxt(24) + rxt(228) + het_rates(31) )

         mat(1921) = -( rxt(26) + het_rates(32) )
         mat(1456) = rxt(216)*y(12) + 2.000_r8*rxt(253)*y(159) + rxt(258)*y(160) &
                      + rxt(263)*y(176) + rxt(264)*y(175)

         mat(707) = -( rxt(25) + het_rates(33) )
         mat(775) = rxt(520) + rxt(526) + rxt(531)

         mat(776) = -( rxt(27) + rxt(28) + rxt(520) + rxt(526) + rxt(531) + het_rates(34) &
       )

         mat(187) = -( rxt(29) + het_rates(35) )

         mat(2067) = -( het_rates(36) )
         mat(189) = rxt(29)
         mat(1408) = rxt(30)
         mat(663) = rxt(31)
         mat(670) = rxt(32)
         mat(434) = rxt(33)
         mat(1904) = rxt(155)*y(160) + rxt(156)*y(168) + rxt(157)*y(167) &
                      + 2.000_r8*rxt(158)*y(173) + 2.000_r8*rxt(159)*y(174) &
                      + 3.000_r8*rxt(160)*y(175) + 2.000_r8*rxt(161)*y(176)
         mat(1617) = rxt(257)*y(160) + 2.000_r8*rxt(261)*y(176) + 3.000_r8*rxt(262)*y(175)
         mat(1460) = rxt(258)*y(160) + 2.000_r8*rxt(263)*y(176) + 3.000_r8*rxt(264)*y(175)

         mat(1396) = -( rxt(30) + het_rates(37) )
         mat(430) = rxt(34)

         mat(664) = -( rxt(32) + het_rates(38) )

         mat(656) = -( rxt(31) + het_rates(39) )
         mat(428) = rxt(521) + rxt(529) + rxt(532)

         mat(427) = -( rxt(33) + rxt(34) + rxt(521) + rxt(529) + rxt(532) + het_rates(40) &
       )

         mat(964) = -( het_rates(59) )
         mat(1204) = .700_r8*rxt(63)

         mat(747) = -( het_rates(92) )

         mat(595) = -( het_rates(64) )

         mat(1079) = -( rxt(56) + het_rates(50) )
         mat(441) = rxt(57)
         mat(239) = rxt(64)
         mat(512) = .400_r8*rxt(72)
         mat(1099) = .330_r8*rxt(78)
         mat(537) = .400_r8*rxt(85)
         mat(247) = rxt(86)

         mat(435) = -( het_rates(49) )

         mat(439) = -( rxt(57) + het_rates(65) )

         mat(1309) = -( het_rates(48) )
         mat(377) = .600_r8*rxt(59) + rxt(310)
         mat(1143) = 1.340_r8*rxt(61)
         mat(1211) = .300_r8*rxt(63)
         mat(296) = rxt(67)
         mat(956) = rxt(68)
         mat(1153) = rxt(69)
         mat(769) = rxt(77)
         mat(1103) = .330_r8*rxt(78)
         mat(1090) = rxt(80)
         mat(417) = rxt(82)
         mat(554) = .130_r8*rxt(83)
         mat(248) = rxt(86)
         mat(1006) = .650_r8*rxt(91)
         mat(152) = .500_r8*rxt(96)
         mat(259) = rxt(99)

         mat(396) = -( rxt(58) + het_rates(54) )

         mat(376) = -( rxt(59) + rxt(310) + het_rates(58) )

         mat(221) = -( het_rates(45) )

         mat(447) = -( het_rates(44) )

         mat(271) = -( het_rates(71) )

         mat(455) = -( rxt(60) + rxt(358) + het_rates(81) )

         mat(508) = -( rxt(72) + het_rates(82) )

         mat(767) = -( rxt(77) + het_rates(83) )

         mat(1101) = -( rxt(78) + het_rates(84) )

         mat(463) = -( het_rates(85) )

         mat(383) = -( het_rates(86) )

         mat(339) = -( rxt(73) + het_rates(87) )

         mat(734) = -( rxt(74) + het_rates(88) )

         mat(261) = -( het_rates(89) )

         mat(409) = -( rxt(75) + het_rates(90) )

         mat(266) = -( rxt(76) + het_rates(91) )

         mat(274) = -( het_rates(70) )

         mat(471) = -( het_rates(73) )

         mat(924) = -( het_rates(93) )

         mat(532) = -( rxt(85) + het_rates(94) )

         mat(414) = -( rxt(82) + het_rates(72) )
         mat(507) = .800_r8*rxt(72)
         mat(531) = .800_r8*rxt(85)

         mat(543) = -( het_rates(74) )

         mat(245) = -( rxt(86) + het_rates(75) )

         mat(160) = -( het_rates(106) )

         mat(168) = -( het_rates(107) )

         mat(568) = -( het_rates(108) )

         mat(521) = -( rxt(87) + het_rates(109) )

         mat(937) = -( het_rates(146) )

         mat(356) = -( rxt(88) + het_rates(147) )

         mat(550) = -( rxt(83) + het_rates(95) )

         mat(837) = -( rxt(84) + het_rates(53) )
         mat(551) = .130_r8*rxt(83)
         mat(525) = .600_r8*rxt(87)
         mat(229) = .700_r8*rxt(101)
         mat(285) = rxt(103)
         mat(302) = .170_r8*rxt(105)
         mat(621) = .340_r8*rxt(106)

         mat(108) = -( het_rates(110) )

         mat(138) = -( het_rates(133) )

         mat(118) = -( het_rates(111) )

         mat(114) = -( rxt(95) + het_rates(112) )

         mat(365) = -( het_rates(113) )

         mat(333) = -( het_rates(114) )

         mat(372) = -( het_rates(115) )
         mat(203) = rxt(102)

         mat(227) = -( rxt(101) + het_rates(116) )

         mat(501) = -( het_rates(117) )

         mat(202) = -( rxt(102) + het_rates(118) )

         mat(282) = -( rxt(103) + het_rates(119) )

         mat(345) = -( rxt(94) + het_rates(120) )
         mat(519) = .200_r8*rxt(87)
         mat(115) = rxt(95)
         mat(283) = .500_r8*rxt(103)
         mat(615) = .060_r8*rxt(106)

         mat(212) = -( rxt(97) + het_rates(121) )
         mat(518) = .200_r8*rxt(87)
         mat(613) = .200_r8*rxt(106)

         mat(488) = -( rxt(98) + het_rates(122) )
         mat(733) = rxt(74)
         mat(520) = .200_r8*rxt(87)
         mat(177) = rxt(100)
         mat(616) = .150_r8*rxt(106)

         mat(256) = -( rxt(99) + het_rates(123) )
         mat(614) = .210_r8*rxt(106)

         mat(579) = -( het_rates(124) )
         mat(346) = .600_r8*rxt(94)

         mat(150) = -( rxt(96) + het_rates(125) )

         mat(308) = -( het_rates(126) )

         mat(251) = -( rxt(104) + het_rates(127) )

         mat(173) = -( het_rates(128) )
         mat(250) = rxt(104)

         mat(403) = -( het_rates(129) )
         mat(90) = rxt(438)

         mat(647) = -( het_rates(130) )
         mat(213) = .600_r8*rxt(97)

         mat(1013) = -( het_rates(131) )
         mat(489) = .600_r8*rxt(98)

         mat(89) = -( rxt(438) + het_rates(132) )

         mat(145) = -( het_rates(134) )

         mat(349) = -( het_rates(135) )

         mat(300) = -( rxt(105) + het_rates(136) )

         mat(636) = -( het_rates(137) )

         mat(618) = -( rxt(106) + het_rates(138) )

         mat(793) = -( het_rates(139) )

         mat(850) = -( het_rates(140) )

         mat(876) = -( het_rates(141) )

         mat(819) = -( het_rates(142) )

         mat(902) = -( het_rates(143) )

         mat(12) = -( het_rates(145) )

         mat(23) = -( het_rates(144) )

         mat(946) = -( rxt(90) + het_rates(148) )
         mat(410) = rxt(75)
         mat(267) = rxt(76)
         mat(358) = rxt(88)

         mat(1049) = -( het_rates(149) )

         mat(1003) = -( rxt(91) + het_rates(150) )
         mat(481) = rxt(89)
         mat(948) = rxt(90)

         mat(479) = -( rxt(89) + het_rates(151) )

         mat(991) = -( het_rates(152) )

         mat(683) = -( het_rates(153) )

         mat(1032) = -( het_rates(154) )

         mat(726) = -( rxt(93) + het_rates(155) )
         mat(420) = .330_r8*rxt(92)

         mat(390) = -( het_rates(156) )

         mat(419) = -( rxt(92) + het_rates(157) )

         mat(604) = -( het_rates(158) )

         mat(1188) = -( het_rates(97) )

         mat(1259) = -( rxt(378) + het_rates(98) )

         mat(176) = -( rxt(100) + het_rates(99) )
         mat(1241) = rxt(378)

         mat(92) = -( het_rates(100) )

         mat(1209) = -( rxt(63) + het_rates(77) )
         mat(560) = .402_r8*rxt(79)

         mat(1139) = -( rxt(61) + rxt(62) + het_rates(78) )
         mat(558) = .288_r8*rxt(79)

         mat(1229) = -( het_rates(79) )


      end subroutine linmat02

      subroutine linmat03( mat, y, rxt, het_rates )
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

         mat(232) = -( het_rates(80) )

         mat(1277) = -( het_rates(76) )
         mat(457) = rxt(60) + rxt(358)
         mat(1142) = .660_r8*rxt(61)

         mat(717) = -( het_rates(46) )
         mat(416) = rxt(82)

         mat(237) = -( rxt(64) + het_rates(47) )

         mat(82) = -( het_rates(60) )

         mat(672) = -( het_rates(61) )

         mat(288) = -( rxt(66) + het_rates(62) )

         mat(954) = -( rxt(68) + het_rates(63) )
         mat(290) = .820_r8*rxt(66)
         mat(511) = .250_r8*rxt(72)
         mat(1095) = .170_r8*rxt(78)
         mat(536) = .250_r8*rxt(85)
         mat(359) = .050_r8*rxt(88)
         mat(480) = .300_r8*rxt(89)
         mat(1002) = .500_r8*rxt(91)
         mat(421) = .670_r8*rxt(92)
         mat(727) = rxt(93)

         mat(294) = -( rxt(67) + het_rates(69) )

         mat(697) = -( het_rates(15) )

         mat(190) = -( het_rates(51) )

         mat(1022) = -( rxt(81) + het_rates(52) )
         mat(1096) = .330_r8*rxt(78)
         mat(482) = .250_r8*rxt(89)
         mat(422) = .670_r8*rxt(92)
         mat(195) = rxt(290)

         mat(1088) = -( rxt(80) + het_rates(66) )
         mat(1100) = .170_r8*rxt(78)

         mat(587) = -( het_rates(55) )

         mat(194) = -( rxt(290) + rxt(291) + het_rates(56) )
         mat(96) = rxt(65)

         mat(95) = -( rxt(65) + het_rates(57) )

         mat(242) = -( het_rates(96) )

         mat(1066) = -( het_rates(67) )
         mat(1005) = .150_r8*rxt(91)

         mat(1152) = -( rxt(69) + het_rates(68) )
         mat(553) = .180_r8*rxt(83)
         mat(528) = .400_r8*rxt(87)
         mat(258) = rxt(99)
         mat(303) = .510_r8*rxt(105)
         mat(624) = .540_r8*rxt(106)

         mat(1123) = -( het_rates(101) )

         mat(98) = -( rxt(71) + het_rates(102) )

         mat(1167) = -( het_rates(103) )

         mat(198) = -( rxt(70) + het_rates(104) )

         mat(557) = -( rxt(79) + het_rates(105) )
         mat(340) = rxt(73)

         mat(215) = -( het_rates(41) )

         mat(763) = -( het_rates(42) )

         mat(314) = -( rxt(279) + het_rates(43) )

         mat(87) = -( het_rates(177) )

         mat(182) = -( het_rates(178) )

         mat(1) = -( het_rates(179) )

         mat(2) = -( het_rates(180) )

         mat(3) = -( het_rates(181) )

         mat(4) = -( het_rates(182) )

         mat(5) = -( het_rates(183) )

         mat(6) = -( het_rates(184) )

         mat(13) = -( het_rates(185) )

         mat(14) = -( het_rates(186) )

         mat(15) = -( het_rates(187) )

         mat(16) = -( het_rates(188) )

         mat(17) = -( het_rates(189) )

         mat(24) = -( het_rates(190) )

         mat(25) = -( het_rates(191) )

         mat(26) = -( het_rates(192) )

         mat(27) = -( het_rates(193) )

         mat(28) = -( het_rates(194) )

         mat(29) = -( het_rates(225) )

         mat(30) = -( het_rates(226) )

         mat(31) = -( het_rates(227) )

         mat(32) = -( het_rates(228) )

         mat(33) = -( het_rates(229) )

         mat(34) = -( het_rates(230) )

         mat(35) = -( het_rates(231) )

         mat(36) = -( rxt(117) + het_rates(195) )

         mat(37) = -( rxt(118) + het_rates(196) )

         mat(38) = -( rxt(119) + het_rates(197) )

         mat(39) = -( rxt(120) + het_rates(198) )

         mat(40) = -( rxt(121) + het_rates(199) )

         mat(41) = -( rxt(107) + het_rates(200) )

         mat(42) = -( rxt(108) + het_rates(201) )

         mat(43) = -( rxt(109) + het_rates(202) )

         mat(44) = -( rxt(110) + het_rates(203) )

         mat(45) = -( rxt(111) + het_rates(204) )

         mat(46) = -( rxt(127) + het_rates(205) )

         mat(47) = -( rxt(128) + het_rates(206) )

         mat(48) = -( rxt(129) + het_rates(207) )

         mat(49) = -( rxt(130) + het_rates(208) )

         mat(50) = -( rxt(131) + het_rates(209) )

         mat(51) = -( rxt(122) + het_rates(210) )

         mat(52) = -( rxt(123) + het_rates(211) )

         mat(53) = -( rxt(124) + het_rates(212) )

         mat(54) = -( rxt(125) + het_rates(213) )

         mat(55) = -( rxt(126) + het_rates(214) )

         mat(56) = -( rxt(112) + het_rates(215) )

         mat(57) = -( rxt(113) + het_rates(216) )

         mat(58) = -( rxt(114) + het_rates(217) )

         mat(59) = -( rxt(115) + het_rates(218) )

         mat(60) = -( rxt(116) + het_rates(219) )

         mat(61) = -( rxt(132) + het_rates(220) )

         mat(62) = -( rxt(133) + het_rates(221) )

         mat(63) = -( rxt(134) + het_rates(222) )

         mat(64) = -( rxt(135) + het_rates(223) )

         mat(65) = -( rxt(136) + het_rates(224) )

         mat(66) = -( het_rates(232) )

         mat(67) = -( het_rates(233) )

         mat(68) = -( het_rates(234) )

         mat(69) = -( het_rates(235) )

         mat(70) = -( het_rates(236) )

         mat(71) = -( het_rates(237) )

         mat(72) = -( het_rates(238) )

         mat(73) = -( het_rates(239) )

         mat(74) = -( het_rates(240) )

         mat(75) = -( het_rates(241) )


      end subroutine linmat03

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
      call linmat03( mat, y, rxt, het_rates )

      end subroutine linmat

      end module mo_lin_matrix
