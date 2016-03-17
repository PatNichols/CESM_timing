




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

         mat(972) = -( rxt(2) + rxt(3) + het_rates(1) )
         mat(1091) = rxt(83)

         mat(1094) = -( rxt(83) + het_rates(2) )
         mat(975) = rxt(3)
         mat(1205) = rxt(5)
         mat(804) = rxt(6)
         mat(109) = rxt(8)
         mat(937) = rxt(10)
         mat(687) = rxt(19)
         mat(1136) = rxt(22)
         mat(40) = rxt(23)
         mat(714) = rxt(30)
         mat(1113) = rxt(86) + rxt(87)
         mat(70) = rxt(134)

         mat(1114) = -( rxt(86) + rxt(87) + rxt(89)*y(4) + rxt(90)*y(4) + rxt(92)*y(101) &
                      + rxt(93)*y(102) + rxt(94)*y(103) + rxt(95)*y(111) + rxt(96)*y(112) &
                      + rxt(97)*y(104) + rxt(98)*y(109) + rxt(99)*y(110) + rxt(100)*y(105) &
                      + rxt(101)*y(100) + rxt(102)*y(108) + rxt(103)*y(107) &
                      + rxt(104)*y(113) + rxt(105)*y(114) + rxt(106)*y(115) &
                      + rxt(107)*y(116) + rxt(108)*y(12) + rxt(109)*y(12) + rxt(110)*y(12) &
                 + het_rates(3) )
         mat(976) = rxt(2)
         mat(688) = rxt(18)

         mat(493) = -( het_rates(18) )
         mat(760) = rxt(16)
         mat(682) = rxt(18)
         mat(1102) = rxt(110)*y(12)

         mat(501) = -( het_rates(17) )
         mat(761) = rxt(15) + rxt(16)
         mat(528) = rxt(56)
         mat(538) = 1.340_r8*rxt(62)
         mat(579) = .700_r8*rxt(63)
         mat(551) = rxt(69)
         mat(442) = rxt(71)
         mat(375) = rxt(74)
         mat(250) = .450_r8*rxt(76)
         mat(319) = 2.000_r8*rxt(77)
         mat(81) = rxt(82)
         mat(988) = rxt(199)*y(99)

         mat(68) = -( rxt(134) + het_rates(5) )
         mat(1160) = rxt(5)

         mat(1209) = -( rxt(5) + het_rates(6) )
         mat(808) = rxt(6) + .500_r8*rxt(343)
         mat(110) = rxt(8)
         mat(941) = rxt(11)
         mat(71) = rxt(134)
         mat(1117) = 2.000_r8*rxt(89)*y(4)

         mat(798) = -( rxt(6) + rxt(343) + het_rates(7) )
         mat(107) = rxt(7) + rxt(146)
         mat(401) = rxt(9)
         mat(931) = rxt(10)
         mat(176) = rxt(13) + rxt(155)
         mat(433) = rxt(28)
         mat(235) = rxt(33)
         mat(202) = .600_r8*rxt(59) + rxt(254)
         mat(258) = rxt(60) + rxt(300)
         mat(445) = rxt(71)

         mat(891) = -( rxt(200)*y(99) + rxt(201)*y(106) + rxt(202)*y(104) &
                      + rxt(203)*y(100) + rxt(205)*y(109) + rxt(206)*y(110) &
                      + rxt(207)*y(116) + rxt(208)*y(115) + rxt(211)*y(12) + het_rates(20) &
       )
         mat(402) = rxt(9)
         mat(177) = rxt(12)
         mat(185) = rxt(14)
         mat(685) = rxt(17)
         mat(290) = 2.000_r8*rxt(20)
         mat(407) = rxt(25)
         mat(368) = rxt(31)
         mat(268) = rxt(57)
         mat(218) = rxt(58)
         mat(131) = rxt(64)
         mat(52) = rxt(65)
         mat(162) = rxt(66)
         mat(169) = rxt(67)
         mat(136) = rxt(70)
         mat(308) = rxt(78)
         mat(122) = rxt(79)
         mat(153) = rxt(80)
         mat(196) = rxt(81)
         mat(799) = .500_r8*rxt(343)
         mat(1108) = rxt(108)*y(12)

         mat(933) = -( rxt(10) + rxt(11) + rxt(342) + het_rates(8) )
         mat(108) = rxt(7) + rxt(8) + rxt(146)
         mat(178) = rxt(12)
         mat(435) = rxt(27)
         mat(236) = rxt(32)
         mat(204) = .400_r8*rxt(59)

         mat(399) = -( rxt(9) + het_rates(9) )
         mat(106) = 2.000_r8*rxt(341) + 2.000_r8*rxt(352) + 2.000_r8*rxt(358) &
                      + 2.000_r8*rxt(363)
         mat(910) = rxt(342)
         mat(786) = .500_r8*rxt(343)
         mat(429) = rxt(353) + rxt(359) + rxt(364)
         mat(233) = rxt(354) + rxt(362) + rxt(365)

         mat(174) = -( rxt(12) + rxt(13) + rxt(155) + het_rates(10) )

         mat(105) = -( rxt(7) + rxt(8) + rxt(146) + rxt(341) + rxt(352) + rxt(358) &
                      + rxt(363) + het_rates(11) )

         mat(744) = -( het_rates(13) )
         mat(533) = rxt(56)
         mat(216) = rxt(58)
         mat(200) = .400_r8*rxt(59)
         mat(588) = .300_r8*rxt(63)
         mat(341) = rxt(68)
         mat(1105) = rxt(108)*y(12)
         mat(993) = rxt(162)*y(12)
         mat(888) = rxt(211)*y(12)

         mat(180) = -( rxt(14) + het_rates(14) )

         mat(72) = -( het_rates(39) )

         mat(32) = -( het_rates(40) )

         mat(764) = -( rxt(15) + rxt(16) + het_rates(16) )
         mat(184) = rxt(14)
         mat(267) = rxt(57)
         mat(546) = 1.340_r8*rxt(61)
         mat(168) = rxt(67)
         mat(444) = rxt(71)
         mat(228) = .690_r8*rxt(72)
         mat(524) = rxt(73)
         mat(376) = rxt(74)
         mat(307) = .100_r8*rxt(78)
         mat(208) = rxt(225)
         mat(97) = 2.000_r8*rxt(235)
         mat(1106) = rxt(109)*y(12) + rxt(110)*y(12)

         mat(692) = -( rxt(115) + het_rates(19) )
         mat(182) = rxt(14)
         mat(763) = 2.000_r8*rxt(15)
         mat(684) = rxt(17) + 2.000_r8*rxt(19)
         mat(1218) = rxt(26)
         mat(381) = rxt(34)
         mat(1104) = rxt(109)*y(12)

         mat(1067) = -( rxt(349) + het_rates(21) )
         mat(179) = rxt(13) + rxt(155)
         mat(537) = rxt(56)
         mat(269) = rxt(57)
         mat(549) = 1.340_r8*rxt(61) + .660_r8*rxt(62)
         mat(132) = rxt(64)
         mat(163) = rxt(66)
         mat(559) = rxt(69)
         mat(448) = rxt(71)
         mat(230) = rxt(72)
         mat(526) = rxt(73)
         mat(378) = 2.000_r8*rxt(74)
         mat(253) = .560_r8*rxt(76)
         mat(321) = 2.000_r8*rxt(77)
         mat(309) = .900_r8*rxt(78)
         mat(197) = rxt(81)
         mat(696) = rxt(115)
         mat(211) = rxt(225)
         mat(98) = rxt(234) + rxt(235)
         mat(1112) = rxt(109)*y(12)
         mat(1000) = rxt(199)*y(99) + rxt(204)*y(100)
         mat(895) = rxt(200)*y(99) + rxt(203)*y(100)

         mat(288) = -( rxt(20) + het_rates(22) )
         mat(1027) = .500_r8*rxt(349)

         mat(683) = -( rxt(17) + rxt(18) + rxt(19) + het_rates(146) )
         mat(885) = rxt(200)*y(99) + rxt(201)*y(106) + rxt(202)*y(104) + rxt(203)*y(100) &
                      + rxt(207)*y(116) + rxt(211)*y(12)

         mat(999) = -( rxt(162)*y(12) + rxt(199)*y(99) + rxt(204)*y(100) + rxt(209)*y(116) &
                      + rxt(210)*y(115) + het_rates(25) )
         mat(63) = 2.000_r8*rxt(21)
         mat(1134) = rxt(22)
         mat(21) = 2.000_r8*rxt(24)
         mat(408) = rxt(25)
         mat(1224) = rxt(26)
         mat(436) = rxt(27)
         mat(77) = rxt(29)
         mat(1111) = 3.000_r8*rxt(92)*y(101) + 2.000_r8*rxt(93)*y(102) &
                      + 3.000_r8*rxt(94)*y(103) + 2.000_r8*rxt(95)*y(111) + rxt(96)*y(112) &
                      + rxt(97)*y(104) + 2.000_r8*rxt(98)*y(109) + rxt(99)*y(110) &
                      + 4.000_r8*rxt(100)*y(105) + rxt(102)*y(108)
         mat(894) = rxt(200)*y(99) + 3.000_r8*rxt(201)*y(106) + rxt(202)*y(104) &
                      + 2.000_r8*rxt(205)*y(109) + rxt(206)*y(110)

         mat(62) = -( rxt(21) + het_rates(26) )

         mat(1138) = -( rxt(22) + het_rates(27) )
         mat(41) = rxt(23)
         mat(438) = rxt(28)
         mat(22) = 2.000_r8*rxt(174)

         mat(39) = -( rxt(23) + het_rates(28) )

         mat(20) = -( rxt(24) + rxt(174) + het_rates(29) )

         mat(1231) = -( rxt(26) + het_rates(30) )
         mat(1006) = rxt(162)*y(12) + 2.000_r8*rxt(199)*y(99) + rxt(204)*y(100) &
                      + rxt(209)*y(116) + rxt(210)*y(115)

         mat(405) = -( rxt(25) + het_rates(31) )
         mat(430) = rxt(353) + rxt(359) + rxt(364)

         mat(431) = -( rxt(27) + rxt(28) + rxt(353) + rxt(359) + rxt(364) + het_rates(32) &
       )

         mat(76) = -( rxt(29) + het_rates(33) )

         mat(1157) = -( het_rates(34) )
         mat(78) = rxt(29)
         mat(717) = rxt(30)
         mat(371) = rxt(31)
         mat(238) = rxt(32)
         mat(385) = rxt(34)
         mat(1116) = rxt(101)*y(100) + rxt(102)*y(108) + rxt(103)*y(107) &
                      + 2.000_r8*rxt(104)*y(113) + 2.000_r8*rxt(105)*y(114) &
                      + 3.000_r8*rxt(106)*y(115) + 2.000_r8*rxt(107)*y(116)
         mat(899) = rxt(203)*y(100) + 2.000_r8*rxt(207)*y(116) + 3.000_r8*rxt(208)*y(115)
         mat(1004) = rxt(204)*y(100) + 2.000_r8*rxt(209)*y(116) + 3.000_r8*rxt(210)*y(115)

         mat(707) = -( rxt(30) + het_rates(35) )
         mat(234) = rxt(33)

         mat(379) = -( rxt(34) + het_rates(36) )

         mat(365) = -( rxt(31) + het_rates(37) )
         mat(232) = rxt(354) + rxt(362) + rxt(365)


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

         mat(231) = -( rxt(32) + rxt(33) + rxt(354) + rxt(362) + rxt(365) + het_rates(38) &
       )

         mat(465) = -( het_rates(56) )
         mat(578) = .700_r8*rxt(63)

         mat(413) = -( het_rates(80) )

         mat(344) = -( het_rates(61) )

         mat(529) = -( rxt(56) + het_rates(47) )
         mat(265) = rxt(57)
         mat(130) = rxt(64)
         mat(305) = .400_r8*rxt(78)
         mat(120) = rxt(79)

         mat(295) = -( het_rates(46) )

         mat(262) = -( rxt(57) + het_rates(62) )

         mat(671) = -( het_rates(45) )
         mat(199) = .600_r8*rxt(59) + rxt(254)
         mat(543) = 1.340_r8*rxt(61)
         mat(585) = .300_r8*rxt(63)
         mat(166) = rxt(67)
         mat(339) = rxt(68)
         mat(553) = rxt(69)
         mat(523) = rxt(73)
         mat(189) = rxt(75)
         mat(252) = .130_r8*rxt(76)
         mat(121) = rxt(79)

         mat(213) = -( rxt(58) + het_rates(51) )

         mat(198) = -( rxt(59) + rxt(254) + het_rates(55) )

         mat(154) = -( het_rates(79) )

         mat(99) = -( het_rates(42) )

         mat(137) = -( het_rates(41) )

         mat(23) = -( het_rates(68) )

         mat(254) = -( rxt(60) + rxt(300) + het_rates(78) )

         mat(26) = -( het_rates(67) )

         mat(111) = -( het_rates(70) )

         mat(326) = -( het_rates(81) )

         mat(300) = -( rxt(78) + het_rates(82) )

         mat(186) = -( rxt(75) + het_rates(69) )
         mat(299) = .800_r8*rxt(78)

         mat(311) = -( het_rates(71) )

         mat(118) = -( rxt(79) + het_rates(72) )

         mat(42) = -( het_rates(91) )

         mat(47) = -( het_rates(92) )

         mat(240) = -( het_rates(93) )

         mat(148) = -( rxt(80) + het_rates(94) )

         mat(64) = -( het_rates(95) )

         mat(508) = -( het_rates(97) )

         mat(191) = -( rxt(81) + het_rates(98) )

         mat(248) = -( rxt(76) + het_rates(83) )
         mat(150) = .900_r8*rxt(80)

         mat(318) = -( rxt(77) + het_rates(50) )
         mat(249) = .130_r8*rxt(76)
         mat(151) = .450_r8*rxt(80)

         mat(626) = -( het_rates(85) )

         mat(583) = -( rxt(63) + het_rates(74) )
         mat(226) = .402_r8*rxt(72)
         mat(195) = rxt(81)

         mat(539) = -( rxt(61) + rxt(62) + het_rates(75) )
         mat(224) = .288_r8*rxt(72)
         mat(194) = rxt(81)

         mat(604) = -( het_rates(76) )

         mat(123) = -( het_rates(77) )

         mat(644) = -( het_rates(73) )
         mat(256) = rxt(60) + rxt(300)
         mat(542) = .660_r8*rxt(61)

         mat(356) = -( het_rates(43) )
         mat(188) = rxt(75)

         mat(128) = -( rxt(64) + het_rates(44) )

         mat(270) = -( het_rates(96) )

         mat(35) = -( het_rates(57) )

         mat(388) = -( het_rates(58) )

         mat(158) = -( rxt(66) + het_rates(59) )

         mat(337) = -( rxt(68) + het_rates(60) )
         mat(159) = .820_r8*rxt(66)
         mat(303) = .250_r8*rxt(78)
         mat(192) = .100_r8*rxt(81)

         mat(164) = -( rxt(67) + het_rates(66) )

         mat(219) = -( het_rates(15) )

         mat(91) = -( het_rates(48) )

         mat(374) = -( rxt(74) + het_rates(49) )
         mat(96) = rxt(234)

         mat(521) = -( rxt(73) + het_rates(63) )

         mat(281) = -( het_rates(52) )

         mat(95) = -( rxt(234) + rxt(235) + het_rates(53) )
         mat(51) = rxt(65)

         mat(50) = -( rxt(65) + het_rates(54) )

         mat(145) = -( het_rates(84) )

         mat(451) = -( het_rates(64) )

         mat(552) = -( rxt(69) + het_rates(65) )
         mat(251) = .180_r8*rxt(76)
         mat(152) = .450_r8*rxt(80)

         mat(481) = -( het_rates(86) )

         mat(441) = -( rxt(71) + het_rates(87) )

         mat(567) = -( het_rates(88) )

         mat(133) = -( rxt(70) + het_rates(89) )

         mat(223) = -( rxt(72) + het_rates(90) )

         mat(56) = -( het_rates(118) )

         mat(170) = -( het_rates(119) )

         mat(206) = -( rxt(225) + het_rates(120) )

         mat(80) = -( rxt(82) + het_rates(121) )

         mat(54) = -( het_rates(122) )

         mat(86) = -( het_rates(123) )

         mat(29) = -( het_rates(124) )

         mat(1) = -( het_rates(125) )

         mat(2) = -( het_rates(130) )

         mat(3) = -( het_rates(128) )

         mat(4) = -( het_rates(129) )

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
