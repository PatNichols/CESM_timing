




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

         mat(936) = -( rxt(2) + rxt(3) + het_rates(1) )
         mat(492) = rxt(44)

         mat(483) = -( rxt(44) + het_rates(2) )
         mat(921) = rxt(3)
         mat(691) = rxt(5)
         mat(94) = rxt(7)
         mat(623) = rxt(9)
         mat(404) = rxt(46) + rxt(47)

         mat(403) = -( rxt(46) + rxt(47) + rxt(49) + rxt(51)*y(4) + rxt(52)*y(4) &
                      + rxt(53)*y(16) + rxt(54)*y(16) + rxt(55)*y(16) + het_rates(3) )
         mat(914) = rxt(2)

         mat(868) = -( het_rates(17) )
         mat(715) = rxt(14) + rxt(15)
         mat(418) = rxt(17)
         mat(468) = 1.340_r8*rxt(23)
         mat(522) = .700_r8*rxt(24)
         mat(477) = rxt(30)
         mat(428) = rxt(32)
         mat(349) = rxt(35)
         mat(231) = .450_r8*rxt(37)
         mat(270) = 2.000_r8*rxt(38)
         mat(73) = rxt(43)

         mat(262) = -( het_rates(11) )
         mat(705) = rxt(15)
         mat(401) = rxt(55)*y(16)

         mat(61) = -( het_rates(87) )

         mat(34) = -( het_rates(88) )

         mat(366) = -( rxt(57) + het_rates(15) )
         mat(147) = rxt(13)
         mat(402) = rxt(54)*y(16)

         mat(673) = -( het_rates(5) )
         mat(695) = rxt(5) + .500_r8*rxt(224)
         mat(96) = rxt(7)
         mat(631) = rxt(10)
         mat(405) = 2.000_r8*rxt(51)*y(4)

         mat(696) = -( rxt(5) + rxt(224) + het_rates(6) )
         mat(97) = rxt(6) + rxt(83)
         mat(207) = rxt(8)
         mat(632) = rxt(9)
         mat(138) = rxt(12) + rxt(92)
         mat(201) = .600_r8*rxt(20) + rxt(135)
         mat(236) = rxt(21) + rxt(181)
         mat(424) = rxt(32)

         mat(630) = -( rxt(9) + rxt(10) + rxt(223) + het_rates(7) )
         mat(95) = rxt(6) + rxt(7) + rxt(83)
         mat(137) = rxt(11)
         mat(200) = .400_r8*rxt(20)

         mat(205) = -( rxt(8) + het_rates(8) )
         mat(93) = 2.000_r8*rxt(222)
         mat(609) = rxt(223)
         mat(685) = .500_r8*rxt(224)

         mat(136) = -( rxt(11) + rxt(12) + rxt(92) + het_rates(9) )

         mat(92) = -( rxt(6) + rxt(7) + rxt(83) + rxt(222) + het_rates(10) )

         mat(796) = -( rxt(93)*y(16) + het_rates(12) )
         mat(208) = rxt(8)
         mat(139) = rxt(11)
         mat(149) = rxt(13)
         mat(90) = 2.000_r8*rxt(16)
         mat(189) = rxt(18)
         mat(177) = rxt(19)
         mat(123) = rxt(25)
         mat(39) = rxt(26)
         mat(144) = rxt(27)
         mat(102) = rxt(28)
         mat(67) = rxt(31)
         mat(280) = rxt(39)
         mat(114) = rxt(40)
         mat(159) = rxt(41)
         mat(196) = rxt(42)
         mat(407) = 2.000_r8*rxt(49) + rxt(53)*y(16)
         mat(698) = .500_r8*rxt(224)

         mat(855) = -( rxt(230) + het_rates(13) )
         mat(140) = rxt(12) + rxt(92)
         mat(417) = rxt(17)
         mat(190) = rxt(18)
         mat(467) = 1.340_r8*rxt(22) + .660_r8*rxt(23)
         mat(124) = rxt(25)
         mat(145) = rxt(27)
         mat(476) = rxt(30)
         mat(427) = rxt(32)
         mat(216) = rxt(33)
         mat(459) = rxt(34)
         mat(348) = 2.000_r8*rxt(35)
         mat(230) = .560_r8*rxt(37)
         mat(269) = 2.000_r8*rxt(38)
         mat(281) = .900_r8*rxt(39)
         mat(197) = rxt(42)
         mat(370) = rxt(57)
         mat(170) = rxt(107)
         mat(87) = rxt(115) + rxt(116)
         mat(408) = rxt(54)*y(16)

         mat(88) = -( rxt(16) + het_rates(14) )
         mat(804) = .500_r8*rxt(230)

         mat(902) = -( het_rates(18) )
         mat(419) = rxt(17)
         mat(178) = rxt(19)
         mat(204) = .400_r8*rxt(20)
         mat(523) = .300_r8*rxt(24)
         mat(294) = rxt(29)
         mat(409) = rxt(53)*y(16)
         mat(799) = rxt(93)*y(16)

         mat(146) = -( rxt(13) + het_rates(19) )

         mat(712) = -( rxt(14) + rxt(15) + het_rates(20) )
         mat(148) = rxt(13)
         mat(188) = rxt(18)
         mat(465) = 1.340_r8*rxt(22)
         mat(101) = rxt(28)
         mat(425) = rxt(32)
         mat(214) = .690_r8*rxt(33)
         mat(457) = rxt(34)
         mat(346) = rxt(35)
         mat(279) = .100_r8*rxt(39)
         mat(168) = rxt(107)
         mat(86) = 2.000_r8*rxt(116)
         mat(406) = rxt(54)*y(16) + rxt(55)*y(16)

         mat(240) = -( het_rates(21) )

         mat(80) = -( het_rates(22) )

         mat(37) = -( rxt(26) + het_rates(28) )

         mat(129) = -( het_rates(23) )

         mat(84) = -( rxt(115) + rxt(116) + het_rates(24) )
         mat(38) = rxt(26)

         mat(246) = -( het_rates(25) )

         mat(171) = -( het_rates(26) )

         mat(345) = -( rxt(35) + het_rates(27) )
         mat(85) = rxt(115)

         mat(22) = -( het_rates(29) )

         mat(324) = -( het_rates(30) )
         mat(181) = rxt(36)

         mat(120) = -( rxt(25) + het_rates(31) )

         mat(412) = -( rxt(17) + het_rates(32) )
         mat(186) = rxt(18)
         mat(122) = rxt(25)
         mat(277) = .400_r8*rxt(39)
         mat(112) = rxt(40)

         mat(594) = -( het_rates(33) )
         mat(199) = .600_r8*rxt(20) + rxt(135)
         mat(464) = 1.340_r8*rxt(22)
         mat(515) = .300_r8*rxt(24)
         mat(100) = rxt(28)
         mat(292) = rxt(29)
         mat(472) = rxt(30)
         mat(456) = rxt(34)
         mat(182) = rxt(36)
         mat(229) = .130_r8*rxt(37)
         mat(113) = rxt(40)

         mat(174) = -( rxt(19) + het_rates(34) )

         mat(387) = -( het_rates(35) )
         mat(509) = .700_r8*rxt(24)

         mat(25) = -( het_rates(36) )

         mat(334) = -( het_rates(37) )

         mat(141) = -( rxt(27) + het_rates(38) )

         mat(296) = -( het_rates(39) )

         mat(184) = -( rxt(18) + het_rates(40) )

         mat(290) = -( rxt(29) + het_rates(41) )
         mat(142) = .820_r8*rxt(27)
         mat(274) = .250_r8*rxt(39)
         mat(192) = .100_r8*rxt(42)

         mat(444) = -( het_rates(42) )

         mat(98) = -( rxt(28) + het_rates(43) )

         mat(28) = -( het_rates(44) )

         mat(103) = -( het_rates(45) )

         mat(31) = -( het_rates(49) )

         mat(309) = -( het_rates(50) )

         mat(272) = -( rxt(39) + het_rates(51) )

         mat(179) = -( rxt(36) + het_rates(46) )
         mat(271) = .800_r8*rxt(39)

         mat(283) = -( het_rates(47) )

         mat(110) = -( rxt(40) + het_rates(48) )

         mat(351) = -( het_rates(52) )

         mat(552) = -( het_rates(53) )

         mat(209) = -( rxt(33) + het_rates(54) )

         mat(513) = -( rxt(24) + het_rates(55) )
         mat(212) = .402_r8*rxt(33)
         mat(195) = rxt(42)

         mat(460) = -( rxt(22) + rxt(23) + het_rates(56) )
         mat(210) = .288_r8*rxt(33)
         mat(194) = rxt(42)

         mat(532) = -( het_rates(57) )

         mat(115) = -( het_rates(58) )

         mat(569) = -( het_rates(59) )
         mat(234) = rxt(21) + rxt(181)
         mat(463) = .660_r8*rxt(22)

         mat(151) = -( het_rates(60) )

         mat(454) = -( rxt(34) + het_rates(61) )

         mat(471) = -( rxt(30) + het_rates(62) )
         mat(228) = .180_r8*rxt(37)
         mat(158) = .450_r8*rxt(41)

         mat(499) = -( het_rates(63) )

         mat(65) = -( rxt(31) + het_rates(64) )

         mat(253) = -( het_rates(65) )

         mat(374) = -( het_rates(66) )

         mat(191) = -( rxt(42) + het_rates(67) )

         mat(40) = -( het_rates(68) )

         mat(45) = -( het_rates(69) )

         mat(218) = -( het_rates(70) )

         mat(154) = -( rxt(41) + het_rates(71) )

         mat(57) = -( het_rates(72) )

         mat(226) = -( rxt(37) + het_rates(73) )
         mat(156) = .900_r8*rxt(41)

         mat(267) = -( rxt(38) + het_rates(74) )
         mat(227) = .130_r8*rxt(37)
         mat(157) = .450_r8*rxt(41)


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

         mat(198) = -( rxt(20) + rxt(135) + het_rates(75) )

         mat(160) = -( het_rates(76) )

         mat(232) = -( rxt(21) + rxt(181) + het_rates(77) )

         mat(431) = -( het_rates(78) )

         mat(421) = -( rxt(32) + het_rates(79) )

         mat(69) = -( rxt(43) + het_rates(80) )

         mat(49) = -( het_rates(81) )

         mat(75) = -( het_rates(82) )

         mat(20) = -( het_rates(83) )

         mat(1) = -( het_rates(84) )

         mat(2) = -( het_rates(94) )

         mat(51) = -( het_rates(89) )

         mat(125) = -( het_rates(90) )

         mat(165) = -( rxt(107) + het_rates(91) )

         mat(3) = -( het_rates(92) )

         mat(4) = -( het_rates(93) )

         mat(5) = -( het_rates(95) )

         mat(6) = -( het_rates(96) )

         mat(7) = -( het_rates(97) )

         mat(8) = -( het_rates(98) )

         mat(9) = -( het_rates(99) )

         mat(10) = -( het_rates(100) )

         mat(11) = -( het_rates(101) )

         mat(12) = -( het_rates(102) )

         mat(13) = -( het_rates(103) )

         mat(14) = -( het_rates(104) )

         mat(15) = -( het_rates(105) )

         mat(16) = -( het_rates(106) )

         mat(17) = -( het_rates(107) )

         mat(18) = -( het_rates(108) )

         mat(19) = -( het_rates(109) )


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
