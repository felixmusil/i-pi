module dms_2b

  implicit none

  integer, parameter, public :: &
    mg411_nr=15, mg411_ngrp=24, mg411_ngen=2, &
    mg411_dvp(0:9) = (/ 0, 4, 4, 4, 3, 0, 0, 0, 0, 0 /), &
    mg411_ivp(0:9) = (/ 0, 4, 8, 12, 15, 15, 15, 15, 15, 15 /), &
    mg411_dvs(0:9) = (/ 1, 0, 3, 13, 32, 62, 129, 221, 335, 442 /), &
    mg411_ivs(0:9) = (/ 1, 1, 4, 17, 49, 111, 240, 461, 796, 1238 /), &
    mg411_dvb(0:9) = (/ 1, 4, 17, 65, 230, 736, 2197, 6093, 15864, &
      38960 /), &
    mg411_ivb(0:9) = (/ 1, 5, 22, 87, 317, 1053, 3250, 9343, 25207, &
      64167 /)
  integer, parameter, public :: &
      mg321_nr=15, mg321_ngrp=12, mg321_ngen=3, &
      mg321_dvp(0:9) = (/ 0, 5, 6, 3, 0, 0, 1, 0, 0, 0 /), &
      mg321_ivp(0:9) = (/ 0, 5, 11, 14, 14, 14, 15, 15, 15, 15 /), &
      mg321_dvs(0:9) = (/ 1, 0, 4, 16, 29, 46, 94, 124, 118, 118 /), &
      mg321_ivs(0:9) = (/ 1, 1, 5, 21, 50, 96, 190, 314, 432, 550 /), &
      mg321_dvb(0:9) = (/ 1, 5, 25, 104, 389, 1303, 4008, 11363, 30061, &
        74702 /), &
      mg321_ivb(0:9) = (/ 1, 6, 31, 135, 524, 1827, 5835, 17198, 47259, &
        121961 /)
contains
  subroutine mgx_mk2d (nk, nr, la, x, d)
    integer, intent (in) :: nk, nr, la(0:)
    real , intent (in) :: x(0:nr-1)
    real , intent (out) :: d(0:nk-1,0:nk-1)
    integer :: i, j, k, l0, l1, lb(0:size(la))
    lb(0) = 0
    do l0 = 1, size(la)
    lb(l0) = lb(l0-1)+la(l0-1)
    enddo
    if (lb(size(la)).ne.nk) then
    stop 'mgx_mk2d: bad count nk'
    endif
    k = 0
    do l1 = 0, size(lb)-1
    do l0 = 0, l1
      do j = lb(l1), lb(l1+1)-1
      do i = lb(l0), min(j-1,lb(l0+1)-1)
        d(i,j) = x(k)
        d(j,i) = d(i,j)
        k = k+1
      enddo
      enddo
    enddo
    enddo
    if (k.ne.nr) then
    stop 'mgx_mk2d: bad count nr'
    endif
    do i = 0, nk-1
    d(i,i) = 0
    enddo
  end subroutine mgx_mk2d

  subroutine mgx_mk1d (nk, nr, la, d, x)
    integer, intent (in) :: nk, nr, la(0:)
    real , intent (in) :: d(0:nk-1,0:nk-1)
    real , intent (out) :: x(0:nr-1)
    integer :: i, j, k, l0, l1, lb(0:size(la))
    lb(0) = 0
    do l0 = 1, size(la)
     lb(l0) = lb(l0-1)+la(l0-1)
    enddo
    if (lb(size(la)).ne.nk) then
     stop 'mgx_mk1d: bad count nk'
    endif
    k = 0
    do l1 = 0, size(la)-1
     do l0 = 0, l1
      do j = lb(l1), lb(l1+1)-1
       do i = lb(l0), min(j-1,lb(l0+1)-1)
        x(k) = d(i,j)
        k = k+1
       enddo
      enddo
     enddo
    enddo
    if (k.ne.nr) then
     stop 'mgx_mk1d: bad count nr'
    endif
  end subroutine mgx_mk1d

  subroutine mg411_secs (mxd, x, v)
    integer, intent (in) :: mxd
    real , intent (in) :: x(0:mg411_nr-1)
    real , intent (out) :: v(0:mg411_ivs(mxd)-1)
    !! Note: We stop at degree 8 for now.
    !-----------------------------------------------------------------------
    integer, parameter :: m=4, nk=6, npv=66, &
      m2=m*(m-1), m3=m*(m-1)*(m-2)
    integer :: i0, i1, i2, j0, k0
    real  :: pv(0:npv-1), d(0:nk-1,0:nk-1), d2(0:nk-1,0:nk-1), &
      d3(0:nk-1,0:nk-1), d4(0:nk-1,0:nk-1), d5(0:nk-1,0:nk-1), &
      d6(0:nk-1,0:nk-1), d7(0:nk-1,0:nk-1)
    call mg411_setd ()
    pv = 0
    j0 = m
    k0 = m+1
    do i0 = 0, m-1
     if (2.le.mxd) then
      call mg411_deg2_i0 ()
     endif
     if (3.le.mxd) then
      call mg411_deg3_i0 ()
     endif
     if (4.le.mxd) then
      call mg411_deg4_i0 ()
     endif
     do i1 = 0, m-1
     if (i1.ne.i0) then
      if (2.le.mxd) then
       call mg411_deg2_i1 ()
      endif
      if (3.le.mxd) then
       call mg411_deg3_i1 ()
      endif
      if (4.le.mxd) then
       call mg411_deg4_i1 ()
      endif
      if (5.le.mxd) then
       call mg411_deg5_i1 ()
      endif
      do i2 = 0, m-1
      if (i2.ne.i0.and.i2.ne.i1) then
       if (3.le.mxd) then
        call mg411_deg3_i2 ()
       endif
       if (4.le.mxd) then
        call mg411_deg4_i2 ()
       endif
       if (5.le.mxd) then
        call mg411_deg5_i2 ()
       endif
       if (6.le.mxd) then
        call mg411_deg6_i2 ()
       endif
      endif
      enddo
     endif
     enddo
    enddo
    v(0) = 1
    if (2.le.mxd) then
     v(1:3) = pv(0:2)
    endif
    if (3.le.mxd) then
     v(4:16) = pv(3:15)
    endif
    if (4.le.mxd) then
     v(17) = pv(0)*pv(0)
     v(18) = pv(0)*pv(1)
     v(19) = pv(1)*pv(1)
     v(20) = pv(0)*pv(2)
     v(21) = pv(1)*pv(2)
     v(22) = pv(2)*pv(2)
     v(23:48) = pv(16:41)
    endif
    if (5.le.mxd) then
     v(49) = pv(0)*pv(3)
     v(50) = pv(1)*pv(3)
     v(51) = pv(2)*pv(3)
     v(52) = pv(0)*pv(4)
     v(53) = pv(1)*pv(4)
     v(54) = pv(2)*pv(4)
     v(55) = pv(0)*pv(5)
     v(56) = pv(1)*pv(5)
     v(57) = pv(2)*pv(5)
     v(58) = pv(0)*pv(6)
     v(59) = pv(1)*pv(6)
     v(60) = pv(2)*pv(6)
     v(61) = pv(0)*pv(7)
     v(62) = pv(1)*pv(7)
     v(63) = pv(2)*pv(7)
     v(64) = pv(0)*pv(8)
     v(65) = pv(1)*pv(8)
     v(66) = pv(2)*pv(8)
     v(67) = pv(0)*pv(9)
     v(68) = pv(1)*pv(9)
     v(69) = pv(2)*pv(9)
     v(70) = pv(0)*pv(10)
     v(71) = pv(1)*pv(10)
     v(72) = pv(2)*pv(10)
     v(73) = pv(0)*pv(11)
     v(74) = pv(1)*pv(11)
     v(75) = pv(2)*pv(11)
     v(76) = pv(0)*pv(12)
     v(77) = pv(1)*pv(12)
     v(78) = pv(2)*pv(12)
     v(79) = pv(0)*pv(13)
     v(80) = pv(1)*pv(13)
     v(81) = pv(2)*pv(13)
     v(82) = pv(0)*pv(14)
     v(83) = pv(1)*pv(14)
     v(84) = pv(2)*pv(14)
     v(85) = pv(0)*pv(15)
     v(86) = pv(1)*pv(15)
     v(87) = pv(2)*pv(15)
     v(88:110) = pv(42:64)
    endif
    if (6.le.mxd) then
     v(111) = pv(3)*pv(3)
     v(112) = pv(3)*pv(4)
     v(113) = pv(3)*pv(5)
     v(114) = pv(3)*pv(6)
     v(115) = pv(5)*pv(6)
     v(116) = pv(3)*pv(7)
     v(117) = pv(4)*pv(7)
     v(118) = pv(5)*pv(7)
     v(119) = pv(6)*pv(7)
     v(120) = pv(7)*pv(7)
     v(121) = pv(3)*pv(8)
     v(122) = pv(3)*pv(9)
     v(123) = pv(4)*pv(9)
     v(124) = pv(3)*pv(10)
     v(125) = pv(5)*pv(10)
     v(126) = pv(3)*pv(11)
     v(127) = pv(4)*pv(11)
     v(128) = pv(5)*pv(11)
     v(129) = pv(7)*pv(11)
     v(130) = pv(3)*pv(12)
     v(131) = pv(4)*pv(12)
     v(132) = pv(5)*pv(12)
     v(133) = pv(6)*pv(12)
     v(134) = pv(7)*pv(12)
     v(135) = pv(3)*pv(13)
     v(136) = pv(5)*pv(13)
     v(137) = pv(7)*pv(13)
     v(138) = pv(9)*pv(13)
     v(139) = pv(3)*pv(14)
     v(140) = pv(4)*pv(14)
     v(141) = pv(5)*pv(14)
     v(142) = pv(6)*pv(14)
     v(143) = pv(7)*pv(14)
     v(144) = pv(8)*pv(14)
     v(145) = pv(9)*pv(14)
     v(146) = pv(11)*pv(14)
     v(147) = pv(12)*pv(14)
     v(148) = pv(3)*pv(15)
     v(149) = pv(4)*pv(15)
     v(150) = pv(5)*pv(15)
     v(151) = pv(6)*pv(15)
     v(152) = pv(7)*pv(15)
     v(153) = pv(8)*pv(15)
     v(154) = pv(9)*pv(15)
     v(155) = pv(10)*pv(15)
     v(156) = pv(11)*pv(15)
     v(157) = pv(12)*pv(15)
     v(158) = pv(13)*pv(15)
     v(159) = pv(14)*pv(15)
     v(160) = pv(15)*pv(15)
     v(161) = pv(0)*pv(16)
     v(162) = pv(1)*pv(16)
     v(163) = pv(2)*pv(16)
     v(164) = pv(0)*pv(17)
     v(165) = pv(1)*pv(17)
     v(166) = pv(2)*pv(17)
     v(167) = pv(0)*pv(18)
     v(168) = pv(1)*pv(18)
     v(169) = pv(2)*pv(18)
     v(170) = pv(0)*pv(19)
     v(171) = pv(1)*pv(19)
     v(172) = pv(2)*pv(19)
     v(173) = pv(0)*pv(20)
     v(174) = pv(1)*pv(20)
     v(175) = pv(2)*pv(20)
     v(176) = pv(0)*pv(21)
     v(177) = pv(1)*pv(21)
     v(178) = pv(2)*pv(21)
     v(179) = pv(0)*pv(22)
     v(180) = pv(1)*pv(22)
     v(181) = pv(2)*pv(22)
     v(182) = pv(0)*pv(23)
     v(183) = pv(1)*pv(23)
     v(184) = pv(2)*pv(23)
     v(185) = pv(0)*pv(24)
     v(186) = pv(1)*pv(24)
     v(187) = pv(2)*pv(24)
     v(188) = pv(0)*pv(25)
     v(189) = pv(1)*pv(25)
     v(190) = pv(2)*pv(25)
     v(191) = pv(0)*pv(26)
     v(192) = pv(1)*pv(26)
     v(193) = pv(2)*pv(26)
     v(194) = pv(0)*pv(27)
     v(195) = pv(1)*pv(27)
     v(196) = pv(2)*pv(27)
     v(197) = pv(0)*pv(28)
     v(198) = pv(1)*pv(28)
     v(199) = pv(2)*pv(28)
     v(200) = pv(0)*pv(29)
     v(201) = pv(1)*pv(29)
     v(202) = pv(2)*pv(29)
     v(203) = pv(0)*pv(30)
     v(204) = pv(1)*pv(30)
     v(205) = pv(2)*pv(30)
     v(206) = pv(0)*pv(31)
     v(207) = pv(1)*pv(31)
     v(208) = pv(2)*pv(31)
     v(209) = pv(0)*pv(32)
     v(210) = pv(1)*pv(32)
     v(211) = pv(2)*pv(32)
     v(212) = pv(0)*pv(33)
     v(213) = pv(1)*pv(33)
     v(214) = pv(2)*pv(33)
     v(215) = pv(0)*pv(34)
     v(216) = pv(1)*pv(34)
     v(217) = pv(2)*pv(34)
     v(218) = pv(0)*pv(35)
     v(219) = pv(1)*pv(35)
     v(220) = pv(2)*pv(35)
     v(221) = pv(0)*pv(36)
     v(222) = pv(1)*pv(36)
     v(223) = pv(2)*pv(36)
     v(224) = pv(0)*pv(37)
     v(225) = pv(1)*pv(37)
     v(226) = pv(2)*pv(37)
     v(227) = pv(0)*pv(38)
     v(228) = pv(1)*pv(38)
     v(229) = pv(2)*pv(38)
     v(230) = pv(0)*pv(39)
     v(231) = pv(1)*pv(39)
     v(232) = pv(2)*pv(39)
     v(233) = pv(0)*pv(40)
     v(234) = pv(1)*pv(40)
     v(235) = pv(2)*pv(40)
     v(236) = pv(0)*pv(41)
     v(237) = pv(1)*pv(41)
     v(238) = pv(2)*pv(41)
     v(239:239) = pv(65:65)
    endif
    if (7.le.mxd) then
     v(240) = pv(3)*pv(18)
     v(241) = pv(3)*pv(19)
     v(242) = pv(3)*pv(20)
     v(243) = pv(5)*pv(20)
     v(244) = pv(3)*pv(21)
     v(245) = pv(4)*pv(21)
     v(246) = pv(5)*pv(21)
     v(247) = pv(3)*pv(22)
     v(248) = pv(5)*pv(22)
     v(249) = pv(3)*pv(23)
     v(250) = pv(4)*pv(23)
     v(251) = pv(5)*pv(23)
     v(252) = pv(6)*pv(23)
     v(253) = pv(7)*pv(23)
     v(254) = pv(12)*pv(23)
     v(255) = pv(3)*pv(25)
     v(256) = pv(3)*pv(26)
     v(257) = pv(4)*pv(26)
     v(258) = pv(5)*pv(26)
     v(259) = pv(6)*pv(26)
     v(260) = pv(7)*pv(26)
     v(261) = pv(3)*pv(27)
     v(262) = pv(4)*pv(27)
     v(263) = pv(5)*pv(27)
     v(264) = pv(3)*pv(28)
     v(265) = pv(4)*pv(28)
     v(266) = pv(5)*pv(28)
     v(267) = pv(6)*pv(28)
     v(268) = pv(7)*pv(28)
     v(269) = pv(3)*pv(29)
     v(270) = pv(4)*pv(29)
     v(271) = pv(5)*pv(29)
     v(272) = pv(7)*pv(29)
     v(273) = pv(12)*pv(29)
     v(274) = pv(3)*pv(30)
     v(275) = pv(4)*pv(30)
     v(276) = pv(5)*pv(30)
     v(277) = pv(7)*pv(30)
     v(278) = pv(9)*pv(30)
     v(279) = pv(12)*pv(30)
     v(280) = pv(14)*pv(30)
     v(281) = pv(15)*pv(30)
     v(282) = pv(3)*pv(31)
     v(283) = pv(4)*pv(31)
     v(284) = pv(5)*pv(31)
     v(285) = pv(6)*pv(31)
     v(286) = pv(7)*pv(31)
     v(287) = pv(8)*pv(31)
     v(288) = pv(9)*pv(31)
     v(289) = pv(10)*pv(31)
     v(290) = pv(11)*pv(31)
     v(291) = pv(12)*pv(31)
     v(292) = pv(3)*pv(32)
     v(293) = pv(4)*pv(32)
     v(294) = pv(5)*pv(32)
     v(295) = pv(6)*pv(32)
     v(296) = pv(7)*pv(32)
     v(297) = pv(8)*pv(32)
     v(298) = pv(9)*pv(32)
     v(299) = pv(10)*pv(32)
     v(300) = pv(11)*pv(32)
     v(301) = pv(12)*pv(32)
     v(302) = pv(13)*pv(32)
     v(303) = pv(14)*pv(32)
     v(304) = pv(15)*pv(32)
     v(305) = pv(3)*pv(33)
     v(306) = pv(4)*pv(33)
     v(307) = pv(5)*pv(33)
     v(308) = pv(6)*pv(33)
     v(309) = pv(7)*pv(33)
     v(310) = pv(8)*pv(33)
     v(311) = pv(9)*pv(33)
     v(312) = pv(10)*pv(33)
     v(313) = pv(11)*pv(33)
     v(314) = pv(12)*pv(33)
     v(315) = pv(14)*pv(33)
     v(316) = pv(15)*pv(33)
     v(317) = pv(3)*pv(34)
     v(318) = pv(4)*pv(34)
     v(319) = pv(5)*pv(34)
     v(320) = pv(6)*pv(34)
     v(321) = pv(7)*pv(34)
     v(322) = pv(9)*pv(34)
     v(323) = pv(3)*pv(35)
     v(324) = pv(4)*pv(35)
     v(325) = pv(5)*pv(35)
     v(326) = pv(6)*pv(35)
     v(327) = pv(7)*pv(35)
     v(328) = pv(8)*pv(35)
     v(329) = pv(9)*pv(35)
     v(330) = pv(12)*pv(35)
     v(331) = pv(3)*pv(36)
     v(332) = pv(4)*pv(36)
     v(333) = pv(5)*pv(36)
     v(334) = pv(6)*pv(36)
     v(335) = pv(7)*pv(36)
     v(336) = pv(8)*pv(36)
     v(337) = pv(9)*pv(36)
     v(338) = pv(12)*pv(36)
     v(339) = pv(3)*pv(37)
     v(340) = pv(4)*pv(37)
     v(341) = pv(5)*pv(37)
     v(342) = pv(6)*pv(37)
     v(343) = pv(7)*pv(37)
     v(344) = pv(8)*pv(37)
     v(345) = pv(9)*pv(37)
     v(346) = pv(10)*pv(37)
     v(347) = pv(12)*pv(37)
     v(348) = pv(15)*pv(37)
     v(349) = pv(3)*pv(38)
     v(350) = pv(4)*pv(38)
     v(351) = pv(5)*pv(38)
     v(352) = pv(6)*pv(38)
     v(353) = pv(7)*pv(38)
     v(354) = pv(8)*pv(38)
     v(355) = pv(9)*pv(38)
     v(356) = pv(10)*pv(38)
     v(357) = pv(11)*pv(38)
     v(358) = pv(12)*pv(38)
     v(359) = pv(3)*pv(39)
     v(360) = pv(4)*pv(39)
     v(361) = pv(5)*pv(39)
     v(362) = pv(6)*pv(39)
     v(363) = pv(7)*pv(39)
     v(364) = pv(9)*pv(39)
     v(365) = pv(10)*pv(39)
     v(366) = pv(11)*pv(39)
     v(367) = pv(12)*pv(39)
     v(368) = pv(3)*pv(40)
     v(369) = pv(4)*pv(40)
     v(370) = pv(5)*pv(40)
     v(371) = pv(6)*pv(40)
     v(372) = pv(7)*pv(40)
     v(373) = pv(8)*pv(40)
     v(374) = pv(9)*pv(40)
     v(375) = pv(10)*pv(40)
     v(376) = pv(11)*pv(40)
     v(377) = pv(12)*pv(40)
     v(378) = pv(15)*pv(40)
     v(379) = pv(3)*pv(41)
     v(380) = pv(4)*pv(41)
     v(381) = pv(5)*pv(41)
     v(382) = pv(6)*pv(41)
     v(383) = pv(7)*pv(41)
     v(384) = pv(8)*pv(41)
     v(385) = pv(9)*pv(41)
     v(386) = pv(10)*pv(41)
     v(387) = pv(11)*pv(41)
     v(388) = pv(12)*pv(41)
     v(389) = pv(13)*pv(41)
     v(390) = pv(14)*pv(41)
     v(391) = pv(15)*pv(41)
     v(392) = pv(0)*pv(42)
     v(393) = pv(1)*pv(42)
     v(394) = pv(2)*pv(42)
     v(395) = pv(0)*pv(43)
     v(396) = pv(1)*pv(43)
     v(397) = pv(2)*pv(43)
     v(398) = pv(0)*pv(44)
     v(399) = pv(1)*pv(44)
     v(400) = pv(2)*pv(44)
     v(401) = pv(0)*pv(45)
     v(402) = pv(1)*pv(45)
     v(403) = pv(2)*pv(45)
     v(404) = pv(0)*pv(46)
     v(405) = pv(1)*pv(46)
     v(406) = pv(2)*pv(46)
     v(407) = pv(0)*pv(47)
     v(408) = pv(1)*pv(47)
     v(409) = pv(2)*pv(47)
     v(410) = pv(0)*pv(48)
     v(411) = pv(1)*pv(48)
     v(412) = pv(2)*pv(48)
     v(413) = pv(0)*pv(49)
     v(414) = pv(1)*pv(49)
     v(415) = pv(2)*pv(49)
     v(416) = pv(0)*pv(50)
     v(417) = pv(1)*pv(50)
     v(418) = pv(2)*pv(50)
     v(419) = pv(0)*pv(51)
     v(420) = pv(1)*pv(51)
     v(421) = pv(2)*pv(51)
     v(422) = pv(0)*pv(52)
     v(423) = pv(1)*pv(52)
     v(424) = pv(2)*pv(52)
     v(425) = pv(0)*pv(53)
     v(426) = pv(1)*pv(53)
     v(427) = pv(2)*pv(53)
     v(428) = pv(0)*pv(54)
     v(429) = pv(1)*pv(54)
     v(430) = pv(2)*pv(54)
     v(431) = pv(0)*pv(55)
     v(432) = pv(1)*pv(55)
     v(433) = pv(2)*pv(55)
     v(434) = pv(0)*pv(56)
     v(435) = pv(1)*pv(56)
     v(436) = pv(2)*pv(56)
     v(437) = pv(0)*pv(57)
     v(438) = pv(1)*pv(57)
     v(439) = pv(2)*pv(57)
     v(440) = pv(0)*pv(58)
     v(441) = pv(1)*pv(58)
     v(442) = pv(2)*pv(58)
     v(443) = pv(0)*pv(59)
     v(444) = pv(1)*pv(59)
     v(445) = pv(2)*pv(59)
     v(446) = pv(0)*pv(60)
     v(447) = pv(1)*pv(60)
     v(448) = pv(2)*pv(60)
     v(449) = pv(0)*pv(61)
     v(450) = pv(1)*pv(61)
     v(451) = pv(2)*pv(61)
     v(452) = pv(0)*pv(62)
     v(453) = pv(1)*pv(62)
     v(454) = pv(2)*pv(62)
     v(455) = pv(0)*pv(63)
     v(456) = pv(1)*pv(63)
     v(457) = pv(2)*pv(63)
     v(458) = pv(0)*pv(64)
     v(459) = pv(1)*pv(64)
     v(460) = pv(2)*pv(64)
    endif
    if (8.le.mxd) then
     v(461) = pv(2)*pv(2)*pv(33)
     v(462) = pv(21)*pv(23)
     v(463) = pv(22)*pv(23)
     v(464) = pv(23)*pv(23)
     v(465) = pv(22)*pv(32)
     v(466) = pv(17)*pv(33)
     v(467) = pv(18)*pv(33)
     v(468) = pv(19)*pv(33)
     v(469) = pv(20)*pv(33)
     v(470) = pv(21)*pv(33)
     v(471) = pv(22)*pv(33)
     v(472) = pv(23)*pv(33)
     v(473) = pv(29)*pv(33)
     v(474) = pv(31)*pv(33)
     v(475) = pv(32)*pv(33)
     v(476) = pv(23)*pv(36)
     v(477) = pv(17)*pv(37)
     v(478) = pv(18)*pv(37)
     v(479) = pv(19)*pv(37)
     v(480) = pv(21)*pv(37)
     v(481) = pv(22)*pv(37)
     v(482) = pv(23)*pv(37)
     v(483) = pv(28)*pv(37)
     v(484) = pv(29)*pv(37)
     v(485) = pv(30)*pv(37)
     v(486) = pv(31)*pv(37)
     v(487) = pv(32)*pv(37)
     v(488) = pv(33)*pv(37)
     v(489) = pv(37)*pv(37)
     v(490) = pv(19)*pv(38)
     v(491) = pv(20)*pv(38)
     v(492) = pv(21)*pv(38)
     v(493) = pv(22)*pv(38)
     v(494) = pv(23)*pv(38)
     v(495) = pv(29)*pv(38)
     v(496) = pv(31)*pv(38)
     v(497) = pv(32)*pv(38)
     v(498) = pv(33)*pv(38)
     v(499) = pv(37)*pv(38)
     v(500) = pv(20)*pv(39)
     v(501) = pv(21)*pv(39)
     v(502) = pv(22)*pv(39)
     v(503) = pv(23)*pv(39)
     v(504) = pv(30)*pv(39)
     v(505) = pv(32)*pv(39)
     v(506) = pv(33)*pv(39)
     v(507) = pv(37)*pv(39)
     v(508) = pv(17)*pv(40)
     v(509) = pv(18)*pv(40)
     v(510) = pv(19)*pv(40)
     v(511) = pv(20)*pv(40)
     v(512) = pv(21)*pv(40)
     v(513) = pv(22)*pv(40)
     v(514) = pv(23)*pv(40)
     v(515) = pv(24)*pv(40)
     v(516) = pv(26)*pv(40)
     v(517) = pv(28)*pv(40)
     v(518) = pv(29)*pv(40)
     v(519) = pv(30)*pv(40)
     v(520) = pv(31)*pv(40)
     v(521) = pv(32)*pv(40)
     v(522) = pv(33)*pv(40)
     v(523) = pv(37)*pv(40)
     v(524) = pv(20)*pv(41)
     v(525) = pv(21)*pv(41)
     v(526) = pv(22)*pv(41)
     v(527) = pv(23)*pv(41)
     v(528) = pv(28)*pv(41)
     v(529) = pv(29)*pv(41)
     v(530) = pv(30)*pv(41)
     v(531) = pv(31)*pv(41)
     v(532) = pv(32)*pv(41)
     v(533) = pv(33)*pv(41)
     v(534) = pv(35)*pv(41)
     v(535) = pv(36)*pv(41)
     v(536) = pv(37)*pv(41)
     v(537) = pv(38)*pv(41)
     v(538) = pv(39)*pv(41)
     v(539) = pv(40)*pv(41)
     v(540) = pv(41)*pv(41)
     v(541) = pv(3)*pv(43)
     v(542) = pv(6)*pv(43)
     v(543) = pv(7)*pv(43)
     v(544) = pv(11)*pv(43)
     v(545) = pv(3)*pv(44)
     v(546) = pv(4)*pv(44)
     v(547) = pv(5)*pv(44)
     v(548) = pv(6)*pv(44)
     v(549) = pv(7)*pv(44)
     v(550) = pv(9)*pv(44)
     v(551) = pv(10)*pv(44)
     v(552) = pv(11)*pv(44)
     v(553) = pv(12)*pv(44)
     v(554) = pv(13)*pv(44)
     v(555) = pv(14)*pv(44)
     v(556) = pv(15)*pv(44)
     v(557) = pv(3)*pv(45)
     v(558) = pv(5)*pv(45)
     v(559) = pv(7)*pv(45)
     v(560) = pv(8)*pv(45)
     v(561) = pv(9)*pv(45)
     v(562) = pv(10)*pv(45)
     v(563) = pv(12)*pv(45)
     v(564) = pv(13)*pv(45)
     v(565) = pv(14)*pv(45)
     v(566) = pv(15)*pv(45)
     v(567) = pv(3)*pv(46)
     v(568) = pv(4)*pv(46)
     v(569) = pv(5)*pv(46)
     v(570) = pv(6)*pv(46)
     v(571) = pv(7)*pv(46)
     v(572) = pv(8)*pv(46)
     v(573) = pv(9)*pv(46)
     v(574) = pv(10)*pv(46)
     v(575) = pv(11)*pv(46)
     v(576) = pv(12)*pv(46)
     v(577) = pv(13)*pv(46)
     v(578) = pv(14)*pv(46)
     v(579) = pv(15)*pv(46)
     v(580) = pv(3)*pv(47)
     v(581) = pv(5)*pv(47)
     v(582) = pv(8)*pv(47)
     v(583) = pv(9)*pv(47)
     v(584) = pv(10)*pv(47)
     v(585) = pv(12)*pv(47)
     v(586) = pv(13)*pv(47)
     v(587) = pv(14)*pv(47)
     v(588) = pv(15)*pv(47)
     v(589) = pv(3)*pv(48)
     v(590) = pv(4)*pv(48)
     v(591) = pv(5)*pv(48)
     v(592) = pv(8)*pv(48)
     v(593) = pv(9)*pv(48)
     v(594) = pv(10)*pv(48)
     v(595) = pv(12)*pv(48)
     v(596) = pv(13)*pv(48)
     v(597) = pv(14)*pv(48)
     v(598) = pv(15)*pv(48)
     v(599) = pv(3)*pv(49)
     v(600) = pv(4)*pv(49)
     v(601) = pv(5)*pv(49)
     v(602) = pv(6)*pv(49)
     v(603) = pv(7)*pv(49)
     v(604) = pv(10)*pv(49)
     v(605) = pv(11)*pv(49)
     v(606) = pv(12)*pv(49)
     v(607) = pv(13)*pv(49)
     v(608) = pv(14)*pv(49)
     v(609) = pv(15)*pv(49)
     v(610) = pv(3)*pv(50)
     v(611) = pv(4)*pv(50)
     v(612) = pv(5)*pv(50)
     v(613) = pv(6)*pv(50)
     v(614) = pv(7)*pv(50)
     v(615) = pv(8)*pv(50)
     v(616) = pv(9)*pv(50)
     v(617) = pv(10)*pv(50)
     v(618) = pv(11)*pv(50)
     v(619) = pv(12)*pv(50)
     v(620) = pv(13)*pv(50)
     v(621) = pv(14)*pv(50)
     v(622) = pv(15)*pv(50)
     v(623) = pv(3)*pv(51)
     v(624) = pv(4)*pv(51)
     v(625) = pv(5)*pv(51)
     v(626) = pv(6)*pv(51)
     v(627) = pv(7)*pv(51)
     v(628) = pv(8)*pv(51)
     v(629) = pv(9)*pv(51)
     v(630) = pv(10)*pv(51)
     v(631) = pv(11)*pv(51)
     v(632) = pv(12)*pv(51)
     v(633) = pv(13)*pv(51)
     v(634) = pv(14)*pv(51)
     v(635) = pv(15)*pv(51)
     v(636) = pv(3)*pv(52)
     v(637) = pv(4)*pv(52)
     v(638) = pv(5)*pv(52)
     v(639) = pv(6)*pv(52)
     v(640) = pv(7)*pv(52)
     v(641) = pv(8)*pv(52)
     v(642) = pv(9)*pv(52)
     v(643) = pv(10)*pv(52)
     v(644) = pv(11)*pv(52)
     v(645) = pv(12)*pv(52)
     v(646) = pv(13)*pv(52)
     v(647) = pv(14)*pv(52)
     v(648) = pv(15)*pv(52)
     v(649) = pv(3)*pv(53)
     v(650) = pv(4)*pv(53)
     v(651) = pv(5)*pv(53)
     v(652) = pv(6)*pv(53)
     v(653) = pv(7)*pv(53)
     v(654) = pv(8)*pv(53)
     v(655) = pv(9)*pv(53)
     v(656) = pv(10)*pv(53)
     v(657) = pv(11)*pv(53)
     v(658) = pv(12)*pv(53)
     v(659) = pv(13)*pv(53)
     v(660) = pv(14)*pv(53)
     v(661) = pv(15)*pv(53)
     v(662) = pv(3)*pv(54)
     v(663) = pv(4)*pv(54)
     v(664) = pv(5)*pv(54)
     v(665) = pv(6)*pv(54)
     v(666) = pv(7)*pv(54)
     v(667) = pv(8)*pv(54)
     v(668) = pv(9)*pv(54)
     v(669) = pv(10)*pv(54)
     v(670) = pv(11)*pv(54)
     v(671) = pv(12)*pv(54)
     v(672) = pv(13)*pv(54)
     v(673) = pv(14)*pv(54)
     v(674) = pv(15)*pv(54)
     v(675) = pv(3)*pv(55)
     v(676) = pv(4)*pv(55)
     v(677) = pv(5)*pv(55)
     v(678) = pv(6)*pv(55)
     v(679) = pv(7)*pv(55)
     v(680) = pv(8)*pv(55)
     v(681) = pv(9)*pv(55)
     v(682) = pv(10)*pv(55)
     v(683) = pv(11)*pv(55)
     v(684) = pv(12)*pv(55)
     v(685) = pv(13)*pv(55)
     v(686) = pv(14)*pv(55)
     v(687) = pv(15)*pv(55)
     v(688) = pv(3)*pv(56)
     v(689) = pv(4)*pv(56)
     v(690) = pv(5)*pv(56)
     v(691) = pv(6)*pv(56)
     v(692) = pv(7)*pv(56)
     v(693) = pv(8)*pv(56)
     v(694) = pv(9)*pv(56)
     v(695) = pv(10)*pv(56)
     v(696) = pv(11)*pv(56)
     v(697) = pv(12)*pv(56)
     v(698) = pv(13)*pv(56)
     v(699) = pv(14)*pv(56)
     v(700) = pv(15)*pv(56)
     v(701) = pv(3)*pv(57)
     v(702) = pv(4)*pv(57)
     v(703) = pv(5)*pv(57)
     v(704) = pv(6)*pv(57)
     v(705) = pv(7)*pv(57)
     v(706) = pv(9)*pv(57)
     v(707) = pv(10)*pv(57)
     v(708) = pv(11)*pv(57)
     v(709) = pv(12)*pv(57)
     v(710) = pv(15)*pv(57)
     v(711) = pv(3)*pv(58)
     v(712) = pv(4)*pv(58)
     v(713) = pv(5)*pv(58)
     v(714) = pv(6)*pv(58)
     v(715) = pv(7)*pv(58)
     v(716) = pv(8)*pv(58)
     v(717) = pv(9)*pv(58)
     v(718) = pv(10)*pv(58)
     v(719) = pv(11)*pv(58)
     v(720) = pv(12)*pv(58)
     v(721) = pv(13)*pv(58)
     v(722) = pv(15)*pv(58)
     v(723) = pv(3)*pv(59)
     v(724) = pv(4)*pv(59)
     v(725) = pv(5)*pv(59)
     v(726) = pv(6)*pv(59)
     v(727) = pv(7)*pv(59)
     v(728) = pv(8)*pv(59)
     v(729) = pv(9)*pv(59)
     v(730) = pv(10)*pv(59)
     v(731) = pv(11)*pv(59)
     v(732) = pv(12)*pv(59)
     v(733) = pv(13)*pv(59)
     v(734) = pv(15)*pv(59)
     v(735) = pv(3)*pv(60)
     v(736) = pv(4)*pv(60)
     v(737) = pv(5)*pv(60)
     v(738) = pv(6)*pv(60)
     v(739) = pv(7)*pv(60)
     v(740) = pv(8)*pv(60)
     v(741) = pv(9)*pv(60)
     v(742) = pv(10)*pv(60)
     v(743) = pv(11)*pv(60)
     v(744) = pv(12)*pv(60)
     v(745) = pv(13)*pv(60)
     v(746) = pv(14)*pv(60)
     v(747) = pv(15)*pv(60)
     v(748) = pv(3)*pv(61)
     v(749) = pv(4)*pv(61)
     v(750) = pv(5)*pv(61)
     v(751) = pv(6)*pv(61)
     v(752) = pv(7)*pv(61)
     v(753) = pv(8)*pv(61)
     v(754) = pv(9)*pv(61)
     v(755) = pv(10)*pv(61)
     v(756) = pv(11)*pv(61)
     v(757) = pv(12)*pv(61)
     v(758) = pv(13)*pv(61)
     v(759) = pv(14)*pv(61)
     v(760) = pv(15)*pv(61)
     v(761) = pv(3)*pv(62)
     v(762) = pv(4)*pv(62)
     v(763) = pv(5)*pv(62)
     v(764) = pv(6)*pv(62)
     v(765) = pv(7)*pv(62)
     v(766) = pv(9)*pv(62)
     v(767) = pv(10)*pv(62)
     v(768) = pv(11)*pv(62)
     v(769) = pv(12)*pv(62)
     v(770) = pv(14)*pv(62)
     v(771) = pv(3)*pv(63)
     v(772) = pv(4)*pv(63)
     v(773) = pv(5)*pv(63)
     v(774) = pv(6)*pv(63)
     v(775) = pv(7)*pv(63)
     v(776) = pv(8)*pv(63)
     v(777) = pv(9)*pv(63)
     v(778) = pv(10)*pv(63)
     v(779) = pv(11)*pv(63)
     v(780) = pv(12)*pv(63)
     v(781) = pv(14)*pv(63)
     v(782) = pv(15)*pv(63)
     v(783) = pv(3)*pv(64)
     v(784) = pv(4)*pv(64)
     v(785) = pv(5)*pv(64)
     v(786) = pv(6)*pv(64)
     v(787) = pv(7)*pv(64)
     v(788) = pv(8)*pv(64)
     v(789) = pv(9)*pv(64)
     v(790) = pv(10)*pv(64)
     v(791) = pv(11)*pv(64)
     v(792) = pv(12)*pv(64)
     v(793) = pv(0)*pv(65)
     v(794) = pv(1)*pv(65)
     v(795) = pv(2)*pv(65)
    endif
    if (9.le.mxd) then
     stop 'mg411: degree 9 not implemented'
    endif
    return
    contains
    subroutine mg411_setd ()
     integer :: i, j, k, l0, l1
     integer, parameter, dimension(0:3) :: lb=(/ 0, m, m+1, m+2 /)
     k = 0
     do l1 = 0, 2
      do l0 = 0, l1
       do j = lb(l1), lb(l1+1)-1
        do i = lb(l0), min(j-1,lb(l0+1)-1)
         d(i,j) = x(k)
         d(j,i) = d(i,j)
         d2(i,j) = x(k)**2
         d2(j,i) = d2(i,j)
         d3(i,j) = x(k)**3
         d3(j,i) = d3(i,j)
         d4(i,j) = x(k)**4
         d4(j,i) = d4(i,j)
         d5(i,j) = x(k)**5
         d5(j,i) = d5(i,j)
         d6(i,j) = x(k)**6
         d6(j,i) = d6(i,j)
         d7(i,j) = x(k)**7
         d7(j,i) = d7(i,j)
         k = k+1
        enddo
       enddo
      enddo
     enddo
     if (k.ne.mg411_nr) then
      stop 'mg411_setd: bad count'
     endif
     do i = 0, nk-1
      d(i,i) = 0
      d2(i,i) = 0
      d3(i,i) = 0
      d4(i,i) = 0
      d5(i,i) = 0
      d6(i,i) = 0
      d7(i,i) = 0
     enddo
    end subroutine mg411_setd
    subroutine mg411_deg2_i0 ()
    ! terms may involve j0 and k0
     pv(2) = pv(2)+d(i0,j0)*d(i0,k0)/m
    end subroutine mg411_deg2_i0
    subroutine mg411_deg2_i1 ()
    ! terms may involve j0 and k0
     pv(0) = pv(0)+d(i0,i1)*d(i0,j0)/m2
     pv(1) = pv(1)+d(i0,i1)*d(i0,k0)/m2
    end subroutine mg411_deg2_i1
    subroutine mg411_deg3_i0 ()
    ! terms may involve j0 and k0
     pv(11) = pv(11)+d2(i0,j0)*d(i0,k0)/m
     pv(14) = pv(14)+d(i0,j0)*d2(i0,k0)/m
    end subroutine mg411_deg3_i0
    subroutine mg411_deg3_i1 ()
    ! terms may involve j0 and k0
     pv(4) = pv(4)+d2(i0,i1)*d(i0,j0)/m2
     pv(6) = pv(6)+d(i0,i1)*d2(i0,j0)/m2
     pv(7) = pv(7)+d(i0,i1)*d(i0,j0)*d(i1,j0)/m2
     pv(8) = pv(8)+d2(i0,i1)*d(i0,k0)/m2
     pv(10) = pv(10)+d(i0,i1)*d(i0,j0)*d(i0,k0)/m2
     pv(12) = pv(12)+d(i0,i1)*d(i1,j0)*d(i0,k0)/m2
     pv(13) = pv(13)+d(i0,i1)*d2(i0,k0)/m2
     pv(15) = pv(15)+d(i0,i1)*d(i0,k0)*d(i1,k0)/m2
    end subroutine mg411_deg3_i1
    subroutine mg411_deg3_i2 ()
    ! terms may involve j0 and k0
     pv(3) = pv(3)+d2(i0,i1)*d(i0,i2)/m3
     pv(5) = pv(5)+d(i0,i1)*d(i0,i2)*d(i0,j0)/m3
     pv(9) = pv(9)+d(i0,i1)*d(i0,i2)*d(i0,k0)/m3
    end subroutine mg411_deg3_i2
    subroutine mg411_deg4_i0 ()
     pv(30) = pv(30)+d3(i0,j0)*d(i0,k0)/m
     pv(37) = pv(37)+d2(i0,j0)*d2(i0,k0)/m
     pv(40) = pv(40)+d(i0,j0)*d3(i0,k0)/m
    end subroutine mg411_deg4_i0
    subroutine mg411_deg4_i1 ()
     pv(16) = pv(16)+d4(i0,i1)/m2
     pv(17) = pv(17)+d3(i0,i1)*d(i0,j0)/m2
     pv(20) = pv(20)+d2(i0,i1)*d2(i0,j0)/m2
     pv(22) = pv(22)+d(i0,i1)*d3(i0,j0)/m2
     pv(23) = pv(23)+d2(i0,i1)*d(i0,j0)*d(i1,j0)/m2
     pv(24) = pv(24)+d3(i0,i1)*d(i0,k0)/m2
     pv(27) = pv(27)+d2(i0,i1)*d(i0,j0)*d(i0,k0)/m2
     pv(29) = pv(29)+d(i0,i1)*d2(i0,j0)*d(i0,k0)/m2
     pv(31) = pv(31)+d2(i0,i1)*d(i1,j0)*d(i0,k0)/m2
     pv(33) = pv(33)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,k0)/m2
     pv(34) = pv(34)+d2(i0,i1)*d2(i0,k0)/m2
     pv(36) = pv(36)+d(i0,i1)*d(i0,j0)*d2(i0,k0)/m2
     pv(38) = pv(38)+d(i0,i1)*d(i1,j0)*d2(i0,k0)/m2
     pv(39) = pv(39)+d(i0,i1)*d3(i0,k0)/m2
     pv(41) = pv(41)+d2(i0,i1)*d(i0,k0)*d(i1,k0)/m2
    end subroutine mg411_deg4_i1
    subroutine mg411_deg4_i2 ()
     pv(18) = pv(18)+d2(i0,i1)*d(i0,i2)*d(i0,j0)/m3
     pv(19) = pv(19)+d2(i0,i1)*d(i1,i2)*d(i0,j0)/m3
     pv(21) = pv(21)+d(i0,i1)*d(i0,i2)*d2(i0,j0)/m3
     pv(25) = pv(25)+d2(i0,i1)*d(i0,i2)*d(i0,k0)/m3
     pv(26) = pv(26)+d2(i0,i1)*d(i1,i2)*d(i0,k0)/m3
     pv(28) = pv(28)+d(i0,i1)*d(i0,i2)*d(i0,j0)*d(i0,k0)/m3
     pv(32) = pv(32)+d(i0,i1)*d(i0,i2)*d(i1,j0)*d(i0,k0)/m3
     pv(35) = pv(35)+d(i0,i1)*d(i0,i2)*d2(i0,k0)/m3
    end subroutine mg411_deg4_i2
    subroutine mg411_deg5_i1 ()
     pv(42) = pv(42)+d5(i0,i1)/m2
     pv(43) = pv(43)+d4(i0,i1)*d(i0,j0)/m2
     pv(45) = pv(45)+d3(i0,i1)*d2(i0,j0)/m2
     pv(47) = pv(47)+d2(i0,i1)*d3(i0,j0)/m2
     pv(48) = pv(48)+d(i0,i1)*d3(i0,j0)*d(i1,j0)/m2
     pv(49) = pv(49)+d4(i0,i1)*d(i0,k0)/m2
     pv(51) = pv(51)+d3(i0,i1)*d(i0,j0)*d(i0,k0)/m2
     pv(53) = pv(53)+d2(i0,i1)*d2(i0,j0)*d(i0,k0)/m2
     pv(55) = pv(55)+d2(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,k0)/m2
     pv(56) = pv(56)+d(i0,i1)*d2(i0,j0)*d(i1,j0)*d(i0,k0)/m2
     pv(57) = pv(57)+d3(i0,i1)*d2(i0,k0)/m2
     pv(59) = pv(59)+d2(i0,i1)*d(i0,j0)*d2(i0,k0)/m2
     pv(60) = pv(60)+d2(i0,i1)*d(i1,j0)*d2(i0,k0)/m2
     pv(61) = pv(61)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d2(i0,k0)/m2
     pv(62) = pv(62)+d2(i0,i1)*d3(i0,k0)/m2
     pv(63) = pv(63)+d(i0,i1)*d(i1,j0)*d3(i0,k0)/m2
     pv(64) = pv(64)+d(i0,i1)*d3(i0,k0)*d(i1,k0)/m2
    end subroutine mg411_deg5_i1
    subroutine mg411_deg5_i2 ()
     pv(44) = pv(44)+d3(i0,i1)*d(i0,i2)*d(i0,j0)/m3
     pv(46) = pv(46)+d2(i0,i1)*d(i0,i2)*d2(i0,j0)/m3
     pv(50) = pv(50)+d3(i0,i1)*d(i0,i2)*d(i0,k0)/m3
     pv(52) = pv(52)+d2(i0,i1)*d(i0,i2)*d(i0,j0)*d(i0,k0)/m3
     pv(54) = pv(54)+d2(i0,i1)*d(i0,i2)*d(i1,j0)*d(i0,k0)/m3
     pv(58) = pv(58)+d2(i0,i1)*d(i0,i2)*d2(i0,k0)/m3
    end subroutine mg411_deg5_i2
    subroutine mg411_deg6_i2 ()
     pv(65) = pv(65)+d2(i0,i1)*d2(i0,i2)*d(i1,j0)*d(i0,k0)/m3
    end subroutine mg411_deg6_i2
  end subroutine mg411_secs

  subroutine mg411_prims (x, u)
    real , intent (in) :: x(0:mg411_nr-1)
    real , intent (out) :: u(0:mg411_nr-1)
    !-----------------------------------------------------------------------
    integer, parameter :: m=4, m2=m*(m-1)/2
    real  :: x0(0:m2-1), x1(0:m-1), x2(0:m-1), x3(0:0), t0(0:m-1)
    x0 = x(0:m2-1)
    x1 = x(m2:m2+m-1)
    x2 = x(m2+m:m2+2*m-1)
    x3(0) = x(m2+2*m)
    t0(0) = (x0(0)+x0(1)+x0(3))/3
    t0(1) = (x0(0)+x0(2)+x0(4))/3
    t0(2) = (x0(1)+x0(2)+x0(5))/3
    t0(3) = (x0(3)+x0(4)+x0(5))/3
    u(0) = sum(x0)/size(x0)
    u(1) = sum(x1)/size(x1)
    u(2) = sum(x2)/size(x2)
    u(3) = sum(x3)/size(x3)
    u(4) = sum(t0**2)/size(t0)
    u(5) = sum(x0**2)/size(x0)
    u(6) = sum(x1**2)/size(x1)
    u(7) = sum(x2**2)/size(x2)
    u(8) = sum(t0**3)/size(t0)
    u(9) = sum(x0**3)/size(x0)
    u(10) = sum(x1**3)/size(x1)
    u(11) = sum(x2**3)/size(x2)
    u(12) = sum(t0**4)/size(t0)
    u(13) = sum(x1**4)/size(x1)
    u(14) = sum(x2**4)/size(x2)
    return
  end subroutine mg411_prims

  subroutine mg411_gens (ind, x, y)
    integer, intent (in) :: ind
    real , intent (in) :: x(0:mg411_nr-1)
    real , intent (out) :: y(0:mg411_nr-1)
    !-----------------------------------------------------------------------
    integer :: iord(0:mg411_nr-1)
    ! Numbering (block revlex):
    ! ( 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14)
    ! (01, 02, 12, 03, 13, 23, 04, 14, 24, 34, 05, 15, 25, 35, 45)
    select case (ind)
    case (0, -1)
    ! permutation (0,1)
     iord = (/ 0, 2, 1, 4, 3, 5, 7, 6, 8, 9, 11, 10, 12, 13, 14 /)
    case (1, -2)
    ! permutation (0,1,2,3)
     iord = (/ 2, 4, 5, 0, 1, 3, 7, 8, 9, 6, 11, 12, 13, 10, 14 /)
    case default
     stop 'mg411_gens: invalid index'
    end select
    if (0.le.ind) then
     y(iord(:)) = x(:)
    else
     y(:) = x(iord(:))
    endif
    return
  end subroutine mg411_gens

  subroutine mg411_base (mxd, x, w)
    integer, intent (in) :: mxd
    real , intent (in) :: x(0:mg411_nr-1)
    real , intent (out) :: w(0:mg411_ivb(mxd)-1)
    !-----------------------------------------------------------------------
    real  :: u(0:mg411_nr-1), v(0:mg411_ivs(mxd)-1)
    call mg411_prims (x, u)
    call mg411_secs (mxd, x, v)
    call inv_base (mxd, mg411_ivp, mg411_ivs, mg411_ivb, u, v, w)
  end subroutine mg411_base

  subroutine mg321_secs (mxd, x, v)
    integer, intent (in) :: mxd
    real , intent (in) :: x(0:mg321_nr-1)
    real , intent (out) :: v(0:mg321_ivs(mxd)-1)
    !! Note: We stop at degree 8 for now.
    !-----------------------------------------------------------------------
    integer, parameter :: m=3, n=2, nk=6, npv=43, &
      m2=m*(m-1), n2=n*(n-1), mn=m*n, m2n=m*(m-1)*n, mn2=m*n*(n-1), &
      m2n2=m2n*(n-1), m3n=m*(m-1)*(m-2)*n, mn3=m*n*(n-1)*(n-2)
    integer :: i0, i1, j0, j1, k0
    real  :: pv(0:npv-1), d(0:nk-1,0:nk-1), d2(0:nk-1,0:nk-1), &
      d3(0:nk-1,0:nk-1), d4(0:nk-1,0:nk-1), d5(0:nk-1,0:nk-1), &
      d6(0:nk-1,0:nk-1), d7(0:nk-1,0:nk-1)
    call mg321_setd ()
    pv = 0
    k0 = m+n
    do i0 = 0, m-1
     do i1 = 0, m-1
     if (i1.ne.i0) then
      if (2.le.mxd) then
       call mg321_deg2_i1k0 ()
      endif
      if (3.le.mxd) then
       call mg321_deg3_i1k0 ()
      endif
     endif
     enddo
    enddo
    do j0 = m, m+n-1
     do i0 = 0, m-1
      if (2.le.mxd) then
       call mg321_deg2_i0j0k0 ()
      endif
      if (3.le.mxd) then
       call mg321_deg3_i0j0 ()
       call mg321_deg3_i0j0k0 ()
      endif
      if (4.le.mxd) then
       call mg321_deg4_i0j0 ()
       call mg321_deg4_i0j0k0 ()
      endif
      if (5.le.mxd) then
       call mg321_deg5_i0j0 ()
       call mg321_deg5_i0j0k0 ()
      endif
      do i1 = 0, m-1
      if (i1.ne.i0) then
       if (2.le.mxd) then
        call mg321_deg2_i1j0 ()
       endif
       if (3.le.mxd) then
        call mg321_deg3_i1j0 ()
        call mg321_deg3_i1j0k0 ()
       endif
       if (4.le.mxd) then
        call mg321_deg4_i1j0 ()
        call mg321_deg4_i1j0k0 ()
       endif
       if (5.le.mxd) then
        call mg321_deg5_i1j0 ()
       endif
      endif
      enddo
     enddo
     do j1 = m, m+n-1
     if (j1.ne.j0) then
      do i0 = 0, m-1
       if (3.le.mxd) then
        call mg321_deg3_i0j1k0 ()
       endif
       if (4.le.mxd) then
        call mg321_deg4_i0j1 ()
        call mg321_deg4_i0j1k0 ()
       endif
       do i1 = 0, m-1
       if (i1.ne.i0) then
        if (3.le.mxd) then
         call mg321_deg3_i1j1 ()
        endif
       endif
       enddo
      enddo
     endif
     enddo
    enddo
    v(0) = 1
    if (2.le.mxd) then
     v(1:4) = pv(0:3)
    endif
    if (3.le.mxd) then
     v(5:20) = pv(4:19)
    endif
    if (4.le.mxd) then
     v(21) = pv(0)*pv(0)
     v(22) = pv(0)*pv(1)
     v(23) = pv(1)*pv(1)
     v(24) = pv(0)*pv(2)
     v(25) = pv(1)*pv(2)
     v(26) = pv(2)*pv(2)
     v(27) = pv(0)*pv(3)
     v(28) = pv(1)*pv(3)
     v(29) = pv(2)*pv(3)
     v(30:49) = pv(20:39)
    endif
    if (5.le.mxd) then
     v(50) = pv(3)*pv(4)
     v(51) = pv(1)*pv(5)
     v(52) = pv(0)*pv(6)
     v(53) = pv(1)*pv(6)
     v(54) = pv(2)*pv(6)
     v(55) = pv(0)*pv(7)
     v(56) = pv(1)*pv(7)
     v(57) = pv(2)*pv(7)
     v(58) = pv(3)*pv(7)
     v(59) = pv(0)*pv(8)
     v(60) = pv(1)*pv(8)
     v(61) = pv(2)*pv(8)
     v(62) = pv(3)*pv(8)
     v(63) = pv(0)*pv(9)
     v(64) = pv(1)*pv(9)
     v(65) = pv(2)*pv(9)
     v(66) = pv(3)*pv(9)
     v(67) = pv(0)*pv(10)
     v(68) = pv(3)*pv(10)
     v(69) = pv(3)*pv(11)
     v(70) = pv(0)*pv(12)
     v(71) = pv(0)*pv(13)
     v(72) = pv(1)*pv(13)
     v(73) = pv(2)*pv(13)
     v(74) = pv(3)*pv(13)
     v(75) = pv(0)*pv(14)
     v(76) = pv(1)*pv(14)
     v(77) = pv(2)*pv(14)
     v(78) = pv(3)*pv(14)
     v(79) = pv(0)*pv(15)
     v(80) = pv(3)*pv(15)
     v(81) = pv(0)*pv(16)
     v(82) = pv(1)*pv(16)
     v(83) = pv(3)*pv(16)
     v(84) = pv(0)*pv(17)
     v(85) = pv(1)*pv(17)
     v(86) = pv(2)*pv(17)
     v(87) = pv(0)*pv(18)
     v(88) = pv(1)*pv(18)
     v(89) = pv(2)*pv(18)
     v(90) = pv(0)*pv(19)
     v(91) = pv(1)*pv(19)
     v(92) = pv(2)*pv(19)
     v(93:95) = pv(40:42)
    endif
    if (6.le.mxd) then
     v(96) = pv(8)*pv(8)
     v(97) = pv(4)*pv(9)
     v(98) = pv(5)*pv(9)
     v(99) = pv(6)*pv(9)
     v(100) = pv(7)*pv(9)
     v(101) = pv(8)*pv(9)
     v(102) = pv(9)*pv(9)
     v(103) = pv(4)*pv(11)
     v(104) = pv(4)*pv(12)
     v(105) = pv(4)*pv(13)
     v(106) = pv(9)*pv(13)
     v(107) = pv(4)*pv(14)
     v(108) = pv(5)*pv(14)
     v(109) = pv(6)*pv(14)
     v(110) = pv(7)*pv(14)
     v(111) = pv(8)*pv(14)
     v(112) = pv(9)*pv(14)
     v(113) = pv(14)*pv(14)
     v(114) = pv(4)*pv(15)
     v(115) = pv(7)*pv(15)
     v(116) = pv(9)*pv(15)
     v(117) = pv(10)*pv(15)
     v(118) = pv(4)*pv(16)
     v(119) = pv(5)*pv(16)
     v(120) = pv(6)*pv(16)
     v(121) = pv(7)*pv(16)
     v(122) = pv(8)*pv(16)
     v(123) = pv(9)*pv(16)
     v(124) = pv(10)*pv(16)
     v(125) = pv(11)*pv(16)
     v(126) = pv(14)*pv(16)
     v(127) = pv(4)*pv(18)
     v(128) = pv(9)*pv(18)
     v(129) = pv(4)*pv(19)
     v(130) = pv(7)*pv(19)
     v(131) = pv(8)*pv(19)
     v(132) = pv(9)*pv(19)
     v(133) = pv(10)*pv(19)
     v(134) = pv(11)*pv(19)
     v(135) = pv(14)*pv(19)
     v(136) = pv(0)*pv(20)
     v(137) = pv(1)*pv(20)
     v(138) = pv(3)*pv(21)
     v(139) = pv(0)*pv(22)
     v(140) = pv(2)*pv(22)
     v(141) = pv(0)*pv(23)
     v(142) = pv(1)*pv(23)
     v(143) = pv(2)*pv(23)
     v(144) = pv(3)*pv(23)
     v(145) = pv(1)*pv(24)
     v(146) = pv(2)*pv(24)
     v(147) = pv(0)*pv(25)
     v(148) = pv(1)*pv(25)
     v(149) = pv(2)*pv(25)
     v(150) = pv(3)*pv(25)
     v(151) = pv(0)*pv(26)
     v(152) = pv(1)*pv(26)
     v(153) = pv(2)*pv(26)
     v(154) = pv(0)*pv(27)
     v(155) = pv(0)*pv(28)
     v(156) = pv(0)*pv(29)
     v(157) = pv(2)*pv(29)
     v(158) = pv(3)*pv(29)
     v(159) = pv(0)*pv(30)
     v(160) = pv(1)*pv(30)
     v(161) = pv(3)*pv(30)
     v(162) = pv(0)*pv(31)
     v(163) = pv(1)*pv(31)
     v(164) = pv(2)*pv(31)
     v(165) = pv(0)*pv(32)
     v(166) = pv(1)*pv(32)
     v(167) = pv(2)*pv(32)
     v(168) = pv(3)*pv(32)
     v(169) = pv(0)*pv(33)
     v(170) = pv(1)*pv(33)
     v(171) = pv(2)*pv(33)
     v(172) = pv(0)*pv(34)
     v(173) = pv(1)*pv(34)
     v(174) = pv(2)*pv(34)
     v(175) = pv(0)*pv(35)
     v(176) = pv(1)*pv(35)
     v(177) = pv(2)*pv(35)
     v(178) = pv(0)*pv(36)
     v(179) = pv(1)*pv(36)
     v(180) = pv(2)*pv(36)
     v(181) = pv(0)*pv(37)
     v(182) = pv(1)*pv(37)
     v(183) = pv(2)*pv(37)
     v(184) = pv(0)*pv(38)
     v(185) = pv(1)*pv(38)
     v(186) = pv(2)*pv(38)
     v(187) = pv(0)*pv(39)
     v(188) = pv(1)*pv(39)
     v(189) = pv(2)*pv(39)
    endif
    if (7.le.mxd) then
     v(190) = pv(0)*pv(0)*pv(15)
     v(191) = pv(0)*pv(0)*pv(16)
     v(192) = pv(0)*pv(1)*pv(16)
     v(193) = pv(16)*pv(20)
     v(194) = pv(5)*pv(23)
     v(195) = pv(9)*pv(23)
     v(196) = pv(13)*pv(23)
     v(197) = pv(14)*pv(23)
     v(198) = pv(16)*pv(23)
     v(199) = pv(4)*pv(24)
     v(200) = pv(7)*pv(24)
     v(201) = pv(9)*pv(24)
     v(202) = pv(4)*pv(25)
     v(203) = pv(8)*pv(25)
     v(204) = pv(9)*pv(25)
     v(205) = pv(4)*pv(26)
     v(206) = pv(7)*pv(26)
     v(207) = pv(8)*pv(26)
     v(208) = pv(9)*pv(26)
     v(209) = pv(10)*pv(26)
     v(210) = pv(14)*pv(26)
     v(211) = pv(15)*pv(26)
     v(212) = pv(4)*pv(27)
     v(213) = pv(9)*pv(27)
     v(214) = pv(10)*pv(27)
     v(215) = pv(8)*pv(28)
     v(216) = pv(4)*pv(29)
     v(217) = pv(5)*pv(29)
     v(218) = pv(6)*pv(29)
     v(219) = pv(7)*pv(29)
     v(220) = pv(8)*pv(29)
     v(221) = pv(9)*pv(29)
     v(222) = pv(10)*pv(29)
     v(223) = pv(11)*pv(29)
     v(224) = pv(14)*pv(29)
     v(225) = pv(4)*pv(30)
     v(226) = pv(6)*pv(30)
     v(227) = pv(7)*pv(30)
     v(228) = pv(9)*pv(30)
     v(229) = pv(10)*pv(30)
     v(230) = pv(11)*pv(30)
     v(231) = pv(14)*pv(30)
     v(232) = pv(4)*pv(31)
     v(233) = pv(7)*pv(31)
     v(234) = pv(8)*pv(31)
     v(235) = pv(9)*pv(31)
     v(236) = pv(10)*pv(31)
     v(237) = pv(11)*pv(31)
     v(238) = pv(14)*pv(31)
     v(239) = pv(4)*pv(32)
     v(240) = pv(5)*pv(32)
     v(241) = pv(6)*pv(32)
     v(242) = pv(7)*pv(32)
     v(243) = pv(8)*pv(32)
     v(244) = pv(9)*pv(32)
     v(245) = pv(10)*pv(32)
     v(246) = pv(11)*pv(32)
     v(247) = pv(12)*pv(32)
     v(248) = pv(14)*pv(32)
     v(249) = pv(8)*pv(33)
     v(250) = pv(9)*pv(33)
     v(251) = pv(13)*pv(33)
     v(252) = pv(14)*pv(33)
     v(253) = pv(16)*pv(33)
     v(254) = pv(4)*pv(34)
     v(255) = pv(9)*pv(34)
     v(256) = pv(10)*pv(34)
     v(257) = pv(11)*pv(34)
     v(258) = pv(14)*pv(34)
     v(259) = pv(4)*pv(35)
     v(260) = pv(8)*pv(35)
     v(261) = pv(9)*pv(35)
     v(262) = pv(10)*pv(35)
     v(263) = pv(11)*pv(35)
     v(264) = pv(15)*pv(35)
     v(265) = pv(4)*pv(36)
     v(266) = pv(5)*pv(36)
     v(267) = pv(7)*pv(36)
     v(268) = pv(8)*pv(36)
     v(269) = pv(9)*pv(36)
     v(270) = pv(10)*pv(36)
     v(271) = pv(11)*pv(36)
     v(272) = pv(12)*pv(36)
     v(273) = pv(13)*pv(36)
     v(274) = pv(14)*pv(36)
     v(275) = pv(15)*pv(36)
     v(276) = pv(16)*pv(36)
     v(277) = pv(4)*pv(37)
     v(278) = pv(8)*pv(37)
     v(279) = pv(9)*pv(37)
     v(280) = pv(10)*pv(37)
     v(281) = pv(11)*pv(37)
     v(282) = pv(14)*pv(37)
     v(283) = pv(4)*pv(38)
     v(284) = pv(5)*pv(38)
     v(285) = pv(7)*pv(38)
     v(286) = pv(8)*pv(38)
     v(287) = pv(9)*pv(38)
     v(288) = pv(10)*pv(38)
     v(289) = pv(11)*pv(38)
     v(290) = pv(14)*pv(38)
     v(291) = pv(15)*pv(38)
     v(292) = pv(4)*pv(39)
     v(293) = pv(5)*pv(39)
     v(294) = pv(6)*pv(39)
     v(295) = pv(7)*pv(39)
     v(296) = pv(8)*pv(39)
     v(297) = pv(9)*pv(39)
     v(298) = pv(10)*pv(39)
     v(299) = pv(11)*pv(39)
     v(300) = pv(13)*pv(39)
     v(301) = pv(14)*pv(39)
     v(302) = pv(0)*pv(40)
     v(303) = pv(1)*pv(40)
     v(304) = pv(2)*pv(40)
     v(305) = pv(3)*pv(40)
     v(306) = pv(0)*pv(41)
     v(307) = pv(1)*pv(41)
     v(308) = pv(2)*pv(41)
     v(309) = pv(3)*pv(41)
     v(310) = pv(0)*pv(42)
     v(311) = pv(1)*pv(42)
     v(312) = pv(2)*pv(42)
     v(313) = pv(3)*pv(42)
    endif
    if (8.le.mxd) then
     v(314) = pv(3)*pv(4)*pv(16)
     v(315) = pv(1)*pv(2)*pv(23)
     v(316) = pv(1)*pv(1)*pv(24)
     v(317) = pv(1)*pv(1)*pv(26)
     v(318) = pv(1)*pv(2)*pv(26)
     v(319) = pv(0)*pv(0)*pv(28)
     v(320) = pv(0)*pv(0)*pv(31)
     v(321) = pv(0)*pv(1)*pv(31)
     v(322) = pv(0)*pv(0)*pv(32)
     v(323) = pv(0)*pv(1)*pv(32)
     v(324) = pv(1)*pv(1)*pv(34)
     v(325) = pv(0)*pv(1)*pv(36)
     v(326) = pv(1)*pv(1)*pv(36)
     v(327) = pv(1)*pv(2)*pv(36)
     v(328) = pv(0)*pv(1)*pv(37)
     v(329) = pv(1)*pv(1)*pv(37)
     v(330) = pv(0)*pv(0)*pv(38)
     v(331) = pv(1)*pv(1)*pv(38)
     v(332) = pv(0)*pv(2)*pv(38)
     v(333) = pv(0)*pv(0)*pv(39)
     v(334) = pv(0)*pv(1)*pv(39)
     v(335) = pv(23)*pv(26)
     v(336) = pv(23)*pv(28)
     v(337) = pv(20)*pv(29)
     v(338) = pv(23)*pv(29)
     v(339) = pv(26)*pv(29)
     v(340) = pv(27)*pv(29)
     v(341) = pv(29)*pv(29)
     v(342) = pv(20)*pv(30)
     v(343) = pv(23)*pv(30)
     v(344) = pv(21)*pv(31)
     v(345) = pv(23)*pv(31)
     v(346) = pv(20)*pv(32)
     v(347) = pv(21)*pv(32)
     v(348) = pv(23)*pv(32)
     v(349) = pv(24)*pv(32)
     v(350) = pv(26)*pv(32)
     v(351) = pv(27)*pv(32)
     v(352) = pv(29)*pv(32)
     v(353) = pv(29)*pv(33)
     v(354) = pv(30)*pv(33)
     v(355) = pv(32)*pv(33)
     v(356) = pv(20)*pv(34)
     v(357) = pv(23)*pv(34)
     v(358) = pv(29)*pv(34)
     v(359) = pv(32)*pv(34)
     v(360) = pv(20)*pv(35)
     v(361) = pv(23)*pv(35)
     v(362) = pv(20)*pv(36)
     v(363) = pv(23)*pv(36)
     v(364) = pv(24)*pv(36)
     v(365) = pv(26)*pv(36)
     v(366) = pv(29)*pv(36)
     v(367) = pv(32)*pv(36)
     v(368) = pv(20)*pv(37)
     v(369) = pv(21)*pv(37)
     v(370) = pv(23)*pv(37)
     v(371) = pv(25)*pv(37)
     v(372) = pv(26)*pv(37)
     v(373) = pv(29)*pv(37)
     v(374) = pv(30)*pv(37)
     v(375) = pv(32)*pv(37)
     v(376) = pv(20)*pv(38)
     v(377) = pv(21)*pv(38)
     v(378) = pv(23)*pv(38)
     v(379) = pv(24)*pv(38)
     v(380) = pv(26)*pv(38)
     v(381) = pv(29)*pv(38)
     v(382) = pv(32)*pv(38)
     v(383) = pv(20)*pv(39)
     v(384) = pv(21)*pv(39)
     v(385) = pv(22)*pv(39)
     v(386) = pv(23)*pv(39)
     v(387) = pv(24)*pv(39)
     v(388) = pv(25)*pv(39)
     v(389) = pv(26)*pv(39)
     v(390) = pv(27)*pv(39)
     v(391) = pv(28)*pv(39)
     v(392) = pv(29)*pv(39)
     v(393) = pv(4)*pv(40)
     v(394) = pv(7)*pv(40)
     v(395) = pv(9)*pv(40)
     v(396) = pv(10)*pv(40)
     v(397) = pv(11)*pv(40)
     v(398) = pv(13)*pv(40)
     v(399) = pv(14)*pv(40)
     v(400) = pv(16)*pv(40)
     v(401) = pv(17)*pv(40)
     v(402) = pv(18)*pv(40)
     v(403) = pv(19)*pv(40)
     v(404) = pv(4)*pv(41)
     v(405) = pv(5)*pv(41)
     v(406) = pv(6)*pv(41)
     v(407) = pv(7)*pv(41)
     v(408) = pv(9)*pv(41)
     v(409) = pv(10)*pv(41)
     v(410) = pv(11)*pv(41)
     v(411) = pv(14)*pv(41)
     v(412) = pv(15)*pv(41)
     v(413) = pv(17)*pv(41)
     v(414) = pv(18)*pv(41)
     v(415) = pv(19)*pv(41)
     v(416) = pv(4)*pv(42)
     v(417) = pv(5)*pv(42)
     v(418) = pv(6)*pv(42)
     v(419) = pv(7)*pv(42)
     v(420) = pv(8)*pv(42)
     v(421) = pv(9)*pv(42)
     v(422) = pv(10)*pv(42)
     v(423) = pv(11)*pv(42)
     v(424) = pv(12)*pv(42)
     v(425) = pv(13)*pv(42)
     v(426) = pv(14)*pv(42)
     v(427) = pv(15)*pv(42)
     v(428) = pv(16)*pv(42)
     v(429) = pv(17)*pv(42)
     v(430) = pv(18)*pv(42)
     v(431) = pv(19)*pv(42)
    endif
    return
    contains
    subroutine mg321_setd ()
     integer :: i, j, k, l0, l1
     integer, parameter, dimension(0:3) :: lb=(/ 0, m, m+n, m+n+1 /)
     k = 0
     do l1 = 0, 2
      do l0 = 0, l1
       do j = lb(l1), lb(l1+1)-1
        do i = lb(l0), min(j-1,lb(l0+1)-1)
         d(i,j) = x(k)
         d(j,i) = d(i,j)
         d2(i,j) = x(k)**2
         d2(j,i) = d2(i,j)
         d3(i,j) = x(k)**3
         d3(j,i) = d3(i,j)
         d4(i,j) = x(k)**4
         d4(j,i) = d4(i,j)
         d5(i,j) = x(k)**5
         d5(j,i) = d5(i,j)
         d6(i,j) = x(k)**6
         d6(j,i) = d6(i,j)
         d7(i,j) = x(k)**7
         d7(j,i) = d7(i,j)
         k = k+1
        enddo
       enddo
      enddo
     enddo
     if (k.ne.mg321_nr) then
      stop 'mg321_setd: bad count'
     endif
     do i = 0, nk-1
      d(i,i) = 0
      d2(i,i) = 0
      d3(i,i) = 0
      d4(i,i) = 0
      d5(i,i) = 0
      d6(i,i) = 0
      d7(i,i) = 0
     enddo
    end subroutine mg321_setd
    subroutine mg321_deg2_i1j0 ()
     pv(0) = pv(0)+d(i0,i1)*d(i0,j0)/m2n
    end subroutine mg321_deg2_i1j0
    subroutine mg321_deg2_i1k0 ()
     pv(1) = pv(1)+d(i0,i1)*d(i0,k0)/m2
    end subroutine mg321_deg2_i1k0
    subroutine mg321_deg2_i0j0k0 ()
     pv(2) = pv(2)+d(i0,j0)*d(i0,k0)/mn
     pv(3) = pv(3)+d(i0,j0)*d(j0,k0)/mn
    end subroutine mg321_deg2_i0j0k0
    subroutine mg321_deg3_i0j0 ()
     pv(6) = pv(6)+d3(i0,j0)/mn
    end subroutine mg321_deg3_i0j0
    subroutine mg321_deg3_i1j0 ()
     pv(4) = pv(4)+d2(i0,i1)*d(i0,j0)/m2n
     pv(5) = pv(5)+d(i0,i1)*d2(i0,j0)/m2n
     pv(7) = pv(7)+d(i0,i1)*d(i0,j0)*d(i1,j0)/m2n
     pv(8) = pv(8)+d2(i0,j0)*d(i1,j0)/m2n
    end subroutine mg321_deg3_i1j0
    subroutine mg321_deg3_i1j1 ()
     pv(9) = pv(9)+d(i0,i1)*d(i0,j0)*d(i0,j1)/m2n2
    end subroutine mg321_deg3_i1j1
    subroutine mg321_deg3_i1k0 ()
     pv(10) = pv(10)+d2(i0,i1)*d(i0,k0)/m2
     pv(15) = pv(15)+d(i0,i1)*d2(i0,k0)/m2
    end subroutine mg321_deg3_i1k0
    subroutine mg321_deg3_i0j0k0 ()
     pv(12) = pv(12)+d2(i0,j0)*d(i0,k0)/mn
     pv(16) = pv(16)+d(i0,j0)*d2(i0,k0)/mn
     pv(18) = pv(18)+d2(i0,j0)*d(j0,k0)/mn
     pv(19) = pv(19)+d(i0,j0)*d(i0,k0)*d(j0,k0)/mn
    end subroutine mg321_deg3_i0j0k0
    subroutine mg321_deg3_i1j0k0 ()
     pv(11) = pv(11)+d(i0,i1)*d(i0,j0)*d(i0,k0)/m2n
     pv(13) = pv(13)+d(i0,j0)*d(i1,j0)*d(i0,k0)/m2n
     pv(17) = pv(17)+d(i0,i1)*d(i0,j0)*d(j0,k0)/m2n
    end subroutine mg321_deg3_i1j0k0
    subroutine mg321_deg3_i0j1k0 ()
     pv(14) = pv(14)+d(i0,j0)*d(i0,j1)*d(i0,k0)/mn2
    end subroutine mg321_deg3_i0j1k0
    subroutine mg321_deg4_i0j0 ()
     pv(22) = pv(22)+d4(i0,j0)/mn
    end subroutine mg321_deg4_i0j0
    subroutine mg321_deg4_i1j0 ()
     pv(20) = pv(20)+d2(i0,i1)*d2(i0,j0)/m2n
     pv(21) = pv(21)+d(i0,i1)*d3(i0,j0)/m2n
     pv(23) = pv(23)+d2(i0,i1)*d(i0,j0)*d(i1,j0)/m2n
     pv(24) = pv(24)+d(i0,i1)*d2(i0,j0)*d(i1,j0)/m2n
     pv(25) = pv(25)+d3(i0,j0)*d(i1,j0)/m2n
    end subroutine mg321_deg4_i1j0
    subroutine mg321_deg4_i0j1 ()
     pv(26) = pv(26)+d3(i0,j0)*d(i0,j1)/mn2
    end subroutine mg321_deg4_i0j1
    subroutine mg321_deg4_i0j0k0 ()
     pv(28) = pv(28)+d3(i0,j0)*d(i0,k0)/mn
     pv(31) = pv(31)+d2(i0,j0)*d2(i0,k0)/mn
     pv(35) = pv(35)+d3(i0,j0)*d(j0,k0)/mn
     pv(38) = pv(38)+d2(i0,j0)*d(i0,k0)*d(j0,k0)/mn
     pv(39) = pv(39)+d(i0,j0)*d2(i0,k0)*d(j0,k0)/mn
    end subroutine mg321_deg4_i0j0k0
    subroutine mg321_deg4_i1j0k0 ()
     pv(27) = pv(27)+d(i0,i1)*d2(i0,j0)*d(i0,k0)/m2n
     pv(29) = pv(29)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,k0)/m2n
     pv(30) = pv(30)+d2(i0,j0)*d(i1,j0)*d(i0,k0)/m2n
     pv(32) = pv(32)+d(i0,j0)*d(i1,j0)*d2(i0,k0)/m2n
     pv(33) = pv(33)+d2(i0,i1)*d(i0,j0)*d(j0,k0)/m2n
     pv(34) = pv(34)+d(i0,i1)*d2(i0,j0)*d(j0,k0)/m2n
     pv(37) = pv(37)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d(j0,k0)/m2n
    end subroutine mg321_deg4_i1j0k0
    subroutine mg321_deg4_i0j1k0 ()
     pv(36) = pv(36)+d2(i0,j0)*d(i0,j1)*d(j0,k0)/mn2
    end subroutine mg321_deg4_i0j1k0
    subroutine mg321_deg5_i0j0 ()
     pv(41) = pv(41)+d5(i0,j0)/mn
    end subroutine mg321_deg5_i0j0
    subroutine mg321_deg5_i1j0 ()
     pv(40) = pv(40)+d(i0,i1)*d4(i0,j0)/m2n
    end subroutine mg321_deg5_i1j0
    subroutine mg321_deg5_i0j0k0 ()
     pv(42) = pv(42)+d4(i0,j0)*d(i0,k0)/mn
    end subroutine mg321_deg5_i0j0k0
  end subroutine mg321_secs

  subroutine mg321_prims (x, u)
    real , intent (in) :: x(0:mg321_nr-1)
    real , intent (out) :: u(0:mg321_nr-1)
    !-----------------------------------------------------------------------
    integer, parameter :: m=3, n=2, m2=m*(m-1)/2, n2=n*(n-1)/2
    integer :: i, j
    real  :: x0(0:m2-1), x1(0:m*n-1), x2(0:n2-1), x3(0:m-1), &
      x4(0:n-1), t0(0:m-1), e(0:m-1), f(0:n-1)
    x0 = x(0:m2-1)
    x1 = x(m2:m2+m*n-1)
    x2 = x(m2+m*n:m2+m*n+n2-1)
    x3 = x(m2+m*n+n2:m2+m*n+n2+m-1)
    x4 = x(m2+m*n+n2+m:m2+m*n+n2+m+n-1)
    t0(0) = (x0(0)+x0(1))/2
    t0(1) = (x0(0)+x0(2))/2
    t0(2) = (x0(1)+x0(2))/2
    do i = 0, m-1
     e(i) = sum(x1(i:i+m*(n-1):m))/n
    enddo
    do j = 0, n-1
     f(j) = sum(x1(m*j:m*(j+1)-1))/m
    enddo
    u(0) = sum(x0)/size(x0)
    u(1) = sum(x1)/size(x1)
    u(2) = sum(x2)/size(x2)
    u(3) = sum(x3)/size(x3)
    u(4) = sum(x4)/size(x4)
    u(5) = sum(t0**2)/size(t0)
    u(6) = sum(e**2)/size(e)
    u(7) = sum(f**2)/size(f)
    u(8) = sum(x1**2)/size(x1)
    u(9) = sum(x3**2)/size(x3)
    u(10) = sum(x4**2)/size(x4)
    u(11) = sum(t0**3)/size(t0)
    u(12) = sum(e**3)/size(e)
    u(13) = sum(x3**3)/size(x3)
    u(14) = sum(x1**6)/size(x1)
    return
  end subroutine mg321_prims

  subroutine mg321_gens (ind, x, y)
    integer, intent (in) :: ind
    real , intent (in) :: x(0:mg321_nr-1)
    real , intent (out) :: y(0:mg321_nr-1)
    !-----------------------------------------------------------------------
    integer :: iord(0:mg321_nr-1)
    ! Numbering (block revlex):
    ! ( 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14)
    ! (01, 02, 12, 03, 13, 23, 04, 14, 24, 34, 05, 15, 25, 35, 45)
    select case (ind)
    case (0, -1)
    ! permutation (0,1)
     iord = (/ 0, 2, 1, 4, 3, 5, 7, 6, 8, 9, 11, 10, 12, 13, 14 /)
    case (1, -2)
    ! permutation (0,1,2)
     iord = (/ 2, 0, 1, 4, 5, 3, 7, 8, 6, 9, 11, 12, 10, 13, 14 /)
    case (2, -3)
    ! permutation (3,4)
     iord = (/ 0, 1, 2, 6, 7, 8, 3, 4, 5, 9, 10, 11, 12, 14, 13 /)
    case default
     stop 'mg321_gens: invalid index'
    end select
    if (0.le.ind) then
     y(iord(:)) = x(:)
    else
     y(:) = x(iord(:))
    endif
    return
  end subroutine mg321_gens

  subroutine mg321_base (mxd, x, w)
    integer, intent (in) :: mxd
    real , intent (in) :: x(0:mg321_nr-1)
    real , intent (out) :: w(0:mg321_ivb(mxd)-1)
    !-----------------------------------------------------------------------
    real  :: u(0:mg321_nr-1), v(0:mg321_ivs(mxd)-1)
    call mg321_prims (x, u)
    call mg321_secs (mxd, x, v)
    call inv_base (mxd, mg321_ivp, mg321_ivs, mg321_ivb, u, v, w)
  end subroutine mg321_base

  subroutine inv_base (mxd, ivp, ivs, ivb, u, v, w)
    integer, intent (in) :: mxd, ivp(0:), ivs(0:), ivb(0:)
    real , intent (in) :: u(0:ivp(mxd)-1), v(0:ivs(mxd)-1)
    real , intent (out) :: w(0:ivb(mxd)-1)
    !-----------------------------------------------------------------------
    integer :: l(0:mxd,0:ivp(mxd)), ind, i, k, d, inc
    l(0,0:ivp(mxd)) = 0
    w(0) = v(0)
    ind = 1
    do d = 1, mxd
     do k = 1, d
      do i = ivp(k-1), ivp(k)-1
       l(d,i) = ind
       inc = l(d+1-k,0)-l(d-k,i)
       w(ind:ind+inc-1) = u(i)*w(l(d-k,i):l(d-k,i)+inc-1)
       ind = ind+inc
      enddo
     enddo
     l(d,ivp(d):ivp(mxd)) = ind
     w(ind:ind+ivs(d)-ivs(d-1)-1) = v(ivs(d-1):ivs(d)-1)
     ind = ind+ivs(d)-ivs(d-1)
    enddo
  end subroutine inv_base

  subroutine getr0 (nk, xn, r0)
    implicit none
    integer nk
    real xn(0:2,0:nk-1), r0(0:nk-1,0:nk-1)
    integer i, j
    do i = 0, nk-1
     r0(i,i) = 0
     do j = i+1, nk-1
      r0(i,j) = sqrt((xn(0,j)-xn(0,i))**2+(xn(1,j)-xn(1,i))**2+ &
        (xn(2,j)-xn(2,i))**2)
      r0(j,i) = r0(i,j)
     enddo
    enddo
    return
  end subroutine getr0

  subroutine getdvec (ma, mb, xn, vec, chg)
    ! version for H4O2
    integer, parameter :: nk=6, nk2=nk*(nk-1)/2
    integer :: ma, mb
    real  :: xn(0:2,0:nk-1), vec(0:3,0:ma+mb-1)
    integer :: i, j, k, dma, dmb, ind(0:nk-1)
    real  :: r0(0:nk-1,0:nk-1), d0(0:nk-1,0:nk-1), &
      d1(0:nk-1,0:nk-1), x0(0:nk2-1), v0(0:ma+mb-1), t0
    !add by yimin to get charges
    real::chg(0:5,0:ma+mb-1)
    chg=0.d0
    !add by yimin to get charges
    !-----------------------------------------------------------------------
    dma = -1 ; dmb = -1
    do k = 9, 0, -1
     if (ma.eq.mg321_ivb(k)) then
      dma = k
     endif
     if (mb.eq.mg411_ivb(k)) then
      dmb = k
     endif
    enddo
    if (dma.eq.-1.or.dmb.eq.-1) then
     stop 'getvec: bad ma, mb'
    endif
    vec = 0
    call getr0 (nk, xn, r0)
    call getd0 (nk, r0, d0)
    ! vector factor xn(0:2,0) (species X)
    ind = (/ 1, 2, 3, 4, 5, 0 /)
    call sortd0 (nk, d0, ind, d1)
    call mgx_mk1d (nk, nk2, (/3,2,1/), d1, x0)
    call mg321_base (dma, x0, v0)
    do k = 0, ma-1
     vec(0:2,k) = vec(0:2,k)+xn(0:2,0)*v0(k)
     !add by yimin to get charges
     chg(0,k)=v0(k)
     !add by yimin to get charges
     vec(3,k) = vec(3,k)+v0(k)
    enddo
    ! vector factor xn(0:2,1) (species X)
    ind = (/ 0, 2, 3, 4, 5, 1 /)
    call sortd0 (nk, d0, ind, d1)
    call mgx_mk1d (nk, nk2, (/3,2,1/), d1, x0)
    call mg321_base (dma, x0, v0)
    do k = 0, ma-1
     vec(0:2,k) = vec(0:2,k)+xn(0:2,1)*v0(k)
     !add by yimin to get charges
     chg(1,k)=v0(k)
     !add by yimin to get charges
     vec(3,k) = vec(3,k)+v0(k)
    enddo
    ! vector factor xn(0:2,2) (species X)
    ind = (/ 0, 1, 3, 4, 5, 2 /)
    call sortd0 (nk, d0, ind, d1)
    call mgx_mk1d (nk, nk2, (/3,2,1/), d1, x0)
    call mg321_base (dma, x0, v0)
    do k = 0, ma-1
     vec(0:2,k) = vec(0:2,k)+xn(0:2,2)*v0(k)
     !add by yimin to get charges
     chg(2,k)=v0(k)
     !add by yimin to get charges
     vec(3,k) = vec(3,k)+v0(k)
    enddo
    ! vector factor xn(0:2,3) (species X)
    ind = (/ 0, 1, 2, 4, 5, 3 /)
    call sortd0 (nk, d0, ind, d1)
    call mgx_mk1d (nk, nk2, (/3,2,1/), d1, x0)
    call mg321_base (dma, x0, v0)
    do k = 0, ma-1
     vec(0:2,k) = vec(0:2,k)+xn(0:2,3)*v0(k)
     !add by yimin to get charges
     chg(3,k)=v0(k)
     !add by yimin to get charges
     vec(3,k) = vec(3,k)+v0(k)
    enddo
    ! vector factor xn(0:2,4) (species Y)
    ind = (/ 0, 1, 2, 3, 5, 4 /)
    call sortd0 (nk, d0, ind, d1)
    call mgx_mk1d (nk, nk2, (/4,1,1/), d1, x0)
    call mg411_base (dmb, x0, v0)
    do k = 0, mb-1
     vec(0:2,ma+k) = vec(0:2,ma+k)+xn(0:2,4)*v0(k)
     !add by yimin to get charges
     chg(4,ma+k)=v0(k)
     !add by yimin to get charges
     vec(3,ma+k) = vec(3,ma+k)+v0(k)
    enddo
    ! vector factor xn(0:2,5) (species Y)
    ind = (/ 0, 1, 2, 3, 4, 5 /)
    call sortd0 (nk, d0, ind, d1)
    call mgx_mk1d (nk, nk2, (/4,1,1/), d1, x0)
    call mg411_base (dmb, x0, v0)
    do k = 0, mb-1
     vec(0:2,ma+k) = vec(0:2,ma+k)+xn(0:2,5)*v0(k)
     !add by yimin to get charges
     chg(5,ma+k)=v0(k)
     !add by yimin to get charges
     vec(3,ma+k) = vec(3,ma+k)+v0(k)
    enddo
    return
    contains
    subroutine sortd0 (nk, d0, ind, d1)
    integer, intent (in) :: nk
    real , intent (in) :: d0(0:nk-1,0:nk-1)
    integer, intent (in) :: ind(0:nk-1)
    real , intent (out) :: d1(0:nk-1,0:nk-1)
    do j = 0, nk-1
     do i = 0, nk-1
      d1(i,j) = d0(ind(i),ind(j))
     enddo
    enddo
    end subroutine sortd0
  end subroutine getdvec

  subroutine getd0 (nk, r0, d0)
    implicit none
    integer :: nk
    real :: r0(0:nk-1,0:nk-1), d0(0:nk-1,0:nk-1)
    integer :: i, j
    do i = 0, nk-1
     d0(i,i) = 0
     do j = i+1, nk-1
      d0(i,j) = 3.d0*exp(-r0(i,j)/3)
      d0(j,i) = d0(i,j)
     enddo
    enddo
    return
  end subroutine getd0

  subroutine predip(dname)

    implicit double precision (a-h,o-z)
    implicit integer (i-n)

    character(len=*),intent(in)::dname
    double precision V, cart_in(6,3), v0
    double precision dc0(0:5,0:5),dw0(0:5,0:5), coef(0:2879)

    integer i,j,k,i1,j1,k1

    common/NCOEdip/ms,mr
    common/h4o2coefdip/dc0,dw0,coef

    ms=1827 ; mr=1053

    open(20,file=dname,status='old')
    read(20,*)
    read(20,*)
    read(20,*)(coef(i1),i1=0,ms+mr-1)
    close(20)

    write(*,*) "Initializing H2O-H2O dipole!"
    return
  end subroutine predip

  subroutine calcdip(dip,chg,cart_in)

    integer ms,mr
    double precision dc0(0:5,0:5),dw0(0:5,0:5),coef(0:2879)

    common/NCOEdip/ms,mr
    common/h4o2coefdip/dc0,dw0,coef
    double precision V, cart_in(6,3), dip(1:3),chg(1:6),chg_ord(0:5)

    double precision cart0(6,3),cart1(6,3)
    integer i,j,k,l,i1,j1,k1,l1,i2,j2,k2,l2

    double precision rvec(0:3),d0(0:5,0:5),r0(0:5,0:5)
    double precision xnuc(0:2,0:5),vec(0:3,0:2879),chgb(0:5,0:2879)

     do j=1,3
      xnuc(j-1,0:1)=cart_in(2:3,j)
      xnuc(j-1,2:3)=cart_in(5:6,j)
      xnuc(j-1,4)=cart_in(1,j)
      xnuc(j-1,5)=cart_in(4,j)
    end do

    call getdvec (ms, mr, xnuc(0:2,0:5), vec,chgb)

    chg_ord=0.d0
    do i=0,5
      chg_ord(i)=dot_product(chgb(i,:),coef)
    end do

    chg(2:3)=chg_ord(0:1)
    chg(5:6)=chg_ord(2:3)
    chg(1)=chg_ord(4)
    chg(4)=chg_ord(5)

    return
  end subroutine calcdip

end module dms_2b