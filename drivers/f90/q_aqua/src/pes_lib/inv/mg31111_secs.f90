SUBROUTINE mg31111_secs (mxd, r, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: v(0:)
!! Note: We stop at degree 6 for now.  We do not have the secondaries
!! at degree 7.
!-----------------------------------------------------------------------
real (kind=wp) :: pv(0:size(v)-1) !! an upper bound size
if (size(r).ne.nk*nk.or.size(v).ne.mg31111_nsc(mxd)) then
 stop 'mg31111_secs: bad dimensions'
endif
call mg31111_isecs (mxd, r, pv)
v(0) = 1
if (2.le.mxd) then
 v(1:10) = pv(0:9)
endif
if (3.le.mxd) then
 v(11:40) = pv(10:39)
endif
if (4.le.mxd) then
 v(41) = pv(0)*pv(0)
 v(42) = pv(0)*pv(1)
 v(43) = pv(1)*pv(1)
 v(44) = pv(0)*pv(2)
 v(45) = pv(1)*pv(2)
 v(46) = pv(2)*pv(2)
 v(47) = pv(0)*pv(3)
 v(48) = pv(1)*pv(3)
 v(49) = pv(2)*pv(3)
 v(50) = pv(3)*pv(3)
 v(51) = pv(0)*pv(4)
 v(52) = pv(1)*pv(4)
 v(53) = pv(2)*pv(4)
 v(54) = pv(3)*pv(4)
 v(55) = pv(4)*pv(4)
 v(56) = pv(0)*pv(5)
 v(57) = pv(1)*pv(5)
 v(58) = pv(2)*pv(5)
 v(59) = pv(3)*pv(5)
 v(60) = pv(4)*pv(5)
 v(61) = pv(5)*pv(5)
 v(62) = pv(0)*pv(6)
 v(63) = pv(1)*pv(6)
 v(64) = pv(2)*pv(6)
 v(65) = pv(3)*pv(6)
 v(66) = pv(4)*pv(6)
 v(67) = pv(5)*pv(6)
 v(68) = pv(6)*pv(6)
 v(69) = pv(0)*pv(7)
 v(70) = pv(1)*pv(7)
 v(71) = pv(2)*pv(7)
 v(72) = pv(3)*pv(7)
 v(73) = pv(4)*pv(7)
 v(74) = pv(5)*pv(7)
 v(75) = pv(6)*pv(7)
 v(76) = pv(7)*pv(7)
 v(77) = pv(0)*pv(8)
 v(78) = pv(1)*pv(8)
 v(79) = pv(2)*pv(8)
 v(80) = pv(3)*pv(8)
 v(81) = pv(4)*pv(8)
 v(82) = pv(5)*pv(8)
 v(83) = pv(6)*pv(8)
 v(84) = pv(7)*pv(8)
 v(85) = pv(8)*pv(8)
 v(86) = pv(0)*pv(9)
 v(87) = pv(1)*pv(9)
 v(88) = pv(2)*pv(9)
 v(89) = pv(3)*pv(9)
 v(90) = pv(4)*pv(9)
 v(91) = pv(5)*pv(9)
 v(92) = pv(6)*pv(9)
 v(93) = pv(7)*pv(9)
 v(94) = pv(8)*pv(9)
 v(95) = pv(9)*pv(9)
endif
if (5.le.mxd) then
 v(96) = pv(1)*pv(11)
 v(97) = pv(3)*pv(11)
 v(98) = pv(6)*pv(11)
 v(99) = pv(0)*pv(12)
 v(100) = pv(3)*pv(13)
 v(101) = pv(6)*pv(13)
 v(102) = pv(0)*pv(14)
 v(103) = pv(3)*pv(14)
 v(104) = pv(6)*pv(14)
 v(105) = pv(0)*pv(15)
 v(106) = pv(3)*pv(15)
 v(107) = pv(6)*pv(15)
 v(108) = pv(0)*pv(16)
 v(109) = pv(1)*pv(16)
 v(110) = pv(3)*pv(16)
 v(111) = pv(4)*pv(16)
 v(112) = pv(6)*pv(16)
 v(113) = pv(7)*pv(16)
 v(114) = pv(0)*pv(17)
 v(115) = pv(1)*pv(17)
 v(116) = pv(1)*pv(18)
 v(117) = pv(6)*pv(18)
 v(118) = pv(0)*pv(19)
 v(119) = pv(1)*pv(19)
 v(120) = pv(2)*pv(19)
 v(121) = pv(6)*pv(19)
 v(122) = pv(0)*pv(20)
 v(123) = pv(6)*pv(20)
 v(124) = pv(0)*pv(21)
 v(125) = pv(1)*pv(21)
 v(126) = pv(6)*pv(21)
 v(127) = pv(7)*pv(21)
 v(128) = pv(0)*pv(22)
 v(129) = pv(1)*pv(22)
 v(130) = pv(2)*pv(22)
 v(131) = pv(6)*pv(22)
 v(132) = pv(7)*pv(22)
 v(133) = pv(0)*pv(23)
 v(134) = pv(1)*pv(23)
 v(135) = pv(2)*pv(23)
 v(136) = pv(6)*pv(23)
 v(137) = pv(0)*pv(24)
 v(138) = pv(1)*pv(24)
 v(139) = pv(2)*pv(24)
 v(140) = pv(3)*pv(24)
 v(141) = pv(6)*pv(24)
 v(142) = pv(7)*pv(24)
 v(143) = pv(0)*pv(25)
 v(144) = pv(1)*pv(25)
 v(145) = pv(2)*pv(25)
 v(146) = pv(3)*pv(25)
 v(147) = pv(4)*pv(25)
 v(148) = pv(6)*pv(25)
 v(149) = pv(7)*pv(25)
 v(150) = pv(8)*pv(25)
 v(151) = pv(0)*pv(26)
 v(152) = pv(1)*pv(26)
 v(153) = pv(3)*pv(26)
 v(154) = pv(1)*pv(27)
 v(155) = pv(3)*pv(27)
 v(156) = pv(0)*pv(28)
 v(157) = pv(1)*pv(28)
 v(158) = pv(2)*pv(28)
 v(159) = pv(3)*pv(28)
 v(160) = pv(4)*pv(28)
 v(161) = pv(0)*pv(29)
 v(162) = pv(3)*pv(29)
 v(163) = pv(0)*pv(30)
 v(164) = pv(1)*pv(30)
 v(165) = pv(3)*pv(30)
 v(166) = pv(4)*pv(30)
 v(167) = pv(0)*pv(31)
 v(168) = pv(1)*pv(31)
 v(169) = pv(2)*pv(31)
 v(170) = pv(3)*pv(31)
 v(171) = pv(4)*pv(31)
 v(172) = pv(5)*pv(31)
 v(173) = pv(0)*pv(32)
 v(174) = pv(1)*pv(32)
 v(175) = pv(2)*pv(32)
 v(176) = pv(0)*pv(33)
 v(177) = pv(1)*pv(33)
 v(178) = pv(2)*pv(33)
 v(179) = pv(3)*pv(33)
 v(180) = pv(0)*pv(34)
 v(181) = pv(1)*pv(34)
 v(182) = pv(2)*pv(34)
 v(183) = pv(3)*pv(34)
 v(184) = pv(4)*pv(34)
 v(185) = pv(0)*pv(35)
 v(186) = pv(1)*pv(35)
 v(187) = pv(2)*pv(35)
 v(188) = pv(3)*pv(35)
 v(189) = pv(4)*pv(35)
 v(190) = pv(5)*pv(35)
 v(191) = pv(0)*pv(36)
 v(192) = pv(1)*pv(36)
 v(193) = pv(2)*pv(36)
 v(194) = pv(3)*pv(36)
 v(195) = pv(4)*pv(36)
 v(196) = pv(5)*pv(36)
 v(197) = pv(0)*pv(37)
 v(198) = pv(1)*pv(37)
 v(199) = pv(2)*pv(37)
 v(200) = pv(3)*pv(37)
 v(201) = pv(4)*pv(37)
 v(202) = pv(5)*pv(37)
 v(203) = pv(6)*pv(37)
 v(204) = pv(0)*pv(38)
 v(205) = pv(1)*pv(38)
 v(206) = pv(2)*pv(38)
 v(207) = pv(3)*pv(38)
 v(208) = pv(4)*pv(38)
 v(209) = pv(5)*pv(38)
 v(210) = pv(6)*pv(38)
 v(211) = pv(7)*pv(38)
 v(212) = pv(0)*pv(39)
 v(213) = pv(1)*pv(39)
 v(214) = pv(2)*pv(39)
 v(215) = pv(3)*pv(39)
 v(216) = pv(4)*pv(39)
 v(217) = pv(5)*pv(39)
 v(218) = pv(6)*pv(39)
 v(219) = pv(7)*pv(39)
 v(220) = pv(8)*pv(39)
endif
if (6.le.mxd) then
 v(221) = pv(10)*pv(11)
 v(222) = pv(10)*pv(13)
 v(223) = pv(10)*pv(14)
 v(224) = pv(10)*pv(15)
 v(225) = pv(12)*pv(15)
 v(226) = pv(10)*pv(16)
 v(227) = pv(11)*pv(16)
 v(228) = pv(12)*pv(16)
 v(229) = pv(13)*pv(16)
 v(230) = pv(14)*pv(16)
 v(231) = pv(14)*pv(17)
 v(232) = pv(16)*pv(17)
 v(233) = pv(10)*pv(18)
 v(234) = pv(10)*pv(19)
 v(235) = pv(12)*pv(19)
 v(236) = pv(15)*pv(19)
 v(237) = pv(10)*pv(20)
 v(238) = pv(12)*pv(20)
 v(239) = pv(10)*pv(21)
 v(240) = pv(11)*pv(21)
 v(241) = pv(12)*pv(21)
 v(242) = pv(13)*pv(21)
 v(243) = pv(14)*pv(21)
 v(244) = pv(10)*pv(22)
 v(245) = pv(11)*pv(22)
 v(246) = pv(12)*pv(22)
 v(247) = pv(13)*pv(22)
 v(248) = pv(14)*pv(22)
 v(249) = pv(10)*pv(23)
 v(250) = pv(12)*pv(23)
 v(251) = pv(13)*pv(23)
 v(252) = pv(14)*pv(23)
 v(253) = pv(16)*pv(23)
 v(254) = pv(17)*pv(23)
 v(255) = pv(10)*pv(24)
 v(256) = pv(11)*pv(24)
 v(257) = pv(12)*pv(24)
 v(258) = pv(13)*pv(24)
 v(259) = pv(14)*pv(24)
 v(260) = pv(15)*pv(24)
 v(261) = pv(17)*pv(24)
 v(262) = pv(18)*pv(24)
 v(263) = pv(19)*pv(24)
 v(264) = pv(10)*pv(25)
 v(265) = pv(11)*pv(25)
 v(266) = pv(12)*pv(25)
 v(267) = pv(13)*pv(25)
 v(268) = pv(14)*pv(25)
 v(269) = pv(15)*pv(25)
 v(270) = pv(16)*pv(25)
 v(271) = pv(17)*pv(25)
 v(272) = pv(18)*pv(25)
 v(273) = pv(19)*pv(25)
 v(274) = pv(20)*pv(25)
 v(275) = pv(21)*pv(25)
 v(276) = pv(22)*pv(25)
 v(277) = pv(14)*pv(26)
 v(278) = pv(16)*pv(26)
 v(279) = pv(19)*pv(26)
 v(280) = pv(21)*pv(26)
 v(281) = pv(22)*pv(26)
 v(282) = pv(24)*pv(26)
 v(283) = pv(25)*pv(26)
 v(284) = pv(10)*pv(27)
 v(285) = pv(22)*pv(27)
 v(286) = pv(25)*pv(27)
 v(287) = pv(10)*pv(28)
 v(288) = pv(12)*pv(28)
 v(289) = pv(15)*pv(28)
 v(290) = pv(17)*pv(28)
 v(291) = pv(20)*pv(28)
 v(292) = pv(22)*pv(28)
 v(293) = pv(23)*pv(28)
 v(294) = pv(25)*pv(28)
 v(295) = pv(10)*pv(29)
 v(296) = pv(12)*pv(29)
 v(297) = pv(19)*pv(29)
 v(298) = pv(24)*pv(29)
 v(299) = pv(10)*pv(30)
 v(300) = pv(11)*pv(30)
 v(301) = pv(12)*pv(30)
 v(302) = pv(13)*pv(30)
 v(303) = pv(14)*pv(30)
 v(304) = pv(17)*pv(30)
 v(305) = pv(23)*pv(30)
 v(306) = pv(10)*pv(31)
 v(307) = pv(11)*pv(31)
 v(308) = pv(12)*pv(31)
 v(309) = pv(13)*pv(31)
 v(310) = pv(14)*pv(31)
 v(311) = pv(17)*pv(31)
 v(312) = pv(18)*pv(31)
 v(313) = pv(19)*pv(31)
 v(314) = pv(23)*pv(31)
 v(315) = pv(24)*pv(31)
 v(316) = pv(10)*pv(32)
 v(317) = pv(12)*pv(32)
 v(318) = pv(13)*pv(32)
 v(319) = pv(14)*pv(32)
 v(320) = pv(16)*pv(32)
 v(321) = pv(17)*pv(32)
 v(322) = pv(10)*pv(33)
 v(323) = pv(11)*pv(33)
 v(324) = pv(12)*pv(33)
 v(325) = pv(13)*pv(33)
 v(326) = pv(14)*pv(33)
 v(327) = pv(15)*pv(33)
 v(328) = pv(17)*pv(33)
 v(329) = pv(18)*pv(33)
 v(330) = pv(19)*pv(33)
 v(331) = pv(10)*pv(34)
 v(332) = pv(11)*pv(34)
 v(333) = pv(12)*pv(34)
 v(334) = pv(13)*pv(34)
 v(335) = pv(14)*pv(34)
 v(336) = pv(15)*pv(34)
 v(337) = pv(16)*pv(34)
 v(338) = pv(17)*pv(34)
 v(339) = pv(18)*pv(34)
 v(340) = pv(19)*pv(34)
 v(341) = pv(20)*pv(34)
 v(342) = pv(21)*pv(34)
 v(343) = pv(22)*pv(34)
 v(344) = pv(27)*pv(34)
 v(345) = pv(10)*pv(35)
 v(346) = pv(11)*pv(35)
 v(347) = pv(12)*pv(35)
 v(348) = pv(13)*pv(35)
 v(349) = pv(14)*pv(35)
 v(350) = pv(15)*pv(35)
 v(351) = pv(16)*pv(35)
 v(352) = pv(17)*pv(35)
 v(353) = pv(18)*pv(35)
 v(354) = pv(19)*pv(35)
 v(355) = pv(20)*pv(35)
 v(356) = pv(21)*pv(35)
 v(357) = pv(22)*pv(35)
 v(358) = pv(10)*pv(36)
 v(359) = pv(12)*pv(36)
 v(360) = pv(13)*pv(36)
 v(361) = pv(14)*pv(36)
 v(362) = pv(16)*pv(36)
 v(363) = pv(17)*pv(36)
 v(364) = pv(18)*pv(36)
 v(365) = pv(19)*pv(36)
 v(366) = pv(20)*pv(36)
 v(367) = pv(21)*pv(36)
 v(368) = pv(22)*pv(36)
 v(369) = pv(24)*pv(36)
 v(370) = pv(25)*pv(36)
 v(371) = pv(26)*pv(36)
 v(372) = pv(10)*pv(37)
 v(373) = pv(11)*pv(37)
 v(374) = pv(12)*pv(37)
 v(375) = pv(13)*pv(37)
 v(376) = pv(14)*pv(37)
 v(377) = pv(15)*pv(37)
 v(378) = pv(17)*pv(37)
 v(379) = pv(18)*pv(37)
 v(380) = pv(19)*pv(37)
 v(381) = pv(20)*pv(37)
 v(382) = pv(21)*pv(37)
 v(383) = pv(22)*pv(37)
 v(384) = pv(23)*pv(37)
 v(385) = pv(25)*pv(37)
 v(386) = pv(26)*pv(37)
 v(387) = pv(27)*pv(37)
 v(388) = pv(28)*pv(37)
 v(389) = pv(10)*pv(38)
 v(390) = pv(11)*pv(38)
 v(391) = pv(12)*pv(38)
 v(392) = pv(13)*pv(38)
 v(393) = pv(14)*pv(38)
 v(394) = pv(15)*pv(38)
 v(395) = pv(16)*pv(38)
 v(396) = pv(17)*pv(38)
 v(397) = pv(18)*pv(38)
 v(398) = pv(19)*pv(38)
 v(399) = pv(20)*pv(38)
 v(400) = pv(21)*pv(38)
 v(401) = pv(22)*pv(38)
 v(402) = pv(23)*pv(38)
 v(403) = pv(24)*pv(38)
 v(404) = pv(26)*pv(38)
 v(405) = pv(27)*pv(38)
 v(406) = pv(28)*pv(38)
 v(407) = pv(29)*pv(38)
 v(408) = pv(30)*pv(38)
 v(409) = pv(31)*pv(38)
 v(410) = pv(10)*pv(39)
 v(411) = pv(11)*pv(39)
 v(412) = pv(12)*pv(39)
 v(413) = pv(13)*pv(39)
 v(414) = pv(14)*pv(39)
 v(415) = pv(15)*pv(39)
 v(416) = pv(16)*pv(39)
 v(417) = pv(17)*pv(39)
 v(418) = pv(18)*pv(39)
 v(419) = pv(19)*pv(39)
 v(420) = pv(20)*pv(39)
 v(421) = pv(21)*pv(39)
 v(422) = pv(22)*pv(39)
 v(423) = pv(23)*pv(39)
 v(424) = pv(24)*pv(39)
 v(425) = pv(25)*pv(39)
 v(426) = pv(26)*pv(39)
 v(427) = pv(27)*pv(39)
 v(428) = pv(28)*pv(39)
 v(429) = pv(29)*pv(39)
 v(430) = pv(30)*pv(39)
 v(431) = pv(31)*pv(39)
 v(432) = pv(32)*pv(39)
 v(433) = pv(33)*pv(39)
 v(434) = pv(34)*pv(39)
 v(435) = pv(35)*pv(39)
endif
if (7.le.mxd) then
 stop 'mg31111: degree 7 not implemented' !! Tried 050815, Magma 18GB
endif
return
END SUBROUTINE mg31111_secs