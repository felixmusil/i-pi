SUBROUTINE mg33_secs (mxd, r, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: v(0:)
!! Note: We stop at degree 7 for now.
!-----------------------------------------------------------------------
real (kind=wp) :: pv(0:size(v)-1) !! an upper bound size
if (size(r).ne.nk*nk.or.size(v).ne.mg33_nsc(mxd)) then
 stop 'mg33_secs: bad dimensions'
endif
call mg33_isecs (mxd, r, pv)
v(0) = 1
if (2.le.mxd) then
 v(1:2) = pv(0:1)
endif
if (3.le.mxd) then
 v(3:14) = pv(2:13)
endif
if (4.le.mxd) then
 v(15) = pv(0)*pv(0)
 v(16) = pv(0)*pv(1)
 v(17) = pv(1)*pv(1)
 v(18:43) = pv(14:39)
endif
if (5.le.mxd) then
 v(44) = pv(1)*pv(2)
 v(45) = pv(1)*pv(3)
 v(46) = pv(0)*pv(4)
 v(47) = pv(1)*pv(4)
 v(48) = pv(0)*pv(5)
 v(49) = pv(1)*pv(5)
 v(50) = pv(0)*pv(6)
 v(51) = pv(1)*pv(6)
 v(52) = pv(0)*pv(7)
 v(53) = pv(1)*pv(7)
 v(54) = pv(0)*pv(8)
 v(55) = pv(1)*pv(8)
 v(56) = pv(0)*pv(9)
 v(57) = pv(1)*pv(9)
 v(58) = pv(0)*pv(10)
 v(59) = pv(0)*pv(11)
 v(60) = pv(1)*pv(11)
 v(61) = pv(0)*pv(12)
 v(62) = pv(1)*pv(12)
 v(63) = pv(0)*pv(13)
 v(64:101) = pv(40:77)
endif
if (6.le.mxd) then
 v(102) = pv(4)*pv(4)
 v(103) = pv(2)*pv(5)
 v(104) = pv(4)*pv(5)
 v(105) = pv(5)*pv(5)
 v(106) = pv(2)*pv(6)
 v(107) = pv(4)*pv(6)
 v(108) = pv(5)*pv(6)
 v(109) = pv(6)*pv(6)
 v(110) = pv(2)*pv(7)
 v(111) = pv(4)*pv(7)
 v(112) = pv(5)*pv(7)
 v(113) = pv(6)*pv(7)
 v(114) = pv(7)*pv(7)
 v(115) = pv(2)*pv(8)
 v(116) = pv(3)*pv(8)
 v(117) = pv(4)*pv(8)
 v(118) = pv(5)*pv(8)
 v(119) = pv(6)*pv(8)
 v(120) = pv(7)*pv(8)
 v(121) = pv(8)*pv(8)
 v(122) = pv(4)*pv(9)
 v(123) = pv(5)*pv(9)
 v(124) = pv(6)*pv(9)
 v(125) = pv(7)*pv(9)
 v(126) = pv(8)*pv(9)
 v(127) = pv(9)*pv(9)
 v(128) = pv(2)*pv(10)
 v(129) = pv(6)*pv(10)
 v(130) = pv(8)*pv(10)
 v(131) = pv(2)*pv(11)
 v(132) = pv(3)*pv(11)
 v(133) = pv(4)*pv(11)
 v(134) = pv(5)*pv(11)
 v(135) = pv(6)*pv(11)
 v(136) = pv(7)*pv(11)
 v(137) = pv(8)*pv(11)
 v(138) = pv(9)*pv(11)
 v(139) = pv(11)*pv(11)
 v(140) = pv(2)*pv(12)
 v(141) = pv(3)*pv(12)
 v(142) = pv(4)*pv(12)
 v(143) = pv(5)*pv(12)
 v(144) = pv(6)*pv(12)
 v(145) = pv(7)*pv(12)
 v(146) = pv(8)*pv(12)
 v(147) = pv(9)*pv(12)
 v(148) = pv(11)*pv(12)
 v(149) = pv(12)*pv(12)
 v(150) = pv(2)*pv(13)
 v(151) = pv(3)*pv(13)
 v(152) = pv(4)*pv(13)
 v(153) = pv(5)*pv(13)
 v(154) = pv(6)*pv(13)
 v(155) = pv(7)*pv(13)
 v(156) = pv(8)*pv(13)
 v(157) = pv(11)*pv(13)
 v(158) = pv(0)*pv(14)
 v(159) = pv(1)*pv(14)
 v(160) = pv(1)*pv(15)
 v(161) = pv(0)*pv(16)
 v(162) = pv(1)*pv(16)
 v(163) = pv(1)*pv(17)
 v(164) = pv(0)*pv(18)
 v(165) = pv(1)*pv(18)
 v(166) = pv(0)*pv(19)
 v(167) = pv(1)*pv(19)
 v(168) = pv(0)*pv(20)
 v(169) = pv(1)*pv(20)
 v(170) = pv(0)*pv(21)
 v(171) = pv(1)*pv(21)
 v(172) = pv(0)*pv(22)
 v(173) = pv(1)*pv(22)
 v(174) = pv(0)*pv(23)
 v(175) = pv(1)*pv(23)
 v(176) = pv(0)*pv(24)
 v(177) = pv(1)*pv(24)
 v(178) = pv(0)*pv(25)
 v(179) = pv(1)*pv(25)
 v(180) = pv(0)*pv(26)
 v(181) = pv(1)*pv(26)
 v(182) = pv(0)*pv(27)
 v(183) = pv(1)*pv(27)
 v(184) = pv(0)*pv(28)
 v(185) = pv(1)*pv(28)
 v(186) = pv(0)*pv(29)
 v(187) = pv(1)*pv(29)
 v(188) = pv(0)*pv(30)
 v(189) = pv(0)*pv(31)
 v(190) = pv(1)*pv(31)
 v(191) = pv(0)*pv(32)
 v(192) = pv(1)*pv(32)
 v(193) = pv(0)*pv(33)
 v(194) = pv(1)*pv(33)
 v(195) = pv(0)*pv(34)
 v(196) = pv(0)*pv(35)
 v(197) = pv(1)*pv(35)
 v(198) = pv(0)*pv(36)
 v(199) = pv(1)*pv(36)
 v(200) = pv(0)*pv(37)
 v(201) = pv(1)*pv(37)
 v(202) = pv(0)*pv(38)
 v(203) = pv(1)*pv(38)
 v(204) = pv(0)*pv(39)
 v(205) = pv(1)*pv(39)
 v(206:226) = pv(78:98)
endif
if (7.le.mxd) then
 v(227) = pv(6)*pv(16)
 v(228) = pv(8)*pv(16)
 v(229) = pv(12)*pv(16)
 v(230) = pv(4)*pv(18)
 v(231) = pv(8)*pv(18)
 v(232) = pv(2)*pv(20)
 v(233) = pv(2)*pv(21)
 v(234) = pv(4)*pv(21)
 v(235) = pv(5)*pv(21)
 v(236) = pv(6)*pv(21)
 v(237) = pv(7)*pv(21)
 v(238) = pv(8)*pv(21)
 v(239) = pv(9)*pv(21)
 v(240) = pv(12)*pv(21)
 v(241) = pv(2)*pv(22)
 v(242) = pv(4)*pv(22)
 v(243) = pv(8)*pv(22)
 v(244) = pv(2)*pv(23)
 v(245) = pv(3)*pv(23)
 v(246) = pv(4)*pv(23)
 v(247) = pv(5)*pv(23)
 v(248) = pv(6)*pv(23)
 v(249) = pv(7)*pv(23)
 v(250) = pv(8)*pv(23)
 v(251) = pv(12)*pv(23)
 v(252) = pv(2)*pv(24)
 v(253) = pv(4)*pv(24)
 v(254) = pv(5)*pv(24)
 v(255) = pv(6)*pv(24)
 v(256) = pv(7)*pv(24)
 v(257) = pv(8)*pv(24)
 v(258) = pv(2)*pv(25)
 v(259) = pv(3)*pv(25)
 v(260) = pv(4)*pv(25)
 v(261) = pv(5)*pv(25)
 v(262) = pv(6)*pv(25)
 v(263) = pv(7)*pv(25)
 v(264) = pv(8)*pv(25)
 v(265) = pv(12)*pv(25)
 v(266) = pv(2)*pv(26)
 v(267) = pv(4)*pv(26)
 v(268) = pv(5)*pv(26)
 v(269) = pv(6)*pv(26)
 v(270) = pv(7)*pv(26)
 v(271) = pv(8)*pv(26)
 v(272) = pv(9)*pv(26)
 v(273) = pv(11)*pv(26)
 v(274) = pv(12)*pv(26)
 v(275) = pv(2)*pv(27)
 v(276) = pv(3)*pv(27)
 v(277) = pv(4)*pv(27)
 v(278) = pv(5)*pv(27)
 v(279) = pv(6)*pv(27)
 v(280) = pv(7)*pv(27)
 v(281) = pv(8)*pv(27)
 v(282) = pv(9)*pv(27)
 v(283) = pv(11)*pv(27)
 v(284) = pv(12)*pv(27)
 v(285) = pv(4)*pv(28)
 v(286) = pv(6)*pv(28)
 v(287) = pv(7)*pv(28)
 v(288) = pv(8)*pv(28)
 v(289) = pv(12)*pv(28)
 v(290) = pv(4)*pv(29)
 v(291) = pv(5)*pv(29)
 v(292) = pv(6)*pv(29)
 v(293) = pv(7)*pv(29)
 v(294) = pv(8)*pv(29)
 v(295) = pv(2)*pv(30)
 v(296) = pv(4)*pv(30)
 v(297) = pv(6)*pv(30)
 v(298) = pv(8)*pv(30)
 v(299) = pv(4)*pv(31)
 v(300) = pv(5)*pv(31)
 v(301) = pv(6)*pv(31)
 v(302) = pv(7)*pv(31)
 v(303) = pv(8)*pv(31)
 v(304) = pv(12)*pv(31)
 v(305) = pv(2)*pv(32)
 v(306) = pv(4)*pv(32)
 v(307) = pv(5)*pv(32)
 v(308) = pv(6)*pv(32)
 v(309) = pv(7)*pv(32)
 v(310) = pv(8)*pv(32)
 v(311) = pv(2)*pv(33)
 v(312) = pv(4)*pv(33)
 v(313) = pv(5)*pv(33)
 v(314) = pv(6)*pv(33)
 v(315) = pv(7)*pv(33)
 v(316) = pv(8)*pv(33)
 v(317) = pv(9)*pv(33)
 v(318) = pv(10)*pv(33)
 v(319) = pv(11)*pv(33)
 v(320) = pv(12)*pv(33)
 v(321) = pv(2)*pv(34)
 v(322) = pv(4)*pv(34)
 v(323) = pv(5)*pv(34)
 v(324) = pv(6)*pv(34)
 v(325) = pv(7)*pv(34)
 v(326) = pv(8)*pv(34)
 v(327) = pv(9)*pv(34)
 v(328) = pv(12)*pv(34)
 v(329) = pv(2)*pv(35)
 v(330) = pv(3)*pv(35)
 v(331) = pv(4)*pv(35)
 v(332) = pv(5)*pv(35)
 v(333) = pv(6)*pv(35)
 v(334) = pv(7)*pv(35)
 v(335) = pv(8)*pv(35)
 v(336) = pv(9)*pv(35)
 v(337) = pv(10)*pv(35)
 v(338) = pv(11)*pv(35)
 v(339) = pv(12)*pv(35)
 v(340) = pv(2)*pv(36)
 v(341) = pv(3)*pv(36)
 v(342) = pv(4)*pv(36)
 v(343) = pv(5)*pv(36)
 v(344) = pv(6)*pv(36)
 v(345) = pv(7)*pv(36)
 v(346) = pv(8)*pv(36)
 v(347) = pv(9)*pv(36)
 v(348) = pv(10)*pv(36)
 v(349) = pv(11)*pv(36)
 v(350) = pv(12)*pv(36)
 v(351) = pv(4)*pv(37)
 v(352) = pv(5)*pv(37)
 v(353) = pv(6)*pv(37)
 v(354) = pv(7)*pv(37)
 v(355) = pv(8)*pv(37)
 v(356) = pv(11)*pv(37)
 v(357) = pv(2)*pv(38)
 v(358) = pv(4)*pv(38)
 v(359) = pv(5)*pv(38)
 v(360) = pv(6)*pv(38)
 v(361) = pv(7)*pv(38)
 v(362) = pv(8)*pv(38)
 v(363) = pv(9)*pv(38)
 v(364) = pv(11)*pv(38)
 v(365) = pv(2)*pv(39)
 v(366) = pv(3)*pv(39)
 v(367) = pv(4)*pv(39)
 v(368) = pv(5)*pv(39)
 v(369) = pv(6)*pv(39)
 v(370) = pv(7)*pv(39)
 v(371) = pv(8)*pv(39)
 v(372) = pv(9)*pv(39)
 v(373) = pv(10)*pv(39)
 v(374) = pv(11)*pv(39)
 v(375) = pv(0)*pv(40)
 v(376) = pv(1)*pv(40)
 v(377) = pv(1)*pv(41)
 v(378) = pv(0)*pv(42)
 v(379) = pv(1)*pv(42)
 v(380) = pv(0)*pv(43)
 v(381) = pv(1)*pv(43)
 v(382) = pv(0)*pv(44)
 v(383) = pv(1)*pv(44)
 v(384) = pv(0)*pv(45)
 v(385) = pv(1)*pv(45)
 v(386) = pv(1)*pv(46)
 v(387) = pv(0)*pv(47)
 v(388) = pv(1)*pv(47)
 v(389) = pv(0)*pv(48)
 v(390) = pv(1)*pv(48)
 v(391) = pv(0)*pv(49)
 v(392) = pv(1)*pv(49)
 v(393) = pv(0)*pv(50)
 v(394) = pv(1)*pv(50)
 v(395) = pv(0)*pv(51)
 v(396) = pv(1)*pv(51)
 v(397) = pv(0)*pv(52)
 v(398) = pv(1)*pv(52)
 v(399) = pv(0)*pv(53)
 v(400) = pv(1)*pv(53)
 v(401) = pv(0)*pv(54)
 v(402) = pv(1)*pv(54)
 v(403) = pv(0)*pv(55)
 v(404) = pv(1)*pv(55)
 v(405) = pv(0)*pv(56)
 v(406) = pv(1)*pv(56)
 v(407) = pv(0)*pv(57)
 v(408) = pv(1)*pv(57)
 v(409) = pv(0)*pv(58)
 v(410) = pv(1)*pv(58)
 v(411) = pv(0)*pv(59)
 v(412) = pv(1)*pv(59)
 v(413) = pv(0)*pv(60)
 v(414) = pv(1)*pv(60)
 v(415) = pv(0)*pv(61)
 v(416) = pv(0)*pv(62)
 v(417) = pv(1)*pv(62)
 v(418) = pv(0)*pv(63)
 v(419) = pv(1)*pv(63)
 v(420) = pv(0)*pv(64)
 v(421) = pv(1)*pv(64)
 v(422) = pv(0)*pv(65)
 v(423) = pv(1)*pv(65)
 v(424) = pv(0)*pv(66)
 v(425) = pv(1)*pv(66)
 v(426) = pv(0)*pv(67)
 v(427) = pv(1)*pv(67)
 v(428) = pv(0)*pv(68)
 v(429) = pv(0)*pv(69)
 v(430) = pv(1)*pv(69)
 v(431) = pv(0)*pv(70)
 v(432) = pv(1)*pv(70)
 v(433) = pv(0)*pv(71)
 v(434) = pv(1)*pv(71)
 v(435) = pv(0)*pv(72)
 v(436) = pv(1)*pv(72)
 v(437) = pv(0)*pv(73)
 v(438) = pv(1)*pv(73)
 v(439) = pv(0)*pv(74)
 v(440) = pv(1)*pv(74)
 v(441) = pv(0)*pv(75)
 v(442) = pv(1)*pv(75)
 v(443) = pv(0)*pv(76)
 v(444) = pv(1)*pv(76)
 v(445) = pv(0)*pv(77)
 v(446) = pv(1)*pv(77)
 v(447:450) = pv(99:102)
endif
if (8.le.mxd) then
 v(451) = pv(21)*pv(21)
 v(452) = pv(16)*pv(27)
 v(453) = pv(21)*pv(27)
 v(454) = pv(26)*pv(27)
 v(455) = pv(27)*pv(27)
 v(456) = pv(16)*pv(36)
 v(457) = pv(27)*pv(36)
 v(458) = pv(36)*pv(36)
 v(459) = pv(16)*pv(39)
 v(460) = pv(4)*pv(40)
 v(461) = pv(6)*pv(43)
 v(462) = pv(8)*pv(43)
 v(463) = pv(10)*pv(43)
 v(464) = pv(11)*pv(43)
 v(465) = pv(12)*pv(43)
 v(466) = pv(4)*pv(44)
 v(467) = pv(6)*pv(44)
 v(468) = pv(8)*pv(44)
 v(469) = pv(9)*pv(44)
 v(470) = pv(10)*pv(44)
 v(471) = pv(11)*pv(44)
 v(472) = pv(12)*pv(44)
 v(473) = pv(13)*pv(44)
 v(474) = pv(4)*pv(45)
 v(475) = pv(9)*pv(45)
 v(476) = pv(2)*pv(46)
 v(477) = pv(4)*pv(46)
 v(478) = pv(6)*pv(46)
 v(479) = pv(9)*pv(46)
 v(480) = pv(2)*pv(48)
 v(481) = pv(4)*pv(48)
 v(482) = pv(6)*pv(48)
 v(483) = pv(9)*pv(48)
 v(484) = pv(11)*pv(48)
 v(485) = pv(12)*pv(48)
 v(486) = pv(4)*pv(49)
 v(487) = pv(6)*pv(49)
 v(488) = pv(9)*pv(49)
 v(489) = pv(2)*pv(50)
 v(490) = pv(3)*pv(50)
 v(491) = pv(4)*pv(50)
 v(492) = pv(5)*pv(50)
 v(493) = pv(6)*pv(50)
 v(494) = pv(7)*pv(50)
 v(495) = pv(8)*pv(50)
 v(496) = pv(9)*pv(50)
 v(497) = pv(10)*pv(50)
 v(498) = pv(11)*pv(50)
 v(499) = pv(12)*pv(50)
 v(500) = pv(13)*pv(50)
 v(501) = pv(2)*pv(51)
 v(502) = pv(3)*pv(51)
 v(503) = pv(4)*pv(51)
 v(504) = pv(5)*pv(51)
 v(505) = pv(6)*pv(51)
 v(506) = pv(7)*pv(51)
 v(507) = pv(8)*pv(51)
 v(508) = pv(9)*pv(51)
 v(509) = pv(2)*pv(52)
 v(510) = pv(3)*pv(52)
 v(511) = pv(4)*pv(52)
 v(512) = pv(5)*pv(52)
 v(513) = pv(6)*pv(52)
 v(514) = pv(7)*pv(52)
 v(515) = pv(8)*pv(52)
 v(516) = pv(9)*pv(52)
 v(517) = pv(10)*pv(52)
 v(518) = pv(11)*pv(52)
 v(519) = pv(12)*pv(52)
 v(520) = pv(13)*pv(52)
 v(521) = pv(2)*pv(53)
 v(522) = pv(3)*pv(53)
 v(523) = pv(4)*pv(53)
 v(524) = pv(5)*pv(53)
 v(525) = pv(6)*pv(53)
 v(526) = pv(7)*pv(53)
 v(527) = pv(8)*pv(53)
 v(528) = pv(9)*pv(53)
 v(529) = pv(2)*pv(54)
 v(530) = pv(3)*pv(54)
 v(531) = pv(4)*pv(54)
 v(532) = pv(5)*pv(54)
 v(533) = pv(6)*pv(54)
 v(534) = pv(7)*pv(54)
 v(535) = pv(8)*pv(54)
 v(536) = pv(9)*pv(54)
 v(537) = pv(10)*pv(54)
 v(538) = pv(11)*pv(54)
 v(539) = pv(12)*pv(54)
 v(540) = pv(13)*pv(54)
 v(541) = pv(2)*pv(55)
 v(542) = pv(3)*pv(55)
 v(543) = pv(4)*pv(55)
 v(544) = pv(5)*pv(55)
 v(545) = pv(6)*pv(55)
 v(546) = pv(7)*pv(55)
 v(547) = pv(8)*pv(55)
 v(548) = pv(9)*pv(55)
 v(549) = pv(2)*pv(56)
 v(550) = pv(3)*pv(56)
 v(551) = pv(4)*pv(56)
 v(552) = pv(5)*pv(56)
 v(553) = pv(6)*pv(56)
 v(554) = pv(7)*pv(56)
 v(555) = pv(8)*pv(56)
 v(556) = pv(9)*pv(56)
 v(557) = pv(11)*pv(56)
 v(558) = pv(12)*pv(56)
 v(559) = pv(2)*pv(57)
 v(560) = pv(3)*pv(57)
 v(561) = pv(4)*pv(57)
 v(562) = pv(5)*pv(57)
 v(563) = pv(6)*pv(57)
 v(564) = pv(7)*pv(57)
 v(565) = pv(8)*pv(57)
 v(566) = pv(9)*pv(57)
 v(567) = pv(10)*pv(57)
 v(568) = pv(11)*pv(57)
 v(569) = pv(12)*pv(57)
 v(570) = pv(2)*pv(58)
 v(571) = pv(3)*pv(58)
 v(572) = pv(4)*pv(58)
 v(573) = pv(5)*pv(58)
 v(574) = pv(6)*pv(58)
 v(575) = pv(7)*pv(58)
 v(576) = pv(8)*pv(58)
 v(577) = pv(9)*pv(58)
 v(578) = pv(10)*pv(58)
 v(579) = pv(11)*pv(58)
 v(580) = pv(12)*pv(58)
 v(581) = pv(3)*pv(59)
 v(582) = pv(4)*pv(59)
 v(583) = pv(5)*pv(59)
 v(584) = pv(6)*pv(59)
 v(585) = pv(7)*pv(59)
 v(586) = pv(8)*pv(59)
 v(587) = pv(10)*pv(59)
 v(588) = pv(11)*pv(59)
 v(589) = pv(12)*pv(59)
 v(590) = pv(2)*pv(60)
 v(591) = pv(4)*pv(60)
 v(592) = pv(5)*pv(60)
 v(593) = pv(6)*pv(60)
 v(594) = pv(7)*pv(60)
 v(595) = pv(8)*pv(60)
 v(596) = pv(9)*pv(60)
 v(597) = pv(10)*pv(60)
 v(598) = pv(11)*pv(60)
 v(599) = pv(12)*pv(60)
 v(600) = pv(4)*pv(61)
 v(601) = pv(5)*pv(61)
 v(602) = pv(6)*pv(61)
 v(603) = pv(7)*pv(61)
 v(604) = pv(8)*pv(61)
 v(605) = pv(9)*pv(61)
 v(606) = pv(3)*pv(62)
 v(607) = pv(4)*pv(62)
 v(608) = pv(5)*pv(62)
 v(609) = pv(6)*pv(62)
 v(610) = pv(7)*pv(62)
 v(611) = pv(8)*pv(62)
 v(612) = pv(9)*pv(62)
 v(613) = pv(10)*pv(62)
 v(614) = pv(11)*pv(62)
 v(615) = pv(12)*pv(62)
 v(616) = pv(2)*pv(63)
 v(617) = pv(3)*pv(63)
 v(618) = pv(4)*pv(63)
 v(619) = pv(5)*pv(63)
 v(620) = pv(6)*pv(63)
 v(621) = pv(7)*pv(63)
 v(622) = pv(8)*pv(63)
 v(623) = pv(9)*pv(63)
 v(624) = pv(10)*pv(63)
 v(625) = pv(11)*pv(63)
 v(626) = pv(12)*pv(63)
 v(627) = pv(2)*pv(64)
 v(628) = pv(4)*pv(64)
 v(629) = pv(5)*pv(64)
 v(630) = pv(6)*pv(64)
 v(631) = pv(7)*pv(64)
 v(632) = pv(8)*pv(64)
 v(633) = pv(9)*pv(64)
 v(634) = pv(12)*pv(64)
 v(635) = pv(2)*pv(65)
 v(636) = pv(3)*pv(65)
 v(637) = pv(4)*pv(65)
 v(638) = pv(5)*pv(65)
 v(639) = pv(6)*pv(65)
 v(640) = pv(7)*pv(65)
 v(641) = pv(8)*pv(65)
 v(642) = pv(9)*pv(65)
 v(643) = pv(10)*pv(65)
 v(644) = pv(11)*pv(65)
 v(645) = pv(12)*pv(65)
 v(646) = pv(13)*pv(65)
 v(647) = pv(2)*pv(66)
 v(648) = pv(3)*pv(66)
 v(649) = pv(4)*pv(66)
 v(650) = pv(5)*pv(66)
 v(651) = pv(6)*pv(66)
 v(652) = pv(7)*pv(66)
 v(653) = pv(8)*pv(66)
 v(654) = pv(9)*pv(66)
 v(655) = pv(12)*pv(66)
 v(656) = pv(2)*pv(67)
 v(657) = pv(3)*pv(67)
 v(658) = pv(4)*pv(67)
 v(659) = pv(5)*pv(67)
 v(660) = pv(6)*pv(67)
 v(661) = pv(7)*pv(67)
 v(662) = pv(8)*pv(67)
 v(663) = pv(9)*pv(67)
 v(664) = pv(10)*pv(67)
 v(665) = pv(11)*pv(67)
 v(666) = pv(12)*pv(67)
 v(667) = pv(13)*pv(67)
 v(668) = pv(2)*pv(68)
 v(669) = pv(3)*pv(68)
 v(670) = pv(4)*pv(68)
 v(671) = pv(5)*pv(68)
 v(672) = pv(6)*pv(68)
 v(673) = pv(7)*pv(68)
 v(674) = pv(8)*pv(68)
 v(675) = pv(9)*pv(68)
 v(676) = pv(10)*pv(68)
 v(677) = pv(11)*pv(68)
 v(678) = pv(12)*pv(68)
 v(679) = pv(2)*pv(69)
 v(680) = pv(3)*pv(69)
 v(681) = pv(4)*pv(69)
 v(682) = pv(5)*pv(69)
 v(683) = pv(6)*pv(69)
 v(684) = pv(7)*pv(69)
 v(685) = pv(8)*pv(69)
 v(686) = pv(9)*pv(69)
 v(687) = pv(10)*pv(69)
 v(688) = pv(11)*pv(69)
 v(689) = pv(12)*pv(69)
 v(690) = pv(2)*pv(70)
 v(691) = pv(3)*pv(70)
 v(692) = pv(4)*pv(70)
 v(693) = pv(5)*pv(70)
 v(694) = pv(6)*pv(70)
 v(695) = pv(7)*pv(70)
 v(696) = pv(8)*pv(70)
 v(697) = pv(9)*pv(70)
 v(698) = pv(10)*pv(70)
 v(699) = pv(11)*pv(70)
 v(700) = pv(12)*pv(70)
 v(701) = pv(2)*pv(71)
 v(702) = pv(3)*pv(71)
 v(703) = pv(4)*pv(71)
 v(704) = pv(5)*pv(71)
 v(705) = pv(6)*pv(71)
 v(706) = pv(7)*pv(71)
 v(707) = pv(8)*pv(71)
 v(708) = pv(9)*pv(71)
 v(709) = pv(10)*pv(71)
 v(710) = pv(11)*pv(71)
 v(711) = pv(12)*pv(71)
 v(712) = pv(4)*pv(72)
 v(713) = pv(5)*pv(72)
 v(714) = pv(6)*pv(72)
 v(715) = pv(7)*pv(72)
 v(716) = pv(8)*pv(72)
 v(717) = pv(9)*pv(72)
 v(718) = pv(10)*pv(72)
 v(719) = pv(11)*pv(72)
 v(720) = pv(12)*pv(72)
 v(721) = pv(2)*pv(73)
 v(722) = pv(4)*pv(73)
 v(723) = pv(5)*pv(73)
 v(724) = pv(6)*pv(73)
 v(725) = pv(7)*pv(73)
 v(726) = pv(8)*pv(73)
 v(727) = pv(9)*pv(73)
 v(728) = pv(10)*pv(73)
 v(729) = pv(11)*pv(73)
 v(730) = pv(12)*pv(73)
 v(731) = pv(2)*pv(74)
 v(732) = pv(4)*pv(74)
 v(733) = pv(5)*pv(74)
 v(734) = pv(6)*pv(74)
 v(735) = pv(7)*pv(74)
 v(736) = pv(8)*pv(74)
 v(737) = pv(9)*pv(74)
 v(738) = pv(10)*pv(74)
 v(739) = pv(11)*pv(74)
 v(740) = pv(12)*pv(74)
 v(741) = pv(2)*pv(75)
 v(742) = pv(4)*pv(75)
 v(743) = pv(5)*pv(75)
 v(744) = pv(6)*pv(75)
 v(745) = pv(7)*pv(75)
 v(746) = pv(8)*pv(75)
 v(747) = pv(9)*pv(75)
 v(748) = pv(10)*pv(75)
 v(749) = pv(11)*pv(75)
 v(750) = pv(12)*pv(75)
 v(751) = pv(2)*pv(76)
 v(752) = pv(3)*pv(76)
 v(753) = pv(4)*pv(76)
 v(754) = pv(5)*pv(76)
 v(755) = pv(6)*pv(76)
 v(756) = pv(7)*pv(76)
 v(757) = pv(8)*pv(76)
 v(758) = pv(9)*pv(76)
 v(759) = pv(10)*pv(76)
 v(760) = pv(11)*pv(76)
 v(761) = pv(12)*pv(76)
 v(762) = pv(2)*pv(77)
 v(763) = pv(3)*pv(77)
 v(764) = pv(4)*pv(77)
 v(765) = pv(5)*pv(77)
 v(766) = pv(6)*pv(77)
 v(767) = pv(7)*pv(77)
 v(768) = pv(8)*pv(77)
 v(769) = pv(9)*pv(77)
 v(770) = pv(10)*pv(77)
 v(771) = pv(11)*pv(77)
 v(772) = pv(12)*pv(77)
 v(773) = pv(0)*pv(78)
 v(774) = pv(1)*pv(78)
 v(775) = pv(1)*pv(79)
 v(776) = pv(0)*pv(80)
 v(777) = pv(1)*pv(80)
 v(778) = pv(0)*pv(81)
 v(779) = pv(1)*pv(81)
 v(780) = pv(0)*pv(82)
 v(781) = pv(1)*pv(82)
 v(782) = pv(0)*pv(83)
 v(783) = pv(1)*pv(83)
 v(784) = pv(0)*pv(84)
 v(785) = pv(1)*pv(84)
 v(786) = pv(0)*pv(85)
 v(787) = pv(1)*pv(85)
 v(788) = pv(0)*pv(86)
 v(789) = pv(1)*pv(86)
 v(790) = pv(0)*pv(87)
 v(791) = pv(1)*pv(87)
 v(792) = pv(0)*pv(88)
 v(793) = pv(1)*pv(88)
 v(794) = pv(0)*pv(89)
 v(795) = pv(1)*pv(89)
 v(796) = pv(0)*pv(90)
 v(797) = pv(1)*pv(90)
 v(798) = pv(0)*pv(91)
 v(799) = pv(1)*pv(91)
 v(800) = pv(0)*pv(92)
 v(801) = pv(1)*pv(92)
 v(802) = pv(0)*pv(93)
 v(803) = pv(1)*pv(93)
 v(804) = pv(0)*pv(94)
 v(805) = pv(1)*pv(94)
 v(806) = pv(0)*pv(95)
 v(807) = pv(1)*pv(95)
 v(808) = pv(0)*pv(96)
 v(809) = pv(1)*pv(96)
 v(810) = pv(0)*pv(97)
 v(811) = pv(1)*pv(97)
 v(812) = pv(0)*pv(98)
endif
if (9.le.mxd) then
 stop 'mg33: degree 9 not implemented' !! Tried 050815
endif
return
END SUBROUTINE mg33_secs