SUBROUTINE mg42_secs (mxd, r, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: v(0:)
!! Note: We stop at degree 7 for now.
!-----------------------------------------------------------------------
real (kind=wp) :: pv(0:size(v)-1) !! an upper bound size
if (size(r).ne.nk*nk.or.size(v).ne.mg42_nsc(mxd)) then
 stop 'mg42_secs: bad dimensions'
endif
call mg42_isecs (mxd, r, pv)
v(0) = 1
if (2.le.mxd) then
 v(1:1) = pv(0:0)
endif
if (3.le.mxd) then
 v(2:11) = pv(1:10)
endif
if (4.le.mxd) then
 v(12) = pv(0)*pv(0)
 v(13:35) = pv(11:33)
endif
if (5.le.mxd) then
 v(36) = pv(0)*pv(1)
 v(37) = pv(0)*pv(2)
 v(38) = pv(0)*pv(3)
 v(39) = pv(0)*pv(4)
 v(40) = pv(0)*pv(5)
 v(41) = pv(0)*pv(6)
 v(42) = pv(0)*pv(7)
 v(43) = pv(0)*pv(8)
 v(44) = pv(0)*pv(9)
 v(45) = pv(0)*pv(10)
 v(46:82) = pv(34:70)
endif
if (6.le.mxd) then
 v(83) = pv(1)*pv(1)
 v(84) = pv(1)*pv(2)
 v(85) = pv(1)*pv(3)
 v(86) = pv(1)*pv(4)
 v(87) = pv(3)*pv(4)
 v(88) = pv(1)*pv(5)
 v(89) = pv(2)*pv(5)
 v(90) = pv(3)*pv(5)
 v(91) = pv(1)*pv(6)
 v(92) = pv(2)*pv(6)
 v(93) = pv(3)*pv(6)
 v(94) = pv(5)*pv(6)
 v(95) = pv(6)*pv(6)
 v(96) = pv(1)*pv(7)
 v(97) = pv(2)*pv(7)
 v(98) = pv(3)*pv(7)
 v(99) = pv(4)*pv(7)
 v(100) = pv(5)*pv(7)
 v(101) = pv(6)*pv(7)
 v(102) = pv(7)*pv(7)
 v(103) = pv(1)*pv(8)
 v(104) = pv(2)*pv(8)
 v(105) = pv(3)*pv(8)
 v(106) = pv(4)*pv(8)
 v(107) = pv(5)*pv(8)
 v(108) = pv(6)*pv(8)
 v(109) = pv(7)*pv(8)
 v(110) = pv(8)*pv(8)
 v(111) = pv(1)*pv(9)
 v(112) = pv(2)*pv(9)
 v(113) = pv(3)*pv(9)
 v(114) = pv(4)*pv(9)
 v(115) = pv(5)*pv(9)
 v(116) = pv(6)*pv(9)
 v(117) = pv(7)*pv(9)
 v(118) = pv(8)*pv(9)
 v(119) = pv(9)*pv(9)
 v(120) = pv(1)*pv(10)
 v(121) = pv(2)*pv(10)
 v(122) = pv(3)*pv(10)
 v(123) = pv(4)*pv(10)
 v(124) = pv(5)*pv(10)
 v(125) = pv(6)*pv(10)
 v(126) = pv(7)*pv(10)
 v(127) = pv(8)*pv(10)
 v(128) = pv(9)*pv(10)
 v(129) = pv(10)*pv(10)
 v(130) = pv(0)*pv(11)
 v(131) = pv(0)*pv(12)
 v(132) = pv(0)*pv(13)
 v(133) = pv(0)*pv(14)
 v(134) = pv(0)*pv(15)
 v(135) = pv(0)*pv(16)
 v(136) = pv(0)*pv(17)
 v(137) = pv(0)*pv(18)
 v(138) = pv(0)*pv(19)
 v(139) = pv(0)*pv(20)
 v(140) = pv(0)*pv(21)
 v(141) = pv(0)*pv(22)
 v(142) = pv(0)*pv(23)
 v(143) = pv(0)*pv(24)
 v(144) = pv(0)*pv(25)
 v(145) = pv(0)*pv(26)
 v(146) = pv(0)*pv(27)
 v(147) = pv(0)*pv(28)
 v(148) = pv(0)*pv(29)
 v(149) = pv(0)*pv(30)
 v(150) = pv(0)*pv(31)
 v(151) = pv(0)*pv(32)
 v(152) = pv(0)*pv(33)
 v(153:183) = pv(71:101)
endif
if (7.le.mxd) then
 v(184) = pv(9)*pv(11)
 v(185) = pv(10)*pv(11)
 v(186) = pv(10)*pv(12)
 v(187) = pv(1)*pv(13)
 v(188) = pv(1)*pv(14)
 v(189) = pv(7)*pv(14)
 v(190) = pv(9)*pv(14)
 v(191) = pv(10)*pv(14)
 v(192) = pv(1)*pv(15)
 v(193) = pv(3)*pv(15)
 v(194) = pv(1)*pv(16)
 v(195) = pv(2)*pv(16)
 v(196) = pv(3)*pv(16)
 v(197) = pv(1)*pv(17)
 v(198) = pv(1)*pv(18)
 v(199) = pv(2)*pv(18)
 v(200) = pv(3)*pv(18)
 v(201) = pv(9)*pv(18)
 v(202) = pv(10)*pv(18)
 v(203) = pv(1)*pv(19)
 v(204) = pv(2)*pv(19)
 v(205) = pv(3)*pv(19)
 v(206) = pv(4)*pv(19)
 v(207) = pv(6)*pv(19)
 v(208) = pv(7)*pv(19)
 v(209) = pv(8)*pv(19)
 v(210) = pv(9)*pv(19)
 v(211) = pv(10)*pv(19)
 v(212) = pv(1)*pv(20)
 v(213) = pv(2)*pv(20)
 v(214) = pv(3)*pv(20)
 v(215) = pv(4)*pv(20)
 v(216) = pv(7)*pv(20)
 v(217) = pv(8)*pv(20)
 v(218) = pv(9)*pv(20)
 v(219) = pv(10)*pv(20)
 v(220) = pv(1)*pv(21)
 v(221) = pv(2)*pv(21)
 v(222) = pv(3)*pv(21)
 v(223) = pv(4)*pv(21)
 v(224) = pv(5)*pv(21)
 v(225) = pv(6)*pv(21)
 v(226) = pv(7)*pv(21)
 v(227) = pv(8)*pv(21)
 v(228) = pv(9)*pv(21)
 v(229) = pv(10)*pv(21)
 v(230) = pv(1)*pv(22)
 v(231) = pv(2)*pv(22)
 v(232) = pv(3)*pv(22)
 v(233) = pv(6)*pv(22)
 v(234) = pv(7)*pv(22)
 v(235) = pv(9)*pv(22)
 v(236) = pv(10)*pv(22)
 v(237) = pv(1)*pv(23)
 v(238) = pv(2)*pv(23)
 v(239) = pv(3)*pv(23)
 v(240) = pv(6)*pv(23)
 v(241) = pv(7)*pv(23)
 v(242) = pv(9)*pv(23)
 v(243) = pv(10)*pv(23)
 v(244) = pv(1)*pv(24)
 v(245) = pv(2)*pv(24)
 v(246) = pv(3)*pv(24)
 v(247) = pv(4)*pv(24)
 v(248) = pv(6)*pv(24)
 v(249) = pv(7)*pv(24)
 v(250) = pv(8)*pv(24)
 v(251) = pv(9)*pv(24)
 v(252) = pv(10)*pv(24)
 v(253) = pv(1)*pv(25)
 v(254) = pv(2)*pv(25)
 v(255) = pv(3)*pv(25)
 v(256) = pv(7)*pv(25)
 v(257) = pv(9)*pv(25)
 v(258) = pv(10)*pv(25)
 v(259) = pv(1)*pv(26)
 v(260) = pv(2)*pv(26)
 v(261) = pv(3)*pv(26)
 v(262) = pv(4)*pv(26)
 v(263) = pv(5)*pv(26)
 v(264) = pv(6)*pv(26)
 v(265) = pv(7)*pv(26)
 v(266) = pv(8)*pv(26)
 v(267) = pv(9)*pv(26)
 v(268) = pv(10)*pv(26)
 v(269) = pv(1)*pv(27)
 v(270) = pv(2)*pv(27)
 v(271) = pv(3)*pv(27)
 v(272) = pv(4)*pv(27)
 v(273) = pv(5)*pv(27)
 v(274) = pv(6)*pv(27)
 v(275) = pv(7)*pv(27)
 v(276) = pv(8)*pv(27)
 v(277) = pv(9)*pv(27)
 v(278) = pv(10)*pv(27)
 v(279) = pv(1)*pv(28)
 v(280) = pv(2)*pv(28)
 v(281) = pv(3)*pv(28)
 v(282) = pv(4)*pv(28)
 v(283) = pv(5)*pv(28)
 v(284) = pv(6)*pv(28)
 v(285) = pv(7)*pv(28)
 v(286) = pv(8)*pv(28)
 v(287) = pv(9)*pv(28)
 v(288) = pv(10)*pv(28)
 v(289) = pv(1)*pv(29)
 v(290) = pv(2)*pv(29)
 v(291) = pv(3)*pv(29)
 v(292) = pv(4)*pv(29)
 v(293) = pv(5)*pv(29)
 v(294) = pv(6)*pv(29)
 v(295) = pv(7)*pv(29)
 v(296) = pv(8)*pv(29)
 v(297) = pv(9)*pv(29)
 v(298) = pv(10)*pv(29)
 v(299) = pv(1)*pv(30)
 v(300) = pv(2)*pv(30)
 v(301) = pv(3)*pv(30)
 v(302) = pv(6)*pv(30)
 v(303) = pv(7)*pv(30)
 v(304) = pv(8)*pv(30)
 v(305) = pv(9)*pv(30)
 v(306) = pv(10)*pv(30)
 v(307) = pv(1)*pv(31)
 v(308) = pv(2)*pv(31)
 v(309) = pv(3)*pv(31)
 v(310) = pv(4)*pv(31)
 v(311) = pv(5)*pv(31)
 v(312) = pv(6)*pv(31)
 v(313) = pv(7)*pv(31)
 v(314) = pv(8)*pv(31)
 v(315) = pv(9)*pv(31)
 v(316) = pv(10)*pv(31)
 v(317) = pv(1)*pv(32)
 v(318) = pv(2)*pv(32)
 v(319) = pv(3)*pv(32)
 v(320) = pv(4)*pv(32)
 v(321) = pv(5)*pv(32)
 v(322) = pv(6)*pv(32)
 v(323) = pv(7)*pv(32)
 v(324) = pv(8)*pv(32)
 v(325) = pv(9)*pv(32)
 v(326) = pv(10)*pv(32)
 v(327) = pv(1)*pv(33)
 v(328) = pv(2)*pv(33)
 v(329) = pv(3)*pv(33)
 v(330) = pv(4)*pv(33)
 v(331) = pv(5)*pv(33)
 v(332) = pv(6)*pv(33)
 v(333) = pv(7)*pv(33)
 v(334) = pv(8)*pv(33)
 v(335) = pv(9)*pv(33)
 v(336) = pv(10)*pv(33)
 v(337) = pv(0)*pv(34)
 v(338) = pv(0)*pv(35)
 v(339) = pv(0)*pv(36)
 v(340) = pv(0)*pv(37)
 v(341) = pv(0)*pv(38)
 v(342) = pv(0)*pv(39)
 v(343) = pv(0)*pv(40)
 v(344) = pv(0)*pv(41)
 v(345) = pv(0)*pv(42)
 v(346) = pv(0)*pv(43)
 v(347) = pv(0)*pv(44)
 v(348) = pv(0)*pv(45)
 v(349) = pv(0)*pv(46)
 v(350) = pv(0)*pv(47)
 v(351) = pv(0)*pv(48)
 v(352) = pv(0)*pv(49)
 v(353) = pv(0)*pv(50)
 v(354) = pv(0)*pv(51)
 v(355) = pv(0)*pv(52)
 v(356) = pv(0)*pv(53)
 v(357) = pv(0)*pv(54)
 v(358) = pv(0)*pv(55)
 v(359) = pv(0)*pv(56)
 v(360) = pv(0)*pv(57)
 v(361) = pv(0)*pv(58)
 v(362) = pv(0)*pv(59)
 v(363) = pv(0)*pv(60)
 v(364) = pv(0)*pv(61)
 v(365) = pv(0)*pv(62)
 v(366) = pv(0)*pv(63)
 v(367) = pv(0)*pv(64)
 v(368) = pv(0)*pv(65)
 v(369) = pv(0)*pv(66)
 v(370) = pv(0)*pv(67)
 v(371) = pv(0)*pv(68)
 v(372) = pv(0)*pv(69)
 v(373) = pv(0)*pv(70)
 v(374:378) = pv(102:106)
endif
if (8.le.mxd) then
 v(379) = pv(21)*pv(31)
 v(380) = pv(24)*pv(31)
 v(381) = pv(28)*pv(31)
 v(382) = pv(28)*pv(32)
 v(383) = pv(31)*pv(32)
 v(384) = pv(14)*pv(33)
 v(385) = pv(24)*pv(33)
 v(386) = pv(26)*pv(33)
 v(387) = pv(31)*pv(33)
 v(388) = pv(10)*pv(34)
 v(389) = pv(1)*pv(35)
 v(390) = pv(10)*pv(35)
 v(391) = pv(1)*pv(36)
 v(392) = pv(2)*pv(36)
 v(393) = pv(3)*pv(36)
 v(394) = pv(6)*pv(36)
 v(395) = pv(7)*pv(36)
 v(396) = pv(9)*pv(36)
 v(397) = pv(10)*pv(36)
 v(398) = pv(1)*pv(37)
 v(399) = pv(3)*pv(37)
 v(400) = pv(1)*pv(38)
 v(401) = pv(2)*pv(38)
 v(402) = pv(3)*pv(38)
 v(403) = pv(7)*pv(38)
 v(404) = pv(1)*pv(39)
 v(405) = pv(2)*pv(39)
 v(406) = pv(3)*pv(39)
 v(407) = pv(6)*pv(39)
 v(408) = pv(7)*pv(39)
 v(409) = pv(9)*pv(39)
 v(410) = pv(10)*pv(39)
 v(411) = pv(1)*pv(40)
 v(412) = pv(3)*pv(40)
 v(413) = pv(1)*pv(41)
 v(414) = pv(2)*pv(41)
 v(415) = pv(3)*pv(41)
 v(416) = pv(1)*pv(42)
 v(417) = pv(2)*pv(42)
 v(418) = pv(3)*pv(42)
 v(419) = pv(1)*pv(43)
 v(420) = pv(10)*pv(43)
 v(421) = pv(2)*pv(44)
 v(422) = pv(3)*pv(44)
 v(423) = pv(4)*pv(44)
 v(424) = pv(5)*pv(44)
 v(425) = pv(6)*pv(44)
 v(426) = pv(7)*pv(44)
 v(427) = pv(8)*pv(44)
 v(428) = pv(9)*pv(44)
 v(429) = pv(10)*pv(44)
 v(430) = pv(1)*pv(45)
 v(431) = pv(2)*pv(45)
 v(432) = pv(3)*pv(45)
 v(433) = pv(4)*pv(45)
 v(434) = pv(5)*pv(45)
 v(435) = pv(6)*pv(45)
 v(436) = pv(7)*pv(45)
 v(437) = pv(8)*pv(45)
 v(438) = pv(9)*pv(45)
 v(439) = pv(10)*pv(45)
 v(440) = pv(1)*pv(46)
 v(441) = pv(2)*pv(46)
 v(442) = pv(3)*pv(46)
 v(443) = pv(4)*pv(46)
 v(444) = pv(5)*pv(46)
 v(445) = pv(6)*pv(46)
 v(446) = pv(7)*pv(46)
 v(447) = pv(8)*pv(46)
 v(448) = pv(9)*pv(46)
 v(449) = pv(10)*pv(46)
 v(450) = pv(1)*pv(47)
 v(451) = pv(2)*pv(47)
 v(452) = pv(3)*pv(47)
 v(453) = pv(4)*pv(47)
 v(454) = pv(5)*pv(47)
 v(455) = pv(6)*pv(47)
 v(456) = pv(7)*pv(47)
 v(457) = pv(8)*pv(47)
 v(458) = pv(9)*pv(47)
 v(459) = pv(10)*pv(47)
 v(460) = pv(1)*pv(48)
 v(461) = pv(2)*pv(48)
 v(462) = pv(3)*pv(48)
 v(463) = pv(6)*pv(48)
 v(464) = pv(7)*pv(48)
 v(465) = pv(9)*pv(48)
 v(466) = pv(10)*pv(48)
 v(467) = pv(1)*pv(49)
 v(468) = pv(2)*pv(49)
 v(469) = pv(3)*pv(49)
 v(470) = pv(4)*pv(49)
 v(471) = pv(6)*pv(49)
 v(472) = pv(7)*pv(49)
 v(473) = pv(9)*pv(49)
 v(474) = pv(10)*pv(49)
 v(475) = pv(1)*pv(50)
 v(476) = pv(2)*pv(50)
 v(477) = pv(3)*pv(50)
 v(478) = pv(6)*pv(50)
 v(479) = pv(7)*pv(50)
 v(480) = pv(9)*pv(50)
 v(481) = pv(10)*pv(50)
 v(482) = pv(1)*pv(51)
 v(483) = pv(2)*pv(51)
 v(484) = pv(3)*pv(51)
 v(485) = pv(4)*pv(51)
 v(486) = pv(5)*pv(51)
 v(487) = pv(6)*pv(51)
 v(488) = pv(7)*pv(51)
 v(489) = pv(8)*pv(51)
 v(490) = pv(9)*pv(51)
 v(491) = pv(10)*pv(51)
 v(492) = pv(1)*pv(52)
 v(493) = pv(2)*pv(52)
 v(494) = pv(3)*pv(52)
 v(495) = pv(4)*pv(52)
 v(496) = pv(5)*pv(52)
 v(497) = pv(6)*pv(52)
 v(498) = pv(7)*pv(52)
 v(499) = pv(8)*pv(52)
 v(500) = pv(9)*pv(52)
 v(501) = pv(10)*pv(52)
 v(502) = pv(1)*pv(53)
 v(503) = pv(2)*pv(53)
 v(504) = pv(3)*pv(53)
 v(505) = pv(4)*pv(53)
 v(506) = pv(5)*pv(53)
 v(507) = pv(6)*pv(53)
 v(508) = pv(7)*pv(53)
 v(509) = pv(8)*pv(53)
 v(510) = pv(9)*pv(53)
 v(511) = pv(10)*pv(53)
 v(512) = pv(1)*pv(54)
 v(513) = pv(2)*pv(54)
 v(514) = pv(3)*pv(54)
 v(515) = pv(4)*pv(54)
 v(516) = pv(5)*pv(54)
 v(517) = pv(6)*pv(54)
 v(518) = pv(7)*pv(54)
 v(519) = pv(8)*pv(54)
 v(520) = pv(9)*pv(54)
 v(521) = pv(10)*pv(54)
 v(522) = pv(1)*pv(55)
 v(523) = pv(2)*pv(55)
 v(524) = pv(3)*pv(55)
 v(525) = pv(4)*pv(55)
 v(526) = pv(6)*pv(55)
 v(527) = pv(7)*pv(55)
 v(528) = pv(8)*pv(55)
 v(529) = pv(9)*pv(55)
 v(530) = pv(10)*pv(55)
 v(531) = pv(1)*pv(56)
 v(532) = pv(2)*pv(56)
 v(533) = pv(3)*pv(56)
 v(534) = pv(4)*pv(56)
 v(535) = pv(5)*pv(56)
 v(536) = pv(6)*pv(56)
 v(537) = pv(7)*pv(56)
 v(538) = pv(8)*pv(56)
 v(539) = pv(9)*pv(56)
 v(540) = pv(10)*pv(56)
 v(541) = pv(1)*pv(57)
 v(542) = pv(2)*pv(57)
 v(543) = pv(3)*pv(57)
 v(544) = pv(5)*pv(57)
 v(545) = pv(6)*pv(57)
 v(546) = pv(7)*pv(57)
 v(547) = pv(8)*pv(57)
 v(548) = pv(9)*pv(57)
 v(549) = pv(10)*pv(57)
 v(550) = pv(1)*pv(58)
 v(551) = pv(2)*pv(58)
 v(552) = pv(3)*pv(58)
 v(553) = pv(4)*pv(58)
 v(554) = pv(5)*pv(58)
 v(555) = pv(6)*pv(58)
 v(556) = pv(7)*pv(58)
 v(557) = pv(8)*pv(58)
 v(558) = pv(9)*pv(58)
 v(559) = pv(10)*pv(58)
 v(560) = pv(1)*pv(59)
 v(561) = pv(2)*pv(59)
 v(562) = pv(3)*pv(59)
 v(563) = pv(4)*pv(59)
 v(564) = pv(5)*pv(59)
 v(565) = pv(6)*pv(59)
 v(566) = pv(7)*pv(59)
 v(567) = pv(8)*pv(59)
 v(568) = pv(9)*pv(59)
 v(569) = pv(10)*pv(59)
 v(570) = pv(1)*pv(60)
 v(571) = pv(2)*pv(60)
 v(572) = pv(3)*pv(60)
 v(573) = pv(4)*pv(60)
 v(574) = pv(5)*pv(60)
 v(575) = pv(6)*pv(60)
 v(576) = pv(7)*pv(60)
 v(577) = pv(8)*pv(60)
 v(578) = pv(9)*pv(60)
 v(579) = pv(10)*pv(60)
 v(580) = pv(1)*pv(61)
 v(581) = pv(2)*pv(61)
 v(582) = pv(3)*pv(61)
 v(583) = pv(4)*pv(61)
 v(584) = pv(5)*pv(61)
 v(585) = pv(6)*pv(61)
 v(586) = pv(7)*pv(61)
 v(587) = pv(8)*pv(61)
 v(588) = pv(9)*pv(61)
 v(589) = pv(10)*pv(61)
 v(590) = pv(1)*pv(62)
 v(591) = pv(2)*pv(62)
 v(592) = pv(3)*pv(62)
 v(593) = pv(6)*pv(62)
 v(594) = pv(7)*pv(62)
 v(595) = pv(8)*pv(62)
 v(596) = pv(9)*pv(62)
 v(597) = pv(10)*pv(62)
 v(598) = pv(1)*pv(63)
 v(599) = pv(2)*pv(63)
 v(600) = pv(3)*pv(63)
 v(601) = pv(6)*pv(63)
 v(602) = pv(7)*pv(63)
 v(603) = pv(8)*pv(63)
 v(604) = pv(9)*pv(63)
 v(605) = pv(10)*pv(63)
 v(606) = pv(1)*pv(64)
 v(607) = pv(2)*pv(64)
 v(608) = pv(3)*pv(64)
 v(609) = pv(4)*pv(64)
 v(610) = pv(5)*pv(64)
 v(611) = pv(6)*pv(64)
 v(612) = pv(7)*pv(64)
 v(613) = pv(8)*pv(64)
 v(614) = pv(9)*pv(64)
 v(615) = pv(10)*pv(64)
 v(616) = pv(1)*pv(65)
 v(617) = pv(2)*pv(65)
 v(618) = pv(3)*pv(65)
 v(619) = pv(4)*pv(65)
 v(620) = pv(5)*pv(65)
 v(621) = pv(6)*pv(65)
 v(622) = pv(7)*pv(65)
 v(623) = pv(8)*pv(65)
 v(624) = pv(9)*pv(65)
 v(625) = pv(10)*pv(65)
 v(626) = pv(1)*pv(66)
 v(627) = pv(2)*pv(66)
 v(628) = pv(3)*pv(66)
 v(629) = pv(4)*pv(66)
 v(630) = pv(5)*pv(66)
 v(631) = pv(6)*pv(66)
 v(632) = pv(7)*pv(66)
 v(633) = pv(8)*pv(66)
 v(634) = pv(9)*pv(66)
 v(635) = pv(10)*pv(66)
 v(636) = pv(1)*pv(67)
 v(637) = pv(2)*pv(67)
 v(638) = pv(3)*pv(67)
 v(639) = pv(4)*pv(67)
 v(640) = pv(5)*pv(67)
 v(641) = pv(6)*pv(67)
 v(642) = pv(7)*pv(67)
 v(643) = pv(8)*pv(67)
 v(644) = pv(9)*pv(67)
 v(645) = pv(10)*pv(67)
 v(646) = pv(1)*pv(68)
 v(647) = pv(2)*pv(68)
 v(648) = pv(3)*pv(68)
 v(649) = pv(4)*pv(68)
 v(650) = pv(5)*pv(68)
 v(651) = pv(6)*pv(68)
 v(652) = pv(7)*pv(68)
 v(653) = pv(8)*pv(68)
 v(654) = pv(9)*pv(68)
 v(655) = pv(10)*pv(68)
 v(656) = pv(1)*pv(69)
 v(657) = pv(2)*pv(69)
 v(658) = pv(3)*pv(69)
 v(659) = pv(4)*pv(69)
 v(660) = pv(5)*pv(69)
 v(661) = pv(6)*pv(69)
 v(662) = pv(7)*pv(69)
 v(663) = pv(8)*pv(69)
 v(664) = pv(9)*pv(69)
 v(665) = pv(10)*pv(69)
 v(666) = pv(1)*pv(70)
 v(667) = pv(2)*pv(70)
 v(668) = pv(3)*pv(70)
 v(669) = pv(4)*pv(70)
 v(670) = pv(5)*pv(70)
 v(671) = pv(6)*pv(70)
 v(672) = pv(7)*pv(70)
 v(673) = pv(8)*pv(70)
 v(674) = pv(9)*pv(70)
 v(675) = pv(10)*pv(70)
 v(676) = pv(0)*pv(71)
 v(677) = pv(0)*pv(72)
 v(678) = pv(0)*pv(73)
 v(679) = pv(0)*pv(74)
 v(680) = pv(0)*pv(75)
 v(681) = pv(0)*pv(76)
 v(682) = pv(0)*pv(77)
 v(683) = pv(0)*pv(78)
 v(684) = pv(0)*pv(79)
 v(685) = pv(0)*pv(80)
 v(686) = pv(0)*pv(81)
 v(687) = pv(0)*pv(82)
 v(688) = pv(0)*pv(83)
 v(689) = pv(0)*pv(84)
 v(690) = pv(0)*pv(85)
 v(691) = pv(0)*pv(86)
 v(692) = pv(0)*pv(87)
 v(693) = pv(0)*pv(88)
 v(694) = pv(0)*pv(89)
 v(695) = pv(0)*pv(90)
 v(696) = pv(0)*pv(91)
 v(697) = pv(0)*pv(92)
 v(698) = pv(0)*pv(93)
 v(699) = pv(0)*pv(94)
 v(700) = pv(0)*pv(95)
 v(701) = pv(0)*pv(96)
 v(702) = pv(0)*pv(97)
 v(703) = pv(0)*pv(98)
 v(704) = pv(0)*pv(99)
 v(705) = pv(0)*pv(100)
 v(706) = pv(0)*pv(101)
endif
if (9.le.mxd) then
 stop 'mg42: degree 9 not implemented'  !! Tried 050814, Magma at 20GB
endif
return
END SUBROUTINE mg42_secs