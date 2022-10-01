SUBROUTINE mg511_secs (mxd, r, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: v(0:)
!! Note: We stop at degree 7 for now.
!-----------------------------------------------------------------------
real (kind=wp) :: pv(0:size(v)-1) !! an upper bound size
if (size(r).ne.nk*nk.or.size(v).ne.mg511_nsc(mxd)) then
 stop 'mg511_secs: bad dimensions'
endif
call mg511_isecs (mxd, r, pv)
v(0) = 1
if (2.le.mxd) then
 v(1:3) = pv(0:2)
endif
if (3.le.mxd) then
 v(4:19) = pv(3:18)
endif
if (4.le.mxd) then
 v(20) = pv(0)*pv(0)
 v(21) = pv(0)*pv(1)
 v(22) = pv(1)*pv(1)
 v(23) = pv(0)*pv(2)
 v(24) = pv(1)*pv(2)
 v(25) = pv(2)*pv(2)
 v(26:73) = pv(19:66)
endif
if (5.le.mxd) then
 v(74) = pv(0)*pv(3)
 v(75) = pv(1)*pv(3)
 v(76) = pv(2)*pv(3)
 v(77) = pv(0)*pv(4)
 v(78) = pv(1)*pv(4)
 v(79) = pv(2)*pv(4)
 v(80) = pv(0)*pv(5)
 v(81) = pv(1)*pv(5)
 v(82) = pv(2)*pv(5)
 v(83) = pv(0)*pv(6)
 v(84) = pv(1)*pv(6)
 v(85) = pv(2)*pv(6)
 v(86) = pv(0)*pv(7)
 v(87) = pv(1)*pv(7)
 v(88) = pv(2)*pv(7)
 v(89) = pv(0)*pv(8)
 v(90) = pv(1)*pv(8)
 v(91) = pv(2)*pv(8)
 v(92) = pv(0)*pv(9)
 v(93) = pv(1)*pv(9)
 v(94) = pv(2)*pv(9)
 v(95) = pv(0)*pv(10)
 v(96) = pv(1)*pv(10)
 v(97) = pv(2)*pv(10)
 v(98) = pv(0)*pv(11)
 v(99) = pv(1)*pv(11)
 v(100) = pv(2)*pv(11)
 v(101) = pv(0)*pv(12)
 v(102) = pv(1)*pv(12)
 v(103) = pv(2)*pv(12)
 v(104) = pv(0)*pv(13)
 v(105) = pv(1)*pv(13)
 v(106) = pv(2)*pv(13)
 v(107) = pv(0)*pv(14)
 v(108) = pv(1)*pv(14)
 v(109) = pv(2)*pv(14)
 v(110) = pv(0)*pv(15)
 v(111) = pv(1)*pv(15)
 v(112) = pv(2)*pv(15)
 v(113) = pv(0)*pv(16)
 v(114) = pv(1)*pv(16)
 v(115) = pv(2)*pv(16)
 v(116) = pv(0)*pv(17)
 v(117) = pv(1)*pv(17)
 v(118) = pv(2)*pv(17)
 v(119) = pv(0)*pv(18)
 v(120) = pv(1)*pv(18)
 v(121) = pv(2)*pv(18)
 v(122:238) = pv(67:183)
endif
if (6.le.mxd) then
 v(239) = pv(0)*pv(0)*pv(0)
 v(240) = pv(0)*pv(0)*pv(1)
 v(241) = pv(0)*pv(1)*pv(1)
 v(242) = pv(1)*pv(1)*pv(1)
 v(243) = pv(0)*pv(0)*pv(2)
 v(244) = pv(0)*pv(1)*pv(2)
 v(245) = pv(1)*pv(1)*pv(2)
 v(246) = pv(0)*pv(2)*pv(2)
 v(247) = pv(1)*pv(2)*pv(2)
 v(248) = pv(2)*pv(2)*pv(2)
 v(249) = pv(3)*pv(3)
 v(250) = pv(3)*pv(4)
 v(251) = pv(4)*pv(4)
 v(252) = pv(3)*pv(5)
 v(253) = pv(4)*pv(5)
 v(254) = pv(5)*pv(5)
 v(255) = pv(3)*pv(6)
 v(256) = pv(4)*pv(6)
 v(257) = pv(5)*pv(6)
 v(258) = pv(6)*pv(6)
 v(259) = pv(3)*pv(7)
 v(260) = pv(4)*pv(7)
 v(261) = pv(5)*pv(7)
 v(262) = pv(6)*pv(7)
 v(263) = pv(7)*pv(7)
 v(264) = pv(3)*pv(8)
 v(265) = pv(4)*pv(8)
 v(266) = pv(5)*pv(8)
 v(267) = pv(6)*pv(8)
 v(268) = pv(7)*pv(8)
 v(269) = pv(8)*pv(8)
 v(270) = pv(3)*pv(9)
 v(271) = pv(4)*pv(9)
 v(272) = pv(5)*pv(9)
 v(273) = pv(6)*pv(9)
 v(274) = pv(7)*pv(9)
 v(275) = pv(8)*pv(9)
 v(276) = pv(9)*pv(9)
 v(277) = pv(3)*pv(10)
 v(278) = pv(4)*pv(10)
 v(279) = pv(5)*pv(10)
 v(280) = pv(6)*pv(10)
 v(281) = pv(7)*pv(10)
 v(282) = pv(8)*pv(10)
 v(283) = pv(9)*pv(10)
 v(284) = pv(10)*pv(10)
 v(285) = pv(3)*pv(11)
 v(286) = pv(4)*pv(11)
 v(287) = pv(5)*pv(11)
 v(288) = pv(6)*pv(11)
 v(289) = pv(7)*pv(11)
 v(290) = pv(8)*pv(11)
 v(291) = pv(9)*pv(11)
 v(292) = pv(10)*pv(11)
 v(293) = pv(11)*pv(11)
 v(294) = pv(3)*pv(12)
 v(295) = pv(4)*pv(12)
 v(296) = pv(5)*pv(12)
 v(297) = pv(6)*pv(12)
 v(298) = pv(7)*pv(12)
 v(299) = pv(8)*pv(12)
 v(300) = pv(9)*pv(12)
 v(301) = pv(10)*pv(12)
 v(302) = pv(11)*pv(12)
 v(303) = pv(12)*pv(12)
 v(304) = pv(3)*pv(13)
 v(305) = pv(4)*pv(13)
 v(306) = pv(5)*pv(13)
 v(307) = pv(6)*pv(13)
 v(308) = pv(7)*pv(13)
 v(309) = pv(8)*pv(13)
 v(310) = pv(9)*pv(13)
 v(311) = pv(10)*pv(13)
 v(312) = pv(11)*pv(13)
 v(313) = pv(12)*pv(13)
 v(314) = pv(13)*pv(13)
 v(315) = pv(3)*pv(14)
 v(316) = pv(4)*pv(14)
 v(317) = pv(5)*pv(14)
 v(318) = pv(6)*pv(14)
 v(319) = pv(7)*pv(14)
 v(320) = pv(8)*pv(14)
 v(321) = pv(9)*pv(14)
 v(322) = pv(10)*pv(14)
 v(323) = pv(11)*pv(14)
 v(324) = pv(12)*pv(14)
 v(325) = pv(13)*pv(14)
 v(326) = pv(14)*pv(14)
 v(327) = pv(3)*pv(15)
 v(328) = pv(4)*pv(15)
 v(329) = pv(5)*pv(15)
 v(330) = pv(6)*pv(15)
 v(331) = pv(7)*pv(15)
 v(332) = pv(8)*pv(15)
 v(333) = pv(9)*pv(15)
 v(334) = pv(10)*pv(15)
 v(335) = pv(11)*pv(15)
 v(336) = pv(12)*pv(15)
 v(337) = pv(13)*pv(15)
 v(338) = pv(14)*pv(15)
 v(339) = pv(15)*pv(15)
 v(340) = pv(3)*pv(16)
 v(341) = pv(4)*pv(16)
 v(342) = pv(5)*pv(16)
 v(343) = pv(6)*pv(16)
 v(344) = pv(7)*pv(16)
 v(345) = pv(8)*pv(16)
 v(346) = pv(9)*pv(16)
 v(347) = pv(10)*pv(16)
 v(348) = pv(11)*pv(16)
 v(349) = pv(12)*pv(16)
 v(350) = pv(13)*pv(16)
 v(351) = pv(14)*pv(16)
 v(352) = pv(15)*pv(16)
 v(353) = pv(16)*pv(16)
 v(354) = pv(3)*pv(17)
 v(355) = pv(4)*pv(17)
 v(356) = pv(5)*pv(17)
 v(357) = pv(6)*pv(17)
 v(358) = pv(7)*pv(17)
 v(359) = pv(8)*pv(17)
 v(360) = pv(9)*pv(17)
 v(361) = pv(10)*pv(17)
 v(362) = pv(11)*pv(17)
 v(363) = pv(12)*pv(17)
 v(364) = pv(13)*pv(17)
 v(365) = pv(14)*pv(17)
 v(366) = pv(15)*pv(17)
 v(367) = pv(16)*pv(17)
 v(368) = pv(17)*pv(17)
 v(369) = pv(3)*pv(18)
 v(370) = pv(4)*pv(18)
 v(371) = pv(5)*pv(18)
 v(372) = pv(6)*pv(18)
 v(373) = pv(7)*pv(18)
 v(374) = pv(8)*pv(18)
 v(375) = pv(9)*pv(18)
 v(376) = pv(10)*pv(18)
 v(377) = pv(11)*pv(18)
 v(378) = pv(12)*pv(18)
 v(379) = pv(13)*pv(18)
 v(380) = pv(14)*pv(18)
 v(381) = pv(15)*pv(18)
 v(382) = pv(16)*pv(18)
 v(383) = pv(17)*pv(18)
 v(384) = pv(18)*pv(18)
 v(385) = pv(0)*pv(19)
 v(386) = pv(1)*pv(19)
 v(387) = pv(2)*pv(19)
 v(388) = pv(0)*pv(20)
 v(389) = pv(1)*pv(20)
 v(390) = pv(2)*pv(20)
 v(391) = pv(0)*pv(21)
 v(392) = pv(1)*pv(21)
 v(393) = pv(2)*pv(21)
 v(394) = pv(0)*pv(22)
 v(395) = pv(1)*pv(22)
 v(396) = pv(2)*pv(22)
 v(397) = pv(0)*pv(23)
 v(398) = pv(1)*pv(23)
 v(399) = pv(2)*pv(23)
 v(400) = pv(0)*pv(24)
 v(401) = pv(1)*pv(24)
 v(402) = pv(2)*pv(24)
 v(403) = pv(0)*pv(25)
 v(404) = pv(1)*pv(25)
 v(405) = pv(2)*pv(25)
 v(406) = pv(0)*pv(26)
 v(407) = pv(1)*pv(26)
 v(408) = pv(2)*pv(26)
 v(409) = pv(0)*pv(27)
 v(410) = pv(1)*pv(27)
 v(411) = pv(2)*pv(27)
 v(412) = pv(0)*pv(28)
 v(413) = pv(1)*pv(28)
 v(414) = pv(2)*pv(28)
 v(415) = pv(0)*pv(29)
 v(416) = pv(1)*pv(29)
 v(417) = pv(2)*pv(29)
 v(418) = pv(0)*pv(30)
 v(419) = pv(1)*pv(30)
 v(420) = pv(2)*pv(30)
 v(421) = pv(0)*pv(31)
 v(422) = pv(1)*pv(31)
 v(423) = pv(2)*pv(31)
 v(424) = pv(0)*pv(32)
 v(425) = pv(1)*pv(32)
 v(426) = pv(2)*pv(32)
 v(427) = pv(0)*pv(33)
 v(428) = pv(1)*pv(33)
 v(429) = pv(2)*pv(33)
 v(430) = pv(0)*pv(34)
 v(431) = pv(1)*pv(34)
 v(432) = pv(2)*pv(34)
 v(433) = pv(0)*pv(35)
 v(434) = pv(1)*pv(35)
 v(435) = pv(2)*pv(35)
 v(436) = pv(0)*pv(36)
 v(437) = pv(1)*pv(36)
 v(438) = pv(2)*pv(36)
 v(439) = pv(0)*pv(37)
 v(440) = pv(1)*pv(37)
 v(441) = pv(2)*pv(37)
 v(442) = pv(0)*pv(38)
 v(443) = pv(1)*pv(38)
 v(444) = pv(2)*pv(38)
 v(445) = pv(0)*pv(39)
 v(446) = pv(1)*pv(39)
 v(447) = pv(2)*pv(39)
 v(448) = pv(0)*pv(40)
 v(449) = pv(1)*pv(40)
 v(450) = pv(2)*pv(40)
 v(451) = pv(0)*pv(41)
 v(452) = pv(1)*pv(41)
 v(453) = pv(2)*pv(41)
 v(454) = pv(0)*pv(42)
 v(455) = pv(1)*pv(42)
 v(456) = pv(2)*pv(42)
 v(457) = pv(0)*pv(43)
 v(458) = pv(1)*pv(43)
 v(459) = pv(2)*pv(43)
 v(460) = pv(0)*pv(44)
 v(461) = pv(1)*pv(44)
 v(462) = pv(2)*pv(44)
 v(463) = pv(0)*pv(45)
 v(464) = pv(1)*pv(45)
 v(465) = pv(2)*pv(45)
 v(466) = pv(0)*pv(46)
 v(467) = pv(1)*pv(46)
 v(468) = pv(2)*pv(46)
 v(469) = pv(0)*pv(47)
 v(470) = pv(1)*pv(47)
 v(471) = pv(2)*pv(47)
 v(472) = pv(0)*pv(48)
 v(473) = pv(1)*pv(48)
 v(474) = pv(2)*pv(48)
 v(475) = pv(0)*pv(49)
 v(476) = pv(1)*pv(49)
 v(477) = pv(2)*pv(49)
 v(478) = pv(0)*pv(50)
 v(479) = pv(1)*pv(50)
 v(480) = pv(2)*pv(50)
 v(481) = pv(0)*pv(51)
 v(482) = pv(1)*pv(51)
 v(483) = pv(2)*pv(51)
 v(484) = pv(0)*pv(52)
 v(485) = pv(1)*pv(52)
 v(486) = pv(2)*pv(52)
 v(487) = pv(0)*pv(53)
 v(488) = pv(1)*pv(53)
 v(489) = pv(2)*pv(53)
 v(490) = pv(0)*pv(54)
 v(491) = pv(1)*pv(54)
 v(492) = pv(2)*pv(54)
 v(493) = pv(0)*pv(55)
 v(494) = pv(1)*pv(55)
 v(495) = pv(2)*pv(55)
 v(496) = pv(0)*pv(56)
 v(497) = pv(1)*pv(56)
 v(498) = pv(2)*pv(56)
 v(499) = pv(0)*pv(57)
 v(500) = pv(1)*pv(57)
 v(501) = pv(2)*pv(57)
 v(502) = pv(0)*pv(58)
 v(503) = pv(1)*pv(58)
 v(504) = pv(2)*pv(58)
 v(505) = pv(0)*pv(59)
 v(506) = pv(1)*pv(59)
 v(507) = pv(2)*pv(59)
 v(508) = pv(0)*pv(60)
 v(509) = pv(1)*pv(60)
 v(510) = pv(2)*pv(60)
 v(511) = pv(0)*pv(61)
 v(512) = pv(1)*pv(61)
 v(513) = pv(2)*pv(61)
 v(514) = pv(0)*pv(62)
 v(515) = pv(1)*pv(62)
 v(516) = pv(2)*pv(62)
 v(517) = pv(0)*pv(63)
 v(518) = pv(1)*pv(63)
 v(519) = pv(2)*pv(63)
 v(520) = pv(0)*pv(64)
 v(521) = pv(1)*pv(64)
 v(522) = pv(2)*pv(64)
 v(523) = pv(0)*pv(65)
 v(524) = pv(1)*pv(65)
 v(525) = pv(2)*pv(65)
 v(526) = pv(0)*pv(66)
 v(527) = pv(1)*pv(66)
 v(528) = pv(2)*pv(66)
 v(529:720) = pv(184:375)
endif
if (7.le.mxd) then
 v(721) = pv(0)*pv(0)*pv(14)
 v(722) = pv(0)*pv(2)*pv(14)
 v(723) = pv(0)*pv(0)*pv(17)
 v(724) = pv(0)*pv(2)*pv(17)
 v(725) = pv(0)*pv(2)*pv(18)
 v(726) = pv(1)*pv(2)*pv(18)
 v(727) = pv(2)*pv(2)*pv(18)
 v(728) = pv(3)*pv(19)
 v(729) = pv(4)*pv(19)
 v(730) = pv(5)*pv(19)
 v(731) = pv(6)*pv(19)
 v(732) = pv(7)*pv(19)
 v(733) = pv(10)*pv(19)
 v(734) = pv(11)*pv(19)
 v(735) = pv(12)*pv(19)
 v(736) = pv(3)*pv(20)
 v(737) = pv(4)*pv(20)
 v(738) = pv(5)*pv(20)
 v(739) = pv(6)*pv(20)
 v(740) = pv(7)*pv(20)
 v(741) = pv(8)*pv(20)
 v(742) = pv(9)*pv(20)
 v(743) = pv(10)*pv(20)
 v(744) = pv(11)*pv(20)
 v(745) = pv(12)*pv(20)
 v(746) = pv(13)*pv(20)
 v(747) = pv(15)*pv(20)
 v(748) = pv(16)*pv(20)
 v(749) = pv(18)*pv(20)
 v(750) = pv(3)*pv(21)
 v(751) = pv(4)*pv(21)
 v(752) = pv(5)*pv(21)
 v(753) = pv(6)*pv(21)
 v(754) = pv(7)*pv(21)
 v(755) = pv(8)*pv(21)
 v(756) = pv(9)*pv(21)
 v(757) = pv(10)*pv(21)
 v(758) = pv(11)*pv(21)
 v(759) = pv(12)*pv(21)
 v(760) = pv(13)*pv(21)
 v(761) = pv(14)*pv(21)
 v(762) = pv(15)*pv(21)
 v(763) = pv(16)*pv(21)
 v(764) = pv(17)*pv(21)
 v(765) = pv(18)*pv(21)
 v(766) = pv(3)*pv(22)
 v(767) = pv(4)*pv(22)
 v(768) = pv(5)*pv(22)
 v(769) = pv(6)*pv(22)
 v(770) = pv(7)*pv(22)
 v(771) = pv(8)*pv(22)
 v(772) = pv(9)*pv(22)
 v(773) = pv(10)*pv(22)
 v(774) = pv(11)*pv(22)
 v(775) = pv(12)*pv(22)
 v(776) = pv(13)*pv(22)
 v(777) = pv(14)*pv(22)
 v(778) = pv(15)*pv(22)
 v(779) = pv(16)*pv(22)
 v(780) = pv(17)*pv(22)
 v(781) = pv(18)*pv(22)
 v(782) = pv(3)*pv(23)
 v(783) = pv(4)*pv(23)
 v(784) = pv(5)*pv(23)
 v(785) = pv(6)*pv(23)
 v(786) = pv(7)*pv(23)
 v(787) = pv(8)*pv(23)
 v(788) = pv(9)*pv(23)
 v(789) = pv(10)*pv(23)
 v(790) = pv(11)*pv(23)
 v(791) = pv(12)*pv(23)
 v(792) = pv(13)*pv(23)
 v(793) = pv(14)*pv(23)
 v(794) = pv(15)*pv(23)
 v(795) = pv(16)*pv(23)
 v(796) = pv(17)*pv(23)
 v(797) = pv(18)*pv(23)
 v(798) = pv(3)*pv(24)
 v(799) = pv(4)*pv(24)
 v(800) = pv(5)*pv(24)
 v(801) = pv(6)*pv(24)
 v(802) = pv(7)*pv(24)
 v(803) = pv(9)*pv(24)
 v(804) = pv(10)*pv(24)
 v(805) = pv(11)*pv(24)
 v(806) = pv(12)*pv(24)
 v(807) = pv(13)*pv(24)
 v(808) = pv(14)*pv(24)
 v(809) = pv(15)*pv(24)
 v(810) = pv(16)*pv(24)
 v(811) = pv(17)*pv(24)
 v(812) = pv(18)*pv(24)
 v(813) = pv(3)*pv(25)
 v(814) = pv(4)*pv(25)
 v(815) = pv(5)*pv(25)
 v(816) = pv(6)*pv(25)
 v(817) = pv(7)*pv(25)
 v(818) = pv(8)*pv(25)
 v(819) = pv(9)*pv(25)
 v(820) = pv(10)*pv(25)
 v(821) = pv(11)*pv(25)
 v(822) = pv(12)*pv(25)
 v(823) = pv(13)*pv(25)
 v(824) = pv(14)*pv(25)
 v(825) = pv(15)*pv(25)
 v(826) = pv(16)*pv(25)
 v(827) = pv(17)*pv(25)
 v(828) = pv(18)*pv(25)
 v(829) = pv(3)*pv(26)
 v(830) = pv(4)*pv(26)
 v(831) = pv(5)*pv(26)
 v(832) = pv(6)*pv(26)
 v(833) = pv(7)*pv(26)
 v(834) = pv(8)*pv(26)
 v(835) = pv(9)*pv(26)
 v(836) = pv(10)*pv(26)
 v(837) = pv(11)*pv(26)
 v(838) = pv(12)*pv(26)
 v(839) = pv(13)*pv(26)
 v(840) = pv(14)*pv(26)
 v(841) = pv(15)*pv(26)
 v(842) = pv(16)*pv(26)
 v(843) = pv(17)*pv(26)
 v(844) = pv(18)*pv(26)
 v(845) = pv(3)*pv(27)
 v(846) = pv(4)*pv(27)
 v(847) = pv(5)*pv(27)
 v(848) = pv(6)*pv(27)
 v(849) = pv(7)*pv(27)
 v(850) = pv(8)*pv(27)
 v(851) = pv(9)*pv(27)
 v(852) = pv(10)*pv(27)
 v(853) = pv(11)*pv(27)
 v(854) = pv(12)*pv(27)
 v(855) = pv(13)*pv(27)
 v(856) = pv(14)*pv(27)
 v(857) = pv(15)*pv(27)
 v(858) = pv(16)*pv(27)
 v(859) = pv(17)*pv(27)
 v(860) = pv(18)*pv(27)
 v(861) = pv(3)*pv(28)
 v(862) = pv(4)*pv(28)
 v(863) = pv(5)*pv(28)
 v(864) = pv(6)*pv(28)
 v(865) = pv(7)*pv(28)
 v(866) = pv(8)*pv(28)
 v(867) = pv(9)*pv(28)
 v(868) = pv(10)*pv(28)
 v(869) = pv(11)*pv(28)
 v(870) = pv(12)*pv(28)
 v(871) = pv(13)*pv(28)
 v(872) = pv(14)*pv(28)
 v(873) = pv(15)*pv(28)
 v(874) = pv(16)*pv(28)
 v(875) = pv(17)*pv(28)
 v(876) = pv(18)*pv(28)
 v(877) = pv(3)*pv(29)
 v(878) = pv(4)*pv(29)
 v(879) = pv(5)*pv(29)
 v(880) = pv(6)*pv(29)
 v(881) = pv(7)*pv(29)
 v(882) = pv(8)*pv(29)
 v(883) = pv(9)*pv(29)
 v(884) = pv(10)*pv(29)
 v(885) = pv(11)*pv(29)
 v(886) = pv(12)*pv(29)
 v(887) = pv(13)*pv(29)
 v(888) = pv(14)*pv(29)
 v(889) = pv(15)*pv(29)
 v(890) = pv(16)*pv(29)
 v(891) = pv(17)*pv(29)
 v(892) = pv(18)*pv(29)
 v(893) = pv(3)*pv(30)
 v(894) = pv(4)*pv(30)
 v(895) = pv(5)*pv(30)
 v(896) = pv(6)*pv(30)
 v(897) = pv(7)*pv(30)
 v(898) = pv(8)*pv(30)
 v(899) = pv(9)*pv(30)
 v(900) = pv(10)*pv(30)
 v(901) = pv(11)*pv(30)
 v(902) = pv(12)*pv(30)
 v(903) = pv(13)*pv(30)
 v(904) = pv(14)*pv(30)
 v(905) = pv(15)*pv(30)
 v(906) = pv(16)*pv(30)
 v(907) = pv(17)*pv(30)
 v(908) = pv(18)*pv(30)
 v(909) = pv(3)*pv(31)
 v(910) = pv(4)*pv(31)
 v(911) = pv(5)*pv(31)
 v(912) = pv(6)*pv(31)
 v(913) = pv(7)*pv(31)
 v(914) = pv(8)*pv(31)
 v(915) = pv(9)*pv(31)
 v(916) = pv(10)*pv(31)
 v(917) = pv(11)*pv(31)
 v(918) = pv(12)*pv(31)
 v(919) = pv(13)*pv(31)
 v(920) = pv(15)*pv(31)
 v(921) = pv(16)*pv(31)
 v(922) = pv(17)*pv(31)
 v(923) = pv(18)*pv(31)
 v(924) = pv(3)*pv(32)
 v(925) = pv(4)*pv(32)
 v(926) = pv(5)*pv(32)
 v(927) = pv(6)*pv(32)
 v(928) = pv(7)*pv(32)
 v(929) = pv(8)*pv(32)
 v(930) = pv(9)*pv(32)
 v(931) = pv(10)*pv(32)
 v(932) = pv(11)*pv(32)
 v(933) = pv(12)*pv(32)
 v(934) = pv(13)*pv(32)
 v(935) = pv(14)*pv(32)
 v(936) = pv(15)*pv(32)
 v(937) = pv(16)*pv(32)
 v(938) = pv(17)*pv(32)
 v(939) = pv(18)*pv(32)
 v(940) = pv(3)*pv(33)
 v(941) = pv(4)*pv(33)
 v(942) = pv(5)*pv(33)
 v(943) = pv(6)*pv(33)
 v(944) = pv(7)*pv(33)
 v(945) = pv(8)*pv(33)
 v(946) = pv(9)*pv(33)
 v(947) = pv(10)*pv(33)
 v(948) = pv(11)*pv(33)
 v(949) = pv(12)*pv(33)
 v(950) = pv(13)*pv(33)
 v(951) = pv(14)*pv(33)
 v(952) = pv(15)*pv(33)
 v(953) = pv(16)*pv(33)
 v(954) = pv(17)*pv(33)
 v(955) = pv(18)*pv(33)
 v(956) = pv(3)*pv(34)
 v(957) = pv(4)*pv(34)
 v(958) = pv(5)*pv(34)
 v(959) = pv(6)*pv(34)
 v(960) = pv(7)*pv(34)
 v(961) = pv(9)*pv(34)
 v(962) = pv(10)*pv(34)
 v(963) = pv(11)*pv(34)
 v(964) = pv(12)*pv(34)
 v(965) = pv(13)*pv(34)
 v(966) = pv(15)*pv(34)
 v(967) = pv(16)*pv(34)
 v(968) = pv(18)*pv(34)
 v(969) = pv(3)*pv(35)
 v(970) = pv(4)*pv(35)
 v(971) = pv(5)*pv(35)
 v(972) = pv(6)*pv(35)
 v(973) = pv(7)*pv(35)
 v(974) = pv(8)*pv(35)
 v(975) = pv(9)*pv(35)
 v(976) = pv(10)*pv(35)
 v(977) = pv(11)*pv(35)
 v(978) = pv(12)*pv(35)
 v(979) = pv(13)*pv(35)
 v(980) = pv(14)*pv(35)
 v(981) = pv(15)*pv(35)
 v(982) = pv(16)*pv(35)
 v(983) = pv(17)*pv(35)
 v(984) = pv(18)*pv(35)
 v(985) = pv(3)*pv(36)
 v(986) = pv(4)*pv(36)
 v(987) = pv(5)*pv(36)
 v(988) = pv(6)*pv(36)
 v(989) = pv(7)*pv(36)
 v(990) = pv(8)*pv(36)
 v(991) = pv(9)*pv(36)
 v(992) = pv(10)*pv(36)
 v(993) = pv(11)*pv(36)
 v(994) = pv(12)*pv(36)
 v(995) = pv(13)*pv(36)
 v(996) = pv(14)*pv(36)
 v(997) = pv(15)*pv(36)
 v(998) = pv(16)*pv(36)
 v(999) = pv(17)*pv(36)
 v(1000) = pv(18)*pv(36)
 v(1001) = pv(3)*pv(37)
 v(1002) = pv(4)*pv(37)
 v(1003) = pv(5)*pv(37)
 v(1004) = pv(6)*pv(37)
 v(1005) = pv(7)*pv(37)
 v(1006) = pv(8)*pv(37)
 v(1007) = pv(9)*pv(37)
 v(1008) = pv(10)*pv(37)
 v(1009) = pv(11)*pv(37)
 v(1010) = pv(12)*pv(37)
 v(1011) = pv(13)*pv(37)
 v(1012) = pv(14)*pv(37)
 v(1013) = pv(15)*pv(37)
 v(1014) = pv(16)*pv(37)
 v(1015) = pv(17)*pv(37)
 v(1016) = pv(18)*pv(37)
 v(1017) = pv(3)*pv(38)
 v(1018) = pv(4)*pv(38)
 v(1019) = pv(5)*pv(38)
 v(1020) = pv(6)*pv(38)
 v(1021) = pv(7)*pv(38)
 v(1022) = pv(8)*pv(38)
 v(1023) = pv(9)*pv(38)
 v(1024) = pv(10)*pv(38)
 v(1025) = pv(11)*pv(38)
 v(1026) = pv(12)*pv(38)
 v(1027) = pv(13)*pv(38)
 v(1028) = pv(14)*pv(38)
 v(1029) = pv(15)*pv(38)
 v(1030) = pv(17)*pv(38)
 v(1031) = pv(18)*pv(38)
 v(1032) = pv(3)*pv(39)
 v(1033) = pv(4)*pv(39)
 v(1034) = pv(5)*pv(39)
 v(1035) = pv(6)*pv(39)
 v(1036) = pv(7)*pv(39)
 v(1037) = pv(8)*pv(39)
 v(1038) = pv(9)*pv(39)
 v(1039) = pv(10)*pv(39)
 v(1040) = pv(11)*pv(39)
 v(1041) = pv(12)*pv(39)
 v(1042) = pv(13)*pv(39)
 v(1043) = pv(14)*pv(39)
 v(1044) = pv(15)*pv(39)
 v(1045) = pv(16)*pv(39)
 v(1046) = pv(17)*pv(39)
 v(1047) = pv(18)*pv(39)
 v(1048) = pv(3)*pv(40)
 v(1049) = pv(4)*pv(40)
 v(1050) = pv(5)*pv(40)
 v(1051) = pv(6)*pv(40)
 v(1052) = pv(7)*pv(40)
 v(1053) = pv(8)*pv(40)
 v(1054) = pv(9)*pv(40)
 v(1055) = pv(10)*pv(40)
 v(1056) = pv(11)*pv(40)
 v(1057) = pv(12)*pv(40)
 v(1058) = pv(13)*pv(40)
 v(1059) = pv(14)*pv(40)
 v(1060) = pv(15)*pv(40)
 v(1061) = pv(16)*pv(40)
 v(1062) = pv(17)*pv(40)
 v(1063) = pv(18)*pv(40)
 v(1064) = pv(3)*pv(41)
 v(1065) = pv(4)*pv(41)
 v(1066) = pv(5)*pv(41)
 v(1067) = pv(6)*pv(41)
 v(1068) = pv(7)*pv(41)
 v(1069) = pv(8)*pv(41)
 v(1070) = pv(9)*pv(41)
 v(1071) = pv(10)*pv(41)
 v(1072) = pv(11)*pv(41)
 v(1073) = pv(12)*pv(41)
 v(1074) = pv(13)*pv(41)
 v(1075) = pv(14)*pv(41)
 v(1076) = pv(15)*pv(41)
 v(1077) = pv(16)*pv(41)
 v(1078) = pv(17)*pv(41)
 v(1079) = pv(18)*pv(41)
 v(1080) = pv(3)*pv(42)
 v(1081) = pv(4)*pv(42)
 v(1082) = pv(5)*pv(42)
 v(1083) = pv(6)*pv(42)
 v(1084) = pv(7)*pv(42)
 v(1085) = pv(8)*pv(42)
 v(1086) = pv(9)*pv(42)
 v(1087) = pv(10)*pv(42)
 v(1088) = pv(11)*pv(42)
 v(1089) = pv(12)*pv(42)
 v(1090) = pv(13)*pv(42)
 v(1091) = pv(14)*pv(42)
 v(1092) = pv(15)*pv(42)
 v(1093) = pv(16)*pv(42)
 v(1094) = pv(17)*pv(42)
 v(1095) = pv(18)*pv(42)
 v(1096) = pv(3)*pv(43)
 v(1097) = pv(4)*pv(43)
 v(1098) = pv(5)*pv(43)
 v(1099) = pv(6)*pv(43)
 v(1100) = pv(7)*pv(43)
 v(1101) = pv(8)*pv(43)
 v(1102) = pv(9)*pv(43)
 v(1103) = pv(10)*pv(43)
 v(1104) = pv(11)*pv(43)
 v(1105) = pv(12)*pv(43)
 v(1106) = pv(13)*pv(43)
 v(1107) = pv(14)*pv(43)
 v(1108) = pv(15)*pv(43)
 v(1109) = pv(16)*pv(43)
 v(1110) = pv(17)*pv(43)
 v(1111) = pv(18)*pv(43)
 v(1112) = pv(3)*pv(44)
 v(1113) = pv(4)*pv(44)
 v(1114) = pv(5)*pv(44)
 v(1115) = pv(6)*pv(44)
 v(1116) = pv(7)*pv(44)
 v(1117) = pv(8)*pv(44)
 v(1118) = pv(9)*pv(44)
 v(1119) = pv(10)*pv(44)
 v(1120) = pv(11)*pv(44)
 v(1121) = pv(12)*pv(44)
 v(1122) = pv(13)*pv(44)
 v(1123) = pv(14)*pv(44)
 v(1124) = pv(15)*pv(44)
 v(1125) = pv(16)*pv(44)
 v(1126) = pv(17)*pv(44)
 v(1127) = pv(18)*pv(44)
 v(1128) = pv(3)*pv(45)
 v(1129) = pv(4)*pv(45)
 v(1130) = pv(5)*pv(45)
 v(1131) = pv(6)*pv(45)
 v(1132) = pv(7)*pv(45)
 v(1133) = pv(8)*pv(45)
 v(1134) = pv(9)*pv(45)
 v(1135) = pv(10)*pv(45)
 v(1136) = pv(11)*pv(45)
 v(1137) = pv(12)*pv(45)
 v(1138) = pv(13)*pv(45)
 v(1139) = pv(14)*pv(45)
 v(1140) = pv(15)*pv(45)
 v(1141) = pv(16)*pv(45)
 v(1142) = pv(17)*pv(45)
 v(1143) = pv(18)*pv(45)
 v(1144) = pv(3)*pv(46)
 v(1145) = pv(4)*pv(46)
 v(1146) = pv(5)*pv(46)
 v(1147) = pv(6)*pv(46)
 v(1148) = pv(7)*pv(46)
 v(1149) = pv(8)*pv(46)
 v(1150) = pv(9)*pv(46)
 v(1151) = pv(10)*pv(46)
 v(1152) = pv(11)*pv(46)
 v(1153) = pv(12)*pv(46)
 v(1154) = pv(13)*pv(46)
 v(1155) = pv(14)*pv(46)
 v(1156) = pv(15)*pv(46)
 v(1157) = pv(16)*pv(46)
 v(1158) = pv(17)*pv(46)
 v(1159) = pv(18)*pv(46)
 v(1160) = pv(3)*pv(47)
 v(1161) = pv(4)*pv(47)
 v(1162) = pv(5)*pv(47)
 v(1163) = pv(6)*pv(47)
 v(1164) = pv(7)*pv(47)
 v(1165) = pv(8)*pv(47)
 v(1166) = pv(9)*pv(47)
 v(1167) = pv(10)*pv(47)
 v(1168) = pv(11)*pv(47)
 v(1169) = pv(12)*pv(47)
 v(1170) = pv(13)*pv(47)
 v(1171) = pv(14)*pv(47)
 v(1172) = pv(15)*pv(47)
 v(1173) = pv(16)*pv(47)
 v(1174) = pv(17)*pv(47)
 v(1175) = pv(18)*pv(47)
 v(1176) = pv(3)*pv(48)
 v(1177) = pv(4)*pv(48)
 v(1178) = pv(5)*pv(48)
 v(1179) = pv(6)*pv(48)
 v(1180) = pv(7)*pv(48)
 v(1181) = pv(8)*pv(48)
 v(1182) = pv(9)*pv(48)
 v(1183) = pv(10)*pv(48)
 v(1184) = pv(11)*pv(48)
 v(1185) = pv(12)*pv(48)
 v(1186) = pv(13)*pv(48)
 v(1187) = pv(14)*pv(48)
 v(1188) = pv(15)*pv(48)
 v(1189) = pv(16)*pv(48)
 v(1190) = pv(18)*pv(48)
 v(1191) = pv(3)*pv(49)
 v(1192) = pv(4)*pv(49)
 v(1193) = pv(5)*pv(49)
 v(1194) = pv(6)*pv(49)
 v(1195) = pv(7)*pv(49)
 v(1196) = pv(8)*pv(49)
 v(1197) = pv(9)*pv(49)
 v(1198) = pv(10)*pv(49)
 v(1199) = pv(11)*pv(49)
 v(1200) = pv(12)*pv(49)
 v(1201) = pv(13)*pv(49)
 v(1202) = pv(15)*pv(49)
 v(1203) = pv(16)*pv(49)
 v(1204) = pv(17)*pv(49)
 v(1205) = pv(18)*pv(49)
 v(1206) = pv(3)*pv(50)
 v(1207) = pv(4)*pv(50)
 v(1208) = pv(5)*pv(50)
 v(1209) = pv(6)*pv(50)
 v(1210) = pv(7)*pv(50)
 v(1211) = pv(8)*pv(50)
 v(1212) = pv(9)*pv(50)
 v(1213) = pv(10)*pv(50)
 v(1214) = pv(11)*pv(50)
 v(1215) = pv(12)*pv(50)
 v(1216) = pv(13)*pv(50)
 v(1217) = pv(14)*pv(50)
 v(1218) = pv(15)*pv(50)
 v(1219) = pv(16)*pv(50)
 v(1220) = pv(17)*pv(50)
 v(1221) = pv(18)*pv(50)
 v(1222) = pv(3)*pv(51)
 v(1223) = pv(4)*pv(51)
 v(1224) = pv(5)*pv(51)
 v(1225) = pv(6)*pv(51)
 v(1226) = pv(7)*pv(51)
 v(1227) = pv(8)*pv(51)
 v(1228) = pv(9)*pv(51)
 v(1229) = pv(10)*pv(51)
 v(1230) = pv(11)*pv(51)
 v(1231) = pv(12)*pv(51)
 v(1232) = pv(13)*pv(51)
 v(1233) = pv(14)*pv(51)
 v(1234) = pv(15)*pv(51)
 v(1235) = pv(16)*pv(51)
 v(1236) = pv(17)*pv(51)
 v(1237) = pv(18)*pv(51)
 v(1238) = pv(3)*pv(52)
 v(1239) = pv(4)*pv(52)
 v(1240) = pv(5)*pv(52)
 v(1241) = pv(6)*pv(52)
 v(1242) = pv(7)*pv(52)
 v(1243) = pv(8)*pv(52)
 v(1244) = pv(9)*pv(52)
 v(1245) = pv(10)*pv(52)
 v(1246) = pv(11)*pv(52)
 v(1247) = pv(12)*pv(52)
 v(1248) = pv(13)*pv(52)
 v(1249) = pv(14)*pv(52)
 v(1250) = pv(15)*pv(52)
 v(1251) = pv(16)*pv(52)
 v(1252) = pv(17)*pv(52)
 v(1253) = pv(18)*pv(52)
 v(1254) = pv(3)*pv(53)
 v(1255) = pv(4)*pv(53)
 v(1256) = pv(5)*pv(53)
 v(1257) = pv(6)*pv(53)
 v(1258) = pv(7)*pv(53)
 v(1259) = pv(8)*pv(53)
 v(1260) = pv(9)*pv(53)
 v(1261) = pv(10)*pv(53)
 v(1262) = pv(11)*pv(53)
 v(1263) = pv(12)*pv(53)
 v(1264) = pv(13)*pv(53)
 v(1265) = pv(14)*pv(53)
 v(1266) = pv(15)*pv(53)
 v(1267) = pv(16)*pv(53)
 v(1268) = pv(17)*pv(53)
 v(1269) = pv(18)*pv(53)
 v(1270) = pv(3)*pv(54)
 v(1271) = pv(4)*pv(54)
 v(1272) = pv(5)*pv(54)
 v(1273) = pv(6)*pv(54)
 v(1274) = pv(7)*pv(54)
 v(1275) = pv(8)*pv(54)
 v(1276) = pv(9)*pv(54)
 v(1277) = pv(10)*pv(54)
 v(1278) = pv(11)*pv(54)
 v(1279) = pv(12)*pv(54)
 v(1280) = pv(13)*pv(54)
 v(1281) = pv(14)*pv(54)
 v(1282) = pv(15)*pv(54)
 v(1283) = pv(16)*pv(54)
 v(1284) = pv(17)*pv(54)
 v(1285) = pv(18)*pv(54)
 v(1286) = pv(3)*pv(55)
 v(1287) = pv(4)*pv(55)
 v(1288) = pv(5)*pv(55)
 v(1289) = pv(6)*pv(55)
 v(1290) = pv(7)*pv(55)
 v(1291) = pv(8)*pv(55)
 v(1292) = pv(9)*pv(55)
 v(1293) = pv(10)*pv(55)
 v(1294) = pv(11)*pv(55)
 v(1295) = pv(12)*pv(55)
 v(1296) = pv(13)*pv(55)
 v(1297) = pv(14)*pv(55)
 v(1298) = pv(15)*pv(55)
 v(1299) = pv(16)*pv(55)
 v(1300) = pv(18)*pv(55)
 v(1301) = pv(3)*pv(56)
 v(1302) = pv(4)*pv(56)
 v(1303) = pv(5)*pv(56)
 v(1304) = pv(6)*pv(56)
 v(1305) = pv(7)*pv(56)
 v(1306) = pv(8)*pv(56)
 v(1307) = pv(9)*pv(56)
 v(1308) = pv(10)*pv(56)
 v(1309) = pv(11)*pv(56)
 v(1310) = pv(12)*pv(56)
 v(1311) = pv(13)*pv(56)
 v(1312) = pv(14)*pv(56)
 v(1313) = pv(15)*pv(56)
 v(1314) = pv(16)*pv(56)
 v(1315) = pv(17)*pv(56)
 v(1316) = pv(18)*pv(56)
 v(1317) = pv(3)*pv(57)
 v(1318) = pv(4)*pv(57)
 v(1319) = pv(5)*pv(57)
 v(1320) = pv(6)*pv(57)
 v(1321) = pv(7)*pv(57)
 v(1322) = pv(8)*pv(57)
 v(1323) = pv(9)*pv(57)
 v(1324) = pv(10)*pv(57)
 v(1325) = pv(11)*pv(57)
 v(1326) = pv(12)*pv(57)
 v(1327) = pv(13)*pv(57)
 v(1328) = pv(14)*pv(57)
 v(1329) = pv(15)*pv(57)
 v(1330) = pv(16)*pv(57)
 v(1331) = pv(17)*pv(57)
 v(1332) = pv(18)*pv(57)
 v(1333) = pv(3)*pv(58)
 v(1334) = pv(4)*pv(58)
 v(1335) = pv(5)*pv(58)
 v(1336) = pv(6)*pv(58)
 v(1337) = pv(7)*pv(58)
 v(1338) = pv(8)*pv(58)
 v(1339) = pv(9)*pv(58)
 v(1340) = pv(10)*pv(58)
 v(1341) = pv(11)*pv(58)
 v(1342) = pv(12)*pv(58)
 v(1343) = pv(13)*pv(58)
 v(1344) = pv(14)*pv(58)
 v(1345) = pv(15)*pv(58)
 v(1346) = pv(16)*pv(58)
 v(1347) = pv(18)*pv(58)
 v(1348) = pv(3)*pv(59)
 v(1349) = pv(4)*pv(59)
 v(1350) = pv(5)*pv(59)
 v(1351) = pv(6)*pv(59)
 v(1352) = pv(7)*pv(59)
 v(1353) = pv(8)*pv(59)
 v(1354) = pv(9)*pv(59)
 v(1355) = pv(10)*pv(59)
 v(1356) = pv(11)*pv(59)
 v(1357) = pv(12)*pv(59)
 v(1358) = pv(13)*pv(59)
 v(1359) = pv(14)*pv(59)
 v(1360) = pv(15)*pv(59)
 v(1361) = pv(16)*pv(59)
 v(1362) = pv(17)*pv(59)
 v(1363) = pv(18)*pv(59)
 v(1364) = pv(3)*pv(60)
 v(1365) = pv(4)*pv(60)
 v(1366) = pv(5)*pv(60)
 v(1367) = pv(6)*pv(60)
 v(1368) = pv(7)*pv(60)
 v(1369) = pv(8)*pv(60)
 v(1370) = pv(9)*pv(60)
 v(1371) = pv(10)*pv(60)
 v(1372) = pv(11)*pv(60)
 v(1373) = pv(12)*pv(60)
 v(1374) = pv(13)*pv(60)
 v(1375) = pv(14)*pv(60)
 v(1376) = pv(15)*pv(60)
 v(1377) = pv(16)*pv(60)
 v(1378) = pv(17)*pv(60)
 v(1379) = pv(18)*pv(60)
 v(1380) = pv(3)*pv(61)
 v(1381) = pv(4)*pv(61)
 v(1382) = pv(5)*pv(61)
 v(1383) = pv(6)*pv(61)
 v(1384) = pv(7)*pv(61)
 v(1385) = pv(8)*pv(61)
 v(1386) = pv(9)*pv(61)
 v(1387) = pv(10)*pv(61)
 v(1388) = pv(11)*pv(61)
 v(1389) = pv(12)*pv(61)
 v(1390) = pv(13)*pv(61)
 v(1391) = pv(14)*pv(61)
 v(1392) = pv(15)*pv(61)
 v(1393) = pv(18)*pv(61)
 v(1394) = pv(3)*pv(62)
 v(1395) = pv(4)*pv(62)
 v(1396) = pv(5)*pv(62)
 v(1397) = pv(6)*pv(62)
 v(1398) = pv(7)*pv(62)
 v(1399) = pv(8)*pv(62)
 v(1400) = pv(9)*pv(62)
 v(1401) = pv(10)*pv(62)
 v(1402) = pv(11)*pv(62)
 v(1403) = pv(12)*pv(62)
 v(1404) = pv(13)*pv(62)
 v(1405) = pv(14)*pv(62)
 v(1406) = pv(15)*pv(62)
 v(1407) = pv(16)*pv(62)
 v(1408) = pv(18)*pv(62)
 v(1409) = pv(3)*pv(63)
 v(1410) = pv(4)*pv(63)
 v(1411) = pv(5)*pv(63)
 v(1412) = pv(6)*pv(63)
 v(1413) = pv(7)*pv(63)
 v(1414) = pv(8)*pv(63)
 v(1415) = pv(9)*pv(63)
 v(1416) = pv(10)*pv(63)
 v(1417) = pv(11)*pv(63)
 v(1418) = pv(12)*pv(63)
 v(1419) = pv(13)*pv(63)
 v(1420) = pv(14)*pv(63)
 v(1421) = pv(15)*pv(63)
 v(1422) = pv(16)*pv(63)
 v(1423) = pv(17)*pv(63)
 v(1424) = pv(18)*pv(63)
 v(1425) = pv(3)*pv(64)
 v(1426) = pv(4)*pv(64)
 v(1427) = pv(5)*pv(64)
 v(1428) = pv(6)*pv(64)
 v(1429) = pv(7)*pv(64)
 v(1430) = pv(8)*pv(64)
 v(1431) = pv(9)*pv(64)
 v(1432) = pv(10)*pv(64)
 v(1433) = pv(11)*pv(64)
 v(1434) = pv(12)*pv(64)
 v(1435) = pv(13)*pv(64)
 v(1436) = pv(14)*pv(64)
 v(1437) = pv(15)*pv(64)
 v(1438) = pv(16)*pv(64)
 v(1439) = pv(17)*pv(64)
 v(1440) = pv(18)*pv(64)
 v(1441) = pv(3)*pv(65)
 v(1442) = pv(4)*pv(65)
 v(1443) = pv(5)*pv(65)
 v(1444) = pv(6)*pv(65)
 v(1445) = pv(7)*pv(65)
 v(1446) = pv(8)*pv(65)
 v(1447) = pv(9)*pv(65)
 v(1448) = pv(10)*pv(65)
 v(1449) = pv(11)*pv(65)
 v(1450) = pv(12)*pv(65)
 v(1451) = pv(13)*pv(65)
 v(1452) = pv(14)*pv(65)
 v(1453) = pv(15)*pv(65)
 v(1454) = pv(16)*pv(65)
 v(1455) = pv(17)*pv(65)
 v(1456) = pv(18)*pv(65)
 v(1457) = pv(3)*pv(66)
 v(1458) = pv(4)*pv(66)
 v(1459) = pv(5)*pv(66)
 v(1460) = pv(6)*pv(66)
 v(1461) = pv(7)*pv(66)
 v(1462) = pv(8)*pv(66)
 v(1463) = pv(9)*pv(66)
 v(1464) = pv(10)*pv(66)
 v(1465) = pv(11)*pv(66)
 v(1466) = pv(12)*pv(66)
 v(1467) = pv(13)*pv(66)
 v(1468) = pv(14)*pv(66)
 v(1469) = pv(15)*pv(66)
 v(1470) = pv(16)*pv(66)
 v(1471) = pv(17)*pv(66)
 v(1472) = pv(18)*pv(66)
 v(1473) = pv(0)*pv(67)
 v(1474) = pv(1)*pv(67)
 v(1475) = pv(2)*pv(67)
 v(1476) = pv(0)*pv(68)
 v(1477) = pv(1)*pv(68)
 v(1478) = pv(2)*pv(68)
 v(1479) = pv(0)*pv(69)
 v(1480) = pv(1)*pv(69)
 v(1481) = pv(2)*pv(69)
 v(1482) = pv(0)*pv(70)
 v(1483) = pv(1)*pv(70)
 v(1484) = pv(2)*pv(70)
 v(1485) = pv(0)*pv(71)
 v(1486) = pv(1)*pv(71)
 v(1487) = pv(2)*pv(71)
 v(1488) = pv(0)*pv(72)
 v(1489) = pv(1)*pv(72)
 v(1490) = pv(2)*pv(72)
 v(1491) = pv(0)*pv(73)
 v(1492) = pv(1)*pv(73)
 v(1493) = pv(2)*pv(73)
 v(1494) = pv(0)*pv(74)
 v(1495) = pv(1)*pv(74)
 v(1496) = pv(2)*pv(74)
 v(1497) = pv(0)*pv(75)
 v(1498) = pv(1)*pv(75)
 v(1499) = pv(2)*pv(75)
 v(1500) = pv(0)*pv(76)
 v(1501) = pv(1)*pv(76)
 v(1502) = pv(2)*pv(76)
 v(1503) = pv(0)*pv(77)
 v(1504) = pv(1)*pv(77)
 v(1505) = pv(2)*pv(77)
 v(1506) = pv(0)*pv(78)
 v(1507) = pv(1)*pv(78)
 v(1508) = pv(2)*pv(78)
 v(1509) = pv(0)*pv(79)
 v(1510) = pv(1)*pv(79)
 v(1511) = pv(2)*pv(79)
 v(1512) = pv(0)*pv(80)
 v(1513) = pv(1)*pv(80)
 v(1514) = pv(2)*pv(80)
 v(1515) = pv(0)*pv(81)
 v(1516) = pv(1)*pv(81)
 v(1517) = pv(2)*pv(81)
 v(1518) = pv(0)*pv(82)
 v(1519) = pv(1)*pv(82)
 v(1520) = pv(2)*pv(82)
 v(1521) = pv(0)*pv(83)
 v(1522) = pv(1)*pv(83)
 v(1523) = pv(2)*pv(83)
 v(1524) = pv(0)*pv(84)
 v(1525) = pv(1)*pv(84)
 v(1526) = pv(2)*pv(84)
 v(1527) = pv(0)*pv(85)
 v(1528) = pv(1)*pv(85)
 v(1529) = pv(2)*pv(85)
 v(1530) = pv(0)*pv(86)
 v(1531) = pv(1)*pv(86)
 v(1532) = pv(2)*pv(86)
 v(1533) = pv(0)*pv(87)
 v(1534) = pv(1)*pv(87)
 v(1535) = pv(2)*pv(87)
 v(1536) = pv(0)*pv(88)
 v(1537) = pv(1)*pv(88)
 v(1538) = pv(2)*pv(88)
 v(1539) = pv(0)*pv(89)
 v(1540) = pv(1)*pv(89)
 v(1541) = pv(2)*pv(89)
 v(1542) = pv(0)*pv(90)
 v(1543) = pv(1)*pv(90)
 v(1544) = pv(2)*pv(90)
 v(1545) = pv(0)*pv(91)
 v(1546) = pv(1)*pv(91)
 v(1547) = pv(2)*pv(91)
 v(1548) = pv(0)*pv(92)
 v(1549) = pv(1)*pv(92)
 v(1550) = pv(2)*pv(92)
 v(1551) = pv(0)*pv(93)
 v(1552) = pv(1)*pv(93)
 v(1553) = pv(2)*pv(93)
 v(1554) = pv(0)*pv(94)
 v(1555) = pv(1)*pv(94)
 v(1556) = pv(2)*pv(94)
 v(1557) = pv(0)*pv(95)
 v(1558) = pv(1)*pv(95)
 v(1559) = pv(2)*pv(95)
 v(1560) = pv(0)*pv(96)
 v(1561) = pv(1)*pv(96)
 v(1562) = pv(2)*pv(96)
 v(1563) = pv(0)*pv(97)
 v(1564) = pv(1)*pv(97)
 v(1565) = pv(2)*pv(97)
 v(1566) = pv(0)*pv(98)
 v(1567) = pv(1)*pv(98)
 v(1568) = pv(2)*pv(98)
 v(1569) = pv(0)*pv(99)
 v(1570) = pv(1)*pv(99)
 v(1571) = pv(2)*pv(99)
 v(1572) = pv(0)*pv(100)
 v(1573) = pv(1)*pv(100)
 v(1574) = pv(2)*pv(100)
 v(1575) = pv(0)*pv(101)
 v(1576) = pv(1)*pv(101)
 v(1577) = pv(2)*pv(101)
 v(1578) = pv(0)*pv(102)
 v(1579) = pv(1)*pv(102)
 v(1580) = pv(2)*pv(102)
 v(1581) = pv(0)*pv(103)
 v(1582) = pv(1)*pv(103)
 v(1583) = pv(2)*pv(103)
 v(1584) = pv(0)*pv(104)
 v(1585) = pv(1)*pv(104)
 v(1586) = pv(2)*pv(104)
 v(1587) = pv(0)*pv(105)
 v(1588) = pv(1)*pv(105)
 v(1589) = pv(2)*pv(105)
 v(1590) = pv(0)*pv(106)
 v(1591) = pv(1)*pv(106)
 v(1592) = pv(2)*pv(106)
 v(1593) = pv(0)*pv(107)
 v(1594) = pv(1)*pv(107)
 v(1595) = pv(2)*pv(107)
 v(1596) = pv(0)*pv(108)
 v(1597) = pv(1)*pv(108)
 v(1598) = pv(2)*pv(108)
 v(1599) = pv(0)*pv(109)
 v(1600) = pv(1)*pv(109)
 v(1601) = pv(2)*pv(109)
 v(1602) = pv(0)*pv(110)
 v(1603) = pv(1)*pv(110)
 v(1604) = pv(2)*pv(110)
 v(1605) = pv(0)*pv(111)
 v(1606) = pv(1)*pv(111)
 v(1607) = pv(2)*pv(111)
 v(1608) = pv(0)*pv(112)
 v(1609) = pv(1)*pv(112)
 v(1610) = pv(2)*pv(112)
 v(1611) = pv(0)*pv(113)
 v(1612) = pv(1)*pv(113)
 v(1613) = pv(2)*pv(113)
 v(1614) = pv(0)*pv(114)
 v(1615) = pv(1)*pv(114)
 v(1616) = pv(2)*pv(114)
 v(1617) = pv(0)*pv(115)
 v(1618) = pv(1)*pv(115)
 v(1619) = pv(2)*pv(115)
 v(1620) = pv(0)*pv(116)
 v(1621) = pv(1)*pv(116)
 v(1622) = pv(2)*pv(116)
 v(1623) = pv(0)*pv(117)
 v(1624) = pv(1)*pv(117)
 v(1625) = pv(2)*pv(117)
 v(1626) = pv(0)*pv(118)
 v(1627) = pv(1)*pv(118)
 v(1628) = pv(2)*pv(118)
 v(1629) = pv(0)*pv(119)
 v(1630) = pv(1)*pv(119)
 v(1631) = pv(2)*pv(119)
 v(1632) = pv(0)*pv(120)
 v(1633) = pv(1)*pv(120)
 v(1634) = pv(2)*pv(120)
 v(1635) = pv(0)*pv(121)
 v(1636) = pv(1)*pv(121)
 v(1637) = pv(2)*pv(121)
 v(1638) = pv(0)*pv(122)
 v(1639) = pv(1)*pv(122)
 v(1640) = pv(2)*pv(122)
 v(1641) = pv(0)*pv(123)
 v(1642) = pv(1)*pv(123)
 v(1643) = pv(2)*pv(123)
 v(1644) = pv(0)*pv(124)
 v(1645) = pv(1)*pv(124)
 v(1646) = pv(2)*pv(124)
 v(1647) = pv(0)*pv(125)
 v(1648) = pv(1)*pv(125)
 v(1649) = pv(2)*pv(125)
 v(1650) = pv(0)*pv(126)
 v(1651) = pv(1)*pv(126)
 v(1652) = pv(2)*pv(126)
 v(1653) = pv(0)*pv(127)
 v(1654) = pv(1)*pv(127)
 v(1655) = pv(2)*pv(127)
 v(1656) = pv(0)*pv(128)
 v(1657) = pv(1)*pv(128)
 v(1658) = pv(2)*pv(128)
 v(1659) = pv(0)*pv(129)
 v(1660) = pv(1)*pv(129)
 v(1661) = pv(2)*pv(129)
 v(1662) = pv(0)*pv(130)
 v(1663) = pv(1)*pv(130)
 v(1664) = pv(2)*pv(130)
 v(1665) = pv(0)*pv(131)
 v(1666) = pv(1)*pv(131)
 v(1667) = pv(2)*pv(131)
 v(1668) = pv(0)*pv(132)
 v(1669) = pv(1)*pv(132)
 v(1670) = pv(2)*pv(132)
 v(1671) = pv(0)*pv(133)
 v(1672) = pv(1)*pv(133)
 v(1673) = pv(2)*pv(133)
 v(1674) = pv(0)*pv(134)
 v(1675) = pv(1)*pv(134)
 v(1676) = pv(2)*pv(134)
 v(1677) = pv(0)*pv(135)
 v(1678) = pv(1)*pv(135)
 v(1679) = pv(2)*pv(135)
 v(1680) = pv(0)*pv(136)
 v(1681) = pv(1)*pv(136)
 v(1682) = pv(2)*pv(136)
 v(1683) = pv(0)*pv(137)
 v(1684) = pv(1)*pv(137)
 v(1685) = pv(2)*pv(137)
 v(1686) = pv(0)*pv(138)
 v(1687) = pv(1)*pv(138)
 v(1688) = pv(2)*pv(138)
 v(1689) = pv(0)*pv(139)
 v(1690) = pv(1)*pv(139)
 v(1691) = pv(2)*pv(139)
 v(1692) = pv(0)*pv(140)
 v(1693) = pv(1)*pv(140)
 v(1694) = pv(2)*pv(140)
 v(1695) = pv(0)*pv(141)
 v(1696) = pv(1)*pv(141)
 v(1697) = pv(2)*pv(141)
 v(1698) = pv(0)*pv(142)
 v(1699) = pv(1)*pv(142)
 v(1700) = pv(2)*pv(142)
 v(1701) = pv(0)*pv(143)
 v(1702) = pv(1)*pv(143)
 v(1703) = pv(2)*pv(143)
 v(1704) = pv(0)*pv(144)
 v(1705) = pv(1)*pv(144)
 v(1706) = pv(2)*pv(144)
 v(1707) = pv(0)*pv(145)
 v(1708) = pv(1)*pv(145)
 v(1709) = pv(2)*pv(145)
 v(1710) = pv(0)*pv(146)
 v(1711) = pv(1)*pv(146)
 v(1712) = pv(2)*pv(146)
 v(1713) = pv(0)*pv(147)
 v(1714) = pv(1)*pv(147)
 v(1715) = pv(2)*pv(147)
 v(1716) = pv(0)*pv(148)
 v(1717) = pv(1)*pv(148)
 v(1718) = pv(2)*pv(148)
 v(1719) = pv(0)*pv(149)
 v(1720) = pv(1)*pv(149)
 v(1721) = pv(2)*pv(149)
 v(1722) = pv(0)*pv(150)
 v(1723) = pv(1)*pv(150)
 v(1724) = pv(2)*pv(150)
 v(1725) = pv(0)*pv(151)
 v(1726) = pv(1)*pv(151)
 v(1727) = pv(2)*pv(151)
 v(1728) = pv(0)*pv(152)
 v(1729) = pv(1)*pv(152)
 v(1730) = pv(2)*pv(152)
 v(1731) = pv(0)*pv(153)
 v(1732) = pv(1)*pv(153)
 v(1733) = pv(2)*pv(153)
 v(1734) = pv(0)*pv(154)
 v(1735) = pv(1)*pv(154)
 v(1736) = pv(2)*pv(154)
 v(1737) = pv(0)*pv(155)
 v(1738) = pv(1)*pv(155)
 v(1739) = pv(2)*pv(155)
 v(1740) = pv(0)*pv(156)
 v(1741) = pv(1)*pv(156)
 v(1742) = pv(2)*pv(156)
 v(1743) = pv(0)*pv(157)
 v(1744) = pv(1)*pv(157)
 v(1745) = pv(2)*pv(157)
 v(1746) = pv(0)*pv(158)
 v(1747) = pv(1)*pv(158)
 v(1748) = pv(2)*pv(158)
 v(1749) = pv(0)*pv(159)
 v(1750) = pv(1)*pv(159)
 v(1751) = pv(2)*pv(159)
 v(1752) = pv(0)*pv(160)
 v(1753) = pv(1)*pv(160)
 v(1754) = pv(2)*pv(160)
 v(1755) = pv(0)*pv(161)
 v(1756) = pv(1)*pv(161)
 v(1757) = pv(2)*pv(161)
 v(1758) = pv(0)*pv(162)
 v(1759) = pv(1)*pv(162)
 v(1760) = pv(2)*pv(162)
 v(1761) = pv(0)*pv(163)
 v(1762) = pv(1)*pv(163)
 v(1763) = pv(2)*pv(163)
 v(1764) = pv(0)*pv(164)
 v(1765) = pv(1)*pv(164)
 v(1766) = pv(2)*pv(164)
 v(1767) = pv(0)*pv(165)
 v(1768) = pv(1)*pv(165)
 v(1769) = pv(2)*pv(165)
 v(1770) = pv(0)*pv(166)
 v(1771) = pv(1)*pv(166)
 v(1772) = pv(2)*pv(166)
 v(1773) = pv(0)*pv(167)
 v(1774) = pv(1)*pv(167)
 v(1775) = pv(2)*pv(167)
 v(1776) = pv(0)*pv(168)
 v(1777) = pv(1)*pv(168)
 v(1778) = pv(2)*pv(168)
 v(1779) = pv(0)*pv(169)
 v(1780) = pv(1)*pv(169)
 v(1781) = pv(2)*pv(169)
 v(1782) = pv(0)*pv(170)
 v(1783) = pv(1)*pv(170)
 v(1784) = pv(2)*pv(170)
 v(1785) = pv(0)*pv(171)
 v(1786) = pv(1)*pv(171)
 v(1787) = pv(2)*pv(171)
 v(1788) = pv(0)*pv(172)
 v(1789) = pv(1)*pv(172)
 v(1790) = pv(2)*pv(172)
 v(1791) = pv(0)*pv(173)
 v(1792) = pv(1)*pv(173)
 v(1793) = pv(2)*pv(173)
 v(1794) = pv(0)*pv(174)
 v(1795) = pv(1)*pv(174)
 v(1796) = pv(2)*pv(174)
 v(1797) = pv(0)*pv(175)
 v(1798) = pv(1)*pv(175)
 v(1799) = pv(2)*pv(175)
 v(1800) = pv(0)*pv(176)
 v(1801) = pv(1)*pv(176)
 v(1802) = pv(2)*pv(176)
 v(1803) = pv(0)*pv(177)
 v(1804) = pv(1)*pv(177)
 v(1805) = pv(2)*pv(177)
 v(1806) = pv(0)*pv(178)
 v(1807) = pv(1)*pv(178)
 v(1808) = pv(2)*pv(178)
 v(1809) = pv(0)*pv(179)
 v(1810) = pv(1)*pv(179)
 v(1811) = pv(2)*pv(179)
 v(1812) = pv(0)*pv(180)
 v(1813) = pv(1)*pv(180)
 v(1814) = pv(2)*pv(180)
 v(1815) = pv(0)*pv(181)
 v(1816) = pv(1)*pv(181)
 v(1817) = pv(2)*pv(181)
 v(1818) = pv(0)*pv(182)
 v(1819) = pv(1)*pv(182)
 v(1820) = pv(2)*pv(182)
 v(1821) = pv(0)*pv(183)
 v(1822) = pv(1)*pv(183)
 v(1823) = pv(2)*pv(183)
 v(1824:2023) = pv(376:575)
endif
if (8.le.mxd) then
 stop 'mg511: degree 8 not implemented'
endif
return
END SUBROUTINE mg511_secs