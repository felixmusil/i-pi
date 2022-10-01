SUBROUTINE mg43_secs (mxd, r, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: v(0:)
!! Note: We stop at degree 7 for now.
!-----------------------------------------------------------------------
real (kind=wp) :: pv(0:size(v)-1) !! an upper bound size
if (size(r).ne.nk*nk.or.size(v).ne.mg43_nsc(mxd)) then
 stop 'mg43_secs: bad dimensions'
endif
call mg43_isecs (mxd, r, pv)
v(0) = 1
if (2.le.mxd) then
 v(1:2) = pv(0:1)
endif
if (3.le.mxd) then
 v(3:18) = pv(2:17)
endif
if (4.le.mxd) then
 v(19) = pv(0)*pv(0)
 v(20) = pv(0)*pv(1)
 v(21) = pv(1)*pv(1)
 v(22:69) = pv(18:65)
endif
if (5.le.mxd) then
 v(70) = pv(0)*pv(2)
 v(71) = pv(1)*pv(2)
 v(72) = pv(0)*pv(3)
 v(73) = pv(1)*pv(3)
 v(74) = pv(0)*pv(4)
 v(75) = pv(1)*pv(4)
 v(76) = pv(0)*pv(5)
 v(77) = pv(1)*pv(5)
 v(78) = pv(0)*pv(6)
 v(79) = pv(1)*pv(6)
 v(80) = pv(0)*pv(7)
 v(81) = pv(1)*pv(7)
 v(82) = pv(0)*pv(8)
 v(83) = pv(1)*pv(8)
 v(84) = pv(0)*pv(9)
 v(85) = pv(1)*pv(9)
 v(86) = pv(0)*pv(10)
 v(87) = pv(1)*pv(10)
 v(88) = pv(0)*pv(11)
 v(89) = pv(1)*pv(11)
 v(90) = pv(0)*pv(12)
 v(91) = pv(1)*pv(12)
 v(92) = pv(0)*pv(13)
 v(93) = pv(1)*pv(13)
 v(94) = pv(0)*pv(14)
 v(95) = pv(0)*pv(15)
 v(96) = pv(1)*pv(15)
 v(97) = pv(0)*pv(16)
 v(98) = pv(1)*pv(16)
 v(99) = pv(0)*pv(17)
 v(100:223) = pv(66:189)
endif
if (6.le.mxd) then
 v(224) = pv(0)*pv(0)*pv(1)
 v(225) = pv(2)*pv(2)
 v(226) = pv(2)*pv(3)
 v(227) = pv(2)*pv(4)
 v(228) = pv(2)*pv(5)
 v(229) = pv(4)*pv(5)
 v(230) = pv(2)*pv(6)
 v(231) = pv(3)*pv(6)
 v(232) = pv(4)*pv(6)
 v(233) = pv(5)*pv(6)
 v(234) = pv(6)*pv(6)
 v(235) = pv(2)*pv(7)
 v(236) = pv(3)*pv(7)
 v(237) = pv(4)*pv(7)
 v(238) = pv(5)*pv(7)
 v(239) = pv(6)*pv(7)
 v(240) = pv(7)*pv(7)
 v(241) = pv(2)*pv(8)
 v(242) = pv(3)*pv(8)
 v(243) = pv(4)*pv(8)
 v(244) = pv(5)*pv(8)
 v(245) = pv(6)*pv(8)
 v(246) = pv(7)*pv(8)
 v(247) = pv(8)*pv(8)
 v(248) = pv(2)*pv(9)
 v(249) = pv(3)*pv(9)
 v(250) = pv(4)*pv(9)
 v(251) = pv(5)*pv(9)
 v(252) = pv(6)*pv(9)
 v(253) = pv(7)*pv(9)
 v(254) = pv(8)*pv(9)
 v(255) = pv(9)*pv(9)
 v(256) = pv(2)*pv(10)
 v(257) = pv(3)*pv(10)
 v(258) = pv(4)*pv(10)
 v(259) = pv(5)*pv(10)
 v(260) = pv(6)*pv(10)
 v(261) = pv(7)*pv(10)
 v(262) = pv(8)*pv(10)
 v(263) = pv(9)*pv(10)
 v(264) = pv(10)*pv(10)
 v(265) = pv(2)*pv(11)
 v(266) = pv(3)*pv(11)
 v(267) = pv(4)*pv(11)
 v(268) = pv(5)*pv(11)
 v(269) = pv(6)*pv(11)
 v(270) = pv(7)*pv(11)
 v(271) = pv(8)*pv(11)
 v(272) = pv(9)*pv(11)
 v(273) = pv(10)*pv(11)
 v(274) = pv(11)*pv(11)
 v(275) = pv(2)*pv(12)
 v(276) = pv(3)*pv(12)
 v(277) = pv(4)*pv(12)
 v(278) = pv(5)*pv(12)
 v(279) = pv(6)*pv(12)
 v(280) = pv(7)*pv(12)
 v(281) = pv(8)*pv(12)
 v(282) = pv(9)*pv(12)
 v(283) = pv(10)*pv(12)
 v(284) = pv(11)*pv(12)
 v(285) = pv(12)*pv(12)
 v(286) = pv(2)*pv(13)
 v(287) = pv(3)*pv(13)
 v(288) = pv(4)*pv(13)
 v(289) = pv(5)*pv(13)
 v(290) = pv(6)*pv(13)
 v(291) = pv(7)*pv(13)
 v(292) = pv(8)*pv(13)
 v(293) = pv(9)*pv(13)
 v(294) = pv(10)*pv(13)
 v(295) = pv(11)*pv(13)
 v(296) = pv(12)*pv(13)
 v(297) = pv(13)*pv(13)
 v(298) = pv(2)*pv(14)
 v(299) = pv(3)*pv(14)
 v(300) = pv(4)*pv(14)
 v(301) = pv(6)*pv(14)
 v(302) = pv(7)*pv(14)
 v(303) = pv(9)*pv(14)
 v(304) = pv(11)*pv(14)
 v(305) = pv(12)*pv(14)
 v(306) = pv(2)*pv(15)
 v(307) = pv(3)*pv(15)
 v(308) = pv(4)*pv(15)
 v(309) = pv(5)*pv(15)
 v(310) = pv(6)*pv(15)
 v(311) = pv(7)*pv(15)
 v(312) = pv(8)*pv(15)
 v(313) = pv(9)*pv(15)
 v(314) = pv(10)*pv(15)
 v(315) = pv(11)*pv(15)
 v(316) = pv(12)*pv(15)
 v(317) = pv(13)*pv(15)
 v(318) = pv(15)*pv(15)
 v(319) = pv(2)*pv(16)
 v(320) = pv(3)*pv(16)
 v(321) = pv(4)*pv(16)
 v(322) = pv(5)*pv(16)
 v(323) = pv(6)*pv(16)
 v(324) = pv(7)*pv(16)
 v(325) = pv(8)*pv(16)
 v(326) = pv(9)*pv(16)
 v(327) = pv(10)*pv(16)
 v(328) = pv(11)*pv(16)
 v(329) = pv(12)*pv(16)
 v(330) = pv(13)*pv(16)
 v(331) = pv(15)*pv(16)
 v(332) = pv(16)*pv(16)
 v(333) = pv(2)*pv(17)
 v(334) = pv(3)*pv(17)
 v(335) = pv(4)*pv(17)
 v(336) = pv(5)*pv(17)
 v(337) = pv(6)*pv(17)
 v(338) = pv(7)*pv(17)
 v(339) = pv(8)*pv(17)
 v(340) = pv(9)*pv(17)
 v(341) = pv(10)*pv(17)
 v(342) = pv(11)*pv(17)
 v(343) = pv(12)*pv(17)
 v(344) = pv(15)*pv(17)
 v(345) = pv(0)*pv(18)
 v(346) = pv(1)*pv(18)
 v(347) = pv(0)*pv(19)
 v(348) = pv(1)*pv(19)
 v(349) = pv(0)*pv(20)
 v(350) = pv(1)*pv(20)
 v(351) = pv(0)*pv(21)
 v(352) = pv(1)*pv(21)
 v(353) = pv(0)*pv(22)
 v(354) = pv(1)*pv(22)
 v(355) = pv(0)*pv(23)
 v(356) = pv(1)*pv(23)
 v(357) = pv(0)*pv(24)
 v(358) = pv(1)*pv(24)
 v(359) = pv(0)*pv(25)
 v(360) = pv(1)*pv(25)
 v(361) = pv(0)*pv(26)
 v(362) = pv(1)*pv(26)
 v(363) = pv(0)*pv(27)
 v(364) = pv(1)*pv(27)
 v(365) = pv(0)*pv(28)
 v(366) = pv(1)*pv(28)
 v(367) = pv(0)*pv(29)
 v(368) = pv(1)*pv(29)
 v(369) = pv(0)*pv(30)
 v(370) = pv(1)*pv(30)
 v(371) = pv(0)*pv(31)
 v(372) = pv(1)*pv(31)
 v(373) = pv(0)*pv(32)
 v(374) = pv(1)*pv(32)
 v(375) = pv(0)*pv(33)
 v(376) = pv(1)*pv(33)
 v(377) = pv(0)*pv(34)
 v(378) = pv(1)*pv(34)
 v(379) = pv(0)*pv(35)
 v(380) = pv(1)*pv(35)
 v(381) = pv(0)*pv(36)
 v(382) = pv(1)*pv(36)
 v(383) = pv(0)*pv(37)
 v(384) = pv(1)*pv(37)
 v(385) = pv(0)*pv(38)
 v(386) = pv(1)*pv(38)
 v(387) = pv(0)*pv(39)
 v(388) = pv(1)*pv(39)
 v(389) = pv(0)*pv(40)
 v(390) = pv(1)*pv(40)
 v(391) = pv(0)*pv(41)
 v(392) = pv(1)*pv(41)
 v(393) = pv(0)*pv(42)
 v(394) = pv(1)*pv(42)
 v(395) = pv(0)*pv(43)
 v(396) = pv(1)*pv(43)
 v(397) = pv(0)*pv(44)
 v(398) = pv(1)*pv(44)
 v(399) = pv(0)*pv(45)
 v(400) = pv(1)*pv(45)
 v(401) = pv(0)*pv(46)
 v(402) = pv(1)*pv(46)
 v(403) = pv(0)*pv(47)
 v(404) = pv(1)*pv(47)
 v(405) = pv(0)*pv(48)
 v(406) = pv(1)*pv(48)
 v(407) = pv(0)*pv(49)
 v(408) = pv(1)*pv(49)
 v(409) = pv(0)*pv(50)
 v(410) = pv(1)*pv(50)
 v(411) = pv(0)*pv(51)
 v(412) = pv(1)*pv(51)
 v(413) = pv(0)*pv(52)
 v(414) = pv(1)*pv(52)
 v(415) = pv(0)*pv(53)
 v(416) = pv(1)*pv(53)
 v(417) = pv(0)*pv(54)
 v(418) = pv(0)*pv(55)
 v(419) = pv(1)*pv(55)
 v(420) = pv(0)*pv(56)
 v(421) = pv(1)*pv(56)
 v(422) = pv(0)*pv(57)
 v(423) = pv(1)*pv(57)
 v(424) = pv(0)*pv(58)
 v(425) = pv(1)*pv(58)
 v(426) = pv(0)*pv(59)
 v(427) = pv(0)*pv(60)
 v(428) = pv(1)*pv(60)
 v(429) = pv(0)*pv(61)
 v(430) = pv(1)*pv(61)
 v(431) = pv(0)*pv(62)
 v(432) = pv(1)*pv(62)
 v(433) = pv(0)*pv(63)
 v(434) = pv(1)*pv(63)
 v(435) = pv(0)*pv(64)
 v(436) = pv(1)*pv(64)
 v(437) = pv(0)*pv(65)
 v(438) = pv(1)*pv(65)
 v(439:680) = pv(190:431)
endif
if (7.le.mxd) then
 v(681) = pv(1)*pv(1)*pv(2)
 v(682) = pv(0)*pv(1)*pv(7)
 v(683) = pv(0)*pv(1)*pv(9)
 v(684) = pv(0)*pv(1)*pv(11)
 v(685) = pv(0)*pv(0)*pv(12)
 v(686) = pv(0)*pv(1)*pv(12)
 v(687) = pv(0)*pv(0)*pv(16)
 v(688) = pv(9)*pv(18)
 v(689) = pv(11)*pv(18)
 v(690) = pv(12)*pv(18)
 v(691) = pv(13)*pv(18)
 v(692) = pv(15)*pv(18)
 v(693) = pv(16)*pv(18)
 v(694) = pv(17)*pv(18)
 v(695) = pv(11)*pv(19)
 v(696) = pv(12)*pv(19)
 v(697) = pv(15)*pv(19)
 v(698) = pv(16)*pv(19)
 v(699) = pv(17)*pv(19)
 v(700) = pv(2)*pv(20)
 v(701) = pv(12)*pv(20)
 v(702) = pv(13)*pv(20)
 v(703) = pv(15)*pv(20)
 v(704) = pv(16)*pv(20)
 v(705) = pv(17)*pv(20)
 v(706) = pv(2)*pv(21)
 v(707) = pv(7)*pv(21)
 v(708) = pv(9)*pv(21)
 v(709) = pv(11)*pv(21)
 v(710) = pv(12)*pv(21)
 v(711) = pv(13)*pv(21)
 v(712) = pv(15)*pv(21)
 v(713) = pv(16)*pv(21)
 v(714) = pv(17)*pv(21)
 v(715) = pv(2)*pv(22)
 v(716) = pv(4)*pv(22)
 v(717) = pv(12)*pv(22)
 v(718) = pv(13)*pv(22)
 v(719) = pv(16)*pv(22)
 v(720) = pv(2)*pv(23)
 v(721) = pv(3)*pv(23)
 v(722) = pv(4)*pv(23)
 v(723) = pv(7)*pv(23)
 v(724) = pv(9)*pv(23)
 v(725) = pv(11)*pv(23)
 v(726) = pv(12)*pv(23)
 v(727) = pv(13)*pv(23)
 v(728) = pv(15)*pv(23)
 v(729) = pv(16)*pv(23)
 v(730) = pv(2)*pv(24)
 v(731) = pv(9)*pv(24)
 v(732) = pv(11)*pv(24)
 v(733) = pv(13)*pv(24)
 v(734) = pv(2)*pv(25)
 v(735) = pv(3)*pv(25)
 v(736) = pv(4)*pv(25)
 v(737) = pv(6)*pv(25)
 v(738) = pv(8)*pv(25)
 v(739) = pv(9)*pv(25)
 v(740) = pv(10)*pv(25)
 v(741) = pv(11)*pv(25)
 v(742) = pv(12)*pv(25)
 v(743) = pv(13)*pv(25)
 v(744) = pv(14)*pv(25)
 v(745) = pv(15)*pv(25)
 v(746) = pv(16)*pv(25)
 v(747) = pv(17)*pv(25)
 v(748) = pv(2)*pv(26)
 v(749) = pv(3)*pv(26)
 v(750) = pv(4)*pv(26)
 v(751) = pv(5)*pv(26)
 v(752) = pv(6)*pv(26)
 v(753) = pv(7)*pv(26)
 v(754) = pv(8)*pv(26)
 v(755) = pv(9)*pv(26)
 v(756) = pv(10)*pv(26)
 v(757) = pv(11)*pv(26)
 v(758) = pv(12)*pv(26)
 v(759) = pv(13)*pv(26)
 v(760) = pv(14)*pv(26)
 v(761) = pv(15)*pv(26)
 v(762) = pv(16)*pv(26)
 v(763) = pv(17)*pv(26)
 v(764) = pv(2)*pv(27)
 v(765) = pv(3)*pv(27)
 v(766) = pv(4)*pv(27)
 v(767) = pv(5)*pv(27)
 v(768) = pv(7)*pv(27)
 v(769) = pv(8)*pv(27)
 v(770) = pv(9)*pv(27)
 v(771) = pv(10)*pv(27)
 v(772) = pv(11)*pv(27)
 v(773) = pv(12)*pv(27)
 v(774) = pv(13)*pv(27)
 v(775) = pv(14)*pv(27)
 v(776) = pv(15)*pv(27)
 v(777) = pv(16)*pv(27)
 v(778) = pv(17)*pv(27)
 v(779) = pv(2)*pv(28)
 v(780) = pv(3)*pv(28)
 v(781) = pv(4)*pv(28)
 v(782) = pv(5)*pv(28)
 v(783) = pv(6)*pv(28)
 v(784) = pv(7)*pv(28)
 v(785) = pv(8)*pv(28)
 v(786) = pv(9)*pv(28)
 v(787) = pv(10)*pv(28)
 v(788) = pv(11)*pv(28)
 v(789) = pv(12)*pv(28)
 v(790) = pv(13)*pv(28)
 v(791) = pv(14)*pv(28)
 v(792) = pv(15)*pv(28)
 v(793) = pv(16)*pv(28)
 v(794) = pv(17)*pv(28)
 v(795) = pv(2)*pv(29)
 v(796) = pv(3)*pv(29)
 v(797) = pv(4)*pv(29)
 v(798) = pv(6)*pv(29)
 v(799) = pv(7)*pv(29)
 v(800) = pv(9)*pv(29)
 v(801) = pv(10)*pv(29)
 v(802) = pv(11)*pv(29)
 v(803) = pv(12)*pv(29)
 v(804) = pv(13)*pv(29)
 v(805) = pv(16)*pv(29)
 v(806) = pv(2)*pv(30)
 v(807) = pv(3)*pv(30)
 v(808) = pv(4)*pv(30)
 v(809) = pv(5)*pv(30)
 v(810) = pv(6)*pv(30)
 v(811) = pv(7)*pv(30)
 v(812) = pv(8)*pv(30)
 v(813) = pv(9)*pv(30)
 v(814) = pv(10)*pv(30)
 v(815) = pv(11)*pv(30)
 v(816) = pv(12)*pv(30)
 v(817) = pv(13)*pv(30)
 v(818) = pv(15)*pv(30)
 v(819) = pv(16)*pv(30)
 v(820) = pv(2)*pv(31)
 v(821) = pv(3)*pv(31)
 v(822) = pv(4)*pv(31)
 v(823) = pv(5)*pv(31)
 v(824) = pv(6)*pv(31)
 v(825) = pv(7)*pv(31)
 v(826) = pv(8)*pv(31)
 v(827) = pv(9)*pv(31)
 v(828) = pv(10)*pv(31)
 v(829) = pv(11)*pv(31)
 v(830) = pv(12)*pv(31)
 v(831) = pv(13)*pv(31)
 v(832) = pv(15)*pv(31)
 v(833) = pv(16)*pv(31)
 v(834) = pv(2)*pv(32)
 v(835) = pv(3)*pv(32)
 v(836) = pv(4)*pv(32)
 v(837) = pv(5)*pv(32)
 v(838) = pv(6)*pv(32)
 v(839) = pv(7)*pv(32)
 v(840) = pv(9)*pv(32)
 v(841) = pv(11)*pv(32)
 v(842) = pv(12)*pv(32)
 v(843) = pv(2)*pv(33)
 v(844) = pv(3)*pv(33)
 v(845) = pv(4)*pv(33)
 v(846) = pv(5)*pv(33)
 v(847) = pv(6)*pv(33)
 v(848) = pv(7)*pv(33)
 v(849) = pv(8)*pv(33)
 v(850) = pv(9)*pv(33)
 v(851) = pv(10)*pv(33)
 v(852) = pv(11)*pv(33)
 v(853) = pv(12)*pv(33)
 v(854) = pv(13)*pv(33)
 v(855) = pv(14)*pv(33)
 v(856) = pv(15)*pv(33)
 v(857) = pv(16)*pv(33)
 v(858) = pv(17)*pv(33)
 v(859) = pv(2)*pv(34)
 v(860) = pv(3)*pv(34)
 v(861) = pv(4)*pv(34)
 v(862) = pv(5)*pv(34)
 v(863) = pv(6)*pv(34)
 v(864) = pv(7)*pv(34)
 v(865) = pv(8)*pv(34)
 v(866) = pv(9)*pv(34)
 v(867) = pv(10)*pv(34)
 v(868) = pv(11)*pv(34)
 v(869) = pv(12)*pv(34)
 v(870) = pv(13)*pv(34)
 v(871) = pv(15)*pv(34)
 v(872) = pv(16)*pv(34)
 v(873) = pv(2)*pv(35)
 v(874) = pv(3)*pv(35)
 v(875) = pv(4)*pv(35)
 v(876) = pv(5)*pv(35)
 v(877) = pv(6)*pv(35)
 v(878) = pv(7)*pv(35)
 v(879) = pv(8)*pv(35)
 v(880) = pv(9)*pv(35)
 v(881) = pv(10)*pv(35)
 v(882) = pv(11)*pv(35)
 v(883) = pv(12)*pv(35)
 v(884) = pv(13)*pv(35)
 v(885) = pv(14)*pv(35)
 v(886) = pv(15)*pv(35)
 v(887) = pv(16)*pv(35)
 v(888) = pv(17)*pv(35)
 v(889) = pv(2)*pv(36)
 v(890) = pv(3)*pv(36)
 v(891) = pv(4)*pv(36)
 v(892) = pv(5)*pv(36)
 v(893) = pv(6)*pv(36)
 v(894) = pv(7)*pv(36)
 v(895) = pv(8)*pv(36)
 v(896) = pv(9)*pv(36)
 v(897) = pv(10)*pv(36)
 v(898) = pv(11)*pv(36)
 v(899) = pv(12)*pv(36)
 v(900) = pv(13)*pv(36)
 v(901) = pv(14)*pv(36)
 v(902) = pv(15)*pv(36)
 v(903) = pv(16)*pv(36)
 v(904) = pv(17)*pv(36)
 v(905) = pv(2)*pv(37)
 v(906) = pv(3)*pv(37)
 v(907) = pv(4)*pv(37)
 v(908) = pv(5)*pv(37)
 v(909) = pv(6)*pv(37)
 v(910) = pv(7)*pv(37)
 v(911) = pv(8)*pv(37)
 v(912) = pv(9)*pv(37)
 v(913) = pv(10)*pv(37)
 v(914) = pv(11)*pv(37)
 v(915) = pv(12)*pv(37)
 v(916) = pv(13)*pv(37)
 v(917) = pv(15)*pv(37)
 v(918) = pv(16)*pv(37)
 v(919) = pv(2)*pv(38)
 v(920) = pv(3)*pv(38)
 v(921) = pv(4)*pv(38)
 v(922) = pv(5)*pv(38)
 v(923) = pv(6)*pv(38)
 v(924) = pv(7)*pv(38)
 v(925) = pv(8)*pv(38)
 v(926) = pv(9)*pv(38)
 v(927) = pv(11)*pv(38)
 v(928) = pv(12)*pv(38)
 v(929) = pv(2)*pv(39)
 v(930) = pv(3)*pv(39)
 v(931) = pv(4)*pv(39)
 v(932) = pv(5)*pv(39)
 v(933) = pv(6)*pv(39)
 v(934) = pv(7)*pv(39)
 v(935) = pv(8)*pv(39)
 v(936) = pv(9)*pv(39)
 v(937) = pv(10)*pv(39)
 v(938) = pv(11)*pv(39)
 v(939) = pv(12)*pv(39)
 v(940) = pv(13)*pv(39)
 v(941) = pv(14)*pv(39)
 v(942) = pv(15)*pv(39)
 v(943) = pv(16)*pv(39)
 v(944) = pv(17)*pv(39)
 v(945) = pv(2)*pv(40)
 v(946) = pv(3)*pv(40)
 v(947) = pv(4)*pv(40)
 v(948) = pv(5)*pv(40)
 v(949) = pv(6)*pv(40)
 v(950) = pv(7)*pv(40)
 v(951) = pv(8)*pv(40)
 v(952) = pv(9)*pv(40)
 v(953) = pv(10)*pv(40)
 v(954) = pv(11)*pv(40)
 v(955) = pv(12)*pv(40)
 v(956) = pv(13)*pv(40)
 v(957) = pv(14)*pv(40)
 v(958) = pv(15)*pv(40)
 v(959) = pv(16)*pv(40)
 v(960) = pv(2)*pv(41)
 v(961) = pv(3)*pv(41)
 v(962) = pv(4)*pv(41)
 v(963) = pv(5)*pv(41)
 v(964) = pv(6)*pv(41)
 v(965) = pv(7)*pv(41)
 v(966) = pv(8)*pv(41)
 v(967) = pv(9)*pv(41)
 v(968) = pv(10)*pv(41)
 v(969) = pv(11)*pv(41)
 v(970) = pv(12)*pv(41)
 v(971) = pv(13)*pv(41)
 v(972) = pv(14)*pv(41)
 v(973) = pv(15)*pv(41)
 v(974) = pv(16)*pv(41)
 v(975) = pv(2)*pv(42)
 v(976) = pv(3)*pv(42)
 v(977) = pv(4)*pv(42)
 v(978) = pv(5)*pv(42)
 v(979) = pv(6)*pv(42)
 v(980) = pv(7)*pv(42)
 v(981) = pv(8)*pv(42)
 v(982) = pv(9)*pv(42)
 v(983) = pv(10)*pv(42)
 v(984) = pv(11)*pv(42)
 v(985) = pv(12)*pv(42)
 v(986) = pv(13)*pv(42)
 v(987) = pv(14)*pv(42)
 v(988) = pv(15)*pv(42)
 v(989) = pv(16)*pv(42)
 v(990) = pv(17)*pv(42)
 v(991) = pv(2)*pv(43)
 v(992) = pv(3)*pv(43)
 v(993) = pv(4)*pv(43)
 v(994) = pv(5)*pv(43)
 v(995) = pv(6)*pv(43)
 v(996) = pv(7)*pv(43)
 v(997) = pv(8)*pv(43)
 v(998) = pv(9)*pv(43)
 v(999) = pv(10)*pv(43)
 v(1000) = pv(11)*pv(43)
 v(1001) = pv(12)*pv(43)
 v(1002) = pv(13)*pv(43)
 v(1003) = pv(15)*pv(43)
 v(1004) = pv(16)*pv(43)
 v(1005) = pv(2)*pv(44)
 v(1006) = pv(3)*pv(44)
 v(1007) = pv(4)*pv(44)
 v(1008) = pv(5)*pv(44)
 v(1009) = pv(6)*pv(44)
 v(1010) = pv(7)*pv(44)
 v(1011) = pv(8)*pv(44)
 v(1012) = pv(9)*pv(44)
 v(1013) = pv(10)*pv(44)
 v(1014) = pv(11)*pv(44)
 v(1015) = pv(12)*pv(44)
 v(1016) = pv(13)*pv(44)
 v(1017) = pv(14)*pv(44)
 v(1018) = pv(15)*pv(44)
 v(1019) = pv(16)*pv(44)
 v(1020) = pv(17)*pv(44)
 v(1021) = pv(2)*pv(45)
 v(1022) = pv(3)*pv(45)
 v(1023) = pv(4)*pv(45)
 v(1024) = pv(5)*pv(45)
 v(1025) = pv(6)*pv(45)
 v(1026) = pv(7)*pv(45)
 v(1027) = pv(8)*pv(45)
 v(1028) = pv(9)*pv(45)
 v(1029) = pv(10)*pv(45)
 v(1030) = pv(11)*pv(45)
 v(1031) = pv(12)*pv(45)
 v(1032) = pv(13)*pv(45)
 v(1033) = pv(15)*pv(45)
 v(1034) = pv(16)*pv(45)
 v(1035) = pv(2)*pv(46)
 v(1036) = pv(3)*pv(46)
 v(1037) = pv(4)*pv(46)
 v(1038) = pv(5)*pv(46)
 v(1039) = pv(6)*pv(46)
 v(1040) = pv(7)*pv(46)
 v(1041) = pv(8)*pv(46)
 v(1042) = pv(9)*pv(46)
 v(1043) = pv(10)*pv(46)
 v(1044) = pv(11)*pv(46)
 v(1045) = pv(12)*pv(46)
 v(1046) = pv(13)*pv(46)
 v(1047) = pv(14)*pv(46)
 v(1048) = pv(15)*pv(46)
 v(1049) = pv(16)*pv(46)
 v(1050) = pv(17)*pv(46)
 v(1051) = pv(2)*pv(47)
 v(1052) = pv(3)*pv(47)
 v(1053) = pv(4)*pv(47)
 v(1054) = pv(5)*pv(47)
 v(1055) = pv(6)*pv(47)
 v(1056) = pv(7)*pv(47)
 v(1057) = pv(8)*pv(47)
 v(1058) = pv(9)*pv(47)
 v(1059) = pv(10)*pv(47)
 v(1060) = pv(11)*pv(47)
 v(1061) = pv(12)*pv(47)
 v(1062) = pv(13)*pv(47)
 v(1063) = pv(15)*pv(47)
 v(1064) = pv(16)*pv(47)
 v(1065) = pv(2)*pv(48)
 v(1066) = pv(3)*pv(48)
 v(1067) = pv(4)*pv(48)
 v(1068) = pv(5)*pv(48)
 v(1069) = pv(6)*pv(48)
 v(1070) = pv(7)*pv(48)
 v(1071) = pv(8)*pv(48)
 v(1072) = pv(9)*pv(48)
 v(1073) = pv(10)*pv(48)
 v(1074) = pv(11)*pv(48)
 v(1075) = pv(12)*pv(48)
 v(1076) = pv(13)*pv(48)
 v(1077) = pv(15)*pv(48)
 v(1078) = pv(16)*pv(48)
 v(1079) = pv(2)*pv(49)
 v(1080) = pv(3)*pv(49)
 v(1081) = pv(4)*pv(49)
 v(1082) = pv(5)*pv(49)
 v(1083) = pv(6)*pv(49)
 v(1084) = pv(7)*pv(49)
 v(1085) = pv(8)*pv(49)
 v(1086) = pv(9)*pv(49)
 v(1087) = pv(10)*pv(49)
 v(1088) = pv(11)*pv(49)
 v(1089) = pv(12)*pv(49)
 v(1090) = pv(13)*pv(49)
 v(1091) = pv(14)*pv(49)
 v(1092) = pv(15)*pv(49)
 v(1093) = pv(16)*pv(49)
 v(1094) = pv(17)*pv(49)
 v(1095) = pv(2)*pv(50)
 v(1096) = pv(3)*pv(50)
 v(1097) = pv(4)*pv(50)
 v(1098) = pv(5)*pv(50)
 v(1099) = pv(6)*pv(50)
 v(1100) = pv(7)*pv(50)
 v(1101) = pv(8)*pv(50)
 v(1102) = pv(9)*pv(50)
 v(1103) = pv(10)*pv(50)
 v(1104) = pv(11)*pv(50)
 v(1105) = pv(12)*pv(50)
 v(1106) = pv(13)*pv(50)
 v(1107) = pv(14)*pv(50)
 v(1108) = pv(15)*pv(50)
 v(1109) = pv(16)*pv(50)
 v(1110) = pv(17)*pv(50)
 v(1111) = pv(2)*pv(51)
 v(1112) = pv(3)*pv(51)
 v(1113) = pv(4)*pv(51)
 v(1114) = pv(5)*pv(51)
 v(1115) = pv(6)*pv(51)
 v(1116) = pv(7)*pv(51)
 v(1117) = pv(8)*pv(51)
 v(1118) = pv(9)*pv(51)
 v(1119) = pv(10)*pv(51)
 v(1120) = pv(11)*pv(51)
 v(1121) = pv(12)*pv(51)
 v(1122) = pv(13)*pv(51)
 v(1123) = pv(14)*pv(51)
 v(1124) = pv(15)*pv(51)
 v(1125) = pv(16)*pv(51)
 v(1126) = pv(2)*pv(52)
 v(1127) = pv(3)*pv(52)
 v(1128) = pv(4)*pv(52)
 v(1129) = pv(5)*pv(52)
 v(1130) = pv(6)*pv(52)
 v(1131) = pv(7)*pv(52)
 v(1132) = pv(8)*pv(52)
 v(1133) = pv(9)*pv(52)
 v(1134) = pv(10)*pv(52)
 v(1135) = pv(11)*pv(52)
 v(1136) = pv(12)*pv(52)
 v(1137) = pv(13)*pv(52)
 v(1138) = pv(14)*pv(52)
 v(1139) = pv(15)*pv(52)
 v(1140) = pv(16)*pv(52)
 v(1141) = pv(2)*pv(53)
 v(1142) = pv(3)*pv(53)
 v(1143) = pv(4)*pv(53)
 v(1144) = pv(5)*pv(53)
 v(1145) = pv(6)*pv(53)
 v(1146) = pv(7)*pv(53)
 v(1147) = pv(8)*pv(53)
 v(1148) = pv(9)*pv(53)
 v(1149) = pv(10)*pv(53)
 v(1150) = pv(11)*pv(53)
 v(1151) = pv(12)*pv(53)
 v(1152) = pv(13)*pv(53)
 v(1153) = pv(15)*pv(53)
 v(1154) = pv(2)*pv(54)
 v(1155) = pv(3)*pv(54)
 v(1156) = pv(4)*pv(54)
 v(1157) = pv(5)*pv(54)
 v(1158) = pv(6)*pv(54)
 v(1159) = pv(7)*pv(54)
 v(1160) = pv(8)*pv(54)
 v(1161) = pv(9)*pv(54)
 v(1162) = pv(10)*pv(54)
 v(1163) = pv(11)*pv(54)
 v(1164) = pv(12)*pv(54)
 v(1165) = pv(13)*pv(54)
 v(1166) = pv(2)*pv(55)
 v(1167) = pv(3)*pv(55)
 v(1168) = pv(4)*pv(55)
 v(1169) = pv(5)*pv(55)
 v(1170) = pv(6)*pv(55)
 v(1171) = pv(7)*pv(55)
 v(1172) = pv(8)*pv(55)
 v(1173) = pv(9)*pv(55)
 v(1174) = pv(10)*pv(55)
 v(1175) = pv(11)*pv(55)
 v(1176) = pv(12)*pv(55)
 v(1177) = pv(13)*pv(55)
 v(1178) = pv(14)*pv(55)
 v(1179) = pv(15)*pv(55)
 v(1180) = pv(16)*pv(55)
 v(1181) = pv(2)*pv(56)
 v(1182) = pv(3)*pv(56)
 v(1183) = pv(4)*pv(56)
 v(1184) = pv(5)*pv(56)
 v(1185) = pv(6)*pv(56)
 v(1186) = pv(7)*pv(56)
 v(1187) = pv(8)*pv(56)
 v(1188) = pv(9)*pv(56)
 v(1189) = pv(10)*pv(56)
 v(1190) = pv(11)*pv(56)
 v(1191) = pv(12)*pv(56)
 v(1192) = pv(13)*pv(56)
 v(1193) = pv(14)*pv(56)
 v(1194) = pv(15)*pv(56)
 v(1195) = pv(16)*pv(56)
 v(1196) = pv(2)*pv(57)
 v(1197) = pv(3)*pv(57)
 v(1198) = pv(4)*pv(57)
 v(1199) = pv(5)*pv(57)
 v(1200) = pv(6)*pv(57)
 v(1201) = pv(7)*pv(57)
 v(1202) = pv(8)*pv(57)
 v(1203) = pv(9)*pv(57)
 v(1204) = pv(10)*pv(57)
 v(1205) = pv(11)*pv(57)
 v(1206) = pv(12)*pv(57)
 v(1207) = pv(13)*pv(57)
 v(1208) = pv(14)*pv(57)
 v(1209) = pv(15)*pv(57)
 v(1210) = pv(16)*pv(57)
 v(1211) = pv(2)*pv(58)
 v(1212) = pv(3)*pv(58)
 v(1213) = pv(4)*pv(58)
 v(1214) = pv(5)*pv(58)
 v(1215) = pv(6)*pv(58)
 v(1216) = pv(7)*pv(58)
 v(1217) = pv(8)*pv(58)
 v(1218) = pv(9)*pv(58)
 v(1219) = pv(10)*pv(58)
 v(1220) = pv(11)*pv(58)
 v(1221) = pv(12)*pv(58)
 v(1222) = pv(13)*pv(58)
 v(1223) = pv(14)*pv(58)
 v(1224) = pv(15)*pv(58)
 v(1225) = pv(16)*pv(58)
 v(1226) = pv(2)*pv(59)
 v(1227) = pv(3)*pv(59)
 v(1228) = pv(4)*pv(59)
 v(1229) = pv(5)*pv(59)
 v(1230) = pv(6)*pv(59)
 v(1231) = pv(7)*pv(59)
 v(1232) = pv(8)*pv(59)
 v(1233) = pv(9)*pv(59)
 v(1234) = pv(10)*pv(59)
 v(1235) = pv(11)*pv(59)
 v(1236) = pv(12)*pv(59)
 v(1237) = pv(13)*pv(59)
 v(1238) = pv(15)*pv(59)
 v(1239) = pv(2)*pv(60)
 v(1240) = pv(3)*pv(60)
 v(1241) = pv(4)*pv(60)
 v(1242) = pv(5)*pv(60)
 v(1243) = pv(6)*pv(60)
 v(1244) = pv(7)*pv(60)
 v(1245) = pv(8)*pv(60)
 v(1246) = pv(9)*pv(60)
 v(1247) = pv(10)*pv(60)
 v(1248) = pv(11)*pv(60)
 v(1249) = pv(12)*pv(60)
 v(1250) = pv(13)*pv(60)
 v(1251) = pv(14)*pv(60)
 v(1252) = pv(15)*pv(60)
 v(1253) = pv(16)*pv(60)
 v(1254) = pv(2)*pv(61)
 v(1255) = pv(3)*pv(61)
 v(1256) = pv(4)*pv(61)
 v(1257) = pv(5)*pv(61)
 v(1258) = pv(6)*pv(61)
 v(1259) = pv(7)*pv(61)
 v(1260) = pv(8)*pv(61)
 v(1261) = pv(9)*pv(61)
 v(1262) = pv(10)*pv(61)
 v(1263) = pv(11)*pv(61)
 v(1264) = pv(12)*pv(61)
 v(1265) = pv(13)*pv(61)
 v(1266) = pv(14)*pv(61)
 v(1267) = pv(15)*pv(61)
 v(1268) = pv(16)*pv(61)
 v(1269) = pv(2)*pv(62)
 v(1270) = pv(3)*pv(62)
 v(1271) = pv(4)*pv(62)
 v(1272) = pv(5)*pv(62)
 v(1273) = pv(6)*pv(62)
 v(1274) = pv(7)*pv(62)
 v(1275) = pv(8)*pv(62)
 v(1276) = pv(9)*pv(62)
 v(1277) = pv(10)*pv(62)
 v(1278) = pv(11)*pv(62)
 v(1279) = pv(12)*pv(62)
 v(1280) = pv(13)*pv(62)
 v(1281) = pv(14)*pv(62)
 v(1282) = pv(15)*pv(62)
 v(1283) = pv(16)*pv(62)
 v(1284) = pv(2)*pv(63)
 v(1285) = pv(3)*pv(63)
 v(1286) = pv(4)*pv(63)
 v(1287) = pv(5)*pv(63)
 v(1288) = pv(6)*pv(63)
 v(1289) = pv(7)*pv(63)
 v(1290) = pv(8)*pv(63)
 v(1291) = pv(9)*pv(63)
 v(1292) = pv(10)*pv(63)
 v(1293) = pv(11)*pv(63)
 v(1294) = pv(12)*pv(63)
 v(1295) = pv(15)*pv(63)
 v(1296) = pv(2)*pv(64)
 v(1297) = pv(3)*pv(64)
 v(1298) = pv(4)*pv(64)
 v(1299) = pv(5)*pv(64)
 v(1300) = pv(6)*pv(64)
 v(1301) = pv(7)*pv(64)
 v(1302) = pv(8)*pv(64)
 v(1303) = pv(9)*pv(64)
 v(1304) = pv(10)*pv(64)
 v(1305) = pv(11)*pv(64)
 v(1306) = pv(12)*pv(64)
 v(1307) = pv(13)*pv(64)
 v(1308) = pv(15)*pv(64)
 v(1309) = pv(2)*pv(65)
 v(1310) = pv(3)*pv(65)
 v(1311) = pv(4)*pv(65)
 v(1312) = pv(5)*pv(65)
 v(1313) = pv(6)*pv(65)
 v(1314) = pv(7)*pv(65)
 v(1315) = pv(8)*pv(65)
 v(1316) = pv(9)*pv(65)
 v(1317) = pv(10)*pv(65)
 v(1318) = pv(11)*pv(65)
 v(1319) = pv(12)*pv(65)
 v(1320) = pv(13)*pv(65)
 v(1321) = pv(14)*pv(65)
 v(1322) = pv(15)*pv(65)
 v(1323) = pv(0)*pv(66)
 v(1324) = pv(1)*pv(66)
 v(1325) = pv(0)*pv(67)
 v(1326) = pv(1)*pv(67)
 v(1327) = pv(0)*pv(68)
 v(1328) = pv(1)*pv(68)
 v(1329) = pv(0)*pv(69)
 v(1330) = pv(1)*pv(69)
 v(1331) = pv(0)*pv(70)
 v(1332) = pv(1)*pv(70)
 v(1333) = pv(0)*pv(71)
 v(1334) = pv(1)*pv(71)
 v(1335) = pv(0)*pv(72)
 v(1336) = pv(1)*pv(72)
 v(1337) = pv(0)*pv(73)
 v(1338) = pv(1)*pv(73)
 v(1339) = pv(0)*pv(74)
 v(1340) = pv(1)*pv(74)
 v(1341) = pv(0)*pv(75)
 v(1342) = pv(0)*pv(76)
 v(1343) = pv(1)*pv(76)
 v(1344) = pv(0)*pv(77)
 v(1345) = pv(1)*pv(77)
 v(1346) = pv(0)*pv(78)
 v(1347) = pv(1)*pv(78)
 v(1348) = pv(0)*pv(79)
 v(1349) = pv(1)*pv(79)
 v(1350) = pv(0)*pv(80)
 v(1351) = pv(1)*pv(80)
 v(1352) = pv(0)*pv(81)
 v(1353) = pv(1)*pv(81)
 v(1354) = pv(0)*pv(82)
 v(1355) = pv(1)*pv(82)
 v(1356) = pv(0)*pv(83)
 v(1357) = pv(1)*pv(83)
 v(1358) = pv(0)*pv(84)
 v(1359) = pv(1)*pv(84)
 v(1360) = pv(0)*pv(85)
 v(1361) = pv(1)*pv(85)
 v(1362) = pv(0)*pv(86)
 v(1363) = pv(1)*pv(86)
 v(1364) = pv(0)*pv(87)
 v(1365) = pv(1)*pv(87)
 v(1366) = pv(0)*pv(88)
 v(1367) = pv(1)*pv(88)
 v(1368) = pv(0)*pv(89)
 v(1369) = pv(1)*pv(89)
 v(1370) = pv(0)*pv(90)
 v(1371) = pv(1)*pv(90)
 v(1372) = pv(0)*pv(91)
 v(1373) = pv(1)*pv(91)
 v(1374) = pv(0)*pv(92)
 v(1375) = pv(1)*pv(92)
 v(1376) = pv(0)*pv(93)
 v(1377) = pv(1)*pv(93)
 v(1378) = pv(0)*pv(94)
 v(1379) = pv(1)*pv(94)
 v(1380) = pv(0)*pv(95)
 v(1381) = pv(1)*pv(95)
 v(1382) = pv(0)*pv(96)
 v(1383) = pv(1)*pv(96)
 v(1384) = pv(0)*pv(97)
 v(1385) = pv(1)*pv(97)
 v(1386) = pv(0)*pv(98)
 v(1387) = pv(1)*pv(98)
 v(1388) = pv(0)*pv(99)
 v(1389) = pv(1)*pv(99)
 v(1390) = pv(0)*pv(100)
 v(1391) = pv(1)*pv(100)
 v(1392) = pv(0)*pv(101)
 v(1393) = pv(1)*pv(101)
 v(1394) = pv(0)*pv(102)
 v(1395) = pv(1)*pv(102)
 v(1396) = pv(0)*pv(103)
 v(1397) = pv(1)*pv(103)
 v(1398) = pv(0)*pv(104)
 v(1399) = pv(1)*pv(104)
 v(1400) = pv(0)*pv(105)
 v(1401) = pv(1)*pv(105)
 v(1402) = pv(0)*pv(106)
 v(1403) = pv(1)*pv(106)
 v(1404) = pv(0)*pv(107)
 v(1405) = pv(1)*pv(107)
 v(1406) = pv(0)*pv(108)
 v(1407) = pv(1)*pv(108)
 v(1408) = pv(0)*pv(109)
 v(1409) = pv(1)*pv(109)
 v(1410) = pv(0)*pv(110)
 v(1411) = pv(1)*pv(110)
 v(1412) = pv(0)*pv(111)
 v(1413) = pv(1)*pv(111)
 v(1414) = pv(0)*pv(112)
 v(1415) = pv(1)*pv(112)
 v(1416) = pv(0)*pv(113)
 v(1417) = pv(1)*pv(113)
 v(1418) = pv(0)*pv(114)
 v(1419) = pv(1)*pv(114)
 v(1420) = pv(0)*pv(115)
 v(1421) = pv(1)*pv(115)
 v(1422) = pv(0)*pv(116)
 v(1423) = pv(1)*pv(116)
 v(1424) = pv(0)*pv(117)
 v(1425) = pv(1)*pv(117)
 v(1426) = pv(0)*pv(118)
 v(1427) = pv(1)*pv(118)
 v(1428) = pv(0)*pv(119)
 v(1429) = pv(1)*pv(119)
 v(1430) = pv(0)*pv(120)
 v(1431) = pv(1)*pv(120)
 v(1432) = pv(0)*pv(121)
 v(1433) = pv(1)*pv(121)
 v(1434) = pv(0)*pv(122)
 v(1435) = pv(1)*pv(122)
 v(1436) = pv(0)*pv(123)
 v(1437) = pv(1)*pv(123)
 v(1438) = pv(0)*pv(124)
 v(1439) = pv(1)*pv(124)
 v(1440) = pv(0)*pv(125)
 v(1441) = pv(1)*pv(125)
 v(1442) = pv(0)*pv(126)
 v(1443) = pv(1)*pv(126)
 v(1444) = pv(0)*pv(127)
 v(1445) = pv(1)*pv(127)
 v(1446) = pv(0)*pv(128)
 v(1447) = pv(1)*pv(128)
 v(1448) = pv(0)*pv(129)
 v(1449) = pv(1)*pv(129)
 v(1450) = pv(0)*pv(130)
 v(1451) = pv(1)*pv(130)
 v(1452) = pv(0)*pv(131)
 v(1453) = pv(1)*pv(131)
 v(1454) = pv(0)*pv(132)
 v(1455) = pv(1)*pv(132)
 v(1456) = pv(0)*pv(133)
 v(1457) = pv(1)*pv(133)
 v(1458) = pv(0)*pv(134)
 v(1459) = pv(1)*pv(134)
 v(1460) = pv(0)*pv(135)
 v(1461) = pv(1)*pv(135)
 v(1462) = pv(0)*pv(136)
 v(1463) = pv(1)*pv(136)
 v(1464) = pv(0)*pv(137)
 v(1465) = pv(1)*pv(137)
 v(1466) = pv(0)*pv(138)
 v(1467) = pv(1)*pv(138)
 v(1468) = pv(0)*pv(139)
 v(1469) = pv(1)*pv(139)
 v(1470) = pv(0)*pv(140)
 v(1471) = pv(1)*pv(140)
 v(1472) = pv(0)*pv(141)
 v(1473) = pv(1)*pv(141)
 v(1474) = pv(0)*pv(142)
 v(1475) = pv(1)*pv(142)
 v(1476) = pv(0)*pv(143)
 v(1477) = pv(1)*pv(143)
 v(1478) = pv(0)*pv(144)
 v(1479) = pv(1)*pv(144)
 v(1480) = pv(0)*pv(145)
 v(1481) = pv(1)*pv(145)
 v(1482) = pv(0)*pv(146)
 v(1483) = pv(1)*pv(146)
 v(1484) = pv(0)*pv(147)
 v(1485) = pv(1)*pv(147)
 v(1486) = pv(0)*pv(148)
 v(1487) = pv(1)*pv(148)
 v(1488) = pv(0)*pv(149)
 v(1489) = pv(1)*pv(149)
 v(1490) = pv(0)*pv(150)
 v(1491) = pv(0)*pv(151)
 v(1492) = pv(1)*pv(151)
 v(1493) = pv(0)*pv(152)
 v(1494) = pv(1)*pv(152)
 v(1495) = pv(0)*pv(153)
 v(1496) = pv(1)*pv(153)
 v(1497) = pv(0)*pv(154)
 v(1498) = pv(1)*pv(154)
 v(1499) = pv(0)*pv(155)
 v(1500) = pv(1)*pv(155)
 v(1501) = pv(0)*pv(156)
 v(1502) = pv(1)*pv(156)
 v(1503) = pv(0)*pv(157)
 v(1504) = pv(1)*pv(157)
 v(1505) = pv(0)*pv(158)
 v(1506) = pv(1)*pv(158)
 v(1507) = pv(0)*pv(159)
 v(1508) = pv(1)*pv(159)
 v(1509) = pv(0)*pv(160)
 v(1510) = pv(1)*pv(160)
 v(1511) = pv(0)*pv(161)
 v(1512) = pv(1)*pv(161)
 v(1513) = pv(0)*pv(162)
 v(1514) = pv(1)*pv(162)
 v(1515) = pv(0)*pv(163)
 v(1516) = pv(1)*pv(163)
 v(1517) = pv(0)*pv(164)
 v(1518) = pv(1)*pv(164)
 v(1519) = pv(0)*pv(165)
 v(1520) = pv(1)*pv(165)
 v(1521) = pv(0)*pv(166)
 v(1522) = pv(1)*pv(166)
 v(1523) = pv(0)*pv(167)
 v(1524) = pv(1)*pv(167)
 v(1525) = pv(0)*pv(168)
 v(1526) = pv(1)*pv(168)
 v(1527) = pv(0)*pv(169)
 v(1528) = pv(1)*pv(169)
 v(1529) = pv(0)*pv(170)
 v(1530) = pv(1)*pv(170)
 v(1531) = pv(0)*pv(171)
 v(1532) = pv(0)*pv(172)
 v(1533) = pv(1)*pv(172)
 v(1534) = pv(0)*pv(173)
 v(1535) = pv(1)*pv(173)
 v(1536) = pv(0)*pv(174)
 v(1537) = pv(1)*pv(174)
 v(1538) = pv(0)*pv(175)
 v(1539) = pv(1)*pv(175)
 v(1540) = pv(0)*pv(176)
 v(1541) = pv(1)*pv(176)
 v(1542) = pv(0)*pv(177)
 v(1543) = pv(1)*pv(177)
 v(1544) = pv(0)*pv(178)
 v(1545) = pv(1)*pv(178)
 v(1546) = pv(0)*pv(179)
 v(1547) = pv(1)*pv(179)
 v(1548) = pv(0)*pv(180)
 v(1549) = pv(1)*pv(180)
 v(1550) = pv(0)*pv(181)
 v(1551) = pv(1)*pv(181)
 v(1552) = pv(0)*pv(182)
 v(1553) = pv(1)*pv(182)
 v(1554) = pv(0)*pv(183)
 v(1555) = pv(1)*pv(183)
 v(1556) = pv(0)*pv(184)
 v(1557) = pv(1)*pv(184)
 v(1558) = pv(0)*pv(185)
 v(1559) = pv(1)*pv(185)
 v(1560) = pv(0)*pv(186)
 v(1561) = pv(1)*pv(186)
 v(1562) = pv(0)*pv(187)
 v(1563) = pv(1)*pv(187)
 v(1564) = pv(0)*pv(188)
 v(1565) = pv(1)*pv(188)
 v(1566) = pv(0)*pv(189)
 v(1567) = pv(1)*pv(189)
 v(1568:1885) = pv(432:749)
endif
if (8.le.mxd) then
 stop 'mg43: degree 8 not implemented' !! Tried it 050813
endif
return
END SUBROUTINE mg43_secs
