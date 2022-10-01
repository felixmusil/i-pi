SUBROUTINE mg4411_secs (mxd, r, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: v(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: pv(0:size(v)-1) !! an upper bound size
if (size(r).ne.nk*nk.or.size(v).ne.mg4411_nsc(mxd)) then
 stop 'mg4411_secs: bad dimensions'
endif
call mg4411_isecs (mxd, r, pv)
v(0) = 1
if (2.le.mxd) then
 v(1:12) = pv(0:11)
endif
if (3.le.mxd) then
 v(13:88) = pv(12:87)
endif
if (4.le.mxd) then
 v(89) = pv(0)*pv(0)
 v(90) = pv(0)*pv(1)
 v(91) = pv(1)*pv(1)
 v(92) = pv(0)*pv(2)
 v(93) = pv(1)*pv(2)
 v(94) = pv(2)*pv(2)
 v(95) = pv(0)*pv(3)
 v(96) = pv(1)*pv(3)
 v(97) = pv(2)*pv(3)
 v(98) = pv(3)*pv(3)
 v(99) = pv(0)*pv(4)
 v(100) = pv(1)*pv(4)
 v(101) = pv(2)*pv(4)
 v(102) = pv(3)*pv(4)
 v(103) = pv(4)*pv(4)
 v(104) = pv(0)*pv(5)
 v(105) = pv(1)*pv(5)
 v(106) = pv(2)*pv(5)
 v(107) = pv(3)*pv(5)
 v(108) = pv(4)*pv(5)
 v(109) = pv(5)*pv(5)
 v(110) = pv(0)*pv(6)
 v(111) = pv(1)*pv(6)
 v(112) = pv(2)*pv(6)
 v(113) = pv(3)*pv(6)
 v(114) = pv(4)*pv(6)
 v(115) = pv(5)*pv(6)
 v(116) = pv(6)*pv(6)
 v(117) = pv(0)*pv(7)
 v(118) = pv(1)*pv(7)
 v(119) = pv(2)*pv(7)
 v(120) = pv(3)*pv(7)
 v(121) = pv(4)*pv(7)
 v(122) = pv(5)*pv(7)
 v(123) = pv(6)*pv(7)
 v(124) = pv(7)*pv(7)
 v(125) = pv(0)*pv(8)
 v(126) = pv(1)*pv(8)
 v(127) = pv(2)*pv(8)
 v(128) = pv(3)*pv(8)
 v(129) = pv(4)*pv(8)
 v(130) = pv(5)*pv(8)
 v(131) = pv(6)*pv(8)
 v(132) = pv(7)*pv(8)
 v(133) = pv(8)*pv(8)
 v(134) = pv(0)*pv(9)
 v(135) = pv(1)*pv(9)
 v(136) = pv(2)*pv(9)
 v(137) = pv(3)*pv(9)
 v(138) = pv(4)*pv(9)
 v(139) = pv(5)*pv(9)
 v(140) = pv(6)*pv(9)
 v(141) = pv(7)*pv(9)
 v(142) = pv(8)*pv(9)
 v(143) = pv(9)*pv(9)
 v(144) = pv(0)*pv(10)
 v(145) = pv(1)*pv(10)
 v(146) = pv(2)*pv(10)
 v(147) = pv(3)*pv(10)
 v(148) = pv(4)*pv(10)
 v(149) = pv(5)*pv(10)
 v(150) = pv(6)*pv(10)
 v(151) = pv(7)*pv(10)
 v(152) = pv(8)*pv(10)
 v(153) = pv(9)*pv(10)
 v(154) = pv(10)*pv(10)
 v(155) = pv(0)*pv(11)
 v(156) = pv(1)*pv(11)
 v(157) = pv(2)*pv(11)
 v(158) = pv(3)*pv(11)
 v(159) = pv(4)*pv(11)
 v(160) = pv(5)*pv(11)
 v(161) = pv(6)*pv(11)
 v(162) = pv(7)*pv(11)
 v(163) = pv(8)*pv(11)
 v(164) = pv(9)*pv(11)
 v(165) = pv(10)*pv(11)
 v(166) = pv(11)*pv(11)
 v(167:492) = pv(88:413)
endif
if (5.le.mxd) then
 v(493) = pv(0)*pv(12)
 v(494) = pv(1)*pv(12)
 v(495) = pv(2)*pv(12)
 v(496) = pv(3)*pv(12)
 v(497) = pv(4)*pv(12)
 v(498) = pv(5)*pv(12)
 v(499) = pv(6)*pv(12)
 v(500) = pv(7)*pv(12)
 v(501) = pv(8)*pv(12)
 v(502) = pv(9)*pv(12)
 v(503) = pv(10)*pv(12)
 v(504) = pv(11)*pv(12)
 v(505) = pv(0)*pv(13)
 v(506) = pv(1)*pv(13)
 v(507) = pv(2)*pv(13)
 v(508) = pv(3)*pv(13)
 v(509) = pv(4)*pv(13)
 v(510) = pv(5)*pv(13)
 v(511) = pv(6)*pv(13)
 v(512) = pv(7)*pv(13)
 v(513) = pv(8)*pv(13)
 v(514) = pv(9)*pv(13)
 v(515) = pv(10)*pv(13)
 v(516) = pv(11)*pv(13)
 v(517) = pv(0)*pv(14)
 v(518) = pv(1)*pv(14)
 v(519) = pv(2)*pv(14)
 v(520) = pv(3)*pv(14)
 v(521) = pv(4)*pv(14)
 v(522) = pv(5)*pv(14)
 v(523) = pv(6)*pv(14)
 v(524) = pv(7)*pv(14)
 v(525) = pv(8)*pv(14)
 v(526) = pv(9)*pv(14)
 v(527) = pv(10)*pv(14)
 v(528) = pv(11)*pv(14)
 v(529) = pv(0)*pv(15)
 v(530) = pv(1)*pv(15)
 v(531) = pv(2)*pv(15)
 v(532) = pv(3)*pv(15)
 v(533) = pv(4)*pv(15)
 v(534) = pv(5)*pv(15)
 v(535) = pv(6)*pv(15)
 v(536) = pv(7)*pv(15)
 v(537) = pv(8)*pv(15)
 v(538) = pv(9)*pv(15)
 v(539) = pv(10)*pv(15)
 v(540) = pv(11)*pv(15)
 v(541) = pv(0)*pv(16)
 v(542) = pv(1)*pv(16)
 v(543) = pv(2)*pv(16)
 v(544) = pv(3)*pv(16)
 v(545) = pv(4)*pv(16)
 v(546) = pv(5)*pv(16)
 v(547) = pv(6)*pv(16)
 v(548) = pv(7)*pv(16)
 v(549) = pv(8)*pv(16)
 v(550) = pv(9)*pv(16)
 v(551) = pv(10)*pv(16)
 v(552) = pv(11)*pv(16)
 v(553) = pv(0)*pv(17)
 v(554) = pv(1)*pv(17)
 v(555) = pv(2)*pv(17)
 v(556) = pv(3)*pv(17)
 v(557) = pv(4)*pv(17)
 v(558) = pv(5)*pv(17)
 v(559) = pv(6)*pv(17)
 v(560) = pv(7)*pv(17)
 v(561) = pv(8)*pv(17)
 v(562) = pv(9)*pv(17)
 v(563) = pv(10)*pv(17)
 v(564) = pv(11)*pv(17)
 v(565) = pv(0)*pv(18)
 v(566) = pv(1)*pv(18)
 v(567) = pv(2)*pv(18)
 v(568) = pv(3)*pv(18)
 v(569) = pv(4)*pv(18)
 v(570) = pv(5)*pv(18)
 v(571) = pv(6)*pv(18)
 v(572) = pv(7)*pv(18)
 v(573) = pv(8)*pv(18)
 v(574) = pv(9)*pv(18)
 v(575) = pv(10)*pv(18)
 v(576) = pv(11)*pv(18)
 v(577) = pv(0)*pv(19)
 v(578) = pv(1)*pv(19)
 v(579) = pv(2)*pv(19)
 v(580) = pv(3)*pv(19)
 v(581) = pv(4)*pv(19)
 v(582) = pv(5)*pv(19)
 v(583) = pv(6)*pv(19)
 v(584) = pv(7)*pv(19)
 v(585) = pv(8)*pv(19)
 v(586) = pv(9)*pv(19)
 v(587) = pv(10)*pv(19)
 v(588) = pv(11)*pv(19)
 v(589) = pv(0)*pv(20)
 v(590) = pv(1)*pv(20)
 v(591) = pv(2)*pv(20)
 v(592) = pv(3)*pv(20)
 v(593) = pv(4)*pv(20)
 v(594) = pv(5)*pv(20)
 v(595) = pv(6)*pv(20)
 v(596) = pv(7)*pv(20)
 v(597) = pv(8)*pv(20)
 v(598) = pv(9)*pv(20)
 v(599) = pv(10)*pv(20)
 v(600) = pv(11)*pv(20)
 v(601) = pv(0)*pv(21)
 v(602) = pv(1)*pv(21)
 v(603) = pv(2)*pv(21)
 v(604) = pv(3)*pv(21)
 v(605) = pv(4)*pv(21)
 v(606) = pv(5)*pv(21)
 v(607) = pv(6)*pv(21)
 v(608) = pv(7)*pv(21)
 v(609) = pv(8)*pv(21)
 v(610) = pv(9)*pv(21)
 v(611) = pv(10)*pv(21)
 v(612) = pv(11)*pv(21)
 v(613) = pv(0)*pv(22)
 v(614) = pv(1)*pv(22)
 v(615) = pv(2)*pv(22)
 v(616) = pv(3)*pv(22)
 v(617) = pv(4)*pv(22)
 v(618) = pv(5)*pv(22)
 v(619) = pv(6)*pv(22)
 v(620) = pv(7)*pv(22)
 v(621) = pv(8)*pv(22)
 v(622) = pv(9)*pv(22)
 v(623) = pv(10)*pv(22)
 v(624) = pv(11)*pv(22)
 v(625) = pv(0)*pv(23)
 v(626) = pv(1)*pv(23)
 v(627) = pv(2)*pv(23)
 v(628) = pv(3)*pv(23)
 v(629) = pv(4)*pv(23)
 v(630) = pv(5)*pv(23)
 v(631) = pv(6)*pv(23)
 v(632) = pv(7)*pv(23)
 v(633) = pv(8)*pv(23)
 v(634) = pv(9)*pv(23)
 v(635) = pv(10)*pv(23)
 v(636) = pv(11)*pv(23)
 v(637) = pv(0)*pv(24)
 v(638) = pv(1)*pv(24)
 v(639) = pv(2)*pv(24)
 v(640) = pv(3)*pv(24)
 v(641) = pv(4)*pv(24)
 v(642) = pv(5)*pv(24)
 v(643) = pv(6)*pv(24)
 v(644) = pv(7)*pv(24)
 v(645) = pv(8)*pv(24)
 v(646) = pv(9)*pv(24)
 v(647) = pv(10)*pv(24)
 v(648) = pv(11)*pv(24)
 v(649) = pv(0)*pv(25)
 v(650) = pv(1)*pv(25)
 v(651) = pv(2)*pv(25)
 v(652) = pv(3)*pv(25)
 v(653) = pv(4)*pv(25)
 v(654) = pv(5)*pv(25)
 v(655) = pv(6)*pv(25)
 v(656) = pv(7)*pv(25)
 v(657) = pv(8)*pv(25)
 v(658) = pv(9)*pv(25)
 v(659) = pv(10)*pv(25)
 v(660) = pv(11)*pv(25)
 v(661) = pv(0)*pv(26)
 v(662) = pv(1)*pv(26)
 v(663) = pv(2)*pv(26)
 v(664) = pv(3)*pv(26)
 v(665) = pv(4)*pv(26)
 v(666) = pv(5)*pv(26)
 v(667) = pv(6)*pv(26)
 v(668) = pv(7)*pv(26)
 v(669) = pv(8)*pv(26)
 v(670) = pv(9)*pv(26)
 v(671) = pv(10)*pv(26)
 v(672) = pv(11)*pv(26)
 v(673) = pv(0)*pv(27)
 v(674) = pv(1)*pv(27)
 v(675) = pv(2)*pv(27)
 v(676) = pv(3)*pv(27)
 v(677) = pv(4)*pv(27)
 v(678) = pv(5)*pv(27)
 v(679) = pv(6)*pv(27)
 v(680) = pv(7)*pv(27)
 v(681) = pv(8)*pv(27)
 v(682) = pv(9)*pv(27)
 v(683) = pv(10)*pv(27)
 v(684) = pv(11)*pv(27)
 v(685) = pv(0)*pv(28)
 v(686) = pv(1)*pv(28)
 v(687) = pv(2)*pv(28)
 v(688) = pv(3)*pv(28)
 v(689) = pv(4)*pv(28)
 v(690) = pv(5)*pv(28)
 v(691) = pv(6)*pv(28)
 v(692) = pv(7)*pv(28)
 v(693) = pv(8)*pv(28)
 v(694) = pv(9)*pv(28)
 v(695) = pv(10)*pv(28)
 v(696) = pv(11)*pv(28)
 v(697) = pv(0)*pv(29)
 v(698) = pv(1)*pv(29)
 v(699) = pv(2)*pv(29)
 v(700) = pv(3)*pv(29)
 v(701) = pv(4)*pv(29)
 v(702) = pv(5)*pv(29)
 v(703) = pv(6)*pv(29)
 v(704) = pv(7)*pv(29)
 v(705) = pv(8)*pv(29)
 v(706) = pv(9)*pv(29)
 v(707) = pv(10)*pv(29)
 v(708) = pv(11)*pv(29)
 v(709) = pv(0)*pv(30)
 v(710) = pv(1)*pv(30)
 v(711) = pv(2)*pv(30)
 v(712) = pv(3)*pv(30)
 v(713) = pv(4)*pv(30)
 v(714) = pv(5)*pv(30)
 v(715) = pv(6)*pv(30)
 v(716) = pv(7)*pv(30)
 v(717) = pv(8)*pv(30)
 v(718) = pv(9)*pv(30)
 v(719) = pv(10)*pv(30)
 v(720) = pv(11)*pv(30)
 v(721) = pv(0)*pv(31)
 v(722) = pv(1)*pv(31)
 v(723) = pv(2)*pv(31)
 v(724) = pv(3)*pv(31)
 v(725) = pv(4)*pv(31)
 v(726) = pv(5)*pv(31)
 v(727) = pv(6)*pv(31)
 v(728) = pv(7)*pv(31)
 v(729) = pv(8)*pv(31)
 v(730) = pv(9)*pv(31)
 v(731) = pv(10)*pv(31)
 v(732) = pv(11)*pv(31)
 v(733) = pv(0)*pv(32)
 v(734) = pv(1)*pv(32)
 v(735) = pv(2)*pv(32)
 v(736) = pv(3)*pv(32)
 v(737) = pv(4)*pv(32)
 v(738) = pv(5)*pv(32)
 v(739) = pv(6)*pv(32)
 v(740) = pv(7)*pv(32)
 v(741) = pv(8)*pv(32)
 v(742) = pv(9)*pv(32)
 v(743) = pv(10)*pv(32)
 v(744) = pv(11)*pv(32)
 v(745) = pv(0)*pv(33)
 v(746) = pv(1)*pv(33)
 v(747) = pv(2)*pv(33)
 v(748) = pv(3)*pv(33)
 v(749) = pv(4)*pv(33)
 v(750) = pv(5)*pv(33)
 v(751) = pv(6)*pv(33)
 v(752) = pv(7)*pv(33)
 v(753) = pv(8)*pv(33)
 v(754) = pv(9)*pv(33)
 v(755) = pv(10)*pv(33)
 v(756) = pv(11)*pv(33)
 v(757) = pv(0)*pv(34)
 v(758) = pv(1)*pv(34)
 v(759) = pv(2)*pv(34)
 v(760) = pv(3)*pv(34)
 v(761) = pv(4)*pv(34)
 v(762) = pv(5)*pv(34)
 v(763) = pv(6)*pv(34)
 v(764) = pv(7)*pv(34)
 v(765) = pv(8)*pv(34)
 v(766) = pv(9)*pv(34)
 v(767) = pv(10)*pv(34)
 v(768) = pv(11)*pv(34)
 v(769) = pv(0)*pv(35)
 v(770) = pv(1)*pv(35)
 v(771) = pv(2)*pv(35)
 v(772) = pv(3)*pv(35)
 v(773) = pv(4)*pv(35)
 v(774) = pv(5)*pv(35)
 v(775) = pv(6)*pv(35)
 v(776) = pv(7)*pv(35)
 v(777) = pv(8)*pv(35)
 v(778) = pv(9)*pv(35)
 v(779) = pv(10)*pv(35)
 v(780) = pv(11)*pv(35)
 v(781) = pv(0)*pv(36)
 v(782) = pv(1)*pv(36)
 v(783) = pv(2)*pv(36)
 v(784) = pv(3)*pv(36)
 v(785) = pv(4)*pv(36)
 v(786) = pv(5)*pv(36)
 v(787) = pv(6)*pv(36)
 v(788) = pv(7)*pv(36)
 v(789) = pv(8)*pv(36)
 v(790) = pv(9)*pv(36)
 v(791) = pv(10)*pv(36)
 v(792) = pv(11)*pv(36)
 v(793) = pv(0)*pv(37)
 v(794) = pv(1)*pv(37)
 v(795) = pv(2)*pv(37)
 v(796) = pv(3)*pv(37)
 v(797) = pv(4)*pv(37)
 v(798) = pv(5)*pv(37)
 v(799) = pv(6)*pv(37)
 v(800) = pv(7)*pv(37)
 v(801) = pv(8)*pv(37)
 v(802) = pv(9)*pv(37)
 v(803) = pv(10)*pv(37)
 v(804) = pv(11)*pv(37)
 v(805) = pv(0)*pv(38)
 v(806) = pv(1)*pv(38)
 v(807) = pv(2)*pv(38)
 v(808) = pv(3)*pv(38)
 v(809) = pv(4)*pv(38)
 v(810) = pv(5)*pv(38)
 v(811) = pv(6)*pv(38)
 v(812) = pv(7)*pv(38)
 v(813) = pv(8)*pv(38)
 v(814) = pv(9)*pv(38)
 v(815) = pv(10)*pv(38)
 v(816) = pv(11)*pv(38)
 v(817) = pv(0)*pv(39)
 v(818) = pv(1)*pv(39)
 v(819) = pv(2)*pv(39)
 v(820) = pv(3)*pv(39)
 v(821) = pv(4)*pv(39)
 v(822) = pv(5)*pv(39)
 v(823) = pv(6)*pv(39)
 v(824) = pv(7)*pv(39)
 v(825) = pv(8)*pv(39)
 v(826) = pv(9)*pv(39)
 v(827) = pv(10)*pv(39)
 v(828) = pv(11)*pv(39)
 v(829) = pv(0)*pv(40)
 v(830) = pv(1)*pv(40)
 v(831) = pv(2)*pv(40)
 v(832) = pv(3)*pv(40)
 v(833) = pv(4)*pv(40)
 v(834) = pv(5)*pv(40)
 v(835) = pv(6)*pv(40)
 v(836) = pv(7)*pv(40)
 v(837) = pv(8)*pv(40)
 v(838) = pv(9)*pv(40)
 v(839) = pv(10)*pv(40)
 v(840) = pv(11)*pv(40)
 v(841) = pv(0)*pv(41)
 v(842) = pv(1)*pv(41)
 v(843) = pv(2)*pv(41)
 v(844) = pv(3)*pv(41)
 v(845) = pv(4)*pv(41)
 v(846) = pv(5)*pv(41)
 v(847) = pv(6)*pv(41)
 v(848) = pv(7)*pv(41)
 v(849) = pv(8)*pv(41)
 v(850) = pv(9)*pv(41)
 v(851) = pv(10)*pv(41)
 v(852) = pv(11)*pv(41)
 v(853) = pv(0)*pv(42)
 v(854) = pv(1)*pv(42)
 v(855) = pv(2)*pv(42)
 v(856) = pv(3)*pv(42)
 v(857) = pv(4)*pv(42)
 v(858) = pv(5)*pv(42)
 v(859) = pv(6)*pv(42)
 v(860) = pv(7)*pv(42)
 v(861) = pv(8)*pv(42)
 v(862) = pv(9)*pv(42)
 v(863) = pv(10)*pv(42)
 v(864) = pv(11)*pv(42)
 v(865) = pv(0)*pv(43)
 v(866) = pv(1)*pv(43)
 v(867) = pv(2)*pv(43)
 v(868) = pv(3)*pv(43)
 v(869) = pv(4)*pv(43)
 v(870) = pv(5)*pv(43)
 v(871) = pv(6)*pv(43)
 v(872) = pv(7)*pv(43)
 v(873) = pv(8)*pv(43)
 v(874) = pv(9)*pv(43)
 v(875) = pv(10)*pv(43)
 v(876) = pv(11)*pv(43)
 v(877) = pv(0)*pv(44)
 v(878) = pv(1)*pv(44)
 v(879) = pv(2)*pv(44)
 v(880) = pv(3)*pv(44)
 v(881) = pv(4)*pv(44)
 v(882) = pv(5)*pv(44)
 v(883) = pv(6)*pv(44)
 v(884) = pv(7)*pv(44)
 v(885) = pv(8)*pv(44)
 v(886) = pv(9)*pv(44)
 v(887) = pv(10)*pv(44)
 v(888) = pv(11)*pv(44)
 v(889) = pv(0)*pv(45)
 v(890) = pv(1)*pv(45)
 v(891) = pv(2)*pv(45)
 v(892) = pv(3)*pv(45)
 v(893) = pv(4)*pv(45)
 v(894) = pv(5)*pv(45)
 v(895) = pv(6)*pv(45)
 v(896) = pv(7)*pv(45)
 v(897) = pv(8)*pv(45)
 v(898) = pv(9)*pv(45)
 v(899) = pv(10)*pv(45)
 v(900) = pv(11)*pv(45)
 v(901) = pv(0)*pv(46)
 v(902) = pv(1)*pv(46)
 v(903) = pv(2)*pv(46)
 v(904) = pv(3)*pv(46)
 v(905) = pv(4)*pv(46)
 v(906) = pv(5)*pv(46)
 v(907) = pv(6)*pv(46)
 v(908) = pv(7)*pv(46)
 v(909) = pv(8)*pv(46)
 v(910) = pv(9)*pv(46)
 v(911) = pv(10)*pv(46)
 v(912) = pv(11)*pv(46)
 v(913) = pv(0)*pv(47)
 v(914) = pv(1)*pv(47)
 v(915) = pv(2)*pv(47)
 v(916) = pv(3)*pv(47)
 v(917) = pv(4)*pv(47)
 v(918) = pv(5)*pv(47)
 v(919) = pv(6)*pv(47)
 v(920) = pv(7)*pv(47)
 v(921) = pv(8)*pv(47)
 v(922) = pv(9)*pv(47)
 v(923) = pv(10)*pv(47)
 v(924) = pv(11)*pv(47)
 v(925) = pv(0)*pv(48)
 v(926) = pv(1)*pv(48)
 v(927) = pv(2)*pv(48)
 v(928) = pv(3)*pv(48)
 v(929) = pv(4)*pv(48)
 v(930) = pv(5)*pv(48)
 v(931) = pv(6)*pv(48)
 v(932) = pv(7)*pv(48)
 v(933) = pv(8)*pv(48)
 v(934) = pv(9)*pv(48)
 v(935) = pv(10)*pv(48)
 v(936) = pv(11)*pv(48)
 v(937) = pv(0)*pv(49)
 v(938) = pv(1)*pv(49)
 v(939) = pv(2)*pv(49)
 v(940) = pv(3)*pv(49)
 v(941) = pv(4)*pv(49)
 v(942) = pv(5)*pv(49)
 v(943) = pv(6)*pv(49)
 v(944) = pv(7)*pv(49)
 v(945) = pv(8)*pv(49)
 v(946) = pv(9)*pv(49)
 v(947) = pv(10)*pv(49)
 v(948) = pv(11)*pv(49)
 v(949) = pv(0)*pv(50)
 v(950) = pv(1)*pv(50)
 v(951) = pv(2)*pv(50)
 v(952) = pv(3)*pv(50)
 v(953) = pv(4)*pv(50)
 v(954) = pv(5)*pv(50)
 v(955) = pv(6)*pv(50)
 v(956) = pv(7)*pv(50)
 v(957) = pv(8)*pv(50)
 v(958) = pv(9)*pv(50)
 v(959) = pv(10)*pv(50)
 v(960) = pv(11)*pv(50)
 v(961) = pv(0)*pv(51)
 v(962) = pv(1)*pv(51)
 v(963) = pv(2)*pv(51)
 v(964) = pv(3)*pv(51)
 v(965) = pv(4)*pv(51)
 v(966) = pv(5)*pv(51)
 v(967) = pv(6)*pv(51)
 v(968) = pv(7)*pv(51)
 v(969) = pv(8)*pv(51)
 v(970) = pv(9)*pv(51)
 v(971) = pv(10)*pv(51)
 v(972) = pv(11)*pv(51)
 v(973) = pv(0)*pv(52)
 v(974) = pv(1)*pv(52)
 v(975) = pv(2)*pv(52)
 v(976) = pv(3)*pv(52)
 v(977) = pv(4)*pv(52)
 v(978) = pv(5)*pv(52)
 v(979) = pv(6)*pv(52)
 v(980) = pv(7)*pv(52)
 v(981) = pv(8)*pv(52)
 v(982) = pv(9)*pv(52)
 v(983) = pv(10)*pv(52)
 v(984) = pv(11)*pv(52)
 v(985) = pv(0)*pv(53)
 v(986) = pv(1)*pv(53)
 v(987) = pv(2)*pv(53)
 v(988) = pv(3)*pv(53)
 v(989) = pv(4)*pv(53)
 v(990) = pv(5)*pv(53)
 v(991) = pv(6)*pv(53)
 v(992) = pv(7)*pv(53)
 v(993) = pv(8)*pv(53)
 v(994) = pv(9)*pv(53)
 v(995) = pv(10)*pv(53)
 v(996) = pv(11)*pv(53)
 v(997) = pv(0)*pv(54)
 v(998) = pv(1)*pv(54)
 v(999) = pv(2)*pv(54)
 v(1000) = pv(3)*pv(54)
 v(1001) = pv(4)*pv(54)
 v(1002) = pv(5)*pv(54)
 v(1003) = pv(6)*pv(54)
 v(1004) = pv(7)*pv(54)
 v(1005) = pv(8)*pv(54)
 v(1006) = pv(9)*pv(54)
 v(1007) = pv(10)*pv(54)
 v(1008) = pv(11)*pv(54)
 v(1009) = pv(0)*pv(55)
 v(1010) = pv(1)*pv(55)
 v(1011) = pv(2)*pv(55)
 v(1012) = pv(3)*pv(55)
 v(1013) = pv(4)*pv(55)
 v(1014) = pv(5)*pv(55)
 v(1015) = pv(6)*pv(55)
 v(1016) = pv(7)*pv(55)
 v(1017) = pv(8)*pv(55)
 v(1018) = pv(9)*pv(55)
 v(1019) = pv(10)*pv(55)
 v(1020) = pv(11)*pv(55)
 v(1021) = pv(0)*pv(56)
 v(1022) = pv(1)*pv(56)
 v(1023) = pv(2)*pv(56)
 v(1024) = pv(3)*pv(56)
 v(1025) = pv(4)*pv(56)
 v(1026) = pv(5)*pv(56)
 v(1027) = pv(6)*pv(56)
 v(1028) = pv(7)*pv(56)
 v(1029) = pv(8)*pv(56)
 v(1030) = pv(9)*pv(56)
 v(1031) = pv(10)*pv(56)
 v(1032) = pv(11)*pv(56)
 v(1033) = pv(0)*pv(57)
 v(1034) = pv(1)*pv(57)
 v(1035) = pv(2)*pv(57)
 v(1036) = pv(3)*pv(57)
 v(1037) = pv(4)*pv(57)
 v(1038) = pv(5)*pv(57)
 v(1039) = pv(6)*pv(57)
 v(1040) = pv(7)*pv(57)
 v(1041) = pv(8)*pv(57)
 v(1042) = pv(9)*pv(57)
 v(1043) = pv(10)*pv(57)
 v(1044) = pv(11)*pv(57)
 v(1045) = pv(0)*pv(58)
 v(1046) = pv(1)*pv(58)
 v(1047) = pv(2)*pv(58)
 v(1048) = pv(3)*pv(58)
 v(1049) = pv(4)*pv(58)
 v(1050) = pv(5)*pv(58)
 v(1051) = pv(6)*pv(58)
 v(1052) = pv(7)*pv(58)
 v(1053) = pv(8)*pv(58)
 v(1054) = pv(9)*pv(58)
 v(1055) = pv(10)*pv(58)
 v(1056) = pv(11)*pv(58)
 v(1057) = pv(0)*pv(59)
 v(1058) = pv(1)*pv(59)
 v(1059) = pv(2)*pv(59)
 v(1060) = pv(3)*pv(59)
 v(1061) = pv(4)*pv(59)
 v(1062) = pv(5)*pv(59)
 v(1063) = pv(6)*pv(59)
 v(1064) = pv(7)*pv(59)
 v(1065) = pv(8)*pv(59)
 v(1066) = pv(9)*pv(59)
 v(1067) = pv(10)*pv(59)
 v(1068) = pv(11)*pv(59)
 v(1069) = pv(0)*pv(60)
 v(1070) = pv(1)*pv(60)
 v(1071) = pv(2)*pv(60)
 v(1072) = pv(3)*pv(60)
 v(1073) = pv(4)*pv(60)
 v(1074) = pv(5)*pv(60)
 v(1075) = pv(6)*pv(60)
 v(1076) = pv(7)*pv(60)
 v(1077) = pv(8)*pv(60)
 v(1078) = pv(9)*pv(60)
 v(1079) = pv(10)*pv(60)
 v(1080) = pv(11)*pv(60)
 v(1081) = pv(0)*pv(61)
 v(1082) = pv(1)*pv(61)
 v(1083) = pv(2)*pv(61)
 v(1084) = pv(3)*pv(61)
 v(1085) = pv(4)*pv(61)
 v(1086) = pv(5)*pv(61)
 v(1087) = pv(6)*pv(61)
 v(1088) = pv(7)*pv(61)
 v(1089) = pv(8)*pv(61)
 v(1090) = pv(9)*pv(61)
 v(1091) = pv(10)*pv(61)
 v(1092) = pv(11)*pv(61)
 v(1093) = pv(0)*pv(62)
 v(1094) = pv(1)*pv(62)
 v(1095) = pv(2)*pv(62)
 v(1096) = pv(3)*pv(62)
 v(1097) = pv(4)*pv(62)
 v(1098) = pv(5)*pv(62)
 v(1099) = pv(6)*pv(62)
 v(1100) = pv(7)*pv(62)
 v(1101) = pv(8)*pv(62)
 v(1102) = pv(9)*pv(62)
 v(1103) = pv(10)*pv(62)
 v(1104) = pv(11)*pv(62)
 v(1105) = pv(0)*pv(63)
 v(1106) = pv(1)*pv(63)
 v(1107) = pv(2)*pv(63)
 v(1108) = pv(3)*pv(63)
 v(1109) = pv(4)*pv(63)
 v(1110) = pv(5)*pv(63)
 v(1111) = pv(6)*pv(63)
 v(1112) = pv(7)*pv(63)
 v(1113) = pv(8)*pv(63)
 v(1114) = pv(9)*pv(63)
 v(1115) = pv(10)*pv(63)
 v(1116) = pv(11)*pv(63)
 v(1117) = pv(0)*pv(64)
 v(1118) = pv(1)*pv(64)
 v(1119) = pv(2)*pv(64)
 v(1120) = pv(3)*pv(64)
 v(1121) = pv(4)*pv(64)
 v(1122) = pv(5)*pv(64)
 v(1123) = pv(6)*pv(64)
 v(1124) = pv(7)*pv(64)
 v(1125) = pv(8)*pv(64)
 v(1126) = pv(9)*pv(64)
 v(1127) = pv(10)*pv(64)
 v(1128) = pv(11)*pv(64)
 v(1129) = pv(0)*pv(65)
 v(1130) = pv(1)*pv(65)
 v(1131) = pv(2)*pv(65)
 v(1132) = pv(3)*pv(65)
 v(1133) = pv(4)*pv(65)
 v(1134) = pv(5)*pv(65)
 v(1135) = pv(6)*pv(65)
 v(1136) = pv(7)*pv(65)
 v(1137) = pv(8)*pv(65)
 v(1138) = pv(9)*pv(65)
 v(1139) = pv(10)*pv(65)
 v(1140) = pv(11)*pv(65)
 v(1141) = pv(0)*pv(66)
 v(1142) = pv(1)*pv(66)
 v(1143) = pv(2)*pv(66)
 v(1144) = pv(3)*pv(66)
 v(1145) = pv(4)*pv(66)
 v(1146) = pv(5)*pv(66)
 v(1147) = pv(6)*pv(66)
 v(1148) = pv(7)*pv(66)
 v(1149) = pv(8)*pv(66)
 v(1150) = pv(9)*pv(66)
 v(1151) = pv(10)*pv(66)
 v(1152) = pv(11)*pv(66)
 v(1153) = pv(0)*pv(67)
 v(1154) = pv(1)*pv(67)
 v(1155) = pv(2)*pv(67)
 v(1156) = pv(3)*pv(67)
 v(1157) = pv(4)*pv(67)
 v(1158) = pv(5)*pv(67)
 v(1159) = pv(6)*pv(67)
 v(1160) = pv(7)*pv(67)
 v(1161) = pv(8)*pv(67)
 v(1162) = pv(9)*pv(67)
 v(1163) = pv(10)*pv(67)
 v(1164) = pv(11)*pv(67)
 v(1165) = pv(0)*pv(68)
 v(1166) = pv(1)*pv(68)
 v(1167) = pv(2)*pv(68)
 v(1168) = pv(3)*pv(68)
 v(1169) = pv(4)*pv(68)
 v(1170) = pv(5)*pv(68)
 v(1171) = pv(6)*pv(68)
 v(1172) = pv(7)*pv(68)
 v(1173) = pv(8)*pv(68)
 v(1174) = pv(9)*pv(68)
 v(1175) = pv(10)*pv(68)
 v(1176) = pv(11)*pv(68)
 v(1177) = pv(0)*pv(69)
 v(1178) = pv(1)*pv(69)
 v(1179) = pv(2)*pv(69)
 v(1180) = pv(3)*pv(69)
 v(1181) = pv(4)*pv(69)
 v(1182) = pv(5)*pv(69)
 v(1183) = pv(6)*pv(69)
 v(1184) = pv(7)*pv(69)
 v(1185) = pv(8)*pv(69)
 v(1186) = pv(9)*pv(69)
 v(1187) = pv(10)*pv(69)
 v(1188) = pv(11)*pv(69)
 v(1189) = pv(0)*pv(70)
 v(1190) = pv(1)*pv(70)
 v(1191) = pv(2)*pv(70)
 v(1192) = pv(3)*pv(70)
 v(1193) = pv(4)*pv(70)
 v(1194) = pv(5)*pv(70)
 v(1195) = pv(6)*pv(70)
 v(1196) = pv(7)*pv(70)
 v(1197) = pv(8)*pv(70)
 v(1198) = pv(9)*pv(70)
 v(1199) = pv(10)*pv(70)
 v(1200) = pv(11)*pv(70)
 v(1201) = pv(0)*pv(71)
 v(1202) = pv(1)*pv(71)
 v(1203) = pv(2)*pv(71)
 v(1204) = pv(3)*pv(71)
 v(1205) = pv(4)*pv(71)
 v(1206) = pv(5)*pv(71)
 v(1207) = pv(6)*pv(71)
 v(1208) = pv(7)*pv(71)
 v(1209) = pv(8)*pv(71)
 v(1210) = pv(9)*pv(71)
 v(1211) = pv(10)*pv(71)
 v(1212) = pv(11)*pv(71)
 v(1213) = pv(0)*pv(72)
 v(1214) = pv(1)*pv(72)
 v(1215) = pv(2)*pv(72)
 v(1216) = pv(3)*pv(72)
 v(1217) = pv(4)*pv(72)
 v(1218) = pv(5)*pv(72)
 v(1219) = pv(6)*pv(72)
 v(1220) = pv(7)*pv(72)
 v(1221) = pv(8)*pv(72)
 v(1222) = pv(9)*pv(72)
 v(1223) = pv(10)*pv(72)
 v(1224) = pv(11)*pv(72)
 v(1225) = pv(0)*pv(73)
 v(1226) = pv(1)*pv(73)
 v(1227) = pv(2)*pv(73)
 v(1228) = pv(3)*pv(73)
 v(1229) = pv(4)*pv(73)
 v(1230) = pv(5)*pv(73)
 v(1231) = pv(6)*pv(73)
 v(1232) = pv(7)*pv(73)
 v(1233) = pv(8)*pv(73)
 v(1234) = pv(9)*pv(73)
 v(1235) = pv(10)*pv(73)
 v(1236) = pv(11)*pv(73)
 v(1237) = pv(0)*pv(74)
 v(1238) = pv(1)*pv(74)
 v(1239) = pv(2)*pv(74)
 v(1240) = pv(3)*pv(74)
 v(1241) = pv(4)*pv(74)
 v(1242) = pv(5)*pv(74)
 v(1243) = pv(6)*pv(74)
 v(1244) = pv(7)*pv(74)
 v(1245) = pv(8)*pv(74)
 v(1246) = pv(9)*pv(74)
 v(1247) = pv(10)*pv(74)
 v(1248) = pv(11)*pv(74)
 v(1249) = pv(0)*pv(75)
 v(1250) = pv(1)*pv(75)
 v(1251) = pv(2)*pv(75)
 v(1252) = pv(3)*pv(75)
 v(1253) = pv(4)*pv(75)
 v(1254) = pv(5)*pv(75)
 v(1255) = pv(6)*pv(75)
 v(1256) = pv(7)*pv(75)
 v(1257) = pv(8)*pv(75)
 v(1258) = pv(9)*pv(75)
 v(1259) = pv(10)*pv(75)
 v(1260) = pv(11)*pv(75)
 v(1261) = pv(0)*pv(76)
 v(1262) = pv(1)*pv(76)
 v(1263) = pv(2)*pv(76)
 v(1264) = pv(3)*pv(76)
 v(1265) = pv(4)*pv(76)
 v(1266) = pv(5)*pv(76)
 v(1267) = pv(6)*pv(76)
 v(1268) = pv(7)*pv(76)
 v(1269) = pv(8)*pv(76)
 v(1270) = pv(9)*pv(76)
 v(1271) = pv(10)*pv(76)
 v(1272) = pv(11)*pv(76)
 v(1273) = pv(0)*pv(77)
 v(1274) = pv(1)*pv(77)
 v(1275) = pv(2)*pv(77)
 v(1276) = pv(3)*pv(77)
 v(1277) = pv(4)*pv(77)
 v(1278) = pv(5)*pv(77)
 v(1279) = pv(6)*pv(77)
 v(1280) = pv(7)*pv(77)
 v(1281) = pv(8)*pv(77)
 v(1282) = pv(9)*pv(77)
 v(1283) = pv(10)*pv(77)
 v(1284) = pv(11)*pv(77)
 v(1285) = pv(0)*pv(78)
 v(1286) = pv(1)*pv(78)
 v(1287) = pv(2)*pv(78)
 v(1288) = pv(3)*pv(78)
 v(1289) = pv(4)*pv(78)
 v(1290) = pv(5)*pv(78)
 v(1291) = pv(6)*pv(78)
 v(1292) = pv(7)*pv(78)
 v(1293) = pv(8)*pv(78)
 v(1294) = pv(9)*pv(78)
 v(1295) = pv(10)*pv(78)
 v(1296) = pv(11)*pv(78)
 v(1297) = pv(0)*pv(79)
 v(1298) = pv(1)*pv(79)
 v(1299) = pv(2)*pv(79)
 v(1300) = pv(3)*pv(79)
 v(1301) = pv(4)*pv(79)
 v(1302) = pv(5)*pv(79)
 v(1303) = pv(6)*pv(79)
 v(1304) = pv(7)*pv(79)
 v(1305) = pv(8)*pv(79)
 v(1306) = pv(9)*pv(79)
 v(1307) = pv(10)*pv(79)
 v(1308) = pv(11)*pv(79)
 v(1309) = pv(0)*pv(80)
 v(1310) = pv(1)*pv(80)
 v(1311) = pv(2)*pv(80)
 v(1312) = pv(3)*pv(80)
 v(1313) = pv(4)*pv(80)
 v(1314) = pv(5)*pv(80)
 v(1315) = pv(6)*pv(80)
 v(1316) = pv(7)*pv(80)
 v(1317) = pv(8)*pv(80)
 v(1318) = pv(9)*pv(80)
 v(1319) = pv(10)*pv(80)
 v(1320) = pv(11)*pv(80)
 v(1321) = pv(0)*pv(81)
 v(1322) = pv(1)*pv(81)
 v(1323) = pv(2)*pv(81)
 v(1324) = pv(3)*pv(81)
 v(1325) = pv(4)*pv(81)
 v(1326) = pv(5)*pv(81)
 v(1327) = pv(6)*pv(81)
 v(1328) = pv(7)*pv(81)
 v(1329) = pv(8)*pv(81)
 v(1330) = pv(9)*pv(81)
 v(1331) = pv(10)*pv(81)
 v(1332) = pv(11)*pv(81)
 v(1333) = pv(0)*pv(82)
 v(1334) = pv(1)*pv(82)
 v(1335) = pv(2)*pv(82)
 v(1336) = pv(3)*pv(82)
 v(1337) = pv(4)*pv(82)
 v(1338) = pv(5)*pv(82)
 v(1339) = pv(6)*pv(82)
 v(1340) = pv(7)*pv(82)
 v(1341) = pv(8)*pv(82)
 v(1342) = pv(9)*pv(82)
 v(1343) = pv(10)*pv(82)
 v(1344) = pv(11)*pv(82)
 v(1345) = pv(0)*pv(83)
 v(1346) = pv(1)*pv(83)
 v(1347) = pv(2)*pv(83)
 v(1348) = pv(3)*pv(83)
 v(1349) = pv(4)*pv(83)
 v(1350) = pv(5)*pv(83)
 v(1351) = pv(6)*pv(83)
 v(1352) = pv(7)*pv(83)
 v(1353) = pv(8)*pv(83)
 v(1354) = pv(9)*pv(83)
 v(1355) = pv(10)*pv(83)
 v(1356) = pv(11)*pv(83)
 v(1357) = pv(0)*pv(84)
 v(1358) = pv(1)*pv(84)
 v(1359) = pv(2)*pv(84)
 v(1360) = pv(3)*pv(84)
 v(1361) = pv(4)*pv(84)
 v(1362) = pv(5)*pv(84)
 v(1363) = pv(6)*pv(84)
 v(1364) = pv(7)*pv(84)
 v(1365) = pv(8)*pv(84)
 v(1366) = pv(9)*pv(84)
 v(1367) = pv(10)*pv(84)
 v(1368) = pv(11)*pv(84)
 v(1369) = pv(0)*pv(85)
 v(1370) = pv(1)*pv(85)
 v(1371) = pv(2)*pv(85)
 v(1372) = pv(3)*pv(85)
 v(1373) = pv(4)*pv(85)
 v(1374) = pv(5)*pv(85)
 v(1375) = pv(6)*pv(85)
 v(1376) = pv(7)*pv(85)
 v(1377) = pv(8)*pv(85)
 v(1378) = pv(9)*pv(85)
 v(1379) = pv(10)*pv(85)
 v(1380) = pv(11)*pv(85)
 v(1381) = pv(0)*pv(86)
 v(1382) = pv(1)*pv(86)
 v(1383) = pv(2)*pv(86)
 v(1384) = pv(3)*pv(86)
 v(1385) = pv(4)*pv(86)
 v(1386) = pv(5)*pv(86)
 v(1387) = pv(6)*pv(86)
 v(1388) = pv(7)*pv(86)
 v(1389) = pv(8)*pv(86)
 v(1390) = pv(9)*pv(86)
 v(1391) = pv(10)*pv(86)
 v(1392) = pv(11)*pv(86)
 v(1393) = pv(0)*pv(87)
 v(1394) = pv(1)*pv(87)
 v(1395) = pv(2)*pv(87)
 v(1396) = pv(3)*pv(87)
 v(1397) = pv(4)*pv(87)
 v(1398) = pv(5)*pv(87)
 v(1399) = pv(6)*pv(87)
 v(1400) = pv(7)*pv(87)
 v(1401) = pv(8)*pv(87)
 v(1402) = pv(9)*pv(87)
 v(1403) = pv(10)*pv(87)
 v(1404) = pv(11)*pv(87)
 v(1405:2611) = pv(414:1620)
endif
if (6.le.mxd) then
 stop 'mg4411: degree 6 not implemented'
endif
return
END SUBROUTINE mg4411_secs