SUBROUTINE mg621_secs (mxd, r, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: v(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: pv(0:size(v)-1) !! an upper bound size
if (size(r).ne.nk*nk.or.size(v).ne.mg621_nsc(mxd)) then
 stop 'mg621_secs: bad dimensions'
endif
call mg621_isecs (mxd, r, pv)
v(0) = 1
if (2.le.mxd) then
 v(1:4) = pv(0:3)
endif
if (3.le.mxd) then
 v(5:28) = pv(4:27)
endif
if (4.le.mxd) then
 v(29) = pv(0)*pv(0)
 v(30) = pv(0)*pv(1)
 v(31) = pv(1)*pv(1)
 v(32) = pv(0)*pv(2)
 v(33) = pv(1)*pv(2)
 v(34) = pv(2)*pv(2)
 v(35) = pv(0)*pv(3)
 v(36) = pv(1)*pv(3)
 v(37) = pv(2)*pv(3)
 v(38:135) = pv(28:125)
endif
if (5.le.mxd) then
 v(136) = pv(0)*pv(4)
 v(137) = pv(1)*pv(4)
 v(138) = pv(2)*pv(4)
 v(139) = pv(3)*pv(4)
 v(140) = pv(0)*pv(5)
 v(141) = pv(1)*pv(5)
 v(142) = pv(2)*pv(5)
 v(143) = pv(3)*pv(5)
 v(144) = pv(0)*pv(6)
 v(145) = pv(1)*pv(6)
 v(146) = pv(2)*pv(6)
 v(147) = pv(3)*pv(6)
 v(148) = pv(0)*pv(7)
 v(149) = pv(1)*pv(7)
 v(150) = pv(2)*pv(7)
 v(151) = pv(3)*pv(7)
 v(152) = pv(0)*pv(8)
 v(153) = pv(1)*pv(8)
 v(154) = pv(2)*pv(8)
 v(155) = pv(0)*pv(9)
 v(156) = pv(1)*pv(9)
 v(157) = pv(2)*pv(9)
 v(158) = pv(3)*pv(9)
 v(159) = pv(0)*pv(10)
 v(160) = pv(1)*pv(10)
 v(161) = pv(2)*pv(10)
 v(162) = pv(3)*pv(10)
 v(163) = pv(0)*pv(11)
 v(164) = pv(1)*pv(11)
 v(165) = pv(2)*pv(11)
 v(166) = pv(0)*pv(12)
 v(167) = pv(1)*pv(12)
 v(168) = pv(2)*pv(12)
 v(169) = pv(3)*pv(12)
 v(170) = pv(0)*pv(13)
 v(171) = pv(1)*pv(13)
 v(172) = pv(2)*pv(13)
 v(173) = pv(3)*pv(13)
 v(174) = pv(0)*pv(14)
 v(175) = pv(1)*pv(14)
 v(176) = pv(2)*pv(14)
 v(177) = pv(3)*pv(14)
 v(178) = pv(0)*pv(15)
 v(179) = pv(1)*pv(15)
 v(180) = pv(2)*pv(15)
 v(181) = pv(3)*pv(15)
 v(182) = pv(0)*pv(16)
 v(183) = pv(1)*pv(16)
 v(184) = pv(2)*pv(16)
 v(185) = pv(3)*pv(16)
 v(186) = pv(0)*pv(17)
 v(187) = pv(1)*pv(17)
 v(188) = pv(2)*pv(17)
 v(189) = pv(3)*pv(17)
 v(190) = pv(0)*pv(18)
 v(191) = pv(1)*pv(18)
 v(192) = pv(2)*pv(18)
 v(193) = pv(0)*pv(19)
 v(194) = pv(1)*pv(19)
 v(195) = pv(2)*pv(19)
 v(196) = pv(3)*pv(19)
 v(197) = pv(0)*pv(20)
 v(198) = pv(1)*pv(20)
 v(199) = pv(2)*pv(20)
 v(200) = pv(3)*pv(20)
 v(201) = pv(0)*pv(21)
 v(202) = pv(1)*pv(21)
 v(203) = pv(2)*pv(21)
 v(204) = pv(3)*pv(21)
 v(205) = pv(0)*pv(22)
 v(206) = pv(1)*pv(22)
 v(207) = pv(2)*pv(22)
 v(208) = pv(3)*pv(22)
 v(209) = pv(0)*pv(23)
 v(210) = pv(1)*pv(23)
 v(211) = pv(2)*pv(23)
 v(212) = pv(3)*pv(23)
 v(213) = pv(0)*pv(24)
 v(214) = pv(1)*pv(24)
 v(215) = pv(2)*pv(24)
 v(216) = pv(3)*pv(24)
 v(217) = pv(0)*pv(25)
 v(218) = pv(1)*pv(25)
 v(219) = pv(2)*pv(25)
 v(220) = pv(0)*pv(26)
 v(221) = pv(1)*pv(26)
 v(222) = pv(2)*pv(26)
 v(223) = pv(0)*pv(27)
 v(224) = pv(1)*pv(27)
 v(225) = pv(2)*pv(27)
 v(226:586) = pv(126:486)
endif
if (6.le.mxd) then
 v(587) = pv(0)*pv(0)*pv(0)
 v(588) = pv(0)*pv(0)*pv(1)
 v(589) = pv(0)*pv(1)*pv(1)
 v(590) = pv(1)*pv(1)*pv(1)
 v(591) = pv(0)*pv(0)*pv(2)
 v(592) = pv(0)*pv(1)*pv(2)
 v(593) = pv(1)*pv(1)*pv(2)
 v(594) = pv(0)*pv(2)*pv(2)
 v(595) = pv(1)*pv(2)*pv(2)
 v(596) = pv(2)*pv(2)*pv(2)
 v(597) = pv(0)*pv(1)*pv(3)
 v(598) = pv(1)*pv(1)*pv(3)
 v(599) = pv(1)*pv(2)*pv(3)
 v(600) = pv(4)*pv(4)
 v(601) = pv(4)*pv(5)
 v(602) = pv(5)*pv(5)
 v(603) = pv(4)*pv(6)
 v(604) = pv(5)*pv(6)
 v(605) = pv(6)*pv(6)
 v(606) = pv(4)*pv(7)
 v(607) = pv(5)*pv(7)
 v(608) = pv(6)*pv(7)
 v(609) = pv(7)*pv(7)
 v(610) = pv(4)*pv(8)
 v(611) = pv(5)*pv(8)
 v(612) = pv(6)*pv(8)
 v(613) = pv(7)*pv(8)
 v(614) = pv(4)*pv(9)
 v(615) = pv(5)*pv(9)
 v(616) = pv(6)*pv(9)
 v(617) = pv(7)*pv(9)
 v(618) = pv(8)*pv(9)
 v(619) = pv(9)*pv(9)
 v(620) = pv(4)*pv(10)
 v(621) = pv(5)*pv(10)
 v(622) = pv(6)*pv(10)
 v(623) = pv(7)*pv(10)
 v(624) = pv(8)*pv(10)
 v(625) = pv(9)*pv(10)
 v(626) = pv(10)*pv(10)
 v(627) = pv(4)*pv(11)
 v(628) = pv(5)*pv(11)
 v(629) = pv(6)*pv(11)
 v(630) = pv(7)*pv(11)
 v(631) = pv(9)*pv(11)
 v(632) = pv(10)*pv(11)
 v(633) = pv(4)*pv(12)
 v(634) = pv(5)*pv(12)
 v(635) = pv(6)*pv(12)
 v(636) = pv(7)*pv(12)
 v(637) = pv(8)*pv(12)
 v(638) = pv(9)*pv(12)
 v(639) = pv(10)*pv(12)
 v(640) = pv(11)*pv(12)
 v(641) = pv(12)*pv(12)
 v(642) = pv(4)*pv(13)
 v(643) = pv(5)*pv(13)
 v(644) = pv(6)*pv(13)
 v(645) = pv(7)*pv(13)
 v(646) = pv(8)*pv(13)
 v(647) = pv(9)*pv(13)
 v(648) = pv(10)*pv(13)
 v(649) = pv(11)*pv(13)
 v(650) = pv(12)*pv(13)
 v(651) = pv(13)*pv(13)
 v(652) = pv(4)*pv(14)
 v(653) = pv(5)*pv(14)
 v(654) = pv(6)*pv(14)
 v(655) = pv(7)*pv(14)
 v(656) = pv(8)*pv(14)
 v(657) = pv(9)*pv(14)
 v(658) = pv(10)*pv(14)
 v(659) = pv(11)*pv(14)
 v(660) = pv(12)*pv(14)
 v(661) = pv(13)*pv(14)
 v(662) = pv(14)*pv(14)
 v(663) = pv(4)*pv(15)
 v(664) = pv(5)*pv(15)
 v(665) = pv(6)*pv(15)
 v(666) = pv(7)*pv(15)
 v(667) = pv(8)*pv(15)
 v(668) = pv(9)*pv(15)
 v(669) = pv(10)*pv(15)
 v(670) = pv(11)*pv(15)
 v(671) = pv(12)*pv(15)
 v(672) = pv(13)*pv(15)
 v(673) = pv(14)*pv(15)
 v(674) = pv(15)*pv(15)
 v(675) = pv(4)*pv(16)
 v(676) = pv(5)*pv(16)
 v(677) = pv(6)*pv(16)
 v(678) = pv(7)*pv(16)
 v(679) = pv(8)*pv(16)
 v(680) = pv(9)*pv(16)
 v(681) = pv(10)*pv(16)
 v(682) = pv(11)*pv(16)
 v(683) = pv(12)*pv(16)
 v(684) = pv(13)*pv(16)
 v(685) = pv(14)*pv(16)
 v(686) = pv(15)*pv(16)
 v(687) = pv(16)*pv(16)
 v(688) = pv(4)*pv(17)
 v(689) = pv(5)*pv(17)
 v(690) = pv(6)*pv(17)
 v(691) = pv(7)*pv(17)
 v(692) = pv(8)*pv(17)
 v(693) = pv(9)*pv(17)
 v(694) = pv(10)*pv(17)
 v(695) = pv(11)*pv(17)
 v(696) = pv(12)*pv(17)
 v(697) = pv(13)*pv(17)
 v(698) = pv(14)*pv(17)
 v(699) = pv(15)*pv(17)
 v(700) = pv(16)*pv(17)
 v(701) = pv(17)*pv(17)
 v(702) = pv(4)*pv(18)
 v(703) = pv(5)*pv(18)
 v(704) = pv(6)*pv(18)
 v(705) = pv(7)*pv(18)
 v(706) = pv(9)*pv(18)
 v(707) = pv(10)*pv(18)
 v(708) = pv(12)*pv(18)
 v(709) = pv(13)*pv(18)
 v(710) = pv(14)*pv(18)
 v(711) = pv(15)*pv(18)
 v(712) = pv(16)*pv(18)
 v(713) = pv(17)*pv(18)
 v(714) = pv(4)*pv(19)
 v(715) = pv(5)*pv(19)
 v(716) = pv(6)*pv(19)
 v(717) = pv(7)*pv(19)
 v(718) = pv(8)*pv(19)
 v(719) = pv(9)*pv(19)
 v(720) = pv(10)*pv(19)
 v(721) = pv(11)*pv(19)
 v(722) = pv(12)*pv(19)
 v(723) = pv(13)*pv(19)
 v(724) = pv(14)*pv(19)
 v(725) = pv(15)*pv(19)
 v(726) = pv(16)*pv(19)
 v(727) = pv(17)*pv(19)
 v(728) = pv(18)*pv(19)
 v(729) = pv(19)*pv(19)
 v(730) = pv(4)*pv(20)
 v(731) = pv(5)*pv(20)
 v(732) = pv(6)*pv(20)
 v(733) = pv(7)*pv(20)
 v(734) = pv(8)*pv(20)
 v(735) = pv(9)*pv(20)
 v(736) = pv(10)*pv(20)
 v(737) = pv(11)*pv(20)
 v(738) = pv(12)*pv(20)
 v(739) = pv(13)*pv(20)
 v(740) = pv(14)*pv(20)
 v(741) = pv(15)*pv(20)
 v(742) = pv(16)*pv(20)
 v(743) = pv(17)*pv(20)
 v(744) = pv(18)*pv(20)
 v(745) = pv(19)*pv(20)
 v(746) = pv(20)*pv(20)
 v(747) = pv(4)*pv(21)
 v(748) = pv(5)*pv(21)
 v(749) = pv(6)*pv(21)
 v(750) = pv(7)*pv(21)
 v(751) = pv(8)*pv(21)
 v(752) = pv(9)*pv(21)
 v(753) = pv(10)*pv(21)
 v(754) = pv(11)*pv(21)
 v(755) = pv(12)*pv(21)
 v(756) = pv(13)*pv(21)
 v(757) = pv(14)*pv(21)
 v(758) = pv(15)*pv(21)
 v(759) = pv(16)*pv(21)
 v(760) = pv(17)*pv(21)
 v(761) = pv(18)*pv(21)
 v(762) = pv(19)*pv(21)
 v(763) = pv(20)*pv(21)
 v(764) = pv(21)*pv(21)
 v(765) = pv(4)*pv(22)
 v(766) = pv(5)*pv(22)
 v(767) = pv(6)*pv(22)
 v(768) = pv(7)*pv(22)
 v(769) = pv(8)*pv(22)
 v(770) = pv(9)*pv(22)
 v(771) = pv(10)*pv(22)
 v(772) = pv(11)*pv(22)
 v(773) = pv(12)*pv(22)
 v(774) = pv(13)*pv(22)
 v(775) = pv(14)*pv(22)
 v(776) = pv(15)*pv(22)
 v(777) = pv(16)*pv(22)
 v(778) = pv(17)*pv(22)
 v(779) = pv(18)*pv(22)
 v(780) = pv(19)*pv(22)
 v(781) = pv(20)*pv(22)
 v(782) = pv(21)*pv(22)
 v(783) = pv(22)*pv(22)
 v(784) = pv(4)*pv(23)
 v(785) = pv(5)*pv(23)
 v(786) = pv(6)*pv(23)
 v(787) = pv(7)*pv(23)
 v(788) = pv(8)*pv(23)
 v(789) = pv(9)*pv(23)
 v(790) = pv(10)*pv(23)
 v(791) = pv(11)*pv(23)
 v(792) = pv(12)*pv(23)
 v(793) = pv(13)*pv(23)
 v(794) = pv(14)*pv(23)
 v(795) = pv(15)*pv(23)
 v(796) = pv(16)*pv(23)
 v(797) = pv(17)*pv(23)
 v(798) = pv(18)*pv(23)
 v(799) = pv(19)*pv(23)
 v(800) = pv(20)*pv(23)
 v(801) = pv(21)*pv(23)
 v(802) = pv(22)*pv(23)
 v(803) = pv(23)*pv(23)
 v(804) = pv(4)*pv(24)
 v(805) = pv(5)*pv(24)
 v(806) = pv(6)*pv(24)
 v(807) = pv(7)*pv(24)
 v(808) = pv(8)*pv(24)
 v(809) = pv(9)*pv(24)
 v(810) = pv(10)*pv(24)
 v(811) = pv(11)*pv(24)
 v(812) = pv(12)*pv(24)
 v(813) = pv(13)*pv(24)
 v(814) = pv(14)*pv(24)
 v(815) = pv(15)*pv(24)
 v(816) = pv(16)*pv(24)
 v(817) = pv(17)*pv(24)
 v(818) = pv(18)*pv(24)
 v(819) = pv(19)*pv(24)
 v(820) = pv(20)*pv(24)
 v(821) = pv(21)*pv(24)
 v(822) = pv(22)*pv(24)
 v(823) = pv(23)*pv(24)
 v(824) = pv(24)*pv(24)
 v(825) = pv(4)*pv(25)
 v(826) = pv(5)*pv(25)
 v(827) = pv(6)*pv(25)
 v(828) = pv(7)*pv(25)
 v(829) = pv(8)*pv(25)
 v(830) = pv(9)*pv(25)
 v(831) = pv(10)*pv(25)
 v(832) = pv(12)*pv(25)
 v(833) = pv(13)*pv(25)
 v(834) = pv(14)*pv(25)
 v(835) = pv(15)*pv(25)
 v(836) = pv(16)*pv(25)
 v(837) = pv(17)*pv(25)
 v(838) = pv(19)*pv(25)
 v(839) = pv(20)*pv(25)
 v(840) = pv(21)*pv(25)
 v(841) = pv(22)*pv(25)
 v(842) = pv(23)*pv(25)
 v(843) = pv(24)*pv(25)
 v(844) = pv(4)*pv(26)
 v(845) = pv(5)*pv(26)
 v(846) = pv(6)*pv(26)
 v(847) = pv(7)*pv(26)
 v(848) = pv(9)*pv(26)
 v(849) = pv(10)*pv(26)
 v(850) = pv(12)*pv(26)
 v(851) = pv(13)*pv(26)
 v(852) = pv(14)*pv(26)
 v(853) = pv(15)*pv(26)
 v(854) = pv(16)*pv(26)
 v(855) = pv(17)*pv(26)
 v(856) = pv(19)*pv(26)
 v(857) = pv(20)*pv(26)
 v(858) = pv(21)*pv(26)
 v(859) = pv(22)*pv(26)
 v(860) = pv(23)*pv(26)
 v(861) = pv(24)*pv(26)
 v(862) = pv(4)*pv(27)
 v(863) = pv(5)*pv(27)
 v(864) = pv(6)*pv(27)
 v(865) = pv(7)*pv(27)
 v(866) = pv(8)*pv(27)
 v(867) = pv(9)*pv(27)
 v(868) = pv(10)*pv(27)
 v(869) = pv(12)*pv(27)
 v(870) = pv(13)*pv(27)
 v(871) = pv(14)*pv(27)
 v(872) = pv(15)*pv(27)
 v(873) = pv(16)*pv(27)
 v(874) = pv(17)*pv(27)
 v(875) = pv(18)*pv(27)
 v(876) = pv(19)*pv(27)
 v(877) = pv(20)*pv(27)
 v(878) = pv(21)*pv(27)
 v(879) = pv(22)*pv(27)
 v(880) = pv(23)*pv(27)
 v(881) = pv(24)*pv(27)
 v(882) = pv(0)*pv(28)
 v(883) = pv(1)*pv(28)
 v(884) = pv(2)*pv(28)
 v(885) = pv(3)*pv(28)
 v(886) = pv(0)*pv(29)
 v(887) = pv(1)*pv(29)
 v(888) = pv(2)*pv(29)
 v(889) = pv(3)*pv(29)
 v(890) = pv(0)*pv(30)
 v(891) = pv(1)*pv(30)
 v(892) = pv(2)*pv(30)
 v(893) = pv(3)*pv(30)
 v(894) = pv(0)*pv(31)
 v(895) = pv(1)*pv(31)
 v(896) = pv(2)*pv(31)
 v(897) = pv(3)*pv(31)
 v(898) = pv(0)*pv(32)
 v(899) = pv(1)*pv(32)
 v(900) = pv(2)*pv(32)
 v(901) = pv(3)*pv(32)
 v(902) = pv(0)*pv(33)
 v(903) = pv(1)*pv(33)
 v(904) = pv(2)*pv(33)
 v(905) = pv(3)*pv(33)
 v(906) = pv(0)*pv(34)
 v(907) = pv(1)*pv(34)
 v(908) = pv(2)*pv(34)
 v(909) = pv(3)*pv(34)
 v(910) = pv(0)*pv(35)
 v(911) = pv(1)*pv(35)
 v(912) = pv(2)*pv(35)
 v(913) = pv(3)*pv(35)
 v(914) = pv(0)*pv(36)
 v(915) = pv(1)*pv(36)
 v(916) = pv(2)*pv(36)
 v(917) = pv(3)*pv(36)
 v(918) = pv(0)*pv(37)
 v(919) = pv(1)*pv(37)
 v(920) = pv(2)*pv(37)
 v(921) = pv(3)*pv(37)
 v(922) = pv(0)*pv(38)
 v(923) = pv(1)*pv(38)
 v(924) = pv(2)*pv(38)
 v(925) = pv(3)*pv(38)
 v(926) = pv(0)*pv(39)
 v(927) = pv(1)*pv(39)
 v(928) = pv(2)*pv(39)
 v(929) = pv(3)*pv(39)
 v(930) = pv(0)*pv(40)
 v(931) = pv(1)*pv(40)
 v(932) = pv(2)*pv(40)
 v(933) = pv(3)*pv(40)
 v(934) = pv(0)*pv(41)
 v(935) = pv(1)*pv(41)
 v(936) = pv(2)*pv(41)
 v(937) = pv(3)*pv(41)
 v(938) = pv(0)*pv(42)
 v(939) = pv(1)*pv(42)
 v(940) = pv(2)*pv(42)
 v(941) = pv(3)*pv(42)
 v(942) = pv(0)*pv(43)
 v(943) = pv(1)*pv(43)
 v(944) = pv(2)*pv(43)
 v(945) = pv(0)*pv(44)
 v(946) = pv(1)*pv(44)
 v(947) = pv(2)*pv(44)
 v(948) = pv(0)*pv(45)
 v(949) = pv(1)*pv(45)
 v(950) = pv(2)*pv(45)
 v(951) = pv(0)*pv(46)
 v(952) = pv(1)*pv(46)
 v(953) = pv(2)*pv(46)
 v(954) = pv(0)*pv(47)
 v(955) = pv(1)*pv(47)
 v(956) = pv(2)*pv(47)
 v(957) = pv(3)*pv(47)
 v(958) = pv(0)*pv(48)
 v(959) = pv(1)*pv(48)
 v(960) = pv(2)*pv(48)
 v(961) = pv(3)*pv(48)
 v(962) = pv(0)*pv(49)
 v(963) = pv(1)*pv(49)
 v(964) = pv(2)*pv(49)
 v(965) = pv(3)*pv(49)
 v(966) = pv(0)*pv(50)
 v(967) = pv(1)*pv(50)
 v(968) = pv(2)*pv(50)
 v(969) = pv(3)*pv(50)
 v(970) = pv(0)*pv(51)
 v(971) = pv(1)*pv(51)
 v(972) = pv(2)*pv(51)
 v(973) = pv(3)*pv(51)
 v(974) = pv(0)*pv(52)
 v(975) = pv(1)*pv(52)
 v(976) = pv(2)*pv(52)
 v(977) = pv(3)*pv(52)
 v(978) = pv(0)*pv(53)
 v(979) = pv(1)*pv(53)
 v(980) = pv(2)*pv(53)
 v(981) = pv(3)*pv(53)
 v(982) = pv(0)*pv(54)
 v(983) = pv(1)*pv(54)
 v(984) = pv(2)*pv(54)
 v(985) = pv(0)*pv(55)
 v(986) = pv(1)*pv(55)
 v(987) = pv(2)*pv(55)
 v(988) = pv(3)*pv(55)
 v(989) = pv(0)*pv(56)
 v(990) = pv(1)*pv(56)
 v(991) = pv(2)*pv(56)
 v(992) = pv(3)*pv(56)
 v(993) = pv(0)*pv(57)
 v(994) = pv(1)*pv(57)
 v(995) = pv(2)*pv(57)
 v(996) = pv(0)*pv(58)
 v(997) = pv(1)*pv(58)
 v(998) = pv(2)*pv(58)
 v(999) = pv(3)*pv(58)
 v(1000) = pv(0)*pv(59)
 v(1001) = pv(1)*pv(59)
 v(1002) = pv(2)*pv(59)
 v(1003) = pv(3)*pv(59)
 v(1004) = pv(0)*pv(60)
 v(1005) = pv(1)*pv(60)
 v(1006) = pv(2)*pv(60)
 v(1007) = pv(3)*pv(60)
 v(1008) = pv(0)*pv(61)
 v(1009) = pv(1)*pv(61)
 v(1010) = pv(2)*pv(61)
 v(1011) = pv(3)*pv(61)
 v(1012) = pv(0)*pv(62)
 v(1013) = pv(1)*pv(62)
 v(1014) = pv(2)*pv(62)
 v(1015) = pv(3)*pv(62)
 v(1016) = pv(0)*pv(63)
 v(1017) = pv(1)*pv(63)
 v(1018) = pv(2)*pv(63)
 v(1019) = pv(3)*pv(63)
 v(1020) = pv(0)*pv(64)
 v(1021) = pv(1)*pv(64)
 v(1022) = pv(2)*pv(64)
 v(1023) = pv(0)*pv(65)
 v(1024) = pv(1)*pv(65)
 v(1025) = pv(2)*pv(65)
 v(1026) = pv(3)*pv(65)
 v(1027) = pv(0)*pv(66)
 v(1028) = pv(1)*pv(66)
 v(1029) = pv(2)*pv(66)
 v(1030) = pv(3)*pv(66)
 v(1031) = pv(0)*pv(67)
 v(1032) = pv(1)*pv(67)
 v(1033) = pv(2)*pv(67)
 v(1034) = pv(3)*pv(67)
 v(1035) = pv(0)*pv(68)
 v(1036) = pv(1)*pv(68)
 v(1037) = pv(2)*pv(68)
 v(1038) = pv(3)*pv(68)
 v(1039) = pv(0)*pv(69)
 v(1040) = pv(1)*pv(69)
 v(1041) = pv(2)*pv(69)
 v(1042) = pv(3)*pv(69)
 v(1043) = pv(0)*pv(70)
 v(1044) = pv(1)*pv(70)
 v(1045) = pv(2)*pv(70)
 v(1046) = pv(3)*pv(70)
 v(1047) = pv(0)*pv(71)
 v(1048) = pv(1)*pv(71)
 v(1049) = pv(2)*pv(71)
 v(1050) = pv(3)*pv(71)
 v(1051) = pv(0)*pv(72)
 v(1052) = pv(1)*pv(72)
 v(1053) = pv(2)*pv(72)
 v(1054) = pv(3)*pv(72)
 v(1055) = pv(0)*pv(73)
 v(1056) = pv(1)*pv(73)
 v(1057) = pv(2)*pv(73)
 v(1058) = pv(3)*pv(73)
 v(1059) = pv(0)*pv(74)
 v(1060) = pv(1)*pv(74)
 v(1061) = pv(2)*pv(74)
 v(1062) = pv(3)*pv(74)
 v(1063) = pv(0)*pv(75)
 v(1064) = pv(1)*pv(75)
 v(1065) = pv(2)*pv(75)
 v(1066) = pv(3)*pv(75)
 v(1067) = pv(0)*pv(76)
 v(1068) = pv(1)*pv(76)
 v(1069) = pv(2)*pv(76)
 v(1070) = pv(3)*pv(76)
 v(1071) = pv(0)*pv(77)
 v(1072) = pv(1)*pv(77)
 v(1073) = pv(2)*pv(77)
 v(1074) = pv(3)*pv(77)
 v(1075) = pv(0)*pv(78)
 v(1076) = pv(1)*pv(78)
 v(1077) = pv(2)*pv(78)
 v(1078) = pv(3)*pv(78)
 v(1079) = pv(0)*pv(79)
 v(1080) = pv(1)*pv(79)
 v(1081) = pv(2)*pv(79)
 v(1082) = pv(3)*pv(79)
 v(1083) = pv(0)*pv(80)
 v(1084) = pv(1)*pv(80)
 v(1085) = pv(2)*pv(80)
 v(1086) = pv(3)*pv(80)
 v(1087) = pv(0)*pv(81)
 v(1088) = pv(1)*pv(81)
 v(1089) = pv(2)*pv(81)
 v(1090) = pv(3)*pv(81)
 v(1091) = pv(0)*pv(82)
 v(1092) = pv(1)*pv(82)
 v(1093) = pv(2)*pv(82)
 v(1094) = pv(0)*pv(83)
 v(1095) = pv(1)*pv(83)
 v(1096) = pv(2)*pv(83)
 v(1097) = pv(0)*pv(84)
 v(1098) = pv(1)*pv(84)
 v(1099) = pv(2)*pv(84)
 v(1100) = pv(3)*pv(84)
 v(1101) = pv(0)*pv(85)
 v(1102) = pv(1)*pv(85)
 v(1103) = pv(2)*pv(85)
 v(1104) = pv(3)*pv(85)
 v(1105) = pv(0)*pv(86)
 v(1106) = pv(1)*pv(86)
 v(1107) = pv(2)*pv(86)
 v(1108) = pv(3)*pv(86)
 v(1109) = pv(0)*pv(87)
 v(1110) = pv(1)*pv(87)
 v(1111) = pv(2)*pv(87)
 v(1112) = pv(3)*pv(87)
 v(1113) = pv(0)*pv(88)
 v(1114) = pv(1)*pv(88)
 v(1115) = pv(2)*pv(88)
 v(1116) = pv(0)*pv(89)
 v(1117) = pv(1)*pv(89)
 v(1118) = pv(2)*pv(89)
 v(1119) = pv(3)*pv(89)
 v(1120) = pv(0)*pv(90)
 v(1121) = pv(1)*pv(90)
 v(1122) = pv(2)*pv(90)
 v(1123) = pv(3)*pv(90)
 v(1124) = pv(0)*pv(91)
 v(1125) = pv(1)*pv(91)
 v(1126) = pv(2)*pv(91)
 v(1127) = pv(3)*pv(91)
 v(1128) = pv(0)*pv(92)
 v(1129) = pv(1)*pv(92)
 v(1130) = pv(2)*pv(92)
 v(1131) = pv(3)*pv(92)
 v(1132) = pv(0)*pv(93)
 v(1133) = pv(1)*pv(93)
 v(1134) = pv(2)*pv(93)
 v(1135) = pv(3)*pv(93)
 v(1136) = pv(0)*pv(94)
 v(1137) = pv(1)*pv(94)
 v(1138) = pv(2)*pv(94)
 v(1139) = pv(3)*pv(94)
 v(1140) = pv(0)*pv(95)
 v(1141) = pv(1)*pv(95)
 v(1142) = pv(2)*pv(95)
 v(1143) = pv(3)*pv(95)
 v(1144) = pv(0)*pv(96)
 v(1145) = pv(1)*pv(96)
 v(1146) = pv(2)*pv(96)
 v(1147) = pv(3)*pv(96)
 v(1148) = pv(0)*pv(97)
 v(1149) = pv(1)*pv(97)
 v(1150) = pv(2)*pv(97)
 v(1151) = pv(3)*pv(97)
 v(1152) = pv(0)*pv(98)
 v(1153) = pv(1)*pv(98)
 v(1154) = pv(2)*pv(98)
 v(1155) = pv(3)*pv(98)
 v(1156) = pv(0)*pv(99)
 v(1157) = pv(1)*pv(99)
 v(1158) = pv(2)*pv(99)
 v(1159) = pv(3)*pv(99)
 v(1160) = pv(0)*pv(100)
 v(1161) = pv(1)*pv(100)
 v(1162) = pv(2)*pv(100)
 v(1163) = pv(3)*pv(100)
 v(1164) = pv(0)*pv(101)
 v(1165) = pv(1)*pv(101)
 v(1166) = pv(2)*pv(101)
 v(1167) = pv(3)*pv(101)
 v(1168) = pv(0)*pv(102)
 v(1169) = pv(1)*pv(102)
 v(1170) = pv(2)*pv(102)
 v(1171) = pv(3)*pv(102)
 v(1172) = pv(0)*pv(103)
 v(1173) = pv(1)*pv(103)
 v(1174) = pv(2)*pv(103)
 v(1175) = pv(0)*pv(104)
 v(1176) = pv(1)*pv(104)
 v(1177) = pv(2)*pv(104)
 v(1178) = pv(3)*pv(104)
 v(1179) = pv(0)*pv(105)
 v(1180) = pv(1)*pv(105)
 v(1181) = pv(2)*pv(105)
 v(1182) = pv(3)*pv(105)
 v(1183) = pv(0)*pv(106)
 v(1184) = pv(1)*pv(106)
 v(1185) = pv(2)*pv(106)
 v(1186) = pv(3)*pv(106)
 v(1187) = pv(0)*pv(107)
 v(1188) = pv(1)*pv(107)
 v(1189) = pv(2)*pv(107)
 v(1190) = pv(3)*pv(107)
 v(1191) = pv(0)*pv(108)
 v(1192) = pv(1)*pv(108)
 v(1193) = pv(2)*pv(108)
 v(1194) = pv(3)*pv(108)
 v(1195) = pv(0)*pv(109)
 v(1196) = pv(1)*pv(109)
 v(1197) = pv(2)*pv(109)
 v(1198) = pv(3)*pv(109)
 v(1199) = pv(0)*pv(110)
 v(1200) = pv(1)*pv(110)
 v(1201) = pv(2)*pv(110)
 v(1202) = pv(3)*pv(110)
 v(1203) = pv(0)*pv(111)
 v(1204) = pv(1)*pv(111)
 v(1205) = pv(2)*pv(111)
 v(1206) = pv(3)*pv(111)
 v(1207) = pv(0)*pv(112)
 v(1208) = pv(1)*pv(112)
 v(1209) = pv(2)*pv(112)
 v(1210) = pv(3)*pv(112)
 v(1211) = pv(0)*pv(113)
 v(1212) = pv(1)*pv(113)
 v(1213) = pv(2)*pv(113)
 v(1214) = pv(3)*pv(113)
 v(1215) = pv(0)*pv(114)
 v(1216) = pv(1)*pv(114)
 v(1217) = pv(2)*pv(114)
 v(1218) = pv(3)*pv(114)
 v(1219) = pv(0)*pv(115)
 v(1220) = pv(1)*pv(115)
 v(1221) = pv(2)*pv(115)
 v(1222) = pv(0)*pv(116)
 v(1223) = pv(1)*pv(116)
 v(1224) = pv(2)*pv(116)
 v(1225) = pv(0)*pv(117)
 v(1226) = pv(1)*pv(117)
 v(1227) = pv(2)*pv(117)
 v(1228) = pv(0)*pv(118)
 v(1229) = pv(1)*pv(118)
 v(1230) = pv(2)*pv(118)
 v(1231) = pv(0)*pv(119)
 v(1232) = pv(1)*pv(119)
 v(1233) = pv(2)*pv(119)
 v(1234) = pv(0)*pv(120)
 v(1235) = pv(1)*pv(120)
 v(1236) = pv(2)*pv(120)
 v(1237) = pv(0)*pv(121)
 v(1238) = pv(1)*pv(121)
 v(1239) = pv(2)*pv(121)
 v(1240) = pv(0)*pv(122)
 v(1241) = pv(1)*pv(122)
 v(1242) = pv(2)*pv(122)
 v(1243) = pv(0)*pv(123)
 v(1244) = pv(1)*pv(123)
 v(1245) = pv(2)*pv(123)
 v(1246) = pv(0)*pv(124)
 v(1247) = pv(1)*pv(124)
 v(1248) = pv(2)*pv(124)
 v(1249) = pv(0)*pv(125)
 v(1250) = pv(1)*pv(125)
 v(1251) = pv(2)*pv(125)
 v(1252:2518) = pv(487:1753)
endif
if (7.le.mxd) then
 stop 'mg621: degree 7 not implemented'
endif
return
END SUBROUTINE mg621_secs