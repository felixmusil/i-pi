SUBROUTINE mg4321_secs (mxd, r, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: v(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: pv(0:size(v)-1) !! an upper bound size
if (size(r).ne.nk*nk.or.size(v).ne.mg4321_nsc(mxd)) then
 stop 'mg4321_secs: bad dimensions'
endif
call mg4321_isecs (mxd, r, pv)
v(0) = 1
if (2.le.mxd) then
 v(1:15) = pv(0:14)
endif
if (3.le.mxd) then
 v(16:116) = pv(15:115)
endif
if (4.le.mxd) then
 v(117) = pv(0)*pv(0)
 v(118) = pv(0)*pv(1)
 v(119) = pv(1)*pv(1)
 v(120) = pv(0)*pv(2)
 v(121) = pv(1)*pv(2)
 v(122) = pv(2)*pv(2)
 v(123) = pv(0)*pv(3)
 v(124) = pv(1)*pv(3)
 v(125) = pv(2)*pv(3)
 v(126) = pv(3)*pv(3)
 v(127) = pv(0)*pv(4)
 v(128) = pv(1)*pv(4)
 v(129) = pv(2)*pv(4)
 v(130) = pv(3)*pv(4)
 v(131) = pv(4)*pv(4)
 v(132) = pv(0)*pv(5)
 v(133) = pv(1)*pv(5)
 v(134) = pv(2)*pv(5)
 v(135) = pv(3)*pv(5)
 v(136) = pv(4)*pv(5)
 v(137) = pv(5)*pv(5)
 v(138) = pv(0)*pv(6)
 v(139) = pv(1)*pv(6)
 v(140) = pv(2)*pv(6)
 v(141) = pv(3)*pv(6)
 v(142) = pv(4)*pv(6)
 v(143) = pv(5)*pv(6)
 v(144) = pv(0)*pv(7)
 v(145) = pv(1)*pv(7)
 v(146) = pv(2)*pv(7)
 v(147) = pv(3)*pv(7)
 v(148) = pv(4)*pv(7)
 v(149) = pv(5)*pv(7)
 v(150) = pv(6)*pv(7)
 v(151) = pv(7)*pv(7)
 v(152) = pv(0)*pv(8)
 v(153) = pv(1)*pv(8)
 v(154) = pv(2)*pv(8)
 v(155) = pv(3)*pv(8)
 v(156) = pv(4)*pv(8)
 v(157) = pv(5)*pv(8)
 v(158) = pv(6)*pv(8)
 v(159) = pv(7)*pv(8)
 v(160) = pv(8)*pv(8)
 v(161) = pv(0)*pv(9)
 v(162) = pv(1)*pv(9)
 v(163) = pv(2)*pv(9)
 v(164) = pv(3)*pv(9)
 v(165) = pv(4)*pv(9)
 v(166) = pv(5)*pv(9)
 v(167) = pv(6)*pv(9)
 v(168) = pv(7)*pv(9)
 v(169) = pv(8)*pv(9)
 v(170) = pv(9)*pv(9)
 v(171) = pv(0)*pv(10)
 v(172) = pv(1)*pv(10)
 v(173) = pv(2)*pv(10)
 v(174) = pv(3)*pv(10)
 v(175) = pv(4)*pv(10)
 v(176) = pv(5)*pv(10)
 v(177) = pv(6)*pv(10)
 v(178) = pv(7)*pv(10)
 v(179) = pv(8)*pv(10)
 v(180) = pv(9)*pv(10)
 v(181) = pv(10)*pv(10)
 v(182) = pv(0)*pv(11)
 v(183) = pv(1)*pv(11)
 v(184) = pv(2)*pv(11)
 v(185) = pv(3)*pv(11)
 v(186) = pv(4)*pv(11)
 v(187) = pv(5)*pv(11)
 v(188) = pv(6)*pv(11)
 v(189) = pv(7)*pv(11)
 v(190) = pv(8)*pv(11)
 v(191) = pv(9)*pv(11)
 v(192) = pv(10)*pv(11)
 v(193) = pv(11)*pv(11)
 v(194) = pv(0)*pv(12)
 v(195) = pv(1)*pv(12)
 v(196) = pv(2)*pv(12)
 v(197) = pv(3)*pv(12)
 v(198) = pv(4)*pv(12)
 v(199) = pv(5)*pv(12)
 v(200) = pv(6)*pv(12)
 v(201) = pv(7)*pv(12)
 v(202) = pv(8)*pv(12)
 v(203) = pv(9)*pv(12)
 v(204) = pv(10)*pv(12)
 v(205) = pv(11)*pv(12)
 v(206) = pv(12)*pv(12)
 v(207) = pv(0)*pv(13)
 v(208) = pv(1)*pv(13)
 v(209) = pv(2)*pv(13)
 v(210) = pv(3)*pv(13)
 v(211) = pv(4)*pv(13)
 v(212) = pv(5)*pv(13)
 v(213) = pv(7)*pv(13)
 v(214) = pv(8)*pv(13)
 v(215) = pv(9)*pv(13)
 v(216) = pv(10)*pv(13)
 v(217) = pv(11)*pv(13)
 v(218) = pv(12)*pv(13)
 v(219) = pv(0)*pv(14)
 v(220) = pv(1)*pv(14)
 v(221) = pv(2)*pv(14)
 v(222) = pv(3)*pv(14)
 v(223) = pv(4)*pv(14)
 v(224) = pv(5)*pv(14)
 v(225) = pv(7)*pv(14)
 v(226) = pv(8)*pv(14)
 v(227) = pv(9)*pv(14)
 v(228) = pv(10)*pv(14)
 v(229) = pv(11)*pv(14)
 v(230) = pv(12)*pv(14)
 v(231:634) = pv(116:519)
endif
if (5.le.mxd) then
 v(635) = pv(0)*pv(15)
 v(636) = pv(1)*pv(15)
 v(637) = pv(2)*pv(15)
 v(638) = pv(3)*pv(15)
 v(639) = pv(4)*pv(15)
 v(640) = pv(5)*pv(15)
 v(641) = pv(6)*pv(15)
 v(642) = pv(7)*pv(15)
 v(643) = pv(8)*pv(15)
 v(644) = pv(9)*pv(15)
 v(645) = pv(10)*pv(15)
 v(646) = pv(11)*pv(15)
 v(647) = pv(12)*pv(15)
 v(648) = pv(13)*pv(15)
 v(649) = pv(14)*pv(15)
 v(650) = pv(0)*pv(16)
 v(651) = pv(1)*pv(16)
 v(652) = pv(2)*pv(16)
 v(653) = pv(3)*pv(16)
 v(654) = pv(4)*pv(16)
 v(655) = pv(5)*pv(16)
 v(656) = pv(6)*pv(16)
 v(657) = pv(7)*pv(16)
 v(658) = pv(8)*pv(16)
 v(659) = pv(9)*pv(16)
 v(660) = pv(10)*pv(16)
 v(661) = pv(11)*pv(16)
 v(662) = pv(12)*pv(16)
 v(663) = pv(13)*pv(16)
 v(664) = pv(14)*pv(16)
 v(665) = pv(0)*pv(17)
 v(666) = pv(1)*pv(17)
 v(667) = pv(2)*pv(17)
 v(668) = pv(3)*pv(17)
 v(669) = pv(4)*pv(17)
 v(670) = pv(5)*pv(17)
 v(671) = pv(6)*pv(17)
 v(672) = pv(7)*pv(17)
 v(673) = pv(8)*pv(17)
 v(674) = pv(9)*pv(17)
 v(675) = pv(10)*pv(17)
 v(676) = pv(11)*pv(17)
 v(677) = pv(12)*pv(17)
 v(678) = pv(13)*pv(17)
 v(679) = pv(14)*pv(17)
 v(680) = pv(0)*pv(18)
 v(681) = pv(1)*pv(18)
 v(682) = pv(2)*pv(18)
 v(683) = pv(3)*pv(18)
 v(684) = pv(4)*pv(18)
 v(685) = pv(5)*pv(18)
 v(686) = pv(6)*pv(18)
 v(687) = pv(7)*pv(18)
 v(688) = pv(8)*pv(18)
 v(689) = pv(9)*pv(18)
 v(690) = pv(10)*pv(18)
 v(691) = pv(11)*pv(18)
 v(692) = pv(12)*pv(18)
 v(693) = pv(13)*pv(18)
 v(694) = pv(14)*pv(18)
 v(695) = pv(0)*pv(19)
 v(696) = pv(1)*pv(19)
 v(697) = pv(2)*pv(19)
 v(698) = pv(3)*pv(19)
 v(699) = pv(4)*pv(19)
 v(700) = pv(5)*pv(19)
 v(701) = pv(6)*pv(19)
 v(702) = pv(7)*pv(19)
 v(703) = pv(8)*pv(19)
 v(704) = pv(9)*pv(19)
 v(705) = pv(10)*pv(19)
 v(706) = pv(11)*pv(19)
 v(707) = pv(12)*pv(19)
 v(708) = pv(13)*pv(19)
 v(709) = pv(14)*pv(19)
 v(710) = pv(0)*pv(20)
 v(711) = pv(1)*pv(20)
 v(712) = pv(2)*pv(20)
 v(713) = pv(3)*pv(20)
 v(714) = pv(4)*pv(20)
 v(715) = pv(5)*pv(20)
 v(716) = pv(6)*pv(20)
 v(717) = pv(7)*pv(20)
 v(718) = pv(8)*pv(20)
 v(719) = pv(9)*pv(20)
 v(720) = pv(10)*pv(20)
 v(721) = pv(11)*pv(20)
 v(722) = pv(12)*pv(20)
 v(723) = pv(13)*pv(20)
 v(724) = pv(14)*pv(20)
 v(725) = pv(0)*pv(21)
 v(726) = pv(1)*pv(21)
 v(727) = pv(2)*pv(21)
 v(728) = pv(3)*pv(21)
 v(729) = pv(4)*pv(21)
 v(730) = pv(5)*pv(21)
 v(731) = pv(6)*pv(21)
 v(732) = pv(7)*pv(21)
 v(733) = pv(8)*pv(21)
 v(734) = pv(9)*pv(21)
 v(735) = pv(10)*pv(21)
 v(736) = pv(11)*pv(21)
 v(737) = pv(12)*pv(21)
 v(738) = pv(13)*pv(21)
 v(739) = pv(14)*pv(21)
 v(740) = pv(0)*pv(22)
 v(741) = pv(1)*pv(22)
 v(742) = pv(2)*pv(22)
 v(743) = pv(3)*pv(22)
 v(744) = pv(4)*pv(22)
 v(745) = pv(5)*pv(22)
 v(746) = pv(6)*pv(22)
 v(747) = pv(7)*pv(22)
 v(748) = pv(8)*pv(22)
 v(749) = pv(9)*pv(22)
 v(750) = pv(10)*pv(22)
 v(751) = pv(11)*pv(22)
 v(752) = pv(12)*pv(22)
 v(753) = pv(13)*pv(22)
 v(754) = pv(14)*pv(22)
 v(755) = pv(0)*pv(23)
 v(756) = pv(1)*pv(23)
 v(757) = pv(2)*pv(23)
 v(758) = pv(3)*pv(23)
 v(759) = pv(4)*pv(23)
 v(760) = pv(5)*pv(23)
 v(761) = pv(6)*pv(23)
 v(762) = pv(7)*pv(23)
 v(763) = pv(8)*pv(23)
 v(764) = pv(9)*pv(23)
 v(765) = pv(10)*pv(23)
 v(766) = pv(11)*pv(23)
 v(767) = pv(12)*pv(23)
 v(768) = pv(13)*pv(23)
 v(769) = pv(14)*pv(23)
 v(770) = pv(0)*pv(24)
 v(771) = pv(1)*pv(24)
 v(772) = pv(2)*pv(24)
 v(773) = pv(3)*pv(24)
 v(774) = pv(4)*pv(24)
 v(775) = pv(5)*pv(24)
 v(776) = pv(6)*pv(24)
 v(777) = pv(7)*pv(24)
 v(778) = pv(8)*pv(24)
 v(779) = pv(9)*pv(24)
 v(780) = pv(10)*pv(24)
 v(781) = pv(11)*pv(24)
 v(782) = pv(12)*pv(24)
 v(783) = pv(13)*pv(24)
 v(784) = pv(14)*pv(24)
 v(785) = pv(0)*pv(25)
 v(786) = pv(1)*pv(25)
 v(787) = pv(2)*pv(25)
 v(788) = pv(3)*pv(25)
 v(789) = pv(4)*pv(25)
 v(790) = pv(5)*pv(25)
 v(791) = pv(6)*pv(25)
 v(792) = pv(7)*pv(25)
 v(793) = pv(8)*pv(25)
 v(794) = pv(9)*pv(25)
 v(795) = pv(10)*pv(25)
 v(796) = pv(11)*pv(25)
 v(797) = pv(12)*pv(25)
 v(798) = pv(13)*pv(25)
 v(799) = pv(14)*pv(25)
 v(800) = pv(0)*pv(26)
 v(801) = pv(1)*pv(26)
 v(802) = pv(2)*pv(26)
 v(803) = pv(3)*pv(26)
 v(804) = pv(4)*pv(26)
 v(805) = pv(5)*pv(26)
 v(806) = pv(6)*pv(26)
 v(807) = pv(7)*pv(26)
 v(808) = pv(8)*pv(26)
 v(809) = pv(9)*pv(26)
 v(810) = pv(10)*pv(26)
 v(811) = pv(11)*pv(26)
 v(812) = pv(12)*pv(26)
 v(813) = pv(13)*pv(26)
 v(814) = pv(14)*pv(26)
 v(815) = pv(0)*pv(27)
 v(816) = pv(2)*pv(27)
 v(817) = pv(3)*pv(27)
 v(818) = pv(6)*pv(27)
 v(819) = pv(7)*pv(27)
 v(820) = pv(8)*pv(27)
 v(821) = pv(9)*pv(27)
 v(822) = pv(13)*pv(27)
 v(823) = pv(14)*pv(27)
 v(824) = pv(0)*pv(28)
 v(825) = pv(1)*pv(28)
 v(826) = pv(2)*pv(28)
 v(827) = pv(3)*pv(28)
 v(828) = pv(4)*pv(28)
 v(829) = pv(5)*pv(28)
 v(830) = pv(6)*pv(28)
 v(831) = pv(7)*pv(28)
 v(832) = pv(8)*pv(28)
 v(833) = pv(9)*pv(28)
 v(834) = pv(10)*pv(28)
 v(835) = pv(11)*pv(28)
 v(836) = pv(12)*pv(28)
 v(837) = pv(13)*pv(28)
 v(838) = pv(14)*pv(28)
 v(839) = pv(0)*pv(29)
 v(840) = pv(1)*pv(29)
 v(841) = pv(2)*pv(29)
 v(842) = pv(3)*pv(29)
 v(843) = pv(4)*pv(29)
 v(844) = pv(5)*pv(29)
 v(845) = pv(6)*pv(29)
 v(846) = pv(7)*pv(29)
 v(847) = pv(8)*pv(29)
 v(848) = pv(9)*pv(29)
 v(849) = pv(10)*pv(29)
 v(850) = pv(11)*pv(29)
 v(851) = pv(12)*pv(29)
 v(852) = pv(13)*pv(29)
 v(853) = pv(14)*pv(29)
 v(854) = pv(0)*pv(30)
 v(855) = pv(2)*pv(30)
 v(856) = pv(3)*pv(30)
 v(857) = pv(4)*pv(30)
 v(858) = pv(6)*pv(30)
 v(859) = pv(7)*pv(30)
 v(860) = pv(8)*pv(30)
 v(861) = pv(9)*pv(30)
 v(862) = pv(10)*pv(30)
 v(863) = pv(13)*pv(30)
 v(864) = pv(14)*pv(30)
 v(865) = pv(0)*pv(31)
 v(866) = pv(1)*pv(31)
 v(867) = pv(2)*pv(31)
 v(868) = pv(3)*pv(31)
 v(869) = pv(4)*pv(31)
 v(870) = pv(5)*pv(31)
 v(871) = pv(6)*pv(31)
 v(872) = pv(7)*pv(31)
 v(873) = pv(8)*pv(31)
 v(874) = pv(9)*pv(31)
 v(875) = pv(10)*pv(31)
 v(876) = pv(11)*pv(31)
 v(877) = pv(12)*pv(31)
 v(878) = pv(13)*pv(31)
 v(879) = pv(14)*pv(31)
 v(880) = pv(0)*pv(32)
 v(881) = pv(1)*pv(32)
 v(882) = pv(2)*pv(32)
 v(883) = pv(3)*pv(32)
 v(884) = pv(4)*pv(32)
 v(885) = pv(5)*pv(32)
 v(886) = pv(6)*pv(32)
 v(887) = pv(7)*pv(32)
 v(888) = pv(8)*pv(32)
 v(889) = pv(9)*pv(32)
 v(890) = pv(10)*pv(32)
 v(891) = pv(11)*pv(32)
 v(892) = pv(12)*pv(32)
 v(893) = pv(13)*pv(32)
 v(894) = pv(14)*pv(32)
 v(895) = pv(0)*pv(33)
 v(896) = pv(1)*pv(33)
 v(897) = pv(2)*pv(33)
 v(898) = pv(3)*pv(33)
 v(899) = pv(4)*pv(33)
 v(900) = pv(5)*pv(33)
 v(901) = pv(6)*pv(33)
 v(902) = pv(7)*pv(33)
 v(903) = pv(8)*pv(33)
 v(904) = pv(9)*pv(33)
 v(905) = pv(10)*pv(33)
 v(906) = pv(11)*pv(33)
 v(907) = pv(12)*pv(33)
 v(908) = pv(13)*pv(33)
 v(909) = pv(14)*pv(33)
 v(910) = pv(0)*pv(34)
 v(911) = pv(1)*pv(34)
 v(912) = pv(2)*pv(34)
 v(913) = pv(3)*pv(34)
 v(914) = pv(4)*pv(34)
 v(915) = pv(5)*pv(34)
 v(916) = pv(6)*pv(34)
 v(917) = pv(7)*pv(34)
 v(918) = pv(8)*pv(34)
 v(919) = pv(9)*pv(34)
 v(920) = pv(10)*pv(34)
 v(921) = pv(11)*pv(34)
 v(922) = pv(12)*pv(34)
 v(923) = pv(13)*pv(34)
 v(924) = pv(14)*pv(34)
 v(925) = pv(0)*pv(35)
 v(926) = pv(1)*pv(35)
 v(927) = pv(2)*pv(35)
 v(928) = pv(3)*pv(35)
 v(929) = pv(4)*pv(35)
 v(930) = pv(5)*pv(35)
 v(931) = pv(6)*pv(35)
 v(932) = pv(7)*pv(35)
 v(933) = pv(8)*pv(35)
 v(934) = pv(9)*pv(35)
 v(935) = pv(10)*pv(35)
 v(936) = pv(11)*pv(35)
 v(937) = pv(12)*pv(35)
 v(938) = pv(13)*pv(35)
 v(939) = pv(14)*pv(35)
 v(940) = pv(0)*pv(36)
 v(941) = pv(1)*pv(36)
 v(942) = pv(2)*pv(36)
 v(943) = pv(3)*pv(36)
 v(944) = pv(4)*pv(36)
 v(945) = pv(5)*pv(36)
 v(946) = pv(6)*pv(36)
 v(947) = pv(7)*pv(36)
 v(948) = pv(8)*pv(36)
 v(949) = pv(9)*pv(36)
 v(950) = pv(10)*pv(36)
 v(951) = pv(11)*pv(36)
 v(952) = pv(12)*pv(36)
 v(953) = pv(13)*pv(36)
 v(954) = pv(14)*pv(36)
 v(955) = pv(0)*pv(37)
 v(956) = pv(1)*pv(37)
 v(957) = pv(2)*pv(37)
 v(958) = pv(3)*pv(37)
 v(959) = pv(4)*pv(37)
 v(960) = pv(5)*pv(37)
 v(961) = pv(6)*pv(37)
 v(962) = pv(7)*pv(37)
 v(963) = pv(8)*pv(37)
 v(964) = pv(9)*pv(37)
 v(965) = pv(10)*pv(37)
 v(966) = pv(11)*pv(37)
 v(967) = pv(12)*pv(37)
 v(968) = pv(13)*pv(37)
 v(969) = pv(14)*pv(37)
 v(970) = pv(0)*pv(38)
 v(971) = pv(1)*pv(38)
 v(972) = pv(2)*pv(38)
 v(973) = pv(3)*pv(38)
 v(974) = pv(4)*pv(38)
 v(975) = pv(5)*pv(38)
 v(976) = pv(6)*pv(38)
 v(977) = pv(7)*pv(38)
 v(978) = pv(8)*pv(38)
 v(979) = pv(9)*pv(38)
 v(980) = pv(10)*pv(38)
 v(981) = pv(11)*pv(38)
 v(982) = pv(12)*pv(38)
 v(983) = pv(13)*pv(38)
 v(984) = pv(14)*pv(38)
 v(985) = pv(0)*pv(39)
 v(986) = pv(1)*pv(39)
 v(987) = pv(2)*pv(39)
 v(988) = pv(3)*pv(39)
 v(989) = pv(4)*pv(39)
 v(990) = pv(5)*pv(39)
 v(991) = pv(7)*pv(39)
 v(992) = pv(8)*pv(39)
 v(993) = pv(9)*pv(39)
 v(994) = pv(10)*pv(39)
 v(995) = pv(11)*pv(39)
 v(996) = pv(12)*pv(39)
 v(997) = pv(0)*pv(40)
 v(998) = pv(1)*pv(40)
 v(999) = pv(2)*pv(40)
 v(1000) = pv(3)*pv(40)
 v(1001) = pv(4)*pv(40)
 v(1002) = pv(5)*pv(40)
 v(1003) = pv(7)*pv(40)
 v(1004) = pv(8)*pv(40)
 v(1005) = pv(9)*pv(40)
 v(1006) = pv(10)*pv(40)
 v(1007) = pv(11)*pv(40)
 v(1008) = pv(12)*pv(40)
 v(1009) = pv(0)*pv(41)
 v(1010) = pv(1)*pv(41)
 v(1011) = pv(2)*pv(41)
 v(1012) = pv(3)*pv(41)
 v(1013) = pv(4)*pv(41)
 v(1014) = pv(5)*pv(41)
 v(1015) = pv(7)*pv(41)
 v(1016) = pv(8)*pv(41)
 v(1017) = pv(9)*pv(41)
 v(1018) = pv(10)*pv(41)
 v(1019) = pv(11)*pv(41)
 v(1020) = pv(12)*pv(41)
 v(1021) = pv(0)*pv(42)
 v(1022) = pv(1)*pv(42)
 v(1023) = pv(2)*pv(42)
 v(1024) = pv(3)*pv(42)
 v(1025) = pv(4)*pv(42)
 v(1026) = pv(5)*pv(42)
 v(1027) = pv(6)*pv(42)
 v(1028) = pv(7)*pv(42)
 v(1029) = pv(8)*pv(42)
 v(1030) = pv(9)*pv(42)
 v(1031) = pv(10)*pv(42)
 v(1032) = pv(11)*pv(42)
 v(1033) = pv(12)*pv(42)
 v(1034) = pv(13)*pv(42)
 v(1035) = pv(14)*pv(42)
 v(1036) = pv(0)*pv(43)
 v(1037) = pv(1)*pv(43)
 v(1038) = pv(2)*pv(43)
 v(1039) = pv(3)*pv(43)
 v(1040) = pv(4)*pv(43)
 v(1041) = pv(5)*pv(43)
 v(1042) = pv(6)*pv(43)
 v(1043) = pv(7)*pv(43)
 v(1044) = pv(8)*pv(43)
 v(1045) = pv(9)*pv(43)
 v(1046) = pv(10)*pv(43)
 v(1047) = pv(11)*pv(43)
 v(1048) = pv(12)*pv(43)
 v(1049) = pv(13)*pv(43)
 v(1050) = pv(14)*pv(43)
 v(1051) = pv(0)*pv(44)
 v(1052) = pv(1)*pv(44)
 v(1053) = pv(2)*pv(44)
 v(1054) = pv(3)*pv(44)
 v(1055) = pv(4)*pv(44)
 v(1056) = pv(5)*pv(44)
 v(1057) = pv(6)*pv(44)
 v(1058) = pv(7)*pv(44)
 v(1059) = pv(8)*pv(44)
 v(1060) = pv(9)*pv(44)
 v(1061) = pv(10)*pv(44)
 v(1062) = pv(11)*pv(44)
 v(1063) = pv(12)*pv(44)
 v(1064) = pv(13)*pv(44)
 v(1065) = pv(14)*pv(44)
 v(1066) = pv(0)*pv(45)
 v(1067) = pv(1)*pv(45)
 v(1068) = pv(2)*pv(45)
 v(1069) = pv(3)*pv(45)
 v(1070) = pv(4)*pv(45)
 v(1071) = pv(5)*pv(45)
 v(1072) = pv(6)*pv(45)
 v(1073) = pv(7)*pv(45)
 v(1074) = pv(8)*pv(45)
 v(1075) = pv(9)*pv(45)
 v(1076) = pv(10)*pv(45)
 v(1077) = pv(11)*pv(45)
 v(1078) = pv(12)*pv(45)
 v(1079) = pv(13)*pv(45)
 v(1080) = pv(14)*pv(45)
 v(1081) = pv(0)*pv(46)
 v(1082) = pv(1)*pv(46)
 v(1083) = pv(2)*pv(46)
 v(1084) = pv(3)*pv(46)
 v(1085) = pv(4)*pv(46)
 v(1086) = pv(5)*pv(46)
 v(1087) = pv(6)*pv(46)
 v(1088) = pv(7)*pv(46)
 v(1089) = pv(8)*pv(46)
 v(1090) = pv(9)*pv(46)
 v(1091) = pv(10)*pv(46)
 v(1092) = pv(11)*pv(46)
 v(1093) = pv(12)*pv(46)
 v(1094) = pv(13)*pv(46)
 v(1095) = pv(14)*pv(46)
 v(1096) = pv(0)*pv(47)
 v(1097) = pv(1)*pv(47)
 v(1098) = pv(2)*pv(47)
 v(1099) = pv(3)*pv(47)
 v(1100) = pv(4)*pv(47)
 v(1101) = pv(5)*pv(47)
 v(1102) = pv(6)*pv(47)
 v(1103) = pv(7)*pv(47)
 v(1104) = pv(8)*pv(47)
 v(1105) = pv(9)*pv(47)
 v(1106) = pv(10)*pv(47)
 v(1107) = pv(11)*pv(47)
 v(1108) = pv(12)*pv(47)
 v(1109) = pv(13)*pv(47)
 v(1110) = pv(14)*pv(47)
 v(1111) = pv(0)*pv(48)
 v(1112) = pv(1)*pv(48)
 v(1113) = pv(2)*pv(48)
 v(1114) = pv(3)*pv(48)
 v(1115) = pv(4)*pv(48)
 v(1116) = pv(5)*pv(48)
 v(1117) = pv(6)*pv(48)
 v(1118) = pv(7)*pv(48)
 v(1119) = pv(8)*pv(48)
 v(1120) = pv(9)*pv(48)
 v(1121) = pv(10)*pv(48)
 v(1122) = pv(11)*pv(48)
 v(1123) = pv(12)*pv(48)
 v(1124) = pv(13)*pv(48)
 v(1125) = pv(14)*pv(48)
 v(1126) = pv(0)*pv(49)
 v(1127) = pv(1)*pv(49)
 v(1128) = pv(2)*pv(49)
 v(1129) = pv(3)*pv(49)
 v(1130) = pv(4)*pv(49)
 v(1131) = pv(5)*pv(49)
 v(1132) = pv(6)*pv(49)
 v(1133) = pv(7)*pv(49)
 v(1134) = pv(8)*pv(49)
 v(1135) = pv(9)*pv(49)
 v(1136) = pv(10)*pv(49)
 v(1137) = pv(11)*pv(49)
 v(1138) = pv(12)*pv(49)
 v(1139) = pv(13)*pv(49)
 v(1140) = pv(14)*pv(49)
 v(1141) = pv(0)*pv(50)
 v(1142) = pv(1)*pv(50)
 v(1143) = pv(2)*pv(50)
 v(1144) = pv(3)*pv(50)
 v(1145) = pv(6)*pv(50)
 v(1146) = pv(7)*pv(50)
 v(1147) = pv(8)*pv(50)
 v(1148) = pv(9)*pv(50)
 v(1149) = pv(13)*pv(50)
 v(1150) = pv(14)*pv(50)
 v(1151) = pv(0)*pv(51)
 v(1152) = pv(1)*pv(51)
 v(1153) = pv(2)*pv(51)
 v(1154) = pv(3)*pv(51)
 v(1155) = pv(4)*pv(51)
 v(1156) = pv(5)*pv(51)
 v(1157) = pv(6)*pv(51)
 v(1158) = pv(7)*pv(51)
 v(1159) = pv(8)*pv(51)
 v(1160) = pv(9)*pv(51)
 v(1161) = pv(10)*pv(51)
 v(1162) = pv(11)*pv(51)
 v(1163) = pv(12)*pv(51)
 v(1164) = pv(13)*pv(51)
 v(1165) = pv(14)*pv(51)
 v(1166) = pv(0)*pv(52)
 v(1167) = pv(1)*pv(52)
 v(1168) = pv(2)*pv(52)
 v(1169) = pv(3)*pv(52)
 v(1170) = pv(4)*pv(52)
 v(1171) = pv(5)*pv(52)
 v(1172) = pv(6)*pv(52)
 v(1173) = pv(7)*pv(52)
 v(1174) = pv(8)*pv(52)
 v(1175) = pv(9)*pv(52)
 v(1176) = pv(10)*pv(52)
 v(1177) = pv(11)*pv(52)
 v(1178) = pv(12)*pv(52)
 v(1179) = pv(13)*pv(52)
 v(1180) = pv(14)*pv(52)
 v(1181) = pv(0)*pv(53)
 v(1182) = pv(2)*pv(53)
 v(1183) = pv(3)*pv(53)
 v(1184) = pv(6)*pv(53)
 v(1185) = pv(7)*pv(53)
 v(1186) = pv(8)*pv(53)
 v(1187) = pv(9)*pv(53)
 v(1188) = pv(10)*pv(53)
 v(1189) = pv(13)*pv(53)
 v(1190) = pv(14)*pv(53)
 v(1191) = pv(0)*pv(54)
 v(1192) = pv(1)*pv(54)
 v(1193) = pv(2)*pv(54)
 v(1194) = pv(3)*pv(54)
 v(1195) = pv(6)*pv(54)
 v(1196) = pv(7)*pv(54)
 v(1197) = pv(8)*pv(54)
 v(1198) = pv(9)*pv(54)
 v(1199) = pv(10)*pv(54)
 v(1200) = pv(13)*pv(54)
 v(1201) = pv(14)*pv(54)
 v(1202) = pv(0)*pv(55)
 v(1203) = pv(1)*pv(55)
 v(1204) = pv(2)*pv(55)
 v(1205) = pv(3)*pv(55)
 v(1206) = pv(4)*pv(55)
 v(1207) = pv(5)*pv(55)
 v(1208) = pv(7)*pv(55)
 v(1209) = pv(8)*pv(55)
 v(1210) = pv(9)*pv(55)
 v(1211) = pv(10)*pv(55)
 v(1212) = pv(11)*pv(55)
 v(1213) = pv(12)*pv(55)
 v(1214) = pv(0)*pv(56)
 v(1215) = pv(1)*pv(56)
 v(1216) = pv(2)*pv(56)
 v(1217) = pv(3)*pv(56)
 v(1218) = pv(4)*pv(56)
 v(1219) = pv(5)*pv(56)
 v(1220) = pv(7)*pv(56)
 v(1221) = pv(8)*pv(56)
 v(1222) = pv(9)*pv(56)
 v(1223) = pv(10)*pv(56)
 v(1224) = pv(11)*pv(56)
 v(1225) = pv(12)*pv(56)
 v(1226) = pv(0)*pv(57)
 v(1227) = pv(1)*pv(57)
 v(1228) = pv(2)*pv(57)
 v(1229) = pv(3)*pv(57)
 v(1230) = pv(4)*pv(57)
 v(1231) = pv(5)*pv(57)
 v(1232) = pv(7)*pv(57)
 v(1233) = pv(8)*pv(57)
 v(1234) = pv(9)*pv(57)
 v(1235) = pv(10)*pv(57)
 v(1236) = pv(11)*pv(57)
 v(1237) = pv(12)*pv(57)
 v(1238) = pv(0)*pv(58)
 v(1239) = pv(1)*pv(58)
 v(1240) = pv(2)*pv(58)
 v(1241) = pv(3)*pv(58)
 v(1242) = pv(4)*pv(58)
 v(1243) = pv(5)*pv(58)
 v(1244) = pv(6)*pv(58)
 v(1245) = pv(7)*pv(58)
 v(1246) = pv(8)*pv(58)
 v(1247) = pv(9)*pv(58)
 v(1248) = pv(10)*pv(58)
 v(1249) = pv(11)*pv(58)
 v(1250) = pv(12)*pv(58)
 v(1251) = pv(13)*pv(58)
 v(1252) = pv(14)*pv(58)
 v(1253) = pv(0)*pv(59)
 v(1254) = pv(1)*pv(59)
 v(1255) = pv(2)*pv(59)
 v(1256) = pv(3)*pv(59)
 v(1257) = pv(4)*pv(59)
 v(1258) = pv(5)*pv(59)
 v(1259) = pv(7)*pv(59)
 v(1260) = pv(8)*pv(59)
 v(1261) = pv(9)*pv(59)
 v(1262) = pv(10)*pv(59)
 v(1263) = pv(11)*pv(59)
 v(1264) = pv(12)*pv(59)
 v(1265) = pv(0)*pv(60)
 v(1266) = pv(1)*pv(60)
 v(1267) = pv(2)*pv(60)
 v(1268) = pv(3)*pv(60)
 v(1269) = pv(4)*pv(60)
 v(1270) = pv(5)*pv(60)
 v(1271) = pv(7)*pv(60)
 v(1272) = pv(8)*pv(60)
 v(1273) = pv(9)*pv(60)
 v(1274) = pv(10)*pv(60)
 v(1275) = pv(11)*pv(60)
 v(1276) = pv(12)*pv(60)
 v(1277) = pv(0)*pv(61)
 v(1278) = pv(1)*pv(61)
 v(1279) = pv(2)*pv(61)
 v(1280) = pv(3)*pv(61)
 v(1281) = pv(4)*pv(61)
 v(1282) = pv(5)*pv(61)
 v(1283) = pv(6)*pv(61)
 v(1284) = pv(7)*pv(61)
 v(1285) = pv(8)*pv(61)
 v(1286) = pv(9)*pv(61)
 v(1287) = pv(10)*pv(61)
 v(1288) = pv(11)*pv(61)
 v(1289) = pv(12)*pv(61)
 v(1290) = pv(13)*pv(61)
 v(1291) = pv(14)*pv(61)
 v(1292) = pv(0)*pv(62)
 v(1293) = pv(1)*pv(62)
 v(1294) = pv(2)*pv(62)
 v(1295) = pv(3)*pv(62)
 v(1296) = pv(7)*pv(62)
 v(1297) = pv(8)*pv(62)
 v(1298) = pv(9)*pv(62)
 v(1299) = pv(10)*pv(62)
 v(1300) = pv(0)*pv(63)
 v(1301) = pv(1)*pv(63)
 v(1302) = pv(2)*pv(63)
 v(1303) = pv(3)*pv(63)
 v(1304) = pv(4)*pv(63)
 v(1305) = pv(7)*pv(63)
 v(1306) = pv(8)*pv(63)
 v(1307) = pv(9)*pv(63)
 v(1308) = pv(10)*pv(63)
 v(1309) = pv(11)*pv(63)
 v(1310) = pv(0)*pv(64)
 v(1311) = pv(1)*pv(64)
 v(1312) = pv(2)*pv(64)
 v(1313) = pv(3)*pv(64)
 v(1314) = pv(4)*pv(64)
 v(1315) = pv(5)*pv(64)
 v(1316) = pv(7)*pv(64)
 v(1317) = pv(8)*pv(64)
 v(1318) = pv(9)*pv(64)
 v(1319) = pv(10)*pv(64)
 v(1320) = pv(11)*pv(64)
 v(1321) = pv(12)*pv(64)
 v(1322) = pv(0)*pv(65)
 v(1323) = pv(1)*pv(65)
 v(1324) = pv(2)*pv(65)
 v(1325) = pv(3)*pv(65)
 v(1326) = pv(4)*pv(65)
 v(1327) = pv(5)*pv(65)
 v(1328) = pv(7)*pv(65)
 v(1329) = pv(8)*pv(65)
 v(1330) = pv(9)*pv(65)
 v(1331) = pv(10)*pv(65)
 v(1332) = pv(11)*pv(65)
 v(1333) = pv(12)*pv(65)
 v(1334) = pv(0)*pv(66)
 v(1335) = pv(1)*pv(66)
 v(1336) = pv(2)*pv(66)
 v(1337) = pv(3)*pv(66)
 v(1338) = pv(4)*pv(66)
 v(1339) = pv(5)*pv(66)
 v(1340) = pv(6)*pv(66)
 v(1341) = pv(7)*pv(66)
 v(1342) = pv(8)*pv(66)
 v(1343) = pv(9)*pv(66)
 v(1344) = pv(10)*pv(66)
 v(1345) = pv(11)*pv(66)
 v(1346) = pv(12)*pv(66)
 v(1347) = pv(13)*pv(66)
 v(1348) = pv(14)*pv(66)
 v(1349) = pv(0)*pv(67)
 v(1350) = pv(1)*pv(67)
 v(1351) = pv(2)*pv(67)
 v(1352) = pv(3)*pv(67)
 v(1353) = pv(4)*pv(67)
 v(1354) = pv(5)*pv(67)
 v(1355) = pv(6)*pv(67)
 v(1356) = pv(7)*pv(67)
 v(1357) = pv(8)*pv(67)
 v(1358) = pv(9)*pv(67)
 v(1359) = pv(10)*pv(67)
 v(1360) = pv(11)*pv(67)
 v(1361) = pv(12)*pv(67)
 v(1362) = pv(13)*pv(67)
 v(1363) = pv(14)*pv(67)
 v(1364) = pv(0)*pv(68)
 v(1365) = pv(1)*pv(68)
 v(1366) = pv(2)*pv(68)
 v(1367) = pv(3)*pv(68)
 v(1368) = pv(4)*pv(68)
 v(1369) = pv(5)*pv(68)
 v(1370) = pv(6)*pv(68)
 v(1371) = pv(7)*pv(68)
 v(1372) = pv(8)*pv(68)
 v(1373) = pv(9)*pv(68)
 v(1374) = pv(10)*pv(68)
 v(1375) = pv(11)*pv(68)
 v(1376) = pv(12)*pv(68)
 v(1377) = pv(13)*pv(68)
 v(1378) = pv(14)*pv(68)
 v(1379) = pv(0)*pv(69)
 v(1380) = pv(1)*pv(69)
 v(1381) = pv(2)*pv(69)
 v(1382) = pv(3)*pv(69)
 v(1383) = pv(4)*pv(69)
 v(1384) = pv(5)*pv(69)
 v(1385) = pv(6)*pv(69)
 v(1386) = pv(7)*pv(69)
 v(1387) = pv(8)*pv(69)
 v(1388) = pv(9)*pv(69)
 v(1389) = pv(10)*pv(69)
 v(1390) = pv(11)*pv(69)
 v(1391) = pv(12)*pv(69)
 v(1392) = pv(13)*pv(69)
 v(1393) = pv(14)*pv(69)
 v(1394) = pv(0)*pv(70)
 v(1395) = pv(1)*pv(70)
 v(1396) = pv(2)*pv(70)
 v(1397) = pv(3)*pv(70)
 v(1398) = pv(4)*pv(70)
 v(1399) = pv(5)*pv(70)
 v(1400) = pv(6)*pv(70)
 v(1401) = pv(7)*pv(70)
 v(1402) = pv(8)*pv(70)
 v(1403) = pv(9)*pv(70)
 v(1404) = pv(10)*pv(70)
 v(1405) = pv(11)*pv(70)
 v(1406) = pv(12)*pv(70)
 v(1407) = pv(13)*pv(70)
 v(1408) = pv(14)*pv(70)
 v(1409) = pv(0)*pv(71)
 v(1410) = pv(1)*pv(71)
 v(1411) = pv(2)*pv(71)
 v(1412) = pv(3)*pv(71)
 v(1413) = pv(4)*pv(71)
 v(1414) = pv(5)*pv(71)
 v(1415) = pv(6)*pv(71)
 v(1416) = pv(7)*pv(71)
 v(1417) = pv(8)*pv(71)
 v(1418) = pv(9)*pv(71)
 v(1419) = pv(10)*pv(71)
 v(1420) = pv(11)*pv(71)
 v(1421) = pv(12)*pv(71)
 v(1422) = pv(13)*pv(71)
 v(1423) = pv(14)*pv(71)
 v(1424) = pv(0)*pv(72)
 v(1425) = pv(1)*pv(72)
 v(1426) = pv(2)*pv(72)
 v(1427) = pv(3)*pv(72)
 v(1428) = pv(4)*pv(72)
 v(1429) = pv(5)*pv(72)
 v(1430) = pv(6)*pv(72)
 v(1431) = pv(7)*pv(72)
 v(1432) = pv(8)*pv(72)
 v(1433) = pv(9)*pv(72)
 v(1434) = pv(10)*pv(72)
 v(1435) = pv(11)*pv(72)
 v(1436) = pv(12)*pv(72)
 v(1437) = pv(13)*pv(72)
 v(1438) = pv(14)*pv(72)
 v(1439) = pv(0)*pv(73)
 v(1440) = pv(1)*pv(73)
 v(1441) = pv(2)*pv(73)
 v(1442) = pv(3)*pv(73)
 v(1443) = pv(4)*pv(73)
 v(1444) = pv(5)*pv(73)
 v(1445) = pv(6)*pv(73)
 v(1446) = pv(7)*pv(73)
 v(1447) = pv(8)*pv(73)
 v(1448) = pv(9)*pv(73)
 v(1449) = pv(10)*pv(73)
 v(1450) = pv(11)*pv(73)
 v(1451) = pv(12)*pv(73)
 v(1452) = pv(13)*pv(73)
 v(1453) = pv(14)*pv(73)
 v(1454) = pv(0)*pv(74)
 v(1455) = pv(1)*pv(74)
 v(1456) = pv(2)*pv(74)
 v(1457) = pv(3)*pv(74)
 v(1458) = pv(4)*pv(74)
 v(1459) = pv(5)*pv(74)
 v(1460) = pv(6)*pv(74)
 v(1461) = pv(7)*pv(74)
 v(1462) = pv(8)*pv(74)
 v(1463) = pv(9)*pv(74)
 v(1464) = pv(10)*pv(74)
 v(1465) = pv(11)*pv(74)
 v(1466) = pv(12)*pv(74)
 v(1467) = pv(13)*pv(74)
 v(1468) = pv(14)*pv(74)
 v(1469) = pv(0)*pv(75)
 v(1470) = pv(1)*pv(75)
 v(1471) = pv(2)*pv(75)
 v(1472) = pv(3)*pv(75)
 v(1473) = pv(4)*pv(75)
 v(1474) = pv(5)*pv(75)
 v(1475) = pv(6)*pv(75)
 v(1476) = pv(7)*pv(75)
 v(1477) = pv(8)*pv(75)
 v(1478) = pv(9)*pv(75)
 v(1479) = pv(10)*pv(75)
 v(1480) = pv(11)*pv(75)
 v(1481) = pv(12)*pv(75)
 v(1482) = pv(13)*pv(75)
 v(1483) = pv(14)*pv(75)
 v(1484) = pv(0)*pv(76)
 v(1485) = pv(1)*pv(76)
 v(1486) = pv(2)*pv(76)
 v(1487) = pv(3)*pv(76)
 v(1488) = pv(4)*pv(76)
 v(1489) = pv(5)*pv(76)
 v(1490) = pv(6)*pv(76)
 v(1491) = pv(7)*pv(76)
 v(1492) = pv(8)*pv(76)
 v(1493) = pv(9)*pv(76)
 v(1494) = pv(10)*pv(76)
 v(1495) = pv(11)*pv(76)
 v(1496) = pv(12)*pv(76)
 v(1497) = pv(13)*pv(76)
 v(1498) = pv(14)*pv(76)
 v(1499) = pv(0)*pv(77)
 v(1500) = pv(1)*pv(77)
 v(1501) = pv(2)*pv(77)
 v(1502) = pv(3)*pv(77)
 v(1503) = pv(4)*pv(77)
 v(1504) = pv(5)*pv(77)
 v(1505) = pv(6)*pv(77)
 v(1506) = pv(7)*pv(77)
 v(1507) = pv(8)*pv(77)
 v(1508) = pv(9)*pv(77)
 v(1509) = pv(10)*pv(77)
 v(1510) = pv(11)*pv(77)
 v(1511) = pv(12)*pv(77)
 v(1512) = pv(13)*pv(77)
 v(1513) = pv(14)*pv(77)
 v(1514) = pv(0)*pv(78)
 v(1515) = pv(1)*pv(78)
 v(1516) = pv(2)*pv(78)
 v(1517) = pv(3)*pv(78)
 v(1518) = pv(4)*pv(78)
 v(1519) = pv(5)*pv(78)
 v(1520) = pv(6)*pv(78)
 v(1521) = pv(7)*pv(78)
 v(1522) = pv(8)*pv(78)
 v(1523) = pv(9)*pv(78)
 v(1524) = pv(10)*pv(78)
 v(1525) = pv(11)*pv(78)
 v(1526) = pv(12)*pv(78)
 v(1527) = pv(13)*pv(78)
 v(1528) = pv(14)*pv(78)
 v(1529) = pv(0)*pv(79)
 v(1530) = pv(1)*pv(79)
 v(1531) = pv(2)*pv(79)
 v(1532) = pv(3)*pv(79)
 v(1533) = pv(4)*pv(79)
 v(1534) = pv(5)*pv(79)
 v(1535) = pv(6)*pv(79)
 v(1536) = pv(7)*pv(79)
 v(1537) = pv(8)*pv(79)
 v(1538) = pv(9)*pv(79)
 v(1539) = pv(10)*pv(79)
 v(1540) = pv(11)*pv(79)
 v(1541) = pv(12)*pv(79)
 v(1542) = pv(13)*pv(79)
 v(1543) = pv(14)*pv(79)
 v(1544) = pv(0)*pv(80)
 v(1545) = pv(1)*pv(80)
 v(1546) = pv(2)*pv(80)
 v(1547) = pv(3)*pv(80)
 v(1548) = pv(4)*pv(80)
 v(1549) = pv(5)*pv(80)
 v(1550) = pv(6)*pv(80)
 v(1551) = pv(7)*pv(80)
 v(1552) = pv(8)*pv(80)
 v(1553) = pv(9)*pv(80)
 v(1554) = pv(10)*pv(80)
 v(1555) = pv(11)*pv(80)
 v(1556) = pv(12)*pv(80)
 v(1557) = pv(13)*pv(80)
 v(1558) = pv(14)*pv(80)
 v(1559) = pv(0)*pv(81)
 v(1560) = pv(1)*pv(81)
 v(1561) = pv(2)*pv(81)
 v(1562) = pv(3)*pv(81)
 v(1563) = pv(4)*pv(81)
 v(1564) = pv(5)*pv(81)
 v(1565) = pv(7)*pv(81)
 v(1566) = pv(8)*pv(81)
 v(1567) = pv(9)*pv(81)
 v(1568) = pv(10)*pv(81)
 v(1569) = pv(11)*pv(81)
 v(1570) = pv(12)*pv(81)
 v(1571) = pv(0)*pv(82)
 v(1572) = pv(1)*pv(82)
 v(1573) = pv(2)*pv(82)
 v(1574) = pv(3)*pv(82)
 v(1575) = pv(4)*pv(82)
 v(1576) = pv(5)*pv(82)
 v(1577) = pv(6)*pv(82)
 v(1578) = pv(7)*pv(82)
 v(1579) = pv(8)*pv(82)
 v(1580) = pv(9)*pv(82)
 v(1581) = pv(10)*pv(82)
 v(1582) = pv(11)*pv(82)
 v(1583) = pv(12)*pv(82)
 v(1584) = pv(13)*pv(82)
 v(1585) = pv(14)*pv(82)
 v(1586) = pv(0)*pv(83)
 v(1587) = pv(1)*pv(83)
 v(1588) = pv(2)*pv(83)
 v(1589) = pv(3)*pv(83)
 v(1590) = pv(4)*pv(83)
 v(1591) = pv(5)*pv(83)
 v(1592) = pv(6)*pv(83)
 v(1593) = pv(7)*pv(83)
 v(1594) = pv(8)*pv(83)
 v(1595) = pv(9)*pv(83)
 v(1596) = pv(10)*pv(83)
 v(1597) = pv(11)*pv(83)
 v(1598) = pv(12)*pv(83)
 v(1599) = pv(13)*pv(83)
 v(1600) = pv(14)*pv(83)
 v(1601) = pv(0)*pv(84)
 v(1602) = pv(1)*pv(84)
 v(1603) = pv(2)*pv(84)
 v(1604) = pv(3)*pv(84)
 v(1605) = pv(4)*pv(84)
 v(1606) = pv(5)*pv(84)
 v(1607) = pv(6)*pv(84)
 v(1608) = pv(7)*pv(84)
 v(1609) = pv(8)*pv(84)
 v(1610) = pv(9)*pv(84)
 v(1611) = pv(10)*pv(84)
 v(1612) = pv(11)*pv(84)
 v(1613) = pv(12)*pv(84)
 v(1614) = pv(13)*pv(84)
 v(1615) = pv(14)*pv(84)
 v(1616) = pv(0)*pv(85)
 v(1617) = pv(1)*pv(85)
 v(1618) = pv(2)*pv(85)
 v(1619) = pv(3)*pv(85)
 v(1620) = pv(4)*pv(85)
 v(1621) = pv(5)*pv(85)
 v(1622) = pv(6)*pv(85)
 v(1623) = pv(7)*pv(85)
 v(1624) = pv(8)*pv(85)
 v(1625) = pv(9)*pv(85)
 v(1626) = pv(10)*pv(85)
 v(1627) = pv(11)*pv(85)
 v(1628) = pv(12)*pv(85)
 v(1629) = pv(13)*pv(85)
 v(1630) = pv(14)*pv(85)
 v(1631) = pv(0)*pv(86)
 v(1632) = pv(1)*pv(86)
 v(1633) = pv(2)*pv(86)
 v(1634) = pv(3)*pv(86)
 v(1635) = pv(4)*pv(86)
 v(1636) = pv(5)*pv(86)
 v(1637) = pv(7)*pv(86)
 v(1638) = pv(8)*pv(86)
 v(1639) = pv(9)*pv(86)
 v(1640) = pv(10)*pv(86)
 v(1641) = pv(11)*pv(86)
 v(1642) = pv(12)*pv(86)
 v(1643) = pv(0)*pv(87)
 v(1644) = pv(1)*pv(87)
 v(1645) = pv(2)*pv(87)
 v(1646) = pv(3)*pv(87)
 v(1647) = pv(4)*pv(87)
 v(1648) = pv(5)*pv(87)
 v(1649) = pv(6)*pv(87)
 v(1650) = pv(7)*pv(87)
 v(1651) = pv(8)*pv(87)
 v(1652) = pv(9)*pv(87)
 v(1653) = pv(10)*pv(87)
 v(1654) = pv(11)*pv(87)
 v(1655) = pv(12)*pv(87)
 v(1656) = pv(13)*pv(87)
 v(1657) = pv(14)*pv(87)
 v(1658) = pv(0)*pv(88)
 v(1659) = pv(1)*pv(88)
 v(1660) = pv(2)*pv(88)
 v(1661) = pv(3)*pv(88)
 v(1662) = pv(4)*pv(88)
 v(1663) = pv(5)*pv(88)
 v(1664) = pv(6)*pv(88)
 v(1665) = pv(7)*pv(88)
 v(1666) = pv(8)*pv(88)
 v(1667) = pv(9)*pv(88)
 v(1668) = pv(10)*pv(88)
 v(1669) = pv(11)*pv(88)
 v(1670) = pv(12)*pv(88)
 v(1671) = pv(13)*pv(88)
 v(1672) = pv(14)*pv(88)
 v(1673) = pv(0)*pv(89)
 v(1674) = pv(1)*pv(89)
 v(1675) = pv(2)*pv(89)
 v(1676) = pv(3)*pv(89)
 v(1677) = pv(4)*pv(89)
 v(1678) = pv(5)*pv(89)
 v(1679) = pv(6)*pv(89)
 v(1680) = pv(7)*pv(89)
 v(1681) = pv(8)*pv(89)
 v(1682) = pv(9)*pv(89)
 v(1683) = pv(10)*pv(89)
 v(1684) = pv(11)*pv(89)
 v(1685) = pv(12)*pv(89)
 v(1686) = pv(13)*pv(89)
 v(1687) = pv(14)*pv(89)
 v(1688) = pv(0)*pv(90)
 v(1689) = pv(1)*pv(90)
 v(1690) = pv(2)*pv(90)
 v(1691) = pv(3)*pv(90)
 v(1692) = pv(4)*pv(90)
 v(1693) = pv(5)*pv(90)
 v(1694) = pv(6)*pv(90)
 v(1695) = pv(7)*pv(90)
 v(1696) = pv(8)*pv(90)
 v(1697) = pv(9)*pv(90)
 v(1698) = pv(10)*pv(90)
 v(1699) = pv(11)*pv(90)
 v(1700) = pv(12)*pv(90)
 v(1701) = pv(13)*pv(90)
 v(1702) = pv(14)*pv(90)
 v(1703) = pv(0)*pv(91)
 v(1704) = pv(1)*pv(91)
 v(1705) = pv(2)*pv(91)
 v(1706) = pv(3)*pv(91)
 v(1707) = pv(4)*pv(91)
 v(1708) = pv(5)*pv(91)
 v(1709) = pv(6)*pv(91)
 v(1710) = pv(7)*pv(91)
 v(1711) = pv(8)*pv(91)
 v(1712) = pv(9)*pv(91)
 v(1713) = pv(10)*pv(91)
 v(1714) = pv(11)*pv(91)
 v(1715) = pv(12)*pv(91)
 v(1716) = pv(13)*pv(91)
 v(1717) = pv(14)*pv(91)
 v(1718) = pv(0)*pv(92)
 v(1719) = pv(1)*pv(92)
 v(1720) = pv(2)*pv(92)
 v(1721) = pv(3)*pv(92)
 v(1722) = pv(4)*pv(92)
 v(1723) = pv(6)*pv(92)
 v(1724) = pv(7)*pv(92)
 v(1725) = pv(8)*pv(92)
 v(1726) = pv(9)*pv(92)
 v(1727) = pv(13)*pv(92)
 v(1728) = pv(14)*pv(92)
 v(1729) = pv(0)*pv(93)
 v(1730) = pv(1)*pv(93)
 v(1731) = pv(2)*pv(93)
 v(1732) = pv(3)*pv(93)
 v(1733) = pv(4)*pv(93)
 v(1734) = pv(5)*pv(93)
 v(1735) = pv(6)*pv(93)
 v(1736) = pv(7)*pv(93)
 v(1737) = pv(8)*pv(93)
 v(1738) = pv(9)*pv(93)
 v(1739) = pv(10)*pv(93)
 v(1740) = pv(11)*pv(93)
 v(1741) = pv(12)*pv(93)
 v(1742) = pv(13)*pv(93)
 v(1743) = pv(14)*pv(93)
 v(1744) = pv(0)*pv(94)
 v(1745) = pv(1)*pv(94)
 v(1746) = pv(2)*pv(94)
 v(1747) = pv(3)*pv(94)
 v(1748) = pv(4)*pv(94)
 v(1749) = pv(5)*pv(94)
 v(1750) = pv(6)*pv(94)
 v(1751) = pv(7)*pv(94)
 v(1752) = pv(8)*pv(94)
 v(1753) = pv(9)*pv(94)
 v(1754) = pv(10)*pv(94)
 v(1755) = pv(11)*pv(94)
 v(1756) = pv(12)*pv(94)
 v(1757) = pv(13)*pv(94)
 v(1758) = pv(14)*pv(94)
 v(1759) = pv(0)*pv(95)
 v(1760) = pv(2)*pv(95)
 v(1761) = pv(3)*pv(95)
 v(1762) = pv(4)*pv(95)
 v(1763) = pv(6)*pv(95)
 v(1764) = pv(7)*pv(95)
 v(1765) = pv(8)*pv(95)
 v(1766) = pv(9)*pv(95)
 v(1767) = pv(13)*pv(95)
 v(1768) = pv(14)*pv(95)
 v(1769) = pv(0)*pv(96)
 v(1770) = pv(1)*pv(96)
 v(1771) = pv(2)*pv(96)
 v(1772) = pv(3)*pv(96)
 v(1773) = pv(4)*pv(96)
 v(1774) = pv(5)*pv(96)
 v(1775) = pv(6)*pv(96)
 v(1776) = pv(7)*pv(96)
 v(1777) = pv(8)*pv(96)
 v(1778) = pv(9)*pv(96)
 v(1779) = pv(13)*pv(96)
 v(1780) = pv(14)*pv(96)
 v(1781) = pv(0)*pv(97)
 v(1782) = pv(1)*pv(97)
 v(1783) = pv(2)*pv(97)
 v(1784) = pv(3)*pv(97)
 v(1785) = pv(4)*pv(97)
 v(1786) = pv(5)*pv(97)
 v(1787) = pv(6)*pv(97)
 v(1788) = pv(7)*pv(97)
 v(1789) = pv(8)*pv(97)
 v(1790) = pv(9)*pv(97)
 v(1791) = pv(10)*pv(97)
 v(1792) = pv(11)*pv(97)
 v(1793) = pv(12)*pv(97)
 v(1794) = pv(13)*pv(97)
 v(1795) = pv(14)*pv(97)
 v(1796) = pv(0)*pv(98)
 v(1797) = pv(1)*pv(98)
 v(1798) = pv(2)*pv(98)
 v(1799) = pv(3)*pv(98)
 v(1800) = pv(6)*pv(98)
 v(1801) = pv(7)*pv(98)
 v(1802) = pv(8)*pv(98)
 v(1803) = pv(9)*pv(98)
 v(1804) = pv(13)*pv(98)
 v(1805) = pv(14)*pv(98)
 v(1806) = pv(0)*pv(99)
 v(1807) = pv(1)*pv(99)
 v(1808) = pv(2)*pv(99)
 v(1809) = pv(3)*pv(99)
 v(1810) = pv(4)*pv(99)
 v(1811) = pv(6)*pv(99)
 v(1812) = pv(7)*pv(99)
 v(1813) = pv(8)*pv(99)
 v(1814) = pv(9)*pv(99)
 v(1815) = pv(13)*pv(99)
 v(1816) = pv(14)*pv(99)
 v(1817) = pv(0)*pv(100)
 v(1818) = pv(1)*pv(100)
 v(1819) = pv(2)*pv(100)
 v(1820) = pv(3)*pv(100)
 v(1821) = pv(4)*pv(100)
 v(1822) = pv(5)*pv(100)
 v(1823) = pv(7)*pv(100)
 v(1824) = pv(8)*pv(100)
 v(1825) = pv(9)*pv(100)
 v(1826) = pv(10)*pv(100)
 v(1827) = pv(11)*pv(100)
 v(1828) = pv(12)*pv(100)
 v(1829) = pv(0)*pv(101)
 v(1830) = pv(1)*pv(101)
 v(1831) = pv(2)*pv(101)
 v(1832) = pv(3)*pv(101)
 v(1833) = pv(4)*pv(101)
 v(1834) = pv(5)*pv(101)
 v(1835) = pv(7)*pv(101)
 v(1836) = pv(8)*pv(101)
 v(1837) = pv(9)*pv(101)
 v(1838) = pv(0)*pv(102)
 v(1839) = pv(1)*pv(102)
 v(1840) = pv(2)*pv(102)
 v(1841) = pv(3)*pv(102)
 v(1842) = pv(4)*pv(102)
 v(1843) = pv(5)*pv(102)
 v(1844) = pv(6)*pv(102)
 v(1845) = pv(7)*pv(102)
 v(1846) = pv(8)*pv(102)
 v(1847) = pv(9)*pv(102)
 v(1848) = pv(10)*pv(102)
 v(1849) = pv(11)*pv(102)
 v(1850) = pv(12)*pv(102)
 v(1851) = pv(13)*pv(102)
 v(1852) = pv(14)*pv(102)
 v(1853) = pv(0)*pv(103)
 v(1854) = pv(1)*pv(103)
 v(1855) = pv(2)*pv(103)
 v(1856) = pv(3)*pv(103)
 v(1857) = pv(4)*pv(103)
 v(1858) = pv(5)*pv(103)
 v(1859) = pv(6)*pv(103)
 v(1860) = pv(7)*pv(103)
 v(1861) = pv(8)*pv(103)
 v(1862) = pv(9)*pv(103)
 v(1863) = pv(10)*pv(103)
 v(1864) = pv(11)*pv(103)
 v(1865) = pv(12)*pv(103)
 v(1866) = pv(13)*pv(103)
 v(1867) = pv(14)*pv(103)
 v(1868) = pv(0)*pv(104)
 v(1869) = pv(1)*pv(104)
 v(1870) = pv(2)*pv(104)
 v(1871) = pv(3)*pv(104)
 v(1872) = pv(4)*pv(104)
 v(1873) = pv(5)*pv(104)
 v(1874) = pv(6)*pv(104)
 v(1875) = pv(7)*pv(104)
 v(1876) = pv(8)*pv(104)
 v(1877) = pv(9)*pv(104)
 v(1878) = pv(10)*pv(104)
 v(1879) = pv(11)*pv(104)
 v(1880) = pv(12)*pv(104)
 v(1881) = pv(13)*pv(104)
 v(1882) = pv(14)*pv(104)
 v(1883) = pv(0)*pv(105)
 v(1884) = pv(1)*pv(105)
 v(1885) = pv(2)*pv(105)
 v(1886) = pv(3)*pv(105)
 v(1887) = pv(4)*pv(105)
 v(1888) = pv(5)*pv(105)
 v(1889) = pv(6)*pv(105)
 v(1890) = pv(7)*pv(105)
 v(1891) = pv(8)*pv(105)
 v(1892) = pv(9)*pv(105)
 v(1893) = pv(13)*pv(105)
 v(1894) = pv(14)*pv(105)
 v(1895) = pv(0)*pv(106)
 v(1896) = pv(1)*pv(106)
 v(1897) = pv(2)*pv(106)
 v(1898) = pv(3)*pv(106)
 v(1899) = pv(4)*pv(106)
 v(1900) = pv(5)*pv(106)
 v(1901) = pv(6)*pv(106)
 v(1902) = pv(7)*pv(106)
 v(1903) = pv(8)*pv(106)
 v(1904) = pv(9)*pv(106)
 v(1905) = pv(10)*pv(106)
 v(1906) = pv(13)*pv(106)
 v(1907) = pv(14)*pv(106)
 v(1908) = pv(0)*pv(107)
 v(1909) = pv(1)*pv(107)
 v(1910) = pv(2)*pv(107)
 v(1911) = pv(3)*pv(107)
 v(1912) = pv(4)*pv(107)
 v(1913) = pv(5)*pv(107)
 v(1914) = pv(6)*pv(107)
 v(1915) = pv(7)*pv(107)
 v(1916) = pv(8)*pv(107)
 v(1917) = pv(9)*pv(107)
 v(1918) = pv(10)*pv(107)
 v(1919) = pv(11)*pv(107)
 v(1920) = pv(13)*pv(107)
 v(1921) = pv(14)*pv(107)
 v(1922) = pv(0)*pv(108)
 v(1923) = pv(1)*pv(108)
 v(1924) = pv(2)*pv(108)
 v(1925) = pv(3)*pv(108)
 v(1926) = pv(4)*pv(108)
 v(1927) = pv(5)*pv(108)
 v(1928) = pv(6)*pv(108)
 v(1929) = pv(7)*pv(108)
 v(1930) = pv(8)*pv(108)
 v(1931) = pv(9)*pv(108)
 v(1932) = pv(10)*pv(108)
 v(1933) = pv(11)*pv(108)
 v(1934) = pv(12)*pv(108)
 v(1935) = pv(0)*pv(109)
 v(1936) = pv(1)*pv(109)
 v(1937) = pv(2)*pv(109)
 v(1938) = pv(3)*pv(109)
 v(1939) = pv(4)*pv(109)
 v(1940) = pv(5)*pv(109)
 v(1941) = pv(6)*pv(109)
 v(1942) = pv(7)*pv(109)
 v(1943) = pv(8)*pv(109)
 v(1944) = pv(9)*pv(109)
 v(1945) = pv(10)*pv(109)
 v(1946) = pv(11)*pv(109)
 v(1947) = pv(12)*pv(109)
 v(1948) = pv(0)*pv(110)
 v(1949) = pv(1)*pv(110)
 v(1950) = pv(2)*pv(110)
 v(1951) = pv(3)*pv(110)
 v(1952) = pv(4)*pv(110)
 v(1953) = pv(5)*pv(110)
 v(1954) = pv(6)*pv(110)
 v(1955) = pv(7)*pv(110)
 v(1956) = pv(8)*pv(110)
 v(1957) = pv(9)*pv(110)
 v(1958) = pv(10)*pv(110)
 v(1959) = pv(11)*pv(110)
 v(1960) = pv(12)*pv(110)
 v(1961) = pv(0)*pv(111)
 v(1962) = pv(1)*pv(111)
 v(1963) = pv(2)*pv(111)
 v(1964) = pv(3)*pv(111)
 v(1965) = pv(4)*pv(111)
 v(1966) = pv(5)*pv(111)
 v(1967) = pv(6)*pv(111)
 v(1968) = pv(7)*pv(111)
 v(1969) = pv(8)*pv(111)
 v(1970) = pv(9)*pv(111)
 v(1971) = pv(10)*pv(111)
 v(1972) = pv(11)*pv(111)
 v(1973) = pv(12)*pv(111)
 v(1974) = pv(0)*pv(112)
 v(1975) = pv(1)*pv(112)
 v(1976) = pv(2)*pv(112)
 v(1977) = pv(3)*pv(112)
 v(1978) = pv(4)*pv(112)
 v(1979) = pv(5)*pv(112)
 v(1980) = pv(6)*pv(112)
 v(1981) = pv(7)*pv(112)
 v(1982) = pv(8)*pv(112)
 v(1983) = pv(9)*pv(112)
 v(1984) = pv(10)*pv(112)
 v(1985) = pv(11)*pv(112)
 v(1986) = pv(12)*pv(112)
 v(1987) = pv(0)*pv(113)
 v(1988) = pv(1)*pv(113)
 v(1989) = pv(2)*pv(113)
 v(1990) = pv(3)*pv(113)
 v(1991) = pv(4)*pv(113)
 v(1992) = pv(5)*pv(113)
 v(1993) = pv(6)*pv(113)
 v(1994) = pv(7)*pv(113)
 v(1995) = pv(8)*pv(113)
 v(1996) = pv(9)*pv(113)
 v(1997) = pv(10)*pv(113)
 v(1998) = pv(11)*pv(113)
 v(1999) = pv(12)*pv(113)
 v(2000) = pv(0)*pv(114)
 v(2001) = pv(1)*pv(114)
 v(2002) = pv(2)*pv(114)
 v(2003) = pv(3)*pv(114)
 v(2004) = pv(4)*pv(114)
 v(2005) = pv(5)*pv(114)
 v(2006) = pv(6)*pv(114)
 v(2007) = pv(7)*pv(114)
 v(2008) = pv(8)*pv(114)
 v(2009) = pv(9)*pv(114)
 v(2010) = pv(10)*pv(114)
 v(2011) = pv(11)*pv(114)
 v(2012) = pv(12)*pv(114)
 v(2013) = pv(0)*pv(115)
 v(2014) = pv(1)*pv(115)
 v(2015) = pv(2)*pv(115)
 v(2016) = pv(3)*pv(115)
 v(2017) = pv(4)*pv(115)
 v(2018) = pv(5)*pv(115)
 v(2019) = pv(6)*pv(115)
 v(2020) = pv(7)*pv(115)
 v(2021) = pv(8)*pv(115)
 v(2022) = pv(9)*pv(115)
 v(2023) = pv(10)*pv(115)
 v(2024) = pv(11)*pv(115)
 v(2025) = pv(12)*pv(115)
 v(2026:3495) = pv(520:1989)
endif
if (6.le.mxd) then
 stop 'mg4321: degree 6 not implemented'
endif
return
END SUBROUTINE mg4321_secs
