SUBROUTINE mg4222_secs (mxd, r, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: v(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: pv(0:size(v)-1) !! an upper bound size
if (size(r).ne.nk*nk.or.size(v).ne.mg4222_nsc(mxd)) then
 stop 'mg4222_secs: bad dimensions'
endif
call mg4222_isecs (mxd, r, pv)
v(0) = 1
if (2.le.mxd) then
 v(1:15) = pv(0:14)
endif
if (3.le.mxd) then
 v(16:123) = pv(15:122)
endif
if (4.le.mxd) then
 v(124) = pv(0)*pv(0)
 v(125) = pv(0)*pv(1)
 v(126) = pv(1)*pv(1)
 v(127) = pv(0)*pv(2)
 v(128) = pv(1)*pv(2)
 v(129) = pv(2)*pv(2)
 v(130) = pv(0)*pv(3)
 v(131) = pv(1)*pv(3)
 v(132) = pv(2)*pv(3)
 v(133) = pv(0)*pv(4)
 v(134) = pv(1)*pv(4)
 v(135) = pv(2)*pv(4)
 v(136) = pv(3)*pv(4)
 v(137) = pv(0)*pv(5)
 v(138) = pv(1)*pv(5)
 v(139) = pv(2)*pv(5)
 v(140) = pv(3)*pv(5)
 v(141) = pv(4)*pv(5)
 v(142) = pv(5)*pv(5)
 v(143) = pv(0)*pv(6)
 v(144) = pv(1)*pv(6)
 v(145) = pv(2)*pv(6)
 v(146) = pv(3)*pv(6)
 v(147) = pv(4)*pv(6)
 v(148) = pv(5)*pv(6)
 v(149) = pv(6)*pv(6)
 v(150) = pv(0)*pv(7)
 v(151) = pv(1)*pv(7)
 v(152) = pv(2)*pv(7)
 v(153) = pv(3)*pv(7)
 v(154) = pv(4)*pv(7)
 v(155) = pv(5)*pv(7)
 v(156) = pv(6)*pv(7)
 v(157) = pv(7)*pv(7)
 v(158) = pv(0)*pv(8)
 v(159) = pv(1)*pv(8)
 v(160) = pv(2)*pv(8)
 v(161) = pv(4)*pv(8)
 v(162) = pv(5)*pv(8)
 v(163) = pv(6)*pv(8)
 v(164) = pv(7)*pv(8)
 v(165) = pv(0)*pv(9)
 v(166) = pv(1)*pv(9)
 v(167) = pv(2)*pv(9)
 v(168) = pv(4)*pv(9)
 v(169) = pv(5)*pv(9)
 v(170) = pv(6)*pv(9)
 v(171) = pv(7)*pv(9)
 v(172) = pv(0)*pv(10)
 v(173) = pv(1)*pv(10)
 v(174) = pv(2)*pv(10)
 v(175) = pv(3)*pv(10)
 v(176) = pv(4)*pv(10)
 v(177) = pv(5)*pv(10)
 v(178) = pv(6)*pv(10)
 v(179) = pv(7)*pv(10)
 v(180) = pv(8)*pv(10)
 v(181) = pv(9)*pv(10)
 v(182) = pv(0)*pv(11)
 v(183) = pv(1)*pv(11)
 v(184) = pv(2)*pv(11)
 v(185) = pv(3)*pv(11)
 v(186) = pv(5)*pv(11)
 v(187) = pv(6)*pv(11)
 v(188) = pv(7)*pv(11)
 v(189) = pv(8)*pv(11)
 v(190) = pv(9)*pv(11)
 v(191) = pv(10)*pv(11)
 v(192) = pv(0)*pv(12)
 v(193) = pv(1)*pv(12)
 v(194) = pv(2)*pv(12)
 v(195) = pv(3)*pv(12)
 v(196) = pv(5)*pv(12)
 v(197) = pv(6)*pv(12)
 v(198) = pv(7)*pv(12)
 v(199) = pv(8)*pv(12)
 v(200) = pv(9)*pv(12)
 v(201) = pv(10)*pv(12)
 v(202) = pv(0)*pv(13)
 v(203) = pv(1)*pv(13)
 v(204) = pv(2)*pv(13)
 v(205) = pv(3)*pv(13)
 v(206) = pv(4)*pv(13)
 v(207) = pv(5)*pv(13)
 v(208) = pv(6)*pv(13)
 v(209) = pv(7)*pv(13)
 v(210) = pv(8)*pv(13)
 v(211) = pv(9)*pv(13)
 v(212) = pv(11)*pv(13)
 v(213) = pv(12)*pv(13)
 v(214) = pv(0)*pv(14)
 v(215) = pv(1)*pv(14)
 v(216) = pv(2)*pv(14)
 v(217) = pv(3)*pv(14)
 v(218) = pv(4)*pv(14)
 v(219) = pv(5)*pv(14)
 v(220) = pv(6)*pv(14)
 v(221) = pv(7)*pv(14)
 v(222) = pv(8)*pv(14)
 v(223) = pv(9)*pv(14)
 v(224) = pv(11)*pv(14)
 v(225) = pv(12)*pv(14)
 v(226:664) = pv(123:561)
endif
if (5.le.mxd) then
 v(665) = pv(0)*pv(15)
 v(666) = pv(1)*pv(15)
 v(667) = pv(2)*pv(15)
 v(668) = pv(3)*pv(15)
 v(669) = pv(4)*pv(15)
 v(670) = pv(5)*pv(15)
 v(671) = pv(6)*pv(15)
 v(672) = pv(7)*pv(15)
 v(673) = pv(8)*pv(15)
 v(674) = pv(9)*pv(15)
 v(675) = pv(10)*pv(15)
 v(676) = pv(11)*pv(15)
 v(677) = pv(12)*pv(15)
 v(678) = pv(13)*pv(15)
 v(679) = pv(14)*pv(15)
 v(680) = pv(0)*pv(16)
 v(681) = pv(1)*pv(16)
 v(682) = pv(2)*pv(16)
 v(683) = pv(3)*pv(16)
 v(684) = pv(4)*pv(16)
 v(685) = pv(5)*pv(16)
 v(686) = pv(6)*pv(16)
 v(687) = pv(7)*pv(16)
 v(688) = pv(8)*pv(16)
 v(689) = pv(9)*pv(16)
 v(690) = pv(10)*pv(16)
 v(691) = pv(11)*pv(16)
 v(692) = pv(12)*pv(16)
 v(693) = pv(13)*pv(16)
 v(694) = pv(14)*pv(16)
 v(695) = pv(0)*pv(17)
 v(696) = pv(1)*pv(17)
 v(697) = pv(2)*pv(17)
 v(698) = pv(3)*pv(17)
 v(699) = pv(4)*pv(17)
 v(700) = pv(5)*pv(17)
 v(701) = pv(6)*pv(17)
 v(702) = pv(7)*pv(17)
 v(703) = pv(8)*pv(17)
 v(704) = pv(9)*pv(17)
 v(705) = pv(10)*pv(17)
 v(706) = pv(11)*pv(17)
 v(707) = pv(12)*pv(17)
 v(708) = pv(13)*pv(17)
 v(709) = pv(14)*pv(17)
 v(710) = pv(0)*pv(18)
 v(711) = pv(1)*pv(18)
 v(712) = pv(2)*pv(18)
 v(713) = pv(4)*pv(18)
 v(714) = pv(5)*pv(18)
 v(715) = pv(6)*pv(18)
 v(716) = pv(7)*pv(18)
 v(717) = pv(10)*pv(18)
 v(718) = pv(11)*pv(18)
 v(719) = pv(12)*pv(18)
 v(720) = pv(13)*pv(18)
 v(721) = pv(14)*pv(18)
 v(722) = pv(0)*pv(19)
 v(723) = pv(1)*pv(19)
 v(724) = pv(2)*pv(19)
 v(725) = pv(4)*pv(19)
 v(726) = pv(5)*pv(19)
 v(727) = pv(6)*pv(19)
 v(728) = pv(7)*pv(19)
 v(729) = pv(10)*pv(19)
 v(730) = pv(11)*pv(19)
 v(731) = pv(12)*pv(19)
 v(732) = pv(13)*pv(19)
 v(733) = pv(14)*pv(19)
 v(734) = pv(0)*pv(20)
 v(735) = pv(1)*pv(20)
 v(736) = pv(2)*pv(20)
 v(737) = pv(3)*pv(20)
 v(738) = pv(4)*pv(20)
 v(739) = pv(5)*pv(20)
 v(740) = pv(6)*pv(20)
 v(741) = pv(7)*pv(20)
 v(742) = pv(8)*pv(20)
 v(743) = pv(9)*pv(20)
 v(744) = pv(10)*pv(20)
 v(745) = pv(11)*pv(20)
 v(746) = pv(12)*pv(20)
 v(747) = pv(13)*pv(20)
 v(748) = pv(14)*pv(20)
 v(749) = pv(0)*pv(21)
 v(750) = pv(1)*pv(21)
 v(751) = pv(2)*pv(21)
 v(752) = pv(3)*pv(21)
 v(753) = pv(4)*pv(21)
 v(754) = pv(5)*pv(21)
 v(755) = pv(6)*pv(21)
 v(756) = pv(7)*pv(21)
 v(757) = pv(8)*pv(21)
 v(758) = pv(9)*pv(21)
 v(759) = pv(10)*pv(21)
 v(760) = pv(11)*pv(21)
 v(761) = pv(12)*pv(21)
 v(762) = pv(13)*pv(21)
 v(763) = pv(14)*pv(21)
 v(764) = pv(0)*pv(22)
 v(765) = pv(1)*pv(22)
 v(766) = pv(2)*pv(22)
 v(767) = pv(3)*pv(22)
 v(768) = pv(4)*pv(22)
 v(769) = pv(5)*pv(22)
 v(770) = pv(6)*pv(22)
 v(771) = pv(7)*pv(22)
 v(772) = pv(8)*pv(22)
 v(773) = pv(9)*pv(22)
 v(774) = pv(10)*pv(22)
 v(775) = pv(11)*pv(22)
 v(776) = pv(12)*pv(22)
 v(777) = pv(13)*pv(22)
 v(778) = pv(14)*pv(22)
 v(779) = pv(0)*pv(23)
 v(780) = pv(1)*pv(23)
 v(781) = pv(2)*pv(23)
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
 v(794) = pv(0)*pv(24)
 v(795) = pv(1)*pv(24)
 v(796) = pv(2)*pv(24)
 v(797) = pv(3)*pv(24)
 v(798) = pv(4)*pv(24)
 v(799) = pv(5)*pv(24)
 v(800) = pv(6)*pv(24)
 v(801) = pv(7)*pv(24)
 v(802) = pv(8)*pv(24)
 v(803) = pv(9)*pv(24)
 v(804) = pv(10)*pv(24)
 v(805) = pv(11)*pv(24)
 v(806) = pv(12)*pv(24)
 v(807) = pv(13)*pv(24)
 v(808) = pv(14)*pv(24)
 v(809) = pv(0)*pv(25)
 v(810) = pv(1)*pv(25)
 v(811) = pv(2)*pv(25)
 v(812) = pv(3)*pv(25)
 v(813) = pv(4)*pv(25)
 v(814) = pv(5)*pv(25)
 v(815) = pv(6)*pv(25)
 v(816) = pv(7)*pv(25)
 v(817) = pv(8)*pv(25)
 v(818) = pv(9)*pv(25)
 v(819) = pv(10)*pv(25)
 v(820) = pv(11)*pv(25)
 v(821) = pv(12)*pv(25)
 v(822) = pv(13)*pv(25)
 v(823) = pv(14)*pv(25)
 v(824) = pv(0)*pv(26)
 v(825) = pv(1)*pv(26)
 v(826) = pv(2)*pv(26)
 v(827) = pv(3)*pv(26)
 v(828) = pv(4)*pv(26)
 v(829) = pv(5)*pv(26)
 v(830) = pv(6)*pv(26)
 v(831) = pv(7)*pv(26)
 v(832) = pv(8)*pv(26)
 v(833) = pv(9)*pv(26)
 v(834) = pv(10)*pv(26)
 v(835) = pv(11)*pv(26)
 v(836) = pv(12)*pv(26)
 v(837) = pv(13)*pv(26)
 v(838) = pv(14)*pv(26)
 v(839) = pv(0)*pv(27)
 v(840) = pv(1)*pv(27)
 v(841) = pv(2)*pv(27)
 v(842) = pv(3)*pv(27)
 v(843) = pv(4)*pv(27)
 v(844) = pv(5)*pv(27)
 v(845) = pv(6)*pv(27)
 v(846) = pv(7)*pv(27)
 v(847) = pv(8)*pv(27)
 v(848) = pv(9)*pv(27)
 v(849) = pv(10)*pv(27)
 v(850) = pv(11)*pv(27)
 v(851) = pv(12)*pv(27)
 v(852) = pv(13)*pv(27)
 v(853) = pv(14)*pv(27)
 v(854) = pv(0)*pv(28)
 v(855) = pv(1)*pv(28)
 v(856) = pv(2)*pv(28)
 v(857) = pv(4)*pv(28)
 v(858) = pv(5)*pv(28)
 v(859) = pv(6)*pv(28)
 v(860) = pv(7)*pv(28)
 v(861) = pv(10)*pv(28)
 v(862) = pv(11)*pv(28)
 v(863) = pv(12)*pv(28)
 v(864) = pv(13)*pv(28)
 v(865) = pv(14)*pv(28)
 v(866) = pv(0)*pv(29)
 v(867) = pv(1)*pv(29)
 v(868) = pv(2)*pv(29)
 v(869) = pv(3)*pv(29)
 v(870) = pv(4)*pv(29)
 v(871) = pv(5)*pv(29)
 v(872) = pv(6)*pv(29)
 v(873) = pv(7)*pv(29)
 v(874) = pv(8)*pv(29)
 v(875) = pv(9)*pv(29)
 v(876) = pv(10)*pv(29)
 v(877) = pv(11)*pv(29)
 v(878) = pv(12)*pv(29)
 v(879) = pv(13)*pv(29)
 v(880) = pv(14)*pv(29)
 v(881) = pv(0)*pv(30)
 v(882) = pv(1)*pv(30)
 v(883) = pv(2)*pv(30)
 v(884) = pv(3)*pv(30)
 v(885) = pv(4)*pv(30)
 v(886) = pv(5)*pv(30)
 v(887) = pv(6)*pv(30)
 v(888) = pv(7)*pv(30)
 v(889) = pv(8)*pv(30)
 v(890) = pv(9)*pv(30)
 v(891) = pv(10)*pv(30)
 v(892) = pv(11)*pv(30)
 v(893) = pv(12)*pv(30)
 v(894) = pv(13)*pv(30)
 v(895) = pv(14)*pv(30)
 v(896) = pv(0)*pv(31)
 v(897) = pv(1)*pv(31)
 v(898) = pv(2)*pv(31)
 v(899) = pv(3)*pv(31)
 v(900) = pv(4)*pv(31)
 v(901) = pv(5)*pv(31)
 v(902) = pv(6)*pv(31)
 v(903) = pv(7)*pv(31)
 v(904) = pv(8)*pv(31)
 v(905) = pv(9)*pv(31)
 v(906) = pv(10)*pv(31)
 v(907) = pv(11)*pv(31)
 v(908) = pv(12)*pv(31)
 v(909) = pv(13)*pv(31)
 v(910) = pv(14)*pv(31)
 v(911) = pv(0)*pv(32)
 v(912) = pv(1)*pv(32)
 v(913) = pv(2)*pv(32)
 v(914) = pv(3)*pv(32)
 v(915) = pv(5)*pv(32)
 v(916) = pv(6)*pv(32)
 v(917) = pv(7)*pv(32)
 v(918) = pv(8)*pv(32)
 v(919) = pv(9)*pv(32)
 v(920) = pv(10)*pv(32)
 v(921) = pv(13)*pv(32)
 v(922) = pv(14)*pv(32)
 v(923) = pv(0)*pv(33)
 v(924) = pv(1)*pv(33)
 v(925) = pv(2)*pv(33)
 v(926) = pv(3)*pv(33)
 v(927) = pv(5)*pv(33)
 v(928) = pv(6)*pv(33)
 v(929) = pv(7)*pv(33)
 v(930) = pv(8)*pv(33)
 v(931) = pv(9)*pv(33)
 v(932) = pv(10)*pv(33)
 v(933) = pv(13)*pv(33)
 v(934) = pv(14)*pv(33)
 v(935) = pv(0)*pv(34)
 v(936) = pv(1)*pv(34)
 v(937) = pv(2)*pv(34)
 v(938) = pv(3)*pv(34)
 v(939) = pv(5)*pv(34)
 v(940) = pv(6)*pv(34)
 v(941) = pv(7)*pv(34)
 v(942) = pv(8)*pv(34)
 v(943) = pv(9)*pv(34)
 v(944) = pv(10)*pv(34)
 v(945) = pv(13)*pv(34)
 v(946) = pv(14)*pv(34)
 v(947) = pv(0)*pv(35)
 v(948) = pv(1)*pv(35)
 v(949) = pv(2)*pv(35)
 v(950) = pv(3)*pv(35)
 v(951) = pv(4)*pv(35)
 v(952) = pv(5)*pv(35)
 v(953) = pv(6)*pv(35)
 v(954) = pv(7)*pv(35)
 v(955) = pv(8)*pv(35)
 v(956) = pv(9)*pv(35)
 v(957) = pv(10)*pv(35)
 v(958) = pv(11)*pv(35)
 v(959) = pv(12)*pv(35)
 v(960) = pv(13)*pv(35)
 v(961) = pv(14)*pv(35)
 v(962) = pv(0)*pv(36)
 v(963) = pv(1)*pv(36)
 v(964) = pv(2)*pv(36)
 v(965) = pv(3)*pv(36)
 v(966) = pv(4)*pv(36)
 v(967) = pv(5)*pv(36)
 v(968) = pv(6)*pv(36)
 v(969) = pv(7)*pv(36)
 v(970) = pv(8)*pv(36)
 v(971) = pv(9)*pv(36)
 v(972) = pv(10)*pv(36)
 v(973) = pv(11)*pv(36)
 v(974) = pv(12)*pv(36)
 v(975) = pv(13)*pv(36)
 v(976) = pv(14)*pv(36)
 v(977) = pv(0)*pv(37)
 v(978) = pv(1)*pv(37)
 v(979) = pv(2)*pv(37)
 v(980) = pv(3)*pv(37)
 v(981) = pv(4)*pv(37)
 v(982) = pv(5)*pv(37)
 v(983) = pv(6)*pv(37)
 v(984) = pv(7)*pv(37)
 v(985) = pv(8)*pv(37)
 v(986) = pv(9)*pv(37)
 v(987) = pv(10)*pv(37)
 v(988) = pv(11)*pv(37)
 v(989) = pv(12)*pv(37)
 v(990) = pv(13)*pv(37)
 v(991) = pv(14)*pv(37)
 v(992) = pv(0)*pv(38)
 v(993) = pv(1)*pv(38)
 v(994) = pv(2)*pv(38)
 v(995) = pv(3)*pv(38)
 v(996) = pv(4)*pv(38)
 v(997) = pv(5)*pv(38)
 v(998) = pv(6)*pv(38)
 v(999) = pv(7)*pv(38)
 v(1000) = pv(8)*pv(38)
 v(1001) = pv(9)*pv(38)
 v(1002) = pv(10)*pv(38)
 v(1003) = pv(11)*pv(38)
 v(1004) = pv(12)*pv(38)
 v(1005) = pv(13)*pv(38)
 v(1006) = pv(14)*pv(38)
 v(1007) = pv(0)*pv(39)
 v(1008) = pv(1)*pv(39)
 v(1009) = pv(2)*pv(39)
 v(1010) = pv(3)*pv(39)
 v(1011) = pv(4)*pv(39)
 v(1012) = pv(5)*pv(39)
 v(1013) = pv(6)*pv(39)
 v(1014) = pv(7)*pv(39)
 v(1015) = pv(8)*pv(39)
 v(1016) = pv(9)*pv(39)
 v(1017) = pv(10)*pv(39)
 v(1018) = pv(11)*pv(39)
 v(1019) = pv(12)*pv(39)
 v(1020) = pv(13)*pv(39)
 v(1021) = pv(14)*pv(39)
 v(1022) = pv(0)*pv(40)
 v(1023) = pv(1)*pv(40)
 v(1024) = pv(2)*pv(40)
 v(1025) = pv(3)*pv(40)
 v(1026) = pv(4)*pv(40)
 v(1027) = pv(5)*pv(40)
 v(1028) = pv(6)*pv(40)
 v(1029) = pv(7)*pv(40)
 v(1030) = pv(8)*pv(40)
 v(1031) = pv(9)*pv(40)
 v(1032) = pv(10)*pv(40)
 v(1033) = pv(11)*pv(40)
 v(1034) = pv(12)*pv(40)
 v(1035) = pv(13)*pv(40)
 v(1036) = pv(14)*pv(40)
 v(1037) = pv(0)*pv(41)
 v(1038) = pv(1)*pv(41)
 v(1039) = pv(2)*pv(41)
 v(1040) = pv(3)*pv(41)
 v(1041) = pv(4)*pv(41)
 v(1042) = pv(5)*pv(41)
 v(1043) = pv(6)*pv(41)
 v(1044) = pv(7)*pv(41)
 v(1045) = pv(8)*pv(41)
 v(1046) = pv(9)*pv(41)
 v(1047) = pv(10)*pv(41)
 v(1048) = pv(11)*pv(41)
 v(1049) = pv(12)*pv(41)
 v(1050) = pv(13)*pv(41)
 v(1051) = pv(14)*pv(41)
 v(1052) = pv(0)*pv(42)
 v(1053) = pv(1)*pv(42)
 v(1054) = pv(2)*pv(42)
 v(1055) = pv(4)*pv(42)
 v(1056) = pv(5)*pv(42)
 v(1057) = pv(6)*pv(42)
 v(1058) = pv(7)*pv(42)
 v(1059) = pv(10)*pv(42)
 v(1060) = pv(11)*pv(42)
 v(1061) = pv(12)*pv(42)
 v(1062) = pv(13)*pv(42)
 v(1063) = pv(14)*pv(42)
 v(1064) = pv(0)*pv(43)
 v(1065) = pv(1)*pv(43)
 v(1066) = pv(2)*pv(43)
 v(1067) = pv(4)*pv(43)
 v(1068) = pv(5)*pv(43)
 v(1069) = pv(6)*pv(43)
 v(1070) = pv(7)*pv(43)
 v(1071) = pv(10)*pv(43)
 v(1072) = pv(11)*pv(43)
 v(1073) = pv(12)*pv(43)
 v(1074) = pv(13)*pv(43)
 v(1075) = pv(14)*pv(43)
 v(1076) = pv(0)*pv(44)
 v(1077) = pv(1)*pv(44)
 v(1078) = pv(2)*pv(44)
 v(1079) = pv(3)*pv(44)
 v(1080) = pv(5)*pv(44)
 v(1081) = pv(6)*pv(44)
 v(1082) = pv(7)*pv(44)
 v(1083) = pv(8)*pv(44)
 v(1084) = pv(9)*pv(44)
 v(1085) = pv(10)*pv(44)
 v(1086) = pv(13)*pv(44)
 v(1087) = pv(14)*pv(44)
 v(1088) = pv(0)*pv(45)
 v(1089) = pv(1)*pv(45)
 v(1090) = pv(2)*pv(45)
 v(1091) = pv(5)*pv(45)
 v(1092) = pv(6)*pv(45)
 v(1093) = pv(7)*pv(45)
 v(1094) = pv(10)*pv(45)
 v(1095) = pv(13)*pv(45)
 v(1096) = pv(14)*pv(45)
 v(1097) = pv(0)*pv(46)
 v(1098) = pv(1)*pv(46)
 v(1099) = pv(2)*pv(46)
 v(1100) = pv(5)*pv(46)
 v(1101) = pv(6)*pv(46)
 v(1102) = pv(7)*pv(46)
 v(1103) = pv(10)*pv(46)
 v(1104) = pv(13)*pv(46)
 v(1105) = pv(14)*pv(46)
 v(1106) = pv(0)*pv(47)
 v(1107) = pv(1)*pv(47)
 v(1108) = pv(2)*pv(47)
 v(1109) = pv(3)*pv(47)
 v(1110) = pv(4)*pv(47)
 v(1111) = pv(5)*pv(47)
 v(1112) = pv(6)*pv(47)
 v(1113) = pv(7)*pv(47)
 v(1114) = pv(8)*pv(47)
 v(1115) = pv(9)*pv(47)
 v(1116) = pv(10)*pv(47)
 v(1117) = pv(11)*pv(47)
 v(1118) = pv(12)*pv(47)
 v(1119) = pv(13)*pv(47)
 v(1120) = pv(14)*pv(47)
 v(1121) = pv(0)*pv(48)
 v(1122) = pv(1)*pv(48)
 v(1123) = pv(2)*pv(48)
 v(1124) = pv(3)*pv(48)
 v(1125) = pv(5)*pv(48)
 v(1126) = pv(6)*pv(48)
 v(1127) = pv(7)*pv(48)
 v(1128) = pv(8)*pv(48)
 v(1129) = pv(9)*pv(48)
 v(1130) = pv(10)*pv(48)
 v(1131) = pv(13)*pv(48)
 v(1132) = pv(14)*pv(48)
 v(1133) = pv(0)*pv(49)
 v(1134) = pv(1)*pv(49)
 v(1135) = pv(2)*pv(49)
 v(1136) = pv(3)*pv(49)
 v(1137) = pv(4)*pv(49)
 v(1138) = pv(5)*pv(49)
 v(1139) = pv(6)*pv(49)
 v(1140) = pv(7)*pv(49)
 v(1141) = pv(8)*pv(49)
 v(1142) = pv(9)*pv(49)
 v(1143) = pv(10)*pv(49)
 v(1144) = pv(11)*pv(49)
 v(1145) = pv(12)*pv(49)
 v(1146) = pv(13)*pv(49)
 v(1147) = pv(14)*pv(49)
 v(1148) = pv(0)*pv(50)
 v(1149) = pv(1)*pv(50)
 v(1150) = pv(2)*pv(50)
 v(1151) = pv(5)*pv(50)
 v(1152) = pv(6)*pv(50)
 v(1153) = pv(7)*pv(50)
 v(1154) = pv(10)*pv(50)
 v(1155) = pv(13)*pv(50)
 v(1156) = pv(14)*pv(50)
 v(1157) = pv(0)*pv(51)
 v(1158) = pv(1)*pv(51)
 v(1159) = pv(2)*pv(51)
 v(1160) = pv(5)*pv(51)
 v(1161) = pv(6)*pv(51)
 v(1162) = pv(7)*pv(51)
 v(1163) = pv(10)*pv(51)
 v(1164) = pv(13)*pv(51)
 v(1165) = pv(14)*pv(51)
 v(1166) = pv(0)*pv(52)
 v(1167) = pv(1)*pv(52)
 v(1168) = pv(2)*pv(52)
 v(1169) = pv(5)*pv(52)
 v(1170) = pv(6)*pv(52)
 v(1171) = pv(7)*pv(52)
 v(1172) = pv(10)*pv(52)
 v(1173) = pv(13)*pv(52)
 v(1174) = pv(14)*pv(52)
 v(1175) = pv(0)*pv(53)
 v(1176) = pv(1)*pv(53)
 v(1177) = pv(2)*pv(53)
 v(1178) = pv(3)*pv(53)
 v(1179) = pv(4)*pv(53)
 v(1180) = pv(5)*pv(53)
 v(1181) = pv(6)*pv(53)
 v(1182) = pv(7)*pv(53)
 v(1183) = pv(8)*pv(53)
 v(1184) = pv(9)*pv(53)
 v(1185) = pv(10)*pv(53)
 v(1186) = pv(11)*pv(53)
 v(1187) = pv(12)*pv(53)
 v(1188) = pv(13)*pv(53)
 v(1189) = pv(14)*pv(53)
 v(1190) = pv(0)*pv(54)
 v(1191) = pv(1)*pv(54)
 v(1192) = pv(2)*pv(54)
 v(1193) = pv(3)*pv(54)
 v(1194) = pv(4)*pv(54)
 v(1195) = pv(5)*pv(54)
 v(1196) = pv(6)*pv(54)
 v(1197) = pv(7)*pv(54)
 v(1198) = pv(8)*pv(54)
 v(1199) = pv(9)*pv(54)
 v(1200) = pv(10)*pv(54)
 v(1201) = pv(11)*pv(54)
 v(1202) = pv(12)*pv(54)
 v(1203) = pv(13)*pv(54)
 v(1204) = pv(14)*pv(54)
 v(1205) = pv(0)*pv(55)
 v(1206) = pv(1)*pv(55)
 v(1207) = pv(2)*pv(55)
 v(1208) = pv(3)*pv(55)
 v(1209) = pv(4)*pv(55)
 v(1210) = pv(5)*pv(55)
 v(1211) = pv(6)*pv(55)
 v(1212) = pv(7)*pv(55)
 v(1213) = pv(8)*pv(55)
 v(1214) = pv(9)*pv(55)
 v(1215) = pv(10)*pv(55)
 v(1216) = pv(11)*pv(55)
 v(1217) = pv(12)*pv(55)
 v(1218) = pv(13)*pv(55)
 v(1219) = pv(14)*pv(55)
 v(1220) = pv(0)*pv(56)
 v(1221) = pv(1)*pv(56)
 v(1222) = pv(2)*pv(56)
 v(1223) = pv(4)*pv(56)
 v(1224) = pv(5)*pv(56)
 v(1225) = pv(6)*pv(56)
 v(1226) = pv(7)*pv(56)
 v(1227) = pv(10)*pv(56)
 v(1228) = pv(11)*pv(56)
 v(1229) = pv(12)*pv(56)
 v(1230) = pv(13)*pv(56)
 v(1231) = pv(14)*pv(56)
 v(1232) = pv(0)*pv(57)
 v(1233) = pv(1)*pv(57)
 v(1234) = pv(2)*pv(57)
 v(1235) = pv(3)*pv(57)
 v(1236) = pv(4)*pv(57)
 v(1237) = pv(5)*pv(57)
 v(1238) = pv(6)*pv(57)
 v(1239) = pv(7)*pv(57)
 v(1240) = pv(8)*pv(57)
 v(1241) = pv(9)*pv(57)
 v(1242) = pv(10)*pv(57)
 v(1243) = pv(11)*pv(57)
 v(1244) = pv(12)*pv(57)
 v(1245) = pv(13)*pv(57)
 v(1246) = pv(14)*pv(57)
 v(1247) = pv(0)*pv(58)
 v(1248) = pv(1)*pv(58)
 v(1249) = pv(2)*pv(58)
 v(1250) = pv(3)*pv(58)
 v(1251) = pv(4)*pv(58)
 v(1252) = pv(5)*pv(58)
 v(1253) = pv(6)*pv(58)
 v(1254) = pv(7)*pv(58)
 v(1255) = pv(8)*pv(58)
 v(1256) = pv(9)*pv(58)
 v(1257) = pv(10)*pv(58)
 v(1258) = pv(11)*pv(58)
 v(1259) = pv(12)*pv(58)
 v(1260) = pv(13)*pv(58)
 v(1261) = pv(14)*pv(58)
 v(1262) = pv(0)*pv(59)
 v(1263) = pv(1)*pv(59)
 v(1264) = pv(2)*pv(59)
 v(1265) = pv(3)*pv(59)
 v(1266) = pv(4)*pv(59)
 v(1267) = pv(5)*pv(59)
 v(1268) = pv(6)*pv(59)
 v(1269) = pv(7)*pv(59)
 v(1270) = pv(8)*pv(59)
 v(1271) = pv(9)*pv(59)
 v(1272) = pv(10)*pv(59)
 v(1273) = pv(11)*pv(59)
 v(1274) = pv(12)*pv(59)
 v(1275) = pv(13)*pv(59)
 v(1276) = pv(14)*pv(59)
 v(1277) = pv(0)*pv(60)
 v(1278) = pv(1)*pv(60)
 v(1279) = pv(2)*pv(60)
 v(1280) = pv(3)*pv(60)
 v(1281) = pv(4)*pv(60)
 v(1282) = pv(5)*pv(60)
 v(1283) = pv(6)*pv(60)
 v(1284) = pv(7)*pv(60)
 v(1285) = pv(8)*pv(60)
 v(1286) = pv(9)*pv(60)
 v(1287) = pv(10)*pv(60)
 v(1288) = pv(11)*pv(60)
 v(1289) = pv(12)*pv(60)
 v(1290) = pv(13)*pv(60)
 v(1291) = pv(14)*pv(60)
 v(1292) = pv(0)*pv(61)
 v(1293) = pv(1)*pv(61)
 v(1294) = pv(2)*pv(61)
 v(1295) = pv(3)*pv(61)
 v(1296) = pv(4)*pv(61)
 v(1297) = pv(5)*pv(61)
 v(1298) = pv(6)*pv(61)
 v(1299) = pv(7)*pv(61)
 v(1300) = pv(8)*pv(61)
 v(1301) = pv(9)*pv(61)
 v(1302) = pv(10)*pv(61)
 v(1303) = pv(11)*pv(61)
 v(1304) = pv(12)*pv(61)
 v(1305) = pv(13)*pv(61)
 v(1306) = pv(14)*pv(61)
 v(1307) = pv(0)*pv(62)
 v(1308) = pv(1)*pv(62)
 v(1309) = pv(2)*pv(62)
 v(1310) = pv(3)*pv(62)
 v(1311) = pv(5)*pv(62)
 v(1312) = pv(6)*pv(62)
 v(1313) = pv(7)*pv(62)
 v(1314) = pv(8)*pv(62)
 v(1315) = pv(9)*pv(62)
 v(1316) = pv(10)*pv(62)
 v(1317) = pv(13)*pv(62)
 v(1318) = pv(14)*pv(62)
 v(1319) = pv(0)*pv(63)
 v(1320) = pv(1)*pv(63)
 v(1321) = pv(2)*pv(63)
 v(1322) = pv(3)*pv(63)
 v(1323) = pv(4)*pv(63)
 v(1324) = pv(5)*pv(63)
 v(1325) = pv(6)*pv(63)
 v(1326) = pv(7)*pv(63)
 v(1327) = pv(8)*pv(63)
 v(1328) = pv(9)*pv(63)
 v(1329) = pv(10)*pv(63)
 v(1330) = pv(11)*pv(63)
 v(1331) = pv(12)*pv(63)
 v(1332) = pv(13)*pv(63)
 v(1333) = pv(14)*pv(63)
 v(1334) = pv(0)*pv(64)
 v(1335) = pv(1)*pv(64)
 v(1336) = pv(2)*pv(64)
 v(1337) = pv(3)*pv(64)
 v(1338) = pv(4)*pv(64)
 v(1339) = pv(5)*pv(64)
 v(1340) = pv(6)*pv(64)
 v(1341) = pv(7)*pv(64)
 v(1342) = pv(8)*pv(64)
 v(1343) = pv(9)*pv(64)
 v(1344) = pv(10)*pv(64)
 v(1345) = pv(11)*pv(64)
 v(1346) = pv(12)*pv(64)
 v(1347) = pv(13)*pv(64)
 v(1348) = pv(14)*pv(64)
 v(1349) = pv(0)*pv(65)
 v(1350) = pv(1)*pv(65)
 v(1351) = pv(2)*pv(65)
 v(1352) = pv(3)*pv(65)
 v(1353) = pv(4)*pv(65)
 v(1354) = pv(5)*pv(65)
 v(1355) = pv(6)*pv(65)
 v(1356) = pv(7)*pv(65)
 v(1357) = pv(8)*pv(65)
 v(1358) = pv(9)*pv(65)
 v(1359) = pv(10)*pv(65)
 v(1360) = pv(11)*pv(65)
 v(1361) = pv(12)*pv(65)
 v(1362) = pv(13)*pv(65)
 v(1363) = pv(14)*pv(65)
 v(1364) = pv(0)*pv(66)
 v(1365) = pv(1)*pv(66)
 v(1366) = pv(2)*pv(66)
 v(1367) = pv(4)*pv(66)
 v(1368) = pv(5)*pv(66)
 v(1369) = pv(6)*pv(66)
 v(1370) = pv(7)*pv(66)
 v(1371) = pv(10)*pv(66)
 v(1372) = pv(11)*pv(66)
 v(1373) = pv(12)*pv(66)
 v(1374) = pv(13)*pv(66)
 v(1375) = pv(14)*pv(66)
 v(1376) = pv(0)*pv(67)
 v(1377) = pv(1)*pv(67)
 v(1378) = pv(2)*pv(67)
 v(1379) = pv(3)*pv(67)
 v(1380) = pv(5)*pv(67)
 v(1381) = pv(6)*pv(67)
 v(1382) = pv(7)*pv(67)
 v(1383) = pv(8)*pv(67)
 v(1384) = pv(9)*pv(67)
 v(1385) = pv(10)*pv(67)
 v(1386) = pv(13)*pv(67)
 v(1387) = pv(14)*pv(67)
 v(1388) = pv(0)*pv(68)
 v(1389) = pv(1)*pv(68)
 v(1390) = pv(2)*pv(68)
 v(1391) = pv(3)*pv(68)
 v(1392) = pv(4)*pv(68)
 v(1393) = pv(5)*pv(68)
 v(1394) = pv(6)*pv(68)
 v(1395) = pv(7)*pv(68)
 v(1396) = pv(8)*pv(68)
 v(1397) = pv(9)*pv(68)
 v(1398) = pv(11)*pv(68)
 v(1399) = pv(12)*pv(68)
 v(1400) = pv(0)*pv(69)
 v(1401) = pv(1)*pv(69)
 v(1402) = pv(2)*pv(69)
 v(1403) = pv(3)*pv(69)
 v(1404) = pv(4)*pv(69)
 v(1405) = pv(5)*pv(69)
 v(1406) = pv(6)*pv(69)
 v(1407) = pv(7)*pv(69)
 v(1408) = pv(8)*pv(69)
 v(1409) = pv(9)*pv(69)
 v(1410) = pv(11)*pv(69)
 v(1411) = pv(12)*pv(69)
 v(1412) = pv(0)*pv(70)
 v(1413) = pv(1)*pv(70)
 v(1414) = pv(2)*pv(70)
 v(1415) = pv(3)*pv(70)
 v(1416) = pv(4)*pv(70)
 v(1417) = pv(5)*pv(70)
 v(1418) = pv(6)*pv(70)
 v(1419) = pv(7)*pv(70)
 v(1420) = pv(8)*pv(70)
 v(1421) = pv(9)*pv(70)
 v(1422) = pv(11)*pv(70)
 v(1423) = pv(12)*pv(70)
 v(1424) = pv(0)*pv(71)
 v(1425) = pv(1)*pv(71)
 v(1426) = pv(2)*pv(71)
 v(1427) = pv(3)*pv(71)
 v(1428) = pv(4)*pv(71)
 v(1429) = pv(5)*pv(71)
 v(1430) = pv(6)*pv(71)
 v(1431) = pv(7)*pv(71)
 v(1432) = pv(8)*pv(71)
 v(1433) = pv(9)*pv(71)
 v(1434) = pv(11)*pv(71)
 v(1435) = pv(12)*pv(71)
 v(1436) = pv(0)*pv(72)
 v(1437) = pv(1)*pv(72)
 v(1438) = pv(2)*pv(72)
 v(1439) = pv(3)*pv(72)
 v(1440) = pv(4)*pv(72)
 v(1441) = pv(5)*pv(72)
 v(1442) = pv(6)*pv(72)
 v(1443) = pv(7)*pv(72)
 v(1444) = pv(8)*pv(72)
 v(1445) = pv(9)*pv(72)
 v(1446) = pv(10)*pv(72)
 v(1447) = pv(11)*pv(72)
 v(1448) = pv(12)*pv(72)
 v(1449) = pv(13)*pv(72)
 v(1450) = pv(14)*pv(72)
 v(1451) = pv(0)*pv(73)
 v(1452) = pv(1)*pv(73)
 v(1453) = pv(2)*pv(73)
 v(1454) = pv(3)*pv(73)
 v(1455) = pv(4)*pv(73)
 v(1456) = pv(5)*pv(73)
 v(1457) = pv(6)*pv(73)
 v(1458) = pv(7)*pv(73)
 v(1459) = pv(8)*pv(73)
 v(1460) = pv(9)*pv(73)
 v(1461) = pv(10)*pv(73)
 v(1462) = pv(11)*pv(73)
 v(1463) = pv(12)*pv(73)
 v(1464) = pv(13)*pv(73)
 v(1465) = pv(14)*pv(73)
 v(1466) = pv(0)*pv(74)
 v(1467) = pv(1)*pv(74)
 v(1468) = pv(2)*pv(74)
 v(1469) = pv(3)*pv(74)
 v(1470) = pv(4)*pv(74)
 v(1471) = pv(5)*pv(74)
 v(1472) = pv(6)*pv(74)
 v(1473) = pv(7)*pv(74)
 v(1474) = pv(8)*pv(74)
 v(1475) = pv(9)*pv(74)
 v(1476) = pv(10)*pv(74)
 v(1477) = pv(11)*pv(74)
 v(1478) = pv(12)*pv(74)
 v(1479) = pv(13)*pv(74)
 v(1480) = pv(14)*pv(74)
 v(1481) = pv(0)*pv(75)
 v(1482) = pv(1)*pv(75)
 v(1483) = pv(2)*pv(75)
 v(1484) = pv(3)*pv(75)
 v(1485) = pv(4)*pv(75)
 v(1486) = pv(5)*pv(75)
 v(1487) = pv(6)*pv(75)
 v(1488) = pv(7)*pv(75)
 v(1489) = pv(8)*pv(75)
 v(1490) = pv(9)*pv(75)
 v(1491) = pv(10)*pv(75)
 v(1492) = pv(11)*pv(75)
 v(1493) = pv(12)*pv(75)
 v(1494) = pv(13)*pv(75)
 v(1495) = pv(14)*pv(75)
 v(1496) = pv(0)*pv(76)
 v(1497) = pv(1)*pv(76)
 v(1498) = pv(2)*pv(76)
 v(1499) = pv(3)*pv(76)
 v(1500) = pv(4)*pv(76)
 v(1501) = pv(5)*pv(76)
 v(1502) = pv(6)*pv(76)
 v(1503) = pv(7)*pv(76)
 v(1504) = pv(8)*pv(76)
 v(1505) = pv(9)*pv(76)
 v(1506) = pv(10)*pv(76)
 v(1507) = pv(11)*pv(76)
 v(1508) = pv(12)*pv(76)
 v(1509) = pv(13)*pv(76)
 v(1510) = pv(14)*pv(76)
 v(1511) = pv(0)*pv(77)
 v(1512) = pv(1)*pv(77)
 v(1513) = pv(2)*pv(77)
 v(1514) = pv(3)*pv(77)
 v(1515) = pv(4)*pv(77)
 v(1516) = pv(5)*pv(77)
 v(1517) = pv(6)*pv(77)
 v(1518) = pv(7)*pv(77)
 v(1519) = pv(8)*pv(77)
 v(1520) = pv(9)*pv(77)
 v(1521) = pv(10)*pv(77)
 v(1522) = pv(11)*pv(77)
 v(1523) = pv(12)*pv(77)
 v(1524) = pv(13)*pv(77)
 v(1525) = pv(14)*pv(77)
 v(1526) = pv(0)*pv(78)
 v(1527) = pv(1)*pv(78)
 v(1528) = pv(2)*pv(78)
 v(1529) = pv(3)*pv(78)
 v(1530) = pv(4)*pv(78)
 v(1531) = pv(5)*pv(78)
 v(1532) = pv(6)*pv(78)
 v(1533) = pv(7)*pv(78)
 v(1534) = pv(8)*pv(78)
 v(1535) = pv(9)*pv(78)
 v(1536) = pv(10)*pv(78)
 v(1537) = pv(11)*pv(78)
 v(1538) = pv(12)*pv(78)
 v(1539) = pv(13)*pv(78)
 v(1540) = pv(14)*pv(78)
 v(1541) = pv(0)*pv(79)
 v(1542) = pv(1)*pv(79)
 v(1543) = pv(2)*pv(79)
 v(1544) = pv(3)*pv(79)
 v(1545) = pv(4)*pv(79)
 v(1546) = pv(5)*pv(79)
 v(1547) = pv(6)*pv(79)
 v(1548) = pv(7)*pv(79)
 v(1549) = pv(8)*pv(79)
 v(1550) = pv(9)*pv(79)
 v(1551) = pv(10)*pv(79)
 v(1552) = pv(11)*pv(79)
 v(1553) = pv(12)*pv(79)
 v(1554) = pv(13)*pv(79)
 v(1555) = pv(14)*pv(79)
 v(1556) = pv(0)*pv(80)
 v(1557) = pv(1)*pv(80)
 v(1558) = pv(2)*pv(80)
 v(1559) = pv(3)*pv(80)
 v(1560) = pv(4)*pv(80)
 v(1561) = pv(5)*pv(80)
 v(1562) = pv(6)*pv(80)
 v(1563) = pv(7)*pv(80)
 v(1564) = pv(8)*pv(80)
 v(1565) = pv(9)*pv(80)
 v(1566) = pv(10)*pv(80)
 v(1567) = pv(11)*pv(80)
 v(1568) = pv(12)*pv(80)
 v(1569) = pv(13)*pv(80)
 v(1570) = pv(14)*pv(80)
 v(1571) = pv(0)*pv(81)
 v(1572) = pv(1)*pv(81)
 v(1573) = pv(2)*pv(81)
 v(1574) = pv(3)*pv(81)
 v(1575) = pv(4)*pv(81)
 v(1576) = pv(5)*pv(81)
 v(1577) = pv(6)*pv(81)
 v(1578) = pv(7)*pv(81)
 v(1579) = pv(10)*pv(81)
 v(1580) = pv(11)*pv(81)
 v(1581) = pv(12)*pv(81)
 v(1582) = pv(13)*pv(81)
 v(1583) = pv(14)*pv(81)
 v(1584) = pv(0)*pv(82)
 v(1585) = pv(1)*pv(82)
 v(1586) = pv(2)*pv(82)
 v(1587) = pv(3)*pv(82)
 v(1588) = pv(4)*pv(82)
 v(1589) = pv(5)*pv(82)
 v(1590) = pv(6)*pv(82)
 v(1591) = pv(7)*pv(82)
 v(1592) = pv(10)*pv(82)
 v(1593) = pv(11)*pv(82)
 v(1594) = pv(12)*pv(82)
 v(1595) = pv(13)*pv(82)
 v(1596) = pv(14)*pv(82)
 v(1597) = pv(0)*pv(83)
 v(1598) = pv(1)*pv(83)
 v(1599) = pv(2)*pv(83)
 v(1600) = pv(3)*pv(83)
 v(1601) = pv(4)*pv(83)
 v(1602) = pv(5)*pv(83)
 v(1603) = pv(6)*pv(83)
 v(1604) = pv(7)*pv(83)
 v(1605) = pv(10)*pv(83)
 v(1606) = pv(11)*pv(83)
 v(1607) = pv(12)*pv(83)
 v(1608) = pv(13)*pv(83)
 v(1609) = pv(14)*pv(83)
 v(1610) = pv(0)*pv(84)
 v(1611) = pv(1)*pv(84)
 v(1612) = pv(2)*pv(84)
 v(1613) = pv(3)*pv(84)
 v(1614) = pv(5)*pv(84)
 v(1615) = pv(6)*pv(84)
 v(1616) = pv(7)*pv(84)
 v(1617) = pv(10)*pv(84)
 v(1618) = pv(13)*pv(84)
 v(1619) = pv(14)*pv(84)
 v(1620) = pv(0)*pv(85)
 v(1621) = pv(1)*pv(85)
 v(1622) = pv(2)*pv(85)
 v(1623) = pv(3)*pv(85)
 v(1624) = pv(5)*pv(85)
 v(1625) = pv(6)*pv(85)
 v(1626) = pv(7)*pv(85)
 v(1627) = pv(10)*pv(85)
 v(1628) = pv(13)*pv(85)
 v(1629) = pv(14)*pv(85)
 v(1630) = pv(0)*pv(86)
 v(1631) = pv(1)*pv(86)
 v(1632) = pv(2)*pv(86)
 v(1633) = pv(3)*pv(86)
 v(1634) = pv(4)*pv(86)
 v(1635) = pv(5)*pv(86)
 v(1636) = pv(6)*pv(86)
 v(1637) = pv(7)*pv(86)
 v(1638) = pv(8)*pv(86)
 v(1639) = pv(9)*pv(86)
 v(1640) = pv(11)*pv(86)
 v(1641) = pv(12)*pv(86)
 v(1642) = pv(0)*pv(87)
 v(1643) = pv(1)*pv(87)
 v(1644) = pv(2)*pv(87)
 v(1645) = pv(4)*pv(87)
 v(1646) = pv(5)*pv(87)
 v(1647) = pv(6)*pv(87)
 v(1648) = pv(7)*pv(87)
 v(1649) = pv(11)*pv(87)
 v(1650) = pv(12)*pv(87)
 v(1651) = pv(0)*pv(88)
 v(1652) = pv(1)*pv(88)
 v(1653) = pv(2)*pv(88)
 v(1654) = pv(3)*pv(88)
 v(1655) = pv(4)*pv(88)
 v(1656) = pv(5)*pv(88)
 v(1657) = pv(6)*pv(88)
 v(1658) = pv(7)*pv(88)
 v(1659) = pv(11)*pv(88)
 v(1660) = pv(12)*pv(88)
 v(1661) = pv(0)*pv(89)
 v(1662) = pv(1)*pv(89)
 v(1663) = pv(2)*pv(89)
 v(1664) = pv(3)*pv(89)
 v(1665) = pv(4)*pv(89)
 v(1666) = pv(5)*pv(89)
 v(1667) = pv(6)*pv(89)
 v(1668) = pv(7)*pv(89)
 v(1669) = pv(8)*pv(89)
 v(1670) = pv(9)*pv(89)
 v(1671) = pv(10)*pv(89)
 v(1672) = pv(11)*pv(89)
 v(1673) = pv(12)*pv(89)
 v(1674) = pv(13)*pv(89)
 v(1675) = pv(14)*pv(89)
 v(1676) = pv(0)*pv(90)
 v(1677) = pv(1)*pv(90)
 v(1678) = pv(2)*pv(90)
 v(1679) = pv(3)*pv(90)
 v(1680) = pv(4)*pv(90)
 v(1681) = pv(5)*pv(90)
 v(1682) = pv(6)*pv(90)
 v(1683) = pv(7)*pv(90)
 v(1684) = pv(8)*pv(90)
 v(1685) = pv(9)*pv(90)
 v(1686) = pv(11)*pv(90)
 v(1687) = pv(12)*pv(90)
 v(1688) = pv(0)*pv(91)
 v(1689) = pv(1)*pv(91)
 v(1690) = pv(2)*pv(91)
 v(1691) = pv(4)*pv(91)
 v(1692) = pv(5)*pv(91)
 v(1693) = pv(6)*pv(91)
 v(1694) = pv(7)*pv(91)
 v(1695) = pv(11)*pv(91)
 v(1696) = pv(12)*pv(91)
 v(1697) = pv(0)*pv(92)
 v(1698) = pv(1)*pv(92)
 v(1699) = pv(2)*pv(92)
 v(1700) = pv(3)*pv(92)
 v(1701) = pv(4)*pv(92)
 v(1702) = pv(5)*pv(92)
 v(1703) = pv(6)*pv(92)
 v(1704) = pv(7)*pv(92)
 v(1705) = pv(8)*pv(92)
 v(1706) = pv(9)*pv(92)
 v(1707) = pv(11)*pv(92)
 v(1708) = pv(12)*pv(92)
 v(1709) = pv(0)*pv(93)
 v(1710) = pv(1)*pv(93)
 v(1711) = pv(2)*pv(93)
 v(1712) = pv(3)*pv(93)
 v(1713) = pv(4)*pv(93)
 v(1714) = pv(5)*pv(93)
 v(1715) = pv(6)*pv(93)
 v(1716) = pv(7)*pv(93)
 v(1717) = pv(8)*pv(93)
 v(1718) = pv(9)*pv(93)
 v(1719) = pv(10)*pv(93)
 v(1720) = pv(11)*pv(93)
 v(1721) = pv(12)*pv(93)
 v(1722) = pv(13)*pv(93)
 v(1723) = pv(14)*pv(93)
 v(1724) = pv(0)*pv(94)
 v(1725) = pv(1)*pv(94)
 v(1726) = pv(2)*pv(94)
 v(1727) = pv(4)*pv(94)
 v(1728) = pv(5)*pv(94)
 v(1729) = pv(6)*pv(94)
 v(1730) = pv(7)*pv(94)
 v(1731) = pv(11)*pv(94)
 v(1732) = pv(12)*pv(94)
 v(1733) = pv(0)*pv(95)
 v(1734) = pv(1)*pv(95)
 v(1735) = pv(2)*pv(95)
 v(1736) = pv(4)*pv(95)
 v(1737) = pv(5)*pv(95)
 v(1738) = pv(6)*pv(95)
 v(1739) = pv(7)*pv(95)
 v(1740) = pv(11)*pv(95)
 v(1741) = pv(12)*pv(95)
 v(1742) = pv(0)*pv(96)
 v(1743) = pv(1)*pv(96)
 v(1744) = pv(2)*pv(96)
 v(1745) = pv(3)*pv(96)
 v(1746) = pv(4)*pv(96)
 v(1747) = pv(5)*pv(96)
 v(1748) = pv(6)*pv(96)
 v(1749) = pv(7)*pv(96)
 v(1750) = pv(11)*pv(96)
 v(1751) = pv(12)*pv(96)
 v(1752) = pv(0)*pv(97)
 v(1753) = pv(1)*pv(97)
 v(1754) = pv(2)*pv(97)
 v(1755) = pv(3)*pv(97)
 v(1756) = pv(4)*pv(97)
 v(1757) = pv(5)*pv(97)
 v(1758) = pv(6)*pv(97)
 v(1759) = pv(7)*pv(97)
 v(1760) = pv(11)*pv(97)
 v(1761) = pv(12)*pv(97)
 v(1762) = pv(0)*pv(98)
 v(1763) = pv(1)*pv(98)
 v(1764) = pv(2)*pv(98)
 v(1765) = pv(3)*pv(98)
 v(1766) = pv(4)*pv(98)
 v(1767) = pv(5)*pv(98)
 v(1768) = pv(6)*pv(98)
 v(1769) = pv(7)*pv(98)
 v(1770) = pv(8)*pv(98)
 v(1771) = pv(9)*pv(98)
 v(1772) = pv(10)*pv(98)
 v(1773) = pv(13)*pv(98)
 v(1774) = pv(14)*pv(98)
 v(1775) = pv(0)*pv(99)
 v(1776) = pv(1)*pv(99)
 v(1777) = pv(2)*pv(99)
 v(1778) = pv(3)*pv(99)
 v(1779) = pv(4)*pv(99)
 v(1780) = pv(5)*pv(99)
 v(1781) = pv(6)*pv(99)
 v(1782) = pv(7)*pv(99)
 v(1783) = pv(8)*pv(99)
 v(1784) = pv(9)*pv(99)
 v(1785) = pv(10)*pv(99)
 v(1786) = pv(13)*pv(99)
 v(1787) = pv(14)*pv(99)
 v(1788) = pv(0)*pv(100)
 v(1789) = pv(1)*pv(100)
 v(1790) = pv(2)*pv(100)
 v(1791) = pv(3)*pv(100)
 v(1792) = pv(4)*pv(100)
 v(1793) = pv(5)*pv(100)
 v(1794) = pv(6)*pv(100)
 v(1795) = pv(7)*pv(100)
 v(1796) = pv(8)*pv(100)
 v(1797) = pv(9)*pv(100)
 v(1798) = pv(10)*pv(100)
 v(1799) = pv(13)*pv(100)
 v(1800) = pv(14)*pv(100)
 v(1801) = pv(0)*pv(101)
 v(1802) = pv(1)*pv(101)
 v(1803) = pv(2)*pv(101)
 v(1804) = pv(4)*pv(101)
 v(1805) = pv(5)*pv(101)
 v(1806) = pv(6)*pv(101)
 v(1807) = pv(7)*pv(101)
 v(1808) = pv(10)*pv(101)
 v(1809) = pv(13)*pv(101)
 v(1810) = pv(14)*pv(101)
 v(1811) = pv(0)*pv(102)
 v(1812) = pv(1)*pv(102)
 v(1813) = pv(2)*pv(102)
 v(1814) = pv(4)*pv(102)
 v(1815) = pv(5)*pv(102)
 v(1816) = pv(6)*pv(102)
 v(1817) = pv(7)*pv(102)
 v(1818) = pv(10)*pv(102)
 v(1819) = pv(13)*pv(102)
 v(1820) = pv(14)*pv(102)
 v(1821) = pv(0)*pv(103)
 v(1822) = pv(1)*pv(103)
 v(1823) = pv(2)*pv(103)
 v(1824) = pv(3)*pv(103)
 v(1825) = pv(4)*pv(103)
 v(1826) = pv(5)*pv(103)
 v(1827) = pv(6)*pv(103)
 v(1828) = pv(7)*pv(103)
 v(1829) = pv(8)*pv(103)
 v(1830) = pv(9)*pv(103)
 v(1831) = pv(10)*pv(103)
 v(1832) = pv(11)*pv(103)
 v(1833) = pv(12)*pv(103)
 v(1834) = pv(0)*pv(104)
 v(1835) = pv(1)*pv(104)
 v(1836) = pv(2)*pv(104)
 v(1837) = pv(3)*pv(104)
 v(1838) = pv(4)*pv(104)
 v(1839) = pv(5)*pv(104)
 v(1840) = pv(6)*pv(104)
 v(1841) = pv(7)*pv(104)
 v(1842) = pv(8)*pv(104)
 v(1843) = pv(9)*pv(104)
 v(1844) = pv(10)*pv(104)
 v(1845) = pv(11)*pv(104)
 v(1846) = pv(12)*pv(104)
 v(1847) = pv(0)*pv(105)
 v(1848) = pv(1)*pv(105)
 v(1849) = pv(2)*pv(105)
 v(1850) = pv(3)*pv(105)
 v(1851) = pv(5)*pv(105)
 v(1852) = pv(6)*pv(105)
 v(1853) = pv(7)*pv(105)
 v(1854) = pv(8)*pv(105)
 v(1855) = pv(9)*pv(105)
 v(1856) = pv(0)*pv(106)
 v(1857) = pv(1)*pv(106)
 v(1858) = pv(2)*pv(106)
 v(1859) = pv(3)*pv(106)
 v(1860) = pv(4)*pv(106)
 v(1861) = pv(5)*pv(106)
 v(1862) = pv(6)*pv(106)
 v(1863) = pv(7)*pv(106)
 v(1864) = pv(8)*pv(106)
 v(1865) = pv(9)*pv(106)
 v(1866) = pv(10)*pv(106)
 v(1867) = pv(0)*pv(107)
 v(1868) = pv(1)*pv(107)
 v(1869) = pv(2)*pv(107)
 v(1870) = pv(3)*pv(107)
 v(1871) = pv(4)*pv(107)
 v(1872) = pv(5)*pv(107)
 v(1873) = pv(6)*pv(107)
 v(1874) = pv(7)*pv(107)
 v(1875) = pv(8)*pv(107)
 v(1876) = pv(9)*pv(107)
 v(1877) = pv(10)*pv(107)
 v(1878) = pv(11)*pv(107)
 v(1879) = pv(12)*pv(107)
 v(1880) = pv(13)*pv(107)
 v(1881) = pv(14)*pv(107)
 v(1882) = pv(0)*pv(108)
 v(1883) = pv(1)*pv(108)
 v(1884) = pv(2)*pv(108)
 v(1885) = pv(3)*pv(108)
 v(1886) = pv(5)*pv(108)
 v(1887) = pv(6)*pv(108)
 v(1888) = pv(7)*pv(108)
 v(1889) = pv(8)*pv(108)
 v(1890) = pv(9)*pv(108)
 v(1891) = pv(0)*pv(109)
 v(1892) = pv(1)*pv(109)
 v(1893) = pv(2)*pv(109)
 v(1894) = pv(3)*pv(109)
 v(1895) = pv(4)*pv(109)
 v(1896) = pv(5)*pv(109)
 v(1897) = pv(6)*pv(109)
 v(1898) = pv(7)*pv(109)
 v(1899) = pv(8)*pv(109)
 v(1900) = pv(9)*pv(109)
 v(1901) = pv(10)*pv(109)
 v(1902) = pv(11)*pv(109)
 v(1903) = pv(12)*pv(109)
 v(1904) = pv(0)*pv(110)
 v(1905) = pv(1)*pv(110)
 v(1906) = pv(2)*pv(110)
 v(1907) = pv(3)*pv(110)
 v(1908) = pv(4)*pv(110)
 v(1909) = pv(5)*pv(110)
 v(1910) = pv(6)*pv(110)
 v(1911) = pv(7)*pv(110)
 v(1912) = pv(8)*pv(110)
 v(1913) = pv(9)*pv(110)
 v(1914) = pv(10)*pv(110)
 v(1915) = pv(11)*pv(110)
 v(1916) = pv(12)*pv(110)
 v(1917) = pv(13)*pv(110)
 v(1918) = pv(14)*pv(110)
 v(1919) = pv(0)*pv(111)
 v(1920) = pv(1)*pv(111)
 v(1921) = pv(2)*pv(111)
 v(1922) = pv(4)*pv(111)
 v(1923) = pv(5)*pv(111)
 v(1924) = pv(6)*pv(111)
 v(1925) = pv(7)*pv(111)
 v(1926) = pv(10)*pv(111)
 v(1927) = pv(11)*pv(111)
 v(1928) = pv(12)*pv(111)
 v(1929) = pv(0)*pv(112)
 v(1930) = pv(1)*pv(112)
 v(1931) = pv(2)*pv(112)
 v(1932) = pv(3)*pv(112)
 v(1933) = pv(5)*pv(112)
 v(1934) = pv(6)*pv(112)
 v(1935) = pv(7)*pv(112)
 v(1936) = pv(8)*pv(112)
 v(1937) = pv(9)*pv(112)
 v(1938) = pv(0)*pv(113)
 v(1939) = pv(1)*pv(113)
 v(1940) = pv(2)*pv(113)
 v(1941) = pv(5)*pv(113)
 v(1942) = pv(6)*pv(113)
 v(1943) = pv(7)*pv(113)
 v(1944) = pv(0)*pv(114)
 v(1945) = pv(1)*pv(114)
 v(1946) = pv(2)*pv(114)
 v(1947) = pv(3)*pv(114)
 v(1948) = pv(4)*pv(114)
 v(1949) = pv(5)*pv(114)
 v(1950) = pv(6)*pv(114)
 v(1951) = pv(7)*pv(114)
 v(1952) = pv(8)*pv(114)
 v(1953) = pv(9)*pv(114)
 v(1954) = pv(10)*pv(114)
 v(1955) = pv(0)*pv(115)
 v(1956) = pv(1)*pv(115)
 v(1957) = pv(2)*pv(115)
 v(1958) = pv(3)*pv(115)
 v(1959) = pv(4)*pv(115)
 v(1960) = pv(5)*pv(115)
 v(1961) = pv(6)*pv(115)
 v(1962) = pv(7)*pv(115)
 v(1963) = pv(10)*pv(115)
 v(1964) = pv(11)*pv(115)
 v(1965) = pv(12)*pv(115)
 v(1966) = pv(13)*pv(115)
 v(1967) = pv(14)*pv(115)
 v(1968) = pv(0)*pv(116)
 v(1969) = pv(1)*pv(116)
 v(1970) = pv(2)*pv(116)
 v(1971) = pv(3)*pv(116)
 v(1972) = pv(4)*pv(116)
 v(1973) = pv(5)*pv(116)
 v(1974) = pv(6)*pv(116)
 v(1975) = pv(7)*pv(116)
 v(1976) = pv(10)*pv(116)
 v(1977) = pv(11)*pv(116)
 v(1978) = pv(12)*pv(116)
 v(1979) = pv(0)*pv(117)
 v(1980) = pv(1)*pv(117)
 v(1981) = pv(2)*pv(117)
 v(1982) = pv(3)*pv(117)
 v(1983) = pv(4)*pv(117)
 v(1984) = pv(5)*pv(117)
 v(1985) = pv(6)*pv(117)
 v(1986) = pv(7)*pv(117)
 v(1987) = pv(8)*pv(117)
 v(1988) = pv(9)*pv(117)
 v(1989) = pv(10)*pv(117)
 v(1990) = pv(11)*pv(117)
 v(1991) = pv(12)*pv(117)
 v(1992) = pv(13)*pv(117)
 v(1993) = pv(14)*pv(117)
 v(1994) = pv(0)*pv(118)
 v(1995) = pv(1)*pv(118)
 v(1996) = pv(2)*pv(118)
 v(1997) = pv(3)*pv(118)
 v(1998) = pv(5)*pv(118)
 v(1999) = pv(6)*pv(118)
 v(2000) = pv(7)*pv(118)
 v(2001) = pv(8)*pv(118)
 v(2002) = pv(9)*pv(118)
 v(2003) = pv(10)*pv(118)
 v(2004) = pv(0)*pv(119)
 v(2005) = pv(1)*pv(119)
 v(2006) = pv(2)*pv(119)
 v(2007) = pv(3)*pv(119)
 v(2008) = pv(5)*pv(119)
 v(2009) = pv(6)*pv(119)
 v(2010) = pv(7)*pv(119)
 v(2011) = pv(8)*pv(119)
 v(2012) = pv(9)*pv(119)
 v(2013) = pv(10)*pv(119)
 v(2014) = pv(0)*pv(120)
 v(2015) = pv(1)*pv(120)
 v(2016) = pv(2)*pv(120)
 v(2017) = pv(3)*pv(120)
 v(2018) = pv(4)*pv(120)
 v(2019) = pv(5)*pv(120)
 v(2020) = pv(6)*pv(120)
 v(2021) = pv(7)*pv(120)
 v(2022) = pv(8)*pv(120)
 v(2023) = pv(9)*pv(120)
 v(2024) = pv(0)*pv(121)
 v(2025) = pv(1)*pv(121)
 v(2026) = pv(2)*pv(121)
 v(2027) = pv(3)*pv(121)
 v(2028) = pv(4)*pv(121)
 v(2029) = pv(5)*pv(121)
 v(2030) = pv(6)*pv(121)
 v(2031) = pv(7)*pv(121)
 v(2032) = pv(8)*pv(121)
 v(2033) = pv(9)*pv(121)
 v(2034) = pv(0)*pv(122)
 v(2035) = pv(1)*pv(122)
 v(2036) = pv(2)*pv(122)
 v(2037) = pv(3)*pv(122)
 v(2038) = pv(4)*pv(122)
 v(2039) = pv(5)*pv(122)
 v(2040) = pv(6)*pv(122)
 v(2041) = pv(7)*pv(122)
 v(2042) = pv(8)*pv(122)
 v(2043) = pv(9)*pv(122)
 v(2044) = pv(10)*pv(122)
 v(2045:3632) = pv(562:2149)
endif
if (6.le.mxd) then
 stop 'mg4222: degree 6 not implemented'
endif
return
END SUBROUTINE mg4222_secs
