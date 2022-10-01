SUBROUTINE mg4411_prib (mxd, u, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: u(0:)
real (kind=wp), intent (out) :: w(0:mg4411_npb(mxd)-1)
!-----------------------------------------------------------------------
if (size(u).ne.nr) then
 stop 'mg4411_prib: bad size u'
!! else if (size(w).ne.mg4411_npb(mxd)) then
!!  stop 'mg4411_prib: bad size w'
endif
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 8 11 13 11 0 1 0 0 0
! prib,  dnpb(0:*): 1 8 47 221 907 3339 11321 35815 106954 303789
! constant term
w(0) = 1
! terms of degree 1
if (1.le.mxd) then
 w(1) = u(0)*w(0)
 w(2) = u(1)*w(0)
 w(3) = u(2)*w(0)
 w(4) = u(3)*w(0)
 w(5) = u(4)*w(0)
 w(6) = u(5)*w(0)
 w(7) = u(6)*w(0)
 w(8) = u(7)*w(0)
endif
! terms of degree 2
if (2.le.mxd) then
 w(9:16) = u(0)*w(1:8)
 w(17:23) = u(1)*w(2:8)
 w(24:29) = u(2)*w(3:8)
 w(30:34) = u(3)*w(4:8)
 w(35:38) = u(4)*w(5:8)
 w(39:41) = u(5)*w(6:8)
 w(42:43) = u(6)*w(7:8)
 w(44) = u(7)*w(8)
 w(45) = u(8)*w(0)
 w(46) = u(9)*w(0)
 w(47) = u(10)*w(0)
 w(48) = u(11)*w(0)
 w(49) = u(12)*w(0)
 w(50) = u(13)*w(0)
 w(51) = u(14)*w(0)
 w(52) = u(15)*w(0)
 w(53) = u(16)*w(0)
 w(54) = u(17)*w(0)
 w(55) = u(18)*w(0)
endif
! terms of degree 3
if (3.le.mxd) then
 w(56:102) = u(0)*w(9:55)
 w(103:141) = u(1)*w(17:55)
 w(142:173) = u(2)*w(24:55)
 w(174:199) = u(3)*w(30:55)
 w(200:220) = u(4)*w(35:55)
 w(221:237) = u(5)*w(39:55)
 w(238:251) = u(6)*w(42:55)
 w(252:263) = u(7)*w(44:55)
 w(264) = u(19)*w(0)
 w(265) = u(20)*w(0)
 w(266) = u(21)*w(0)
 w(267) = u(22)*w(0)
 w(268) = u(23)*w(0)
 w(269) = u(24)*w(0)
 w(270) = u(25)*w(0)
 w(271) = u(26)*w(0)
 w(272) = u(27)*w(0)
 w(273) = u(28)*w(0)
 w(274) = u(29)*w(0)
 w(275) = u(30)*w(0)
 w(276) = u(31)*w(0)
endif
! terms of degree 4
if (4.le.mxd) then
 w(277:497) = u(0)*w(56:276)
 w(498:671) = u(1)*w(103:276)
 w(672:806) = u(2)*w(142:276)
 w(807:909) = u(3)*w(174:276)
 w(910:986) = u(4)*w(200:276)
 w(987:1042) = u(5)*w(221:276)
 w(1043:1081) = u(6)*w(238:276)
 w(1082:1106) = u(7)*w(252:276)
 w(1107:1117) = u(8)*w(45:55)
 w(1118:1127) = u(9)*w(46:55)
 w(1128:1136) = u(10)*w(47:55)
 w(1137:1144) = u(11)*w(48:55)
 w(1145:1151) = u(12)*w(49:55)
 w(1152:1157) = u(13)*w(50:55)
 w(1158:1162) = u(14)*w(51:55)
 w(1163:1166) = u(15)*w(52:55)
 w(1167:1169) = u(16)*w(53:55)
 w(1170:1171) = u(17)*w(54:55)
 w(1172) = u(18)*w(55)
 w(1173) = u(32)*w(0)
 w(1174) = u(33)*w(0)
 w(1175) = u(34)*w(0)
 w(1176) = u(35)*w(0)
 w(1177) = u(36)*w(0)
 w(1178) = u(37)*w(0)
 w(1179) = u(38)*w(0)
 w(1180) = u(39)*w(0)
 w(1181) = u(40)*w(0)
 w(1182) = u(41)*w(0)
 w(1183) = u(42)*w(0)
endif
! terms of degree 5
if (5.le.mxd) then
 w(1184:2090) = u(0)*w(277:1183)
 w(2091:2776) = u(1)*w(498:1183)
 w(2777:3288) = u(2)*w(672:1183)
 w(3289:3665) = u(3)*w(807:1183)
 w(3666:3939) = u(4)*w(910:1183)
 w(3940:4136) = u(5)*w(987:1183)
 w(4137:4277) = u(6)*w(1043:1183)
 w(4278:4379) = u(7)*w(1082:1183)
 w(4380:4392) = u(8)*w(264:276)
 w(4393:4405) = u(9)*w(264:276)
 w(4406:4418) = u(10)*w(264:276)
 w(4419:4431) = u(11)*w(264:276)
 w(4432:4444) = u(12)*w(264:276)
 w(4445:4457) = u(13)*w(264:276)
 w(4458:4470) = u(14)*w(264:276)
 w(4471:4483) = u(15)*w(264:276)
 w(4484:4496) = u(16)*w(264:276)
 w(4497:4509) = u(17)*w(264:276)
 w(4510:4522) = u(18)*w(264:276)
endif
! terms of degree 6
if (6.le.mxd) then
 w(4523:7861) = u(0)*w(1184:4522)
 w(7862:10293) = u(1)*w(2091:4522)
 w(10294:12039) = u(2)*w(2777:4522)
 w(12040:13273) = u(3)*w(3289:4522)
 w(13274:14130) = u(4)*w(3666:4522)
 w(14131:14713) = u(5)*w(3940:4522)
 w(14714:15099) = u(6)*w(4137:4522)
 w(15100:15344) = u(7)*w(4278:4522)
 w(15345:15421) = u(8)*w(1107:1183)
 w(15422:15487) = u(9)*w(1118:1183)
 w(15488:15543) = u(10)*w(1128:1183)
 w(15544:15590) = u(11)*w(1137:1183)
 w(15591:15629) = u(12)*w(1145:1183)
 w(15630:15661) = u(13)*w(1152:1183)
 w(15662:15687) = u(14)*w(1158:1183)
 w(15688:15708) = u(15)*w(1163:1183)
 w(15709:15725) = u(16)*w(1167:1183)
 w(15726:15739) = u(17)*w(1170:1183)
 w(15740:15751) = u(18)*w(1172:1183)
 w(15752:15764) = u(19)*w(264:276)
 w(15765:15776) = u(20)*w(265:276)
 w(15777:15787) = u(21)*w(266:276)
 w(15788:15797) = u(22)*w(267:276)
 w(15798:15806) = u(23)*w(268:276)
 w(15807:15814) = u(24)*w(269:276)
 w(15815:15821) = u(25)*w(270:276)
 w(15822:15827) = u(26)*w(271:276)
 w(15828:15832) = u(27)*w(272:276)
 w(15833:15836) = u(28)*w(273:276)
 w(15837:15839) = u(29)*w(274:276)
 w(15840:15841) = u(30)*w(275:276)
 w(15842) = u(31)*w(276)
 w(15843) = u(43)*w(0)
endif
! terms of degree 7
if (7.le.mxd) then
 w(15844:27164) = u(0)*w(4523:15843)
 w(27165:35146) = u(1)*w(7862:15843)
 w(35147:40696) = u(2)*w(10294:15843)
 w(40697:44500) = u(3)*w(12040:15843)
 w(44501:47070) = u(4)*w(13274:15843)
 w(47071:48783) = u(5)*w(14131:15843)
 w(48784:49913) = u(6)*w(14714:15843)
 w(49914:50657) = u(7)*w(15100:15843)
 w(50658:50800) = u(8)*w(4380:4522)
 w(50801:50930) = u(9)*w(4393:4522)
 w(50931:51047) = u(10)*w(4406:4522)
 w(51048:51151) = u(11)*w(4419:4522)
 w(51152:51242) = u(12)*w(4432:4522)
 w(51243:51320) = u(13)*w(4445:4522)
 w(51321:51385) = u(14)*w(4458:4522)
 w(51386:51437) = u(15)*w(4471:4522)
 w(51438:51476) = u(16)*w(4484:4522)
 w(51477:51502) = u(17)*w(4497:4522)
 w(51503:51515) = u(18)*w(4510:4522)
 w(51516:51526) = u(19)*w(1173:1183)
 w(51527:51537) = u(20)*w(1173:1183)
 w(51538:51548) = u(21)*w(1173:1183)
 w(51549:51559) = u(22)*w(1173:1183)
 w(51560:51570) = u(23)*w(1173:1183)
 w(51571:51581) = u(24)*w(1173:1183)
 w(51582:51592) = u(25)*w(1173:1183)
 w(51593:51603) = u(26)*w(1173:1183)
 w(51604:51614) = u(27)*w(1173:1183)
 w(51615:51625) = u(28)*w(1173:1183)
 w(51626:51636) = u(29)*w(1173:1183)
 w(51637:51647) = u(30)*w(1173:1183)
 w(51648:51658) = u(31)*w(1173:1183)
endif
! terms of degree 8
if (8.le.mxd) then
 w(51659:87473) = u(0)*w(15844:51658)
 w(87474:111967) = u(1)*w(27165:51658)
 w(111968:128479) = u(2)*w(35147:51658)
 w(128480:139441) = u(3)*w(40697:51658)
 w(139442:146599) = u(4)*w(44501:51658)
 w(146600:151187) = u(5)*w(47071:51658)
 w(151188:154062) = u(6)*w(48784:51658)
 w(154063:155807) = u(7)*w(49914:51658)
 w(155808:156306) = u(8)*w(15345:15843)
 w(156307:156728) = u(9)*w(15422:15843)
 w(156729:157084) = u(10)*w(15488:15843)
 w(157085:157384) = u(11)*w(15544:15843)
 w(157385:157637) = u(12)*w(15591:15843)
 w(157638:157851) = u(13)*w(15630:15843)
 w(157852:158033) = u(14)*w(15662:15843)
 w(158034:158189) = u(15)*w(15688:15843)
 w(158190:158324) = u(16)*w(15709:15843)
 w(158325:158442) = u(17)*w(15726:15843)
 w(158443:158546) = u(18)*w(15740:15843)
 w(158547:158557) = u(32)*w(1173:1183)
 w(158558:158567) = u(33)*w(1174:1183)
 w(158568:158576) = u(34)*w(1175:1183)
 w(158577:158584) = u(35)*w(1176:1183)
 w(158585:158591) = u(36)*w(1177:1183)
 w(158592:158597) = u(37)*w(1178:1183)
 w(158598:158602) = u(38)*w(1179:1183)
 w(158603:158606) = u(39)*w(1180:1183)
 w(158607:158609) = u(40)*w(1181:1183)
 w(158610:158611) = u(41)*w(1182:1183)
 w(158612) = u(42)*w(1183)
endif
! terms of degree 9
if (9.le.mxd) then
 w(158613:265566) = u(0)*w(51659:158612)
 w(265567:336705) = u(1)*w(87474:158612)
 w(336706:383350) = u(2)*w(111968:158612)
 w(383351:413483) = u(3)*w(128480:158612)
 w(413484:432654) = u(4)*w(139442:158612)
 w(432655:444667) = u(5)*w(146600:158612)
 w(444668:452092) = u(6)*w(151188:158612)
 w(452093:456642) = u(7)*w(154063:158612)
 w(456643:457643) = u(8)*w(50658:51658)
 w(457644:458501) = u(9)*w(50801:51658)
 w(458502:459229) = u(10)*w(50931:51658)
 w(459230:459840) = u(11)*w(51048:51658)
 w(459841:460347) = u(12)*w(51152:51658)
 w(460348:460763) = u(13)*w(51243:51658)
 w(460764:461101) = u(14)*w(51321:51658)
 w(461102:461374) = u(15)*w(51386:51658)
 w(461375:461595) = u(16)*w(51438:51658)
 w(461596:461777) = u(17)*w(51477:51658)
 w(461778:461933) = u(18)*w(51503:51658)
 w(461934:462025) = u(19)*w(15752:15843)
 w(462026:462104) = u(20)*w(15765:15843)
 w(462105:462171) = u(21)*w(15777:15843)
 w(462172:462227) = u(22)*w(15788:15843)
 w(462228:462273) = u(23)*w(15798:15843)
 w(462274:462310) = u(24)*w(15807:15843)
 w(462311:462339) = u(25)*w(15815:15843)
 w(462340:462361) = u(26)*w(15822:15843)
 w(462362:462377) = u(27)*w(15828:15843)
 w(462378:462388) = u(28)*w(15833:15843)
 w(462389:462395) = u(29)*w(15837:15843)
 w(462396:462399) = u(30)*w(15840:15843)
 w(462400:462401) = u(31)*w(15842:15843)
endif
END SUBROUTINE mg4411_prib
