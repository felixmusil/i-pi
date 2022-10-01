SUBROUTINE mg111111111_prib (mxd, u, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: u(0:)
real (kind=wp), intent (out) :: w(0:mg111111111_npb(mxd)-1)
!-----------------------------------------------------------------------
if (size(u).ne.nr) then
 stop 'mg111111111_prib: bad size u'
!! else if (size(w).ne.mg111111111_npb(mxd)) then
!!  stop 'mg111111111_prib: bad size w'
endif
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 36 0 0 0 0 0 0 0 0
! prib,  dnpb(0:*): 1 36 666 8436 82251 658008 4496388 26978328 145008513 708930508
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
 w(9) = u(8)*w(0)
 w(10) = u(9)*w(0)
 w(11) = u(10)*w(0)
 w(12) = u(11)*w(0)
 w(13) = u(12)*w(0)
 w(14) = u(13)*w(0)
 w(15) = u(14)*w(0)
 w(16) = u(15)*w(0)
 w(17) = u(16)*w(0)
 w(18) = u(17)*w(0)
 w(19) = u(18)*w(0)
 w(20) = u(19)*w(0)
 w(21) = u(20)*w(0)
 w(22) = u(21)*w(0)
 w(23) = u(22)*w(0)
 w(24) = u(23)*w(0)
 w(25) = u(24)*w(0)
 w(26) = u(25)*w(0)
 w(27) = u(26)*w(0)
 w(28) = u(27)*w(0)
 w(29) = u(28)*w(0)
 w(30) = u(29)*w(0)
 w(31) = u(30)*w(0)
 w(32) = u(31)*w(0)
 w(33) = u(32)*w(0)
 w(34) = u(33)*w(0)
 w(35) = u(34)*w(0)
 w(36) = u(35)*w(0)
endif
! terms of degree 2
if (2.le.mxd) then
 w(37:72) = u(0)*w(1:36)
 w(73:107) = u(1)*w(2:36)
 w(108:141) = u(2)*w(3:36)
 w(142:174) = u(3)*w(4:36)
 w(175:206) = u(4)*w(5:36)
 w(207:237) = u(5)*w(6:36)
 w(238:267) = u(6)*w(7:36)
 w(268:296) = u(7)*w(8:36)
 w(297:324) = u(8)*w(9:36)
 w(325:351) = u(9)*w(10:36)
 w(352:377) = u(10)*w(11:36)
 w(378:402) = u(11)*w(12:36)
 w(403:426) = u(12)*w(13:36)
 w(427:449) = u(13)*w(14:36)
 w(450:471) = u(14)*w(15:36)
 w(472:492) = u(15)*w(16:36)
 w(493:512) = u(16)*w(17:36)
 w(513:531) = u(17)*w(18:36)
 w(532:549) = u(18)*w(19:36)
 w(550:566) = u(19)*w(20:36)
 w(567:582) = u(20)*w(21:36)
 w(583:597) = u(21)*w(22:36)
 w(598:611) = u(22)*w(23:36)
 w(612:624) = u(23)*w(24:36)
 w(625:636) = u(24)*w(25:36)
 w(637:647) = u(25)*w(26:36)
 w(648:657) = u(26)*w(27:36)
 w(658:666) = u(27)*w(28:36)
 w(667:674) = u(28)*w(29:36)
 w(675:681) = u(29)*w(30:36)
 w(682:687) = u(30)*w(31:36)
 w(688:692) = u(31)*w(32:36)
 w(693:696) = u(32)*w(33:36)
 w(697:699) = u(33)*w(34:36)
 w(700:701) = u(34)*w(35:36)
 w(702) = u(35)*w(36)
endif
! terms of degree 3
if (3.le.mxd) then
 w(703:1368) = u(0)*w(37:702)
 w(1369:1998) = u(1)*w(73:702)
 w(1999:2593) = u(2)*w(108:702)
 w(2594:3154) = u(3)*w(142:702)
 w(3155:3682) = u(4)*w(175:702)
 w(3683:4178) = u(5)*w(207:702)
 w(4179:4643) = u(6)*w(238:702)
 w(4644:5078) = u(7)*w(268:702)
 w(5079:5484) = u(8)*w(297:702)
 w(5485:5862) = u(9)*w(325:702)
 w(5863:6213) = u(10)*w(352:702)
 w(6214:6538) = u(11)*w(378:702)
 w(6539:6838) = u(12)*w(403:702)
 w(6839:7114) = u(13)*w(427:702)
 w(7115:7367) = u(14)*w(450:702)
 w(7368:7598) = u(15)*w(472:702)
 w(7599:7808) = u(16)*w(493:702)
 w(7809:7998) = u(17)*w(513:702)
 w(7999:8169) = u(18)*w(532:702)
 w(8170:8322) = u(19)*w(550:702)
 w(8323:8458) = u(20)*w(567:702)
 w(8459:8578) = u(21)*w(583:702)
 w(8579:8683) = u(22)*w(598:702)
 w(8684:8774) = u(23)*w(612:702)
 w(8775:8852) = u(24)*w(625:702)
 w(8853:8918) = u(25)*w(637:702)
 w(8919:8973) = u(26)*w(648:702)
 w(8974:9018) = u(27)*w(658:702)
 w(9019:9054) = u(28)*w(667:702)
 w(9055:9082) = u(29)*w(675:702)
 w(9083:9103) = u(30)*w(682:702)
 w(9104:9118) = u(31)*w(688:702)
 w(9119:9128) = u(32)*w(693:702)
 w(9129:9134) = u(33)*w(697:702)
 w(9135:9137) = u(34)*w(700:702)
 w(9138) = u(35)*w(702)
endif
! terms of degree 4
if (4.le.mxd) then
 w(9139:17574) = u(0)*w(703:9138)
 w(17575:25344) = u(1)*w(1369:9138)
 w(25345:32484) = u(2)*w(1999:9138)
 w(32485:39029) = u(3)*w(2594:9138)
 w(39030:45013) = u(4)*w(3155:9138)
 w(45014:50469) = u(5)*w(3683:9138)
 w(50470:55429) = u(6)*w(4179:9138)
 w(55430:59924) = u(7)*w(4644:9138)
 w(59925:63984) = u(8)*w(5079:9138)
 w(63985:67638) = u(9)*w(5485:9138)
 w(67639:70914) = u(10)*w(5863:9138)
 w(70915:73839) = u(11)*w(6214:9138)
 w(73840:76439) = u(12)*w(6539:9138)
 w(76440:78739) = u(13)*w(6839:9138)
 w(78740:80763) = u(14)*w(7115:9138)
 w(80764:82534) = u(15)*w(7368:9138)
 w(82535:84074) = u(16)*w(7599:9138)
 w(84075:85404) = u(17)*w(7809:9138)
 w(85405:86544) = u(18)*w(7999:9138)
 w(86545:87513) = u(19)*w(8170:9138)
 w(87514:88329) = u(20)*w(8323:9138)
 w(88330:89009) = u(21)*w(8459:9138)
 w(89010:89569) = u(22)*w(8579:9138)
 w(89570:90024) = u(23)*w(8684:9138)
 w(90025:90388) = u(24)*w(8775:9138)
 w(90389:90674) = u(25)*w(8853:9138)
 w(90675:90894) = u(26)*w(8919:9138)
 w(90895:91059) = u(27)*w(8974:9138)
 w(91060:91179) = u(28)*w(9019:9138)
 w(91180:91263) = u(29)*w(9055:9138)
 w(91264:91319) = u(30)*w(9083:9138)
 w(91320:91354) = u(31)*w(9104:9138)
 w(91355:91374) = u(32)*w(9119:9138)
 w(91375:91384) = u(33)*w(9129:9138)
 w(91385:91388) = u(34)*w(9135:9138)
 w(91389) = u(35)*w(9138)
endif
! terms of degree 5
if (5.le.mxd) then
 w(91390:173640) = u(0)*w(9139:91389)
 w(173641:247455) = u(1)*w(17575:91389)
 w(247456:313500) = u(2)*w(25345:91389)
 w(313501:372405) = u(3)*w(32485:91389)
 w(372406:424765) = u(4)*w(39030:91389)
 w(424766:471141) = u(5)*w(45014:91389)
 w(471142:512061) = u(6)*w(50470:91389)
 w(512062:548021) = u(7)*w(55430:91389)
 w(548022:579486) = u(8)*w(59925:91389)
 w(579487:606891) = u(9)*w(63985:91389)
 w(606892:630642) = u(10)*w(67639:91389)
 w(630643:651117) = u(11)*w(70915:91389)
 w(651118:668667) = u(12)*w(73840:91389)
 w(668668:683617) = u(13)*w(76440:91389)
 w(683618:696267) = u(14)*w(78740:91389)
 w(696268:706893) = u(15)*w(80764:91389)
 w(706894:715748) = u(16)*w(82535:91389)
 w(715749:723063) = u(17)*w(84075:91389)
 w(723064:729048) = u(18)*w(85405:91389)
 w(729049:733893) = u(19)*w(86545:91389)
 w(733894:737769) = u(20)*w(87514:91389)
 w(737770:740829) = u(21)*w(88330:91389)
 w(740830:743209) = u(22)*w(89010:91389)
 w(743210:745029) = u(23)*w(89570:91389)
 w(745030:746394) = u(24)*w(90025:91389)
 w(746395:747395) = u(25)*w(90389:91389)
 w(747396:748110) = u(26)*w(90675:91389)
 w(748111:748605) = u(27)*w(90895:91389)
 w(748606:748935) = u(28)*w(91060:91389)
 w(748936:749145) = u(29)*w(91180:91389)
 w(749146:749271) = u(30)*w(91264:91389)
 w(749272:749341) = u(31)*w(91320:91389)
 w(749342:749376) = u(32)*w(91355:91389)
 w(749377:749391) = u(33)*w(91375:91389)
 w(749392:749396) = u(34)*w(91385:91389)
 w(749397) = u(35)*w(91389)
endif
! terms of degree 6
if (6.le.mxd) then
 w(749398:1407405) = u(0)*w(91390:749397)
 w(1407406:1983162) = u(1)*w(173641:749397)
 w(1983163:2485104) = u(2)*w(247456:749397)
 w(2485105:2921001) = u(3)*w(313501:749397)
 w(2921002:3297993) = u(4)*w(372406:749397)
 w(3297994:3622625) = u(5)*w(424766:749397)
 w(3622626:3900881) = u(6)*w(471142:749397)
 w(3900882:4138217) = u(7)*w(512062:749397)
 w(4138218:4339593) = u(8)*w(548022:749397)
 w(4339594:4509504) = u(9)*w(579487:749397)
 w(4509505:4652010) = u(10)*w(606892:749397)
 w(4652011:4770765) = u(11)*w(630643:749397)
 w(4770766:4869045) = u(12)*w(651118:749397)
 w(4869046:4949775) = u(13)*w(668668:749397)
 w(4949776:5015555) = u(14)*w(683618:749397)
 w(5015556:5068685) = u(15)*w(696268:749397)
 w(5068686:5111189) = u(16)*w(706894:749397)
 w(5111190:5144838) = u(17)*w(715749:749397)
 w(5144839:5171172) = u(18)*w(723064:749397)
 w(5171173:5191521) = u(19)*w(729049:749397)
 w(5191522:5207025) = u(20)*w(733894:749397)
 w(5207026:5218653) = u(21)*w(737770:749397)
 w(5218654:5227221) = u(22)*w(740830:749397)
 w(5227222:5233409) = u(23)*w(743210:749397)
 w(5233410:5237777) = u(24)*w(745030:749397)
 w(5237778:5240780) = u(25)*w(746395:749397)
 w(5240781:5242782) = u(26)*w(747396:749397)
 w(5242783:5244069) = u(27)*w(748111:749397)
 w(5244070:5244861) = u(28)*w(748606:749397)
 w(5244862:5245323) = u(29)*w(748936:749397)
 w(5245324:5245575) = u(30)*w(749146:749397)
 w(5245576:5245701) = u(31)*w(749272:749397)
 w(5245702:5245757) = u(32)*w(749342:749397)
 w(5245758:5245778) = u(33)*w(749377:749397)
 w(5245779:5245784) = u(34)*w(749392:749397)
 w(5245785) = u(35)*w(749397)
endif
! terms of degree 7
if (7.le.mxd) then
 w(5245786:9742173) = u(0)*w(749398:5245785)
 w(9742174:13580553) = u(1)*w(1407406:5245785)
 w(13580554:16843176) = u(2)*w(1983163:5245785)
 w(16843177:19603857) = u(3)*w(2485105:5245785)
 w(19603858:21928641) = u(4)*w(2921002:5245785)
 w(21928642:23876433) = u(5)*w(3297994:5245785)
 w(23876434:25499593) = u(6)*w(3622626:5245785)
 w(25499594:26844497) = u(7)*w(3900882:5245785)
 w(26844498:27952065) = u(8)*w(4138218:5245785)
 w(27952066:28858257) = u(9)*w(4339594:5245785)
 w(28858258:29594538) = u(10)*w(4509505:5245785)
 w(29594539:30188313) = u(11)*w(4652011:5245785)
 w(30188314:30663333) = u(12)*w(4770766:5245785)
 w(30663334:31040073) = u(13)*w(4869046:5245785)
 w(31040074:31336083) = u(14)*w(4949776:5245785)
 w(31336084:31566313) = u(15)*w(5015556:5245785)
 w(31566314:31743413) = u(16)*w(5068686:5245785)
 w(31743414:31878009) = u(17)*w(5111190:5245785)
 w(31878010:31978956) = u(18)*w(5144839:5245785)
 w(31978957:32053569) = u(19)*w(5171173:5245785)
 w(32053570:32107833) = u(20)*w(5191522:5245785)
 w(32107834:32146593) = u(21)*w(5207026:5245785)
 w(32146594:32173725) = u(22)*w(5218654:5245785)
 w(32173726:32192289) = u(23)*w(5227222:5245785)
 w(32192290:32204665) = u(24)*w(5233410:5245785)
 w(32204666:32212673) = u(25)*w(5237778:5245785)
 w(32212674:32217678) = u(26)*w(5240781:5245785)
 w(32217679:32220681) = u(27)*w(5242783:5245785)
 w(32220682:32222397) = u(28)*w(5244070:5245785)
 w(32222398:32223321) = u(29)*w(5244862:5245785)
 w(32223322:32223783) = u(30)*w(5245324:5245785)
 w(32223784:32223993) = u(31)*w(5245576:5245785)
 w(32223994:32224077) = u(32)*w(5245702:5245785)
 w(32224078:32224105) = u(33)*w(5245758:5245785)
 w(32224106:32224112) = u(34)*w(5245779:5245785)
 w(32224113) = u(35)*w(5245785)
endif
! terms of degree 8
if (8.le.mxd) then
 w(32224114:59202441) = u(0)*w(5245786:32224113)
 w(59202442:81684381) = u(1)*w(9742174:32224113)
 w(81684382:100327941) = u(2)*w(13580554:32224113)
 w(100327942:115708878) = u(3)*w(16843177:32224113)
 w(115708879:128329134) = u(4)*w(19603858:32224113)
 w(128329135:138624606) = u(5)*w(21928642:32224113)
 w(138624607:146972286) = u(6)*w(23876434:32224113)
 w(146972287:153696806) = u(7)*w(25499594:32224113)
 w(153696807:159076422) = u(8)*w(26844498:32224113)
 w(159076423:163348470) = u(9)*w(27952066:32224113)
 w(163348471:166714326) = u(10)*w(28858258:32224113)
 w(166714327:169343901) = u(11)*w(29594539:32224113)
 w(169343902:171379701) = u(12)*w(30188314:32224113)
 w(171379702:172940481) = u(13)*w(30663334:32224113)
 w(172940482:174124521) = u(14)*w(31040074:32224113)
 w(174124522:175012551) = u(15)*w(31336084:32224113)
 w(175012552:175670351) = u(16)*w(31566314:32224113)
 w(175670352:176151051) = u(17)*w(31743414:32224113)
 w(176151052:176497155) = u(18)*w(31878010:32224113)
 w(176497156:176742312) = u(19)*w(31978957:32224113)
 w(176742313:176912856) = u(20)*w(32053570:32224113)
 w(176912857:177029136) = u(21)*w(32107834:32224113)
 w(177029137:177106656) = u(22)*w(32146594:32224113)
 w(177106657:177157044) = u(23)*w(32173726:32224113)
 w(177157045:177188868) = u(24)*w(32192290:32224113)
 w(177188869:177208316) = u(25)*w(32204666:32224113)
 w(177208317:177219756) = u(26)*w(32212674:32224113)
 w(177219757:177226191) = u(27)*w(32217679:32224113)
 w(177226192:177229623) = u(28)*w(32220682:32224113)
 w(177229624:177231339) = u(29)*w(32222398:32224113)
 w(177231340:177232131) = u(30)*w(32223322:32224113)
 w(177232132:177232461) = u(31)*w(32223784:32224113)
 w(177232462:177232581) = u(32)*w(32223994:32224113)
 w(177232582:177232617) = u(33)*w(32224078:32224113)
 w(177232618:177232625) = u(34)*w(32224106:32224113)
 w(177232626) = u(35)*w(32224113)
endif
! terms of degree 9
if (9.le.mxd) then
! These terms may cause stacksize trouble
!! w(177232627:322241139) = u(0)*w(32224114:177232626)
!! w(322241140:440271324) = u(1)*w(59202442:177232626)
!! w(440271325:535819569) = u(2)*w(81684382:177232626)
!! w(535819570:612724254) = u(3)*w(100327942:177232626)
!! w(612724255:674248002) = u(4)*w(115708879:177232626)
!! w(674248003:723151494) = u(5)*w(128329135:177232626)
!! w(723151495:761759514) = u(6)*w(138624607:177232626)
!! w(761759515:792019854) = u(7)*w(146972287:177232626)
!! w(792019855:815555674) = u(8)*w(153696807:177232626)
!! w(815555675:833711878) = u(9)*w(159076423:177232626)
!! w(833711879:847596034) = u(10)*w(163348471:177232626)
!! w(847596035:858114334) = u(11)*w(166714327:177232626)
!! w(858114335:866003059) = u(12)*w(169343902:177232626)
!! w(866003060:871855984) = u(13)*w(171379702:177232626)
!! w(871855985:876148129) = u(14)*w(172940482:177232626)
!! w(876148130:879256234) = u(15)*w(174124522:177232626)
!! w(879256235:881476309) = u(16)*w(175012552:177232626)
!! w(881476310:883038584) = u(17)*w(175670352:177232626)
!! w(883038585:884120159) = u(18)*w(176151052:177232626)
!! w(884120160:884855630) = u(19)*w(176497156:177232626)
!! w(884855631:885345944) = u(20)*w(176742313:177232626)
!! w(885345945:885665714) = u(21)*w(176912857:177232626)
!! w(885665715:885869204) = u(22)*w(177029137:177232626)
!! w(885869205:885995174) = u(23)*w(177106657:177232626)
!! w(885995175:886070756) = u(24)*w(177157045:177232626)
!! w(886070757:886114514) = u(25)*w(177188869:177232626)
!! w(886114515:886138824) = u(26)*w(177208317:177232626)
!! w(886138825:886151694) = u(27)*w(177219757:177232626)
!! w(886151695:886158129) = u(28)*w(177226192:177232626)
!! w(886158130:886161132) = u(29)*w(177229624:177232626)
!! w(886161133:886162419) = u(30)*w(177231340:177232626)
!! w(886162420:886162914) = u(31)*w(177232132:177232626)
!! w(886162915:886163079) = u(32)*w(177232462:177232626)
!! w(886163080:886163124) = u(33)*w(177232582:177232626)
!! w(886163125:886163133) = u(34)*w(177232618:177232626)
!! w(886163134) = u(35)*w(177232626)
endif
END SUBROUTINE mg111111111_prib