SUBROUTINE mg4311_prib (mxd, u, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: u(0:)
real (kind=wp), intent (out) :: w(0:mg4311_npb(mxd)-1)
!-----------------------------------------------------------------------
if (size(u).ne.nr) then
 stop 'mg4311_prib: bad size u'
!! else if (size(w).ne.mg4311_npb(mxd)) then
!!  stop 'mg4311_prib: bad size w'
endif
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 8 10 10 6 0 1 0 0 0
! prib,  dnpb(0:*): 1 8 46 210 831 2940 9548 28870 82247 222550
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
endif
! terms of degree 3
if (3.le.mxd) then
 w(55:100) = u(0)*w(9:54)
 w(101:138) = u(1)*w(17:54)
 w(139:169) = u(2)*w(24:54)
 w(170:194) = u(3)*w(30:54)
 w(195:214) = u(4)*w(35:54)
 w(215:230) = u(5)*w(39:54)
 w(231:243) = u(6)*w(42:54)
 w(244:254) = u(7)*w(44:54)
 w(255) = u(18)*w(0)
 w(256) = u(19)*w(0)
 w(257) = u(20)*w(0)
 w(258) = u(21)*w(0)
 w(259) = u(22)*w(0)
 w(260) = u(23)*w(0)
 w(261) = u(24)*w(0)
 w(262) = u(25)*w(0)
 w(263) = u(26)*w(0)
 w(264) = u(27)*w(0)
endif
! terms of degree 4
if (4.le.mxd) then
 w(265:474) = u(0)*w(55:264)
 w(475:638) = u(1)*w(101:264)
 w(639:764) = u(2)*w(139:264)
 w(765:859) = u(3)*w(170:264)
 w(860:929) = u(4)*w(195:264)
 w(930:979) = u(5)*w(215:264)
 w(980:1013) = u(6)*w(231:264)
 w(1014:1034) = u(7)*w(244:264)
 w(1035:1044) = u(8)*w(45:54)
 w(1045:1053) = u(9)*w(46:54)
 w(1054:1061) = u(10)*w(47:54)
 w(1062:1068) = u(11)*w(48:54)
 w(1069:1074) = u(12)*w(49:54)
 w(1075:1079) = u(13)*w(50:54)
 w(1080:1083) = u(14)*w(51:54)
 w(1084:1086) = u(15)*w(52:54)
 w(1087:1088) = u(16)*w(53:54)
 w(1089) = u(17)*w(54)
 w(1090) = u(28)*w(0)
 w(1091) = u(29)*w(0)
 w(1092) = u(30)*w(0)
 w(1093) = u(31)*w(0)
 w(1094) = u(32)*w(0)
 w(1095) = u(33)*w(0)
endif
! terms of degree 5
if (5.le.mxd) then
 w(1096:1926) = u(0)*w(265:1095)
 w(1927:2547) = u(1)*w(475:1095)
 w(2548:3004) = u(2)*w(639:1095)
 w(3005:3335) = u(3)*w(765:1095)
 w(3336:3571) = u(4)*w(860:1095)
 w(3572:3737) = u(5)*w(930:1095)
 w(3738:3853) = u(6)*w(980:1095)
 w(3854:3935) = u(7)*w(1014:1095)
 w(3936:3945) = u(8)*w(255:264)
 w(3946:3955) = u(9)*w(255:264)
 w(3956:3965) = u(10)*w(255:264)
 w(3966:3975) = u(11)*w(255:264)
 w(3976:3985) = u(12)*w(255:264)
 w(3986:3995) = u(13)*w(255:264)
 w(3996:4005) = u(14)*w(255:264)
 w(4006:4015) = u(15)*w(255:264)
 w(4016:4025) = u(16)*w(255:264)
 w(4026:4035) = u(17)*w(255:264)
endif
! terms of degree 6
if (6.le.mxd) then
 w(4036:6975) = u(0)*w(1096:4035)
 w(6976:9084) = u(1)*w(1927:4035)
 w(9085:10572) = u(2)*w(2548:4035)
 w(10573:11603) = u(3)*w(3005:4035)
 w(11604:12303) = u(4)*w(3336:4035)
 w(12304:12767) = u(5)*w(3572:4035)
 w(12768:13065) = u(6)*w(3738:4035)
 w(13066:13247) = u(7)*w(3854:4035)
 w(13248:13308) = u(8)*w(1035:1095)
 w(13309:13359) = u(9)*w(1045:1095)
 w(13360:13401) = u(10)*w(1054:1095)
 w(13402:13435) = u(11)*w(1062:1095)
 w(13436:13462) = u(12)*w(1069:1095)
 w(13463:13483) = u(13)*w(1075:1095)
 w(13484:13499) = u(14)*w(1080:1095)
 w(13500:13511) = u(15)*w(1084:1095)
 w(13512:13520) = u(16)*w(1087:1095)
 w(13521:13527) = u(17)*w(1089:1095)
 w(13528:13537) = u(18)*w(255:264)
 w(13538:13546) = u(19)*w(256:264)
 w(13547:13554) = u(20)*w(257:264)
 w(13555:13561) = u(21)*w(258:264)
 w(13562:13567) = u(22)*w(259:264)
 w(13568:13572) = u(23)*w(260:264)
 w(13573:13576) = u(24)*w(261:264)
 w(13577:13579) = u(25)*w(262:264)
 w(13580:13581) = u(26)*w(263:264)
 w(13582) = u(27)*w(264)
 w(13583) = u(34)*w(0)
endif
! terms of degree 7
if (7.le.mxd) then
 w(13584:23131) = u(0)*w(4036:13583)
 w(23132:29739) = u(1)*w(6976:13583)
 w(29740:34238) = u(2)*w(9085:13583)
 w(34239:37249) = u(3)*w(10573:13583)
 w(37250:39229) = u(4)*w(11604:13583)
 w(39230:40509) = u(5)*w(12304:13583)
 w(40510:41325) = u(6)*w(12768:13583)
 w(41326:41843) = u(7)*w(13066:13583)
 w(41844:41943) = u(8)*w(3936:4035)
 w(41944:42033) = u(9)*w(3946:4035)
 w(42034:42113) = u(10)*w(3956:4035)
 w(42114:42183) = u(11)*w(3966:4035)
 w(42184:42243) = u(12)*w(3976:4035)
 w(42244:42293) = u(13)*w(3986:4035)
 w(42294:42333) = u(14)*w(3996:4035)
 w(42334:42363) = u(15)*w(4006:4035)
 w(42364:42383) = u(16)*w(4016:4035)
 w(42384:42393) = u(17)*w(4026:4035)
 w(42394:42399) = u(18)*w(1090:1095)
 w(42400:42405) = u(19)*w(1090:1095)
 w(42406:42411) = u(20)*w(1090:1095)
 w(42412:42417) = u(21)*w(1090:1095)
 w(42418:42423) = u(22)*w(1090:1095)
 w(42424:42429) = u(23)*w(1090:1095)
 w(42430:42435) = u(24)*w(1090:1095)
 w(42436:42441) = u(25)*w(1090:1095)
 w(42442:42447) = u(26)*w(1090:1095)
 w(42448:42453) = u(27)*w(1090:1095)
endif
! terms of degree 8
if (8.le.mxd) then
 w(42454:71323) = u(0)*w(13584:42453)
 w(71324:90645) = u(1)*w(23132:42453)
 w(90646:103359) = u(2)*w(29740:42453)
 w(103360:111574) = u(3)*w(34239:42453)
 w(111575:116778) = u(4)*w(37250:42453)
 w(116779:120002) = u(5)*w(39230:42453)
 w(120003:121946) = u(6)*w(40510:42453)
 w(121947:123074) = u(7)*w(41326:42453)
 w(123075:123410) = u(8)*w(13248:13583)
 w(123411:123685) = u(9)*w(13309:13583)
 w(123686:123909) = u(10)*w(13360:13583)
 w(123910:124091) = u(11)*w(13402:13583)
 w(124092:124239) = u(12)*w(13436:13583)
 w(124240:124360) = u(13)*w(13463:13583)
 w(124361:124460) = u(14)*w(13484:13583)
 w(124461:124544) = u(15)*w(13500:13583)
 w(124545:124616) = u(16)*w(13512:13583)
 w(124617:124679) = u(17)*w(13521:13583)
 w(124680:124685) = u(28)*w(1090:1095)
 w(124686:124690) = u(29)*w(1091:1095)
 w(124691:124694) = u(30)*w(1092:1095)
 w(124695:124697) = u(31)*w(1093:1095)
 w(124698:124699) = u(32)*w(1094:1095)
 w(124700) = u(33)*w(1095)
endif
! terms of degree 9
if (9.le.mxd) then
 w(124701:206947) = u(0)*w(42454:124700)
 w(206948:260324) = u(1)*w(71324:124700)
 w(260325:294379) = u(2)*w(90646:124700)
 w(294380:315720) = u(3)*w(103360:124700)
 w(315721:328846) = u(4)*w(111575:124700)
 w(328847:336768) = u(5)*w(116779:124700)
 w(336769:341466) = u(6)*w(120003:124700)
 w(341467:344220) = u(7)*w(121947:124700)
 w(344221:344830) = u(8)*w(41844:42453)
 w(344831:345340) = u(9)*w(41944:42453)
 w(345341:345760) = u(10)*w(42034:42453)
 w(345761:346100) = u(11)*w(42114:42453)
 w(346101:346370) = u(12)*w(42184:42453)
 w(346371:346580) = u(13)*w(42244:42453)
 w(346581:346740) = u(14)*w(42294:42453)
 w(346741:346860) = u(15)*w(42334:42453)
 w(346861:346950) = u(16)*w(42364:42453)
 w(346951:347020) = u(17)*w(42384:42453)
 w(347021:347076) = u(18)*w(13528:13583)
 w(347077:347122) = u(19)*w(13538:13583)
 w(347123:347159) = u(20)*w(13547:13583)
 w(347160:347188) = u(21)*w(13555:13583)
 w(347189:347210) = u(22)*w(13562:13583)
 w(347211:347226) = u(23)*w(13568:13583)
 w(347227:347237) = u(24)*w(13573:13583)
 w(347238:347244) = u(25)*w(13577:13583)
 w(347245:347248) = u(26)*w(13580:13583)
 w(347249:347250) = u(27)*w(13582:13583)
endif
END SUBROUTINE mg4311_prib