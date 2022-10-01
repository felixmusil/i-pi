SUBROUTINE mg3111_prib (mxd, u, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: u(0:)
real (kind=wp), intent (out) :: w(0:mg3111_npb(mxd)-1)
!-----------------------------------------------------------------------
if (size(u).ne.nr) then
 stop 'mg3111_prib: bad size u'
!! else if (size(w).ne.mg3111_npb(mxd)) then
!!  stop 'mg3111_prib: bad size w'
endif
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 7 4 4 0 0 0 0 0 0
! prib,  dnpb(0:*): 1 7 32 116 360 996 2522 5942 13186 27810
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
endif
! terms of degree 2
if (2.le.mxd) then
 w(8:14) = u(0)*w(1:7)
 w(15:20) = u(1)*w(2:7)
 w(21:25) = u(2)*w(3:7)
 w(26:29) = u(3)*w(4:7)
 w(30:32) = u(4)*w(5:7)
 w(33:34) = u(5)*w(6:7)
 w(35) = u(6)*w(7)
 w(36) = u(7)*w(0)
 w(37) = u(8)*w(0)
 w(38) = u(9)*w(0)
 w(39) = u(10)*w(0)
endif
! terms of degree 3
if (3.le.mxd) then
 w(40:71) = u(0)*w(8:39)
 w(72:96) = u(1)*w(15:39)
 w(97:115) = u(2)*w(21:39)
 w(116:129) = u(3)*w(26:39)
 w(130:139) = u(4)*w(30:39)
 w(140:146) = u(5)*w(33:39)
 w(147:151) = u(6)*w(35:39)
 w(152) = u(11)*w(0)
 w(153) = u(12)*w(0)
 w(154) = u(13)*w(0)
 w(155) = u(14)*w(0)
endif
! terms of degree 4
if (4.le.mxd) then
 w(156:271) = u(0)*w(40:155)
 w(272:355) = u(1)*w(72:155)
 w(356:414) = u(2)*w(97:155)
 w(415:454) = u(3)*w(116:155)
 w(455:480) = u(4)*w(130:155)
 w(481:496) = u(5)*w(140:155)
 w(497:505) = u(6)*w(147:155)
 w(506:509) = u(7)*w(36:39)
 w(510:512) = u(8)*w(37:39)
 w(513:514) = u(9)*w(38:39)
 w(515) = u(10)*w(39)
endif
! terms of degree 5
if (5.le.mxd) then
 w(516:875) = u(0)*w(156:515)
 w(876:1119) = u(1)*w(272:515)
 w(1120:1279) = u(2)*w(356:515)
 w(1280:1380) = u(3)*w(415:515)
 w(1381:1441) = u(4)*w(455:515)
 w(1442:1476) = u(5)*w(481:515)
 w(1477:1495) = u(6)*w(497:515)
 w(1496:1499) = u(7)*w(152:155)
 w(1500:1503) = u(8)*w(152:155)
 w(1504:1507) = u(9)*w(152:155)
 w(1508:1511) = u(10)*w(152:155)
endif
! terms of degree 6
if (6.le.mxd) then
 w(1512:2507) = u(0)*w(516:1511)
 w(2508:3143) = u(1)*w(876:1511)
 w(3144:3535) = u(2)*w(1120:1511)
 w(3536:3767) = u(3)*w(1280:1511)
 w(3768:3898) = u(4)*w(1381:1511)
 w(3899:3968) = u(5)*w(1442:1511)
 w(3969:4003) = u(6)*w(1477:1511)
 w(4004:4013) = u(7)*w(506:515)
 w(4014:4019) = u(8)*w(510:515)
 w(4020:4022) = u(9)*w(513:515)
 w(4023) = u(10)*w(515)
 w(4024:4027) = u(11)*w(152:155)
 w(4028:4030) = u(12)*w(153:155)
 w(4031:4032) = u(13)*w(154:155)
 w(4033) = u(14)*w(155)
endif
! terms of degree 7
if (7.le.mxd) then
 w(4034:6555) = u(0)*w(1512:4033)
 w(6556:8081) = u(1)*w(2508:4033)
 w(8082:8971) = u(2)*w(3144:4033)
 w(8972:9469) = u(3)*w(3536:4033)
 w(9470:9735) = u(4)*w(3768:4033)
 w(9736:9870) = u(5)*w(3899:4033)
 w(9871:9935) = u(6)*w(3969:4033)
 w(9936:9951) = u(7)*w(1496:1511)
 w(9952:9963) = u(8)*w(1500:1511)
 w(9964:9971) = u(9)*w(1504:1511)
 w(9972:9975) = u(10)*w(1508:1511)
endif
! terms of degree 8
if (8.le.mxd) then
 w(9976:15917) = u(0)*w(4034:9975)
 w(15918:19337) = u(1)*w(6556:9975)
 w(19338:21231) = u(2)*w(8082:9975)
 w(21232:22235) = u(3)*w(8972:9975)
 w(22236:22741) = u(4)*w(9470:9975)
 w(22742:22981) = u(5)*w(9736:9975)
 w(22982:23086) = u(6)*w(9871:9975)
 w(23087:23116) = u(7)*w(4004:4033)
 w(23117:23136) = u(8)*w(4014:4033)
 w(23137:23150) = u(9)*w(4020:4033)
 w(23151:23161) = u(10)*w(4023:4033)
endif
! terms of degree 9
if (9.le.mxd) then
 w(23162:36347) = u(0)*w(9976:23161)
 w(36348:43591) = u(1)*w(15918:23161)
 w(43592:47415) = u(2)*w(19338:23161)
 w(47416:49345) = u(3)*w(21232:23161)
 w(49346:50271) = u(4)*w(22236:23161)
 w(50272:50691) = u(5)*w(22742:23161)
 w(50692:50871) = u(6)*w(22982:23161)
 w(50872:50911) = u(7)*w(9936:9975)
 w(50912:50935) = u(8)*w(9952:9975)
 w(50936:50947) = u(9)*w(9964:9975)
 w(50948:50951) = u(10)*w(9972:9975)
 w(50952:50961) = u(11)*w(4024:4033)
 w(50962:50967) = u(12)*w(4028:4033)
 w(50968:50970) = u(13)*w(4031:4033)
 w(50971) = u(14)*w(4033)
endif
END SUBROUTINE mg3111_prib