SUBROUTINE mg3221_prib (mxd, u, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: u(0:)
real (kind=wp), intent (out) :: w(0:mg3221_npb(mxd)-1)
!-----------------------------------------------------------------------
if (size(u).ne.nr) then
 stop 'mg3221_prib: bad size u'
!! else if (size(w).ne.mg3221_npb(mxd)) then
!!  stop 'mg3221_prib: bad size w'
endif
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 9 13 4 0 0 2 0 0 0
! prib,  dnpb(0:*): 1 9 58 286 1207 4483 15128 47068 136949 375901
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
endif
! terms of degree 2
if (2.le.mxd) then
 w(10:18) = u(0)*w(1:9)
 w(19:26) = u(1)*w(2:9)
 w(27:33) = u(2)*w(3:9)
 w(34:39) = u(3)*w(4:9)
 w(40:44) = u(4)*w(5:9)
 w(45:48) = u(5)*w(6:9)
 w(49:51) = u(6)*w(7:9)
 w(52:53) = u(7)*w(8:9)
 w(54) = u(8)*w(9)
 w(55) = u(9)*w(0)
 w(56) = u(10)*w(0)
 w(57) = u(11)*w(0)
 w(58) = u(12)*w(0)
 w(59) = u(13)*w(0)
 w(60) = u(14)*w(0)
 w(61) = u(15)*w(0)
 w(62) = u(16)*w(0)
 w(63) = u(17)*w(0)
 w(64) = u(18)*w(0)
 w(65) = u(19)*w(0)
 w(66) = u(20)*w(0)
 w(67) = u(21)*w(0)
endif
! terms of degree 3
if (3.le.mxd) then
 w(68:125) = u(0)*w(10:67)
 w(126:174) = u(1)*w(19:67)
 w(175:215) = u(2)*w(27:67)
 w(216:249) = u(3)*w(34:67)
 w(250:277) = u(4)*w(40:67)
 w(278:300) = u(5)*w(45:67)
 w(301:319) = u(6)*w(49:67)
 w(320:335) = u(7)*w(52:67)
 w(336:349) = u(8)*w(54:67)
 w(350) = u(22)*w(0)
 w(351) = u(23)*w(0)
 w(352) = u(24)*w(0)
 w(353) = u(25)*w(0)
endif
! terms of degree 4
if (4.le.mxd) then
 w(354:639) = u(0)*w(68:353)
 w(640:867) = u(1)*w(126:353)
 w(868:1046) = u(2)*w(175:353)
 w(1047:1184) = u(3)*w(216:353)
 w(1185:1288) = u(4)*w(250:353)
 w(1289:1364) = u(5)*w(278:353)
 w(1365:1417) = u(6)*w(301:353)
 w(1418:1451) = u(7)*w(320:353)
 w(1452:1469) = u(8)*w(336:353)
 w(1470:1482) = u(9)*w(55:67)
 w(1483:1494) = u(10)*w(56:67)
 w(1495:1505) = u(11)*w(57:67)
 w(1506:1515) = u(12)*w(58:67)
 w(1516:1524) = u(13)*w(59:67)
 w(1525:1532) = u(14)*w(60:67)
 w(1533:1539) = u(15)*w(61:67)
 w(1540:1545) = u(16)*w(62:67)
 w(1546:1550) = u(17)*w(63:67)
 w(1551:1554) = u(18)*w(64:67)
 w(1555:1557) = u(19)*w(65:67)
 w(1558:1559) = u(20)*w(66:67)
 w(1560) = u(21)*w(67)
endif
! terms of degree 5
if (5.le.mxd) then
 w(1561:2767) = u(0)*w(354:1560)
 w(2768:3688) = u(1)*w(640:1560)
 w(3689:4381) = u(2)*w(868:1560)
 w(4382:4895) = u(3)*w(1047:1560)
 w(4896:5271) = u(4)*w(1185:1560)
 w(5272:5543) = u(5)*w(1289:1560)
 w(5544:5739) = u(6)*w(1365:1560)
 w(5740:5882) = u(7)*w(1418:1560)
 w(5883:5991) = u(8)*w(1452:1560)
 w(5992:5995) = u(9)*w(350:353)
 w(5996:5999) = u(10)*w(350:353)
 w(6000:6003) = u(11)*w(350:353)
 w(6004:6007) = u(12)*w(350:353)
 w(6008:6011) = u(13)*w(350:353)
 w(6012:6015) = u(14)*w(350:353)
 w(6016:6019) = u(15)*w(350:353)
 w(6020:6023) = u(16)*w(350:353)
 w(6024:6027) = u(17)*w(350:353)
 w(6028:6031) = u(18)*w(350:353)
 w(6032:6035) = u(19)*w(350:353)
 w(6036:6039) = u(20)*w(350:353)
 w(6040:6043) = u(21)*w(350:353)
endif
! terms of degree 6
if (6.le.mxd) then
 w(6044:10526) = u(0)*w(1561:6043)
 w(10527:13802) = u(1)*w(2768:6043)
 w(13803:16157) = u(2)*w(3689:6043)
 w(16158:17819) = u(3)*w(4382:6043)
 w(17820:18967) = u(4)*w(4896:6043)
 w(18968:19739) = u(5)*w(5272:6043)
 w(19740:20239) = u(6)*w(5544:6043)
 w(20240:20543) = u(7)*w(5740:6043)
 w(20544:20704) = u(8)*w(5883:6043)
 w(20705:20795) = u(9)*w(1470:1560)
 w(20796:20873) = u(10)*w(1483:1560)
 w(20874:20939) = u(11)*w(1495:1560)
 w(20940:20994) = u(12)*w(1506:1560)
 w(20995:21039) = u(13)*w(1516:1560)
 w(21040:21075) = u(14)*w(1525:1560)
 w(21076:21103) = u(15)*w(1533:1560)
 w(21104:21124) = u(16)*w(1540:1560)
 w(21125:21139) = u(17)*w(1546:1560)
 w(21140:21149) = u(18)*w(1551:1560)
 w(21150:21155) = u(19)*w(1555:1560)
 w(21156:21158) = u(20)*w(1558:1560)
 w(21159) = u(21)*w(1560)
 w(21160:21163) = u(22)*w(350:353)
 w(21164:21166) = u(23)*w(351:353)
 w(21167:21168) = u(24)*w(352:353)
 w(21169) = u(25)*w(353)
 w(21170) = u(26)*w(0)
 w(21171) = u(27)*w(0)
endif
! terms of degree 7
if (7.le.mxd) then
 w(21172:36299) = u(0)*w(6044:21171)
 w(36300:46944) = u(1)*w(10527:21171)
 w(46945:54313) = u(2)*w(13803:21171)
 w(54314:59327) = u(3)*w(16158:21171)
 w(59328:62679) = u(4)*w(17820:21171)
 w(62680:64883) = u(5)*w(18968:21171)
 w(64884:66315) = u(6)*w(19740:21171)
 w(66316:67247) = u(7)*w(20240:21171)
 w(67248:67875) = u(8)*w(20544:21171)
 w(67876:67927) = u(9)*w(5992:6043)
 w(67928:67975) = u(10)*w(5996:6043)
 w(67976:68019) = u(11)*w(6000:6043)
 w(68020:68059) = u(12)*w(6004:6043)
 w(68060:68095) = u(13)*w(6008:6043)
 w(68096:68127) = u(14)*w(6012:6043)
 w(68128:68155) = u(15)*w(6016:6043)
 w(68156:68179) = u(16)*w(6020:6043)
 w(68180:68199) = u(17)*w(6024:6043)
 w(68200:68215) = u(18)*w(6028:6043)
 w(68216:68227) = u(19)*w(6032:6043)
 w(68228:68235) = u(20)*w(6036:6043)
 w(68236:68239) = u(21)*w(6040:6043)
endif
! terms of degree 8
if (8.le.mxd) then
 w(68240:115307) = u(0)*w(21172:68239)
 w(115308:147247) = u(1)*w(36300:68239)
 w(147248:168542) = u(2)*w(46945:68239)
 w(168543:182468) = u(3)*w(54314:68239)
 w(182469:191380) = u(4)*w(59328:68239)
 w(191381:196940) = u(5)*w(62680:68239)
 w(196941:200296) = u(6)*w(64884:68239)
 w(200297:202220) = u(7)*w(66316:68239)
 w(202221:203212) = u(8)*w(67248:68239)
 w(203213:203679) = u(9)*w(20705:21171)
 w(203680:204055) = u(10)*w(20796:21171)
 w(204056:204353) = u(11)*w(20874:21171)
 w(204354:204585) = u(12)*w(20940:21171)
 w(204586:204762) = u(13)*w(20995:21171)
 w(204763:204894) = u(14)*w(21040:21171)
 w(204895:204990) = u(15)*w(21076:21171)
 w(204991:205058) = u(16)*w(21104:21171)
 w(205059:205105) = u(17)*w(21125:21171)
 w(205106:205137) = u(18)*w(21140:21171)
 w(205138:205159) = u(19)*w(21150:21171)
 w(205160:205175) = u(20)*w(21156:21171)
 w(205176:205188) = u(21)*w(21159:21171)
endif
! terms of degree 9
if (9.le.mxd) then
 w(205189:342137) = u(0)*w(68240:205188)
 w(342138:432018) = u(1)*w(115308:205188)
 w(432019:489959) = u(2)*w(147248:205188)
 w(489960:526605) = u(3)*w(168543:205188)
 w(526606:549325) = u(4)*w(182469:205188)
 w(549326:563133) = u(5)*w(191381:205188)
 w(563134:571381) = u(6)*w(196941:205188)
 w(571382:576273) = u(7)*w(200297:205188)
 w(576274:579241) = u(8)*w(202221:205188)
 w(579242:579605) = u(9)*w(67876:68239)
 w(579606:579917) = u(10)*w(67928:68239)
 w(579918:580181) = u(11)*w(67976:68239)
 w(580182:580401) = u(12)*w(68020:68239)
 w(580402:580581) = u(13)*w(68060:68239)
 w(580582:580725) = u(14)*w(68096:68239)
 w(580726:580837) = u(15)*w(68128:68239)
 w(580838:580921) = u(16)*w(68156:68239)
 w(580922:580981) = u(17)*w(68180:68239)
 w(580982:581021) = u(18)*w(68200:68239)
 w(581022:581045) = u(19)*w(68216:68239)
 w(581046:581057) = u(20)*w(68228:68239)
 w(581058:581061) = u(21)*w(68236:68239)
 w(581062:581073) = u(22)*w(21160:21171)
 w(581074:581081) = u(23)*w(21164:21171)
 w(581082:581086) = u(24)*w(21167:21171)
 w(581087:581089) = u(25)*w(21169:21171)
endif
END SUBROUTINE mg3221_prib
