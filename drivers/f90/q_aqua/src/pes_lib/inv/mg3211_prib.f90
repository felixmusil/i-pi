SUBROUTINE mg3211_prib (mxd, u, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: u(0:)
real (kind=wp), intent (out) :: w(0:mg3211_npb(mxd)-1)
!-----------------------------------------------------------------------
if (size(u).ne.nr) then
 stop 'mg3211_prib: bad size u'
!! else if (size(w).ne.mg3211_npb(mxd)) then
!!  stop 'mg3211_prib: bad size w'
endif
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 8 8 4 0 0 1 0 0 0
! prib,  dnpb(0:*): 1 8 44 188 686 2216 6519 17752 45337 109584
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
endif
! terms of degree 3
if (3.le.mxd) then
 w(53:96) = u(0)*w(9:52)
 w(97:132) = u(1)*w(17:52)
 w(133:161) = u(2)*w(24:52)
 w(162:184) = u(3)*w(30:52)
 w(185:202) = u(4)*w(35:52)
 w(203:216) = u(5)*w(39:52)
 w(217:227) = u(6)*w(42:52)
 w(228:236) = u(7)*w(44:52)
 w(237) = u(16)*w(0)
 w(238) = u(17)*w(0)
 w(239) = u(18)*w(0)
 w(240) = u(19)*w(0)
endif
! terms of degree 4
if (4.le.mxd) then
 w(241:428) = u(0)*w(53:240)
 w(429:572) = u(1)*w(97:240)
 w(573:680) = u(2)*w(133:240)
 w(681:759) = u(3)*w(162:240)
 w(760:815) = u(4)*w(185:240)
 w(816:853) = u(5)*w(203:240)
 w(854:877) = u(6)*w(217:240)
 w(878:890) = u(7)*w(228:240)
 w(891:898) = u(8)*w(45:52)
 w(899:905) = u(9)*w(46:52)
 w(906:911) = u(10)*w(47:52)
 w(912:916) = u(11)*w(48:52)
 w(917:920) = u(12)*w(49:52)
 w(921:923) = u(13)*w(50:52)
 w(924:925) = u(14)*w(51:52)
 w(926) = u(15)*w(52)
endif
! terms of degree 5
if (5.le.mxd) then
 w(927:1612) = u(0)*w(241:926)
 w(1613:2110) = u(1)*w(429:926)
 w(2111:2464) = u(2)*w(573:926)
 w(2465:2710) = u(3)*w(681:926)
 w(2711:2877) = u(4)*w(760:926)
 w(2878:2988) = u(5)*w(816:926)
 w(2989:3061) = u(6)*w(854:926)
 w(3062:3110) = u(7)*w(878:926)
 w(3111:3114) = u(8)*w(237:240)
 w(3115:3118) = u(9)*w(237:240)
 w(3119:3122) = u(10)*w(237:240)
 w(3123:3126) = u(11)*w(237:240)
 w(3127:3130) = u(12)*w(237:240)
 w(3131:3134) = u(13)*w(237:240)
 w(3135:3138) = u(14)*w(237:240)
 w(3139:3142) = u(15)*w(237:240)
endif
! terms of degree 6
if (6.le.mxd) then
 w(3143:5358) = u(0)*w(927:3142)
 w(5359:6888) = u(1)*w(1613:3142)
 w(6889:7920) = u(2)*w(2111:3142)
 w(7921:8598) = u(3)*w(2465:3142)
 w(8599:9030) = u(4)*w(2711:3142)
 w(9031:9295) = u(5)*w(2878:3142)
 w(9296:9449) = u(6)*w(2989:3142)
 w(9450:9530) = u(7)*w(3062:3142)
 w(9531:9566) = u(8)*w(891:926)
 w(9567:9594) = u(9)*w(899:926)
 w(9595:9615) = u(10)*w(906:926)
 w(9616:9630) = u(11)*w(912:926)
 w(9631:9640) = u(12)*w(917:926)
 w(9641:9646) = u(13)*w(921:926)
 w(9647:9649) = u(14)*w(924:926)
 w(9650) = u(15)*w(926)
 w(9651:9654) = u(16)*w(237:240)
 w(9655:9657) = u(17)*w(238:240)
 w(9658:9659) = u(18)*w(239:240)
 w(9660) = u(19)*w(240)
 w(9661) = u(20)*w(0)
endif
! terms of degree 7
if (7.le.mxd) then
 w(9662:16180) = u(0)*w(3143:9661)
 w(16181:20483) = u(1)*w(5359:9661)
 w(20484:23256) = u(2)*w(6889:9661)
 w(23257:24997) = u(3)*w(7921:9661)
 w(24998:26060) = u(4)*w(8599:9661)
 w(26061:26691) = u(5)*w(9031:9661)
 w(26692:27057) = u(6)*w(9296:9661)
 w(27058:27269) = u(7)*w(9450:9661)
 w(27270:27301) = u(8)*w(3111:3142)
 w(27302:27329) = u(9)*w(3115:3142)
 w(27330:27353) = u(10)*w(3119:3142)
 w(27354:27373) = u(11)*w(3123:3142)
 w(27374:27389) = u(12)*w(3127:3142)
 w(27390:27401) = u(13)*w(3131:3142)
 w(27402:27409) = u(14)*w(3135:3142)
 w(27410:27413) = u(15)*w(3139:3142)
endif
! terms of degree 8
if (8.le.mxd) then
 w(27414:45165) = u(0)*w(9662:27413)
 w(45166:56398) = u(1)*w(16181:27413)
 w(56399:63328) = u(2)*w(20484:27413)
 w(63329:67485) = u(3)*w(23257:27413)
 w(67486:69901) = u(4)*w(24998:27413)
 w(69902:71254) = u(5)*w(26061:27413)
 w(71255:71976) = u(6)*w(26692:27413)
 w(71977:72332) = u(7)*w(27058:27413)
 w(72333:72463) = u(8)*w(9531:9661)
 w(72464:72558) = u(9)*w(9567:9661)
 w(72559:72625) = u(10)*w(9595:9661)
 w(72626:72671) = u(11)*w(9616:9661)
 w(72672:72702) = u(12)*w(9631:9661)
 w(72703:72723) = u(13)*w(9641:9661)
 w(72724:72738) = u(14)*w(9647:9661)
 w(72739:72750) = u(15)*w(9650:9661)
endif
! terms of degree 9
if (9.le.mxd) then
 w(72751:118087) = u(0)*w(27414:72750)
 w(118088:145672) = u(1)*w(45166:72750)
 w(145673:162024) = u(2)*w(56399:72750)
 w(162025:171446) = u(3)*w(63329:72750)
 w(171447:176711) = u(4)*w(67486:72750)
 w(176712:179560) = u(5)*w(69902:72750)
 w(179561:181056) = u(6)*w(71255:72750)
 w(181057:181830) = u(7)*w(71977:72750)
 w(181831:181974) = u(8)*w(27270:27413)
 w(181975:182086) = u(9)*w(27302:27413)
 w(182087:182170) = u(10)*w(27330:27413)
 w(182171:182230) = u(11)*w(27354:27413)
 w(182231:182270) = u(12)*w(27374:27413)
 w(182271:182294) = u(13)*w(27390:27413)
 w(182295:182306) = u(14)*w(27402:27413)
 w(182307:182310) = u(15)*w(27410:27413)
 w(182311:182321) = u(16)*w(9651:9661)
 w(182322:182328) = u(17)*w(9655:9661)
 w(182329:182332) = u(18)*w(9658:9661)
 w(182333:182334) = u(19)*w(9660:9661)
endif
END SUBROUTINE mg3211_prib