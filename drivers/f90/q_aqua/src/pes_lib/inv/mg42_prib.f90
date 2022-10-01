SUBROUTINE mg42_prib (mxd, u, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: u(0:)
real (kind=wp), intent (out) :: w(0:mg42_npb(mxd)-1)
!-----------------------------------------------------------------------
if (size(u).ne.nr) then
 stop 'mg42_prib: bad size u'
!! else if (size(w).ne.mg42_npb(mxd)) then
!!  stop 'mg42_prib: bad size w'
endif
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 3 5 3 3 0 1 0 0 0
! prib,  dnpb(0:*): 1 3 11 28 72 158 343 681 1328 2447
! constant term
w(0) = 1
! terms of degree 1
if (1.le.mxd) then
 w(1) = u(0)*w(0)
 w(2) = u(1)*w(0)
 w(3) = u(2)*w(0)
endif
! terms of degree 2
if (2.le.mxd) then
 w(4:6) = u(0)*w(1:3)
 w(7:8) = u(1)*w(2:3)
 w(9) = u(2)*w(3)
 w(10) = u(3)*w(0)
 w(11) = u(4)*w(0)
 w(12) = u(5)*w(0)
 w(13) = u(6)*w(0)
 w(14) = u(7)*w(0)
endif
! terms of degree 3
if (3.le.mxd) then
 w(15:25) = u(0)*w(4:14)
 w(26:33) = u(1)*w(7:14)
 w(34:39) = u(2)*w(9:14)
 w(40) = u(8)*w(0)
 w(41) = u(9)*w(0)
 w(42) = u(10)*w(0)
endif
! terms of degree 4
if (4.le.mxd) then
 w(43:70) = u(0)*w(15:42)
 w(71:87) = u(1)*w(26:42)
 w(88:96) = u(2)*w(34:42)
 w(97:101) = u(3)*w(10:14)
 w(102:105) = u(4)*w(11:14)
 w(106:108) = u(5)*w(12:14)
 w(109:110) = u(6)*w(13:14)
 w(111) = u(7)*w(14)
 w(112) = u(11)*w(0)
 w(113) = u(12)*w(0)
 w(114) = u(13)*w(0)
endif
! terms of degree 5
if (5.le.mxd) then
 w(115:186) = u(0)*w(43:114)
 w(187:230) = u(1)*w(71:114)
 w(231:257) = u(2)*w(88:114)
 w(258:260) = u(3)*w(40:42)
 w(261:263) = u(4)*w(40:42)
 w(264:266) = u(5)*w(40:42)
 w(267:269) = u(6)*w(40:42)
 w(270:272) = u(7)*w(40:42)
endif
! terms of degree 6
if (6.le.mxd) then
 w(273:430) = u(0)*w(115:272)
 w(431:516) = u(1)*w(187:272)
 w(517:558) = u(2)*w(231:272)
 w(559:576) = u(3)*w(97:114)
 w(577:589) = u(4)*w(102:114)
 w(590:598) = u(5)*w(106:114)
 w(599:604) = u(6)*w(109:114)
 w(605:608) = u(7)*w(111:114)
 w(609:611) = u(8)*w(40:42)
 w(612:613) = u(9)*w(41:42)
 w(614) = u(10)*w(42)
 w(615) = u(14)*w(0)
endif
! terms of degree 7
if (7.le.mxd) then
 w(616:958) = u(0)*w(273:615)
 w(959:1143) = u(1)*w(431:615)
 w(1144:1242) = u(2)*w(517:615)
 w(1243:1257) = u(3)*w(258:272)
 w(1258:1269) = u(4)*w(261:272)
 w(1270:1278) = u(5)*w(264:272)
 w(1279:1284) = u(6)*w(267:272)
 w(1285:1287) = u(7)*w(270:272)
 w(1288:1290) = u(8)*w(112:114)
 w(1291:1293) = u(9)*w(112:114)
 w(1294:1296) = u(10)*w(112:114)
endif
! terms of degree 8
if (8.le.mxd) then
 w(1297:1977) = u(0)*w(616:1296)
 w(1978:2315) = u(1)*w(959:1296)
 w(2316:2468) = u(2)*w(1144:1296)
 w(2469:2525) = u(3)*w(559:615)
 w(2526:2564) = u(4)*w(577:615)
 w(2565:2590) = u(5)*w(590:615)
 w(2591:2607) = u(6)*w(599:615)
 w(2608:2618) = u(7)*w(605:615)
 w(2619:2621) = u(11)*w(112:114)
 w(2622:2623) = u(12)*w(113:114)
 w(2624) = u(13)*w(114)
endif
! terms of degree 9
if (9.le.mxd) then
 w(2625:3952) = u(0)*w(1297:2624)
 w(3953:4599) = u(1)*w(1978:2624)
 w(4600:4908) = u(2)*w(2316:2624)
 w(4909:4962) = u(3)*w(1243:1296)
 w(4963:5001) = u(4)*w(1258:1296)
 w(5002:5028) = u(5)*w(1270:1296)
 w(5029:5046) = u(6)*w(1279:1296)
 w(5047:5058) = u(7)*w(1285:1296)
 w(5059:5065) = u(8)*w(609:615)
 w(5066:5069) = u(9)*w(612:615)
 w(5070:5071) = u(10)*w(614:615)
endif
END SUBROUTINE mg42_prib