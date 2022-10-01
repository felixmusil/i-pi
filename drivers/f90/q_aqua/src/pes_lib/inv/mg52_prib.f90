SUBROUTINE mg52_prib (mxd, u, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: u(0:)
real (kind=wp), intent (out) :: w(0:mg52_npb(mxd)-1)
!-----------------------------------------------------------------------
if (size(u).ne.nr) then
 stop 'mg52_prib: bad size u'
!! else if (size(w).ne.mg52_npb(mxd)) then
!!  stop 'mg52_prib: bad size w'
endif
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 3 5 3 4 3 2 0 0 0
! prib,  dnpb(0:*): 1 3 11 28 73 164 364 745 1496 2855
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
 w(115) = u(14)*w(0)
endif
! terms of degree 5
if (5.le.mxd) then
 w(116:188) = u(0)*w(43:115)
 w(189:233) = u(1)*w(71:115)
 w(234:261) = u(2)*w(88:115)
 w(262:264) = u(3)*w(40:42)
 w(265:267) = u(4)*w(40:42)
 w(268:270) = u(5)*w(40:42)
 w(271:273) = u(6)*w(40:42)
 w(274:276) = u(7)*w(40:42)
 w(277) = u(15)*w(0)
 w(278) = u(16)*w(0)
 w(279) = u(17)*w(0)
endif
! terms of degree 6
if (6.le.mxd) then
 w(280:443) = u(0)*w(116:279)
 w(444:534) = u(1)*w(189:279)
 w(535:580) = u(2)*w(234:279)
 w(581:599) = u(3)*w(97:115)
 w(600:613) = u(4)*w(102:115)
 w(614:623) = u(5)*w(106:115)
 w(624:630) = u(6)*w(109:115)
 w(631:635) = u(7)*w(111:115)
 w(636:638) = u(8)*w(40:42)
 w(639:640) = u(9)*w(41:42)
 w(641) = u(10)*w(42)
 w(642) = u(18)*w(0)
 w(643) = u(19)*w(0)
endif
! terms of degree 7
if (7.le.mxd) then
 w(644:1007) = u(0)*w(280:643)
 w(1008:1207) = u(1)*w(444:643)
 w(1208:1316) = u(2)*w(535:643)
 w(1317:1334) = u(3)*w(262:279)
 w(1335:1349) = u(4)*w(265:279)
 w(1350:1361) = u(5)*w(268:279)
 w(1362:1370) = u(6)*w(271:279)
 w(1371:1376) = u(7)*w(274:279)
 w(1377:1380) = u(8)*w(112:115)
 w(1381:1384) = u(9)*w(112:115)
 w(1385:1388) = u(10)*w(112:115)
endif
! terms of degree 8
if (8.le.mxd) then
 w(1389:2133) = u(0)*w(644:1388)
 w(2134:2514) = u(1)*w(1008:1388)
 w(2515:2695) = u(2)*w(1208:1388)
 w(2696:2758) = u(3)*w(581:643)
 w(2759:2802) = u(4)*w(600:643)
 w(2803:2832) = u(5)*w(614:643)
 w(2833:2852) = u(6)*w(624:643)
 w(2853:2865) = u(7)*w(631:643)
 w(2866:2868) = u(8)*w(277:279)
 w(2869:2871) = u(9)*w(277:279)
 w(2872:2874) = u(10)*w(277:279)
 w(2875:2878) = u(11)*w(112:115)
 w(2879:2881) = u(12)*w(113:115)
 w(2882:2883) = u(13)*w(114:115)
 w(2884) = u(14)*w(115)
endif
! terms of degree 9
if (9.le.mxd) then
 w(2885:4380) = u(0)*w(1389:2884)
 w(4381:5131) = u(1)*w(2134:2884)
 w(5132:5501) = u(2)*w(2515:2884)
 w(5502:5573) = u(3)*w(1317:1388)
 w(5574:5627) = u(4)*w(1335:1388)
 w(5628:5666) = u(5)*w(1350:1388)
 w(5667:5693) = u(6)*w(1362:1388)
 w(5694:5711) = u(7)*w(1371:1388)
 w(5712:5719) = u(8)*w(636:643)
 w(5720:5724) = u(9)*w(639:643)
 w(5725:5727) = u(10)*w(641:643)
 w(5728:5730) = u(11)*w(277:279)
 w(5731:5733) = u(12)*w(277:279)
 w(5734:5736) = u(13)*w(277:279)
 w(5737:5739) = u(14)*w(277:279)
endif
END SUBROUTINE mg52_prib