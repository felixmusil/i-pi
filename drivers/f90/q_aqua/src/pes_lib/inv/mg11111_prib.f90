SUBROUTINE mg11111_prib (mxd, u, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: u(0:)
real (kind=wp), intent (out) :: w(0:mg11111_npb(mxd)-1)
!-----------------------------------------------------------------------
if (size(u).ne.nr) then
 stop 'mg11111_prib: bad size u'
!! else if (size(w).ne.mg11111_npb(mxd)) then
!!  stop 'mg11111_prib: bad size w'
endif
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 10 0 0 0 0 0 0 0 0
! prib,  dnpb(0:*): 1 10 55 220 715 2002 5005 11440 24310 48620
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
endif
! terms of degree 2
if (2.le.mxd) then
 w(11:20) = u(0)*w(1:10)
 w(21:29) = u(1)*w(2:10)
 w(30:37) = u(2)*w(3:10)
 w(38:44) = u(3)*w(4:10)
 w(45:50) = u(4)*w(5:10)
 w(51:55) = u(5)*w(6:10)
 w(56:59) = u(6)*w(7:10)
 w(60:62) = u(7)*w(8:10)
 w(63:64) = u(8)*w(9:10)
 w(65) = u(9)*w(10)
endif
! terms of degree 3
if (3.le.mxd) then
 w(66:120) = u(0)*w(11:65)
 w(121:165) = u(1)*w(21:65)
 w(166:201) = u(2)*w(30:65)
 w(202:229) = u(3)*w(38:65)
 w(230:250) = u(4)*w(45:65)
 w(251:265) = u(5)*w(51:65)
 w(266:275) = u(6)*w(56:65)
 w(276:281) = u(7)*w(60:65)
 w(282:284) = u(8)*w(63:65)
 w(285) = u(9)*w(65)
endif
! terms of degree 4
if (4.le.mxd) then
 w(286:505) = u(0)*w(66:285)
 w(506:670) = u(1)*w(121:285)
 w(671:790) = u(2)*w(166:285)
 w(791:874) = u(3)*w(202:285)
 w(875:930) = u(4)*w(230:285)
 w(931:965) = u(5)*w(251:285)
 w(966:985) = u(6)*w(266:285)
 w(986:995) = u(7)*w(276:285)
 w(996:999) = u(8)*w(282:285)
 w(1000) = u(9)*w(285)
endif
! terms of degree 5
if (5.le.mxd) then
 w(1001:1715) = u(0)*w(286:1000)
 w(1716:2210) = u(1)*w(506:1000)
 w(2211:2540) = u(2)*w(671:1000)
 w(2541:2750) = u(3)*w(791:1000)
 w(2751:2876) = u(4)*w(875:1000)
 w(2877:2946) = u(5)*w(931:1000)
 w(2947:2981) = u(6)*w(966:1000)
 w(2982:2996) = u(7)*w(986:1000)
 w(2997:3001) = u(8)*w(996:1000)
 w(3002) = u(9)*w(1000)
endif
! terms of degree 6
if (6.le.mxd) then
 w(3003:5004) = u(0)*w(1001:3002)
 w(5005:6291) = u(1)*w(1716:3002)
 w(6292:7083) = u(2)*w(2211:3002)
 w(7084:7545) = u(3)*w(2541:3002)
 w(7546:7797) = u(4)*w(2751:3002)
 w(7798:7923) = u(5)*w(2877:3002)
 w(7924:7979) = u(6)*w(2947:3002)
 w(7980:8000) = u(7)*w(2982:3002)
 w(8001:8006) = u(8)*w(2997:3002)
 w(8007) = u(9)*w(3002)
endif
! terms of degree 7
if (7.le.mxd) then
 w(8008:13012) = u(0)*w(3003:8007)
 w(13013:16015) = u(1)*w(5005:8007)
 w(16016:17731) = u(2)*w(6292:8007)
 w(17732:18655) = u(3)*w(7084:8007)
 w(18656:19117) = u(4)*w(7546:8007)
 w(19118:19327) = u(5)*w(7798:8007)
 w(19328:19411) = u(6)*w(7924:8007)
 w(19412:19439) = u(7)*w(7980:8007)
 w(19440:19446) = u(8)*w(8001:8007)
 w(19447) = u(9)*w(8007)
endif
! terms of degree 8
if (8.le.mxd) then
 w(19448:30887) = u(0)*w(8008:19447)
 w(30888:37322) = u(1)*w(13013:19447)
 w(37323:40754) = u(2)*w(16016:19447)
 w(40755:42470) = u(3)*w(17732:19447)
 w(42471:43262) = u(4)*w(18656:19447)
 w(43263:43592) = u(5)*w(19118:19447)
 w(43593:43712) = u(6)*w(19328:19447)
 w(43713:43748) = u(7)*w(19412:19447)
 w(43749:43756) = u(8)*w(19440:19447)
 w(43757) = u(9)*w(19447)
endif
! terms of degree 9
if (9.le.mxd) then
 w(43758:68067) = u(0)*w(19448:43757)
 w(68068:80937) = u(1)*w(30888:43757)
 w(80938:87372) = u(2)*w(37323:43757)
 w(87373:90375) = u(3)*w(40755:43757)
 w(90376:91662) = u(4)*w(42471:43757)
 w(91663:92157) = u(5)*w(43263:43757)
 w(92158:92322) = u(6)*w(43593:43757)
 w(92323:92367) = u(7)*w(43713:43757)
 w(92368:92376) = u(8)*w(43749:43757)
 w(92377) = u(9)*w(43757)
endif
END SUBROUTINE mg11111_prib
