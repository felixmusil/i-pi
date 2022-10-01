SUBROUTINE mg521_prib (mxd, u, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: u(0:)
real (kind=wp), intent (out) :: w(0:mg521_npb(mxd)-1)
!-----------------------------------------------------------------------
if (size(u).ne.nr) then
 stop 'mg521_prib: bad size u'
!! else if (size(w).ne.mg521_npb(mxd)) then
!!  stop 'mg521_prib: bad size w'
endif
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 5 7 4 5 4 2 0 0 0
! prib,  dnpb(0:*): 1 5 22 74 228 628 1626 3942 9129 20209
! constant term
w(0) = 1
! terms of degree 1
if (1.le.mxd) then
 w(1) = u(0)*w(0)
 w(2) = u(1)*w(0)
 w(3) = u(2)*w(0)
 w(4) = u(3)*w(0)
 w(5) = u(4)*w(0)
endif
! terms of degree 2
if (2.le.mxd) then
 w(6:10) = u(0)*w(1:5)
 w(11:14) = u(1)*w(2:5)
 w(15:17) = u(2)*w(3:5)
 w(18:19) = u(3)*w(4:5)
 w(20) = u(4)*w(5)
 w(21) = u(5)*w(0)
 w(22) = u(6)*w(0)
 w(23) = u(7)*w(0)
 w(24) = u(8)*w(0)
 w(25) = u(9)*w(0)
 w(26) = u(10)*w(0)
 w(27) = u(11)*w(0)
endif
! terms of degree 3
if (3.le.mxd) then
 w(28:49) = u(0)*w(6:27)
 w(50:66) = u(1)*w(11:27)
 w(67:79) = u(2)*w(15:27)
 w(80:89) = u(3)*w(18:27)
 w(90:97) = u(4)*w(20:27)
 w(98) = u(12)*w(0)
 w(99) = u(13)*w(0)
 w(100) = u(14)*w(0)
 w(101) = u(15)*w(0)
endif
! terms of degree 4
if (4.le.mxd) then
 w(102:175) = u(0)*w(28:101)
 w(176:227) = u(1)*w(50:101)
 w(228:262) = u(2)*w(67:101)
 w(263:284) = u(3)*w(80:101)
 w(285:296) = u(4)*w(90:101)
 w(297:303) = u(5)*w(21:27)
 w(304:309) = u(6)*w(22:27)
 w(310:314) = u(7)*w(23:27)
 w(315:318) = u(8)*w(24:27)
 w(319:321) = u(9)*w(25:27)
 w(322:323) = u(10)*w(26:27)
 w(324) = u(11)*w(27)
 w(325) = u(16)*w(0)
 w(326) = u(17)*w(0)
 w(327) = u(18)*w(0)
 w(328) = u(19)*w(0)
 w(329) = u(20)*w(0)
endif
! terms of degree 5
if (5.le.mxd) then
 w(330:557) = u(0)*w(102:329)
 w(558:711) = u(1)*w(176:329)
 w(712:813) = u(2)*w(228:329)
 w(814:880) = u(3)*w(263:329)
 w(881:925) = u(4)*w(285:329)
 w(926:929) = u(5)*w(98:101)
 w(930:933) = u(6)*w(98:101)
 w(934:937) = u(7)*w(98:101)
 w(938:941) = u(8)*w(98:101)
 w(942:945) = u(9)*w(98:101)
 w(946:949) = u(10)*w(98:101)
 w(950:953) = u(11)*w(98:101)
 w(954) = u(21)*w(0)
 w(955) = u(22)*w(0)
 w(956) = u(23)*w(0)
 w(957) = u(24)*w(0)
endif
! terms of degree 6
if (6.le.mxd) then
 w(958:1585) = u(0)*w(330:957)
 w(1586:1985) = u(1)*w(558:957)
 w(1986:2231) = u(2)*w(712:957)
 w(2232:2375) = u(3)*w(814:957)
 w(2376:2452) = u(4)*w(881:957)
 w(2453:2485) = u(5)*w(297:329)
 w(2486:2511) = u(6)*w(304:329)
 w(2512:2531) = u(7)*w(310:329)
 w(2532:2546) = u(8)*w(315:329)
 w(2547:2557) = u(9)*w(319:329)
 w(2558:2565) = u(10)*w(322:329)
 w(2566:2571) = u(11)*w(324:329)
 w(2572:2575) = u(12)*w(98:101)
 w(2576:2578) = u(13)*w(99:101)
 w(2579:2580) = u(14)*w(100:101)
 w(2581) = u(15)*w(101)
 w(2582) = u(25)*w(0)
 w(2583) = u(26)*w(0)
endif
! terms of degree 7
if (7.le.mxd) then
 w(2584:4209) = u(0)*w(958:2583)
 w(4210:5207) = u(1)*w(1586:2583)
 w(5208:5805) = u(2)*w(1986:2583)
 w(5806:6157) = u(3)*w(2232:2583)
 w(6158:6365) = u(4)*w(2376:2583)
 w(6366:6397) = u(5)*w(926:957)
 w(6398:6425) = u(6)*w(930:957)
 w(6426:6449) = u(7)*w(934:957)
 w(6450:6469) = u(8)*w(938:957)
 w(6470:6485) = u(9)*w(942:957)
 w(6486:6497) = u(10)*w(946:957)
 w(6498:6505) = u(11)*w(950:957)
 w(6506:6510) = u(12)*w(325:329)
 w(6511:6515) = u(13)*w(325:329)
 w(6516:6520) = u(14)*w(325:329)
 w(6521:6525) = u(15)*w(325:329)
endif
! terms of degree 8
if (8.le.mxd) then
 w(6526:10467) = u(0)*w(2584:6525)
 w(10468:12783) = u(1)*w(4210:6525)
 w(12784:14101) = u(2)*w(5208:6525)
 w(14102:14821) = u(3)*w(5806:6525)
 w(14822:15189) = u(4)*w(6158:6525)
 w(15190:15320) = u(5)*w(2453:2583)
 w(15321:15418) = u(6)*w(2486:2583)
 w(15419:15490) = u(7)*w(2512:2583)
 w(15491:15542) = u(8)*w(2532:2583)
 w(15543:15579) = u(9)*w(2547:2583)
 w(15580:15605) = u(10)*w(2558:2583)
 w(15606:15623) = u(11)*w(2566:2583)
 w(15624:15627) = u(12)*w(954:957)
 w(15628:15631) = u(13)*w(954:957)
 w(15632:15635) = u(14)*w(954:957)
 w(15636:15639) = u(15)*w(954:957)
 w(15640:15644) = u(16)*w(325:329)
 w(15645:15648) = u(17)*w(326:329)
 w(15649:15651) = u(18)*w(327:329)
 w(15652:15653) = u(19)*w(328:329)
 w(15654) = u(20)*w(329)
endif
! terms of degree 9
if (9.le.mxd) then
 w(15655:24783) = u(0)*w(6526:15654)
 w(24784:29970) = u(1)*w(10468:15654)
 w(29971:32841) = u(2)*w(12784:15654)
 w(32842:34394) = u(3)*w(14102:15654)
 w(34395:35227) = u(4)*w(14822:15654)
 w(35228:35387) = u(5)*w(6366:6525)
 w(35388:35515) = u(6)*w(6398:6525)
 w(35516:35615) = u(7)*w(6426:6525)
 w(35616:35691) = u(8)*w(6450:6525)
 w(35692:35747) = u(9)*w(6470:6525)
 w(35748:35787) = u(10)*w(6486:6525)
 w(35788:35815) = u(11)*w(6498:6525)
 w(35816:35827) = u(12)*w(2572:2583)
 w(35828:35835) = u(13)*w(2576:2583)
 w(35836:35840) = u(14)*w(2579:2583)
 w(35841:35843) = u(15)*w(2581:2583)
 w(35844:35847) = u(16)*w(954:957)
 w(35848:35851) = u(17)*w(954:957)
 w(35852:35855) = u(18)*w(954:957)
 w(35856:35859) = u(19)*w(954:957)
 w(35860:35863) = u(20)*w(954:957)
endif
END SUBROUTINE mg521_prib
