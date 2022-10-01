SUBROUTINE mg22111_prib (mxd, u, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: u(0:)
real (kind=wp), intent (out) :: w(0:mg22111_npb(mxd)-1)
!-----------------------------------------------------------------------
if (size(u).ne.nr) then
 stop 'mg22111_prib: bad size u'
!! else if (size(w).ne.mg22111_npb(mxd)) then
!!  stop 'mg22111_prib: bad size w'
endif
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 12 9 0 0 0 0 0 0 0
! prib,  dnpb(0:*): 1 12 87 472 2112 8184 28336 89496 261756 716936
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
endif
! terms of degree 2
if (2.le.mxd) then
 w(13:24) = u(0)*w(1:12)
 w(25:35) = u(1)*w(2:12)
 w(36:45) = u(2)*w(3:12)
 w(46:54) = u(3)*w(4:12)
 w(55:62) = u(4)*w(5:12)
 w(63:69) = u(5)*w(6:12)
 w(70:75) = u(6)*w(7:12)
 w(76:80) = u(7)*w(8:12)
 w(81:84) = u(8)*w(9:12)
 w(85:87) = u(9)*w(10:12)
 w(88:89) = u(10)*w(11:12)
 w(90) = u(11)*w(12)
 w(91) = u(12)*w(0)
 w(92) = u(13)*w(0)
 w(93) = u(14)*w(0)
 w(94) = u(15)*w(0)
 w(95) = u(16)*w(0)
 w(96) = u(17)*w(0)
 w(97) = u(18)*w(0)
 w(98) = u(19)*w(0)
 w(99) = u(20)*w(0)
endif
! terms of degree 3
if (3.le.mxd) then
 w(100:186) = u(0)*w(13:99)
 w(187:261) = u(1)*w(25:99)
 w(262:325) = u(2)*w(36:99)
 w(326:379) = u(3)*w(46:99)
 w(380:424) = u(4)*w(55:99)
 w(425:461) = u(5)*w(63:99)
 w(462:491) = u(6)*w(70:99)
 w(492:515) = u(7)*w(76:99)
 w(516:534) = u(8)*w(81:99)
 w(535:549) = u(9)*w(85:99)
 w(550:561) = u(10)*w(88:99)
 w(562:571) = u(11)*w(90:99)
endif
! terms of degree 4
if (4.le.mxd) then
 w(572:1043) = u(0)*w(100:571)
 w(1044:1428) = u(1)*w(187:571)
 w(1429:1738) = u(2)*w(262:571)
 w(1739:1984) = u(3)*w(326:571)
 w(1985:2176) = u(4)*w(380:571)
 w(2177:2323) = u(5)*w(425:571)
 w(2324:2433) = u(6)*w(462:571)
 w(2434:2513) = u(7)*w(492:571)
 w(2514:2569) = u(8)*w(516:571)
 w(2570:2606) = u(9)*w(535:571)
 w(2607:2628) = u(10)*w(550:571)
 w(2629:2638) = u(11)*w(562:571)
 w(2639:2647) = u(12)*w(91:99)
 w(2648:2655) = u(13)*w(92:99)
 w(2656:2662) = u(14)*w(93:99)
 w(2663:2668) = u(15)*w(94:99)
 w(2669:2673) = u(16)*w(95:99)
 w(2674:2677) = u(17)*w(96:99)
 w(2678:2680) = u(18)*w(97:99)
 w(2681:2682) = u(19)*w(98:99)
 w(2683) = u(20)*w(99)
endif
! terms of degree 5
if (5.le.mxd) then
 w(2684:4795) = u(0)*w(572:2683)
 w(4796:6435) = u(1)*w(1044:2683)
 w(6436:7690) = u(2)*w(1429:2683)
 w(7691:8635) = u(3)*w(1739:2683)
 w(8636:9334) = u(4)*w(1985:2683)
 w(9335:9841) = u(5)*w(2177:2683)
 w(9842:10201) = u(6)*w(2324:2683)
 w(10202:10451) = u(7)*w(2434:2683)
 w(10452:10621) = u(8)*w(2514:2683)
 w(10622:10735) = u(9)*w(2570:2683)
 w(10736:10812) = u(10)*w(2607:2683)
 w(10813:10867) = u(11)*w(2629:2683)
endif
! terms of degree 6
if (6.le.mxd) then
 w(10868:19051) = u(0)*w(2684:10867)
 w(19052:25123) = u(1)*w(4796:10867)
 w(25124:29555) = u(2)*w(6436:10867)
 w(29556:32732) = u(3)*w(7691:10867)
 w(32733:34964) = u(4)*w(8636:10867)
 w(34965:36497) = u(5)*w(9335:10867)
 w(36498:37523) = u(6)*w(9842:10867)
 w(37524:38189) = u(7)*w(10202:10867)
 w(38190:38605) = u(8)*w(10452:10867)
 w(38606:38851) = u(9)*w(10622:10867)
 w(38852:38983) = u(10)*w(10736:10867)
 w(38984:39038) = u(11)*w(10813:10867)
 w(39039:39083) = u(12)*w(2639:2683)
 w(39084:39119) = u(13)*w(2648:2683)
 w(39120:39147) = u(14)*w(2656:2683)
 w(39148:39168) = u(15)*w(2663:2683)
 w(39169:39183) = u(16)*w(2669:2683)
 w(39184:39193) = u(17)*w(2674:2683)
 w(39194:39199) = u(18)*w(2678:2683)
 w(39200:39202) = u(19)*w(2681:2683)
 w(39203) = u(20)*w(2683)
endif
! terms of degree 7
if (7.le.mxd) then
 w(39204:67539) = u(0)*w(10868:39203)
 w(67540:87691) = u(1)*w(19052:39203)
 w(87692:101771) = u(2)*w(25124:39203)
 w(101772:111419) = u(3)*w(29556:39203)
 w(111420:117890) = u(4)*w(32733:39203)
 w(117891:122129) = u(5)*w(34965:39203)
 w(122130:124835) = u(6)*w(36498:39203)
 w(124836:126515) = u(7)*w(37524:39203)
 w(126516:127529) = u(8)*w(38190:39203)
 w(127530:128127) = u(9)*w(38606:39203)
 w(128128:128479) = u(10)*w(38852:39203)
 w(128480:128699) = u(11)*w(38984:39203)
endif
! terms of degree 8
if (8.le.mxd) then
 w(128700:218195) = u(0)*w(39204:128699)
 w(218196:279355) = u(1)*w(67540:128699)
 w(279356:320363) = u(2)*w(87692:128699)
 w(320364:347291) = u(3)*w(101772:128699)
 w(347292:364571) = u(4)*w(111420:128699)
 w(364572:375380) = u(5)*w(117891:128699)
 w(375381:381950) = u(6)*w(122130:128699)
 w(381951:385814) = u(7)*w(124836:128699)
 w(385815:387998) = u(8)*w(126516:128699)
 w(387999:389168) = u(9)*w(127530:128699)
 w(389169:389740) = u(10)*w(128128:128699)
 w(389741:389960) = u(11)*w(128480:128699)
 w(389961:390125) = u(12)*w(39039:39203)
 w(390126:390245) = u(13)*w(39084:39203)
 w(390246:390329) = u(14)*w(39120:39203)
 w(390330:390385) = u(15)*w(39148:39203)
 w(390386:390420) = u(16)*w(39169:39203)
 w(390421:390440) = u(17)*w(39184:39203)
 w(390441:390450) = u(18)*w(39194:39203)
 w(390451:390454) = u(19)*w(39200:39203)
 w(390455) = u(20)*w(39203)
endif
! terms of degree 9
if (9.le.mxd) then
 w(390456:652211) = u(0)*w(128700:390455)
 w(652212:824471) = u(1)*w(218196:390455)
 w(824472:935571) = u(2)*w(279356:390455)
 w(935572:1005663) = u(3)*w(320364:390455)
 w(1005664:1048827) = u(4)*w(347292:390455)
 w(1048828:1074711) = u(5)*w(364572:390455)
 w(1074712:1089786) = u(6)*w(375381:390455)
 w(1089787:1098291) = u(7)*w(381951:390455)
 w(1098292:1102932) = u(8)*w(385815:390455)
 w(1102933:1105389) = u(9)*w(387999:390455)
 w(1105390:1106676) = u(10)*w(389169:390455)
 w(1106677:1107391) = u(11)*w(389741:390455)
endif
END SUBROUTINE mg22111_prib
