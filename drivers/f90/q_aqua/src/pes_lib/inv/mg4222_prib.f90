SUBROUTINE mg4222_prib (mxd, u, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: u(0:)
real (kind=wp), intent (out) :: w(0:mg4222_npb(mxd)-1)
!-----------------------------------------------------------------------
if (size(u).ne.nr) then
 stop 'mg4222_prib: bad size u'
!! else if (size(w).ne.mg4222_npb(mxd)) then
!!  stop 'mg4222_prib: bad size w'
endif
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 10 20 5 7 0 3 0 0 0
! prib,  dnpb(0:*): 1 10 75 425 2082 8947 35038 126360 426528 1357194
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
 w(66) = u(10)*w(0)
 w(67) = u(11)*w(0)
 w(68) = u(12)*w(0)
 w(69) = u(13)*w(0)
 w(70) = u(14)*w(0)
 w(71) = u(15)*w(0)
 w(72) = u(16)*w(0)
 w(73) = u(17)*w(0)
 w(74) = u(18)*w(0)
 w(75) = u(19)*w(0)
 w(76) = u(20)*w(0)
 w(77) = u(21)*w(0)
 w(78) = u(22)*w(0)
 w(79) = u(23)*w(0)
 w(80) = u(24)*w(0)
 w(81) = u(25)*w(0)
 w(82) = u(26)*w(0)
 w(83) = u(27)*w(0)
 w(84) = u(28)*w(0)
 w(85) = u(29)*w(0)
endif
! terms of degree 3
if (3.le.mxd) then
 w(86:160) = u(0)*w(11:85)
 w(161:225) = u(1)*w(21:85)
 w(226:281) = u(2)*w(30:85)
 w(282:329) = u(3)*w(38:85)
 w(330:370) = u(4)*w(45:85)
 w(371:405) = u(5)*w(51:85)
 w(406:435) = u(6)*w(56:85)
 w(436:461) = u(7)*w(60:85)
 w(462:484) = u(8)*w(63:85)
 w(485:505) = u(9)*w(65:85)
 w(506) = u(30)*w(0)
 w(507) = u(31)*w(0)
 w(508) = u(32)*w(0)
 w(509) = u(33)*w(0)
 w(510) = u(34)*w(0)
endif
! terms of degree 4
if (4.le.mxd) then
 w(511:935) = u(0)*w(86:510)
 w(936:1285) = u(1)*w(161:510)
 w(1286:1570) = u(2)*w(226:510)
 w(1571:1799) = u(3)*w(282:510)
 w(1800:1980) = u(4)*w(330:510)
 w(1981:2120) = u(5)*w(371:510)
 w(2121:2225) = u(6)*w(406:510)
 w(2226:2300) = u(7)*w(436:510)
 w(2301:2349) = u(8)*w(462:510)
 w(2350:2375) = u(9)*w(485:510)
 w(2376:2395) = u(10)*w(66:85)
 w(2396:2414) = u(11)*w(67:85)
 w(2415:2432) = u(12)*w(68:85)
 w(2433:2449) = u(13)*w(69:85)
 w(2450:2465) = u(14)*w(70:85)
 w(2466:2480) = u(15)*w(71:85)
 w(2481:2494) = u(16)*w(72:85)
 w(2495:2507) = u(17)*w(73:85)
 w(2508:2519) = u(18)*w(74:85)
 w(2520:2530) = u(19)*w(75:85)
 w(2531:2540) = u(20)*w(76:85)
 w(2541:2549) = u(21)*w(77:85)
 w(2550:2557) = u(22)*w(78:85)
 w(2558:2564) = u(23)*w(79:85)
 w(2565:2570) = u(24)*w(80:85)
 w(2571:2575) = u(25)*w(81:85)
 w(2576:2579) = u(26)*w(82:85)
 w(2580:2582) = u(27)*w(83:85)
 w(2583:2584) = u(28)*w(84:85)
 w(2585) = u(29)*w(85)
 w(2586) = u(35)*w(0)
 w(2587) = u(36)*w(0)
 w(2588) = u(37)*w(0)
 w(2589) = u(38)*w(0)
 w(2590) = u(39)*w(0)
 w(2591) = u(40)*w(0)
 w(2592) = u(41)*w(0)
endif
! terms of degree 5
if (5.le.mxd) then
 w(2593:4674) = u(0)*w(511:2592)
 w(4675:6331) = u(1)*w(936:2592)
 w(6332:7638) = u(2)*w(1286:2592)
 w(7639:8660) = u(3)*w(1571:2592)
 w(8661:9453) = u(4)*w(1800:2592)
 w(9454:10065) = u(5)*w(1981:2592)
 w(10066:10537) = u(6)*w(2121:2592)
 w(10538:10904) = u(7)*w(2226:2592)
 w(10905:11196) = u(8)*w(2301:2592)
 w(11197:11439) = u(9)*w(2350:2592)
 w(11440:11444) = u(10)*w(506:510)
 w(11445:11449) = u(11)*w(506:510)
 w(11450:11454) = u(12)*w(506:510)
 w(11455:11459) = u(13)*w(506:510)
 w(11460:11464) = u(14)*w(506:510)
 w(11465:11469) = u(15)*w(506:510)
 w(11470:11474) = u(16)*w(506:510)
 w(11475:11479) = u(17)*w(506:510)
 w(11480:11484) = u(18)*w(506:510)
 w(11485:11489) = u(19)*w(506:510)
 w(11490:11494) = u(20)*w(506:510)
 w(11495:11499) = u(21)*w(506:510)
 w(11500:11504) = u(22)*w(506:510)
 w(11505:11509) = u(23)*w(506:510)
 w(11510:11514) = u(24)*w(506:510)
 w(11515:11519) = u(25)*w(506:510)
 w(11520:11524) = u(26)*w(506:510)
 w(11525:11529) = u(27)*w(506:510)
 w(11530:11534) = u(28)*w(506:510)
 w(11535:11539) = u(29)*w(506:510)
endif
! terms of degree 6
if (6.le.mxd) then
 w(11540:20486) = u(0)*w(2593:11539)
 w(20487:27351) = u(1)*w(4675:11539)
 w(27352:32559) = u(2)*w(6332:11539)
 w(32560:36460) = u(3)*w(7639:11539)
 w(36461:39339) = u(4)*w(8661:11539)
 w(39340:41425) = u(5)*w(9454:11539)
 w(41426:42899) = u(6)*w(10066:11539)
 w(42900:43901) = u(7)*w(10538:11539)
 w(43902:44536) = u(8)*w(10905:11539)
 w(44537:44879) = u(9)*w(11197:11539)
 w(44880:45096) = u(10)*w(2376:2592)
 w(45097:45293) = u(11)*w(2396:2592)
 w(45294:45471) = u(12)*w(2415:2592)
 w(45472:45631) = u(13)*w(2433:2592)
 w(45632:45774) = u(14)*w(2450:2592)
 w(45775:45901) = u(15)*w(2466:2592)
 w(45902:46013) = u(16)*w(2481:2592)
 w(46014:46111) = u(17)*w(2495:2592)
 w(46112:46196) = u(18)*w(2508:2592)
 w(46197:46269) = u(19)*w(2520:2592)
 w(46270:46331) = u(20)*w(2531:2592)
 w(46332:46383) = u(21)*w(2541:2592)
 w(46384:46426) = u(22)*w(2550:2592)
 w(46427:46461) = u(23)*w(2558:2592)
 w(46462:46489) = u(24)*w(2565:2592)
 w(46490:46511) = u(25)*w(2571:2592)
 w(46512:46528) = u(26)*w(2576:2592)
 w(46529:46541) = u(27)*w(2580:2592)
 w(46542:46551) = u(28)*w(2583:2592)
 w(46552:46559) = u(29)*w(2585:2592)
 w(46560:46564) = u(30)*w(506:510)
 w(46565:46568) = u(31)*w(507:510)
 w(46569:46571) = u(32)*w(508:510)
 w(46572:46573) = u(33)*w(509:510)
 w(46574) = u(34)*w(510)
 w(46575) = u(42)*w(0)
 w(46576) = u(43)*w(0)
 w(46577) = u(44)*w(0)
endif
! terms of degree 7
if (7.le.mxd) then
 w(46578:81615) = u(0)*w(11540:46577)
 w(81616:107706) = u(1)*w(20487:46577)
 w(107707:126932) = u(2)*w(27352:46577)
 w(126933:140950) = u(3)*w(32560:46577)
 w(140951:151067) = u(4)*w(36461:46577)
 w(151068:158305) = u(5)*w(39340:46577)
 w(158306:163457) = u(6)*w(41426:46577)
 w(163458:167135) = u(7)*w(42900:46577)
 w(167136:169811) = u(8)*w(43902:46577)
 w(169812:171852) = u(9)*w(44537:46577)
 w(171853:171952) = u(10)*w(11440:11539)
 w(171953:172047) = u(11)*w(11445:11539)
 w(172048:172137) = u(12)*w(11450:11539)
 w(172138:172222) = u(13)*w(11455:11539)
 w(172223:172302) = u(14)*w(11460:11539)
 w(172303:172377) = u(15)*w(11465:11539)
 w(172378:172447) = u(16)*w(11470:11539)
 w(172448:172512) = u(17)*w(11475:11539)
 w(172513:172572) = u(18)*w(11480:11539)
 w(172573:172627) = u(19)*w(11485:11539)
 w(172628:172677) = u(20)*w(11490:11539)
 w(172678:172722) = u(21)*w(11495:11539)
 w(172723:172762) = u(22)*w(11500:11539)
 w(172763:172797) = u(23)*w(11505:11539)
 w(172798:172827) = u(24)*w(11510:11539)
 w(172828:172852) = u(25)*w(11515:11539)
 w(172853:172872) = u(26)*w(11520:11539)
 w(172873:172887) = u(27)*w(11525:11539)
 w(172888:172897) = u(28)*w(11530:11539)
 w(172898:172902) = u(29)*w(11535:11539)
 w(172903:172909) = u(30)*w(2586:2592)
 w(172910:172916) = u(31)*w(2586:2592)
 w(172917:172923) = u(32)*w(2586:2592)
 w(172924:172930) = u(33)*w(2586:2592)
 w(172931:172937) = u(34)*w(2586:2592)
endif
! terms of degree 8
if (8.le.mxd) then
 w(172938:299297) = u(0)*w(46578:172937)
 w(299298:390619) = u(1)*w(81616:172937)
 w(390620:455850) = u(2)*w(107707:172937)
 w(455851:501855) = u(3)*w(126933:172937)
 w(501856:533842) = u(4)*w(140951:172937)
 w(533843:555712) = u(5)*w(151068:172937)
 w(555713:570344) = u(6)*w(158306:172937)
 w(570345:579824) = u(7)*w(163458:172937)
 w(579825:585626) = u(8)*w(167136:172937)
 w(585627:588752) = u(9)*w(169812:172937)
 w(588753:590450) = u(10)*w(44880:46577)
 w(590451:591931) = u(11)*w(45097:46577)
 w(591932:593215) = u(12)*w(45294:46577)
 w(593216:594321) = u(13)*w(45472:46577)
 w(594322:595267) = u(14)*w(45632:46577)
 w(595268:596070) = u(15)*w(45775:46577)
 w(596071:596746) = u(16)*w(45902:46577)
 w(596747:597310) = u(17)*w(46014:46577)
 w(597311:597776) = u(18)*w(46112:46577)
 w(597777:598157) = u(19)*w(46197:46577)
 w(598158:598465) = u(20)*w(46270:46577)
 w(598466:598711) = u(21)*w(46332:46577)
 w(598712:598905) = u(22)*w(46384:46577)
 w(598906:599056) = u(23)*w(46427:46577)
 w(599057:599172) = u(24)*w(46462:46577)
 w(599173:599260) = u(25)*w(46490:46577)
 w(599261:599326) = u(26)*w(46512:46577)
 w(599327:599375) = u(27)*w(46529:46577)
 w(599376:599411) = u(28)*w(46542:46577)
 w(599412:599437) = u(29)*w(46552:46577)
 w(599438:599444) = u(35)*w(2586:2592)
 w(599445:599450) = u(36)*w(2587:2592)
 w(599451:599455) = u(37)*w(2588:2592)
 w(599456:599459) = u(38)*w(2589:2592)
 w(599460:599462) = u(39)*w(2590:2592)
 w(599463:599464) = u(40)*w(2591:2592)
 w(599465) = u(41)*w(2592)
endif
! terms of degree 9
if (9.le.mxd) then
 w(599466:1025993) = u(0)*w(172938:599465)
 w(1025994:1326161) = u(1)*w(299298:599465)
 w(1326162:1535007) = u(2)*w(390620:599465)
 w(1535008:1678622) = u(3)*w(455851:599465)
 w(1678623:1776232) = u(4)*w(501856:599465)
 w(1776233:1841855) = u(5)*w(533843:599465)
 w(1841856:1885608) = u(6)*w(555713:599465)
 w(1885609:1914729) = u(7)*w(570345:599465)
 w(1914730:1934370) = u(8)*w(579825:599465)
 w(1934371:1948209) = u(9)*w(585627:599465)
 w(1948210:1949294) = u(10)*w(171853:172937)
 w(1949295:1950279) = u(11)*w(171953:172937)
 w(1950280:1951169) = u(12)*w(172048:172937)
 w(1951170:1951969) = u(13)*w(172138:172937)
 w(1951970:1952684) = u(14)*w(172223:172937)
 w(1952685:1953319) = u(15)*w(172303:172937)
 w(1953320:1953879) = u(16)*w(172378:172937)
 w(1953880:1954369) = u(17)*w(172448:172937)
 w(1954370:1954794) = u(18)*w(172513:172937)
 w(1954795:1955159) = u(19)*w(172573:172937)
 w(1955160:1955469) = u(20)*w(172628:172937)
 w(1955470:1955729) = u(21)*w(172678:172937)
 w(1955730:1955944) = u(22)*w(172723:172937)
 w(1955945:1956119) = u(23)*w(172763:172937)
 w(1956120:1956259) = u(24)*w(172798:172937)
 w(1956260:1956369) = u(25)*w(172828:172937)
 w(1956370:1956454) = u(26)*w(172853:172937)
 w(1956455:1956519) = u(27)*w(172873:172937)
 w(1956520:1956569) = u(28)*w(172888:172937)
 w(1956570:1956609) = u(29)*w(172898:172937)
 w(1956610:1956627) = u(30)*w(46560:46577)
 w(1956628:1956640) = u(31)*w(46565:46577)
 w(1956641:1956649) = u(32)*w(46569:46577)
 w(1956650:1956655) = u(33)*w(46572:46577)
 w(1956656:1956659) = u(34)*w(46574:46577)
endif
END SUBROUTINE mg4222_prib
