SUBROUTINE mg433_prib (mxd, u, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: u(0:)
real (kind=wp), intent (out) :: w(0:mg433_npb(mxd)-1)
!-----------------------------------------------------------------------
if (size(u).ne.nr) then
 stop 'mg433_prib: bad size u'
!! else if (size(w).ne.mg433_npb(mxd)) then
!!  stop 'mg433_prib: bad size w'
endif
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 6 13 13 8 0 3 0 0 0
! prib,  dnpb(0:*): 1 6 34 147 576 2016 6574 20004 57748 158738
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
endif
! terms of degree 2
if (2.le.mxd) then
 w(7:12) = u(0)*w(1:6)
 w(13:17) = u(1)*w(2:6)
 w(18:21) = u(2)*w(3:6)
 w(22:24) = u(3)*w(4:6)
 w(25:26) = u(4)*w(5:6)
 w(27) = u(5)*w(6)
 w(28) = u(6)*w(0)
 w(29) = u(7)*w(0)
 w(30) = u(8)*w(0)
 w(31) = u(9)*w(0)
 w(32) = u(10)*w(0)
 w(33) = u(11)*w(0)
 w(34) = u(12)*w(0)
 w(35) = u(13)*w(0)
 w(36) = u(14)*w(0)
 w(37) = u(15)*w(0)
 w(38) = u(16)*w(0)
 w(39) = u(17)*w(0)
 w(40) = u(18)*w(0)
endif
! terms of degree 3
if (3.le.mxd) then
 w(41:74) = u(0)*w(7:40)
 w(75:102) = u(1)*w(13:40)
 w(103:125) = u(2)*w(18:40)
 w(126:144) = u(3)*w(22:40)
 w(145:160) = u(4)*w(25:40)
 w(161:174) = u(5)*w(27:40)
 w(175) = u(19)*w(0)
 w(176) = u(20)*w(0)
 w(177) = u(21)*w(0)
 w(178) = u(22)*w(0)
 w(179) = u(23)*w(0)
 w(180) = u(24)*w(0)
 w(181) = u(25)*w(0)
 w(182) = u(26)*w(0)
 w(183) = u(27)*w(0)
 w(184) = u(28)*w(0)
 w(185) = u(29)*w(0)
 w(186) = u(30)*w(0)
 w(187) = u(31)*w(0)
endif
! terms of degree 4
if (4.le.mxd) then
 w(188:334) = u(0)*w(41:187)
 w(335:447) = u(1)*w(75:187)
 w(448:532) = u(2)*w(103:187)
 w(533:594) = u(3)*w(126:187)
 w(595:637) = u(4)*w(145:187)
 w(638:664) = u(5)*w(161:187)
 w(665:677) = u(6)*w(28:40)
 w(678:689) = u(7)*w(29:40)
 w(690:700) = u(8)*w(30:40)
 w(701:710) = u(9)*w(31:40)
 w(711:719) = u(10)*w(32:40)
 w(720:727) = u(11)*w(33:40)
 w(728:734) = u(12)*w(34:40)
 w(735:740) = u(13)*w(35:40)
 w(741:745) = u(14)*w(36:40)
 w(746:749) = u(15)*w(37:40)
 w(750:752) = u(16)*w(38:40)
 w(753:754) = u(17)*w(39:40)
 w(755) = u(18)*w(40)
 w(756) = u(32)*w(0)
 w(757) = u(33)*w(0)
 w(758) = u(34)*w(0)
 w(759) = u(35)*w(0)
 w(760) = u(36)*w(0)
 w(761) = u(37)*w(0)
 w(762) = u(38)*w(0)
 w(763) = u(39)*w(0)
endif
! terms of degree 5
if (5.le.mxd) then
 w(764:1339) = u(0)*w(188:763)
 w(1340:1768) = u(1)*w(335:763)
 w(1769:2084) = u(2)*w(448:763)
 w(2085:2315) = u(3)*w(533:763)
 w(2316:2484) = u(4)*w(595:763)
 w(2485:2610) = u(5)*w(638:763)
 w(2611:2623) = u(6)*w(175:187)
 w(2624:2636) = u(7)*w(175:187)
 w(2637:2649) = u(8)*w(175:187)
 w(2650:2662) = u(9)*w(175:187)
 w(2663:2675) = u(10)*w(175:187)
 w(2676:2688) = u(11)*w(175:187)
 w(2689:2701) = u(12)*w(175:187)
 w(2702:2714) = u(13)*w(175:187)
 w(2715:2727) = u(14)*w(175:187)
 w(2728:2740) = u(15)*w(175:187)
 w(2741:2753) = u(16)*w(175:187)
 w(2754:2766) = u(17)*w(175:187)
 w(2767:2779) = u(18)*w(175:187)
endif
! terms of degree 6
if (6.le.mxd) then
 w(2780:4795) = u(0)*w(764:2779)
 w(4796:6235) = u(1)*w(1340:2779)
 w(6236:7246) = u(2)*w(1769:2779)
 w(7247:7941) = u(3)*w(2085:2779)
 w(7942:8405) = u(4)*w(2316:2779)
 w(8406:8700) = u(5)*w(2485:2779)
 w(8701:8799) = u(6)*w(665:763)
 w(8800:8885) = u(7)*w(678:763)
 w(8886:8959) = u(8)*w(690:763)
 w(8960:9022) = u(9)*w(701:763)
 w(9023:9075) = u(10)*w(711:763)
 w(9076:9119) = u(11)*w(720:763)
 w(9120:9155) = u(12)*w(728:763)
 w(9156:9184) = u(13)*w(735:763)
 w(9185:9207) = u(14)*w(741:763)
 w(9208:9225) = u(15)*w(746:763)
 w(9226:9239) = u(16)*w(750:763)
 w(9240:9250) = u(17)*w(753:763)
 w(9251:9259) = u(18)*w(755:763)
 w(9260:9272) = u(19)*w(175:187)
 w(9273:9284) = u(20)*w(176:187)
 w(9285:9295) = u(21)*w(177:187)
 w(9296:9305) = u(22)*w(178:187)
 w(9306:9314) = u(23)*w(179:187)
 w(9315:9322) = u(24)*w(180:187)
 w(9323:9329) = u(25)*w(181:187)
 w(9330:9335) = u(26)*w(182:187)
 w(9336:9340) = u(27)*w(183:187)
 w(9341:9344) = u(28)*w(184:187)
 w(9345:9347) = u(29)*w(185:187)
 w(9348:9349) = u(30)*w(186:187)
 w(9350) = u(31)*w(187)
 w(9351) = u(40)*w(0)
 w(9352) = u(41)*w(0)
 w(9353) = u(42)*w(0)
endif
! terms of degree 7
if (7.le.mxd) then
 w(9354:15927) = u(0)*w(2780:9353)
 w(15928:20485) = u(1)*w(4796:9353)
 w(20486:23603) = u(2)*w(6236:9353)
 w(23604:25710) = u(3)*w(7247:9353)
 w(25711:27122) = u(4)*w(7942:9353)
 w(27123:28070) = u(5)*w(8406:9353)
 w(28071:28239) = u(6)*w(2611:2779)
 w(28240:28395) = u(7)*w(2624:2779)
 w(28396:28538) = u(8)*w(2637:2779)
 w(28539:28668) = u(9)*w(2650:2779)
 w(28669:28785) = u(10)*w(2663:2779)
 w(28786:28889) = u(11)*w(2676:2779)
 w(28890:28980) = u(12)*w(2689:2779)
 w(28981:29058) = u(13)*w(2702:2779)
 w(29059:29123) = u(14)*w(2715:2779)
 w(29124:29175) = u(15)*w(2728:2779)
 w(29176:29214) = u(16)*w(2741:2779)
 w(29215:29240) = u(17)*w(2754:2779)
 w(29241:29253) = u(18)*w(2767:2779)
 w(29254:29261) = u(19)*w(756:763)
 w(29262:29269) = u(20)*w(756:763)
 w(29270:29277) = u(21)*w(756:763)
 w(29278:29285) = u(22)*w(756:763)
 w(29286:29293) = u(23)*w(756:763)
 w(29294:29301) = u(24)*w(756:763)
 w(29302:29309) = u(25)*w(756:763)
 w(29310:29317) = u(26)*w(756:763)
 w(29318:29325) = u(27)*w(756:763)
 w(29326:29333) = u(28)*w(756:763)
 w(29334:29341) = u(29)*w(756:763)
 w(29342:29349) = u(30)*w(756:763)
 w(29350:29357) = u(31)*w(756:763)
endif
! terms of degree 8
if (8.le.mxd) then
 w(29358:49361) = u(0)*w(9354:29357)
 w(49362:62791) = u(1)*w(15928:29357)
 w(62792:71663) = u(2)*w(20486:29357)
 w(71664:77417) = u(3)*w(23604:29357)
 w(77418:81064) = u(4)*w(25711:29357)
 w(81065:83299) = u(5)*w(27123:29357)
 w(83300:83952) = u(6)*w(8701:9353)
 w(83953:84506) = u(7)*w(8800:9353)
 w(84507:84974) = u(8)*w(8886:9353)
 w(84975:85368) = u(9)*w(8960:9353)
 w(85369:85699) = u(10)*w(9023:9353)
 w(85700:85977) = u(11)*w(9076:9353)
 w(85978:86211) = u(12)*w(9120:9353)
 w(86212:86409) = u(13)*w(9156:9353)
 w(86410:86578) = u(14)*w(9185:9353)
 w(86579:86724) = u(15)*w(9208:9353)
 w(86725:86852) = u(16)*w(9226:9353)
 w(86853:86966) = u(17)*w(9240:9353)
 w(86967:87069) = u(18)*w(9251:9353)
 w(87070:87077) = u(32)*w(756:763)
 w(87078:87084) = u(33)*w(757:763)
 w(87085:87090) = u(34)*w(758:763)
 w(87091:87095) = u(35)*w(759:763)
 w(87096:87099) = u(36)*w(760:763)
 w(87100:87102) = u(37)*w(761:763)
 w(87103:87104) = u(38)*w(762:763)
 w(87105) = u(39)*w(763)
endif
! terms of degree 9
if (9.le.mxd) then
 w(87106:144853) = u(0)*w(29358:87105)
 w(144854:182597) = u(1)*w(49362:87105)
 w(182598:206911) = u(2)*w(62792:87105)
 w(206912:222353) = u(3)*w(71664:87105)
 w(222354:232041) = u(4)*w(77418:87105)
 w(232042:238082) = u(5)*w(81065:87105)
 w(238083:239369) = u(6)*w(28071:29357)
 w(239370:240487) = u(7)*w(28240:29357)
 w(240488:241449) = u(8)*w(28396:29357)
 w(241450:242268) = u(9)*w(28539:29357)
 w(242269:242957) = u(10)*w(28669:29357)
 w(242958:243529) = u(11)*w(28786:29357)
 w(243530:243997) = u(12)*w(28890:29357)
 w(243998:244374) = u(13)*w(28981:29357)
 w(244375:244673) = u(14)*w(29059:29357)
 w(244674:244907) = u(15)*w(29124:29357)
 w(244908:245089) = u(16)*w(29176:29357)
 w(245090:245232) = u(17)*w(29215:29357)
 w(245233:245349) = u(18)*w(29241:29357)
 w(245350:245443) = u(19)*w(9260:9353)
 w(245444:245524) = u(20)*w(9273:9353)
 w(245525:245593) = u(21)*w(9285:9353)
 w(245594:245651) = u(22)*w(9296:9353)
 w(245652:245699) = u(23)*w(9306:9353)
 w(245700:245738) = u(24)*w(9315:9353)
 w(245739:245769) = u(25)*w(9323:9353)
 w(245770:245793) = u(26)*w(9330:9353)
 w(245794:245811) = u(27)*w(9336:9353)
 w(245812:245824) = u(28)*w(9341:9353)
 w(245825:245833) = u(29)*w(9345:9353)
 w(245834:245839) = u(30)*w(9348:9353)
 w(245840:245843) = u(31)*w(9350:9353)
endif
END SUBROUTINE mg433_prib