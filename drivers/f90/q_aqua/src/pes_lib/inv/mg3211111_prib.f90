SUBROUTINE mg3211111_prib (mxd, u, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: u(0:)
real (kind=wp), intent (out) :: w(0:mg3211111_npb(mxd)-1)
!-----------------------------------------------------------------------
if (size(u).ne.nr) then
 stop 'mg3211111_prib: bad size u'
!! else if (size(w).ne.mg3211111_npb(mxd)) then
!!  stop 'mg3211111_prib: bad size w'
endif
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 23 14 7 0 0 1 0 0 0
! prib,  dnpb(0:*): 1 23 290 2629 19080 117375 633963 3078480 13669800 56215574
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
 w(13) = u(12)*w(0)
 w(14) = u(13)*w(0)
 w(15) = u(14)*w(0)
 w(16) = u(15)*w(0)
 w(17) = u(16)*w(0)
 w(18) = u(17)*w(0)
 w(19) = u(18)*w(0)
 w(20) = u(19)*w(0)
 w(21) = u(20)*w(0)
 w(22) = u(21)*w(0)
 w(23) = u(22)*w(0)
endif
! terms of degree 2
if (2.le.mxd) then
 w(24:46) = u(0)*w(1:23)
 w(47:68) = u(1)*w(2:23)
 w(69:89) = u(2)*w(3:23)
 w(90:109) = u(3)*w(4:23)
 w(110:128) = u(4)*w(5:23)
 w(129:146) = u(5)*w(6:23)
 w(147:163) = u(6)*w(7:23)
 w(164:179) = u(7)*w(8:23)
 w(180:194) = u(8)*w(9:23)
 w(195:208) = u(9)*w(10:23)
 w(209:221) = u(10)*w(11:23)
 w(222:233) = u(11)*w(12:23)
 w(234:244) = u(12)*w(13:23)
 w(245:254) = u(13)*w(14:23)
 w(255:263) = u(14)*w(15:23)
 w(264:271) = u(15)*w(16:23)
 w(272:278) = u(16)*w(17:23)
 w(279:284) = u(17)*w(18:23)
 w(285:289) = u(18)*w(19:23)
 w(290:293) = u(19)*w(20:23)
 w(294:296) = u(20)*w(21:23)
 w(297:298) = u(21)*w(22:23)
 w(299) = u(22)*w(23)
 w(300) = u(23)*w(0)
 w(301) = u(24)*w(0)
 w(302) = u(25)*w(0)
 w(303) = u(26)*w(0)
 w(304) = u(27)*w(0)
 w(305) = u(28)*w(0)
 w(306) = u(29)*w(0)
 w(307) = u(30)*w(0)
 w(308) = u(31)*w(0)
 w(309) = u(32)*w(0)
 w(310) = u(33)*w(0)
 w(311) = u(34)*w(0)
 w(312) = u(35)*w(0)
 w(313) = u(36)*w(0)
endif
! terms of degree 3
if (3.le.mxd) then
 w(314:603) = u(0)*w(24:313)
 w(604:870) = u(1)*w(47:313)
 w(871:1115) = u(2)*w(69:313)
 w(1116:1339) = u(3)*w(90:313)
 w(1340:1543) = u(4)*w(110:313)
 w(1544:1728) = u(5)*w(129:313)
 w(1729:1895) = u(6)*w(147:313)
 w(1896:2045) = u(7)*w(164:313)
 w(2046:2179) = u(8)*w(180:313)
 w(2180:2298) = u(9)*w(195:313)
 w(2299:2403) = u(10)*w(209:313)
 w(2404:2495) = u(11)*w(222:313)
 w(2496:2575) = u(12)*w(234:313)
 w(2576:2644) = u(13)*w(245:313)
 w(2645:2703) = u(14)*w(255:313)
 w(2704:2753) = u(15)*w(264:313)
 w(2754:2795) = u(16)*w(272:313)
 w(2796:2830) = u(17)*w(279:313)
 w(2831:2859) = u(18)*w(285:313)
 w(2860:2883) = u(19)*w(290:313)
 w(2884:2903) = u(20)*w(294:313)
 w(2904:2920) = u(21)*w(297:313)
 w(2921:2935) = u(22)*w(299:313)
 w(2936) = u(37)*w(0)
 w(2937) = u(38)*w(0)
 w(2938) = u(39)*w(0)
 w(2939) = u(40)*w(0)
 w(2940) = u(41)*w(0)
 w(2941) = u(42)*w(0)
 w(2942) = u(43)*w(0)
endif
! terms of degree 4
if (4.le.mxd) then
 w(2943:5571) = u(0)*w(314:2942)
 w(5572:7910) = u(1)*w(604:2942)
 w(7911:9982) = u(2)*w(871:2942)
 w(9983:11809) = u(3)*w(1116:2942)
 w(11810:13412) = u(4)*w(1340:2942)
 w(13413:14811) = u(5)*w(1544:2942)
 w(14812:16025) = u(6)*w(1729:2942)
 w(16026:17072) = u(7)*w(1896:2942)
 w(17073:17969) = u(8)*w(2046:2942)
 w(17970:18732) = u(9)*w(2180:2942)
 w(18733:19376) = u(10)*w(2299:2942)
 w(19377:19915) = u(11)*w(2404:2942)
 w(19916:20362) = u(12)*w(2496:2942)
 w(20363:20729) = u(13)*w(2576:2942)
 w(20730:21027) = u(14)*w(2645:2942)
 w(21028:21266) = u(15)*w(2704:2942)
 w(21267:21455) = u(16)*w(2754:2942)
 w(21456:21602) = u(17)*w(2796:2942)
 w(21603:21714) = u(18)*w(2831:2942)
 w(21715:21797) = u(19)*w(2860:2942)
 w(21798:21856) = u(20)*w(2884:2942)
 w(21857:21895) = u(21)*w(2904:2942)
 w(21896:21917) = u(22)*w(2921:2942)
 w(21918:21931) = u(23)*w(300:313)
 w(21932:21944) = u(24)*w(301:313)
 w(21945:21956) = u(25)*w(302:313)
 w(21957:21967) = u(26)*w(303:313)
 w(21968:21977) = u(27)*w(304:313)
 w(21978:21986) = u(28)*w(305:313)
 w(21987:21994) = u(29)*w(306:313)
 w(21995:22001) = u(30)*w(307:313)
 w(22002:22007) = u(31)*w(308:313)
 w(22008:22012) = u(32)*w(309:313)
 w(22013:22016) = u(33)*w(310:313)
 w(22017:22019) = u(34)*w(311:313)
 w(22020:22021) = u(35)*w(312:313)
 w(22022) = u(36)*w(313)
endif
! terms of degree 5
if (5.le.mxd) then
 w(22023:41102) = u(0)*w(2943:22022)
 w(41103:57553) = u(1)*w(5572:22022)
 w(57554:71665) = u(2)*w(7911:22022)
 w(71666:83705) = u(3)*w(9983:22022)
 w(83706:93918) = u(4)*w(11810:22022)
 w(93919:102528) = u(5)*w(13413:22022)
 w(102529:109739) = u(6)*w(14812:22022)
 w(109740:115736) = u(7)*w(16026:22022)
 w(115737:120686) = u(8)*w(17073:22022)
 w(120687:124739) = u(9)*w(17970:22022)
 w(124740:128029) = u(10)*w(18733:22022)
 w(128030:130675) = u(11)*w(19377:22022)
 w(130676:132782) = u(12)*w(19916:22022)
 w(132783:134442) = u(13)*w(20363:22022)
 w(134443:135735) = u(14)*w(20730:22022)
 w(135736:136730) = u(15)*w(21028:22022)
 w(136731:137486) = u(16)*w(21267:22022)
 w(137487:138053) = u(17)*w(21456:22022)
 w(138054:138473) = u(18)*w(21603:22022)
 w(138474:138781) = u(19)*w(21715:22022)
 w(138782:139006) = u(20)*w(21798:22022)
 w(139007:139172) = u(21)*w(21857:22022)
 w(139173:139299) = u(22)*w(21896:22022)
 w(139300:139306) = u(23)*w(2936:2942)
 w(139307:139313) = u(24)*w(2936:2942)
 w(139314:139320) = u(25)*w(2936:2942)
 w(139321:139327) = u(26)*w(2936:2942)
 w(139328:139334) = u(27)*w(2936:2942)
 w(139335:139341) = u(28)*w(2936:2942)
 w(139342:139348) = u(29)*w(2936:2942)
 w(139349:139355) = u(30)*w(2936:2942)
 w(139356:139362) = u(31)*w(2936:2942)
 w(139363:139369) = u(32)*w(2936:2942)
 w(139370:139376) = u(33)*w(2936:2942)
 w(139377:139383) = u(34)*w(2936:2942)
 w(139384:139390) = u(35)*w(2936:2942)
 w(139391:139397) = u(36)*w(2936:2942)
endif
! terms of degree 6
if (6.le.mxd) then
 w(139398:256772) = u(0)*w(22023:139397)
 w(256773:355067) = u(1)*w(41103:139397)
 w(355068:436911) = u(2)*w(57554:139397)
 w(436912:504643) = u(3)*w(71666:139397)
 w(504644:560335) = u(4)*w(83706:139397)
 w(560336:605814) = u(5)*w(93919:139397)
 w(605815:642683) = u(6)*w(102529:139397)
 w(642684:672341) = u(7)*w(109740:139397)
 w(672342:696002) = u(8)*w(115737:139397)
 w(696003:714713) = u(9)*w(120687:139397)
 w(714714:729371) = u(10)*w(124740:139397)
 w(729372:740739) = u(11)*w(128030:139397)
 w(740740:749461) = u(12)*w(130676:139397)
 w(749462:756076) = u(13)*w(132783:139397)
 w(756077:761031) = u(14)*w(134443:139397)
 w(761032:764693) = u(15)*w(135736:139397)
 w(764694:767360) = u(16)*w(136731:139397)
 w(767361:769271) = u(17)*w(137487:139397)
 w(769272:770615) = u(18)*w(138054:139397)
 w(770616:771539) = u(19)*w(138474:139397)
 w(771540:772155) = u(20)*w(138782:139397)
 w(772156:772546) = u(21)*w(139007:139397)
 w(772547:772771) = u(22)*w(139173:139397)
 w(772772:772876) = u(23)*w(21918:22022)
 w(772877:772967) = u(24)*w(21932:22022)
 w(772968:773045) = u(25)*w(21945:22022)
 w(773046:773111) = u(26)*w(21957:22022)
 w(773112:773166) = u(27)*w(21968:22022)
 w(773167:773211) = u(28)*w(21978:22022)
 w(773212:773247) = u(29)*w(21987:22022)
 w(773248:773275) = u(30)*w(21995:22022)
 w(773276:773296) = u(31)*w(22002:22022)
 w(773297:773311) = u(32)*w(22008:22022)
 w(773312:773321) = u(33)*w(22013:22022)
 w(773322:773327) = u(34)*w(22017:22022)
 w(773328:773330) = u(35)*w(22020:22022)
 w(773331) = u(36)*w(22022)
 w(773332:773338) = u(37)*w(2936:2942)
 w(773339:773344) = u(38)*w(2937:2942)
 w(773345:773349) = u(39)*w(2938:2942)
 w(773350:773353) = u(40)*w(2939:2942)
 w(773354:773356) = u(41)*w(2940:2942)
 w(773357:773358) = u(42)*w(2941:2942)
 w(773359) = u(43)*w(2942)
 w(773360) = u(44)*w(0)
endif
! terms of degree 7
if (7.le.mxd) then
 w(773361:1407323) = u(0)*w(139398:773360)
 w(1407324:1923911) = u(1)*w(256773:773360)
 w(1923912:2342204) = u(2)*w(355068:773360)
 w(2342205:2678653) = u(3)*w(436912:773360)
 w(2678654:2947370) = u(4)*w(504644:773360)
 w(2947371:3160395) = u(5)*w(560336:773360)
 w(3160396:3327941) = u(6)*w(605815:773360)
 w(3327942:3458618) = u(7)*w(642684:773360)
 w(3458619:3559637) = u(8)*w(672342:773360)
 w(3559638:3636995) = u(9)*w(696003:773360)
 w(3636996:3695642) = u(10)*w(714714:773360)
 w(3695643:3739631) = u(11)*w(729372:773360)
 w(3739632:3772252) = u(12)*w(740740:773360)
 w(3772253:3796151) = u(13)*w(749462:773360)
 w(3796152:3813435) = u(14)*w(756077:773360)
 w(3813436:3825764) = u(15)*w(761032:773360)
 w(3825765:3834431) = u(16)*w(764694:773360)
 w(3834432:3840431) = u(17)*w(767361:773360)
 w(3840432:3844520) = u(18)*w(769272:773360)
 w(3844521:3847265) = u(19)*w(770616:773360)
 w(3847266:3849086) = u(20)*w(771540:773360)
 w(3849087:3850291) = u(21)*w(772156:773360)
 w(3850292:3851105) = u(22)*w(772547:773360)
 w(3851106:3851203) = u(23)*w(139300:139397)
 w(3851204:3851294) = u(24)*w(139307:139397)
 w(3851295:3851378) = u(25)*w(139314:139397)
 w(3851379:3851455) = u(26)*w(139321:139397)
 w(3851456:3851525) = u(27)*w(139328:139397)
 w(3851526:3851588) = u(28)*w(139335:139397)
 w(3851589:3851644) = u(29)*w(139342:139397)
 w(3851645:3851693) = u(30)*w(139349:139397)
 w(3851694:3851735) = u(31)*w(139356:139397)
 w(3851736:3851770) = u(32)*w(139363:139397)
 w(3851771:3851798) = u(33)*w(139370:139397)
 w(3851799:3851819) = u(34)*w(139377:139397)
 w(3851820:3851833) = u(35)*w(139384:139397)
 w(3851834:3851840) = u(36)*w(139391:139397)
endif
! terms of degree 8
if (8.le.mxd) then
 w(3851841:6930320) = u(0)*w(773361:3851840)
 w(6930321:9374837) = u(1)*w(1407324:3851840)
 w(9374838:11302766) = u(2)*w(1923912:3851840)
 w(11302767:12812402) = u(3)*w(2342205:3851840)
 w(12812403:13985589) = u(4)*w(2678654:3851840)
 w(13985590:14890059) = u(5)*w(2947371:3851840)
 w(14890060:15581504) = u(6)*w(3160396:3851840)
 w(15581505:16105403) = u(7)*w(3327942:3851840)
 w(16105404:16498625) = u(8)*w(3458619:3851840)
 w(16498626:16790828) = u(9)*w(3559638:3851840)
 w(16790829:17005673) = u(10)*w(3636996:3851840)
 w(17005674:17161871) = u(11)*w(3695643:3851840)
 w(17161872:17274080) = u(12)*w(3739632:3851840)
 w(17274081:17353668) = u(13)*w(3772253:3851840)
 w(17353669:17409357) = u(14)*w(3796152:3851840)
 w(17409358:17447762) = u(15)*w(3813436:3851840)
 w(17447763:17473838) = u(16)*w(3825765:3851840)
 w(17473839:17491247) = u(17)*w(3834432:3851840)
 w(17491248:17502656) = u(18)*w(3840432:3851840)
 w(17502657:17509976) = u(19)*w(3844521:3851840)
 w(17509977:17514551) = u(20)*w(3847266:3851840)
 w(17514552:17517305) = u(21)*w(3849087:3851840)
 w(17517306:17518854) = u(22)*w(3850292:3851840)
 w(17518855:17519443) = u(23)*w(772772:773360)
 w(17519444:17519927) = u(24)*w(772877:773360)
 w(17519928:17520320) = u(25)*w(772968:773360)
 w(17520321:17520635) = u(26)*w(773046:773360)
 w(17520636:17520884) = u(27)*w(773112:773360)
 w(17520885:17521078) = u(28)*w(773167:773360)
 w(17521079:17521227) = u(29)*w(773212:773360)
 w(17521228:17521340) = u(30)*w(773248:773360)
 w(17521341:17521425) = u(31)*w(773276:773360)
 w(17521426:17521489) = u(32)*w(773297:773360)
 w(17521490:17521538) = u(33)*w(773312:773360)
 w(17521539:17521577) = u(34)*w(773322:773360)
 w(17521578:17521610) = u(35)*w(773328:773360)
 w(17521611:17521640) = u(36)*w(773331:773360)
endif
! terms of degree 9
if (9.le.mxd) then
 w(17521641:31191440) = u(0)*w(3851841:17521640)
 w(31191441:41782760) = u(1)*w(6930321:17521640)
 w(41782761:49929563) = u(2)*w(9374838:17521640)
 w(49929564:56148437) = u(3)*w(11302767:17521640)
 w(56148438:60857675) = u(4)*w(12812403:17521640)
 w(60857676:64393726) = u(5)*w(13985590:17521640)
 w(64393727:67025307) = u(6)*w(14890060:17521640)
 w(67025308:68965443) = u(7)*w(15581505:17521640)
 w(68965444:70381680) = u(8)*w(16105404:17521640)
 w(70381681:71404695) = u(9)*w(16498626:17521640)
 w(71404696:72135507) = u(10)*w(16790829:17521640)
 w(72135508:72651474) = u(11)*w(17005674:17521640)
 w(72651475:73011243) = u(12)*w(17161872:17521640)
 w(73011244:73258803) = u(13)*w(17274081:17521640)
 w(73258804:73426775) = u(14)*w(17353669:17521640)
 w(73426776:73539058) = u(15)*w(17409358:17521640)
 w(73539059:73612936) = u(16)*w(17447763:17521640)
 w(73612937:73660738) = u(17)*w(17473839:17521640)
 w(73660739:73691131) = u(18)*w(17491248:17521640)
 w(73691132:73710115) = u(19)*w(17502657:17521640)
 w(73710116:73721779) = u(20)*w(17509977:17521640)
 w(73721780:73728868) = u(21)*w(17514552:17521640)
 w(73728869:73733203) = u(22)*w(17517306:17521640)
 w(73733204:73733938) = u(23)*w(3851106:3851840)
 w(73733939:73734575) = u(24)*w(3851204:3851840)
 w(73734576:73735121) = u(25)*w(3851295:3851840)
 w(73735122:73735583) = u(26)*w(3851379:3851840)
 w(73735584:73735968) = u(27)*w(3851456:3851840)
 w(73735969:73736283) = u(28)*w(3851526:3851840)
 w(73736284:73736535) = u(29)*w(3851589:3851840)
 w(73736536:73736731) = u(30)*w(3851645:3851840)
 w(73736732:73736878) = u(31)*w(3851694:3851840)
 w(73736879:73736983) = u(32)*w(3851736:3851840)
 w(73736984:73737053) = u(33)*w(3851771:3851840)
 w(73737054:73737095) = u(34)*w(3851799:3851840)
 w(73737096:73737116) = u(35)*w(3851820:3851840)
 w(73737117:73737123) = u(36)*w(3851834:3851840)
 w(73737124:73737152) = u(37)*w(773332:773360)
 w(73737153:73737174) = u(38)*w(773339:773360)
 w(73737175:73737190) = u(39)*w(773345:773360)
 w(73737191:73737201) = u(40)*w(773350:773360)
 w(73737202:73737208) = u(41)*w(773354:773360)
 w(73737209:73737212) = u(42)*w(773357:773360)
 w(73737213:73737214) = u(43)*w(773359:773360)
endif
END SUBROUTINE mg3211111_prib
