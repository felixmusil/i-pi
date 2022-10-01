SUBROUTINE mg4111111_prib (mxd, u, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: u(0:)
real (kind=wp), intent (out) :: w(0:mg4111111_npb(mxd)-1)
!-----------------------------------------------------------------------
if (size(u).ne.nr) then
 stop 'mg4111111_prib: bad size u'
!! else if (size(w).ne.mg4111111_npb(mxd)) then
!!  stop 'mg4111111_prib: bad size w'
endif
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 22 8 8 7 0 0 0 0 0
! prib,  dnpb(0:*): 1 22 261 2208 14893 85006 425901 1919712 7922053 30323094
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
endif
! terms of degree 2
if (2.le.mxd) then
 w(23:44) = u(0)*w(1:22)
 w(45:65) = u(1)*w(2:22)
 w(66:85) = u(2)*w(3:22)
 w(86:104) = u(3)*w(4:22)
 w(105:122) = u(4)*w(5:22)
 w(123:139) = u(5)*w(6:22)
 w(140:155) = u(6)*w(7:22)
 w(156:170) = u(7)*w(8:22)
 w(171:184) = u(8)*w(9:22)
 w(185:197) = u(9)*w(10:22)
 w(198:209) = u(10)*w(11:22)
 w(210:220) = u(11)*w(12:22)
 w(221:230) = u(12)*w(13:22)
 w(231:239) = u(13)*w(14:22)
 w(240:247) = u(14)*w(15:22)
 w(248:254) = u(15)*w(16:22)
 w(255:260) = u(16)*w(17:22)
 w(261:265) = u(17)*w(18:22)
 w(266:269) = u(18)*w(19:22)
 w(270:272) = u(19)*w(20:22)
 w(273:274) = u(20)*w(21:22)
 w(275) = u(21)*w(22)
 w(276) = u(22)*w(0)
 w(277) = u(23)*w(0)
 w(278) = u(24)*w(0)
 w(279) = u(25)*w(0)
 w(280) = u(26)*w(0)
 w(281) = u(27)*w(0)
 w(282) = u(28)*w(0)
 w(283) = u(29)*w(0)
endif
! terms of degree 3
if (3.le.mxd) then
 w(284:544) = u(0)*w(23:283)
 w(545:783) = u(1)*w(45:283)
 w(784:1001) = u(2)*w(66:283)
 w(1002:1199) = u(3)*w(86:283)
 w(1200:1378) = u(4)*w(105:283)
 w(1379:1539) = u(5)*w(123:283)
 w(1540:1683) = u(6)*w(140:283)
 w(1684:1811) = u(7)*w(156:283)
 w(1812:1924) = u(8)*w(171:283)
 w(1925:2023) = u(9)*w(185:283)
 w(2024:2109) = u(10)*w(198:283)
 w(2110:2183) = u(11)*w(210:283)
 w(2184:2246) = u(12)*w(221:283)
 w(2247:2299) = u(13)*w(231:283)
 w(2300:2343) = u(14)*w(240:283)
 w(2344:2379) = u(15)*w(248:283)
 w(2380:2408) = u(16)*w(255:283)
 w(2409:2431) = u(17)*w(261:283)
 w(2432:2449) = u(18)*w(266:283)
 w(2450:2463) = u(19)*w(270:283)
 w(2464:2474) = u(20)*w(273:283)
 w(2475:2483) = u(21)*w(275:283)
 w(2484) = u(30)*w(0)
 w(2485) = u(31)*w(0)
 w(2486) = u(32)*w(0)
 w(2487) = u(33)*w(0)
 w(2488) = u(34)*w(0)
 w(2489) = u(35)*w(0)
 w(2490) = u(36)*w(0)
 w(2491) = u(37)*w(0)
endif
! terms of degree 4
if (4.le.mxd) then
 w(2492:4699) = u(0)*w(284:2491)
 w(4700:6646) = u(1)*w(545:2491)
 w(6647:8354) = u(2)*w(784:2491)
 w(8355:9844) = u(3)*w(1002:2491)
 w(9845:11136) = u(4)*w(1200:2491)
 w(11137:12249) = u(5)*w(1379:2491)
 w(12250:13201) = u(6)*w(1540:2491)
 w(13202:14009) = u(7)*w(1684:2491)
 w(14010:14689) = u(8)*w(1812:2491)
 w(14690:15256) = u(9)*w(1925:2491)
 w(15257:15724) = u(10)*w(2024:2491)
 w(15725:16106) = u(11)*w(2110:2491)
 w(16107:16414) = u(12)*w(2184:2491)
 w(16415:16659) = u(13)*w(2247:2491)
 w(16660:16851) = u(14)*w(2300:2491)
 w(16852:16999) = u(15)*w(2344:2491)
 w(17000:17111) = u(16)*w(2380:2491)
 w(17112:17194) = u(17)*w(2409:2491)
 w(17195:17254) = u(18)*w(2432:2491)
 w(17255:17296) = u(19)*w(2450:2491)
 w(17297:17324) = u(20)*w(2464:2491)
 w(17325:17341) = u(21)*w(2475:2491)
 w(17342:17349) = u(22)*w(276:283)
 w(17350:17356) = u(23)*w(277:283)
 w(17357:17362) = u(24)*w(278:283)
 w(17363:17367) = u(25)*w(279:283)
 w(17368:17371) = u(26)*w(280:283)
 w(17372:17374) = u(27)*w(281:283)
 w(17375:17376) = u(28)*w(282:283)
 w(17377) = u(29)*w(283)
 w(17378) = u(38)*w(0)
 w(17379) = u(39)*w(0)
 w(17380) = u(40)*w(0)
 w(17381) = u(41)*w(0)
 w(17382) = u(42)*w(0)
 w(17383) = u(43)*w(0)
 w(17384) = u(44)*w(0)
endif
! terms of degree 5
if (5.le.mxd) then
 w(17385:32277) = u(0)*w(2492:17384)
 w(32278:44962) = u(1)*w(4700:17384)
 w(44963:55700) = u(2)*w(6647:17384)
 w(55701:64730) = u(3)*w(8355:17384)
 w(64731:72270) = u(4)*w(9845:17384)
 w(72271:78518) = u(5)*w(11137:17384)
 w(78519:83653) = u(6)*w(12250:17384)
 w(83654:87836) = u(7)*w(13202:17384)
 w(87837:91211) = u(8)*w(14010:17384)
 w(91212:93906) = u(9)*w(14690:17384)
 w(93907:96034) = u(10)*w(15257:17384)
 w(96035:97694) = u(11)*w(15725:17384)
 w(97695:98972) = u(12)*w(16107:17384)
 w(98973:99942) = u(13)*w(16415:17384)
 w(99943:100667) = u(14)*w(16660:17384)
 w(100668:101200) = u(15)*w(16852:17384)
 w(101201:101585) = u(16)*w(17000:17384)
 w(101586:101858) = u(17)*w(17112:17384)
 w(101859:102048) = u(18)*w(17195:17384)
 w(102049:102178) = u(19)*w(17255:17384)
 w(102179:102266) = u(20)*w(17297:17384)
 w(102267:102326) = u(21)*w(17325:17384)
 w(102327:102334) = u(22)*w(2484:2491)
 w(102335:102342) = u(23)*w(2484:2491)
 w(102343:102350) = u(24)*w(2484:2491)
 w(102351:102358) = u(25)*w(2484:2491)
 w(102359:102366) = u(26)*w(2484:2491)
 w(102367:102374) = u(27)*w(2484:2491)
 w(102375:102382) = u(28)*w(2484:2491)
 w(102383:102390) = u(29)*w(2484:2491)
endif
! terms of degree 6
if (6.le.mxd) then
 w(102391:187396) = u(0)*w(17385:102390)
 w(187397:257509) = u(1)*w(32278:102390)
 w(257510:314937) = u(2)*w(44963:102390)
 w(314938:361627) = u(3)*w(55701:102390)
 w(361628:399287) = u(4)*w(64731:102390)
 w(399288:429407) = u(5)*w(72271:102390)
 w(429408:453279) = u(6)*w(78519:102390)
 w(453280:472016) = u(7)*w(83654:102390)
 w(472017:486570) = u(8)*w(87837:102390)
 w(486571:497749) = u(9)*w(91212:102390)
 w(497750:506233) = u(10)*w(93907:102390)
 w(506234:512589) = u(11)*w(96035:102390)
 w(512590:517285) = u(12)*w(97695:102390)
 w(517286:520703) = u(13)*w(98973:102390)
 w(520704:523151) = u(14)*w(99943:102390)
 w(523152:524874) = u(15)*w(100668:102390)
 w(524875:526064) = u(16)*w(101201:102390)
 w(526065:526869) = u(17)*w(101586:102390)
 w(526870:527401) = u(18)*w(101859:102390)
 w(527402:527743) = u(19)*w(102049:102390)
 w(527744:527955) = u(20)*w(102179:102390)
 w(527956:528079) = u(21)*w(102267:102390)
 w(528080:528122) = u(22)*w(17342:17384)
 w(528123:528157) = u(23)*w(17350:17384)
 w(528158:528185) = u(24)*w(17357:17384)
 w(528186:528207) = u(25)*w(17363:17384)
 w(528208:528224) = u(26)*w(17368:17384)
 w(528225:528237) = u(27)*w(17372:17384)
 w(528238:528247) = u(28)*w(17375:17384)
 w(528248:528255) = u(29)*w(17377:17384)
 w(528256:528263) = u(30)*w(2484:2491)
 w(528264:528270) = u(31)*w(2485:2491)
 w(528271:528276) = u(32)*w(2486:2491)
 w(528277:528281) = u(33)*w(2487:2491)
 w(528282:528285) = u(34)*w(2488:2491)
 w(528286:528288) = u(35)*w(2489:2491)
 w(528289:528290) = u(36)*w(2490:2491)
 w(528291) = u(37)*w(2491)
endif
! terms of degree 7
if (7.le.mxd) then
 w(528292:954192) = u(0)*w(102391:528291)
 w(954193:1295087) = u(1)*w(187397:528291)
 w(1295088:1565869) = u(2)*w(257510:528291)
 w(1565870:1779223) = u(3)*w(314938:528291)
 w(1779224:1945887) = u(4)*w(361628:528291)
 w(1945888:2074891) = u(5)*w(399288:528291)
 w(2074892:2173775) = u(6)*w(429408:528291)
 w(2173776:2248787) = u(7)*w(453280:528291)
 w(2248788:2305062) = u(8)*w(472017:528291)
 w(2305063:2346783) = u(9)*w(486571:528291)
 w(2346784:2377325) = u(10)*w(497750:528291)
 w(2377326:2399383) = u(11)*w(506234:528291)
 w(2399384:2415085) = u(12)*w(512590:528291)
 w(2415086:2426091) = u(13)*w(517286:528291)
 w(2426092:2433679) = u(14)*w(520704:528291)
 w(2433680:2438819) = u(15)*w(523152:528291)
 w(2438820:2442236) = u(16)*w(524875:528291)
 w(2442237:2444463) = u(17)*w(526065:528291)
 w(2444464:2445885) = u(18)*w(526870:528291)
 w(2445886:2446775) = u(19)*w(527402:528291)
 w(2446776:2447323) = u(20)*w(527744:528291)
 w(2447324:2447659) = u(21)*w(527956:528291)
 w(2447660:2447723) = u(22)*w(102327:102390)
 w(2447724:2447779) = u(23)*w(102335:102390)
 w(2447780:2447827) = u(24)*w(102343:102390)
 w(2447828:2447867) = u(25)*w(102351:102390)
 w(2447868:2447899) = u(26)*w(102359:102390)
 w(2447900:2447923) = u(27)*w(102367:102390)
 w(2447924:2447939) = u(28)*w(102375:102390)
 w(2447940:2447947) = u(29)*w(102383:102390)
 w(2447948:2447954) = u(30)*w(17378:17384)
 w(2447955:2447961) = u(31)*w(17378:17384)
 w(2447962:2447968) = u(32)*w(17378:17384)
 w(2447969:2447975) = u(33)*w(17378:17384)
 w(2447976:2447982) = u(34)*w(17378:17384)
 w(2447983:2447989) = u(35)*w(17378:17384)
 w(2447990:2447996) = u(36)*w(17378:17384)
 w(2447997:2448003) = u(37)*w(17378:17384)
endif
! terms of degree 8
if (8.le.mxd) then
 w(2448004:4367715) = u(0)*w(528292:2448003)
 w(4367716:5861526) = u(1)*w(954193:2448003)
 w(5861527:7014442) = u(2)*w(1295088:2448003)
 w(7014443:7896576) = u(3)*w(1565870:2448003)
 w(7896577:8565356) = u(4)*w(1779224:2448003)
 w(8565357:9067472) = u(5)*w(1945888:2448003)
 w(9067473:9440584) = u(6)*w(2074892:2448003)
 w(9440585:9714812) = u(7)*w(2173776:2448003)
 w(9714813:9914028) = u(8)*w(2248788:2448003)
 w(9914029:10056969) = u(9)*w(2305063:2448003)
 w(10056970:10158189) = u(10)*w(2346784:2448003)
 w(10158190:10228867) = u(11)*w(2377326:2448003)
 w(10228868:10277487) = u(12)*w(2399384:2448003)
 w(10277488:10310405) = u(13)*w(2415086:2448003)
 w(10310406:10332317) = u(14)*w(2426092:2448003)
 w(10332318:10346641) = u(15)*w(2433680:2448003)
 w(10346642:10355825) = u(16)*w(2438820:2448003)
 w(10355826:10361592) = u(17)*w(2442237:2448003)
 w(10361593:10365132) = u(18)*w(2444464:2448003)
 w(10365133:10367250) = u(19)*w(2445886:2448003)
 w(10367251:10368478) = u(20)*w(2446776:2448003)
 w(10368479:10369158) = u(21)*w(2447324:2448003)
 w(10369159:10369370) = u(22)*w(528080:528291)
 w(10369371:10369539) = u(23)*w(528123:528291)
 w(10369540:10369673) = u(24)*w(528158:528291)
 w(10369674:10369779) = u(25)*w(528186:528291)
 w(10369780:10369863) = u(26)*w(528208:528291)
 w(10369864:10369930) = u(27)*w(528225:528291)
 w(10369931:10369984) = u(28)*w(528238:528291)
 w(10369985:10370028) = u(29)*w(528248:528291)
 w(10370029:10370035) = u(38)*w(17378:17384)
 w(10370036:10370041) = u(39)*w(17379:17384)
 w(10370042:10370046) = u(40)*w(17380:17384)
 w(10370047:10370050) = u(41)*w(17381:17384)
 w(10370051:10370053) = u(42)*w(17382:17384)
 w(10370054:10370055) = u(43)*w(17383:17384)
 w(10370056) = u(44)*w(17384)
endif
! terms of degree 9
if (9.le.mxd) then
 w(10370057:18292109) = u(0)*w(2448004:10370056)
 w(18292110:24294450) = u(1)*w(4367716:10370056)
 w(24294451:28802980) = u(2)*w(5861527:10370056)
 w(28802981:32158594) = u(3)*w(7014443:10370056)
 w(32158595:34632074) = u(4)*w(7896577:10370056)
 w(34632075:36436774) = u(5)*w(8565357:10370056)
 w(36436775:37739358) = u(6)*w(9067473:10370056)
 w(37739359:38668830) = u(7)*w(9440585:10370056)
 w(38668831:39324074) = u(8)*w(9714813:10370056)
 w(39324075:39780102) = u(9)*w(9914029:10370056)
 w(39780103:40093189) = u(10)*w(10056970:10370056)
 w(40093190:40305056) = u(11)*w(10158190:10370056)
 w(40305057:40446245) = u(12)*w(10228868:10370056)
 w(40446246:40538814) = u(13)*w(10277488:10370056)
 w(40538815:40598465) = u(14)*w(10310406:10370056)
 w(40598466:40636204) = u(15)*w(10332318:10370056)
 w(40636205:40659619) = u(16)*w(10346642:10370056)
 w(40659620:40673850) = u(17)*w(10355826:10370056)
 w(40673851:40682314) = u(18)*w(10361593:10370056)
 w(40682315:40687238) = u(19)*w(10365133:10370056)
 w(40687239:40690044) = u(20)*w(10367251:10370056)
 w(40690045:40691622) = u(21)*w(10368479:10370056)
 w(40691623:40691966) = u(22)*w(2447660:2448003)
 w(40691967:40692246) = u(23)*w(2447724:2448003)
 w(40692247:40692470) = u(24)*w(2447780:2448003)
 w(40692471:40692646) = u(25)*w(2447828:2448003)
 w(40692647:40692782) = u(26)*w(2447868:2448003)
 w(40692783:40692886) = u(27)*w(2447900:2448003)
 w(40692887:40692966) = u(28)*w(2447924:2448003)
 w(40692967:40693030) = u(29)*w(2447940:2448003)
 w(40693031:40693066) = u(30)*w(528256:528291)
 w(40693067:40693094) = u(31)*w(528264:528291)
 w(40693095:40693115) = u(32)*w(528271:528291)
 w(40693116:40693130) = u(33)*w(528277:528291)
 w(40693131:40693140) = u(34)*w(528282:528291)
 w(40693141:40693146) = u(35)*w(528286:528291)
 w(40693147:40693149) = u(36)*w(528289:528291)
 w(40693150) = u(37)*w(528291)
endif
END SUBROUTINE mg4111111_prib
