SUBROUTINE mg21111111_prib (mxd, u, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: u(0:)
real (kind=wp), intent (out) :: w(0:mg21111111_npb(mxd)-1)
!-----------------------------------------------------------------------
if (size(u).ne.nr) then
 stop 'mg21111111_prib: bad size u'
!! else if (size(w).ne.mg21111111_npb(mxd)) then
!!  stop 'mg21111111_prib: bad size w'
endif
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 29 7 0 0 0 0 0 0 0
! prib,  dnpb(0:*): 1 29 442 4698 39033 269613 1608888 8514168 40718298 178504338
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
 w(24) = u(23)*w(0)
 w(25) = u(24)*w(0)
 w(26) = u(25)*w(0)
 w(27) = u(26)*w(0)
 w(28) = u(27)*w(0)
 w(29) = u(28)*w(0)
endif
! terms of degree 2
if (2.le.mxd) then
 w(30:58) = u(0)*w(1:29)
 w(59:86) = u(1)*w(2:29)
 w(87:113) = u(2)*w(3:29)
 w(114:139) = u(3)*w(4:29)
 w(140:164) = u(4)*w(5:29)
 w(165:188) = u(5)*w(6:29)
 w(189:211) = u(6)*w(7:29)
 w(212:233) = u(7)*w(8:29)
 w(234:254) = u(8)*w(9:29)
 w(255:274) = u(9)*w(10:29)
 w(275:293) = u(10)*w(11:29)
 w(294:311) = u(11)*w(12:29)
 w(312:328) = u(12)*w(13:29)
 w(329:344) = u(13)*w(14:29)
 w(345:359) = u(14)*w(15:29)
 w(360:373) = u(15)*w(16:29)
 w(374:386) = u(16)*w(17:29)
 w(387:398) = u(17)*w(18:29)
 w(399:409) = u(18)*w(19:29)
 w(410:419) = u(19)*w(20:29)
 w(420:428) = u(20)*w(21:29)
 w(429:436) = u(21)*w(22:29)
 w(437:443) = u(22)*w(23:29)
 w(444:449) = u(23)*w(24:29)
 w(450:454) = u(24)*w(25:29)
 w(455:458) = u(25)*w(26:29)
 w(459:461) = u(26)*w(27:29)
 w(462:463) = u(27)*w(28:29)
 w(464) = u(28)*w(29)
 w(465) = u(29)*w(0)
 w(466) = u(30)*w(0)
 w(467) = u(31)*w(0)
 w(468) = u(32)*w(0)
 w(469) = u(33)*w(0)
 w(470) = u(34)*w(0)
 w(471) = u(35)*w(0)
endif
! terms of degree 3
if (3.le.mxd) then
 w(472:913) = u(0)*w(30:471)
 w(914:1326) = u(1)*w(59:471)
 w(1327:1711) = u(2)*w(87:471)
 w(1712:2069) = u(3)*w(114:471)
 w(2070:2401) = u(4)*w(140:471)
 w(2402:2708) = u(5)*w(165:471)
 w(2709:2991) = u(6)*w(189:471)
 w(2992:3251) = u(7)*w(212:471)
 w(3252:3489) = u(8)*w(234:471)
 w(3490:3706) = u(9)*w(255:471)
 w(3707:3903) = u(10)*w(275:471)
 w(3904:4081) = u(11)*w(294:471)
 w(4082:4241) = u(12)*w(312:471)
 w(4242:4384) = u(13)*w(329:471)
 w(4385:4511) = u(14)*w(345:471)
 w(4512:4623) = u(15)*w(360:471)
 w(4624:4721) = u(16)*w(374:471)
 w(4722:4806) = u(17)*w(387:471)
 w(4807:4879) = u(18)*w(399:471)
 w(4880:4941) = u(19)*w(410:471)
 w(4942:4993) = u(20)*w(420:471)
 w(4994:5036) = u(21)*w(429:471)
 w(5037:5071) = u(22)*w(437:471)
 w(5072:5099) = u(23)*w(444:471)
 w(5100:5121) = u(24)*w(450:471)
 w(5122:5138) = u(25)*w(455:471)
 w(5139:5151) = u(26)*w(459:471)
 w(5152:5161) = u(27)*w(462:471)
 w(5162:5169) = u(28)*w(464:471)
endif
! terms of degree 4
if (4.le.mxd) then
 w(5170:9867) = u(0)*w(472:5169)
 w(9868:14123) = u(1)*w(914:5169)
 w(14124:17966) = u(2)*w(1327:5169)
 w(17967:21424) = u(3)*w(1712:5169)
 w(21425:24524) = u(4)*w(2070:5169)
 w(24525:27292) = u(5)*w(2402:5169)
 w(27293:29753) = u(6)*w(2709:5169)
 w(29754:31931) = u(7)*w(2992:5169)
 w(31932:33849) = u(8)*w(3252:5169)
 w(33850:35529) = u(9)*w(3490:5169)
 w(35530:36992) = u(10)*w(3707:5169)
 w(36993:38258) = u(11)*w(3904:5169)
 w(38259:39346) = u(12)*w(4082:5169)
 w(39347:40274) = u(13)*w(4242:5169)
 w(40275:41059) = u(14)*w(4385:5169)
 w(41060:41717) = u(15)*w(4512:5169)
 w(41718:42263) = u(16)*w(4624:5169)
 w(42264:42711) = u(17)*w(4722:5169)
 w(42712:43074) = u(18)*w(4807:5169)
 w(43075:43364) = u(19)*w(4880:5169)
 w(43365:43592) = u(20)*w(4942:5169)
 w(43593:43768) = u(21)*w(4994:5169)
 w(43769:43901) = u(22)*w(5037:5169)
 w(43902:43999) = u(23)*w(5072:5169)
 w(44000:44069) = u(24)*w(5100:5169)
 w(44070:44117) = u(25)*w(5122:5169)
 w(44118:44148) = u(26)*w(5139:5169)
 w(44149:44166) = u(27)*w(5152:5169)
 w(44167:44174) = u(28)*w(5162:5169)
 w(44175:44181) = u(29)*w(465:471)
 w(44182:44187) = u(30)*w(466:471)
 w(44188:44192) = u(31)*w(467:471)
 w(44193:44196) = u(32)*w(468:471)
 w(44197:44199) = u(33)*w(469:471)
 w(44200:44201) = u(34)*w(470:471)
 w(44202) = u(35)*w(471)
endif
! terms of degree 5
if (5.le.mxd) then
 w(44203:83235) = u(0)*w(5170:44202)
 w(83236:117570) = u(1)*w(9868:44202)
 w(117571:147649) = u(2)*w(14124:44202)
 w(147650:173885) = u(3)*w(17967:44202)
 w(173886:196663) = u(4)*w(21425:44202)
 w(196664:216341) = u(5)*w(24525:44202)
 w(216342:233251) = u(6)*w(27293:44202)
 w(233252:247700) = u(7)*w(29754:44202)
 w(247701:259971) = u(8)*w(31932:44202)
 w(259972:270324) = u(9)*w(33850:44202)
 w(270325:278997) = u(10)*w(35530:44202)
 w(278998:286207) = u(11)*w(36993:44202)
 w(286208:292151) = u(12)*w(38259:44202)
 w(292152:297007) = u(13)*w(39347:44202)
 w(297008:300935) = u(14)*w(40275:44202)
 w(300936:304078) = u(15)*w(41060:44202)
 w(304079:306563) = u(16)*w(41718:44202)
 w(306564:308502) = u(17)*w(42264:44202)
 w(308503:309993) = u(18)*w(42712:44202)
 w(309994:311121) = u(19)*w(43075:44202)
 w(311122:311959) = u(20)*w(43365:44202)
 w(311960:312569) = u(21)*w(43593:44202)
 w(312570:313003) = u(22)*w(43769:44202)
 w(313004:313304) = u(23)*w(43902:44202)
 w(313305:313507) = u(24)*w(44000:44202)
 w(313508:313640) = u(25)*w(44070:44202)
 w(313641:313725) = u(26)*w(44118:44202)
 w(313726:313779) = u(27)*w(44149:44202)
 w(313780:313815) = u(28)*w(44167:44202)
endif
! terms of degree 6
if (6.le.mxd) then
 w(313816:583428) = u(0)*w(44203:313815)
 w(583429:814008) = u(1)*w(83236:313815)
 w(814009:1010253) = u(2)*w(117571:313815)
 w(1010254:1176419) = u(3)*w(147650:313815)
 w(1176420:1316349) = u(4)*w(173886:313815)
 w(1316350:1433501) = u(5)*w(196664:313815)
 w(1433502:1530975) = u(6)*w(216342:313815)
 w(1530976:1611539) = u(7)*w(233252:313815)
 w(1611540:1677654) = u(8)*w(247701:313815)
 w(1677655:1731498) = u(9)*w(259972:313815)
 w(1731499:1774989) = u(10)*w(270325:313815)
 w(1774990:1809807) = u(11)*w(278998:313815)
 w(1809808:1837415) = u(12)*w(286208:313815)
 w(1837416:1859079) = u(13)*w(292152:313815)
 w(1859080:1875887) = u(14)*w(297008:313815)
 w(1875888:1888767) = u(15)*w(300936:313815)
 w(1888768:1898504) = u(16)*w(304079:313815)
 w(1898505:1905756) = u(17)*w(306564:313815)
 w(1905757:1911069) = u(18)*w(308503:313815)
 w(1911070:1914891) = u(19)*w(309994:313815)
 w(1914892:1917585) = u(20)*w(311122:313815)
 w(1917586:1919441) = u(21)*w(311960:313815)
 w(1919442:1920687) = u(22)*w(312570:313815)
 w(1920688:1921499) = u(23)*w(313004:313815)
 w(1921500:1922010) = u(24)*w(313305:313815)
 w(1922011:1922318) = u(25)*w(313508:313815)
 w(1922319:1922493) = u(26)*w(313641:313815)
 w(1922494:1922583) = u(27)*w(313726:313815)
 w(1922584:1922619) = u(28)*w(313780:313815)
 w(1922620:1922647) = u(29)*w(44175:44202)
 w(1922648:1922668) = u(30)*w(44182:44202)
 w(1922669:1922683) = u(31)*w(44188:44202)
 w(1922684:1922693) = u(32)*w(44193:44202)
 w(1922694:1922699) = u(33)*w(44197:44202)
 w(1922700:1922702) = u(34)*w(44200:44202)
 w(1922703) = u(35)*w(44202)
endif
! terms of degree 7
if (7.le.mxd) then
 w(1922704:3531591) = u(0)*w(313816:1922703)
 w(3531592:4870866) = u(1)*w(583429:1922703)
 w(4870867:5979561) = u(2)*w(814009:1922703)
 w(5979562:6892011) = u(3)*w(1010254:1922703)
 w(6892012:7638295) = u(4)*w(1176420:1922703)
 w(7638296:8244649) = u(5)*w(1316350:1922703)
 w(8244650:8733851) = u(6)*w(1433502:1922703)
 w(8733852:9125579) = u(7)*w(1530976:1922703)
 w(9125580:9436743) = u(8)*w(1611540:1922703)
 w(9436744:9681792) = u(9)*w(1677655:1922703)
 w(9681793:9872997) = u(10)*w(1731499:1922703)
 w(9872998:10020711) = u(11)*w(1774990:1922703)
 w(10020712:10133607) = u(12)*w(1809808:1922703)
 w(10133608:10218895) = u(13)*w(1837416:1922703)
 w(10218896:10282519) = u(14)*w(1859080:1922703)
 w(10282520:10329335) = u(15)*w(1875888:1922703)
 w(10329336:10363271) = u(16)*w(1888768:1922703)
 w(10363272:10387470) = u(17)*w(1898505:1922703)
 w(10387471:10404417) = u(18)*w(1905757:1922703)
 w(10404418:10416051) = u(19)*w(1911070:1922703)
 w(10416052:10423863) = u(20)*w(1914892:1922703)
 w(10423864:10428981) = u(21)*w(1917586:1922703)
 w(10428982:10432243) = u(22)*w(1919442:1922703)
 w(10432244:10434259) = u(23)*w(1920688:1922703)
 w(10434260:10435463) = u(24)*w(1921500:1922703)
 w(10435464:10436156) = u(25)*w(1922011:1922703)
 w(10436157:10436541) = u(26)*w(1922319:1922703)
 w(10436542:10436751) = u(27)*w(1922494:1922703)
 w(10436752:10436871) = u(28)*w(1922584:1922703)
endif
! terms of degree 8
if (8.le.mxd) then
 w(10436872:18951039) = u(0)*w(1922704:10436871)
 w(18951040:25856319) = u(1)*w(3531592:10436871)
 w(25856320:31422324) = u(2)*w(4870867:10436871)
 w(31422325:35879634) = u(3)*w(5979562:10436871)
 w(35879635:39424494) = u(4)*w(6892012:10436871)
 w(39424495:42223070) = u(5)*w(7638296:10436871)
 w(42223071:44415292) = u(6)*w(8244650:10436871)
 w(44415293:46118312) = u(7)*w(8733852:10436871)
 w(46118313:47429604) = u(8)*w(9125580:10436871)
 w(47429605:48429732) = u(9)*w(9436744:10436871)
 w(48429733:49184811) = u(10)*w(9681793:10436871)
 w(49184812:49748685) = u(11)*w(9872998:10436871)
 w(49748686:50164845) = u(12)*w(10020712:10436871)
 w(50164846:50468109) = u(13)*w(10133608:10436871)
 w(50468110:50686085) = u(14)*w(10218896:10436871)
 w(50686086:50840437) = u(15)*w(10282520:10436871)
 w(50840438:50947973) = u(16)*w(10329336:10436871)
 w(50947974:51021573) = u(17)*w(10363272:10436871)
 w(51021574:51070974) = u(18)*w(10387471:10436871)
 w(51070975:51103428) = u(19)*w(10404418:10436871)
 w(51103429:51124248) = u(20)*w(10416052:10436871)
 w(51124249:51137256) = u(21)*w(10423864:10436871)
 w(51137257:51145146) = u(22)*w(10428982:10436871)
 w(51145147:51149774) = u(23)*w(10432244:10436871)
 w(51149775:51152386) = u(24)*w(10434260:10436871)
 w(51152387:51153794) = u(25)*w(10435464:10436871)
 w(51153795:51154509) = u(26)*w(10436157:10436871)
 w(51154510:51154839) = u(27)*w(10436542:10436871)
 w(51154840:51154959) = u(28)*w(10436752:10436871)
 w(51154960:51155043) = u(29)*w(1922620:1922703)
 w(51155044:51155099) = u(30)*w(1922648:1922703)
 w(51155100:51155134) = u(31)*w(1922669:1922703)
 w(51155135:51155154) = u(32)*w(1922684:1922703)
 w(51155155:51155164) = u(33)*w(1922694:1922703)
 w(51155165:51155168) = u(34)*w(1922700:1922703)
 w(51155169) = u(35)*w(1922703)
endif
! terms of degree 9
if (9.le.mxd) then
 w(51155170:91873467) = u(0)*w(10436872:51155169)
 w(91873468:124077597) = u(1)*w(18951040:51155169)
 w(124077598:149376447) = u(2)*w(25856320:51155169)
 w(149376448:169109292) = u(3)*w(31422325:51155169)
 w(169109293:184384827) = u(4)*w(35879635:51155169)
 w(184384828:196115502) = u(5)*w(39424495:51155169)
 w(196115503:205047601) = u(6)*w(42223071:51155169)
 w(205047602:211787478) = u(7)*w(44415293:51155169)
 w(211787479:216824335) = u(8)*w(46118313:51155169)
 w(216824336:220549900) = u(9)*w(47429605:51155169)
 w(220549901:223275337) = u(10)*w(48429733:51155169)
 w(223275338:225245695) = u(11)*w(49184812:51155169)
 w(225245696:226652179) = u(12)*w(49748686:51155169)
 w(226652180:227642503) = u(13)*w(50164846:51155169)
 w(227642504:228329563) = u(14)*w(50468110:51155169)
 w(228329564:228798647) = u(15)*w(50686086:51155169)
 w(228798648:229113379) = u(16)*w(50840438:51155169)
 w(229113380:229320575) = u(17)*w(50947974:51155169)
 w(229320576:229454171) = u(18)*w(51021574:51155169)
 w(229454172:229538366) = u(19)*w(51070975:51155169)
 w(229538367:229590107) = u(20)*w(51103429:51155169)
 w(229590108:229621028) = u(21)*w(51124249:51155169)
 w(229621029:229638941) = u(22)*w(51137257:51155169)
 w(229638942:229648964) = u(23)*w(51145147:51155169)
 w(229648965:229654359) = u(24)*w(51149775:51155169)
 w(229654360:229657142) = u(25)*w(51152387:51155169)
 w(229657143:229658517) = u(26)*w(51153795:51155169)
 w(229658518:229659177) = u(27)*w(51154510:51155169)
 w(229659178:229659507) = u(28)*w(51154840:51155169)
endif
END SUBROUTINE mg21111111_prib