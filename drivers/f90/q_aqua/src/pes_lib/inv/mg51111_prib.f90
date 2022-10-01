SUBROUTINE mg51111_prib (mxd, u, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: u(0:)
real (kind=wp), intent (out) :: w(0:mg51111_npb(mxd)-1)
!-----------------------------------------------------------------------
if (size(u).ne.nr) then
 stop 'mg51111_prib: bad size u'
!! else if (size(w).ne.mg51111_npb(mxd)) then
!!  stop 'mg51111_prib: bad size w'
endif
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 11 6 6 6 6 1 0 0 0
! prib,  dnpb(0:*): 1 11 72 358 1490 5454 18088 55418 159006 431536
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
endif
! terms of degree 2
if (2.le.mxd) then
 w(12:22) = u(0)*w(1:11)
 w(23:32) = u(1)*w(2:11)
 w(33:41) = u(2)*w(3:11)
 w(42:49) = u(3)*w(4:11)
 w(50:56) = u(4)*w(5:11)
 w(57:62) = u(5)*w(6:11)
 w(63:67) = u(6)*w(7:11)
 w(68:71) = u(7)*w(8:11)
 w(72:74) = u(8)*w(9:11)
 w(75:76) = u(9)*w(10:11)
 w(77) = u(10)*w(11)
 w(78) = u(11)*w(0)
 w(79) = u(12)*w(0)
 w(80) = u(13)*w(0)
 w(81) = u(14)*w(0)
 w(82) = u(15)*w(0)
 w(83) = u(16)*w(0)
endif
! terms of degree 3
if (3.le.mxd) then
 w(84:155) = u(0)*w(12:83)
 w(156:216) = u(1)*w(23:83)
 w(217:267) = u(2)*w(33:83)
 w(268:309) = u(3)*w(42:83)
 w(310:343) = u(4)*w(50:83)
 w(344:370) = u(5)*w(57:83)
 w(371:391) = u(6)*w(63:83)
 w(392:407) = u(7)*w(68:83)
 w(408:419) = u(8)*w(72:83)
 w(420:428) = u(9)*w(75:83)
 w(429:435) = u(10)*w(77:83)
 w(436) = u(17)*w(0)
 w(437) = u(18)*w(0)
 w(438) = u(19)*w(0)
 w(439) = u(20)*w(0)
 w(440) = u(21)*w(0)
 w(441) = u(22)*w(0)
endif
! terms of degree 4
if (4.le.mxd) then
 w(442:799) = u(0)*w(84:441)
 w(800:1085) = u(1)*w(156:441)
 w(1086:1310) = u(2)*w(217:441)
 w(1311:1484) = u(3)*w(268:441)
 w(1485:1616) = u(4)*w(310:441)
 w(1617:1714) = u(5)*w(344:441)
 w(1715:1785) = u(6)*w(371:441)
 w(1786:1835) = u(7)*w(392:441)
 w(1836:1869) = u(8)*w(408:441)
 w(1870:1891) = u(9)*w(420:441)
 w(1892:1904) = u(10)*w(429:441)
 w(1905:1910) = u(11)*w(78:83)
 w(1911:1915) = u(12)*w(79:83)
 w(1916:1919) = u(13)*w(80:83)
 w(1920:1922) = u(14)*w(81:83)
 w(1923:1924) = u(15)*w(82:83)
 w(1925) = u(16)*w(83)
 w(1926) = u(23)*w(0)
 w(1927) = u(24)*w(0)
 w(1928) = u(25)*w(0)
 w(1929) = u(26)*w(0)
 w(1930) = u(27)*w(0)
 w(1931) = u(28)*w(0)
endif
! terms of degree 5
if (5.le.mxd) then
 w(1932:3421) = u(0)*w(442:1931)
 w(3422:4553) = u(1)*w(800:1931)
 w(4554:5399) = u(2)*w(1086:1931)
 w(5400:6020) = u(3)*w(1311:1931)
 w(6021:6467) = u(4)*w(1485:1931)
 w(6468:6782) = u(5)*w(1617:1931)
 w(6783:6999) = u(6)*w(1715:1931)
 w(7000:7145) = u(7)*w(1786:1931)
 w(7146:7241) = u(8)*w(1836:1931)
 w(7242:7303) = u(9)*w(1870:1931)
 w(7304:7343) = u(10)*w(1892:1931)
 w(7344:7349) = u(11)*w(436:441)
 w(7350:7355) = u(12)*w(436:441)
 w(7356:7361) = u(13)*w(436:441)
 w(7362:7367) = u(14)*w(436:441)
 w(7368:7373) = u(15)*w(436:441)
 w(7374:7379) = u(16)*w(436:441)
 w(7380) = u(29)*w(0)
 w(7381) = u(30)*w(0)
 w(7382) = u(31)*w(0)
 w(7383) = u(32)*w(0)
 w(7384) = u(33)*w(0)
 w(7385) = u(34)*w(0)
endif
! terms of degree 6
if (6.le.mxd) then
 w(7386:12839) = u(0)*w(1932:7385)
 w(12840:16803) = u(1)*w(3422:7385)
 w(16804:19635) = u(2)*w(4554:7385)
 w(19636:21621) = u(3)*w(5400:7385)
 w(21622:22986) = u(4)*w(6021:7385)
 w(22987:23904) = u(5)*w(6468:7385)
 w(23905:24507) = u(6)*w(6783:7385)
 w(24508:24893) = u(7)*w(7000:7385)
 w(24894:25133) = u(8)*w(7146:7385)
 w(25134:25277) = u(9)*w(7242:7385)
 w(25278:25359) = u(10)*w(7304:7385)
 w(25360:25386) = u(11)*w(1905:1931)
 w(25387:25407) = u(12)*w(1911:1931)
 w(25408:25423) = u(13)*w(1916:1931)
 w(25424:25435) = u(14)*w(1920:1931)
 w(25436:25444) = u(15)*w(1923:1931)
 w(25445:25451) = u(16)*w(1925:1931)
 w(25452:25457) = u(17)*w(436:441)
 w(25458:25462) = u(18)*w(437:441)
 w(25463:25466) = u(19)*w(438:441)
 w(25467:25469) = u(20)*w(439:441)
 w(25470:25471) = u(21)*w(440:441)
 w(25472) = u(22)*w(441)
 w(25473) = u(35)*w(0)
endif
! terms of degree 7
if (7.le.mxd) then
 w(25474:43561) = u(0)*w(7386:25473)
 w(43562:56195) = u(1)*w(12840:25473)
 w(56196:64865) = u(2)*w(16804:25473)
 w(64866:70703) = u(3)*w(19636:25473)
 w(70704:74555) = u(4)*w(21622:25473)
 w(74556:77042) = u(5)*w(22987:25473)
 w(77043:78611) = u(6)*w(23905:25473)
 w(78612:79577) = u(7)*w(24508:25473)
 w(79578:80157) = u(8)*w(24894:25473)
 w(80158:80497) = u(9)*w(25134:25473)
 w(80498:80693) = u(10)*w(25278:25473)
 w(80694:80735) = u(11)*w(7344:7385)
 w(80736:80771) = u(12)*w(7350:7385)
 w(80772:80801) = u(13)*w(7356:7385)
 w(80802:80825) = u(14)*w(7362:7385)
 w(80826:80843) = u(15)*w(7368:7385)
 w(80844:80855) = u(16)*w(7374:7385)
 w(80856:80861) = u(17)*w(1926:1931)
 w(80862:80867) = u(18)*w(1926:1931)
 w(80868:80873) = u(19)*w(1926:1931)
 w(80874:80879) = u(20)*w(1926:1931)
 w(80880:80885) = u(21)*w(1926:1931)
 w(80886:80891) = u(22)*w(1926:1931)
endif
! terms of degree 8
if (8.le.mxd) then
 w(80892:136309) = u(0)*w(25474:80891)
 w(136310:173639) = u(1)*w(43562:80891)
 w(173640:198335) = u(2)*w(56196:80891)
 w(198336:214361) = u(3)*w(64866:80891)
 w(214362:224549) = u(4)*w(70704:80891)
 w(224550:230885) = u(5)*w(74556:80891)
 w(230886:234734) = u(6)*w(77043:80891)
 w(234735:237014) = u(7)*w(78612:80891)
 w(237015:238328) = u(8)*w(79578:80891)
 w(238329:239062) = u(9)*w(80158:80891)
 w(239063:239456) = u(10)*w(80498:80891)
 w(239457:239570) = u(11)*w(25360:25473)
 w(239571:239657) = u(12)*w(25387:25473)
 w(239658:239723) = u(13)*w(25408:25473)
 w(239724:239773) = u(14)*w(25424:25473)
 w(239774:239811) = u(15)*w(25436:25473)
 w(239812:239840) = u(16)*w(25445:25473)
 w(239841:239846) = u(17)*w(7380:7385)
 w(239847:239852) = u(18)*w(7380:7385)
 w(239853:239858) = u(19)*w(7380:7385)
 w(239859:239864) = u(20)*w(7380:7385)
 w(239865:239870) = u(21)*w(7380:7385)
 w(239871:239876) = u(22)*w(7380:7385)
 w(239877:239882) = u(23)*w(1926:1931)
 w(239883:239887) = u(24)*w(1927:1931)
 w(239888:239891) = u(25)*w(1928:1931)
 w(239892:239894) = u(26)*w(1929:1931)
 w(239895:239896) = u(27)*w(1930:1931)
 w(239897) = u(28)*w(1931)
endif
! terms of degree 9
if (9.le.mxd) then
 w(239898:398903) = u(0)*w(80892:239897)
 w(398904:502491) = u(1)*w(136310:239897)
 w(502492:568749) = u(2)*w(173640:239897)
 w(568750:610311) = u(3)*w(198336:239897)
 w(610312:635847) = u(4)*w(214362:239897)
 w(635848:651195) = u(5)*w(224550:239897)
 w(651196:660207) = u(6)*w(230886:239897)
 w(660208:665370) = u(7)*w(234735:239897)
 w(665371:668253) = u(8)*w(237015:239897)
 w(668254:669822) = u(9)*w(238329:239897)
 w(669823:670657) = u(10)*w(239063:239897)
 w(670658:670855) = u(11)*w(80694:80891)
 w(670856:671011) = u(12)*w(80736:80891)
 w(671012:671131) = u(13)*w(80772:80891)
 w(671132:671221) = u(14)*w(80802:80891)
 w(671222:671287) = u(15)*w(80826:80891)
 w(671288:671335) = u(16)*w(80844:80891)
 w(671336:671357) = u(17)*w(25452:25473)
 w(671358:671373) = u(18)*w(25458:25473)
 w(671374:671384) = u(19)*w(25463:25473)
 w(671385:671391) = u(20)*w(25467:25473)
 w(671392:671395) = u(21)*w(25470:25473)
 w(671396:671397) = u(22)*w(25472:25473)
 w(671398:671403) = u(23)*w(7380:7385)
 w(671404:671409) = u(24)*w(7380:7385)
 w(671410:671415) = u(25)*w(7380:7385)
 w(671416:671421) = u(26)*w(7380:7385)
 w(671422:671427) = u(27)*w(7380:7385)
 w(671428:671433) = u(28)*w(7380:7385)
endif
END SUBROUTINE mg51111_prib