SUBROUTINE mg522_prib (mxd, u, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: u(0:)
real (kind=wp), intent (out) :: w(0:mg522_npb(mxd)-1)
!-----------------------------------------------------------------------
if (size(u).ne.nr) then
 stop 'mg522_prib: bad size u'
!! else if (size(w).ne.mg522_npb(mxd)) then
!!  stop 'mg522_prib: bad size w'
endif
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 6 11 4 6 4 3 0 0 0
! prib,  dnpb(0:*): 1 6 32 126 453 1432 4237 11630 30371 75356
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
endif
! terms of degree 3
if (3.le.mxd) then
 w(39:70) = u(0)*w(7:38)
 w(71:96) = u(1)*w(13:38)
 w(97:117) = u(2)*w(18:38)
 w(118:134) = u(3)*w(22:38)
 w(135:148) = u(4)*w(25:38)
 w(149:160) = u(5)*w(27:38)
 w(161) = u(17)*w(0)
 w(162) = u(18)*w(0)
 w(163) = u(19)*w(0)
 w(164) = u(20)*w(0)
endif
! terms of degree 4
if (4.le.mxd) then
 w(165:290) = u(0)*w(39:164)
 w(291:384) = u(1)*w(71:164)
 w(385:452) = u(2)*w(97:164)
 w(453:499) = u(3)*w(118:164)
 w(500:529) = u(4)*w(135:164)
 w(530:545) = u(5)*w(149:164)
 w(546:556) = u(6)*w(28:38)
 w(557:566) = u(7)*w(29:38)
 w(567:575) = u(8)*w(30:38)
 w(576:583) = u(9)*w(31:38)
 w(584:590) = u(10)*w(32:38)
 w(591:596) = u(11)*w(33:38)
 w(597:601) = u(12)*w(34:38)
 w(602:605) = u(13)*w(35:38)
 w(606:608) = u(14)*w(36:38)
 w(609:610) = u(15)*w(37:38)
 w(611) = u(16)*w(38)
 w(612) = u(21)*w(0)
 w(613) = u(22)*w(0)
 w(614) = u(23)*w(0)
 w(615) = u(24)*w(0)
 w(616) = u(25)*w(0)
 w(617) = u(26)*w(0)
endif
! terms of degree 5
if (5.le.mxd) then
 w(618:1070) = u(0)*w(165:617)
 w(1071:1397) = u(1)*w(291:617)
 w(1398:1630) = u(2)*w(385:617)
 w(1631:1795) = u(3)*w(453:617)
 w(1796:1913) = u(4)*w(500:617)
 w(1914:2001) = u(5)*w(530:617)
 w(2002:2005) = u(6)*w(161:164)
 w(2006:2009) = u(7)*w(161:164)
 w(2010:2013) = u(8)*w(161:164)
 w(2014:2017) = u(9)*w(161:164)
 w(2018:2021) = u(10)*w(161:164)
 w(2022:2025) = u(11)*w(161:164)
 w(2026:2029) = u(12)*w(161:164)
 w(2030:2033) = u(13)*w(161:164)
 w(2034:2037) = u(14)*w(161:164)
 w(2038:2041) = u(15)*w(161:164)
 w(2042:2045) = u(16)*w(161:164)
 w(2046) = u(27)*w(0)
 w(2047) = u(28)*w(0)
 w(2048) = u(29)*w(0)
 w(2049) = u(30)*w(0)
endif
! terms of degree 6
if (6.le.mxd) then
 w(2050:3481) = u(0)*w(618:2049)
 w(3482:4460) = u(1)*w(1071:2049)
 w(4461:5112) = u(2)*w(1398:2049)
 w(5113:5531) = u(3)*w(1631:2049)
 w(5532:5785) = u(4)*w(1796:2049)
 w(5786:5921) = u(5)*w(1914:2049)
 w(5922:5993) = u(6)*w(546:617)
 w(5994:6054) = u(7)*w(557:617)
 w(6055:6105) = u(8)*w(567:617)
 w(6106:6147) = u(9)*w(576:617)
 w(6148:6181) = u(10)*w(584:617)
 w(6182:6208) = u(11)*w(591:617)
 w(6209:6229) = u(12)*w(597:617)
 w(6230:6245) = u(13)*w(602:617)
 w(6246:6257) = u(14)*w(606:617)
 w(6258:6266) = u(15)*w(609:617)
 w(6267:6273) = u(16)*w(611:617)
 w(6274:6277) = u(17)*w(161:164)
 w(6278:6280) = u(18)*w(162:164)
 w(6281:6282) = u(19)*w(163:164)
 w(6283) = u(20)*w(164)
 w(6284) = u(31)*w(0)
 w(6285) = u(32)*w(0)
 w(6286) = u(33)*w(0)
endif
! terms of degree 7
if (7.le.mxd) then
 w(6287:10523) = u(0)*w(2050:6286)
 w(10524:13328) = u(1)*w(3482:6286)
 w(13329:15154) = u(2)*w(4461:6286)
 w(15155:16328) = u(3)*w(5113:6286)
 w(16329:17083) = u(4)*w(5532:6286)
 w(17084:17584) = u(5)*w(5786:6286)
 w(17585:17632) = u(6)*w(2002:2049)
 w(17633:17676) = u(7)*w(2006:2049)
 w(17677:17716) = u(8)*w(2010:2049)
 w(17717:17752) = u(9)*w(2014:2049)
 w(17753:17784) = u(10)*w(2018:2049)
 w(17785:17812) = u(11)*w(2022:2049)
 w(17813:17836) = u(12)*w(2026:2049)
 w(17837:17856) = u(13)*w(2030:2049)
 w(17857:17872) = u(14)*w(2034:2049)
 w(17873:17884) = u(15)*w(2038:2049)
 w(17885:17892) = u(16)*w(2042:2049)
 w(17893:17898) = u(17)*w(612:617)
 w(17899:17904) = u(18)*w(612:617)
 w(17905:17910) = u(19)*w(612:617)
 w(17911:17916) = u(20)*w(612:617)
endif
! terms of degree 8
if (8.le.mxd) then
 w(17917:29546) = u(0)*w(6287:17916)
 w(29547:36939) = u(1)*w(10524:17916)
 w(36940:41527) = u(2)*w(13329:17916)
 w(41528:44289) = u(3)*w(15155:17916)
 w(44290:45877) = u(4)*w(16329:17916)
 w(45878:46710) = u(5)*w(17084:17916)
 w(46711:47075) = u(6)*w(5922:6286)
 w(47076:47368) = u(7)*w(5994:6286)
 w(47369:47600) = u(8)*w(6055:6286)
 w(47601:47781) = u(9)*w(6106:6286)
 w(47782:47920) = u(10)*w(6148:6286)
 w(47921:48025) = u(11)*w(6182:6286)
 w(48026:48103) = u(12)*w(6209:6286)
 w(48104:48160) = u(13)*w(6230:6286)
 w(48161:48201) = u(14)*w(6246:6286)
 w(48202:48230) = u(15)*w(6258:6286)
 w(48231:48250) = u(16)*w(6267:6286)
 w(48251:48254) = u(17)*w(2046:2049)
 w(48255:48258) = u(18)*w(2046:2049)
 w(48259:48262) = u(19)*w(2046:2049)
 w(48263:48266) = u(20)*w(2046:2049)
 w(48267:48272) = u(21)*w(612:617)
 w(48273:48277) = u(22)*w(613:617)
 w(48278:48281) = u(23)*w(614:617)
 w(48282:48284) = u(24)*w(615:617)
 w(48285:48286) = u(25)*w(616:617)
 w(48287) = u(26)*w(617)
endif
! terms of degree 9
if (9.le.mxd) then
 w(48288:78658) = u(0)*w(17917:48287)
 w(78659:97399) = u(1)*w(29547:48287)
 w(97400:108747) = u(2)*w(36940:48287)
 w(108748:115507) = u(3)*w(41528:48287)
 w(115508:119505) = u(4)*w(44290:48287)
 w(119506:121915) = u(5)*w(45878:48287)
 w(121916:122247) = u(6)*w(17585:17916)
 w(122248:122531) = u(7)*w(17633:17916)
 w(122532:122771) = u(8)*w(17677:17916)
 w(122772:122971) = u(9)*w(17717:17916)
 w(122972:123135) = u(10)*w(17753:17916)
 w(123136:123267) = u(11)*w(17785:17916)
 w(123268:123371) = u(12)*w(17813:17916)
 w(123372:123451) = u(13)*w(17837:17916)
 w(123452:123511) = u(14)*w(17857:17916)
 w(123512:123555) = u(15)*w(17873:17916)
 w(123556:123587) = u(16)*w(17885:17916)
 w(123588:123600) = u(17)*w(6274:6286)
 w(123601:123609) = u(18)*w(6278:6286)
 w(123610:123615) = u(19)*w(6281:6286)
 w(123616:123619) = u(20)*w(6283:6286)
 w(123620:123623) = u(21)*w(2046:2049)
 w(123624:123627) = u(22)*w(2046:2049)
 w(123628:123631) = u(23)*w(2046:2049)
 w(123632:123635) = u(24)*w(2046:2049)
 w(123636:123639) = u(25)*w(2046:2049)
 w(123640:123643) = u(26)*w(2046:2049)
endif
END SUBROUTINE mg522_prib