SUBROUTINE mg4321_prib (mxd, u, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: u(0:)
real (kind=wp), intent (out) :: w(0:mg4321_npb(mxd)-1)
!-----------------------------------------------------------------------
if (size(u).ne.nr) then
 stop 'mg4321_prib: bad size u'
!! else if (size(w).ne.mg4321_npb(mxd)) then
!!  stop 'mg4321_prib: bad size w'
endif
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 9 15 10 7 0 3 0 0 0
! prib,  dnpb(0:*): 1 9 60 310 1387 5505 19986 67252 212563 636091
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
endif
! terms of degree 2
if (2.le.mxd) then
 w(10:18) = u(0)*w(1:9)
 w(19:26) = u(1)*w(2:9)
 w(27:33) = u(2)*w(3:9)
 w(34:39) = u(3)*w(4:9)
 w(40:44) = u(4)*w(5:9)
 w(45:48) = u(5)*w(6:9)
 w(49:51) = u(6)*w(7:9)
 w(52:53) = u(7)*w(8:9)
 w(54) = u(8)*w(9)
 w(55) = u(9)*w(0)
 w(56) = u(10)*w(0)
 w(57) = u(11)*w(0)
 w(58) = u(12)*w(0)
 w(59) = u(13)*w(0)
 w(60) = u(14)*w(0)
 w(61) = u(15)*w(0)
 w(62) = u(16)*w(0)
 w(63) = u(17)*w(0)
 w(64) = u(18)*w(0)
 w(65) = u(19)*w(0)
 w(66) = u(20)*w(0)
 w(67) = u(21)*w(0)
 w(68) = u(22)*w(0)
 w(69) = u(23)*w(0)
endif
! terms of degree 3
if (3.le.mxd) then
 w(70:129) = u(0)*w(10:69)
 w(130:180) = u(1)*w(19:69)
 w(181:223) = u(2)*w(27:69)
 w(224:259) = u(3)*w(34:69)
 w(260:289) = u(4)*w(40:69)
 w(290:314) = u(5)*w(45:69)
 w(315:335) = u(6)*w(49:69)
 w(336:353) = u(7)*w(52:69)
 w(354:369) = u(8)*w(54:69)
 w(370) = u(24)*w(0)
 w(371) = u(25)*w(0)
 w(372) = u(26)*w(0)
 w(373) = u(27)*w(0)
 w(374) = u(28)*w(0)
 w(375) = u(29)*w(0)
 w(376) = u(30)*w(0)
 w(377) = u(31)*w(0)
 w(378) = u(32)*w(0)
 w(379) = u(33)*w(0)
endif
! terms of degree 4
if (4.le.mxd) then
 w(380:689) = u(0)*w(70:379)
 w(690:939) = u(1)*w(130:379)
 w(940:1138) = u(2)*w(181:379)
 w(1139:1294) = u(3)*w(224:379)
 w(1295:1414) = u(4)*w(260:379)
 w(1415:1504) = u(5)*w(290:379)
 w(1505:1569) = u(6)*w(315:379)
 w(1570:1613) = u(7)*w(336:379)
 w(1614:1639) = u(8)*w(354:379)
 w(1640:1654) = u(9)*w(55:69)
 w(1655:1668) = u(10)*w(56:69)
 w(1669:1681) = u(11)*w(57:69)
 w(1682:1693) = u(12)*w(58:69)
 w(1694:1704) = u(13)*w(59:69)
 w(1705:1714) = u(14)*w(60:69)
 w(1715:1723) = u(15)*w(61:69)
 w(1724:1731) = u(16)*w(62:69)
 w(1732:1738) = u(17)*w(63:69)
 w(1739:1744) = u(18)*w(64:69)
 w(1745:1749) = u(19)*w(65:69)
 w(1750:1753) = u(20)*w(66:69)
 w(1754:1756) = u(21)*w(67:69)
 w(1757:1758) = u(22)*w(68:69)
 w(1759) = u(23)*w(69)
 w(1760) = u(34)*w(0)
 w(1761) = u(35)*w(0)
 w(1762) = u(36)*w(0)
 w(1763) = u(37)*w(0)
 w(1764) = u(38)*w(0)
 w(1765) = u(39)*w(0)
 w(1766) = u(40)*w(0)
endif
! terms of degree 5
if (5.le.mxd) then
 w(1767:3153) = u(0)*w(380:1766)
 w(3154:4230) = u(1)*w(690:1766)
 w(4231:5057) = u(2)*w(940:1766)
 w(5058:5685) = u(3)*w(1139:1766)
 w(5686:6157) = u(4)*w(1295:1766)
 w(6158:6509) = u(5)*w(1415:1766)
 w(6510:6771) = u(6)*w(1505:1766)
 w(6772:6968) = u(7)*w(1570:1766)
 w(6969:7121) = u(8)*w(1614:1766)
 w(7122:7131) = u(9)*w(370:379)
 w(7132:7141) = u(10)*w(370:379)
 w(7142:7151) = u(11)*w(370:379)
 w(7152:7161) = u(12)*w(370:379)
 w(7162:7171) = u(13)*w(370:379)
 w(7172:7181) = u(14)*w(370:379)
 w(7182:7191) = u(15)*w(370:379)
 w(7192:7201) = u(16)*w(370:379)
 w(7202:7211) = u(17)*w(370:379)
 w(7212:7221) = u(18)*w(370:379)
 w(7222:7231) = u(19)*w(370:379)
 w(7232:7241) = u(20)*w(370:379)
 w(7242:7251) = u(21)*w(370:379)
 w(7252:7261) = u(22)*w(370:379)
 w(7262:7271) = u(23)*w(370:379)
endif
! terms of degree 6
if (6.le.mxd) then
 w(7272:12776) = u(0)*w(1767:7271)
 w(12777:16894) = u(1)*w(3154:7271)
 w(16895:19935) = u(2)*w(4231:7271)
 w(19936:22149) = u(3)*w(5058:7271)
 w(22150:23735) = u(4)*w(5686:7271)
 w(23736:24849) = u(5)*w(6158:7271)
 w(24850:25611) = u(6)*w(6510:7271)
 w(25612:26111) = u(7)*w(6772:7271)
 w(26112:26414) = u(8)*w(6969:7271)
 w(26415:26541) = u(9)*w(1640:1766)
 w(26542:26653) = u(10)*w(1655:1766)
 w(26654:26751) = u(11)*w(1669:1766)
 w(26752:26836) = u(12)*w(1682:1766)
 w(26837:26909) = u(13)*w(1694:1766)
 w(26910:26971) = u(14)*w(1705:1766)
 w(26972:27023) = u(15)*w(1715:1766)
 w(27024:27066) = u(16)*w(1724:1766)
 w(27067:27101) = u(17)*w(1732:1766)
 w(27102:27129) = u(18)*w(1739:1766)
 w(27130:27151) = u(19)*w(1745:1766)
 w(27152:27168) = u(20)*w(1750:1766)
 w(27169:27181) = u(21)*w(1754:1766)
 w(27182:27191) = u(22)*w(1757:1766)
 w(27192:27199) = u(23)*w(1759:1766)
 w(27200:27209) = u(24)*w(370:379)
 w(27210:27218) = u(25)*w(371:379)
 w(27219:27226) = u(26)*w(372:379)
 w(27227:27233) = u(27)*w(373:379)
 w(27234:27239) = u(28)*w(374:379)
 w(27240:27244) = u(29)*w(375:379)
 w(27245:27248) = u(30)*w(376:379)
 w(27249:27251) = u(31)*w(377:379)
 w(27252:27253) = u(32)*w(378:379)
 w(27254) = u(33)*w(379)
 w(27255) = u(41)*w(0)
 w(27256) = u(42)*w(0)
 w(27257) = u(43)*w(0)
endif
! terms of degree 7
if (7.le.mxd) then
 w(27258:47243) = u(0)*w(7272:27257)
 w(47244:61724) = u(1)*w(12777:27257)
 w(61725:72087) = u(2)*w(16895:27257)
 w(72088:79409) = u(3)*w(19936:27257)
 w(79410:84517) = u(4)*w(22150:27257)
 w(84518:88039) = u(5)*w(23736:27257)
 w(88040:90447) = u(6)*w(24850:27257)
 w(90448:92093) = u(7)*w(25612:27257)
 w(92094:93239) = u(8)*w(26112:27257)
 w(93240:93389) = u(9)*w(7122:7271)
 w(93390:93529) = u(10)*w(7132:7271)
 w(93530:93659) = u(11)*w(7142:7271)
 w(93660:93779) = u(12)*w(7152:7271)
 w(93780:93889) = u(13)*w(7162:7271)
 w(93890:93989) = u(14)*w(7172:7271)
 w(93990:94079) = u(15)*w(7182:7271)
 w(94080:94159) = u(16)*w(7192:7271)
 w(94160:94229) = u(17)*w(7202:7271)
 w(94230:94289) = u(18)*w(7212:7271)
 w(94290:94339) = u(19)*w(7222:7271)
 w(94340:94379) = u(20)*w(7232:7271)
 w(94380:94409) = u(21)*w(7242:7271)
 w(94410:94429) = u(22)*w(7252:7271)
 w(94430:94439) = u(23)*w(7262:7271)
 w(94440:94446) = u(24)*w(1760:1766)
 w(94447:94453) = u(25)*w(1760:1766)
 w(94454:94460) = u(26)*w(1760:1766)
 w(94461:94467) = u(27)*w(1760:1766)
 w(94468:94474) = u(28)*w(1760:1766)
 w(94475:94481) = u(29)*w(1760:1766)
 w(94482:94488) = u(30)*w(1760:1766)
 w(94489:94495) = u(31)*w(1760:1766)
 w(94496:94502) = u(32)*w(1760:1766)
 w(94503:94509) = u(33)*w(1760:1766)
endif
! terms of degree 8
if (8.le.mxd) then
 w(94510:161761) = u(0)*w(27258:94509)
 w(161762:209027) = u(1)*w(47244:94509)
 w(209028:241812) = u(2)*w(61725:94509)
 w(241813:264234) = u(3)*w(72088:94509)
 w(264235:279334) = u(4)*w(79410:94509)
 w(279335:289326) = u(5)*w(84518:94509)
 w(289327:295796) = u(6)*w(88040:94509)
 w(295797:299858) = u(7)*w(90448:94509)
 w(299859:302274) = u(8)*w(92094:94509)
 w(302275:303117) = u(9)*w(26415:27257)
 w(303118:303833) = u(10)*w(26542:27257)
 w(303834:304437) = u(11)*w(26654:27257)
 w(304438:304943) = u(12)*w(26752:27257)
 w(304944:305364) = u(13)*w(26837:27257)
 w(305365:305712) = u(14)*w(26910:27257)
 w(305713:305998) = u(15)*w(26972:27257)
 w(305999:306232) = u(16)*w(27024:27257)
 w(306233:306423) = u(17)*w(27067:27257)
 w(306424:306579) = u(18)*w(27102:27257)
 w(306580:306707) = u(19)*w(27130:27257)
 w(306708:306813) = u(20)*w(27152:27257)
 w(306814:306902) = u(21)*w(27169:27257)
 w(306903:306978) = u(22)*w(27182:27257)
 w(306979:307044) = u(23)*w(27192:27257)
 w(307045:307051) = u(34)*w(1760:1766)
 w(307052:307057) = u(35)*w(1761:1766)
 w(307058:307062) = u(36)*w(1762:1766)
 w(307063:307066) = u(37)*w(1763:1766)
 w(307067:307069) = u(38)*w(1764:1766)
 w(307070:307071) = u(39)*w(1765:1766)
 w(307072) = u(40)*w(1766)
endif
! terms of degree 9
if (9.le.mxd) then
 w(307073:519635) = u(0)*w(94510:307072)
 w(519636:664946) = u(1)*w(161762:307072)
 w(664947:762991) = u(2)*w(209028:307072)
 w(762992:828251) = u(3)*w(241813:307072)
 w(828252:871089) = u(4)*w(264235:307072)
 w(871090:898827) = u(5)*w(279335:307072)
 w(898828:916573) = u(6)*w(289327:307072)
 w(916574:927849) = u(7)*w(295797:307072)
 w(927850:935063) = u(8)*w(299859:307072)
 w(935064:936333) = u(9)*w(93240:94509)
 w(936334:937453) = u(10)*w(93390:94509)
 w(937454:938433) = u(11)*w(93530:94509)
 w(938434:939283) = u(12)*w(93660:94509)
 w(939284:940013) = u(13)*w(93780:94509)
 w(940014:940633) = u(14)*w(93890:94509)
 w(940634:941153) = u(15)*w(93990:94509)
 w(941154:941583) = u(16)*w(94080:94509)
 w(941584:941933) = u(17)*w(94160:94509)
 w(941934:942213) = u(18)*w(94230:94509)
 w(942214:942433) = u(19)*w(94290:94509)
 w(942434:942603) = u(20)*w(94340:94509)
 w(942604:942733) = u(21)*w(94380:94509)
 w(942734:942833) = u(22)*w(94410:94509)
 w(942834:942913) = u(23)*w(94430:94509)
 w(942914:942971) = u(24)*w(27200:27257)
 w(942972:943019) = u(25)*w(27210:27257)
 w(943020:943058) = u(26)*w(27219:27257)
 w(943059:943089) = u(27)*w(27227:27257)
 w(943090:943113) = u(28)*w(27234:27257)
 w(943114:943131) = u(29)*w(27240:27257)
 w(943132:943144) = u(30)*w(27245:27257)
 w(943145:943153) = u(31)*w(27249:27257)
 w(943154:943159) = u(32)*w(27252:27257)
 w(943160:943163) = u(33)*w(27254:27257)
endif
END SUBROUTINE mg4321_prib