SUBROUTINE mg322_prib (mxd, u, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: u(0:)
real (kind=wp), intent (out) :: w(0:mg322_npb(mxd)-1)
!-----------------------------------------------------------------------
if (size(u).ne.nr) then
 stop 'mg322_prib: bad size u'
!! else if (size(w).ne.mg322_npb(mxd)) then
!!  stop 'mg322_prib: bad size w'
endif
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 6 10 3 0 0 2 0 0 0
! prib,  dnpb(0:*): 1 6 31 119 409 1235 3453 8933 21846 50627
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
endif
! terms of degree 3
if (3.le.mxd) then
 w(38:68) = u(0)*w(7:37)
 w(69:93) = u(1)*w(13:37)
 w(94:113) = u(2)*w(18:37)
 w(114:129) = u(3)*w(22:37)
 w(130:142) = u(4)*w(25:37)
 w(143:153) = u(5)*w(27:37)
 w(154) = u(16)*w(0)
 w(155) = u(17)*w(0)
 w(156) = u(18)*w(0)
endif
! terms of degree 4
if (4.le.mxd) then
 w(157:275) = u(0)*w(38:156)
 w(276:363) = u(1)*w(69:156)
 w(364:426) = u(2)*w(94:156)
 w(427:469) = u(3)*w(114:156)
 w(470:496) = u(4)*w(130:156)
 w(497:510) = u(5)*w(143:156)
 w(511:520) = u(6)*w(28:37)
 w(521:529) = u(7)*w(29:37)
 w(530:537) = u(8)*w(30:37)
 w(538:544) = u(9)*w(31:37)
 w(545:550) = u(10)*w(32:37)
 w(551:555) = u(11)*w(33:37)
 w(556:559) = u(12)*w(34:37)
 w(560:562) = u(13)*w(35:37)
 w(563:564) = u(14)*w(36:37)
 w(565) = u(15)*w(37)
endif
! terms of degree 5
if (5.le.mxd) then
 w(566:974) = u(0)*w(157:565)
 w(975:1264) = u(1)*w(276:565)
 w(1265:1466) = u(2)*w(364:565)
 w(1467:1605) = u(3)*w(427:565)
 w(1606:1701) = u(4)*w(470:565)
 w(1702:1770) = u(5)*w(497:565)
 w(1771:1773) = u(6)*w(154:156)
 w(1774:1776) = u(7)*w(154:156)
 w(1777:1779) = u(8)*w(154:156)
 w(1780:1782) = u(9)*w(154:156)
 w(1783:1785) = u(10)*w(154:156)
 w(1786:1788) = u(11)*w(154:156)
 w(1789:1791) = u(12)*w(154:156)
 w(1792:1794) = u(13)*w(154:156)
 w(1795:1797) = u(14)*w(154:156)
 w(1798:1800) = u(15)*w(154:156)
endif
! terms of degree 6
if (6.le.mxd) then
 w(1801:3035) = u(0)*w(566:1800)
 w(3036:3861) = u(1)*w(975:1800)
 w(3862:4397) = u(2)*w(1265:1800)
 w(4398:4731) = u(3)*w(1467:1800)
 w(4732:4926) = u(4)*w(1606:1800)
 w(4927:5025) = u(5)*w(1702:1800)
 w(5026:5080) = u(6)*w(511:565)
 w(5081:5125) = u(7)*w(521:565)
 w(5126:5161) = u(8)*w(530:565)
 w(5162:5189) = u(9)*w(538:565)
 w(5190:5210) = u(10)*w(545:565)
 w(5211:5225) = u(11)*w(551:565)
 w(5226:5235) = u(12)*w(556:565)
 w(5236:5241) = u(13)*w(560:565)
 w(5242:5244) = u(14)*w(563:565)
 w(5245) = u(15)*w(565)
 w(5246:5248) = u(16)*w(154:156)
 w(5249:5250) = u(17)*w(155:156)
 w(5251) = u(18)*w(156)
 w(5252) = u(19)*w(0)
 w(5253) = u(20)*w(0)
endif
! terms of degree 7
if (7.le.mxd) then
 w(5254:8706) = u(0)*w(1801:5253)
 w(8707:10924) = u(1)*w(3036:5253)
 w(10925:12316) = u(2)*w(3862:5253)
 w(12317:13172) = u(3)*w(4398:5253)
 w(13173:13694) = u(4)*w(4732:5253)
 w(13695:14021) = u(5)*w(4927:5253)
 w(14022:14051) = u(6)*w(1771:1800)
 w(14052:14078) = u(7)*w(1774:1800)
 w(14079:14102) = u(8)*w(1777:1800)
 w(14103:14123) = u(9)*w(1780:1800)
 w(14124:14141) = u(10)*w(1783:1800)
 w(14142:14156) = u(11)*w(1786:1800)
 w(14157:14168) = u(12)*w(1789:1800)
 w(14169:14177) = u(13)*w(1792:1800)
 w(14178:14183) = u(14)*w(1795:1800)
 w(14184:14186) = u(15)*w(1798:1800)
endif
! terms of degree 8
if (8.le.mxd) then
 w(14187:23119) = u(0)*w(5254:14186)
 w(23120:28599) = u(1)*w(8707:14186)
 w(28600:31861) = u(2)*w(10925:14186)
 w(31862:33731) = u(3)*w(12317:14186)
 w(33732:34745) = u(4)*w(13173:14186)
 w(34746:35237) = u(5)*w(13695:14186)
 w(35238:35465) = u(6)*w(5026:5253)
 w(35466:35638) = u(7)*w(5081:5253)
 w(35639:35766) = u(8)*w(5126:5253)
 w(35767:35858) = u(9)*w(5162:5253)
 w(35859:35922) = u(10)*w(5190:5253)
 w(35923:35965) = u(11)*w(5211:5253)
 w(35966:35993) = u(12)*w(5226:5253)
 w(35994:36011) = u(13)*w(5236:5253)
 w(36012:36023) = u(14)*w(5242:5253)
 w(36024:36032) = u(15)*w(5245:5253)
endif
! terms of degree 9
if (9.le.mxd) then
 w(36033:57878) = u(0)*w(14187:36032)
 w(57879:70791) = u(1)*w(23120:36032)
 w(70792:78224) = u(2)*w(28600:36032)
 w(78225:82395) = u(3)*w(31862:36032)
 w(82396:84696) = u(4)*w(33732:36032)
 w(84697:85983) = u(5)*w(34746:36032)
 w(85984:86148) = u(6)*w(14022:14186)
 w(86149:86283) = u(7)*w(14052:14186)
 w(86284:86391) = u(8)*w(14079:14186)
 w(86392:86475) = u(9)*w(14103:14186)
 w(86476:86538) = u(10)*w(14124:14186)
 w(86539:86583) = u(11)*w(14142:14186)
 w(86584:86613) = u(12)*w(14157:14186)
 w(86614:86631) = u(13)*w(14169:14186)
 w(86632:86640) = u(14)*w(14178:14186)
 w(86641:86643) = u(15)*w(14184:14186)
 w(86644:86651) = u(16)*w(5246:5253)
 w(86652:86656) = u(17)*w(5249:5253)
 w(86657:86659) = u(18)*w(5251:5253)
endif
END SUBROUTINE mg322_prib