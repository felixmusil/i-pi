SUBROUTINE mg2211_prib (mxd, u, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: u(0:)
real (kind=wp), intent (out) :: w(0:mg2211_npb(mxd)-1)
!-----------------------------------------------------------------------
if (size(u).ne.nr) then
 stop 'mg2211_prib: bad size u'
!! else if (size(w).ne.mg2211_npb(mxd)) then
!!  stop 'mg2211_prib: bad size w'
endif
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 8 7 0 0 0 0 0 0 0
! prib,  dnpb(0:*): 1 8 43 176 610 1856 5118 13008 30921 69400
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
endif
! terms of degree 2
if (2.le.mxd) then
 w(9:16) = u(0)*w(1:8)
 w(17:23) = u(1)*w(2:8)
 w(24:29) = u(2)*w(3:8)
 w(30:34) = u(3)*w(4:8)
 w(35:38) = u(4)*w(5:8)
 w(39:41) = u(5)*w(6:8)
 w(42:43) = u(6)*w(7:8)
 w(44) = u(7)*w(8)
 w(45) = u(8)*w(0)
 w(46) = u(9)*w(0)
 w(47) = u(10)*w(0)
 w(48) = u(11)*w(0)
 w(49) = u(12)*w(0)
 w(50) = u(13)*w(0)
 w(51) = u(14)*w(0)
endif
! terms of degree 3
if (3.le.mxd) then
 w(52:94) = u(0)*w(9:51)
 w(95:129) = u(1)*w(17:51)
 w(130:157) = u(2)*w(24:51)
 w(158:179) = u(3)*w(30:51)
 w(180:196) = u(4)*w(35:51)
 w(197:209) = u(5)*w(39:51)
 w(210:219) = u(6)*w(42:51)
 w(220:227) = u(7)*w(44:51)
endif
! terms of degree 4
if (4.le.mxd) then
 w(228:403) = u(0)*w(52:227)
 w(404:536) = u(1)*w(95:227)
 w(537:634) = u(2)*w(130:227)
 w(635:704) = u(3)*w(158:227)
 w(705:752) = u(4)*w(180:227)
 w(753:783) = u(5)*w(197:227)
 w(784:801) = u(6)*w(210:227)
 w(802:809) = u(7)*w(220:227)
 w(810:816) = u(8)*w(45:51)
 w(817:822) = u(9)*w(46:51)
 w(823:827) = u(10)*w(47:51)
 w(828:831) = u(11)*w(48:51)
 w(832:834) = u(12)*w(49:51)
 w(835:836) = u(13)*w(50:51)
 w(837) = u(14)*w(51)
endif
! terms of degree 5
if (5.le.mxd) then
 w(838:1447) = u(0)*w(228:837)
 w(1448:1881) = u(1)*w(404:837)
 w(1882:2182) = u(2)*w(537:837)
 w(2183:2385) = u(3)*w(635:837)
 w(2386:2518) = u(4)*w(705:837)
 w(2519:2603) = u(5)*w(753:837)
 w(2604:2657) = u(6)*w(784:837)
 w(2658:2693) = u(7)*w(802:837)
endif
! terms of degree 6
if (6.le.mxd) then
 w(2694:4549) = u(0)*w(838:2693)
 w(4550:5795) = u(1)*w(1448:2693)
 w(5796:6607) = u(2)*w(1882:2693)
 w(6608:7118) = u(3)*w(2183:2693)
 w(7119:7426) = u(4)*w(2386:2693)
 w(7427:7601) = u(5)*w(2519:2693)
 w(7602:7691) = u(6)*w(2604:2693)
 w(7692:7727) = u(7)*w(2658:2693)
 w(7728:7755) = u(8)*w(810:837)
 w(7756:7776) = u(9)*w(817:837)
 w(7777:7791) = u(10)*w(823:837)
 w(7792:7801) = u(11)*w(828:837)
 w(7802:7807) = u(12)*w(832:837)
 w(7808:7810) = u(13)*w(835:837)
 w(7811) = u(14)*w(837)
endif
! terms of degree 7
if (7.le.mxd) then
 w(7812:12929) = u(0)*w(2694:7811)
 w(12930:16191) = u(1)*w(4550:7811)
 w(16192:18207) = u(2)*w(5796:7811)
 w(18208:19411) = u(3)*w(6608:7811)
 w(19412:20104) = u(4)*w(7119:7811)
 w(20105:20489) = u(5)*w(7427:7811)
 w(20490:20699) = u(6)*w(7602:7811)
 w(20700:20819) = u(7)*w(7692:7811)
endif
! terms of degree 8
if (8.le.mxd) then
 w(20820:33827) = u(0)*w(7812:20819)
 w(33828:41717) = u(1)*w(12930:20819)
 w(41718:46345) = u(2)*w(16192:20819)
 w(46346:48957) = u(3)*w(18208:20819)
 w(48958:50365) = u(4)*w(19412:20819)
 w(50366:51080) = u(5)*w(20105:20819)
 w(51081:51410) = u(6)*w(20490:20819)
 w(51411:51530) = u(7)*w(20700:20819)
 w(51531:51614) = u(8)*w(7728:7811)
 w(51615:51670) = u(9)*w(7756:7811)
 w(51671:51705) = u(10)*w(7777:7811)
 w(51706:51725) = u(11)*w(7792:7811)
 w(51726:51735) = u(12)*w(7802:7811)
 w(51736:51739) = u(13)*w(7808:7811)
 w(51740) = u(14)*w(7811)
endif
! terms of degree 9
if (9.le.mxd) then
 w(51741:82661) = u(0)*w(20820:51740)
 w(82662:100574) = u(1)*w(33828:51740)
 w(100575:110597) = u(2)*w(41718:51740)
 w(110598:115992) = u(3)*w(46346:51740)
 w(115993:118775) = u(4)*w(48958:51740)
 w(118776:120150) = u(5)*w(50366:51740)
 w(120151:120810) = u(6)*w(51081:51740)
 w(120811:121140) = u(7)*w(51411:51740)
endif
END SUBROUTINE mg2211_prib
