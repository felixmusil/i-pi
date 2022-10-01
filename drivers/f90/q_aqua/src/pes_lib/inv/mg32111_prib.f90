SUBROUTINE mg32111_prib (mxd, u, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: u(0:)
real (kind=wp), intent (out) :: w(0:mg32111_npb(mxd)-1)
!-----------------------------------------------------------------------
if (size(u).ne.nr) then
 stop 'mg32111_prib: bad size u'
!! else if (size(w).ne.mg32111_npb(mxd)) then
!!  stop 'mg32111_prib: bad size w'
endif
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 12 10 5 0 0 1 0 0 0
! prib,  dnpb(0:*): 1 12 88 489 2260 9108 32972 109356 337040 975564
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
endif
! terms of degree 2
if (2.le.mxd) then
 w(13:24) = u(0)*w(1:12)
 w(25:35) = u(1)*w(2:12)
 w(36:45) = u(2)*w(3:12)
 w(46:54) = u(3)*w(4:12)
 w(55:62) = u(4)*w(5:12)
 w(63:69) = u(5)*w(6:12)
 w(70:75) = u(6)*w(7:12)
 w(76:80) = u(7)*w(8:12)
 w(81:84) = u(8)*w(9:12)
 w(85:87) = u(9)*w(10:12)
 w(88:89) = u(10)*w(11:12)
 w(90) = u(11)*w(12)
 w(91) = u(12)*w(0)
 w(92) = u(13)*w(0)
 w(93) = u(14)*w(0)
 w(94) = u(15)*w(0)
 w(95) = u(16)*w(0)
 w(96) = u(17)*w(0)
 w(97) = u(18)*w(0)
 w(98) = u(19)*w(0)
 w(99) = u(20)*w(0)
 w(100) = u(21)*w(0)
endif
! terms of degree 3
if (3.le.mxd) then
 w(101:188) = u(0)*w(13:100)
 w(189:264) = u(1)*w(25:100)
 w(265:329) = u(2)*w(36:100)
 w(330:384) = u(3)*w(46:100)
 w(385:430) = u(4)*w(55:100)
 w(431:468) = u(5)*w(63:100)
 w(469:499) = u(6)*w(70:100)
 w(500:524) = u(7)*w(76:100)
 w(525:544) = u(8)*w(81:100)
 w(545:560) = u(9)*w(85:100)
 w(561:573) = u(10)*w(88:100)
 w(574:584) = u(11)*w(90:100)
 w(585) = u(22)*w(0)
 w(586) = u(23)*w(0)
 w(587) = u(24)*w(0)
 w(588) = u(25)*w(0)
 w(589) = u(26)*w(0)
endif
! terms of degree 4
if (4.le.mxd) then
 w(590:1078) = u(0)*w(101:589)
 w(1079:1479) = u(1)*w(189:589)
 w(1480:1804) = u(2)*w(265:589)
 w(1805:2064) = u(3)*w(330:589)
 w(2065:2269) = u(4)*w(385:589)
 w(2270:2428) = u(5)*w(431:589)
 w(2429:2549) = u(6)*w(469:589)
 w(2550:2639) = u(7)*w(500:589)
 w(2640:2704) = u(8)*w(525:589)
 w(2705:2749) = u(9)*w(545:589)
 w(2750:2778) = u(10)*w(561:589)
 w(2779:2794) = u(11)*w(574:589)
 w(2795:2804) = u(12)*w(91:100)
 w(2805:2813) = u(13)*w(92:100)
 w(2814:2821) = u(14)*w(93:100)
 w(2822:2828) = u(15)*w(94:100)
 w(2829:2834) = u(16)*w(95:100)
 w(2835:2839) = u(17)*w(96:100)
 w(2840:2843) = u(18)*w(97:100)
 w(2844:2846) = u(19)*w(98:100)
 w(2847:2848) = u(20)*w(99:100)
 w(2849) = u(21)*w(100)
endif
! terms of degree 5
if (5.le.mxd) then
 w(2850:5109) = u(0)*w(590:2849)
 w(5110:6880) = u(1)*w(1079:2849)
 w(6881:8250) = u(2)*w(1480:2849)
 w(8251:9295) = u(3)*w(1805:2849)
 w(9296:10080) = u(4)*w(2065:2849)
 w(10081:10660) = u(5)*w(2270:2849)
 w(10661:11081) = u(6)*w(2429:2849)
 w(11082:11381) = u(7)*w(2550:2849)
 w(11382:11591) = u(8)*w(2640:2849)
 w(11592:11736) = u(9)*w(2705:2849)
 w(11737:11836) = u(10)*w(2750:2849)
 w(11837:11907) = u(11)*w(2779:2849)
 w(11908:11912) = u(12)*w(585:589)
 w(11913:11917) = u(13)*w(585:589)
 w(11918:11922) = u(14)*w(585:589)
 w(11923:11927) = u(15)*w(585:589)
 w(11928:11932) = u(16)*w(585:589)
 w(11933:11937) = u(17)*w(585:589)
 w(11938:11942) = u(18)*w(585:589)
 w(11943:11947) = u(19)*w(585:589)
 w(11948:11952) = u(20)*w(585:589)
 w(11953:11957) = u(21)*w(585:589)
endif
! terms of degree 6
if (6.le.mxd) then
 w(11958:21065) = u(0)*w(2850:11957)
 w(21066:27913) = u(1)*w(5110:11957)
 w(27914:32990) = u(2)*w(6881:11957)
 w(32991:36697) = u(3)*w(8251:11957)
 w(36698:39359) = u(4)*w(9296:11957)
 w(39360:41236) = u(5)*w(10081:11957)
 w(41237:42533) = u(6)*w(10661:11957)
 w(42534:43409) = u(7)*w(11082:11957)
 w(43410:43985) = u(8)*w(11382:11957)
 w(43986:44351) = u(9)*w(11592:11957)
 w(44352:44572) = u(10)*w(11737:11957)
 w(44573:44693) = u(11)*w(11837:11957)
 w(44694:44748) = u(12)*w(2795:2849)
 w(44749:44793) = u(13)*w(2805:2849)
 w(44794:44829) = u(14)*w(2814:2849)
 w(44830:44857) = u(15)*w(2822:2849)
 w(44858:44878) = u(16)*w(2829:2849)
 w(44879:44893) = u(17)*w(2835:2849)
 w(44894:44903) = u(18)*w(2840:2849)
 w(44904:44909) = u(19)*w(2844:2849)
 w(44910:44912) = u(20)*w(2847:2849)
 w(44913) = u(21)*w(2849)
 w(44914:44918) = u(22)*w(585:589)
 w(44919:44922) = u(23)*w(586:589)
 w(44923:44925) = u(24)*w(587:589)
 w(44926:44927) = u(25)*w(588:589)
 w(44928) = u(26)*w(589)
 w(44929) = u(27)*w(0)
endif
! terms of degree 7
if (7.le.mxd) then
 w(44930:77901) = u(0)*w(11958:44929)
 w(77902:101765) = u(1)*w(21066:44929)
 w(101766:118781) = u(2)*w(27914:44929)
 w(118782:130720) = u(3)*w(32991:44929)
 w(130721:138952) = u(4)*w(36698:44929)
 w(138953:144522) = u(5)*w(39360:44929)
 w(144523:148215) = u(6)*w(41237:44929)
 w(148216:150611) = u(7)*w(42534:44929)
 w(150612:152131) = u(8)*w(43410:44929)
 w(152132:153075) = u(9)*w(43986:44929)
 w(153076:153653) = u(10)*w(44352:44929)
 w(153654:154010) = u(11)*w(44573:44929)
 w(154011:154060) = u(12)*w(11908:11957)
 w(154061:154105) = u(13)*w(11913:11957)
 w(154106:154145) = u(14)*w(11918:11957)
 w(154146:154180) = u(15)*w(11923:11957)
 w(154181:154210) = u(16)*w(11928:11957)
 w(154211:154235) = u(17)*w(11933:11957)
 w(154236:154255) = u(18)*w(11938:11957)
 w(154256:154270) = u(19)*w(11943:11957)
 w(154271:154280) = u(20)*w(11948:11957)
 w(154281:154285) = u(21)*w(11953:11957)
endif
! terms of degree 8
if (8.le.mxd) then
 w(154286:263641) = u(0)*w(44930:154285)
 w(263642:340025) = u(1)*w(77902:154285)
 w(340026:392545) = u(2)*w(101766:154285)
 w(392546:428049) = u(3)*w(118782:154285)
 w(428050:451614) = u(4)*w(130721:154285)
 w(451615:466947) = u(5)*w(138953:154285)
 w(466948:476710) = u(6)*w(144523:154285)
 w(476711:482780) = u(7)*w(148216:154285)
 w(482781:486454) = u(8)*w(150612:154285)
 w(486455:488608) = u(9)*w(152132:154285)
 w(488609:489818) = u(10)*w(153076:154285)
 w(489819:490450) = u(11)*w(153654:154285)
 w(490451:490686) = u(12)*w(44694:44929)
 w(490687:490867) = u(13)*w(44749:44929)
 w(490868:491003) = u(14)*w(44794:44929)
 w(491004:491103) = u(15)*w(44830:44929)
 w(491104:491175) = u(16)*w(44858:44929)
 w(491176:491226) = u(17)*w(44879:44929)
 w(491227:491262) = u(18)*w(44894:44929)
 w(491263:491288) = u(19)*w(44904:44929)
 w(491289:491308) = u(20)*w(44910:44929)
 w(491309:491325) = u(21)*w(44913:44929)
endif
! terms of degree 9
if (9.le.mxd) then
 w(491326:828365) = u(0)*w(154286:491325)
 w(828366:1056049) = u(1)*w(263642:491325)
 w(1056050:1207349) = u(2)*w(340026:491325)
 w(1207350:1306129) = u(3)*w(392546:491325)
 w(1306130:1369405) = u(4)*w(428050:491325)
 w(1369406:1409116) = u(5)*w(451615:491325)
 w(1409117:1433494) = u(6)*w(466948:491325)
 w(1433495:1448109) = u(7)*w(476711:491325)
 w(1448110:1456654) = u(8)*w(482781:491325)
 w(1456655:1461525) = u(9)*w(486455:491325)
 w(1461526:1464242) = u(10)*w(488609:491325)
 w(1464243:1465749) = u(11)*w(489819:491325)
 w(1465750:1466024) = u(12)*w(154011:154285)
 w(1466025:1466249) = u(13)*w(154061:154285)
 w(1466250:1466429) = u(14)*w(154106:154285)
 w(1466430:1466569) = u(15)*w(154146:154285)
 w(1466570:1466674) = u(16)*w(154181:154285)
 w(1466675:1466749) = u(17)*w(154211:154285)
 w(1466750:1466799) = u(18)*w(154236:154285)
 w(1466800:1466829) = u(19)*w(154256:154285)
 w(1466830:1466844) = u(20)*w(154271:154285)
 w(1466845:1466849) = u(21)*w(154281:154285)
 w(1466850:1466865) = u(22)*w(44914:44929)
 w(1466866:1466876) = u(23)*w(44919:44929)
 w(1466877:1466883) = u(24)*w(44923:44929)
 w(1466884:1466887) = u(25)*w(44926:44929)
 w(1466888:1466889) = u(26)*w(44928:44929)
endif
END SUBROUTINE mg32111_prib
