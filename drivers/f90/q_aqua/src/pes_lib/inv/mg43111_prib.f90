SUBROUTINE mg43111_prib (mxd, u, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: u(0:)
real (kind=wp), intent (out) :: w(0:mg43111_npb(mxd)-1)
!-----------------------------------------------------------------------
if (size(u).ne.nr) then
 stop 'mg43111_prib: bad size u'
!! else if (size(w).ne.mg43111_npb(mxd)) then
!!  stop 'mg43111_prib: bad size w'
endif
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 12 12 12 7 0 1 0 0 0
! prib,  dnpb(0:*): 1 12 90 520 2530 10836 42009 150136 501184 1577984
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
 w(101) = u(22)*w(0)
 w(102) = u(23)*w(0)
endif
! terms of degree 3
if (3.le.mxd) then
 w(103:192) = u(0)*w(13:102)
 w(193:270) = u(1)*w(25:102)
 w(271:337) = u(2)*w(36:102)
 w(338:394) = u(3)*w(46:102)
 w(395:442) = u(4)*w(55:102)
 w(443:482) = u(5)*w(63:102)
 w(483:515) = u(6)*w(70:102)
 w(516:542) = u(7)*w(76:102)
 w(543:564) = u(8)*w(81:102)
 w(565:582) = u(9)*w(85:102)
 w(583:597) = u(10)*w(88:102)
 w(598:610) = u(11)*w(90:102)
 w(611) = u(24)*w(0)
 w(612) = u(25)*w(0)
 w(613) = u(26)*w(0)
 w(614) = u(27)*w(0)
 w(615) = u(28)*w(0)
 w(616) = u(29)*w(0)
 w(617) = u(30)*w(0)
 w(618) = u(31)*w(0)
 w(619) = u(32)*w(0)
 w(620) = u(33)*w(0)
 w(621) = u(34)*w(0)
 w(622) = u(35)*w(0)
endif
! terms of degree 4
if (4.le.mxd) then
 w(623:1142) = u(0)*w(103:622)
 w(1143:1572) = u(1)*w(193:622)
 w(1573:1924) = u(2)*w(271:622)
 w(1925:2209) = u(3)*w(338:622)
 w(2210:2437) = u(4)*w(395:622)
 w(2438:2617) = u(5)*w(443:622)
 w(2618:2757) = u(6)*w(483:622)
 w(2758:2864) = u(7)*w(516:622)
 w(2865:2944) = u(8)*w(543:622)
 w(2945:3002) = u(9)*w(565:622)
 w(3003:3042) = u(10)*w(583:622)
 w(3043:3067) = u(11)*w(598:622)
 w(3068:3079) = u(12)*w(91:102)
 w(3080:3090) = u(13)*w(92:102)
 w(3091:3100) = u(14)*w(93:102)
 w(3101:3109) = u(15)*w(94:102)
 w(3110:3117) = u(16)*w(95:102)
 w(3118:3124) = u(17)*w(96:102)
 w(3125:3130) = u(18)*w(97:102)
 w(3131:3135) = u(19)*w(98:102)
 w(3136:3139) = u(20)*w(99:102)
 w(3140:3142) = u(21)*w(100:102)
 w(3143:3144) = u(22)*w(101:102)
 w(3145) = u(23)*w(102)
 w(3146) = u(36)*w(0)
 w(3147) = u(37)*w(0)
 w(3148) = u(38)*w(0)
 w(3149) = u(39)*w(0)
 w(3150) = u(40)*w(0)
 w(3151) = u(41)*w(0)
 w(3152) = u(42)*w(0)
endif
! terms of degree 5
if (5.le.mxd) then
 w(3153:5682) = u(0)*w(623:3152)
 w(5683:7692) = u(1)*w(1143:3152)
 w(7693:9272) = u(2)*w(1573:3152)
 w(9273:10500) = u(3)*w(1925:3152)
 w(10501:11443) = u(4)*w(2210:3152)
 w(11444:12158) = u(5)*w(2438:3152)
 w(12159:12693) = u(6)*w(2618:3152)
 w(12694:13088) = u(7)*w(2758:3152)
 w(13089:13376) = u(8)*w(2865:3152)
 w(13377:13584) = u(9)*w(2945:3152)
 w(13585:13734) = u(10)*w(3003:3152)
 w(13735:13844) = u(11)*w(3043:3152)
 w(13845:13856) = u(12)*w(611:622)
 w(13857:13868) = u(13)*w(611:622)
 w(13869:13880) = u(14)*w(611:622)
 w(13881:13892) = u(15)*w(611:622)
 w(13893:13904) = u(16)*w(611:622)
 w(13905:13916) = u(17)*w(611:622)
 w(13917:13928) = u(18)*w(611:622)
 w(13929:13940) = u(19)*w(611:622)
 w(13941:13952) = u(20)*w(611:622)
 w(13953:13964) = u(21)*w(611:622)
 w(13965:13976) = u(22)*w(611:622)
 w(13977:13988) = u(23)*w(611:622)
endif
! terms of degree 6
if (6.le.mxd) then
 w(13989:24824) = u(0)*w(3153:13988)
 w(24825:33130) = u(1)*w(5683:13988)
 w(33131:39426) = u(2)*w(7693:13988)
 w(39427:44142) = u(3)*w(9273:13988)
 w(44143:47630) = u(4)*w(10501:13988)
 w(47631:50175) = u(5)*w(11444:13988)
 w(50176:52005) = u(6)*w(12159:13988)
 w(52006:53300) = u(7)*w(12694:13988)
 w(53301:54200) = u(8)*w(13089:13988)
 w(54201:54812) = u(9)*w(13377:13988)
 w(54813:55216) = u(10)*w(13585:13988)
 w(55217:55470) = u(11)*w(13735:13988)
 w(55471:55555) = u(12)*w(3068:3152)
 w(55556:55628) = u(13)*w(3080:3152)
 w(55629:55690) = u(14)*w(3091:3152)
 w(55691:55742) = u(15)*w(3101:3152)
 w(55743:55785) = u(16)*w(3110:3152)
 w(55786:55820) = u(17)*w(3118:3152)
 w(55821:55848) = u(18)*w(3125:3152)
 w(55849:55870) = u(19)*w(3131:3152)
 w(55871:55887) = u(20)*w(3136:3152)
 w(55888:55900) = u(21)*w(3140:3152)
 w(55901:55910) = u(22)*w(3143:3152)
 w(55911:55918) = u(23)*w(3145:3152)
 w(55919:55930) = u(24)*w(611:622)
 w(55931:55941) = u(25)*w(612:622)
 w(55942:55951) = u(26)*w(613:622)
 w(55952:55960) = u(27)*w(614:622)
 w(55961:55968) = u(28)*w(615:622)
 w(55969:55975) = u(29)*w(616:622)
 w(55976:55981) = u(30)*w(617:622)
 w(55982:55986) = u(31)*w(618:622)
 w(55987:55990) = u(32)*w(619:622)
 w(55991:55993) = u(33)*w(620:622)
 w(55994:55995) = u(34)*w(621:622)
 w(55996) = u(35)*w(622)
 w(55997) = u(43)*w(0)
endif
! terms of degree 7
if (7.le.mxd) then
 w(55998:98006) = u(0)*w(13989:55997)
 w(98007:129179) = u(1)*w(24825:55997)
 w(129180:152046) = u(2)*w(33131:55997)
 w(152047:168617) = u(3)*w(39427:55997)
 w(168618:180472) = u(4)*w(44143:55997)
 w(180473:188839) = u(5)*w(47631:55997)
 w(188840:194661) = u(6)*w(50176:55997)
 w(194662:198653) = u(7)*w(52006:55997)
 w(198654:201350) = u(8)*w(53301:55997)
 w(201351:203147) = u(9)*w(54201:55997)
 w(203148:204332) = u(10)*w(54813:55997)
 w(204333:205113) = u(11)*w(55217:55997)
 w(205114:205257) = u(12)*w(13845:13988)
 w(205258:205389) = u(13)*w(13857:13988)
 w(205390:205509) = u(14)*w(13869:13988)
 w(205510:205617) = u(15)*w(13881:13988)
 w(205618:205713) = u(16)*w(13893:13988)
 w(205714:205797) = u(17)*w(13905:13988)
 w(205798:205869) = u(18)*w(13917:13988)
 w(205870:205929) = u(19)*w(13929:13988)
 w(205930:205977) = u(20)*w(13941:13988)
 w(205978:206013) = u(21)*w(13953:13988)
 w(206014:206037) = u(22)*w(13965:13988)
 w(206038:206049) = u(23)*w(13977:13988)
 w(206050:206056) = u(24)*w(3146:3152)
 w(206057:206063) = u(25)*w(3146:3152)
 w(206064:206070) = u(26)*w(3146:3152)
 w(206071:206077) = u(27)*w(3146:3152)
 w(206078:206084) = u(28)*w(3146:3152)
 w(206085:206091) = u(29)*w(3146:3152)
 w(206092:206098) = u(30)*w(3146:3152)
 w(206099:206105) = u(31)*w(3146:3152)
 w(206106:206112) = u(32)*w(3146:3152)
 w(206113:206119) = u(33)*w(3146:3152)
 w(206120:206126) = u(34)*w(3146:3152)
 w(206127:206133) = u(35)*w(3146:3152)
endif
! terms of degree 8
if (8.le.mxd) then
 w(206134:356269) = u(0)*w(55998:206133)
 w(356270:464396) = u(1)*w(98007:206133)
 w(464397:541350) = u(2)*w(129180:206133)
 w(541351:595437) = u(3)*w(152047:206133)
 w(595438:632953) = u(4)*w(168618:206133)
 w(632954:658614) = u(5)*w(180473:206133)
 w(658615:675908) = u(6)*w(188840:206133)
 w(675909:687380) = u(7)*w(194662:206133)
 w(687381:694860) = u(8)*w(198654:206133)
 w(694861:699643) = u(9)*w(201351:206133)
 w(699644:702629) = u(10)*w(203148:206133)
 w(702630:704430) = u(11)*w(204333:206133)
 w(704431:704957) = u(12)*w(55471:55997)
 w(704958:705399) = u(13)*w(55556:55997)
 w(705400:705768) = u(14)*w(55629:55997)
 w(705769:706075) = u(15)*w(55691:55997)
 w(706076:706330) = u(16)*w(55743:55997)
 w(706331:706542) = u(17)*w(55786:55997)
 w(706543:706719) = u(18)*w(55821:55997)
 w(706720:706868) = u(19)*w(55849:55997)
 w(706869:706995) = u(20)*w(55871:55997)
 w(706996:707105) = u(21)*w(55888:55997)
 w(707106:707202) = u(22)*w(55901:55997)
 w(707203:707289) = u(23)*w(55911:55997)
 w(707290:707296) = u(36)*w(3146:3152)
 w(707297:707302) = u(37)*w(3147:3152)
 w(707303:707307) = u(38)*w(3148:3152)
 w(707308:707311) = u(39)*w(3149:3152)
 w(707312:707314) = u(40)*w(3150:3152)
 w(707315:707316) = u(41)*w(3151:3152)
 w(707317) = u(42)*w(3152)
endif
! terms of degree 9
if (9.le.mxd) then
 w(707318:1208501) = u(0)*w(206134:707317)
 w(1208502:1559549) = u(1)*w(356270:707317)
 w(1559550:1802470) = u(2)*w(464397:707317)
 w(1802471:1968437) = u(3)*w(541351:707317)
 w(1968438:2080317) = u(4)*w(595438:707317)
 w(2080318:2154681) = u(5)*w(632954:707317)
 w(2154682:2203384) = u(6)*w(658615:707317)
 w(2203385:2234793) = u(7)*w(675909:707317)
 w(2234794:2254730) = u(8)*w(687381:707317)
 w(2254731:2267187) = u(9)*w(694861:707317)
 w(2267188:2274861) = u(10)*w(699644:707317)
 w(2274862:2279549) = u(11)*w(702630:707317)
 w(2279550:2280569) = u(12)*w(205114:206133)
 w(2280570:2281445) = u(13)*w(205258:206133)
 w(2281446:2282189) = u(14)*w(205390:206133)
 w(2282190:2282813) = u(15)*w(205510:206133)
 w(2282814:2283329) = u(16)*w(205618:206133)
 w(2283330:2283749) = u(17)*w(205714:206133)
 w(2283750:2284085) = u(18)*w(205798:206133)
 w(2284086:2284349) = u(19)*w(205870:206133)
 w(2284350:2284553) = u(20)*w(205930:206133)
 w(2284554:2284709) = u(21)*w(205978:206133)
 w(2284710:2284829) = u(22)*w(206014:206133)
 w(2284830:2284925) = u(23)*w(206038:206133)
 w(2284926:2285004) = u(24)*w(55919:55997)
 w(2285005:2285071) = u(25)*w(55931:55997)
 w(2285072:2285127) = u(26)*w(55942:55997)
 w(2285128:2285173) = u(27)*w(55952:55997)
 w(2285174:2285210) = u(28)*w(55961:55997)
 w(2285211:2285239) = u(29)*w(55969:55997)
 w(2285240:2285261) = u(30)*w(55976:55997)
 w(2285262:2285277) = u(31)*w(55982:55997)
 w(2285278:2285288) = u(32)*w(55987:55997)
 w(2285289:2285295) = u(33)*w(55991:55997)
 w(2285296:2285299) = u(34)*w(55994:55997)
 w(2285300:2285301) = u(35)*w(55996:55997)
endif
END SUBROUTINE mg43111_prib