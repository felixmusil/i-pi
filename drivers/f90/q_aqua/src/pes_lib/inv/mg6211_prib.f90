SUBROUTINE mg6211_prib (mxd, u, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: u(0:)
real (kind=wp), intent (out) :: w(0:mg6211_npb(mxd)-1)
!-----------------------------------------------------------------------
if (size(u).ne.nr) then
 stop 'mg6211_prib: bad size u'
!! else if (size(w).ne.mg6211_npb(mxd)) then
!!  stop 'mg6211_prib: bad size w'
endif
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 8 9 8 7 6 6 0 0 0
! prib,  dnpb(0:*): 1 8 45 200 770 2654 8412 24878 69479 184716
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
 w(52) = u(15)*w(0)
 w(53) = u(16)*w(0)
endif
! terms of degree 3
if (3.le.mxd) then
 w(54:98) = u(0)*w(9:53)
 w(99:135) = u(1)*w(17:53)
 w(136:165) = u(2)*w(24:53)
 w(166:189) = u(3)*w(30:53)
 w(190:208) = u(4)*w(35:53)
 w(209:223) = u(5)*w(39:53)
 w(224:235) = u(6)*w(42:53)
 w(236:245) = u(7)*w(44:53)
 w(246) = u(17)*w(0)
 w(247) = u(18)*w(0)
 w(248) = u(19)*w(0)
 w(249) = u(20)*w(0)
 w(250) = u(21)*w(0)
 w(251) = u(22)*w(0)
 w(252) = u(23)*w(0)
 w(253) = u(24)*w(0)
endif
! terms of degree 4
if (4.le.mxd) then
 w(254:453) = u(0)*w(54:253)
 w(454:608) = u(1)*w(99:253)
 w(609:726) = u(2)*w(136:253)
 w(727:814) = u(3)*w(166:253)
 w(815:878) = u(4)*w(190:253)
 w(879:923) = u(5)*w(209:253)
 w(924:953) = u(6)*w(224:253)
 w(954:971) = u(7)*w(236:253)
 w(972:980) = u(8)*w(45:53)
 w(981:988) = u(9)*w(46:53)
 w(989:995) = u(10)*w(47:53)
 w(996:1001) = u(11)*w(48:53)
 w(1002:1006) = u(12)*w(49:53)
 w(1007:1010) = u(13)*w(50:53)
 w(1011:1013) = u(14)*w(51:53)
 w(1014:1015) = u(15)*w(52:53)
 w(1016) = u(16)*w(53)
 w(1017) = u(25)*w(0)
 w(1018) = u(26)*w(0)
 w(1019) = u(27)*w(0)
 w(1020) = u(28)*w(0)
 w(1021) = u(29)*w(0)
 w(1022) = u(30)*w(0)
 w(1023) = u(31)*w(0)
endif
! terms of degree 5
if (5.le.mxd) then
 w(1024:1793) = u(0)*w(254:1023)
 w(1794:2363) = u(1)*w(454:1023)
 w(2364:2778) = u(2)*w(609:1023)
 w(2779:3075) = u(3)*w(727:1023)
 w(3076:3284) = u(4)*w(815:1023)
 w(3285:3429) = u(5)*w(879:1023)
 w(3430:3529) = u(6)*w(924:1023)
 w(3530:3599) = u(7)*w(954:1023)
 w(3600:3607) = u(8)*w(246:253)
 w(3608:3615) = u(9)*w(246:253)
 w(3616:3623) = u(10)*w(246:253)
 w(3624:3631) = u(11)*w(246:253)
 w(3632:3639) = u(12)*w(246:253)
 w(3640:3647) = u(13)*w(246:253)
 w(3648:3655) = u(14)*w(246:253)
 w(3656:3663) = u(15)*w(246:253)
 w(3664:3671) = u(16)*w(246:253)
 w(3672) = u(32)*w(0)
 w(3673) = u(33)*w(0)
 w(3674) = u(34)*w(0)
 w(3675) = u(35)*w(0)
 w(3676) = u(36)*w(0)
 w(3677) = u(37)*w(0)
endif
! terms of degree 6
if (6.le.mxd) then
 w(3678:6331) = u(0)*w(1024:3677)
 w(6332:8215) = u(1)*w(1794:3677)
 w(8216:9529) = u(2)*w(2364:3677)
 w(9530:10428) = u(3)*w(2779:3677)
 w(10429:11030) = u(4)*w(3076:3677)
 w(11031:11423) = u(5)*w(3285:3677)
 w(11424:11671) = u(6)*w(3430:3677)
 w(11672:11819) = u(7)*w(3530:3677)
 w(11820:11871) = u(8)*w(972:1023)
 w(11872:11914) = u(9)*w(981:1023)
 w(11915:11949) = u(10)*w(989:1023)
 w(11950:11977) = u(11)*w(996:1023)
 w(11978:11999) = u(12)*w(1002:1023)
 w(12000:12016) = u(13)*w(1007:1023)
 w(12017:12029) = u(14)*w(1011:1023)
 w(12030:12039) = u(15)*w(1014:1023)
 w(12040:12047) = u(16)*w(1016:1023)
 w(12048:12055) = u(17)*w(246:253)
 w(12056:12062) = u(18)*w(247:253)
 w(12063:12068) = u(19)*w(248:253)
 w(12069:12073) = u(20)*w(249:253)
 w(12074:12077) = u(21)*w(250:253)
 w(12078:12080) = u(22)*w(251:253)
 w(12081:12082) = u(23)*w(252:253)
 w(12083) = u(24)*w(253)
 w(12084) = u(38)*w(0)
 w(12085) = u(39)*w(0)
 w(12086) = u(40)*w(0)
 w(12087) = u(41)*w(0)
 w(12088) = u(42)*w(0)
 w(12089) = u(43)*w(0)
endif
! terms of degree 7
if (7.le.mxd) then
 w(12090:20501) = u(0)*w(3678:12089)
 w(20502:26259) = u(1)*w(6332:12089)
 w(26260:30133) = u(2)*w(8216:12089)
 w(30134:32693) = u(3)*w(9530:12089)
 w(32694:34354) = u(4)*w(10429:12089)
 w(34355:35413) = u(5)*w(11031:12089)
 w(35414:36079) = u(6)*w(11424:12089)
 w(36080:36497) = u(7)*w(11672:12089)
 w(36498:36575) = u(8)*w(3600:3677)
 w(36576:36645) = u(9)*w(3608:3677)
 w(36646:36707) = u(10)*w(3616:3677)
 w(36708:36761) = u(11)*w(3624:3677)
 w(36762:36807) = u(12)*w(3632:3677)
 w(36808:36845) = u(13)*w(3640:3677)
 w(36846:36875) = u(14)*w(3648:3677)
 w(36876:36897) = u(15)*w(3656:3677)
 w(36898:36911) = u(16)*w(3664:3677)
 w(36912:36918) = u(17)*w(1017:1023)
 w(36919:36925) = u(18)*w(1017:1023)
 w(36926:36932) = u(19)*w(1017:1023)
 w(36933:36939) = u(20)*w(1017:1023)
 w(36940:36946) = u(21)*w(1017:1023)
 w(36947:36953) = u(22)*w(1017:1023)
 w(36954:36960) = u(23)*w(1017:1023)
 w(36961:36967) = u(24)*w(1017:1023)
endif
! terms of degree 8
if (8.le.mxd) then
 w(36968:61845) = u(0)*w(12090:36967)
 w(61846:78311) = u(1)*w(20502:36967)
 w(78312:89019) = u(2)*w(26260:36967)
 w(89020:95853) = u(3)*w(30134:36967)
 w(95854:100127) = u(4)*w(32694:36967)
 w(100128:102740) = u(5)*w(34355:36967)
 w(102741:104294) = u(6)*w(35414:36967)
 w(104295:105182) = u(7)*w(36080:36967)
 w(105183:105452) = u(8)*w(11820:12089)
 w(105453:105670) = u(9)*w(11872:12089)
 w(105671:105845) = u(10)*w(11915:12089)
 w(105846:105985) = u(11)*w(11950:12089)
 w(105986:106097) = u(12)*w(11978:12089)
 w(106098:106187) = u(13)*w(12000:12089)
 w(106188:106260) = u(14)*w(12017:12089)
 w(106261:106320) = u(15)*w(12030:12089)
 w(106321:106370) = u(16)*w(12040:12089)
 w(106371:106376) = u(17)*w(3672:3677)
 w(106377:106382) = u(18)*w(3672:3677)
 w(106383:106388) = u(19)*w(3672:3677)
 w(106389:106394) = u(20)*w(3672:3677)
 w(106395:106400) = u(21)*w(3672:3677)
 w(106401:106406) = u(22)*w(3672:3677)
 w(106407:106412) = u(23)*w(3672:3677)
 w(106413:106418) = u(24)*w(3672:3677)
 w(106419:106425) = u(25)*w(1017:1023)
 w(106426:106431) = u(26)*w(1018:1023)
 w(106432:106436) = u(27)*w(1019:1023)
 w(106437:106440) = u(28)*w(1020:1023)
 w(106441:106443) = u(29)*w(1021:1023)
 w(106444:106445) = u(30)*w(1022:1023)
 w(106446) = u(31)*w(1023)
endif
! terms of degree 9
if (9.le.mxd) then
 w(106447:175925) = u(0)*w(36968:106446)
 w(175926:220526) = u(1)*w(61846:106446)
 w(220527:248661) = u(2)*w(78312:106446)
 w(248662:266088) = u(3)*w(89020:106446)
 w(266089:276681) = u(4)*w(95854:106446)
 w(276682:283000) = u(5)*w(100128:106446)
 w(283001:286706) = u(6)*w(102741:106446)
 w(286707:288858) = u(7)*w(104295:106446)
 w(288859:289328) = u(8)*w(36498:36967)
 w(289329:289720) = u(9)*w(36576:36967)
 w(289721:290042) = u(10)*w(36646:36967)
 w(290043:290302) = u(11)*w(36708:36967)
 w(290303:290508) = u(12)*w(36762:36967)
 w(290509:290668) = u(13)*w(36808:36967)
 w(290669:290790) = u(14)*w(36846:36967)
 w(290791:290882) = u(15)*w(36876:36967)
 w(290883:290952) = u(16)*w(36898:36967)
 w(290953:290994) = u(17)*w(12048:12089)
 w(290995:291028) = u(18)*w(12056:12089)
 w(291029:291055) = u(19)*w(12063:12089)
 w(291056:291076) = u(20)*w(12069:12089)
 w(291077:291092) = u(21)*w(12074:12089)
 w(291093:291104) = u(22)*w(12078:12089)
 w(291105:291113) = u(23)*w(12081:12089)
 w(291114:291120) = u(24)*w(12083:12089)
 w(291121:291126) = u(25)*w(3672:3677)
 w(291127:291132) = u(26)*w(3672:3677)
 w(291133:291138) = u(27)*w(3672:3677)
 w(291139:291144) = u(28)*w(3672:3677)
 w(291145:291150) = u(29)*w(3672:3677)
 w(291151:291156) = u(30)*w(3672:3677)
 w(291157:291162) = u(31)*w(3672:3677)
endif
END SUBROUTINE mg6211_prib