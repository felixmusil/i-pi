SUBROUTINE mg721_prib (mxd, u, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: u(0:)
real (kind=wp), intent (out) :: w(0:mg721_npb(mxd)-1)
!-----------------------------------------------------------------------
if (size(u).ne.nr) then
 stop 'mg721_prib: bad size u'
!! else if (size(w).ne.mg721_npb(mxd)) then
!!  stop 'mg721_prib: bad size w'
endif
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 5 7 8 6 5 5 5 0 0
! prib,  dnpb(0:*): 1 5 22 78 249 722 1962 5024 12279 28787
! constant term
w(0) = 1
! terms of degree 1
if (1.le.mxd) then
 w(1) = u(0)*w(0)
 w(2) = u(1)*w(0)
 w(3) = u(2)*w(0)
 w(4) = u(3)*w(0)
 w(5) = u(4)*w(0)
endif
! terms of degree 2
if (2.le.mxd) then
 w(6:10) = u(0)*w(1:5)
 w(11:14) = u(1)*w(2:5)
 w(15:17) = u(2)*w(3:5)
 w(18:19) = u(3)*w(4:5)
 w(20) = u(4)*w(5)
 w(21) = u(5)*w(0)
 w(22) = u(6)*w(0)
 w(23) = u(7)*w(0)
 w(24) = u(8)*w(0)
 w(25) = u(9)*w(0)
 w(26) = u(10)*w(0)
 w(27) = u(11)*w(0)
endif
! terms of degree 3
if (3.le.mxd) then
 w(28:49) = u(0)*w(6:27)
 w(50:66) = u(1)*w(11:27)
 w(67:79) = u(2)*w(15:27)
 w(80:89) = u(3)*w(18:27)
 w(90:97) = u(4)*w(20:27)
 w(98) = u(12)*w(0)
 w(99) = u(13)*w(0)
 w(100) = u(14)*w(0)
 w(101) = u(15)*w(0)
 w(102) = u(16)*w(0)
 w(103) = u(17)*w(0)
 w(104) = u(18)*w(0)
 w(105) = u(19)*w(0)
endif
! terms of degree 4
if (4.le.mxd) then
 w(106:183) = u(0)*w(28:105)
 w(184:239) = u(1)*w(50:105)
 w(240:278) = u(2)*w(67:105)
 w(279:304) = u(3)*w(80:105)
 w(305:320) = u(4)*w(90:105)
 w(321:327) = u(5)*w(21:27)
 w(328:333) = u(6)*w(22:27)
 w(334:338) = u(7)*w(23:27)
 w(339:342) = u(8)*w(24:27)
 w(343:345) = u(9)*w(25:27)
 w(346:347) = u(10)*w(26:27)
 w(348) = u(11)*w(27)
 w(349) = u(20)*w(0)
 w(350) = u(21)*w(0)
 w(351) = u(22)*w(0)
 w(352) = u(23)*w(0)
 w(353) = u(24)*w(0)
 w(354) = u(25)*w(0)
endif
! terms of degree 5
if (5.le.mxd) then
 w(355:603) = u(0)*w(106:354)
 w(604:774) = u(1)*w(184:354)
 w(775:889) = u(2)*w(240:354)
 w(890:965) = u(3)*w(279:354)
 w(966:1015) = u(4)*w(305:354)
 w(1016:1023) = u(5)*w(98:105)
 w(1024:1031) = u(6)*w(98:105)
 w(1032:1039) = u(7)*w(98:105)
 w(1040:1047) = u(8)*w(98:105)
 w(1048:1055) = u(9)*w(98:105)
 w(1056:1063) = u(10)*w(98:105)
 w(1064:1071) = u(11)*w(98:105)
 w(1072) = u(26)*w(0)
 w(1073) = u(27)*w(0)
 w(1074) = u(28)*w(0)
 w(1075) = u(29)*w(0)
 w(1076) = u(30)*w(0)
endif
! terms of degree 6
if (6.le.mxd) then
 w(1077:1798) = u(0)*w(355:1076)
 w(1799:2271) = u(1)*w(604:1076)
 w(2272:2573) = u(2)*w(775:1076)
 w(2574:2760) = u(3)*w(890:1076)
 w(2761:2871) = u(4)*w(966:1076)
 w(2872:2905) = u(5)*w(321:354)
 w(2906:2932) = u(6)*w(328:354)
 w(2933:2953) = u(7)*w(334:354)
 w(2954:2969) = u(8)*w(339:354)
 w(2970:2981) = u(9)*w(343:354)
 w(2982:2990) = u(10)*w(346:354)
 w(2991:2997) = u(11)*w(348:354)
 w(2998:3005) = u(12)*w(98:105)
 w(3006:3012) = u(13)*w(99:105)
 w(3013:3018) = u(14)*w(100:105)
 w(3019:3023) = u(15)*w(101:105)
 w(3024:3027) = u(16)*w(102:105)
 w(3028:3030) = u(17)*w(103:105)
 w(3031:3032) = u(18)*w(104:105)
 w(3033) = u(19)*w(105)
 w(3034) = u(31)*w(0)
 w(3035) = u(32)*w(0)
 w(3036) = u(33)*w(0)
 w(3037) = u(34)*w(0)
 w(3038) = u(35)*w(0)
endif
! terms of degree 7
if (7.le.mxd) then
 w(3039:5000) = u(0)*w(1077:3038)
 w(5001:6240) = u(1)*w(1799:3038)
 w(6241:7007) = u(2)*w(2272:3038)
 w(7008:7472) = u(3)*w(2574:3038)
 w(7473:7750) = u(4)*w(2761:3038)
 w(7751:7811) = u(5)*w(1016:1076)
 w(7812:7864) = u(6)*w(1024:1076)
 w(7865:7909) = u(7)*w(1032:1076)
 w(7910:7946) = u(8)*w(1040:1076)
 w(7947:7975) = u(9)*w(1048:1076)
 w(7976:7996) = u(10)*w(1056:1076)
 w(7997:8009) = u(11)*w(1064:1076)
 w(8010:8015) = u(12)*w(349:354)
 w(8016:8021) = u(13)*w(349:354)
 w(8022:8027) = u(14)*w(349:354)
 w(8028:8033) = u(15)*w(349:354)
 w(8034:8039) = u(16)*w(349:354)
 w(8040:8045) = u(17)*w(349:354)
 w(8046:8051) = u(18)*w(349:354)
 w(8052:8057) = u(19)*w(349:354)
 w(8058) = u(36)*w(0)
 w(8059) = u(37)*w(0)
 w(8060) = u(38)*w(0)
 w(8061) = u(39)*w(0)
 w(8062) = u(40)*w(0)
endif
! terms of degree 8
if (8.le.mxd) then
 w(8063:13086) = u(0)*w(3039:8062)
 w(13087:16148) = u(1)*w(5001:8062)
 w(16149:17970) = u(2)*w(6241:8062)
 w(17971:19025) = u(3)*w(7008:8062)
 w(19026:19615) = u(4)*w(7473:8062)
 w(19616:19782) = u(5)*w(2872:3038)
 w(19783:19915) = u(6)*w(2906:3038)
 w(19916:20021) = u(7)*w(2933:3038)
 w(20022:20106) = u(8)*w(2954:3038)
 w(20107:20175) = u(9)*w(2970:3038)
 w(20176:20232) = u(10)*w(2982:3038)
 w(20233:20280) = u(11)*w(2991:3038)
 w(20281:20285) = u(12)*w(1072:1076)
 w(20286:20290) = u(13)*w(1072:1076)
 w(20291:20295) = u(14)*w(1072:1076)
 w(20296:20300) = u(15)*w(1072:1076)
 w(20301:20305) = u(16)*w(1072:1076)
 w(20306:20310) = u(17)*w(1072:1076)
 w(20311:20315) = u(18)*w(1072:1076)
 w(20316:20320) = u(19)*w(1072:1076)
 w(20321:20326) = u(20)*w(349:354)
 w(20327:20331) = u(21)*w(350:354)
 w(20332:20335) = u(22)*w(351:354)
 w(20336:20338) = u(23)*w(352:354)
 w(20339:20340) = u(24)*w(353:354)
 w(20341) = u(25)*w(354)
endif
! terms of degree 9
if (9.le.mxd) then
 w(20342:32620) = u(0)*w(8063:20341)
 w(32621:39875) = u(1)*w(13087:20341)
 w(39876:44068) = u(2)*w(16149:20341)
 w(44069:46439) = u(3)*w(17971:20341)
 w(46440:47755) = u(4)*w(19026:20341)
 w(47756:48067) = u(5)*w(7751:8062)
 w(48068:48318) = u(6)*w(7812:8062)
 w(48319:48516) = u(7)*w(7865:8062)
 w(48517:48669) = u(8)*w(7910:8062)
 w(48670:48785) = u(9)*w(7947:8062)
 w(48786:48872) = u(10)*w(7976:8062)
 w(48873:48938) = u(11)*w(7997:8062)
 w(48939:48979) = u(12)*w(2998:3038)
 w(48980:49012) = u(13)*w(3006:3038)
 w(49013:49038) = u(14)*w(3013:3038)
 w(49039:49058) = u(15)*w(3019:3038)
 w(49059:49073) = u(16)*w(3024:3038)
 w(49074:49084) = u(17)*w(3028:3038)
 w(49085:49092) = u(18)*w(3031:3038)
 w(49093:49098) = u(19)*w(3033:3038)
 w(49099:49103) = u(20)*w(1072:1076)
 w(49104:49108) = u(21)*w(1072:1076)
 w(49109:49113) = u(22)*w(1072:1076)
 w(49114:49118) = u(23)*w(1072:1076)
 w(49119:49123) = u(24)*w(1072:1076)
 w(49124:49128) = u(25)*w(1072:1076)
endif
END SUBROUTINE mg721_prib
