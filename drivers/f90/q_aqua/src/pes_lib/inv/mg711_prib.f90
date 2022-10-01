SUBROUTINE mg711_prib (mxd, u, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: u(0:)
real (kind=wp), intent (out) :: w(0:mg711_npb(mxd)-1)
!-----------------------------------------------------------------------
if (size(u).ne.nr) then
 stop 'mg711_prib: bad size u'
!! else if (size(w).ne.mg711_npb(mxd)) then
!!  stop 'mg711_prib: bad size w'
endif
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 4 4 7 5 5 4 5 0 0
! prib,  dnpb(0:*): 1 4 14 43 118 299 718 1637 3581 7562
! constant term
w(0) = 1
! terms of degree 1
if (1.le.mxd) then
 w(1) = u(0)*w(0)
 w(2) = u(1)*w(0)
 w(3) = u(2)*w(0)
 w(4) = u(3)*w(0)
endif
! terms of degree 2
if (2.le.mxd) then
 w(5:8) = u(0)*w(1:4)
 w(9:11) = u(1)*w(2:4)
 w(12:13) = u(2)*w(3:4)
 w(14) = u(3)*w(4)
 w(15) = u(4)*w(0)
 w(16) = u(5)*w(0)
 w(17) = u(6)*w(0)
 w(18) = u(7)*w(0)
endif
! terms of degree 3
if (3.le.mxd) then
 w(19:32) = u(0)*w(5:18)
 w(33:42) = u(1)*w(9:18)
 w(43:49) = u(2)*w(12:18)
 w(50:54) = u(3)*w(14:18)
 w(55) = u(8)*w(0)
 w(56) = u(9)*w(0)
 w(57) = u(10)*w(0)
 w(58) = u(11)*w(0)
 w(59) = u(12)*w(0)
 w(60) = u(13)*w(0)
 w(61) = u(14)*w(0)
endif
! terms of degree 4
if (4.le.mxd) then
 w(62:104) = u(0)*w(19:61)
 w(105:133) = u(1)*w(33:61)
 w(134:152) = u(2)*w(43:61)
 w(153:164) = u(3)*w(50:61)
 w(165:168) = u(4)*w(15:18)
 w(169:171) = u(5)*w(16:18)
 w(172:173) = u(6)*w(17:18)
 w(174) = u(7)*w(18)
 w(175) = u(15)*w(0)
 w(176) = u(16)*w(0)
 w(177) = u(17)*w(0)
 w(178) = u(18)*w(0)
 w(179) = u(19)*w(0)
endif
! terms of degree 5
if (5.le.mxd) then
 w(180:297) = u(0)*w(62:179)
 w(298:372) = u(1)*w(105:179)
 w(373:418) = u(2)*w(134:179)
 w(419:445) = u(3)*w(153:179)
 w(446:452) = u(4)*w(55:61)
 w(453:459) = u(5)*w(55:61)
 w(460:466) = u(6)*w(55:61)
 w(467:473) = u(7)*w(55:61)
 w(474) = u(20)*w(0)
 w(475) = u(21)*w(0)
 w(476) = u(22)*w(0)
 w(477) = u(23)*w(0)
 w(478) = u(24)*w(0)
endif
! terms of degree 6
if (6.le.mxd) then
 w(479:777) = u(0)*w(180:478)
 w(778:958) = u(1)*w(298:478)
 w(959:1064) = u(2)*w(373:478)
 w(1065:1124) = u(3)*w(419:478)
 w(1125:1139) = u(4)*w(165:179)
 w(1140:1150) = u(5)*w(169:179)
 w(1151:1158) = u(6)*w(172:179)
 w(1159:1164) = u(7)*w(174:179)
 w(1165:1171) = u(8)*w(55:61)
 w(1172:1177) = u(9)*w(56:61)
 w(1178:1182) = u(10)*w(57:61)
 w(1183:1186) = u(11)*w(58:61)
 w(1187:1189) = u(12)*w(59:61)
 w(1190:1191) = u(13)*w(60:61)
 w(1192) = u(14)*w(61)
 w(1193) = u(25)*w(0)
 w(1194) = u(26)*w(0)
 w(1195) = u(27)*w(0)
 w(1196) = u(28)*w(0)
endif
! terms of degree 7
if (7.le.mxd) then
 w(1197:1914) = u(0)*w(479:1196)
 w(1915:2333) = u(1)*w(778:1196)
 w(2334:2571) = u(2)*w(959:1196)
 w(2572:2703) = u(3)*w(1065:1196)
 w(2704:2736) = u(4)*w(446:478)
 w(2737:2762) = u(5)*w(453:478)
 w(2763:2781) = u(6)*w(460:478)
 w(2782:2793) = u(7)*w(467:478)
 w(2794:2798) = u(8)*w(175:179)
 w(2799:2803) = u(9)*w(175:179)
 w(2804:2808) = u(10)*w(175:179)
 w(2809:2813) = u(11)*w(175:179)
 w(2814:2818) = u(12)*w(175:179)
 w(2819:2823) = u(13)*w(175:179)
 w(2824:2828) = u(14)*w(175:179)
 w(2829) = u(29)*w(0)
 w(2830) = u(30)*w(0)
 w(2831) = u(31)*w(0)
 w(2832) = u(32)*w(0)
 w(2833) = u(33)*w(0)
endif
! terms of degree 8
if (8.le.mxd) then
 w(2834:4470) = u(0)*w(1197:2833)
 w(4471:5389) = u(1)*w(1915:2833)
 w(5390:5889) = u(2)*w(2334:2833)
 w(5890:6151) = u(3)*w(2572:2833)
 w(6152:6223) = u(4)*w(1125:1196)
 w(6224:6280) = u(5)*w(1140:1196)
 w(6281:6326) = u(6)*w(1151:1196)
 w(6327:6364) = u(7)*w(1159:1196)
 w(6365:6369) = u(8)*w(474:478)
 w(6370:6374) = u(9)*w(474:478)
 w(6375:6379) = u(10)*w(474:478)
 w(6380:6384) = u(11)*w(474:478)
 w(6385:6389) = u(12)*w(474:478)
 w(6390:6394) = u(13)*w(474:478)
 w(6395:6399) = u(14)*w(474:478)
 w(6400:6404) = u(15)*w(175:179)
 w(6405:6408) = u(16)*w(176:179)
 w(6409:6411) = u(17)*w(177:179)
 w(6412:6413) = u(18)*w(178:179)
 w(6414) = u(19)*w(179)
endif
! terms of degree 9
if (9.le.mxd) then
 w(6415:9995) = u(0)*w(2834:6414)
 w(9996:11939) = u(1)*w(4471:6414)
 w(11940:12964) = u(2)*w(5390:6414)
 w(12965:13489) = u(3)*w(5890:6414)
 w(13490:13619) = u(4)*w(2704:2833)
 w(13620:13716) = u(5)*w(2737:2833)
 w(13717:13787) = u(6)*w(2763:2833)
 w(13788:13839) = u(7)*w(2782:2833)
 w(13840:13871) = u(8)*w(1165:1196)
 w(13872:13896) = u(9)*w(1172:1196)
 w(13897:13915) = u(10)*w(1178:1196)
 w(13916:13929) = u(11)*w(1183:1196)
 w(13930:13939) = u(12)*w(1187:1196)
 w(13940:13946) = u(13)*w(1190:1196)
 w(13947:13951) = u(14)*w(1192:1196)
 w(13952:13956) = u(15)*w(474:478)
 w(13957:13961) = u(16)*w(474:478)
 w(13962:13966) = u(17)*w(474:478)
 w(13967:13971) = u(18)*w(474:478)
 w(13972:13976) = u(19)*w(474:478)
endif
END SUBROUTINE mg711_prib