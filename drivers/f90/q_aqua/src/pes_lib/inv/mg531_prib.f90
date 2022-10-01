SUBROUTINE mg531_prib (mxd, u, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: u(0:)
real (kind=wp), intent (out) :: w(0:mg531_npb(mxd)-1)
!-----------------------------------------------------------------------
if (size(u).ne.nr) then
 stop 'mg531_prib: bad size u'
!! else if (size(w).ne.mg531_npb(mxd)) then
!!  stop 'mg531_prib: bad size w'
endif
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 5 8 8 6 4 2 0 0 0
! prib,  dnpb(0:*): 1 5 23 83 272 804 2226 5786 14336 33980
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
 w(28) = u(12)*w(0)
endif
! terms of degree 3
if (3.le.mxd) then
 w(29:51) = u(0)*w(6:28)
 w(52:69) = u(1)*w(11:28)
 w(70:83) = u(2)*w(15:28)
 w(84:94) = u(3)*w(18:28)
 w(95:103) = u(4)*w(20:28)
 w(104) = u(13)*w(0)
 w(105) = u(14)*w(0)
 w(106) = u(15)*w(0)
 w(107) = u(16)*w(0)
 w(108) = u(17)*w(0)
 w(109) = u(18)*w(0)
 w(110) = u(19)*w(0)
 w(111) = u(20)*w(0)
endif
! terms of degree 4
if (4.le.mxd) then
 w(112:194) = u(0)*w(29:111)
 w(195:254) = u(1)*w(52:111)
 w(255:296) = u(2)*w(70:111)
 w(297:324) = u(3)*w(84:111)
 w(325:341) = u(4)*w(95:111)
 w(342:349) = u(5)*w(21:28)
 w(350:356) = u(6)*w(22:28)
 w(357:362) = u(7)*w(23:28)
 w(363:367) = u(8)*w(24:28)
 w(368:371) = u(9)*w(25:28)
 w(372:374) = u(10)*w(26:28)
 w(375:376) = u(11)*w(27:28)
 w(377) = u(12)*w(28)
 w(378) = u(21)*w(0)
 w(379) = u(22)*w(0)
 w(380) = u(23)*w(0)
 w(381) = u(24)*w(0)
 w(382) = u(25)*w(0)
 w(383) = u(26)*w(0)
endif
! terms of degree 5
if (5.le.mxd) then
 w(384:655) = u(0)*w(112:383)
 w(656:844) = u(1)*w(195:383)
 w(845:973) = u(2)*w(255:383)
 w(974:1060) = u(3)*w(297:383)
 w(1061:1119) = u(4)*w(325:383)
 w(1120:1127) = u(5)*w(104:111)
 w(1128:1135) = u(6)*w(104:111)
 w(1136:1143) = u(7)*w(104:111)
 w(1144:1151) = u(8)*w(104:111)
 w(1152:1159) = u(9)*w(104:111)
 w(1160:1167) = u(10)*w(104:111)
 w(1168:1175) = u(11)*w(104:111)
 w(1176:1183) = u(12)*w(104:111)
 w(1184) = u(27)*w(0)
 w(1185) = u(28)*w(0)
 w(1186) = u(29)*w(0)
 w(1187) = u(30)*w(0)
endif
! terms of degree 6
if (6.le.mxd) then
 w(1188:1991) = u(0)*w(384:1187)
 w(1992:2523) = u(1)*w(656:1187)
 w(2524:2866) = u(2)*w(845:1187)
 w(2867:3080) = u(3)*w(974:1187)
 w(3081:3207) = u(4)*w(1061:1187)
 w(3208:3249) = u(5)*w(342:383)
 w(3250:3283) = u(6)*w(350:383)
 w(3284:3310) = u(7)*w(357:383)
 w(3311:3331) = u(8)*w(363:383)
 w(3332:3347) = u(9)*w(368:383)
 w(3348:3359) = u(10)*w(372:383)
 w(3360:3368) = u(11)*w(375:383)
 w(3369:3375) = u(12)*w(377:383)
 w(3376:3383) = u(13)*w(104:111)
 w(3384:3390) = u(14)*w(105:111)
 w(3391:3396) = u(15)*w(106:111)
 w(3397:3401) = u(16)*w(107:111)
 w(3402:3405) = u(17)*w(108:111)
 w(3406:3408) = u(18)*w(109:111)
 w(3409:3410) = u(19)*w(110:111)
 w(3411) = u(20)*w(111)
 w(3412) = u(31)*w(0)
 w(3413) = u(32)*w(0)
endif
! terms of degree 7
if (7.le.mxd) then
 w(3414:5639) = u(0)*w(1188:3413)
 w(5640:7061) = u(1)*w(1992:3413)
 w(7062:7951) = u(2)*w(2524:3413)
 w(7952:8498) = u(3)*w(2867:3413)
 w(8499:8831) = u(4)*w(3081:3413)
 w(8832:8899) = u(5)*w(1120:1187)
 w(8900:8959) = u(6)*w(1128:1187)
 w(8960:9011) = u(7)*w(1136:1187)
 w(9012:9055) = u(8)*w(1144:1187)
 w(9056:9091) = u(9)*w(1152:1187)
 w(9092:9119) = u(10)*w(1160:1187)
 w(9120:9139) = u(11)*w(1168:1187)
 w(9140:9151) = u(12)*w(1176:1187)
 w(9152:9157) = u(13)*w(378:383)
 w(9158:9163) = u(14)*w(378:383)
 w(9164:9169) = u(15)*w(378:383)
 w(9170:9175) = u(16)*w(378:383)
 w(9176:9181) = u(17)*w(378:383)
 w(9182:9187) = u(18)*w(378:383)
 w(9188:9193) = u(19)*w(378:383)
 w(9194:9199) = u(20)*w(378:383)
endif
! terms of degree 8
if (8.le.mxd) then
 w(9200:14985) = u(0)*w(3414:9199)
 w(14986:18545) = u(1)*w(5640:9199)
 w(18546:20683) = u(2)*w(7062:9199)
 w(20684:21931) = u(3)*w(7952:9199)
 w(21932:22632) = u(4)*w(8499:9199)
 w(22633:22838) = u(5)*w(3208:3413)
 w(22839:23002) = u(6)*w(3250:3413)
 w(23003:23132) = u(7)*w(3284:3413)
 w(23133:23235) = u(8)*w(3311:3413)
 w(23236:23317) = u(9)*w(3332:3413)
 w(23318:23383) = u(10)*w(3348:3413)
 w(23384:23437) = u(11)*w(3360:3413)
 w(23438:23482) = u(12)*w(3369:3413)
 w(23483:23486) = u(13)*w(1184:1187)
 w(23487:23490) = u(14)*w(1184:1187)
 w(23491:23494) = u(15)*w(1184:1187)
 w(23495:23498) = u(16)*w(1184:1187)
 w(23499:23502) = u(17)*w(1184:1187)
 w(23503:23506) = u(18)*w(1184:1187)
 w(23507:23510) = u(19)*w(1184:1187)
 w(23511:23514) = u(20)*w(1184:1187)
 w(23515:23520) = u(21)*w(378:383)
 w(23521:23525) = u(22)*w(379:383)
 w(23526:23529) = u(23)*w(380:383)
 w(23530:23532) = u(24)*w(381:383)
 w(23533:23534) = u(25)*w(382:383)
 w(23535) = u(26)*w(383)
endif
! terms of degree 9
if (9.le.mxd) then
 w(23536:37871) = u(0)*w(9200:23535)
 w(37872:46421) = u(1)*w(14986:23535)
 w(46422:51411) = u(2)*w(18546:23535)
 w(51412:54263) = u(3)*w(20684:23535)
 w(54264:55867) = u(4)*w(21932:23535)
 w(55868:56235) = u(5)*w(8832:9199)
 w(56236:56535) = u(6)*w(8900:9199)
 w(56536:56775) = u(7)*w(8960:9199)
 w(56776:56963) = u(8)*w(9012:9199)
 w(56964:57107) = u(9)*w(9056:9199)
 w(57108:57215) = u(10)*w(9092:9199)
 w(57216:57295) = u(11)*w(9120:9199)
 w(57296:57355) = u(12)*w(9140:9199)
 w(57356:57393) = u(13)*w(3376:3413)
 w(57394:57423) = u(14)*w(3384:3413)
 w(57424:57446) = u(15)*w(3391:3413)
 w(57447:57463) = u(16)*w(3397:3413)
 w(57464:57475) = u(17)*w(3402:3413)
 w(57476:57483) = u(18)*w(3406:3413)
 w(57484:57488) = u(19)*w(3409:3413)
 w(57489:57491) = u(20)*w(3411:3413)
 w(57492:57495) = u(21)*w(1184:1187)
 w(57496:57499) = u(22)*w(1184:1187)
 w(57500:57503) = u(23)*w(1184:1187)
 w(57504:57507) = u(24)*w(1184:1187)
 w(57508:57511) = u(25)*w(1184:1187)
 w(57512:57515) = u(26)*w(1184:1187)
endif
END SUBROUTINE mg531_prib
