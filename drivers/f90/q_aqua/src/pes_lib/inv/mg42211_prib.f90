SUBROUTINE mg42211_prib (mxd, u, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: u(0:)
real (kind=wp), intent (out) :: w(0:mg42211_npb(mxd)-1)
!-----------------------------------------------------------------------
if (size(u).ne.nr) then
 stop 'mg42211_prib: bad size u'
!! else if (size(w).ne.mg42211_npb(mxd)) then
!!  stop 'mg42211_prib: bad size w'
endif
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 13 17 6 7 0 2 0 0 0
! prib,  dnpb(0:*): 1 13 108 682 3605 16651 69231 263989 936212 3119446
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
 w(13) = u(12)*w(0)
endif
! terms of degree 2
if (2.le.mxd) then
 w(14:26) = u(0)*w(1:13)
 w(27:38) = u(1)*w(2:13)
 w(39:49) = u(2)*w(3:13)
 w(50:59) = u(3)*w(4:13)
 w(60:68) = u(4)*w(5:13)
 w(69:76) = u(5)*w(6:13)
 w(77:83) = u(6)*w(7:13)
 w(84:89) = u(7)*w(8:13)
 w(90:94) = u(8)*w(9:13)
 w(95:98) = u(9)*w(10:13)
 w(99:101) = u(10)*w(11:13)
 w(102:103) = u(11)*w(12:13)
 w(104) = u(12)*w(13)
 w(105) = u(13)*w(0)
 w(106) = u(14)*w(0)
 w(107) = u(15)*w(0)
 w(108) = u(16)*w(0)
 w(109) = u(17)*w(0)
 w(110) = u(18)*w(0)
 w(111) = u(19)*w(0)
 w(112) = u(20)*w(0)
 w(113) = u(21)*w(0)
 w(114) = u(22)*w(0)
 w(115) = u(23)*w(0)
 w(116) = u(24)*w(0)
 w(117) = u(25)*w(0)
 w(118) = u(26)*w(0)
 w(119) = u(27)*w(0)
 w(120) = u(28)*w(0)
 w(121) = u(29)*w(0)
endif
! terms of degree 3
if (3.le.mxd) then
 w(122:229) = u(0)*w(14:121)
 w(230:324) = u(1)*w(27:121)
 w(325:407) = u(2)*w(39:121)
 w(408:479) = u(3)*w(50:121)
 w(480:541) = u(4)*w(60:121)
 w(542:594) = u(5)*w(69:121)
 w(595:639) = u(6)*w(77:121)
 w(640:677) = u(7)*w(84:121)
 w(678:709) = u(8)*w(90:121)
 w(710:736) = u(9)*w(95:121)
 w(737:759) = u(10)*w(99:121)
 w(760:779) = u(11)*w(102:121)
 w(780:797) = u(12)*w(104:121)
 w(798) = u(30)*w(0)
 w(799) = u(31)*w(0)
 w(800) = u(32)*w(0)
 w(801) = u(33)*w(0)
 w(802) = u(34)*w(0)
 w(803) = u(35)*w(0)
endif
! terms of degree 4
if (4.le.mxd) then
 w(804:1485) = u(0)*w(122:803)
 w(1486:2059) = u(1)*w(230:803)
 w(2060:2538) = u(2)*w(325:803)
 w(2539:2934) = u(3)*w(408:803)
 w(2935:3258) = u(4)*w(480:803)
 w(3259:3520) = u(5)*w(542:803)
 w(3521:3729) = u(6)*w(595:803)
 w(3730:3893) = u(7)*w(640:803)
 w(3894:4019) = u(8)*w(678:803)
 w(4020:4113) = u(9)*w(710:803)
 w(4114:4180) = u(10)*w(737:803)
 w(4181:4224) = u(11)*w(760:803)
 w(4225:4248) = u(12)*w(780:803)
 w(4249:4265) = u(13)*w(105:121)
 w(4266:4281) = u(14)*w(106:121)
 w(4282:4296) = u(15)*w(107:121)
 w(4297:4310) = u(16)*w(108:121)
 w(4311:4323) = u(17)*w(109:121)
 w(4324:4335) = u(18)*w(110:121)
 w(4336:4346) = u(19)*w(111:121)
 w(4347:4356) = u(20)*w(112:121)
 w(4357:4365) = u(21)*w(113:121)
 w(4366:4373) = u(22)*w(114:121)
 w(4374:4380) = u(23)*w(115:121)
 w(4381:4386) = u(24)*w(116:121)
 w(4387:4391) = u(25)*w(117:121)
 w(4392:4395) = u(26)*w(118:121)
 w(4396:4398) = u(27)*w(119:121)
 w(4399:4400) = u(28)*w(120:121)
 w(4401) = u(29)*w(121)
 w(4402) = u(36)*w(0)
 w(4403) = u(37)*w(0)
 w(4404) = u(38)*w(0)
 w(4405) = u(39)*w(0)
 w(4406) = u(40)*w(0)
 w(4407) = u(41)*w(0)
 w(4408) = u(42)*w(0)
endif
! terms of degree 5
if (5.le.mxd) then
 w(4409:8013) = u(0)*w(804:4408)
 w(8014:10936) = u(1)*w(1486:4408)
 w(10937:13285) = u(2)*w(2060:4408)
 w(13286:15155) = u(3)*w(2539:4408)
 w(15156:16629) = u(4)*w(2935:4408)
 w(16630:17779) = u(5)*w(3259:4408)
 w(17780:18667) = u(6)*w(3521:4408)
 w(18668:19346) = u(7)*w(3730:4408)
 w(19347:19861) = u(8)*w(3894:4408)
 w(19862:20250) = u(9)*w(4020:4408)
 w(20251:20545) = u(10)*w(4114:4408)
 w(20546:20773) = u(11)*w(4181:4408)
 w(20774:20957) = u(12)*w(4225:4408)
 w(20958:20963) = u(13)*w(798:803)
 w(20964:20969) = u(14)*w(798:803)
 w(20970:20975) = u(15)*w(798:803)
 w(20976:20981) = u(16)*w(798:803)
 w(20982:20987) = u(17)*w(798:803)
 w(20988:20993) = u(18)*w(798:803)
 w(20994:20999) = u(19)*w(798:803)
 w(21000:21005) = u(20)*w(798:803)
 w(21006:21011) = u(21)*w(798:803)
 w(21012:21017) = u(22)*w(798:803)
 w(21018:21023) = u(23)*w(798:803)
 w(21024:21029) = u(24)*w(798:803)
 w(21030:21035) = u(25)*w(798:803)
 w(21036:21041) = u(26)*w(798:803)
 w(21042:21047) = u(27)*w(798:803)
 w(21048:21053) = u(28)*w(798:803)
 w(21054:21059) = u(29)*w(798:803)
endif
! terms of degree 6
if (6.le.mxd) then
 w(21060:37710) = u(0)*w(4409:21059)
 w(37711:50756) = u(1)*w(8014:21059)
 w(50757:60879) = u(2)*w(10937:21059)
 w(60880:68653) = u(3)*w(13286:21059)
 w(68654:74557) = u(4)*w(15156:21059)
 w(74558:78987) = u(5)*w(16630:21059)
 w(78988:82267) = u(6)*w(17780:21059)
 w(82268:84659) = u(7)*w(18668:21059)
 w(84660:86372) = u(8)*w(19347:21059)
 w(86373:87570) = u(9)*w(19862:21059)
 w(87571:88379) = u(10)*w(20251:21059)
 w(88380:88893) = u(11)*w(20546:21059)
 w(88894:89179) = u(12)*w(20774:21059)
 w(89180:89339) = u(13)*w(4249:4408)
 w(89340:89482) = u(14)*w(4266:4408)
 w(89483:89609) = u(15)*w(4282:4408)
 w(89610:89721) = u(16)*w(4297:4408)
 w(89722:89819) = u(17)*w(4311:4408)
 w(89820:89904) = u(18)*w(4324:4408)
 w(89905:89977) = u(19)*w(4336:4408)
 w(89978:90039) = u(20)*w(4347:4408)
 w(90040:90091) = u(21)*w(4357:4408)
 w(90092:90134) = u(22)*w(4366:4408)
 w(90135:90169) = u(23)*w(4374:4408)
 w(90170:90197) = u(24)*w(4381:4408)
 w(90198:90219) = u(25)*w(4387:4408)
 w(90220:90236) = u(26)*w(4392:4408)
 w(90237:90249) = u(27)*w(4396:4408)
 w(90250:90259) = u(28)*w(4399:4408)
 w(90260:90267) = u(29)*w(4401:4408)
 w(90268:90273) = u(30)*w(798:803)
 w(90274:90278) = u(31)*w(799:803)
 w(90279:90282) = u(32)*w(800:803)
 w(90283:90285) = u(33)*w(801:803)
 w(90286:90287) = u(34)*w(802:803)
 w(90288) = u(35)*w(803)
 w(90289) = u(43)*w(0)
 w(90290) = u(44)*w(0)
endif
! terms of degree 7
if (7.le.mxd) then
 w(90291:159521) = u(0)*w(21060:90290)
 w(159522:212101) = u(1)*w(37711:90290)
 w(212102:251635) = u(2)*w(50757:90290)
 w(251636:281046) = u(3)*w(60880:90290)
 w(281047:302683) = u(4)*w(68654:90290)
 w(302684:318416) = u(5)*w(74558:90290)
 w(318417:329719) = u(6)*w(78988:90290)
 w(329720:337742) = u(7)*w(82268:90290)
 w(337743:343373) = u(8)*w(84660:90290)
 w(343374:347291) = u(9)*w(86373:90290)
 w(347292:350011) = u(10)*w(87571:90290)
 w(350012:351922) = u(11)*w(88380:90290)
 w(351923:353319) = u(12)*w(88894:90290)
 w(353320:353421) = u(13)*w(20958:21059)
 w(353422:353517) = u(14)*w(20964:21059)
 w(353518:353607) = u(15)*w(20970:21059)
 w(353608:353691) = u(16)*w(20976:21059)
 w(353692:353769) = u(17)*w(20982:21059)
 w(353770:353841) = u(18)*w(20988:21059)
 w(353842:353907) = u(19)*w(20994:21059)
 w(353908:353967) = u(20)*w(21000:21059)
 w(353968:354021) = u(21)*w(21006:21059)
 w(354022:354069) = u(22)*w(21012:21059)
 w(354070:354111) = u(23)*w(21018:21059)
 w(354112:354147) = u(24)*w(21024:21059)
 w(354148:354177) = u(25)*w(21030:21059)
 w(354178:354201) = u(26)*w(21036:21059)
 w(354202:354219) = u(27)*w(21042:21059)
 w(354220:354231) = u(28)*w(21048:21059)
 w(354232:354237) = u(29)*w(21054:21059)
 w(354238:354244) = u(30)*w(4402:4408)
 w(354245:354251) = u(31)*w(4402:4408)
 w(354252:354258) = u(32)*w(4402:4408)
 w(354259:354265) = u(33)*w(4402:4408)
 w(354266:354272) = u(34)*w(4402:4408)
 w(354273:354279) = u(35)*w(4402:4408)
endif
! terms of degree 8
if (8.le.mxd) then
 w(354280:618268) = u(0)*w(90291:354279)
 w(618269:813026) = u(1)*w(159522:354279)
 w(813027:955204) = u(2)*w(212102:354279)
 w(955205:1057848) = u(3)*w(251636:354279)
 w(1057849:1131081) = u(4)*w(281047:354279)
 w(1131082:1182677) = u(5)*w(302684:354279)
 w(1182678:1218540) = u(6)*w(318417:354279)
 w(1218541:1243100) = u(7)*w(329720:354279)
 w(1243101:1259637) = u(8)*w(337743:354279)
 w(1259638:1270543) = u(9)*w(343374:354279)
 w(1270544:1277531) = u(10)*w(347292:354279)
 w(1277532:1281799) = u(11)*w(350012:354279)
 w(1281800:1284156) = u(12)*w(351923:354279)
 w(1284157:1285267) = u(13)*w(89180:90290)
 w(1285268:1286218) = u(14)*w(89340:90290)
 w(1286219:1287026) = u(15)*w(89483:90290)
 w(1287027:1287707) = u(16)*w(89610:90290)
 w(1287708:1288276) = u(17)*w(89722:90290)
 w(1288277:1288747) = u(18)*w(89820:90290)
 w(1288748:1289133) = u(19)*w(89905:90290)
 w(1289134:1289446) = u(20)*w(89978:90290)
 w(1289447:1289697) = u(21)*w(90040:90290)
 w(1289698:1289896) = u(22)*w(90092:90290)
 w(1289897:1290052) = u(23)*w(90135:90290)
 w(1290053:1290173) = u(24)*w(90170:90290)
 w(1290174:1290266) = u(25)*w(90198:90290)
 w(1290267:1290337) = u(26)*w(90220:90290)
 w(1290338:1290391) = u(27)*w(90237:90290)
 w(1290392:1290432) = u(28)*w(90250:90290)
 w(1290433:1290463) = u(29)*w(90260:90290)
 w(1290464:1290470) = u(36)*w(4402:4408)
 w(1290471:1290476) = u(37)*w(4403:4408)
 w(1290477:1290481) = u(38)*w(4404:4408)
 w(1290482:1290485) = u(39)*w(4405:4408)
 w(1290486:1290488) = u(40)*w(4406:4408)
 w(1290489:1290490) = u(41)*w(4407:4408)
 w(1290491) = u(42)*w(4408)
endif
! terms of degree 9
if (9.le.mxd) then
 w(1290492:2226703) = u(0)*w(354280:1290491)
 w(2226704:2898926) = u(1)*w(618269:1290491)
 w(2898927:3376391) = u(2)*w(813027:1290491)
 w(3376392:3711678) = u(3)*w(955205:1290491)
 w(3711679:3944321) = u(4)*w(1057849:1290491)
 w(3944322:4103731) = u(5)*w(1131082:1290491)
 w(4103732:4211545) = u(6)*w(1182678:1290491)
 w(4211546:4283496) = u(7)*w(1218541:1290491)
 w(4283497:4330887) = u(8)*w(1243101:1290491)
 w(4330888:4361741) = u(9)*w(1259638:1290491)
 w(4361742:4381689) = u(10)*w(1270544:1290491)
 w(4381690:4394649) = u(11)*w(1277532:1290491)
 w(4394650:4403341) = u(12)*w(1281800:1290491)
 w(4403342:4404301) = u(13)*w(353320:354279)
 w(4404302:4405159) = u(14)*w(353422:354279)
 w(4405160:4405921) = u(15)*w(353518:354279)
 w(4405922:4406593) = u(16)*w(353608:354279)
 w(4406594:4407181) = u(17)*w(353692:354279)
 w(4407182:4407691) = u(18)*w(353770:354279)
 w(4407692:4408129) = u(19)*w(353842:354279)
 w(4408130:4408501) = u(20)*w(353908:354279)
 w(4408502:4408813) = u(21)*w(353968:354279)
 w(4408814:4409071) = u(22)*w(354022:354279)
 w(4409072:4409281) = u(23)*w(354070:354279)
 w(4409282:4409449) = u(24)*w(354112:354279)
 w(4409450:4409581) = u(25)*w(354148:354279)
 w(4409582:4409683) = u(26)*w(354178:354279)
 w(4409684:4409761) = u(27)*w(354202:354279)
 w(4409762:4409821) = u(28)*w(354220:354279)
 w(4409822:4409869) = u(29)*w(354232:354279)
 w(4409870:4409892) = u(30)*w(90268:90290)
 w(4409893:4409909) = u(31)*w(90274:90290)
 w(4409910:4409921) = u(32)*w(90279:90290)
 w(4409922:4409929) = u(33)*w(90283:90290)
 w(4409930:4409934) = u(34)*w(90286:90290)
 w(4409935:4409937) = u(35)*w(90288:90290)
endif
END SUBROUTINE mg42211_prib