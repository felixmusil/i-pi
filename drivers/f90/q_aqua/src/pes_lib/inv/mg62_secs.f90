SUBROUTINE mg62_secs (mxd, r, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: v(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: pv(0:size(v)-1) !! an upper bound size
if (size(r).ne.nk*nk.or.size(v).ne.mg62_nsc(mxd)) then
 stop 'mg62_secs: bad dimensions'
endif
call mg62_isecs (mxd, r, pv)
v(0) = 1
if (2.le.mxd) then
 v(1:1) = pv(0:0)
endif
if (3.le.mxd) then
 v(2:11) = pv(1:10)
endif
if (4.le.mxd) then
 v(12) = pv(0)*pv(0)
 v(13:55) = pv(11:53)
endif
if (5.le.mxd) then
 v(56) = pv(0)*pv(1)
 v(57) = pv(0)*pv(2)
 v(58) = pv(0)*pv(3)
 v(59) = pv(0)*pv(4)
 v(60) = pv(0)*pv(5)
 v(61) = pv(0)*pv(6)
 v(62) = pv(0)*pv(7)
 v(63) = pv(0)*pv(8)
 v(64) = pv(0)*pv(9)
 v(65) = pv(0)*pv(10)
 v(66:208) = pv(54:196)
endif
if (6.le.mxd) then
 v(209) = pv(0)*pv(0)*pv(0)
 v(210) = pv(1)*pv(1)
 v(211) = pv(1)*pv(2)
 v(212) = pv(2)*pv(2)
 v(213) = pv(1)*pv(3)
 v(214) = pv(2)*pv(3)
 v(215) = pv(3)*pv(3)
 v(216) = pv(1)*pv(4)
 v(217) = pv(2)*pv(4)
 v(218) = pv(3)*pv(4)
 v(219) = pv(4)*pv(4)
 v(220) = pv(1)*pv(5)
 v(221) = pv(2)*pv(5)
 v(222) = pv(3)*pv(5)
 v(223) = pv(4)*pv(5)
 v(224) = pv(1)*pv(6)
 v(225) = pv(2)*pv(6)
 v(226) = pv(3)*pv(6)
 v(227) = pv(4)*pv(6)
 v(228) = pv(5)*pv(6)
 v(229) = pv(6)*pv(6)
 v(230) = pv(1)*pv(7)
 v(231) = pv(2)*pv(7)
 v(232) = pv(3)*pv(7)
 v(233) = pv(4)*pv(7)
 v(234) = pv(5)*pv(7)
 v(235) = pv(6)*pv(7)
 v(236) = pv(7)*pv(7)
 v(237) = pv(1)*pv(8)
 v(238) = pv(2)*pv(8)
 v(239) = pv(3)*pv(8)
 v(240) = pv(4)*pv(8)
 v(241) = pv(6)*pv(8)
 v(242) = pv(7)*pv(8)
 v(243) = pv(1)*pv(9)
 v(244) = pv(2)*pv(9)
 v(245) = pv(3)*pv(9)
 v(246) = pv(4)*pv(9)
 v(247) = pv(5)*pv(9)
 v(248) = pv(6)*pv(9)
 v(249) = pv(7)*pv(9)
 v(250) = pv(8)*pv(9)
 v(251) = pv(9)*pv(9)
 v(252) = pv(1)*pv(10)
 v(253) = pv(2)*pv(10)
 v(254) = pv(3)*pv(10)
 v(255) = pv(4)*pv(10)
 v(256) = pv(5)*pv(10)
 v(257) = pv(6)*pv(10)
 v(258) = pv(7)*pv(10)
 v(259) = pv(8)*pv(10)
 v(260) = pv(9)*pv(10)
 v(261) = pv(10)*pv(10)
 v(262) = pv(0)*pv(11)
 v(263) = pv(0)*pv(12)
 v(264) = pv(0)*pv(13)
 v(265) = pv(0)*pv(14)
 v(266) = pv(0)*pv(15)
 v(267) = pv(0)*pv(16)
 v(268) = pv(0)*pv(17)
 v(269) = pv(0)*pv(18)
 v(270) = pv(0)*pv(19)
 v(271) = pv(0)*pv(20)
 v(272) = pv(0)*pv(21)
 v(273) = pv(0)*pv(22)
 v(274) = pv(0)*pv(23)
 v(275) = pv(0)*pv(24)
 v(276) = pv(0)*pv(25)
 v(277) = pv(0)*pv(26)
 v(278) = pv(0)*pv(27)
 v(279) = pv(0)*pv(28)
 v(280) = pv(0)*pv(29)
 v(281) = pv(0)*pv(30)
 v(282) = pv(0)*pv(31)
 v(283) = pv(0)*pv(32)
 v(284) = pv(0)*pv(33)
 v(285) = pv(0)*pv(34)
 v(286) = pv(0)*pv(35)
 v(287) = pv(0)*pv(36)
 v(288) = pv(0)*pv(37)
 v(289) = pv(0)*pv(38)
 v(290) = pv(0)*pv(39)
 v(291) = pv(0)*pv(40)
 v(292) = pv(0)*pv(41)
 v(293) = pv(0)*pv(42)
 v(294) = pv(0)*pv(43)
 v(295) = pv(0)*pv(44)
 v(296) = pv(0)*pv(45)
 v(297) = pv(0)*pv(46)
 v(298) = pv(0)*pv(47)
 v(299) = pv(0)*pv(48)
 v(300) = pv(0)*pv(49)
 v(301) = pv(0)*pv(50)
 v(302) = pv(0)*pv(51)
 v(303) = pv(0)*pv(52)
 v(304) = pv(0)*pv(53)
 v(305:749) = pv(197:641)
endif
if (7.le.mxd) then
 stop 'mg62: degree 7 not implemented'
endif
return
END SUBROUTINE mg62_secs