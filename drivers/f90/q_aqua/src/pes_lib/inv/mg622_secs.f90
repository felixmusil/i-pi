SUBROUTINE mg622_secs (mxd, r, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: v(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: pv(0:size(v)-1) !! an upper bound size
if (size(r).ne.nk*nk.or.size(v).ne.mg622_nsc(mxd)) then
 stop 'mg622_secs: bad dimensions'
endif
call mg622_isecs (mxd, r, pv)
v(0) = 1
if (2.le.mxd) then
 v(1:5) = pv(0:4)
endif
if (3.le.mxd) then
 v(6:43) = pv(5:42)
endif
if (4.le.mxd) then
 v(44) = pv(0)*pv(0)
 v(45) = pv(0)*pv(1)
 v(46) = pv(1)*pv(1)
 v(47) = pv(0)*pv(2)
 v(48) = pv(1)*pv(2)
 v(49) = pv(2)*pv(2)
 v(50) = pv(0)*pv(3)
 v(51) = pv(1)*pv(3)
 v(52) = pv(2)*pv(3)
 v(53) = pv(0)*pv(4)
 v(54) = pv(1)*pv(4)
 v(55) = pv(2)*pv(4)
 v(56) = pv(3)*pv(4)
 v(57:223) = pv(43:209)
endif
if (5.le.mxd) then
 v(224) = pv(0)*pv(5)
 v(225) = pv(1)*pv(5)
 v(226) = pv(2)*pv(5)
 v(227) = pv(3)*pv(5)
 v(228) = pv(4)*pv(5)
 v(229) = pv(0)*pv(6)
 v(230) = pv(1)*pv(6)
 v(231) = pv(2)*pv(6)
 v(232) = pv(3)*pv(6)
 v(233) = pv(4)*pv(6)
 v(234) = pv(0)*pv(7)
 v(235) = pv(1)*pv(7)
 v(236) = pv(2)*pv(7)
 v(237) = pv(3)*pv(7)
 v(238) = pv(4)*pv(7)
 v(239) = pv(0)*pv(8)
 v(240) = pv(1)*pv(8)
 v(241) = pv(2)*pv(8)
 v(242) = pv(3)*pv(8)
 v(243) = pv(4)*pv(8)
 v(244) = pv(0)*pv(9)
 v(245) = pv(1)*pv(9)
 v(246) = pv(2)*pv(9)
 v(247) = pv(4)*pv(9)
 v(248) = pv(0)*pv(10)
 v(249) = pv(1)*pv(10)
 v(250) = pv(2)*pv(10)
 v(251) = pv(3)*pv(10)
 v(252) = pv(4)*pv(10)
 v(253) = pv(0)*pv(11)
 v(254) = pv(1)*pv(11)
 v(255) = pv(2)*pv(11)
 v(256) = pv(3)*pv(11)
 v(257) = pv(4)*pv(11)
 v(258) = pv(0)*pv(12)
 v(259) = pv(1)*pv(12)
 v(260) = pv(2)*pv(12)
 v(261) = pv(4)*pv(12)
 v(262) = pv(0)*pv(13)
 v(263) = pv(1)*pv(13)
 v(264) = pv(2)*pv(13)
 v(265) = pv(3)*pv(13)
 v(266) = pv(4)*pv(13)
 v(267) = pv(0)*pv(14)
 v(268) = pv(1)*pv(14)
 v(269) = pv(2)*pv(14)
 v(270) = pv(3)*pv(14)
 v(271) = pv(4)*pv(14)
 v(272) = pv(0)*pv(15)
 v(273) = pv(1)*pv(15)
 v(274) = pv(2)*pv(15)
 v(275) = pv(3)*pv(15)
 v(276) = pv(4)*pv(15)
 v(277) = pv(0)*pv(16)
 v(278) = pv(1)*pv(16)
 v(279) = pv(2)*pv(16)
 v(280) = pv(3)*pv(16)
 v(281) = pv(4)*pv(16)
 v(282) = pv(0)*pv(17)
 v(283) = pv(1)*pv(17)
 v(284) = pv(2)*pv(17)
 v(285) = pv(3)*pv(17)
 v(286) = pv(4)*pv(17)
 v(287) = pv(0)*pv(18)
 v(288) = pv(1)*pv(18)
 v(289) = pv(2)*pv(18)
 v(290) = pv(3)*pv(18)
 v(291) = pv(4)*pv(18)
 v(292) = pv(0)*pv(19)
 v(293) = pv(1)*pv(19)
 v(294) = pv(2)*pv(19)
 v(295) = pv(4)*pv(19)
 v(296) = pv(0)*pv(20)
 v(297) = pv(1)*pv(20)
 v(298) = pv(2)*pv(20)
 v(299) = pv(3)*pv(20)
 v(300) = pv(4)*pv(20)
 v(301) = pv(0)*pv(21)
 v(302) = pv(1)*pv(21)
 v(303) = pv(2)*pv(21)
 v(304) = pv(3)*pv(21)
 v(305) = pv(4)*pv(21)
 v(306) = pv(0)*pv(22)
 v(307) = pv(1)*pv(22)
 v(308) = pv(2)*pv(22)
 v(309) = pv(3)*pv(22)
 v(310) = pv(4)*pv(22)
 v(311) = pv(0)*pv(23)
 v(312) = pv(1)*pv(23)
 v(313) = pv(2)*pv(23)
 v(314) = pv(3)*pv(23)
 v(315) = pv(0)*pv(24)
 v(316) = pv(1)*pv(24)
 v(317) = pv(2)*pv(24)
 v(318) = pv(3)*pv(24)
 v(319) = pv(0)*pv(25)
 v(320) = pv(1)*pv(25)
 v(321) = pv(2)*pv(25)
 v(322) = pv(3)*pv(25)
 v(323) = pv(4)*pv(25)
 v(324) = pv(0)*pv(26)
 v(325) = pv(1)*pv(26)
 v(326) = pv(2)*pv(26)
 v(327) = pv(3)*pv(26)
 v(328) = pv(4)*pv(26)
 v(329) = pv(0)*pv(27)
 v(330) = pv(1)*pv(27)
 v(331) = pv(2)*pv(27)
 v(332) = pv(3)*pv(27)
 v(333) = pv(4)*pv(27)
 v(334) = pv(0)*pv(28)
 v(335) = pv(1)*pv(28)
 v(336) = pv(2)*pv(28)
 v(337) = pv(3)*pv(28)
 v(338) = pv(0)*pv(29)
 v(339) = pv(1)*pv(29)
 v(340) = pv(2)*pv(29)
 v(341) = pv(3)*pv(29)
 v(342) = pv(4)*pv(29)
 v(343) = pv(0)*pv(30)
 v(344) = pv(1)*pv(30)
 v(345) = pv(2)*pv(30)
 v(346) = pv(3)*pv(30)
 v(347) = pv(4)*pv(30)
 v(348) = pv(0)*pv(31)
 v(349) = pv(1)*pv(31)
 v(350) = pv(2)*pv(31)
 v(351) = pv(3)*pv(31)
 v(352) = pv(4)*pv(31)
 v(353) = pv(0)*pv(32)
 v(354) = pv(1)*pv(32)
 v(355) = pv(2)*pv(32)
 v(356) = pv(4)*pv(32)
 v(357) = pv(0)*pv(33)
 v(358) = pv(1)*pv(33)
 v(359) = pv(2)*pv(33)
 v(360) = pv(4)*pv(33)
 v(361) = pv(0)*pv(34)
 v(362) = pv(1)*pv(34)
 v(363) = pv(2)*pv(34)
 v(364) = pv(3)*pv(34)
 v(365) = pv(0)*pv(35)
 v(366) = pv(1)*pv(35)
 v(367) = pv(2)*pv(35)
 v(368) = pv(0)*pv(36)
 v(369) = pv(1)*pv(36)
 v(370) = pv(2)*pv(36)
 v(371) = pv(0)*pv(37)
 v(372) = pv(1)*pv(37)
 v(373) = pv(2)*pv(37)
 v(374) = pv(3)*pv(37)
 v(375) = pv(4)*pv(37)
 v(376) = pv(0)*pv(38)
 v(377) = pv(1)*pv(38)
 v(378) = pv(2)*pv(38)
 v(379) = pv(3)*pv(38)
 v(380) = pv(0)*pv(39)
 v(381) = pv(1)*pv(39)
 v(382) = pv(2)*pv(39)
 v(383) = pv(3)*pv(39)
 v(384) = pv(4)*pv(39)
 v(385) = pv(0)*pv(40)
 v(386) = pv(1)*pv(40)
 v(387) = pv(2)*pv(40)
 v(388) = pv(0)*pv(41)
 v(389) = pv(1)*pv(41)
 v(390) = pv(2)*pv(41)
 v(391) = pv(0)*pv(42)
 v(392) = pv(1)*pv(42)
 v(393) = pv(2)*pv(42)
 v(394:1075) = pv(210:891)
endif
if (6.le.mxd) then
 stop 'mg622: degree 6 not implemented'
endif
return
END SUBROUTINE mg622_secs