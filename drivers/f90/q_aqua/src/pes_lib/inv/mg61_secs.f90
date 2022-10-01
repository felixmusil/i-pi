SUBROUTINE mg61_secs (mxd, r, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: v(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: pv(0:size(v)-1) !! an upper bound size
if (size(r).ne.nk*nk.or.size(v).ne.mg61_nsc(mxd)) then
 stop 'mg61_secs: bad dimensions'
endif
call mg61_isecs (mxd, r, pv)
v(0) = 1
if (2.le.mxd) then
 v(1:1) = pv(0:0)
endif
if (3.le.mxd) then
 v(2:7) = pv(1:6)
endif
if (4.le.mxd) then
 v(8) = pv(0)*pv(0)
 v(9:31) = pv(7:29)
endif
if (5.le.mxd) then
 v(32) = pv(0)*pv(1)
 v(33) = pv(0)*pv(2)
 v(34) = pv(0)*pv(3)
 v(35) = pv(0)*pv(4)
 v(36) = pv(0)*pv(5)
 v(37) = pv(0)*pv(6)
 v(38:105) = pv(30:97)
endif
if (6.le.mxd) then
 v(106) = pv(0)*pv(0)*pv(0)
 v(107) = pv(1)*pv(1)
 v(108) = pv(1)*pv(2)
 v(109) = pv(2)*pv(2)
 v(110) = pv(1)*pv(3)
 v(111) = pv(2)*pv(3)
 v(112) = pv(3)*pv(3)
 v(113) = pv(1)*pv(4)
 v(114) = pv(2)*pv(4)
 v(115) = pv(3)*pv(4)
 v(116) = pv(4)*pv(4)
 v(117) = pv(1)*pv(5)
 v(118) = pv(2)*pv(5)
 v(119) = pv(3)*pv(5)
 v(120) = pv(4)*pv(5)
 v(121) = pv(5)*pv(5)
 v(122) = pv(1)*pv(6)
 v(123) = pv(2)*pv(6)
 v(124) = pv(3)*pv(6)
 v(125) = pv(4)*pv(6)
 v(126) = pv(5)*pv(6)
 v(127) = pv(6)*pv(6)
 v(128) = pv(0)*pv(7)
 v(129) = pv(0)*pv(8)
 v(130) = pv(0)*pv(9)
 v(131) = pv(0)*pv(10)
 v(132) = pv(0)*pv(11)
 v(133) = pv(0)*pv(12)
 v(134) = pv(0)*pv(13)
 v(135) = pv(0)*pv(14)
 v(136) = pv(0)*pv(15)
 v(137) = pv(0)*pv(16)
 v(138) = pv(0)*pv(17)
 v(139) = pv(0)*pv(18)
 v(140) = pv(0)*pv(19)
 v(141) = pv(0)*pv(20)
 v(142) = pv(0)*pv(21)
 v(143) = pv(0)*pv(22)
 v(144) = pv(0)*pv(23)
 v(145) = pv(0)*pv(24)
 v(146) = pv(0)*pv(25)
 v(147) = pv(0)*pv(26)
 v(148) = pv(0)*pv(27)
 v(149) = pv(0)*pv(28)
 v(150) = pv(0)*pv(29)
 v(151:321) = pv(98:268)
endif
if (7.le.mxd) then
 v(322) = pv(0)*pv(0)*pv(1)
 v(323) = pv(0)*pv(0)*pv(2)
 v(324) = pv(0)*pv(0)*pv(3)
 v(325) = pv(0)*pv(0)*pv(4)
 v(326) = pv(0)*pv(0)*pv(5)
 v(327) = pv(0)*pv(0)*pv(6)
 v(328) = pv(1)*pv(7)
 v(329) = pv(2)*pv(7)
 v(330) = pv(3)*pv(7)
 v(331) = pv(4)*pv(7)
 v(332) = pv(5)*pv(7)
 v(333) = pv(6)*pv(7)
 v(334) = pv(1)*pv(8)
 v(335) = pv(2)*pv(8)
 v(336) = pv(3)*pv(8)
 v(337) = pv(4)*pv(8)
 v(338) = pv(5)*pv(8)
 v(339) = pv(6)*pv(8)
 v(340) = pv(1)*pv(9)
 v(341) = pv(2)*pv(9)
 v(342) = pv(3)*pv(9)
 v(343) = pv(4)*pv(9)
 v(344) = pv(5)*pv(9)
 v(345) = pv(6)*pv(9)
 v(346) = pv(1)*pv(10)
 v(347) = pv(2)*pv(10)
 v(348) = pv(3)*pv(10)
 v(349) = pv(4)*pv(10)
 v(350) = pv(5)*pv(10)
 v(351) = pv(6)*pv(10)
 v(352) = pv(1)*pv(11)
 v(353) = pv(2)*pv(11)
 v(354) = pv(3)*pv(11)
 v(355) = pv(4)*pv(11)
 v(356) = pv(5)*pv(11)
 v(357) = pv(6)*pv(11)
 v(358) = pv(1)*pv(12)
 v(359) = pv(2)*pv(12)
 v(360) = pv(3)*pv(12)
 v(361) = pv(4)*pv(12)
 v(362) = pv(5)*pv(12)
 v(363) = pv(6)*pv(12)
 v(364) = pv(1)*pv(13)
 v(365) = pv(2)*pv(13)
 v(366) = pv(3)*pv(13)
 v(367) = pv(4)*pv(13)
 v(368) = pv(5)*pv(13)
 v(369) = pv(6)*pv(13)
 v(370) = pv(1)*pv(14)
 v(371) = pv(2)*pv(14)
 v(372) = pv(3)*pv(14)
 v(373) = pv(4)*pv(14)
 v(374) = pv(5)*pv(14)
 v(375) = pv(6)*pv(14)
 v(376) = pv(1)*pv(15)
 v(377) = pv(2)*pv(15)
 v(378) = pv(3)*pv(15)
 v(379) = pv(4)*pv(15)
 v(380) = pv(5)*pv(15)
 v(381) = pv(6)*pv(15)
 v(382) = pv(1)*pv(16)
 v(383) = pv(2)*pv(16)
 v(384) = pv(3)*pv(16)
 v(385) = pv(4)*pv(16)
 v(386) = pv(5)*pv(16)
 v(387) = pv(6)*pv(16)
 v(388) = pv(1)*pv(17)
 v(389) = pv(2)*pv(17)
 v(390) = pv(3)*pv(17)
 v(391) = pv(4)*pv(17)
 v(392) = pv(5)*pv(17)
 v(393) = pv(6)*pv(17)
 v(394) = pv(1)*pv(18)
 v(395) = pv(2)*pv(18)
 v(396) = pv(3)*pv(18)
 v(397) = pv(4)*pv(18)
 v(398) = pv(5)*pv(18)
 v(399) = pv(6)*pv(18)
 v(400) = pv(1)*pv(19)
 v(401) = pv(2)*pv(19)
 v(402) = pv(3)*pv(19)
 v(403) = pv(4)*pv(19)
 v(404) = pv(5)*pv(19)
 v(405) = pv(6)*pv(19)
 v(406) = pv(1)*pv(20)
 v(407) = pv(2)*pv(20)
 v(408) = pv(3)*pv(20)
 v(409) = pv(4)*pv(20)
 v(410) = pv(5)*pv(20)
 v(411) = pv(6)*pv(20)
 v(412) = pv(1)*pv(21)
 v(413) = pv(2)*pv(21)
 v(414) = pv(3)*pv(21)
 v(415) = pv(4)*pv(21)
 v(416) = pv(5)*pv(21)
 v(417) = pv(6)*pv(21)
 v(418) = pv(1)*pv(22)
 v(419) = pv(2)*pv(22)
 v(420) = pv(3)*pv(22)
 v(421) = pv(4)*pv(22)
 v(422) = pv(5)*pv(22)
 v(423) = pv(6)*pv(22)
 v(424) = pv(1)*pv(23)
 v(425) = pv(2)*pv(23)
 v(426) = pv(3)*pv(23)
 v(427) = pv(4)*pv(23)
 v(428) = pv(5)*pv(23)
 v(429) = pv(6)*pv(23)
 v(430) = pv(1)*pv(24)
 v(431) = pv(2)*pv(24)
 v(432) = pv(3)*pv(24)
 v(433) = pv(4)*pv(24)
 v(434) = pv(5)*pv(24)
 v(435) = pv(6)*pv(24)
 v(436) = pv(1)*pv(25)
 v(437) = pv(2)*pv(25)
 v(438) = pv(3)*pv(25)
 v(439) = pv(4)*pv(25)
 v(440) = pv(5)*pv(25)
 v(441) = pv(6)*pv(25)
 v(442) = pv(1)*pv(26)
 v(443) = pv(2)*pv(26)
 v(444) = pv(3)*pv(26)
 v(445) = pv(4)*pv(26)
 v(446) = pv(5)*pv(26)
 v(447) = pv(6)*pv(26)
 v(448) = pv(1)*pv(27)
 v(449) = pv(2)*pv(27)
 v(450) = pv(3)*pv(27)
 v(451) = pv(4)*pv(27)
 v(452) = pv(5)*pv(27)
 v(453) = pv(6)*pv(27)
 v(454) = pv(1)*pv(28)
 v(455) = pv(2)*pv(28)
 v(456) = pv(3)*pv(28)
 v(457) = pv(4)*pv(28)
 v(458) = pv(5)*pv(28)
 v(459) = pv(6)*pv(28)
 v(460) = pv(1)*pv(29)
 v(461) = pv(2)*pv(29)
 v(462) = pv(3)*pv(29)
 v(463) = pv(4)*pv(29)
 v(464) = pv(5)*pv(29)
 v(465) = pv(6)*pv(29)
 v(466:533) = pv(0)*pv(30:97)
 v(534:911) = pv(269:646)
endif
if (8.le.mxd) then
 stop 'mg61: degree 8 not implemented'
endif
return
END SUBROUTINE mg61_secs