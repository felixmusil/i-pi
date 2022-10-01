SUBROUTINE mg541_secs (mxd, r, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: v(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: pv(0:size(v)-1) !! an upper bound size
if (size(r).ne.nk*nk.or.size(v).ne.mg541_nsc(mxd)) then
 stop 'mg541_secs: bad dimensions'
endif
call mg541_isecs (mxd, r, pv)
v(0) = 1
if (2.le.mxd) then
 v(1:6) = pv(0:5)
endif
if (3.le.mxd) then
 v(7:50) = pv(6:49)
endif
if (4.le.mxd) then
 v(51) = pv(0)*pv(0)
 v(52) = pv(0)*pv(1)
 v(53) = pv(1)*pv(1)
 v(54) = pv(0)*pv(2)
 v(55) = pv(1)*pv(2)
 v(56) = pv(2)*pv(2)
 v(57) = pv(0)*pv(3)
 v(58) = pv(1)*pv(3)
 v(59) = pv(2)*pv(3)
 v(60) = pv(3)*pv(3)
 v(61) = pv(0)*pv(4)
 v(62) = pv(1)*pv(4)
 v(63) = pv(2)*pv(4)
 v(64) = pv(3)*pv(4)
 v(65) = pv(4)*pv(4)
 v(66) = pv(0)*pv(5)
 v(67) = pv(1)*pv(5)
 v(68) = pv(2)*pv(5)
 v(69) = pv(3)*pv(5)
 v(70) = pv(4)*pv(5)
 v(71) = pv(5)*pv(5)
 v(72:268) = pv(50:246)
endif
if (5.le.mxd) then
 v(269) = pv(0)*pv(6)
 v(270) = pv(1)*pv(6)
 v(271) = pv(2)*pv(6)
 v(272) = pv(3)*pv(6)
 v(273) = pv(4)*pv(6)
 v(274) = pv(5)*pv(6)
 v(275) = pv(0)*pv(7)
 v(276) = pv(1)*pv(7)
 v(277) = pv(2)*pv(7)
 v(278) = pv(3)*pv(7)
 v(279) = pv(4)*pv(7)
 v(280) = pv(5)*pv(7)
 v(281) = pv(0)*pv(8)
 v(282) = pv(1)*pv(8)
 v(283) = pv(2)*pv(8)
 v(284) = pv(3)*pv(8)
 v(285) = pv(4)*pv(8)
 v(286) = pv(5)*pv(8)
 v(287) = pv(0)*pv(9)
 v(288) = pv(1)*pv(9)
 v(289) = pv(2)*pv(9)
 v(290) = pv(3)*pv(9)
 v(291) = pv(4)*pv(9)
 v(292) = pv(5)*pv(9)
 v(293) = pv(0)*pv(10)
 v(294) = pv(1)*pv(10)
 v(295) = pv(2)*pv(10)
 v(296) = pv(3)*pv(10)
 v(297) = pv(4)*pv(10)
 v(298) = pv(5)*pv(10)
 v(299) = pv(0)*pv(11)
 v(300) = pv(1)*pv(11)
 v(301) = pv(2)*pv(11)
 v(302) = pv(3)*pv(11)
 v(303) = pv(4)*pv(11)
 v(304) = pv(5)*pv(11)
 v(305) = pv(0)*pv(12)
 v(306) = pv(1)*pv(12)
 v(307) = pv(2)*pv(12)
 v(308) = pv(3)*pv(12)
 v(309) = pv(4)*pv(12)
 v(310) = pv(5)*pv(12)
 v(311) = pv(0)*pv(13)
 v(312) = pv(1)*pv(13)
 v(313) = pv(2)*pv(13)
 v(314) = pv(3)*pv(13)
 v(315) = pv(4)*pv(13)
 v(316) = pv(5)*pv(13)
 v(317) = pv(0)*pv(14)
 v(318) = pv(1)*pv(14)
 v(319) = pv(2)*pv(14)
 v(320) = pv(3)*pv(14)
 v(321) = pv(4)*pv(14)
 v(322) = pv(5)*pv(14)
 v(323) = pv(0)*pv(15)
 v(324) = pv(1)*pv(15)
 v(325) = pv(2)*pv(15)
 v(326) = pv(3)*pv(15)
 v(327) = pv(4)*pv(15)
 v(328) = pv(5)*pv(15)
 v(329) = pv(0)*pv(16)
 v(330) = pv(1)*pv(16)
 v(331) = pv(2)*pv(16)
 v(332) = pv(3)*pv(16)
 v(333) = pv(4)*pv(16)
 v(334) = pv(5)*pv(16)
 v(335) = pv(0)*pv(17)
 v(336) = pv(1)*pv(17)
 v(337) = pv(2)*pv(17)
 v(338) = pv(3)*pv(17)
 v(339) = pv(4)*pv(17)
 v(340) = pv(5)*pv(17)
 v(341) = pv(0)*pv(18)
 v(342) = pv(1)*pv(18)
 v(343) = pv(2)*pv(18)
 v(344) = pv(3)*pv(18)
 v(345) = pv(4)*pv(18)
 v(346) = pv(5)*pv(18)
 v(347) = pv(0)*pv(19)
 v(348) = pv(1)*pv(19)
 v(349) = pv(2)*pv(19)
 v(350) = pv(3)*pv(19)
 v(351) = pv(4)*pv(19)
 v(352) = pv(5)*pv(19)
 v(353) = pv(0)*pv(20)
 v(354) = pv(1)*pv(20)
 v(355) = pv(2)*pv(20)
 v(356) = pv(3)*pv(20)
 v(357) = pv(4)*pv(20)
 v(358) = pv(5)*pv(20)
 v(359) = pv(0)*pv(21)
 v(360) = pv(1)*pv(21)
 v(361) = pv(2)*pv(21)
 v(362) = pv(3)*pv(21)
 v(363) = pv(4)*pv(21)
 v(364) = pv(5)*pv(21)
 v(365) = pv(0)*pv(22)
 v(366) = pv(1)*pv(22)
 v(367) = pv(2)*pv(22)
 v(368) = pv(3)*pv(22)
 v(369) = pv(4)*pv(22)
 v(370) = pv(5)*pv(22)
 v(371) = pv(0)*pv(23)
 v(372) = pv(1)*pv(23)
 v(373) = pv(2)*pv(23)
 v(374) = pv(3)*pv(23)
 v(375) = pv(4)*pv(23)
 v(376) = pv(5)*pv(23)
 v(377) = pv(0)*pv(24)
 v(378) = pv(1)*pv(24)
 v(379) = pv(2)*pv(24)
 v(380) = pv(3)*pv(24)
 v(381) = pv(4)*pv(24)
 v(382) = pv(5)*pv(24)
 v(383) = pv(0)*pv(25)
 v(384) = pv(1)*pv(25)
 v(385) = pv(2)*pv(25)
 v(386) = pv(3)*pv(25)
 v(387) = pv(4)*pv(25)
 v(388) = pv(5)*pv(25)
 v(389) = pv(0)*pv(26)
 v(390) = pv(1)*pv(26)
 v(391) = pv(2)*pv(26)
 v(392) = pv(3)*pv(26)
 v(393) = pv(4)*pv(26)
 v(394) = pv(5)*pv(26)
 v(395) = pv(0)*pv(27)
 v(396) = pv(1)*pv(27)
 v(397) = pv(2)*pv(27)
 v(398) = pv(3)*pv(27)
 v(399) = pv(4)*pv(27)
 v(400) = pv(5)*pv(27)
 v(401) = pv(0)*pv(28)
 v(402) = pv(1)*pv(28)
 v(403) = pv(2)*pv(28)
 v(404) = pv(3)*pv(28)
 v(405) = pv(4)*pv(28)
 v(406) = pv(5)*pv(28)
 v(407) = pv(0)*pv(29)
 v(408) = pv(1)*pv(29)
 v(409) = pv(2)*pv(29)
 v(410) = pv(3)*pv(29)
 v(411) = pv(4)*pv(29)
 v(412) = pv(5)*pv(29)
 v(413) = pv(0)*pv(30)
 v(414) = pv(1)*pv(30)
 v(415) = pv(2)*pv(30)
 v(416) = pv(3)*pv(30)
 v(417) = pv(4)*pv(30)
 v(418) = pv(5)*pv(30)
 v(419) = pv(0)*pv(31)
 v(420) = pv(1)*pv(31)
 v(421) = pv(2)*pv(31)
 v(422) = pv(3)*pv(31)
 v(423) = pv(4)*pv(31)
 v(424) = pv(5)*pv(31)
 v(425) = pv(0)*pv(32)
 v(426) = pv(1)*pv(32)
 v(427) = pv(2)*pv(32)
 v(428) = pv(3)*pv(32)
 v(429) = pv(4)*pv(32)
 v(430) = pv(5)*pv(32)
 v(431) = pv(0)*pv(33)
 v(432) = pv(1)*pv(33)
 v(433) = pv(2)*pv(33)
 v(434) = pv(3)*pv(33)
 v(435) = pv(4)*pv(33)
 v(436) = pv(5)*pv(33)
 v(437) = pv(0)*pv(34)
 v(438) = pv(1)*pv(34)
 v(439) = pv(2)*pv(34)
 v(440) = pv(3)*pv(34)
 v(441) = pv(4)*pv(34)
 v(442) = pv(5)*pv(34)
 v(443) = pv(0)*pv(35)
 v(444) = pv(1)*pv(35)
 v(445) = pv(2)*pv(35)
 v(446) = pv(3)*pv(35)
 v(447) = pv(4)*pv(35)
 v(448) = pv(5)*pv(35)
 v(449) = pv(0)*pv(36)
 v(450) = pv(1)*pv(36)
 v(451) = pv(2)*pv(36)
 v(452) = pv(3)*pv(36)
 v(453) = pv(4)*pv(36)
 v(454) = pv(5)*pv(36)
 v(455) = pv(0)*pv(37)
 v(456) = pv(1)*pv(37)
 v(457) = pv(2)*pv(37)
 v(458) = pv(3)*pv(37)
 v(459) = pv(4)*pv(37)
 v(460) = pv(5)*pv(37)
 v(461) = pv(0)*pv(38)
 v(462) = pv(1)*pv(38)
 v(463) = pv(2)*pv(38)
 v(464) = pv(3)*pv(38)
 v(465) = pv(4)*pv(38)
 v(466) = pv(5)*pv(38)
 v(467) = pv(0)*pv(39)
 v(468) = pv(1)*pv(39)
 v(469) = pv(2)*pv(39)
 v(470) = pv(3)*pv(39)
 v(471) = pv(4)*pv(39)
 v(472) = pv(5)*pv(39)
 v(473) = pv(0)*pv(40)
 v(474) = pv(1)*pv(40)
 v(475) = pv(2)*pv(40)
 v(476) = pv(3)*pv(40)
 v(477) = pv(4)*pv(40)
 v(478) = pv(5)*pv(40)
 v(479) = pv(0)*pv(41)
 v(480) = pv(1)*pv(41)
 v(481) = pv(2)*pv(41)
 v(482) = pv(3)*pv(41)
 v(483) = pv(4)*pv(41)
 v(484) = pv(5)*pv(41)
 v(485) = pv(0)*pv(42)
 v(486) = pv(1)*pv(42)
 v(487) = pv(2)*pv(42)
 v(488) = pv(3)*pv(42)
 v(489) = pv(4)*pv(42)
 v(490) = pv(5)*pv(42)
 v(491) = pv(0)*pv(43)
 v(492) = pv(1)*pv(43)
 v(493) = pv(2)*pv(43)
 v(494) = pv(3)*pv(43)
 v(495) = pv(4)*pv(43)
 v(496) = pv(5)*pv(43)
 v(497) = pv(0)*pv(44)
 v(498) = pv(1)*pv(44)
 v(499) = pv(2)*pv(44)
 v(500) = pv(3)*pv(44)
 v(501) = pv(4)*pv(44)
 v(502) = pv(5)*pv(44)
 v(503) = pv(0)*pv(45)
 v(504) = pv(1)*pv(45)
 v(505) = pv(2)*pv(45)
 v(506) = pv(3)*pv(45)
 v(507) = pv(4)*pv(45)
 v(508) = pv(5)*pv(45)
 v(509) = pv(0)*pv(46)
 v(510) = pv(1)*pv(46)
 v(511) = pv(2)*pv(46)
 v(512) = pv(3)*pv(46)
 v(513) = pv(4)*pv(46)
 v(514) = pv(5)*pv(46)
 v(515) = pv(0)*pv(47)
 v(516) = pv(1)*pv(47)
 v(517) = pv(2)*pv(47)
 v(518) = pv(3)*pv(47)
 v(519) = pv(4)*pv(47)
 v(520) = pv(5)*pv(47)
 v(521) = pv(0)*pv(48)
 v(522) = pv(1)*pv(48)
 v(523) = pv(2)*pv(48)
 v(524) = pv(3)*pv(48)
 v(525) = pv(4)*pv(48)
 v(526) = pv(5)*pv(48)
 v(527) = pv(0)*pv(49)
 v(528) = pv(1)*pv(49)
 v(529) = pv(2)*pv(49)
 v(530) = pv(3)*pv(49)
 v(531) = pv(4)*pv(49)
 v(532) = pv(5)*pv(49)
 v(533:1334) = pv(247:1048)
endif
if (6.le.mxd) then
 stop 'mg541: degree 6 not implemented'
endif
return
END SUBROUTINE mg541_secs