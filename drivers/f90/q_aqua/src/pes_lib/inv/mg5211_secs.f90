SUBROUTINE mg5211_secs (mxd, r, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: v(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: pv(0:size(v)-1) !! an upper bound size
if (size(r).ne.nk*nk.or.size(v).ne.mg5211_nsc(mxd)) then
 stop 'mg5211_secs: bad dimensions'
endif
call mg5211_isecs (mxd, r, pv)
v(0) = 1
if (2.le.mxd) then
 v(1:9) = pv(0:8)
endif
if (3.le.mxd) then
 v(10:56) = pv(9:55)
endif
if (4.le.mxd) then
 v(57) = pv(0)*pv(0)
 v(58) = pv(0)*pv(1)
 v(59) = pv(1)*pv(1)
 v(60) = pv(0)*pv(2)
 v(61) = pv(1)*pv(2)
 v(62) = pv(2)*pv(2)
 v(63) = pv(0)*pv(3)
 v(64) = pv(1)*pv(3)
 v(65) = pv(2)*pv(3)
 v(66) = pv(0)*pv(4)
 v(67) = pv(1)*pv(4)
 v(68) = pv(2)*pv(4)
 v(69) = pv(3)*pv(4)
 v(70) = pv(4)*pv(4)
 v(71) = pv(0)*pv(5)
 v(72) = pv(1)*pv(5)
 v(73) = pv(2)*pv(5)
 v(74) = pv(3)*pv(5)
 v(75) = pv(4)*pv(5)
 v(76) = pv(5)*pv(5)
 v(77) = pv(0)*pv(6)
 v(78) = pv(1)*pv(6)
 v(79) = pv(2)*pv(6)
 v(80) = pv(3)*pv(6)
 v(81) = pv(4)*pv(6)
 v(82) = pv(5)*pv(6)
 v(83) = pv(6)*pv(6)
 v(84) = pv(0)*pv(7)
 v(85) = pv(1)*pv(7)
 v(86) = pv(2)*pv(7)
 v(87) = pv(4)*pv(7)
 v(88) = pv(5)*pv(7)
 v(89) = pv(6)*pv(7)
 v(90) = pv(0)*pv(8)
 v(91) = pv(1)*pv(8)
 v(92) = pv(2)*pv(8)
 v(93) = pv(4)*pv(8)
 v(94) = pv(5)*pv(8)
 v(95) = pv(6)*pv(8)
 v(96:272) = pv(56:232)
endif
if (5.le.mxd) then
 v(273) = pv(0)*pv(9)
 v(274) = pv(1)*pv(9)
 v(275) = pv(2)*pv(9)
 v(276) = pv(3)*pv(9)
 v(277) = pv(4)*pv(9)
 v(278) = pv(5)*pv(9)
 v(279) = pv(6)*pv(9)
 v(280) = pv(7)*pv(9)
 v(281) = pv(8)*pv(9)
 v(282) = pv(0)*pv(10)
 v(283) = pv(1)*pv(10)
 v(284) = pv(2)*pv(10)
 v(285) = pv(3)*pv(10)
 v(286) = pv(4)*pv(10)
 v(287) = pv(5)*pv(10)
 v(288) = pv(6)*pv(10)
 v(289) = pv(7)*pv(10)
 v(290) = pv(8)*pv(10)
 v(291) = pv(0)*pv(11)
 v(292) = pv(1)*pv(11)
 v(293) = pv(2)*pv(11)
 v(294) = pv(3)*pv(11)
 v(295) = pv(4)*pv(11)
 v(296) = pv(5)*pv(11)
 v(297) = pv(6)*pv(11)
 v(298) = pv(7)*pv(11)
 v(299) = pv(8)*pv(11)
 v(300) = pv(0)*pv(12)
 v(301) = pv(1)*pv(12)
 v(302) = pv(2)*pv(12)
 v(303) = pv(3)*pv(12)
 v(304) = pv(4)*pv(12)
 v(305) = pv(5)*pv(12)
 v(306) = pv(6)*pv(12)
 v(307) = pv(7)*pv(12)
 v(308) = pv(8)*pv(12)
 v(309) = pv(0)*pv(13)
 v(310) = pv(1)*pv(13)
 v(311) = pv(2)*pv(13)
 v(312) = pv(3)*pv(13)
 v(313) = pv(4)*pv(13)
 v(314) = pv(5)*pv(13)
 v(315) = pv(6)*pv(13)
 v(316) = pv(7)*pv(13)
 v(317) = pv(8)*pv(13)
 v(318) = pv(0)*pv(14)
 v(319) = pv(1)*pv(14)
 v(320) = pv(2)*pv(14)
 v(321) = pv(4)*pv(14)
 v(322) = pv(5)*pv(14)
 v(323) = pv(6)*pv(14)
 v(324) = pv(0)*pv(15)
 v(325) = pv(1)*pv(15)
 v(326) = pv(2)*pv(15)
 v(327) = pv(4)*pv(15)
 v(328) = pv(5)*pv(15)
 v(329) = pv(6)*pv(15)
 v(330) = pv(0)*pv(16)
 v(331) = pv(1)*pv(16)
 v(332) = pv(2)*pv(16)
 v(333) = pv(3)*pv(16)
 v(334) = pv(4)*pv(16)
 v(335) = pv(5)*pv(16)
 v(336) = pv(6)*pv(16)
 v(337) = pv(7)*pv(16)
 v(338) = pv(8)*pv(16)
 v(339) = pv(0)*pv(17)
 v(340) = pv(1)*pv(17)
 v(341) = pv(2)*pv(17)
 v(342) = pv(3)*pv(17)
 v(343) = pv(4)*pv(17)
 v(344) = pv(5)*pv(17)
 v(345) = pv(6)*pv(17)
 v(346) = pv(7)*pv(17)
 v(347) = pv(8)*pv(17)
 v(348) = pv(0)*pv(18)
 v(349) = pv(1)*pv(18)
 v(350) = pv(2)*pv(18)
 v(351) = pv(3)*pv(18)
 v(352) = pv(4)*pv(18)
 v(353) = pv(5)*pv(18)
 v(354) = pv(6)*pv(18)
 v(355) = pv(7)*pv(18)
 v(356) = pv(8)*pv(18)
 v(357) = pv(0)*pv(19)
 v(358) = pv(1)*pv(19)
 v(359) = pv(2)*pv(19)
 v(360) = pv(3)*pv(19)
 v(361) = pv(4)*pv(19)
 v(362) = pv(5)*pv(19)
 v(363) = pv(6)*pv(19)
 v(364) = pv(7)*pv(19)
 v(365) = pv(8)*pv(19)
 v(366) = pv(0)*pv(20)
 v(367) = pv(1)*pv(20)
 v(368) = pv(2)*pv(20)
 v(369) = pv(3)*pv(20)
 v(370) = pv(4)*pv(20)
 v(371) = pv(5)*pv(20)
 v(372) = pv(6)*pv(20)
 v(373) = pv(7)*pv(20)
 v(374) = pv(8)*pv(20)
 v(375) = pv(0)*pv(21)
 v(376) = pv(1)*pv(21)
 v(377) = pv(2)*pv(21)
 v(378) = pv(3)*pv(21)
 v(379) = pv(4)*pv(21)
 v(380) = pv(5)*pv(21)
 v(381) = pv(6)*pv(21)
 v(382) = pv(7)*pv(21)
 v(383) = pv(8)*pv(21)
 v(384) = pv(0)*pv(22)
 v(385) = pv(1)*pv(22)
 v(386) = pv(2)*pv(22)
 v(387) = pv(3)*pv(22)
 v(388) = pv(4)*pv(22)
 v(389) = pv(5)*pv(22)
 v(390) = pv(6)*pv(22)
 v(391) = pv(7)*pv(22)
 v(392) = pv(8)*pv(22)
 v(393) = pv(0)*pv(23)
 v(394) = pv(1)*pv(23)
 v(395) = pv(2)*pv(23)
 v(396) = pv(3)*pv(23)
 v(397) = pv(4)*pv(23)
 v(398) = pv(5)*pv(23)
 v(399) = pv(6)*pv(23)
 v(400) = pv(7)*pv(23)
 v(401) = pv(8)*pv(23)
 v(402) = pv(0)*pv(24)
 v(403) = pv(1)*pv(24)
 v(404) = pv(2)*pv(24)
 v(405) = pv(3)*pv(24)
 v(406) = pv(4)*pv(24)
 v(407) = pv(5)*pv(24)
 v(408) = pv(6)*pv(24)
 v(409) = pv(7)*pv(24)
 v(410) = pv(8)*pv(24)
 v(411) = pv(0)*pv(25)
 v(412) = pv(1)*pv(25)
 v(413) = pv(2)*pv(25)
 v(414) = pv(4)*pv(25)
 v(415) = pv(5)*pv(25)
 v(416) = pv(6)*pv(25)
 v(417) = pv(0)*pv(26)
 v(418) = pv(1)*pv(26)
 v(419) = pv(2)*pv(26)
 v(420) = pv(3)*pv(26)
 v(421) = pv(4)*pv(26)
 v(422) = pv(5)*pv(26)
 v(423) = pv(6)*pv(26)
 v(424) = pv(7)*pv(26)
 v(425) = pv(8)*pv(26)
 v(426) = pv(0)*pv(27)
 v(427) = pv(1)*pv(27)
 v(428) = pv(2)*pv(27)
 v(429) = pv(3)*pv(27)
 v(430) = pv(4)*pv(27)
 v(431) = pv(5)*pv(27)
 v(432) = pv(6)*pv(27)
 v(433) = pv(7)*pv(27)
 v(434) = pv(8)*pv(27)
 v(435) = pv(0)*pv(28)
 v(436) = pv(1)*pv(28)
 v(437) = pv(2)*pv(28)
 v(438) = pv(3)*pv(28)
 v(439) = pv(4)*pv(28)
 v(440) = pv(5)*pv(28)
 v(441) = pv(6)*pv(28)
 v(442) = pv(7)*pv(28)
 v(443) = pv(8)*pv(28)
 v(444) = pv(0)*pv(29)
 v(445) = pv(1)*pv(29)
 v(446) = pv(2)*pv(29)
 v(447) = pv(3)*pv(29)
 v(448) = pv(4)*pv(29)
 v(449) = pv(5)*pv(29)
 v(450) = pv(6)*pv(29)
 v(451) = pv(7)*pv(29)
 v(452) = pv(8)*pv(29)
 v(453) = pv(0)*pv(30)
 v(454) = pv(1)*pv(30)
 v(455) = pv(2)*pv(30)
 v(456) = pv(3)*pv(30)
 v(457) = pv(4)*pv(30)
 v(458) = pv(5)*pv(30)
 v(459) = pv(6)*pv(30)
 v(460) = pv(7)*pv(30)
 v(461) = pv(8)*pv(30)
 v(462) = pv(0)*pv(31)
 v(463) = pv(1)*pv(31)
 v(464) = pv(2)*pv(31)
 v(465) = pv(3)*pv(31)
 v(466) = pv(4)*pv(31)
 v(467) = pv(5)*pv(31)
 v(468) = pv(6)*pv(31)
 v(469) = pv(7)*pv(31)
 v(470) = pv(8)*pv(31)
 v(471) = pv(0)*pv(32)
 v(472) = pv(1)*pv(32)
 v(473) = pv(2)*pv(32)
 v(474) = pv(4)*pv(32)
 v(475) = pv(5)*pv(32)
 v(476) = pv(6)*pv(32)
 v(477) = pv(0)*pv(33)
 v(478) = pv(1)*pv(33)
 v(479) = pv(2)*pv(33)
 v(480) = pv(4)*pv(33)
 v(481) = pv(5)*pv(33)
 v(482) = pv(6)*pv(33)
 v(483) = pv(0)*pv(34)
 v(484) = pv(1)*pv(34)
 v(485) = pv(2)*pv(34)
 v(486) = pv(4)*pv(34)
 v(487) = pv(5)*pv(34)
 v(488) = pv(6)*pv(34)
 v(489) = pv(0)*pv(35)
 v(490) = pv(1)*pv(35)
 v(491) = pv(2)*pv(35)
 v(492) = pv(3)*pv(35)
 v(493) = pv(4)*pv(35)
 v(494) = pv(5)*pv(35)
 v(495) = pv(6)*pv(35)
 v(496) = pv(7)*pv(35)
 v(497) = pv(8)*pv(35)
 v(498) = pv(0)*pv(36)
 v(499) = pv(1)*pv(36)
 v(500) = pv(2)*pv(36)
 v(501) = pv(3)*pv(36)
 v(502) = pv(4)*pv(36)
 v(503) = pv(5)*pv(36)
 v(504) = pv(6)*pv(36)
 v(505) = pv(7)*pv(36)
 v(506) = pv(8)*pv(36)
 v(507) = pv(0)*pv(37)
 v(508) = pv(1)*pv(37)
 v(509) = pv(2)*pv(37)
 v(510) = pv(3)*pv(37)
 v(511) = pv(4)*pv(37)
 v(512) = pv(5)*pv(37)
 v(513) = pv(6)*pv(37)
 v(514) = pv(7)*pv(37)
 v(515) = pv(8)*pv(37)
 v(516) = pv(0)*pv(38)
 v(517) = pv(1)*pv(38)
 v(518) = pv(2)*pv(38)
 v(519) = pv(3)*pv(38)
 v(520) = pv(4)*pv(38)
 v(521) = pv(5)*pv(38)
 v(522) = pv(6)*pv(38)
 v(523) = pv(7)*pv(38)
 v(524) = pv(8)*pv(38)
 v(525) = pv(0)*pv(39)
 v(526) = pv(1)*pv(39)
 v(527) = pv(2)*pv(39)
 v(528) = pv(4)*pv(39)
 v(529) = pv(5)*pv(39)
 v(530) = pv(6)*pv(39)
 v(531) = pv(0)*pv(40)
 v(532) = pv(1)*pv(40)
 v(533) = pv(2)*pv(40)
 v(534) = pv(3)*pv(40)
 v(535) = pv(4)*pv(40)
 v(536) = pv(5)*pv(40)
 v(537) = pv(6)*pv(40)
 v(538) = pv(7)*pv(40)
 v(539) = pv(8)*pv(40)
 v(540) = pv(0)*pv(41)
 v(541) = pv(1)*pv(41)
 v(542) = pv(2)*pv(41)
 v(543) = pv(3)*pv(41)
 v(544) = pv(4)*pv(41)
 v(545) = pv(5)*pv(41)
 v(546) = pv(6)*pv(41)
 v(547) = pv(7)*pv(41)
 v(548) = pv(8)*pv(41)
 v(549) = pv(0)*pv(42)
 v(550) = pv(1)*pv(42)
 v(551) = pv(2)*pv(42)
 v(552) = pv(3)*pv(42)
 v(553) = pv(4)*pv(42)
 v(554) = pv(5)*pv(42)
 v(555) = pv(6)*pv(42)
 v(556) = pv(7)*pv(42)
 v(557) = pv(8)*pv(42)
 v(558) = pv(0)*pv(43)
 v(559) = pv(1)*pv(43)
 v(560) = pv(2)*pv(43)
 v(561) = pv(3)*pv(43)
 v(562) = pv(4)*pv(43)
 v(563) = pv(5)*pv(43)
 v(564) = pv(6)*pv(43)
 v(565) = pv(7)*pv(43)
 v(566) = pv(8)*pv(43)
 v(567) = pv(0)*pv(44)
 v(568) = pv(1)*pv(44)
 v(569) = pv(2)*pv(44)
 v(570) = pv(3)*pv(44)
 v(571) = pv(4)*pv(44)
 v(572) = pv(5)*pv(44)
 v(573) = pv(6)*pv(44)
 v(574) = pv(7)*pv(44)
 v(575) = pv(8)*pv(44)
 v(576) = pv(0)*pv(45)
 v(577) = pv(1)*pv(45)
 v(578) = pv(2)*pv(45)
 v(579) = pv(3)*pv(45)
 v(580) = pv(4)*pv(45)
 v(581) = pv(5)*pv(45)
 v(582) = pv(6)*pv(45)
 v(583) = pv(7)*pv(45)
 v(584) = pv(8)*pv(45)
 v(585) = pv(0)*pv(46)
 v(586) = pv(1)*pv(46)
 v(587) = pv(2)*pv(46)
 v(588) = pv(3)*pv(46)
 v(589) = pv(4)*pv(46)
 v(590) = pv(5)*pv(46)
 v(591) = pv(6)*pv(46)
 v(592) = pv(7)*pv(46)
 v(593) = pv(8)*pv(46)
 v(594) = pv(0)*pv(47)
 v(595) = pv(1)*pv(47)
 v(596) = pv(2)*pv(47)
 v(597) = pv(4)*pv(47)
 v(598) = pv(5)*pv(47)
 v(599) = pv(6)*pv(47)
 v(600) = pv(0)*pv(48)
 v(601) = pv(1)*pv(48)
 v(602) = pv(2)*pv(48)
 v(603) = pv(3)*pv(48)
 v(604) = pv(4)*pv(48)
 v(605) = pv(5)*pv(48)
 v(606) = pv(6)*pv(48)
 v(607) = pv(7)*pv(48)
 v(608) = pv(8)*pv(48)
 v(609) = pv(0)*pv(49)
 v(610) = pv(1)*pv(49)
 v(611) = pv(2)*pv(49)
 v(612) = pv(3)*pv(49)
 v(613) = pv(4)*pv(49)
 v(614) = pv(5)*pv(49)
 v(615) = pv(6)*pv(49)
 v(616) = pv(7)*pv(49)
 v(617) = pv(8)*pv(49)
 v(618) = pv(0)*pv(50)
 v(619) = pv(1)*pv(50)
 v(620) = pv(2)*pv(50)
 v(621) = pv(3)*pv(50)
 v(622) = pv(4)*pv(50)
 v(623) = pv(5)*pv(50)
 v(624) = pv(6)*pv(50)
 v(625) = pv(7)*pv(50)
 v(626) = pv(8)*pv(50)
 v(627) = pv(0)*pv(51)
 v(628) = pv(1)*pv(51)
 v(629) = pv(2)*pv(51)
 v(630) = pv(3)*pv(51)
 v(631) = pv(4)*pv(51)
 v(632) = pv(5)*pv(51)
 v(633) = pv(6)*pv(51)
 v(634) = pv(7)*pv(51)
 v(635) = pv(8)*pv(51)
 v(636) = pv(0)*pv(52)
 v(637) = pv(1)*pv(52)
 v(638) = pv(2)*pv(52)
 v(639) = pv(3)*pv(52)
 v(640) = pv(4)*pv(52)
 v(641) = pv(5)*pv(52)
 v(642) = pv(6)*pv(52)
 v(643) = pv(0)*pv(53)
 v(644) = pv(1)*pv(53)
 v(645) = pv(2)*pv(53)
 v(646) = pv(3)*pv(53)
 v(647) = pv(4)*pv(53)
 v(648) = pv(5)*pv(53)
 v(649) = pv(6)*pv(53)
 v(650) = pv(0)*pv(54)
 v(651) = pv(1)*pv(54)
 v(652) = pv(2)*pv(54)
 v(653) = pv(3)*pv(54)
 v(654) = pv(4)*pv(54)
 v(655) = pv(5)*pv(54)
 v(656) = pv(6)*pv(54)
 v(657) = pv(0)*pv(55)
 v(658) = pv(1)*pv(55)
 v(659) = pv(2)*pv(55)
 v(660) = pv(3)*pv(55)
 v(661) = pv(4)*pv(55)
 v(662) = pv(5)*pv(55)
 v(663) = pv(6)*pv(55)
 v(664:1266) = pv(233:835)
endif
if (6.le.mxd) then
 stop 'mg5211: degree 6 not implemented'
endif
return
END SUBROUTINE mg5211_secs
