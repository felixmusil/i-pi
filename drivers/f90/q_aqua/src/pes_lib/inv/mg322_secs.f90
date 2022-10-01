SUBROUTINE mg322_secs (mxd, r, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: v(0:)
!! Note: We stop at degree 7 for now
!-----------------------------------------------------------------------
real (kind=wp) :: pv(0:size(v)-1) !! an upper bound size
if (size(r).ne.nk*nk.or.size(v).ne.mg322_nsc(mxd)) then
 stop 'mg322_secs: bad dimensions'
endif
call mg322_isecs (mxd, r, pv)
v(0) = 1
if (2.le.mxd) then
 v(1:5) = pv(0:4)
endif
if (3.le.mxd) then
 v(6:35) = pv(5:34)
endif
if (4.le.mxd) then
 v(36) = pv(0)*pv(0)
 v(37) = pv(0)*pv(1)
 v(38) = pv(1)*pv(1)
 v(39) = pv(0)*pv(2)
 v(40) = pv(1)*pv(2)
 v(41) = pv(2)*pv(2)
 v(42) = pv(0)*pv(3)
 v(43) = pv(1)*pv(3)
 v(44) = pv(2)*pv(3)
 v(45) = pv(0)*pv(4)
 v(46) = pv(1)*pv(4)
 v(47) = pv(2)*pv(4)
 v(48) = pv(3)*pv(4)
 v(49:112) = pv(35:98)
endif
if (5.le.mxd) then
 v(113) = pv(3)*pv(5)
 v(114) = pv(4)*pv(5)
 v(115) = pv(1)*pv(6)
 v(116) = pv(4)*pv(6)
 v(117) = pv(0)*pv(7)
 v(118) = pv(1)*pv(7)
 v(119) = pv(2)*pv(7)
 v(120) = pv(4)*pv(7)
 v(121) = pv(0)*pv(8)
 v(122) = pv(1)*pv(8)
 v(123) = pv(2)*pv(8)
 v(124) = pv(3)*pv(8)
 v(125) = pv(4)*pv(8)
 v(126) = pv(0)*pv(9)
 v(127) = pv(1)*pv(9)
 v(128) = pv(2)*pv(9)
 v(129) = pv(3)*pv(9)
 v(130) = pv(4)*pv(9)
 v(131) = pv(0)*pv(10)
 v(132) = pv(1)*pv(10)
 v(133) = pv(2)*pv(10)
 v(134) = pv(3)*pv(10)
 v(135) = pv(4)*pv(10)
 v(136) = pv(0)*pv(11)
 v(137) = pv(3)*pv(11)
 v(138) = pv(4)*pv(11)
 v(139) = pv(3)*pv(12)
 v(140) = pv(4)*pv(12)
 v(141) = pv(0)*pv(13)
 v(142) = pv(4)*pv(13)
 v(143) = pv(0)*pv(14)
 v(144) = pv(1)*pv(14)
 v(145) = pv(2)*pv(14)
 v(146) = pv(3)*pv(14)
 v(147) = pv(4)*pv(14)
 v(148) = pv(0)*pv(15)
 v(149) = pv(1)*pv(15)
 v(150) = pv(2)*pv(15)
 v(151) = pv(3)*pv(15)
 v(152) = pv(4)*pv(15)
 v(153) = pv(0)*pv(16)
 v(154) = pv(3)*pv(16)
 v(155) = pv(0)*pv(17)
 v(156) = pv(1)*pv(17)
 v(157) = pv(3)*pv(17)
 v(158) = pv(0)*pv(18)
 v(159) = pv(1)*pv(18)
 v(160) = pv(2)*pv(18)
 v(161) = pv(3)*pv(18)
 v(162) = pv(0)*pv(19)
 v(163) = pv(1)*pv(19)
 v(164) = pv(2)*pv(19)
 v(165) = pv(3)*pv(19)
 v(166) = pv(4)*pv(19)
 v(167) = pv(0)*pv(20)
 v(168) = pv(1)*pv(20)
 v(169) = pv(2)*pv(20)
 v(170) = pv(3)*pv(20)
 v(171) = pv(4)*pv(20)
 v(172) = pv(0)*pv(21)
 v(173) = pv(1)*pv(21)
 v(174) = pv(2)*pv(21)
 v(175) = pv(3)*pv(21)
 v(176) = pv(4)*pv(21)
 v(177) = pv(0)*pv(22)
 v(178) = pv(1)*pv(22)
 v(179) = pv(2)*pv(22)
 v(180) = pv(3)*pv(22)
 v(181) = pv(4)*pv(22)
 v(182) = pv(0)*pv(23)
 v(183) = pv(1)*pv(23)
 v(184) = pv(2)*pv(23)
 v(185) = pv(3)*pv(23)
 v(186) = pv(4)*pv(23)
 v(187) = pv(0)*pv(24)
 v(188) = pv(1)*pv(24)
 v(189) = pv(2)*pv(24)
 v(190) = pv(4)*pv(24)
 v(191) = pv(0)*pv(25)
 v(192) = pv(1)*pv(25)
 v(193) = pv(2)*pv(25)
 v(194) = pv(4)*pv(25)
 v(195) = pv(0)*pv(26)
 v(196) = pv(1)*pv(26)
 v(197) = pv(2)*pv(26)
 v(198) = pv(3)*pv(26)
 v(199) = pv(0)*pv(27)
 v(200) = pv(1)*pv(27)
 v(201) = pv(2)*pv(27)
 v(202) = pv(0)*pv(28)
 v(203) = pv(1)*pv(28)
 v(204) = pv(2)*pv(28)
 v(205) = pv(0)*pv(29)
 v(206) = pv(1)*pv(29)
 v(207) = pv(2)*pv(29)
 v(208) = pv(3)*pv(29)
 v(209) = pv(4)*pv(29)
 v(210) = pv(0)*pv(30)
 v(211) = pv(1)*pv(30)
 v(212) = pv(2)*pv(30)
 v(213) = pv(3)*pv(30)
 v(214) = pv(0)*pv(31)
 v(215) = pv(1)*pv(31)
 v(216) = pv(2)*pv(31)
 v(217) = pv(3)*pv(31)
 v(218) = pv(4)*pv(31)
 v(219) = pv(0)*pv(32)
 v(220) = pv(1)*pv(32)
 v(221) = pv(2)*pv(32)
 v(222) = pv(0)*pv(33)
 v(223) = pv(1)*pv(33)
 v(224) = pv(2)*pv(33)
 v(225) = pv(0)*pv(34)
 v(226) = pv(1)*pv(34)
 v(227) = pv(2)*pv(34)
 v(228:301) = pv(99:172)
endif
if (6.le.mxd) then
 v(302) = pv(9)*pv(9)
 v(303) = pv(5)*pv(10)
 v(304) = pv(6)*pv(10)
 v(305) = pv(7)*pv(10)
 v(306) = pv(8)*pv(10)
 v(307) = pv(9)*pv(10)
 v(308) = pv(10)*pv(10)
 v(309) = pv(5)*pv(12)
 v(310) = pv(5)*pv(13)
 v(311) = pv(5)*pv(14)
 v(312) = pv(10)*pv(14)
 v(313) = pv(5)*pv(15)
 v(314) = pv(6)*pv(15)
 v(315) = pv(7)*pv(15)
 v(316) = pv(8)*pv(15)
 v(317) = pv(9)*pv(15)
 v(318) = pv(10)*pv(15)
 v(319) = pv(15)*pv(15)
 v(320) = pv(8)*pv(16)
 v(321) = pv(10)*pv(16)
 v(322) = pv(5)*pv(17)
 v(323) = pv(7)*pv(17)
 v(324) = pv(8)*pv(17)
 v(325) = pv(9)*pv(17)
 v(326) = pv(10)*pv(17)
 v(327) = pv(7)*pv(18)
 v(328) = pv(8)*pv(18)
 v(329) = pv(9)*pv(18)
 v(330) = pv(10)*pv(18)
 v(331) = pv(14)*pv(18)
 v(332) = pv(15)*pv(18)
 v(333) = pv(7)*pv(19)
 v(334) = pv(8)*pv(19)
 v(335) = pv(9)*pv(19)
 v(336) = pv(10)*pv(19)
 v(337) = pv(14)*pv(19)
 v(338) = pv(15)*pv(19)
 v(339) = pv(5)*pv(20)
 v(340) = pv(7)*pv(20)
 v(341) = pv(8)*pv(20)
 v(342) = pv(9)*pv(20)
 v(343) = pv(10)*pv(20)
 v(344) = pv(11)*pv(20)
 v(345) = pv(12)*pv(20)
 v(346) = pv(14)*pv(20)
 v(347) = pv(15)*pv(20)
 v(348) = pv(5)*pv(21)
 v(349) = pv(6)*pv(21)
 v(350) = pv(7)*pv(21)
 v(351) = pv(8)*pv(21)
 v(352) = pv(9)*pv(21)
 v(353) = pv(10)*pv(21)
 v(354) = pv(14)*pv(21)
 v(355) = pv(15)*pv(21)
 v(356) = pv(21)*pv(21)
 v(357) = pv(5)*pv(22)
 v(358) = pv(7)*pv(22)
 v(359) = pv(8)*pv(22)
 v(360) = pv(9)*pv(22)
 v(361) = pv(10)*pv(22)
 v(362) = pv(11)*pv(22)
 v(363) = pv(14)*pv(22)
 v(364) = pv(15)*pv(22)
 v(365) = pv(16)*pv(22)
 v(366) = pv(18)*pv(22)
 v(367) = pv(19)*pv(22)
 v(368) = pv(20)*pv(22)
 v(369) = pv(21)*pv(22)
 v(370) = pv(22)*pv(22)
 v(371) = pv(5)*pv(23)
 v(372) = pv(6)*pv(23)
 v(373) = pv(7)*pv(23)
 v(374) = pv(8)*pv(23)
 v(375) = pv(9)*pv(23)
 v(376) = pv(10)*pv(23)
 v(377) = pv(11)*pv(23)
 v(378) = pv(12)*pv(23)
 v(379) = pv(13)*pv(23)
 v(380) = pv(14)*pv(23)
 v(381) = pv(15)*pv(23)
 v(382) = pv(16)*pv(23)
 v(383) = pv(17)*pv(23)
 v(384) = pv(18)*pv(23)
 v(385) = pv(19)*pv(23)
 v(386) = pv(20)*pv(23)
 v(387) = pv(21)*pv(23)
 v(388) = pv(22)*pv(23)
 v(389) = pv(23)*pv(23)
 v(390) = pv(18)*pv(24)
 v(391) = pv(19)*pv(24)
 v(392) = pv(20)*pv(24)
 v(393) = pv(21)*pv(24)
 v(394) = pv(22)*pv(24)
 v(395) = pv(23)*pv(24)
 v(396) = pv(5)*pv(25)
 v(397) = pv(10)*pv(25)
 v(398) = pv(18)*pv(25)
 v(399) = pv(19)*pv(25)
 v(400) = pv(20)*pv(25)
 v(401) = pv(21)*pv(25)
 v(402) = pv(22)*pv(25)
 v(403) = pv(23)*pv(25)
 v(404) = pv(7)*pv(26)
 v(405) = pv(8)*pv(26)
 v(406) = pv(9)*pv(26)
 v(407) = pv(10)*pv(26)
 v(408) = pv(14)*pv(26)
 v(409) = pv(15)*pv(26)
 v(410) = pv(24)*pv(26)
 v(411) = pv(25)*pv(26)
 v(412) = pv(5)*pv(27)
 v(413) = pv(9)*pv(27)
 v(414) = pv(10)*pv(27)
 v(415) = pv(22)*pv(27)
 v(416) = pv(5)*pv(28)
 v(417) = pv(8)*pv(28)
 v(418) = pv(9)*pv(28)
 v(419) = pv(10)*pv(28)
 v(420) = pv(11)*pv(28)
 v(421) = pv(12)*pv(28)
 v(422) = pv(14)*pv(28)
 v(423) = pv(15)*pv(28)
 v(424) = pv(19)*pv(28)
 v(425) = pv(20)*pv(28)
 v(426) = pv(21)*pv(28)
 v(427) = pv(22)*pv(28)
 v(428) = pv(23)*pv(28)
 v(429) = pv(5)*pv(29)
 v(430) = pv(6)*pv(29)
 v(431) = pv(7)*pv(29)
 v(432) = pv(8)*pv(29)
 v(433) = pv(9)*pv(29)
 v(434) = pv(10)*pv(29)
 v(435) = pv(11)*pv(29)
 v(436) = pv(12)*pv(29)
 v(437) = pv(13)*pv(29)
 v(438) = pv(14)*pv(29)
 v(439) = pv(15)*pv(29)
 v(440) = pv(18)*pv(29)
 v(441) = pv(19)*pv(29)
 v(442) = pv(20)*pv(29)
 v(443) = pv(21)*pv(29)
 v(444) = pv(22)*pv(29)
 v(445) = pv(23)*pv(29)
 v(446) = pv(24)*pv(29)
 v(447) = pv(25)*pv(29)
 v(448) = pv(26)*pv(29)
 v(449) = pv(5)*pv(30)
 v(450) = pv(6)*pv(30)
 v(451) = pv(7)*pv(30)
 v(452) = pv(8)*pv(30)
 v(453) = pv(9)*pv(30)
 v(454) = pv(10)*pv(30)
 v(455) = pv(11)*pv(30)
 v(456) = pv(12)*pv(30)
 v(457) = pv(13)*pv(30)
 v(458) = pv(14)*pv(30)
 v(459) = pv(15)*pv(30)
 v(460) = pv(22)*pv(30)
 v(461) = pv(23)*pv(30)
 v(462) = pv(24)*pv(30)
 v(463) = pv(25)*pv(30)
 v(464) = pv(29)*pv(30)
 v(465) = pv(5)*pv(31)
 v(466) = pv(6)*pv(31)
 v(467) = pv(7)*pv(31)
 v(468) = pv(8)*pv(31)
 v(469) = pv(9)*pv(31)
 v(470) = pv(10)*pv(31)
 v(471) = pv(11)*pv(31)
 v(472) = pv(12)*pv(31)
 v(473) = pv(13)*pv(31)
 v(474) = pv(14)*pv(31)
 v(475) = pv(15)*pv(31)
 v(476) = pv(16)*pv(31)
 v(477) = pv(17)*pv(31)
 v(478) = pv(18)*pv(31)
 v(479) = pv(19)*pv(31)
 v(480) = pv(20)*pv(31)
 v(481) = pv(21)*pv(31)
 v(482) = pv(22)*pv(31)
 v(483) = pv(23)*pv(31)
 v(484) = pv(24)*pv(31)
 v(485) = pv(25)*pv(31)
 v(486) = pv(26)*pv(31)
 v(487) = pv(28)*pv(31)
 v(488) = pv(29)*pv(31)
 v(489) = pv(30)*pv(31)
 v(490) = pv(31)*pv(31)
 v(491) = pv(5)*pv(32)
 v(492) = pv(8)*pv(32)
 v(493) = pv(9)*pv(32)
 v(494) = pv(10)*pv(32)
 v(495) = pv(11)*pv(32)
 v(496) = pv(12)*pv(32)
 v(497) = pv(14)*pv(32)
 v(498) = pv(15)*pv(32)
 v(499) = pv(16)*pv(32)
 v(500) = pv(19)*pv(32)
 v(501) = pv(20)*pv(32)
 v(502) = pv(21)*pv(32)
 v(503) = pv(22)*pv(32)
 v(504) = pv(23)*pv(32)
 v(505) = pv(5)*pv(33)
 v(506) = pv(6)*pv(33)
 v(507) = pv(8)*pv(33)
 v(508) = pv(9)*pv(33)
 v(509) = pv(10)*pv(33)
 v(510) = pv(11)*pv(33)
 v(511) = pv(12)*pv(33)
 v(512) = pv(13)*pv(33)
 v(513) = pv(14)*pv(33)
 v(514) = pv(15)*pv(33)
 v(515) = pv(19)*pv(33)
 v(516) = pv(20)*pv(33)
 v(517) = pv(21)*pv(33)
 v(518) = pv(22)*pv(33)
 v(519) = pv(23)*pv(33)
 v(520) = pv(5)*pv(34)
 v(521) = pv(8)*pv(34)
 v(522) = pv(9)*pv(34)
 v(523) = pv(10)*pv(34)
 v(524) = pv(11)*pv(34)
 v(525) = pv(12)*pv(34)
 v(526) = pv(14)*pv(34)
 v(527) = pv(15)*pv(34)
 v(528) = pv(19)*pv(34)
 v(529) = pv(20)*pv(34)
 v(530) = pv(21)*pv(34)
 v(531) = pv(22)*pv(34)
 v(532) = pv(23)*pv(34)
 v(533) = pv(0)*pv(35)
 v(534) = pv(1)*pv(35)
 v(535) = pv(4)*pv(35)
 v(536) = pv(3)*pv(36)
 v(537) = pv(4)*pv(36)
 v(538) = pv(0)*pv(37)
 v(539) = pv(2)*pv(37)
 v(540) = pv(4)*pv(37)
 v(541) = pv(0)*pv(38)
 v(542) = pv(1)*pv(38)
 v(543) = pv(2)*pv(38)
 v(544) = pv(3)*pv(38)
 v(545) = pv(4)*pv(38)
 v(546) = pv(1)*pv(39)
 v(547) = pv(2)*pv(39)
 v(548) = pv(4)*pv(39)
 v(549) = pv(0)*pv(40)
 v(550) = pv(1)*pv(40)
 v(551) = pv(2)*pv(40)
 v(552) = pv(3)*pv(40)
 v(553) = pv(4)*pv(40)
 v(554) = pv(0)*pv(41)
 v(555) = pv(1)*pv(41)
 v(556) = pv(2)*pv(41)
 v(557) = pv(4)*pv(41)
 v(558) = pv(0)*pv(42)
 v(559) = pv(4)*pv(42)
 v(560) = pv(0)*pv(43)
 v(561) = pv(4)*pv(43)
 v(562) = pv(0)*pv(44)
 v(563) = pv(2)*pv(44)
 v(564) = pv(3)*pv(44)
 v(565) = pv(4)*pv(44)
 v(566) = pv(0)*pv(45)
 v(567) = pv(1)*pv(45)
 v(568) = pv(3)*pv(45)
 v(569) = pv(4)*pv(45)
 v(570) = pv(0)*pv(46)
 v(571) = pv(1)*pv(46)
 v(572) = pv(3)*pv(46)
 v(573) = pv(1)*pv(47)
 v(574) = pv(3)*pv(47)
 v(575) = pv(0)*pv(48)
 v(576) = pv(1)*pv(48)
 v(577) = pv(2)*pv(48)
 v(578) = pv(0)*pv(49)
 v(579) = pv(1)*pv(49)
 v(580) = pv(2)*pv(49)
 v(581) = pv(3)*pv(49)
 v(582) = pv(0)*pv(50)
 v(583) = pv(1)*pv(50)
 v(584) = pv(2)*pv(50)
 v(585) = pv(3)*pv(50)
 v(586) = pv(3)*pv(51)
 v(587) = pv(4)*pv(51)
 v(588) = pv(3)*pv(52)
 v(589) = pv(0)*pv(53)
 v(590) = pv(1)*pv(53)
 v(591) = pv(2)*pv(53)
 v(592) = pv(3)*pv(53)
 v(593) = pv(0)*pv(54)
 v(594) = pv(1)*pv(54)
 v(595) = pv(2)*pv(54)
 v(596) = pv(3)*pv(54)
 v(597) = pv(4)*pv(54)
 v(598) = pv(1)*pv(55)
 v(599) = pv(2)*pv(55)
 v(600) = pv(3)*pv(55)
 v(601) = pv(4)*pv(55)
 v(602) = pv(0)*pv(56)
 v(603) = pv(1)*pv(56)
 v(604) = pv(2)*pv(56)
 v(605) = pv(4)*pv(56)
 v(606) = pv(0)*pv(57)
 v(607) = pv(1)*pv(57)
 v(608) = pv(2)*pv(57)
 v(609) = pv(3)*pv(57)
 v(610) = pv(4)*pv(57)
 v(611) = pv(0)*pv(58)
 v(612) = pv(1)*pv(58)
 v(613) = pv(2)*pv(58)
 v(614) = pv(3)*pv(58)
 v(615) = pv(4)*pv(58)
 v(616) = pv(0)*pv(59)
 v(617) = pv(2)*pv(59)
 v(618) = pv(3)*pv(59)
 v(619) = pv(0)*pv(60)
 v(620) = pv(1)*pv(60)
 v(621) = pv(3)*pv(60)
 v(622) = pv(4)*pv(60)
 v(623) = pv(0)*pv(61)
 v(624) = pv(1)*pv(61)
 v(625) = pv(2)*pv(61)
 v(626) = pv(3)*pv(61)
 v(627) = pv(4)*pv(61)
 v(628) = pv(0)*pv(62)
 v(629) = pv(1)*pv(62)
 v(630) = pv(2)*pv(62)
 v(631) = pv(4)*pv(62)
 v(632) = pv(0)*pv(63)
 v(633) = pv(1)*pv(63)
 v(634) = pv(2)*pv(63)
 v(635) = pv(3)*pv(63)
 v(636) = pv(4)*pv(63)
 v(637) = pv(0)*pv(64)
 v(638) = pv(1)*pv(64)
 v(639) = pv(2)*pv(64)
 v(640) = pv(3)*pv(64)
 v(641) = pv(0)*pv(65)
 v(642) = pv(1)*pv(65)
 v(643) = pv(2)*pv(65)
 v(644) = pv(4)*pv(65)
 v(645) = pv(0)*pv(66)
 v(646) = pv(1)*pv(66)
 v(647) = pv(2)*pv(66)
 v(648) = pv(4)*pv(66)
 v(649) = pv(0)*pv(67)
 v(650) = pv(1)*pv(67)
 v(651) = pv(2)*pv(67)
 v(652) = pv(4)*pv(67)
 v(653) = pv(0)*pv(68)
 v(654) = pv(1)*pv(68)
 v(655) = pv(2)*pv(68)
 v(656) = pv(4)*pv(68)
 v(657) = pv(0)*pv(69)
 v(658) = pv(1)*pv(69)
 v(659) = pv(2)*pv(69)
 v(660) = pv(3)*pv(69)
 v(661) = pv(0)*pv(70)
 v(662) = pv(1)*pv(70)
 v(663) = pv(2)*pv(70)
 v(664) = pv(0)*pv(71)
 v(665) = pv(1)*pv(71)
 v(666) = pv(2)*pv(71)
 v(667) = pv(0)*pv(72)
 v(668) = pv(1)*pv(72)
 v(669) = pv(2)*pv(72)
 v(670) = pv(4)*pv(72)
 v(671) = pv(0)*pv(73)
 v(672) = pv(1)*pv(73)
 v(673) = pv(2)*pv(73)
 v(674) = pv(3)*pv(73)
 v(675) = pv(0)*pv(74)
 v(676) = pv(1)*pv(74)
 v(677) = pv(2)*pv(74)
 v(678) = pv(0)*pv(75)
 v(679) = pv(1)*pv(75)
 v(680) = pv(2)*pv(75)
 v(681) = pv(3)*pv(75)
 v(682) = pv(0)*pv(76)
 v(683) = pv(1)*pv(76)
 v(684) = pv(2)*pv(76)
 v(685) = pv(3)*pv(76)
 v(686) = pv(4)*pv(76)
 v(687) = pv(0)*pv(77)
 v(688) = pv(1)*pv(77)
 v(689) = pv(2)*pv(77)
 v(690) = pv(0)*pv(78)
 v(691) = pv(1)*pv(78)
 v(692) = pv(2)*pv(78)
 v(693) = pv(3)*pv(78)
 v(694) = pv(4)*pv(78)
 v(695) = pv(0)*pv(79)
 v(696) = pv(1)*pv(79)
 v(697) = pv(2)*pv(79)
 v(698) = pv(3)*pv(79)
 v(699) = pv(4)*pv(79)
 v(700) = pv(0)*pv(80)
 v(701) = pv(1)*pv(80)
 v(702) = pv(2)*pv(80)
 v(703) = pv(3)*pv(80)
 v(704) = pv(0)*pv(81)
 v(705) = pv(1)*pv(81)
 v(706) = pv(2)*pv(81)
 v(707) = pv(0)*pv(82)
 v(708) = pv(1)*pv(82)
 v(709) = pv(2)*pv(82)
 v(710) = pv(4)*pv(82)
 v(711) = pv(0)*pv(83)
 v(712) = pv(1)*pv(83)
 v(713) = pv(2)*pv(83)
 v(714) = pv(3)*pv(83)
 v(715) = pv(0)*pv(84)
 v(716) = pv(1)*pv(84)
 v(717) = pv(2)*pv(84)
 v(718) = pv(3)*pv(84)
 v(719) = pv(0)*pv(85)
 v(720) = pv(1)*pv(85)
 v(721) = pv(2)*pv(85)
 v(722) = pv(4)*pv(85)
 v(723) = pv(0)*pv(86)
 v(724) = pv(1)*pv(86)
 v(725) = pv(2)*pv(86)
 v(726) = pv(3)*pv(86)
 v(727) = pv(4)*pv(86)
 v(728) = pv(0)*pv(87)
 v(729) = pv(1)*pv(87)
 v(730) = pv(2)*pv(87)
 v(731) = pv(3)*pv(87)
 v(732) = pv(4)*pv(87)
 v(733) = pv(0)*pv(88)
 v(734) = pv(1)*pv(88)
 v(735) = pv(2)*pv(88)
 v(736) = pv(0)*pv(89)
 v(737) = pv(1)*pv(89)
 v(738) = pv(2)*pv(89)
 v(739) = pv(3)*pv(89)
 v(740) = pv(0)*pv(90)
 v(741) = pv(1)*pv(90)
 v(742) = pv(2)*pv(90)
 v(743) = pv(3)*pv(90)
 v(744) = pv(4)*pv(90)
 v(745) = pv(0)*pv(91)
 v(746) = pv(1)*pv(91)
 v(747) = pv(2)*pv(91)
 v(748) = pv(3)*pv(91)
 v(749) = pv(4)*pv(91)
 v(750) = pv(0)*pv(92)
 v(751) = pv(1)*pv(92)
 v(752) = pv(2)*pv(92)
 v(753) = pv(3)*pv(92)
 v(754) = pv(0)*pv(93)
 v(755) = pv(1)*pv(93)
 v(756) = pv(2)*pv(93)
 v(757) = pv(3)*pv(93)
 v(758) = pv(0)*pv(94)
 v(759) = pv(1)*pv(94)
 v(760) = pv(2)*pv(94)
 v(761) = pv(4)*pv(94)
 v(762) = pv(0)*pv(95)
 v(763) = pv(1)*pv(95)
 v(764) = pv(2)*pv(95)
 v(765) = pv(0)*pv(96)
 v(766) = pv(1)*pv(96)
 v(767) = pv(2)*pv(96)
 v(768) = pv(0)*pv(97)
 v(769) = pv(1)*pv(97)
 v(770) = pv(2)*pv(97)
 v(771) = pv(4)*pv(97)
 v(772) = pv(0)*pv(98)
 v(773) = pv(1)*pv(98)
 v(774) = pv(2)*pv(98)
 v(775) = pv(3)*pv(98)
 v(776) = pv(4)*pv(98)
 v(777:798) = pv(173:194)
endif
if (7.le.mxd) then
 v(799) = pv(0)*pv(0)*pv(22)
 v(800) = pv(0)*pv(0)*pv(23)
 v(801) = pv(0)*pv(1)*pv(23)
 v(802) = pv(0)*pv(1)*pv(34)
 v(803) = pv(23)*pv(35)
 v(804) = pv(31)*pv(36)
 v(805) = pv(6)*pv(38)
 v(806) = pv(10)*pv(38)
 v(807) = pv(14)*pv(38)
 v(808) = pv(15)*pv(38)
 v(809) = pv(23)*pv(38)
 v(810) = pv(31)*pv(38)
 v(811) = pv(34)*pv(38)
 v(812) = pv(5)*pv(39)
 v(813) = pv(8)*pv(39)
 v(814) = pv(10)*pv(39)
 v(815) = pv(5)*pv(40)
 v(816) = pv(9)*pv(40)
 v(817) = pv(10)*pv(40)
 v(818) = pv(31)*pv(40)
 v(819) = pv(5)*pv(41)
 v(820) = pv(8)*pv(41)
 v(821) = pv(9)*pv(41)
 v(822) = pv(10)*pv(41)
 v(823) = pv(11)*pv(41)
 v(824) = pv(15)*pv(41)
 v(825) = pv(22)*pv(41)
 v(826) = pv(31)*pv(41)
 v(827) = pv(5)*pv(42)
 v(828) = pv(10)*pv(42)
 v(829) = pv(11)*pv(42)
 v(830) = pv(9)*pv(43)
 v(831) = pv(5)*pv(44)
 v(832) = pv(6)*pv(44)
 v(833) = pv(7)*pv(44)
 v(834) = pv(8)*pv(44)
 v(835) = pv(9)*pv(44)
 v(836) = pv(10)*pv(44)
 v(837) = pv(11)*pv(44)
 v(838) = pv(12)*pv(44)
 v(839) = pv(15)*pv(44)
 v(840) = pv(31)*pv(44)
 v(841) = pv(34)*pv(44)
 v(842) = pv(5)*pv(45)
 v(843) = pv(7)*pv(45)
 v(844) = pv(8)*pv(45)
 v(845) = pv(10)*pv(45)
 v(846) = pv(11)*pv(45)
 v(847) = pv(12)*pv(45)
 v(848) = pv(15)*pv(45)
 v(849) = pv(31)*pv(45)
 v(850) = pv(8)*pv(46)
 v(851) = pv(10)*pv(46)
 v(852) = pv(15)*pv(46)
 v(853) = pv(5)*pv(47)
 v(854) = pv(8)*pv(47)
 v(855) = pv(10)*pv(47)
 v(856) = pv(5)*pv(48)
 v(857) = pv(9)*pv(48)
 v(858) = pv(10)*pv(48)
 v(859) = pv(11)*pv(48)
 v(860) = pv(5)*pv(49)
 v(861) = pv(6)*pv(49)
 v(862) = pv(7)*pv(49)
 v(863) = pv(8)*pv(49)
 v(864) = pv(9)*pv(49)
 v(865) = pv(10)*pv(49)
 v(866) = pv(11)*pv(49)
 v(867) = pv(12)*pv(49)
 v(868) = pv(15)*pv(49)
 v(869) = pv(5)*pv(50)
 v(870) = pv(6)*pv(50)
 v(871) = pv(7)*pv(50)
 v(872) = pv(8)*pv(50)
 v(873) = pv(9)*pv(50)
 v(874) = pv(10)*pv(50)
 v(875) = pv(11)*pv(50)
 v(876) = pv(12)*pv(50)
 v(877) = pv(14)*pv(50)
 v(878) = pv(15)*pv(50)
 v(879) = pv(31)*pv(51)
 v(880) = pv(8)*pv(54)
 v(881) = pv(10)*pv(54)
 v(882) = pv(13)*pv(54)
 v(883) = pv(14)*pv(54)
 v(884) = pv(15)*pv(54)
 v(885) = pv(16)*pv(54)
 v(886) = pv(20)*pv(54)
 v(887) = pv(22)*pv(54)
 v(888) = pv(23)*pv(54)
 v(889) = pv(31)*pv(54)
 v(890) = pv(34)*pv(54)
 v(891) = pv(5)*pv(55)
 v(892) = pv(8)*pv(55)
 v(893) = pv(10)*pv(55)
 v(894) = pv(11)*pv(55)
 v(895) = pv(19)*pv(55)
 v(896) = pv(22)*pv(55)
 v(897) = pv(23)*pv(55)
 v(898) = pv(31)*pv(55)
 v(899) = pv(34)*pv(55)
 v(900) = pv(5)*pv(56)
 v(901) = pv(9)*pv(56)
 v(902) = pv(10)*pv(56)
 v(903) = pv(11)*pv(56)
 v(904) = pv(12)*pv(56)
 v(905) = pv(15)*pv(56)
 v(906) = pv(22)*pv(56)
 v(907) = pv(5)*pv(57)
 v(908) = pv(6)*pv(57)
 v(909) = pv(7)*pv(57)
 v(910) = pv(8)*pv(57)
 v(911) = pv(9)*pv(57)
 v(912) = pv(10)*pv(57)
 v(913) = pv(11)*pv(57)
 v(914) = pv(12)*pv(57)
 v(915) = pv(13)*pv(57)
 v(916) = pv(14)*pv(57)
 v(917) = pv(15)*pv(57)
 v(918) = pv(21)*pv(57)
 v(919) = pv(22)*pv(57)
 v(920) = pv(23)*pv(57)
 v(921) = pv(31)*pv(57)
 v(922) = pv(5)*pv(58)
 v(923) = pv(6)*pv(58)
 v(924) = pv(7)*pv(58)
 v(925) = pv(8)*pv(58)
 v(926) = pv(9)*pv(58)
 v(927) = pv(10)*pv(58)
 v(928) = pv(11)*pv(58)
 v(929) = pv(12)*pv(58)
 v(930) = pv(13)*pv(58)
 v(931) = pv(14)*pv(58)
 v(932) = pv(15)*pv(58)
 v(933) = pv(16)*pv(58)
 v(934) = pv(17)*pv(58)
 v(935) = pv(19)*pv(58)
 v(936) = pv(20)*pv(58)
 v(937) = pv(21)*pv(58)
 v(938) = pv(22)*pv(58)
 v(939) = pv(23)*pv(58)
 v(940) = pv(31)*pv(58)
 v(941) = pv(5)*pv(59)
 v(942) = pv(6)*pv(59)
 v(943) = pv(7)*pv(59)
 v(944) = pv(8)*pv(59)
 v(945) = pv(9)*pv(59)
 v(946) = pv(10)*pv(59)
 v(947) = pv(11)*pv(59)
 v(948) = pv(12)*pv(59)
 v(949) = pv(13)*pv(59)
 v(950) = pv(14)*pv(59)
 v(951) = pv(15)*pv(59)
 v(952) = pv(19)*pv(59)
 v(953) = pv(22)*pv(59)
 v(954) = pv(5)*pv(60)
 v(955) = pv(6)*pv(60)
 v(956) = pv(7)*pv(60)
 v(957) = pv(8)*pv(60)
 v(958) = pv(9)*pv(60)
 v(959) = pv(10)*pv(60)
 v(960) = pv(11)*pv(60)
 v(961) = pv(12)*pv(60)
 v(962) = pv(13)*pv(60)
 v(963) = pv(14)*pv(60)
 v(964) = pv(15)*pv(60)
 v(965) = pv(19)*pv(60)
 v(966) = pv(20)*pv(60)
 v(967) = pv(22)*pv(60)
 v(968) = pv(23)*pv(60)
 v(969) = pv(31)*pv(60)
 v(970) = pv(5)*pv(61)
 v(971) = pv(6)*pv(61)
 v(972) = pv(7)*pv(61)
 v(973) = pv(8)*pv(61)
 v(974) = pv(9)*pv(61)
 v(975) = pv(10)*pv(61)
 v(976) = pv(11)*pv(61)
 v(977) = pv(12)*pv(61)
 v(978) = pv(13)*pv(61)
 v(979) = pv(14)*pv(61)
 v(980) = pv(15)*pv(61)
 v(981) = pv(21)*pv(61)
 v(982) = pv(22)*pv(61)
 v(983) = pv(23)*pv(61)
 v(984) = pv(31)*pv(61)
 v(985) = pv(5)*pv(62)
 v(986) = pv(8)*pv(62)
 v(987) = pv(9)*pv(62)
 v(988) = pv(10)*pv(62)
 v(989) = pv(11)*pv(62)
 v(990) = pv(12)*pv(62)
 v(991) = pv(14)*pv(62)
 v(992) = pv(15)*pv(62)
 v(993) = pv(16)*pv(62)
 v(994) = pv(17)*pv(62)
 v(995) = pv(18)*pv(62)
 v(996) = pv(19)*pv(62)
 v(997) = pv(20)*pv(62)
 v(998) = pv(21)*pv(62)
 v(999) = pv(22)*pv(62)
 v(1000) = pv(23)*pv(62)
 v(1001) = pv(5)*pv(63)
 v(1002) = pv(6)*pv(63)
 v(1003) = pv(7)*pv(63)
 v(1004) = pv(8)*pv(63)
 v(1005) = pv(9)*pv(63)
 v(1006) = pv(10)*pv(63)
 v(1007) = pv(11)*pv(63)
 v(1008) = pv(12)*pv(63)
 v(1009) = pv(13)*pv(63)
 v(1010) = pv(14)*pv(63)
 v(1011) = pv(15)*pv(63)
 v(1012) = pv(16)*pv(63)
 v(1013) = pv(17)*pv(63)
 v(1014) = pv(18)*pv(63)
 v(1015) = pv(19)*pv(63)
 v(1016) = pv(20)*pv(63)
 v(1017) = pv(21)*pv(63)
 v(1018) = pv(22)*pv(63)
 v(1019) = pv(23)*pv(63)
 v(1020) = pv(28)*pv(63)
 v(1021) = pv(31)*pv(63)
 v(1022) = pv(34)*pv(63)
 v(1023) = pv(5)*pv(64)
 v(1024) = pv(6)*pv(64)
 v(1025) = pv(7)*pv(64)
 v(1026) = pv(8)*pv(64)
 v(1027) = pv(9)*pv(64)
 v(1028) = pv(10)*pv(64)
 v(1029) = pv(11)*pv(64)
 v(1030) = pv(12)*pv(64)
 v(1031) = pv(13)*pv(64)
 v(1032) = pv(14)*pv(64)
 v(1033) = pv(15)*pv(64)
 v(1034) = pv(19)*pv(64)
 v(1035) = pv(20)*pv(64)
 v(1036) = pv(21)*pv(64)
 v(1037) = pv(22)*pv(64)
 v(1038) = pv(23)*pv(64)
 v(1039) = pv(31)*pv(64)
 v(1040) = pv(9)*pv(65)
 v(1041) = pv(10)*pv(65)
 v(1042) = pv(14)*pv(65)
 v(1043) = pv(15)*pv(65)
 v(1044) = pv(17)*pv(65)
 v(1045) = pv(19)*pv(65)
 v(1046) = pv(20)*pv(65)
 v(1047) = pv(22)*pv(65)
 v(1048) = pv(5)*pv(66)
 v(1049) = pv(10)*pv(66)
 v(1050) = pv(11)*pv(66)
 v(1051) = pv(12)*pv(66)
 v(1052) = pv(15)*pv(66)
 v(1053) = pv(19)*pv(66)
 v(1054) = pv(21)*pv(66)
 v(1055) = pv(22)*pv(66)
 v(1056) = pv(26)*pv(66)
 v(1057) = pv(5)*pv(67)
 v(1058) = pv(9)*pv(67)
 v(1059) = pv(10)*pv(67)
 v(1060) = pv(11)*pv(67)
 v(1061) = pv(12)*pv(67)
 v(1062) = pv(16)*pv(67)
 v(1063) = pv(19)*pv(67)
 v(1064) = pv(20)*pv(67)
 v(1065) = pv(22)*pv(67)
 v(1066) = pv(23)*pv(67)
 v(1067) = pv(26)*pv(67)
 v(1068) = pv(5)*pv(68)
 v(1069) = pv(6)*pv(68)
 v(1070) = pv(8)*pv(68)
 v(1071) = pv(9)*pv(68)
 v(1072) = pv(10)*pv(68)
 v(1073) = pv(11)*pv(68)
 v(1074) = pv(12)*pv(68)
 v(1075) = pv(13)*pv(68)
 v(1076) = pv(14)*pv(68)
 v(1077) = pv(15)*pv(68)
 v(1078) = pv(16)*pv(68)
 v(1079) = pv(17)*pv(68)
 v(1080) = pv(18)*pv(68)
 v(1081) = pv(19)*pv(68)
 v(1082) = pv(20)*pv(68)
 v(1083) = pv(21)*pv(68)
 v(1084) = pv(22)*pv(68)
 v(1085) = pv(23)*pv(68)
 v(1086) = pv(31)*pv(68)
 v(1087) = pv(8)*pv(69)
 v(1088) = pv(10)*pv(69)
 v(1089) = pv(14)*pv(69)
 v(1090) = pv(20)*pv(69)
 v(1091) = pv(21)*pv(69)
 v(1092) = pv(22)*pv(69)
 v(1093) = pv(23)*pv(69)
 v(1094) = pv(24)*pv(69)
 v(1095) = pv(22)*pv(70)
 v(1096) = pv(5)*pv(71)
 v(1097) = pv(10)*pv(72)
 v(1098) = pv(19)*pv(72)
 v(1099) = pv(22)*pv(72)
 v(1100) = pv(26)*pv(72)
 v(1101) = pv(8)*pv(73)
 v(1102) = pv(10)*pv(73)
 v(1103) = pv(22)*pv(73)
 v(1104) = pv(24)*pv(73)
 v(1105) = pv(5)*pv(74)
 v(1106) = pv(9)*pv(74)
 v(1107) = pv(10)*pv(74)
 v(1108) = pv(22)*pv(74)
 v(1109) = pv(23)*pv(74)
 v(1110) = pv(5)*pv(75)
 v(1111) = pv(6)*pv(75)
 v(1112) = pv(8)*pv(75)
 v(1113) = pv(9)*pv(75)
 v(1114) = pv(10)*pv(75)
 v(1115) = pv(11)*pv(75)
 v(1116) = pv(22)*pv(75)
 v(1117) = pv(23)*pv(75)
 v(1118) = pv(5)*pv(76)
 v(1119) = pv(7)*pv(76)
 v(1120) = pv(8)*pv(76)
 v(1121) = pv(9)*pv(76)
 v(1122) = pv(10)*pv(76)
 v(1123) = pv(11)*pv(76)
 v(1124) = pv(12)*pv(76)
 v(1125) = pv(14)*pv(76)
 v(1126) = pv(15)*pv(76)
 v(1127) = pv(19)*pv(76)
 v(1128) = pv(20)*pv(76)
 v(1129) = pv(21)*pv(76)
 v(1130) = pv(22)*pv(76)
 v(1131) = pv(23)*pv(76)
 v(1132) = pv(24)*pv(76)
 v(1133) = pv(26)*pv(76)
 v(1134) = pv(9)*pv(77)
 v(1135) = pv(10)*pv(77)
 v(1136) = pv(11)*pv(77)
 v(1137) = pv(19)*pv(77)
 v(1138) = pv(22)*pv(77)
 v(1139) = pv(23)*pv(77)
 v(1140) = pv(5)*pv(78)
 v(1141) = pv(6)*pv(78)
 v(1142) = pv(7)*pv(78)
 v(1143) = pv(8)*pv(78)
 v(1144) = pv(9)*pv(78)
 v(1145) = pv(10)*pv(78)
 v(1146) = pv(11)*pv(78)
 v(1147) = pv(12)*pv(78)
 v(1148) = pv(14)*pv(78)
 v(1149) = pv(15)*pv(78)
 v(1150) = pv(16)*pv(78)
 v(1151) = pv(19)*pv(78)
 v(1152) = pv(20)*pv(78)
 v(1153) = pv(21)*pv(78)
 v(1154) = pv(22)*pv(78)
 v(1155) = pv(23)*pv(78)
 v(1156) = pv(24)*pv(78)
 v(1157) = pv(25)*pv(78)
 v(1158) = pv(5)*pv(79)
 v(1159) = pv(6)*pv(79)
 v(1160) = pv(7)*pv(79)
 v(1161) = pv(8)*pv(79)
 v(1162) = pv(9)*pv(79)
 v(1163) = pv(10)*pv(79)
 v(1164) = pv(11)*pv(79)
 v(1165) = pv(12)*pv(79)
 v(1166) = pv(13)*pv(79)
 v(1167) = pv(14)*pv(79)
 v(1168) = pv(15)*pv(79)
 v(1169) = pv(16)*pv(79)
 v(1170) = pv(19)*pv(79)
 v(1171) = pv(20)*pv(79)
 v(1172) = pv(21)*pv(79)
 v(1173) = pv(22)*pv(79)
 v(1174) = pv(23)*pv(79)
 v(1175) = pv(24)*pv(79)
 v(1176) = pv(25)*pv(79)
 v(1177) = pv(26)*pv(79)
 v(1178) = pv(31)*pv(79)
 v(1179) = pv(5)*pv(80)
 v(1180) = pv(8)*pv(80)
 v(1181) = pv(9)*pv(80)
 v(1182) = pv(10)*pv(80)
 v(1183) = pv(11)*pv(80)
 v(1184) = pv(12)*pv(80)
 v(1185) = pv(13)*pv(80)
 v(1186) = pv(14)*pv(80)
 v(1187) = pv(15)*pv(80)
 v(1188) = pv(22)*pv(80)
 v(1189) = pv(23)*pv(80)
 v(1190) = pv(24)*pv(80)
 v(1191) = pv(5)*pv(81)
 v(1192) = pv(8)*pv(81)
 v(1193) = pv(9)*pv(81)
 v(1194) = pv(10)*pv(81)
 v(1195) = pv(11)*pv(81)
 v(1196) = pv(12)*pv(81)
 v(1197) = pv(23)*pv(81)
 v(1198) = pv(5)*pv(82)
 v(1199) = pv(6)*pv(82)
 v(1200) = pv(8)*pv(82)
 v(1201) = pv(9)*pv(82)
 v(1202) = pv(10)*pv(82)
 v(1203) = pv(11)*pv(82)
 v(1204) = pv(12)*pv(82)
 v(1205) = pv(14)*pv(82)
 v(1206) = pv(15)*pv(82)
 v(1207) = pv(19)*pv(82)
 v(1208) = pv(20)*pv(82)
 v(1209) = pv(22)*pv(82)
 v(1210) = pv(23)*pv(82)
 v(1211) = pv(5)*pv(83)
 v(1212) = pv(6)*pv(83)
 v(1213) = pv(7)*pv(83)
 v(1214) = pv(8)*pv(83)
 v(1215) = pv(9)*pv(83)
 v(1216) = pv(10)*pv(83)
 v(1217) = pv(11)*pv(83)
 v(1218) = pv(12)*pv(83)
 v(1219) = pv(14)*pv(83)
 v(1220) = pv(15)*pv(83)
 v(1221) = pv(16)*pv(83)
 v(1222) = pv(19)*pv(83)
 v(1223) = pv(21)*pv(83)
 v(1224) = pv(22)*pv(83)
 v(1225) = pv(23)*pv(83)
 v(1226) = pv(25)*pv(83)
 v(1227) = pv(5)*pv(84)
 v(1228) = pv(6)*pv(84)
 v(1229) = pv(7)*pv(84)
 v(1230) = pv(8)*pv(84)
 v(1231) = pv(9)*pv(84)
 v(1232) = pv(10)*pv(84)
 v(1233) = pv(11)*pv(84)
 v(1234) = pv(12)*pv(84)
 v(1235) = pv(13)*pv(84)
 v(1236) = pv(14)*pv(84)
 v(1237) = pv(15)*pv(84)
 v(1238) = pv(21)*pv(84)
 v(1239) = pv(22)*pv(84)
 v(1240) = pv(23)*pv(84)
 v(1241) = pv(24)*pv(84)
 v(1242) = pv(5)*pv(85)
 v(1243) = pv(6)*pv(85)
 v(1244) = pv(8)*pv(85)
 v(1245) = pv(9)*pv(85)
 v(1246) = pv(10)*pv(85)
 v(1247) = pv(11)*pv(85)
 v(1248) = pv(12)*pv(85)
 v(1249) = pv(13)*pv(85)
 v(1250) = pv(14)*pv(85)
 v(1251) = pv(15)*pv(85)
 v(1252) = pv(16)*pv(85)
 v(1253) = pv(17)*pv(85)
 v(1254) = pv(18)*pv(85)
 v(1255) = pv(19)*pv(85)
 v(1256) = pv(20)*pv(85)
 v(1257) = pv(21)*pv(85)
 v(1258) = pv(22)*pv(85)
 v(1259) = pv(23)*pv(85)
 v(1260) = pv(5)*pv(86)
 v(1261) = pv(6)*pv(86)
 v(1262) = pv(7)*pv(86)
 v(1263) = pv(8)*pv(86)
 v(1264) = pv(9)*pv(86)
 v(1265) = pv(10)*pv(86)
 v(1266) = pv(11)*pv(86)
 v(1267) = pv(12)*pv(86)
 v(1268) = pv(13)*pv(86)
 v(1269) = pv(14)*pv(86)
 v(1270) = pv(15)*pv(86)
 v(1271) = pv(17)*pv(86)
 v(1272) = pv(18)*pv(86)
 v(1273) = pv(19)*pv(86)
 v(1274) = pv(20)*pv(86)
 v(1275) = pv(21)*pv(86)
 v(1276) = pv(22)*pv(86)
 v(1277) = pv(23)*pv(86)
 v(1278) = pv(24)*pv(86)
 v(1279) = pv(25)*pv(86)
 v(1280) = pv(26)*pv(86)
 v(1281) = pv(29)*pv(86)
 v(1282) = pv(30)*pv(86)
 v(1283) = pv(31)*pv(86)
 v(1284) = pv(5)*pv(87)
 v(1285) = pv(6)*pv(87)
 v(1286) = pv(7)*pv(87)
 v(1287) = pv(8)*pv(87)
 v(1288) = pv(9)*pv(87)
 v(1289) = pv(10)*pv(87)
 v(1290) = pv(11)*pv(87)
 v(1291) = pv(12)*pv(87)
 v(1292) = pv(13)*pv(87)
 v(1293) = pv(14)*pv(87)
 v(1294) = pv(15)*pv(87)
 v(1295) = pv(16)*pv(87)
 v(1296) = pv(17)*pv(87)
 v(1297) = pv(18)*pv(87)
 v(1298) = pv(19)*pv(87)
 v(1299) = pv(20)*pv(87)
 v(1300) = pv(21)*pv(87)
 v(1301) = pv(22)*pv(87)
 v(1302) = pv(23)*pv(87)
 v(1303) = pv(24)*pv(87)
 v(1304) = pv(25)*pv(87)
 v(1305) = pv(26)*pv(87)
 v(1306) = pv(28)*pv(87)
 v(1307) = pv(29)*pv(87)
 v(1308) = pv(31)*pv(87)
 v(1309) = pv(5)*pv(88)
 v(1310) = pv(6)*pv(88)
 v(1311) = pv(7)*pv(88)
 v(1312) = pv(8)*pv(88)
 v(1313) = pv(9)*pv(88)
 v(1314) = pv(10)*pv(88)
 v(1315) = pv(11)*pv(88)
 v(1316) = pv(12)*pv(88)
 v(1317) = pv(13)*pv(88)
 v(1318) = pv(14)*pv(88)
 v(1319) = pv(15)*pv(88)
 v(1320) = pv(21)*pv(88)
 v(1321) = pv(22)*pv(88)
 v(1322) = pv(23)*pv(88)
 v(1323) = pv(5)*pv(89)
 v(1324) = pv(6)*pv(89)
 v(1325) = pv(7)*pv(89)
 v(1326) = pv(8)*pv(89)
 v(1327) = pv(9)*pv(89)
 v(1328) = pv(10)*pv(89)
 v(1329) = pv(11)*pv(89)
 v(1330) = pv(12)*pv(89)
 v(1331) = pv(13)*pv(89)
 v(1332) = pv(14)*pv(89)
 v(1333) = pv(15)*pv(89)
 v(1334) = pv(16)*pv(89)
 v(1335) = pv(17)*pv(89)
 v(1336) = pv(19)*pv(89)
 v(1337) = pv(20)*pv(89)
 v(1338) = pv(21)*pv(89)
 v(1339) = pv(22)*pv(89)
 v(1340) = pv(23)*pv(89)
 v(1341) = pv(24)*pv(89)
 v(1342) = pv(25)*pv(89)
 v(1343) = pv(29)*pv(89)
 v(1344) = pv(31)*pv(89)
 v(1345) = pv(5)*pv(90)
 v(1346) = pv(6)*pv(90)
 v(1347) = pv(7)*pv(90)
 v(1348) = pv(8)*pv(90)
 v(1349) = pv(9)*pv(90)
 v(1350) = pv(10)*pv(90)
 v(1351) = pv(11)*pv(90)
 v(1352) = pv(12)*pv(90)
 v(1353) = pv(13)*pv(90)
 v(1354) = pv(14)*pv(90)
 v(1355) = pv(15)*pv(90)
 v(1356) = pv(16)*pv(90)
 v(1357) = pv(17)*pv(90)
 v(1358) = pv(18)*pv(90)
 v(1359) = pv(19)*pv(90)
 v(1360) = pv(20)*pv(90)
 v(1361) = pv(21)*pv(90)
 v(1362) = pv(22)*pv(90)
 v(1363) = pv(23)*pv(90)
 v(1364) = pv(24)*pv(90)
 v(1365) = pv(25)*pv(90)
 v(1366) = pv(26)*pv(90)
 v(1367) = pv(28)*pv(90)
 v(1368) = pv(29)*pv(90)
 v(1369) = pv(30)*pv(90)
 v(1370) = pv(31)*pv(90)
 v(1371) = pv(5)*pv(91)
 v(1372) = pv(6)*pv(91)
 v(1373) = pv(7)*pv(91)
 v(1374) = pv(8)*pv(91)
 v(1375) = pv(9)*pv(91)
 v(1376) = pv(10)*pv(91)
 v(1377) = pv(11)*pv(91)
 v(1378) = pv(12)*pv(91)
 v(1379) = pv(13)*pv(91)
 v(1380) = pv(14)*pv(91)
 v(1381) = pv(15)*pv(91)
 v(1382) = pv(16)*pv(91)
 v(1383) = pv(17)*pv(91)
 v(1384) = pv(18)*pv(91)
 v(1385) = pv(19)*pv(91)
 v(1386) = pv(20)*pv(91)
 v(1387) = pv(21)*pv(91)
 v(1388) = pv(22)*pv(91)
 v(1389) = pv(23)*pv(91)
 v(1390) = pv(24)*pv(91)
 v(1391) = pv(25)*pv(91)
 v(1392) = pv(26)*pv(91)
 v(1393) = pv(28)*pv(91)
 v(1394) = pv(29)*pv(91)
 v(1395) = pv(30)*pv(91)
 v(1396) = pv(31)*pv(91)
 v(1397) = pv(10)*pv(92)
 v(1398) = pv(19)*pv(92)
 v(1399) = pv(20)*pv(92)
 v(1400) = pv(21)*pv(92)
 v(1401) = pv(22)*pv(92)
 v(1402) = pv(23)*pv(92)
 v(1403) = pv(24)*pv(92)
 v(1404) = pv(5)*pv(93)
 v(1405) = pv(7)*pv(93)
 v(1406) = pv(8)*pv(93)
 v(1407) = pv(10)*pv(93)
 v(1408) = pv(19)*pv(93)
 v(1409) = pv(20)*pv(93)
 v(1410) = pv(21)*pv(93)
 v(1411) = pv(22)*pv(93)
 v(1412) = pv(23)*pv(93)
 v(1413) = pv(24)*pv(93)
 v(1414) = pv(25)*pv(93)
 v(1415) = pv(8)*pv(94)
 v(1416) = pv(9)*pv(94)
 v(1417) = pv(10)*pv(94)
 v(1418) = pv(14)*pv(94)
 v(1419) = pv(15)*pv(94)
 v(1420) = pv(22)*pv(94)
 v(1421) = pv(26)*pv(94)
 v(1422) = pv(5)*pv(95)
 v(1423) = pv(8)*pv(95)
 v(1424) = pv(9)*pv(95)
 v(1425) = pv(10)*pv(95)
 v(1426) = pv(15)*pv(95)
 v(1427) = pv(19)*pv(95)
 v(1428) = pv(22)*pv(95)
 v(1429) = pv(23)*pv(95)
 v(1430) = pv(5)*pv(96)
 v(1431) = pv(6)*pv(96)
 v(1432) = pv(8)*pv(96)
 v(1433) = pv(9)*pv(96)
 v(1434) = pv(10)*pv(96)
 v(1435) = pv(11)*pv(96)
 v(1436) = pv(12)*pv(96)
 v(1437) = pv(13)*pv(96)
 v(1438) = pv(14)*pv(96)
 v(1439) = pv(15)*pv(96)
 v(1440) = pv(19)*pv(96)
 v(1441) = pv(20)*pv(96)
 v(1442) = pv(21)*pv(96)
 v(1443) = pv(22)*pv(96)
 v(1444) = pv(23)*pv(96)
 v(1445) = pv(5)*pv(97)
 v(1446) = pv(6)*pv(97)
 v(1447) = pv(8)*pv(97)
 v(1448) = pv(9)*pv(97)
 v(1449) = pv(10)*pv(97)
 v(1450) = pv(11)*pv(97)
 v(1451) = pv(12)*pv(97)
 v(1452) = pv(13)*pv(97)
 v(1453) = pv(14)*pv(97)
 v(1454) = pv(15)*pv(97)
 v(1455) = pv(18)*pv(97)
 v(1456) = pv(19)*pv(97)
 v(1457) = pv(22)*pv(97)
 v(1458) = pv(23)*pv(97)
 v(1459) = pv(26)*pv(97)
 v(1460) = pv(30)*pv(97)
 v(1461) = pv(5)*pv(98)
 v(1462) = pv(6)*pv(98)
 v(1463) = pv(7)*pv(98)
 v(1464) = pv(8)*pv(98)
 v(1465) = pv(9)*pv(98)
 v(1466) = pv(10)*pv(98)
 v(1467) = pv(11)*pv(98)
 v(1468) = pv(12)*pv(98)
 v(1469) = pv(13)*pv(98)
 v(1470) = pv(14)*pv(98)
 v(1471) = pv(15)*pv(98)
 v(1472) = pv(16)*pv(98)
 v(1473) = pv(17)*pv(98)
 v(1474) = pv(18)*pv(98)
 v(1475) = pv(19)*pv(98)
 v(1476) = pv(20)*pv(98)
 v(1477) = pv(21)*pv(98)
 v(1478) = pv(22)*pv(98)
 v(1479) = pv(23)*pv(98)
 v(1480) = pv(24)*pv(98)
 v(1481) = pv(25)*pv(98)
 v(1482) = pv(26)*pv(98)
 v(1483) = pv(27)*pv(98)
 v(1484) = pv(28)*pv(98)
 v(1485) = pv(29)*pv(98)
 v(1486) = pv(30)*pv(98)
 v(1487) = pv(31)*pv(98)
 v(1488) = pv(0)*pv(99)
 v(1489) = pv(1)*pv(99)
 v(1490) = pv(2)*pv(99)
 v(1491) = pv(3)*pv(99)
 v(1492) = pv(4)*pv(99)
 v(1493) = pv(0)*pv(100)
 v(1494) = pv(1)*pv(100)
 v(1495) = pv(2)*pv(100)
 v(1496) = pv(3)*pv(100)
 v(1497) = pv(4)*pv(100)
 v(1498) = pv(0)*pv(101)
 v(1499) = pv(1)*pv(101)
 v(1500) = pv(2)*pv(101)
 v(1501) = pv(3)*pv(101)
 v(1502) = pv(4)*pv(101)
 v(1503) = pv(0)*pv(102)
 v(1504) = pv(1)*pv(102)
 v(1505) = pv(2)*pv(102)
 v(1506) = pv(0)*pv(103)
 v(1507) = pv(1)*pv(103)
 v(1508) = pv(2)*pv(103)
 v(1509) = pv(0)*pv(104)
 v(1510) = pv(1)*pv(104)
 v(1511) = pv(2)*pv(104)
 v(1512) = pv(3)*pv(104)
 v(1513) = pv(0)*pv(105)
 v(1514) = pv(1)*pv(105)
 v(1515) = pv(2)*pv(105)
 v(1516) = pv(3)*pv(105)
 v(1517) = pv(0)*pv(106)
 v(1518) = pv(1)*pv(106)
 v(1519) = pv(2)*pv(106)
 v(1520) = pv(0)*pv(107)
 v(1521) = pv(1)*pv(107)
 v(1522) = pv(2)*pv(107)
 v(1523) = pv(4)*pv(107)
 v(1524) = pv(0)*pv(108)
 v(1525) = pv(1)*pv(108)
 v(1526) = pv(2)*pv(108)
 v(1527) = pv(3)*pv(108)
 v(1528) = pv(4)*pv(108)
 v(1529) = pv(0)*pv(109)
 v(1530) = pv(1)*pv(109)
 v(1531) = pv(2)*pv(109)
 v(1532) = pv(3)*pv(109)
 v(1533) = pv(4)*pv(109)
 v(1534) = pv(0)*pv(110)
 v(1535) = pv(1)*pv(110)
 v(1536) = pv(2)*pv(110)
 v(1537) = pv(3)*pv(110)
 v(1538) = pv(4)*pv(110)
 v(1539) = pv(0)*pv(111)
 v(1540) = pv(1)*pv(111)
 v(1541) = pv(2)*pv(111)
 v(1542) = pv(4)*pv(111)
 v(1543) = pv(0)*pv(112)
 v(1544) = pv(1)*pv(112)
 v(1545) = pv(2)*pv(112)
 v(1546) = pv(3)*pv(112)
 v(1547) = pv(4)*pv(112)
 v(1548) = pv(0)*pv(113)
 v(1549) = pv(1)*pv(113)
 v(1550) = pv(2)*pv(113)
 v(1551) = pv(3)*pv(113)
 v(1552) = pv(4)*pv(113)
 v(1553) = pv(0)*pv(114)
 v(1554) = pv(1)*pv(114)
 v(1555) = pv(2)*pv(114)
 v(1556) = pv(4)*pv(114)
 v(1557) = pv(0)*pv(115)
 v(1558) = pv(1)*pv(115)
 v(1559) = pv(2)*pv(115)
 v(1560) = pv(3)*pv(115)
 v(1561) = pv(4)*pv(115)
 v(1562) = pv(0)*pv(116)
 v(1563) = pv(1)*pv(116)
 v(1564) = pv(2)*pv(116)
 v(1565) = pv(3)*pv(116)
 v(1566) = pv(0)*pv(117)
 v(1567) = pv(1)*pv(117)
 v(1568) = pv(2)*pv(117)
 v(1569) = pv(0)*pv(118)
 v(1570) = pv(1)*pv(118)
 v(1571) = pv(2)*pv(118)
 v(1572) = pv(0)*pv(119)
 v(1573) = pv(1)*pv(119)
 v(1574) = pv(2)*pv(119)
 v(1575) = pv(0)*pv(120)
 v(1576) = pv(1)*pv(120)
 v(1577) = pv(2)*pv(120)
 v(1578) = pv(3)*pv(120)
 v(1579) = pv(4)*pv(120)
 v(1580) = pv(0)*pv(121)
 v(1581) = pv(1)*pv(121)
 v(1582) = pv(2)*pv(121)
 v(1583) = pv(3)*pv(121)
 v(1584) = pv(4)*pv(121)
 v(1585) = pv(0)*pv(122)
 v(1586) = pv(1)*pv(122)
 v(1587) = pv(2)*pv(122)
 v(1588) = pv(3)*pv(122)
 v(1589) = pv(4)*pv(122)
 v(1590) = pv(0)*pv(123)
 v(1591) = pv(1)*pv(123)
 v(1592) = pv(2)*pv(123)
 v(1593) = pv(3)*pv(123)
 v(1594) = pv(4)*pv(123)
 v(1595) = pv(0)*pv(124)
 v(1596) = pv(1)*pv(124)
 v(1597) = pv(2)*pv(124)
 v(1598) = pv(3)*pv(124)
 v(1599) = pv(0)*pv(125)
 v(1600) = pv(1)*pv(125)
 v(1601) = pv(2)*pv(125)
 v(1602) = pv(3)*pv(125)
 v(1603) = pv(4)*pv(125)
 v(1604) = pv(0)*pv(126)
 v(1605) = pv(1)*pv(126)
 v(1606) = pv(2)*pv(126)
 v(1607) = pv(3)*pv(126)
 v(1608) = pv(0)*pv(127)
 v(1609) = pv(1)*pv(127)
 v(1610) = pv(2)*pv(127)
 v(1611) = pv(3)*pv(127)
 v(1612) = pv(0)*pv(128)
 v(1613) = pv(1)*pv(128)
 v(1614) = pv(2)*pv(128)
 v(1615) = pv(0)*pv(129)
 v(1616) = pv(1)*pv(129)
 v(1617) = pv(2)*pv(129)
 v(1618) = pv(3)*pv(129)
 v(1619) = pv(4)*pv(129)
 v(1620) = pv(0)*pv(130)
 v(1621) = pv(1)*pv(130)
 v(1622) = pv(2)*pv(130)
 v(1623) = pv(3)*pv(130)
 v(1624) = pv(4)*pv(130)
 v(1625) = pv(0)*pv(131)
 v(1626) = pv(1)*pv(131)
 v(1627) = pv(2)*pv(131)
 v(1628) = pv(3)*pv(131)
 v(1629) = pv(4)*pv(131)
 v(1630) = pv(0)*pv(132)
 v(1631) = pv(1)*pv(132)
 v(1632) = pv(2)*pv(132)
 v(1633) = pv(3)*pv(132)
 v(1634) = pv(4)*pv(132)
 v(1635) = pv(0)*pv(133)
 v(1636) = pv(1)*pv(133)
 v(1637) = pv(2)*pv(133)
 v(1638) = pv(0)*pv(134)
 v(1639) = pv(1)*pv(134)
 v(1640) = pv(2)*pv(134)
 v(1641) = pv(0)*pv(135)
 v(1642) = pv(1)*pv(135)
 v(1643) = pv(2)*pv(135)
 v(1644) = pv(3)*pv(135)
 v(1645) = pv(4)*pv(135)
 v(1646) = pv(0)*pv(136)
 v(1647) = pv(2)*pv(136)
 v(1648) = pv(3)*pv(136)
 v(1649) = pv(4)*pv(136)
 v(1650) = pv(0)*pv(137)
 v(1651) = pv(1)*pv(137)
 v(1652) = pv(2)*pv(137)
 v(1653) = pv(0)*pv(138)
 v(1654) = pv(1)*pv(138)
 v(1655) = pv(2)*pv(138)
 v(1656) = pv(3)*pv(138)
 v(1657) = pv(4)*pv(138)
 v(1658) = pv(0)*pv(139)
 v(1659) = pv(1)*pv(139)
 v(1660) = pv(2)*pv(139)
 v(1661) = pv(0)*pv(140)
 v(1662) = pv(1)*pv(140)
 v(1663) = pv(2)*pv(140)
 v(1664) = pv(3)*pv(140)
 v(1665) = pv(4)*pv(140)
 v(1666) = pv(0)*pv(141)
 v(1667) = pv(1)*pv(141)
 v(1668) = pv(2)*pv(141)
 v(1669) = pv(0)*pv(142)
 v(1670) = pv(1)*pv(142)
 v(1671) = pv(2)*pv(142)
 v(1672) = pv(4)*pv(142)
 v(1673) = pv(0)*pv(143)
 v(1674) = pv(1)*pv(143)
 v(1675) = pv(2)*pv(143)
 v(1676) = pv(3)*pv(143)
 v(1677) = pv(4)*pv(143)
 v(1678) = pv(0)*pv(144)
 v(1679) = pv(1)*pv(144)
 v(1680) = pv(2)*pv(144)
 v(1681) = pv(3)*pv(144)
 v(1682) = pv(4)*pv(144)
 v(1683) = pv(0)*pv(145)
 v(1684) = pv(1)*pv(145)
 v(1685) = pv(2)*pv(145)
 v(1686) = pv(3)*pv(145)
 v(1687) = pv(4)*pv(145)
 v(1688) = pv(0)*pv(146)
 v(1689) = pv(1)*pv(146)
 v(1690) = pv(2)*pv(146)
 v(1691) = pv(4)*pv(146)
 v(1692) = pv(0)*pv(147)
 v(1693) = pv(1)*pv(147)
 v(1694) = pv(2)*pv(147)
 v(1695) = pv(0)*pv(148)
 v(1696) = pv(1)*pv(148)
 v(1697) = pv(2)*pv(148)
 v(1698) = pv(0)*pv(149)
 v(1699) = pv(1)*pv(149)
 v(1700) = pv(2)*pv(149)
 v(1701) = pv(4)*pv(149)
 v(1702) = pv(0)*pv(150)
 v(1703) = pv(1)*pv(150)
 v(1704) = pv(2)*pv(150)
 v(1705) = pv(3)*pv(150)
 v(1706) = pv(4)*pv(150)
 v(1707) = pv(0)*pv(151)
 v(1708) = pv(1)*pv(151)
 v(1709) = pv(2)*pv(151)
 v(1710) = pv(3)*pv(151)
 v(1711) = pv(4)*pv(151)
 v(1712) = pv(0)*pv(152)
 v(1713) = pv(1)*pv(152)
 v(1714) = pv(2)*pv(152)
 v(1715) = pv(3)*pv(152)
 v(1716) = pv(4)*pv(152)
 v(1717) = pv(0)*pv(153)
 v(1718) = pv(1)*pv(153)
 v(1719) = pv(2)*pv(153)
 v(1720) = pv(3)*pv(153)
 v(1721) = pv(4)*pv(153)
 v(1722) = pv(0)*pv(154)
 v(1723) = pv(1)*pv(154)
 v(1724) = pv(2)*pv(154)
 v(1725) = pv(3)*pv(154)
 v(1726) = pv(0)*pv(155)
 v(1727) = pv(1)*pv(155)
 v(1728) = pv(2)*pv(155)
 v(1729) = pv(3)*pv(155)
 v(1730) = pv(0)*pv(156)
 v(1731) = pv(1)*pv(156)
 v(1732) = pv(2)*pv(156)
 v(1733) = pv(3)*pv(156)
 v(1734) = pv(0)*pv(157)
 v(1735) = pv(1)*pv(157)
 v(1736) = pv(2)*pv(157)
 v(1737) = pv(3)*pv(157)
 v(1738) = pv(0)*pv(158)
 v(1739) = pv(1)*pv(158)
 v(1740) = pv(2)*pv(158)
 v(1741) = pv(4)*pv(158)
 v(1742) = pv(0)*pv(159)
 v(1743) = pv(1)*pv(159)
 v(1744) = pv(2)*pv(159)
 v(1745) = pv(0)*pv(160)
 v(1746) = pv(1)*pv(160)
 v(1747) = pv(2)*pv(160)
 v(1748) = pv(0)*pv(161)
 v(1749) = pv(1)*pv(161)
 v(1750) = pv(2)*pv(161)
 v(1751) = pv(0)*pv(162)
 v(1752) = pv(1)*pv(162)
 v(1753) = pv(2)*pv(162)
 v(1754) = pv(4)*pv(162)
 v(1755) = pv(0)*pv(163)
 v(1756) = pv(1)*pv(163)
 v(1757) = pv(2)*pv(163)
 v(1758) = pv(0)*pv(164)
 v(1759) = pv(1)*pv(164)
 v(1760) = pv(2)*pv(164)
 v(1761) = pv(4)*pv(164)
 v(1762) = pv(0)*pv(165)
 v(1763) = pv(1)*pv(165)
 v(1764) = pv(2)*pv(165)
 v(1765) = pv(0)*pv(166)
 v(1766) = pv(1)*pv(166)
 v(1767) = pv(2)*pv(166)
 v(1768) = pv(0)*pv(167)
 v(1769) = pv(1)*pv(167)
 v(1770) = pv(2)*pv(167)
 v(1771) = pv(4)*pv(167)
 v(1772) = pv(0)*pv(168)
 v(1773) = pv(1)*pv(168)
 v(1774) = pv(2)*pv(168)
 v(1775) = pv(3)*pv(168)
 v(1776) = pv(4)*pv(168)
 v(1777) = pv(0)*pv(169)
 v(1778) = pv(1)*pv(169)
 v(1779) = pv(2)*pv(169)
 v(1780) = pv(3)*pv(169)
 v(1781) = pv(4)*pv(169)
 v(1782) = pv(0)*pv(170)
 v(1783) = pv(1)*pv(170)
 v(1784) = pv(2)*pv(170)
 v(1785) = pv(3)*pv(170)
 v(1786) = pv(0)*pv(171)
 v(1787) = pv(1)*pv(171)
 v(1788) = pv(2)*pv(171)
 v(1789) = pv(4)*pv(171)
 v(1790) = pv(0)*pv(172)
 v(1791) = pv(1)*pv(172)
 v(1792) = pv(2)*pv(172)
 v(1793) = pv(3)*pv(172)
 v(1794) = pv(4)*pv(172)
 v(1795:1797) = pv(195:197) !! don't know them; Magma 17 GB
endif
if (8.le.mxd) then
 stop 'mg322: degree 8 not implemented'
endif
return
END SUBROUTINE mg322_secs