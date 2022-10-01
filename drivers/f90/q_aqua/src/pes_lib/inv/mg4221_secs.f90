SUBROUTINE mg4221_secs (mxd, r, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: v(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: pv(0:size(v)-1) !! an upper bound size
if (size(r).ne.nk*nk.or.size(v).ne.mg4221_nsc(mxd)) then
 stop 'mg4221_secs: bad dimensions'
endif
call mg4221_isecs (mxd, r, pv)
v(0) = 1
if (2.le.mxd) then
 v(1:12) = pv(0:11)
endif
if (3.le.mxd) then
 v(13:82) = pv(12:81)
endif
if (4.le.mxd) then
 v(83) = pv(0)*pv(0)
 v(84) = pv(0)*pv(1)
 v(85) = pv(1)*pv(1)
 v(86) = pv(0)*pv(2)
 v(87) = pv(1)*pv(2)
 v(88) = pv(2)*pv(2)
 v(89) = pv(0)*pv(3)
 v(90) = pv(1)*pv(3)
 v(91) = pv(2)*pv(3)
 v(92) = pv(0)*pv(4)
 v(93) = pv(1)*pv(4)
 v(94) = pv(2)*pv(4)
 v(95) = pv(3)*pv(4)
 v(96) = pv(0)*pv(5)
 v(97) = pv(1)*pv(5)
 v(98) = pv(2)*pv(5)
 v(99) = pv(3)*pv(5)
 v(100) = pv(4)*pv(5)
 v(101) = pv(5)*pv(5)
 v(102) = pv(0)*pv(6)
 v(103) = pv(1)*pv(6)
 v(104) = pv(2)*pv(6)
 v(105) = pv(3)*pv(6)
 v(106) = pv(4)*pv(6)
 v(107) = pv(5)*pv(6)
 v(108) = pv(6)*pv(6)
 v(109) = pv(0)*pv(7)
 v(110) = pv(1)*pv(7)
 v(111) = pv(2)*pv(7)
 v(112) = pv(3)*pv(7)
 v(113) = pv(4)*pv(7)
 v(114) = pv(5)*pv(7)
 v(115) = pv(6)*pv(7)
 v(116) = pv(7)*pv(7)
 v(117) = pv(0)*pv(8)
 v(118) = pv(1)*pv(8)
 v(119) = pv(2)*pv(8)
 v(120) = pv(4)*pv(8)
 v(121) = pv(5)*pv(8)
 v(122) = pv(6)*pv(8)
 v(123) = pv(7)*pv(8)
 v(124) = pv(0)*pv(9)
 v(125) = pv(1)*pv(9)
 v(126) = pv(2)*pv(9)
 v(127) = pv(4)*pv(9)
 v(128) = pv(5)*pv(9)
 v(129) = pv(6)*pv(9)
 v(130) = pv(7)*pv(9)
 v(131) = pv(0)*pv(10)
 v(132) = pv(1)*pv(10)
 v(133) = pv(2)*pv(10)
 v(134) = pv(3)*pv(10)
 v(135) = pv(5)*pv(10)
 v(136) = pv(6)*pv(10)
 v(137) = pv(7)*pv(10)
 v(138) = pv(8)*pv(10)
 v(139) = pv(9)*pv(10)
 v(140) = pv(0)*pv(11)
 v(141) = pv(1)*pv(11)
 v(142) = pv(2)*pv(11)
 v(143) = pv(3)*pv(11)
 v(144) = pv(5)*pv(11)
 v(145) = pv(6)*pv(11)
 v(146) = pv(7)*pv(11)
 v(147) = pv(8)*pv(11)
 v(148) = pv(9)*pv(11)
 v(149:395) = pv(82:328)
endif
if (5.le.mxd) then
 v(396) = pv(0)*pv(12)
 v(397) = pv(1)*pv(12)
 v(398) = pv(2)*pv(12)
 v(399) = pv(3)*pv(12)
 v(400) = pv(4)*pv(12)
 v(401) = pv(5)*pv(12)
 v(402) = pv(6)*pv(12)
 v(403) = pv(7)*pv(12)
 v(404) = pv(8)*pv(12)
 v(405) = pv(9)*pv(12)
 v(406) = pv(10)*pv(12)
 v(407) = pv(11)*pv(12)
 v(408) = pv(0)*pv(13)
 v(409) = pv(1)*pv(13)
 v(410) = pv(2)*pv(13)
 v(411) = pv(3)*pv(13)
 v(412) = pv(4)*pv(13)
 v(413) = pv(5)*pv(13)
 v(414) = pv(6)*pv(13)
 v(415) = pv(7)*pv(13)
 v(416) = pv(8)*pv(13)
 v(417) = pv(9)*pv(13)
 v(418) = pv(10)*pv(13)
 v(419) = pv(11)*pv(13)
 v(420) = pv(0)*pv(14)
 v(421) = pv(1)*pv(14)
 v(422) = pv(2)*pv(14)
 v(423) = pv(3)*pv(14)
 v(424) = pv(4)*pv(14)
 v(425) = pv(5)*pv(14)
 v(426) = pv(6)*pv(14)
 v(427) = pv(7)*pv(14)
 v(428) = pv(8)*pv(14)
 v(429) = pv(9)*pv(14)
 v(430) = pv(10)*pv(14)
 v(431) = pv(11)*pv(14)
 v(432) = pv(0)*pv(15)
 v(433) = pv(1)*pv(15)
 v(434) = pv(2)*pv(15)
 v(435) = pv(4)*pv(15)
 v(436) = pv(5)*pv(15)
 v(437) = pv(6)*pv(15)
 v(438) = pv(7)*pv(15)
 v(439) = pv(10)*pv(15)
 v(440) = pv(11)*pv(15)
 v(441) = pv(0)*pv(16)
 v(442) = pv(1)*pv(16)
 v(443) = pv(2)*pv(16)
 v(444) = pv(4)*pv(16)
 v(445) = pv(5)*pv(16)
 v(446) = pv(6)*pv(16)
 v(447) = pv(7)*pv(16)
 v(448) = pv(10)*pv(16)
 v(449) = pv(11)*pv(16)
 v(450) = pv(0)*pv(17)
 v(451) = pv(1)*pv(17)
 v(452) = pv(2)*pv(17)
 v(453) = pv(3)*pv(17)
 v(454) = pv(4)*pv(17)
 v(455) = pv(5)*pv(17)
 v(456) = pv(6)*pv(17)
 v(457) = pv(7)*pv(17)
 v(458) = pv(8)*pv(17)
 v(459) = pv(9)*pv(17)
 v(460) = pv(10)*pv(17)
 v(461) = pv(11)*pv(17)
 v(462) = pv(0)*pv(18)
 v(463) = pv(1)*pv(18)
 v(464) = pv(2)*pv(18)
 v(465) = pv(3)*pv(18)
 v(466) = pv(4)*pv(18)
 v(467) = pv(5)*pv(18)
 v(468) = pv(6)*pv(18)
 v(469) = pv(7)*pv(18)
 v(470) = pv(8)*pv(18)
 v(471) = pv(9)*pv(18)
 v(472) = pv(10)*pv(18)
 v(473) = pv(11)*pv(18)
 v(474) = pv(0)*pv(19)
 v(475) = pv(1)*pv(19)
 v(476) = pv(2)*pv(19)
 v(477) = pv(3)*pv(19)
 v(478) = pv(4)*pv(19)
 v(479) = pv(5)*pv(19)
 v(480) = pv(6)*pv(19)
 v(481) = pv(7)*pv(19)
 v(482) = pv(8)*pv(19)
 v(483) = pv(9)*pv(19)
 v(484) = pv(10)*pv(19)
 v(485) = pv(11)*pv(19)
 v(486) = pv(0)*pv(20)
 v(487) = pv(1)*pv(20)
 v(488) = pv(2)*pv(20)
 v(489) = pv(3)*pv(20)
 v(490) = pv(4)*pv(20)
 v(491) = pv(5)*pv(20)
 v(492) = pv(6)*pv(20)
 v(493) = pv(7)*pv(20)
 v(494) = pv(8)*pv(20)
 v(495) = pv(9)*pv(20)
 v(496) = pv(10)*pv(20)
 v(497) = pv(11)*pv(20)
 v(498) = pv(0)*pv(21)
 v(499) = pv(1)*pv(21)
 v(500) = pv(2)*pv(21)
 v(501) = pv(3)*pv(21)
 v(502) = pv(4)*pv(21)
 v(503) = pv(5)*pv(21)
 v(504) = pv(6)*pv(21)
 v(505) = pv(7)*pv(21)
 v(506) = pv(8)*pv(21)
 v(507) = pv(9)*pv(21)
 v(508) = pv(10)*pv(21)
 v(509) = pv(11)*pv(21)
 v(510) = pv(0)*pv(22)
 v(511) = pv(1)*pv(22)
 v(512) = pv(2)*pv(22)
 v(513) = pv(3)*pv(22)
 v(514) = pv(4)*pv(22)
 v(515) = pv(5)*pv(22)
 v(516) = pv(6)*pv(22)
 v(517) = pv(7)*pv(22)
 v(518) = pv(8)*pv(22)
 v(519) = pv(9)*pv(22)
 v(520) = pv(10)*pv(22)
 v(521) = pv(11)*pv(22)
 v(522) = pv(0)*pv(23)
 v(523) = pv(1)*pv(23)
 v(524) = pv(2)*pv(23)
 v(525) = pv(3)*pv(23)
 v(526) = pv(4)*pv(23)
 v(527) = pv(5)*pv(23)
 v(528) = pv(6)*pv(23)
 v(529) = pv(7)*pv(23)
 v(530) = pv(8)*pv(23)
 v(531) = pv(9)*pv(23)
 v(532) = pv(10)*pv(23)
 v(533) = pv(11)*pv(23)
 v(534) = pv(0)*pv(24)
 v(535) = pv(1)*pv(24)
 v(536) = pv(2)*pv(24)
 v(537) = pv(3)*pv(24)
 v(538) = pv(4)*pv(24)
 v(539) = pv(5)*pv(24)
 v(540) = pv(6)*pv(24)
 v(541) = pv(7)*pv(24)
 v(542) = pv(8)*pv(24)
 v(543) = pv(9)*pv(24)
 v(544) = pv(10)*pv(24)
 v(545) = pv(11)*pv(24)
 v(546) = pv(0)*pv(25)
 v(547) = pv(1)*pv(25)
 v(548) = pv(2)*pv(25)
 v(549) = pv(4)*pv(25)
 v(550) = pv(5)*pv(25)
 v(551) = pv(6)*pv(25)
 v(552) = pv(7)*pv(25)
 v(553) = pv(10)*pv(25)
 v(554) = pv(11)*pv(25)
 v(555) = pv(0)*pv(26)
 v(556) = pv(1)*pv(26)
 v(557) = pv(2)*pv(26)
 v(558) = pv(3)*pv(26)
 v(559) = pv(4)*pv(26)
 v(560) = pv(5)*pv(26)
 v(561) = pv(6)*pv(26)
 v(562) = pv(7)*pv(26)
 v(563) = pv(8)*pv(26)
 v(564) = pv(9)*pv(26)
 v(565) = pv(10)*pv(26)
 v(566) = pv(11)*pv(26)
 v(567) = pv(0)*pv(27)
 v(568) = pv(1)*pv(27)
 v(569) = pv(2)*pv(27)
 v(570) = pv(3)*pv(27)
 v(571) = pv(4)*pv(27)
 v(572) = pv(5)*pv(27)
 v(573) = pv(6)*pv(27)
 v(574) = pv(7)*pv(27)
 v(575) = pv(8)*pv(27)
 v(576) = pv(9)*pv(27)
 v(577) = pv(10)*pv(27)
 v(578) = pv(11)*pv(27)
 v(579) = pv(0)*pv(28)
 v(580) = pv(1)*pv(28)
 v(581) = pv(2)*pv(28)
 v(582) = pv(3)*pv(28)
 v(583) = pv(4)*pv(28)
 v(584) = pv(5)*pv(28)
 v(585) = pv(6)*pv(28)
 v(586) = pv(7)*pv(28)
 v(587) = pv(8)*pv(28)
 v(588) = pv(9)*pv(28)
 v(589) = pv(10)*pv(28)
 v(590) = pv(11)*pv(28)
 v(591) = pv(0)*pv(29)
 v(592) = pv(1)*pv(29)
 v(593) = pv(2)*pv(29)
 v(594) = pv(3)*pv(29)
 v(595) = pv(5)*pv(29)
 v(596) = pv(6)*pv(29)
 v(597) = pv(7)*pv(29)
 v(598) = pv(8)*pv(29)
 v(599) = pv(9)*pv(29)
 v(600) = pv(0)*pv(30)
 v(601) = pv(1)*pv(30)
 v(602) = pv(2)*pv(30)
 v(603) = pv(3)*pv(30)
 v(604) = pv(5)*pv(30)
 v(605) = pv(6)*pv(30)
 v(606) = pv(7)*pv(30)
 v(607) = pv(8)*pv(30)
 v(608) = pv(9)*pv(30)
 v(609) = pv(0)*pv(31)
 v(610) = pv(1)*pv(31)
 v(611) = pv(2)*pv(31)
 v(612) = pv(3)*pv(31)
 v(613) = pv(5)*pv(31)
 v(614) = pv(6)*pv(31)
 v(615) = pv(7)*pv(31)
 v(616) = pv(8)*pv(31)
 v(617) = pv(9)*pv(31)
 v(618) = pv(0)*pv(32)
 v(619) = pv(1)*pv(32)
 v(620) = pv(2)*pv(32)
 v(621) = pv(3)*pv(32)
 v(622) = pv(4)*pv(32)
 v(623) = pv(5)*pv(32)
 v(624) = pv(6)*pv(32)
 v(625) = pv(7)*pv(32)
 v(626) = pv(8)*pv(32)
 v(627) = pv(9)*pv(32)
 v(628) = pv(10)*pv(32)
 v(629) = pv(11)*pv(32)
 v(630) = pv(0)*pv(33)
 v(631) = pv(1)*pv(33)
 v(632) = pv(2)*pv(33)
 v(633) = pv(3)*pv(33)
 v(634) = pv(4)*pv(33)
 v(635) = pv(5)*pv(33)
 v(636) = pv(6)*pv(33)
 v(637) = pv(7)*pv(33)
 v(638) = pv(8)*pv(33)
 v(639) = pv(9)*pv(33)
 v(640) = pv(10)*pv(33)
 v(641) = pv(11)*pv(33)
 v(642) = pv(0)*pv(34)
 v(643) = pv(1)*pv(34)
 v(644) = pv(2)*pv(34)
 v(645) = pv(3)*pv(34)
 v(646) = pv(4)*pv(34)
 v(647) = pv(5)*pv(34)
 v(648) = pv(6)*pv(34)
 v(649) = pv(7)*pv(34)
 v(650) = pv(8)*pv(34)
 v(651) = pv(9)*pv(34)
 v(652) = pv(10)*pv(34)
 v(653) = pv(11)*pv(34)
 v(654) = pv(0)*pv(35)
 v(655) = pv(1)*pv(35)
 v(656) = pv(2)*pv(35)
 v(657) = pv(3)*pv(35)
 v(658) = pv(4)*pv(35)
 v(659) = pv(5)*pv(35)
 v(660) = pv(6)*pv(35)
 v(661) = pv(7)*pv(35)
 v(662) = pv(8)*pv(35)
 v(663) = pv(9)*pv(35)
 v(664) = pv(10)*pv(35)
 v(665) = pv(11)*pv(35)
 v(666) = pv(0)*pv(36)
 v(667) = pv(1)*pv(36)
 v(668) = pv(2)*pv(36)
 v(669) = pv(3)*pv(36)
 v(670) = pv(4)*pv(36)
 v(671) = pv(5)*pv(36)
 v(672) = pv(6)*pv(36)
 v(673) = pv(7)*pv(36)
 v(674) = pv(8)*pv(36)
 v(675) = pv(9)*pv(36)
 v(676) = pv(10)*pv(36)
 v(677) = pv(11)*pv(36)
 v(678) = pv(0)*pv(37)
 v(679) = pv(1)*pv(37)
 v(680) = pv(2)*pv(37)
 v(681) = pv(3)*pv(37)
 v(682) = pv(4)*pv(37)
 v(683) = pv(5)*pv(37)
 v(684) = pv(6)*pv(37)
 v(685) = pv(7)*pv(37)
 v(686) = pv(8)*pv(37)
 v(687) = pv(9)*pv(37)
 v(688) = pv(10)*pv(37)
 v(689) = pv(11)*pv(37)
 v(690) = pv(0)*pv(38)
 v(691) = pv(1)*pv(38)
 v(692) = pv(2)*pv(38)
 v(693) = pv(3)*pv(38)
 v(694) = pv(4)*pv(38)
 v(695) = pv(5)*pv(38)
 v(696) = pv(6)*pv(38)
 v(697) = pv(7)*pv(38)
 v(698) = pv(8)*pv(38)
 v(699) = pv(9)*pv(38)
 v(700) = pv(10)*pv(38)
 v(701) = pv(11)*pv(38)
 v(702) = pv(0)*pv(39)
 v(703) = pv(1)*pv(39)
 v(704) = pv(2)*pv(39)
 v(705) = pv(4)*pv(39)
 v(706) = pv(5)*pv(39)
 v(707) = pv(6)*pv(39)
 v(708) = pv(7)*pv(39)
 v(709) = pv(10)*pv(39)
 v(710) = pv(11)*pv(39)
 v(711) = pv(0)*pv(40)
 v(712) = pv(1)*pv(40)
 v(713) = pv(2)*pv(40)
 v(714) = pv(4)*pv(40)
 v(715) = pv(5)*pv(40)
 v(716) = pv(6)*pv(40)
 v(717) = pv(7)*pv(40)
 v(718) = pv(10)*pv(40)
 v(719) = pv(11)*pv(40)
 v(720) = pv(0)*pv(41)
 v(721) = pv(1)*pv(41)
 v(722) = pv(2)*pv(41)
 v(723) = pv(3)*pv(41)
 v(724) = pv(5)*pv(41)
 v(725) = pv(6)*pv(41)
 v(726) = pv(7)*pv(41)
 v(727) = pv(8)*pv(41)
 v(728) = pv(9)*pv(41)
 v(729) = pv(0)*pv(42)
 v(730) = pv(1)*pv(42)
 v(731) = pv(2)*pv(42)
 v(732) = pv(5)*pv(42)
 v(733) = pv(6)*pv(42)
 v(734) = pv(7)*pv(42)
 v(735) = pv(0)*pv(43)
 v(736) = pv(1)*pv(43)
 v(737) = pv(2)*pv(43)
 v(738) = pv(5)*pv(43)
 v(739) = pv(6)*pv(43)
 v(740) = pv(7)*pv(43)
 v(741) = pv(0)*pv(44)
 v(742) = pv(1)*pv(44)
 v(743) = pv(2)*pv(44)
 v(744) = pv(3)*pv(44)
 v(745) = pv(4)*pv(44)
 v(746) = pv(5)*pv(44)
 v(747) = pv(6)*pv(44)
 v(748) = pv(7)*pv(44)
 v(749) = pv(8)*pv(44)
 v(750) = pv(9)*pv(44)
 v(751) = pv(10)*pv(44)
 v(752) = pv(11)*pv(44)
 v(753) = pv(0)*pv(45)
 v(754) = pv(1)*pv(45)
 v(755) = pv(2)*pv(45)
 v(756) = pv(3)*pv(45)
 v(757) = pv(5)*pv(45)
 v(758) = pv(6)*pv(45)
 v(759) = pv(7)*pv(45)
 v(760) = pv(8)*pv(45)
 v(761) = pv(9)*pv(45)
 v(762) = pv(0)*pv(46)
 v(763) = pv(1)*pv(46)
 v(764) = pv(2)*pv(46)
 v(765) = pv(3)*pv(46)
 v(766) = pv(4)*pv(46)
 v(767) = pv(5)*pv(46)
 v(768) = pv(6)*pv(46)
 v(769) = pv(7)*pv(46)
 v(770) = pv(8)*pv(46)
 v(771) = pv(9)*pv(46)
 v(772) = pv(10)*pv(46)
 v(773) = pv(11)*pv(46)
 v(774) = pv(0)*pv(47)
 v(775) = pv(1)*pv(47)
 v(776) = pv(2)*pv(47)
 v(777) = pv(5)*pv(47)
 v(778) = pv(6)*pv(47)
 v(779) = pv(7)*pv(47)
 v(780) = pv(0)*pv(48)
 v(781) = pv(1)*pv(48)
 v(782) = pv(2)*pv(48)
 v(783) = pv(5)*pv(48)
 v(784) = pv(6)*pv(48)
 v(785) = pv(7)*pv(48)
 v(786) = pv(0)*pv(49)
 v(787) = pv(1)*pv(49)
 v(788) = pv(2)*pv(49)
 v(789) = pv(5)*pv(49)
 v(790) = pv(6)*pv(49)
 v(791) = pv(7)*pv(49)
 v(792) = pv(0)*pv(50)
 v(793) = pv(1)*pv(50)
 v(794) = pv(2)*pv(50)
 v(795) = pv(3)*pv(50)
 v(796) = pv(4)*pv(50)
 v(797) = pv(5)*pv(50)
 v(798) = pv(6)*pv(50)
 v(799) = pv(7)*pv(50)
 v(800) = pv(8)*pv(50)
 v(801) = pv(9)*pv(50)
 v(802) = pv(10)*pv(50)
 v(803) = pv(11)*pv(50)
 v(804) = pv(0)*pv(51)
 v(805) = pv(1)*pv(51)
 v(806) = pv(2)*pv(51)
 v(807) = pv(3)*pv(51)
 v(808) = pv(4)*pv(51)
 v(809) = pv(5)*pv(51)
 v(810) = pv(6)*pv(51)
 v(811) = pv(7)*pv(51)
 v(812) = pv(8)*pv(51)
 v(813) = pv(9)*pv(51)
 v(814) = pv(10)*pv(51)
 v(815) = pv(11)*pv(51)
 v(816) = pv(0)*pv(52)
 v(817) = pv(1)*pv(52)
 v(818) = pv(2)*pv(52)
 v(819) = pv(3)*pv(52)
 v(820) = pv(4)*pv(52)
 v(821) = pv(5)*pv(52)
 v(822) = pv(6)*pv(52)
 v(823) = pv(7)*pv(52)
 v(824) = pv(8)*pv(52)
 v(825) = pv(9)*pv(52)
 v(826) = pv(10)*pv(52)
 v(827) = pv(11)*pv(52)
 v(828) = pv(0)*pv(53)
 v(829) = pv(1)*pv(53)
 v(830) = pv(2)*pv(53)
 v(831) = pv(4)*pv(53)
 v(832) = pv(5)*pv(53)
 v(833) = pv(6)*pv(53)
 v(834) = pv(7)*pv(53)
 v(835) = pv(10)*pv(53)
 v(836) = pv(11)*pv(53)
 v(837) = pv(0)*pv(54)
 v(838) = pv(1)*pv(54)
 v(839) = pv(2)*pv(54)
 v(840) = pv(3)*pv(54)
 v(841) = pv(4)*pv(54)
 v(842) = pv(5)*pv(54)
 v(843) = pv(6)*pv(54)
 v(844) = pv(7)*pv(54)
 v(845) = pv(8)*pv(54)
 v(846) = pv(9)*pv(54)
 v(847) = pv(10)*pv(54)
 v(848) = pv(11)*pv(54)
 v(849) = pv(0)*pv(55)
 v(850) = pv(1)*pv(55)
 v(851) = pv(2)*pv(55)
 v(852) = pv(3)*pv(55)
 v(853) = pv(4)*pv(55)
 v(854) = pv(5)*pv(55)
 v(855) = pv(6)*pv(55)
 v(856) = pv(7)*pv(55)
 v(857) = pv(8)*pv(55)
 v(858) = pv(9)*pv(55)
 v(859) = pv(10)*pv(55)
 v(860) = pv(11)*pv(55)
 v(861) = pv(0)*pv(56)
 v(862) = pv(1)*pv(56)
 v(863) = pv(2)*pv(56)
 v(864) = pv(3)*pv(56)
 v(865) = pv(4)*pv(56)
 v(866) = pv(5)*pv(56)
 v(867) = pv(6)*pv(56)
 v(868) = pv(7)*pv(56)
 v(869) = pv(8)*pv(56)
 v(870) = pv(9)*pv(56)
 v(871) = pv(10)*pv(56)
 v(872) = pv(11)*pv(56)
 v(873) = pv(0)*pv(57)
 v(874) = pv(1)*pv(57)
 v(875) = pv(2)*pv(57)
 v(876) = pv(3)*pv(57)
 v(877) = pv(4)*pv(57)
 v(878) = pv(5)*pv(57)
 v(879) = pv(6)*pv(57)
 v(880) = pv(7)*pv(57)
 v(881) = pv(8)*pv(57)
 v(882) = pv(9)*pv(57)
 v(883) = pv(10)*pv(57)
 v(884) = pv(11)*pv(57)
 v(885) = pv(0)*pv(58)
 v(886) = pv(1)*pv(58)
 v(887) = pv(2)*pv(58)
 v(888) = pv(3)*pv(58)
 v(889) = pv(4)*pv(58)
 v(890) = pv(5)*pv(58)
 v(891) = pv(6)*pv(58)
 v(892) = pv(7)*pv(58)
 v(893) = pv(8)*pv(58)
 v(894) = pv(9)*pv(58)
 v(895) = pv(10)*pv(58)
 v(896) = pv(11)*pv(58)
 v(897) = pv(0)*pv(59)
 v(898) = pv(1)*pv(59)
 v(899) = pv(2)*pv(59)
 v(900) = pv(3)*pv(59)
 v(901) = pv(5)*pv(59)
 v(902) = pv(6)*pv(59)
 v(903) = pv(7)*pv(59)
 v(904) = pv(8)*pv(59)
 v(905) = pv(9)*pv(59)
 v(906) = pv(0)*pv(60)
 v(907) = pv(1)*pv(60)
 v(908) = pv(2)*pv(60)
 v(909) = pv(3)*pv(60)
 v(910) = pv(4)*pv(60)
 v(911) = pv(5)*pv(60)
 v(912) = pv(6)*pv(60)
 v(913) = pv(7)*pv(60)
 v(914) = pv(8)*pv(60)
 v(915) = pv(9)*pv(60)
 v(916) = pv(10)*pv(60)
 v(917) = pv(11)*pv(60)
 v(918) = pv(0)*pv(61)
 v(919) = pv(1)*pv(61)
 v(920) = pv(2)*pv(61)
 v(921) = pv(3)*pv(61)
 v(922) = pv(4)*pv(61)
 v(923) = pv(5)*pv(61)
 v(924) = pv(6)*pv(61)
 v(925) = pv(7)*pv(61)
 v(926) = pv(8)*pv(61)
 v(927) = pv(9)*pv(61)
 v(928) = pv(10)*pv(61)
 v(929) = pv(11)*pv(61)
 v(930) = pv(0)*pv(62)
 v(931) = pv(1)*pv(62)
 v(932) = pv(2)*pv(62)
 v(933) = pv(3)*pv(62)
 v(934) = pv(4)*pv(62)
 v(935) = pv(5)*pv(62)
 v(936) = pv(6)*pv(62)
 v(937) = pv(7)*pv(62)
 v(938) = pv(8)*pv(62)
 v(939) = pv(9)*pv(62)
 v(940) = pv(10)*pv(62)
 v(941) = pv(11)*pv(62)
 v(942) = pv(0)*pv(63)
 v(943) = pv(1)*pv(63)
 v(944) = pv(2)*pv(63)
 v(945) = pv(4)*pv(63)
 v(946) = pv(5)*pv(63)
 v(947) = pv(6)*pv(63)
 v(948) = pv(7)*pv(63)
 v(949) = pv(10)*pv(63)
 v(950) = pv(11)*pv(63)
 v(951) = pv(0)*pv(64)
 v(952) = pv(1)*pv(64)
 v(953) = pv(2)*pv(64)
 v(954) = pv(3)*pv(64)
 v(955) = pv(5)*pv(64)
 v(956) = pv(6)*pv(64)
 v(957) = pv(7)*pv(64)
 v(958) = pv(8)*pv(64)
 v(959) = pv(9)*pv(64)
 v(960) = pv(0)*pv(65)
 v(961) = pv(1)*pv(65)
 v(962) = pv(2)*pv(65)
 v(963) = pv(3)*pv(65)
 v(964) = pv(4)*pv(65)
 v(965) = pv(5)*pv(65)
 v(966) = pv(6)*pv(65)
 v(967) = pv(7)*pv(65)
 v(968) = pv(8)*pv(65)
 v(969) = pv(9)*pv(65)
 v(970) = pv(10)*pv(65)
 v(971) = pv(11)*pv(65)
 v(972) = pv(0)*pv(66)
 v(973) = pv(1)*pv(66)
 v(974) = pv(2)*pv(66)
 v(975) = pv(3)*pv(66)
 v(976) = pv(4)*pv(66)
 v(977) = pv(5)*pv(66)
 v(978) = pv(6)*pv(66)
 v(979) = pv(7)*pv(66)
 v(980) = pv(8)*pv(66)
 v(981) = pv(9)*pv(66)
 v(982) = pv(10)*pv(66)
 v(983) = pv(11)*pv(66)
 v(984) = pv(0)*pv(67)
 v(985) = pv(1)*pv(67)
 v(986) = pv(2)*pv(67)
 v(987) = pv(3)*pv(67)
 v(988) = pv(4)*pv(67)
 v(989) = pv(5)*pv(67)
 v(990) = pv(6)*pv(67)
 v(991) = pv(7)*pv(67)
 v(992) = pv(8)*pv(67)
 v(993) = pv(9)*pv(67)
 v(994) = pv(10)*pv(67)
 v(995) = pv(11)*pv(67)
 v(996) = pv(0)*pv(68)
 v(997) = pv(1)*pv(68)
 v(998) = pv(2)*pv(68)
 v(999) = pv(3)*pv(68)
 v(1000) = pv(4)*pv(68)
 v(1001) = pv(5)*pv(68)
 v(1002) = pv(6)*pv(68)
 v(1003) = pv(7)*pv(68)
 v(1004) = pv(8)*pv(68)
 v(1005) = pv(9)*pv(68)
 v(1006) = pv(10)*pv(68)
 v(1007) = pv(11)*pv(68)
 v(1008) = pv(0)*pv(69)
 v(1009) = pv(1)*pv(69)
 v(1010) = pv(2)*pv(69)
 v(1011) = pv(3)*pv(69)
 v(1012) = pv(4)*pv(69)
 v(1013) = pv(5)*pv(69)
 v(1014) = pv(6)*pv(69)
 v(1015) = pv(7)*pv(69)
 v(1016) = pv(10)*pv(69)
 v(1017) = pv(11)*pv(69)
 v(1018) = pv(0)*pv(70)
 v(1019) = pv(1)*pv(70)
 v(1020) = pv(2)*pv(70)
 v(1021) = pv(3)*pv(70)
 v(1022) = pv(4)*pv(70)
 v(1023) = pv(5)*pv(70)
 v(1024) = pv(6)*pv(70)
 v(1025) = pv(7)*pv(70)
 v(1026) = pv(10)*pv(70)
 v(1027) = pv(11)*pv(70)
 v(1028) = pv(0)*pv(71)
 v(1029) = pv(1)*pv(71)
 v(1030) = pv(2)*pv(71)
 v(1031) = pv(3)*pv(71)
 v(1032) = pv(4)*pv(71)
 v(1033) = pv(5)*pv(71)
 v(1034) = pv(6)*pv(71)
 v(1035) = pv(7)*pv(71)
 v(1036) = pv(10)*pv(71)
 v(1037) = pv(11)*pv(71)
 v(1038) = pv(0)*pv(72)
 v(1039) = pv(1)*pv(72)
 v(1040) = pv(2)*pv(72)
 v(1041) = pv(3)*pv(72)
 v(1042) = pv(5)*pv(72)
 v(1043) = pv(6)*pv(72)
 v(1044) = pv(7)*pv(72)
 v(1045) = pv(0)*pv(73)
 v(1046) = pv(1)*pv(73)
 v(1047) = pv(2)*pv(73)
 v(1048) = pv(3)*pv(73)
 v(1049) = pv(5)*pv(73)
 v(1050) = pv(6)*pv(73)
 v(1051) = pv(7)*pv(73)
 v(1052) = pv(0)*pv(74)
 v(1053) = pv(1)*pv(74)
 v(1054) = pv(2)*pv(74)
 v(1055) = pv(3)*pv(74)
 v(1056) = pv(4)*pv(74)
 v(1057) = pv(5)*pv(74)
 v(1058) = pv(6)*pv(74)
 v(1059) = pv(7)*pv(74)
 v(1060) = pv(10)*pv(74)
 v(1061) = pv(11)*pv(74)
 v(1062) = pv(0)*pv(75)
 v(1063) = pv(1)*pv(75)
 v(1064) = pv(2)*pv(75)
 v(1065) = pv(3)*pv(75)
 v(1066) = pv(4)*pv(75)
 v(1067) = pv(5)*pv(75)
 v(1068) = pv(6)*pv(75)
 v(1069) = pv(7)*pv(75)
 v(1070) = pv(8)*pv(75)
 v(1071) = pv(9)*pv(75)
 v(1072) = pv(0)*pv(76)
 v(1073) = pv(1)*pv(76)
 v(1074) = pv(2)*pv(76)
 v(1075) = pv(3)*pv(76)
 v(1076) = pv(4)*pv(76)
 v(1077) = pv(5)*pv(76)
 v(1078) = pv(6)*pv(76)
 v(1079) = pv(7)*pv(76)
 v(1080) = pv(8)*pv(76)
 v(1081) = pv(9)*pv(76)
 v(1082) = pv(0)*pv(77)
 v(1083) = pv(1)*pv(77)
 v(1084) = pv(2)*pv(77)
 v(1085) = pv(3)*pv(77)
 v(1086) = pv(4)*pv(77)
 v(1087) = pv(5)*pv(77)
 v(1088) = pv(6)*pv(77)
 v(1089) = pv(7)*pv(77)
 v(1090) = pv(8)*pv(77)
 v(1091) = pv(9)*pv(77)
 v(1092) = pv(0)*pv(78)
 v(1093) = pv(1)*pv(78)
 v(1094) = pv(2)*pv(78)
 v(1095) = pv(4)*pv(78)
 v(1096) = pv(5)*pv(78)
 v(1097) = pv(6)*pv(78)
 v(1098) = pv(7)*pv(78)
 v(1099) = pv(0)*pv(79)
 v(1100) = pv(1)*pv(79)
 v(1101) = pv(2)*pv(79)
 v(1102) = pv(4)*pv(79)
 v(1103) = pv(5)*pv(79)
 v(1104) = pv(6)*pv(79)
 v(1105) = pv(7)*pv(79)
 v(1106) = pv(0)*pv(80)
 v(1107) = pv(1)*pv(80)
 v(1108) = pv(2)*pv(80)
 v(1109) = pv(3)*pv(80)
 v(1110) = pv(4)*pv(80)
 v(1111) = pv(5)*pv(80)
 v(1112) = pv(6)*pv(80)
 v(1113) = pv(7)*pv(80)
 v(1114) = pv(8)*pv(80)
 v(1115) = pv(9)*pv(80)
 v(1116) = pv(0)*pv(81)
 v(1117) = pv(1)*pv(81)
 v(1118) = pv(2)*pv(81)
 v(1119) = pv(3)*pv(81)
 v(1120) = pv(4)*pv(81)
 v(1121) = pv(5)*pv(81)
 v(1122) = pv(6)*pv(81)
 v(1123) = pv(7)*pv(81)
 v(1124:1838) = pv(329:1043)
endif
if (6.le.mxd) then
 stop 'mg4221: degree 6 not implemented'
endif
return
END SUBROUTINE mg4221_secs
