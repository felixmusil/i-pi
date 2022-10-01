SUBROUTINE mg33211_secs (mxd, r, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: v(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: pv(0:size(v)-1) !! an upper bound size
if (size(r).ne.nk*nk.or.size(v).ne.mg33211_nsc(mxd)) then
 stop 'mg33211_secs: bad dimensions'
endif
call mg33211_isecs (mxd, r, pv)
v(0) = 1
if (2.le.mxd) then
 v(1:26) = pv(0:25)
endif
if (3.le.mxd) then
 v(27:172) = pv(26:171)
endif
if (4.le.mxd) then
 v(173) = pv(0)*pv(0)
 v(174) = pv(0)*pv(1)
 v(175) = pv(1)*pv(1)
 v(176) = pv(0)*pv(2)
 v(177) = pv(1)*pv(2)
 v(178) = pv(2)*pv(2)
 v(179) = pv(0)*pv(3)
 v(180) = pv(1)*pv(3)
 v(181) = pv(2)*pv(3)
 v(182) = pv(3)*pv(3)
 v(183) = pv(0)*pv(4)
 v(184) = pv(1)*pv(4)
 v(185) = pv(2)*pv(4)
 v(186) = pv(3)*pv(4)
 v(187) = pv(4)*pv(4)
 v(188) = pv(0)*pv(5)
 v(189) = pv(1)*pv(5)
 v(190) = pv(2)*pv(5)
 v(191) = pv(3)*pv(5)
 v(192) = pv(4)*pv(5)
 v(193) = pv(5)*pv(5)
 v(194) = pv(0)*pv(6)
 v(195) = pv(1)*pv(6)
 v(196) = pv(2)*pv(6)
 v(197) = pv(3)*pv(6)
 v(198) = pv(4)*pv(6)
 v(199) = pv(5)*pv(6)
 v(200) = pv(0)*pv(7)
 v(201) = pv(1)*pv(7)
 v(202) = pv(2)*pv(7)
 v(203) = pv(3)*pv(7)
 v(204) = pv(4)*pv(7)
 v(205) = pv(5)*pv(7)
 v(206) = pv(6)*pv(7)
 v(207) = pv(7)*pv(7)
 v(208) = pv(0)*pv(8)
 v(209) = pv(1)*pv(8)
 v(210) = pv(2)*pv(8)
 v(211) = pv(3)*pv(8)
 v(212) = pv(4)*pv(8)
 v(213) = pv(5)*pv(8)
 v(214) = pv(6)*pv(8)
 v(215) = pv(7)*pv(8)
 v(216) = pv(8)*pv(8)
 v(217) = pv(0)*pv(9)
 v(218) = pv(1)*pv(9)
 v(219) = pv(2)*pv(9)
 v(220) = pv(3)*pv(9)
 v(221) = pv(4)*pv(9)
 v(222) = pv(5)*pv(9)
 v(223) = pv(6)*pv(9)
 v(224) = pv(7)*pv(9)
 v(225) = pv(8)*pv(9)
 v(226) = pv(9)*pv(9)
 v(227) = pv(0)*pv(10)
 v(228) = pv(1)*pv(10)
 v(229) = pv(2)*pv(10)
 v(230) = pv(3)*pv(10)
 v(231) = pv(4)*pv(10)
 v(232) = pv(5)*pv(10)
 v(233) = pv(6)*pv(10)
 v(234) = pv(7)*pv(10)
 v(235) = pv(8)*pv(10)
 v(236) = pv(9)*pv(10)
 v(237) = pv(10)*pv(10)
 v(238) = pv(0)*pv(11)
 v(239) = pv(1)*pv(11)
 v(240) = pv(2)*pv(11)
 v(241) = pv(3)*pv(11)
 v(242) = pv(4)*pv(11)
 v(243) = pv(5)*pv(11)
 v(244) = pv(6)*pv(11)
 v(245) = pv(7)*pv(11)
 v(246) = pv(8)*pv(11)
 v(247) = pv(9)*pv(11)
 v(248) = pv(10)*pv(11)
 v(249) = pv(11)*pv(11)
 v(250) = pv(0)*pv(12)
 v(251) = pv(1)*pv(12)
 v(252) = pv(2)*pv(12)
 v(253) = pv(3)*pv(12)
 v(254) = pv(4)*pv(12)
 v(255) = pv(5)*pv(12)
 v(256) = pv(6)*pv(12)
 v(257) = pv(7)*pv(12)
 v(258) = pv(8)*pv(12)
 v(259) = pv(9)*pv(12)
 v(260) = pv(10)*pv(12)
 v(261) = pv(11)*pv(12)
 v(262) = pv(12)*pv(12)
 v(263) = pv(0)*pv(13)
 v(264) = pv(1)*pv(13)
 v(265) = pv(2)*pv(13)
 v(266) = pv(3)*pv(13)
 v(267) = pv(4)*pv(13)
 v(268) = pv(5)*pv(13)
 v(269) = pv(7)*pv(13)
 v(270) = pv(8)*pv(13)
 v(271) = pv(9)*pv(13)
 v(272) = pv(10)*pv(13)
 v(273) = pv(11)*pv(13)
 v(274) = pv(12)*pv(13)
 v(275) = pv(0)*pv(14)
 v(276) = pv(1)*pv(14)
 v(277) = pv(2)*pv(14)
 v(278) = pv(3)*pv(14)
 v(279) = pv(4)*pv(14)
 v(280) = pv(5)*pv(14)
 v(281) = pv(7)*pv(14)
 v(282) = pv(8)*pv(14)
 v(283) = pv(9)*pv(14)
 v(284) = pv(10)*pv(14)
 v(285) = pv(11)*pv(14)
 v(286) = pv(12)*pv(14)
 v(287) = pv(0)*pv(15)
 v(288) = pv(1)*pv(15)
 v(289) = pv(2)*pv(15)
 v(290) = pv(3)*pv(15)
 v(291) = pv(4)*pv(15)
 v(292) = pv(5)*pv(15)
 v(293) = pv(6)*pv(15)
 v(294) = pv(7)*pv(15)
 v(295) = pv(8)*pv(15)
 v(296) = pv(9)*pv(15)
 v(297) = pv(10)*pv(15)
 v(298) = pv(11)*pv(15)
 v(299) = pv(12)*pv(15)
 v(300) = pv(13)*pv(15)
 v(301) = pv(14)*pv(15)
 v(302) = pv(15)*pv(15)
 v(303) = pv(0)*pv(16)
 v(304) = pv(1)*pv(16)
 v(305) = pv(2)*pv(16)
 v(306) = pv(3)*pv(16)
 v(307) = pv(4)*pv(16)
 v(308) = pv(5)*pv(16)
 v(309) = pv(6)*pv(16)
 v(310) = pv(7)*pv(16)
 v(311) = pv(8)*pv(16)
 v(312) = pv(9)*pv(16)
 v(313) = pv(10)*pv(16)
 v(314) = pv(11)*pv(16)
 v(315) = pv(12)*pv(16)
 v(316) = pv(13)*pv(16)
 v(317) = pv(14)*pv(16)
 v(318) = pv(15)*pv(16)
 v(319) = pv(16)*pv(16)
 v(320) = pv(0)*pv(17)
 v(321) = pv(1)*pv(17)
 v(322) = pv(2)*pv(17)
 v(323) = pv(3)*pv(17)
 v(324) = pv(4)*pv(17)
 v(325) = pv(5)*pv(17)
 v(326) = pv(6)*pv(17)
 v(327) = pv(7)*pv(17)
 v(328) = pv(8)*pv(17)
 v(329) = pv(9)*pv(17)
 v(330) = pv(10)*pv(17)
 v(331) = pv(11)*pv(17)
 v(332) = pv(12)*pv(17)
 v(333) = pv(13)*pv(17)
 v(334) = pv(14)*pv(17)
 v(335) = pv(15)*pv(17)
 v(336) = pv(16)*pv(17)
 v(337) = pv(17)*pv(17)
 v(338) = pv(0)*pv(18)
 v(339) = pv(1)*pv(18)
 v(340) = pv(2)*pv(18)
 v(341) = pv(3)*pv(18)
 v(342) = pv(4)*pv(18)
 v(343) = pv(5)*pv(18)
 v(344) = pv(6)*pv(18)
 v(345) = pv(7)*pv(18)
 v(346) = pv(8)*pv(18)
 v(347) = pv(9)*pv(18)
 v(348) = pv(10)*pv(18)
 v(349) = pv(11)*pv(18)
 v(350) = pv(12)*pv(18)
 v(351) = pv(13)*pv(18)
 v(352) = pv(14)*pv(18)
 v(353) = pv(15)*pv(18)
 v(354) = pv(16)*pv(18)
 v(355) = pv(17)*pv(18)
 v(356) = pv(18)*pv(18)
 v(357) = pv(0)*pv(19)
 v(358) = pv(1)*pv(19)
 v(359) = pv(2)*pv(19)
 v(360) = pv(3)*pv(19)
 v(361) = pv(4)*pv(19)
 v(362) = pv(5)*pv(19)
 v(363) = pv(6)*pv(19)
 v(364) = pv(7)*pv(19)
 v(365) = pv(8)*pv(19)
 v(366) = pv(9)*pv(19)
 v(367) = pv(10)*pv(19)
 v(368) = pv(11)*pv(19)
 v(369) = pv(12)*pv(19)
 v(370) = pv(13)*pv(19)
 v(371) = pv(14)*pv(19)
 v(372) = pv(15)*pv(19)
 v(373) = pv(16)*pv(19)
 v(374) = pv(17)*pv(19)
 v(375) = pv(18)*pv(19)
 v(376) = pv(19)*pv(19)
 v(377) = pv(0)*pv(20)
 v(378) = pv(1)*pv(20)
 v(379) = pv(2)*pv(20)
 v(380) = pv(3)*pv(20)
 v(381) = pv(4)*pv(20)
 v(382) = pv(5)*pv(20)
 v(383) = pv(6)*pv(20)
 v(384) = pv(7)*pv(20)
 v(385) = pv(8)*pv(20)
 v(386) = pv(9)*pv(20)
 v(387) = pv(10)*pv(20)
 v(388) = pv(11)*pv(20)
 v(389) = pv(12)*pv(20)
 v(390) = pv(13)*pv(20)
 v(391) = pv(14)*pv(20)
 v(392) = pv(15)*pv(20)
 v(393) = pv(16)*pv(20)
 v(394) = pv(17)*pv(20)
 v(395) = pv(18)*pv(20)
 v(396) = pv(19)*pv(20)
 v(397) = pv(20)*pv(20)
 v(398) = pv(0)*pv(21)
 v(399) = pv(1)*pv(21)
 v(400) = pv(2)*pv(21)
 v(401) = pv(3)*pv(21)
 v(402) = pv(4)*pv(21)
 v(403) = pv(5)*pv(21)
 v(404) = pv(6)*pv(21)
 v(405) = pv(7)*pv(21)
 v(406) = pv(8)*pv(21)
 v(407) = pv(9)*pv(21)
 v(408) = pv(10)*pv(21)
 v(409) = pv(11)*pv(21)
 v(410) = pv(12)*pv(21)
 v(411) = pv(13)*pv(21)
 v(412) = pv(14)*pv(21)
 v(413) = pv(15)*pv(21)
 v(414) = pv(16)*pv(21)
 v(415) = pv(17)*pv(21)
 v(416) = pv(18)*pv(21)
 v(417) = pv(19)*pv(21)
 v(418) = pv(20)*pv(21)
 v(419) = pv(21)*pv(21)
 v(420) = pv(0)*pv(22)
 v(421) = pv(1)*pv(22)
 v(422) = pv(2)*pv(22)
 v(423) = pv(3)*pv(22)
 v(424) = pv(4)*pv(22)
 v(425) = pv(5)*pv(22)
 v(426) = pv(6)*pv(22)
 v(427) = pv(7)*pv(22)
 v(428) = pv(8)*pv(22)
 v(429) = pv(9)*pv(22)
 v(430) = pv(10)*pv(22)
 v(431) = pv(11)*pv(22)
 v(432) = pv(12)*pv(22)
 v(433) = pv(13)*pv(22)
 v(434) = pv(14)*pv(22)
 v(435) = pv(15)*pv(22)
 v(436) = pv(16)*pv(22)
 v(437) = pv(17)*pv(22)
 v(438) = pv(18)*pv(22)
 v(439) = pv(19)*pv(22)
 v(440) = pv(20)*pv(22)
 v(441) = pv(21)*pv(22)
 v(442) = pv(22)*pv(22)
 v(443) = pv(0)*pv(23)
 v(444) = pv(1)*pv(23)
 v(445) = pv(2)*pv(23)
 v(446) = pv(3)*pv(23)
 v(447) = pv(4)*pv(23)
 v(448) = pv(5)*pv(23)
 v(449) = pv(7)*pv(23)
 v(450) = pv(8)*pv(23)
 v(451) = pv(9)*pv(23)
 v(452) = pv(10)*pv(23)
 v(453) = pv(11)*pv(23)
 v(454) = pv(12)*pv(23)
 v(455) = pv(15)*pv(23)
 v(456) = pv(16)*pv(23)
 v(457) = pv(17)*pv(23)
 v(458) = pv(18)*pv(23)
 v(459) = pv(19)*pv(23)
 v(460) = pv(20)*pv(23)
 v(461) = pv(21)*pv(23)
 v(462) = pv(22)*pv(23)
 v(463) = pv(0)*pv(24)
 v(464) = pv(1)*pv(24)
 v(465) = pv(2)*pv(24)
 v(466) = pv(3)*pv(24)
 v(467) = pv(4)*pv(24)
 v(468) = pv(5)*pv(24)
 v(469) = pv(7)*pv(24)
 v(470) = pv(8)*pv(24)
 v(471) = pv(9)*pv(24)
 v(472) = pv(10)*pv(24)
 v(473) = pv(11)*pv(24)
 v(474) = pv(12)*pv(24)
 v(475) = pv(15)*pv(24)
 v(476) = pv(16)*pv(24)
 v(477) = pv(17)*pv(24)
 v(478) = pv(18)*pv(24)
 v(479) = pv(19)*pv(24)
 v(480) = pv(20)*pv(24)
 v(481) = pv(21)*pv(24)
 v(482) = pv(22)*pv(24)
 v(483) = pv(0)*pv(25)
 v(484) = pv(1)*pv(25)
 v(485) = pv(2)*pv(25)
 v(486) = pv(3)*pv(25)
 v(487) = pv(4)*pv(25)
 v(488) = pv(5)*pv(25)
 v(489) = pv(6)*pv(25)
 v(490) = pv(7)*pv(25)
 v(491) = pv(8)*pv(25)
 v(492) = pv(9)*pv(25)
 v(493) = pv(10)*pv(25)
 v(494) = pv(11)*pv(25)
 v(495) = pv(12)*pv(25)
 v(496) = pv(15)*pv(25)
 v(497) = pv(16)*pv(25)
 v(498) = pv(17)*pv(25)
 v(499) = pv(18)*pv(25)
 v(500) = pv(19)*pv(25)
 v(501) = pv(20)*pv(25)
 v(502) = pv(21)*pv(25)
 v(503) = pv(22)*pv(25)
 v(504:972) = pv(172:640)
endif
if (5.le.mxd) then
 stop 'mg33211: degree 5 not implemented'
endif
return
END SUBROUTINE mg33211_secs
