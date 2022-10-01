SUBROUTINE mg222111_secs (mxd, r, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: v(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: pv(0:size(v)-1) !! an upper bound size
if (size(r).ne.nk*nk.or.size(v).ne.mg222111_nsc(mxd)) then
 stop 'mg222111_secs: bad dimensions'
endif
call mg222111_isecs (mxd, r, pv)
v(0) = 1
if (2.le.mxd) then
 v(1:30) = pv(0:29)
endif
if (3.le.mxd) then
 v(31:106) = pv(30:105)
endif
if (4.le.mxd) then
 v(107) = pv(0)*pv(1)
 v(108) = pv(0)*pv(2)
 v(109) = pv(1)*pv(2)
 v(110) = pv(1)*pv(3)
 v(111) = pv(2)*pv(3)
 v(112) = pv(1)*pv(4)
 v(113) = pv(2)*pv(4)
 v(114) = pv(0)*pv(5)
 v(115) = pv(2)*pv(5)
 v(116) = pv(3)*pv(5)
 v(117) = pv(4)*pv(5)
 v(118) = pv(0)*pv(6)
 v(119) = pv(2)*pv(6)
 v(120) = pv(3)*pv(6)
 v(121) = pv(4)*pv(6)
 v(122) = pv(0)*pv(7)
 v(123) = pv(1)*pv(7)
 v(124) = pv(3)*pv(7)
 v(125) = pv(4)*pv(7)
 v(126) = pv(5)*pv(7)
 v(127) = pv(6)*pv(7)
 v(128) = pv(0)*pv(8)
 v(129) = pv(1)*pv(8)
 v(130) = pv(3)*pv(8)
 v(131) = pv(4)*pv(8)
 v(132) = pv(5)*pv(8)
 v(133) = pv(6)*pv(8)
 v(134) = pv(1)*pv(9)
 v(135) = pv(2)*pv(9)
 v(136) = pv(5)*pv(9)
 v(137) = pv(6)*pv(9)
 v(138) = pv(7)*pv(9)
 v(139) = pv(8)*pv(9)
 v(140) = pv(1)*pv(10)
 v(141) = pv(2)*pv(10)
 v(142) = pv(5)*pv(10)
 v(143) = pv(6)*pv(10)
 v(144) = pv(7)*pv(10)
 v(145) = pv(8)*pv(10)
 v(146) = pv(0)*pv(11)
 v(147) = pv(1)*pv(11)
 v(148) = pv(2)*pv(11)
 v(149) = pv(5)*pv(11)
 v(150) = pv(6)*pv(11)
 v(151) = pv(7)*pv(11)
 v(152) = pv(8)*pv(11)
 v(153) = pv(0)*pv(12)
 v(154) = pv(2)*pv(12)
 v(155) = pv(3)*pv(12)
 v(156) = pv(4)*pv(12)
 v(157) = pv(7)*pv(12)
 v(158) = pv(8)*pv(12)
 v(159) = pv(9)*pv(12)
 v(160) = pv(10)*pv(12)
 v(161) = pv(11)*pv(12)
 v(162) = pv(0)*pv(13)
 v(163) = pv(2)*pv(13)
 v(164) = pv(3)*pv(13)
 v(165) = pv(4)*pv(13)
 v(166) = pv(7)*pv(13)
 v(167) = pv(8)*pv(13)
 v(168) = pv(9)*pv(13)
 v(169) = pv(10)*pv(13)
 v(170) = pv(11)*pv(13)
 v(171) = pv(0)*pv(14)
 v(172) = pv(1)*pv(14)
 v(173) = pv(2)*pv(14)
 v(174) = pv(3)*pv(14)
 v(175) = pv(4)*pv(14)
 v(176) = pv(7)*pv(14)
 v(177) = pv(8)*pv(14)
 v(178) = pv(9)*pv(14)
 v(179) = pv(10)*pv(14)
 v(180) = pv(11)*pv(14)
 v(181) = pv(0)*pv(15)
 v(182) = pv(1)*pv(15)
 v(183) = pv(3)*pv(15)
 v(184) = pv(4)*pv(15)
 v(185) = pv(5)*pv(15)
 v(186) = pv(6)*pv(15)
 v(187) = pv(9)*pv(15)
 v(188) = pv(10)*pv(15)
 v(189) = pv(11)*pv(15)
 v(190) = pv(12)*pv(15)
 v(191) = pv(13)*pv(15)
 v(192) = pv(14)*pv(15)
 v(193) = pv(0)*pv(16)
 v(194) = pv(1)*pv(16)
 v(195) = pv(3)*pv(16)
 v(196) = pv(4)*pv(16)
 v(197) = pv(5)*pv(16)
 v(198) = pv(6)*pv(16)
 v(199) = pv(9)*pv(16)
 v(200) = pv(10)*pv(16)
 v(201) = pv(11)*pv(16)
 v(202) = pv(12)*pv(16)
 v(203) = pv(13)*pv(16)
 v(204) = pv(14)*pv(16)
 v(205) = pv(0)*pv(17)
 v(206) = pv(1)*pv(17)
 v(207) = pv(2)*pv(17)
 v(208) = pv(3)*pv(17)
 v(209) = pv(4)*pv(17)
 v(210) = pv(5)*pv(17)
 v(211) = pv(6)*pv(17)
 v(212) = pv(9)*pv(17)
 v(213) = pv(10)*pv(17)
 v(214) = pv(11)*pv(17)
 v(215) = pv(12)*pv(17)
 v(216) = pv(13)*pv(17)
 v(217) = pv(14)*pv(17)
 v(218) = pv(1)*pv(18)
 v(219) = pv(2)*pv(18)
 v(220) = pv(5)*pv(18)
 v(221) = pv(6)*pv(18)
 v(222) = pv(7)*pv(18)
 v(223) = pv(8)*pv(18)
 v(224) = pv(12)*pv(18)
 v(225) = pv(13)*pv(18)
 v(226) = pv(14)*pv(18)
 v(227) = pv(15)*pv(18)
 v(228) = pv(16)*pv(18)
 v(229) = pv(17)*pv(18)
 v(230) = pv(1)*pv(19)
 v(231) = pv(2)*pv(19)
 v(232) = pv(5)*pv(19)
 v(233) = pv(6)*pv(19)
 v(234) = pv(7)*pv(19)
 v(235) = pv(8)*pv(19)
 v(236) = pv(12)*pv(19)
 v(237) = pv(13)*pv(19)
 v(238) = pv(14)*pv(19)
 v(239) = pv(15)*pv(19)
 v(240) = pv(16)*pv(19)
 v(241) = pv(17)*pv(19)
 v(242) = pv(0)*pv(20)
 v(243) = pv(1)*pv(20)
 v(244) = pv(2)*pv(20)
 v(245) = pv(5)*pv(20)
 v(246) = pv(6)*pv(20)
 v(247) = pv(7)*pv(20)
 v(248) = pv(8)*pv(20)
 v(249) = pv(12)*pv(20)
 v(250) = pv(13)*pv(20)
 v(251) = pv(14)*pv(20)
 v(252) = pv(15)*pv(20)
 v(253) = pv(16)*pv(20)
 v(254) = pv(17)*pv(20)
 v(255) = pv(0)*pv(21)
 v(256) = pv(1)*pv(21)
 v(257) = pv(2)*pv(21)
 v(258) = pv(3)*pv(21)
 v(259) = pv(4)*pv(21)
 v(260) = pv(5)*pv(21)
 v(261) = pv(6)*pv(21)
 v(262) = pv(7)*pv(21)
 v(263) = pv(8)*pv(21)
 v(264) = pv(12)*pv(21)
 v(265) = pv(13)*pv(21)
 v(266) = pv(14)*pv(21)
 v(267) = pv(15)*pv(21)
 v(268) = pv(16)*pv(21)
 v(269) = pv(17)*pv(21)
 v(270) = pv(0)*pv(22)
 v(271) = pv(2)*pv(22)
 v(272) = pv(3)*pv(22)
 v(273) = pv(4)*pv(22)
 v(274) = pv(7)*pv(22)
 v(275) = pv(8)*pv(22)
 v(276) = pv(9)*pv(22)
 v(277) = pv(10)*pv(22)
 v(278) = pv(11)*pv(22)
 v(279) = pv(15)*pv(22)
 v(280) = pv(16)*pv(22)
 v(281) = pv(17)*pv(22)
 v(282) = pv(18)*pv(22)
 v(283) = pv(19)*pv(22)
 v(284) = pv(20)*pv(22)
 v(285) = pv(21)*pv(22)
 v(286) = pv(0)*pv(23)
 v(287) = pv(2)*pv(23)
 v(288) = pv(3)*pv(23)
 v(289) = pv(4)*pv(23)
 v(290) = pv(7)*pv(23)
 v(291) = pv(8)*pv(23)
 v(292) = pv(9)*pv(23)
 v(293) = pv(10)*pv(23)
 v(294) = pv(11)*pv(23)
 v(295) = pv(15)*pv(23)
 v(296) = pv(16)*pv(23)
 v(297) = pv(17)*pv(23)
 v(298) = pv(18)*pv(23)
 v(299) = pv(19)*pv(23)
 v(300) = pv(20)*pv(23)
 v(301) = pv(21)*pv(23)
 v(302) = pv(0)*pv(24)
 v(303) = pv(1)*pv(24)
 v(304) = pv(2)*pv(24)
 v(305) = pv(3)*pv(24)
 v(306) = pv(4)*pv(24)
 v(307) = pv(7)*pv(24)
 v(308) = pv(8)*pv(24)
 v(309) = pv(9)*pv(24)
 v(310) = pv(10)*pv(24)
 v(311) = pv(11)*pv(24)
 v(312) = pv(15)*pv(24)
 v(313) = pv(16)*pv(24)
 v(314) = pv(17)*pv(24)
 v(315) = pv(18)*pv(24)
 v(316) = pv(19)*pv(24)
 v(317) = pv(20)*pv(24)
 v(318) = pv(21)*pv(24)
 v(319) = pv(0)*pv(25)
 v(320) = pv(1)*pv(25)
 v(321) = pv(2)*pv(25)
 v(322) = pv(3)*pv(25)
 v(323) = pv(4)*pv(25)
 v(324) = pv(5)*pv(25)
 v(325) = pv(6)*pv(25)
 v(326) = pv(7)*pv(25)
 v(327) = pv(8)*pv(25)
 v(328) = pv(9)*pv(25)
 v(329) = pv(10)*pv(25)
 v(330) = pv(11)*pv(25)
 v(331) = pv(15)*pv(25)
 v(332) = pv(16)*pv(25)
 v(333) = pv(17)*pv(25)
 v(334) = pv(18)*pv(25)
 v(335) = pv(19)*pv(25)
 v(336) = pv(20)*pv(25)
 v(337) = pv(21)*pv(25)
 v(338) = pv(0)*pv(26)
 v(339) = pv(1)*pv(26)
 v(340) = pv(3)*pv(26)
 v(341) = pv(4)*pv(26)
 v(342) = pv(5)*pv(26)
 v(343) = pv(6)*pv(26)
 v(344) = pv(9)*pv(26)
 v(345) = pv(10)*pv(26)
 v(346) = pv(11)*pv(26)
 v(347) = pv(12)*pv(26)
 v(348) = pv(13)*pv(26)
 v(349) = pv(14)*pv(26)
 v(350) = pv(18)*pv(26)
 v(351) = pv(19)*pv(26)
 v(352) = pv(20)*pv(26)
 v(353) = pv(21)*pv(26)
 v(354) = pv(22)*pv(26)
 v(355) = pv(23)*pv(26)
 v(356) = pv(24)*pv(26)
 v(357) = pv(25)*pv(26)
 v(358) = pv(0)*pv(27)
 v(359) = pv(1)*pv(27)
 v(360) = pv(3)*pv(27)
 v(361) = pv(4)*pv(27)
 v(362) = pv(5)*pv(27)
 v(363) = pv(6)*pv(27)
 v(364) = pv(9)*pv(27)
 v(365) = pv(10)*pv(27)
 v(366) = pv(11)*pv(27)
 v(367) = pv(12)*pv(27)
 v(368) = pv(13)*pv(27)
 v(369) = pv(14)*pv(27)
 v(370) = pv(18)*pv(27)
 v(371) = pv(19)*pv(27)
 v(372) = pv(20)*pv(27)
 v(373) = pv(21)*pv(27)
 v(374) = pv(22)*pv(27)
 v(375) = pv(23)*pv(27)
 v(376) = pv(24)*pv(27)
 v(377) = pv(25)*pv(27)
 v(378) = pv(0)*pv(28)
 v(379) = pv(1)*pv(28)
 v(380) = pv(2)*pv(28)
 v(381) = pv(3)*pv(28)
 v(382) = pv(4)*pv(28)
 v(383) = pv(5)*pv(28)
 v(384) = pv(6)*pv(28)
 v(385) = pv(9)*pv(28)
 v(386) = pv(10)*pv(28)
 v(387) = pv(11)*pv(28)
 v(388) = pv(12)*pv(28)
 v(389) = pv(13)*pv(28)
 v(390) = pv(14)*pv(28)
 v(391) = pv(18)*pv(28)
 v(392) = pv(19)*pv(28)
 v(393) = pv(20)*pv(28)
 v(394) = pv(21)*pv(28)
 v(395) = pv(22)*pv(28)
 v(396) = pv(23)*pv(28)
 v(397) = pv(24)*pv(28)
 v(398) = pv(25)*pv(28)
 v(399) = pv(0)*pv(29)
 v(400) = pv(1)*pv(29)
 v(401) = pv(2)*pv(29)
 v(402) = pv(3)*pv(29)
 v(403) = pv(4)*pv(29)
 v(404) = pv(5)*pv(29)
 v(405) = pv(6)*pv(29)
 v(406) = pv(7)*pv(29)
 v(407) = pv(8)*pv(29)
 v(408) = pv(9)*pv(29)
 v(409) = pv(10)*pv(29)
 v(410) = pv(11)*pv(29)
 v(411) = pv(12)*pv(29)
 v(412) = pv(13)*pv(29)
 v(413) = pv(14)*pv(29)
 v(414) = pv(18)*pv(29)
 v(415) = pv(19)*pv(29)
 v(416) = pv(20)*pv(29)
 v(417) = pv(21)*pv(29)
 v(418) = pv(22)*pv(29)
 v(419) = pv(23)*pv(29)
 v(420) = pv(24)*pv(29)
 v(421) = pv(25)*pv(29)
 v(422:496) = pv(106:180)
endif
if (5.le.mxd) then
 stop 'mg222111: degree 5 not implemented'
endif
return
END SUBROUTINE mg222111_secs
