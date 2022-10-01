SUBROUTINE mg221111_secs (mxd, r, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: v(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: pv(0:size(v)-1) !! an upper bound size
if (size(r).ne.nk*nk.or.size(v).ne.mg221111_nsc(mxd)) then
 stop 'mg221111_secs: bad dimensions'
endif
call mg221111_isecs (mxd, r, pv)
v(0) = 1
if (2.le.mxd) then
 v(1:20) = pv(0:19)
endif
if (3.le.mxd) then
 v(21:45) = pv(20:44)
endif
if (4.le.mxd) then
 v(46) = pv(0)*pv(1)
 v(47) = pv(1)*pv(2)
 v(48) = pv(1)*pv(3)
 v(49) = pv(0)*pv(4)
 v(50) = pv(2)*pv(4)
 v(51) = pv(3)*pv(4)
 v(52) = pv(0)*pv(5)
 v(53) = pv(2)*pv(5)
 v(54) = pv(3)*pv(5)
 v(55) = pv(1)*pv(6)
 v(56) = pv(4)*pv(6)
 v(57) = pv(5)*pv(6)
 v(58) = pv(1)*pv(7)
 v(59) = pv(4)*pv(7)
 v(60) = pv(5)*pv(7)
 v(61) = pv(0)*pv(8)
 v(62) = pv(1)*pv(8)
 v(63) = pv(4)*pv(8)
 v(64) = pv(5)*pv(8)
 v(65) = pv(0)*pv(9)
 v(66) = pv(2)*pv(9)
 v(67) = pv(3)*pv(9)
 v(68) = pv(6)*pv(9)
 v(69) = pv(7)*pv(9)
 v(70) = pv(8)*pv(9)
 v(71) = pv(0)*pv(10)
 v(72) = pv(2)*pv(10)
 v(73) = pv(3)*pv(10)
 v(74) = pv(6)*pv(10)
 v(75) = pv(7)*pv(10)
 v(76) = pv(8)*pv(10)
 v(77) = pv(0)*pv(11)
 v(78) = pv(1)*pv(11)
 v(79) = pv(2)*pv(11)
 v(80) = pv(3)*pv(11)
 v(81) = pv(6)*pv(11)
 v(82) = pv(7)*pv(11)
 v(83) = pv(8)*pv(11)
 v(84) = pv(1)*pv(12)
 v(85) = pv(4)*pv(12)
 v(86) = pv(5)*pv(12)
 v(87) = pv(9)*pv(12)
 v(88) = pv(10)*pv(12)
 v(89) = pv(11)*pv(12)
 v(90) = pv(1)*pv(13)
 v(91) = pv(4)*pv(13)
 v(92) = pv(5)*pv(13)
 v(93) = pv(9)*pv(13)
 v(94) = pv(10)*pv(13)
 v(95) = pv(11)*pv(13)
 v(96) = pv(0)*pv(14)
 v(97) = pv(1)*pv(14)
 v(98) = pv(4)*pv(14)
 v(99) = pv(5)*pv(14)
 v(100) = pv(9)*pv(14)
 v(101) = pv(10)*pv(14)
 v(102) = pv(11)*pv(14)
 v(103) = pv(0)*pv(15)
 v(104) = pv(1)*pv(15)
 v(105) = pv(2)*pv(15)
 v(106) = pv(3)*pv(15)
 v(107) = pv(4)*pv(15)
 v(108) = pv(5)*pv(15)
 v(109) = pv(9)*pv(15)
 v(110) = pv(10)*pv(15)
 v(111) = pv(11)*pv(15)
 v(112) = pv(0)*pv(16)
 v(113) = pv(2)*pv(16)
 v(114) = pv(3)*pv(16)
 v(115) = pv(6)*pv(16)
 v(116) = pv(7)*pv(16)
 v(117) = pv(8)*pv(16)
 v(118) = pv(12)*pv(16)
 v(119) = pv(13)*pv(16)
 v(120) = pv(14)*pv(16)
 v(121) = pv(15)*pv(16)
 v(122) = pv(0)*pv(17)
 v(123) = pv(2)*pv(17)
 v(124) = pv(3)*pv(17)
 v(125) = pv(6)*pv(17)
 v(126) = pv(7)*pv(17)
 v(127) = pv(8)*pv(17)
 v(128) = pv(12)*pv(17)
 v(129) = pv(13)*pv(17)
 v(130) = pv(14)*pv(17)
 v(131) = pv(15)*pv(17)
 v(132) = pv(0)*pv(18)
 v(133) = pv(1)*pv(18)
 v(134) = pv(2)*pv(18)
 v(135) = pv(3)*pv(18)
 v(136) = pv(6)*pv(18)
 v(137) = pv(7)*pv(18)
 v(138) = pv(8)*pv(18)
 v(139) = pv(12)*pv(18)
 v(140) = pv(13)*pv(18)
 v(141) = pv(14)*pv(18)
 v(142) = pv(15)*pv(18)
 v(143) = pv(0)*pv(19)
 v(144) = pv(1)*pv(19)
 v(145) = pv(2)*pv(19)
 v(146) = pv(3)*pv(19)
 v(147) = pv(4)*pv(19)
 v(148) = pv(5)*pv(19)
 v(149) = pv(6)*pv(19)
 v(150) = pv(7)*pv(19)
 v(151) = pv(8)*pv(19)
 v(152) = pv(12)*pv(19)
 v(153) = pv(13)*pv(19)
 v(154) = pv(14)*pv(19)
 v(155) = pv(15)*pv(19)
endif
if (5.le.mxd) then
 v(156) = pv(0)*pv(24)
 v(157) = pv(0)*pv(25)
 v(158) = pv(1)*pv(26)
 v(159) = pv(1)*pv(27)
 v(160) = pv(0)*pv(28)
 v(161) = pv(1)*pv(28)
 v(162) = pv(0)*pv(29)
 v(163) = pv(2)*pv(29)
 v(164) = pv(3)*pv(29)
 v(165) = pv(0)*pv(30)
 v(166) = pv(2)*pv(30)
 v(167) = pv(3)*pv(30)
 v(168) = pv(0)*pv(31)
 v(169) = pv(1)*pv(31)
 v(170) = pv(2)*pv(31)
 v(171) = pv(3)*pv(31)
 v(172) = pv(1)*pv(32)
 v(173) = pv(4)*pv(32)
 v(174) = pv(5)*pv(32)
 v(175) = pv(1)*pv(33)
 v(176) = pv(4)*pv(33)
 v(177) = pv(5)*pv(33)
 v(178) = pv(0)*pv(34)
 v(179) = pv(1)*pv(34)
 v(180) = pv(4)*pv(34)
 v(181) = pv(5)*pv(34)
 v(182) = pv(0)*pv(35)
 v(183) = pv(1)*pv(35)
 v(184) = pv(2)*pv(35)
 v(185) = pv(3)*pv(35)
 v(186) = pv(4)*pv(35)
 v(187) = pv(5)*pv(35)
 v(188) = pv(0)*pv(36)
 v(189) = pv(2)*pv(36)
 v(190) = pv(3)*pv(36)
 v(191) = pv(6)*pv(36)
 v(192) = pv(7)*pv(36)
 v(193) = pv(8)*pv(36)
 v(194) = pv(0)*pv(37)
 v(195) = pv(2)*pv(37)
 v(196) = pv(3)*pv(37)
 v(197) = pv(6)*pv(37)
 v(198) = pv(7)*pv(37)
 v(199) = pv(8)*pv(37)
 v(200) = pv(0)*pv(38)
 v(201) = pv(1)*pv(38)
 v(202) = pv(2)*pv(38)
 v(203) = pv(3)*pv(38)
 v(204) = pv(6)*pv(38)
 v(205) = pv(7)*pv(38)
 v(206) = pv(8)*pv(38)
 v(207) = pv(0)*pv(39)
 v(208) = pv(1)*pv(39)
 v(209) = pv(2)*pv(39)
 v(210) = pv(3)*pv(39)
 v(211) = pv(4)*pv(39)
 v(212) = pv(5)*pv(39)
 v(213) = pv(6)*pv(39)
 v(214) = pv(7)*pv(39)
 v(215) = pv(8)*pv(39)
 v(216) = pv(1)*pv(40)
 v(217) = pv(4)*pv(40)
 v(218) = pv(5)*pv(40)
 v(219) = pv(9)*pv(40)
 v(220) = pv(10)*pv(40)
 v(221) = pv(11)*pv(40)
 v(222) = pv(1)*pv(41)
 v(223) = pv(4)*pv(41)
 v(224) = pv(5)*pv(41)
 v(225) = pv(9)*pv(41)
 v(226) = pv(10)*pv(41)
 v(227) = pv(11)*pv(41)
 v(228) = pv(0)*pv(42)
 v(229) = pv(1)*pv(42)
 v(230) = pv(4)*pv(42)
 v(231) = pv(5)*pv(42)
 v(232) = pv(9)*pv(42)
 v(233) = pv(10)*pv(42)
 v(234) = pv(11)*pv(42)
 v(235) = pv(0)*pv(43)
 v(236) = pv(1)*pv(43)
 v(237) = pv(2)*pv(43)
 v(238) = pv(3)*pv(43)
 v(239) = pv(4)*pv(43)
 v(240) = pv(5)*pv(43)
 v(241) = pv(9)*pv(43)
 v(242) = pv(10)*pv(43)
 v(243) = pv(11)*pv(43)
 v(244) = pv(0)*pv(44)
 v(245) = pv(1)*pv(44)
 v(246) = pv(2)*pv(44)
 v(247) = pv(3)*pv(44)
 v(248) = pv(4)*pv(44)
 v(249) = pv(5)*pv(44)
 v(250) = pv(6)*pv(44)
 v(251) = pv(7)*pv(44)
 v(252) = pv(8)*pv(44)
 v(253) = pv(9)*pv(44)
 v(254) = pv(10)*pv(44)
 v(255) = pv(11)*pv(44)
endif
if (6.le.mxd) then
 stop 'mg221111: degree 6 not implemented'
endif
return
END SUBROUTINE mg221111_secs
