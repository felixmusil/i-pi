SUBROUTINE mg3111_secs (mxd, r, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: v(0:)
!! Note: We stop at degree 8 for now.  At degree 9, Magma computation
!! ballooned to 43 GB and failed to progress.
!-----------------------------------------------------------------------
real (kind=wp) :: pv(0:size(v)-1) !! an upper bound size
if (size(r).ne.nk*nk.or.size(v).ne.mg3111_nsc(mxd)) then
 stop 'mg3111_secs: bad dimensions'
endif
call mg3111_isecs (mxd, r, pv)
v(0) = 1
if (2.le.mxd) then
 v(1:6) = pv(0:5)
endif
if (3.le.mxd) then
 v(7:22) = pv(6:21)
endif
if (4.le.mxd) then
 v(23) = pv(0)*pv(0)
 v(24) = pv(0)*pv(1)
 v(25) = pv(1)*pv(1)
 v(26) = pv(0)*pv(2)
 v(27) = pv(1)*pv(2)
 v(28) = pv(2)*pv(2)
 v(29) = pv(0)*pv(3)
 v(30) = pv(1)*pv(3)
 v(31) = pv(2)*pv(3)
 v(32) = pv(3)*pv(3)
 v(33) = pv(0)*pv(4)
 v(34) = pv(1)*pv(4)
 v(35) = pv(2)*pv(4)
 v(36) = pv(3)*pv(4)
 v(37) = pv(4)*pv(4)
 v(38) = pv(0)*pv(5)
 v(39) = pv(1)*pv(5)
 v(40) = pv(2)*pv(5)
 v(41) = pv(3)*pv(5)
 v(42) = pv(4)*pv(5)
 v(43) = pv(5)*pv(5)
endif
if (5.le.mxd) then
 v(44) = pv(1)*pv(7)
 v(45) = pv(3)*pv(7)
 v(46) = pv(0)*pv(8)
 v(47) = pv(3)*pv(9)
 v(48) = pv(0)*pv(10)
 v(49) = pv(3)*pv(10)
 v(50) = pv(0)*pv(11)
 v(51) = pv(3)*pv(11)
 v(52) = pv(0)*pv(12)
 v(53) = pv(1)*pv(12)
 v(54) = pv(3)*pv(12)
 v(55) = pv(4)*pv(12)
 v(56) = pv(0)*pv(13)
 v(57) = pv(1)*pv(13)
 v(58) = pv(1)*pv(14)
 v(59) = pv(0)*pv(15)
 v(60) = pv(1)*pv(15)
 v(61) = pv(2)*pv(15)
 v(62) = pv(0)*pv(16)
 v(63) = pv(0)*pv(17)
 v(64) = pv(1)*pv(17)
 v(65) = pv(0)*pv(18)
 v(66) = pv(1)*pv(18)
 v(67) = pv(2)*pv(18)
 v(68) = pv(0)*pv(19)
 v(69) = pv(1)*pv(19)
 v(70) = pv(2)*pv(19)
 v(71) = pv(0)*pv(20)
 v(72) = pv(1)*pv(20)
 v(73) = pv(2)*pv(20)
 v(74) = pv(3)*pv(20)
 v(75) = pv(0)*pv(21)
 v(76) = pv(1)*pv(21)
 v(77) = pv(2)*pv(21)
 v(78) = pv(3)*pv(21)
 v(79) = pv(4)*pv(21)
endif
if (6.le.mxd) then
 v(80) = pv(6)*pv(7)
 v(81) = pv(6)*pv(9)
 v(82) = pv(6)*pv(10)
 v(83) = pv(6)*pv(11)
 v(84) = pv(8)*pv(11)
 v(85) = pv(6)*pv(12)
 v(86) = pv(7)*pv(12)
 v(87) = pv(8)*pv(12)
 v(88) = pv(9)*pv(12)
 v(89) = pv(10)*pv(12)
 v(90) = pv(10)*pv(13)
 v(91) = pv(12)*pv(13)
 v(92) = pv(6)*pv(14)
 v(93) = pv(6)*pv(15)
 v(94) = pv(8)*pv(15)
 v(95) = pv(11)*pv(15)
 v(96) = pv(6)*pv(16)
 v(97) = pv(8)*pv(16)
 v(98) = pv(6)*pv(17)
 v(99) = pv(7)*pv(17)
 v(100) = pv(8)*pv(17)
 v(101) = pv(9)*pv(17)
 v(102) = pv(10)*pv(17)
 v(103) = pv(6)*pv(18)
 v(104) = pv(7)*pv(18)
 v(105) = pv(8)*pv(18)
 v(106) = pv(9)*pv(18)
 v(107) = pv(10)*pv(18)
 v(108) = pv(6)*pv(19)
 v(109) = pv(8)*pv(19)
 v(110) = pv(9)*pv(19)
 v(111) = pv(10)*pv(19)
 v(112) = pv(12)*pv(19)
 v(113) = pv(13)*pv(19)
 v(114) = pv(6)*pv(20)
 v(115) = pv(7)*pv(20)
 v(116) = pv(8)*pv(20)
 v(117) = pv(9)*pv(20)
 v(118) = pv(10)*pv(20)
 v(119) = pv(11)*pv(20)
 v(120) = pv(13)*pv(20)
 v(121) = pv(14)*pv(20)
 v(122) = pv(15)*pv(20)
 v(123) = pv(6)*pv(21)
 v(124) = pv(7)*pv(21)
 v(125) = pv(8)*pv(21)
 v(126) = pv(9)*pv(21)
 v(127) = pv(10)*pv(21)
 v(128) = pv(11)*pv(21)
 v(129) = pv(12)*pv(21)
 v(130) = pv(13)*pv(21)
 v(131) = pv(14)*pv(21)
 v(132) = pv(15)*pv(21)
 v(133) = pv(16)*pv(21)
 v(134) = pv(17)*pv(21)
 v(135) = pv(18)*pv(21)
endif
if (7.le.mxd) then
 v(136) = pv(3)*pv(3)*pv(10)
 v(137) = pv(0)*pv(0)*pv(11)
 v(138) = pv(0)*pv(0)*pv(12)
 v(139) = pv(0)*pv(1)*pv(12)
 v(140) = pv(3)*pv(3)*pv(12)
 v(141) = pv(3)*pv(4)*pv(12)
 v(142) = pv(1)*pv(1)*pv(14)
 v(143) = pv(1)*pv(1)*pv(15)
 v(144) = pv(1)*pv(2)*pv(15)
 v(145) = pv(0)*pv(0)*pv(16)
 v(146) = pv(0)*pv(0)*pv(17)
 v(147) = pv(0)*pv(1)*pv(17)
 v(148) = pv(0)*pv(0)*pv(18)
 v(149) = pv(0)*pv(1)*pv(18)
 v(150) = pv(0)*pv(2)*pv(18)
 v(151) = pv(0)*pv(0)*pv(19)
 v(152) = pv(0)*pv(1)*pv(19)
 v(153) = pv(1)*pv(1)*pv(19)
 v(154) = pv(0)*pv(0)*pv(20)
 v(155) = pv(0)*pv(1)*pv(20)
 v(156) = pv(1)*pv(1)*pv(20)
 v(157) = pv(0)*pv(2)*pv(20)
 v(158) = pv(1)*pv(2)*pv(20)
 v(159) = pv(2)*pv(2)*pv(20)
 v(160) = pv(0)*pv(3)*pv(20)
 v(161) = pv(0)*pv(0)*pv(21)
 v(162) = pv(0)*pv(1)*pv(21)
 v(163) = pv(1)*pv(1)*pv(21)
 v(164) = pv(0)*pv(2)*pv(21)
 v(165) = pv(1)*pv(2)*pv(21)
 v(166) = pv(2)*pv(2)*pv(21)
 v(167) = pv(0)*pv(3)*pv(21)
 v(168) = pv(1)*pv(3)*pv(21)
 v(169) = pv(0)*pv(4)*pv(21)
 v(170) = pv(1)*pv(4)*pv(21)
 v(171) = pv(2)*pv(4)*pv(21)
endif
if (8.le.mxd) then
 v(172) = pv(0)*pv(12)*pv(13)
 v(173) = pv(0)*pv(8)*pv(15)
 v(174) = pv(0)*pv(11)*pv(15)
 v(175) = pv(1)*pv(7)*pv(18)
 v(176) = pv(0)*pv(8)*pv(18)
 v(177) = pv(0)*pv(10)*pv(18)
 v(178) = pv(0)*pv(12)*pv(19)
 v(179) = pv(1)*pv(7)*pv(20)
 v(180) = pv(0)*pv(8)*pv(20)
 v(181) = pv(0)*pv(11)*pv(20)
 v(182) = pv(1)*pv(7)*pv(21)
 v(183) = pv(3)*pv(7)*pv(21)
 v(184) = pv(0)*pv(8)*pv(21)
 v(185) = pv(0)*pv(10)*pv(21)
 v(186) = pv(0)*pv(11)*pv(21)
 v(187) = pv(0)*pv(12)*pv(21)
 v(188) = pv(0)*pv(13)*pv(21)
 v(189) = pv(0)*pv(15)*pv(21)
 v(190) = pv(0)*pv(16)*pv(21)
 v(191) = pv(0)*pv(17)*pv(21)
 v(192) = pv(0)*pv(18)*pv(21)
endif
if (9.le.mxd) then
 stop 'mg3111: degree 9 not implemented' !! Tried it 050813
endif
return
END SUBROUTINE mg3111_secs
