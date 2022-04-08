subroutine mg42_secs (mxd, x, v)
integer, intent (in) :: mxd
real , intent (in) :: x(0:mg42_nr-1)
real , intent (out) :: v(0:mg42_ivs(mxd)-1)
!! Note: We stop at degree 7 for now.
!-----------------------------------------------------------------------
integer, parameter :: m=4, n=2, nk=6, npv=107, &
  m2=m*(m-1), m3=m*(m-1)*(m-2), n2=n*(n-1), mn=m*n, m2n=m*(m-1)*n, &
  mn2=m*n*(n-1), m2n2=m2n*(n-1), m3n=m*(m-1)*(m-2)*n, &
  m3n2=m*(m-1)*(m-2)*n*(n-1)
integer :: i0, i1, i2, j0, j1
real  :: pv(0:npv-1), d(0:nk-1,0:nk-1), d2(0:nk-1,0:nk-1), &
  d3(0:nk-1,0:nk-1), d4(0:nk-1,0:nk-1), d5(0:nk-1,0:nk-1), &
  d6(0:nk-1,0:nk-1), d7(0:nk-1,0:nk-1)
call mg42_setd ()
pv = 0
do i0 = 0, m-1
 do i1 = 0, m-1
 if (i1.ne.i0) then
  if (4.le.mxd) then
   call mg42_deg4_i1 ()
  endif
  if (5.le.mxd) then
   call mg42_deg5_i1 ()
  endif
  do i2 = 0, m-1
  if (i2.ne.i0.and.i2.ne.i1) then
   if (3.le.mxd) then
    call mg42_deg3_i2 ()
   endif
  endif
  enddo
 endif
 enddo
enddo
do j0 = m, m+n-1
 do i0 = 0, m-1
  if (3.le.mxd) then
   call mg42_deg3_i0j0 ()
  endif
  if (5.le.mxd) then
   call mg42_deg5_i0j0 ()
  endif
  if (7.le.mxd) then
   call mg42_deg7_i0j0 ()
  endif
  do i1 = 0, m-1
  if (i1.ne.i0) then
   if (2.le.mxd) then
    call mg42_deg2_i1j0 ()
   endif
   if (3.le.mxd) then
    call mg42_deg3_i1j0 ()
   endif
   if (4.le.mxd) then
    call mg42_deg4_i1j0 ()
   endif
   if (5.le.mxd) then
    call mg42_deg5_i1j0 ()
   endif
   if (6.le.mxd) then
    call mg42_deg6_i1j0 ()
   endif
   if (7.le.mxd) then
    call mg42_deg7_i1j0 ()
   endif
   do i2 = 0, m-1
   if (i2.ne.i0.and.i2.ne.i1) then
    if (3.le.mxd) then
     call mg42_deg3_i2j0 ()
    endif
    if (4.le.mxd) then
     call mg42_deg4_i2j0 ()
    endif
    if (5.le.mxd) then
     call mg42_deg5_i2j0 ()
    endif
    if (6.le.mxd) then
     call mg42_deg6_i2j0 ()
    endif
    if (7.le.mxd) then
     call mg42_deg7_i2j0 ()
    endif
   endif
   enddo
  endif
  enddo
 enddo
 do j1 = m, m+n-1
 if (j1.ne.j0) then
  do i0 = 0, m-1
   if (4.le.mxd) then
    call mg42_deg4_i0j1 ()
   endif
   if (5.le.mxd) then
    call mg42_deg5_i0j1 ()
   endif
   if (6.le.mxd) then
    call mg42_deg6_i0j1 ()
   endif
   do i1 = 0, m-1
   if (i1.ne.i0) then
    if (3.le.mxd) then
     call mg42_deg3_i1j1 ()
    endif
    if (4.le.mxd) then
     call mg42_deg4_i1j1 ()
    endif
    if (5.le.mxd) then
     call mg42_deg5_i1j1 ()
    endif
    if (6.le.mxd) then
     call mg42_deg6_i1j1 ()
    endif
    do i2 = 0, m-1
    if (i2.ne.i0.and.i2.ne.i1) then
     if (4.le.mxd) then
      call mg42_deg4_i2j1 ()
     endif
     if (5.le.mxd) then
      call mg42_deg5_i2j1 ()
     endif
     if (6.le.mxd) then
      call mg42_deg6_i2j1 ()
     endif
     if (7.le.mxd) then
      call mg42_deg7_i2j1 ()
     endif
    endif
    enddo
   endif
   enddo
  enddo
 endif
 enddo
enddo
v(0) = 1
if (2.le.mxd) then
 v(1:1) = pv(0:0)
endif
if (3.le.mxd) then
 v(2:11) = pv(1:10)
endif
if (4.le.mxd) then
 v(12) = pv(0)*pv(0)
 v(13:35) = pv(11:33)
endif
if (5.le.mxd) then
 v(36) = pv(0)*pv(1)
 v(37) = pv(0)*pv(2)
 v(38) = pv(0)*pv(3)
 v(39) = pv(0)*pv(4)
 v(40) = pv(0)*pv(5)
 v(41) = pv(0)*pv(6)
 v(42) = pv(0)*pv(7)
 v(43) = pv(0)*pv(8)
 v(44) = pv(0)*pv(9)
 v(45) = pv(0)*pv(10)
 v(46:82) = pv(34:70)
endif
if (6.le.mxd) then
 v(83) = pv(1)*pv(1)
 v(84) = pv(1)*pv(2)
 v(85) = pv(1)*pv(3)
 v(86) = pv(1)*pv(4)
 v(87) = pv(3)*pv(4)
 v(88) = pv(1)*pv(5)
 v(89) = pv(2)*pv(5)
 v(90) = pv(3)*pv(5)
 v(91) = pv(1)*pv(6)
 v(92) = pv(2)*pv(6)
 v(93) = pv(3)*pv(6)
 v(94) = pv(5)*pv(6)
 v(95) = pv(6)*pv(6)
 v(96) = pv(1)*pv(7)
 v(97) = pv(2)*pv(7)
 v(98) = pv(3)*pv(7)
 v(99) = pv(4)*pv(7)
 v(100) = pv(5)*pv(7)
 v(101) = pv(6)*pv(7)
 v(102) = pv(7)*pv(7)
 v(103) = pv(1)*pv(8)
 v(104) = pv(2)*pv(8)
 v(105) = pv(3)*pv(8)
 v(106) = pv(4)*pv(8)
 v(107) = pv(5)*pv(8)
 v(108) = pv(6)*pv(8)
 v(109) = pv(7)*pv(8)
 v(110) = pv(8)*pv(8)
 v(111) = pv(1)*pv(9)
 v(112) = pv(2)*pv(9)
 v(113) = pv(3)*pv(9)
 v(114) = pv(4)*pv(9)
 v(115) = pv(5)*pv(9)
 v(116) = pv(6)*pv(9)
 v(117) = pv(7)*pv(9)
 v(118) = pv(8)*pv(9)
 v(119) = pv(9)*pv(9)
 v(120) = pv(1)*pv(10)
 v(121) = pv(2)*pv(10)
 v(122) = pv(3)*pv(10)
 v(123) = pv(4)*pv(10)
 v(124) = pv(5)*pv(10)
 v(125) = pv(6)*pv(10)
 v(126) = pv(7)*pv(10)
 v(127) = pv(8)*pv(10)
 v(128) = pv(9)*pv(10)
 v(129) = pv(10)*pv(10)
 v(130) = pv(0)*pv(11)
 v(131) = pv(0)*pv(12)
 v(132) = pv(0)*pv(13)
 v(133) = pv(0)*pv(14)
 v(134) = pv(0)*pv(15)
 v(135) = pv(0)*pv(16)
 v(136) = pv(0)*pv(17)
 v(137) = pv(0)*pv(18)
 v(138) = pv(0)*pv(19)
 v(139) = pv(0)*pv(20)
 v(140) = pv(0)*pv(21)
 v(141) = pv(0)*pv(22)
 v(142) = pv(0)*pv(23)
 v(143) = pv(0)*pv(24)
 v(144) = pv(0)*pv(25)
 v(145) = pv(0)*pv(26)
 v(146) = pv(0)*pv(27)
 v(147) = pv(0)*pv(28)
 v(148) = pv(0)*pv(29)
 v(149) = pv(0)*pv(30)
 v(150) = pv(0)*pv(31)
 v(151) = pv(0)*pv(32)
 v(152) = pv(0)*pv(33)
 v(153:183) = pv(71:101)
endif
if (7.le.mxd) then
 v(184) = pv(9)*pv(11)
 v(185) = pv(10)*pv(11)
 v(186) = pv(10)*pv(12)
 v(187) = pv(1)*pv(13)
 v(188) = pv(1)*pv(14)
 v(189) = pv(7)*pv(14)
 v(190) = pv(9)*pv(14)
 v(191) = pv(10)*pv(14)
 v(192) = pv(1)*pv(15)
 v(193) = pv(3)*pv(15)
 v(194) = pv(1)*pv(16)
 v(195) = pv(2)*pv(16)
 v(196) = pv(3)*pv(16)
 v(197) = pv(1)*pv(17)
 v(198) = pv(1)*pv(18)
 v(199) = pv(2)*pv(18)
 v(200) = pv(3)*pv(18)
 v(201) = pv(9)*pv(18)
 v(202) = pv(10)*pv(18)
 v(203) = pv(1)*pv(19)
 v(204) = pv(2)*pv(19)
 v(205) = pv(3)*pv(19)
 v(206) = pv(4)*pv(19)
 v(207) = pv(6)*pv(19)
 v(208) = pv(7)*pv(19)
 v(209) = pv(8)*pv(19)
 v(210) = pv(9)*pv(19)
 v(211) = pv(10)*pv(19)
 v(212) = pv(1)*pv(20)
 v(213) = pv(2)*pv(20)
 v(214) = pv(3)*pv(20)
 v(215) = pv(4)*pv(20)
 v(216) = pv(7)*pv(20)
 v(217) = pv(8)*pv(20)
 v(218) = pv(9)*pv(20)
 v(219) = pv(10)*pv(20)
 v(220) = pv(1)*pv(21)
 v(221) = pv(2)*pv(21)
 v(222) = pv(3)*pv(21)
 v(223) = pv(4)*pv(21)
 v(224) = pv(5)*pv(21)
 v(225) = pv(6)*pv(21)
 v(226) = pv(7)*pv(21)
 v(227) = pv(8)*pv(21)
 v(228) = pv(9)*pv(21)
 v(229) = pv(10)*pv(21)
 v(230) = pv(1)*pv(22)
 v(231) = pv(2)*pv(22)
 v(232) = pv(3)*pv(22)
 v(233) = pv(6)*pv(22)
 v(234) = pv(7)*pv(22)
 v(235) = pv(9)*pv(22)
 v(236) = pv(10)*pv(22)
 v(237) = pv(1)*pv(23)
 v(238) = pv(2)*pv(23)
 v(239) = pv(3)*pv(23)
 v(240) = pv(6)*pv(23)
 v(241) = pv(7)*pv(23)
 v(242) = pv(9)*pv(23)
 v(243) = pv(10)*pv(23)
 v(244) = pv(1)*pv(24)
 v(245) = pv(2)*pv(24)
 v(246) = pv(3)*pv(24)
 v(247) = pv(4)*pv(24)
 v(248) = pv(6)*pv(24)
 v(249) = pv(7)*pv(24)
 v(250) = pv(8)*pv(24)
 v(251) = pv(9)*pv(24)
 v(252) = pv(10)*pv(24)
 v(253) = pv(1)*pv(25)
 v(254) = pv(2)*pv(25)
 v(255) = pv(3)*pv(25)
 v(256) = pv(7)*pv(25)
 v(257) = pv(9)*pv(25)
 v(258) = pv(10)*pv(25)
 v(259) = pv(1)*pv(26)
 v(260) = pv(2)*pv(26)
 v(261) = pv(3)*pv(26)
 v(262) = pv(4)*pv(26)
 v(263) = pv(5)*pv(26)
 v(264) = pv(6)*pv(26)
 v(265) = pv(7)*pv(26)
 v(266) = pv(8)*pv(26)
 v(267) = pv(9)*pv(26)
 v(268) = pv(10)*pv(26)
 v(269) = pv(1)*pv(27)
 v(270) = pv(2)*pv(27)
 v(271) = pv(3)*pv(27)
 v(272) = pv(4)*pv(27)
 v(273) = pv(5)*pv(27)
 v(274) = pv(6)*pv(27)
 v(275) = pv(7)*pv(27)
 v(276) = pv(8)*pv(27)
 v(277) = pv(9)*pv(27)
 v(278) = pv(10)*pv(27)
 v(279) = pv(1)*pv(28)
 v(280) = pv(2)*pv(28)
 v(281) = pv(3)*pv(28)
 v(282) = pv(4)*pv(28)
 v(283) = pv(5)*pv(28)
 v(284) = pv(6)*pv(28)
 v(285) = pv(7)*pv(28)
 v(286) = pv(8)*pv(28)
 v(287) = pv(9)*pv(28)
 v(288) = pv(10)*pv(28)
 v(289) = pv(1)*pv(29)
 v(290) = pv(2)*pv(29)
 v(291) = pv(3)*pv(29)
 v(292) = pv(4)*pv(29)
 v(293) = pv(5)*pv(29)
 v(294) = pv(6)*pv(29)
 v(295) = pv(7)*pv(29)
 v(296) = pv(8)*pv(29)
 v(297) = pv(9)*pv(29)
 v(298) = pv(10)*pv(29)
 v(299) = pv(1)*pv(30)
 v(300) = pv(2)*pv(30)
 v(301) = pv(3)*pv(30)
 v(302) = pv(6)*pv(30)
 v(303) = pv(7)*pv(30)
 v(304) = pv(8)*pv(30)
 v(305) = pv(9)*pv(30)
 v(306) = pv(10)*pv(30)
 v(307) = pv(1)*pv(31)
 v(308) = pv(2)*pv(31)
 v(309) = pv(3)*pv(31)
 v(310) = pv(4)*pv(31)
 v(311) = pv(5)*pv(31)
 v(312) = pv(6)*pv(31)
 v(313) = pv(7)*pv(31)
 v(314) = pv(8)*pv(31)
 v(315) = pv(9)*pv(31)
 v(316) = pv(10)*pv(31)
 v(317) = pv(1)*pv(32)
 v(318) = pv(2)*pv(32)
 v(319) = pv(3)*pv(32)
 v(320) = pv(4)*pv(32)
 v(321) = pv(5)*pv(32)
 v(322) = pv(6)*pv(32)
 v(323) = pv(7)*pv(32)
 v(324) = pv(8)*pv(32)
 v(325) = pv(9)*pv(32)
 v(326) = pv(10)*pv(32)
 v(327) = pv(1)*pv(33)
 v(328) = pv(2)*pv(33)
 v(329) = pv(3)*pv(33)
 v(330) = pv(4)*pv(33)
 v(331) = pv(5)*pv(33)
 v(332) = pv(6)*pv(33)
 v(333) = pv(7)*pv(33)
 v(334) = pv(8)*pv(33)
 v(335) = pv(9)*pv(33)
 v(336) = pv(10)*pv(33)
 v(337) = pv(0)*pv(34)
 v(338) = pv(0)*pv(35)
 v(339) = pv(0)*pv(36)
 v(340) = pv(0)*pv(37)
 v(341) = pv(0)*pv(38)
 v(342) = pv(0)*pv(39)
 v(343) = pv(0)*pv(40)
 v(344) = pv(0)*pv(41)
 v(345) = pv(0)*pv(42)
 v(346) = pv(0)*pv(43)
 v(347) = pv(0)*pv(44)
 v(348) = pv(0)*pv(45)
 v(349) = pv(0)*pv(46)
 v(350) = pv(0)*pv(47)
 v(351) = pv(0)*pv(48)
 v(352) = pv(0)*pv(49)
 v(353) = pv(0)*pv(50)
 v(354) = pv(0)*pv(51)
 v(355) = pv(0)*pv(52)
 v(356) = pv(0)*pv(53)
 v(357) = pv(0)*pv(54)
 v(358) = pv(0)*pv(55)
 v(359) = pv(0)*pv(56)
 v(360) = pv(0)*pv(57)
 v(361) = pv(0)*pv(58)
 v(362) = pv(0)*pv(59)
 v(363) = pv(0)*pv(60)
 v(364) = pv(0)*pv(61)
 v(365) = pv(0)*pv(62)
 v(366) = pv(0)*pv(63)
 v(367) = pv(0)*pv(64)
 v(368) = pv(0)*pv(65)
 v(369) = pv(0)*pv(66)
 v(370) = pv(0)*pv(67)
 v(371) = pv(0)*pv(68)
 v(372) = pv(0)*pv(69)
 v(373) = pv(0)*pv(70)
 v(374:378) = pv(102:106)
endif
if (8.le.mxd) then
 stop 'mg42: degree 8 not implemented'
endif
return
contains
subroutine mg42_setd ()
 integer :: i, j, k, l0, l1
 integer, parameter, dimension(0:2) :: lb=(/ 0, m, m+n /)
 k = 0
 do l1 = 0, 1
  do l0 = 0, l1
   do j = lb(l1), lb(l1+1)-1
    do i = lb(l0), min(j-1,lb(l0+1)-1)
     d(i,j) = x(k)
     d(j,i) = d(i,j)
     d2(i,j) = x(k)**2
     d2(j,i) = d2(i,j)
     d3(i,j) = x(k)**3
     d3(j,i) = d3(i,j)
     d4(i,j) = x(k)**4
     d4(j,i) = d4(i,j)
     d5(i,j) = x(k)**5
     d5(j,i) = d5(i,j)
     d6(i,j) = x(k)**6
     d6(j,i) = d6(i,j)
     d7(i,j) = x(k)**7
     d7(j,i) = d7(i,j)
     k = k+1
    enddo
   enddo
  enddo
 enddo
 if (k.ne.mg42_nr) then
  stop 'mg42_setd: bad count'
 endif
 do i = 0, nk-1
  d(i,i) = 0
  d2(i,i) = 0
  d3(i,i) = 0
  d4(i,i) = 0
  d5(i,i) = 0
  d6(i,i) = 0
  d7(i,i) = 0
 enddo
end subroutine mg42_setd
subroutine mg42_deg2_i1j0 ()
 pv(0) = pv(0)+d(i0,i1)*d(i0,j0)/m2n
end subroutine mg42_deg2_i1j0
subroutine mg42_deg3_i2 ()
 pv(1) = pv(1)+d2(i0,i1)*d(i0,i2)/m3
end subroutine mg42_deg3_i2
subroutine mg42_deg3_i0j0 ()
 pv(5) = pv(5)+d3(i0,j0)/mn
end subroutine mg42_deg3_i0j0
subroutine mg42_deg3_i1j0 ()
 pv(2) = pv(2)+d2(i0,i1)*d(i0,j0)/m2n
 pv(4) = pv(4)+d(i0,i1)*d2(i0,j0)/m2n
 pv(6) = pv(6)+d(i0,i1)*d(i0,j0)*d(i1,j0)/m2n
 pv(8) = pv(8)+d2(i0,j0)*d(i1,j0)/m2n
end subroutine mg42_deg3_i1j0
subroutine mg42_deg3_i1j1 ()
 pv(9) = pv(9)+d(i0,i1)*d(i0,j0)*d(i0,j1)/m2n2
 pv(10) = pv(10)+d(i0,i1)*d(i1,j0)*d(i0,j1)/m2n2
end subroutine mg42_deg3_i1j1
subroutine mg42_deg3_i2j0 ()
 pv(3) = pv(3)+d(i0,i1)*d(i0,i2)*d(i0,j0)/m3n
 pv(7) = pv(7)+d(i0,i2)*d(i0,j0)*d(i1,j0)/m3n
end subroutine mg42_deg3_i2j0
subroutine mg42_deg4_i1 ()
 pv(11) = pv(11)+d4(i0,i1)/m2
end subroutine mg42_deg4_i1
subroutine mg42_deg4_i1j0 ()
 pv(12) = pv(12)+d3(i0,i1)*d(i0,j0)/m2n
 pv(15) = pv(15)+d2(i0,i1)*d2(i0,j0)/m2n
 pv(17) = pv(17)+d(i0,i1)*d3(i0,j0)/m2n
 pv(18) = pv(18)+d2(i0,i1)*d(i0,j0)*d(i1,j0)/m2n
 pv(22) = pv(22)+d(i0,i1)*d2(i0,j0)*d(i1,j0)/m2n
 pv(25) = pv(25)+d3(i0,j0)*d(i1,j0)/m2n
 pv(26) = pv(26)+d2(i0,j0)*d2(i1,j0)/m2n
end subroutine mg42_deg4_i1j0
subroutine mg42_deg4_i2j0 ()
 pv(13) = pv(13)+d2(i0,i1)*d(i0,i2)*d(i0,j0)/m3n
 pv(14) = pv(14)+d2(i0,i1)*d(i1,i2)*d(i0,j0)/m3n
 pv(16) = pv(16)+d(i0,i1)*d(i0,i2)*d2(i0,j0)/m3n
 pv(19) = pv(19)+d(i0,i1)*d(i0,i2)*d(i0,j0)*d(i1,j0)/m3n
 pv(20) = pv(20)+d2(i0,i2)*d(i0,j0)*d(i1,j0)/m3n
 pv(21) = pv(21)+d(i0,i2)*d(i1,i2)*d(i0,j0)*d(i1,j0)/m3n
 pv(23) = pv(23)+d(i0,i2)*d2(i0,j0)*d(i1,j0)/m3n
 pv(24) = pv(24)+d(i1,i2)*d2(i0,j0)*d(i1,j0)/m3n
end subroutine mg42_deg4_i2j0
subroutine mg42_deg4_i0j1 ()
 pv(30) = pv(30)+d3(i0,j0)*d(i0,j1)/mn2
end subroutine mg42_deg4_i0j1
subroutine mg42_deg4_i1j1 ()
 pv(27) = pv(27)+d2(i0,i1)*d(i0,j0)*d(i0,j1)/m2n2
 pv(29) = pv(29)+d(i0,i1)*d2(i0,j0)*d(i0,j1)/m2n2
 pv(31) = pv(31)+d2(i0,i1)*d(i1,j0)*d(i0,j1)/m2n2
 pv(32) = pv(32)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,j1)/m2n2
 pv(33) = pv(33)+d2(i0,j0)*d(i1,j0)*d(i0,j1)/m2n2
end subroutine mg42_deg4_i1j1
subroutine mg42_deg4_i2j1 ()
 pv(28) = pv(28)+d(i0,i1)*d(i0,i2)*d(i0,j0)*d(i0,j1)/m3n2
end subroutine mg42_deg4_i2j1
subroutine mg42_deg5_i1 ()
 pv(34) = pv(34)+d5(i0,i1)/m2
end subroutine mg42_deg5_i1
subroutine mg42_deg5_i0j0 ()
 pv(43) = pv(43)+d5(i0,j0)/mn
end subroutine mg42_deg5_i0j0
subroutine mg42_deg5_i1j0 ()
 pv(35) = pv(35)+d4(i0,i1)*d(i0,j0)/m2n
 pv(37) = pv(37)+d3(i0,i1)*d2(i0,j0)/m2n
 pv(40) = pv(40)+d2(i0,i1)*d3(i0,j0)/m2n
 pv(42) = pv(42)+d(i0,i1)*d4(i0,j0)/m2n
 pv(44) = pv(44)+d3(i0,i1)*d(i0,j0)*d(i1,j0)/m2n
 pv(48) = pv(48)+d2(i0,i1)*d2(i0,j0)*d(i1,j0)/m2n
 pv(54) = pv(54)+d(i0,i1)*d3(i0,j0)*d(i1,j0)/m2n
 pv(57) = pv(57)+d4(i0,j0)*d(i1,j0)/m2n
 pv(58) = pv(58)+d(i0,i1)*d2(i0,j0)*d2(i1,j0)/m2n
end subroutine mg42_deg5_i1j0
subroutine mg42_deg5_i2j0 ()
 pv(36) = pv(36)+d3(i0,i1)*d(i0,i2)*d(i0,j0)/m3n
 pv(38) = pv(38)+d2(i0,i1)*d(i0,i2)*d2(i0,j0)/m3n
 pv(39) = pv(39)+d2(i0,i1)*d(i1,i2)*d2(i0,j0)/m3n
 pv(41) = pv(41)+d(i0,i1)*d(i0,i2)*d3(i0,j0)/m3n
 pv(45) = pv(45)+d2(i0,i1)*d(i0,i2)*d(i0,j0)*d(i1,j0)/m3n
 pv(46) = pv(46)+d(i0,i1)*d2(i0,i2)*d(i0,j0)*d(i1,j0)/m3n
 pv(47) = pv(47)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,j0)*d(i1,j0)/m3n
 pv(49) = pv(49)+d(i0,i1)*d(i0,i2)*d2(i0,j0)*d(i1,j0)/m3n
 pv(50) = pv(50)+d2(i0,i2)*d2(i0,j0)*d(i1,j0)/m3n
 pv(51) = pv(51)+d(i0,i1)*d(i1,i2)*d2(i0,j0)*d(i1,j0)/m3n
 pv(52) = pv(52)+d(i0,i2)*d(i1,i2)*d2(i0,j0)*d(i1,j0)/m3n
 pv(53) = pv(53)+d2(i1,i2)*d2(i0,j0)*d(i1,j0)/m3n
 pv(55) = pv(55)+d(i0,i2)*d3(i0,j0)*d(i1,j0)/m3n
 pv(56) = pv(56)+d(i1,i2)*d3(i0,j0)*d(i1,j0)/m3n
end subroutine mg42_deg5_i2j0
subroutine mg42_deg5_i0j1 ()
 pv(63) = pv(63)+d4(i0,j0)*d(i0,j1)/mn2
end subroutine mg42_deg5_i0j1
subroutine mg42_deg5_i1j1 ()
 pv(59) = pv(59)+d3(i0,i1)*d(i0,j0)*d(i0,j1)/m2n2
 pv(61) = pv(61)+d2(i0,i1)*d2(i0,j0)*d(i0,j1)/m2n2
 pv(62) = pv(62)+d(i0,i1)*d3(i0,j0)*d(i0,j1)/m2n2
 pv(64) = pv(64)+d2(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,j1)/m2n2
 pv(66) = pv(66)+d(i0,i1)*d2(i0,j0)*d(i1,j0)*d(i0,j1)/m2n2
 pv(68) = pv(68)+d3(i0,j0)*d(i1,j0)*d(i0,j1)/m2n2
 pv(69) = pv(69)+d(i0,i1)*d(i0,j0)*d2(i1,j0)*d(i0,j1)/m2n2
 pv(70) = pv(70)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d2(i0,j1)/m2n2
end subroutine mg42_deg5_i1j1
subroutine mg42_deg5_i2j1 ()
 pv(60) = pv(60)+d2(i0,i1)*d(i0,i2)*d(i0,j0)*d(i0,j1)/m3n2
 pv(65) = pv(65)+d(i0,i1)*d(i0,i2)*d(i0,j0)*d(i1,j0)*d(i0,j1)/m3n2
 pv(67) = pv(67)+d(i0,i2)*d2(i0,j0)*d(i1,j0)*d(i0,j1)/m3n2
end subroutine mg42_deg5_i2j1
subroutine mg42_deg6_i1j0 ()
 pv(71) = pv(71)+d4(i0,i1)*d2(i0,j0)/m2n
 pv(73) = pv(73)+d3(i0,i1)*d3(i0,j0)/m2n
 pv(75) = pv(75)+d2(i0,i1)*d4(i0,j0)/m2n
 pv(77) = pv(77)+d(i0,i1)*d5(i0,j0)/m2n
 pv(78) = pv(78)+d4(i0,i1)*d(i0,j0)*d(i1,j0)/m2n
 pv(81) = pv(81)+d3(i0,i1)*d2(i0,j0)*d(i1,j0)/m2n
 pv(86) = pv(86)+d2(i0,i1)*d3(i0,j0)*d(i1,j0)/m2n
 pv(90) = pv(90)+d(i0,i1)*d4(i0,j0)*d(i1,j0)/m2n
 pv(92) = pv(92)+d(i0,i1)*d3(i0,j0)*d2(i1,j0)/m2n
 pv(94) = pv(94)+d4(i0,j0)*d2(i1,j0)/m2n
end subroutine mg42_deg6_i1j0
subroutine mg42_deg6_i2j0 ()
 pv(72) = pv(72)+d3(i0,i1)*d(i0,i2)*d2(i0,j0)/m3n
 pv(74) = pv(74)+d2(i0,i1)*d(i0,i2)*d3(i0,j0)/m3n
 pv(76) = pv(76)+d(i0,i1)*d(i0,i2)*d4(i0,j0)/m3n
 pv(79) = pv(79)+d3(i0,i1)*d(i0,i2)*d(i0,j0)*d(i1,j0)/m3n
 pv(80) = pv(80)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,j0)*d(i1,j0)/m3n
 pv(82) = pv(82)+d2(i0,i1)*d(i0,i2)*d2(i0,j0)*d(i1,j0)/m3n
 pv(83) = pv(83)+d(i0,i1)*d2(i0,i2)*d2(i0,j0)*d(i1,j0)/m3n
 pv(84) = pv(84)+d2(i0,i1)*d(i1,i2)*d2(i0,j0)*d(i1,j0)/m3n
 pv(85) = pv(85)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d2(i0,j0)*d(i1,j0)/m3n
 pv(87) = pv(87)+d(i0,i1)*d(i0,i2)*d3(i0,j0)*d(i1,j0)/m3n
 pv(88) = pv(88)+d2(i0,i2)*d3(i0,j0)*d(i1,j0)/m3n
 pv(89) = pv(89)+d(i0,i1)*d(i1,i2)*d3(i0,j0)*d(i1,j0)/m3n
 pv(91) = pv(91)+d(i1,i2)*d4(i0,j0)*d(i1,j0)/m3n
 pv(93) = pv(93)+d(i0,i2)*d3(i0,j0)*d2(i1,j0)/m3n
end subroutine mg42_deg6_i2j0
subroutine mg42_deg6_i0j1 ()
 pv(96) = pv(96)+d5(i0,j0)*d(i0,j1)/mn2
end subroutine mg42_deg6_i0j1
subroutine mg42_deg6_i1j1 ()
 pv(95) = pv(95)+d2(i0,i1)*d3(i0,j0)*d(i0,j1)/m2n2
 pv(98) = pv(98)+d2(i0,i1)*d2(i0,j0)*d(i1,j0)*d(i0,j1)/m2n2
 pv(100) = pv(100)+d(i0,i1)*d3(i0,j0)*d(i1,j0)*d(i0,j1)/m2n2
 pv(101) = pv(101)+d(i0,i1)*d2(i0,j0)*d2(i1,j0)*d(i0,j1)/m2n2
end subroutine mg42_deg6_i1j1
subroutine mg42_deg6_i2j1 ()
 pv(97) = pv(97)+d2(i0,i1)*d(i0,i2)*d(i0,j0)*d(i1,j0)*d(i0,j1)/m3n2
 pv(99) = pv(99)+d(i0,i1)*d(i0,i2)*d2(i0,j0)*d(i1,j0)*d(i0,j1)/m3n2
end subroutine mg42_deg6_i2j1
subroutine mg42_deg7_i0j0 ()
 pv(103) = pv(103)+d7(i0,j0)/mn
end subroutine mg42_deg7_i0j0
subroutine mg42_deg7_i1j0 ()
 pv(102) = pv(102)+d(i0,i1)*d6(i0,j0)/m2n
 pv(105) = pv(105)+d2(i0,i1)*d4(i0,j0)*d(i1,j0)/m2n
end subroutine mg42_deg7_i1j0
subroutine mg42_deg7_i2j0 ()
 pv(104) = pv(104)+d2(i0,i1)*d2(i0,i2)*d2(i0,j0)*d(i1,j0)/m3n
end subroutine mg42_deg7_i2j0
subroutine mg42_deg7_i2j1 ()
 pv(106) = pv(106)+d2(i0,i1)*d2(i0,i2)*d(i0,j0)*d(i1,j0)*d(i0,j1)/m3n2
end subroutine mg42_deg7_i2j1
end subroutine mg42_secs
