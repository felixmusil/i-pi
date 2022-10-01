SUBROUTINE mg6211_isecs (mxd, r, pv)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: pv(0:)
!-----------------------------------------------------------------------
integer, parameter :: m=6, n=2, m2=m*(m-1), m3=m2*(m-2), m4=m3*(m-3), &
  mm=m4*(m-4), nn=n
integer :: i0, i1, i2, i3, i4, i5, j0, j1, k0, l0
real (kind=wp), dimension (0:nk-1,0:nk-1) :: d, d2, d3, d4, d5
if (size(r).ne.nk*nk) then
 stop 'mg6211_isecs: bad dimensions'
endif
call mgx_setd (r, d, d2, d3, d4, d5)
pv = 0
k0 = m+n ; l0 = k0+1
do i0 = 0, m-1
 call mg6211_i0 (mxd, m)
 do i1 = 0, m-1
  if (i1.ne.i0) then
   call mg6211_i1 (mxd, m2)
   do i2 = 0, m-1
    if (i2.ne.i0.and.i2.ne.i1) then
     call mg6211_i2 (mxd, m3)
     do i3 = 0, m-1
      if (i3.ne.i0.and.i3.ne.i1.and.i3.ne.i2) then
       call mg6211_i3 (mxd, m4)
       do i4 = 0, m-1
        if (i4.ne.i0.and.i4.ne.i1.and.i4.ne.i2.and.i4.ne.i3) then
         do i5 = 0, m-1
          if (i5.ne.i0.and.i5.ne.i1.and.i5.ne.i2.and.i5.ne.i3.and. &
            i5.ne.i4) then
           call mg6211_ii (mxd, mm)
          endif
         enddo
        endif
       enddo
      endif
     enddo
    endif
   enddo
  endif
 enddo
enddo
do j0 = m, m+n-1
 do j1 = m, m+n-1
  if (j1.ne.j0) then
   call mg6211_jj (mxd, nn)
   do i0 = 0, m-1
    call mg6211_i0jj (mxd, m*nn)
    do i1 = 0, m-1
     if (i1.ne.i0) then
      call mg6211_i1jj (mxd, m2*nn)
      do i2 = 0, m-1
       if (i2.ne.i0.and.i2.ne.i1) then
        call mg6211_i2jj (mxd, m3*nn)
        do i3 = 0, m-1
         if (i3.ne.i0.and.i3.ne.i1.and.i3.ne.i2) then
          call mg6211_i3jj (mxd, m4*nn)
          do i4 = 0, m-1
           if (i4.ne.i0.and.i4.ne.i1.and.i4.ne.i2.and.i4.ne.i3) then
            do i5 = 0, m-1
             if (i5.ne.i0.and.i5.ne.i1.and.i5.ne.i2.and.i5.ne.i3.and. &
               i5.ne.i4) then
              call mg6211_iijj (mxd, mm*nn)
             endif
            enddo
           endif
          enddo
         endif
        enddo
       endif
      enddo
     endif
    enddo
   enddo
  endif
 enddo
enddo
return
CONTAINS
SUBROUTINE mg6211_i0 (mxd, mu)
integer, intent (in) :: mxd, mu
if (2.le.mxd) then
 pv(6) = pv(6)+d(i0,k0)*d(i0,l0)/mu
endif
if (3.le.mxd) then
 pv(43) = pv(43)+d2(i0,k0)*d(i0,l0)/mu
 pv(48) = pv(48)+d(i0,k0)*d2(i0,l0)/mu
endif
if (4.le.mxd) then
 pv(190) = pv(190)+d3(i0,k0)*d(i0,l0)/mu
 pv(214) = pv(214)+d2(i0,k0)*d2(i0,l0)/mu
 pv(219) = pv(219)+d(i0,k0)*d3(i0,l0)/mu
endif
if (5.le.mxd) then
 pv(751) = pv(751)+d4(i0,k0)*d(i0,l0)/mu
 pv(866) = pv(866)+d3(i0,k0)*d2(i0,l0)/mu
 pv(890) = pv(890)+d2(i0,k0)*d3(i0,l0)/mu
 pv(895) = pv(895)+d(i0,k0)*d4(i0,l0)/mu
endif
END SUBROUTINE mg6211_i0
SUBROUTINE mg6211_i1 (mxd, mu)
integer, intent (in) :: mxd, mu
if (2.le.mxd) then
 pv(1) = pv(1)+d(i0,i1)*d(i0,k0)/mu
 pv(4) = pv(4)+d(i0,i1)*d(i0,l0)/mu
endif
if (3.le.mxd) then
 pv(19) = pv(19)+d2(i0,i1)*d(i0,k0)/mu
 pv(27) = pv(27)+d(i0,i1)*d2(i0,k0)/mu
 pv(29) = pv(29)+d(i0,i1)*d(i0,k0)*d(i1,k0)/mu
 pv(33) = pv(33)+d2(i0,i1)*d(i0,l0)/mu
 pv(41) = pv(41)+d(i0,i1)*d(i0,k0)*d(i0,l0)/mu
 pv(44) = pv(44)+d(i0,i1)*d(i1,k0)*d(i0,l0)/mu
 pv(46) = pv(46)+d(i0,i1)*d2(i0,l0)/mu
 pv(49) = pv(49)+d(i0,i1)*d(i0,l0)*d(i1,l0)/mu
endif
if (4.le.mxd) then
 pv(97) = pv(97)+d3(i0,i1)*d(i0,k0)/mu
 pv(125) = pv(125)+d2(i0,i1)*d2(i0,k0)/mu
 pv(133) = pv(133)+d(i0,i1)*d3(i0,k0)/mu
 pv(135) = pv(135)+d2(i0,i1)*d(i0,k0)*d(i1,k0)/mu
 pv(140) = pv(140)+d(i0,i1)*d2(i0,k0)*d(i1,k0)/mu
 pv(152) = pv(152)+d3(i0,i1)*d(i0,l0)/mu
 pv(180) = pv(180)+d2(i0,i1)*d(i0,k0)*d(i0,l0)/mu
 pv(188) = pv(188)+d(i0,i1)*d2(i0,k0)*d(i0,l0)/mu
 pv(191) = pv(191)+d2(i0,i1)*d(i1,k0)*d(i0,l0)/mu
 pv(198) = pv(198)+d(i0,i1)*d(i0,k0)*d(i1,k0)*d(i0,l0)/mu
 pv(199) = pv(199)+d(i0,i1)*d2(i1,k0)*d(i0,l0)/mu
 pv(204) = pv(204)+d2(i0,i1)*d2(i0,l0)/mu
 pv(212) = pv(212)+d(i0,i1)*d(i0,k0)*d2(i0,l0)/mu
 pv(215) = pv(215)+d(i0,i1)*d(i1,k0)*d2(i0,l0)/mu
 pv(217) = pv(217)+d(i0,i1)*d3(i0,l0)/mu
 pv(220) = pv(220)+d2(i0,i1)*d(i0,l0)*d(i1,l0)/mu
 pv(225) = pv(225)+d(i0,i1)*d(i0,k0)*d(i0,l0)*d(i1,l0)/mu
 pv(226) = pv(226)+d(i0,i1)*d2(i0,l0)*d(i1,l0)/mu
endif
if (5.le.mxd) then
 pv(386) = pv(386)+d4(i0,i1)*d(i0,k0)/mu
 pv(495) = pv(495)+d3(i0,i1)*d2(i0,k0)/mu
 pv(523) = pv(523)+d2(i0,i1)*d3(i0,k0)/mu
 pv(531) = pv(531)+d(i0,i1)*d4(i0,k0)/mu
 pv(533) = pv(533)+d3(i0,i1)*d(i0,k0)*d(i1,k0)/mu
 pv(555) = pv(555)+d2(i0,i1)*d2(i0,k0)*d(i1,k0)/mu
 pv(562) = pv(562)+d(i0,i1)*d3(i0,k0)*d(i1,k0)/mu
 pv(563) = pv(563)+d(i0,i1)*d2(i0,k0)*d2(i1,k0)/mu
 pv(604) = pv(604)+d4(i0,i1)*d(i0,l0)/mu
 pv(713) = pv(713)+d3(i0,i1)*d(i0,k0)*d(i0,l0)/mu
 pv(741) = pv(741)+d2(i0,i1)*d2(i0,k0)*d(i0,l0)/mu
 pv(749) = pv(749)+d(i0,i1)*d3(i0,k0)*d(i0,l0)/mu
 pv(752) = pv(752)+d3(i0,i1)*d(i1,k0)*d(i0,l0)/mu
 pv(791) = pv(791)+d2(i0,i1)*d(i0,k0)*d(i1,k0)*d(i0,l0)/mu
 pv(798) = pv(798)+d(i0,i1)*d2(i0,k0)*d(i1,k0)*d(i0,l0)/mu
 pv(799) = pv(799)+d2(i0,i1)*d2(i1,k0)*d(i0,l0)/mu
 pv(806) = pv(806)+d(i0,i1)*d(i0,k0)*d2(i1,k0)*d(i0,l0)/mu
 pv(807) = pv(807)+d(i0,i1)*d3(i1,k0)*d(i0,l0)/mu
 pv(828) = pv(828)+d3(i0,i1)*d2(i0,l0)/mu
 pv(856) = pv(856)+d2(i0,i1)*d(i0,k0)*d2(i0,l0)/mu
 pv(864) = pv(864)+d(i0,i1)*d2(i0,k0)*d2(i0,l0)/mu
 pv(867) = pv(867)+d2(i0,i1)*d(i1,k0)*d2(i0,l0)/mu
 pv(874) = pv(874)+d(i0,i1)*d(i0,k0)*d(i1,k0)*d2(i0,l0)/mu
 pv(875) = pv(875)+d(i0,i1)*d2(i1,k0)*d2(i0,l0)/mu
 pv(880) = pv(880)+d2(i0,i1)*d3(i0,l0)/mu
 pv(888) = pv(888)+d(i0,i1)*d(i0,k0)*d3(i0,l0)/mu
 pv(891) = pv(891)+d(i0,i1)*d(i1,k0)*d3(i0,l0)/mu
 pv(893) = pv(893)+d(i0,i1)*d4(i0,l0)/mu
 pv(896) = pv(896)+d3(i0,i1)*d(i0,l0)*d(i1,l0)/mu
 pv(918) = pv(918)+d2(i0,i1)*d(i0,k0)*d(i0,l0)*d(i1,l0)/mu
 pv(925) = pv(925)+d(i0,i1)*d2(i0,k0)*d(i0,l0)*d(i1,l0)/mu
 pv(926) = pv(926)+d(i0,i1)*d(i0,k0)*d(i1,k0)*d(i0,l0)*d(i1,l0)/mu
 pv(929) = pv(929)+d2(i0,i1)*d2(i0,l0)*d(i1,l0)/mu
 pv(936) = pv(936)+d(i0,i1)*d(i0,k0)*d2(i0,l0)*d(i1,l0)/mu
 pv(937) = pv(937)+d(i0,i1)*d(i1,k0)*d2(i0,l0)*d(i1,l0)/mu
 pv(938) = pv(938)+d(i0,i1)*d3(i0,l0)*d(i1,l0)/mu
 pv(939) = pv(939)+d(i0,i1)*d2(i0,l0)*d2(i1,l0)/mu
endif
END SUBROUTINE mg6211_i1
SUBROUTINE mg6211_i2 (mxd, mu)
integer, intent (in) :: mxd, mu
if (2.le.mxd) then
endif
if (3.le.mxd) then
 pv(20) = pv(20)+d(i0,i1)*d(i0,i2)*d(i0,k0)/mu
 pv(21) = pv(21)+d(i0,i1)*d(i1,i2)*d(i0,k0)/mu
 pv(34) = pv(34)+d(i0,i1)*d(i0,i2)*d(i0,l0)/mu
 pv(35) = pv(35)+d(i0,i1)*d(i1,i2)*d(i0,l0)/mu
endif
if (4.le.mxd) then
 pv(54) = pv(54)+d2(i0,i1)*d2(i0,i2)/mu
 pv(55) = pv(55)+d2(i0,i1)*d(i0,i2)*d(i1,i2)/mu
 pv(98) = pv(98)+d2(i0,i1)*d(i0,i2)*d(i0,k0)/mu
 pv(99) = pv(99)+d2(i0,i1)*d(i1,i2)*d(i0,k0)/mu
 pv(100) = pv(100)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,k0)/mu
 pv(101) = pv(101)+d(i0,i1)*d2(i1,i2)*d(i0,k0)/mu
 pv(126) = pv(126)+d(i0,i1)*d(i0,i2)*d2(i0,k0)/mu
 pv(127) = pv(127)+d(i0,i1)*d(i1,i2)*d2(i0,k0)/mu
 pv(136) = pv(136)+d(i0,i1)*d(i0,i2)*d(i0,k0)*d(i1,k0)/mu
 pv(137) = pv(137)+d(i0,i2)*d(i1,i2)*d(i0,k0)*d(i1,k0)/mu
 pv(153) = pv(153)+d2(i0,i1)*d(i0,i2)*d(i0,l0)/mu
 pv(154) = pv(154)+d2(i0,i1)*d(i1,i2)*d(i0,l0)/mu
 pv(155) = pv(155)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,l0)/mu
 pv(156) = pv(156)+d(i0,i1)*d2(i1,i2)*d(i0,l0)/mu
 pv(181) = pv(181)+d(i0,i1)*d(i0,i2)*d(i0,k0)*d(i0,l0)/mu
 pv(182) = pv(182)+d(i0,i1)*d(i1,i2)*d(i0,k0)*d(i0,l0)/mu
 pv(192) = pv(192)+d(i0,i1)*d(i0,i2)*d(i1,k0)*d(i0,l0)/mu
 pv(193) = pv(193)+d(i0,i1)*d(i1,i2)*d(i1,k0)*d(i0,l0)/mu
 pv(194) = pv(194)+d(i0,i2)*d(i1,i2)*d(i1,k0)*d(i0,l0)/mu
 pv(205) = pv(205)+d(i0,i1)*d(i0,i2)*d2(i0,l0)/mu
 pv(206) = pv(206)+d(i0,i1)*d(i1,i2)*d2(i0,l0)/mu
 pv(221) = pv(221)+d(i0,i1)*d(i0,i2)*d(i0,l0)*d(i1,l0)/mu
 pv(222) = pv(222)+d(i0,i2)*d(i1,i2)*d(i0,l0)*d(i1,l0)/mu
endif
if (5.le.mxd) then
 pv(243) = pv(243)+d3(i0,i1)*d2(i0,i2)/mu
 pv(244) = pv(244)+d3(i0,i1)*d(i0,i2)*d(i1,i2)/mu
 pv(245) = pv(245)+d2(i0,i1)*d2(i0,i2)*d(i1,i2)/mu
 pv(387) = pv(387)+d3(i0,i1)*d(i0,i2)*d(i0,k0)/mu
 pv(388) = pv(388)+d2(i0,i1)*d2(i0,i2)*d(i0,k0)/mu
 pv(389) = pv(389)+d3(i0,i1)*d(i1,i2)*d(i0,k0)/mu
 pv(390) = pv(390)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,k0)/mu
 pv(391) = pv(391)+d2(i0,i1)*d2(i1,i2)*d(i0,k0)/mu
 pv(392) = pv(392)+d(i0,i1)*d(i0,i2)*d2(i1,i2)*d(i0,k0)/mu
 pv(393) = pv(393)+d(i0,i1)*d3(i1,i2)*d(i0,k0)/mu
 pv(496) = pv(496)+d2(i0,i1)*d(i0,i2)*d2(i0,k0)/mu
 pv(497) = pv(497)+d2(i0,i1)*d(i1,i2)*d2(i0,k0)/mu
 pv(498) = pv(498)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d2(i0,k0)/mu
 pv(499) = pv(499)+d(i0,i1)*d2(i1,i2)*d2(i0,k0)/mu
 pv(524) = pv(524)+d(i0,i1)*d(i0,i2)*d3(i0,k0)/mu
 pv(525) = pv(525)+d(i0,i1)*d(i1,i2)*d3(i0,k0)/mu
 pv(534) = pv(534)+d2(i0,i1)*d(i0,i2)*d(i0,k0)*d(i1,k0)/mu
 pv(535) = pv(535)+d(i0,i1)*d2(i0,i2)*d(i0,k0)*d(i1,k0)/mu
 pv(536) = pv(536)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,k0)*d(i1,k0)/mu
 pv(537) = pv(537)+d2(i0,i2)*d(i1,i2)*d(i0,k0)*d(i1,k0)/mu
 pv(556) = pv(556)+d(i0,i1)*d(i0,i2)*d2(i0,k0)*d(i1,k0)/mu
 pv(557) = pv(557)+d(i0,i1)*d(i1,i2)*d2(i0,k0)*d(i1,k0)/mu
 pv(558) = pv(558)+d(i0,i2)*d(i1,i2)*d2(i0,k0)*d(i1,k0)/mu
 pv(605) = pv(605)+d3(i0,i1)*d(i0,i2)*d(i0,l0)/mu
 pv(606) = pv(606)+d2(i0,i1)*d2(i0,i2)*d(i0,l0)/mu
 pv(607) = pv(607)+d3(i0,i1)*d(i1,i2)*d(i0,l0)/mu
 pv(608) = pv(608)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,l0)/mu
 pv(609) = pv(609)+d2(i0,i1)*d2(i1,i2)*d(i0,l0)/mu
 pv(610) = pv(610)+d(i0,i1)*d(i0,i2)*d2(i1,i2)*d(i0,l0)/mu
 pv(611) = pv(611)+d(i0,i1)*d3(i1,i2)*d(i0,l0)/mu
 pv(714) = pv(714)+d2(i0,i1)*d(i0,i2)*d(i0,k0)*d(i0,l0)/mu
 pv(715) = pv(715)+d2(i0,i1)*d(i1,i2)*d(i0,k0)*d(i0,l0)/mu
 pv(716) = pv(716)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,k0)*d(i0,l0)/mu
 pv(717) = pv(717)+d(i0,i1)*d2(i1,i2)*d(i0,k0)*d(i0,l0)/mu
 pv(742) = pv(742)+d(i0,i1)*d(i0,i2)*d2(i0,k0)*d(i0,l0)/mu
 pv(743) = pv(743)+d(i0,i1)*d(i1,i2)*d2(i0,k0)*d(i0,l0)/mu
 pv(753) = pv(753)+d2(i0,i1)*d(i0,i2)*d(i1,k0)*d(i0,l0)/mu
 pv(754) = pv(754)+d(i0,i1)*d2(i0,i2)*d(i1,k0)*d(i0,l0)/mu
 pv(755) = pv(755)+d2(i0,i1)*d(i1,i2)*d(i1,k0)*d(i0,l0)/mu
 pv(756) = pv(756)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i1,k0)*d(i0,l0)/mu
 pv(757) = pv(757)+d2(i0,i2)*d(i1,i2)*d(i1,k0)*d(i0,l0)/mu
 pv(758) = pv(758)+d(i0,i1)*d2(i1,i2)*d(i1,k0)*d(i0,l0)/mu
 pv(759) = pv(759)+d(i0,i2)*d2(i1,i2)*d(i1,k0)*d(i0,l0)/mu
 pv(792) = pv(792)+d(i0,i1)*d(i0,i2)*d(i0,k0)*d(i1,k0)*d(i0,l0)/mu
 pv(793) = pv(793)+d(i0,i1)*d(i1,i2)*d(i0,k0)*d(i1,k0)*d(i0,l0)/mu
 pv(794) = pv(794)+d(i0,i2)*d(i1,i2)*d(i0,k0)*d(i1,k0)*d(i0,l0)/mu
 pv(800) = pv(800)+d(i0,i1)*d(i0,i2)*d2(i1,k0)*d(i0,l0)/mu
 pv(801) = pv(801)+d(i0,i1)*d(i1,i2)*d2(i1,k0)*d(i0,l0)/mu
 pv(802) = pv(802)+d(i0,i2)*d(i1,i2)*d2(i1,k0)*d(i0,l0)/mu
 pv(808) = pv(808)+d(i0,i1)*d(i0,i2)*d(i1,k0)*d(i2,k0)*d(i0,l0)/mu
 pv(829) = pv(829)+d2(i0,i1)*d(i0,i2)*d2(i0,l0)/mu
 pv(830) = pv(830)+d2(i0,i1)*d(i1,i2)*d2(i0,l0)/mu
 pv(831) = pv(831)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d2(i0,l0)/mu
 pv(832) = pv(832)+d(i0,i1)*d2(i1,i2)*d2(i0,l0)/mu
 pv(857) = pv(857)+d(i0,i1)*d(i0,i2)*d(i0,k0)*d2(i0,l0)/mu
 pv(858) = pv(858)+d(i0,i1)*d(i1,i2)*d(i0,k0)*d2(i0,l0)/mu
 pv(868) = pv(868)+d(i0,i1)*d(i0,i2)*d(i1,k0)*d2(i0,l0)/mu
 pv(869) = pv(869)+d(i0,i1)*d(i1,i2)*d(i1,k0)*d2(i0,l0)/mu
 pv(870) = pv(870)+d(i0,i2)*d(i1,i2)*d(i1,k0)*d2(i0,l0)/mu
 pv(881) = pv(881)+d(i0,i1)*d(i0,i2)*d3(i0,l0)/mu
 pv(882) = pv(882)+d(i0,i1)*d(i1,i2)*d3(i0,l0)/mu
 pv(897) = pv(897)+d2(i0,i1)*d(i0,i2)*d(i0,l0)*d(i1,l0)/mu
 pv(898) = pv(898)+d(i0,i1)*d2(i0,i2)*d(i0,l0)*d(i1,l0)/mu
 pv(899) = pv(899)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,l0)*d(i1,l0)/mu
 pv(900) = pv(900)+d2(i0,i2)*d(i1,i2)*d(i0,l0)*d(i1,l0)/mu
 pv(919) = pv(919)+d(i0,i1)*d(i0,i2)*d(i0,k0)*d(i0,l0)*d(i1,l0)/mu
 pv(920) = pv(920)+d(i0,i1)*d(i1,i2)*d(i0,k0)*d(i0,l0)*d(i1,l0)/mu
 pv(921) = pv(921)+d(i0,i2)*d(i1,i2)*d(i0,k0)*d(i0,l0)*d(i1,l0)/mu
 pv(927) = pv(927)+d(i0,i1)*d(i0,i2)*d(i2,k0)*d(i0,l0)*d(i1,l0)/mu
 pv(930) = pv(930)+d(i0,i1)*d(i0,i2)*d2(i0,l0)*d(i1,l0)/mu
 pv(931) = pv(931)+d(i0,i1)*d(i1,i2)*d2(i0,l0)*d(i1,l0)/mu
 pv(932) = pv(932)+d(i0,i2)*d(i1,i2)*d2(i0,l0)*d(i1,l0)/mu
endif
END SUBROUTINE mg6211_i2
SUBROUTINE mg6211_i3 (mxd, mu)
integer, intent (in) :: mxd, mu
if (3.le.mxd) then
 pv(9) = pv(9)+d(i0,i1)*d(i1,i2)*d(i0,i3)/mu
endif
if (4.le.mxd) then
 pv(56) = pv(56)+d2(i0,i1)*d(i0,i2)*d(i0,i3)/mu
 pv(57) = pv(57)+d2(i0,i1)*d(i1,i2)*d(i0,i3)/mu
 pv(58) = pv(58)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)/mu
 pv(59) = pv(59)+d(i0,i1)*d2(i1,i2)*d(i0,i3)/mu
 pv(60) = pv(60)+d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)/mu
 pv(102) = pv(102)+d(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,k0)/mu
 pv(103) = pv(103)+d(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,k0)/mu
 pv(104) = pv(104)+d(i0,i1)*d(i1,i2)*d(i1,i3)*d(i0,k0)/mu
 pv(157) = pv(157)+d(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,l0)/mu
 pv(158) = pv(158)+d(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,l0)/mu
 pv(159) = pv(159)+d(i0,i1)*d(i1,i2)*d(i1,i3)*d(i0,l0)/mu
endif
if (5.le.mxd) then
 pv(246) = pv(246)+d3(i0,i1)*d(i0,i2)*d(i0,i3)/mu
 pv(247) = pv(247)+d2(i0,i1)*d2(i0,i2)*d(i0,i3)/mu
 pv(248) = pv(248)+d3(i0,i1)*d(i1,i2)*d(i0,i3)/mu
 pv(249) = pv(249)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)/mu
 pv(250) = pv(250)+d2(i0,i1)*d2(i1,i2)*d(i0,i3)/mu
 pv(251) = pv(251)+d(i0,i1)*d(i0,i2)*d2(i1,i2)*d(i0,i3)/mu
 pv(252) = pv(252)+d(i0,i1)*d3(i1,i2)*d(i0,i3)/mu
 pv(253) = pv(253)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d2(i0,i3)/mu
 pv(254) = pv(254)+d(i0,i1)*d2(i1,i2)*d2(i0,i3)/mu
 pv(255) = pv(255)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)/mu
 pv(256) = pv(256)+d2(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)/mu
 pv(394) = pv(394)+d2(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,k0)/mu
 pv(395) = pv(395)+d2(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,k0)/mu
 pv(396) = pv(396)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i0,k0)/mu
 pv(397) = pv(397)+d(i0,i1)*d2(i1,i2)*d(i0,i3)*d(i0,k0)/mu
 pv(398) = pv(398)+d(i0,i1)*d(i1,i2)*d2(i0,i3)*d(i0,k0)/mu
 pv(399) = pv(399)+d2(i0,i1)*d(i1,i2)*d(i1,i3)*d(i0,k0)/mu
 pv(400) = pv(400)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,k0)/mu
 pv(401) = pv(401)+d2(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,k0)/mu
 pv(402) = pv(402)+d(i0,i1)*d2(i1,i2)*d(i1,i3)*d(i0,k0)/mu
 pv(403) = pv(403)+d(i0,i2)*d2(i1,i2)*d(i1,i3)*d(i0,k0)/mu
 pv(404) = pv(404)+d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,k0)/mu
 pv(405) = pv(405)+d(i0,i1)*d(i1,i2)*d(i1,i3)*d(i2,i3)*d(i0,k0)/mu
 pv(500) = pv(500)+d(i0,i1)*d(i0,i2)*d(i0,i3)*d2(i0,k0)/mu
 pv(501) = pv(501)+d(i0,i1)*d(i1,i2)*d(i0,i3)*d2(i0,k0)/mu
 pv(502) = pv(502)+d(i0,i1)*d(i1,i2)*d(i1,i3)*d2(i0,k0)/mu
 pv(538) = pv(538)+d(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,k0)*d(i1,k0)/mu
 pv(539) = pv(539)+d(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,k0)*d(i1,k0)/mu
 pv(540) = pv(540)+d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i0,k0)*d(i1,k0)/mu
 pv(541) = pv(541)+d(i0,i1)*d(i0,i2)*d(i2,i3)*d(i0,k0)*d(i1,k0)/mu
 pv(612) = pv(612)+d2(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,l0)/mu
 pv(613) = pv(613)+d2(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,l0)/mu
 pv(614) = pv(614)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i0,l0)/mu
 pv(615) = pv(615)+d(i0,i1)*d2(i1,i2)*d(i0,i3)*d(i0,l0)/mu
 pv(616) = pv(616)+d(i0,i1)*d(i1,i2)*d2(i0,i3)*d(i0,l0)/mu
 pv(617) = pv(617)+d2(i0,i1)*d(i1,i2)*d(i1,i3)*d(i0,l0)/mu
 pv(618) = pv(618)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,l0)/mu
 pv(619) = pv(619)+d2(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,l0)/mu
 pv(620) = pv(620)+d(i0,i1)*d2(i1,i2)*d(i1,i3)*d(i0,l0)/mu
 pv(621) = pv(621)+d(i0,i2)*d2(i1,i2)*d(i1,i3)*d(i0,l0)/mu
 pv(622) = pv(622)+d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,l0)/mu
 pv(623) = pv(623)+d(i0,i1)*d(i1,i2)*d(i1,i3)*d(i2,i3)*d(i0,l0)/mu
 pv(718) = pv(718)+d(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,k0)*d(i0,l0)/mu
 pv(719) = pv(719)+d(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,k0)*d(i0,l0)/mu
 pv(720) = pv(720)+d(i0,i1)*d(i1,i2)*d(i1,i3)*d(i0,k0)*d(i0,l0)/mu
 pv(760) = pv(760)+d(i0,i1)*d(i0,i2)*d(i0,i3)*d(i1,k0)*d(i0,l0)/mu
 pv(761) = pv(761)+d(i0,i1)*d(i1,i2)*d(i0,i3)*d(i1,k0)*d(i0,l0)/mu
 pv(762) = pv(762)+d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,k0)*d(i0,l0)/mu
 pv(763) = pv(763)+d(i0,i1)*d(i1,i2)*d(i1,i3)*d(i1,k0)*d(i0,l0)/mu
 pv(764) = pv(764)+d(i0,i2)*d(i1,i2)*d(i1,i3)*d(i1,k0)*d(i0,l0)/mu
 pv(765) = pv(765)+d(i0,i1)*d(i0,i2)*d(i2,i3)*d(i1,k0)*d(i0,l0)/mu
 pv(766) = pv(766)+d(i0,i1)*d(i1,i2)*d(i2,i3)*d(i1,k0)*d(i0,l0)/mu
 pv(833) = pv(833)+d(i0,i1)*d(i0,i2)*d(i0,i3)*d2(i0,l0)/mu
 pv(834) = pv(834)+d(i0,i1)*d(i1,i2)*d(i0,i3)*d2(i0,l0)/mu
 pv(835) = pv(835)+d(i0,i1)*d(i1,i2)*d(i1,i3)*d2(i0,l0)/mu
 pv(901) = pv(901)+d(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,l0)*d(i1,l0)/mu
 pv(902) = pv(902)+d(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,l0)*d(i1,l0)/mu
 pv(903) = pv(903)+d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i0,l0)*d(i1,l0)/mu
 pv(904) = pv(904)+d(i0,i1)*d(i0,i2)*d(i2,i3)*d(i0,l0)*d(i1,l0)/mu
endif
END SUBROUTINE mg6211_i3
SUBROUTINE mg6211_ii (mxd, mu)
integer, intent (in) :: mxd, mu
if (5.le.mxd) then
 pv(257) = pv(257)+d2(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,i4)/mu
 pv(258) = pv(258)+d2(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,i4)/mu
 pv(259) = pv(259)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i0,i4)/mu
 pv(260) = pv(260)+d(i0,i1)*d2(i1,i2)*d(i0,i3)*d(i0,i4)/mu
 pv(406) = pv(406)+d(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,i4)*d(i0,k0)/mu
 pv(407) = pv(407)+d(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,i4)*d(i0,k0)/mu
 pv(408) = pv(408)+d(i0,i2)*d(i1,i2)*d(i1,i3)*d(i1,i4)*d(i0,k0)/mu
 pv(624) = pv(624)+d(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,i4)*d(i0,l0)/mu
 pv(625) = pv(625)+d(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,i4)*d(i0,l0)/mu
 pv(626) = pv(626)+d(i0,i2)*d(i1,i2)*d(i1,i3)*d(i1,i4)*d(i0,l0)/mu
endif
END SUBROUTINE mg6211_ii
SUBROUTINE mg6211_jj (mxd, mu)
integer, intent (in) :: mxd, mu
if (2.le.mxd) then
 pv(8) = pv(8)+d(j0,k0)*d(j0,l0)/mu
endif
if (3.le.mxd) then
endif
if (4.le.mxd) then
endif
if (5.le.mxd) then
endif
END SUBROUTINE mg6211_jj
SUBROUTINE mg6211_i0jj (mxd, mu)
integer, intent (in) :: mxd, mu
if (2.le.mxd) then
 pv(2) = pv(2)+d(i0,j0)*d(i0,k0)/mu
 pv(3) = pv(3)+d(i0,j0)*d(j0,k0)/mu
 pv(5) = pv(5)+d(i0,j0)*d(i0,l0)/mu
 pv(7) = pv(7)+d(i0,j0)*d(j0,l0)/mu
endif
if (3.le.mxd) then
 pv(23) = pv(23)+d2(i0,j0)*d(i0,k0)/mu
 pv(26) = pv(26)+d(i0,j0)*d(i0,j1)*d(i0,k0)/mu
 pv(28) = pv(28)+d(i0,j0)*d2(i0,k0)/mu
 pv(31) = pv(31)+d2(i0,j0)*d(j0,k0)/mu
 pv(32) = pv(32)+d(i0,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(37) = pv(37)+d2(i0,j0)*d(i0,l0)/mu
 pv(40) = pv(40)+d(i0,j0)*d(i0,j1)*d(i0,l0)/mu
 pv(42) = pv(42)+d(i0,j0)*d(i0,k0)*d(i0,l0)/mu
 pv(45) = pv(45)+d(i0,j0)*d(j0,k0)*d(i0,l0)/mu
 pv(47) = pv(47)+d(i0,j0)*d2(i0,l0)/mu
 pv(51) = pv(51)+d2(i0,j0)*d(j0,l0)/mu
 pv(52) = pv(52)+d(i0,j0)*d(i0,k0)*d(j0,l0)/mu
 pv(53) = pv(53)+d(i0,j0)*d(i0,l0)*d(j0,l0)/mu
endif
if (4.le.mxd) then
 pv(90) = pv(90)+d3(i0,j0)*d(i0,j1)/mu
 pv(109) = pv(109)+d3(i0,j0)*d(i0,k0)/mu
 pv(122) = pv(122)+d2(i0,j0)*d(i0,j1)*d(i0,k0)/mu
 pv(129) = pv(129)+d2(i0,j0)*d2(i0,k0)/mu
 pv(132) = pv(132)+d(i0,j0)*d(i0,j1)*d2(i0,k0)/mu
 pv(134) = pv(134)+d(i0,j0)*d3(i0,k0)/mu
 pv(145) = pv(145)+d3(i0,j0)*d(j0,k0)/mu
 pv(147) = pv(147)+d2(i0,j0)*d(i0,j1)*d(j0,k0)/mu
 pv(149) = pv(149)+d2(i0,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(151) = pv(151)+d(i0,j0)*d2(i0,k0)*d(j0,k0)/mu
 pv(164) = pv(164)+d3(i0,j0)*d(i0,l0)/mu
 pv(177) = pv(177)+d2(i0,j0)*d(i0,j1)*d(i0,l0)/mu
 pv(184) = pv(184)+d2(i0,j0)*d(i0,k0)*d(i0,l0)/mu
 pv(187) = pv(187)+d(i0,j0)*d(i0,j1)*d(i0,k0)*d(i0,l0)/mu
 pv(189) = pv(189)+d(i0,j0)*d2(i0,k0)*d(i0,l0)/mu
 pv(201) = pv(201)+d2(i0,j0)*d(j0,k0)*d(i0,l0)/mu
 pv(203) = pv(203)+d(i0,j0)*d(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(208) = pv(208)+d2(i0,j0)*d2(i0,l0)/mu
 pv(211) = pv(211)+d(i0,j0)*d(i0,j1)*d2(i0,l0)/mu
 pv(213) = pv(213)+d(i0,j0)*d(i0,k0)*d2(i0,l0)/mu
 pv(216) = pv(216)+d(i0,j0)*d(j0,k0)*d2(i0,l0)/mu
 pv(218) = pv(218)+d(i0,j0)*d3(i0,l0)/mu
 pv(231) = pv(231)+d3(i0,j0)*d(j0,l0)/mu
 pv(233) = pv(233)+d2(i0,j0)*d(i0,j1)*d(j0,l0)/mu
 pv(235) = pv(235)+d2(i0,j0)*d(i0,k0)*d(j0,l0)/mu
 pv(237) = pv(237)+d(i0,j0)*d2(i0,k0)*d(j0,l0)/mu
 pv(239) = pv(239)+d2(i0,j0)*d(i0,l0)*d(j0,l0)/mu
 pv(241) = pv(241)+d(i0,j0)*d(i0,k0)*d(i0,l0)*d(j0,l0)/mu
 pv(242) = pv(242)+d(i0,j0)*d2(i0,l0)*d(j0,l0)/mu
endif
if (5.le.mxd) then
 pv(296) = pv(296)+d5(i0,j0)/mu
 pv(353) = pv(353)+d4(i0,j0)*d(i0,j1)/mu
 pv(421) = pv(421)+d4(i0,j0)*d(i0,k0)/mu
 pv(476) = pv(476)+d3(i0,j0)*d(i0,j1)*d(i0,k0)/mu
 pv(485) = pv(485)+d2(i0,j0)*d2(i0,j1)*d(i0,k0)/mu
 pv(507) = pv(507)+d3(i0,j0)*d2(i0,k0)/mu
 pv(520) = pv(520)+d2(i0,j0)*d(i0,j1)*d2(i0,k0)/mu
 pv(527) = pv(527)+d2(i0,j0)*d3(i0,k0)/mu
 pv(530) = pv(530)+d(i0,j0)*d(i0,j1)*d3(i0,k0)/mu
 pv(532) = pv(532)+d(i0,j0)*d4(i0,k0)/mu
 pv(576) = pv(576)+d4(i0,j0)*d(j0,k0)/mu
 pv(582) = pv(582)+d3(i0,j0)*d(i0,j1)*d(j0,k0)/mu
 pv(590) = pv(590)+d3(i0,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(597) = pv(597)+d2(i0,j0)*d(i0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(600) = pv(600)+d2(i0,j0)*d2(i0,k0)*d(j0,k0)/mu
 pv(602) = pv(602)+d(i0,j0)*d3(i0,k0)*d(j0,k0)/mu
 pv(639) = pv(639)+d4(i0,j0)*d(i0,l0)/mu
 pv(694) = pv(694)+d3(i0,j0)*d(i0,j1)*d(i0,l0)/mu
 pv(703) = pv(703)+d2(i0,j0)*d2(i0,j1)*d(i0,l0)/mu
 pv(725) = pv(725)+d3(i0,j0)*d(i0,k0)*d(i0,l0)/mu
 pv(738) = pv(738)+d2(i0,j0)*d(i0,j1)*d(i0,k0)*d(i0,l0)/mu
 pv(745) = pv(745)+d2(i0,j0)*d2(i0,k0)*d(i0,l0)/mu
 pv(748) = pv(748)+d(i0,j0)*d(i0,j1)*d2(i0,k0)*d(i0,l0)/mu
 pv(750) = pv(750)+d(i0,j0)*d3(i0,k0)*d(i0,l0)/mu
 pv(813) = pv(813)+d3(i0,j0)*d(j0,k0)*d(i0,l0)/mu
 pv(820) = pv(820)+d2(i0,j0)*d(i0,j1)*d(j0,k0)*d(i0,l0)/mu
 pv(823) = pv(823)+d2(i0,j0)*d(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(825) = pv(825)+d(i0,j0)*d2(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(840) = pv(840)+d3(i0,j0)*d2(i0,l0)/mu
 pv(853) = pv(853)+d2(i0,j0)*d(i0,j1)*d2(i0,l0)/mu
 pv(860) = pv(860)+d2(i0,j0)*d(i0,k0)*d2(i0,l0)/mu
 pv(863) = pv(863)+d(i0,j0)*d(i0,j1)*d(i0,k0)*d2(i0,l0)/mu
 pv(865) = pv(865)+d(i0,j0)*d2(i0,k0)*d2(i0,l0)/mu
 pv(877) = pv(877)+d2(i0,j0)*d(j0,k0)*d2(i0,l0)/mu
 pv(879) = pv(879)+d(i0,j0)*d(i0,k0)*d(j0,k0)*d2(i0,l0)/mu
 pv(884) = pv(884)+d2(i0,j0)*d3(i0,l0)/mu
 pv(887) = pv(887)+d(i0,j0)*d(i0,j1)*d3(i0,l0)/mu
 pv(889) = pv(889)+d(i0,j0)*d(i0,k0)*d3(i0,l0)/mu
 pv(892) = pv(892)+d(i0,j0)*d(j0,k0)*d3(i0,l0)/mu
 pv(894) = pv(894)+d(i0,j0)*d4(i0,l0)/mu
 pv(952) = pv(952)+d4(i0,j0)*d(j0,l0)/mu
 pv(958) = pv(958)+d3(i0,j0)*d(i0,j1)*d(j0,l0)/mu
 pv(966) = pv(966)+d3(i0,j0)*d(i0,k0)*d(j0,l0)/mu
 pv(973) = pv(973)+d2(i0,j0)*d(i0,j1)*d(i0,k0)*d(j0,l0)/mu
 pv(976) = pv(976)+d2(i0,j0)*d2(i0,k0)*d(j0,l0)/mu
 pv(978) = pv(978)+d(i0,j0)*d3(i0,k0)*d(j0,l0)/mu
 pv(984) = pv(984)+d3(i0,j0)*d(i0,l0)*d(j0,l0)/mu
 pv(991) = pv(991)+d2(i0,j0)*d(i0,j1)*d(i0,l0)*d(j0,l0)/mu
 pv(994) = pv(994)+d2(i0,j0)*d(i0,k0)*d(i0,l0)*d(j0,l0)/mu
 pv(996) = pv(996)+d(i0,j0)*d2(i0,k0)*d(i0,l0)*d(j0,l0)/mu
 pv(1000) = pv(1000)+d2(i0,j0)*d2(i0,l0)*d(j0,l0)/mu
 pv(1002) = pv(1002)+d(i0,j0)*d(i0,k0)*d2(i0,l0)*d(j0,l0)/mu
 pv(1003) = pv(1003)+d(i0,j0)*d3(i0,l0)*d(j0,l0)/mu
endif
END SUBROUTINE mg6211_i0jj
SUBROUTINE mg6211_i1jj (mxd, mu)
integer, intent (in) :: mxd, mu
if (2.le.mxd) then
 pv(0) = pv(0)+d(i0,i1)*d(i0,j0)/mu
endif
if (3.le.mxd) then
 pv(10) = pv(10)+d2(i0,i1)*d(i0,j0)/mu
 pv(13) = pv(13)+d(i0,i1)*d2(i0,j0)/mu
 pv(14) = pv(14)+d(i0,i1)*d(i0,j0)*d(i1,j0)/mu
 pv(16) = pv(16)+d2(i0,j0)*d(i1,j0)/mu
 pv(17) = pv(17)+d(i0,i1)*d(i0,j0)*d(i0,j1)/mu
 pv(18) = pv(18)+d(i0,i1)*d(i1,j0)*d(i0,j1)/mu
 pv(22) = pv(22)+d(i0,i1)*d(i0,j0)*d(i0,k0)/mu
 pv(24) = pv(24)+d(i0,i1)*d(i1,j0)*d(i0,k0)/mu
 pv(25) = pv(25)+d(i0,j0)*d(i1,j0)*d(i0,k0)/mu
 pv(30) = pv(30)+d(i0,i1)*d(i0,j0)*d(j0,k0)/mu
 pv(36) = pv(36)+d(i0,i1)*d(i0,j0)*d(i0,l0)/mu
 pv(38) = pv(38)+d(i0,i1)*d(i1,j0)*d(i0,l0)/mu
 pv(39) = pv(39)+d(i0,j0)*d(i1,j0)*d(i0,l0)/mu
 pv(50) = pv(50)+d(i0,i1)*d(i0,j0)*d(j0,l0)/mu
endif
if (4.le.mxd) then
 pv(61) = pv(61)+d3(i0,i1)*d(i0,j0)/mu
 pv(69) = pv(69)+d2(i0,i1)*d2(i0,j0)/mu
 pv(72) = pv(72)+d(i0,i1)*d3(i0,j0)/mu
 pv(73) = pv(73)+d2(i0,i1)*d(i0,j0)*d(i1,j0)/mu
 pv(80) = pv(80)+d(i0,i1)*d2(i0,j0)*d(i1,j0)/mu
 pv(83) = pv(83)+d3(i0,j0)*d(i1,j0)/mu
 pv(84) = pv(84)+d2(i0,j0)*d2(i1,j0)/mu
 pv(86) = pv(86)+d2(i0,i1)*d(i0,j0)*d(i0,j1)/mu
 pv(89) = pv(89)+d(i0,i1)*d2(i0,j0)*d(i0,j1)/mu
 pv(91) = pv(91)+d2(i0,i1)*d(i1,j0)*d(i0,j1)/mu
 pv(94) = pv(94)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,j1)/mu
 pv(95) = pv(95)+d2(i0,j0)*d(i1,j0)*d(i0,j1)/mu
 pv(96) = pv(96)+d(i0,i1)*d2(i1,j0)*d(i0,j1)/mu
 pv(105) = pv(105)+d2(i0,i1)*d(i0,j0)*d(i0,k0)/mu
 pv(108) = pv(108)+d(i0,i1)*d2(i0,j0)*d(i0,k0)/mu
 pv(110) = pv(110)+d2(i0,i1)*d(i1,j0)*d(i0,k0)/mu
 pv(114) = pv(114)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,k0)/mu
 pv(117) = pv(117)+d2(i0,j0)*d(i1,j0)*d(i0,k0)/mu
 pv(118) = pv(118)+d(i0,i1)*d2(i1,j0)*d(i0,k0)/mu
 pv(119) = pv(119)+d(i0,j0)*d2(i1,j0)*d(i0,k0)/mu
 pv(121) = pv(121)+d(i0,i1)*d(i0,j0)*d(i0,j1)*d(i0,k0)/mu
 pv(123) = pv(123)+d(i0,i1)*d(i1,j0)*d(i0,j1)*d(i0,k0)/mu
 pv(124) = pv(124)+d(i0,i1)*d(i1,j0)*d(i1,j1)*d(i0,k0)/mu
 pv(128) = pv(128)+d(i0,i1)*d(i0,j0)*d2(i0,k0)/mu
 pv(130) = pv(130)+d(i0,i1)*d(i1,j0)*d2(i0,k0)/mu
 pv(131) = pv(131)+d(i0,j0)*d(i1,j0)*d2(i0,k0)/mu
 pv(138) = pv(138)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d(i1,k0)/mu
 pv(139) = pv(139)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d(i1,k0)/mu
 pv(141) = pv(141)+d2(i0,i1)*d(i0,j0)*d(j0,k0)/mu
 pv(144) = pv(144)+d(i0,i1)*d2(i0,j0)*d(j0,k0)/mu
 pv(146) = pv(146)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(j0,k0)/mu
 pv(148) = pv(148)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(150) = pv(150)+d(i0,i1)*d(i1,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(160) = pv(160)+d2(i0,i1)*d(i0,j0)*d(i0,l0)/mu
 pv(163) = pv(163)+d(i0,i1)*d2(i0,j0)*d(i0,l0)/mu
 pv(165) = pv(165)+d2(i0,i1)*d(i1,j0)*d(i0,l0)/mu
 pv(169) = pv(169)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,l0)/mu
 pv(172) = pv(172)+d2(i0,j0)*d(i1,j0)*d(i0,l0)/mu
 pv(173) = pv(173)+d(i0,i1)*d2(i1,j0)*d(i0,l0)/mu
 pv(174) = pv(174)+d(i0,j0)*d2(i1,j0)*d(i0,l0)/mu
 pv(176) = pv(176)+d(i0,i1)*d(i0,j0)*d(i0,j1)*d(i0,l0)/mu
 pv(178) = pv(178)+d(i0,i1)*d(i1,j0)*d(i0,j1)*d(i0,l0)/mu
 pv(179) = pv(179)+d(i0,i1)*d(i1,j0)*d(i1,j1)*d(i0,l0)/mu
 pv(183) = pv(183)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d(i0,l0)/mu
 pv(185) = pv(185)+d(i0,i1)*d(i1,j0)*d(i0,k0)*d(i0,l0)/mu
 pv(186) = pv(186)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d(i0,l0)/mu
 pv(195) = pv(195)+d(i0,i1)*d(i0,j0)*d(i1,k0)*d(i0,l0)/mu
 pv(196) = pv(196)+d(i0,i1)*d(i1,j0)*d(i1,k0)*d(i0,l0)/mu
 pv(197) = pv(197)+d(i0,j0)*d(i1,j0)*d(i1,k0)*d(i0,l0)/mu
 pv(200) = pv(200)+d(i0,i1)*d(i0,j0)*d(j0,k0)*d(i0,l0)/mu
 pv(202) = pv(202)+d(i0,i1)*d(i1,j0)*d(j0,k0)*d(i0,l0)/mu
 pv(207) = pv(207)+d(i0,i1)*d(i0,j0)*d2(i0,l0)/mu
 pv(209) = pv(209)+d(i0,i1)*d(i1,j0)*d2(i0,l0)/mu
 pv(210) = pv(210)+d(i0,j0)*d(i1,j0)*d2(i0,l0)/mu
 pv(223) = pv(223)+d(i0,i1)*d(i0,j0)*d(i0,l0)*d(i1,l0)/mu
 pv(224) = pv(224)+d(i0,j0)*d(i1,j0)*d(i0,l0)*d(i1,l0)/mu
 pv(227) = pv(227)+d2(i0,i1)*d(i0,j0)*d(j0,l0)/mu
 pv(230) = pv(230)+d(i0,i1)*d2(i0,j0)*d(j0,l0)/mu
 pv(232) = pv(232)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(j0,l0)/mu
 pv(234) = pv(234)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d(j0,l0)/mu
 pv(236) = pv(236)+d(i0,i1)*d(i1,j0)*d(i0,k0)*d(j0,l0)/mu
 pv(238) = pv(238)+d(i0,i1)*d(i0,j0)*d(i0,l0)*d(j0,l0)/mu
 pv(240) = pv(240)+d(i0,i1)*d(i1,j0)*d(i0,l0)*d(j0,l0)/mu
endif
if (5.le.mxd) then
 pv(261) = pv(261)+d4(i0,i1)*d(i0,j0)/mu
 pv(284) = pv(284)+d3(i0,i1)*d2(i0,j0)/mu
 pv(292) = pv(292)+d2(i0,i1)*d3(i0,j0)/mu
 pv(295) = pv(295)+d(i0,i1)*d4(i0,j0)/mu
 pv(297) = pv(297)+d3(i0,i1)*d(i0,j0)*d(i1,j0)/mu
 pv(317) = pv(317)+d2(i0,i1)*d2(i0,j0)*d(i1,j0)/mu
 pv(328) = pv(328)+d(i0,i1)*d3(i0,j0)*d(i1,j0)/mu
 pv(331) = pv(331)+d4(i0,j0)*d(i1,j0)/mu
 pv(332) = pv(332)+d(i0,i1)*d2(i0,j0)*d2(i1,j0)/mu
 pv(334) = pv(334)+d3(i0,j0)*d2(i1,j0)/mu
 pv(341) = pv(341)+d3(i0,i1)*d(i0,j0)*d(i0,j1)/mu
 pv(349) = pv(349)+d2(i0,i1)*d2(i0,j0)*d(i0,j1)/mu
 pv(352) = pv(352)+d(i0,i1)*d3(i0,j0)*d(i0,j1)/mu
 pv(354) = pv(354)+d3(i0,i1)*d(i1,j0)*d(i0,j1)/mu
 pv(363) = pv(363)+d2(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,j1)/mu
 pv(367) = pv(367)+d(i0,i1)*d2(i0,j0)*d(i1,j0)*d(i0,j1)/mu
 pv(370) = pv(370)+d3(i0,j0)*d(i1,j0)*d(i0,j1)/mu
 pv(371) = pv(371)+d2(i0,i1)*d2(i1,j0)*d(i0,j1)/mu
 pv(375) = pv(375)+d(i0,i1)*d(i0,j0)*d2(i1,j0)*d(i0,j1)/mu
 pv(376) = pv(376)+d2(i0,j0)*d2(i1,j0)*d(i0,j1)/mu
 pv(377) = pv(377)+d(i0,i1)*d3(i1,j0)*d(i0,j1)/mu
 pv(382) = pv(382)+d(i0,i1)*d2(i0,j0)*d2(i0,j1)/mu
 pv(383) = pv(383)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d2(i0,j1)/mu
 pv(384) = pv(384)+d(i0,i1)*d2(i1,j0)*d2(i0,j1)/mu
 pv(385) = pv(385)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,j1)*d(i1,j1)/mu
 pv(409) = pv(409)+d3(i0,i1)*d(i0,j0)*d(i0,k0)/mu
 pv(417) = pv(417)+d2(i0,i1)*d2(i0,j0)*d(i0,k0)/mu
 pv(420) = pv(420)+d(i0,i1)*d3(i0,j0)*d(i0,k0)/mu
 pv(422) = pv(422)+d3(i0,i1)*d(i1,j0)*d(i0,k0)/mu
 pv(437) = pv(437)+d2(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,k0)/mu
 pv(448) = pv(448)+d(i0,i1)*d2(i0,j0)*d(i1,j0)*d(i0,k0)/mu
 pv(451) = pv(451)+d3(i0,j0)*d(i1,j0)*d(i0,k0)/mu
 pv(452) = pv(452)+d2(i0,i1)*d2(i1,j0)*d(i0,k0)/mu
 pv(456) = pv(456)+d(i0,i1)*d(i0,j0)*d2(i1,j0)*d(i0,k0)/mu
 pv(459) = pv(459)+d2(i0,j0)*d2(i1,j0)*d(i0,k0)/mu
 pv(460) = pv(460)+d(i0,i1)*d3(i1,j0)*d(i0,k0)/mu
 pv(461) = pv(461)+d(i0,j0)*d3(i1,j0)*d(i0,k0)/mu
 pv(472) = pv(472)+d2(i0,i1)*d(i0,j0)*d(i0,j1)*d(i0,k0)/mu
 pv(475) = pv(475)+d(i0,i1)*d2(i0,j0)*d(i0,j1)*d(i0,k0)/mu
 pv(477) = pv(477)+d2(i0,i1)*d(i1,j0)*d(i0,j1)*d(i0,k0)/mu
 pv(481) = pv(481)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,j1)*d(i0,k0)/mu
 pv(482) = pv(482)+d2(i0,j0)*d(i1,j0)*d(i0,j1)*d(i0,k0)/mu
 pv(483) = pv(483)+d(i0,i1)*d2(i1,j0)*d(i0,j1)*d(i0,k0)/mu
 pv(486) = pv(486)+d(i0,i1)*d(i1,j0)*d2(i0,j1)*d(i0,k0)/mu
 pv(487) = pv(487)+d2(i0,i1)*d(i1,j0)*d(i1,j1)*d(i0,k0)/mu
 pv(491) = pv(491)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i1,j1)*d(i0,k0)/mu
 pv(492) = pv(492)+d(i0,i1)*d2(i1,j0)*d(i1,j1)*d(i0,k0)/mu
 pv(493) = pv(493)+d(i0,j0)*d2(i1,j0)*d(i1,j1)*d(i0,k0)/mu
 pv(503) = pv(503)+d2(i0,i1)*d(i0,j0)*d2(i0,k0)/mu
 pv(506) = pv(506)+d(i0,i1)*d2(i0,j0)*d2(i0,k0)/mu
 pv(508) = pv(508)+d2(i0,i1)*d(i1,j0)*d2(i0,k0)/mu
 pv(512) = pv(512)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d2(i0,k0)/mu
 pv(515) = pv(515)+d2(i0,j0)*d(i1,j0)*d2(i0,k0)/mu
 pv(516) = pv(516)+d(i0,i1)*d2(i1,j0)*d2(i0,k0)/mu
 pv(517) = pv(517)+d(i0,j0)*d2(i1,j0)*d2(i0,k0)/mu
 pv(519) = pv(519)+d(i0,i1)*d(i0,j0)*d(i0,j1)*d2(i0,k0)/mu
 pv(521) = pv(521)+d(i0,i1)*d(i1,j0)*d(i0,j1)*d2(i0,k0)/mu
 pv(522) = pv(522)+d(i0,i1)*d(i1,j0)*d(i1,j1)*d2(i0,k0)/mu
 pv(526) = pv(526)+d(i0,i1)*d(i0,j0)*d3(i0,k0)/mu
 pv(528) = pv(528)+d(i0,i1)*d(i1,j0)*d3(i0,k0)/mu
 pv(529) = pv(529)+d(i0,j0)*d(i1,j0)*d3(i0,k0)/mu
 pv(542) = pv(542)+d2(i0,i1)*d(i0,j0)*d(i0,k0)*d(i1,k0)/mu
 pv(546) = pv(546)+d(i0,i1)*d2(i0,j0)*d(i0,k0)*d(i1,k0)/mu
 pv(547) = pv(547)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,k0)*d(i1,k0)/mu
 pv(549) = pv(549)+d2(i0,j0)*d(i1,j0)*d(i0,k0)*d(i1,k0)/mu
 pv(553) = pv(553)+d(i0,i1)*d(i0,j0)*d(i0,j1)*d(i0,k0)*d(i1,k0)/mu
 pv(554) = pv(554)+d(i0,i1)*d(i1,j0)*d(i0,j1)*d(i0,k0)*d(i1,k0)/mu
 pv(559) = pv(559)+d(i0,i1)*d(i0,j0)*d2(i0,k0)*d(i1,k0)/mu
 pv(560) = pv(560)+d(i0,i1)*d(i1,j0)*d2(i0,k0)*d(i1,k0)/mu
 pv(561) = pv(561)+d(i0,j0)*d(i1,j0)*d2(i0,k0)*d(i1,k0)/mu
 pv(564) = pv(564)+d3(i0,i1)*d(i0,j0)*d(j0,k0)/mu
 pv(572) = pv(572)+d2(i0,i1)*d2(i0,j0)*d(j0,k0)/mu
 pv(575) = pv(575)+d(i0,i1)*d3(i0,j0)*d(j0,k0)/mu
 pv(577) = pv(577)+d2(i0,i1)*d(i0,j0)*d(i1,j0)*d(j0,k0)/mu
 pv(580) = pv(580)+d(i0,i1)*d2(i0,j0)*d(i1,j0)*d(j0,k0)/mu
 pv(581) = pv(581)+d(i0,i1)*d2(i0,j0)*d(i0,j1)*d(j0,k0)/mu
 pv(584) = pv(584)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,j1)*d(j0,k0)/mu
 pv(585) = pv(585)+d(i0,i1)*d2(i1,j0)*d(i0,j1)*d(j0,k0)/mu
 pv(586) = pv(586)+d2(i0,i1)*d(i0,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(589) = pv(589)+d(i0,i1)*d2(i0,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(591) = pv(591)+d2(i0,i1)*d(i1,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(595) = pv(595)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(596) = pv(596)+d(i0,i1)*d2(i1,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(598) = pv(598)+d(i0,i1)*d(i1,j0)*d(i0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(599) = pv(599)+d(i0,i1)*d(i0,j0)*d2(i0,k0)*d(j0,k0)/mu
 pv(601) = pv(601)+d(i0,i1)*d(i1,j0)*d2(i0,k0)*d(j0,k0)/mu
 pv(603) = pv(603)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d(i1,k0)*d(j0,k0)/mu
 pv(627) = pv(627)+d3(i0,i1)*d(i0,j0)*d(i0,l0)/mu
 pv(635) = pv(635)+d2(i0,i1)*d2(i0,j0)*d(i0,l0)/mu
 pv(638) = pv(638)+d(i0,i1)*d3(i0,j0)*d(i0,l0)/mu
 pv(640) = pv(640)+d3(i0,i1)*d(i1,j0)*d(i0,l0)/mu
 pv(655) = pv(655)+d2(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,l0)/mu
 pv(666) = pv(666)+d(i0,i1)*d2(i0,j0)*d(i1,j0)*d(i0,l0)/mu
 pv(669) = pv(669)+d3(i0,j0)*d(i1,j0)*d(i0,l0)/mu
 pv(670) = pv(670)+d2(i0,i1)*d2(i1,j0)*d(i0,l0)/mu
 pv(674) = pv(674)+d(i0,i1)*d(i0,j0)*d2(i1,j0)*d(i0,l0)/mu
 pv(677) = pv(677)+d2(i0,j0)*d2(i1,j0)*d(i0,l0)/mu
 pv(678) = pv(678)+d(i0,i1)*d3(i1,j0)*d(i0,l0)/mu
 pv(679) = pv(679)+d(i0,j0)*d3(i1,j0)*d(i0,l0)/mu
 pv(690) = pv(690)+d2(i0,i1)*d(i0,j0)*d(i0,j1)*d(i0,l0)/mu
 pv(693) = pv(693)+d(i0,i1)*d2(i0,j0)*d(i0,j1)*d(i0,l0)/mu
 pv(695) = pv(695)+d2(i0,i1)*d(i1,j0)*d(i0,j1)*d(i0,l0)/mu
 pv(699) = pv(699)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,j1)*d(i0,l0)/mu
 pv(700) = pv(700)+d2(i0,j0)*d(i1,j0)*d(i0,j1)*d(i0,l0)/mu
 pv(701) = pv(701)+d(i0,i1)*d2(i1,j0)*d(i0,j1)*d(i0,l0)/mu
 pv(704) = pv(704)+d(i0,i1)*d(i1,j0)*d2(i0,j1)*d(i0,l0)/mu
 pv(705) = pv(705)+d2(i0,i1)*d(i1,j0)*d(i1,j1)*d(i0,l0)/mu
 pv(709) = pv(709)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i1,j1)*d(i0,l0)/mu
 pv(710) = pv(710)+d(i0,i1)*d2(i1,j0)*d(i1,j1)*d(i0,l0)/mu
 pv(711) = pv(711)+d(i0,j0)*d2(i1,j0)*d(i1,j1)*d(i0,l0)/mu
 pv(721) = pv(721)+d2(i0,i1)*d(i0,j0)*d(i0,k0)*d(i0,l0)/mu
 pv(724) = pv(724)+d(i0,i1)*d2(i0,j0)*d(i0,k0)*d(i0,l0)/mu
 pv(726) = pv(726)+d2(i0,i1)*d(i1,j0)*d(i0,k0)*d(i0,l0)/mu
 pv(730) = pv(730)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,k0)*d(i0,l0)/mu
 pv(733) = pv(733)+d2(i0,j0)*d(i1,j0)*d(i0,k0)*d(i0,l0)/mu
 pv(734) = pv(734)+d(i0,i1)*d2(i1,j0)*d(i0,k0)*d(i0,l0)/mu
 pv(735) = pv(735)+d(i0,j0)*d2(i1,j0)*d(i0,k0)*d(i0,l0)/mu
 pv(737) = pv(737)+d(i0,i1)*d(i0,j0)*d(i0,j1)*d(i0,k0)*d(i0,l0)/mu
 pv(739) = pv(739)+d(i0,i1)*d(i1,j0)*d(i0,j1)*d(i0,k0)*d(i0,l0)/mu
 pv(740) = pv(740)+d(i0,i1)*d(i1,j0)*d(i1,j1)*d(i0,k0)*d(i0,l0)/mu
 pv(744) = pv(744)+d(i0,i1)*d(i0,j0)*d2(i0,k0)*d(i0,l0)/mu
 pv(746) = pv(746)+d(i0,i1)*d(i1,j0)*d2(i0,k0)*d(i0,l0)/mu
 pv(747) = pv(747)+d(i0,j0)*d(i1,j0)*d2(i0,k0)*d(i0,l0)/mu
 pv(767) = pv(767)+d2(i0,i1)*d(i0,j0)*d(i1,k0)*d(i0,l0)/mu
 pv(771) = pv(771)+d(i0,i1)*d2(i0,j0)*d(i1,k0)*d(i0,l0)/mu
 pv(772) = pv(772)+d2(i0,i1)*d(i1,j0)*d(i1,k0)*d(i0,l0)/mu
 pv(776) = pv(776)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i1,k0)*d(i0,l0)/mu
 pv(779) = pv(779)+d2(i0,j0)*d(i1,j0)*d(i1,k0)*d(i0,l0)/mu
 pv(780) = pv(780)+d(i0,i1)*d2(i1,j0)*d(i1,k0)*d(i0,l0)/mu
 pv(781) = pv(781)+d(i0,j0)*d2(i1,j0)*d(i1,k0)*d(i0,l0)/mu
 pv(788) = pv(788)+d(i0,i1)*d(i0,j0)*d(i0,j1)*d(i1,k0)*d(i0,l0)/mu
 pv(789) = pv(789)+d(i0,i1)*d(i1,j0)*d(i0,j1)*d(i1,k0)*d(i0,l0)/mu
 pv(790) = pv(790)+d(i0,i1)*d(i1,j0)*d(i1,j1)*d(i1,k0)*d(i0,l0)/mu
 pv(795) = pv(795)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d(i1,k0)*d(i0,l0)/mu
 pv(796) = pv(796)+d(i0,i1)*d(i1,j0)*d(i0,k0)*d(i1,k0)*d(i0,l0)/mu
 pv(797) = pv(797)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d(i1,k0)*d(i0,l0)/mu
 pv(803) = pv(803)+d(i0,i1)*d(i0,j0)*d2(i1,k0)*d(i0,l0)/mu
 pv(804) = pv(804)+d(i0,i1)*d(i1,j0)*d2(i1,k0)*d(i0,l0)/mu
 pv(805) = pv(805)+d(i0,j0)*d(i1,j0)*d2(i1,k0)*d(i0,l0)/mu
 pv(809) = pv(809)+d2(i0,i1)*d(i0,j0)*d(j0,k0)*d(i0,l0)/mu
 pv(812) = pv(812)+d(i0,i1)*d2(i0,j0)*d(j0,k0)*d(i0,l0)/mu
 pv(814) = pv(814)+d2(i0,i1)*d(i1,j0)*d(j0,k0)*d(i0,l0)/mu
 pv(818) = pv(818)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(j0,k0)*d(i0,l0)/mu
 pv(819) = pv(819)+d(i0,i1)*d2(i1,j0)*d(j0,k0)*d(i0,l0)/mu
 pv(821) = pv(821)+d(i0,i1)*d(i1,j0)*d(i0,j1)*d(j0,k0)*d(i0,l0)/mu
 pv(822) = pv(822)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(824) = pv(824)+d(i0,i1)*d(i1,j0)*d(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(826) = pv(826)+d(i0,i1)*d(i0,j0)*d(i1,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(827) = pv(827)+d(i0,i1)*d(i1,j0)*d(i1,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(836) = pv(836)+d2(i0,i1)*d(i0,j0)*d2(i0,l0)/mu
 pv(839) = pv(839)+d(i0,i1)*d2(i0,j0)*d2(i0,l0)/mu
 pv(841) = pv(841)+d2(i0,i1)*d(i1,j0)*d2(i0,l0)/mu
 pv(845) = pv(845)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d2(i0,l0)/mu
 pv(848) = pv(848)+d2(i0,j0)*d(i1,j0)*d2(i0,l0)/mu
 pv(849) = pv(849)+d(i0,i1)*d2(i1,j0)*d2(i0,l0)/mu
 pv(850) = pv(850)+d(i0,j0)*d2(i1,j0)*d2(i0,l0)/mu
 pv(852) = pv(852)+d(i0,i1)*d(i0,j0)*d(i0,j1)*d2(i0,l0)/mu
 pv(854) = pv(854)+d(i0,i1)*d(i1,j0)*d(i0,j1)*d2(i0,l0)/mu
 pv(855) = pv(855)+d(i0,i1)*d(i1,j0)*d(i1,j1)*d2(i0,l0)/mu
 pv(859) = pv(859)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d2(i0,l0)/mu
 pv(861) = pv(861)+d(i0,i1)*d(i1,j0)*d(i0,k0)*d2(i0,l0)/mu
 pv(862) = pv(862)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d2(i0,l0)/mu
 pv(871) = pv(871)+d(i0,i1)*d(i0,j0)*d(i1,k0)*d2(i0,l0)/mu
 pv(872) = pv(872)+d(i0,i1)*d(i1,j0)*d(i1,k0)*d2(i0,l0)/mu
 pv(873) = pv(873)+d(i0,j0)*d(i1,j0)*d(i1,k0)*d2(i0,l0)/mu
 pv(876) = pv(876)+d(i0,i1)*d(i0,j0)*d(j0,k0)*d2(i0,l0)/mu
 pv(878) = pv(878)+d(i0,i1)*d(i1,j0)*d(j0,k0)*d2(i0,l0)/mu
 pv(883) = pv(883)+d(i0,i1)*d(i0,j0)*d3(i0,l0)/mu
 pv(885) = pv(885)+d(i0,i1)*d(i1,j0)*d3(i0,l0)/mu
 pv(886) = pv(886)+d(i0,j0)*d(i1,j0)*d3(i0,l0)/mu
 pv(905) = pv(905)+d2(i0,i1)*d(i0,j0)*d(i0,l0)*d(i1,l0)/mu
 pv(909) = pv(909)+d(i0,i1)*d2(i0,j0)*d(i0,l0)*d(i1,l0)/mu
 pv(910) = pv(910)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,l0)*d(i1,l0)/mu
 pv(912) = pv(912)+d2(i0,j0)*d(i1,j0)*d(i0,l0)*d(i1,l0)/mu
 pv(916) = pv(916)+d(i0,i1)*d(i0,j0)*d(i0,j1)*d(i0,l0)*d(i1,l0)/mu
 pv(917) = pv(917)+d(i0,i1)*d(i1,j0)*d(i0,j1)*d(i0,l0)*d(i1,l0)/mu
 pv(922) = pv(922)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d(i0,l0)*d(i1,l0)/mu
 pv(923) = pv(923)+d(i0,i1)*d(i1,j0)*d(i0,k0)*d(i0,l0)*d(i1,l0)/mu
 pv(924) = pv(924)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d(i0,l0)*d(i1,l0)/mu
 pv(928) = pv(928)+d(i0,i1)*d(i0,j0)*d(j0,k0)*d(i0,l0)*d(i1,l0)/mu
 pv(933) = pv(933)+d(i0,i1)*d(i0,j0)*d2(i0,l0)*d(i1,l0)/mu
 pv(934) = pv(934)+d(i0,i1)*d(i1,j0)*d2(i0,l0)*d(i1,l0)/mu
 pv(935) = pv(935)+d(i0,j0)*d(i1,j0)*d2(i0,l0)*d(i1,l0)/mu
 pv(940) = pv(940)+d3(i0,i1)*d(i0,j0)*d(j0,l0)/mu
 pv(948) = pv(948)+d2(i0,i1)*d2(i0,j0)*d(j0,l0)/mu
 pv(951) = pv(951)+d(i0,i1)*d3(i0,j0)*d(j0,l0)/mu
 pv(953) = pv(953)+d2(i0,i1)*d(i0,j0)*d(i1,j0)*d(j0,l0)/mu
 pv(956) = pv(956)+d(i0,i1)*d2(i0,j0)*d(i1,j0)*d(j0,l0)/mu
 pv(957) = pv(957)+d(i0,i1)*d2(i0,j0)*d(i0,j1)*d(j0,l0)/mu
 pv(960) = pv(960)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,j1)*d(j0,l0)/mu
 pv(961) = pv(961)+d(i0,i1)*d2(i1,j0)*d(i0,j1)*d(j0,l0)/mu
 pv(962) = pv(962)+d2(i0,i1)*d(i0,j0)*d(i0,k0)*d(j0,l0)/mu
 pv(965) = pv(965)+d(i0,i1)*d2(i0,j0)*d(i0,k0)*d(j0,l0)/mu
 pv(967) = pv(967)+d2(i0,i1)*d(i1,j0)*d(i0,k0)*d(j0,l0)/mu
 pv(971) = pv(971)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,k0)*d(j0,l0)/mu
 pv(972) = pv(972)+d(i0,i1)*d2(i1,j0)*d(i0,k0)*d(j0,l0)/mu
 pv(974) = pv(974)+d(i0,i1)*d(i1,j0)*d(i0,j1)*d(i0,k0)*d(j0,l0)/mu
 pv(975) = pv(975)+d(i0,i1)*d(i0,j0)*d2(i0,k0)*d(j0,l0)/mu
 pv(977) = pv(977)+d(i0,i1)*d(i1,j0)*d2(i0,k0)*d(j0,l0)/mu
 pv(979) = pv(979)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d(i1,k0)*d(j0,l0)/mu
 pv(980) = pv(980)+d2(i0,i1)*d(i0,j0)*d(i0,l0)*d(j0,l0)/mu
 pv(983) = pv(983)+d(i0,i1)*d2(i0,j0)*d(i0,l0)*d(j0,l0)/mu
 pv(985) = pv(985)+d2(i0,i1)*d(i1,j0)*d(i0,l0)*d(j0,l0)/mu
 pv(989) = pv(989)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,l0)*d(j0,l0)/mu
 pv(990) = pv(990)+d(i0,i1)*d2(i1,j0)*d(i0,l0)*d(j0,l0)/mu
 pv(992) = pv(992)+d(i0,i1)*d(i1,j0)*d(i0,j1)*d(i0,l0)*d(j0,l0)/mu
 pv(993) = pv(993)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d(i0,l0)*d(j0,l0)/mu
 pv(995) = pv(995)+d(i0,i1)*d(i1,j0)*d(i0,k0)*d(i0,l0)*d(j0,l0)/mu
 pv(997) = pv(997)+d(i0,i1)*d(i0,j0)*d(i1,k0)*d(i0,l0)*d(j0,l0)/mu
 pv(998) = pv(998)+d(i0,i1)*d(i1,j0)*d(i1,k0)*d(i0,l0)*d(j0,l0)/mu
 pv(999) = pv(999)+d(i0,i1)*d(i0,j0)*d2(i0,l0)*d(j0,l0)/mu
 pv(1001) = pv(1001)+d(i0,i1)*d(i1,j0)*d2(i0,l0)*d(j0,l0)/mu
 pv(1004) = pv(1004)+d(i0,i1)*d(i0,j0)*d(i0,l0)*d(i1,l0)*d(j0,l0)/mu
endif
END SUBROUTINE mg6211_i1jj
SUBROUTINE mg6211_i2jj (mxd, mu)
integer, intent (in) :: mxd, mu
if (3.le.mxd) then
 pv(11) = pv(11)+d(i0,i1)*d(i0,i2)*d(i0,j0)/mu
 pv(12) = pv(12)+d(i0,i1)*d(i1,i2)*d(i0,j0)/mu
 pv(15) = pv(15)+d(i0,i2)*d(i0,j0)*d(i1,j0)/mu
endif
if (4.le.mxd) then
 pv(62) = pv(62)+d2(i0,i1)*d(i0,i2)*d(i0,j0)/mu
 pv(63) = pv(63)+d2(i0,i1)*d(i1,i2)*d(i0,j0)/mu
 pv(64) = pv(64)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,j0)/mu
 pv(65) = pv(65)+d(i0,i1)*d2(i1,i2)*d(i0,j0)/mu
 pv(70) = pv(70)+d(i0,i1)*d(i0,i2)*d2(i0,j0)/mu
 pv(71) = pv(71)+d(i0,i1)*d(i1,i2)*d2(i0,j0)/mu
 pv(74) = pv(74)+d(i0,i1)*d(i0,i2)*d(i0,j0)*d(i1,j0)/mu
 pv(75) = pv(75)+d2(i0,i2)*d(i0,j0)*d(i1,j0)/mu
 pv(76) = pv(76)+d(i0,i2)*d(i1,i2)*d(i0,j0)*d(i1,j0)/mu
 pv(81) = pv(81)+d(i0,i2)*d2(i0,j0)*d(i1,j0)/mu
 pv(82) = pv(82)+d(i1,i2)*d2(i0,j0)*d(i1,j0)/mu
 pv(85) = pv(85)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i2,j0)/mu
 pv(87) = pv(87)+d(i0,i1)*d(i0,i2)*d(i0,j0)*d(i0,j1)/mu
 pv(88) = pv(88)+d(i0,i1)*d(i1,i2)*d(i0,j0)*d(i0,j1)/mu
 pv(92) = pv(92)+d(i0,i1)*d(i0,i2)*d(i1,j0)*d(i0,j1)/mu
 pv(93) = pv(93)+d(i0,i2)*d(i1,i2)*d(i1,j0)*d(i0,j1)/mu
 pv(106) = pv(106)+d(i0,i1)*d(i0,i2)*d(i0,j0)*d(i0,k0)/mu
 pv(107) = pv(107)+d(i0,i1)*d(i1,i2)*d(i0,j0)*d(i0,k0)/mu
 pv(111) = pv(111)+d(i0,i1)*d(i0,i2)*d(i1,j0)*d(i0,k0)/mu
 pv(112) = pv(112)+d(i0,i1)*d(i1,i2)*d(i1,j0)*d(i0,k0)/mu
 pv(113) = pv(113)+d(i0,i2)*d(i1,i2)*d(i1,j0)*d(i0,k0)/mu
 pv(115) = pv(115)+d(i0,i2)*d(i0,j0)*d(i1,j0)*d(i0,k0)/mu
 pv(116) = pv(116)+d(i1,i2)*d(i0,j0)*d(i1,j0)*d(i0,k0)/mu
 pv(120) = pv(120)+d(i0,i1)*d(i1,j0)*d(i2,j0)*d(i0,k0)/mu
 pv(142) = pv(142)+d(i0,i1)*d(i0,i2)*d(i0,j0)*d(j0,k0)/mu
 pv(143) = pv(143)+d(i0,i1)*d(i1,i2)*d(i0,j0)*d(j0,k0)/mu
 pv(161) = pv(161)+d(i0,i1)*d(i0,i2)*d(i0,j0)*d(i0,l0)/mu
 pv(162) = pv(162)+d(i0,i1)*d(i1,i2)*d(i0,j0)*d(i0,l0)/mu
 pv(166) = pv(166)+d(i0,i1)*d(i0,i2)*d(i1,j0)*d(i0,l0)/mu
 pv(167) = pv(167)+d(i0,i1)*d(i1,i2)*d(i1,j0)*d(i0,l0)/mu
 pv(168) = pv(168)+d(i0,i2)*d(i1,i2)*d(i1,j0)*d(i0,l0)/mu
 pv(170) = pv(170)+d(i0,i2)*d(i0,j0)*d(i1,j0)*d(i0,l0)/mu
 pv(171) = pv(171)+d(i1,i2)*d(i0,j0)*d(i1,j0)*d(i0,l0)/mu
 pv(175) = pv(175)+d(i0,i1)*d(i1,j0)*d(i2,j0)*d(i0,l0)/mu
 pv(228) = pv(228)+d(i0,i1)*d(i0,i2)*d(i0,j0)*d(j0,l0)/mu
 pv(229) = pv(229)+d(i0,i1)*d(i1,i2)*d(i0,j0)*d(j0,l0)/mu
endif
if (5.le.mxd) then
 pv(262) = pv(262)+d3(i0,i1)*d(i0,i2)*d(i0,j0)/mu
 pv(263) = pv(263)+d2(i0,i1)*d2(i0,i2)*d(i0,j0)/mu
 pv(264) = pv(264)+d3(i0,i1)*d(i1,i2)*d(i0,j0)/mu
 pv(265) = pv(265)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,j0)/mu
 pv(266) = pv(266)+d2(i0,i1)*d2(i1,i2)*d(i0,j0)/mu
 pv(267) = pv(267)+d(i0,i1)*d(i0,i2)*d2(i1,i2)*d(i0,j0)/mu
 pv(268) = pv(268)+d(i0,i1)*d3(i1,i2)*d(i0,j0)/mu
 pv(285) = pv(285)+d2(i0,i1)*d(i0,i2)*d2(i0,j0)/mu
 pv(286) = pv(286)+d2(i0,i1)*d(i1,i2)*d2(i0,j0)/mu
 pv(287) = pv(287)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d2(i0,j0)/mu
 pv(288) = pv(288)+d(i0,i1)*d2(i1,i2)*d2(i0,j0)/mu
 pv(293) = pv(293)+d(i0,i1)*d(i0,i2)*d3(i0,j0)/mu
 pv(294) = pv(294)+d(i0,i1)*d(i1,i2)*d3(i0,j0)/mu
 pv(298) = pv(298)+d2(i0,i1)*d(i0,i2)*d(i0,j0)*d(i1,j0)/mu
 pv(299) = pv(299)+d(i0,i1)*d2(i0,i2)*d(i0,j0)*d(i1,j0)/mu
 pv(300) = pv(300)+d3(i0,i2)*d(i0,j0)*d(i1,j0)/mu
 pv(301) = pv(301)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,j0)*d(i1,j0)/mu
 pv(302) = pv(302)+d2(i0,i2)*d(i1,i2)*d(i0,j0)*d(i1,j0)/mu
 pv(318) = pv(318)+d(i0,i1)*d(i0,i2)*d2(i0,j0)*d(i1,j0)/mu
 pv(319) = pv(319)+d2(i0,i2)*d2(i0,j0)*d(i1,j0)/mu
 pv(320) = pv(320)+d(i0,i1)*d(i1,i2)*d2(i0,j0)*d(i1,j0)/mu
 pv(321) = pv(321)+d(i0,i2)*d(i1,i2)*d2(i0,j0)*d(i1,j0)/mu
 pv(322) = pv(322)+d2(i1,i2)*d2(i0,j0)*d(i1,j0)/mu
 pv(329) = pv(329)+d(i0,i2)*d3(i0,j0)*d(i1,j0)/mu
 pv(330) = pv(330)+d(i1,i2)*d3(i0,j0)*d(i1,j0)/mu
 pv(333) = pv(333)+d(i0,i2)*d2(i0,j0)*d2(i1,j0)/mu
 pv(335) = pv(335)+d2(i0,i1)*d(i0,j0)*d(i1,j0)*d(i2,j0)/mu
 pv(336) = pv(336)+d(i0,i1)*d(i0,i2)*d(i0,j0)*d(i1,j0)*d(i2,j0)/mu
 pv(339) = pv(339)+d(i0,i1)*d2(i0,j0)*d(i1,j0)*d(i2,j0)/mu
 pv(340) = pv(340)+d(i1,i2)*d2(i0,j0)*d(i1,j0)*d(i2,j0)/mu
 pv(342) = pv(342)+d2(i0,i1)*d(i0,i2)*d(i0,j0)*d(i0,j1)/mu
 pv(343) = pv(343)+d2(i0,i1)*d(i1,i2)*d(i0,j0)*d(i0,j1)/mu
 pv(344) = pv(344)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,j0)*d(i0,j1)/mu
 pv(345) = pv(345)+d(i0,i1)*d2(i1,i2)*d(i0,j0)*d(i0,j1)/mu
 pv(350) = pv(350)+d(i0,i1)*d(i0,i2)*d2(i0,j0)*d(i0,j1)/mu
 pv(351) = pv(351)+d(i0,i1)*d(i1,i2)*d2(i0,j0)*d(i0,j1)/mu
 pv(355) = pv(355)+d2(i0,i1)*d(i0,i2)*d(i1,j0)*d(i0,j1)/mu
 pv(356) = pv(356)+d(i0,i1)*d2(i0,i2)*d(i1,j0)*d(i0,j1)/mu
 pv(357) = pv(357)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i1,j0)*d(i0,j1)/mu
 pv(358) = pv(358)+d2(i0,i2)*d(i1,i2)*d(i1,j0)*d(i0,j1)/mu
 pv(364) = pv(364)+d(i0,i1)*d(i0,i2)*d(i0,j0)*d(i1,j0)*d(i0,j1)/mu
 pv(365) = pv(365)+d(i0,i1)*d(i1,i2)*d(i0,j0)*d(i1,j0)*d(i0,j1)/mu
 pv(366) = pv(366)+d(i0,i2)*d(i1,i2)*d(i0,j0)*d(i1,j0)*d(i0,j1)/mu
 pv(368) = pv(368)+d(i0,i2)*d2(i0,j0)*d(i1,j0)*d(i0,j1)/mu
 pv(369) = pv(369)+d(i1,i2)*d2(i0,j0)*d(i1,j0)*d(i0,j1)/mu
 pv(372) = pv(372)+d(i0,i1)*d(i0,i2)*d2(i1,j0)*d(i0,j1)/mu
 pv(373) = pv(373)+d(i0,i1)*d(i1,i2)*d2(i1,j0)*d(i0,j1)/mu
 pv(374) = pv(374)+d(i0,i2)*d(i1,i2)*d2(i1,j0)*d(i0,j1)/mu
 pv(378) = pv(378)+d(i0,i1)*d(i0,i2)*d(i1,j0)*d(i2,j0)*d(i0,j1)/mu
 pv(380) = pv(380)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i2,j0)*d(i0,j1)/mu
 pv(381) = pv(381)+d(i0,i1)*d2(i1,j0)*d(i2,j0)*d(i0,j1)/mu
 pv(410) = pv(410)+d2(i0,i1)*d(i0,i2)*d(i0,j0)*d(i0,k0)/mu
 pv(411) = pv(411)+d2(i0,i1)*d(i1,i2)*d(i0,j0)*d(i0,k0)/mu
 pv(412) = pv(412)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,j0)*d(i0,k0)/mu
 pv(413) = pv(413)+d(i0,i1)*d2(i1,i2)*d(i0,j0)*d(i0,k0)/mu
 pv(418) = pv(418)+d(i0,i1)*d(i0,i2)*d2(i0,j0)*d(i0,k0)/mu
 pv(419) = pv(419)+d(i0,i1)*d(i1,i2)*d2(i0,j0)*d(i0,k0)/mu
 pv(423) = pv(423)+d2(i0,i1)*d(i0,i2)*d(i1,j0)*d(i0,k0)/mu
 pv(424) = pv(424)+d(i0,i1)*d2(i0,i2)*d(i1,j0)*d(i0,k0)/mu
 pv(425) = pv(425)+d2(i0,i1)*d(i1,i2)*d(i1,j0)*d(i0,k0)/mu
 pv(426) = pv(426)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i1,j0)*d(i0,k0)/mu
 pv(427) = pv(427)+d2(i0,i2)*d(i1,i2)*d(i1,j0)*d(i0,k0)/mu
 pv(428) = pv(428)+d(i0,i1)*d2(i1,i2)*d(i1,j0)*d(i0,k0)/mu
 pv(429) = pv(429)+d(i0,i2)*d2(i1,i2)*d(i1,j0)*d(i0,k0)/mu
 pv(438) = pv(438)+d(i0,i1)*d(i0,i2)*d(i0,j0)*d(i1,j0)*d(i0,k0)/mu
 pv(439) = pv(439)+d2(i0,i2)*d(i0,j0)*d(i1,j0)*d(i0,k0)/mu
 pv(440) = pv(440)+d(i0,i1)*d(i1,i2)*d(i0,j0)*d(i1,j0)*d(i0,k0)/mu
 pv(441) = pv(441)+d(i0,i2)*d(i1,i2)*d(i0,j0)*d(i1,j0)*d(i0,k0)/mu
 pv(442) = pv(442)+d2(i1,i2)*d(i0,j0)*d(i1,j0)*d(i0,k0)/mu
 pv(449) = pv(449)+d(i0,i2)*d2(i0,j0)*d(i1,j0)*d(i0,k0)/mu
 pv(450) = pv(450)+d(i1,i2)*d2(i0,j0)*d(i1,j0)*d(i0,k0)/mu
 pv(453) = pv(453)+d(i0,i1)*d(i0,i2)*d2(i1,j0)*d(i0,k0)/mu
 pv(454) = pv(454)+d(i0,i1)*d(i1,i2)*d2(i1,j0)*d(i0,k0)/mu
 pv(455) = pv(455)+d(i0,i2)*d(i1,i2)*d2(i1,j0)*d(i0,k0)/mu
 pv(457) = pv(457)+d(i0,i2)*d(i0,j0)*d2(i1,j0)*d(i0,k0)/mu
 pv(458) = pv(458)+d(i1,i2)*d(i0,j0)*d2(i1,j0)*d(i0,k0)/mu
 pv(462) = pv(462)+d2(i0,i1)*d(i1,j0)*d(i2,j0)*d(i0,k0)/mu
 pv(463) = pv(463)+d(i0,i1)*d(i0,i2)*d(i1,j0)*d(i2,j0)*d(i0,k0)/mu
 pv(464) = pv(464)+d(i0,i1)*d(i1,i2)*d(i1,j0)*d(i2,j0)*d(i0,k0)/mu
 pv(468) = pv(468)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i2,j0)*d(i0,k0)/mu
 pv(469) = pv(469)+d(i1,i2)*d(i0,j0)*d(i1,j0)*d(i2,j0)*d(i0,k0)/mu
 pv(470) = pv(470)+d(i0,i1)*d2(i1,j0)*d(i2,j0)*d(i0,k0)/mu
 pv(471) = pv(471)+d(i0,i2)*d2(i1,j0)*d(i2,j0)*d(i0,k0)/mu
 pv(473) = pv(473)+d(i0,i1)*d(i0,i2)*d(i0,j0)*d(i0,j1)*d(i0,k0)/mu
 pv(474) = pv(474)+d(i0,i1)*d(i1,i2)*d(i0,j0)*d(i0,j1)*d(i0,k0)/mu
 pv(478) = pv(478)+d(i0,i1)*d(i0,i2)*d(i1,j0)*d(i0,j1)*d(i0,k0)/mu
 pv(479) = pv(479)+d(i0,i1)*d(i1,i2)*d(i1,j0)*d(i0,j1)*d(i0,k0)/mu
 pv(480) = pv(480)+d(i0,i2)*d(i1,i2)*d(i1,j0)*d(i0,j1)*d(i0,k0)/mu
 pv(484) = pv(484)+d(i0,i1)*d(i1,j0)*d(i2,j0)*d(i0,j1)*d(i0,k0)/mu
 pv(488) = pv(488)+d(i0,i1)*d(i0,i2)*d(i1,j0)*d(i1,j1)*d(i0,k0)/mu
 pv(489) = pv(489)+d(i0,i1)*d(i1,i2)*d(i1,j0)*d(i1,j1)*d(i0,k0)/mu
 pv(490) = pv(490)+d(i0,i2)*d(i1,i2)*d(i1,j0)*d(i1,j1)*d(i0,k0)/mu
 pv(494) = pv(494)+d(i0,i1)*d(i0,i2)*d(i2,j0)*d(i1,j1)*d(i0,k0)/mu
 pv(504) = pv(504)+d(i0,i1)*d(i0,i2)*d(i0,j0)*d2(i0,k0)/mu
 pv(505) = pv(505)+d(i0,i1)*d(i1,i2)*d(i0,j0)*d2(i0,k0)/mu
 pv(509) = pv(509)+d(i0,i1)*d(i0,i2)*d(i1,j0)*d2(i0,k0)/mu
 pv(510) = pv(510)+d(i0,i1)*d(i1,i2)*d(i1,j0)*d2(i0,k0)/mu
 pv(511) = pv(511)+d(i0,i2)*d(i1,i2)*d(i1,j0)*d2(i0,k0)/mu
 pv(513) = pv(513)+d(i0,i2)*d(i0,j0)*d(i1,j0)*d2(i0,k0)/mu
 pv(514) = pv(514)+d(i1,i2)*d(i0,j0)*d(i1,j0)*d2(i0,k0)/mu
 pv(518) = pv(518)+d(i0,i1)*d(i1,j0)*d(i2,j0)*d2(i0,k0)/mu
 pv(543) = pv(543)+d(i0,i1)*d(i0,i2)*d(i0,j0)*d(i0,k0)*d(i1,k0)/mu
 pv(544) = pv(544)+d(i0,i1)*d(i1,i2)*d(i0,j0)*d(i0,k0)*d(i1,k0)/mu
 pv(545) = pv(545)+d(i0,i2)*d(i1,i2)*d(i0,j0)*d(i0,k0)*d(i1,k0)/mu
 pv(548) = pv(548)+d(i0,i2)*d(i0,j0)*d(i1,j0)*d(i0,k0)*d(i1,k0)/mu
 pv(550) = pv(550)+d(i0,i1)*d(i0,i2)*d(i2,j0)*d(i0,k0)*d(i1,k0)/mu
 pv(551) = pv(551)+d(i0,i1)*d(i0,j0)*d(i2,j0)*d(i0,k0)*d(i1,k0)/mu
 pv(552) = pv(552)+d(i1,i2)*d(i0,j0)*d(i2,j0)*d(i0,k0)*d(i1,k0)/mu
 pv(565) = pv(565)+d2(i0,i1)*d(i0,i2)*d(i0,j0)*d(j0,k0)/mu
 pv(566) = pv(566)+d2(i0,i1)*d(i1,i2)*d(i0,j0)*d(j0,k0)/mu
 pv(567) = pv(567)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,j0)*d(j0,k0)/mu
 pv(568) = pv(568)+d(i0,i1)*d2(i1,i2)*d(i0,j0)*d(j0,k0)/mu
 pv(573) = pv(573)+d(i0,i1)*d(i0,i2)*d2(i0,j0)*d(j0,k0)/mu
 pv(574) = pv(574)+d(i0,i1)*d(i1,i2)*d2(i0,j0)*d(j0,k0)/mu
 pv(578) = pv(578)+d(i0,i1)*d(i0,i2)*d(i0,j0)*d(i1,j0)*d(j0,k0)/mu
 pv(579) = pv(579)+d(i0,i2)*d(i1,i2)*d(i0,j0)*d(i1,j0)*d(j0,k0)/mu
 pv(583) = pv(583)+d(i0,i1)*d(i0,i2)*d(i1,j0)*d(i0,j1)*d(j0,k0)/mu
 pv(587) = pv(587)+d(i0,i1)*d(i0,i2)*d(i0,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(588) = pv(588)+d(i0,i1)*d(i1,i2)*d(i0,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(592) = pv(592)+d(i0,i1)*d(i0,i2)*d(i1,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(593) = pv(593)+d(i0,i1)*d(i1,i2)*d(i1,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(594) = pv(594)+d(i0,i2)*d(i1,i2)*d(i1,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(628) = pv(628)+d2(i0,i1)*d(i0,i2)*d(i0,j0)*d(i0,l0)/mu
 pv(629) = pv(629)+d2(i0,i1)*d(i1,i2)*d(i0,j0)*d(i0,l0)/mu
 pv(630) = pv(630)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,j0)*d(i0,l0)/mu
 pv(631) = pv(631)+d(i0,i1)*d2(i1,i2)*d(i0,j0)*d(i0,l0)/mu
 pv(636) = pv(636)+d(i0,i1)*d(i0,i2)*d2(i0,j0)*d(i0,l0)/mu
 pv(637) = pv(637)+d(i0,i1)*d(i1,i2)*d2(i0,j0)*d(i0,l0)/mu
 pv(641) = pv(641)+d2(i0,i1)*d(i0,i2)*d(i1,j0)*d(i0,l0)/mu
 pv(642) = pv(642)+d(i0,i1)*d2(i0,i2)*d(i1,j0)*d(i0,l0)/mu
 pv(643) = pv(643)+d2(i0,i1)*d(i1,i2)*d(i1,j0)*d(i0,l0)/mu
 pv(644) = pv(644)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i1,j0)*d(i0,l0)/mu
 pv(645) = pv(645)+d2(i0,i2)*d(i1,i2)*d(i1,j0)*d(i0,l0)/mu
 pv(646) = pv(646)+d(i0,i1)*d2(i1,i2)*d(i1,j0)*d(i0,l0)/mu
 pv(647) = pv(647)+d(i0,i2)*d2(i1,i2)*d(i1,j0)*d(i0,l0)/mu
 pv(656) = pv(656)+d(i0,i1)*d(i0,i2)*d(i0,j0)*d(i1,j0)*d(i0,l0)/mu
 pv(657) = pv(657)+d2(i0,i2)*d(i0,j0)*d(i1,j0)*d(i0,l0)/mu
 pv(658) = pv(658)+d(i0,i1)*d(i1,i2)*d(i0,j0)*d(i1,j0)*d(i0,l0)/mu
 pv(659) = pv(659)+d(i0,i2)*d(i1,i2)*d(i0,j0)*d(i1,j0)*d(i0,l0)/mu
 pv(660) = pv(660)+d2(i1,i2)*d(i0,j0)*d(i1,j0)*d(i0,l0)/mu
 pv(667) = pv(667)+d(i0,i2)*d2(i0,j0)*d(i1,j0)*d(i0,l0)/mu
 pv(668) = pv(668)+d(i1,i2)*d2(i0,j0)*d(i1,j0)*d(i0,l0)/mu
 pv(671) = pv(671)+d(i0,i1)*d(i0,i2)*d2(i1,j0)*d(i0,l0)/mu
 pv(672) = pv(672)+d(i0,i1)*d(i1,i2)*d2(i1,j0)*d(i0,l0)/mu
 pv(673) = pv(673)+d(i0,i2)*d(i1,i2)*d2(i1,j0)*d(i0,l0)/mu
 pv(675) = pv(675)+d(i0,i2)*d(i0,j0)*d2(i1,j0)*d(i0,l0)/mu
 pv(676) = pv(676)+d(i1,i2)*d(i0,j0)*d2(i1,j0)*d(i0,l0)/mu
 pv(680) = pv(680)+d2(i0,i1)*d(i1,j0)*d(i2,j0)*d(i0,l0)/mu
 pv(681) = pv(681)+d(i0,i1)*d(i0,i2)*d(i1,j0)*d(i2,j0)*d(i0,l0)/mu
 pv(682) = pv(682)+d(i0,i1)*d(i1,i2)*d(i1,j0)*d(i2,j0)*d(i0,l0)/mu
 pv(686) = pv(686)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i2,j0)*d(i0,l0)/mu
 pv(687) = pv(687)+d(i1,i2)*d(i0,j0)*d(i1,j0)*d(i2,j0)*d(i0,l0)/mu
 pv(688) = pv(688)+d(i0,i1)*d2(i1,j0)*d(i2,j0)*d(i0,l0)/mu
 pv(689) = pv(689)+d(i0,i2)*d2(i1,j0)*d(i2,j0)*d(i0,l0)/mu
 pv(691) = pv(691)+d(i0,i1)*d(i0,i2)*d(i0,j0)*d(i0,j1)*d(i0,l0)/mu
 pv(692) = pv(692)+d(i0,i1)*d(i1,i2)*d(i0,j0)*d(i0,j1)*d(i0,l0)/mu
 pv(696) = pv(696)+d(i0,i1)*d(i0,i2)*d(i1,j0)*d(i0,j1)*d(i0,l0)/mu
 pv(697) = pv(697)+d(i0,i1)*d(i1,i2)*d(i1,j0)*d(i0,j1)*d(i0,l0)/mu
 pv(698) = pv(698)+d(i0,i2)*d(i1,i2)*d(i1,j0)*d(i0,j1)*d(i0,l0)/mu
 pv(702) = pv(702)+d(i0,i1)*d(i1,j0)*d(i2,j0)*d(i0,j1)*d(i0,l0)/mu
 pv(706) = pv(706)+d(i0,i1)*d(i0,i2)*d(i1,j0)*d(i1,j1)*d(i0,l0)/mu
 pv(707) = pv(707)+d(i0,i1)*d(i1,i2)*d(i1,j0)*d(i1,j1)*d(i0,l0)/mu
 pv(708) = pv(708)+d(i0,i2)*d(i1,i2)*d(i1,j0)*d(i1,j1)*d(i0,l0)/mu
 pv(712) = pv(712)+d(i0,i1)*d(i0,i2)*d(i2,j0)*d(i1,j1)*d(i0,l0)/mu
 pv(722) = pv(722)+d(i0,i1)*d(i0,i2)*d(i0,j0)*d(i0,k0)*d(i0,l0)/mu
 pv(723) = pv(723)+d(i0,i1)*d(i1,i2)*d(i0,j0)*d(i0,k0)*d(i0,l0)/mu
 pv(727) = pv(727)+d(i0,i1)*d(i0,i2)*d(i1,j0)*d(i0,k0)*d(i0,l0)/mu
 pv(728) = pv(728)+d(i0,i1)*d(i1,i2)*d(i1,j0)*d(i0,k0)*d(i0,l0)/mu
 pv(729) = pv(729)+d(i0,i2)*d(i1,i2)*d(i1,j0)*d(i0,k0)*d(i0,l0)/mu
 pv(731) = pv(731)+d(i0,i2)*d(i0,j0)*d(i1,j0)*d(i0,k0)*d(i0,l0)/mu
 pv(732) = pv(732)+d(i1,i2)*d(i0,j0)*d(i1,j0)*d(i0,k0)*d(i0,l0)/mu
 pv(736) = pv(736)+d(i0,i1)*d(i1,j0)*d(i2,j0)*d(i0,k0)*d(i0,l0)/mu
 pv(768) = pv(768)+d(i0,i1)*d(i0,i2)*d(i0,j0)*d(i1,k0)*d(i0,l0)/mu
 pv(769) = pv(769)+d(i0,i1)*d(i1,i2)*d(i0,j0)*d(i1,k0)*d(i0,l0)/mu
 pv(770) = pv(770)+d(i0,i2)*d(i1,i2)*d(i0,j0)*d(i1,k0)*d(i0,l0)/mu
 pv(773) = pv(773)+d(i0,i1)*d(i0,i2)*d(i1,j0)*d(i1,k0)*d(i0,l0)/mu
 pv(774) = pv(774)+d(i0,i1)*d(i1,i2)*d(i1,j0)*d(i1,k0)*d(i0,l0)/mu
 pv(775) = pv(775)+d(i0,i2)*d(i1,i2)*d(i1,j0)*d(i1,k0)*d(i0,l0)/mu
 pv(777) = pv(777)+d(i0,i2)*d(i0,j0)*d(i1,j0)*d(i1,k0)*d(i0,l0)/mu
 pv(778) = pv(778)+d(i1,i2)*d(i0,j0)*d(i1,j0)*d(i1,k0)*d(i0,l0)/mu
 pv(782) = pv(782)+d(i0,i1)*d(i0,i2)*d(i2,j0)*d(i1,k0)*d(i0,l0)/mu
 pv(783) = pv(783)+d(i0,i1)*d(i1,i2)*d(i2,j0)*d(i1,k0)*d(i0,l0)/mu
 pv(784) = pv(784)+d(i0,i1)*d(i0,j0)*d(i2,j0)*d(i1,k0)*d(i0,l0)/mu
 pv(785) = pv(785)+d(i1,i2)*d(i0,j0)*d(i2,j0)*d(i1,k0)*d(i0,l0)/mu
 pv(786) = pv(786)+d(i0,i1)*d(i1,j0)*d(i2,j0)*d(i1,k0)*d(i0,l0)/mu
 pv(787) = pv(787)+d(i0,i2)*d(i1,j0)*d(i2,j0)*d(i1,k0)*d(i0,l0)/mu
 pv(810) = pv(810)+d(i0,i1)*d(i0,i2)*d(i0,j0)*d(j0,k0)*d(i0,l0)/mu
 pv(811) = pv(811)+d(i0,i1)*d(i1,i2)*d(i0,j0)*d(j0,k0)*d(i0,l0)/mu
 pv(815) = pv(815)+d(i0,i1)*d(i0,i2)*d(i1,j0)*d(j0,k0)*d(i0,l0)/mu
 pv(816) = pv(816)+d(i0,i1)*d(i1,i2)*d(i1,j0)*d(j0,k0)*d(i0,l0)/mu
 pv(817) = pv(817)+d(i0,i2)*d(i1,i2)*d(i1,j0)*d(j0,k0)*d(i0,l0)/mu
 pv(837) = pv(837)+d(i0,i1)*d(i0,i2)*d(i0,j0)*d2(i0,l0)/mu
 pv(838) = pv(838)+d(i0,i1)*d(i1,i2)*d(i0,j0)*d2(i0,l0)/mu
 pv(842) = pv(842)+d(i0,i1)*d(i0,i2)*d(i1,j0)*d2(i0,l0)/mu
 pv(843) = pv(843)+d(i0,i1)*d(i1,i2)*d(i1,j0)*d2(i0,l0)/mu
 pv(844) = pv(844)+d(i0,i2)*d(i1,i2)*d(i1,j0)*d2(i0,l0)/mu
 pv(846) = pv(846)+d(i0,i2)*d(i0,j0)*d(i1,j0)*d2(i0,l0)/mu
 pv(847) = pv(847)+d(i1,i2)*d(i0,j0)*d(i1,j0)*d2(i0,l0)/mu
 pv(851) = pv(851)+d(i0,i1)*d(i1,j0)*d(i2,j0)*d2(i0,l0)/mu
 pv(906) = pv(906)+d(i0,i1)*d(i0,i2)*d(i0,j0)*d(i0,l0)*d(i1,l0)/mu
 pv(907) = pv(907)+d(i0,i1)*d(i1,i2)*d(i0,j0)*d(i0,l0)*d(i1,l0)/mu
 pv(908) = pv(908)+d(i0,i2)*d(i1,i2)*d(i0,j0)*d(i0,l0)*d(i1,l0)/mu
 pv(911) = pv(911)+d(i0,i2)*d(i0,j0)*d(i1,j0)*d(i0,l0)*d(i1,l0)/mu
 pv(913) = pv(913)+d(i0,i1)*d(i0,i2)*d(i2,j0)*d(i0,l0)*d(i1,l0)/mu
 pv(914) = pv(914)+d(i0,i1)*d(i0,j0)*d(i2,j0)*d(i0,l0)*d(i1,l0)/mu
 pv(915) = pv(915)+d(i1,i2)*d(i0,j0)*d(i2,j0)*d(i0,l0)*d(i1,l0)/mu
 pv(941) = pv(941)+d2(i0,i1)*d(i0,i2)*d(i0,j0)*d(j0,l0)/mu
 pv(942) = pv(942)+d2(i0,i1)*d(i1,i2)*d(i0,j0)*d(j0,l0)/mu
 pv(943) = pv(943)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,j0)*d(j0,l0)/mu
 pv(944) = pv(944)+d(i0,i1)*d2(i1,i2)*d(i0,j0)*d(j0,l0)/mu
 pv(949) = pv(949)+d(i0,i1)*d(i0,i2)*d2(i0,j0)*d(j0,l0)/mu
 pv(950) = pv(950)+d(i0,i1)*d(i1,i2)*d2(i0,j0)*d(j0,l0)/mu
 pv(954) = pv(954)+d(i0,i1)*d(i0,i2)*d(i0,j0)*d(i1,j0)*d(j0,l0)/mu
 pv(955) = pv(955)+d(i0,i2)*d(i1,i2)*d(i0,j0)*d(i1,j0)*d(j0,l0)/mu
 pv(959) = pv(959)+d(i0,i1)*d(i0,i2)*d(i1,j0)*d(i0,j1)*d(j0,l0)/mu
 pv(963) = pv(963)+d(i0,i1)*d(i0,i2)*d(i0,j0)*d(i0,k0)*d(j0,l0)/mu
 pv(964) = pv(964)+d(i0,i1)*d(i1,i2)*d(i0,j0)*d(i0,k0)*d(j0,l0)/mu
 pv(968) = pv(968)+d(i0,i1)*d(i0,i2)*d(i1,j0)*d(i0,k0)*d(j0,l0)/mu
 pv(969) = pv(969)+d(i0,i1)*d(i1,i2)*d(i1,j0)*d(i0,k0)*d(j0,l0)/mu
 pv(970) = pv(970)+d(i0,i2)*d(i1,i2)*d(i1,j0)*d(i0,k0)*d(j0,l0)/mu
 pv(981) = pv(981)+d(i0,i1)*d(i0,i2)*d(i0,j0)*d(i0,l0)*d(j0,l0)/mu
 pv(982) = pv(982)+d(i0,i1)*d(i1,i2)*d(i0,j0)*d(i0,l0)*d(j0,l0)/mu
 pv(986) = pv(986)+d(i0,i1)*d(i0,i2)*d(i1,j0)*d(i0,l0)*d(j0,l0)/mu
 pv(987) = pv(987)+d(i0,i1)*d(i1,i2)*d(i1,j0)*d(i0,l0)*d(j0,l0)/mu
 pv(988) = pv(988)+d(i0,i2)*d(i1,i2)*d(i1,j0)*d(i0,l0)*d(j0,l0)/mu
endif
END SUBROUTINE mg6211_i2jj
SUBROUTINE mg6211_i3jj (mxd, mu)
integer, intent (in) :: mxd, mu
if (4.le.mxd) then
 pv(66) = pv(66)+d(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,j0)/mu
 pv(67) = pv(67)+d(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,j0)/mu
 pv(68) = pv(68)+d(i0,i1)*d(i1,i2)*d(i1,i3)*d(i0,j0)/mu
 pv(77) = pv(77)+d(i0,i2)*d(i0,i3)*d(i0,j0)*d(i1,j0)/mu
 pv(78) = pv(78)+d(i1,i2)*d(i0,i3)*d(i0,j0)*d(i1,j0)/mu
 pv(79) = pv(79)+d(i0,i2)*d(i2,i3)*d(i0,j0)*d(i1,j0)/mu
endif
if (5.le.mxd) then
 pv(269) = pv(269)+d2(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,j0)/mu
 pv(270) = pv(270)+d2(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,j0)/mu
 pv(271) = pv(271)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i0,j0)/mu
 pv(272) = pv(272)+d(i0,i1)*d2(i1,i2)*d(i0,i3)*d(i0,j0)/mu
 pv(273) = pv(273)+d(i0,i1)*d(i1,i2)*d2(i0,i3)*d(i0,j0)/mu
 pv(274) = pv(274)+d2(i0,i1)*d(i1,i2)*d(i1,i3)*d(i0,j0)/mu
 pv(275) = pv(275)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,j0)/mu
 pv(276) = pv(276)+d2(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,j0)/mu
 pv(277) = pv(277)+d(i0,i1)*d2(i1,i2)*d(i1,i3)*d(i0,j0)/mu
 pv(278) = pv(278)+d(i0,i2)*d2(i1,i2)*d(i1,i3)*d(i0,j0)/mu
 pv(279) = pv(279)+d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,j0)/mu
 pv(280) = pv(280)+d(i0,i1)*d(i1,i2)*d(i1,i3)*d(i2,i3)*d(i0,j0)/mu
 pv(289) = pv(289)+d(i0,i1)*d(i0,i2)*d(i0,i3)*d2(i0,j0)/mu
 pv(290) = pv(290)+d(i0,i1)*d(i1,i2)*d(i0,i3)*d2(i0,j0)/mu
 pv(291) = pv(291)+d(i0,i1)*d(i1,i2)*d(i1,i3)*d2(i0,j0)/mu
 pv(303) = pv(303)+d(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,j0)*d(i1,j0)/mu
 pv(304) = pv(304)+d2(i0,i2)*d(i0,i3)*d(i0,j0)*d(i1,j0)/mu
 pv(305) = pv(305)+d(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,j0)*d(i1,j0)/mu
 pv(306) = pv(306)+d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i0,j0)*d(i1,j0)/mu
 pv(307) = pv(307)+d2(i1,i2)*d(i0,i3)*d(i0,j0)*d(i1,j0)/mu
 pv(308) = pv(308)+d(i0,i1)*d(i0,i2)*d(i2,i3)*d(i0,j0)*d(i1,j0)/mu
 pv(309) = pv(309)+d2(i0,i2)*d(i2,i3)*d(i0,j0)*d(i1,j0)/mu
 pv(310) = pv(310)+d(i0,i2)*d(i1,i2)*d(i2,i3)*d(i0,j0)*d(i1,j0)/mu
 pv(311) = pv(311)+d(i0,i2)*d(i0,i3)*d(i2,i3)*d(i0,j0)*d(i1,j0)/mu
 pv(312) = pv(312)+d(i1,i2)*d(i0,i3)*d(i2,i3)*d(i0,j0)*d(i1,j0)/mu
 pv(313) = pv(313)+d(i0,i2)*d2(i2,i3)*d(i0,j0)*d(i1,j0)/mu
 pv(323) = pv(323)+d(i0,i2)*d(i0,i3)*d2(i0,j0)*d(i1,j0)/mu
 pv(324) = pv(324)+d(i1,i2)*d(i0,i3)*d2(i0,j0)*d(i1,j0)/mu
 pv(325) = pv(325)+d(i1,i2)*d(i1,i3)*d2(i0,j0)*d(i1,j0)/mu
 pv(326) = pv(326)+d(i0,i2)*d(i2,i3)*d2(i0,j0)*d(i1,j0)/mu
 pv(327) = pv(327)+d(i1,i2)*d(i2,i3)*d2(i0,j0)*d(i1,j0)/mu
 pv(337) = pv(337)+d(i0,i1)*d(i0,i3)*d(i0,j0)*d(i1,j0)*d(i2,j0)/mu
 pv(338) = pv(338)+d(i1,i2)*d(i0,i3)*d(i0,j0)*d(i1,j0)*d(i2,j0)/mu
 pv(346) = pv(346)+d(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,j0)*d(i0,j1)/mu
 pv(347) = pv(347)+d(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,j0)*d(i0,j1)/mu
 pv(348) = pv(348)+d(i0,i1)*d(i1,i2)*d(i1,i3)*d(i0,j0)*d(i0,j1)/mu
 pv(359) = pv(359)+d(i0,i1)*d(i0,i2)*d(i0,i3)*d(i1,j0)*d(i0,j1)/mu
 pv(360) = pv(360)+d(i0,i1)*d(i1,i2)*d(i0,i3)*d(i1,j0)*d(i0,j1)/mu
 pv(361) = pv(361)+d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,j0)*d(i0,j1)/mu
 pv(362) = pv(362)+d(i0,i1)*d(i0,i2)*d(i2,i3)*d(i1,j0)*d(i0,j1)/mu
 pv(379) = pv(379)+d(i0,i1)*d(i0,i3)*d(i1,j0)*d(i2,j0)*d(i0,j1)/mu
 pv(414) = pv(414)+d(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,j0)*d(i0,k0)/mu
 pv(415) = pv(415)+d(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,j0)*d(i0,k0)/mu
 pv(416) = pv(416)+d(i0,i1)*d(i1,i2)*d(i1,i3)*d(i0,j0)*d(i0,k0)/mu
 pv(430) = pv(430)+d(i0,i1)*d(i0,i2)*d(i0,i3)*d(i1,j0)*d(i0,k0)/mu
 pv(431) = pv(431)+d(i0,i1)*d(i1,i2)*d(i0,i3)*d(i1,j0)*d(i0,k0)/mu
 pv(432) = pv(432)+d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,j0)*d(i0,k0)/mu
 pv(433) = pv(433)+d(i0,i1)*d(i1,i2)*d(i1,i3)*d(i1,j0)*d(i0,k0)/mu
 pv(434) = pv(434)+d(i0,i2)*d(i1,i2)*d(i1,i3)*d(i1,j0)*d(i0,k0)/mu
 pv(435) = pv(435)+d(i0,i1)*d(i0,i2)*d(i2,i3)*d(i1,j0)*d(i0,k0)/mu
 pv(436) = pv(436)+d(i0,i1)*d(i1,i2)*d(i2,i3)*d(i1,j0)*d(i0,k0)/mu
 pv(443) = pv(443)+d(i0,i2)*d(i0,i3)*d(i0,j0)*d(i1,j0)*d(i0,k0)/mu
 pv(444) = pv(444)+d(i1,i2)*d(i0,i3)*d(i0,j0)*d(i1,j0)*d(i0,k0)/mu
 pv(445) = pv(445)+d(i1,i2)*d(i1,i3)*d(i0,j0)*d(i1,j0)*d(i0,k0)/mu
 pv(446) = pv(446)+d(i0,i2)*d(i2,i3)*d(i0,j0)*d(i1,j0)*d(i0,k0)/mu
 pv(447) = pv(447)+d(i1,i2)*d(i2,i3)*d(i0,j0)*d(i1,j0)*d(i0,k0)/mu
 pv(465) = pv(465)+d(i0,i1)*d(i0,i3)*d(i1,j0)*d(i2,j0)*d(i0,k0)/mu
 pv(466) = pv(466)+d(i0,i1)*d(i1,i3)*d(i1,j0)*d(i2,j0)*d(i0,k0)/mu
 pv(467) = pv(467)+d(i0,i2)*d(i1,i3)*d(i1,j0)*d(i2,j0)*d(i0,k0)/mu
 pv(569) = pv(569)+d(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,j0)*d(j0,k0)/mu
 pv(570) = pv(570)+d(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,j0)*d(j0,k0)/mu
 pv(571) = pv(571)+d(i0,i1)*d(i1,i2)*d(i1,i3)*d(i0,j0)*d(j0,k0)/mu
 pv(632) = pv(632)+d(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,j0)*d(i0,l0)/mu
 pv(633) = pv(633)+d(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,j0)*d(i0,l0)/mu
 pv(634) = pv(634)+d(i0,i1)*d(i1,i2)*d(i1,i3)*d(i0,j0)*d(i0,l0)/mu
 pv(648) = pv(648)+d(i0,i1)*d(i0,i2)*d(i0,i3)*d(i1,j0)*d(i0,l0)/mu
 pv(649) = pv(649)+d(i0,i1)*d(i1,i2)*d(i0,i3)*d(i1,j0)*d(i0,l0)/mu
 pv(650) = pv(650)+d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,j0)*d(i0,l0)/mu
 pv(651) = pv(651)+d(i0,i1)*d(i1,i2)*d(i1,i3)*d(i1,j0)*d(i0,l0)/mu
 pv(652) = pv(652)+d(i0,i2)*d(i1,i2)*d(i1,i3)*d(i1,j0)*d(i0,l0)/mu
 pv(653) = pv(653)+d(i0,i1)*d(i0,i2)*d(i2,i3)*d(i1,j0)*d(i0,l0)/mu
 pv(654) = pv(654)+d(i0,i1)*d(i1,i2)*d(i2,i3)*d(i1,j0)*d(i0,l0)/mu
 pv(661) = pv(661)+d(i0,i2)*d(i0,i3)*d(i0,j0)*d(i1,j0)*d(i0,l0)/mu
 pv(662) = pv(662)+d(i1,i2)*d(i0,i3)*d(i0,j0)*d(i1,j0)*d(i0,l0)/mu
 pv(663) = pv(663)+d(i1,i2)*d(i1,i3)*d(i0,j0)*d(i1,j0)*d(i0,l0)/mu
 pv(664) = pv(664)+d(i0,i2)*d(i2,i3)*d(i0,j0)*d(i1,j0)*d(i0,l0)/mu
 pv(665) = pv(665)+d(i1,i2)*d(i2,i3)*d(i0,j0)*d(i1,j0)*d(i0,l0)/mu
 pv(683) = pv(683)+d(i0,i1)*d(i0,i3)*d(i1,j0)*d(i2,j0)*d(i0,l0)/mu
 pv(684) = pv(684)+d(i0,i1)*d(i1,i3)*d(i1,j0)*d(i2,j0)*d(i0,l0)/mu
 pv(685) = pv(685)+d(i0,i2)*d(i1,i3)*d(i1,j0)*d(i2,j0)*d(i0,l0)/mu
 pv(945) = pv(945)+d(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,j0)*d(j0,l0)/mu
 pv(946) = pv(946)+d(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,j0)*d(j0,l0)/mu
 pv(947) = pv(947)+d(i0,i1)*d(i1,i2)*d(i1,i3)*d(i0,j0)*d(j0,l0)/mu
endif
END SUBROUTINE mg6211_i3jj
SUBROUTINE mg6211_iijj (mxd, mu)
integer, intent (in) :: mxd, mu
if (5.le.mxd) then
 pv(281) = pv(281)+d(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,i4)*d(i0,j0)/mu
 pv(282) = pv(282)+d(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,i4)*d(i0,j0)/mu
 pv(283) = pv(283)+d(i0,i2)*d(i1,i2)*d(i1,i3)*d(i1,i4)*d(i0,j0)/mu
 pv(314) = pv(314)+d(i0,i2)*d(i0,i3)*d(i0,i4)*d(i0,j0)*d(i1,j0)/mu
 pv(315) = pv(315)+d(i1,i2)*d(i0,i3)*d(i0,i4)*d(i0,j0)*d(i1,j0)/mu
 pv(316) = pv(316)+d(i0,i2)*d(i2,i3)*d(i0,i4)*d(i0,j0)*d(i1,j0)/mu
endif
END SUBROUTINE mg6211_iijj
END SUBROUTINE mg6211_isecs
