SUBROUTINE mg321_isecs (mxd, r, pv)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: pv(0:)
!-----------------------------------------------------------------------
integer, parameter :: m=3, n=2, mm=m*(m-1), nn=n
integer :: i0, i1, j0, j1, k0
real (kind=wp), dimension (0:nk-1,0:nk-1) :: d, d2, d3, d4, d5
if (size(r).ne.nk*nk) then
 stop 'mg321_isecs: bad dimensions'
endif
call mgx_setd (r, d, d2, d3, d4, d5)
pv = 0
k0 = m+n
do j0 = m, m+n-1
 do j1 = m, m+n-1
  if (j1.ne.j0) then
   do i0 = 0, m-1
    call mg321_i0jj (mxd, m*nn)
    do i1 = 0, m-1
     if (i1.ne.i0) then
      call mg321_iijj (mxd, mm*nn)
     endif
    enddo
   enddo
  endif
 enddo
enddo
return
CONTAINS
SUBROUTINE mg321_i0jj (mxd, mu)
integer, intent (in) :: mxd, mu
if (2.le.mxd) then
 pv(2) = pv(2)+d(i0,j0)*d(i0,k0)/mu
 pv(3) = pv(3)+d(i0,j0)*d(j0,k0)/mu
endif
if (3.le.mxd) then
 pv(6) = pv(6)+d3(i0,j0)/mu
 pv(12) = pv(12)+d2(i0,j0)*d(i0,k0)/mu
 pv(14) = pv(14)+d(i0,j0)*d(i0,j1)*d(i0,k0)/mu
 pv(16) = pv(16)+d(i0,j0)*d2(i0,k0)/mu
 pv(18) = pv(18)+d2(i0,j0)*d(j0,k0)/mu
 pv(19) = pv(19)+d(i0,j0)*d(i0,k0)*d(j0,k0)/mu
endif
if (4.le.mxd) then
 pv(22) = pv(22)+d4(i0,j0)/mu
 pv(26) = pv(26)+d3(i0,j0)*d(i0,j1)/mu
 pv(28) = pv(28)+d3(i0,j0)*d(i0,k0)/mu
 pv(31) = pv(31)+d2(i0,j0)*d2(i0,k0)/mu
 pv(35) = pv(35)+d3(i0,j0)*d(j0,k0)/mu
 pv(36) = pv(36)+d2(i0,j0)*d(i0,j1)*d(j0,k0)/mu
 pv(38) = pv(38)+d2(i0,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(39) = pv(39)+d(i0,j0)*d2(i0,k0)*d(j0,k0)/mu
endif
if (5.le.mxd) then
 pv(41) = pv(41)+d5(i0,j0)/mu
 pv(42) = pv(42)+d4(i0,j0)*d(i0,k0)/mu
endif
END SUBROUTINE mg321_i0jj
SUBROUTINE mg321_iijj (mxd, mu)
integer, intent (in) :: mxd, mu
if (2.le.mxd) then
 pv(0) = pv(0)+d(i0,i1)*d(i0,j0)/mu
 pv(1) = pv(1)+d(i0,i1)*d(i0,k0)/mu
endif
if (3.le.mxd) then
 pv(4) = pv(4)+d2(i0,i1)*d(i0,j0)/mu
 pv(5) = pv(5)+d(i0,i1)*d2(i0,j0)/mu
 pv(7) = pv(7)+d(i0,i1)*d(i0,j0)*d(i1,j0)/mu
 pv(8) = pv(8)+d2(i0,j0)*d(i1,j0)/mu
 pv(9) = pv(9)+d(i0,i1)*d(i0,j0)*d(i0,j1)/mu
 pv(10) = pv(10)+d2(i0,i1)*d(i0,k0)/mu
 pv(11) = pv(11)+d(i0,i1)*d(i0,j0)*d(i0,k0)/mu
 pv(13) = pv(13)+d(i0,j0)*d(i1,j0)*d(i0,k0)/mu
 pv(15) = pv(15)+d(i0,i1)*d2(i0,k0)/mu
 pv(17) = pv(17)+d(i0,i1)*d(i0,j0)*d(j0,k0)/mu
endif
if (4.le.mxd) then
 pv(20) = pv(20)+d2(i0,i1)*d2(i0,j0)/mu
 pv(21) = pv(21)+d(i0,i1)*d3(i0,j0)/mu
 pv(23) = pv(23)+d2(i0,i1)*d(i0,j0)*d(i1,j0)/mu
 pv(24) = pv(24)+d(i0,i1)*d2(i0,j0)*d(i1,j0)/mu
 pv(25) = pv(25)+d3(i0,j0)*d(i1,j0)/mu
 pv(27) = pv(27)+d(i0,i1)*d2(i0,j0)*d(i0,k0)/mu
 pv(29) = pv(29)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,k0)/mu
 pv(30) = pv(30)+d2(i0,j0)*d(i1,j0)*d(i0,k0)/mu
 pv(32) = pv(32)+d(i0,j0)*d(i1,j0)*d2(i0,k0)/mu
 pv(33) = pv(33)+d2(i0,i1)*d(i0,j0)*d(j0,k0)/mu
 pv(34) = pv(34)+d(i0,i1)*d2(i0,j0)*d(j0,k0)/mu
 pv(37) = pv(37)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d(j0,k0)/mu
endif
if (5.le.mxd) then
 pv(40) = pv(40)+d(i0,i1)*d4(i0,j0)/mu
endif
END SUBROUTINE mg321_iijj
END SUBROUTINE mg321_isecs