SUBROUTINE mg2221_isecs (mxd, r, pv)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: pv(0:)
!-----------------------------------------------------------------------
integer, parameter :: m=2, n=2, l=2, mm=m, nn=n, ll=l
integer :: i0, i1, j0, j1, k0, k1, l0
real (kind=wp), dimension (0:nk-1,0:nk-1) :: d, d2, d3, d4, d5, d6
if (size(r).ne.nk*nk) then
 stop 'mg2221_isecs: bad dimensions'
endif
call mgx_setd (r, d, d2, d3, d4, d5, d6)
pv = 0
l0 = m+n+l
do k0 = m+n, m+n+l-1
 do k1 = m+n, m+n+l-1
  if (k1.ne.k0) then
   do j0 = m, m+n-1
    do j1 = m, m+n-1
     if (j1.ne.j0) then
      do i0 = 0, m-1
       do i1 = 0, m-1
        if (i1.ne.i0) then
         call mg2221_all (mxd, mm*nn*ll)
        endif
       enddo
      enddo
     endif
    enddo
   enddo
  endif
 enddo
enddo
return
CONTAINS
SUBROUTINE mg2221_all (mxd, mu)
integer, intent (in) :: mxd, mu
if (2.le.mxd) then
 pv(0) = pv(0)+d(i0,j0)*d(i0,k0)/mu
 pv(1) = pv(1)+d(i0,j0)*d(j0,k0)/mu
 pv(2) = pv(2)+d(i0,k0)*d(j0,k0)/mu
 pv(3) = pv(3)+d(i0,j0)*d(i0,l0)/mu
 pv(4) = pv(4)+d(i0,k0)*d(i0,l0)/mu
 pv(5) = pv(5)+d(i0,j0)*d(j0,l0)/mu
 pv(6) = pv(6)+d(j0,k0)*d(j0,l0)/mu
 pv(7) = pv(7)+d(i0,k0)*d(k0,l0)/mu
 pv(8) = pv(8)+d(j0,k0)*d(k0,l0)/mu
endif
if (3.le.mxd) then
 pv(9) = pv(9)+d3(i0,j0)/mu
 pv(10) = pv(10)+d2(i0,j0)*d(i0,k0)/mu
 pv(11) = pv(11)+d(i0,j0)*d2(i0,k0)/mu
 pv(12) = pv(12)+d3(i0,k0)/mu
 pv(13) = pv(13)+d2(i0,j0)*d(j0,k0)/mu
 pv(14) = pv(14)+d(i0,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(15) = pv(15)+d(i1,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(16) = pv(16)+d(i0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(17) = pv(17)+d2(i0,k0)*d(j0,k0)/mu
 pv(18) = pv(18)+d(i0,j0)*d(i0,k1)*d(j0,k0)/mu
 pv(19) = pv(19)+d(i0,j0)*d2(j0,k0)/mu
 pv(20) = pv(20)+d(i0,k0)*d2(j0,k0)/mu
 pv(21) = pv(21)+d3(j0,k0)/mu
 pv(22) = pv(22)+d2(i0,j0)*d(i0,l0)/mu
 pv(23) = pv(23)+d2(i0,k0)*d(i0,l0)/mu
 pv(24) = pv(24)+d(i0,j0)*d(j0,k0)*d(i0,l0)/mu
 pv(25) = pv(25)+d(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(26) = pv(26)+d2(i0,j0)*d(j0,l0)/mu
 pv(27) = pv(27)+d(i0,j0)*d(i0,k0)*d(j0,l0)/mu
 pv(28) = pv(28)+d(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(29) = pv(29)+d2(j0,k0)*d(j0,l0)/mu
 pv(30) = pv(30)+d(i0,j0)*d(i0,l0)*d(j0,l0)/mu
 pv(31) = pv(31)+d(i0,j0)*d(i0,k0)*d(k0,l0)/mu
 pv(32) = pv(32)+d2(i0,k0)*d(k0,l0)/mu
 pv(33) = pv(33)+d(i0,j0)*d(j0,k0)*d(k0,l0)/mu
 pv(34) = pv(34)+d2(j0,k0)*d(k0,l0)/mu
 pv(35) = pv(35)+d(i0,k0)*d(i0,l0)*d(k0,l0)/mu
 pv(36) = pv(36)+d(j0,k0)*d(j0,l0)*d(k0,l0)/mu
endif
if (4.le.mxd) then
 pv(37) = pv(37)+d2(i0,j0)*d2(i0,k0)/mu
 pv(38) = pv(38)+d2(i0,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(39) = pv(39)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(40) = pv(40)+d(i0,j0)*d(i0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(41) = pv(41)+d(i0,j0)*d2(i0,k0)*d(j0,k0)/mu
 pv(42) = pv(42)+d(i1,j0)*d2(i0,k0)*d(j0,k0)/mu
 pv(43) = pv(43)+d(i0,j0)*d(i0,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(44) = pv(44)+d2(i0,j0)*d2(j0,k0)/mu
 pv(45) = pv(45)+d(i0,j0)*d(i0,k0)*d2(j0,k0)/mu
 pv(46) = pv(46)+d(i0,j1)*d(i0,k0)*d2(j0,k0)/mu
 pv(47) = pv(47)+d2(i0,k0)*d2(j0,k0)/mu
 pv(48) = pv(48)+d(i0,j0)*d(i0,k1)*d2(j0,k0)/mu
 pv(49) = pv(49)+d(i0,j0)*d(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(50) = pv(50)+d(i1,j0)*d(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(51) = pv(51)+d(i0,j0)*d2(j0,k0)*d(i0,l0)/mu
 pv(52) = pv(52)+d(i0,k0)*d2(j0,k0)*d(i0,l0)/mu
 pv(53) = pv(53)+d(i0,j0)*d2(i0,k0)*d(j0,l0)/mu
 pv(54) = pv(54)+d(i0,j0)*d(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(55) = pv(55)+d(i0,j1)*d(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(56) = pv(56)+d2(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(57) = pv(57)+d(i0,k0)*d(j0,k0)*d(i0,l0)*d(j0,l0)/mu
 pv(58) = pv(58)+d2(i0,j0)*d(i0,k0)*d(k0,l0)/mu
 pv(59) = pv(59)+d2(i0,j0)*d(j0,k0)*d(k0,l0)/mu
 pv(60) = pv(60)+d(i0,j0)*d(i0,k0)*d(j0,k0)*d(k0,l0)/mu
 pv(61) = pv(61)+d(i0,j0)*d(i0,k1)*d(j0,k0)*d(k0,l0)/mu
 pv(62) = pv(62)+d(i0,j0)*d(j0,k0)*d(i0,l0)*d(k0,l0)/mu
 pv(63) = pv(63)+d(i0,j0)*d(i0,k0)*d(j0,l0)*d(k0,l0)/mu
endif
if (5.le.mxd) then
! No irreducible secondaries at this degree
endif
if (6.le.mxd) then
! No irreducible secondaries at this degree
endif
END SUBROUTINE mg2221_all
END SUBROUTINE mg2221_isecs
