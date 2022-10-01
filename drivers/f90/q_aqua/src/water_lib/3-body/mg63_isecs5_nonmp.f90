SUBROUTINE mg63_isecs5 (mxd, r, pv)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: pv(0:)
!-----------------------------------------------------------------------
integer, parameter :: m=6, n=3, m2=m*(m-1), m3=m2*(m-2), m4=m3*(m-3), &
  mm=m4*(m-4), nn=n*(n-1)
integer :: i0, i1, i2, i3, i4, i5, j0, j1, j2, mu,i
real (kind=wp), dimension (0:nk-1,0:nk-1) :: d, d2, d3, d4, d5, d6
real (kind=wp)::vi2(5),pvi2(5),vi3(17),pvi3(17),vii(4),pvii(4),&
     vi1j0(21),vi2j0(48),vi3j0(45),viij0(8),&
     pvi1j0(21),pvi2j0(48),pvi3j0(45),pviij0(8),&
     vi0jj(22),vi1jj(79),vi2jj(78),vi3jj(24),&
     pvi0jj(22),pvi1jj(79),pvi2jj(78),pvi3jj(24)

if (size(r).ne.nk*nk) then
 stop 'mg63_isecs: bad dimensions'
endif
call mgx_setd (r, d, d2, d3, d4, d5, d6)
pv = 0


pvi2 = 0
pvi3 = 0
pvii = 0
pvi1j0 = 0
pvi2j0 = 0
pvi3j0 = 0
pviij0 = 0
pvi0jj = 0
pvi1jj = 0
pvi2jj = 0
pvi3jj = 0

do i=0,719,6   
   i0=mg63_mii(0,i)
   i1=mg63_mii(1,i)
   i2=mg63_mii(2,i)
   vi2(1) = d2(i0,i1)*d2(i0,i2)
   vi2(2) = d2(i0,i1)*d(i0,i2)*d(i1,i2)
   vi2(3) = d3(i0,i1)*d2(i0,i2)
   vi2(4) = d3(i0,i1)*d(i0,i2)*d(i1,i2)
   vi2(5) = d2(i0,i1)*d2(i0,i2)*d(i1,i2)
   pvi2=pvi2+vi2
end do

do i=0,719,2
   i0=mg63_mii(0,i)
   i1=mg63_mii(1,i)
   i2=mg63_mii(2,i)
   i3=mg63_mii(3,i)
   vi3(1)  = d(i0,i1)*d(i1,i2)*d(i0,i3)
   vi3(2)  = d2(i0,i1)*d(i0,i2)*d(i0,i3)
   vi3(3)  = d2(i0,i1)*d(i1,i2)*d(i0,i3)
   vi3(4)  = d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)
   vi3(5)  = d(i0,i1)*d2(i1,i2)*d(i0,i3)
   vi3(6)  = d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)
   vi3(7)  = d3(i0,i1)*d(i0,i2)*d(i0,i3)
   vi3(8)  = d2(i0,i1)*d2(i0,i2)*d(i0,i3)
   vi3(9)  = d3(i0,i1)*d(i1,i2)*d(i0,i3)
   vi3(10) = d2(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)
   vi3(11) = d2(i0,i1)*d2(i1,i2)*d(i0,i3)
   vi3(12) = d(i0,i1)*d(i0,i2)*d2(i1,i2)*d(i0,i3)
   vi3(13) = d(i0,i1)*d3(i1,i2)*d(i0,i3)
   vi3(14) = d(i0,i1)*d(i0,i2)*d(i1,i2)*d2(i0,i3)
   vi3(15) = d(i0,i1)*d2(i1,i2)*d2(i0,i3)
   vi3(16) = d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)
   vi3(17) = d2(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)
   pvi3=pvi3+vi3
end do

do i=0,719
   i0=mg63_mii(0,i)
   i1=mg63_mii(1,i)
   i2=mg63_mii(2,i)
   i3=mg63_mii(3,i)
   i4=mg63_mii(4,i)
   vii(1) = d2(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,i4)
   vii(2) = d2(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,i4)
   vii(3) = d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i0,i4)
   vii(4) = d(i0,i1)*d2(i1,i2)*d(i0,i3)*d(i0,i4)
   pvii=pvii+vii
end do

do i=0,2159,24
   j0=mg63_miij0(0,i)
   i0=mg63_miij0(1,i)
   i1=mg63_miij0(2,i)
 vi1j0(1 ) = d(i0,i1)*d(i0,j0)
 vi1j0(2 ) = d2(i0,i1)*d(i0,j0)
 vi1j0(3 ) = d(i0,i1)*d2(i0,j0)
 vi1j0(4 ) = d(i0,i1)*d(i0,j0)*d(i1,j0)
 vi1j0(5 ) = d2(i0,j0)*d(i1,j0)
 vi1j0(6 ) = d3(i0,i1)*d(i0,j0)
 vi1j0(7 ) = d2(i0,i1)*d2(i0,j0)
 vi1j0(8 ) = d(i0,i1)*d3(i0,j0)
 vi1j0(9 ) = d2(i0,i1)*d(i0,j0)*d(i1,j0)
 vi1j0(10) = d(i0,i1)*d2(i0,j0)*d(i1,j0)
 vi1j0(11) = d2(i0,j0)*d2(i1,j0)
 vi1j0(12) = d4(i0,i1)*d(i0,j0)
 vi1j0(13) = d3(i0,i1)*d2(i0,j0)
 vi1j0(14) = d2(i0,i1)*d3(i0,j0)
 vi1j0(15) = d(i0,i1)*d4(i0,j0)
 vi1j0(16) = d3(i0,i1)*d(i0,j0)*d(i1,j0)
 vi1j0(17) = d2(i0,i1)*d2(i0,j0)*d(i1,j0)
 vi1j0(18) = d(i0,i1)*d3(i0,j0)*d(i1,j0)
 vi1j0(19) = d4(i0,j0)*d(i1,j0)
 vi1j0(20) = d(i0,i1)*d2(i0,j0)*d2(i1,j0)
 vi1j0(21) = d3(i0,j0)*d2(i1,j0)
   pvi1j0=pvi1j0+vi1j0
end do

do i=0,2159,6
   j0=mg63_miij0(0,i)
   i0=mg63_miij0(1,i)
   i1=mg63_miij0(2,i)
   i2=mg63_miij0(3,i)
 vi2j0(1 ) = d(i0,i1)*d(i0,i2)*d(i0,j0)
 vi2j0(2 ) = d(i0,i1)*d(i1,i2)*d(i0,j0)
 vi2j0(3 ) = d(i0,i2)*d(i0,j0)*d(i1,j0)
 vi2j0(4 ) = d2(i0,i1)*d(i0,i2)*d(i0,j0)
 vi2j0(5 ) = d2(i0,i1)*d(i1,i2)*d(i0,j0)
 vi2j0(6 ) = d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,j0)
 vi2j0(7 ) = d(i0,i1)*d2(i1,i2)*d(i0,j0)
 vi2j0(8 ) = d(i0,i1)*d(i0,i2)*d2(i0,j0)
 vi2j0(9 ) = d(i0,i1)*d(i1,i2)*d2(i0,j0)
 vi2j0(10) = d(i0,i1)*d(i0,i2)*d(i0,j0)*d(i1,j0)
 vi2j0(11) = d2(i0,i2)*d(i0,j0)*d(i1,j0)
 vi2j0(12) = d(i0,i2)*d(i1,i2)*d(i0,j0)*d(i1,j0)
 vi2j0(13) = d(i0,i2)*d2(i0,j0)*d(i1,j0)
 vi2j0(14) = d(i1,i2)*d2(i0,j0)*d(i1,j0)
 vi2j0(15) = d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i2,j0)
 vi2j0(16) = d2(i0,j0)*d(i1,j0)*d(i2,j0)
 vi2j0(17) = d3(i0,i1)*d(i0,i2)*d(i0,j0)
 vi2j0(18) = d2(i0,i1)*d2(i0,i2)*d(i0,j0)
 vi2j0(19) = d3(i0,i1)*d(i1,i2)*d(i0,j0)
 vi2j0(20) = d2(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,j0)
 vi2j0(21) = d2(i0,i1)*d2(i1,i2)*d(i0,j0)
 vi2j0(22) = d(i0,i1)*d(i0,i2)*d2(i1,i2)*d(i0,j0)
 vi2j0(23) = d(i0,i1)*d3(i1,i2)*d(i0,j0)
 vi2j0(24) = d2(i0,i1)*d(i0,i2)*d2(i0,j0)
 vi2j0(25) = d2(i0,i1)*d(i1,i2)*d2(i0,j0)
 vi2j0(26) = d(i0,i1)*d(i0,i2)*d(i1,i2)*d2(i0,j0)
 vi2j0(27) = d(i0,i1)*d2(i1,i2)*d2(i0,j0)
 vi2j0(28) = d(i0,i1)*d(i0,i2)*d3(i0,j0)
 vi2j0(29) = d(i0,i1)*d(i1,i2)*d3(i0,j0)
 vi2j0(30) = d2(i0,i1)*d(i0,i2)*d(i0,j0)*d(i1,j0)
 vi2j0(31) = d(i0,i1)*d2(i0,i2)*d(i0,j0)*d(i1,j0)
 vi2j0(32) = d3(i0,i2)*d(i0,j0)*d(i1,j0)
 vi2j0(33) = d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,j0)*d(i1,j0)
 vi2j0(34) = d2(i0,i2)*d(i1,i2)*d(i0,j0)*d(i1,j0)
 vi2j0(35) = d(i0,i1)*d(i0,i2)*d2(i0,j0)*d(i1,j0)
 vi2j0(36) = d2(i0,i2)*d2(i0,j0)*d(i1,j0)
 vi2j0(37) = d(i0,i1)*d(i1,i2)*d2(i0,j0)*d(i1,j0)
 vi2j0(38) = d(i0,i2)*d(i1,i2)*d2(i0,j0)*d(i1,j0)
 vi2j0(39) = d2(i1,i2)*d2(i0,j0)*d(i1,j0)
 vi2j0(40) = d(i0,i2)*d3(i0,j0)*d(i1,j0)
 vi2j0(41) = d(i1,i2)*d3(i0,j0)*d(i1,j0)
 vi2j0(42) = d(i0,i2)*d2(i0,j0)*d2(i1,j0)
 vi2j0(43) = d2(i0,i1)*d(i0,j0)*d(i1,j0)*d(i2,j0)
 vi2j0(44) = d(i0,i1)*d(i0,i2)*d(i0,j0)*d(i1,j0)*d(i2,j0)
 vi2j0(45) = d(i0,i1)*d2(i0,j0)*d(i1,j0)*d(i2,j0)
 vi2j0(46) = d(i1,i2)*d2(i0,j0)*d(i1,j0)*d(i2,j0)
 vi2j0(47) = d3(i0,j0)*d(i1,j0)*d(i2,j0)
 vi2j0(48) = d2(i0,j0)*d2(i1,j0)*d(i2,j0)
   pvi2j0=pvi2j0+vi2j0
end do

do i=0,2159,2
   j0=mg63_miij0(0,i)
   i0=mg63_miij0(1,i)
   i1=mg63_miij0(2,i)
   i2=mg63_miij0(3,i)
   i3=mg63_miij0(4,i)
 vi3j0(1 ) = d(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,j0)
 vi3j0(2 ) = d(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,j0)
 vi3j0(3 ) = d(i0,i1)*d(i1,i2)*d(i1,i3)*d(i0,j0)
 vi3j0(4 ) = d(i0,i2)*d(i0,i3)*d(i0,j0)*d(i1,j0)
 vi3j0(5 ) = d(i1,i2)*d(i0,i3)*d(i0,j0)*d(i1,j0)
 vi3j0(6 ) = d(i0,i2)*d(i2,i3)*d(i0,j0)*d(i1,j0)
 vi3j0(7 ) = d(i0,i3)*d(i0,j0)*d(i1,j0)*d(i2,j0)
 vi3j0(8 ) = d2(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,j0)
 vi3j0(9 ) = d2(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,j0)
 vi3j0(10) = d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i0,j0)
 vi3j0(11) = d(i0,i1)*d2(i1,i2)*d(i0,i3)*d(i0,j0)
 vi3j0(12) = d(i0,i1)*d(i1,i2)*d2(i0,i3)*d(i0,j0)
 vi3j0(13) = d2(i0,i1)*d(i1,i2)*d(i1,i3)*d(i0,j0)
 vi3j0(14) = d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,j0)
 vi3j0(15) = d2(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,j0)
 vi3j0(16) = d(i0,i1)*d2(i1,i2)*d(i1,i3)*d(i0,j0)
 vi3j0(17) = d(i0,i2)*d2(i1,i2)*d(i1,i3)*d(i0,j0)
 vi3j0(18) = d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,j0)
 vi3j0(19) = d(i0,i1)*d(i1,i2)*d(i1,i3)*d(i2,i3)*d(i0,j0)
 vi3j0(20) = d(i0,i1)*d(i0,i2)*d(i0,i3)*d2(i0,j0)
 vi3j0(21) = d(i0,i1)*d(i1,i2)*d(i0,i3)*d2(i0,j0)
 vi3j0(22) = d(i0,i1)*d(i1,i2)*d(i1,i3)*d2(i0,j0)
 vi3j0(23) = d(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,j0)*d(i1,j0)
 vi3j0(24) = d2(i0,i2)*d(i0,i3)*d(i0,j0)*d(i1,j0)
 vi3j0(25) = d(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,j0)*d(i1,j0)
 vi3j0(26) = d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i0,j0)*d(i1,j0)
 vi3j0(27) = d2(i1,i2)*d(i0,i3)*d(i0,j0)*d(i1,j0)
 vi3j0(28) = d(i0,i1)*d(i0,i2)*d(i2,i3)*d(i0,j0)*d(i1,j0)
 vi3j0(29) = d2(i0,i2)*d(i2,i3)*d(i0,j0)*d(i1,j0)
 vi3j0(30) = d(i0,i2)*d(i1,i2)*d(i2,i3)*d(i0,j0)*d(i1,j0)
 vi3j0(31) = d(i0,i2)*d(i0,i3)*d(i2,i3)*d(i0,j0)*d(i1,j0)
 vi3j0(32) = d(i1,i2)*d(i0,i3)*d(i2,i3)*d(i0,j0)*d(i1,j0)
 vi3j0(33) = d(i0,i2)*d2(i2,i3)*d(i0,j0)*d(i1,j0)
 vi3j0(34) = d(i0,i2)*d(i0,i3)*d2(i0,j0)*d(i1,j0)
 vi3j0(35) = d(i1,i2)*d(i0,i3)*d2(i0,j0)*d(i1,j0)
 vi3j0(36) = d(i1,i2)*d(i1,i3)*d2(i0,j0)*d(i1,j0)
 vi3j0(37) = d(i0,i2)*d(i2,i3)*d2(i0,j0)*d(i1,j0)
 vi3j0(38) = d(i1,i2)*d(i2,i3)*d2(i0,j0)*d(i1,j0)
 vi3j0(39) = d(i0,i1)*d(i0,i3)*d(i0,j0)*d(i1,j0)*d(i2,j0)
 vi3j0(40) = d(i1,i2)*d(i0,i3)*d(i0,j0)*d(i1,j0)*d(i2,j0)
 vi3j0(41) = d2(i0,i3)*d(i0,j0)*d(i1,j0)*d(i2,j0)
 vi3j0(42) = d(i0,i3)*d(i1,i3)*d(i0,j0)*d(i1,j0)*d(i2,j0)
 vi3j0(43) = d(i0,i3)*d2(i0,j0)*d(i1,j0)*d(i2,j0)
 vi3j0(44) = d(i1,i3)*d2(i0,j0)*d(i1,j0)*d(i2,j0)
 vi3j0(45) = d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i2,j0)*d(i3,j0)
   pvi3j0=pvi3j0+vi3j0
end do

do i=0,2159
   j0=mg63_miij0(0,i)
   i0=mg63_miij0(1,i)
   i1=mg63_miij0(2,i)
   i2=mg63_miij0(3,i)
   i3=mg63_miij0(4,i)
   i4=mg63_miij0(5,i)
 viij0(1) = d(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,i4)*d(i0,j0)
 viij0(2) = d(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,i4)*d(i0,j0)
 viij0(3) = d(i0,i2)*d(i1,i2)*d(i1,i3)*d(i1,i4)*d(i0,j0)
 viij0(4) = d(i0,i2)*d(i0,i3)*d(i0,i4)*d(i0,j0)*d(i1,j0)
 viij0(5) = d(i1,i2)*d(i0,i3)*d(i0,i4)*d(i0,j0)*d(i1,j0)
 viij0(6) = d(i0,i2)*d(i2,i3)*d(i0,i4)*d(i0,j0)*d(i1,j0)
 viij0(7) = d(i0,i3)*d(i0,i4)*d(i0,j0)*d(i1,j0)*d(i2,j0)
 viij0(8) = d(i1,i3)*d(i0,i4)*d(i0,j0)*d(i1,j0)*d(i2,j0)
   pviij0=pviij0+viij0
end do

do i=0,4319,120
   j0=mg63_miijj(0,i)
   j1=mg63_miijj(1,i)
   j2=mg63_miijj(2,i)
   i0=mg63_miijj(3,i)
 vi0jj(1 ) = d(i0,j0)*d(j0,j1)
 vi0jj(2 ) = d2(i0,j0)*d(i0,j1)
 vi0jj(3 ) = d2(i0,j0)*d(j0,j1)
 vi0jj(4 ) = d(i0,j0)*d(i0,j1)*d(j0,j1)
 vi0jj(5 ) = d(i0,j0)*d2(j0,j1)
 vi0jj(6 ) = d2(i0,j0)*d2(i0,j1)
 vi0jj(7 ) = d3(i0,j0)*d(j0,j1)
 vi0jj(8 ) = d2(i0,j0)*d(i0,j1)*d(j0,j1)
 vi0jj(9 ) = d2(i0,j0)*d(i0,j2)*d(j0,j1)
 vi0jj(10) = d2(i0,j0)*d2(j0,j1)
 vi0jj(11) = d(i0,j0)*d(i0,j1)*d2(j0,j1)
 vi0jj(12) = d4(i0,j0)*d(i0,j1)
 vi0jj(13) = d3(i0,j0)*d2(i0,j1)
 vi0jj(14) = d3(i0,j0)*d(i0,j1)*d(i0,j2)
 vi0jj(15) = d4(i0,j0)*d(j0,j1)
 vi0jj(16) = d3(i0,j0)*d(i0,j1)*d(j0,j1)
 vi0jj(17) = d2(i0,j0)*d2(i0,j1)*d(j0,j1)
 vi0jj(18) = d3(i0,j0)*d(i0,j2)*d(j0,j1)
 vi0jj(19) = d2(i0,j0)*d(i0,j1)*d(i0,j2)*d(j0,j1)
 vi0jj(20) = d3(i0,j0)*d2(j0,j1)
 vi0jj(21) = d2(i0,j0)*d(i0,j1)*d2(j0,j1)
 vi0jj(22) = d2(i0,j0)*d(i0,j2)*d2(j0,j1)
   pvi0jj=pvi0jj+vi0jj
end do

do i=0,4319,24
   j0=mg63_miijj(0,i)
   j1=mg63_miijj(1,i)
   j2=mg63_miijj(2,i)
   i0=mg63_miijj(3,i)
   i1=mg63_miijj(4,i)
 vi1jj(1 ) = d(i0,i1)*d(i0,j0)*d(i0,j1)
 vi1jj(2 ) = d(i0,i1)*d(i1,j0)*d(i0,j1)
 vi1jj(3 ) = d(i0,j0)*d(i1,j0)*d(i0,j1)
 vi1jj(4 ) = d(i0,i1)*d(i0,j0)*d(j0,j1)
 vi1jj(5 ) = d(i0,j0)*d(i1,j0)*d(j0,j1)
 vi1jj(6 ) = d2(i0,i1)*d(i0,j0)*d(i0,j1)
 vi1jj(7 ) = d(i0,i1)*d2(i0,j0)*d(i0,j1)
 vi1jj(8 ) = d2(i0,i1)*d(i1,j0)*d(i0,j1)
 vi1jj(9 ) = d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,j1)
 vi1jj(10) = d2(i0,j0)*d(i1,j0)*d(i0,j1)
 vi1jj(11) = d(i0,i1)*d2(i1,j0)*d(i0,j1)
 vi1jj(12) = d(i0,j0)*d2(i1,j0)*d(i0,j1)
 vi1jj(13) = d(i0,j0)*d(i1,j0)*d2(i0,j1)
 vi1jj(14) = d(i0,j0)*d(i1,j0)*d(i0,j1)*d(i1,j1)
 vi1jj(15) = d(i0,i1)*d(i0,j0)*d(i0,j1)*d(i0,j2)
 vi1jj(16) = d(i0,i1)*d(i1,j0)*d(i0,j1)*d(i0,j2)
 vi1jj(17) = d2(i0,i1)*d(i0,j0)*d(j0,j1)
 vi1jj(18) = d(i0,i1)*d2(i0,j0)*d(j0,j1)
 vi1jj(19) = d(i0,i1)*d(i0,j0)*d(i1,j0)*d(j0,j1)
 vi1jj(20) = d2(i0,j0)*d(i1,j0)*d(j0,j1)
 vi1jj(21) = d(i0,i1)*d(i0,j0)*d(i0,j1)*d(j0,j1)
 vi1jj(22) = d(i0,i1)*d(i1,j0)*d(i0,j1)*d(j0,j1)
 vi1jj(23) = d(i0,j0)*d(i1,j0)*d(i0,j1)*d(j0,j1)
 vi1jj(24) = d(i0,i1)*d(i0,j0)*d2(j0,j1)
 vi1jj(25) = d3(i0,i1)*d(i0,j0)*d(i0,j1)
 vi1jj(26) = d2(i0,i1)*d2(i0,j0)*d(i0,j1)
 vi1jj(27) = d(i0,i1)*d3(i0,j0)*d(i0,j1)
 vi1jj(28) = d3(i0,i1)*d(i1,j0)*d(i0,j1)
 vi1jj(29) = d2(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,j1)
 vi1jj(30) = d(i0,i1)*d2(i0,j0)*d(i1,j0)*d(i0,j1)
 vi1jj(31) = d3(i0,j0)*d(i1,j0)*d(i0,j1)
 vi1jj(32) = d2(i0,i1)*d2(i1,j0)*d(i0,j1)
 vi1jj(33) = d(i0,i1)*d(i0,j0)*d2(i1,j0)*d(i0,j1)
 vi1jj(34) = d2(i0,j0)*d2(i1,j0)*d(i0,j1)
 vi1jj(35) = d(i0,i1)*d3(i1,j0)*d(i0,j1)
 vi1jj(36) = d(i0,j0)*d3(i1,j0)*d(i0,j1)
 vi1jj(37) = d(i0,i1)*d2(i0,j0)*d2(i0,j1)
 vi1jj(38) = d(i0,i1)*d(i0,j0)*d(i1,j0)*d2(i0,j1)
 vi1jj(39) = d2(i0,j0)*d(i1,j0)*d2(i0,j1)
 vi1jj(40) = d(i0,i1)*d2(i1,j0)*d2(i0,j1)
 vi1jj(41) = d(i0,j0)*d2(i1,j0)*d2(i0,j1)
 vi1jj(42) = d(i0,j0)*d(i1,j0)*d3(i0,j1)
 vi1jj(43) = d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,j1)*d(i1,j1)
 vi1jj(44) = d2(i0,j0)*d(i1,j0)*d(i0,j1)*d(i1,j1)
 vi1jj(45) = d2(i0,i1)*d(i0,j0)*d(i0,j1)*d(i0,j2)
 vi1jj(46) = d(i0,i1)*d2(i0,j0)*d(i0,j1)*d(i0,j2)
 vi1jj(47) = d2(i0,i1)*d(i1,j0)*d(i0,j1)*d(i0,j2)
 vi1jj(48) = d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,j1)*d(i0,j2)
 vi1jj(49) = d2(i0,j0)*d(i1,j0)*d(i0,j1)*d(i0,j2)
 vi1jj(50) = d(i0,i1)*d2(i1,j0)*d(i0,j1)*d(i0,j2)
 vi1jj(51) = d(i0,i1)*d(i1,j0)*d2(i0,j1)*d(i0,j2)
 vi1jj(52) = d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i1,j1)*d(i0,j2)
 vi1jj(53) = d2(i0,j0)*d(i1,j0)*d(i1,j1)*d(i0,j2)
 vi1jj(54) = d3(i0,i1)*d(i0,j0)*d(j0,j1)
 vi1jj(55) = d2(i0,i1)*d2(i0,j0)*d(j0,j1)
 vi1jj(56) = d(i0,i1)*d3(i0,j0)*d(j0,j1)
 vi1jj(57) = d2(i0,i1)*d(i0,j0)*d(i1,j0)*d(j0,j1)
 vi1jj(58) = d(i0,i1)*d2(i0,j0)*d(i1,j0)*d(j0,j1)
 vi1jj(59) = d3(i0,j0)*d(i1,j0)*d(j0,j1)
 vi1jj(60) = d2(i0,j0)*d2(i1,j0)*d(j0,j1)
 vi1jj(61) = d2(i0,i1)*d(i0,j0)*d(i0,j1)*d(j0,j1)
 vi1jj(62) = d(i0,i1)*d2(i0,j0)*d(i0,j1)*d(j0,j1)
 vi1jj(63) = d2(i0,i1)*d(i1,j0)*d(i0,j1)*d(j0,j1)
 vi1jj(64) = d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,j1)*d(j0,j1)
 vi1jj(65) = d2(i0,j0)*d(i1,j0)*d(i0,j1)*d(j0,j1)
 vi1jj(66) = d(i0,i1)*d2(i1,j0)*d(i0,j1)*d(j0,j1)
 vi1jj(67) = d(i0,j0)*d2(i1,j0)*d(i0,j1)*d(j0,j1)
 vi1jj(68) = d(i0,j0)*d(i1,j0)*d2(i0,j1)*d(j0,j1)
 vi1jj(69) = d(i0,j0)*d(i1,j0)*d(i0,j1)*d(i1,j1)*d(j0,j1)
 vi1jj(70) = d(i0,i1)*d2(i0,j0)*d(i0,j2)*d(j0,j1)
 vi1jj(71) = d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,j2)*d(j0,j1)
 vi1jj(72) = d2(i0,j0)*d(i1,j0)*d(i0,j2)*d(j0,j1)
 vi1jj(73) = d(i0,i1)*d2(i1,j0)*d(i0,j2)*d(j0,j1)
 vi1jj(74) = d(i0,i1)*d(i1,j0)*d(i0,j1)*d(i0,j2)*d(j0,j1)
 vi1jj(75) = d2(i0,i1)*d(i0,j0)*d2(j0,j1)
 vi1jj(76) = d(i0,i1)*d2(i0,j0)*d2(j0,j1)
 vi1jj(77) = d(i0,i1)*d(i0,j0)*d(i1,j0)*d2(j0,j1)
 vi1jj(78) = d(i0,i1)*d(i0,j0)*d(i0,j1)*d2(j0,j1)
 vi1jj(79) = d(i0,i1)*d(i1,j0)*d(i0,j1)*d2(j0,j1)
   pvi1jj=pvi1jj+vi1jj
end do

do i=0,4319,6
   j0=mg63_miijj(0,i)
   j1=mg63_miijj(1,i)
   j2=mg63_miijj(2,i)
   i0=mg63_miijj(3,i)
   i1=mg63_miijj(4,i)
   i2=mg63_miijj(5,i)
 vi2jj(1 ) = d(i0,i1)*d(i0,i2)*d(i0,j0)*d(i0,j1)
 vi2jj(2 ) = d(i0,i1)*d(i1,i2)*d(i0,j0)*d(i0,j1)
 vi2jj(3 ) = d(i0,i1)*d(i0,i2)*d(i1,j0)*d(i0,j1)
 vi2jj(4 ) = d(i0,i2)*d(i1,i2)*d(i1,j0)*d(i0,j1)
 vi2jj(5 ) = d(i0,i2)*d(i0,j0)*d(i1,j0)*d(i0,j1)
 vi2jj(6 ) = d(i1,i2)*d(i0,j0)*d(i1,j0)*d(i0,j1)
 vi2jj(7 ) = d(i0,i1)*d(i1,j0)*d(i2,j0)*d(i0,j1)
 vi2jj(8 ) = d(i0,j0)*d(i1,j0)*d(i2,j0)*d(i0,j1)
 vi2jj(9 ) = d(i0,i1)*d(i0,i2)*d(i0,j0)*d(j0,j1)
 vi2jj(10) = d(i0,i1)*d(i1,i2)*d(i0,j0)*d(j0,j1)
 vi2jj(11) = d(i0,i2)*d(i0,j0)*d(i1,j0)*d(j0,j1)
 vi2jj(12) = d2(i0,i1)*d(i0,i2)*d(i0,j0)*d(i0,j1)
 vi2jj(13) = d2(i0,i1)*d(i1,i2)*d(i0,j0)*d(i0,j1)
 vi2jj(14) = d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,j0)*d(i0,j1)
 vi2jj(15) = d(i0,i1)*d2(i1,i2)*d(i0,j0)*d(i0,j1)
 vi2jj(16) = d(i0,i1)*d(i0,i2)*d2(i0,j0)*d(i0,j1)
 vi2jj(17) = d(i0,i1)*d(i1,i2)*d2(i0,j0)*d(i0,j1)
 vi2jj(18) = d2(i0,i1)*d(i0,i2)*d(i1,j0)*d(i0,j1)
 vi2jj(19) = d(i0,i1)*d2(i0,i2)*d(i1,j0)*d(i0,j1)
 vi2jj(20) = d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i1,j0)*d(i0,j1)
 vi2jj(21) = d2(i0,i2)*d(i1,i2)*d(i1,j0)*d(i0,j1)
 vi2jj(22) = d(i0,i1)*d(i0,i2)*d(i0,j0)*d(i1,j0)*d(i0,j1)
 vi2jj(23) = d2(i0,i2)*d(i0,j0)*d(i1,j0)*d(i0,j1)
 vi2jj(24) = d(i0,i1)*d(i1,i2)*d(i0,j0)*d(i1,j0)*d(i0,j1)
 vi2jj(25) = d(i0,i2)*d(i1,i2)*d(i0,j0)*d(i1,j0)*d(i0,j1)
 vi2jj(26) = d2(i1,i2)*d(i0,j0)*d(i1,j0)*d(i0,j1)
 vi2jj(27) = d(i0,i2)*d2(i0,j0)*d(i1,j0)*d(i0,j1)
 vi2jj(28) = d(i1,i2)*d2(i0,j0)*d(i1,j0)*d(i0,j1)
 vi2jj(29) = d(i0,i1)*d(i0,i2)*d2(i1,j0)*d(i0,j1)
 vi2jj(30) = d(i0,i1)*d(i1,i2)*d2(i1,j0)*d(i0,j1)
 vi2jj(31) = d(i0,i2)*d(i1,i2)*d2(i1,j0)*d(i0,j1)
 vi2jj(32) = d(i0,i2)*d(i0,j0)*d2(i1,j0)*d(i0,j1)
 vi2jj(33) = d(i1,i2)*d(i0,j0)*d2(i1,j0)*d(i0,j1)
 vi2jj(34) = d2(i0,i1)*d(i1,j0)*d(i2,j0)*d(i0,j1)
 vi2jj(35) = d(i0,i1)*d(i0,i2)*d(i1,j0)*d(i2,j0)*d(i0,j1)
 vi2jj(36) = d(i0,i1)*d(i1,i2)*d(i1,j0)*d(i2,j0)*d(i0,j1)
 vi2jj(37) = d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i2,j0)*d(i0,j1)
 vi2jj(38) = d(i1,i2)*d(i0,j0)*d(i1,j0)*d(i2,j0)*d(i0,j1)
 vi2jj(39) = d2(i0,j0)*d(i1,j0)*d(i2,j0)*d(i0,j1)
 vi2jj(40) = d(i0,i1)*d2(i1,j0)*d(i2,j0)*d(i0,j1)
 vi2jj(41) = d(i0,i2)*d2(i1,j0)*d(i2,j0)*d(i0,j1)
 vi2jj(42) = d(i0,j0)*d2(i1,j0)*d(i2,j0)*d(i0,j1)
 vi2jj(43) = d(i0,i2)*d(i0,j0)*d(i1,j0)*d2(i0,j1)
 vi2jj(44) = d(i1,i2)*d(i0,j0)*d(i1,j0)*d2(i0,j1)
 vi2jj(45) = d(i0,i1)*d(i1,j0)*d(i2,j0)*d2(i0,j1)
 vi2jj(46) = d(i0,j0)*d(i1,j0)*d(i2,j0)*d2(i0,j1)
 vi2jj(47) = d(i0,i2)*d(i0,j0)*d(i1,j0)*d(i0,j1)*d(i1,j1)
 vi2jj(48) = d(i0,i1)*d(i0,j0)*d(i2,j0)*d(i0,j1)*d(i1,j1)
 vi2jj(49) = d(i1,i2)*d(i0,j0)*d(i2,j0)*d(i0,j1)*d(i1,j1)
 vi2jj(50) = d(i0,j0)*d(i1,j0)*d(i2,j0)*d(i0,j1)*d(i1,j1)
 vi2jj(51) = d(i0,i1)*d(i0,i2)*d(i0,j0)*d(i0,j1)*d(i0,j2)
 vi2jj(52) = d(i0,i1)*d(i1,i2)*d(i0,j0)*d(i0,j1)*d(i0,j2)
 vi2jj(53) = d(i0,i1)*d(i0,i2)*d(i1,j0)*d(i0,j1)*d(i0,j2)
 vi2jj(54) = d(i0,i1)*d(i1,i2)*d(i1,j0)*d(i0,j1)*d(i0,j2)
 vi2jj(55) = d(i0,i2)*d(i1,i2)*d(i1,j0)*d(i0,j1)*d(i0,j2)
 vi2jj(56) = d(i0,i1)*d(i1,j0)*d(i2,j0)*d(i0,j1)*d(i0,j2)
 vi2jj(57) = d2(i0,i1)*d(i0,i2)*d(i0,j0)*d(j0,j1)
 vi2jj(58) = d2(i0,i1)*d(i1,i2)*d(i0,j0)*d(j0,j1)
 vi2jj(59) = d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,j0)*d(j0,j1)
 vi2jj(60) = d(i0,i1)*d2(i1,i2)*d(i0,j0)*d(j0,j1)
 vi2jj(61) = d(i0,i1)*d(i0,i2)*d2(i0,j0)*d(j0,j1)
 vi2jj(62) = d(i0,i1)*d(i1,i2)*d2(i0,j0)*d(j0,j1)
 vi2jj(63) = d(i0,i1)*d(i0,i2)*d(i0,j0)*d(i1,j0)*d(j0,j1)
 vi2jj(64) = d2(i0,i2)*d(i0,j0)*d(i1,j0)*d(j0,j1)
 vi2jj(65) = d(i0,i2)*d(i1,i2)*d(i0,j0)*d(i1,j0)*d(j0,j1)
 vi2jj(66) = d(i0,i2)*d2(i0,j0)*d(i1,j0)*d(j0,j1)
 vi2jj(67) = d(i1,i2)*d2(i0,j0)*d(i1,j0)*d(j0,j1)
 vi2jj(68) = d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i2,j0)*d(j0,j1)
 vi2jj(69) = d(i0,i1)*d(i0,i2)*d(i0,j0)*d(i0,j1)*d(j0,j1)
 vi2jj(70) = d(i0,i1)*d(i1,i2)*d(i0,j0)*d(i0,j1)*d(j0,j1)
 vi2jj(71) = d(i0,i1)*d(i0,i2)*d(i1,j0)*d(i0,j1)*d(j0,j1)
 vi2jj(72) = d(i0,i2)*d(i1,i2)*d(i1,j0)*d(i0,j1)*d(j0,j1)
 vi2jj(73) = d(i0,i2)*d(i0,j0)*d(i1,j0)*d(i0,j1)*d(j0,j1)
 vi2jj(74) = d(i1,i2)*d(i0,j0)*d(i1,j0)*d(i0,j1)*d(j0,j1)
 vi2jj(75) = d(i0,i1)*d(i1,j0)*d(i2,j0)*d(i0,j1)*d(j0,j1)
 vi2jj(76) = d(i0,i1)*d(i0,i2)*d(i1,j0)*d(i0,j2)*d(j0,j1)
 vi2jj(77) = d(i0,i1)*d(i0,i2)*d(i0,j0)*d2(j0,j1)
 vi2jj(78) = d(i0,i1)*d(i1,i2)*d(i0,j0)*d2(j0,j1)
   pvi2jj=pvi2jj+vi2jj
end do

do i=0,4319,2
   j0=mg63_miijj(0,i)
   j1=mg63_miijj(1,i)
   j2=mg63_miijj(2,i)
   i0=mg63_miijj(3,i)
   i1=mg63_miijj(4,i)
   i2=mg63_miijj(5,i)
   i3=mg63_miijj(6,i)
 vi3jj(1 ) = d(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,j0)*d(i0,j1)
 vi3jj(2 ) = d(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,j0)*d(i0,j1)
 vi3jj(3 ) = d(i0,i1)*d(i1,i2)*d(i1,i3)*d(i0,j0)*d(i0,j1)
 vi3jj(4 ) = d(i0,i1)*d(i0,i2)*d(i0,i3)*d(i1,j0)*d(i0,j1)
 vi3jj(5 ) = d(i0,i1)*d(i1,i2)*d(i0,i3)*d(i1,j0)*d(i0,j1)
 vi3jj(6 ) = d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,j0)*d(i0,j1)
 vi3jj(7 ) = d(i0,i1)*d(i0,i2)*d(i2,i3)*d(i1,j0)*d(i0,j1)
 vi3jj(8 ) = d(i0,i2)*d(i0,i3)*d(i0,j0)*d(i1,j0)*d(i0,j1)
 vi3jj(9 ) = d(i1,i2)*d(i0,i3)*d(i0,j0)*d(i1,j0)*d(i0,j1)
 vi3jj(10) = d(i1,i2)*d(i1,i3)*d(i0,j0)*d(i1,j0)*d(i0,j1)
 vi3jj(11) = d(i0,i2)*d(i2,i3)*d(i0,j0)*d(i1,j0)*d(i0,j1)
 vi3jj(12) = d(i1,i2)*d(i2,i3)*d(i0,j0)*d(i1,j0)*d(i0,j1)
 vi3jj(13) = d(i0,i1)*d(i0,i3)*d(i1,j0)*d(i2,j0)*d(i0,j1)
 vi3jj(14) = d(i0,i1)*d(i1,i3)*d(i1,j0)*d(i2,j0)*d(i0,j1)
 vi3jj(15) = d(i0,i2)*d(i1,i3)*d(i1,j0)*d(i2,j0)*d(i0,j1)
 vi3jj(16) = d(i0,i3)*d(i0,j0)*d(i1,j0)*d(i2,j0)*d(i0,j1)
 vi3jj(17) = d(i1,i3)*d(i0,j0)*d(i1,j0)*d(i2,j0)*d(i0,j1)
 vi3jj(18) = d(i0,i1)*d(i1,j0)*d(i2,j0)*d(i3,j0)*d(i0,j1)
 vi3jj(19) = d(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,j0)*d(j0,j1)
 vi3jj(20) = d(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,j0)*d(j0,j1)
 vi3jj(21) = d(i0,i1)*d(i1,i2)*d(i1,i3)*d(i0,j0)*d(j0,j1)
 vi3jj(22) = d(i0,i2)*d(i0,i3)*d(i0,j0)*d(i1,j0)*d(j0,j1)
 vi3jj(23) = d(i1,i2)*d(i0,i3)*d(i0,j0)*d(i1,j0)*d(j0,j1)
 vi3jj(24) = d(i0,i2)*d(i2,i3)*d(i0,j0)*d(i1,j0)*d(j0,j1)
   pvi3jj=pvi3jj+vi3jj
end do


pv((/19,20,88,89,90/))=pvi2/m3
pv((/2,21,22,23,24,25,91,92,93,94,95,96,97,98,99,100,101/))=pvi3/m4
pv((/102,103,104,105/))=pvii/mm
pv((/0,3,6,7,9,26,34,37,38,45,48,106,129,137,140,141,161,172,175,176, &
& 178/))=pvi1j0/(m2*n)
pv((/4,5,8,27,28,29,30,35,36,39,40,41,46,47,49,51,107,108,109,110,111, &
& 112,113,130,131,132,133,138,139,142,143,144,145,146,162,163,164,165, &
& 166,173,174,177,179,180,187,188,191,192/))=pvi2j0/(m3*n)
pv((/31,32,33,42,43,44,50,114,115,116,117,118,119,120,121,122,123,124, &
& 125,134,135,136,147,148,149,150,151,152,153,154,155,156,157,167,168, &
& 169,170,171,181,182,183,184,189,190,193/))=pvi3j0/(m4*n)
pv((/126,127,128,158,159,160,185,186/))=pviij0/(mm*n)
pv((/1,11,15,17,18,67,76,81,84,86,87,206,257,277,301,319,330,334,339, &
& 345,348,350/))=pvi0jj/(m*nn)
pv((/10,12,13,14,16,52,55,56,59,62,63,64,68,69,70,71,72,75,77,79,80,82, &
& 83,85,194,202,205,207,216,227,230,231,235,238,239,240,256,258,261,262, &
& 263,266,267,269,273,276,278,282,283,284,286,287,288,289,297,300,302, &
& 309,312,313,315,318,320,323,326,327,328,331,332,333,336,337,338,340,341, &
344,346,347,349/))=pvi1jj/(m2*nn)
pv((/53,54,57,58,60,61,65,66,73,74,78,195,196,197,198,203,204,208,209,210, &
& 211,217,218,219,220,221,228,229,232,233,234,236,237,241,242,243,247,248, &
& 251,252,253,254,259,260,264,265,268,270,271,272,274,275,279,280,281,285, &
& 290,291,292,293,298,299,303,304,305,310,311,314,316,317,321,322,324,325, &
& 329,335,342,343/))=pvi2jj/(m3*nn)
pv((/199,200,201,212,213,214,215,222,223,224,225,226,244,245,246,249,250, &
& 255,294,295,296,306,307,308/))=pvi3jj/(m4*nn)
return
END SUBROUTINE mg63_isecs5