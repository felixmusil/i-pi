SUBROUTINE mg73_prims (r, u)
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: u(0:)
!-----------------------------------------------------------------------
integer, parameter :: m=7, n=3, m2=m*(m-1)/2, n2=n*(n-1)/2
real (kind=wp) :: x(0:nr-1), u0(0:m2-1), u1(0:m*n-1), u2(0:n2-1)
if (size(r,1).ne.nk.or.size(r,2).ne.nk.or.size(u).ne.nr) then
 stop 'mg73_prims: bad dimensions'
endif
call mgx_mk1d (nkj, r, x)
call cg7_prims (x(0:m2-1), u0)
call bg73_prims (x(m2:m2+m*n-1), u1)
call cg3_prims (x(m2+m*n:m2+m*n+n2-1), u2)
u = (/ u0(0), u1(0), u2(0), &
  u0(1), u0(2), u1(1), u1(2), u1(3), u2(1), &
  u0(3), u0(4), u0(5), u0(6), u0(7), u1(4), u1(5), u1(6), u2(2), &
  u0(8), u0(9), u0(10), u1(7), u1(8), u1(9), u1(10), &
  u0(11), u0(12), u0(13), u1(11), u1(12), &
  u0(14), u0(15), u1(13), u1(14), &
  u0(16), u0(17), u0(18), u1(15), &
  u0(19), u1(16), &
  u0(20), u1(17), &
  u1(18), &
  u1(19), &
  u1(20) /)
return
END SUBROUTINE mg73_prims