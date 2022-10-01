SUBROUTINE mg433_prims (r, u)
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: u(0:)
!-----------------------------------------------------------------------
integer, parameter :: m=4, n=3, l=3, m2=m*(m-1)/2, n2=n*(n-1)/2, &
  l2=l*(l-1)/2
real (kind=wp) :: x(0:nr-1), u0(0:m2-1), u1(0:m*n-1), u2(0:n2-1), &
  u3(0:m*l-1), u4(0:n*l-1), u5(0:l2-1)
if (size(r,1).ne.nk.or.size(r,2).ne.nk.or.size(u).ne.nr) then
 stop 'mg433_prims: bad dimensions'
endif
call mgx_mk1d (nkj, r, x)
call cg4_prims (x(0:m2-1), u0)
call bg43_prims (x(m2:m2+m*n-1), u1)
call cg3_prims (x(m2+m*n:m2+m*n+n2-1), u2)
call bg43_prims (x(m2+m*n+n2:m2+m*n+n2+m*l-1), u3)
call bg33_prims (x(m2+m*n+n2+m*l:m2+m*n+n2+m*l+n*l-1), u4)
call cg3_prims (x(m2+m*n+n2+m*l+n*l:nr-1), u5)
u = (/ u0(0), u1(0), u2(0), u3(0), u4(0), u5(0), &
  u0(1), u0(2), u1(1), u1(2), u1(3), u2(1), u3(1), u3(2), u3(3), &
  u4(1), u4(2), u4(3), u5(1), &
  u0(3), u0(4), u1(4), u1(5), u1(6), u2(2), &
  u3(4), u3(5), u3(6), u4(4), u4(5), u4(6), u5(2), &
  u0(5), u1(7), u1(8), u1(9), u3(7), u3(8), u3(9), u4(7), &
  u1(10), u3(10), u4(8), &
  u1(11), u3(11) /)
return
END SUBROUTINE mg433_prims
