SUBROUTINE mg2111_prims (r, u)
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: u(0:)
!-----------------------------------------------------------------------
integer, parameter :: m=2, m2=m*(m-1)/2
real (kind=wp) :: x(0:nr-1), u0(0:m2-1), u1(0:m-1), u2(0:m-1), u3(0:0), &
  u4(0:m-1), u5(0:0), u6(0:0)
if (size(r,1).ne.nk.or.size(r,2).ne.nk.or.size(u).ne.nr) then
 stop 'mg2111_prims: bad dimensions'
endif
call mgx_mk1d (nkj, r, x)
call cg2_prims (x(0:m2-1), u0)
call sym2_prims (x(m2:m2+m-1), u1)
call sym2_prims (x(m2+m:m2+2*m-1), u2)
u3(0) = x(m2+2*m)
call sym2_prims (x(m2+2*m+1:m2+3*m), u4)
u5(0) = x(m2+3*m+1)
u6(0) = x(m2+3*m+2)
u = (/ u0(0), u1(0), u2(0), u3(0), u4(0), u5(0), u6(0), &
  u1(1), u2(1), u4(1) /)
return
END SUBROUTINE mg2111_prims