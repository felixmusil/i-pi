SUBROUTINE mg2221111_prims (r, u)
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: u(0:)
!-----------------------------------------------------------------------
integer, parameter :: m=2, n=2, l=2, m2=m*(m-1)/2, n2=n*(n-1)/2, &
  l2=l*(l-1)/2
integer :: ib0
real (kind=wp) :: x(0:nr-1), u0(0:m2-1), u1(0:m*n-1), u2(0:n2-1), &
  u3(0:m*l-1), u4(0:n*l-1), u5(0:l2-1), u6(0:m-1), u7(0:n-1), u8(0:l-1), &
  u9(0:m-1), u10(0:n-1), u11(0:l-1), u12(0:0), &
  u13(0:m-1), u14(0:n-1), u15(0:l-1), u16(0:0), u17(0:0), &
  u18(0:m-1), u19(0:n-1), u20(0:l-1), u21(0:0), u22(0:0), u23(0:0)
if (size(r,1).ne.nk.or.size(r,2).ne.nk.or.size(u).ne.nr) then
 stop 'mg2221111_prims: bad dimensions'
endif
call mgx_mk1d (nkj, r, x)
call cg2_prims (x(0:m2-1), u0)
call bg22_prims (x(m2:m2+m*n-1), u1)
call cg2_prims (x(m2+m*n:m2+m*n+n2-1), u2)
call bg22_prims (x(m2+m*n+n2:m2+m*n+n2+m*l-1), u3)
call bg22_prims (x(m2+m*n+n2+m*l:m2+m*n+n2+m*l+n*l-1), u4)
call cg2_prims (x(m2+m*n+n2+m*l+n*l:m2+m*n+n2+m*l+n*l+l2-1), u5)
ib0 = m2+m*n+n2+m*l+n*l+l2
call sym2_prims (x(ib0:ib0+m-1), u6)
call sym2_prims (x(ib0+m:ib0+m+n-1), u7)
call sym2_prims (x(ib0+m+n:ib0+m+n+l-1), u8)
ib0 = ib0+m+n+l
call sym2_prims (x(ib0:ib0+m-1), u9)
call sym2_prims (x(ib0+m:ib0+m+n-1), u10)
call sym2_prims (x(ib0+m+n:ib0+m+n+l-1), u11)
u12(0) = x(ib0+m+n+l)
ib0 = ib0+m+n+l+1
call sym2_prims (x(ib0:ib0+m-1), u13)
call sym2_prims (x(ib0+m:ib0+m+n-1), u14)
call sym2_prims (x(ib0+m+n:ib0+m+n+l-1), u15)
u16(0) = x(ib0+m+n+l)
u17(0) = x(ib0+m+n+l+1)
ib0 = ib0+m+n+l+2
call sym2_prims (x(ib0:ib0+m-1), u18)
call sym2_prims (x(ib0+m:ib0+m+n-1), u19)
call sym2_prims (x(ib0+m+n:ib0+m+n+l-1), u20)
u21(0) = x(ib0+m+n+l)
u22(0) = x(ib0+m+n+l+1)
u23(0) = x(ib0+m+n+l+2)
u = (/ u0(0), u1(0), u2(0), u3(0), u4(0), u5(0), u6(0), u7(0), u8(0), &
  u9(0), u10(0), u11(0), u12(0), &
  u13(0), u14(0), u15(0), u16(0), u17(0), &
  u18(0), u19(0), u20(0), u21(0), u22(0), u23(0), &
  u1(1), u1(2), u1(3), u3(1), u3(2), u3(3), u4(1), u4(2), u4(3), &
  u6(1), u7(1), u8(1), u9(1), u10(1), u11(1), &
  u13(1), u14(1), u15(1), u18(1), u19(1), u20(1) /)
return
END SUBROUTINE mg2221111_prims
