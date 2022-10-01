SUBROUTINE mg211111111_prims (r, u)
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: u(0:)
!-----------------------------------------------------------------------
integer, parameter :: m=2, m2=m*(m-1)/2
real (kind=wp) :: x(0:nr-1), u0(0:m2-1), u1(0:m-1), u2(0:m-1), u3(0:0), &
  u4(0:m-1), u5(0:0), u6(0:0), u7(0:m-1), u8(0:0), u9(0:0), u10(0:0), &
  u11(0:m-1), u12(0:0), u13(0:0), u14(0:0), u15(0:0), &
  u16(0:m-1), u17(0:0), u18(0:0), u19(0:0), u20(0:0), u21(0:0), &
  u22(0:m-1), u23(0:0), u24(0:0), u25(0:0), u26(0:0), u27(0:0), &
  u28(0:0), &
  u29(0:m-1), u30(0:0), u31(0:0), u32(0:0), u33(0:0), u34(0:0), &
  u35(0:0), u36(0:0)
if (size(r,1).ne.nk.or.size(r,2).ne.nk.or.size(u).ne.nr) then
 stop 'mg211111111_prims: bad dimensions'
endif
call mgx_mk1d (nkj, r, x)
call cg2_prims (x(0:m2-1), u0)
call sym2_prims (x(m2:m2+m-1), u1)
call sym2_prims (x(m2+m:m2+2*m-1), u2)
u3(0) = x(m2+2*m)
call sym2_prims (x(m2+2*m+1:m2+3*m), u4)
u5(0) = x(m2+3*m+1)
u6(0) = x(m2+3*m+2)
call sym2_prims (x(m2+3*m+3:m2+4*m+2), u7)
u8(0) = x(m2+4*m+3)
u9(0) = x(m2+4*m+4)
u10(0) = x(m2+4*m+5)
call sym2_prims (x(m2+4*m+6:m2+5*m+5), u11)
u12(0) = x(m2+5*m+6)
u13(0) = x(m2+5*m+7)
u14(0) = x(m2+5*m+8)
u15(0) = x(m2+5*m+9)
call sym2_prims (x(m2+5*m+10:m2+6*m+9), u16)
u17(0) = x(m2+6*m+10)
u18(0) = x(m2+6*m+11)
u19(0) = x(m2+6*m+12)
u20(0) = x(m2+6*m+13)
u21(0) = x(m2+6*m+14)
call sym2_prims (x(m2+6*m+15:m2+7*m+14), u22)
u23(0) = x(m2+7*m+15)
u24(0) = x(m2+7*m+16)
u25(0) = x(m2+7*m+17)
u26(0) = x(m2+7*m+18)
u27(0) = x(m2+7*m+19)
u28(0) = x(m2+7*m+20)
call sym2_prims (x(m2+7*m+21:m2+8*m+20), u29)
u30(0) = x(m2+8*m+21)
u31(0) = x(m2+8*m+22)
u32(0) = x(m2+8*m+23)
u33(0) = x(m2+8*m+24)
u34(0) = x(m2+8*m+25)
u35(0) = x(m2+8*m+26)
u36(0) = x(m2+8*m+27)
u = (/ u0(0), u1(0), u2(0), u3(0), u4(0), u5(0), u6(0), u7(0), u8(0), &
  u9(0), u10(0), u11(0), u12(0), u13(0), u14(0), u15(0), u16(0), &
  u17(0), u18(0), u19(0), u20(0), u21(0), &
  u22(0), u23(0), u24(0), u25(0), u26(0), u27(0), u28(0), &
  u29(0), u30(0), u31(0), u32(0), u33(0), u34(0), u35(0), u36(0), &
  u1(1), u2(1), u4(1), u7(1), u11(1), u16(1), u22(1), u29(1) /)
return
END SUBROUTINE mg211111111_prims