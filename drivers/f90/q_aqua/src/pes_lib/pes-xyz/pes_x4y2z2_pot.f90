FUNCTION pes_x4y2z2_pot (xn) RESULT (f)
! Potential for generic X4Y2Z2
real (kind=wp), intent (in) :: xn(0:,0:)
real (kind=wp) :: f
!-----------------------------------------------------------------------
real (kind=wp) :: r(0:pes_x4y2z2_nk-1,0:pes_x4y2z2_nk-1)
integer, parameter :: nki(0:2)=pes_x4y2z2_nki
call pes_dists (xn, r)
f = pes_x1_cf*nki(0)+ &
  pes_y1_cf*nki(1)+ &
  pes_z1_cf*nki(2)+ &
  cx_f2(nki,r,pes_x2_pc,pes_x2_cf)+ &
  cx_f11(nki,r,pes_x1y1_pc,pes_x1y1_cf)+ &
  cx_f02(nki,r,pes_y2_pc,pes_y2_cf)+ &
  cx_f101(nki,r,pes_x1z1_pc,pes_x1z1_cf)+ &
  cx_f011(nki,r,pes_y1z1_pc,pes_y1z1_cf)+ &
  cx_f002(nki,r,pes_z2_pc,pes_z2_cf)+ &
  cx_f3(nki,r,pes_x3_pc,pes_x3_cf)+ &
  cx_f21(nki,r,pes_x2y1_pc,pes_x2y1_cf)+ &
  cx_f12(nki,r,pes_x1y2_pc,pes_x1y2_cf)+ &
  cx_f201(nki,r,pes_x2z1_pc,pes_x2z1_cf)+ &
  cx_f111(nki,r,pes_x1y1z1_pc,pes_x1y1z1_cf)+ &
  cx_f021(nki,r,pes_y2z1_pc,pes_y2z1_cf)+ &
  cx_f102(nki,r,pes_x1z2_pc,pes_x1z2_cf)+ &
  cx_f012(nki,r,pes_y1z2_pc,pes_y1z2_cf)+ &
  cx_f4(nki,r,pes_x4_pc,pes_x4_cf)+ &
  cx_f31(nki,r,pes_x3y1_pc,pes_x3y1_cf)+ &
  cx_f22(nki,r,pes_x2y2_pc,pes_x2y2_cf)+ &
  cx_f301(nki,r,pes_x3z1_pc,pes_x3z1_cf)+ &
  cx_f211(nki,r,pes_x2y1z1_pc,pes_x2y1z1_cf)+ &
  cx_f121(nki,r,pes_x1y2z1_pc,pes_x1y2z1_cf)+ &
  cx_f202(nki,r,pes_x2z2_pc,pes_x2z2_cf)+ &
  cx_f112(nki,r,pes_x1y1z2_pc,pes_x1y1z2_cf)+ &
  cx_f022(nki,r,pes_y2z2_pc,pes_y2z2_cf)+ &
  cx_f41(nki,r,pes_x4y1_pc,pes_x4y1_cf)+ &
  cx_f32(nki,r,pes_x3y2_pc,pes_x3y2_cf)+ &
  cx_f401(nki,r,pes_x4z1_pc,pes_x4z1_cf)+ &
  cx_f311(nki,r,pes_x3y1z1_pc,pes_x3y1z1_cf)+ &
  cx_f221(nki,r,pes_x2y2z1_pc,pes_x2y2z1_cf)+ &
  cx_f302(nki,r,pes_x3z2_pc,pes_x3z2_cf)+ &
  cx_f212(nki,r,pes_x2y1z2_pc,pes_x2y1z2_cf)+ &
  cx_f122(nki,r,pes_x1y2z2_pc,pes_x1y2z2_cf)+ &
  cx_f42(nki,r,pes_x4y2_pc,pes_x4y2_cf)+ &
  cx_f411(nki,r,pes_x4y1z1_pc,pes_x4y1z1_cf)+ &
  cx_f321(nki,r,pes_x3y2z1_pc,pes_x3y2z1_cf)+ &
  cx_f402(nki,r,pes_x4z2_pc,pes_x4z2_cf)+ &
  cx_f312(nki,r,pes_x3y1z2_pc,pes_x3y1z2_cf)+ &
  cx_f222(nki,r,pes_x2y2z2_pc,pes_x2y2z2_cf)+ &
  cx_f421(nki,r,pes_x4y2z1_pc,pes_x4y2z1_cf)+ &
  cx_f412(nki,r,pes_x4y1z2_pc,pes_x4y1z2_cf)+ &
  cx_f322(nki,r,pes_x3y2z2_pc,pes_x3y2z2_cf)+ &
  cx_f422(nki,r,pes_x4y2z2_pc,pes_x4y2z2_cf)
return
END FUNCTION pes_x4y2z2_pot
