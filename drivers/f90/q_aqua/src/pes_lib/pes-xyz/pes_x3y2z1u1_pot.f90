FUNCTION pes_x3y2z1u1_pot (xn) RESULT (f)
! Potential for generic X3Y2Z1U1
real (kind=wp), intent (in) :: xn(0:,0:)
real (kind=wp) :: f
!-----------------------------------------------------------------------
real (kind=wp) :: r(0:pes_x3y2z1u1_nk-1,0:pes_x3y2z1u1_nk-1)
integer, parameter :: nki(0:3)=pes_x3y2z1u1_nki
call pes_dists (xn, r)
f = pes_x1_cf*nki(0)+pes_y1_cf*nki(1)+pes_z1_cf*nki(2)+ &
  pes_u1_cf*nki(3)+ &
  cx_f2(nki,r,pes_x2_pc,pes_x2_cf)+ &
  cx_f11(nki,r,pes_x1y1_pc,pes_x1y1_cf)+ &
  cx_f02(nki,r,pes_y2_pc,pes_y2_cf)+ &
  cx_f101(nki,r,pes_x1z1_pc,pes_x1z1_cf)+ &
  cx_f011(nki,r,pes_y1z1_pc,pes_y1z1_cf)+ &
  cx_f1001(nki,r,pes_x1u1_pc,pes_x1u1_cf)+ &
  cx_f0101(nki,r,pes_y1u1_pc,pes_y1u1_cf)+ &
  cx_f0011(nki,r,pes_z1u1_pc,pes_z1u1_cf)+ &
  cx_f3(nki,r,pes_x3_pc,pes_x3_cf)+ &
  cx_f21(nki,r,pes_x2y1_pc,pes_x2y1_cf)+ &
  cx_f12(nki,r,pes_x1y2_pc,pes_x1y2_cf)+ &
  cx_f201(nki,r,pes_x2z1_pc,pes_x2z1_cf)+ &
  cx_f111(nki,r,pes_x1y1z1_pc,pes_x1y1z1_cf)+ &
  cx_f021(nki,r,pes_y2z1_pc,pes_y2z1_cf)+ &
  cx_f2001(nki,r,pes_x2u1_pc,pes_x2u1_cf)+ &
  cx_f1101(nki,r,pes_x1y1u1_pc,pes_x1y1u1_cf)+ &
  cx_f0201(nki,r,pes_y2u1_pc,pes_y2u1_cf)+ &
  cx_f1011(nki,r,pes_x1z1u1_pc,pes_x1z1u1_cf)+ &
  cx_f0111(nki,r,pes_y1z1u1_pc,pes_y1z1u1_cf)+ &
  cx_f31(nki,r,pes_x3y1_pc,pes_x3y1_cf)+ &
  cx_f22(nki,r,pes_x2y2_pc,pes_x2y2_cf)+ &
  cx_f301(nki,r,pes_x3z1_pc,pes_x3z1_cf)+ &
  cx_f211(nki,r,pes_x2y1z1_pc,pes_x2y1z1_cf)+ &
  cx_f121(nki,r,pes_x1y2z1_pc,pes_x1y2z1_cf)+ &
  cx_f3001(nki,r,pes_x3u1_pc,pes_x3u1_cf)+ &
  cx_f2101(nki,r,pes_x2y1u1_pc,pes_x2y1u1_cf)+ &
  cx_f1201(nki,r,pes_x1y2u1_pc,pes_x1y2u1_cf)+ &
  cx_f2011(nki,r,pes_x2z1u1_pc,pes_x2z1u1_cf)+ &
  cx_f1111(nki,r,pes_x1y1z1u1_pc,pes_x1y1z1u1_cf)+ &
  cx_f0211(nki,r,pes_y2z1u1_pc,pes_y2z1u1_cf)+ &
  cx_f32(nki,r,pes_x3y2_pc,pes_x3y2_cf)+ &
  cx_f311(nki,r,pes_x3y1z1_pc,pes_x3y1z1_cf)+ &
  cx_f221(nki,r,pes_x2y2z1_pc,pes_x2y2z1_cf)+ &
  cx_f3101(nki,r,pes_x3y1u1_pc,pes_x3y1u1_cf)+ &
  cx_f2201(nki,r,pes_x2y2u1_pc,pes_x2y2u1_cf)+ &
  cx_f3011(nki,r,pes_x3z1u1_pc,pes_x3z1u1_cf)+ &
  cx_f2111(nki,r,pes_x2y1z1u1_pc,pes_x2y1z1u1_cf)+ &
  cx_f1211(nki,r,pes_x1y2z1u1_pc,pes_x1y2z1u1_cf)+ &
  cx_f321(nki,r,pes_x3y2z1_pc,pes_x3y2z1_cf)+ &
  cx_f3201(nki,r,pes_x3y2u1_pc,pes_x3y2u1_cf)+ &
  cx_f3111(nki,r,pes_x3y1z1u1_pc,pes_x3y1z1u1_cf)+ &
  cx_f2211(nki,r,pes_x2y2z1u1_pc,pes_x2y2z1u1_cf)+ &
  cx_f3211(nki,r,pes_x3y2z1u1_pc,pes_x3y2z1u1_cf)
return
END FUNCTION pes_x3y2z1u1_pot