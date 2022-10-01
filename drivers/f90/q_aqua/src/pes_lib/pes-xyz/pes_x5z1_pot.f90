FUNCTION pes_x5z1_pot (xn) RESULT (f)
! Potential for generic X5Z1
real (kind=wp), intent (in) :: xn(0:,0:)
real (kind=wp) :: f
!-----------------------------------------------------------------------
real (kind=wp) :: r(0:pes_x5z1_nk-1,0:pes_x5z1_nk-1)
integer, parameter :: nki(0:1)=pes_x5z1_nki
call pes_dists (xn, r)
f = pes_x1_cf*nki(0)+pes_z1_cf*nki(1)+ &
  cx_f2(nki,r,pes_x2_pc,pes_x2_cf)+ &
  cx_f11(nki,r,pes_x1z1_pc,pes_x1z1_cf)+ &
  cx_f3(nki,r,pes_x3_pc,pes_x3_cf)+ &
  cx_f21(nki,r,pes_x2z1_pc,pes_x2z1_cf)+ &
  cx_f4(nki,r,pes_x4_pc,pes_x4_cf)+ &
  cx_f31(nki,r,pes_x3z1_pc,pes_x3z1_cf)+ &
  cx_f5(nki,r,pes_x5_pc,pes_x5_cf)+ &
  cx_f41(nki,r,pes_x4z1_pc,pes_x4z1_cf)+ &
  cx_f51(nki,r,pes_x5z1_pc,pes_x5z1_cf)
return
END FUNCTION pes_x5z1_pot