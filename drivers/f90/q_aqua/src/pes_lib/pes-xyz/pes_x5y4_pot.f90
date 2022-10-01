FUNCTION pes_x5y4_pot (xn) RESULT (f)
! Potential for generic X5Y4
real (kind=wp), intent (in) :: xn(0:,0:)
real (kind=wp) :: f
!-----------------------------------------------------------------------
real (kind=wp) :: r(0:pes_x5y4_nk-1,0:pes_x5y4_nk-1)
integer, parameter :: nki(0:1)=pes_x5y4_nki
call pes_dists (xn, r)
f = pes_x1_cf*nki(0)+pes_y1_cf*nki(1)+ &
  cx_f2(nki,r,pes_x2_pc,pes_x2_cf)+ &
  cx_f11(nki,r,pes_x1y1_pc,pes_x1y1_cf)+ &
  cx_f02(nki,r,pes_y2_pc,pes_y2_cf)+ &
  cx_f54(nki,r,pes_x5y4_pc,pes_x5y4_cf)
return
END FUNCTION pes_x5y4_pot
