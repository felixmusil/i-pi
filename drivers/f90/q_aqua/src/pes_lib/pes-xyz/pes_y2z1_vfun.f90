FUNCTION pes_y2z1_vfun (xn) RESULT (f)
! Vector function for generic Y2Z1
real (kind=wp), intent (in) :: xn(0:,0:)
real (kind=wp) :: f(0:size(xn,2)-1)
!-----------------------------------------------------------------------
real (kind=wp) :: r(0:pes_y2z1_nk-1,0:pes_y2z1_nk-1)
integer, parameter :: nki(0:1)=pes_y2z1_nki
call pes_dists (xn, r)
f = cxv_f1(nki,pes_y1_vcf)+ &
  cxv_f01(nki,pes_z1_vcf)+ &
  cxv_f2(nki,r,pes_y2_vpc,pes_y2_vcf)+ &
  cxv_f11(nki,r,pes_y1z1_vpc,pes_y1z1_vcf)+ &
  cxv_f21(nki,r,pes_y2z1_vpc,pes_y2z1_vcf)
return
END FUNCTION pes_y2z1_vfun