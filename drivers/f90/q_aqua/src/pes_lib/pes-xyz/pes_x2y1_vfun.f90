FUNCTION pes_x2y1_vfun (xn) RESULT (f)
! Vector function for generic x2y1
real (kind=wp), intent (in) :: xn(0:,0:)
real (kind=wp) :: f(0:size(xn,2)-1)
!-----------------------------------------------------------------------
real (kind=wp) :: r(0:pes_x2y1_nk-1,0:pes_x2y1_nk-1)
integer, parameter :: nki(0:1)=pes_x2y1_nki
call pes_dists (xn, r)
! Dissociates into X1+X1Y1, X2+Y1
f = cxv_f1(nki,pes_x1_vcf)+ &
  cxv_f01(nki,pes_y1_vcf)+ &
  cxv_f2(nki,r,pes_x2_vpc,pes_x2_vcf)+ &
  cxv_f11(nki,r,pes_x1y1_vpc,pes_x1y1_vcf)+ &
  cxv_f21(nki,r,pes_x2y1_vpc,pes_x2y1_vcf)
return
END FUNCTION pes_x2y1_vfun
