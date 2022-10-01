SUBROUTINE pes_x8_getvcf (nki, nkj, pc, cf)
! Callback routine for inv/cx_getvcf
integer, intent (in) :: nki(0:), nkj(0:)
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: cf(0:)
!-----------------------------------------------------------------------
integer :: iun
call pes_getiun (iun)
if (size(nki).ne.size(pes_x8_nki)) then
 stop 'pes_x8_getvcf: bad dimension'
else if (any(nki.ne.pes_x8_nki)) then
 stop 'pes_x8_getvcf: bad nki'
endif
if (all(nkj.eq.(/1/))) then
 call pes_x1_vadd (cf)
 call pes_write0 (iun, 'vpcf-x1', pes_x1_vcf(0))
else if (all(nkj.eq.(/2/))) then
 call pes_x2_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x2', pes_x2_vpc, pes_x2_vcf)
else if (all(nkj.eq.(/3/))) then
 call pes_x3_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x3', pes_x3_vpc, pes_x3_vcf)
else if (all(nkj.eq.(/4/))) then
 call pes_x4_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x4', pes_x4_vpc, pes_x4_vcf)
else if (all(nkj.eq.(/5/))) then
 call pes_x5_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x5', pes_x5_vpc, pes_x5_vcf)
else if (all(nkj.eq.(/6/))) then
 call pes_x6_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x6', pes_x6_vpc, pes_x6_vcf)
else if (all(nkj.eq.(/7/))) then
 call pes_x7_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x7', pes_x7_vpc, pes_x7_vcf)
else if (all(nkj.eq.(/8/))) then
 call pes_x8_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x8', pes_x8_vpc, pes_x8_vcf)
else
 stop 'pes_x8_getvcf: bad nkj'
end if
return
END SUBROUTINE pes_x8_getvcf