SUBROUTINE pes_y2_vadd (pc, cf)
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: cf(0:)
!-----------------------------------------------------------------------
integer :: nb
if (allocated(pes_y2_vcf).and.pes_y2_vpc.eq.pc) then
 pes_y2_vcf = pes_y2_vcf+cf
else
! Require that pes_y2_vcf is null
 if (allocated(pes_y2_vcf)) then
  if (any(pes_y2_vcf.ne.0.0_wp)) then
   stop 'pes_y2_vadd: mismatch'
  endif
  deallocate (pes_y2_vcf)
 endif
 nb = pes_y2_nvb(pc%dg)
 allocate (pes_y2_vcf(0:nb-1))
 pes_y2_vpc = pc
 pes_y2_vcf = cf
endif
return
END SUBROUTINE pes_y2_vadd
