SUBROUTINE pes_x2z1_vadd (pc, cf)
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: cf(0:)
!-----------------------------------------------------------------------
integer :: nb
if (allocated(pes_x2z1_vcf).and.pes_x2z1_vpc.eq.pc) then
 pes_x2z1_vcf = pes_x2z1_vcf+cf
else
! Require that pes_x2z1_vcf is null
 if (allocated(pes_x2z1_vcf)) then
  if (any(pes_x2z1_vcf.ne.0.0_wp)) then
   stop 'pes_x2z1_vadd: mismatch'
  endif
  deallocate (pes_x2z1_vcf)
 endif
 nb = pes_x2z1_nvb(pc%dg)
 allocate (pes_x2z1_vcf(0:nb-1))
 pes_x2z1_vpc = pc
 pes_x2z1_vcf = cf
endif
return
END SUBROUTINE pes_x2z1_vadd