SUBROUTINE pes_y1u1_vadd (pc, cf)
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: cf(0:)
!-----------------------------------------------------------------------
integer :: nb
if (allocated(pes_y1u1_vcf).and.pes_y1u1_vpc.eq.pc) then
 pes_y1u1_vcf = pes_y1u1_vcf+cf
else
! Require that pes_y1u1_vcf is null
 if (allocated(pes_y1u1_vcf)) then
  if (any(pes_y1u1_vcf.ne.0.0_wp)) then
   stop 'pes_y1u1_vadd: mismatch'
  endif
  deallocate (pes_y1u1_vcf)
 endif
 nb = pes_y1u1_nvb(pc%dg)
 allocate (pes_y1u1_vcf(0:nb-1))
 pes_y1u1_vpc = pc
 pes_y1u1_vcf = cf
endif
return
END SUBROUTINE pes_y1u1_vadd
