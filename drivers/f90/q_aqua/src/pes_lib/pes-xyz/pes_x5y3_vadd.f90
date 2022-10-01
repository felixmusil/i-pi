SUBROUTINE pes_x5y3_vadd (pc, cf)
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: cf(0:)
!-----------------------------------------------------------------------
integer :: nb
if (allocated(pes_x5y3_vcf).and.pes_x5y3_vpc.eq.pc) then
 pes_x5y3_vcf = pes_x5y3_vcf+cf
else
! Require that pes_x5y3_vcf is null
 if (allocated(pes_x5y3_vcf)) then
  if (any(pes_x5y3_vcf.ne.0.0_wp)) then
   stop 'pes_x5y3_vadd: mismatch'
  endif
  deallocate (pes_x5y3_vcf)
 endif
 nb = pes_x5y3_nvb(pc%dg)
 allocate (pes_x5y3_vcf(0:nb-1))
 pes_x5y3_vpc = pc
 pes_x5y3_vcf = cf
endif
return
END SUBROUTINE pes_x5y3_vadd