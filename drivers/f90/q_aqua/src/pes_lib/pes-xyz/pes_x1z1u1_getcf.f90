SUBROUTINE pes_x1z1u1_getcf (nki, nkj, pc, cf)
! Callback routine for inv/cx_getcf
integer, intent (in) :: nki(0:), nkj(0:)
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: cf(0:)
!-----------------------------------------------------------------------
integer :: iun
call pes_getiun (iun)
if (size(nki).ne.size(pes_x1z1u1_nki)) then
 stop 'pes_x1z1u1_getcf: bad dimension'
else if (any(nki.ne.pes_x1z1u1_nki)) then
 stop 'pes_x1z1u1_getcf: bad nki'
endif
if (all(nkj.eq.(/1,0,0/))) then
 call pes_x1_add (cf)
 call pes_write0 (iun, 'pcf-x1', pes_x1_cf)
else if (all(nkj.eq.(/0,1,0/))) then
 call pes_z1_add (cf)
 call pes_write0 (iun, 'pcf-z1', pes_z1_cf)
else if (all(nkj.eq.(/0,0,1/))) then
 call pes_u1_add (cf)
 call pes_write0 (iun, 'pcf-u1', pes_u1_cf)
else if (all(nkj.eq.(/1,1,0/))) then
 call pes_x1z1_add (pc, cf)
 call pes_write (iun, 'pcf-x1z1', pes_x1z1_pc, pes_x1z1_cf)
else if (all(nkj.eq.(/1,0,1/))) then
 call pes_x1u1_add (pc, cf)
 call pes_write (iun, 'pcf-x1u1', pes_x1u1_pc, pes_x1u1_cf)
else if (all(nkj.eq.(/0,1,1/))) then
 call pes_z1u1_add (pc, cf)
 call pes_write (iun, 'pcf-z1u1', pes_z1u1_pc, pes_z1u1_cf)
else if (all(nkj.eq.(/1,1,1/))) then
 call pes_x1z1u1_add (pc, cf)
 call pes_write (iun, 'pcf-x1z1u1', &
   pes_x1z1u1_pc, pes_x1z1u1_cf)
else
 stop 'pes_x1z1u1_getcf: bad nkj'
end if
return
END SUBROUTINE pes_x1z1u1_getcf