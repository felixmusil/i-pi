SUBROUTINE pes_x5y4_getcf (nki, nkj, pc, cf)
! Callback routine for inv/cx_getcf
integer, intent (in) :: nki(0:), nkj(0:)
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: cf(0:)
!-----------------------------------------------------------------------
integer :: iun
call pes_getiun (iun)
if (size(nki).ne.size(pes_x5y4_nki)) then
 stop 'pes_x5y4_getcf: bad dimension'
else if (any(nki.ne.pes_x5y4_nki)) then
 stop 'pes_x5y4_getcf: bad nki'
endif
if (all(nkj.eq.(/1,0/))) then
 call pes_x1_add (cf)
 call pes_write0 (iun, 'pcf-x1', pes_x1_cf)
else if (all(nkj.eq.(/0,1/))) then
 call pes_y1_add (cf)
 call pes_write0 (iun, 'pcf-y1', pes_y1_cf)
else if (all(nkj.eq.(/2,0/))) then
 call pes_x2_add (pc, cf)
 call pes_write (iun, 'pcf-x2', pes_x2_pc, pes_x2_cf)
else if (all(nkj.eq.(/1,1/))) then
 call pes_x1y1_add (pc, cf)
 call pes_write (iun, 'pcf-x1y1', pes_x1y1_pc, pes_x1y1_cf)
else if (all(nkj.eq.(/0,2/))) then
 call pes_y2_add (pc, cf)
 call pes_write (iun, 'pcf-y2', pes_y2_pc, pes_y2_cf)
else if (all(nkj.eq.(/5,4/))) then
 call pes_x5y4_add (pc, cf)
 call pes_write (iun, 'pcf-x5y4', &
   pes_x5y4_pc, pes_x5y4_cf)
else
 stop 'pes_x5y4_getcf: bad nkj'
end if
return
END SUBROUTINE pes_x5y4_getcf