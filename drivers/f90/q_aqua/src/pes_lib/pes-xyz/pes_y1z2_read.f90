SUBROUTINE pes_y1z2_read (iun, fn)
integer, intent (in) :: iun
character (len=*), intent (in) :: fn
!-----------------------------------------------------------------------
integer :: nb
open (iun, status='old', file=fn)
read (iun,*) pes_y1z2_pc
read (iun,*) nb
if (nb.ne.pes_y1z2_nb(pes_y1z2_pc%dg)) then
 stop 'pes_y1z2_read: dimension error'
endif
allocate (pes_y1z2_cf(0:nb-1))
if (1.le.nb) then
 read (iun,*) pes_y1z2_cf
endif
close (iun)
return
END SUBROUTINE pes_y1z2_read