SUBROUTINE cxv_b11 (nki, ik, pc, r, w)
integer, intent (in) :: nki(0:), ik(0:)
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: w(0:,0:)
!-----------------------------------------------------------------------
integer :: i0, j0
real (kind=wp) :: t0, r0(0:mg11_nk-1,0:mg11_nk-1), &
  y0(0:mg11_nk-1,0:mg11_nk-1), &
  w0(0:mg11_nk-1,0:cx_dim(mgv11_nb,pc%dg)-1), &
  w1(0:size(w,1)-1,0:cx_dim(mgv11_nb,pc%dg)-1)
if (any(size(nki).lt.ik)) then
 stop 'cxv_b11: bad nki, ik'
else if (size(ik).ne.mg11_nkk) then
 stop 'cxv_b11: bad dimension ik'
else if (size(r,1).ne.sum(nki).or.size(r,2).ne.sum(nki)) then
 stop 'cxv_b11: bad dimension nki, r'
else if (size(w,1).ne.sum(nki)) then
 stop 'cxv_b11: bad dimension w (1)'
else if (size(w,2).ne.cx_dim(mgv11_nb,pc%dg)) then
 stop 'cxv_b11: bad dimension w (2)'
endif
w1 = 0
if (0.le.pc%dg) then
 do j0 = sum(nki(0:ik(1)-1)), sum(nki(0:ik(1)))-1
  do i0 = sum(nki(0:ik(0)-1)), sum(nki(0:ik(0)))-1
   r0 = r((/i0,j0/),(/i0,j0/))
   t0 = cx_cut(pc,r0)
   if (t0.ne.0) then
    call cx_var (pc, r0, y0)
    call mgv11_base (pc%dg, y0, w0)
    w1((/i0,j0/),:) = w1((/i0,j0/),:)+w0*t0
   endif
  enddo
 enddo
endif
w = w1
return
END SUBROUTINE cxv_b11