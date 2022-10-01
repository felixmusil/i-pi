SUBROUTINE cx_b54 (nki, ik, pc, r, w)
integer, intent (in) :: nki(0:), ik(0:)
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: w(0:)
!-----------------------------------------------------------------------
integer :: i0, i1, i2, i3, i4, j0, j1, j2, j3
real (kind=wp) :: t0, r0(0:mg54_nk-1,0:mg54_nk-1), &
  y0(0:mg54_nk-1,0:mg54_nk-1), &
  w0(0:cx_dim(mg54_nb,pc%dg)-1), w1(0:cx_dim(mg54_nb,pc%dg)-1)
if (any(size(nki).lt.ik)) then
 stop 'cx_b54: bad nki, ik'
else if (size(ik).ne.mg54_nkk) then
 stop 'cx_b54: bad dimension ik'
else if (size(r,1).ne.sum(nki).or.size(r,2).ne.sum(nki)) then
 stop 'cx_b54: bad dimension nki, r'
else if (size(w).ne.cx_dim(mg54_nb,pc%dg)) then
 stop 'cx_b54: bad dimension w'
endif
w1 = 0
if (0.le.pc%dg) then
 do j3 = sum(nki(0:ik(1)-1))+3, sum(nki(0:ik(1)))-1
  do j2 = sum(nki(0:ik(1)-1))+2, j3-1
   do j1 = sum(nki(0:ik(1)-1))+1, j2-1
    do j0 = sum(nki(0:ik(1)-1)), j1-1
     do i4 = sum(nki(0:ik(0)-1))+4, sum(nki(0:ik(0)))-1
      do i3 = sum(nki(0:ik(0)-1))+3, i4-1
       do i2 = sum(nki(0:ik(0)-1))+2, i3-1
        do i1 = sum(nki(0:ik(0)-1))+1, i2-1
         do i0 = sum(nki(0:ik(0)-1)), i1-1
          r0 = r((/i0,i1,i2,i3,i4,j0,j1,j2,j3/), &
            (/i0,i1,i2,i3,i4,j0,j1,j2,j3/))
          t0 = cx_cut(pc,r0)
          if (t0.ne.0) then
           call cx_var (pc, r0, y0)
           call mg54_base (pc%dg, y0, w0)
           w1 = w1+w0*t0
          endif
         enddo
        enddo
       enddo
      enddo
     enddo
    enddo
   enddo
  enddo
 enddo
endif
w = w1
return
END SUBROUTINE cx_b54
