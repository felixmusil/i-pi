SUBROUTINE mg111111111_base (mxd, r, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: w(0:mg111111111_nb(mxd)-1)
!-----------------------------------------------------------------------
integer :: k, l0, m0, l, n, ind
real (kind=wp) :: u(0:nr-1), v(0:mg111111111_nsc(mxd)-1), &
  w0(0:mg111111111_npb(mxd)-1)
if (size(r,1).ne.nk.or.size(r,2).ne.nk) then
 stop 'mg111111111_base: bad size r'
!! else if (size(w).ne.mg111111111_nb(mxd)) then
!!  stop 'mg111111111_base: bad size w'
endif
call mg111111111_prims (r, u)
call mg111111111_prib (mxd, u, w0)
call mg111111111_secs (mxd, r, v)
! trivial code for this special case
if (size(v).eq.1) then
 w = v(0)*w0
else
 stop 'mg111111111_base: bad element count'
endif
return
END SUBROUTINE mg111111111_base