SUBROUTINE mgv421111_base1 (mxd, r, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: w(0:,0:)
!-----------------------------------------------------------------------
integer, parameter :: k0=nkj(0)
integer :: k
if (size(r,1).ne.nk.or.size(r,2).ne.nk) then
 stop 'mgv421111_base1: bad size r'
else if (size(w,1).ne.nkj(1).or.size(w,2).ne.mgv421111_nb1(mxd)) then
 stop 'mgv421111_base1: bad size w'
endif
do k = 0, nkj(1)-1
 call mg4111111_base (mxd, &
   r(mgv421111_iord(:,k0+k),mgv421111_iord(:,k0+k)), w(k,:))
enddo
return
END SUBROUTINE mgv421111_base1
