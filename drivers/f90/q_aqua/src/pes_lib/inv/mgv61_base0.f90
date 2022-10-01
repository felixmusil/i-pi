SUBROUTINE mgv61_base0 (mxd, r, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: w(0:,0:)
!-----------------------------------------------------------------------
integer :: k
if (size(r,1).ne.nk.or.size(r,2).ne.nk) then
 stop 'mgv61_base0: bad size r'
else if (size(w,1).ne.nkj(0).or.size(w,2).ne.mgv61_nb0(mxd)) then
 stop 'mgv61_base0: bad size w'
endif
do k = 0, nkj(0)-1
 call mg511_base (mxd, r(mgv61_iord(:,k),mgv61_iord(:,k)), w(k,:))
enddo
return
END SUBROUTINE mgv61_base0
