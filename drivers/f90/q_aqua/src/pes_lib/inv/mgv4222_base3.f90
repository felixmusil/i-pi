SUBROUTINE mgv4222_base3 (mxd, r, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: w(0:,0:)
!-----------------------------------------------------------------------
integer, parameter :: k0=nkj(0)+nkj(1)+nkj(2)
integer :: k
if (size(r,1).ne.nk.or.size(r,2).ne.nk) then
 stop 'mgv4222_base3: bad size r'
else if (size(w,1).ne.nkj(3).or.size(w,2).ne.mgv4222_nb3(mxd)) then
 stop 'mgv4222_base3: bad size w'
endif
do k = 0, nkj(3)-1
 call mg42211_base (mxd, r(mgv4222_iord(:,k0+k),mgv4222_iord(:,k0+k)), w(k,:))
enddo
return
END SUBROUTINE mgv4222_base3