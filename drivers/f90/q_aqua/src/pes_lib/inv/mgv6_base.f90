SUBROUTINE mgv6_base (mxd, r, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: w(0:,0:)
!-----------------------------------------------------------------------
if (size(r,1).ne.nk.or.size(r,2).ne.nk) then
 stop 'mgv6_base: bad size r'
else if (size(w,1).ne.nk.or.size(w,2).ne.mgv6_nb(mxd)) then
 stop 'mgv6_base: bad size w'
endif
call mgv6_base0 (mxd, r, w)
return
END SUBROUTINE mgv6_base
