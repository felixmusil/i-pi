SUBROUTINE mgv711_base (mxd, r, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: w(0:,0:)
!-----------------------------------------------------------------------
integer :: l1
real (kind=wp) :: wx(0:mg711_nb(mxd)-1)
if (size(r,1).ne.nk.or.size(r,2).ne.nk) then
 stop 'mgv711_base: bad size r'
else if (size(w,1).ne.nk.or.size(w,2).ne.mgv711_nb(mxd)) then
 stop 'mgv711_base: bad size w'
endif
w = 0
call mgv711_base0 (mxd, r, w(0:nkj(0)-1,0:mgv711_nb0(mxd)-1))
l1 = mgv711_nb0(mxd)
call mg711_base (mxd, r, wx)
w(nkj(0),l1:l1+size(wx)-1) = wx
w(sum(nkj(0:1)),l1+size(wx):l1+2*size(wx)-1) = wx
return
END SUBROUTINE mgv711_base
