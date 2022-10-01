SUBROUTINE mgv2111111_base (mxd, r, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: w(0:,0:)
!-----------------------------------------------------------------------
integer :: l1
real (kind=wp) :: wx(0:mg2111111_nb(mxd)-1)
if (size(r,1).ne.nk.or.size(r,2).ne.nk) then
 stop 'mgv2111111_base: bad size r'
else if (size(w,1).ne.nk.or.size(w,2).ne.mgv2111111_nb(mxd)) then
 stop 'mgv2111111_base: bad size w'
endif
w = 0
call mgv2111111_base0 (mxd, r, w(0:nkj(0)-1,0:mgv2111111_nb0(mxd)-1))
l1 = mgv2111111_nb0(mxd)
call mg2111111_base (mxd, r, wx)
w(nkj(0),l1:l1+size(wx)-1) = wx
w(sum(nkj(0:1)),l1+size(wx):l1+2*size(wx)-1) = wx
w(sum(nkj(0:2)),l1+2*size(wx):l1+3*size(wx)-1) = wx
w(sum(nkj(0:3)),l1+3*size(wx):l1+4*size(wx)-1) = wx
w(sum(nkj(0:4)),l1+4*size(wx):l1+5*size(wx)-1) = wx
w(sum(nkj(0:5)),l1+5*size(wx):l1+6*size(wx)-1) = wx
return
END SUBROUTINE mgv2111111_base
