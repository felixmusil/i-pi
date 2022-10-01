SUBROUTINE mg3_prims (r, u)
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: u(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: x(0:nr-1)
if (size(r,1).ne.nk.or.size(r,2).ne.nk.or.size(u).ne.nr) then
 stop 'mg3_prims: bad dimensions'
endif
call mgx_mk1d (nkj, r, x)
call cg3_prims (x, u)
return
END SUBROUTINE mg3_prims
