SUBROUTINE sym5_prims (x, u)
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: u(0:)
!-----------------------------------------------------------------------
if (size(x).ne.sym5_nr.or.size(u).ne.sym5_nr) then
 stop 'sym5_prims: bad dimensions'
endif
u(0) = sum(x)/size(x)
u(1) = sum(x**2)/size(x)
u(2) = sum(x**3)/size(x)
u(3) = sum(x**4)/size(x)
u(4) = sum(x**5)/size(x)
return
END SUBROUTINE sym5_prims
