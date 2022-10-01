SUBROUTINE bg54_prims (x, u)
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: u(0:)
!-----------------------------------------------------------------------
integer, parameter :: m=5, n=4
integer :: i, j
real (kind=wp) :: e(0:m-1), f(0:n-1), t0, t1, t2, t3
if (size(x).ne.bg54_nr.or.size(u).ne.bg54_nr) then
 stop 'bg54_prims: bad dimensions'
endif
do i = 0, m-1
 e(i) = sum(x(i:i+m*(n-1):m))/n
enddo
do j = 0, n-1
 f(j) = sum(x(m*j:m*(j+1)-1))/m
enddo
t0 = 0 ; t1 = 0 ; t2 = 0 ; t3 = 0
do j = 0, n-1
 do i = 0, m-1
  t0 = t0+x(i+m*j)**2*f(j)
  t1 = t1+x(i+m*j)**2*e(i)
  t2 = t2+x(i+m*j)**3*f(j)
  t3 = t3+x(i+m*j)**3*e(i)
 enddo
enddo
u(0) = sum(x)/size(x)
u(1) = sum(e**2)/size(e)
u(2) = sum(f**2)/size(f)
u(3) = sum(x**2)/size(x)
u(4) = sum(e**3)/size(e)
u(5) = sum(f**3)/size(f)
u(6) = sum(x**3)/size(x)
u(7) = t0/(m*n)
u(8) = t1/(m*n)
u(9) = sum(e**4)/size(e)
u(10) = sum(f**4)/size(f)
u(11) = sum(x**4)/size(x)
u(12) = t2/(m*n)
u(13) = t3/(m*n)
u(14) = sum(e**5)/size(e)
u(15) = sum(x**6)/size(x)
u(16) = sum(x**10)/size(x)
u(17) = sum(x**12)/size(x)
u(18) = sum(x**15)/size(x)
u(19) = sum(x**20)/size(x)
return
END SUBROUTINE bg54_prims