FUNCTION cxv_f501 (nki, r, pc, cf) RESULT (f)
integer, intent (in) :: nki(0:)
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: r(0:,0:), cf(0:)
real (kind=wp) :: f(0:size(r,1)-1)
!-----------------------------------------------------------------------
real (kind=wp) :: w(0:size(r,1)-1,0:size(cf)-1)
call cxv_b51 (nki, (/0,2/), pc, r, w)
f = matmul(w,cf)
return
END FUNCTION cxv_f501
