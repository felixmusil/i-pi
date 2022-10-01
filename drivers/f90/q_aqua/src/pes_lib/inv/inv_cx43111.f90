MODULE inv_cx43111
!..use and access
use inv_core
use inv_mg43111
use inv_cxx
implicit none
private
!..procedures
public :: cx_b43111, cx_f43111
!..data
integer, parameter, public :: &
  cx_nb43111(-1:ubound(mg43111_nb,dim=1))=(/0,mg43111_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b43111.f90'
include 'cx_f43111.f90'
END MODULE inv_cx43111