MODULE inv_mgv532
!..use and access
use inv_wp
use inv_mg532
use inv_mg4321
use inv_mg5221
use inv_mg5311
implicit none
private
public :: mgv532_base0, mgv532_base1, mgv532_base2, mgv532_base
!..data
integer, parameter, private :: &
  nkk=mg532_nkk, nk=mg532_nk, nr=mg532_nr, &
  nkj(0:nkk-1)=mg532_nkj
integer, parameter, public :: &
  mgv532_nb0(0:9) = mg4321_nb, &
  mgv532_nb1(0:9) = mg5221_nb, &
  mgv532_nb2(0:9) = mg5311_nb, &
  mgv532_nb(0:9) = mgv532_nb0+mgv532_nb1+mgv532_nb2, &
  mgv532_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,6,7,8,9,0, &
      0,2,3,4,5,6,7,8,9,1, &
      0,1,3,4,5,6,7,8,9,2, &
      0,1,2,4,5,6,7,8,9,3, &
      0,1,2,3,5,6,7,8,9,4, &
      0,1,2,3,4,6,7,8,9,5, &
      0,1,2,3,4,5,7,8,9,6, &
      0,1,2,3,4,5,6,8,9,7, &
      0,1,2,3,4,5,6,7,9,8, &
      0,1,2,3,4,5,6,7,8,9/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv532_base0.f90'
include 'mgv532_base1.f90'
include 'mgv532_base2.f90'
include 'mgv532_base.f90'
END MODULE inv_mgv532