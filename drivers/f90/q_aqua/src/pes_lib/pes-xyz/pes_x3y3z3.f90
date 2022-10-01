MODULE pes_x3y3z3
!..use and access
use inv, wp=>inv_wp
use pes0
use pes_x1
use pes_y1
use pes_z1
use pes_x2
use pes_x1y1
use pes_y2
use pes_x1z1
use pes_y1z1
use pes_z2
use pes_x3
use pes_x2y1
use pes_x1y2
use pes_y3
use pes_x2z1
use pes_x1y1z1
use pes_y2z1
use pes_x1z2
use pes_y1z2
use pes_z3
use pes_x3y1
use pes_x2y2
use pes_x1y3
use pes_x3z1
use pes_x2y1z1
use pes_x1y2z1
use pes_y3z1
use pes_x2z2
use pes_x1y1z2
use pes_y2z2
use pes_x1z3
use pes_y1z3
use pes_x3y2
use pes_x2y3
use pes_x3y1z1
use pes_x2y2z1
use pes_x1y3z1
use pes_x3z2
use pes_x2y1z2
use pes_x1y2z2
use pes_y3z2
use pes_x2z3
use pes_x1y1z3
use pes_y2z3
use pes_x3y3
use pes_x3y2z1
use pes_x2y3z1
use pes_x3y1z2
use pes_x2y2z2
use pes_x1y3z2
use pes_x3z3
use pes_x2y1z3
use pes_x1y2z3
use pes_y3z3
use pes_x3y3z1
use pes_x3y2z2
use pes_x2y3z2
use pes_x3y1z3
use pes_x2y2z3
use pes_x1y3z3
use pes_x3y3z2
use pes_x3y2z3
use pes_x2y3z3
private
public :: pes_x3y3z3_read, pes_x3y3z3_pot, pes_x3y3z3_add, &
  pes_x3y3z3_getcf, pes_x3y3z3_vread, pes_x3y3z3_vfun, &
  pes_x3y3z3_vadd, pes_x3y3z3_getvcf
!..data
save
integer, parameter, public :: &
  pes_x3y3z3_nki(0:2)=(/3,3,3/), pes_x3y3z3_nk=9, &
  pes_x3y3z3_nb(-1:size(cx_nb333(-1:))-2)=cx_nb333(-1:), &
  pes_x3y3z3_nvb(-1:size(cxv_nb333(-1:))-2)=cxv_nb333(-1:)
character (len=*), parameter, public :: &
  pes_x3y3z3_sysall='x1 y1 z1 x2 x1y1 y2 x1z1 y1z1 z2 '// &
    'x3 x2y1 x1y2 y3 x2z1 x1y1z1 y2z1 x1z2 y1z2 z3 '// &
    'x3y1 x2y2 x1y3 x3z1 x2y1z1 x1y2z1 y3z1 '// &
    'x2z2 x1y1z2 y2z2 x1z3 y1z3 '// &
    'x3y2 x2y3 x3y1z1 x2y2z1 x1y3z1 '// &
    'x3z2 x2y1z2 x1y2z2 y3z2 x2z3 x1y1z3 y2z3 '// &
    'x3y3 x3y2z1 x2y3z1 x3y1z2 x2y2z2 x1y3z2 x3z3 x2y1z3 x1y2z3 y3z3 '// &
    'x3y3z1 x3y2z2 x2y3z2 x3y1z3 x2y2z3 x1y3z3 '// &
    'x3y3z2 x3y2z3 x2y3z3 '// &
    'x3y3z3'
type (cx_t), public :: &
  pes_x3y3z3_pc = cx_null, &
  pes_x3y3z3_vpc = cx_null
real (kind=wp), allocatable, public :: &
  pes_x3y3z3_cf(:), &
  pes_x3y3z3_vcf(:)
!..procedures
CONTAINS
include 'pes_x3y3z3_read.f90'
include 'pes_x3y3z3_pot.f90'
include 'pes_x3y3z3_add.f90'
include 'pes_x3y3z3_getcf.f90'
include 'pes_x3y3z3_vread.f90'
include 'pes_x3y3z3_vfun.f90'
include 'pes_x3y3z3_vadd.f90'
include 'pes_x3y3z3_getvcf.f90'
END MODULE pes_x3y3z3
