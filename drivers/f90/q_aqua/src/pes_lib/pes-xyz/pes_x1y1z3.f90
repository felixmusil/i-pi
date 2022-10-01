MODULE pes_x1y1z3
!..use and access
use inv, wp=>inv_wp
use pes0
private
public :: pes_x1y1z3_read, pes_x1y1z3_add, &
  pes_x1y1z3_vread, pes_x1y1z3_vadd
!..data
save
integer, parameter, public :: &
  pes_x1y1z3_nki(0:2)=(/1,1,3/), pes_x1y1z3_nk=5, &
  pes_x1y1z3_nb(-1:size(cx_nb311(-1:))-2)=cx_nb311(-1:), &
  pes_x1y1z3_nvb(-1:size(cxv_nb311(-1:))-2)=cxv_nb311(-1:)
type (cx_t), public :: &
  pes_x1y1z3_pc = cx_null, &
  pes_x1y1z3_vpc = cx_null
real (kind=wp), allocatable, public :: &
  pes_x1y1z3_cf(:), &
  pes_x1y1z3_vcf(:)
!..procedures
CONTAINS
include 'pes_x1y1z3_read.f90'
include 'pes_x1y1z3_add.f90'
include 'pes_x1y1z3_vread.f90'
include 'pes_x1y1z3_vadd.f90'
END MODULE pes_x1y1z3