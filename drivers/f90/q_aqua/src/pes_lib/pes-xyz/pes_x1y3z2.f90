MODULE pes_x1y3z2
!..use and access
use inv, wp=>inv_wp
use pes0
private
public :: pes_x1y3z2_read, pes_x1y3z2_add, pes_x1y3z2_vread, &
  pes_x1y3z2_vadd
!..data
save
integer, parameter, public :: &
  pes_x1y3z2_nki(0:2)=(/1,3,2/), pes_x1y3z2_nk=6, &
  pes_x1y3z2_nb(-1:size(cx_nb321(-1:))-2)=cx_nb321(-1:), &
  pes_x1y3z2_nvb(-1:size(cxv_nb321(-1:))-2)=cxv_nb321(-1:)
type (cx_t), public :: &
  pes_x1y3z2_pc = cx_null, &
  pes_x1y3z2_vpc = cx_null
real (kind=wp), allocatable, public :: &
  pes_x1y3z2_cf(:), pes_x1y3z2_vcf(:)
!..procedures
CONTAINS
include 'pes_x1y3z2_read.f90'
include 'pes_x1y3z2_add.f90'
include 'pes_x1y3z2_vread.f90'
include 'pes_x1y3z2_vadd.f90'
END MODULE pes_x1y3z2
