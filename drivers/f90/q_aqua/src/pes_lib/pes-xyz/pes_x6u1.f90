MODULE pes_x6u1
!..use and access
use inv, wp=>inv_wp
use pes0
private
public :: pes_x6u1_read, pes_x6u1_add, &
  pes_x6u1_vread, pes_x6u1_vadd
!..data
save
integer, parameter, public :: &
  pes_x6u1_nki(0:1)=(/6,1/), pes_x6u1_nk=7, &
  pes_x6u1_nb(-1:size(cx_nb61(-1:))-2)=cx_nb61(-1:), &
  pes_x6u1_nvb(-1:size(cxv_nb61(-1:))-2)=cxv_nb61(-1:)
type (cx_t), public :: &
  pes_x6u1_pc = cx_null, &
  pes_x6u1_vpc = cx_null
real (kind=wp), allocatable, public :: &
  pes_x6u1_cf(:), pes_x6u1_vcf(:)
!..procedures
CONTAINS
include 'pes_x6u1_read.f90'
include 'pes_x6u1_add.f90'
include 'pes_x6u1_vread.f90'
include 'pes_x6u1_vadd.f90'
END MODULE pes_x6u1