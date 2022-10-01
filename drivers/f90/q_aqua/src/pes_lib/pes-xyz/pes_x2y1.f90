MODULE pes_x2y1
!..use and access
use inv, wp=>inv_wp
use pes0
use pes_x1
use pes_x2
use pes_y1
use pes_x1y1
private
public :: pes_x2y1_read, pes_x2y1_pot, pes_x2y1_add, pes_x2y1_getcf, &
  pes_x2y1_vread, pes_x2y1_vfun, pes_x2y1_vadd, pes_x2y1_getvcf
!..data
save
integer, parameter, public :: &
  pes_x2y1_nki(0:1)=(/2,1/), pes_x2y1_nk=3, &
  pes_x2y1_nb(-1:size(cx_nb21(-1:))-2)=cx_nb21(-1:), &
  pes_x2y1_nvb(-1:size(cxv_nb21(-1:))-2)=cxv_nb21(-1:)
character (len=*), parameter, public :: &
  pes_x2y1_sysall='x1 x2 y1 x1y1 x2y1'
type (cx_t), public :: &
  pes_x2y1_pc = cx_null, &
  pes_x2y1_vpc = cx_null
real (kind=wp), allocatable, public :: &
  pes_x2y1_cf(:), pes_x2y1_vcf(:)
!..procedures
CONTAINS
include 'pes_x2y1_read.f90'
include 'pes_x2y1_pot.f90'
include 'pes_x2y1_add.f90'
include 'pes_x2y1_getcf.f90'
include 'pes_x2y1_vread.f90'
include 'pes_x2y1_vfun.f90'
include 'pes_x2y1_vadd.f90'
include 'pes_x2y1_getvcf.f90'
END MODULE pes_x2y1
