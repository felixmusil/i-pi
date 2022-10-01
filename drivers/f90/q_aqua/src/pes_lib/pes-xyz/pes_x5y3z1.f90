MODULE pes_x5y3z1
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
private
public :: pes_x5y3z1_read, pes_x5y3z1_pot, pes_x5y3z1_add, pes_x5y3z1_getcf, &
  pes_x5y3z1_vread, pes_x5y3z1_vfun, pes_x5y3z1_vadd, pes_x5y3z1_getvcf
!..data
save
integer, parameter, public :: &
  pes_x5y3z1_nki(0:2)=(/5,3,1/), pes_x5y3z1_nk=9, &
  pes_x5y3z1_nb(-1:size(cx_nb531(-1:))-2)=cx_nb531(-1:), &
  pes_x5y3z1_nvb(-1:size(cxv_nb531(-1:))-2)=cxv_nb531(-1:)
character (len=*), parameter, public :: &
  pes_x5y3z1_sysall='x1 y1 z1 x2 x1y1 y2 x1z1 y1z1 x5y3z1'
type (cx_t), public :: &
  pes_x5y3z1_pc = cx_null, &
  pes_x5y3z1_vpc = cx_null
real (kind=wp), allocatable, public :: &
  pes_x5y3z1_cf(:), pes_x5y3z1_vcf(:)
!..procedures
CONTAINS
include 'pes_x5y3z1_read.f90'
include 'pes_x5y3z1_pot.f90'
include 'pes_x5y3z1_add.f90'
include 'pes_x5y3z1_getcf.f90'
include 'pes_x5y3z1_vread.f90'
include 'pes_x5y3z1_vfun.f90'
include 'pes_x5y3z1_vadd.f90'
include 'pes_x5y3z1_getvcf.f90'
END MODULE pes_x5y3z1
