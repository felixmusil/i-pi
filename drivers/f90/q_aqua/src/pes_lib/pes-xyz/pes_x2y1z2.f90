MODULE pes_x2y1z2
!..use and access
use inv, wp=>inv_wp
use pes0
use pes_x1
use pes_y1
use pes_z1
use pes_x2
use pes_x1y1
use pes_x1z1
use pes_y1z1
use pes_z2
use pes_x2y1
use pes_x2z1
use pes_x1y1z1
use pes_x1z2
use pes_y1z2
use pes_x2y1z1
use pes_x2z2
use pes_x1y1z2
private
public :: pes_x2y1z2_read, pes_x2y1z2_pot, pes_x2y1z2_add, &
  pes_x2y1z2_getcf, pes_x2y1z2_vread, pes_x2y1z2_vfun, &
  pes_x2y1z2_vadd, pes_x2y1z2_getvcf
!..data
save
integer, parameter, public :: &
  pes_x2y1z2_nki(0:2)=(/2,1,2/), pes_x2y1z2_nk=5, &
  pes_x2y1z2_nb(-1:size(cx_nb221(-1:))-2)=cx_nb221(-1:), &
  pes_x2y1z2_nvb(-1:size(cxv_nb221(-1:))-2)=cxv_nb221(-1:)
character (len=*), parameter, public :: &
  pes_x2y1z2_sysall='x1 y1 z1 x2 x1y1 x1z1 y1z1 z2 '// &
    'x2y1 x2z1 x1y1z1 x1z2 y1z2 x2y1z1 x2z2 x1y1z2 x2y1z2'
type (cx_t), public :: &
  pes_x2y1z2_pc = cx_null, &
  pes_x2y1z2_vpc = cx_null
real (kind=wp), allocatable, public :: &
  pes_x2y1z2_cf(:), &
  pes_x2y1z2_vcf(:)
!..procedures
CONTAINS
include 'pes_x2y1z2_read.f90'
include 'pes_x2y1z2_pot.f90'
include 'pes_x2y1z2_add.f90'
include 'pes_x2y1z2_getcf.f90'
include 'pes_x2y1z2_vread.f90'
include 'pes_x2y1z2_vfun.f90'
include 'pes_x2y1z2_vadd.f90'
include 'pes_x2y1z2_getvcf.f90'
END MODULE pes_x2y1z2
