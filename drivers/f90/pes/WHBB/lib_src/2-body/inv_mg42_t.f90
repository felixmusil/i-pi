module inv_mg42_t
!..use and access
use inv_share
implicit none
private
public :: mg42_gens, mg42_prims, mg42_secs, mg42_base
!..types
!..data

!..procedures
contains
include "mg42_gens.f90"
include "mg42_prims.f90"
include "mg42_secs.f90"
include "mg42_base.f90"
end module inv_mg42_t
