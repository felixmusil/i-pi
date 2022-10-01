SUBROUTINE bg54_gens (ind, iord)
integer, intent (in) :: ind
integer, intent (out) :: iord(0:)
!-----------------------------------------------------------------------
if (size(iord).ne.bg54_nr) then
 stop 'bg54_gens: bad size iord'
endif
select case (ind)
case (0)
! permutation (0,1) on the left
! (00, 10, 20, 30, 40, 01, 11, 21, 31, 41, 02, 12, 22, 32, 42,
!  03, 13, 23, 33, 43) =>
! (10, 00, 20, 30, 40, 11, 01, 21, 31, 41, 12, 02, 22, 32, 42,
!  13, 03, 23, 33, 43)
 iord = (/ 1, 0, 2, 3, 4, 6, 5, 7, 8, 9, 11, 10, 12, 13, 14, &
  16, 15, 17, 18, 19 /)
case (1)
! permutation (0,1,2,3,4) on the left
! (00, 10, 20, 30, 40, 01, 11, 21, 31, 41, 02, 12, 22, 32, 42,
!  03, 13, 23, 33, 43) =>
! (10, 20, 30, 40, 00, 11, 21, 31, 41, 01, 12, 22, 32, 42, 02,
!  13, 23, 33, 43, 03)
 iord = (/ 1, 2, 3, 4, 0, 6, 7, 8, 9, 5, 11, 12, 13, 14, 10, &
  16, 17, 18, 19, 15 /)
case (2)
! permutation (0,1) on the right
! (00, 10, 20, 30, 40, 01, 11, 21, 31, 41, 02, 12, 22, 32, 42,
!  03, 13, 23, 33, 43) =>
! (01, 11, 21, 31, 41, 00, 10, 20, 30, 40, 02, 12, 22, 32, 42,
!  03, 13, 23, 33, 43)
 iord = (/ 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 10, 11, 12, 13, 14, &
  15, 16, 17, 18, 19 /)
case (3)
! permutation (0,1,2,3) on the right
! (00, 10, 20, 30, 40, 01, 11, 21, 31, 41, 02, 12, 22, 32, 42,
!  03, 13, 23, 33, 43) =>
! (01, 11, 21, 31, 41, 02, 12, 22, 32, 42, 03, 13, 23, 33, 43, &
!  00, 10, 20, 30, 40)
 iord = (/ 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, &
   0, 1, 2, 3, 4 /)
case default
 stop 'bg54_gens: invalid index'
end select
return
END SUBROUTINE bg54_gens
