PURE FUNCTION cx_nbx (nkj, dg) RESULT (nb)
integer, intent (in) :: nkj(0:), dg
integer :: nb
!-----------------------------------------------------------------------
! Note: All error conditions are indicated by returning nb = -huge(0)
integer :: id
if (dg.lt.0) then
 nb = 0
 return
endif
select case (mgx_id(nkj))
case (0)
 nb = 0
case (1)
 nb = 1 !! must look into this
case (mg2_id)
 nb = mg2_nb(dg)
case (mg11_id)
 nb = mg11_nb(dg)
case (mg3_id)
 nb = mg3_nb(dg)
case (mg21_id)
 nb = mg21_nb(dg)
case (mg111_id)
 nb = mg111_nb(dg)
case (mg4_id)
 nb = mg4_nb(dg)
case (mg31_id)
 nb = mg31_nb(dg)
case (mg22_id)
 nb = mg22_nb(dg)
case (mg211_id)
 nb = mg211_nb(dg)
case (mg1111_id)
 nb = mg1111_nb(dg)
case (mg5_id)
 nb = mg5_nb(dg)
case (mg41_id)
 nb = mg41_nb(dg)
case (mg32_id)
 nb = mg32_nb(dg)
case (mg311_id)
 nb = mg311_nb(dg)
case (mg221_id)
 nb = mg221_nb(dg)
case (mg2111_id)
 nb = mg2111_nb(dg)
case (mg11111_id)
 nb = mg11111_nb(dg)
case (mg6_id)
 nb = mg6_nb(dg)
case (mg51_id)
 nb = mg51_nb(dg)
case (mg42_id)
 nb = mg42_nb(dg)
case (mg33_id)
 nb = mg33_nb(dg)
case (mg411_id)
 nb = mg411_nb(dg)
case (mg321_id)
 nb = mg321_nb(dg)
case (mg222_id)
 nb = mg222_nb(dg)
case (mg3111_id)
 nb = mg3111_nb(dg)
case (mg2211_id)
 nb = mg2211_nb(dg)
case (mg21111_id)
 nb = mg21111_nb(dg)
case (mg111111_id)
 nb = mg111111_nb(dg)
case (mg7_id)
 nb = mg7_nb(dg)
case (mg61_id)
 nb = mg61_nb(dg)
case (mg52_id)
 nb = mg52_nb(dg)
case (mg43_id)
 nb = mg43_nb(dg)
case (mg511_id)
 nb = mg511_nb(dg)
case (mg421_id)
 nb = mg421_nb(dg)
case (mg331_id)
 nb = mg331_nb(dg)
case (mg322_id)
 nb = mg322_nb(dg)
case (mg4111_id)
 nb = mg4111_nb(dg)
case (mg3211_id)
 nb = mg3211_nb(dg)
case (mg2221_id)
 nb = mg2221_nb(dg)
case (mg31111_id)
 nb = mg31111_nb(dg)
case (mg22111_id)
 nb = mg22111_nb(dg)
case (mg211111_id)
 nb = mg211111_nb(dg)
case (mg1111111_id)
 nb = mg1111111_nb(dg)
!! case (mg8_id)
!!  nb = mg8_nb(dg)
case (mg71_id)
 nb = mg71_nb(dg)
case (mg62_id)
 nb = mg62_nb(dg)
case (mg53_id)
 nb = mg53_nb(dg)
case (mg44_id)
 nb = mg44_nb(dg)
case (mg611_id)
 nb = mg611_nb(dg)
case (mg521_id)
 nb = mg521_nb(dg)
case (mg431_id)
 nb = mg431_nb(dg)
case (mg422_id)
 nb = mg422_nb(dg)
case (mg332_id)
 nb = mg332_nb(dg)
case (mg5111_id)
 nb = mg5111_nb(dg)
case (mg4211_id)
 nb = mg4211_nb(dg)
case (mg3311_id)
 nb = mg3311_nb(dg)
case (mg3221_id)
 nb = mg3221_nb(dg)
case (mg2222_id)
 nb = mg2222_nb(dg)
case (mg41111_id)
 nb = mg41111_nb(dg)
case (mg32111_id)
 nb = mg32111_nb(dg)
case (mg22211_id)
 nb = mg22211_nb(dg)
case (mg311111_id)
 nb = mg311111_nb(dg)
case (mg221111_id)
 nb = mg221111_nb(dg)
case (mg2111111_id)
 nb = mg2111111_nb(dg)
case (mg11111111_id)
 nb = mg11111111_nb(dg)
!! case (mg9_id)
!!  nb = mg9_nb(dg)
!! case (mg81_id)
!!  nb = mg81_nb(dg)
case (mg72_id)
 nb = mg72_nb(dg)
case (mg63_id)
 nb = mg63_nb(dg)
case (mg54_id)
 nb = mg54_nb(dg)
case (mg711_id)
 nb = mg711_nb(dg)
case (mg621_id)
 nb = mg621_nb(dg)
case (mg531_id)
 nb = mg531_nb(dg)
case (mg441_id)
 nb = mg441_nb(dg)
case (mg522_id)
 nb = mg522_nb(dg)
case (mg432_id)
 nb = mg432_nb(dg)
case (mg333_id)
 nb = mg333_nb(dg)
case (mg6111_id)
 nb = mg6111_nb(dg)
case (mg5211_id)
 nb = mg5211_nb(dg)
case (mg4311_id)
 nb = mg4311_nb(dg)
case (mg4221_id)
 nb = mg4221_nb(dg)
case (mg3321_id)
 nb = mg3321_nb(dg)
case (mg3222_id)
 nb = mg3222_nb(dg)
case (mg51111_id)
 nb = mg51111_nb(dg)
case (mg42111_id)
 nb = mg42111_nb(dg)
case (mg33111_id)
 nb = mg33111_nb(dg)
case (mg32211_id)
 nb = mg32211_nb(dg)
case (mg22221_id)
 nb = mg22221_nb(dg)
case (mg411111_id)
 nb = mg411111_nb(dg)
case (mg321111_id)
 nb = mg321111_nb(dg)
case (mg222111_id)
 nb = mg222111_nb(dg)
case (mg3111111_id)
 nb = mg3111111_nb(dg)
case (mg2211111_id)
 nb = mg2211111_nb(dg)
case (mg21111111_id)
 nb = mg21111111_nb(dg)
case (mg111111111_id)
 nb = mg111111111_nb(dg)
case (mg73_id)
 nb = mg73_nb(dg)
case (mg721_id)
 nb = mg721_nb(dg)
case (mg631_id)
 nb = mg631_nb(dg)
case (mg541_id)
 nb = mg541_nb(dg)
case (mg622_id)
 nb = mg622_nb(dg)
case (mg532_id)
 nb = mg532_nb(dg)
case (mg442_id)
 nb = mg442_nb(dg)
case (mg433_id)
 nb = mg433_nb(dg)
case (mg7111_id)
 nb = mg7111_nb(dg)
case (mg6211_id)
 nb = mg6211_nb(dg)
case (mg5311_id)
 nb = mg5311_nb(dg)
case (mg4411_id)
 nb = mg4411_nb(dg)
case (mg5221_id)
 nb = mg5221_nb(dg)
case (mg4321_id)
 nb = mg4321_nb(dg)
case (mg3331_id)
 nb = mg3331_nb(dg)
case (mg4222_id)
 nb = mg4222_nb(dg)
case (mg3322_id)
 nb = mg3322_nb(dg)
case (mg61111_id)
 nb = mg61111_nb(dg)
case (mg52111_id)
 nb = mg52111_nb(dg)
case (mg43111_id)
 nb = mg43111_nb(dg)
case (mg42211_id)
 nb = mg42211_nb(dg)
case (mg33211_id)
 nb = mg33211_nb(dg)
case (mg32221_id)
 nb = mg32221_nb(dg)
case (mg22222_id)
 nb = mg22222_nb(dg)
case (mg511111_id)
 nb = mg511111_nb(dg)
case (mg421111_id)
 nb = mg421111_nb(dg)
case (mg331111_id)
 nb = mg331111_nb(dg)
case (mg322111_id)
 nb = mg322111_nb(dg)
case (mg222211_id)
 nb = mg222211_nb(dg)
case (mg4111111_id)
 nb = mg4111111_nb(dg)
case (mg3211111_id)
 nb = mg3211111_nb(dg)
case (mg2221111_id)
 nb = mg2221111_nb(dg)
case (mg31111111_id)
 nb = mg31111111_nb(dg)
case (mg22111111_id)
 nb = mg22111111_nb(dg)
case (mg211111111_id)
 nb = mg211111111_nb(dg)
case (mg1111111111_id)
 nb = mg1111111111_nb(dg)
case default
 nb = -huge(0)
end select
return
END FUNCTION cx_nbx