PURE FUNCTION px_x6y3_nbase () RESULT (nb)
integer :: nb
!-----------------------------------------------------------------------
nb = cx_nbase(pes_x6y3_nki,pes_x6y3_sysnew,px_pcv)
return
END FUNCTION px_x6y3_nbase