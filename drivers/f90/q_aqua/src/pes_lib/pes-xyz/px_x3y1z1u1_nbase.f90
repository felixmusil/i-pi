PURE FUNCTION px_x3y1z1u1_nbase () RESULT (nb)
integer :: nb
!-----------------------------------------------------------------------
nb = cx_nbase(pes_x3y1z1u1_nki,pes_x3y1z1u1_sysnew,px_pcv)
return
END FUNCTION px_x3y1z1u1_nbase