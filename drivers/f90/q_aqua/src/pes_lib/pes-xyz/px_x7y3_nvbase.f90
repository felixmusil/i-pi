PURE FUNCTION px_x7y3_nvbase () RESULT (nb)
integer :: nb
!-----------------------------------------------------------------------
nb = cxv_nbase(pes_x7y3_nki,pes_x7y3_sysnew,px_vpcv)
return
END FUNCTION px_x7y3_nvbase