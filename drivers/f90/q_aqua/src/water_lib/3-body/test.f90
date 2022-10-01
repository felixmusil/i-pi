program main
  implicit none
  integer,pointer::a(2)
  integer,target::i1=0,i2=1
  a(1)=>i1
  a(2)=>i2
  print *,a
end program main

