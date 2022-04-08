program main
  use pes_shell
  implicit none

  real,dimension(:),allocatable::x  
  integer::natm,nwater,j
  real::pot0,pot
  character::symb

  read(*,*) natm
  nwater = natm / 3
  allocate(x(3*natm))

  call pes_init(nwater)

  read(*,*)
  do j=1,natm
     read(*,*) symb,x(j*3-2:j*3)
  end do
  x=x/auang

  pot=f(x)
  write(*,'(A,F16.8)') "Potential (hartree): ", pot

end program main

