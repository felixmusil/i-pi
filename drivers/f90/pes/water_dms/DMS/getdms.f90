program getpot
use dms_shell
implicit none

integer::natm,i,j,dim,file
character(len=32)::mole_name,base_name
real,dimension(:),allocatable::x
character(len=2),dimension(:),allocatable:: symb
real::dip(3)

call getarg(1,mole_name)
j=index(mole_name,'.',.true.)
base_name=mole_name(1:j-1)

!Water coordinates as: H H H H...O O O...
open(1,status='unknown',file=trim(mole_name))
read(1,*) natm
read(1,*)

dim=3*natm
call dms_init(natm/3)

allocate(x(1:dim))
allocate(symb(1:natm))

do i=1,natm
   read(1,*) symb(i),x(3*i-2),x(3*i-1),x(3*i)
   write(*,'(I4,3F15.8)') i,x(3*(i-1)+1:3*i)
end do
x=x/auang
call dipole_whbb(natm,x,dip)
write(*,*) dip

close(1)



end program
