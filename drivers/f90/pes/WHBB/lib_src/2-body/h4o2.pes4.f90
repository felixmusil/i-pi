        subroutine prepot_2b(dname)

!        implicit none
        implicit double precision (a-h,o-z)
        implicit integer (i-n)

        double precision V, cart_in(6,3), v0
        double precision dc0(0:5,0:5),dw0(0:5,0:5), coef(0:5226)
        character (len=*), intent(in) :: dname


!        integer,parameter :: m=1976, mr=15
!        double precision coef(0:m+3*mr-1)
!        double precision dc0,dc1,dc2,dw0,dw1,dw2

        integer i,j,k,i1,j1,k1

        common/NCOE/ms,mr
        common/h4o2coef/dc0,dw0,coef

        ms=5056 ; mr=57

        open(20,file=dname,status='old')
        read(20,*)
        read(20,*)
        read(20,*)(coef(i1),i1=0,ms+3*mr-1)
        ! write(*,*)(coef(i1),i1=0,ms+3*mr-1)
        close(20)

        ! write(*,*) "Initializing H2O-H2O potential!"
        return
        end

!***************************************************************

        subroutine calcpot(V,cart_in)

        implicit none

        integer ms,mr
        double precision dc0(0:5,0:5),dw0(0:5,0:5),coef(0:5226)

        common/NCOE/ms,mr
        common/h4o2coef/dc0,dw0,coef
        double precision V, cart_in(6,3)

        double precision cart0(6,3),cart1(6,3)
        integer i,j,k,l,i1,j1,k1,l1,i2,j2,k2,l2


        double precision rvec(0:3),d0(0:5,0:5),r0(0:5,0:5),vec(0:ms+3*mr-1)
        double precision xnuc(0:2,0:5)
        WRITE(*,*) i,"eval pot 2b calcpot"
         do j=1,3
          xnuc(j-1,0:1)=cart_in(2:3,j)
          xnuc(j-1,2:3)=cart_in(5:6,j)
          xnuc(j-1,4)=cart_in(1,j)
          xnuc(j-1,5)=cart_in(4,j)
        end do
        WRITE(*,*) i,"eval pot 2b loop"
        call getvec (ms, mr, xnuc(0:2,0:5), vec)
        WRITE(*,*) i,"eval pot 2b getvec"
        V = dot_product(coef,vec)
        WRITE(*,*) i,"eval pot 2b dot"
        V = V + 152.68465163999
        WRITE(*,*) i,"eval pot 2b end"
        return
        end subroutine calcpot

