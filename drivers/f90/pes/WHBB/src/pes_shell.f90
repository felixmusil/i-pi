module pes_shell
  use pot_monomer_mod
  use constants
  implicit none

  ! parameters for switching function
  ! smaller than ri: use PIP fit
  ! between ri and rf: linear combination of PIP fit and TTM3-F (for 2b), or
  !                    linear combination of PIP fit and zero (for 3b)
  ! greater than rf: use TTM3-F (2b) or simply zero (3b)
  real,parameter::r2i=6.5/auang
  real,parameter::r2f=7.5/auang
  real,parameter::r3i=5.5/auang
  real,parameter::r3f=6.5/auang

  ! some global variables
  integer,dimension(:,:),allocatable::idx_3b,idx_2b

contains
  include 'nbody.f90'
  !=================================================!
  ! Initializing HBB water potential                !
  !=================================================!
  subroutine pes_init(nw)
    integer,intent(in)::nw
    !::::::::::::::::::::

    ! 3-body init
    call pes_init_3b('./') ! Change this for another version of the fit
    allocate(idx_3b(9,nw*(nw-1)*(nw-2)/6))
    call map_3b(nw,idx_3b)

    ! 2-body init
    call prepot_2b('./coef.pes2b.dat')
    allocate(idx_2b(6,nw*(nw-1)/2))
    call map_2b(nw,idx_2b)

    ! monomer init
    call monomer_init()

    return
  end subroutine pes_init

  !==================================================!
  ! water potential                                  !
  !   x(1:9N): cartesian coordinates in bohr, where  !
  !            N is the number of water monomers     !
  !            must be in order H H H H ... O O ...  !
  !  im: optional augument should only be used for   !
  !      efficient finite difference gradient        !
  !  jm: optional augument should only be used for   !
  !      efficient finite-difference hessian         !
  ! * To obtain the full potential of water cluster, !
  !   one should call this function with ONLY ONE    !
  !   argument, simply as f(x)                       !
  !==================================================!
  function f(x,im,jm) result(pot)
    real,dimension(:),intent(in)::x
    integer,optional,intent(in)::im,jm
    real::pot
    ! ::::::::::::::::::::
    real,dimension(3,1:size(x)/3)::xn
    real::p1,p2,p3,pmb
    integer::natm

    natm=size(x)/3
    xn=reshape(x,(/3,natm/))

    p1=0.d0; p2=0.d0; p3=0.d0; pmb=0.d0

    if (present(jm)) then ! these are used to compute finite-difference Hessian efficiently
       call pot1b_h(natm,xn,p1,im,jm)
       call potc2bt1_h(natm,xn,p2,im,jm)
       call potc3b_h(natm,xn,p3,im,jm)
    elseif (present(im))then ! these are used to compute finite-difference gradient efficiently
       call pot1b_g(natm,xn,p1,im)
       call potc2bt1_g(natm,xn,p2,im)
       call potc3b_g(natm,xn,p3,im)
    else ! these are just used to compute the potential energy
       call pot1b(natm,xn,p1)    ! 1-body
       call potc2bt1(natm,xn,p2) ! 2-body
       call potc3b(natm,xn,p3)   ! 3-body
    end if
    call potmb(natm,xn,pmb) ! include TTM3-F 4b and higher-order interactions
    pot=p1+p2+p3+pmb

    return
  end function f

  !==================================================!
  ! efficient numerical gradient of the potential    !
  !==================================================!
  function gradient(x) result(gd)
    real,dimension(:),intent(in)::x
    !::::::::::::::::::::
    real::eps
    real,dimension(1:size(x))::xt,gd
    real::fa,fb
    integer::dim,i,im

    eps=0.001d0
    dim=size(x)

    do i=1,dim
       ! im: index of the atom
       im=(i+2)/3
       xt=x; xt(i)=xt(i)-eps
       fa=f(xt,im)
       xt=x; xt(i)=xt(i)+eps
       fb=f(xt,im)
       gd(i)=0.5d0*(fb-fa)/eps
    end do

    return
  end function gradient

  !========================================!
  ! efficient numerical Hessian of WHBB    !
  !========================================!
  subroutine hessian(x,H)
    real,dimension(:),intent(in)::x
    real,dimension(:,:),intent(inout)::H
    !:::::::::::::::::::::::::::
    real::f_ff,f_fb,f_bf,f_bb,fx
    real::rm,eps
    real,dimension(1:size(x))::tx
    integer,dimension(:,:),allocatable::idx_hh
    integer::dim,i,j,ii,jj,k,l

    allocate(idx_hh(4,1:size(x,1)*(size(x,1)-1)/2))

    tx=x
    fx=f(tx)
    dim=size(x)
    eps=0.001d0

    k=1
    do i=1,dim-1
       ii=(i+2)/3
       do j=i+1,dim
          jj=(j+2)/3
          idx_hh(1:4,k)=(/i,ii,j,jj/)
          k=k+1
       end do
    end do
    l=k-1

    do k=1,l
       i  = idx_hh(1,k)
       ii = idx_hh(2,k)
       j  = idx_hh(3,k)
       jj = idx_hh(4,k)
       tx=x; tx(i)=tx(i)+eps; tx(j)=tx(j)+eps; f_ff=f(tx,ii,jj)
       tx=x; tx(i)=tx(i)+eps; tx(j)=tx(j)-eps; f_fb=f(tx,ii,jj)
       tx=x; tx(i)=tx(i)-eps; tx(j)=tx(j)+eps; f_bf=f(tx,ii,jj)
       tx=x; tx(i)=tx(i)-eps; tx(j)=tx(j)-eps; f_bb=f(tx,ii,jj)

       H(i,j)=0.25*(f_ff-f_fb-f_bf+f_bb)/eps/eps
       H(j,i)=H(i,j)

    end do

    do i=1,dim
       tx=x; tx(i)=tx(i)+eps; f_ff=f(tx)
       tx=x; tx(i)=tx(i)-eps; f_bb=f(tx)
       H(i,i)=(f_ff-2*fx+f_bb)/eps/eps
    end do

    return
  end subroutine hessian

end module pes_shell
