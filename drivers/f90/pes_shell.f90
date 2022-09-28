module pes_shell
  use constants
  use pot_monomer_mod
  use bemsa2b_v8
  use bemsa3b_v1
  use bemsa3b_v9
  use bemsa4b_v3
  implicit none

  integer::nw ! number of water molecules, be specified before callling pes_init()
  integer::n4b,n3b,n2b

  ! version number of 2b and 3b
  ! 2b: order 7 fit on HBB data set, Morse 3
  ! 3b: 222111_4 in short range and 222111_3 in the long range
  ! 4b: fully purified, grouped basis, 200 coefs

  ! coefs of 2b, 3b, 4b
  real::coef2(5056)
  real::coef3_1(1182),coef3_2(13230)
  real::coef4(200)
 
  ! parameters for switching function
  real::r2i,r2f,r3i,r3f,r4i,r4f,r2cut
  real::r3bi,r3bf

  ! store the monomer charge and its derivative
  real,dimension(:,:,:),allocatable::chg_gd

  ! store the geometries
  real,dimension(:,:,:),allocatable::x2b,x3b,x4b
  integer,dimension(:,:),allocatable::loc2b,loc3b,loc4b

contains
  !=================================================!
  ! Initializing 2b, 3b, 4b potential               !
  !=================================================!
  subroutine pes_init(nwat)
    integer::i,ncoef,nwat

    r2i = 6.5/auang
    r2f = 7.8/auang
    r2cut = 14.0/auang
    r3i = 7.0/auang
    r3f = 9.0/auang
    r3bi = 5.0/auang
    r3bf = 6.0/auang
    r4i = 5.0/auang
    r4f = 5.5/auang
    
    nw = nwat
    !note: for large system, n4b can be too large
    n4b=300000
    n3b=nw*(nw-1)*(nw-2)/6
    n2b=nw*(nw-1)/2*27

    allocate(x2b(n2b,3,7))
    allocate(x3b(n3b,3,10))
    allocate(x4b(n4b,3,13))
    allocate(loc2b(n2b,2))
    allocate(loc3b(n3b,3))
    allocate(loc4b(n4b,4))

    call monomer_init()

    open(20,file='/home/qiyu/AIMB/coef/coef_2b_v8_cbs.dat',status='old')
    ncoef = size(coef2)
    do i=1,ncoef
       read (20,*) coef2(i)
    end do
    close(20)

    open(20,file='/home/qiyu/AIMB/coef/coef_3b_v6_5.0-9.5.dat',status='old')
    ncoef = size(coef3_1)
    do i=1,ncoef
       read (20,*) coef3_1(i)
    end do
    close(20)

    open(20,file='/home/qiyu/AIMB/coef/coef_3b_v9_2.0-7.0.dat',status='old')
    ncoef = size(coef3_2)
    do i=1,ncoef
       read (20,*) coef3_2(i)
    end do
    close(20)

    open(20,file='/home/qiyu/AIMB/coef/coef_4b_v3.dat',status='old')
    ncoef = size(coef4)
    do i=1,ncoef
       read (20,*) coef4(i)
    end do
    close(20)

    allocate(chg_gd(nw,3,19))

    return
  end subroutine pes_init


  subroutine fg_all(x,f_all,gd_all,virial,box)
    real,dimension(:,:),intent(in) ::x
    real,dimension(:,:),intent(inout)::virial
    real::box(3)
    real,dimension(size(x))::xn
    real::f_all
    real::gd_all(size(x,1),size(x,2))
    real::tmpg(3,size(x)/3)
    integer::i,j

    do i=1,3*nw
      xn(3*i-2:3*i)=x(i,:)
    end do

    call pot_gd(xn,f_all,tmpg,virial,box)

    do i=1,3*nw
      gd_all(i,:)=-tmpg(:,i)
    end do
   
    virial = -virial
    return
  end subroutine fg_all

  subroutine pot_gd(x,pot,gd,virial,box)
    real,dimension(:),intent(in)::x
    real::pot,virial(3,3),box(3)
    real::gd(3,size(x)/3),g1(3,3)
    real,dimension(3,size(x)/3)::xn
    integer::natm,i,j,k,l
    real::engttm,eng2b,eng3b,eng4b,eng1b
    real::ximage1(3,3),ximage2(3,3),ximage3(3,3),ximage4(3,3)
    integer::flag1,flag2,flag3
    real,dimension(:,:,:),allocatable::pg4b1,pg3b1,pg2b1
    real::pg4b(3,13),pg3b(3,10),pg2b(3,7),x1(3,3),p1
    real::oodist(nw,nw),rmax4b,rmax3b,rmax2b,rmax(6)
    integer::cnt2b,cnt3b,cnt4b
    real::watdm(3,nw),eps,dpm(3),chgm(3),tmpx1(3,3)

    natm = size(x)/3

    xn=reshape(x,(/3,natm/))
    !put system into main box
    call setPBC(xn,box)

    pot=0.d0
    gd =0.d0
    virial = 0.d0

    !start correction for 2b,3b,4b
    eng1b=0.d0
    eng2b=0.d0
    eng3b=0.d0
    eng4b=0.d0

    cnt2b=0
    cnt3b=0
    cnt4b=0

    !calculate all OO distance pairs
    oodist=0.d0
    do i=1,nw
      do j=i+1,nw
        oodist(i,j)=norm2(xn(:,2*nw+i)-xn(:,2*nw+j))
      end do
    end do

    !compute water monomer charges and their pertubations
    eps=0.001d0
    do i=1,nw
       x1(:,1:2)=xn(:,i*2-1:i*2)
       x1(:,3)=xn(:,2*nw+i)
       call dip_ltp2011(x1,dpm,chgm)
       chg_gd(i,:,1)=chgm
       do j=1,3
         do k=1,3
           tmpx1=x1
           tmpx1(j,k)=tmpx1(j,k)-eps
           call dip_ltp2011(tmpx1,dpm,chgm)
           chg_gd(i,:,6*j+2*k-6)= chgm
           tmpx1=x1
           tmpx1(j,k)=tmpx1(j,k)+eps
           call dip_ltp2011(tmpx1,dpm,chgm)
           chg_gd(i,:,6*j+2*k-6+1)= chgm
         end do
       end do
     end do
    
    !compute water 1b energy/gradient/virial
    do i=1,nw
       x1(:,1:2)=xn(:,i*2-1:i*2)
       x1(:,3)=xn(:,2*nw+i)
       call pot_gd_1b(x1,p1,g1)
       gd(:,i*2-1:i*2)=gd(:,i*2-1:i*2)+g1(:,1:2)
       gd(:,2*nw+i)=gd(:,2*nw+i)+g1(:,3)
       eng1b=eng1b+p1

       virial(1,1)=virial(1,1)+x1(1,1)*g1(1,1)+x1(1,2)*g1(1,2)+x1(1,3)*g1(1,3)
       virial(1,2)=virial(1,2)+x1(1,1)*g1(2,1)+x1(1,2)*g1(2,2)+x1(1,3)*g1(2,3)
       virial(1,3)=virial(1,3)+x1(1,1)*g1(3,1)+x1(1,2)*g1(3,2)+x1(1,3)*g1(3,3)
       virial(2,2)=virial(2,2)+x1(2,1)*g1(2,1)+x1(2,2)*g1(2,2)+x1(2,3)*g1(2,3)
       virial(2,3)=virial(2,3)+x1(2,1)*g1(3,1)+x1(2,2)*g1(3,2)+x1(2,3)*g1(3,3)
       virial(3,3)=virial(3,3)+x1(3,1)*g1(3,1)+x1(3,2)*g1(3,2)+x1(3,3)*g1(3,3)
       virial(2,1)=virial(1,2)
       virial(3,1)=virial(1,3)
       virial(3,2)=virial(2,3)

    end do

    !prepare 2b,3b,4b geometries
    do i=1,nw
      do j=i+1,nw
        ximage1(:,1:2)=xn(:,i*2-1:i*2)
        ximage1(:,3)=xn(:,2*nw+i)
        ximage2(:,1:2) = xn(:,j*2-1:j*2)
        ximage2(:,3) = xn(:,2*nw+j)
        call setPBC_image(ximage1,ximage2,flag1,box)
        if(flag1==1) then !PBC applies for dimer
           rmax(1)=norm2(ximage1(:,3)-ximage2(:,3))
        else
           rmax(1)=oodist(i,j)
        end if
        rmax2b=rmax(1)
        if(rmax2b < r2cut) then
          cnt2b=cnt2b+1
          x2b(cnt2b,:,1:2)=ximage1(:,1:2)
          x2b(cnt2b,:,3:4)=ximage2(:,1:2)
          x2b(cnt2b,:,5)=ximage1(:,3)
          x2b(cnt2b,:,6)=ximage2(:,3)
          x2b(cnt2b,1,7)=rmax2b
          x2b(cnt2b,2,7)=i
          x2b(cnt2b,3,7)=j
          loc2b(cnt2b,1)=i
          loc2b(cnt2b,2)=j
          if(rmax2b < r3f) then
            do k=j+1,0
              ximage3(:,1:2)=xn(:,k*2-1:k*2)
              ximage3(:,3)=xn(:,2*nw+k)
              call setPBC_image(ximage1,ximage3,flag2,box)       
              if(flag2==1) then !PBC applies for trimer
                rmax(2)=norm2(ximage1(:,3)-ximage3(:,3))
                rmax(3)=norm2(ximage2(:,3)-ximage3(:,3))
              else
                rmax(2)=oodist(i,k)
                if(flag1==1) then
                    rmax(3)=norm2(ximage2(:,3)-ximage3(:,3))
                else
                    rmax(3)=oodist(j,k)
                end if
              end if
              rmax3b = maxval(rmax(1:3))
              if(rmax3b < r3f) then
                cnt3b=cnt3b+1
                x3b(cnt3b,:,1:2)=ximage1(:,1:2)
                x3b(cnt3b,:,3:4)=ximage2(:,1:2)
                x3b(cnt3b,:,5:6)=ximage3(:,1:2)
                x3b(cnt3b,:,7)=ximage1(:,3)
                x3b(cnt3b,:,8)=ximage2(:,3)
                x3b(cnt3b,:,9)=ximage3(:,3)
                x3b(cnt3b,1,10)=rmax3b
                loc3b(cnt3b,1)=i
                loc3b(cnt3b,2)=j
                loc3b(cnt3b,3)=k
              end if
              if(rmax3b < r4f) then
                do l=k+1,0
                  ximage4(:,1:2)=xn(:,l*2-1:l*2)
                  ximage4(:,3)=xn(:,2*nw+l)
                  call setPBC_image(ximage1,ximage4,flag3,box)!PBC applies 
                  if(flag3==1) then
                    rmax(4)=norm2(ximage1(:,3)-ximage4(:,3))
                    rmax(5)=norm2(ximage2(:,3)-ximage4(:,3))
                    rmax(6)=norm2(ximage3(:,3)-ximage4(:,3))
                  else
                    rmax(4)=oodist(i,l)
                    if(flag1==1) then
                      rmax(5)=norm2(ximage2(:,3)-ximage4(:,3))
                    else
                      rmax(5)=oodist(j,l)
                    end if
                    if(flag2==1) then
                      rmax(6)=norm2(ximage3(:,3)-ximage4(:,3))
                    else
                      rmax(6)=oodist(k,l)
                    end if
                  end if
                  rmax4b = maxval(rmax(1:6))
                  if(rmax4b < r4f) then
                    cnt4b=cnt4b+1
                    x4b(cnt4b,:,1:2)=ximage1(:,1:2)
                    x4b(cnt4b,:,3:4)=ximage2(:,1:2)
                    x4b(cnt4b,:,5:6)=ximage3(:,1:2)
                    x4b(cnt4b,:,7:8)=ximage4(:,1:2)
                    x4b(cnt4b,:,9)=ximage1(:,3)
                    x4b(cnt4b,:,10)=ximage2(:,3)
                    x4b(cnt4b,:,11)=ximage3(:,3)
                    x4b(cnt4b,:,12)=ximage4(:,3)
                    x4b(cnt4b,1,13)=rmax4b
                    loc4b(cnt4b,1)=i
                    loc4b(cnt4b,2)=j
                    loc4b(cnt4b,3)=k
                    loc4b(cnt4b,4)=l
                 end if
               end do
             end if
           end do
         end if
       end if   
     end do
   end do

   allocate(pg2b1(cnt2b,3,7))
   allocate(pg3b1(cnt3b,3,10))
   allocate(pg4b1(cnt4b,3,13))


!$omp parallel do private(pg2b)
   do i=1,cnt2b
     call pot_gd_2b(x2b(i,:,:),pg2b)
     pg2b1(i,:,:)=pg2b(:,:)
   end do
!$omp end parallel do

   eng2b = 0.d0
   do i=1,cnt2b
     eng2b = eng2b + pg2b1(i,1,7) 
   end do

   pot = eng1b+eng2b!+eng3b+eng4b
   do i=1,cnt2b
     gd(:,2*loc2b(i,1)-1)=gd(:,2*loc2b(i,1)-1)+pg2b1(i,:,1)
     gd(:,2*loc2b(i,1))=gd(:,2*loc2b(i,1))+pg2b1(i,:,2)
     gd(:,2*loc2b(i,2)-1)=gd(:,2*loc2b(i,2)-1)+pg2b1(i,:,3)
     gd(:,2*loc2b(i,2))=gd(:,2*loc2b(i,2))+pg2b1(i,:,4)
     gd(:,2*nw+loc2b(i,1))=gd(:,2*nw+loc2b(i,1))+pg2b1(i,:,5)
     gd(:,2*nw+loc2b(i,2))=gd(:,2*nw+loc2b(i,2))+pg2b1(i,:,6)
    virial(1,1)=virial(1,1)+x2b(i,1,1)*pg2b1(i,1,1)+x2b(i,1,2)*pg2b1(i,1,2)+x2b(i,1,3)*pg2b1(i,1,3)
     virial(1,1)=virial(1,1)+x2b(i,1,4)*pg2b1(i,1,4)+x2b(i,1,5)*pg2b1(i,1,5)+x2b(i,1,6)*pg2b1(i,1,6)
     virial(1,2)=virial(1,2)+x2b(i,1,1)*pg2b1(i,2,1)+x2b(i,1,2)*pg2b1(i,2,2)+x2b(i,1,3)*pg2b1(i,2,3)
     virial(1,2)=virial(1,2)+x2b(i,1,4)*pg2b1(i,2,4)+x2b(i,1,5)*pg2b1(i,2,5)+x2b(i,1,6)*pg2b1(i,2,6)
     virial(1,3)=virial(1,3)+x2b(i,1,1)*pg2b1(i,3,1)+x2b(i,1,2)*pg2b1(i,3,2)+x2b(i,1,3)*pg2b1(i,3,3)
     virial(1,3)=virial(1,3)+x2b(i,1,4)*pg2b1(i,3,4)+x2b(i,1,5)*pg2b1(i,3,5)+x2b(i,1,6)*pg2b1(i,3,6)
     virial(2,2)=virial(2,2)+x2b(i,2,1)*pg2b1(i,2,1)+x2b(i,2,2)*pg2b1(i,2,2)+x2b(i,2,3)*pg2b1(i,2,3)
     virial(2,2)=virial(2,2)+x2b(i,2,4)*pg2b1(i,2,4)+x2b(i,2,5)*pg2b1(i,2,5)+x2b(i,2,6)*pg2b1(i,2,6)
     virial(2,3)=virial(2,3)+x2b(i,2,1)*pg2b1(i,3,1)+x2b(i,2,2)*pg2b1(i,3,2)+x2b(i,2,3)*pg2b1(i,3,3)
     virial(2,3)=virial(2,3)+x2b(i,2,4)*pg2b1(i,3,4)+x2b(i,2,5)*pg2b1(i,3,5)+x2b(i,2,6)*pg2b1(i,3,6)
     virial(3,3)=virial(3,3)+x2b(i,3,1)*pg2b1(i,3,1)+x2b(i,3,2)*pg2b1(i,3,2)+x2b(i,3,3)*pg2b1(i,3,3)
     virial(3,3)=virial(3,3)+x2b(i,3,4)*pg2b1(i,3,4)+x2b(i,3,5)*pg2b1(i,3,5)+x2b(i,3,6)*pg2b1(i,3,6)
     virial(2,1)=virial(1,2)
     virial(3,1)=virial(1,3)
     virial(3,2)=virial(2,3)
   end do
    deallocate(pg2b1)
    deallocate(pg3b1)
    deallocate(pg4b1)

   return

  end subroutine pot_gd

  subroutine pot_gd_1b(x1,p1,g1)
  real,dimension(:,:),intent(in) :: x1
  real,intent(inout) :: p1
  real,dimension(:,:),intent(inout) :: g1
  real::eps,fa,fb
  real::xt(3,3)
  integer::i,j

  eps=0.001d0
  call pot1b(x1,p1)

  do i=1,3
    do j=1,3
      xt=x1;xt(i,j)=xt(i,j)-eps;call pot1b(xt,fa)
      xt=x1;xt(i,j)=xt(i,j)+eps;call pot1b(xt,fb)
      g1(i,j)=0.5d0*(fb-fa)/eps
    end do
  end do

  return
  end subroutine pot_gd_1b

  !=================================================!
  ! Monomer potentials using Partridge-Schwenke's   !
  !=================================================!
  subroutine pot1b(xx,pot)
    real,dimension(:,:),intent(in)::xx
    real,intent(inout)::pot
    !::::::::::::::::::::
    real,dimension(1:9)::x1
    real,dimension(3,3)::xr
    real,dimension(3)::rij
    real::e1
    integer::i,j,fo

    pot=0.d0

    x1(1:3)=xx(:,1)
    x1(4:6)=xx(:,2)
    x1(7:9)=xx(:,3)
    call bond(3,x1,xr)
          !h2o pot
    rij(1)=xr(1,3)  !O3-H1
    rij(2)=xr(2,3)  !O3-H2
    rij(3)=(xr(1,3)**2+xr(2,3)**2-xr(1,2)**2)*0.5/xr(1,3)/xr(2,3) !cos
    rij(3)=dacos(rij(3))  !angle H1-O3-H2

    call vibpot(rij,e1,1)
    pot=e1+0.000001910936

    return
  end subroutine pot1b

  subroutine pot_gd_2b(x2,pg2)
  real,dimension(:,:),intent(in) :: x2
  real,dimension(:,:),intent(inout) :: pg2
  real::m2(16665),q2(210)
  real:: chg2b(2,3,19)

  real::p(size(coef2)), dp(size(coef2))
  real::roo,e2,s,y(15),ypes(15),pot_chg,r(6,6)
  real::xt(3,6),x2n(3,6),chg(6)
  integer::i,j
  real::p2,g2(3,6),gd(18),dsdx(3,6),eps,dist_a,dist_b
  real::sa,sb,gd_chg(3,6)

  p2=0.d0
  g2=0.d0
  eps=0.001d0
  dsdx=0.d0

  x2n=x2(1:3,1:6)
  chg2b(1,1:3,1:19)=chg_gd(x2(2,7),1:3,1:19)
  chg2b(2,1:3,1:19)=chg_gd(x2(3,7),1:3,1:19)
  roo=x2(1,7)

  call get_r(x2n, r, ypes,3)
  call get_r(x2n, r, y, 0)

  if (roo < r2f) then
      e2 = emsav_2b_v8(ypes, coef2)
      call evmono_2b_v8(ypes, m2)
      call evpoly_2b_v8(m2, p)
      call deriv_2b_v8(coef2, m2, p, x2n, r, gd)

      if (roo < r2i) then
         p2 = e2
         g2 = reshape(gd,(/3,6/))
      else
         call f_switch(s,roo,r2i,r2f)
         chg(1:3) = chg2b(1,:,1)
         chg(4:6) = chg2b(2,:,1)
         call pot2_dip(chg,y,pot_chg)
         p2 = pot_chg*s + (1.d0-s)*e2

         do i=1,3
         do j=5,6
         xt=x2n;xt(i,j)=xt(i,j)-eps;
         dist_a=norm2(xt(:,5)-xt(:,6))
         call f_switch(sa,dist_a,r2i,r2f)
         xt=x2n;xt(i,j)=xt(i,j)+eps;
         dist_b=norm2(xt(:,5)-xt(:,6))
         call f_switch(sb,dist_b,r2i,r2f)
         dsdx(i,j)=0.5d0*(sb-sa)/eps
         end do
         end do
         call grad2b_dip(chg2b,y,x2n,gd_chg)
         g2 = reshape(gd,(/3,6/))
         g2 = (1.d0-s)*g2+(pot_chg-e2)*dsdx+s*gd_chg

      end if
  else
      chg(1:3) = chg2b(1,:,1)
      chg(4:6) = chg2b(2,:,1)
      call pot2_dip(chg,y,pot_chg)
      p2 = pot_chg
      call grad2b_dip(chg2b,y,x2n,gd_chg)
      g2 = gd_chg
  end if

  pg2(:,1:6)=g2
  pg2(:,7)=p2

  return
  end subroutine pot_gd_2b

  subroutine pot_gd_3b(x3,pg3)
    real,dimension(:,:),intent(in) :: x3
    real,dimension(:,:),intent(inout) :: pg3
    real::s,e3,e31,e32,roo(3),rmax,y(36),dsdx(3,9),r(9,9)
    integer::i,j,k
    real::eps,sa,sb
    real::xt(3,9)
    real::p1(size(coef3_1)), dp1(size(coef3_1))
    real::p2(size(coef3_2)), dp2(size(coef3_2))
    real,dimension(27)::gd,gd1,gd2
    real::p3,g3(3,9),x3n(3,9)
    real::m2(2574),q2(983),m1(573),q1(143)
    real::droo(3,3,2)

    x3n=x3(:,1:9)

    p3=0.d0
    g3=0.d0
    rmax = x3(1,10)
    eps=0.001d0
    dsdx=0.d0

    if(rmax.ge.r3bi) then
      do i=1,3
         do j=7,9
         xt=x3n;xt(i,j)=xt(i,j)-eps;
         roo(1)=norm2(xt(:,7)-xt(:,8))
         roo(2)=norm2(xt(:,7)-xt(:,9))
         roo(3)=norm2(xt(:,8)-xt(:,9))
         droo(i,j-6,1)=maxval(roo)
         xt=x3n;xt(i,j)=xt(i,j)+eps;
         roo(1)=norm2(xt(:,7)-xt(:,8))
         roo(2)=norm2(xt(:,7)-xt(:,9))
         roo(3)=norm2(xt(:,8)-xt(:,9))
         droo(i,j-6,2)=maxval(roo)
         end do
      end do
    end if

    if(rmax.le.r3bi) then
      call get_r(x3n,r,y,3)
      e3 = emsav_3b_v9(y,coef3_2)
      call evmono_3b_v9(y, m2)
      call evpoly_3b_v9(m2, p2, q2)
      call deriv_3b_v9(coef3_2, m2, p2, q2, x3n, r, gd)
      g3=reshape(gd,(/3,9/))
    elseif(rmax.ge.r3bf) then
      call get_r(x3n,r,y,1)
      e3 = emsav_3b_v1(y,coef3_1)
      call evmono_3b_v1(y, m1)
      call evpoly_3b_v1(m1, p1, q1)
      call deriv_3b_v1(coef3_1, m1, p1, q1, x3n, r, gd)
      g3=reshape(gd,(/3,9/))
    else
      call get_r(x3n,r,y,1)
      e31 = emsav_3b_v1(y,coef3_1)
      call evmono_3b_v1(y, m1)
      call evpoly_3b_v1(m1, p1, q1)
      call deriv_3b_v1(coef3_1, m1, p1, q1, x3n, r, gd1)
      call get_r(x3n,r,y,3)
      e32 = emsav_3b_v9(y,coef3_2)
      call evmono_3b_v9(y, m2)
      call evpoly_3b_v9(m2, p2, q2)
      call deriv_3b_v9(coef3_2, m2, p2, q2, x3n, r, gd2)
      call f_switch(s,rmax,r3bi,r3bf)
      do i=1,3
         do j=7,9
         call f_switch(sa,droo(i,j-6,1),r3bi,r3bf)
         call f_switch(sb,droo(i,j-6,2),r3bi,r3bf)
         dsdx(i,j)=0.5d0*(sb-sa)/eps
         end do
      end do
      e3=(1-s)*e32+s*e31
      g3=(1-s)*reshape(gd2,(/3,9/))+s*reshape(gd1,(/3,9/))-dsdx*e32+dsdx*e31
   end if

   if (rmax < r3i) then
      p3=e3
      g3=g3
   else
      call f_switch(s,rmax,r3i,r3f)
      do i=1,3
         do j=7,9
         call f_switch(sa,droo(i,j-6,1),r3i,r3f)
         call f_switch(sb,droo(i,j-6,2),r3i,r3f)
         dsdx(i,j)=0.5d0*(sb-sa)/eps
         end do
      end do
      p3=(1-s)*e3
      g3=(1-s)*g3-dsdx*e3
   end if
   pg3(:,1:9)=g3
   pg3(:,10)=p3

  return
  end subroutine pot_gd_3b

  subroutine pot_gd_4b(x4,pg4)
    real,dimension(:,:),intent(in) :: x4
    real,dimension(:,:),intent(inout) :: pg4
    real,dimension(66)::var
    real::s,e4,roo(6),rmax,dsdx(3,12),r(12,12)
    integer::i,j,k
    real::eps,sa,sb
    real::xt(3,12),x4n(3,12)
    real::p(size(coef4)),dp(size(coef4))
    real,dimension(36)::gd
    real::p4,g4(3,12)
    real::m4(1438),q4(5490)

    x4n=x4(:,1:12)

    p4=0.d0
    g4=0.d0
    rmax = x4(1,13)
    eps=0.001d0
    dsdx=0.d0

    if (rmax < r4f) then
        call get_r(x4n, r, var, 2)
        call evmono_4b_v3(var, m4)
        call evpoly_4b_v3(m4, p, q4)
        e4 = dot_product(p, coef4)
        call deriv_4b_v3(coef4, m4, p, q4, x4n, r, gd)

      if (rmax < r4i) then
         p4=e4
         g4=reshape(gd,(/3,12/))
      else
         call f_switch(s,rmax,r4i,r4f)
         do i=1,3
         do j=9,12
         xt=x4n;xt(i,j)=xt(i,j)-eps;
         roo(1)=norm2(xt(:,9)-xt(:,10))
         roo(2)=norm2(xt(:,9)-xt(:,11))
         roo(3)=norm2(xt(:,9)-xt(:,12))
         roo(4)=norm2(xt(:,10)-xt(:,11))
         roo(5)=norm2(xt(:,10)-xt(:,12))
         roo(6)=norm2(xt(:,11)-xt(:,12))
         call f_switch(sa,maxval(roo),r4i,r4f)
         xt=x4n;xt(i,j)=xt(i,j)+eps;
         roo(1)=norm2(xt(:,9)-xt(:,10))
         roo(2)=norm2(xt(:,9)-xt(:,11))
         roo(3)=norm2(xt(:,9)-xt(:,12))
         roo(4)=norm2(xt(:,10)-xt(:,11))
         roo(5)=norm2(xt(:,10)-xt(:,12))
         roo(6)=norm2(xt(:,11)-xt(:,12))
         call f_switch(sb,maxval(roo),r4i,r4f)
         dsdx(i,j)=0.5d0*(sb-sa)/eps
         end do
         end do
         p4=(1-s)*e4
         g4=reshape(gd,(/3,12/))
         g4=(1-s)*g4-dsdx*e4
      end if
   end if
   pg4(:,1:12)=g4
   pg4(:,13)=p4

  return
  end subroutine pot_gd_4b

  !=======================================!
  ! Calculate the internuclear distances  !
  !=======================================!
  subroutine bond(natm,xx,rr)
    integer,intent(in)::natm
    real,dimension(1:natm*3),intent(in)::xx
    real,dimension(1:natm,1:natm),intent(inout)::rr
    !::::::::::::::::::::
    real,dimension(1:3)::vect
    integer::i,j
    
    do i=1,natm-1
       do j=i+1,natm
          vect(:)=xx(i*3-2:i*3)-xx(j*3-2:j*3)
          rr(i,j)=sqrt(sum(vect*vect))
          rr(j,i)=rr(i,j)
       end do
    end do
  
    return
  end subroutine bond
  
  !==================================================!
  ! switching functions for 2b,3b,4b
  !==================================================!
  subroutine f_switch(s,r,ri,rf)
    real,intent(out)::s
    real,intent(in)::r,ri,rf
    !::::::::::::::::::::
    real::ra,ra2,ra3
  
    ra=(r-ri)/(rf-ri)
    ra2=ra*ra
    ra3=ra2*ra
  
    s=10.0*ra3-15.0*ra*ra3+6.0*ra3*ra2
  
  end subroutine f_switch

  !===================================================!
  ! compute the point charge on two separate monomers !
  !===================================================!
  subroutine chg_2b(xx,chg)
    real,dimension(:,:),intent(in)::xx
    real,dimension(size(xx,2)),intent(out)::chg
    !::::::::::::::::::::
    real,dimension(3,3)::xmono
    real,dimension(3)::chgm,dpm
    integer::i,j

    !get monomer charges
    chg=0.d0
    do i=1,2
       xmono(:,3)=xx(:,2*2+i)
       xmono(:,1:2)=xx(:,2*i-1:2*i)
       call dip_ltp2011(xmono,dpm,chgm)
       chg(2*2+i)=chgm(3)
       chg(2*i-1:2*i)=chgm(1:2)
    end do

    return
  end subroutine chg_2b

  !==================================================!
  ! Get the variable used in the fit (1/R or Morse), !
  ! based on the Cartesian coordinates xyz           !
  !==================================================!
  subroutine get_r(xyz,r,y,a)
    real,dimension(:,:),intent(in)::xyz
    real,dimension(:,:),intent(out)::r
    real,dimension(:),intent(out)::y
    integer,intent(in)::a
    !:::::::::::::::::
    integer::i,j,natm,k

    natm = size(xyz,2)
    k = 1
    r = 0.0
    do i=1,natm-1
       do j=i+1,natm
          y(k) = norm2(xyz(:,i)-xyz(:,j))
          r(i,j) = y(k)
          r(j,i) = y(k)
          k = k + 1
       end do
    end do

    if (a==0) then
       y= y
    elseif (a==1) then
       y = 1.0 / y
    else
       y = exp(-y / real(a))
    end if

    return
  end subroutine


  !=====================================!
  ! Dipole-dipole for 2b in long range  !
  !=====================================! 
  subroutine pot2_dip(chg,r,pot)
    real,dimension(:),intent(in)::r
    real,intent(out)::pot
    !::::::::::::::::::::
    real,dimension(3,3)::dist
    real::chg(6)
    integer::i,j

    dist(1,1)=r(2)
    dist(1,2)=r(3)
    dist(1,3)=r(5)
    dist(2,1)=r(6)
    dist(2,2)=r(7)
    dist(2,3)=r(9)
    dist(3,1)=r(11)
    dist(3,2)=r(13)
    dist(3,3)=r(15)

    pot=0.d0
    do i=1,3
       do j=1,3
          pot = pot + chg(i)*chg(3+j)/dist(i,j)
       end do
    end do

    return
  end subroutine

  !==============================================!
  ! Gradient for dipole-dipole 2b in long range  !
  !==============================================! 
  subroutine grad2b_dip(chg2b,r,x2n,gd)
    real,dimension(:,:,:),intent(in)::chg2b
    real,dimension(:),intent(in)::r
    real,dimension(:,:),intent(in)::x2n
    real,dimension(:,:),intent(out)::gd
    !::::::::::::::::::::
    real,dimension(3,3)::dist
    real::chg1(3),chg2(3),gdchg1(3,3,3),gdchg2(3,3,3)
    real::eps,tmpgd1(3,3),tmpgd2(3,3)
    integer::i,j,k,l
    real::x1(3,3),x2(3,3),tmpx(3,3)
    real::vect(3,3,3)

    eps=0.001d0

    dist(1,1)=r(2)
    dist(1,2)=r(3)
    dist(1,3)=r(5)
    dist(2,1)=r(6)
    dist(2,2)=r(7)
    dist(2,3)=r(9)
    dist(3,1)=r(11)
    dist(3,2)=r(13)
    dist(3,3)=r(15)

    chg1(:)=chg2b(1,:,1)
    chg2(:)=chg2b(2,:,1)

    gd=0.d0
    gdchg1=0.d0
    gdchg2=0.d0

    do i=1,3
      do j=1,3
         do k=1,3
         gdchg1(i,j,k)=0.5d0*(chg2b(1,i,6*j+2*k-5)-chg2b(1,i,6*j+2*k-6))/eps
         gdchg2(i,j,k)=0.5d0*(chg2b(2,i,6*j+2*k-5)-chg2b(2,i,6*j+2*k-6))/eps
         end do
      end do
    end do


    x1(:,1:2)=x2n(:,1:2);x1(:,3)=x2n(:,5)
    x2(:,1:2)=x2n(:,3:4);x2(:,3)=x2n(:,6)

    do i=1,3
     do j=1,3
       vect(:,i,j)=x1(:,i)-x2(:,j)
     end do
    end do

    tmpgd1=0.d0
    tmpgd2=0.d0
    do i=1,3
      do j=1,3
         do k=1,3
           do l=1,3
             tmpgd1(i,j)=tmpgd1(i,j)+gdchg1(k,i,j)*chg2(l)/dist(k,l)
             tmpgd2(i,j)=tmpgd2(i,j)+chg1(k)*gdchg2(l,i,j)/dist(k,l)
           end do
         end do
      end do
    end do

    do i=1,3
      do j=1,3
         do k=1,3
             tmpgd1(i,j)=tmpgd1(i,j)-chg1(j)*chg2(k)*vect(i,j,k)/(dist(j,k)**3)
             tmpgd2(i,j)=tmpgd2(i,j)+chg1(k)*chg2(j)*vect(i,k,j)/(dist(k,j)**3)
         end do
      end do
    end do

    gd(:,1:2)=tmpgd1(:,1:2)
    gd(:,3:4)=tmpgd2(:,1:2)
    gd(:,5)=tmpgd1(:,3)
    gd(:,6)=tmpgd2(:,3)

    return
  end subroutine



  subroutine hessian(x,H)
    real,dimension(:),intent(in)::x
    real,dimension(:,:),intent(inout)::H

    real::eps,pot,virial(3,3)
    real,dimension(1:size(x))::tx,gd
    real,dimension(1:size(x),1:size(x))::gd1,gd2
    real::box(3)
    integer::i,j

    eps=0.005d0
    H=0.d0

    do i=1,size(x)
      tx=x;tx(i)=tx(i)+eps;call pot_gd(tx,pot,gd1(:,i),virial,box)
      tx=x;tx(i)=tx(i)-eps;call pot_gd(tx,pot,gd2(:,i),virial,box)
    end do

    do i=1,size(x)
      do j=i+1,size(x)
        H(i,j)=(gd1(i,j)-gd2(i,j))/(4.0*eps)+(gd1(j,i)-gd2(j,i))/(4.0*eps)
        H(j,i)=H(i,j)
      end do
    end do
          
    do i=1,size(x)
       H(i,i)=(gd1(i,i)-gd2(i,i))/(2.0*eps)
    end do
    return
  end subroutine hessian

  !==================================!!
  !Set PBC for water system
  !==================================!
  subroutine setPBC(x,box0)
    real,dimension(:,:),intent(inout) :: x !x(3,natm)
    real :: box(9),box0(3)
    real :: tempx(3,size(x)/3) !store new coordinates    
    real :: box2(size(box)) !half the box size
    real :: cen_z(3),cen_w(3),di
    integer :: i,j,k

    if (size(box).ne.9) then
      write(*,*) "Current Box size is not acceptable"
      return
    end if

    box=0.d0
    box(1)=box0(1)
    box(5)=box0(2)
    box(9)=box0(3)
    !initialize the half box
    box2 = 0.5*box

    !1. move oxygen atoms to main box
    do i = 1,nw
       !1. move oxygen atoms to main box
       cen_w = x(:,2*nw+i)
       do j = 1,3
         if(cen_w(j).lt.-box2(4*j-3)) then
           !do k = 1,2
           !x(j,2*i-2+k) = x(j,2*i-2+k) + box(4*j-3)
           !end do
           x(j,2*nw+i) = x(j,2*nw+i) + box(4*j-3)
         else if(cen_w(j).gt.box2(4*j-3)) then
           !do k = 1,2
           !x(j,2*i-2+k) = x(j,2*i-2+k) - box(4*j-3)
           !end do
           x(j,2*nw+i) = x(j,2*nw+i) - box(4*j-3)
         end if
       end do
       !2. move hydrogen atoms to each water
       do j = 1,3
         di = x(j,2*i-1) - x(j,2*nw+i)
         if(di.lt.-box2(4*j-3)) then
           x(j,2*i-1) = x(j,2*i-1) + box(4*j-3)
         else if(di.gt.box2(4*j-3)) then
           x(j,2*i-1) = x(j,2*i-1) - box(4*j-3)
         end if
       end do

       do j = 1,3
         di = x(j,2*i) - x(j,2*nw+i)
         if(di.lt.-box2(4*j-3)) then
           x(j,2*i) = x(j,2*i) + box(4*j-3)
         else if(di.gt.box2(4*j-3)) then
           x(j,2*i) = x(j,2*i) - box(4*j-3)
         end if
       end do
    end do

    return
  end subroutine setPBC

  subroutine setPBC_image(x1,x2,flag,box0)
    real,dimension(:,:),intent(in) :: x1 !x(3,3)
    real,dimension(:,:),intent(inout) :: x2 !x(3,3)
    real::box0(3),box(9)
    real :: box2(size(box)) !half the box size
    real :: di
    integer :: i,j,k,flag

    if (size(box).ne.9) then
      write(*,*) "Current Box size is not acceptable"
      return
    end if

    box=0.d0
    box(1)=box0(1)
    box(5)=box0(2)
    box(9)=box0(3)

    !initialize the half box
    box2 = 0.5*box
    flag = 0

    do i = 1,3
      di = x1(i,3)-x2(i,3)
      if(di.lt.-box2(4*i-3)) then
         x2(i,1) = x2(i,1) - box(4*i-3)
         x2(i,2) = x2(i,2) - box(4*i-3)
         x2(i,3) = x2(i,3) - box(4*i-3)
         flag = 1
      else if(di.gt.box2(4*i-3)) then
         x2(i,1) = x2(i,1) + box(4*i-3)
         x2(i,2) = x2(i,2) + box(4*i-3)
         x2(i,3) = x2(i,3) + box(4*i-3)
         flag = 1
      end if
    end do

    return
  end subroutine setPBC_image
end module pes_shell
