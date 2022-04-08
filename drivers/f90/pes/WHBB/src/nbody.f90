!==================================================
! Monomer potentials using Partridge-Schwenke's
!==================================================
subroutine pot1b(natm,xx,pot)
  integer,intent(in)::natm
  real,dimension(3,natm),intent(in)::xx
  real,intent(inout)::pot
  !::::::::::::::::::::
  real,dimension(1:9)::x1
  real,dimension(3,3)::xr
  real,dimension(natm/3,3)::rij
  real,dimension(natm/3)::e1
  integer::i,fo,nw

  nw=natm/3
  fo=nw*2
  pot=0.d0

  do i=1,nw
     x1(1:3)=xx(:,i*2-1)
     x1(4:6)=xx(:,i*2)
     x1(7:9)=xx(:,fo+i)
     call bond(3,x1,xr)
     !h2o pot
     rij(i,1)=xr(1,3)  !O3-H1
     rij(i,2)=xr(2,3)  !O3-H2
     rij(i,3)=(xr(1,3)**2+xr(2,3)**2-xr(1,2)**2)*0.5/xr(1,3)/xr(2,3) !cos
     rij(i,3)=dacos(rij(i,3))  !angle H1-O3-H2
  end do

  call vibpot(rij,e1,nw)
  pot=sum(e1)+0.000001910936*real(nw)

  return
end subroutine pot1b
!==================================================
subroutine pot1b_g(natm,xx,pot,im)
  integer,intent(in)::natm
  real,dimension(3,natm),intent(in)::xx
  real,intent(inout)::pot
  integer,intent(in)::im
  !::::::::::::::::::::
  real,dimension(1:9)::x1
  real,dimension(3,3)::xr
  real,dimension(1,3)::rij
  integer::i,fo
  real,dimension(1)::e1

  fo=natm/3*2
  if (im>fo) then
     i=im-fo
  else
     i=(im+1)/2
  end if

  x1(1:3)=xx(:,i*2-1)
  x1(4:6)=xx(:,i*2)
  x1(7:9)=xx(:,i+fo)

  call bond(3,x1,xr)
  !h2o pot
  rij(1,1)=xr(1,3)  !O3-H1
  rij(1,2)=xr(2,3)  !O3-H2
  rij(1,3)=(xr(1,3)**2+xr(2,3)**2-xr(1,2)**2)*0.5/xr(1,3)/xr(2,3) !cos
  rij(1,3)=dacos(rij(1,3))  !angle H1-O3-H2

  call vibpot(rij,e1,1)
  pot=pot+e1(1)

  return
end subroutine pot1b_g
!==================================================
! Intrinsic two-body interaction energies, using T1 fit,
! switched to TTM3-F potential in the long range
!==================================================
subroutine potc2bt1(natm,xx,pot)
  integer,intent(in)::natm
  real,dimension(3,natm),intent(in)::xx
  real,intent(inout)::pot
  !::::::::::::::::::::
  real,dimension(6,3)::x2
  real,dimension(3,6)::xttm,df
  real::e2,vect(3),ettm(4),rmax,s
  integer::i,k

  e2=0.d0;pot=0.d0

  do i=1,size(idx_2b,2)
     x2((/(k,k=1,6)/),:)=transpose(xx(:,idx_2b(:,i)))
     vect(:)=x2(4,:)-x2(1,:)
     rmax=sqrt(sum(vect*vect))

     if (rmax<r2f) then
        call calcpot(e2,x2,2)

        if (rmax<r2i) then
           pot=pot+e2
        else
           call f_switch(s,rmax,r2i,r2f)
           xttm(:,(/1,3,4,2,5,6/))=transpose(x2*auang)
           call ttm3f_mb(2,xttm,df,ettm)
           pot=pot+(ettm(2)+ettm(3)+ettm(4))*s/aukcal+(1.d0-s)*e2
        end if
     else
        xttm(:,(/1,3,4,2,5,6/))=transpose(x2*auang)
        call ttm3f_mb(2,xttm,df,ettm)
        pot=pot+(ettm(2)+ettm(3)+ettm(4))/aukcal
     end if
  end do

  return
end subroutine potc2bt1
!==================================================
subroutine potc2bt1_g(natm,xx,pot,im)
  integer,intent(in)::natm
  real,dimension(3,natm),intent(in)::xx
  real,intent(inout)::pot
  integer,intent(in)::im
  !::::::::::::::::::::
  real,dimension(6,3)::x2
  real,dimension(3,6)::xttm,df
  real::e2,vect(3),ettm(4),rmax,s
  integer::i,k

  e2=0.d0;pot=0.d0

  do i=1,size(idx_2b,2)
     if (any(idx_2b(:,i)==im)) then
        x2((/(k,k=1,6)/),:)=transpose(xx(:,idx_2b(:,i)))
        vect(:)=x2(4,:)-x2(1,:)
        rmax=sqrt(sum(vect*vect))

        if (rmax<r2f) then
           call calcpot(e2,x2,2)

           if (rmax<r2i) then
              pot=pot+e2
           else
              call f_switch(s,rmax,r2i,r2f)
              xttm(:,(/1,3,4,2,5,6/))=transpose(x2*auang)
              call ttm3f_mb(2,xttm,df,ettm)
              pot=pot+(ettm(2)+ettm(3)+ettm(4))*s/aukcal+(1.d0-s)*e2
           end if
        else
           xttm(:,(/1,3,4,2,5,6/))=transpose(x2*auang)
           call ttm3f_mb(2,xttm,df,ettm)
           pot=pot+(ettm(2)+ettm(3)+ettm(4))/aukcal
        end if
     end if
  end do

  return
end subroutine potc2bt1_g
!==================================================
! Calculate the three body potentials
!==================================================
subroutine potc3b(natm,xx,pot)
  integer,intent(in)::natm
  real,dimension(3,natm),intent(in)::xx
  real,intent(inout)::pot
  !::::::::::::::::::::
  real,dimension(3,9)::x3
  real::e3,rr(3,3),roo(3),rmax,s
  real,external::fpes
  integer::i,k

  pot=0.d0
  do i=1,size(idx_3b,2)
     e3=0.d0
     x3(:,(/(k,k=1,9)/))=xx(:,idx_3b(:,i))
     call bond(3,x3(:,(/7,8,9/)),rr)
     roo=(/rr(1,2),rr(1,3),rr(2,3)/)
     rmax=maxval(roo)

     if (rmax<r3f) then
        e3=fpes(x3)      ! three-body correction
        if (rmax<r3i) then
           pot=pot+e3
        else
           call f_switch(s,rmax,r3i,r3f)
           pot=pot+(1-s)*e3
        end if
     end if

  end do

  return
end subroutine potc3b
!==================================================
subroutine potc3b_g(natm,xx,pot,im)
  integer,intent(in)::natm
  real,dimension(3,natm),intent(in)::xx
  real,intent(inout)::pot
  integer,intent(in)::im
  !::::::::::::::::::::
  real,dimension(3,9)::x3
  real::e3,rr(3,3),roo(3),rmax,s
  real,external::fpes
  integer::i,k

  e3=0.d0;pot=0.d0
  do i=1,size(idx_3b,2)
     if (any(idx_3b(:,i)==im)) then
        x3(:,(/(k,k=1,9)/))=xx(:,idx_3b(:,i))
        call bond(3,x3(:,(/7,8,9/)),rr)
        roo=(/rr(1,2),rr(1,3),rr(2,3)/)
        rmax=maxval(roo)

        if (rmax<r3f) then
           x3(:,(/(k,k=1,9)/))=xx(:,idx_3b(:,i))
           e3=fpes(x3)      ! three-body correction
           if (rmax<r3i) then
              pot=pot+e3
           else
              call f_switch(s,rmax,r3i,r3f)
              pot=pot+(1-s)*e3
           end if
        end if

     end if
  end do

  return
end subroutine potc3b_g

!==================================================
! Calculate the many body potentials from ttm3-f
!==================================================
subroutine potmb(natm,xx,pot)
  integer,intent(in)::natm
  real,dimension(3,natm),intent(in)::xx
  real,intent(inout)::pot
  !::::::::::::::::::::
  real,dimension(3,9)::x3ttm,d3f
  real,dimension(3,6)::x2ttm,d2f
  real,dimension(3,natm)::xttm,df
  real::ettm(4),e2
  integer::nw,i

  nw=natm/3
  pot=0.d0
  ettm=0.d0
  !full ttm3-f energy for the cluster
  do i=1,nw
     xttm(:,i)=xx(:,nw*2+i)
  end do
  do i=1,nw*2
     xttm(:,nw+i)=xx(:,i)
  end do
  xttm=xttm*auang
  call ttm3f_mb(nw,xttm,df,ettm)
  pot=ettm(4)

!2body energies in ttm
  xttm=xx*auang
  e2=0.d0
  do i=1,size(idx_2b,2)
     x2ttm(:,(/1,3,4,2,5,6/))=xttm(:,idx_2b(:,i))
     call ttm3f_mb(2,x2ttm,d2f,ettm)
     e2=e2+ettm(4)
  end do
  pot=pot+e2*(nw-3) !discounted factor: nw-3

  !3body energies in ttm
  do i=1,size(idx_3b,2)
     x3ttm(:,(/4,5,6,7,8,9,1,2,3/))=xttm(:,idx_3b(:,i))
     call ttm3f_mb(3,x3ttm,d3f,ettm)
     pot=pot-ettm(4)
  end do

  pot=pot/aukcal

  return
end subroutine potmb

!==================================================
! Mapping 3body
!==================================================
subroutine map_3b(nw,idx)
  integer,intent(in)::nw
  integer,dimension(:,:),intent(inout)::idx
  !::::::::::::::::::::
  integer::i,j,k,vec(9),fo,l

  fo=nw*2
  l=1
  do i=1,nw-2
     vec((/7,1,2/))=(/fo+i,i*2-1,i*2/)
     do j=i+1,nw-1
        vec((/8,3,4/))=(/fo+j,j*2-1,j*2/)
        do k=j+1,nw
           vec((/9,5,6/))=(/fo+k,k*2-1,k*2/)
           idx(:,l)=vec
           l=l+1
        end do
     end do
  end do

  return
end subroutine map_3b
!==================================================
! Mapping 2body
!==================================================
subroutine map_2b(nw,idx)
  integer,intent(in)::nw
  integer,dimension(:,:),intent(inout)::idx
  !::::::::::::::::::::
  integer::i,j,vec(6),fo,l

  fo=nw*2
  l=1
  do i=1,nw-1
     vec((/1,2,3/))=(/fo+i,i*2-1,i*2/)
     do j=i+1,nw
        vec((/4,5,6/))=(/fo+j,j*2-1,j*2/)
        idx(:,l)=vec
        l=l+1
     end do
  end do

  return
end subroutine map_2b

!==================================================
! Calculate the internuclear distances
!==================================================
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
        !rr(i,j)=vect(1)**2+vect(2)**2+vect(3)**2
        !rr(i,j)=sqrt(rr(i,j))
        rr(i,j)=sqrt(sum(vect*vect))
        rr(j,i)=rr(i,j)
     end do
  end do

  return
end subroutine bond
!==================================================
!switching functions for combining HBB and TTM3-F
!==================================================
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

!======================================================
! 1-, 2- and 3-body subroutines for efficient Hessian
!======================================================
subroutine pot1b_h(natm,xx,pot,im,jm)
  integer,intent(in)::natm
  real,dimension(3,natm),intent(in)::xx
  real,intent(inout)::pot
  integer,intent(in)::im,jm
  !::::::::::::::::::::
  real,dimension(1:9)::x1
  real,dimension(3,3)::xr
  real,dimension(1,3)::rij
  integer::i,j,fo
  real,dimension(1)::e1

  fo=natm/3*2

  if (im>fo) then
     i=im-fo
  else
     i=(im+1)/2
  end if
  if (jm>fo) then
     j=jm-fo
  else
     j=(jm+1)/2
  end if

  x1(1:3)=xx(:,i*2-1)
  x1(4:6)=xx(:,i*2)
  x1(7:9)=xx(:,fo+i)
  call bond(3,x1,xr)
  !h2o pot
  rij(1,1)=xr(1,3)  !O3-H1
  rij(1,2)=xr(2,3)  !O3-H2
  rij(1,3)=(xr(1,3)**2+xr(2,3)**2-xr(1,2)**2)*0.5/xr(1,3)/xr(2,3) !cos
  rij(1,3)=dacos(rij(1,3))  !angle H1-O3-H2

  call vibpot(rij,e1,1)
  pot=pot+e1(1)+0.000001910936

  if (i/=j) then
     i=j
     x1(1:3)=xx(:,i*2-1)
     x1(4:6)=xx(:,i*2)
     x1(7:9)=xx(:,fo+i)
     call bond(3,x1,xr)
     !h2o pot
     rij(1,1)=xr(1,3)  !O3-H1
     rij(1,2)=xr(2,3)  !O3-H2
     rij(1,3)=(xr(1,3)**2+xr(2,3)**2-xr(1,2)**2)*0.5/xr(1,3)/xr(2,3) !cos
     rij(1,3)=dacos(rij(1,3))  !angle H1-O3-H2

     call vibpot(rij,e1,1)
     pot=pot+e1(1)+0.000001910936
  end if

  return
end subroutine pot1b_h

subroutine potc2bt1_h(natm,xx,pot,im,jm)
  integer,intent(in)::natm
  real,dimension(3,natm),intent(in)::xx
  real,intent(inout)::pot
  integer,intent(in)::im,jm
  !::::::::::::::::::::
  real,dimension(6,3)::x2
  real,dimension(3,6)::xttm,df
  real::e2,vect(3),ettm(4),rmax,s
  integer::i,k

  e2=0.d0;pot=0.d0

  do i=1,size(idx_2b,2)
     if (any(idx_2b(:,i)==im).or.any(idx_2b(:,i)==jm)) then
        x2((/(k,k=1,6)/),:)=transpose(xx(:,idx_2b(:,i)))
        vect(:)=x2(4,:)-x2(1,:)
        rmax=sqrt(sum(vect*vect))

        if (rmax<r2f) then
           call calcpot(e2,x2,2)

           if (rmax<r2i) then
              pot=pot+e2
           else
              call f_switch(s,rmax,r2i,r2f)
              xttm(:,(/1,3,4,2,5,6/))=transpose(x2*auang)
              call ttm3f_mb(2,xttm,df,ettm)
              pot=pot+(ettm(2)+ettm(3)+ettm(4))*s/aukcal+(1.d0-s)*e2
           end if
        else
           xttm(:,(/1,3,4,2,5,6/))=transpose(x2*auang)
           call ttm3f_mb(2,xttm,df,ettm)
           pot=pot+(ettm(2)+ettm(3)+ettm(4))/aukcal
        end if
     end if
  end do

  return
end subroutine potc2bt1_h

subroutine potc3b_h(natm,xx,pot,im,jm)
  integer,intent(in)::natm
  real,dimension(3,natm),intent(in)::xx
  real,intent(inout)::pot
  integer,intent(in)::im,jm
  !::::::::::::::::::::
  real,dimension(3,9)::x3
  real::e3,rr(3,3),roo(3),rmax,s
  real,external::fpes
  integer::i,k

  e3=0.d0;pot=0.d0
  do i=1,size(idx_3b,2)
     if (any(idx_3b(:,i)==im).or.any(idx_3b(:,i)==jm)) then
        x3(:,(/(k,k=1,9)/))=xx(:,idx_3b(:,i))
        call bond(3,x3(:,(/7,8,9/)),rr)
        roo=(/rr(1,2),rr(1,3),rr(2,3)/)
        rmax=maxval(roo)

        if (rmax<r3f) then
           e3=fpes(x3)      ! three-body correction
           if (rmax<r3i) then
              pot=pot+e3
           else
              call f_switch(s,rmax,r3i,r3f)
              pot=pot+(1-s)*e3
           end if
        end if
     end if
  end do

  return
end subroutine potc3b_h
