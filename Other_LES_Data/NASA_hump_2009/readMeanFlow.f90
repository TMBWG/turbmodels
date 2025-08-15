! Compiling:
!     gfortran readMeanFlow.f90 
!
program readMeanFlow
!
integer,parameter :: nx=207,ny=23
!
real,dimension(nx,ny) :: x,y,z,U,V,W
real,dimension(nx,ny) :: uu,vv,ww,uv,uw,vw
!
open(20,file='Case3B-mean.dat',form='formatted')
do i=1,28
  read(20,*)
enddo
!
do i = 1,nx
do j = 1,ny
   read(20,*) x(i,j),y(i,j),U(i,j),V(i,j),W(i,j)&
             ,uu(i,j),vv(i,j),ww(i,j)&
             ,uv(i,j),uw(i,j),vw(i,j)
enddo
read(20,*)
enddo
!
open(21,file='Case3B-mean-tecplot.dat',form='formatted')
write(21,'(''variables="x/c","y/c","u/U_in","v/U_in","w/U_in","uu/U_in^2","vv/U_in^2","ww/U_in^2","uv/U_in^2","uw/U_in^2","vw/U_in^2"'')')
write(21,'(''zone, t="LES data"'')')
write(21,'('' I=207, J=23, K=1, ZONETYPE=Ordered'')')
write(21,'('' DATAPACKING=BLOCK'')')
write(21,'('' DT=(SINGLE SINGLE SINGLE SINGLE SINGLE SINGLE SINGLE SINGLE SINGLE SINGLE SINGLE)'')')
write(21,*) ((x(i,j),i=1,nx),j=1,ny)
write(21,*) ((y(i,j),i=1,nx),j=1,ny)
write(21,*) ((U(i,j),i=1,nx),j=1,ny)
write(21,*) ((V(i,j),i=1,nx),j=1,ny)
write(21,*) ((W(i,j),i=1,nx),j=1,ny)
write(21,*) ((uu(i,j),i=1,nx),j=1,ny)
write(21,*) ((vv(i,j),i=1,nx),j=1,ny)
write(21,*) ((ww(i,j),i=1,nx),j=1,ny)
write(21,*) ((uv(i,j),i=1,nx),j=1,ny)
write(21,*) ((uw(i,j),i=1,nx),j=1,ny)
write(21,*) ((vw(i,j),i=1,nx),j=1,ny)
!
write(6,'('' output to Case3B-mean-tecplot.dat'')')
end program readMeanFlow
