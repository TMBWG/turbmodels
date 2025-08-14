program makegrid_pipe_str

  implicit none
! Make structured rotating pipe grid.
! This program is hardwired to output specific grid(s):
! finest=769x17x513, with option for coarser variants.
! Grid is in meters, approximately corresponding to 
! Zaets et al experiment.

  integer, parameter :: imax = 769
  integer, parameter :: jmax = 17
  integer, parameter :: kmax = 513
  real(kind=8), dimension(imax, jmax, kmax) :: x,y,z
  real(kind=8), dimension(imax) :: xx
  real(kind=8), dimension(kmax) :: zz
  real(kind=8) delta, angle, d_angle, pi, piby2
  integer :: n,i, j, k, iuse, nbin, ifactor
  integer :: idim, jdim, kdim
  pi = dacos(-1.0d+0)

! The following default creates singular (pole) axis at y=z=0:
  iuse=2

! Create radial distribution (in z)
  zz(513)=0.5d+0
  delta=8.d-5
  do n=512,1,-1
     zz(n)=zz(n+1)-delta
     if (n .eq. 1) then
        if (iuse .eq. 0) then
            zz(n)=1.d-10
        else if (iuse .eq. 1) then
            zz(n)=0.0005d+0
        else
            zz(n)=0.0d+0
        end if
     end if
     delta=delta*1.007618d+0
  enddo

! Create streamwise distribution (in x)
  xx(257)=150.d+0
  delta=.075d+0
  do n=256,1,-1
     xx(n)=xx(n+1)-delta
     if (xx(n) .lt. 0.) xx(n)=0.
     delta=delta*1.01294d+0
  enddo
  delta=.075d+0
  do n=258,659
     xx(n)=xx(n-1)+delta
  enddo
  do n=660,769
     xx(n)=xx(n-1)+delta
     delta=delta*1.02d+0
  enddo

! Fill the arrays
! default angle between the two periodic sides is 5 deg.
  d_angle = 5.0d+0/16.d+0
  piby2   = 180.0d+0/pi
  d_angle=d_angle/piby2
  angle=2.5d+0/piby2
  do j=1,17
     do i=1,769
        do k=1,513
           x(i,j,k)=xx(i)*0.06d+0
           y(i,j,k)=-zz(k)*sin(angle)*0.06d+0
           z(i,j,k)=zz(k)*cos(angle)*0.06d+0
        enddo
     enddo
     angle=angle-d_angle
  enddo

! Write PLOT3D grid
  open(2,file='grid.ufmt',form='unformatted',status='unknown')
  nbin=1
  idim=769
  jdim=17
  kdim=513
  write(6,'('' input ifactor (1,2,4,8,16):'')')
  write(6,'(''   (1=finest, 2=half size, 4=quarter size, etc...)'')')
  read(5,*) ifactor
! write(2,'(i8)') nbin
! write(2,'(3i8)') ((idim-1)/ifactor)+1,((jdim-1)/ifactor)+1,((kdim-1)/ifactor)+1
! write(2,'(10(1x,es20.10))') (((x(i,j,k),i=1,idim,ifactor),j=1,jdim,ifactor),k=1,kdim,ifactor), &
!          (((y(i,j,k),i=1,idim,ifactor),j=1,jdim,ifactor),k=1,kdim,ifactor), &
!          (((z(i,j,k),i=1,idim,ifactor),j=1,jdim,ifactor),k=1,kdim,ifactor)
  write(2) nbin
  write(2) ((idim-1)/ifactor)+1,((jdim-1)/ifactor)+1,((kdim-1)/ifactor)+1
  write(2) (((x(i,j,k),i=1,idim,ifactor),j=1,jdim,ifactor),k=1,kdim,ifactor), &
           (((y(i,j,k),i=1,idim,ifactor),j=1,jdim,ifactor),k=1,kdim,ifactor), &
           (((z(i,j,k),i=1,idim,ifactor),j=1,jdim,ifactor),k=1,kdim,ifactor)

  write(6,'('' NOTE: original (posted) structured grids switched the i and j directions'')')
  write(6,'('' output to grid.ufmt'')')

  end
