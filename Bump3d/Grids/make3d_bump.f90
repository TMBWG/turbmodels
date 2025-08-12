  program make3d_bump
!
! Reads in 2-D version of 2-D formatted PLOT3D bump grid, and creates
! the 3-D version
! This is hardwired very specifically for use with the bump case,
! whose 2-D grid sizes are:    and 3-D grid sizes are:
!   705x321                         65x705x321
!   353x161                         33x353x161
!   177x81                          17x177x81
!   89x41                           9x89x41
!   45x21                           5x45x21
!
  implicit none
!
  integer :: nbin, idim, jdim, kdim, i, j, k
  real*8  :: pi, delta
  real*8, dimension(:,:,:), allocatable ::  x, y, z
  character(len=80) :: filename
!
  write(6,'('' input 2-D structured (formatted) bump file to make 3-D'')')
  write(6,'(''    (note... you must input the 2-D version):'')')
  read(5,'(a80)') filename
  open(2,file=filename,form='formatted',status='old')
!
  read(2,*) nbin
  if (nbin /= 1) then
    write(6,'('' Error!  nbin should be 1'')')
    write(6,'(''  ... it is '',i6)') nbin
    stop
   end if
  read(2,*) jdim,kdim
  write(6,'('' jdim,kdim='',2i6)') jdim,kdim
  if (jdim == 2) then
    write(6,'('' Error!  It appears you might be reading in the 3-D version'')')
    write(6,'('' of the 2-D grid (i.e., idim=2).  Stopping.'')')
    stop
  end if
!
  if (jdim == 45) then
    idim=5
  else if (jdim == 89) then
    idim=9
  else if (jdim == 177) then
    idim=17
  else if (jdim == 353) then
    idim=33
  else if (jdim == 705) then
    idim=65
  else
    write(6,'('' Error!  Jdim not an accepted value!  Stopping'')')
    stop
  end if
  delta = 1.0/float(idim-1)
!
  allocate (x(idim,jdim,kdim))
  allocate (y(idim,jdim,kdim))
  allocate (z(idim,jdim,kdim))
!
  read(2,*) ((x(1,j,k),j=1,jdim),k=1,kdim), &
            ((z(1,j,k),j=1,jdim),k=1,kdim)
!
  do j=1,jdim
    do k=1,kdim
      y(1,j,k)=0.
    enddo
  enddo
!
  pi=acos(-1.)
  do i=2,idim
    do j=1,jdim
      do k=1,kdim
        y(i,j,k)=y(i-1,j,k)-delta
        if (j==1 .or. j==jdim) then
          x(i,j,k)=x(1,j,k)
        else
          x(i,j,k)=x(1,j,k)+0.30*(sin(pi*y(i,j,k)))**4
        end if
        z(i,j,k)=z(i-1,j,k)
      enddo
    enddo
  enddo
!
  open(3,file='bump3dgrid.p3dfmt',form='formatted',status='unknown')
  write(3,*) nbin
  write(3,*) idim,jdim,kdim
  write(3,*) (((x(i,j,k),i=1,idim),j=1,jdim),k=1,kdim), &
             (((y(i,j,k),i=1,idim),j=1,jdim),k=1,kdim), &
             (((z(i,j,k),i=1,idim),j=1,jdim),k=1,kdim)
!
  write(6,'('' output to bump3dgrid.p3dfmt'')')
  deallocate (x,y,z)
!
  stop
  end
