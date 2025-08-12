  program make3d_bump
!
! Reads in 2-D version of 2-D formatted PLOT3D bump grid, and creates
! the 3-D version
! IMPORTANT NOTE: this creates a grid that is half-span compared to the
! original 2-D bump grids on:
! http://turbmodels.larc.nasa.gov/bump3d_grids.html
! (due to symmetry,full span is not necessary)
! This is hardwired very specifically for use with the bump case,
! whose 2-D and 3-D (full symmetry: half symmetry) grid sizes are:
!  1409x641                         65x1409x641 (58.7M) : 129x1409x641
!   705x321                         33x705x321  ( 7.5M) :  65x705x321
!   353x161                         17x353x161  ( 1.0M) :  33x353x161
!   177x81                          9x177x81    ( 0.1M) :  17x177x81
!   89x41                           5x89x41             :   9x89x41
!   45x21                           3x45x21             :   5x45x21
!
  implicit none
!
  integer :: nbin, idim, jdim, kdim, i, j, k, ii, jj, kk, jpts1, jpts2
  integer :: j_le, j_te
  real*8  :: pi, delta, zprofile, zerror, diff
  real*8  :: x_le, fact, termj
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
  elseif ( jdim == 1409 ) then
    idim=129
  else
    write(6,'('' Error!  Jdim not an accepted value!  Stopping'')')
    stop
  end if

  delta = 1.0/float(idim-1)
!

  idim = ( idim - 1 )/2 + 1  !rewire output to use symmetry
  allocate (x(idim,jdim,kdim))
  allocate (y(idim,jdim,kdim))
  allocate (z(idim,jdim,kdim))
!
  read(2,*) ((x(1,j,k),j=1,jdim),k=1,kdim), &
            ((z(1,j,k),j=1,jdim),k=1,kdim)

!Correct that:
!In all 2D bump grids, the viscous surface starts at X=2.780639759250000E-016
!instead of X=0.0.

! Ensure LE is at x=0. for all k values

  x_le = -huge(1.)
  do j=1,jdim
    if ( abs( x(1,j,1) ) > 1.0e-15 ) cycle
    do k=1,kdim
     !write(*,*) ' x=',x(1,j,k),z(1,j,k)
      x_le = max( x_le, abs( x(1,j,k) ) )
      x(1,j,k) = 0.
    enddo
  enddo
  write(*,*) ' x_le set to zero....max non-zero was =',x_le

  if ( 0 == 1 ) then
  x_le = -huge(1.)
  do j=1,jdim
    !if ( abs( x(1,j,1) ) > 1.0e-15 ) cycle
    do k=1,1
      write(*,*) ' x=',x(1,j,k),z(1,j,k)
      !x_le = max( x_le, abs( x(1,j,k) ) )
      !x(1,j,k) = 0.
    enddo
  enddo
  stop
  endif

  pi = acos(-1.)

!Correct that:
!In the P3DFMT files generated using the program for 3D grids, there are
!several points on the K=1 symmetry plane downstream of the bump that have
!non-zero Z-coordinate (values of the order of E-81).

! Shear z-profile of bump to analytic definition.

  zerror = -huge(1.)
  jpts1 = 0
  do j=1,jdim
   !write(*,*) ' j,x,z=',j,x(1,j,1),z(1,j,1)
    zprofile = 0.05*(sin(pi*x(1,j,1)/0.9-(pi/3.)))**4
    if ( x(1,j,1) >= 0.3 .and. x(1,j,1) <= 1.2 ) then
      diff = zprofile - z(1,j,1)
      jpts1 = jpts1 + 1
      z(1,j,1) = zprofile
      do k=2,kdim
        z(1,j,k) = z(1,j,k) + diff*real(kdim-k)/real(kdim-1)
        zerror = max( zerror, abs(diff) )
      enddo
    endif
  enddo
  write(*,*) ' Corrected max bump z-profile error |zerror|=',zerror,&
             ' jpts1=',jpts1

! Shear z-profile ahead and behind bump to analytic definition.

  zerror = -huge(1.)
  jpts2 = 0
  do j=1,jdim
    zprofile = 0.
    if ( x(1,j,1) >= 0.3 .and. x(1,j,1) <= 1.2 ) then
    else
      diff = zprofile - z(1,j,1)
      jpts2 = jpts2 + 1
      z(1,j,1) = zprofile
      do k=2,kdim
        z(1,j,k) = z(1,j,k) + diff*real(kdim-k)/real(kdim-1)
        zerror = max( zerror, abs(diff) )
      enddo
    endif
  enddo
  write(*,*) ' Corrected max z-symmetry |zerror|=',zerror, &
             ' jpts2=',jpts2

  if ( jpts1 + jpts2 /= jdim ) then
    write(6,'('' Error!  Inconsistency in ensuring bump and z-symmetry'')')
    write(6,'('' jpts1 + jpts2 /= jdim Stopping.'')')
    stop
  end if

  k = kdim
  zerror = -huge(1.)
  do j=1,jdim
    zerror = max( zerror, abs(z(1,j,k) - 5.0 ) )
  enddo
  write(*,*) ' Max top of domain |zerror|=',zerror

  ! Define y-points.

  do j=1,jdim
    do k=1,kdim
      y(1,j,k)=0.
    enddo
  enddo
!

!Correct that:
!The same spanwise shearing is applied to all points of the original 2D grids
!except the first and last streamwise points. Consequently, the bounding box is
!rectangular. However, there remains a possibility that using this approach one
!may end up with crossed grids (maximum value of sheared X can be larger than
!26.5).

  j_le = 0
  j_te = 0
  do j=1,jdim
    if ( x(1,j,1) <= 0.) then
      j_le = j
    elseif ( x(1,j,1) >= 1.5 ) then
      j_te = j
      exit
    end if
  enddo
  write(*,*) ' j_le,x=',j_le,' at x=',x(1,j_le,1)
  write(*,*) ' j_te,x=',j_te,' at x=',x(1,j_te,1)

  do j=1,jdim
    fact = 0.
    if ( j <= j_le ) then
      fact = real(j_le-j)/real(j_le-1)
    elseif ( j >= j_te ) then
      fact = real(j-j_te)/real(jdim-j_te)
    endif
    termj = 1. - fact**3
    do i=2,idim
      do k=1,kdim
        y(i,j,k)=y(i-1,j,k)-delta
        x(i,j,k)=x(1,j,k)+termj*0.30*(sin(pi*y(i,j,k)))**4
        z(i,j,k)=z(i-1,j,k)
      enddo
    enddo
  enddo

  do i=1,idim
    do j=1,jdim
      k=1
      if ( x(i,j,k) > 1.2 + 0.30*(sin(pi*y(i,j,k)))**4 ) then
        if ( abs( z(i,j,k) ) > 0. ) then
          write(*,"(1x,a,3i5,3e20.12)") 'i,j,k,x,y,z=',&
          i,j,k,x(i,j,k),y(i,j,k),z(i,j,k)
          write(*,*) ' Error...non-zero z at k=1 downstream of bump.'
          stop
        endif
      endif
    enddo
  enddo

  open(3,file='bump3dgrid.p3dfmt',form='formatted',status='unknown')
  write(3,*) nbin
  write(3,*) idim,jdim,kdim
  write(3,*) (((x(i,j,k),i=1,idim),j=1,jdim),k=1,kdim), &
             (((y(i,j,k),i=1,idim),j=1,jdim),k=1,kdim), &
             (((z(i,j,k),i=1,idim),j=1,jdim),k=1,kdim)
!
  write(6,*) ' idim,jdim,kdim=',idim,jdim,kdim
  write(6,'('' output to bump3dgrid.p3dfmt'')')
  deallocate (x,y,z)
!
  stop
  end
