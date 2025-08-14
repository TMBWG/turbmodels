!Compile options:
! ifort -O2                             -o hc_tmr  hc_tmr.f90
! g95 -O0 -g -Werror -Wall -Wextra -Wno=140,141,165 -fmodule-private
!                                       -o hc_tmr  hc_tmr.f90
! ifort -O0 -debug -g -check all -fpe0 -debug full -warn
! -stack_temps -traceback -check bounds -o hc_tmr  hc_tmr.f90

!*******************************************************************************
!
!*******************************************************************************
 program main

  implicit none
  integer, parameter :: dp=selected_real_kind(15, 307)

  !integer , parameter :: dp = selected_real_kind(P=15)
  real(dp), parameter :: zero=0.0_dp, one=1.0_dp, two=2.0_dp, half=0.5_dp
  real(dp) :: x1, z1
  real(dp) :: x2, z2, z_bl, dz_bl_edge
  real(dp) :: outer_boundary_in_radii, pi

  real(dp) :: h_r, radius_curvature, cgamma, cgamma_alt

  integer  :: n_hemisphere, n_cylinder
  integer  :: naxisym, naxisym_nodes, nr, nnodesx

  real(dp), allocatable, dimension(:,:) :: xR, xL, yR, yL, zR, zL
  real(dp), allocatable, dimension(:,:,:) :: x, y, z
  real(dp), allocatable, dimension(:,:,:) :: x2d, y2d, z2d
  real(dp) :: dtheta, phi, dphi, theta_axis
  real(dp) :: dxa, dza, beta, aspect_ratio, normal_spacing

  integer :: i, j, k, n_unity_ar, pni, pnj, pnk, jl, ju
  integer :: n_bl, n_outer_bl, naxial, check_total, nz_in_lines

  real(dp) :: dz1, scale_factor
  integer  :: nz
  real(dp), allocatable, dimension(:) :: vspacing, rspacing, rvalues
  real(dp), allocatable, dimension(:) :: xspacing

  integer :: ksym

  real(dp) :: tmin, tmax
  real(dp), allocatable, dimension(:) :: phis

  real(dp) :: x_normal, z_normal
  real(dp) :: x_cylinder, z_cylinder
  real(dp) :: x_inner, z_inner
  real(dp) :: x_outer, z_outer
  real(dp) :: eta, fraction, phi_outer, dphi_outer
  real(dp) :: distance_radial, factor_blend
  real(dp) :: sp, hemi_radius_outer, a, b, c!, cp, a, b, c  !NEW
  real(dp) :: cylinder_length_in_radii, rtd
  real(dp) :: deta, phi_outer_limit

  real(dp), allocatable, dimension(:) :: theta_outer

  integer :: kk, n_bl_straight, grid_flag, bl_flag, viscous_flag
  integer :: level, ngrid_max, fact, pni_level, pnj_level, pnk_level

  logical :: symmetry_y, twod, reference_fine_grid

  real(dp) :: reL, yplus, bl_delta, bl_dy
  real(dp) :: reL_laminar, reL_turbulent

  real(dp) :: dx_at_x1, dy_at_x1, beta_x

continue

  ngrid_max = 5+1
  write(*,*) 'ngrid_max=',ngrid_max
  write(*,*) "Input grid level (1 is finest, ngrid_max is coarsest)= ?"
  write(*,*) ' =1,         TMR website finest grid'
  write(*,*) ' =ngrid_max, TMR website coarsest grid'
  write(*,*) ' <=0,        specify some grid generation parameters'
  read(*,*) level
  write(*,*) ' level=',level

  reference_fine_grid = .true.
  if ( level > 0 .and. level <= ngrid_max ) then
    fact = 2**(level-1)
  elseif ( level <= 0 ) then
    reference_fine_grid = .false.
    level = 1
    fact  = 1
  else
    write(*,*)
    write(*,*) 'Error:level cannot be greater than ngrid_max=',ngrid_max
    stop
  endif

  pi =  acos(-1._dp)
  rtd = 180._dp/pi

!*******************************************************************************
!* Geometry and dimensions.
!
!
!      Outer boundary
!                               * <-- Hemisphere
!                         *
!                   *
!              *
!          *
!       *
!     *
!   *                           |
!  *      ______________________|
!  *     (______________________| <-- Hemishpere cylinder
!
!
!*******************************************************************************

  !ReL at x=1 (end of hemisphere)
  !Re  at end of hemisphere AEDC-TR-76-112 = 1.75x10^5
  !based on Re/ft = 4.2x10^6 and radius = 0.5 inches

  reL_laminar     =    400._dp*0.5_dp
  reL_turbulent   = 3.5e+05_dp*0.5_dp
  yplus           = 1._dp

  ! AEDC-TR-76-112 Radius = 0.5 Total-Length = 10.  Total-Length/Radius = 20.
  !
  cylinder_length_in_radii = 19.0_dp

  outer_boundary_in_radii  = 40_dp   ! Distance to the outer boundary in radii

  write(*,*)
  write(*,*) 'Grid generated with radius of hemisphere = 1'
  write(*,*) 'Cylinder_length_in_radii=',cylinder_length_in_radii
  write(*,*) 'Outer_boundary_in_radii=',outer_boundary_in_radii

  if ( reference_fine_grid ) then

    n_hemisphere = 32*2
    write(*,*)
    write(*,*) 'Cells (from hemisphere apex to 90 deg)=',n_hemisphere
    write(*,*) '(from x=0 to x=1):'

  else

    write(*,*)
    write(*,*) 'Input # of cells from hemisphere apex to 90 deg point'
    write(*,*) '(from x=0 to x=1):'
    read(*,*) n_hemisphere

  endif
  write(*,*) ' ...n_hemisphere=',n_hemisphere

! Along the hemisphere from the apex to the junction.

  dphi = half*pi/real(n_hemisphere,dp)

  write(*,*) 'Minimum viscous spacing sized from flat plate boundary'
  write(*,*) 'at x = 1'

  write(*,*)
  write(*,*) 'Input viscous_flag:'
  write(*,*) '  =0, Inviscid'
  write(*,*) '  =1, Laminar.....ReL/unit-length=',reL_laminar
  write(*,*) '  =2, Turbulent...ReL/unit-length=',reL_turbulent
  read(*,*) viscous_flag
  write(*,*) ' ...viscous_flag=',viscous_flag

  if ( viscous_flag == 0 ) then
    bl_flag  = 0
    n_bl     = 1
    bl_delta = dphi
    bl_dy    = dphi
  elseif ( viscous_flag == 1 ) then
    reL     = reL_laminar
    bl_flag = 1
    n_bl    = n_hemisphere
    call bl_spacing( yplus, reL, bl_flag, bl_dy, bl_delta )
  else
    reL     = reL_turbulent
    bl_flag = 2
    n_bl    = 2*n_hemisphere
    call bl_spacing( yplus, reL, bl_flag, bl_dy, bl_delta )
  endif

  write(*,*)
  write(*,*) '....cells in boundary layer taken as n_bl=',n_bl
  write(*,*) '..boundary layer extent taken as bl_delta=',bl_delta

  write(*,*)
  write(*,*) 'Input grid_flag:'
  write(*,*) '  = 1: 2D'
  write(*,*) '  = 2: 3D with symmetry in y'
  write(*,*) '  = 3: 3D without symmetry in y'
  read(*,*) grid_flag
  write(*,*) ' ...grid_flag=',grid_flag

  factor_blend = 0.5_dp

  twod = .false.
  symmetry_y = .false.
  if ( grid_flag == 1 ) then
    twod = .true.
  elseif ( grid_flag == 2 ) then
    symmetry_y = .true.
  endif

!  x0 = 0.0_dp ! The apex of the hemisphere (leading edge location).
!  y0 = 0.0_dp
!  z0 = 0.0_dp

   x1 = 1.0_dp ! Hemisphere-cylinder junction (radius of hemisphere)
!  y1 = 0.0_dp
   z1 = 1.0_dp

   x2 = x1 + cylinder_length_in_radii !End of cyllinder (trailing edge location)
!  y2 = 0.0_dp
   z2 = outer_boundary_in_radii

! Determine the # of cells along the cylinder part by requiring
! the same cell spacing as those along the hemisphere.

  n_cylinder = int( (x2-x1)/ (half*pi/real(n_hemisphere,dp)) )

  if ( reference_fine_grid ) then

    n_cylinder = 48*2
    write(*,*)
    write(*,*) 'Cells along the cylinder=',n_cylinder
    write(*,*) '(from x1=',x1,' to x2=',x2,'):'

  else

    write(*,*)
    write(*,*) 'Input # of cells along the cylinder'
    write(*,*) '(from x1=',x1,' to x2=',x2,'):'
    write(*,*) '...for equal spacing n_cylinder=',n_cylinder
    read(*,*) n_cylinder

  endif
  write(*,*) ' ...n_cylinder=',n_cylinder

  ! Preserve AR of underlying cell.

  naxisym = 4*n_hemisphere !to preserve ar=1
  if ( symmetry_y ) naxisym = naxisym/2
  write(*,*) '# of cells around the axisymetric axis =',naxisym

  if ( symmetry_y ) then
    naxisym_nodes = naxisym + 1
  else
    naxisym_nodes = naxisym
  endif

  n_outer_bl = n_hemisphere + n_cylinder !+ n_cylinder
  if ( n_bl == 1 ) then
    n_outer_bl = n_outer_bl - 1
  endif
  nr = n_bl + n_outer_bl

  n_bl_straight = n_bl/2

  naxial  = n_hemisphere + n_cylinder
  nnodesx = naxial + 1

  write(*,*)
  write(*,*) ' Number of cells in structured grid:'
  write(*,*) '      n_hemisphere = ', n_hemisphere
  write(*,*) ' n_cylinder = ', n_cylinder
  write(*,*) '     naxial = ', n_hemisphere+n_cylinder
  write(*,*) '       n_bl = ', n_bl
  write(*,*) ' n_outer_bl = ', n_outer_bl
  write(*,*) '         nr = ', nr
  write(*,*)
  write(*,*) '          naxisym = ', naxisym
  write(*,*) '    naxisym_nodes = ', naxisym_nodes
  write(*,*)

!*******************************************************************************
!* Allocate arrays
!*******************************************************************************

  allocate(xL(nnodesx,nr+1),xR(nnodesx,nr+1))
  allocate(yL(nnodesx,nr+1),yR(nnodesx,nr+1))
  allocate(zL(nnodesx,nr+1),zR(nnodesx,nr+1))

  allocate(vspacing(nr))
  allocate(rspacing(nr+1))
  allocate( rvalues(nr+1))

!*******************************************************************************
!* Boundary points along the center line: theta = 0.
!*******************************************************************************

  yR(:,:) = zero

  allocate(phis(n_hemisphere+1))

     tmin = 0.0_dp
     tmax = half*pi



  do i = 1, n_hemisphere+1

    phi = real(i-1,dp)*dphi

    phi = real(i-1,dp)*dphi
    phis(i) = phi
    xR(i,:) = one - cos(phi)
    zR(i,:) =       sin(phi)

  end do

  if ( 0 == 1 ) then

  do i = 1, n_cylinder
   xR(n_hemisphere+1+i,:) = x1 + real(i,dp) * (x2-x1)/real(n_cylinder,dp)
   zR(n_hemisphere+1+i,:) = z1
  end do

  else

  allocate(xspacing(n_cylinder+1))

  dx_at_x1 = dphi

  dy_at_x1 = 1.0e+40_dp

  call stretch_function( n_cylinder+1, x1, x2, dx_at_x1, &
                         dy_at_x1, .false., xspacing,    &
                         beta_x, n_unity_ar )

  xspacing(n_cylinder+1) = x2

  do i = 1, n_cylinder+1
    xR(n_hemisphere+i,:) = xspacing(i)
    zR(n_hemisphere+i,:) = z1
  end do

  deallocate(xspacing)

  endif

  write(*,*)
  write(*,*) 'Axial distribution of point in x,z'
  write(*,*) 'Along cylinder beta_x=',beta_x
  write(*,'(1x,9x,a,19x,a,19x,a)') 'i','x','z'
  write(*,'(1x,i10,4f20.10)') 1,xR(1,1),zR(1,1)
  do i=2,n_hemisphere+n_cylinder+1
    dxa = xR(i,1)-xR(i-1,1)
    dza = zR(i,1)-zR(i-1,1)
    write(*,'(1x,i10,4f20.10)') i,xR(i,1),zR(i,1),dxa,dza
  enddo
  write(*,*)

!*******************************************************************************
!* Construct a spacing-function for node generation along a vertical line.
!*******************************************************************************

 z_bl = z1 + bl_delta
 dz1  = bl_dy

 write(*,*) '         nr= ', nr
 write(*,*) '         z1= ', z1
 write(*,*) '       z_bl= ', z_bl
 write(*,*) '         z2= ', z2
 write(*,*) '        dz1= ', dz1

  if ( n_bl > 1 ) then

    call stretch_function( n_bl+1, z1, z_bl, dz1, &
                           dphi, .false., rspacing, beta, n_unity_ar )

    write(*,*)
    write(*,*) 'From stretch_function:'
    write(*,*) '   .........beta=',beta
    write(*,*) '   ...n_unity_ar=',n_unity_ar

  else

   n_unity_ar = 1
   rspacing(1) = z1
   rspacing(2) = z_bl
   beta        = 1._dp

   write(*,*)
   write(*,*) 'Skipping stretch_function:'

  endif

  write(*,"(1x,9x,a,12x,a,11x,a,8x,a)") &
  'k','rspacing','drspacing','aspect-ratio'
  write(*,"(1x,i10,4f20.10)") 1,rspacing(1)
  do k = 2, n_bl+1
    normal_spacing = rspacing(k) - rspacing(k-1)
    aspect_ratio   = dphi / normal_spacing
    write(*,"(1x,i10,6f20.10)") k,rspacing(k),&
                               normal_spacing,&
                                 aspect_ratio
    dz_bl_edge = normal_spacing*beta
  enddo
  rvalues(1)      = z1
  do k=2,n_bl
    rvalues(k)    = rspacing(k)
  enddo
  rvalues(n_bl+1) = z_bl

  call stretch_function( n_outer_bl+1, z_bl, z2, dz_bl_edge, &
                         100._dp*dphi, .false., rspacing, beta, n_unity_ar )

  write(*,*)
  write(*,*) 'From stretch_function:'
  write(*,*) '   .........beta=',beta
  write(*,*) '   ...n_unity_ar=',n_unity_ar
  write(*,"(1x,9x,a,12x,a,11x,a,8x,a)") &
  'k','rspacing','drspacing','aspect-ratio'
  write(*,"(1x,i10,4f20.10)") 1,rspacing(1)
  do k = 2, n_outer_bl + 1
    normal_spacing = rspacing(k) - rspacing(k-1)
    aspect_ratio   = dphi / normal_spacing
    write(*,"(1x,i10,6f20.10)") k,rspacing(k),&
                               normal_spacing,&
                                 aspect_ratio
  enddo

  do k=2,n_outer_bl
    rvalues(n_bl+k) = rspacing(k)
  enddo
  rvalues(nr+1)     = z2


  write(*,*)
  write(*,"(1x,9x,a,12x,a,11x,a,18x,a)") 'k','rspacing','drspacing','ar'
  write(*,"(1x,i10,4f20.10)") 1,rvalues(1)
  do k = 2, nr+1
    normal_spacing = rvalues(k) - rvalues(k-1)
    aspect_ratio   = 1._dp/( normal_spacing / dphi )
    write(*,"(1x,i10,6f20.10)") k,rvalues(k), &
                               normal_spacing,&
                                 aspect_ratio
  enddo

  check_total = (nr+1)*(naxial+1)            !principal plane
  write(*,*)   '  Points      (principal plane)=',check_total
  do k=2,naxisym
    check_total = check_total + (nr+1)*(naxial)
  enddo
  if ( .not.symmetry_y ) then
    write(*,*) '  Points    (total-no symmetry)=',check_total
  else
    write(*,*) '  Points       (total-symmetry)=',check_total
  endif
  write(*,*) ' Millions=',real( check_total, dp) / 1.0e+06_dp

  nz = nr + 1

  nz_in_lines = nz


!*******************************************************************************
!* Construct the upper-central plane which will be copied to generate the grid.
!*******************************************************************************
  write(*,*) ' Construct the upper-central plane'
  write(*,*) ' nnodesx, nz, naxisym = ', nnodesx,nz,naxisym

  allocate(x(nnodesx,nz,naxisym_nodes))
  write(*,*) ' allocate y'
  allocate(y(nnodesx,nz,naxisym_nodes))
  write(*,*) ' allocate z'
  allocate(z(nnodesx,nz,naxisym_nodes))

  write(*,*) ' 1 Construct the upper-central plane'
  x(:,:,1) = xR(1:nnodesx,1:nz)
  y(:,:,1) = yR(1:nnodesx,1:nz)
  z(:,:,1) = zR(1:nnodesx,1:nz)

  write(*,*) ' 2 Construct the upper-central plane'

  do k=2,nz
    vspacing(k-1) = rvalues(k) - rvalues(k-1)
  enddo

  dphi_outer = half*pi/real(n_hemisphere+n_cylinder,dp)

  distance_radial = outer_boundary_in_radii + x2

  hemi_radius_outer = outer_boundary_in_radii + z1

  phi_outer_limit = asin( cylinder_length_in_radii / &
    sqrt( cylinder_length_in_radii**2 + hemi_radius_outer**2 ) )

  allocate( theta_outer(naxial+1))

  do i = 1, naxial+1

    phi_outer = 0.5_dp*pi - real(i-1,dp)*dphi_outer

    if ( phi_outer >= phi_outer_limit ) then

    sp = sin(phi_outer)

    a = hemi_radius_outer**2

    b = 2._dp*hemi_radius_outer*cylinder_length_in_radii*( 1._dp - sp**2 )

    c = cylinder_length_in_radii**2 - &
       ( hemi_radius_outer**2 + cylinder_length_in_radii**2 ) * ( sp**2 )

    deta = b**2 - 4._dp*a*c

    if ( deta < 0._dp ) then
      write(*,*) ' stopping....deta=',deta
      stop
    endif

    theta_outer(i) = ( -b + sqrt( deta ) ) / ( 2._dp*a )

    if ( theta_outer(i) < 0._dp ) then

      theta_outer(i) = ( -b - sqrt( deta ) ) / ( 2._dp*a )

    endif

    theta_outer(i) = asin( theta_outer(i) )

else

theta_outer(i) = 0.0_dp

endif

enddo

  do i = 1, naxial+1

    phi_outer = 0.5_dp*pi - real(i-1,dp)*dphi_outer

    write(*,"(1x,a,i5,4f20.10)") &
    ' phi_outer,theta_outer=',i, phi_outer*rtd,theta_outer(i)*rtd

enddo


  do i = 1, n_hemisphere+1
   phi = real(i-1,dp)*dphi
   phi_outer = real(i-1,dp)*dphi_outer

   do k = 2, nz

   !x(i,k,1) = vspacing(k-1)*(-cos(phi)) + x(i,k-1,1)
   !z(i,k,1) = vspacing(k-1)*  sin(phi)  + z(i,k-1,1)

    x_normal = vspacing(k-1)*(-cos(phi)) + x(i,k-1,1)
    z_normal = vspacing(k-1)*  sin(phi)  + z(i,k-1,1)

    x_inner = x(i,1,1)
    z_inner = z(i,1,1)

    if ( 1 == 1 ) then  !NEW

      if ( theta_outer(i) > 0._dp ) then
        x_outer = hemi_radius_outer*(-sin(theta_outer(i))) + x1
        z_outer = hemi_radius_outer*( cos(theta_outer(i)))
      else

        phi_outer = 0.5_dp*pi - real(i-1,dp)*dphi_outer


        x_outer = hemi_radius_outer*(-tan(theta_outer(i))) + x2
        z_outer = hemi_radius_outer
      endif

    else

      x_outer = distance_radial*(-cos(phi_outer)) + x2
      z_outer = distance_radial*( sin(phi_outer)) + 0._dp

    endif

    fraction = ( rvalues( k) - rvalues(1) ) / &
               ( rvalues(nz) - rvalues(1) )

    x_cylinder = x_inner + fraction*( x_outer - x_inner )
    z_cylinder = z_inner + fraction*( z_outer - z_inner )

    if ( 1 == 1 ) then
      if ( k <= n_bl_straight ) then
        eta = 1._dp
      else
        eta = real( nz-k, dp ) / real( nz-1-n_bl_straight, dp )

        eta = eta**factor_blend
      endif
if ( i == 1 ) write(98,*) ' k,eta=',k,eta
    else
      eta = real( nz-k, dp ) / real( nz-1, dp )

      eta = eta**factor_blend
    endif

    x(i,k,1) = eta*x_normal + (1._dp-eta)*x_cylinder
    z(i,k,1) = eta*z_normal + (1._dp-eta)*z_cylinder

   end do
  end do

  write(*,*) ' 3 Construct the upper-central plane'

  do i = n_hemisphere+2, naxial+1
   phi_outer = real(i-1,dp)*dphi_outer
   do k = 2, nz

   !x(n_hemisphere+1+i,k,1) =                 x(n_hemisphere+1+i,k-1,1)
   !z(n_hemisphere+1+i,k,1) = vspacing(k-1) + z(n_hemisphere+1+i,k-1,1)

    x_normal =                 x(i,k-1,1)
    z_normal = vspacing(k-1) + z(i,k-1,1)

    x_inner = x(i,1,1)
    z_inner = z(i,1,1)

    if ( 1 == 1 ) then  !NEW

      if ( theta_outer(i) > 0._dp ) then
        x_outer = hemi_radius_outer*(-sin(theta_outer(i))) + x1
        z_outer = hemi_radius_outer*( cos(theta_outer(i)))
      else

        phi_outer = 0.5_dp*pi - real(i-1,dp)*dphi_outer

        x_outer = hemi_radius_outer*(-tan(phi_outer)) + x2
        z_outer = hemi_radius_outer
      endif

    else

      x_outer = distance_radial*(-cos(phi_outer)) + x2
      z_outer = distance_radial*( sin(phi_outer)) + 0._dp

    endif

    fraction = ( rvalues( k) - rvalues(1) ) / &
               ( rvalues(nz) - rvalues(1) )

    x_cylinder = x_inner + fraction*( x_outer - x_inner )
    z_cylinder = z_inner + fraction*( z_outer - z_inner )

    if ( 1 == 1 ) then
      if ( k <= n_bl_straight ) then
        eta = 1._dp
      else
        eta = real( nz-k, dp ) / real( nz-1-n_bl_straight, dp )

        eta = eta**factor_blend
      endif
    else
      eta = real( nz-k, dp ) / real( nz-1, dp )

      eta = eta**factor_blend
    endif

    x(i,k,1) = eta*x_normal + (1._dp-eta)*x_cylinder
    z(i,k,1) = eta*z_normal + (1._dp-eta)*z_cylinder

   end do
  end do

  !     Write grid in Tecplot format to file
  open(unit=33,file='upper_central_plane.dat',access='sequential')
  write(33,*)'TITLE = "GRID GENERATION"'
  write(33,*)'VARIABLES = X, Y, Z'
  write(33,*)'ZONE T="ZONE 1", I=',naxial+1,' J=',nz,' K=',1
  write(33,*)'F=BLOCK'
  write(33,*) (((x(i,k,1),i=1,naxial+1,1),k=1,nz,1),kk=1,1,1)
  write(33,*) (((y(i,k,1),i=1,naxial+1,1),k=1,nz,1),kk=1,1,1)
  write(33,*) (((z(i,k,1),i=1,naxial+1,1),k=1,nz,1),kk=1,1,1)
  close(33)

!*******************************************************************************
!* Generate other planes by rotating the principal plane.
!*******************************************************************************
  write(*,*) ' Generate other planes'
  dtheta = (two*pi)/real(naxisym,dp)
  if (  symmetry_y ) dtheta = (pi)/real(naxisym,dp)

  write(*,*) ' dtheta(circumferential,degrees)=',dtheta*rtd

  ! Ensure pole is at correct location (y=z=0)
  i = 1
  do k = 1, nz
    write(800,*) ' pole...',x(i,k,1),y(i,k,1),z(i,k,1)
    z(i,k,1) = 0._dp
    y(i,k,1) = 0._dp
  enddo

  axisym : do ksym = 2, naxisym_nodes

   theta_axis = -real(ksym-1,dp)*dtheta

   do i = 1, nnodesx
    do k = 1, nz
     x(i,k,ksym) = x(i,k,1)
     if ( i == 0 ) then
       z(i,k,1) = 0._dp
       y(i,k,1) = 0._dp
       if ( ksym == 2 ) write(800,*) ' pole...',x(i,k,1),y(i,k,1),z(i,k,1)
       z(i,k,ksym) = 0._dp
       y(i,k,ksym) = 0._dp
     else
       if ( ksym == 2 ) write(800,*) ' pole...',x(i,k,1),y(i,k,1),z(i,k,1)
       z(i,k,ksym) = cos(theta_axis)*z(i,k,1) - sin(theta_axis)*y(i,k,1)
       y(i,k,ksym) = sin(theta_axis)*z(i,k,1) + cos(theta_axis)*y(i,k,1)
     endif
    end do
   end do

  end do axisym

  write(*,*)
  write(*,*) ' Variation of cgamma along polar singularity:'
  write(*,"(1x,4x,a,14x,a,9x,a,5x,a,13x,a,12x,a,12x,a)") &
  'k','x','cgamma','cgamma-alt','AR','h_r','R_c'
   do k = 2, nz
     h_r              = abs( x(1,k,1) - x(1,k-1,1) )
     radius_curvature = ( -x(1,k,1)+1._dp )
     cgamma           = radius_curvature*dtheta**2/(2._dp*h_r )
     aspect_ratio     = sqrt( ( y(2,k,1) - y(1,k,1) )**2 + &
                              ( z(2,k,1) - z(1,k,1) )**2 ) / h_r
     cgamma_alt       = aspect_ratio*dtheta/2._dp
     write(*,"(1x,i5,3f15.6,f15.3,7f15.6)") k, x(1,k,1), cgamma, cgamma_alt, &
     aspect_ratio, h_r, radius_curvature
   end do

   write(*,*) ' CCCCCCCCCCCCCCCCCCCCCC '

  ! Scale grid so that diameter of cylinder is unity.

  scale_factor = 0.50_dp
  write(*,*) ' Scaling grid so that D=1...scale_factor',scale_factor

  if ( twod ) then

    pni = 2
    pnj = 2*naxial + 1
    pnk = nz

    pni_level =  pni
    pnj_level = (pnj-1)/fact + 1
    pnk_level = (pnk-1)/fact + 1

    allocate(x2d(2,pnj,nz))
    allocate(y2d(2,pnj,nz))
    allocate(z2d(2,pnj,nz))

    y2d(1,:,:) =  0._dp
    y2d(2,:,:) = -1._dp  !delta_y = 1 scaled to 2 in FUN3D

    jl = 0
    ju = naxial
    write(*,*) ' K=1 surface'
    write(*,"(1x,2(9x,a,19x,a,19x,a))") 'j','x','z','j','x','z'
    do j=1,naxial+1
      jl = jl + 1
      ju = ju + 1
      do k=1,pnk
        x2d(1,jl,k) = x(naxial+2-j,k,1)
        z2d(1,jl,k) =-z(naxial+2-j,k,1)
        x2d(1,ju,k) = x(         j,k,1)
        z2d(1,ju,k) = z(         j,k,1)
        if ( k == 1 ) write(*,"(1x,2(i10,2f20.10))")        &
                                 jl,x2d(1,jl,k),z2d(1,jl,k),&
                                 ju,x2d(1,ju,k),z2d(1,ju,k)
      enddo
    enddo

    write(*,*) ' K=kmax surface'
    write(*,"(1x,2(9x,a,19x,a,19x,a))") 'j','x','z'
    do j=1,pnj
      k=pnk
      write(*,"(1x,2(i10,2f20.10))")        &
                                 j,x2d(1,j,k),z2d(1,j,k)
    enddo

    x2d(2,:,:) = x2d(1,:,:)
    z2d(2,:,:) = z2d(1,:,:)

    x2d(:,:,:) = x2d(:,:,:)*scale_factor
    z2d(:,:,:) = z2d(:,:,:)*scale_factor

    deallocate(x,y,z)

    write(*,*)
    write(*,*) '...Generating Plot3d grid file=','grid.p3d'
    write(*,*) '.....ni,nj,nk=',pni,pnj_level,pnk_level,' unformatted'
    open(80, file='grid.p3d', form='unformatted')

    write(80) 1

    write(80) pni, pnj_level, pnk_level

    write(80) (((x2d(i,j,k),i=1,pni),j=1,pnj,fact),k=1,pnk,fact), &
              (((y2d(i,j,k),i=1,pni),j=1,pnj,fact),k=1,pnk,fact), &
              (((z2d(i,j,k),i=1,pni),j=1,pnj,fact),k=1,pnk,fact)
    close(80)

    deallocate(x2d,y2d,z2d)

    call map2d( pni_level, pnj_level, pnk_level, viscous_flag )

  else

    pni = naxial + 1     !axial
    pnj = nz             !radial
    pnk = naxisym_nodes  !circumferential

    x(:,:,:) = x(:,:,:)*scale_factor
    y(:,:,:) = y(:,:,:)*scale_factor
    z(:,:,:) = z(:,:,:)*scale_factor

    pni_level = (pni-1)/fact + 1
    pnj_level = (pnj-1)/fact + 1
    pnk_level = (pnk-1)/fact + 1

    write(*,*)
    write(*,*) '...Generating Plot3d grid file=','grid.p3d'
    write(*,*) '.....ni,nj,nk=',pni_level,pnj_level,pnk_level,' unformatted'
    open(80, file='grid.p3d', form='unformatted')

    write(80) 1

    write(80) pni_level, pnj_level, pnk_level

    write(80) (((x(i,j,k),i=1,pni,fact),j=1,pnj,fact),k=1,pnk,fact), &
              (((y(i,j,k),i=1,pni,fact),j=1,pnj,fact),k=1,pnk,fact), &
              (((z(i,j,k),i=1,pni,fact),j=1,pnj,fact),k=1,pnk,fact)
    close(80)

    deallocate(x,y,z)

    call map3d( pni_level, pnj_level, pnk_level, viscous_flag, symmetry_y )

  endif


contains


!================================== STRETCH_FUNCTION =========================80
!
! Stretch the grid and freeze stretching factor when ar reaches unity.
!
!=============================================================================80

subroutine stretch_function( n, y_min, y_max, dy_min, dx_at_y_min, radial,     &
                             y, beta, n_unity_ar )

  implicit none

  !integer, parameter :: dp = selected_real_kind(12)

  integer, intent(in)  :: n
  integer, intent(out) :: n_unity_ar

  logical, intent(in) :: radial

  real(kind=dp), intent(in)  :: y_min, y_max, dy_min, dx_at_y_min
  real(kind=dp), intent(out) :: beta

  real (kind=dp), intent(out)  :: y(n)

  integer :: k, outer, tries, nbeta

  real(kind=dp) :: beta_last, y_last, dydb, dc, beta_new, beta_initial, dbeta

  logical :: found

continue

  y(1) = y_min

  dc = dx_at_y_min

  dydb = 0._dp

  dbeta = 0.0001_dp

  beta   = 1.0_dp
  beta_initial = beta
  do tries = 1,20
    !beta = beta_initial
    found = .false.
    dbeta = 0.0001_dp*real(tries,dp)
    nbeta = ( 10._dp - 1._dp ) / dbeta
  do k=1,nbeta

    beta = beta + dbeta

    call stretch_distance( n, y_min, dy_min, dx_at_y_min, radial, &
                           y, beta, n_unity_ar )

    if ( y(n) > y_max ) then
      found = .true.
      exit
    else
      beta_last = beta
      y_last    = y(n)
    endif

  enddo
  if ( found ) then
    exit
  else
    write(*,*) ' tries=',tries,nbeta,dbeta,beta,y(n),y_max
  endif
  enddo

  do outer = 1,10

    write(*,"(1x,a,i5,2f20.10)") ' outer,beta,target=',&
                                   outer,beta,y(n)-y_max

    if ( abs( y(n) - y_max ) < 1.0e-11_dp ) exit

    dydb  = ( y(n) - y_last )/( beta - beta_last )
    beta_new  = beta + ( y_max - y(n) )/dydb

    y_last    = y(n)
    beta_last = beta

    beta = beta_new

    call stretch_distance( n, y_min, dy_min, dx_at_y_min, radial, &
                           y, beta, n_unity_ar )

  enddo

  ! Set final spacing

  call stretch_distance( n, y_min, dy_min, dx_at_y_min, radial, &
                         y, beta, n_unity_ar )

  write(*,"(i10,1x,a,i10)") n_unity_ar,' Value for n_unity_ar...n=',n
  write(171,"(i10,1x,a,i10)") n_unity_ar,' Value for n_unity_ar...n=',n

end subroutine stretch_function

!================================== STRETCH_DISTANCE =========================80
!
! Distance reached by stretching with sretching frozen when ar reaches unity.
!
!=============================================================================80

subroutine stretch_distance( n, y_min, dy_min, dx_at_y_min, radial,            &
                             y, beta, n_unity_ar )

  implicit none

  !integer, parameter :: dp = selected_real_kind(12)

  integer, intent(in)  :: n
  integer, intent(out) :: n_unity_ar

  logical, intent(in) :: radial

  real(kind=dp), intent(in)  :: y_min, dy_min, dx_at_y_min, beta

  real (kind=dp), intent(out)  :: y(n)

  integer :: j

  real(kind=dp) :: dy, dc

continue

  y(1) = y_min

  dc = dx_at_y_min

  n_unity_ar = 0

  dy = dy_min
  do j=2,n
    y(j) = y(j-1) + dy
    if ( radial ) dc = dx_at_y_min*y(j)/y_min
    if ( dy < dc .and. n_unity_ar == 0 ) then
      dy = dy*beta
    elseif( radial ) then
      dy = y(j)*( dx_at_y_min / (1._dp - 0.5_dp*dx_at_y_min ) )
      if ( n_unity_ar == 0 ) n_unity_ar = j
    else
      if ( n_unity_ar == 0 ) n_unity_ar = j
    endif
  end do

end subroutine stretch_distance

subroutine bl_spacing( yplus, reL, bl_flag, bl_dy, bl_delta )

  implicit none

  !integer, parameter :: dp = selected_real_kind(12)

  integer,  intent(in) :: bl_flag
  real(dp), intent(in)  :: yplus, reL

  real(dp), intent(out) :: bl_dy, bl_delta

  real(dp) :: yminl, ymint, cf

continue

  !   determines approximate appropriate min spacing necessary for
  !   laminar or turbulent computations

  if ( bl_flag == 1 ) then

    !laminar assumes bl=1/sqrt(reL), then takes 1/20th for ymin

    yminl=1./(sqrt(reL)*20._dp)  !bl_delta/10

    bl_delta = 4.91_dp/sqrt(reL)

    bl_dy = yminl
    write(6,*) ' laminar  min spacing required=',yminl

  else

    ! turbulent assumes ymin=sqrt(2/cf)*yplus/reL
    ! where cf is for flat plate

    cf=.455_dp/((log(.06_dp*reL))**2)
    ymint=sqrt(2._dp/cf)*yplus/reL

    bl_delta = 0.382_dp/sqrt(reL**0.2_dp)

    bl_dy = ymint
    write(6,*) ' turbulent min spacing required=', ymint

  endif

    write(6,*) ' ...boundary layer height=',bl_delta
    write(*,*) ' ...boundary layer height/min_spacing=',bl_delta/bl_dy

end subroutine bl_spacing

subroutine map2d( ni, nj, nk, viscous_flag )

  implicit none

  integer,  intent(in) :: ni, nj, nk, viscous_flag

  character(80) :: fmt1
  character(1)  :: pr = "'"

continue

  write(*,*)
  write(*,*) "...Generating .nmf file=",'grid.nmf'
  open(80, file='grid.nmf', form='formatted')

  write(80,"(a)") '# =============&
  & NASA Langley Geometry Laboratory TOG Neutral Map File ==============='
  write(80,"(a)") '# =============&
  &======================================================================'
  write(80,"(a)") '# Block#   IDIM   JDIM   KDIM'
  write(80,"(a)") '# =============&
  &======================================================================'

  write(80,"(i6)") 1
  write(80,*)
  write(80,"(i6,3i10)") 1, ni, nj, nk
  write(80,*)

  write(80,"(a)") '# =============&
  &======================================================================'
  write(80,"(a)") '# Type         &
  &B1  F1     S1   E1     S2   E2    B2  F2     S1   E1     S2   E2  Swap'
  write(80,"(a)") '#--------------&
  &----------------------------------------------------------------------'

  fmt1 = '(1x,a,a,a,2(1x,i6,i2,1x,4i6),1x,a10)'

  write(80,fmt1) pr,'symmetry_y_strong',pr,1, 3, 1, nj, 1, nk  !i=constant
  write(80,fmt1) pr,'symmetry_y_strong',pr,1, 4, 1, nj, 1, nk

  if ( viscous_flag == 0 ) then

    write(80,fmt1) pr,'farfield_riem',pr,1, 5, 1, nk, 1, ni !j=constant
    write(80,fmt1) pr,'farfield_riem',pr,1, 6, 1, nk, 1, ni

    write(80,fmt1) pr,     'tangency',pr,1, 1, 1, ni, 1, nj

  else
    write(80,fmt1) pr,'back_pressure',pr,1, 5, 1, nk, 1, ni !j=constant
    write(80,fmt1) pr,'back_pressure',pr,1, 6, 1, nk, 1, ni

    write(80,fmt1) pr,'viscous_solid',pr,1, 1, 1, ni, 1, nj
  endif

  write(80,fmt1) pr,    'farfield_riem',pr,1, 2, 1, ni, 1, nj   !k=constant

end subroutine map2d

subroutine map3d( ni, nj, nk, viscous_flag, symmetry_y )

  implicit none

  integer,  intent(in) :: ni, nj, nk, viscous_flag

  integer :: nprz, ntria, nodes, nquad, nhex

  logical, intent(in) :: symmetry_y

  character(80) :: fmt1
  character(1)  :: pr = "'"

continue

  write(*,*)
  write(*,*) "...Generating .nmf file=",'grid.nmf'
  open(80, file='grid.nmf', form='formatted')

  write(80,"(a)") '# =============&
  & NASA Langley Geometry Laboratory TOG Neutral Map File ==============='
  write(80,"(a)") '# =============&
  &======================================================================'
  write(80,"(a)") '# Block#   IDIM   JDIM   KDIM'
  write(80,"(a)") '# =============&
  &======================================================================'

  write(80,"(i6)") 1
  write(80,*)
  write(80,"(i6,3i10)") 1, ni, nj, nk
  write(80,*)

  write(80,"(a)") '# =============&
  &======================================================================'
  write(80,"(a)") '# Type         &
  &B1  F1     S1   E1     S2   E2    B2  F2     S1   E1     S2   E2  Swap'
  write(80,"(a)") '#--------------&
  &----------------------------------------------------------------------'

  fmt1 = '(1x,a,a,a,2(1x,i6,i2,1x,4i6),1x,a10)'

  write(80,fmt1) pr,          'pole',pr,1, 3, 1, nj, 1, nk, 2  !i=constant

  if ( viscous_flag == 0 ) then

    write(80,fmt1) pr, 'farfield_riem',pr,1, 4, 1, nj, 1, nk

    write(80,fmt1) pr,      'tangency',pr,1, 5, 1, nk, 1, ni !j=constant
    write(80,fmt1) pr, 'farfield_riem',pr,1, 6, 1, nk, 1, ni

  else

    write(80,fmt1) pr, 'back_pressure',pr,1, 4, 1, nj, 1, nk

    write(80,fmt1) pr, 'viscous_solid',pr,1, 5, 1, nk, 1, ni !j=constant
    write(80,fmt1) pr, 'farfield_riem',pr,1, 6, 1, nk, 1, ni

  endif

  if ( symmetry_y ) then

    write(80,fmt1) pr,'symmetry_y',pr,1, 1, 1, ni, 1, nj  !k=constant
    write(80,fmt1) pr,'symmetry_y',pr,1, 2, 1, ni, 1, nj

  else

    !'one-to-one'      2   6      1   49      1  129      &
    !                  3   5      1   49      1  129  FALSE

    write(80,fmt1) pr,  'one-to-one',pr,   &
                        1, 1, 1, ni, 1, nj,&
                        1, 2, 1, ni, 1, nj, .false.   !k=constant

  endif

  nprz  = (nj-1)*(nk-1)
  nhex  = (ni-1)*(nj-1)*(nk-1) - nprz

  ntria =                        2*(nk-1)
  nquad = (nj-1)*(nk-1) + 2*(ni-1)*(nk-1) + 2*(ni-1)*(nj-1) - ntria

  nodes = ni*nj*nk - nj*(nk-1)

  write(*,*) ' From pole singularity at I=1:'
  write(*,*) ' ............expected hexes=',nhex
  write(*,*) ' ...........expected prisms=',nprz
  write(*,*) ' ....expected tria bc faces=',ntria
  write(*,*) ' ....expected quad bc faces=',nquad
  write(*,*) ' ............expected nodes=',nodes

end subroutine map3d


end program main

