!*******************************************************************************
! Grid generation code for a hemisphere cylinder.
!
! This is Version 23hexyp (October 2015).
!
! This code generates a 3D grid over a hemisphere cylinder.
!
! ------------------------------------------------------------------------------
!
!  Input:
!
!  target_reynolds_number = Reynolds number to determine the mesh spacing at wall
!           target_y_plus = y-plus to determine the mesh spacing at wall
!              igrid_type = Element type. 1=Prism, 2=Tet, 3=Mixed(tet/prism), 4=Mixed(prism/hex)
!         b8_ugrid_format = T: unformatted, F: formatted
!                distance = Distance to the outer boundary
!    nodes_cylinder_input = Number of nodes along the cylinder part in x direction
!                      x2 = Length of the hemisphere-cylinder
!                   nr_gs = Number of elements from the apex to the shoulder
!
!   Note: nodes_cylinder_input and nr_gs determine the final grid size (total # of nodes).
!
! Output:
!
!   Two output files by default:
!
!     "hemisphere_'element_type'.mapbc"           !Boundary condition file for FUN3D
!     "hemisphere_'element_type'.ugrid/.b8.ugrid" !Unstructured grid
!
!  --------
!   Below are optional (can be activated inside the program):
!
!   (1)Tecplot files for viewing the grids: boundary grid and volume grid
!
!   (2)Line information
!
!     "hemisphere_'element_type'lines_fmt"   !Lines for implicit solvers or line-agglomeration.
!
!   (3)File containing k1-k2-k3-k4-k5 structured indices.
!
!     "hemisphere_'element_type'k"
!
! [Send comments or bug report to Hiro at hiroaki.nishikawa(at)nasa.gov]
!
!*******************************************************************************
 program hemisphere_cylinder_grid

 implicit none

! Parameters
  integer , parameter ::    dp = selected_real_kind(P=15)
  real(dp), parameter ::  zero = 0.0_dp
  real(dp), parameter ::   one = 1.0_dp
  real(dp), parameter ::   two = 2.0_dp
  real(dp), parameter :: three = 3.0_dp
  real(dp), parameter ::  half = 0.5_dp
  real(dp), parameter ::    pi = 3.14159265358979323846_dp

! Custom data types
  type tria_data
   integer, dimension(3) :: v    !Vertices (nodes) of the triangle
   integer               :: type !Type of triangle (upward, downward, cylinder)
  end type tria_data

  type node_data_yz
    integer :: gnode     !Global node number
   real(dp) :: y, z      !Coordinates in yz-plane.
  end type node_data_yz

  type node_data
   real(dp) :: x,y,z       !Coordinates in xyz space
   real(dp) :: nx,ny,nz    !Unit vector normal to the surface
   real(dp) :: nx2,ny2,nz2 !Unit vector along the second direction
  end type node_data

! Input variables
   integer :: igrid_type !Type of grid: 1=Prismatic, 2=Tet, 3=Mixed
   integer :: nr_gs      !Number of division of the generating sector

! Output File names

! (1) Tecplot files for viewing intermediate/final grids for debugging.
  character(80) :: filename_gs        = "debug_generating_sector.dat"
  character(80) :: filename_disk      = "debug_disk.dat"
  character(80) :: filename_surface   = "debug_hemisphere_surface.dat"
  character(80) :: filename_cylinder  = "debug_hemisphere_cylinder_surface.dat"
  character(80) :: filename_outer     = "debug_outer_boundary.dat"
  character(80) :: filename_tecplot_b
  character(80) :: filename_tecplot_v

! (2) These are the files used for CFD computations
  character(80) :: filename_mapbc
  character(80) :: filename_lines
  character(80) :: filename_ugrid
  character(80) :: filename_k

! Local variables
   integer :: os           !IO constant
  real(dp) :: x1           !Left end of the cylinder.
  real(dp) :: x2           !Rear end coordinate of the cylinder
  real(dp) :: distance     !Distance to the outer boundary from the body
   integer :: i,j,k,inode

   integer :: ntrias_gs    !Number of triangles in the generating sector
   integer :: nnodes_gs    !Number of nodes in the generating sector
  real(dp) :: Rd           !Radius of the hemisphere
  real(dp) :: r_gs         !Radius of the generating sector
  real(dp) :: dr_gs        !Spacing along the side of the generating sector
  real(dp) :: r2, dtheta, theta
  type(node_data_yz), dimension(:),     pointer :: nodes1, nodes2, node_gs
  integer, dimension(:),     pointer :: k1_gs, k2_gs, k3_gs
  type(tria_data)   , dimension(:), allocatable :: tria_gs

  integer :: nnodes_disk, ntrias_disk
  type(node_data_yz), dimension(:),     pointer :: node_disk
  integer, dimension(:),     pointer :: k1_disk
  integer, dimension(:),     pointer :: k2_disk
  integer, dimension(:),     pointer :: k3_disk
  type(tria_data)   , dimension(:), allocatable :: tria_disk

  real(dp) :: xp, yp, zp

  integer  :: nnodes
  real(dp) :: s
  type(node_data), dimension(:),     pointer :: node_body
  type(node_data), dimension(:),     pointer :: node_outer
  type(tria_data), dimension(:), allocatable :: tria
  integer, dimension(:),     pointer :: k1_body, k2_body, k3_body, k4_body

  integer, dimension(:), allocatable :: node_map
  integer :: node1, node2, node3

  integer :: nnodes_circum, nnodes_cylinder , ntrias
  integer, dimension(:)  , allocatable :: nodes_circum
  integer, dimension(:,:), allocatable :: nodes_cylinder
  real(dp) :: dx
  type(node_data), dimension(:), pointer :: node
  integer, dimension(:)  , allocatable :: k1, k2, k3, k4, k5

  integer  :: nr, nm
  real(dp) :: hx, rmax, drN, dr1, dr_outer, gr
  real(dp) :: sf, drnmp1 !Stretching factor in the outer region
  real(dp), allocatable, dimension(:) :: rs
  real(dp), allocatable, dimension(:) :: vspacing

  real(dp) :: dxnmp1

  integer :: nnodes_body
  integer :: node4, node5, node6
  integer :: ntrias_b, nquads_b, ntet, nprs,   nhex,nquads_cyl
  integer :: nquads_bcyl1 = 0, nquads_bcyl2 = 0, nquads_bcyl3 = 0
  integer :: nquads_bcyl_sym1 = 0, nquads_bcyl_sym2 = 0
  integer,  dimension(:,:), allocatable :: tri, quad, tet, prs, quad_cyl, hex
  integer,  dimension(:)  , allocatable :: node_above

  real(dp) :: xc, yc, zc
  real(dp) :: dirx, diry, dirz, blending, xi, sf2, xicyl, sf3

! Remove 6 nodes (Optional)
 
  integer, dimension(6)   :: nodes_removed, nodes_removed_nghbr
  integer, dimension(6,2) :: nodes_changed
  integer :: rnode, newnode
  logical :: deleted
  character(80) :: remove_corner_nodes

! For smoothing applied on the disk triangulation
  integer  :: i_smoothing, max_smoothing, v(3)
  real(dp) :: smoothing_factor, dyz_norm, dxyz_norm
  real(dp), dimension(:,:), allocatable :: dyz
  real(dp), dimension(:,:), allocatable :: dxyz
  integer  :: smoothing_type
  real(dp) :: nx, ny, nz

  real(dp) :: spacing_ratio

  integer :: p1, p2
  integer :: line_extent, mglevels, nnodes_cylinder_input

  logical :: b8_ugrid_format    = .false.
  logical :: generate_tec_files = .false.
  logical :: generate_k_file    = .false.
  logical :: generate_line_file = .false.

  real(dp) :: target_reynolds_number, target_yplus, cf
  logical  :: debug_mode
  integer  :: sk12p, sk23p, sk31p
  integer  :: sk12m, sk23m, sk31m
  integer  :: n_sections, n_temp


  nquads_bcyl1 = 0
  nquads_bcyl2 = 0
  nquads_bcyl3 = 0

      nhex = 0
      nprs = 0
      ntet = 0
  ntrias_b = 0
  nquads_b = 0

   debug_mode = .false.

    n_sections = 6 ! Full geometry
!   n_sections = 1 ! 60-degree section grid

!*******************************************************************************
! Hemisphere Cylinder Geometry (This is an axisymmetric 3D geometry.)
!
!      Outer boundary
!
!                       *
!                  *
!               * 
!            *
!          *
!         *
!       *                             z
!     *                               ^
!   *                                 |
!  *      ______________________      |
!  *     (______________________|     -------> x
!   *       Hemisphere cylinder
!     *
!       *
!         *
!          *
!            *
!              * 
!                  *
!                       *
!
! NOTE: The hemisphere has the unit diameter (radius = 0.5).
!
!*******************************************************************************
!  x0 = zero ! The apex of the hemisphere (leading edge location).
!  y0 = zero
!  z0 = zero

   x1 =  0.5_dp ! Hemisphere-cylinder junction (radius of hemisphere)
   Rd =  x1

!*******************************************************************************
! 0. Select the type of grid to generate.
!*******************************************************************************

   write(*,*) 
   write(*,*) " To determine the grid spacing above the wall..."
   write(*,*) "  1. Target Reynolds number = ?"
    read(*,*) target_reynolds_number
   write(*,*) "  2. Target y_plus value = ?"
    read(*,*) target_yplus

   write(*,*)
   write(*,*) 
   write(*,*) "Grid type = 1: Prismatic grid"
   write(*,*) "          = 2: Tetrahedral grid"
   write(*,*) "          = 3: Mixed grid: Tet/prism"
   write(*,*) "          = 4: Mixed grid: Prism/Hex"
 1 write(*,*) "grid_type = ?"

   read(*,*) igrid_type
   if (igrid_type/=1 .and. igrid_type/=2 .and. igrid_type/=3 .and. igrid_type/=4) then
    write(*,*) " >>> Invalid input value."
    go to 1
   endif

   write(*,*) "Use b8.ugrid format for grid (T/F)= ?"
   write(*,*) " F -> .ugrid    (formatted                   )"
   write(*,*) " T -> .b8.ugrid (unformatted stream bigEndian)"
   read(*,*) b8_ugrid_format

   if ( b8_ugrid_format) then
     write(*,*) ' Ensure big Endian -> setenv F_UFMTENDIAN big'
   else
     write(*,*) ' Using ascii type for ugrid file'
   endif

  if     ( igrid_type ==1  ) then
   filename_mapbc     = "hemisphere_prism.1.mapbc"
   filename_lines     = "hemisphere_prism.1.lines_fmt"
   if ( b8_ugrid_format ) then
   filename_ugrid     = "hemisphere_prism.1.b8.ugrid"
   else
   filename_ugrid     = "hemisphere_prism.1.ugrid"
   endif
   filename_k         = "hemisphere_prism.1.k"
   filename_tecplot_b = "hemisphere_prism_boundary_tec.1.dat"
   filename_tecplot_v = "hemisphere_prism_volume_tec.1.dat"
  elseif ( igrid_type ==2  ) then
   filename_mapbc     = "hemisphere_tetra.1.mapbc"
   filename_lines     = "hemisphere_tetra.1.lines_fmt"
   if ( b8_ugrid_format ) then
   filename_ugrid     = "hemisphere_tetra.1.b8.ugrid"
   else
   filename_ugrid     = "hemisphere_tetra.1.ugrid"
   endif
   filename_k         = "hemisphere_tetra.1.k"
   filename_tecplot_b = "hemisphere_tetra_boundary_tec.1.dat"
   filename_tecplot_v = "hemisphere_tetra_volume_tec.1.dat"
  elseif ( igrid_type ==3  ) then
   filename_mapbc     = "hemisphere_mixed.1.mapbc"
   filename_lines     = "hemisphere_mixed.1.lines_fmt"
   if ( b8_ugrid_format ) then
   filename_ugrid     = "hemisphere_mixed.1.b8.ugrid"
   else
   filename_ugrid     = "hemisphere_mixed.1.ugrid"
   endif
   filename_k         = "hemisphere_mixed.1.k"
   filename_tecplot_b = "hemisphere_mixed_boundary_tec.1.dat"
  elseif ( igrid_type ==4  ) then
   filename_mapbc     = "hemisphere_mixed_ph.1.mapbc"
   filename_lines     = "hemisphere_mixed_ph.1.lines_fmt"
   if ( b8_ugrid_format ) then
   filename_ugrid     = "hemisphere_mixed_ph.1.b8.ugrid"
   else
   filename_ugrid     = "hemisphere_mixed_ph.1.ugrid"
   endif
   filename_k         = "hemisphere_mixed_ph.1.k"
   filename_tecplot_b = "hemisphere_mixed_ph_boundary_tec.1.dat"
   filename_tecplot_v = "hemisphere_mixed_ph_volume_tec.1.dat"
   filename_tecplot_v = "hemisphere_mixed_ph_volume_tec.1.dat"
  endif

!   write(*,*) 
!   write(*,*) " Smoothing the hemisphere surface triangulation"
!   write(*,*) "   0 : No smoothing"
!   write(*,*) "   1 : Smoothing"
!   write(*,*) "   2 : Constrained smoothing"
!   read(*,*) smoothing_type

   smoothing_type = 0

!     remove_corner_nodes = Remove corner nodes for smooth triangulation.
!                           "yes" = remove;  "no" = don't remove
!   write(*,*) 
!   write(*,*) "--- Node removal recommended if smoothing applied..."
!   write(*,*) " Remove_corner_nodes? (Input yes or no)"
!   write(*,*) " NOTE: This should be 'no' for k1-k2-k3-k4 structured grid"
!   read(*,*) remove_corner_nodes

   remove_corner_nodes = "no"

!          smoothing_type = Smooth the surface triangulation.
!                           0 = no smoothing; 1 smoothing; 2 constrained smoothing
   write(*,*) 
   write(*,*) "--- Location of outer boundary..."
   write(*,*) " Distance to the outer boundary from the body = ? (e.g., 10.0)"
   read(*,*) distance

!           spacing_ratio = Transition from hemisphere to cylinder in mesh spacing
!                           E.g., spacing_ratio=2 gives the spacing of the first two
!                           nodes in the cylinder part twice as large as the mesh spacing
!                           in the hemisphere surface grid.
!   write(*,*) 
!   write(*,*) "--- Transition of spacing from hemisphere to cylinder..."
!   write(*,*) "    Specify the ratio, h_cyl/h_hemi (>1.0 for larger spacing in cylinder)"
!   write(*,*) " Spacing ratio = ?"
!   read(*,*) spacing_ratio

   spacing_ratio = one

   nnodes_cylinder_input = 0
   write(*,*) "---Number of nodes along the cylinder part = ?"
   read(*,*) nnodes_cylinder_input
   write(*,*) ' nodes_cylinder_input = ', nnodes_cylinder_input


   write(*,*) 
   write(*,*) "--- Length of the hemisphere-cylinder..."
   write(*,*) "    Apex of the hemisphere is at x=0, this is the position of"
   write(*,*) "    the end of the cylinder, which is equal to the length of HC."
   write(*,*) " Length of the hemisphere-cylinder = ?"
   read(*,*) x2

!   write(*,*) 
!   write(*,*) "--- Implicit lines..."
!   write(*,*) "   1 = Lines within a thin boundary layer region"
!   write(*,*) "   2 = Lines go all the way to the outer boundary."
!   write(*,*) " line_extent = ?"
!   read(*,*) line_extent
    line_extent = 1

! Note: The length of the hemisphere-cylinder is x2-x0.

!*******************************************************************************
! 1. Systematic triangulation of the generating sector (isotropic).
!    Resolution of this sector will determine other dimensions.
!
! This is a sector with the central angle 60 degrees.
! It is called here the generating sector.
! It is located in the yz-plane.
! We first triangulate this, and use it to build a triangulation of the
! whole circle (the disk).
!
!       ____________
!       \/\/\/\/\/\/    This is an example corresponding to nr_gs = 6.
!        \/\/\/\/\/
!         \/\/\/\/        z ^
!          \/\/\/           |
!           \/\/            |
!            \/             ----> y
!
! NOTE: The number of triangles = 1 + 3 + 5 + ... + 2*nr_gs-1 = nr_gs*nr_gs
!       The number of nodes  = 1 + 2 + 3 + ... + nr_gs+1
!
! NOTE: Important to distinguish two types of triangles.
!       They will define the way the prism is subdivided into 3 tets.
!       The subdivision must be done carefully to match the triangular faces
!       between two adjacent tetrahedra.
!
!*******************************************************************************

   write(*,*) "***********************************************************"
   write(*,*) " 1. Triangulate the generating sector, the building block"
   write(*,*) "***********************************************************"
   write(*,*) "Division of the generating sector = ?"

   read(*,*) nr_gs

!   write(*,*) "generate_tec_files (T/F) = ?"
!   read(*,*)   generate_tec_files
    generate_tec_files = .false.

!   write(*,*) "generate_k_file (T/F) = ?"
!   read(*,*)   generate_k_file
    generate_k_file = .false.

!   write(*,*) "generate_line_file (T/F) = ?"
!   read(*,*)   generate_line_file
    generate_line_file= .false.

   k = 0
   do
    k = k + 1
    if (mod(nr_gs,2**(k-1))==0) then
      mglevels = k
    else
      write(*,*)
      write(*,*) " Maximum multigrid level is ", mglevels
      write(*,*)
      exit
    endif
   end do

   r_gs = half*pi*Rd          ! Radius of the isotropic triangle
   write(*,*) " Radius of the isotropic triangle, r_gs = ", r_gs

  dr_gs = r_gs/real(nr_gs,dp) ! Uniform refinement
   write(*,*) " dr_gs = ", dr_gs

  nnodes_gs = (nr_gs+1)*((nr_gs+1)+1)/2
   write(*,*) " nnodes_gs = ", nnodes_gs
  ntrias_gs = nr_gs**2
   write(*,*) " ntrias_gs = ", ntrias_gs
  allocate(node_gs(nnodes_gs))
  allocate(tria_gs(ntrias_gs))
  allocate(k1_gs(nnodes_gs))
  allocate(k2_gs(nnodes_gs))
  allocate(k3_gs(nnodes_gs))

  nnodes_gs = 0
  ntrias_gs = 0

  triangulate : do i = 1, nr_gs

!   r1 = dr_gs * real(i-1,dp) ! Current arc
    r2 = dr_gs * real(i,dp)   !    Next arc

! Nodes on the current arc (r = r1)
   call my_alloc_ndyz_ptr(nodes1,i)

   if (i ==  1) then

      nodes1(1)%y = zero
      nodes1(1)%z = zero
      nodes1(1)%gnode = 1

        nnodes_gs = 1
     node_gs(1)%y = zero
     node_gs(1)%z = zero

     k1_gs(1) = 0
     k2_gs(1) = 0
     k3_gs(1) = - ( k1_gs(1) + k2_gs(1) )

   else

    do k = 1, i
     nodes1(k)%y     = nodes2(k)%y
     nodes1(k)%z     = nodes2(k)%z
     nodes1(k)%gnode = nodes2(k)%gnode
    end do

   endif

! Nodes on the next arc (r = r2): New nodes

    call my_alloc_ndyz_ptr(nodes2,i+1)
    dtheta = (pi/three) / real(i,dp)
    do k = 1, i+1
     theta = dtheta * real(k-1,dp)
     nodes2(k)%y = r2 * cos(theta)
     nodes2(k)%z = r2 * sin(theta)

     nnodes_gs = nnodes_gs + 1
     nodes2(k)%gnode = nnodes_gs
     node_gs(nnodes_gs)%y = nodes2(k)%y
     node_gs(nnodes_gs)%z = nodes2(k)%z


     k1_gs(nnodes_gs) = i + (1 - k)
     k2_gs(nnodes_gs) = k - 1
     k3_gs(nnodes_gs) = - ( k1_gs(nnodes_gs) + k2_gs(nnodes_gs) )


!    Keep the record of corner nodes and their neighbors for node removal
     if (trim(remove_corner_nodes) == "yes") then
      if (i==nr_gs-1 .and. k==1  ) nodes_removed_nghbr(1) = nnodes_gs
      if (i==nr_gs   .and. k==1  ) nodes_removed(1)       = nnodes_gs
      if (i==nr_gs-1 .and. k==i+1) nodes_removed_nghbr(2) = nnodes_gs
      if (i==nr_gs   .and. k==i+1) nodes_removed(2)       = nnodes_gs
     endif

    end do

! Triangulate the region between nodes1 and nodes2.
! Right to left

! NOTE: Nodes are ordered clockwise here. It will be counter-clockwise
!       when the hemisphere surface is seen from the interior domain.

! Type 1 triangle
!
! nodes2(k+1)   nodes2(k)
!      1             2
!       o-----------o
!        \         /  
!         \       /
!          \     /
!           \   /
!            \ /
!             o
!             3
!         nodes1(k)

   downward_tria : do k = 1, i
    ntrias_gs = ntrias_gs + 1
    tria_gs(ntrias_gs)%v(1) = nodes2(k+1)%gnode
    tria_gs(ntrias_gs)%v(2) = nodes2(k  )%gnode
    tria_gs(ntrias_gs)%v(3) = nodes1(k  )%gnode
    tria_gs(ntrias_gs)%type = 1
   end do downward_tria


! Type 2 triangle
!
!         nodes2(k+1)
!             3
!             o
!            / \
!           /   \
!          /     \ 
!         /       \
!        /         \
!       o-----------o
!      2             1
! nodes1(k+1)   nodes1(k)

   if (i > 1) then
    upward_tria : do k = 1, i-1
     ntrias_gs = ntrias_gs + 1
     tria_gs(ntrias_gs)%v(1) = nodes1(k  )%gnode
     tria_gs(ntrias_gs)%v(2) = nodes1(k+1)%gnode
     tria_gs(ntrias_gs)%v(3) = nodes2(k+1)%gnode
     tria_gs(ntrias_gs)%type = 2

     if (trim(remove_corner_nodes) == "yes") then
      if (i==nr_gs .and. k==i-1) then
       tria_gs(ntrias_gs)%type = 20
      endif
     endif

    end do upward_tria
   endif

  end do triangulate

! So, now I have a triangulation defined by tria_gs and node_gs.
! Number of triangles = ntrias_gs
! Number of nodes     = nnodes_gs

!*******************************************************************************
! Write a Tecplot file for the generating sector
!******************************************************************************
 debug_mode_01 : if (debug_mode) then
 open(unit=1, file=filename_gs, status="unknown", iostat=os)

  write(1,*) 'TITLE = "GRID"'
  write(1,*) 'VARIABLES = "x","y","z","k1+k2"'
  write(1,*) 'ZONE  N=', nnodes_gs,',E=', ntrias_gs,' , ET=triangle, F=FEPOINT'

! Nodes
  do i = 1, nnodes_gs
    write(1,'(3ES20.10,i10)') 0.0, node_gs(i)%y, node_gs(i)%z, k1_gs(i)+k2_gs(i)
  end do

! Triangles
  do i = 1, ntrias_gs
   write(1,'(3I10)') tria_gs(i)%v(1), tria_gs(i)%v(2), tria_gs(i)%v(3)
  end do

 close(1)

 write(*,*) "Tecplot file has been written: ", filename_gs
 endif debug_mode_01
!*******************************************************************************

!*******************************************************************************
! 2. Generate a triangulated disk.
!
! Now, rotate and copy this triangulation onto 5 places to form
! a triangulation of a whole disk (6 patches in total).
! Generate new data: tria_disk and node_disk
! Number of triangles = ntrias_disk
! Number of nodes     = nnodes_disk
!*******************************************************************************

  write(*,*) "***********************************************************"
  write(*,*) " 2. Generate a disk by copying the generating sector"
  write(*,*) "***********************************************************"

  nnodes_disk = nnodes_gs * n_sections !<- More than enough (by the # of overlapping ndoes)
  ntrias_disk = ntrias_gs * n_sections
  allocate(node_disk(nnodes_disk))
  allocate(tria_disk(ntrias_disk))
  allocate(k1_disk(nnodes_disk))
  allocate(k2_disk(nnodes_disk))
  allocate(k3_disk(nnodes_disk))

  nnodes_disk = 0
  ntrias_disk = 0

! Copy the data from the generating-sector data to the disk data.

!  Copy the node data
!  This is the first one: theta = 0 to 60 degrees

   do i = 1, nnodes_gs
    node_disk(i)%y = node_gs(i)%y
    node_disk(i)%z = node_gs(i)%z

        k1_disk(i) = k1_gs(i)
        k2_disk(i) = k2_gs(i)
        k3_disk(i) = k3_gs(i)

   end do

   nnodes_disk = nnodes_gs

!  Copy the triangle data
!  This is the first one: theta = 0 to 60 degrees

   do i = 1, ntrias_gs
    tria_disk(i)%v    = tria_gs(i)%v
    tria_disk(i)%type = tria_gs(i)%type
   end do

   ntrias_disk = ntrias_gs

! Now generate other parts of the disk: i=1,2,3,4,5
! 1. theta =  60 to 120 degrees
! 2. theta = 120 to 180 degrees
! 3. theta = 180 to 240 degrees
! 4. theta = 240 to 300 degrees
! 5. theta = 300 to 360 degrees

! Each part has (n+1)*(n+2)/2 nodes: 1+2+3+...+(nr_gs+1) = (n_gs+1)*(n_gs+2)/2
  allocate(node_map((nr_gs + 1)*(nr_gs + 2)/2))

 full_geometry : if (n_sections /= 1) then

 new_sectors : do i = 1, 5

! (1)Generate new nodes and construct a node map (one to one)
!    inode = local node numbering for node_map(:)

  node_map(1) = 1 !Node at the origin

  do k = 2, nr_gs + 1 !Origin to outer boundary
   do j = 1, k        !Right to left

    inode = (k-1)*k/2 + j !Local node number = Right-most node + increment(1,2,..,k)

!   Right side nodes are existing nodes: Left side nodes of the previous sector
    if (j==1) then

     if     (i == 1) then
      node_map(inode) = (k-1)*k/2 + k         !Left-most node of the original sector
     elseif (i == 2) then
      node_map(inode) = nnodes_gs + (k-1)*k/2 !Left-most node of the second sector
     else
      node_map(inode) = nnodes_gs + (nnodes_gs-(nr_gs+1))*(i-2) + (k-1)*k/2
     endif

!   Left side of the last one is the right side of the original sector
    elseif (i==5 .and. j==k) then

      node_map(inode) = (k-1)*k/2 + 1         !Right-most node of the original sector

!   New nodes: Rotate the original nodes by theta = i*pi/3 (i times 60 degrees).
    else

     theta = (pi/three) * real(i,dp)
     nnodes_disk = nnodes_disk + 1
     node_map(inode) = nnodes_disk
     node_disk(nnodes_disk)%y = cos(theta)*node_gs(inode)%y - sin(theta)*node_gs(inode)%z
     node_disk(nnodes_disk)%z = sin(theta)*node_gs(inode)%y + cos(theta)*node_gs(inode)%z

     if (i==1) then

      k1_disk(nnodes_disk) = -k2_gs(inode)
      k2_disk(nnodes_disk) = -k3_gs(inode)
      k3_disk(nnodes_disk) = -k1_gs(inode)

     elseif(i==2) then

      k1_disk(nnodes_disk) =  k3_gs(inode)
      k2_disk(nnodes_disk) =  k1_gs(inode)
      k3_disk(nnodes_disk) =  k2_gs(inode)
 
     elseif(i==3) then
 
      k1_disk(nnodes_disk) = -k1_gs(inode)
      k2_disk(nnodes_disk) = -k2_gs(inode)
      k3_disk(nnodes_disk) = -k3_gs(inode)

     elseif(i==4) then
 
      k1_disk(nnodes_disk) =  k2_gs(inode)
      k2_disk(nnodes_disk) =  k3_gs(inode)
      k3_disk(nnodes_disk) =  k1_gs(inode)

     elseif(i==5) then
 
      k1_disk(nnodes_disk) = -k3_gs(inode)
      k2_disk(nnodes_disk) = -k1_gs(inode)
      k3_disk(nnodes_disk) = -k2_gs(inode)

     endif

!    Keep the record of corner nodes and their neighbors
     if (trim(remove_corner_nodes) == "yes") then
      if (j == k .and. k == nr_gs+1 .and. i < 5) nodes_removed(i+2)       = nnodes_disk
      if (j == k .and. k == nr_gs   .and. i < 5) nodes_removed_nghbr(i+2) = nnodes_disk
     endif

    endif
   end do
  end do

! (2)Generate triangles on the new sector.

  do k = 1, ntrias_gs
   ntrias_disk = ntrias_disk + 1
   tria_disk(ntrias_disk)%v    = node_map(tria_gs(k)%v)
   tria_disk(ntrias_disk)%type = tria_gs(k)%type
  end do

 end do new_sectors

 endif full_geometry

  write(*,*) " nnodes_disk = ", nnodes_disk
  write(*,*) " ntrias_disk = ", ntrias_disk

!--------------------------------------------------------------------------------
! (If requested) Remove the six nodes (or equivalently remove 12 triangles)
! to avoid locally small cells typically caused by smoothing.
!
 if (trim(remove_corner_nodes) == "yes" .and. n_sections == 6) then

 remove_node : do k = 1, 6
    rnode = nodes_removed(k)
  newnode = nodes_removed_nghbr(k)

! (1)Remove the "rnode"

!  Move newnode to the boundary (rnode position)
   node_disk(newnode)%y = node_disk(rnode)%y
   node_disk(newnode)%z = node_disk(rnode)%z
!  Change the last node index to rnode to save/keep the last node
   node_disk(rnode)%y   = node_disk(nnodes_disk)%y
   node_disk(rnode)%z   = node_disk(nnodes_disk)%z
   nodes_changed(k,1)   = nnodes_disk
   nodes_changed(k,2)   = rnode
!  Reduce the dimension: "rnode" information is gone now.
   nnodes_disk = nnodes_disk - 1

! (2)Delete triangles with "rnode" and modify those having "nnodes_disk+1"

   i = 0
  do
   i = i + 1
   if ( i > ntrias_disk) exit

!  Delete if the triangle has "rnode" as its vertex.
   deleted = .false.
   if (tria_disk(i)%v(1) == rnode .or. &
       tria_disk(i)%v(2) == rnode .or. &
       tria_disk(i)%v(3) == rnode        ) then

       tria_disk(i)%v    = tria_disk(ntrias_disk)%v 
       tria_disk(i)%type = tria_disk(ntrias_disk)%type
       ntrias_disk = ntrias_disk - 1
       deleted = .true.
   endif

!  Check if the triangle has the node "nnodes_disk+1" (which no longer exists)
!  as its vertex. If so, overwrite it by rnode.
   if(.not.deleted) then

    if     (tria_disk(i)%v(1) == nnodes_disk+1) then
     tria_disk(i)%v(1) = rnode
    elseif (tria_disk(i)%v(2) == nnodes_disk+1) then
     tria_disk(i)%v(2) = rnode
    elseif (tria_disk(i)%v(3) == nnodes_disk+1) then
     tria_disk(i)%v(3) = rnode
    endif

   endif

!  If the triangle is removed, then shift the index to inspect the new
!  triangle ('i' is again a new triangle coming from ntrias_disk).
   if (deleted) i = i - 1

  end do

 end do remove_node

  write(*,*) " 6 corner nodes have been removed."
  write(*,*) " Updated nnodes_disk = ", nnodes_disk
  write(*,*) " Updated ntrias_disk = ", ntrias_disk

 endif

!--------------------------------------------------------------------------------
! (If requested) Apply smoothing on the disk triangulation

 if (smoothing_type /= 0 .and. n_sections == 6) then

  write(*,*) "Applying smoothing..."

  allocate(dyz(nnodes_disk,2))
  smoothing_factor = 0.05_dp
     max_smoothing = 1000

 smoothing : do i_smoothing = 1, max_smoothing

  dyz = zero !Initialize the changes

! Accumulate the changes by looping over triangles
  do i = 1, ntrias_disk

   v = tria_disk(i)%v

!  y-coordinate
    dyz(v(1),1) = dyz(v(1),1) + ( node_disk(v(2))%y - node_disk(v(1))%y )
    dyz(v(1),1) = dyz(v(1),1) + ( node_disk(v(3))%y - node_disk(v(1))%y )

    dyz(v(2),1) = dyz(v(2),1) + ( node_disk(v(1))%y - node_disk(v(2))%y )
    dyz(v(2),1) = dyz(v(2),1) + ( node_disk(v(3))%y - node_disk(v(2))%y )

    dyz(v(3),1) = dyz(v(3),1) + ( node_disk(v(1))%y - node_disk(v(3))%y )
    dyz(v(3),1) = dyz(v(3),1) + ( node_disk(v(2))%y - node_disk(v(3))%y )


!  z-coordinate
    dyz(v(1),2) = dyz(v(1),2) + ( node_disk(v(2))%z - node_disk(v(1))%z )
    dyz(v(1),2) = dyz(v(1),2) + ( node_disk(v(3))%z - node_disk(v(1))%z )

    dyz(v(2),2) = dyz(v(2),2) + ( node_disk(v(1))%z - node_disk(v(2))%z )
    dyz(v(2),2) = dyz(v(2),2) + ( node_disk(v(3))%z - node_disk(v(2))%z )


    dyz(v(3),2) = dyz(v(3),2) + ( node_disk(v(1))%z - node_disk(v(3))%z )
    dyz(v(3),2) = dyz(v(3),2) + ( node_disk(v(2))%z - node_disk(v(3))%z )

  end do

! Make changes to each node except the boundary nodes.
  dyz_norm = -1000000.0_dp
  do i=1, nnodes_disk

   if ( abs( sqrt(node_disk(i)%y**2 + node_disk(i)%z**2) - r_gs ) < 1.0e-14 ) then
   else

!  Constrained smoothing: skip nodes in the sector boundaries.
   if (smoothing_type == 2) then

    if ( abs(atan2(node_disk(i)%z, node_disk(i)%y) - pi/three)     < 1.0e-13 ) cycle
    if ( abs(atan2(node_disk(i)%z, node_disk(i)%y) + pi/three)     < 1.0e-13 ) cycle
    if ( abs(atan2(node_disk(i)%z, node_disk(i)%y) - two*pi/three) < 1.0e-13 ) cycle
    if ( abs(atan2(node_disk(i)%z, node_disk(i)%y) + two*pi/three) < 1.0e-13 ) cycle
    if ( abs(node_disk(i)%z) < 1.0e-13 ) cycle

   endif

     node_disk(i)%y = node_disk(i)%y + smoothing_factor * dyz(i,1)
     node_disk(i)%z = node_disk(i)%z + smoothing_factor * dyz(i,2)

!    L_inf norm of changes scaled by the typical mesh spacing, dr_gs.
     dyz_norm = max( dyz_norm, abs(dyz(i,1)**2 + dyz(i,2)**2)/dr_gs )

   endif

 end do

! Exit if converged
  if ( dyz_norm < 1.0e-04) then
   write(*,*) " Smoothing converged at ", i_smoothing
   exit smoothing
  elseif (i_smoothing == max_smoothing) then
   write(*,*) " Smoothing didn't converge... ", "  dyz_norm = ", dyz_norm
  endif

 end do smoothing

 deallocate(dyz)

 endif

 deallocate(k1_gs,k2_gs,k3_gs)

!--------------------------------------------------------------------------------

! Now, at this point, we have a triangulation of a disk defined by
! tria_disk and node_disk.
! Number of triangles = ntrias_disk
! Number of nodes     = nnodes_disk

!*******************************************************************************
! Write a Tecplot file for the triangulated disk.
!******************************************************************************
 debug_mode_02 : if (debug_mode) then
 open(unit=2, file=filename_disk, status="unknown", iostat=os)

  write(2,*) 'TITLE = "GRID"'
  write(2,*) 'VARIABLES = "x","y","z","k1","k2","k3","kd"'
  write(2,*) 'ZONE  N=', nnodes_disk,',E=', ntrias_disk,' , ET=triangle, F=FEPOINT'

! Nodes
  do i = 1, nnodes_disk

   sk12p = ( 1 + int_sign( k1_disk(i)*k2_disk(i) ) )/2
   sk23p = ( 1 + int_sign( k2_disk(i)*k3_disk(i) ) )/2
   sk31p = ( 1 + int_sign( k3_disk(i)*k1_disk(i) ) )/2

   sk12m = ( 1 - int_sign( k1_disk(i)*k2_disk(i) ) )/2
   sk23m = ( 1 - int_sign( k2_disk(i)*k3_disk(i) ) )/2
   sk31m = ( 1 - int_sign( k3_disk(i)*k1_disk(i) ) )/2

   if     (sk12p > 0) then

     sk23p = 0
     sk23m = 1

     sk31p = 0
     sk31m = 1

   elseif (sk23p > 0) then

     sk12p = 0
     sk12m = 1

     sk31p = 0
     sk31m = 1

   elseif (sk31p > 0) then

     sk12p = 0
     sk12m = 1

     sk23p = 0
     sk23m = 1

   endif

    write(2,'(3ES20.10,4i10)') 0.0,  node_disk(i)%y, node_disk(i)%z, k1_disk(i),k2_disk(i),k3_disk(i), &
    sk31m*sk23m*sk12p*abs(k3_disk(i)) + &
    sk31m*sk12m*sk23p*abs(k1_disk(i)) + &
    sk23m*sk12m*sk31p*abs(k2_disk(i))

  end do

! Triangles
  do i = 1, ntrias_disk
   write(2,'(3I10)') tria_disk(i)%v(1), tria_disk(i)%v(2), tria_disk(i)%v(3)
  end do

 close(2)

 write(*,*) "Tecplot file has been written: ", filename_disk
 endif debug_mode_02
!*******************************************************************************

!*******************************************************************************
! 3. Map the disk triangulation onto the hemisphere surface.
!
! OK, now, let's gently attach the disk onto the hemisphere.
! This is done locally at each node, just by using the node_disk data.
! It doesn't matter how they are ordered.
! Connectivity data are unchanged.
!*******************************************************************************

  write(*,*) "***********************************************************"
  write(*,*) " 3. Place the disk onto the hemisphere "
  write(*,*) "***********************************************************"

  s = (half*pi*Rd) / real(nr_gs,dp)
  write(*,*) " s = ", s 
  nnodes_cylinder = int( (x2-x1)/s )   !Isotropic grid
  nnodes_cylinder = int( (x2-x1)/s )/12

   if (mglevels>1) then

   write(*,*) " nnodes_cylinder (original) = ", nnodes_cylinder
   p1 = nnodes_cylinder
   do k = 1, mglevels
    if (mod(p1/2**(k-1),2)/=0) p1 = p1 - 2**(k-1)
   end do

   p2 = nnodes_cylinder
   do k = 1, mglevels
    if (mod(p2/2**(k-1),2)/=0) p2 = p2 + 2**(k-1)
   end do

   if (nnodes_cylinder-p1 < p2-nnodes_cylinder .and. p1/=0) then
!    nnodes_cylinder = p1
   else
!    nnodes_cylinder = p2
   endif

!    nnodes_cylinder = 2*nr_gs
!   write(*,*) " nnodes_cylinder (adjusted) = ", nnodes_cylinder

   endif

! Use input value if requested
  if (nnodes_cylinder_input > 0) then
    nnodes_cylinder = nnodes_cylinder_input
  endif

  write(*,*) " nnodes_cylinder (actual) = ", nnodes_cylinder

!  nnodes_circum = 6*(nr_gs)
  nnodes_circum = n_sections*nr_gs

  if (n_sections == 1) nnodes_circum = nnodes_circum + 1

  nnodes = nnodes_disk + nnodes_circum*(nnodes_cylinder+1)
  allocate(node_body(nnodes))
  allocate(k1_body(nnodes))
  allocate(k2_body(nnodes))
  allocate(k3_body(nnodes))
  allocate(k4_body(nnodes))

   nnodes = 0

  do i = 1, nnodes_disk

   k1_body(i) = -k1_disk(i) ! Flip the sign JFC
   k2_body(i) = -k2_disk(i) ! Flip the sign JFC
   k3_body(i) = -k3_disk(i) ! Flip the sign JFC
   k4_body(i) =  0          ! <- On the hemisphere

! Push the node onto the circle located at x=Rd.
   s = sqrt(node_disk(i)%y**2 + node_disk(i)%z**2)
   if (i==1) then
    yp = zero
    zp = zero
   else
    yp = node_disk(i)%y/s * Rd !Extend it to the circle of radius Rd
    zp = node_disk(i)%z/s * Rd !Extend it to the circle of radius Rd
   endif

!    xp = Rd  The circle is located at x = Rd.

! Now, the node (xp,yp,zp) is located on the perimeter of the circle at x=Rd.
! Rotate the node onto the sphere.
            theta = s/Rd
           nnodes = nnodes + 1
   node_body(nnodes)%y = yp*sin(theta)
   node_body(nnodes)%z = zp*sin(theta)
   node_body(nnodes)%x = Rd - Rd*cos(theta)

!  Surface normal direction along which we go up to generate prismatic elements.
   node_body(nnodes)%nx = node_body(nnodes)%x - Rd
   node_body(nnodes)%ny = node_body(nnodes)%y
   node_body(nnodes)%nz = node_body(nnodes)%z

!  Make it the unit vector (well, probably already a unit vector, though...)
   sf = sqrt(node_body(nnodes)%nx**2 + node_body(nnodes)%ny**2 + node_body(nnodes)%nz**2)
   node_body(nnodes)%nx = node_body(nnodes)%nx / sf
   node_body(nnodes)%ny = node_body(nnodes)%ny / sf
   node_body(nnodes)%nz = node_body(nnodes)%nz / sf

  end do

 deallocate(k1_disk,k2_disk,k3_disk)
 write(*,*) " nnodes, nnodes_disk = ", nnodes, nnodes_disk 

!*******************************************************************************
! Write a Tecplot file for the triangulated hemisphere surface.
!******************************************************************************
 debug_mode_03 : if (debug_mode) then
 open(unit=3, file=filename_surface, status="unknown", iostat=os)

  write(3,*) 'TITLE = "GRID"'
  write(3,*) 'VARIABLES = "x","y","z","k1+k2"'
  write(3,*) 'ZONE  N=', nnodes,',E=', ntrias_disk,' , ET=triangle, F=FEPOINT'

! Nodes
  do i = 1, nnodes

    write(3,'(3ES20.10,i10)') node_body(i)%x,  node_body(i)%y, node_body(i)%z, &
                          k1_body(i)+ k2_body(i)
  end do

! Triangles
  do i = 1, ntrias_disk
   write(3,'(3I10)') tria_disk(i)%v(1), tria_disk(i)%v(2), tria_disk(i)%v(3)
  end do

 close(3)

 write(*,*) "Tecplot file has been written: ", filename_surface
 endif debug_mode_03
!*******************************************************************************

!*******************************************************************************
! 4. Triangulate the cylinder surface.
!
! NOTE: Stretching is applied to the distribution of nodes along the cylinder
!       for a smooth the spacing variation from the shoulder of the hemisphere.
!
! NOTE: Important to distinguish two types of triangles.
!       They will define the way the prism is subdivided into 3 tets.
!       The subdivision must be done carefully to match the triangular faces
!       between two adjacent tetrahedra.
!
!*******************************************************************************

  write(*,*) "***********************************************************"
  write(*,*) " 4. Triangulate the cylinder surface"
  write(*,*) "***********************************************************"

! OK, now generate a regular triangulation over the cylinder part.

  write(*,*) " nnodes along the cylinder = ", nnodes_cylinder

! Construct the list of nodes around the hemisphere at the shoulder.
! nodes_cicum(1:nnodes_circum)
! The list starts at the node located at (y,z)=(0.5,0)
! and goes in the negative y-direction (rotating y-axis to z-axis, pointing
! the positive x-direction).

  allocate(nodes_circum(nnodes_circum+1))
  write(*,*) " nnodes around the cylinder = ", nnodes_circum

  nnodes_circum = 0

! Circum of generating sector
  do k = 1, nr_gs+1
   nnodes_circum = nnodes_circum + 1
   nodes_circum(nnodes_circum) = nr_gs*(nr_gs+1)/2 + k
  end do

!-------------------------------------------------------------------------------
 full_geom : if (n_sections == 6) then

! Circum of other sectors
  do i = 1, 4
   do k = 1, nr_gs
    nnodes_circum = nnodes_circum + 1
    nodes_circum(nnodes_circum) = nnodes_gs + (i-1)*(nr_gs+1)*nr_gs/2 + (nr_gs-1)*nr_gs/2 + k
   end do
  end do

! i == 5
   do k = 1, nr_gs-1
    nnodes_circum = nnodes_circum + 1
    nodes_circum(nnodes_circum) = nnodes_gs + (5-1)*(nr_gs+1)*nr_gs/2 + (nr_gs-2)*(nr_gs-1)/2 + k
   end do

  nodes_circum(nnodes_circum + 1) = nodes_circum(1)

! Fix the node number if 6 nodes have been removed.

  if (trim(remove_corner_nodes) == "yes" .and. n_sections == 6) then

   do i = 1, nnodes_circum + 1

    do k = 1, 6

     if (nodes_circum(i) == nodes_removed(k)) then

      nodes_circum(i) = nodes_removed_nghbr(k)

     elseif (nodes_circum(i) == nodes_changed(k,1)) then

      nodes_circum(i) = nodes_changed(k,2)

     endif

    end do

   end do

  endif

  endif full_geom
!-------------------------------------------------------------------------------

! Compute the stretching factor for a smooth stretching along the cylinder.
! The stretching is activated only if we don't have enough nodes to generate
! an isotropic grid. The default is the isotropic grid.

!   Length of the edge on the hemisphere surface
    s = (half*pi*Rd) / real(nr_gs,dp)

!   18% larger spacing for a smooth transition
    s = s*1.18_dp
!   700% larger spacing for a smooth transition
!    s = s*7.0_dp

!   User specified spacing for a smooth transition
    s = s*spacing_ratio

! Stretching is applied if nnodes_cylinder is less than that
! for a uniform spacing.
  if (nnodes_cylinder < int( (x2-x1)/s )) then

    dx = (x2-x1)/real(nnodes_cylinder,dp)
    sf = 2.0_dp

   do k = 1, 500

    dxnmp1 = (one-exp(sf*dx/(x2-x1)))/(one-exp(sf)) * (x2-x1)

    if (abs(dxnmp1 - s )/s < 1.0e-03_dp) exit

    if (dxnmp1 > s ) then
      sf = sf + 0.01_dp
    else
      sf = sf - 0.01_dp
    endif

   end do

   write(*,*) "Determined stretching factor = ",sf, " at iterations = ", k
   write(*,*) "          First spacing on the cylinder = ", dxnmp1
   write(*,*) "  Edge length on the hemisphere surface = ", s/1.18_dp

  endif

! Generate nodes on the cylinder surface

 full_geom2 : if (n_sections == 6) then

  allocate(nodes_cylinder(nnodes_circum+1,nnodes_cylinder+1))

! Copy the data
  do k = 1, nnodes_circum+1
   nodes_cylinder(k,1) = nodes_circum(k)
  end do

! Uniform spacing
  dx = (x2-x1)/real(nnodes_cylinder,dp)

! Move along the cylinder in the positive x-direction
  do i = 2, nnodes_cylinder+1

!  Go around the circumfenrential direction
   do k = 1, nnodes_circum

    nnodes = nnodes + 1
    nodes_cylinder(k,i) = nnodes
    node_body(nnodes)%y = node_body(nodes_circum(k))%y
    node_body(nnodes)%z = node_body(nodes_circum(k))%z

    if (nnodes_cylinder /= int( (x2-x1)/s )) then
!    Stretched
     node_body(nnodes)%x = x1 + (one-exp(sf*real(i-1,dp)*dx/(x2-x1)))/(one-exp(sf))*(x2-x1)
    else
!    Uniform spacing
    node_body(nnodes)%x  = node_body(nodes_circum(k))%x + real(i-1,dp)*dx
    endif

!   Unit vector normal to the cylinder surface.
    node_body(nnodes)%nx = zero
    node_body(nnodes)%ny = node_body(nodes_circum(k))%ny
    node_body(nnodes)%nz = node_body(nodes_circum(k))%nz

    k1_body(nnodes) = k1_body(nodes_circum(k))
    k2_body(nnodes) = k2_body(nodes_circum(k))
    k3_body(nnodes) = k3_body(nodes_circum(k))
    k4_body(nnodes) = i-1                     ! <- Cylinder index; 0 on the hemisphere

    if (k==1)  nodes_cylinder(nnodes_circum+1,i) = nnodes

   end do

  end do

 elseif (n_sections == 1) then

  allocate(nodes_cylinder(nnodes_circum,nnodes_cylinder+1))

! Copy the data
  do k = 1, nnodes_circum
   nodes_cylinder(k,1) = nodes_circum(k)
  end do

! Uniform spacing
  dx = (x2-x1)/real(nnodes_cylinder,dp)

! Move along the cylinder in the positive x-direction
  do i = 2, nnodes_cylinder+1

!  Go around the circumferential direction
   do k = 1, nnodes_circum

    nnodes = nnodes + 1
    nodes_cylinder(k,i) = nnodes
    node_body(nnodes)%y = node_body(nodes_circum(k))%y
    node_body(nnodes)%z = node_body(nodes_circum(k))%z

    if (nnodes_cylinder /= int( (x2-x1)/s )) then
!    Stretched
     node_body(nnodes)%x = x1 + (one-exp(sf*real(i-1,dp)*dx/(x2-x1)))/(one-exp(sf))*(x2-x1)
    else
!    Uniform spacing
    node_body(nnodes)%x  = node_body(nodes_circum(k))%x + real(i-1,dp)*dx
    endif

!   Unit vector normal to the cylinder surface.
    node_body(nnodes)%nx = zero
    node_body(nnodes)%ny = node_body(nodes_circum(k))%ny
    node_body(nnodes)%nz = node_body(nodes_circum(k))%nz

    k1_body(nnodes) = k1_body(nodes_circum(k))
    k2_body(nnodes) = k2_body(nodes_circum(k))
    k3_body(nnodes) = k3_body(nodes_circum(k))
    k4_body(nnodes) = i-1                     ! <- Cylinder index; 0 on the hemisphere

   end do

  end do

 endif full_geom2

 write(*,*) " nnodes_circum = ", nnodes_circum

!*************************************************************************************
!*************************************************************************************
!*************************************************************************************
  mixed_ph_cylinder : if ( igrid_type == 4 ) then

! Allocate and copy the hemisphere triangulation data to the global array

  ntrias = ntrias_disk ! + nnodes_circum*nnodes_cylinder*2
  allocate(tria(ntrias))

  nquads_cyl = nnodes_circum * nnodes_cylinder
  allocate(quad_cyl(nquads_cyl,5))

! Copy the data for triangles (DONE)
  do i = 1, ntrias
   tria(i)%v    = tria_disk(i)%v
   tria(i)%type = tria_disk(i)%type
  end do

! Quadrangulate(?) the cylinder surface

  nquads_cyl = 0

  if     (n_sections==6) then
   n_temp = nnodes_circum
  elseif (n_sections==1) then
   n_temp = nnodes_circum - 1
  endif

  do i = 1, nnodes_cylinder
   do k = 1, n_temp

!
!   (k,i)        (k,i+1)
!      1           4
!       o----------o          ------> x
!       |          |          |
!       |          |          |
!       |          |          |
!       |          |          v
!       |          |   Circumferential direction
!       o----------o
!       2          3
!   (k+1,i)    (k+1,i+1)

      node1 = nodes_cylinder(k  ,i  )
      node2 = nodes_cylinder(k+1,i  )
      node3 = nodes_cylinder(k+1,i+1)
      node4 = nodes_cylinder(k  ,i+1)
     nquads_cyl = nquads_cyl + 1
     quad_cyl(nquads_cyl,1) = node1
     quad_cyl(nquads_cyl,2) = node2
     quad_cyl(nquads_cyl,3) = node3
     quad_cyl(nquads_cyl,4) = node4
     quad_cyl(nquads_cyl,5) = 1

   end do
  end do


!*************************************************************************************
!*************************************************************************************
!*************************************************************************************
  else

! Allocate and copy the hemisphere triangulation data to the global array

  ntrias = ntrias_disk + nnodes_circum*nnodes_cylinder*2
  allocate(tria(ntrias))

! Copy the data
  do i = 1, ntrias_disk
   tria(i)%v    = tria_disk(i)%v
   tria(i)%type = tria_disk(i)%type
  end do

! Triangulate the cylinder surface

  ntrias = ntrias_disk

  if     (n_sections==6) then
   n_temp = nnodes_circum
  elseif (n_sections==1) then
   n_temp = nnodes_circum - 1
  endif

  do i = 1, nnodes_cylinder
   do k = 1, n_temp

! Type 3 triangle
!
!   (k,i)        (k,i+1)
!      1            3
!       o----------o          ------> x
!       |        .            |
!       |      .              |
!       |    .                |
!       |  .                  v
!       |.          Circumferential direction
!       o
!       2
!   (k+1,i)

      node1 = nodes_cylinder(k  ,i  )
      node2 = nodes_cylinder(k+1,i  )
      node3 = nodes_cylinder(k  ,i+1)
     ntrias = ntrias + 1
     tria(ntrias)%v(1) = node1
     tria(ntrias)%v(2) = node2
     tria(ntrias)%v(3) = node3
     tria(ntrias)%type = 3

! Type 4 triangle
!
!              (k,i+1)
!                  2
!                  o              ------> x
!                . |              |
!             .    |              |
!           .      |              |
!         .        |              v
!       .          |    Circumferential direction
!      o---------- o
!      3           1
!    (k+1,i)    (k+1,i+1)

      node1 = nodes_cylinder(k+1,i+1)
      node2 = nodes_cylinder(k  ,i+1)
      node3 = nodes_cylinder(k+1,i  )
     ntrias = ntrias + 1
     tria(ntrias)%v(1) = node1
     tria(ntrias)%v(2) = node2
     tria(ntrias)%v(3) = node3
     tria(ntrias)%type = 4

   end do
  end do

  endif mixed_ph_cylinder
!*************************************************************************************
!*************************************************************************************
!*************************************************************************************

!*******************************************************************************
! Write a Tecplot file for the hemisphere cylinder surface triangulation.
!******************************************************************************
 debug_mode_04 : if (debug_mode) then
 open(unit=4, file=filename_cylinder, status="unknown", iostat=os)

  write(4,*) 'TITLE = "GRID"'
  write(4,*) 'VARIABLES = "x","y","z"'
  write(4,*) 'ZONE  N=', nnodes,',E=', ntrias,' , ET=triangle, F=FEPOINT'

! Nodes
  do i = 1, nnodes
    write(4,'(3ES20.10)') node_body(i)%x, node_body(i)%y, node_body(i)%z
  end do

! Triangles
  do i = 1, ntrias
   write(4,'(3I10)') tria(i)%v(1), tria(i)%v(2), tria(i)%v(3)
  end do

 close(4)

 write(*,*) "Tecplot file has been written: ", filename_cylinder

 endif debug_mode_04
!*******************************************************************************

write(*,*) "    Completed ", filename_cylinder

!*******************************************************************************
! 5. We now go up to generate the interior nodes.
!
! Determine the vertical spacing, vspacing: r(i+1) = vspacing(i) + r(i),
! and generate interior nodes by going up in the direction of surface normal.
!
!   Geometric sequence       Exponential stretching
! o-o--o---o----o-----o-----x-------x----------x--------------x        -----> r
! 1 2  3              nm                                 nr
!   Prismatic layer       Outer tetrahedral region
!
! NOTE: Here, r indicates the direction of surface normal.
! NOTE: The number of nodes in the prismatic layer (nm) is automatically determined
!       for a specified the aspect ratio of the first element on the body = 50
!
! NOTE: The exponential stretching is determined by iterations to equate
!       the spacing of the elements below and above the nm-th node.
!
!*******************************************************************************

  write(*,*) "***********************************************************"
  write(*,*) " 5. Go up to the outer boundary and generate interior nodes"
  write(*,*) "***********************************************************"

    hx = (half*pi*Rd) / real(nr_gs,dp)
     s = pi*Rd

! rmax = thickness of the prismatic region (boundary layer):

  rmax = 1.0e-01_dp

  write(*,*) " Boundary layer grid thickness = ", rmax

! Compute the parameters for geometric sequence.
! Determine the number of nodes in the r-direction for a given max AR.

  drN = hx !* 0.5_dp !Spacing of the outer most cell in the layer.

  if (drN > rmax) then
   write(*,*) ">>>>> drN > rmax !! Impossible... drN = hx = ", drN
   drN = rmax *0.5_dp
   write(*,*) ">>>>> Modified drN = rmax *0.5_dp = ", drN
  endif

!   dr1 = 7.0e-05_dp  * ( 27.0_dp / real(nr_gs, dp) )
!   write(*,*) " original dr1 = ", dr1

    cf = 0.026_dp/target_reynolds_number**(1.0_dp/7.0_dp)
   dr1 = ( sqrt(2.0_dp/cf)/target_reynolds_number) * target_yplus
   write(*,*) "--- dr1 determined by the target Re and y_plus:"
   write(*,*) " target_reynolds_number = ", target_reynolds_number
   write(*,*) " target_yplus = ", target_yplus
   write(*,*) " --> dr1 = ", dr1 
   write(*,*) "      AR = ", hx/dr1
   gr = (rmax-dr1)/(rmax-drN)
   nm = ceiling( log(gr*drN/dr1)/log(gr) )
   gr = (drN/dr1)**( one/(real(nm,dp)-one) )

   write(*,*)
   write(*,*) " NOTE: dr1 is a non-dimensionalized length. "
   write(*,*) "       Reference length is the diameter of the hemisphere (=1.0 in the grid)."
   write(*,*)

   write(*,*) "hx (mesh spacing along the cylinder in x) = ", hx
   write(*,*) "rmax (the end of the boundary layer)      = ", rmax
   write(*,*) "dr1 = ", dr1
   write(*,*) "drN = ", drN
!   write(*,*) "nm (elements) = ", nm
   write(*,*) "gr = ", gr
   write(*,*) "dr1*gr**(nm-1) = ", dr1*gr**(nm-1)

   if (mglevels>1) then

   write(*,*) " nm (original) = ", nm
   p1 = nm
   do k = 1, mglevels
    if (mod(p1/2**(k-1),2)/=0) p1 = p1 - 2**(k-1)
   end do

   p2 = nm
   do k = 1, mglevels
    if (mod(p2/2**(k-1),2)/=0) p2 = p2 + 2**(k-1)
   end do

   if (nm-p1 < p2-nm .and. p1/=0) then
!    nm = p1
   else
!    nm = p2
   endif

!    nm = p1

!   write(*,*) " nm (adjusted) = ", nm

   endif

!  At this point, nm is the number of elements within the layer.
!  Add 1 to make it the number of nodes.
   nm = nm + 1 ! where change the type of cell from prism to tetra for a mixed grid.
   write(*,*) "nm (nodes) = ", nm

!  Define the number of nodes in the outer tet region.
!  Half the number of nodes that generates an equal spacing hx,
!  considering that the cell expands to the outer boundary.

     nr = nm + int(ceiling( (distance - rmax) / hx )/ (10.0_dp * ( 0.1_dp*real(nr_gs)/5.0_dp + 0.9_dp ) ))
!     nr = nm + ceiling( (distance - rmax) / hx )/ (3.0_dp * ( 0.1_dp*real(nr_gs)/5.0_dp + 0.9_dp ) )
!     nr = nm + ceiling( (distance - rmax) / hx )/3

   if (mglevels>1) then

   write(*,*) " nr (original) = ", nr
   p1 = nr
   do k = 1, mglevels
    if (mod(p1/2**(k-1),2)/=0) p1 = p1 - 2**(k-1)
   end do

   p2 = nr
   do k = 1, mglevels
    if (mod(p2/2**(k-1),2)/=0) p2 = p2 + 2**(k-1)
   end do

   if (nr-p1 < p2-nr .and. p1/=0) then
!    nr = p1
   else
!    nr = p2
   endif

!    nr = 4*nr_gs
   write(*,*) " nr (adjusted) = ", nr

   endif

nr=nr-5
   write(*,*) " nr (adjusted) = ", nr




    nr = nr + 1

     write(*,*) "nr = ", nr

!  Allocate the vertical spacing array
   allocate(vspacing(nr))

  do i = 1, nr

   if (i == 1) then

    vspacing(i) = dr1

!  Geometric sequence for the cell spacing inside the layer.
   elseif (i < nm) then

    vspacing(i) = dr1*gr**(i-1)

!  Outer tet region: uniform spacing for now.
   else

    vspacing(i) = vspacing(i-1)

   endif

  end do

! NOTE: The last node in the prismatic layer is nm-th node.
!       The spacing array vspacing() goes from 1 to nm-1.

! Create spacing array.
  allocate(rs(nr))
  rs(1) = zero

! Compute the vertical coordinate based on the spacing generated by
! the geometric sequence as above.
  do i = 2, nm
   rs(i) = vspacing(i-1) + rs(i-1)
  end do

 write(*,*)
 write(*,*) "Vertical spacing of the first interior node = ",vspacing(1)
 write(*,*) "Actual last spacing = ", vspacing(nm-1)
 write(*,*) "Actual end of the prismatic layer = ", rs(nm)
 write(*,*)

! Uniform spacing in the outer region for now (tetrahedral for mixed grids).
   dr_outer = (distance - rs(nm))/real(nr-nm,dp)
  do i = nm+1, nr
   rs(i) = rs(nm) + real(i-nm,dp)*dr_outer
  end do

! Compute the stretching factor for a smooth transition from the boundary layer to
! outer region.

!  Target spacing: 1.3 times the spacing below for a smooth transition
    s = (rs(nm)-rs(nm-1)) * 3.0_dp

!    s = 0.5_dp * hx

!  Initial value
   sf = 2.0_dp

!  Determine sf iteratively
   do k = 1, 500

    drnmp1 = (one-exp(sf*(rs(nm+1)-rs(nm))/(distance-rs(nm))))/(one-exp(sf)) * distance

    if (abs(drnmp1 - s)/s < 1.0e-02_dp) exit

    if (drnmp1 > s ) then
      sf = sf + 0.01_dp
    else
      sf = sf - 0.01_dp
    endif
   
   end do

  write(*,*) "Determined stretching factor = ",sf, " at iterations = ", k

! Apply the exponential stretching in the outer region.

  do i = nm+1, nr
   rs(i) = rs(nm) + (one-exp(sf*(rs(i)-rs(nm))/(distance-rs(nm))))/(one-exp(sf)) * distance
  end do

  write(*,*) " Space above must be close to spave below for smooth transition"
  write(*,*) " from the prismatic layer to the tet region."
  write(*,*) "  Space below = ", rs(nm) - rs(nm-1)
  write(*,*) "  Space above = ", rs(nm+1) - rs(nm)

! Re-compute the spacing modified by the stretching.
  do i = nm+1, nr
   vspacing(i-1) = rs(i) - rs(i-1)
  end do

! Generate nodes by going up along the surface normal from each node on the body.

  nnodes_body = nnodes
          nnodes = nnodes*nr
  allocate(node(nnodes))
  allocate(node_above(nr*nnodes_body))
  allocate(k1(nnodes))
  allocate(k2(nnodes))
  allocate(k3(nnodes))
  allocate(k4(nnodes))
  allocate(k5(nnodes))

  write(*,*) " nnodes_body =", nnodes_body 
  do i = 1, nnodes_body
   node(i)%x  = node_body(i)%x
   node(i)%y  = node_body(i)%y
   node(i)%z  = node_body(i)%z
   node(i)%nx = node_body(i)%nx
   node(i)%ny = node_body(i)%ny
   node(i)%nz = node_body(i)%nz
   k1(i)  = k1_body(i)
   k2(i)  = k2_body(i)
   k3(i)  = k3_body(i)
   k4(i)  = k4_body(i)
   k5(i)  = 0          ! <- On the hemisphere cylinder
  end do


!--------------------------------------------------------------------------------
! Nodes on the outer boundary

  allocate(node_outer(nnodes_body))
  do i = 1, nnodes_body
   node_outer(i)%x  = node_body(i)%x  +  distance * node_body(i)%nx
   node_outer(i)%y  = node_body(i)%y  +  distance * node_body(i)%ny
   node_outer(i)%z  = node_body(i)%z  +  distance * node_body(i)%nz
  end do

! Apply smoothing on the outer boundary

  write(*,*) "Applying smoothing (OUTER BOUNDARY)..."

  allocate(dxyz(nnodes_body,3))
  smoothing_factor = 0.05_dp
     max_smoothing = 0

 smoothing_outer : do i_smoothing = 1, max_smoothing

  dxyz = zero !Initialize the changes

! Accumulate the changes by looping over triangles
  do i = 1, ntrias

   v = tria(i)%v

!  x-coordinate
    dxyz(v(1),1) = dxyz(v(1),1) + ( node_outer(v(2))%x - node_outer(v(1))%x )
    dxyz(v(1),1) = dxyz(v(1),1) + ( node_outer(v(3))%x - node_outer(v(1))%x )

    dxyz(v(2),1) = dxyz(v(2),1) + ( node_outer(v(1))%x - node_outer(v(2))%x )
    dxyz(v(2),1) = dxyz(v(2),1) + ( node_outer(v(3))%x - node_outer(v(2))%x )

    dxyz(v(3),1) = dxyz(v(3),1) + ( node_outer(v(1))%x - node_outer(v(3))%x )
    dxyz(v(3),1) = dxyz(v(3),1) + ( node_outer(v(2))%x - node_outer(v(3))%x )

!  y-coordinate
    dxyz(v(1),2) = dxyz(v(1),2) + ( node_outer(v(2))%y - node_outer(v(1))%y )
    dxyz(v(1),2) = dxyz(v(1),2) + ( node_outer(v(3))%y - node_outer(v(1))%y )

    dxyz(v(2),2) = dxyz(v(2),2) + ( node_outer(v(1))%y - node_outer(v(2))%y )
    dxyz(v(2),2) = dxyz(v(2),2) + ( node_outer(v(3))%y - node_outer(v(2))%y )

    dxyz(v(3),2) = dxyz(v(3),2) + ( node_outer(v(1))%y - node_outer(v(3))%y )
    dxyz(v(3),2) = dxyz(v(3),2) + ( node_outer(v(2))%y - node_outer(v(3))%y )

!  z-coordinate
    dxyz(v(1),3) = dxyz(v(1),3) + ( node_outer(v(2))%z - node_outer(v(1))%z )
    dxyz(v(1),3) = dxyz(v(1),3) + ( node_outer(v(3))%z - node_outer(v(1))%z )

    dxyz(v(2),3) = dxyz(v(2),3) + ( node_outer(v(1))%z - node_outer(v(2))%z )
    dxyz(v(2),3) = dxyz(v(2),3) + ( node_outer(v(3))%z - node_outer(v(2))%z )

    dxyz(v(3),3) = dxyz(v(3),3) + ( node_outer(v(1))%z - node_outer(v(3))%z )
    dxyz(v(3),3) = dxyz(v(3),3) + ( node_outer(v(2))%z - node_outer(v(3))%z )

  end do

! Make changes to each node except the boundary nodes.
  dxyz_norm = -1000000.0_dp
  do i=1, nnodes_body

   if     ( abs( node_outer(i)%x - x2       ) < 1.0e-14 ) then
   elseif ( abs( node_outer(i)%x + distance ) < 1.0e-14 ) then
   else

     node_outer(i)%x = node_outer(i)%x  +  smoothing_factor * dxyz(i,1)

    if ( node_outer(i)%x < x1 ) then
     node_outer(i)%y = node_outer(i)%y  +  smoothing_factor * dxyz(i,2)
     node_outer(i)%z = node_outer(i)%z  +  smoothing_factor * dxyz(i,3)

     nx = node_outer(i)%x -   x1
     ny = node_outer(i)%y - zero
     nz = node_outer(i)%z - zero
     sf = sqrt(nx*nx + ny*ny + nz*nz)
     node_outer(i)%x = (distance) * nx/sf
     node_outer(i)%y = (distance) * ny/sf
     node_outer(i)%z = (distance) * nz/sf

    endif

! project it back to the outer surface


!    L_inf norm of changes scaled by the typical mesh spacing, dr_gs.
     dxyz_norm = max( dxyz_norm, abs(dxyz(i,1)**2 + dxyz(i,2)**2 + dxyz(i,3)**2)/dr_gs )

   endif

  end do

! Exit if converged
  if ( dxyz_norm < 1.0e-04) then
   write(*,*) " Smoothing converged at ", i_smoothing
   exit smoothing_outer
  elseif (i_smoothing == max_smoothing) then
   write(*,*) " Smoothing didn't converge... ", "  dxyz_norm = ", dxyz_norm
  endif

 end do smoothing_outer

 deallocate(dxyz)

  do i = 1, nnodes_body
   node(i)%nx2 = node_outer(i)%x - node_body(i)%x
   node(i)%ny2 = node_outer(i)%y - node_body(i)%y
   node(i)%nz2 = node_outer(i)%z - node_body(i)%z
            sf = sqrt(node(i)%nx2**2 + node(i)%ny2**2 + node(i)%nz2**2)
   node(i)%nx2 = node(i)%nx2 / sf
   node(i)%ny2 = node(i)%ny2 / sf
   node(i)%nz2 = node(i)%nz2 / sf
  end do

!*******************************************************************************
! Write a Tecplot file for the hemisphere cylinder surface triangulation.
!******************************************************************************
 debug_mode_05 : if (debug_mode) then
 open(unit=5, file=filename_outer, status="unknown", iostat=os)

  write(5,*) 'TITLE = "GRID"'
  write(5,*) 'VARIABLES = "x","y","z"'
  write(5,*) 'ZONE  N=', nnodes_body,',E=', ntrias,' , ET=triangle, F=FEPOINT'

! Nodes
  do i = 1, nnodes_body
    write(5,'(3ES20.10)') node_outer(i)%x, node_outer(i)%y, node_outer(i)%z
  end do

! Triangles
  do i = 1, ntrias
   write(5,'(3I10)') tria(i)%v(1), tria(i)%v(2), tria(i)%v(3)
  end do

 close(5)

 write(*,*) "Tecplot file has been written: ", filename_outer
 endif debug_mode_05
!*******************************************************************************

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Define the second direction.
!
! NOTE: Generating nodes from the surface in the surface normal direction
!       creates high-aspect ration cells towards the outer boundary above
!       the cylinder part. We wish to avoid it. To avoid it, we deflect the
!       direction along which interior nodes are generated in Step 5, by
!       introducing the second direction and gradually change the direction
!       from the surface normal to the second direction.
!
! NOTE: The second direction is defined as the direction that points from
!       the center of the cylinder section, (x2,0,0), to each boundary node.
!

! The center

  xc = x2
  yc = zero
  zc = zero

  do i = 1, nnodes_body

   node(i)%nx2 = node(i)%x - xc
   node(i)%ny2 = node(i)%y - yc
   node(i)%nz2 = node(i)%z - zc

!   A little adjustment for better smoothness
    node(i)%nx2 = (node(i)%x - xc) * 0.8_dp

   sf = sqrt(node(i)%nx2**2 + node(i)%ny2**2 + node(i)%nz2**2)
   node(i)%nx2 = node(i)%nx2 / sf
   node(i)%ny2 = node(i)%ny2 / sf
   node(i)%nz2 = node(i)%nz2 / sf

  end do

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! We now go up and generate interior nodes

  nnodes = nnodes_body
  node_on_body : do i = 1, nnodes_body

!  Second node
   nnodes = nnodes + 1
   node(nnodes)%x = vspacing(1)*node(i)%nx + node(i)%x
   node(nnodes)%y = vspacing(1)*node(i)%ny + node(i)%y
   node(nnodes)%z = vspacing(1)*node(i)%nz + node(i)%z
   xp = node(nnodes)%x
   yp = node(nnodes)%y
   zp = node(nnodes)%z
   node_above(i) = nnodes

   k1(nnodes)  = k1_body(i)
   k2(nnodes)  = k2_body(i)
   k3(nnodes)  = k3_body(i)
   k4(nnodes)  = k4_body(i)
   k5(nnodes)  = 1

!  Third node to nm-th node
!  Nodes in the prismatic layer along the surface normal. 
   do k = 3, nm

    nnodes = nnodes + 1
    node(nnodes)%x = vspacing(k-1)*node(i)%nx + xp
    node(nnodes)%y = vspacing(k-1)*node(i)%ny + yp
    node(nnodes)%z = vspacing(k-1)*node(i)%nz + zp
    node_above(nnodes-1) = nnodes

    xp = node(nnodes)%x
    yp = node(nnodes)%y
    zp = node(nnodes)%z

    k1(nnodes)  = k1_body(i)
    k2(nnodes)  = k2_body(i)
    k3(nnodes)  = k3_body(i)
    k4(nnodes)  = k4_body(i)
    k5(nnodes)  = k-1

   end do

!  Outer region
!  Gradually deflected towards the second direction
!  to avoid high-aspect ratio cells near the outer boundary
!  above the cylinder.
   do k = nm+1, nr

!   Stretching factor
    sf = 3.0_dp

!   Uniform spacing function: e.g., xi = [0, 0.1, 0.2, 0.3, ..., 0.8, 0.9, 1]
    xi = real( k - (nm+1) , dp ) / real( nr - (nm+1) , dp)

!   Apply the stretching to the uniform spacing:
!   e.g., blending = [0, 0.1, 0.4, 0.8, 0.85, 0.9, 0.95, 1],
!   so that the direction quickly switches to the second direction.
!   Second term is to pull back to the original direction near the outer boundary.
!   (second term = [0,0,0,0,0,....,0.1,0.3,,0.5,0.8.1.0]
    sf2 = 4.0_dp
    blending = (one-exp(-sf*xi))/(one-exp(-sf))  - (one-exp(sf2*xi))/(one-exp(sf2))

!   This factor is to generate some space in the region near the hemisphere
!   and cylinder junction.
      sf3 = 3.0_dp
    xicyl = (node(i)%x-x1)/(x2-x1) !Normalized distance, xicyl=[0,1]

!   Apply the stretching sf3 only in the cylinder part.
    if (xicyl > zero) then
     blending = blending * ( one - (one-exp(-sf3*xicyl))/(one-exp(-sf3)) )
!     blending = blending * 1.4_dp * (one - sin(xicyl))
    else
!     blending = blending * 1.4_dp
    endif

!   Define the blended direction.
    dirx = (one-blending)*node(i)%nx + blending*node(i)%nx2
    diry = (one-blending)*node(i)%ny + blending*node(i)%ny2
    dirz = (one-blending)*node(i)%nz + blending*node(i)%nz2

!    dirx = node(i)%nx
!    diry = node(i)%ny
!    dirz = node(i)%nz

!   Go up in the blended direction and generate a new interior node.
    nnodes = nnodes + 1
    node(nnodes)%x = vspacing(k-1)*dirx + xp
    node(nnodes)%y = vspacing(k-1)*diry + yp
    node(nnodes)%z = vspacing(k-1)*dirz + zp
    node_above(nnodes-1) = nnodes

    xp = node(nnodes)%x
    yp = node(nnodes)%y
    zp = node(nnodes)%z

    k1(nnodes)  = k1_body(i)
    k2(nnodes)  = k2_body(i)
    k3(nnodes)  = k3_body(i)
    k4(nnodes)  = k4_body(i)
    k5(nnodes)  = k-1

   end do

  end do node_on_body

 deallocate(k1_body,k2_body,k3_body,k4_body)

! At this point, all nodes have been generated.
! It is now a matter of how to connect them (i.e., type of elements).

!*******************************************************************************
! 6. Write a map file: boundary marks
!
! There are three boundary parts (full-geometry case):
! 1. Hemishpere cylinder body
! 2. Outflow plane
! 3. Outer boundary
!
! NOTE: The boundary condition numbers (e.g., 4000) are specific to a solver.
!       Appropriate number needs to be assigned for your solver.
!
!*******************************************************************************

  write(*,*) "***********************************************************"
  write(*,*) " 6. Write a boudnary info file"
  write(*,*) "***********************************************************"

  open(unit=16, file=filename_mapbc, status="unknown", iostat=os)

   write(16,*) "3       !Number of boundary parts (boundary conditions)"
   write(16,*) "1, 4000 !Viscous wall in FUN3D"
   write(16,*) "2, 7012 !Outflow with specified pressure (taken as freestream pressure) in FUN3D"
   write(16,*) "3, 5000 !Characteristic-based inflow/outflow in FUN3D"

  close(16)

  write(*,*) " Boundary info file written:", filename_mapbc

!*******************************************************************************
! 7. Generate elements
!
!    Different grids will be generated by connecting existing nodes.
!
!    igrid_type = Choice of element type:
!
!      1 = Prismatic grid
!      2 = Tetrahedral grid
!      3 = Mixed grid (Prism/Tet)
!      4 = Mixed grid (Prism/Hex)
!
!*******************************************************************************
!*******************************************************************************
!* (1). Generate a prismatic grid
!*******************************************************************************
 if (igrid_type == 1) then

  write(*,*) "***********************************************************"
  write(*,*) " 7. Generate a prismatic grid"
  write(*,*) "***********************************************************"

 call prismatic_grid

!*******************************************************************************
!* (2). Generate a tet grid
!*******************************************************************************
 elseif ( igrid_type == 2 ) then

  write(*,*) "***********************************************************"
  write(*,*) " 7. Generate a tetrahedral grid"
  write(*,*) "***********************************************************"

  call tet_grid

!*******************************************************************************
!* (3). Generate a mixed grid  (Prism/Tet)
!*******************************************************************************
 elseif ( igrid_type == 3 ) then

  write(*,*) "***********************************************************"
  write(*,*) " 7. Generate a mixed grid"
  write(*,*) "***********************************************************"

 call mixed_grid

 write(*,*) " Mixed has been generated."

!*******************************************************************************
!* (4). Generate a mixed grid (Prism/Hex)
!*******************************************************************************
 elseif ( igrid_type == 4 ) then

  write(*,*) "***********************************************************"
  write(*,*) " 7. Generate a mixed grid: Prism/Hex"
  write(*,*) "***********************************************************"

 call mixed_ph_grid

 write(*,*) " Mixed has been generated."

!*******************************************************************************
!* (4). Error
!*******************************************************************************
 else

  write(*,*) "Invalid input: igrid_type = ", igrid_type
  write(*,*) "               igrid_type must be 1, 2, or 3. Try again."
  stop

 endif

!*******************************************************************************
! 9. Generate two line files:
!
!    (1) hemisphere.lines_fmt     - Lines are within the viscous layer.
!    (2) hemisphere.lines_fmt_all - Lines go up to the outer boundary.
!
!*******************************************************************************

  write(*,*) "***********************************************************"
  write(*,*) " 9. Generate lin-info files"
  write(*,*) "***********************************************************"

  if( generate_line_file ) then

! Lines within the viscous layer
  if (line_extent==1) then

  write(*,*)
  write(*,*) "Writing a regular line-info file...."
  call line_info_file(nm)

! Lines go all the way up to the outer boundary (all nodes are in lines)
  elseif (line_extent==2) then

  write(*,*)
  write(*,*) "Writing an all-node line-info file...."
  call line_info_file_all(nr)

  else

  write(*,*) " Invalid input for line_extent. Must be 1 or 2..."
  write(*,*) " Try again. Stop."
  stop

  endif
  else
    write(*,*) "Skipping write of line-info files"
  endif

!*******************************************************************************
! 10. Write a ugrid file.
!*******************************************************************************

  write(*,*) "***********************************************************"
  write(*,*) " 10. Generate a k file"
  write(*,*) "***********************************************************"

  if (generate_k_file) then
    write(*,*) "Writing a k file...."
    call write_k_file
    write(*,*) "K file written : ", filename_k
  else
    write(*,*) "Skipping write of K file : ", filename_k
  endif

!*******************************************************************************
! 11. Write a ugrid file.
!*******************************************************************************

  write(*,*) "***********************************************************"
  write(*,*) " 11. Generate a .ugrid file for running a flow solver"
  write(*,*) "***********************************************************"

  write(*,*) "Writing a ugrid file...."
  call write_ugrid_file
  write(*,*) "UGRID file written : ", filename_ugrid

!*******************************************************************************
! 12. Write a Tecplot file for boundaries.
!*******************************************************************************

  write(*,*) "***********************************************************"
  write(*,*) " 12. Generate a tecplot file for viewing boundaries"
  write(*,*) "***********************************************************"

  if (debug_mode .or. generate_tec_files) then

   write(*,*) "Writing a Tecplot file for boundaries...."
   call write_tecplot_boundary_file
   write(*,*) "Tecplot file for boundaries written : ", filename_tecplot_b

  else

   write(*,*) "Skipping write of Teplot boundary file : ", filename_tecplot_b

  endif

!*******************************************************************************
! 13. Write a Tecplot file for the volume grid.
!*******************************************************************************

  if (generate_tec_files) then

   write(*,*) "***********************************************************"
   write(*,*) " 13. Generate a tecplot file for viewing a volume grid"
   write(*,*) "***********************************************************"

   write(*,*) "Writing a Tecplot file for volumes...."
   call write_tecplot_volume_file
   write(*,*) "Tecplot file for volume grid written : ", filename_tecplot_v

  else
   write(*,*) "Skipping write of Teplot volume file : ", filename_tecplot_v
  endif

 write(*,*)
 write(*,*) "Congratulations!"
 write(*,*) "Grid generation successfully completed."

 stop


contains 

!*******************************************************************************
!* Prismatic Grid Generation
!*******************************************************************************
 subroutine prismatic_grid
 implicit none
!*******************************************************************************
! Generate prismatic grid
!*******************************************************************************
  ntrias_b = ntrias + ntrias        !Inner and outer boundaries
  allocate(tri(ntrias_b,5))

  if (n_sections==6) then
   nquads_b = nnodes_circum * (nr-1) !Outflow boundary
   allocate(quad(nquads_b,5))
  elseif (n_sections==1) then
   nquads_b = nnodes_circum * (nr-1) + 2*(nr_gs+nnodes_cylinder)*(nr-1)
   allocate(quad(nquads_b,5))
  endif

  nprs = ntrias*(nr-1)
  allocate(prs(nprs,6) )
  ntet = 0

  write(*,*) "1. Prismatic grid "
  write(*,*) "    nodes      = ", nnodes
  write(*,*) "  Volume elements:"
  write(*,*) "    prisms     = ", nprs
  write(*,*) "    tetrahedra = ", ntet
  write(*,*) "  Boundary elements:"
  write(*,*) "     triangles = ", ntrias_b
  write(*,*) "         quads = ", nquads_b

  nprs = 0
  ntrias_b = 0
  nquads_b = 0

! Copy the triangulation on the body

  do i = 1, ntrias
          ntrias_b = ntrias_b + 1
   tri(ntrias_b,5) = 1
   tri(ntrias_b,1) = tria(i)%v(1)
   tri(ntrias_b,2) = tria(i)%v(2)
   tri(ntrias_b,3) = tria(i)%v(3)
  end do

! Generate prisms

  do i = 1, ntrias

   node1 = tria(i)%v(1)
   node2 = tria(i)%v(2)
   node3 = tria(i)%v(3)
   node4 = node_above(tria(i)%v(1))
   node5 = node_above(tria(i)%v(2))
   node6 = node_above(tria(i)%v(3))

     nprs = nprs + 1

     prs(nprs,1) = node1
     prs(nprs,2) = node2
     prs(nprs,3) = node3
     prs(nprs,4) = node4
     prs(nprs,5) = node5
     prs(nprs,6) = node6

   do k = 2, nr-1

    node1 = node4
    node2 = node5
    node3 = node6
    node4 = node_above(node1)
    node5 = node_above(node2)
    node6 = node_above(node3)

    nprs = nprs + 1
    prs(nprs,1) = node1
    prs(nprs,2) = node2
    prs(nprs,3) = node3
    prs(nprs,4) = node4
    prs(nprs,5) = node5
    prs(nprs,6) = node6


   end do

     ntrias_b = ntrias_b + 1
     tri(ntrias_b,5) = 3
     tri(ntrias_b,1) = node6
     tri(ntrias_b,2) = node5
     tri(ntrias_b,3) = node4

  end do

! Outflow boundary
   nquads_b = 0

  if     (n_sections==6) then
   n_temp = nnodes_circum
  elseif (n_sections==1) then
   n_temp = nnodes_circum - 1
  endif

   do i = 1, n_temp

    node1 = nodes_cylinder(i  , nnodes_cylinder+1)
    node2 = nodes_cylinder(i+1, nnodes_cylinder+1)
    node3 = node_above(node1)
    node4 = node_above(node2)

    nquads_b = nquads_b + 1
    quad(nquads_b,5) = 2
    quad(nquads_b,1) = node1
    quad(nquads_b,2) = node2
    quad(nquads_b,3) = node4
    quad(nquads_b,4) = node3

    do k = 2, nr-1

     node1 = node3
     node2 = node4
     node3 = node_above(node1)
     node4 = node_above(node2)

     nquads_b = nquads_b + 1
     quad(nquads_b,5) = 2
     quad(nquads_b,1) = node1
     quad(nquads_b,2) = node2
     quad(nquads_b,3) = node4
     quad(nquads_b,4) = node3

    end do

   end do

 section1 : if (n_sections==1) then

 ! Symmetry boundary 1

   do i = 1, nr_gs

    node1 = (i-1)*i/2 + 1 !Left-most node of the original sector
    node2 = (i+1)*i/2 + 1 !Left-most node of the original sector
    node3 = node_above(node1)
    node4 = node_above(node2)

    nquads_b = nquads_b + 1
    quad(nquads_b,5) = 4
    quad(nquads_b,1) = node1
    quad(nquads_b,2) = node2
    quad(nquads_b,3) = node4
    quad(nquads_b,4) = node3

    do k = 2, nr-1

     node1 = node3
     node2 = node4
     node3 = node_above(node1)
     node4 = node_above(node2)

     nquads_b = nquads_b + 1
     quad(nquads_b,5) = 4
     quad(nquads_b,1) = node1
     quad(nquads_b,2) = node2
     quad(nquads_b,3) = node4
     quad(nquads_b,4) = node3

    end do

   end do


   do i = 1, nnodes_cylinder

    node1 = nodes_cylinder(1, i  ) !Left-most node of the original sector
    node2 = nodes_cylinder(1, i+1) !Left-most node of the original sector
    node3 = node_above(node1)
    node4 = node_above(node2)

    nquads_b = nquads_b + 1
    quad(nquads_b,5) = 4
    quad(nquads_b,1) = node1
    quad(nquads_b,2) = node2
    quad(nquads_b,3) = node4
    quad(nquads_b,4) = node3

    do k = 2, nr-1

     node1 = node3
     node2 = node4
     node3 = node_above(node1)
     node4 = node_above(node2)

     nquads_b = nquads_b + 1
     quad(nquads_b,5) = 4
     quad(nquads_b,1) = node1
     quad(nquads_b,2) = node2
     quad(nquads_b,3) = node4
     quad(nquads_b,4) = node3

    end do

   end do

 ! Symmetry boundary 2

   do i = 1, nr_gs

    node1 = (i+1)*i/2 + i+1 !Right-most node of the original sector
    node2 = (i-1)*i/2 + i   !Right-most node of the original sector
    node3 = node_above(node1)
    node4 = node_above(node2)

    nquads_b = nquads_b + 1
    quad(nquads_b,5) = 5
    quad(nquads_b,1) = node1
    quad(nquads_b,2) = node2
    quad(nquads_b,3) = node4
    quad(nquads_b,4) = node3

    do k = 2, nr-1

     node1 = node3
     node2 = node4
     node3 = node_above(node1)
     node4 = node_above(node2)

     nquads_b = nquads_b + 1
     quad(nquads_b,5) = 5
     quad(nquads_b,1) = node1
     quad(nquads_b,2) = node2
     quad(nquads_b,3) = node4
     quad(nquads_b,4) = node3

    end do

   end do

   do i = 1, nnodes_cylinder

    node1 = nodes_cylinder(nnodes_circum, i+1)
    node2 = nodes_cylinder(nnodes_circum, i  )
    node3 = node_above(node1)
    node4 = node_above(node2)

    nquads_b = nquads_b + 1
    quad(nquads_b,5) = 5
    quad(nquads_b,1) = node1
    quad(nquads_b,2) = node2
    quad(nquads_b,3) = node4
    quad(nquads_b,4) = node3

    do k = 2, nr-1

     node1 = node3
     node2 = node4
     node3 = node_above(node1)
     node4 = node_above(node2)

     nquads_b = nquads_b + 1
     quad(nquads_b,5) = 5
     quad(nquads_b,1) = node1
     quad(nquads_b,2) = node2
     quad(nquads_b,3) = node4
     quad(nquads_b,4) = node3

    end do

   end do

 endif section1

 end subroutine prismatic_grid


!*******************************************************************************
!* Tet grid Generation
!
!*******************************************************************************
 subroutine tet_grid
 implicit none
 integer :: tria_type
!*******************************************************************************
! Generate tet grid
!*******************************************************************************

  if (n_sections==6) then
   ntrias_b = ntrias + ntrias + 2*nnodes_circum * (nr-1) !Inner and outer boundaries
   allocate(tri(ntrias_b,5))
  elseif (n_sections==1) then
   ntrias_b = ntrias + ntrias + 2*nnodes_circum * (nr-1) !Inner and outer boundaries
   ntrias_b = ntrias_b + 2*2*(nr_gs+nnodes_cylinder)*(nr-1)
   allocate(tri(ntrias_b,5))
  endif

!  ntrias_b = ntrias + ntrias + 2*nnodes_circum * (nr-1) !Inner and outer boundaries
!  allocate(tri(ntrias_b,5))

  nquads_b = 0
  nprs = 0
  ntet = ntrias*(nr-1)*3
  allocate(tet(ntet,6) )

  write(*,*) "2. Tetrahedral grid "
  write(*,*) "    nodes      = ", nnodes
  write(*,*) "  Volume elements:"
  write(*,*) "    prisms     = ", nprs
  write(*,*) "    tetrahedra = ", ntet
  write(*,*) "  Boundary elements:"
  write(*,*) "     triangles = ", ntrias_b
  write(*,*) "         quads = ", nquads_b

  nprs = 0
  ntet = 0
  ntrias_b = 0
  nquads_b = 0

! Copy the triangulation on the body: Reverse the orientation.

  do i = 1, ntrias
          ntrias_b = ntrias_b + 1
   tri(ntrias_b,5) = 1
   tri(ntrias_b,1) = tria(i)%v(1)
   tri(ntrias_b,2) = tria(i)%v(2)
   tri(ntrias_b,3) = tria(i)%v(3)
  end do

! Generate prisms

  do i = 1, ntrias

   do k = 1, nr-1

    if (k == 1) then

     node1 = tria(i)%v(1)
     node2 = tria(i)%v(2)
     node3 = tria(i)%v(3)
     node4 = node_above(tria(i)%v(1))
     node5 = node_above(tria(i)%v(2))
     node6 = node_above(tria(i)%v(3))

     tria_type = tria(i)%type

    else

     node1 = node4
     node2 = node5
     node3 = node6
     node4 = node_above(node1)
     node5 = node_above(node2)
     node6 = node_above(node3)

    endif

     t_type :if (tria_type == 1) then

      ntet = ntet + 1
      tet(ntet,1) = node1
      tet(ntet,2) = node2
      tet(ntet,3) = node3
      tet(ntet,4) = node4

      ntet = ntet + 1
      tet(ntet,1) = node5
      tet(ntet,2) = node6
      tet(ntet,3) = node3
      tet(ntet,4) = node4

      ntet = ntet + 1
      tet(ntet,1) = node5
      tet(ntet,2) = node3
      tet(ntet,3) = node2
      tet(ntet,4) = node4

     elseif (tria_type == 2) then

      ntet = ntet + 1
      tet(ntet,1) = node4
      tet(ntet,2) = node6
      tet(ntet,3) = node5
      tet(ntet,4) = node1

      ntet = ntet + 1
      tet(ntet,1) = node5
      tet(ntet,2) = node6
      tet(ntet,3) = node2
      tet(ntet,4) = node1

      ntet = ntet + 1
      tet(ntet,1) = node6
      tet(ntet,2) = node3
      tet(ntet,3) = node2
      tet(ntet,4) = node1

     elseif (tria_type == 20) then

      ntet = ntet + 1
      tet(ntet,1) = node4
      tet(ntet,2) = node6
      tet(ntet,3) = node5
      tet(ntet,4) = node1

      ntet = ntet + 1
      tet(ntet,1) = node5
      tet(ntet,2) = node6
      tet(ntet,3) = node3
      tet(ntet,4) = node1

      ntet = ntet + 1
      tet(ntet,1) = node5
      tet(ntet,2) = node3
      tet(ntet,3) = node2
      tet(ntet,4) = node1

     elseif (tria_type == 3) then

      ntet = ntet + 1
      tet(ntet,1) = node1
      tet(ntet,2) = node2
      tet(ntet,3) = node3
      tet(ntet,4) = node5

      ntet = ntet + 1
      tet(ntet,1) = node6
      tet(ntet,2) = node4
      tet(ntet,3) = node3
      tet(ntet,4) = node5

      ntet = ntet + 1
      tet(ntet,1) = node4
      tet(ntet,2) = node1
      tet(ntet,3) = node3
      tet(ntet,4) = node5

     elseif (tria_type == 4) then

      ntet = ntet + 1
      tet(ntet,1) = node1
      tet(ntet,2) = node2
      tet(ntet,3) = node3
      tet(ntet,4) = node6

      ntet = ntet + 1
      tet(ntet,1) = node4
      tet(ntet,2) = node5
      tet(ntet,3) = node2
      tet(ntet,4) = node6

      ntet = ntet + 1
      tet(ntet,1) = node4
      tet(ntet,2) = node2
      tet(ntet,3) = node1
      tet(ntet,4) = node6

     endif t_type

   end do

     ntrias_b = ntrias_b + 1
     tri(ntrias_b,5) = 3
     tri(ntrias_b,1) = node6
     tri(ntrias_b,2) = node5
     tri(ntrias_b,3) = node4

  end do

! Outflow boundary
   nquads_b = 0

  if     (n_sections==6) then
   n_temp = nnodes_circum
  elseif (n_sections==1) then
   n_temp = nnodes_circum - 1
  endif

   do i = 1, n_temp

    do k = 1, nr-1

     if (k == 1) then

      node1 = nodes_cylinder(i  , nnodes_cylinder+1)
      node2 = nodes_cylinder(i+1, nnodes_cylinder+1)
      node3 = node_above(node1)
      node4 = node_above(node2)

     else

      node1 = node3
      node2 = node4
      node3 = node_above(node1)
      node4 = node_above(node2)

     endif

      ntrias_b = ntrias_b + 1
      tri(ntrias_b,5) = 2
      tri(ntrias_b,1) = node1
      tri(ntrias_b,2) = node2
      tri(ntrias_b,3) = node4

      ntrias_b = ntrias_b + 1
      tri(ntrias_b,5) = 2
      tri(ntrias_b,1) = node1
      tri(ntrias_b,2) = node4
      tri(ntrias_b,3) = node3

    end do

   end do

  section1 : if (n_sections==1) then

 ! Symmetry boundary 1

   do i = 1, nr_gs

    node1 = (i-1)*i/2 + 1
    node2 = (i+1)*i/2 + 1
    node3 = node_above(node1)
    node4 = node_above(node2)

      ntrias_b = ntrias_b + 1
      tri(ntrias_b,5) = 4
      tri(ntrias_b,1) = node1
      tri(ntrias_b,2) = node2
      tri(ntrias_b,3) = node4

      ntrias_b = ntrias_b + 1
      tri(ntrias_b,5) = 4
      tri(ntrias_b,1) = node1
      tri(ntrias_b,2) = node4
      tri(ntrias_b,3) = node3

    do k = 2, nr-1

     node1 = node3
     node2 = node4
     node3 = node_above(node1)
     node4 = node_above(node2)

      ntrias_b = ntrias_b + 1
      tri(ntrias_b,5) = 4
      tri(ntrias_b,1) = node1
      tri(ntrias_b,2) = node2
      tri(ntrias_b,3) = node4

      ntrias_b = ntrias_b + 1
      tri(ntrias_b,5) = 4
      tri(ntrias_b,1) = node1
      tri(ntrias_b,2) = node4
      tri(ntrias_b,3) = node3

    end do

   end do

   do i = 1, nnodes_cylinder

    node1 = nodes_cylinder(1, i  ) !Left-most node of the original sector
    node2 = nodes_cylinder(1, i+1) !Left-most node of the original sector
    node3 = node_above(node1)
    node4 = node_above(node2)

      ntrias_b = ntrias_b + 1
      tri(ntrias_b,5) = 4
      tri(ntrias_b,1) = node1
      tri(ntrias_b,2) = node2
      tri(ntrias_b,3) = node4

      ntrias_b = ntrias_b + 1
      tri(ntrias_b,5) = 4
      tri(ntrias_b,1) = node1
      tri(ntrias_b,2) = node4
      tri(ntrias_b,3) = node3

    do k = 2, nr-1

     node1 = node3
     node2 = node4
     node3 = node_above(node1)
     node4 = node_above(node2)

      ntrias_b = ntrias_b + 1
      tri(ntrias_b,5) = 4
      tri(ntrias_b,1) = node1
      tri(ntrias_b,2) = node2
      tri(ntrias_b,3) = node4

      ntrias_b = ntrias_b + 1
      tri(ntrias_b,5) = 4
      tri(ntrias_b,1) = node1
      tri(ntrias_b,2) = node4
      tri(ntrias_b,3) = node3

    end do

   end do
   
 ! Symmetry boundary 2

   do i = 1, nr_gs

    node1 = (i+1)*i/2 + i+1 !Right-most node of the original sector
    node2 = (i-1)*i/2 + i   !Right-most node of the original sector
    node3 = node_above(node1)
    node4 = node_above(node2)

      ntrias_b = ntrias_b + 1
      tri(ntrias_b,5) = 5
      tri(ntrias_b,1) = node1
      tri(ntrias_b,2) = node2
      tri(ntrias_b,3) = node4

      ntrias_b = ntrias_b + 1
      tri(ntrias_b,5) = 5
      tri(ntrias_b,1) = node1
      tri(ntrias_b,2) = node4
      tri(ntrias_b,3) = node3

    do k = 2, nr-1

     node1 = node3
     node2 = node4
     node3 = node_above(node1)
     node4 = node_above(node2)

      ntrias_b = ntrias_b + 1
      tri(ntrias_b,5) = 5
      tri(ntrias_b,1) = node1
      tri(ntrias_b,2) = node2
      tri(ntrias_b,3) = node4

      ntrias_b = ntrias_b + 1
      tri(ntrias_b,5) = 5
      tri(ntrias_b,1) = node1
      tri(ntrias_b,2) = node4
      tri(ntrias_b,3) = node3

    end do

   end do

   do i = 1, nnodes_cylinder

    node1 = nodes_cylinder(nnodes_circum, i+1) !Left-most node of the original sector
    node2 = nodes_cylinder(nnodes_circum, i  ) !Left-most node of the original sector
    node3 = node_above(node1)
    node4 = node_above(node2)

      ntrias_b = ntrias_b + 1
      tri(ntrias_b,5) = 5
      tri(ntrias_b,1) = node1
      tri(ntrias_b,2) = node2
      tri(ntrias_b,3) = node4

      ntrias_b = ntrias_b + 1
      tri(ntrias_b,5) = 5
      tri(ntrias_b,1) = node1
      tri(ntrias_b,2) = node4
      tri(ntrias_b,3) = node3

    do k = 2, nr-1

     node1 = node3
     node2 = node4
     node3 = node_above(node1)
     node4 = node_above(node2)

      ntrias_b = ntrias_b + 1
      tri(ntrias_b,5) = 5
      tri(ntrias_b,1) = node1
      tri(ntrias_b,2) = node2
      tri(ntrias_b,3) = node4

      ntrias_b = ntrias_b + 1
      tri(ntrias_b,5) = 5
      tri(ntrias_b,1) = node1
      tri(ntrias_b,2) = node4
      tri(ntrias_b,3) = node3

    end do

   end do

  endif section1

 end subroutine tet_grid


!*******************************************************************************
!* Mixed grid Generation
!
!*******************************************************************************
 subroutine mixed_grid
 implicit none
 integer :: tria_type
!*******************************************************************************
! Generate mixed grid
!*******************************************************************************

  ntrias_b = 2*ntrias + 2*nnodes_circum * (nr-1)-(nm-1) !All boundaries
!  allocate(tri(ntrias_b,5))
  nquads_b = nnodes_circum * (nm-1) !A part of outflow boundary
!  allocate(quad(nquads_b,5))

  nprs = ntrias*(nm-1)              !Boundary layer
  allocate(prs(nprs,6) )
  ntet = ntrias*((nr-1)-(nm-1))*3   !Outer region
  allocate(tet(ntet,6) )

  if (n_sections==1) then
   ntrias_b = ntrias_b + 2*2*(nr_gs+nnodes_cylinder)*(nr-1)
   nquads_b = nquads_b +   2*(nr_gs+nnodes_cylinder)*(nr-1)
  endif

  allocate(tri(ntrias_b,5))
  allocate(quad(nquads_b,5))

  write(*,*) "3. Mixed grid "
  write(*,*) "    nodes      = ", nnodes
  write(*,*) "  Volume elements:"
  write(*,*) "    prisms     = ", nprs
  write(*,*) "    tetrahedra = ", ntet
  write(*,*) "  Boundary elements:"
  write(*,*) "     triangles = ", ntrias_b
  write(*,*) "         quads = ", nquads_b

  nprs = 0
  ntet = 0
  ntrias_b = 0
  nquads_b = 0

! Copy the triangulation on the body

  do i = 1, ntrias
          ntrias_b = ntrias_b + 1
   tri(ntrias_b,5) = 1
   tri(ntrias_b,1) = tria(i)%v(1)
   tri(ntrias_b,2) = tria(i)%v(2)
   tri(ntrias_b,3) = tria(i)%v(3)
  end do

! Generate prisms

  do i = 1, ntrias

   node1 = tria(i)%v(1)
   node2 = tria(i)%v(2)
   node3 = tria(i)%v(3)
   node4 = node_above(tria(i)%v(1))
   node5 = node_above(tria(i)%v(2))
   node6 = node_above(tria(i)%v(3))
   tria_type = tria(i)%type

     nprs = nprs + 1

     prs(nprs,1) = node1
     prs(nprs,2) = node2
     prs(nprs,3) = node3
     prs(nprs,4) = node4
     prs(nprs,5) = node5
     prs(nprs,6) = node6

   do k = 2, nr-1

    node1 = node4
    node2 = node5
    node3 = node6
    node4 = node_above(node1)
    node5 = node_above(node2)
    node6 = node_above(node3)

    prs_or_tet : if (k < nm) then

     nprs = nprs + 1
     prs(nprs,1) = node1
     prs(nprs,2) = node2
     prs(nprs,3) = node3
     prs(nprs,4) = node4
     prs(nprs,5) = node5
     prs(nprs,6) = node6

    else

     t_type :if (tria_type == 1) then

      ntet = ntet + 1
      tet(ntet,1) = node1
      tet(ntet,2) = node2
      tet(ntet,3) = node3
      tet(ntet,4) = node4

      ntet = ntet + 1
      tet(ntet,1) = node5
      tet(ntet,2) = node6
      tet(ntet,3) = node3
      tet(ntet,4) = node4

      ntet = ntet + 1
      tet(ntet,1) = node5
      tet(ntet,2) = node3
      tet(ntet,3) = node2
      tet(ntet,4) = node4

     elseif (tria_type == 2) then

      ntet = ntet + 1
      tet(ntet,1) = node4
      tet(ntet,2) = node6
      tet(ntet,3) = node5
      tet(ntet,4) = node1

      ntet = ntet + 1
      tet(ntet,1) = node5
      tet(ntet,2) = node6
      tet(ntet,3) = node2
      tet(ntet,4) = node1

      ntet = ntet + 1
      tet(ntet,1) = node6
      tet(ntet,2) = node3
      tet(ntet,3) = node2
      tet(ntet,4) = node1

     elseif (tria_type == 20) then

      ntet = ntet + 1
      tet(ntet,1) = node4
      tet(ntet,2) = node6
      tet(ntet,3) = node5
      tet(ntet,4) = node1

      ntet = ntet + 1
      tet(ntet,1) = node5
      tet(ntet,2) = node6
      tet(ntet,3) = node3
      tet(ntet,4) = node1

      ntet = ntet + 1
      tet(ntet,1) = node5
      tet(ntet,2) = node3
      tet(ntet,3) = node2
      tet(ntet,4) = node1

     elseif (tria_type == 3) then

      ntet = ntet + 1
      tet(ntet,1) = node1
      tet(ntet,2) = node2
      tet(ntet,3) = node3
      tet(ntet,4) = node5

      ntet = ntet + 1
      tet(ntet,1) = node6
      tet(ntet,2) = node4
      tet(ntet,3) = node3
      tet(ntet,4) = node5

      ntet = ntet + 1
      tet(ntet,1) = node4
      tet(ntet,2) = node1
      tet(ntet,3) = node3
      tet(ntet,4) = node5

     elseif (tria_type == 4) then

      ntet = ntet + 1
      tet(ntet,1) = node6
      tet(ntet,2) = node5
      tet(ntet,3) = node4
      tet(ntet,4) = node2

      ntet = ntet + 1
      tet(ntet,1) = node1
      tet(ntet,2) = node3
      tet(ntet,3) = node6
      tet(ntet,4) = node2

      ntet = ntet + 1
      tet(ntet,1) = node1
      tet(ntet,2) = node6
      tet(ntet,3) = node4
      tet(ntet,4) = node2


     endif t_type

    endif prs_or_tet

   end do

     ntrias_b = ntrias_b + 1
     tri(ntrias_b,5) = 3
     tri(ntrias_b,1) = node6
     tri(ntrias_b,2) = node5
     tri(ntrias_b,3) = node4

  end do

! Outflow boundary
   nquads_b = 0

  if     (n_sections==6) then
   n_temp = nnodes_circum
  elseif (n_sections==1) then
   n_temp = nnodes_circum - 1
  endif

   do i = 1, n_temp

    node1 = nodes_cylinder(i  , nnodes_cylinder+1)
    node2 = nodes_cylinder(i+1, nnodes_cylinder+1)
    node3 = node_above(node1)
    node4 = node_above(node2)

    nquads_b = nquads_b + 1
    quad(nquads_b,5) = 2
    quad(nquads_b,1) = node1
    quad(nquads_b,2) = node2
    quad(nquads_b,3) = node4
    quad(nquads_b,4) = node3

    do k = 2, nr-1

     node1 = node3
     node2 = node4
     node3 = node_above(node1)
     node4 = node_above(node2)

     if (k < nm) then

      nquads_b = nquads_b + 1
      quad(nquads_b,5) = 2
      quad(nquads_b,1) = node1
      quad(nquads_b,2) = node2
      quad(nquads_b,3) = node4
      quad(nquads_b,4) = node3

     else

      ntrias_b = ntrias_b + 1
      tri(ntrias_b,5) = 2
      tri(ntrias_b,1) = node1
      tri(ntrias_b,2) = node2
      tri(ntrias_b,3) = node4

      ntrias_b = ntrias_b + 1
      tri(ntrias_b,5) = 2
      tri(ntrias_b,1) = node1
      tri(ntrias_b,2) = node4
      tri(ntrias_b,3) = node3

     endif

    end do

   end do


  section1 : if (n_sections==1) then

 ! Symmetry boundary 1

   do i = 1, nr_gs

    node1 = (i-1)*i/2 + 1
    node2 = (i+1)*i/2 + 1
    node3 = node_above(node1)
    node4 = node_above(node2)

    nquads_b = nquads_b + 1
    quad(nquads_b,5) = 4
    quad(nquads_b,1) = node1
    quad(nquads_b,2) = node2
    quad(nquads_b,3) = node4
    quad(nquads_b,4) = node3

    do k = 2, nr-1

     node1 = node3
     node2 = node4
     node3 = node_above(node1)
     node4 = node_above(node2)

     if (k < nm) then

     nquads_b = nquads_b + 1
     quad(nquads_b,5) = 4
     quad(nquads_b,1) = node1
     quad(nquads_b,2) = node2
     quad(nquads_b,3) = node4
     quad(nquads_b,4) = node3

     else

      ntrias_b = ntrias_b + 1
      tri(ntrias_b,5) = 4
      tri(ntrias_b,1) = node1
      tri(ntrias_b,2) = node2
      tri(ntrias_b,3) = node4

      ntrias_b = ntrias_b + 1
      tri(ntrias_b,5) = 4
      tri(ntrias_b,1) = node1
      tri(ntrias_b,2) = node4
      tri(ntrias_b,3) = node3

     endif

    end do

   end do


   do i = 1, nnodes_cylinder

    node1 = nodes_cylinder(1, i  ) !Left-most node of the original sector
    node2 = nodes_cylinder(1, i+1) !Left-most node of the original sector
    node3 = node_above(node1)
    node4 = node_above(node2)

    nquads_b = nquads_b + 1
    quad(nquads_b,5) = 4
    quad(nquads_b,1) = node1
    quad(nquads_b,2) = node2
    quad(nquads_b,3) = node4
    quad(nquads_b,4) = node3

    do k = 2, nr-1

     node1 = node3
     node2 = node4
     node3 = node_above(node1)
     node4 = node_above(node2)

     if (k < nm) then

     nquads_b = nquads_b + 1
     quad(nquads_b,5) = 4
     quad(nquads_b,1) = node1
     quad(nquads_b,2) = node2
     quad(nquads_b,3) = node4
     quad(nquads_b,4) = node3

     else

      ntrias_b = ntrias_b + 1
      tri(ntrias_b,5) = 4
      tri(ntrias_b,1) = node1
      tri(ntrias_b,2) = node2
      tri(ntrias_b,3) = node4

      ntrias_b = ntrias_b + 1
      tri(ntrias_b,5) = 4
      tri(ntrias_b,1) = node1
      tri(ntrias_b,2) = node4
      tri(ntrias_b,3) = node3

     endif

    end do

   end do

 ! Symmetry boundary 2

   do i = 1, nr_gs

    node1 = (i+1)*i/2 + i+1 !Right-most node of the original sector
    node2 = (i-1)*i/2 + i   !Right-most node of the original sector
    node3 = node_above(node1)
    node4 = node_above(node2)

    nquads_b = nquads_b + 1
    quad(nquads_b,5) = 4
    quad(nquads_b,1) = node1
    quad(nquads_b,2) = node2
    quad(nquads_b,3) = node4
    quad(nquads_b,4) = node3

    do k = 2, nr-1

     node1 = node3
     node2 = node4
     node3 = node_above(node1)
     node4 = node_above(node2)

     if (k < nm) then

     nquads_b = nquads_b + 1
     quad(nquads_b,5) = 5
     quad(nquads_b,1) = node1
     quad(nquads_b,2) = node2
     quad(nquads_b,3) = node4
     quad(nquads_b,4) = node3

     else

      ntrias_b = ntrias_b + 1
      tri(ntrias_b,5) = 5
      tri(ntrias_b,1) = node1
      tri(ntrias_b,2) = node2
      tri(ntrias_b,3) = node4

      ntrias_b = ntrias_b + 1
      tri(ntrias_b,5) = 5
      tri(ntrias_b,1) = node1
      tri(ntrias_b,2) = node4
      tri(ntrias_b,3) = node3

     endif

    end do

   end do


   do i = 1, nnodes_cylinder

    node1 = nodes_cylinder(nnodes_circum, i+1) !Left-most node of the original sector
    node2 = nodes_cylinder(nnodes_circum, i  ) !Left-most node of the original sector
    node3 = node_above(node1)
    node4 = node_above(node2)

    nquads_b = nquads_b + 1
    quad(nquads_b,5) = 5
    quad(nquads_b,1) = node1
    quad(nquads_b,2) = node2
    quad(nquads_b,3) = node4
    quad(nquads_b,4) = node3

    do k = 2, nr-1

     node1 = node3
     node2 = node4
     node3 = node_above(node1)
     node4 = node_above(node2)

     if (k < nm) then

     nquads_b = nquads_b + 1
     quad(nquads_b,5) = 5
     quad(nquads_b,1) = node1
     quad(nquads_b,2) = node2
     quad(nquads_b,3) = node4
     quad(nquads_b,4) = node3

     else

      ntrias_b = ntrias_b + 1
      tri(ntrias_b,5) = 5
      tri(ntrias_b,1) = node1
      tri(ntrias_b,2) = node2
      tri(ntrias_b,3) = node4

      ntrias_b = ntrias_b + 1
      tri(ntrias_b,5) = 5
      tri(ntrias_b,1) = node1
      tri(ntrias_b,2) = node4
      tri(ntrias_b,3) = node3

     endif

    end do

   end do

  endif section1

  write(*,*) "  Volume elements:"
  write(*,*) "    prisms     = ", nprs
  write(*,*) "    tetrahedra = ", ntet
  write(*,*) "  Boundary elements:"
  write(*,*) "     triangles = ", ntrias_b
  write(*,*) "         quads = ", nquads_b

 end subroutine mixed_grid


!*******************************************************************************
!* Mixed grid Generation: Prism and Hex
!
!*******************************************************************************
 subroutine mixed_ph_grid
 implicit none
 integer :: tria_type, node7, node8
!*******************************************************************************
! Generate mixed grid
!*******************************************************************************
  ntrias_b = 2*ntrias !All boundaries
  allocate(tri(ntrias_b,5))

  if (n_sections==6) then
   nquads_b = nnodes_circum * (nr-1)   ! All over outflow boundary
   nquads_b = nquads_b + 2*nquads_cyl  ! + All over cylinder surface and outer
   allocate(quad(nquads_b,5))
  elseif (n_sections==1) then
   nquads_b = nnodes_circum * (nr-1)   ! All over outflow boundary
   nquads_b = nquads_b + 2*nquads_cyl  ! + All over cylinder surface and outer
   nquads_b = nquads_b + 2*(nr_gs+nnodes_cylinder)*(nr-1)
   allocate(quad(nquads_b,5))
  endif

  nprs = ntrias*(nr-1)              ! All over the hemisphere
  allocate(prs(nprs,6) )
  nhex = nquads_cyl*(nr-1)          ! All over the cylinder part
  allocate(hex(nhex,8))
  ntet = 0
  allocate(tet(ntet,6) )


  write(*,*) "3. Mixed grid "
  write(*,*) "    nodes      = ", nnodes
  write(*,*) "  Volume elements:"
  write(*,*) "    prisms     = ", nprs
  write(*,*) "    tetrahedra = ", ntet
  write(*,*) "    hex        = ", nhex
  write(*,*) "  Boundary elements:"
  write(*,*) "     triangles = ", ntrias_b
  write(*,*) "         quads = ", nquads_b

  nprs = 0
  ntet = 0
  nhex = 0
  ntrias_b = 0
  nquads_b = 0

!------------------------------------------------------------
! Copy the triangulation on the body
!------------------------------------------------------------

  do i = 1, ntrias
          ntrias_b = ntrias_b + 1
   tri(ntrias_b,5) = 1
   tri(ntrias_b,1) = tria(i)%v(1)
   tri(ntrias_b,2) = tria(i)%v(2)
   tri(ntrias_b,3) = tria(i)%v(3)
  end do

! Generate prisms

  do i = 1, ntrias

   node1 = tria(i)%v(1)
   node2 = tria(i)%v(2)
   node3 = tria(i)%v(3)
   node4 = node_above(tria(i)%v(1))
   node5 = node_above(tria(i)%v(2))
   node6 = node_above(tria(i)%v(3))

   tria_type = tria(i)%type

     nprs = nprs + 1

     prs(nprs,1) = node1
     prs(nprs,2) = node2
     prs(nprs,3) = node3
     prs(nprs,4) = node4
     prs(nprs,5) = node5
     prs(nprs,6) = node6

   do k = 2, nr-1

    node1 = node4
    node2 = node5
    node3 = node6
    node4 = node_above(node1)
    node5 = node_above(node2)
    node6 = node_above(node3)

     nprs = nprs + 1
     prs(nprs,1) = node1
     prs(nprs,2) = node2
     prs(nprs,3) = node3
     prs(nprs,4) = node4
     prs(nprs,5) = node5
     prs(nprs,6) = node6

   end do

     ntrias_b = ntrias_b + 1
     tri(ntrias_b,5) = 3
     tri(ntrias_b,1) = node6
     tri(ntrias_b,2) = node5
     tri(ntrias_b,3) = node4

  end do

!------------------------------------------------------------
! Quads on the cylinder part
!------------------------------------------------------------

  nquads_b = 0
  do i = 1, nquads_cyl
           nquads_b = nquads_b + 1
   quad(nquads_b,:) = quad_cyl(i,:)
       nquads_bcyl1 = nquads_bcyl1 + 1
  end do

! Generate Hex

  nquads_b = 0

  do i = 1, nquads_cyl

     node1 = quad_cyl(i,1)
     node2 = quad_cyl(i,2)
     node3 = quad_cyl(i,3)
     node4 = quad_cyl(i,4)

   node5 = node_above(quad(nquads_b,1))
   node6 = node_above(quad(nquads_b,2))
   node7 = node_above(quad(nquads_b,3))
   node8 = node_above(quad(nquads_b,4))

     nhex = nhex + 1
     hex(nhex,1) = node1
     hex(nhex,2) = node2
     hex(nhex,3) = node3
     hex(nhex,4) = node4
     hex(nhex,5) = node5
     hex(nhex,6) = node6
     hex(nhex,7) = node7
     hex(nhex,8) = node8

   do k = 2, nr-1

    node1 = node5
    node2 = node6
    node3 = node7
    node4 = node8
    node5 = node_above(node1)
    node6 = node_above(node2)
    node7 = node_above(node3)
    node8 = node_above(node4)

     nhex = nhex + 1
     hex(nhex,1) = node1
     hex(nhex,2) = node2
     hex(nhex,3) = node3
     hex(nhex,4) = node4
     hex(nhex,5) = node5
     hex(nhex,6) = node6
     hex(nhex,7) = node7
     hex(nhex,8) = node8

   end do

     nquads_b = nquads_b + 1
     quad(nquads_b,5) = 3      ! Quads on the outer boundary
     nquads_bcyl3 = nquads_bcyl3 + 1
     quad(nquads_b,1) = node8
     quad(nquads_b,2) = node7
     quad(nquads_b,3) = node6
     quad(nquads_b,4) = node5

  end do

! Outflow boundary

  if     (n_sections==6) then
   n_temp = nnodes_circum
  elseif (n_sections==1) then
   n_temp = nnodes_circum - 1
  endif

   do i = 1, n_temp

    node1 = nodes_cylinder(i  , nnodes_cylinder+1)
    node2 = nodes_cylinder(i+1, nnodes_cylinder+1)
    node3 = node_above(node1)
    node4 = node_above(node2)

    nquads_b = nquads_b + 1
    quad(nquads_b,5) = 2   ! Quads on the outflow boundary
    nquads_bcyl2 = nquads_bcyl2 + 1
    quad(nquads_b,1) = node1
    quad(nquads_b,2) = node2
    quad(nquads_b,3) = node4
    quad(nquads_b,4) = node3

    do k = 2, nr-1

     node1 = node3
     node2 = node4
     node3 = node_above(node1)
     node4 = node_above(node2)

      nquads_b = nquads_b + 1
      quad(nquads_b,5) = 2   ! Quads on the outflow boundary
      nquads_bcyl2 = nquads_bcyl2 + 1
      quad(nquads_b,1) = node1
      quad(nquads_b,2) = node2
      quad(nquads_b,3) = node4
      quad(nquads_b,4) = node3

    end do

   end do

 section1 : if (n_sections==1) then

    nquads_bcyl_sym1 = 0

 ! Symmetry boundary 1

   do i = 1, nr_gs

    node1 = (i-1)*i/2 + 1
    node2 = (i+1)*i/2 + 1
    node3 = node_above(node1)
    node4 = node_above(node2)

    nquads_b = nquads_b + 1
    nquads_bcyl_sym1 = nquads_bcyl_sym1 + 1
    quad(nquads_b,5) = 4
    quad(nquads_b,1) = node1
    quad(nquads_b,2) = node2
    quad(nquads_b,3) = node4
    quad(nquads_b,4) = node3

    do k = 2, nr-1

     node1 = node3
     node2 = node4
     node3 = node_above(node1)
     node4 = node_above(node2)

     nquads_b = nquads_b + 1
     nquads_bcyl_sym1 = nquads_bcyl_sym1 + 1
     quad(nquads_b,5) = 4
     quad(nquads_b,1) = node1
     quad(nquads_b,2) = node2
     quad(nquads_b,3) = node4
     quad(nquads_b,4) = node3

    end do

   end do


   do i = 1, nnodes_cylinder

    node1 = nodes_cylinder(1, i  ) !Left-most node of the original sector
    node2 = nodes_cylinder(1, i+1) !Left-most node of the original sector
    node3 = node_above(node1)
    node4 = node_above(node2)

    nquads_b = nquads_b + 1
    nquads_bcyl_sym1 = nquads_bcyl_sym1 + 1
    quad(nquads_b,5) = 4
    quad(nquads_b,1) = node1
    quad(nquads_b,2) = node2
    quad(nquads_b,3) = node4
    quad(nquads_b,4) = node3

    do k = 2, nr-1

     node1 = node3
     node2 = node4
     node3 = node_above(node1)
     node4 = node_above(node2)

     nquads_b = nquads_b + 1
     nquads_bcyl_sym1 = nquads_bcyl_sym1 + 1
     quad(nquads_b,5) = 4
     quad(nquads_b,1) = node1
     quad(nquads_b,2) = node2
     quad(nquads_b,3) = node4
     quad(nquads_b,4) = node3

    end do

   end do

 ! Symmetry boundary 2

    nquads_bcyl_sym2 = 0

   do i = 1, nr_gs

    node1 = (i+1)*i/2 + i+1 !Right-most node of the original sector
    node2 = (i-1)*i/2 + i   !Right-most node of the original sector
    node3 = node_above(node1)
    node4 = node_above(node2)

    nquads_b = nquads_b + 1
     nquads_bcyl_sym2 = nquads_bcyl_sym2 + 1
    quad(nquads_b,5) = 5
    quad(nquads_b,1) = node1
    quad(nquads_b,2) = node2
    quad(nquads_b,3) = node4
    quad(nquads_b,4) = node3

    do k = 2, nr-1

     node1 = node3
     node2 = node4
     node3 = node_above(node1)
     node4 = node_above(node2)

     nquads_b = nquads_b + 1
     nquads_bcyl_sym2 = nquads_bcyl_sym2 + 1
     quad(nquads_b,5) = 5
     quad(nquads_b,1) = node1
     quad(nquads_b,2) = node2
     quad(nquads_b,3) = node4
     quad(nquads_b,4) = node3

    end do

   end do

   do i = 1, nnodes_cylinder

    node1 = nodes_cylinder(nnodes_circum, i+1)
    node2 = nodes_cylinder(nnodes_circum, i  )
    node3 = node_above(node1)
    node4 = node_above(node2)

    nquads_b = nquads_b + 1
    nquads_bcyl_sym2 = nquads_bcyl_sym2 + 1
    quad(nquads_b,5) = 5
    quad(nquads_b,1) = node1
    quad(nquads_b,2) = node2
    quad(nquads_b,3) = node4
    quad(nquads_b,4) = node3

    do k = 2, nr-1

     node1 = node3
     node2 = node4
     node3 = node_above(node1)
     node4 = node_above(node2)

     nquads_b = nquads_b + 1
     nquads_bcyl_sym2 = nquads_bcyl_sym2 + 1
     quad(nquads_b,5) = 5
     quad(nquads_b,1) = node1
     quad(nquads_b,2) = node2
     quad(nquads_b,3) = node4
     quad(nquads_b,4) = node3

    end do

   end do

 endif section1

  write(*,*) " -----------------------------------------------------"
  write(*,*) " Generated elements....."
  write(*,*) "  Volume elements:"
  write(*,*) "    prisms     = ", nprs
  write(*,*) "    tetrahedra = ", ntet
  write(*,*) "    hex        = ", nhex
  write(*,*) "  Boundary elements:"
  write(*,*) "     triangles = ", ntrias_b
  write(*,*) "         quads = ", nquads_b
  write(*,*) " -----------------------------------------------------"

 end subroutine mixed_ph_grid


!*******************************************************************************
!* Write out a file containing line info (only within the viscous layer)):
!* This information is for the implicit line relaxations or line agglomeration.
!* This format is used by FUN3D.
!*******************************************************************************
 subroutine line_info_file(n_points)
 implicit none

 integer, intent(in) :: n_points

 integer :: node_below
 integer :: i, k, n_lines, n_total_points, max_points, min_points, i_count, inode,os

  open(unit=1, file=filename_lines, status="unknown", iostat=os)
  max_points = n_points
  min_points = n_points
  n_lines = nnodes_body
  n_total_points = n_lines * n_points

  write(*,*) " Total number of lines = ", n_lines
  write(*,*) "            max points = ", max_points
  write(*,*) "            min points = ", min_points

  write(1,*) n_lines, n_total_points, "Total lines and points"
  write(1,*) min_points, max_points, "Min and max points in line"

  i_count = 0

! Go up and write out the node info for every node on the body.
! NOTE: Go up to the given limit: n_points

  do i = 1, nnodes_body

     i_count = i_count + 1

!  1. First node (on the body)
     write(1,*) n_points, " Points in line for line = ", i_count
     write(1,'(i10,a10,3es30.20)') i, " x/y/z= ", node(i)%x, node(i)%y, node(i)%z
     node_below = i

!  2. Second node to the node below the last one.
   do k = 2, n_points-1
     write(1,'(i10)') node_above(node_below)
     node_below = node_above(node_below)
   end do

!  3. Last node
     inode = node_above(node_below)
     write(1,'(i10,a10,3es30.20)') inode, " x/y/z= ", node(inode)%x, node(inode)%y, node(inode)%z

  end do

 if (i_count /= n_lines) write(*,*) "Error: i_count /= n_lines"
 write(*,*)
 write(*,*) "lines_fmt file has been written: ", filename_lines

 close(1)
 end subroutine line_info_file




!*******************************************************************************
!* Write out a file containing line info (Lines go all the way up to the outer):
!* This information is for the implicit line relaxations or line agglomeration.
!* This format is used by FUN3D.
!*******************************************************************************
 subroutine line_info_file_all(n_points)
 implicit none

 integer, intent(in) :: n_points

 integer :: node_below
 integer :: i, k, n_lines, n_total_points, max_points, min_points, i_count, inode,os

  open(unit=1, file=filename_lines, status="unknown", iostat=os)
  max_points = n_points
  min_points = n_points
  n_lines = nnodes_body
  n_total_points = n_lines * n_points

  write(*,*) " Total number of lines = ", n_lines
  write(*,*) "            max points = ", max_points
  write(*,*) "            min points = ", min_points

  write(1,*) n_lines, n_total_points, "Total lines and points"
  write(1,*) min_points, max_points, "Min and max points in line"

  i_count = 0

! Go up and write out the node info for every node on the body.
! NOTE: Go up to the given limit: n_points

  do i = 1, nnodes_body

     i_count = i_count + 1

!  1. First node (on the body)
     write(1,*) n_points, " Points in line for line = ", i_count
     write(1,'(i10,a10,3es30.20)') i, " x/y/z= ", node(i)%x, node(i)%y, node(i)%z
     node_below = i

!  2. Second node to the node below the last one.
   do k = 2, n_points-1
     write(1,'(i10)') node_above(node_below)
     node_below = node_above(node_below)
   end do

!  3. Last node
     inode = node_above(node_below)
     write(1,'(i10,a10,3es30.20)') inode, " x/y/z= ", node(inode)%x, node(inode)%y, node(inode)%z

  end do

 if (i_count /= n_lines) write(*,*) "Error: i_count /= n_lines"
 write(*,*)
 write(*,*) "lines_fmt file has been written: ", filename_lines

 close(1)
 end subroutine line_info_file_all


!*******************************************************************************
! This subroutine writes a Tecplot file for the volume grid.
!*******************************************************************************
 subroutine write_tecplot_volume_file

 open(unit=8, file=filename_tecplot_v, status="unknown", iostat=os)
 write(8,*) 'TITLE = "GRID"'
 write(8,*) 'VARIABLES = "x","y","z","k1","k2","k3","k4","k5"'

! Tetra Zone
  if (ntet > 0) then

   write(8,*) 'zone  n=', nnodes,',e=', ntet,' , et=tetrahedron, f=fepoint'
   do i = 1, nnodes
     write(8,'(3es20.10,5i13)') node(i)%x, node(i)%y, node(i)%z, &
                                k1(i),k2(i),k3(i),k4(i),k5(i)
   end do

   do i = 1, ntet
    write(8,'(4i10)') tet(i,1), tet(i,2), tet(i,3), tet(i,4)
   end do

  endif

! Prism zone
  if (nprs > 0) then

   write(8,*) 'zone  n=', nnodes,',e=', nprs,' , et=brick, f=fepoint'
   do i = 1, nnodes
     write(8,'(3es20.10,5i13)') node(i)%x, node(i)%y, node(i)%z, &
                                k1(i),k2(i),k3(i),k4(i),k5(i)
   end do

   do i = 1, nprs
    write(8,'(8i10)') prs(i,1), prs(i,2), prs(i,3), prs(i,3), &
                      prs(i,4), prs(i,5), prs(i,6), prs(i,6)
   end do

  endif

! Hex zone
  if (nhex > 0) then

   write(8,*) 'zone  n=', nnodes,',e=', nhex,' , et=brick, f=fepoint'
   do i = 1, nnodes
     write(8,'(3es20.10,5i13)') node(i)%x, node(i)%y, node(i)%z, &
                                k1(i),k2(i),k3(i),k4(i),k5(i)
   end do

   do i = 1, nhex
    write(8,'(8i10)') hex(i,1), hex(i,2), hex(i,3), hex(i,4), &
                      hex(i,5), hex(i,6), hex(i,7), hex(i,8)
   end do

  endif

 close(8)

 end subroutine write_tecplot_volume_file

!*******************************************************************************
! This subroutine writes  a Tecplot file for boundaries.
!******************************************************************************
 subroutine write_tecplot_boundary_file

 integer :: ntrias_outflow, ntrias_outer

 open(unit=7, file=filename_tecplot_b, status="unknown", iostat=os)
 write(7,*) 'TITLE = "GRID"'
 write(7,*) 'VARIABLES = "x","y","z","k1","k2","k3","k4","k5"'


! Triangles on the hemisphere-cylinder surface
 write(7,*) 'ZONE T="Body"  N=', nnodes,',E=', ntrias,' , ET=quadrilateral, F=FEPOINT'
  do i = 1, nnodes
    write(7,'(3ES20.10,5i13)') node(i)%x, node(i)%y, node(i)%z, &
                               k1(i),k2(i),k3(i),k4(i),k5(i)
  end do
  do i = 1, ntrias
   write(7,'(4I10)') tri(i,1), tri(i,2), tri(i,3), tri(i,3)
  end do

! Triangles on the outflow plane
  ntrias_outflow = 0
  do i = ntrias+1, ntrias_b
   if ( tri(i,5) == 2 ) ntrias_outflow = ntrias_outflow + 1
  end do

 if (ntrias_outflow > 0) then

  write(7,*) 'ZONE T="Outflow: triangles"  N=', nnodes,',E=', ntrias_outflow, &
             ', ET=quadrilateral, F=FEPOINT'
  do i = 1, nnodes
    write(7,'(3ES20.10,5i13)') node(i)%x, node(i)%y, node(i)%z, &
                               k1(i),k2(i),k3(i),k4(i),k5(i)
  end do
  do i = ntrias+1, ntrias_b
   if ( tri(i,5) == 2 ) write(7,'(4I10)') tri(i,1), tri(i,2), tri(i,3), tri(i,3)
  end do

 endif

! Triangles on the outer boundary.
  ntrias_outer = 0
  do i = ntrias+1, ntrias_b
   if ( tri(i,5) == 3 ) ntrias_outer = ntrias_outer + 1
  end do

 write(7,*) 'ZONE T="Outer"  N=', nnodes,',E=', ntrias_outer, &
            ' , ET=quadrilateral, F=FEPOINT'
  do i = 1, nnodes
    write(7,'(3ES20.10,5i13)') node(i)%x, node(i)%y, node(i)%z, &
                               k1(i),k2(i),k3(i),k4(i),k5(i)
  end do
  do i = ntrias+1, ntrias_b
   if ( tri(i,5) == 3 ) write(7,'(4I10)') tri(i,1), tri(i,2), tri(i,3), tri(i,3)
  end do

! Triangles on the symmetry plane 1
  ntrias_outflow = 0
  do i = ntrias+1, ntrias_b
   if ( tri(i,5) == 4 ) ntrias_outflow = ntrias_outflow + 1
  end do

 if (ntrias_outflow > 0) then

  write(7,*) 'ZONE T="Symmetry 1: triangles"  N=', nnodes,',E=', ntrias_outflow, &
             ', ET=quadrilateral, F=FEPOINT'
  do i = 1, nnodes
    write(7,'(3ES20.10,5i13)') node(i)%x, node(i)%y, node(i)%z, &
                               k1(i),k2(i),k3(i),k4(i),k5(i)
  end do
  do i = ntrias+1, ntrias_b
   if ( tri(i,5) == 4 ) write(7,'(4I10)') tri(i,1), tri(i,2), tri(i,3), tri(i,3)
  end do

 endif

! Triangles on the symmetry plane 2
  ntrias_outflow = 0
  do i = ntrias+1, ntrias_b
   if ( tri(i,5) == 5 ) ntrias_outflow = ntrias_outflow + 1
  end do

 if (ntrias_outflow > 0) then

  write(7,*) 'ZONE T="Symmetry 2: triangles"  N=', nnodes,',E=', ntrias_outflow, &
             ', ET=quadrilateral, F=FEPOINT'
  do i = 1, nnodes
    write(7,'(3ES20.10,5i13)') node(i)%x, node(i)%y, node(i)%z, &
                               k1(i),k2(i),k3(i),k4(i),k5(i)
  end do
  do i = ntrias+1, ntrias_b
   if ( tri(i,5) == 5 ) write(7,'(4I10)') tri(i,1), tri(i,2), tri(i,3), tri(i,3)
  end do

 endif





 if (nquads_bcyl1 > 0) then

  !--------------------------------------
  write(7,*) 'Zone T="Cylinder: quads(boundary layer)" N=', nnodes,',E=', &
             nquads_bcyl1,' , ET=quadrilateral, F=FEPOINT'
   do i = 1, nnodes
     write(7,'(3ES20.10,5i13)') node(i)%x, node(i)%y, node(i)%z, &
                                k1(i),k2(i),k3(i),k4(i),k5(i)
   end do
   do i = 1, nquads_b
    if (quad(i,5)==1) then
     write(7,'(4I10)') quad(i,1), quad(i,2), quad(i,3), quad(i,4)
    endif
   end do

  !--------------------------------------
  write(7,*) 'Zone T="Outflow: quads(boundary layer)" N=', nnodes,',E=', &
             nquads_bcyl2,' , ET=quadrilateral, F=FEPOINT'
   do i = 1, nnodes
     write(7,'(3ES20.10,5i13)') node(i)%x, node(i)%y, node(i)%z, &
                                k1(i),k2(i),k3(i),k4(i),k5(i)
   end do
   do i = 1, nquads_b
    if (quad(i,5)==2) then
     write(7,'(4I10)') quad(i,1), quad(i,2), quad(i,3), quad(i,4)
    endif
   end do

  !--------------------------------------
  write(7,*) 'Zone T="Outer quads: quads(boundary layer)" N=', nnodes,',E=', &
             nquads_bcyl3,' , ET=quadrilateral, F=FEPOINT'
   do i = 1, nnodes
     write(7,'(3ES20.10,5i13)') node(i)%x, node(i)%y, node(i)%z, &
                                k1(i),k2(i),k3(i),k4(i),k5(i)
   end do
   do i = 1, nquads_b
    if (quad(i,5)==3) then
     write(7,'(4I10)') quad(i,1), quad(i,2), quad(i,3), quad(i,4)
    endif
   end do
  !--------------------------------------

  if (n_sections == 1) then

    !--------------------------------------
    write(7,*) 'Zone T="Outer quads: quads(left symmetry)" N=', nnodes,',E=', &
               nquads_bcyl_sym1,' , ET=quadrilateral, F=FEPOINT'
     do i = 1, nnodes
       write(7,'(3ES20.10,5i13)') node(i)%x, node(i)%y, node(i)%z, &
                                  k1(i),k2(i),k3(i),k4(i),k5(i)
     end do
     do i = 1, nquads_b
      if (quad(i,5)==4) then
       write(7,'(4I10)') quad(i,1), quad(i,2), quad(i,3), quad(i,4)
      endif
     end do
    !--------------------------------------

    !--------------------------------------
    write(7,*) 'Zone T="Outer quads: quads(right symmetry)" N=', nnodes,',E=', &
               nquads_bcyl_sym2,' , ET=quadrilateral, F=FEPOINT'
     do i = 1, nnodes
       write(7,'(3ES20.10,5i13)') node(i)%x, node(i)%y, node(i)%z, &
                                  k1(i),k2(i),k3(i),k4(i),k5(i)
     end do
     do i = 1, nquads_b
      if (quad(i,5)==5) then
       write(7,'(4I10)') quad(i,1), quad(i,2), quad(i,3), quad(i,4)
      endif
     end do
    !--------------------------------------

  endif

 else

  if (nquads_b > 0) then

   write(7,*) 'Zone T="Outflow: quads" N=', nnodes,',E=', &
              nquads_b,' , ET=quadrilateral, F=FEPOINT'
    do i = 1, nnodes
      write(7,'(3ES20.10,5i13)') node(i)%x, node(i)%y, node(i)%z, &
                                 k1(i),k2(i),k3(i),k4(i),k5(i)
    end do
    do i = 1, nquads_b
     write(7,'(4I10)') quad(i,1), quad(i,2), quad(i,3), quad(i,4)
    end do

  endif

 endif


 close(7)

 end subroutine write_tecplot_boundary_file

!*******************************************************************************
! This subroutine writes a ugrid file.
!*******************************************************************************
 subroutine write_ugrid_file

  if ( b8_ugrid_format ) then
    open(unit=9, file=filename_ugrid, form='unformatted',access="stream",&
                                      status='unknown', iostat=os )
    write(9) nnodes,   ntrias_b,    nquads_b,   ntet,    0, nprs, nhex
  else
    open(unit=9, file=filename_ugrid, status="unknown", iostat=os)
    !                    #nodes, #tri_faces, #quad_faces, #tetra, #pyr, #prz,
    !                    #hex
    write(9,'(7I10)') nnodes,   ntrias_b,    nquads_b,   ntet,    0, nprs, nhex
  endif

   if ( b8_ugrid_format ) then
   write(9)                                                                     &
   (( node(i)%x, node(i)%y, node(i)%z            ), i = 1, nnodes   ),          &
   (( tri(i,1), tri(i,2), tri(i,3)               ), i = 1, ntrias_b ),          &
   (( quad(i,1), quad(i,2), quad(i,3), quad(i,4) ), i = 1, nquads_b ),          &
   (( tri(i,5)                                   ), i = 1, ntrias_b ),          &
   (( quad(i,5)                                  ), i = 1, nquads_b ),          &
   (( tet(i,1), tet(i,2), tet(i,3), tet(i,4)     ), i = 1, ntet     ),          &
   (( prs(i,1), prs(i,2), prs(i,3),                                             &
      prs(i,4), prs(i,5), prs(i,6)               ), i = 1, nprs     ),          &
   (( hex(i,1), hex(i,2), hex(i,3),                                             &
      hex(i,4), hex(i,5), hex(i,6),                                             &
      hex(i,7), hex(i,8)                         ), i = 1, nhex     )

   else

! Nodes
  do i = 1, nnodes
   write(9,'(3ES20.10)') node(i)%x, node(i)%y, node(i)%z
  end do

! Triangular faces = ntri
  if (ntrias_b > 0) then
   do i = 1, ntrias_b
    write(9,'(3I10)') tri(i,1), tri(i,2), tri(i,3)
   end do
  endif

! Quad faces = nquad
  if (nquads_b > 0) then
   do i = 1, nquads_b
    write(9,'(4I10)') quad(i,1), quad(i,2), quad(i,3), quad(i,4)
   end do
  endif

! Face tag
  if (ntrias_b > 0) then
   do i = 1, ntrias_b
    write(9,'(I10)')  tri(i,5)
   end do
  endif

  if (nquads_b > 0) then
   do i = 1, nquads_b
    write(9,'(I10)') quad(i,5)
   end do
  endif

! tet
  if (ntet > 0) then
   do i = 1, ntet
    write(9,'(4I10)') tet(i,1), tet(i,2), tet(i,3), tet(i,4)
   end do
  endif

! Prism
  if (nprs > 0) then
   do i = 1, nprs
    write(9,'(6I10)') prs(i,1), prs(i,2), prs(i,3), &
                      prs(i,4), prs(i,5), prs(i,6)
   end do
  endif

! Hex
  if (nhex > 0) then
   do i = 1, nhex
    write(9,'(8I10)') hex(i,1), hex(i,2), hex(i,3), &
                      hex(i,4), hex(i,5), hex(i,6), hex(i,7), hex(i,8)
   end do
  endif
endif

  close(9)

 end subroutine write_ugrid_file


!*******************************************************************************
! This subroutine writes an k file
!******************************************************************************
 subroutine write_k_file

 open(unit=10, file=filename_k, status="unknown", iostat=os)

! Write node_number, k1, k2, k3, k4, k5

  write(10,*) nnodes

 do i = 1, nnodes
  write(10,'(6i13)') i, k1(i),k2(i),k3(i),k4(i),k5(i)
 end do

 close(10)

 end subroutine write_k_file


!********************************************************************************
!* This subroutine is useful to expand or shrink type(node_data_yz) arrays.
!*
!*  Array, x, will be allocated if the requested dimension is 1 (i.e., n=1)
!*                     expanded to the requested dimension, n, if n > dim(x).
!*                     shrunk to the requested dimension, n, if n < dim(x).
!*
!********************************************************************************
  subroutine my_alloc_ndyz_ptr(x,n)
  implicit none

  integer,                    intent(in   ) :: n
  type(node_data_yz), dimension(:), pointer :: x

  integer :: i
  type(node_data_yz), dimension(:), pointer :: temp

  if (n <= 0) then
   write(*,*) "my_alloc_ndyz_ptr received non-positive dimension. Stop."
   stop
  endif

! If initial, allocate and return
  if (.not.(associated(x))) then
   allocate(x(n))
   return
  endif

! If reallocation, create a pointer with a target of new dimension.
  allocate(temp(n))
    temp(n)%gnode = 0
    temp(n)%y     = zero
    temp(n)%z     = zero

! (1) Expand the array dimension
  if ( n > size(x) ) then

   do i = 1, size(x) 
    temp(i)%gnode = x(i)%gnode
    temp(i)%y     = x(i)%y
    temp(i)%z     = x(i)%z
  end do

! (2) Shrink the array dimension: the extra data, x(n+1:size(x)), discarded.
  else

   do i = 1, n
    temp(i)%gnode = x(i)%gnode
    temp(i)%y     = x(i)%y
    temp(i)%z     = x(i)%z
   end do

  endif

! Destroy the target of x
  deallocate(x)

! Re-assign the pointer
   x => temp

  return

  end subroutine my_alloc_ndyz_ptr
!********************************************************************************

function int_sign(n)
implicit none
integer, intent(in) :: n
integer :: int_sign

if (n >= 0) then
 int_sign = 1
else
 int_sign = -1
endif

end function int_sign

end program hemisphere_cylinder_grid
