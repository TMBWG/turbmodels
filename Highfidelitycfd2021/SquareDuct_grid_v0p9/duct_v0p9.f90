!*******************************************************************************
!
!          --- Grid generation code for a duct (rectangular box) ---
!
!
! This code generates a hexahedral grid for a duct (rectangular box).
!
! This is Version 0.9 (December 6, 2019).
!
!------------------------------------------------------------------------------
!
!-------------
!  Input: Parameters are set in the input file named as 'input.nml'.
!         See 'Input parameter module' below.
!
!
! v0.9 (12-06-2019): Added an option to generate points with a geometric stretching:
!                    z(k+1) = z(k) + dr1*r**(k-1) 
!                    Added an option to provide a user-defined dr1.
! v0.8 (09-02-2019): Fixed BC tags: ymin <--> ymax.
! v0.7: Added prismatic and tetrahedral grids.
! v0.6: Fixed bugs: boundary markers for SU2,
!                   outlet size, nx_out was # of nodes...
! v0.5: Added an outlet region as an option.
! v0.4: Added an inlet region as an option.
! v0.3: Simplified.
! v0.2: Added some additional comments.
!
!-------------
! Output:
!
!   For each grid level, k = 1, 2, 3, ...
!
!   - Grid file = "duct_strct.k.(b8/lb8)ugrid"
!
!   - Node line-info( BL) = "duct_strct.k.lines_fmt"     - Points in lines in the BL region.
!   - Node line-info(all) = "duct_strct.k.lines_fmt_all" - Points in lines all across the domain.
!                        (NOTE: Rename .lines_fmt_all as .lines_fmt, for use in FUN3D.)
!
!   - PLOT3D grid file = "duct_strct.p3d/ufmt" with "duct_strct.nmf" (only for hex).
!
!   - SU2 grid file    = "duct_strct.su2".
!
!   - Cell line-info( BL) = "duct_strct.k.lines_fmt_cc"     - Cells in lines in the BL region.
!   - Cell line-info(all) = "duct_strct.k.lines_fmt_cc_all" - Cells in lines all across the domain.
!                        (NOTE: Rename .lines_fmt_all as .lines_fmt, for use in FUN3D.)
!
!       Note: Line information can be used for line relaxation or line agglomeration.
!
!   - duct_strct.k.mapbc"        !Boundary condition file for FUN3D
!
!   - Tecplot file = "duct_strct.k.tec_bndary.dat" !Boundary grid for viewing
!   - Tecplot file = "duct_strct.k.tec_volume.dat" !Volume grid for viewing
!
!
! [Send comments/bug-report to Hiro at hiro(at)nianet.org.]
!
!
! Note: To convert .ugrid+.mapbc file to a CGNS file. Install the CGNS package
!       (see http://cgns.github.io), and use the utility code 'aflr3_to_cgns': at a terminal,
!
!       %$HOME/cgnslib/bin/cgnstools/aflr3_to_cgns duct_strct.b8.ugrid duct_strct.cgns
!
!       where $HOME/cgnslib/ is the directory of the installed CGNS on your machine.
!
!
!*******************************************************************************

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
!  Input parameter module
!
!-------------------------------------------------------------------------------
!
!  Sample input file: 'input.nml' to generate a hex grid.
!  ------------------------------------------------------
!  &input_parameters
!                      debug_mode = F
!          target_reynolds_number = 508e+3 !Target Reynolds number based on length=1.0.
!                   target_y_plus = 0.022  !Target y-plus value.
!               generate_pts_type = 0      !Two ways to generate pts in z-direction.
!                bl_height_factor = 1.0    !Factor by which BL thickness estimate is multiplied.
!                bl_pts_frac      = 0.8    !80% of points are palced in the BL region in z/y-direction.
!                        dr1_user = -1.0   !User specified dr1 (will be used if positive).
!                    viscous_wall = 2      !1=zmin is viscous wall, 2=zmin and ymin are viscous walls.
!          upstream_create_region = T      !T: create an upstream region (symmetrry BCs for all there).
!                 upstream_x      =-1.26829
!        downstream_create_region = T      !T: create an upstream region (symmetrry BCs for all there).
!               downstream_x      = 2.26829
!        downstream_spacing_ratio = 0.75
!                            xmin =  0.0   !Domain xmin
!                            xmax = 52.0   !Domain xmax
!                            ymin =  0.0   !Domain ymin
!                            ymax =  0.5   !Domain ymax
!                            zmin =  0.0   !Domain zmin
!                            zmax =  0.5   !Domain zmax
!                      igrid_type = 1      !1=strct(hex), 2=prism, 3=tetra.
!                              nx = 240    !# of elements (spacing, not nodes) in x-direction
!                              ny =  40    !# of elements (spacing, not nodes) in y-direction
!                              nz =  40    !# of elements (spacing, not nodes) in z-direction
!           generate_ugrid_file   = T      !T = write a .ugrid file, F=not to write
!          ugrid_file_unformatted = F      !T = unformatted .ugrid/.ufmt, F = formatted .ugrid/.p3d
!           generate_line_file_nc = T      !T = write node line files, F=not to write
!           generate_line_file_cc = T      !T = write cell line files, F=not to write
!           generate_tec_file_b   = T      !T = write a boundary grid (Tecplot), F=not to write
!           generate_tec_file_v   = F      !T = write a volume grid (Tecplot), F=not to write
!  /
!  ------------------------------------------------------
!
!  Note: No need to specify all namelist variables.
!        In the above, those not shown are given their default values
!        as defined below.
!
!  NOTE: If you wish to generate coarser grids later by regular coarsening,
!        you must set (nx,ny,nz) as multiples of 2: e.g., nx=ny=nz=2**n.
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
 module input_parameter_module

  implicit none

  integer , parameter :: dp = selected_real_kind(P=15)
  integer , parameter :: kd = selected_int_kind(8)

  public

!----------------------------
! Default input values
!----------------------------

!----------------------------
! debug_mode = T: for debug use only.
!              F: Default
!
  logical :: debug_mode = .false.

!----------------------------
!  igrid_type = Element type as listed below.
!                            1 = Structured - Hexa
!                            2 = Prismatic
!                            3 = Tetrahedral

  integer :: igrid_type = 1

!----------------------------
! target_reynolds_number =
!   Reynolds number to determine the mesh spacing at wall,
!                                  based on length = 1.0.

  real(dp) :: target_reynolds_number = 508e+3

!----------------------------
! target_y_plus = y-plus to determine the mesh spacing at wall.

  real(dp) :: target_y_plus = 0.022

!----------------------------
! generate_pts_type
!
!  = 0 : BL and outer regions separately.
!  = 1 : simple stretching all the way.

  integer :: generate_pts_type = 0

!----------------------------
! BL region height factor: BL region is defined as
!    bl_height_factor*(theoretical estimate).
! E.g., set 1.5 if you increase it to 1.5 times the estimate.

  real(dp) :: bl_height_factor = 1.0_dp

!----------------------------
! Fraction of pts put in BC region.
! Default is 80% as set below by 0.8.

  real(dp) :: bl_pts_frac = 0.8_dp

!----------------------------
! Specified first off-the-wall distance.
!
!  This will be used if positive and ignored otherwise.

  real(dp) :: dr1_user = -1.0_dp

!----------------------------
! # of viscous walls
!
! 1 -> zmin is viscous wall.
! 2 -> zmin and ymin are viscous walls.

  integer :: viscous_wall = 2

!----------------------------
! Extended upstream  = T: Create an upstream region
!                      F: Default
!
!                 Upstream region
!               <------------------->
!               o---o---o---o---o---x---o---o---o---o-----------
! xmin + upstream_x                xmin
!
! Note: # of elms in the upstream region will be determined inside
!       the code for smoothness. Some of nx elements will be chosen
!       to be placed in this region. The grid is uniform in this region,
!       and the interior gerid will be stretched for smooth transition.

  logical     :: upstream_create_region = .false.
  real(dp)    :: upstream_x             = -1.26829_dp

!----------------------------
! Extended downstream  = T: Create an downstream region
!                        F: Default
!
! Example of downstream_spacing_ratio = 1.0
!
!                     Downstream region
!                    <----------------->
! ----o----o----o----x----o---o--o-o-o-o
!                  xmax               xmax + downstream_x
!               <---> <--->
!                 dx1   dx2=dx1*downstream_spacing_ratio
!
! Note: If downstream_nx is negative, # of elements in this region
!       will be determined automatically inside the code to create
!       almost uniform spacing.
!
  logical     :: downstream_create_region = .false.
  real(dp)    :: downstream_x             = 2.26829_dp
  real(dp)    :: downstream_spacing_ratio = 1.0_dp

!----------------------------
! Domain size
! 
  real(dp) :: xmin = 0.0_dp, xmax = 52.0_dp
  real(dp) :: ymin = 0.0_dp, ymax =  0.5_dp
  real(dp) :: zmin = 0.0_dp, zmax =  0.5_dp

!----------------------------
! # of Elements

  integer(kd) :: nx, ny, nz

!----------------------------
! generate_ugrid_file = T to write .ugrid file.
!                       F not to write.

  logical :: generate_ugrid_file = .true.

!----------------------------
! ugrid_file_unformatted = T: unformatted, F: formatted

  logical :: ugrid_file_unformatted = .true.

!----------------------------
! generate_p3d_ufmt_file = T = Write .p3d/.umft and .nmf files, F: don't write.

  logical :: generate_p3d_file = .true.

!----------------------------
! p3d_file_unformatted = T: unformatted, F: formatted

  logical :: p3d_file_unformatted = .true.

!----------------------------
! generate_su2grid_file = T to write .su2 file
!                         F not to write.

  logical :: generate_su2grid_file = .false. 

!----------------------------
! generate_line_file_nc = T or F.

  logical :: generate_line_file_nc = .false.

!----------------------------
! generate_line_file_cc = T or F.

  logical :: generate_line_file_cc = .false.

!----------------------------
! generate_tec_file_b = T to write a Tecplot file for boundary.

  logical :: generate_tec_file_b = .false.

!----------------------------
! generate_tec_file_v = T: write a Tecplot file for the entire grid.

  logical :: generate_tec_file_v = .false.

!----------------------------
! End of Default input values
!----------------------------

! Below is the list of all input parameters available:

  namelist / input_parameters /   &
                      debug_mode, &
          target_reynolds_number, &
                   target_y_plus, &
               generate_pts_type, &
                bl_height_factor, &
                bl_pts_frac     , &
                        dr1_user, &
                      xmin, xmax, &
                      ymin, ymax, &
                      zmin, zmax, &
                      igrid_type, &
                    viscous_wall, &
          upstream_create_region, &
          upstream_x            , &
        downstream_create_region, &
        downstream_x            , &
        downstream_spacing_ratio, &
                      nx, ny, nz, &
             generate_ugrid_file, &
          ugrid_file_unformatted, &
               generate_p3d_file, &
            p3d_file_unformatted, &
           generate_su2grid_file, &
           generate_line_file_nc, &
           generate_line_file_cc, &
             generate_tec_file_b, &
             generate_tec_file_v

 contains

!*****************************************************************************
!* Read input_parameters in the input file: file name = namelist_file
!*****************************************************************************
  subroutine read_nml_input_parameters(namelist_file)

  implicit none
  character(9), intent(in) :: namelist_file
  integer :: os

  write(*,*) "**************************************************************"
  write(*,*) " List of namelist variables and their values"
  write(*,*)

  open(unit=10,file=namelist_file,form='formatted',status='old',iostat=os)
  read(unit=10,nml=input_parameters)

  write(*,nml=input_parameters) ! Print the namelist variables.
  close(10)

  end subroutine read_nml_input_parameters

 end module input_parameter_module
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
!  End of input parameter module
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------


!*******************************************************************************
!*******************************************************************************
! Main program begins here.
!*******************************************************************************
!*******************************************************************************
 program duct_grid

 use input_parameter_module

 implicit none

! Parameters
  integer(kd), parameter ::  max_lvls = 50 !Just assume 50.

  real(dp),    parameter :: zero = 0.0_dp
  real(dp),    parameter ::  one = 1.0_dp

  integer(kd), parameter ::  kd0 = 0_kd
  integer(kd), parameter ::  kd1 = 1_kd
  integer(kd), parameter ::  kd2 = 2_kd
  integer(kd), parameter ::  kd3 = 3_kd

  character(80) :: elmtype

! For debugging.
  character(80) :: filename01 = "debug01_boundary.dat"

  character(80), dimension(max_lvls) :: filename_mapbc
  character(80), dimension(max_lvls) :: filename_ugrid
  character(80), dimension(max_lvls) :: filename_p3d
  character(80), dimension(max_lvls) :: filename_p3d_nmf
  character(80), dimension(max_lvls) :: filename_su2
  character(80), dimension(max_lvls) :: filename_tecplot_b
  character(80), dimension(max_lvls) :: filename_tecplot_v

  character(80), dimension(max_lvls) :: filename_lines_z      !Line information within BL in z
  character(80), dimension(max_lvls) :: filename_lines_y      !Line information within BL in y
  character(80), dimension(max_lvls) :: filename_lines_z_all  !Line information all the way to outer boundary in z
  character(80), dimension(max_lvls) :: filename_lines_y_all  !Line information all the way to outer boundary in y

  character(80), dimension(max_lvls) :: filename_lines_cz     !Cell-line information within BL in z
  character(80), dimension(max_lvls) :: filename_lines_cy     !Cell-line information within BL in y
  character(80), dimension(max_lvls) :: filename_lines_cz_all !Cell-line information all the way to outer boundary in z
  character(80), dimension(max_lvls) :: filename_lines_cy_all !Cell-line information all the way to outer boundary in y

! Local variables

  real(dp)  ,  dimension(:), allocatable :: x
  real(dp)  ,  dimension(:), allocatable :: x_temp
  real(dp)  ,  dimension(:), allocatable :: y
  real(dp)  ,  dimension(:), allocatable :: z

  integer(kd), dimension(10)              :: warning
  real(dp)   , dimension(10)              :: sfactor

  integer(kd)   :: os,  i, k, nm, itr, cglevels, nxs, nys, nzs, id, iskip, glevel
  real(dp)      :: rmax, dr1, sf, sf0, gr, target_x, cx, current_x, xi, cf
  character(80) :: glevel_char

  integer(kd)   :: izero, izero_0
  real(dp)      :: x_upstream, dx

  integer(kd)   :: nx_out, ixmax, ixmax_0
  real(dp)      :: last_dx

   write(*,*)
   write(*,*) "----------------------------------------------------------------"
   write(*,*) "----------------------------------------------------------------"
   write(*,*) "----------------------------------------------------------------"
   write(*,*) "----------------------------------------------------------------"
   write(*,*) "----------------------------------------------------------------"
   write(*,*) " Duct grid generator"
   write(*,*) "----------------------------------------------------------------"
   write(*,*) "----------------------------------------------------------------"
   write(*,*) "----------------------------------------------------------------"
   write(*,*) "----------------------------------------------------------------"
   write(*,*) "----------------------------------------------------------------"
   write(*,*)

!*******************************************************************************
! Read the input parameters, defined in the file named as 'input.nml'.
!*******************************************************************************

   write(*,*) "Reading the input file: input.nml..... "
   write(*,*)
   call read_nml_input_parameters('input.nml')
   write(*,*)

!*******************************************************************************
! Duct geometery without inlet and outlet.
!
!        z
!        ^  x
!        | /
!        |/
! y <----
!                                     -----------------------
!                                    /.                     /|
!                                   / .                    / |
!                                  /  .                   /  |
!                                 /   .                  /   |
!                                /    .                 /    |
!                               /     .                /     |
!                              /      .               /      |
!                             /       .              /       |
!                            /        .             /        |
!                           /         .            /         |
!                          /          .           /          |
!                    zmax ------------------------           |
!                  (kmax) !           .          !           |
!                         !           .          !           |
!                         !           .          !           |
!                         |           ...........|........... xmax (i_max)
!                         |          .           |          /
!                         |         .            |         /
!                         |        .             |        /
!                         |       .              |       /
!                         |      .               |      /
!                         |     .                |     /
!                         |    .                 |    /
!                         |   .                  |   /
!                         |  .                   |  /
!                         | .                    | /
!                  (kmin) |.                     |/
!                    zmin ----------------------- xmin (i_min)
!                       ymax                    ymin (j_min)
!                      (j_max)
!
! The domain size is determined by the input parameters:
!  (xmin,xmax)
!  (ymin,ymax)
!  (zmin,zmax)
!
!
! Nodes will be ordered with the lexicographic ordering, and so the node
! number for a point at (i,j,k) is given by
!
!    node number = i + (j-1)*(nx+1) + (k-1)*(nx+1)*(ny+1)
!
! This is implemented in the function "node_number(i,j,k,nx,ny)".
!
!
!*******************************************************************************

 id = 1 !the number added to filenames.

!*******************************************************************************
!
! Initial setup
!
!*******************************************************************************

   write(*,*)
   write(*,*)
   write(*,*) "   -------------------------------------------------  "
   write(*,*) "   ---- Initial setup                           ----  "
   write(*,*) "   -------------------------------------------------  "

   if     (igrid_type==1) then

     elmtype = "strct"

   elseif (igrid_type==2) then

     elmtype = "prism"

   elseif (igrid_type==3) then

     elmtype = "tetra"

   else

    write(*,*) " >>> Invalid input value: igrid_type = ", igrid_type
    write(*,*) "        igrid_type must be 1 or 2 or 3. Try again."
    stop

   endif

!*******************************************************************************
!
! Maximum coarse grid level.
!
!*******************************************************************************

   write(*,*)
   write(*,*)
   write(*,*) "   -------------------------------------------------  "
   write(*,*) "   ----   Maximum level of regular coarsening   ----  "
   write(*,*) "   -------------------------------------------------  "

   nxs = nx
   nys = ny
   nzs = nz

     cglevels = 0
   do

   !Check the current level.
    if ( mod(nxs,kd2)==kd0 .and. &
         mod(nys,kd2)==kd0 .and. &
         mod(nzs,kd2)==kd0       ) then
     cglevels = cglevels + kd1 !can be regualrly coarsened.
    else
     exit  !cannot be regularly coarsend. Stop. The current is the coarsest one.
    endif

    !Coarsen and proceed.
     nxs = nxs / kd2
     nys = nys / kd2
     nzs = nzs / kd2

   end do

     write(*,*) "         # of coarse grids that can be generated = ", cglevels
     write(*,*) "  Total grid levels (including the target level) = ", cglevels+kd1

   write(*,*) "   -------------------------------------------------  "
   write(*,*) "  "
   write(*,*) "   -------------------------------------------------  "
   write(*,*)

!*******************************************************************************
!
! End of Maximum coarse grid level.
!
!*******************************************************************************


!*******************************************************************************
!
! Start generating points in the z-coordinate direction.
!
!*******************************************************************************

   write(*,*)
   write(*,*)
   write(*,*) "   -------------------------------------------------  "
   write(*,*) "   ----   Point distribution                    ----  "
   write(*,*) "   -------------------------------------------------  "
   write(*,*)

   allocate( x(nx+kd1), y(ny+kd1), z(nz+kd1) )

   write(*,*) " Parameters for interior node generation"

 !  wall
 !  z=0                                         z=rmax
 !  ----------------------------------------------|------> z
 !
 !              Boundary layer region
 !  <-------------------------------------------->
 !
 !  o-----o-------o-----------o-----------o--------------
 !    dr1


 !------------------------------------------------------------------
 ! First vertical spacing off the wall:

   !----------------------------------------------------------------
   ! Option 1: User specifies the distance.

    if ( dr1_user >= 0.0_dp ) then

      dr1 = dr1_user
 
      write(*,*) " User provided the spacing: "
      write(*,*) "    --------->       dr1 = ", dr1

   !----------------------------------------------------------------
   ! Option 2: Estimate from Reynolds # and y-plus.

    else

       cf = 0.026_dp/(target_reynolds_number)**(1.0_dp/7.0_dp)
      dr1 = ( sqrt(2.0_dp/cf)/target_reynolds_number) * target_y_plus
      write(*,*)
      write(*,*) ">>> Compute the first off-the-wall spacing dr1:"
      write(*,*) "    dr1 determined by the target Re and y_plus:"
      write(*,*) "    target_reynolds_number = ", target_reynolds_number
      write(*,*) "             target_y_plus = ", target_y_plus
      write(*,*) "    --------->         dr1 = ", dr1

    endif
   !----------------------------------------------------------------

 ! End of First vertical spacing off the wall:
 !------------------------------------------------------------------


 !------------------------------------------------------------------
 ! rmax = estimated thickness of the specified boundary-layer region:

     write(*,*)
     write(*,*) ">>> Boundary-layer thickness estimate, rmax, at xmax:"
     write(*,*) "    from a turbulent flow over a flat plate:"
     write(*,*) "    Eq.(6-72), page 430, 'Viscous Fluid Flow' 2nd edition (1991) by White."
     rmax = bl_height_factor * ( 0.37_dp/( target_reynolds_number*(xmax-xmin) )**(0.2_dp) )
     write(*,*) "    ------------> rmax = ", rmax
     write(*,*)

    ! If rmax < dr1.......

     if (rmax < dr1) then
      rmax = dr1 * 10.0_dp
      warning(1) = 1
      write(*,*)
      write(*,*) ">>> The estimated rmax is too small (rmax < dr1)."
      write(*,*) "    Set rmax = dr1 * 10.0_dp."
      write(*,*) "    ------------> will try with rmax = ", rmax
     else
      warning(1) = 0
     endif

     write(*,*)
     write(*,*) "  NOTE: rmax defines the BL region in the grid."
     write(*,*) "        It can be adjusted by the parameter: bl_height_factor."
     write(*,*)



    if ( generate_pts_type /= 0 ) then
     write(*,*)
     write(*,*) " ----- OK, a simple stretching is requested. The rmax is not used."
     write(*,*)
    endif


     write(*,*)
     write(*,*) " ---------- Generate points in z-direction ---------- "
     write(*,*)
 
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
! Option 0
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

 generate_pts_in_z : if ( generate_pts_type == 0 ) then

    write(*,*)
    write(*,*) " --- Option 0: BL and outer regions separately."
    write(*,*)

 !-------------------------------------------------------------------------------
 !-------------------------------------------------------------------------------
 ! BL region for z.
 !-------------------------------------------------------------------------------
 !-------------------------------------------------------------------------------

   write(*,*)
   write(*,*) " ------------------------------------------------------"
   write(*,*) "  (1) BL region:"
   write(*,*) " ------------------------------------------------------"

 !----------------------------------------------------------------
 ! Put (bl_pts_frac)x(nz) in the BL region.
 ! Default: bl_pts_frac = 0.8 -> 80% pts in BL region.

   nm = int( real(nz+kd1,dp) * bl_pts_frac )

   write(*,*)
   write(*,*) " nm: points inside the BL region:"
   write(*,*) "     nm = ", nm, "  nz = ", nz
   write(*,*)

 !----------------------------------------------------------------
 ! But wait. Check if it is possible.
 !
 ! Add up dr1 and see if we reach rmax or not.
 ! If k==0 at the end, we don't reach -> apply stretching.
 ! If not, we generate a uniform grid with a smaller nm (nm=k+1).
 !
 ! E.g., nm = 8.
 !
 !  z=0                                           z=rmax
 !  -----------------------------------------------|------> z
 !
 ! Case(1): We don't reach rmax. -> apply stretching.
 !
 !  1   2   3   4   5   6   7   8
 !  o---o---o---o---o---o---o---o
 !   dr1
 !
 ! Case(2): We exceed rmax. -> uniform grid.
 !
 !  1       2       3       4       5       6       7       8
 !  o-------o-------o-------o-------o-------o-------o-------o
 !     dr1
 !               In this case, we redefine: nm = 7.
 !

    i = 0
    k = 0
   xi = zero

  do
    i = i + 1
   xi = xi + dr1

   if (i==nm) exit !-> we don't reach rmax -> apply stretching.

   if (xi > rmax) then !-> we exceed rmax -> stop here for uniform grid.
    k = i
    exit
   endif

  end do

!------------------------------------------------------------------------------
! (1)Not enough points for uniform grid. Good. Apply stretching.

  if ( k == 0 ) then

  ! E.g., nm = 8.
  !
  !  z=0                                             z=rmax
  !  -------------------------------------------------|------> z
  !
  !  1   2    3     4      5       6        7         8
  !  o---o----o-----o------o-------o--------o---------o----------
  !   dr1

   write(*,*)
   write(*,*) " Not enough points for uniform grid -> apply stretching."
   write(*,*)

          sf0 = 7.0_dp !initial guess
    target_x  = dr1    !target spacing

    ! we try to match this by cx*xi, where

           cx = rmax
           xi = one/real(nm-kd1,dp) !xi=(k-1)/nm=[0,1] for k=[1,nm]

   write(*,*)
   write(*,*) " Calling stretching-factor solver..."
    call compute_stretching_factor(dr1,rmax,xi,sf0, current_x,sf,itr,warning(2)  )
   write(*,*)

     sfactor(1) = sf

     write(*,*)
     write(*,*) "Actual stretching factor = ", sf
     write(*,*) "           at iterations = ", itr
     write(*,*) "                     dr1 = ", dr1
     write(*,*) "                     z2  = ", current_x
     write(*,*)

    z(1) = zmin
   do k = 2, nm
    xi = real(k-kd1,dp)/real(nm-kd1,dp) !xi=[0,1] for k=[1,nm]
    gr = (one-exp(sf*xi))/(one-exp(sf))
    z(k) = gr*rmax
   end do

!------------------------------------------------------------------------------
! (2)Enough points for uniform spacing. Then, generate a uniform grid.

  else

  ! E.g., nm = 13.
  !
  !  z=0                                           z=rmax
  !  -----------------------------------------------|------> z
  !
  !  1   2   3   4   5   6   7   8   9  10  11  12  13
  !  o---o---o---o---o---o---o---o---o---o---o---o---o------------
  !   dr1
  !                         Note: a node is not precisely at z=rmax.

   write(*,*) " Enough points for uniform grid -> uniform spacing in BL."

   nm = k + 1 !k=# of elements to exceed rmax. nm=# of points.

    z(1) = zmin
   do k = 2, nm
    z(k) = z(1) + real(k,dp)*dr1
   end do

  endif

 !-------------------------------------------------------------------------------
 !-------------------------------------------------------------------------------
 ! Outer region for y and z.
 !-------------------------------------------------------------------------------
 !-------------------------------------------------------------------------------

  !Compute the stretching parameter:

  ! We wish to match the spacing at z=rmax.
  !
  !                        Outer region
  !         <---------------------------------------->
  !
  !       z=rmax                                     zmax
  !     ----|-----------------------------------------|------> z
  !
  !   nm-1  nm   nm+1
  !     o---o---o----o-----o------o---------o---------o
  !
  ! If the remaining points (nz+1-nm) are distributed uniformly:
  !
  ! (1)Coarser than z(nm)-z(nm-1)
  !
  !       z=rmax                                     zmax
  !     ----|-----------------------------------------|------> z
  !
  !   nm-1  nm   nm+1
  !     o---o------o------o------o------o------o------o
  !
  !    Good. Then we apply stretching and create something like below:
  !
  !   nm-1  nm   nm+1
  !     o---o---o----o-----o------o---------o---------o
  !
  !
  ! (2)Finer than z(nm)-z(nm-1)
  !  
  !   nm-1 nm nm+1
  !     o---o--o--o--o--o--o--o--o--o--o--o--o--o--o--o
  !
  !   Then, for now, we apply stretching but it will find
  !   negative stretching factor to create something like below:
  !
  !   nm-1 nm nm+1
  !     o---o---o---o---o---o--o--o--o--o--o--o-o-o-o-o
  !
  !   Not sure if you like it. If you don't, adjust parametes.
  !

      write(*,*)
      write(*,*) " ------------------------------------------------------"
      write(*,*) "  Outer region:"
      write(*,*) " ------------------------------------------------------"
      write(*,*)
      write(*,*) "           z(nm) = ", z(nm)
      write(*,*) "         z(nm-1) = ", z(nm-kd1)
      write(*,*) " z(nm) - z(nm-1) = ", z(nm) - z(nm-kd1)
      write(*,*) " uniform spacing = ", (zmax-z(nm))/real(nz-nm,dp)
      write(*,*)

    !Good if the uniform spacing is larger.
      if ( (zmax-z(nm))/real(nz-nm,dp) > z(nm) - z(nm-kd1) ) then
       write(*,*) " >>>>> Good that uniform spacing is larger. "
       write(*,*) " >>>>> Let's apply stretching."
      else
       write(*,*) " >>>>> Uniform spacing is smaller...."
       write(*,*) " >>>>> Stretching factor is expected to be negative... Well, OK..."
      endif

          sf0 = 2.0_dp            !initial guess
    target_x  = z(nm) - z(nm-kd1) !target spacing

    ! we try to match this by cx*xi, where xi is the first point at k=nm+1:

           cx = zmax - z(nm)
           xi = real(nm+kd1 - nm,dp)/ real(nz+kd1 - nm,dp) !xi=(k-nm)/(nz+1-nm)=[0,1] for k=[nm,nz+1]

   write(*,*)
   write(*,*) " Calling stretching-factor solver..."
    call compute_stretching_factor(target_x,cx,xi,sf0, current_x,sf,itr, warning(3))
   write(*,*)

      sfactor(2) = sf

     write(*,*)
     write(*,*) "Actual stretching factor = ", sf
     write(*,*) "           at iterations = ", itr
     write(*,*) "           at   a node i = ", i
     write(*,*) "       z(nm  ) - z(nm-1) = ", target_x
     write(*,*) "       z(nm+1) - z(nm  ) = ", current_x
     write(*,*)

     if (warning(3) == 2) then
      write(*,*)
      write(*,*) ">>>>> Negative stretching factor..."
      write(*,*) ">>>>> -> Many points outside the BL region."
      write(*,*) ">>>>> -> If you don't like it, increase bl_height_factor."
      write(*,*)
     endif

 !--------------------------------------------------------
 ! Generate points in z-direction.

   do k = nm+kd1, nz+kd1

!   Uniform parameter function: e.g., xi = [0, 0.1, 0.2, 0.3, ..., 0.8, 0.9, 1]
        xi = real( k - (nm) , dp ) / real( nz+kd1 - (nm) , dp)
    !Stretched parameter function
    !  xis = [0,0.01,0.015,0.02,...,0.5,0.8,1.0]
        gr = (one-exp(sf*xi))/(one-exp(sf))
      z(k) = z(nm) + gr*( zmax - z(nm) )

   end do
 !--------------------------------------------------------

    write(*,*) " Spacings match, right?"
    write(*,*) " z(nm  ) - z(nm-1) = ", z(nm) - z(nm-kd1)
    write(*,*) " z(nm+1) - z(nm  ) = ", z(nm+kd1) - z(nm)

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
! End of Option 0
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
! Option 1: Simple stretching all the way to the other side of the boundary.
!
!   Spacing is detemined by a geometric sequence:
!
!    z(1) = zmin
!    z(k) = z(k-1) + dr1*r**(k-1), k = 2, 3,..., nz+1
!
!   which gives
!
!    z(k) = zmin + dr1*sum_{k=1}^{(nz+1)-1} r**k
!         = zmin + dr1*(r^(nz+1)-1)/(r-1)
!
!   The factor r will be determined for a given set of zmin, zmax, dr1, nz
!   by solving
!              zmax = zmin + dr1*(r^(nz+1)-1)/(r-1)
!   for r by Newton's method.
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

 elseif ( generate_pts_type == 1 ) then

    write(*,*)
    write(*,*) " --- Option 1: Simple stretching: z(k) = z(k-1) + dr1*gr**(k-1)"
    write(*,*)

  !Derive the factor gr by solving zmax = zmin + dr1*(gr^(nz+1)-1)/(gr-1).

    call compute_geometric_factor(nz+kd1,dr1,zmin,zmax, gr,itr,warning(3))

    write(*,*) "   Computed a good geometric factor: gr = ", gr

  !Generate z-coordinates:

    z(1) = zmin
   do k = 2, nz+kd1
    z(k) = z(k-1) + dr1*gr**(k-1)
   end do

  !z(nz+1) may not be exactly equal to zmax...

    write(*,*)
    write(*,*) "  z(nz+1) may not be exactly equal to zmax..."
    write(*,*) "  z(nz+1) = ", z(nz+kd1)
    write(*,*) "  zmax    = ", zmax

  !So, let's set z(nz+1) = zmax.

    z(nz+kd1) = zmax
    write(*,*)
    write(*,*) " Well, I'll set z(nz+1) = zmax = ", z(nz+kd1)
    write(*,*)

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
! End of Option 1: Simple stretching all the way to the other side of the boundary.
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

 else

  write(*,*) " Invalid input: generate_pts_type  = ", generate_pts_type
  write(*,*) " It must be 0 or 1. Stop..."
  stop

 endif generate_pts_in_z

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------



     write(*,*)
     write(*,*) " ---------- Finished: Generate points in z-direction ---------- "
     write(*,*)
 


!*******************************************************************************
!
! End of generating points in the z-coordinate direction.
!
!*******************************************************************************


!*******************************************************************************
!
! Start generating points in the x- and y-coordinate directions.
!
!*******************************************************************************

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
! OK, z is done. How about y and x?
! Well, copy z to y if viscous_wall=2 is requested.

  !-------------------------------------------------
  ! Default: 2 viscous walls in supersonic jet

   if (viscous_wall == 2) then

!   y = ymax - z !<- This will treat ymax as viscous wall. Not used.
    y = z

  !Or viscous bottom only: uniform in y.
   elseif (viscous_wall == 1) then

      y(1) = ymin
    do k = 2, ny+kd1
      y(k) = y(k-kd1) + (ymax-ymin)/real(ny,dp)
    end do

   endif

  !-------------------------------------------------
  ! Uniform in x.

      x(1) = xmin
    do k = 2, nx+kd1
      x(k) = x(k-kd1) + (xmax-xmin)/real(nx,dp)
    end do
     last_dx = (xmax-xmin)/real(nx,dp) !Uniform

  !-------------------------------------------------
  !-------------------------------------------------
  !-------------------------------------------------
  ! Adjust the domain in x-direction.

  !
  !                 Upstream region
  !               <------------------->
  !               o---o---o---o---o---x---o---o---o---o-----------
  ! xmin + upstream_x                xmin
  !
  ! The node at x=xmin will be chosen such that it exists on all coarser grids.


    izero = kd1

   create_upstream_inlet : if (upstream_create_region) then

    if (upstream_x > 0.0_dp) then
     write(*,*) " Invalid input: upstream_x must be negative... Stop."
     write(*,*) "                upstream_x      = ", upstream_x
     write(*,*)
     stop
    endif

    write(*,*)
    write(*,*)
    write(*,*) ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
    write(*,*) ">>>>>>>>> Upstream region is created: " 

     x_upstream = upstream_x
             dx = ( xmax-x_upstream )/real(nx,dp)
     write(*,*) ">>>>>>>>>        dx = ", dx

       if ( dx > abs(upstream_x) ) then
        write(*,*) " Invalid input: Not enough cells in x direction... Stop."
        write(*,*) "                       Increase nx and try gain..."
        stop
       endif

    !Estimate from xups+(i-1)*dx = 0 -> i = 1 + (-xups)/dx 
     izero = kd1 + int(abs(upstream_x)/dx)
     write(*,*) ">>>>>>>>> Estimate(izero)                   = ", izero
     write(*,*) ">>>>>>>>> Estimate( x_upstream + izero*dx ) = ", x_upstream + real(izero,dp)*dx

    !Find the first node that will remain to the coarsest, and close to the estimated node.
     do k = 1, 100
      if ( 2**(cglevels)*( (k+1) - 1 ) + 1 > izero ) then
       write(*,*) ">>>>>>>>> izero_candidate = ", 2**(cglevels)*( (k+1) - 1 ) + 1
       write(*,*) ">>>>>>>>> cells in inlet  = ", k, " on the coarsest lvl = cglevels+1 = ", cglevels+1
       izero = 2**(cglevels)*( (k+1) - 1 ) + 1
       exit
      endif
     end do

    !izero is the node at which we set x=0.0. This node will stay to the coarsest.
     write(*,*) ">>>>>>>>>         izero = ", izero
     write(*,*)

    !-------------------------------------------------------
    !-------------------------------------------------------
        x(1) = x_upstream
          dx = ( zero-x_upstream )/real(izero-kd1,dp)
      write(*,*) ">>>>>>>>>     dx(inlet)  = ", dx
      do k = 2, izero
        x(k) = x(k-kd1) + dx
      end do
    !-------------------------------------------------------
    !-------------------------------------------------------

      write(*,*) ">>>>>>>>>      x(izero)  = ", x(izero)
      write(*,*) ">>>>>>>>>     dx(duct )  = ", xmax/real(nx-(izero-kd1),dp)

    !-------------------------------------------------------
    ! Apply stretching for smoothness.

            sf0 = 2.0_dp !initial guess
      target_x  = dx     !target spacing
      ! we try to match this by cx*xi, where xi is the first point at k=nm+1:
             cx = xmax
             xi = real(kd1,dp)/real(nx-(izero-kd1),dp)

       write(*,*)
       write(*,*) " Calling stretching-factor solver..."
       call compute_stretching_factor(target_x,cx,xi,sf0, current_x,sf,itr, warning(3))
       write(*,*)
       write(*,*) "Actual stretching factor = ", sf
       write(*,*) "           at iterations = ", itr
       write(*,*) "               dx(inlet) = ", target_x
       write(*,*) "               dx1(duct) = ", current_x
       write(*,*)

    !-------------------------------------------------------
    !-------------------------------------------------------
      do k = 1, nx-(izero-kd1)
                xi = real(k,dp)/real(nx-(izero-kd1),dp)
                gr = (one-exp(sf*xi))/(one-exp(sf))
        x(k+izero) = zero + xmax * gr
!       x(k+izero) = zero + xmax * xi !No stretching
      end do
      last_dx = x(nx-(izero-kd1)+izero) - x(nx-(izero-kd1)+izero -kd1)
    !-------------------------------------------------------
    !-------------------------------------------------------

    write(*,*) ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
    write(*,*)
    write(*,*)

   endif create_upstream_inlet
  !-------------------------------------------------
  !-------------------------------------------------
  !-------------------------------------------------

   izero_0 = izero

  !-------------------------------------------------
  !-------------------------------------------------
  !-------------------------------------------------
  ! Add the downstream outlet region with almost uniform spacing of last_dx.

  !                     Downstream region
  !                    <----------------->
  ! ----o----o----o----x----o---o--o-o-o-o
  !                  xmax               xmax + downstream_x
  !               <---> <--->
  !                 dx1   dx2=dx1*downstream_spacing_ratio
  !
  ! The # of element in the downstream region will be chosen such
  ! that it exists on all coarser grids.

   ixmax   = nx+1
   ixmax_0 = nx+1

   create_downstream_outlet : if (downstream_create_region) then
    
    if (downstream_x <= 0.0_dp) then
     write(*,*) " Invalid input: downstream_x must be positive... Stop."
     write(*,*) "                downstream_x      = ", downstream_x
     write(*,*)
     stop
    endif

    write(*,*)
    write(*,*)
    write(*,*) ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
    write(*,*) ">>>>>>>>> Downstream region is created: " 

   !Estimated # of elements in x for uniform spacing.
    nx_out = int( downstream_x / last_dx ) + 1
    write(*,*) "   # of elements in x (uniform) = ", nx_out

    !Find the first node that will remain to the coarsest, and close to the estimated node.
     do k = 1, 100
      if ( 2**(cglevels)*( (k+1) - 1 ) + 1 > nx_out ) then
       write(*,*) ">>>>>>>>> izero_candidate = ", 2**(cglevels)*( (k+1) - 1 ) + 1
       write(*,*) ">>>>>>>>> cells in inlet  = ", k, " on the coarsest lvl = cglevels+1 = ", cglevels+1
       nx_out = 2**(cglevels)*( (k+1) - 1 ) + 1
       exit
      endif
     end do

    !# of nodes -> # of elements
     nx_out = nx_out - 1

     write(*,*) ">>>>>>>>>         nx_out = ", nx_out
     write(*,*)


   !Need to expand the array, x(:), to accomodate the additional cells in the extra region.
    allocate(x_temp(nx+kd1))
    x_temp = x
    deallocate(x)
      allocate(x(nx+kd1+nx_out)) !New grid has nx+nx_out elements in x-direction.
    x(1:nx+kd1) = x_temp(1:nx+kd1)
    deallocate(x_temp)

    !-------------------------------------------------------
    ! Apply stretching for smoothness.

            sf0 = 1.0_dp                           !initial guess
      target_x  = last_dx*downstream_spacing_ratio !target spacing
      ! we try to match this by cx*xi, where xi is the first point at k=nm+1:
             cx = downstream_x
             xi = real(kd1,dp)/real(nx_out,dp)

       write(*,*)
       write(*,*) " Calling stretching-factor solver..."
       call compute_stretching_factor(target_x,cx,xi,sf0, current_x,sf,itr, warning(4))
       write(*,*)
       write(*,*) "Actual stretching factor = ", sf
       write(*,*) "           at iterations = ", itr
       write(*,*) "                dx(duct) = ", target_x
       write(*,*) "             dx1(outlet) = ", current_x
       write(*,*)

      do k = 1, nx_out
                xi = real(k,dp)/real(nx_out,dp)
                gr = (one-exp(sf*xi))/(one-exp(sf))
        x(k+nx+kd1) = xmax + downstream_x * gr
      end do

    write(*,*) "           Original nx = ", nx
    write(*,*) "   # of elms in outlet = ", nx_out
    nx = nx + nx_out
    write(*,*) "           Updated  nx = ", nx

    write(*,*) ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
    write(*,*)
    write(*,*)

   endif create_downstream_outlet

!-------------------------------------------------------------------------------
! Checking the data.

  if (debug_mode) then

   do k = 2, nz+kd1
    if ( z(k)-z(k-kd1) < 0.0_dp ) then
     write(*,*) " z-coordinates are not valid: "
     write(*,*) "       k = ", k    
     write(*,*) "  z(k)   = ", z(k)
     write(*,*) "  z(k-1) = ", z(k-kd1)
     write(*,*) "      nm = ", nm
     stop
    endif
   end do

  endif

!*******************************************************************************
!
! End of Start generating points in the x- and y-coordinate directions.
!
!*******************************************************************************






  !*************************************************************
  ! Write a Tecplot file for the box boundary grid.
  !*************************************************************
   debug_mode_01 : if (debug_mode) then
    call write_tecplot_boundary_file_hex(kd1,filename01)
   endif debug_mode_01
  !*************************************************************






!*******************************************************************************
!
! Generate hexahedral elements and write grid files.
!
!*******************************************************************************

!-----------------------------------------------------------------------------
! - ugrid_file_unformatted = Grid file format - T: unformatted, F: formatted

   if (ugrid_file_unformatted) then
    if ( big_endian_io(9999) ) then
      write(*,*) 'The system is big Endian'
      write(*,*) ' Ensure big Endian -> setenv F_UFMTENDIAN big'
    else
      write(*,*) 'The system is little Endian'
      write(*,*) ' Ensure little Endian -> setenv F_UFMTENDIAN little'
    endif
   else
      write(*,*) ' Using ascii type for ugrid file'
   endif

!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------
! Generate grids
!
!  glevel = 0:     finest grid with (nx  ,ny  ,nz)
!  glevel = 1: 1st coarse grid with (nx/2,ny/2,nz/2)
!  glevel = 2: 2nd coarse grid with (nx/4,ny/4,nz/4)
!   .
!   .
!
!  until not possible (i.e., stop when any of the dimension becomes odd).
!  So, the coarsest grid will have at least one of (nx,ny,nz) odd.
!
!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------

 cgrid_loop : do glevel = 0, cglevels

 write(*,*)
 write(*,*)
 write(*,*)
 write(*,*) "------------------------------------------------------------------"
 write(*,*) "------------------------------------------------------------------"
 write(*,*) "  Grid level = ", glevel+kd1
 write(*,*) "------------------------------------------------------------------"
 write(*,*) "------------------------------------------------------------------"
 write(*,*)

  iskip = 2**glevel

  write(*,*)
  write(*,*) " Grid dimension: "
  write(*,*) " nx (cells)       = ", nx/iskip
  write(*,*) " ny (cells)       = ", ny/iskip
  write(*,*) " nz (cells)       = ", nz/iskip

  if (generate_pts_type==0) then
  write(*,*) " nm (cells in bl) = ", (nm-kd1)/iskip
  endif

 write(*,*)
 write(*,*) " Previous izero = ", izero
 izero =      (izero_0-kd1)/iskip + kd1
 write(*,*) " Adjusted izero = ", izero
 write(*,*)

 write(*,*)
 write(*,*) " Previous ixmax = ", ixmax
 ixmax = (nx/iskip+kd1) - ( (nx+kd1)-ixmax_0)/iskip
 write(*,*) " Adjusted ixmax = ", ixmax
 write(*,*)


  write( glevel_char, '(i0)' ) glevel+kd1

  id = glevel+kd1

   filename_mapbc(id)        = 'duct_' // trim(elmtype) // trim(".") // trim(glevel_char) // '.mapbc'
   filename_lines_z(id)      = 'duct_' // trim(elmtype) // trim(".") // trim(glevel_char) // '.lines_z_fmt'
   filename_lines_z_all(id)  = 'duct_' // trim(elmtype) // trim(".") // trim(glevel_char) // '.lines_z_fmt_all'
   filename_lines_cz(id)     = 'duct_' // trim(elmtype) // trim(".") // trim(glevel_char) // '.lines_z_fmt_cc'
   filename_lines_cz_all(id) = 'duct_' // trim(elmtype) // trim(".") // trim(glevel_char) // '.lines_z_fmt_cc_all'

   filename_lines_y(id)      = 'duct_' // trim(elmtype) // trim(".") // trim(glevel_char) // '.lines_y_fmt'
   filename_lines_y_all(id)  = 'duct_' // trim(elmtype) // trim(".") // trim(glevel_char) // '.lines_y_fmt_all'
   filename_lines_cy(id)     = 'duct_' // trim(elmtype) // trim(".") // trim(glevel_char) // '.lines_y_fmt_cc'
   filename_lines_cy_all(id) = 'duct_' // trim(elmtype) // trim(".") // trim(glevel_char) // '.lines_y_fmt_cc_all'

   if ( ugrid_file_unformatted ) then
     if ( big_endian_io(9999) ) then
      filename_ugrid(id)  = 'duct_' // trim(elmtype) // trim(".") // trim(glevel_char) // '.b8.ugrid'
     else
      filename_ugrid(id)  = 'duct_' // trim(elmtype) // trim(".") // trim(glevel_char) // '.lb8.ugrid'
     end if
   else
      filename_ugrid(id)  = 'duct_' // trim(elmtype) // trim(".") // trim(glevel_char) // '.ugrid'
   endif

   if (p3d_file_unformatted) then
     filename_p3d(id)     = 'duct_' // trim(elmtype) // trim(".") // trim(glevel_char) // '.ufmt'
   else
     filename_p3d(id)     = 'duct_' // trim(elmtype) // trim(".") // trim(glevel_char) // '.p3d'
   endif
     filename_p3d_nmf(id) = 'duct_' // trim(elmtype) // trim(".") // trim(glevel_char) // '.nmf'

     filename_su2(id)     = 'duct_' // trim(elmtype) // trim(".") // trim(glevel_char) // '.su2'

   filename_tecplot_b(id) = 'duct_' // trim(elmtype) // trim(".") // trim(glevel_char) // '.tec_bndary.dat'
   filename_tecplot_v(id) = 'duct_' // trim(elmtype) // trim(".") // trim(glevel_char) // '.tec_volume.dat'


  write(*,*)
  write(*,*)
  write(*,*) "--------------------------------------------"
  write(*,*) ">>> Generate requested grid files..."
  write(*,*)

 !---------------------------------------------------------------------------------
 ! Tecplot boundary file:

   if (generate_tec_file_b) then
    write(*,*)
    write(*,*) " writing tecplot boundary-grid file: ", trim(filename_tecplot_b(id))
    if (igrid_type==1) call write_tecplot_boundary_file_hex( iskip, filename_tecplot_b(id) )
    if (igrid_type==2) call write_tecplot_boundary_file_prs( iskip, filename_tecplot_b(id) )
    if (igrid_type==3) call write_tecplot_boundary_file_tet( iskip, filename_tecplot_b(id) )
   endif

 !---------------------------------------------------------------------------------
 ! Tecplot volume file:

   if (generate_tec_file_v) then
    write(*,*)
    write(*,*) " writing tecplot volume-grid file: ", trim(filename_tecplot_v(id))
    if (igrid_type==1) call write_tecplot_volume_file_hex( iskip, filename_tecplot_v(id) )
    if (igrid_type==2) call write_tecplot_volume_file_prs( iskip, filename_tecplot_v(id) )
    if (igrid_type==3) call write_tecplot_volume_file_tet( iskip, filename_tecplot_v(id) )
   endif

 !---------------------------------------------------------------------------------
 ! UGRID file:

   if (generate_ugrid_file) then
    write(*,*)
    write(*,*) " writing .ugrid file: ", trim(filename_ugrid(id))
    if (igrid_type==1) call write_ugrid_file_hex( iskip, filename_ugrid(id) )
    if (igrid_type==2) call write_ugrid_file_prs( iskip, filename_ugrid(id) )
    if (igrid_type==3) call write_ugrid_file_tet( iskip, filename_ugrid(id) )
   endif

 !---------------------------------------------------------------------------------
 ! SU2 file:

   if (generate_su2grid_file) then
    write(*,*)
    write(*,*) " writing SU2 grid file: ", trim(filename_su2(id))
    if (igrid_type==1) call write_su2grid_file_hex(iskip,filename_su2(id))
    if (igrid_type==2) call write_su2grid_file_prs(iskip,filename_su2(id))
    if (igrid_type==3) call write_su2grid_file_tet(iskip,filename_su2(id))
   endif

 !---------------------------------------------------------------------------------
 ! PLOT3D file:

   if (generate_p3d_file .and. igrid_type==1) then
    write(*,*)
    write(*,*) " writing PLOT3D file: ", trim(filename_p3d(id))
    call write_plot3d_file(iskip,filename_p3d(id),filename_p3d_nmf(id))
   else
    write(*,*) " Skip writing PLOT3D file for non-hex grids."
   endif

 !---------------------------------------------------------------------------------
 ! Lines for nodes. These are independent of element types.
 !
  if( generate_line_file_nc ) then

  write(*,*)
  write(*,*)
  write(*,*) "--------------------------------------------"
  write(*,*) ">>> Write node-line files for nodes..."
  write(*,*)

    write(*,*)
    write(*,*) " Generating a node-line file for z-direction all the way:"
     call line_info_file_bottom( nz/iskip+kd1,iskip, filename_lines_z_all(id) )

    write(*,*)
    write(*,*) " Generating a node-line file for y-direction all the way:"
     call line_info_file_left(   ny/iskip+kd1,iskip, filename_lines_y_all(id) )

  !nm=pts in bl region.
   if ( (nm-kd1)/iskip+kd1 > 1) then

    write(*,*)
    write(*,*) " Generating a node-line file for z-direction within BL:"
     call line_info_file_bottom(  (nm-kd1)/iskip+kd1  ,iskip, filename_lines_z(id) )

    write(*,*)
    write(*,*) " Generating a node-line file for y-direction within B:"
     call line_info_file_left(    (nm-kd1)/iskip+kd1  ,iskip, filename_lines_y(id) )

   else

    if ( generate_pts_type == 0 ) then

     write(*,*)
     write(*,*) " Generating a node-line file for z-direction within BL:"
      call line_info_file_bottom(                  kd1 ,iskip, filename_lines_z(id) ) ! 1 pt

     write(*,*)
     write(*,*) " Generating a node-line file for y-direction within B:"
      call line_info_file_left(                    kd1 ,iskip, filename_lines_y(id) ) ! 1 pt

    endif

   endif

  else

   write(*,*)
   write(*,*) " Skip writing node-line files for nodes."
   write(*,*)

  endif

 !---------------------------------------------------------------------------------
 ! Lines for cells. These are dependent of element types.

  if( generate_line_file_cc ) then

   if (igrid_type==1) then

    write(*,*)
    write(*,*)
    write(*,*) "--------------------------------------------"
    write(*,*) ">>> Write cell-line files for cells..."
    write(*,*)

     write(*,*)
     write(*,*) " Generating a cell-line file for z-direction all the way:"
     call line_info_file_c_bottom_hex(   nz/iskip  ,iskip, filename_lines_cz_all(id) )

     write(*,*)
     write(*,*) " Generating a cell-line file for y-direction all the way:"
     call line_info_file_c_left_hex(     ny/iskip  ,iskip, filename_lines_cy_all(id) )

  !nm=pts in bl region -> nm-1 cells
   if ((nm-kd1)/iskip > 1) then

     write(*,*)
     write(*,*) " Generating a cell-line file for z-direction within BL:"
     call line_info_file_c_bottom_hex( (nm-kd1)/iskip,iskip, filename_lines_cz(id)    )

     write(*,*)
     write(*,*) " Generating a cell-line file for y-direction within B:"
     call line_info_file_c_left_hex(   (nm-kd1)/iskip,iskip, filename_lines_cy(id)    )

   else

    if ( generate_pts_type == 0 ) then

     write(*,*)
     write(*,*) " Generating a cell-line file for z-direction within BL:"
      call line_info_file_c_bottom_hex(           kd1 ,iskip, filename_lines_cz(id)    ) ! 1 cell

     write(*,*)
     write(*,*) " Generating a cell-line file for y-direction within B:"
      call line_info_file_c_left_hex(             kd1 ,iskip, filename_lines_cy(id)    ) ! 1 cell
     endif

    endif

   else

    write(*,*)
    write(*,*) "--------------------------------------------"
    write(*,*) ">>> Skip writing cell-line files for cells other than hex (for now)..."
    write(*,*)

   endif

  else

   write(*,*)
   write(*,*) " Skip writing cell-line files for cells."
   write(*,*)

  endif

 !---------------------------------------------------------------------------------

  write(*,*)
  write(*,*) "--------------------------------------------"
  write(*,*) ">>> Write BC file..."

   write(*,*)
   write(*,*) " writing .mapbc file: ", trim(filename_mapbc(id))
  open(unit=16, file=filename_mapbc(id), status="unknown", iostat=os)

   if (upstream_create_region .and. downstream_create_region) then

    write(16,'(a)') "14                  !Number of boundary parts (boundary conditions)"
    write(16,'(a)') "1  4000   duct_zmin !4000 = viscous wall BC in FUN3D"
    write(16,'(a)') "2  6023   duct_zmax !6023 =   z-symmetry BC in FUN3D"
    write(16,'(a)') "3  4000   duct_ymin !4000 = viscous wall BC in FUN3D"
    write(16,'(a)') "4  6022   duct_ymax !6022 =   y-symmetry BC in FUN3D"
    write(16,'(a)') "5  5050   duct_xmin !5050 =   freestream BC in FUN3D"
    write(16,'(a)') "6  5051   duct_xmax !5051 =      outflow BC in FUN3D"
    write(16,'(a)') "7  6023  inlet_zmin !6023 =   z-symmetry BC in FUN3D"
    write(16,'(a)') "8  6023  inlet_zmax !6023 =   z-symmetry BC in FUN3D"
    write(16,'(a)') "9  6022  inlet_ymin !6022 =   y-symmetry BC in FUN3D"
    write(16,'(a)') "10 6022  inlet_ymax !6022 =   y-symmetry BC in FUN3D"
    write(16,'(a)') "11 6023 outlet_zmin !6023 =   z-symmetry BC in FUN3D"
    write(16,'(a)') "12 6023 outlet_zmax !6023 =   z-symmetry BC in FUN3D"
    write(16,'(a)') "13 6022 outlet_ymin !6022 =   y-symmetry BC in FUN3D"
    write(16,'(a)') "14 6022 outlet_ymax !6022 =   y-symmetry BC in FUN3D"

   elseif (upstream_create_region .and. .not.downstream_create_region) then

    write(16,'(a)') "10                 !Number of boundary parts (boundary conditions)"
    write(16,'(a)') "1  4000  duct_zmin !4000 = viscous wall BC in FUN3D"
    write(16,'(a)') "2  6023  duct_zmax !6023 =   z-symmetry BC in FUN3D"
    write(16,'(a)') "3  4000  duct_ymin !4000 = viscous wall BC in FUN3D"
    write(16,'(a)') "4  6022  duct_ymax !6022 =   y-symmetry BC in FUN3D"
    write(16,'(a)') "5  5050  duct_xmin !5050 =   freestream BC in FUN3D"
    write(16,'(a)') "6  5051  duct_xmax !5051 =      outflow BC in FUN3D"
    write(16,'(a)') "7  6023 inlet_zmin !6023 =   z-symmetry BC in FUN3D"
    write(16,'(a)') "8  6023 inlet_zmax !6023 =   z-symmetry BC in FUN3D"
    write(16,'(a)') "9  6022 inlet_ymin !6022 =   y-symmetry BC in FUN3D"
    write(16,'(a)') "10 6022 inlet_ymax !6022 =   y-symmetry BC in FUN3D"

   elseif (.not.upstream_create_region .and. downstream_create_region) then

    write(16,'(a)') "10                  !Number of boundary parts (boundary conditions)"
    write(16,'(a)') "1  4000   duct_zmin !4000 = viscous wall BC in FUN3D"
    write(16,'(a)') "2  6023   duct_zmax !6023 =   z-symmetry BC in FUN3D"
    write(16,'(a)') "3  4000   duct_ymin !4000 = viscous wall BC in FUN3D"
    write(16,'(a)') "4  6022   duct_ymax !6022 =   y-symmetry BC in FUN3D"
    write(16,'(a)') "5  5050   duct_xmin !5050 =   freestream BC in FUN3D"
    write(16,'(a)') "6  5051   duct_xmax !5051 =      outflow BC in FUN3D"
    write(16,'(a)') "7  6023 outlet_zmin !6023 =   z-symmetry BC in FUN3D"
    write(16,'(a)') "8  6023 outlet_zmax !6023 =   z-symmetry BC in FUN3D"
    write(16,'(a)') "9  6022 outlet_ymin !6022 =   y-symmetry BC in FUN3D"
    write(16,'(a)') "10 6022 outlet_ymax !6022 =   y-symmetry BC in FUN3D"

  !No inlet and outlet.
   else

    write(16,'(a)') "6                !Number of boundary parts (boundary conditions)"
    write(16,'(a)') "1 4000 duct_zmin !4000 = viscous wall BC in FUN3D"
    write(16,'(a)') "2 6023 duct_zmax !6023 =   z-symmetry BC in FUN3D"
    write(16,'(a)') "3 4000 duct_ymin !4000 = viscous wall BC in FUN3D"
    write(16,'(a)') "4 6022 duct_ymax !6022 =   y-symmetry BC in FUN3D"
    write(16,'(a)') "5 5050 duct_xmin !5050 =   freestream BC in FUN3D"
    write(16,'(a)') "6 5051 duct_xmax !5051 =      outflow BC in FUN3D"

   endif

  close(16)

 write(*,*)
 write(*,*)
 write(*,*) "------------------------------------------------------------------"
 write(*,*) "------------------------------------------------------------------"
 write(*,*) "------------------------------------------------------------------"
 write(*,*)

 end do cgrid_loop

!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------
! End of Generate grids
!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------

 if (cglevels==0) then
  write(*,*)
  write(*,*) "Regular coarsening is not possible."
  write(*,*)
 endif

!*******************************************************************************
!
! End of Generate hexahedral elements and write grid files.
!
!*******************************************************************************

!*******************************************************************************
!
! Sumamry
!
!*******************************************************************************

   write(*,*)
   write(*,*) "--------------------------------------------------------------  "
   write(*,*) "----   Summary                               -----------------  "
   write(*,*) "--------------------------------------------------------------  "

 write(*,*)
 write(*,*) " Total hexa grids generated = ", cglevels+1
 write(*,*)

 do glevel = 1, cglevels+1

  write(*,'(a10,i3,3(a,i12))') " level = ", glevel, ": nx=",nx/2**(glevel-1), &
                                                   "   ny=",ny/2**(glevel-1), &
                                                   "   nz=",nz/2**(glevel-1)

 end do

!-------------------------------------------------

  write(*,*)
  write(*,*) " First off-the-wall spacing: "
  write(*,*)

  do glevel = 1, cglevels+1
   write(*,*) " level = ", glevel,"  dz=dy = ", z( 1+2**(glevel-1) ) - z(1)
  end do

!-------------------------------------------------

  write(*,*)
  write(*,*) " BC files generated: "
  write(*,*)

  do glevel = 1, cglevels+1
   write(*,*) "     ", trim(filename_mapbc(glevel))
  end do

!-------------------------------------------------
 if (generate_tec_file_b) then

  write(*,*)
  write(*,*) " Tecplot boundary-grid files generated: "
  write(*,*)

  do glevel = 1, cglevels+1
   write(*,*) "     ", trim(filename_tecplot_b(glevel))
  end do

 endif

!-------------------------------------------------
 if (generate_tec_file_v) then

  write(*,*)
  write(*,*) " Tecplot volume-grid files generated: "
  write(*,*)

  do glevel = 1, cglevels+1
   write(*,*) "     ", trim(filename_tecplot_v(glevel))
  end do

 endif

!-------------------------------------------------
 if (generate_ugrid_file) then

  write(*,*)
  write(*,*) " .ugrid grid files generated: "
  write(*,*)

  do glevel = 1, cglevels+1
   write(*,*) "     ", trim(filename_ugrid(glevel))
  end do

 endif

!-------------------------------------------------
 if (generate_p3d_file) then

  write(*,*)
  write(*,*) " PLOT3D grid files generated: "
  write(*,*)

  do glevel = 1, cglevels+1
   write(*,*) "     ", trim(filename_p3d(glevel)), "   ", trim(filename_p3d_nmf(glevel))
  end do

 endif

!-------------------------------------------------
 if (generate_su2grid_file) then

  write(*,*)
  write(*,*) " PLOT3D grid files generated: "
  write(*,*)

  do glevel = 1, cglevels+1
   write(*,*) "     ", trim(filename_su2(glevel))
  end do

 endif

!-------------------------------------------------

 if( generate_line_file_nc ) then

  write(*,*)
  write(*,*) " Node line files generated: within BL and all across the domain."

  write(*,*)
  write(*,*) "   Lines in z-direction:"
  write(*,*)

  do glevel = 1, cglevels+1
   write(*,*) "     ", trim(filename_lines_z(glevel))    , "  ",&
                       trim(filename_lines_z_all(glevel))
  end do

  write(*,*)
  write(*,*) "   Lines in y-direction:"
  write(*,*)

  do glevel = 1, cglevels+1
   write(*,*) "     ", trim(filename_lines_y(glevel))    , "  ",&
                       trim(filename_lines_y_all(glevel))
  end do

 endif

!-------------------------------------------------

 if( generate_line_file_cc ) then

  write(*,*)
  write(*,*) " Cell line files generated: within BL and all across the domain."

  write(*,*)
  write(*,*) "   Lines in z-direction:"
  write(*,*)

  do glevel = 1, cglevels+1
   write(*,*) "     ", trim(filename_lines_cz(glevel))    , "  ",&
                       trim(filename_lines_cz_all(glevel))
  end do

  write(*,*)
  write(*,*) "   Lines in y-direction:"
  write(*,*)

  do glevel = 1, cglevels+1
   write(*,*) "     ", trim(filename_lines_cy(glevel))    , "  ",&
                       trim(filename_lines_cy_all(glevel))
  end do

 endif

!*******************************************************************************
!
! End of Sumamry
!
!*******************************************************************************

!*******************************************************************************
!
! Warning messages
!
!*******************************************************************************

 if     ( warning(1) == 1 ) then
  write(*,*) "Warning >>>>> BL thickness estimate was too small: rmax < dr1..."
 endif

 if     ( warning(2) == 1 ) then
  write(*,*) "Warning >>>>> Stretching-factor solver didn't converge for BL region..."
  write(*,*) " stretching factor = ",  sfactor(1)
 elseif ( warning(2) == 2 ) then
  write(*,*) "Warning >>>>> Negative stretching-factor for BL region..."
  write(*,*) " stretching factor = ",  sfactor(1)
 endif

 if     ( warning(3) == 1 ) then
  write(*,*) "Warning >>>>> Stretching-factor solver didn't converge for outer region..."
  write(*,*) " stretching factor = ",  sfactor(2)
 elseif ( warning(3) == 2 ) then
  write(*,*) "Warning >>>>> Negative stretching-factor for outer region..."
  write(*,*) " stretching factor = ",  sfactor(2)
 endif

!*******************************************************************************
!
! Finish.
!
!*******************************************************************************

 write(*,*)
 write(*,*) "Congratulations!"
 write(*,*) "Grid generation successfully completed."

 stop

contains 

!*******************************************************************************
!
! Find the node number in the lexicographic ordering in a 2D quad grid with
! dimension (nx+1,jmax+1).
!
!*******************************************************************************
 function node_number2d(i,j,nx)

 implicit none

 integer(kd) :: i, j, nx, node_number2d

  node_number2d = i + (j-kd1)*(nx+kd1) !kd1 = 1

 end function node_number2d

!*******************************************************************************
!
! Find the node number in the lexicographic ordering in a 3D quad grid with
! dimension (nx+1,ny+1,kmax+1).
!
!*******************************************************************************
 function node_number(i,j,k,nx,ny)

 implicit none

 integer(kd) :: i, j, k, nx, ny, node_number

  node_number = i + (j-kd1)*(nx+kd1) + (k-kd1)*(nx+kd1)*(ny+kd1) !kd1 = 1

 end function node_number


!*******************************************************************************
!
! This subroutine writes a Tecplot file for boundaries.
!
!******************************************************************************
 subroutine write_tecplot_boundary_file_hex(inc,filename)

 implicit none
 integer(kd)  , intent(in) :: inc
 character(80), intent(in) :: filename
 integer(kd)               :: nnodes, nquads, i, j, k
 integer(kd)               :: nxp, nyp, nzp


 open(unit=2, file=filename, status="unknown", iostat=os)
 write(2,'(2a)') 'variables = "x","y","z"'

  nxp = nx / inc
  nyp = ny / inc
  nzp = nz / inc

 !----------------------------------------------------------------------------
 ! Constant z planes
 !----------------------------------------------------------------------------
  nnodes = (nxp+kd1)*(nyp+kd1)
  nquads = (nxp - (izero-1) - (nxp+1-ixmax) )*nyp
  write(2,*) 'ZONE T="Boundary 1 (Bottom)"  N=', nnodes,',E=', nquads, &
              ' , ET=quadrilateral, F=FEPOINT'

   k = kd1

  do j = kd1, ny+kd1, inc
   do i = kd1, nx+kd1, inc
    write(2,'(3es20.10)') x(i), y(j), z(k)
   end do
  end do

  do j = kd1, nyp
   do i = izero, ixmax-1 !nxp
    write(2,'(4i10)') node_number2d(i    ,j    ,nxp), node_number2d(i+kd1,j    ,nxp), &
                      node_number2d(i+kd1,j+kd1,nxp), node_number2d(i    ,j+kd1,nxp)
   end do
  end do

 !----------------------------------------------------------------------------
  nnodes = (nxp+kd1)*(nyp+kd1)
  nquads = (nxp - (izero-1) - (nxp+1-ixmax))*nyp
  write(2,*) 'ZONE T="Boundary 2 (Top)"  N=', nnodes,',E=', nquads, &
              ' , ET=quadrilateral, F=FEPOINT'

   k = nz+kd1

  do j = kd1, ny+kd1, inc
   do i = kd1, nx+kd1, inc
    write(2,'(3es20.10)') x(i), y(j), z(k)
   end do
  end do

  do j = kd1, nyp
   do i = izero, ixmax-1 !, nxp
    write(2,'(4i10)') node_number2d(i  ,j  ,nxp), node_number2d(i+kd1,j  ,nxp), &
                      node_number2d(i+kd1,j+kd1,nxp), node_number2d(i  ,j+kd1,nxp)
   end do
  end do


 !----------------------------------------------------------------------------
 ! Constant y planes
 !----------------------------------------------------------------------------
  nnodes = (nxp+kd1)*(nzp+kd1)
  nquads = (nxp - (izero-1) - (nxp+1-ixmax))*nzp
  write(2,*) 'ZONE T="Boundary 3 (Right)"  N=', nnodes,',E=', nquads, &
              ' , ET=quadrilateral, F=FEPOINT'

   j = kd1

  do k = kd1, nz+kd1, inc
   do i = kd1, nx+kd1, inc
    write(2,'(3es20.10)') x(i), y(j), z(k)
   end do
  end do

  do k = kd1, nzp
   do i = izero, ixmax-1 !, nxp
    write(2,'(4i10)') node_number2d(i    ,k    ,nxp), node_number2d(i+kd1,k    ,nxp), &
                      node_number2d(i+kd1,k+kd1,nxp), node_number2d(i    ,k+kd1,nxp)
   end do
  end do

 !----------------------------------------------------------------------------
  nnodes = (nxp+kd1)*(nzp+kd1)
  nquads = (nxp - (izero-1) - (nxp+1-ixmax))*nzp
  write(2,*) 'ZONE T="Boundary 4 (Left)"  N=', nnodes,',E=', nquads, &
              ' , ET=quadrilateral, F=FEPOINT'

   j = ny+kd1

  do k = kd1, nz+kd1, inc
   do i = kd1, nx+kd1, inc
    write(2,'(3es20.10)') x(i), y(j), z(k)
   end do
  end do

  do k = kd1, nzp
   do i = izero, ixmax-1 !, nxp
    write(2,'(4i10)') node_number2d(i    ,k    ,nxp), node_number2d(i+kd1,k    ,nxp), &
                      node_number2d(i+kd1,k+kd1,nxp), node_number2d(i    ,k+kd1,nxp)
   end do
  end do

 !----------------------------------------------------------------------------
 ! Constant x planes
 !----------------------------------------------------------------------------
  nnodes = (nyp+kd1)*(nzp+kd1)
  nquads = nyp*nzp
  write(2,*) 'ZONE T="Boundary 5 (Near)"  N=', nnodes,',E=', nquads, &
              ' , ET=quadrilateral, F=FEPOINT'

   i = kd1

  do k = kd1, nz+kd1, inc
   do j = kd1, ny+kd1, inc
    write(2,'(3es20.10)') x(i), y(j), z(k)
   end do
  end do

  do k = kd1, nzp
   do j = kd1, nyp
    write(2,'(4i10)') node_number2d(j    ,k    ,nyp), node_number2d(j+kd1,k    ,nyp), &
                      node_number2d(j+kd1,k+kd1,nyp), node_number2d(j    ,k+kd1,nyp)
   end do
  end do

 !----------------------------------------------------------------------------
  nnodes = (nyp+kd1)*(nzp+kd1)
  nquads = nyp*nzp
  write(2,*) 'ZONE T="Boundary 6 (Far)"  N=', nnodes,',E=', nquads, &
              ' , ET=quadrilateral, F=FEPOINT'

   i = nx+kd1

  do k = kd1, nz+kd1, inc
   do j = kd1, ny+kd1, inc
    write(2,'(3es20.10)') x(i), y(j), z(k)
   end do
  end do

  do k = kd1, nzp
   do j = kd1, nyp
    write(2,'(4i10)') node_number2d(j    ,k    ,nyp), node_number2d(j+kd1,k    ,nyp), &
                      node_number2d(j+kd1,k+kd1,nyp), node_number2d(j    ,k+kd1,nyp)
   end do
  end do


 !--------------------------------------------------------------------------------------
 !--------------------------------------------------------------------------------------
 ! Upstream (inlet) region
 !--------------------------------------------------------------------------------------
 !--------------------------------------------------------------------------------------
 if (izero > 1) then

 !----------------------------------------------------------------------------
   nnodes = (nxp+kd1)*(nyp+kd1)
   nquads = (izero-1)*nyp
   write(2,*) 'ZONE T="Boundary 1 (Inlet: Bottom)"  N=', nnodes,',E=', nquads, &
               ' , ET=quadrilateral, F=FEPOINT'

    k = kd1

   do j = kd1, ny+kd1, inc
    do i = kd1, nx+kd1, inc
     write(2,'(3es20.10)') x(i), y(j), z(k)
    end do
   end do

    do j = kd1, nyp
    do i = kd1, izero-1
     write(2,'(4i10)') node_number2d(i    ,j    ,nxp), node_number2d(i+kd1,j    ,nxp), &
                       node_number2d(i+kd1,j+kd1,nxp), node_number2d(i    ,j+kd1,nxp)
    end do
    end do

 !----------------------------------------------------------------------------
   nnodes = (nxp+kd1)*(nyp+kd1)
   nquads = (izero-1)*nyp
   write(2,*) 'ZONE T="Boundary 2 (Inlet: Top)"  N=', nnodes,',E=', nquads, &
               ' , ET=quadrilateral, F=FEPOINT'

    k = nz+kd1

   do j = kd1, ny+kd1, inc
    do i = kd1, nx+kd1, inc
     write(2,'(3es20.10)') x(i), y(j), z(k)
    end do
   end do

   do j = kd1, nyp
    do i = kd1, izero-1
     write(2,'(4i10)') node_number2d(i  ,j  ,nxp), node_number2d(i+kd1,j  ,nxp), &
                       node_number2d(i+kd1,j+kd1,nxp), node_number2d(i  ,j+kd1,nxp)
    end do
   end do

 !----------------------------------------------------------------------------
   nnodes = (nxp+kd1)*(nzp+kd1)
   nquads = (izero-1)*nzp
   write(2,*) 'ZONE T="Boundary 3 (Inlet: Right)"  N=', nnodes,',E=', nquads, &
               ' , ET=quadrilateral, F=FEPOINT'

    j = kd1

   do k = kd1, nz+kd1, inc
    do i = kd1, nx+kd1, inc
     write(2,'(3es20.10)') x(i), y(j), z(k)
    end do
   end do

   do k = kd1, nzp
    do i = kd1, izero-1
     write(2,'(4i10)') node_number2d(i    ,k    ,nxp), node_number2d(i+kd1,k    ,nxp), &
                       node_number2d(i+kd1,k+kd1,nxp), node_number2d(i    ,k+kd1,nxp)
    end do
   end do

 !----------------------------------------------------------------------------
   nnodes = (nxp+kd1)*(nzp+kd1)
   nquads = (izero-1)*nzp
   write(2,*) 'ZONE T="Boundary 4 (Inlet: Left)"  N=', nnodes,',E=', nquads, &
               ' , ET=quadrilateral, F=FEPOINT'

    j = ny+kd1

   do k = kd1, nz+kd1, inc
    do i = kd1, nx+kd1, inc
     write(2,'(3es20.10)') x(i), y(j), z(k)
    end do
   end do

   do k = kd1, nzp
    do i = kd1, izero-1
     write(2,'(4i10)') node_number2d(i    ,k    ,nxp), node_number2d(i+kd1,k    ,nxp), &
                       node_number2d(i+kd1,k+kd1,nxp), node_number2d(i    ,k+kd1,nxp)
    end do
   end do

 endif

 !--------------------------------------------------------------------------------------
 !--------------------------------------------------------------------------------------
 ! End of Upstream (inlet) region
 !--------------------------------------------------------------------------------------
 !--------------------------------------------------------------------------------------


 !--------------------------------------------------------------------------------------
 !--------------------------------------------------------------------------------------
 ! Downstream (outlet) region
 !--------------------------------------------------------------------------------------
 !--------------------------------------------------------------------------------------
 if (ixmax < nxp+1) then

 !----------------------------------------------------------------------------
   nnodes = (nxp+kd1)*(nyp+kd1)
   nquads = (nxp+1-ixmax)*nyp
   write(2,*) 'ZONE T="Boundary 1 (Outlet: Bottom)"  N=', nnodes,',E=', nquads, &
               ' , ET=quadrilateral, F=FEPOINT'

    k = kd1

   do j = kd1, ny+kd1, inc
    do i = kd1, nx+kd1, inc
     write(2,'(3es20.10)') x(i), y(j), z(k)
    end do
   end do

    do j = kd1, nyp
    do i = ixmax, nxp
     write(2,'(4i10)') node_number2d(i    ,j    ,nxp), node_number2d(i+kd1,j    ,nxp), &
                       node_number2d(i+kd1,j+kd1,nxp), node_number2d(i    ,j+kd1,nxp)
    end do
    end do

 !----------------------------------------------------------------------------
   nnodes = (nxp+kd1)*(nyp+kd1)
   nquads = (nxp+1-ixmax)*nyp
   write(2,*) 'ZONE T="Boundary 2 (Outlet: Top)"  N=', nnodes,',E=', nquads, &
               ' , ET=quadrilateral, F=FEPOINT'

    k = nz+kd1

   do j = kd1, ny+kd1, inc
    do i = kd1, nx+kd1, inc
     write(2,'(3es20.10)') x(i), y(j), z(k)
    end do
   end do

   do j = kd1, nyp
    do i = ixmax, nxp
     write(2,'(4i10)') node_number2d(i  ,j  ,nxp), node_number2d(i+kd1,j  ,nxp), &
                       node_number2d(i+kd1,j+kd1,nxp), node_number2d(i  ,j+kd1,nxp)
    end do
   end do

 !----------------------------------------------------------------------------
   nnodes = (nxp+kd1)*(nzp+kd1)
   nquads = (nxp+1-ixmax)*nzp
   write(2,*) 'ZONE T="Boundary 3 (Outlet: Right)"  N=', nnodes,',E=', nquads, &
               ' , ET=quadrilateral, F=FEPOINT'

    j = kd1

   do k = kd1, nz+kd1, inc
    do i = kd1, nx+kd1, inc
     write(2,'(3es20.10)') x(i), y(j), z(k)
    end do
   end do

   do k = kd1, nzp
    do i = ixmax, nxp
     write(2,'(4i10)') node_number2d(i    ,k    ,nxp), node_number2d(i+kd1,k    ,nxp), &
                       node_number2d(i+kd1,k+kd1,nxp), node_number2d(i    ,k+kd1,nxp)
    end do
   end do

 !----------------------------------------------------------------------------
   nnodes = (nxp+kd1)*(nzp+kd1)
   nquads = (nxp+1-ixmax)*nzp
   write(2,*) 'ZONE T="Boundary 4 (Outlet: Left)"  N=', nnodes,',E=', nquads, &
               ' , ET=quadrilateral, F=FEPOINT'

    j = ny+kd1

   do k = kd1, nz+kd1, inc
    do i = kd1, nx+kd1, inc
     write(2,'(3es20.10)') x(i), y(j), z(k)
    end do
   end do

   do k = kd1, nzp
    do i = ixmax, nxp
     write(2,'(4i10)') node_number2d(i    ,k    ,nxp), node_number2d(i+kd1,k    ,nxp), &
                       node_number2d(i+kd1,k+kd1,nxp), node_number2d(i    ,k+kd1,nxp)
    end do
   end do

 endif
 !--------------------------------------------------------------------------------------
 !--------------------------------------------------------------------------------------
 ! End of Downstream (outlet) region
 !--------------------------------------------------------------------------------------
 !--------------------------------------------------------------------------------------

 close(2)

 end subroutine write_tecplot_boundary_file_hex
!********************************************************************************

!*******************************************************************************
!
! HEX: This subroutine writes a Tecplot file for the volume grid.
!
!*******************************************************************************
 subroutine write_tecplot_volume_file_hex(inc,filename)

 implicit none
 integer(kd)  , intent(in) :: inc
 character(80), intent(in) :: filename
 integer(kd)               :: nnodes, i, j, k
 integer(kd)               :: nxp, nyp, nzp, nhex

  nxp = nx / inc
  nyp = ny / inc
  nzp = nz / inc


  nnodes = (nxp+kd1)*(nyp+kd1)*(nzp+kd1)
    nhex = nxp*nyp*nzp

  open(unit=8, file=filename, status="unknown", iostat=os)
  write(8,*) 'TITLE = "GRID"'
  write(8,*) 'VARIABLES = "x","y","z"'

 !Hex zone

   write(8,*) 'zone  n=', nnodes,',e=', nhex,' , et=brick, f=fepoint'

   do k = kd1, nz+kd1, inc
    do j = kd1, ny+kd1, inc
     do i = kd1, nx+kd1, inc
       write(8,'(3es20.10)')  x(i), y(j), z(k)
     end do
    end do
   end do

   do k = kd1, nzp
    do j = kd1, nyp
     do i = kd1, nxp
      write(8,'(8i10)') node_number(i    ,j    ,k    ,nxp,nyp), &
                        node_number(i+kd1,j    ,k    ,nxp,nyp), &
                        node_number(i+kd1,j+kd1,k    ,nxp,nyp), &
                        node_number(i    ,j+kd1,k    ,nxp,nyp), &
                        node_number(i    ,j    ,k+kd1,nxp,nyp), &
                        node_number(i+kd1,j    ,k+kd1,nxp,nyp), &
                        node_number(i+kd1,j+kd1,k+kd1,nxp,nyp), &
                        node_number(i    ,j+kd1,k+kd1,nxp,nyp)
     end do
    end do
   end do

  close(8)

 end subroutine write_tecplot_volume_file_hex
!*******************************************************************************

!*******************************************************************************
!
! HEX: This subroutine writes a ugrid file.
!
!*******************************************************************************
 subroutine write_ugrid_file_hex(inc,filename)

 implicit none
 integer(kd)  , intent(in) :: inc
 character(80), intent(in) :: filename
 integer(kd)               :: nnodes, i, j, k
 integer(kd)               :: nxp, nyp, nzp, nhex, nquads_b

  nxp = nx / inc
  nyp = ny / inc
  nzp = nz / inc


    nnodes = (nxp+kd1)*(nyp+kd1)*(nzp+kd1)
      nhex = nxp*nyp*nzp
  nquads_b = kd2*(nxp*nyp + nyp*nzp + nzp*nxp)

  if ( ugrid_file_unformatted ) then
    open(unit=9, file=filename, form='unformatted',access="stream",&
                                             status='unknown', iostat=os )
!   write(9) nnodes,   ntrias_b,    nquads_b,   ntet,    0, nprs, nhex
    write(9) nnodes,          0,    nquads_b,      0,    0,    0, nhex
  else
    open(unit=9, file=filename, status="unknown", iostat=os)
    !                    #nodes, #tri_faces, #quad_faces, #tetra, #pyr, #prz,
    !                    #hex
    write(9,'(7i20)') nnodes,          0,    nquads_b,      0,    0,    0, nhex
  endif

!---------------------------------------------------------------
!(1) Unformatted grid file
!---------------------------------------------------------------

  if ( ugrid_file_unformatted ) then

  !------------------------------------------------------------------------------
  ! Nodes
    do k = kd1, nz+kd1, inc
     do j = kd1, ny+kd1, inc
      do i = kd1, nx+kd1, inc
        write(9)  x(i), y(j), z(k)
      end do
     end do
    end do

  !------------------------------------------------------------------------------
  ! Quad faces = nquad  !Oriented inward

   !(1)Bottom: iquad = 1, nx*nz
     k = kd1
    do j = kd1, nyp
     do i = izero, ixmax-1
      write(9) node_number(i    ,j    ,k,nxp,nyp), node_number(i+kd1,j    ,k,nxp,nyp), &
               node_number(i+kd1,j+kd1,k,nxp,nyp), node_number(i    ,j+kd1,k,nxp,nyp)
     end do
    end do

   !(2)Top:    iquad = nx*nz+kd1, 2*nx*nz
     k = nzp+kd1
    do j = kd1, nyp
     do i = izero, ixmax-1
      write(9) node_number(i    ,j    ,k,nxp,nyp), node_number(i    ,j+kd1,k,nxp,nyp), &
               node_number(i+kd1,j+kd1,k,nxp,nyp), node_number(i+kd1,j    ,k,nxp,nyp)
     end do
    end do

   !(3)Right (y=ymin): iquad = 2*nx*nz+kd1, 2*nx*nz+nx*nz
     j = kd1
    do k = kd1, nzp
     do i = izero, ixmax-1
      write(9) node_number(i    ,j,k    ,nxp,nyp), node_number(i    ,j,k+kd1,nxp,nyp), &
               node_number(i+kd1,j,k+kd1,nxp,nyp), node_number(i+kd1,j,k    ,nxp,nyp)
     end do
    end do

   !(4)Left (y=ymax): iquad = 2*nx*nz+nx*nz+kd1, 2*(nx*nz+nx*nz)
     j = nyp+kd1
    do k = kd1, nzp
     do i = izero, ixmax-1
      write(9) node_number(i    ,j,k    ,nxp,nyp), node_number(i+kd1,j,k    ,nxp,nyp), &
               node_number(i+kd1,j,k+kd1,nxp,nyp), node_number(i    ,j,k+kd1,nxp,nyp)
     end do
    end do

   !(5)Near: iquad = 2*(nx*nz+nx*nz)+kd1, 2*(nx*nz+nx*nz)+ny*nz
     i = kd1
    do k = kd1, nzp
     do j = kd1, nyp
      write(9) node_number(i,j    ,k    ,nxp,nyp), node_number(i,j+kd1,k    ,nxp,nyp), &
               node_number(i,j+kd1,k+kd1,nxp,nyp), node_number(i,j    ,k+kd1,nxp,nyp)
     end do
    end do

   !(6)Far: iquad = 2*(nx*nz+nx*nz)+ny*nz+kd1, 2*(nx*nz+nx*nz+ny*nz)
     i = nxp+kd1
    do k = kd1, nzp
     do j = kd1, nyp
      write(9) node_number(i,j    ,k    ,nxp,nyp), node_number(i,j    ,k+kd1,nxp,nyp), &
               node_number(i,j+kd1,k+kd1,nxp,nyp), node_number(i,j+kd1,k    ,nxp,nyp)
     end do
    end do

   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   if (izero > 1) then
   
   !(7)Bottom: iquad = 1, nx*nz
     k = kd1
    do j = kd1, nyp
     do i = kd1, izero-kd1
      write(9) node_number(i    ,j    ,k,nxp,nyp), node_number(i+kd1,j    ,k,nxp,nyp), &
               node_number(i+kd1,j+kd1,k,nxp,nyp), node_number(i    ,j+kd1,k,nxp,nyp)
     end do
    end do

   !(8)Top:    iquad = nx*nz+kd1, 2*nx*nz
     k = nzp+kd1
    do j = kd1, nyp
     do i = kd1, izero-kd1
      write(9) node_number(i    ,j    ,k,nxp,nyp), node_number(i    ,j+kd1,k,nxp,nyp), &
               node_number(i+kd1,j+kd1,k,nxp,nyp), node_number(i+kd1,j    ,k,nxp,nyp)
     end do
    end do

   !(9)Right (y=ymin): iquad = 2*nx*nz+kd1, 2*nx*nz+nx*nz
     j = kd1
    do k = kd1, nzp
     do i = kd1, izero-kd1
      write(9) node_number(i    ,j,k    ,nxp,nyp), node_number(i    ,j,k+kd1,nxp,nyp), &
               node_number(i+kd1,j,k+kd1,nxp,nyp), node_number(i+kd1,j,k    ,nxp,nyp)
     end do
    end do

   !(10)Left (y=ymax): iquad = 2*nx*nz+nx*nz+kd1, 2*(nx*nz+nx*nz)
     j = nyp+kd1
    do k = kd1, nzp
     do i = kd1, izero-kd1
      write(9) node_number(i    ,j,k    ,nxp,nyp), node_number(i+kd1,j,k    ,nxp,nyp), &
               node_number(i+kd1,j,k+kd1,nxp,nyp), node_number(i    ,j,k+kd1,nxp,nyp)
     end do
    end do

   endif
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------

   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   if (ixmax < nxp+1) then
   
   !(7)Bottom: iquad = 1, nx*nz
     k = kd1
    do j = kd1, nyp
     do i = ixmax, nxp
      write(9) node_number(i    ,j    ,k,nxp,nyp), node_number(i+kd1,j    ,k,nxp,nyp), &
               node_number(i+kd1,j+kd1,k,nxp,nyp), node_number(i    ,j+kd1,k,nxp,nyp)
     end do
    end do

   !(8)Top:    iquad = nx*nz+kd1, 2*nx*nz
     k = nzp+kd1
    do j = kd1, nyp
     do i = ixmax, nxp
      write(9) node_number(i    ,j    ,k,nxp,nyp), node_number(i    ,j+kd1,k,nxp,nyp), &
               node_number(i+kd1,j+kd1,k,nxp,nyp), node_number(i+kd1,j    ,k,nxp,nyp)
     end do
    end do

   !(9)Right (y=ymin): iquad = 2*nx*nz+kd1, 2*nx*nz+nx*nz
     j = kd1
    do k = kd1, nzp
     do i = ixmax, nxp
      write(9) node_number(i    ,j,k    ,nxp,nyp), node_number(i    ,j,k+kd1,nxp,nyp), &
               node_number(i+kd1,j,k+kd1,nxp,nyp), node_number(i+kd1,j,k    ,nxp,nyp)
     end do
    end do

   !(10)Left (y=ymax): iquad = 2*nx*nz+nx*nz+kd1, 2*(nx*nz+nx*nz)
     j = nyp+kd1
    do k = kd1, nzp
     do i = ixmax, nxp
      write(9) node_number(i    ,j,k    ,nxp,nyp), node_number(i+kd1,j,k    ,nxp,nyp), &
               node_number(i+kd1,j,k+kd1,nxp,nyp), node_number(i    ,j,k+kd1,nxp,nyp)
     end do
    end do

   endif
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------

  !--------------------------------------------------------------------------------
  ! Face tag

   !(1)Bottom: iquad = 1, nx*nz
    do j = kd1, nyp
     do i = izero, ixmax-1 !nxp
      write(9) 1
     end do
    end do

   !(2)Top:    iquad = nx*nz+kd1, 2*nx*nz
    do j = kd1, nyp
     do i = izero, ixmax-1 ! nxp
      write(9) 2
     end do
    end do

   !(3)Right: iquad = 2*nx*nz+kd1, 2*nx*nz+nx*nz
    do k = kd1, nzp
     do i = izero, ixmax-1 ! nxp
      write(9) 3
     end do
    end do

   !(4)Left: iquad = 2*nx*nz+nx*nz+kd1, 2*(nx*nz+nx*nz)
    do k = kd1, nzp
     do i = izero, ixmax-1 ! nxp
      write(9) 4
     end do
    end do

   !(5)Near: iquad = 2*(nx*nz+nx*nz)+kd1, 2*(nx*nz+nx*nz)+ny*nz
    do k = kd1, nzp
     do j = kd1, nyp
      write(9) 5
     end do
    end do

   !(6)Far: iquad = 2*(nx*nz+nx*nz)+ny*nz+kd1, 2*(nx*nz+nx*nz+ny*nz)
    do k = kd1, nzp
     do j = kd1, nyp
      write(9) 6
     end do
    end do

   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   if (izero > 1) then

   !(1)Bottom: iquad = 1, nx*nz
    do j = kd1, nyp
     do i = 1, izero-1
      write(9) 7
     end do
    end do

   !(2)Top:    iquad = nx*nz+kd1, 2*nx*nz
    do j = kd1, nyp
     do i = 1, izero-1
      write(9) 8
     end do
    end do

   !(3)Right: iquad = 2*nx*nz+kd1, 2*nx*nz+nx*nz
    do k = kd1, nzp
     do i = 1, izero-1
      write(9) 9
     end do
    end do

   !(4)Left: iquad = 2*nx*nz+nx*nz+kd1, 2*(nx*nz+nx*nz)
    do k = kd1, nzp
     do i = 1, izero-1
      write(9) 10
     end do
    end do

   endif
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------

   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   if (ixmax < nxp+1) then

   !(1)Bottom: iquad = 1, nx*nz
    do j = kd1, nyp
     do i = ixmax, nxp
      write(9) 11
     end do
    end do

   !(2)Top:    iquad = nx*nz+kd1, 2*nx*nz
    do j = kd1, nyp
     do i = ixmax, nxp
      write(9) 12
     end do
    end do

   !(3)Right: iquad = 2*nx*nz+kd1, 2*nx*nz+nx*nz
    do k = kd1, nzp
     do i = ixmax, nxp
      write(9) 13
     end do
    end do

   !(4)Left: iquad = 2*nx*nz+nx*nz+kd1, 2*(nx*nz+nx*nz)
    do k = kd1, nzp
     do i = ixmax, nxp
      write(9) 14
     end do
    end do

   endif
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------

  ! Hex
    do k = kd1, nzp
     do j = kd1, nyp
      do i = kd1, nxp
       write(9) node_number(i    ,j    ,k    ,nxp,nyp), &
                node_number(i+kd1,j    ,k    ,nxp,nyp), &
                node_number(i+kd1,j+kd1,k    ,nxp,nyp), &
                node_number(i    ,j+kd1,k    ,nxp,nyp), &
                node_number(i    ,j    ,k+kd1,nxp,nyp), &
                node_number(i+kd1,j    ,k+kd1,nxp,nyp), &
                node_number(i+kd1,j+kd1,k+kd1,nxp,nyp), &
                node_number(i    ,j+kd1,k+kd1,nxp,nyp)
      end do
     end do
    end do

!---------------------------------------------------------------
!(2) Formatted grid file
!---------------------------------------------------------------

  else

  !------------------------------------------------------------------------------
  ! Nodes
    do k = kd1, nz+kd1, inc
     do j = kd1, ny+kd1, inc
      do i = kd1, nx+kd1, inc
        write(9,'(3es26.15)')  x(i), y(j), z(k)
      end do
     end do
    end do

  !------------------------------------------------------------------------------
  ! Quad faces = nquad  !Oriented inward

   !(1)Bottom: iquad = 1, nx*nz
     k = kd1
    do j = kd1, nyp
     do i = izero, ixmax-1
      write(9,'(4i20)') node_number(i    ,j    ,k,nxp,nyp), node_number(i+kd1,j    ,k,nxp,nyp), &
                        node_number(i+kd1,j+kd1,k,nxp,nyp), node_number(i    ,j+kd1,k,nxp,nyp)
     end do
    end do

   !(2)Top:    iquad = nx*nz+kd1, 2*nx*nz
     k = nzp+kd1
    do j = kd1, nyp
     do i = izero, ixmax-1
      write(9,'(4i20)') node_number(i    ,j    ,k,nxp,nyp), node_number(i    ,j+kd1,k,nxp,nyp), &
                        node_number(i+kd1,j+kd1,k,nxp,nyp), node_number(i+kd1,j    ,k,nxp,nyp)
     end do
    end do

   !(3)Right: iquad = 2*nx*nz+kd1, 2*nx*nz+nx*nz
     j = kd1
    do k = kd1, nzp
     do i = izero, ixmax-1
      write(9,'(4i20)') node_number(i    ,j,k    ,nxp,nyp), node_number(i    ,j,k+kd1,nxp,nyp), &
                        node_number(i+kd1,j,k+kd1,nxp,nyp), node_number(i+kd1,j,k    ,nxp,nyp)
     end do
    end do

   !(4)Left: iquad = 2*nx*nz+nx*nz+kd1, 2*(nx*nz+nx*nz)
     j = nyp+kd1
    do k = kd1, nzp
     do i = izero, ixmax-1
      write(9,'(4i20)') node_number(i    ,j,k    ,nxp,nyp), node_number(i+kd1,j,k    ,nxp,nyp), &
                        node_number(i+kd1,j,k+kd1,nxp,nyp), node_number(i    ,j,k+kd1,nxp,nyp)
     end do
    end do

   !(5)Near: iquad = 2*(nx*nz+nx*nz)+kd1, 2*(nx*nz+nx*nz)+ny*nz
     i = kd1
    do k = kd1, nzp
     do j = kd1, nyp
      write(9,'(4i20)') node_number(i,j    ,k    ,nxp,nyp), node_number(i,j+kd1,k    ,nxp,nyp), &
                        node_number(i,j+kd1,k+kd1,nxp,nyp), node_number(i,j    ,k+kd1,nxp,nyp)
     end do
    end do

   !(6)Far: iquad = 2*(nx*nz+nx*nz)+ny*nz+kd1, 2*(nx*nz+nx*nz+ny*nz)
     i = nxp+kd1
    do k = kd1, nzp
     do j = kd1, nyp
      write(9,'(4i20)') node_number(i,j    ,k    ,nxp,nyp), node_number(i,j    ,k+kd1,nxp,nyp), &
                        node_number(i,j+kd1,k+kd1,nxp,nyp), node_number(i,j+kd1,k    ,nxp,nyp)
     end do
    end do

   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   if (izero > 1) then
   
   !(7)Bottom: iquad = 1, nx*nz
     k = kd1
    do j = kd1, nyp
     do i = kd1, izero-kd1
      write(9,'(4i20)') node_number(i    ,j    ,k,nxp,nyp), node_number(i+kd1,j    ,k,nxp,nyp), &
                        node_number(i+kd1,j+kd1,k,nxp,nyp), node_number(i    ,j+kd1,k,nxp,nyp)
     end do
    end do

   !(8)Top:    iquad = nx*nz+kd1, 2*nx*nz
     k = nzp+kd1
    do j = kd1, nyp
     do i = kd1, izero-kd1
      write(9,'(4i20)') node_number(i    ,j    ,k,nxp,nyp), node_number(i    ,j+kd1,k,nxp,nyp), &
                        node_number(i+kd1,j+kd1,k,nxp,nyp), node_number(i+kd1,j    ,k,nxp,nyp)
     end do
    end do

   !(9)Right (y=ymin): iquad = 2*nx*nz+kd1, 2*nx*nz+nx*nz
     j = kd1
    do k = kd1, nzp
     do i = kd1, izero-kd1
      write(9,'(4i20)') node_number(i    ,j,k    ,nxp,nyp), node_number(i    ,j,k+kd1,nxp,nyp), &
                        node_number(i+kd1,j,k+kd1,nxp,nyp), node_number(i+kd1,j,k    ,nxp,nyp)
     end do
    end do

   !(10)Left (y=ymax): iquad = 2*nx*nz+nx*nz+kd1, 2*(nx*nz+nx*nz)
     j = nyp+kd1
    do k = kd1, nzp
     do i = kd1, izero-kd1
      write(9,'(4i20)') node_number(i    ,j,k    ,nxp,nyp), node_number(i+kd1,j,k    ,nxp,nyp), &
                        node_number(i+kd1,j,k+kd1,nxp,nyp), node_number(i    ,j,k+kd1,nxp,nyp)
     end do
    end do

   endif
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------

   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   if (ixmax < nxp+1) then
   
   !(7)Bottom: iquad = 1, nx*nz
     k = kd1
    do j = kd1, nyp
     do i = ixmax, nxp
      write(9,'(4i20)') node_number(i    ,j    ,k,nxp,nyp), node_number(i+kd1,j    ,k,nxp,nyp), &
                        node_number(i+kd1,j+kd1,k,nxp,nyp), node_number(i    ,j+kd1,k,nxp,nyp)
     end do
    end do

   !(8)Top:    iquad = nx*nz+kd1, 2*nx*nz
     k = nzp+kd1
    do j = kd1, nyp
     do i = ixmax, nxp
      write(9,'(4i20)') node_number(i    ,j    ,k,nxp,nyp), node_number(i    ,j+kd1,k,nxp,nyp), &
                        node_number(i+kd1,j+kd1,k,nxp,nyp), node_number(i+kd1,j    ,k,nxp,nyp)
     end do
    end do

   !(9)Right (y=ymin): iquad = 2*nx*nz+kd1, 2*nx*nz+nx*nz
     j = kd1
    do k = kd1, nzp
     do i = ixmax, nxp
      write(9,'(4i20)') node_number(i    ,j,k    ,nxp,nyp), node_number(i    ,j,k+kd1,nxp,nyp), &
                        node_number(i+kd1,j,k+kd1,nxp,nyp), node_number(i+kd1,j,k    ,nxp,nyp)
     end do
    end do

   !(10)Left (y=ymax): iquad = 2*nx*nz+nx*nz+kd1, 2*(nx*nz+nx*nz)
     j = nyp+kd1
    do k = kd1, nzp
     do i = ixmax, nxp
      write(9,'(4i20)') node_number(i    ,j,k    ,nxp,nyp), node_number(i+kd1,j,k    ,nxp,nyp), &
                        node_number(i+kd1,j,k+kd1,nxp,nyp), node_number(i    ,j,k+kd1,nxp,nyp)
     end do
    end do

   endif
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------

  !--------------------------------------------------------------------------------
  ! Face tag

   !(1)Bottom: iquad = 1, nx*nz
    do j = kd1, nyp
     do i = izero, ixmax-1
      write(9,'(i10)') 1
     end do
    end do

   !(2)Top:    iquad = nx*nz+kd1, 2*nx*nz
    do j = kd1, nyp
     do i = izero, ixmax-1
      write(9,'(i10)') 2
     end do
    end do

   !(3)Right: iquad = 2*nx*nz+kd1, 2*nx*nz+nx*nz
    do k = kd1, nzp
     do i = izero, ixmax-1
      write(9,'(i10)') 3
     end do
    end do

   !(4)Left: iquad = 2*nx*nz+nx*nz+kd1, 2*(nx*nz+nx*nz)
    do k = kd1, nzp
     do i = izero, ixmax-1
      write(9,'(i10)') 4
     end do
    end do

   !(5)Near: iquad = 2*(nx*nz+nx*nz)+kd1, 2*(nx*nz+nx*nz)+ny*nz
    do k = kd1, nzp
     do j = kd1, nyp
      write(9,'(i10)') 5
     end do
    end do

   !(6)Far: iquad = 2*(nx*nz+nx*nz)+ny*nz+kd1, 2*(nx*nz+nx*nz+ny*nz)
    do k = kd1, nzp
     do j = kd1, nyp
      write(9,'(i10)') 6
     end do
    end do

   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   if (izero > 1) then

   !(1)Bottom: iquad = 1, nx*nz
    do j = kd1, nyp
     do i = 1, izero-1
      write(9,'(i10)') 7
     end do
    end do

   !(2)Top:    iquad = nx*nz+kd1, 2*nx*nz
    do j = kd1, nyp
     do i = 1, izero-1
      write(9,'(i10)') 8
     end do
    end do

   !(3)Right: iquad = 2*nx*nz+kd1, 2*nx*nz+nx*nz
    do k = kd1, nzp
     do i = 1, izero-1
      write(9,'(i10)') 9
     end do
    end do

   !(4)Left: iquad = 2*nx*nz+nx*nz+kd1, 2*(nx*nz+nx*nz)
    do k = kd1, nzp
     do i = 1, izero-1
      write(9,'(i10)') 10
     end do
    end do

   endif
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------

   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   if (ixmax < nxp+1) then

   !(1)Bottom: iquad = 1, nx*nz
    do j = kd1, nyp
     do i = ixmax, nxp
      write(9,'(i10)') 11
     end do
    end do

   !(2)Top:    iquad = nx*nz+kd1, 2*nx*nz
    do j = kd1, nyp
     do i = ixmax, nxp
      write(9,'(i10)') 12
     end do
    end do

   !(3)Right: iquad = 2*nx*nz+kd1, 2*nx*nz+nx*nz
    do k = kd1, nzp
     do i = ixmax, nxp
      write(9,'(i10)') 13
     end do
    end do

   !(4)Left: iquad = 2*nx*nz+nx*nz+kd1, 2*(nx*nz+nx*nz)
    do k = kd1, nzp
     do i = ixmax, nxp
      write(9,'(i10)') 14
     end do
    end do

   endif
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------

  ! Hex
    do k = kd1, nzp
     do j = kd1, nyp
      do i = kd1, nxp
       write(9,'(8i20)') &
                node_number(i    ,j    ,k    ,nxp,nyp), &
                node_number(i+kd1,j    ,k    ,nxp,nyp), &
                node_number(i+kd1,j+kd1,k    ,nxp,nyp), &
                node_number(i    ,j+kd1,k    ,nxp,nyp), &
                node_number(i    ,j    ,k+kd1,nxp,nyp), &
                node_number(i+kd1,j    ,k+kd1,nxp,nyp), &
                node_number(i+kd1,j+kd1,k+kd1,nxp,nyp), &
                node_number(i    ,j+kd1,k+kd1,nxp,nyp)
      end do
     end do
    end do

  endif

!---------------------------------------------------------------
! End of Unformatted or Formatted
!---------------------------------------------------------------

  close(9)

 end subroutine write_ugrid_file_hex
!********************************************************************************



!*******************************************************************************
! This subroutine writes a PLOT3D file.
!*******************************************************************************
 subroutine write_plot3d_file(inc,filename,filename_nmf)

 implicit none
 integer(kd)  , intent(in) :: inc
 character(80), intent(in) :: filename, filename_nmf

  integer(kd) :: i, j, k
  integer(kd) :: imax, jmax, kmax


  ! imin = 1
    imax = nx/inc+kd1
  ! jmin = 1
    jmax = ny/inc+kd1
  ! kmin = 1
    kmax = nz/inc+kd1


  if ( p3d_file_unformatted ) then
    !Note: access="stream" removed (04-04-2017). It was causing some problems...
    open(unit=9, file=filename, form='unformatted', status='unknown', iostat=os )
    write(9) kd1 ! Single block
    write(9) imax, jmax, kmax ! ni, nj, nk
  else
    open(unit=9, file=filename, status="unknown", iostat=os)
    write(9,'(i20)') kd1 ! Single block
    write(9,'(3i20)') imax, jmax, kmax ! ni, nj, nk
  endif

   if ( p3d_file_unformatted ) then
    write(9)                                                                 &
         ((( x(i) , i=kd1,nx+kd1,inc), j=kd1,ny+kd1,inc), k=kd1,nz+kd1,inc), &
         ((( y(j) , i=kd1,nx+kd1,inc), j=kd1,ny+kd1,inc), k=kd1,nz+kd1,inc), &
         ((( z(k) , i=kd1,nx+kd1,inc), j=kd1,ny+kd1,inc), k=kd1,nz+kd1,inc)
   else
    write(9,'(3es26.15)')                                                    &
         ((( x(i) , i=kd1,nx+kd1,inc), j=kd1,ny+kd1,inc), k=kd1,nz+kd1,inc), &
         ((( y(j) , i=kd1,nx+kd1,inc), j=kd1,ny+kd1,inc), k=kd1,nz+kd1,inc), &
         ((( z(k) , i=kd1,nx+kd1,inc), j=kd1,ny+kd1,inc), k=kd1,nz+kd1,inc)
   endif

  close(9)

!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
! Write .nmf file.
!----------------------------------------------------------------------------------

 open(unit=9, file=filename_nmf, status='unknown', iostat=os )

 write(9,'(a)') "# ============= NASA Langley Geometry Laboratory TOG Neutral Map File ==============="
 write(9,'(a)') "# ==================================================================================="
 write(9,'(a)') "# Block#   IDIM   JDIM   KDIM"
 write(9,'(a)') "# ==================================================================================="
 write(9,'(i3)')     kd1
 write(9,*)
 write(9,'(4i15)')   kd1,  imax, jmax, kmax
 write(9,*)
 write(9,'(a)') "# ==================================================================================="
 write(9,'(a)') "# Type         B1  F1     S1   E1     S2   E2    B2  F2     S1   E1     S2   E2  Swap"
 write(9,'(a)') "#------------------------------------------------------------------------------------"
 write(9,'(a,2i3,i3,i15,i3,i15)')    " supersonic_inflow ", kd1, 3, kd1, jmax, kd1, kmax ! 3 -> imin,   j, k
 write(9,'(a,2i3,i3,i15,i3,i15)')    "       extrapolate ", kd1, 4, kd1, jmax, kd1, kmax ! 4 -> imax,   j, k
 write(9,'(a,2i3,i3,i15,i3,i15)')    "     viscous_solid ", kd1, 5, kd1, kmax, kd1, imax ! 5 -> jmin,   k, i
 write(9,'(a,2i3,i3,i15,i3,i15)')    " symmetry_y_strong ", kd1, 6, kd1, kmax, kd1, imax ! 6 -> jmax,   k, i
 write(9,'(a,2i3,i3,i15,i3,i15)')    "     viscous_solid ", kd1, 1, kd1, imax, kd1, jmax ! 1 -> kmin,   i, j
 write(9,'(a,2i3,i3,i15,i3,i15)')    " symmetry_y_strong ", kd1, 2, kd1, imax, kd1, jmax ! 2 -> kmax,   i, j

 close(9)
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------

 end subroutine write_plot3d_file
!********************************************************************************


!*******************************************************************************
! This subroutine writes a su2 grid file.
!
! Note: Nodes -> i = 0,1,2,...; Elements -> i = 0,1,2,...
!
!*******************************************************************************
 subroutine write_su2grid_file_hex(inc,filename)

 implicit none
 integer(kd)  , intent(in) :: inc
 character(80), intent(in) :: filename

 integer(kd) :: nnodes, i, j, k, counter
 integer(kd) :: nxp, nyp, nzp, nhex

  nxp    = nx / inc
  nyp    = ny / inc
  nzp    = nz / inc
  nnodes = (nxp+kd1)*(nyp+kd1)*(nzp+kd1)
  nhex   = nxp*nyp*nzp

  write(*,*) "    write_su2grid_file: nnodes = ", nnodes
  write(*,*) "    write_su2grid_file:   nhex = ", nhex

  open(unit=7, file=filename, status="unknown", iostat=os)

  write(7,*) "%"
  write(7,*) "% Problem dimension"
  write(7,*) "%"
  write(7,5) 3
5 format('NDIME= ',i12)

   write(7,*) "%"
   write(7,*) "% Inner element connectivity"
   write(7,10) nhex
10 format('NELEM= ',i12)

   counter = 0

 !-------------------------------------------------------------------------
 ! Elements

  ! No tetra
  ! No prism
  ! Only Hexahedra:
    do k = kd1, nzp
     do j = kd1, nyp
      do i = kd1, nxp
      write(7,'(10i10)') 12                                      , &
                        node_number(i    ,j    ,k    ,nxp,nyp)-kd1, &
                        node_number(i+kd1,j    ,k    ,nxp,nyp)-kd1, &
                        node_number(i+kd1,j+kd1,k    ,nxp,nyp)-kd1, &
                        node_number(i    ,j+kd1,k    ,nxp,nyp)-kd1, &
                        node_number(i    ,j    ,k+kd1,nxp,nyp)-kd1, &
                        node_number(i+kd1,j    ,k+kd1,nxp,nyp)-kd1, &
                        node_number(i+kd1,j+kd1,k+kd1,nxp,nyp)-kd1, &
                        node_number(i    ,j+kd1,k+kd1,nxp,nyp)-kd1, counter

      counter = counter + kd1
      end do
     end do
    end do

   write(*,*) "  --- elm check", nhex, counter

 !--------------------------------------------------------------------------
 ! Nodes

   write(7,*) "%"
   write(7,*) "% Node coordinates"
   write(7,*) "%"
   write(7,20) nnodes
20 format('NPOIN= ', i12)

   counter = 0

  ! Nodes
    do k = kd1, nz+kd1, inc
     do j = kd1, ny+kd1, inc
      do i = kd1, nx+kd1, inc
        write(7,'(3es26.15,i20)')  x(i), y(j), z(k), counter
        counter = counter + kd1
      end do
     end do
    end do

   write(*,*) "  --- node check", nnodes, counter

 !--------------------------------------------------------------------------
 ! Boundary

    write(7,*) "%"
    write(7,*) "% Boundary elements"
    write(7,*) "%"

  !+Inlet and +Outlet
   if     (izero >  1 .and. ixmax <  nxp+1) then

    write(7,30) 14

  !+Inlet
   elseif (izero >  1 .and. ixmax >= nxp+1) then

    write(7,30) 10

  !Outlet
   elseif (izero <= 1 .and. ixmax <  nxp+1) then

    write(7,30) 10

  !No inlet and outlet
   else

    write(7,30) 6

   endif

30 format('NMARK= ',i12)

40 format('MARKER_TAG= ',a)
50 format('MARKER_ELEMS= ', i12)

 !--------------------------------------------------------------------------
 ! (1) Viscous bottom surface (z=zmin)
 !--------------------------------------------------------------------------

   write(*,40) "DUCT_VISCOUS_ZMIN"
   write(7,40) "DUCT_VISCOUS_ZMIN"
   write(7,50) (nxp-(izero-1) - (nxp+1-ixmax))*nyp

   k = kd1
    do j = kd1, nyp
     do i = izero, ixmax-1
      write(7,'(5i20)') 9, node_number(i    ,j    ,k,nxp,nyp)-kd1, node_number(i+kd1,j    ,k,nxp,nyp)-kd1, &
                           node_number(i+kd1,j+kd1,k,nxp,nyp)-kd1, node_number(i    ,j+kd1,k,nxp,nyp)-kd1
     end do
    end do

 !--------------------------------------------------------------------------
 ! (2) Top symmetry z (z=zmax)
 !--------------------------------------------------------------------------

   write(*,40) "DUCT_SYMMETRY_Z"
   write(7,40) "DUCT_SYMMETRY_Z"
   write(7,50) (nxp-(izero-1) - (nxp+1-ixmax))*nyp

     k = nzp+kd1
    do j = kd1, nyp
     do i = izero, ixmax-1
      write(7,'(5i20)') 9, node_number(i    ,j    ,k,nxp,nyp)-kd1, node_number(i    ,j+kd1,k,nxp,nyp)-kd1, &
                           node_number(i+kd1,j+kd1,k,nxp,nyp)-kd1, node_number(i+kd1,j    ,k,nxp,nyp)-kd1
     end do
    end do
 !--------------------------------------------------------------------------


 !--------------------------------------------------------------------------
 ! (3) Viscous side (y=ymin)
 !--------------------------------------------------------------------------

   write(*,40) "DUCT_VISCOUS_YMIN"
   write(7,40) "DUCT_VISCOUS_YMIN"
   write(7,50) (nxp-(izero-1) - (nxp+1-ixmax))*nzp

     j = kd1
    do k = kd1, nzp
     do i = izero, ixmax-1
      write(7,'(5i20)') 9, node_number(i    ,j,k    ,nxp,nyp)-kd1, node_number(i    ,j,k+kd1,nxp,nyp)-kd1, &
                           node_number(i+kd1,j,k+kd1,nxp,nyp)-kd1, node_number(i+kd1,j,k    ,nxp,nyp)-kd1
     end do
    end do

 !--------------------------------------------------------------------------
 ! (4) Symmetry y (y=ymax)
 !--------------------------------------------------------------------------

   write(*,40) "DUCT_SYMMETRY_Y"
   write(7,40) "DUCT_SYMMETRY_Y"
   write(7,50) (nxp-(izero-1) - (nxp+1-ixmax))*nzp

     j = nyp+kd1
    do k = kd1, nzp
     do i = izero, ixmax-1
      write(7,'(5i20)') 9, node_number(i    ,j,k    ,nxp,nyp)-kd1, node_number(i+kd1,j,k    ,nxp,nyp)-kd1, &
                           node_number(i+kd1,j,k+kd1,nxp,nyp)-kd1, node_number(i    ,j,k+kd1,nxp,nyp)-kd1
     end do
    end do
 !--------------------------------------------------------------------------


 !--------------------------------------------------------------------------
 ! (5) Inflow (x=xmin)
 !--------------------------------------------------------------------------

   write(*,40) "INFLOW"
   write(7,40) "INFLOW"
   write(7,50) nyp*nzp

     i = kd1
    do k = kd1, nzp
     do j = kd1, nyp
      write(7,'(5i20)') 9, node_number(i,j    ,k    ,nxp,nyp)-kd1, node_number(i,j+kd1,k    ,nxp,nyp)-kd1, &
                           node_number(i,j+kd1,k+kd1,nxp,nyp)-kd1, node_number(i,j    ,k+kd1,nxp,nyp)-kd1
     end do
    end do

 !--------------------------------------------------------------------------
 ! (6) Outflow (x=xmax)
 !--------------------------------------------------------------------------

   write(*,40) "OUTFLOW"
   write(7,40) "OUTFLOW"
   write(7,50) nyp*nzp

     i = nxp+kd1
    do k = kd1, nzp
     do j = kd1, nyp
      write(7,'(5i20)') 9, node_number(i,j    ,k    ,nxp,nyp)-kd1, node_number(i,j    ,k+kd1,nxp,nyp)-kd1, &
                           node_number(i,j+kd1,k+kd1,nxp,nyp)-kd1, node_number(i,j+kd1,k    ,nxp,nyp)-kd1
     end do
    end do

 !--------------------------------------------------------------------------


 if (izero > 1) then

 !--------------------------------------------------------------------------
 ! (7) Symmetry z (z=zmin)
 !--------------------------------------------------------------------------

   write(*,40) "INLET_SYMMETRY_Z"
   write(7,40) "INLET_SYMMETRY_Z"
   write(7,50) (izero-1)*nyp

   k = kd1
    do j = kd1, nyp
     do i = 1, izero-1
      write(7,'(5i20)') 9, node_number(i    ,j    ,k,nxp,nyp)-kd1, node_number(i+kd1,j    ,k,nxp,nyp)-kd1, &
                           node_number(i+kd1,j+kd1,k,nxp,nyp)-kd1, node_number(i    ,j+kd1,k,nxp,nyp)-kd1
     end do
    end do

 !--------------------------------------------------------------------------
 ! (8) Symmetry z (z=zmax)
 !--------------------------------------------------------------------------

   write(*,40) "INLET_SYMMETRY_Z"
   write(7,40) "INLET_SYMMETRY_Z"
   write(7,50) (izero-1)*nyp

     k = nzp+kd1
    do j = kd1, nyp
     do i = 1, izero-1
      write(7,'(5i20)') 9, node_number(i    ,j    ,k,nxp,nyp)-kd1, node_number(i    ,j+kd1,k,nxp,nyp)-kd1, &
                           node_number(i+kd1,j+kd1,k,nxp,nyp)-kd1, node_number(i+kd1,j    ,k,nxp,nyp)-kd1
     end do
    end do
 !--------------------------------------------------------------------------


 !--------------------------------------------------------------------------
 ! (9) Symmetry y (y=ymin)
 !--------------------------------------------------------------------------

   write(*,40) "INLET_SYMMETRY_Y"
   write(7,40) "INLET_SYMMETRY_Y"
   write(7,50) (izero-1)*nzp

     j = kd1
    do k = kd1, nzp
     do i = 1, izero-1
      write(7,'(5i20)') 9, node_number(i    ,j,k    ,nxp,nyp)-kd1, node_number(i    ,j,k+kd1,nxp,nyp)-kd1, &
                           node_number(i+kd1,j,k+kd1,nxp,nyp)-kd1, node_number(i+kd1,j,k    ,nxp,nyp)-kd1
     end do
    end do

 !--------------------------------------------------------------------------
 ! (10) Symmetry y (y=ymax)
 !-------------------------------------------------------------------------

   write(*,40) "INLET_SYMMETRY_Y"
   write(7,40) "INLET_SYMMETRY_Y"
   write(7,50) (izero-1)*nzp

     j = nyp+kd1
    do k = kd1, nzp
     do i = 1, izero-1
      write(7,'(5i20)') 9, node_number(i    ,j,k    ,nxp,nyp)-kd1, node_number(i+kd1,j,k    ,nxp,nyp)-kd1, &
                           node_number(i+kd1,j,k+kd1,nxp,nyp)-kd1, node_number(i    ,j,k+kd1,nxp,nyp)-kd1
     end do
    end do
 !--------------------------------------------------------------------------

 endif

 if (ixmax < nxp+1) then

 !--------------------------------------------------------------------------
 ! (7) Symmetry z (z=zmin)
 !--------------------------------------------------------------------------

   write(*,40) "OUTLET_SYMMETRY_Z"
   write(7,40) "OUTLET_SYMMETRY_Z"
   write(7,50) (nxp+1-ixmax)*nyp

   k = kd1
    do j = kd1, nyp
     do i = ixmax, nxp
      write(7,'(5i20)') 9, node_number(i    ,j    ,k,nxp,nyp)-kd1, node_number(i+kd1,j    ,k,nxp,nyp)-kd1, &
                           node_number(i+kd1,j+kd1,k,nxp,nyp)-kd1, node_number(i    ,j+kd1,k,nxp,nyp)-kd1
     end do
    end do

 !--------------------------------------------------------------------------
 ! (8) Symmetry z (z=zmax)
 !--------------------------------------------------------------------------

   write(*,40) "OUTLET_SYMMETRY_Z"
   write(7,40) "OUTLET_SYMMETRY_Z"
   write(7,50) (nxp+1-ixmax)*nyp

     k = nzp+kd1
    do j = kd1, nyp
     do i = ixmax, nxp
      write(7,'(5i20)') 9, node_number(i    ,j    ,k,nxp,nyp)-kd1, node_number(i    ,j+kd1,k,nxp,nyp)-kd1, &
                           node_number(i+kd1,j+kd1,k,nxp,nyp)-kd1, node_number(i+kd1,j    ,k,nxp,nyp)-kd1
     end do
    end do
 !--------------------------------------------------------------------------


 !--------------------------------------------------------------------------
 ! (9) Symmetry y (y=ymin)
 !--------------------------------------------------------------------------

   write(*,40) "OUTLET_SYMMETRY_Y"
   write(7,40) "OUTLET_SYMMETRY_Y"
   write(7,50) (nxp+1-ixmax)*nzp

     j = kd1
    do k = kd1, nzp
     do i = ixmax, nxp
      write(7,'(5i20)') 9, node_number(i    ,j,k    ,nxp,nyp)-kd1, node_number(i    ,j,k+kd1,nxp,nyp)-kd1, &
                           node_number(i+kd1,j,k+kd1,nxp,nyp)-kd1, node_number(i+kd1,j,k    ,nxp,nyp)-kd1
     end do
    end do

 !--------------------------------------------------------------------------
 ! (10) Symmetry y (y=ymax)
 !-------------------------------------------------------------------------

   write(*,40) "OUTLET_SYMMETRY_Y"
   write(7,40) "OUTLET_SYMMETRY_Y"
   write(7,50) (nxp+1-ixmax)*nzp

     j = nyp+kd1
    do k = kd1, nzp
     do i = ixmax, nxp
      write(7,'(5i20)') 9, node_number(i    ,j,k    ,nxp,nyp)-kd1, node_number(i+kd1,j,k    ,nxp,nyp)-kd1, &
                           node_number(i+kd1,j,k+kd1,nxp,nyp)-kd1, node_number(i    ,j,k+kd1,nxp,nyp)-kd1
     end do
    end do
 !--------------------------------------------------------------------------

 endif

 !--------------------------------------------------------------------------
 !--------------------------------------------------------------------------

  close(7)

 end subroutine write_su2grid_file_hex
!********************************************************************************

!*******************************************************************************
!
! Write out a file containing line info (only within the viscous layer)):
! This information is for the implicit line relaxations or line agglomeration.
! This format is used by FUN3D.
!
!*******************************************************************************
 subroutine line_info_file_c_bottom_hex(n_points, inc, file_name)
 implicit none

 integer(kd)  , intent(in) :: n_points
 integer(kd)  , intent(in) :: inc
 character(80), intent(in) :: file_name

 integer(kd)   :: i_count, os
 integer(kd)   :: i, j, k, n_lines, n_total_points, max_points, min_points
 integer(kd)   :: nxp, nyp, nzp
 integer(kd)   ::  ip,  jp,  kp
 character(80) :: filename_debug, i_char
 real(dp)      ::  xc,  yc,  zc

  nxp = nx/inc
  nyp = ny/inc
  nzp = nz/inc

    !----------------------------------------------------------
     if (debug_mode) then
      filename_debug = "debug_" // trim(file_name) // ".dat"
      open(unit=2, file=filename_debug, status="unknown", iostat=os)
      write(2,*) 'title = "Lines"'
      write(2,'(2a)') 'variables = "x","y","z"'
     endif
    !----------------------------------------------------------
 
  open(unit=1, file=file_name, status="unknown", iostat=os)
  max_points = n_points
  min_points = n_points
  n_lines    = (nxp)*(nyp)

  n_total_points = n_lines * n_points

  write(*,*) " Total number of lines = ", n_lines
  write(*,*) "            max points = ", max_points
  write(*,*) "            min points = ", min_points

  write(1,*) n_lines, n_total_points, "Total lines and points"
  write(1,*) min_points, max_points, "Min and max points in line"

  i_count = kd0

! Go up and write out the node info for every node on the body.
! NOTE: Go up to the given limit: n_points

    do j = kd1, ny-inc+1, inc
     do i = kd1, nx-inc+1, inc

     i_count = i_count + kd1

      jp = (j-kd1)/inc + kd1
      ip = (i-kd1)/inc + kd1

    !----------------------------------------------------------
    !1. First node (on the bottom)
      k  = kd1
      kp = kd1
      write(1,*) n_points, " Points in line for line = ", i_count
      xc = ( x(i) + x(i+inc) + x(i+inc) + x(i)     + x(i)     + x(i+inc) + x(i+inc) + x(i)     )/8.0_dp
      yc = ( y(j) + y(j)     + y(j+inc) + y(j+inc) + y(j)     + y(j)     + y(j+inc) + y(j+inc) )/8.0_dp
      zc = ( z(k) + z(k)     + z(k)     + z(k)     + z(k+inc) + z(k+inc) + z(k+inc) + z(k+inc) )/8.0_dp
      write(1,'(i12,a10,3es30.20)') node_number(ip,jp,kp,nxp,nyp), " x/y/z= ", xc, yc, zc

    !----------------------------------------------------------
    !2. Second node to the node below the last one.
     if (n_points > 2) then
      do kp = kd2, n_points-kd1
        write(1,'(i12)')  node_number(ip,jp,kp,nxp,nyp)
      end do
     endif

    !----------------------------------------------------------
    !3. Last node
     if (n_points > 1) then
      kp = n_points
       k = kd1 + inc*(kp-kd1)
      xc = ( x(i) + x(i+inc) + x(i+inc) + x(i)     + x(i)     + x(i+inc) + x(i+inc) + x(i)     )/8.0_dp
      yc = ( y(j) + y(j)     + y(j+inc) + y(j+inc) + y(j)     + y(j)     + y(j+inc) + y(j+inc) )/8.0_dp
      zc = ( z(k) + z(k)     + z(k)     + z(k)     + z(k+inc) + z(k+inc) + z(k+inc) + z(k+inc) )/8.0_dp
      write(1,'(i12,a10,3es30.20)') node_number(ip,jp,kp,nxp,nyp), " x/y/z= ", xc, yc, zc
     endif

    !----------------------------------------------------------
     if (debug_mode) then
       write( i_char, '(i0)' ) i_count
       write(2,*) 'zone t="line ' // trim(i_char) // '"'
       write(2,*) 'i=1', ' j=1', ' k=',n_points, ' f=point'
      do kp = kd1, n_points
       k = kd1 + inc*(kp-kd1)
      xc = ( x(i) + x(i+inc) + x(i+inc) + x(i)     + x(i)     + x(i+inc) + x(i+inc) + x(i)     )/8.0_dp
      yc = ( y(j) + y(j)     + y(j+inc) + y(j+inc) + y(j)     + y(j)     + y(j+inc) + y(j+inc) )/8.0_dp
      zc = ( z(k) + z(k)     + z(k)     + z(k)     + z(k+inc) + z(k+inc) + z(k+inc) + z(k+inc) )/8.0_dp
       write(2,'(3es30.20)') xc, yc, zc
      end do
     endif
    !----------------------------------------------------------

     end do
    end do

   if (i_count /= n_lines) write(*,*) "Error: i_count /= n_lines", i_count, n_lines
   write(*,*)
   write(*,*) "lines_fmt file has been written in : ", file_name

 close(1)
 if (debug_mode) close(2)

 end subroutine line_info_file_c_bottom_hex
!********************************************************************************

!*******************************************************************************
!
! Write out a file containing line info (only within the viscous layer)):
! This information is for the implicit line relaxations or line agglomeration.
! This format is used by FUN3D.
!
!*******************************************************************************
 subroutine line_info_file_c_left_hex(n_points, inc, file_name)
 implicit none

 integer(kd)  , intent(in) :: n_points
 integer(kd)  , intent(in) :: inc
 character(80), intent(in) :: file_name

 integer(kd) :: i_count, os
 integer(kd) :: i, j, k, n_lines, n_total_points, max_points, min_points
 integer(kd)   :: nxp, nyp, nzp
 integer(kd)   ::  ip,  jp,  kp
 character(80) :: filename_debug, i_char
 real(dp)      :: xc, yc ,zc

  nxp = nx/inc
  nyp = ny/inc
  nzp = nz/inc

    !----------------------------------------------------------
     if (debug_mode) then
      filename_debug = "debug_" // trim(file_name) // ".dat"
      open(unit=2, file=filename_debug, status="unknown", iostat=os)
      write(2,*) 'title = "Lines"'
      write(2,'(2a)') 'variables = "x","y","z"'
     endif
    !----------------------------------------------------------
 
  open(unit=1, file=file_name, status="unknown", iostat=os)
  max_points = n_points
  min_points = n_points
  n_lines    = (nxp)*(nyp)

  n_total_points = n_lines * n_points

  write(*,*) " Total number of lines = ", n_lines
  write(*,*) "            max points = ", max_points
  write(*,*) "            min points = ", min_points

  write(1,*) n_lines, n_total_points, "Total lines and points"
  write(1,*) min_points, max_points, "Min and max points in line"

  i_count = kd0

! Go up and write out the node info for every node on the body.
! NOTE: Go up to the given limit: n_points

    do k = kd1, nz-inc+1, inc
     do i = kd1, nx-inc+1, inc

     i_count = i_count + kd1

      kp = (k-kd1)/inc + kd1
      ip = (i-kd1)/inc + kd1

    !----------------------------------------------------------
    !1. First node (on the bottom)
     j  = kd1
     jp = kd1
     write(1,*) n_points, " Points in line for line = ", i_count
      xc = ( x(i) + x(i+inc) + x(i+inc) + x(i)     + x(i)     + x(i+inc) + x(i+inc) + x(i)     )/8.0_dp
      yc = ( y(j) + y(j)     + y(j)     + y(j)     + y(j+inc) + y(j+inc) + y(j+inc) + y(j+inc) )/8.0_dp
      zc = ( z(k) + z(k)     + z(k+inc) + z(k+inc) + z(k)     + z(k)     + z(k+inc) + z(k+inc) )/8.0_dp
      write(1,'(i12,a10,3es30.20)') &
      node_number(ip,jp,kp,nxp,nyp), " x/y/z= ", xc, yc, zc

    !----------------------------------------------------------
    !2. Second node to the node below the last one.
     if (n_points > 2) then
      do jp = kd2, n_points-kd1
        write(1,'(i12)')  node_number(ip,jp,kp,nxp,nyp)
      end do
     endif

    !----------------------------------------------------------
    !3. Last node
     if (n_points > kd1) then
      jp = n_points
       j = kd1 + inc*(jp-kd1)
      xc = ( x(i) + x(i+inc) + x(i+inc) + x(i)     + x(i)     + x(i+inc) + x(i+inc) + x(i)     )/8.0_dp
      yc = ( y(j) + y(j)     + y(j)     + y(j)     + y(j+inc) + y(j+inc) + y(j+inc) + y(j+inc) )/8.0_dp
      zc = ( z(k) + z(k)     + z(k+inc) + z(k+inc) + z(k)     + z(k)     + z(k+inc) + z(k+inc) )/8.0_dp
      write(1,'(i12,a10,3es30.20)') &
       node_number(ip,jp,kp,nxp,nyp), " x/y/z= ", xc, yc, zc
     endif

    !----------------------------------------------------------
     if (debug_mode) then
       write( i_char, '(i0)' ) i_count
       write(2,*) 'zone t="line ' // trim(i_char) // '"'
       write(2,*) 'i=1', ' j=',n_points, ' k=1', ' f=point'
      do jp = kd1, n_points
       j = kd1 + inc*(jp-kd1)
      xc = ( x(i) + x(i+inc) + x(i+inc) + x(i)     + x(i)     + x(i+inc) + x(i+inc) + x(i)     )/8.0_dp
      yc = ( y(j) + y(j)     + y(j)     + y(j)     + y(j+inc) + y(j+inc) + y(j+inc) + y(j+inc) )/8.0_dp
      zc = ( z(k) + z(k)     + z(k+inc) + z(k+inc) + z(k)     + z(k)     + z(k+inc) + z(k+inc) )/8.0_dp
       write(2,'(3es30.20)') xc, yc, zc
      end do
     endif
    !----------------------------------------------------------

     end do
    end do

   if (i_count /= n_lines) write(*,*) "Error: i_count /= n_lines"
   write(*,*)
   write(*,*) "lines_fmt file has been written in : ", file_name

 close(1)
 if (debug_mode) close(2)

 end subroutine line_info_file_c_left_hex
!********************************************************************************



!*******************************************************************************
!
! Write out a file containing line info (only within the viscous layer)):
! This information is for the implicit line relaxations or line agglomeration.
! This format is used by FUN3D.
!
!*******************************************************************************
 subroutine line_info_file_bottom(n_points, inc, file_name)
 implicit none

 integer(kd)  , intent(in) :: n_points
 integer(kd)  , intent(in) :: inc
 character(80), intent(in) :: file_name

 integer(kd) :: i_count, os
 integer(kd) :: i, j, k, n_lines, n_total_points, max_points, min_points
 integer(kd)   :: nxp, nyp, nzp
 integer(kd)   ::  ip,  jp,  kp
 character(80) :: filename_debug, i_char

  nxp = nx/inc
  nyp = ny/inc
  nzp = nz/inc

    !----------------------------------------------------------
     if (debug_mode) then
      filename_debug = "debug_" // trim(file_name) // ".dat"
      open(unit=2, file=filename_debug, status="unknown", iostat=os)
      write(2,*) 'title = "Lines"'
      write(2,'(2a)') 'variables = "x","y","z"'
     endif
    !----------------------------------------------------------
 
  open(unit=1, file=file_name, status="unknown", iostat=os)
  max_points = n_points
  min_points = n_points
  n_lines    = (nxp+kd1)*(nyp+kd1)

  n_total_points = n_lines * n_points

  write(*,*) " Total number of lines = ", n_lines
  write(*,*) "            max points = ", max_points
  write(*,*) "            min points = ", min_points

  write(1,*) n_lines, n_total_points, "Total lines and points"
  write(1,*) min_points, max_points, "Min and max points in line"

  i_count = 0

! Go up and write out the node info for every node on the body.
! NOTE: Go up to the given limit: n_points

    do j = kd1, ny+kd1, inc
     do i = kd1, nx+kd1, inc

     i_count = i_count + kd1

      jp = (j-kd1)/inc + kd1
      ip = (i-kd1)/inc + kd1

    !----------------------------------------------------------
    !1. First node (on the bottom)
     write(1,*) n_points, " Points in line for line = ", i_count
     write(1,'(i12,a10,3es30.20)') &
      node_number(ip,jp,kd1,nxp,nyp), " x/y/z= ", x(i), y(j), z(1)

    !----------------------------------------------------------
    !2. Second node to the node below the last one.
     if (n_points > 2) then
      do kp = kd2, n_points-kd1
        write(1,'(i12)')  node_number(ip,jp,kp,nxp,nyp)
      end do
     endif

    !----------------------------------------------------------
    !3. Last node
     if (n_points > 1) then
      kp = n_points
       k = kd1 + inc*(kp-kd1)
      write(1,'(i12,a10,3es30.20)') &
       node_number(ip,jp,kp,nxp,nyp), " x/y/z= ", x(i), y(j), z(k)
     endif

    !----------------------------------------------------------
     if (debug_mode) then
       write( i_char, '(i0)' ) i_count
       write(2,*) 'zone t="line ' // trim(i_char) // '"'
       write(2,*) 'i=1', ' j=1', ' k=',n_points, ' f=point'
      do kp = kd1, n_points
      k = kd1 + inc*(kp-kd1)
      write(2,'(3es30.20)') x(i), y(j), z(k)
      end do
     endif
    !----------------------------------------------------------

     end do
    end do

   if (i_count /= n_lines) then
    write(*,*) "Error: i_count /= n_lines in line_info_file_bottom"
    write(*,*) " i_count = ", i_count
    write(*,*) " n_lines = ", n_lines
    stop
   endif
   write(*,*)
   write(*,*) "lines_fmt file has been written in : ", file_name

 close(1)
 if (debug_mode) close(2)

 end subroutine line_info_file_bottom
!********************************************************************************

!*******************************************************************************
!
! Write out a file containing line info (only within the viscous layer)):
! This information is for the implicit line relaxations or line agglomeration.
! This format is used by FUN3D.
!
!*******************************************************************************
 subroutine line_info_file_left(n_points, inc, file_name)
 implicit none

 integer(kd)  , intent(in) :: n_points
 integer(kd)  , intent(in) :: inc
 character(80), intent(in) :: file_name

 integer(kd)   :: i_count, os
 integer(kd)   :: i, j, k, n_lines, n_total_points, max_points, min_points
 integer(kd)   :: nxp, nyp, nzp
 integer(kd)   ::  ip,  jp,  kp
 character(80) :: filename_debug, i_char

  nxp = nx/inc
  nyp = ny/inc
  nzp = nz/inc

    !----------------------------------------------------------
     if (debug_mode) then
      filename_debug = "debug_" // trim(file_name) // ".dat"
      open(unit=2, file=filename_debug, status="unknown", iostat=os)
      write(2,*) 'title = "Lines"'
      write(2,'(2a)') 'variables = "x","y","z"'
     endif
    !----------------------------------------------------------
 
  open(unit=1, file=file_name, status="unknown", iostat=os)
  max_points = n_points
  min_points = n_points
  n_lines    = (nxp+kd1)*(nzp+kd1)

  n_total_points = n_lines * n_points

  write(*,*) " Total number of lines = ", n_lines
  write(*,*) "            max points = ", max_points
  write(*,*) "            min points = ", min_points

  write(1,*) n_lines, n_total_points, "Total lines and points"
  write(1,*) min_points, max_points, "Min and max points in line"

  i_count = kd0

! Go up and write out the node info for every node on the body.
! NOTE: Go up to the given limit: n_points

    do k = kd1, nz+kd1, inc
     do i = kd1, nx+kd1, inc

     i_count = i_count + kd1

      kp = (k-kd1)/inc + kd1
      ip = (i-kd1)/inc + kd1

    !----------------------------------------------------------
    !1. First node (on the bottom)
     write(1,*) n_points, " Points in line for line = ", i_count
     write(1,'(i12,a10,3es30.20)') &
     node_number(ip,nyp+kd1,kp,nxp,nyp), " x/y/z= ", x(i), y(ny+kd1), z(k)

    !----------------------------------------------------------
    !2. Second node to the node below the last one.
     if (n_points > 2) then
      do jp = nyp, (nyp+kd1)-n_points+2, -kd1
        write(1,'(i12)')  node_number(ip,jp,kp,nxp,nyp)
      end do
     endif

    !----------------------------------------------------------
    !3. Last node
     if (n_points > kd1) then
      write(1,'(i12,a10,3es30.20)') &
        node_number(ip,(nyp+kd1)-n_points+kd1,kp,nxp,nyp), " x/y/z= ", x(i), y(1), z(k)
     endif

    !----------------------------------------------------------
     if (debug_mode) then
       write( i_char, '(i0)' ) i_count
       write(2,*) 'zone t="line ' // trim(i_char) // '"'
       write(2,*) 'i=1 ', ' j=',n_points, ' k=1', 'f=point'
      do  jp = nyp+kd1, (nyp+kd1)-n_points+kd1, -kd1
       j = kd1 + inc*(jp-kd1)
      write(2,'(3es30.20)') x(i), y(j), z(k)
      end do
     endif
    !----------------------------------------------------------

     end do
    end do

   if (i_count /= n_lines) then
    write(*,*) "Error: i_count /= n_lines in line_info_file_left"
    write(*,*) " i_count = ", i_count
    write(*,*) " n_lines = ", n_lines 
    stop
   endif
   write(*,*)
   write(*,*) "lines_fmt file has been written in : ", file_name

 close(1)
 if (debug_mode) close(2)

 end subroutine line_info_file_left
!********************************************************************************



!********************************************************************************
! This solves target_x - cx*(one-exp(sf*xi))/(one-exp(sf)) for sf.
!********************************************************************************
 subroutine compute_stretching_factor(target_x,cx,xi,sf_initial, final_x,sf,itr,stat )
 implicit none

 real(dp)   , intent( in) :: target_x, cx, xi, sf_initial
 real(dp)   , intent(out) :: final_x, sf
 integer(kd), intent(out) :: itr, stat

 real(dp)                 :: res, dres
 
  stat = kd0
   itr = kd0
    sf = sf_initial

 do

   !Equation to sovle: res = 0.0.

       gr = (one-exp(sf*xi))/(one-exp(sf))
      res =  target_x - cx*gr

      final_x = gr*cx

     if (debug_mode) write(*,'(a,i6,a,es10.2,a,es10.2,a,es10.2,a,es10.2)') &
       "   itr=", itr, " current_x = ", final_x, " target_x = ", target_x, &
       " sf=",sf, " res=", abs(res/target_x)

     if (abs(res)/target_x < 1.0e-08_dp) exit !<--Smaller the better.

   ! The following doesn't work. This creates a trivial solution sf=0
   ! and Newton's method converges to it.
   !     res =  dr1*(one-exp(sf)) - rmax*(one-exp(sf*xi))
   !    dres = -dr1*     exp(sf)  +   xi*rmax*exp(sf*xi)

   !So, we solve directly: target_x - cx*(one-exp(sf*xi))/(one-exp(sf))
   !Its derivative is 
    dres = - cx*( -xi*exp(sf*xi)*(one-exp(sf)) + exp(sf)*(one-exp(sf*xi)) )/(one-exp(sf))**2

   !Newton iteration:
      sf = sf - res/dres

  itr = itr + kd1
  if (itr > 500) then
    stat = kd1
    exit
  endif

 end do

   if (sf < 0.0_dp) stat = 2 !Negative stretching factor.

 end subroutine compute_stretching_factor
!********************************************************************************


!********************************************************************************
! This solves an equation for an exponentially stretched points in z for the
! geometeric factor.
!
!   Spacing is detemined by a geometric sequence:
!
!    z(1) = zmin
!    z(k) = z(k-1) + dr1*r**(k-1), k = 2, 3,..., nz+1
!
!   which gives
!
!    z(k) = zmin + dr1*sum_{k=1}^{(nz+1)-1} r**k
!         = zmin + dr1*(r^(nz+1)-1)/(r-1)
!
!   The factor r will be determined for a given set of zmin, zmax, dr1, nz
!   by solving
!              zmax = zmin + dr1*(r^(nz+1)-1)/(r-1)
!
!********************************************************************************
 subroutine compute_geometric_factor(n,dr1,zmin,zmax, r,itr,stat )
 implicit none

 integer(kd), intent( in) :: n
 real(dp)   , intent( in) :: dr1,zmin,zmax
 real(dp)   , intent(out) :: r
 integer(kd), intent(out) :: itr,stat

 real(dp)                 :: res, dres, r_pre
 
    stat = kd0
     itr = kd0
       r = 1.2_dp !Initial guess (do not use 1.0)
   r_pre = r

 do

   !Equation to sovle: res = 0.0.

      res = r**n - ( r + (zmax-zmin)*(r-1.0_dp)/dr1 )

     if (debug_mode) write(*,'(a,i6,a,es10.2,a,es10.2)') &
       "   itr=", itr, " current r = ", r, " res=", abs(res)

   !Derivative of res: dres/dr
    dres = real(n,dp)*r**(n-1) - 1.0_dp

   !Newton iteration:
     r_pre = r
         r = r - res/dres

     if (abs(r-r_pre)/r_pre < 1.0e-15_dp) exit !<--Smaller the better.

  itr = itr + kd1
  if (itr > 500) then
    stat = kd1 !stat=1 indicates the iteration did not converge.
    exit
  endif

 end do

  if (r < 0.0_dp) stat = 2 !Negative stretching factor.

  write(*,*)
  write(*,*) "  ----- End of Newton iteration : "
  write(*,'(a,i6,a,es10.2,a,es10.2)')  "  ----- itr=", itr, " r = ", r, " res=", abs(res)
  write(*,*)

 !Stop when debugging.
  if (debug_mode .and. stat > 0) then
   stop
  endif

 end subroutine compute_geometric_factor
!********************************************************************************


!********************************************************************************
! Find out big_endian_io.
!********************************************************************************
 function big_endian_io( opt_unit )

 integer, intent(in) :: opt_unit
 logical             :: big_endian_io
! one-byte integer
 integer, parameter :: i1 = selected_int_kind(2)
! two-byte integer
 integer, parameter :: i2 = selected_int_kind(4)
 integer(i1) :: byte_one, byte_two
! 00000000 00000001 big-endian binary
 integer(i2) :: two_byte_int = 1_i2

    open(opt_unit,status='scratch',form='unformatted')
      write( opt_unit) two_byte_int
      rewind(opt_unit)
      read(  opt_unit) byte_one, byte_two
    close(opt_unit)
    big_endian_io = ( byte_one == 0 .and. byte_two == 1 )

 end function big_endian_io
!********************************************************************************






!*******************************************************************************
!
! PRS: This subroutine writes a Tecplot file for boundaries.
!
!******************************************************************************
 subroutine write_tecplot_boundary_file_prs(inc,filename)

 implicit none
 integer(kd)  , intent(in) :: inc
 character(80), intent(in) :: filename
 integer(kd)               :: nnodes, nquads, i, j, k
 integer(kd)               :: nxp, nyp, nzp

 integer(kd)               :: ntrias

 open(unit=2, file=filename, status="unknown", iostat=os)
 write(2,'(2a)') 'variables = "x","y","z"'

  nxp = nx / inc
  nyp = ny / inc
  nzp = nz / inc

 !----------------------------------------------------------------------------
 ! Constant z planes
 !----------------------------------------------------------------------------
  nnodes = (nxp+kd1)*(nyp+kd1)
  nquads = (nxp - (izero-1) - (nxp+1-ixmax) )*nyp
  write(2,*) 'ZONE T="Boundary 1 (Bottom)"  N=', nnodes,',E=', nquads, &
              ' , ET=quadrilateral, F=FEPOINT'

   k = kd1

  do j = kd1, ny+kd1, inc
   do i = kd1, nx+kd1, inc
    write(2,'(3es20.10)') x(i), y(j), z(k)
   end do
  end do

  do j = kd1, nyp
   do i = izero, ixmax-1 !nxp
    write(2,'(4i10)') node_number2d(i    ,j    ,nxp), node_number2d(i+kd1,j    ,nxp), &
                      node_number2d(i+kd1,j+kd1,nxp), node_number2d(i    ,j+kd1,nxp)
   end do
  end do

 !----------------------------------------------------------------------------
  nnodes = (nxp+kd1)*(nyp+kd1)
  nquads = (nxp - (izero-1) - (nxp+1-ixmax))*nyp
  write(2,*) 'ZONE T="Boundary 2 (Top)"  N=', nnodes,',E=', nquads, &
              ' , ET=quadrilateral, F=FEPOINT'

   k = nz+kd1

  do j = kd1, ny+kd1, inc
   do i = kd1, nx+kd1, inc
    write(2,'(3es20.10)') x(i), y(j), z(k)
   end do
  end do

  do j = kd1, nyp
   do i = izero, ixmax-1 !, nxp
    write(2,'(4i10)') node_number2d(i  ,j  ,nxp), node_number2d(i+kd1,j  ,nxp), &
                      node_number2d(i+kd1,j+kd1,nxp), node_number2d(i  ,j+kd1,nxp)
   end do
  end do


 !----------------------------------------------------------------------------
 ! Constant y planes
 !----------------------------------------------------------------------------
  nnodes = (nxp+kd1)*(nzp+kd1)
  nquads = (nxp - (izero-1) - (nxp+1-ixmax))*nzp
  write(2,*) 'ZONE T="Boundary 4 (Right)"  N=', nnodes,',E=', nquads, &
              ' , ET=quadrilateral, F=FEPOINT'

   j = kd1

  do k = kd1, nz+kd1, inc
   do i = kd1, nx+kd1, inc
    write(2,'(3es20.10)') x(i), y(j), z(k)
   end do
  end do

  do k = kd1, nzp
   do i = izero, ixmax-1 !, nxp
    write(2,'(4i10)') node_number2d(i    ,k    ,nxp), node_number2d(i+kd1,k    ,nxp), &
                      node_number2d(i+kd1,k+kd1,nxp), node_number2d(i    ,k+kd1,nxp)
   end do
  end do

 !----------------------------------------------------------------------------
  nnodes = (nxp+kd1)*(nzp+kd1)
  nquads = (nxp - (izero-1) - (nxp+1-ixmax))*nzp
  write(2,*) 'ZONE T="Boundary 5 (Left)"  N=', nnodes,',E=', nquads, &
              ' , ET=quadrilateral, F=FEPOINT'

   j = ny+kd1

  do k = kd1, nz+kd1, inc
   do i = kd1, nx+kd1, inc
    write(2,'(3es20.10)') x(i), y(j), z(k)
   end do
  end do

  do k = kd1, nzp
   do i = izero, ixmax-1 !, nxp
    write(2,'(4i10)') node_number2d(i    ,k    ,nxp), node_number2d(i+kd1,k    ,nxp), &
                      node_number2d(i+kd1,k+kd1,nxp), node_number2d(i    ,k+kd1,nxp)
   end do
  end do

 !----------------------------------------------------------------------------
 ! Constant x planes
 !----------------------------------------------------------------------------
  nnodes = (nyp+kd1)*(nzp+kd1)
  ntrias = 2 * nyp*nzp
  write(2,*) 'ZONE T="Boundary 5 (Near)"  N=', nnodes,',E=', ntrias, &
              ' , ET=quadrilateral, F=FEPOINT'

   i = kd1

  do k = kd1, nz+kd1, inc
   do j = kd1, ny+kd1, inc
    write(2,'(3es20.10)') x(i), y(j), z(k)
   end do
  end do

  do k = kd1, nzp
   do j = kd1, nyp

!                     node_number2d(j    ,k    ,nyp), & !1
!                     node_number2d(j+kd1,k    ,nyp), & !2
!                     node_number2d(j+kd1,k+kd1,nyp), & !3 
!                     node_number2d(j    ,k+kd1,nyp)    !4

    write(2,'(4i10)') node_number2d(j    ,k    ,nyp), & !1
                      node_number2d(j+kd1,k    ,nyp), & !2
                      node_number2d(j+kd1,k+kd1,nyp), & !3 
                      node_number2d(j+kd1,k+kd1,nyp)    !3 <-duplicate to make quad data.

    write(2,'(4i10)') node_number2d(j    ,k    ,nyp), & !1
                      node_number2d(j+kd1,k+kd1,nyp), & !3 
                      node_number2d(j    ,k+kd1,nyp), & !4
                      node_number2d(j    ,k+kd1,nyp)    !4 <-duplicate to make quad data.
   end do
  end do

 !----------------------------------------------------------------------------
  nnodes = (nyp+kd1)*(nzp+kd1)
  ntrias = 2 * nyp*nzp
  write(2,*) 'ZONE T="Boundary 6 (Far)"  N=', nnodes,',E=', ntrias, &
              ' , ET=quadrilateral, F=FEPOINT'

   i = nx+kd1

  do k = kd1, nz+kd1, inc
   do j = kd1, ny+kd1, inc
    write(2,'(3es20.10)') x(i), y(j), z(k)
   end do
  end do

  do k = kd1, nzp
   do j = kd1, nyp

!                    node_number2d(j    ,k    ,nyp), & !1
!                    node_number2d(j+kd1,k    ,nyp), & !2
!                    node_number2d(j+kd1,k+kd1,nyp), & !3
!                    node_number2d(j    ,k+kd1,nyp)    !4

    write(2,'(4i10)') node_number2d(j    ,k    ,nyp), & !1
                      node_number2d(j+kd1,k    ,nyp), & !2
                      node_number2d(j+kd1,k+kd1,nyp), & !3
                      node_number2d(j+kd1,k+kd1,nyp)    !3 <-duplicate to make quad data.

    write(2,'(4i10)') node_number2d(j    ,k    ,nyp), & !1
                      node_number2d(j+kd1,k+kd1,nyp), & !3
                      node_number2d(j    ,k+kd1,nyp), & !4
                      node_number2d(j    ,k+kd1,nyp)    !4 <-duplicate to make quad data.

   end do
  end do


 !--------------------------------------------------------------------------------------
 !--------------------------------------------------------------------------------------
 ! Upstream (inlet) region
 !--------------------------------------------------------------------------------------
 !--------------------------------------------------------------------------------------
 if (izero > 1) then

 !----------------------------------------------------------------------------
   nnodes = (nxp+kd1)*(nyp+kd1)
   nquads = (izero-1)*nyp
   write(2,*) 'ZONE T="Boundary 1 (Inlet: Bottom)"  N=', nnodes,',E=', nquads, &
               ' , ET=quadrilateral, F=FEPOINT'

    k = kd1

   do j = kd1, ny+kd1, inc
    do i = kd1, nx+kd1, inc
     write(2,'(3es20.10)') x(i), y(j), z(k)
    end do
   end do

    do j = kd1, nyp
    do i = kd1, izero-1
     write(2,'(4i10)') node_number2d(i    ,j    ,nxp), node_number2d(i+kd1,j    ,nxp), &
                       node_number2d(i+kd1,j+kd1,nxp), node_number2d(i    ,j+kd1,nxp)
    end do
    end do

 !----------------------------------------------------------------------------
   nnodes = (nxp+kd1)*(nyp+kd1)
   nquads = (izero-1)*nyp
   write(2,*) 'ZONE T="Boundary 2 (Inlet: Top)"  N=', nnodes,',E=', nquads, &
               ' , ET=quadrilateral, F=FEPOINT'

    k = nz+kd1

   do j = kd1, ny+kd1, inc
    do i = kd1, nx+kd1, inc
     write(2,'(3es20.10)') x(i), y(j), z(k)
    end do
   end do

   do j = kd1, nyp
    do i = kd1, izero-1
     write(2,'(4i10)') node_number2d(i  ,j  ,nxp), node_number2d(i+kd1,j  ,nxp), &
                       node_number2d(i+kd1,j+kd1,nxp), node_number2d(i  ,j+kd1,nxp)
    end do
   end do

 !----------------------------------------------------------------------------
   nnodes = (nxp+kd1)*(nzp+kd1)
   nquads = (izero-1)*nzp
   write(2,*) 'ZONE T="Boundary 4 (Inlet: Right)"  N=', nnodes,',E=', nquads, &
               ' , ET=quadrilateral, F=FEPOINT'

    j = kd1

   do k = kd1, nz+kd1, inc
    do i = kd1, nx+kd1, inc
     write(2,'(3es20.10)') x(i), y(j), z(k)
    end do
   end do

   do k = kd1, nzp
    do i = kd1, izero-1
     write(2,'(4i10)') node_number2d(i    ,k    ,nxp), node_number2d(i+kd1,k    ,nxp), &
                       node_number2d(i+kd1,k+kd1,nxp), node_number2d(i    ,k+kd1,nxp)
    end do
   end do

 !----------------------------------------------------------------------------
   nnodes = (nxp+kd1)*(nzp+kd1)
   nquads = (izero-1)*nzp
   write(2,*) 'ZONE T="Boundary 5 (Inlet: Left)"  N=', nnodes,',E=', nquads, &
               ' , ET=quadrilateral, F=FEPOINT'

    j = ny+kd1

   do k = kd1, nz+kd1, inc
    do i = kd1, nx+kd1, inc
     write(2,'(3es20.10)') x(i), y(j), z(k)
    end do
   end do

   do k = kd1, nzp
    do i = kd1, izero-1
     write(2,'(4i10)') node_number2d(i    ,k    ,nxp), node_number2d(i+kd1,k    ,nxp), &
                       node_number2d(i+kd1,k+kd1,nxp), node_number2d(i    ,k+kd1,nxp)
    end do
   end do

 endif

 !--------------------------------------------------------------------------------------
 !--------------------------------------------------------------------------------------
 ! End of Upstream (inlet) region
 !--------------------------------------------------------------------------------------
 !--------------------------------------------------------------------------------------


 !--------------------------------------------------------------------------------------
 !--------------------------------------------------------------------------------------
 ! Downstream (outlet) region
 !--------------------------------------------------------------------------------------
 !--------------------------------------------------------------------------------------
 if (ixmax < nxp+1) then

 !----------------------------------------------------------------------------
   nnodes = (nxp+kd1)*(nyp+kd1)
   nquads = (nxp+1-ixmax)*nyp
   write(2,*) 'ZONE T="Boundary 1 (Outlet: Bottom)"  N=', nnodes,',E=', nquads, &
               ' , ET=quadrilateral, F=FEPOINT'

    k = kd1

   do j = kd1, ny+kd1, inc
    do i = kd1, nx+kd1, inc
     write(2,'(3es20.10)') x(i), y(j), z(k)
    end do
   end do

    do j = kd1, nyp
    do i = ixmax, nxp
     write(2,'(4i10)') node_number2d(i    ,j    ,nxp), node_number2d(i+kd1,j    ,nxp), &
                       node_number2d(i+kd1,j+kd1,nxp), node_number2d(i    ,j+kd1,nxp)
    end do
    end do

 !----------------------------------------------------------------------------
   nnodes = (nxp+kd1)*(nyp+kd1)
   nquads = (nxp+1-ixmax)*nyp
   write(2,*) 'ZONE T="Boundary 2 (Outlet: Top)"  N=', nnodes,',E=', nquads, &
               ' , ET=quadrilateral, F=FEPOINT'

    k = nz+kd1

   do j = kd1, ny+kd1, inc
    do i = kd1, nx+kd1, inc
     write(2,'(3es20.10)') x(i), y(j), z(k)
    end do
   end do

   do j = kd1, nyp
    do i = ixmax, nxp
     write(2,'(4i10)') node_number2d(i  ,j  ,nxp), node_number2d(i+kd1,j  ,nxp), &
                       node_number2d(i+kd1,j+kd1,nxp), node_number2d(i  ,j+kd1,nxp)
    end do
   end do

 !----------------------------------------------------------------------------
   nnodes = (nxp+kd1)*(nzp+kd1)
   nquads = (nxp+1-ixmax)*nzp
   write(2,*) 'ZONE T="Boundary 4 (Outlet: Right)"  N=', nnodes,',E=', nquads, &
               ' , ET=quadrilateral, F=FEPOINT'

    j = kd1

   do k = kd1, nz+kd1, inc
    do i = kd1, nx+kd1, inc
     write(2,'(3es20.10)') x(i), y(j), z(k)
    end do
   end do

   do k = kd1, nzp
    do i = ixmax, nxp
     write(2,'(4i10)') node_number2d(i    ,k    ,nxp), node_number2d(i+kd1,k    ,nxp), &
                       node_number2d(i+kd1,k+kd1,nxp), node_number2d(i    ,k+kd1,nxp)
    end do
   end do

 !----------------------------------------------------------------------------
   nnodes = (nxp+kd1)*(nzp+kd1)
   nquads = (nxp+1-ixmax)*nzp
   write(2,*) 'ZONE T="Boundary 5 (Outlet: Left)"  N=', nnodes,',E=', nquads, &
               ' , ET=quadrilateral, F=FEPOINT'

    j = ny+kd1

   do k = kd1, nz+kd1, inc
    do i = kd1, nx+kd1, inc
     write(2,'(3es20.10)') x(i), y(j), z(k)
    end do
   end do

   do k = kd1, nzp
    do i = ixmax, nxp
     write(2,'(4i10)') node_number2d(i    ,k    ,nxp), node_number2d(i+kd1,k    ,nxp), &
                       node_number2d(i+kd1,k+kd1,nxp), node_number2d(i    ,k+kd1,nxp)
    end do
   end do

 endif
 !--------------------------------------------------------------------------------------
 !--------------------------------------------------------------------------------------
 ! End of Downstream (outlet) region
 !--------------------------------------------------------------------------------------
 !--------------------------------------------------------------------------------------

 close(2)

 end subroutine write_tecplot_boundary_file_prs
!********************************************************************************

!*******************************************************************************
!
! PRS: This subroutine writes a Tecplot file for the volume grid.
!
!*******************************************************************************
 subroutine write_tecplot_volume_file_prs(inc,filename)

 implicit none
 integer(kd)  , intent(in) :: inc
 character(80), intent(in) :: filename
 integer(kd)               :: nnodes, i, j, k
 integer(kd)               :: nxp, nyp, nzp, nprs, nprs_check

  nxp = nx / inc
  nyp = ny / inc
  nzp = nz / inc


  nnodes = (nxp+kd1)*(nyp+kd1)*(nzp+kd1)
    nprs = 2_kd * nxp*nyp*nzp

  open(unit=8, file=filename, status="unknown", iostat=os)
  write(8,*) 'TITLE = "GRID"'
  write(8,*) 'VARIABLES = "x","y","z"'

 !Hex zone

   write(8,*) 'zone  n=', nnodes,',e=', nprs,' , et=brick, f=fepoint'

   do k = kd1, nz+kd1, inc
    do j = kd1, ny+kd1, inc
     do i = kd1, nx+kd1, inc
       write(8,'(3es20.10)')  x(i), y(j), z(k)
     end do
    end do
   end do

   nprs_check = 0

   do k = kd1, nzp
    do j = kd1, nyp
     do i = kd1, nxp

      nprs_check = nprs_check + 2

! Reference hex, which is split into 2 prisms:
!
!                       node_number(i    ,j    ,k    ,nxp,nyp), & !1
!                       node_number(i+kd1,j    ,k    ,nxp,nyp), & !2
!                       node_number(i+kd1,j+kd1,k    ,nxp,nyp), & !3
!                       node_number(i    ,j+kd1,k    ,nxp,nyp), & !4
!                       node_number(i    ,j    ,k+kd1,nxp,nyp), & !5
!                       node_number(i+kd1,j    ,k+kd1,nxp,nyp), & !6
!                       node_number(i+kd1,j+kd1,k+kd1,nxp,nyp), & !7
!                       node_number(i    ,j+kd1,k+kd1,nxp,nyp)    !8

      write(8,'(8i10)') node_number(i+kd1,j    ,k    ,nxp,nyp), & !2
                        node_number(i+kd1,j+kd1,k    ,nxp,nyp), & !3
                        node_number(i+kd1,j+kd1,k+kd1,nxp,nyp), & !7
                        node_number(i+kd1,j+kd1,k+kd1,nxp,nyp), & !7 <-duplicate to make hex data.
                        node_number(i    ,j    ,k    ,nxp,nyp), & !1
                        node_number(i    ,j+kd1,k    ,nxp,nyp), & !4
                        node_number(i    ,j+kd1,k+kd1,nxp,nyp), & !8
                        node_number(i    ,j+kd1,k+kd1,nxp,nyp)    !8 <-duplicate to make hex data.

      write(8,'(8i10)') node_number(i+kd1,j    ,k    ,nxp,nyp), & !2
                        node_number(i+kd1,j+kd1,k+kd1,nxp,nyp), & !7
                        node_number(i+kd1,j    ,k+kd1,nxp,nyp), & !6
                        node_number(i+kd1,j    ,k+kd1,nxp,nyp), & !6 <-duplicate to make hex data.
                        node_number(i    ,j    ,k    ,nxp,nyp), & !1
                        node_number(i    ,j+kd1,k+kd1,nxp,nyp), & !8
                        node_number(i    ,j    ,k+kd1,nxp,nyp), & !5
                        node_number(i    ,j    ,k+kd1,nxp,nyp)    !5 <-duplicate to make hex data.

     end do
    end do
   end do

   write(*,*) "  Checking: nprs       = ", nprs
   write(*,*) "  Checking: nprs_check = ", nprs_check

  close(8)

 end subroutine write_tecplot_volume_file_prs
!*******************************************************************************

!*******************************************************************************
!
! PRS: This subroutine writes a ugrid file.
!
!*******************************************************************************
 subroutine write_ugrid_file_prs(inc,filename)

 implicit none
 integer(kd)  , intent(in) :: inc
 character(80), intent(in) :: filename
 integer(kd)               :: nnodes, i, j, k
 integer(kd)               :: nxp, nyp, nzp, nprs, nquads_b, ntrias_b

  nxp = nx / inc
  nyp = ny / inc
  nzp = nz / inc


    nnodes = (nxp+kd1)*(nyp+kd1)*(nzp+kd1)
      nprs = kd2* nxp*nyp*nzp
  nquads_b = kd2*(nxp*nyp + nzp*nxp)
  ntrias_b = kd2*kd2*(nyp*nzp) ! triangles = 2*(nyp*nzp) and two boundaries.

  if ( ugrid_file_unformatted ) then
    open(unit=9, file=filename, form='unformatted',access="stream",&
                                             status='unknown', iostat=os )
!   write(9) nnodes,   ntrias_b,    nquads_b,   ntet,    0, nprs, nhex
    write(9) nnodes,   ntrias_b,    nquads_b,      0,    0, nprs,    0
  else
    open(unit=9, file=filename, status="unknown", iostat=os)
    !#nodes, #tri_faces, #quad_faces, #tetra, #pyr, #prz, #hex
    write(9,'(7i20)') nnodes, ntrias_b, nquads_b,   0,   0,  nprs,  0
  endif

!---------------------------------------------------------------
!(1) Unformatted grid file
!---------------------------------------------------------------

  if ( ugrid_file_unformatted ) then

  !------------------------------------------------------------------------------
  ! Nodes
    do k = kd1, nz+kd1, inc
     do j = kd1, ny+kd1, inc
      do i = kd1, nx+kd1, inc
        write(9)  x(i), y(j), z(k)
      end do
     end do
    end do

  !------------------------------------------------------------------------------
  ! Tria faces = ntria  !Oriented inward

   !(5)Near: iquad = 2*(nx*nz+nx*nz)+kd1, 2*(nx*nz+nx*nz)+ny*nz
     i = kd1
    do k = kd1, nzp
     do j = kd1, nyp

! Reference quad split into two triangles:
!      write(9) node_number(i,j    ,k    ,nxp,nyp), & !1
!               node_number(i,j+kd1,k    ,nxp,nyp), & !2
!               node_number(i,j+kd1,k+kd1,nxp,nyp), & !3
!               node_number(i,j    ,k+kd1,nxp,nyp)    !4

       write(9)                                     &
                node_number(i,j    ,k    ,nxp,nyp), & !1
                node_number(i,j+kd1,k    ,nxp,nyp), & !2
                node_number(i,j+kd1,k+kd1,nxp,nyp)    !3 

       write(9)                                     &
                node_number(i,j    ,k    ,nxp,nyp), & !1
                node_number(i,j+kd1,k+kd1,nxp,nyp), & !3 
                node_number(i,j    ,k+kd1,nxp,nyp)    !4

     end do
    end do

   !(6)Far: iquad = 2*(nx*nz+nx*nz)+ny*nz+kd1, 2*(nx*nz+nx*nz+ny*nz)
     i = nxp+kd1
    do k = kd1, nzp
     do j = kd1, nyp

! Reference quad split into two triangles:
!      write(9) node_number(i,j    ,k    ,nxp,nyp), & !1
!               node_number(i,j    ,k+kd1,nxp,nyp), & !2
!               node_number(i,j+kd1,k+kd1,nxp,nyp), & !3
!               node_number(i,j+kd1,k    ,nxp,nyp)    !4

       write(9)                                     &
                node_number(i,j    ,k    ,nxp,nyp), & !1
                node_number(i,j    ,k+kd1,nxp,nyp), & !2
                node_number(i,j+kd1,k+kd1,nxp,nyp)    !3

       write(9)                                     &
                node_number(i,j    ,k    ,nxp,nyp), & !1
                node_number(i,j+kd1,k+kd1,nxp,nyp), & !3 
                node_number(i,j+kd1,k    ,nxp,nyp)    !4

     end do
    end do

  !------------------------------------------------------------------------------
  ! Quad faces = nquad  !Oriented inward

   !(1)Bottom: iquad = 1, nx*nz
     k = kd1
    do j = kd1, nyp
     do i = izero, ixmax-1
      write(9) node_number(i    ,j    ,k,nxp,nyp), node_number(i+kd1,j    ,k,nxp,nyp), &
               node_number(i+kd1,j+kd1,k,nxp,nyp), node_number(i    ,j+kd1,k,nxp,nyp)
     end do
    end do

   !(2)Top:    iquad = nx*nz+kd1, 2*nx*nz
     k = nzp+kd1
    do j = kd1, nyp
     do i = izero, ixmax-1
      write(9) node_number(i    ,j    ,k,nxp,nyp), node_number(i    ,j+kd1,k,nxp,nyp), &
               node_number(i+kd1,j+kd1,k,nxp,nyp), node_number(i+kd1,j    ,k,nxp,nyp)
     end do
    end do

   !(3)Right (y=ymin): iquad = 2*nx*nz+kd1, 2*nx*nz+nx*nz
     j = kd1
    do k = kd1, nzp
     do i = izero, ixmax-1
      write(9) node_number(i    ,j,k    ,nxp,nyp), node_number(i    ,j,k+kd1,nxp,nyp), &
               node_number(i+kd1,j,k+kd1,nxp,nyp), node_number(i+kd1,j,k    ,nxp,nyp)
     end do
    end do

   !(4)Left (y=ymax): iquad = 2*nx*nz+nx*nz+kd1, 2*(nx*nz+nx*nz)
     j = nyp+kd1
    do k = kd1, nzp
     do i = izero, ixmax-1
      write(9) node_number(i    ,j,k    ,nxp,nyp), node_number(i+kd1,j,k    ,nxp,nyp), &
               node_number(i+kd1,j,k+kd1,nxp,nyp), node_number(i    ,j,k+kd1,nxp,nyp)
     end do
    end do

   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   if (izero > 1) then
   
   !(7)Bottom: iquad = 1, nx*nz
     k = kd1
    do j = kd1, nyp
     do i = kd1, izero-kd1
      write(9) node_number(i    ,j    ,k,nxp,nyp), node_number(i+kd1,j    ,k,nxp,nyp), &
               node_number(i+kd1,j+kd1,k,nxp,nyp), node_number(i    ,j+kd1,k,nxp,nyp)
     end do
    end do

   !(8)Top:    iquad = nx*nz+kd1, 2*nx*nz
     k = nzp+kd1
    do j = kd1, nyp
     do i = kd1, izero-kd1
      write(9) node_number(i    ,j    ,k,nxp,nyp), node_number(i    ,j+kd1,k,nxp,nyp), &
               node_number(i+kd1,j+kd1,k,nxp,nyp), node_number(i+kd1,j    ,k,nxp,nyp)
     end do
    end do

   !(9)Right (y=ymin): iquad = 2*nx*nz+kd1, 2*nx*nz+nx*nz
     j = kd1
    do k = kd1, nzp
     do i = kd1, izero-kd1
      write(9) node_number(i    ,j,k    ,nxp,nyp), node_number(i    ,j,k+kd1,nxp,nyp), &
               node_number(i+kd1,j,k+kd1,nxp,nyp), node_number(i+kd1,j,k    ,nxp,nyp)
     end do
    end do

   !(10)Left (y=ymax): iquad = 2*nx*nz+nx*nz+kd1, 2*(nx*nz+nx*nz)
     j = nyp+kd1
    do k = kd1, nzp
     do i = kd1, izero-kd1
      write(9) node_number(i    ,j,k    ,nxp,nyp), node_number(i+kd1,j,k    ,nxp,nyp), &
               node_number(i+kd1,j,k+kd1,nxp,nyp), node_number(i    ,j,k+kd1,nxp,nyp)
     end do
    end do

   endif
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------

   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   if (ixmax < nxp+1) then
   
   !(7)Bottom: iquad = 1, nx*nz
     k = kd1
    do j = kd1, nyp
     do i = ixmax, nxp
      write(9) node_number(i    ,j    ,k,nxp,nyp), node_number(i+kd1,j    ,k,nxp,nyp), &
               node_number(i+kd1,j+kd1,k,nxp,nyp), node_number(i    ,j+kd1,k,nxp,nyp)
     end do
    end do

   !(8)Top:    iquad = nx*nz+kd1, 2*nx*nz
     k = nzp+kd1
    do j = kd1, nyp
     do i = ixmax, nxp
      write(9) node_number(i    ,j    ,k,nxp,nyp), node_number(i    ,j+kd1,k,nxp,nyp), &
               node_number(i+kd1,j+kd1,k,nxp,nyp), node_number(i+kd1,j    ,k,nxp,nyp)
     end do
    end do

   !(9)Right (y=ymin): iquad = 2*nx*nz+kd1, 2*nx*nz+nx*nz
     j = kd1
    do k = kd1, nzp
     do i = ixmax, nxp
      write(9) node_number(i    ,j,k    ,nxp,nyp), node_number(i    ,j,k+kd1,nxp,nyp), &
               node_number(i+kd1,j,k+kd1,nxp,nyp), node_number(i+kd1,j,k    ,nxp,nyp)
     end do
    end do

   !(10)Left (y=ymax): iquad = 2*nx*nz+nx*nz+kd1, 2*(nx*nz+nx*nz)
     j = nyp+kd1
    do k = kd1, nzp
     do i = ixmax, nxp
      write(9) node_number(i    ,j,k    ,nxp,nyp), node_number(i+kd1,j,k    ,nxp,nyp), &
               node_number(i+kd1,j,k+kd1,nxp,nyp), node_number(i    ,j,k+kd1,nxp,nyp)
     end do
    end do

   endif
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------

  !--------------------------------------------------------------------------------
  ! Face tag

   !(5)Near: iquad = 2*(nx*nz+nx*nz)+kd1, 2*(nx*nz+nx*nz)+ny*nz
    do k = kd1, nzp
     do j = kd1, nyp
      write(9) 5
      write(9) 5
     end do
    end do

   !(6)Far: iquad = 2*(nx*nz+nx*nz)+ny*nz+kd1, 2*(nx*nz+nx*nz+ny*nz)
    do k = kd1, nzp
     do j = kd1, nyp
      write(9) 6
      write(9) 6
     end do
    end do

   !(1)Bottom: iquad = 1, nx*nz
    do j = kd1, nyp
     do i = izero, ixmax-1 !nxp
      write(9) 1
     end do
    end do

   !(2)Top:    iquad = nx*nz+kd1, 2*nx*nz
    do j = kd1, nyp
     do i = izero, ixmax-1 ! nxp
      write(9) 2
     end do
    end do

   !(3)Right: iquad = 2*nx*nz+kd1, 2*nx*nz+nx*nz
    do k = kd1, nzp
     do i = izero, ixmax-1 ! nxp
      write(9) 3
     end do
    end do

   !(4)Left: iquad = 2*nx*nz+nx*nz+kd1, 2*(nx*nz+nx*nz)
    do k = kd1, nzp
     do i = izero, ixmax-1 ! nxp
      write(9) 4
     end do
    end do

   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   if (izero > 1) then

   !(1)Bottom: iquad = 1, nx*nz
    do j = kd1, nyp
     do i = 1, izero-1
      write(9) 7
     end do
    end do

   !(2)Top:    iquad = nx*nz+kd1, 2*nx*nz
    do j = kd1, nyp
     do i = 1, izero-1
      write(9) 8
     end do
    end do

   !(3)Right: iquad = 2*nx*nz+kd1, 2*nx*nz+nx*nz
    do k = kd1, nzp
     do i = 1, izero-1
      write(9) 9
     end do
    end do

   !(4)Left: iquad = 2*nx*nz+nx*nz+kd1, 2*(nx*nz+nx*nz)
    do k = kd1, nzp
     do i = 1, izero-1
      write(9) 10
     end do
    end do

   endif
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------

   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   if (ixmax < nxp+1) then

   !(1)Bottom: iquad = 1, nx*nz
    do j = kd1, nyp
     do i = ixmax, nxp
      write(9) 11
     end do
    end do

   !(2)Top:    iquad = nx*nz+kd1, 2*nx*nz
    do j = kd1, nyp
     do i = ixmax, nxp
      write(9) 12
     end do
    end do

   !(3)Right: iquad = 2*nx*nz+kd1, 2*nx*nz+nx*nz
    do k = kd1, nzp
     do i = ixmax, nxp
      write(9) 13
     end do
    end do

   !(4)Left: iquad = 2*nx*nz+nx*nz+kd1, 2*(nx*nz+nx*nz)
    do k = kd1, nzp
     do i = ixmax, nxp
      write(9) 14
     end do
    end do

   endif
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------

  ! Prs
    do k = kd1, nzp
     do j = kd1, nyp
      do i = kd1, nxp

!      write(9) &
!               node_number(i    ,j    ,k    ,nxp,nyp), & !1
!               node_number(i+kd1,j    ,k    ,nxp,nyp), & !2
!               node_number(i+kd1,j+kd1,k    ,nxp,nyp), & !3
!               node_number(i    ,j+kd1,k    ,nxp,nyp), & !4
!               node_number(i    ,j    ,k+kd1,nxp,nyp), & !5
!               node_number(i+kd1,j    ,k+kd1,nxp,nyp), & !6
!               node_number(i+kd1,j+kd1,k+kd1,nxp,nyp), & !7
!               node_number(i    ,j+kd1,k+kd1,nxp,nyp)    !8

       write(9)                                         &
                node_number(i+kd1,j    ,k    ,nxp,nyp), & !2
                node_number(i+kd1,j+kd1,k+kd1,nxp,nyp), & !7
                node_number(i+kd1,j+kd1,k    ,nxp,nyp), & !3
                node_number(i    ,j    ,k    ,nxp,nyp), & !1
                node_number(i    ,j+kd1,k+kd1,nxp,nyp), & !8
                node_number(i    ,j+kd1,k    ,nxp,nyp)    !4

       write(9)                                         &
                node_number(i+kd1,j    ,k    ,nxp,nyp), & !2
                node_number(i+kd1,j    ,k+kd1,nxp,nyp), & !6
                node_number(i+kd1,j+kd1,k+kd1,nxp,nyp), & !7
                node_number(i    ,j    ,k    ,nxp,nyp), & !1
                node_number(i    ,j    ,k+kd1,nxp,nyp), & !5
                node_number(i    ,j+kd1,k+kd1,nxp,nyp)    !8

      end do
     end do
    end do

!---------------------------------------------------------------
!(2) Formatted grid file
!---------------------------------------------------------------

  else

  !------------------------------------------------------------------------------
  ! Nodes
    do k = kd1, nz+kd1, inc
     do j = kd1, ny+kd1, inc
      do i = kd1, nx+kd1, inc
        write(9,'(3es26.15)')  x(i), y(j), z(k)
      end do
     end do
    end do

  !------------------------------------------------------------------------------
  ! Tria faces = ntria  !Oriented inward

   !(5)Near: iquad = 2*(nx*nz+nx*nz)+kd1, 2*(nx*nz+nx*nz)+ny*nz
     i = kd1
    do k = kd1, nzp
     do j = kd1, nyp

! Reference quad split into two triangles:
!      write(9,'(4i20)') node_number(i,j    ,k    ,nxp,nyp), & !1
!                        node_number(i,j+kd1,k    ,nxp,nyp), & !2
!                        node_number(i,j+kd1,k+kd1,nxp,nyp), & !3
!                        node_number(i,j    ,k+kd1,nxp,nyp)    !4

       write(9,'(3i20)')                                     &  
                         node_number(i,j    ,k    ,nxp,nyp), & !1
                         node_number(i,j+kd1,k    ,nxp,nyp), & !2
                         node_number(i,j+kd1,k+kd1,nxp,nyp)    !3 

       write(9,'(3i20)')                                     &  
                         node_number(i,j    ,k    ,nxp,nyp), & !1
                         node_number(i,j+kd1,k+kd1,nxp,nyp), & !3 
                         node_number(i,j    ,k+kd1,nxp,nyp)    !4

     end do
    end do

   !(6)Far: iquad = 2*(nx*nz+nx*nz)+ny*nz+kd1, 2*(nx*nz+nx*nz+ny*nz)
     i = nxp+kd1
    do k = kd1, nzp
     do j = kd1, nyp

! Reference quad split into two triangles:
!      write(9,'(4i20)') node_number(i,j    ,k    ,nxp,nyp), & !1
!                        node_number(i,j    ,k+kd1,nxp,nyp), & !2
!                        node_number(i,j+kd1,k+kd1,nxp,nyp), & !3
!                        node_number(i,j+kd1,k    ,nxp,nyp)    !4

       write(9,'(3i20)')                                     &  
                         node_number(i,j    ,k    ,nxp,nyp), & !1
                         node_number(i,j    ,k+kd1,nxp,nyp), & !2
                         node_number(i,j+kd1,k+kd1,nxp,nyp)    !3

       write(9,'(3i20)')                                     &  
                         node_number(i,j    ,k    ,nxp,nyp), & !1
                         node_number(i,j+kd1,k+kd1,nxp,nyp), & !3 
                         node_number(i,j+kd1,k    ,nxp,nyp)    !4

     end do
    end do

  !------------------------------------------------------------------------------
  ! Quad faces = nquad  !Oriented inward

   !(1)Bottom: iquad = 1, nx*nz
     k = kd1
    do j = kd1, nyp
     do i = izero, ixmax-1
      write(9,'(4i20)') node_number(i    ,j    ,k,nxp,nyp), node_number(i+kd1,j    ,k,nxp,nyp), &
                        node_number(i+kd1,j+kd1,k,nxp,nyp), node_number(i    ,j+kd1,k,nxp,nyp)
     end do
    end do

   !(2)Top:    iquad = nx*nz+kd1, 2*nx*nz
     k = nzp+kd1
    do j = kd1, nyp
     do i = izero, ixmax-1
      write(9,'(4i20)') node_number(i    ,j    ,k,nxp,nyp), node_number(i    ,j+kd1,k,nxp,nyp), &
                        node_number(i+kd1,j+kd1,k,nxp,nyp), node_number(i+kd1,j    ,k,nxp,nyp)
     end do
    end do

   !(3)Right: iquad = 2*nx*nz+kd1, 2*nx*nz+nx*nz
     j = kd1
    do k = kd1, nzp
     do i = izero, ixmax-1
      write(9,'(4i20)') node_number(i    ,j,k    ,nxp,nyp), node_number(i    ,j,k+kd1,nxp,nyp), &
                        node_number(i+kd1,j,k+kd1,nxp,nyp), node_number(i+kd1,j,k    ,nxp,nyp)
     end do
    end do

   !(4)Left: iquad = 2*nx*nz+nx*nz+kd1, 2*(nx*nz+nx*nz)
     j = nyp+kd1
    do k = kd1, nzp
     do i = izero, ixmax-1
      write(9,'(4i20)') node_number(i    ,j,k    ,nxp,nyp), node_number(i+kd1,j,k    ,nxp,nyp), &
                        node_number(i+kd1,j,k+kd1,nxp,nyp), node_number(i    ,j,k+kd1,nxp,nyp)
     end do
    end do

   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   if (izero > 1) then
   
   !(7)Bottom: iquad = 1, nx*nz
     k = kd1
    do j = kd1, nyp
     do i = kd1, izero-kd1
      write(9,'(4i20)') node_number(i    ,j    ,k,nxp,nyp), node_number(i+kd1,j    ,k,nxp,nyp), &
                        node_number(i+kd1,j+kd1,k,nxp,nyp), node_number(i    ,j+kd1,k,nxp,nyp)
     end do
    end do

   !(8)Top:    iquad = nx*nz+kd1, 2*nx*nz
     k = nzp+kd1
    do j = kd1, nyp
     do i = kd1, izero-kd1
      write(9,'(4i20)') node_number(i    ,j    ,k,nxp,nyp), node_number(i    ,j+kd1,k,nxp,nyp), &
                        node_number(i+kd1,j+kd1,k,nxp,nyp), node_number(i+kd1,j    ,k,nxp,nyp)
     end do
    end do

   !(9)Right (y=ymin): iquad = 2*nx*nz+kd1, 2*nx*nz+nx*nz
     j = kd1
    do k = kd1, nzp
     do i = kd1, izero-kd1
      write(9,'(4i20)') node_number(i    ,j,k    ,nxp,nyp), node_number(i    ,j,k+kd1,nxp,nyp), &
                        node_number(i+kd1,j,k+kd1,nxp,nyp), node_number(i+kd1,j,k    ,nxp,nyp)
     end do
    end do

   !(10)Left (y=ymax): iquad = 2*nx*nz+nx*nz+kd1, 2*(nx*nz+nx*nz)
     j = nyp+kd1
    do k = kd1, nzp
     do i = kd1, izero-kd1
      write(9,'(4i20)') node_number(i    ,j,k    ,nxp,nyp), node_number(i+kd1,j,k    ,nxp,nyp), &
                        node_number(i+kd1,j,k+kd1,nxp,nyp), node_number(i    ,j,k+kd1,nxp,nyp)
     end do
    end do

   endif
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------

   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   if (ixmax < nxp+1) then
   
   !(7)Bottom: iquad = 1, nx*nz
     k = kd1
    do j = kd1, nyp
     do i = ixmax, nxp
      write(9,'(4i20)') node_number(i    ,j    ,k,nxp,nyp), node_number(i+kd1,j    ,k,nxp,nyp), &
                        node_number(i+kd1,j+kd1,k,nxp,nyp), node_number(i    ,j+kd1,k,nxp,nyp)
     end do
    end do

   !(8)Top:    iquad = nx*nz+kd1, 2*nx*nz
     k = nzp+kd1
    do j = kd1, nyp
     do i = ixmax, nxp
      write(9,'(4i20)') node_number(i    ,j    ,k,nxp,nyp), node_number(i    ,j+kd1,k,nxp,nyp), &
                        node_number(i+kd1,j+kd1,k,nxp,nyp), node_number(i+kd1,j    ,k,nxp,nyp)
     end do
    end do

   !(9)Right (y=ymin): iquad = 2*nx*nz+kd1, 2*nx*nz+nx*nz
     j = kd1
    do k = kd1, nzp
     do i = ixmax, nxp
      write(9,'(4i20)') node_number(i    ,j,k    ,nxp,nyp), node_number(i    ,j,k+kd1,nxp,nyp), &
                        node_number(i+kd1,j,k+kd1,nxp,nyp), node_number(i+kd1,j,k    ,nxp,nyp)
     end do
    end do

   !(10)Left (y=ymax): iquad = 2*nx*nz+nx*nz+kd1, 2*(nx*nz+nx*nz)
     j = nyp+kd1
    do k = kd1, nzp
     do i = ixmax, nxp
      write(9,'(4i20)') node_number(i    ,j,k    ,nxp,nyp), node_number(i+kd1,j,k    ,nxp,nyp), &
                        node_number(i+kd1,j,k+kd1,nxp,nyp), node_number(i    ,j,k+kd1,nxp,nyp)
     end do
    end do

   endif
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------

  !--------------------------------------------------------------------------------
  ! Face tag

   !(5)Near: iquad = 2*(nx*nz+nx*nz)+kd1, 2*(nx*nz+nx*nz)+ny*nz
    do k = kd1, nzp
     do j = kd1, nyp
      write(9,'(i10)') 5
      write(9,'(i10)') 5
     end do
    end do

   !(6)Far: iquad = 2*(nx*nz+nx*nz)+ny*nz+kd1, 2*(nx*nz+nx*nz+ny*nz)
    do k = kd1, nzp
     do j = kd1, nyp
      write(9,'(i10)') 6
      write(9,'(i10)') 6
     end do
    end do

   !(1)Bottom: iquad = 1, nx*nz
    do j = kd1, nyp
     do i = izero, ixmax-1
      write(9,'(i10)') 1
     end do
    end do

   !(2)Top:    iquad = nx*nz+kd1, 2*nx*nz
    do j = kd1, nyp
     do i = izero, ixmax-1
      write(9,'(i10)') 2
     end do
    end do

   !(3)Right: iquad = 2*nx*nz+kd1, 2*nx*nz+nx*nz
    do k = kd1, nzp
     do i = izero, ixmax-1
      write(9,'(i10)') 3
     end do
    end do

   !(4)Left: iquad = 2*nx*nz+nx*nz+kd1, 2*(nx*nz+nx*nz)
    do k = kd1, nzp
     do i = izero, ixmax-1
      write(9,'(i10)') 4
     end do
    end do
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   if (izero > 1) then

   !(1)Bottom: iquad = 1, nx*nz
    do j = kd1, nyp
     do i = 1, izero-1
      write(9,'(i10)') 7
     end do
    end do

   !(2)Top:    iquad = nx*nz+kd1, 2*nx*nz
    do j = kd1, nyp
     do i = 1, izero-1
      write(9,'(i10)') 8
     end do
    end do

   !(3)Right: iquad = 2*nx*nz+kd1, 2*nx*nz+nx*nz
    do k = kd1, nzp
     do i = 1, izero-1
      write(9,'(i10)') 9
     end do
    end do

   !(4)Left: iquad = 2*nx*nz+nx*nz+kd1, 2*(nx*nz+nx*nz)
    do k = kd1, nzp
     do i = 1, izero-1
      write(9,'(i10)') 10
     end do
    end do

   endif
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------

   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   if (ixmax < nxp+1) then

   !(1)Bottom: iquad = 1, nx*nz
    do j = kd1, nyp
     do i = ixmax, nxp
      write(9,'(i10)') 11
     end do
    end do

   !(2)Top:    iquad = nx*nz+kd1, 2*nx*nz
    do j = kd1, nyp
     do i = ixmax, nxp
      write(9,'(i10)') 12
     end do
    end do

   !(3)Right: iquad = 2*nx*nz+kd1, 2*nx*nz+nx*nz
    do k = kd1, nzp
     do i = ixmax, nxp
      write(9,'(i10)') 13
     end do
    end do

   !(4)Left: iquad = 2*nx*nz+nx*nz+kd1, 2*(nx*nz+nx*nz)
    do k = kd1, nzp
     do i = ixmax, nxp
      write(9,'(i10)') 14
     end do
    end do

   endif
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------

  ! Prs
    do k = kd1, nzp
     do j = kd1, nyp
      do i = kd1, nxp

!      write(9,'(8i20)') &
!               node_number(i    ,j    ,k    ,nxp,nyp), & !1
!               node_number(i+kd1,j    ,k    ,nxp,nyp), & !2
!               node_number(i+kd1,j+kd1,k    ,nxp,nyp), & !3
!               node_number(i    ,j+kd1,k    ,nxp,nyp), & !4
!               node_number(i    ,j    ,k+kd1,nxp,nyp), & !5
!               node_number(i+kd1,j    ,k+kd1,nxp,nyp), & !6
!               node_number(i+kd1,j+kd1,k+kd1,nxp,nyp), & !7
!               node_number(i    ,j+kd1,k+kd1,nxp,nyp)    !8

       write(9,'(6i20)')                                &
                node_number(i+kd1,j    ,k    ,nxp,nyp), & !2
                node_number(i+kd1,j+kd1,k+kd1,nxp,nyp), & !7
                node_number(i+kd1,j+kd1,k    ,nxp,nyp), & !3
                node_number(i    ,j    ,k    ,nxp,nyp), & !1
                node_number(i    ,j+kd1,k+kd1,nxp,nyp), & !8
                node_number(i    ,j+kd1,k    ,nxp,nyp)    !4

       write(9,'(6i20)')                                &
                node_number(i+kd1,j    ,k    ,nxp,nyp), & !2
                node_number(i+kd1,j    ,k+kd1,nxp,nyp), & !6
                node_number(i+kd1,j+kd1,k+kd1,nxp,nyp), & !7
                node_number(i    ,j    ,k    ,nxp,nyp), & !1
                node_number(i    ,j    ,k+kd1,nxp,nyp), & !5
                node_number(i    ,j+kd1,k+kd1,nxp,nyp)    !8

      end do
     end do
    end do

  endif

!---------------------------------------------------------------
! End of Unformatted or Formatted
!---------------------------------------------------------------

  close(9)

 end subroutine write_ugrid_file_prs
!********************************************************************************

!*******************************************************************************
! This subroutine writes a su2 grid file.
!
! Note: Nodes -> i = 0,1,2,...; Elements -> i = 0,1,2,...
!
!*******************************************************************************
 subroutine write_su2grid_file_prs(inc,filename)

 implicit none
 integer(kd)  , intent(in) :: inc
 character(80), intent(in) :: filename

 integer(kd) :: nnodes, i, j, k, counter
 integer(kd) :: nxp, nyp, nzp, nprs

  nxp    = nx / inc
  nyp    = ny / inc
  nzp    = nz / inc
  nnodes = (nxp+kd1)*(nyp+kd1)*(nzp+kd1)
  nprs   = kd2*nxp*nyp*nzp

  write(*,*) "    write_su2grid_file: nnodes = ", nnodes
  write(*,*) "    write_su2grid_file:   nprs = ", nprs

  open(unit=7, file=filename, status="unknown", iostat=os)

  write(7,*) "%"
  write(7,*) "% Problem dimension"
  write(7,*) "%"
  write(7,5) 3
5 format('NDIME= ',i12)

   write(7,*) "%"
   write(7,*) "% Inner element connectivity"
   write(7,10) nprs
10 format('NELEM= ',i12)

   counter = 0

 !-------------------------------------------------------------------------
 ! Elements

  ! No tetra
  ! No hex
  ! Only prisms:
    do k = kd1, nzp
     do j = kd1, nyp
      do i = kd1, nxp

! Reference hex to be split into two prisms:
!      write(7,'(10i10)') 12                                       , &
!                        node_number(i    ,j    ,k    ,nxp,nyp)-kd1, & !1
!                        node_number(i+kd1,j    ,k    ,nxp,nyp)-kd1, & !2
!                        node_number(i+kd1,j+kd1,k    ,nxp,nyp)-kd1, & !3
!                        node_number(i    ,j+kd1,k    ,nxp,nyp)-kd1, & !4
!                        node_number(i    ,j    ,k+kd1,nxp,nyp)-kd1, & !5
!                        node_number(i+kd1,j    ,k+kd1,nxp,nyp)-kd1, & !6
!                        node_number(i+kd1,j+kd1,k+kd1,nxp,nyp)-kd1, & !7
!                        node_number(i    ,j+kd1,k+kd1,nxp,nyp)-kd1, & !8
!                        counter

      write(7,'(8i10)') 13                                        , &
                        node_number(i+kd1,j    ,k    ,nxp,nyp)-kd1, & !2
                        node_number(i+kd1,j+kd1,k    ,nxp,nyp)-kd1, & !3
                        node_number(i+kd1,j+kd1,k+kd1,nxp,nyp)-kd1, & !7
                        node_number(i    ,j    ,k    ,nxp,nyp)-kd1, & !1
                        node_number(i    ,j+kd1,k    ,nxp,nyp)-kd1, & !4
                        node_number(i    ,j+kd1,k+kd1,nxp,nyp)-kd1, & !8
                        counter

      counter = counter + kd1

      write(7,'(8i10)') 13                                        , &
                        node_number(i+kd1,j    ,k    ,nxp,nyp)-kd1, & !2
                        node_number(i+kd1,j+kd1,k+kd1,nxp,nyp)-kd1, & !7
                        node_number(i+kd1,j    ,k+kd1,nxp,nyp)-kd1, & !6
                        node_number(i    ,j    ,k    ,nxp,nyp)-kd1, & !1
                        node_number(i    ,j+kd1,k+kd1,nxp,nyp)-kd1, & !8
                        node_number(i    ,j    ,k+kd1,nxp,nyp)-kd1, & !5
                        counter

      counter = counter + kd1

      end do
     end do
    end do

   write(*,*) "  --- elm check", nprs, counter

 !--------------------------------------------------------------------------
 ! Nodes

   write(7,*) "%"
   write(7,*) "% Node coordinates"
   write(7,*) "%"
   write(7,20) nnodes
20 format('NPOIN= ', i12)

   counter = 0

  ! Nodes
    do k = kd1, nz+kd1, inc
     do j = kd1, ny+kd1, inc
      do i = kd1, nx+kd1, inc
        write(7,'(3es26.15,i20)')  x(i), y(j), z(k), counter
        counter = counter + kd1
      end do
     end do
    end do

   write(*,*) "  --- node check", nnodes, counter

 !--------------------------------------------------------------------------
 ! Boundary

    write(7,*) "%"
    write(7,*) "% Boundary elements"
    write(7,*) "%"

  !+Inlet and +Outlet
   if     (izero >  1 .and. ixmax <  nxp+1) then

    write(7,30) 14

  !+Inlet
   elseif (izero >  1 .and. ixmax >= nxp+1) then

    write(7,30) 10

  !Outlet
   elseif (izero <= 1 .and. ixmax <  nxp+1) then

    write(7,30) 10

  !No inlet and outlet
   else

    write(7,30) 6

   endif

30 format('NMARK= ',i12)

40 format('MARKER_TAG= ',a)
50 format('MARKER_ELEMS= ', i12)

 !--------------------------------------------------------------------------
 ! (1) Viscous bottom surface (z=zmin)
 !--------------------------------------------------------------------------

   write(*,40) "DUCT_VISCOUS_ZMIN"
   write(7,40) "DUCT_VISCOUS_ZMIN"
   write(7,50) (nxp-(izero-1) - (nxp+1-ixmax))*nyp

   k = kd1
    do j = kd1, nyp
     do i = izero, ixmax-1
      write(7,'(5i20)') 9, node_number(i    ,j    ,k,nxp,nyp)-kd1, node_number(i+kd1,j    ,k,nxp,nyp)-kd1, &
                           node_number(i+kd1,j+kd1,k,nxp,nyp)-kd1, node_number(i    ,j+kd1,k,nxp,nyp)-kd1
     end do
    end do

 !--------------------------------------------------------------------------
 ! (2) Top symmetry z (z=zmax)
 !--------------------------------------------------------------------------

   write(*,40) "DUCT_SYMMETRY_Z"
   write(7,40) "DUCT_SYMMETRY_Z"
   write(7,50) (nxp-(izero-1) - (nxp+1-ixmax))*nyp

     k = nzp+kd1
    do j = kd1, nyp
     do i = izero, ixmax-1
      write(7,'(5i20)') 9, node_number(i    ,j    ,k,nxp,nyp)-kd1, node_number(i    ,j+kd1,k,nxp,nyp)-kd1, &
                           node_number(i+kd1,j+kd1,k,nxp,nyp)-kd1, node_number(i+kd1,j    ,k,nxp,nyp)-kd1
     end do
    end do
 !--------------------------------------------------------------------------


 !--------------------------------------------------------------------------
 ! (3) Viscous side (y=ymin)
 !--------------------------------------------------------------------------

   write(*,40) "DUCT_VISCOUS_YMIN"
   write(7,40) "DUCT_VISCOUS_YMIN"
   write(7,50) (nxp-(izero-1) - (nxp+1-ixmax))*nzp

     j = kd1
    do k = kd1, nzp
     do i = izero, ixmax-1
      write(7,'(5i20)') 9, node_number(i    ,j,k    ,nxp,nyp)-kd1, node_number(i    ,j,k+kd1,nxp,nyp)-kd1, &
                           node_number(i+kd1,j,k+kd1,nxp,nyp)-kd1, node_number(i+kd1,j,k    ,nxp,nyp)-kd1
     end do
    end do

 !--------------------------------------------------------------------------
 ! (4) Symmetry y (y=ymax)
 !--------------------------------------------------------------------------

   write(*,40) "DUCT_SYMMETRY_Y"
   write(7,40) "DUCT_SYMMETRY_Y"
   write(7,50) (nxp-(izero-1) - (nxp+1-ixmax))*nzp

     j = nyp+kd1
    do k = kd1, nzp
     do i = izero, ixmax-1
      write(7,'(5i20)') 9, node_number(i    ,j,k    ,nxp,nyp)-kd1, node_number(i+kd1,j,k    ,nxp,nyp)-kd1, &
                           node_number(i+kd1,j,k+kd1,nxp,nyp)-kd1, node_number(i    ,j,k+kd1,nxp,nyp)-kd1
     end do
    end do
 !--------------------------------------------------------------------------


 !--------------------------------------------------------------------------
 ! (5) Inflow (x=xmin)
 !--------------------------------------------------------------------------

   write(*,40) "INFLOW"
   write(7,40) "INFLOW"
   write(7,50) kd2*nyp*nzp

     i = kd1
    do k = kd1, nzp
     do j = kd1, nyp

! Reference quad split into two triangles:
!     write(7,'(5i20)') 9, node_number(i,j    ,k    ,nxp,nyp)-kd1, & !1
!                          node_number(i,j+kd1,k    ,nxp,nyp)-kd1, & !2
!                          node_number(i,j+kd1,k+kd1,nxp,nyp)-kd1, & !3
!                          node_number(i,j    ,k+kd1,nxp,nyp)-kd1    !4

      write(7,'(4i20)') 5, node_number(i,j    ,k    ,nxp,nyp)-kd1, & !1
                           node_number(i,j+kd1,k    ,nxp,nyp)-kd1, & !2
                           node_number(i,j+kd1,k+kd1,nxp,nyp)-kd1    !3

      write(7,'(4i20)') 5, node_number(i,j    ,k    ,nxp,nyp)-kd1, & !1
                           node_number(i,j+kd1,k+kd1,nxp,nyp)-kd1, & !3
                           node_number(i,j    ,k+kd1,nxp,nyp)-kd1    !4

     end do
    end do

 !--------------------------------------------------------------------------
 ! (6) Outflow (x=xmax)
 !--------------------------------------------------------------------------

   write(*,40) "OUTFLOW"
   write(7,40) "OUTFLOW"
   write(7,50) kd2*nyp*nzp

     i = nxp+kd1
    do k = kd1, nzp
     do j = kd1, nyp

! Reference quad split into two triangles:
!     write(7,'(5i20)') 9, node_number(i,j    ,k    ,nxp,nyp)-kd1, & !1
!                          node_number(i,j    ,k+kd1,nxp,nyp)-kd1, & !2
!                          node_number(i,j+kd1,k+kd1,nxp,nyp)-kd1, & !3
!                          node_number(i,j+kd1,k    ,nxp,nyp)-kd1    !4

      write(7,'(4i20)') 5, node_number(i,j    ,k    ,nxp,nyp)-kd1, & !1
                           node_number(i,j    ,k+kd1,nxp,nyp)-kd1, & !2
                           node_number(i,j+kd1,k+kd1,nxp,nyp)-kd1    !3

      write(7,'(4i20)') 5, node_number(i,j    ,k    ,nxp,nyp)-kd1, & !1
                           node_number(i,j+kd1,k+kd1,nxp,nyp)-kd1, & !3
                           node_number(i,j+kd1,k    ,nxp,nyp)-kd1    !4

     end do
    end do

 !--------------------------------------------------------------------------


 if (izero > 1) then

 !--------------------------------------------------------------------------
 ! (7) Symmetry z (z=zmin)
 !--------------------------------------------------------------------------

   write(*,40) "INLET_SYMMETRY_Z"
   write(7,40) "INLET_SYMMETRY_Z"
   write(7,50) (izero-1)*nyp

   k = kd1
    do j = kd1, nyp
     do i = 1, izero-1
      write(7,'(5i20)') 9, node_number(i    ,j    ,k,nxp,nyp)-kd1, node_number(i+kd1,j    ,k,nxp,nyp)-kd1, &
                           node_number(i+kd1,j+kd1,k,nxp,nyp)-kd1, node_number(i    ,j+kd1,k,nxp,nyp)-kd1
     end do
    end do

 !--------------------------------------------------------------------------
 ! (8) Symmetry z (z=zmax)
 !--------------------------------------------------------------------------

   write(*,40) "INLET_SYMMETRY_Z"
   write(7,40) "INLET_SYMMETRY_Z"
   write(7,50) (izero-1)*nyp

     k = nzp+kd1
    do j = kd1, nyp
     do i = 1, izero-1
      write(7,'(5i20)') 9, node_number(i    ,j    ,k,nxp,nyp)-kd1, node_number(i    ,j+kd1,k,nxp,nyp)-kd1, &
                           node_number(i+kd1,j+kd1,k,nxp,nyp)-kd1, node_number(i+kd1,j    ,k,nxp,nyp)-kd1
     end do
    end do
 !--------------------------------------------------------------------------


 !--------------------------------------------------------------------------
 ! (9) Symmetry y (y=ymin)
 !--------------------------------------------------------------------------

   write(*,40) "INLET_SYMMETRY_Y"
   write(7,40) "INLET_SYMMETRY_Y"
   write(7,50) (izero-1)*nzp

     j = kd1
    do k = kd1, nzp
     do i = 1, izero-1
      write(7,'(5i20)') 9, node_number(i    ,j,k    ,nxp,nyp)-kd1, node_number(i    ,j,k+kd1,nxp,nyp)-kd1, &
                           node_number(i+kd1,j,k+kd1,nxp,nyp)-kd1, node_number(i+kd1,j,k    ,nxp,nyp)-kd1
     end do
    end do

 !--------------------------------------------------------------------------
 ! (10) Symmetry y (y=ymax)
 !-------------------------------------------------------------------------

   write(*,40) "INLET_SYMMETRY_Y"
   write(7,40) "INLET_SYMMETRY_Y"
   write(7,50) (izero-1)*nzp

     j = nyp+kd1
    do k = kd1, nzp
     do i = 1, izero-1
      write(7,'(5i20)') 9, node_number(i    ,j,k    ,nxp,nyp)-kd1, node_number(i+kd1,j,k    ,nxp,nyp)-kd1, &
                           node_number(i+kd1,j,k+kd1,nxp,nyp)-kd1, node_number(i    ,j,k+kd1,nxp,nyp)-kd1
     end do
    end do
 !--------------------------------------------------------------------------

 endif

 if (ixmax < nxp+1) then

 !--------------------------------------------------------------------------
 ! (7) Symmetry z (z=zmin)
 !--------------------------------------------------------------------------

   write(*,40) "OUTLET_SYMMETRY_Z"
   write(7,40) "OUTLET_SYMMETRY_Z"
   write(7,50) (nxp+1-ixmax)*nyp

   k = kd1
    do j = kd1, nyp
     do i = ixmax, nxp
      write(7,'(5i20)') 9, node_number(i    ,j    ,k,nxp,nyp)-kd1, node_number(i+kd1,j    ,k,nxp,nyp)-kd1, &
                           node_number(i+kd1,j+kd1,k,nxp,nyp)-kd1, node_number(i    ,j+kd1,k,nxp,nyp)-kd1
     end do
    end do

 !--------------------------------------------------------------------------
 ! (8) Symmetry z (z=zmax)
 !--------------------------------------------------------------------------

   write(*,40) "OUTLET_SYMMETRY_Z"
   write(7,40) "OUTLET_SYMMETRY_Z"
   write(7,50) (nxp+1-ixmax)*nyp

     k = nzp+kd1
    do j = kd1, nyp
     do i = ixmax, nxp
      write(7,'(5i20)') 9, node_number(i    ,j    ,k,nxp,nyp)-kd1, node_number(i    ,j+kd1,k,nxp,nyp)-kd1, &
                           node_number(i+kd1,j+kd1,k,nxp,nyp)-kd1, node_number(i+kd1,j    ,k,nxp,nyp)-kd1
     end do
    end do
 !--------------------------------------------------------------------------


 !--------------------------------------------------------------------------
 ! (9) Symmetry y (y=ymin)
 !--------------------------------------------------------------------------

   write(*,40) "OUTLET_SYMMETRY_Y"
   write(7,40) "OUTLET_SYMMETRY_Y"
   write(7,50) (nxp+1-ixmax)*nzp

     j = kd1
    do k = kd1, nzp
     do i = ixmax, nxp
      write(7,'(5i20)') 9, node_number(i    ,j,k    ,nxp,nyp)-kd1, node_number(i    ,j,k+kd1,nxp,nyp)-kd1, &
                           node_number(i+kd1,j,k+kd1,nxp,nyp)-kd1, node_number(i+kd1,j,k    ,nxp,nyp)-kd1
     end do
    end do

 !--------------------------------------------------------------------------
 ! (10) Symmetry y (y=ymax)
 !-------------------------------------------------------------------------

   write(*,40) "OUTLET_SYMMETRY_Y"
   write(7,40) "OUTLET_SYMMETRY_Y"
   write(7,50) (nxp+1-ixmax)*nzp

     j = nyp+kd1
    do k = kd1, nzp
     do i = ixmax, nxp
      write(7,'(5i20)') 9, node_number(i    ,j,k    ,nxp,nyp)-kd1, node_number(i+kd1,j,k    ,nxp,nyp)-kd1, &
                           node_number(i+kd1,j,k+kd1,nxp,nyp)-kd1, node_number(i    ,j,k+kd1,nxp,nyp)-kd1
     end do
    end do
 !--------------------------------------------------------------------------

 endif

 !--------------------------------------------------------------------------
 !--------------------------------------------------------------------------

  close(7)

 end subroutine write_su2grid_file_prs
!********************************************************************************

















!*******************************************************************************
!
! TET: This subroutine writes a Tecplot file for boundaries.
!
!******************************************************************************
 subroutine write_tecplot_boundary_file_tet(inc,filename)

 implicit none
 integer(kd)  , intent(in) :: inc
 character(80), intent(in) :: filename
 integer(kd)               :: nnodes, i, j, k
 integer(kd)               :: nxp, nyp, nzp

 integer(kd)               :: ntrias

 open(unit=2, file=filename, status="unknown", iostat=os)
 write(2,'(2a)') 'variables = "x","y","z"'

  nxp = nx / inc
  nyp = ny / inc
  nzp = nz / inc

 !----------------------------------------------------------------------------
 ! Constant z planes
 !----------------------------------------------------------------------------
  nnodes = (nxp+kd1)*(nyp+kd1)
  ntrias = kd2*(nxp - (izero-1) - (nxp+1-ixmax) )*nyp
  write(2,*) 'ZONE T="Boundary 1 (Bottom)"  N=', nnodes,',E=', ntrias, &
              ' , ET=quadrilateral, F=FEPOINT'

   k = kd1

  do j = kd1, ny+kd1, inc
   do i = kd1, nx+kd1, inc
    write(2,'(3es20.10)') x(i), y(j), z(k)
   end do
  end do

  do j = kd1, nyp
   do i = izero, ixmax-1 !nxp

!   write(2,'(4i10)')                                 &
!                     node_number2d(i    ,j    ,nxp), & !1
!                     node_number2d(i+kd1,j    ,nxp), & !2
!                     node_number2d(i+kd1,j+kd1,nxp), & !3
!                     node_number2d(i    ,j+kd1,nxp)    !4

    write(2,'(4i10)')                                 &
                      node_number2d(i    ,j    ,nxp), & !1
                      node_number2d(i+kd1,j    ,nxp), & !2
                      node_number2d(i    ,j+kd1,nxp), & !4
                      node_number2d(i    ,j+kd1,nxp)    !4

    write(2,'(4i10)')                                 &
                      node_number2d(i+kd1,j    ,nxp), & !2
                      node_number2d(i+kd1,j+kd1,nxp), & !3
                      node_number2d(i    ,j+kd1,nxp), & !4
                      node_number2d(i    ,j+kd1,nxp)    !4

   end do
  end do

 !----------------------------------------------------------------------------
  nnodes = (nxp+kd1)*(nyp+kd1)
  ntrias = kd2*(nxp - (izero-1) - (nxp+1-ixmax))*nyp
  write(2,*) 'ZONE T="Boundary 2 (Top)"  N=', nnodes,',E=', ntrias, &
              ' , ET=quadrilateral, F=FEPOINT'

   k = nz+kd1

  do j = kd1, ny+kd1, inc
   do i = kd1, nx+kd1, inc
    write(2,'(3es20.10)') x(i), y(j), z(k)
   end do
  end do

  do j = kd1, nyp
   do i = izero, ixmax-1 !, nxp

!    write(2,'(4i10)')                                 &
!                      node_number2d(i    ,j    ,nxp), & !1
!                      node_number2d(i+kd1,j    ,nxp), & !2
!                      node_number2d(i+kd1,j+kd1,nxp), & !3
!                      node_number2d(i    ,j+kd1,nxp)    !4

    write(2,'(4i10)')                                 &
                      node_number2d(i    ,j    ,nxp), & !1
                      node_number2d(i+kd1,j    ,nxp), & !2
                      node_number2d(i    ,j+kd1,nxp), & !4
                      node_number2d(i    ,j+kd1,nxp)    !4

    write(2,'(4i10)')                                 &
                      node_number2d(i+kd1,j    ,nxp), & !2
                      node_number2d(i+kd1,j+kd1,nxp), & !3
                      node_number2d(i    ,j+kd1,nxp), & !4
                      node_number2d(i    ,j+kd1,nxp)    !4

   end do
  end do


 !----------------------------------------------------------------------------
 ! Constant y planes
 !----------------------------------------------------------------------------
  nnodes = (nxp+kd1)*(nzp+kd1)
  ntrias = kd2*(nxp - (izero-1) - (nxp+1-ixmax))*nzp
  write(2,*) 'ZONE T="Boundary 4 (Right)"  N=', nnodes,',E=', ntrias, &
              ' , ET=quadrilateral, F=FEPOINT'

   j = kd1

  do k = kd1, nz+kd1, inc
   do i = kd1, nx+kd1, inc
    write(2,'(3es20.10)') x(i), y(j), z(k)
   end do
  end do

  do k = kd1, nzp
   do i = izero, ixmax-1 !, nxp

!    write(2,'(4i10)')                                 &
!                      node_number2d(i    ,k    ,nxp), & !1
!                      node_number2d(i+kd1,k    ,nxp), & !2
!                      node_number2d(i+kd1,k+kd1,nxp), & !3
!                      node_number2d(i    ,k+kd1,nxp)    !4

    write(2,'(4i10)')                                 &
                      node_number2d(i    ,k    ,nxp), & !1
                      node_number2d(i+kd1,k    ,nxp), & !2
                      node_number2d(i    ,k+kd1,nxp), & !4
                      node_number2d(i    ,k+kd1,nxp)    !4

    write(2,'(4i10)')                                 &
                      node_number2d(i+kd1,k    ,nxp), & !2
                      node_number2d(i+kd1,k+kd1,nxp), & !3
                      node_number2d(i    ,k+kd1,nxp), & !4
                      node_number2d(i    ,k+kd1,nxp)    !4

   end do
  end do

 !----------------------------------------------------------------------------
  nnodes = (nxp+kd1)*(nzp+kd1)
  ntrias = kd2*(nxp - (izero-1) - (nxp+1-ixmax))*nzp
  write(2,*) 'ZONE T="Boundary 5 (Left)"  N=', nnodes,',E=', ntrias, &
              ' , ET=quadrilateral, F=FEPOINT'

   j = ny+kd1

  do k = kd1, nz+kd1, inc
   do i = kd1, nx+kd1, inc
    write(2,'(3es20.10)') x(i), y(j), z(k)
   end do
  end do

  do k = kd1, nzp
   do i = izero, ixmax-1 !, nxp

!    write(2,'(4i10)')                                 &
!                      node_number2d(i    ,k    ,nxp), & !1
!                      node_number2d(i+kd1,k    ,nxp), & !2
!                      node_number2d(i+kd1,k+kd1,nxp), & !3
!                      node_number2d(i    ,k+kd1,nxp)    !4

    write(2,'(4i10)')                                 &
                      node_number2d(i    ,k    ,nxp), & !1
                      node_number2d(i+kd1,k    ,nxp), & !2
                      node_number2d(i    ,k+kd1,nxp), & !4
                      node_number2d(i    ,k+kd1,nxp)    !4

    write(2,'(4i10)')                                 &
                      node_number2d(i+kd1,k    ,nxp), & !2
                      node_number2d(i+kd1,k+kd1,nxp), & !3
                      node_number2d(i    ,k+kd1,nxp), & !4
                      node_number2d(i    ,k+kd1,nxp)    !4

   end do
  end do

 !----------------------------------------------------------------------------
 ! Constant x planes
 !----------------------------------------------------------------------------
  nnodes = (nyp+kd1)*(nzp+kd1)
  ntrias = kd2 * nyp*nzp
  write(2,*) 'ZONE T="Boundary 5 (Near)"  N=', nnodes,',E=', ntrias, &
              ' , ET=quadrilateral, F=FEPOINT'

   i = kd1

  do k = kd1, nz+kd1, inc
   do j = kd1, ny+kd1, inc
    write(2,'(3es20.10)') x(i), y(j), z(k)
   end do
  end do

  do k = kd1, nzp
   do j = kd1, nyp

!                     node_number2d(j    ,k    ,nyp), & !1
!                     node_number2d(j+kd1,k    ,nyp), & !2
!                     node_number2d(j+kd1,k+kd1,nyp), & !3 
!                     node_number2d(j    ,k+kd1,nyp)    !4

    write(2,'(4i10)') node_number2d(j    ,k    ,nyp), & !1
                      node_number2d(j+kd1,k    ,nyp), & !2
                      node_number2d(j+kd1,k+kd1,nyp), & !3 
                      node_number2d(j+kd1,k+kd1,nyp)    !3 <-duplicate to make quad data.

    write(2,'(4i10)') node_number2d(j    ,k    ,nyp), & !1
                      node_number2d(j+kd1,k+kd1,nyp), & !3 
                      node_number2d(j    ,k+kd1,nyp), & !4
                      node_number2d(j    ,k+kd1,nyp)    !4 <-duplicate to make quad data.
   end do
  end do

 !----------------------------------------------------------------------------
  nnodes = (nyp+kd1)*(nzp+kd1)
  ntrias = kd2 * nyp*nzp
  write(2,*) 'ZONE T="Boundary 6 (Far)"  N=', nnodes,',E=', ntrias, &
              ' , ET=quadrilateral, F=FEPOINT'

   i = nx+kd1

  do k = kd1, nz+kd1, inc
   do j = kd1, ny+kd1, inc
    write(2,'(3es20.10)') x(i), y(j), z(k)
   end do
  end do

  do k = kd1, nzp
   do j = kd1, nyp

!                    node_number2d(j    ,k    ,nyp), & !1
!                    node_number2d(j+kd1,k    ,nyp), & !2
!                    node_number2d(j+kd1,k+kd1,nyp), & !3
!                    node_number2d(j    ,k+kd1,nyp)    !4

    write(2,'(4i10)') node_number2d(j    ,k    ,nyp), & !1
                      node_number2d(j+kd1,k    ,nyp), & !2
                      node_number2d(j+kd1,k+kd1,nyp), & !3
                      node_number2d(j+kd1,k+kd1,nyp)    !3 <-duplicate to make quad data.

    write(2,'(4i10)') node_number2d(j    ,k    ,nyp), & !1
                      node_number2d(j+kd1,k+kd1,nyp), & !3
                      node_number2d(j    ,k+kd1,nyp), & !4
                      node_number2d(j    ,k+kd1,nyp)    !4 <-duplicate to make quad data.

   end do
  end do


 !--------------------------------------------------------------------------------------
 !--------------------------------------------------------------------------------------
 ! Upstream (inlet) region
 !--------------------------------------------------------------------------------------
 !--------------------------------------------------------------------------------------
 if (izero > 1) then

 !----------------------------------------------------------------------------
   nnodes = (nxp+kd1)*(nyp+kd1)
   ntrias = kd2*(izero-1)*nyp
   write(2,*) 'ZONE T="Boundary 1 (Inlet: Bottom)"  N=', nnodes,',E=', ntrias, &
               ' , ET=quadrilateral, F=FEPOINT'

    k = kd1

   do j = kd1, ny+kd1, inc
    do i = kd1, nx+kd1, inc
     write(2,'(3es20.10)') x(i), y(j), z(k)
    end do
   end do

    do j = kd1, nyp
    do i = kd1, izero-1

!   write(2,'(4i10)')                                 &
!                     node_number2d(i    ,j    ,nxp), & !1
!                     node_number2d(i+kd1,j    ,nxp), & !2
!                     node_number2d(i+kd1,j+kd1,nxp), & !3
!                     node_number2d(i    ,j+kd1,nxp)    !4

    write(2,'(4i10)')                                 &
                      node_number2d(i    ,j    ,nxp), & !1
                      node_number2d(i+kd1,j    ,nxp), & !2
                      node_number2d(i    ,j+kd1,nxp), & !4
                      node_number2d(i    ,j+kd1,nxp)    !4

    write(2,'(4i10)')                                 &
                      node_number2d(i+kd1,j    ,nxp), & !2
                      node_number2d(i+kd1,j+kd1,nxp), & !3
                      node_number2d(i    ,j+kd1,nxp), & !4
                      node_number2d(i    ,j+kd1,nxp)    !4

    end do
    end do

 !----------------------------------------------------------------------------
   nnodes = (nxp+kd1)*(nyp+kd1)
   ntrias = kd2*(izero-1)*nyp
   write(2,*) 'ZONE T="Boundary 2 (Inlet: Top)"  N=', nnodes,',E=', ntrias, &
               ' , ET=quadrilateral, F=FEPOINT'

    k = nz+kd1

   do j = kd1, ny+kd1, inc
    do i = kd1, nx+kd1, inc
     write(2,'(3es20.10)') x(i), y(j), z(k)
    end do
   end do

   do j = kd1, nyp
    do i = kd1, izero-1

!    write(2,'(4i10)')                                 &
!                      node_number2d(i    ,j    ,nxp), & !1
!                      node_number2d(i+kd1,j    ,nxp), & !2
!                      node_number2d(i+kd1,j+kd1,nxp), & !3
!                      node_number2d(i    ,j+kd1,nxp)    !4

    write(2,'(4i10)')                                 &
                      node_number2d(i    ,j    ,nxp), & !1
                      node_number2d(i+kd1,j    ,nxp), & !2
                      node_number2d(i    ,j+kd1,nxp), & !4
                      node_number2d(i    ,j+kd1,nxp)    !4

    write(2,'(4i10)')                                 &
                      node_number2d(i+kd1,j    ,nxp), & !2
                      node_number2d(i+kd1,j+kd1,nxp), & !3
                      node_number2d(i    ,j+kd1,nxp), & !4
                      node_number2d(i    ,j+kd1,nxp)    !4

    end do
   end do

 !----------------------------------------------------------------------------
   nnodes = (nxp+kd1)*(nzp+kd1)
   ntrias = kd2*(izero-1)*nzp
   write(2,*) 'ZONE T="Boundary 4 (Inlet: Right)"  N=', nnodes,',E=', ntrias, &
               ' , ET=quadrilateral, F=FEPOINT'

    j = kd1

   do k = kd1, nz+kd1, inc
    do i = kd1, nx+kd1, inc
     write(2,'(3es20.10)') x(i), y(j), z(k)
    end do
   end do

   do k = kd1, nzp
    do i = kd1, izero-1

!    write(2,'(4i10)')                                 &
!                      node_number2d(i    ,k    ,nxp), & !1
!                      node_number2d(i+kd1,k    ,nxp), & !2
!                      node_number2d(i+kd1,k+kd1,nxp), & !3
!                      node_number2d(i    ,k+kd1,nxp)    !4

    write(2,'(4i10)')                                 &
                      node_number2d(i    ,k    ,nxp), & !1
                      node_number2d(i+kd1,k    ,nxp), & !2
                      node_number2d(i    ,k+kd1,nxp), & !4
                      node_number2d(i    ,k+kd1,nxp)    !4

    write(2,'(4i10)')                                 &
                      node_number2d(i+kd1,k    ,nxp), & !2
                      node_number2d(i+kd1,k+kd1,nxp), & !3
                      node_number2d(i    ,k+kd1,nxp), & !4
                      node_number2d(i    ,k+kd1,nxp)    !4

    end do
   end do

 !----------------------------------------------------------------------------
   nnodes = (nxp+kd1)*(nzp+kd1)
   ntrias = kd2*(izero-1)*nzp
   write(2,*) 'ZONE T="Boundary 5 (Inlet: Left)"  N=', nnodes,',E=', ntrias, &
               ' , ET=quadrilateral, F=FEPOINT'

    j = ny+kd1

   do k = kd1, nz+kd1, inc
    do i = kd1, nx+kd1, inc
     write(2,'(3es20.10)') x(i), y(j), z(k)
    end do
   end do

   do k = kd1, nzp
    do i = kd1, izero-1

!    write(2,'(4i10)')                                 &
!                      node_number2d(i    ,k    ,nxp), & !1
!                      node_number2d(i+kd1,k    ,nxp), & !2
!                      node_number2d(i+kd1,k+kd1,nxp), & !3
!                      node_number2d(i    ,k+kd1,nxp)    !4

    write(2,'(4i10)')                                 &
                      node_number2d(i    ,k    ,nxp), & !1
                      node_number2d(i+kd1,k    ,nxp), & !2
                      node_number2d(i    ,k+kd1,nxp), & !4
                      node_number2d(i    ,k+kd1,nxp)    !4

    write(2,'(4i10)')                                 &
                      node_number2d(i+kd1,k    ,nxp), & !2
                      node_number2d(i+kd1,k+kd1,nxp), & !3
                      node_number2d(i    ,k+kd1,nxp), & !4
                      node_number2d(i    ,k+kd1,nxp)    !4

    end do
   end do

 endif

 !--------------------------------------------------------------------------------------
 !--------------------------------------------------------------------------------------
 ! End of Upstream (inlet) region
 !--------------------------------------------------------------------------------------
 !--------------------------------------------------------------------------------------


 !--------------------------------------------------------------------------------------
 !--------------------------------------------------------------------------------------
 ! Downstream (outlet) region
 !--------------------------------------------------------------------------------------
 !--------------------------------------------------------------------------------------
 if (ixmax < nxp+1) then

 !----------------------------------------------------------------------------
   nnodes = (nxp+kd1)*(nyp+kd1)
   ntrias = kd2*(nxp+1-ixmax)*nyp
   write(2,*) 'ZONE T="Boundary 1 (Outlet: Bottom)"  N=', nnodes,',E=', ntrias, &
               ' , ET=quadrilateral, F=FEPOINT'

    k = kd1

   do j = kd1, ny+kd1, inc
    do i = kd1, nx+kd1, inc
     write(2,'(3es20.10)') x(i), y(j), z(k)
    end do
   end do

    do j = kd1, nyp
    do i = ixmax, nxp

!   write(2,'(4i10)')                                 &
!                     node_number2d(i    ,j    ,nxp), & !1
!                     node_number2d(i+kd1,j    ,nxp), & !2
!                     node_number2d(i+kd1,j+kd1,nxp), & !3
!                     node_number2d(i    ,j+kd1,nxp)    !4

    write(2,'(4i10)')                                 &
                      node_number2d(i    ,j    ,nxp), & !1
                      node_number2d(i+kd1,j    ,nxp), & !2
                      node_number2d(i    ,j+kd1,nxp), & !4
                      node_number2d(i    ,j+kd1,nxp)    !4

    write(2,'(4i10)')                                 &
                      node_number2d(i+kd1,j    ,nxp), & !2
                      node_number2d(i+kd1,j+kd1,nxp), & !3
                      node_number2d(i    ,j+kd1,nxp), & !4
                      node_number2d(i    ,j+kd1,nxp)    !4

    end do
    end do

 !----------------------------------------------------------------------------
   nnodes = (nxp+kd1)*(nyp+kd1)
   ntrias = kd2*(nxp+1-ixmax)*nyp
   write(2,*) 'ZONE T="Boundary 2 (Outlet: Top)"  N=', nnodes,',E=', ntrias, &
               ' , ET=quadrilateral, F=FEPOINT'

    k = nz+kd1

   do j = kd1, ny+kd1, inc
    do i = kd1, nx+kd1, inc
     write(2,'(3es20.10)') x(i), y(j), z(k)
    end do
   end do

   do j = kd1, nyp
    do i = ixmax, nxp

!    write(2,'(4i10)')                                 &
!                      node_number2d(i    ,j    ,nxp), & !1
!                      node_number2d(i+kd1,j    ,nxp), & !2
!                      node_number2d(i+kd1,j+kd1,nxp), & !3
!                      node_number2d(i    ,j+kd1,nxp)    !4

    write(2,'(4i10)')                                 &
                      node_number2d(i    ,j    ,nxp), & !1
                      node_number2d(i+kd1,j    ,nxp), & !2
                      node_number2d(i    ,j+kd1,nxp), & !4
                      node_number2d(i    ,j+kd1,nxp)    !4

    write(2,'(4i10)')                                 &
                      node_number2d(i+kd1,j    ,nxp), & !2
                      node_number2d(i+kd1,j+kd1,nxp), & !3
                      node_number2d(i    ,j+kd1,nxp), & !4
                      node_number2d(i    ,j+kd1,nxp)    !4

    end do
   end do

 !----------------------------------------------------------------------------
   nnodes = (nxp+kd1)*(nzp+kd1)
   ntrias = kd2*(nxp+1-ixmax)*nzp
   write(2,*) 'ZONE T="Boundary 4 (Outlet: Right)"  N=', nnodes,',E=', ntrias, &
               ' , ET=quadrilateral, F=FEPOINT'

    j = kd1

   do k = kd1, nz+kd1, inc
    do i = kd1, nx+kd1, inc
     write(2,'(3es20.10)') x(i), y(j), z(k)
    end do
   end do

   do k = kd1, nzp
    do i = ixmax, nxp

!    write(2,'(4i10)')                                 &
!                      node_number2d(i    ,k    ,nxp), & !1
!                      node_number2d(i+kd1,k    ,nxp), & !2
!                      node_number2d(i+kd1,k+kd1,nxp), & !3
!                      node_number2d(i    ,k+kd1,nxp)    !4

    write(2,'(4i10)')                                 &
                      node_number2d(i    ,k    ,nxp), & !1
                      node_number2d(i+kd1,k    ,nxp), & !2
                      node_number2d(i    ,k+kd1,nxp), & !4
                      node_number2d(i    ,k+kd1,nxp)    !4

    write(2,'(4i10)')                                 &
                      node_number2d(i+kd1,k    ,nxp), & !2
                      node_number2d(i+kd1,k+kd1,nxp), & !3
                      node_number2d(i    ,k+kd1,nxp), & !4
                      node_number2d(i    ,k+kd1,nxp)    !4

    end do
   end do

 !----------------------------------------------------------------------------
   nnodes = (nxp+kd1)*(nzp+kd1)
   ntrias = kd2*(nxp+1-ixmax)*nzp
   write(2,*) 'ZONE T="Boundary 5 (Outlet: Left)"  N=', nnodes,',E=', ntrias, &
               ' , ET=quadrilateral, F=FEPOINT'

    j = ny+kd1

   do k = kd1, nz+kd1, inc
    do i = kd1, nx+kd1, inc
     write(2,'(3es20.10)') x(i), y(j), z(k)
    end do
   end do

   do k = kd1, nzp
    do i = ixmax, nxp

 !    write(2,'(4i10)')                                 &
 !                      node_number2d(i    ,k    ,nxp), & !1
 !                      node_number2d(i+kd1,k    ,nxp), & !2
 !                      node_number2d(i+kd1,k+kd1,nxp), & !3
 !                      node_number2d(i    ,k+kd1,nxp)    !4

    write(2,'(4i10)')                                 &
                      node_number2d(i    ,k    ,nxp), & !1
                      node_number2d(i+kd1,k    ,nxp), & !2
                      node_number2d(i    ,k+kd1,nxp), & !4
                      node_number2d(i    ,k+kd1,nxp)    !4

    write(2,'(4i10)')                                 &
                      node_number2d(i+kd1,k    ,nxp), & !2
                      node_number2d(i+kd1,k+kd1,nxp), & !3
                      node_number2d(i    ,k+kd1,nxp), & !4
                      node_number2d(i    ,k+kd1,nxp)    !4

    end do
   end do

 endif
 !--------------------------------------------------------------------------------------
 !--------------------------------------------------------------------------------------
 ! End of Downstream (outlet) region
 !--------------------------------------------------------------------------------------
 !--------------------------------------------------------------------------------------

 close(2)

 end subroutine write_tecplot_boundary_file_tet
!********************************************************************************

!*******************************************************************************
!
! TET: This subroutine writes a Tecplot file for the volume grid.
!
!*******************************************************************************
 subroutine write_tecplot_volume_file_tet(inc,filename)

 implicit none
 integer(kd)  , intent(in) :: inc
 character(80), intent(in) :: filename
 integer(kd)               :: nnodes, i, j, k
 integer(kd)               :: nxp, nyp, nzp, ntet

  nxp = nx / inc
  nyp = ny / inc
  nzp = nz / inc

  nnodes = (nxp+kd1)*(nyp+kd1)*(nzp+kd1)
    ntet = 3_kd * 2_kd * nxp*nyp*nzp

  open(unit=8, file=filename, status="unknown", iostat=os)
  write(8,*) 'TITLE = "GRID"'
  write(8,*) 'VARIABLES = "x","y","z"'

 !Hex zone

   write(8,*) 'zone  n=', nnodes,',e=', ntet,' , et=tetrahedron, f=fepoint'

   do k = kd1, nz+kd1, inc
    do j = kd1, ny+kd1, inc
     do i = kd1, nx+kd1, inc
       write(8,'(3es20.10)')  x(i), y(j), z(k)
     end do
    end do
   end do

   do k = kd1, nzp
    do j = kd1, nyp
     do i = kd1, nxp

! Reference hex, which is split into 2 prisms:
!
!                       node_number(i    ,j    ,k    ,nxp,nyp), & !1
!                       node_number(i+kd1,j    ,k    ,nxp,nyp), & !2
!                       node_number(i+kd1,j+kd1,k    ,nxp,nyp), & !3
!                       node_number(i    ,j+kd1,k    ,nxp,nyp), & !4
!                       node_number(i    ,j    ,k+kd1,nxp,nyp), & !5
!                       node_number(i+kd1,j    ,k+kd1,nxp,nyp), & !6
!                       node_number(i+kd1,j+kd1,k+kd1,nxp,nyp), & !7
!                       node_number(i    ,j+kd1,k+kd1,nxp,nyp)    !8

! Reference prims which is split into 6 tets:
!
! Note: Ordering is compatible with ugrid. It is reversed for soem reason for _prs subroutine.
!
! Prism 1:
!      write(8,'(8i10)') node_number(i+kd1,j    ,k    ,nxp,nyp), & !2
!                        node_number(i+kd1,j+kd1,k+kd1,nxp,nyp), & !7
!                        node_number(i+kd1,j+kd1,k    ,nxp,nyp), & !3
!                        node_number(i+kd1,j+kd1,k    ,nxp,nyp), & !3 <-duplicate to make hex data.
!                        node_number(i    ,j    ,k    ,nxp,nyp), & !1
!                        node_number(i    ,j+kd1,k+kd1,nxp,nyp), & !8
!                        node_number(i    ,j+kd1,k    ,nxp,nyp), & !4
!                        node_number(i    ,j+kd1,k    ,nxp,nyp), & !4 <-duplicate to make hex data.
!
! Prism 2:
!      write(8,'(8i10)') node_number(i+kd1,j    ,k    ,nxp,nyp), & !2
!                        node_number(i+kd1,j    ,k+kd1,nxp,nyp), & !6
!                        node_number(i+kd1,j+kd1,k+kd1,nxp,nyp), & !7
!                        node_number(i+kd1,j+kd1,k+kd1,nxp,nyp), & !7 <-duplicate to make hex data.
!                        node_number(i    ,j    ,k    ,nxp,nyp), & !1
!                        node_number(i    ,j    ,k+kd1,nxp,nyp), & !5
!                        node_number(i    ,j+kd1,k+kd1,nxp,nyp), & !8
!                        node_number(i    ,j+kd1,k+kd1,nxp,nyp), & !8 <-duplicate to make hex data.

! Prism 1:
!      write(8,'(8i10)') &
!                        node_number(i+kd1,j    ,k    ,nxp,nyp), & !1
!                        node_number(i+kd1,j+kd1,k+kd1,nxp,nyp), & !2
!                        node_number(i+kd1,j+kd1,k    ,nxp,nyp), & !3
!                        node_number(i    ,j    ,k    ,nxp,nyp), & !4
!                        node_number(i    ,j+kd1,k+kd1,nxp,nyp), & !5
!                        node_number(i    ,j+kd1,k    ,nxp,nyp), & !6
!

      write(8,'(4i10)')                                         &
                        node_number(i    ,j    ,k    ,nxp,nyp), & !4
                        node_number(i    ,j+kd1,k    ,nxp,nyp), & !6
                        node_number(i    ,j+kd1,k+kd1,nxp,nyp), & !5
                        node_number(i+kd1,j    ,k    ,nxp,nyp)    !1

      write(8,'(4i10)')                                         &
                        node_number(i+kd1,j    ,k    ,nxp,nyp), & !1
                        node_number(i+kd1,j+kd1,k+kd1,nxp,nyp), & !2
                        node_number(i+kd1,j+kd1,k    ,nxp,nyp), & !3
                        node_number(i    ,j+kd1,k+kd1,nxp,nyp)    !5

      write(8,'(4i10)')                                         &
                        node_number(i+kd1,j+kd1,k    ,nxp,nyp), & !3
                        node_number(i    ,j+kd1,k+kd1,nxp,nyp), & !5
                        node_number(i    ,j+kd1,k    ,nxp,nyp), & !6
                        node_number(i+kd1,j    ,k    ,nxp,nyp)    !1

! Prism 2:
!      write(8,'(4i10)') 
!                        node_number(i+kd1,j    ,k    ,nxp,nyp), & !1
!                        node_number(i+kd1,j    ,k+kd1,nxp,nyp), & !2
!                        node_number(i+kd1,j+kd1,k+kd1,nxp,nyp), & !3
!                        node_number(i    ,j    ,k    ,nxp,nyp), & !4
!                        node_number(i    ,j    ,k+kd1,nxp,nyp), & !5
!                        node_number(i    ,j+kd1,k+kd1,nxp,nyp), & !6

      write(8,'(4i10)')                                         &
                        node_number(i    ,j    ,k    ,nxp,nyp), & !4
                        node_number(i    ,j+kd1,k+kd1,nxp,nyp), & !6
                        node_number(i    ,j    ,k+kd1,nxp,nyp), & !5
                        node_number(i+kd1,j    ,k    ,nxp,nyp)    !1

      write(8,'(4i10)')                                         &
                        node_number(i+kd1,j    ,k    ,nxp,nyp), & !1
                        node_number(i+kd1,j    ,k+kd1,nxp,nyp), & !2
                        node_number(i+kd1,j+kd1,k+kd1,nxp,nyp), & !3
                        node_number(i    ,j+kd1,k+kd1,nxp,nyp)    !6

      write(8,'(4i10)')                                         &
                        node_number(i+kd1,j    ,k+kd1,nxp,nyp), & !2
                        node_number(i    ,j    ,k+kd1,nxp,nyp), & !5
                        node_number(i    ,j+kd1,k+kd1,nxp,nyp), & !6
                        node_number(i+kd1,j    ,k    ,nxp,nyp)    !1

     end do
    end do
   end do

  close(8)

 end subroutine write_tecplot_volume_file_tet
!*******************************************************************************



!********************************************************************************
!********************************************************************************
!********************************************************************************
!********************************************************************************
!********************************************************************************
!********************************************************************************

!*******************************************************************************
!
! PRS: This subroutine writes a ugrid file.
!
!*******************************************************************************
 subroutine write_ugrid_file_tet(inc,filename)

 implicit none
 integer(kd)  , intent(in) :: inc
 character(80), intent(in) :: filename
 integer(kd)               :: nnodes, i, j, k
 integer(kd)               :: nxp, nyp, nzp, ntet, ntrias_b

  nxp = nx / inc
  nyp = ny / inc
  nzp = nz / inc


    nnodes = (nxp+kd1)*(nyp+kd1)*(nzp+kd1)
      ntet = kd3*kd2* nxp*nyp*nzp
  ntrias_b = kd2*kd2*(nyp*nzp + nxp*nyp + nzp*nxp)

  if ( ugrid_file_unformatted ) then
    open(unit=9, file=filename, form='unformatted',access="stream",&
                                             status='unknown', iostat=os )
!   write(9) nnodes,   ntrias_b,    nquads_b,   ntet,    0, nprs, nhex
    write(9) nnodes,   ntrias_b,    0,   ntet,    0, 0,    0
  else
    open(unit=9, file=filename, status="unknown", iostat=os)
    !#nodes, #tri_faces, #quad_faces, #tetra, #pyr, #prz, #hex
    write(9,'(7i20)') nnodes, ntrias_b, 0, ntet,   0,  0,  0
  endif

!---------------------------------------------------------------
!---------------------------------------------------------------
!---------------------------------------------------------------
!---------------------------------------------------------------
!---------------------------------------------------------------
!---------------------------------------------------------------
!(1) Unformatted grid file
!---------------------------------------------------------------
!---------------------------------------------------------------
!---------------------------------------------------------------
!---------------------------------------------------------------
!---------------------------------------------------------------
!---------------------------------------------------------------

  if ( ugrid_file_unformatted ) then

  !------------------------------------------------------------------------------
  ! Nodes
    do k = kd1, nz+kd1, inc
     do j = kd1, ny+kd1, inc
      do i = kd1, nx+kd1, inc
        write(9)  x(i), y(j), z(k)
      end do
     end do
    end do

  !------------------------------------------------------------------------------
  ! Tria faces = ntria  !Oriented inward

   !(5)Near: iquad = 2*(nx*nz+nx*nz)+kd1, 2*(nx*nz+nx*nz)+ny*nz
     i = kd1
    do k = kd1, nzp
     do j = kd1, nyp

! Reference quad split into two triangles:
!      write(9) node_number(i,j    ,k    ,nxp,nyp), & !1
!               node_number(i,j+kd1,k    ,nxp,nyp), & !2
!               node_number(i,j+kd1,k+kd1,nxp,nyp), & !3
!               node_number(i,j    ,k+kd1,nxp,nyp)    !4

       write(9)                                     &
                node_number(i,j    ,k    ,nxp,nyp), & !1
                node_number(i,j+kd1,k    ,nxp,nyp), & !2
                node_number(i,j+kd1,k+kd1,nxp,nyp)    !3 

       write(9)                                     &
                node_number(i,j    ,k    ,nxp,nyp), & !1
                node_number(i,j+kd1,k+kd1,nxp,nyp), & !3 
                node_number(i,j    ,k+kd1,nxp,nyp)    !4

     end do
    end do

   !(6)Far: iquad = 2*(nx*nz+nx*nz)+ny*nz+kd1, 2*(nx*nz+nx*nz+ny*nz)
     i = nxp+kd1
    do k = kd1, nzp
     do j = kd1, nyp

! Reference quad split into two triangles:
!      write(9) node_number(i,j    ,k    ,nxp,nyp), & !1
!               node_number(i,j    ,k+kd1,nxp,nyp), & !2
!               node_number(i,j+kd1,k+kd1,nxp,nyp), & !3
!               node_number(i,j+kd1,k    ,nxp,nyp)    !4

       write(9)                                     &
                node_number(i,j    ,k    ,nxp,nyp), & !1
                node_number(i,j    ,k+kd1,nxp,nyp), & !2
                node_number(i,j+kd1,k+kd1,nxp,nyp)    !3

       write(9)                                     &
                node_number(i,j    ,k    ,nxp,nyp), & !1
                node_number(i,j+kd1,k+kd1,nxp,nyp), & !3 
                node_number(i,j+kd1,k    ,nxp,nyp)    !4

     end do
    end do

  !------------------------------------------------------------------------------
  ! Quad faces = nquad  !Oriented inward

   !(1)Bottom: iquad = 1, nx*nz
     k = kd1
    do j = kd1, nyp
     do i = izero, ixmax-1

!      write(9) &
!               node_number(i    ,j    ,k,nxp,nyp), & !1
!               node_number(i+kd1,j    ,k,nxp,nyp), & !2
!               node_number(i+kd1,j+kd1,k,nxp,nyp), & !3
!               node_number(i    ,j+kd1,k,nxp,nyp)    !4

    write(9)                                 &
               node_number(i    ,j    ,k,nxp,nyp), & !1
               node_number(i+kd1,j    ,k,nxp,nyp), & !2
               node_number(i    ,j+kd1,k,nxp,nyp)    !4

    write(9)                                 &
               node_number(i+kd1,j    ,k,nxp,nyp), & !2
               node_number(i+kd1,j+kd1,k,nxp,nyp), & !3
               node_number(i    ,j+kd1,k,nxp,nyp)    !4

     end do
    end do

   !(2)Top:    iquad = nx*nz+kd1, 2*nx*nz
     k = nzp+kd1
    do j = kd1, nyp
     do i = izero, ixmax-1

!      write(9) &
!               node_number(i    ,j    ,k,nxp,nyp), & !1
!               node_number(i    ,j+kd1,k,nxp,nyp), & !2
!               node_number(i+kd1,j+kd1,k,nxp,nyp), & !3
!               node_number(i+kd1,j    ,k,nxp,nyp)    !4

    write(9)                                 &
               node_number(i    ,j    ,k,nxp,nyp), & !1
               node_number(i    ,j+kd1,k,nxp,nyp), & !2
               node_number(i+kd1,j    ,k,nxp,nyp)    !4

    write(9)                                 &
               node_number(i    ,j+kd1,k,nxp,nyp), & !2
               node_number(i+kd1,j+kd1,k,nxp,nyp), & !3
               node_number(i+kd1,j    ,k,nxp,nyp)    !4

     end do
    end do

   !(3)Right (y=ymin): iquad = 2*nx*nz+kd1, 2*nx*nz+nx*nz
     j = kd1
    do k = kd1, nzp
     do i = izero, ixmax-1

!      write(9) &
!               node_number(i    ,j,k    ,nxp,nyp), & !1
!               node_number(i    ,j,k+kd1,nxp,nyp), & !2
!               node_number(i+kd1,j,k+kd1,nxp,nyp), & !3
!               node_number(i+kd1,j,k    ,nxp,nyp)    !4

    write(9)                                 &
               node_number(i    ,j,k    ,nxp,nyp), & !1
               node_number(i    ,j,k+kd1,nxp,nyp), & !2
               node_number(i+kd1,j,k    ,nxp,nyp)    !4

    write(9)                                 &
               node_number(i    ,j,k+kd1,nxp,nyp), & !2
               node_number(i+kd1,j,k+kd1,nxp,nyp), & !3
               node_number(i+kd1,j,k    ,nxp,nyp)    !4

     end do
    end do

   !(4)Left (y=ymax): iquad = 2*nx*nz+nx*nz+kd1, 2*(nx*nz+nx*nz)
     j = nyp+kd1
    do k = kd1, nzp
     do i = izero, ixmax-1

!      write(9) &
!               node_number(i    ,j,k    ,nxp,nyp), & !1
!               node_number(i+kd1,j,k    ,nxp,nyp), & !2
!               node_number(i+kd1,j,k+kd1,nxp,nyp), & !3
!               node_number(i    ,j,k+kd1,nxp,nyp)    !4

    write(9)                                 &
               node_number(i    ,j,k    ,nxp,nyp), & !1
               node_number(i+kd1,j,k    ,nxp,nyp), & !2
               node_number(i    ,j,k+kd1,nxp,nyp)    !4

    write(9)                                 &
               node_number(i+kd1,j,k    ,nxp,nyp), & !2
               node_number(i+kd1,j,k+kd1,nxp,nyp), & !3
               node_number(i    ,j,k+kd1,nxp,nyp)    !4

     end do
    end do

   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   if (izero > 1) then
   
   !(7)Bottom: iquad = 1, nx*nz
     k = kd1
    do j = kd1, nyp
     do i = kd1, izero-kd1

!      write(9) &
!               node_number(i    ,j    ,k,nxp,nyp), &
!               node_number(i+kd1,j    ,k,nxp,nyp), &
!               node_number(i+kd1,j+kd1,k,nxp,nyp), &
!               node_number(i    ,j+kd1,k,nxp,nyp)

    write(9)                                 &
               node_number(i    ,j    ,k,nxp,nyp), & !1
               node_number(i+kd1,j    ,k,nxp,nyp), & !2
               node_number(i    ,j+kd1,k,nxp,nyp)    !4

    write(9)                                 &
               node_number(i+kd1,j    ,k,nxp,nyp), & !2
               node_number(i+kd1,j+kd1,k,nxp,nyp), & !3
               node_number(i    ,j+kd1,k,nxp,nyp)    !4

     end do
    end do

   !(8)Top:    iquad = nx*nz+kd1, 2*nx*nz
     k = nzp+kd1
    do j = kd1, nyp
     do i = kd1, izero-kd1

!      write(9) node_number(i    ,j    ,k,nxp,nyp), &
!               node_number(i    ,j+kd1,k,nxp,nyp), &
!               node_number(i+kd1,j+kd1,k,nxp,nyp), &
!               node_number(i+kd1,j    ,k,nxp,nyp)

    write(9)                                 &
               node_number(i    ,j    ,k,nxp,nyp), & !1
               node_number(i    ,j+kd1,k,nxp,nyp), & !2
               node_number(i+kd1,j    ,k,nxp,nyp)    !4

    write(9)                                 &
               node_number(i    ,j+kd1,k,nxp,nyp), & !2
               node_number(i+kd1,j+kd1,k,nxp,nyp), & !3
               node_number(i+kd1,j    ,k,nxp,nyp)    !4

     end do
    end do

   !(9)Right (y=ymin): iquad = 2*nx*nz+kd1, 2*nx*nz+nx*nz
     j = kd1
    do k = kd1, nzp
     do i = kd1, izero-kd1

!      write(9) node_number(i    ,j,k    ,nxp,nyp), &
!               node_number(i    ,j,k+kd1,nxp,nyp), &
!               node_number(i+kd1,j,k+kd1,nxp,nyp), &
!               node_number(i+kd1,j,k    ,nxp,nyp)

    write(9)                                 &
               node_number(i    ,j,k    ,nxp,nyp), & !1
               node_number(i    ,j,k+kd1,nxp,nyp), & !2
               node_number(i+kd1,j,k    ,nxp,nyp)    !4

    write(9)                                 &
               node_number(i    ,j,k+kd1,nxp,nyp), & !2
               node_number(i+kd1,j,k+kd1,nxp,nyp), & !3
               node_number(i+kd1,j,k    ,nxp,nyp)    !4

     end do
    end do

   !(10)Left (y=ymax): iquad = 2*nx*nz+nx*nz+kd1, 2*(nx*nz+nx*nz)
     j = nyp+kd1
    do k = kd1, nzp
     do i = kd1, izero-kd1

!      write(9) node_number(i    ,j,k    ,nxp,nyp), &
!               node_number(i+kd1,j,k    ,nxp,nyp), &
!               node_number(i+kd1,j,k+kd1,nxp,nyp), &
!               node_number(i    ,j,k+kd1,nxp,nyp)

    write(9)                                 &
               node_number(i    ,j,k    ,nxp,nyp), & !1
               node_number(i+kd1,j,k    ,nxp,nyp), & !2
               node_number(i    ,j,k+kd1,nxp,nyp)    !4

    write(9)                                 &
               node_number(i+kd1,j,k    ,nxp,nyp), & !2
               node_number(i+kd1,j,k+kd1,nxp,nyp), & !3
               node_number(i    ,j,k+kd1,nxp,nyp)    !4

     end do
    end do

   endif
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------

   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   if (ixmax < nxp+1) then
   
   !(7)Bottom: iquad = 1, nx*nz
     k = kd1
    do j = kd1, nyp
     do i = ixmax, nxp

!      write(9) node_number(i    ,j    ,k,nxp,nyp), &
!               node_number(i+kd1,j    ,k,nxp,nyp), &
!               node_number(i+kd1,j+kd1,k,nxp,nyp), &
!               node_number(i    ,j+kd1,k,nxp,nyp)

    write(9)                                 &
               node_number(i    ,j    ,k,nxp,nyp), & !1
               node_number(i+kd1,j    ,k,nxp,nyp), & !2
               node_number(i    ,j+kd1,k,nxp,nyp)    !4

    write(9)                                 &
               node_number(i+kd1,j    ,k,nxp,nyp), & !2
               node_number(i+kd1,j+kd1,k,nxp,nyp), & !3
               node_number(i    ,j+kd1,k,nxp,nyp)    !4

     end do
    end do

   !(8)Top:    iquad = nx*nz+kd1, 2*nx*nz
     k = nzp+kd1
    do j = kd1, nyp
     do i = ixmax, nxp

!      write(9) node_number(i    ,j    ,k,nxp,nyp), &
!               node_number(i    ,j+kd1,k,nxp,nyp), &
!               node_number(i+kd1,j+kd1,k,nxp,nyp), &
!               node_number(i+kd1,j    ,k,nxp,nyp)

    write(9)                                 &
               node_number(i    ,j    ,k,nxp,nyp), & !1
               node_number(i    ,j+kd1,k,nxp,nyp), & !2
               node_number(i+kd1,j    ,k,nxp,nyp)    !4

    write(9)                                 &
               node_number(i    ,j+kd1,k,nxp,nyp), & !2
               node_number(i+kd1,j+kd1,k,nxp,nyp), & !3
               node_number(i+kd1,j    ,k,nxp,nyp)    !4

     end do
    end do

   !(9)Right (y=ymin): iquad = 2*nx*nz+kd1, 2*nx*nz+nx*nz
     j = kd1
    do k = kd1, nzp
     do i = ixmax, nxp

!      write(9) node_number(i    ,j,k    ,nxp,nyp), &
!               node_number(i    ,j,k+kd1,nxp,nyp), &
!               node_number(i+kd1,j,k+kd1,nxp,nyp), &
!               node_number(i+kd1,j,k    ,nxp,nyp)

    write(9)                                 &
               node_number(i    ,j,k    ,nxp,nyp), & !1
               node_number(i    ,j,k+kd1,nxp,nyp), & !2
               node_number(i+kd1,j,k    ,nxp,nyp)    !4

    write(9)                                 &
               node_number(i    ,j,k+kd1,nxp,nyp), & !2
               node_number(i+kd1,j,k+kd1,nxp,nyp), & !3
               node_number(i+kd1,j,k    ,nxp,nyp)    !4

     end do
    end do

   !(10)Left (y=ymax): iquad = 2*nx*nz+nx*nz+kd1, 2*(nx*nz+nx*nz)
     j = nyp+kd1
    do k = kd1, nzp
     do i = ixmax, nxp

!      write(9) node_number(i    ,j,k    ,nxp,nyp), &
!               node_number(i+kd1,j,k    ,nxp,nyp), &
!               node_number(i+kd1,j,k+kd1,nxp,nyp), &
!               node_number(i    ,j,k+kd1,nxp,nyp)

    write(9)                                 &
               node_number(i    ,j,k    ,nxp,nyp), & !1
               node_number(i+kd1,j,k    ,nxp,nyp), & !2
               node_number(i    ,j,k+kd1,nxp,nyp)    !4

    write(9)                                 &
               node_number(i+kd1,j,k    ,nxp,nyp), & !2
               node_number(i+kd1,j,k+kd1,nxp,nyp), & !3
               node_number(i    ,j,k+kd1,nxp,nyp)    !4

     end do
    end do

   endif
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------

  !--------------------------------------------------------------------------------
  ! Face tag

   !(5)Near: iquad = 2*(nx*nz+nx*nz)+kd1, 2*(nx*nz+nx*nz)+ny*nz
    do k = kd1, nzp
     do j = kd1, nyp
      write(9) 5
      write(9) 5
     end do
    end do

   !(6)Far: iquad = 2*(nx*nz+nx*nz)+ny*nz+kd1, 2*(nx*nz+nx*nz+ny*nz)
    do k = kd1, nzp
     do j = kd1, nyp
      write(9) 6
      write(9) 6
     end do
    end do

   !(1)Bottom: iquad = 1, nx*nz
    do j = kd1, nyp
     do i = izero, ixmax-1 !nxp
      write(9) 1
      write(9) 1
     end do
    end do

   !(2)Top:    iquad = nx*nz+kd1, 2*nx*nz
    do j = kd1, nyp
     do i = izero, ixmax-1 ! nxp
      write(9) 2
      write(9) 2
     end do
    end do

   !(3)Right: iquad = 2*nx*nz+kd1, 2*nx*nz+nx*nz
    do k = kd1, nzp
     do i = izero, ixmax-1 ! nxp
      write(9) 3
      write(9) 3
     end do
    end do

   !(4)Left: iquad = 2*nx*nz+nx*nz+kd1, 2*(nx*nz+nx*nz)
    do k = kd1, nzp
     do i = izero, ixmax-1 ! nxp
      write(9) 4
      write(9) 4
     end do
    end do

   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   if (izero > 1) then

   !(1)Bottom: iquad = 1, nx*nz
    do j = kd1, nyp
     do i = 1, izero-1
      write(9) 7
      write(9) 7
     end do
    end do

   !(2)Top:    iquad = nx*nz+kd1, 2*nx*nz
    do j = kd1, nyp
     do i = 1, izero-1
      write(9) 8
      write(9) 8
     end do
    end do

   !(3)Right: iquad = 2*nx*nz+kd1, 2*nx*nz+nx*nz
    do k = kd1, nzp
     do i = 1, izero-1
      write(9) 9
      write(9) 9
     end do
    end do

   !(4)Left: iquad = 2*nx*nz+nx*nz+kd1, 2*(nx*nz+nx*nz)
    do k = kd1, nzp
     do i = 1, izero-1
      write(9) 10
      write(9) 10
     end do
    end do

   endif
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------

   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   if (ixmax < nxp+1) then

   !(1)Bottom: iquad = 1, nx*nz
    do j = kd1, nyp
     do i = ixmax, nxp
      write(9) 11
      write(9) 11
     end do
    end do

   !(2)Top:    iquad = nx*nz+kd1, 2*nx*nz
    do j = kd1, nyp
     do i = ixmax, nxp
      write(9) 12
      write(9) 12
     end do
    end do

   !(3)Right: iquad = 2*nx*nz+kd1, 2*nx*nz+nx*nz
    do k = kd1, nzp
     do i = ixmax, nxp
      write(9) 13
      write(9) 13
     end do
    end do

   !(4)Left: iquad = 2*nx*nz+nx*nz+kd1, 2*(nx*nz+nx*nz)
    do k = kd1, nzp
     do i = ixmax, nxp
      write(9) 14
      write(9) 14
     end do
    end do

   endif
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------

  ! Tet
    do k = kd1, nzp
     do j = kd1, nyp
      do i = kd1, nxp

! Hexa to be split into 2 prims:
!      write(9) &
!               node_number(i    ,j    ,k    ,nxp,nyp), & !1
!               node_number(i+kd1,j    ,k    ,nxp,nyp), & !2
!               node_number(i+kd1,j+kd1,k    ,nxp,nyp), & !3
!               node_number(i    ,j+kd1,k    ,nxp,nyp), & !4
!               node_number(i    ,j    ,k+kd1,nxp,nyp), & !5
!               node_number(i+kd1,j    ,k+kd1,nxp,nyp), & !6
!               node_number(i+kd1,j+kd1,k+kd1,nxp,nyp), & !7
!               node_number(i    ,j+kd1,k+kd1,nxp,nyp)    !8

! Prism1 to be split into 2 tets.
!       write(9)                                         &
!                node_number(i+kd1,j    ,k    ,nxp,nyp), & !2
!                node_number(i+kd1,j+kd1,k+kd1,nxp,nyp), & !7
!                node_number(i+kd1,j+kd1,k    ,nxp,nyp), & !3
!                node_number(i    ,j    ,k    ,nxp,nyp), & !1
!                node_number(i    ,j+kd1,k+kd1,nxp,nyp), & !8
!                node_number(i    ,j+kd1,k    ,nxp,nyp)    !4

      write(9)                                         &
                        node_number(i    ,j    ,k    ,nxp,nyp), & !4
                        node_number(i    ,j+kd1,k    ,nxp,nyp), & !6
                        node_number(i    ,j+kd1,k+kd1,nxp,nyp), & !5
                        node_number(i+kd1,j    ,k    ,nxp,nyp)    !1

      write(9)                                         &
                        node_number(i+kd1,j    ,k    ,nxp,nyp), & !1
                        node_number(i+kd1,j+kd1,k+kd1,nxp,nyp), & !2
                        node_number(i+kd1,j+kd1,k    ,nxp,nyp), & !3
                        node_number(i    ,j+kd1,k+kd1,nxp,nyp)    !5

      write(9)                                         &
                        node_number(i+kd1,j+kd1,k    ,nxp,nyp), & !3
                        node_number(i    ,j+kd1,k+kd1,nxp,nyp), & !5
                        node_number(i    ,j+kd1,k    ,nxp,nyp), & !6
                        node_number(i+kd1,j    ,k    ,nxp,nyp)    !1

! Prism2 to be split into 2 tets.
!       write(9)                                         &
!                node_number(i+kd1,j    ,k    ,nxp,nyp), & !2
!                node_number(i+kd1,j    ,k+kd1,nxp,nyp), & !6
!                node_number(i+kd1,j+kd1,k+kd1,nxp,nyp), & !7
!                node_number(i    ,j    ,k    ,nxp,nyp), & !1
!                node_number(i    ,j    ,k+kd1,nxp,nyp), & !5
!                node_number(i    ,j+kd1,k+kd1,nxp,nyp)    !8

      write(9)                                         &
                        node_number(i    ,j    ,k    ,nxp,nyp), & !4
                        node_number(i    ,j+kd1,k+kd1,nxp,nyp), & !6
                        node_number(i    ,j    ,k+kd1,nxp,nyp), & !5
                        node_number(i+kd1,j    ,k    ,nxp,nyp)    !1

      write(9)                                         &
                        node_number(i+kd1,j    ,k    ,nxp,nyp), & !1
                        node_number(i+kd1,j    ,k+kd1,nxp,nyp), & !2
                        node_number(i+kd1,j+kd1,k+kd1,nxp,nyp), & !3
                        node_number(i    ,j+kd1,k+kd1,nxp,nyp)    !6

      write(9)                                         &
                        node_number(i+kd1,j    ,k+kd1,nxp,nyp), & !2
                        node_number(i    ,j    ,k+kd1,nxp,nyp), & !5
                        node_number(i    ,j+kd1,k+kd1,nxp,nyp), & !6
                        node_number(i+kd1,j    ,k    ,nxp,nyp)    !1

      end do
     end do
    end do

!---------------------------------------------------------------
!---------------------------------------------------------------
!---------------------------------------------------------------
!---------------------------------------------------------------
!---------------------------------------------------------------
!---------------------------------------------------------------
!(2) Formatted grid file
!---------------------------------------------------------------
!---------------------------------------------------------------
!---------------------------------------------------------------
!---------------------------------------------------------------
!---------------------------------------------------------------
!---------------------------------------------------------------

  else

  !------------------------------------------------------------------------------
  ! Nodes
    do k = kd1, nz+kd1, inc
     do j = kd1, ny+kd1, inc
      do i = kd1, nx+kd1, inc
        write(9,'(3es26.15)')  x(i), y(j), z(k)
      end do
     end do
    end do

  !------------------------------------------------------------------------------
  ! Tria faces = ntria  !Oriented inward

   !(5)Near: iquad = 2*(nx*nz+nx*nz)+kd1, 2*(nx*nz+nx*nz)+ny*nz
     i = kd1
    do k = kd1, nzp
     do j = kd1, nyp

! Reference quad split into two triangles:
!      write(9,'(4i20)') node_number(i,j    ,k    ,nxp,nyp), & !1
!                        node_number(i,j+kd1,k    ,nxp,nyp), & !2
!                        node_number(i,j+kd1,k+kd1,nxp,nyp), & !3
!                        node_number(i,j    ,k+kd1,nxp,nyp)    !4

       write(9,'(3i20)')                                     &  
                         node_number(i,j    ,k    ,nxp,nyp), & !1
                         node_number(i,j+kd1,k    ,nxp,nyp), & !2
                         node_number(i,j+kd1,k+kd1,nxp,nyp)    !3 

       write(9,'(3i20)')                                     &  
                         node_number(i,j    ,k    ,nxp,nyp), & !1
                         node_number(i,j+kd1,k+kd1,nxp,nyp), & !3 
                         node_number(i,j    ,k+kd1,nxp,nyp)    !4

     end do
    end do

   !(6)Far: iquad = 2*(nx*nz+nx*nz)+ny*nz+kd1, 2*(nx*nz+nx*nz+ny*nz)
     i = nxp+kd1
    do k = kd1, nzp
     do j = kd1, nyp

! Reference quad split into two triangles:
!      write(9,'(4i20)') node_number(i,j    ,k    ,nxp,nyp), & !1
!                        node_number(i,j    ,k+kd1,nxp,nyp), & !2
!                        node_number(i,j+kd1,k+kd1,nxp,nyp), & !3
!                        node_number(i,j+kd1,k    ,nxp,nyp)    !4

       write(9,'(3i20)')                                     &  
                         node_number(i,j    ,k    ,nxp,nyp), & !1
                         node_number(i,j    ,k+kd1,nxp,nyp), & !2
                         node_number(i,j+kd1,k+kd1,nxp,nyp)    !3

       write(9,'(3i20)')                                     &  
                         node_number(i,j    ,k    ,nxp,nyp), & !1
                         node_number(i,j+kd1,k+kd1,nxp,nyp), & !3 
                         node_number(i,j+kd1,k    ,nxp,nyp)    !4

     end do
    end do

  !------------------------------------------------------------------------------
  ! Quad faces = nquad  !Oriented inward

   !(1)Bottom: iquad = 1, nx*nz
     k = kd1
    do j = kd1, nyp
     do i = izero, ixmax-1

    write(9,'(3i20)')                                 &
               node_number(i    ,j    ,k,nxp,nyp), & !1
               node_number(i+kd1,j    ,k,nxp,nyp), & !2
               node_number(i    ,j+kd1,k,nxp,nyp)    !4

    write(9,'(3i20)')                                 &
               node_number(i+kd1,j    ,k,nxp,nyp), & !2
               node_number(i+kd1,j+kd1,k,nxp,nyp), & !3
               node_number(i    ,j+kd1,k,nxp,nyp)    !4

     end do
    end do

   !(2)Top:    iquad = nx*nz+kd1, 2*nx*nz
     k = nzp+kd1
    do j = kd1, nyp
     do i = izero, ixmax-1

    write(9,'(3i20)')                                 &
               node_number(i    ,j    ,k,nxp,nyp), & !1
               node_number(i    ,j+kd1,k,nxp,nyp), & !2
               node_number(i+kd1,j    ,k,nxp,nyp)    !4

    write(9,'(3i20)')                                 &
               node_number(i    ,j+kd1,k,nxp,nyp), & !2
               node_number(i+kd1,j+kd1,k,nxp,nyp), & !3
               node_number(i+kd1,j    ,k,nxp,nyp)    !4

     end do
    end do

   !(3)Right: iquad = 2*nx*nz+kd1, 2*nx*nz+nx*nz
     j = kd1
    do k = kd1, nzp
     do i = izero, ixmax-1

    write(9,'(3i20)')                                 &
               node_number(i    ,j,k    ,nxp,nyp), & !1
               node_number(i    ,j,k+kd1,nxp,nyp), & !2
               node_number(i+kd1,j,k    ,nxp,nyp)    !4

    write(9,'(3i20)')                                 &
               node_number(i    ,j,k+kd1,nxp,nyp), & !2
               node_number(i+kd1,j,k+kd1,nxp,nyp), & !3
               node_number(i+kd1,j,k    ,nxp,nyp)    !4

     end do
    end do

   !(4)Left: iquad = 2*nx*nz+nx*nz+kd1, 2*(nx*nz+nx*nz)
     j = nyp+kd1
    do k = kd1, nzp
     do i = izero, ixmax-1

    write(9,'(3i20)')                                 &
               node_number(i    ,j,k    ,nxp,nyp), & !1
               node_number(i+kd1,j,k    ,nxp,nyp), & !2
               node_number(i    ,j,k+kd1,nxp,nyp)    !4

    write(9,'(3i20)')                                 &
               node_number(i+kd1,j,k    ,nxp,nyp), & !2
               node_number(i+kd1,j,k+kd1,nxp,nyp), & !3
               node_number(i    ,j,k+kd1,nxp,nyp)    !4

     end do
    end do

   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   if (izero > 1) then
   
   !(7)Bottom: iquad = 1, nx*nz
     k = kd1
    do j = kd1, nyp
     do i = kd1, izero-kd1

    write(9,'(3i20)')                                 &
               node_number(i    ,j    ,k,nxp,nyp), & !1
               node_number(i+kd1,j    ,k,nxp,nyp), & !2
               node_number(i    ,j+kd1,k,nxp,nyp)    !4

    write(9,'(3i20)')                                 &
               node_number(i+kd1,j    ,k,nxp,nyp), & !2
               node_number(i+kd1,j+kd1,k,nxp,nyp), & !3
               node_number(i    ,j+kd1,k,nxp,nyp)    !4

     end do
    end do

   !(8)Top:    iquad = nx*nz+kd1, 2*nx*nz
     k = nzp+kd1
    do j = kd1, nyp
     do i = kd1, izero-kd1

    write(9,'(3i20)')                                 &
               node_number(i    ,j    ,k,nxp,nyp), & !1
               node_number(i    ,j+kd1,k,nxp,nyp), & !2
               node_number(i+kd1,j    ,k,nxp,nyp)    !4

    write(9,'(3i20)')                                 &
               node_number(i    ,j+kd1,k,nxp,nyp), & !2
               node_number(i+kd1,j+kd1,k,nxp,nyp), & !3
               node_number(i+kd1,j    ,k,nxp,nyp)    !4

     end do
    end do

   !(9)Right (y=ymin): iquad = 2*nx*nz+kd1, 2*nx*nz+nx*nz
     j = kd1
    do k = kd1, nzp
     do i = kd1, izero-kd1

    write(9,'(3i20)')                                 &
               node_number(i    ,j,k    ,nxp,nyp), & !1
               node_number(i    ,j,k+kd1,nxp,nyp), & !2
               node_number(i+kd1,j,k    ,nxp,nyp)    !4

    write(9,'(3i20)')                                 &
               node_number(i    ,j,k+kd1,nxp,nyp), & !2
               node_number(i+kd1,j,k+kd1,nxp,nyp), & !3
               node_number(i+kd1,j,k    ,nxp,nyp)    !4

     end do
    end do

   !(10)Left (y=ymax): iquad = 2*nx*nz+nx*nz+kd1, 2*(nx*nz+nx*nz)
     j = nyp+kd1
    do k = kd1, nzp
     do i = kd1, izero-kd1

    write(9,'(3i20)')                                 &
               node_number(i    ,j,k    ,nxp,nyp), & !1
               node_number(i+kd1,j,k    ,nxp,nyp), & !2
               node_number(i    ,j,k+kd1,nxp,nyp)    !4

    write(9,'(3i20)')                                 &
               node_number(i+kd1,j,k    ,nxp,nyp), & !2
               node_number(i+kd1,j,k+kd1,nxp,nyp), & !3
               node_number(i    ,j,k+kd1,nxp,nyp)    !4

     end do
    end do

   endif
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------

   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   if (ixmax < nxp+1) then
   
   !(7)Bottom: iquad = 1, nx*nz
     k = kd1
    do j = kd1, nyp
     do i = ixmax, nxp

    write(9,'(3i20)')                                 &
               node_number(i    ,j    ,k,nxp,nyp), & !1
               node_number(i+kd1,j    ,k,nxp,nyp), & !2
               node_number(i    ,j+kd1,k,nxp,nyp)    !4

    write(9,'(3i20)')                                 &
               node_number(i+kd1,j    ,k,nxp,nyp), & !2
               node_number(i+kd1,j+kd1,k,nxp,nyp), & !3
               node_number(i    ,j+kd1,k,nxp,nyp)    !4

     end do
    end do

   !(8)Top:    iquad = nx*nz+kd1, 2*nx*nz
     k = nzp+kd1
    do j = kd1, nyp
     do i = ixmax, nxp

    write(9,'(3i20)')                                 &
               node_number(i    ,j    ,k,nxp,nyp), & !1
               node_number(i    ,j+kd1,k,nxp,nyp), & !2
               node_number(i+kd1,j    ,k,nxp,nyp)    !4

    write(9,'(3i20)')                                 &
               node_number(i    ,j+kd1,k,nxp,nyp), & !2
               node_number(i+kd1,j+kd1,k,nxp,nyp), & !3
               node_number(i+kd1,j    ,k,nxp,nyp)    !4

     end do
    end do

   !(9)Right (y=ymin): iquad = 2*nx*nz+kd1, 2*nx*nz+nx*nz
     j = kd1
    do k = kd1, nzp
     do i = ixmax, nxp

    write(9,'(3i20)')                                 &
               node_number(i    ,j,k    ,nxp,nyp), & !1
               node_number(i    ,j,k+kd1,nxp,nyp), & !2
               node_number(i+kd1,j,k    ,nxp,nyp)    !4

    write(9,'(3i20)')                                 &
               node_number(i    ,j,k+kd1,nxp,nyp), & !2
               node_number(i+kd1,j,k+kd1,nxp,nyp), & !3
               node_number(i+kd1,j,k    ,nxp,nyp)    !4

     end do
    end do

   !(10)Left (y=ymax): iquad = 2*nx*nz+nx*nz+kd1, 2*(nx*nz+nx*nz)
     j = nyp+kd1
    do k = kd1, nzp
     do i = ixmax, nxp

    write(9,'(3i20)')                                 &
               node_number(i    ,j,k    ,nxp,nyp), & !1
               node_number(i+kd1,j,k    ,nxp,nyp), & !2
               node_number(i    ,j,k+kd1,nxp,nyp)    !4

    write(9,'(3i20)')                                 &
               node_number(i+kd1,j,k    ,nxp,nyp), & !2
               node_number(i+kd1,j,k+kd1,nxp,nyp), & !3
               node_number(i    ,j,k+kd1,nxp,nyp)    !4

     end do
    end do

   endif
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------

  !--------------------------------------------------------------------------------
  ! Face tag

   !(5)Near: iquad = 2*(nx*nz+nx*nz)+kd1, 2*(nx*nz+nx*nz)+ny*nz
    do k = kd1, nzp
     do j = kd1, nyp
      write(9,'(i10)') 5
      write(9,'(i10)') 5
     end do
    end do

   !(6)Far: iquad = 2*(nx*nz+nx*nz)+ny*nz+kd1, 2*(nx*nz+nx*nz+ny*nz)
    do k = kd1, nzp
     do j = kd1, nyp
      write(9,'(i10)') 6
      write(9,'(i10)') 6
     end do
    end do

   !(1)Bottom: iquad = 1, nx*nz
    do j = kd1, nyp
     do i = izero, ixmax-1
      write(9,'(i10)') 1
      write(9,'(i10)') 1
     end do
    end do

   !(2)Top:    iquad = nx*nz+kd1, 2*nx*nz
    do j = kd1, nyp
     do i = izero, ixmax-1
      write(9,'(i10)') 2
      write(9,'(i10)') 2
     end do
    end do

   !(3)Right: iquad = 2*nx*nz+kd1, 2*nx*nz+nx*nz
    do k = kd1, nzp
     do i = izero, ixmax-1
      write(9,'(i10)') 3
      write(9,'(i10)') 3
     end do
    end do

   !(4)Left: iquad = 2*nx*nz+nx*nz+kd1, 2*(nx*nz+nx*nz)
    do k = kd1, nzp
     do i = izero, ixmax-1
      write(9,'(i10)') 4
      write(9,'(i10)') 4
     end do
    end do
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   if (izero > 1) then

   !(1)Bottom: iquad = 1, nx*nz
    do j = kd1, nyp
     do i = 1, izero-1
      write(9,'(i10)') 7
      write(9,'(i10)') 7
     end do
    end do

   !(2)Top:    iquad = nx*nz+kd1, 2*nx*nz
    do j = kd1, nyp
     do i = 1, izero-1
      write(9,'(i10)') 8
      write(9,'(i10)') 8
     end do
    end do

   !(3)Right: iquad = 2*nx*nz+kd1, 2*nx*nz+nx*nz
    do k = kd1, nzp
     do i = 1, izero-1
      write(9,'(i10)') 9
      write(9,'(i10)') 9
     end do
    end do

   !(4)Left: iquad = 2*nx*nz+nx*nz+kd1, 2*(nx*nz+nx*nz)
    do k = kd1, nzp
     do i = 1, izero-1
      write(9,'(i10)') 10
      write(9,'(i10)') 10
     end do
    end do

   endif
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------

   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   if (ixmax < nxp+1) then

   !(1)Bottom: iquad = 1, nx*nz
    do j = kd1, nyp
     do i = ixmax, nxp
      write(9,'(i10)') 11
      write(9,'(i10)') 11
     end do
    end do

   !(2)Top:    iquad = nx*nz+kd1, 2*nx*nz
    do j = kd1, nyp
     do i = ixmax, nxp
      write(9,'(i10)') 12
      write(9,'(i10)') 12
     end do
    end do

   !(3)Right: iquad = 2*nx*nz+kd1, 2*nx*nz+nx*nz
    do k = kd1, nzp
     do i = ixmax, nxp
      write(9,'(i10)') 13
      write(9,'(i10)') 13
     end do
    end do

   !(4)Left: iquad = 2*nx*nz+nx*nz+kd1, 2*(nx*nz+nx*nz)
    do k = kd1, nzp
     do i = ixmax, nxp
      write(9,'(i10)') 14
      write(9,'(i10)') 14
     end do
    end do

   endif
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------
   !--------------------------------------------------------------------------------------

  ! Tet
    do k = kd1, nzp
     do j = kd1, nyp
      do i = kd1, nxp

! Hexa to be split into 2 prims:
!      write(9,'(8i20)') &
!               node_number(i    ,j    ,k    ,nxp,nyp), & !1
!               node_number(i+kd1,j    ,k    ,nxp,nyp), & !2
!               node_number(i+kd1,j+kd1,k    ,nxp,nyp), & !3
!               node_number(i    ,j+kd1,k    ,nxp,nyp), & !4
!               node_number(i    ,j    ,k+kd1,nxp,nyp), & !5
!               node_number(i+kd1,j    ,k+kd1,nxp,nyp), & !6
!               node_number(i+kd1,j+kd1,k+kd1,nxp,nyp), & !7
!               node_number(i    ,j+kd1,k+kd1,nxp,nyp)    !8

! Prism1 to be split into 2 tets.
!       write(9,'(6i20)')                                &
!                node_number(i+kd1,j    ,k    ,nxp,nyp), & !2
!                node_number(i+kd1,j+kd1,k+kd1,nxp,nyp), & !7
!                node_number(i+kd1,j+kd1,k    ,nxp,nyp), & !3
!                node_number(i    ,j    ,k    ,nxp,nyp), & !1
!                node_number(i    ,j+kd1,k+kd1,nxp,nyp), & !8
!                node_number(i    ,j+kd1,k    ,nxp,nyp)    !4

      write(9,'(4i10)')                                         &
                        node_number(i    ,j    ,k    ,nxp,nyp), & !4
                        node_number(i    ,j+kd1,k    ,nxp,nyp), & !6
                        node_number(i    ,j+kd1,k+kd1,nxp,nyp), & !5
                        node_number(i+kd1,j    ,k    ,nxp,nyp)    !1

      write(9,'(4i10)')                                         &
                        node_number(i+kd1,j    ,k    ,nxp,nyp), & !1
                        node_number(i+kd1,j+kd1,k+kd1,nxp,nyp), & !2
                        node_number(i+kd1,j+kd1,k    ,nxp,nyp), & !3
                        node_number(i    ,j+kd1,k+kd1,nxp,nyp)    !5

      write(9,'(4i10)')                                         &
                        node_number(i+kd1,j+kd1,k    ,nxp,nyp), & !3
                        node_number(i    ,j+kd1,k+kd1,nxp,nyp), & !5
                        node_number(i    ,j+kd1,k    ,nxp,nyp), & !6
                        node_number(i+kd1,j    ,k    ,nxp,nyp)    !1

! Prism2 to be split into 2 tets.
!       write(9,'(6i20)')                                &
!                node_number(i+kd1,j    ,k    ,nxp,nyp), & !2
!                node_number(i+kd1,j    ,k+kd1,nxp,nyp), & !6
!                node_number(i+kd1,j+kd1,k+kd1,nxp,nyp), & !7
!                node_number(i    ,j    ,k    ,nxp,nyp), & !1
!                node_number(i    ,j    ,k+kd1,nxp,nyp), & !5
!                node_number(i    ,j+kd1,k+kd1,nxp,nyp)    !8

      write(9,'(4i10)')                                         &
                        node_number(i    ,j    ,k    ,nxp,nyp), & !4
                        node_number(i    ,j+kd1,k+kd1,nxp,nyp), & !6
                        node_number(i    ,j    ,k+kd1,nxp,nyp), & !5
                        node_number(i+kd1,j    ,k    ,nxp,nyp)    !1

      write(9,'(4i10)')                                         &
                        node_number(i+kd1,j    ,k    ,nxp,nyp), & !1
                        node_number(i+kd1,j    ,k+kd1,nxp,nyp), & !2
                        node_number(i+kd1,j+kd1,k+kd1,nxp,nyp), & !3
                        node_number(i    ,j+kd1,k+kd1,nxp,nyp)    !6

      write(9,'(4i10)')                                         &
                        node_number(i+kd1,j    ,k+kd1,nxp,nyp), & !2
                        node_number(i    ,j    ,k+kd1,nxp,nyp), & !5
                        node_number(i    ,j+kd1,k+kd1,nxp,nyp), & !6
                        node_number(i+kd1,j    ,k    ,nxp,nyp)    !1

      end do
     end do
    end do

  endif

!---------------------------------------------------------------
! End of Unformatted or Formatted
!---------------------------------------------------------------

  close(9)

 end subroutine write_ugrid_file_tet
!********************************************************************************

!*******************************************************************************
! This subroutine writes a su2 grid file.
!
! Note: Nodes -> i = 0,1,2,...; Elements -> i = 0,1,2,...
!
!*******************************************************************************
 subroutine write_su2grid_file_tet(inc,filename)

 implicit none
 integer(kd)  , intent(in) :: inc
 character(80), intent(in) :: filename

 integer(kd) :: nnodes, i, j, k, counter
 integer(kd) :: nxp, nyp, nzp, ntet

  nxp    = nx / inc
  nyp    = ny / inc
  nzp    = nz / inc
  nnodes = (nxp+kd1)*(nyp+kd1)*(nzp+kd1)
  ntet   = kd3*kd2*nxp*nyp*nzp

  write(*,*) "    write_su2grid_file: nnodes = ", nnodes
  write(*,*) "    write_su2grid_file:   ntet = ", ntet

  open(unit=7, file=filename, status="unknown", iostat=os)

  write(7,*) "%"
  write(7,*) "% Problem dimension"
  write(7,*) "%"
  write(7,5) 3
5 format('NDIME= ',i12)

   write(7,*) "%"
   write(7,*) "% Inner element connectivity"
   write(7,10) ntet
10 format('NELEM= ',i12)

   counter = 0

 !-------------------------------------------------------------------------
 ! Elements

  ! No tetra
  ! No hex
  ! Only prisms:
    do k = kd1, nzp
     do j = kd1, nyp
      do i = kd1, nxp

! Reference hex to be split into two prisms:
!      write(7,'(10i10)') 12                                       , &
!                        node_number(i    ,j    ,k    ,nxp,nyp)-kd1, & !1
!                        node_number(i+kd1,j    ,k    ,nxp,nyp)-kd1, & !2
!                        node_number(i+kd1,j+kd1,k    ,nxp,nyp)-kd1, & !3
!                        node_number(i    ,j+kd1,k    ,nxp,nyp)-kd1, & !4
!                        node_number(i    ,j    ,k+kd1,nxp,nyp)-kd1, & !5
!                        node_number(i+kd1,j    ,k+kd1,nxp,nyp)-kd1, & !6
!                        node_number(i+kd1,j+kd1,k+kd1,nxp,nyp)-kd1, & !7
!                        node_number(i    ,j+kd1,k+kd1,nxp,nyp)-kd1, & !8
!                        counter

! Prism1 to be split into 3 tetra:
!      write(7,'(8i10)') 13                                        , &
!                        node_number(i+kd1,j    ,k    ,nxp,nyp)-kd1, & !2
!                        node_number(i+kd1,j+kd1,k    ,nxp,nyp)-kd1, & !3
!                        node_number(i+kd1,j+kd1,k+kd1,nxp,nyp)-kd1, & !7
!                        node_number(i    ,j    ,k    ,nxp,nyp)-kd1, & !1
!                        node_number(i    ,j+kd1,k    ,nxp,nyp)-kd1, & !4
!                        node_number(i    ,j+kd1,k+kd1,nxp,nyp)-kd1, & !8
!                        counter

      write(7,'(6i10)') 10,                                         &
                        node_number(i    ,j    ,k    ,nxp,nyp)-kd1, & !4
                        node_number(i    ,j+kd1,k    ,nxp,nyp)-kd1, & !6
                        node_number(i    ,j+kd1,k+kd1,nxp,nyp)-kd1, & !5
                        node_number(i+kd1,j    ,k    ,nxp,nyp)-kd1, & !1
                        counter

      counter = counter + kd1

      write(7,'(6i10)') 10,                                         &
                        node_number(i+kd1,j    ,k    ,nxp,nyp)-kd1, & !1
                        node_number(i+kd1,j+kd1,k+kd1,nxp,nyp)-kd1, & !2
                        node_number(i+kd1,j+kd1,k    ,nxp,nyp)-kd1, & !3
                        node_number(i    ,j+kd1,k+kd1,nxp,nyp)-kd1, & !5
                        counter

      counter = counter + kd1

      write(7,'(6i10)') 10,                                         &
                        node_number(i+kd1,j+kd1,k    ,nxp,nyp)-kd1, & !3
                        node_number(i    ,j+kd1,k+kd1,nxp,nyp)-kd1, & !5
                        node_number(i    ,j+kd1,k    ,nxp,nyp)-kd1, & !6
                        node_number(i+kd1,j    ,k    ,nxp,nyp)-kd1, & !1
                        counter

      counter = counter + kd1


!      write(7,'(8i10)') 13                                        , &
!                        node_number(i+kd1,j    ,k    ,nxp,nyp)-kd1, & !2
!                        node_number(i+kd1,j+kd1,k+kd1,nxp,nyp)-kd1, & !7
!                        node_number(i+kd1,j    ,k+kd1,nxp,nyp)-kd1, & !6
!                        node_number(i    ,j    ,k    ,nxp,nyp)-kd1, & !1
!                        node_number(i    ,j+kd1,k+kd1,nxp,nyp)-kd1, & !8
!                        node_number(i    ,j    ,k+kd1,nxp,nyp)-kd1, & !5
!                        counter

      write(7,'(6i10)') 10,                                         &
                        node_number(i    ,j    ,k    ,nxp,nyp)-kd1, & !4
                        node_number(i    ,j+kd1,k+kd1,nxp,nyp)-kd1, & !6
                        node_number(i    ,j    ,k+kd1,nxp,nyp)-kd1, & !5
                        node_number(i+kd1,j    ,k    ,nxp,nyp)-kd1, & !1
                        counter

      counter = counter + kd1

      write(7,'(6i10)') 10,                                         &
                        node_number(i+kd1,j    ,k    ,nxp,nyp)-kd1, & !1
                        node_number(i+kd1,j    ,k+kd1,nxp,nyp)-kd1, & !2
                        node_number(i+kd1,j+kd1,k+kd1,nxp,nyp)-kd1, & !3
                        node_number(i    ,j+kd1,k+kd1,nxp,nyp)-kd1, & !6
                        counter

      counter = counter + kd1

      write(7,'(6i10)') 10,                                         &
                        node_number(i+kd1,j    ,k+kd1,nxp,nyp)-kd1, & !2
                        node_number(i    ,j    ,k+kd1,nxp,nyp)-kd1, & !5
                        node_number(i    ,j+kd1,k+kd1,nxp,nyp)-kd1, & !6
                        node_number(i+kd1,j    ,k    ,nxp,nyp)-kd1, & !1
                        counter

      counter = counter + kd1

      end do
     end do
    end do

   write(*,*) "  --- elm check", ntet, counter

 !--------------------------------------------------------------------------
 ! Nodes

   write(7,*) "%"
   write(7,*) "% Node coordinates"
   write(7,*) "%"
   write(7,20) nnodes
20 format('NPOIN= ', i12)

   counter = 0

  ! Nodes
    do k = kd1, nz+kd1, inc
     do j = kd1, ny+kd1, inc
      do i = kd1, nx+kd1, inc
        write(7,'(3es26.15,i20)')  x(i), y(j), z(k), counter
        counter = counter + kd1
      end do
     end do
    end do

   write(*,*) "  --- node check", nnodes, counter

 !--------------------------------------------------------------------------
 ! Boundary

    write(7,*) "%"
    write(7,*) "% Boundary elements"
    write(7,*) "%"

  !+Inlet and +Outlet
   if     (izero >  1 .and. ixmax <  nxp+1) then

    write(7,30) 14

  !+Inlet
   elseif (izero >  1 .and. ixmax >= nxp+1) then

    write(7,30) 10

  !Outlet
   elseif (izero <= 1 .and. ixmax <  nxp+1) then

    write(7,30) 10

  !No inlet and outlet
   else

    write(7,30) 6

   endif

30 format('NMARK= ',i12)

40 format('MARKER_TAG= ',a)
50 format('MARKER_ELEMS= ', i12)

 !--------------------------------------------------------------------------
 ! (1) Viscous bottom surface (z=zmin)
 !--------------------------------------------------------------------------

   write(*,40) "DUCT_VISCOUS_ZMIN"
   write(7,40) "DUCT_VISCOUS_ZMIN"
   write(7,50) kd2*(nxp-(izero-1) - (nxp+1-ixmax))*nyp

   k = kd1
    do j = kd1, nyp
     do i = izero, ixmax-1

!      write(7,'(5i20)') 9, &
!                        node_number(i    ,j    ,k,nxp,nyp)-kd1, & !1
!                        node_number(i+kd1,j    ,k,nxp,nyp)-kd1, & !2
!                        node_number(i+kd1,j+kd1,k,nxp,nyp)-kd1, & !3
!                        node_number(i    ,j+kd1,k,nxp,nyp)-kd1    !4

      write(7,'(4i20)') 5,                               &
                 node_number(i    ,j    ,k,nxp,nyp)-kd1, & !1
                 node_number(i+kd1,j    ,k,nxp,nyp)-kd1, & !2
                 node_number(i    ,j+kd1,k,nxp,nyp)-kd1    !4

      write(7,'(4i20)') 5,                               &
                 node_number(i+kd1,j    ,k,nxp,nyp)-kd1, & !2
                 node_number(i+kd1,j+kd1,k,nxp,nyp)-kd1, & !3
                 node_number(i    ,j+kd1,k,nxp,nyp)-kd1    !4

     end do
    end do

 !--------------------------------------------------------------------------
 ! (2) Top symmetry z (z=zmax)
 !--------------------------------------------------------------------------

   write(*,40) "DUCT_SYMMETRY_Z"
   write(7,40) "DUCT_SYMMETRY_Z"
   write(7,50) kd2*(nxp-(izero-1) - (nxp+1-ixmax))*nyp

     k = nzp+kd1
    do j = kd1, nyp
     do i = izero, ixmax-1

!      write(7,'(5i20)') 9, &
!                        node_number(i    ,j    ,k,nxp,nyp)-kd1, & !1
!                        node_number(i    ,j+kd1,k,nxp,nyp)-kd1, & !2
!                        node_number(i+kd1,j+kd1,k,nxp,nyp)-kd1, & !3
!                        node_number(i+kd1,j    ,k,nxp,nyp)-kd1    !4

    write(7,'(4i20)') 5,                               &
               node_number(i    ,j    ,k,nxp,nyp)-kd1, & !1
               node_number(i    ,j+kd1,k,nxp,nyp)-kd1, & !2
               node_number(i+kd1,j    ,k,nxp,nyp)-kd1    !4

    write(7,'(4i20)') 5,                               &
               node_number(i    ,j+kd1,k,nxp,nyp)-kd1, & !2
               node_number(i+kd1,j+kd1,k,nxp,nyp)-kd1, & !3
               node_number(i+kd1,j    ,k,nxp,nyp)-kd1    !4

     end do
    end do
 !--------------------------------------------------------------------------


 !--------------------------------------------------------------------------
 ! (3) Viscous side (y=ymin)
 !--------------------------------------------------------------------------

   write(*,40) "DUCT_VISCOUS_YMIN"
   write(7,40) "DUCT_VISCOUS_YMIN"
   write(7,50) kd2*(nxp-(izero-1) - (nxp+1-ixmax))*nzp

     j = kd1
    do k = kd1, nzp
     do i = izero, ixmax-1

!      write(7,'(5i20)') 9, &
!                        node_number(i    ,j,k    ,nxp,nyp)-kd1, & !1
!                        node_number(i    ,j,k+kd1,nxp,nyp)-kd1, & !2
!                        node_number(i+kd1,j,k+kd1,nxp,nyp)-kd1, & !3
!                        node_number(i+kd1,j,k    ,nxp,nyp)-kd1    !4

    write(7,'(4i20)') 5,                               &
               node_number(i    ,j,k    ,nxp,nyp)-kd1, & !1
               node_number(i    ,j,k+kd1,nxp,nyp)-kd1, & !2
               node_number(i+kd1,j,k    ,nxp,nyp)-kd1    !4

    write(7,'(4i20)') 5,                               &
               node_number(i    ,j,k+kd1,nxp,nyp)-kd1, & !2
               node_number(i+kd1,j,k+kd1,nxp,nyp)-kd1, & !3
               node_number(i+kd1,j,k    ,nxp,nyp)-kd1    !4

     end do
    end do

 !--------------------------------------------------------------------------
 ! (4) Symmetry y (y=ymax)
 !--------------------------------------------------------------------------

   write(*,40) "DUCT_SYMMETRY_Y"
   write(7,40) "DUCT_SYMMETRY_Y"
   write(7,50) kd2*(nxp-(izero-1) - (nxp+1-ixmax))*nzp

     j = nyp+kd1
    do k = kd1, nzp
     do i = izero, ixmax-1

!      write(7,'(5i20)') 9, &
!                        node_number(i    ,j,k    ,nxp,nyp)-kd1, & !1
!                        node_number(i+kd1,j,k    ,nxp,nyp)-kd1, & !2
!                        node_number(i+kd1,j,k+kd1,nxp,nyp)-kd1, & !3
!                        node_number(i    ,j,k+kd1,nxp,nyp)-kd1    !4

    write(7,'(4i20)') 5,                               &
               node_number(i    ,j,k    ,nxp,nyp)-kd1, & !1
               node_number(i+kd1,j,k    ,nxp,nyp)-kd1, & !2
               node_number(i    ,j,k+kd1,nxp,nyp)-kd1    !4

    write(7,'(4i20)') 5,                               &
               node_number(i+kd1,j,k    ,nxp,nyp)-kd1, & !2
               node_number(i+kd1,j,k+kd1,nxp,nyp)-kd1, & !3
               node_number(i    ,j,k+kd1,nxp,nyp)-kd1    !4

     end do
    end do
 !--------------------------------------------------------------------------


 !--------------------------------------------------------------------------
 ! (5) Inflow (x=xmin)
 !--------------------------------------------------------------------------

   write(*,40) "INFLOW"
   write(7,40) "INFLOW"
   write(7,50) kd2*nyp*nzp

     i = kd1
    do k = kd1, nzp
     do j = kd1, nyp

! Reference quad split into two triangles:
!     write(7,'(5i20)') 9, node_number(i,j    ,k    ,nxp,nyp)-kd1, & !1
!                          node_number(i,j+kd1,k    ,nxp,nyp)-kd1, & !2
!                          node_number(i,j+kd1,k+kd1,nxp,nyp)-kd1, & !3
!                          node_number(i,j    ,k+kd1,nxp,nyp)-kd1    !4

      write(7,'(4i20)') 5, node_number(i,j    ,k    ,nxp,nyp)-kd1, & !1
                           node_number(i,j+kd1,k    ,nxp,nyp)-kd1, & !2
                           node_number(i,j+kd1,k+kd1,nxp,nyp)-kd1    !3

      write(7,'(4i20)') 5, node_number(i,j    ,k    ,nxp,nyp)-kd1, & !1
                           node_number(i,j+kd1,k+kd1,nxp,nyp)-kd1, & !3
                           node_number(i,j    ,k+kd1,nxp,nyp)-kd1    !4

     end do
    end do

 !--------------------------------------------------------------------------
 ! (6) Outflow (x=xmax)
 !--------------------------------------------------------------------------

   write(*,40) "OUTFLOW"
   write(7,40) "OUTFLOW"
   write(7,50) kd2*nyp*nzp

     i = nxp+kd1
    do k = kd1, nzp
     do j = kd1, nyp

! Reference quad split into two triangles:
!     write(7,'(5i20)') 9, node_number(i,j    ,k    ,nxp,nyp)-kd1, & !1
!                          node_number(i,j    ,k+kd1,nxp,nyp)-kd1, & !2
!                          node_number(i,j+kd1,k+kd1,nxp,nyp)-kd1, & !3
!                          node_number(i,j+kd1,k    ,nxp,nyp)-kd1    !4

      write(7,'(4i20)') 5, node_number(i,j    ,k    ,nxp,nyp)-kd1, & !1
                           node_number(i,j    ,k+kd1,nxp,nyp)-kd1, & !2
                           node_number(i,j+kd1,k+kd1,nxp,nyp)-kd1    !3

      write(7,'(4i20)') 5, node_number(i,j    ,k    ,nxp,nyp)-kd1, & !1
                           node_number(i,j+kd1,k+kd1,nxp,nyp)-kd1, & !3
                           node_number(i,j+kd1,k    ,nxp,nyp)-kd1    !4

     end do
    end do

 !--------------------------------------------------------------------------


 if (izero > 1) then

 !--------------------------------------------------------------------------
 ! (7) Symmetry z (z=zmin)
 !--------------------------------------------------------------------------

   write(*,40) "INLET_SYMMETRY_Z"
   write(7,40) "INLET_SYMMETRY_Z"
   write(7,50) kd2*(izero-1)*nyp

   k = kd1
    do j = kd1, nyp
     do i = 1, izero-1

      write(7,'(4i20)') 5,                               &
                 node_number(i    ,j    ,k,nxp,nyp)-kd1, & !1
                 node_number(i+kd1,j    ,k,nxp,nyp)-kd1, & !2
                 node_number(i    ,j+kd1,k,nxp,nyp)-kd1    !4

      write(7,'(4i20)') 5,                               &
                 node_number(i+kd1,j    ,k,nxp,nyp)-kd1, & !2
                 node_number(i+kd1,j+kd1,k,nxp,nyp)-kd1, & !3
                 node_number(i    ,j+kd1,k,nxp,nyp)-kd1    !4

     end do
    end do

 !--------------------------------------------------------------------------
 ! (8) Symmetry z (z=zmax)
 !--------------------------------------------------------------------------

   write(*,40) "INLET_SYMMETRY_Z"
   write(7,40) "INLET_SYMMETRY_Z"
   write(7,50) kd2*(izero-1)*nyp

     k = nzp+kd1
    do j = kd1, nyp
     do i = 1, izero-1

    write(7,'(4i20)') 5,                               &
               node_number(i    ,j    ,k,nxp,nyp)-kd1, & !1
               node_number(i    ,j+kd1,k,nxp,nyp)-kd1, & !2
               node_number(i+kd1,j    ,k,nxp,nyp)-kd1    !4

    write(7,'(4i20)') 5,                               &
               node_number(i    ,j+kd1,k,nxp,nyp)-kd1, & !2
               node_number(i+kd1,j+kd1,k,nxp,nyp)-kd1, & !3
               node_number(i+kd1,j    ,k,nxp,nyp)-kd1    !4

     end do
    end do
 !--------------------------------------------------------------------------


 !--------------------------------------------------------------------------
 ! (9) Symmetry y (y=ymin)
 !--------------------------------------------------------------------------

   write(*,40) "INLET_SYMMETRY_Y"
   write(7,40) "INLET_SYMMETRY_Y"
   write(7,50) kd2*(izero-1)*nzp

     j = kd1
    do k = kd1, nzp
     do i = 1, izero-1

    write(7,'(4i20)') 5,                               &
               node_number(i    ,j,k    ,nxp,nyp)-kd1, & !1
               node_number(i    ,j,k+kd1,nxp,nyp)-kd1, & !2
               node_number(i+kd1,j,k    ,nxp,nyp)-kd1    !4

    write(7,'(4i20)') 5,                               &
               node_number(i    ,j,k+kd1,nxp,nyp)-kd1, & !2
               node_number(i+kd1,j,k+kd1,nxp,nyp)-kd1, & !3
               node_number(i+kd1,j,k    ,nxp,nyp)-kd1    !4

     end do
    end do

 !--------------------------------------------------------------------------
 ! (10) Symmetry y (y=ymax)
 !-------------------------------------------------------------------------

   write(*,40) "INLET_SYMMETRY_Y"
   write(7,40) "INLET_SYMMETRY_Y"
   write(7,50) kd2*(izero-1)*nzp

     j = nyp+kd1
    do k = kd1, nzp
     do i = 1, izero-1

    write(7,'(4i20)') 5,                               &
               node_number(i    ,j,k    ,nxp,nyp)-kd1, & !1
               node_number(i+kd1,j,k    ,nxp,nyp)-kd1, & !2
               node_number(i    ,j,k+kd1,nxp,nyp)-kd1    !4

    write(7,'(4i20)') 5,                               &
               node_number(i+kd1,j,k    ,nxp,nyp)-kd1, & !2
               node_number(i+kd1,j,k+kd1,nxp,nyp)-kd1, & !3
               node_number(i    ,j,k+kd1,nxp,nyp)-kd1    !4

     end do
    end do
 !--------------------------------------------------------------------------

 endif

 if (ixmax < nxp+1) then

 !--------------------------------------------------------------------------
 ! (7) Symmetry z (z=zmin)
 !--------------------------------------------------------------------------

   write(*,40) "OUTLET_SYMMETRY_Z"
   write(7,40) "OUTLET_SYMMETRY_Z"
   write(7,50) kd2*(nxp+1-ixmax)*nyp

   k = kd1
    do j = kd1, nyp
     do i = ixmax, nxp

      write(7,'(4i20)') 5,                               &
                 node_number(i    ,j    ,k,nxp,nyp)-kd1, & !1
                 node_number(i+kd1,j    ,k,nxp,nyp)-kd1, & !2
                 node_number(i    ,j+kd1,k,nxp,nyp)-kd1    !4

      write(7,'(4i20)') 5,                               &
                 node_number(i+kd1,j    ,k,nxp,nyp)-kd1, & !2
                 node_number(i+kd1,j+kd1,k,nxp,nyp)-kd1, & !3
                 node_number(i    ,j+kd1,k,nxp,nyp)-kd1    !4

     end do
    end do

 !--------------------------------------------------------------------------
 ! (8) Symmetry z (z=zmax)
 !--------------------------------------------------------------------------

   write(*,40) "OUTLET_SYMMETRY_Z"
   write(7,40) "OUTLET_SYMMETRY_Z"
   write(7,50) kd2*(nxp+1-ixmax)*nyp

     k = nzp+kd1
    do j = kd1, nyp
     do i = ixmax, nxp

    write(7,'(4i20)') 5,                               &
               node_number(i    ,j    ,k,nxp,nyp)-kd1, & !1
               node_number(i    ,j+kd1,k,nxp,nyp)-kd1, & !2
               node_number(i+kd1,j    ,k,nxp,nyp)-kd1    !4

    write(7,'(4i20)') 5,                               &
               node_number(i    ,j+kd1,k,nxp,nyp)-kd1, & !2
               node_number(i+kd1,j+kd1,k,nxp,nyp)-kd1, & !3
               node_number(i+kd1,j    ,k,nxp,nyp)-kd1    !4

     end do
    end do
 !--------------------------------------------------------------------------


 !--------------------------------------------------------------------------
 ! (9) Symmetry y (y=ymin)
 !--------------------------------------------------------------------------

   write(*,40) "OUTLET_SYMMETRY_Y"
   write(7,40) "OUTLET_SYMMETRY_Y"
   write(7,50) kd2*(nxp+1-ixmax)*nzp

     j = kd1
    do k = kd1, nzp
     do i = ixmax, nxp

    write(7,'(4i20)') 5,                               &
               node_number(i    ,j,k    ,nxp,nyp)-kd1, & !1
               node_number(i    ,j,k+kd1,nxp,nyp)-kd1, & !2
               node_number(i+kd1,j,k    ,nxp,nyp)-kd1    !4

    write(7,'(4i20)') 5,                               &
               node_number(i    ,j,k+kd1,nxp,nyp)-kd1, & !2
               node_number(i+kd1,j,k+kd1,nxp,nyp)-kd1, & !3
               node_number(i+kd1,j,k    ,nxp,nyp)-kd1    !4

     end do
    end do

 !--------------------------------------------------------------------------
 ! (10) Symmetry y (y=ymax)
 !-------------------------------------------------------------------------

   write(*,40) "OUTLET_SYMMETRY_Y"
   write(7,40) "OUTLET_SYMMETRY_Y"
   write(7,50) kd2*(nxp+1-ixmax)*nzp

     j = nyp+kd1
    do k = kd1, nzp
     do i = ixmax, nxp

    write(7,'(4i20)') 5,                               &
               node_number(i    ,j,k    ,nxp,nyp)-kd1, & !1
               node_number(i+kd1,j,k    ,nxp,nyp)-kd1, & !2
               node_number(i    ,j,k+kd1,nxp,nyp)-kd1    !4

    write(7,'(4i20)') 5,                               &
               node_number(i+kd1,j,k    ,nxp,nyp)-kd1, & !2
               node_number(i+kd1,j,k+kd1,nxp,nyp)-kd1, & !3
               node_number(i    ,j,k+kd1,nxp,nyp)-kd1    !4

     end do
    end do
 !--------------------------------------------------------------------------

 endif

 !--------------------------------------------------------------------------
 !--------------------------------------------------------------------------

  close(7)

 end subroutine write_su2grid_file_tet
!********************************************************************************




end program duct_grid
