The cell-centered data for the budget quantites are in the file curvedbackstep_budgets_all.dat
in blocked "Tecplot-format" (Tecplot360 version).  
For example, for the particular zone with:

 I=768, J=160, K=1, ZONETYPE=Ordered
 DATAPACKING=BLOCK

The data from this zone should be read from the files (ignoring title, zone,
variables, and other header lines) via:

      idim=768
      jdim=160
      read(2,*) ((x(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((y(i,j),i=1,idim),j=1,jdim)
      read(2,*)     ((production_k(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((turb_transport_k(i,j),i=1,idim),j=1,jdim)
      read(2,*)    ((pres_strain_k(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((pres_diffusion_k(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((visc_diffusion_k(i,j),i=1,idim),j=1,jdim)
      read(2,*)    ((dissipation_k(i,j),i=1,idim),j=1,jdim)
      read(2,*)     ((convection_k(i,j),i=1,idim),j=1,jdim)
      read(2,*)     ((production_uu(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((turb_transport_uu(i,j),i=1,idim),j=1,jdim)
      read(2,*)    ((pres_strain_uu(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((pres_diffusion_uu(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((visc_diffusion_uu(i,j),i=1,idim),j=1,jdim)
      read(2,*)    ((dissipation_uu(i,j),i=1,idim),j=1,jdim)
      read(2,*)     ((convection_uu(i,j),i=1,idim),j=1,jdim)
      read(2,*)     ((production_uv(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((turb_transport_uv(i,j),i=1,idim),j=1,jdim)
      read(2,*)    ((pres_strain_uv(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((pres_diffusion_uv(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((visc_diffusion_uv(i,j),i=1,idim),j=1,jdim)
      read(2,*)    ((dissipation_uv(i,j),i=1,idim),j=1,jdim)
      read(2,*)     ((convection_uv(i,j),i=1,idim),j=1,jdim)
      read(2,*)     ((production_vv(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((turb_transport_vv(i,j),i=1,idim),j=1,jdim)
      read(2,*)    ((pres_strain_vv(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((pres_diffusion_vv(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((visc_diffusion_vv(i,j),i=1,idim),j=1,jdim)
      read(2,*)    ((dissipation_vv(i,j),i=1,idim),j=1,jdim)
      read(2,*)     ((convection_vv(i,j),i=1,idim),j=1,jdim)
      read(2,*)     ((production_ww(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((turb_transport_ww(i,j),i=1,idim),j=1,jdim)
      read(2,*)    ((pres_strain_ww(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((pres_diffusion_ww(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((visc_diffusion_ww(i,j),i=1,idim),j=1,jdim)
      read(2,*)    ((dissipation_ww(i,j),i=1,idim),j=1,jdim)
      read(2,*)     ((convection_ww(i,j),i=1,idim),j=1,jdim)

Variables:
"x" = x cell-center location (nondimensionalized so that backstep height=1)
"y" = y cell-center location (nondimensionalized so that backstep height=1)
"production_XX" = production term for quantity XX (=-<ui*uk>duj/dxk-<uj*uk>dui/dxk for Re stress terms)
"turb_transport_XX" = turbulent transport (triple moment) term for quantity XX (=-d<ui*uj*uk>/dxk for Re stress terms)
"pres_strain_XX" = pressure strain term for quantity XX (=<p/rho*(dui/dxj+duj/dxi)> for Re stress terms)
"pres_diffusion_XX" = pressure diffusion term for quantity XX (=-1/rho*(d<uj*p>/dxi+d<ui*p>/dxj) for Re stress terms)
"visc_diffusion_XX" = viscous diffusion term for quantity XX (=nu*d^2<ui*uj>/(dxk*dxk) for Re stress terms)
"dissipation_XX" = dissipation term for quantity XX (=2*nu*<dui/dxk*duj/dxi> for Re stress terms)
"convection_XX" = convection term for quantity XX (=<uk>d<uiuj>/Dxk for Re stress terms)
"sgsterm_XX" = SGS (viscous diffusion) term for quantity XX

Note: Computed dissipation (eps_ij) terms (extracted explicitly from the simulation)
are NOT given here; instead, they were determined from the
other terms to make the balance sum to zero.  As noted in J of Turbulence 13(4):1-28, 2012:
"Tests have shown that the resolved dissipation, extracted explicitly from the simulation, is of order 50-70%
of the balance, thus indicating a good resolution even beyond the wavenumber separating inertial and highly dissipative
ranges."

So each budget reads as follows:

  0 = P_ij + T_ij + phi_ij
      +Dp_ij + D_ij
      -eps_ij - C_ij

  0 = +production     +turb_transport +pres_strain
      +pres_diffusion +visc_diffusion
      -dissipation    -convection

Note that there may be round-off errors.
