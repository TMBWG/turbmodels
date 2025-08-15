************************************************************************************
NOTE: The budget "dissipation" terms given in the data file (<dui/dxk*duj/dxi>)
are only *half* of the intended value that should appear in the Reynolds
stress balance (eps_ij=2*<dui/dxk*duj/dxi>).
************************************************************************************

The data for the budgets are in the file conv-div-budgets.dat
in blocked "Tecplot-format" (Tecplot360 version).  
For example, for the particular zone with:

 I=385, J=2304, K=1, ZONETYPE=Ordered
 DATAPACKING=BLOCK

the data from this zone should be read from the files (ignoring title, zone,
variables, and other header lines) via:

      idim=385
      jdim=2304
      read(2,*) ((x(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((y(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((dissipation_uu(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((dissipation_uv(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((dissipation_uw(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((dissipation_vv(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((dissipation_vw(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((dissipation_ww(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((pressure_strain_uu(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((pressure_strain_uv(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((pressure_strain_uw(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((pressure_strain_vv(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((pressure_strain_vw(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((pressure_strain_ww(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((pressure_diffusion_uu(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((pressure_diffusion_uv(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((pressure_diffusion_uw(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((pressure_diffusion_vv(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((pressure_diffusion_vw(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((pressure_diffusion_ww(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((production_uu(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((production_uv(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((production_uw(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((production_vv(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((production_vw(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((production_ww(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((convective_terms_uu(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((convective_terms_uv(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((convective_terms_uw(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((convective_terms_vv(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((convective_terms_vw(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((convective_terms_ww(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((viscous_diffusion_uu(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((viscous_diffusion_uv(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((viscous_diffusion_uw(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((viscous_diffusion_vv(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((viscous_diffusion_vw(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((viscous_diffusion_ww(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((turbulent_transport_uu(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((turbulent_transport_uv(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((turbulent_transport_uw(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((turbulent_transport_vv(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((turbulent_transport_vw(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((turbulent_transport_ww(i,j),i=1,idim),j=1,jdim)

Variables:
"x" = x location
"y" = y location
"dissipation_XX" = HALF of dissipation term for quantity XX (=<dui/dxk*duj/dxi>) <- note "nu" is not included here (nu=1/12600)
"pressure_strain_XX" = pressure strain term for quantity XX (=<p/rho*(dui/dxj+duj/dxi)>)
"pressure_diffusion_XX" = pressure diffusion term for quantity XX (=-1/rho*(d<uj*p>/dxi+d<ui*p>/dxj))
"production_XX" = production term for quantity XX (=-<ui*uk>duj/dxk-<uj*uk>dui/dxk)
"convective_terms_XX" = convection term for quantity XX (=<uk>d<uiuj>/Dxk)
"viscous_diffusion_XX" = viscous diffusion term for quantity XX (=d^2<ui*uj>/(dxk*dxk)) <- note "nu" is not included here (nu=1/12600)
"turbulent_transport_XX" = turbulent transport (triple moment) term for quantity XX (=-d<ui*uj*uk>/dxk)

Each budget reads as follows:

  0 = P_ij + T_ij + phi_ij
      +Dp_ij + D_ij
      -eps_ij - C_ij

  0 = +production_XX         +turbulent_transport_XX   +pressure_strain_XX
      +pressure_diffusion_XX +viscous_diffusion_XX*nu
      -2*dissipation_XX*nu   -convective_terms_XX

  where nu=1/12600

Note that there may be round-off errors.

