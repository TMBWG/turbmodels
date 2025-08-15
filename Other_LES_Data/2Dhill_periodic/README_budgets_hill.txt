The cell-centered data for the budget quantites are in the files hill_LES_budgets_all.dat
in blocked "Tecplot-format" (Tecplot360 version).  
For example, for the particular zone with:

 I=196, J=128, K=1, ZONETYPE=Ordered
 DATAPACKING=BLOCK

the data from this zone should be read from the files (ignoring title, zone,
variables, and other header lines) via:

      idim=196
      jdim=128
      read(2,*) ((x(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((y(i,j),i=1,idim),j=1,jdim)
      read(2,*)     ((production_k(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((turb_transport_k(i,j),i=1,idim),j=1,jdim)
      read(2,*)    ((pres_strain_k(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((pres_diffusion_k(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((visc_diffusion_k(i,j),i=1,idim),j=1,jdim)
      read(2,*)    ((dissipation_k(i,j),i=1,idim),j=1,jdim)
      read(2,*)     ((convection_k(i,j),i=1,idim),j=1,jdim)
      read(2,*)        ((sgsterm_k(i,j),i=1,idim),j=1,jdim)
      read(2,*)     ((production_uu(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((turb_transport_uu(i,j),i=1,idim),j=1,jdim)
      read(2,*)    ((pres_strain_uu(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((pres_diffusion_uu(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((visc_diffusion_uu(i,j),i=1,idim),j=1,jdim)
      read(2,*)    ((dissipation_uu(i,j),i=1,idim),j=1,jdim)
      read(2,*)     ((convection_uu(i,j),i=1,idim),j=1,jdim)
      read(2,*)        ((sgsterm_uu(i,j),i=1,idim),j=1,jdim)
      read(2,*)     ((production_uv(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((turb_transport_uv(i,j),i=1,idim),j=1,jdim)
      read(2,*)    ((pres_strain_uv(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((pres_diffusion_uv(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((visc_diffusion_uv(i,j),i=1,idim),j=1,jdim)
      read(2,*)    ((dissipation_uv(i,j),i=1,idim),j=1,jdim)
      read(2,*)     ((convection_uv(i,j),i=1,idim),j=1,jdim)
      read(2,*)        ((sgsterm_uv(i,j),i=1,idim),j=1,jdim)
      read(2,*)     ((production_uw(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((turb_transport_uw(i,j),i=1,idim),j=1,jdim)
      read(2,*)    ((pres_strain_uw(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((pres_diffusion_uw(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((visc_diffusion_uw(i,j),i=1,idim),j=1,jdim)
      read(2,*)    ((dissipation_uw(i,j),i=1,idim),j=1,jdim)
      read(2,*)     ((convection_uw(i,j),i=1,idim),j=1,jdim)
      read(2,*)        ((sgsterm_uw(i,j),i=1,idim),j=1,jdim)
      read(2,*)     ((production_vv(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((turb_transport_vv(i,j),i=1,idim),j=1,jdim)
      read(2,*)    ((pres_strain_vv(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((pres_diffusion_vv(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((visc_diffusion_vv(i,j),i=1,idim),j=1,jdim)
      read(2,*)    ((dissipation_vv(i,j),i=1,idim),j=1,jdim)
      read(2,*)     ((convection_vv(i,j),i=1,idim),j=1,jdim)
      read(2,*)        ((sgsterm_vv(i,j),i=1,idim),j=1,jdim)
      read(2,*)     ((production_vw(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((turb_transport_vw(i,j),i=1,idim),j=1,jdim)
      read(2,*)    ((pres_strain_vw(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((pres_diffusion_vw(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((visc_diffusion_vw(i,j),i=1,idim),j=1,jdim)
      read(2,*)    ((dissipation_vw(i,j),i=1,idim),j=1,jdim)
      read(2,*)     ((convection_vw(i,j),i=1,idim),j=1,jdim)
      read(2,*)        ((sgsterm_vw(i,j),i=1,idim),j=1,jdim)
      read(2,*)     ((production_ww(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((turb_transport_ww(i,j),i=1,idim),j=1,jdim)
      read(2,*)    ((pres_strain_ww(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((pres_diffusion_ww(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((visc_diffusion_ww(i,j),i=1,idim),j=1,jdim)
      read(2,*)    ((dissipation_ww(i,j),i=1,idim),j=1,jdim)
      read(2,*)     ((convection_ww(i,j),i=1,idim),j=1,jdim)
      read(2,*)        ((sgsterm_ww(i,j),i=1,idim),j=1,jdim)

Variables:
"x" = x cell-center location (nondimensionalized so that hill height=1)
"y" = y cell-center location (nondimensionalized so that hill height=1)
"production_XX" = production term for quantity XX (=-<ui*uk>duj/dxk-<uj*uk>dui/dxk for Re stress terms)
"turb_transport_XX" = turbulent transport (triple moment) term for quantity XX (=-d<ui*uj*uk>/dxk for Re stress terms)
"pres_strain_XX" = pressure strain term for quantity XX (=<p/rho*(dui/dxj+duj/dxi)> for Re stress terms)
"pres_diffusion_XX" = pressure diffusion term for quantity XX (=-1/rho*(d<uj*p>/dxi+d<ui*p>/dxj) for Re stress terms)
"visc_diffusion_XX" = viscous diffusion term for quantity XX (=nu*d^2<ui*uj>/(dxk*dxk) for Re stress terms)
"dissipation_XX" = dissipation term for quantity XX (=-2*nu*<dui/dxk*duj/dxi> for Re stress terms)
"convection_XX" = convection term for quantity XX (=-<uk>d<uiuj>/Dxk for Re stress terms)
"sgsterm_XX" = SGS (viscous diffusion) term for quantity XX

Note 1: Dissipation (eps_ij) and convection (C_ij) terms given in the files are negative
of traditional definitions. 

Note 2: Dissipation (eps_ij) was NOT computed, it was determined from the
other terms to make the balance sum to zero.  As noted in JFM 526:19-66, 2005:
"An explicit evaluation of eps_ij yielded approximately 50-70% of the value obtained
from the balance, a level regarded as reasonable in view of the fact that the ratio
of grid distance to Kolmogorov length was of order 10."

So each budget reads as follows:

  0 = P_ij + T_ij + phi_ij
      +Dp_ij + D_ij
      +eps_ij + C_ij

  0 = +production     +turb_transport +pres_strain
      +pres_diffusion +visc_diffusion
      +dissipation    +convection

Note that the data were originally provided in single precision, so there may be
round-off errors.
