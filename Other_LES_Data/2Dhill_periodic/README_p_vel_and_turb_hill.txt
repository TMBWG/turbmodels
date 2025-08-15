The cell-centered data for the pressure, velocities, and turbulence quantites are in the file hill_LES_avgresults.dat
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
      read(2,*) ((p(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((u(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((v(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((w(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((xnu_t(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((uu(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((vv(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((ww(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((uv(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((uw(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((vw(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((zk(i,j),i=1,idim),j=1,jdim)

Variables:
"x" = x cell-center location (nondimensionalized so that hill height=1)
"y" = y cell-center location (nondimensionalized so that hill height=1)
"p" = normalized pressure
"u" = u-velocity, normalized by Ub
"v" = v-velocity, normalized by Ub
"w" = w-velocity, normalized by Ub
"xnu_t" = unresolved turbulent viscosity nu_t (for SGS model), normalized by reference viscosity nu
"uu" = u'u', normalized by Ub^2
"vv" = v'v', normalized by Ub^2
"ww" = w'w', normalized by Ub^2
"uv" = u'v', normalized by Ub^2
"uw" = u'w', normalized by Ub^2
"vw" = v'w', normalized by Ub^2
"zk" = turbulent kinetic energy k, normalized by Ub^2 (=(uu+vv+ww)/2)

Note that the data were originally provided in single precision, so there may be
round-off errors.
