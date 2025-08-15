The cell-centered data for the pressure, velocities, and turbulence quantites are in the file curvedbackstep_vel_stress.dat
in blocked "Tecplot-format" (Tecplot360 version).  
For example, for the particular zone with:

 I=768, J=160, K=1, ZONETYPE=Ordered
 DATAPACKING=BLOCK

the data from this zone should be read from the files (ignoring title, zone,
variables, and other header lines) via:

      idim=768
      jdim=160
      read(2,*) ((x(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((y(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((p(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((u(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((v(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((w(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((uu(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((vv(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((ww(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((uv(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((uw(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((vw(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((zk(i,j),i=1,idim),j=1,jdim)

Variables:
"x" = x cell-center location (nondimensionalized so that backstep height=1)
"y" = y cell-center location (nondimensionalized so that backstep height=1)
"p" = normalized pressure
"u" = u-velocity, normalized by U_in
"v" = v-velocity, normalized by U_in
"w" = w-velocity, normalized by U_in
"uu" = u'u', normalized by U_in^2
"vv" = v'v', normalized by U_in^2
"ww" = w'w', normalized by U_in^2
"uv" = u'v', normalized by U_in^2
"uw" = u'w', normalized by U_in^2
"vw" = v'w', normalized by U_in^2
"zk" = turbulent kinetic energy k, normalized by U_in^2 (=(uu+vv+ww)/2)

Note that there may be round-off errors.
