Unofficial addendum to LES data for 2D hill.

Velocity derivatives can be useful for turbulence modeling analysis and
development, so they are provided here as a convenience.

These velocity derivatives were NOT given in the original data:
they were derived after-the-fact via (mostly) 2nd-order central differencing, using grid metrics.
Velocities are scaled by Ub, lengths by hill height h.
Note that the velocity data were originally provided in single precision, so there may be round-off errors
in addition to errors from the central differencing during the postprocessing step.

No guarantees: use at your own risk!!

The cell-centered data for the postprocessed velocity derivatives are in the files hill_LES_vel_derivs-derived.dat
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
      read(2,*) ((dudx(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((dudy(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((dvdx(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((dvdy(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((dwdx(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((dwdy(i,j),i=1,idim),j=1,jdim)

Variables:
"x" = x cell-center location (nondimensionalized so that hill height=1)
"y" = y cell-center location (nondimensionalized so that hill height=1)
"dudx" = derivative of u wrt x
"dudy" = derivative of u wrt y
"dvdx" = derivative of v wrt x
"dvdy" = derivative of v wrt y
"dwdx" = derivative of w wrt x
"dwdy" = derivative of w wrt y

