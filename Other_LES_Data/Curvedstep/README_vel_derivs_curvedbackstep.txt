Addendum to LES data for 2D curved backstep.

Velocity derivatives can be useful for turbulence modeling analysis and
development, so they are provided here as a convenience.

These velocity derivatives were provided by S. Lardeau of CD-ADAPCO.
Note that this data file replaces an earlier one given on this site prior to
3/12/2012 (the largest differences between the files occurred near the inlet and exit).
Please use this file in place of the old one.

Velocities are scaled by U_in, lengths by backstep height H.

The cell-centered data for the postprocessed velocity derivatives are in the file curvedbackstep_vel_derivs.dat
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

