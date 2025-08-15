The grid points (in the x-y plane) from the LES are given in the file curvedbackstep_grid_n.dat
in blocked "Tecplot-format" (Tecplot360 version).  
For example, for the particular zone with:

 I=769, J=161, K=1, ZONETYPE=Ordered
 DATAPACKING=BLOCK

The data from this zone should be read from the files (ignoring title, zone,
variables, and other header lines) via:

      idim=769
      jdim=161
      read(2,*) ((x(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((y(i,j),i=1,idim),j=1,jdim)

Variables:
"x" = x location (nondimensionalized so that backstep height=1)
"y" = y location (nondimensionalized so that backstep height=1)

Note that there may be round-off errors.
