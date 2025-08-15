The grid points (in the x-y plane) from the LES are given in the file hill_grid.dat
in blocked "Tecplot-format" (Tecplot360 version).  
For example, for the particular zone with:

 I=197, J=129, K=1, ZONETYPE=Ordered
 DATAPACKING=BLOCK

the data from this zone should be read from the files (ignoring title, zone,
variables, and other header lines) via:

      idim=197
      jdim=129
      read(2,*) ((x(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((y(i,j),i=1,idim),j=1,jdim)

Variables:
"x" = x location (nondimensionalized so that hill height=1)
"y" = y location (nondimensionalized so that hill height=1)

Note that the data were originally provided in single precision, so there may be
round-off errors.
