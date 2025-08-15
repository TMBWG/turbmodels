The data files for the velocities and turbulence quantites are in blocked "Tecplot-format" (Tecplot360 version).  
For example, for the particular zone with:

 I=801, J=201, K=1, ZONETYPE=Ordered
 DATAPACKING=BLOCK

the data from this zone should be read from the files (ignoring title, zone,
variables, and other header lines) via:

      idim=801
      jdim=201
      read(2,*) ((x(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((y(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((u(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((v(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((uu(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((vv(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((ww(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((uv(i,j),i=1,idim),j=1,jdim)

