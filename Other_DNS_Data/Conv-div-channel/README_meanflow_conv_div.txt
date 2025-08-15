The data for the mean flow, velocity derivatives, and Reynolds stresses are in the file conv-div-mean.dat
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
      read(2,*) ((mean_u(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((mean_v(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((mean_w(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((dx_mean_u(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((dx_mean_v(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((dx_mean_w(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((dy_mean_u(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((dy_mean_v(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((dy_mean_w(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((dz_mean_u(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((dz_mean_v(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((dz_mean_w(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((reynolds_stress_uu(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((reynolds_stress_uv(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((reynolds_stress_uw(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((reynolds_stress_vv(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((reynolds_stress_vw(i,j),i=1,idim),j=1,jdim)
      read(2,*) ((reynolds_stress_ww(i,j),i=1,idim),j=1,jdim)

Variables:
"x" = x location
"y" = y location
"mean_u" = u-velocity
"mean_v" = v-velocity
"mean_w" = w-velocity
"dx_mean_u" = derivative of mean u-velocity wrt x
"dx_mean_v" = derivative of mean v-velocity wrt x
"dx_mean_w" = derivative of mean w-velocity wrt x
"dy_mean_u" = derivative of mean u-velocity wrt y
"dy_mean_v" = derivative of mean v-velocity wrt y
"dy_mean_w" = derivative of mean w-velocity wrt y
"dz_mean_u" = derivative of mean u-velocity wrt z
"dz_mean_v" = derivative of mean v-velocity wrt z
"dz_mean_w" = derivative of mean w-velocity wrt z
"reynolds_stress_uu" = u'u'
"reynolds_stress_uv" = u'v'
"reynolds_stress_uw" = u'w'
"reynolds_stress_vv" = v'v'
"reynolds_stress_vw" = v'w'
"reynolds_stress_ww" = w'w'

Note that there may be round-off errors.

