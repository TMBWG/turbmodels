      program make_duct_grid3
c
c     Hardwired to create PLOT3D grids of sizes 
c     961x161x161
c     481x81x81
c     241x41x41
c     COMPILE AND RUN USING DOUBLE PRECISION!
c
      dimension xx(161)
      dimension x(961,161,161),y(961,161,161),z(961,161,161)
c
      deltas=8.5e-6
      str=1.051361524032
      write(6,'('' stretching factor='',f21.12)') str
      xx(1)=0.
      delta=deltas
      do i=2,161
        xx(i)=xx(i-1)+delta
        delta=delta*str
      enddo
      write(6,'('' xx(161)='',f20.10)') xx(161)
      write(6,'('' final delta='',f15.5)') delta/str
c
      deltax=52./960.
      write(6,'('' deltax='',f21.12)') deltax
      do j=1,161
      do k=1,161
        x(1,j,k)=0.
        do i=2,961
          x(i,j,k)=x(i-1,j,k)+deltax
        enddo
      enddo
      enddo
c
      do i=1,961
        do j=1,161
          do k=1,161
            y(i,j,k)=xx(j)
            z(i,j,k)=xx(k)
          enddo
        enddo
      enddo
c
c   Write 3D grids here (PLOT3D unformatted):
c
      open(2,file='duct.p3dbin',form='unformatted',
     +     status='unknown')
      nbl=1
      ni=961
      nj=161
      nk=161
      write(2) nbl
      write(2) ni,nj,nk
      write(2) (((x(i,j,k),i=1,ni),j=1,nj),k=1,nk),
     +         (((y(i,j,k),i=1,ni),j=1,nj),k=1,nk),
     +         (((z(i,j,k),i=1,ni),j=1,nj),k=1,nk)
      open(12,file='duct_half.p3dbin',form='unformatted',
     +     status='unknown')
      nbl=1
      nihalf=481
      njhalf=81
      nkhalf=81
      write(12) nbl
      write(12) nihalf,njhalf,nkhalf
      write(12) (((x(i,j,k),i=1,ni,2),j=1,nj,2),k=1,nk,2),
     +          (((y(i,j,k),i=1,ni,2),j=1,nj,2),k=1,nk,2),
     +          (((z(i,j,k),i=1,ni,2),j=1,nj,2),k=1,nk,2)
      open(22,file='duct_quarter.p3dbin',form='unformatted',
     +     status='unknown')
      nbl=1
      niquarter=241
      njquarter=41
      nkquarter=41
      write(22) nbl
      write(22) niquarter,njquarter,nkquarter
      write(22) (((x(i,j,k),i=1,ni,4),j=1,nj,4),k=1,nk,4),
     +          (((y(i,j,k),i=1,ni,4),j=1,nj,4),k=1,nk,4),
     +          (((z(i,j,k),i=1,ni,4),j=1,nj,4),k=1,nk,4)
c
      write(6,'('' new 3D grids to duct.p3dbin, duct_half.p3dbin,'')')
      write(6,'('' duct_quarter.p3dbin'')')
c
c   Write 2D x=const plane grids here (PLOT3D formatted)
c   y and z only (x value not included):
c
      open(3,file='duct.p2dfmt',form='formatted',
     +     status='unknown')
      write(3,*) nbl
      write(3,*) nj,nk
      write(3,*) (((y(i,j,k),i=1,1),j=1,nj),k=1,nk),
     +           (((z(i,j,k),i=1,1),j=1,nj),k=1,nk)
      open(13,file='duct_half.p2dfmt',form='formatted',
     +     status='unknown')
      write(13,*) nbl
      write(13,*) njhalf,nkhalf
      write(13,*) (((y(i,j,k),i=1,1),j=1,nj,2),k=1,nk,2),
     +            (((z(i,j,k),i=1,1),j=1,nj,2),k=1,nk,2)
      open(23,file='duct_quarter.p2dfmt',form='formatted',
     +     status='unknown')
      write(23,*) nbl
      write(23,*) njquarter,nkquarter
      write(23,*) (((y(i,j,k),i=1,1),j=1,nj,4),k=1,nk,4),
     +            (((z(i,j,k),i=1,1),j=1,nj,4),k=1,nk,4)
c
      write(6,'('' new 2D formatted grid to duct.p2dfmt,'')')
      write(6,'('' duct_half.p2dfmt, duct_quarter.p2dfmt'')')
      stop
      end
