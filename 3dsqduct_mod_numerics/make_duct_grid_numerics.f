      program make_duct_grid_numerics
c
c     Hardwired to create PLOT3D grids of sizes 
c     673x193x193
c     337x97x97
c     169x49x49
c     85x25x25
c     COMPILE AND RUN USING DOUBLE PRECISION!
c
      dimension xx(193)
      dimension x(673,193,193),y(673,193,193),z(673,193,193)
c
      deltas=8.5e-6
      str=1.041459716831
      write(6,'('' stretching factor='',f21.12)') str
      xx(1)=0.
      delta=deltas
      do i=2,193
        xx(i)=xx(i-1)+delta
        delta=delta*str
      enddo
      write(6,'('' xx(193)='',f20.10)') xx(193)
      write(6,'('' final delta='',f15.5)') delta/str
c
      deltax=52./656.
      write(6,'('' deltax fine grid='',f21.12)') deltax
      write(6,'('' deltax half grid='',f21.12)') deltax*2.
      write(6,'('' deltax quarter grid='',f21.12)') deltax*4.
      write(6,'('' deltax eighth grid='',f21.12)') deltax*8.
      do j=1,193
      do k=1,193
        x(1,j,k)=-deltax*16.
        do i=2,673
          x(i,j,k)=x(i-1,j,k)+deltax
        enddo
      enddo
      enddo
c
      do i=1,673
        do j=1,193
          do k=1,193
            y(i,j,k)=xx(j)
            z(i,j,k)=xx(k)
          enddo
        enddo
      enddo
c
c   Write 3D grids here (PLOT3D unformatted):
c
      open(2,file='ductmod.p3dbin',form='unformatted',
     +     status='unknown')
      nbl=1
      ni=673
      nj=193
      nk=193
      write(2) nbl
      write(2) ni,nj,nk
      write(2) (((x(i,j,k),i=1,ni),j=1,nj),k=1,nk),
     +         (((y(i,j,k),i=1,ni),j=1,nj),k=1,nk),
     +         (((z(i,j,k),i=1,ni),j=1,nj),k=1,nk)
      open(12,file='ductmod_half.p3dbin',form='unformatted',
     +     status='unknown')
      nbl=1
      nihalf=(ni+1)/2
      njhalf=(nj+1)/2
      nkhalf=(nk+1)/2
      write(12) nbl
      write(12) nihalf,njhalf,nkhalf
      write(12) (((x(i,j,k),i=1,ni,2),j=1,nj,2),k=1,nk,2),
     +          (((y(i,j,k),i=1,ni,2),j=1,nj,2),k=1,nk,2),
     +          (((z(i,j,k),i=1,ni,2),j=1,nj,2),k=1,nk,2)
      open(22,file='ductmod_quarter.p3dbin',form='unformatted',
     +     status='unknown')
      nbl=1
      niquarter=(nihalf+1)/2
      njquarter=(njhalf+1)/2
      nkquarter=(nkhalf+1)/2
      write(22) nbl
      write(22) niquarter,njquarter,nkquarter
      write(22) (((x(i,j,k),i=1,ni,4),j=1,nj,4),k=1,nk,4),
     +          (((y(i,j,k),i=1,ni,4),j=1,nj,4),k=1,nk,4),
     +          (((z(i,j,k),i=1,ni,4),j=1,nj,4),k=1,nk,4)
      open(32,file='ductmod_eighth.p3dbin',form='unformatted',
     +     status='unknown')
      nbl=1
      nieighth=(niquarter+1)/2
      njeighth=(njquarter+1)/2
      nkeighth=(nkquarter+1)/2
      write(32) nbl
      write(32) nieighth,njeighth,nkeighth
      write(32) (((x(i,j,k),i=1,ni,8),j=1,nj,8),k=1,nk,8),
     +          (((y(i,j,k),i=1,ni,8),j=1,nj,8),k=1,nk,8),
     +          (((z(i,j,k),i=1,ni,8),j=1,nj,8),k=1,nk,8)
c
      write(6,'('' new 3D grids to ductmod.p3dbin,'')')
      write(6,'('' ductmod_half.p3dbin,'')')
      write(6,'('' ductmod_quarter.p3dbin,'')')
      write(6,'('' ductmod_eighth.p3dbin'')')
c
c   Write 2D x=const plane grids here (PLOT3D formatted)
c   y and z only (x value not included):
c
      open(3,file='ductmod.p2dfmt',form='formatted',
     +     status='unknown')
      write(3,*) nbl
      write(3,*) nj,nk
      write(3,*) (((y(i,j,k),i=1,1),j=1,nj),k=1,nk),
     +           (((z(i,j,k),i=1,1),j=1,nj),k=1,nk)
      open(13,file='ductmod_half.p2dfmt',form='formatted',
     +     status='unknown')
      write(13,*) nbl
      write(13,*) njhalf,nkhalf
      write(13,*) (((y(i,j,k),i=1,1),j=1,nj,2),k=1,nk,2),
     +            (((z(i,j,k),i=1,1),j=1,nj,2),k=1,nk,2)
      open(23,file='ductmod_quarter.p2dfmt',form='formatted',
     +     status='unknown')
      write(23,*) nbl
      write(23,*) njquarter,nkquarter
      write(23,*) (((y(i,j,k),i=1,1),j=1,nj,4),k=1,nk,4),
     +            (((z(i,j,k),i=1,1),j=1,nj,4),k=1,nk,4)
      open(33,file='ductmod_eighth.p2dfmt',form='formatted',
     +     status='unknown')
      write(33,*) nbl
      write(33,*) njeighth,nkeighth
      write(33,*) (((y(i,j,k),i=1,1),j=1,nj,8),k=1,nk,8),
     +            (((z(i,j,k),i=1,1),j=1,nj,8),k=1,nk,8)
c
      write(6,'('' new 2D formatted grid to ductmod.p2dfmt,'')')
      write(6,'('' ductmod_half.p2dfmt,'')')
      write(6,'('' ductmod_quarter.p2dfmt,'')')
      write(6,'('' ductmod_eighth.p2dfmt'')')
      stop
      end
