      open(4,file='cf_plate.dat',form='formatted',status='new')
      open(2,file='cf_cfl3d.dat',form='formatted',status='old')
      read(2,*)
      write(4,'(''variables="x","cf"'')')
      write(4,'(''zone, t="CFL3D"'')')
      do n=1,10000
        read(2,*,end=100) x,cf
        write(4,*) x,cf
      enddo
 100  continue
      open(3,file='cf_fun3d.dat',form='formatted',status='old')
      read(3,*)
      write(4,'(''zone, t="FUN3D"'')')
      do n=1,10000
        read(3,*,end=200) x,z,cp,cf,yplus
        write(4,*) x,cf
      enddo
 200  continue
      stop
      end
