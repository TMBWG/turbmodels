      program extract
      open(2,file='u_huang.dat',form='formatted',status='old')
      open(3,file='u+_new.dat',form='formatted',status='unknown')
      read(2,*)
      read(2,*)
      do n=1,1000
        read(2,*,end=100) a1,a2,a3,a4
        write(3,'(2e25.16)') a1,a4
      enddo
 100  continue
      do n=1,100000
        read(2,*,end=200) a1,a2,a3,a4
        if (mod(n,20) .eq. 0) then
          write(3,'(2e25.16)') a1,a4
        end if
      enddo
 200  continue
      write(6,'('' output to u+_new.dat'')')
      stop
      end
