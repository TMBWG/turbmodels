      program get_cf
c
      open(21,file='cf_K-S.dat',form='formatted',status='unknown')
      write(21,'(''variables="retheta","cf"'')')
      do m=200,20000,200
        retheta=float(m)
        xlog=log10(retheta)
        cf=1./(17.08*xlog*xlog + 25.11*xlog + 6.012)
        write(21,'(f20.3,f20.8)') retheta,cf
      enddo
      write(6,'('' output to cf_K-S.dat'')')
      stop
      end
