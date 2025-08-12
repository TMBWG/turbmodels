      program upyp_other
c
      open(2,file='u+y+other.dat',form='formatted',status='unknown')
      write(2,'(''variables="log(y+)","u+"'')')
      write(2,'(''zone, t="log layer 1"'')')
      do m=30,1000,10
        yp=float(m)
        zlog=log10(yp)
        up=log(yp)/0.40+5.5
        write(2,'(2e17.7)') zlog,up
      enddo
      write(2,'(''zone, t="log layer 2"'')')
      do m=30,1000,10
        yp=float(m)
        zlog=log10(yp)
        up=log(yp)/0.41+5.0
        write(2,'(2e17.7)') zlog,up
      enddo
      write(2,'(''zone, t="Spalding 1"'')')
      do m=1,23
        up=float(m)
        zkap=0.4
        b=5.5
        yp=up+exp(-zkap*b)*(exp(zkap*up) - 1. - zkap*up -
     +    0.5*(zkap*up)**2 - (1./6.)*(zkap*up)**3)
        zlog=log10(yp)
        write(2,'(2e17.7)') zlog,up
      enddo
      write(2,'(''zone, t="Spalding 2"'')')
      do m=1,23
        up=float(m)
        zkap=0.41
        b=5.0
        yp=up+exp(-zkap*b)*(exp(zkap*up) - 1. - zkap*up -
     +    0.5*(zkap*up)**2 - (1./6.)*(zkap*up)**3)
        zlog=log10(yp)
        write(2,'(2e17.7)') zlog,up
      enddo
      write(6,'('' output to u+y+other.dat'')')
      stop
      end
