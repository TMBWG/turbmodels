      program get_other_cf
c
      open(21,file='cf_other.dat',form='formatted',status='unknown')
      write(21,'(''variables="retheta","cf1","cf2","cf3"'',
     +  '',"cf4","cf5","cf6","cf7"'')')
      do m=200,20000,200
        retheta=float(m)
        rex=143.1079*(retheta**(7./6.))
        xlogt=log10(retheta)
        xlogx=log10(rex)
c       White 6-112a
        cf1=0.0592*rex**(-.2)
c       White 6-116
        cf2=0.012*retheta**(-1./6.)
c       White 6-121
        cf3=0.026*rex**(-1./7.)
c       White 6-134
        cf4=0.455/((log(.06*rex))**2)
c       White 6-135a
        cf5=(2.*xlogx-0.65)**(-2.3)
c       White 6-135c
        cf6=0.37/(xlogx**2.584)
c       White 6-135b
c       initial guess:
        cf7=0.003
        do ii=1,100
          f=4.15*log10(rex*cf7) - cf7**(-.5)+1.7
          fprime=4.15*log10(2.7182818)/cf7 + 0.5*cf7**(-1.5)
          error=f/fprime
          cf7=cf7-error
          if (abs(error) .lt. 1.e-6) then
            goto 100
          end if
        enddo
        write(6,'('' cf7 NOT converged!'')')
 100    continue
        write(21,'(f20.3,7f20.8)') retheta,cf1,cf2,cf3,cf4,cf5,cf6,cf7
      enddo
      write(6,'('' output to cf_other.dat'')')
      stop
      end
