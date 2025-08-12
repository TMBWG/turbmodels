    MODULE variables
      IMPLICIT NONE
      INTEGER, PARAMETER :: high = selected_real_kind (15, 307)
      REAL (high) :: great = 1.e30_high, tiny = 1.e-30_high, pi=3.14159265358979323846_high, tol=1.e-3_high, &
                   & CAPPA=0.41_high,CC=5.0_high
      integer, parameter::njp=1000000,nj=10000001
      contains
      SUBROUTINE RKG1(H,N,X,Y,Q)
      implicit none
      integer :: N,I,J
      real(high), dimension(1):: Y, Q, F
      real(high),dimension(4) :: a,b,c,d
      real(high):: H, X,aj,bj,cj,r1,r2
      DATA A/.5_high,.292893219_high,1.707106781_high,.16666666667_high/
      DATA B/2._high,1._high,1._high,2._high/
      DATA C/.5_high,.292893219_high,1.707106781_high,.5_high/
      DATA D/.0_high,.5_high,0._high,.5_high/
      A(2)=(2._high-sqrt(2._high))*0.5_high
      A(3)=(2._high+sqrt(2._high))*0.5_high
      A(4)=1._high/6._high
      C(2)=A(2)
      C(3)=A(3)
      DO 2 J=1,4
      AJ=A(J)
      BJ=B(J)
      CJ=C(J)
      X =X+D(J)*H
      CALL DXY(X,F)
      DO 1 I=1,N
      R1=H*F(I)
      R2=AJ*(R1-BJ*Q(I))
      Y(I)=Y(I)+R2
      Q(I)=Q(I)+R2*3.-CJ*R1
    1 CONTINUE
    2 CONTINUE
      RETURN
      END SUBROUTINE RKG1
      SUBROUTINE DXY(YP,DUDY)
      implicit none
      real(high):: yp,al,AAA=24.53848209701907_high
      real(high)::DUDY(1)
      AL=cappa*YP*(1._high-EXP(-YP/AAA))
      DUDY(1)=2._high/(SQRT(1._high+4._high*AL**2)+1._high)
      RETURN
      END SUBROUTINE DXY

     end module variables


      program BL
      use variables
      implicit none
      integer::ni,n,iii,iter,ii,is,i,j,iret
      real(high):: YY(1),Q(1),UUP(njp),yyp(njp),uuuup(nj+1),uuuP(nj+1),U(nj+1),Y(nj+1),wp(nj+1),yd(nj+1),yk(nj+1)
      real(high) :: dy,yp,ypp,cfv2,ret,ppi, cfv1, &
                 & delr,rap,ram,cf,cfo,utdui,uidut,ypdel,del,updel,delh, &
                 & up,udut,udui,ord,ord1,thet,delt,thedel,deldth, H, kappa
!
      yy(1)=0._high
      N=1
      Q(1)=0.
      YP=0.
      DY=100._high/real(njp-1,high)
      YYP(1)=0.
      UUP(1)=0.
      DO 44 I=2,njp
      CALL RKG1(DY,N,YP,YY,Q)
      YYP(I)=YP
      UUP(I)=yy(1)
   44 CONTINUE


    RET=10000._high
    iret=1
    PPI=0.55

!    do while(ret<=1.e8_high)
      CFV1=2._high/(log(ret)/0.384_high+4.127_high)**2
      CFV2=1./(17.08*(LOG10(RET))**2+25.11*LOG10(RET)+6.012)
!
      y(1)=0.
      yd(1)=0.
      yk(1)=0.
      u(1)=0.
      ITER=0
      cf=cfv2
      cfo=great
      thedel=1.
!
      do while(ABS(CF-CFO)/CF >= 1.E-6)
        CFO=CF
        UTDUI=SQRT(0.5*CF)
        YPDEL=RET/THEDEL*UTDUI
        IF(YPDEL.EQ.0.)THEN
          UPDEL=0.
        ELSE IF(YPDEL.LT.100.)THEN
          DO II=1,njp
            IF(YYP(II).GT.YPDEL)exit
          ENDDO
          DEL=YYP(II)-YYP(II-1)
          RAP=(YPDEL-YYP(II-1))/DEL
          RAM=(YYP(II)-YPDEL)/DEL
          UPDEL=UUP(II)*RAP+UUP(II-1)*RAM+PPI*2./CAPPA
        ELSE
          UPDEL=1./CAPPA*LOG(YPDEL)+CC+PPI*2./CAPPA
        ENDIF
        UIDUT=UPDEL
        CF=1./(0.5*UIDUT**2)
        IS=1
        NI=min(1000000,MIN(INT(YPDEL/0.1)/2*2,nj)+1)
        DELH=1._high/real(NI-1,high)
        DO J=2,NI
          YP=real(J-1,high)*DELH
          YPP=YP*YPDEL
          IF(YPP.EQ.0.)THEN
            UP=0.
          ELSE IF(YPP.LT.100.)THEN
            DO II=1,njp
              IF(YYP(II).GT.YPP)exit
            ENDDO
            DEL=YYP(II)-YYP(II-1)
            RAP=(YPP-YYP(II-1))/DEL
            RAM=(YYP(II)-YPP)/DEL
!            UP=UUP(II)*RAP+UUP(II-1)*RAM+PPI/CAPPA*2.*YP**2*(3.-2.*YP)
!            UP=UUP(II)*RAP+UUP(II-1)*RAM+PPI/CAPPA*2.*YP**2*(3.-2.*YP)+1./CAPPA*YP**2*(1.-YP)
!            UP=UUP(II)*RAP+UUP(II-1)*RAM+PPI/CAPPA*2.*sin(0.5*pi*yp)**2+1./CAPPA*YP**2*(1.-YP)
            UP=UUP(II)*RAP+UUP(II-1)*RAM+PPI/CAPPA*2.*sin(0.5*pi*yp)**2
          ELSE
!            UP=1./CAPPA*LOG(YPP)+CC+PPI/CAPPA*2.*YP**2*(3.-2.*YP)
!            UP=1./CAPPA*LOG(YPP)+CC+PPI/CAPPA*2.*sin(0.5*pi*yp)**2+1./CAPPA*YP**2*(1.-YP)
            UP=1./CAPPA*LOG(YPP)+CC+PPI/CAPPA*2.*sin(0.5*pi*yp)**2
          ENDIF
          y(j)=ypp
          u(j)=1./CAPPA*LOG(YPP)+CC+PPI/CAPPA*2.*sin(0.5*pi*yp)**2
          uuup(j)=1./CAPPA*LOG(YPP)+CC
          uuuup(j)=UP
          UDUT=UP
          UDUI=UDUT/UIDUT
          IF(UDUI.GT.1.)THEN
            UDUI=1.
          ENDIF
          IF(J.EQ.2)THET=0._high
          IF(J.EQ.2)DELT=1.-high
          ORD=UDUI*(1.-UDUI)
          ORD1=1.-UDUI
          IF(J.EQ.NI)THEN
            THET=THET+ORD
            DELT=DELT+ORD1
          ELSE
            THET=THET+real(3.+IS,high)*ORD
            DELT=DELT+real(3.+IS,high)*ORD1
            IS=-IS
          ENDIF
        ENDDO
        THET=THET*DELH/3._high
        DELT=DELT*DELH/3._high
        THEDEL=THET
        DELDTH=DELT/THEDEL
        ITER=ITER+1
      enddo
      del=delt*uidut
      kappa=thet*uidut
      do j=2,ni
        wp(j)=uidut-uuuup(j)
        yd(j)=y(j)/ypdel/del
        yk(j)=y(j)/ypdel/kappa
      enddo
      H=deldth
     WRITE(*,11)ITER,RET,CF, CFV1, CFv2,H, ppi,NI
  11 FORMAT(I4,6E11.3,I8)
     if(iret==1)then
       open(1,file='u.dat',position='rewind')
!       open(2,file='w.dat',position='rewind')
     else
       open(1,file='u.dat',position='append')
!       open(2,file='w.dat',position='append')
     endif
     write(1,*)'zone'
!     write(2,*)'zone'
     do j=2,ni
       write(1,*)y(j),u(j),uuup(j), uuuup(j)
!       write(2,*)y(j)/ypdel,yd(j),yk(j),wp(j)
     enddo
   ret=ret*10.
   iret=iret+1
!   enddo
   stop
   END
