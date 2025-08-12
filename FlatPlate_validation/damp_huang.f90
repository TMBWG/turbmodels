    MODULE variables
      IMPLICIT NONE
      INTEGER, PARAMETER :: high = selected_real_kind (15, 307)
      REAL (high) :: great = 1.e30_high, tiny = 1.e-30_high, pi=3.14159265358979323846_high, tol=1.e-3_high, AAA=25.5324_high, &
                   & CAPPA=0.41_high, CC=5.0_high
      integer, parameter::njp=1000,nj=1001
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
      real(high):: yp,al
      real(high)::DUDY(1)
      AL=cappa*YP*(1._high-EXP(-YP/AAA))
      DUDY(1)=2._high/(SQRT(1._high+4._high*AL**2)+1._high)
      RETURN
      END SUBROUTINE DXY

     end module variables


      program BL
      use variables
      implicit none
      integer::ni,n,i
      real(high):: YY(1),Q(1)
      real(high) :: dy,yp, AA0, AA1, UPP, upp0, upp1, AANEW
      NI=1000000
      DY=100._high/real(ni,high)
      UPP=1./CAPPA*LOG(DY*real(NI,high))+CC

      AA0=AAA-0.9_high
      AA1=AAA-1._high

      AAA=AA0
      yy(1)=0._high
      N=1
      Q(1)=0._high
      YP=0._high
      DO I=1,NI
        CALL RKG1(DY,N,YP,YY,Q)
      ENDDO
      UPP0=yy(1)
      print *,yp,upp0,upp

      do while (abs(AAA-AA1)/AAA>1.e-20_high)
        AAA=AA1
        yy(1)=0._high
        N=1
        Q(1)=0._high
        YP=0._high
        DO I=1,NI
          CALL RKG1(DY,N,YP,YY,Q)
        ENDDO
        UPP1=yy(1)
        AANEW=(UPP-UPP0)/(UPP1-UPP0)*(AA1-AA0)+AA0
        if(abs(upp1-upp)>abs(upp0-upp))then
          AA1=AANEW
        else
          upp0=upp1
          AA0=AA1
          AA1=AANEW
        endif
        print *,AAA,UPP,UPP1
      enddo


    stop
   END
