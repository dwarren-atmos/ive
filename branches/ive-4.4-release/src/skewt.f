      SUBROUTINE SKEWT (PRES,TEMP,DWPT,NN,BADVAL,ISKDRW)              
c
c     $Id: skewt.f,v 1.4 2000/05/09 19:54:03 harry Exp $
c
c     This routine plots a SKEW T diagram.
c
c     $Log: skewt.f,v $
c     Revision 1.4  2000/05/09 19:54:03  harry
c     Seperate contour and line colors.  Make patterns work like colors.
c
c     Revision 1.3  1997/09/04 21:31:20  harry
c     TEMP is real, not integer.
c
c     Revision 1.2  1997/07/02 20:08:15  warren
c     Fixes for problems found by DEC f90 compiler.
c
c     Revision 1.1  1996/04/25 16:25:31  harry
c     Add Skew-T plots and the beginning of trajectory plots.
c
C                                                                       
C     SKEWT- PLOTS SOUNDINGS ON A SKEWT, LOG P THERMODYNAMIC DIAGRAM    
C                                                                       
C     PRES- PRESSURE ARRAY FOR THERMODYNAMIC DATA (MB)                  
C     TEMP- TEMPERATURE ARRAY (CELSIUS)                                 
C     DWPT- DEW POINT ARRAY (CELSIUS)                                   
C     SPD- WIND SPEED ARRAY (M/S)                                       
C     DIR- WIND DIRECTION ARRAY (DEGREES-0 IS NORTH)                    
C     NN- NUMBER OF DATA LEVELS                                         
C     BADVAL- VALUE ASSIGNED TO MISSING DATA  --TEMP,DWPT TRACES ARE    
C                   TERMINATED AS SOON AS THIS NUMBER IS ENCOUNTERED.   
C                                                                       
C        OUTPUT PARAMETERS.....                                         
C     ALL INPUT PARAMETERS REMAIN UNCHANGED BY THIS ROUTINE.            
C                                                                       
      implicit real (a-h,o-z)
      implicit integer (i-n)
      
      DIMENSION PRES(1),TEMP(1),DWPT(1)
      integer ISKDRW, oldlw, ierr, i, nn, linecolor
      real linwdth, x, y, fx, fy, badval, pres, temp, dwpt
      logical error, first
C                                                                       
C  MAPPINGS FROM (P,T) TO CM ON SKEWT                                   
C                                                                       
      FY(P) = 132.182 - 44.061 * ALOG10(P)                              
      FX(T,Y) = 0.54 * T + 0.90692 * Y                                  
C                                                                       
C  TEST TO SEE IF A BACKGROUND HAS BEEN DRAWN, IF NOT CALL SKWTBKG      
C                                                                       
      IF (ISKDRW .EQ. 0) THEN                                           
         CALL SKWTBKG
      END IF     
C                                                                       
C  SKEWT BACKGROUND HAS BEEN GENERATED-- PLOT THE SOUNDING              
C                                                                       
      CALL SET(.095,.925,.095,.925,-19.0,27.1,-.9346217,44.061,1)           
C                                                                       
C     read in the appropriate line stuff.
C                                                                       
      call gsclip (1) 
      call gqlwsc (ierr, oldlw)
      call getivar ('hicolor', linecolor, error)
      call gsplci(linecolor)
      call getrvar ('linwdth', linwdth, error)
      linwdth= linwdth+1.
      call gslwsc (linwdth)  
c      call getivar ('hipattern', linepat, error) 
c      call dashdb (linepat)       
     

      call dashdb (65535) 
      first = .true.

      DO 60 I=1,NN
         IF(TEMP(I).EQ.BADVAL) then
            if (.not. first) call lastd
            first = .true.
         else
            Y=FY(PRES(I))                                                 
            X=FX(TEMP(I),Y)
            if (first) then
               first = .false.
               call frstd(x, y)
            else
               call vectd(x, y)
            endif
         endif
 60   CONTINUE

      call lastd
      first = .true.
      call dashdb (34952)

      DO 70 I=1,NN   
         IF(DWPT(I).EQ.BADVAL) then
            if (.not. first) call lastd
            first = .true.
         else
            Y=FY(PRES(I))                                               
            X=FX(DWPT(I),Y)                                             
            if (first) then
               first = .false.
               call frstd(x, y)
            else
               call vectd(x, y)
            endif
         endif
 70   CONTINUE                                                          

      call lastd
      call plotit(0,0,0)
      call gslwsc (oldlw)

      RETURN                                                            
      END                                                               
      SUBROUTINE SKWTBKG                                                
C     
C  SKWTBKG- PLOTS BACKGROUND FOR A SKEWT, LOG P THERMODYNAMIC DIAGRAM   
C                                                                       
      implicit real (a-h,o-z)
      implicit integer (i-n)
      COMMON /SKWDRW/ ISKDRW                                            
C     
C  ENCODE BUFFER                                                        
C                                                                       
      CHARACTER*4 ITIT                                                  
C                                                                       
C     SKEWT BORDER                                                         
C     
      REAL XB, YB, PLV, PLN, TP, Y1, T, X1, X2, Y2, TMR, TS, P, TK
      INTEGER K, ITS, I, J, ISKDRW
      DIMENSION XB(7),YB(7)                                             
      DATA XB/-19.,27.1,27.1,18.6,18.6,-19.,-19./                       
      DATA YB/-.9346217,-.9346217,9.,17.53,44.061,44.061,-.9346217/     
C     
C     PRESSURE LINE SPECS                                                  
C     
      DIMENSION PLV(11),PLN(11,2)                                       
      DATA PLV/100.,200.,300.,400.,500.,600.,700.,800.,900.,            
     &     1000.,1050./                                             
      DATA PLN/11*-19.,4*18.6,22.83,26.306,5*27.1/                      
C     
C     TEMPERATURE LINE SPECS                                               
C     
      DIMENSION TP(15,2)                                                
      DATA TP/8*1050.,855.,625.,459.,337.,247.,181.,132.,730.,580.,500.,
     &     430.,342.,251.,185.,135.,7*100./                               
C     
C     MIXING RATIO SPECS                                                   
C     
      REAL        RAT(8), ABZ, FX, FY, AOS, OS, ATSA, TSA, SX, SY, Y45
      REAL TD, YD, YPD, TX, XPD, X, Y
      CHARACTER*2 LRAT(8)                                               
      DATA RAT/20.,12.,8.,5.,3.,2.,1.,0.4/                              
      DATA LRAT/'20','12',' 8',' 5',' 3',' 2',' 1','.4'/                
C     
C     DRY/SATURATED ADAIBAT BUFFERS                                        
C     
      DIMENSION SX(162),SY(162),Y45(162)                                
C     
C     DEGREES TO RADIANS, ABSOLUTE ZERO                                    
C     
      PARAMETER (ABZ = 273.16)                                          
C     
C     MAPPINGS FROM (P,T) TO CM ON SKEWT                                   
C     
      FY(P)=132.182-44.061*ALOG10(P)                                    
      FX(T,Y)=0.54*T+0.90692*Y                                          
c     
c     set the color, and width
c 
      call gsplci (1)
      call gslwsc (1.0)
      call gsclip (0)      
C     
C     DRAW SKEWT BORDER                                                    
C     
      CALL SET(.095,.925,.095,.925,-19.0,27.1,-.9346217,44.061,1)           
      CALL CURVE(XB,YB,7)                                               
C     
C     DRAW THE PRESSURE LINES                                              
C     
      DO 10 K=1,11                                                      
         Y1=FY(PLV(K))                                                  
         IF(K.NE.1.AND.K.NE.11) CALL LINE(PLN(K,1),Y1,PLN(K,2),Y1)      
         ITS=NINT(PLV(K))                                               
         WRITE (ITIT,101) ITS                                              
 101     FORMAT(I4)                                                     
         CALL WTSTR (-19.2,Y1,ITIT,11,0,1)                                 
 10   CONTINUE                                                          
C     
C     DRAW TEMPERATURE LINES                                               
C     
      T=40.                                                             
      DO 20 I=1,15                                                      
         Y1=FY(TP(I,1))                                                 
         Y2=FY(TP(I,2))                                                 
         X1=FX(T,Y1)                                                    
         X2=FX(T,Y2)                                                    
         CALL LINE(X1,Y1,X2,Y2)                                         
         ITS=NINT(T)                                                    
         IF(ITS.EQ.20) GO TO 19                                         
         X2=X2+0.4                                                   
         Y2=Y2+0.441                                                 
         WRITE (ITIT,101) ITS                                              
         CALL WTSTR (X2,Y2,ITIT,12,47,-1)                                  
 19      T=T-10.                                                        
 20   CONTINUE                                                          
C     
C     DRAW MIXING RATIO LINES                                              
C     
      CALL DASHDB (3855)        ! PATTERN = 0000111100001111              
      Y1=FY(1050.)                                                      
      Y2=FY(700.)                                                       
      DO 30 I=1,8                                                       
         X1=FX(TMR(RAT(I),1050.)-ABZ,Y1)                                
         X2=FX(TMR(RAT(I), 700.)-ABZ,Y2)                                
         CALL LINED(X1,Y1,X2,Y2)                                        
C     CALL PWRY(X2,Y2+0.6,LRAT(I),2,10,0,1)                          
         CALL WTSTR (X2,Y2+0.6,LRAT(I),10,0,0)                             
 30   CONTINUE                                                          
C     
C     DRAW SATURATED ADIABATS                                              
C     
      CALL DASHDB (31710)       ! PATTERN = 0111101111011110              
      TS=32.                                                            
      DO 40 I=1,7                                                       
         P=1060.                                                        
         TK=TS+ABZ                                                      
         AOS=OS(TK,1000.)                                               
         DO 35 J=1,86                                                   
            P=P-10.                                                     
            ATSA=TSA(AOS,P)-ABZ                                         
            SY(J)=FY(P)                                                 
            SX(J)=FX(ATSA,SY(J))                                        
 35      CONTINUE                                                       
         CALL CURVED(SX,SY,86)                                          
         ITS=NINT(TS)                                                   
         WRITE (ITIT,102) ITS                                              
 102     FORMAT(I2)                                                     
C        CALL PWRY(SX(86),SY(86)+0.6,ITIT,2,10,0,1)                     
         CALL WTSTR (SX(86),SY(86)+0.6,ITIT(1:2),10,0,0)                   
         TS=TS-4.0                                                      
 40   CONTINUE                                                          
C     
C     DRAW DRY ADIABAT LINES                                               
C     
      CALL DASHDB (21845)       ! PATTERN = 0101010101010101              
C     CALL DASHD(4444B)                                                 
      T=51.                                                             
      DO 45 I=1,162                                                     
         Y45(I)=66.67*(5.7625544-ALOG(T+ABZ))                           
         T=T-1.0                                                        
 45   CONTINUE                                                          
      T=450.                                                            
      TD=52.                                                            
      DO 55 I=1,20                                                      
         T=T-10.                                                        
         K=0                                                            
         YD=66.67*(ALOG(T)-5.7625544)                                   
         DO 50 J=1,162                                                  
            YPD=Y45(J)+YD                                               
            TX=TD-FLOAT(J)                                              
            IF(YPD.GT.44.061) GO TO 54                                  
            IF(YPD.LT.-.9346217) GO TO 50                               
            XPD=FX(TX,YPD)                                              
            IF(XPD.LT.-19.0)GO TO 54                                    
            IF(XPD.GT.27.1)GO TO 50                                     
            IF(XPD.GT.18.6.AND.T.GT.350.0)GO TO 50                      
            K=K+1                                                    
            SX(K)=XPD                                                
            SY(K)=YPD                                                
 50      CONTINUE                                                       
C     
 54      CALL CURVED(SX,SY,K)                                           
         ITS=NINT(T)                                                    
         WRITE (ITIT,103) ITS                                              
 103     FORMAT(I3)                                                     
         X=SX(K-3)                                                      
         Y=SY(K-3)                                                      
         IF(X.LT.-15.0) X = -17.95                                      
         IF(Y.GT.40.0)  Y = 42.9                                        
         CALL WTSTR (X,Y,ITIT(1:3),10,0,0)                                 
 55   CONTINUE                                                          
C     
      ISKDRW = 1  
      call plotit(0,0,0)

      RETURN                                                            
      END                                                               
c************************************************
      REAL FUNCTION  ESAT(T)
      implicit real (a-h,o-z)
      implicit integer (i-n)                                                    
C     ESAT(MILLIBARS),T(KELVIN)                                            
      REAL ABZ, TC, T
      PARAMETER (ABZ=273.16)                                            
      TC=T-ABZ                                                          
      ESAT=6.1078*EXP((17.2693882*TC)/(TC+237.3))                       
      RETURN                                                            
      END                                                               
c************************************************
      REAL FUNCTION  OS(T, P)
      implicit real (a-h,o-z)
      implicit integer (i-n)                                                   
      REAL T, P, W
C     OS AND T (KELVIN) , P (MILLIBARS )                                   
      OS = T*((1000./P)**.286)/(EXP(-2.6518986*W(T,P)/T))               
      RETURN                                                            
      END                                                               
c************************************************
      REAL FUNCTION  TMR(W, P)
      implicit real (a-h,o-z)
      implicit integer (i-n)   
      REAL X, W, P
C  TMR(KELVIN),W(GRAMS WATER VAPOR/KILOGRAM DRY AIR),P(MILLIBAR)        
      X =  ALOG10 (W * P / (622.+ W))                                   
      TMR = 10.**(.0498646455*X+2.4082965)-7.07475+38.9114*             
     &                   ((10.**( .0915*X ) - 1.2035 )**2 )             
      RETURN                                                            
      END                                                               
c************************************************
      REAL FUNCTION TSA(OS, P)                                               
      implicit real (a-h,o-z)
      implicit integer (i-n)   
      REAL OS, P, A, TQ, D, X, W
      INTEGER I
C  TSA AND OS(KELVIN),P(MILLIBARS)                                      
C  SIGN(A,B) REPLACES THE ALGEBRAIC SIGN OF A WITH THE SIGN OF B        
          A = OS                                                        
          TQ = 253.16                                                   
          D = 120                                                       
          DO 1  I = 1,12                                                
             D = D/2.                                                   
C  IF THE TEMPERATURE DIFFERENCE,X, IS SMALL,EXIT THIS LOOP             
             X=A*EXP(-2.6518986*W(TQ,P)/TQ)-TQ*((1000./P)**.286)        
             IF(ABS(X).LT.0.01)GO TO 2                                  
             TQ = TQ + SIGN(D,X)                                        
  1       CONTINUE                                                      
  2   TSA=TQ                                                            
      RETURN                                                            
      END
c************************************************                             
      REAL FUNCTION W(T, P) 
      implicit real (a-h,o-z)
      implicit integer (i-n)                                            
      REAL T, P, X, ESAT
C     W(GRAMS WATER VAPOR/KILOGRAM DRY AIR ), P(MILLIBAR )                 
      IF (T .GE. 999.) GO TO 10                                         
      X =  ESAT(T)                                                   
      W = 621.97 * X / (P - X)                                       
      RETURN                                                            
   10 W = 0.0                                                           
      RETURN                                                            
      END                                                               
