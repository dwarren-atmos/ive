      SUBROUTINE VVUMXY (XX,YY,U,V,UVM,XB,YB,XE,YE,IST)
C
C     $Id: vvumxy.f,v 1.2 1999/02/04 00:28:47 harry Exp $
C
C This is a user modifiable routine that allows custom projections of
C the vector space. X and Y give the vector position within the domain
C of the data space. By default, this space is coincident with the
C grid space (i.e. 1 through dimension lengths of the U and V arrays).
C The vector endpoints are output in fractional coordinates (NDC space).
C Note that this is different from the old MXF and MYF routines, which
C output in 'plotter coordinate' space. It also differs from the 
C Conpack routine CPMPXY, which returns values in user space. 
C 
C VVUMXY (Velocity Vector -- User Map X,Y) is called whenever 
C the internal parameter MAP is set to a value other than 0, 1, or 2.
C
C Based on the magnitude and direction of the vector the start and 
C ending points of the vector are returned in NDC space.
C
C Input parameters:
C
C X,Y   -- vector position in the user coordinate system
C U,V   -- vector components from the U,V arrays for this position
C UVM   -- magnitude of the U,V components (supplied for convenience
C          and efficiency - but note that many mappings do not need 
C          this value)
C
C Output parameters:
C
C XB,YB -- starting point of the vector in fractional coordinates
C          (NDC space), before offset based on the value of the
C          vector positioning parameter, VPO.
C XE,YE -- ending point of the vector in fractional coordinates
C          (NDC space), before offset based on the value of the
C          vector positioning parameter, VPO.
C IST   -- status results of the mapping: 0 indicates success -- any
C          non-zero value causes VVECTR to discard the vector at this
C          location.
C
C     $Log: vvumxy.f,v $
C     Revision 1.2  1999/02/04 00:28:47  harry
C     Add vector scaling.
C
C     Revision 1.1  1994/11/22 22:45:25  harry
C     Add vector transform routine vvumxy for use with new NCAR vector routine.
C
C
C The mapping common block: made available to user mapping routines
C     
      integer ist
      real xx, yy, u, v, uvm, xb, yb, xe, ye

      integer imap, nxct, nyct, lnlg, invx, invy, itrt, iwct, ibig
      real  xvpl, xvpr, yvpb, yvpt, wxmn, wxmx, wymn, wymx, xlov, xhiv,
     +     ylov, yhiv, sxdc, sydc, rlen, fw2w, fh2h, dvmn, dvmx, rbig
      COMMON /VVMAP/
     +                IMAP       ,
     +                XVPL       ,XVPR       ,YVPB       ,YVPT       ,
     +                WXMN       ,WXMX       ,WYMN       ,WYMX       ,
     +                XLOV       ,XHIV       ,YLOV       ,YHIV       ,
     +                SXDC       ,SYDC       ,NXCT       ,NYCT       ,
     +                RLEN       ,LNLG       ,INVX       ,INVY       ,
     +                ITRT       ,IWCT       ,FW2W       ,FH2H       ,
     +                DVMN       ,DVMX       ,RBIG       ,IBIG
C
      SAVE /VVMAP/
c
      real yxfactor
      common /c_vvumxy/yxfactor
C
C Math constants
C
      real pdtor, prtod, p1xpi, p2xpi, p1d2pi, p5d2pi
      PARAMETER (PDTOR  = 0.017453292519943,
     +           PRTOD  = 57.2957795130823,
     +           P1XPI  = 3.14159265358979,
     +           P2XPI  = 6.28318530717959,
     +           P1D2PI = 1.57079632679489,
     +           P5D2PI = 7.85398163397448) 
C
C --------------------------------------------------------------------
C
C Description of VVMAP contents:
C
C IMAP                - value of the internal parameter 'MAP'
C XVPL,XVPR,YVPB,YVPT - the currently set viewport values. (GETSET
C                       arguments 1, 2, 3, and 4)
C WXMN,WXMX,WYMN,WYMX - the min and max boundaries of user coordinate
C                       space, (usually but not always equivalent to
C                       window coordinates). WXMN and WYMN are true
C                       minimum values even one or both axes is 
C                       inverted. (i.e. WXMN = MIN of GETSET args 5,6;
C                       WYMN = MIN of GETSET args 7,8)
C XLOV,XHIV,YLOV,YHIV - min and max boundaries of the data space, by
C                       default equivalent to the array grid space.
C                       XLOV and YLOV are not necessarily less than 
C                       XHIV and YHIV respectively.
C SXDC,SYDC           - Scaling factors for converting vector component
C                       values into lengths in NDC space.
C NXCT,NYCT           - Length of each dimension of the U and V 
C                       component arrays.
C RLEN                - Length of the maximum vector in user 
C                       coordinates.
C LNLG                - The linear/log mode (GETSET argument 9)
C INVX,INVY           - User coordinates inversion flags: 
C                       0 - not inverted, 1 - inverted
C ITRT                - value of the internal parameter TRT
C IWCT                - not currently used
C FW2W,FH2H           - scale factors for converting from fraction of
C                       viewport width/height to NDC width/height 
C DVMN,DVMX           - min/max vector lengths in NDC
C RBIG,IBIG           - machine dependent maximum REAL/INTEGER values
C
C --------------------------------------------------------------------
C This code is a modification of VVMPXY code.
C --------------------------------------------------------------------
C
C Local parameters:
C
C PRCFAC - Precision factor used to resolve float equality within
C            the precision of a 4 byte REAL
C PVFRAC - Initial fraction of the vector magnitude used to
C            determine the differential increment
C PFOVFL - Floating point overflow value
C IPMXCT - Number of times to allow the differential to increase
C PDUVML - Multiplier when the differential is increased
C PCSTST - Test value for closeness to 90 degree latitude
C
      real prcfac, pvfrac, pfovfl, ipmxct, pduvml, ipctst
      PARAMETER (PRCFAC=1E5,
     +           PVFRAC=0.001,
     +           PFOVFL=1E12,
     +           IPMXCT=40,
     +           PDUVML=2.0,
     +           IPCTST=PRCFAC*90)
C
C Local variables:
C
C DUV   - incremental UV magnitude difference
C CLT   - cosine of the latitude
C ICT   - repetition count for the difference loop
C SGN   - sign factor, for switching from pos to neg difference
C XT,YT - temporary projected X,Y position values
C XTF,YTF - temporary X,Y fractional coordinate values
C DV1,DV2 - vector magnitudes
C T     - temporary value
C
      real x, y, x1, y1, duv, sgn, xt, yt, xtf, ytf, dv1, dv2
      real clt, t
      integer ict
      real cufx, cufy, uu, vv
      external cufx, cufy
C
C Each end of the vector is calculated in the user coordinate space.
C
C Zero the status flag
C
      IST=0
      if (imap.eq.3) then
         call cpmpxy(4, xx, yy, x, y)
C
C Linear transformation
C
         IF (X .LT. WXMN .OR. X .GT. WXMX .OR.
     +        Y .LT. WYMN .OR. Y .GT. WYMX) THEN
            IST=-5
            RETURN
         ENDIF
         XB=CUFX(X)
         YB=CUFY(Y)
         XE=XB+U*SXDC
         YE=YB+V*SYDC
C
C The following code accounts for non-uniform window coordinate
C systems, such as log scaled coordinates. The technique used
C is similar to the one used in the EZMAP case
C
         IF (ITRT .ne. 0) THEN
            if (itrt .ne. 1 .or. yxfactor .eq. 0.) then
C     
            DV1=SQRT((XE-XB)*(XE-XB)+(YE-YB)*(YE-YB))
C
C Set up an initial increment factor
C
            DUV=0.1/UVM
            ICT=0
            SGN=1.0
            if (itrt .eq. 1) then
               uu = 1.
               vv = 1.
            else
               uu = u
               vv = v
            endif
C     
 10         CONTINUE
C
C Bail out if it's not working
C     
            IF (ICT .GT. IPMXCT) THEN
               IST = -3
               RETURN
            END IF
C
C Calculate the incrmental end points, then check to see if 
C they take us out of the user coordinate boundaries. If they
C do, try incrementing in the other direction
C
            XT=X+SGN*uu*DUV
            YT=Y+SGN*vv*DUV
            IF (XT .LT. WXMN .OR. XT .GT. WXMX .OR.
     +           YT .LT. WYMN .OR. YT .GT. WYMX) THEN
               IF (SGN .EQ. 1.0) THEN
                  SGN = -1.0
                  GO TO 10
               ELSE
                  IST=-4
                  RETURN
               ENDIF
            END IF
C
C Convert to fractional coordinates and find the incremental
C distance in the fractional system. To ensure that this distance
C is meaningful, we require that it be between 1E3 and 1E4
C times smaller than the maximum vector length.
C 
            XTF=CUFX(XT)
            YTF=CUFY(YT)
            DV2=SQRT((XTF-XB)*(XTF-XB)+(YTF-YB)*(YTF-YB))
            IF (DV2*1E3 .GT. DVMX) THEN
               ICT=ICT+1
               DUV=DUV/2.0
               GO TO 10
            ELSE IF (DV2*1E4 .LT. DVMX) THEN
               ICT=ICT+1
               DUV=DUV*2.0
               GO TO 10
            END IF
            endif
C
C The actual endpoints are found using the ratio of the incremental
C distance to the actual distance times the fractional component
C length
            if (itrt .eq. 1) then
               if (yxfactor .eq. 0.) then
                  yxfactor = (ytf-yb)/(xtf-xb)
               endif
               if (yxfactor.gt.1) then
                  ye=yb+v*sydc*yxfactor
               else
                  xe=xb+u*sxdc/yxfactor
               endif
            else
               XE=XB+SGN*(XTF-XB)*DV1/DV2
               YE=YB+SGN*(YTF-YB)*DV1/DV2
            endif
         END IF
      else
C
C     Map
C
         call cpmpxy(4, xx, yy, x1, y1)
         call phys_2_lonlat_trans(x1, y1, x, y, 1)
C
C X is longitude, Y is latitude
C If Y is 90 degrees, a directional vector is meaningless
C Since EZMAP uses uniform user coordinates, don't convert to 
C fractional coordinates until the end
C The tranformation type parameter is not used.
C
         IF (IFIX(ABS(Y)*PRCFAC+0.5).EQ.IPCTST) THEN
            IST=-1
            RETURN
         END IF
C
C Project the starting value: bail out if outside the window
C
         CALL MAPTRN (Y,X,XB,YB)
         IF (XB .LT. WXMN .OR. XB .GT. WXMX .OR.
     +       YB .LT. WYMN .OR. YB .GT. WYMX) THEN
            IST=-5
            RETURN
         END IF
C
C Check the vector magnitude
C
         IF (IFIX(UVM*PRCFAC+0.5) .EQ. 0) THEN
            IST=-2
            RETURN
         END IF
C
C The incremental distance is proportional to a small fraction
C of the vector magnitude
C
         DUV=PVFRAC/UVM
         CLT=COS(Y*PDTOR)
C
C Project the incremental distance. If the positive difference doesn't
C work, try the negative difference. If the difference results in a 
C zero length vector, try a number of progressively larger increments. 
C
         ICT=0
         SGN=1.0
 20      CONTINUE

         CALL MAPTRN(Y+SGN*V*DUV,X+SGN*U*DUV/CLT,XT,YT)

         DV1=SQRT((XT-XB)*(XT-XB)+(YT-YB)*(YT-YB))
         IF (DV1 .GT. RLEN) THEN
            IF (SGN .EQ. -1.0) THEN
               IST=-4
               RETURN
            ELSE
               SGN=-1.0
               GO TO 20
            END IF
         END IF
C
         IF (IFIX(DV1*PRCFAC) .EQ. 0) THEN
            IF (ICT .LT. IPMXCT) THEN
               ICT = ICT + 1
               DUV=DUV*PDUVML
               GO TO 20
            ELSE
               IST=-3
               RETURN
            END IF
         END IF
C
         IF (ABS(XT) .GE. PFOVFL .OR. ABS(YT) .GE. PFOVFL) THEN
            IST=-6
            RETURN
         END IF
C
         T=SGN*((XT-XB)/DV1)*UVM
         XB=CUFX(XB)
         XE=XB+T*SXDC
         T=SGN*((YT-YB)/DV1)*UVM
         YB=CUFY(YB)
         YE=YB+T*SYDC
C
      END IF
C
C Done.
C
      RETURN
C
      END




