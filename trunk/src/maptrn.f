C
C-----------------------------------------------------------------------
C
      SUBROUTINE MAPTRN (PHLAT,PHLON,U,V)
C This subroutine overrides the NCAR library-code of MAPTRN to insert
C a parameter-transform PHLAT/PHLON -> RLAT/RLON as defined in the routine
C phys_2_lonlat_trans in the uwgap7/user/transforms_rp.f file. (or equal)
C if no such routine is present, or the transformation-file is not
C loaded, the parameters PHLAT and PHLON are just passed to 
C MAPTRN_2
C Then the NCAR routine MAPTRN is called which has been renamed to
C MAPTRN_2 (a verbatim copy of MAPTRN of the NCAR-library with only
C its name changed).

C the common block containing the flag that indicates the use of
C phys_2_lonlat_trans is imported:
      include 'ph2ll.icl'

      REAL      PHLAT,PHLON,U,V
      REAL      INLAT,INLON,RLAT,RLON
c
c if the displayed data needs to be transformed, do it:
      if (ph2ll_flag.eq.1) then
c the INLAT/INLON vars are used to prevent problems, if the user
c changes the input-parameter values in phys_2_lonlat_trans.
           INLAT=PHLAT
           INLON=PHLON
           CALL PHYS_2_LONLAT_TRANS(INLON,INLAT,RLON,RLAT,1)
      else
           RLAT=PHLAT
           RLON=PHLON
      endif

      write(6,*)'RLAT='RLAT
c call the NCAR - routine
      CALL MAPTRN_2(RLAT,RLON,U,V)

      END
c of MAPTRN


C
C-----------------------------------------------------------------------
C
      SUBROUTINE MAPTRN_2 (RLAT,RLON,U,V)
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCM1/ IPRJ,PHOC,IROD,RSNO,RCSO,RSNR,RCSR
      SAVE /MAPCM1/
      COMMON /MAPDP1/ DSNO,DCSO,DSNR,DCSR
      DOUBLE PRECISION DSNO,DCSO,DSNR,DCSR
      SAVE /MAPDP1/
      COMMON /MAPCM8/ P,Q,R
      SAVE /MAPCM8/
      COMMON /MAPCMB/ IIER
      SAVE /MAPCMB/
      COMMON /MAPSAT/ SALT,SSMO,SRSS,ALFA,BETA,RSNA,RCSA,RSNB,RCSB
      SAVE /MAPSAT/
      COMMON /MAPDPS/ DSNA,DCSA,DSNB,DCSB
      DOUBLE PRECISION DSNA,DCSA,DSNB,DCSB
      SAVE /MAPDPS/
c declare all vars
      REAL      phoc, rsno, rcso, rsnr, rcsr
      REAL      p, q, r, salt, ssmo, rutm1, rutm2, rutm3
      REAL      srss, alfa, beta, rsna, rsnb, rcsb, rlat, rlon
      REAL      u, v, dtor, dtrh, rtod, topi, temp, chi, rsinph
      REAL      rtmp1, rsinla, rtmp2, rcosph, rcosla, rcosa, rsina
      REAL      rsinb, rcosb, cosr, sinr, zz, yy, xx, rtcos, rcsa
      INTEGER   iprj, iier, irod, rvtm1, rvtm2, rvtm3
C
C Declare some things double precision.
C
      DOUBLE PRECISION TMP1,TMP2,SINPH,SINLA,COSPH,COSLA,TCOS,COSA,SINA,
     +                 COSB,SINB,UTM1,VTM1,UTM2,VTM2,UTM3,VTM3
C
C Define required constants.  DTOR is pi over 180, DTRH is half of DTOR
C or pi over 360, and TOPI is 2 over pi.
C
      DATA DTOR / .017453292519943 /
      DATA DTRH / .008726646259971 /
      DATA RTOD / 57.2957795130823 /
      DATA TOPI / .636619772367581 /
C
C Set up U and V for the fast paths.  U is a longitude, in degrees,
C between -180. and +180., inclusive, and V is a latitude, in degrees.
C
      TEMP=RLON-PHOC

      U=TEMP-SIGN(180.,TEMP+180.)+SIGN(180.,180.-TEMP)
      V=RLAT
C
C Take fast paths for simple cylindrical projections.
C
      IF (IPRJ-11) 101,116,112
C
C No fast path.  Sort out the Lambert conformal conic from the rest.
C
  101 IF (IPRJ-1) 901,102,103
C
C Lambert conformal conic.
C
  102 P=U
      CHI=90.-RSNO*RLAT
      IF (CHI.GE.179.9999) GO TO 118
      R=TAN(DTRH*CHI)**RCSO
      U=U*RCSO*DTOR
      V=-R*RSNO*COS(U)
      U=R*SIN(U)
      GO TO 117
C
C Not Lambert conformal conic.  Calculate constants common to most of
C the other projections.
C
  103 IF (IROD.EQ.0) THEN
        RTMP1=U*DTOR
        RTMP2=V*DTOR
        RSINPH=SIN(RTMP1)
        RSINLA=SIN(RTMP2)
        RCOSPH=COS(RTMP1)
        RCOSLA=COS(RTMP2)
        RTCOS=RCOSLA*RCOSPH
        RCOSA=MAX(-1.,MIN(+1.,RSINLA*RSNO+RTCOS*RCSO))
        RSINA=SQRT(1.-RCOSA*RCOSA)
        IF (RSINA.LT..0001) THEN
          RSINA=0.
          IF (IPRJ.EQ.3.AND.ABS(SALT).GT.1.) THEN
            RSINB=0.
            RCOSB=1.
            GO TO 105
          END IF
          IF (IPRJ.GE.7.OR.RCOSA.LT.0.) GO TO 118
          U=0.
          V=0.
          GO TO 116
        END IF
        RSINB=RCOSLA*RSINPH/RSINA
        RCOSB=(RSINLA*RCSO-RTCOS*RSNO)/RSINA
      ELSE
        TMP1=DBLE(U)*.017453292519943D0
        TMP2=DBLE(V)*.017453292519943D0
        SINPH=SIN(TMP1)
        SINLA=SIN(TMP2)
        COSPH=COS(TMP1)
        COSLA=COS(TMP2)
        TCOS=COSLA*COSPH
        COSA=MAX(-1.D0,MIN(+1.D0,SINLA*DSNO+TCOS*DCSO))
        SINA=SQRT(1.D0-COSA*COSA)
        IF (SINA.LT..0001D0) THEN
          SINA=0.D0
          IF (IPRJ.EQ.3.AND.ABS(SALT).GT.1.) THEN
            SINB=0.D0
            COSB=1.D0
            GO TO 105
          END IF
          IF (IPRJ.GE.7.OR.COSA.LT.0.D0) GO TO 118
          U=0.
          V=0.
          GO TO 116
        END IF
        SINB=COSLA*SINPH/SINA
        COSB=(SINLA*DCSO-TCOS*DSNO)/SINA
      END IF
C
C Jump to code appropriate for the chosen projection.
C
      GO TO (104,105,106,107,108,109,110,111,1111) , IPRJ-1
C
C Stereographic.
C
  104 IF (IROD.EQ.0) THEN
        IF (ABS(RSINA).LT..0001) THEN
          R=RSINA/2.
        ELSE
          R=(1.-RCOSA)/RSINA
        END IF
      ELSE
        IF (ABS(SINA).LT..0001D0) THEN
          R=REAL(SINA/2.D0)
        ELSE
          R=REAL((1.D0-COSA)/SINA)
        END IF
      END IF
      GO TO 115
C
C Orthographic or satellite-view, depending on the value of SALT.
C
  105 IF (IROD.EQ.0) THEN
        IF (ABS(SALT).LE.1.) THEN
          IF (RCOSA.GT.0.) THEN
            R=RSINA
          ELSE
            IF (SALT.GE.0.) GO TO 118
            R=2.-RSINA
          END IF
          GO TO 115
        ELSE
          IF (RCOSA.GT.1./ABS(SALT)) THEN
            R=SRSS*RSINA/(ABS(SALT)-RCOSA)
          ELSE
            IF (SALT.GE.0.) GO TO 118
            R=2.-SRSS*RSINA/(ABS(SALT)-RCOSA)
          END IF
          IF (ALFA.EQ.0.) GO TO 115
          RUTM1=R*(RSINB*RCSR+RCOSB*RSNR)
          RVTM1=R*(RCOSB*RCSR-RSINB*RSNR)
          RUTM2=RUTM1*RCSB+RVTM1*RSNB
          RVTM2=RVTM1*RCSB-RUTM1*RSNB
          IF ((SRSS*RCSA+RUTM2*RSNA)/SRSS.LT.1.E-4) GO TO 118
          RUTM3=SRSS*(RUTM2*RCSA-SRSS*RSNA)/(RUTM2*RSNA+SRSS*RCSA)
          RVTM3=SRSS*RVTM2/(RUTM2*RSNA+SRSS*RCSA)
          U=RUTM3*RCSB-RVTM3*RSNB
          V=RVTM3*RCSB+RUTM3*RSNB
          GO TO 116
        END IF
      ELSE
        IF (ABS(SALT).LE.1.) THEN
          IF (COSA.GT.0.D0) THEN
            R=REAL(SINA)
          ELSE
            IF (SALT.GE.0.) GO TO 118
            R=REAL(2.D0-SINA)
          END IF
          GO TO 115
        ELSE
          IF (COSA.GT.1.D0/ABS(DBLE(SALT))) THEN
            R=REAL(DBLE(SRSS)*SINA/(ABS(DBLE(SALT))-COSA))
          ELSE
            IF (SALT.GE.0.) GO TO 118
            R=REAL(2.D0-DBLE(SRSS)*SINA/(ABS(DBLE(SALT))-COSA))
          END IF
          IF (ALFA.EQ.0.) GO TO 115
          UTM1=DBLE(R)*(SINB*DCSR+COSB*DSNR)
          VTM1=DBLE(R)*(COSB*DCSR-SINB*DSNR)
          UTM2=UTM1*DCSB+VTM1*DSNB
          VTM2=VTM1*DCSB-UTM1*DSNB
          IF ((DBLE(SRSS)*DCSA+UTM2*DSNA)/DBLE(SRSS).LT.1.D-4) GO TO 118
          UTM3=DBLE(SRSS)*(UTM2*DCSA-DBLE(SRSS)*DSNA)/
     +                    (UTM2*DSNA+DBLE(SRSS)*DCSA)
          VTM3=DBLE(SRSS)*VTM2/(UTM2*DSNA+DBLE(SRSS)*DCSA)
          U=REAL(UTM3*DCSB-VTM3*DSNB)
          V=REAL(VTM3*DCSB+UTM3*DSNB)
          GO TO 116
        END IF
      END IF
C
C Lambert equal area.
C
  106 IF (IROD.EQ.0) THEN
        IF (ABS(RCOSA+1.).LT.1.E-6) GO TO 118
        R=(1.+RCOSA)/RSINA
      ELSE
        IF (ABS(COSA+1.D0).LT.1.D-6) GO TO 118
        R=REAL((1.D0+COSA)/SINA)
      END IF
      R=2./SQRT(1.+R*R)
      GO TO 115
C
C Gnomonic.
C
  107 IF (IROD.EQ.0) THEN
        IF (RCOSA.LE..0001) GO TO 118
        R=RSINA/RCOSA
      ELSE
        IF (COSA.LE..0001D0) GO TO 118
        R=REAL(SINA/COSA)
      END IF
      GO TO 115
C
C Azimuthal equidistant.
C
  108 IF (IROD.EQ.0) THEN
        IF (ABS(RCOSA+1.).LT.1.E-6) GO TO 118
        R=ACOS(RCOSA)
      ELSE
        IF (ABS(COSA+1.D0).LT.1.D-6) GO TO 118
        R=REAL(ACOS(COSA))
      END IF
      GO TO 115
C
C Cylindrical equidistant, arbitrary pole and orientation.
C
  109 IF (IROD.EQ.0) THEN
        U=ATAN2(RSINB*RCSR+RCOSB*RSNR,RSINB*RSNR-RCOSB*RCSR)*RTOD
        V=90.-ACOS(RCOSA)*RTOD
      ELSE
        U=REAL(ATAN2(SINB*DCSR+COSB*DSNR,SINB*DSNR-COSB*DCSR))*RTOD
        V=90.-REAL(ACOS(COSA))*RTOD
      END IF
      GO TO 116
C
C Mercator, arbitrary pole and orientation.
C
  110 IF (IROD.EQ.0) THEN
        U=ATAN2(RSINB*RCSR+RCOSB*RSNR,RSINB*RSNR-RCOSB*RCSR)
        V=LOG((1.+RCOSA)/RSINA)
      ELSE
        U=REAL(ATAN2(SINB*DCSR+COSB*DSNR,SINB*DSNR-COSB*DCSR))
        V=REAL(LOG((1.D0+COSA)/SINA))
      END IF
      GO TO 116
C
C Mollweide, arbitrary pole and orientation.
C
  111 IF (IROD.EQ.0) THEN
        U=ATAN2(RSINB*RCSR+RCOSB*RSNR,RSINB*RSNR-RCOSB*RCSR)*TOPI
        P=U
        V=RCOSA
        U=U*RSINA
      ELSE
        U=REAL(ATAN2(SINB*DCSR+COSB*DSNR,SINB*DSNR-COSB*DCSR))*TOPI
        P=U
        V=REAL(COSA)
        U=U*REAL(SINA)
      END IF
      GO TO 117
C
C Hammer-Aitoff, arbitrary pole and orientation.
C
 1111 U=ATAN2(SINB*COSR+COSB*SINR,SINB*SINR-COSB*COSR)
      P=U*TOPI
      ZZ=COSA
      YY=SINA*COS(U*0.5)
      XX=SINA*SIN(U*0.5)
      GO TO 1142
C
C Fast-path cylindrical projections (with PLAT=ROTA=0).
C
  112 IF (IPRJ-13) 113,114,1141
C
C Fast-path Mercator.
C
  113 IF (ABS(RLAT).GT.89.9999) GO TO 118
      U=U*DTOR
      V=LOG(TAN((RLAT+90.)*DTRH))
      GO TO 116
C
C Fast-path Mollweide.
C
  114 U=U/90.
      V=SIN(RLAT*DTOR)
      P=U
      U=U*SQRT(1.-V*V)
      GO TO 117
C
C Fast-path Hammer-Aitoff.
C
 1141 IF (IPRJ.GT.14) GO TO 901
      P=U/90.
      ZZ=SIN(RLAT*DTOR)
      YY=COS(RLAT*DTOR)*COS(U*DTOR*0.5)
      XX=COS(RLAT*DTOR)*SIN(U*DTOR*0.5)
 1142 R=SQRT(XX*XX+ZZ*ZZ)
      IF (R.EQ.0.) R=1.
      R=SIN(ACOS(YY))/(SQRT(1.+YY)*R)
      U=R*XX*2.
      V=R*ZZ
      GO TO 117
C
C Common terminal code for certain projections.
C
  115 IF (IROD.EQ.0) THEN
        U=R*(RSINB*RCSR+RCOSB*RSNR)
        V=R*(RCOSB*RCSR-RSINB*RSNR)
      ELSE
        U=R*REAL(SINB*DCSR+COSB*DSNR)
        V=R*REAL(COSB*DCSR-SINB*DSNR)
      END IF
C
  116 P=U
C
  117 Q=V
C
C Normal exit.
C
      RETURN
C
C Projection of point is invisible or undefined.
C
  118 U=1.E12
      P=U
      RETURN
C
C Error exit.
C
  901 IF (IIER.NE.0) GO TO 118
      IIER=16
      CALL SETER (' MAPTRN - ATTEMPT TO USE NON-EXISTENT PROJECTION',
     +              IIER,1)
      GO TO 118
C
      END

CCC
C     Add mapdrw_dl here - fixed mapdrw to use the new european borders
C     Contributed by the folks at atmos.umnw.ethz.ch
CCC
      subroutine mapdrw_dl
c     ====================
      integer   ilvl,initial
      character*2  out
      
      call mapgti('IN',initial)
      if  (initial.ne.0) call mapint
      call mapgrd
      call maplbl
      call mapgtc('OU',out)
      ilvl = 1
      if (out.eq.'CO') then
         ilvl = 2
      elseif (out.eq.'US') then
         ilvl = 4
      elseif (out.eq.'PS') then
         ilvl = 4
      elseif (out.eq.'PO') then
         ilvl = 3
      endif
      call mplndr('Earth..2',ilvl)
c     
      return
      end
      
