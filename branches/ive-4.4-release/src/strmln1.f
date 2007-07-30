      SUBROUTINE STRMLN1 (U,V,WORK,IMAX,IPTSX,JPTSY,NSET,IER, imap)

c-----------------------------------------------------------------------
c     $Id: strmln1.f,v 1.3 2000/05/10 22:55:50 warren Exp $
c     Purpose:
c        This is a modified version of strmln. The major modification
c        is that the statement functions FX and FY have been replaced
c        by calls to the routine cpmpxy.
c     Added arguments:
c        imap  int  input  indicates which mapping will be used by
c                          cpmpxy.
c     History:
c     $Log: strmln1.f,v $
c     Revision 1.3  2000/05/10 22:55:50  warren
c     commented out common blocks STR02 and STR03 and included strmln.icl to
c     make all versions of the common blocks consitant.
c
c     Revision 1.2  1997/01/30 21:39:10  harry
c     Fix strmln1 to look for out of range values from cpmpxy.
c
c     Revision 1.1.1.1  1993/04/07 19:40:39  harry
c     Initial CVS checkin of IVE.
c
c Revision 2.2  1992/10/29  23:10:20  warren
c make sure special value is not 0
c
c Revision 2.1  1992/10/29  20:21:39  warren
c fixed missing value problem
c
c Revision 2.0  1992/07/01  21:15:13  millerp
c Released version rereving to 2.0
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
C
C +-----------------------------------------------------------------+
C |                                                                 |
C |                Copyright (C) 1989 by UCAR                       |
C |        University Corporation for Atmospheric Research          |
C |                    All Rights Reserved                          |
C |                                                                 |
C |                 NCARGRAPHICS  Version 3.00                      |
C |                                                                 |
C +-----------------------------------------------------------------+
C
C SUBROUTINE STRMLN (U,V,WORK,IMAX,IPTSX,JPTSY,NSET,IER)
C
C DIMENSION OF           U(IMAX,JPTSY) , V(IMAX,JPTSY) ,
C ARGUMENTS              WORK(2*IMAX*JPTSY)
C
C PURPOSE                STRMLN draws a streamline representation of
C                        the flow field. The representation is
C                        independent of the flow speed.
C
C USAGE                  If the following assumptions are met, use
C
C                            CALL EZSTRM  (U,V,WORK,IMAX,JMAX)
C
C                          Assumptions:
C                            --The whole array is to be processed.
C                            --The arrays are dimensioned
C                              U(IMAX,JMAX) , V(IMAX,JMAX) and
C                              WORK(2*IMAX*JMAX).
C                            --Window and viewport are to be chosen
C                              by STRMLN.
C                            --PERIM is to be called.
C
C                        If these assumptions are not met, use
C
C                            CALL STRMLN (U,V,WORK,IMAX,IPTSX,JPTSY,
C                                         NSET,IER)
C
C                        The user must call FRAME in the calling
C                        routine.
C
C                        The user may change various internal
C                        parameters via common blocks. See below.
C
C ARGUMENTS
C
C ON INPUT               U, V
C                          Two dimensional arrays containing the
C                          velocity fields to be plotted.
C
C                          Note:  If the U AND V components
C                          are, for example, defined in Cartesian
C                          coordinates and the user wishes to plot them
C                          on a different projection (i.e., stereo-
C                          graphic), then the appropriate
C                          transformation must be made to the U and V
C                          components via the functions FU and FV
C                          (located in DRWSTR).
C
C                        WORK
C                          User provided work array.  The dimension
C                          of this array must be .GE. 2*IMAX*JPTSY.
C
C                          Caution:  This routine does not check the
C                          size of the work array.
C
C                        IMAX
C                          The first dimension of U and V in the
C                          calling program. (X-direction)
C
C                        IPTSX
C                          The number of points to be plotted in the
C                          first subscript direction.  (X-direction)
C
C                        JPTSY
C                          The number of points to be plotted in the
C                          second subscript direction. (Y-direction)
C
C                        NSET
C                          Flag to control scaling
C                          > 0  STRMLN assumes that the window
C                               and viewport have been set by the
C                               user in such a way as to properly
C                               scale the plotting instructions
C                               generated by STRMLN. PERIM is not
C                               called.
C                          = 0  STRMLN will establish the window and
C                               viewport to properly scale the
C                               plotting instructions to the standard
C                               configuration. PERIM is called to draw
C                               the border.
C                          < 0  STRMLN establishes the window
C                               and viewport so as to place the
C                               streamlines within the limits
C                               of the user's window.  PERIM is
C                               not called.
C
C ON OUTPUT              Only the IER argument may be changed. All
C                        other arguments are unchanged.
C
C
C                        IER
C                          =  0 when no errors are detected
C                          = -1 when the routine is called with ICYC
C                               .NE. 0  and the data are not cyclic
C                               (ICYC is an internal parameter
C                               described below); in this case the
C                               routine will draw the
C                               streamlines with the non-cyclic
C                               interpolation formulas.
C
C ENTRY POINTS           STRMLN, DRWSTR, EZSTRM, GNEWPT, CHKCYC
C
C COMMON BLOCKS          STR01, STR02, STR03, STR04
C
C REQUIRED LIBRARY       GRIDAL, GBYTES, and the SPPS
C ROUTINES
C
C REQUIRED GKS LEVEL     0A
C
C I/O                    None
C
C PRECISION              Single
C
C LANGUAGE               FORTRAN 77
C
C HISTORY                Written and standardized in November 1973.
C
C                        Converted to FORTRAN 77 and GKS in June, 1984.
C
C
C PORTABILITY            FORTRAN 77
C
C ALGORITHM              Wind components are normalized to the value
C                        of DISPL. The least significant two
C                        bits of the work array are
C                        utilized as flags for each grid box. Flag 1
C                        indicates whether any streamline has
C                        previously passed through this box.  Flag 2
C                        indicates whether a directional arrow has
C                        already appeared in a box. Judicious use
C                        of these flags prevents overcrowding of
C                        streamlines and directional arrows.
C                        Experience indicates that a final pleasing
C                        picture is produced when streamlines are
C                        initiated in the center of a grid box. The
C                        streamlines are drawn in one direction then
C                        in the opposite direction.
C
C REFERENCE              The techniques utilized here are described
C                        in an article by Thomas Whittaker (U. of
C                        Wisconsin) which appeared in the notes and
C                        correspondence section of Monthly Weather
C                        Review, June 1977.
C
C TIMING                 Highly variable
C                          It depends on the complexity of the
C                          flow field and the parameters:  DISPL,
C                          DISPC , CSTOP , INITA , INITB , ITERC ,
C                          and IGFLG. (See below for a discussion
C                          of these parameters.) If all values
C                          are default, then a simple linear
C                          flow field for a 40 x 40 grid will
C                          take about 0.4 seconds on the CRAY1-A;
C                          a fairly complex flow field will take about
C                          1.5 seconds on the CRAY1-A.
C
C
C INTERNAL PARAMETERS
C
C                        NAME     DEFAULT            FUNCTION
C                        ----     -------            --------
C
C                        EXT       0.25      Lengths of the sides of the
C                                            plot are proportional to
C                                            IPTSX and JPTSY except in
C                                            the case when MIN(IPTSX,JPT)
C                                            / MAX(IPTSX,JPTSY) .LT. EXT;
C                                            in that case a square
C                                            graph is plotted.
C
C                        SIDE      0.90      Length of longer edge of
C                                            plot. (See also EXT.)
C
C                        XLT       0.05      Left hand edge of the plot.
C                                            (0.0 = left edge of frame)
C                                            (1.0 = right edge of frame)
C
C                        YBT       0.05      Bottom edge of the plot.
C                                            (0.0 = bottom ; 1.0 = top)
C
C                                            (YBT+SIDE and XLT+SIDE must
C                                            be .LE. 1. )
C
C                        INITA     2         Used to precondition grid
C                                            boxes to be eligible to
C                                            start a streamline.
C                                            For example, a value of 4
C                                            means that every fourth
C                                            grid box is eligible ; a
C                                            value of 2 means that every
C                                            other grid box is eligible.
C                                            (see INITB)
C
C                        INITB     2         Used to precondition grid
C                                            boxes to be eligible for
C                                            direction arrows.
C                                            If the user changes the
C                                            default values of INITA
C                                            and/or INITB, it should
C                                            be done such that
C                                            MOD(INITA,INITB) = 0 .
C                                            For a dense grid try
C                                            INITA=4 and INITB=2 to
C                                            reduce the CPU time.
C
C                        AROWL     0.33      Length of direction arrow.
C                                            For example, 0.33 means
C                                            each directional arrow will
C                                            take up a third of a grid
C                                            box.
C
C                        ITERP     35        Every 'ITERP' iterations
C                                            the streamline progress
C                                            is checked.
C
C                        ITERC     -99       The default value of this
C                                            parameter is such that
C                                            it has no effect on the
C                                            code. When set to some
C                                            positive value, the program
C                                            will check for streamline
C                                            crossover every 'ITERC'
C                                            iterations. (The routine
C                                            currently does this every
C                                            time it enters a new grid
C                                            box.)
C                                            Caution:  When this
C                                            parameter is activated,
C                                            CPU time will increase.
C
C                        IGFLG     0         A value of zero means that
C                                            the sixteen point Bessel
C                                            Interpolation Formula will
C                                            be utilized where possible;
C                                            when near the grid edges,
C                                            quadratic and bi-linear
C                                            interpolation  will be
C                                            used. This mixing of
C                                            interpolation schemes can
C                                            sometimes cause slight
C                                            raggedness near the edges
C                                            of the plot.  If IGFLG.NE.0,
C                                            then only the bilinear
C                                            interpolation formula
C                                            is used; this will generally
C                                            result in slightly faster
C                                            plot times but a less
C                                            pleasing plot.
C
C                        IMSG      0         If zero, then no missing
C                                            U and V components are
C                                            present.
C                                            If .NE. 0, STRMLN will
C                                            utilize the
C                                            bi-linear interpolation
C                                            scheme and terminate if
C                                            any data points are missing.
C
C                        UVMSG     1.E+36    Value assigned to a missing
C                                            point.
C
C                        ICYC      0         Zero means the data are
C                                            non-cyclic in the X
C                                            direction.
C                                            If .NE 0, the
C                                            cyclic interpolation
C                                            formulas will be used.
C                                            (Note:  Even if the data
C                                            are cyclic in X, leaving
C                                            ICYC = 0 will do no harm.)
C
C                        DISPL     0.33      The wind speed is
C                                            normalized to this value.
C                                            (See the discussion below.)
C
C                        DISPC     0.67      The critical displacement.
C                                            If after 'ITERP' iterations
C                                            the streamline has not
C                                            moved this distance, the
C                                            streamline will be
C                                            terminated.
C
C                        CSTOP     0.50      This parameter controls
C                                            the spacing between
C                                            streamlines.  The checking
C                                            is done when a new grid
C                                            box is entered.
C
C DISCUSSION OF          Assume a value of 0.33 for DISPL.  This
C DISPL,DISPC            means that it will take three steps to move
C AND CSTOP              across one grid box if the flow was all in the
C                        X direction. If the flow is zonal, then a
C                        larger value of DISPL is in order.
C                        If the flow is highly turbulent, then
C                        a smaller value is in order.  The smaller
C                        DISPL, the more the CPU time.  A value
C                        of 2 to 4 times DISPL is a reasonable value
C                        for DISPC.  DISPC should always be greater
C                        than DISPL. A value of 0.33 for CSTOP would
C                        mean that a maximum of three stream-
C                        lines will be drawn per grid box. This max
C                        will normally only occur in areas of singular
C                        points.
C
C                                            ***************************
C                                            Any or all of the above
C                                            parameters may be changed
C                                            by utilizing common blocks
C                                            STR02 and/or STR03
C                                            ***************************
C
C                        UXSML               A number which is small
C                                            compared to the average
C                                            normalized u component.
C                                            Set automatically.
C
C                        NCHK      750       This parameter is located
C                                            in DRWSTR. It specifies the
C                                            length of the circular
C                                            lists  used for checking
C                                            for STRMLN crossovers.
C                                            For most plots this number
C                                            may be reduced to 500
C                                            or less and the plots will
C                                            not be altered.
C
C                        ISKIP               Number of bits to be
C                                            skipped to get to the
C                                            least two significant bits
C                                            in a floating point number.
C                                            The default value is set to
C                                            I1MACH(5) - 2 . This value
C                                            may have to be changed
C                                            depending on the target
C                                            computer; see subroutine
C                                            DRWSTR.
C
C
C
      DIMENSION       U(IMAX,JPTSY)         ,V(IMAX,JPTSY)           ,
     1                WORK(1)
      DIMENSION       WNDW(4)               ,VWPRT(4)
C
      COMMON /STR01/  IS         ,IEND      ,JS        ,JEND
     1             ,  IEND1      ,JEND1     ,I         ,J
     2             ,  X          ,Y         ,DELX      ,DELY
     3             ,  ICYC1      ,IMSG1     ,IGFL1
c      COMMON /STR02/  EXT , SIDE , XLT , YBT
c      COMMON /STR03/  INITA , INITB , AROWL , ITERP , ITERC , IGFLG
c     1             ,  IMSG , UVMSG , ICYC , DISPL , DISPC , CSTOP
      include 'strmln.icl'
C
      SAVE
C
      EXT       = 0.25
      SIDE      = 0.90
      XLT       = 0.05
      YBT       = 0.05
C
      INITA     = 2
      INITB     = 2
      AROWL     = 0.33
      ITERP     = 35
      ITERC     = -99
      IGFLG     = 0
      ICYC      = 0
      IMSG      = 0
C      UVMSG     = 1.E+36
      DISPL     = 0.33
      DISPC     = 0.67
      CSTOP     = 0.50
C
C THE FOLLOWING CALL IS FOR MONITORING LIBRARY USE AT NCAR
C
      CALL Q8QST4 ( 'GRAPHX', 'STRMLN', 'STRMLN', 'VERSION 01')
C
      IER = 0
C
C LOAD THE COMMUNICATION COMMON BLOCK WITH PARAMETERS
C
      IS = 1
      IEND = IPTSX
      JS = 1
      JEND = JPTSY
      IEND1 = IEND-1
      JEND1 = JEND-1
      IEND2 = IEND-2
      JEND2 = JEND-2
      XNX = FLOAT(IEND-IS+1)
      XNY = FLOAT(JEND-JS+1)
      ICYC1 = ICYC
      IGFL1 = IGFLG
      IMSG1 = 0
C
C IF ICYC .NE. 0 THEN CHECK TO MAKE SURE THE CYCLIC CONDITION EXISTS.
C
      IF (ICYC1.NE.0) CALL CHKCYC  (U,V,IMAX,JPTSY,IER)
C     
C SAVE ORIGINAL  NORMALIZATION TRANSFORMATION NUMBER
C     
      CALL GQCNTN ( IERR,NTORIG )
C
C SET UP SCALING
C
      IF (NSET) 10 , 20 , 60
 10   CALL GETUSV ( 'LS' , ITYPE )
      CALL GQNT ( NTORIG,IERR,WNDW,VWPRT )
      CALL GETUSV('LS',IOLLS)
      X1 = VWPRT(1)
      X2 = VWPRT(2)
      Y1 = VWPRT(3)
      Y2 = VWPRT(4)
      X3 = IS
      X4 = IEND
      Y3 = JS
      Y4 = JEND
      GO TO  55
C
   20 ITYPE = 1
      X1 = XLT
      X2 = (XLT+SIDE)
      Y1 = YBT
      Y2 = (YBT+SIDE)
      X3 = IS
      X4 = IEND
      Y3 = JS
      Y4 = JEND
      IF (AMIN1(XNX,XNY)/AMAX1(XNX,XNY).LT.EXT) GO TO  50
      IF (XNX-XNY)  30, 50, 40
   30 X2 = (SIDE*(XNX/XNY) + XLT)
      GO TO  50
   40 Y2 = (SIDE*(XNY/XNX) + YBT)
   50 CONTINUE
C
C CENTER THE PLOT
C
      DX = 0.25*( 1. - (X2-X1) )
      DY = 0.25*( 1. - (Y2-Y1) )
      X1 = (XLT+DX)
      X2 = (X2+DX )
      Y1 = (YBT+DY)
      Y2 = (Y2+DY )
C
   55 CONTINUE
C
C SAVE NORMALIZATION TRANSFORMATION 1
C
      CALL GQNT ( 1,IERR,WNDW,VWPRT )
C
C DEFINE AND SELECT NORMALIZATION TRANS, SET LOG SCALING
C
      CALL SET(X1,X2,Y1,Y2,X3,X4,Y3,Y4,ITYPE)
C
      IF (NSET.EQ.0) CALL PERIM (1,0,1,0)
C
   60 CONTINUE
C
C DRAW THE STREAMLINES
C .   BREAK THE WORK ARRAY INTO TWO PARTS.  SEE DRWSTR FOR FURTHER
C .   COMMENTS ON THIS.
C
      CALL DRWSTR (U,V,WORK(1),WORK(IMAX*JPTSY+1),IMAX,JPTSY, imap)
C
C RESET NORMALIATION TRANSFORMATION 1 TO ORIGINAL VALUES
C
      IF (NSET .LE. 0) THEN
        CALL SET(VWPRT(1),VWPRT(2),VWPRT(3),VWPRT(4),
     -           WNDW(1),WNDW(2),WNDW(3),WNDW(4),IOLLS)
      ENDIF
      CALL GSELNT (NTORIG)
C
      RETURN
      END
c
c
      SUBROUTINE DRWSTR  (U,V,UX,VY,IMAX,JPTSY, imap)
c-----------------------------------------------------------------------
C
      PARAMETER (NCHK=750)
C
C THIS ROUTINE DRAWS THE STREAMLINES.
C .   THE XCHK AND YCHK ARRAYS SERVE AS A CIRCULAR LIST. THEY
C .   ARE USED TO PREVENT LINES FROM CROSSING ONE ANOTHER.
C
C THE WORK ARRAY HAS BEEN BROKEN UP INTO TWO ARRAYS FOR CLARITY.  THE
C .   TOP HALF OF WORK (CALLED UX) WILL HAVE THE NORMALIZED (AND
C .   POSSIBLY TRANSFORMED) U COMPONENTS AND WILL BE USED FOR BOOK
C .   KEEPING.  THE LOWER HALF OF THE WORK ARRAY (CALLED VY) WILL
C .   CONTAIN THE NORMALIZED (AND POSSIBLY TRANSFORMED) V COMPONENTS.
C
      DIMENSION       U(IMAX,JPTSY)         ,V(IMAX,JPTSY)
     1             ,  UX(IMAX,JPTSY)        ,VY(IMAX,JPTSY)
      COMMON /STR01/  IS         ,IEND      ,JS        ,JEND
     1             ,  IEND1      ,JEND1     ,I         ,J
     2             ,  X          ,Y         ,DELX      ,DELY
     3             ,  ICYC1      ,IMSG1     ,IGFL1
c      COMMON /STR03/  INITA , INITB , AROWL , ITERP , ITERC , IGFLG
c     1             ,  IMSG , UVMSG , ICYC , DISPL , DISPC , CSTOP
      include 'strmln.icl'
      COMMON /STR04/  XCHK(NCHK) ,YCHK(NCHK) , NUMCHK , UXSML
C
C
        SAVE
C
C STATEMENT FUNCTIONS FOR SPATIAL AND VELOCITY TRANSFORMATIONS.
C .   (IF THE USER WISHES OTHER TRANSFORMATIONS  REPLACE THESE STATEMENT
C .   FUNCTIONS WITH THE APPROPRIATE NEW ONES, OR , IF THE TRANSFORMA-
C .   TIONS ARE COMPLICATED DELETE THESE STATEMENT FUNCTIONS
C .   AND ADD EXTERNAL ROUTINES WITH THE SAME NAMES TO DO THE TRANS-
C .   FORMING.)
C
c      FX(X,Y) = X
c      FY(X,Y) = Y
      FU(X,Y) = X
      FV(X,Y) = Y
C
C     INITIALIZE
C
      ISKIP    = I1MACH(5) - 2
      ISKIP1   = ISKIP + 1
      UXSML    = R1MACH(3) * DISPL
C
C
      NUMCHK   = NCHK
      LCHK = 1
      ICHK = 1
      XCHK(1) = 0.
      YCHK(1) = 0.
      KFLAG = 0
      IZERO = 0
      IONE = 1
      ITWO = 2
C
C
C     COMPUTE THE X AND Y NORMALIZED (AND POSSIBLY TRANSFORMED)
C     .   DISPLACEMENT COMPONENTS (UX AND VY).
C
      DO  40 J=JS,JEND
         DO  30 I=IS,IEND
            UX(I,J) = FU(U(I,J),V(I,J))
            VY(I,J) = FV(U(I,J),V(I,J))
            if(uvmsg .ne. 0.0) then
               if ( UX(I,J) .eq. uvmsg .or. VY(I,J) .eq. uvmsg ) then
                  UX(I,J) = 0
                  VY(I,J) = 0
                  CALL SBYTES( UX(I,J) , IONE , ISKIP1, 1 , 0 , 1 )
                  CALL SBYTES( UX(I,J) , IONE , ISKIP , 1 , 0 , 1 )
                  goto 30
               endif
            endif
            IF (UX(I,J).NE.0. .OR. VY(I,J).NE.0.) THEN
               CON = DISPL/SQRT(UX(I,J)*UX(I,J) + VY(I,J)*VY(I,J))
               UX(I,J) = CON*UX(I,J)
               VY(I,J) = CON*VY(I,J)
            END IF
C
C     BOOKKEEPING IS DONE IN THE LEAST SIGNIFICANT BITS OF THE UX ARRAY.
C     .   WHEN UX(I,J) IS EXACTLY ZERO THIS CAN PRESENT SOME PROBLEMS.
C     .   TO GET AROUND THIS PROBLEM, SET IT TO A RELATIVELY SMALL 
C         NUMBER.
C
            IF(UX(I,J) .EQ. 0.) UX(I,J) = UXSML
C
C     MASK OUT THE LEAST SIGNIFICANT TWO BITS AS FLAGS FOR EACH GRID BOX
C     .   A GRID BOX IS ANY REGION SURROUNDED BY FOUR GRID POINTS.
C     .   FLAG 1 INDICATES WHETHER ANY STREAMLINE HAS PREVIOUSLY PASSED
C     .          THROUGH THIS BOX.
C     .   FLAG 2 INDICATES WHETHER ANY DIRECTIONAL ARROW HAS ALREADY
C     .          APPEARED IN THIS BOX.
C     .   JUDICIOUS USE OF THESE FLAGS PREVENTS OVERCROWDING OF
C     .   STREAMLINES AND DIRECTIONAL ARROWS.
C     
            CALL SBYTES( UX(I,J) , IZERO , ISKIP , 2 , 0 , 1 )
C     
            IF (MOD(I,INITA).NE.0 .OR. MOD(J,INITA).NE.0)
     1           CALL SBYTES( UX(I,J) , IONE , ISKIP1, 1 , 0 , 1 )
            IF (MOD(I,INITB).NE.0 .OR. MOD(J,INITB).NE.0)
     1           CALL SBYTES( UX(I,J) , IONE , ISKIP , 1 , 0 , 1 )
C
 30      CONTINUE
 40   CONTINUE
C
 50   CONTINUE
C
C     START A STREAMLINE. EXPERIENCE HAS SHOWN THAT A PLEASING PICTURE
C     .   WILL BE PRODUCED IF NEW STREAMLINES ARE STARTED ONLY IN GRID
C     .   BOXES THAT PREVIOUSLY HAVE NOT HAD OTHER STREAMLINES PASS THROUGH
C     .   THEM. AS LONG AS A REASONABLY DENSE PATTERN OF AVAILABLE BOXES
C     .   IS INITIALLY PRESCRIBED, THE ORDER OF SCANNING THE GRID PTS. FOR
C     .   AVAILABLE BOXES IS IMMATERIAL
C     
C     FIND AN AVAILABLE BOX FOR STARTING A STREAMLINE
C
      IF (KFLAG.NE.0) GO TO  90
      DO  70 J=JS,JEND1
         DO  60 I=IS,IEND1
            CALL GBYTES( UX(I,J) , IUX , ISKIP , 2 , 0 , 1 )
            IF ( IAND( IUX , IONE ) .EQ. IZERO ) GO TO 80
 60      CONTINUE
 70   CONTINUE
C     
C     MUST BE NO AVAILABLE BOXES FOR STARTING A STREAMLINE
C     
      GO TO 190
 80   CONTINUE
C     
C     INITILIZE PARAMETERS FOR STARTING A STREAMLINE
C     .   TURN THE BOX OFF FOR STARTING A STREAMLINE
C     .   CHECK TO SEE IF THIS BOX HAS MISSING DATA (IMSG.NE.0). IF SO ,
C     .      FIND A NEW STARTING BOX
C     
      CALL SBYTES( UX(I,J) , IONE , ISKIP1 , 1 , 0 , 1 )
      IF ( IMSG.EQ.0) GO TO 85
      IF (U(I,J).EQ.UVMSG .OR. U(I,J+1).EQ.UVMSG .OR.
     1     U(I+1,J).EQ.UVMSG .OR. U(I+1,J+1).EQ.UVMSG) GO TO 50
C     
 85   ISAV = I
      JSAV = J
      KFLAG = 1
      PLMN1 = +1.
      GO TO 100
 90   CONTINUE
C     
C     COME TO HERE TO DRAW IN THE OPPOSITE DIRECTION
C     
      KFLAG = 0
      PLMN1 = -1.
      I = ISAV
      J = JSAV
 100  CONTINUE
C     
C     INITIATE THE DRAWING SEQUENCE
C     .   START ALL STREAMLINES IN THE CENTER OF A BOX
C     
      NBOX = 0
      ITER = 0
      IF (KFLAG.NE.0) ICHKB = ICHK+1
      IF (ICHKB.GT.NUMCHK) ICHKB = 1
      X = FLOAT(I)+0.5
      Y = FLOAT(J)+0.5
      XBASE = X
      YBASE = Y
      call cpmpxy (imap, x, y, xout, yout)
      call fl2int (xout, yout, ifx, ify)
ccc   CALL FL2INT (FX(X,Y),FY(X,Y),IFX,IFY)
      CALL PLOTIT (IFX,IFY,0)
      CALL GBYTES( UX(I,J) , IUX , ISKIP , 2 , 0 , 1 )
      IF ( (KFLAG.EQ.0) .OR. (IAND( IUX , ITWO ) .NE. 0 ) ) GO TO 110
C     
C     GRID BOX MUST BE ELIGIBLE FOR A DIRECTIONAL ARROW
C     
      CALL GNEWPT (UX,VY,IMAX,JPTSY)
      MFLAG = 1
      GO TO 160
C     
 110  CONTINUE
C     
C     PLOT LOOP
C     .   CHECK TO SEE IF THE STREAMLINE HAS ENTERED A NEW GRID BOX
C     
      IF (I.NE.IFIX(X) .OR. J.NE.IFIX(Y)) GO TO 120
C     
C     MUST BE IN SAME BOX CALCULATE THE DISPLACEMENT COMPONENTS
C     
      CALL GNEWPT (UX,VY,IMAX,JPTSY)
C     
C     UPDATE THE POSITION AND DRAW THE VECTOR
C     
      X = X+PLMN1*DELX
      Y = Y+PLMN1*DELY
      call cpmpxy (imap, x, y, xout, yout)
      if (xout .eq. 1.0e12) goto 50
      call fl2int (xout, yout, ifx, ify)
ccc      CALL FL2INT (FX(X,Y),FY(X,Y),IFX,IFY)
      CALL PLOTIT (IFX,IFY,1)
      ITER = ITER+1
C     
C     CHECK STREAMLINE PROGRESS EVERY 'ITERP' OR SO ITERATIONS
C     
      IF (MOD(ITER,ITERP).NE.0) GO TO 115
      IF (ABS(X-XBASE).LT.DISPC  .AND. ABS(Y-YBASE).LT.DISPC ) GO TO  50
      XBASE = X
      YBASE = Y
      GO TO 110
 115  CONTINUE
C     
C     SHOULD THE CIRCULAR LISTS BE CHECKED FOR STREAMLINE CROSSOVER
C     
      IF ( (ITERC.LT.0) .OR. (MOD(ITER,ITERC).NE.0) ) GO TO 110
C     
C     MUST WANT THE CIRCULAR LIST CHECKED
C     
      GO TO 130
 120  CONTINUE
C     
C     MUST HAVE ENTERED A NEW GRID BOX  CHECK FOR THE FOLLOWING :
C     .   (1) ARE THE NEW POINTS ON THE GRID
C     .   (2) CHECK FOR MISSING DATA IF MSG  DATA FLAG (IMSG) HAS BEEN SET.
C     .   (3) IS THIS BOX ELIGIBLE FOR A DIRECTIONAL ARROW
C     .   (4) LOCATION OF THIS ENTRY VERSUS OTHER STREAMLINE ENTRIES
C     
      NBOX = NBOX+1
C     
C     CHECK (1)
C     
      IF (IFIX(X).LT.IS .OR. IFIX(X).GT.IEND1) GO TO  50
      IF (IFIX(Y).LT.JS .OR. IFIX(Y).GT.JEND1) GO TO  50
C     
C     CHECK (2)
C     
      IF ( IMSG.EQ.0) GO TO 125
      II = IFIX(X)
      JJ = IFIX(Y)
      IF (U(II,JJ).EQ.UVMSG .OR. U(II,JJ+1).EQ.UVMSG .OR.
     1     U(II+1,JJ).EQ.UVMSG .OR. U(II+1,JJ+1).EQ.UVMSG) GO TO 50
 125  CONTINUE
C     
C     CHECK (3)
C     
      CALL GBYTES( UX(I,J) , IUX , ISKIP , 2 , 0 , 1 )
      IF ( IAND( IUX , ITWO )  .NE. 0) GO TO 130
      MFLAG = 2
      GO TO 160
 130  CONTINUE
C     
C     CHECK (4)
C     
      DO 140 LOC=1,LCHK
         IF (ABS( X-XCHK(LOC) ).GT.CSTOP .OR.
     1        ABS( Y-YCHK(LOC) ).GT.CSTOP) GO TO 140
         LFLAG = 1
         IF (ICHKB.LE.ICHK .AND. LOC.GE.ICHKB .AND. LOC.LE.ICHK) 
     &       LFLAG = 2
         IF (ICHKB.GE.ICHK .AND. (LOC.GE.ICHKB .OR. LOC.LE.ICHK)) 
     &       LFLAG = 2
         IF (LFLAG.EQ.1) GO TO  50
 140  CONTINUE
      LCHK = MIN0(LCHK+1,NUMCHK)
      ICHK = ICHK+1
      IF (ICHK.GT.NUMCHK) ICHK = 1
      XCHK(ICHK) = X
      YCHK(ICHK) = Y
      I = IFIX(X)
      J = IFIX(Y)
      CALL SBYTES( UX(I,J) , IONE , ISKIP1 , 1 , 0 , 1 )
      IF (NBOX.LT.5) GO TO 150
      ICHKB = ICHKB+1
      IF (ICHKB.GT.NUMCHK) ICHKB = 1
 150  CONTINUE
      GO TO 110
C     
 160  CONTINUE
C     
C     THIS SECTION DRAWS A DIRECTIONAL ARROW BASED ON THE MOST RECENT DIS-
C     .   PLACEMENT COMPONENTS ,DELX AND DELY, RETURNED BY GNEWPT. IN EARLIE
C     .   VERSIONS THIS WAS A SEPARATE SUBROUTINE (CALLED DRWDAR). IN THAT
C     .   CASE ,HOWEVER, FX AND FY WERE DEFINED EXTERNAL SINCE THESE
C     .   FUNCTIONS WERE USED BY BOTH DRWSTR AND DRWDAR. IN ORDER TO
C     .   MAKE ALL DEFAULT TRANSFORMATIONS STATEMENT FUNCTIONS I HAVE
C     .   PUT DRWDAR  HERE AND I WILL USE MFLAG TO RETURN  TO THE CORRECT
C     .   LOCATION IN THE CODE.
C     
      IF ( (DELX.EQ.0.) .AND. (DELY.EQ.0.) ) GO TO 50
C     
      CALL SBYTES( UX(I,J) ,IONE , ISKIP , 1 ,0 , 1 )
      D = ATAN2(-DELX,DELY)
      D30 = D+0.5
 170  YY = -AROWL*COS(D30)+Y
      XX = +AROWL*SIN(D30)+X
      call cpmpxy (imap, xx, yy, xout, yout)
      call fl2int (xout, yout, ifxx, ifyy)
ccc      CALL FL2INT (FX(XX,YY),FY(XX,YY),IFXX,IFYY)
      CALL PLOTIT (IFXX,IFYY,1)
      call cpmpxy (imap, x, y, xout, yout)
      call fl2int (xout, yout, ifx, ify)
ccc      CALL FL2INT (FX(X,Y),FY(X,Y),IFX,IFY)
      CALL PLOTIT (IFX,IFY,0)
      IF (D30.LT.D) GO TO 180
      D30 = D-0.5
      GO TO 170
 180  IF (MFLAG.EQ.1) GO TO 110
      IF (MFLAG.EQ.2) GO TO 130
C     
 190  CONTINUE
C     
C     FLUSH PLOTIT BUFFER
C     
      CALL PLOTIT(0,0,0)
      RETURN
      END
