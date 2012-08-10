c     
c $Id: cpline.f,v 1.52 2007-07-30 21:59:09 reinecke Exp $
c
c $Log: cpline.f,v $
c Revision 1.52  2007-07-30 21:59:09  reinecke
c Fixed clipping bug instituted by clipping fix.
c
c Revision 1.51  2007-07-27 19:43:18  warren
c fix clipping in 2d contour plot. I don't know why it was commented out.
c
c Revision 1.50  2007-07-05 19:24:47  warren
c Final commit of everything pre svn
c
c Revision 1.48  2003/04/02 22:25:23  warren
c Revision 1.49  2007-06-21 20:58:30  reinecke
c Commiting local modifications for transfer to svn
c
c Revision 1.48  2003/04/02 22:25:23  warren
c don't plot iaia(0) as this is under the mountain
c
c Revision 1.47  2003/03/27 21:40:58  davidive
c Fixed gif output to work from pixmap and not crash o 2 colors
c
c Revision 1.46  2002/08/09 19:57:56  warren
c Fixed for Linux and tested with DEC sun etc.
c most items inside #ifdef's, or were general cleanup/fixes
c
c Revision 1.45  2002/06/11 17:26:15  harryive
c Change aspect ratio to affect the plot, not just printing.
c
c Add first part of contour value specification.
c
c Revision 1.44  2001/04/04 20:08:25  harryive
c Better handle on constant fields.
c
c Revision 1.43  2001/03/05 23:51:37  davidive
c Fixed the zero_color code to correctly deal with contour values instead of
c deviding by zero after discovering that the number of contours assigned
c was zero. Fixed the line plotting routines to only plot terrain when it
c exists.
c
c Revision 1.42  2001/02/09 00:14:14  davidive
c fixed cpline to use the zero color correctly. - note - a negative is + !!!!
c
c Revision 1.41  2001/01/31 01:03:31  davidive
c Make zero_color mean the first positive interval, not the last negative.
c
c Revision 1.40  2001/01/23 22:47:10  davidive
c cpline.f: fixed to use zero_color
c everything else: First cut at wireframes and isosurfaces. No dimentions yet
c no widgets yet.
c
c Revision 1.39  2000/06/12 18:38:39  harry
c Fix crash with no contours and windowing problem.
c
c Revision 1.38  2000/05/09 19:53:44  harry
c Seperate contour and line colors.  Make patterns work like colors.
c
c Revision 1.37  2000/03/20 20:27:03  harry
c Fix "out of bounds" errors when NCL is 0.
c
c Revision 1.36  2000/01/19 18:49:52  warren
c Doubled ARSCAM work array again for Gretchen's out of space error.
c
c Revision 1.35  1999/12/22 00:33:09  warren
c Fixed Dales crashing bug when changing from point slicer to normal. Also
c have some ifdef linuxes in there in case we ever find a fortran compiler.
c
c Revision 1.34  1999/08/06 22:57:07  warren
c Fixed bug introduced into solid fill cint with features to lock all vars.
c
c Revision 1.33  1999/07/23 20:03:37  warren
c Background code for the lock all fields option.
c
c Revision 1.32  1999/03/01 21:19:28  warren
c Fixed trajectory code to set the correct attributes at plot time instead of
c at traj componant selection time. - now to do vectors.
c
c Revision 1.31  1998/12/18 01:00:19  warren
c Say bye bye to more yuri code we don't use.
c
c Revision 1.30  1998/07/15 20:19:23  harryive
c Fix roundoff and wrap-around errors in seting color table.
c
c Revision 1.29  1998/06/11 21:53:00  harry
c Fix patteren area fill - add additional value to clevels array.
c
c Revision 1.28  1998/06/03 17:54:38  harry
c Fix tick mark labels.   Add new command COLOR_BAR.
c
c Revision 1.27  1997/12/23 22:43:50  warren
c increased MXCRA to 50000 and cleaned up some unused variables and split a
c couple of large fortran if not blocks, so it wouldn't evaluate it all if
c unnecessary.
c
c Revision 1.26  1997/08/08 18:32:14  warren
c Fixed color locking for Dales new bug.
c
c Revision 1.25  1997/07/29 17:31:49  davidive
c Fix number of MXBANDS to equal max levels and run circularly through
c the color table to fix the last color locking bug.
c
c Revision 1.24  1997/07/11 14:07:02  davidive
c Fix color fill when there are no values or interval set to start from the
c middle of the color table again.
c
c Revision 1.23  1997/07/02 15:06:15  harry
c Put clipping back into vtopo.
c
c Revision 1.22  1997/06/06 20:17:54  harry
c Fix color locking - distinguish value from contour interval.
c
c Revision 1.21  1997/04/18 17:41:12  warren
c Actually clear the locking variables when freeing color table due to
c change in contour etc.
c
c Revision 1.20  1997/03/20 22:22:58  harry
c Remove more meaningless yuri stuff.
c
c Revision 1.19  1997/03/20 16:29:22  warren
c yanked unused yuri code to make sgi happy
c
c Revision 1.18  1997/01/29 23:09:59  warren
c Cleaned up and fixed Dales Color problem
c
c Revision 1.17  1996/10/11 01:12:48  warren
c To fill or not to fill is no longer the only question. Now there is a seperate
c question of solid or pattern. Also first part of mouse slicing. 2d -> 1d
c Labels are not fixed yet.
c
c Revision 1.16  1996/08/23 02:12:45  harry
c More color fixes - specific to "interesting" behavior in NCAR graphics.
c
c Revision 1.15  1996/08/01  18:35:04  harry
c Fix color number for the topography contours to 1 from -1.  -1 causes
c crashes with printing.
c
c Revision 1.14  1996/04/08 18:57:10  warren
c first parts of num unlabeled lines for terrain and fixed transform routine
c to use last directory if none is given.
c
c Revision 1.13  1996/04/08 15:31:57  warren
c Draw terrain at the correct time not backwards.
c
c Revision 1.12  1996/04/01 20:23:36  warren
c Terrain form and new terrain command (on / off)
c
c Revision 1.11  1996/02/27 23:15:13  harry
c Fix labels to use new netCDF attributes.
c
c Revision 1.10  1996/02/13 00:17:44  harry
c Remove supposed "fixes" to NCAR graphics that actually make thinks worse.
c
c Revision 1.9  1995/12/15 22:33:15  warren
c moved patterns into 4.0 and implemented the list of fixes and changes from
c Curtis.
c
c Revision 1.8  1995/10/18  19:18:05  warren
c Stipples from 3.3, First crack at mouse windowing and put window_points_
c into a header file and out of the mouse stuff and slicers. Touched almost
c everything it seems.
c
c Revision 1.7  1995/09/05  20:33:40  harry
c Add new slicer.  Good luck!
c
c Revision 1.6  1995/08/28  18:11:01  harry
c Take out unnecessary passing of terrain variables in filled contour plots.
c
c Revision 1.5  1994/12/28  00:00:29  warren
c put entry_used into the common block it should have been in all the time and
c move the setting of it out of the loop it was in.
c
c Revision 1.4  1994/11/09  16:51:59  harry
c Remove internal CONPACK common block.
c
c Revision 1.3  1994/10/08  19:01:23  warren
c The annotation widget and updating it. As well as changing tic_bold to
c tic_labels and adding an off option.
c
c Revision 1.2  1994/09/07  22:07:04  warren
c removed the extra plot from the color table locking sequence.
c
c Revision 1.1  1994/07/08  23:02:40  harry
c Change back to .f and remove Yuri's modified NCAR calls.
c
c Revision 1.10  1994/03/30  22:47:09  yuri
c Added comments and more RIGHT comments.
c More stuff will follow.
c
c Revision 1.9  1994/03/25  23:12:29  harry
c Fixes to plot labelling again.
c
c Revision 1.8  1994/03/25  22:24:33  warren
c first fix to Dales label problem.
c
c Revision 1.7  1994/02/23  00:15:27  warren
c Final 3.1 label changes.
c
c Revision 1.6  1994/02/10  18:10:54  warren
c Fixed missing decimals
c
c Revision 1.5  1994/02/09  01:24:46  warren
c make sure data_display_units is saved and increase MKS in areas calls.
c
c Revision 1.4  1994/02/03  22:42:54  warren
c last of the movie labels and made the tic mark labels smarter.
c
c Revision 1.3  1994/02/01  02:06:59  warren
c Cleaned up line and area fill movie labels somewhat.
c
c Revision 1.2  1994/01/20  23:17:08  yuri
c New format of numbers in averaging messages. (After long long testing).
c
c Revision 1.1  1993/11/29  19:39:06  warren
c Changes to 3.1 to make it work on alphas.
c FORTRAN: new data type pointer ifdefed to either 32 bit or 64 bit.
c C: Change int to long where necessary.
c Files: Quite a few .f files are now .F
c
c Revision 1.35  1993/11/19  18:05:22  yuri
c Propagation of corrections from IVE 3.1.
c Tell conpack in "set2d" that set has already been called.
c
c Revision 1.34  1993/09/03  17:18:37  yuri
c 	Now MXCRA = 20000 to avoid ncar error message.
c
c Revision 1.33  1993/08/26  19:27:26  warren
c Brought over fix from 3.0.2
c
c Revision 1.32  1993/08/24  17:59:17  warren
c Brings current source in line with fixed 3.0.1 version.
c Red mountains -> whith mountains
c
c Revision 1.31  1993/08/18  15:47:12  harry
c Change to new transform routines.
c
c Revision 1.30  1993/07/26  19:11:31  warren
c Fixed new color under mountain problem
c
c Revision 1.29  1993/07/19  20:47:41  warren
c removed unused duplicate declaration of linlog.
c
c Revision 1.28  1993/07/16  17:49:14  yuri
c 	Merging with new color fill  stuff.
c
c Revision 1.27  1993/07/16  17:19:49  yuri
c 	Updated ncar stuff added.
c
c Revision 1.26  1993/07/15  22:13:10  warren
c Fixed scaling for default 1d ticmarks. Also fixed plotting under terrain
c problem.
c
c Revision 1.25  1993/07/09  19:09:26  warren
c Pretty labels to go with the 2d ticmarks. Also I removed superfluous calls
c to chkwin and moved all the others to driver.f.
c
c Revision 1.24  1993/07/09  16:35:26  harry
c Add number of contour values to attributes and change its meaning so
c that a zero or negative number means use contour interval instead.
c
c Revision 1.23  1993/06/29  21:09:15  warren
c Fixed check for constant flag when deciding to put up message that there are
c no contours.
c
c Revision 1.22  1993/06/29  21:02:53  warren
c Attempt to restore constant field plot
c
c Revision 1.21  1993/06/28  19:56:26  warren
c Removed multiple declarations.
c
c Revision 1.20  1993/06/24  22:14:14  yuri
c 	Corrected bugs in overlays scaling + cleaning up.
c
c Revision 1.19  1993/06/23  18:45:48  warren
c fixed so that compact is not entered if there are no contours to draw.
c
c Revision 1.18  1993/06/08  18:14:45  harry
c Clean up from last change.
c
c Revision 1.17  1993/06/08  18:09:58  harry
c Fix problem when nozero is set and there are no contours above zero.
c
c Revision 1.16  1993/06/07  20:43:02  harry
c Fix problems with color fill and label bar, especially when nozero set.
c
c Revision 1.15  1993/06/03  22:39:16  yuri
c colbar is updated for contour levels/intervals.
c
c Revision 1.14  1993/06/03  17:28:37  yuri
c Return to ncar version for a while, sorry.
c
c Revision 1.13  1993/06/01  19:52:08  yuri
c Second attempt.
c
c Revision 1.11  1993/05/24  15:38:03  yuri
c cleaning up.
c
c Revision 1.10  1993/05/24  00:37:47  yuri
c Just clean-up.
c
c Revision 1.9  1993/05/23  23:46:07  yuri
c New dashed/dotted lines patters.
c
c Revision 1.3  1993/04/09  16:04:59  yuri
c 	returned to plchlq routine to make nicer text labels
c
c
c Revision 2.8  1993/03/25  15:53:46  yuri
c fixed to use new contouring routines.
c
c Revision 2.7  1993/03/12  15:53:46  davidb
c added map features
c
c Revision 2.6  1993/02/19  17:30:12  warren
c fixed getting contour int from property sheet for color fill
c
c Revision 2.5  1992/10/22  21:02:32  millerp
c *** empty log message ***
c
c Revision 2.4  1992/10/22  20:35:34  millerp
c Fixed PAI error in looping color filled plots.
c
c Revision 2.3  1992/10/15  23:01:21  warren
c added color table locking
c
c Revision 2.2  1992/09/08  19:53:31  millerp
c Added stuff for new slicers.
c
c Revision 2.1  1992/08/25  23:29:32  warren
c fixed color allocation problem and added fallback to smaller color table
c
c Revision 2.0  1992/07/01  21:12:46  millerp
c Released version rereving to 2.0
c
c Revision 1.5  1992/06/15  18:59:42  millerp
c Change argument list for scale.
c
c Revision 1.4  1992/06/10  20:07:17  millerp
c Re-do scaling, fix cptopo.
c
c Revision 1.3  1992/05/29  22:31:49  millerp
c Added units stuff.
c
c Revision 1.2  1992/05/07  20:03:04  millerp
c Increased workspace for gradients and contours. When large arrays are
c contoured NCAR needs this extra space or line labels may not be
c drawn.
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
      subroutine ypline(rkx,rky,n,value)
c-----------------------------------------------------------------------
c	low-lovel routine to plot contour line with
c	label somewhere on it.
c	currently not used.
c-----------------------------------------------------------------------
      integer n,i
      real rkx(n),rky(n), value
      real vleft, vright, vbot, vtop, 
     & wleft_c, wright_c, wbot_c, wtop_c
      integer axes
      real cufx,cufy
c
c-----------------------------------------------------------------------
c
      include 'plotops.icl'
      include 'color.icl'
c
c-----------------------------------------------------------------------
c

      call getset(vleft, vright, vbot, vtop, 
     &     wleft_c, wright_c, wbot_c, wtop_c, axes)
      do i=1,n
         if(axes .eq. 2) then
            rky(i) = 10** (rky(i))
         elseif(axes .eq. 3) then
            rkx(i) = 10**( rkx(i) )
         elseif(axes .eq. 4) then
            rky(i) = 10** (rky(i))
            rkx(i) = 10**( rkx(i) )
         endif
         rky(i) = cufy( rky(i) ) 
         rkx(i) = cufx( rkx(i) )
      enddo

      call set(vleft, vright, vbot, vtop, 
     &     vleft, vright, vbot, vtop, 1)

      if( wbot_c .gt. wtop_c)then
        do i=1,n
            rky(i) = vtop -rky(i) + vbot
         enddo
       endif
      if( wright_c .lt. wleft_c)then
         do i=1,n
            rkx(i) =  vright - rkx(i) + vleft
         enddo
      endif

      call  gslwsc (linwdth)
      if(value .lt. 0) then
         call dashdb (lopattern)
         call gsplci (locolor)
         if(lopattern .eq. 65535) then
            call  curve(rkx,rky,n)
          else
           call  curved(rkx,rky,n)
        endif
      else
         call dashdb (hipattern)
         call gsplci (hicolor)
         if(lopattern .eq. 65535) then
            call  curve(rkx,rky,n)
          else
           call  curved(rkx,rky,n)
        endif
      endif
C     restore old params
C
         call set(vleft, vright, vbot, vtop, 
     &     wleft_c, wright_c, wbot_c, wtop_c, axes)

C
C Done.
C
      return
      end
c----------------------------------------------------------------------
      SUBROUTINE CURVED (X,Y,N)
C USER ENTRY POINT.
C
      real x,y
      integer i,j,n, iixbeg,iiybeg,iixcur,iiycur
      integer nn
      DIMENSION X(N),Y(N)
C
      nn = n
         i=1
            do j=n-1,4,-1
          if(abs(x(i) - x(j)) .le. 1e-5 .and.
     &       abs(y(i) - y(j)) .le. 1e-5 .and.
     &     i .ne. j  ) nn = j
            enddo

      CALL FRSTD (X(1),Y(1))
      CALL FL2INT (X(1),Y(1),IIXbeg,IIYbeg)
      DO 10 I=2,Nn
         CALL FL2INT (X(i),Y(i),IIXcur,IIYcur)

            CALL VECTD (X(I),Y(I))
            iixbeg = iixcur
            iiybeg = iiycur
c         endif
   10 CONTINUE
      CALL LASTD
C
      RETURN
      END
c---------------------------------------------------------------------
      subroutine  cptopo (dat, l, m, n, error)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine produces a contour line plot for terrain.
c     Arguments:
c        dat   real  input  the origin of data array to be contoured.
c        l     int   input  the first dimension of dat
c        m     int   input  the number of points to be contoured in 
c                           the first subscript direction.
c        n     int   input  the number of points to be contoured in 
c                           the second subscript direction.
c        error  logi  output  indicates possible errors encountered in
c                             this routine.
c                             error = true    error detected.
c                             error = false   no errors detected.
c-----------------------------------------------------------------------
c
      include 'work.icl'
      include 'plotops.icl'
      include 'conre.icl'
      include 'color.icl'
c
c                       
c     Argument declarations.
c
      integer        l, m, n
      logical        error
      real           dat
      dimension dat(l, n)
c
c                       
c     Local variable declarations.
c
      integer        clu, cff, HCLCLR, i, iasf, ierr, LCLCLR, MXIASF, 
     &               ncl, oldclp, qual
      real           clv, crec
      real            ORVAL
      parameter ( ORVAL = 1.0e12 )
      parameter      (MXIASF = 13 , 
     &                HCLCLR = 1 , 
     &                LCLCLR = 1 )
      dimension      crec(4), iasf(MXIASF)
c     Initialization of GKS aspect source flags.
      data iasf / MXIASF*1 /
c
c
c
c
c
c
      error = .false.
      if (.not. terrflg)return
c
c     
c     Turn on clipping indicator.
c
      call gqclip (ierr, oldclp, crec)
      call gsclip (1)
c
c
c     Set all aspect source flags to "individual".
c
      call gsasf (iasf)
c
c      call pcgeti ('QU - quality flag', qual)
c     
c     Use medium quality characters for Text_soli strings.
c
c      call pcseti ('QU - quality flag', 1)
c
c
c
c     Set selected CONPACK internal parameters.
c
c     Set contour interval.
c
      if (tnum_vals .le. 0) then
C
C     This means nooone gave us values
C
         call cpsetr ('CIS - contour interval specfier', tcint)
c
c     Tell conpack that set has already been called.
c
         call cpseti ('SET - do-set-call flag', 0)

      endif
c
c     Set special value.
c
      call cpsetr ('SPV - special value', spval)
c
c     Turn off highs and lows.
c
      call cpsetc ('HLT - high/low label text string', ' ')
c
c     Produce larger characters.
c
      call cpsetr ('CWM - character width multiplier', 1.25)
c
c     Use conpack out-of-range feature to avoid unwanted contour lines.
c     Note that this particular value is used because the EZMAP routines
c     use it by default.
c
      call cpsetr ('ORV - out-of-range value', ORVAL)
c
c
c     Set up workspace used in line labeling. Use large values to 
c     insure that labels get drawn on plots with lots of points. 
c     It these values are too low, labels may not appear on plots
c     of large data arrays.
c
      call cpseti ('RWG - real workspace for gradients', m*n)
      call cpseti ('RWC - real workspace for contours', 1000)
c
c
c     Initialize drawing of the contour plot.
c
      call cprect (dat, l, m, n, rwrk1, MXRWRK, iwrk1, MXIWRK)
c
c        Draw contour lines only if a constant field was not
c        detected.
c
      call cpgeti ('CFF - constant field found flag', cff)
c
c
c     Select contour lines; if the single value flag has been set,
c     the single contour value cint is to be plotted. Otherwise, 
c     conpack chooses contour values.
c
      if (tnum_vals .eq. 1) then
         ncl = 1
         call cpseti ('NCL - number of contour levels', 1)
         call cpseti ('PAI - parameter array index', 1)
         call cpsetr ('CLV - contour level value', tcont_values(1))
      elseif (tnum_vals .le. 0) then
         call cppkcl (dat, rwrk1, iwrk1)
c
c        Get the number of contour levels.
c     
         call cpgeti ('NCL - number of contour levels', ncl)
      else
         call cpseti ('NCL - number of contour levels', tnum_vals)
         do 1000 ncl = 1, tnum_vals
            call cpseti ('PAI - parameter array index', ncl)
            call cpsetr ('CLV - contour level value', tcont_values(ncl))
 1000    continue
         ncl = tnum_vals
      endif
c
c
c     Get the number of contour levels.
c     
      call cpgeti ('NCL - number of contour levels', ncl)

      if ( cff .eq. 0 .and. ncl .gt. 0) then
c
c
c     Select contour levels.
c
         call cppkcl (dat, rwrk1, iwrk1)
c
c
c     Get the number of contour levels.
c     
         call cpgeti ('NCL - number of contour levels', ncl)
c
c     Set contour line types and line colors.
c
         call gsplci (1)
c
c
c     Set internal parameters affecting the appearance of each contour 
c     line.
c
         do 10 i = 1, ncl
            call cpseti ('PAI - parameter array index', i)
            call cpgetr ('CLV - contour level values', clv)
            call cpgeti ('CLU - contour level use flag', clu)
c     Set line contour line width.
            call cpsetr ('CLL - contour line line width', 1)
            if ( terr_nulbll .eq. 0 ) then
c     Turn off contour line labels.
               call cpseti ('CLU - contour level use flag', 1)
            else
c     Label contour lines according to the value of terr_nulbll.
               if ( mod(i, terr_nulbll) .eq. 0 ) then
c     Label line.
                  call cpseti ('CLU - contour level use flag', 3)
               else
c     Don't label line.
                  call cpseti ('CLU - contour level use flag', 1)
               endif
            endif
            if (clv .lt. 0) then
c     Use high contour line pattern and color.
               call cpseti ('CLD - contour line dash pattern',lopattern)
               call cpseti ('CLC - contour line color', LCLCLR)
            else if (clv .gt. 0) then
c     Use low contour line pattern and color.
               call cpseti ('CLD - contour line dash pattern',hipattern)
               call cpseti ('CLC - contour line color', HCLCLR)
            else
c     Use high contour line pattern and color for the zero
c     contour line.
               call cpseti ('CLD - contour line dash pattern',hipattern)
               call cpseti ('CLC - contour line color', HCLCLR)
            end if
 10      continue
c
c
c
c
         call cpcldr (dat, rwrk1, iwrk1)
      endif
c
c
c     Reset quality flag and clipping to their former values.
c
c      call pcseti ('QU - quality flag', qual) 
      call gsclip (oldclp)
c
c
      return
      end 
c
      subroutine vtopo (topo, topopt, npts)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine plots terrain on vertical contour plots.
c     Arguments:
c        topo    real  input  array containing topography heights.
c        topopt  real  input  array containing the location of the 
c                             points in topo.
c        npts    int   input  the number of points in topo and topopt.
c-----------------------------------------------------------------------
c
c
c     Argument declarations.
c
      integer   npts
      real      topo, topopt
      dimension topo(npts), topopt(npts)
c
c
c     Local variable declarations.
c
      integer   BACKGROUND, SOLID_LINE
      parameter (BACKGROUND = 1, SOLID_LINE = 65535)
      integer   ierr, oldclp, oldcolor
      real      crec
      dimension crec(4)
c
      include 'plotops.icl'
c
      if (.not. terrflg)return
c
c
c
c
c     Turn clipping on.
c
      call gqclip (ierr, oldclp, crec)
      call gsclip (1)
c
c
c     Set line type to solid_line and color to the background color.
c
      call dashdb (SOLID_LINE)
      call gqplci (ierr, oldcolor)
      call gsplci (BACKGROUND)
c
c     Draw the terrain curve.
c
      call curved (topopt, topo, npts)
c
c
c     Reset clipping and color.
c
      call gsclip (oldclp)
      call gsplci (oldcolor)
c
c
      return
      end
C
C

C-----------------------------------------------------------------
      subroutine  yuri_erase( xn1, yn1, xn2, yn2, string_length)
C-----------------------------------------------------------------
c	Yuri: I was using it to maker blank space for contour
c	labels. Not any more.
C-----------------------------------------------------------------
      real pointsx(4),pointsy(4)
      real vx,vy
      real xn1,xn2,yn1,yn2, scx,scy
      integer string_length

      scx = 0.0233
      scy = 0.006

      scx = scx / 4. * float(string_length) 

      vx = xn2-xn1
      vy = yn2-yn1

      vx = vx/sqrt((xn2-xn1)**2 + (yn2-yn1)**2 + 1e-20)
      vy = vy/sqrt((xn2-xn1)**2 + (yn2-yn1)**2 + 1e-20)

      pointsx(1) = xn1+scx*vx-scy*vy
      pointsy(1) = yn1+scx*vy+scy*vx

      pointsx(2) = xn1+scx*vx+scy*vy
      pointsy(2) = yn1+scx*vy-scy*vx

      pointsx(3) = xn1-scx*vx+scy*vy
      pointsy(3) = yn1-scx*vy-scy*vx

      pointsx(4) = xn1-scx*vx-scy*vy
      pointsy(4) = yn1-scx*vy+scy*vx
      call gsfais(1)
c   white = 2 red=3 6 = green 
      call gsfaci(2)
      call gfa(4,pointsx,pointsy)
      call gsfaci(1)
      RETURN
C
      END
c-----------------------------------------------------------------------
      subroutine cpline (dat, l, m, n, error)
c     Purpose:
c        This routine produces a contour line plot.
c     Arguments:
c        dat   real  input  the origin of data array to be contoured.
c        l     int   input  the first dimension of dat
c        m     int   input  the number of points to be contoured in 
c                           the first subscript direction.
c        n     int   input  the number of points to be contoured in 
c                           the second subscript direction.
c        error  logi  output  indicates possible errors encountered in
c                             this routine.
c                             error = true    error detected.
c                             error = false   no errors detected.
c-----------------------------------------------------------------------
c
       include 'work.icl'
       include 'plotops.icl'
       include 'conre.icl'
       include 'color.icl'
       include 'ph2ll.icl'
       include 'attributes.icl'
       include 'message.icl'
c     
c                       
c     Argument declarations.
c
      integer        l, m, n
      logical        error
      real           dat
      dimension dat(l, n)
c
c                       
c     Local variable declarations.
c
      integer        clu, i, iasf, ierr, MXIASF, 
     &               ncl, nulbllp1, oldclp, qual, cff
      real           clv, crec,arrlev(259)
      parameter      (MXIASF = 13)
      dimension      crec(4), iasf(MXIASF)
c
c     Initialization of GKS aspect source flags.
      data iasf / MXIASF*1 /
c
c
      external cpdrpl

c
c
c     Turn on clipping indicator.
c
      call gqclip (ierr, oldclp, crec)
      call gsclip (1)
c
c
c     Set all aspect source flags to "individual".
c
      call gsasf (iasf)
c
c      call pcgeti ('QU - quality flag', qual)
c     
c     Use medium quality characters for text strings.
c
c      call pcseti ('QU - quality flag', 1)
c
c
cpmpxyc
c     Set selected CONPACK internal parameters.
      cint  = cont_int
      if(no_zero) then
         nozero = .true.
      else
         nozero = .false.
      endif
c
c     Set contour interval.
c
      if (num_vals .le. 0) then
c
C        this means noone gave us values
c     
         call cpsetr ('CIS - contour interval specfier', cint)
c
c        Tell conpack that set has already been called.
c
         call cpseti ('SET - do-set-call flag', 0)
c
      endif
c
c     Set special value.
c
      call cpsetr ('SPV - special value', spval)
c
c     Turn off highs and lows, and informational label.
c
cc      call cpsetc ('HLT - high/low label text string', ' ')
cc      call cpsetc ('ILT - informational label text string', ' ')
c
c     Produce larger contour line labels.
c
      call cpsetr ('CWM - character width multiplier', 1.25)
c
c
c     Set up workspace used in line labeling. Use large values to 
c     insure that labels get drawn on plots with lots of points. 
c     It these values are too low, labels may not appear on plots
c     of large data arrays.
c
      call cpseti ('RWG - real workspace for gradients', m*n)
      call cpseti ('RWC - real workspace for contours', 1000)
c
c
c     Initialize drawing of the contour plot.
c
      call cprect (dat, l, m, n, rwrk1, MXRWRK, iwrk1, MXIWRK)
      call cpgeti ('CFF - constant field found flag', cff)
c
c
c     Select contour lines; if the single value flag has been set,
c     the single contour value cint is to be plotted. Otherwise, 
c     conpack chooses contour values.
c
      if (num_vals .eq. 1) then
         ncl = 1
         call cpseti ('NCL - number of contour levels', 1)
         call cpseti ('PAI - parameter array index', 1)
         call cpsetr ('CLV - contour level value', cont_values(1))
      elseif (num_vals .le. 0) then
         call cppkcl (dat, rwrk1, iwrk1)
c
c        Get the number of contour levels.
c     
         call cpgeti ('NCL - number of contour levels', ncl)
      else
         call cpseti ('NCL - number of contour levels', num_vals)
         do 1000 ncl = 1, num_vals
            call cpseti ('PAI - parameter array index', ncl)
            call cpsetr ('CLV - contour level value', cont_values(ncl))
 1000    continue
         ncl = num_vals
      endif
c
c
c     Get the number of contour levels.
c     
      call cpgeti ('NCL - number of contour levels', ncl)

      if(ncl .eq. 0 .and. cff .eq. 0) then
         write (message, 110 ) ncl
         call write_message
         error = .true.
         return
      endif

c
c     Set contour line types and line colors.
c
      call gsplci (1)
c
      nulbllp1 = nulbll + 1
c
c
c     Set internal parameters affecting the appearance of each contour 
c     line.
c
      do 10 i = 1, ncl
         call cpseti ('PAI - parameter array index', i)
         call cpgetr ('CLV - contour level values', clv)
         arrlev(i)=clv
         call cpgeti ('CLU - contour level use flag', clu)
c        Set line contour line width.
         call cpsetr ('CLL - contour line line width', linwdth)
         if ( cllab ) then
c           Label contour lines according to the value of nulbll.
            if ( mod(i, nulbllp1) .eq. 0 ) then
c              Label line.
               call cpseti ('CLU - contour level use flag', 3)
            else
c              Don't label line.
               call cpseti ('CLU - contour level use flag', 1)
            endif
         else
c           Turn off all contour line labels.
            call cpseti ('CLU - contour level use flag', 1)
         endif
         if (clv .lt. 0) then
c           Use low contour line pattern and color.
            call cpseti ('CLD - contour line dash pattern', lopattern)
            call cpseti ('CLC - contour line color', locolor)
         else if (clv .gt. 0) then
c           Use high contour line pattern and color.
            call cpseti ('CLD - contour line dash pattern', hipattern)
            call cpseti ('CLC - contour line color', hicolor)
         else
            if ( nozero ) then
c              Do not draw the zero contour line.
               call cpseti ('CLU - contour level use flag', 0)
            else
c              Use high contour line pattern and color for the zero
c              contour line.
               call cpseti ('CLD - contour line dash pattern',
     &              hipattern)
               call cpseti ('CLC - contour line color', hicolor)
            end if
         end if
 10   continue
c
c
c     Draw contour lines.
c the ph2ll_flag is set to 0 to omit ph2ll
      ph2ll_flag=0

      call cpinrc
c
      call cpcldr (dat, rwrk1, iwrk1)
c
c
c     Reset quality flag and clipping to their former values.
c
c      call pcseti ('QU - quality flag', qual) 
      call gsclip (oldclp)
c
c
 110  format (1x, 
     &     'This contour interval or levels produces no contours ',i4)
      return
      end 
c
c
c---------------------------------------------------------------------
c
      subroutine cpsoli (dat, l, m, n, error)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine produces a color-filled contour plot.
c     Arguments:
c        dat   real  input  the origin of data array to be contoured.
c        l     int   input  the first dimension of dat
c        m     int   input  the number of points to be contoured in 
c                           the first subscript direction.
c        n     int   input  the number of points to be contoured in 
c                           the second subscript direction.
c        error  logi  output  indicates possible errors encountered in
c                             this routine.
c                             error = true    error detected.
c                             error = false   no errors detected.
c-----------------------------------------------------------------------
c
      include 'work.icl'
      include 'plotops.icl'
      include 'conre.icl'
      include 'color.icl'
      include 'message.icl'
      include 'attributes.icl'

c
c                       
c     Argument declarations.
c
      integer        l, m, n
      logical        error
      real           dat, cmax
      dimension dat(l, n)
c
c                       
c     Local variable declarations.
c
      integer        i, j, k, iasf, ierr, MXIASF, 
     &               ncl, nlevs, oldclp, qual
      integer        cff,iaia, igia
      integer        MXIA, MXCRA, skipclr
      integer        MXLEVS
      real           xcra, ycra
      real           clevel, clv, crec, cval
      parameter      (MXIASF = 13)
      parameter      (MXIA  = 1000, 
     &                MXCRA = 25000000,
     &                MXLEVS = 300  )
c     &                MXLEVS = 150  )
      dimension      crec(4), iasf(MXIASF)
      dimension      iaia(MXIA), igia(MXIA), xcra(MXCRA), ycra(MXCRA),
     &               clevel(MXLEVS)
      real arrlev(259), level1
      common /contour_levels/ clevel


c
c
      external colram
c
c
c     Initialization of GKS aspect source flags.
      data iasf / MXIASF*1 /
c
c
c
c
      error = .false.
c
c
c     Turn on clipping indicator.
c
c      call gqclip (ierr, oldclp, crec)
c      call gsclip (1)
c
c
c     Set all aspect source flags to "individual".
c
      call gsasf (iasf)
c
c      call pcgeti ('QU - quality flag', qual)
c     
c     Use medium quality characters.
c
c      call pcseti ('QU - quality flag', 1)
c
c
c     Set selected CONPACK internal parameters.
c
c     Set contour interval.
c      if(ignore_lock_var .ne. 0) then
         cint  = cont_int
c      else
c         cint = cont_int_lock_var
c      endif
      if(no_zero) then
         nozero = .true.
      else
         nozero = .false.
      endif
c

c      print*,'CIS - contour interval specfier', cint
      call cpsetr ('CIS - contour interval specfier', cint)
c
c
c     Tell conpack that set has already been called.
c
      call cpseti ('SET - do-set-call flag', 0)
c
c     Set special value.
c
      call cpsetr ('SPV - special value', spval)
c
c     Turn off highs and lows.
c
      if (filtyp .ne. PATTERN) then
         call cpsetc ('HLT - high/low label Text_soli string', ' ')
      endif
c
c
c     Set up workspace used in line labeling. Use large values to 
c     insure that labels get drawn on plots with lots of points. 
c     It these values are too low, labels may not appear on plots
c     of large data arrays.
c
      call cpseti ('RWG - real workspace for gradients', m*n)
      call cpseti ('RWC - real workspace for contours', 1000)
c
c
c     Initialize drawing of the contour plot.
c
      call cprect (dat, l, m, n, rwrk1, MXRWRK, iwrk1, MXIWRK)
c
c     Produce the constant field message if appropriate.
c
      call cpgeti ('CFF - constant field flag', cff)
c
      if ( cff .ne. 0 ) then
c
c        Print the constant field message.
c
         call cpcldr (dat, rwrk1, iwrk1)
         ncl=0
      else
c
c
c     Select contour lines; if the single value flag has been set,
c     the single contour value cint is to be plotted. Otherwise, 
c     conpack chooses contour values.
c
         if ( single ) then
            call cpseti ('NCL - number of contour levels', 1)
            call cpseti ('PAI - parameter array index', 1)
            call cpsetr ('CLV - contour level value', cint)
         elseif ( num_vals .gt. 0) then
            ncl = num_vals
            nlevs  = num_vals
            call  cpseti ('NCL - number of contour levels', ncl)
            do i=1, num_vals
               clevel(i) = cont_values (i)
               call cpseti ('PAI - parameter array index', i)
               call cpsetr ('CLV - contour level values', clevel(i) )
               arrlev(i) = clevel(i)
            enddo
         else   
            call cppkcl (dat, rwrk1, iwrk1)
         end if
c
c
c     Get the number of contour levels.
c     
         call cpgeti ('NCL - number of contour levels', ncl)
c
         if ( ncl .gt. MXLEVS ) then
            write (message, 100 ) ncl
            call write_message
            error = .true.
            return
         endif
c
c
         nlevs = 0
c
         nbands = ncl + 1
         if ( nbands .gt. MXBANDS ) then
            write (message, 110) nbands
            call write_message
            nbands = 0
            return
         endif
         skipclr = (ncl+10)
         do 10 i = 1, ncl
            nlevs = nlevs + 1
            call cpseti ('PAI - parameter array index', i)
            call cpgetr ('CLV - contour level values', clv)
            if(cint .eq. 0) then
               call cpgetr ('CIU - Contour Interval Used', cont_used)
            else 
               cont_used = cint
            endif
            clevel(nlevs) = clv
            arrlev(i) = clv
            if (filtyp .eq. PATTERN) then
               bandclr(i) = i
            else
               if (i .eq. 1) then
                  call cpgetr('CIU', cval)
                  if (lock_entry .ne. 0) then
                     if (lock_set_cint.gt.0 .and. num_vals .gt. 0) then
                        k = 0
                        do j = 1, num_vals
                           if (cont_values(j) .eq. midval) k = 1
                        enddo
                        if (k .eq. 0) then
                           lock_field = ' '
                           lock_entry = 0
                           lock_int = 0.
                        else
                           bandclr(1) = 
     &                          (lock_entry - nint((midval - clv)/cval))
                        endif
                     else if (lock_int .eq. cint) then
                        bandclr(1) = 
     &                       (lock_entry - nint((midval - clv)/cint))
                     else
                        lock_field = ' '
                        lock_entry = 0
                        lock_int = 0.
                        bandclr(1) = (max_user_color - min_user_color)/2
     &                       + min_user_color - (ncl/2 - 1)
                  
                     endif
                  else
                     if(nozero) then
                        if(num_vals .gt. 0) then
                           bandclr(1) = zero_color - (num_vals/2 - 1)
                        else
                           bandclr(1) = zero_color - (ncl/2 - 1)
                        endif
                     else
                        if(num_vals .gt. 0) then
                           k=1
                           do while(k .lt. num_vals .and. 
     &                          cont_values(k) .lt. 0)
                              k = k + 1
                           enddo
                           bandclr(1) = zero_color - (i -1)
                        else
                           bandclr(1) = zero_color -1+int(clv/cont_used)
                        endif
c     Really first positive color int 0 -> xxx
                     endif
                  endif
                  do while (bandclr(1) .lt. min_user_color) 
                     bandclr(1) = max_user_color -abs(min_user_color-1-
     &                    bandclr(1))
                  enddo
                  do while (bandclr(1) .gt. max_user_color) 
                     bandclr(1) = min_user_color + abs(bandclr(1) -
     &                    max_user_color - 1)
                  enddo
               else if (i.eq.skipclr+1) then
                  bandclr(i) = bandclr(i-1)
               else
                  bandclr(i) = bandclr(i-1) + 1
               endif
               do while (bandclr(i) .lt. min_user_color) 
                  bandclr(i) = max_user_color -abs(min_user_color - 1 -
     &                 bandclr(i))
               enddo
               do while (bandclr(i) .gt. max_user_color) 
                  bandclr(i) = min_user_color + abs(bandclr(i) -
     &                 max_user_color - 1)
               enddo
c               write(6,*)i,bandclr(i)
            endif
c
c        Do not draw contour lines.
c
            if(filtyp .eq. PATTERN) then
               call cpseti ('CLU - contour level use flag', 1) 
            else
               call cpseti ('CLU - contour level use flag', 0) 
            endif
            if ( (clv .eq. 0.0) .and. nozero ) then
c
c           Turn off area identifiers so that zero contour is not 
c           added to area map.
c
               call cpseti ('AIA - area identifier above level',0)
               call cpseti ('AIB - area identifier below level',0)
c
c           Reset nlevs so that zero contour band is not shown on
c           color bar.
c
               nlevs = nlevs - 1
c
c           Set a flag telling colbar which color index to skip when
c           constructing the color bar.
c
               skipclr = i
            end if
 10      continue
         if (ncl .gt. 0) then
            if (filtyp .eq. PATTERN) then 
               if (num_vals .le. 0) then
                  clevel(ncl+1) = clevel(ncl)
     &                 + (clevel(ncl)-clevel(ncl-1))
                  bandclr(ncl+1) = ncl+1
               else
                  cmax = clevel(1)
                  do i = 2, ncl
                     if (cmax .lt. clevel(i)) cmax = clevel(i)
                  enddo
                  clevel(ncl+1) = cmax + 1
               endif
            endif
            if ((clv+cval .eq. 0) .and. nozero) then
               skipclr = ncl+1
            endif
            if (ncl.eq.skipclr) then
               bandclr(ncl+1) = bandclr(ncl)
            else
               bandclr(ncl+1) = bandclr(ncl) + 1
            endif
         endif
c
 11      continue
c
c     Initialize area map and place contour lines in area map.
c
         call arinam (iama, MXIAMA)
         call cpclam (dat, rwrk1, iwrk1, iama)
c
c
c     Color fill area map.
c
c     Set GKS to do solid fill.
c
         call gsfais (1)
         call arpram (iama, 0, 0, 0)
         call arscam (iama, xcra, ycra, MXCRA, iaia, igia, MXIA, colram)
c
c        Construct color bar.
c
         if (color_bar) then
            call colbar (nlevs, clevel, MXLEVS, nozero, skipclr, error)
         endif
      endif
c
c
c     Reset quality flag and clipping to their former values.
c
c      call pcseti ('QU - quality flag', qual) 
c      call gsclip (oldclp)
c
c
c     get midval for locking
         if(lock_entry .eq. 0 .and. ncl .gt. 1) then
            midval = clevel(ncl/2)
            entry_used =  bandclr(ncl/2)  
            if(cont_int .le. 0) then
               call cpgetr ('CIU - Contour Interval Used', cont_used)
            endif
         endif
c     

 100  format (1x, 'The parameter MXLEVS in cpsoli must be at least ',i4)
 110  format (1x, 'The parameter MXBNDS must be at least ',i4)
c
      return
      end 
c
c-----------------------------------------------------------------------
      subroutine colram (xcra, ycra, ncra, iaia, igia, naia)
c-----------------------------------------------------------------------
c     Purpose: 
c        This subroutine colors an area map. See NCAR GRAPHICS guide, 
c        Version 3.00 for details.
c     Arguments:
c       xcra  real  input  contains the x coordinates of points defining
c                          a polygon to be filled.
c       ycra  real  input  contains the y coordinates of points 
c                          defining a polygon to be filled.
c       ncra  int   input  the number of points in xcra and ycra.
c       iaia  int   input  array containing area identifiers.
c       igia  int   input  array containing group identifiers.
c       naia  int   input  dimension of iaia and igia
c-----------------------------------------------------------------------
c
      include 'color.icl'
      include 'message.icl'
      include 'plotops.icl'
c
c     Argument declarations.
c
      integer ncra, iaia, igia, naia, MXLEVS, set_pattern
      parameter (MXLEVS = 300)
      real    xcra, ycra
      real clevel
      dimension  xcra(ncra), ycra(ncra), iaia(naia), igia(naia),
     &     clevel(MXLEVS)
      common /contour_levels/ clevel
c
c     Local variable declarations.
c
      integer i, ifill
c
c     Flag determining whether or not we fill any given polygon. Ifill 
c     greater than or equal to 1 indicates that a polygon will be 
c     filled.
c
      ifill = 1
c
      do 10 i = 1, naia
c
c        If an area identifier is negative, don't fill the polygon.
c
c         write(6,*)'IAIA = ',iaia(i)
         if ( iaia(i) .lt. 0 ) then
            ifill = 0
         endif
 10   continue
c
      if ( ifill .ne. 0 ) then
c
c        Fill a polygon in the color implied by it's area identifier
c        relative to edge group 3, the contour-line group.
c
         if (filtyp .eq. PATTERN) then
            call gsfaci(1)
            do  i = 1, naia
               if ( (igia(i) .eq. 3) .and. (iaia(i) .gt. 0)) then
                  if (set_pattern(clevel(iaia(i))) .gt. 0) then 
                     call gfa (ncra - 1, xcra, ycra)
                  endif
               endif
            enddo
         else
            do  i = 1, naia
               if ( (igia(i) .eq. 3) .and. (iaia(i) .gt. 0)) then
                  call gsfaci(bandclr(iaia(i)))
                  call gfa (ncra - 1, xcra, ycra)
c               elseif (igia(i) .eq. 3) then
c                  call gsfaci(1)
c                  call gfa (ncra - 1, xcra, ycra)
               endif
            enddo
         endif
      endif
c
c
      return
      end
c
      subroutine colbar (nlevs, clevel, mxlevs, nozero, skipclr, error)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine constructs the color bar for color-filled
c        plots.
c     Arguments:
c        nlevs   int   input   the number of contour levels in the plot.
c        clevel  real  input   array containing contour level values.
c        mxlevs  int   input   the maxumum dimension of clevel.
c        error   logi  output  indicates possible errors encountered in
c                              this routine.
c                              error = true    error detected.
c                              error = false   no errors detected.
c-----------------------------------------------------------------------
c
      include 'color.icl'
      include 'units.icl'
c
c     Argument declarations.
c
      integer    mxlevs, nlevs, skipclr
      logical    nozero, error
      real       clevel(mxlevs)
c
c
c     Local variable declarations.
c
      character *(80) ftoa
      integer       i, ierr, linlog, iend, zero_color
     &              oldclp, qual,outnum,outsig,outpow
      logical       movielabs, mapflg
      real          vpl, vpr, vpb, vpt, wdl, wdr, wdb, wdt
      real          cint, crec, xmajor, ymajor
      dimension     crec(4)
c
c
      integer strend
c
c
c
      error = .false.
      if ( nbands .eq. 0 ) return
c
c
      call getlvar('movielabs', movielabs, error)
      if(error) then
         return
      endif
c
c
c     Turn off clipping so label bar can be placed outside plot
c     window.
c
c      call gqclip (ierr, oldclp, crec)
c      call gsclip (0)
c
c
c      call pcgeti ('QU - quality flag', qual)
c     
c     Use medium quality characters for text strings.
c
c      call pcseti ('QU - quality flag', 1)
c
c
c     Get current set call for use in positioning label bar.
c
      call getset (vpl, vpr, vpb, vpt, wdl, wdr, wdb, wdt, linlog)
c
c
c
c     Fill the arrays containing the list of color bar labels and the 
c     color indicies of each box in the color bar. 
c
c
c
      call cpinrc
c     Scale labels if necessary.
c
c      call cpgetr ('CIS - contour interval specifier', cint)
      call cpgetr ('CIU - contour interval used', cint)
c      call scale (cint, 1, data_slope, data_intercept, 0.0)
c
c      call scale (clevel(1), 1, data_slope, data_intercept, 0.0)
      if(cint .ne. 0)then
	call cpnumb(clevel(1) - cint,4,-10000,4,-1,'E',' ',' ',1,0,0,0,
     &              1,1,barlab(1),outnum,outsig,outpow)
        call clean_zero(barlab(1))
c         call cpsetr ('ZDV - z data value', clevel(1) - cint)
c         call cpgetc ('ZDV - z data value', barlab(1))
      else
         barlab(1) = ' '
      endif
c
      do 10 i = 1, nlevs
         if (i .ge. skipclr) then
            bandclr(i) = bandclr(i+1)
         endif
c         if ( i .gt. 1 ) then
c            call scale (clevel(i), 1, data_slope, data_intercept, 0.0)
c         endif
c         call cpsetr ('ZDV - z data value', clevel(i))
c         call cpgetc ('ZDV - z data value', barlab(i + 1))
	call cpnumb(clevel(i),4,-10000,4,-1,'E',' ',' ',1,0,0,0,
     &              1,1,barlab(i + 1),outnum,outsig,outpow)
        call clean_zero(barlab(i + 1))
 10   continue
c
      if (nlevs+1 .ge. skipclr) then
         bandclr(nlevs+1) = bandclr(nlevs+2)
      endif
c      call scale (clevel(1), 1, data_slope, data_intercept, 0.0)
      if(cint .ne. 0 .and. nlevs .gt. 0)then
         if (skipclr.eq.nlevs+1) then
            call cpnumb(clevel(nlevs) + cint + cint,4,-10000,4,-1,
     &                  'E',' ',' ',1,0,0,0,
     &                  1,1,barlab(i + 1),outnum,outsig,outpow)
         else
            call cpnumb(clevel(nlevs) + cint,4,-10000,4,-1,
     &                  'E',' ',' ',1,0,0,0,
     &                  1,1,barlab(i + 1),outnum,outsig,outpow)
         endif
c         call cpgetc ('ZDV - z data value', barlab(nlevs + 2))
         call clean_zero(barlab(i + 1))
      else
         barlab(nlevs + 2) = ' '
      endif
c
c
c
      call getrvar('xmajor', xmajor, error)
      call getrvar('ymajor', ymajor, error)
      call getlvar('mapflg', mapflg, error)
c
c     fix bug in lblbar
c
      call gstxci(1)
      call gsplci (1)
      if (mapflg .or. xmajor .gt. 0 .or. ymajor .gt. 0) then
         if(movielabs) then
            call lblbar (1, vpr + .0095, 
     &                   1.0-((vpr - vpl)*0.009)+.01, 
     &                   vpb, vpt, nlevs+1, 0.3, 
     &        1.0, bandclr, 1, barlab, nlevs+2, 1)
         else
            call lblbar (1, vpr + .0095, 
     &                   1.0-((vpr - vpl)*0.009), 
     &                   vpb, vpt, nlevs+1, 0.3, 
     &        1.0, bandclr, 1, barlab, nlevs+2, 1)
         endif
      else
         if(movielabs) then
            call lblbar (0, vpl, vpr, 0.0, vpb - 0.002, 
     &                   nlevs+1, 1.0, 
     &                   0.33, bandclr, 1, barlab, nlevs+2, 1)
         else
            call lblbar (0, vpl, vpr, 0.0, vpb - 0.005, 
     &                   nlevs+1, 1.0, 
     &                   0.33, bandclr, 1, barlab, nlevs+2, 1)
         endif
      endif
c
c
c     Reset quality flag and clipping to their former values.
c
c      call pcseti ('QU - quality flag', qual)
c      call gsclip (oldclp)
c
c
      return
      end

      INTEGER FUNCTION SET_PATTERN (VALUE)
c----------------------------------------------------------------------
c     This function sets the dot pattern for a value
c----------------------------------------------------------------------
      include 'plotops.icl'

      real VALUE
      integer i

      SET_PATTERN=0
      if (VALUE .gt. pat1(1) .and. VALUE .le. pat1(2)) then
         call GSFAIS(2)
         call GSFASI(1)
         SET_PATTERN=1
         return
      endif

      if(VALUE .gt. pat2(1) .and. VALUE .le. pat2(2)) then
         call GSFAIS(2)
         call GSFASI(2)
         SET_PATTERN=2
         return
      endif

      if(VALUE .gt. pat3(1) .and. VALUE .le. pat3(2)) then
         call GSFAIS(2)
         call GSFASI(3)
         SET_PATTERN=3
         return
      endif

      if(VALUE .gt. pat4(1) .and. VALUE .le. pat4(2)) then
         call GSFAIS(2)
         call GSFASI(4)
         SET_PATTERN=4
         return
      endif

      if(VALUE .gt. pat5(1) .and. VALUE .le. pat5(2)) then
         call GSFAIS(2)
         call GSFASI(5)
         SET_PATTERN=5
         return
      endif

      if (VALUE .gt. pat6(1) .and. VALUE .le. pat6(2)) then
         call GSFAIS(2)
         call GSFASI(6)
         SET_PATTERN=6
         return
      endif
      call GSFAIS(1)
      return
      end


      Subroutine LBFILL(IFTP,XCRA,YCRA,NCRA,INDX)
c------------------------------------------------------------------------
c We have our own version of lbfill to take care of the case of patterns.
c Otherwise this matches NCAR's
c------------------------------------------------------------------------
      INTEGER IFTP, NCRA, INDX
      REAL XCRA, YCRA
      DIMENSION XCRA(*),YCRA(*)
      
      include 'plotops.icl'

      integer set_pattern,mxlevs
      parameter (mxlevs = 300)
      real clevel
      dimension clevel(mxlevs)
      common /contour_levels/ clevel
      
      if(filtyp .eq. PATTERN) then
         if(set_pattern(clevel(INDX)) .gt. 0) then
c           make it black
            CALL GSFACI (1)
            CALL GFA (NCRA-1,XCRA,YCRA)
         else
            call GSFAIS(0)
            return
         endif
      else
         CALL GSFACI (INDX)
         CALL GFA (NCRA-1,XCRA,YCRA)
      endif
      RETURN
      END
         
c      subroutine cpmpxy_p(x, y, t, z)
c
c      real x,y,t,z
c
c      print *, x, y, t, z
c      return
c      end
 
