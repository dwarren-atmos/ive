      subroutine drlineplt (data, dataloc, nu, savflg, error,
     &     time, axes)
c-----------------------------------------------------------------------
c     $Id: drlineplt.f,v 1.34 2001/08/15 22:25:46 davidive Exp $
c     Purpose:
c        This routine makes the set call and calls the line drawing
c          routine for line plots.
c     Arguments:
c        data     real  input  array containing values along the plot
c                              abacissa.
c        dataloc  real  input  array containing values along the plot
c                              ordinate. 
c        nu       int   input  number of points in the
c                              first subscript direction of data.
c        savflg   logi  input  indicates whether or not the current plot
c                              is saved (used to overlay plots). 
c        error    logi  output  indicates possible errors found in this
c                               routine.
c                               error = false  no errors detected
c                               error = true   error detected
c     History:
c      $Log: drlineplt.f,v $
c      Revision 1.34  2001/08/15 22:25:46  davidive
c      Added point_axis.c to deal with which axis to plot agains in diagonal 1d plots.
c      Added code to set up which axis to use and to convert the labels in 1d plots.
c
c      Revision 1.33  2000/12/12 17:58:23  harry
c      Fix problems with arbitrary line slices.
c
c      Revision 1.32  2000/05/09 19:53:48  harry
c      Seperate contour and line colors.  Make patterns work like colors.
c
c      Revision 1.31  2000/02/11 23:14:33  harry
c      Allow surface 1d plots.  Fix Z slider for surface plots.
c
c      Revision 1.30  1997/06/27 21:40:10  davidive
c      Fix arbitrary slice line labeling
c
c      Revision 1.29  1997/06/05 15:55:31  harry
c      Fixes to point slicing.
c
c      Revision 1.28  1997/05/07 18:43:04  harry
c      Mouse lines are drawn with same colors/patterns as normal lines.
c
c      Revision 1.27  1996/10/16 16:05:26  warren
c      correct non perpendicular 1d plot lables
c
c      Revision 1.26  1996/05/07 18:42:50  harry
c      Fix plot labelling for vertical axis.
c
c      Revision 1.25  1996/02/27 23:15:15  harry
c      Fix labels to use new netCDF attributes.
c
c Revision 1.24  1995/10/04  22:09:53  harry
c Add new netCDF attributes and specification of staggering.  Add
c changes to slicer and plot labels to reflect user specified labels.
c
c Revision 1.23  1995/09/05  20:33:45  harry
c Add new slicer.  Good luck!
c
c Revision 1.22  1995/08/24  20:08:01  harry
c Add David Bresch and Chris Schaer's heading transform routine.
c
c Revision 1.21  1995/06/23  21:16:18  warren
c First part of new ive help
c Changed " to ' in fortran files
c Fixed the label spacing in 1d plots
c added frame_width command
c
c Revision 1.20  1995/03/01  23:16:35  warren
c Fixed Rajul's crashing on ALPHA bug and in the process found a couple of
c variables that were read before they were set.
c
c Revision 1.19  1995/01/09  18:52:27  warren
c Moved the line color rotation selection from everywhere to makseg.
c Note: makseg now has one argument. it should be called with true if
c you only use hicolor and false if you use two colors in the type of plot.
c
c Revision 1.18  1994/09/14  18:19:53  harry
c Big 1D flip uncommit.
c
c Revision 1.17  1994/09/07  22:07:08  warren
c removed the extra plot from the color table locking sequence.
c
c Revision 1.16  1994/06/28  17:43:29  warren
c First cut at fancy tic marks.
c
c Revision 1.15  1994/06/11  21:33:18  yuri
c Added more comments.
c
c Revision 1.14  1994/04/19  23:02:53  yuri
c Big 1D flip commit.
c
c Revision 1.13  1994/03/29  23:56:38  harry
c Correct contour min/max/interval label and make sure calls to gslwsc
c have a real parameter.
c
c Revision 1.12  1994/02/23  00:39:43  warren
c final final changes to labes for 3.1
c
c Revision 1.11  1994/02/23  00:15:29  warren
c Final 3.1 label changes.
c
c Revision 1.10  1994/02/01  02:07:02  warren
c Cleaned up line and area fill movie labels somewhat.
c
c Revision 1.9  1994/01/29  00:11:31  warren
c first cut at movie labels. Line plots still need work
c
c Revision 1.8  1994/01/20  20:44:34  yuri
c Propagating of "overlay_patterns" stuff to newer version.
c
c Revision 1.7  1993/10/26  18:29:10  yuri
c Corrected log(negative number) problem, reported by PABLO.
c
c Revision 1.6  1993/07/09  20:02:01  warren
c Removed double label on line plot corners.
c
c Revision 1.5  1993/07/09  19:09:30  warren
c Pretty labels to go with the 2d ticmarks. Also I removed superfluous calls
c to chkwin and moved all the others to driver.f.
c
c Revision 1.4  1993/07/03  01:53:16  yuri
c Tuning of overlay colors.
c
c Revision 1.3  1993/06/21  17:09:45  yuri
c case with t=free added.
c
c Revision 1.2  1993/06/11  18:20:47  yuri
c New colors for overlay.
c
c Revision 1.1.1.1  1993/04/07  19:39:53  harry
c Initial CVS checkin of IVE.
c
c Revision 2.15  1993/02/01  23:42:56  warren
c switched to use the regular tick mark vars as per property sheets.
c
c Revision 2.14  1992/12/10  21:15:11  millerp
c Add code to produce nicer line plots using variable data.
c
c Revision 2.13  1992/12/07  23:53:50  millerp
c Fixed up indenting.
c
c Revision 2.12  1992/11/24  16:33:44  millerp
c Make changes related to removing hslice, vsice.
c
c Revision 2.11  1992/11/06  17:36:55  millerp
c Fix argument list and dimensions of data and dataloc.
c
c Revision 2.10  1992/10/30  19:45:46  millerp
c Added stuff for averaged line plots.
c
c Revision 2.9  1992/10/16  15:10:23  warren
c fixed multiple declaration of time.
c
c Revision 2.8  1992/10/06  18:26:45  warren
c added xtics_line and ytics_line and plot types scaler and vector
c
c Revision 2.7  1992/10/05  18:21:21  warren
c increased space between axis label and unit label.
c
c Revision 2.6  1992/10/02  22:58:59  warren
c changed the commonblock window_points to specify witch axis is what
c and put it in pltwin.icl
c
c Revision 2.5  1992/10/02  18:27:51  warren
c added units to the line plot axes labels.
c
c Revision 2.4  1992/09/30  18:47:08  warren
c fixed labeling so that it works better with overlays.
c
c Revision 2.3  1992/09/30  18:23:03  warren
c fixes for labeling and 0 sized plot boxes
c
c Revision 2.2  1992/09/30  15:10:01  warren
c fixed missing dimensions
c
c Revision 2.1  1992/09/30  15:01:10  warren
c new line slicer version
c
c Revision 2.0  1992/07/01  21:13:01  millerp
c Released version rereving to 2.0
c
c Revision 1.4  1992/06/15  19:07:13  millerp
c Change labeling, change argument list for scale.
c
c Revision 1.3  1992/06/10  20:19:47  millerp
c Scale data before plotting, and fix up labeling.
c
c Revision 1.2  1992/05/05  15:24:08  millerp
c Added stuff for sounding plots.
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
      include 'data.icl'
      include 'plotops.icl'
      include 'units.icl'
      include 'color.icl'
c
c
c     Argument declarations.
c
      integer     nu, axes
      real        data, dataloc, time
      logical     error, savflg
      dimension data(nu), dataloc(nu), time(2)
c
c
c     Local variable declarations.
c
      integer         xaxis, yaxis ! which axes in which way
      logical         movielabs    ! DW's stuff
      real            ORVAL
      parameter ( ORVAL = 1.0e12 )
      real            xmajr, xminr, ymajr, yminr ! ticks - minor and major
      real            lolim, uplim, spval ! min, max limits and special value
      dimension       lolim(4), uplim(4)
c
c
c     Initialization
c
c
      error = .false.
c
c
      call getrvar ('spval', spval, error)
      if ( error ) then
         return
      endif
      call getlvar('movielabs', movielabs, error)
      if ( error ) then
         return
      endif
c
c     Make appropriate set call. to set up limits, ticks and colors
c
      call setlineplt (nu, savflg, lolim, uplim, axes, error)
      if ( error ) then
         return
      endif
c
c     low level drawing routine (just ncar calls)
c
      call drawline (data, dataloc, nu, error)
 10   continue
c
c     If the contouring routines encountered any errors, then return.
c
      if ( error ) then
         return
      endif
c
c     Draw perimeter.
c
      if ( .not. savflg ) then
         xmajr = xmajor
         xminr = xminor
         ymajr = ymajor
         yminr = yminor
         call getivar ('xaxis', xaxis, error)
         call getivar ('yaxis', yaxis, error)
         xaxis = mod(xaxis,10)
         yaxis = mod(yaxis,10)
         
         call scaletics (xmajr, xminr, ymajr, yminr,
     &         xaxis, yaxis)
            if (movielabs) then
               call pcseti ('QU', 0)            
               call pcsetc ('FN', 'HELVETICA-BOLD    ')
            else
               call pcseti ('QU',1)
               call pcseti ('CL',1)
            endif
               call uwperim (xmajr, xminr, ymajr, yminr)
c
c        Reset xmajorl, ymajorl if the graphics package has chosen a
c        major tic mark interval, rather than the user.
c
         if (ive_set_xmajorl) xmajor = 0.0
         if (ive_set_ymajorl) ymajor = 0.0
      endif
c
c     Label plot.
c
      if ( label .and. axes .ne. -1) then
         call lablineplt (datfil, field, lolim, uplim, time, 
     &                    savflg,average,  axes, surface)
      endif
c
c
c     Release all output.
c
      call plotit (0, 0, 0)
c
c
      return
      end
c
c
      subroutine lablineplt (datfil, field, lolim, uplim, time, 
     &                       savflg, average, axes, surface)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine is used to label line plots.
c     Arguments:
c        datfil  char  input   the data file name.
c        field   char  input   the plotted field.
c        lolim   real  input   array containing the plot window
c                              coordinates at the lower left corner.
c        uplim   real  input   array containing the plot window
c                              coordinates at the upper right corner.
c        time  real  input     array containing info about the slice
c                              location.
c                              slice.
c        savflg  logi  input   indicates whether the save flag is on or
c                              off
c        axes    int   axes    1 = x, 2 = y, 3 = z, 4 = t
c                              z & t are vertical x and y are horizontal
c        surface logi  input   indicates surface plot
c     History:
c-----------------------------------------------------------------------
c
c
      include 'pltwin.icl'
      include 'units.icl'
      include 'segments.icl'
c     
c
c     Argument declarations.
c
      character *(*)   datfil, field
      character *(80)  dim_names(4)
      logical          savflg, average, surface
      real             lolim, uplim, time, szsc
      integer          axes
      dimension lolim(3), uplim(3), time(2)
c
c
c     Local variable declarations.
c
      character *(256)  labellab, numlab
      character *(UNIT_LEN+2) unitlabel
      integer          i, ibeg, iend, ierror, line_index, lineclr,
     &                 linepat, linlog, axmod, use_buttons,
     &                 maxdim, sign, text_index, plot_number, ive_plock
      logical          error, movielabs
      real             vpl, vpr, vpb, vpt, wdl, wdr, wdb, wdt
      real             center, orient, size, szsf, shift, 
     &                 xpos, ypos, xpts, ypts, linewdth
      real             value, xmajor, ymajor

      data  size / 0.015 /
      dimension xpts(2), ypts(2), ive_plock(4)
c
c     External function declarations.
c
      character *(80) ftoa
      integer    nblank, strbeg, strend
      real       cfux, cfuy
c
c
      call getrvar('linwdth', linewdth, error)
      call getlvar('movielabs',movielabs, error)
      call getivar('use_buttons',use_buttons, error)
      call getiarr('ive_plock',ive_plock, 4, error)
      call getaarr('dim_display_names', dim_names, 4, error)
      if(movielabs) then
         szsc = 1.2
      else
         szsc = 0.75
      endif
c      if(use_buttons .eq. 0) then
c         call getset (vpl, vpr, vpb, vpt, wdl, wdr, wdb, wdt, linlog)
c         if (yaxis .gt. 0 .and. yaxis .le. 4) then
c            axes=yaxis
c         endif
c      endif

      plot_number = curoverlay(curlist)
c
c     Only label first 3 overlays
c
      if (plot_number .gt. 3 . or. 
     &    (plot_number .gt. 2 .and. movielabs)) return
c
c
c     Set text and line colors to the background color.
c
      call gqtxci (ierror, text_index)
      call gqplci (ierror, line_index)
      call gstxci (1)
      call gsplci (1)
c
c     Get current set parameters.
c
      call gsclip (0)
      call getset (vpl, vpr, vpb, vpt, wdl, wdr, wdb, wdt, linlog)
      szsf = size * (vpr - vpl)
      
c
c     Cat nulls to strings sent to C routines.
c
      call catnull (data_units, len(data_units), error)
      call catnull (data_units, len(data_display_units), error)
      call getivar ('maxdim', maxdim, error)
      call getrvar('xmajor',xmajor,error)
      call getrvar('ymajor',ymajor,error)
      do 5 i = 1, maxdim
         call catnull (domain_units(i), len(domain_units(i)), error)
         call catnull (domain_display_units(i),
     &                 len(domain_display_units(i)), error)
 5    continue
c
c
c     Call set so we can use fractional coordinates.
c
      call set (vpl, vpr, vpb, vpt, 0.0, 1.0, 0.0, 1.0, 1)
c
c
c
c
c     Write data set label at top of plot.
c
c     First eliminate the "directory" part of the file name.
c
      ibeg = strbeg(datfil)
      iend = strend(datfil)
      do 10 i = iend, ibeg, -1
         if ( (datfil(i:i) .eq. '/') .or. (datfil(i:i) .eq. ']') ) then
            ibeg = i + 1
            go to 20
         endif
 10   continue
c
 20   labellab= datfil(ibeg:iend)
c
c     Place label just above top plot border.
c
      if (plot_number .eq. 1) then
         xpos = cfux (vpl)
         center = -1.0
      else if ( plot_number .eq. 2 ) then
         xpos = cfux (vpr)
         center = 1.0
      else
         xpos = cfux ((vpl + vpr)/2.0)
         center = 0.0
      endif
      ypos = cfuy (vpt + 0.06)
      orient = 0.0
      call upcase (labellab(1:iend - ibeg + 1), iend - ibeg + 1)
c call user-defined heading routine (MOD DAVID BRESCH)
      call heading_trans(3,1,labellab(1:iend - ibeg + 1))
      ibeg = strbeg(labellab)
      iend = strend(labellab)
      if(.not. movielabs) then
         call plchhq (xpos, ypos, labellab(1:iend - ibeg + 1), szsf,
     &                orient, center)
      endif
c
c     Write slice information at top left of plot.
c
c     Add time and endpoints label.
c     
c
      value = lolim(1)
      call scale (value, 1, domain_slope(1), domain_intercept(1),
     &     0.0)
      numlab = ftoa(value)
      call clean_zero(numlab(strbeg(numlab):strend(numlab)))
      labellab =  '('//numlab(strbeg(numlab):strend(numlab))
c
      value = lolim(2)
      call scale (value, 1, domain_slope(2), domain_intercept(2), 0.0)
      numlab = ftoa(value)
      call clean_zero(numlab(strbeg(numlab):strend(numlab)))
      labellab = labellab(strbeg(labellab):strend(labellab))//
     &       ' '//numlab(strbeg(numlab):strend(numlab))
c
      if (surface) then
         numlab = 'sfc'
      else
         value = lolim(3)
         call scale (value, 1, domain_slope(3), domain_intercept(3),
     &        0.0)
         numlab = ftoa(value)
         call clean_zero(numlab(strbeg(numlab):strend(numlab)))
      endif
      labellab = labellab(strbeg(labellab):strend(labellab))//
     &       ' '//numlab(strbeg(numlab):strend(numlab))

      call scale (time(1), 1, domain_slope(4), domain_intercept(4), 0.0)
      numlab = ftoa(time(1))
      call clean_zero(numlab(strbeg(numlab):strend(numlab)))
      labellab = labellab(strbeg(labellab):strend(labellab))//
     &        ' '//numlab(strbeg(numlab):strend(numlab))//')'
c
      value = uplim(1)
      call scale (value, 1, domain_slope(1), domain_intercept(1), 
     &     0.0)
      numlab = ftoa(value)
      call clean_zero(numlab(strbeg(numlab):strend(numlab)))
      labellab =labellab(strbeg(labellab):strend(labellab))//
     &     ' to '//'('//numlab(strbeg(numlab):strend(numlab))
c
      value = uplim(2)
      call scale (value, 1, domain_slope(2), domain_intercept(2), 0.0)
      numlab = ftoa(value)
      call clean_zero(numlab(strbeg(numlab):strend(numlab)))
      labellab = labellab(strbeg(labellab):strend(labellab))//' '
     &        //numlab(strbeg(numlab):strend(numlab))
c
      if (surface) then
         numlab = 'sfc'
      else
         value = uplim(3)
         call scale (value, 1, domain_slope(3), domain_intercept(3),
     &        0.0)
         numlab = ftoa(value)
         call clean_zero(numlab(strbeg(numlab):strend(numlab)))
      endif
      labellab =labellab(strbeg(labellab):strend(labellab))//' '
     &       //numlab(strbeg(numlab):strend(numlab))

      call scale (time(2), 1, domain_slope(4), domain_intercept(4), 0.0)
      numlab = ftoa(time(2))
      call clean_zero(numlab(strbeg(numlab):strend(numlab)))
      labellab = labellab(strbeg(labellab):strend(labellab))//
     &        ' '//numlab(strbeg(numlab):strend(numlab))//')'
c
      ibeg = 1
      iend = strend (labellab)
      if (plot_number .eq. 1) then
         xpos = cfux (vpl)
         center = -1.0
      else if ( plot_number .eq. 2 ) then
         xpos = cfux (vpr)
         center = 1.0
      else
         xpos = cfux ((vpl + vpr)/2.0)
         center = 0.0
      endif
      if (movielabs) then
         ypos   = cfuy (vpt + 0.03)
      else
         ypos   = cfuy (vpt + 0.035)
      endif
      orient = 0.0
c call user-defined heading routine (MOD DAVID BRESCH)
      call heading_trans(3,2,labellab(ibeg:iend))
      ibeg = strbeg(labellab)
      iend = strend(labellab)
      if (movielabs .or. tic_scale .gt. 1.2) then
         call plchhq (xpos, ypos, labellab(ibeg:iend), 
     &        (szsf * szsc)/1.7, orient, center)
      else
         call plchhq (xpos, ypos, labellab(ibeg:iend), szsf/2.1,
     &                orient, center)
      endif
c
c
c     Create labels for the axes
c
c
      if (plot_number .eq. 1 ) then
         if(xmajor .eq. 0 .and. xaxis .ne. 0) then
            axmod = mod(xaxis,10)
            value = x1
            if(axmod .ne. xaxis) then
               call point_fix_label_value(axmod, value)
            endif
            call scale(value, 1, domain_slope(axmod),
     &           domain_intercept(axmod), 0.0)
            numlab = ftoa(value)
            call clean_zero(numlab(strbeg(numlab):strend(numlab)))
            xpos   = cfux (vpl)
            center = 0.0
            ypos   = cfuy (vpb - 0.02)
            orient = 0.0
            ibeg = 1
            iend = strend (numlab)
            call plchhq (xpos, ypos, numlab(ibeg:iend), szsf * szsc, 
     &           orient, center)
            
            value = x2
            if(axmod .ne. xaxis) then
               call point_fix_label_value(axmod, value)
            endif
            call scale(value, 1, domain_slope(axmod),
     &           domain_intercept(axmod), 0.0)
            numlab = ftoa(value)
            call clean_zero(numlab(strbeg(numlab):strend(numlab)))
            xpos   = cfux (vpr)
            center = 0.0
            ypos   = cfuy (vpb - 0.02)
            orient = 0.0
            ibeg = 1
            iend = strend (numlab)
            call plchhq (xpos, ypos, numlab(ibeg:iend), szsf * szsc, 
     &           orient, center)
         endif
         if(ymajor .eq. 0 .and. yaxis .ne. 0) then
            axmod = mod(yaxis,10)
            value = y1
            call scale(value, 1, domain_slope(axmod),
     &           domain_intercept(axmod), 0.0)
            numlab = ftoa(value)
            call clean_zero(numlab(strbeg(numlab):strend(numlab)))
            xpos   = cfux (vpl - 0.01)
            center = 1.0
            ypos   = cfuy (vpb)
            orient = 0.0
            ibeg = 1
            iend = strend (numlab)
            call plchhq (xpos, ypos, numlab(ibeg:iend), szsf * szsc, 
     &           orient, center)
            
            value = y2
            call scale(value, 1, domain_slope(axmod),
     &           domain_intercept(axmod), 0.0)
            numlab = ftoa(value)
            call clean_zero(numlab(strbeg(numlab):strend(numlab)))
            xpos   = cfux (vpl - 0.01)
            center = 1.0
            ypos   = cfuy (vpt)
            orient = 0.0
            ibeg = 1
            iend = strend (numlab)
            call plchhq (xpos, ypos, numlab(ibeg:iend), szsf * szsc, 
     &           orient, center)
         endif
      endif
c     Set up unit label.
c
      if ( nblank(data_display_units) .gt. 0 ) then
         unitlabel = ' ('//
     &        data_display_units(strbeg(data_display_units):
     &        strend(data_display_units))//
     &        ')'
      elseif ( nblank(data_units) .gt. 0 ) then
         unitlabel = '   ('//
     &        data_units(strbeg(data_units):
     &        strend(data_units))//
     &        ')'
      else
         unitlabel = ' '
      endif
c
c
c
c     Write Field and dimension on appropriate sides
c
      ibeg = strbeg(field)
      iend = strend(field)
      if (average) then
         labellab = 'average '//field(ibeg:iend)
      else
         labellab = field(ibeg:iend)
      endif
      ibeg = strbeg(labellab)
      iend = strend(labellab)
      labellab = labellab(ibeg:iend)//
     &             unitlabel(strbeg(unitlabel):strend(unitlabel))
      ibeg = strbeg(labellab)
      iend = strend(labellab)
c
c     X,Y,T field
c
      if (xaxis .ne. 0) then
c     FIELD goes on the left side
         if(tic_scale .gt. 1.2) then
            xpos   = cfux (vpl - 0.13)
         else
            xpos   = cfux (vpl - 0.1)
         endif
         if(plot_number .eq. 2) then 
            ypos = cfuy(((vpt - vpb)/4.0)*3.0 + vpb)
         else if(plot_number .eq. 1) then
            ypos   = cfuy ((vpt - vpb)/4.0 + vpb)
         else if(plot_number .eq. 3) then
            ypos   = cfuy ((vpt - vpb)/2.0 + vpb)
         endif
         center = 0.0
         orient = 90.0
         call plchhq (xpos, ypos, labellab(ibeg:iend), szsf * szsc, 
     &        orient, center)
      else
c     
c     FIELD goes on the bottom         
c     
         if(plot_number .eq. 2) then
            xpos   = cfux (vpr)
            center = 1.0
         else if(plot_number .eq. 1) then
            xpos   = cfux (vpl)
            center = -1.0
         else if(plot_number .eq. 3) then
            xpos   = cfux ((vpr - vpl)/2.0 + vpl)
            center = 0
         endif
         if(tic_scale .gt. 1.2) then
            ypos   = cfuy (vpb - 0.13)
         else
            ypos   = cfuy (vpb - 0.1)
         endif
         orient = 0.0
         call plchhq (xpos, ypos, labellab(ibeg:iend), szsf * szsc, 
     &        orient, center)
      endif
c
c     axis label
c
      if(use_buttons .ne. 0) then
         labellab = dim_names(axes)
         ibeg = strbeg(labellab)
         iend = strend(labellab)
         if(axes .eq. 1) then
            xpos   = cfux ((vpl + vpr)/2.0)
            ypos   = cfuy (vpb - 0.06)
            center = 1.0
            orient = 0.0
c     
c     Set up unit label.
c
            if (nblank(domain_display_units(1)) .ne. 0) then
               unitlabel = '   ('//
     &              domain_display_units(1)
     &              (strbeg(domain_display_units(1)):
     &              strend(domain_display_units(1)))//')'
               
            elseif ( nblank(domain_units(1)) .ne. 0 ) then
               unitlabel = ' ('//
     &           domain_units(1)(strbeg(domain_units(1)):
     &              strend(domain_units(1)))//
     &              ')'
            else
               unitlabel = ' '
            endif
c     
         elseif(axes .eq. 2) then
            xpos   = cfux ((vpl + vpr)/2.0)
            ypos   = cfuy (vpb - 0.06)
            center = 1.0
            orient = 0.0
c     
c     Set up unit label.
c     
            if (nblank(domain_display_units(2)) .ne. 0) then
               unitlabel = ' ('//
     &              domain_display_units(2)
     &              (strbeg(domain_display_units(2)):
     &              strend(domain_display_units(2)))//')'
            elseif ( nblank(domain_units(2)) .ne. 0 ) then
               unitlabel = ' ('//
     &              domain_units(2)(strbeg(domain_units(2)):
     &              strend(domain_units(2)))//
     &              ')'
            else
               unitlabel = ' (unspecified Units)'
            endif
c     
         elseif(axes .eq. 3) then
            xpos   = cfux (vpl - 0.075)
            ypos   = cfuy ((vpt + vpb)/2.0)
            center = 1.0
            orient = 90.0
c
c     Set up unit label.
c
            if (nblank(domain_display_units(3)) .ne. 0) then
               unitlabel = ' ('//
     &              domain_display_units(3)
     &              (strbeg(domain_display_units(3)):
     &              strend(domain_display_units(3)))//')'
            elseif ( nblank(domain_units(3)) .ne. 0 ) then
               unitlabel = ' ('//
     &              domain_units(3)(strbeg(domain_units(3)):
     &              strend(domain_units(3)))//
     &              ')'
            else
               unitlabel = ' (unspecified Units)'
            endif
c

         elseif(axes .eq. 4) then
            xpos   = cfux ((vpl + vpr)/2.0)
            ypos   = cfuy (vpb - 0.06)
            center = 1.0
            orient = 0.0
c
c     Set up unit label.
c
            if (nblank(domain_display_units(4)) .ne. 0) then
               unitlabel = ' ('//
     &              domain_display_units(4)
     &              (strbeg(domain_display_units(4)):
     &              strend(domain_display_units(4)))//')'
            elseif ( nblank(domain_units(4)) .ne. 0 ) then
               unitlabel = ' ('//
     &              domain_units(4)(strbeg(domain_units(4)):
     &              strend(domain_units(4)))//
     &              ')'
            else
               unitlabel = ' (unspecified Units)'
            endif
c
         endif
         labellab = labellab(ibeg:iend)//
     &        unitlabel(strbeg(unitlabel):strend(unitlabel))
         ibeg = strbeg(labellab)
         iend = strend(labellab)
         call plchhq (xpos, ypos, labellab(ibeg:iend), szsf * szsc, 
     &        orient, center)
      else                      !use_buttons .ne. 0
         if(xaxis .ne. 0) then
            if(xaxis .gt. 4) then
               axmod = mod(xaxis,10)
            else
               axmod = xaxis
            endif
            labellab = dim_names(axmod)
            ibeg = strbeg(labellab)
            iend = strend(labellab)
            if (nblank(domain_display_units(axmod)) .ne. 0) then
               unitlabel = ' ('//
     &              domain_display_units(axmod)
     &              (strbeg(domain_display_units(axmod)):
     &              strend(domain_display_units(axmod)))//')'
            elseif ( nblank(domain_units(axmod)) .ne. 0 ) then
               unitlabel = ' ('//
     &              domain_units(axmod)(strbeg(domain_units(axmod)):
     &              strend(domain_units(axmod)))//
     &              ')'
            else
               unitlabel = ' (unspecified Units)'
            endif
c     
            labellab = labellab(ibeg:iend)//
     &           unitlabel(strbeg(unitlabel):strend(unitlabel))
            ibeg = strbeg(labellab)
            iend = strend(labellab)
c     
            xpos   = cfux ((vpl + vpr)/2.0)
            ypos   = cfuy (vpb - 0.06)
            center = 1.0
            orient = 0.0
            if(axmod .eq. xaxis)
     &           call plchhq (xpos, ypos, labellab(ibeg:iend), szsf * 
     &           szsc*1.1, orient, center)
            if(axmod .ne. xaxis) then ! must be line through 2 dims
               
               if(axmod .eq. xdim_lab) then 
                  labellab = labellab(ibeg:iend)//' along '//
     &                 dim_names(ydim_lab)
                  ibeg = strbeg(labellab)
                  iend = strend(labellab)
                  if (nblank(domain_display_units(ydim_lab)) .ne. 0)then
                     unitlabel = ' ('//
     &                    domain_display_units(ydim_lab)
     &                    (strbeg(domain_display_units(ydim_lab)):
     &                    strend(domain_display_units(ydim_lab)))//')'
                  elseif ( nblank(domain_units(ydim_lab)) .ne. 0 ) then
                     unitlabel = ' ('//
     &                    domain_units(ydim_lab)(
     &                    strbeg(domain_units(ydim_lab)):
     &                    strend(domain_units(ydim_lab)))//
     &                    ')'
                  else
                     unitlabel = ' (unspecified Units)'
                  endif
c     
                  labellab = labellab(ibeg:iend)//
     &                 unitlabel(strbeg(unitlabel):strend(unitlabel))
                  ibeg = strbeg(labellab)
                  iend = strend(labellab)
               else
                  labellab = labellab(ibeg:iend)//' along '//
     &                 dim_names(xdim_lab)
                  ibeg = strbeg(labellab)
                  iend = strend(labellab)
                  if (nblank(domain_display_units(xdim_lab)).ne. 0) then
                     unitlabel = ' ('//
     &                    domain_display_units(xdim_lab)
     &                    (strbeg(domain_display_units(xdim_lab)):
     &                    strend(domain_display_units(xdim_lab)))//')'
                  elseif ( nblank(domain_units(xdim_lab)) .ne. 0 ) then
                     unitlabel = ' ('//
     &                    domain_units(xdim_lab)(
     &                    strbeg(domain_units(xdim_lab)):
     &                    strend(domain_units(xdim_lab)))//
     &                    ')'
                  else
                     unitlabel = ' (unspecified Units)'
                  endif
c     
                  labellab = labellab(ibeg:iend)//
     &                 unitlabel(strbeg(unitlabel):strend(unitlabel))
                  ibeg = strbeg(labellab)
                  iend = strend(labellab)
               endif
               labellab =  labellab(ibeg:iend)//' = '
               ibeg = strbeg(labellab)
               iend = strend(labellab) + 1
               if(axmod .eq. xdim_lab) then
                  numlab = ftoa(slice_slope)
                  labellab=labellab(ibeg:iend)//numlab
                  ibeg = strbeg(labellab)
                  iend = strend(labellab)
                  labellab =  labellab(ibeg:iend)//dim_names(xdim_lab)
                  ibeg = strbeg(labellab)
                  iend = strend(labellab)

                  if(slice_inter .eq. 0) then
                     labellab=labellab(ibeg:iend)//')/'
                     ibeg = strbeg(labellab)
                     iend = strend(labellab)
                  else
                     if(slice_inter .gt. 0)
     &                    labellab =  labellab(ibeg:iend)//'+'
                  
                     ibeg = strbeg(labellab)
                     iend = strend(labellab)
                     numlab = ftoa(slice_inter)
                     labellab=labellab(ibeg:iend)//numlab(strbeg(numlab)
     &                    :strend(numlab))
                     ibeg = strbeg(labellab)
                     iend = strend(labellab)
                  endif
               else
                  labellab =  labellab(ibeg:iend)//'('//
     &              dim_names(ydim_lab)
                  ibeg = strbeg(labellab)
                  iend = strend(labellab)
                  if(slice_inter .lt. 0) then
                     numlab = ftoa(-slice_inter)
                     labellab=labellab(ibeg:iend)//'+'//numlab
                     ibeg = strbeg(labellab)
                     iend = strend(labellab)
                     labellab=labellab(ibeg:iend)//')/'
                     ibeg = strbeg(labellab)
                     iend = strend(labellab)
                  elseif(slice_inter .eq. 0) then
                     labellab=labellab(ibeg:iend)//')/'
                     ibeg = strbeg(labellab)
                     iend = strend(labellab)
                  else
                     numlab = ftoa(-slice_inter)
                     labellab=labellab(ibeg:iend)//'-'//numlab
                     ibeg = strbeg(labellab)
                     iend = strend(labellab)
                     labellab=labellab(ibeg:iend)//')/'
                     ibeg = strbeg(labellab)
                     iend = strend(labellab)
                  endif
                  numlab = ftoa(slice_slope)
                  labellab=labellab(ibeg:iend)//numlab
                  ibeg = strbeg(labellab)
                  iend = strend(labellab)
               endif
c     These are done up top
c               xpos   = cfux ((vpl + vpr)/4.0)
c               ypos   = cfuy (vpb - 0.06)
c               center = 1.0
c               orient = 0.0
               call plchhq (xpos, ypos, labellab(ibeg:iend), 
     &              szsf * szsc*.80, orient, center)
            endif
         else
            if(yaxis .gt. 4) then
               axmod = mod(yaxis,10)
            else
               axmod = yaxis
            endif
            labellab = dim_names(axmod)
            ibeg = strbeg(labellab)
            iend = strend(labellab)
             
             if (nblank(domain_display_units(axmod)) .ne. 0) then
                unitlabel = ' ('//
     &               domain_display_units(axmod)
     &               (strbeg(domain_display_units(axmod)):
     &               strend(domain_display_units(axmod)))//')'
             elseif ( nblank(domain_units(axmod)) .ne. 0 ) then
               unitlabel = ' ('//
     &               domain_units(axmod)(strbeg(domain_units(axmod)):
     &               strend(domain_units(axmod)))//
     &               ')'
            else
               unitlabel = ' (unspecified Units)'
            endif
c     
            labellab = labellab(ibeg:iend)//
     &           unitlabel(strbeg(unitlabel):strend(unitlabel))
            ibeg = strbeg(labellab)
            iend = strend(labellab)
c     
            xpos   = cfux (vpl - 0.075)
            ypos   = cfuy ((vpt + vpb)/2.0)
            center = 1.0
            orient = 90.0
            if(axmod .eq. yaxis)
     &           call plchhq (xpos, ypos, labellab(ibeg:iend), szsf * 
     &           szsc*1.1, orient, center)
c
            if(axmod .ne. yaxis) then ! must be line through 2 dims
               if(axmod .eq. xdim_lab) then
                  labellab = labellab(ibeg:iend)//' along '//
     &                 dim_names(ydim_lab)
                  ibeg = strbeg(labellab)
                  iend = strend(labellab)
                  if (nblank(domain_display_units(ydim_lab)) .ne. 0)then
                     unitlabel = ' ('//
     &                    domain_display_units(ydim_lab)
     &                    (strbeg(domain_display_units(ydim_lab)):
     &                    strend(domain_display_units(ydim_lab)))//')'
                  elseif ( nblank(domain_units(ydim_lab)) .ne. 0 ) then
                     unitlabel = ' ('//
     &                    domain_units(ydim_lab)(strbeg(domain_units
     &                    (ydim_lab)):strend(domain_units(ydim_lab)))//
     &                    ')'
                  else
                     unitlabel = ' (unspecified Units)'
                  endif
c     
                  labellab = labellab(ibeg:iend)//
     &                 unitlabel(strbeg(unitlabel):strend(unitlabel))
                  ibeg = strbeg(labellab)
                  iend = strend(labellab)
               else
                  labellab = labellab(ibeg:iend)//' along '//
     &                 dim_names(xdim_lab)
                  ibeg = strbeg(labellab)
                  iend = strend(labellab)
                  if (nblank(domain_display_units(xdim_lab)) .ne. 0)then
                     unitlabel = ' ('//
     &                    domain_display_units(xdim_lab)
     &                    (strbeg(domain_display_units(xdim_lab)):
     &                    strend(domain_display_units(xdim_lab)))//')'
                  elseif ( nblank(domain_units(xdim_lab)) .ne. 0 ) then
                     unitlabel = ' ('//
     &                    domain_units(xdim_lab)(strbeg(domain_units
     &                    (xdim_lab)):strend(domain_units(xdim_lab)))//
     &                    ')'
                  else
                     unitlabel = ' (unspecified Units)'
                  endif
c     
                  labellab = labellab(ibeg:iend)//
     &                 unitlabel(strbeg(unitlabel):strend(unitlabel))
                  ibeg = strbeg(labellab)
                  iend = strend(labellab)
               endif
               labellab = labellab(ibeg:iend)//' = '
               ibeg = strbeg(labellab)
               iend = strend(labellab) + 1
               if(axmod .eq. xdim_lab) then 
                  numlab = ftoa(slice_slope)
                  labellab=labellab(ibeg:iend)//numlab
                  ibeg = strbeg(labellab)
                  iend = strend(labellab)
                  labellab =  labellab(ibeg:iend)//dim_names(xdim_lab)
                  ibeg = strbeg(labellab)
                  iend = strend(labellab)
               
                  if(slice_inter .eq. 0) then
                     labellab=labellab(ibeg:iend)//')/'
                     ibeg = strbeg(labellab)
                     iend = strend(labellab)
                  else
                     if(slice_inter .ge. 0.)
     &                    labellab =  labellab(ibeg:iend)//' + '
               
                     numlab = ftoa(slice_inter)
                     ibeg = strbeg(labellab)
                     iend = strend(labellab)
                     labellab=labellab(ibeg:iend)//numlab(strbeg(numlab)
     &                    :strend(numlab))
                     ibeg = strbeg(labellab)
                     iend = strend(labellab)
                  endif
               else
                  labellab =  labellab(ibeg:iend)//'('//
     &              dim_names(ydim_lab)
                  ibeg = strbeg(labellab)
                  iend = strend(labellab)
                  if(slice_inter .lt. 0) then
                     numlab = ftoa(-slice_inter)
                     labellab=labellab(ibeg:iend)//'+'//numlab
                     ibeg = strbeg(labellab)
                     iend = strend(labellab)
                     labellab=labellab(ibeg:iend)//')/'
                     ibeg = strbeg(labellab)
                     iend = strend(labellab) 
                  elseif(slice_inter .eq. 0) then
                     labellab=labellab(ibeg:iend)//')/'
                     ibeg = strbeg(labellab)
                     iend = strend(labellab)
                 else
                     numlab = ftoa(-slice_inter)
                     labellab=labellab(ibeg:iend)//'-'//numlab
                     ibeg = strbeg(labellab)
                     iend = strend(labellab)
                     labellab=labellab(ibeg:iend)//')/'
                     ibeg = strbeg(labellab)
                     iend = strend(labellab)
                  endif
                  numlab = ftoa(slice_slope)
                  labellab=labellab(ibeg:iend)//numlab
                  ibeg = strbeg(labellab)
                  iend = strend(labellab)
               endif
c               xpos   = cfux (vpl - 0.075)
c               ypos   = cfuy ((vpt + vpb)/4.0)
c               center = 1.0
c               orient = 90.0
               call plchhq (xpos, ypos, labellab(ibeg:iend), 
     &              szsf * szsc*.80, orient, center)
            endif
         endif
      endif
c     
c
c
c     Create labels indicating contour line type and color for 
c     each plot.
c
      if ( plot_number .eq. 2 ) then
         xpos = cfux (vpr)
         sign = -1
      else if ( plot_number .eq. 1 ) then
         xpos = cfux (vpl)
         sign = 1
      else if ( plot_number .eq. 3 ) then
         shift = 0                        ! I have no idea what this should be
         xpos = cfux((vpl + vpr)/2) - shift/2
         sign = 1
      endif
      shift = 0.15
      xpts(2) = xpos
      if(movielabs) then
         ypos = cfuy (vpt + (0.015))
      else
         ypos = cfuy (vpt + (0.025))
      endif
      call getivar ('hicolor', lineclr, error)
      call gsplci (lineclr)
      call getivar ('hipattern', linepat, error)
      call dashdb (linepat)
      xpts(1) = xpos
      xpts(2) = xpos + sign*shift
      ypts(1) = ypos
      ypts(2) = ypos
      call  gslwsc (linewdth)
      call curved (xpts, ypts, 2)
      call plotit (0, 0, 0)
      call  gslwsc (1.)
c     
      call set (vpl, vpr, vpb, vpt, wdl, wdr, wdb, wdt, linlog)
c
c     Reset text and line color.
c
      call gstxci (text_index)
      call gsplci (line_index)
c
c
      return
      end












