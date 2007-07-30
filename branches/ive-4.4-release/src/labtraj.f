      subroutine labtraj (datfil, lolim, uplim, savflg)
c
c     $Id
c
c     This routine labels the trajectory plot.
c
c     $Log: labtraj.f,v $
c     Revision 1.4  2000/05/09 19:53:52  harry
c     Seperate contour and line colors.  Make patterns work like colors.
c
c     Revision 1.3  1998/07/28 18:22:29  harryive
c     New fixes for trajectories when they are the first overlay.
c     Also, remove old Yuri routine.
c
c     Revision 1.2  1997/07/02 20:08:10  warren
c     Fixes for problems found by DEC f90 compiler.
c
c     Revision 1.1  1996/04/25 16:25:21  harry
c     Add Skew-T plots and the beginning of trajectory plots.
c
c
      include 'pltwin.icl'
      include 'units.icl'
      include 'segments.icl'  
      include 'traj.icl'

c     
c
c     Argument declarations.
c
      character *(*)   datfil
      logical          savflg, average
      real             lolim, uplim, szsc
      integer          axes
      dimension lolim(4), uplim(4)
c
c
c     Local variable declarations.
c
      character *(256)  label, numlab
      character *(UNIT_LEN+2) unitlabel
      integer          i, ibeg, iend, ierror, line_index, lineclr,
     &                 linepat, linlog, 
     &                 maxdim, sign, text_index, plot_number
      logical          error, movielabs
      real             vpl, vpr, vpb, vpt, wdl, wdr, wdb, wdt
      real             xx1, xx2, yy1, yy2
      real             center, orient, size, szsf, shift, 
     &                 xpos, ypos, xpts, ypts, linewdth
      real             value, xmajor, ymajor

      data  size / 0.015 /
      dimension xpts(2), ypts(2)
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
      if(movielabs) then
         szsc = 1.2
      else
         szsc = 0.75
      endif
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
 20   label= datfil(ibeg:iend)
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
      ypos = cfuy (vpt + 0.04)
      orient = 0.0
c      call upcase (label(1:iend - ibeg + 1), iend - ibeg + 1)
      if(.not. movielabs) then
         call plchhq (xpos, ypos, label(1:iend - ibeg + 1), szsf,
     &                orient, center)
      endif
c
c
c     Write trajectory information at the top left of the plot
c     Add time and endpoints label.
c     
c
      value = trajbeg
      numlab = ftoa(value)
      call clean_zero(numlab(strbeg(numlab):strend(numlab)))
      label = 'Trajectory ('//
     &            numlab(strbeg(numlab):strend(numlab))
c
      value = trajend
      numlab = ftoa(value)
      call clean_zero(numlab(strbeg(numlab):strend(numlab)))
      label = label(strbeg(label):strend(label))//
     &       ' - '//numlab(strbeg(numlab):strend(numlab))//' s)'
c
c
      ibeg = 1
      iend = strend (label)
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
         ypos   = cfuy (vpt + 0.015)
      endif
      orient = 0.0
      if (movielabs) then
         call plchhq (xpos, ypos, label(ibeg:iend), (szsf * szsc)/1.7,
     &                 orient, center)
      else
         call plchhq (xpos, ypos, label(ibeg:iend), szsf/2.1,
     &                orient, center)
      endif
c
c
c     Create labels for the axes
c
c
      if (plot_number .eq. 1 ) then
         xx1 = x1
         xx2 = x2
         yy1 = y1
         yy2 = y2
         if(xaxis .eq. 0) then
            call scale (xx1, 1, data_slope, data_intercept, 0.0)
            call scale (xx2, 1, data_slope, data_intercept, 0.0)
         elseif(xaxis .ne. 5) then
            call scale (xx1, 1, domain_slope(xaxis), 
     &           domain_intercept(xaxis), 0.0)
            call scale (xx2, 1, domain_slope(xaxis), 
     &           domain_intercept(xaxis), 0.0)
         endif
         if(yaxis .eq. 0) then
            call scale (yy1, 1, data_slope, data_intercept, 0.0)
            call scale (yy2, 1, data_slope, data_intercept, 0.0)
         elseif(yaxis .ne. 5) then
            call scale (yy1, 1, domain_slope(yaxis), 
     &           domain_intercept(yaxis), 0.0)
            call scale (yy2, 1, domain_slope(yaxis), 
     &           domain_intercept(yaxis), 0.0)
         endif
         if(xmajor .eq. 0 .and. xaxis .ne. 0) then
c         xx1 = lolim(xaxis)
c         xx2 = uplim(xaxis)
            numlab = ftoa(xx1)
            call clean_zero(numlab(strbeg(numlab):strend(numlab)))
            xpos   = cfux (vpl)
            center = 0.0
            ypos   = cfuy (vpb - 0.02)
            orient = 0.0
           ibeg = 1
            iend = strend (numlab)
            call plchhq (xpos, ypos, numlab(ibeg:iend), szsf * szsc, 
     &           orient, center)
            
            numlab = ftoa(xx2)
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
            numlab = ftoa(yy1)
            call clean_zero(numlab(strbeg(numlab):strend(numlab)))
            xpos   = cfux (vpl - 0.01)
            center = 1.0
            ypos   = cfuy (vpb)
            orient = 0.0
            ibeg = 1
            iend = strend (numlab)
            call plchhq (xpos, ypos, numlab(ibeg:iend), szsf * szsc, 
     &           orient, center)
            
            numlab = ftoa(yy2)
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
c     axis label
c
      if ( curoverlay(curlist) .eq. 1 ) then
         if(xaxis .eq. 1) then
            label = 'X'
            ibeg = strbeg(label)
            iend = strend(label)
         elseif(xaxis .eq. 2) then
            label = 'Y'
            ibeg = strbeg(label)
            iend = strend(label)
         elseif(xaxis .eq. 3) then
            label = 'Z'
            ibeg = strbeg(label)
            iend = strend(label)
         end if
         if (nblank(domain_display_units(xaxis)) .ne. 0) then
            unitlabel = ' ('//
     &                  domain_display_units(xaxis)
     &                  (strbeg(domain_display_units(xaxis)):
     &                  strend(domain_display_units(xaxis)))//')'
         elseif ( nblank(domain_units(xaxis)) .ne. 0 ) then
            unitlabel = ' ('//
     &                  domain_units(xaxis)(strbeg(domain_units(xaxis)):
     &                  strend(domain_units(xaxis)))//
     &                  ')'
         else
            unitlabel = ' (unspecified Units)'
         endif
         label = label(ibeg:iend)//
     &               unitlabel(strbeg(unitlabel):strend(unitlabel))
         ibeg = strbeg(label)
         iend = strend(label)
         xpos = cfux ((vpr + vpl)/2)
         if(movielabs .or. tic_scale .gt. 1.2) then
            ypos = cfuy (vpb - .050)
         else
            ypos   = cfuy (vpb - .035)
         endif
         center = 0.0
         orient = 0.0
         call plchhq (xpos, ypos, label(ibeg:iend), szsf*szsc, 
     &               orient, center)
         
         if(yaxis .eq. 1) then
            label = 'X'
            ibeg = strbeg(label)
            iend = strend(label)
            
         elseif(yaxis .eq. 2) then
            label = 'Y'
            ibeg = strbeg(label)
            iend = strend(label)
         elseif(yaxis .eq. 3) then
            label = 'Z'
            ibeg = strbeg(label)
            iend = strend(label)
         endif
         if (nblank(domain_display_units(yaxis)) .ne. 0) then
            unitlabel = ' ('//
     &                  domain_display_units(yaxis)
     &                  (strbeg(domain_display_units(yaxis)):
     &                  strend(domain_display_units(yaxis)))//')'
         elseif ( nblank(domain_units(yaxis)) .ne. 0 ) then
            unitlabel = ' ('//
     &                  domain_units(yaxis)(strbeg(domain_units(yaxis)):
     &                  strend(domain_units(yaxis)))//
     &                  ')'
         else
            unitlabel = ' (unspecified Units)'
         endif
         label = label(ibeg:iend)//
     &               unitlabel(strbeg(unitlabel):strend(unitlabel))
         ibeg = strbeg(label)
         iend = strend(label)
         if(movielabs .or. tic_scale .gt. 1.2) then
            xpos   = cfux (vpl - 0.095)
         else
            xpos   = cfux (vpl - 0.075)
         endif
         ypos   = cfuy ((vpt + vpb)/2.0)
         center = 0.0
         orient = 90.0
         call plchhq (xpos, ypos, label(ibeg:iend), szsf*szsc, 
     &               orient, center)
      end if
c
c
c
c     Create labels indicating contour line type and color for 
c     each plot.
c
      shift = 0.15
      if ( plot_number .eq. 2 ) then
         xpos = cfux (vpr)
         sign = -1
      else if ( plot_number .eq. 1 ) then
         xpos = cfux (vpl)
         sign = 1
      else if ( plot_number .eq. 3 ) then
         xpos = cfux((vpl + vpr)/2) - shift/2
         sign = 1
      endif
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
