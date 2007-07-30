      subroutine labskewt (lolim, uplim, time, 
     &                       savflg, average, axes)
c-----------------------------------------------------------------------
c     $Id: labskewt.f,v 1.4 2000/05/09 19:53:51 harry Exp $
c
c     Purpose:
c        This routine is used to label skewt plots.
c     Arguments:
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
c     History:
c     $Log: labskewt.f,v $
c     Revision 1.4  2000/05/09 19:53:51  harry
c     Seperate contour and line colors.  Make patterns work like colors.
c
c     Revision 1.3  1997/07/02 20:08:07  warren
c     Fixes for problems found by DEC f90 compiler.
c
c     Revision 1.2  1996/06/18 18:47:05  harry
c     Add in call to user header transform (from David Bresch).
c
c     Revision 1.1  1996/04/25 16:25:20  harry
c     Add Skew-T plots and the beginning of trajectory plots.
c
c-----------------------------------------------------------------------
c
c
      include 'pltwin.icl'
      include 'units.icl'
      include 'segments.icl'
      include 'data.icl'
c     
c
c     Argument declarations.
c
      logical          savflg, average
      real             lolim, uplim, time, szsc
      integer          axes, mxdim
      dimension lolim(3), uplim(3), time(2)
c
c
c     Local variable declarations.
c
      character *(6)   title
      character *(256)  label, numlab
      character *(UNIT_LEN+2) unitlabel
      integer          i, ibeg, iend, ierror, line_index, lineclr,
     &                 linepat, linlog, 
     &                 sign, text_index, plot_number
      logical          error, movielabs
      real             vpl, vpr, vpb, vpt, wdl, wdr, wdb, wdt
      real             center, orient, size, szsf, shift, 
     &                 xpos, ypos, xpts, ypts, linewdth
      real             value, xmajor, ymajor

      parameter (title = 'Skew T')
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
      if (movielabs) then
         call pcseti ('QU', 0)            
         call pcsetc ('FN', 'HELVETICA-BOLD    ')
      else
         call pcseti ('QU',1)
         call pcseti ('CL',1)
      endif
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
      call getivar ('maxdim', mxdim, error)
      call getrvar('xmajor',xmajor,error)
      call getrvar('ymajor',ymajor,error)
      do 5 i = 1, mxdim
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
      ypos = cfuy (vpb - 0.045)
      orient = 0.0
      call upcase (label(1:iend - ibeg + 1), iend - ibeg + 1)
c     call user-defined heading routine (MOD DAVID BRESCH)
      call heading_trans(3,1,label(1:iend - ibeg + 1))
      ibeg = strbeg(label)
      iend = strend(label)
c      call upcase (label(1:iend - ibeg + 1), iend - ibeg + 1)
      if(.not. movielabs) then
         call plchhq (xpos, ypos, label(1:iend - ibeg + 1), szsf,
     &                orient, center)
      endif
c
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
      label =  '('//numlab(strbeg(numlab):strend(numlab))
c
      value = lolim(2)
      call scale (value, 1, domain_slope(2), domain_intercept(2), 0.0)
      numlab = ftoa(value)
      call clean_zero(numlab(strbeg(numlab):strend(numlab)))
      label = label(strbeg(label):strend(label))//
     &       ' '//numlab(strbeg(numlab):strend(numlab))
c
      value = lolim(3)
      call scale (value, 1, domain_slope(3), domain_intercept(3), 0.0)
      numlab = ftoa(value)
      call clean_zero(numlab(strbeg(numlab):strend(numlab)))
      label = label(strbeg(label):strend(label))//
     &       ' '//numlab(strbeg(numlab):strend(numlab))

      call scale (time(1), 1, domain_slope(4), domain_intercept(4), 0.0)
      numlab = ftoa(time(1))
      call clean_zero(numlab(strbeg(numlab):strend(numlab)))
      label = label(strbeg(label):strend(label))//
     &        ' '//numlab(strbeg(numlab):strend(numlab))//')'

      value = uplim(1)
      call scale (value, 1, domain_slope(1), domain_intercept(1), 
     &     0.0)
      numlab = ftoa(value)
      call clean_zero(numlab(strbeg(numlab):strend(numlab)))
      label =label(strbeg(label):strend(label))//
     &     ' to '//'('//numlab(strbeg(numlab):strend(numlab))
c
      value = uplim(2)
      call scale (value, 1, domain_slope(2), domain_intercept(2), 0.0)
      numlab = ftoa(value)
      call clean_zero(numlab(strbeg(numlab):strend(numlab)))
      label = label(strbeg(label):strend(label))//' '
     &        //numlab(strbeg(numlab):strend(numlab))
c
      value = uplim(3)
      call scale (value, 1, domain_slope(3), domain_intercept(3), 0.0)
      numlab = ftoa(value)
      call clean_zero(numlab(strbeg(numlab):strend(numlab)))
      label =label(strbeg(label):strend(label))//' '
     &       //numlab(strbeg(numlab):strend(numlab))

      call scale (time(2), 1, domain_slope(4), domain_intercept(4), 0.0)
      numlab = ftoa(time(2))
      call clean_zero(numlab(strbeg(numlab):strend(numlab)))
      label = label(strbeg(label):strend(label))//
     &        ' '//numlab(strbeg(numlab):strend(numlab))//')'
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
         ypos   = cfuy (vpb - 0.055)
      else
         ypos   = cfuy (vpb - 0.07)
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
         ypos = cfuy (vpb - (0.07))
      else
         ypos = cfuy (vpb - (0.06))
      endif
      call getivar ('hicolor', lineclr, error)
      call gsplci (lineclr)
      call getivar ('hipattern', linepat, error)
      call dashdb (linepat)
      xpts(1) = xpos
      xpts(2) = xpos + sign*shift
      ypts(1) = ypos
      ypts(2) = ypos
      call  gslwsc (linewdth+1.)
      call curved (xpts, ypts,2)
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




