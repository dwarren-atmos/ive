#ifdef SIXTY_FOUR_bptr
#define ive_ptr integer*8
#else
#define ive_ptr integer
#endif
      subroutine con2dc (data, nu, nv, mapflg, savflg, error)
c-----------------------------------------------------------------------
c     $Id: con2dc.F,v 1.46 2007-06-21 20:58:30 reinecke Exp $
c     Purpose:
c        This routine makes the set call and calls the contour drawing
c          routine for 2D contour line plots.
c     Arguments:
c        data   real  input   array containing data to be plotted.
c        nu     int   input   number of points to be contoured in the
c                             first subscript direction of data.
c        nv     int   input   number of points to be contoured in the
c                             second subscript direction of data.
c        mapflg  logi  input  indicates whether or not the plot is 
c                             projected onto a map background.
c        savflg  logi  input  indicates whether or not the current plot
c                             is saved (used to overlay plots). 
c        error  logi  output  indicates possible errors found in this
c                              routine.
c                              error = false  no errors detected
c                              error = true   error detected
c     History:
c      $Log: con2dc.F,v $
c      Revision 1.46  2007-06-21 20:58:30  reinecke
c      Commiting local modifications for transfer to svn
c
c      Revision 1.45  2007/05/25 18:06:56  reinecke
c      Took out print statements that I accidentally left in.
c
c      Revision 1.44  2007/05/25 16:10:04  reinecke
c      Units on labels are now domain_display_units instead of domain_units
c
c      Revision 1.43  2002/12/27 18:44:29  warren
c      First set of changes from zurich. use mapdrw_dl to deal with ever changing
c      borders in Europe. Increase arrays in work.
c
c      Revision 1.42  2002/08/09 19:57:56  warren
c      Fixed for Linux and tested with DEC sun etc.
c      most items inside #ifdef's, or were general cleanup/fixes
c
c      Revision 1.41  2000/05/09 19:53:43  harry
c      Seperate contour and line colors.  Make patterns work like colors.
c
c      Revision 1.40  1999/12/22 00:33:07  warren
c      Fixed Dales crashing bug when changing from point slicer to normal. Also
c      have some ifdef linuxes in there in case we ever find a fortran compiler.
c
c      Revision 1.39  1999/07/23 20:03:35  warren
c      Background code for the lock all fields option.
c
c      Revision 1.38  1997/04/18 22:09:39  harry
c      Fix routines to handle user errors more elegantly.
c
c      Revision 1.37  1997/04/16 16:52:28  warren
c      Final fix to color locking. When freeing set all variables needed.
c
c      Revision 1.36  1997/03/07 15:39:59  warren
c      Fixed multiple declarations to make Alpha happy.
c
c      Revision 1.35  1997/03/06 19:00:00  harry
c      Make changes to properly handle exact_fit case for mouse slicing.
c
c      Revision 1.34  1997/02/12 17:35:46  warren
c      Changed pointer type to ive_ptr to avoid interaction with F90 pointer type.
c
c      Revision 1.33  1996/11/15 21:15:55  harry
c      Vector plots should be labelled by the data units, not display data units.
c
c      Revision 1.32  1996/10/25 16:31:54  harry
c      crec needs to be a 4 element array.
c
c      Revision 1.31  1996/10/18 23:22:05  harry
c      Do not call driver to free variable, just do it in con2ds.
c
c Revision 1.30  1996/10/11  01:12:47  warren
c To fill or not to fill is no longer the only question. Now there is a seperate
c question of solid or pattern. Also first part of mouse slicing. 2d -> 1d
c Labels are not fixed yet.
c
c      Revision 1.29  1996/07/26 21:03:29  harry
c      Change slope and intercept to real*8.
c
c      Revision 1.28  1996/04/25 16:25:07  harry
c      Add Skew-T plots and the beginning of trajectory plots.
c
c      Revision 1.27  1996/04/12 18:33:11  harry
c      Fix lab2d to restore clipping after the routine is finished.
c
c      Revision 1.26  1996/02/27 23:15:10  harry
c      Fix labels to use new netCDF attributes.
c
c      Revision 1.25  1996/02/23 22:31:24  harry
c      Fix for reset_spval feature for new slicer.
c
c Revision 1.24  1995/10/04  22:09:47  harry
c Add new netCDF attributes and specification of staggering.  Add
c changes to slicer and plot labels to reflect user specified labels.
c
c Revision 1.23  1995/09/05  20:33:37  harry
c Add new slicer.  Good luck!
c
c Revision 1.22  1995/08/28  18:10:55  harry
c Take out unnecessary passing of terrain variables in filled contour plots.
c
c Revision 1.21  1995/08/24  20:07:55  harry
c Add David Bresch and Chris Schaer's heading transform routine.
c
c Revision 1.20  1995/06/23  21:16:11  warren
c First part of new ive help
c Changed " to ' in fortran files
c Fixed the label spacing in 1d plots
c added frame_width command
c
c Revision 1.19  1995/01/09  18:52:24  warren
c Moved the line color rotation selection from everywhere to makseg.
c Note: makseg now has one argument. it should be called with true if
c you only use hicolor and false if you use two colors in the type of plot.
c
c Revision 1.18  1994/06/28  17:43:26  warren
c First cut at fancy tic marks.
c
c Revision 1.17  1994/06/23  21:33:11  harry
c maxdim is a parameter, use max_dim for the variable name.
c
c Revision 1.16  1994/06/11  21:33:14  yuri
c Added more comments.
c
c Revision 1.15  1994/03/29  23:56:34  harry
c Correct contour min/max/interval label and make sure calls to gslwsc
c have a real parameter.
c
c Revision 1.14  1994/03/29  23:31:14  warren
c fixed label for vector plots.
c
c Revision 1.13  1994/03/25  23:12:25  harry
c Fixes to plot labelling again.
c
c Revision 1.12  1994/03/22  21:32:03  warren
c fixed bad vectors at the bottom of Dale's plot and made vector labels
c behave better (they no longer overwrite the solid fill labels)
c
c Revision 1.11  1994/03/21  18:38:32  warren
c fixed time=0 problem
c
c Revision 1.10  1994/02/23  00:39:41  warren
c final final changes to labes for 3.1
c
c Revision 1.9  1994/02/23  00:15:25  warren
c Final 3.1 label changes.
c
c Revision 1.8  1994/02/03  22:42:49  warren
c last of the movie labels and made the tic mark labels smarter.
c
c Revision 1.7  1994/02/01  20:15:44  warren
c passed error into set2d from con2ds (it was missing)
c
c Revision 1.6  1994/02/01  18:13:26  warren
c changed '' to ' ' for dec compiler
c
c Revision 1.5  1994/02/01  02:06:57  warren
c Cleaned up line and area fill movie labels somewhat.
c
c Revision 1.4  1994/01/29  00:11:27  warren
c first cut at movie labels. Line plots still need work
c
c Revision 1.3  1994/01/20  23:17:05  yuri
c New format of numbers in averaging messages. (After long long testing).
c
c Revision 1.2  1994/01/20  20:44:27  yuri
c Propagating of "overlay_patterns" stuff to newer version.
c
c Revision 1.1  1993/11/29  19:38:58  warren
c Changes to 3.1 to make it work on alphas.
c FORTRAN: new data type ive_ptr ifdefed to either 32 bit or 64 bit.
c C: Change int to long where necessary.
c Files: Quite a few .f files are now .F
c
c Revision 1.16  1993/11/17  18:31:49  yuri
c Now field name is the same as button name.
c
c Revision 1.15  1993/10/27  16:40:56  yuri
c Added stuff for checking log(negative limits) in 1D and 2D cases
c with separate messages for horisontal and vertical axes.
c
c Revision 1.14  1993/10/08  17:10:59  yuri
c Labels corrected in case of time is averaged.
c
c Revision 1.13  1993/07/16  15:47:20  warren
c make sure that labels don't over run array.
c
c Revision 1.12  1993/07/15  22:13:08  warren
c Fixed scaling for default 1d ticmarks. Also fixed plotting under terrain
c problem.
c
c Revision 1.11  1993/07/15  00:10:24  yuri
c 	Corrected problem with colors in con2ds.
c
c Revision 1.10  1993/07/09  19:09:25  warren
c Pretty labels to go with the 2d ticmarks. Also I removed superfluous calls
c to chkwin and moved all the others to driver.f.
c
c Revision 1.9  1993/07/03  01:53:14  yuri
c Tuning of overlay colors.
c
c Revision 1.8  1993/07/02  00:18:15  warren
c Some work on color labels.
c
c Revision 1.7  1993/07/01  22:02:06  warren
c Nicer labeling when doing tic marks.
c
c Revision 1.6  1993/06/11  18:21:09  yuri
c New colors for overlay.
c
c Revision 1.5  1993/06/01  20:48:53  yuri
c 	Lables for vectors/solid plots are updated too.
c
c Revision 1.4  1993/06/01  19:50:23  yuri
c Second attempt.
c
c Revision 1.2  1993/05/23  23:40:45  yuri
c Inserted possibility "nblank(data_display_units = 0" too.
c
c Revision 1.1.1.1  1993/04/07  19:39:50  harry
c Initial CVS checkin of IVE.
c
c Revision 2.15  1993/01/13  19:49:25  millerp
c Change argument for curoverlay to curlist.
c
c Revision 2.14  1993/01/08  16:42:16  harry
c Only plot labels for 1st two overlays.  Move side labels out for
c 2nd overlay.
c
c Revision 2.13  1993/01/06  21:53:36  millerp
c remove multiply defined variables spval and reset_sval.
c
c Revision 2.12  1993/01/05  20:15:11  warren
c set cint from cont_int property and nozero from no_zero property
c
c Revision 2.11  1992/12/03  00:02:33  millerp
c Fix up labeling for surface plots.
c
c Revision 2.10  1992/11/24  16:33:03  millerp
c Make changes related to removing hslice, vsice.
c
c Revision 2.9  1992/11/18  21:45:20  millerp
c Removed debugging write statements.
c
c Revision 2.8  1992/11/06  17:25:04  warren
c removed multiple maxdim declarations.
c
c Revision 2.7  1992/11/05  20:30:28  millerp
c Added stuff for averages and time-dependent cross sections.
c
c Revision 2.6  1992/10/15  23:01:21  warren
c added color table locking
c
c Revision 2.5  1992/09/24  23:14:25  harry
c Add set2d call for con2ds.
c
c Revision 2.4  1992/09/17  15:42:49  warren
c changed the slicing syntax to use xloc, yloc, zloc and time instead of
c hloc and vloc. this include changes to the common blocks and command set.
c
c Revision 2.3  1992/09/08  19:45:41  millerp
c Added stuff for new slicers.
c
c Revision 2.2  1992/07/16  16:18:44  millerp
c Fixed label sizes and fonts; modified contour line type,color lab
c
c Revision 2.1  1992/07/06  22:03:25  millerp
c Added tic mark scaling for color filled plots.
c
c Revision 2.0  1992/07/01  21:12:43  millerp
c Released version rereving to 2.0
c
c Revision 1.5  1992/06/15  18:59:08  millerp
c Change argument list for scale.
c
c Revision 1.4  1992/06/10  20:05:11  millerp
c Re-do labeling and scaling.
c
c Revision 1.3  1992/05/29  22:39:57  millerp
c *** empty log message ***
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
      include 'attributes.icl'
      include 'conre.icl'
      include 'data.icl'
      include 'plotops.icl'
      include 'units.icl'
      include 'color.icl'
      include 'pltwin.icl'
      include 'supma.icl'
c
c
c     Argument declarations.
c
      integer     nu, nv
      real        data
      logical     error, mapflg, savflg
      dimension data(nu, nv)
c
c
c     Local variable declarations.
c
c      integer         xaxis, yaxis ! which axes go which way (in pltwin.icl)
      logical         local_err,   ! error flag
     &     movielabs            ! DW's stuff
      real            lolim, uplim ! min, max limits 
c
      real            xmajr, xminr, ymajr, yminr 
c     ticks - minor and major /\
      integer         axes
      real            vbot, vtop, vleft, vrght, 
     &                wbot, wtop, wleft, wrght 
c
      real            ORVAL
      parameter ( ORVAL = 1.0e12 )
c
      dimension       lolim(MAXDIM), uplim(MAXDIM)
c
c     Initialization
c
      error = .false.
      local_err = .false.
      call getlvar('movielabs',movielabs,error)
      if ( error ) then
         return
      endif
      if (movielabs) then
         call pcseti ('QU',0)
         call pcsetc ('FN', 'HELVETICA-BOLD    ')
      else
         call pcseti ('QU',1)
         call pcseti ('CL',1)
      endif
c
c
c     Use conpack out-of-range feature to avoid unwanted contour lines.
c     Note that this particular value is used because the EZMAP routines
c     use it by default.
c
      call cpsetr ('ORV - out-of-range value', ORVAL)
c
c     Make appropriate set call.
c     set2d sets up: limits, ticks and colors
c
      call set2d (nu, mapflg, savflg, lolim, uplim, local_err)
      if( local_err)then
         return
      endif
c
c
c     Scale data if necessary.
c
      call scale (data, nu*nv, data_slope, data_intercept, spval)
c
c
c     Set the contour interval from attributes
c
      cint = cont_int
      if(no_zero) then
         nozero = .true.
      else
         nozero = .false.
      endif
c
c     Plot data (just ncar calls)
c
      call cpline (data, nu, nu, nv, local_err)
      error = error .or. local_err
c
c
c     If the contouring routines encountered any errors, then return.
c
      if ( error ) then
         return
      endif
c
c
c     Draw either a map foreground or perimeter, whichever is 
c     appropriate.
c
      if ( .not. savflg ) then
         if ( mapflg ) then
            call mapdrw_dl
            if (defmap .and. exact_fit) then
               call getset ( vleft, vrght, vbot, vtop, 
     &              wleft, wrght, wbot, wtop, axes)
               call set ( vleft, vrght, vbot, vtop,
     &              x1, x2, y1, y2, axes)
            endif
         endif 

         if ( .not. mapflg .or. (defmap .and. exact_fit)) then
            xmajr = xmajor
            xminr = xminor
            ymajr = ymajor
            yminr = yminor
c            call getivar ('xaxis', xaxis, error)
c            call getivar ('yaxis', yaxis, error)
            call scaletics (xmajr, xminr, ymajr, yminr,
     &           mod(xaxis,10), mod(yaxis,10))
            call uwperim (xmajr, xminr, ymajr, yminr)
         endif
      endif
c
c
c     Label plot.
c
      
      if ( label ) then
         call lab2d (datfil, button_name, lolim, uplim,
     &                savflg, mapflg, surface, .false., .false.)
      endif
c     
c

c
c     Release all output.
c
      call plotit (0, 0, 0)
c
c     Reset out-of-range value to its default.
c
      call cpsetr ('ORV -out-of-range value', 0.0)
c
c
      return
      end
c
c
      subroutine con2ds (data, nu, nv, mapflg, savflg, error)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine makes the set call and calls the contour drawing
c          routine for 2D contour line plots.
c     Arguments:
c        data   real  input   array containing data to be plotted.
c        nu     int   input   number of points to be contoured in the
c                             first subscript direction of data.
c        nv     int   input   number of points to be contoured in the
c                             second subscript direction of data.
c        mapflg  logi  input  indicates whether or not the plot is 
c                             projected onto a map background.
c        savflg  logi  input  indicates whether or not the current plot
c                             is saved (used to overlay plots). 
c        error  logi  output  indicates possible errors found in this
c                              routine.
c                              error = false  no errors detected
c                              error = true   error detected
c     History:
c-----------------------------------------------------------------------
c
      include 'attributes.icl'
      include 'conre.icl'
      include 'data.icl'
      include 'plotops.icl'
      include 'color.icl'
      include 'units.icl'
      include 'message.icl'
      include 'pltwin.icl'
      include 'supma.icl'
c
c
c     Argument declarations.
c
      integer     nu, nv
      real        data
      logical     error, mapflg, savflg
      dimension data(nu, nv)
c
c
c     External Functions
c
      integer strbeg, strend, nblank
c
c
c     Local variable declarations.
c
c      integer         xaxis, yaxis ! which axes go which way
      logical         local_err,   ! error flag
     &     movielabs               ! DW's stuff
      real            lolim, uplim ! min, max limits 
      real            xmajr, xminr, ymajr, yminr ! ticks - minor and major
      integer         axes
      real            vbot, vtop, vleft, vrght, 
     &                wbot, wtop, wleft, wrght 
c

      real            ORVAL
      parameter ( ORVAL = 1.0e12 )
      dimension       lolim(MAXDIM), uplim(MAXDIM)
c
c   Initialization
c
      error = .false.
      local_err = .false.
      call getlvar('movielabs',movielabs,error)
      if ( error ) then
         return
      endif
      if (movielabs) then
         call pcseti ('QU',0)
         call pcsetc ('FN', 'HELVETICA         ')
         call pcseti ('CC', 1)
         call pcseti ('CC(1)', 1)
         call pcseti ('CC(2)', 1)
         call pcseti ('CC(3)', 1)
         call pcseti ('CC(4)', 1)
         call pcseti ('CC(5)', 1)
         call pcseti ('CC(6)', 1)
         call pcseti ('CC(7)', 1)
         call pcseti ('CC(8)', 1)
         call pcseti ('CC(9)', 1)
         call pcseti ('CC(10)', 1)
         call pcseti ('CC(11)', 1)
         call pcseti ('CC(12)', 1)
         call pcseti ('CC(13)', 1)
         call pcseti ('CC(14)', 1)
         call pcseti ('CC(15)', 1)
         call pcseti ('CC(16)', 1)
      else
         call pcseti ('QU',1)
         call pcseti ('CL',1)
      endif
c     
c     get special value - VERY important
c
c     all getrvar ('spval', spval, local_err)
      error = error .or. local_err
      if ( error ) then
         return
      endif
c
c
c     Use conpack out-of-range feature to avoid unwanted contour lines.
c     Note that this particular value is used because the EZMAP routines
c     use it by default.
c
      call cpsetr ('ORV - out-of-range value', ORVAL)
c
c
c     Make appropriate set call.
c     set2d sets up: limits, ticks and colors
c
      call set2d (nu, mapflg, savflg, lolim, uplim, error)
      if (error) return
c
c     Scale data if necessary.
c
      call scale (data, nu*nv, data_slope, data_intercept, spval)
c
c
c     Plot data.
c     
      if(ignore_lock_var .ne. 0) then
         if (midval .ne. 0 .and. nblank(lock_field) .ne. 0) then
            if(field(strbeg(field):strend(field)) .ne.
     &           lock_field(strbeg(lock_field):strend(lock_field))) then
               lock_field = ' '
               lock_entry = 0
               midval = 0
               lock_int = 0
            endif
         endif
      endif
c
c     Plot data (just ncar calls)
c
      call cpsoli (data, nu, nu, nv, local_err)
      if (local_err) return
      if(midval .ne. 0)lock_field = field
c
c
c     Draw either a map foreground or perimeter, whichever is 
c     appropriate.
c
      if ( .not. savflg ) then
         if ( mapflg ) then
            call mapdrw
            if (defmap .and. exact_fit) then
               call getset ( vleft, vrght, vbot, vtop, 
     &              wleft, wrght, wbot, wtop, axes)
               call set ( vleft, vrght, vbot, vtop,
     &              x1, x2, y1, y2, axes)
            endif
	 endif
	 if ( .not. mapflg .or. (defmap .and. exact_fit)) then
            xmajr = xmajor
            xminr = xminor
            ymajr = ymajor
            yminr = yminor
c            call getivar ('xaxis', xaxis, error)
c            call getivar ('yaxis', yaxis, error)
            call scaletics (xmajr, xminr, ymajr, yminr,
     &           mod(xaxis,10), mod(yaxis,10))
            call uwperim (xmajr, xminr, ymajr, yminr)
         endif
      endif
      call plotit (0, 0, 0)
c     
c     
c     Label plot.
c     
      if ( label ) then
         call lab2d (datfil, button_name, lolim, uplim,
     &                savflg, mapflg, surface, .true., .false.)
      endif
c

c
c     Release all output.
c
      call plotit (0, 0, 0)
c
c     Reset out-of-range value to its default.
c
      call cpsetr ('ORV -out-of-range value', 0.0)
c
c
      return
      end
c
c
      subroutine lab2d (datfil, field, lolim, uplim,
     &                   savflg, mapflg, surface, color, vector)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine is used to label 2d contour line plots.
c     Arguments:
c        datfil  char  input   the data file name.
c        field   char  input   the plotted field.
c        lolim   real  input   array containing the plot window
c                              coordinates at the lower left corner.
c        uplim   real  input   array containing the plot window
c                              coordinates at the upper right corner.
c        savflg  logi  input   indicates whether the save flag is on or
c                              off.
c        mapflg  logi  input   indicates whether the map flag is on or
c                              off.
c        surface  logi  input  indicates whether this is a plot of 
c                              data at the surface.
c        color    logi  input  indicates this is a color fill plot.
c        vector   logi  input  indicates that this is a vector plot.
c     History:
c-----------------------------------------------------------------------
c
c
      include 'units.icl'
      include 'segments.icl'
c
c
c     Argument declarations.
c
      integer          MAXDIM
      parameter (MAXDIM = 4)
      character *(*)   datfil, field
      logical          savflg, mapflg, surface, color, vector
      real             lolim, uplim, plwmin_scaled, plwmax_scaled,
     &                 xmajor, ymajor
      dimension lolim(MAXDIM), uplim(MAXDIM),
     &          plwmin_scaled(MAXDIM), plwmax_scaled(MAXDIM)
c
c
c     Local variable declarations.
c
      character *(80)  label, numlab, numlab2,  plotyp,
     &	 dim_names(4)
      character *(UNIT_LEN+2) unitlabel
      integer          cff, i, ibeg, iend, ierror, line_index, 
     &                 linepat1, lineclr1, linepat2, lineclr2, linlog, 
     &                 nlevs, sign, status, text_index, tictest, 
     &                 lock(4), plot_number, max_dim
      integer          xaxis, yaxis, oldclp, filltyp, fill_frequency
      logical          error, movielabs
      real             offset, shift, value,szsc
      real             vpl, vpr, vpb, vpt, wdl, wdr, wdb, wdt,
     &                 linewdth, crec(4)
      real             center, cint, cmax, cmin, orient, size, szsf, 
     &                 xpos, ypos, xpts, ypts, loc(4)
      real*8           slope, intercept
      dimension xpts(2), ypts(2)
      data  size / 0.019 /
c
c     External function declarations.
c
      character *(80) ftoa
      integer         convert_units, nblank, strbeg, strend
      real            cfux, cfuy, tic_scale
      integer ibuf1,ibuf2,ibuf3
c
c
c     Constants from plotops.icl which can't be included here
c
C     Fill defines
      integer SOLID, PATTERN
      integer NEVER, EXECPT_OVERLAY, ALWAYS
 
      parameter(        SOLID   =  1,
     &                  PATTERN = 2)
 
      parameter(        NEVER          =  1,
     &                  EXECPT_OVERLAY =  2,
     &                  ALWAYS         =  3)
c
c
      call getivar ('xaxis', xaxis, error)
      call getivar ('yaxis', yaxis, error)
      xaxis = mod(xaxis,10)
      yaxis = mod(yaxis,10)
      call getaarr('dim_display_names', dim_names, 4, error)
      call getrvar ('tic_scale', tic_scale, error)
      call getrvar ('xmajor', xmajor, error)
      call getrvar ('ymajor', ymajor, error)
      tictest=0
      if(xmajor .gt. 0.0)tictest=1
      if(ymajor .gt. 0.0)tictest=1
      call getrvar('linwdth', linewdth, error)
      call getivar('filtyp', filltyp, error)
      call getivar('fill_frequency', fill_frequency, error)
      call getavar('plotyp2', plotyp, error)
      call getlvar('movielabs',movielabs,error)
      if(movielabs) then
         szsc = 1.
      else
         szsc = .70
      endif
      if(.not. movielabs .and. (filltyp .eq. SOLID) .and.
     &     fill_frequency .ne. NEVER) then
         plot_number = curoverlay(curlist) - 1
      else
         plot_number = curoverlay(curlist)
      endif
      if(error) then
         return
      endif
      if (plot_number .eq. 0)plot_number = 1
c
c     Only label first 2 overlays (or 3 if first is color fill).
c
      if (plot_number .gt. 2) return
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
      call gqclip(ierror, oldclp, crec)
      call gsclip (0)
      call getset (vpl, vpr, vpb, vpt, wdl, wdr, wdb, wdt, linlog)
      szsf = size * (vpr - vpl)
c
c     Cat nulls to strings sent to C routines.
c
      call catnull (data_units, len(data_units), error)
      call catnull (data_units, len(data_display_units), error)
      call getivar ('maxdim', max_dim, error)
      do 5 i = 1, max_dim
         call catnull (domain_units(i), len(domain_units(i)), error)
         call catnull (domain_display_units(i),
     &                 len(domain_display_units(i)), error)
 5    continue
      call getrarr('scaled_loc',loc,4,error)
      call getiarr('lock',lock,4,error)
c
c     Set up unit label.
c
      if ( nblank(data_units) .eq. 0 
     & .and. nblank(data_display_units) .eq. 0) then
         unitlabel = ' '
      else
         status = convert_units (data_units, data_display_units, 
     &                           slope, intercept)
         if ( status .eq. 0 .and. .not. vector .and.
     &        nblank(data_display_units) .ne. 0 ) then
            unitlabel = '('//
     &                  data_display_units(strbeg(data_display_units):
     &                                     strend(data_display_units))//
     &                  ')'
            else
            unitlabel = '('//
     &                  data_units(strbeg(data_units):
     &                             strend(data_units))//
     &                  ')'
            endif
      endif
c
c     Get length back from right to write label for tic
c
      call getrarr('plwmin_scaled',plwmin_scaled,MAXDIM,error)
      call getrarr('plwmax_scaled',plwmax_scaled,MAXDIM,error)
c
c     Call set so we can use fractional coordinates.
c
      call set (vpl, vpr, vpb, vpt, 0.0, 1.0, 0.0, 1.0, 1)
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
      if(movielabs) then
         label = ' '
      else
         ibeg = strbeg(label)
         iend = strend(label)
c         call upcase (label(ibeg:iend - ibeg + 1), iend - ibeg + 1)
      endif
      if (mapflg) then
         if(lock(4) .eq. 1) then
            numlab = ftoa(loc(4))
            call clean_zero(numlab(strbeg(numlab):strend(numlab)))
            if(movielabs) then
               label = 'TIME = '//
     &              numlab(strbeg(numlab):strend(numlab))//
     &              ' '//
     &              domain_display_units(4)(
     &              strbeg(domain_display_units(4)):
     &              strend(domain_display_units(4)))
               ibeg = strbeg(label)
               iend = strend(label)
            else
               label = label(strbeg(label):strend(label))//
     &              ' AT TIME = '//
     &              numlab(strbeg(numlab):strend(numlab))//
     &              ' '//
     &              domain_display_units(4)(
     &              strbeg(domain_display_units(4)):
     &              strend(domain_display_units(4)))
               iend = strend(label)
            endif
         else
            numlab = ftoa(plwmin_scaled(4))
            call clean_zero(numlab(strbeg(numlab):strend(numlab)))
            numlab2 = ftoa(plwmax_scaled(4))
            call clean_zero(numlab2(strbeg(numlab2):strend(numlab2)))
            if(movielabs) then
               label = 'time from '//
     &              numlab(strbeg(numlab):strend(numlab))//
     &              ' to '//
     &              numlab2(strbeg(numlab2):strend(numlab2))//
     &              domain_display_units(4)(
     &              strbeg(domain_display_units(4)):
     &              strend(domain_display_units(4)))
               ibeg = strbeg(label)
               iend = strend(label)
            else
               label = label(strbeg(label):strend(label))//
     &              ' time from '//
     &              numlab(strbeg(numlab):strend(numlab))//
     &              ' to '//
     &              numlab2(strbeg(numlab2):strend(numlab2))//
     &              domain_display_units(4)(
     &              strbeg(domain_display_units(4)):
     &              strend(domain_display_units(4)))
               iend = strend(label)
            endif
         endif
      endif
c
c     Place label just above top plot border.
c
      ypos = cfuy (vpt + 0.04)
      if(color .and. .not. movielabs .and.
     &     .not. (plotyp(1:1) .eq. 'V' .and. 
     &     (plot_number .ne. curoverlay(curlist)))) then
         xpos = cfux ((vpr + vpl)/2.0) 
         center = 0.0
         if(mapflg) ypos = cfuy (vpb - 0.04)
      else if ( plot_number .eq. 2 ) then
         xpos = cfux (vpr) 
         center = 1.0
         if(mapflg) ypos = cfuy (vpb - 0.04)
      else
         xpos = cfux (vpl)
         center = -1.0
      endif
      orient = 0.0
c call user-defined heading routine (MOD DAVID BRESCH)
      call heading_trans(1,1,label(ibeg:iend-ibeg+1))
      ibeg = strbeg(label)
      iend = strend(label)
      if(label .ne. ' ') then
         call plchhq (xpos, ypos, label(ibeg:iend-ibeg+1), szsf*szsc,
     &                orient, center)
      endif
c
c
c     Write variable name and units at top of plot.
c
      ibeg = strbeg(field)
      iend = strend(field)
      label = field(ibeg:iend)
c
      if ( surface ) then
         label = label(strbeg(label):strend(label))//
     &           ' '//
     &           unitlabel(strbeg(unitlabel):strend(unitlabel))//
     &           ' at SURFACE'
      else
         label = label(strbeg(label):strend(label))//
     &           ' '//
     &           unitlabel(strbeg(unitlabel):strend(unitlabel))
      endif
      if (mapflg) then
         numlab = ftoa(loc(3))
         call clean_zero(numlab(strbeg(numlab):strend(numlab)))
         label = label(strbeg(label):strend(label))//
     &           ' at '//
     &           numlab(strbeg(numlab):strend(numlab))//
     &           ' '//
     &              domain_display_units(3)(
     &              strbeg(domain_display_units(3)):
     &              strend(domain_display_units(3)))
      endif
      ibeg = 1
      iend = strend (label)

      ypos   = cfuy (vpt + 0.015)
      if(color .and. .not. movielabs .and.
     &     .not. (plotyp(1:1) .eq. 'V' .and. 
     &     (plot_number .ne. curoverlay(curlist)))) then
         xpos = cfux ((vpr + vpl)/2.0) 
         center = 0.0
         if(mapflg) ypos = cfuy (vpb - 0.015)
      else if ( plot_number .eq. 2 ) then
         xpos   = cfux (vpr) 
         center = 1.0
         if(mapflg)ypos   = cfuy (vpb - 0.015)
      else
         xpos   = cfux (vpl)
         center = -1.0
      endif
      orient = 0.0
c call user-defined heading routine (MOD DAVID BRESCH)
      call heading_trans(1,2,label(ibeg:iend))
      ibeg = strbeg (label)
      iend = strend (label)
      call plchhq (xpos, ypos, label(ibeg:iend), szsf*szsc,
     &              orient, center)
c
c
      if ( .not. mapflg ) then
         if(xmajor .ne. 0.0 .or. ymajor .ne. 0.0)then
            label = ' '
            do 1000 i = 1, 4
               if(lock(i) .eq. 1) then
                  if(i .eq. 1) then
                     numlab2 = ftoa(loc(1))
                     call clean_zero(numlab2)
                     numlab = '   X='//numlab2(strbeg(numlab2):
     &                                         strend(numlab2))
                  else if (i .eq. 2) then
                     numlab2 = ftoa(loc(2))
                     call clean_zero(numlab2)
                     numlab = '   Y='//numlab2(strbeg(numlab2):
     &                                         strend(numlab2))
                  else if (i .eq. 3) then
                     numlab2 = ftoa(loc(3))
                     call clean_zero(numlab2)
                     numlab = '   Z='//numlab2(strbeg(numlab2):
     &                                         strend(numlab2))
                  else if (i .eq. 4) then
                     numlab2 = ftoa(loc(4))
                     call clean_zero(numlab2)
                     numlab = '   TIME='//numlab2(strbeg(numlab2):
     &                                         strend(numlab2))
                  endif
               else if (lock(i) .eq. 2) then
                  if(i .eq. 1) then
                     numlab2 = ftoa(plwmin_scaled(1))
                     call clean_zero(numlab2)
                     numlab = '   X Avg '//numlab2(strbeg(numlab2):
     &                        strend(numlab2))
                     numlab2 = ftoa(plwmax_scaled(1))
                     call clean_zero(numlab2)
                     numlab = numlab(strbeg(numlab):strend(numlab))//
     &                    ' to '//numlab2(strbeg(numlab2):
     &                    strend(numlab2))
                  else if (i .eq. 2) then
                     numlab2 = ftoa(plwmin_scaled(2))
                     call clean_zero(numlab2)
                     numlab = '   Y Avg '//numlab2(strbeg(numlab2):
     &                    strend(numlab2))
                     numlab2 = ftoa(plwmax_scaled(2))
                     call clean_zero(numlab2)
                     numlab = numlab(strbeg(numlab):strend(numlab))//
     &                    ' to '//numlab2(strbeg(numlab2):
     &                    strend(numlab2))
                  else if (i .eq. 3) then
                     numlab2 = ftoa(plwmin_scaled(3))
                     call clean_zero(numlab2)
                     numlab = '   Z Avg '//numlab2(strbeg(numlab2):
     &                    strend(numlab2))
                     numlab2 = ftoa(plwmax_scaled(3))
                     call clean_zero(numlab2)
                     numlab = numlab(strbeg(numlab):strend(numlab))//
     &                    ' to '//numlab2(strbeg(numlab2):
     &                    strend(numlab2))
                  else if (i .eq. 4) then
                     numlab2 = ftoa(plwmin_scaled(4))
                     call clean_zero(numlab2)
                     numlab = '   Time Avg '
     &                    //numlab2(strbeg(numlab2):
     &                    strend(numlab2))
                     numlab2 = ftoa(plwmax_scaled(4))
                     call clean_zero(numlab2)
                     numlab = numlab(strbeg(numlab):strend(numlab))//
     &                    ' to '//numlab2(strbeg(numlab2):
     &                    strend(numlab2))
                  endif
               else
                  numlab = ' '
               endif
               if(numlab .ne. ' ') then
                  call clean_zero(numlab(strbeg(numlab):strend(numlab)))
                  label = label(strbeg(label):strend(label))//
     &                 numlab(1:strend(numlab))
                  iend = strend(label)
               endif
 1000       enddo
c     
c     Place label at bottom of plot border.
c     
            if(color .and. .not. movielabs .and.
     &     .not. (plotyp(1:1) .eq. 'V' .and. 
     &           (plot_number .ne. curoverlay(curlist)))) then
               xpos = cfux ((vpr + vpl)/2.0) 
               center = 0.0
            else if ( plot_number .eq. 2 ) then
               xpos = cfux (vpr) 
               center = 1.0
            else
               xpos = cfux (vpl)
               center = -1.0
            endif
            ypos = cfuy (szsf)
            orient = 0.0
            if(label .ne. ' ') then
               call plchhq (xpos, ypos, label(ibeg:iend-ibeg+1), 
     &              szsf* szsc, orient,  
     &              center)
            endif
c     
            if ( curoverlay(curlist) .eq. 1 ) then
               label = dim_names(xaxis)
               ibeg = strbeg(label)
               iend = strend(label)
               if (nblank(domain_display_units(xaxis)) .ne. 0) then
                  unitlabel = ' ('//
     &                 domain_display_units(xaxis)
     &                 (strbeg(domain_display_units(xaxis)):
     &                 strend(domain_display_units(xaxis)))//')'
               elseif ( nblank(domain_units(xaxis)) .ne. 0 ) then
                  unitlabel = ' ('//
     &                 domain_units(xaxis)(strbeg(domain_units(xaxis)):
     &                 strend(domain_units(xaxis)))//
     &                 ')'
               else
                  unitlabel = ' (unspecified Units)'
               endif
               label = label(ibeg:iend)//
     &              unitlabel(strbeg(unitlabel):strend(unitlabel))
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
     &              orient, center)

               label = dim_names(yaxis)
               ibeg = strbeg(label)
               iend = strend(label)
               if (nblank(domain_display_units(yaxis)) .ne. 0) then
                  unitlabel = ' ('//
     &                 domain_display_units(yaxis)
     &                 (strbeg(domain_display_units(yaxis)):
     &                 strend(domain_display_units(yaxis)))//')'
               elseif ( nblank(domain_units(yaxis)) .ne. 0 ) then
                  unitlabel = ' ('//
     &                 domain_units(yaxis)(strbeg(domain_units(yaxis)):
     &                 strend(domain_units(yaxis)))//
     &                 ')'
               else
                  unitlabel = ' (unspecified Units)'
               endif
               label = label(ibeg:iend)//
     &              unitlabel(strbeg(unitlabel):strend(unitlabel))
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
     &              orient, center)
            endif
         endif
         if(xmajor .eq. 0.0 .and. ymajor .eq. 0.0)then
c
c           Write plot limits at bottom left of plot.
c     
            value = lolim(1)
            call scale (value, 1, domain_slope(1), domain_intercept(1), 
     &           0.0)
            numlab = ftoa(value)
            call clean_zero(numlab(strbeg(numlab):strend(numlab)))
            label = '('//numlab(strbeg(numlab):strend(numlab))
c
            value = lolim(2)
            call scale (value, 1, domain_slope(2), domain_intercept(2), 
     &                  0.0) 
            numlab = ftoa(value)
            call clean_zero(numlab(strbeg(numlab):strend(numlab)))
            label = label(strbeg(label):strend(label))//' , '
     &           //numlab(strbeg(numlab):strend(numlab))
c     
            if (surface) then
               numlab = 'sfc'
            else
               value = lolim(3)
               call scale (value, 1, domain_slope(3), 
     &                     domain_intercept(3), 0.0)
               numlab = ftoa(value)
               call clean_zero(numlab(strbeg(numlab):strend(numlab)))
            endif
            label = label(strbeg(label):strend(label))//' , '
     &           //numlab(strbeg(numlab):strend(numlab))
c
            value = lolim(4)
            call scale (value, 1, domain_slope(4), domain_intercept(4), 
     &                  0.0)
            numlab = ftoa(value)
            call clean_zero(numlab(strbeg(numlab):strend(numlab)))
            label = label(strbeg(label):strend(label))//' , '
     &           //numlab(strbeg(numlab):strend(numlab))//')'
            ibeg = 1
            iend = strend (label)
            if (curoverlay(curlist) .eq. 1) then
               xpos = cfux (vpl - 0.015)
            else if (curoverlay(curlist) .eq. 2) then
               xpos = cfux (vpl - 0.030)
            else
               xpos = cfux (vpl - 0.045)
            endif
            ypos = cfuy (vpb)
            orient = 90.0
            center = -1.0
               call plchhq (xpos, ypos, label(ibeg:iend), szsf*szsc, 
     &                      orient, center)
c     
c
c           Write plot limits at top right of plot.
c
            value = uplim(1)
            call scale (value, 1, domain_slope(1), domain_intercept(1), 
     &                  0.0)
            numlab = ftoa(value)
            call clean_zero(numlab(strbeg(numlab):strend(numlab)))
            label = '('//numlab(strbeg(numlab):strend(numlab))
c     
            value = uplim(2)
            call scale (value, 1, domain_slope(2), domain_intercept(2), 
     &                  0.0)
            numlab = ftoa(value)
            call clean_zero(numlab(strbeg(numlab):strend(numlab)))
            label = label(strbeg(label):strend(label))//' , '
     &           //numlab(strbeg(numlab):strend(numlab))
c     
            if (surface) then
               numlab = 'sfc'
            else
               value = uplim(3)
               call scale (value, 1, domain_slope(3), 
     &                     domain_intercept(3), 0.0)
               numlab = ftoa(value)
               call clean_zero(numlab(strbeg(numlab):strend(numlab)))
            endif
            label =label(strbeg(label):strend(label))//' , '
     &           //numlab(strbeg(numlab):strend(numlab))
c 
            value = uplim(4)
            call scale (value, 1, domain_slope(4), domain_intercept(4), 
     &                  0.0)
            numlab = ftoa(value)
            call clean_zero(numlab(strbeg(numlab):strend(numlab)))
            label =label(strbeg(label):strend(label))//' , '
     &           //numlab(strbeg(numlab):strend(numlab))//')'
c 
            ibeg = 1
            iend = strend (label)
            if (curoverlay(curlist) .eq. 1) then
               xpos = cfux (vpr + 0.015)
            else if (curoverlay(curlist) .eq. 2) then
               xpos = cfux (vpr + 0.030)
            else
               xpos = cfux (vpr + 0.045)
            endif
            ypos = cfuy (vpt)
            orient = 270.0
            center = -1.0
               call plchhq (xpos, ypos, label(ibeg:iend), szsf*szsc, 
     &                      orient, center)
         endif
      endif
c
c     Return if color fill
      if (color) then
c     
c        Call set with original values.
         call set (vpl, vpr, vpb, vpt, wdl, wdr, wdb, wdt, linlog)
c
c        Reset text and line color.
         call gstxci (text_index)
         call gsplci (line_index)
	 call gsclip(oldclp)
         return
      endif
c
c     Draw label indicating contour minimum, maximum, and interval 
c     used. Do this only if a constant field has not been detected.
c
      call cpgeti ('CFF - constant field flag', cff)
      if ( cff .eq. 0 ) then
         call cpseti ('PAI - parameter array index', 1)
         call cpgetr ('CLV - contour level values', cmin)
         call cpsetr ('ZDV - z data value', cmin)
      	 CALL CPNUMB(cmin, 4, -10000 , 4 , -1,
     +        'E',' ',' ',1,0,0,0,1,1,numlab,ibuf3,ibuf2,Ibuf1)
         call clean_zero(numlab(strbeg(numlab):strend(numlab)))
         label = 'FROM '//
     &           numlab(strbeg(numlab):strend(numlab))
         call cpgeti ('NCL - number of contour levels', nlevs)
         call cpseti ('PAI - parameter array index', nlevs)
         call cpgetr ('CLV - contour level values', cmax)
         call cpsetr ('ZDV - z data value', cmax)
      	 CALL CPNUMB(cmax, 4, -10000 , 4 , -1,
     +        'E',' ',' ',1,0,0,0,1,1,numlab,ibuf3,ibuf2,Ibuf1)
         call clean_zero(numlab(strbeg(numlab):strend(numlab)))
cy         call cpgetc ('ZDV - z data value', numlab)
         label = label(strbeg(label):strend(label))//' TO '//
     &           numlab(strbeg(numlab):strend(numlab))
         call cpgetr ('CIU - contour interval used', cint)
         call cpsetr ('ZDV - z data value', cint)
      	 CALL CPNUMB(cint, 4, -10000 , 4 , -1,
     +        'E',' ',' ',1,0,0,0,1,1,numlab,ibuf3,ibuf2,Ibuf1)
         call clean_zero(numlab(strbeg(numlab):strend(numlab)))
cy         call cpgetc ('ZDV - z data value', numlab)
         label = label(strbeg(label):strend(label))//' BY '//
     &           numlab(strbeg(numlab):strend(numlab))
         if(color .and. .not. movielabs) then
            xpos = cfux ((vpr + vpl)/2.0) 
            center = 0.0
         elseif (plot_number .eq. 2 ) then
            xpos   = cfux (vpr) 
            center = 1.0
         else
            xpos = cfux (vpl)
            center = -1.0
         endif
         if(filltyp .eq. SOLID .and. fill_frequency .ne. NEVER
     &        .and. tictest .eq. 0.0) then
            ypos = cfuy (szsf*.9)
         else
            ypos   = cfuy (szsf*2.2)
         endif
         orient = 0.0
         ibeg = strbeg (label)
         iend = strend (label)
         if(.not. movielabs) then
            call plchhq (xpos, ypos, label(ibeg:iend), (szsc * szsf), 
     &                   orient, center) 
         endif
c     
c
c        Create labels indicating contour line type and color for 
c        each plot.
c
         if(color .and. .not. movielabs) then
            sign = 0
         else if ( plot_number .eq. 2 ) then
            sign = -1
         else
            sign = 1
         endif
         shift = 0.15
         xpts(2) = xpos
         if(filltyp .eq. SOLID .and. fill_frequency .ne. NEVER
     &        .and. tictest .eq. 0.0) then
            !ypos = cfuy (szsf + .01)
            ypos = cfuy (szsf + .005)
         else
            ypos = cfuy (szsf + 0.03)
         endif
         call  gslwsc (linewdth)
         if ( curoverlay(curlist) .eq. 2 ) then
            if ( cmax .gt. 0.0 ) then
               offset = 0.025
               call getivar ('hicolor', lineclr1, error)
               call gsplci (lineclr1)
               call getivar ('hipattern', linepat1, error)
               call dashdb (linepat1)
               xpts(1) = xpos
               xpts(2) = xpos + sign*shift
               ypts(1) = ypos
               ypts(2) = ypos
               call curved (xpts, ypts, 2)
               call plotit (0, 0, 0)
            else
               offset = 0.0
            endif
c     
            if ( cmin .le. 0.0 ) then
               call getivar ('locolor', lineclr2, error)
               call gsplci (lineclr2)
               call getivar ('lopattern', linepat2, error)
               call dashdb (linepat2)
               xpos = xpts(2) + sign*offset
               xpts(1) = xpos
               xpts(2) = xpos + sign*shift
               ypts(1) = ypos
               ypts(2) = ypos
               if ( (lineclr1 .ne. lineclr2) .or. 
     &              (linepat1 .ne. linepat2) ) then
                  call curved (xpts, ypts, 2)
                  call plotit (0, 0, 0)
               endif
            endif
         else
            if ( cmin .le. 0.0 ) then
               offset = 0.025
               call getivar ('locolor', lineclr2, error)
               call gsplci (lineclr2)
               call getivar ('lopattern', linepat2, error)
               call dashdb (linepat2)
               xpts(1) = xpos
               xpts(2) = xpos + sign*shift
               ypts(1) = ypos
               ypts(2) = ypos
               call curved (xpts, ypts, 2)
               call plotit (0, 0, 0)
            else
               offset = 0.0
            endif
c     
            if ( cmax .gt. 0.0 ) then
               call getivar ('hicolor', lineclr1, error)
               call gsplci (lineclr1)
               call getivar ('hipattern', linepat1, error)
               call dashdb (linepat1)
               xpos = xpts(2) + sign*offset
               xpts(1) = xpos
               xpts(2) = xpos + sign*shift
               ypts(1) = ypos
               ypts(2) = ypos
               if ( (lineclr1 .ne. lineclr2) .or. 
     &              (linepat1 .ne. linepat2) ) then
                  call curved (xpts, ypts, 2)
                  call plotit (0, 0, 0)
               endif
            endif
         endif
      endif
      call  gslwsc (1.)
c
c
      call set (vpl, vpr, vpb, vpt, wdl, wdr, wdb, wdt, linlog)
c
c     Reset text and line color.
c
      call gstxci (text_index)
      call gsplci (line_index)
c
c
      call gsclip(oldclp)
      return
      end
c
c
