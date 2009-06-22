      subroutine vect2d (data1, data2, nu, nv, mapflg, savflg, 
     &                   error)
c-----------------------------------------------------------------------
c     $Id: vect2d.f,v 1.8 2002-12-27 18:44:29 warren Exp $
c     Purpose:
c        This routine makes the set call and calls the vector drawing
c          routine for 2D vector plots.
c     Arguments:
c        data1   real  input   array containing the first vector 
c                              component.
c        data2   real  input   array containing the second vector 
c                              component.
c        nu      int   input   number of points in the
c                              first subscript direction of data.
c        nv      int   input   number of points in the
c                              second subscript direction of data.
c        mapflg  logi  input   indicates whether or not the plot is 
c                              projected onto a map background.
c        savflg  logi  input   indicates whether or not the current plot
c                              is saved (used to overlay plots). 
c        error   logi  output  indicates possible errors found in this
c                              routine.
c                              error = false  no errors detected
c                              error = true   error detected
c     History:
c      $Log: vect2d.f,v $
c      Revision 1.8  2002-12-27 18:44:29  warren
c      First set of changes from zurich. use mapdrw_dl to deal with ever changing
c      borders in Europe. Increase arrays in work.
c
c      Revision 1.7  1997/03/07 15:40:00  warren
c      Fixed multiple declarations to make Alpha happy.
c
c      Revision 1.6  1997/03/06 19:00:02  harry
c      Make changes to properly handle exact_fit case for mouse slicing.
c
c      Revision 1.5  1996/11/15 21:15:56  harry
c      Vector plots should be labelled by the data units, not display data units.
c
c      Revision 1.4  1996/02/23 22:31:43  harry
c      Fix for reset_spval feature for new slicer.
c
c Revision 1.3  1995/08/24  22:19:07  harry
c Pointers is as pointers does.
c
c Revision 1.2  1994/01/29  00:11:45  warren
c first cut at movie labels. Line plots still need work
c
c Revision 1.1.1.1  1993/04/07  19:40:42  harry
c Initial CVS checkin of IVE.
c
c Revision 2.4  1992/12/02  23:59:32  millerp
c Fix argument list for lab2ds; fix dimension for lolim, uplim.
c
c Revision 2.3  1992/11/24  16:33:44  millerp
c Make changes related to removing hslice, vsice.
c
c Revision 2.2  1992/09/17  15:42:49  warren
c changed the slicing syntax to use xloc, yloc, zloc and time instead of
c hloc and vloc. this include changes to the common blocks and command set.
c
c Revision 2.1  1992/09/08  19:53:31  millerp
c Added stuff for new slicers.
c
c Revision 2.0  1992/07/01  21:15:21  millerp
c Released version rereving to 2.0
c
c Revision 1.2  1992/06/10  20:18:16  millerp
c Fix argument list in call lab2ds.
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
      include 'data.icl'
      include 'plotops.icl'
      include 'vector.icl'
      include 'supma.icl'
      include 'pltwin.icl'
c
c
c     Argument declarations.
c
      integer     nu, nv
      real        data1, data2
      real        xmajr, xminr, ymajr, yminr
      logical     drstrmln, error, mapflg, savflg
      dimension data1(nu, nv), data2(nu, nv)
c
c
c     Local variable declarations.
c
      real             ORVAL
      parameter ( ORVAL = 1.0e12 )
      character *(240) string
      integer         imap
c     Integer	       xaxis, yaxis (in pltwun)
      logical         movielabs
      real            lolim, uplim
      dimension       lolim(MAXDIM), uplim(MAXDIM)
      integer         axes
      real            vbot, vtop, vleft, vrght, 
     &                wbot, wtop, wleft, wrght 
c
c
c     External function declarations.
c
      integer         strbeg, strend
c
c
c
c
      error = .false.
      call getlvar('movielabs',movielabs,error)
      if ( error ) then
         return
      endif
c
c
c     Set line width.
c
      call gslwsc (linwdth)
c
c
c     Use conpack out-of-range feature to avoid unwanted vectors.
c     Note that this particular value is used because the EZMAP routines
c     use it by default.
c
      call cpsetr ('ORV - out-of-range value', ORVAL)
c
c
c     Make appropriate set call.
c
      if(defmap .and. savflg) then
c     need to call set as if no overlay
         call set2d (nu, mapflg, .false., lolim, uplim, error)
      else
         call set2d (nu, mapflg, savflg, lolim, uplim, error)
      endif
      if (error) then
         return
      endif
c
c
c     Draw the vectors.
c
      call cpgeti ('MAP - mapping flag', imap) 
      call getlvar ('drstrmln', drstrmln, error)
c
      if ( drstrmln ) then
c         call dr2dstrmln (data1, data2, nu, nv, imap, VECT_SPVAL)
         call physuv_2_lonlatuv_trans(data1,data2,nu,nv,imap)
         call dr2dstrmln (data1, data2, nu, nv, imap, VECT_SPVAL)
c         call dr2dstrmln (data1, data2, nu, nv, imap, VECT_ORVAL)
      else
c     Rotational transform from atmos.umnw.ethz.ch
         call physuv_2_lonlatuv_trans(data1,data2,nu,nv,imap)
         call dr2dv (data1, data2, nu, nv, imap)
      endif
c
c     If the vector routines encountered any errors, then return.
c
      if ( error ) then
         return
      endif
c
c
c     Draw either a map foreground or perimeter, whichever is 
c     appropriate. Done in mak2dv
c     
      if ( .not. savflg ) then
         if ( mapflg ) then
            call getset ( vleft, vrght, vbot, vtop, 
     &           wleft, wrght, wbot, wtop, axes)
c            call mapdrw_dl
            if (defmap) then
               call set ( vleft, vrght, vbot, vtop,
     &           wleft, wrght, wbot, wtop, axes)
c     &              x1, x2, y1, y2, axes)
            endif
c         endif
         else
c         if ( .not. mapflg .or. (defmap)) then
            xmajr = xmajor
            xminr = xminor
            ymajr = ymajor
            yminr = yminor
            call getivar ('xaxis', xaxis, error)
            call getivar ('yaxis', yaxis, error)
            call scaletics (xmajr, xminr, ymajr, yminr,
     &           mod(xaxis,10), mod(yaxis,10))
            if (movielabs) then
               call pcseti ('QU',0)
               call pcsetc ('FN', 'HELVETICA-BOLD    ')
            else
               call pcseti ('QU',1)
               call pcseti ('CL',1)
            endif
            call uwperim (xmajr, xminr, ymajr, yminr)
         endif
      endif
c
c
c     Label plot.
c
      if ( label ) then
c
c        Use labeling routine for solid fill to label vector plots.
c
         string(1:len(string)) = 
     &        vect_component(1)(strbeg(vect_component(1)):
     &                          strend(vect_component(1)))//
     &        '/'//
     &        vect_component(2)(strbeg(vect_component(2)):
     &                          strend(vect_component(2)))
         call lab2d (datfil, string, lolim, uplim, 
     &                savflg, mapflg, surface, .true., .true.)
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
