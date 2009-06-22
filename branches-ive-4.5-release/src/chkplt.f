      subroutine chkplt (file, transform_path, field, plotyp, plwmin, 
     &                   plwmax, MAXDIM, error)
c-----------------------------------------------------------------------
c     $Id: chkplt.f,v 1.6 1996/10/11 01:12:44 warren Exp $
c     Purpose:
c        This subroutine checks to make sure that all necessary
c        parameters are set before drawing a plot. At a minimum,
c        a data set, a field , a plot window, and a plot type must be 
c        specified.
c     Arguments:
c        file    char  input   the current data file. This is blank if
c                              no data file has been specified.
c        field   char  input   the current field. This is blank if
c                              no field has been specified.
c        plwmin  real  input   the array containing the minimum values 
c                              of the plot window in physical space. If
c                              no window has been set, all values are
c                              zero.
c        plwmax  real  input   the array containing the maximum values 
c                              of the plot window in physical space. If
c                              no window has been set, all values are
c                              zero.
c        MAXDIM  int   input   The dimension of plwmin and plwmax.
c        error   logi  output  flag indicating if errors were 
c                              encountered
c                              in this routine.
c                              false  no errors detected.
c                              true   error detected.
c     History:
c      $Log: chkplt.f,v $
c      Revision 1.6  1996/10/11 01:12:44  warren
c      To fill or not to fill is no longer the only question. Now there is a seperate
c      question of solid or pattern. Also first part of mouse slicing. 2d -> 1d
c      Labels are not fixed yet.
c
c      Revision 1.5  1996/04/25 16:25:03  harry
c      Add Skew-T plots and the beginning of trajectory plots.
c
c Revision 1.4  1993/06/01  19:49:40  yuri
c Second attempt.
c
c Revision 1.2  1993/05/23  23:38:18  yuri
c Case "nblank(transform_path) .eq. 0" ommitted.
c
c Revision 1.1.1.1  1993/04/07  19:39:47  harry
c Initial CVS checkin of IVE.
c
c Revision 2.1  1992/09/17  15:42:49  warren
c changed the slicing syntax to use xloc, yloc, zloc and time instead of
c hloc and vloc. this include changes to the common blocks and command set.
c
c Revision 2.0  1992/07/01  21:12:31  millerp
c Released version rereving to 2.0
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
      include 'message.icl'
c
c
c     Argument declarations.
c
      character *(*) file, transform_path, field, plotyp
      integer        MAXDIM
      logical        error
      real           plwmin, plwmax
      dimension      plwmin(MAXDIM), plwmax(MAXDIM)
c
c
c     External function declarations.
c
      integer nblank
c
c
c
c
      error = .false.
c
c
c     Has a data set been specified?
c
      if ( nblank(file) .eq. 0 ) then
         error = .true.
         write (message, 100)
         call write_message
      endif
c
c     Has a field been specified?
c
      if ( nblank(field) .eq. 0 .and. plotyp(1:3) .ne.'VEC'.and.
     &     plotyp(1:3) .ne.'SKE' .and. plotyp(1:3) .ne.'TRA') then
         error = .true.
         write (message, 120)
         call write_message
      end if
c
c
c     Has a plot type been specified?
c
      if ( nblank(plotyp) .eq. 0 ) then
         error = .true.
         write (message, 130)
         call write_message
      endif
c
c
c     Has a plot window been specified?
c
      if ( (plwmin(1) .eq. 0.0) .and. (plwmax(1) .eq. 0.0) .and.
     &     (plwmin(2) .eq. 0.0) .and. (plwmax(2) .eq. 0.0) .and.
     &     (plwmin(3) .eq. 0.0) .and. (plwmax(3) .eq. 0.0)      ) then
         error = .true.
         write (message, 140)
         call write_message
      endif
c 
c
c     Format statements.
c
 100  format (1x,'A data file must be specified before a plot can ',
     &           'be made.')
 110  format (1x,'A transform file must be specified before a plot can',
     &           ' be made.')
 120  format (1x,'A field must be specified before a plot can ',
     &           'be made.')
 130  format (1x,'A plot type must be specified before a plot can ',
     &           'be made.')
 140  format (1x,'A plot window must be specified before a plot can ',
     &           'be made.')
c
c
      return
      end

