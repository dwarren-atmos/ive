#ifdef SIXTY_FOUR_bptr
#define ive_ptr integer*8
#else
#define ive_ptr integer
#endif
      subroutine mak2dv
c-----------------------------------------------------------------------
c     $Id: mak2dv.F,v 1.8 2007-07-30 21:59:09 reinecke Exp $
c     Purpose:
c        This subroutine is the top-level driver which produces any 2D
c        vector plot.
c     Arguments:
c        None
c     History:
c      $Log: mak2dv.F,v $
c      Revision 1.8  2007-07-30 21:59:09  reinecke
c      Fixed clipping bug instituted by clipping fix.
c
c      Revision 1.7  2007-07-05 19:24:47  warren
c      Final commit of everything pre svn
c
c      Revision 1.6  2002/02/04 22:54:48  warren
c      Fortran Arrays start at 1 not 0
c
c      Revision 1.5  1997/02/12 17:36:17  warren
c      Changed pointer type to ive_ptr to avoid interaction with F90 pointer type.
c
c      Revision 1.4  1995/09/05 20:34:00  harry
c      Add new slicer.  Good luck!
c
c Revision 1.3  1995/01/09  18:52:32  warren
c Moved the line color rotation selection from everywhere to makseg.
c Note: makseg now has one argument. it should be called with true if
c you only use hicolor and false if you use two colors in the type of plot.
c
c Revision 1.2  1994/01/20  20:44:39  yuri
c Propagating of "overlay_patterns" stuff to newer version.
c
c Revision 1.1  1993/11/29  19:39:31  warren
c Changes to 3.1 to make it work on alphas.
c FORTRAN: new data type ive_ptr ifdefed to either 32 bit or 64 bit.
c C: Change int to long where necessary.
c Files: Quite a few .f files are now .F
c
c Revision 1.5  1993/07/09  19:09:32  warren
c Pretty labels to go with the 2d ticmarks. Also I removed superfluous calls
c to chkwin and moved all the others to driver.f.
c
c Revision 1.4  1993/06/01  19:53:57  yuri
c Second try.
c
c Revision 1.2  1993/05/24  00:01:48  yuri
c It's not my improvements mostly. Yuri (?!)
c
c Revision 1.1.1.1  1993/04/07  19:40:12  harry
c Initial CVS checkin of IVE.
c
c Revision 2.8  1993/02/17  22:03:34  warren
c added code for memory debugging and cleaned up freeing bugs
c
c Revision 2.7  1993/02/10  18:28:14  warren
c changed freemem to freemem_c for things allocated by slicers.
c
c Revision 2.6  1993/01/06  17:19:17  harry
c Change a segment from one each plot to one each overlay.
c
c Revision 2.5  1992/12/03  00:00:16  millerp
c Fix topo plotting for surface plots.
c
c Revision 2.4  1992/11/11  04:00:08  millerp
c Corrected checks for plotting terrain.
c
c Revision 2.3  1992/10/23  17:23:25  millerp
c Re-added save flag check before plotting terrain.
c
c Revision 2.2  1992/09/17  15:42:49  warren
c changed the slicing syntax to use xloc, yloc, zloc and time instead of
c hloc and vloc. this include changes to the common blocks and command set.
c
c Revision 2.1  1992/09/08  19:53:31  millerp
c Added stuff for new slicers.
c
c Revision 2.0  1992/07/01  21:13:45  millerp
c Released version rereving to 2.0
c
c Revision 1.3  1992/06/19  17:49:32  millerp
c Added time dimension to vector calculations.
c
c Revision 1.2  1992/05/05  15:29:34  millerp
c Got rid of unused variables.
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
      include 'segments.icl'
       include 'message.icl'
c
c
c     Local variable declarations.
c
      integer  ni, nj,  buttons, fix, nter
      ive_ptr slab1, slab2, topoad, topopt
      logical  error, mapflg, savflg, surface,check_vect_slabs
      real pt1, pt2, pt3
      dimension  fix(4)
      dimension pt1(4), pt2(4), pt3(4)
c
c
c     External function declarations.
c
      integer  strbeg, strend
c
c
c
c
      error = .false.
c
c      call chkwin
c

      call chk2d (error)
      if ( error ) then
         go to 900
      endif
c
c
c     Get vector components at points in the slice plane.
c
      topoad=0
      topopt=0
      call get2dv (slab1, slab2, topoad, topopt, ni, nj, nter, error)
      if(check_vect_slabs(slab1, slab2, ni,nj,error)) then
         write (message, 90)
 90      format(' One of the vector components is all missing values')
         call write_message
       endif
c
c
c     Plot the data.
c
      if ( .not. error ) then
c
c
         call getlvar ('savflg', savflg, error)
         if ( error ) go to 900
c
         call makseg(.false.)
c
c
c
c        Draw a map if required.
c
         call getivar ('use_buttons', buttons, error)
         if ( error ) go to 900
         if(buttons .ne. 0) then
            call getiarr ('lock', fix, 4, error)
            if ( error ) go to 900
         else
            call getrarr ('point_1', pt1, 4, error)
            if ( error ) go to 900
            call getrarr ('point_2', pt2, 4, error)
            if ( error ) go to 900
            call getrarr ('point_3', pt3, 4, error)
            if ( error ) go to 900
            if(pt1(3) .eq. pt2(3) .and. pt2(3) .eq. pt3(3)) fix(3) = 1
            if(pt1(4) .eq. pt2(4) .and. pt2(4) .eq. pt3(4)) fix(4) = 1
         endif
c     
         if(fix(3) .ne. 0 .and. fix(4) .ne. 0 ) then
            call getlvar ('mapflg', mapflg, error)
            if ( error ) go to 900
         else
            mapflg = .false.
         endif
         if ( mapflg ) then
            call drwmap (error)
         endif
c
         if ( error ) then
            go to 900
         endif
c
c        Draw the vectors.
c
         call vect2d (%val(slab1), %val(slab2), ni, nj, 
     &                mapflg, savflg, error)
c
c
         if ((.not. savflg) .and. (topoad .ne. 0)) then
c
            call getlvar ('surface', surface, error)
c     
c           Plot terrain.
c     
            if (fix(3) .ne. 0 .and. fix(4) .ne. 0) then
               call cptopo (%val(topoad), ni, ni, nj, error)
            elseif (surface .and. (fix(4) .ne. 0)) then
               call cptopo (%val(topoad),  ni, ni, nj, error)
            else
               if(buttons .ne. 0) then
                  if(fix(4) .ne. 0 .and. 
     &                 (fix(1) .ne. 0 .or. fix(2) .ne. 0)) then
                     if (topopt .ne. 0) then
                        call vtopo (%val(topoad), %val(topopt), nter)
                     endif
                  endif
               elseif(fix(4) .ne. 0 .and. 
     &                 (pt1(1)*(pt2(2)-pt3(2)) + 
     &                 pt2(1)*(pt3(2)-pt1(2)) + 
     &                 pt3(1)*(pt1(2)-pt2(2))) .eq. 0) then
                  if (topopt .ne. 0) then
                     call vtopo (%val(topoad), %val(topopt), nter)
                  endif
               endif
            endif
         endif
c    
c     
c     
c        Add command line to the command list if no errors. 
c
         call gclsg()
         if ( .not. error ) then
            call add_to_list (cmdstr(strbeg(cmdstr):strend(cmdstr)))
         else
            call delseg (curseg)
         endif
         cmdstr(1:len(cmdstr)) = ' '
      endif
c
c
c
 900  continue
       if(error)then
          write (message, 100)
  100     format(' There was an error in mak2dV')
          call write_message
       endif
c
c
c     Free all allocated memory.
c
      if ( slab1  .gt. 0 ) call freemem(slab1)
      if ( slab2  .gt. 0 ) call freemem(slab2) 
      if ( topopt .gt. 0 ) call freemem (topopt)
      if ( topoad .gt. 0 ) call freemem (topoad)
c
c
      return
      end


      logical function check_vect_slabs(slab1, slab2, ni,nj,error)
      integer ni,nj,i,j
      logical error, b1, b2
      real slab1(ni,nj), slab2(ni,nj),spval, epspval
      double precision epsilon

      call getrvar('spval',spval,error)
      b1=.true.
      b2=.true.
      check_vect_slabs=.false.
      epspval = epsilon(spval)
      do j=1,nj
         do i=1,ni
            if(abs(slab1(i,j) - spval) .le. epspval) then
               b1=.false.
               goto 1000
            endif
         enddo
      enddo
 1000 continue
      do j=1,nj
         do i=1,ni
            if(abs(slab2(i,j) - spval) .le. epspval) then
               b2=.false.
               goto 1010
            endif
         enddo
      enddo
 1010 continue
      
      if(b1 .or. b2) then
         check_vect_slabs = .true.
         error = .true.
      endif
      return
      end

      
