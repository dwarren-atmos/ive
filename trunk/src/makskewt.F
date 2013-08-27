      subroutine makskewt(p, t, td, nz,special)

c
c     $Id: makskewt.F,v 1.4 1999/03/15 20:55:15 warren Exp $
c
c     This routine makes the SKEWT plot by calling skewt.
c
c     $Log: makskewt.F,v $
c     Revision 1.4  1999/03/15 20:55:15  warren
c     Got rid of debud write(6,*) stuff
c
c     Revision 1.3  1999/03/12 00:18:01  warren
c     *** empty log message ***
c
c     Revision 1.2  1997/02/12 17:36:21  warren
c     Changed pointer type to ive_ptr to avoid interaction with F90 pointer type.
c
c     Revision 1.1  1996/04/25 16:25:23  harry
c     Add Skew-T plots and the beginning of trajectory plots.
c
c
                                                                       
#ifdef SIXTY_FOUR_bptr
#define ive_ptr integer*8
#else
#define ive_ptr integer
#endif            
      include 'message.icl'
      include 'data.icl'
      include 'segments.icl'
      include 'plotops.icl'

      integer zmax
      parameter (zmax = 2000)
      integer nz ,k, badval,count, drbgd, ax
      real t(nz), p(nz), td(nz), pres(zmax), dewpt(zmax),tout(zmax)
      real special
      real low(3), hi(3), time(2)

      logical error, savflg,avg
      
c     external functions
      integer strbeg, strend
      
c      write(6,*) 'nz=',nz, 't=',t, 'pres=',pres
      count = 0
      do 10 k=1,nz
         if (special.eq.0.or.p(k).ne.special) then
            count = count+1
            pres(count) = p(k)
            dewpt(count) = td(k)
            tout(count) = t(k)
            write(6,*) 'count=',count, 'pres(count)=',pres(count)
            if (pres(count).le.100.) goto 20
         end if
 10   continue
    
 20   continue  
      
      call makseg(.true.)

      call getlvar ('savflg', savflg, error)
      if (.not.savflg) then
         drbgd = 0
      else
         drbgd = 1
      end if

      call skewt(pres,tout,dewpt,count,special,drbgd)

c     label the plot
      low(1) = xloc
      low(2) = yloc
      low(3) = plwmin(3)
      hi(1) = xloc
      hi(2) = yloc
      hi(3) = plwmax(3)
      time(1) = tloc
      time(2) = tloc
      avg=.false.
      ax = 1
      call labskewt(low, hi, time, savflg, average, ax)
c     release all output
      call plotit (0, 0, 0)
      call gclsg()
      call add_to_list (cmdstr(strbeg(cmdstr):strend(cmdstr)))

      return
      end
      










