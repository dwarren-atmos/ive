C      $Id: mapdrw_dl.f,v 1.1 2002/12/27 18:46:13 warren Exp $
C      $Log: mapdrw_dl.f,v $
C      Revision 1.1  2002/12/27 18:46:13  warren
C      mapdrw_dl is mapdrw fixed to use the newer ncar routines that deal with
C      the changing borders in Europe.
C
CCC
C     Add mapdrw_dl here - fixed mapdrw to use the new european borders
C     Contributed by the folks at atmos.umnw.ethz.ch
CCC
      subroutine mapdrw_dl
c     ====================
      integer   ilvl,initial
      character*2  out
      
      call mapgti('IN',initial)
      if  (initial.ne.0) call mapint
      call mapgrd
      call maplbl
      call mapgtc('OU',out)
      ilvl = 1
      if (out.eq.'CO') then
         ilvl = 2
      elseif (out.eq.'US') then
         ilvl = 4
      elseif (out.eq.'PS') then
         ilvl = 4
      elseif (out.eq.'PO') then
         ilvl = 3
      endif
      call mplndr('Earth..2',ilvl)
c     
      return
      end
      
