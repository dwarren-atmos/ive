      subroutine add_to_list (string)
c-----------------------------------------------------------------------
c     $Id: add_to_list.f,v 1.1.1.1 1993/04/07 19:39:46 harry Exp $
c     Purpose:
c        This routine adds a command to the segment list at the place 
c        specified by segnum. If segnum is greater than the length of
c        the list, the list is rearranged so that the oldest member of
c        the list is deleted, and all entries are moved down one place
c        in the list.
c     Arguments:
c        string  char input  the string to be added to the list.
c        segnum  int  input  the place in the list where string will
c                            be added.
c     History:
c     $Log: add_to_list.f,v $
c     Revision 1.1.1.1  1993/04/07 19:39:46  harry
c     Initial CVS checkin of IVE.
c
c Revision 2.5  1993/03/23  18:33:26  harry
c DEC does not like zero-length strings!
c
c Revision 2.4  1993/03/23  18:28:47  harry
c Corrections to add_to_list and print_list.
c
c Revision 2.3  1993/01/06  17:18:21  harry
c Change a segment from one each plot to one each overlay.
c
c Revision 2.2  1992/09/23  23:10:48  harry
c Change command seperator , to ;
c
c Revision 2.1  1992/08/11  16:10:39  harry
c Properly handle adding to empty entry in list.
c
c Revision 2.0  1992/07/01  21:12:27  millerp
c Released version rereving to 2.0
c
c Revision 1.2  1992/07/01  20:41:26  millerp
c *** empty log message ***
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
      include 'segments.icl'
c
c
c     Argument declarations.
c
      character *(*) string
      integer        nblank
c
c
c     Local variable declarations.
c
      integer        beglst, begstr, endlst, endstr, lastseg, 
     &               lenlst, lenstr
c
c
c     External function declarations.
c
      integer        strbeg, strend
c
      data lastseg/0/
c
      save lastseg

      if (curlist .ne. lastseg) then
         list(curlist) = ' '
         lastseg = curlist
      endif
c
c     Find the position in the list where the string will be written.
c     Also check to see if we need to rearrange the list.
c     Add string to the list.
c
      begstr = strbeg(string)
      endstr = strend(string)
      lenstr = endstr - begstr + 1
c
      beglst = strbeg(list(curlist))
      endlst = strend(list(curlist))
      lenlst = endlst - beglst + 1
c
c
c     Only add a new string if it will not overflow the character 
c     string list.
c
      if ( (lenlst + lenstr + 2) .lt. MXLEN ) then
         if (nblank(list(curlist)) .eq. 0) then
            list(curlist) = string(begstr:endstr)
         else if ( lenlst .gt. 0 ) then
            list(curlist) = list(curlist)(beglst:endlst)//'; '//
     &                      string(begstr:endstr)
         else
            list(curlist) = string(begstr:endstr)
         endif
      endif
c
c
      return
      end
c
c
      subroutine print_list 
c-----------------------------------------------------------------------
c     Purpose:
c        This routine prints out the segment list.
c     Arguments:
c        curseg  int  input  the current segment number.
c     History:
c-----------------------------------------------------------------------
c
      include 'segments.icl'
c
c
c     Local variable declarations.
c
      integer      i, j, segnum
c     
c
c     External function declarations.
c
      integer      nblank, strbeg, strend
c
c
c
      if ( curplot .gt. MAXLIST ) then
         segnum = curplot - MAXLIST + 1 
      else
         segnum = 1
      endif
c
c
      write (6, 100)
      write (6, 105)
c
c
      do 10 i = 1, MAXLIST
         j = mod(segnum-1,MAXLIST)+1
         if ( nblank(list(j)) .ne. 0 ) then
            write (6, 110) 
     &           segnum, list(j)(strbeg(list(j)):strend(list(j)))
         endif
         segnum = segnum + 1
 10   continue
c
c
c     Format statements.
c
 100  format (1x, ' PLOT ', 5x, 'COMMAND' / 1x, 'NUMBER' )
 105  format (1x, '======', 5x, '=======')
 110  format (3x, i3, 6x, a)
c
c
      return
      end
