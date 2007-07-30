      subroutine inscmd (comlin)
c-----------------------------------------------------------------------
c     $Id: inscmd.f,v 1.1.1.1 1993/04/07 19:40:04 harry Exp $
c     Purpose:
c        This routine inserts the current command line into the 
c        segment command list. 
c     Arguments:
c        comlin  char  input  the current command line.
c     History:
c        $Log: inscmd.f,v $
c        Revision 1.1.1.1  1993/04/07 19:40:04  harry
c        Initial CVS checkin of IVE.
c
c Revision 2.1  1992/09/23  23:11:09  harry
c Change command seperator , to ;
c
c Revision 2.0  1992/07/01  21:13:26  millerp
c Released version rereving to 2.0
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
      character *(*) comlin
c
c
c     Local variable declarations.
c
      integer  endcmd, begcom, endcom, lencom
c
c
c     External function declarations.
c
      integer nblank, strbeg, strend
c
c
c
c
      if ( nblank(cmdstr) .eq. 0 ) then
         cmdstr = comlin(strbeg(comlin):strend(comlin))
      else
c
c        Make sure that we do not try to write more than MXLEN 
c        characters into cmdstr.
c
         endcmd = strend(cmdstr)
         begcom = strbeg(comlin)
         endcom = strend(comlin) 
         lencom = endcom - begcom + 1
         if ( (endcmd + lencom) .le. (MXLEN - 2) ) then
             cmdstr = cmdstr(strbeg(cmdstr):endcmd)//'; '//
     &                comlin(begcom:endcom)
         endif
      endif
c
c
      return
      end
