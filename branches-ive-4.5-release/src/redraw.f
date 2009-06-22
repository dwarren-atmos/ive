      subroutine redraw
c-----------------------------------------------------------------------
c     $Id: redraw.f,v 1.3 2005/11/03 19:51:57 warren Exp $
c     Purpose:
c        This routine redraws a list of segments.
c     Arguments:
c        None
c     History:
c        $Log: redraw.f,v $
c        Revision 1.3  2005/11/03 19:51:57  warren
c        Fixed redraw command to really redraw plots
c
c        Revision 1.2  1994/02/16 21:03:09  yuri
c        Now redraw command acts this way: if "redraw=2" is typed in, it draws one plot;
c        if "redraw=2,4,5" is typed in, IVE draws first plot, waits until user hits
c        "return", draws second plot, waits, draws third one.
c
c Revision 1.1.1.1  1993/04/07  19:40:15  harry
c Initial CVS checkin of IVE.
c
c Revision 2.0  1992/07/01  21:13:52  millerp
c Released version rereving to 2.0
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
c
      include 'incommands.icl'
      include 'message.icl'
      include 'segments.icl'
c
c
c     Local valiable declarations.
c
      character *(CMDLEN) curval
      integer             i, j,inkey, istate, MXVAL, plotnum, 
     &                    numplot, value, WS_ID
      integer             GSGOP, GALWAY, GPERFO
      logical             error
      parameter (MXVAL  = 8 , 
     &           GSGOP  = 4 ,
     &           GALWAY = 1 ,
     &           GPERFO = 1 ,      
     &           WS_ID  = 1 )
      dimension plotnum(MXVAL)
c
c
c     External function declarations.
c
      integer nblank, strbeg, strend
c
c
c
c
      error = .false.
c
c     Get the first value for the current command.
c
      curval = comand(fstval)(1:CMDLEN)
c
c
c     Make sure a value is given.
c
      if ( nblank(curval) .eq. 0 ) then
c
c        Log an error and return.
c
         write (message, 100) comand(1)(strbeg(comand(1)):
     &                                  strend(comand(1)))
         call write_message
         error = .true.
c
      else
c
c        Convert all values to integer.
c
         do 10 i = 1, MXVAL
            curval = comand(FSTVAL + (i - 1))(1:CMDLEN) 
            if ( nblank(curval) .eq. 0 ) then
               go to 20
            endif
            call cvatoi (curval(strbeg(curval):strend(curval)), value, 
     &                   error)
            if ( error ) then
               write (message, 110) curval(strbeg(curval):
     &                                     strend(curval)), 
     &                   comand(1)(strbeg(comand(1)):strend(comand(1)))
               call write_message
               go to 20
            else
               numplot = i
               plotnum(i) = value
            endif
 10      continue
c
      endif
 20   continue
c
      if ( .not. error ) then
c
c        Redraw the requested segments.
c
c        Close any open segments.
c
         call gqops (istate)
         if ( istate .eq. GSGOP ) call gclsg()
c
c        Redraw requested segments.
c
         call gclrwk (WS_ID, GALWAY)
         do 30 i = 1, numplot
            DO j=1,curoverlay(plotnum(i))
               call gasgwk (WS_ID, segs(j,plotnum(i)))
               call guwk (1, GPERFO)
            enddo
            if ( i .eq. MXVAL)  go to 40
c 
c           Pause between each redrawn plot.
c
            if(numplot .gt. 1 .and. i .ne. numplot) then 
               read (5, 25) inkey
               call gclrwk (WS_ID, GALWAY)
            endif
c
 30      continue
c
 40      continue
      endif
c
c
c     Format statements.
c
 25   format (a)         
 100  format (1x,'Command ',a,' requires a value.')
 110  format (1x,a,' is an incorrect value for command ',a,'.')
c
c
      return
      end
