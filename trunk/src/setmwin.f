      subroutine map_win (error)
c-----------------------------------------------------------------------
c     $Id: setmwin.f,v 1.1 1997/03/14 22:26:34 warren Exp $
c     Purpose:
c        This subroutine sets u1,u2 v1,v2 for windowing in map space
c     Arguments:
c        error  logi  output  indicates error encountered in this
c                             routine.
c     History:
c      $Log: setmwin.f,v $
c      Revision 1.1  1997/03/14 22:26:34  warren
c      Added new command map_window to allow for mouse windowing on non
c      equadistant or default maps.
c
c
c-----------------------------------------------------------------------
c
      include 'incommands.icl'
      include 'message.icl'
      include 'supma.icl'
      include 'ive_widgets.icl'
c
c     Argument declarations.
c
      logical  error
c
c     Local variable declarations.
c
      integer       i, numval
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
      numval = 0
      do 5 i = fstval, 5
         if ( nblank(comand(i)) .ne. 0 )  numval = numval + 1
 5    continue
      if(numval .lt. 4 ) then
         error = 1
         write (message,*)
     &        'map_window requires 8 arguments (u1 v1 u2 v2)'
         call write_message
         return
      endif
      
      
c
c
c     Make sure a value is given.
c
      call cvatof ( comand(fstval)(strbeg(comand(fstval)):
     &     strend(comand(fstval))),
     &     mapwin_u1, error) 
      if ( error ) then
         write (message, 110) comand(fstval)(strbeg(comand(fstval)):
     &        strend(comand(fstval)))
     &        ,comand(1)(strbeg(comand(1)):strend(comand(1)))
         call write_message
         mapwin_u1=0.0
         mapwin_u2=0.0
         mapwin_v1=0.0
         mapwin_v2=0.0
         return
      endif

      call cvatof ( comand(fstval+1)(strbeg(comand(fstval+1)):
     &     strend(comand(fstval+1))),
     &     mapwin_v1, error) 
      if ( error ) then
         write (message, 110) comand(fstval+1)(strbeg(comand(fstval+1)):
     &        strend(comand(fstval+1)))
     &        ,comand(1)(strbeg(comand(1)):strend(comand(1)))
         mapwin_u1=0.0
         mapwin_u2=0.0
         mapwin_v1=0.0
         mapwin_v2=0.0
         call write_message
         return
      endif
      
      call cvatof ( comand(fstval+2)(strbeg(comand(fstval+2)):
     &     strend(comand(fstval+2))),
     &     mapwin_u2, error) 
      if ( error ) then
         write (message, 110) comand(fstval+2)(strbeg(comand(fstval+2)):
     &        strend(comand(fstval+2)))
     &        ,comand(1)(strbeg(comand(1)):strend(comand(1)))
         mapwin_u1=0.0
         mapwin_u2=0.0
         mapwin_v1=0.0
         mapwin_v2=0.0
         call write_message
         return
      endif
      
      call cvatof ( comand(fstval+3)(strbeg(comand(fstval+3)):
     &     strend(comand(fstval+3))),
     &     mapwin_v2, error) 
      if ( error ) then
         write (message, 110) comand(fstval+3)(strbeg(comand(fstval+3)):
     &        strend(comand(fstval+3)))
     &        ,comand(1)(strbeg(comand(1)):strend(comand(1)))
         mapwin_u1=0.0
         mapwin_u2=0.0
         mapwin_v1=0.0
         mapwin_v2=0.0
         call write_message
         return
      endif
      
c
c     Format statements.
c
 100  format (1x,'Command ',a,' requires a value.')
 110  format (1x,a,' is an incorrect value for command ',a,'.')
c
c
      return
      end
