c     History:
c      $Log: set_printer.f,v $
c      Revision 1.3  1997/01/31 18:21:18  warren
c      Added print_command command for lp or lpr
c
c      Revision 1.2  1996/12/23 20:58:14  warren
c      Landscape printing option
c
c Revision 1.1  1993/07/20  22:44:51  warren
c Added ability to set prints and eps files to draw color lines. Added
c ability to choose printer. Added ability to specify which spot on the
c page to print at.
c
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine set_pr_name(error)
c
      include 'incommands.icl'
      include 'printer.icl'
      include 'message.icl'
c
c
c     Argument declarations.
c
      logical  error
c
c
c     Local variable declarations.
c
      character *(CMDLEN)   curval
      integer               beg, end
c
c
c     External function declarations.
c
      integer nblank, strbeg, strend
c
c
c     Get the first value for the current command.
c
      curval = comand(fstval)(1:CMDLEN)
      beg = strbeg(curval)
      end = strend(curval)
c
c
c     If a value is not given, do not set tloc.
c     Otherwise set printer_name to the specified value.
c
      if ( nblank(curval) .eq. 0 ) then
         write (message, 100) comand(1)(strbeg(comand(1)):
     &        strend(comand(1)))
         call write_message
         error = .true.
      else
         printer_name=curval(strbeg(curval):strend(curval))
      endif
c
c     Format statements.
c
 100  format (1x,'Command ',a,' requires a value.')
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine set_pr_color(error)
c
      include 'incommands.icl'
      include 'printer.icl'
      include 'message.icl'
c
c
c     Argument declarations.
c
      logical  error
c
c
c     Local variable declarations.
c
      character *(CMDLEN)   curval
      integer               beg, end
c
c
c     External function declarations.
c
      integer nblank, strbeg, strend
c
c
c     Get the first value for the current command.
c
      curval = comand(fstval)(1:CMDLEN)
      beg = strbeg(curval)
      end = strend(curval)
c
c
c     If a value is not given, do not set printer_color.
c     Otherwise set printer_color to the specified value.
c
      if ( nblank(curval) .eq. 0 ) then
         write (message, 100) comand(1)(strbeg(comand(1)):
     &        strend(comand(1)))
         call write_message
         error = .true.
      else
         if(curval(strbeg(curval):strend(curval)) .eq. 'on' .or.
     &      curval(strbeg(curval):strend(curval)) .eq. 'ON' .or.
     &      curval(strbeg(curval):strend(curval)) .eq. 'On'  ) then
            color_printer = .true.
         elseif(curval(strbeg(curval):strend(curval)) .eq. 'off' .or.
     &      curval(strbeg(curval):strend(curval)) .eq. 'OFF' .or.
     &      curval(strbeg(curval):strend(curval)) .eq. 'Off'  ) then
            color_printer = .false.
         else
            write (message, 110) comand(1)(strbeg(comand(1)):
     &           strend(comand(1)))
            call write_message
            error = .true.
         endif
      endif
c     
c
c     Format statements.
c
 100  format (1x,'Command ',a,' requires a value.')
 110  format (1x,'On and off are the only correct values for ',a,'.')
c
c
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine set_fi_color(error)
c
      include 'incommands.icl'
      include 'printer.icl'
      include 'message.icl'
c
c
c     Argument declarations.
c
      logical  error
c
c
c     Local variable declarations.
c
      character *(CMDLEN)   curval
      integer               beg, end
c
c
c     External function declarations.
c
      integer nblank, strbeg, strend
c
c
c     Get the first value for the current command.
c
      curval = comand(fstval)(1:CMDLEN)
      beg = strbeg(curval)
      end = strend(curval)
c
c
c     If a value is not given, do not set printer_color.
c     Otherwise set printer_color to the specified value.
c
      if ( nblank(curval) .eq. 0 ) then
         write (message, 100) comand(1)(strbeg(comand(1)):
     &        strend(comand(1)))
         call write_message
         error = .true.
      else
         if(curval(strbeg(curval):strend(curval)) .eq. 'on' .or.
     &      curval(strbeg(curval):strend(curval)) .eq. 'ON' .or.
     &      curval(strbeg(curval):strend(curval)) .eq. 'On'  ) then
            color_file = .true.
         elseif(curval(strbeg(curval):strend(curval)) .eq. 'off' .or.
     &      curval(strbeg(curval):strend(curval)) .eq. 'OFF' .or.
     &      curval(strbeg(curval):strend(curval)) .eq. 'Off'  ) then
            color_file = .false.
         else
            write (message, 110) comand(1)(strbeg(comand(1)):
     &           strend(comand(1)))
            call write_message
            error = .true.
         endif
      endif
c     
c
c     Format statements.
c
 100  format (1x,'Command ',a,' requires a value.')
 110  format (1x,'On and off are the only correct values for ',a,'.')
c
c
      return
      end

         
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine set_landscape(error)
c
      include 'incommands.icl'
      include 'printer.icl'
      include 'message.icl'
c
c
c     Argument declarations.
c
      logical  error
c
c
c     Local variable declarations.
c
      character *(CMDLEN)   curval
      integer               beg, end
c
c
c     External function declarations.
c
      integer nblank, strbeg, strend
c
c
c     Get the first value for the current command.
c
      curval = comand(fstval)(1:CMDLEN)
      beg = strbeg(curval)
      end = strend(curval)
c
c
c     If a value is not given, do not set printer_color.
c     Otherwise set printer_color to the specified value.
c
      if ( nblank(curval) .eq. 0 ) then
         write (message, 100) comand(1)(strbeg(comand(1)):
     &        strend(comand(1)))
         call write_message
         error = .true.
      else
         if(curval(strbeg(curval):strend(curval)) .eq. 'on' .or.
     &      curval(strbeg(curval):strend(curval)) .eq. 'ON' .or.
     &      curval(strbeg(curval):strend(curval)) .eq. 'On'  ) then
            landscape = 1
         elseif(curval(strbeg(curval):strend(curval)) .eq. 'off' .or.
     &      curval(strbeg(curval):strend(curval)) .eq. 'OFF' .or.
     &      curval(strbeg(curval):strend(curval)) .eq. 'Off'  ) then
            landscape = 0
         else
            write (message, 110) comand(1)(strbeg(comand(1)):
     &           strend(comand(1)))
            call write_message
            error = .true.
         endif
      endif
c     
c
c     Format statements.
c
 100  format (1x,'Command ',a,' requires a value.')
 110  format (1x,'On and off are the only correct values for ',a,'.')
c
c
      return
      end

         
      subroutine set_pr_com(error)
c
      include 'incommands.icl'
      include 'printer.icl'
      include 'message.icl'
c
c
c     Argument declarations.
c
      logical  error
c
c
c     Local variable declarations.
c
      character *(CMDLEN)   curval
      integer               beg, end
c
c
c     External function declarations.
c
      integer nblank, strbeg, strend
c
c
c     Get the first value for the current command.
c
      curval = comand(fstval)(1:CMDLEN)
      beg = strbeg(curval)
      end = strend(curval)
c
c
c     If a value is not given, do not set tloc.
c     Otherwise set printer_name to the specified value.
c
      if ( nblank(curval) .eq. 0 ) then
         write (message, 100) comand(1)(strbeg(comand(1)):
     &        strend(comand(1)))
         call write_message
         error = .true.
      else
         print_command=curval(strbeg(curval):strend(curval))
      endif
c
c     Format statements.
c
 100  format (1x,'Command ',a,' requires a value.')
      return
      end
