      subroutine setmvct (error)
c-----------------------------------------------------------------------
c     $Id: setmvct.f,v 1.1.1.1 1993/04/07 19:40:53 harry Exp $
c     Purpose:
c        This subroutine sets the value of mapvct, which is used by velvct1
c        to set the usage of ph2ll in mapfac.
c     Arguments:
c        error  logi  output  indicates error encountered in this
c                             routine.
c     History:
c      $Log: setmvct.f,v $
c      Revision 1.1.1.1  1993/04/07 19:40:53  harry
c      Initial CVS checkin of IVE.
c
cRevision 1.2  1993/03/26  09:15:47  davidb
cinitial
c
c
c
c-----------------------------------------------------------------------
c
      include 'incommands.icl'
      include 'message.icl'
      include 'vector.icl'
c
c     Argument declarations.
c
      logical  error
c
c
c     Local variable declarations.
c
      character *(CMDLEN) curval
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
      curval = comand(fstval)(1:CMDLEN)
c
c     Make sure a value is given.
c
      if ( nblank(curval) .eq. 0 ) then
         write (message, 100) comand(1)(strbeg(comand(1)):
     &                                  strend(comand(1)))
         call write_message
         error = .true.
      else
c        Check to see that the value given is valid.
         call chkvct (curval(strbeg(curval):strend(curval)), error)
         if ( error ) then
c           The value for this command is incorrect.
            write (message, 110) curval(strbeg(curval):strend(curval)),
     &                           comand(1)(strbeg(comand(1)):
     &                                     strend(comand(1)))
            call write_message
         else
c           Set the value of proj.
            mapvct = curval(strbeg(curval):strend(curval))
         endif
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
c
c
      subroutine chkvct (instr, error)
c-----------------------------------------------------------------------
c     Purpose:
c        This subroutine is used to make sure that the value of instr
c        is a valid command for MAPVECTORS
c     Arguments:
c        instr  char  input   the character string to be tested against 
c                             the list of valid chars.
c        error  logi  output  indicates possible errors found in this
c                              routine.
c                              error = true   error detected.
c                              error = false  no error detected.
c     History:
c-----------------------------------------------------------------------
c
      include 'vector.icl'
c
c     Argument declarations.
      character *(*) instr
      logical error
c
c     Local variable declarations
      integer      i, ibeg, iend
c
c     External function declarations.
      integer strbeg, strend
c
c
c
      ibeg = strbeg (instr)
      iend = strend (instr)
      call upcase (instr(ibeg:iend), (iend - ibeg) + 1)
c
      do 10 i = 1, MAXVCT
         if ( instr(ibeg:iend) .eq. valvct(i) ) then
c
c
c           instr was found in the list of valid commands. Set 
c           error to false and return.
c
            error = .false.
            return
         endif
 10   continue
c
c
c     instr was not found in the list of valid commands, so log an
c     error and return.
c
      error = .true.
c
c
      return
      end
