      subroutine setsurf (error)
c-----------------------------------------------------------------------
c     $Id: setsurf.f,v 1.1 2001/01/23 22:47:15 davidive Exp $
c     Purpose:
c        This subroutine sets the value of sint, which indicates the
c        desired surface to show in 3D
c     Arguments:
c        error  logi  output  indicates error encountered in this
c                             routine.
c     History:
c      $Log: setsurf.f,v $
c      Revision 1.1  2001/01/23 22:47:15  davidive
c      cpline.f: fixed to use zero_color
c      everything else: First cut at wireframes and isosurfaces. No dimentions yet
c      no widgets yet.
c
c-----------------------------------------------------------------------
c
      include 'incommands.icl'
      include 'attributes.icl'
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
      character *(cmdlen) curval
      real                value
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
      curval = comand(fstval)(1:cmdlen)
c
c
c     Make sure a value is given.
c
      if ( nblank(curval) .eq. 0 ) then
         write (message, 100) comand(1)(strbeg(comand(1)):
     &                                  strend(comand(1)))
         call write_message
         error = .true.
      elseif(curval(1:1) .eq. 'd')then
         value = 0d0
      else
c
c        Convert the first value to a real number.
c
         call cvatof (curval(strbeg(curval):strend(curval)), value,
     &                error)
         if ( error ) then
c
c           The value for this command is incorrect.
c
            write (message, 110) curval(strbeg(curval):strend(curval)),
     &                           comand(1)(strbeg(comand(1)):
     &                                     strend(comand(1)))
         call write_message
         endif
      endif
c      
c
      if ( .not. error ) then
c
c        Set the value of sint.
c
         sint = value
c
      endif
c
c
c     Format statements.
c
 100  format (1x,'Command ',a,' requires a value.')
 110  format (1x,a,' is an incorrect value for command ',a,'.')
c
c
      return
      end

