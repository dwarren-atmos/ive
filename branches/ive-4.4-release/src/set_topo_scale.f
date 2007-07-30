      subroutine set_topo_scale (error)
c-----------------------------------------------------------------------
c
c
      include 'incommands.icl'
      include 'message.icl'
c
c
c     Argument declaration.
c
      logical             error
c
c     Local variable declarations.
c
      character *(CMDLEN) curval
      integer             ibeg, iend
      real                value
c
c
c     External function declarations.
c
      integer             nblank, strbeg, strend
c
c
c
c
      error = .false.
c
c     Get the first value for the current command.
c
      curval = comand(FSTVAL)(1:CMDLEN)
c
      if ( nblank(curval) .eq. 0 ) then
         value = 1.0
      else
         ibeg = strbeg (comand(FSTVAL))
         iend = strend (comand(FSTVAL))
         call cvatof (comand(FSTVAL)(ibeg:iend), value, error)
         if ( value .lt. 0.0 ) then
            error = .true.
         endif
         if ( error ) then
            write (message, 200) comand(FSTVAL)(ibeg:iend),
     &                    comand(1)(strbeg(comand(1)):strend(comand(1)))
            call write_message 
         endif
      endif
c
c
      if ( .not. error ) then
         call setrvar ('topo_scale', value, error)
      endif
c
c
c     Format statements.
c
 100  format (1x,'Command ',a,' requires a value.')
 200  format (1x,a,' is an incorrect value for command ',a,'.')
c
c
      return
      end


