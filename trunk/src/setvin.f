      subroutine setvin (error)
c-----------------------------------------------------------------------
c     $Id: setvin.f,v 1.4 1997/02/21 20:02:10 harry Exp $
c     Purpose:
c        This routine sets the grid intervals at which vectors will
c        be drawn.
c     Arguments:
c        error  logi  output  indicates error encountered in this
c                             routine.
c     History:
c        $Log: setvin.f,v $
c        Revision 1.4  1997/02/21 20:02:10  harry
c        Fix vector interval to be floating point and have values less than one.
c        Handling of vector interval is now in the slicer.
c
c Revision 1.3  1993/05/27  16:38:57  warren
c Recover files removed by yuri's commit
c
c Revision 1.1.1.1  1993/04/07  19:40:35  harry
c Initial CVS checkin of IVE.
c
c Revision 2.0  1992/07/01  21:15:03  millerp
c Released version rereving to 2.0
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
      include 'incommands.icl'
      include 'message.icl'
      include 'vector.icl'
c
c
c     Argument declarations.
c
      logical  error
c
c
c     Local variable declarations.
c
      character *(CMDLEN) curval
      integer             i, ibeg, iend
      real                val, value
      dimension val(2)
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
      curval = comand(FSTVAL)(1:CMDLEN)
c
c
c     Make sure at least one value is given.
c
      if ( nblank(curval) .eq. 0 ) then
c
c        Log an error.
c
         write (message, 100) comand(1)(strbeg(comand(1)):
     &                                  strend(comand(1)))
         call write_message
         error = .true.
      else
c
         do 10 i = 1, 2
            val(i) = 1
c
c           Get the first two values.
c
            curval = comand(FSTVAL+(i-1))(1:CMDLEN)
            if ( nblank(curval) .ne. 0 ) then
               ibeg = strbeg (curval)
               iend = strend (curval)
               call cvatof (curval(ibeg:iend), value, error)
               if ( .not. error ) then
                  val(i) = value
               else
                  go to 20
               endif
            endif
 10      continue
      endif
c
c
 20   continue
c
c
      if ( error ) then
c
c        There was an error in the values given, so log an error.
c
         write (message, 110) curval(ibeg:iend), 
     &                        comand(1)(strbeg(comand(1)):
     &                                  strend(comand(1)))
         call write_message
      else
c
c        Set the grid intervals.
c
         if ( val(1) .gt. 0 ) incx = val(1)
         if ( val(2) .gt. 0 ) incy = val(2)
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

