c     History:
c      $Log: set_multiple_read.f,v $
c      Revision 1.3  1997/02/07 20:16:59  harry
c      Yet more MULTIPLE_READ changes.
c
c      Revision 1.2  1997/02/07 18:39:10  harry
c      Remove PARTIAL_READ and expand MULTIPLE_READ.
c
c      Revision 1.1  1997/01/17 20:38:18  harry
c      First changes for MULTIPLE_READ and PARTIAL_READ.
c
c     Purpose:
c        This subroutine sets the multiple read flag
c
c     Syntax of command:
c
c     MULTIPLE_READ=coordinate
c                   -coordinate
c                   coordinate,start,end
c
c     coordinate = x, y, z, or t
c
c     form 1 - set file coordinate
c          2 - set file coordinate, use from largest to smallest
c          3 - set file coordinate with start and end values
c
c
c     Arguments:
c        error  logi  output  indicates error encountered in this
c                             routine.
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine set_multiple_read(error)
c
      include 'data.icl'
      include 'incommands.icl'
      include 'message.icl'
      include 'plotops.icl'
c
c
c     Argument declarations.
c
      logical  error
c
c
c     Local variable declarations.
c
      character *(CMDLEN)   coorval, curval
      integer               beg, end, beg1, cbeg, cend
c
c
c     External function declarations.
c
      integer nblank, strbeg, strend
      integer numval, i, mr, mr_start, mr_end, mr_flag
C
C     Check number of parameters
C
      numval = 0
      do i = FSTVAL, MAXCOM
         if (nblank(comand(i)) .ne. 0) numval = numval + 1
      enddo
c
c     Get the first value for the current command.
c
      coorval = comand(FSTVAL)(1:CMDLEN)
      beg = strbeg(coorval)
      end = strend(coorval)
      if (numval .eq. 1) then
         if (curval(beg:beg).eq.'-') then
            mr_flag = -1
            beg1 = beg+1
         else
            mr_flag = 1
            beg1 = beg
         endif
      else if (numval .eq. 3) then
         mr_flag = 0
         beg1 = beg
         curval = comand(FSTVAL+1)(1:CMDLEN)
         cbeg = strbeg(curval)
         cend = strend(curval)
         call cvatoi(curval(cbeg:cend), mr_start, error)
         if (.not. error) then
            curval = comand(FSTVAL+2)(1:CMDLEN)
            cbeg = strbeg(curval)
            cend = strend(curval)
            call cvatoi(curval(cbeg:cend), mr_end, error)
         endif
         if (error) then
            write(message, 110) curval(cbeg:cend),
     &           comand(1)(strbeg(comand(1)):strend(comand(1)))
            call write_message
            return
         endif
      else
         write (message, 100) comand(1)(strbeg(comand(1)):
     &        strend(comand(1)))
         call write_message
         error = .true.
         return
      endif
c
c
c     If a value is not given, do not set tloc.
c     Otherwise set printer_name to the specified value.
c
      error = .false.
      if (coorval(beg1:beg1) .eq. 'N' .or.
     &        coorval(beg1:beg1) .eq. 'n') then
         mr = -1
      else if (coorval(beg1:beg1) .eq. 'X' .or.
     &        coorval(beg1:beg1) .eq. 'x') then
         mr = 0
      else if (coorval(beg1:beg1) .eq. 'Y' .or.
     &        coorval(beg1:beg1) .eq. 'y') then
         mr = 1
      else if (coorval(beg1:beg1) .eq. 'Z' .or.
     &        coorval(beg1:beg1) .eq. 'z') then
         mr = 2
      else if (coorval(beg1:beg1) .eq. 'T' .or.
     &        coorval(beg1:beg1) .eq. 't') then
         mr = 3
      else
         write(message, 110) coorval(beg:end),
     &        comand(1)(strbeg(comand(1)):strend(comand(1)))
         call write_message
         return
      endif
      if (mr .ne. multiple_read .or. mr_start .ne. multiple_read_start
     &     .or. mr_end .ne. multiple_read_end
     &     .or. mr_flag .ne. multiple_read_flag) then
         old_file = ' '
         multiple_read = mr
         multiple_read_start = mr_start
         multiple_read_end = mr_end
         multiple_read_flag = mr_flag
      endif
c     
c     Format statements.
c
 100  format (1x,'Command ',a,' requires 1 or 3 values.')
 110  format (1x,x,' is an incorrect value for command ',a,'.')
      return
      end
