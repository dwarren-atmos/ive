      subroutine select_old (field)
c-----------------------------------------------------------------------
c     $Id: select_old.f,v 1.3 2001/01/31 00:19:13 davidive Exp $
c     Purpose:
c        This routine selects the current values of the input arguments
c        when data sets are changed, provided they make sense for the
c        new file, If not, the user is forced to make new selections 
c        before making a new plot.
c     Arguments:
c        field  char  in/out  the current field name.
c     History:
c        $Log: select_old.f,v $
c        Revision 1.3  2001/01/31 00:19:13  davidive
c        Read Color now looks for a rgb value bracketed by 888s to set as the zero
c        coloe.
c        Select old checks for movement from lower to higher dimensions and moves
c        the window in that dimension from 0,0 to as big as it goes.
c
c        Revision 1.2  1993/06/22 17:50:13  harry
c        Change call to getvid to strip out blanks in field.
c
c Revision 1.1.1.1  1993/04/07  19:40:18  harry
c Initial CVS checkin of IVE.
c
c Revision 2.0  1992/07/06  21:45:31  millerp
c *** empty log message ***
c
c-----------------------------------------------------------------------
c
c
c     Argument declarations.
c
      character *(*) field
c
c
c     Local variable declarations.
c
      integer        varid, error
      logical        winchange
      real           plwmin(4) ,plwmax(4), plmin(4), plmax(4)
      character*256   buff
c
c
c     External function declarations.
c
      integer        getvid, nblank, strbeg, strend
c
c
c
c
      if (nblank(field) .ne. 0) then
         varid = getvid(field(strbeg(field):strend(field)))
         if (varid .ge. 0) then
c
c           The current field exists in the new data file, so select
c           it.
c
            call driver ('field = '//field(strbeg(field):strend(field)),
     &                    0)
         else
c
c           The current field does not exist in the new data file, so 
c           force the user to select a new field before plotting.
c
            field(1:len(field)) = ' '
         endif
      endif
c now make sure that the window makes sense for # of dimensions.
      winchange = .false.
      call getrarr('plwmin',plwmin,4,error)
      call getrarr('plwmax',plwmax,4,error)
      call getrarr('plmin_scaled',plmin,4,error)
      call getrarr('plmax_scaled',plmax,4,error)
      buff='window='
      if(plwmin(1) .eq. 0 .and. plwmax(1) .eq. 0 .and.
     &     plmin(1) .ne. plmax(1)) then
         write(buff,'(A,F,A,F,A)')buff(strbeg(buff):strend(buff)),
     &        plmin(1),',',plmax(1),','
         winchange=.true.
      else
         buff=buff(strbeg(buff):strend(buff))//',,'
      endif
      if(plwmin(2) .eq. 0 .and. plwmax(2) .eq. 0 .and.
     &     plmin(2) .ne. plmax(2)) then
         write(buff,'(A,F,A,F,A)')buff(strbeg(buff):strend(buff)),
     &        plmin(2),',',plmax(2),','
         winchange=.true.
      else
         buff=buff(strbeg(buff):strend(buff))//',,'
      endif
      if(plwmin(3) .eq. 0 .and. plwmax(3) .eq. 0 .and.
     &     plmin(3) .ne. plmax(3)) then
         write(buff,'(A,F,A,F,A)')buff(strbeg(buff):strend(buff)),
     &        plmin(3),',',plmax(3),','
         winchange=.true.
      else
         buff=buff(strbeg(buff):strend(buff))//',,'
      endif
      if(plwmin(4) .eq. 0 .and. plwmax(4) .eq. 0 .and.
     &     plmin(4) .ne. plmax(4)) then
         write(buff,'(A,F,A,F)')buff(strbeg(buff):strend(buff)),
     &        plmin(4),',',plmax(4)
         winchange=.true.
      else
         buff=buff(strbeg(buff):strend(buff))//','
      endif
      if(winchange) call driver(buff(strbeg(buff):strend(buff)),0)
c
c
      return
      end
