      subroutine setordinate (error)
c-----------------------------------------------------------------------
c     $Id: setordinate.f,v 1.15 1997/06/12 15:28:18 davidive Exp $
c     Purpose:
c        This routine sets the upper and lower limits of the vertical
c        plot axis for sounding plots.
c     Arguments:
c        error  logi  output  indicates possible errors encountered in 
c                             this routine.
c                             error = false   no errors detected.
c                             error = true    error detected.
c     History:
c     $Log: setordinate.f,v $
c     Revision 1.15  1997/06/12 15:28:18  davidive
c     Fix multidimentional 1 D plots to pay attention to min, max and tic mark
c     settings, and remember them.
c
c Revision 1.14  1994/09/14  18:20:06  harry
c Big 1D flip uncommit.
c
c Revision 1.13  1994/09/07  22:07:14  warren
c removed the extra plot from the color table locking sequence.
c
c Revision 1.12  1994/06/28  17:43:39  warren
c First cut at fancy tic marks.
c
c Revision 1.11  1994/06/11  21:33:47  yuri
c Added more comments.
c
c Revision 1.10  1994/04/19  23:03:24  yuri
c Big 1D flip commit.
c
c Revision 1.9  1994/03/31  00:22:29  warren
c Fixed it so we now actually use the and store the tic mark and limit values
c in the filed's property sheet.
c
c Revision 1.8  1994/03/30  22:17:59  harry
c Allow the user to set the minor tick marks as well as the major tick
c marks.
c
c Revision 1.7  1993/10/18  23:07:33  warren
c updates from 3.0 version.
c
c Revision 1.6  1993/10/13  00:38:07  warren
c fixed missed extra variable.
c
c Revision 1.5  1993/10/13  00:04:26  warren
c Update current version for fixes to 3.0 version related to the meaning of
c default in the property sheet boxes.
c
c
c Revision 1.4  1993/06/14  21:56:58  warren
c Fixed to recognize the value default
c
c Revision 1.3  1993/05/27  16:38:56  warren
c Recover files removed by yuri's commit
c
c Revision 1.1.1.1  1993/04/07  19:40:29  harry
c Initial CVS checkin of IVE.
c
c Revision 2.1  1992/12/10  21:14:24  millerp
c Delete references to unused variables.
c
c Revision 2.0  1992/07/01  21:14:40  millerp
c Released version rereving to 2.0
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
c
      include 'incommands.icl'
      include 'lineplt.icl'
      include 'message.icl'
      include 'ive_widgets.icl'
      include 'plotops.icl'
      include 'attributes.icl'
c
c     Argument declarations.
c
      logical     error
c
c     Local variable declarations.
c
      integer     i, ibeg, iend
      real        FILLVAL, value
      dimension value(2)
      parameter (FILLVAL = -999.999)
c
c     External function declarations.
c
      integer     nblank, strbeg, strend
c
c
c
      error = .false.
      value(1) = FILLVAL
      value(2) = FILLVAL
c
      do 10 i = 1, 2
         if ( nblank (comand(FSTVAL+i-1)) .ne. 0 ) then
            ibeg = strbeg (comand(FSTVAL+i-1))
            iend = strend (comand(FSTVAL+i-1))
            call cvatof (comand(FSTVAL+i-1)(ibeg:iend), value(i), 
     &           error)
            if ( error ) then
               if((comand(FSTVAL+i-1)(ibeg:ibeg) .eq. 'D') .or. 
     &              (comand(FSTVAL+i-1)(ibeg:ibeg) .eq. 'd')) then
                  error = .false.
                  value(i)=bigone
               else
                  write (message, 200) comand(FSTVAL+i-1)(ibeg:iend),
     &                 comand(1)(strbeg(comand(1)):
     &                 strend(comand(1)))
                  call write_message 
                  return
               endif
            endif
         endif
 10   continue
c
c     Set the ordinate min and/or max, as appropriate. Also set flags
c     indicating whether or not the user has specified these values.
c
      if ( .not. error ) then
         if ( value(1) .ne. FILLVAL ) then
            ordinate_min = value(1)
         endif
         if ( value(2) .ne. FILLVAL ) then
            ordinate_max = value(2)
         endif
      endif
c
      if (lock(3) .ne. 0 .or. use_buttons .eq. 0) then
         lmin = ordinate_min
         lmax = ordinate_max
      endif
c      
c     Format statements.
c
 100  format (1x,'Command ',a,' requires a value.')
 200  format (1x,a,' is an incorrect value for command ',a,'.')
c
c
      return
      end


