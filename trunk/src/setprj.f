      subroutine setprj (error)
c-----------------------------------------------------------------------
c     $Id: setprj.f,v 1.6 1994/04/19 23:03:30 yuri Exp $
c     Purpose:
c        This subroutine sets the value of proj, which is used by the
c        NCAR EZMAP routine maproj to set the map projection. The value
c        of proj wiss be set to a two-character string.
c     Arguments:
c        error  logi  output  indicates error encountered in this
c                             routine.
c     History:
c      $Log: setprj.f,v $
c      Revision 1.6  1994/04/19 23:03:30  yuri
c      Big 1D flip commit.
c
c Revision 1.5  1994/02/16  17:00:51  yuri
c 	Just cleaning and getting rid of "interp_factors" stuff.
c
c Revision 1.4  1993/12/16  23:00:18  harry
c Set maplim correctly when turning off default map.
c
c Revision 1.3  1993/12/15  18:15:51  harry
c Add support for DEFAULT_MAP command, default_map transform.  Modify map
c clipping to work properly with the map background widget.
c
c Revision 1.2  1993/08/05  17:59:33  warren
c Christoph's map stuff
c
c Revision 1.1.1.1  1993/04/07  19:40:31  harry
c Initial CVS checkin of IVE.
c
c Revision 2.0  1992/07/01  21:14:47  millerp
c Released version rereving to 2.0
c
c Revision 1.2  1992/05/05  15:31:07  millerp
c Added projection choices.
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
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
      character *(CMDLEN) curval
c
c     External function declarations.
c
      integer nblank, strbeg, strend
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
         call chkprj (curval(strbeg(curval):strend(curval)), error)
         if ( error ) then
c           The value for this command is incorrect.
            write (message, 110) curval(strbeg(curval):strend(curval)),
     &                           comand(1)(strbeg(comand(1)):
     &                                     strend(comand(1)))
            call write_message
         else
c           Set the value of proj.
            proj = curval(strbeg(curval):strend(curval))
            if (defmap) then
               defmap = .false.
               call ui_update(DEFAULT_MAP_UI)
               if (exact_fit) then
                  maplim = 'CL'
                  call ui_update(MAPCLIP_UI)
               endif
            endif
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
      subroutine chkprj (instr, error)
c-----------------------------------------------------------------------
c     Purpose:
c        This subroutine is used to make sure that the value of 
c        which is used by the NCAR EZMAP routine maproj to set the map 
c        projection, is valid.
c     Arguments:
c        instr  char  input   the character string to be tested against 
c                             the list of valid projections.
c        error  logi  output  indicates possible errors found in this
c                              routine.
c                              error = true   error detected.
c                              error = false  no error detected.
c     History:
c-----------------------------------------------------------------------
c
      include 'supma.icl'
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
      do 10 i = 1, MAXPRJ
         if ( instr(ibeg:iend) .eq. valprj(i) ) then
c
c
c           instr was found in the list of valid projections. Set 
c           error to false and return.
c
            error = .false.
            return
         endif
 10   continue
c
c
c     instr was not found in the list of valid projections, so log an
c     error and return.
c
      error = .true.
c
c
      return
      end
