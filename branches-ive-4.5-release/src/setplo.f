      subroutine setplo (error)
c-----------------------------------------------------------------------
c     $Id: setplo.f,v 1.6 1994/04/19 23:03:28 yuri Exp $
c     Purpose:
c        This subroutine sets the value of plon, which is used by the
c        NCAR EZMAP routine maproj.
c     Arguments:
c        error  logi  output  indicates error encountered in this
c                             routine.
c     History:
c      $Log: setplo.f,v $
c      Revision 1.6  1994/04/19 23:03:28  yuri
c      Big 1D flip commit.
c
c Revision 1.5  1994/02/16  17:00:49  yuri
c 	Just cleaning and getting rid of "interp_factors" stuff.
c
c Revision 1.4  1993/12/16  23:00:16  harry
c Set maplim correctly when turning off default map.
c
c Revision 1.3  1993/12/15  18:15:49  harry
c Add support for DEFAULT_MAP command, default_map transform.  Modify map
c clipping to work properly with the map background widget.
c
c Revision 1.2  1993/08/05  17:59:32  warren
c Christoph's map stuff
c
c Revision 2.0  1992/07/01  21:14:44  millerp
c Released version rereving to 2.0
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
      real                value
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
      curval = comand(fstval)(1:CMDLEN)
c
c
c     Make sure a value is given.
c
      if ( nblank(curval) .eq. 0 ) then
c
c        Log an error and return.
c
         write (message, 100) comand(1)(strbeg(comand(1)):
     &                                  strend(comand(1)))
         call write_message
         error = .true.
      else
c
c        Convert the first value to a real number.
c
         call cvatof (curval(strbeg(curval):strend(curval)), value, 
     &                error)
c
         if ( error ) then
c
c           Log an error message and do not set the value of plon.
c
            write (message, 110) curval(strbeg(curval):strend(curval)), 
     &                    comand(1)(strbeg(comand(1)):strend(comand(1)))
            call write_message
         else
c
c           Set the value of plon.
c
            plon = value
            if (defmap) then
               defmap = .false.
               call ui_update(DEFAULT_MAP_UI)
               if (exact_fit) then
                  maplim = 'CL'
                  call ui_update(MAPCLIP_UI)
               endif
            endif
c
         endif
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
