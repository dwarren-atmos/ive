      subroutine setmpl (error)
c-----------------------------------------------------------------------
c     $Id: setmpl.f,v 1.3 1996/05/30 17:37:09 harry Exp $
c     Purpose:
c        This routine sets the EZMAP parameter jlts, which specifies
c        how the map limits are chosen, and the map limits plm1 to
c        plm4
c     Arguments:
c        error  logi  output  indicates error encountered in this
c                             routine.
c     History:
c        $Log: setmpl.f,v $
c        Revision 1.3  1996/05/30 17:37:09  harry
c        Correct MAPLIM to properly handle the "MA" option.
c
c Revision 1.2  1993/12/15  18:15:45  harry
c Add support for DEFAULT_MAP command, default_map transform.  Modify map
c clipping to work properly with the map background widget.
c
c Revision 1.1.1.1  1993/04/07  19:40:28  harry
c Initial CVS checkin of IVE.
c
c Revision 2.1  1993/03/12  15:53:46  davidb
c added map features
c
c Revision 2.0  1992/07/01  21:14:33  millerp
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
c
c     Argument declarations.
c
      logical  error
c
c
c     Local variable declarations.
c
      character *(CMDLEN) curval
      character*8         nmaplim
      integer             i, ibeg, iend
      real                val1, val2, val3, val4
      dimension           val1(2), val2(2), val3(2), val4(2)
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
c     Make sure a value is given.
c
      if ( nblank(curval) .eq. 0 ) then
c
c        Log an error.
c
         write (message,100) comand(1)(strbeg(comand(1)):
     &                                 strend(comand(1)))
         call write_message
         error = .true.
      else
c
c        Make sure that the value is a valid one. (Note that we only 
c        have to match the first two characters.) 
c
         ibeg = strbeg (curval)
         iend = strend (curval)
         call upcase (curval(ibeg:iend), (iend - ibeg) + 1)
c
         do 10 i = 1, MAXLIM
            if ( curval(ibeg:ibeg+1) .eq. 
     &           vallim(i)(strbeg(vallim(i)):strbeg(vallim(i))+1) ) then
c
c              The given value was found in the list of valid map limit
c              specifiers. Set maplim and exit the loop.
c
               nmaplim = curval(ibeg:ibeg+1)
               go to 20
            endif
 10      continue
         error = .true.
c
 20      continue
      endif
c
      if ( error ) then
c        The value was not found in the list of valid map limit
c        specifiers, so log an error and return.
c
         write (message,110) curval(ibeg:iend), 
     &                 comand(1)(strbeg(comand(1)):strend(comand(1)))
         call write_message
         return
c
      endif
c
c
c     Now read the next four values to find the limit values. Note that
c     whether or not we need these values, and how they are written, 
c     depends on the value of maplim.
c
      do 25 i = 1, 2
         val1(i) = plm1(i)
         val2(i) = plm2(i)
         val3(i) = plm3(i)
         val4(i) = plm4(i)
 25   continue
c     
      if     ( nmaplim(1:2) .ne. 'MA' ) then
         if ( nmaplim(1:2) .eq. 'CO') then
c
c     corners are in lon, lat - supmap wants lat, lon
c
            call corner (val2, val1, val4, val3, error)
         elseif (nmaplim(1:2) .eq. 'AN') then  
            call corner (val1, val2, val3, val4, error)
         elseif ( nmaplim(1:2) .eq. 'PO' ) then
            call points (val1, val2, val3, val4, error)         
	 elseif ( nmaplim(1:2) .eq. 'LI' ) then
            call maplimits (val1, val2, val3, val4, error)
	 elseif ( nmaplim(1:2) .eq. 'CL' ) then
            call mapclip (val1, val2, val3, val4, error)
         endif
      endif
c     
      if ( .not. error ) then
c     
c           Set the values of plm1 - plm4.
c     
         do 30 i = 1, 2
            plm1(i) = val1(i)
            plm2(i) = val2(i)
            plm3(i) = val3(i)
            plm4(i) = val4(i)
 30      continue
         maplim = nmaplim
         if (defmap) then
            defmap = .false.
            call ui_update(DEFAULT_MAP_UI)
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
c
c
      subroutine points (val1, val2, val3, val4, error)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine sets the values of plm1, plm2, plm3, plm4 if the
c        value of jlts is PO.  Note that the command has longitude
c        first, but NCAR expects latitude first, thus the reversal
c        of the array values.
c     Arguments:
c        val1    real  output  the values that plm1 will be set to 
c                              provided that they are specified 
c                              correctly.
c        val2    real  output  the values that plm2 will be set to 
c                              provided that they are specified 
c                              correctly.
c        val3    real  output  the values that plm3 will be set to 
c                              provided that they are specified 
c                              correctly.
c        val4    real  output  the values that plm4 will be set to 
c                              provided that they are specified 
c                              correctly.
c        error   logi  output  indicates possible errors found in this
c                              routine.
c                              error = false   no errors detected.
c                              error = true    error detected.
c-----------------------------------------------------------------------
      include 'incommands.icl'
      include 'message.icl'
c
c     Argument declarations.
c
      logical         error
      real            val1, val2, val3, val4
      dimension val1(2), val2(2), val3(2), val4(2)
c
c
c     Local variable declarations.
c
      character *(80)  curval
      real             value
c
c
c     External function declarations.
c
      integer nblank, strbeg, strend
c
c
c
c
c
      curval = comand(FSTVAL+1)(1:CMDLEN)
      if ( nblank(curval) .ne. 0 ) then
         call cvatof (curval(strbeg(curval):strend(curval)), 
     &                value, error)
         if ( error ) then
            write (message, 110) curval(strbeg(curval):strend(curval)), 
     &                    comand(1)(strbeg(comand(1)):strend(comand(1)))
            call write_message
            error = .true.
            return
         else
            val1(2) = value
         endif
      endif
c
      curval = comand(FSTVAL+2)(1:CMDLEN)
      if ( nblank(curval) .ne. 0 ) then
         call cvatof (curval(strbeg(curval):strend(curval)), 
     &                value, error)
         if ( error ) then
            write (message, 110) curval(strbeg(curval):strend(curval)), 
     &                    comand(1)(strbeg(comand(1)):strend(comand(1)))
            call write_message
            error = .true.
            return
         else
            val1(1) = value
         endif
      endif
c
c
      curval = comand(FSTVAL+3)(1:CMDLEN)
      if ( nblank(curval) .ne. 0 ) then
         call cvatof (curval(strbeg(curval):strend(curval)), 
     &                value, error)
         if ( error ) then
            write (message, 110) curval(strbeg(curval):strend(curval)), 
     &                    comand(1)(strbeg(comand(1)):strend(comand(1)))
            call write_message
            error = .true.
            return
         else
            val2(2) = value
         endif
      endif
c
      curval = comand(FSTVAL+4)(1:CMDLEN)
      if ( nblank(curval) .ne. 0 ) then
         call cvatof (curval(strbeg(curval):strend(curval)), 
     &                value, error)
         if ( error ) then
            write (message, 110) curval(strbeg(curval):strend(curval)), 
     &                    comand(1)(strbeg(comand(1)):strend(comand(1)))
            call write_message
            error = .true.
            return
         else
            val2(1) = value
         endif
      endif
c
c
      curval = comand(FSTVAL+5)(1:CMDLEN)
      if ( nblank(curval) .ne. 0 ) then
         call cvatof (curval(strbeg(curval):strend(curval)), 
     &                value, error)
         if ( error ) then
            write (message, 110) curval(strbeg(curval):strend(curval)), 
     &                    comand(1)(strbeg(comand(1)):strend(comand(1)))
           call write_message
           error = .true.
            return
         else
            val3(2) = value
         endif
      endif
c
      curval = comand(FSTVAL+6)(1:CMDLEN)
      if ( nblank(curval) .ne. 0 ) then
         call cvatof (curval(strbeg(curval):strend(curval)), 
     &                value, error)
         if ( error ) then
            write (message, 110) curval(strbeg(curval):strend(curval)), 
     &                    comand(1)(strbeg(comand(1)):strend(comand(1)))
            call write_message
            error = .true.
            return
         else
            val3(1) = value
         endif
      endif
c
c
      curval = comand(FSTVAL+7)(1:CMDLEN)
      if ( nblank(curval) .ne. 0 ) then
         call cvatof (curval(strbeg(curval):strend(curval)), 
     &                value, error)
         if ( error ) then
            write (message, 110) curval(strbeg(curval):strend(curval)), 
     &                    comand(1)(strbeg(comand(1)):strend(comand(1)))
            call write_message
            error = .true.
            return
         else
            val4(2) = value
         endif
      endif
c
      curval = comand(FSTVAL+8)(1:CMDLEN)
      if ( nblank(curval) .ne. 0 ) then
         call cvatof (curval(strbeg(curval):strend(curval)), 
     &                value, error)
         if ( error ) then
            write (message, 110) curval(strbeg(curval):strend(curval)), 
     &                    comand(1)(strbeg(comand(1)):strend(comand(1)))
            call write_message
            error = .true.
            return
         else
            val4(1) = value
         endif
      endif
c
c
c     Format statements.
c
 110  format (1x,a,' is an incorrect value for command ',a,'.')
c
c
      return
      end
c
c
      subroutine corner (val1, val2, val3, val4, error)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine sets the values of plm1, plm2, plm3, plm4 if the
c        value of jlts is CO or AN.
c     Arguments:
c        val1    real  output  the values that plm1 will be set to 
c                              provided that they are specified 
c                              correctly.
c        val2    real  output  the values that plm2 will be set to 
c                              provided that they are specified 
c                              correctly.
c        val3    real  output  the values that plm3 will be set to 
c                              provided that they are specified 
c                              correctly.
c        val4    real  output  the values that plm4 will be set to 
c                              provided that they are specified 
c                              correctly.
c        error   logi  output  indicates possible errors found in this
c                              routine.
c                              error = false   no errors detected.
c                              error = true    error detected.
c-----------------------------------------------------------------------
      include 'incommands.icl'
      include 'message.icl'
c
c     Argument declarations.
c
      logical         error
      real            val1, val2, val3, val4
      dimension val1(2), val2(2), val3(2), val4(2)
c
c
c     Local variable declarations.
c
      character *(80)  curval
      real             value
c
c
c     External function declarations.
c
      integer nblank, strbeg, strend
c
c
c
c
c
      curval = comand(FSTVAL+1)(1:CMDLEN)
      if ( nblank(curval) .ne. 0 ) then
         call cvatof (curval(strbeg(curval):strend(curval)), 
     &                value, error)
         if ( error ) then
            write (message, 110) curval(strbeg(curval):strend(curval)), 
     &                    comand(1)(strbeg(comand(1)):strend(comand(1)))
            call write_message
            error = .true.
            return
         else
            val1(1) = value
         endif
      endif
c
      curval = comand(FSTVAL+2)(1:CMDLEN)
      if ( nblank(curval) .ne. 0 ) then
         call cvatof (curval(strbeg(curval):strend(curval)), 
     &                value, error)
         if ( error ) then
            write (message, 110) curval(strbeg(curval):strend(curval)), 
     &                    comand(1)(strbeg(comand(1)):strend(comand(1)))
            call write_message
            error = .true.
            return
         else
            val2(1) = value
         endif
      endif
c
      curval = comand(FSTVAL+3)(1:CMDLEN)
      if ( nblank(curval) .ne. 0 ) then
         call cvatof (curval(strbeg(curval):strend(curval)), 
     &                value, error)
         if ( error ) then
            write (message, 110) curval(strbeg(curval):strend(curval)), 
     &                    comand(1)(strbeg(comand(1)):strend(comand(1)))
            call write_message
            error = .true.
            return
         else
            val3(1) = value
         endif
      endif
c
      curval = comand(FSTVAL+4)(1:CMDLEN)
      if ( nblank(curval) .ne. 0 ) then
         call cvatof (curval(strbeg(curval):strend(curval)), 
     &                value, error)
         if ( error ) then
            write (message, 110) curval(strbeg(curval):strend(curval)), 
     &                    comand(1)(strbeg(comand(1)):strend(comand(1)))
            call write_message
            error = .true.
            return
         else
            val4(1) = value
         endif
      endif
c
c
c     Format statements.
c
 110  format (1x,a,' is an incorrect value for command ',a,'.')
c
c
      return
      end

c
      subroutine maplimits (val1, val2, val3, val4, error)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine sets the values of plm1, plm2, plm3, plm4 if the
c        value of jlts LI
c     Arguments:
c        val1    real  output  the values that plm1 will be set to 
c                              provided that they are specified 
c                              correctly.
c        val2    real  output  the values that plm2 will be set to 
c                              provided that they are specified 
c                              correctly.
c        val3    real  output  the values that plm3 will be set to 
c                              provided that they are specified 
c                              correctly.
c        val4    real  output  the values that plm4 will be set to 
c                              provided that they are specified 
c                              correctly.
c        error   logi  output  indicates possible errors found in this
c                              routine.
c                              error = false   no errors detected.
c                              error = true    error detected.
c
c	 ADDED by David Bresch 930305, ETHZ
c the order is set to umax,umin,vmax,vmin because this order is
c changed after the call of this subroutine to umin,umax...
c-----------------------------------------------------------------------
      include 'incommands.icl'
      include 'message.icl'
c
c     Argument declarations.
c
      logical         error
      real            val1, val2, val3, val4
      dimension val1(2), val2(2), val3(2), val4(2)
c
c
c     Local variable declarations.
c
      character *(80)  curval
      real             value
c
c
c     External function declarations.
c
      integer nblank, strbeg, strend
c
c
c
c
c
      curval = comand(FSTVAL+1)(1:CMDLEN)
      if ( nblank(curval) .ne. 0 ) then
         call cvatof (curval(strbeg(curval):strend(curval)), 
     &                value, error)
         if ( error ) then
            write (message, 110) curval(strbeg(curval):strend(curval)), 
     &                    comand(1)(strbeg(comand(1)):strend(comand(1)))
            call write_message
            error = .true.
            return
         else
            val2(1) = value
         endif
      endif
c
      curval = comand(FSTVAL+2)(1:CMDLEN)
      if ( nblank(curval) .ne. 0 ) then
         call cvatof (curval(strbeg(curval):strend(curval)), 
     &                value, error)
         if ( error ) then
            write (message, 110) curval(strbeg(curval):strend(curval)), 
     &                    comand(1)(strbeg(comand(1)):strend(comand(1)))
            call write_message
            error = .true.
            return
         else
            val1(1) = value
         endif
      endif
c
      curval = comand(FSTVAL+3)(1:CMDLEN)
      if ( nblank(curval) .ne. 0 ) then
         call cvatof (curval(strbeg(curval):strend(curval)), 
     &                value, error)
         if ( error ) then
            write (message, 110) curval(strbeg(curval):strend(curval)), 
     &                    comand(1)(strbeg(comand(1)):strend(comand(1)))
            call write_message
            error = .true.
            return
         else
            val4(1) = value
         endif
      endif
c
      curval = comand(FSTVAL+4)(1:CMDLEN)
      if ( nblank(curval) .ne. 0 ) then
         call cvatof (curval(strbeg(curval):strend(curval)), 
     &                value, error)
         if ( error ) then
            write (message, 110) curval(strbeg(curval):strend(curval)), 
     &                    comand(1)(strbeg(comand(1)):strend(comand(1)))
            call write_message
            error = .true.
            return
         else
            val3(1) = value
         endif
      endif
c
c
c     Format statements.
c
 110  format (1x,a,' is an incorrect value for command ',a,'.')
c
c
      return
      end

c
      subroutine mapclip (val1, val2, val3, val4, error)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine sets the values of plm1, plm2, plm3, plm4 if the
c        value of jlts LI
c     Arguments:
c        val1    real  output  the values that plm1 will be set to 
c                              provided that they are specified 
c                              correctly.
c        val2    real  output  the values that plm2 will be set to 
c                              provided that they are specified 
c                              correctly.
c        val3    real  output  the values that plm3 will be set to 
c                              provided that they are specified 
c                              correctly.
c        val4    real  output  the values that plm4 will be set to 
c                              provided that they are specified 
c                              correctly.
c        error   logi  output  indicates possible errors found in this
c                              routine.
c                              error = false   no errors detected.
c                              error = true    error detected.
c
c	 ADDED by David Bresch 930307, ETHZ
c-----------------------------------------------------------------------
      include 'incommands.icl'
      include 'message.icl'
c
c     Argument declarations.
c
      logical         error
      real            val1, val2, val3, val4
      dimension val1(2), val2(2), val3(2), val4(2)
c
c
c     Local variable declarations.
c
      character *(80)  curval
      real             value
c
c
c     External function declarations.
c
      integer nblank, strbeg, strend
c
c
c
c
c
      val1(1)=0.
      val2(1)=0.
      val3(1)=0.
      val4(1)=0.
      curval = comand(FSTVAL+1)(1:CMDLEN)
      if ( nblank(curval) .ne. 0 ) then
         call cvatof (curval(strbeg(curval):strend(curval)), 
     &                value, error)
         if ( error ) then
c if no value specified, return with zeroes.
	    val1(1)=0.
            val2(1)=0.
            val3(1)=0.
            val4(1)=0.
            return
         else
            val1(1) = value
         endif
      endif
c
      curval = comand(FSTVAL+2)(1:CMDLEN)
      if ( nblank(curval) .ne. 0 ) then
         call cvatof (curval(strbeg(curval):strend(curval)), 
     &                value, error)
         if ( error ) then
            write (message, 110) curval(strbeg(curval):strend(curval)), 
     &                    comand(1)(strbeg(comand(1)):strend(comand(1)))
            call write_message
            error = .true.
            return         
         else
            val2(1) = value
         endif
      endif
c
      curval = comand(FSTVAL+3)(1:CMDLEN)
      if ( nblank(curval) .ne. 0 ) then
         call cvatof (curval(strbeg(curval):strend(curval)), 
     &                value, error)
         if ( error ) then
            write (message, 110) curval(strbeg(curval):strend(curval)), 
     &                    comand(1)(strbeg(comand(1)):strend(comand(1)))
            call write_message
            error = .true.
            return 
        else
            val3(1) = value
         endif
      endif
c
      curval = comand(FSTVAL+4)(1:CMDLEN)
      if ( nblank(curval) .ne. 0 ) then
         call cvatof (curval(strbeg(curval):strend(curval)), 
     &                value, error)
         if ( error ) then
            write (message, 110) curval(strbeg(curval):strend(curval)), 
     &                    comand(1)(strbeg(comand(1)):strend(comand(1)))
            call write_message
            error = .true.
            return 
         else
            val4(1) = value
         endif
      endif
c
c
c     Format statements.
c
 110  format (1x,a,' is an incorrect value for command ',a,'.')
c
c
      return
      end



