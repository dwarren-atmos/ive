      subroutine set_units (error)
c-----------------------------------------------------------------------
c     $Id: set_units.f,v 1.6 1999/03/23 19:10:30 warren Exp $
c     Purpose:
c        This routine sets the data display units. These specified units
c        are used to convert the data from the units of the data in the 
c        netCDF data file to the specified units. The data will then be
c        displayed in the specified units. Note that this 
c        routine cannot check to see if a valid unit string is entered.
c        Error checking is done at the time that the units are 
c        converted.
c     Arguments:
c        error  logi  output  error flag.
c     History:
c     $Log: set_units.f,v $
c     Revision 1.6  1999/03/23 19:10:30  warren
c     Added command to set units on any field reguardless of current units.
c     This is primarily for fields derived by IVE math.
c
c     Revision 1.5  1996/11/06 20:55:04  harry
c     Maintain domain units across different data files.
c
c     Revision 1.4  1996/07/12 20:59:05  harry
c     Change data and domain slopes and intercepts to real*8.
c
c Revision 1.3  1993/08/18  15:47:43  harry
c Change to new transform routines.
c
c Revision 1.2  1993/07/26  19:05:45  warren
c Fixed bug in X part of set units.
c
c Revision 1.1.1.1  1993/04/07  19:40:19  harry
c Initial CVS checkin of IVE.
c
c Revision 2.1  1992/09/24  23:15:31  harry
c Add processing for DEFAULT option.
c
c Revision 2.0  1992/07/01  21:16:33  millerp
c Released version rereving to 2.0
c
c Revision 1.4  1992/07/01  16:10:40  millerp
c Added catnull before call to convert_units.
c
c Revision 1.3  1992/06/15  19:02:07  millerp
c Change argument list for scale.
c
c Revision 1.2  1992/06/10  20:12:55  millerp
c *** empty log message ***
c
c Revision 1.1  1992/06/01  16:18:21  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
c
      include 'incommands.icl'
      include 'message.icl'
      include 'units.icl'
c
c
c     Argument declarations.
c
      logical  error
c
c
c     Local variable declarations.
c
      character *(CMDLEN)   curval
      character *(UNIT_LEN) value
      character *(80)       field
      integer               beg, stend, status, varid
      real*8                slope, intercept
c
c
c     External function declarations.
c
      integer               convert_units, getvid, 
     &                      nblank, strbeg, strend
c
c
c
c
      error = .false.
c
c
c     Get the first value for the current command.
c
      curval = comand(fstval)(1:CMDLEN)
      beg = strbeg(curval)
      stend = strend(curval)
c
c
c     If a value is not given, do not set data_display_units.
c     Otherwise set data_display_units to the specified
c     string. Do not change case since some unit specifications are
c     case-sensitive.
c
      if ( nblank(curval) .ne. 0 ) then
c
c              Get current field and its id.
c
         call getavar ('field', field, error)
         varid = getvid (field)
         if ( varid .lt. 0) then
            error = .true.
            write (message,100)
            call write_message
            return
         endif
c
c        Make sure destination string is long enough. If not, log an
c        error.
c
         if ( UNIT_LEN .ge. (stend - beg + 1) ) then  
            if (curval(beg:stend) .eq. 'DEFAULT' .or.
     &           curval(beg:stend) .eq. 'default') then
               data_display_units = 'DEFAULT' // char(0)
               call change_units (varid, data_display_units)
               status = convert_units (data_units, data_display_units,
     &              slope, intercept)
            else
               value = curval(beg:stend)//char(0)
               call catnull (data_units, len(data_units), error)
               status = convert_units (data_units, value,
     &              slope, intercept)
               if ( status .eq. 0) call change_units(varid, value)
            endif
            if ( status .eq. 0 ) then
               data_display_units = value
               data_slope = slope
               data_intercept = intercept
            else
               write (message, 100)
               call write_message
               error = .true.
            endif
         else
            write (message, 110) UNIT_LEN
            call write_message 
            error = .true.
         endif
      endif
c
c
c     Format statements.
c
 100  format (1x, 'SET_UNITS: error setting data_display_units.')
 110  format (1x, 'SET_UNITS: maximum length of units string ',
     &            'is ',i2,' characters.')
c
c
      return
      end
c
c
      subroutine set_domain_units (error)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine sets the domain display units. These specified 
c        units are used to convert the physical domain units from the 
c        units of the domain in the netCDF data file to the specified 
c        units. The plot will then be displayed in the specified units. 
c        Note that this routine cannot check to see if a valid unit 
c        string is entered. Error checking is done at the time that the 
c        units are converted.
c     Arguments:
c        error  logi  output  error flag.
c
c-----------------------------------------------------------------------
c
c
      include 'incommands.icl'
      include 'message.icl'
      include 'units.icl'
c
c
c     Argument declarations.
c
      logical  error
c
c
c     Local variable declarations.
c
      character *(CMDLEN)   curval
      character *(UNIT_LEN) string
      character *(1)        attname          
      integer               beg, stend, i, im1, numerr, status
      real*8                slope, intercept
      dimension attname(4), string(MAX_UNITS), 
     &          slope(MAX_UNITS), intercept(MAX_UNITS)
      data attname(1) / 'x' /,
     &     attname(2) / 'y' /,
     &     attname(3) / 'z' /,
     &     attname(4) / 't' /
c
c
c     External function declarations.
c
      integer               convert_units, nblank, strbeg, strend
c
c
c
c
      error = .false.
      numerr = 0
c
c
      do 10 i = 1, MAX_UNITS 
         im1 = i - 1
         curval = comand(fstval+im1)(1:CMDLEN)
         beg = strbeg(curval)
         stend = strend(curval)
c
c        If a value is not given, do not set domain_display_units.
c        Otherwise set domain_display_units to the specified
c        string. Do not change case since some unit specifications are
c        case-sensitive.
c
         if ( nblank(curval) .ne. 0 ) then
c
c           Make sure destination string is long enough. If not, log an
c           error.
c
            if ( UNIT_LEN .ge. (stend - beg + 1) ) then  
               if (curval(beg:stend) .eq. 'DEFAULT' .or.
     &              curval(beg:stend) .eq. 'default') then
                  string(i) = 'DEFAULT' // char(0)
                  call change_domain_units(i, string(i))
               else
                  string(i) = curval(beg:stend)//char(0)
               endif
            else
               write (message, 100) UNIT_LEN
               call write_message 
               numerr = numerr + 1
            endif
         else
            string(i) = domain_display_units(i)
     &                 (strbeg(domain_display_units(i)):
     &                  strend(domain_display_units(i)))
         endif
 10   continue
c
      if ( numerr .ne. 0 ) then
         error = .true.
      endif
c
c
c     Set the values of domain_units, domain_intercept and domain_slope.
c     First convert the units.
c
      if ( .not. error ) then
         do 15 i = 1, MAX_UNITS
            call catnull (string(i), len(string(i)), error)
            call catnull (domain_units(i), len(domain_units(i)), error)
            status = convert_units (domain_units(i), string(i), 
     &                              slope(i), intercept(i)) 
            if ( status .ne. 0 ) then
               numerr = numerr + 1
            endif
 15      continue
      endif
c
      if ( numerr .ne. 0 ) then
         error = .true.
      endif
c
c     Now set the values.
c
      if ( .not. error ) then
         do 18 i = 1, MAX_UNITS
            domain_slope(i) = slope(i)
            domain_intercept(i) = intercept(i)
            domain_display_units(i) = string(i)
            call change_domain_units(i, string(i))
 18      continue
      endif
c
c
c     Rescale the plot limits according to the new units.
c
      if ( .not. error ) then
         call scale_plot_limits
      endif
c
c
c     Format statements.
c
 100  format (1x, 'SET_DOMAIN_UNITS: maximum length of units string ',
     &            'is ',i2,' characters.')
c
c
      return
      end
c
      subroutine scale_plot_limits
c
c
      include 'data.icl'
      include 'units.icl'
c
c
c     Local variable declarations.
c
      integer       i
c
c
c
c
      do 10 i = 1, MAXDIM
         plmin_scaled(i)  = plmin(i)
         plmax_scaled(i)  = plmax(i)
         plwmin_scaled(i) = plwmin(i)
         plwmax_scaled(i) = plwmax(i)
         call scale (plmin_scaled(i), 1, domain_slope(i), 
     &               domain_intercept(i), 0.0)
         call scale (plmax_scaled(i), 1, domain_slope(i), 
     &               domain_intercept(i), 0.0)
         call scale (plwmin_scaled(i), 1, domain_slope(i), 
     &               domain_intercept(i), 0.0)
         call scale (plwmax_scaled(i), 1, domain_slope(i), 
     &               domain_intercept(i), 0.0)
 10   continue
c
c
      return
      end



      subroutine force_set_units (error)
c-----------------------------------------------------------------------
c     $Id: set_units.f,v 1.6 1999/03/23 19:10:30 warren Exp $
c     Purpose:
c        This routine sets the data display units. These specified units
c        are used to convert the data from the units of the data in the 
c        netCDF data file to the specified units. The data will then be
c        displayed in the specified units. Note that this 
c        routine cannot check to see if a valid unit string is entered.
c        Error checking is done at the time that the units are 
c        converted.
c     Arguments:
c        error  logi  output  error flag.
c-----------------------------------------------------------------------
c
c
      include 'incommands.icl'
      include 'message.icl'
      include 'units.icl'
c
c
c     Argument declarations.
c
      logical  error
c
c
c     Local variable declarations.
c
      character *(CMDLEN)   curval
      character *(80)       units
      character *(80)       field
      integer               beg, stend, status, varid
      real*8                slope, intercept
c
c
c     External function declarations.
c     
      integer               getvid, 
     &     nblank, strbeg, strend
c
c
c
c
      error = .false.
c
c
c     Get the first value for the current command.
c
c
c
c     If a value is not given, do not set data_display_units.
c     Otherwise set data_display_units to the specified
c     string. Do not change case since some unit specifications are
c     case-sensitive.
c     
      curval = comand(fstval)(1:CMDLEN)
      beg = strbeg(curval)
      stend = strend(curval)
      if ( nblank(curval) .ne. 0 ) then
c     
c     Get current field and its id.
c     
         field = curval(beg:stend)//char(0)
         curval = comand(fstval+1)(1:CMDLEN)
         beg = strbeg(curval)
         stend = strend(curval)
         if ( nblank(curval) .ne. 0 ) then
            units=curval(beg:stend)//char(0)

            varid = getvid (field)
            if ( varid .lt. 0) then
               error = .true.
               write (message,100)
               call write_message
               return
            endif
c
c        Make sure destination string is long enough. If not, log an
c        error.
c
            call force_change_units(varid, units)
            data_display_units = units
            data_slope = 1.
            data_intercept = 0.
         else
            write (message, 100) UNIT_LEN
            call write_message 
            error = .true.
         endif
      else
         write (message, 100) UNIT_LEN
         call write_message 
         error = .true.
      endif
c
c
c     Format statements.
c
 100  format (1x, 'FORCE_SET_UNITS: Requires valid field and units')
c
c
      return
      end
