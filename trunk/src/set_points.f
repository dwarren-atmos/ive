      subroutine set_points (error)
c-----------------------------------------------------------------------
c     $Id: set_points.f,v 1.4 1997/02/21 21:17:09 warren Exp $
c     Purpose:
c     This subroutine sets point_1 and point_2 and unsets
c     use_buttons if the user types in point values using the command 
c     slice=(x,y,z,t),(x,y,z,t)
c
c     History:
c     $Log: set_points.f,v $
c     Revision 1.4  1997/02/21 21:17:09  warren
c     Final fixes for point/mouse slicing and expanding into 2d. It is tested
c     with the z dimention free and seems to work with others, but still should
c     be looked at with known data for this.
c
c Revision 1.3  1993/09/10  21:24:05  yuri
c 	Minor declaration bugs for DEC compiler satisfaction fixed.
c
c Revision 1.2  1993/08/31  18:00:49  yuri
c New comand "VSLICE" (vertical slice from (x1,y1) to (x2,y2)) installed
c 	into version with NEW transforms.
c
c Revision 1.1.1.1  1993/04/07  19:40:19  harry
c Initial CVS checkin of IVE.
c
c Revision 1.2  1992/10/30  19:45:46  millerp
c Added stuff for averaged line plots.
c
c Revision 1.1  1992/09/22  17:49:37  warren
c Initial revision
c
c-----------------------------------------------------------------------
      	include 'incommands.icl'
      	include 'message.icl'
        include 'plotops.icl'
        include 'pltwin.icl'
        include 'data.icl'
        include 'units.icl'
c
c     	Argument declarations.
c
      	logical       error, ajustment
      	character *(CMDLEN) curval
      	integer              ERRFLG, i, ibeg, iend
      	parameter ( ERRFLG = -999 )
	real x_1, y_1, x_2, y_2
	real x_1_scaled, y_1_scaled, x_2_scaled, y_2_scaled
        real value	
c
c     	External function declarations.
      	integer nblank, strbeg, strend
c
c      
      	error = .false. 
        ajustment = .false.
c
      	do 10 i = 1,  4 
         curval = comand(FSTVAL+(i-1))(1:CMDLEN)
         	if ( nblank(curval) .eq. 0 ) then
	      	error= .true.
         	write (message, 100) comand(1)(strbeg(comand(1)):
     &                                 strend(comand(1)))
         	call write_message
         	return
         endif
         ibeg = strbeg (curval)
         iend = strend (curval)
	 if(curval(ibeg:ibeg) .eq. '('
     * .or. curval(ibeg:ibeg) .eq. ')' ) ibeg = ibeg + 1 
	 if(curval(iend:iend) .eq. '('
     * .or. curval(iend:iend) .eq. ')' ) iend = iend - 1 
         call upcase (curval(ibeg:iend), (iend - ibeg) + 1) 
	call cvatof(curval(ibeg:iend), value,error)    
	if(i.eq.1) then
           x_1 = value
           x_1_scaled = x_1
         call unscale (x_1, 1, domain_slope(1), domain_intercept(1),
     &     0.0)
        elseif(i.eq.2) then
           y_1 = value
           y_1_scaled = y_1
         call unscale (y_1, 1, domain_slope(2), domain_intercept(2),
     &     0.0)
        elseif(i.eq.3) then
           x_2 = value
           x_2_scaled = x_2
         call unscale (x_2, 1, domain_slope(1), domain_intercept(1),
     &     0.0)
        elseif(i.eq.4) then
           y_2 = value
           y_2_scaled = y_2
         call unscale (y_2, 1, domain_slope(2), domain_intercept(2),
     &     0.0)
        endif
 10   continue
c
c     check for limits
c
      if( x_1 .lt. plmin(1) )then
         x_1 = plmin(1)
         x_1_scaled = plmin_scaled(1)
         ajustment = .true.
      endif
      if( x_2 .gt. plmax(1) )then 
        x_2 = plmax(1)   
         x_2_scaled = plmax_scaled(1)
        ajustment = .true.
      endif
      if( y_1 .lt. plmin(2) )then
         y_1 = plmin(2)
         y_1_scaled = plmin_scaled(2)
         ajustment = .true.
      endif
      if( y_2 .gt. plmax(2) )then
         y_2 = plmax(2)
         y_2_scaled = plmax_scaled(2)
         ajustment = .true.
      endif
      if(ajustment) then
         write (message, 130)x_1_scaled,  y_1_scaled,
     &     x_2_scaled, y_2_scaled
         call write_message
      endif
c
c     set up buttons = 0 for mak2dc
c
      call setivar ('use_buttons', 0, error)
c
c     set up points: time + one of (x,y) should be locked/aver
c
      call getiarr ('lock', lock, 4, error)
      if(lock(4). eq. 0)then
         error = .true.
         write (message, 120)
         call write_message
         return
      else
         point_1(4) =  tloc
         point_2(4) =  tloc
         point_3(4) =  tloc
      endif
c
c     z coord is suppose to be free, pt1,2 are in hor. plane,
c     pt3 = pt1 except z coor
         point_1(3) = plwmin(3) 
         point_2(3) = plwmin(3) 
c------------- max z
         point_3(3) = plwmax(3) 
c
c     we don't care  is x/y  locked or not
         point_1(1) = x_1
         point_1(2) = y_1
         point_2(1) = x_2
         point_2(2) = y_2
         point_3(1) = x_1
         point_3(2) = y_1
c
         call setrarr ('point_1', point_1, 4, error)
         call setrarr ('point_2', point_2, 4, error)
         call setrarr ('point_3', point_3, 4, error)
c
c       Format statements.
c
 100  format (1x,'Command ',a,' requires a value.')
 120  format (1x,' Time should not be free for vertical slice')
 130  format (1x,' This slice out of limits, new slice is:',
     &  4(f12.6,' '))
c
      return
      end

