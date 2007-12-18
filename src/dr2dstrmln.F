#ifdef SIXTY_FOUR_bptr
#define ive_ptr integer*8
#else
#define ive_ptr integer
#endif
      subroutine dr2dstrmln (u, v, nu, nv, imap, spval)
c-----------------------------------------------------------------------
c     $Id: dr2dstrmln.F,v 1.3 1998/07/17 19:49:41 warren Exp $
c     Purpose:
c        This routine produces 2D streamlines.
c     Auguments:
c        u      real  input  the first vector component.
c        v      real  input  the second vdctor component.
c        nu     int   input  the first dimension of u and v.
c        nv     int   input  the second dimension of u and v.
c        imap   int   input  mapping flag.
c        spval  real  input  the special value.
c     History:
c     $Log: dr2dstrmln.F,v $
c     Revision 1.3  1998/07/17 19:49:41  warren
c     Make steamlines change colors like vectors
c
c     Revision 1.2  1997/02/12 17:35:49  warren
c     Changed pointer type to ive_ptr to avoid interaction with F90 pointer type.
c
c     Revision 1.1  1993/11/30 17:33:32  warren
c     missing .F file from yesterday
c
c Revision 1.1.1.1  1993/04/07  19:39:52  harry
c Initial CVS checkin of IVE.
c
c Revision 2.0  1992/07/01  21:12:49  millerp
c Released version rereving to 2.0
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
c
      include 'strmln.icl'
      include 'color.icl'
      include 'message.icl'
      include 'plotops.icl'
c
c
c     Argument declarations.
c
      integer        nu, nv, imap, incx, incy
      real           u, v, spval
      dimension u(nu,nv), v(nu,nv)
      logical error
c
c
c     Local variable declarations.
c
      integer        ier, line_index
      ive_ptr        work
c
c
c
c     External function declarations.
c
      ive_ptr        getmem
c
c
c
c     Allocate space for a work array.
c
      work = getmem (2 * nu * nv)
c
c
c     Set some strmln common block variables.
c
      imsg = 1
      uvmsg = spval
c
c
c     Call a modified version of strmln.
c
c      
c     Set line color 
c
      call gqplci (ier, line_index)
      call gsplci (hicolor)
      call gslwsc(linwdth)
c      
      error=.false.
      call getivar('incx',incx,error)
      if(error) then
         write (message, *)' error getting incx'
         return
      endif
      call getivar('incy',incy,error)
      if(error) then
         write (message, *)' error getting incx'
         return
      endif
c     Using our strmline based on older strmline module - in STR03 common block
      INITA= incx * 2
      INITB= incy * 2
      if(incx .lt. 2) INITA= 2
      if(incy .lt. 2) INITB= 2
      call strmln1 (u, v, %val(work), nu, nu, nv, 1, ier, imap)
c
c
c     Free work array.
c
      call freemem (work)
c     Reset line color.
c
      call gsplci (line_index)
c      
c
c
      return
      end
