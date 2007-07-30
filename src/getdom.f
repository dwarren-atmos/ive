      subroutine getdom (iwi, jwi, kwi, nxw, nyw, nzw)
c-----------------------------------------------------------------------
c     $Id: getdom.f,v 1.6 1994/02/16 19:19:01 yuri Exp $
c     Purpose:
c        This routine gets the domain limits in the computational grid.
c     Arguments:
c        iwi  int  output  the beginning of the windowed domain in the
c                          computational grid along the first dimension.
c        jwi  int  output  the beginning of the windowed domain in the
c                          computational grid along the second 
c                          dimension.
c        kwi  int  output  the beginning of the windowed domain in the
c                          computational grid along the third dimension.
c        nxw  int  output  the number of points in the windowed domain
c                          in the computational grid along the first
c                          dimension.
c        nyw  int  output  the number of points in the windowed domain
c                          in the computational grid along the second
c                          dimension.
c        nzw  int  output  the number of points in the windowed domain
c                          in the computational grid along the third
c                          dimension.
c     History:
c     $Log: getdom.f,v $
c     Revision 1.6  1994/02/16 19:19:01  yuri
c     Backing up to slicer in physical space.
c
c Revision 1.4  1994/01/28  23:11:20  harry
c Correct calculation of maximum index value.
c
c Revision 1.3  1993/11/16  01:16:42  warren
c change jmin0 and jmax0 to min and max
c
c Revision 1.2  1993/08/18  15:47:28  harry
c Change to new transform routines.
c
c Revision 1.1.1.1  1993/04/07  19:39:58  harry
c Initial CVS checkin of IVE.
c
c Revision 2.1  1992/10/23  17:23:25  millerp
c  Made slicer & getdom compatable.
c
c Revision 2.0  1992/07/01  21:13:13  millerp
c Released version rereving to 2.0
c
c Revision 1.2  1992/06/18  19:57:11  millerp
c Change dimension of plwmin, compt, iflag to 4.
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
      include 'data.icl'
c

c     Argument declarations.
c
      integer    iwi, jwi, kwi, nxw, nyw, nzw
c
c
c     Local variable declarations.
c
      integer    iflag
      real       compt
      dimension  compt(MAXDIM), iflag(MAXDIM)
c
c
c
c
      iflag(1) = 1
      iflag(2) = 1
      iflag(3) = 1
      iflag(4) = 0
c
c
c     Convert the minimum plot window locations in physical space to 
c     points in the computational grid of the variable.
c
      call phys_2_index_trans (plwmin, compt, iflag, MAXDIM, 1)
c
c     Points are bounded below by 1 (since array indicies are never
c     less than 1).
c
      iwi = max (int (compt(1)), 1) 
      jwi = max (int (compt(2)), 1)
      kwi = max (int (compt(3)), 1)
c
c     Convert the maximum plot window locations in physical space to 
c     points in the computational grid of the variable.
c
      call phys_2_index_trans (plwmax, compt, iflag, MAXDIM, 1)
      compt(1) = min(compt(1)+1.0, float(cpmax1))
      compt(2) = min(compt(2)+1.0, float(cpmax2))
      compt(3) = min(compt(3)+1.0, float(cpmax3))
c
c     Compute the number of computational points in the windowed
c     domain. The number of points is bounded above by the maximum 
c     computational grid size.
c
      nxw = min (int (compt(1)), cpmax1) -iwi + 1
      nyw = min (int (compt(2)), cpmax2) -jwi + 1
      nzw = min (int (compt(3)), cpmax3) -kwi + 1
c
c
      return
      end
