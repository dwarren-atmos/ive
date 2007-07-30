c-----------------------------------------------------------------------
c     $Id: slices.icl,v 1.2 1997/02/12 17:36:37 warren Exp $
c     Purpose:
c        This file contains the common blocks for the current 1D and
c	 2D slices.
c
c     History:
c       $Log: slices.icl,v $
c       Revision 1.2  1997/02/12 17:36:37  warren
c       Changed pointer type to ive_ptr to avoid interaction with F90 pointer type.
c
c       Revision 1.1  1995/05/11 22:15:13  harry
c       Add the WRITE_1D command.
c
c
c-----------------------------------------------------------------------
c
      ive_ptr slabad_1d,	! Dependent variable 1d slice
     &        slabloc_1d,	! Independent variable 1d slice
     &        slabad_2d		! 2d slice
      integer ni_1d,		! Size of 1d slice
     &        ni_2d, nj_2d,	! Size of 2d slice
     &        axes		! Which dimension is the indep. var in 1d

      common/slice_com/slabad_1d, slabloc_1d, slabad_2d, ni_1d,
     &     ni_2d, nj_2d, axes

