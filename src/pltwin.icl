c-----------------------------------------------------------------------
c     $Id: pltwin.icl,v 1.7 2001/01/23 22:47:14 davidive Exp $
c     Purpose:
c     Common blocks used by set, slicer and line labeling
c	axes - 0 = var, 1 = x, 2 = y, 3 = z, 4 = t, 5 = combo
c
c     History:
c     $Log: pltwin.icl,v $
c     Revision 1.7  2001/01/23 22:47:14  davidive
c     cpline.f: fixed to use zero_color
c     everything else: First cut at wireframes and isosurfaces. No dimentions yet
c     no widgets yet.
c
c     Revision 1.6  1997/06/27 21:40:15  davidive
c     Fix arbitrary slice line labeling
c
cRevision 1.5  1995/09/05  20:34:05  harry
cAdd new slicer.  Good luck!
c
cRevision 1.4  1995/06/23  21:16:27  warren
cFirst part of new ive help
cChanged " to ' in fortran files
cFixed the label spacing in 1d plots
cadded frame_width command
c
cRevision 1.3  1994/10/08  19:01:34  warren
cThe annotation widget and updating it. As well as changing tic_bold to
ctic_labels and adding an off option.
c
cRevision 1.2  1994/06/28  17:43:34  warren
cFirst cut at fancy tic marks.
c
cRevision 1.1.1.1  1993/04/07  19:40:13  harry
cInitial CVS checkin of IVE.
c
c Revision 1.1  1992/10/02  22:58:59  warren
c Initial revision
c
c-----------------------------------------------------------------------
      real tic_width_maj, tic_width_min
      integer tic_labels, tic_decimals
      real    tic_scale, frmwdth,xoff,yoff
      common / tic_stuff / tic_width_maj, tic_width_min, tic_labels, 
     &	                   tic_decimals, tic_scale, frmwdth,xoff,yoff

      real x1, x2, y1, y2
      integer  xaxis, yaxis, window_axes, reverse
      integer  i_min, i_max, j_min, j_max
      common / window_points / x1, x2, y1, y2, i_min, i_max,
     &	                       j_min, j_max, xaxis, yaxis, 
     &			       window_axes, reverse

      real slice_slope, slice_inter
      integer xdim_lab, ydim_lab
      common / label_values / slice_slope, slice_inter,
     &     xdim_lab, ydim_lab

      real     x13, x23, y13, y23, z13, z23
      integer  imin3, imax3, jmin3, jmax3, kmin3, kmax3	
      integer  xaxis3, yaxis3, zaxis3, window_axes3, reverse3

      common / window_points3 / x13, x23, y13, y23, z13, z23,
     & 	                        imin3,imax3,jmin3,jmax3,kmin3,kmax3,
     &                          xaxis3, yaxis3, zaxis3,
     &                          window_axes3, reverse3
