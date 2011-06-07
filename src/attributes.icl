c-----------------------------------------------------------------------
c     $Id: attributes.icl,v 1.11 2002/08/09 19:57:55 warren Exp $
c     Purpose:
c        This file contains all common blocks which carry global
c        variables related to the attributes of the current var.
c
c     History:
c       $Log: attributes.icl,v $
c       Revision 1.11  2002/08/09 19:57:55  warren
c       Fixed for Linux and tested with DEC sun etc.
c       most items inside #ifdef's, or were general cleanup/fixes
c
c       Revision 1.10  2001/01/23 22:47:08  davidive
c       cpline.f: fixed to use zero_color
c       everything else: First cut at wireframes and isosurfaces. No dimentions yet
c       no widgets yet.
c
c       Revision 1.9  1999/12/22 00:33:06  warren
c       Fixed Dales crashing bug when changing from point slicer to normal. Also
c       have some ifdef linuxes in there in case we ever find a fortran compiler.
c
c       Revision 1.8  1995/12/15 22:33:13  warren
c       moved patterns into 4.0 and implemented the list of fixes and changes from
c       Curtis.
c
cRevision 1.7  1994/02/09  01:24:44  warren
cmake sure data_display_units is saved and increase MKS in areas calls.
c
cRevision 1.6  1993/11/16  01:14:08  warren
crest of the rearranging for alignment.
c
cRevision 1.5  1993/08/18  15:47:09  harry
cChange to new transform routines.
c
cRevision 1.4  1993/07/09  16:35:24  harry
cAdd number of contour values to attributes and change its meaning so
cthat a zero or negative number means use contour interval instead.
c
cRevision 1.3  1993/05/20  17:33:01  warren
cAdditions and changes to make the new forms active.
c
cRevision 1.2  1993/05/04  18:11:08  warren
cadded the command contour_values
c
cRevision 1.1.1.1  1993/04/07  19:39:47  harry
cInitial CVS checkin of IVE.
c
c Revision 1.2  1993/01/12  20:30:38  warren
c resized disp_units and separated chars from common.
c
c Revision 1.1  1993/01/01  00:46:55  warren
c Initial revision
c
c-----------------------------------------------------------------------


      real major_tic, minor_tic, lmax, lmin
      real cont_int_bck, cont_values_bck
      real cont_int, cont_values, sint
      integer num_vals, num_vals_bck, linlog3d
      logical lin, read_from_nc, no_zero
      dimension cont_values(40), cont_values_bck(40)
      common / var_atts / major_tic, minor_tic, lmax, lmin,
     &                    cont_int_bck, cont_int,
     &                    no_zero, num_vals, num_vals_bck,
     &                    lin, read_from_nc,
     &                    cont_values_bck, cont_values, sint, linlog3d
