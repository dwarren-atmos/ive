!-----------------------------------------------------------------------
!     $Id: data.icl,v 1.14 2005/11/03 19:51:56 warren Exp $
!     Purpose:
!        This file contains all common blocks which carry global
!        variables related to the data files and directories.
!
!     History:
!	$Log: data.icl,v $
!	Revision 1.14  2005/11/03 19:51:56  warren
!	Fixed redraw command to really redraw plots
!	
!	Revision 1.13  2002/08/09 19:57:57  warren
!	Fixed for Linux and tested with DEC sun etc.
!	most items inside #ifdef's, or were general cleanup/fixes
!	
!	Revision 1.12  1997/12/18 20:25:22  harry
!	Add field_delta, which is needed for derivative.
!	
!	Revision 1.11  1997/04/15 17:24:38  harry
!	Remove time_tag - use multiple files instead.
!	Various fixes to vector/scalar looping.
!
!	Revision 1.10  1997/04/09 19:37:05  harry
!	Add scalar_name so when you go from vector to scalar you end up with
!	the last scalar field.
!
!	Revision 1.9  1997/02/07 20:16:58  harry
!	Yet more MULTIPLE_READ changes.
!
!	Revision 1.8  1996/10/09 18:54:35  harry
!	Add the ability to loop on file names if time value is part of name,
!	and there is only one time per file.
!
!	Revision 1.7  1996/03/27 23:53:57  harry
!	Slicer should check new variable "field_pointer" to find out name of
!	field currently being sliced instead of "field" to properly handle
!	vectors and other future plots (like Skew-T).
!
!Revision 1.6  1995/10/04  22:09:50  harry
!Add new netCDF attributes and specification of staggering.  Add
!changes to slicer and plot labels to reflect user specified labels.
!
!Revision 1.5  1995/09/05  20:33:41  harry
!Add new slicer.  Good luck!
!
!Revision 1.4  1994/02/17  21:34:38  harry
!Add data_dim_names and num_dims to common blocks.
!
!Revision 1.3  1993/12/28  21:00:46  harry
!Death to constfil!
!
!Revision 1.2  1993/11/29  19:39:09  warren
!Changes to 3.1 to make it work on alphas.
!FORTRAN: new data type pointer ifdefed to either 32 bit or 64 bit.
!C: Change int to long where necessary.
!Files: Quite a few .f files are now .F
!
!Revision 1.1.1.1  1993/04/07  19:39:51  harry
!Initial CVS checkin of IVE.
!
!Revision 2.2  1993/01/04  16:06:25  millerp
!Make field length 80 chars long.
!
!Revision 2.1  1992/12/31  18:45:17  millerp
!Add button_name, delete unused variables.
!
!Revision 2.0  1992/07/01  21:15:41  millerp
!Released version rereving to 2.0
!
!Revision 1.2  1992/06/10  17:40:22  millerp
!Added scaled plot window variables.
!
!Revision 1.1  1992/04/21  00:11:26  millerp
!Initial revision
!
!-----------------------------------------------------------------------
!
!
      integer MAX_FIELD_LENGTH
      parameter (MAX_FIELD_LENGTH = 80)
!
!     Data variables.
!     The field to be plotted.
!
      character *(MAX_FIELD_LENGTH) field, button_name, field_pointer
!
!     The name of the netCDF file where field can be found.
!
      character *256 datfil, old_file, scalar_name

      common / datablk   / datfil, old_file, field, button_name,
     &                     field_pointer, scalar_name
!
!     Graphics file and transform file names.
!
      character *(256) graphics_path, transform_path
      common / filepaths / graphics_path, transform_path
!
!
!     The arrays cpmin and cpmax hold the minimum and maximum 
!     dimensions of the computational grid. 
!     The arrays cpwmin and cpwmax hold the minimum and maximum
!     dimensions of the windowed portion of the computational grid.
!
      integer cpmin1, cpmin2, cpmin3, cpmin4, 
     &        cpmax1, cpmax2, cpmax3, cpmax4,
     &        cpwmin, cpwmax
      common / cpdims   / cpmin1, cpmin2, cpmin3, cpmin4,
     &                    cpmax1, cpmax2, cpmax3, cpmax4
      common / cpwindow / cpwmin, cpwmax
!
!
!     The arrays phmin and phmax hold the minimum and maximum 
!     dimensions of the physical grid for any variable. 
!
      real phmin, phmax
      integer MAXDIM, num_dims
      parameter ( MAXDIM = 4 )
      dimension phmin(MAXDIM), phmax(MAXDIM)
      common / phdims   / phmin, phmax, num_dims
c
!     The names of the current field's dimensions and display labels
c
      character *(MAX_FIELD_LENGTH) data_dim_names(MAXDIM),
     &          dim_display_names(MAXDIM)
      common / cphdims / data_dim_names, dim_display_names
!
!
!     The arrays plmin and plmax hold the minimum and maximum
!     dimensions of the plot domain. 
!     The arrays plwmin and plwmax hold the minimum and maximum
!     dimensions of the windowed portion of the physical grid.
!     The arrays plwmin_scaled and plwmax_scaled hold the 
!     minimum and maximum scaled
!     dimensions of the windowed portion of the physical grid.
!
      real       plmin, plmax, plmin_scaled, plmax_scaled,
     &           plwmin, plwmax, plwmin_scaled, plwmax_scaled
      dimension  plmin(MAXDIM),  plmax(MAXDIM),
     &           plmin_scaled(MAXDIM), plmax_scaled(MAXDIM),
     &           plwmin(MAXDIM), plwmax(MAXDIM),
     &           plwmin_scaled(MAXDIM), plwmax_scaled(MAXDIM) 
      common / plotdims   / plmin, plmax, plmin_scaled, plmax_scaled
      common / plotwindow / plwmin, plwmax, plwmin_scaled, plwmax_scaled

      integer    coord_dep      
      dimension coord_dep(MAXDIM, MAXDIM)
      common / coord_dep / coord_dep
!
!     The array stag contains staggering information.
!     The array grid_delta contains the grid spacing.
!     The array field_delta contains the current field grid spacing.
!
      real stag, grid_delta, field_delta
!
      dimension stag(MAXDIM), grid_delta(MAXDIM), field_delta(MAXDIM)
      common / staggering / stag, grid_delta, field_delta
!
!
!     Maximum number of variables (in nteCDFfile + diagnostic) allowed.
!
      integer MAXVAR 
      parameter ( MAXVAR = 300 )
