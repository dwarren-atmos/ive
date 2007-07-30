c-----------------------------------------------------------------------
c     $Id: data.icl,v 1.14 2005/11/03 19:51:56 warren Exp $
c     Purpose:
c        This file contains all common blocks which carry global
c        variables related to the data files and directories.
c
c     History:
c	$Log: data.icl,v $
c	Revision 1.14  2005/11/03 19:51:56  warren
c	Fixed redraw command to really redraw plots
c	
c	Revision 1.13  2002/08/09 19:57:57  warren
c	Fixed for Linux and tested with DEC sun etc.
c	most items inside #ifdef's, or were general cleanup/fixes
c	
c	Revision 1.12  1997/12/18 20:25:22  harry
c	Add field_delta, which is needed for derivative.
c	
c	Revision 1.11  1997/04/15 17:24:38  harry
c	Remove time_tag - use multiple files instead.
c	Various fixes to vector/scalar looping.
c
c	Revision 1.10  1997/04/09 19:37:05  harry
c	Add scalar_name so when you go from vector to scalar you end up with
c	the last scalar field.
c
c	Revision 1.9  1997/02/07 20:16:58  harry
c	Yet more MULTIPLE_READ changes.
c
c	Revision 1.8  1996/10/09 18:54:35  harry
c	Add the ability to loop on file names if time value is part of name,
c	and there is only one time per file.
c
c	Revision 1.7  1996/03/27 23:53:57  harry
c	Slicer should check new variable "field_pointer" to find out name of
c	field currently being sliced instead of "field" to properly handle
c	vectors and other future plots (like Skew-T).
c
cRevision 1.6  1995/10/04  22:09:50  harry
cAdd new netCDF attributes and specification of staggering.  Add
cchanges to slicer and plot labels to reflect user specified labels.
c
cRevision 1.5  1995/09/05  20:33:41  harry
cAdd new slicer.  Good luck!
c
cRevision 1.4  1994/02/17  21:34:38  harry
cAdd data_dim_names and num_dims to common blocks.
c
cRevision 1.3  1993/12/28  21:00:46  harry
cDeath to constfil!
c
cRevision 1.2  1993/11/29  19:39:09  warren
cChanges to 3.1 to make it work on alphas.
cFORTRAN: new data type pointer ifdefed to either 32 bit or 64 bit.
cC: Change int to long where necessary.
cFiles: Quite a few .f files are now .F
c
cRevision 1.1.1.1  1993/04/07  19:39:51  harry
cInitial CVS checkin of IVE.
c
cRevision 2.2  1993/01/04  16:06:25  millerp
cMake field length 80 chars long.
c
cRevision 2.1  1992/12/31  18:45:17  millerp
cAdd button_name, delete unused variables.
c
cRevision 2.0  1992/07/01  21:15:41  millerp
cReleased version rereving to 2.0
c
cRevision 1.2  1992/06/10  17:40:22  millerp
cAdded scaled plot window variables.
c
cRevision 1.1  1992/04/21  00:11:26  millerp
cInitial revision
c
c-----------------------------------------------------------------------
c
c
      integer MAX_FIELD_LENGTH
      parameter (MAX_FIELD_LENGTH = 80)
c
c     Data variables.
c     The field to be plotted.
c
      character *(MAX_FIELD_LENGTH) field, button_name, field_pointer
c
c     The name of the netCDF file where field can be found.
c
      character *256 datfil, old_file, scalar_name

      common / datablk   / datfil, old_file, field, button_name,
     &                     field_pointer, scalar_name
c
c     Graphics file and transform file names.
c
      character *(256) graphics_path, transform_path
      common / filepaths / graphics_path, transform_path
c
c
c     The arrays cpmin and cpmax hold the minimum and maximum 
c     dimensions of the computational grid. 
c     The arrays cpwmin and cpwmax hold the minimum and maximum
c     dimensions of the windowed portion of the computational grid.
c
      integer cpmin1, cpmin2, cpmin3, cpmin4, 
     &        cpmax1, cpmax2, cpmax3, cpmax4,
     &        cpwmin, cpwmax
      common / cpdims   / cpmin1, cpmin2, cpmin3, cpmin4,
     &                    cpmax1, cpmax2, cpmax3, cpmax4
      common / cpwindow / cpwmin, cpwmax
c
c
c     The arrays phmin and phmax hold the minimum and maximum 
c     dimensions of the physical grid for any variable. 
c
      real phmin, phmax
      integer MAXDIM, num_dims
      parameter ( MAXDIM = 4 )
      dimension phmin(MAXDIM), phmax(MAXDIM)
      common / phdims   / phmin, phmax, num_dims
c
c     The names of the current field's dimensions and display labels
c
      character *(MAX_FIELD_LENGTH) data_dim_names(MAXDIM),
     &          dim_display_names(MAXDIM)
      common / cphdims / data_dim_names, dim_display_names
c
c
c     The arrays plmin and plmax hold the minimum and maximum
c     dimensions of the plot domain. 
c     The arrays plwmin and plwmax hold the minimum and maximum
c     dimensions of the windowed portion of the physical grid.
c     The arrays plwmin_scaled and plwmax_scaled hold the 
c     minimum and maximum scaled
c     dimensions of the windowed portion of the physical grid.
c
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
c
c     The array stag contains staggering information.
c     The array grid_delta contains the grid spacing.
c     The array field_delta contains the current field grid spacing.
c
      real stag, grid_delta, field_delta
c
      dimension stag(MAXDIM), grid_delta(MAXDIM), field_delta(MAXDIM)
      common / staggering / stag, grid_delta, field_delta
c
c
c     Maximum number of variables (in nteCDFfile + diagnostic) allowed.
c
      integer MAXVAR 
      parameter ( MAXVAR = 300 )
