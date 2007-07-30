c-----------------------------------------------------------------------
c     $Id: ive_widgets.icl,v 1.23 2002/08/09 19:57:58 warren Exp $
c     $Log: ive_widgets.icl,v $
c     Revision 1.23  2002/08/09 19:57:58  warren
c     Fixed for Linux and tested with DEC sun etc.
c     most items inside #ifdef's, or were general cleanup/fixes
c
c     Revision 1.22  2002/05/22 17:56:45  davidive
c     First working version of trajectories colored by alternative fields
c     There is not a complete interface yet!!!
c
c     Revision 1.21  2002/03/11 20:14:17  davidive
c     Fixed ui_pudate to update trajectory step and value.
c
c     Revision 1.20  1999/02/04 00:28:29  harry
c     Add vector scaling.
c
c     Revision 1.19  1999/01/29 20:46:31  warren
c     ets the flag for time independence and the plot the trajectories
c
c     Revision 1.18  1998/12/31 16:01:42  warren
c     First cut of new trajectory look and feel. Trajectories are no longer
c     on the popup menu, just plot type.
c
c     Revision 1.17  1998/03/04 19:14:01  davidive
c     First cut at trajectories and trajectory widget using colors to show depth
c     and arrows to show direction.
c
c     Revision 1.16  1997/06/27 21:06:51  harry
c     Designed multiplot printing.
c
c     Revision 1.15  1997/04/11 18:48:20  warren
c     Fixed for sgi and also to update color widget more frequently.
c
c     Revision 1.14  1996/11/07 16:55:34  warren
c     Fixed to deal with the case of use_buttons == 0
c
c     Revision 1.13  1996/04/25 16:25:18  harry
c     Add Skew-T plots and the beginning of trajectory plots.
c
c     Revision 1.12  1996/04/08 18:57:12  warren
c     first parts of num unlabeled lines for terrain and fixed transform routine
c     to use last directory if none is given.
c
c     Revision 1.11  1996/04/01 20:23:42  warren
c     Terrain form and new terrain command (on / off)
c
c     Revision 1.10  1995/12/15 22:33:18  warren
c     moved patterns into 4.0 and implemented the list of fixes and changes from
c     Curtis.
c
c Revision 1.9  1994/11/22  22:47:54  harry
c Correct vector locking, add support for vector positioning, add
c support for filled contour overlays.  Use new NCAR vector plotting
c routine instead of modified old velvct.
c
c Revision 1.8  1994/10/08  19:01:31  warren
c The annotation widget and updating it. As well as changing tic_bold to
c tic_labels and adding an off option.
c
c Revision 1.7  1994/09/14  18:19:57  harry
c Big 1D flip uncommit.
c
c Revision 1.6  1994/04/19  23:03:07  yuri
c Big 1D flip commit.
c
c Revision 1.5  1994/04/14  23:29:16  harry
c Make sure vector locking can only be turned on after a vector plot.
c Also fix the user interface updating for the vector_lock command.
c
c Revision 1.4  1993/12/15  18:15:36  harry
c Add support for DEFAULT_MAP command, default_map transform.  Modify map
c clipping to work properly with the map background widget.
c
c Revision 1.3  1993/07/07  18:15:24  harry
c Remove "TIME" update.
c
c Revision 1.2  1993/06/25  20:07:27  warren
c Call ui_update for the map stuff when needed.
c
c Revision 1.1  1993/05/20  17:35:00  warren
c Additions and changes to make the new forms active.
c
cRevision 1.1.1.1  1993/04/07  19:40:41  harry
cInitial CVS checkin of IVE.
c
cRevision 2.4  1992/10/15  23:01:21  warren
cadded color locking stuff
c
cRevision 2.3  1992/10/07  22:19:58  harry
cAdd LOOP_UI.
c
cRevision 2.2  1992/09/30  17:30:59  warren
cremoved removed sounding, average and vindex
c
cRevision 2.1  1992/08/24  21:33:33  warren
cadded time
c
cRevision 2.0  1992/07/01  21:16:02  millerp
cReleased version rereving to 2.0
c
cRevision 1.7  1992/06/15  19:00:51  warren
cadded average for line plots
c
cRevision 1.6  1992/06/10  21:16:53  warren
cadded LOC
c
cRevision 1.5  1992/06/10  17:36:53  millerp
c*** empty log message ***
c
cRevision 1.4  1992/06/09  00:16:41  warren
cadded ability to change contour options to line or vector
c
cRevision 1.3  1992/05/06  23:16:17  warren
cnew map form finished.
c
cRevision 1.2  1992/05/06  21:52:00  warren
cnew map info form
c
cRevision 1.1  1992/04/21  00:11:26  millerp
cInitial revision
c
c-----------------------------------------------------------------------
      integer DIR_UI, BUTTONS_UI, CINT_UI, TCINT_UI, NUNLAB_UI, 
     &        NOZERO_UI, LABELPLOT_UI, LABELLINE_UI, SINGLEVAL_UI, 
     &        FILLTYPE_UI, AXES_UI, SAVE_UI, XTICS_UI, YTICS_UI,
     &        WINDOW_UI, TRANS_UI, CTB_UI, DRMAP_UI, MAPOUT_UI, LAT_UI,
     &        LON_UI, ROT_UI, GRID_UI, PLOTTYPE, ORDINATE, ABSCISSA,
     &        TOPOSCALE_UI, VINT, STREAMLINE, MAPROJ_UI,
     &        MAPCLIP_UI, DEFAULT_MAP_UI, VLOCK_UI,
     &        LOC, LOOP_UI, COLORTAB_UI, UI_VECT_COMP, UI_VMAX,
     &        TICLABELS, TICSCALE, TICDECIMALS, TICWIDTH, VPOS,
     &	      FILLPATTERN, TERR_UI, TERR_NUNLAB_UI, SKEWT_COMP_UI,
     &	      POINTS_UI, LANDSCAPE_UI, UI_TRAJ_COMP, UI_TRAJ_ENDS,
     &        TRAJ_IND_UI, VSCALE_UI, TRAJ_STEP_VAL_UI, 
     &	      TRAJ_BACKGROUND_UI

c
      parameter( DIR_UI       =   0,
     &           BUTTONS_UI   =   1,	
     &           CINT_UI      =   2,
     &           TCINT_UI     =   3,
     &           NUNLAB_UI    =   4,
     &           NOZERO_UI    =   5,
     &           LABELPLOT_UI =   6,
     &           LABELLINE_UI =   7,
     &           SINGLEVAL_UI =   8,
     &           FILLTYPE_UI  =   9,
     &           AXES_UI      =   10,
     &           SAVE_UI      =   11)

      parameter( XTICS_UI     =   12,
     &           YTICS_UI     =   13,
     &           WINDOW_UI    =   14,
     &           TRANS_UI     =   15,
     &           CTB_UI       =   16,
     &           DRMAP_UI     =   17,
     &           MAPOUT_UI    =   18,
     &           LAT_UI       =   19,
     &           LON_UI       =   20,
     &           ROT_UI       =   21,
     &           GRID_UI      =   22,
     &           PLOTTYPE     =   23)

      parameter(  ORDINATE     = 24,
     &            ABSCISSA     = 25,
     &            TOPOSCALE_UI = 26,
     &	          DEFAULT_MAP_UI = 27,
     &		  VLOCK_UI     = 28,
     &            VINT         = 29,
     &            STREAMLINE   = 30,
     &            LOC          = 31,
     &            LOOP_UI      = 32,
     &	          MAPCLIP_UI   = 33,
     &		  COLORTAB_UI  = 34,
     &            UI_VECT_COMP = 35,
     &            UI_VMAX      = 36,
     &            MAPROJ_UI    = 37)

      parameter(  TICLABELS    = 38,
     &            TICSCALE     = 39,
     &            TICDECIMALS  = 40,
     &            TICWIDTH     = 41,
     &		  VPOS	       = 42,
     &            FILLPATTERN  = 43,
     &		  TERR_UI      = 44,
     &		  TERR_NUNLAB_UI=45,
     &		  SKEWT_COMP_UI =46,
     &		  POINTS_UI 	=47,
     &		  LANDSCAPE_UI  =48,
     &		  UI_TRAJ_COMP 	=49,
     &		  UI_TRAJ_ENDS 	=50)

      parameter(  TRAJ_IND_UI    =51,
     &	          VSCALE_UI     =52,
     &		  TRAJ_STEP_VAL_UI = 53,
     &            TRAJ_BACKGROUND_UI =54)


