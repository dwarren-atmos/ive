/*
 * $Id: ive_widgets.h,v 1.37 2006-10-05 16:40:43 reinecke Exp $
 *
 * $Log: ive_widgets.h,v $
 * Revision 1.37  2006-10-05 16:40:43  reinecke
 * adding grey line option and new white to red color table menu choice
 *
 * Revision 1.36  2002/06/14 19:43:59  harryive
 * Allow specification of terrain contours by value.
 *
 * Revision 1.35  2002/05/22 17:56:30  davidive
 * First working version of trajectories colored by alternative fields
 * There is not a complete interface yet!!!
 *
 * Revision 1.34  2002/03/11 20:14:04  davidive
 * Fixed ui_pudate to update trajectory step and value.
 *
 * Revision 1.33  1999/02/04 00:28:15  harry
 * Add vector scaling.
 *
 * Revision 1.32  1999/01/29 20:45:56  warren
 * new stuff to deal with time independent trajectories
 *
 * Revision 1.31  1998/12/31 16:01:27  warren
 * First cut of new trajectory look and feel. Trajectories are no longer
 * on the popup menu, just plot type.
 *
 * Revision 1.30  1998/03/04 19:13:49  davidive
 * First cut at trajectories and trajectory widget using colors to show depth
 * and arrows to show direction.
 *
 * Revision 1.29  1997/06/27 21:06:42  harry
 * Designed multiplot printing.
 *
 * Revision 1.28  1997/06/03 19:14:03  davidive
 * The big mouse slicing changeover. We have the new form, only requires you
 * set 2 points (eg x1,y1) (x2,y2) and window or zloc and tloc, and now
 * always runs through init_points_ to catch all these things.
 * note: only actually works when two dimentions have points set. Needs to
 * be expanded into 1, 3 or 4.
 *
 * Revision 1.27  1997/04/30 18:46:15  harry
 * Add widget to write out color table.
 *
 * Revision 1.26  1997/03/12 18:57:28  harry
 * Add menu for map outlines.
 *
 * Revision 1.25  1996/11/07 16:54:57  warren
 * New widget for point slicing
 *
 * Revision 1.24  1996/04/25 16:24:45  harry
 * Add Skew-T plots and the beginning of trajectory plots.
 *
 * Revision 1.23  1996/04/23 18:17:13  warren
 * Changes for terrain
 *
 * Revision 1.22  1996/04/01 20:23:23  warren
 * Terrain form and new terrain command (on / off)
 *
 * Revision 1.21  1995/12/15 22:33:00  warren
 * moved patterns into 4.0 and implemented the list of fixes and changes from
 * Curtis.
 *
 * Revision 1.20  1995/06/23  21:15:06  warren
 * First part of new ive help
 * Changed " to ' in fortran files
 * Fixed the label spacing in 1d plots
 * added frame_width command
 *
 * Revision 1.19  1995/05/26  15:20:37  harry
 * Add SAVE_WINDOW command and widget.
 *
 * Revision 1.18  1994/11/22  22:44:49  harry
 * Add support for vector position widget.
 *
 * Revision 1.17  1994/10/08  19:00:21  warren
 * The annotation widget and updating it.
 *
 * Revision 1.16  1994/09/14  18:19:31  harry
 * Big 1D flip uncommit.
 *
 * Revision 1.15  1994/04/26  18:06:24  warren
 * Beginning of mouse slice interface (doesn't really do anything yet except
 * mark up your plot)
 *
 * Revision 1.14  1994/04/19  23:02:25  yuri
 * Big 1D flip commit.
 *
 * Revision 1.13  1994/04/14  23:30:41  harry
 * Add VLOCK_UI.
 *
 * Revision 1.12  1994/01/20  20:43:55  yuri
 * Propagating of "overlay_patterns" stuff to newer version.
 *
 * Revision 1.11  1994/01/04  17:47:42  warren
 * updated the main_widget to include all the widgets. The by product is that
 * ui_update works again for plot type.
 *
 * Revision 1.10  1993/12/15  18:00:30  harry
 * Changes to comments.
 *
 */

/*general*/
extern Widget linepattern, axes;
extern Widget linlin,linlog,loglin,loglog;
extern Widget line_solid,dashdot,dash,dot;
extern Widget Dir_list,Trans_list,Ctb_list,WCtb_list;
extern WidgetList buttons;
extern Widget fill_menu,axes_menu;
extern Widget loop_setup_form;
extern struct{
    Widget info_2d, mins[4], maxs[4], fix[4];
  }slicer_widget;
extern Widget color_form, lock_color_widget;
/*map stuff*/

extern struct{
    Widget plot,overlay,lift_overlay;
    Widget input_menu, input,load_file,load_trans,load_input;
    Widget output_menu, output,dump_win,print,eps,pdf;
    Widget print_current, print_setup, print_menu;
    Widget print_twoper, print_fourper, print_sixper;
    Widget set_log, save_win, quit;
    Widget type_menu, type, Scalar, Vector, Trajectory, Skewt, Line;
    Widget color_menu, cotable_sel, color, readtab, fore_to_back;
    Widget Surface, Scatter, Wireframe;
    Widget mixer, mixerrgb, mixerhsl, mixer_sel,show_color;
    Widget default_table, DL_table, custom_table;
    Widget LD_table, BR_table, GBR_table, MBR_table, RBW_table, WR_table;
    Widget color_opts, write_table;
    Widget loop,help;
    Widget menu,row1,row2;
    Widget field_scroll,field_holder,command;
    Widget plot_menu, plot_att;
    Widget background_menu, background, map, terrain, annotation;
    Widget line_att, line_menu, linecolor_menu;
    Widget linecolorhi_menu, linecolorlo_menu, linecolorbo_menu;
    Widget colorsel, chi, clo, cbo, cdef;
    Widget colorb, red, green, blue, grey; 
    Widget black, white, brown, violet, orange, yellow;
    Widget colorhi,redhi,greenhi,bluehi, greyhi, yellowhi;
    Widget blackhi,whitehi,brownhi,violethi,orangehi;
    Widget colorlo,redlo,greenlo,bluelo,greylo;
    Widget blacklo,whitelo,brownlo,violetlo,orangelo, yellowlo;
    Widget linepattern, patternsel, patternb, patternhi, patternlo, patdef;
    Widget patb, line_solid, dash, dot, dashdot, 
           dashdotdot, longdash_short,longdash;
    Widget pathi, line_solidhi, dashhi, dothi, dashdothi,
           dashdotdothi, longdash_shorthi,longdashhi;
    Widget patlo, line_solidlo, dashlo, dotlo, dashdotlo,
           dashdotdotlo, longdash_shortlo,longdashlo;

    Widget linewidth, widthsel, one, two, three;
} main_widget;

extern struct{
    Widget map_form;
    Widget drawmap,outline,outline_menu,plat,plon,rota,glspace,proj;
    Widget out_no,out_co,out_us,out_ps,out_po,out_m3,out_m1;
    Widget proj_lc,proj_st,proj_or,proj_le,proj_gn,proj_ae,proj_sv;
    Widget proj_ce,proj_me,proj_mo,proj_nhst,proj_shst,proj_nhor,proj_shor;
    Widget proj_menu, labbutton, terbutton, defaultbutton;
    Widget clip, clip_menu, clip_window, clip_last;
}map_forms;

extern struct{
    Widget form;
    Widget two_1, two_2;
    Widget print;
}twoup;

extern struct{
    Widget form;
    Widget four_1, four_2, four_3, four_4;
    Widget print;
}fourup;

extern struct{
    Widget form;
    Widget six_1, six_2, six_3, six_4, six_5, six_6;
    Widget print;
}sixup;
extern struct {
    Widget form;
    Widget ticlab;
    Widget tic_labels, tblab;
    Widget tic_labels_off, tic_labels_norm,tic_labels_bold;
    Widget tic_scale, tslab;
    Widget tic_widthmaj, twlabmaj, tic_widthmin, twlabmin;
    Widget tic_widthmaj_thin, tic_widthmaj_thick, tic_widthmaj_thickest;
    Widget tic_widthmin_thin, tic_widthmin_thick, tic_widthmin_thickest;
    Widget tic_decimals, tdlab;
    Widget generallab, sep;
    Widget label;
}Annotation;
extern struct{
    Widget form, lablab, slab, elab;
    Widget label1, start1, end1;
    Widget label2, start2, end2;
    Widget label3, start3, end3;
    Widget label4, start4, end4;
    Widget label5, start5, end5;
    /*    Widget label6, start6, end6;*/
}stipple;

extern struct{
    Widget terrain_form;
    Widget tcint,labbutton, show_ter,clab1,clab2,cont_row;
    Widget linelab1,linelab2,numterr,cont_values,cont_int,cont_val;
}terrain_forms;

extern struct { 
  Widget point_form, p1x,p1y,p2x,p2y,
    x1lab,y1lab,x2lab,y2lab,boxx1,boxy1,boxx2,boxy2,
    leftlab, rightlab, secondlab, dim1lab, dim2lab,
    slide1l, slide1r, slidelab1, button1,
    slide2l, slide2r, slidelab2, button2;
}point_slicer;

extern struct
{
  Widget ThreeD;
  Widget TransLab;
  Widget TransLRt, TransUDt;
  Widget toggle[10];
  Widget moveU, moveD, moveL, moveR;
  Widget RotLab;
  Widget RotUD, RotLR;
  Widget ZoomLab, StrSlide;
  Widget reset,print;
  Widget scroll, Objs, UDCol, RLCol, RUDCol, RRLCol;
}controls_3D;
 
#define DIR         0
#define BUTTONS     1
#define CINT        2
#define TCINT       3
#define NUNLAB      4
#define NOZERO      5
#define LABELPLOT   6
#define LABELLINE   7
#define SINGLEVAL   8
#define FILLTYPE    9
#define AXES       10
#define SAVE       11
#define XTICS      12
#define YTICS      13
#define WINDOW     14
#define TRANS      15
#define CTB        16
#define DRMAP      17
#define MAPOUT     18
#define LAT        19
#define LON        20
#define ROT        21
#define GRID       22
#define PLOTTYPE   23
#define ORDINATE   24
#define ABSCISSA   25
#define TOPOSCALE  26
#define DEFAULT_MAP_UI 27
#define VLOCK_UI   28
#define VINT       29
#define STREAMLINE 30
#define LOC        31
#define LOOP       32
#define MAPCLIP_UI 33
#define COLORTAB   34
#define VECT_COMP  35
#define VMAX_UI    36
#define MAPROJ_UI  37
#define TICLABELS  38
#define TICSCALE   39
#define TICDECIMALS 40
#define TICWIDTH   41
#define TICOFFSET  42
#define VPOS       43
#define FILLPATTERN 44
#define TERR       45
#define TERR_NUNLAB 46
#define SKEWT_COMP 47
#define POINTS_UI  48
#define LANDSCAPE_UI 49
#define UI_TRAJ_COMP 50
#define UI_TRAJ_ENDS 51
#define TRAJ_IND_UI 52
#define VSCALE_UI  53
#define TRAJ_STEP_VAL_UI 54
#define TRAJ_BACKGROUND_UI 55
#define TRAJ_2D_UI 56
#define THREED_CONTROL_FORM 57
