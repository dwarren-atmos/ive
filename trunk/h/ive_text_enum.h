/*
 * $Id: ive_text_enum.h,v 1.15 2002/08/09 19:57:44 warren Exp $
 * Header file for text boxes.
 *
 * $Log: ive_text_enum.h,v $
 * Revision 1.15  2002/08/09 19:57:44  warren
 * Fixed for Linux and tested with DEC sun etc.
 * most items inside #ifdef's, or were general cleanup/fixes
 *
 * Revision 1.14  2002/05/22 17:56:30  davidive
 * First working version of trajectories colored by alternative fields
 * There is not a complete interface yet!!!
 *
 * Revision 1.13  2001/11/15 21:37:20  davidive
 * added variables and widgets for mid val, step size and color mid for
 * trajectories
 *
 * Revision 1.12  1997/10/22 19:34:55  davidive
 * Turned the particle movement part of trajectories into a user transform
 * and added unit checking to the default transform.
 *
 * Revision 1.11  1997/06/03 19:14:03  davidive
 * The big mouse slicing changeover. We have the new form, only requires you
 * set 2 points (eg x1,y1) (x2,y2) and window or zloc and tloc, and now
 * always runs through init_points_ to catch all these things.
 * note: only actually works when two dimentions have points set. Needs to
 * be expanded into 1, 3 or 4.
 *
 * Revision 1.10  1996/10/11 01:12:25  warren
 * To fill or not to fill is no longer the only question. Now there is a seperate
 * question of solid or pattern. Also first part of mouse slicing. 2d -> 1d
 * Labels are not fixed yet.
 *
 * Revision 1.9  1996/10/09 18:54:20  harry
 * Add the ability to loop on file names if time value is part of name,
 * and there is only one time per file.
 *
 * Revision 1.8  1996/04/25 16:24:44  harry
 * Add Skew-T plots and the beginning of trajectory plots.
 *
 * Revision 1.7  1996/04/01 20:23:22  warren
 * Terrain form and new terrain command (on / off)
 *
 * Revision 1.6  1995/12/15 22:32:59  warren
 * moved patterns into 4.0 and implemented the list of fixes and changes from
 * Curtis.
 *
 * Revision 1.5  1994/10/08  19:00:19  warren
 * The annotation widget and updating it.
 *
 * Revision 1.4  1993/11/03  23:41:05  warren
 * Cleanup for port to alpha
 *
 * Revision 1.3  1993/09/08  23:14:47  warren
 * Print widgets and print functions.
 *
 * Revision 1.2  1993/05/20  17:31:26  warren
 * Additions and changes to make the new forms active.
 *
 * Revision 1.1.1.1  1993/04/07  19:41:12  harry
 * Initial CVS checkin of IVE.
 *
 * Revision 1.5  1992/12/23  18:56:15  harry
 * Change values of MAX_*_BOXES.
 *
 * Revision 1.4  1992/11/20  16:33:21  harry
 * Change define names CINT, TCINT and TOPOSCALE to eliminate name
 * conflicts with ive_widgets.h.
 *
 */

#define CONTOUR_TEXT_BOXES  1
#define MAP_TEXT_BOXES  2
#define VECTOR_TEXT_BOXES  3
#define LINE_TEXT_BOXES  4
#define LOOP_TEXT_BOXES  5
#define TWO_TEXT_BOXES  6
#define FOUR_TEXT_BOXES  7
#define SIX_TEXT_BOXES  8
#define ANNOTATION_TEXT_BOXES 9
#define STIPPLE_TEXT_BOXES 0
#define TERRAIN_TEXT_BOXES -1
#define SKEWT_TEXT_BOXES -2
#define POINT_TEXT_BOXES -3
#define TRAJ_TEXT_BOXES -4
#define COLOR_TEXT_BOXES -5
#define ThreeD_TEXT_BOXES -6
#define ThreeD_CONTROL_TEXT_BOXES -7

#define MAX_CONTOUR_BOXES  5
#define MAX_MAP_BOXES  4
#define MAX_VECTOR_BOXES  7
#define MAX_LINE_BOXES  8 
#define MAX_LOOP_BOXES  3
#define MAX_TWO_BOXES  2
#define MAX_FOUR_BOXES  4
#define MAX_SIX_BOXES  6
#define MAX_ANNOTATION_BOXES 2
#define MAX_STIPPLE_BOXES 10
#define MAX_TERRAIN_BOXES 1
#define MAX_SKEWT_BOXES 3
#define MAX_POINT_BOXES 4
#define MAX_TRAJ_BOXES 6
#define MAX_COLOR_BOXES 1
#define MAX_ThreeD_BOXES 1
#define MAX_ThreeD_CONTROL_BOXES 2

#define CVALS  100
#define XMAJOR  101
#define XMINOR  102
#define YMAJOR  103
#define YMINOR  104
#define TICOFFSET 105

#define PLAT  200
#define PLON  201
#define ROTA  202
#define GLSPACE  203

#define VCOMP_1  300
#define VCOMP_2  301
#define VMAX  302
#define XMAJORV  303
#define XMINORV  304
#define YMAJORV  305
#define YMINORV  306

#define DMIN  400
#define DMAX  401
#define DMAJOR  402
#define DMINOR  403
#define IMIN  404
#define IMAX  405
#define IMAJOR  406
#define IMINOR  407

#define INCR_WIDGET  500
#define START_T_WIDGET 501
#define ENDV_T_WIDGET 502

#define T1  600
#define T2  601

#define F1  700
#define F2  701
#define F3  703
#define F4  704

#define S1  800
#define S2  801
#define S3  803
#define S4  804
#define S5  805
#define S6  806

#define TSCALE 900
#define TMOTION 901

#define STIPSTART1 0
#define STIPEND1   1
#define STIPSTART2 2
#define STIPEND2   3
#define STIPSTART3 4
#define STIPEND3   5
#define STIPSTART4 6
#define STIPEND4   7
#define STIPSTART5 8
#define STIPEND5   9

#define TERRCONT -100

#define SCOMP_1  -200
#define SCOMP_2  -201
#define SCOMP_3  -202

#define POINT_X1  -300
#define POINT_X2  -301
#define POINT_Y1  -302
#define POINT_Y2  -303

#define TCOMP_1 -400
#define TCOMP_2 -401
#define TCOMP_3 -402
#define TRAJ_VAL -403
#define TRAJ_STEP -404
#define TRAJ_BACKGROUND -405

#define COLOR_SHIFT -500

#define ThreeVALS -600

#ifdef use_text_arrays
int contour_text_boxes[MAX_CONTOUR_BOXES];
  /*
    cint
    tcint
    xmajor
    xminor
    ymajor
    yminor
    */
int map_text_boxes[MAX_MAP_BOXES];
  /*
    plat
    plon
    rota
    glspace
    */
int vector_text_boxes[MAX_VECTOR_BOXES];
  /*
    vint_x
    vint_y
    xmajorv
    xminorv
    ymajorv
    yminorv
    */
int skewt_text_boxes[MAX_SKEWT_BOXES];
  /*
     skewt_p
     skewt_t
     skewt_dpt
    */
int line_text_boxes[MAX_LINE_BOXES];
  /*
    xmajorl
    xminorl
    ymajorl
    yminorl
    abscissa_min 
    abscissa_max
    ord_min
    ord_max
    toposcale
    */
int loop_text_boxes[MAX_LOOP_BOXES];
  /*
    incr_widget
    start_widget
    */
int two_text_boxes[MAX_TWO_BOXES];
int four_text_boxes[MAX_FOUR_BOXES];
int six_text_boxes[MAX_SIX_BOXES];
int annotation_text_boxes[MAX_ANNOTATION_BOXES];
  /*
    scale_widget
    decimals_widget
    */

int stipple_text_boxes[MAX_STIPPLE_BOXES];
int terrain_text_boxes[MAX_TERRAIN_BOXES];
   /*
      labbutton
  */
int point_text_boxes[MAX_POINT_BOXES];
/*
  x1lab
  y1lab
  y2lab
  x2lab
  */
int traj_text_boxes[MAX_TRAJ_BOXES];
/*
  tcomp1
  tcomp2
  tcomp2
  traj_val
  traj_step
  traj_background
  */
int color_text_boxes[MAX_COLOR_BOXES];
/*
  color_shift
*/
int threed_text_boxes[MAX_ThreeD_BOXES];
int threed_control_text_boxes[MAX_ThreeD_CONTROL_BOXES];
/*
  Surface values
*/

#else
extern int contour_text_boxes[MAX_CONTOUR_BOXES];
extern int map_text_boxes[MAX_MAP_BOXES];
extern int vector_text_boxes[MAX_VECTOR_BOXES];
extern int line_text_boxes[MAX_LINE_BOXES];
extern int loop_text_boxes[MAX_LOOP_BOXES];
extern int two_text_boxes[MAX_TWO_BOXES];
extern int four_text_boxes[MAX_FOUR_BOXES];
extern int six_text_boxes[MAX_SIX_BOXES];
extern int annotation_text_boxes[MAX_ANNOTATION_BOXES];
extern int stipple_text_boxes[MAX_STIPPLE_BOXES];
extern int terrain_text_boxes[MAX_TERRAIN_BOXES];
extern int point_text_boxes[MAX_POINT_BOXES];
extern int traj_text_boxes[MAX_TRAJ_BOXES];
extern int color_text_boxes[MAX_COLOR_BOXES];
extern int threed_text_boxes[MAX_ThreeD_BOXES];
extern int threed_control_text_boxes[MAX_ThreeD_CONTROL_BOXES];
void cleanup_box_call();
void cleanup_form_call();
void text_box_motion();
void swap_fg();
#endif

#define SOLID 1
#define PATTERN 2
#define NEVER 1
#define EXCEPT_OVERLAY 2
#define ALWAYS 3
