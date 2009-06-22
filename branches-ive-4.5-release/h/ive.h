/*
 * $Id: ive.h,v 1.27 2006-10-05 16:40:07 reinecke Exp $
 *
 * $Log: ive.h,v $
 * Revision 1.27  2006-10-05 16:40:07  reinecke
 * adding grey lines choices and new color table option
 *
 * Revision 1.26  2002/08/09 19:57:43  warren
 * Fixed for Linux and tested with DEC sun etc.
 * most items inside #ifdef's, or were general cleanup/fixes
 *
 * Revision 1.25  2002/05/22 17:56:29  davidive
 * First working version of trajectories colored by alternative fields
 * There is not a complete interface yet!!!
 *
 * Revision 1.24  1999/02/04 00:28:14  harry
 * Add vector scaling.
 *
 * Revision 1.23  1999/01/29 20:45:55  warren
 * new stuff to deal with time independent trajectories
 *
 * Revision 1.22  1998/01/28 15:27:34  harry
 * Add fix_slicer_value.
 *
 * Revision 1.21  1997/10/22 19:34:55  davidive
 * Turned the particle movement part of trajectories into a user transform
 * and added unit checking to the default transform.
 *
 * Revision 1.20  1997/06/27 21:06:41  harry
 * Designed multiplot printing.
 *
 * Revision 1.19  1997/06/03 19:14:02  davidive
 * The big mouse slicing changeover. We have the new form, only requires you
 * set 2 points (eg x1,y1) (x2,y2) and window or zloc and tloc, and now
 * always runs through init_points_ to catch all these things.
 * note: only actually works when two dimentions have points set. Needs to
 * be expanded into 1, 3 or 4.
 *
 * Revision 1.18  1996/12/23 20:58:08  warren
 * Landscape printing option
 *
 * Revision 1.17  1996/08/09 19:28:19  harry
 * Add float_nozero.
 *
 * Revision 1.16  1996/04/25 16:24:43  harry
 * Add Skew-T plots and the beginning of trajectory plots.
 *
 * Revision 1.15  1996/04/23 18:17:11  warren
 * Changes for terrain
 *
 * Revision 1.14  1996/04/01 20:23:21  warren
 * Terrain form and new terrain command (on / off)
 *
 * Revision 1.13  1996/03/29 20:37:10  harry
 * Add new environment variable IVECTBLIB, which specifies the location
 * of the built-in color tables.
 *
 * Revision 1.12  1995/12/15  22:32:56  warren
 * moved patterns into 4.0 and implemented the list of fixes and changes from
 * Curtis.
 *
 * Revision 1.11  1995/10/19 22:10:33  harry
 * Add default_check_handler.
 *
 * Revision 1.10  1995/06/23  21:15:04  warren
 * First part of new ive help
 * Changed " to ' in fortran files
 * Fixed the label spacing in 1d plots
 * added frame_width command
 *
 * Revision 1.9  1994/11/22  22:44:47  harry
 * Add support for vector position widget.
 *
 * Revision 1.8  1994/05/18  17:54:05  harry
 * Change swap_colors to swap_colors_callback.
 *
 * Revision 1.7  1993/12/15  18:00:57  harry
 * Changes to comments.
 *
 */

/*parent widget of the window inter face; must be global so fortran doesn't
  need to know about widgets
*/
#define DEFCOLORTAB "readcolortable=%s/default.ctb"
#define GREYCOLORTAB "readcolortable=%s/greyscale.ctb"
#define DLCOLORTAB  "readcolortable=%s/dark_light.ctb"
#define LDCOLORTAB  "readcolortable=%s/light_dark.ctb"
#define BRCOLORTAB  "readcolortable=%s/blue_red.ctb"
#define GBRCOLORTAB "readcolortable=%s/green_blue_red.ctb"
#define MBRCOLORTAB "readcolortable=%s/movie_blue_red.ctb"
#define RBWCOLORTAB "readcolortable=%s/rainbow.ctb"
#define WRCOLORTAB "readcolortable=%s/wtr.ctb"
extern Widget Box;

/*callbacks and window functions defined in ive_callbacks.c*/
void show_widget();
void quit_ive();
void done_callback();
void done_parent_callback();
void done_pparent_callback();
void del_callback();
void del_parent_callback();
void make_help_widget_(/* char * s*/);
void make_info_widget_(/* char * s*/);
void mixer_callback();
void swap_colors_callback();
void check_default_handler();
void check_help_handler();
void check_help_call();
void command_callback();
void command_callback_check();
void DoComHist();
void make_field_buttons_(/* Widget w,char * str*/);
#define make_field_buttons(a,b) make_field_buttons_(a,b);
void call_driver();
void call_driver_scv();
void window_callback();
void min_window_check_callback();
void max_window_check_callback();
void save_callback();
void domain_units_call();
void horizontal_vertical();
void im_gone_callback();
void set_loc_callback();
void not_implemented();
void fix_var();
void dep_var();
void fix_contour_button();
void color_bar_callback();
void window_scale_type_in();
void ive_beg_tear_call();
void ive_end_tear_call();
/*callbacks defined in forms_callbacks.c*/
void form_cint_call();
void form_tcint_call();
void form_label_call();
void form_line_width_call();
void form_nunlablines_call();
void form_filltype_call();
void form_single_call();
void form_linelabel_call();
void form_zero_call();
void form_spval_call();
void form_tick_call();
void form_drawmap_call();
void form_mapoutlines_call();
void form_defaultmap_call();
void form_plat_call();
void form_plon_call();
void form_rota_call();
void form_glspace_call();
void check_num();
void fix_option();
void form_axes_scale_call();
void form_sound_call();
void form_vint_call();
void form_stream_call();
void form_avg_call();
void form_linlog();
void form_vcomp_call();
void form_scomp_call();
void form_tcomp_call();
void form_vect_max_call();
void form_vect_lock_call();
void form_vect_pos_call();
void form_vect_scale_call();
void form_color_lines_call();
void form_landscape_callback();
void form_printer_call();
void form_terrain_call();
void form_tcint_call();
void form_numterr_call();
void numterr_scale_type_in();
void form_traj_call();
void form_traj_ind_call();
void form_traj_2d_call();
void traj_scale_type_in();
#ifdef Linux
typedef char *caddr_t;
#endif
/*entry routine into the color mixer*/
void  gks_color_mixer(/*Widget parent,int min,int max,Bool *RGB*/);
char *float_nozero();
/* point slicer routines */
void point_min_window_check_callback(Widget w, caddr_t data, 
				     XmAnyCallbackStruct call);
void point_max_window_check_callback(Widget w, caddr_t data, 
				     XmAnyCallbackStruct call);
void point_window_callback(Widget w, caddr_t data, XmAnyCallbackStruct call);
float fix_slicer_value(int slicer, float value);
char *ive_get_display_units();
char *ive_get_units();
