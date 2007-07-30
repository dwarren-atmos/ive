/*
 * $Id: properties.h,v 1.10 2002/08/09 19:57:44 warren Exp $
 *
 * $Log: properties.h,v $
 * Revision 1.10  2002/08/09 19:57:44  warren
 * Fixed for Linux and tested with DEC sun etc.
 * most items inside #ifdef's, or were general cleanup/fixes
 *
 * Revision 1.9  2002/05/22 17:56:30  davidive
 * First working version of trajectories colored by alternative fields
 * There is not a complete interface yet!!!
 *
 * Revision 1.8  2001/11/15 21:37:21  davidive
 * added variables and widgets for mid val, step size and color mid for
 * trajectories
 *
 * Revision 1.7  2001/08/15 22:24:15  davidive
 * Added radio_1 radio_2 and radio_3 and a rowcolumn widget to the forms structure
 *
 * Revision 1.6  1999/02/04 00:28:16  harry
 * Add vector scaling.
 *
 * Revision 1.5  1999/01/29 20:45:57  warren
 * new stuff to deal with time independent trajectories
 *
 * Revision 1.4  1997/10/22 19:34:56  davidive
 * Turned the particle movement part of trajectories into a user transform
 * and added unit checking to the default transform.
 *
 * Revision 1.3  1996/12/23 20:58:08  warren
 * Landscape printing option
 *
 * Revision 1.2  1996/10/11 01:12:26  warren
 * To fill or not to fill is no longer the only question. Now there is a seperate
 * question of solid or pattern. Also first part of mouse slicing. 2d -> 1d
 * Labels are not fixed yet.
 *
 * Revision 1.1  1996/04/25 16:24:46  harry
 * Add Skew-T plots and the beginning of trajectory plots.
 *
 */

extern struct {
/* dependant var */
  Widget form;
  Widget dep_form_c,ind_form_c;
  Widget dep_form_l, ind_form_l;
  Widget dep_form_v,ind_form_v;
  Widget dep_form_s,ind_form_s;
  Widget dep_form_t,ind_form_t;
  Widget cont_int, cont_val, clab1, clab2, cont_row;       /*contour*/
  Widget cont_values;                                      /*contour*/
  Widget zero, zerolab;                                    /*contour*/
  Widget dmin, dmax, varlab, dminlab, dmaxlab;             /*line*/
  Widget dmajor, dminor, dmajorlab, dminorlab, dmarklab;   /*line*/
  Widget daxeslin, daxeslog,daxesrow,daxeslab;             /*line*/
  Widget Vect_c_lab, vcomp_1, vcomp_2;                     /*vector*/
  Widget vcomp_1_lab, vcomp_2_lab;                         /*vector*/
  Widget Tcomp_c_lab, tcomp_1_lab, tcomp_2_lab, tcomp_3_lab; /*trajectory*/
  Widget tcomp_1, tcomp_2, tcomp_3, Traj_c_lab, Wind_lab;    /*trajectory*/
  Widget traj_ts_lab, traj_ts, traj_te_lab, traj_te;        /*trajectory*/
  Widget traj_ind_lab, traj_ind;                              /*trajectory*/
  Widget traj_expl_lab, traj_help;                           /*trajectory*/
  /* per plot */
  Widget vect_max, vect_lock, vect_maxlab, vect_locklab;   /*vector*/
  Widget vect_poslab, vect_pos;			     /*vector*/
  Widget vect_strlab, vect_strrow, vect_str, vect_strv;    /*vector*/
  Widget vect_scalelab, vect_scale;                        /*vector*/
  Widget fill, freq, filllab, numlines, linelab1, linelab2;      /*contour*/
  /* Independant vars */ 
  Widget xmajor,xminor,ymajor,yminor;                      /*contour*/
  Widget xmajorlab,xminorlab,ymajorlab, ivaxesrow;         /*contour*/
  Widget yminorlab, ihaxeslog, ivaxeslog, ihaxesrow;       /*contour*/
  Widget ihaxeslin, ivaxeslin, ihaxeslab, ivaxeslab;       /*contour*/
  Widget laxeslin, laxeslog, laxesrow, laxeslab;           /*line*/
  Widget imin, imax, iminlab, imaxlab,lvarlab;             /*line*/
  Widget imajor,iminor, imajorlab,iminorlab;               /*line*/
  Widget direction_radio_box, radio_1, radio_2, radio_3;   /*line*/
  Widget skewt_c_lab, scomp_1, scomp_2, scomp_3;	     /*skewt*/
  Widget scomp_1_lab, scomp_2_lab, scomp_3_lab;            /*skewt*/
  Widget vintx, vinty, vintxlab, vintylab;                 /*vector*/
  Widget vintxlab2, vintylab2;                             /*vector*/
  Widget xmajorv,xminorv,ymajorv,yminorv;                  /*vect*/
  Widget xmajorlabv,xminorlabv,ymajorlabv, ivaxesrowv;     /*vect*/
  Widget yminorlabv, ihaxeslogv, ivaxeslogv, ihaxesrowv;   /*vect*/
  Widget ihaxeslinv, ivaxeslinv, ihaxeslabv, ivaxeslabv;   /*vect*/
  Widget vect_type, vect_type_menu, Free_Max, Vector_lockmax,Vector_lockscale ;
  Widget xmajort,xminort,ymajort,yminort;                  /*traj*/
  Widget xmajorlabt,xminorlabt,ymajorlabt, yminorlabt;     /*traj*/
  Widget ihaxeslogt, ivaxeslogt;                           /*traj*/
  Widget ihaxeslabt, ivaxeslabt;                           /*traj*/
  Widget traj_steplab, traj_step, traj_vallab, traj_val;   /*traj*/
  Widget traj_backgroundlab, traj_background;             /*traj*/
  Widget color_form, lock_color_widget, color_shift;         /*color*/
  Widget color_reset, color_bar, color_toggle;               /*color*/
}Properties;

extern struct {
    Widget form;
    Widget printer_name;
    Widget color_lab;
    Widget color;
    Widget landscape_lab;
    Widget landscape;
    char current[256];
}printer_setup;
