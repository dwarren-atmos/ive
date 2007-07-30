/*-----------------------------------------------------------------------*
 *$Id: ive_color.h,v 1.3 2002/12/09 18:03:48 warren Exp $
 *
 *Purpose:
 *   This file contains all common block which carries global
 *   variables related to color options.
 *
 *History:
 *$Log: ive_color.h,v $
 *Revision 1.3  2002/12/09 18:03:48  warren
 *add int to record true color(T/F).
 *
 *Revision 1.2  2001/11/15 21:37:20  davidive
 *added variables and widgets for mid val, step size and color mid for
 *trajectories
 *
 *Revision 1.1  2001/11/02 18:49:59  davidive
 *added this so that the C routines can access the fortran common blocks
 *
 *-----------------------------------------------------------------------*/
#define MXBANDS 300
#define MXLABS 301 /* MXBANDS+1 */


extern struct {
  char clrtab[80];
} color_;

extern struct {
  int hicolor[1], locolor[1];
} linecolors_;

extern struct {
  int hipattern[1], lopattern[1];
} linepatterns_;

extern struct {
  int  traj_entry[1];
  float traj_val[1], traj_step[1], traj_levels[40];
} traj_colors_;

extern struct {
  int lock_entry[1], entry_used[1], lock_set_cint[1];
  float midval[1], cont_int_lock_var[1], cont_used[1], lock_int[1];
  int ignore_lock_var[1], zero_color[1];
} conrec_colors_;

extern struct {
  char lock_field[80];
} conrec_colors_char_;

extern struct {
  int nbands[1], bandclr[MXBANDS];
} colorstuff_;

extern struct {
  char barlab[MXLABS][16];
} colorstuff_char_;

int IVE_TRUE_COLOR;

