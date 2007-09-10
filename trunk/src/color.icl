c-----------------------------------------------------------------------
c     $Id: color.icl,v 1.18 2005/09/06 20:50:22 reinecke Exp $
c     Purpose:
c        This file contains all common block which carries global
c        variables related to color options.
c
c     History:
c	$Log: color.icl,v $
c	Revision 1.18  2005/09/06 20:50:22  reinecke
c	change dimension of traj_levels from 80 to 160
c	
c	Revision 1.17  2002/08/09 19:57:56  warren
c	Fixed for Linux and tested with DEC sun etc.
c	most items inside #ifdef's, or were general cleanup/fixes
c	
c	Revision 1.16  2001/11/20 23:29:55  davidive
c	Track if step and mid val are set by ive or a person. Also initialize all
c	variables for no depth.
c	
c	Revision 1.15  2001/11/15 21:38:07  davidive
c	added control over the step size and color matching for trajectories, as well
c	as color bar expansion on overlays
c	
c	Revision 1.14  2001/01/23 21:42:00  davidive
c	First part of new zero centered colors.
c	added zero_color to color.icl and an option for setting it to the
c	call to read_color.
c
c	Revision 1.13  2000/05/09 19:53:41  harry
c	Seperate contour and line colors.  Make patterns work like colors.
c
c	Revision 1.12  1999/07/23 20:03:33  warren
c	Background code for the lock all fields option.
c
c	Revision 1.11  1998/03/04 19:13:56  davidive
c	First cut at trajectories and trajectory widget using colors to show depth
c	and arrows to show direction.
c
c	Revision 1.10  1997/12/31 23:31:16  warren
c	Fixed Dales color loxking problem, holton's crashes etc.
c
c	Revision 1.9  1997/07/29 17:31:48  davidive
c	Fix number of MXBANDS to equal max levels and run circularly through
c	the color table to fix the last color locking bug.
c
c	Revision 1.8  1997/06/06 20:17:53  harry
c	Fix color locking - distinguish value from contour interval.
c
c	Revision 1.7  1997/02/27 16:22:44  warren
c	put lock_int in common block where it belongs and changed maxval and minval
c	to ive_maxval and ive_minval to not interfere with the f90 intrinsic.
c	Also changed maxval in uwperim to ive_maxval_per to distinguish the
c	variable from the function.
c
cRevision 1.6  1997/01/29  23:09:58  warren
cCleaned up and fixed Dales Color problem
c
cRevision 1.5  1994/12/28  00:00:28  warren
cput entry_used into the common block it should have been in all the time and
cmove the setting of it out of the loop it was in.
c
cRevision 1.4  1994/09/07  22:07:02  warren
cremoved the extra plot from the color table locking sequence.
c
cRevision 1.3  1993/05/27  16:38:41  warren
cRecover files removed by yuri's commit
c
cRevision 1.1.1.1  1993/04/07  19:39:49  harry
cInitial CVS checkin of IVE.
c
cRevision 2.5  1993/01/13  23:15:27  millerp
cMove char variables to separate common block.
c
cRevision 2.4  1992/10/22  20:36:04  millerp
cAdded common block colorstuff.
c
cRevision 2.3  1992/10/15  23:01:21  warren
cadded color locking stuff
c
cRevision 2.2  1992/08/25  23:29:32  warren
cfixed color allocation problem and added fallback to smaller color table
c
cRevision 2.1  1992/08/24  20:32:42  millerp
cAdded MIN_USER_COLOR, MAX_USER_COLOR
c
cRevision 2.0  1992/07/01  21:15:34  millerp
cReleased version rereving to 2.0
c
cRevision 1.1  1992/04/21  00:11:26  millerp
cInitial revision
c
c-----------------------------------------------------------------------
c
c     Variables for using color.
c
      character *80 clrtab
      common / color / clrtab
c
c
      integer hicolor, locolor
      common / linecolors / hicolor, locolor
      integer hipattern, lopattern
      common / linepatterns / hipattern, lopattern
c
c
      integer min_user_color, max_user_color, min_traj_color,
     &        max_traj_color 
      common / user_colors/ min_user_color, max_user_color,
     &                      min_traj_color, max_traj_color
c     These are set by C when we know how many colors there are.      

C     These are for setting the trajectory depth colors
      logical traj_step_set, traj_val_set
      integer traj_entry, old_traj_min_color
      real traj_val, traj_step, traj_levels
      Dimension traj_levels(160)
      common / traj_colors / traj_step_set, traj_val_set,
     &  traj_entry, traj_val, traj_step, 
     &  traj_levels, old_traj_min_color
c
c     These are for locking the color table
      integer lock_entry, entry_used, lock_set_cint, ignore_lock_var,
     &        zero_color
      real    midval, cont_used, lock_int, cont_int_lock_var
      character *(80) lock_field
c
      common / conrec_colors / lock_entry, entry_used, lock_set_cint,
     &                         midval, cont_int_lock_var,
     &                         cont_used, lock_int, 
     &                         ignore_lock_var, zero_color
      common / conrec_colors_char / lock_field  
c
c
c     These are for color bar colors and labels.
c
      integer  MXBANDS, MXLABS
      parameter (MXBANDS = 300, MXLABS = MXBANDS+1)
      character *(16) barlab(MXLABS)
      integer nbands, bandclr(MXBANDS)
      common / colorstuff / nbands, bandclr
      common / colorstuff_char / barlab
c
