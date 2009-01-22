c
c     $Id: traj.icl,v 1.11 2005/09/06 20:22:15 warren Exp $
c
c     Common block carrying the pointers to the three vector components.
c
c     $Log: traj.icl,v $
c     Revision 1.11  2005/09/06 20:22:15  warren
c     Move updates into current stream
c
c     Revision 1.10.2.1  2005/09/06 17:55:24  warren
c     fixed trajectories to deal with maps. Also first part of bold arrows for
c     trajectories
c
c     Revision 1.10  2002/05/22 17:56:47  davidive
c     First working version of trajectories colored by alternative fields
c     There is not a complete interface yet!!!
c
c     Revision 1.9  2001/11/20 23:29:57  davidive
c     Track if step and mid val are set by ive or a person. Also initialize all
c     variables for no depth.
c
c     Revision 1.8  2001/11/15 21:38:09  davidive
c     added control over the step size and color matching for trajectories, as well
c     as color bar expansion on overlays
c
c     Revision 1.7  2001/11/02 18:51:19  davidive
c     converted rdctab to c, fixed trajectories so that their independant color
c     tables work, including setting center color, center value and interval.
c
c     Revision 1.6  1999/01/29 20:46:34  warren
c     ets the flag for time independence and the plot the trajectories
c
c     Revision 1.5  1998/12/31 16:01:45  warren
c     First cut of new trajectory look and feel. Trajectories are no longer
c     on the popup menu, just plot type.
c
c     Revision 1.4  1998/12/11 19:35:18  warren
c     Fixed the tradjectory plots to go forward to the end time as well as
c     backwards to the beginning (Don't check if returned time is in min to max
c     window any more).
c
c     Revision 1.3  1997/10/30 23:07:53  davidive
c     convert traj to c to dynamically allocate memory, also place trajectory
c     inside black lines one each side.
c
c     Revision 1.2  1997/10/22 19:35:10  davidive
c     Turned the particle movement part of trajectories into a user transform
c     and added unit checking to the default transform.
c
c     Revision 1.1  1996/04/25 16:25:36  harry
c     Add Skew-T plots and the beginning of trajectory plots.
c
c	ANY CHANGES HERE MUST ALSO BE MADE IN TRAJ.C !!!!!!!!!!!
c
      real start, trajend, trajbeg, ustag(4),  vstag(4), wstag(4)
      integer num_traj_comp
      logical time_ind, traj_2d
      dimension start(4)
      common /traj_start/ start
      common /traj_times/ trajend, trajbeg
      common /traj_stag/ ustag,  vstag, wstag

      character *(80)att_var_traj, traj_component, traj_units
      character *(80)traj_background_field
      character *(80)old_traj_background
      dimension traj_component(3), traj_units(3)
      common / traj_pointers /att_var_traj, traj_component, traj_units,
     &			      traj_background_field, old_traj_background
      common/ traj_count / num_traj_comp
      common / traj_time_ind/ time_ind
      common / traj_dim_2d/ traj_2d

      real traj_data_max, traj_data_min
      integer 	traj_depth
      common / traj_minmax/traj_data_max, traj_data_min, 
     &		traj_depth

      integer arrow_bold_interval
      common/traj_arrows/arrow_bold_interval

