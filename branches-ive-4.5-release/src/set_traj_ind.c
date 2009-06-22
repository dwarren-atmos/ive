#include <strings.h>
#ifdef MEMDBG
#include <mnemosyne.h>
#endif
#include <string.h>
#include <stdio.h>
#include <in_commands.h>

extern void make_help_widget_(),setlvar_();
/*
 *Turn on or off time independant trajectories
 */

static int chk_traj_log(char *line){
  if(!strncasecmp(line,"NO",2))
    return(0);
  if(!strncasecmp(line, "YES",3))
    return(1);
  if(!strncasecmp(line,"ON",2))
    return(0);
  if(!strncasecmp(line, "OFF",3))
    return(1);
  return(-1);
}

void set_traj_time_ind_(int *error){
  char *ptr, **argv, **get_args();
  int argc, traj_ind;

  *error=0;
   argv = get_args(&argc);
   if (argc != 2 ||((traj_ind = chk_traj_log(argv[1]))<0)){
     (void)make_help_widget_("traj_ind requires either yes or no");
     *error=1;
     return;
   }

   (void)setlvar_("time_ind", &traj_ind, error, 8);
   if(*error){
     (void)make_help_widget_("An error occured while setting the variable traj_ind\nplease report this to the Ive maintenance group.");
   }
}

void set_traj_2d_(int *error){
  char *ptr, **argv, **get_args();
  int argc, traj_2d;

  *error=0;
   argv = get_args(&argc);
   if (argc != 2 ||((traj_2d = chk_traj_log(argv[1]))<0)){
     (void)make_help_widget_("traj_ind requires either on or off");
     *error=1;
     return;
   }

   (void)setlvar_("traj_2d", &traj_2d, error, 8);
   if(*error){
     (void)make_help_widget_("An error occured while setting the variable traj_ind\nplease report this to the Ive maintenance group.");
   }
}
