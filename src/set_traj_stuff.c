#ifdef MEMDBG
#include <mnemosyne.h>
#endif
#include <string.h>
#include <stdio.h>
#include <in_commands.h>

/*$Id: set_traj_stuff.c,v 1.4 2002/05/30 21:42:13 warren Exp $
 *set traj_val and traj_step
 *$History$
 */

void set_traj_val_(int *error){
  char *ptr, **argv, **get_args();
  int argc,zero = 0, one=1;
  double val, strtod();
  float traj_val;

  *error=0;
   argv = get_args(&argc);
   if (argc != 2 ||(((val = strtod(*(argv+1), &ptr)) == 0.0 &&
                      ptr == *(argv+1)))){
     if(argc == 2 && (*argv[1] == 'd' ||*argv[1] == 'D')){
       val = -1e19;
       (void)setlvar_("traj_val_set", &zero, error, 12);
       traj_val = (float)val;
       (void)setrvar_("traj_val", &traj_val, error, 8);
     }
     else{
       (void)make_help_widget_("traj_val requires a number");
       *error=1;
     }
     return;
   }
   traj_val = (float)val;
   (void)setrvar_("traj_val", &traj_val, error, 8);
   (void)setlvar_("traj_val_set", &one, error, 12);
   if(*error){
     (void)make_help_widget_("An error occured while setting the variable traj_val\nplease report this to the Ive maintenance group.");
   }
}

void set_traj_step_(int *error){
  char *ptr, **argv, **get_args();
  int argc, zero = 0, one = 1;
  double val, strtod();
  float traj_step;

  *error=0;
   argv = get_args(&argc);
   if (argc != 2 ||(((val = strtod(*(argv+1), &ptr)) == 0.0 &&
		     ptr == *(argv+1)))){
     if(argc == 2 && (*argv[1] == 'd' ||*argv[1] == 'D')){
       val = 0.0;
       (void)setlvar_("traj_step_set", &zero, error, 13);
       traj_step = (float)val;
       (void)setrvar_("traj_step", &traj_step, error, 9);
     }
     else{
       (void)make_help_widget_("traj_step requires a number");
       *error=1;
     }
     return;
   }
   traj_step = (float)val;
   (void)setlvar_("traj_step_set", &one, error, 13);
   (void)setrvar_("traj_step", &traj_step, error, 9);
   if(*error){
     (void)make_help_widget_("An error occured while setting the variable traj_step\nplease report this to the Ive maintenance group.");
   }
}

void set_traj_background_(int *error){
  char *ptr, **argv, **get_args();
  int argc, vid, zero = 0, one=1;
  double val, strtod();
  char traj_back[256];
  float traj_val, traj_step;
  
  *error=0;
   argv = get_args(&argc);
   if (argc == 2){
     if(!strncasecmp(argv[1],"default",7)){
       printf("Traj background is depth\n");
       (void)setavar_("traj_background_field","default",error,21,7);
	 if(*error){
	   (void)make_help_widget_("An error occured while setting the variable traj_background_field\nplease report this to the Ive maintenance group.");
	 }
	 else{
	   int fix[4];
	   (void)getiarr_("lock", fix, 4, error,4,4);
	   if(fix[0] == 0 && fix[1] == 0 && (fix[2] != 0 && fix[3] != 0))
	     vid = -3;
	   else if(fix[0] == 0 && fix[2] == 0 && (fix[1] != 0 && fix[3] != 0))
	     vid = -2;
	   else if(fix[1] == 0 && fix[2] == 0 && (fix[0] != 0 && fix[3] != 0))
	      vid = -1;
	   else return;
	   
	 }
	 get_traj_atts_(&vid, &traj_val, &traj_step);

	 (void)setrvar_("traj_val", &traj_val, error, 8);
	 if(traj_val > -9.999e18)
	   (void)setlvar_("traj_val_set", &one, error, 12);
	 else
	   (void)setlvar_("traj_val_set", &zero, error, 12);

	 (void)setrvar_("traj_step", &traj_step, error, 9);
	 if(traj_step != 0)
	   (void)setlvar_("traj_step_set", &one, error, 13);
	 else
	   (void)setlvar_("traj_step_set", &zero, error, 13);
	 return;
     }
   }
   else{
     (void)make_help_widget_("traj_background requires a field, or default");
     *error=1;
     return;
   }
   
   printf("Traj background is %s\n",argv[1]);
   (void)setavar_("traj_background_field",argv[1],error,21,strlen(argv[1]));
     if(*error){
       (void)make_help_widget_("An error occured while setting the variable traj_background_field\nplease report this to the Ive maintenance group.");
     }
     vid = getvid_(argv[1],strlen(argv[1]));
     get_traj_atts_(&vid, &traj_val, &traj_step);

     (void)setrvar_("traj_val", &traj_val, error, 8);
     if(traj_val > -9.999e18)
       (void)setlvar_("traj_val_set", &one, error, 12);
     else
       (void)setlvar_("traj_val_set", &zero, error, 12);

     (void)setrvar_("traj_step", &traj_step, error, 9);
     if(traj_step != 0)
       (void)setlvar_("traj_step_set", &one, error, 13);
     else
       (void)setlvar_("traj_step_set", &zero, error, 13);
}

