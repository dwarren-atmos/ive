#include <string.h>
#ifdef MEMDBG
#include <mnemosyne.h>
#else
#include <malloc.h>
#endif
#include <stdio.h>
#include <ive_macros.h>
static int not_window=0; /*flag passed to driver; means do update windows*/
static int zero=0, one=1;

extern void make_help_widget_(),run_sub_();

int
run_sub_trans_(iflag)

     int *iflag;

{
  char  buf[256], **argv, **get_args();
  int argc;
  argv = get_args(&argc);
  if (argc != 2) {
    sprintf(buf, "Command %s requires 1 value", argv[0]);
    (void) make_help_widget_(buf);
    return_args(1);
  }
  run_sub_(argv[1],strlen(argv[1]));
  return_args(0);
}

  
