 /************************************************************************
  * $Id: in_commands.h,v 1.2 1994/06/28 17:42:53 warren Exp $
  * Purpose:
  * This file is used for the array which contains a parsed command
  * line. 
  * History:
  * $Log: in_commands.h,v $
  * Revision 1.2  1994/06/28 17:42:53  warren
  * First cut at fancy tic marks.
  *
 * Revision 1.1.1.1  1993/04/07  19:41:14  harry
 * Initial CVS checkin of IVE.
 *
  * Revision 2.1  1992/08/19  15:41:37  warren
  * increased size of MAXCOM to 13
  * 
  * Revision 2.0  1992/07/01  21:15:50  millerp
  * Released version rereving to 2.0
  * 
  * Revision 1.1  1992/04/21  00:11:26  millerp
  * Initial revision
  * 
  * 
  *************************************************************************/
      static int cmdlen, fstval=2;
      extern struct {
	  char comand[13*80];
	  /* fortran 13 array of 80 char*/
      }prscmd_;

