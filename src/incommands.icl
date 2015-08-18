!-----------------------------------------------------------------------
!     $Id: incommands.icl,v 1.4 1996/04/01 20:23:41 warren Exp $
!     Purpose:
!        This file is used for the array which contains a parsed command
!        line. 
!     History:
!	$Log: incommands.icl,v $
!	Revision 1.4  1996/04/01 20:23:41  warren
!	Terrain form and new terrain command (on / off)
!	
!Revision 1.3  1993/07/28  17:14:43  warren
!mad MAXCMDS 41 so you can have 40 contour values.
!
!Revision 1.2  1993/06/23  18:04:13  warren
!Increased line length to 256 from 80 per command so you can have long
!file names.
!
!Revision 1.1.1.1  1993/04/07  19:40:02  harry
!Initial CVS checkin of IVE.
!
!Revision 2.1  1992/08/19  15:41:37  warren
!increased size of MAXCOM to 13
!
!Revision 2.0  1992/07/01  21:15:50  millerp
!Released version rereving to 2.0
!
!Revision 1.1  1992/04/21  00:11:26  millerp
!Initial revision
!
!
!-----------------------------------------------------------------------
      integer cmdlen, fstval, maxcom
      parameter (MAXCOM = 41, CMDLEN = 512, FSTVAL = 2)
      character *(CMDLEN) comand(MAXCOM) 
      common /prscmd/ comand
!
!     common prscmd contains an array whose structure is,
!       comand(1)             some command.
!       comand(2 to maxcom)   the values associated with the command.
!