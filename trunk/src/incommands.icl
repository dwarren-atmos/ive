c-----------------------------------------------------------------------
c     $Id: incommands.icl,v 1.4 1996/04/01 20:23:41 warren Exp $
c     Purpose:
c        This file is used for the array which contains a parsed command
c        line. 
c     History:
c	$Log: incommands.icl,v $
c	Revision 1.4  1996/04/01 20:23:41  warren
c	Terrain form and new terrain command (on / off)
c	
cRevision 1.3  1993/07/28  17:14:43  warren
cmad MAXCMDS 41 so you can have 40 contour values.
c
cRevision 1.2  1993/06/23  18:04:13  warren
cIncreased line length to 256 from 80 per command so you can have long
cfile names.
c
cRevision 1.1.1.1  1993/04/07  19:40:02  harry
cInitial CVS checkin of IVE.
c
cRevision 2.1  1992/08/19  15:41:37  warren
cincreased size of MAXCOM to 13
c
cRevision 2.0  1992/07/01  21:15:50  millerp
cReleased version rereving to 2.0
c
cRevision 1.1  1992/04/21  00:11:26  millerp
cInitial revision
c
c
c-----------------------------------------------------------------------
      integer cmdlen, fstval, maxcom
      parameter (MAXCOM = 41, CMDLEN = 512, FSTVAL = 2)
      character *(CMDLEN) comand(MAXCOM) 
      common /prscmd/ comand
c
c     common prscmd contains an array whose structure is,
c       comand(1)             some command.
c       comand(2 to maxcom)   the values associated with the command.
c