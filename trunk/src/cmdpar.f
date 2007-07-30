      subroutine cmdpar (comlin, beggrp)
c-----------------------------------------------------------------------
c     $Id: cmdpar.f,v 1.1.1.1 1993/04/07 19:39:49 harry Exp $
c     Purpose:
c        This routine parses a command line into individual commands
c           and their associated values.
c        The structure of the command line is expected to be of the
c           form 
c           COMMAND{=VALUE/VALUE/...},COMMAND{=VALUE/VALUE/...},... 
c        Each COMMAND may have several optional VALUEs associated 
c           with it, or none at all. 
c     Arguments:
c        comlin  char  input  command line to be parsed.
c        beggrp  int   i/o    indicates to the calling program where
c                             the next command starts (0 => none)
c     History:
c      $Log: cmdpar.f,v $
c      Revision 1.1.1.1  1993/04/07 19:39:49  harry
c      Initial CVS checkin of IVE.
c
c Revision 2.4  1992/09/24  23:13:11  harry
c Remove call to strip.
c
c Revision 2.3  1992/09/22  20:54:07  warren
c changed ; to , and , to ;
c
c Revision 2.2  1992/08/19  15:39:59  warren
c add call to strip to take care of point specifications.
c
c Revision 2.1  1992/08/11  16:13:12  harry
c Change calling sequence to properly handle recursion.
c Must use new version of driver.f with this change.
c
c Revision 2.0  1992/07/01  21:12:41  millerp
c Released version rereving to 2.0
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
      include 'incommands.icl'
c
c
c     Argument declarations.
c
      character *(*) comlin
c
c
c     Local variable declarations.
c
      character *1         CVSEP, GRPSEP, VALSEP
      integer              beggrp, begval, cntstr, endcmd, endgrp, 
     &                     endval, i, j, lenlin, numval, seppos
      logical done
c
c
c     External function declarations.
c
      integer len, strend
      parameter (CVSEP  = '=' , 
     &           GRPSEP = ';' , 
     &           VALSEP = ',' )
c
c
c     Strip Blanks and Find the length of the input command line. 
c
      done = .false.
c     call  strip(comlin)
      lenlin = strend (comlin)
c
c
c     Blank out comand.
c
      do 10 i = 1, MAXCOM
         comand(i)(1:len(comand(i))) = ' '
 10   continue
c
c
c     Find the separator between each COMMAND=VALUE group.
c
      seppos = index(comlin(beggrp:lenlin),grpsep) 
c
c
c     If there is no separator, this must be the last COMMAND=VALUE
c     group. Set a variable pointing to the end of the current group.
c
      if (seppos .eq. 0) then
         done = .true.
         endgrp = lenlin - seppos
      else
         endgrp = beggrp + seppos - 2
      endif
c
c
c     Find the separator between COMMAND and VALUE in the group, and
c     set a variable pointing to the end of the command. 
c     Also count the number of values associated with the command.
c
      seppos = index(comlin(beggrp:endgrp),CVSEP)
      if (seppos .eq. 0) then
c
c        This command has no values.
c
         numval = 0
         endcmd = endgrp
      else
c
c        This command has values, so count them.
c
         numval = cntstr(comlin(beggrp:endgrp),VALSEP) + 1
         endcmd = beggrp + seppos - 2
      endif
c
c        
c     Now fill the array comand. The first element is always COMMAND,
c     and the rest are VALUEs.
c
      comand(1)(1:CMDLEN) = comlin(beggrp:endcmd)
      begval = endcmd + 2
      do 20 j = FSTVAL, FSTVAL + (numval - 1)
         seppos = index(comlin(begval:endgrp),VALSEP)
         if (seppos .eq. 0) seppos = endgrp - begval + 2
         endval = begval + seppos - 2
         comand(j) = comlin(begval:endval)
         begval = begval + seppos
 20   continue
c
c
c     Check to see if we are done. If so, reset beggrp.
c
      if (done) then
         beggrp = 0
      else
c
c        Reset beggrp to point to the beginning of the next
c        COMMAND=VALUE group in the current command line.
c
         beggrp = endgrp + 2
      endif
c
c
      return
      end






