      subroutine get_prscmd(commands, length, number)
c-----------------------------------------------------------------------
c     $Id: get_prscmd.F,v 1.2 1997/02/12 17:35:57 warren Exp $
c     Purpose:
c        This routine gets the command arguments for a C routine.
c
c     Arguments:
c        commands int  output Address of the command array.
c        length   int  output Number of characters in a entry.
c        number   int  output Maximum number of parameters.
c
c     History:
c        $Log: get_prscmd.F,v $
c        Revision 1.2  1997/02/12 17:35:57  warren
c        Changed pointer type to ive_ptr to avoid interaction with F90 pointer type.
c
c        Revision 1.1  1993/11/29 19:39:20  warren
c        Changes to 3.1 to make it work on alphas.
c        FORTRAN: new data type ive_ptr ifdefed to either 32 bit or 64 bit.
c        C: Change int to long where necessary.
c        Files: Quite a few .f files are now .F
c
c Revision 1.1.1.1  1993/04/07  19:39:57  harry
c Initial CVS checkin of IVE.
c
c Revision 1.2  1992/11/18  19:09:12  warren
c made LOC %LOC for the DECstations
c
c Revision 1.1  1992/11/18  18:11:43  harry
c Initial revision
c
c-----------------------------------------------------------------------
#ifdef SIXTY_FOUR_bptr
#define ive_ptr integer*8
#else
#define ive_ptr integer
#endif

      ive_ptr commands
      integer length, number

      include 'incommands.icl'

      commands = %LOC(comand)
      length = CMDLEN
      number = MAXCOM
      return

      end
