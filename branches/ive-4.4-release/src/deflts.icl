c-----------------------------------------------------------------------
c     $Id: deflts.icl,v 1.11 2007-06-21 20:58:30 reinecke Exp $
c     Purpose:
c        This file contains the common block which carries the default
c        settings for all local variables and all plotting utility
c        common blocks.
c
c     History:
c	$Log: deflts.icl,v $
c	Revision 1.11  2007-06-21 20:58:30  reinecke
c	Commiting local modifications for transfer to svn
c
c	Revision 1.10  2002/06/11 17:26:16  harryive
c	Change aspect ratio to affect the plot, not just printing.
c	
c	Add first part of contour value specification.
c	
c	Revision 1.9  1998/03/04 19:13:58  davidive
c	First cut at trajectories and trajectory widget using colors to show depth
c	and arrows to show direction.
c
cRevision 1.8  1995/07/12  14:54:08  warren
cShrink frame a bit more for big letters so they fit in the window
c
cRevision 1.7  1995/06/23  21:16:12  warren
cFirst part of new ive help
cChanged " to ' in fortran files
cFixed the label spacing in 1d plots
cadded frame_width command
c
cRevision 1.6  1994/01/29  00:11:28  warren
cfirst cut at movie labels. Line plots still need work
c
cRevision 1.5  1993/12/28  21:00:48  harry
cDeath to constfil!
c
cRevision 1.4  1993/12/15  18:13:16  harry
cChange default map transform to agree with default version of
cdefault_map in trans/loadfunc.c.
c
cRevision 1.3  1993/07/12  19:13:09  warren
cChanged window limits for ticmark plot to mak compac happy.
c
cRevision 1.2  1993/07/09  19:09:28  warren
cPretty labels to go with the 2d ticmarks. Also I removed superfluous calls
cto chkwin and moved all the others to driver.f.
c
cRevision 1.1.1.1  1993/04/07  19:39:51  harry
cInitial CVS checkin of IVE.
c
cRevision 2.1  1992/09/29  23:34:46  warren
cadditions for setlineplt
c
cRevision 2.0  1992/07/01  21:15:44  millerp
cReleased version rereving to 2.0
c
cRevision 1.1  1992/04/21  00:11:26  millerp
cInitial revision
c
c-----------------------------------------------------------------------
c
c
c
c     Defaults for color options.
c
      character*80 D_CLRTAB
      parameter (D_CLRTAB = '/usr/local/lib/ive/default.ctb' )
c
c
c     Defaults used by the contouring routines conrec and conpack.
c
      integer D_IOFFP, D_NOZERO 
      integer D_ILAB, D_NULBLL, D_IOFFM
      real D_SPVAL
      parameter (D_IOFFP  = 1       ,
     &           D_SPVAL  = 9999.99 ,
     &           D_NOZERO = 0       ,
     &           D_ILAB   = 1       ,
     &           D_NULBLL = 3       ,
     &           D_IOFFM  = 0       )
c
c
c     Defaults for data variables.
c
      character*80 D_DATFIL
      parameter (D_DATFIL   = ' ')
c
c
c     Defaults for flags.
c
      logical D_SAVFLG
      parameter (D_SAVFLG = .false. )
c
c
c     Defaults for general plotting options.
c
      character*5 D_FILTYP
      real D_LINWDTH
      parameter (D_FILTYP  = 'LINES' ,
     &           D_LINWDTH = 1.0      )
C
c
c     Defaults for subroutine supmap.
c
      character *4 D_JPRJ, D_OUTL
      real    D_PLAT, D_PLON, D_ROTA
      parameter (D_JPRJ = 'CE  ',
     &           D_PLAT =  0.0  ,
     &           D_PLON =  0.0  ,
     &           D_ROTA =  0.0  ,
     &           D_OUTL = 'PS'  )     
c
c
c     Default plot viewport.
c
      real VPLEFT, VPRIGHT, VPBOT, VPTOP
      parameter ( VPLEFT  = 0.075 ,
     &            VPRIGHT = 0.925 ,
     &            VPBOT   = 0.075 ,
     &            VPTOP   = 0.925 )

      real VPLEFT_TIC, VPRIGHT_TIC, VPBOT_TIC, VPTOP_TIC
      parameter ( VPLEFT_TIC  = 0.096 ,
     &            VPRIGHT_TIC = 0.905 ,
     &            VPBOT_TIC  = 0.096 ,
     &            VPTOP_TIC  = 0.905 )

      real VPLEFT_LINE, VPRIGHT_LINE, VPBOT_LINE, VPTOP_LINE
      parameter ( VPLEFT_LINE  = 0.16 ,
     &            VPRIGHT_LINE = 0.925 ,
     &            VPBOT_LINE   = 0.15 ,
     &            VPTOP_LINE   = 0.915 )


      real VPLEFT_BIG, VPRIGHT_BIG, VPBOT_BIG, VPTOP_BIG
      parameter ( VPLEFT_BIG  = 0.13 ,
     &            VPRIGHT_BIG = 0.925 ,
     &            VPBOT_BIG   = 0.11 ,
     &            VPTOP_BIG   = 0.905 )

