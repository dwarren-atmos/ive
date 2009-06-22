#ifdef SIXTY_FOUR_bptr
#define ive_ptr integer*8
#else
#define ive_ptr integer
#endif
      subroutine printr (nplts, pltnum, prfilename)
c-----------------------------------------------------------------------
c     $Id: printr.F,v 1.20 2007-02-26 23:48:38 warren Exp $
c     Purpose:
c        This routine will print one or more plots. Plots are 
c        written to a metacode workstation, then translated to 
c        postscript and printed.
c     Arguments:
c        nsegs   int  input  indicates the total number of segments 
c                            to be printed.
c        segnum  int  input  array containing the id numbers of the 
c                            segments to be written. A maximum of maxdim
c                            segments may be printed at once.
c     History:
c
c $Log: printr.F,v $
c Revision 1.20  2007-02-26 23:48:38  warren
c Fix isopen to match in here and segments.c
c
c Revision 1.19  2006/08/22 20:25:43  reinecke
c Increased file length string from 80 to 512
c
c Revision 1.18  2006/03/06 19:47:45  warren
c Call gps_init_page correctly in all spots, not just 2 out of 3
c
c Revision 1.17  2002/06/11 17:26:17  harryive
c Change aspect ratio to affect the plot, not just printing.
c
c Add first part of contour value specification.
c
c Revision 1.16  1998/09/03 18:36:27  warren
c Commandline interface for printing to a file.
c
c Revision 1.15  1998/06/12 22:03:39  harry
c Make sure prnam is blank when no printer name has been specified.
c
c Revision 1.14  1997/12/31 23:31:20  warren
c Fixed Dales color loxking problem, holton's crashes etc.
c
c Revision 1.13  1997/06/27 21:15:59  harry
c Add missing Id and Log lines.
c
c
c Revision 1.4  1993/07/20  22:44:49  warren
c Added ability to set prints and eps files to draw color lines. Added
c ability to choose printer. Added ability to specify which spot on the
c page to print at.
c
c Revision 1.3  1993/06/01  19:55:51  yuri
c Second try.
c
c Revision 1.1.1.1  1993/04/07  19:40:36  harry
c Initial CVS checkin of IVE.
c
c Revision 2.25  1993/03/05  23:48:08  warren
c mod to work with newest xgks
c
c Revision 2.24  1993/01/07  17:25:40  harry
c Correct error in printr.  Update print_file.
c
c Revision 2.23  1993/01/06  17:19:51  harry
c Change a segment from one each plot to one each overlay.
c
c Revision 2.22  1992/11/18  19:59:22  millerp
c Fixed mystery bug.
c
c Revision 2.19  1992/10/23  17:35:05  millerp
c deleted unused variables.
c
c Revision 2.18  1992/10/21  21:33:12  warren
c added write_file
c
c Revision 2.17  1992/09/14  17:31:57  warren
c fixed i to segnum(i)
c
c Revision 2.16  1992/09/11  01:44:29  warren
c fixed to use the new aspect value in gps_print.
c
c Revision 2.15  1992/08/24  20:39:22  millerp
c Fix aspect ratio feature.
c
c Revision 2.14  1992/07/22  22:57:38  millerp
c Deleted unused variables.
c
c Revision 2.13  1992/07/16  17:47:14  warren
c fixed backwards ws and segid problem
c
c Revision 2.12  1992/07/13  23:10:50  warren
c uses new postscript output
c
c Revision 2.11  1992/07/09  15:37:48  millerp
c *** empty log message ***
c
c Revision 2.0  1992/07/01  21:28:03  millerp
c Released version rereving to 2.0
c
c Revision 1.3  1992/06/30  21:04:56  millerp
c Improved multiple prints per page scheme.
c
c Revision 1.2  1992/06/05  14:55:58  millerp
c Fixed color prints using argument to gksm2ps, and used scaling,
c rotation on some plots.
c
c Revision 1.1  1992/04/21  00:23:37  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
#ifndef LIBDIR
#define LIBDIR '/usr/local/lib/ive/'
#endif
      include 'message.icl'
      include 'segments.icl'
      include 'printer.icl'
c     
c     GKS DEFS
c     
c     
      INTEGER    GPERFO, GSGOP
      PARAMETER (GPERFO=1, GSGOP=4)
      
c     
c     Argument declarations.
c     
      integer nplts, pltnum(nplts)
      character *(*)prfilename
c     
c     
c     Local variable declarations.
c
      ive_ptr ctb, get_color_table
      integer i, ibeg, iend, istate, ierr, MAXPLTS, 
     &        numplts, PLT_ID, ii, j, ix, land,junk
      real    xscale, yscale
      character *2 string
      character *10  SCRATCHDIR
      character *80 curdir, metaname, prnam,randname
      character *256 syscommand, PSDIR
      parameter ( PLT_ID = 200, 
     &            MAXPLTS = 6,
     &            SCRATCHDIR = '~/ive_ps.')
c
c     External function declarations.
c     
      integer  genran, strbeg, strend, nblank
c     

      call getenv ('IVEPSDIR', PSDIR)      
      if(nblank(PSDIR) .eq. 0)  PSDIR=LIBDIR
      xscale = 1.0
      yscale = 1.0
c
c     Fix printing for lpr or lp
c
      if(printer_name(1:1) .ne. ' ') then
         if(print_command(strbeg(print_command):
     &        strend(print_command)) .eq. 'lpr') then
            prnam = '-P'//printer_name(strbeg(printer_name):
     &           strend(printer_name))
         else if (print_command(strbeg(print_command):
     &           strend(print_command)) .eq. 'lp') then
            prnam = '-d'//printer_name(strbeg(printer_name):
     &           strend(printer_name))
         endif
      else
         if (print_command(strbeg(print_command):
     &        strend(print_command)) .eq. 'lp') then
            prnam = '-'
         else
            prnam = ' '
         endif
      endif
c     
c     Update screen and WISS.
c     
      call guwk(1, GPERFO)
c     
c     Close any open segment. 
c     
      call gqops (istate)
      if ( istate .eq. GSGOP ) call gclsg()
c     
c     Deactivate screen workstation.
c     
      call gdawk (1)
c     
c     Create a unique postscript file name by appending a random 
c     number.
c     
      junk = genran (10000)
      write (randname, 919)junk
 919  format (i5)
      curdir = SCRATCHDIR
      call catnull(curdir, len(curdir),ierr)
      call expand_path(curdir)
      metaname = curdir(strbeg(curdir):strend(curdir))//
c     &     metaname(strbeg(metaname):strend(metaname))//
     &     randname(strbeg(randname):strend(randname))//
     &     char(0)
      call gps_init(metaname)
c
c     Dump color table
c
      call dump_color_table(curlist)
c     
c     Write all specified segments (up to 6).
c     
      numplts = min(nplts, MAXPLTS)
      if (numplts.eq.1.or.numplts.eq.3.or.numplts.eq.4) then
         land = landscape
      else
         land = 1-landscape
      endif
      do i = 1, numplts
         if (land .eq. 1 .and. numplts .gt. 2) then
            if (numplts .le. 4) then
               ix = (mod(i-1,2)+1)*2 - int((i-1)/2)
            else
               if(mod(i,2) .eq. 1) then
                  ix = i+1
               else
                  ix = i-1
               endif
            endif
         else
            if (numplts .le.4) then
               ix = i
               else
                  if(mod(i,2) .eq. 1) then
                     ix = 3 - (mod(i,3)+1)
                  else
                     ix = 6 - mod(i,3)
                  endif
                  if(ix .eq. 0)ix = 3
               endif
         endif
         if(pltnum(ix) .eq. -1) then
            call gps_init_page(land,xscale/yscale)
            call gps_page()
         else
            ii = mod(pltnum(ix)-1, MAXLIST) + 1
            ctb = get_color_table(ii)
            call gps_init_page(land,xscale/yscale)
            if(color_printer) then
               do j = 1, curoverlay(ii)
                  call gps_nprint(1, segs(j,ii),.false., xscale/yscale, 
     &                           %val(ctb))
               enddo
            else
               do j = 1, curoverlay(ii)
                  call gps_nprint(1, segs(j,ii),.true., xscale/yscale, 
     &                           %val(ctb))
               enddo
            endif
            call gps_page()
         endif
      enddo
      call gps_end()
      call gacwk (1)
c     
c     Print and return. Remember to null-terminate those strings!
c     
      ibeg = strbeg(metaname)
      iend = strend(metaname)
      if ( nplts .eq. 1 ) then
         if(printer_name(strbeg(printer_name):
     &        strend(printer_name)) .eq. 'File' .or.
     &        printer_name(strbeg(printer_name):
     &        strend(printer_name)) .eq. 'FILE' .or.
     &        printer_name(strbeg(printer_name):
     &        strend(printer_name)) .eq. 'file' ) then
            if(prfilename .eq. ' ') then
               write (message,*) 'No vaild file name was given'
               call write_message
               syscommand = '/bin/rm '//metaname(ibeg:iend)//char(0)
               return
            endif
            syscommand ='cat '//metaname(ibeg:iend)//' > '
     &           //prfilename(strbeg(prfilename):strend(prfilename))
         else
            
            if(nblank(prnam) .gt. 0) then
               syscommand ='cat '//metaname(ibeg:iend)//' | '//
     &              print_command(strbeg(print_command):
     &              strend(print_command))// 
     &              ' '//prnam(strbeg(prnam):strend(prnam))//char(0)	
            else
               syscommand ='cat '//metaname(ibeg:iend)//' | '//
     &              print_command(strbeg(print_command):
     &              strend(print_command))//char(0)	
            endif
         endif
      else
c     
c     numplts must be either 2, 4, or 6.
c     
         if ( (nplts .eq. 3) .or. (nplts .eq. 5) ) then
            numplts = nplts + 1
         else
            numplts = min(nplts, MAXPLTS)
         endif
         write (string, 999) numplts
 999     format (i2)
         if(printer_name(strbeg(printer_name):
     &        strend(printer_name)) .eq. 'File' .or.
     &        printer_name(strbeg(printer_name):
     &        strend(printer_name)) .eq. 'FILE' .or.
     &        printer_name(strbeg(printer_name):
     &        strend(printer_name)) .eq. 'file' ) then
            if(prfilename .eq. ' ') then
               write (message,*) 'No vaild file name was given'
               call write_message
               syscommand = '/bin/rm '//metaname(ibeg:iend)//char(0)
               return
            endif
            syscommand = PSDIR(strbeg(PSDIR):strend(PSDIR))//
     &           '/mpage -n '//string//
     &           ' '//metaname(ibeg:iend)//' > '//         
     &           prfilename(strbeg(prfilename):strend(prfilename))

         elseif(nblank(prnam) .gt. 0) then
            syscommand = PSDIR(strbeg(PSDIR):strend(PSDIR))//
     &           '/mpage -n '//string//
     &           ' '//metaname(ibeg:iend)//' | '//
     &           print_command(strbeg(print_command):
     &           strend(print_command))//
     &           ' '//prnam(strbeg(prnam):
     &           strend(prnam))//
     &           char(0)
         else
            syscommand = PSDIR(strbeg(PSDIR):strend(PSDIR))//
     &		 '/mpage -n '//string//
     &           ' '//metaname(ibeg:iend)//' | '//
     &           print_command(strbeg(print_command):
     &           strend(print_command))//char(0)
         endif
      endif
      CALL DO_SYSTEM (syscommand)	
      syscommand = '/bin/rm '//metaname(ibeg:iend)//char(0)	
      CALL DO_SYSTEM (syscommand)
      
c     endif
      return
c     
      end           


      subroutine print_file (pltnum, name)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine will write a plot to a file.
c     Arguments:
c        pltnum  int  input  number of plot to write.
c        name    char input  file name.	
c-----------------------------------------------------------------------
c
      include 'message.icl'
      include 'segments.icl'
      include 'printer.icl'
c
c     Argument declarations.
c
      integer pltnum
      character *(*) name
c
c     GKS DEFS
c
      ive_ptr ctb, get_color_table
      INTEGER    GPERFO, GSGOP
      PARAMETER (GPERFO=1, GSGOP=4)
c
c     Local variable declarations.
c
      character *512 metaname
      integer istate, ierr, ii, j
      real    xscale, yscale
      
      integer  genran, strbeg, strend, nblank
c
c     Common blocks.
c
      logical*4 isopen
      common / is_eps_file_open / isopen
c
      xscale = 1.0
      yscale = 1.0
c     
c     Update screen and WISS.
c     
      call guwk(1, GPERFO)
c     
c     Close any open pltment. 
c     
      call gqops (istate)
      if ( istate .eq. GSGOP ) call gclsg()
c     
c     Create a unique postscript file name by appending a random 
c     number.
c     
      metaname = name	
      call catnull(metaname, len(metaname),ierr)
      call expand_path(metaname)
      call geps_init(metaname)
      if(isopen) then
         call gps_init_page(landscape,xscale/yscale)
c
c     Dump color table
c
         call dump_color_table(curlist)
c     
c     Write specified plot to file
c     
         ii = mod(pltnum-1, MAXLIST) + 1
         ctb = get_color_table(ii)
         if(color_file) then
            do j = 1, curoverlay(ii)
               call gps_nprint(1, segs(j, ii),.false., xscale/yscale,
     &              %val(ctb))
            enddo
         else
            do j = 1, curoverlay(ii)
               call gps_nprint(1, segs(j, ii),.true., xscale/yscale,
     &              %val(ctb))
            enddo
         endif
         call gps_page()
         call gps_end()
      else
         write (message, 100) metaname(strbeg(metaname):
     &        strend(metaname))
         call write_message
      endif
c     
c     
 100     format (1x,'File ',a,' could not be opened.')
           return
      end           
      


      subroutine pdf_file (pltnum, name)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine will write a plot to a pdf file.
c     Arguments:
c        pltnum  int  input  number of plot to write.
c        name    char input  file name.	
c-----------------------------------------------------------------------
c
      include 'message.icl'
      include 'segments.icl'
      include 'printer.icl'
c
c     Argument declarations.
c
      integer pltnum
      character *(*) name
c
c     GKS DEFS
c
      ive_ptr ctb, get_color_table
      INTEGER    GPERFO, GSGOP
      PARAMETER (GPERFO=1, GSGOP=4)
c
c     Local variable declarations.
c
      character*1024 metaname
      integer istate, ierr, ii, j, overlay
      real    xscale, yscale
      
      integer  genran, strbeg, strend, nblank
c
c     Common blocks.
c
      logical*4 isopen
      common / is_eps_file_open / isopen
c
      xscale = 1.0
      yscale = 1.0
c     
c     Update screen and WISS.
c     
      call guwk(1, GPERFO)
c     
c     Close any open pltment. 
c     
      call gqops (istate)
      if ( istate .eq. GSGOP ) call gclsg()
c     
c     Create a unique postscript file name by appending a random 
c     number.
c     
      metaname = name	
      call catnull(metaname, len(metaname),ierr)
      call expand_path(metaname)
      call gpdf_init(metaname)
      call gpdf_init_page(landscape,xscale/yscale)

c
c     Dump color table
c
      call dump_color_table(curlist)
c     
c     Write specified plot to file
c     
      ii = mod(pltnum-1, MAXLIST) + 1
      ctb = get_color_table(ii)
      overlay=0
      do j = 1, curoverlay(ii)
         call gpdf_nprint(1, segs(j, ii), xscale/yscale,
     &        %val(ctb),overlay)
         overlay=1
      enddo
      call gpdf_end()
      end           
      


