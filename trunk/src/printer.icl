ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     Printer.icl
c     This file contains the printer setup commonblock
c$Id: printer.icl,v 1.3 1997/01/31 18:21:17 warren Exp $
c$Log: printer.icl,v $
cRevision 1.3  1997/01/31 18:21:17  warren
cAdded print_command command for lp or lpr
c
cRevision 1.2  1996/12/23 20:58:13  warren
cLandscape printing option
c
c Revision 1.1  1993/07/20  22:44:46  warren
c Added ability to set prints and eps files to draw color lines. Added
c ability to choose printer. Added ability to specify which spot on the
c page to print at.
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      character*80 printer_name, print_command
      logical color_printer,color_file
      integer landscape
      
      common/printer_strings/printer_name,print_command
      common/printer_logicals/color_printer,color_file,landscape
