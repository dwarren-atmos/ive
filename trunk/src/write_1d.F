	subroutine write_1d(error)
c-----------------------------------------------------------------------
c     $Id: write_1d.F,v 1.4 1997/02/12 19:55:33 harry Exp $
c     Purpose:
c     This subroutine prints values used in the last 1D plot to
c     a named file.
c
c     Arguments:
c        error    logi  output  indicates possible errors found in this
c                               routine.
c                               error = false  no errors detected
c                               error = true   error detected
c     History:
c      $Log: write_1d.F,v $
c      Revision 1.4  1997/02/12 19:55:33  harry
c      Fix error introduced in last change.
c
c      Revision 1.3  1997/02/12 17:36:39  warren
c      Changed pointer type to ive_ptr to avoid interaction with F90 pointer type.
c
c      Revision 1.2  1995/06/23 21:16:35  warren
c      First part of new ive help
c      Changed " to ' in fortran files
c      Fixed the label spacing in 1d plots
c      added frame_width command
c
c Revision 1.1  1995/05/11  22:15:15  harry
c Add the WRITE_1D command.
c
c
c-----------------------------------------------------------------------

#ifdef SIXTY_FOUR_bptr
#define ive_ptr integer*8
#else
#define ive_ptr integer
#endif

#include "slices.icl" 

	include 'incommands.icl'
	include 'message.icl'
	include 'segments.icl'
	include 'pltwin.icl'
c
c
c     Argument declarations.
c
	logical error
c
c
c     Local variable declarations.
c
	character *1024  file, field
	integer          ibeg, iend
c
c
c     External function declarations.
c
	integer          nblank, strbeg, strend
c
c
c
c
	error = .false.
	ibeg  = strbeg(comand(FSTVAL))
	iend  = strend(comand(FSTVAL))
c
c
c     Make sure a value is given.
c
	if ( nblank(comand(FSTVAL)) .eq. 0 ) then
	   write (message, 100) comand(FSTVAL)(ibeg:iend)
	   call write_message
	   error = .true.
	else
c
c        Set the file name.
c
	   file(1:len(file)) = ' '
	   file = comand(FSTVAL)(ibeg:iend)
	endif
	file(iend+1:iend+1) = char(0)
	call expand_path(file)
	call getavar('field', field, error)
	call catnull(field, len(field), error)
	if(xaxis .eq. 0) then
	   call output_1d(file, %val(slabad_1d), %val(slabloc_1d),
	1	ni_1d, yaxis, field)
	else
	   call output_1d(file, %val(slabad_1d), %val(slabloc_1d),
	1	ni_1d, xaxis, field)
	endif
c
c
c     Format statements.
c
 100	format (1x,'Command ',a,' requires a value.')
c
c
      return
      end
