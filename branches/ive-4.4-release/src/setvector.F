      subroutine setvector (error)
c-----------------------------------------------------------------------
c     $Id: setvector.F,v 1.6 1997/05/16 20:49:02 harryive Exp $
c     Purpose:
c        This routine is used to get the fields necessary to produce
c        vector plots.
c     Arguments:
c        error  logi  output  error flag.
c     History:
c        $Log: setvector.F,v $
c        Revision 1.6  1997/05/16 20:49:02  harryive
c        Change a (i) to a (1).
c
c        Revision 1.5  1997/02/12 17:36:36  warren
c        Changed pointer type to ive_ptr to avoid interaction with F90 pointer type.
c
c        Revision 1.4  1995/08/24 22:19:05  harry
c        Ive_ptrs is as ive_ptrs does.
c
c Revision 1.3  1994/03/22  21:32:07  warren
c fixed bad vectors at the bottom of Dale's plot and made vector labels
c behave better (they no longer overwrite the solid fill labels)
c
c Revision 1.2  1994/02/17  21:36:25  harry
c Add dim_names to getvar.  Also include modified definition of the flag.
c
c Revision 1.1  1993/11/29  19:39:42  warren
c Changes to 3.1 to make it work on alphas.
c FORTRAN: new data type ive_ptr ifdefed to either 32 bit or 64 bit.
c C: Change int to long where necessary.
c Files: Quite a few .f files are now .F
c
c Revision 1.1.1.1  1993/04/07  19:40:35  harry
c Initial CVS checkin of IVE.
c
c Revision 1.2  1992/12/31  18:49:44  millerp
c Make changes for new getvar.F.
c
c Revision 1.1  1992/09/08  19:53:31  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
c
      include 'incommands.icl'
      include 'vector.icl'
      include 'message.icl'
#ifdef SIXTY_FOUR_bptr
#define ive_ptr integer*8
#else
#define ive_ptr integer
#endif
c
c
c     Argument declarations.
c
      logical             error
c
c
c     Local variable declarations.
c
      integer             MAXDIM
      parameter (MAXDIM = 4)
      character *(CMDLEN) curval
      character *(80)     component, units, dunits, vunits, vdunits,
     &                    dnames, vdnames
      integer             i, ibeg, iend, j, ndims, num_found, nvdims
      ive_ptr             temp_ptr, dims, vdims
      logical             loaded, new
      real                datmax, datmin, misdat, stag, vdatmax, 
     &                    vdatmin, vmisdat, vstag
      dimension dims(MAXDIM), datmax(MAXDIM), datmin(MAXDIM), 
     &          stag(MAXDIM), vdims(MAXDIM), 
     &          vdatmax(MAXDIM), vdatmin(MAXDIM), 
     &          dnames(MAXDIM), vdnames(MAXDIM),
     &          vstag(MAXDIM), component(3)
c
c
c     External function declarations.
c
      integer             nblank, strbeg, strend
      ive_ptr             addvar
c
c
c
c
      error = .false.
      loaded = .false.
      num_found = 0
      units(1:len(units)) = ' '
      dunits(1:len(dunits)) = ' '
      att_var(1:len(att_var)) = ' '
c
c
      do 10 i = 1, 3
         curval(1:len(curval)) = comand(FSTVAL+(i-1))(1:CMDLEN)
         ibeg = strbeg (curval)
         iend = strend (curval)
         temp_ptr = 0
         component(i)(1:len(component(i))) = ' '
c     
c        Get a ive_ptr to each variable in the users input list. 
c      
         if ( nblank(curval(ibeg:iend)) .ne. 0 ) then
            component(i)(1:len(component(i))) = curval(ibeg:iend) 
            nvdims = 1
            do j = 1, MAXDIM
               vdims(j) = 1
               vstag(j) = 0.0
               vdatmin(j) = 0.0
               vdatmax(j) = 0.0
            enddo
            vmisdat = 0.0
            vunits(1:len(vunits)) = ' '
            vdunits(1:len(vdunits)) = ' '
            temp_ptr = addvar (curval(ibeg:iend), curval(ibeg:iend),
     &                         nvdims, vdims, 
     &                         vstag, vdatmin, vdatmax, vmisdat, 
     &                         vunits, vdunits, vdnames, new)
            if ( temp_ptr .eq. 0 ) then
               write (message, 100) curval(ibeg:iend)
               call write_message
               error = .true.
               return
            else
               num_found = num_found + 1
               if ( .not. loaded ) then
                  loaded = .true.
c     
c                 The attributes for vectors are set equal to the 
c                 attributes of the first varible found in the data
c                 file.
c
                  att_var(1:len(att_var)) = curval(ibeg:iend)
                  do j = 1, MAXDIM
                     dims(j)  = vdims(j)
                     stag(j)  = vstag(j)
                     datmin(j) = vdatmin(j)
                     datmax(j) = vdatmax(j)
	             dnames(j) = vdnames(j)
                  enddo
                  ndims = nvdims
                  misdat = vmisdat
                  units(1:len(units)) = vunits(strbeg(vunits):
     &                                         strend(vunits))
                  dunits(1:len(dunits)) = vdunits(strbeg(vdunits):
     &                                            strend(vdunits))
               endif
            endif
         endif
 10   continue
c     
c
      if ( num_found .gt. 0 ) then
c
         if(nblank(component(1)) .ne. 0) then
            vect_component(1) = ' '		
            vect_component(1)(1:len(vect_component(1))) = 
     &           component(1)(strbeg(component(1)):
     &           strend(component(1)))
         endif	
         if(nblank(component(2)) .ne. 0) then
            vect_component(2) = ' '
            vect_component(2)(1:len(vect_component(2))) = 
     &           component(2)(strbeg(component(2)):
     &           strend(component(2)))
         endif
         if(nblank(component(3)) .ne. 0) then
            vect_component(3) = ' '
            vect_component(3)(1:len(vect_component(3))) = 
     &           component(3)(strbeg(component(3)):
     &           strend(component(3)))
         endif
c     
c        Load attributes for vectors. 
c     
         call load_atts (ndims, dims, stag, datmin, datmax, 
     &                   misdat, units, dunits, dnames)
c         call setavar ('field', att_var, error)
c         call readfld (error)
      else
         write (message, 110)
         call write_message
         error = .true.
      endif
c
c
c     Format statements.
c
 100  format (1x,'SETVECTOR: The vector component ',a,' cannot be ',
     &           'found.')
 110  format (1x,'SETVECTOR: Cannot get any of the specified vector '
     &           'field components.')
c
c
      return
      end

      
