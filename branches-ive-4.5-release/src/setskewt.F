      subroutine setskewt (error)
c
c     $Id: setskewt.F,v 1.3 2000/01/12 21:18:31 warren Exp $
c
c     This routine processes the SKEWT_COMPONENTS command.
c     The syntax is:
c
c     SKEWT_COMPONENTS=P,T,TD
c
c     where P is the pressure coordinate, T is the temperature coordinate,
c     and TD is the dewpoint coordinate.  If P is missing, IVE assumes that
c     the vertical (Z) coordinate is pressure.  Either T or TD can be
c     missing.
c
c     $Log: setskewt.F,v $
c     Revision 1.3  2000/01/12 21:18:31  warren
c     had an i where a 1 should have been - making for random crashes.
c
c     Revision 1.2  1997/02/12 17:36:33  warren
c     Changed pointer type to ive_ptr to avoid interaction with F90 pointer type.
c
c     Revision 1.1  1996/04/25 16:25:28  harry
c     Add Skew-T plots and the beginning of trajectory plots.
c
c                                                                       
      include 'incommands.icl'
      include 'skewt.icl'
      include 'message.icl'
      include 'data.icl'

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
      character *(CMDLEN) curval
      character *(80)     component, units, dunits, vunits, vdunits,
     &                    dnames, vdnames
      integer             dims, i, ibeg, iend, j, ndims, num_found, 
     &                    vdims, nvdims, ivars
      ive_ptr             temp_ptr
      logical             loaded, new
      real                datmax, datmin, misdat, vdatmax, 
     &                    vdatmin, vmisdat, vstag
      dimension dims(MAXDIM), datmax(MAXDIM), datmin(MAXDIM), 
     &          vdims(MAXDIM), 
     &          vdatmax(MAXDIM), vdatmin(MAXDIM), 
     &          dnames(MAXDIM), vdnames(MAXDIM),
     &          vstag(MAXDIM), component(3)
      dimension ivars(MAXVAR)
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
      new = .false.
      error = .false.
      loaded = .false.
      num_found = 0
      units(1:len(units)) = ' '
      dunits(1:len(dunits)) = ' '
      skewt_component(1) = ' '
      skewt_component(2) = ' '
      skewt_component(3) = ' '
c
c
      do 10 i = 1, 3
         curval(1:len(curval)) = comand(FSTVAL+(i-1))(1:CMDLEN)
         ibeg = strbeg (curval)
         iend = strend (curval)
         call upcase (curval(ibeg:iend), (iend - ibeg + 1))
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
               call garbage_collect_vars()
               if (new) then
                  call make_button_list (ivars,MAXVAR) 
                  call make_field_buttons (ivars, %val(0))
               endif
               num_found = num_found + 1
c               if ( .not. loaded ) then
c                  loaded = .true.
c     
c                 The attributes for skewt are set equal to the 
c                 attributes of the first varible found in the data
c                 file.
c
                  att_var_skewt(1:len(att_var_skewt)) =
     &                 curval(ibeg:iend)
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
                  call load_atts (ndims, dims, stag, datmin, datmax, 
     &                 misdat, units, dunits, dnames)
c               endif
            endif
         endif
 10   continue
c     
c
      if ( num_found .gt. 0 ) then
c
         if(nblank(component(1)) .ne. 0) then
            skewt_component(1) = ' '		
            skewt_component(1)(1:len(skewt_component(1))) = 
     &           component(1)(strbeg(component(1)):
     &           strend(component(1)))
         endif	
         if(nblank(component(2)) .ne. 0) then
            skewt_component(2) = ' '
            skewt_component(2)(1:len(skewt_component(2))) = 
     &           component(2)(strbeg(component(2)):
     &           strend(component(2)))
         endif
         if(nblank(component(3)) .ne. 0) then
            skewt_component(3) = ' '
            skewt_component(3)(1:len(skewt_component(3))) = 
     &           component(3)(strbeg(component(3)):
     &           strend(component(3)))
         endif
      endif
c
c      call load_atts (ndims, dims, stag, datmin, datmax, 
c     &                   misdat, units, dunits, dnames)
c
c     Format statements.
c
 100  format (1x,'SETSKEWT: The skewt component ',a,' cannot be ',
     &           'found.')
 110  format (1x,'SETSKEWT: Cannot get any of the specified skewt '
     &           'field components.')
c
c
      return
      end
