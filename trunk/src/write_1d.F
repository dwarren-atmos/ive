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


      include 'incommands.icl'
      include 'message.icl'
      include 'segments.icl'
      include 'pltwin.icl'
      include 'flags.icl'
#include "slices.icl"

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
      ive_ptr          x,y,z,t,f,getmem
      real                phmin(4),phmax(4)
      real                xloc,yloc,zloc,tloc
      integer          lock(4)
      integer          ibeg, iend,netcdf
c
c
c     External function declarations.
c
      integer          nblank, strbeg, strend
c
      error = .false.
      ibeg  = strbeg(comand(FSTVAL))
      iend  = strend(comand(FSTVAL))
c     default to text output
      netcdf=0
c
c     Make sure a value is g`iven.
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
      if((file(iend-2:iend) .eq. ".nc").or.
     &     (file(iend-3:iend) .eq. ".cdf")) netcdf=1
         
      call expand_path(file)
      call getavar('field', field, error)
      call catnull(field, len(field), error)
      
      x=0
      y=0
      z=0
      t=0
      f=0
      ! start of printing out lat or lon location when on a map.
      ! I am not sure how to proceed for two reasons:
      !  1)  slabad_1d is not in physical coordinates
      !  2)  slabad_1d only contains the slab location in either the x
      !  or the y direction, not both.  phys_2_lonlat needs both.
c      if(mapflg) then 
c        call phys_2_lonlat(%val(slabad_1d), y, lon, lat, ni_1d) 
c      end if

      call getiarr('lock',lock,4,error)
      if(error) then
         write (message,*)'Cant get lock structure in write_1d'
         call write_message
         return
      endif

      call getrarr('phmin',phmin,4,error)
      if(error) then
         write (message,*)'Cant get phmin structure in write_1d'
         call write_message
         return
      endif

      call getrarr('phmax',phmax,4,error)
      if(error) then
         write (message,*)'Cant get phmax structure in write_1d'
         call write_message
         return
      endif

      f=slabloc_1d

      if (slope .eq. 0.) then
         select case (ind)
            case(1)
               x = slabad_1d
               y=getmem(ni_1d)
               z=getmem(ni_1d)
               t=getmem(ni_1d)
               if(lock(2) .eq. 1) then
                  call getrvar('yloc',yloc,error)
               else
                  yloc=(phmin(2)+phmax(2))/2.
               endif
               if(lock(3) .eq. 1) then
                  call getrvar('zloc',zloc,error)
               else
                  zloc=(phmin(3)+phmax(3))/2.
               endif
               if(lock(4) .eq. 1) then
                  call getrvar('time',tloc,error)
               else
                  tloc=(phmin(4)+phmax(4))/2.
               endif
               if(error) then
                  write (message,*)'Cant get loc in write_1d'
                  call write_message
                  call freemem(y)
                  call freemem(z)
                  call freemem(t)
                  return
               endif
               call fill_array(%val(y),ni_1d,yloc)
               call fill_array(%val(z),ni_1d,zloc)
               call fill_array(%val(t),ni_1d,tloc)           
            case(2)
               y = slabad_1d
               x=getmem(ni_1d)
               z=getmem(ni_1d)
               t=getmem(ni_1d)
               if(lock(1) .eq. 1) then
                  call getrvar('xloc',xloc,error)
               else
                  xloc=(phmin(1)+phmax(1))/2.
               endif
               if(lock(3) .eq. 1) then
                  call getrvar('zloc',zloc,error)
               else
                  zloc=(phmin(3)+phmax(3))/2.
               endif
               if(lock(4) .eq. 1) then
                  call getrvar('time',tloc,error)
               else
                  tloc=(phmin(4)+phmax(4))/2.
               endif
               if(error) then
                  write (message,*)'Cant get loc in write_1d'
                  call write_message
                  call freemem(y)
                  call freemem(z)
                  call freemem(t)
                  return
               endif
               call fill_array(%val(y),ni_1d,yloc)
               call fill_array(%val(z),ni_1d,zloc)
               call fill_array(%val(t),ni_1d,tloc)           
            case(3)
               z = slabad_1d
               y=getmem(ni_1d)
               x=getmem(ni_1d)
               t=getmem(ni_1d)
               if(lock(2) .eq. 1) then
                  call getrvar('yloc',yloc,error)
               else
                  yloc=(phmin(2)+phmax(2))/2.
               endif
               if(lock(1) .eq. 1) then
                  call getrvar('xloc',xloc,error)
               else
                  xloc=(phmin(1)+phmax(1))/2.
               endif
               if(lock(4) .eq. 1) then
                  call getrvar('time',tloc,error)
               else
                  tloc=(phmin(4)+phmax(4))/2.
               endif
               if(error) then
                  write (message,*)'Cant get loc in write_1d'
                  call write_message
                  call freemem(y)
                  call freemem(z)
                  call freemem(t)
                  return
               endif
               call fill_array(%val(y),ni_1d,yloc)
               call fill_array(%val(z),ni_1d,zloc)
               call fill_array(%val(t),ni_1d,tloc)           
            case(4)
               t = slabad_1d
               y=getmem(ni_1d)
               z=getmem(ni_1d)
               x=getmem(ni_1d)
               if(lock(2) .eq. 1) then
                  call getrvar('yloc',yloc,error)
               else
                  yloc=(phmin(2)+phmax(2))/2.
               endif
               if(lock(3) .eq. 1) then
                  call getrvar('zloc',zloc,error)
               else
                  zloc=(phmin(3)+phmax(3))/2.
               endif
               if(lock(1) .eq. 1) then
                  call getrvar('xloc',xloc,error)
               else
                  xloc=(phmin(1)+phmax(1))/2.
               endif
               if(error) then
                  write (message,*)'Cant get loc in write_1d'
                  call write_message
                  call freemem(y)
                  call freemem(z)
                  call freemem(t)
                  return
               endif
               call fill_array(%val(y),ni_1d,yloc)
               call fill_array(%val(z),ni_1d,zloc)
               call fill_array(%val(t),ni_1d,tloc)           
            end select
         else

C
C   either part of line, or locked or averaged      
C
            select case (ind)
             case(1)
                x = slabad_1d
                y=getmem(ni_1d)
                z=getmem(ni_1d)
                t=getmem(ni_1d)
                select case (dep)
                  case(2)
                     call ive_fill_line(%val(y),%val(x),slope,
     &                    intercept,ni_1d)
                     if(lock(3) .eq. 1) then
                        call getrvar('zloc',zloc,error)
                     else
                        zloc=(phmin(3)+phmax(3))/2.
                     endif
                     if(lock(4) .eq. 1) then
                        call getrvar('time',tloc,error)
                     else
                        tloc=(phmin(4)+phmax(4))/2.
                     endif
                     if(error) then
                        write (message,*)'Cant get loc in write_1d'
                        call write_message
                        call freemem(y)
                        call freemem(z)
                        call freemem(t)
                        return
                     endif
                     call fill_array(%val(z),ni_1d,zloc)
                     call fill_array(%val(t),ni_1d,tloc)           
                  case(3)
                     call ive_fill_line(%val(z),%val(x),slope,
     &                    intercept,ni_1d)
                     if(lock(2) .eq. 1) then
                        call getrvar('yloc',yloc,error)
                     else
                        yloc=(phmin(2)+phmax(2))/2.
                     endif
                     if(lock(4) .eq. 1) then
                        call getrvar('time',tloc,error)
                     else
                        tloc=(phmin(4)+phmax(4))/2.
                     endif
                     if(error) then
                        write (message,*)'Cant get loc in write_1d'
                        call write_message
                        call freemem(y)
                        call freemem(z)
                        call freemem(t)
                        return
                     endif
                     call fill_array(%val(y),ni_1d,yloc)
                     call fill_array(%val(t),ni_1d,tloc)           
                  case(4)
                     call ive_fill_line(%val(t),%val(x),slope,
     &                    intercept,ni_1d)
                     if(lock(2) .eq. 1) then
                        call getrvar('yloc',yloc,error)
                     else
                        yloc=(phmin(2)+phmax(2))/2.
                     endif
                     if(lock(3) .eq. 1) then
                        call getrvar('zloc',zloc,error)
                     else
                        zloc=(phmin(3)+phmax(3))/2.
                     endif
                     if(error) then
                        write (message,*)'Cant get loc in write_1d'
                        call write_message
                        call freemem(y)
                        call freemem(z)
                        call freemem(t)
                        return
                     endif
                     call fill_array(%val(z),ni_1d,zloc)
                     call fill_array(%val(y),ni_1d,yloc)           
                end select
             case(2)
                y = slabad_1d
                x=getmem(ni_1d)
                z=getmem(ni_1d)
                t=getmem(ni_1d)
                select case (dep)
                  case(1)
                     call ive_fill_line(%val(x),%val(y),slope,
     &                    intercept,ni_1d)
                     if(lock(3) .eq. 1) then
                        call getrvar('zloc',zloc,error)
                     else
                        zloc=(phmin(3)+phmax(3))/2.
                     endif
                     if(lock(4) .eq. 1) then
                        call getrvar('time',tloc,error)
                     else
                        tloc=(phmin(4)+phmax(4))/2.
                     endif
                     if(error) then
                        write (message,*)'Cant get loc in write_1d'
                        call write_message
                        call freemem(y)
                        call freemem(z)
                        call freemem(t)
                        return
                     endif
                     call fill_array(%val(z),ni_1d,zloc)
                     call fill_array(%val(t),ni_1d,tloc)           
                  case(3)
                     call ive_fill_line(%val(z),%val(y),slope,
     &                    intercept,ni_1d)
                     if(lock(1) .eq. 1) then
                        call getrvar('xloc',xloc,error)
                     else
                        xloc=(phmin(1)+phmax(1))/2.
                     endif
                     if(lock(4) .eq. 1) then
                        call getrvar('time',tloc,error)
                     else
                        tloc=(phmin(4)+phmax(4))/2.
                     endif
                     if(error) then
                        write (message,*)'Cant get loc in write_1d'
                        call write_message
                        call freemem(y)
                        call freemem(z)
                        call freemem(t)
                        return
                     endif
                     call fill_array(%val(x),ni_1d,xloc)
                     call fill_array(%val(t),ni_1d,tloc)           
                  case(4)
                     call ive_fill_line(%val(t),%val(y),slope,
     &                    intercept,ni_1d)
                     if(lock(1) .eq. 1) then
                        call getrvar('xloc',xloc,error)
                     else
                        xloc=(phmin(1)+phmax(1))/2.
                     endif
                     if(lock(3) .eq. 1) then
                        call getrvar('zloc',zloc,error)
                     else
                        zloc=(phmin(3)+phmax(3))/2.
                     endif
                     if(error) then
                        write (message,*)'Cant get loc in write_1d'
                        call write_message
                        call freemem(y)
                        call freemem(z)
                        call freemem(t)
                        return
                     endif
                     call fill_array(%val(z),ni_1d,zloc)
                     call fill_array(%val(x),ni_1d,tloc)           
                end select
             case(3)
                z = slabad_1d
                y=getmem(ni_1d)
                t=getmem(ni_1d)
                x=getmem(ni_1d)
                select case (dep)
                 case(2)
                    call ive_fill_line(%val(y),%val(z),slope,
     &                   intercept,ni_1d)
                    if(lock(3) .eq. 1) then
                       call getrvar('xloc',xloc,error)
                    else
                       xloc=(phmin(1)+phmax(1))/2.
                    endif
                    if(lock(4) .eq. 1) then
                       call getrvar('time',tloc,error)
                    else
                       tloc=(phmin(4)+phmax(4))/2.
                    endif
                    if(error) then
                       write (message,*)'Cant get loc in write_1d'
                       call write_message
                       call freemem(y)
                       call freemem(z)
                       call freemem(t)
                       return
                    endif
                    call fill_array(%val(x),ni_1d,xloc)
                    call fill_array(%val(t),ni_1d,tloc)           
                 case(1)
                    call ive_fill_line(%val(x),%val(z),slope,
     &                   intercept,ni_1d)
                    if(lock(2) .eq. 1) then
                       call getrvar('yloc',yloc,error)
                    else
                       yloc=(phmin(2)+phmax(2))/2.
                    endif
                    if(lock(4) .eq. 1) then
                       call getrvar('time',tloc,error)
                    else
                       tloc=(phmin(4)+phmax(4))/2.
                    endif
                    if(error) then
                       write (message,*)'Cant get loc in write_1d'
                       call write_message
                       call freemem(y)
                       call freemem(z)
                       call freemem(t)
                       return
                    endif
                    call fill_array(%val(y),ni_1d,yloc)
                    call fill_array(%val(t),ni_1d,tloc)           
                 case(4)
                    call ive_fill_line(%val(t),%val(z),slope,
     &                   intercept,ni_1d)
                    if(lock(2) .eq. 1) then
                       call getrvar('yloc',yloc,error)
                    else
                       yloc=(phmin(2)+phmax(2))/2.
                    endif
                    if(lock(1) .eq. 1) then
                       call getrvar('xloc',xloc,error)
                    else
                       xloc=(phmin(1)+phmax(1))/2.
                    endif
                    if(error) then
                       write (message,*)'Cant get loc in write_1d'
                       call write_message
                       call freemem(y)
                       call freemem(z)
                       call freemem(t)
                       return
                    endif
                    call fill_array(%val(x),ni_1d,xloc)
                    call fill_array(%val(y),ni_1d,tloc)           
                end select
             case(4)
                t = slabad_1d
                y=getmem(ni_1d)
                z=getmem(ni_1d)
                x=getmem(ni_1d)
                select case (dep)
                 case(2)
                    call ive_fill_line(%val(y),%val(t),slope,
     &                   intercept,ni_1d)
                    if(lock(3) .eq. 1) then
                       call getrvar('zloc',zloc,error)
                    else
                       zloc=(phmin(3)+phmax(3))/2.
                    endif
                    if(lock(1) .eq. 1) then
                       call getrvar('xloc',xloc,error)
                    else
                       xloc=(phmin(1)+phmax(1))/2.
                    endif
                    if(error) then
                       write (message,*)'Cant get loc in write_1d'
                       call write_message
                       call freemem(y)
                       call freemem(z)
                       call freemem(t)
                       return
                    endif
                    call fill_array(%val(z),ni_1d,zloc)
                    call fill_array(%val(x),ni_1d,tloc)           
                 case(3)
                    call ive_fill_line(%val(z),%val(t),slope,
     &                   intercept,ni_1d)
                    if(lock(2) .eq. 1) then
                       call getrvar('yloc',yloc,error)
                    else
                       yloc=(phmin(2)+phmax(2))/2.
                    endif
                    if(lock(1) .eq. 1) then
                       call getrvar('xloc',xloc,error)
                    else
                       xloc=(phmin(1)+phmax(1))/2.
                    endif
                    if(error) then
                       write (message,*)'Cant get loc in write_1d'
                       call write_message
                       call freemem(y)
                       call freemem(z)
                       call freemem(t)
                       return
                    endif
                    call fill_array(%val(y),ni_1d,yloc)
                    call fill_array(%val(x),ni_1d,tloc)           
                 case(1)
                    call ive_fill_line(%val(x),%val(t),slope,
     &                   intercept,ni_1d)
                    if(lock(2) .eq. 1) then
                       call getrvar('yloc',yloc,error)
                    else
                       yloc=(phmin(2)+phmax(2))/2.
                    endif
                    if(lock(3) .eq. 1) then
                       call getrvar('zloc',zloc,error)
                    else
                       zloc=(phmin(3)+phmax(3))/2.
                    endif
                    if(error) then
                       write (message,*)'Cant get loc in write_1d'
                       call write_message
                       call freemem(y)
                       call freemem(z)
                       call freemem(t)
                       return
                    endif
                    call fill_array(%val(z),ni_1d,zloc)
                    call fill_array(%val(y),ni_1d,yloc)           
                end select
             end select
          endif
          call  ive_1d_make_phys(%val(x),%val(y),%val(z),%val(t),ni_1d)
          call output_1d(file, %val(x), %val(y),%val(z),%val(t),%val(f),
     &         ni_1d, field,netcdf)
          select case (ind)
       case(1) 
          call freemem(y)
          call freemem(z)
          call freemem(t)
       case(2)
           call freemem(x)
          call freemem(z)
          call freemem(t)
       case(3) 
          call freemem(y)
          call freemem(x)
          call freemem(t)
       case(4) 
          call freemem(y)
          call freemem(z)
          call freemem(x)
       end select
c
c     Format statements.
c
  100 format (1x,'Command ',a,' requires a value.')
c
      return 
      end


      subroutine  ive_fill_line(y,x,slope,int,ni)
      real x,y
      real slope,int
      integer ni, i
      dimension x(ni), y(ni)
      do i=1,ni
         y(i)=x(i)*slope+int
      end do
      end

      subroutine ive_1d_make_phys(x,y,z,t,ni)

      include 'units.icl'

       real x,y,z,t,lat,lon
       integer ni, i
       logical mapflg,error
       dimension x(ni), y(ni),z(ni),t(ni),lat(ni),lon(ni)
       
       call getlvar('mapflg',mapflg,error)
       
       if(mapflg) then
          call phys_2_lonlat_trans (x,y,lon,lat,ni)
          x=lon
          y=lat
       else
          call scale(x,ni,domain_slope(1),domain_intercept(1),0.0)
          call scale(y,ni,domain_slope(2),domain_intercept(2),0.0)
       endif

       call scale(z,ni,domain_slope(3),domain_intercept(3),0.0)
       call scale(t,ni,domain_slope(4),domain_intercept(4),0.0)

       end
