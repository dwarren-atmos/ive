      subroutine drwmap (error)
c-----------------------------------------------------------------------
c     $Id: drwmap.f,v 1.5 2006-10-05 16:21:50 reinecke Exp $
c     Purpose:
c        This routine is the interface to the NCAR EZMAP routines. 
c     Arguments:
c        error  logi  output  indicates possible errors found in this
c                             routine.
c                             error = true   error detected.
c                             error = false  no error detected.
c     History:
c        $Log: drwmap.f,v $
c        Revision 1.5  2006-10-05 16:21:50  reinecke
c        Make line width of map boundries thicker.
c
c        Revision 1.4  1997/03/14 22:26:32  warren
c        Added new command map_window to allow for mouse windowing on non
c        equadistant or default maps.
c
c Revision 1.3  1993/12/15  18:15:29  harry
c Add support for DEFAULT_MAP command, default_map transform.  Modify map
c clipping to work properly with the map background widget.
c
c Revision 1.2  1993/08/18  15:47:18  harry
c Change to new transform routines.
c
c Revision 1.1.1.1  1993/04/07  19:39:54  harry
c Initial CVS checkin of IVE.
c
c Revision 2.1  1993/03/26  08:56:44  davidb
c map_max,map_clip added
c
c Revision 2.0  1992/07/01  21:12:55  millerp
c Released version rereving to 2.0
c
c Revision 1.2  1992/05/05  15:24:40  millerp
c Made changes so that parameters for selected maps depend on plot window.
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
      include 'data.icl'
      include 'deflts.icl'
      include 'message.icl'
      include 'supma.icl'
      include 'ive_widgets.icl'
c
c
c     Argument declarations.
c
      logical        error
c
c     Local variable declarations.
c
      character *(2) dmaplim
      character *(8) ncarproj
      integer        i, ibeg, idum, iend, ierr
      integer        DOTTED, SOLID
      real           dplat, dplon, dplm1, dplm2, dplm3, dplm4
      parameter ( DOTTED = 43690 ,
     &            SOLID  = 65535 )
      dimension dplm1(2), dplm2(2), dplm3(2), dplm4(2)
c
c
c     External function declarations.
c
      integer        nerro, strbeg, strend
c
c
c clipping-variables:
	real	umin,umax,vmin,vmax
c
c
      if (defmap) then
         call call_defmap
         call ui_update(MAPROJ_UI)
         call ui_update(LON_UI)
         call ui_update(LAT_UI)
         call ui_update(ROT_UI)
         call ui_update(MAPCLIP_UI)
      endif
      error = .false.
      ncarproj(1:len(ncarproj)) = ' '
      ibeg = strbeg(proj)
      iend = strend(proj)
c
c     Insert code to allow error recovery.
c
      call entsr (idum, 1)
c
c     Set viewport for map.
c     
      call mappos ( VPLEFT, VPRIGHT, VPBOT, VPTOP )

      if ( nerro(ierr) .ne. 0 ) then
         call maperr ('mappos', error)
         return
      endif

c now, for all map-projection, the parameters are set:
c projections depend on the user having set the correct
c values of maplim, plm1 - plm4 or clipping is required.

c setting the internal maplimits:
	dmaplim=maplim

c projection pole is set here, because it is overwritten for some proj.:
        dplat    = plat
        dplon    = plon

c passing projection-parameters:
        do 10 i = 1, 2
        	dplm1(i) = plm1(i)
            	dplm2(i) = plm2(i)
            	dplm3(i) = plm3(i)
            	dplm4(i) = plm4(i)
 10      	continue

c remove elliptic border, if set:
	call mapsti ('EL', 0)

        ncarproj = proj(ibeg:iend)

c first, process special-not NCAR projections (NHST,NHOR,...)to 
c transform them to NCAR projections:
c and other projections that need special care (MO,ME):

      if(mapwin_u1 .ne. 0.0 .or. mapwin_u2 .ne. 0.0
     &     .or. mapwin_v1 .ne. 0.0 .or. mapwin_v2 .ne. 0.0)
     &     then
         dplm1(1)=mapwin_u1
         dplm2(1)=mapwin_u2
         dplm3(1)=mapwin_v1
         dplm4(1)=mapwin_v2
         dmaplim='LI'

      else

         if ( (proj(ibeg:iend) .eq. 'NHST') .or. 
     &        (proj(ibeg:iend) .eq. 'NHOR') ) then
c
c        Set up map parameters for northern hemisphere polar 
c        stereographic or orthographic projections.
c 
            ncarproj = proj(ibeg+2:iend)
            dplat    = 90.0
            dplon    = plon
            call mapsti ('EL', 1)
            if (maplim.eq. 'MA') then
c           show the northern hemisphere (from 0 to 90)
               dmaplim='AN'
               dplm1(1) = dplat
               dplm2(1) = dplm1(1)
               dplm3(1) = dplm1(1)
               dplm4(1) = dplm1(1)
               call mapsti ('EL', 1)
            endif
         elseif ( (proj(ibeg:iend) .eq. 'SHST') .or. 
     &           (proj(ibeg:iend) .eq. 'SHOR') ) then
c
c        Set up map parameters for southern hemisphere polar 
c        stereographic or orthographic projections. 
            ncarproj = proj(ibeg+2:iend)
            dplat    = -90.0
            dplon    = plon
            call mapsti ('EL', 1)
            if (maplim.eq. 'MA') then
c               show the northern hemisphere (from 0 to 90)
               dmaplim='AN'
               dplm1(1) = abs (dplat)
               dplm2(1) = dplm1(1)
               dplm3(1) = dplm1(1)
               dplm4(1) = dplm1(1)
            endif
         elseif (proj(ibeg:iend) .eq. 'MO' ) then
            if (maplim.eq. 'MA') then
c               elliptic border for whole map:
               call mapsti ('EL', 1)
            endif
         elseif (proj(ibeg:iend).eq. 'ME' ) then
c       mercator range-test:
            if ((plwmin(2).lt.-85.0).or.(plwmax(2).gt.85.0)) then
               error = .true.
               write (message, 100) char(10)
               call write_message
               return
            endif
         endif

c now, get the umin,umax,vmin,vmax to do clipping, if selected:
         if (maplim.eq.'CL' ) then 
            call map_clip(umin,umax,vmin,vmax,error)
            if (error) then
               return
            endif
            dmaplim='LI'
            dplm1(1)=umin
            dplm2(1)=umax
            dplm3(1)=vmin
            dplm4(1)=vmax
         endif
      endif

c the following lines are done for every projection to set it:

      call maproj (ncarproj(strbeg(ncarproj):strend(ncarproj)), 
     &     dplat, dplon, rota )
      if ( nerro(ierr) .ne. 0 ) then
         call maperr ('maproj', error)
         return
      endif

c
c     Set the portion of the u/v plane to be drawn on.
c
      
      call mapset (dmaplim(strbeg(dmaplim):strend(dmaplim)), 
     &     dplm1, dplm2, dplm3, dplm4)
      
      if ( nerro(ierr) .ne. 0 ) then
         call maperr ('mapset', error)
         return
      endif
      call mapint
      if ( nerro(ierr) .ne. 0 ) then
         call maperr ('mapint', error)
         return
      endif
c
c     Set some parameters affecting the appearance of the map.
c
c     Set map outline options.
c     
      call mapstc ( 'OU', outlin(strbeg(outlin):strend(outlin)) )
      if ( nerro(ierr) .ne. 0 ) then
         call maperr ('mapstc', error)
         return
      endif
	call gslwsc(2.00)
c
c     Set the grid spacing.
c
      call mapstr ('GR', grdspa)
c
c
c     Set line type for the grid
c 
      call mapsti ('DA', DOTTED)
c
c
c     Turn on perimeter:
c
      call mapsti ('PE', 1)


c
c
c     Format statements.
c
 100  format (1x, 'For Mercator projections, do not try to plot ',a,
     &            'within 5 degrees of either pole.')
c
      return
      end
c
c
      subroutine maperr (routine, error)
c-----------------------------------------------------------------------
c     Purpose: 
c        This routine prints error messages if EZMAP routines 
c        encountered errors.
c     Arguments:
c        routine  char  input   the routine that had the error.
c        error    logi  in/out  error flag.
c     History:
c-----------------------------------------------------------------------
c
      include 'message.icl'
c
c
c     Argument declarations.
c
      character *(*) routine
      logical        error
c
c
c     External function declarations.
c
      integer        strbeg, strend
c
c
c
c
c     Print message.
c
      write (message, 100) routine (strbeg(routine):strend(routine))
      call write_message 
c
c     Call EZMAP error handling routines.
c
      call eprin
      call errof
c
c     Set error flag.
c
      error = .true.
c
c
c     Format statements.
c
 100  format (1x, 'EZMAP error in routine ',a,'.')
c
c
      return
      end

      subroutine map_max (dumin,dumax,dvmin,dvmax,found)
c-----------------------------------------------------------------------
c     Purpose: 
c        This routine returns the maximal u,v values for a map-projection.
c        The type of the projection is read out of the common blocks.
c        The accepted map-projections are:
c        NHST,NHOR,SHST,SHOR,ST,OR,LC,LE,GN,AE,SV,CE,ME,MO
c     Arguments:
c     Output:
c        dumin,dumax: range of u for this projection.
c        dvmin,dvmax: range of v for this projection.
c        found:    -1:not found, 0:0 sliders
c                   1:1 slider,  2:2 sliders.
c                   1 slider not used yet.
c     History:
c-----------------------------------------------------------------------
c
      include 'data.icl'
      include 'deflts.icl'
      include 'supma.icl'
      include 'message.icl'

c unfortunately, ncarproj is a string, but SUPMAP needs an integer:
c convert string to integer and do many other important things as
c setting the maximum for u,v, etc...
c if no match, ST is taken

c output vars:
	 real             dumin,dumax,dvmin,dvmax
         integer          found
c internal vars:
         integer          ibeg,iend
         character*(2)    proj_name
         character *(8)   ncarproj

c     External function declarations.
c
         integer        strbeg, strend

c set found to 2 to indicate rectangular
         found=2
c work on string:
         ncarproj(1:len(ncarproj)) = ' '
         ibeg = strbeg(proj)
         iend = strend(proj)
         ncarproj = proj(ibeg:iend)
         if ( (proj(ibeg:iend) .eq. 'NHST') .or. 
     &     (proj(ibeg:iend) .eq. 'NHOR') ) then
c use the last 2 chracters in these cases: 
                ncarproj = proj(ibeg+2:iend)
c set flag to indicate no sliders:
		found=0
          elseif ( (proj(ibeg:iend) .eq. 'SHST') .or. 
     &         (proj(ibeg:iend) .eq. 'SHOR') ) then
c use the last 2 chracters in these cases: 
                ncarproj = proj(ibeg+2:iend)
c set flag to indicate no sliders:
		found=0
          endif

c now, the max-limits are choosen:
         proj_name=ncarproj(ibeg:iend)
	 if (proj_name.eq. 'ST') then 
                dumin=-2.0
                dumax=2.0
                dvmin=-2.0
                dvmax=2.0
	 elseif (proj_name.eq. 'OR') then 
c        range circle of radius 1
                dumin=-1.0
                dumax=1.0
                dvmin=-1.0
                dvmax=1.0
	 elseif (proj_name.eq. 'LC') then 
                dumin=-2.0
                dumax=2.0
                dvmin=-2.0
                dvmax=2.0
	 elseif (proj_name.eq. 'LE') then 
c        range circle of radius 2
                dumin=-2.0
                dumax=2.0
                dvmin=-2.0
                dvmax=2.0
	 elseif (proj_name.eq. 'GN') then 
c        range circle of radius Pi
c        here, this radius is taken to provide a large range
c        but holding things together. The entire range would be the
c        entire u/v-plane, but distortion becomes large.
c        the range provided here is larger than using 'MA' !
                dumin=-3.1415692
                dumax=3.1415692
                dvmin=-3.1415692
                dvmax=3.1415692
	 elseif (proj_name.eq. 'AE') then 
c        range circle of radius Pi
                dumin=-3.1415692
                dumax=3.1415692
                dvmin=-3.1415692
                dvmax=3.1415692
	 elseif (proj_name.eq. 'SV') then 
c        range circle of radius 1
                dumin=-1.0
                dumax=1.0
                dvmin=-1.0
                dvmax=1.0
         elseif (proj_name.eq. 'CE' ) then
                dumin=-180.0
                dumax=180.0
                dvmin=-90.0
                dvmax=90.0
         elseif (proj_name.eq. 'ME' ) then
                dumin=-3.1415692
                dumax=3.1415692
c               following represents the fact, that distortion becomes
c               large for ABS(lat).gt.85.
                dvmin=-3.1415692
                dvmax=3.1415692
         elseif (proj_name.eq. 'MO' ) then
                dumin=-2.0
                dumax=2.0
                dvmin=-1.0
                dvmax=1.0
      	 else
	        write (message, 100) 'keyword not found'
                call write_message 
                found=-1
	 endif

c     Format statements.
 100  format (1x, 'Error in map_li: ',a,'.')

       end
c of map_max


      subroutine map_clip (umin,umax,vmin,vmax,error)
c-----------------------------------------------------------------------
c     Purpose: 
c        This routine returns the actual u,v values for a map-projection,
c        doing clipping using data-window sizes, edge-points of projection,
c        projection-angles,... all as in help=maplimits.
c        The routine map_li is called to get dumin,dumax,...
c        allowed limit-types are: MA,CO,PO,AN,LI,CL
c        IF projection is 'NHST' or equal, CL without parameters clips
c	 a region around...
c     Arguments:
c     Output:
c        umin,umax: range of u for this projection.
c        vmin,vmax: range of v for this projection.
c        error:    .false., if no error occurred, .true. else.
c     History:
c-----------------------------------------------------------------------
c
        include 'data.icl'
        include 'deflts.icl'
        include 'supma.icl'
        include 'message.icl'

c output vars:
        real	       umin,umax,vmin,vmax
        logical        error

c internal vars:
        character *(8) ncarproj
        integer        i, ibeg, iend, ierr
        real           dplat, dplon, dplm1, dplm2, dplm3, dplm4
        real           dplwmin,dplwmax,lat,lon
        dimension dplm1(2), dplm2(2), dplm3(2), dplm4(2)
        dimension      dplwmin(2),dplwmax(2)
        integer	       clip,found
c clipping-variables:
	real	       latres,lonres,latstep,lonstep,u,v
	real	       dumin,dumax,dvmin,dvmax
	real	       latmin,latmax
c External function declarations.
        integer        nerro,strbeg, strend

c setting the grid-preprocessing resolution,.i.e the number of points
c on the lat and lon borders to be taken in consideration to calculate
c the clipping area containing the whole specified area on the globe:
      latres=20.
      lonres=20.

c clipping default value: 1:rectangular cl., 2:radius cl., 0:no cl.
	 clip=1
c no error occurred:
         error=.false.

c passing projection-parameters:
         dplat    = plat
         dplon    = plon
         do 10 i = 1, 2
            	dplm1(i) = plm1(i)
            	dplm2(i) = plm2(i)
            	dplm3(i) = plm3(i)
            	dplm4(i) = plm4(i)
 10      	continue

c work on string:  to set the correct map-projection.
         ncarproj(1:len(ncarproj)) = ' '
         ibeg = strbeg(proj)
         iend = strend(proj)
         ncarproj = proj(ibeg:iend)
         if ( (proj(ibeg:iend) .eq. 'NHST') .or. 
     &     (proj(ibeg:iend) .eq. 'NHOR') ) then
c use the last 2 chracters in these cases: 
                ncarproj = proj(ibeg+2:iend)
c set pole latitude:
                dplat=90.0
c set radius clipping:
		clip=2
          elseif ( (proj(ibeg:iend) .eq. 'SHST') .or. 
     &         (proj(ibeg:iend) .eq. 'SHOR') ) then
c use the last 2 chracters in these cases: 
                ncarproj = proj(ibeg+2:iend)
c set pole latitude:
		dplat=-90.0
c set radius clipping:
		clip=2
          endif

c set up the map-projection to calculate points:
         CALL MAPROJ (ncarproj(strbeg(ncarproj):strend(ncarproj)),
     &  	dPLAT,dPLON,ROTA)
         CALL MAPINT
         if ( nerro(ierr) .ne. 0 ) then
                call maperr ('maproj', error)
                return
         endif

c set umin,.. to do min/max later, see clip=1 - clause
		umin=1000.
		umax=-1000.
		vmin=1000.
		vmax=-1000.

c the meaning of plmX(i) is the same as in the NCAR manuals.
c hint: phys_2_lonlat_trans(inlon,inlat,outlon,outlat)
c hint: maptrn(inlat,inlon,u,v)
	if (maplim.eq. 'MA') then
		clip=0
                call map_max(umin,umax,vmin,vmax,found)
                if (found.eq.-1) then
                        error=.true.
                        return
                endif
	elseif (maplim.eq. 'CO') then
		clip=0
		CALL MAPTRN(dplm1(1),dplm2(1),u,v)
	        umin=AMIN1(u,umin)
		umax=AMAX1(u,umax)
		vmin=AMIN1(v,vmin)
		vmax=AMAX1(v,vmax)
		CALL MAPTRN(dplm3(1),dplm4(1),u,v)
	        umin=AMIN1(u,umin)
		umax=AMAX1(u,umax)
		vmin=AMIN1(v,vmin)
		vmax=AMAX1(v,vmax)
        elseif (maplim.eq. 'PO') then
		clip=0
		CALL MAPTRN(dplm1(1),dplm1(2),u,v)
	        umin=AMIN1(u,umin)
		umax=AMAX1(u,umax)
		vmin=AMIN1(v,vmin)
		vmax=AMAX1(v,vmax)
		CALL MAPTRN(dplm2(1),dplm2(2),u,v)
	        umin=AMIN1(u,umin)
		umax=AMAX1(u,umax)
		vmin=AMIN1(v,vmin)
		vmax=AMAX1(v,vmax)
		CALL MAPTRN(dplm3(1),dplm3(2),u,v)
	        umin=AMIN1(u,umin)
		umax=AMAX1(u,umax)
		vmin=AMIN1(v,vmin)
		vmax=AMAX1(v,vmax)
		CALL MAPTRN(dplm4(1),dplm4(2),u,v)
	        umin=AMIN1(u,umin)
		umax=AMAX1(u,umax)
		vmin=AMIN1(v,vmin)
		vmax=AMAX1(v,vmax)
	elseif (maplim.eq. 'AN') then
		clip=0
c               the angles are turned into lat/lon positions 
c               using dplon,dplat:
		CALL MAPTRN(dplat,dplon-dplm1(1),u,v)
	        umin=AMIN1(u,umin)
		umax=AMAX1(u,umax)
		vmin=AMIN1(v,vmin)
		vmax=AMAX1(v,vmax)
		CALL MAPTRN(dplat,dplon+dplm2(1),u,v)
	        umin=AMIN1(u,umin)
		umax=AMAX1(u,umax)
		vmin=AMIN1(v,vmin)
		vmax=AMAX1(v,vmax)
		CALL MAPTRN(dplat-dplm3(1),dplon,u,v)
	        umin=AMIN1(u,umin)
		umax=AMAX1(u,umax)
		vmin=AMIN1(v,vmin)
		vmax=AMAX1(v,vmax)
		CALL MAPTRN(dplat+dplm3(1),dplon,u,v)
	        umin=AMIN1(u,umin)
		umax=AMAX1(u,umax)
		vmin=AMIN1(v,vmin)
		vmax=AMAX1(v,vmax)
	elseif (maplim.eq. 'LI') then
		clip=0
		umin=dplm1(1)
                umax=dplm2(1)
                vmin=dplm3(1)
                vmax=dplm4(1)
	elseif (maplim.eq. 'CL') then
c               if the plmX values are set, take them to 
c               find the plot-region, if not, use the
c               data values to do this:
                dplwmin(1)=plwmin(1)
                dplwmin(2)=plwmin(2)
                dplwmax(1)=plwmax(1)
                dplwmax(2)=plwmax(2)
	 else
                write (message, 100) 'limit-type not found'
                call write_message 
                error=.true.
                return
	 endif
c of maplim-selection

	if (clip.eq.1) then
c begins rectangular clipping:
c the border of the area of interest is preprocessed trough the call of
c PHYS_2_LONLAT_TRANS and MAPTRN and the paper-ccordinates are stored and sorted to 
c obtain the smallest area containing all the data:
		lonstep=(dplwmax(1)-dplwmin(1))/lonres
		latstep=(dplwmax(2)-dplwmin(2))/latres
		umin=1000.
		umax=-1000.
		vmin=1000.
		vmax=-1000.

c the first loop processes all the longitudes:
		do 30 i=1,lonres+1
c upper limit of area:
                CALL PHYS_2_LONLAT_TRANS(dplwmin(1)+lonstep*(i-1),
     &                  dplwmax(2),lon,lat,1)
		CALL MAPTRN(lat,lon,u,v)
			umin=AMIN1(u,umin)
			umax=AMAX1(u,umax)
			vmin=AMIN1(v,vmin)
			vmax=AMAX1(v,vmax)
c lower limit of area:
		CALL PHYS_2_LONLAT_TRANS(dplwmin(1)+lonstep*(i-1),
     &                       dplwmin(2),lon,lat,1)
		CALL MAPTRN(lat,lon,u,v)
			umin=AMIN1(u,umin)
			umax=AMAX1(u,umax)
			vmin=AMIN1(v,vmin)
			vmax=AMAX1(v,vmax)
 30			continue
c the second loop processes all the latitudes:
		do 40 i=1,latres+1
c right limit of area:
		CALL PHYS_2_LONLAT_TRANS(dplwmax(1),
     &                  dplwmin(2)+latstep*(i-1),lon,lat,1)
		CALL MAPTRN(lat,lon,u,v)
			umin=AMIN1(u,umin)
			umax=AMAX1(u,umax)
			vmin=AMIN1(v,vmin)
			vmax=AMAX1(v,vmax)
c left limit of area:
		CALL PHYS_2_LONLAT_TRANS(dplwmin(1),
     &                       dplwmin(2)+latstep*(i-1),lon,lat,1)
		CALL MAPTRN(lat,lon,u,v)
			umin=AMIN1(u,umin)
			umax=AMAX1(u,umax)
			vmin=AMIN1(v,vmin)
			vmax=AMAX1(v,vmax)
 40			continue

c following line marks end of clip.eq.1 clause:
	elseif (clip.eq.2) then
c clipping using radii: the phys_2_lonlat_trans routine
c is used to find latmin and
c latmax: the border of the data-window is processed:
		lonstep=(dplwmax(1)-dplwmin(1))/lonres
		latstep=(dplwmax(2)-dplwmin(2))/latres
                latmin=1000.
		latmax=-1000.

c the first loop processes all the longitudes:
		do 50 i=1,lonres+1
c upper limit of area:
                CALL PHYS_2_LONLAT_TRANS(dplwmin(1)+lonstep*(i-1),
     &                  dplwmax(2),lon,lat,1)
			latmin=AMIN1(lat,latmin)
			latmax=AMAX1(lat,latmax)
c lower limit of area:
		CALL PHYS_2_LONLAT_TRANS(dplwmin(1)+lonstep*(i-1),
     &                       dplwmin(2),lon,lat,1)
			latmin=AMIN1(lat,latmin)
			latmax=AMAX1(lat,latmax)
 50			continue
c the second loop processes all the latitudes:
		do 60 i=1,latres+1
c right limit of area:
		CALL PHYS_2_LONLAT_TRANS(dplwmax(1),
     &                  dplwmin(2)+latstep*(i-1),lon,lat,1)
			latmin=AMIN1(lat,latmin)
			latmax=AMAX1(lat,latmax)
c left limit of area:
		CALL PHYS_2_LONLAT_TRANS(dplwmin(1),
     &                       dplwmin(2)+latstep*(i-1),lon,lat,1)
			latmin=AMIN1(lat,latmin)
			latmax=AMAX1(lat,latmax)
 60			continue
c now calculate the u/v values:
		CALL MAPTRN(latmin,0.0,umin,vmin)
		CALL MAPTRN(latmax,0.0,umax,vmax)
		umax=AMAX1(ABS(umin),ABS(umax),ABS(vmin),ABS(vmax))
        	umin=-umax
		vmin=umin
		vmax=umax
c endif clip using radii:
	endif

c range-test of umin,umax,...:
        call map_max(dumin,dumax,dvmin,dvmax,found)
        if (found.ne.-1) then 
c       test for convenient range of umin,...
		if (umin.lt.dumin) then
			umin=dumin
		endif	
		if (umax.gt.dumax) then
			umax=dumax
		endif	
		if (vmin.lt.dvmin) then
			vmin=dvmin
		endif	
		if (vmax.gt.dvmax) then
			vmax=dvmax
		endif
        else
              error=.true.
              return
	endif        

c if the limit is LI, pass umin,... to plmX(1):
c this is done to store the limits of a CL -call to allow overplot
c with new data on the same range. to set the clipping to fit
c the data-range, call MAPLIM=CL again.
        if (maplim.eq. 'CL' .or. maplim .eq. 'LI') then
		plm1(1)=umin
                plm2(1)=umax
                plm3(1)=vmin
                plm4(1)=vmax
        endif

c     Format statements.
 100  format (1x, 'Error in map_clip: ',a,'.')

	end
c of map_clip


